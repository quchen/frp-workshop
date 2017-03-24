{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE NumDecimals     #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where



import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.Function
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq
import           Debug.Trace
import qualified Graphics.Vty               as Vty
import           Reactive.Banana
import           Reactive.Banana.Frameworks



data Arena = Arena
    { _arenaWidth  :: Int
    , _arenaHeight :: Int }

data Vec2 = Vec2
    { _vx :: Int
    , _vy :: Int }
    deriving (Eq, Show)

data Snake = Snake
    { _body :: Seq Vec2 }
    deriving (Show)

data Direction = U | D | L | R

data RenderState = RenderState
    { _arena  :: Arena
    , _snake  :: Snake
    , _edible :: Vec2 }

data RenderTick = RenderTick

makeLenses ''Arena
makeLenses ''Vec2
makeLenses ''Snake
makeLenses ''RenderState
makePrisms ''Direction

paintRenderState :: RenderState -> Vty.Picture
paintRenderState state = Vty.picForLayers (arenaLayer : Vty.translate 1 1 edibleLayer : map (Vty.translate 1 1) snakeLayers)
  where
    arenaLayer = Vty.vertCat
        [ horizontalBorder
        , Vty.horizCat
            [ verticalBorder
            , Vty.backgroundFill (view (arena . arenaWidth) state) (view (arena . arenaHeight) state)
            , verticalBorder ]
        , horizontalBorder ]
    horizontalBorder = Vty.charFill Vty.defAttr '-' (view (arena . arenaWidth) state + 2) 1
    verticalBorder = Vty.charFill Vty.defAttr '|' 1 (view (arena . arenaHeight) state)

    edibleLayer = drawChar '*' (view edible state)
    snakeLayers = toList (fmap (drawChar 'o') (view (snake . body) state))

    drawChar c (Vec2 x y) = Vty.translate x y (Vty.char Vty.defAttr c)

initialEdible :: Vec2
initialEdible = Vec2 15 15

initialArena :: Arena
initialArena = Arena
    { _arenaHeight = 20
    , _arenaWidth = 20 }

initialSnake :: Snake
initialSnake = Snake
    { _body = Seq.fromList [Vec2 10 y | y <- [10..15]] }

initialDirection :: Direction
initialDirection = U

moveSnake :: Direction -> Snake -> Snake
moveSnake dir s = set body newBody s
  where
    newHead = case dir of
        U -> Vec2 headX (headY-1)
        D -> Vec2 headX (headY+1)
        L -> Vec2 (headX-1) headY
        R -> Vec2 (headX+1) headY
    Vec2 headX headY = view (body . sequenceHead) s

    newBody = newHead Seq.<| view (body . sequenceInit) s

sequenceHead :: Getter (Seq a) a
sequenceHead = to (\s -> case Seq.viewl s of
    Seq.EmptyL -> error "Empty sequence!"
    x Seq.:< _ -> x)

sequenceTail :: Getter (Seq a) (Seq a)
sequenceTail = to (\s -> case Seq.viewl s of
    Seq.EmptyL  -> error "Empty sequence!"
    _ Seq.:< xs -> xs)

sequenceInit :: Getter (Seq a) (Seq a)
sequenceInit = to (\s -> case Seq.viewr s of
    Seq.EmptyR  -> error "Empty sequence!"
    xs Seq.:> _ -> xs)

withVty :: (Vty.Vty -> IO a) -> IO a
withVty = bracket (Vty.standardIOConfig >>= Vty.mkVty) Vty.shutdown

main :: IO ()
main = withVty (\vty -> do
    (addClockEvent, fireClockEvent) <- newAddHandler
    (addDirectionEvent, fireDirectionEvent) <- newAddHandler
    (addDeathEvent, fireDeathEvent) <- newAddHandler

    eventNetwork <- compile $ do
        eClock <- fromAddHandler addClockEvent

        bDirection <- fromChanges initialDirection addDirectionEvent

        eDeath <- fromAddHandler addDeathEvent

        let collidesWithArena :: Snake -> Bool
            collidesWithArena sn =
                let Vec2 headX headY = view (body . sequenceHead) sn
                    arenaH = view arenaHeight initialArena
                    arenaW = view arenaWidth  initialArena
                in  headX < 0 || headY < 0 || headX >= arenaW || headY >= arenaH

            collidesWithSelf :: Snake -> Bool
            collidesWithSelf s = view (body . sequenceHead) s `elem` view (body . sequenceTail) s

        let eSnakeMotion = moveSnake <$> bDirection <@ eClock
            newBSnake = accumB initialSnake eSnakeMotion
        bSnake <- newBSnake >>= \initialBSnake -> switchB initialBSnake _


        let bIsAlive :: Behavior Bool
            bIsAlive = fmap (\s -> not (collidesWithArena s || collidesWithSelf s)) bSnake

        let bEdible = pure initialEdible

        let bRender :: Behavior RenderState
            bRender = RenderState initialArena <$> bSnake <*> bEdible


        eRender <- fmap (whenE           bIsAlive ) (changes bRender)
        eDeath  <- fmap (whenE (fmap not bIsAlive)) (changes bRender)

        reactimate' (fmap (fmap (Vty.update vty . paintRenderState)) eRender)
        reactimate' (fmap (fmap (const (fireDeathEvent ()))) eDeath)

    actuate eventNetwork

    withClock 8 (fireClockEvent RenderTick) (fix (\loop -> Vty.nextEvent vty >>= \case
        Vty.EvKey (Vty.KChar 'q') _ -> pure ()
        Vty.EvKey Vty.KEsc _        -> pure ()
        Vty.EvKey Vty.KUp _         -> fireDirectionEvent U >> loop
        Vty.EvKey Vty.KDown _       -> fireDirectionEvent D >> loop
        Vty.EvKey Vty.KLeft _       -> fireDirectionEvent L >> loop
        Vty.EvKey Vty.KRight _      -> fireDirectionEvent R >> loop
        _other                      -> loop
        )))

withClock
    :: Int           -- ^ Tick periodicity
    -> IO tickAction -- ^ Action to run each tick
    -> IO body       -- ^ Action to perform with the clock ticking
    -> IO body
withClock ticksPerSecond tickAction stuff = bracket acquire release (const stuff)
  where
    acquire = async (forever (do
        _ <- tickAction
        threadDelay (1e6 `quot` ticksPerSecond) ))
    release = cancel
