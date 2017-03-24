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
import qualified Graphics.Vty               as Vty
import           Reactive.Banana
import           Reactive.Banana.Frameworks



data Arena = Arena
    { _arenaWidth  :: Int
    , _arenaHeight :: Int }

data Vec2 = Vec2
    { _vx :: Int
    , _vy :: Int }

data Snake = Snake
    { _body :: Seq Vec2 }

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

    eventNetwork <- compile $ do
        eClock <- fromAddHandler addClockEvent

        bDirection <- fromChanges initialDirection addDirectionEvent

        eSnake <- accumE initialSnake (moveSnake <$> bDirection <@ eClock)

        let bArena = pure initialArena
        let bEdible = pure initialEdible

        let bEverythingElse :: Behavior (Snake -> RenderState)
            bEverythingElse = fmap flip RenderState <$> bArena <*> bEdible

        let eRender :: Reactive.Banana.Event RenderState
            eRender = bEverythingElse <@> eSnake

        reactimate (fmap (Vty.update vty . paintRenderState) eRender)

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
