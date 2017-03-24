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
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq
import           Graphics.Vty
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
    { _direction :: Direction
    , _body      :: Seq Vec2 }

data Direction = U | D | L | R

data GameState = GameState
    { _arena  :: Arena
    , _snake  :: Snake
    , _edible :: Vec2 }

makeLenses ''Arena
makeLenses ''Vec2
makeLenses ''Snake
makeLenses ''GameState
makePrisms ''Direction

paintGameState :: GameState -> Vty.Picture
paintGameState state = Vty.picForLayers (arenaLayer : translate 1 1 edibleLayer : map (translate 1 1) snakeLayers)
  where
    arenaLayer = Vty.vertCat
        [ horizontalBorder
        , Vty.horizCat
            [ verticalBorder
            , Vty.backgroundFill (view (arena . arenaWidth) state) (view (arena . arenaHeight) state)
            , verticalBorder ]
        , horizontalBorder ]
    horizontalBorder = Vty.charFill defAttr '-' (view (arena . arenaWidth) state + 2) 1
    verticalBorder = Vty.charFill defAttr '|' 1 (view (arena . arenaHeight) state)

    edibleLayer = drawChar '*' (view edible state)
    snakeLayers = toList (fmap (drawChar 'o') (view (snake . body) state))

    drawChar c (Vec2 x y) = Vty.translate x y (Vty.char Vty.defAttr c)

initialGameState :: GameState
initialGameState = GameState
    { _arena = initialArena
    , _snake = initialSnake
    , _edible = initialEdible
    }

initialEdible :: Vec2
initialEdible = Vec2 15 15

initialArena :: Arena
initialArena = Arena
    { _arenaHeight = 20
    , _arenaWidth = 20 }

initialSnake :: Snake
initialSnake = Snake
    { _body = Seq.fromList [Vec2 10 10, Vec2 10 11, Vec2 10 12]
    , _direction = U }

moveSnake :: Snake -> Snake
moveSnake s = set body newBody s
  where
    newHead = case view direction s of
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

withVty :: (Vty -> IO a) -> IO a
withVty = bracket (Vty.standardIOConfig >>= Vty.mkVty) Vty.shutdown

main = withVty (\vty -> do
    (addClockEvent, fireClockEvent) <- newAddHandler

    eventNetwork <- compile (do
        eClock <- fromAddHandler addClockEvent

        bSnake <- accumB initialSnake (fmap (const moveSnake) eClock)
        let bArena = pure initialArena
        let bEdible = pure initialEdible

        let bGameState = GameState <$> bArena <*> bSnake <*> bEdible

        reactimate (fmap (\gameState -> Vty.update vty (paintGameState gameState))
                         (bGameState <@ eClock))
        )
    actuate eventNetwork

    withClock 1 (fireClockEvent ()) (do
        input <- Vty.nextEvent vty
        pure () ))

withClock
    :: Int           -- ^ Tick periodicity
    -> IO tickAction -- ^ Action to run each tick
    -> IO body       -- ^ Action to perform with the clock ticking
    -> IO body
withClock ticksPerSecond tickAction stuff = bracket acquire release (const stuff)
  where
    acquire = async (forever (do
        threadDelay (1e6 `quot` ticksPerSecond)
        tickAction ))
    release = cancel
