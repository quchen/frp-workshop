{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Data.Function
import Data.Monoid
import Graphics.Vty               as Vty
import Reactive.Banana            as Frp
import Reactive.Banana.Frameworks as Frp
import System.IO




data Player = Player
    { _score  :: Word
    , _paddle :: Double
    }

data Ball = Ball
    { _position :: Vec2
    , _velocity :: Vec2 }

data Vec2 = Vec2
    { _x :: Double
    , _y :: Double }

data GameState = GameState
    { _leftPlayer  :: Player
    , _rightPlayer :: Player
    , _ball        :: Ball
    }

makeLenses ''Player
makeLenses ''Ball
makeLenses ''Vec2
makeLenses ''GameState

renderToTerminal :: GameState -> IO ()
renderToTerminal = error "TODO: renderToTerminal"

makeNetworkDescription
    :: AddHandler Char -- ^ User input
    -> AddHandler ()   -- ^ Clock tick
    -> MomentIO ()
makeNetworkDescription addKeyEvent addClockTickEvent = do
    eKey <- fromAddHandler addKeyEvent

    eTime <- do
        eTick <- fromAddHandler addClockTickEvent
        accumE (0 :: Int) (fmap (\_ time -> time + 1) eTick)

    bGameState <- do
        bTime <- stepper 0 eTime
        let eUpdateGameState = (,) <$> bTime <@> eKey
        accumB initialGameState (fmap updateGameState eUpdateGameState)

    let eRender = bGameState <@ eTime

    reactimate (fmap (\time -> putStrLn ("Time: " ++ show time)) eTime)
    reactimate (fmap (\key -> putStrLn ("Key press: " ++ show key)) eKey)
    reactimate (fmap (\_gs -> putStrLn "Rendering frame!") eRender)

updateGameState :: (Int, Char) -> GameState -> GameState
updateGameState (time, key) s = undefined time key s

main :: IO ()
main = do

--     cfg <- standardIOConfig
--     vty <- mkVty cfg
--     let line0 = string (defAttr ` withForeColor ` green) "first line"
--         line1 = string (defAttr ` withBackColor ` blue) "second line"
--         img = backgroundFill 30 20 <> line0 <-> line1
--         pic = picForImage img
--     -- let image = render initialGameState
--     --     pic = picForImage image
--     update vty pic
--     e <- nextEvent vty
--     shutdown vty
--     print ("Last event was: " ++ show e)
--
-- foo = do


    (addKeyEvent, fireKey) <- newAddHandler
    (addClockTickEvent, fireClock) <- newAddHandler
    network <- compile (makeNetworkDescription addKeyEvent addClockTickEvent)
    actuate network

    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    withClock 30 (fireClock ()) (
        fix (\loop -> getChar >>= \case
            'q' -> putStrLn "Quit"
            key -> fireKey key >> loop ))

withClock
    :: Int  -- ^ Ticks per second
    -> IO a -- ^ Action to run each tick
    -> IO b -- ^ Action to perform with the clock ticking
    -> IO b
withClock ticksPerSecond tickAction body = do
    thread <- async (forever (do
        threadDelay (1e6 `quot` ticksPerSecond)
        tickAction ))
    result <- body
    cancel thread
    pure result

initialGameState :: GameState
initialGameState = GameState
    { _leftPlayer = Player
        { _score = 0
        , _paddle = 0 }
    , _rightPlayer = Player
        { _score = 0
        , _paddle = 0 }
    , _ball = Ball
        { _position = Vec2 0 0
        , _velocity = Vec2 0 0 }
    }

render :: GameState -> Image
render s = error "TODO" s
  where
    renderPaddle :: Player -> Image
    renderPaddle = error "TODO"
