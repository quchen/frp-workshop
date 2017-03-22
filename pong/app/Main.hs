{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NumDecimals     #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Data.Function
import Reactive.Banana
import Reactive.Banana.Frameworks
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

    reactimate (fmap (\time -> putStrLn ("Time: " ++ show time)) eTime)
    reactimate (fmap (\key -> putStrLn ("Key press: " ++ show key)) eKey)

main :: IO ()
main = do
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
