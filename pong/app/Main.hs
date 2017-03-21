{-# LANGUAGE NumDecimals     #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Reactive.Banana
import Reactive.Banana.Combinators
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
    :: AddHandler Char
    -> AddHandler Integer
    -> MomentIO ()
makeNetworkDescription addKeyEvent addClockEvent = do
    eKey <- fromAddHandler addKeyEvent

    eTime <- do
        eTick <- fromAddHandler addClockEvent
        accumE 0 (fmap (+) eTick)

    error "TODO: render on time tick, but allow key events at any time"


main :: IO ()
main = do
    (addKeyEvent, fireKey) <- newAddHandler
    (addClockEvent, fireClock) <- newAddHandler
    network <- compile (makeNetworkDescription addKeyEvent addClockEvent)
    actuate network
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    _ <- forkIO (let fps = 60
                 in forever (threadDelay (1e6 `quot` fps) >> fireClock 1))
    forever (getChar >>= fireKey)
