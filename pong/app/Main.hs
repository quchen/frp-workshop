{-# LANGUAGE TemplateHaskell #-}

module Main where


import Control.Lens
import Reactive.Banana.Frameworks

import Lib



main :: IO ()
main = someFunc

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

networkDescription :: MomentIO ()
networkDescription = do
    undefined
