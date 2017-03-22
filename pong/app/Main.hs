{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Function
import Graphics.Vty               as Vty
import Reactive.Banana            as Frp
import Reactive.Banana.Frameworks as Frp



data Player = Player
    { _score  :: Word
    , _paddle :: Double
    }

data Ball = Ball
    { _position :: Vec2Cart
    , _velocity :: Vec2Rad }

data Vec2Cart = Vec2Cart
    { _x :: Double
    , _y :: Double }

data Vec2Rad = Vec2Rad
    { _r   :: Double
    , _phi :: Double }

data GameState = GameState
    { _leftPlayer  :: Player
    , _rightPlayer :: Player
    , _ball        :: Ball
    }

makeLenses ''Player
makeLenses ''Ball
makeLenses ''Vec2Cart
makeLenses ''Vec2Rad
makeLenses ''GameState

renderToTerminal :: GameState -> IO ()
renderToTerminal = error "TODO: renderToTerminal"

makeNetworkDescription
    :: Vty                  -- ^ Vty to render to
    -> AddHandler Vty.Event -- ^ User input
    -> AddHandler ()        -- ^ Clock tick
    -> MomentIO ()
makeNetworkDescription vty addVtyEvent addClockTickEvent = do
    eVty <- fromAddHandler addVtyEvent

    eTime <- do
        eTick <- fromAddHandler addClockTickEvent
        accumE (0 :: Int) (fmap (\_ time -> time + 1) eTick)

    bGameState <- do
        let eKey = flip fmap eVty (\case
                EvKey key modifiers -> Just (key, modifiers)
                _other -> Nothing
                )
        bKey <- stepper Nothing eKey
        let eUpdateGameState = (,) <$> bKey <@> eTime
        accumB initialGameState (fmap updateGameState eUpdateGameState)

    let eRender = bGameState <@ eTime

    -- reactimate (fmap (\_ -> putStrLn "tick") eTime)
    reactimate (flip fmap eRender (update vty . render))

mapMaybe :: (a -> Maybe b) -> Frp.Event a -> Frp.Event b
mapMaybe f es = filterJust (fmap f es)

updateGameState :: (Maybe (Vty.Key, [Vty.Modifier]), time) -> GameState -> GameState
updateGameState = \case
    (Nothing, _time) -> moveBall
    (Just (key, _modifiers), _time) -> moveBall . userInput key
  where
    userInput key s = case key of
        KUp    -> over (leftPlayer . paddle) (subtract 1) s
        KDown  -> over (leftPlayer . paddle) (+ 1) s
        _other -> s

    moveBall :: GameState -> GameState
    moveBall s =
        let Vec2Cart xPos yPos = view (ball . position) s
            Vec2Rad magnitude angle = view (ball . velocity) s
            newPos = Vec2Cart (xPos + magnitude * cos angle)
                              (yPos + magnitude * sin angle)
        in set (ball . position) newPos s

main :: IO ()
main = withVty standardIOConfig (\vty -> do

    (fireVty, fireClock) <- do
        (addVtyEvent, fireVty) <- newAddHandler
        (addClockTickEvent, fireClock) <- newAddHandler
        network <- compile (makeNetworkDescription vty addVtyEvent addClockTickEvent)
        actuate network
        pure (fireVty, fireClock)

    withClock 30 (fireClock ()) (
        fix (\loop -> nextEvent vty >>= \case
            EvKey KEsc _ -> pure ()
            EvKey (KChar 'q') _ -> pure ()
            ev -> fireVty ev >> loop
            ))
    )

withVty :: IO Config -> (Vty -> IO a) -> IO a
withVty mkConfig = bracket (mkConfig >>= mkVty) shutdown

withClock
    :: Int  -- ^ Ticks per second
    -> IO a -- ^ Action to run each tick
    -> IO b -- ^ Action to perform with the clock ticking
    -> IO b
withClock ticksPerSecond tickAction body = bracket acquire release (const body)
  where
    acquire = async (forever (do
        threadDelay (1e6 `quot` ticksPerSecond)
        tickAction ))
    release = cancel

initialGameState :: GameState
initialGameState = GameState
    { _leftPlayer = Player
        { _score = 0
        , _paddle = 0 }
    , _rightPlayer = Player
        { _score = 0
        , _paddle = 0 }
    , _ball = Ball
        { _position = Vec2Cart 0 0
        , _velocity = Vec2Rad 1 0 }
    }

render :: GameState -> Picture
render gameState = picForLayers
    [ renderBall (view ball gameState)
    , renderLeftPlayer
    , renderRightPlayer
    ]
  where
    renderLeftPlayer  = translate 2       0 (renderPaddle (view leftPlayer  gameState))
    renderRightPlayer = translate (120-2) 0 (renderPaddle (view rightPlayer gameState))

    renderPaddle :: Player -> Image
    renderPaddle player = translate 0 yOffset paddleImage
      where
        style = mempty `withForeColor` white `withBackColor` black
        yOffset = view (paddle . to round) player
        paddleImage = charFill style 'x' (1 :: Int) 10 `horizJoin` charFill defAttr ' ' (1 :: Int) 10

    renderBall :: Ball -> Image
    renderBall b = translate (round (view (position . x) b))
                             (round (view (position . y) b))
                             (text' defAttr "o")
