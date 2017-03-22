{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
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
    , _paddle :: Paddle
    }

data Paddle = Paddle
    { _pWidth  :: Double
    , _pHeight :: Double
    , _pPos    :: Vec2Cart }

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
makeLenses ''Paddle
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
    let eKey = flip mapMaybe eVty (\case
            EvKey key modifiers -> Just (key, modifiers)
            _other -> Nothing
            )

    eTime <- do
        eTick <- fromAddHandler addClockTickEvent
        accumE (0 :: Int) (fmap (\_ time -> time + 1) eTick)

    let ePaddleMove :: Frp.Event (GameState -> GameState)
        ePaddleMove = fmap (\(key, _mod) -> movePaddle key) eKey

    let eBallMove :: Frp.Event (GameState -> GameState)
        eBallMove = fmap (const moveBall) eTime

    eRender <- accumE initialGameState (unions [ePaddleMove, eBallMove])

    reactimate (fmap (update vty . render) eRender)

mapMaybe :: (a -> Maybe b) -> Frp.Event a -> Frp.Event b
mapMaybe f es = filterJust (fmap f es)

movePaddle :: Vty.Key -> GameState -> GameState
movePaddle = \case
    KUp    -> over (leftPlayer . paddle . pPos . y) (subtract 1)
    KDown  -> over (leftPlayer . paddle . pPos . y) (+ 1)
    _other -> id

moveBall :: GameState -> GameState
moveBall = collision . inertia
  where
    collision = do
      ballPos <- view (ball . position)
      lPlayer <- view leftPlayer
      rPlayer <- view rightPlayer
      let mirrorBall = over (ball . velocity . phi) (pi -)
      if | ballPos `isInside` lPlayer -> mirrorBall
         | ballPos `isInside` rPlayer -> mirrorBall
         | otherwise -> id

    isInside :: Vec2Cart -> Player -> Bool
    isInside pos player = insideX && insideY
      where
        insideX = let playerXL = view (paddle . pPos . x) player
                      playerXR = playerXL + view (paddle . pWidth) player
                  in view (x . to (\xx -> xx >= playerXL && xx <= playerXR)) pos
        insideY = let playerYT = view (paddle . pPos . y) player
                      playerYB = playerYT + view (paddle . pHeight) player
                  in view (y . to (\yy -> yy >= playerYT && yy <= playerYB)) pos

    inertia = do
        Vec2Cart xPos yPos <- view (ball . position)
        Vec2Rad magnitude angle <- view (ball . velocity)
        let newPos = Vec2Cart (xPos + magnitude * cos angle)
                              (yPos + magnitude * sin angle)
        set (ball . position) newPos

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
        , _paddle = Paddle
            { _pWidth = 1
            , _pHeight = 10
            , _pPos = Vec2Cart 0 0 } }
    , _rightPlayer = Player
        { _score = 0
        , _paddle = Paddle
            { _pWidth = 1
            , _pHeight = 10
            , _pPos = Vec2Cart 40 0 } }
    , _ball = Ball
        { _position = Vec2Cart 10 0
        , _velocity = Vec2Rad 1 0 }
    }

render :: GameState -> Picture
render gameState = picForLayers
    [ renderBall (view ball gameState)
    , renderLeftPlayer
    , renderRightPlayer
    ]
  where
    renderLeftPlayer  = renderPaddle (view leftPlayer  gameState)
    renderRightPlayer = renderPaddle (view rightPlayer gameState)

    renderPaddle :: Player -> Image
    renderPaddle player = translate xOffset yOffset paddleImage
      where
        style = mempty `withForeColor` white `withBackColor` black
        xOffset = view (paddle . pPos . x . to round) player
        yOffset = view (paddle . pPos . y . to round) player
        paddleImage =
            let viewPlayer l = view (paddle . l . to round) player :: Int
            in horizCat
                [ charFill style 'x' (viewPlayer pWidth) (viewPlayer pHeight)
                , charFill defAttr ' ' 1 (viewPlayer pHeight) ]

    renderBall :: Ball -> Image
    renderBall b = translate (round (view (position . x) b))
                             (round (view (position . y) b))
                             (text' defAttr "o")
