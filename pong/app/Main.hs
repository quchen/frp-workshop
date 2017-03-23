{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
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
    , _fieldWidth  :: Int
    , _fieldHeight :: Int
    }

data PlayerEvent
    = MoveLeftPaddle  Int
    | MoveRightPaddle Int

makeLenses ''Player
makeLenses ''Ball
makeLenses ''Paddle
makeLenses ''Vec2Cart
makeLenses ''Vec2Rad
makeLenses ''GameState

renderToTerminal :: GameState -> IO ()
renderToTerminal = error "TODO: renderToTerminal"

makeNetworkDescription
    :: Vty                    -- ^ Vty to render to
    -> AddHandler PlayerEvent -- ^ User input
    -> AddHandler ()          -- ^ Clock tick
    -> MomentIO ()
makeNetworkDescription vty addPlayerEvent addClockTickEvent = do
    ePlayer <- fromAddHandler addPlayerEvent

    eTime <- do
        eTick <- fromAddHandler addClockTickEvent
        accumE (0 :: Int) (fmap (\_ time -> time + 1) eTick)

    let ePaddleMove :: Frp.Event (GameState -> GameState)
        ePaddleMove = flip fmap ePlayer (\case
            MoveLeftPaddle delta -> movePaddle leftPlayer delta
            MoveRightPaddle delta -> movePaddle rightPlayer delta )

    let eInertia, eCollisionWithField, eCollisionWithPaddle:: Frp.Event (GameState -> GameState)
        eInertia = fmap (const inertia) eTime
        eCollisionWithField = fmap (const collisionWithField) eTime
        eCollisionWithPaddle = fmap (const collisionWithPaddle) eTime

    eRender <- accumE (initialGameState (100, 30) 10) (unions
        [ ePaddleMove
        , eInertia
        , eCollisionWithField
        , eCollisionWithPaddle ] )

    reactimate (fmap (update vty . render) eRender)

mapMaybe :: (a -> Maybe b) -> Frp.Event a -> Frp.Event b
mapMaybe f es = filterJust (fmap f es)

movePaddle :: Lens' GameState Player -> Int -> GameState -> GameState
movePaddle player delta = do
    top <- view (player . paddle . pPos . y)
    bot <- (+ top) . view (player . paddle . pHeight)
    maxBot <- view (fieldHeight . to fromIntegral)
    let delta' = fromIntegral delta
    over (player . paddle . pPos . y) (\yPos ->
        if | top + delta' <= 0 -> yPos
           | bot + delta' >= maxBot + 2 -> yPos -- +2 works. Don’t ask me why. :-|
           | otherwise -> yPos + delta' )

collisionWithField :: GameState -> GameState
collisionWithField = do
    ballPosY <- view (ball . position . y)
    fieldLowerBound <- view fieldHeight
    let mirrorBall = over (ball . velocity . phi) negate
    if | ballPosY >= (fromIntegral fieldLowerBound + 1) -> mirrorBall
        | ballPosY <= 1 -> mirrorBall
        | otherwise -> id

collisionWithPaddle :: GameState -> GameState
collisionWithPaddle = do
    ballPos <- view (ball . position)
    lPlayer <- view leftPlayer
    rPlayer <- view rightPlayer
    let mirrorBall = over (ball . velocity . phi) (pi -)
    if | ballPos `collidesWith` lPlayer -> mirrorBall
        | ballPos `collidesWith` rPlayer -> mirrorBall
        | otherwise -> id
  where
    collidesWith :: Vec2Cart -> Player -> Bool
    collidesWith pos player = insideX && insideY
      where
        insideX = let playerXL = view (paddle . pPos . x) player
                      playerXR = playerXL + view (paddle . pWidth) player
                  in view (x . to (\xx -> xx >= playerXL && xx <= playerXR)) pos
        insideY = let playerYT = view (paddle . pPos . y) player
                      playerYB = playerYT + view (paddle . pHeight) player
                  in view (y . to (\yy -> yy >= playerYT && yy <= playerYB)) pos

inertia :: GameState -> GameState
inertia = do
    Vec2Cart xPos yPos <- view (ball . position)
    Vec2Rad magnitude angle <- view (ball . velocity)
    let newPos = Vec2Cart (xPos + magnitude * cos angle)
                          (yPos + magnitude * sin angle)
    set (ball . position) newPos

main :: IO ()
main = withVty standardIOConfig (\vty -> do

    (firePlayerEvent, fireClockEvent) <- do
        (addPlayerEvent, firePlayerEvent) <- newAddHandler
        (addClockTickEvent, fireClockEvent) <- newAddHandler
        network <- compile (makeNetworkDescription vty addPlayerEvent addClockTickEvent)
        actuate network
        pure (firePlayerEvent, fireClockEvent)

    withClock 60 (fireClockEvent ()) (
        fix (\loop -> nextEvent vty >>= \case
            EvKey KEsc _        -> pure ()
            EvKey (KChar 'q') _ -> pure ()
            EvKey KUp _         -> firePlayerEvent (MoveLeftPaddle (-1)) >> loop
            EvKey KDown _       -> firePlayerEvent (MoveLeftPaddle   1 ) >> loop
            _otherwise          ->  loop
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

initialGameState :: (Int, Int) -> Int -> GameState
initialGameState (fWidth, fHeight) paddleHeight = GameState
    { _leftPlayer = Player
        { _score = 0
        , _paddle = Paddle
            { _pWidth = 1
            , _pHeight = fromIntegral paddleHeight
            , _pPos = Vec2Cart 1 (fromIntegral fHeight / 2 - fromIntegral paddleHeight / 2) } }
    , _rightPlayer = Player
        { _score = 0
        , _paddle = Paddle
            { _pWidth = 1
            , _pHeight = fromIntegral paddleHeight * 2
            , _pPos = Vec2Cart (fromIntegral fWidth) (fromIntegral fHeight / 2 - fromIntegral paddleHeight / 2) } }
    , _ball = Ball
        { _position = Vec2Cart 5 (fromIntegral fHeight / 2)
        , _velocity = Vec2Rad 1 0.05 }
    , _fieldWidth = fWidth
    , _fieldHeight = fHeight
    }

render :: GameState -> Picture
render gameState = picForLayers
    [ renderBall (view ball gameState)
    , renderLeftPlayer
    , renderRightPlayer
    , renderGameField
    ]
  where
    renderLeftPlayer  = renderPaddle (view leftPlayer  gameState)
    renderRightPlayer = renderPaddle (view rightPlayer gameState)

    renderGameField = horizCat
        [ charFill defAttr '|' 1 (view fieldHeight gameState + 2)
        , vertCat
            [ charFill defAttr '-' (view fieldWidth gameState) 1
            , charFill defAttr ' ' (view fieldWidth gameState) (view fieldHeight gameState)
            , charFill defAttr '-' (view fieldWidth gameState) 1
            ]
        , charFill defAttr '|' 1 (view fieldHeight gameState + 2)
        ]

    renderPaddle :: Player -> Image
    renderPaddle player = translate xOffset yOffset paddleImage
      where
        xOffset = view (paddle . pPos . x . to round) player
        yOffset = view (paddle . pPos . y . to round) player
        paddleImage =
            let viewPlayer l = view (paddle . l . to round) player :: Int
            in charFill defAttr '#' (viewPlayer pWidth) (viewPlayer pHeight)

    renderBall :: Ball -> Image
    renderBall b = translate (round (view (position . x) b))
                             (round (view (position . y) b))
                             (text' defAttr "o")
