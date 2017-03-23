{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where



import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Function
import Graphics.Vty               as Vty
import Reactive.Banana            as Frp
import Reactive.Banana.Frameworks as Frp
import System.Random



renderSpeed, physicsSpeed, enemyMoveSpeed :: PerSecond
renderSpeed = PerSecond 60
physicsSpeed = PerSecond 600
enemyMoveSpeed = PerSecond 10




newtype PerSecond = PerSecond Int

data Score = Score
    { _leftPlayer  :: Word
    , _rightPlayer :: Word }

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
    { _leftPaddle  :: Paddle
    , _rightPaddle :: Paddle
    , _ball        :: Ball
    , _fieldWidth  :: Int
    , _fieldHeight :: Int
    }

data PlayerEvent
    = MoveLeftPaddle  Int
    | MoveRightPaddle Int

data ScoreEvent
    = LeftPlayerScores
    | RightPlayerScores

makeLenses ''Score
makeLenses ''Ball
makeLenses ''Paddle
makeLenses ''Vec2Cart
makeLenses ''Vec2Rad
makeLenses ''GameState

makeNetworkDescription
    :: Vty                     -- ^ Vty to render to
    -> [Double]                -- ^ Infinite supply of random values in range 0..1
    -> AddHandler PlayerEvent  -- ^ User input
    -> Frp.Handler PlayerEvent -- ^ Fire player events
    -> AddHandler ()           -- ^ Render clock tick
    -> AddHandler ()           -- ^ Physics clock tick
    -> AddHandler ()           -- ^ Opponent clock tick
    -> MomentIO ()
makeNetworkDescription
    vty
    randomNumbers
    addPlayerEvent
    firePlayerEvent
    addRenderEvent
    addPhysicsEvent
    addOpponentEvent

  = do

    ePaddleMove <- mkEPaddleMoves addPlayerEvent
    eGameTime <- mkEGameTime addPhysicsEvent
    ePhysicsChange <- mkEPhysics eGameTime randomNumbers
    (bGame, fireGameEvent) <- mkBGame [ePaddleMove, ePhysicsChange]
    eOpponent <- mkEOpponent addOpponentEvent bGame


    do eRender <- fromAddHandler addRenderEvent
       reactimate (fmap (update vty . render (Score 0 0)) (bGame <@ eRender))
    reactimate (fmap firePlayerEvent eOpponent)
    do let eScore = mapMaybe scoreWhenOutOfBounds (bGame <@ eGameTime)
       reactimate (fmap fireGameEvent eScore)

  where

    mkEPaddleMoves addEvent = do
        ePlayer <- fromAddHandler addEvent
        let playerEventToAction = \case
                MoveLeftPaddle delta  -> movePaddle leftPaddle delta
                MoveRightPaddle delta -> movePaddle rightPaddle delta
        pure (fmap playerEventToAction ePlayer)

    mkEGameTime addEvent = do
        eTick <- fromAddHandler addEvent
        accumE (0 :: Int) (fmap (\_ time -> time + 1) eTick)

    mkEPhysics eGameTime infiniteNumberSupply = mdo
        let drawRandom (_current, nextRandom : doubles) = (nextRandom, doubles)
            drawRandom (_, []) = error "Finite supply of randoms exhausted"
        bRandom <- (fmap . fmap) fst (accumB (0, infiniteNumberSupply) (drawRandom <$ eCollisionWithPaddle))
        let eInertia = fmap (const inertia) eGameTime
            eCollisionWithField = fmap (const collisionWithField) eGameTime
            eCollisionWithPaddle = fmap collisionWithPaddle (bRandom <@ eGameTime)
        pure (unions [eInertia, eCollisionWithField, eCollisionWithPaddle])

    mkBGame gameStateEvents = do
        (addGameEvent, fireGameEvent) <- liftIO newAddHandler
        ev <- fromAddHandler addGameEvent
        let newMovementB :: Moment (Behavior GameState)
            newMovementB = accumB (initialGameState (100, 30) 10)
                                  (unions gameStateEvents)

        let eChangeMovements :: Frp.Event (Behavior GameState)
            eChangeMovements = observeE (flip fmap ev (\case
                LeftPlayerScores  -> newMovementB
                RightPlayerScores -> newMovementB ))

        bMovementsInitial <- liftMoment newMovementB
        bGame <- switchB bMovementsInitial eChangeMovements
        pure (bGame, fireGameEvent)

    mkEOpponent addEvent bGame = do
        eOpponentTick <- fromAddHandler addEvent
        pure (mapMaybe opponent (bGame <@ eOpponentTick))

mapMaybe :: (a -> Maybe b) -> Frp.Event a -> Frp.Event b
mapMaybe f es = filterJust (fmap f es)

movePaddle :: Lens' GameState Paddle -> Int -> GameState -> GameState
movePaddle paddle delta = do
    top <- view (paddle . pPos . y)
    bot <- (+ top) . view (paddle . pHeight)
    maxBot <- view (fieldHeight . to fromIntegral)
    let delta' = fromIntegral delta
    over (paddle . pPos . y) (\yPos ->
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

collisionWithPaddle :: Double -> GameState -> GameState
collisionWithPaddle randomDouble = do
    ballPos <- view (ball . position)
    lPaddle <- view leftPaddle
    rPaddle <- view rightPaddle
    let mirrorBall = over (ball . velocity . phi) (pi -)
                   . over (ball . velocity . r)   (1.1 *)
    let angleNoise = (2 * pi * (randomDouble - 0.5)) / 20
        noisyBall = over (ball . velocity . phi) (\angle ->
            let angle' = normalizeAngle (angle + angleNoise)
            in if angleAllowed angle'
                then angle'
                else angle )
    if | ballPos `collidesWith` lPaddle -> noisyBall . mirrorBall
       | ballPos `collidesWith` rPaddle -> noisyBall . mirrorBall
       | otherwise -> id
  where

    angleAllowed xx = not (between (deg2rad (90-10))  (deg2rad (90+10))  xx)
                   && not (between (deg2rad (270-10)) (deg2rad (270+10)) xx)
    deg2rad xx = xx * 2 * pi / 360

    between lo hi xx | lo > hi = between hi lo xx
    between lo hi xx = xx >= lo && xx <= hi

    normalizeAngle xx
        | xx > 2 * pi = normalizeAngle (xx - 2 * pi)
        | xx < 0      = normalizeAngle (xx + 2 * pi)
        | otherwise   = xx

    collidesWith :: Vec2Cart -> Paddle -> Bool
    collidesWith pos paddle = insideX && insideY
      where
        insideX = let paddleXL = view (pPos . x) paddle
                      paddleXR = paddleXL + view pWidth paddle
                  in view (x . to (\xx -> xx >= paddleXL && xx <= paddleXR)) pos
        insideY = let paddleYT = view (pPos . y) paddle
                      paddleYB = paddleYT + view pHeight paddle
                  in view (y . to (\yy -> yy >= paddleYT && yy <= paddleYB)) pos

inertia :: GameState -> GameState
inertia = do
    Vec2Cart xPos yPos <- view (ball . position)
    Vec2Rad magnitude angle <- view (ball . velocity)
    let newPos = Vec2Cart (xPos + magnitude * cos angle)
                          (yPos + magnitude * sin angle)
    set (ball . position) newPos

opponent :: GameState -> Maybe PlayerEvent
opponent = do
    yBall <- view (ball . position . y)
    paddleTop <- view (rightPaddle . pPos . y)
    paddleHeight <- view (rightPaddle . pHeight)
    let yPlayer = paddleTop + paddleHeight / 2
    pure (if | yBall < (yPlayer - 1) -> Just (MoveRightPaddle (-1))
             | yBall > (yPlayer + 1) -> Just (MoveRightPaddle 1)
             | otherwise             -> Nothing )

scoreWhenOutOfBounds :: GameState -> Maybe ScoreEvent
scoreWhenOutOfBounds = do
    xBall <- view (ball . position . x)
    fWidth <- view (fieldWidth . to fromIntegral)
    pure (if | xBall < 0      -> Just RightPlayerScores
             | xBall > fWidth -> Just LeftPlayerScores
             | otherwise      -> Nothing )

main :: IO ()
main = withVty standardIOConfig (\vty -> do

    randomNumbers <- fmap (randomRs (0,1)) getStdGen
    (firePlayerEvent, fireRenderEvent, firePhysicsEvent, fireOpponentEvent) <- do
        (addPlayerEvent,   firePlayerEvent  ) <- newAddHandler
        (addPhysicsEvent,  firePhysicsEvent ) <- newAddHandler
        (addRenderEvent,   fireRenderEvent  ) <- newAddHandler
        (addOpponentEvent, fireOpponentEvent) <- newAddHandler
        network <- compile
            (makeNetworkDescription
                vty
                randomNumbers
                addPlayerEvent
                firePlayerEvent
                addRenderEvent
                addPhysicsEvent
                addOpponentEvent )
        actuate network
        pure (firePlayerEvent, fireRenderEvent, firePhysicsEvent, fireOpponentEvent)

    withClock renderSpeed fireRenderEvent (
        withClock physicsSpeed firePhysicsEvent (
            withClock enemyMoveSpeed fireOpponentEvent (
                fix (\loop -> nextEvent vty >>= \case
                    EvKey KEsc _        -> pure ()
                    EvKey (KChar 'q') _ -> pure ()
                    EvKey KUp _         -> firePlayerEvent (MoveLeftPaddle (-1)) >> loop
                    EvKey (KChar 'k') _ -> firePlayerEvent (MoveLeftPaddle (-1)) >> loop
                    EvKey KDown _       -> firePlayerEvent (MoveLeftPaddle   1 ) >> loop
                    EvKey (KChar 'j') _ -> firePlayerEvent (MoveLeftPaddle   1 ) >> loop
                    _otherwise          -> loop
                    )))))

withVty :: IO Config -> (Vty -> IO a) -> IO a
withVty mkConfig = bracket (mkConfig >>= mkVty) shutdown

withClock
    :: PerSecond    -- ^ Tick periodicity
    -> (() -> IO a) -- ^ Action to run each tick
    -> IO b         -- ^ Action to perform with the clock ticking
    -> IO b
withClock (PerSecond ps) tickAction body = bracket acquire release (const body)
  where
    acquire = async (forever (do
        threadDelay (1e6 `quot` ps)
        tickAction () ))
    release = cancel

initialGameState :: (Int, Int) -> Int -> GameState
initialGameState (fWidth, fHeight) paddleHeight = GameState
    { _leftPaddle = Paddle
        { _pWidth = 1
        , _pHeight = paddleHeightD
        , _pPos = Vec2Cart 1 (fHeightD / 2 - paddleHeightD / 2) }
    , _rightPaddle = Paddle
        { _pWidth = 1
        , _pHeight = paddleHeightD / 2
        , _pPos = Vec2Cart fWidthD (fHeightD / 2 - paddleHeightD / 2) }
    , _ball = Ball
        { _position = Vec2Cart 5 (fHeightD / 2)
        , _velocity = Vec2Rad 0.1 0 }
    , _fieldWidth = fWidth
    , _fieldHeight = fHeight
    }
  where
    [fWidthD, fHeightD, paddleHeightD]
      = map fromIntegral [fWidth, fHeight, paddleHeight] :: [Double]

render :: Score -> GameState -> Picture
render score gameState = picForLayers
    [ renderBall (view ball gameState)
    , renderScore
    , renderLeftPlayer
    , renderRightPlayer
    , renderGameField
    ]
  where
    renderLeftPlayer  = renderPaddle (view leftPaddle  gameState)
    renderRightPlayer = renderPaddle (view rightPaddle gameState)

    renderGameField = horizCat
        [ vertCat
            [ char     defAttr '┌'
            , charFill defAttr '│' 1 (view fieldHeight gameState)
            , char     defAttr '└'
            ]
        , vertCat
            [ charFill defAttr '─' (view fieldWidth gameState) 1
            , charFill defAttr ' ' (view fieldWidth gameState) (view fieldHeight gameState)
            , charFill defAttr '─' (view fieldWidth gameState) 1
            ]
        , vertCat
            [ char     defAttr '┐'
            , charFill defAttr '│' 1 (view fieldHeight gameState)
            , char     defAttr '┘'
            ]
        ]

    renderPaddle :: Paddle -> Image
    renderPaddle paddle = translate xOffset yOffset paddleImage
      where
        xOffset = view (pPos . x . to round) paddle
        yOffset = view (pPos . y . to round) paddle
        paddleImage =
            let viewPlayer l = view (l . to round) paddle :: Int
            in charFill defAttr '█' (viewPlayer pWidth) (viewPlayer pHeight)

    renderBall :: Ball -> Image
    renderBall b = translate (round (view (position . x) b))
                             (round (view (position . y) b))
                             (char defAttr '●')

    renderScore :: Image
    renderScore =
        let halfFieldWidth = view (fieldWidth . to (`quot` 2)) gameState
            scoreLeft  = view leftPlayer score
            scoreRight = view rightPlayer score
            offset = halfFieldWidth - length (show scoreLeft)
            displayScore = show scoreLeft ++ ":" ++ show scoreRight
        in  translate offset 1 (string defAttr displayScore)
