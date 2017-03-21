{-----------------------------------------------------------------------------
    reactive-banana
    Example: "The world's worst synthesizer"
    from the unofficial tutorial.
    <http://wiki.haskell.org/FRP_explanation_using_reactive-banana>
------------------------------------------------------------------------------}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Concurrent
import Control.Monad      (forever)
import Data.Char          (toUpper)
import System.IO          (BufferMode (..), hSetBuffering, hSetEcho, stdin)

import Reactive.Banana
import Reactive.Banana.Frameworks


type Octave = Int

data Pitch = PA | PB | PC | PD | PE | PF | PG
    deriving (Eq, Enum)

-- Mapping between pitch and the char responsible for it.
pitchChars :: [(Pitch, Char)]
pitchChars = [(p, toEnum $ fromEnum 'a' + fromEnum p) |
              p <- [PA .. PG]]

-- Reverse of pitchChars
charPitches :: [(Char, Pitch)]
charPitches = [(b, a) | (a, b) <- pitchChars]

data Note = Note Octave Pitch

instance Show Pitch where
    show p = case lookup p pitchChars of
        Nothing -> error "cannot happen"
        Just c  -> [toUpper c]

instance Show Note where
    show (Note o p) = show p ++ show o

-- Filter and transform events at the same time.
filterMapJust :: (a -> Maybe b) -> Event a -> Event b
filterMapJust f = filterJust . fmap f

-- Change the original octave by adding a number of octaves, taking
-- care to limit the resulting octave to the 0..10 range.
changeOctave :: Int -> Octave -> Octave
changeOctave d = max 0 . min 10 . (d+)

-- Get the octave change for the '+' and '-' chars.
getOctaveChange :: Char -> Maybe Int
getOctaveChange c = case c of
    '+' -> Just 1
    '-' -> Just (-1)
    _   -> Nothing

makeNetworkDescription :: AddHandler Char -> AddHandler Integer -> MomentIO ()
makeNetworkDescription addKeyEvent addClockEvent = do

    eKey <- fromAddHandler addKeyEvent
    let eOctaveChange = filterMapJust getOctaveChange eKey
    bOctave <- accumB 3 (changeOctave <$> eOctaveChange)

    let ePitch = filterMapJust (`lookup` charPitches) eKey
    bPitch <- stepper PC ePitch

    let bNote = Note <$> bOctave <*> bPitch

    eNoteChanged <- changes bNote
    reactimate' $ fmap (\n -> putStrLn ("Now playing " ++ show n))
                 <$> eNoteChanged

    eTick <- fromAddHandler addClockEvent
    eTime <- accumE 0 (fmap (+) eTick)
    reactimate (fmap (\t -> putStrLn ("Tick " ++ show t)) eTime)

main :: IO ()
main = do
    (addKeyEvent, fireKey) <- newAddHandler
    (addClockEvent, fireClock) <- newAddHandler
    network <- compile (makeNetworkDescription addKeyEvent addClockEvent)
    actuate network
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    _ <- forkIO (forever (threadDelay 1e5 >> fireClock 1))
    forever (getChar >>= fireKey)
