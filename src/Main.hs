module Main where

import Interval
import Data.Foldable (toList)

main = undefined

circleOfFifths :: [Note]
circleOfFifths = iterate (`iPlus` Perf5) C

intsFromNotes :: [Note] -> [Interval]
intsFromNotes (tonic : others) = Uni : fmap (\x -> x `iMinus` tonic) others

major7 :: [Interval]
major7 = intsFromNotes [ C, E, G, B ]

minor7 :: [Interval]
minor7 = intsFromNotes [ C, Eb, G, Bb ]

dom7 :: [Interval]
dom7 = intsFromNotes [ C, E, G, Bb ]

major :: [Interval]
major = intsFromNotes [ C, D, E, F, G, A, B ]

minor :: [Interval]
minor = intsFromNotes [ C, D, Eb, F, G, A, Bb ]

allScales :: [Scale]
allScales =
  [ (tonic, scale, mode)
  | tonic <- [minBound .. maxBound]
  , scale <- [major, minor]
  , mode  <- [minBound .. maxBound]
  ]

type Scale = (Note, [Interval], Mode)

getNotes :: Scale -> [Note]
getNotes (tonic, scale, mode) = modeOf mode $ transpose tonic scale


transpose :: Note -> [Interval] -> [Note]
transpose n = fmap (iPlus n)


progression2'5'1 :: [[Interval]]
progression2'5'1 = [ fmap (iAdd Maj2) minor7
                   , fmap (iAdd Perf5) dom7
                   , major7
                   ]

data BasicChord = Major | Minor | Dominant | HalfDim
  deriving (Eq, Ord, Show, Enum, Bounded)

classify :: [Interval] -> Maybe BasicChord
classify ints
  | Tri  `elem` ints && Min7 `elem` ints = Just HalfDim
  | Maj3 `elem` ints && Maj7 `elem` ints = Just Major
  | Min3 `elem` ints && Min7 `elem` ints = Just Minor
  | Maj3 `elem` ints && Min7 `elem` ints = Just Dominant
  | otherwise = Nothing

promote :: Note -> [Interval] -> [Scale]
promote n ints = filter (containsAll (transpose n ints) . getNotes) allScales

containsAll :: (Eq a, Foldable t) => [a] -> t a -> Bool
containsAll as ta = all (`elem` toList ta) as

