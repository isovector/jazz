module Main where

import Control.Monad (guard)
import Interval
import Data.Bool (bool)
import Debug.Trace
import Data.List (intercalate)

showTrace :: Show a => a -> a
showTrace = trace =<< show

main = undefined

circleOfFifths :: [Note]
circleOfFifths = iterate (`iPlus` Perf5) C

intsFromNotes :: [Note] -> [Interval]
intsFromNotes notes = fmap (\x -> x `iMinus` head notes) notes

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

chord :: [Note] -> String
chord ns = ('(' :)
         . (++ "/4)")
         . fmap (\x -> bool x '@' $ x == 'b')
         . intercalate "-"
         . fmap (nameOfNote $ head ns)
         $ intsFromNotes ns

ascChord :: [Note] -> String
ascChord ns = ('(' :)
            . (++ ")")
            . fmap (\x -> bool x '@' $ x == 'b')
            . fmap (\x -> bool x '#' $ x == '\'')
            . intercalate "."
            . fmap (asc $ head ns)
            . zip ns
            $ intsFromNotes ns
  where
    asc :: Note -> (Note, Interval) -> String
    asc r (n, i) = show n ++ bool "/4" "/5" (r > n)


practice2'5'1 :: String
practice2'5'1 = concat $ do
  each <- gather 2 $ do
    note <- reverse $ take 12 circleOfFifths
    pure . (++ "^3^") . concat . fmap (ascChord . transpose note) $ progression2'5'1

  pure $ "stave\nnotes " ++  concat each ++ "\n\n"


modeRules :: Mode -> [Diff Interval]
modeRules m = fmap (uncurry Diff)
            . filter (uncurry (/=))
            . zip (intsFromNotes $ transpose C major)
            . intsFromNotes
            . transpose C
            $ modeOf m major


fingeringOf :: Hand -> [Note] -> [Int]
fingeringOf hand notes = head $ do
  fingers <- handed reverse id hand <$> patterns
  guard . not
        . or
        . fmap (\(n, f) -> isBlack n && f == 1)
        . zip notes
        $ fingers
  pure fingers


  where
    patterns = [ [1..3] ++ [1..5]
               , [1..4] ++ [1..4]
               ]

