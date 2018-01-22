{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interval where

import Data.Bool (bool)
import Data.Foldable (toList)
data Diff a = Diff
  { diffBefore :: a
  , diffAfter :: a
  }

instance (Eq a, Show a) => Show (Diff a) where
  show (Diff a b) | a == b = ""
                  | otherwise = show a ++ " => " ++ show b


data Hand
  = LeftHand
  | RightHand
  deriving (Eq, Ord, Show, Enum, Bounded, Read)

handed :: a -> a -> Hand -> a
handed l _ LeftHand  = l
handed _ r RightHand = r

data Mode
  = Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolydian
  | Aeolian
  | Locrian
  deriving (Eq, Ord, Show, Enum, Bounded)

data Note = C | C' | D | D' | E | F | F' | G | G'| A | A' | B
  deriving (Eq, Ord, Show, Enum, Bounded, Read)

pattern Ab = G'
pattern Bb = A'
pattern Db = C'
pattern Eb = D'
pattern Gb = F'

data Interval = Uni | Min2 | Maj2 | Min3 | Maj3 | Perf4 | Tri | Perf5 | Min6 | Maj6 | Min7 | Maj7
  deriving (Eq, Ord, Show, Enum, Bounded)

pattern Oct = Uni
pattern Min9 = Min2
pattern Maj9 = Maj2
pattern Perf11 = Perf4
pattern Min13 = Min6
pattern Maj13 = Maj6

clamp :: Int -> Int -> Int
clamp a x | x >= 0 && x < a = x
clamp a x | x < 0           = clamp12 $ x + a
clamp a x | x >= a          = clamp12 $ x - a

clamp12 :: Int -> Int
clamp12 = clamp 12

toInterval :: Integer -> Interval
toInterval = toEnum . clamp12 . fromIntegral

mod12 :: (Enum a, Enum b, Enum c) => (Int -> Int -> Int) -> a -> b -> c
mod12 f a b = toEnum $ clamp12 $ fromEnum a `f` fromEnum b

iMinus :: Note -> Note -> Interval
iMinus = mod12 (-)

iAdd :: Interval -> Interval -> Interval
iAdd = mod12 (+)

iPlus :: Note -> Interval -> Note
iPlus = mod12 (+)

modeOf :: Mode -> [a] -> [a]
modeOf _ [] = []
modeOf n xs = zipWith const (drop (fromEnum n) (cycle xs)) xs

containsAll :: (Eq a, Foldable t) => [a] -> t a -> Bool
containsAll as ta = all (`elem` toList ta) as

readNote :: Char -> Note
readNote 'A' = A
readNote 'B' = B
readNote 'C' = C
readNote 'D' = D
readNote 'E' = E
readNote 'F' = F
readNote 'G' = G
readNote x = error $ show x

baseName :: Note -> Char
baseName = read . (\x -> '\'':x:'\'':[]) . head . show

intervalSize :: Interval -> Int
intervalSize Uni   = 1
intervalSize Min2  = 2
intervalSize Maj2  = 2
intervalSize Min3  = 3
intervalSize Maj3  = 3
intervalSize Perf4 = 4
intervalSize Tri   = 4
intervalSize Perf5 = 5
intervalSize Min6  = 6
intervalSize Maj6  = 6
intervalSize Min7  = 7
intervalSize Maj7  = 7

isBlack :: Note -> Bool
isBlack A' = True
isBlack C' = True
isBlack D' = True
isBlack F' = True
isBlack G' = True
isBlack _ = False

nameOfNote :: Note -> Interval -> String
nameOfNote a Uni = fmap (\x -> bool x '#' $ x == '\'') $ show a
nameOfNote a i =
  let b = iPlus a i
      base = baseName a
      newbase = readNote
              . toEnum
              . (+ fromEnum 'A')
              . clamp 7
              . ((+) $ intervalSize i - 1)
              . (subtract $ fromEnum 'A')
              $ fromEnum base
      f x = (`mod` 12) $ fromEnum x - fromEnum a
      delta = f newbase - f b
      add = replicate (abs delta) $ if delta > 0 then 'b' else '#'
   in show newbase ++ add

gather :: forall a. Int -> [a] -> [[a]]
gather n = go n n []
  where
    go :: Int -> Int -> [a] -> [a] -> [[a]]
    go _ _ c [] = [c]
    go t 0 c as = c : go t t [] as
    go t n c (a : as) = go t (n - 1) (c ++ [a]) as

