{-# LANGUAGE PatternSynonyms #-}

module Interval where

data Note = A | A' | B | C | C' | D | D' | E | F | F' | G | G'
  deriving (Eq, Ord, Show, Enum, Bounded)

pattern Ab = G'
pattern Bb = A'
pattern Db = C'
pattern Eb = D'
pattern Gb = F'

data Interval = Uni | Min2 | Maj2 | Min3 | Maj3 | Perf4 | Tri | Perf5 | Min6 | Maj6 | Min7 | Maj7
  deriving (Eq, Ord, Show, Enum, Bounded)

clamp12 :: Int -> Int
clamp12 x | x >= 0 && x < 12 = x
clamp12 x | x < 0            = clamp12 $ x + 12
clamp12 x | x >= 12          = clamp12 $ x - 12

toInterval :: Integer -> Interval
toInterval = toEnum . clamp12 . fromIntegral

iMinus :: Note -> Note -> Interval
iMinus a b = toEnum $ clamp12 $ fromEnum a - fromEnum b

iAdd :: Interval -> Interval -> Interval
iAdd a b = toEnum $ clamp12 $ fromEnum a + fromEnum b

iPlus :: Note -> Interval -> Note
iPlus a b = toEnum $ clamp12 $ fromEnum a + fromEnum b

