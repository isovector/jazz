{-# LANGUAGE PatternSynonyms #-}

module Interval where


data Mode
  = Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolydian
  | Aeolian
  | Locrian
  deriving (Eq, Ord, Show, Enum, Bounded)

data Note = A | A' | B | C | C' | D | D' | E | F | F' | G | G'
  deriving (Eq, Ord, Show, Enum, Bounded)

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

clamp12 :: Int -> Int
clamp12 x | x >= 0 && x < 12 = x
clamp12 x | x < 0            = clamp12 $ x + 12
clamp12 x | x >= 12          = clamp12 $ x - 12

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
