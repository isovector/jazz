{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import  Interval

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

transpose :: Note -> [Interval] -> [Note]
transpose n = fmap (iPlus n)


progression2'5'1 :: [[Interval]]
progression2'5'1 = [ fmap (iAdd Maj2) minor7
                   , fmap (iAdd Perf5) dom7
                   , major7
                   ]

