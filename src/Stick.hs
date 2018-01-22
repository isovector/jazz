{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}

module Stick where

import           Control.Lens
import qualified Data.Array.IArray as A
import qualified Data.Map as M

data Stick a = Stick
  { _stickOffsets :: [Int]
  , _stickData :: A.Array (Int, Int) a
  } deriving (Functor, Eq, Ord, Show)

makeLenses ''Stick

newStickF :: [Int] -> Int -> (Int -> Int -> a) -> Stick a
newStickF offs w f = Stick offs
                   $ A.array ((0, 0), (length offs - 1, w - 1))
                   [ ((strand, fret), f strand fret)
                   | strand <- [0 .. length offs - 1]
                   , fret   <- [0 .. w - 1]
                   ]

newStick :: [Int] -> Int -> a -> Stick a
newStick offs w a = Stick offs
                . A.listArray ((0, 0), (length offs - 1, w - 1))
                $ repeat a

atFret :: Int -> Int -> a -> Stick a -> Stick a
atFret strand fret a s =
  s & stickData %~ (A.// [((strand, fret), a)])

atTone :: Int -> Int -> a -> Stick a -> Stick a
atTone strand tone a s =
  atFret strand (tone - (head . drop strand $ _stickOffsets s)) a s

imap :: (((Int, Int), Int, a) -> b) -> Stick a -> Stick b
imap f a =
  a & stickData %~ \aa ->
        A.array (A.bounds aa)
          . fmap (f' $ _stickOffsets a)
          $ A.assocs aa
  where
    f' offs ((fret, s), a) = ((fret, s), f ((fret, s), (offs !! s) + fret, a))

