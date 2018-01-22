{-# LANGUAGE ViewPatterns #-}

module Guitar where

import Data.Bool (bool)
import qualified Data.Array as A
import           Data.Foldable (for_)
import           Interval
import           Stick

main :: IO ()
main = main'
     $ imap (\(_, s, n) ->
        case n of
          E  -> "o"
          G' -> "*"
          B  -> "."
          _  -> ""
          ) guitar

main' :: Stick String -> IO ()
main' g = do
  putStrLn ""
  showGuitar 2 g
  putStrLn ""

guitar :: Stick Note
guitar =
  newStickF offs 24 $ \s f -> iPlus E
                            . toInterval
                            . fromIntegral
                            $ (offs !! s) + f
  where
    offs = [0, 5, 10, 15, 19, 24]


showGuitar :: Int -> Stick String -> IO ()
showGuitar (max 2 -> w) s = do
    let padded = _stickData $ fmap showPad s
        ((ls, lf), (hs, hf)) = A.bounds $ _stickData s
    putStr " "
    for_ [lf .. hf] $ \f -> do
      putStr . showPad $ show f
      putStr " | "
    putStrLn ""
    putStrLn $ replicate ((w + 3) * (hf - lf + 1)) '-'
    for_ [hs, hs - 1 .. ls] $ \s -> do
      putStr " "
      for_ [lf .. hf] $ \f -> do
        let n = padded A.! (s, f)
        putStr n
        putStr " | "
      putStrLn ""
  where
    showPad = take w
            . (++ replicate w ' ')
            . take w

