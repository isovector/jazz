{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module ChordServer where

import Data.Bool
import Data.List hiding (transpose)
import Control.Arrow (second)
import Main hiding (main)
import Control.Monad.IO.Class
import Network.Wai.Handler.Warp
import Servant
import Servant.Server
import Servant.JS
import Data.Text (Text)
import Interval
import System.Random (randomRIO)

type API = "chord" :> Get '[JSON] String

type MyServer = API
           :<|> "api.js" :> Get '[PlainText] Text
           :<|> Raw


main :: IO ()
main = run 8080
     . serve (Proxy @MyServer)
     $ getChord
  :<|> (pure $ jsForAPI (Proxy @API) vanillaJS)
  :<|> serveDirectoryFileServer "."

getChord :: Handler String
getChord = do
  key <- liftIO $ oneOf [C,F,G]
  inv <- liftIO $ oneOf [0,1,2]
  pure $ dumpChord $ invert inv $ makeChord key

oneOf :: [a] -> IO a
oneOf ls = do
  i <- randomRIO (0, length ls - 1)
  pure $ ls !! i


makeChord :: Note -> [(Note, Int)]
makeChord n = [inOctave Uni, inOctave Maj3, inOctave Perf5]
  where
    inOctave i =
      let n' = iPlus n i
       in if n <= n'
             then (n', 4)
             else (n', 5)

invert :: Int -> [(Note, Int)] -> [(Note, Int)]
invert 0 ns = ns
invert i ((n, o) : ns) = invert (i - 1) $ ns ++ [(n, o + 1)]

dumpChord :: [(Note, Int)] -> String
dumpChord nos = ('(' :)
              . (++ ")")
              . fmap (\x -> bool x '@' $ x == 'b')
              . intercalate "."
              . fmap (\(z, o) -> nameOfNote (head ns) z ++ "/" ++ show o)
              . flip zip is
              $ intsFromNotes ns
  where
    (ns, is) = unzip nos



