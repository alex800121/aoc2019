module Day9 where

import Control.Monad.ST.Strict (runST)
import Data.Functor ((<&>))
import Debug.Trace (traceM)
import IntCode
import Paths_AOC2019
import Queue qualified as Q

day9a v i = runST $ do
  x <- fromPure v
  runIntCode (x {_input = Q.singleton i}) <&> _output

day9 :: IO ()
day9 = do
  input <- readPure <$> (getDataDir >>= readFile . (++ "/input/input9.txt"))
  putStrLn
    . ("day9a: " ++)
    . show
    $ day9a input 1
  putStrLn
    . ("day9b: " ++)
    . show
    $ day9a input 2
