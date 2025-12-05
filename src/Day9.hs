module Day9 where

import Control.Monad.ST.Strict (runST)
import Data.Functor ((<&>))
import Debug.Trace (traceM)
import IntCode
import Paths_AOC2019
import Data.Sequence qualified as S

day9a v i = runST $ do
  x <- fromPure v
  runIntCode (x {_input = S.singleton i}) <&> _output

day9 :: IO (String, String)
day9 = do
  input <- readPure <$> (getDataDir >>= readFile . (++ "/input/input9.txt"))
  let
   !finalAnsa
    = show
    $ day9a input 1
  let
   !finalAnsb
    = show
    $ day9a input 2
  pure (finalAnsa, finalAnsb)
