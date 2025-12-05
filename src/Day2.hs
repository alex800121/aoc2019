module Day2 where

import Control.Monad.ST.Strict (runST)
import Control.Parallel.Strategies
import Data.Foldable (find)
import Data.Maybe (catMaybes)
import Data.Vector.Unboxed.Mutable qualified as MV
import IntCode (IntCode (..), fromPure, readPure, runIntCode, toPure)
import Paths_AOC2019
import Data.STRef (readSTRef)
import Control.Monad ((>=>))

day2a v noun verb =
  runST
    ( fromPure v
        >>= (\ic -> readSTRef (_code ic) >>= \c -> MV.write c 1 noun >> MV.write c 2 verb >> pure ic)
        >>= runIntCode
        >>= (readSTRef >=> (`MV.read` 0)) . _code
    )

day2b v =
  parMap
    rpar
    (\(x, y) -> if day2a v x y == 19690720 then Just (100 * x + y) else Nothing)
    [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99]]

day2 :: IO (String, String)
day2 = do
  v <- readPure <$> (getDataDir >>= readFile . (++ "/input/input2.txt"))
  let
   !finalAnsa
    = show
    $ day2a v 12 2
  let
   !finalAnsb
    = show
    . catMaybes
    $ day2b v
  pure (finalAnsa, finalAnsb)
