module Day7 where

import Control.Monad (foldM, join, replicateM, (<=<))
import Control.Monad.ST.Strict (runST)
import Data.List (permutations)
import IntCode
import Paths_AOC2019
-- import Queue (Queue (..))
-- import Queue qualified as Q
import Data.Sequence (Seq (..))
import Data.Sequence qualified as S
import Control.Monad.Trans.State(execStateT)

inputA = permutations [0 .. 4]

inputB = permutations [5 .. 9]

day7a v i = runST $ do
  xs <- mapM (fromPure . (\a b -> a {_pureInput = S.singleton b}) v) i
  foldM (\i x -> _output <$> runIntCode (x {_input = _input x <> i})) (S.singleton 0) xs

day7b v i = runST $ do
  xs <- mapM (fromPure . (\a b -> a {_pureInput = S.singleton b}) v) i
  f (S.fromList xs) (S.singleton 0)
  where
    f Empty _ = pure Nothing
    f (x :<| xs) i
      | _halt x = pure (Just i)
      | otherwise = runIntCode (x {_input = _input x <> i}) >>= \x' -> f (xs :|> (x' {_output = S.empty})) (_output x')

day7 :: IO (String, String)
day7 = do
  v <- readPure <$> (getDataDir >>= readFile . (++ "/input/input7.txt"))
  let
   !finalAnsa
    = show
    . maximum
    $ map ((\(x :<| _) -> x) . day7a v) inputA
  let
   !finalAnsb
    = show
    . maximum
    $ map (fmap (\(x :<| _) -> x) . day7b v) inputB
  pure (finalAnsa, finalAnsb)
