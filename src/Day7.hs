module Day7 where

import Control.Monad (foldM, join, replicateM, (<=<))
import Control.Monad.ST.Strict (runST)
import Data.List (permutations)
import IntCode
import Paths_AOC2019
import Queue (Queue (..))
import Queue qualified as Q

inputA = permutations [0 .. 4]

inputB = permutations [5 .. 9]

day7a v i = runST $ do
  xs <- mapM (fromPure . (\a b -> a {_pureInput = Q.singleton b}) v) i
  foldM (\i x -> _output <$> runIntCode (x {_input = _input x <> i})) (Q.singleton 0) xs

day7b v i = runST $ do
  xs <- mapM (fromPure . (\a b -> a {_pureInput = Q.singleton b}) v) i
  f (Q.fromList xs) (Q.singleton 0)
  where
    f Empty _ = pure Nothing
    f (Full x xs) i
      | _halt x = pure (Just i)
      | otherwise = runIntCode (x {_input = _input x <> i}) >>= \x' -> f (Q.enqueue (x' {_output = Q.empty}) xs) (_output x')

day7 :: IO ()
day7 = do
  v <- readPure <$> (getDataDir >>= readFile . (++ "/input/input7.txt"))
  putStrLn
    . ("day7a: " ++)
    . show
    . maximum
    $ map (fmap fst . Q.dequeue . day7a v) inputA
  putStrLn
    . ("day7b: " ++)
    . show
    . maximum
    $ map (fmap fst . Q.dequeue <=< day7b v) inputB
