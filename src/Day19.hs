{-# LANGUAGE MultiWayIf #-}

module Day19 where

import Control.Monad.ST.Strict (ST, runST)
import Data.Functor ((<&>))
import Data.STRef (newSTRef, readSTRef)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as S
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as MV
import Debug.Trace
import IntCode
import Paths_AOC2019

buildBeam ic n m = runST $ f 0 0 0 =<< MV.replicate n (0, 0)
  where
    f :: Int -> Int -> Int -> STVector s (Int, Int) -> ST s (Vector (Int, Int))
    f i0 x y acc
      | y >= n = V.freeze acc
      | x >= m = f i0 i0 (y + 1) acc
      | otherwise = do
          ic' <- runIntCode =<< fromPure (ic {_pureInput = S.fromList [x, y]})
          let b :<| _ = _output ic'
          if b == 1 then g x x (x + 1) y acc else f i0 (x + 1) y acc
    g :: Int -> Int -> Int -> Int -> STVector s (Int, Int) -> ST s (Vector (Int, Int))
    g i0 j0 j1 y acc = do
      ic' <- runIntCode =<< fromPure (ic {_pureInput = S.fromList [j1, y]})
      let b :<| _ = _output ic'
      if
        | b == 0 && j1 == j0 + 1 -> MV.write acc y (i0, min m j1) >> f i0 i0 (y + 1) acc
        | b == 0 -> g i0 j0 ((j1 + j0) `div` 2) y acc
        | b == 1 -> g i0 j1 (j1 + (j1 - j0) * 2) y acc
        | otherwise -> error (show b)

day19b ic = go 0 99
  where
    go x y
      | 0 :<| _ <- l = go (x + 1) y
      | 1 :<| _ <- l, 0 :<| _ <- r = go x (y + 1)
      | 1 :<| _ <- l, 1 :<| _ <- r = x * 10000 + y'
      where
        l = runST (fromPure (ic {_pureInput = S.fromList [x, y]}) >>= runIntCode <&> _output)
        r = runST (fromPure (ic {_pureInput = S.fromList [x', y']}) >>= runIntCode <&> _output)
        x' = x + 99
        y' = y - 99

day19 :: IO ()
day19 = do
  v <- readPure <$> (getDataDir >>= readFile . (++ "/input/input19.txt"))
  putStrLn
    . ("day19a: " ++)
    . show
    . V.foldl' (\acc (x, y) -> acc + y - x) 0
    $ buildBeam v 50 50
  putStrLn
    . ("day19b: " ++)
    . show
    $ day19b v
