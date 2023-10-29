{-# LANGUAGE RankNTypes #-}

module OpCode
  ( runOpCodeWith,
    runSTOC,
    PrimOC (..),
    UBOC,
    STOC,
    readInput,
  )
where

import Control.Monad.ST.Strict (ST, runST)
import Data.List (uncons)
import Data.List.Split (splitOn)
import Data.STRef.Strict
import Data.Vector.Unboxed hiding (elem, head, map, read, tail, uncons)
import Data.Vector.Unboxed.Mutable hiding (read, tail)
import qualified Data.Vector.Unboxed.Mutable as UM

data PrimOC i v j = PrimOC
  { _position :: i,
    _vector :: v,
    _input :: j,
    _output :: j
  }
  deriving (Show, Eq, Ord)

type UBOC = PrimOC Int (Vector Int) [Int]

type STOC s = PrimOC (STRef s Int) (MVector s Int) (STRef s [Int])

readInput :: String -> UBOC
readInput s = PrimOC 0 (fromList (map (read @Int) . splitOn "," $ s)) [] []

runOpCodeWith :: (forall s. STOC s -> ST s Bool) -> UBOC -> (Bool, UBOC)
runOpCodeWith f (PrimOC i v input output) = g
  where
    g = runST $ do
      a <- newSTRef i
      b <- thaw v
      c <- newSTRef input
      d <- newSTRef output
      halt <- f (PrimOC a b c d)
      x <- readSTRef a
      y <- freeze b
      z <- readSTRef c
      w <- readSTRef d
      return (halt, PrimOC x y z w)

runSTOC :: STOC s -> ST s Bool
runSTOC oc = do
  i <- readSTRef $ _position oc
  let v = _vector oc
  y <- UM.read v i
  let x = y `mod` 100
      p1 = f $ (y `div` 100) `mod` 10
      p2 = f $ (y `div` 1000) `mod` 10
      p3 = f $ (y `div` 10000) `mod` 10
      f n = if n == 0 then UM.read v else pure
  case x of
    99 -> pure True
    3 -> do
      c <- UM.read v (i + 1)
      input <- readSTRef $ _input oc
      case uncons input of
        Nothing -> pure False
        Just (h, t) -> do
          UM.write v c h
          writeSTRef (_input oc) t
          modifySTRef (_position oc) (+ 2)
          runSTOC oc
    4 -> do
      c <- UM.read v (i + 1) >>= p1
      modifySTRef (_output oc) (c :)
      modifySTRef (_position oc) (+ 2)
      runSTOC oc
    y | y `elem` [5, 6] -> do
      a <- UM.read v (i + 1) >>= p1
      let c = case y of
            5 -> a /= 0
            6 -> a == 0
      if c
        then UM.read v (i + 2) >>= p2 >>= writeSTRef (_position oc)
        else modifySTRef (_position oc) (+ 3)
      runSTOC oc
    y | y `elem` [1, 2, 7, 8] -> do
      a <- UM.read v (i + 1) >>= p1
      b <- UM.read v (i + 2) >>= p2
      c <- UM.read v (i + 3)
      let op = case y of
            1 -> (+)
            2 -> (*)
            7 -> \e f -> if e < f then 1 else 0
            8 -> \e f -> if e == f then 1 else 0
      UM.write v c (op a b)
      modifySTRef (_position oc) (+ 4)
      runSTOC oc
