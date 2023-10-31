{-# LANGUAGE LambdaCase #-}
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

import Control.Monad (void)
import Control.Monad.ST.Strict (ST, runST)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.List (uncons)
import Data.List.Split (splitOn)
-- import Data.Map (Map)
-- import qualified Data.Map as IM
import Data.STRef.Strict

-- import Data.Vector.Unboxed hiding (elem, head, map, read, tail, uncons)
-- import Data.Vector.Unboxed.Mutable hiding (read, tail)
-- import qualified Data.Vector.Unboxed.Mutable as UM
-- import Debug.Trace (traceM)

-- type IntMap = Map Integer

data PrimOC i v j b = PrimOC
  { _position :: i,
    _base :: i,
    _vector :: v,
    _input :: j,
    _output :: j,
    _halt :: b
  }
  deriving (Show, Eq, Ord)

type UBOC = PrimOC Int (IntMap Integer) [Integer] Bool
-- type UBOC = PrimOC Integer (IntMap Integer) [Integer] Bool

type STOC s = STRef s UBOC

readDefaultWith :: Integer -> STOC s -> Int -> ST s Integer
readDefaultWith def oc i = do
  m <- _vector <$> readSTRef oc
  case m IM.!? i of
    Just x -> pure x
    Nothing -> do
      modifySTRef' oc (\x -> x {_vector = IM.insert i def $ _vector x})
      pure def

readDefault = readDefaultWith 0

-- readDefault oc i = let v = _vector oc in UM.readMaybe v i >>= \case
--   Just x -> return x
--   Nothing -> do
--     let l = UM.length v
--         d = i - l + 2
--     v' <- UM.grow v d
--     UM.read v' i

readInput :: String -> UBOC
readInput s = PrimOC 0 0 (IM.fromList (zip [0 ..] . map (read @Integer) . splitOn "," $ s)) [] [] False

runOpCodeWith :: (forall s. STOC s -> ST s ()) -> UBOC -> UBOC
runOpCodeWith f oc = g
  where
    g = runST $ do
      oc' <- newSTRef oc
      f oc'
      readSTRef oc'

runSTOC :: STOC s -> ST s ()
runSTOC oc = do
  oc' <- readSTRef oc
  let i = _position oc'
      base = _base oc'
  y <- readDefault oc i
  -- traceM (show (i, base))
  let x = y `mod` 100
      p1 = f $ (y `div` 100) `mod` 10
      p1' = g $ (y `div` 100) `mod` 10
      p2 = f $ (y `div` 1000) `mod` 10
      p2' = f $ (y `div` 1000) `mod` 10
      p3 = f $ (y `div` 10000) `mod` 10
      p3' = g $ (y `div` 10000) `mod` 10
      f = \case
        0 -> readDefault oc . fromIntegral
        1 -> pure . fromIntegral
        2 -> readDefault oc . (+ base) . fromIntegral
      g = \case
        0 -> fromIntegral
        2 -> (+ base) . fromIntegral
  case x of
    99 -> void $ modifySTRef' oc (\x -> x {_halt = True})
    3 -> do
      c <- p1' <$> readDefault oc (i + 1)
      case uncons (_input oc') of
        Nothing -> pure ()
        Just (h, t) -> do
          modifySTRef oc (\x -> x {_vector = IM.insert (fromIntegral c) h $ _vector x})
          modifySTRef oc (\x -> x {_input = t})
          modifySTRef oc (\x -> x {_position = 2 + _position x})
          runSTOC oc
    4 -> do
      c <- readDefault oc (i + 1) >>= p1
      modifySTRef oc (\x -> x {_output = c : _output x})
      modifySTRef oc (\x -> x {_position = 2 + _position x})
      runSTOC oc
    9 -> do
      c <- readDefault oc (i + 1) >>= p1
      modifySTRef oc (\x -> x {_base = fromIntegral c + _base x})
      modifySTRef oc (\x -> x {_position = 2 + _position x})
      runSTOC oc
    y | y `elem` [5, 6] -> do
      a <- readDefault oc (i + 1) >>= p1
      let c = case y of
            5 -> a /= 0
            6 -> a == 0
      if c
        then readDefault oc (i + 2) >>= p2 >>= \w -> modifySTRef' oc (\x -> x {_position = fromIntegral w})
        else modifySTRef oc (\x -> x {_position = 3 + _position x})
      runSTOC oc
    y | y `elem` [1, 2, 7, 8] -> do
      a <- readDefault oc (i + 1) >>= p1
      b <- readDefault oc (i + 2) >>= p2
      c <- p3' <$> readDefault oc (i + 3)
      let op = case y of
            1 -> (+)
            2 -> (*)
            7 -> \e f -> if e < f then 1 else 0
            8 -> \e f -> if e == f then 1 else 0
      modifySTRef oc (\x -> x {_vector = IM.insert c (op a b) (_vector x)})
      modifySTRef oc (\x -> x {_position = 4 + _position x})
      runSTOC oc
