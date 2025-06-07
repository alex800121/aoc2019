{-# LANGUAGE LambdaCase #-}

module IntCode where

import Control.Monad.ST.Strict (ST, runST)
import Data.List.Split (splitOn)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as MV
import Debug.Trace (traceM)
import Queue (Queue (..))
import Queue qualified as Q

data PureIntCode = PureIntCode
  { _purePos :: Int,
    _pureInput :: Queue Int,
    _pureOutput :: Queue Int,
    _pureHalt :: Bool,
    _pureBase :: Int,
    _pureCode :: Vector Int
  }
  deriving (Show, Eq)

data IntCode s = IntCode
  { _pos :: Int,
    _input :: Queue Int,
    _output :: Queue Int,
    _halt :: Bool,
    _base :: Int,
    _code :: STVector s Int
  }

fromPure :: PureIntCode -> ST s (IntCode s)
fromPure (PureIntCode p i o h b c) = IntCode p i o h b <$> V.thaw c

toPure :: IntCode s -> ST s PureIntCode
toPure (IntCode p i o h b c) = PureIntCode p i o h b <$> V.freeze c

readPure :: String -> PureIntCode
readPure s = PureIntCode 0 Q.empty Q.empty False 0 v
  where
    ss = map (read @Int) $ splitOn "," s
    m = 10000
    -- m = maximum ss
    v = V.fromList (ss <> replicate m 0)

runIntCode :: IntCode s -> ST s (IntCode s)
runIntCode ic@(IntCode pos i o True base code) = pure ic
runIntCode ic@(IntCode pos i o h base code) = do
  op <- MV.read code pos
  case f op of
    (1, r1, r2, r3) -> do
      a <- g code (pos + 1) r1
      b <- g code (pos + 2) r2
      c <- g' code (pos + 3) r3
      MV.write code c (a + b)
      runIntCode (IntCode (pos + 4) i o h base code)
    (2, r1, r2, r3) -> do
      a <- g code (pos + 1) r1
      b <- g code (pos + 2) r2
      c <- g' code (pos + 3) r3
      MV.write code c (a * b)
      runIntCode (IntCode (pos + 4) i o h base code)
    (3, r1, _, _)
      | Just (x, i') <- Q.dequeue i ->
          g' code (pos + 1) r1
            >>= \p -> MV.write code p x >> runIntCode (IntCode (pos + 2) i' o h base code)
    (3, _, _, _) -> pure ic
    (4, r1, _, _) -> g code (pos + 1) r1 >>= \o0 -> runIntCode (IntCode (pos + 2) i (Q.enqueue o0 o) h base code)
    (5, r1, r2, _) ->
      g code (pos + 1) r1 >>= \case
        0 -> runIntCode (IntCode (pos + 3) i o h base code)
        _ -> g code (pos + 2) r2 >>= \p -> runIntCode (IntCode p i o h base code)
    (6, r1, r2, _) ->
      g code (pos + 1) r1 >>= \case
        0 -> g code (pos + 2) r2 >>= \p -> runIntCode (IntCode p i o h base code)
        _ -> runIntCode (IntCode (pos + 3) i o h base code)
    (7, r1, r2, r3) -> do
      a <- g code (pos + 1) r1
      b <- g code (pos + 2) r2
      g' code (pos + 3) r3 >>= \p -> MV.write code p (if a < b then 1 else 0)
      runIntCode (IntCode (pos + 4) i o h base code)
    (8, r1, r2, r3) -> do
      a <- g code (pos + 1) r1
      b <- g code (pos + 2) r2
      g' code (pos + 3) r3 >>= \p -> MV.write code p (if a == b then 1 else 0)
      runIntCode (IntCode (pos + 4) i o h base code)
    (9, r1, _, _) -> g code (pos + 1) r1 >>= \b -> runIntCode (IntCode (pos + 2) i o h (base + b) code)
    (99, _, _, _) -> pure (IntCode pos i o True base code)
  where
    f i = (i `mod` 100, (i `div` 100) `mod` 10, (i `div` 1000) `mod` 10, (i `div` 10000) `mod` 10)
    g c p 0 = MV.read c p >>= MV.read c
    g c p 1 = MV.read c p
    g c p 2 = MV.read c p >>= MV.read c . (base +)
    g' c p 0 = MV.read c p
    g' c p 2 = (base +) <$> MV.read c p
