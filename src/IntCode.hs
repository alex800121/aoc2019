{-# LANGUAGE LambdaCase #-}

module IntCode where

import Control.Monad.ST.Strict (ST, runST)
import Data.Bifunctor (Bifunctor (..))
import Data.List.Split (splitOn)
import Data.STRef
import Data.Sequence (Seq (..))
import Data.Sequence qualified as S
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as MV
import Debug.Trace (traceM, traceShow)

data PureIntCode = PureIntCode
  { _purePos :: Int,
    _pureInput :: Seq Int,
    _pureOutput :: Seq Int,
    _pureHalt :: Bool,
    _pureBase :: Int,
    _pureCode :: Vector Int
  }
  deriving (Show, Eq, Ord)

data IntCode s = IntCode
  { _pos :: Int,
    _input :: Seq Int,
    _output :: Seq Int,
    _halt :: Bool,
    _base :: Int,
    _code :: STRef s (STVector s Int)
  }

fromPure :: PureIntCode -> ST s (IntCode s)
fromPure (PureIntCode p i o h b c) = IntCode p i o h b <$> (V.thaw c >>= newSTRef)

toPure :: IntCode s -> ST s PureIntCode
toPure (IntCode p i o h b c) = PureIntCode p i o h b <$> (V.freeze =<< readSTRef c)

readPure :: String -> PureIntCode
readPure s = PureIntCode 0 S.empty S.empty False 0 v
  where
    ss = map (read @Int) $ splitOn "," s
    -- m = 10000
    m = 0
    v = V.fromList (ss <> replicate m 0)

-- runPureIntCodeLoop :: Set (Int, Int, Seq Int) -> PureIntCode -> PureIntCode
runPureIntCodeLoop cache ic@(PureIntCode pos i o h base code)
  | traceShow (_pureOutput ic) False = undefined
  | ics `Set.member` cache = ic
  | otherwise = case f op of
    (1, r1, r2, r3) ->
      let (ca, a) = g code (pos + 1) r1
          (cb, b) = g code (pos + 2) r2
          (cc, c) = g' code (pos + 3) r3
          cw = writeExtend code c (a + b)
       in runPureIntCodeLoop cache' (PureIntCode (pos + 4) i o h base cw)
    (2, r1, r2, r3) -> do
      let (ca, a) = g code (pos + 1) r1
          (cb, b) = g code (pos + 2) r2
          (cc, c) = g' code (pos + 3) r3
          cw = writeExtend code c (a * b)
       in runPureIntCodeLoop cache' (PureIntCode (pos + 4) i o h base cw)
    (3, r1, _, _)
      | x :<| i' <- i ->
          let (cp, p) = g' code (pos + 1) r1
              cw = writeExtend code p x
           in runPureIntCodeLoop cache' (PureIntCode (pos + 2) i' o h base cw)
    (3, _, _, _) -> ic
    (4, r1, _, _) ->
      let (co, o0) = g code (pos + 1) r1
       in runPureIntCodeLoop cache' (PureIntCode (pos + 2) i (o :|> o0) h base co)
    (5, r1, r2, _) ->
      let (cx, x) = g code (pos + 1) r1
          (cp, p) = g code (pos + 2) r2
       in case x of
            0 -> runPureIntCodeLoop cache' (PureIntCode (pos + 3) i o h base cx)
            _ -> runPureIntCodeLoop cache' (PureIntCode p i o h base cp)
    (6, r1, r2, _) ->
      let (cx, x) = g code (pos + 1) r1
          (cp, p) = g code (pos + 2) r2
       in case x of
            0 -> runPureIntCodeLoop cache' (PureIntCode p i o h base cp)
            _ -> runPureIntCodeLoop cache' (PureIntCode (pos + 3) i o h base cx)
    (7, r1, r2, r3) ->
      let (ca, a) = g code (pos + 1) r1
          (cb, b) = g code (pos + 2) r2
          (cp, p) = g' code (pos + 3) r3
          cw = writeExtend code p (if a < b then 1 else 0)
       in runPureIntCodeLoop cache' (PureIntCode (pos + 4) i o h base cw)
    (8, r1, r2, r3) -> do
      let (ca, a) = g code (pos + 1) r1
          (cb, b) = g code (pos + 2) r2
          (cp, p) = g' code (pos + 3) r3
          cw = writeExtend code p (if a == b then 1 else 0)
       in runPureIntCodeLoop cache' (PureIntCode (pos + 4) i o h base cw)
    (9, r1, _, _) ->
      let (cb, b) = g code (pos + 1) r1
       in runPureIntCodeLoop cache' (PureIntCode (pos + 2) i o h (base + b) cb)
    (99, _, _, _) -> PureIntCode (pos + 1) i o True base code
  where
    ics = (_purePos ic, _pureBase ic, _pureInput ic, _pureCode ic)
    cache' = Set.insert ics cache
    op = code V.! pos
    f i = (i `mod` 100, (i `div` 100) `mod` 10, (i `div` 1000) `mod` 10, (i `div` 10000) `mod` 10)
    g c p 0 = uncurry readExtend $ readExtend c p
    g c p 1 = readExtend c p
    g c p 2 = uncurry readExtend $ second (base +) $ readExtend c p
    g' c p 0 = readExtend c p
    g' c p 2 = second (base +) $ readExtend c p
    readExtend c i = do
      if i < l
        then (code, code V.! i)
        else (code', 0)
      where
        l = V.length code
        code' = code V.++ V.replicate (i - l + 1) 0
    writeExtend c i x =
      if i < l
        then code V.// [(i, x)]
        else code' V.// [(i, x)]
      where
        l = V.length code
        code' = code V.++ V.replicate (i - l + 1) 0

runIntCodeLoop :: Set (Int, Int, Seq Int, Vector Int) -> IntCode s -> ST s (IntCode s)
runIntCodeLoop cache ic@(IntCode pos i o h base code) = do
  code' <- readSTRef code
  old <- (pos, base, i, ) <$> V.freeze code'
  if old `Set.member` cache
    then pure ic
    else do
      let cache' = Set.insert old cache
      op <- (`MV.read` pos) =<< readSTRef code
      case f op of
        (1, r1, r2, r3) -> do
          a <- g code (pos + 1) r1
          b <- g code (pos + 2) r2
          c <- g' code (pos + 3) r3
          writeExtend code c (a + b)
          runIntCodeLoop cache' (IntCode (pos + 4) i o h base code)
        (2, r1, r2, r3) -> do
          a <- g code (pos + 1) r1
          b <- g code (pos + 2) r2
          c <- g' code (pos + 3) r3
          writeExtend code c (a * b)
          runIntCodeLoop cache' (IntCode (pos + 4) i o h base code)
        (3, r1, _, _)
          | x :<| i' <- i ->
              g' code (pos + 1) r1
                >>= \p -> writeExtend code p x >> runIntCodeLoop cache' (IntCode (pos + 2) i' o h base code)
        (3, _, _, _) -> pure ic
        (4, r1, _, _) -> g code (pos + 1) r1 >>= \o0 -> runIntCodeLoop cache' (IntCode (pos + 2) i (o :|> o0) h base code)
        (5, r1, r2, _) ->
          g code (pos + 1) r1 >>= \case
            0 -> runIntCodeLoop cache' (IntCode (pos + 3) i o h base code)
            _ -> g code (pos + 2) r2 >>= \p -> runIntCodeLoop cache' (IntCode p i o h base code)
        (6, r1, r2, _) ->
          g code (pos + 1) r1 >>= \case
            0 -> g code (pos + 2) r2 >>= \p -> runIntCodeLoop cache' (IntCode p i o h base code)
            _ -> runIntCodeLoop cache' (IntCode (pos + 3) i o h base code)
        (7, r1, r2, r3) -> do
          a <- g code (pos + 1) r1
          b <- g code (pos + 2) r2
          g' code (pos + 3) r3 >>= \p -> writeExtend code p (if a < b then 1 else 0)
          runIntCodeLoop cache' (IntCode (pos + 4) i o h base code)
        (8, r1, r2, r3) -> do
          a <- g code (pos + 1) r1
          b <- g code (pos + 2) r2
          g' code (pos + 3) r3 >>= \p -> writeExtend code p (if a == b then 1 else 0)
          runIntCodeLoop cache' (IntCode (pos + 4) i o h base code)
        (9, r1, _, _) -> g code (pos + 1) r1 >>= \b -> runIntCodeLoop cache' (IntCode (pos + 2) i o h (base + b) code)
        (99, _, _, _) -> pure (IntCode (pos + 1) i o True base code)
  where
    f i = (i `mod` 100, (i `div` 100) `mod` 10, (i `div` 1000) `mod` 10, (i `div` 10000) `mod` 10)
    g c p 0 = readExtend c p >>= readExtend c
    g c p 1 = readExtend c p
    g c p 2 = readExtend c p >>= readExtend c . (base +)
    g' c p 0 = readExtend c p
    g' c p 2 = (base +) <$> readExtend c p
    readExtend c i = do
      code <- readSTRef c
      let l = MV.length code
      if i < l
        then MV.read code i
        else MV.grow code (i - l + 1) >>= writeSTRef c >> pure 0
    writeExtend c i x = do
      code <- readSTRef c
      let l = MV.length code
      if i < l
        then MV.write code i x
        else MV.grow code (i - l + 1) >>= \code' -> MV.write code' i x >> writeSTRef c code'

runIntCode :: IntCode s -> ST s (IntCode s)
runIntCode ic@(IntCode pos i o h base code) = do
  op <- (`MV.read` pos) =<< readSTRef code
  case f op of
    (1, r1, r2, r3) -> do
      a <- g code (pos + 1) r1
      b <- g code (pos + 2) r2
      c <- g' code (pos + 3) r3
      writeExtend code c (a + b)
      runIntCode (IntCode (pos + 4) i o h base code)
    (2, r1, r2, r3) -> do
      a <- g code (pos + 1) r1
      b <- g code (pos + 2) r2
      c <- g' code (pos + 3) r3
      writeExtend code c (a * b)
      runIntCode (IntCode (pos + 4) i o h base code)
    (3, r1, _, _)
      | x :<| i' <- i ->
          g' code (pos + 1) r1
            >>= \p -> writeExtend code p x >> runIntCode (IntCode (pos + 2) i' o h base code)
    (3, _, _, _) -> pure ic
    (4, r1, _, _) -> g code (pos + 1) r1 >>= \o0 -> runIntCode (IntCode (pos + 2) i (o :|> o0) h base code)
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
      g' code (pos + 3) r3 >>= \p -> writeExtend code p (if a < b then 1 else 0)
      runIntCode (IntCode (pos + 4) i o h base code)
    (8, r1, r2, r3) -> do
      a <- g code (pos + 1) r1
      b <- g code (pos + 2) r2
      g' code (pos + 3) r3 >>= \p -> writeExtend code p (if a == b then 1 else 0)
      runIntCode (IntCode (pos + 4) i o h base code)
    (9, r1, _, _) -> g code (pos + 1) r1 >>= \b -> runIntCode (IntCode (pos + 2) i o h (base + b) code)
    (99, _, _, _) -> pure (IntCode (pos + 1) i o True base code)
  where
    f i = (i `mod` 100, (i `div` 100) `mod` 10, (i `div` 1000) `mod` 10, (i `div` 10000) `mod` 10)
    g c p 0 = readExtend c p >>= readExtend c
    g c p 1 = readExtend c p
    g c p 2 = readExtend c p >>= readExtend c . (base +)
    g' c p 0 = readExtend c p
    g' c p 2 = (base +) <$> readExtend c p
    readExtend c i = do
      code <- readSTRef c
      let l = MV.length code
      if i < l
        then MV.read code i
        else MV.grow code (i - l + 1) >>= writeSTRef c >> pure 0
    writeExtend c i x = do
      code <- readSTRef c
      let l = MV.length code
      if i < l
        then MV.write code i x
        else MV.grow code (i - l + 1) >>= \code' -> MV.write code' i x >> writeSTRef c code'
