{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module OpCode
  ( runOpCodeWith,
    runSTOC,
    PrimOC (..),
    UBOC,
    STOC,
    readInput,
    stToUB,
    ubToST,
    mkUBOC,
    inputOpCode,
    runIOOC
  )
where

import Control.Monad (void)
-- import Data.Map (Map)
-- import qualified Data.Map as IM

import Control.Monad.ST (stToIO)
import Control.Monad.ST.Strict (RealWorld, ST, runST)
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.IORef (IORef, newIORef, readIORef)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (uncons)
import Data.List.Split (splitOn)
import Data.STRef.Strict

-- import Data.Vector.Unboxed hiding (elem, head, map, read, tail, uncons)
-- import Data.Vector.Unboxed.Mutable hiding (read, tail)
-- import qualified Data.Vector.Unboxed.Mutable as UM
-- import Debug.Trace (traceM)

-- type IntMap = Map Integer

data PrimOC i v j b f = PrimOC
  { _position :: f i,
    _base :: f i,
    _vector :: f v,
    _input :: f j,
    _output :: f j,
    _halt :: f b
  }
  deriving (Show, Eq, Ord)

type UBOC = PrimOC Int (IntMap Integer) (DList Integer) Bool Identity
-- type UBOC = PrimOC Integer (IntMap Integer) [Integer] Bool
type STOC s = PrimOC Int (IntMap Integer) (DList Integer) Bool (STRef s)
-- type IOOC = PrimOC Int (IntMap Integer) (DList Integer) Bool IORef

readDefaultWith :: Integer -> STOC s -> Int -> ST s Integer
readDefaultWith def oc i = do
  m <- readSTRef $ _vector oc
  case m IM.!? i of
    Just x -> pure x
    Nothing -> do
      modifySTRef' (_vector oc) (IM.insert i def)
      pure def

readDefault = readDefaultWith 0

-- readDefault oc i = let v = _vector oc in UM.readMaybe v i >>= \case
--   Just x -> return x
--   Nothing -> do
--     let l = UM.length v
--         d = i - l + 2
--     v' <- UM.grow v d
--     UM.read v' i

mkUBOC :: Int -> Int -> IntMap Integer -> DList Integer -> DList Integer -> Bool -> UBOC
mkUBOC a b c d e f =
  PrimOC
    (Identity a)
    (Identity b)
    (Identity c)
    (Identity d)
    (Identity e)
    (Identity f)

readInput :: String -> UBOC
readInput s = mkUBOC 0 0 (IM.fromList (zip [0 ..] . map (read @Integer) . splitOn "," $ s)) DL.empty DL.empty False

ubToST :: UBOC -> ST s (STOC s)
ubToST (PrimOC a b c d e f) = do
  a' <- newSTRef $ runIdentity a
  b' <- newSTRef $ runIdentity b
  c' <- newSTRef $ runIdentity c
  d' <- newSTRef $ runIdentity d
  e' <- newSTRef $ runIdentity e
  f' <- newSTRef $ runIdentity f
  return $ PrimOC a' b' c' d' e' f'

-- ubToIO :: UBOC -> IO IOOC
-- ubToIO (PrimOC a b c d e f) = do
--   a' <- newIORef $ runIdentity a
--   b' <- newIORef $ runIdentity b
--   c' <- newIORef $ runIdentity c
--   d' <- newIORef $ runIdentity d
--   e' <- newIORef $ runIdentity e
--   f' <- newIORef $ runIdentity f
--   return $ PrimOC a' b' c' d' e' f'
--
-- ioToUB :: IOOC -> IO UBOC
-- ioToUB (PrimOC a b c d e f) = do
--   a' <- Identity <$> readIORef a
--   b' <- Identity <$> readIORef b
--   c' <- Identity <$> readIORef c
--   d' <- Identity <$> readIORef d
--   e' <- Identity <$> readIORef e
--   f' <- Identity <$> readIORef f
--   return $ PrimOC a' b' c' d' e' f'

stToUB :: STOC s -> ST s UBOC
stToUB (PrimOC a b c d e f) = do
  a' <- Identity <$> readSTRef a
  b' <- Identity <$> readSTRef b
  c' <- Identity <$> readSTRef c
  d' <- Identity <$> readSTRef d
  e' <- Identity <$> readSTRef e
  f' <- Identity <$> readSTRef f
  return $ PrimOC a' b' c' d' e' f'

inputOpCode :: [Integer] -> UBOC -> UBOC
inputOpCode i oc = oc {_input = Identity $ (`DL.append` DL.fromList i) $ runIdentity $ _input oc}

runOpCodeWith :: (forall s. STOC s -> ST s ()) -> UBOC -> UBOC
runOpCodeWith f oc = runST $ do
  oc' <- ubToST oc
  f oc'
  stToUB oc'

runIOOC :: STOC RealWorld -> IO ()
runIOOC = stToIO . runSTOC

runSTOC :: STOC s -> ST s ()
runSTOC oc = do
  i <- readSTRef $ _position oc
  base <- readSTRef $ _base oc
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
    99 -> writeSTRef (_halt oc) True
    3 -> do
      c <- p1' <$> readDefault oc (i + 1)
      l <- readSTRef $ _input oc
      case l of
        DL.Nil -> pure ()
        DL.Cons h t -> do
          modifySTRef (_vector oc) (IM.insert (fromIntegral c) h)
          writeSTRef (_input oc) (DL.fromList t)
          modifySTRef (_position oc) (+ 2)
          runSTOC oc
    4 -> do
      c <- readDefault oc (i + 1) >>= p1
      modifySTRef (_output oc) (`DL.snoc` c)
      modifySTRef (_position oc) (+ 2)
      runSTOC oc
    9 -> do
      c <- readDefault oc (i + 1) >>= p1
      modifySTRef (_base oc) (+ fromIntegral c)
      modifySTRef (_position oc) (+ 2)
      runSTOC oc
    y | y `elem` [5, 6] -> do
      a <- readDefault oc (i + 1) >>= p1
      let c = case y of
            5 -> a /= 0
            6 -> a == 0
      if c
        then readDefault oc (i + 2) >>= p2 >>= \w -> writeSTRef (_position oc) (fromIntegral w)
        else modifySTRef (_position oc) (+ 3)
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
      modifySTRef (_vector oc) (IM.insert c (op a b))
      modifySTRef (_position oc) (+ 4)
      runSTOC oc
