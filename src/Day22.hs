{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day22 where

import Data.Finite
import Data.Foldable (Foldable (..))
import Data.Group
import Data.Kind (Type)
import Data.List (elemIndex, findIndex, sort)
import Data.Semigroup (Semigroup (..))
import GHC.TypeLits (Natural)
import GHC.TypeNats (KnownNat)
import Paths_AOC2019

newtype Perm (f :: Natural -> Type) n = Perm {applyPerm :: f n -> f n}

data Affine n = Aff {_affScale :: Finite n, _affShift :: Finite n}

readPerm :: (KnownNat n) => String -> Perm Finite n
readPerm s = case words s of
  ["cut", x] | i <- read x -> Perm $ subtract (modulo i)
  ["deal", "into", _, _] -> Perm negate
  ["deal", "with", _, x] | i <- read x -> Perm (* modulo i)

readAffine :: (KnownNat n) => String -> Affine n
readAffine s = case words s of
  ["cut", x] | i <- modulo (read x) -> Aff 1 (negate i)
  ["deal", "into", _, _] -> Aff (-1) (-1)
  ["deal", "with", _, x] | i <- modulo (read x) -> Aff i 0

applyAff :: (KnownNat n) => Affine n -> Finite n -> Finite n
applyAff (Aff a b) x = a * x + b

instance (KnownNat n) => Semigroup (Affine n) where
  Aff a b <> Aff c d = Aff (a * c) (a * d + b)

instance (KnownNat n) => Monoid (Affine n) where
  mempty = Aff 1 0

instance (KnownNat n) => Group (Affine n) where
  invert (Aff a b) = Aff c d
    where
      -- c = a ^ (maxBound @(Finite n) - 1)
      c = a ^ modulo @n (-2)
      d = negate (c * b)

{-
 KnownNat n
 Aff a b <> Aff c d = Aff 1 0
 a * c = 1
 c = a ^ (-1)
 a * d + b = 0
 a * d = (-b)
 d = (a ^ (-1)) * (-b)
 d = c * (-b)
-}
type SmallDeck = Finite 10007

type BigDeck = Finite 119315717514047

shuffleTime = 101741582076661

mergeInstruction :: (KnownNat n) => [Affine n] -> Affine n
mergeInstruction = mconcat . reverse

day22 :: IO (String, String)
day22 = do
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input22.txt"))
  let permA = mergeInstruction $ map readAffine input
      permB = invert $ stimes shuffleTime $ mergeInstruction $ map readAffine input
  let
   !finalAnsa
    = show
    $ applyAff permA (2019 :: SmallDeck)
  let
   !finalAnsb
    = show
    $ applyAff permB (2020 :: BigDeck)
  pure (finalAnsa, finalAnsb)
