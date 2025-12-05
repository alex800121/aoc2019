module Day16 where

import Control.Parallel.Strategies
import Data.Bits
import Data.Char (digitToInt, intToDigit)
import Data.Finite
import Data.List (tails)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Debug.Trace (traceShow)
import MyLib (extEuc)
import Paths_AOC2019

p = [0, 1, 0, -1]

step = V.fromList . zipWith (\x y -> (`mod` 10) . abs . V.sum $ V.zipWith (*) x y) m0 . replicate l

m0 :: [Vector Int] =
  take
    l
    [V.fromList $ take l $ drop 1 (cycle (concatMap (replicate i) p)) | i <- [1 .. l]]

fastStep = parMap rpar ((`mod` 10) . sum . zipWith (*) m)

l = 650 :: Int

i = 100 :: Int

r = 10000 :: Int

m :: [Int] =
  [ fromIntegral (binCoeffMod10 (k + 99) k `mod` 10)
    | k <- [0 .. fromIntegral l * r]
  ]

binCoeff a b = product [a - b + 1 .. a] `div` product [1 .. b]

binCoeffModPrime a b p = go 1 a b
  where
    go !acc 0 0 = acc
    go !acc a b = go (acc * binCoeff ma mb) da db
      where
        (da, ma) = a `divMod` p
        (db, mb) = b `divMod` p

binCoeffMod10 a b = 5 * binCoeffModPrime a b 2 - 4 * binCoeffModPrime a b 5

-- extEuc 2 5
-- (-2,1,1)
-- -2 * 2 + 1 * 5 = 1
-- N = m2 mod 2 = m5 mod 5 = m10 mod 10
-- m10 = m2 * 5 * 1 + m5 * -2 * 2

day16 :: IO (String, String)
day16 = do
  input <- init <$> (getDataDir >>= readFile . (++ "/input/input16.txt"))
  let input' = map digitToInt input
      offset = read $ take 7 input
      !l' = take 8 $ tails (drop offset $ concat (replicate (fromIntegral r) input'))
  let
   !finalAnsa
    = map intToDigit
    . V.toList
    . V.take 8
    $ iterate step (V.fromList input') !! 100
  let
   !finalAnsb
    = map intToDigit
    $ fastStep l'
  pure (finalAnsa, finalAnsb)
