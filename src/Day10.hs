{-# LANGUAGE LambdaCase #-}

module Day10 where

import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Map (keysSet)
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MS
import Data.Set (Set)
import Data.Set qualified as Set
import MyLib (drawMap)
import Paths_AOC2019

type Index = (Int, Int)

newtype Index' = Index' {_index' :: (Int, Index)} deriving (Eq, Show)

instance Ord Index' where
  Index' (m0, a0@(x0, y0)) `compare` Index' (m1, b0@(x1, y1)) =
    compare m0 m1
      <> compare (p a0) (p b0)
      <> compare (fromIntegral y0 / fromIntegral x0) (fromIntegral y1 / fromIntegral x1)
    where
      p (x, y)
        | x >= 0 && y <= 0 = 0
        | x >= 0 && y > 0 = 1
        | x < 0 && y > 0 = 2
        | x < 0 && y <= 0 = 3

reduceAsteroid :: Index -> Index -> Index
reduceAsteroid (x0, y0) (x1, y1)
  | x' == 0 && y' == 0 = (0, 0)
  | otherwise = (x, y)
  where
    x' = x1 - x0
    y' = y1 - y0
    a = gcd x' y'
    x = x' `div` a
    y = y' `div` a

buildIndex' :: MultiSet Index -> Set Index'
buildIndex' =
  MS.foldOccur
    ( \x n acc ->
        acc <> Set.fromList [Index' (i, x) | i <- [0 .. (n - 1)]]
    )
    Set.empty

day10 :: IO ()
day10 = do
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input10.txt"))
  let m =
        keysSet
          . drawMap
            ( \case
                '.' -> Nothing
                '#' -> Just ()
            )
          $ input
      m' =
        Set.map
          (\x -> (x, MS.deleteAll (0, 0) $ MS.map (reduceAsteroid x) $ MS.fromSet m))
          m
      day10a =
        maximumBy (compare `on` (MS.distinctSize . snd)) $
          Set.toList m'
      day10b =
        (\(x, y) -> 100 * x + y)
          . uncurry (\(a, b) (c, d) -> (a + c, b + d))
          $ fmap (snd . _index' . Set.elemAt 199 . buildIndex') day10a
  putStrLn
    . ("day10a: " ++)
    . show
    . MS.distinctSize
    . snd
    $ day10a
  putStrLn
    . ("day10b: " ++)
    . show
    $ day10b
