{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day24 where

import Data.Array.IArray
import Data.Array.Unboxed
import Data.Bifunctor (Bifunctor (bimap))
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Debug.Trace (traceShow)
import MyLib

adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

type Index = (Int, Int)

type IndexPlus = (Int, (Int, Int))

type M = UArray Index Int

type MPlus = Map IndexPlus Int

(!?) :: (IArray a e, Ix i) => a i e -> i -> Maybe e
m !? i
  | inRange (bounds m) i = Just (m ! i)
  | otherwise = Nothing

midX = 2

midY = 2

mid = (midX, midY)

minX = 0

minY = 0

minXY = (minX, minY)

maxX = 4

maxY = 4

maxXY = (maxX, maxY)

adjacentPlus :: IndexPlus -> Index -> [IndexPlus]
adjacentPlus (l, (x, y)) (a, b)
  | (x', y') == mid && a == 1 = [(l + 1, (minX, a')) | a' <- [minY .. maxY]]
  | (x', y') == mid && a == -1 = [(l + 1, (maxX, a')) | a' <- [minY .. maxY]]
  | (x', y') == mid && b == 1 = [(l + 1, (b', minY)) | b' <- [minX .. maxX]]
  | (x', y') == mid && b == -1 = [(l + 1, (b', maxY)) | b' <- [minX .. maxX]]
  | otherwise = [(if inRange (minY, maxY) y' && inRange (minX, maxX) x' then l else l - 1, (x'' `mod` (maxX - minX + 1), y'' `mod` (maxY - minY + 1)))]
  where
    (x', y') = (x + a, y + b)
    x''
      | inRange (minX, maxX) x' && inRange (minY, maxY) y' = x'
      | inRange (minX, maxX) x' = midX 
      | otherwise = midX + a
    y'' 
      | inRange (minY, maxY) y' && inRange (minX, maxX) x' = y' 
      | inRange (minY, maxY) y' = midY 
      | otherwise = midY + b

stepPlus :: MPlus -> MPlus
stepPlus m = m'
  where
    l = Map.keys m
    (minL, maxL) = (minimum $ map fst l, maximum $ map fst l)
    b = ((minL - 1, (minX, minY)), (maxL + 1, (maxX, maxY)))
    f 1 1 = 1
    f 1 _ = 0
    f 0 1 = 1
    f 0 2 = 1
    f 0 _ = 0
    m' =
      Map.fromList
        [ (i, x)
          | i@(_, (a, b)) <- range b,
            let ix = concatMap (adjacentPlus i) adjacent,
            let x = f (fromMaybe 0 (m Map.!? i)) (sum $ mapMaybe (m Map.!?) ix),
            (a, b) /= (2, 2)
        ]

step :: M -> M
step m = accum f m l
  where
    b = bounds m
    f 1 1 = 1
    f 1 _ = 0
    f 0 1 = 1
    f 0 2 = 1
    f 0 _ = 0
    l =
      [ (i, x)
        | i <- range b,
          let ix = map (bimap (+ fst i) (+ snd i)) adjacent,
          let x = sum $ mapMaybe (m Day24.!?) ix
      ]

test =
  [ ( \(x, y) ->
        ((x, y), concatMap (adjacentPlus (0, (x, y))) adjacent)
    )
      (x, y)
    | x <- [0 .. 4],
      y <- [0 .. 4]
  ]

day24 :: IO ()
day24 = do
  inputMap <-
    drawMap
      ( \case
          '#' -> Just 1
          _ -> Just 0
      )
      . lines
      <$> readFile "input/input24.txt"
  -- <$> readFile "input/test24.txt"
  let a = Map.keys inputMap
      b = (minimum a, maximum a)
      input = array @UArray @Int b $ Map.toList inputMap
      mapPlus = Map.mapKeys (0,) $ Map.delete (2, 2) inputMap
  putStrLn
    . ("day24a: " ++)
    . show
    . foldr (\x acc -> acc * 2 + x) 0
    . concat
    . drawGraph (fromMaybe 0)
    . Map.fromList
    . assocs
    . snd
    . fromJust
    . firstRepeat
    $ iterate step input
  putStrLn
    . ("day24b: " ++)
    . show
    . sum
    . (!! 200)
    $ iterate stepPlus mapPlus
