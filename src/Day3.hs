module Day3 where


import Paths_AOC2019
import Data.Bifunctor (first, second)

import Data.List (delete, foldl', sort)

import Data.List.Split (splitOn)

import Data.Maybe (catMaybes)

type Index = (Int, Int)

type Line = (Index, Index)

drawLine :: [String] -> [Line]
drawLine = f (0, 0)
  where
    f _ [] = []
    f m0@(x0, y0) (n : xs) = case n of
      'R' : ns -> let m = (x0 + read ns, y0) in (m0, m) : f m xs
      'L' : ns -> let m = (x0 - read ns, y0) in (m0, m) : f m xs
      'U' : ns -> let m = (x0, y0 - read ns) in (m0, m) : f m xs
      'D' : ns -> let m = (x0, y0 + read ns) in (m0, m) : f m xs

intersect :: Line -> Line -> Maybe Line
intersect e f
  | l <= n && m <= o = Just t
  | otherwise = Nothing
  where
    ((x, y), (z, w)) = (uncurry min e, uncurry max e)
    ((a, b), (c, d)) = (uncurry min f, uncurry max f)
    t@((l, m), (n, o)) = ((max x a, max y b), (min z c, min w d))

calcDistance :: Index -> [Line] -> Maybe Int
calcDistance _ [] = Nothing
calcDistance (x, y) (((x0, y0), (x1, y1)) : xs)
  | x == x0 || y == y0 = Just (manhattan (x, y) (x0, y0))
  | otherwise = (+ manhattan (x0, y0) (x1, y1)) <$> calcDistance (x, y) xs

manhattan :: Index -> Index -> Int
manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

day3 :: IO ()
day3 = do
  l0 : l1 : _ <- map (drawLine . splitOn ",") . lines <$> (getDataDir >>= readFile . (++ "/input/input3.txt"))
  let l = delete (0, 0) $ catMaybes [fst <$> intersect x y | x <- l0, y <- l1]
      l' = minimum $ catMaybes [(+) <$> calcDistance x l0 <*> calcDistance x l1 | x <- l]
  putStrLn
    . ("day3a: " ++)
    . show
    . minimum
    . map (manhattan (0, 0))
    $ l
  putStrLn
    . ("day3b: " ++)
    . show
    $ l'
