module Day4 where


import Paths_AOC2019
import Data.Char (digitToInt)

import Data.List (group, partition)

input = map (map digitToInt . show) [172930 .. 683082]

pairs :: [a] -> [(a, a)]
pairs (x : y : xs) = (x, y) : pairs (y : xs)
pairs _ = []

inc :: (Ord a) => (a, a) -> Bool
inc = uncurry (<=)

eq :: (Eq a) => (a, a) -> Bool
eq = uncurry (==)

day4 :: IO ()
day4 = do
  putStrLn
    . ("day4a: " ++)
    . show
    . length
    . filter ((&&) <$> all inc . pairs <*> any eq . pairs)
    $ input

  putStrLn
    . ("day4b: " ++)
    . show
    . length
    . filter ((&&) <$> all inc . pairs <*> any ((== 2) . length) . group)
    $ input
