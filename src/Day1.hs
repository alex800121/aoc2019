module Day1 where

import Paths_AOC2019

fuel :: Int -> Int
fuel = subtract 2 . (`div` 3)

day1 :: IO (String, String)
day1 = do
  input <- map (read @Int) . lines <$> (getDataDir >>= readFile . (++ "/input/input1.txt"))
  let
   !finalAnsa
    = show
    . sum
    . map fuel
    $ input
  let
   !finalAnsb
    = show
    . sum
    . map (sum . tail . takeWhile (> 0) . iterate fuel)
    $ input
  pure (finalAnsa, finalAnsb)
