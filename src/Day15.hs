module Day15 where

import Paths_AOC2019

day15 :: IO ()
day15 = do
  input <- (getDataDir >>= readFile . (++ "/input/input15.txt"))
  print input
