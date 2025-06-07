module Day19 where

import Paths_AOC2019

day19 :: IO ()
day19 = do
  v <- getDataDir >>= readFile . (++ "/input/input9.txt")
  print ""
