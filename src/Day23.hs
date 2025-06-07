module Day23 where

import Paths_AOC2019
day23 :: IO ()
day23 = do
  oc <- (getDataDir >>= readFile . (++ "/input/input23.txt"))
  print oc
