module Day13 where

import Paths_AOC2019
import IntCode

day13 :: IO ()
day13 = do
  v <- readPure <$> (getDataDir >>= readFile . (++ "/input/input13.txt"))
  print v
