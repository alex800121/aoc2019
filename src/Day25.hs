module Day25 where
import Paths_AOC2019

day25 :: IO ()
day25 = do
  input <- (getDataDir >>= readFile . (++ "/input/input25.txt"))
  print input
