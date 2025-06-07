module Day17 where
import Paths_AOC2019
day17 :: IO ()
day17 = do
  oc <- (getDataDir >>= readFile . (++ "/input/input17.txt"))
  print oc
