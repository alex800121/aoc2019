module Day21 where
import Paths_AOC2019
day21 :: IO ()
day21 = do
  oc <- (getDataDir >>= readFile . (++ "/input/input21.txt"))
  print ""
