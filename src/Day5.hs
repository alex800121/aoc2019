module Day5 where

import Data.List.Split (splitOn)
import Data.Vector.Unboxed (fromList)
import OpCode 

day5 :: IO ()
day5 = do
  input <- readInput <$> readFile "input/input5.txt"
  let day5a = runOpCodeWith runSTOC $ input { _input = [1] }
      day5b = runOpCodeWith runSTOC $ input { _input = [5] }
  putStrLn
    . ("day5a: " ++)
    . show
    . head
    . _output
    $ day5a
  putStrLn
    . ("day5b: " ++)
    . show
    . head
    . _output
    $ day5b
