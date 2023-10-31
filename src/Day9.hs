module Day9 where

import OpCode

x = readInput "104,1125899906842624,99"

day9 :: IO ()
day9 = do
  input <- readInput <$> readFile "input/input9.txt"
  -- print input
  print $ runOpCodeWith runSTOC $ input {_input = [1]}
  -- print $ runOpCodeWith runSTOC $ x
