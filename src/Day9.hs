module Day9 where

import OpCode

x = readInput "1102,34915192,34915192,7,4,7,99,0"

day9 :: IO ()
day9 = do
  input <- readInput <$> readFile "input/input9.txt"
  putStrLn
    . ("day9a: " ++)
    . show
    . _output
    . runOpCodeWith runSTOC $ input {_input = [1]}
  putStrLn
    . ("day9b: " ++)
    . show
    . _output
    . runOpCodeWith runSTOC $ input {_input = [2]}
