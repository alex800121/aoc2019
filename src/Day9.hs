module Day9 where


import Paths_AOC2019
import OpCode

import Data.DList (singleton)

import Data.Functor.Identity (Identity(..))

x = readInput "1102,34915192,34915192,7,4,7,99,0"

day9 :: IO ()
day9 = do
  input <- readInput <$> (getDataDir >>= readFile . (++ "/input/input9.txt"))
  putStrLn
    . ("day9a: " ++)
    . show
    . runIdentity
    . _output
    . runOpCodeWith runSTOC $ input {_input = Identity $ singleton 1}
  putStrLn
    . ("day9b: " ++)
    . show
    . runIdentity
    . _output
    . runOpCodeWith runSTOC $ input {_input = Identity $ singleton 2}
