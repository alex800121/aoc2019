module Day5 where


import Paths_AOC2019
import Data.List.Split (splitOn)

import Data.Vector.Unboxed (fromList)

import OpCode 

import Data.DList (toList, singleton)

import Data.Functor.Identity (Identity(..))

day5 :: IO ()
day5 = do
  input <- readInput <$> (getDataDir >>= readFile . (++ "/input/input5.txt"))
  let day5a = runOpCodeWith runSTOC $ input { _input = Identity $ singleton 1 }
      day5b = runOpCodeWith runSTOC $ input { _input = Identity $ singleton 5 }
  putStrLn
    . ("day5a: " ++)
    . show
    . last
    . toList
    . runIdentity
    . _output
    $ day5a
  putStrLn
    . ("day5b: " ++)
    . show
    . last
    . toList
    . runIdentity
    . _output
    $ day5b
