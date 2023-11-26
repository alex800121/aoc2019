module Day22 where

import Data.Foldable (Foldable (..))
import Data.List (elemIndex, findIndex, sort)

readIns :: String -> Int -> Int -> Int
readIns s len = case words s of
  ["cut", x] | i <- read x -> (`mod` len) . subtract i
  ["deal", "into", _, _] -> (+ (len - 1)) . negate
  ["deal", "with", _, x] | i <- read x -> (`mod` len) . (* i)

deckSize = 119315717514047

shuffleTime = 101741582076661

initList = [0 .. 10006]

testList = [0 .. 9]

day22 :: IO ()
day22 = do
  -- input <- lines <$> readFile "input/test22.txt"
  -- print $ foldl' (\acc x -> map (readIns x (length acc)) acc) testList input
  input <- lines <$> readFile "input/input22.txt"
  putStrLn
    . ("day22a: " ++)
    . show
    . (!! 2019)
    $ foldl' (\acc x -> map (readIns x (length acc)) acc) initList input
  print $ maxBound @Int
