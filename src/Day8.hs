module Day8 where

import Data.Foldable (Foldable (foldl'), minimumBy)
import Data.Function (on)
import Data.List.Split (chunksOf)

tall = 6

wide = 25

day8 :: IO ()
day8 = do
  input <- chunksOf (tall * wide) . init <$> readFile "input/input8.txt"
  putStrLn
    . ("day8a: " ++)
    . show
    . ((*) <$> length . filter (== '1') <*> length . filter (== '2'))
    . minimumBy (compare `on` length . filter (== '0'))
    $ input
  putStrLn
    . ("day8b: \n" ++)
    . unlines
    . chunksOf 25
    . foldl'
      ( zipWith
          ( \acc y -> case acc of
              '2' -> case y of
                '1' -> '#'
                '0' -> ' '
                z -> z
              _ -> acc
          )
      )
      (repeat '2')
    $ input
