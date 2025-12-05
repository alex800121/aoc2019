module Day8 where

import Data.Foldable (Foldable (foldl'), minimumBy)
import Data.Function (on)
import Data.List.Split (chunksOf)
import Paths_AOC2019

tall = 6

wide = 25

day8 :: IO (String, String)
day8 = do
  input <- chunksOf (tall * wide) . init <$> (getDataDir >>= readFile . (++ "/input/input8.txt"))
  let
   !finalAnsa
    = show
    . ((*) <$> length . filter (== '1') <*> length . filter (== '2'))
    . minimumBy (compare `on` length . filter (== '0'))
    $ input
  let
   !finalAnsb
    = ('\n' :)
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
  pure (finalAnsa, finalAnsb)
