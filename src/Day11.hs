{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Day11 where

import Control.Monad.ST.Strict (runST)
import Data.Bifunctor (Bifunctor (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import MyLib (Direction (..), drawGraph, toIndex)
import IntCode
import Paths_AOC2019
import Data.Sequence qualified as S
import Data.Foldable (toList)

type Index = (Int, Int)

type Floor = Map Index Bool

pattern White = True

pattern Black = True

paint :: PureIntCode -> Floor -> Floor
paint v fl = runST $ do
  ic <- fromPure v
  f ic (0, 0) North fl
  where
    f ic i d fl
      | _halt ic = pure fl
      | otherwise = do
          ico <- runIntCode ici
          let [a, b] = toList (_output ico)
              fl' = Map.insert i (a == 1) fl
              d' = if b == 0 then pred d else succ d
              i' = bimap (+ fst i) (+ snd i) (toIndex d')
              ic' = ico {_output = S.empty}
          f ic' i' d' fl'
      where
        c = if Just White == fl Map.!? i then 1 else 0
        ici = ic {_input = S.singleton c}

day11 :: IO ()
day11 = do
  v <- readPure <$> (getDataDir >>= readFile . (++ "/input/input11.txt"))
  putStrLn
    . ("day11a: " ++)
    . show
    . Map.size
    $ paint v Map.empty
  putStrLn
    . ("day11b: \n" ++)
    . unlines
    . drawGraph (\case Just True -> '#'; _ -> ' ')
    $ paint v (Map.singleton (0, 0) True)
