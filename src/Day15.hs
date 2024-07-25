{-# LANGUAGE LambdaCase #-}

module Day15 where


import Paths_AOC2019
import Data.Bifunctor (Bifunctor (..))

import qualified Data.DList as DL

import Data.Functor.Identity (Identity (..))

import Data.Map (Map)

import qualified Data.Map as Map

import Debug.Trace (trace)

import MyLib (drawGraph)

import OpCode

type Index = (Int, Int)

data Block
  = Wall
  | Space UBOC
  | Oxygen UBOC
  deriving (Show, Ord, Eq)

type M = Map Index Block

directions = [((0, -1), 1), ((0, 1), 2), ((-1, 0), 3), ((1, 0), 4)]

(+^) :: Index -> Index -> Index
(a, b) +^ (c, d) = (a + c, b + d)

dfs :: M -> M -> (Block -> Bool) -> Int -> (Int, Int)
dfs cache start f1 n
  | not . Map.null $ ox = first (const n) $ dfs Map.empty ox' (const False) 0
  | Map.null $ Map.filter (/= Wall) start' = (undefined, n)
  | otherwise = dfs cache' start' f1 (n + 1)
  where
    ox = Map.filter f1 start
    ox' = Map.map (\(Oxygen u) -> Space u) ox
    cache' = Map.union cache start
    start' =
      Map.unions $
        map
          ( \(i, x) ->
              Map.foldlWithKey'
                ( \acc i' block -> case block of
                    Space u
                      | index <- i +^ i',
                        Map.notMember index cache' ->
                          let u' =
                                runOpCodeWith
                                  runSTOC
                                  ( u
                                      { _input = Identity $ DL.singleton x,
                                        _output = Identity DL.empty
                                      }
                                  )
                           in case _output u' of
                                Identity (DL.Cons 0 _) -> Map.insert index Wall acc
                                Identity (DL.Cons 1 _) -> Map.insert index (Space u') acc
                                Identity (DL.Cons 2 _) -> Map.insert index (Oxygen u') acc
                    _ -> acc
                )
                Map.empty
                start
          )
          directions

isOxygen :: Block -> Bool
isOxygen = \case
  Oxygen _ -> True
  _ -> False

day15 :: IO ()
day15 = do
  input <- readInput <$> (getDataDir >>= readFile . (++ "/input/input15.txt"))
  let (a, b) =
        dfs
          Map.empty
          (Map.singleton (0, 0) (Space input))
          isOxygen
          0
  putStrLn
    . ("day15a: " ++)
    $ show a
  putStrLn
    . ("day15b: " ++)
    $ show b
