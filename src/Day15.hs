module Day15 where

import Control.Monad (foldM)
import Control.Monad.ST.Strict (runST)
import Data.Bifunctor (Bifunctor (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vector.Unboxed.Mutable qualified as MV
import IntCode
import MyLib (Direction (..), drawGraph, toIndex)
import Paths_AOC2019
import Data.Sequence qualified as S
import Data.Sequence (Seq(..))
import Data.STRef

data Status = Wall | Floor | Oxygen
  deriving (Show, Eq, Enum, Ord)

explore v = runST $ do
  ic <- fromPure v
  foldM f Map.empty [((0, 0), ic, d) | d <- [minBound .. maxBound]]
  where
    f acc (i, ic, d)
      | i' `Map.member` acc = pure acc
      | otherwise = do
          code' <- readSTRef (_code ic) >>= MV.clone >>= newSTRef
          ic' <- runIntCode (ic {_input = S.singleton (fromDir d), _output = S.empty, _code = code'})
          let x :<| _ = _output ic'
              acc' = Map.insert i' (toEnum @Status x) acc
          if x == 0
            then pure acc'
            else foldM f acc' [(i', ic' {_output = S.empty}, d') | d' <- [minBound .. maxBound]]
      where
        i' = bimap (+ fst i) (+ snd i) (toIndex d)

fromDir North = 1
fromDir South = 2
fromDir West = 3
fromDir East = 4

bfs m visited !starts (!s, !t)
  | Set.null next = (s, t)
  | otherwise = bfs m visited' next (s', succ t)
  where
    visited' = visited <> starts
    next =
      Set.fromList
        [ i
          | (x, y) <- Set.toList starts,
            d <- [minBound .. maxBound],
            let i = bimap (+ x) (+ y) (toIndex d),
            m Map.!? i `elem` [Just Floor, Just Oxygen],
            i `Set.notMember` visited'
        ]
    s' = if (0, 0) `Set.member` starts then t else s

day15 :: IO ()
day15 = do
  v <- readPure <$> (getDataDir >>= readFile . (++ "/input/input15.txt"))
  let m = explore v
      o = Map.keysSet $ Map.filter (== Oxygen) m
      (a, b) = bfs m Set.empty o (0, 0)
  putStrLn
    . ("day15a: " ++)
    $ show a
  putStrLn
    . ("day15b: " ++)
    $ show b
