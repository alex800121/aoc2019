module Day12 where

import Control.Parallel.Strategies
import Data.Bifunctor (bimap)
import Data.List (foldl')
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as M
import Debug.Trace
import MyLib
import Paths_AOC2019
import Text.Megaparsec (parseMaybe, takeRest)
import Text.Megaparsec.Char (string)

type Axis = Vector (Int, Int)

type Planets = [Axis]

parsePlanet :: Parser Planets
parsePlanet = do
  string "<x="
  x <- signedInteger
  string ", y="
  y <- signedInteger
  string ", z="
  z <- signedInteger
  takeRest
  pure $ map (V.fromList . pure) [(x, 0), (y, 0), (z, 0)]

moveAxis v =
  V.modify
    ( \mv ->
        let (x0, v0) = v V.! 0
            (x1, v1) = v V.! 1
            (x2, v2) = v V.! 2
            (x3, v3) = v V.! 3
            v0' = v0 + signum (x1 - x0) + signum (x2 - x0) + signum (x3 - x0)
            v1' = v1 + signum (x0 - x1) + signum (x2 - x1) + signum (x3 - x1)
            v2' = v2 + signum (x1 - x2) + signum (x0 - x2) + signum (x3 - x2)
            v3' = v3 + signum (x1 - x3) + signum (x2 - x3) + signum (x0 - x3)
            x0' = x0 + v0'
            x1' = x1 + v1'
            x2' = x2 + v2'
            x3' = x3 + v3'
         in M.write mv 0 (x0', v0') >> M.write mv 1 (x1', v1') >> M.write mv 2 (x2', v2') >> M.write mv 3 (x3', v3')
    )
    v

energy = V.sum . V.map (uncurry (*)) . foldl' (V.zipWith (\(a, b) (c, d) -> (a + abs c, b + abs d))) (V.replicate 4 (0, 0))

findZero f n x = if V.all ((== 0) . snd) x' then 2 * n else findZero f (succ n) x'
  where
    x' = f x

day12b = foldl' lcm 1 . parMap rpar (findZero moveAxis 1)

day12 :: IO (String, String)
day12 = do
  planets <-
    foldl' (zipWith (<>)) (replicate 3 V.empty)
      . mapMaybe (parseMaybe parsePlanet)
      . lines
      <$> (getDataDir >>= readFile . (++ "/input/input12.txt"))
  let xs = iterate (map moveAxis) planets
  let
   !finalAnsa
    = show
    . energy
    . (!! 1000)
    $ xs
  let
   !finalAnsb
    = show
    $ day12b planets
  pure (finalAnsa, finalAnsb)
