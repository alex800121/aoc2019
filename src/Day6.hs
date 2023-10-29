module Day6 where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

data Tree a = Tree {_name :: a, _level :: Int, _children :: [Tree a]} deriving (Eq, Ord, Show)

type Planet = String

type Orbit = Tree Planet

type OrbitMap = Map Planet [Planet]

parseInput :: String -> (Planet, OrbitMap)
parseInput s = (Set.elemAt 0 $ mother Set.\\ children, s')
  where
    mother = Map.keysSet s'
    children = Set.fromList $ concat $ Map.elems s'
    s' =
      Map.unionsWith (<>)
        . map ((\(x : y : _) -> Map.singleton x [y]) . splitOn ")")
        . lines
        $ s

buildOrbit :: Int -> Planet -> OrbitMap -> Orbit
buildOrbit i mom om = Tree mom i (maybe [] (map (\x -> buildOrbit (i + 1) x om)) (om Map.!? mom))

countChildren :: Orbit -> Int
countChildren (Tree m i c) = i + sum (map countChildren c)

findParent :: Orbit -> Planet -> [[Planet]]
findParent (Tree n _ c) p
  | p == n = pure []
  | null c = []
  | otherwise = do
      c' <- c
      (n :) <$> findParent c' p

day6b :: [Planet] -> [Planet] -> Int
day6b a@(x : xs) b@(y : ys)
  | x == y = day6b xs ys
  | otherwise = length a + length b

day6 :: IO ()
day6 = do
  -- orbit <- uncurry (buildOrbit 0) . parseInput <$> readFile "input/test6.txt"
  orbit <- uncurry (buildOrbit 0) . parseInput <$> readFile "input/input6.txt"
  putStrLn
    . ("day6a: " ++)
    . show
    . countChildren
    $ orbit
  putStrLn
    . ("day6b: " ++)
    . show
    $ day6b (head $ findParent orbit "SAN") (head $ findParent orbit "YOU")
