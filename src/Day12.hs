{-# LANGUAGE TupleSections #-}
module Day12 where

import Data.List (foldl', transpose)
import Data.Maybe (mapMaybe, catMaybes)
import MyLib
import Text.Megaparsec (parseMaybe, takeRest)
import Text.Megaparsec.Char (string)
import Data.Function (on)
import Data.Bifunctor (bimap)

-- <x=-4, y=-14, z=8>
-- <x=1, y=-8, z=10>
-- <x=-15, y=2, z=1>
-- <x=-17, y=-17, z=16>

type Index = [Int]

type Velo = Index

type Pos = Index

type Planet = (Pos, Velo)

parsePlanet :: Parser Planet
parsePlanet = do
  string "<x="
  x <- signedInteger
  string ", y="
  y <- signedInteger
  string ", z="
  z <- signedInteger
  takeRest
  let a = [0, 0, 0]
  return ([x, y, z], a)

movePlanets :: [Planet] -> [Planet]
movePlanets s = do
  ((x, v0), xs) <- pickAnySplit s
  let v1 =
        foldl'
          ( \a (d, _) ->
              let f' g h = case compare g h of
                    EQ -> id
                    LT -> subtract 1
                    GT -> (+ 1)
               -- in (f' d x a, f' e y b, f' f z c)
               in zipWith ($) (zipWith f' d x) a
          )
          v0
          xs
  return (zipWith (+) x v1, v1)

energy :: Planet -> Int
energy (x, a) = product $ map (sum . map abs) [x, a]

day12b :: [Planet] -> Int
day12b p = foldr lcm 1 $ mapMaybe pf p'
  where
    p' :: [[Planet]]
    p' = map (map ((, [0]) . pure)) $ transpose $ map fst p
    pf = fmap fst . firstRepeat' . iterate movePlanets

day12 :: IO ()
day12 = do
  planets <- mapMaybe (parseMaybe parsePlanet) . lines <$> readFile "input/input12.txt"
  let xs = iterate movePlanets planets
  putStrLn
    . ("day12a: " ++)
    . show
    . sum
    . map energy
    . (!! 1000)
    $ xs
  putStrLn
    . ("day12a: " ++)
    . show
    $ day12b planets
