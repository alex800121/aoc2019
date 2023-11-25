{-# LANGUAGE LambdaCase #-}

module Day20 where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import MyLib (drawGraph, drawMap)
import Data.List (sort)

data Block a
  = Space
  | Outer a
  | Inner a
  | AA
  | ZZ
  deriving (Show, Eq, Ord)

type Donut = Map Index (Block String)

type Portal = Map Index (Block Index)

type Index = (Int, Int)

readInput :: String -> (Donut, Portal)
readInput s = (m', p')
  where
    s' = lines s
    h = length s'
    w = length $ head s'
    m = drawMap (\x -> if x == '#' then Nothing else Just x) s'
    f (x, y) c = case c of
      '.'
        | Just a <- m Map.!? (x - 2, y),
          Just b <- m Map.!? (x - 1, y),
          a /= '.' && b /= '.' ->
            if x - 2 == 0 then Just (Outer [a, b]) else Just (Inner [a, b])
      '.'
        | Just a <- m Map.!? (x + 1, y),
          Just b <- m Map.!? (x + 2, y),
          a /= '.' && b /= '.' ->
            if x + 2 == w - 1 then Just (Outer [a, b]) else Just (Inner [a, b])
      '.'
        | Just a <- m Map.!? (x, y - 2),
          Just b <- m Map.!? (x, y - 1),
          a /= '.' && b /= '.' ->
            if y - 2 == 0 then Just (Outer [a, b]) else Just (Inner [a, b])
      '.'
        | Just a <- m Map.!? (x, y + 1),
          Just b <- m Map.!? (x, y + 2),
          a /= '.' && b /= '.' ->
            if y + 2 == h - 1 then Just (Outer [a, b]) else Just (Inner [a, b])
      '.' -> Just Space
      _ -> Nothing
    m' = Map.mapMaybe id $ Map.mapWithKey f m
    p' =
      Map.map
        ( \case
            Space -> Space
            Inner s | s == "AA" -> AA
            Inner s | s == "ZZ" -> ZZ
            Outer s | s == "ZZ" -> ZZ
            Outer s | s == "AA" -> AA
            Inner s -> Inner $ fst $ Map.findMin $ Map.filter (== Outer s) m'
            Outer s -> Outer $ fst $ Map.findMin $ Map.filter (== Inner s) m'
        )
        m'

day20 :: IO ()
day20 = do
  (donut, portal) <- readInput <$> readFile "input/input20.txt"
  -- putStrLn $ unlines $ drawGraph (\case ; Nothing -> ' '; Just Space -> '.'; Just (Inner c) -> toLower $ head c; Just (Outer c) -> head c) donut
  putStrLn $ unlines $ drawGraph (\case Nothing -> ' '; Just Space -> '.'; Just (Inner c) -> 'p'; Just (Outer c) -> 'P' ; Just AA -> 'A'; Just ZZ -> 'Z') portal
