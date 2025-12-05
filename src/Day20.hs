{-# LANGUAGE TupleSections #-}

module Day20 where

import Data.Bifunctor (Bifunctor (..))
import Data.Char (toLower)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.PQueue.Prio.Min qualified as Q
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import MyLib (drawGraph, drawMap)
import Paths_AOC2019

data Block a
  = Space
  | Portal {_io :: PortalIO, _name :: a}
  deriving (Show, Eq, Ord)

data PortalIO = Inner | Outer deriving (Show, Eq, Ord)

instance Enum PortalIO where
  fromEnum Inner = 0
  fromEnum Outer = 1
  toEnum i = case i `mod` 2 of
    0 -> Inner
    1 -> Outer

type Q a = Q.MinPQueue Int a

type Donut = Map Index (Block String)

type Portal = Map (Block String) [(Int, Block String)]

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
            if x - 2 == 0 then Just (Portal Outer [a, b]) else Just (Portal Inner [a, b])
      '.'
        | Just a <- m Map.!? (x + 1, y),
          Just b <- m Map.!? (x + 2, y),
          a /= '.' && b /= '.' ->
            if x + 2 == w - 1 then Just (Portal Outer [a, b]) else Just (Portal Inner [a, b])
      '.'
        | Just a <- m Map.!? (x, y - 2),
          Just b <- m Map.!? (x, y - 1),
          a /= '.' && b /= '.' ->
            if y - 2 == 0 then Just (Portal Outer [a, b]) else Just (Portal Inner [a, b])
      '.'
        | Just a <- m Map.!? (x, y + 1),
          Just b <- m Map.!? (x, y + 2),
          a /= '.' && b /= '.' ->
            if y + 2 == h - 1 then Just (Portal Outer [a, b]) else Just (Portal Inner [a, b])
      '.' -> Just Space
      _ -> Nothing
    m' = Map.mapMaybe id $ Map.mapWithKey f m
    portals = filter ((/= Space) . snd) $ Map.toList m'
    p' = Map.unions $ map (\(i, k) -> Map.singleton k (bfs m' Set.empty (Set.singleton (i, Space)) 0 [])) portals

dijkstra :: Portal -> String -> Set String -> Q (Block String) -> Maybe Int
dijkstra p fin visited q = case q of
  Q.Empty -> Nothing
  (len, Portal _ b) Q.:< q' | b == fin -> Just len
  (len, Portal _ b) Q.:< q' | b `Set.member` visited -> dijkstra p fin visited q'
  (len, Portal io x) Q.:< q' ->
    let visited' = Set.insert x visited
        next = map (first (+ (len + 1))) $ filter ((`Set.notMember` visited') . _name . snd) $ p Map.! Portal (succ io) x
        q'' = Q.union (Q.fromList next) q'
     in dijkstra p fin visited' q''

dijkstra' :: Portal -> Portal -> String -> Set (Int, Block String) -> Q (Int, Block String) -> Maybe Int
dijkstra' pin pout fin visited q = case q of
  Q.Empty -> Nothing
  (len, (_, Portal _ b)) Q.:< _ | b == fin -> Just len
  (len, b) Q.:< q' | b `Set.member` visited -> dijkstra' pin pout fin visited q'
  (len, b@(level, Portal io x)) Q.:< q' ->
    let visited' = Set.insert b visited
        p = case compare 0 level' of
          GT -> Map.empty
          EQ -> pout
          _ -> pin
        level' =
          level + case io of
            Inner -> 1
            Outer -> -1
        next = filter ((`Set.notMember` visited') . snd) $ maybe [] (map (bimap (+ (len + 1)) (level',))) (p Map.!? Portal (succ io) x)
        q'' = Q.union (Q.fromList next) q'
     in dijkstra' pin pout fin visited' q''

bfs :: Donut -> Set Index -> Set (Index, Block String) -> Int -> [(Int, Block String)] -> [(Int, Block String)]
bfs d visited next len m
  -- \| traceShow (s) False = undefined
  | Set.null next = m
  | otherwise = bfs d visited' next' (len + 1) m'
  where
    visited' = Set.union visited $ Set.map fst next
    (s, p) = Set.partition ((== Space) . snd) next
    next' =
      Set.filter ((`Set.notMember` visited') . fst)
        . Set.unions
        $ map
          ( \(x, y) ->
              Set.map fromJust
                . Set.filter isJust
                $ Set.map
                  ( \(k, a) ->
                      let k' = bimap (+ x) (+ y) k
                       in (k',) <$> d Map.!? k'
                  )
                  s
          )
          adjacent
    m' = map ((len,) . snd) (Set.toList p) ++ m

adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

day20 :: IO (String, String)
day20 = do
  -- (donut, portal) <- readInput <$> readFile "input/test20.txt"
  (donut, portal) <- readInput <$> (getDataDir >>= readFile . (++ "/input/input20.txt"))
  let fin = (`notElem` ["AA", "ZZ"]) . _name
      pin = Map.map (filter (fin . snd)) $ Map.filterWithKey (\k _ -> fin k) portal
      fout x = _io x == Inner || _name x `elem` ["AA", "ZZ"]
      pout = Map.filterWithKey (\k a -> fout k && not (null a)) $ Map.map (filter (fout . snd)) portal
  -- print donut
  -- print $ portal
  let
   !finalAnsa
    = show
    $ dijkstra portal "ZZ" (Set.singleton "AA") (Q.fromList $ portal Map.! Portal Outer "AA")
  -- print pin
  -- print pout
  let
   !finalAnsb
    = show
    $ dijkstra' pin pout "ZZ" (Set.singleton (0, Portal Outer "AA")) (Q.fromList $ map (fmap (0,)) $ pout Map.! Portal Outer "AA")
  pure (finalAnsa, finalAnsb)
