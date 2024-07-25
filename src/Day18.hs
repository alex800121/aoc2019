{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Day18 where


import Paths_AOC2019
import Control.Monad (join)

import Data.Bifunctor (Bifunctor (..))

import Data.Bits

import Data.Char (chr, isLower, isUpper, ord, toLower, toUpper)

import Data.List (elemIndex, find, findIndex, foldl')

import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map

import Data.Maybe (isJust, mapMaybe)

import Data.PQueue.Prio.Min (MinPQueue (..))

import qualified Data.PQueue.Prio.Min as PQ

import Data.Set (Set)

import qualified Data.Set as Set

import Data.Tuple (swap)

import Debug.Trace (trace, traceShow)

import MyLib (drawGraph, drawMap, pickAnySplit)

data Block
  = Space
  | Key Key
  | Door Door
  deriving (Eq, Ord)

type Index = (Int, Int)

type M = Map Index Block

type KeyMap = Map Key (Map Key [(Doors, Int)])

newtype Key = K Word
  deriving (Eq, Ord, Num, Bits, FiniteBits)

type Keys = Key

type Door = Key

type Doors = Key

instance Show Key where
  show (K w) = foldr (\x acc -> if testBit w x then chr (ord '=' + x) : acc else acc) "" [0 .. 63]

adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

data GameState = G
  { _key :: Key,
    _keys :: Keys
  }
  deriving (Show, Eq, Ord)

type Q = MinPQueue Int GameState

instance Show Block where
  show Space = "."
  show (Key x) | x `elem` map toBit "@?>=" = "@"
  show (Key x) = [chr $ fromIntegral $ ord '=' + (ord 'a' - ord 'A') + log2 x]
  show (Door x) = [chr $ fromIntegral $ ord '=' + log2 x]

log2 x = finiteBitSize x - 1 - countLeadingZeros x

buildKeyMap :: M -> Key -> Set Index -> Set (Index, Doors) -> Int -> Map Key [(Doors, Int)] -> KeyMap
buildKeyMap m startKey visited startIndex len output
  | Set.null startIndex = Map.singleton startKey output
  | otherwise = buildKeyMap m startKey visited' next' (len + 1) output'
  where
    next' =
      Set.unions $
        map (\(x, y) -> Set.map (first (bimap (+ x) (+ y))) startIndex') adjacent
    visited' = Set.union visited $ Set.map fst startIndex'
    (startIndex', output') =
      Set.foldl'
        ( \acc (x, doors) ->
            if x `Set.member` visited
              then acc
              else case m Map.!? x of
                Just Space -> first (Set.insert (x, doors)) acc
                Just (Key k) ->
                  bimap
                    (Set.insert (x, doors))
                    ( if k /= startKey
                        then Map.insertWith (++) k [(doors, len)]
                        else id
                    )
                    acc
                Just (Door d) ->
                  let doors' = doors .|. d
                   in first (Set.insert (x, doors')) acc
                _ -> acc
        )
        (Set.empty, output)
        startIndex

readInput :: String -> (Key, M)
readInput s = (keys, m)
  where
    keys = foldr ((.|.) . toBit) 0 (filter ((||) <$> (== '@') <*> isLower) s)
    s' = lines s
    m =
      drawMap
        ( \case
            '.' -> Just Space
            '#' -> Nothing
            x | isUpper x -> Just $ Door $ toBit x
            x -> Just $ Key $ toBit x
        )
        s'

toBit :: Char -> Key
toBit = K . bit . subtract (ord '=') . ord . toUpper

toChar :: Block -> Char
toChar = head . show

dijkstra :: KeyMap -> Keys -> Set GameState -> MinPQueue Int GameState -> Maybe Int
-- dijkstra km finKeys visited q | traceShow (length q) False = undefined
dijkstra km finKeys visited q = case q of
  Empty -> Nothing
  -- t :< q' | traceShow t False -> undefined
  (_, g) :< q' | g `Set.member` visited -> dijkstra km finKeys visited q'
  (len, g) :< _ | _keys g == finKeys -> Just len
  (len, g) :< q' ->
    let keys = filter (/= 0) $ map ((.&. _key g) . setBit 0) [0 .. 63]
        m = map (\x -> (x, km Map.! x)) keys
        m' = map ((filter ((`notElem` map toBit "@?>=") . fst) . Map.toList . Map.mapMaybe (fmap snd . find f)) <$>) m
        f needed = fst needed .&. _keys g == fst needed
        -- q'' = trace ("**" ++ show m') $
        q'' =
          foldl'
            ( \acc (from, l) ->
                foldl'
                  ( \acc' (to, len') ->
                      let g' = G (_key g - from + to) (_keys g + to)
                       in PQ.insert (len + len') g' acc'
                      -- in PQ.insert (len + snd x) g' acc
                  )
                  acc
                  l
            )
            q'
            m'
        visited' = Set.insert g visited
     in dijkstra km finKeys visited' q''

day18 :: IO ()
day18 = do
  (allKeys, m) <- readInput <$> (getDataDir >>= readFile . (++ "/input/input18.txt"))
  let keys =
        Map.toList $
          Map.mapMaybe
            ( \case
                Key c -> Just c
                _ -> Nothing
            )
            m
      keyMap =
        Map.unionsWith Map.union $
          map
            ( \x ->
                buildKeyMap m (snd x) Set.empty (Set.singleton x) 0 Map.empty
            )
            keys
      initG = G (toBit '@') (toBit '@')
      (x, y) = fst $ Map.findMin $ Map.filter (== Key (toBit '@')) m
      added =
        Map.fromList $
          zip [(a + x, b + y) | a <- [-1, 1], b <- [-1, 1]] $
            map (Key . toBit) "@?>="
      deleted =
        Map.fromList
          [((a + x, b + y), Space) | a <- [-1 .. 1], b <- [-1 .. 1], abs a + abs b <= 1]
      m' = Map.union added m Map.\\ deleted
      keys' =
        Map.toList $
          Map.mapMaybe
            ( \case
                Key c -> Just c
                _ -> Nothing
            )
            m'
      allKeys' = map snd keys'
      keyMap' =
        Map.unionsWith Map.union $
          map
            ( \x ->
                buildKeyMap m' (snd x) Set.empty (Set.singleton x) 0 Map.empty
            )
            keys'
      initQ = PQ.fromList $ do
        let m x = keyMap' Map.! toBit x
            m' x = Map.toList $ Map.mapMaybe (fmap snd . find (f x)) (m x)
            f x needed = fst needed .&. toBit x == fst needed
        a0 <- m' '@'
        a1 <- m' '?'
        a2 <- m' '>'
        a3 <- m' '='
        return $ second (\x -> G x (foldl' (\acc y -> acc .|. toBit y) x "=>?@")) $ swap $ foldl' (\acc -> bimap (.|. fst acc) (+ snd acc)) (0, 0) [a0, a1, a2, a3]
  putStrLn
    . ("day18a: " ++)
    . show
    $ dijkstra keyMap (foldl' (+) 0 $ map snd keys) Set.empty (PQ.singleton 0 initG)
  putStrLn
    . ("day18b: " ++)
    . show
    $ dijkstra keyMap' (foldl' (+) 0 $ map snd keys') Set.empty initQ
