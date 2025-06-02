module Day18 where

import Control.Parallel (par)
import Data.Array.IArray qualified as I
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (Bits (..))
import Data.Char (chr, isLowerCase, isUpperCase, ord, toUpper)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List (foldl', unfoldr)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.PQueue.Prio.Min (MinPQueue (..))
import Data.PQueue.Prio.Min qualified as Q
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Debug.Trace
import MyLib (drawArray)
import Paths_AOC2019 (getDataDir)

type Index = (Int, Int)

adjacent = [(-1, 0), (1, 0), (0, -1), (0, 1)]

readInput f ls =
  I.accumArray
    (\_ (i, j) -> (i `shiftL` 32) .|. foldl' (\acc x -> acc `setBit` fromChar x) (0 :: Word64) j)
    maxBound
    ((0, 0), (29, 29))
    xs
  where
    xs =
      [ ((s, e), (n, d))
        | (i, k) <- keys,
          (s, (e, (n, d))) <- bfs (fromChar k) Set.empty 0 (Map.singleton i "") [],
          e < 26
      ]
    b = I.bounds ls
    ks = I.assocs ls
    keys = [(i, k) | (i, k) <- ks, k `elem` take 4 (iterate pred '@') || isLowerCase k]
    bfs start visited n starts acc
      | Map.null starts = acc
      | otherwise = bfs start visited' n' starts' acc'
      where
        n' = n + 1
        visited' = Map.keysSet starts <> visited
        (starts', acc') =
          Map.foldlWithKey'
            ( \(ne, a) i d -> case ls I.!? i of
                _ | i `Set.member` visited' -> (ne, a)
                Nothing -> (ne, a)
                Just '#' -> (ne, a)
                Just c | isUpperCase c -> (Map.insert i (c : d) ne, a)
                Just c | isLowerCase c -> (f i c d ne, (start, (fromChar c, (n', d))) : a)
                -- Just c | isLowerCase c -> (Map.insert i (c : d) ne, (start, (fromChar c, (n', d))) : a)
                Just _ -> (Map.insert i d ne, a)
            )
            (Map.empty, acc)
            $ Map.unionsWith
              (<>)
              [ Map.mapKeys (bimap (+ x) (+ y)) starts
                | (x, y) <- adjacent
              ]

fromChar :: Char -> Int
fromChar c
  | isUpperCase c = ord c - ord 'A'
  | isLowerCase c = ord c - ord 'a'
  | otherwise = ord c - ord '@' + 29

toChar :: Int -> Char
toChar i
  | i < 26 = chr (i + ord 'a')
  | otherwise = chr (i - 29 + ord '@')

toDoors f x = [f i | i <- [0 .. 25], x `testBit` i]

dijkstra' _ _ Empty = Nothing
dijkstra' ref visited ((n, (start, keys)) :< q)
  | keys == 0 = Just n
  | otherwise = dijkstra' ref visited' q'
  where
    (!visited', !q') =
      foldl'
        ( \(v, q) (n, x) -> case v IM.!? toInt x of
            Just n' | n' <= n -> (v, q)
            _ -> (IM.insert (toInt x) n v, Q.insert n x q)
        )
        (visited, q)
        [ (n + n', x)
          | next <- [0 .. 25],
            let (n', doors) = (ref I.! (start, next)) `divMod` bit 32,
            doors /= maxBound,
            doors .&. keys == 0,
            let keys' = keys `clearBit` next,
            let x = (next, keys')
        ]

dijkstra _ _ Empty = Nothing
dijkstra ref visited ((n, (starts, keys)) :< q)
  | keys == 0 = Just n
  | otherwise = dijkstra ref visited' q'
  where
    (!visited', !q') =
      foldl'
        ( \(v, q) (n, x) -> case v IM.!? toInt x of
            Just n' | n' <= n -> (v, q)
            _ -> (IM.insert (toInt x) n v, Q.insert n x q)
        )
        (visited, q)
        [ (n + n', x)
          | start <- [0 .. 29],
            starts `testBit` start,
            next <- [0 .. 25],
            keys `testBit` next,
            let (n', doors) = (ref I.! (start, next)) `divMod` bit 32,
            doors /= maxBound,
            doors .&. keys == 0,
            let keys' = keys `clearBit` next,
            let x = (starts `clearBit` start `setBit` next, keys')
        ]

toInt (startW, keys) = fromIntegral startW `shiftL` 26 .|. fromIntegral keys

fixInput a = a I.// ys
  where
    ((minx, miny), (maxx, maxy)) = I.bounds a
    (cx, cy) = ((minx + maxx) `div` 2, (miny + maxy) `div` 2)
    xs :: UArray Index Char = drawArray ["@#=", "###", ">#?"]
    ys = [((a + cx - 1, b + cy - 1), c) | ((a, b), c) <- I.assocs xs]

day18 :: IO ()
day18 = do
  input :: UArray Index Char <- drawArray . lines <$> (getDataDir >>= readFile . (++ "/input/input18.txt"))
  let !inputA :: UArray Index Word64 = readInput (\_ _ _ ne -> ne) input
      !inputB :: UArray Index Word64 = readInput (\i c d ne -> Map.insert i (c : d) ne) (fixInput input)
      !allKeys = bit 26 - 1
      !startA = (29, allKeys)
      !startB = (foldl' setBit 0 [26 .. 29] :: Word64, allKeys)
      a = par b $ dijkstra' inputA (IM.singleton (toInt startA) 0) (Q.singleton 0 startA)
      b = dijkstra inputB (IM.singleton (toInt startB) 0) (Q.singleton 0 startB)
  putStrLn
    . ("day18a: " ++)
    . show
    $ a
  putStrLn
    . ("day18b: " ++)
    . show
    $ b
