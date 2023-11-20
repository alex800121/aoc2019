{-# LANGUAGE LambdaCase #-}

module Day17 where

import Data.Bifunctor (Bifunctor (..))
import Data.Char (chr, ord)
import qualified Data.DList as DL
import Data.Functor.Identity (Identity (runIdentity))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as Map
import MyLib (Direction (..), drawMap)
import OpCode

l = [(0, 1), (0, -1), (0, 0), (1, 0), (-1, 0)]

type Index = (Int, Int)

type Scaffold = Map Index Char

data Instruction
  = R
  | L
  | F Int
  deriving (Show, Eq, Ord)

buildMainRoute :: Scaffold -> (Index, Direction) -> [Instruction]
buildMainRoute scaf (i, d) = turn (i, d)
  where
    f = \case
      North -> second (subtract 1)
      South -> second (+ 1)
      West -> first (subtract 1)
      East -> first (+ 1)
    turn (a, b)
      | Map.member l scaf = L : forward (a, pred b) 0
      | Map.member r scaf = R : forward (a, succ b) 0
      | otherwise = []
      where
        l = f (pred b) a
        r = f (succ b) a
    forward (x, b) n
      | Map.member i' scaf = forward (i', b) (n + 1)
      | otherwise = F n : turn (x, b)
      where
        i' = f b x

day17 :: IO ()
day17 = do
  oc <- readInput <$> readFile "input/input17.txt"
  let rawInput =
        map (chr . fromIntegral)
          . DL.toList
          . runIdentity
          . _output
          $ runOpCodeWith runSTOC oc
      input =
        drawMap
          ( \case
              '.' -> Nothing
              x -> Just x
          )
          $ lines rawInput
      start =
        head
          . Map.toList
          . Map.map
            ( \case
                '>' -> East
                '<' -> West
                'v' -> South
                '^' -> North
                x -> error (show x)
            )
          $ Map.filter (/= '#') input
      inputA = Map.map (const 1) input
      day17a =
        sum
          . map (uncurry (*))
          . Map.keys
          . Map.filter (== 5)
          . Map.unionsWith (+)
          $ map (\(x, y) -> Map.mapKeys (bimap (+ x) (+ y)) inputA) l
      oc' = oc {_vector = IM.insert 0 2 <$> _vector oc}
  putStrLn
    . ("day17a: " ++)
    $ show day17a
  print $ buildMainRoute input start
