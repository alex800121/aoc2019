{-# LANGUAGE LambdaCase #-}

module Day17 where

import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (..))
import Data.Char (chr, ord)
import qualified Data.DList as DL
import Data.Functor.Identity (Identity (..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (inits, insert, intercalate, intersperse, nub, sort, stripPrefix, tails)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (traceShow)
import MyLib (Direction (..), drawASCII, drawMap)
import OpCode

l = [(0, 1), (0, -1), (0, 0), (1, 0), (-1, 0)]

type Index = (Int, Int)

type Scaffold = Map Index Char

data Instruction
  = R
  | L
  | F Int
  deriving (Eq, Ord)

instance Show Instruction where
  show R = "R"
  show L = "L"
  show (F i) = show i

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

findFunctions :: Int -> [[Instruction]] -> [[[Instruction]]]
findFunctions i ins
  | i == 0 && null ins = [[]]
  | i <= 0 || (i > 0 && null ins) = []
  | otherwise = do
      seg <- ins
      ini <- inits seg
      guard $ length (intercalate "," (map show ini)) <= 20
      insert ini <$> findFunctions (i - 1) (concatMap (filter (not . null) . splitOn ini) ins)

findRoutine :: [(Char, [Instruction])] -> [Instruction] -> [String]
findRoutine ins [] = [[]]
findRoutine ins x = do
  (c, i) <- ins
  case stripPrefix i x of
    Nothing -> []
    Just x' -> (c :) <$> findRoutine ins x'

drawOC =
  drawASCII
    . DL.toList
    . runIdentity
    . _output

day17 :: IO ()
day17 = do
  oc <- readInput <$> readFile "input/input17.txt"
  let rawInput = drawOC $ runOpCodeWith runSTOC oc
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
  putStrLn
    . ("day17a: " ++)
    $ show day17a
  let route = buildMainRoute input start
      functions = head $ findFunctions 3 $ pure route
      mainRoutine =
        (++ [10])
          . map ord
          . intersperse ','
          . head
          $ findRoutine (zip "ABC" functions) route
      functionString = map ((++ [10]) . map ord . intercalate "," . map show) functions
      oc' =
        oc
          { _vector = IM.insert 0 2 <$> _vector oc,
            _input =
              Identity
                . DL.fromList
                . map fromIntegral
                $ mainRoutine ++ concat functionString ++ [ord 'n', 10]
          }
      day17b = last $ DL.toList $ runIdentity $ _output $ runOpCodeWith runSTOC oc'
  putStrLn 
    . ("day17b: " ++)
    $ show day17b
