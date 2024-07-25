module Day21 where


import Paths_AOC2019
import Data.Char (chr, ord)

import qualified Data.DList as DL

import Data.Functor.Identity (Identity (..))

import Data.Map (Map)

import qualified Data.Map as Map

import OpCode

data Instruction
  = AND Reg Reg
  | OR Reg Reg
  | NOT Reg Reg
  | WALK
  | RUN
  deriving (Show, Ord, Eq)

data Reg
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | T
  | J
  deriving (Show, Ord, Eq)

inputIns :: [Instruction] -> UBOC -> UBOC
inputIns i oc = oc {_input = Identity $ DL.fromList input}
  where
    input = map (fromIntegral . ord) $ (++ "\n") $ unlines $ map show i

day21a =
  [ NOT T T,
    AND A T,
    AND B T,
    AND C T,
    NOT T T,
    AND D T,
    OR T J,
    WALK
  ]

day21b =
  [ NOT T T,
    AND A T,
    AND B T,
    AND C T,
    NOT T T,
    AND D T,
    OR E J,
    OR H J,
    AND T J,
    RUN
  ]
printOutput :: [Integer] -> String
printOutput [] = ""
printOutput (x : xs) = (if x > fromIntegral (ord maxBound) || x < fromIntegral (ord minBound) then (show x ++) else (chr (fromIntegral x) :)) $ printOutput xs

day21 :: IO ()
day21 = do
  oc <- readInput <$> (getDataDir >>= readFile . (++ "/input/input21.txt"))
  putStrLn
    . ("day21a: " ++)
    . show
    . last
    . DL.toList
    . runIdentity
    . _output
    . runOpCodeWith runSTOC
    $ inputIns day21a oc
  putStrLn
    . ("day21b: " ++)
    . show
    . last
    . DL.toList
    . runIdentity
    . _output
    . runOpCodeWith runSTOC
    $ inputIns day21b oc
