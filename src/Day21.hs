module Day21 where

import Control.Monad.ST.Strict (runST)
import Data.Char (chr, ord)
import Data.Foldable (Foldable (..))
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as S
import IntCode
import Paths_AOC2019

toIns ins x = map ord $ intercalate "\n" ins ++ '\n' : x

-- insA
-- NOT (A && B && C) && D
-- OR A T
-- AND B T
-- AND C T
-- NOT T T
-- AND D T
-- OR T J
insA =
  [ "OR A T",
    "AND B T",
    "AND C T",
    "NOT T T",
    "AND D T",
    "OR T J"
  ]

-- insB
-- NOT (A && B && C) && D && (E || H)
-- OR A T
-- AND B T
-- AND C T
-- NOT T T
-- AND D T
-- OR E J
-- OR H J
-- AND T J
insB =
  [ "OR A T",
    "AND B T",
    "AND C T",
    "NOT T T",
    "AND D T",
    "OR E J",
    "OR H J",
    "AND T J"
  ]

day21 :: IO (String, String)
day21 = do
  v <- readPure <$> (getDataDir >>= readFile . (++ "/input/input21.txt"))
  let
   !finalAnsa
    = show
    . (\(_ :|> x) -> x)
    $ runST (fromPure v >>= \x -> runIntCode (x {_input = S.fromList (toIns insA "WALK\n")}) <&> _output)
  let
   !finalAnsb
    = show
    . (\(_ :|> x) -> x)
    $ runST (fromPure v >>= \x -> runIntCode (x {_input = S.fromList (toIns insB "RUN\n")}) <&> _output)
  pure (finalAnsa, finalAnsb)
