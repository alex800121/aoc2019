{-# LANGUAGE MultiWayIf #-}

module Day17 where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.ST.Strict (runST)
import Data.Array.IArray qualified as I
import Data.Array.Unboxed (UArray)
import Data.Bifunctor (Bifunctor (..))
import Data.Char (chr, ord)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.List (elemIndex, inits, intercalate, intersperse)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as S
import Data.Vector.Unboxed qualified as V
import Debug.Trace (traceShow)
import IntCode (IntCode (..), PureIntCode (..), fromPure, readPure, runIntCode)
import MyLib (Direction (..), drawArray, toIndex)
import Paths_AOC2019

data Ins = L | R | F Int deriving (Eq, Ord)

instance Show Ins where
  show L = "L"
  show R = "R"
  show (F i) = show i

buildRoutine r ans l | length ans > 3 || length r > 10 = []
buildRoutine r ans [] = pure (r, ans)
buildRoutine r ans l = do
  x <- inits l
  let ls = length (show x)
  guard $ even (length x) && ls <= 22 && not (null x)
  if
    | Just i <- elemIndex x ans -> buildRoutine (r <> [i]) ans (drop (length x) l)
    | otherwise -> buildRoutine (r <> [length ans]) (ans <> [x]) (drop (length x) l)

buildPath m = tail (f 0 start d)
  where
    (start, d) : _ = [(i, fromChar d) | (i, d) <- I.assocs m, d `elem` "v^<>"]
    path = [i | (i, d) <- I.assocs m, d /= '.']
    f n i d
      | maybe False (/= '.') (m I.!? i') = f (succ n) i' d
      | maybe False (/= '.') (m I.!? i0) = F n : R : f 1 i0 (succ d)
      | maybe False (/= '.') (m I.!? i1) = F n : L : f 1 i1 (pred d)
      | otherwise = [F n]
      where
        i' = bimap (+ fst i) (+ snd i) (toIndex d)
        i0 = bimap (+ fst i) (+ snd i) (toIndex (succ d))
        i1 = bimap (+ fst i) (+ snd i) (toIndex (pred d))

fromChar '^' = North
fromChar 'v' = South
fromChar '<' = West
fromChar '>' = East

buildAns (x, xs) = intercalate [10] (routine : subRoutine <> [[ord 'n']]) <> [10]
  where
    routine = map ord $ intersperse ',' $ map (\x -> chr (x + ord 'A')) x
    f = init . tail . show
    subRoutine = map (map ord . f) xs

day17a m = sum [x * y | ((x, y), '#') <- I.assocs m, 2 < length (filter ((Just '#' ==) . (m I.!?) . bimap (+ x) (+ y) . toIndex) [minBound .. maxBound])]

day17 :: IO (String, String)
day17 = do
  v <- readPure <$> (getDataDir >>= readFile . (++ "/input/input17.txt"))
  let input = runST (fromPure v >>= runIntCode <&> map chr . toList . _output)
      m =
        drawArray @UArray
          . init
          $ lines input
      routine = buildRoutine [] [] $ buildPath m
      ans = head $ map buildAns routine
      v' = v {_pureCode = _pureCode v V.// [(0, 2)], _pureInput = S.fromList ans}
  let
   !finalAnsa
    = show
    $ day17a m
  let
   !finalAnsb
    = show
    $ runST (fromPure v' >>= runIntCode <&> (\(xs :|> x) -> x) . _output)
  pure (finalAnsa, finalAnsb)
