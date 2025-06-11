{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Day25 where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.ST.Strict (runST, stToIO)
import Data.Char (chr, isNumber, ord)
import Data.Foldable (Foldable (toList))
import Data.List (isInfixOf, tails, uncons, find)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as S
import Data.Set qualified as Set
import Data.Vector.Unboxed qualified as V
import Debug.Trace (trace, traceShow)
import IntCode
import MyLib (pickAnySplit)
import Paths_AOC2019

play v lastIns items initIns = f items' initV
  where
    items' = map ((:|> 10) . S.fromList . map ord) items
    run v x = runST (fromPure (v {_pureInput = x, _pureOutput = S.empty}) >>= runIntCode >>= toPure)
    initV = run v initIns
    lighter = map ord "lighter"
    heavier = map ord "heavier"
    takeItem = S.fromList $ map ord "take "
    walk = S.fromList (map ord lastIns)
    f items v = do
      (x, xs) <- mapMaybe uncons (tails items)
      let i = (takeItem <> x :|> 10) <> walk
          v' = run v i
          o = toList $ _pureOutput v'
      if
        | lighter `isInfixOf` o -> []
        | heavier `isInfixOf` o -> f xs v'
        | otherwise -> pure $ map chr $ toList o

readInitIns s0 = (concatMap f (lines s), lines i, f l)
  where
    [s, i, l] = splitOn "\n\n" s0
    f y
      | length x == 1 = concatMap (\case "s" -> "south\n"; "e" -> "east\n"; "w" -> "west\n"; "n" -> "north\n") xs
      | otherwise = y <> "\n"
      where
        xs@(x : _) = words y

day25 :: IO ()
day25 = do
  v <- readPure <$> (getDataDir >>= readFile . (++ "/input/input25.txt"))
  (initIns, items, lastIns) <- readInitIns <$> (getDataDir >>= readFile . (++ "/input/initIns.txt"))
  putStrLn
    . ("day25a: " ++)
    . show
    . find (all isNumber)
    . words
    . head
    . play v lastIns items
    $ S.fromList (map ord initIns <> concatMap (map ord . (<> "\n") . ("drop " <>)) items)

-- print initIns
