module Day23 where

import Control.Monad.ST.Strict (runST)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as S
import Data.Vector.Strict.Mutable (STVector)
import Data.Vector.Strict.Mutable qualified as MV
import Debug.Trace (traceM)
import IntCode
import Paths_AOC2019

day23a v = runST $ do
  ics <- MV.generateM 50 (\i -> fromPure v >>= \v -> runIntCode (v {_input = S.singleton i}))
  -- MV.mapM_ (traceM . show . _input) ics
  f (S.fromList [S.fromList [i, -1] | i <- [0 .. 49]]) ics
  where
    f ((255 :<| s) :<| _) _ = pure $ s S.!? 1
    f ((i :<| s) :<| xs) ics = do
      ic <- MV.read ics i
      ic' <- runIntCode (ic {_input = _input ic <> s})
      let o = S.chunksOf 3 $ _output ic'
      MV.write ics i (ic' {_output = S.empty})
      f (xs <> o) ics

day23b v = runST $ do
  ics <- MV.generateM 50 (\i -> fromPure v >>= \v -> runIntCode (v {_input = S.singleton i}))
  f (S.fromList [S.fromList [i, -1] | i <- [0 .. 49]]) S.empty (S.singleton (-1)) ics
  where
    f Empty delivered r ics
      | delivered == r = pure $ r S.!? 1
      | otherwise = f (S.singleton (0 :<| r)) r r ics
    f ((255 :<| s) :<| xs) delivered r ics = f xs delivered s ics
    f ((i :<| s) :<| xs) delivered r ics = do
      ic <- MV.read ics i
      ic' <- runIntCode (ic {_input = _input ic <> s})
      let o = S.chunksOf 3 $ _output ic'
      MV.write ics i (ic' {_output = S.empty})
      f (xs <> o) delivered r ics

day23 :: IO (String, String)
day23 = do
  oc <- readPure <$> (getDataDir >>= readFile . (++ "/input/input23.txt"))
  let
   !finalAnsa
    = show
    $ day23a oc
  let
   !finalAnsb
    = show
    $ day23b oc
  pure (finalAnsa, finalAnsb)
