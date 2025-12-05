{-# LANGUAGE MultiWayIf #-}

module Day13 where

import Control.Monad ((>=>))
import Control.Monad.ST.Strict (runST)
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..))
import Data.Sequence qualified as S
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Unboxed.Mutable qualified as MV
import IntCode
import MyLib (drawGraph)
import Paths_AOC2019
import Debug.Trace
import Data.STRef (readSTRef)

type Arcade = (Int, Index, Index, Set Index)

type Index = (Int, Int)

drawScreen (n, acc) ((-1) :<| 0 :<| z :<| xs) = drawScreen (z, acc) xs
drawScreen (n, acc) (x :<| y :<| z :<| xs) = drawScreen (z, Map.insert (x, y) z acc) xs
drawScreen acc _ = acc

play v = runST $ do
  ic <- fromPure v >>= runIntCode
  readSTRef (_code ic) >>= \c -> MV.write c 0 2
  go arcade0 ic
  where
    go acc ic = do
      ic' <- runIntCode ic
      let acc'@(score, ball, pad, block) = f acc (_output ic')
      if
        | Set.null block -> pure score
        | fst ball > fst pad -> go acc' (ic' {_input = S.singleton 1, _output = S.empty})
        | fst ball == fst pad -> go acc' (ic' {_input = S.singleton 0, _output = S.empty})
        | fst ball < fst pad -> go acc' (ic' {_input = S.singleton (-1), _output = S.empty})
    arcade0 = (0, (0, 0), (0, 0), Set.empty)
    f (score, ball, pad, block) ((-1) :<| 0 :<| z :<| xs) = f (z, ball, pad, block) xs
    f (score, ball, pad, block) (x :<| y :<| 0 :<| xs) = f (score, ball, pad, Set.delete (x, y) block) xs
    f (score, ball, pad, block) (x :<| y :<| 2 :<| xs) = f (score, ball, pad, Set.insert (x, y) block) xs
    f (score, ball, pad, block) (x :<| y :<| 3 :<| xs) = f (score, ball, (x, y), block) xs
    f (score, ball, pad, block) (x :<| y :<| 4 :<| xs) = f (score, (x, y), pad, block) xs
    f (score, ball, pad, block) (x :<| y :<| _ :<| xs) = f (score, ball, pad, block) xs
    f acc _ = acc

fromInt 0 = ' '
fromInt 1 = 'X'
fromInt 2 = '#'
fromInt 3 = '_'
fromInt 4 = 'o'

day13 :: IO (String, String)
day13 = do
  v <- readPure <$> (getDataDir >>= readFile . (++ "/input/input13.txt"))
  let
   !finalAnsa
    = show
    . Map.size
    . Map.filter (== 2)
    . snd
    . drawScreen (0, Map.empty)
    $ runST (fromPure v >>= runIntCode <&> _output)
  let
   !finalAnsb
    = show
    $ play v
  pure (finalAnsa, finalAnsb)
