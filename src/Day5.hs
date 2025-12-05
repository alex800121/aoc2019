module Day5 where

import Control.Monad.ST.Strict (runST)
import Data.Functor ((<&>))
import Data.Foldable (find)
import IntCode
import Paths_AOC2019
-- import Queue qualified as Q
import Data.Sequence qualified as S

day5a v i = runST ((fromPure v' >>= runIntCode) <&> _output)
  where
    v' = v {_pureInput = S.fromList [i]}

day5 :: IO (String, String)
day5 = do
  v <- readPure <$> (getDataDir >>= readFile . (++ "/input/input5.txt"))
  let
   !finalAnsa
    = show
    . find (/= 0)
    $ day5a v 1
  let
   !finalAnsb
    = show
    $ day5a v 5
  pure (finalAnsa, finalAnsb)
