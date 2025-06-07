module Day13 where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import IntCode
import Paths_AOC2019
import Control.Monad.ST.Strict (runST)
import Control.Monad ((>=>))
import Data.Functor ((<&>))

day13 :: IO ()
day13 = do
  v <- readPure <$> (getDataDir >>= readFile . (++ "/input/input13.txt"))
  print $ runST  (fromPure v >>= runIntCode <&> _output)
