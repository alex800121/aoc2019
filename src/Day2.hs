module Day2 where

import Control.Monad.ST.Strict (runST)
import Data.Vector.Unboxed.Mutable qualified as MV
import IntCode (IntCode (..), fromPure, readPure, runIntCode, toPure)
import Paths_AOC2019

day2a v noun verb =
  runST
    ( fromPure v
        >>= (\ic -> MV.write (_code ic) 1 noun >> MV.write (_code ic) 2 verb >> pure ic)
        >>= runIntCode
        >>= (`MV.read` 0) . _code
    )

day2b v =
  [ 100 * noun + verb
    | noun <- [0 .. 99],
      verb <- [0 .. 99],
      day2a v noun verb == 19690720
  ]

day2 :: IO ()
day2 = do
  v <- readPure <$> (getDataDir >>= readFile . (++ "/input/input2.txt"))
  putStrLn
    . ("day2a: " ++)
    . show
    $ day2a v 12 2
  putStrLn
    . ("day2b: " ++)
    . show
    $ day2b v
