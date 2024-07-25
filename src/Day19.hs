module Day19 where


import Paths_AOC2019
import qualified Data.DList as DL

import Data.Functor.Identity (Identity (..))

import OpCode

import Debug.Trace (traceShow)

day19a oc =
  [ ((x, y), a)
    | x <- [0 .. 49],
      y <- [0 .. 49],
      let a = f oc (x, y)
  ]

f oc (x, y) = DL.head $ runIdentity $ _output $ runOpCodeWith runSTOC $ oc {_input = Identity $ DL.fromList [x, y]}

day19b oc (x, y)
  -- | traceShow (x, y) False = undefined
  | f oc a == 1 && f oc b == 1 = (x, y)
  | f oc a == 0 = day19b oc (x, y + 1)
  | f oc b == 0 = day19b oc (x + 1, y)
  where
    a = (x + 99, y)
    b = (x, y + 99)

day19 :: IO ()
day19 = do
  input <- readInput <$> (getDataDir >>= readFile . (++ "/input/input19.txt"))
  putStrLn
    . ("day19a: " ++)
    . show
    . length
    . filter ((== 1) . snd)
    $ day19a input
  putStrLn
    . ("day19b: " ++)
    . show
    . (\(x, y) -> 10000 * x + y)
    $ day19b input (0, 0)
