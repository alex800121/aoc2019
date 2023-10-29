module Day2 where

import Data.List.Split (splitOn)
import Data.Vector.Unboxed (fromList)
import qualified Data.Vector.Unboxed as U
import OpCode

target = 19690720

day2 :: IO ()
day2 = do
  v <- map (read @Int) . splitOn "," <$> readFile "input/input2.txt"
  let v' = fromList v U.// [(1, 12), (2, 2)]
      x = runOpCodeWith runSTOC (PrimOC 0 v' [] [])
      f = (U.! 0) . _vector
      v'' =
        [ a * 100 + b
          | a <- [0 .. 99],
            b <- [0 .. 99],
            let c = PrimOC 0 (fromList v U.// [(1, a), (2, b)]) [] [],
            target == f (snd $ runOpCodeWith runSTOC c)
        ]
  putStrLn
    . ("day2a: " ++)
    . show
    . f
    . snd
    $ x
  putStrLn
    . ("day2b: " ++)
    . show
    $ v''
