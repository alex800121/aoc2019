module Day2 where


import Paths_AOC2019
import Data.List.Split (splitOn)
import Data.Vector.Unboxed (fromList)
import qualified Data.Vector.Unboxed as U
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
-- import qualified Data.Map as IM
import OpCode
import Data.Functor.Identity (Identity(..))

-- type IntMap = Map Integer
target = 19690720

day2 :: IO ()
day2 = do
  v <- readInput <$> (getDataDir >>= readFile . (++ "/input/input2.txt"))
  let v' = v { _vector = Identity $ IM.insert 1 12 $ IM.insert 2 2 (runIdentity $ _vector v) }
      x = runOpCodeWith runSTOC v'
      f = (IM.! 0) . runIdentity . _vector
      v'' =
        [ a * 100 + b
          | a <- [0 .. 99],
            b <- [0 .. 99],
            let c = v { _vector = Identity $ IM.insert 1 a $ IM.insert 2 b (runIdentity $ _vector v) },
            target == f (runOpCodeWith runSTOC c)
        ]
  putStrLn
    . ("day2a: " ++)
    . show
    . f
    $ x
  putStrLn
    . ("day2b: " ++)
    . show
    $ v''
