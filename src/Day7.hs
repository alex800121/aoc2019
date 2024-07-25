module Day7 where


import Paths_AOC2019
import Data.List (foldl', permutations)

import OpCode

import qualified Data.DList as DL

import Data.Functor.Identity (Identity(..))

candidates = permutations [0 .. 4]
candidates' = permutations [5 .. 9]

day7a :: DL.DList Integer -> UBOC -> Integer
day7a input oc = foldl' (\acc x -> DL.head (runIdentity $ _output (runOpCodeWith runSTOC (oc {_input = Identity $ DL.fromList [x, acc]})))) 0 input

day7b :: [[Integer]] -> [UBOC] -> [[Integer]]
day7b input oc
  | all (runIdentity . _halt) runOC = output
  | otherwise = day7b input' runOC
  where
    ocStart = zipWith (\x y -> x {_input = Identity (runIdentity (_input x) `DL.append` DL.fromList y), _output = Identity DL.empty}) oc input
    runOC = map (runOpCodeWith runSTOC) ocStart
    output = map (DL.toList . runIdentity . _output) runOC
    input' = last output : init output

day7 :: IO ()
day7 = do
  oc <- readInput <$> (getDataDir >>= readFile . (++ "/input/input7.txt"))
  putStrLn
    . ("day7a: " ++)
    . show
    . maximum
    $ map ((`day7a` oc) . DL.fromList) candidates
  putStrLn
    . ("day7b: " ++)
    . show
    . maximum
    $ map (head . last . (`day7b` repeat oc) . (\x -> (head x : [0]) : map (:[]) (tail x))) candidates'
