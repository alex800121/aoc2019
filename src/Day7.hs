module Day7 where

import Data.List (foldl', permutations)
import OpCode

candidates = permutations [0 .. 4]
candidates' = permutations [5 .. 9]

day7a :: [Int] -> UBOC -> Int
day7a input oc = foldl' (\acc x -> head (_output $ snd (runOpCodeWith runSTOC (oc {_input = [x, acc]})))) 0 input

day7b :: [[Int]] -> [UBOC] -> [[Int]]
day7b input oc
  | all fst runOC = output
  | otherwise = day7b input' oc'
  where
    ocStart = zipWith (\x y -> x {_input = _input x ++ y, _output = []}) oc input
    runOC = map (runOpCodeWith runSTOC) ocStart
    oc' = map snd runOC
    output = map _output oc'
    input' = last output : init output

day7 :: IO ()
day7 = do
  oc <- readInput <$> readFile "input/input7.txt"
  putStrLn
    . ("day7a: " ++)
    . show
    . maximum
    $ map (`day7a` oc) candidates
  putStrLn
    . ("day7b: " ++)
    . show
    . maximum
    $ map (head . last . (`day7b` repeat oc) . (\x -> (head x : [0]) : map (:[]) (tail x))) candidates'
