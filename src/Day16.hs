module Day16 where

import Data.Char (digitToInt, intToDigit)

p = [0, 1, 0, -1]

f i j = drop (j + 1) . cycle . concatMap (replicate i)

step :: Int -> [Int] -> [Int]
step j l = map ((`mod` 10) . abs . sum) $ zipWith (zipWith (*)) (replicate len l) [f i j p | i <- [1 .. len]]
  where
    len = length l

fastStep = scanr (\x acc -> (x + acc) `mod` 10) 0

day16 :: IO ()
day16 = do
  input <- init <$> readFile "input/input16.txt"
  let offset = read @Int $ take 7 input
      input' = map digitToInt input
  putStrLn
    . ("day16a: " ++)
    . map intToDigit
    . take 8
    . (!! 100)
    $ iterate (step 0) input'
  putStrLn
    . ("day16b: " ++)
    . map intToDigit
    . take 8
    . (!! 100)
    . iterate fastStep
    . drop offset
    . concat
    $ replicate 10000 input'
