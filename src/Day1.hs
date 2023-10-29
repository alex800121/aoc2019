module Day1 where

fuel :: Int -> Int
fuel = subtract 2 . (`div` 3)
day1 :: IO ()
day1 = do
  input <- map (read @Int) . lines <$> readFile "input/input1.txt"
  putStrLn
    . ("day1a: " ++)
    . show
    . sum
    . map fuel
    $ input
  putStrLn
    . ("day1a: " ++)
    . show
    . sum
    . map (sum . tail . takeWhile (> 0) . iterate fuel)
    $ input
