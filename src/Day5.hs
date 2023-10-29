module Day5 where
import Data.List.Split (splitOn)
import OpCode (runSTOC, PrimOC (..), runOpCodeWith)
import Data.Vector.Unboxed (fromList)

day5 :: IO ()
day5 = do
  input <- map (read @Int) . splitOn "," <$> readFile "input/input5.txt"
  let day5a = runOpCodeWith runSTOC $ PrimOC 0 (fromList input) [1] []
      day5b = runOpCodeWith runSTOC $ PrimOC 0 (fromList input) [5] []
  putStrLn
    . ("day5a: " ++)
    . show
    . head
    . _output
    . snd
    $ day5a
  putStrLn
    . ("day5b: " ++)
    . show
    . head
    . _output
    . snd
    $ day5b
