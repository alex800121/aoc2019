module Day14 where

import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Merge.Strict
import Data.Maybe (fromMaybe)
import Debug.Trace
import Paths_AOC2019

type Reaction = Map String (Int, [(String, Int)])

type Remains = Map String Int

type Target = Map String Int

-- 1 TMTNM, 5 WMZD => 4 JVBK
inputParser :: String -> Reaction
inputParser s = Map.singleton a (b, ing)
  where
    ings : pro : _ = splitOn " => " s
    ing = map f $ splitOn ", " ings
    f i = let x : y : _ = words i in (y, read x)
    (a, b) = f pro

buildTarget :: Reaction -> String -> Remains -> Remains
buildTarget reaction goal remains
  | Map.foldlWithKey' (\acc k a -> (k == goal || a <= 0) && acc) True remains = remains
  | otherwise = buildTarget reaction goal require
  where
    require =
      Map.foldlWithKey'
        ( \acc k a' ->
            let a = a' + fromMaybe 0 (acc Map.!? k)
             in case reaction Map.!? k of
                  Just (i, b)
                    | a > 0 ->
                        let (x', y) = negate a `divMod` i
                            x = negate x'
                         in foldl'
                              ( \acc' (x'', a') ->
                                  Map.insertWith
                                    (+)
                                    x''
                                    (x * a')
                                    acc'
                              )
                              (Map.insert k (negate y) acc)
                              b
                  _ -> Map.insert k a acc
        )
        Map.empty
        remains

binSearch :: Reaction -> Int -> Int -> Int -> Int
binSearch reaction target l u
  -- | traceShow (l, u) False = undefined
  | l == u - 1 && l' < target && target < u' = l
  | u' < target = binSearch reaction target u (d * 2 + u)
  | u' == target = u
  | m' < target && target < u' = binSearch reaction target m u
  | m' == target = m
  | l' < target && target < m' = binSearch reaction target l m
  | target == l' = l
  | target < l' = binSearch reaction target (l - 2 * d) l
  where
    f i = buildTarget reaction "ORE" (Map.singleton "FUEL" i) Map.! "ORE"
    m = (l + u) `div` 2
    d = u - l
    u' = f u
    l' = f l
    m' = f m

day14 :: IO ()
day14 = do
  reaction <- Map.unions . map inputParser . lines <$> (getDataDir >>= readFile . (++ "/input/input14.txt"))
  let remains = Map.singleton "FUEL"
      goal = "ORE"
  putStrLn
    . ("day14a: " ++)
    . show
    . (Map.! "ORE")
    $ buildTarget reaction goal (remains 1)
  putStrLn
    . ("day14b: " ++)
    . show
    $ binSearch reaction 1000000000000 1 256
