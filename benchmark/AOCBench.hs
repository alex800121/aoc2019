module Main (main) where

import Criterion.Main
import Criterion.Types (Config (..), Verbosity (Quiet))
import Day1
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day2
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import MyLib
import Solve (solveAll)

main :: IO ()
main =
  defaultMainWith
    (defaultConfig{verbosity = Quiet, reportFile = Just "./bench.html"})
    [ bench "main" (nfIO solveAll)
    , bgroup
        "Day"
        [ bench "day1" (nfIO (day1 >>= print))
        , bench "day2" (nfIO (day2 >>= print))
        , bench "day3" (nfIO (day3 >>= print))
        , bench "day4" (nfIO (day4 >>= print))
        , bench "day5" (nfIO (day5 >>= print))
        , bench "day6" (nfIO (day6 >>= print))
        , bench "day7" (nfIO (day7 >>= print))
        , bench "day8" (nfIO (day8 >>= print))
        , bench "day9" (nfIO (day9 >>= print))
        , bench "day10" (nfIO (day10 >>= print))
        , bench "day11" (nfIO (day11 >>= print))
        , bench "day12" (nfIO (day12 >>= print))
        , bench "day13" (nfIO (day13 >>= print))
        , bench "day14" (nfIO (day14 >>= print))
        , bench "day15" (nfIO (day15 >>= print))
        , bench "day16" (nfIO (day16 >>= print))
        , bench "day17" (nfIO (day17 >>= print))
        , bench "day18" (nfIO (day18 >>= print))
        , bench "day19" (nfIO (day19 >>= print))
        , bench "day20" (nfIO (day20 >>= print))
        , bench "day21" (nfIO (day21 >>= print))
        , bench "day22" (nfIO (day22 >>= print))
        , bench "day23" (nfIO (day23 >>= print))
        , bench "day24" (nfIO (day24 >>= print))
        , bench "day25" (nfIO (day25 >>= print))
        ]
    ]
