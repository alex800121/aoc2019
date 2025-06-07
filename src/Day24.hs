module Day24 where

import Data.Array.IArray qualified as IA
import Data.Array.Unboxed (Array, UArray)
import Data.Word (Word32)
import Data.Bifunctor (Bifunctor (..))
import Data.List (foldl')
import MyLib (drawArray, firstRepeat')
import Paths_AOC2019

type Index = (Int, Int)

type IndexPlus = (Int, Index)

type AArray a = Array a [a]

type Bugs a = UArray a Bool

r = 200

(a, b) +^ (c, d) = (a + c, b + d)

adjacent = [(-1, 0), (0, -1), (0, 1), (1, 0)]

adjArr :: AArray Index =
  IA.accumArray
    (flip (:))
    []
    b
    [(i, j) | i <- IA.range b, a <- adjacent, let j = i +^ a, IA.inRange b j]
  where
    b = ((-2, -2), (2, 2))

adjArrPlus :: AArray IndexPlus =
  IA.accumArray
    (flip (:))
    []
    bPlus
    ( [ (i, j)
        | i <- IA.range bPlus,
          j <- adj i,
          IA.inRange bPlus j
      ]
    )
  where
    b = ((-2, -2), (2, 2))
    r2 = r `div` 2
    bPlus = bimap (-r2,) (r2,) b

adj (l, (0, 0)) = []
adj (l, i@(0, -1)) = [(l, i +^ j) | j <- adjacent, i +^ j /= (0, 0)] <> [(l + 1, (x', -2)) | x' <- [-2 .. 2]]
adj (l, i@(0, 1)) = [(l, i +^ j) | j <- adjacent, i +^ j /= (0, 0)] <> [(l + 1, (x', 2)) | x' <- [-2 .. 2]]
adj (l, i@(-1, 0)) = [(l, i +^ j) | j <- adjacent, i +^ j /= (0, 0)] <> [(l + 1, (-2, x')) | x' <- [-2 .. 2]]
adj (l, i@(1, 0)) = [(l, i +^ j) | j <- adjacent, i +^ j /= (0, 0)] <> [(l + 1, (2, x')) | x' <- [-2 .. 2]]
adj (l, i@(x, y)) =
  [(l, i +^ j) | j <- adjacent, i +^ j /= (0, 0)]
    <> [(l - 1, (0, y')) | y' <- f y]
    <> [(l - 1, (x', 0)) | x' <- f x]
  where
    f (-2) = [-1]
    f (2) = [1]
    f _ = []

step :: (IA.Ix a) => AArray a -> Bugs a -> Bugs a
step ref bugs =
  IA.array
    b
    [ (i, x')
      | (i, x) <- IA.assocs bugs,
        let l = length (filter (bugs IA.!) (ref IA.! i)),
        let x' = l == 1 || l == 2 && not x
    ]
  where
    b = IA.bounds bugs

bioRating :: Bugs Index -> Int
bioRating b =
  snd $
    foldl'
      (\(n, acc) i -> if b IA.! i then (2 * n, acc + n) else (2 * n, acc))
      (1, 0)
      [(x, y) | y <- [-2 .. 2], x <- [-2 .. 2]]

detectCycle xs = go 0 1 xs xs
  where
    go a b (x : xs) (_ : y : ys)
      | x == y = Just (a, b - a, x)
      | otherwise = go (a + 1) (b + 2) xs ys
    go _ _ _ _ = Nothing

plus input = IA.accumArray (||) False bPlus [((0, i), x) | (i, x) <- IA.assocs input]
  where
    r2 = r `div` 2
    bPlus = bimap (-r2,) (r2,) (IA.bounds input)

day24 :: IO ()
day24 = do
  input <-
    IA.ixmap ((-2, -2), (2, 2)) (\(a, b) -> (a + 2, b + 2))
      . IA.amap (== '#')
      . drawArray @UArray
      . lines
      <$> (getDataDir >>= readFile . (++ "/input/input24.txt"))
  putStrLn
    . ("day24a: " ++)
    . show
    . fmap (bioRating . snd)
    . firstRepeat'
    $ iterate (step adjArr) input
  putStrLn
    . ("day24a: " ++)
    . show
    . length
    . filter snd
    . IA.assocs
    $ iterate (step adjArrPlus) (plus input) !! 200
