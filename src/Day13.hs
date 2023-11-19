{-# LANGUAGE LambdaCase #-}

module Day13 where

import Control.Monad.ST (ST, runST)
import qualified Data.IntMap as IM
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Debug.Trace (traceM)
import MyLib
import OpCode
import qualified Data.DList as DL
import Data.Functor.Identity (Identity(..))

data Tile
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  deriving (Show, Eq, Ord, Enum)

type Index = (Int, Int)

type Cabinet = Map Index Tile

drawGraph' =
  unlines
    . drawGraph
      ( \case
          Just Wall -> '#'
          Just Ball -> 'o'
          Just Block -> '%'
          Just Paddle -> '_'
          _ -> ' '
      )

draw :: DL.DList Integer -> (Cabinet, [Int])
draw = f . DL.map fromIntegral
  where
    f DL.Nil = (Map.empty, [])
    f (DL.Cons (-1) (0 : z : xs)) = fmap (z :) (f $ DL.fromList xs)
    f (DL.Cons x (y : z : xs)) = mapFirst (Map.insertWith (\a b -> b) (x, y) $ toEnum $ fromIntegral z) (f $ DL.fromList xs)

loopSTOC :: Cabinet -> STOC s -> ST s Int
loopSTOC cache oc = do
  initOC <- stToUB oc
  runSTOC oc
  (newCab, score) <- draw <$> readSTRef (_output oc)
  let cache' = Map.union newCab cache
      (xBall, yBall) = fst $ Map.findMin $ Map.filter (== Ball) cache'
      (xPaddle, yPaddle) = fst $ Map.findMin $ Map.filter (== Paddle) cache'
      oc'' = case compare xBall xPaddle of
        EQ -> initOC {_input = Identity $ DL.singleton 0, _output = Identity DL.empty}
        LT -> initOC {_input = Identity $ DL.singleton (-1), _output = Identity DL.empty}
        GT -> initOC {_input = Identity $ DL.singleton 1, _output = Identity DL.empty} 
  if Map.null (Map.filter (== Block) cache')
    then return $ last score
    else do
      oc <- ubToST oc''
      runSTOC oc
      (newCab', _) <- draw <$> readSTRef (_output oc)
      -- traceM (drawGraph' newCab)
      loopSTOC (Map.union newCab' cache) oc

day13 :: IO ()
day13 = do
  oc <- readInput <$> readFile "input/input13.txt"
  let output :: Cabinet
      output =
        Map.fromList
          . map ((\[x, y, z] -> ((x, y), toEnum z)) . map fromIntegral)
          . chunksOf 3
          . DL.toList
          . runIdentity
          . _output
          $ runOpCodeWith runSTOC oc
      ocStart = oc {_vector = Identity $ IM.insert 0 2 $ runIdentity $ _vector oc}
  putStrLn
    . ("day13a: " ++)
    . show
    . length
    $ Map.filter (== Block) output
  putStrLn
    . ("day13b: " ++)
    . show
    $ runST
    $ do
      oc <- ubToST ocStart
      loopSTOC Map.empty oc
