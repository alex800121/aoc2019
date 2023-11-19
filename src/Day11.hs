{-# LANGUAGE LambdaCase #-}

module Day11 where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import MyLib (Direction (..), drawGraph)
import qualified Data.DList as DL
import OpCode
import Data.Functor.Identity (Identity(..))

type Floor = Map Index Integer

type Index = (Int, Int)

data GameState = G {_floor :: Floor, _oc :: UBOC, _pos :: Index, _dir :: Direction} deriving (Show, Eq)

run :: GameState -> GameState
run g@(G f o p@(x, y) d)
  | runIdentity $ _halt o = g
  | otherwise = run g'
  where
    g' = G f' o' p' d'
    input = fromIntegral $ fromMaybe 0 (f Map.!? p)
    o' = runOpCodeWith runSTOC (o {_input = Identity $ DL.singleton input})
    turn : paint : _ = reverse $ DL.toList $ runIdentity $ _output o'
    f' = Map.insert p paint f
    d' = (if turn == 0 then pred else succ) d
    p' = bimap (+ x) (+ y) $ case d' of
      North -> (0, -1)
      East -> (1, 0)
      South -> (0, 1)
      West -> (-1, 0)

day11 :: IO ()
day11 = do
  oc <- readInput <$> readFile "input/input11.txt"
  putStrLn
    . ("day11a: " ++)
    . show
    . length
    . _floor
    . run
    $ G Map.empty oc (0, 0) North
  putStrLn
    . ("day11b: \n" ++)
    . unlines
    . drawGraph
      ( \case
          Just 1 -> '#'
          _ -> ' '
      )
    . _floor
    . run
    $ G (Map.singleton (0, 0) 1) oc (0, 0) North
