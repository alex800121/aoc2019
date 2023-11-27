module Day23 where

import qualified Data.DList as DL
import Data.Functor.Identity (Identity (..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List.Split (chunksOf)
import OpCode

type GameState = IntMap UBOC

collectOutput :: GameState -> (IntMap [Integer], GameState)
collectOutput g = (outputs, g')
  where
    outputs =
      foldr
        ( \[i, x, y] ->
            IM.insertWith (++) (fromIntegral i) [x, y]
        )
        IM.empty
        . chunksOf 3
        . concatMap (DL.toList . runIdentity . _output)
        $ IM.elems g
    g' = IM.map (\x -> x {_output = Identity DL.empty}) g

updateInput :: IntMap [Integer] -> GameState -> GameState
updateInput outputs =
  IM.mapWithKey
    (\k a ->
        undefined
    )

day23a :: GameState -> [Integer]
day23a g = case outputs IM.!? 255 of
  Nothing -> day23a g''
  Just x -> x
  where
    (outputs, g') = collectOutput g
    g'' = undefined

day23 :: IO ()
day23 = do
  oc <- readInput <$> readFile "input/input23.txt"
  let initState = IM.fromList $ map (\x -> (x, inputOpCode [fromIntegral x] oc)) [0 .. 49]
  print oc
