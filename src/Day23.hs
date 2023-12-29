{-# LANGUAGE LambdaCase #-}

module Day23 where

import qualified Data.DList as DL
import Data.Functor.Identity (Identity (..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List.Split (chunksOf)
import Debug.Trace (traceShow)
import MyLib ((!?))
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

updateInput :: [Integer] -> IntMap [Integer] -> GameState -> GameState
updateInput def outputs =
  IM.mapWithKey
    ( \k a -> case outputs IM.!? k of
        Just l -> a {_input = (`DL.append` DL.fromList l) <$> _input a}
        -- _ -> a
        _ ->
          a
            { _input =
                ( \case
                    DL.Nil -> DL.fromList def
                    _ -> runIdentity (_input a)
                )
                  <$> _input a
            }
    )

day23a :: GameState -> [Integer]
-- day23a g | traceShow (IM.filter (/= Identity DL.empty) $ IM.map _output g) False = undefined
day23a g = case outputs IM.!? 255 of
  Nothing -> day23a g''
  Just x -> x
  where
    (outputs, g') = collectOutput g
    g'' = IM.map (runOpCodeWith runSTOC) $ updateInput [-1] outputs g'

day23b :: GameState -> [Integer] -> [Integer]
day23b g nat
  -- \| traceShow (nat') False = undefined
  | IM.null outputs && g'' == g = day23b (updateInput [] (IM.singleton 0 nat) g) nat
  | IM.null outputs = day23b g'' nat
  | otherwise = case nat' of
      Nothing -> day23b g'' nat
      Just x | x !? 1 == nat !? 1 -> x
      Just x -> day23b g'' x
  where
    nat' = reverse . take 2 . reverse <$> outputs IM.!? 255
    (outputs, g') = collectOutput g
    g'' = IM.map (runOpCodeWith runSTOC) $ updateInput [-1] outputs g'

day23 :: IO ()
day23 = do
  oc <- readInput <$> readFile "input/input23.txt"
  let initState = IM.fromList $ map (\x -> (x, inputOpCode [fromIntegral x] oc)) [0 .. 49]
  putStrLn
    . ("day23a: " ++)
    . show
    . (!! 1)
    $ day23a initState
  putStrLn
    . ("day23b: " ++)
    . show
    . (!! 1)
    $ day23b (IM.map (runOpCodeWith runSTOC) initState) []

-- print $ IM.filter (/= Identity DL.empty) $ IM.map (_output . runOpCodeWith runSTOC) initState
