module Day25 where


import Paths_AOC2019
import Control.Monad (forM_)

import Control.Monad.ST (RealWorld, ST, stToIO)

import Data.Char (chr, ord)

import qualified Data.DList as DL

import Data.Functor.Identity (Identity (..))

import Data.List (find, intercalate, isInfixOf, subsequences)

import Data.Map (Map)

import qualified Data.Map as Map

import Data.Maybe (fromJust)

import Data.STRef

import MyLib (drawASCII)

import OpCode

dropped = subsequences items

items =
  [ "manifold",
    "whirled peas",
    "space heater",
    "dark matter",
    "antenna",
    "bowl of rice",
    "klein bottle",
    "spool of cat6"
  ]

command =
  [ "east",
    "take manifold",
    "south",
    "take whirled peas",
    "north",
    "west",
    "south",
    "take space heater",
    "south",
    "take dark matter",
    "north",
    "east",
    "north",
    "west",
    "south",
    "take antenna",
    "north",
    "east",
    "south",
    "east",
    "take bowl of rice",
    "north",
    "take klein bottle",
    "north",
    "take spool of cat6",
    "west"
  ]

initGame :: STOC s -> ST s ()
initGame oc = do
  runSTOC oc
  forM_
    command
    ( \s -> do
        let input = DL.fromList . (<> [10]) . map (fromIntegral . ord) $ s
        writeSTRef (_input oc) input
        writeSTRef (_output oc) DL.empty
        runSTOC oc
    )

playGame :: STOC RealWorld -> IO ()
playGame oc = do
  runIOOC oc
  x <- stToIO $ readSTRef (_output oc)
  putStrLn $ map (chr . fromIntegral) $ DL.toList x
  input <- DL.fromList . (<> [10]) . map (fromIntegral . ord) <$> getLine
  stToIO $ writeSTRef (_input oc) input
  stToIO $ writeSTRef (_output oc) DL.empty
  playGame oc

dropItemOutput :: UBOC -> [String] -> String
dropItemOutput g item = drawASCII . DL.toList . runIdentity . _output $ runOpCodeWith runSTOC g''
  where
    input = DL.fromList . map (fromIntegral . ord) $ intercalate "\n" (map ("drop " ++) item) ++ "\n"
    input2 = DL.fromList . map (fromIntegral . ord) $ "north\n"
    g' = runOpCodeWith runSTOC $ g {_input = Identity input}
    g'' = g' {_input = Identity input2, _output = Identity DL.empty}

day25 :: IO ()
day25 = do
  input <- readInput <$> (getDataDir >>= readFile . (++ "/input/input25.txt"))
  input' <- stToIO $ ubToST input
  stToIO $ initGame input'
  g <- stToIO $ stToUB input'
  putStrLn
    . ("day25a: " ++)
    . fromJust
    . find ("complete" `isInfixOf`)
    . Map.keys
    $ foldr (\x acc -> Map.insertWith (<>) (dropItemOutput g x) [x] acc) Map.empty dropped
