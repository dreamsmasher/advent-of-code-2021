module AOC2021.Solutions.Day6 where

import AOC2021.Prelude
import Data.Map.Lazy qualified as Map
import Data.Text qualified as Text

stepFish :: Map.Map Int Int -> Map.Map Int Int
stepFish fishes = 
  let (zeros, rest) = Map.alterF (\z -> (z ?? 0, Nothing)) 0 fishes
  in Map.insert 8 zeros . Map.insertWith (+) 6 zeros $ Map.mapKeys pred rest

solveDay6 :: Int -> String -> Int
solveDay6 n = sum . Map.elems . iterateTimes n stepFish . countElems . map read . splitOn ","

part1 :: String -> Int
part1 = solveDay6 80

part2 :: String -> Int
part2 = solveDay6 256
