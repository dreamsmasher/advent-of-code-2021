module AOC2021.Solutions.Day1 (part1, part2) where

import AOC2021.Prelude

countIncr :: [Int] -> Int
countIncr = length . filter id . (zipWith (<) <*> tail)

part1 :: String -> String
part1 = solveLines countIncr

part2 :: String -> String
part2 = solveLines $ countIncr . mapMaybe threes . tails
  where threes = \case
          (a : b : c : _) -> Just $ a + b + c
          _ -> Nothing