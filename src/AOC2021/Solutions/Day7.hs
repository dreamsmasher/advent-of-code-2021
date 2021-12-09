module AOC2021.Solutions.Day7 (part1, part2) where

import AOC2021.Prelude
import Data.Map.Lazy qualified as Map

solveDay7 :: (Int -> Int -> Int) -> String -> Int
solveDay7 f (parseCommas -> xs) =
    let counts = countElems xs
        getDist n = Map.foldlWithKey' (\a k v -> a + abs (f k n) * v) 0 counts
     in minimum $ getDist <$> [0..maximum xs]

part1 :: String -> Int
part1 = solveDay7 subtract

part2 :: String -> Int
part2 = solveDay7 countDistance
  where sumTo n = (n * (n + 1)) `shiftR` 1 -- thank you based Gauss
        countDistance x y = sumTo $ abs (x - y)