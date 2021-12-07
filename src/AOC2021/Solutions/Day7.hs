module AOC2021.Solutions.Day7 (part1, part2) where

import AOC2021.Prelude
import Data.Map.Lazy qualified as Map
import Data.Text qualified as Text

solveDay7 :: (Int -> Int -> Int) -> String -> Int
solveDay7 f = minimum . getDists . parseCommas
  where getDists xs =
          let counts = countElems xs
              lim = maximum xs
              getDist n = sum $ Map.mapWithKey (\k v -> abs (f k n) * v) counts
           in map getDist [0..lim] 

part1 :: String -> Int
part1 = solveDay7 subtract

part2 :: String -> Int
part2 = solveDay7 countDistance
  where sumTo n = (n * (n + 1)) `shiftR` 1 -- thank you based Gauss
        countDistance x y = sumTo $ abs (x - y)