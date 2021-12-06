module AOC2021.Solutions.Day5 where
-- module AOC2021.Solutions.Day5 (part1, part2, parseLineRepr, testData) where

import AOC2021.Prelude
import Data.Map.Lazy qualified as Map
import Data.Text qualified as Text
import Data.Ord (comparing)
import Data.Foldable (traverse_)

data Line = Line {start :: (Int, Int), end :: (Int, Int)} deriving (Eq, Show)

parseLineRepr :: String -> Line
parseLineRepr = uncurryP Line order2 . both (get2 . map read . splitOn ",") . get2 . splitOn " -> "
  where get2 (x : y : _) = (x, y)

axisAligned :: Line -> Bool
axisAligned (Line (x1, y1) (x2, y2)) = x1 == x2 || y1 == y2

solveDay5 :: Foldable t => ([Line] -> t Line) -> String -> Int
solveDay5 f = length . Map.filter (> 1) . countElems . concatMap getPoints . f . parseLines parseLineRepr

mkRange :: (Ord a, Enum a) => a -> a -> [a]
mkRange x y = case compare x y of
  LT -> [x..y]
  EQ -> [x]
  GT -> reverse [y..x]

getPoints :: Line -> [(Int, Int)]
getPoints (Line (x1, y1) (x2, y2)) = alignWith (these (, y1) (x1, ) (,)) (mkRange x1 x2) (mkRange y1 y2)

part1 :: String -> Int
part1 = solveDay5 $ filter axisAligned

part2 :: String -> Int
part2 = solveDay5 id