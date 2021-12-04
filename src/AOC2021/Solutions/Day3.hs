module AOC2021.Solutions.Day3 (part1, part2) where

import AOC2021.Prelude
import Data.Map.Lazy qualified as Map

part1 :: String -> Int
part1 = ((*) `uncurryOn` fromBits)
      . unzip 
      . map (dupWith not . ((<) `uncurryOn` length) . partition id) 
      . transpose 
      . parseLines (map (== '1'))

fromBits :: Enum a => [a] -> Int
fromBits = foldl' (\a x -> a `shiftL` 1 .|. fromEnum x) 0

unconsAll :: [[a]] -> ([a], [[a]])
unconsAll xs = (map head xs, map tail' xs)

findRating :: (Ordering -> Bool) -> [([Bool], [Bool])] -> Int
findRating cmp = fromBits 
               . fst 
               . Map.findMin 
               . coalesce mempty
               . find ((== 1) . length) 
               . iterate go' 
               . Map.fromList
  where go' st = 
          let (ones, zeros) = Map.partition head $ Map.filter (not . null) st
              target = cmp $ (compare `on` length) zeros ones
            in fmap tail' $ if target then ones else zeros

part2 :: String -> Int
part2 = (liftA2 (*) `on` (findRating . ($ GT))) (==) (/=) . map dup . parseLines (map (== '1'))