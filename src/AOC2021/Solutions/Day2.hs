module AOC2021.Solutions.Day2 (part1, part2) where

import AOC2021.Prelude
import Data.Map.Lazy qualified as Map
import Data.Text qualified as Text

data Dir = Forward | Down | Up deriving (Eq, Show, Read, Enum)

data Command = Command Dir Int deriving (Eq, Show)
data Pos = Pos {
  depth :: Int,
  horiz :: Int,
  aim :: Int
} deriving (Eq, Show, Ord)

calcPos :: (Pos -> Int -> (Pos, Pos, Pos)) -> String -> String
calcPos step = show . mul . foldl' go (Pos 0 0 0) . parseLines parse
  where parse (words -> [dir, n]) = Command (fromJust $ readLower dir) (read n)
        mul (Pos d h _) = d * h
        go pos (Command d n) = 
          let (fwd, down, up) = step pos n 
            in case d of
                Forward -> fwd
                Down -> down
                _ -> up

part1 :: String -> String
part1 = calcPos \(Pos d h a) n -> (Pos d (h + n) a, Pos (d + n) h a, Pos (d - n) h a)

part2 :: String -> String
part2 = calcPos \(Pos d h a) n -> (Pos (d + n * a) (h + n) a, Pos d h (a + n), Pos d h (a - n))