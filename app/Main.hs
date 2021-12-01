module Main where

import AOC2021.Solutions.Day1 qualified as Day1
import AOC2021.Prelude

main :: IO ()
main = do
  body <- readFile "aoc_cache/1.txt"
  let p1 =  Day1.part1 body
  let p2 =  Day1.part2 body
  putStrLn p1
  putStrLn p2

