module Main where

import AOC2021.Solutions.Day2 qualified as Day2
import AOC2021.Prelude

main :: IO ()
main = do
  body <- readFile "aoc_cache/2.txt"
  let p1 =  Day2.part1 body
  let p2 =  Day2.part2 body
  putStrLn p1
  putStrLn p2

