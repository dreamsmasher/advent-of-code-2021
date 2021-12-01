{-# LANGUAGE TemplateHaskell #-}
module AOC2021.Runner where

import AOC2021.Prelude
import AOC2021.Solutions.Day1  qualified 
import AOC2021.Solutions.Day2  qualified
import AOC2021.Solutions.Day3  qualified
import AOC2021.Solutions.Day4  qualified
import AOC2021.Solutions.Day5  qualified
import AOC2021.Solutions.Day6  qualified
import AOC2021.Solutions.Day7  qualified
import AOC2021.Solutions.Day8  qualified
import AOC2021.Solutions.Day9  qualified
import AOC2021.Solutions.Day10 qualified
import AOC2021.Solutions.Day11 qualified
import AOC2021.Solutions.Day12 qualified
import AOC2021.Solutions.Day13 qualified
import AOC2021.Solutions.Day14 qualified
import AOC2021.Solutions.Day15 qualified
import AOC2021.Solutions.Day16 qualified
import AOC2021.Solutions.Day17 qualified
import AOC2021.Solutions.Day18 qualified
import AOC2021.Solutions.Day19 qualified
import AOC2021.Solutions.Day20 qualified
import AOC2021.Solutions.Day21 qualified
import AOC2021.Solutions.Day22 qualified
import AOC2021.Solutions.Day23 qualified
import AOC2021.Solutions.Day24 qualified
import AOC2021.Solutions.Day25 qualified

import AOC2021.Codegen

resolveSolutionsFor :: Int -> (String, String)
-- \case {1 -> (Day1.part1, Day1.part2); ...}
resolveSolutionsFor = $(genGetter)