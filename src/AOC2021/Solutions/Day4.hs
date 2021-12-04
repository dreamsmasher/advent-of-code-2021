{-# LANGUAGE NoOverloadedLists #-}
module AOC2021.Solutions.Day4 where

import AOC2021.Prelude
import Data.Map.Lazy qualified as Map
import Data.Text qualified as Text
import Control.Monad.ST
import Data.STRef
import Data.List.Split (splitOn, chunksOf)
import Control.Monad.Zip (MonadZip(mzip))
import Data.Ord (comparing, Down (Down))

newtype Board = Board {getBoard :: [[Cell Int]]} deriving (Eq, Show)

data Bingo = Bingo {nums :: [Int], boards :: [Board]} deriving (Eq, Show)

parseBingo :: String -> Bingo
parseBingo = lines >>> map (dropWhile (== ' ')) >>> filter (not . null) >>> \case 
  (nums : rest) -> Bingo (map read $ splitOn "," nums) boards
    where fmtLine = map (Unmarked . read @Int) . words
          boards = map Board $ chunksOf 5 $ map fmtLine rest
  _ -> Bingo [] []

data Cell a = Unmarked a | Marked a deriving (Eq, Functor, Show)

unwrapCell :: Cell p -> p
unwrapCell (Unmarked x) = x
unwrapCell (Marked x) = x

isMarked :: Cell a -> Bool
isMarked (Marked _) = True
isMarked _ = False

mark :: Int -> Board -> Board
-- mark n = Board . fmap2 (markCellIf (== n)) . getBoard
mark n = Board . fmap2 verify . getBoard
  where verify m@(Marked _) = m
        verify (Unmarked x) = bool Unmarked Marked (x == n) x

isBingo :: Board -> Bool
isBingo (Board xs) = check xs || check (transpose xs)
  where check = any (all isMarked)

data BingoRes = BingoRes {iterations :: Int, res :: Int, board :: Board} deriving (Eq, Show)

getAllResults :: Bingo -> [BingoRes] 
getAllResults (Bingo nums boards) = mapMaybe (getRes nums) boards
  where 
    getRes :: [Int] -> Board -> Maybe BingoRes
    getRes nums board = runST do
      ref <- newSTRef board
      ct <- newSTRef 1
      let go = foldWhileM \n -> do
            cur <- readSTRef ref
            let marked = mark n cur
            if isBingo marked 
              then pure $ Just (n, marked) 
              else writeSTRef ref marked >> modifySTRef' ct succ $> Nothing
      res <- go nums
      iters <- readSTRef ct
      pure $ uncurry (BingoRes iters) <$> res

solveBingo :: BingoRes -> Int
solveBingo (BingoRes _ res (Board xs)) = res * (sum . map unwrapCell . filter (not . isMarked) $ concat xs)

solveDay4 :: Ord a => (Int -> a) -> String -> Int
solveDay4 f = solveBingo . minimumBy (comparing (f . iterations)) . getAllResults . parseBingo

part1 :: String -> Int
part1 = solveDay4 id

part2 :: String -> Int
part2 = solveDay4 Down
