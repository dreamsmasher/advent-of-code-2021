module AOC2021.Solutions.Day9 (part1, part2) where

import AOC2021.Prelude
import Data.Map.Lazy qualified as Map
import Data.Map.Lazy (Map, (!?))
import Data.Text qualified as Text
import Data.Set qualified as Set
import Control.Monad.ST 
import Data.STRef

newtype Grid = Grid {getGrid :: Map (Int, Int) Int} deriving (Eq, Ord, Show)

mkGrid :: String -> Grid
mkGrid = Grid . Map.fromList . concat . zipWith combine [0..] . parseLines (zip [0..] . map digitToInt)
  where combine y = map (\(x, v) -> ((y, x), v))

vecs :: [(Int, Int)]
vecs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

offset :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
offset (y, x) (a, b) = (y + a, x + b)

lookAround :: Grid -> (Int, Int) -> Maybe Int
lookAround (Grid g) p = do
  let dirs = (0, 0) : vecs
      (mbVal : rest) = map ((g !?) . offset p) dirs
  val <- mbVal
  all (maybe True (> val)) rest `thenJust` (val + 1)

part1 :: String -> Int
part1 = sum . liftA2 mapMaybe lookAround (Map.keys . getGrid) . mkGrid

markPeaks :: String -> Grid
markPeaks (mkGrid -> Grid g) = Grid $ Map.filter (/= 9) g

readWith :: STRef s a -> (a -> b) -> ST s b
readWith ref f = f <$> readSTRef ref

infixr 7 `readWith`

part2 :: String -> Int
part2 (markPeaks -> g@(Grid grid)) = runST do

  gridRef <- newSTRef grid

  let go p = (?? 0) <$> runMaybeT do
        -- break if key doesn't exist
        _ <- MaybeT $ gridRef `alterRef` Map.alterF (, Nothing) p
        lift $ succ . sum <$> traverse (go . offset p) vecs
  
  basins <- newSTRef mempty

  Map.keys grid ~>> \p -> do
    exists <- gridRef `readWith` Map.member p
    when exists do
      size <- go p
      modifySTRef' basins (Set.insert size)

  basins `readWith` product . take 3 . Set.toDescList
