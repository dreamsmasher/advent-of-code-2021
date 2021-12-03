module Main where

import System.Environment
import AOC2021.Prelude
import AOC2021.Config (getOpts)
import Data.Text qualified as Text
import Advent 
import AOC2021.Runner (resolveSolutionsFor)

orExit :: MonadFail m => m (Maybe a) -> String -> m a
orExit ma msg = ma >>= maybe (fail msg) pure

unwrapE :: (MonadFail m, Show a) => m (Either a b) -> m b
unwrapE = (>>= either (fail . show) pure)

toPart :: Integral n => n -> Part
toPart 1 = Part1
toPart _ = Part2

main :: IO ()
main = do
  args <- getArgs
  (day, part) <- case traverse (readMaybe @Integer) args of
    Just [day, part] -> pure (mkDay day, toPart part)
    _ -> fail "Could not parse arguments. USAGE: advent2021 [day] [part]"
  day <- pure day `orExit` "Day needs to be within 1 <= d <= 25"
  aocOpts <- getOpts `orExit` "Could not initialize configuration for advent of code session, exiting!" 
  input <- fmap Text.unpack . unwrapE . runAoC aocOpts $ AoCInput day

  let (part1, part2) = resolveSolutionsFor . fromIntegral $ dayInt day
      part1Sol = part1 input
      part2Sol = part2 input
  displayPretty 1 part1Sol
  displayPretty 2 part2Sol
  
  pure ()

displayPretty :: Int -> String -> IO ()
displayPretty part res = putStrLn $ "--- PART " <> show part <> " ---\n" <> res