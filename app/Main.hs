{-# LANGUAGE NoOverloadedLists #-}
module Main where

import System.Environment
import AOC2021.Prelude
import AOC2021.Config (getOpts)
import Text.Printf
import Data.Text qualified as Text
import Advent 
import AOC2021.Runner (resolveSolutionsFor)
import Data.Foldable (traverse_)

orExit :: MonadFail m => m (Maybe a) -> String -> m a
orExit ma msg = ma >>= maybe (fail msg) pure

unwrapE :: (MonadFail m, Show a) => m (Either a b) -> m b
unwrapE = (>>= either (fail . show) pure)

toPart :: Integral n => n -> Part
toPart 1 = Part1
toPart _ = Part2

runAoC' :: AoCOpts -> AoC a -> ExceptT ClientError IO a
runAoC' = (ExceptT . fmap (first AoCErr)) .: runAoC

data ClientError = AoCErr AoCError | ClientErr String deriving (Show)

confirmOrBreak :: (MonadError ClientError m, MonadIO m) => String -> m ()
confirmOrBreak msg = do
  let msg' = msg <> " [Y,n]"
  input <- liftIO (putStrLn msg' *> getLine)
  when (input == "n") . throwError $ ClientErr "User abort"

main :: IO ()
main = do
  args <- getArgs
  (day, part) <- case traverse (readMaybe @Integer) args of
    Just [day, part] -> pure (mkDay day, toPart part)
    _ -> fail "Could not parse arguments. USAGE: advent2021 [day] [part]"
  day <- pure day `orExit` "Day needs to be within 1 <= d <= 25"
  aocOpts <- getOpts `orExit` "Could not initialize configuration for advent of code session, exiting!" 

  let (solve1, solve2) = resolveSolutionsFor . fromIntegral $ dayInt day
      solve = case part of
        Part1 -> solve1
        Part2 -> solve2
      log :: MonadIO m => String -> m ()
      log = liftIO . putStrLn

  aocRes <- runExceptT do
    input <- fmap Text.unpack . runAoC' aocOpts $ AoCInput day
    log $ printf "Running day %d part %d..." (dayInt day) (partInt part)
    let !soln = solve input
    log $ "\nResult: " <> soln <> "\n"
    confirmOrBreak "Submit?"
    (_, result) <- runAoC' aocOpts $ AoCSubmit day part (solve input)
    log $ fmtResult result

  case aocRes of
    Left (AoCErr error) -> 
      traverse_ log ["Received server error:", show error]
    _ -> pure ()

fmtResult :: SubmitRes -> String
fmtResult = 
  let fmt = printf @(String -> String) "-- %s --" . capitalize
      [right, wrong, error] = map fmt ["correct", "incorrect", "error"]
   in unlines . \case
        SubCorrect mbRank -> [right, maybe "" (printf "Congrats! You ranked #%d globally!") mbRank]
        SubIncorrect delay errMsg -> [wrong, errMsg ?? "", printf "You can retry in %d seconds" delay]
        SubWait delay -> [wrong, printf "Tried to resubmit too soon! Try again in %d seconds." delay]
        SubInvalid -> [wrong, "Invalid submission. Maybe you submitted to a part you've already solved or haven't unlocked?"]
        SubUnknown err -> [wrong, "Error parsing server response, received err:"] <> map ("  " <>) (lines err)

displayPretty :: Int -> String -> IO ()
displayPretty part res = putStrLn $ "--- PART " <> show part <> " ---\n" <> res