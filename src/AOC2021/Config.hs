module AOC2021.Config where

import AOC2021.Prelude
import Advent
import Control.Exception
import System.Environment (lookupEnv)

readFileSafe :: FilePath -> IO (Maybe String)
readFileSafe path = (Just <$> readFile path) `catch` \(e :: IOException) -> print e $> Nothing

-- definitely not best practices
aocSessionPath :: FilePath
aocSessionPath = "session_key.txt"

aocYear :: Integer
aocYear = 2021

aocCachePath :: FilePath
aocCachePath = "aoc_cache"

getOpts :: IO (Maybe AoCOpts)
getOpts = runMaybeT do
  let fromEnv = lookupEnv "AOC_COOKIE"
      fromFile = readFileSafe aocSessionPath
      orM = (<|>) `on` MaybeT
  key <- fromEnv `orM` fromFile
  let opts = (defaultAoCOpts aocYear key) {_aCache = Just aocCachePath}
  pure opts
