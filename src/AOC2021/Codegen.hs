{-# LANGUAGE TemplateHaskell, NoOverloadedLists #-}
module AOC2021.Codegen (genGetters) where

import AOC2021.Prelude
import Language.Haskell.TH

intToPat :: Int -> PatQ
intToPat = litP . integerL . fromIntegral

getPart :: Int -> String -> Q Name
getPart part modName = 
  let path = modName <> ".part" <> show part
  in lookupValueName path >>= maybe (fail $ "Could not resolve " <> path) pure

getParts :: Int -> ExpQ
getParts day = 
  let modName = "AOC2021.Solutions.Day" <> show day
      toExp part = 
        VarE <$> getPart part modName
   in tupE $ map toExp [1,2]

genGetters :: ExpQ
genGetters = 
  let days = [1..25]
      patterns = days <&> intToPat
      bodies = days <&> getParts
      clauses = zipWith (\pat body -> match pat (normalB body) []) patterns bodies
   in lamCaseE clauses