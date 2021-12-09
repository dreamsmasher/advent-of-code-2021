{-# LANGUAGE PolyKinds, UndecidableInstances #-}
module AOC2021.Solutions.Day6.Sol where

-- import AOC2021.Prelude

-- import GHC.TypeNats
-- import GHC.TypeLits
-- import Data.Data (Proxy(Proxy))

-- data Ocean 
--   (zero :: Nat)
--   (one :: Nat)
--   (two :: Nat)
--   (three :: Nat)
--   (four :: Nat)
--   (five :: Nat)
--   (six :: Nat)
--   (seven :: Nat)
--   (eight :: Nat)
--   = Ocean

-- data Step (o :: k) where
--   Step :: Step (Ocean zero one two three four five six seven eight) -> Step (Ocean one two three four five six (seven + zero) eight zero)
--   Lift :: o -> Step o
--   Solve :: Step (Ocean a b c d e f g h i) -> Step (a + b + c + d + e + f + g + h + i)

-- class StepN (n :: Nat) where
--   data AtStepN n o
--   stepN :: proxy n -> Ocean a b c d e f g h i  -> AtStepN n o

-- data Nat' = Z | S Nat' deriving (Eq, Show)

-- -- type family ToNat' (n :: Nat) :: Nat' where
-- --   ToNat' 0 = Z
-- --   ToNat' n = S (ToNat' (n - 1))

-- s = Ocean @0 @1 @1 @2 @1 @0 @0 @0 @0
-- lifted = Lift s
-- stepped = Step lifted
-- s2 = Step stepped

-- class RunStep (n :: Nat') (o :: k) where
--   type Stepped n o :: k
--   runStep :: proxy n -> Step o -> Step (Stepped n o)

-- instance RunStep Z o where
--   type Stepped Z o = o
--   runStep _ step = step
-- -- x :: Step (Ocean 1 2 1 0 0 0 1 0 1)
-- -- x = f (Lift s)
-- --   where f = Step . Step . Step
-- -- type family StepOcean (o :: Ocean 
-- --   (zero :: Nat)
-- --   (one :: Nat)
-- --   (two :: Nat)
-- --   (three :: Nat)
-- --   (four :: Nat)
-- --   (five :: Nat)
-- --   (six :: Nat)
-- --   (seven :: Nat)
-- --   (eight :: Nat)
-- --   ) where
