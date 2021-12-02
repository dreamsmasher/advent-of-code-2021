{-# LANGUAGE UndecidableInstances #-}
module AOC2021.Helpers where

import Data.List.Split
import Data.Maybe
import Data.Char
import Prelude hiding (sum, product)
import Text.Read (readMaybe)
import Data.Either (isLeft, fromRight)
import Data.Foldable (foldl')
import Control.Arrow
import Data.HashMap.Lazy qualified as HMLazy
import Data.Map.Lazy qualified as MLazy
import Data.Text qualified as T
import Data.List qualified as List
import Control.Applicative
import Data.Traversable

parseLines :: (String -> a) -> String -> [a]
parseLines f = map f . lines

parseLinesWithSep :: Char -> ([String] -> a) -> String -> [a]
parseLinesWithSep sep f = parseLines (f . splitOn (pure sep))

class Falsy a where
  {-# MINIMAL falsy | truthful #-}
  falsy :: a -> Bool
  falsy = not . truthful
  truthful :: a -> Bool
  truthful = not . falsy

  -- let foo = True ? 1 $ 2
  (?) :: a -> b -> b -> b
  (?) a = if truthful a then const else const id

  infixl 6 ?

instance Falsy Bool where
  truthful = id

instance Foldable t => Falsy (t a) where
  falsy = null

instance Falsy () where
  truthful _ = True

-- to avoid "\" ... \"" when calling `show` on a `String`
class ToString s where
  toString :: s -> String

-- safer variants when we don't care about empty inputs
init' :: [a] -> [a]
init' [] = []
init' xs = init xs

tail' :: [a] -> [a]
tail' = drop 1

instance {-# OVERLAPPABLE #-} Show s => ToString s where
  toString = show

instance {-# OVERLAPPABLE #-} Show a => ToString [a] where
  toString = show >>> \case
    '"' : s -> init s
    s -> s

instance {-# OVERLAPS #-} ToString T.Text where
  toString = T.unpack

solve :: ToString b => (String -> a) -> (a -> b) -> String -> String
solve parser cont = toString . cont . parser

solveLines :: (ToString b, Read a) => ([a] -> b) -> String -> String
solveLines = solve (parseLines read)

sum, product :: (Foldable t, Num a) => t a -> a
sum = foldl' (+) 0
product = foldl' (*) 1

class Coalesce f where
  coalesce :: a -> f a -> a
  (??) :: f a -> a -> a
  (??) = flip coalesce
  infixr 6 ??

-- Maybe, Either, [], ...
instance Foldable f => Coalesce f where
  coalesce = foldr const

(~>) :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
(~>) = for
infixr 6 ~>

dropR :: Int -> [a] -> [a]
dropR n xs = take (length xs - n) xs

triple :: (a -> a -> a) -> (a -> a -> a -> a)
triple f a = f . f a

-- \a b -> f ((g `on` h) a b)
applyOn :: (c -> d) -> (b -> b -> c) -> (a -> b) -> a -> a -> d
applyOn h g f a1 a2 = h $ g (f a1) (f a2)

capitalize :: String -> String
capitalize (c : cs) = toUpper c : map toLower cs
capitalize _ = []

readLower :: Read a => String -> Maybe a
readLower = readMaybe . capitalize