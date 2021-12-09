{-# LANGUAGE UndecidableInstances #-}
module AOC2021.Helpers where

import Data.List.Split
import Data.Maybe
import Data.Char
import Prelude hiding (sum, product)
import Text.Read (readMaybe)
import Data.Either (isLeft, fromRight)
import Data.Foldable (foldl', Foldable (toList))
import Control.Arrow
import Data.HashMap.Lazy qualified as HMLazy
import Data.HashMap.Strict qualified as HMStrict
import Data.Map.Lazy qualified as MLazy
import Data.Map.Strict qualified as MStrict
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Data.List qualified as List
import Control.Applicative
import Data.Traversable
import Data.Bifunctor (Bifunctor (bimap))
import Control.Monad (join)
import Data.Function (on)
import Data.List (iterate')
import Control.Category (Category)
import Data.Kind (Type)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Concurrent.MVar (MVar, newMVar, readMVar, putMVar, modifyMVar, modifyMVar_)
import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')
import Control.Monad.State (MonadState, gets)
import Data.Hashable (Hashable)
import qualified Data.IntMap as IntMap
import Data.Set qualified as Set
import Data.Monoid (Sum (..))

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

orMempty :: (Coalesce f, Monoid a) => f a -> a
orMempty = coalesce mempty

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

dup :: b -> (b, b)
dup x = (x, x)

dupWith :: (a -> b) -> a -> (a, b)
dupWith f x = (x, f x)

both :: Bifunctor f => (a -> b) -> f a a -> f b b
both = join bimap

-- (+) `liftOn` (+) x y = \z w -> x + y + z + w
-- (+) `liftOn` (Just . succ) 1 2 = Just (2 + 3)
liftOn :: Applicative f => (a -> a -> c) -> (b -> f a) -> b -> b -> f c
liftOn f g = liftA2 f `on` g

uncurryOn :: (b -> b -> c) -> (a -> b) -> (a, a) -> c
uncurryOn f g = uncurry (f `on` g)

infixr 9 `uncurryOn`

thenJust :: Bool -> a -> Maybe a
thenJust True = Just 
thenJust _ = const Nothing

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g x y = f (g x y)
infixl 9 .:

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap2
infixl 8 <<$>>

foldWhileM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
foldWhileM f = go where 
  go [] = pure Nothing
  go (x : xs) = f x >>= \case
    j@(Just _) -> pure j
    _ -> go xs

order2 :: Ord a => a -> a -> (a, a)
order2 a b = if a <= b then (a, b) else (b, a)

uncurryP :: (a -> b -> c) -> (d -> e -> (a, b)) -> (d, e) -> c
uncurryP f g = uncurry f . uncurry g

(+!) :: Num a => a -> a -> a
(!x) +! (!y) = x + y

(*!) :: Num a => a -> a -> a
(!x) *! (!y) = x * y

countElems :: (Ord k, Num a) => [k] -> MLazy.Map k a
countElems = MLazy.fromListWith (+!) . map (,1)

iterateTimes :: Int -> (a -> a) -> a -> a
iterateTimes n f = (!! n) . iterate' f

parseCommas :: Read a => String -> [a]
parseCommas = orMempty . traverse readMaybe . splitOn ","

splitOnce :: Eq a => a -> [a] -> ([a], [a])
splitOnce sep = fmap tail' . span (/= sep)

countIf :: Foldable t => (a -> Bool) -> t a -> Int
countIf p = foldl' (\a -> (a +) . fromEnum . p) 0

alterRefM :: STRef s a -> (a -> ST s (b, a)) -> ST s b
alterRefM ref f = do
  val <- readSTRef ref
  (res, !val') <- f val
  writeSTRef ref val' 
  pure res

alterRef :: STRef s a -> (a -> (b, a)) -> ST s b
alterRef ref f = alterRefM ref (pure . f)

catchEither :: Maybe b -> a -> Either a b
catchEither mb e = maybe (Left e) Right mb

justOr :: a -> Maybe b -> Either a b
justOr = flip catchEither

class Semigroup s => Subtractible s where
  (<->) :: s -> s -> s

instance Subtractible a => Subtractible (Maybe a) where
  Just x <-> Just y = Just (x <-> y)
  x <-> _ = x

instance Eq a => Subtractible [a] where
  (<->) = (List.\\)

instance Ord k => Subtractible (MLazy.Map k v) where
  (<->) = MLazy.difference

instance (Hashable k, Eq k) => Subtractible (HMLazy.HashMap k v) where
  (<->) = HMLazy.difference
instance Subtractible (IntMap.IntMap v) where
  (<->) = IntMap.difference

instance (Ord k) => Subtractible (Set.Set k) where
  (<->) = Set.difference

instance Num a => Subtractible (Sum a) where
  Sum a <-> Sum b = Sum (a - b)

slipR :: (a -> b -> c -> d) -> b -> c -> a -> d
slipR f b c a = f a b c

fromDigits :: (Num a, Foldable t) => t a -> a
fromDigits = foldl' (\a x -> (a * 10) + x) 0

toMapOf :: (Ord k, Foldable t) => (v -> k) -> (v -> v') -> t v -> MLazy.Map k v'
toMapOf = toMapWith const

toMapWith :: (Ord k, Foldable t) => (v' -> v' -> v') -> (v -> k) -> (v -> v') -> t v -> MLazy.Map k v'
toMapWith combine mkKey mkVal = foldr (liftA2 (MLazy.insertWith combine) mkKey mkVal) mempty

toMapSemi :: (Ord k, Foldable t, Semigroup v') => (v -> k) -> (v -> v') -> t v -> MLazy.Map k v'
toMapSemi = toMapWith (<>)

toMap :: (Foldable t, Ord k) => t k -> t v -> MLazy.Map k v
toMap keys vals = MLazy.fromList $ zip (toList keys) (toList vals)