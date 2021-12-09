{-# LANGUAGE UndecidableInstances #-}
module AOC2021.Solutions.Day8  where
-- module AOC2021.Solutions.Day8 (part1, part2) where

import AOC2021.Prelude
import Data.Map.Lazy qualified as Map
import Data.Set qualified as Set
import Data.Set (Set)
import Control.Monad.ST
import Data.STRef
import Data.Text qualified as Text
import Data.List.NonEmpty (NonEmpty)
import GHC.TypeLits
import Data.Proxy (Proxy(Proxy))

data Signal = Signal {pats :: [Set Char], outputs :: [Set Char]} deriving (Eq, Show)

parseSignals :: String -> [Signal]
parseSignals = parseLines $ uncurry Signal . both (map Set.fromList) . splitOnce "|" . words

-- size-indexed Set for typesafe manual set operations
newtype LenSet (n :: Nat) s = LenSet {getLenSet :: Set.Set s} deriving (Eq, Show, Ord, Foldable)

type Positive n = 1 <= n

findMin :: (Positive n) => LenSet n s -> s
findMin (LenSet s) = Set.findMin s

removeFirst :: (Positive n, Ord s) => (s -> Bool) -> LenSet n s -> Maybe (s, LenSet (n - 1) s)
removeFirst f (LenSet s) = Set.lookupMin (Set.filter f s) <&> ((,) <*> (LenSet . (`Set.delete` s)))

fromSetWithLen :: forall proxy s (n :: Nat). (Ord s, KnownNat n) => proxy n -> Set.Set s -> Maybe (LenSet n s)
fromSetWithLen _ = liftA2 thenJust ((nv ==) . length) LenSet 
  where nv = fromIntegral $ natVal (Proxy @n)

{-
   aaaa
  b    c
  b    c
   dddd
  e    f
  e    f
   gggg
-}

decodeSignal :: Signal -> Int
decodeSignal (Signal pats outputs) = 
  let buckets = toMapSemi length Set.singleton pats
      findSet = Set.lookupMin .: Set.filter
      getByLen = orMempty . (`Map.lookup` buckets)

      getChunks :: (Positive n, KnownNat n) => Proxy n -> Int -> Maybe (LenSet n (Set Char))
      getChunks proxyN len = fromSetWithLen proxyN (getByLen len)

      consume :: Ord c => Set c -> LenSet 2 (Set c) -> Maybe (Set c, Set c)
      consume marker = fmap2 findMin . removeFirst (Set.isSubsetOf marker)

   in fromDigits $ orMempty do
        [one, seven, four, eight] <- traverse (Set.lookupMin . getByLen) [2, 3, 4, 7]
        let eg = eight <-> four <-> seven
            cf = one

        (nine, sixZero) <- removeFirst (not . Set.isSubsetOf eg) =<< getChunks (Proxy @3) 6
        (zero, six) <- consume cf sixZero

        let e = Set.findMin $ eight <-> nine

        (three, twoFive) <- removeFirst (Set.isSubsetOf cf) =<< getChunks (Proxy @3) 5
        (two, five) <- consume (Set.singleton e) twoFive

        let mp = toMap [zero, one, two, three, four, five, six, seven, eight, nine] [0..]
        traverse (`Map.lookup` mp) outputs

part1 :: String -> Int
part1 = countIf ((`elem` [2, 3, 4, 7]) . length) . concatMap outputs . parseSignals

part2 :: String -> Int
part2 = sum . map decodeSignal . parseSignals