{-# LANGUAGE NoImplicitPrelude #-}
module AOC2021.Prelude 
  ( module Data.Char
  , module Data.List
  , module Control.Applicative
  , module Control.Monad
  , module Data.Functor
  , module Data.Maybe
  , module Data.Either
  , module Data.Function
  , module Data.Bifunctor
  , module Data.Functor.Contravariant
  , module Data.Profunctor
  , module AOC2021.Helpers
  , module GHC.Exts
  , module Control.Arrow
  , module Prelude
  , module Control.Monad.Trans.Class
  , module Control.Monad.IO.Class
  , module Control.Monad.Trans.Maybe
  , module Control.Monad.Cont
  , module Control.Monad.Except
  , module Control.Monad.Identity
  , module Control.Monad.State
  , module Control.Monad.Writer
  , module Control.Monad.Reader
  , module Data.Functor.Compose
  , module Data.Foldable
  , module Data.Semigroup
  , module Debug.Trace
  , module Text.Read
  , module Data.Bool
  , module Data.Bits
  , module Data.List.Split
  , module Data.Align
  , module Data.These
  , module Data.Coerce
  , module Data.Validation
  ) where

import Data.Char
import Data.List hiding (sum, product, foldl')
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Maybe
import Data.Either
import Data.Function
import Data.Bifunctor
import Data.Functor.Contravariant
import Data.Profunctor hiding (WrapArrow, WrappedArrow, unwrapArrow)
import AOC2021.Helpers
import GHC.Exts (IsString (..), IsList (..))
import Control.Arrow hiding (first, second)
import Prelude hiding (sum, product)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe hiding (liftCatch, liftCallCC)
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer hiding (First (..), Last (..))
import Control.Monad.Reader
import Data.Functor.Compose
import Data.Foldable hiding (sum, product, toList)
import Data.Semigroup
import Debug.Trace
import Text.Read (readMaybe)
import Data.Bool
import Data.Bits
import Data.List.Split
import Data.Align
import Data.These
import Data.Coerce
import Data.Validation