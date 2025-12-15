{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Internal.DList
-- Description : Difference lists for efficient O(1) append
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides a difference list implementation for efficient
-- accumulation of diagnostics. Unlike regular lists where (++) is O(n),
-- DList provides O(1) append operations by representing lists as functions.
--
-- The key insight is that a difference list is represented as a function
-- that prepends elements to its argument:
--
-- > [1,2,3] becomes \xs -> [1,2,3] ++ xs
--
-- This allows O(1) concatenation:
--
-- > append dl1 dl2 = \xs -> dl1 (dl2 xs)
--
-- Performance characteristics:
-- * empty: O(1)
-- * singleton: O(1)
-- * append: O(1)
-- * snoc: O(1)
-- * toList: O(n)
-- * fromList: O(n)
module Argus.Internal.DList
  ( -- * Type
    DList

    -- * Construction
  , empty
  , singleton
  , fromList

    -- * Operations
  , append
  , snoc
  , cons

    -- * Conversion
  , toList
  , length

    -- * Instances
    -- | DList is a Semigroup and Monoid
  ) where

import Prelude hiding (length)
import Control.DeepSeq (NFData (..))
import Data.Foldable qualified as F

-- | A difference list implemented as a function
-- The representation is a function from list to list
newtype DList a = DList ([a] -> [a])

-- | The empty difference list
empty :: DList a
empty = DList id
{-# INLINE empty #-}

-- | A singleton difference list
singleton :: a -> DList a
singleton x = DList (x:)
{-# INLINE singleton #-}

-- | Convert a regular list to a difference list
-- Complexity: O(1)
fromList :: [a] -> DList a
fromList xs = DList (xs++)
{-# INLINE fromList #-}

-- | Append two difference lists
-- Complexity: O(1)
append :: DList a -> DList a -> DList a
append (DList f) (DList g) = DList (f . g)
{-# INLINE append #-}

-- | Add an element to the end of a difference list
-- Complexity: O(1)
snoc :: DList a -> a -> DList a
snoc dl x = dl `append` singleton x
{-# INLINE snoc #-}

-- | Add an element to the front of a difference list
-- Complexity: O(1)
cons :: a -> DList a -> DList a
cons x dl = singleton x `append` dl
{-# INLINE cons #-}

-- | Convert a difference list to a regular list
-- Complexity: O(n)
toList :: DList a -> [a]
toList (DList f) = f []
{-# INLINE toList #-}

-- | Get the length of a difference list
-- Complexity: O(n) - must convert to list first
-- This is not recommended for performance-critical code
length :: DList a -> Int
length = F.length . toList
{-# INLINE length #-}

-- | Semigroup instance for DList
-- Uses O(1) append operation
instance Semigroup (DList a) where
  (<>) = append
  {-# INLINE (<>) #-}

-- | Monoid instance for DList
-- Identity is the empty difference list
instance Monoid (DList a) where
  mempty = empty
  {-# INLINE mempty #-}

-- | Foldable instance for DList
-- Converts to list first, then folds
instance Foldable DList where
  foldr f z dl = F.foldr f z (toList dl)
  {-# INLINE foldr #-}

  foldl f z dl = F.foldl f z (toList dl)
  {-# INLINE foldl #-}

  foldl' f z dl = F.foldl' f z (toList dl)
  {-# INLINE foldl' #-}

  length = Argus.Internal.DList.length
  {-# INLINE length #-}

  null dl = null (toList dl)
  {-# INLINE null #-}

-- | Functor instance for DList
instance Functor DList where
  fmap f dl = fromList (map f (toList dl))
  {-# INLINE fmap #-}

-- | Applicative instance for DList
instance Applicative DList where
  pure = singleton
  {-# INLINE pure #-}

  fs <*> xs = fromList [f x | f <- toList fs, x <- toList xs]
  {-# INLINE (<*>) #-}

-- | Monad instance for DList
instance Monad DList where
  m >>= f = fromList (concatMap (toList . f) (toList m))
  {-# INLINE (>>=) #-}

-- | Show instance for DList
-- Shows as the underlying list
instance Show a => Show (DList a) where
  showsPrec p dl = showsPrec p (toList dl)

-- | Eq instance for DList
-- Compares the underlying lists
instance Eq a => Eq (DList a) where
  dl1 == dl2 = toList dl1 == toList dl2
  {-# INLINE (==) #-}

-- | Ord instance for DList
-- Compares the underlying lists
instance Ord a => Ord (DList a) where
  compare dl1 dl2 = compare (toList dl1) (toList dl2)
  {-# INLINE compare #-}

-- | NFData instance for DList
-- Forces evaluation of the underlying list
instance NFData a => NFData (DList a) where
  rnf dl = rnf (toList dl)
  {-# INLINE rnf #-}
