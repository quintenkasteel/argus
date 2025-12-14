{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeclassLawsExamples where

import Control.Monad (ap)
import Data.Monoid (Sum(..))

--------------------------------------------------------------------------------
-- Eq instance without reflexivity
--------------------------------------------------------------------------------

data BadEq = BadEq Int deriving Show

-- Should warn: Eq instance may violate reflexivity
instance Eq BadEq where
  BadEq x == BadEq y = x /= y  -- Violates: a == a should be True

-- Good: lawful Eq
data GoodEq = GoodEq Int deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Ord instance inconsistent with Eq
--------------------------------------------------------------------------------

data BadOrd = BadOrd Int Int deriving (Show, Eq)

-- Should warn: Ord inconsistent with Eq
instance Ord BadOrd where
  compare (BadOrd x _) (BadOrd y _) = compare x y
  -- Violates: compare a b == EQ implies a == b

-- Good: consistent Ord
data GoodOrd = GoodOrd Int deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Semigroup without associativity
--------------------------------------------------------------------------------

newtype BadSemigroup = BadSemi Int deriving Show

-- Should warn: Semigroup may not be associative
instance Semigroup BadSemigroup where
  BadSemi x <> BadSemi y = BadSemi (x - y)
  -- Violates: (a <> b) <> c == a <> (b <> c)

-- Good: associative Semigroup
newtype GoodSemigroup = GoodSemi Int deriving (Show, Eq)

instance Semigroup GoodSemigroup where
  GoodSemi x <> GoodSemi y = GoodSemi (x + y)

--------------------------------------------------------------------------------
-- Monoid without identity
--------------------------------------------------------------------------------

newtype BadMonoid = BadMon Int deriving (Show, Semigroup)

-- Should warn: Monoid identity law violated
instance Monoid BadMonoid where
  mempty = BadMon 1  -- With (<>) = (+), mempty should be 0

-- Good: lawful Monoid
instance Monoid GoodSemigroup where
  mempty = GoodSemi 0

--------------------------------------------------------------------------------
-- Functor without identity
--------------------------------------------------------------------------------

data BadFunctor a = BadF a deriving Show

-- Should warn: Functor identity law violated
instance Functor BadFunctor where
  fmap f (BadF x) = BadF (f (f x))  -- Violates: fmap id == id

-- Good: lawful Functor
data GoodFunctor a = GoodF a deriving (Show, Functor)

--------------------------------------------------------------------------------
-- Applicative without identity
--------------------------------------------------------------------------------

newtype BadApplicative a = BadApp a deriving (Show, Functor)

-- Should warn: Applicative laws may be violated
instance Applicative BadApplicative where
  pure x = BadApp x
  BadApp f <*> BadApp x = BadApp (f x)
  -- This is actually lawful, but worth checking

--------------------------------------------------------------------------------
-- Monad with inconsistent return
--------------------------------------------------------------------------------

newtype BadMonad a = BadM a deriving (Show, Functor)

instance Applicative BadMonad where
  pure = BadM
  BadM f <*> BadM x = BadM (f x)

-- Should warn: Monad laws - check return/pure consistency
instance Monad BadMonad where
  return x = BadM x  -- Should be same as pure
  BadM x >>= f = f x

-- Good: consistent Monad
newtype GoodMonad a = GoodM a deriving (Show, Functor)

instance Applicative GoodMonad where
  pure = GoodM
  (<*>) = ap

instance Monad GoodMonad where
  GoodM x >>= f = f x

--------------------------------------------------------------------------------
-- Foldable/Traversable consistency
--------------------------------------------------------------------------------

data BadFoldable a = BF [a] deriving (Show, Functor)

-- Should warn: foldMap should be consistent with fold
instance Foldable BadFoldable where
  foldr f z (BF xs) = foldr f z xs
  foldMap f (BF xs) = mconcat (map f (reverse xs))  -- Inconsistent!

-- Good: consistent Foldable
newtype GoodFoldable a = GF [a] deriving (Show, Functor, Foldable)

--------------------------------------------------------------------------------
-- Num laws
--------------------------------------------------------------------------------

newtype BadNum = BNum Int deriving Show

-- Should warn: Num instance may violate laws
instance Num BadNum where
  BNum x + BNum y = BNum (x * y)  -- Not addition!
  BNum x * BNum y = BNum (x + y)  -- Swapped with +
  abs (BNum x) = BNum x
  signum (BNum x) = BNum 1
  fromInteger = BNum . fromInteger
  negate (BNum x) = BNum (-x)

-- Good: use newtype deriving
newtype GoodNum = GNum Int deriving (Show, Eq, Num)

--------------------------------------------------------------------------------
-- Show/Read roundtrip
--------------------------------------------------------------------------------

data BadShowRead = BSR Int deriving Eq

-- Should warn: Show/Read roundtrip fails
instance Show BadShowRead where
  show (BSR x) = "value: " ++ show x  -- Can't be parsed back

instance Read BadShowRead where
  readsPrec _ _ = []  -- Can't read anything!

-- Good: derivable Show/Read
data GoodShowRead = GSR Int deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Hashable consistency with Eq
--------------------------------------------------------------------------------

-- Note: this would need hashable package
-- Should warn: Hashable inconsistent with Eq
-- If a == b then hash a == hash b must hold

--------------------------------------------------------------------------------
-- Alternative laws
--------------------------------------------------------------------------------

-- Should check: empty <|> x == x
-- Should check: x <|> empty == x
-- Should check: (x <|> y) <|> z == x <|> (y <|> z)

--------------------------------------------------------------------------------
-- Category laws
--------------------------------------------------------------------------------

-- Should check: f . id == f
-- Should check: id . f == f
-- Should check: (f . g) . h == f . (g . h)
