{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module TypeFamiliesExamples where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Nat, Symbol, type (+), type (-))

--------------------------------------------------------------------------------
-- Open type family without default
--------------------------------------------------------------------------------

-- Should suggest: consider adding a default instance or using closed family
type family Element a

type instance Element [a] = a
type instance Element (Maybe a) = a
-- Missing instances for other types...

-- Good: closed type family with all cases
type family ElementClosed a where
  ElementClosed [a] = a
  ElementClosed (Maybe a) = a
  ElementClosed (Either e a) = a

--------------------------------------------------------------------------------
-- Overlapping type family instances
--------------------------------------------------------------------------------

-- Should warn: overlapping instances in open type family
type family Container a

type instance Container [a] = [a]
type instance Container ([] a) = [a]  -- Same as above!

-- Good: no overlap
type family ContainerGood a where
  ContainerGood [a] = [a]
  ContainerGood (Maybe a) = Maybe a

--------------------------------------------------------------------------------
-- Non-injective type family used in ambiguous context
--------------------------------------------------------------------------------

type family FromMaybe a where
  FromMaybe (Maybe a) = a
  FromMaybe a = a

-- Should warn: non-injective type family may cause ambiguity
extractMaybe :: FromMaybe (Maybe a) -> a
extractMaybe x = x  -- Type 'a' is ambiguous

-- Good: use TypeFamilyDependencies or explicit type
type family FromMaybeInj a = r | r -> a where
  FromMaybeInj (Maybe a) = a

--------------------------------------------------------------------------------
-- Recursive type family without base case
--------------------------------------------------------------------------------

-- Should warn: potentially non-terminating
type family Forever a where
  Forever a = Forever (Maybe a)

-- Good: has base case
type family Limited (n :: Nat) a where
  Limited 0 a = a
  Limited n a = Maybe (Limited (n - 1) a)

--------------------------------------------------------------------------------
-- Missing kind signature
--------------------------------------------------------------------------------

-- Should suggest: add kind signature for clarity
type family MyFamily a where
  MyFamily Int = Bool
  MyFamily Bool = Int

-- Good: explicit kind signature
type MyFamilyKinded :: Type -> Type
type family MyFamilyKinded a where
  MyFamilyKinded Int = Bool
  MyFamilyKinded Bool = Int

--------------------------------------------------------------------------------
-- Associated type without documentation
--------------------------------------------------------------------------------

class Container' c where
  -- Should suggest: document associated type
  type Elem c

instance Container' [a] where
  type Elem [a] = a

-- Good: well-documented class
-- | A container that supports efficient append
class Appendable c where
  -- | The element type of the container
  type AppendElem c
  append :: c -> AppendElem c -> c

--------------------------------------------------------------------------------
-- Data family could be type family
--------------------------------------------------------------------------------

-- Should suggest: type family may be simpler
data family Simple a

data instance Simple Int = SimpleInt Int
data instance Simple Bool = SimpleBool Bool

-- Good: uses data family features (different representations)
data family Complex a

data instance Complex Int = ComplexInt !Int !Int  -- Strict, two fields
data instance Complex Bool = ComplexBool Bool     -- Single field

--------------------------------------------------------------------------------
-- Type family in constraint position
--------------------------------------------------------------------------------

type family IsNum a :: Constraint where
  IsNum Int = ()
  IsNum Integer = ()
  IsNum Double = ()
  IsNum Float = ()

-- Should suggest: consider using type class instead
addNums :: IsNum a => a -> a -> a
addNums = undefined  -- Can't actually implement!

-- Good: use type class
class NumLike a where
  addNumsGood :: a -> a -> a

--------------------------------------------------------------------------------
-- Partial type family
--------------------------------------------------------------------------------

-- Should warn: partial type family (not all types covered)
type family Unwrap a where
  Unwrap (Maybe a) = a
  Unwrap (Either e a) = a
  -- What about other types?

-- Good: total with fallback
type family UnwrapTotal a where
  UnwrapTotal (Maybe a) = a
  UnwrapTotal (Either e a) = a
  UnwrapTotal a = a  -- Fallback

--------------------------------------------------------------------------------
-- Type family vs type synonym
--------------------------------------------------------------------------------

-- Should suggest: simple type synonym would suffice
type family Identity a where
  Identity a = a

-- Good: use type synonym
type IdentitySyn a = a

--------------------------------------------------------------------------------
-- Complex type-level computation
--------------------------------------------------------------------------------

-- Should suggest: consider singletons or type-level library
type family Add (m :: Nat) (n :: Nat) :: Nat where
  Add 0 n = n
  Add m n = Add (m - 1) (n + 1)

-- Good: use GHC.TypeLits
type AddGood m n = m + n

--------------------------------------------------------------------------------
-- Stuck type family
--------------------------------------------------------------------------------

type family GetFirst a where
  GetFirst (a, b) = a

-- Should warn: type family may get stuck on unknown types
processFirst :: GetFirst (a, b) -> a
processFirst x = x  -- This works

-- Pattern that causes stuck family:
-- processUnknown :: GetFirst t -> ???  -- 't' not known to be tuple
