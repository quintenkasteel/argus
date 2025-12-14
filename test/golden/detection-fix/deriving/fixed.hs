{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module DerivingExamples where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Control.DeepSeq (NFData)

--------------------------------------------------------------------------------
-- Prefer deriving strategies
--------------------------------------------------------------------------------

-- Should warn: ambiguous deriving without strategy
data PersonOld = PersonOld
  { poName :: String
  , poAge :: Int
  } deriving (Show, Eq, Generic, ToJSON)

-- Good: explicit deriving strategies
data PersonNew = PersonNew
  { pnName :: String
  , pnAge :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Prefer GeneralizedNewtypeDeriving
--------------------------------------------------------------------------------

-- Should warn: could use newtype deriving
newtype UserId = UserId Int
  deriving (Show, Eq)

-- Should warn: manual instance when deriving would work
newtype EmailOld = EmailOld String

instance Show EmailOld where
  show (EmailOld s) = s

-- Good: using GeneralizedNewtypeDeriving
newtype EmailNew = EmailNew String
  deriving newtype (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Missing common derivations
--------------------------------------------------------------------------------

-- Should warn: missing Eq when Ord is derived
data Priority = Low | Medium | High
  deriving (Ord)

-- Should warn: missing Show for debugging
data InternalError = InternalError String Int

-- Good: has common instances
data Status = Active | Inactive | Pending
  deriving stock (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Prefer newtype for single-field types
--------------------------------------------------------------------------------

-- Should warn: single-field data should be newtype
data Wrapper a = Wrapper a
  deriving (Show, Eq)

-- Good: newtype for single field
newtype WrapperNew a = WrapperNew a
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- DerivingVia for common patterns
--------------------------------------------------------------------------------

-- Should suggest: could use DerivingVia
newtype PositiveInt = PositiveInt Int
  deriving stock (Show)

instance Eq PositiveInt where
  PositiveInt a == PositiveInt b = a == b

instance Ord PositiveInt where
  compare (PositiveInt a) (PositiveInt b) = compare a b

-- Good: using newtype deriving
newtype NatInt = NatInt Int
  deriving newtype (Show, Eq, Ord, Num)

--------------------------------------------------------------------------------
-- Standalone deriving when needed
--------------------------------------------------------------------------------

data Box a = Box a

-- Should suggest: use standalone deriving for constrained instance
instance Show a => Show (Box a) where
  show (Box x) = "Box " ++ show x

-- Good: standalone deriving
deriving stock instance Eq a => Eq (Box a)

--------------------------------------------------------------------------------
-- Generic deriving for NFData
--------------------------------------------------------------------------------

-- Should warn: manual NFData when Generic deriving would work
data TreeOld a = LeafOld a | NodeOld (TreeOld a) (TreeOld a)
  deriving (Show, Generic)

-- Good: derive NFData via Generic
data TreeNew a = LeafNew a | NodeNew (TreeNew a) (TreeNew a)
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

--------------------------------------------------------------------------------
-- Redundant derivations
--------------------------------------------------------------------------------

-- Should warn: Enum implies Bounded order for simple types
data SimpleEnum = A | B | C
  deriving (Enum)

-- Good: derive both
data CompleteEnum = X | Y | Z
  deriving stock (Show, Eq, Ord, Enum, Bounded)