{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Argus.HIE.Types
-- Description : Core types for HIE-backed analysis
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module defines the core types used for HIE-backed analysis,
-- including symbol information, type constraints, and safety checks.
module Argus.HIE.Types
  ( -- * Symbol Information
    HieSymbol (..)
  , SymbolScope (..)
  , SymbolVisibility (..)

    -- * Type Information
  , TypeConstraint (..)
  , ConstraintKind (..)
  , TypeInfo (..)
  , InstanceInfo (..)

    -- * Replace Safety
  , ReplaceSafety (..)
  , ReplaceWarning (..)

    -- * Fix Validation
  , TypeValidation (..)
  , ValidationIssue (..)

    -- * Query Results
  , SymbolQuery (..)
  , QueryResult (..)

    -- * Cross-Module
  , ModuleSymbols (..)
  , SymbolReference (..)
  , ReferenceKind (..)
  ) where

import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

import Argus.Types (SrcSpan, SymbolKind)

--------------------------------------------------------------------------------
-- Symbol Information
--------------------------------------------------------------------------------

-- | Complete symbol information extracted from HIE
data HieSymbol = HieSymbol
  { hsName         :: Text           -- ^ The symbol name
  , hsModule       :: Text           -- ^ Defining module
  , hsQualified    :: Text           -- ^ Fully qualified name (Module.name)
  , hsKind         :: SymbolKind     -- ^ Symbol kind (Value, Type, Class, etc.)
  , hsType         :: Maybe Text     -- ^ Type signature if available
  , hsDefinition   :: Maybe SrcSpan  -- ^ Definition location
  , hsReferences   :: [SrcSpan]      -- ^ All reference locations
  , hsExported     :: Bool           -- ^ Is the symbol exported?
  , hsScope        :: SymbolScope    -- ^ Scope of the symbol
  , hsVisibility   :: SymbolVisibility -- ^ Visibility level
  , hsDocumentation :: Maybe Text    -- ^ Haddock documentation if available
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

instance ToJSON HieSymbol
instance FromJSON HieSymbol

-- | Scope of a symbol
data SymbolScope
  = ScopeLocal        -- ^ Local binding (let, where, lambda)
  | ScopeModule       -- ^ Module-level binding
  | ScopeGlobal       -- ^ Exported and visible globally
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | Visibility of a symbol
data SymbolVisibility
  = VisPrivate        -- ^ Not exported
  | VisInternal       -- ^ Exported but internal module
  | VisPublic         -- ^ Publicly exported
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Type Information
--------------------------------------------------------------------------------

-- | Type constraint information
data TypeConstraint = TypeConstraint
  { tcClass      :: Text           -- ^ Type class name (e.g., "Ord", "Eq")
  , tcType       :: Text           -- ^ Type the constraint applies to
  , tcSatisfied  :: Bool           -- ^ Whether the constraint is satisfied
  , tcInstance   :: Maybe InstanceInfo -- ^ Instance info if satisfied
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | Kind of constraint
data ConstraintKind
  = CKTypeClass Text     -- ^ Type class constraint (Ord a, Eq a)
  | CKEquality Text Text -- ^ Type equality (a ~ b)
  | CKHasField Text Text -- ^ HasField constraint
  | CKTypeable           -- ^ Typeable constraint
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | Type information for an expression or binding
data TypeInfo = TypeInfo
  { tiType        :: Text            -- ^ Pretty-printed type
  , tiKind        :: Text            -- ^ Kind of the type (*, * -> *, etc.)
  , tiConstraints :: [TypeConstraint] -- ^ Required constraints
  , tiMonomorphic :: Bool            -- ^ Is the type fully monomorphic?
  , tiPolymorphic :: [Text]          -- ^ Type variables if polymorphic
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | Instance information
data InstanceInfo = InstanceInfo
  { iiClass      :: Text           -- ^ Class name
  , iiType       :: Text           -- ^ Instance type
  , iiModule     :: Text           -- ^ Defining module
  , iiOrphan     :: Bool           -- ^ Is this an orphan instance?
  , iiOverlap    :: Maybe Text     -- ^ Overlap mode if any
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Replace Safety
--------------------------------------------------------------------------------

-- | Safety analysis for replacing one symbol with another
data ReplaceSafety
  = SafeReplace                    -- ^ Always safe to replace
  | SafeWithConstraint TypeConstraint -- ^ Safe if constraint holds
  | SafeWithImport Text Text       -- ^ Safe if import added (module, symbol)
  | UnsafeConflict Text            -- ^ Would conflict with existing name
  | UnsafeShadow Text Text         -- ^ Would shadow imported name (module, name)
  | UnsafeTypeChange Text Text     -- ^ Would change type (old, new)
  | UnsafeSemanticChange Text      -- ^ Would change semantics (reason)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | Warning about a replacement
data ReplaceWarning = ReplaceWarning
  { rwSeverity   :: Text           -- ^ Warning severity
  , rwMessage    :: Text           -- ^ Warning message
  , rwLocation   :: Maybe SrcSpan  -- ^ Location if relevant
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Fix Validation
--------------------------------------------------------------------------------

-- | Result of validating a fix's type correctness
data TypeValidation
  = TypesPreserved                 -- ^ Fix preserves types
  | TypesCompatible TypeInfo       -- ^ Types are compatible (more specific)
  | TypesIncompatible ValidationIssue -- ^ Types are incompatible
  | TypesUnknown                   -- ^ Unable to determine type compatibility
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Issue found during validation
data ValidationIssue = ValidationIssue
  { viKind       :: Text           -- ^ Issue kind
  , viMessage    :: Text           -- ^ Human-readable message
  , viLocation   :: SrcSpan        -- ^ Location of the issue
  , viExpected   :: Maybe Text     -- ^ Expected type/constraint
  , viActual     :: Maybe Text     -- ^ Actual type/constraint
  , viSuggestion :: Maybe Text     -- ^ Suggested fix
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Query Results
--------------------------------------------------------------------------------

-- | Query for finding symbols
data SymbolQuery = SymbolQuery
  { sqName       :: Text           -- ^ Symbol name to find
  , sqModule     :: Maybe Text     -- ^ Specific module to search
  , sqKind       :: Maybe SymbolKind -- ^ Filter by kind
  , sqExported   :: Maybe Bool     -- ^ Filter by export status
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Result of a symbol query
data QueryResult a = QueryResult
  { qrResults    :: [a]            -- ^ Found results
  , qrTotal      :: Int            -- ^ Total count
  , qrTruncated  :: Bool           -- ^ Whether results were truncated
  , qrDuration   :: Double         -- ^ Query duration in ms
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Cross-Module Types
--------------------------------------------------------------------------------

-- | All symbols in a module
data ModuleSymbols = ModuleSymbols
  { msModule     :: Text           -- ^ Module name
  , msFile       :: Maybe FilePath -- ^ Source file path
  , msExports    :: Set Text       -- ^ Exported symbols
  , msImports    :: Map Text [Text] -- ^ Imported symbols by module
  , msDefinitions :: [HieSymbol]   -- ^ All definitions
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A reference to a symbol
data SymbolReference = SymbolReference
  { srSymbol     :: Text           -- ^ Symbol name
  , srModule     :: Text           -- ^ Module containing the reference
  , srFile       :: FilePath       -- ^ File containing the reference
  , srSpan       :: SrcSpan        -- ^ Location of the reference
  , srKind       :: ReferenceKind  -- ^ Kind of reference
  , srQualified  :: Maybe Text     -- ^ Qualifier used (if any)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | Kind of symbol reference
data ReferenceKind
  = RefUsage           -- ^ Normal usage
  | RefDefinition      -- ^ Definition site
  | RefImport          -- ^ In import list
  | RefExport          -- ^ In export list
  | RefTypeSignature   -- ^ In type signature
  | RefInstanceHead    -- ^ In instance head
  | RefPattern         -- ^ In pattern
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)
