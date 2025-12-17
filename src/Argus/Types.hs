{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Types
-- Description : Core types for the Argus static analyzer
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
-- Portability : GHC
--
-- = Overview
--
-- This module defines the fundamental data types used throughout Argus.
-- It is the foundation of the type system and should be imported by virtually
-- all other Argus modules.
--
-- = Architecture
--
-- The types in this module are organized into several categories:
--
-- * __Source Locations__: "Argus.Types.SrcSpan", "Argus.Types.SrcLoc", "Argus.Types.Line", "Argus.Types.Column" for precise
--   source code positioning
-- * __Diagnostics__: "Argus.Types.Diagnostic", "Argus.Types.Severity", "Argus.Types.DiagnosticKind" for reporting issues
-- * __Fixes__: "Argus.Types.Fix", "Argus.Types.FixEdit", "Argus.Types.FixImport" for auto-fix functionality
-- * __Symbols__: "Argus.Types.Symbol", "Argus.Types.QualifiedName", "Argus.Types.SymbolKind" for code navigation
-- * __Results__: "Argus.Types.AnalysisResult", "Argus.Types.FileResult" for aggregating analysis output
-- * __Configuration__: "Argus.Types.ArgusOptions", "Argus.Types.AnalysisMode" for runtime configuration
--
-- = Key Invariants
--
-- * All line and column numbers are __1-indexed__ (matching editor conventions)
-- * @SrcSpan@ ranges are __inclusive__ on the start and __exclusive__ on the end
-- * Empty @SrcSpan@ values should use @noSrcSpan@, not arbitrary values
-- * @Fix@ edits should never be empty; use @mkFixSafe@ for safe construction
--
-- = Serialization
--
-- All exported types support JSON serialization via @ToJSON@ and @FromJSON@
-- instances, making them suitable for:
--
-- * SARIF output
-- * LSP communication
-- * Baseline persistence
-- * Inter-process communication
--
-- = Thread Safety
--
-- All types are immutable and safe for concurrent access. Strict fields
-- (@StrictData@ pragma) ensure predictable memory usage.
--
-- = Example
--
-- @
-- -- Creating a diagnostic with a fix
-- let span = mkSrcSpanRaw "src/Main.hs" 10 5 10 15
--     edit = FixEdit span "newCode"
--     fix = mkFix "Replace with newCode" [edit] True
--     diag = Diagnostic
--       { diagSpan = span
--       , diagSeverity = Warning
--       , diagKind = CodePattern
--       , diagMessage = "Consider using newCode"
--       , diagCode = Just "ARGUS-001"
--       , diagFixes = [fix]
--       , diagRelated = []
--       }
-- @
--
-- @since 1.0.0
module Argus.Types
  ( -- * Diagnostics
    Diagnostic (..)
  , DiagnosticKind (..)
  , Severity (..)
  , Fix (..)
  , FixEdit (..)
  , mkFixSafe
  , mkFix
  , mkFixWithImports

    -- * Import Management for Fixes
  , FixImport (..)
  , ImportSymbol (..)
  , ImportSymbolType (..)
  , FixCategory (..)
  , FixSafety (..)
  , mkFixImport
  , mkImportSymbol

    -- * Source locations
  , SrcSpan (..)
  , SrcLoc (..)
  , Line (..)
  , Column (..)
  , srcSpanStart
  , srcSpanEnd
  , mkSrcSpan
  , mkSrcSpanRaw
  , noSrcSpan
  , srcSpanStartLineRaw
  , srcSpanStartColRaw
  , srcSpanEndLineRaw
  , srcSpanEndColRaw

    -- * Symbol information
  , Symbol (..)
  , SymbolKind (..)
  , QualifiedName (..)
  , mkQualifiedName

    -- * Analysis results
  , AnalysisResult (..)
  , FileResult (..)
  , emptyAnalysisResult
  , mergeResults

    -- * Mode and options
  , AnalysisMode (..)
  , Verbosity (..)
  , isQuiet
  , isVerbose
  , isDebug
  , ArgusOptions (..)
  , defaultOptions

    -- * Time types
  , Seconds (..)
  , Milliseconds (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..), FromJSON (..), ToJSONKey (..), FromJSONKey (..), (.=), (.:), (.:?), (.!=), object, withObject, withText)
import Data.Hashable (Hashable (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

--------------------------------------------------------------------------------
-- Newtypes for Type Safety
--------------------------------------------------------------------------------

-- | Line number in a source file.
--
-- __Invariant__: Line numbers are 1-indexed, matching editor conventions.
-- Line 1 is the first line of the file.
--
-- Use @Line 0@ only for synthetic/generated code that has no source location.
--
-- @since 1.0.0
newtype Line = Line
  { unLine :: Int
    -- ^ Extract the raw line number. Prefer pattern matching or using
    -- @srcSpanStartLineRaw@ when interacting with external APIs.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Enum, ToJSON, FromJSON, Hashable, NFData)

-- | Column number in a source file.
--
-- __Invariant__: Column numbers are 1-indexed. Column 1 is the first character
-- of a line (not column 0).
--
-- __Note__: Column positions count Unicode code points, not bytes or grapheme
-- clusters. This matches GHC's internal representation.
--
-- @since 1.0.0
newtype Column = Column
  { unColumn :: Int
    -- ^ Extract the raw column number. Prefer pattern matching or using
    -- @srcSpanStartColRaw@ when interacting with external APIs.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Enum, ToJSON, FromJSON, Hashable, NFData)

-- | Time duration in seconds.
--
-- Used for performance measurements, timeouts, and profiling data.
-- Supports fractional values for sub-second precision.
--
-- @since 1.0.0
newtype Seconds = Seconds
  { unSeconds :: Double
    -- ^ Extract the raw duration in seconds.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Fractional, Real, RealFrac, ToJSON, FromJSON, NFData)

-- | Time duration in milliseconds.
--
-- Used for fine-grained timing, debouncing, and rule execution metrics.
--
-- @since 1.0.0
newtype Milliseconds = Milliseconds
  { unMilliseconds :: Double
    -- ^ Extract the raw duration in milliseconds.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Fractional, Real, RealFrac, ToJSON, FromJSON, NFData)

--------------------------------------------------------------------------------
-- Source Locations
--------------------------------------------------------------------------------

-- | A single position in a source file.
--
-- Represents a point (not a range) in source code. For ranges, use @SrcSpan@.
--
-- __Invariants__:
--
-- * @srcLocLine@ and @srcLocColumn@ are 1-indexed
-- * @srcLocFile@ should be an absolute path when possible for reliable comparisons
--
-- @since 1.0.0
data SrcLoc = SrcLoc
  { srcLocFile   :: !FilePath
    -- ^ Path to the source file. May be relative or absolute depending on context.
  , srcLocLine   :: {-# UNPACK #-} !Line
    -- ^ Line number (1-indexed).
  , srcLocColumn :: {-# UNPACK #-} !Column
    -- ^ Column number (1-indexed).
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | A contiguous span of source code.
--
-- Represents a range from a start position to an end position within a single file.
-- This is the primary type for identifying code regions in diagnostics and fixes.
--
-- __Invariants__:
--
-- * Start position must be before or equal to end position
-- * All line\/column values are 1-indexed
-- * The range is __inclusive__ at the start and __exclusive__ at the end
--   (i.e., @[start, end)@)
--
-- __Construction__:
--
-- * Use @mkSrcSpan@ when you have @SrcLoc@ values
-- * Use @mkSrcSpanRaw@ when you have raw @Int@ values (e.g., from GHC API)
-- * Use @noSrcSpan@ for generated code without source location
--
-- __Ordering__: @SrcSpan@ values are ordered first by file, then by start line,
-- then by start column, then by end line, then by end column. This provides
-- a stable sort order for diagnostics.
--
-- @since 1.0.0
data SrcSpan = SrcSpan
  { srcSpanFile      :: !FilePath
    -- ^ Path to the source file containing this span.
  , srcSpanStartLine :: {-# UNPACK #-} !Line
    -- ^ Starting line number (1-indexed, inclusive).
  , srcSpanStartCol  :: {-# UNPACK #-} !Column
    -- ^ Starting column number (1-indexed, inclusive).
  , srcSpanEndLine   :: {-# UNPACK #-} !Line
    -- ^ Ending line number (1-indexed, inclusive for the line).
  , srcSpanEndCol    :: {-# UNPACK #-} !Column
    -- ^ Ending column number (1-indexed, exclusive).
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, NFData)

-- | Extract the start location from a span.
--
-- __Postcondition__: The returned @SrcLoc@ will have the same file and
-- the start line\/column of the span.
--
-- @since 1.0.0
srcSpanStart :: SrcSpan -> SrcLoc
srcSpanStart SrcSpan{..} = SrcLoc srcSpanFile srcSpanStartLine srcSpanStartCol

-- | Extract the end location from a span.
--
-- __Postcondition__: The returned @SrcLoc@ will have the same file and
-- the end line\/column of the span.
--
-- @since 1.0.0
srcSpanEnd :: SrcSpan -> SrcLoc
srcSpanEnd SrcSpan{..} = SrcLoc srcSpanFile srcSpanEndLine srcSpanEndCol

-- | Create a source span from start and end locations.
--
-- __Precondition__: Both locations should refer to the same file.
-- If they don't, the start location's file is used.
--
-- __Precondition__: Start should be before or equal to end.
-- This is not enforced but violating it may cause unexpected behavior.
--
-- @since 1.0.0
mkSrcSpan :: SrcLoc -> SrcLoc -> SrcSpan
mkSrcSpan start end = SrcSpan
  { srcSpanFile      = srcLocFile start
  , srcSpanStartLine = srcLocLine start
  , srcSpanStartCol  = srcLocColumn start
  , srcSpanEndLine   = srcLocLine end
  , srcSpanEndCol    = srcLocColumn end
  }

-- | Sentinel value representing a missing or synthetic source span.
--
-- Use this for:
--
-- * Generated code that has no source location
-- * Diagnostics about the project as a whole (not specific code)
-- * Placeholder values before real spans are computed
--
-- __Note__: Code should check for @noSrcSpan@ before displaying locations
-- to users, as "line 0, column 0" is confusing.
--
-- @since 1.0.0
noSrcSpan :: SrcSpan
noSrcSpan = SrcSpan "" (Line 0) (Column 0) (Line 0) (Column 0)

-- | Create a @SrcSpan@ from raw @Int@ values.
--
-- This is the preferred constructor when interfacing with the GHC API
-- or other systems that use raw integers for positions.
--
-- __Precondition__: All values should be positive (1-indexed).
-- Zero or negative values should only be used for @noSrcSpan@-like sentinels.
--
-- @since 1.0.0
mkSrcSpanRaw :: FilePath  -- ^ Source file path
             -> Int       -- ^ Start line (1-indexed)
             -> Int       -- ^ Start column (1-indexed)
             -> Int       -- ^ End line (1-indexed)
             -> Int       -- ^ End column (1-indexed, exclusive)
             -> SrcSpan
mkSrcSpanRaw file sl sc el ec = SrcSpan file (Line sl) (Column sc) (Line el) (Column ec)

-- | Extract start line as raw @Int@.
--
-- Use when interfacing with APIs that require plain integers.
--
-- @since 1.0.0
srcSpanStartLineRaw :: SrcSpan -> Int
srcSpanStartLineRaw = unLine . srcSpanStartLine

-- | Extract start column as raw @Int@.
--
-- Use when interfacing with APIs that require plain integers.
--
-- @since 1.0.0
srcSpanStartColRaw :: SrcSpan -> Int
srcSpanStartColRaw = unColumn . srcSpanStartCol

-- | Extract end line as raw @Int@.
--
-- Use when interfacing with APIs that require plain integers.
--
-- @since 1.0.0
srcSpanEndLineRaw :: SrcSpan -> Int
srcSpanEndLineRaw = unLine . srcSpanEndLine

-- | Extract end column as raw @Int@.
--
-- Use when interfacing with APIs that require plain integers.
--
-- @since 1.0.0
srcSpanEndColRaw :: SrcSpan -> Int
srcSpanEndColRaw = unColumn . srcSpanEndCol

--------------------------------------------------------------------------------
-- Severity and Diagnostic Kinds
--------------------------------------------------------------------------------

-- | Severity level of a diagnostic.
--
-- Severity levels are ordered from most severe (@Error@) to least severe (@Info@).
-- The @Ord@ instance reflects this ordering, with @Error < Warning < Suggestion < Info@.
--
-- __CI Integration__:
--
-- * Exit code 1: If any @Error@ diagnostics are present
-- * Exit code 0: Otherwise (warnings and below are advisory)
--
-- __Filtering__: Users can filter diagnostics by minimum severity using
-- the @--fail-on-severity@ flag.
--
-- @since 1.0.0
data Severity
  = Error
    -- ^ Critical issue that must be fixed. Typically indicates code that will
    -- fail at runtime or has serious security implications.
  | Warning
    -- ^ Issue that should be fixed. Indicates likely bugs, performance problems,
    -- or code that violates best practices.
  | Suggestion
    -- ^ Improvement opportunity. Code works correctly but could be cleaner,
    -- more idiomatic, or more performant.
  | Info
    -- ^ Informational message. No action required; used for metrics,
    -- style preferences, or educational hints.
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey, NFData)

-- | Classification of diagnostic issues.
--
-- Each diagnostic is categorized by kind to enable:
--
-- * Filtering diagnostics by category in the CLI
-- * Grouping related issues in reports
-- * Applying category-specific suppressions
--
-- The @Custom@ constructor allows plugins and custom rules to define
-- their own categories.
--
-- @since 1.0.0
data DiagnosticKind
  = NamingConvention
    -- ^ Naming convention violation (e.g., camelCase for functions,
    -- PascalCase for types).
  | UnusedCode
    -- ^ Dead code that is never executed or referenced.
  | UnusedImport
    -- ^ Import statement that brings in symbols not used in the module.
  | RedundantCode
    -- ^ Code that can be simplified without changing behavior
    -- (e.g., @id x@ → @x@).
  | CodePattern
    -- ^ Code pattern that has a better alternative
    -- (e.g., @foldl@ → @foldl'@).
  | TypeSignature
    -- ^ Missing or problematic type signature.
  | ImportStyle
    -- ^ Import organization or qualification issue.
  | TemplateHaskellRef
    -- ^ Template Haskell cross-reference tracking (for splice analysis).
  | SecurityIssue
    -- ^ Security vulnerability (e.g., SQL injection, hardcoded secrets).
  | PerformanceIssue
    -- ^ Performance anti-pattern (e.g., O(n²) complexity, unnecessary allocations).
  | ArchitecturalIssue
    -- ^ Module architecture problem (e.g., layer violation, cyclic dependency).
  | SpaceLeak
    -- ^ Potential space leak (e.g., lazy accumulator, thunk buildup).
  | PartialFunction
    -- ^ Use of a partial function that can crash at runtime
    -- (e.g., @head@, \"fromJust\").
  | ComplexityIssue
    -- ^ Excessive code complexity (cyclomatic complexity, nesting depth).
  | Custom Text
    -- ^ User-defined diagnostic kind from plugins or custom rules.
    -- The @Text@ value is the category name.
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, NFData)

--------------------------------------------------------------------------------
-- Import Symbol Types (for Fix Import Management)
--------------------------------------------------------------------------------

-- | Classification of symbols for import statement generation.
--
-- When a fix requires adding an import, this type tells the import manager
-- how to format the import correctly (e.g., operators need parentheses).
--
-- @since 1.0.0
data ImportSymbolType
  = ISTFunction
    -- ^ Regular function or value. Imported as-is: @import M (foo)@
  | ISTOperator
    -- ^ Infix operator. Imported with parentheses: @import M ((<>))@
  | ISTType
    -- ^ Type or type synonym. May need @type@ keyword in explicit imports.
  | ISTClass
    -- ^ Type class. Supports importing methods: @import M (Class(..))@
  | ISTConstructor
    -- ^ Data constructor. Associated with a parent type.
  | ISTPattern
    -- ^ Pattern synonym. Requires @pattern@ keyword: @import M (pattern P)@
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

instance ToJSON ImportSymbolType where
  toJSON ISTFunction    = "function"
  toJSON ISTOperator    = "operator"
  toJSON ISTType        = "type"
  toJSON ISTClass       = "class"
  toJSON ISTConstructor = "constructor"
  toJSON ISTPattern     = "pattern"

instance FromJSON ImportSymbolType where
  parseJSON = withText "ImportSymbolType" $ \case
    "function"    -> pure ISTFunction
    "operator"    -> pure ISTOperator
    "type"        -> pure ISTType
    "class"       -> pure ISTClass
    "constructor" -> pure ISTConstructor
    "pattern"     -> pure ISTPattern
    other         -> fail $ "Unknown ImportSymbolType: " <> T.unpack other

-- | A single symbol to be imported.
--
-- Represents one item in an import list, with enough information to
-- generate syntactically correct import statements.
--
-- __Example__:
--
-- @
-- -- Import a function
-- ImportSymbol "foldl'" ISTFunction []
--
-- -- Import an operator
-- ImportSymbol "<>" ISTOperator []
--
-- -- Import a type with constructors
-- ImportSymbol "Maybe" ISTType ["Just", "Nothing"]
-- @
--
-- @since 1.0.0
data ImportSymbol = ImportSymbol
  { isymName     :: Text
    -- ^ Symbol name (e.g., @"foldl'"@, @"Map"@, @"(<>)"@).
    -- For operators, include the operator without parentheses.
  , isymType     :: ImportSymbolType
    -- ^ Classification for correct import formatting.
  , isymChildren :: [Text]
    -- ^ Child items for types\/classes (constructors, methods).
    -- Use @[".."]@ for wildcard imports.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

instance ToJSON ImportSymbol where
  toJSON ImportSymbol{..} = object
    [ "name"     .= isymName
    , "type"     .= isymType
    , "children" .= isymChildren
    ]

instance FromJSON ImportSymbol where
  parseJSON = withObject "ImportSymbol" $ \o -> do
    isymName     <- o .: "name"
    isymType     <- o .:? "type" .!= ISTFunction
    isymChildren <- o .:? "children" .!= []
    pure ImportSymbol{..}

-- | Create an @ImportSymbol@ with no child items.
--
-- Use this for simple imports of functions, operators, or types without
-- explicit constructor\/method lists.
--
-- @since 1.0.0
mkImportSymbol :: Text             -- ^ Symbol name
               -> ImportSymbolType -- ^ Symbol classification
               -> ImportSymbol
mkImportSymbol name symType = ImportSymbol
  { isymName = name
  , isymType = symType
  , isymChildren = []
  }

-- | Complete specification for an import statement.
--
-- This type contains all information needed to generate or modify an import:
--
-- * Which module to import from
-- * Which symbols to import (or if it's a hiding import)
-- * Whether the import should be qualified
-- * Package disambiguation (for duplicate module names)
--
-- __Example__:
--
-- @
-- -- import Data.List (foldl')
-- FixImport "Data.List" [mkImportSymbol "foldl'" ISTFunction] Nothing False Nothing
--
-- -- import qualified Data.Map as M
-- FixImport "Data.Map" [] (Just "M") False Nothing
-- @
--
-- @since 1.0.0
data FixImport = FixImport
  { fimpModule    :: Text
    -- ^ Module to import (e.g., @"Data.Foldable"@).
  , fimpSymbols   :: [ImportSymbol]
    -- ^ Specific symbols to import. Empty list means import the whole module.
  , fimpQualified :: Maybe Text
    -- ^ Qualifier for qualified imports. @Just "F"@ generates @as F@.
  , fimpHiding    :: Bool
    -- ^ If @True@, this is a hiding import that excludes the listed symbols.
  , fimpPackage   :: Maybe Text
    -- ^ Package name for disambiguation when multiple packages export the
    -- same module name. Generates @import "package" Module@.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

instance ToJSON FixImport where
  toJSON FixImport{..} = object
    [ "module"    .= fimpModule
    , "symbols"   .= fimpSymbols
    , "qualified" .= fimpQualified
    , "hiding"    .= fimpHiding
    , "package"   .= fimpPackage
    ]

instance FromJSON FixImport where
  parseJSON = withObject "FixImport" $ \o -> do
    fimpModule    <- o .: "module"
    fimpSymbols   <- o .:? "symbols" .!= []
    fimpQualified <- o .:? "qualified"
    fimpHiding    <- o .:? "hiding" .!= False
    fimpPackage   <- o .:? "package"
    pure FixImport{..}

-- | Create a simple unqualified import.
--
-- For qualified imports or hiding imports, construct @FixImport@ directly.
--
-- @since 1.0.0
mkFixImport :: Text           -- ^ Module name
            -> [ImportSymbol] -- ^ Symbols to import
            -> FixImport
mkFixImport modName symbols = FixImport
  { fimpModule = modName
  , fimpSymbols = symbols
  , fimpQualified = Nothing
  , fimpHiding = False
  , fimpPackage = Nothing
  }

--------------------------------------------------------------------------------
-- Fix Category and Safety
--------------------------------------------------------------------------------

-- | Category of a fix for filtering and organization.
--
-- Allows users to selectively apply fixes by category:
--
-- @
-- # Apply only safety fixes
-- argus fix --category safety src/
--
-- # Apply all performance improvements
-- argus fix --category performance src/
-- @
--
-- @since 1.0.0
data FixCategory
  = FCPerformance
    -- ^ Performance improvement (e.g., @foldl@ → @foldl'@, strictness).
  | FCModernize
    -- ^ Modernization using newer APIs (e.g., @Monad m => m@ → @Applicative f => f@).
  | FCSafety
    -- ^ Safety improvement replacing partial functions with total alternatives.
  | FCStyle
    -- ^ Code style improvement for readability and consistency.
  | FCImports
    -- ^ Import organization and cleanup.
  | FCRedundant
    -- ^ Removal of redundant code (e.g., @id x@ → @x@).
  | FCSpaceLeaks
    -- ^ Space leak fixes (strictness, bang patterns).
  | FCSecurity
    -- ^ Security improvement (input validation, safe operations).
  | FCCustom Text
    -- ^ User-defined category from plugins or custom rules.
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

instance ToJSON FixCategory where
  toJSON FCPerformance   = "performance"
  toJSON FCModernize     = "modernize"
  toJSON FCSafety        = "safety"
  toJSON FCStyle         = "style"
  toJSON FCImports       = "imports"
  toJSON FCRedundant     = "redundant"
  toJSON FCSpaceLeaks    = "space-leaks"
  toJSON FCSecurity      = "security"
  toJSON (FCCustom name) = toJSON name

instance FromJSON FixCategory where
  parseJSON = withText "FixCategory" $ \case
    "performance"  -> pure FCPerformance
    "modernize"    -> pure FCModernize
    "safety"       -> pure FCSafety
    "style"        -> pure FCStyle
    "imports"      -> pure FCImports
    "redundant"    -> pure FCRedundant
    "space-leaks"  -> pure FCSpaceLeaks
    "security"     -> pure FCSecurity
    other          -> pure $ FCCustom other

-- | Safety classification for automatic fix application.
--
-- Determines whether a fix can be applied automatically or requires review.
-- The @--safe-only@ flag restricts fixes to @FSAlways@ safety level.
--
-- __Ordering__: @FSAlways < FSMostly < FSReview < FSUnsafe@
-- (safest to least safe).
--
-- @since 1.0.0
data FixSafety
  = FSAlways
    -- ^ Always safe to apply automatically. The fix is guaranteed not to
    -- change program semantics. Example: @id x@ → @x@.
  | FSMostly
    -- ^ Safe in the vast majority of cases; rare edge cases may need review.
    -- Example: @foldl@ → @foldl'@ (safe unless laziness is intentional).
  | FSReview
    -- ^ Requires human review before applying. The fix is likely correct
    -- but context-dependent. Example: removing apparently unused code.
  | FSUnsafe
    -- ^ May change semantics. Should only be applied with explicit user
    -- confirmation. Example: changing evaluation order.
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, NFData)

instance ToJSON FixSafety where
  toJSON FSAlways = "safe"
  toJSON FSMostly = "mostly-safe"
  toJSON FSReview = "review"
  toJSON FSUnsafe = "unsafe"

instance FromJSON FixSafety where
  parseJSON = withText "FixSafety" $ \case
    "safe"        -> pure FSAlways
    "mostly-safe" -> pure FSMostly
    "review"      -> pure FSReview
    "unsafe"      -> pure FSUnsafe
    _             -> pure FSAlways  -- Default to safe

--------------------------------------------------------------------------------
-- Fixes
--------------------------------------------------------------------------------

-- | A single edit to apply to source code.
--
-- Represents the atomic unit of code transformation: replacing the text
-- at a specific @SrcSpan@ with new text.
--
-- __Invariants__:
--
-- * @fixEditSpan@ must be a valid span (not @noSrcSpan@ for real edits)
-- * @fixEditNewText@ may be empty to represent deletion
-- * Multiple @FixEdit@s in a single @Fix@ must not have overlapping spans
--
-- __Construction__:
--
-- Simply use the @FixEdit@ constructor directly:
--
-- @
-- -- Replace "foldl" with "foldl'" at the given span
-- FixEdit span "foldl'"
--
-- -- Delete the text at the span
-- FixEdit span ""
--
-- -- Insert text (use a zero-width span)
-- FixEdit (mkSrcSpanRaw file line col line col) "inserted text"
-- @
--
-- __Application Order__:
--
-- When a @Fix@ contains multiple edits, they are applied in reverse order
-- by position (end to start) to preserve span validity during transformation.
--
-- @since 1.0.0
data FixEdit = FixEdit
  { fixEditSpan    :: SrcSpan
    -- ^ The source span to replace. The entire text within this span
    -- will be replaced with @fixEditNewText@.
  , fixEditNewText :: Text
    -- ^ The replacement text. May be empty for deletions.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, NFData)

-- | A complete fix that can be applied to source code.
--
-- A fix consists of one or more text edits, optional import management,
-- and metadata for categorization and safety classification.
--
-- __Invariants__:
--
-- * @fixEdits@ should never be empty (use @mkFixSafe@ to enforce this)
-- * Edits within a fix must not have overlapping spans
-- * All edits must target the same file (multi-file fixes use @MultiFileFix@)
--
-- __Lifecycle__:
--
-- 1. Created by rule matchers when issues are detected
-- 2. Attached to @Diagnostic@ via @diagFixes@
-- 3. Presented to users for review or auto-application
-- 4. Applied by "Argus.Refactor.SafeRefactor" with validation
--
-- __Import Management__:
--
-- Fixes can automatically manage imports:
--
-- @
-- -- A fix that replaces 'foldl' with 'foldl'' and adds the import
-- Fix
--   { fixTitle = "Use strict foldl'"
--   , fixEdits = [FixEdit span "foldl'"]
--   , fixIsPreferred = True
--   , fixAddImports = [mkFixImport "Data.Foldable" [mkImportSymbol "foldl'" ISTFunction]]
--   , fixRemoveImports = ["foldl"]
--   , fixCategory = FCPerformance
--   , fixSafety = FSMostly
--   }
-- @
--
-- __Construction__:
--
-- * @mkFixSafe@: Safe constructor returning @Maybe@ for empty edits
-- * @mkFix@: Simple constructor with defaults
-- * @mkFixWithImports@: Full constructor with all fields
--
-- @since 1.0.0
data Fix = Fix
  { fixTitle         :: Text
    -- ^ Human-readable title displayed to users.
    -- Should be concise and action-oriented (e.g., \"Use foldl'\").
  , fixEdits         :: [FixEdit]
    -- ^ Edits to apply. Must be non-empty (use @mkFixSafe@ to enforce).
  , fixIsPreferred   :: Bool
    -- ^ If @True@, this fix is recommended and may be auto-applied.
    -- Only one fix per diagnostic should be marked as preferred.
  , fixAddImports    :: [FixImport]
    -- ^ Imports to add when the fix is applied.
  , fixRemoveImports :: [Text]
    -- ^ Import symbols that become unused after applying this fix.
    -- The import manager will clean these up.
  , fixCategory      :: FixCategory
    -- ^ Category for filtering fixes by type.
  , fixSafety        :: FixSafety
    -- ^ Safety level determining auto-application eligibility.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON Fix where
  toJSON Fix{..} = object
    [ "fixTitle"         .= fixTitle
    , "fixEdits"         .= fixEdits
    , "fixIsPreferred"   .= fixIsPreferred
    , "fixAddImports"    .= fixAddImports
    , "fixRemoveImports" .= fixRemoveImports
    , "fixCategory"      .= fixCategory
    , "fixSafety"        .= fixSafety
    ]

instance FromJSON Fix where
  parseJSON = withObject "Fix" $ \o -> do
    fixTitle         <- o .: "fixTitle"
    fixEdits         <- o .: "fixEdits"
    fixIsPreferred   <- o .: "fixIsPreferred"
    -- Backward compatibility: these fields are optional with defaults
    fixAddImports    <- o .:? "fixAddImports" .!= []
    fixRemoveImports <- o .:? "fixRemoveImports" .!= []
    fixCategory      <- o .:? "fixCategory" .!= FCStyle
    fixSafety        <- o .:? "fixSafety" .!= FSAlways
    pure Fix{..}

-- | Smart constructor for @Fix@ that validates non-empty edits.
--
-- Returns @Nothing@ if the edits list is empty, enforcing the invariant
-- that fixes must contain at least one edit.
--
-- Uses default values for optional fields:
--
-- * @fixAddImports@: @[]@
-- * @fixRemoveImports@: @[]@
-- * @fixCategory@: @FCStyle@
-- * @fixSafety@: @FSAlways@
--
-- __Example__:
--
-- @
-- case mkFixSafe "Remove redundant id" [FixEdit span "x"] True of
--   Just fix -> pure fix
--   Nothing  -> error "BUG: empty edit list"
-- @
--
-- @since 1.0.0
mkFixSafe :: Text      -- ^ Fix title
          -> [FixEdit] -- ^ Edits to apply (must be non-empty)
          -> Bool      -- ^ Is this the preferred fix?
          -> Maybe Fix -- ^ @Nothing@ if edits is empty
mkFixSafe _ [] _ = Nothing
mkFixSafe title edits preferred = Just Fix
  { fixTitle = title
  , fixEdits = edits
  , fixIsPreferred = preferred
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSAlways
  }

-- | Simple @Fix@ constructor for common use cases.
--
-- Creates a fix with empty import management and default safety.
-- Use @mkFixWithImports@ when you need import management or custom categories.
--
-- __Note__: Does not validate that edits is non-empty. Use @mkFixSafe@
-- if you need this validation.
--
-- __Example__:
--
-- @
-- -- Simple style fix
-- mkFix "Remove redundant parens" [FixEdit span expr] True
-- @
--
-- @since 1.0.0
mkFix :: Text      -- ^ Fix title
      -> [FixEdit] -- ^ Edits to apply
      -> Bool      -- ^ Is this the preferred fix?
      -> Fix
mkFix title edits preferred = Fix
  { fixTitle = title
  , fixEdits = edits
  , fixIsPreferred = preferred
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSAlways
  }

-- | Full @Fix@ constructor with import management and categorization.
--
-- Use this when you need to:
--
-- * Add imports required by the replacement code
-- * Remove imports that become unused after the fix
-- * Categorize the fix for filtering (e.g., performance, safety)
-- * Specify a non-default safety level
--
-- __Example__:
--
-- @
-- -- Replace foldl with foldl' and manage imports
-- mkFixWithImports
--   "Use strict foldl'"
--   [FixEdit span "foldl'"]
--   True  -- preferred
--   [mkFixImport "Data.Foldable" [mkImportSymbol "foldl'" ISTFunction]]
--   ["foldl"]  -- old import becomes unused
--   FCPerformance
--   FSMostly
-- @
--
-- @since 1.0.0
mkFixWithImports :: Text         -- ^ Fix title
                 -> [FixEdit]    -- ^ Edits to apply
                 -> Bool         -- ^ Is this the preferred fix?
                 -> [FixImport]  -- ^ Imports to add when fix is applied
                 -> [Text]       -- ^ Import symbols that become unused
                 -> FixCategory  -- ^ Category for filtering
                 -> FixSafety    -- ^ Safety level
                 -> Fix
mkFixWithImports title edits preferred addImps remImps cat safety = Fix
  { fixTitle = title
  , fixEdits = edits
  , fixIsPreferred = preferred
  , fixAddImports = addImps
  , fixRemoveImports = remImps
  , fixCategory = cat
  , fixSafety = safety
  }

--------------------------------------------------------------------------------
-- Diagnostics
--------------------------------------------------------------------------------

-- | A diagnostic message representing an issue found during analysis.
--
-- Diagnostics are the primary output of Argus analysis, representing
-- issues, warnings, and suggestions found in the codebase.
--
-- __Lifecycle__:
--
-- 1. Created by rules when they match problematic code patterns
-- 2. Collected during file analysis
-- 3. Filtered by severity, category, or suppression
-- 4. Rendered to the user via chosen output format
-- 5. Optionally, fixes are applied to resolve the issue
--
-- __Invariants__:
--
-- * @diagSpan@ should be a valid span (not @noSrcSpan@) for real diagnostics
-- * @diagMessage@ should be a complete, grammatically correct sentence
-- * If @diagFixes@ is non-empty, at most one fix should have @fixIsPreferred@
--
-- __Related Locations__:
--
-- Use @diagRelated@ to provide context for complex issues:
--
-- @
-- Diagnostic
--   { diagSpan = usageSpan
--   , diagMessage = \"Variable \'x\' is used before definition\"
--   , diagRelated = [(definitionSpan, "Variable is defined here")]
--   , ...
--   }
-- @
--
-- __Suppression__:
--
-- Diagnostics can be suppressed via:
--
-- * Inline comments: @{- argus:ignore RULE-CODE -}@
-- * Baseline files for legacy code
-- * Configuration file exclusions
--
-- @since 1.0.0
data Diagnostic = Diagnostic
  { diagSpan      :: SrcSpan
    -- ^ Source location of the issue. Points to the problematic code.
  , diagSeverity  :: Severity
    -- ^ Severity level. Affects exit code and filtering.
  , diagKind      :: DiagnosticKind
    -- ^ Category of the diagnostic for filtering and grouping.
  , diagMessage   :: Text
    -- ^ Human-readable explanation of the issue.
    -- Should be clear enough to understand without seeing the code.
  , diagCode      :: Maybe Text
    -- ^ Optional rule code (e.g., \"ARGUS-001\", \"partial\/head\").
    -- Used for suppression and documentation lookup.
  , diagFixes     :: [Fix]
    -- ^ Available automatic fixes. May be empty if no fix is possible.
  , diagRelated   :: [(SrcSpan, Text)]
    -- ^ Related source locations with explanatory messages.
    -- Useful for issues spanning multiple locations.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON Diagnostic where
  toJSON Diagnostic{..} = object
    [ "span"     .= diagSpan
    , "severity" .= diagSeverity
    , "kind"     .= diagKind
    , "message"  .= diagMessage
    , "code"     .= diagCode
    , "fixes"    .= diagFixes
    , "related"  .= diagRelated
    ]

instance FromJSON Diagnostic where
  parseJSON = withObject "Diagnostic" $ \o -> do
    diagSpan     <- o .: "span"
    diagSeverity <- o .: "severity"
    diagKind     <- o .: "kind"
    diagMessage  <- o .: "message"
    diagCode     <- o .:? "code"
    diagFixes    <- o .: "fixes"
    diagRelated  <- o .: "related"
    pure Diagnostic{..}

--------------------------------------------------------------------------------
-- Symbols
--------------------------------------------------------------------------------

-- | Classification of Haskell symbols.
--
-- Used for navigation, go-to-definition, and symbol search functionality.
-- The classification affects how symbols are displayed and filtered.
--
-- @since 1.0.0
data SymbolKind
  = Function
    -- ^ Regular function or value binding.
  | TypeConstructor
    -- ^ Type or newtype name (the @T@ in @data T = ...@).
  | DataConstructor
    -- ^ Data constructor (the @C@ in @data T = C ...@).
  | TypeClass
    -- ^ Type class name.
  | TypeClassMethod
    -- ^ Method within a type class.
  | TypeFamily
    -- ^ Type family or data family.
  | PatternSynonym
    -- ^ Pattern synonym.
  | Module
    -- ^ Module name (used for cross-module analysis).
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | A qualified name consisting of optional module and symbol name.
--
-- Used to uniquely identify symbols across the codebase.
--
-- __Examples__:
--
-- @
-- QualifiedName (Just "Data.List") "foldl'"  -- Data.List.foldl'
-- QualifiedName Nothing "localHelper"         -- Local definition
-- @
--
-- __Ordering__: Lexicographic by module (with @Nothing@ first), then by name.
--
-- @since 1.0.0
data QualifiedName = QualifiedName
  { qnModule :: Maybe Text
    -- ^ Module name, or @Nothing@ for local bindings.
  , qnName   :: Text
    -- ^ The symbol name itself.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | Create a @QualifiedName@.
--
-- Simple constructor wrapper for convenience.
--
-- @since 1.0.0
mkQualifiedName :: Maybe Text -- ^ Module name (@Nothing@ for local)
                -> Text       -- ^ Symbol name
                -> QualifiedName
mkQualifiedName = QualifiedName

-- | A symbol definition in the codebase.
--
-- Represents a named entity that can be referenced, navigated to,
-- or analyzed for usage. Symbols are extracted from HIE files for
-- accurate type information.
--
-- __Lifecycle__:
--
-- 1. Extracted from HIE files during indexing
-- 2. Stored in the symbol table for lookup
-- 3. Used for go-to-definition, find-references, unused code detection
-- 4. Aggregated in @FileResult@ and @AnalysisResult@
--
-- __Example__:
--
-- @
-- Symbol
--   { symbolName = QualifiedName (Just "MyModule") "myFunc"
--   , symbolKind = Function
--   , symbolSpan = mkSrcSpanRaw "src/MyModule.hs" 10 1 12 20
--   , symbolExported = True
--   , symbolType = Just "Int -> String -> IO ()"
--   }
-- @
--
-- @since 1.0.0
data Symbol = Symbol
  { symbolName     :: QualifiedName
    -- ^ Fully qualified name of the symbol.
  , symbolKind     :: SymbolKind
    -- ^ Classification of the symbol.
  , symbolSpan     :: SrcSpan
    -- ^ Location of the symbol's definition.
  , symbolExported :: Bool
    -- ^ Whether the symbol is exported from its module.
    -- Used for unused code detection.
  , symbolType     :: Maybe Text
    -- ^ Type signature if available from HIE data.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Hashable Symbol where
  hashWithSalt salt Symbol{..} =
    salt `hashWithSalt` symbolName `hashWithSalt` symbolKind

instance Ord Symbol where
  compare s1 s2 = compare (symbolName s1) (symbolName s2)

--------------------------------------------------------------------------------
-- Analysis Results
--------------------------------------------------------------------------------

-- | Analysis results for a single source file.
--
-- Contains all diagnostics, symbols, and metadata extracted from one file.
-- Multiple @FileResult@s are combined into an @AnalysisResult@.
--
-- __Lifecycle__:
--
-- 1. Created during file analysis (syntactic or semantic)
-- 2. Populated with diagnostics from rule matching
-- 3. Enriched with symbol information from HIE (in @FullMode@)
-- 4. Aggregated into @AnalysisResult@ for reporting
--
-- @since 1.0.0
data FileResult = FileResult
  { fileResultPath        :: FilePath
    -- ^ Path to the analyzed file. Should be consistent (relative or absolute)
    -- with other paths in the analysis.
  , fileResultDiagnostics :: [Diagnostic]
    -- ^ All diagnostics found in this file, sorted by position.
  , fileResultSymbols     :: [Symbol]
    -- ^ Symbols defined in this file (functions, types, etc.).
  , fileResultImports     :: [QualifiedName]
    -- ^ Modules imported by this file.
  , fileResultExports     :: [QualifiedName]
    -- ^ Symbols exported from this file.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Combined analysis result for multiple files.
--
-- Aggregates results from analyzing multiple files and provides
-- cross-file analysis information like unused code detection.
--
-- __Construction__:
--
-- * Start with @emptyAnalysisResult@
-- * Add file results using @mergeResults@
-- * Or use "Argus.Core.analyze" to get a complete result
--
-- __Thread Safety__:
--
-- @AnalysisResult@ is immutable. For parallel analysis, results are
-- merged after each worker completes.
--
-- @since 1.0.0
data AnalysisResult = AnalysisResult
  { resultFiles       :: Map.Map FilePath FileResult
    -- ^ Results indexed by file path. Use 'Map.lookup' to find
    -- results for a specific file.
  , resultUnusedCode  :: Set.Set QualifiedName
    -- ^ Symbols that are defined but never referenced anywhere
    -- in the analyzed codebase.
  , resultDiagCount   :: Map.Map Severity Int
    -- ^ Count of diagnostics by severity for summary reporting.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty analysis result with no files analyzed.
--
-- Use as the starting point for accumulating results:
--
-- @
-- results <- foldM (\\acc file -> do
--     fileResult <- analyzeFile file
--     pure $ mergeResults acc (singleFileResult fileResult))
--   emptyAnalysisResult
--   files
-- @
--
-- @since 1.0.0
emptyAnalysisResult :: AnalysisResult
emptyAnalysisResult = AnalysisResult Map.empty Set.empty Map.empty

-- | Merge two analysis results.
--
-- Combines file results, unused code sets, and diagnostic counts.
-- Used for aggregating results from parallel analysis.
--
-- __Properties__:
--
-- * Associative: @mergeResults a (mergeResults b c) == mergeResults (mergeResults a b) c@
-- * Identity: @mergeResults emptyAnalysisResult x == x@
-- * For duplicate file paths, the second result's @FileResult@ wins
--
-- @since 1.0.0
mergeResults :: AnalysisResult -> AnalysisResult -> AnalysisResult
mergeResults r1 r2 = AnalysisResult
  { resultFiles = Map.union (resultFiles r1) (resultFiles r2)
  , resultUnusedCode = Set.union (resultUnusedCode r1) (resultUnusedCode r2)
  , resultDiagCount = Map.unionWith (+) (resultDiagCount r1) (resultDiagCount r2)
  }

--------------------------------------------------------------------------------
-- Analysis Mode and Options
--------------------------------------------------------------------------------

-- | Analysis mode determining what information is available.
--
-- The analysis mode controls the depth of analysis and what prerequisites
-- are required (e.g., compiled HIE files).
--
-- __Mode Comparison__:
--
-- +-------------+------------------+---------------------+-------------------+
-- | Mode        | Prerequisites    | Analysis Depth      | Speed             |
-- +=============+==================+=====================+===================+
-- | @QuickMode@ | None             | Syntactic patterns  | Fast              |
-- +-------------+------------------+---------------------+-------------------+
-- | @FullMode@  | HIE files        | Type-aware semantic | Moderate          |
-- +-------------+------------------+---------------------+-------------------+
-- | @PluginMode@| GHC compilation  | Full TH support     | Slow (compiles)   |
-- +-------------+------------------+---------------------+-------------------+
--
-- __Ordering__: @QuickMode < FullMode < PluginMode@ (by analysis depth).
--
-- @since 1.0.0
data AnalysisMode
  = QuickMode
    -- ^ Syntactic analysis only. No compilation required.
    -- Fastest mode, suitable for editor integration and pre-commit hooks.
    -- Detects pattern-based issues but cannot perform type-aware analysis.
  | FullMode
    -- ^ HIE-based semantic analysis. Requires @.hie@ files from compilation.
    -- Enables type-aware rules, cross-module analysis, and unused code detection.
  | PluginMode
    -- ^ GHC plugin mode. Runs during compilation for full Template Haskell
    -- support. Most accurate but slowest, as it requires full compilation.
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Verbosity level for output and logging.
--
-- Controls how much information Argus outputs during analysis.
-- Use @-v@ for verbose mode, @-vv@ for debug mode, or @--quiet@ for minimal output.
--
-- @since 1.0.0
data Verbosity
  = Quiet
    -- ^ Minimal output. Only errors and final results.
  | Normal
    -- ^ Default verbosity. Progress indicators and warnings.
  | Verbose
    -- ^ Detailed output. Summaries, timing, and additional context.
  | Debug
    -- ^ Maximum detail. File operations, batching, GC, and internal tracing.
    -- Useful for troubleshooting issues like "too many open files".
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Check if verbosity is at quiet level (minimal output).
isQuiet :: Verbosity -> Bool
isQuiet Quiet = True
isQuiet _     = False

-- | Check if verbosity is at least verbose level.
isVerbose :: Verbosity -> Bool
isVerbose v = v >= Verbose

-- | Check if verbosity is at debug level (maximum detail).
isDebug :: Verbosity -> Bool
isDebug Debug = True
isDebug _     = False

-- | Runtime configuration for Argus analysis.
--
-- Parsed from command-line arguments and configuration files.
-- Controls analysis behavior, output format, and fix application.
--
-- __Construction__:
--
-- * Use @defaultOptions@ as a starting point
-- * Override fields as needed
-- * Or use CLI argument parsing in "Argus.CLI"
--
-- __Note__: @optOutputFormat@ is @Text@ (not an enum) to avoid circular
-- dependencies with "Argus.Output.Types". Valid values are:
-- @\"terminal\"@, @\"json\"@, @\"sarif\"@, @\"html\"@, @\"plain\"@.
--
-- @since 1.0.0
data ArgusOptions = ArgusOptions
  { optMode          :: AnalysisMode
    -- ^ Analysis mode (quick, full, or plugin).
  , optConfigFile    :: Maybe FilePath
    -- ^ Path to configuration file (@argus.toml@ or @linter.toml@).
    -- If @Nothing@, auto-discovery is used.
  , optTargetPaths   :: [FilePath]
    -- ^ Files or directories to analyze. Defaults to @[\".\"]@.
  , optHieDir        :: Maybe FilePath
    -- ^ Directory containing @.hie@ files. Required for @FullMode@.
    -- If @Nothing@, defaults to @.hie@ in the project root.
  , optOutputFormat  :: Text
    -- ^ Output format name. See module note for valid values.
  , optApplyFixes    :: Bool
    -- ^ If @True@, automatically apply fixes to files.
  , optInteractive   :: Bool
    -- ^ If @True@, prompt for confirmation before each fix.
  , optPreview       :: Bool
    -- ^ If @True@, show fix previews without applying them.
  , optVerbosity     :: Verbosity
    -- ^ Output verbosity level. Use 'Quiet' for minimal output, 'Normal' for
    -- default, 'Verbose' for detailed summaries, 'Debug' for troubleshooting.
  , optNoColor       :: Bool
    -- ^ If @True@, disable ANSI color codes in terminal output.
  , optParallel      :: Natural
    -- ^ Number of parallel analysis threads. Use 1 for sequential analysis.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default options for analysis.
--
-- Provides sensible defaults for quick, non-destructive analysis:
--
-- * @QuickMode@ analysis (no HIE required)
-- * Current directory as target
-- * Terminal output with colors
-- * No automatic fix application
-- * 4 parallel threads
--
-- __Example__:
--
-- @
-- -- Customize for CI usage
-- let ciOptions = defaultOptions
--       { optOutputFormat = "sarif"
--       , optNoColor = True
--       , optParallel = 8
--       }
-- @
--
-- @since 1.0.0
defaultOptions :: ArgusOptions
defaultOptions = ArgusOptions
  { optMode         = QuickMode
  , optConfigFile   = Nothing
  , optTargetPaths  = ["."]
  , optHieDir       = Nothing
  , optOutputFormat = "terminal"
  , optApplyFixes   = False
  , optInteractive  = False
  , optPreview      = False
  , optVerbosity    = Normal
  , optNoColor      = False
  , optParallel     = 4
  }
