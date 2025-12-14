{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Types
-- Description : Core types for the Haskell linter
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module defines all the fundamental data types used throughout
-- Argus, including diagnostic types, source locations, and severity levels.
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

-- | Line number (1-indexed)
newtype Line = Line { unLine :: Int }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Enum, ToJSON, FromJSON, Hashable, NFData)

-- | Column number (1-indexed)
newtype Column = Column { unColumn :: Int }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Enum, ToJSON, FromJSON, Hashable, NFData)

-- | Time duration in seconds
newtype Seconds = Seconds { unSeconds :: Double }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Fractional, Real, RealFrac, ToJSON, FromJSON, NFData)

-- | Time duration in milliseconds
newtype Milliseconds = Milliseconds { unMilliseconds :: Double }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Fractional, Real, RealFrac, ToJSON, FromJSON, NFData)

--------------------------------------------------------------------------------
-- Source Locations
--------------------------------------------------------------------------------

-- | A position in a source file
data SrcLoc = SrcLoc
  { srcLocFile   :: !FilePath
  , srcLocLine   :: {-# UNPACK #-} !Line
  , srcLocColumn :: {-# UNPACK #-} !Column
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | A span of source code (using semantic newtypes for type safety)
data SrcSpan = SrcSpan
  { srcSpanFile      :: !FilePath
  , srcSpanStartLine :: {-# UNPACK #-} !Line
  , srcSpanStartCol  :: {-# UNPACK #-} !Column
  , srcSpanEndLine   :: {-# UNPACK #-} !Line
  , srcSpanEndCol    :: {-# UNPACK #-} !Column
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, NFData)

-- | Get the start location of a span
srcSpanStart :: SrcSpan -> SrcLoc
srcSpanStart SrcSpan{..} = SrcLoc srcSpanFile srcSpanStartLine srcSpanStartCol

-- | Get the end location of a span
srcSpanEnd :: SrcSpan -> SrcLoc
srcSpanEnd SrcSpan{..} = SrcLoc srcSpanFile srcSpanEndLine srcSpanEndCol

-- | Create a source span from start and end locations
mkSrcSpan :: SrcLoc -> SrcLoc -> SrcSpan
mkSrcSpan start end = SrcSpan
  { srcSpanFile      = srcLocFile start
  , srcSpanStartLine = srcLocLine start
  , srcSpanStartCol  = srcLocColumn start
  , srcSpanEndLine   = srcLocLine end
  , srcSpanEndCol    = srcLocColumn end
  }

-- | Empty source span for generated code
noSrcSpan :: SrcSpan
noSrcSpan = SrcSpan "" (Line 0) (Column 0) (Line 0) (Column 0)

-- | Create a SrcSpan from raw Int values (for boundary conversion)
mkSrcSpanRaw :: FilePath -> Int -> Int -> Int -> Int -> SrcSpan
mkSrcSpanRaw file sl sc el ec = SrcSpan file (Line sl) (Column sc) (Line el) (Column ec)

-- | Get start line as raw Int (for boundary conversion)
srcSpanStartLineRaw :: SrcSpan -> Int
srcSpanStartLineRaw = unLine . srcSpanStartLine

-- | Get start column as raw Int (for boundary conversion)
srcSpanStartColRaw :: SrcSpan -> Int
srcSpanStartColRaw = unColumn . srcSpanStartCol

-- | Get end line as raw Int (for boundary conversion)
srcSpanEndLineRaw :: SrcSpan -> Int
srcSpanEndLineRaw = unLine . srcSpanEndLine

-- | Get end column as raw Int (for boundary conversion)
srcSpanEndColRaw :: SrcSpan -> Int
srcSpanEndColRaw = unColumn . srcSpanEndCol

--------------------------------------------------------------------------------
-- Severity and Diagnostic Kinds
--------------------------------------------------------------------------------

-- | Severity level of a diagnostic
data Severity
  = Error      -- ^ Must be fixed
  | Warning    -- ^ Should be fixed
  | Suggestion -- ^ Could be improved
  | Info       -- ^ Informational only
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey, NFData)

-- | The kind of diagnostic
data DiagnosticKind
  = NamingConvention      -- ^ Naming convention violation
  | UnusedCode            -- ^ Dead/unused code
  | UnusedImport          -- ^ Unused import
  | RedundantCode         -- ^ Redundant/unnecessary code
  | CodePattern           -- ^ Code pattern suggestion
  | TypeSignature         -- ^ Type signature issue
  | ImportStyle           -- ^ Import style issue
  | TemplateHaskellRef    -- ^ TH reference tracking
  | SecurityIssue         -- ^ Security vulnerability
  | PerformanceIssue      -- ^ Performance anti-pattern
  | ArchitecturalIssue    -- ^ Architectural/structural issue
  | SpaceLeak             -- ^ Space leak pattern
  | PartialFunction       -- ^ Partial function usage
  | ComplexityIssue       -- ^ Code complexity warning
  | Custom Text           -- ^ Custom diagnostic kind
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, NFData)

--------------------------------------------------------------------------------
-- Import Symbol Types (for Fix Import Management)
--------------------------------------------------------------------------------

-- | Type of symbol being imported
data ImportSymbolType
  = ISTFunction      -- ^ Regular function or value
  | ISTOperator      -- ^ Infix operator (needs parens in import)
  | ISTType          -- ^ Type or type synonym
  | ISTClass         -- ^ Type class
  | ISTConstructor   -- ^ Data constructor
  | ISTPattern       -- ^ Pattern synonym
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

-- | A single symbol to import
data ImportSymbol = ImportSymbol
  { isymName     :: Text              -- ^ Symbol name (e.g., "foldl'", "Map", "(<>)")
  , isymType     :: ImportSymbolType  -- ^ Type of symbol
  , isymChildren :: [Text]            -- ^ Child items for types/classes (constructors/methods)
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

-- | Smart constructor for ImportSymbol
mkImportSymbol :: Text -> ImportSymbolType -> ImportSymbol
mkImportSymbol name symType = ImportSymbol
  { isymName = name
  , isymType = symType
  , isymChildren = []
  }

-- | Import specification required by a fix
data FixImport = FixImport
  { fimpModule    :: Text              -- ^ Module to import (e.g., "Data.Foldable")
  , fimpSymbols   :: [ImportSymbol]    -- ^ Specific symbols to import
  , fimpQualified :: Maybe Text        -- ^ Qualifier (e.g., Just "F" for "as F")
  , fimpHiding    :: Bool              -- ^ Is this a hiding import?
  , fimpPackage   :: Maybe Text        -- ^ Package name if ambiguous
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

-- | Smart constructor for FixImport
mkFixImport :: Text -> [ImportSymbol] -> FixImport
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

-- | Category of a fix for filtering and organization
data FixCategory
  = FCPerformance     -- ^ Performance improvement
  | FCModernize       -- ^ Modernization (use newer APIs)
  | FCSafety          -- ^ Safety improvement (avoid partial functions)
  | FCStyle           -- ^ Code style improvement
  | FCImports         -- ^ Import management
  | FCRedundant       -- ^ Remove redundant code
  | FCSpaceLeaks      -- ^ Fix space leaks
  | FCSecurity        -- ^ Security improvement
  | FCCustom Text     -- ^ Custom category
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

-- | Safety level of a fix
data FixSafety
  = FSAlways        -- ^ Always safe to apply automatically
  | FSMostly        -- ^ Safe in most cases, rare edge cases
  | FSReview        -- ^ Requires human review before applying
  | FSUnsafe        -- ^ May change semantics, manual only
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

-- | A single edit to apply to source code
data FixEdit = FixEdit
  { fixEditSpan    :: SrcSpan  -- ^ Where to apply the edit
  , fixEditNewText :: Text     -- ^ The replacement text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, NFData)

-- | A fix that can be applied to source code
--
-- This type supports comprehensive import management:
-- * @fixAddImports@: Imports to add when the fix is applied
-- * @fixRemoveImports@: Import symbols that become unused after the fix
--
-- INVARIANT: fixEdits should never be empty. Use mkFixSafe for safe construction.
data Fix = Fix
  { fixTitle         :: Text           -- ^ Human-readable title
  , fixEdits         :: [FixEdit]      -- ^ Edits to apply (should be non-empty)
  , fixIsPreferred   :: Bool           -- ^ Is this the preferred fix?
  , fixAddImports    :: [FixImport]    -- ^ Imports to add when fix is applied
  , fixRemoveImports :: [Text]         -- ^ Import symbols that become unused
  , fixCategory      :: FixCategory    -- ^ Category for filtering
  , fixSafety        :: FixSafety      -- ^ Safety level
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

-- | Smart constructor for Fix that validates non-empty edits
-- Returns Nothing if edits list is empty
-- Uses default values for new fields (backward compatible)
mkFixSafe :: Text -> [FixEdit] -> Bool -> Maybe Fix
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

-- | Simple constructor for backward compatibility
-- Creates a fix with empty import management fields
mkFix :: Text -> [FixEdit] -> Bool -> Fix
mkFix title edits preferred = Fix
  { fixTitle = title
  , fixEdits = edits
  , fixIsPreferred = preferred
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSAlways
  }

-- | Full constructor with import management
mkFixWithImports :: Text              -- ^ Title
                 -> [FixEdit]         -- ^ Edits
                 -> Bool              -- ^ Is preferred
                 -> [FixImport]       -- ^ Imports to add
                 -> [Text]            -- ^ Imports to remove
                 -> FixCategory       -- ^ Category
                 -> FixSafety         -- ^ Safety level
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

-- | A diagnostic message
data Diagnostic = Diagnostic
  { diagSpan      :: SrcSpan        -- ^ Source location
  , diagSeverity  :: Severity       -- ^ Severity level
  , diagKind      :: DiagnosticKind -- ^ Kind of diagnostic
  , diagMessage   :: Text           -- ^ Human-readable message
  , diagCode      :: Maybe Text     -- ^ Optional diagnostic code
  , diagFixes     :: [Fix]          -- ^ Available fixes
  , diagRelated   :: [(SrcSpan, Text)] -- ^ Related locations with messages
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

-- | Kind of symbol
data SymbolKind
  = Function
  | TypeConstructor
  | DataConstructor
  | TypeClass
  | TypeClassMethod
  | TypeFamily
  | PatternSynonym
  | Module
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | A qualified name
data QualifiedName = QualifiedName
  { qnModule :: Maybe Text  -- ^ Module name (Nothing for local)
  , qnName   :: Text        -- ^ The name itself
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | Create a qualified name
mkQualifiedName :: Maybe Text -> Text -> QualifiedName
mkQualifiedName = QualifiedName

-- | A symbol in the codebase
data Symbol = Symbol
  { symbolName     :: QualifiedName  -- ^ Qualified name
  , symbolKind     :: SymbolKind     -- ^ Kind of symbol
  , symbolSpan     :: SrcSpan        -- ^ Definition location
  , symbolExported :: Bool           -- ^ Is it exported?
  , symbolType     :: Maybe Text     -- ^ Type signature (if available)
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

-- | Result for a single file
data FileResult = FileResult
  { fileResultPath        :: FilePath
  , fileResultDiagnostics :: [Diagnostic]
  , fileResultSymbols     :: [Symbol]
  , fileResultImports     :: [QualifiedName]
  , fileResultExports     :: [QualifiedName]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Combined analysis result
data AnalysisResult = AnalysisResult
  { resultFiles       :: Map.Map FilePath FileResult
  , resultUnusedCode  :: Set.Set QualifiedName
  , resultDiagCount   :: Map.Map Severity Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty analysis result
emptyAnalysisResult :: AnalysisResult
emptyAnalysisResult = AnalysisResult Map.empty Set.empty Map.empty

-- | Merge two analysis results
mergeResults :: AnalysisResult -> AnalysisResult -> AnalysisResult
mergeResults r1 r2 = AnalysisResult
  { resultFiles = Map.union (resultFiles r1) (resultFiles r2)
  , resultUnusedCode = Set.union (resultUnusedCode r1) (resultUnusedCode r2)
  , resultDiagCount = Map.unionWith (+) (resultDiagCount r1) (resultDiagCount r2)
  }

--------------------------------------------------------------------------------
-- Analysis Mode and Options
--------------------------------------------------------------------------------

-- | Analysis mode
data AnalysisMode
  = QuickMode   -- ^ Syntactic only, no compilation needed
  | FullMode    -- ^ HIE-based, requires compiled project
  | PluginMode  -- ^ GHC plugin mode for TH support
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Linter options
-- NOTE: optOutputFormat is Text to avoid circular dependency with Output.Types
-- Valid values: "terminal", "json", "sarif", "html", "plain"
data ArgusOptions = ArgusOptions
  { optMode          :: AnalysisMode
  , optConfigFile    :: Maybe FilePath
  , optTargetPaths   :: [FilePath]
  , optHieDir        :: Maybe FilePath
  , optOutputFormat  :: Text           -- ^ Output format name
  , optApplyFixes    :: Bool
  , optInteractive   :: Bool
  , optPreview       :: Bool
  , optVerbose       :: Bool
  , optNoColor       :: Bool
  , optParallel      :: Natural        -- ^ Number of threads (non-negative)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default options
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
  , optVerbose      = False
  , optNoColor      = False
  , optParallel     = 4
  }
