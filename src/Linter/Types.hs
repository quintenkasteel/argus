{-# LANGUAGE StrictData #-}

-- |
-- Module      : Linter.Types
-- Description : Core types for the Haskell linter
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module defines all the fundamental data types used throughout
-- the linter, including diagnostic types, source locations, and severity levels.
module Linter.Types
  ( -- * Diagnostics
    Diagnostic (..)
  , DiagnosticKind (..)
  , Severity (..)
  , Fix (..)
  , FixEdit (..)

    -- * Source locations
  , SrcSpan (..)
  , SrcLoc (..)
  , srcSpanStart
  , srcSpanEnd
  , mkSrcSpan
  , noSrcSpan

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
  , LinterOptions (..)
  , defaultOptions
  ) where

import Data.Aeson (ToJSON (..), FromJSON (..), ToJSONKey (..), FromJSONKey (..), (.=), (.:), (.:?), object, withObject)
import Data.Hashable (Hashable (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Source Locations
--------------------------------------------------------------------------------

-- | A position in a source file
data SrcLoc = SrcLoc
  { srcLocFile   :: FilePath
  , srcLocLine   :: {-# UNPACK #-} Int
  , srcLocColumn :: {-# UNPACK #-} Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | A span of source code
data SrcSpan = SrcSpan
  { srcSpanFile      :: FilePath
  , srcSpanStartLine :: {-# UNPACK #-} Int
  , srcSpanStartCol  :: {-# UNPACK #-} Int
  , srcSpanEndLine   :: {-# UNPACK #-} Int
  , srcSpanEndCol    :: {-# UNPACK #-} Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

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
noSrcSpan = SrcSpan "" 0 0 0 0

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
  deriving anyclass (Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

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
  | Custom Text           -- ^ Custom diagnostic kind
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Fixes
--------------------------------------------------------------------------------

-- | A single edit to apply to source code
data FixEdit = FixEdit
  { fixEditSpan    :: SrcSpan  -- ^ Where to apply the edit
  , fixEditNewText :: Text     -- ^ The replacement text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A fix that can be applied to source code
data Fix = Fix
  { fixTitle   :: Text      -- ^ Human-readable title
  , fixEdits   :: [FixEdit] -- ^ Edits to apply
  , fixIsPreferred :: Bool  -- ^ Is this the preferred fix?
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
data LinterOptions = LinterOptions
  { optMode          :: AnalysisMode
  , optConfigFile    :: Maybe FilePath
  , optTargetPaths   :: [FilePath]
  , optHieDir        :: Maybe FilePath
  , optOutputFormat  :: Text  -- ^ "terminal", "json", "sarif"
  , optApplyFixes    :: Bool
  , optInteractive   :: Bool
  , optPreview       :: Bool
  , optVerbose       :: Bool
  , optNoColor       :: Bool
  , optParallel      :: Int   -- ^ Number of threads
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default options
defaultOptions :: LinterOptions
defaultOptions = LinterOptions
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
