{-# LANGUAGE StrictData #-}
{-# LANGUAGE ApplicativeDo #-}

-- |
-- Module      : Argus.Config
-- Description : Configuration loading and parsing
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module handles configuration for Argus, supporting both
-- TOML (preferred) and YAML formats for backwards compatibility.
module Argus.Config
  ( -- * Configuration types
    Config (..)
  , GeneralConfig (..)
  , OutputConfig (..)
  , UnusedConfig (..)
  , NamingConfig (..)
  , PatternsConfig (..)
  , ImportsConfig (..)
  , FixConfig (..)
  , AutoFixImportsConfig (..)
  , ComplexityConfig (..)
  , ResourceConfig (..)
  , QualifyImportConfig (..)
  , AliasStrategy (..)
  , ArchitectureConfig (..)
  , LayerConfig (..)

    -- * Defaults
  , defaultQualifyImportConfig
  , defaultArchitectureConfig

    -- * Rule definitions
  , TypeRule (..)
  , VariableRule (..)
  , PatternRule (..)
  , RuleSeverity (..)
  , patternRuleToRule
  , ruleSeverityToSeverity

    -- * Loading configuration
  , loadConfig
  , loadConfigFromFile
  , loadConfigWithEnv
  , applyEnvToConfig
  , defaultConfig
  , defaultAutoFixImportsConfig

    -- * TOML Codecs (for testing)
  , configCodec
  , generalCodec
  , outputCodec
  , complexityCodec

    -- * Conversion
  , toImportManagerConfig

    -- * Legacy YAML support
  , loadLegacyConfig
  , convertLegacyConfig
  , LegacyConfig (..)
  , LegacySignature (..)
  , LegacyVariable (..)
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.!=), withObject, object, Value(..))
import qualified Data.Aeson as AE ((.=))
-- import Data.Map.Strict (Map)  -- removed - unused
-- import Data.Map.Strict qualified as Map  -- removed - unused
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)
import Toml (TomlCodec, (.=))
import Toml qualified as TOML

import Argus.Analysis.Architecture (ArchitectureConfig(..), LayerConfig(..), defaultArchitectureConfig)
import Argus.Config.Env (EnvConfig(..), loadEnvConfig)
import Argus.Imports.Manager (ImportManagerConfig(..))
import Argus.Rules.Types qualified as RT
import Argus.Types (Severity(..))

--------------------------------------------------------------------------------
-- Rule Severity (used in config)
--------------------------------------------------------------------------------

data RuleSeverity = RSError | RSWarning | RSSuggestion | RSInfo
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Commented out to avoid unused warning - can uncomment when needed
-- toSeverity :: RuleSeverity -> Severity
-- toSeverity = \case
--   RSError      -> Error
--   RSWarning    -> Warning
--   RSSuggestion -> Suggestion
--   RSInfo       -> Info

--------------------------------------------------------------------------------
-- Configuration Types
--------------------------------------------------------------------------------

-- | Main configuration
data Config = Config
  { cfgGeneral       :: GeneralConfig
  , cfgOutput        :: OutputConfig
  , cfgUnused        :: UnusedConfig
  , cfgNaming        :: NamingConfig
  , cfgPatterns      :: PatternsConfig
  , cfgImports       :: ImportsConfig
  , cfgFix           :: FixConfig
  , cfgComplexity    :: ComplexityConfig
  , cfgResource      :: ResourceConfig
  , cfgQualifyImport :: QualifyImportConfig
  , cfgArchitecture  :: ArchitectureConfig
  }
  deriving stock (Eq, Show, Generic)

-- | General configuration
data GeneralConfig = GeneralConfig
  { genDirectories :: [FilePath]  -- ^ Directories to analyze
  , genExclude     :: [Text]      -- ^ Glob patterns to exclude
  , genMode        :: Text        -- ^ "quick", "full", or "plugin"
  , genHieDir      :: Maybe FilePath -- ^ Path to HIE files
  }
  deriving stock (Eq, Show, Generic)

-- | Output configuration
data OutputConfig = OutputConfig
  { outFormat       :: Text      -- ^ "terminal", "json", "html", "sarif"
  , outColor        :: Bool      -- ^ Use colors
  , outGroupBy      :: Text      -- ^ "file", "rule", "severity"
  , outShowContext  :: Bool      -- ^ Show source context
  , outContextLines :: Int       -- ^ Lines of context
  }
  deriving stock (Eq, Show, Generic)

-- | Unused code detection configuration
data UnusedConfig = UnusedConfig
  { unusedEnabled          :: Bool
  , unusedCheckFunctions   :: Bool
  , unusedCheckTypes       :: Bool
  , unusedCheckImports     :: Bool
  , unusedCheckExports     :: Bool
  , unusedCheckConstructors :: Bool      -- ^ Check for unused data constructors
  , unusedCheckRecordFields :: Bool      -- ^ Check for unused record fields
  , unusedCheckLocalBinds   :: Bool      -- ^ Check for unused local bindings (where/let)
  , unusedCheckInstances    :: Bool      -- ^ Check for unused typeclass instances
  , unusedTypeClassRoots    :: Bool      -- ^ Treat typeclass methods as roots (Weeder-style)
  , unusedDeriveRoots       :: Bool      -- ^ Treat derived instances as roots
  , unusedMinConfidence     :: Double    -- ^ Minimum confidence level (0.0-1.0)
  , unusedRoots             :: [Text]    -- ^ Regex patterns for roots
  , unusedThRoots           :: [Text]    -- ^ TH-referenced roots
  , unusedIgnorePatterns    :: [Text]    -- ^ Patterns to ignore (e.g., test files)
  }
  deriving stock (Eq, Show, Generic)

-- | Naming convention configuration
data NamingConfig = NamingConfig
  { namingEnabled   :: Bool
  , namingTypes     :: [TypeRule]
  , namingVariables :: [VariableRule]
  }
  deriving stock (Eq, Show, Generic)

-- | Type naming rule
data TypeRule = TypeRule
  { trPattern     :: Text       -- ^ Pattern to match (supports wildcards)
  , trReplacement :: Text       -- ^ Replacement pattern
  , trSeverity    :: RuleSeverity
  , trMessage     :: Maybe Text -- ^ Custom message
  , trWithin      :: Maybe Text -- ^ Restrict to specific modules
  }
  deriving stock (Eq, Show, Generic)

-- | Variable naming rule
data VariableRule = VariableRule
  { vrType        :: Text        -- ^ Type pattern to match
  , vrFrom        :: Maybe Text  -- ^ Name pattern (Nothing = any)
  , vrTo          :: Text        -- ^ Target name
  , vrSeverity    :: RuleSeverity
  , vrMessage     :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

-- | Pattern matching configuration
data PatternsConfig = PatternsConfig
  { patternsEnabled :: Bool
  , patternsRules   :: [RT.Rule]  -- ^ Pattern rules (converted from PatternRule)
  }
  deriving stock (Eq, Show, Generic)

-- | Pattern rule (TOML parsing format, converted to RT.Rule)
data PatternRule = PatternRule
  { prName        :: Text        -- ^ Rule name
  , prMatch       :: Text        -- ^ Pattern to match (Haskell syntax)
  , prFix         :: Maybe Text  -- ^ Replacement pattern
  , prWhere       :: Maybe Text  -- ^ Type constraint
  , prSeverity    :: RuleSeverity
  , prMessage     :: Text        -- ^ Message to show
  }
  deriving stock (Eq, Show, Generic)

-- | Convert a PatternRule to unified Rule type
patternRuleToRule :: PatternRule -> RT.Rule
patternRuleToRule PatternRule{..} = RT.defaultRule
  { RT.ruleId = "pattern/" <> prName
  , RT.ruleCategory = RT.Style
  , RT.ruleSeverity = ruleSeverityToSeverity prSeverity
  , RT.ruleMessage = prMessage
  , RT.rulePattern = RT.TextPatternSpec prMatch
  , RT.ruleReplacement = prFix
  , RT.ruleConditions = maybeToList (parseWhereCondition <$> prWhere)
  , RT.ruleSafety = RT.MostlySafe
  }
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

    parseWhereCondition :: Text -> RT.SideCondition
    parseWhereCondition w = RT.HasType "$X" w  -- Simplified: treat "where" as type constraint

-- | Convert RuleSeverity to unified Severity
ruleSeverityToSeverity :: RuleSeverity -> Severity
ruleSeverityToSeverity = \case
  RSError      -> Error
  RSWarning    -> Warning
  RSSuggestion -> Suggestion
  RSInfo       -> Info

-- | Convert unified Severity to RuleSeverity
severityToRuleSeverity :: Severity -> RuleSeverity
severityToRuleSeverity = \case
  Error      -> RSError
  Warning    -> RSWarning
  Suggestion -> RSSuggestion
  Info       -> RSInfo

-- | Convert RT.Rule back to PatternRule for serialization
ruleToPatternRule :: RT.Rule -> PatternRule
ruleToPatternRule rule = PatternRule
  { prName     = stripPatternPrefix (RT.ruleId rule)
  , prMatch    = extractPatternText (RT.rulePattern rule)
  , prFix      = RT.ruleReplacement rule
  , prWhere    = extractWhereCondition (RT.ruleConditions rule)
  , prSeverity = severityToRuleSeverity (RT.ruleSeverity rule)
  , prMessage  = RT.ruleMessage rule
  }
  where
    stripPatternPrefix t = fromMaybe t (T.stripPrefix "pattern/" t)
    extractPatternText = RT.rulePatternToText
    extractWhereCondition [] = Nothing
    extractWhereCondition (RT.HasType _ t : _) = Just t
    extractWhereCondition (_ : rest) = extractWhereCondition rest

-- | Import configuration
data ImportsConfig = ImportsConfig
  { importsRemoveUnused   :: Bool
  , importsSuggestQualified :: [Text]  -- ^ Modules that should be qualified
  , importsAllowUnqualifiedTypes :: Bool  -- ^ Allow unqualified type-only imports
  , importsAllowUnqualifiedOperators :: Bool  -- ^ Allow unqualified operator imports
  , importsRequireExplicit  :: Bool    -- ^ Require explicit import lists
  , importsCombine          :: Bool    -- ^ Combine fragmented imports
  , importsThRoots          :: [Text]  -- ^ TH patterns that mark imports as used (regex)
  , importsSuppressForTH    :: Bool    -- ^ Suppress unused warnings for files with TH but no HIE
  }
  deriving stock (Eq, Show, Generic)

-- | Fix configuration
data FixConfig = FixConfig
  { fixEnabled     :: Bool
  , fixSafeOnly    :: Bool              -- ^ Only apply safe fixes
  , fixPreview     :: Bool              -- ^ Show preview before applying
  , fixBackup      :: Bool              -- ^ Create .bak files
  , fixAutoImports :: AutoFixImportsConfig  -- ^ Auto-fix import management
  }
  deriving stock (Eq, Show, Generic)

-- | Configuration for auto-fixing imports
data AutoFixImportsConfig = AutoFixImportsConfig
  { afiEnabled           :: Bool      -- ^ Enable import management in fixes
  , afiAddMissing        :: Bool      -- ^ Add missing imports from fixes
  , afiRemoveUnused      :: Bool      -- ^ Remove imports for removed symbols
  , afiOrganize          :: Bool      -- ^ Sort and organize imports
  , afiUseExplicit       :: Bool      -- ^ Prefer explicit import lists
  , afiQualifyNew        :: Bool      -- ^ Qualify newly added imports
  , afiGroupByCategory   :: Bool      -- ^ Group imports by category
  }
  deriving stock (Eq, Show, Generic)

-- | Complexity analysis configuration
data ComplexityConfig = ComplexityConfig
  { compEnabled             :: Bool  -- ^ Enable complexity checking
  , compCyclomaticWarning   :: Int   -- ^ Cyclomatic complexity warning threshold
  , compCyclomaticError     :: Int   -- ^ Cyclomatic complexity error threshold
  , compCognitiveWarning    :: Int   -- ^ Cognitive complexity warning threshold
  , compCognitiveError      :: Int   -- ^ Cognitive complexity error threshold
  , compLineLengthWarning   :: Int   -- ^ Function line count warning threshold
  , compNestingWarning      :: Int   -- ^ Nesting depth warning threshold
  , compParameterWarning    :: Int   -- ^ Parameter count warning threshold
  , compPatternBranchWarning :: Int  -- ^ Pattern branch count warning threshold
  }
  deriving stock (Eq, Show, Generic)

-- | Resource limits and timeout configuration
data ResourceConfig = ResourceConfig
  { resTimeoutSeconds     :: Maybe Int    -- ^ Per-file timeout in seconds (Nothing = no timeout)
  , resMaxMemoryMB        :: Maybe Int    -- ^ Maximum memory usage in MB (Nothing = no limit)
  , resForceGCInterval    :: Maybe Int    -- ^ Force GC every N files (Nothing = never)
  , resTrackPerFile       :: Bool         -- ^ Track per-file resource usage
  , resWarnSlowFiles      :: Maybe Double -- ^ Warn if file takes longer than N seconds
  , resMaxRetries         :: Int          -- ^ Maximum retries for failed files
  , resKillOnTimeout      :: Bool         -- ^ Kill analysis thread on timeout
  }
  deriving stock (Eq, Show, Generic)

-- | Strategy for generating import aliases
data AliasStrategy
  = LastPart           -- ^ Use last module component: Data.Text -> Text
  | FirstLetter        -- ^ Use first letter of last component: Data.Text -> T
  | Initials           -- ^ Use initials of all components: Data.Text -> DT
  | FirstNChars Int    -- ^ Use first N characters of last component: Data.Text -> Tex (n=3)
  deriving stock (Eq, Show, Generic)

-- | Configuration for qualify-import refactoring
data QualifyImportConfig = QualifyImportConfig
  { qicEnabled           :: Bool               -- ^ Enable qualify-import refactoring
  , qicStrategy          :: AliasStrategy      -- ^ Strategy for generating aliases
  , qicCustomAliases     :: [(Text, Text)]     -- ^ Custom module -> alias mappings (overrides strategy)
  , qicMaxAliasLength    :: Maybe Int          -- ^ Maximum alias length (Nothing = no limit)
  , qicPreferUppercase   :: Bool               -- ^ Uppercase short aliases (T instead of t)
  , qicAllowConflicts    :: Bool               -- ^ Allow alias conflicts (will suffix with numbers)
  }
  deriving stock (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Default Configuration
--------------------------------------------------------------------------------

defaultConfig :: Config
defaultConfig = Config
  { cfgGeneral = GeneralConfig
      { genDirectories = ["src", "app"]
      , genExclude = defaultExcludePatterns
      , genMode = "quick"
      , genHieDir = Just ".hie"
      }
  , cfgOutput = OutputConfig
      { outFormat = "terminal"
      , outColor = True
      , outGroupBy = "file"
      , outShowContext = True
      , outContextLines = 2
      }
  , cfgUnused = UnusedConfig
      { unusedEnabled = True
      , unusedCheckFunctions = True
      , unusedCheckTypes = True
      , unusedCheckImports = True
      , unusedCheckExports = True
      , unusedCheckConstructors = True
      , unusedCheckRecordFields = True
      , unusedCheckLocalBinds = True
      , unusedCheckInstances = False  -- Disabled by default (many false positives)
      , unusedTypeClassRoots = True   -- Weeder-style: treat typeclass methods as roots
      , unusedDeriveRoots = True      -- Treat derived instances as roots
      , unusedMinConfidence = 0.5     -- Only report high-confidence issues
      , unusedRoots = [ "^Main.main$"
                      , "^Paths_.*"
                      , ".*\\.spec$"  -- Test specs (hspec)
                      , ".*Spec\\.spec$"
                      , "^.*\\.main$"  -- Any main function
                      , "^.*\\.plugin$"  -- GHC plugins
                      ]
      , unusedThRoots = ["parseJSON", "toJSON", "makeLenses", "makeFields", "deriveJSON"]
      , unusedIgnorePatterns = []
      }
  , cfgNaming = NamingConfig
      { namingEnabled = True
      , namingTypes = []
      , namingVariables = []
      }
  , cfgPatterns = PatternsConfig
      { patternsEnabled = True
      , patternsRules = defaultPatternRules
      }
  , cfgImports = ImportsConfig
      { importsRemoveUnused = True
      , importsSuggestQualified = ["Data.Map", "Data.Set", "Data.Text", "Data.ByteString"]
      , importsAllowUnqualifiedTypes = True  -- Allow "import Data.Text (Text)"
      , importsAllowUnqualifiedOperators = True  -- Allow "import Control.Applicative ((<$>))"
      , importsRequireExplicit = False
      , importsCombine = True
      , importsThRoots = ["widgetFile", "hamletFile", "juliusFile", "cassiusFile", "luciusFile",
                          "whamletFile", "mkYesod", "mkPersist", "share", "derivePersistField",
                          "makeLenses", "makeFields", "deriveJSON", "deriveToJSON", "deriveFromJSON"]
      , importsSuppressForTH = True  -- Suppress unused import warnings for TH files without HIE
      }
  , cfgFix = FixConfig
      { fixEnabled = True
      , fixSafeOnly = True
      , fixPreview = True
      , fixBackup = True
      , fixAutoImports = defaultAutoFixImportsConfig
      }
  , cfgComplexity = ComplexityConfig
      { compEnabled = True
      , compCyclomaticWarning = 10
      , compCyclomaticError = 20
      , compCognitiveWarning = 15
      , compCognitiveError = 25
      , compLineLengthWarning = 50
      , compNestingWarning = 4
      , compParameterWarning = 5
      , compPatternBranchWarning = 10
      }
  , cfgResource = ResourceConfig
      { resTimeoutSeconds = Just 60       -- 60 second timeout per file
      , resMaxMemoryMB = Just 2048        -- 2GB max memory
      , resForceGCInterval = Just 100     -- GC every 100 files
      , resTrackPerFile = True            -- Track per-file stats
      , resWarnSlowFiles = Just 10.0      -- Warn if file takes > 10s
      , resMaxRetries = 1                 -- One retry on failure
      , resKillOnTimeout = True           -- Kill thread on timeout
      }
  , cfgQualifyImport = defaultQualifyImportConfig
  , cfgArchitecture = defaultArchitectureConfig
  }

-- | Default exclude patterns for common build artifacts
defaultExcludePatterns :: [Text]
defaultExcludePatterns =
  [ -- Build artifacts
    ".stack-work/**"
  , "dist-newstyle/**"
  , "dist/**"
  , ".cabal/**"
  , ".cabal-sandbox/**"
    -- Generated code
  , "Generated/**"
  , "*.gen.hs"
  , "**/autogen/**"
  , "**/Paths_*.hs"
  , "**/PackageInfo_*.hs"
    -- Version control and editor files
  , ".git/**"
  , ".hg/**"
  , ".svn/**"
  ]

-- | Default pattern rules (common anti-patterns)
defaultPatternRules :: [RT.Rule]
defaultPatternRules = map patternRuleToRule
  [ PatternRule
      { prName = "avoid-head"
      , prMatch = "head xs"
      , prFix = Just "listToMaybe xs"
      , prWhere = Nothing
      , prSeverity = RSWarning
      , prMessage = "Use listToMaybe instead of partial head"
      }
  , PatternRule
      { prName = "avoid-fromJust"
      , prMatch = "fromJust x"
      , prFix = Nothing
      , prWhere = Nothing
      , prSeverity = RSWarning
      , prMessage = "fromJust is partial, consider pattern matching"
      }
  , PatternRule
      { prName = "redundant-id"
      , prMatch = "id x"
      , prFix = Just "x"
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Redundant use of id"
      }
  ]

-- | Default configuration for auto-fix import management
defaultAutoFixImportsConfig :: AutoFixImportsConfig
defaultAutoFixImportsConfig = AutoFixImportsConfig
  { afiEnabled         = True        -- Enable import management
  , afiAddMissing      = True        -- Add missing imports from fixes
  , afiRemoveUnused    = True        -- Remove imports for removed symbols
  , afiOrganize        = False       -- Don't reorganize by default (conservative)
  , afiUseExplicit     = True        -- Prefer explicit import lists
  , afiQualifyNew      = False       -- Don't qualify by default
  , afiGroupByCategory = False       -- Don't group by default
  }

-- | Default configuration for qualify-import refactoring
defaultQualifyImportConfig :: QualifyImportConfig
defaultQualifyImportConfig = QualifyImportConfig
  { qicEnabled         = True                -- Enable by default
  , qicStrategy        = LastPart            -- Use last module component (Data.Text -> Text)
  , qicCustomAliases   = []                  -- No custom aliases by default
  , qicMaxAliasLength  = Nothing             -- No length limit
  , qicPreferUppercase = False               -- Keep natural casing (Map, Text, not MAP, TEXT)
  , qicAllowConflicts  = True                -- Allow conflicts, suffix with numbers
  }

-- | Convert AutoFixImportsConfig to ImportManagerConfig
toImportManagerConfig :: AutoFixImportsConfig -> ImportManagerConfig
toImportManagerConfig AutoFixImportsConfig{..} = ImportManagerConfig
  { imcAddMissingImports   = afiAddMissing
  , imcRemoveUnusedImports = afiRemoveUnused
  , imcOrganizeImports     = afiOrganize
  , imcExplicitImports     = afiUseExplicit
  , imcQualifyNewImports   = afiQualifyNew
  , imcGroupImports        = afiGroupByCategory
  }

--------------------------------------------------------------------------------
-- JSON/Aeson Instances
--------------------------------------------------------------------------------

instance ToJSON Config where
  toJSON Config{..} = object
    [ "general" AE..= cfgGeneral
    , "output" AE..= cfgOutput
    , "unused" AE..= cfgUnused
    , "naming" AE..= cfgNaming
    , "patterns" AE..= cfgPatterns
    , "imports" AE..= cfgImports
    , "fix" AE..= cfgFix
    , "complexity" AE..= cfgComplexity
    , "resource" AE..= cfgResource
    , "qualify-import" AE..= cfgQualifyImport
    , "architecture" AE..= cfgArchitecture
    ]

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    cfgGeneral       <- o .:? "general"        .!= cfgGeneral defaultConfig
    cfgOutput        <- o .:? "output"         .!= cfgOutput defaultConfig
    cfgUnused        <- o .:? "unused"         .!= cfgUnused defaultConfig
    cfgNaming        <- o .:? "naming"         .!= cfgNaming defaultConfig
    cfgPatterns      <- o .:? "patterns"       .!= cfgPatterns defaultConfig
    cfgImports       <- o .:? "imports"        .!= cfgImports defaultConfig
    cfgFix           <- o .:? "fix"            .!= cfgFix defaultConfig
    cfgComplexity    <- o .:? "complexity"     .!= cfgComplexity defaultConfig
    cfgResource      <- o .:? "resource"       .!= cfgResource defaultConfig
    cfgQualifyImport <- o .:? "qualify-import" .!= cfgQualifyImport defaultConfig
    cfgArchitecture  <- o .:? "architecture"   .!= cfgArchitecture defaultConfig
    pure Config{..}

instance ToJSON GeneralConfig where
  toJSON GeneralConfig{..} = object
    [ "directories" AE..= genDirectories
    , "exclude" AE..= genExclude
    , "mode" AE..= genMode
    , "hie-dir" AE..= genHieDir
    ]

instance FromJSON GeneralConfig where
  parseJSON = withObject "GeneralConfig" $ \o -> do
    genDirectories <- o .:? "directories" .!= ["src", "app"]
    genExclude     <- o .:? "exclude"     .!= []
    genMode        <- o .:? "mode"        .!= "quick"
    genHieDir      <- o .:? "hie-dir"
    pure GeneralConfig{..}

instance ToJSON OutputConfig where
  toJSON OutputConfig{..} = object
    [ "format" AE..= outFormat
    , "color" AE..= outColor
    , "group-by" AE..= outGroupBy
    , "show-context" AE..= outShowContext
    , "context-lines" AE..= outContextLines
    ]

instance FromJSON OutputConfig where
  parseJSON = withObject "OutputConfig" $ \o -> do
    outFormat       <- o .:? "format"        .!= "terminal"
    outColor        <- o .:? "color"         .!= True
    outGroupBy      <- o .:? "group-by"      .!= "file"
    outShowContext  <- o .:? "show-context"  .!= True
    outContextLines <- o .:? "context-lines" .!= 2
    pure OutputConfig{..}

instance ToJSON UnusedConfig where
  toJSON UnusedConfig{..} = object
    [ "enabled" AE..= unusedEnabled
    , "check-functions" AE..= unusedCheckFunctions
    , "check-types" AE..= unusedCheckTypes
    , "check-imports" AE..= unusedCheckImports
    , "check-exports" AE..= unusedCheckExports
    , "check-constructors" AE..= unusedCheckConstructors
    , "check-record-fields" AE..= unusedCheckRecordFields
    , "check-local-binds" AE..= unusedCheckLocalBinds
    , "check-instances" AE..= unusedCheckInstances
    , "typeclass-roots" AE..= unusedTypeClassRoots
    , "derive-roots" AE..= unusedDeriveRoots
    , "min-confidence" AE..= unusedMinConfidence
    , "roots" AE..= unusedRoots
    , "th-roots" AE..= unusedThRoots
    , "ignore-patterns" AE..= unusedIgnorePatterns
    ]

instance FromJSON UnusedConfig where
  parseJSON = withObject "UnusedConfig" $ \o -> do
    unusedEnabled           <- o .:? "enabled"             .!= True
    unusedCheckFunctions    <- o .:? "check-functions"     .!= True
    unusedCheckTypes        <- o .:? "check-types"         .!= True
    unusedCheckImports      <- o .:? "check-imports"       .!= True
    unusedCheckExports      <- o .:? "check-exports"       .!= True
    unusedCheckConstructors <- o .:? "check-constructors"  .!= True
    unusedCheckRecordFields <- o .:? "check-record-fields" .!= True
    unusedCheckLocalBinds   <- o .:? "check-local-binds"   .!= True
    unusedCheckInstances    <- o .:? "check-instances"     .!= False
    unusedTypeClassRoots    <- o .:? "typeclass-roots"     .!= True
    unusedDeriveRoots       <- o .:? "derive-roots"        .!= True
    unusedMinConfidence     <- o .:? "min-confidence"      .!= 0.5
    unusedRoots             <- o .:? "roots"               .!= ["^Main.main$"]
    unusedThRoots           <- o .:? "th-roots"            .!= []
    unusedIgnorePatterns    <- o .:? "ignore-patterns"     .!= []
    pure UnusedConfig{..}

instance ToJSON NamingConfig where
  toJSON NamingConfig{..} = object
    [ "enabled" AE..= namingEnabled
    , "types" AE..= namingTypes
    , "variables" AE..= namingVariables
    ]

instance FromJSON NamingConfig where
  parseJSON = withObject "NamingConfig" $ \o -> do
    namingEnabled   <- o .:? "enabled"   .!= True
    namingTypes     <- o .:? "types"     .!= []
    namingVariables <- o .:? "variables" .!= []
    pure NamingConfig{..}

instance ToJSON TypeRule where
  toJSON TypeRule{..} = object
    [ "pattern" AE..= trPattern
    , "replacement" AE..= trReplacement
    , "severity" AE..= trSeverity
    , "message" AE..= trMessage
    , "within" AE..= trWithin
    ]

instance FromJSON TypeRule where
  parseJSON = withObject "TypeRule" $ \o -> do
    trPattern     <- o .: "pattern"
    trReplacement <- o .: "replacement"
    trSeverity    <- o .:? "severity" .!= RSWarning
    trMessage     <- o .:? "message"
    trWithin      <- o .:? "within"
    pure TypeRule{..}

instance ToJSON VariableRule where
  toJSON VariableRule{..} = object
    [ "type" AE..= vrType
    , "from" AE..= vrFrom
    , "to" AE..= vrTo
    , "severity" AE..= vrSeverity
    , "message" AE..= vrMessage
    ]

instance FromJSON VariableRule where
  parseJSON = withObject "VariableRule" $ \o -> do
    vrType     <- o .: "type"
    vrFrom     <- o .:? "from"
    vrTo       <- o .: "to"
    vrSeverity <- o .:? "severity" .!= RSWarning
    vrMessage  <- o .:? "message"
    pure VariableRule{..}

instance ToJSON PatternsConfig where
  toJSON PatternsConfig{..} = object
    [ "enabled" AE..= patternsEnabled
    , "rules" AE..= map ruleToPatternRule patternsRules
    ]

instance FromJSON PatternsConfig where
  parseJSON = withObject "PatternsConfig" $ \o -> do
    patternsEnabled <- o .:? "enabled" .!= True
    rawRules        <- o .:? "rules"   .!= ([] :: [PatternRule])
    let patternsRules = map patternRuleToRule rawRules
    pure PatternsConfig{..}

instance ToJSON PatternRule where
  toJSON PatternRule{..} = object
    [ "name" AE..= prName
    , "match" AE..= prMatch
    , "fix" AE..= prFix
    , "where" AE..= prWhere
    , "severity" AE..= prSeverity
    , "message" AE..= prMessage
    ]

instance FromJSON PatternRule where
  parseJSON = withObject "PatternRule" $ \o -> do
    prName     <- o .: "name"
    prMatch    <- o .: "match"
    prFix      <- o .:? "fix"
    prWhere    <- o .:? "where"
    prSeverity <- o .:? "severity" .!= RSWarning
    prMessage  <- o .: "message"
    pure PatternRule{..}

instance ToJSON ImportsConfig where
  toJSON ImportsConfig{..} = object
    [ "remove-unused" AE..= importsRemoveUnused
    , "suggest-qualified" AE..= importsSuggestQualified
    , "require-explicit" AE..= importsRequireExplicit
    , "combine" AE..= importsCombine
    , "th-roots" AE..= importsThRoots
    , "suppress-for-th" AE..= importsSuppressForTH
    ]

instance FromJSON ImportsConfig where
  parseJSON = withObject "ImportsConfig" $ \o -> do
    importsRemoveUnused              <- o .:? "remove-unused"              .!= True
    importsSuggestQualified          <- o .:? "suggest-qualified"          .!= []
    importsAllowUnqualifiedTypes     <- o .:? "allow-unqualified-types"    .!= True
    importsAllowUnqualifiedOperators <- o .:? "allow-unqualified-operators" .!= True
    importsRequireExplicit           <- o .:? "require-explicit"           .!= False
    importsCombine                   <- o .:? "combine"                    .!= True
    importsThRoots                   <- o .:? "th-roots"                   .!= []
    importsSuppressForTH             <- o .:? "suppress-for-th"            .!= True
    pure ImportsConfig{..}

instance ToJSON FixConfig where
  toJSON FixConfig{..} = object
    [ "enabled" AE..= fixEnabled
    , "safe-only" AE..= fixSafeOnly
    , "preview" AE..= fixPreview
    , "backup" AE..= fixBackup
    , "auto-imports" AE..= fixAutoImports
    ]

instance FromJSON FixConfig where
  parseJSON = withObject "FixConfig" $ \o -> do
    fixEnabled     <- o .:? "enabled"   .!= True
    fixSafeOnly    <- o .:? "safe-only" .!= True
    fixPreview     <- o .:? "preview"   .!= True
    fixBackup      <- o .:? "backup"    .!= True
    fixAutoImports <- o .:? "auto-imports" .!= defaultAutoFixImportsConfig
    pure FixConfig{..}

instance ToJSON AutoFixImportsConfig where
  toJSON AutoFixImportsConfig{..} = object
    [ "enabled" AE..= afiEnabled
    , "add-missing" AE..= afiAddMissing
    , "remove-unused" AE..= afiRemoveUnused
    , "organize" AE..= afiOrganize
    , "use-explicit" AE..= afiUseExplicit
    , "qualify-new" AE..= afiQualifyNew
    , "group-by-category" AE..= afiGroupByCategory
    ]

instance FromJSON AutoFixImportsConfig where
  parseJSON = withObject "AutoFixImportsConfig" $ \o -> do
    afiEnabled         <- o .:? "enabled"           .!= True
    afiAddMissing      <- o .:? "add-missing"       .!= True
    afiRemoveUnused    <- o .:? "remove-unused"     .!= True
    afiOrganize        <- o .:? "organize"          .!= False
    afiUseExplicit     <- o .:? "use-explicit"      .!= True
    afiQualifyNew      <- o .:? "qualify-new"       .!= False
    afiGroupByCategory <- o .:? "group-by-category" .!= False
    pure AutoFixImportsConfig{..}

instance ToJSON ComplexityConfig where
  toJSON ComplexityConfig{..} = object
    [ "enabled" AE..= compEnabled
    , "cyclomatic-warning" AE..= compCyclomaticWarning
    , "cyclomatic-error" AE..= compCyclomaticError
    , "cognitive-warning" AE..= compCognitiveWarning
    , "cognitive-error" AE..= compCognitiveError
    , "line-length-warning" AE..= compLineLengthWarning
    , "nesting-warning" AE..= compNestingWarning
    , "parameter-warning" AE..= compParameterWarning
    , "pattern-branch-warning" AE..= compPatternBranchWarning
    ]

instance FromJSON ComplexityConfig where
  parseJSON = withObject "ComplexityConfig" $ \o -> do
    compEnabled <- o .:? "enabled" .!= True
    compCyclomaticWarning <- o .:? "cyclomatic-warning" .!= 10
    compCyclomaticError <- o .:? "cyclomatic-error" .!= 20
    compCognitiveWarning <- o .:? "cognitive-warning" .!= 15
    compCognitiveError <- o .:? "cognitive-error" .!= 25
    compLineLengthWarning <- o .:? "line-length-warning" .!= 50
    compNestingWarning <- o .:? "nesting-warning" .!= 4
    compParameterWarning <- o .:? "parameter-warning" .!= 5
    compPatternBranchWarning <- o .:? "pattern-branch-warning" .!= 10
    pure ComplexityConfig{..}

instance ToJSON ResourceConfig where
  toJSON ResourceConfig{..} = object
    [ "timeout-seconds" AE..= resTimeoutSeconds
    , "max-memory-mb" AE..= resMaxMemoryMB
    , "force-gc-interval" AE..= resForceGCInterval
    , "track-per-file" AE..= resTrackPerFile
    , "warn-slow-files" AE..= resWarnSlowFiles
    , "max-retries" AE..= resMaxRetries
    , "kill-on-timeout" AE..= resKillOnTimeout
    ]

instance FromJSON ResourceConfig where
  parseJSON = withObject "ResourceConfig" $ \o -> do
    resTimeoutSeconds  <- o .:? "timeout-seconds"    .!= Just 60
    resMaxMemoryMB     <- o .:? "max-memory-mb"      .!= Just 2048
    resForceGCInterval <- o .:? "force-gc-interval"  .!= Just 100
    resTrackPerFile    <- o .:? "track-per-file"     .!= True
    resWarnSlowFiles   <- o .:? "warn-slow-files"    .!= Just 10.0
    resMaxRetries      <- o .:? "max-retries"        .!= 1
    resKillOnTimeout   <- o .:? "kill-on-timeout"    .!= True
    pure ResourceConfig{..}

instance ToJSON AliasStrategy where
  toJSON = \case
    LastPart      -> "last-part"
    FirstLetter   -> "first-letter"
    Initials      -> "initials"
    FirstNChars n -> object ["first-n-chars" AE..= n]

instance FromJSON AliasStrategy where
  parseJSON (String "last-part")    = pure LastPart
  parseJSON (String "first-letter") = pure FirstLetter
  parseJSON (String "initials")     = pure Initials
  parseJSON (Object o)              = FirstNChars <$> o .: "first-n-chars"
  parseJSON _                       = fail "Invalid alias strategy"

instance ToJSON QualifyImportConfig where
  toJSON QualifyImportConfig{..} = object
    [ "enabled" AE..= qicEnabled
    , "strategy" AE..= qicStrategy
    , "custom-aliases" AE..= qicCustomAliases
    , "max-alias-length" AE..= qicMaxAliasLength
    , "prefer-uppercase" AE..= qicPreferUppercase
    , "allow-conflicts" AE..= qicAllowConflicts
    ]

instance FromJSON QualifyImportConfig where
  parseJSON = withObject "QualifyImportConfig" $ \o -> do
    qicEnabled         <- o .:? "enabled"          .!= True
    qicStrategy        <- o .:? "strategy"         .!= LastPart
    qicCustomAliases   <- o .:? "custom-aliases"   .!= []
    qicMaxAliasLength  <- o .:? "max-alias-length"
    qicPreferUppercase <- o .:? "prefer-uppercase" .!= True
    qicAllowConflicts  <- o .:? "allow-conflicts"  .!= True
    pure QualifyImportConfig{..}

--------------------------------------------------------------------------------
-- Configuration Loading
--------------------------------------------------------------------------------

-- | Load configuration from default locations or specific file
loadConfig :: Maybe FilePath -> IO Config
loadConfig mpath = case mpath of
  Just path -> loadConfigFromFile path
  Nothing   -> findAndLoadConfig

-- | Load configuration with environment variable overrides
--
-- This function:
-- 1. Checks ARGUS_CONFIG for config file path (overrides the mpath argument)
-- 2. Loads configuration from file (or default)
-- 3. Applies environment variable overrides
--
-- Environment variables take precedence over config file values.
loadConfigWithEnv :: Maybe FilePath -> IO Config
loadConfigWithEnv mpath = do
  envCfg <- loadEnvConfig
  -- Use env config path if set, otherwise use provided path
  let effectivePath = envCfgPath envCfg <|> mpath
  baseCfg <- loadConfig effectivePath
  pure $ applyEnvToConfig envCfg baseCfg
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) Nothing y  = y
    (<|>) x       _  = x

-- | Apply environment configuration overrides to a Config
--
-- Each environment variable, when set, overrides the corresponding
-- config file value.
applyEnvToConfig :: EnvConfig -> Config -> Config
applyEnvToConfig EnvConfig{..} cfg = cfg
  { cfgGeneral = (cfgGeneral cfg)
      { genMode   = maybe (genMode $ cfgGeneral cfg) id envCfgMode
      , genHieDir = envCfgHieDir <|> genHieDir (cfgGeneral cfg)
      , genExclude = maybe (genExclude $ cfgGeneral cfg) id envCfgExclude
      }
  , cfgOutput = (cfgOutput cfg)
      { outFormat       = maybe (outFormat $ cfgOutput cfg) id envCfgOutputFormat
      , outColor        = maybe (outColor $ cfgOutput cfg) id envCfgColor
      , outContextLines = maybe (outContextLines $ cfgOutput cfg) id envCfgContextLines
      }
  , cfgResource = (cfgResource cfg)
      { resTimeoutSeconds = envCfgTimeout <|> resTimeoutSeconds (cfgResource cfg)
      , resMaxMemoryMB    = envCfgMemoryLimit <|> resMaxMemoryMB (cfgResource cfg)
      }
  }
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) Nothing y  = y
    (<|>) x       _  = x

-- | Try to find config in standard locations
findAndLoadConfig :: IO Config
findAndLoadConfig = do
  let candidates = ["linter.toml", ".linter.toml", "linter.yaml", ".linter.yaml", "config.yaml"]
  findFirst candidates
  where
    findFirst [] = pure defaultConfig
    findFirst (f:fs) = do
      exists <- doesFileExist f
      if exists
        then loadConfigFromFile f
        else findFirst fs

-- | Load configuration from a specific file
loadConfigFromFile :: FilePath -> IO Config
loadConfigFromFile path = do
  let ext = takeExtension path
  case ext of
    ".toml" -> loadTomlConfig path
    ".yaml" -> loadYamlConfig path
    ".yml"  -> loadYamlConfig path
    _       -> loadYamlConfig path  -- Default to YAML

-- | Load TOML configuration
loadTomlConfig :: FilePath -> IO Config
loadTomlConfig path = do
  content <- TIO.readFile path
  case TOML.decode configCodec content of
    Left err  -> error $ "Failed to parse TOML config: " <> show err
    Right cfg -> pure cfg

-- | Load YAML configuration (with legacy support)
loadYamlConfig :: FilePath -> IO Config
loadYamlConfig path = do
  result <- try @SomeException $ Yaml.decodeFileThrow path
  case result of
    Right cfg -> pure cfg
    Left _ -> do
      -- Try legacy format
      legacy <- loadLegacyConfig path
      pure $ convertLegacyConfig legacy

--------------------------------------------------------------------------------
-- TOML Codec
--------------------------------------------------------------------------------

configCodec :: TomlCodec Config
configCodec = Config
  <$> TOML.table generalCodec "general" .= cfgGeneral
  <*> TOML.table outputCodec  "output"  .= cfgOutput
  <*> TOML.table unusedCodec  "unused"  .= cfgUnused
  <*> TOML.table namingCodec  "naming"  .= cfgNaming
  <*> TOML.table patternsCodec "patterns" .= cfgPatterns
  <*> TOML.table importsCodec "imports" .= cfgImports
  <*> TOML.table fixCodec     "fix"     .= cfgFix
  <*> TOML.table complexityCodec "complexity" .= cfgComplexity
  <*> TOML.table resourceCodec "resource" .= cfgResource
  <*> TOML.table qualifyImportCodec "qualify-import" .= cfgQualifyImport
  <*> TOML.table architectureCodec "architecture" .= cfgArchitecture

generalCodec :: TomlCodec GeneralConfig
generalCodec = GeneralConfig
  <$> TOML.arrayOf TOML._String "directories" .= genDirectories
  <*> TOML.arrayOf TOML._Text "exclude" .= genExclude
  <*> TOML.text "mode" .= genMode
  <*> TOML.dioptional (TOML.string "hie-dir") .= genHieDir

outputCodec :: TomlCodec OutputConfig
outputCodec = OutputConfig
  <$> TOML.text "format" .= outFormat
  <*> TOML.bool "color" .= outColor
  <*> TOML.text "group-by" .= outGroupBy
  <*> TOML.bool "show-context" .= outShowContext
  <*> TOML.int "context-lines" .= outContextLines

unusedCodec :: TomlCodec UnusedConfig
unusedCodec = UnusedConfig
  <$> TOML.bool "enabled" .= unusedEnabled
  <*> TOML.bool "check-functions" .= unusedCheckFunctions
  <*> TOML.bool "check-types" .= unusedCheckTypes
  <*> TOML.bool "check-imports" .= unusedCheckImports
  <*> TOML.bool "check-exports" .= unusedCheckExports
  <*> TOML.bool "check-constructors" .= unusedCheckConstructors
  <*> TOML.bool "check-record-fields" .= unusedCheckRecordFields
  <*> TOML.bool "check-local-binds" .= unusedCheckLocalBinds
  <*> TOML.bool "check-instances" .= unusedCheckInstances
  <*> TOML.bool "typeclass-roots" .= unusedTypeClassRoots
  <*> TOML.bool "derive-roots" .= unusedDeriveRoots
  <*> TOML.double "min-confidence" .= unusedMinConfidence
  <*> TOML.arrayOf TOML._Text "roots" .= unusedRoots
  <*> TOML.arrayOf TOML._Text "th-roots" .= unusedThRoots
  <*> TOML.arrayOf TOML._Text "ignore-patterns" .= unusedIgnorePatterns

namingCodec :: TomlCodec NamingConfig
namingCodec = NamingConfig
  <$> TOML.bool "enabled" .= namingEnabled
  <*> TOML.list typeRuleCodec "types" .= namingTypes
  <*> TOML.list variableRuleCodec "variables" .= namingVariables

typeRuleCodec :: TomlCodec TypeRule
typeRuleCodec = TypeRule
  <$> TOML.text "pattern" .= trPattern
  <*> TOML.text "replacement" .= trReplacement
  <*> severityCodec "severity" .= trSeverity
  <*> TOML.dioptional (TOML.text "message") .= trMessage
  <*> TOML.dioptional (TOML.text "within") .= trWithin

variableRuleCodec :: TomlCodec VariableRule
variableRuleCodec = VariableRule
  <$> TOML.text "type" .= vrType
  <*> TOML.dioptional (TOML.text "from") .= vrFrom
  <*> TOML.text "to" .= vrTo
  <*> severityCodec "severity" .= vrSeverity
  <*> TOML.dioptional (TOML.text "message") .= vrMessage

patternsCodec :: TomlCodec PatternsConfig
patternsCodec = TOML.dimap toPatternsRaw fromPatternsRaw rawPatternsCodec
  where
    -- Internal type for TOML parsing
    rawPatternsCodec :: TomlCodec (Bool, [PatternRule])
    rawPatternsCodec = (,)
      <$> TOML.bool "enabled" .= fst
      <*> TOML.list patternRuleCodec "rules" .= snd

    toPatternsRaw :: PatternsConfig -> (Bool, [PatternRule])
    toPatternsRaw PatternsConfig{..} = (patternsEnabled, [])  -- Can't reverse convert rules

    fromPatternsRaw :: (Bool, [PatternRule]) -> PatternsConfig
    fromPatternsRaw (enabled, rawRules) = PatternsConfig
      { patternsEnabled = enabled
      , patternsRules = map patternRuleToRule rawRules
      }

patternRuleCodec :: TomlCodec PatternRule
patternRuleCodec = PatternRule
  <$> TOML.text "name" .= prName
  <*> TOML.text "match" .= prMatch
  <*> TOML.dioptional (TOML.text "fix") .= prFix
  <*> TOML.dioptional (TOML.text "where") .= prWhere
  <*> severityCodec "severity" .= prSeverity
  <*> TOML.text "message" .= prMessage

importsCodec :: TomlCodec ImportsConfig
importsCodec = ImportsConfig
  <$> TOML.bool "remove-unused" .= importsRemoveUnused
  <*> TOML.arrayOf TOML._Text "suggest-qualified" .= importsSuggestQualified
  <*> boolWithDefault True "allow-unqualified-types" .= importsAllowUnqualifiedTypes
  <*> boolWithDefault True "allow-unqualified-operators" .= importsAllowUnqualifiedOperators
  <*> TOML.bool "require-explicit" .= importsRequireExplicit
  <*> TOML.bool "combine" .= importsCombine
  <*> textArrayWithDefault [] "th-roots" .= importsThRoots
  <*> boolWithDefault True "suppress-for-th" .= importsSuppressForTH

-- | Text array codec with a default value
textArrayWithDefault :: [Text] -> TOML.Key -> TomlCodec [Text]
textArrayWithDefault def key = TOML.dimap Just (fromMaybe def) $ TOML.dioptional (TOML.arrayOf TOML._Text key)

-- | Bool codec with a default value
boolWithDefault :: Bool -> TOML.Key -> TomlCodec Bool
boolWithDefault def key = TOML.dimap Just (fromMaybe def) $ TOML.dioptional (TOML.bool key)

fixCodec :: TomlCodec FixConfig
fixCodec = FixConfig
  <$> TOML.bool "enabled" .= fixEnabled
  <*> TOML.bool "safe-only" .= fixSafeOnly
  <*> TOML.bool "preview" .= fixPreview
  <*> TOML.bool "backup" .= fixBackup
  <*> TOML.table autoFixImportsCodec "auto-imports" .= fixAutoImports

autoFixImportsCodec :: TomlCodec AutoFixImportsConfig
autoFixImportsCodec = AutoFixImportsConfig
  <$> boolWithDefault True "enabled" .= afiEnabled
  <*> boolWithDefault True "add-missing" .= afiAddMissing
  <*> boolWithDefault True "remove-unused" .= afiRemoveUnused
  <*> boolWithDefault False "organize" .= afiOrganize
  <*> boolWithDefault True "use-explicit" .= afiUseExplicit
  <*> boolWithDefault False "qualify-new" .= afiQualifyNew
  <*> boolWithDefault False "group-by-category" .= afiGroupByCategory

complexityCodec :: TomlCodec ComplexityConfig
complexityCodec = ComplexityConfig
  <$> TOML.bool "enabled" .= compEnabled
  <*> TOML.int "cyclomatic-warning" .= compCyclomaticWarning
  <*> TOML.int "cyclomatic-error" .= compCyclomaticError
  <*> TOML.int "cognitive-warning" .= compCognitiveWarning
  <*> TOML.int "cognitive-error" .= compCognitiveError
  <*> TOML.int "line-length-warning" .= compLineLengthWarning
  <*> TOML.int "nesting-warning" .= compNestingWarning
  <*> TOML.int "parameter-warning" .= compParameterWarning
  <*> TOML.int "pattern-branch-warning" .= compPatternBranchWarning

resourceCodec :: TomlCodec ResourceConfig
resourceCodec = ResourceConfig
  <$> TOML.dioptional (TOML.int "timeout-seconds") .= resTimeoutSeconds
  <*> TOML.dioptional (TOML.int "max-memory-mb") .= resMaxMemoryMB
  <*> TOML.dioptional (TOML.int "force-gc-interval") .= resForceGCInterval
  <*> boolWithDefault True "track-per-file" .= resTrackPerFile
  <*> TOML.dioptional (TOML.double "warn-slow-files") .= resWarnSlowFiles
  <*> intWithDefault 1 "max-retries" .= resMaxRetries
  <*> boolWithDefault True "kill-on-timeout" .= resKillOnTimeout

-- | Int codec with a default value
intWithDefault :: Int -> TOML.Key -> TomlCodec Int
intWithDefault def key = TOML.dimap Just (fromMaybe def) $ TOML.dioptional (TOML.int key)

severityCodec :: TOML.Key -> TomlCodec RuleSeverity
severityCodec key = TOML.textBy toText fromText key
  where
    toText = \case
      RSError      -> "error"
      RSWarning    -> "warning"
      RSSuggestion -> "suggestion"
      RSInfo       -> "info"
    fromText t = case T.toLower t of
      "error"      -> Right RSError
      "warning"    -> Right RSWarning
      "suggestion" -> Right RSSuggestion
      "info"       -> Right RSInfo
      _            -> Left $ "Invalid severity: " <> t

qualifyImportCodec :: TomlCodec QualifyImportConfig
qualifyImportCodec = QualifyImportConfig
  <$> boolWithDefault True "enabled" .= qicEnabled
  <*> aliasStrategyCodec "strategy" .= qicStrategy
  <*> customAliasListCodec "custom-aliases" .= qicCustomAliases
  <*> TOML.dioptional (TOML.int "max-alias-length") .= qicMaxAliasLength
  <*> boolWithDefault True "prefer-uppercase" .= qicPreferUppercase
  <*> boolWithDefault True "allow-conflicts" .= qicAllowConflicts

aliasStrategyCodec :: TOML.Key -> TomlCodec AliasStrategy
aliasStrategyCodec key = TOML.textBy toText fromText key
  where
    toText = \case
      LastPart      -> "last-part"
      FirstLetter   -> "first-letter"
      Initials      -> "initials"
      FirstNChars n -> "first-" <> T.pack (show n) <> "-chars"
    fromText t = case T.toLower t of
      "last-part"     -> Right LastPart
      "first-letter"  -> Right FirstLetter
      "initials"      -> Right Initials
      _ | "first-" `T.isPrefixOf` t && "-chars" `T.isSuffixOf` t ->
          case reads (T.unpack $ T.dropEnd 6 $ T.drop 6 t) of
            [(n, "")] -> Right (FirstNChars n)
            _         -> Left $ "Invalid first-n-chars: " <> t
        | otherwise -> Left $ "Invalid alias strategy: " <> t

-- | Codec for custom alias list (list of [module, alias] pairs)
customAliasListCodec :: TOML.Key -> TomlCodec [(Text, Text)]
customAliasListCodec key = TOML.dimap toList fromList $ TOML.list customAliasCodec key
  where
    toList = map (\(m, a) -> CustomAlias m a)
    fromList = map (\(CustomAlias m a) -> (m, a))

-- | Helper type for TOML codec
data CustomAlias = CustomAlias { caModule :: Text, caAlias :: Text }

customAliasCodec :: TomlCodec CustomAlias
customAliasCodec = CustomAlias
  <$> TOML.text "module" .= caModule
  <*> TOML.text "alias" .= caAlias

-- | TOML codec for architecture configuration
architectureCodec :: TomlCodec ArchitectureConfig
architectureCodec = ArchitectureConfig
  <$> boolWithDefault True "enabled" .= acEnabled
  <*> layerListCodec "layers" .= acLayers
  <*> intWithDefault 10 "max-cycle-length" .= acMaxCycleLength
  <*> doubleWithDefault 0.8 "instability-threshold" .= acInstabilityThreshold
  <*> intWithDefault 15 "coupling-threshold" .= acCouplingThreshold
  <*> boolWithDefault True "check-orphans" .= acCheckOrphans
  <*> boolWithDefault True "check-qualified" .= acCheckQualified

-- | TOML codec for layer configuration list
layerListCodec :: TOML.Key -> TomlCodec [LayerConfig]
layerListCodec key = TOML.list layerCodec key

-- | TOML codec for a single layer configuration
layerCodec :: TomlCodec LayerConfig
layerCodec = LayerConfig
  <$> TOML.text "name" .= lcName
  <*> TOML.arrayOf TOML._Text "patterns" .= lcPatterns
  <*> TOML.arrayOf TOML._Text "can-import" .= lcCanImport

-- | Double codec with a default value
doubleWithDefault :: Double -> TOML.Key -> TomlCodec Double
doubleWithDefault def key = TOML.dimap Just (fromMaybe def) $ TOML.dioptional (TOML.double key)

--------------------------------------------------------------------------------
-- Legacy YAML Configuration (backwards compatibility)
--------------------------------------------------------------------------------

-- | Legacy configuration format (the old config.yaml)
data LegacyConfig = LegacyConfig
  { legacySignatures :: [LegacySignature]
  , legacyVariables  :: [LegacyVariable]
  , legacyInPlace    :: Bool
  , legacyDirectory  :: FilePath
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON LegacyConfig where
  parseJSON = withObject "LegacyConfig" $ \o -> do
    legacySignatures <- o .:? "signatures" .!= []
    legacyVariables  <- o .:? "variables"  .!= []
    legacyInPlace    <- o .:? "in-place"   .!= False
    legacyDirectory  <- o .:? "directory"  .!= ""
    pure LegacyConfig{..}

data LegacySignature = LegacySignature
  { lsFrom   :: Text
  , lsTo     :: Text
  , lsWithin :: Maybe Text
  , lsMsg    :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON LegacySignature where
  parseJSON = withObject "LegacySignature" $ \o -> do
    lsFrom   <- o .: "from"
    lsTo     <- o .: "to"
    lsWithin <- o .:? "within"
    lsMsg    <- o .:? "msg"
    pure LegacySignature{..}

data LegacyVariable = LegacyVariable
  { lvType :: Text
  , lvFrom :: Maybe Text
  , lvTo   :: Text
  , lvMsg  :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON LegacyVariable where
  parseJSON = withObject "LegacyVariable" $ \o -> do
    lvType <- o .: "type"
    lvFrom <- o .:? "from"
    lvTo   <- o .: "to"
    lvMsg  <- o .:? "msg"
    pure LegacyVariable{..}

-- | Load legacy YAML configuration
loadLegacyConfig :: FilePath -> IO LegacyConfig
loadLegacyConfig = Yaml.decodeFileThrow

-- | Convert legacy config to new format
convertLegacyConfig :: LegacyConfig -> Config
convertLegacyConfig LegacyConfig{..} = defaultConfig
  { cfgNaming = (cfgNaming defaultConfig)
      { namingTypes = map convertSignature legacySignatures
      , namingVariables = map convertVariable legacyVariables
      }
  , cfgFix = (cfgFix defaultConfig)
      { fixEnabled = legacyInPlace
      }
  , cfgGeneral = (cfgGeneral defaultConfig)
      { genDirectories = if null legacyDirectory then ["src", "app"] else [legacyDirectory]
      }
  }
  where
    convertSignature LegacySignature{..} = TypeRule
      { trPattern     = lsFrom
      , trReplacement = lsTo
      , trSeverity    = RSWarning
      , trMessage     = lsMsg
      , trWithin      = lsWithin
      }

    convertVariable LegacyVariable{..} = VariableRule
      { vrType     = lvType
      , vrFrom     = lvFrom
      , vrTo       = lvTo
      , vrSeverity = RSWarning
      , vrMessage  = lvMsg
      }
