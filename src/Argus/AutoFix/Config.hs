{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Argus.AutoFix.Config
-- Description : Configuration types and TOML loading for auto-fix system
-- Copyright   : (c) Quinten, 2024
-- License     : MIT
-- Maintainer  : quinten@example.com
-- Stability   : stable
-- Portability : GHC
--
-- = Overview
--
-- This module provides comprehensive configuration support for the auto-fix
-- system, including TOML-based configuration loading, engine-specific settings,
-- and pipeline customization.
--
-- = Configuration Sources
--
-- Configuration is loaded from multiple sources in order of priority:
--
-- 1. @argus.toml@ (project root)
-- 2. @linter.toml@ (project root)
-- 3. @.argus.toml@ (hidden config)
--
-- = Key Configuration Sections
--
-- * __Global settings__: Enable/disable, dry-run mode, backup options
-- * __Engine configuration__: Per-engine settings (boolean, list, monad, partial)
-- * __Pipeline configuration__: Filtering, sorting, deduplication
-- * __Safety configuration__: Type checking, parsing validation
-- * __Output configuration__: Diff format, verbosity, logging
--
-- = Example Configuration (TOML)
--
-- @
-- enabled = true
-- dry_run = false
-- min_confidence = 70
-- max_fixes_per_file = 100
--
-- [engines.boolean]
-- enabled = true
-- simplify_not = true
-- simplify_and = true
--
-- [pipeline]
-- deduplicate = true
-- validate_all = true
--
-- [safety]
-- require_type_check = true
-- preserve_comments = true
-- @
--
-- @since 1.0.0
module Argus.AutoFix.Config
  ( -- * Main Configuration Types
    AutoFixConfig (..)
  , defaultAutoFixConfig

    -- * Engine Configuration
  , EngineConfig (..)
  , defaultEngineConfig
  , BooleanEngineConfig (..)
  , defaultBooleanEngineConfig
  , ListEngineConfig (..)
  , defaultListEngineConfig
  , MonadEngineConfig (..)
  , defaultMonadEngineConfig
  , PartialEngineConfig (..)
  , defaultPartialEngineConfig

    -- * Pipeline Configuration
  , PipelineConfig (..)
  , defaultPipelineConfig
  , FilterConfig (..)
  , SortConfig (..)
  , SortCriterion (..)
  , SortDirection (..)

    -- * Safety Configuration
  , SafetyConfig (..)
  , defaultSafetyConfig

    -- * Output Configuration
  , OutputConfig (..)
  , defaultOutputConfig
  , DiffFormat (..)

    -- * Rule-Specific Fix Settings
  , RuleFixConfig (..)
  , defaultRuleFixConfig

    -- * Loading and Parsing
  , loadAutoFixConfig
  , loadAutoFixConfigFromFile
  , parseAutoFixConfig
  , mergeConfigs

    -- * Validation
  , validateConfig
  , ConfigValidationError (..)

    -- * Serialization
  , encodeAutoFixConfig
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:?), object)
import Data.Aeson qualified as Aeson
import Data.Aeson qualified as AE ((.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)
import Toml (TomlCodec, (.=))
import Toml qualified

import Argus.AutoFix.Types
  ( Confidence (..)
  , unConfidenceValue
  )
import Argus.Rules.Types
  ( SafetyLevel (..)
  , safetyToText
  , textToSafety
  )
import Argus.Types
  ( FixCategory (..)
  )

--------------------------------------------------------------------------------
-- Main Configuration Types
--------------------------------------------------------------------------------

-- | Main configuration for the entire auto-fix system
data AutoFixConfig = AutoFixConfig
  { afcEnabled           :: !Bool
    -- ^ Whether auto-fix is enabled globally
  , afcDryRun            :: !Bool
    -- ^ If true, show fixes but don't apply them
  , afcInteractive       :: !Bool
    -- ^ If true, prompt for confirmation before each fix
  , afcBackupOriginal    :: !Bool
    -- ^ If true, create backup files before modifying
  , afcMaxFixesPerFile   :: !Int
    -- ^ Maximum number of fixes to apply per file
  , afcMaxFixesTotal     :: !Int
    -- ^ Maximum total fixes across all files
  , afcMinConfidence     :: !Confidence
    -- ^ Minimum confidence threshold for applying fixes
  , afcAllowedSafety     :: !(Set SafetyLevel)
    -- ^ Safety levels that are allowed to be applied
  , afcEnabledCategories :: !(Set FixCategory)
    -- ^ Which fix categories are enabled
  , afcDisabledRules     :: !(Set Text)
    -- ^ Rule IDs that should not have fixes applied
  , afcEngines           :: !EngineConfigs
    -- ^ Per-engine configuration
  , afcPipeline          :: !PipelineConfig
    -- ^ Pipeline configuration
  , afcSafety            :: !SafetyConfig
    -- ^ Safety-related settings
  , afcOutput            :: !OutputConfig
    -- ^ Output and reporting settings
  , afcRuleOverrides     :: !(Map Text RuleFixConfig)
    -- ^ Per-rule configuration overrides
  } deriving stock (Eq, Show, Generic)

instance ToJSON AutoFixConfig where
  toJSON AutoFixConfig{..} = object
    [ "enabled" AE..= afcEnabled
    , "dry_run" AE..= afcDryRun
    , "interactive" AE..= afcInteractive
    , "backup_original" AE..= afcBackupOriginal
    , "max_fixes_per_file" AE..= afcMaxFixesPerFile
    , "max_fixes_total" AE..= afcMaxFixesTotal
    , "min_confidence" AE..= afcMinConfidence
    , "allowed_safety" AE..= Set.toList afcAllowedSafety
    , "enabled_categories" AE..= Set.toList afcEnabledCategories
    , "disabled_rules" AE..= Set.toList afcDisabledRules
    , "engines" AE..= afcEngines
    , "pipeline" AE..= afcPipeline
    , "safety" AE..= afcSafety
    , "output" AE..= afcOutput
    , "rule_overrides" AE..= afcRuleOverrides
    ]

instance FromJSON AutoFixConfig where
  parseJSON = Aeson.withObject "AutoFixConfig" $ \o -> do
    afcEnabled <- o .:? "enabled" .!= True
    afcDryRun <- o .:? "dry_run" .!= False
    afcInteractive <- o .:? "interactive" .!= False
    afcBackupOriginal <- o .:? "backup_original" .!= True
    afcMaxFixesPerFile <- o .:? "max_fixes_per_file" .!= 100
    afcMaxFixesTotal <- o .:? "max_fixes_total" .!= 1000
    afcMinConfidence <- o .:? "min_confidence" .!= Confidence 70
    safetyList <- o .:? "allowed_safety" .!= [Safe, MostlySafe, NeedsReview]
    afcAllowedSafety <- pure $ Set.fromList safetyList
    catList <- o .:? "enabled_categories" .!= defaultCategories
    afcEnabledCategories <- pure $ Set.fromList catList
    disabledList <- o .:? "disabled_rules" .!= []
    afcDisabledRules <- pure $ Set.fromList disabledList
    afcEngines <- o .:? "engines" .!= defaultEngineConfigs
    afcPipeline <- o .:? "pipeline" .!= defaultPipelineConfig
    afcSafety <- o .:? "safety" .!= defaultSafetyConfig
    afcOutput <- o .:? "output" .!= defaultOutputConfig
    afcRuleOverrides <- o .:? "rule_overrides" .!= Map.empty
    pure AutoFixConfig{..}
    where
      defaultCategories = [FCPerformance, FCModernize, FCSafety, FCStyle, FCImports, FCRedundant, FCSpaceLeaks, FCSecurity]

-- | Default auto-fix configuration
defaultAutoFixConfig :: AutoFixConfig
defaultAutoFixConfig = AutoFixConfig
  { afcEnabled = True
  , afcDryRun = False
  , afcInteractive = False
  , afcBackupOriginal = True
  , afcMaxFixesPerFile = 100
  , afcMaxFixesTotal = 1000
  , afcMinConfidence = Confidence 70
  , afcAllowedSafety = Set.fromList [Safe, MostlySafe, NeedsReview]
  , afcEnabledCategories = Set.fromList [FCPerformance, FCModernize, FCSafety, FCStyle, FCImports, FCRedundant, FCSpaceLeaks, FCSecurity]
  , afcDisabledRules = Set.empty
  , afcEngines = defaultEngineConfigs
  , afcPipeline = defaultPipelineConfig
  , afcSafety = defaultSafetyConfig
  , afcOutput = defaultOutputConfig
  , afcRuleOverrides = Map.empty
  }

--------------------------------------------------------------------------------
-- Engine Configuration
--------------------------------------------------------------------------------

-- | Generic engine configuration
data EngineConfig = EngineConfig
  { ecEnabled      :: !Bool
    -- ^ Whether this engine is enabled
  , ecPriority     :: !Int
    -- ^ Priority for this engine (higher = processed first)
  , ecMaxFixes     :: !Int
    -- ^ Maximum fixes from this engine
  , ecTags         :: !(Set Text)
    -- ^ Tags to add to all fixes from this engine
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Default engine configuration
defaultEngineConfig :: EngineConfig
defaultEngineConfig = EngineConfig
  { ecEnabled = True
  , ecPriority = 50
  , ecMaxFixes = 500
  , ecTags = Set.empty
  }

-- | Collection of engine-specific configurations
data EngineConfigs = EngineConfigs
  { ecBoolean :: !BooleanEngineConfig
  , ecList    :: !ListEngineConfig
  , ecMonad   :: !MonadEngineConfig
  , ecPartial :: !PartialEngineConfig
  , ecCustom  :: !(Map Text Aeson.Value)
    -- ^ Custom engine configs stored as JSON
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Default engine configurations
defaultEngineConfigs :: EngineConfigs
defaultEngineConfigs = EngineConfigs
  { ecBoolean = defaultBooleanEngineConfig
  , ecList = defaultListEngineConfig
  , ecMonad = defaultMonadEngineConfig
  , ecPartial = defaultPartialEngineConfig
  , ecCustom = Map.empty
  }

-- | Boolean simplification engine configuration
data BooleanEngineConfig = BooleanEngineConfig
  { becBase             :: !EngineConfig
    -- ^ Base engine settings
  , becSimplifyNot      :: !Bool
    -- ^ Simplify double negation (not (not x) -> x)
  , becSimplifyAnd      :: !Bool
    -- ^ Simplify and expressions (x && True -> x)
  , becSimplifyOr       :: !Bool
    -- ^ Simplify or expressions (x || False -> x)
  , becSimplifyIf       :: !Bool
    -- ^ Simplify if-then-else (if True then x else y -> x)
  , becSimplifyComparison :: !Bool
    -- ^ Simplify comparisons (x == True -> x)
  , becPreserveComments :: !Bool
    -- ^ Preserve comments in simplified expressions
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Default boolean engine configuration
defaultBooleanEngineConfig :: BooleanEngineConfig
defaultBooleanEngineConfig = BooleanEngineConfig
  { becBase = defaultEngineConfig { ecPriority = 60 }
  , becSimplifyNot = True
  , becSimplifyAnd = True
  , becSimplifyOr = True
  , becSimplifyIf = True
  , becSimplifyComparison = True
  , becPreserveComments = True
  }

-- | List operation engine configuration
data ListEngineConfig = ListEngineConfig
  { lecBase              :: !EngineConfig
    -- ^ Base engine settings
  , lecOptimizeConcat    :: !Bool
    -- ^ Optimize list concatenation
  , lecUseListComprehensions :: !Bool
    -- ^ Suggest list comprehensions where appropriate
  , lecPreferFoldr       :: !Bool
    -- ^ Prefer foldr over foldl for lazy evaluation
  , lecInlineSingleUse   :: !Bool
    -- ^ Inline single-use list bindings
  , lecMaxInlineLength   :: !Int
    -- ^ Maximum expression length for inlining
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Default list engine configuration
defaultListEngineConfig :: ListEngineConfig
defaultListEngineConfig = ListEngineConfig
  { lecBase = defaultEngineConfig { ecPriority = 55 }
  , lecOptimizeConcat = True
  , lecUseListComprehensions = True
  , lecPreferFoldr = True
  , lecInlineSingleUse = True
  , lecMaxInlineLength = 80
  }

-- | Monad transformation engine configuration
data MonadEngineConfig = MonadEngineConfig
  { mecBase              :: !EngineConfig
    -- ^ Base engine settings
  , mecSimplifyBind      :: !Bool
    -- ^ Simplify bind operations (x >>= return -> x)
  , mecSimplifyReturn    :: !Bool
    -- ^ Simplify return (return x >>= f -> f x)
  , mecUseFmap           :: !Bool
    -- ^ Use fmap instead of bind where possible
  , mecUseApplicative    :: !Bool
    -- ^ Use Applicative style where possible
  , mecPreferDo          :: !Bool
    -- ^ Prefer do-notation for complex chains
  , mecMinChainLength    :: !Int
    -- ^ Minimum chain length before suggesting do-notation
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Default monad engine configuration
defaultMonadEngineConfig :: MonadEngineConfig
defaultMonadEngineConfig = MonadEngineConfig
  { mecBase = defaultEngineConfig { ecPriority = 50 }
  , mecSimplifyBind = True
  , mecSimplifyReturn = True
  , mecUseFmap = True
  , mecUseApplicative = True
  , mecPreferDo = True
  , mecMinChainLength = 3
  }

-- | Partial function replacement engine configuration
data PartialEngineConfig = PartialEngineConfig
  { pecBase              :: !EngineConfig
    -- ^ Base engine settings
  , pecReplaceHead       :: !Bool
    -- ^ Replace head with safe alternatives
  , pecReplaceTail       :: !Bool
    -- ^ Replace tail with safe alternatives
  , pecReplaceInit       :: !Bool
    -- ^ Replace init with safe alternatives
  , pecReplaceLast       :: !Bool
    -- ^ Replace last with safe alternatives
  , pecReplaceFromJust   :: !Bool
    -- ^ Replace fromJust with safe alternatives
  , pecReplaceRead       :: !Bool
    -- ^ Replace read with readMaybe
  , pecPreferPatternMatch :: !Bool
    -- ^ Prefer pattern matching over maybe/either functions
  , pecSuggestNonEmpty   :: !Bool
    -- ^ Suggest NonEmpty for head/tail replacement
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Default partial engine configuration
defaultPartialEngineConfig :: PartialEngineConfig
defaultPartialEngineConfig = PartialEngineConfig
  { pecBase = defaultEngineConfig { ecPriority = 70 }
  , pecReplaceHead = True
  , pecReplaceTail = True
  , pecReplaceInit = True
  , pecReplaceLast = True
  , pecReplaceFromJust = True
  , pecReplaceRead = True
  , pecPreferPatternMatch = False
  , pecSuggestNonEmpty = True
  }

--------------------------------------------------------------------------------
-- Pipeline Configuration
--------------------------------------------------------------------------------

-- | Pipeline configuration
data PipelineConfig = PipelineConfig
  { pcFilters      :: !FilterConfig
    -- ^ Filtering configuration
  , pcSort         :: !SortConfig
    -- ^ Sorting configuration
  , pcDeduplicate  :: !Bool
    -- ^ Whether to deduplicate overlapping fixes
  , pcLimit        :: !(Maybe Int)
    -- ^ Optional limit on total fixes in pipeline
  , pcStopOnEmpty  :: !Bool
    -- ^ Stop pipeline if no fixes remain after a stage
  , pcValidateAll  :: !Bool
    -- ^ Validate all fixes before applying
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Default pipeline configuration
defaultPipelineConfig :: PipelineConfig
defaultPipelineConfig = PipelineConfig
  { pcFilters = defaultFilterConfig
  , pcSort = defaultSortConfig
  , pcDeduplicate = True
  , pcLimit = Nothing
  , pcStopOnEmpty = True
  , pcValidateAll = True
  }

-- | Filter configuration
data FilterConfig = FilterConfig
  { fcMinConfidence     :: !(Maybe Int)
    -- ^ Minimum confidence (0-100)
  , fcMaxConfidence     :: !(Maybe Int)
    -- ^ Maximum confidence (0-100)
  , fcAllowedCategories :: !(Maybe (Set FixCategory))
    -- ^ If set, only allow these categories
  , fcBlockedCategories :: !(Maybe (Set FixCategory))
    -- ^ If set, block these categories
  , fcAllowedEngines    :: !(Maybe (Set Text))
    -- ^ If set, only allow fixes from these engines
  , fcBlockedEngines    :: !(Maybe (Set Text))
    -- ^ If set, block fixes from these engines
  , fcRequiredTags      :: !(Maybe (Set Text))
    -- ^ If set, require these tags
  , fcBlockedTags       :: !(Maybe (Set Text))
    -- ^ If set, block fixes with these tags
  , fcPathPatterns      :: !(Maybe [Text])
    -- ^ Glob patterns for file paths to include
  , fcExcludePatterns   :: !(Maybe [Text])
    -- ^ Glob patterns for file paths to exclude
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Default filter configuration (no filters applied)
defaultFilterConfig :: FilterConfig
defaultFilterConfig = FilterConfig
  { fcMinConfidence = Nothing
  , fcMaxConfidence = Nothing
  , fcAllowedCategories = Nothing
  , fcBlockedCategories = Nothing
  , fcAllowedEngines = Nothing
  , fcBlockedEngines = Nothing
  , fcRequiredTags = Nothing
  , fcBlockedTags = Nothing
  , fcPathPatterns = Nothing
  , fcExcludePatterns = Nothing
  }

-- | Sort configuration
data SortConfig = SortConfig
  { scCriteria  :: ![SortCriterion]
    -- ^ Sort criteria in order of priority
  , scDirection :: !SortDirection
    -- ^ Overall sort direction
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Default sort configuration
defaultSortConfig :: SortConfig
defaultSortConfig = SortConfig
  { scCriteria = [SortByConfidence, SortBySafety, SortBySpan]
  , scDirection = Descending
  }

-- | Sort criterion
data SortCriterion
  = SortByConfidence
  | SortBySafety
  | SortBySpan
  | SortByEngine
  | SortByCategory
  | SortByFile
  deriving stock (Eq, Show, Generic, Ord, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

-- | Sort direction
data SortDirection
  = Ascending
  | Descending
  deriving stock (Eq, Show, Generic, Ord, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Safety Configuration
--------------------------------------------------------------------------------

-- | Safety-related configuration
data SafetyConfig = SafetyConfig
  { scRequireTypeCheck     :: !Bool
    -- ^ Require type checking before applying fixes
  , scRequireParseCheck    :: !Bool
    -- ^ Require parse checking before applying fixes
  , scMaxUnsafePerFile     :: !Int
    -- ^ Maximum unsafe fixes per file
  , scRequireConfirmUnsafe :: !Bool
    -- ^ Require confirmation for unsafe fixes
  , scBlockDestructive     :: !Bool
    -- ^ Block destructive fixes (deletions, major changes)
  , scPreserveComments     :: !Bool
    -- ^ Preserve comments when refactoring
  , scPreserveFormatting   :: !Bool
    -- ^ Preserve formatting when refactoring
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Default safety configuration
defaultSafetyConfig :: SafetyConfig
defaultSafetyConfig = SafetyConfig
  { scRequireTypeCheck = True
  , scRequireParseCheck = True
  , scMaxUnsafePerFile = 5
  , scRequireConfirmUnsafe = True
  , scBlockDestructive = False
  , scPreserveComments = True
  , scPreserveFormatting = True
  }

--------------------------------------------------------------------------------
-- Output Configuration
--------------------------------------------------------------------------------

-- | Output and reporting configuration
data OutputConfig = OutputConfig
  { ocShowDiff        :: !Bool
    -- ^ Show diff for each fix
  , ocDiffFormat      :: !DiffFormat
    -- ^ Format for diffs
  , ocShowExplanation :: !Bool
    -- ^ Show explanation for each fix
  , ocShowConfidence  :: !Bool
    -- ^ Show confidence scores
  , ocGroupByFile     :: !Bool
    -- ^ Group output by file
  , ocColorOutput     :: !Bool
    -- ^ Use colored output
  , ocVerbosity       :: !Int
    -- ^ Verbosity level (0-3)
  , ocLogFile         :: !(Maybe FilePath)
    -- ^ Optional log file for fix operations
  , ocReportFile      :: !(Maybe FilePath)
    -- ^ Optional report file (JSON/HTML)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Default output configuration
defaultOutputConfig :: OutputConfig
defaultOutputConfig = OutputConfig
  { ocShowDiff = True
  , ocDiffFormat = UnifiedDiff
  , ocShowExplanation = True
  , ocShowConfidence = True
  , ocGroupByFile = True
  , ocColorOutput = True
  , ocVerbosity = 1
  , ocLogFile = Nothing
  , ocReportFile = Nothing
  }

-- | Diff format options
data DiffFormat
  = UnifiedDiff
  | SideBySide
  | InlineHighlight
  | MinimalDiff
  deriving stock (Eq, Show, Generic, Ord, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Rule-Specific Fix Settings
--------------------------------------------------------------------------------

-- | Per-rule fix configuration
data RuleFixConfig = RuleFixConfig
  { rfcEnabled         :: !Bool
    -- ^ Whether fixes for this rule are enabled
  , rfcMinConfidence   :: !(Maybe Confidence)
    -- ^ Override minimum confidence for this rule
  , rfcSafetyOverride  :: !(Maybe SafetyLevel)
    -- ^ Override safety level for this rule
  , rfcCategoryOverride :: !(Maybe FixCategory)
    -- ^ Override category for this rule
  , rfcCustomConfig    :: !(Maybe Aeson.Value)
    -- ^ Custom JSON configuration for the rule
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Default rule fix configuration
defaultRuleFixConfig :: RuleFixConfig
defaultRuleFixConfig = RuleFixConfig
  { rfcEnabled = True
  , rfcMinConfidence = Nothing
  , rfcSafetyOverride = Nothing
  , rfcCategoryOverride = Nothing
  , rfcCustomConfig = Nothing
  }

--------------------------------------------------------------------------------
-- Loading and Parsing
--------------------------------------------------------------------------------

-- | Load auto-fix configuration from default locations
-- Searches in order: ./argus.toml, ./linter.toml, ~/.config/argus/config.toml
loadAutoFixConfig :: IO AutoFixConfig
loadAutoFixConfig = do
  let candidates =
        [ "argus.toml"
        , "linter.toml"
        , ".argus.toml"
        ]
  findFirst candidates
  where
    findFirst [] = pure defaultAutoFixConfig
    findFirst (path:rest) = do
      exists <- doesFileExist path
      if exists
        then loadAutoFixConfigFromFile path
        else findFirst rest

-- | Load auto-fix configuration from a specific file
loadAutoFixConfigFromFile :: FilePath -> IO AutoFixConfig
loadAutoFixConfigFromFile path = do
  exists <- doesFileExist path
  if not exists
    then pure defaultAutoFixConfig
    else case takeExtension path of
      ".toml" -> loadFromToml path
      ".yaml" -> loadFromYaml path
      ".yml"  -> loadFromYaml path
      ".json" -> loadFromJson path
      _       -> pure defaultAutoFixConfig

-- | Load configuration from TOML file
loadFromToml :: FilePath -> IO AutoFixConfig
loadFromToml path = do
  result <- Toml.decodeFileEither autoFixConfigCodec path
  case result of
    Left err -> do
      putStrLn $ "Warning: Failed to parse TOML config: " <> show err
      pure defaultAutoFixConfig
    Right config -> pure config

-- | Load configuration from YAML file
loadFromYaml :: FilePath -> IO AutoFixConfig
loadFromYaml path = do
  content <- TIO.readFile path
  case Aeson.decodeStrict (encodeUtf8 content) of
    Nothing -> do
      putStrLn "Warning: Failed to parse YAML config"
      pure defaultAutoFixConfig
    Just config -> pure config

-- | Load configuration from JSON file
loadFromJson :: FilePath -> IO AutoFixConfig
loadFromJson path = do
  content <- TIO.readFile path
  case Aeson.decodeStrict (encodeUtf8 content) of
    Nothing -> do
      putStrLn "Warning: Failed to parse JSON config"
      pure defaultAutoFixConfig
    Just config -> pure config

-- | Parse auto-fix configuration from TOML text
parseAutoFixConfig :: Text -> Either Text AutoFixConfig
parseAutoFixConfig content =
  case Toml.decode autoFixConfigCodec content of
    Left err -> Left $ T.pack $ show err
    Right config -> Right config

-- | TOML codec for AutoFixConfig
autoFixConfigCodec :: TomlCodec AutoFixConfig
autoFixConfigCodec = AutoFixConfig
  <$> Toml.bool "enabled" Toml..= afcEnabled
  <*> Toml.bool "dry_run" Toml..= afcDryRun
  <*> Toml.bool "interactive" Toml..= afcInteractive
  <*> Toml.bool "backup_original" Toml..= afcBackupOriginal
  <*> Toml.int "max_fixes_per_file" Toml..= afcMaxFixesPerFile
  <*> Toml.int "max_fixes_total" Toml..= afcMaxFixesTotal
  <*> confidenceCodec "min_confidence" Toml..= afcMinConfidence
  <*> safetySetCodec "allowed_safety" Toml..= afcAllowedSafety
  <*> categorySetCodec "enabled_categories" Toml..= afcEnabledCategories
  <*> textSetCodec "disabled_rules" Toml..= afcDisabledRules
  <*> Toml.table engineConfigsCodec "engines" Toml..= afcEngines
  <*> Toml.table pipelineConfigCodec "pipeline" Toml..= afcPipeline
  <*> Toml.table safetyConfigCodec "safety" Toml..= afcSafety
  <*> Toml.table outputConfigCodec "output" Toml..= afcOutput
  <*> ruleOverridesCodec "rule_overrides" Toml..= afcRuleOverrides

-- | Codec for Confidence
confidenceCodec :: Toml.Key -> TomlCodec Confidence
confidenceCodec key = Toml.dimap unConfidenceValue Confidence (Toml.double key)

-- | Codec for Set SafetyLevel
safetySetCodec :: Toml.Key -> TomlCodec (Set SafetyLevel)
safetySetCodec key = Toml.dimap (map safetyLevelToText . Set.toList) (Set.fromList . map textToSafetyLevel) (Toml.arrayOf Toml._Text key)

-- | Codec for Set FixCategory
categorySetCodec :: Toml.Key -> TomlCodec (Set FixCategory)
categorySetCodec key = Toml.dimap (map categoryToText . Set.toList) (Set.fromList . map textToCategory) (Toml.arrayOf Toml._Text key)

-- | Codec for Set Text
textSetCodec :: Toml.Key -> TomlCodec (Set Text)
textSetCodec key = Toml.dimap Set.toList Set.fromList (Toml.arrayOf Toml._Text key)

-- | Convert SafetyLevel to Text for config serialization
safetyLevelToText :: SafetyLevel -> Text
safetyLevelToText = safetyToText

-- | Convert Text to SafetyLevel for config parsing
textToSafetyLevel :: Text -> SafetyLevel
textToSafetyLevel = textToSafety

-- | Convert FixCategory to Text
categoryToText :: FixCategory -> Text
categoryToText = \case
  FCPerformance -> "performance"
  FCModernize -> "modernize"
  FCSafety -> "safety"
  FCStyle -> "style"
  FCImports -> "imports"
  FCRedundant -> "redundant"
  FCSpaceLeaks -> "space_leaks"
  FCSecurity -> "security"
  FCCustom t -> "custom:" <> t

-- | Convert Text to FixCategory
textToCategory :: Text -> FixCategory
textToCategory t = case T.toLower t of
  "performance" -> FCPerformance
  "modernize" -> FCModernize
  "safety" -> FCSafety
  "style" -> FCStyle
  "imports" -> FCImports
  "redundant" -> FCRedundant
  "space_leaks" -> FCSpaceLeaks
  "spaceleaks" -> FCSpaceLeaks
  "security" -> FCSecurity
  other -> if "custom:" `T.isPrefixOf` other
           then FCCustom (T.drop 7 other)
           else FCStyle

-- | Codec for EngineConfigs
engineConfigsCodec :: TomlCodec EngineConfigs
engineConfigsCodec = EngineConfigs
  <$> Toml.table booleanEngineConfigCodec "boolean" Toml..= ecBoolean
  <*> Toml.table listEngineConfigCodec "list" Toml..= ecList
  <*> Toml.table monadEngineConfigCodec "monad" Toml..= ecMonad
  <*> Toml.table partialEngineConfigCodec "partial" Toml..= ecPartial
  <*> pure Map.empty  -- Custom configs loaded separately

-- | Codec for BooleanEngineConfig
booleanEngineConfigCodec :: TomlCodec BooleanEngineConfig
booleanEngineConfigCodec = BooleanEngineConfig
  <$> Toml.table engineConfigCodec "base" Toml..= becBase
  <*> Toml.bool "simplify_not" Toml..= becSimplifyNot
  <*> Toml.bool "simplify_and" Toml..= becSimplifyAnd
  <*> Toml.bool "simplify_or" Toml..= becSimplifyOr
  <*> Toml.bool "simplify_if" Toml..= becSimplifyIf
  <*> Toml.bool "simplify_comparison" Toml..= becSimplifyComparison
  <*> Toml.bool "preserve_comments" Toml..= becPreserveComments

-- | Codec for ListEngineConfig
listEngineConfigCodec :: TomlCodec ListEngineConfig
listEngineConfigCodec = ListEngineConfig
  <$> Toml.table engineConfigCodec "base" Toml..= lecBase
  <*> Toml.bool "optimize_concat" Toml..= lecOptimizeConcat
  <*> Toml.bool "use_list_comprehensions" Toml..= lecUseListComprehensions
  <*> Toml.bool "prefer_foldr" Toml..= lecPreferFoldr
  <*> Toml.bool "inline_single_use" Toml..= lecInlineSingleUse
  <*> Toml.int "max_inline_length" Toml..= lecMaxInlineLength

-- | Codec for MonadEngineConfig
monadEngineConfigCodec :: TomlCodec MonadEngineConfig
monadEngineConfigCodec = MonadEngineConfig
  <$> Toml.table engineConfigCodec "base" Toml..= mecBase
  <*> Toml.bool "simplify_bind" Toml..= mecSimplifyBind
  <*> Toml.bool "simplify_return" Toml..= mecSimplifyReturn
  <*> Toml.bool "use_fmap" Toml..= mecUseFmap
  <*> Toml.bool "use_applicative" Toml..= mecUseApplicative
  <*> Toml.bool "prefer_do" Toml..= mecPreferDo
  <*> Toml.int "min_chain_length" Toml..= mecMinChainLength

-- | Codec for PartialEngineConfig
partialEngineConfigCodec :: TomlCodec PartialEngineConfig
partialEngineConfigCodec = PartialEngineConfig
  <$> Toml.table engineConfigCodec "base" Toml..= pecBase
  <*> Toml.bool "replace_head" Toml..= pecReplaceHead
  <*> Toml.bool "replace_tail" Toml..= pecReplaceTail
  <*> Toml.bool "replace_init" Toml..= pecReplaceInit
  <*> Toml.bool "replace_last" Toml..= pecReplaceLast
  <*> Toml.bool "replace_fromJust" Toml..= pecReplaceFromJust
  <*> Toml.bool "replace_read" Toml..= pecReplaceRead
  <*> Toml.bool "prefer_pattern_match" Toml..= pecPreferPatternMatch
  <*> Toml.bool "suggest_non_empty" Toml..= pecSuggestNonEmpty

-- | Codec for EngineConfig
engineConfigCodec :: TomlCodec EngineConfig
engineConfigCodec = EngineConfig
  <$> Toml.bool "enabled" Toml..= ecEnabled
  <*> Toml.int "priority" Toml..= ecPriority
  <*> Toml.int "max_fixes" Toml..= ecMaxFixes
  <*> textSetCodec "tags" Toml..= ecTags

-- | Codec for PipelineConfig
pipelineConfigCodec :: TomlCodec PipelineConfig
pipelineConfigCodec = PipelineConfig
  <$> Toml.table filterConfigCodec "filters" Toml..= pcFilters
  <*> Toml.table sortConfigCodec "sort" Toml..= pcSort
  <*> Toml.bool "deduplicate" Toml..= pcDeduplicate
  <*> Toml.dioptional (Toml.int "limit") Toml..= pcLimit
  <*> Toml.bool "stop_on_empty" Toml..= pcStopOnEmpty
  <*> Toml.bool "validate_all" Toml..= pcValidateAll

-- | Codec for FilterConfig
filterConfigCodec :: TomlCodec FilterConfig
filterConfigCodec = FilterConfig
  <$> Toml.dioptional (Toml.int "min_confidence") Toml..= fcMinConfidence
  <*> Toml.dioptional (Toml.int "max_confidence") Toml..= fcMaxConfidence
  <*> Toml.dioptional (categorySetCodec "allowed_categories") Toml..= fcAllowedCategories
  <*> Toml.dioptional (categorySetCodec "blocked_categories") Toml..= fcBlockedCategories
  <*> Toml.dioptional (textSetCodec "allowed_engines") Toml..= fcAllowedEngines
  <*> Toml.dioptional (textSetCodec "blocked_engines") Toml..= fcBlockedEngines
  <*> Toml.dioptional (textSetCodec "required_tags") Toml..= fcRequiredTags
  <*> Toml.dioptional (textSetCodec "blocked_tags") Toml..= fcBlockedTags
  <*> Toml.dioptional (Toml.arrayOf Toml._Text "path_patterns") Toml..= fcPathPatterns
  <*> Toml.dioptional (Toml.arrayOf Toml._Text "exclude_patterns") Toml..= fcExcludePatterns

-- | Codec for SortConfig
sortConfigCodec :: TomlCodec SortConfig
sortConfigCodec = SortConfig
  <$> sortCriteriaCodec "criteria" Toml..= scCriteria
  <*> sortDirectionCodec "direction" Toml..= scDirection

-- | Codec for sort criteria list
sortCriteriaCodec :: Toml.Key -> TomlCodec [SortCriterion]
sortCriteriaCodec key = Toml.dimap (map criterionToText) (map textToCriterion) (Toml.arrayOf Toml._Text key)

-- | Codec for sort direction
sortDirectionCodec :: Toml.Key -> TomlCodec SortDirection
sortDirectionCodec key = Toml.dimap directionToText textToDirection (Toml.text key)

-- | Convert SortCriterion to Text
criterionToText :: SortCriterion -> Text
criterionToText = \case
  SortByConfidence -> "confidence"
  SortBySafety -> "safety"
  SortBySpan -> "span"
  SortByEngine -> "engine"
  SortByCategory -> "category"
  SortByFile -> "file"

-- | Convert Text to SortCriterion
textToCriterion :: Text -> SortCriterion
textToCriterion t = case T.toLower t of
  "confidence" -> SortByConfidence
  "safety" -> SortBySafety
  "span" -> SortBySpan
  "engine" -> SortByEngine
  "category" -> SortByCategory
  "file" -> SortByFile
  _ -> SortByConfidence

-- | Convert SortDirection to Text
directionToText :: SortDirection -> Text
directionToText = \case
  Ascending -> "ascending"
  Descending -> "descending"

-- | Convert Text to SortDirection
textToDirection :: Text -> SortDirection
textToDirection t = case T.toLower t of
  "ascending" -> Ascending
  "asc" -> Ascending
  "descending" -> Descending
  "desc" -> Descending
  _ -> Descending

-- | Codec for SafetyConfig
safetyConfigCodec :: TomlCodec SafetyConfig
safetyConfigCodec = SafetyConfig
  <$> Toml.bool "require_type_check" Toml..= scRequireTypeCheck
  <*> Toml.bool "require_parse_check" Toml..= scRequireParseCheck
  <*> Toml.int "max_unsafe_per_file" Toml..= scMaxUnsafePerFile
  <*> Toml.bool "require_confirm_unsafe" Toml..= scRequireConfirmUnsafe
  <*> Toml.bool "block_destructive" Toml..= scBlockDestructive
  <*> Toml.bool "preserve_comments" Toml..= scPreserveComments
  <*> Toml.bool "preserve_formatting" Toml..= scPreserveFormatting

-- | Codec for OutputConfig
outputConfigCodec :: TomlCodec OutputConfig
outputConfigCodec = OutputConfig
  <$> Toml.bool "show_diff" Toml..= ocShowDiff
  <*> diffFormatCodec "diff_format" Toml..= ocDiffFormat
  <*> Toml.bool "show_explanation" Toml..= ocShowExplanation
  <*> Toml.bool "show_confidence" Toml..= ocShowConfidence
  <*> Toml.bool "group_by_file" Toml..= ocGroupByFile
  <*> Toml.bool "color_output" Toml..= ocColorOutput
  <*> Toml.int "verbosity" Toml..= ocVerbosity
  <*> Toml.dioptional (Toml.string "log_file") Toml..= ocLogFile
  <*> Toml.dioptional (Toml.string "report_file") Toml..= ocReportFile

-- | Codec for DiffFormat
diffFormatCodec :: Toml.Key -> TomlCodec DiffFormat
diffFormatCodec key = Toml.dimap diffFormatToText textToDiffFormat (Toml.text key)

-- | Convert DiffFormat to Text
diffFormatToText :: DiffFormat -> Text
diffFormatToText = \case
  UnifiedDiff -> "unified"
  SideBySide -> "side_by_side"
  InlineHighlight -> "inline"
  MinimalDiff -> "minimal"

-- | Convert Text to DiffFormat
textToDiffFormat :: Text -> DiffFormat
textToDiffFormat t = case T.toLower t of
  "unified" -> UnifiedDiff
  "side_by_side" -> SideBySide
  "inline" -> InlineHighlight
  "minimal" -> MinimalDiff
  _ -> UnifiedDiff

-- | Codec for rule overrides (simplified - uses JSON internally)
ruleOverridesCodec :: Toml.Key -> TomlCodec (Map Text RuleFixConfig)
ruleOverridesCodec _key = pure Map.empty  -- Rule overrides loaded via JSON

-- | Merge two configurations, with the second taking precedence
mergeConfigs :: AutoFixConfig -> AutoFixConfig -> AutoFixConfig
mergeConfigs base override = AutoFixConfig
  { afcEnabled = afcEnabled override
  , afcDryRun = afcDryRun override
  , afcInteractive = afcInteractive override
  , afcBackupOriginal = afcBackupOriginal override
  , afcMaxFixesPerFile = afcMaxFixesPerFile override
  , afcMaxFixesTotal = afcMaxFixesTotal override
  , afcMinConfidence = afcMinConfidence override
  , afcAllowedSafety =
      if Set.null (afcAllowedSafety override)
        then afcAllowedSafety base
        else afcAllowedSafety override
  , afcEnabledCategories =
      if Set.null (afcEnabledCategories override)
        then afcEnabledCategories base
        else afcEnabledCategories override
  , afcDisabledRules = Set.union (afcDisabledRules base) (afcDisabledRules override)
  , afcEngines = afcEngines override  -- Full override for engines
  , afcPipeline = afcPipeline override
  , afcSafety = afcSafety override
  , afcOutput = afcOutput override
  , afcRuleOverrides = Map.union (afcRuleOverrides override) (afcRuleOverrides base)
  }

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

-- | Configuration validation error
data ConfigValidationError
  = InvalidConfidenceRange Double
  | InvalidMaxFixes Int
  | InvalidVerbosity Int
  | EmptyAllowedSafety
  | EmptyEnabledCategories
  | ConflictingFilters Text
  | InvalidPathPattern Text
  | DuplicateEngineConfig Text
  | InvalidEnginePriority Text Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Validate configuration
validateConfig :: AutoFixConfig -> Either [ConfigValidationError] ()
validateConfig config = do
  let errors = concat
        [ validateConfidence config
        , validateMaxFixes config
        , validateVerbosity config
        , validateSafetyLevels config
        , validateCategories config
        , validateFilters config
        ]
  if null errors
    then Right ()
    else Left errors

-- | Validate confidence settings
validateConfidence :: AutoFixConfig -> [ConfigValidationError]
validateConfidence config =
  let conf = unConfidenceValue (afcMinConfidence config)
  in [ InvalidConfidenceRange conf | conf < 0 || conf > 100 ]

-- | Validate max fixes settings
validateMaxFixes :: AutoFixConfig -> [ConfigValidationError]
validateMaxFixes config = concat
  [ [ InvalidMaxFixes (afcMaxFixesPerFile config) | afcMaxFixesPerFile config < 0 ]
  , [ InvalidMaxFixes (afcMaxFixesTotal config) | afcMaxFixesTotal config < 0 ]
  ]

-- | Validate verbosity
validateVerbosity :: AutoFixConfig -> [ConfigValidationError]
validateVerbosity config =
  let v = ocVerbosity (afcOutput config)
  in [ InvalidVerbosity v | v < 0 || v > 3 ]

-- | Validate safety levels
validateSafetyLevels :: AutoFixConfig -> [ConfigValidationError]
validateSafetyLevels config =
  [ EmptyAllowedSafety | Set.null (afcAllowedSafety config) ]

-- | Validate categories
validateCategories :: AutoFixConfig -> [ConfigValidationError]
validateCategories config =
  [ EmptyEnabledCategories | Set.null (afcEnabledCategories config) ]

-- | Validate filter settings
validateFilters :: AutoFixConfig -> [ConfigValidationError]
validateFilters config =
  let filters = pcFilters (afcPipeline config)
      hasConflict = case (fcAllowedCategories filters, fcBlockedCategories filters) of
        (Just allowed, Just blocked) -> not (Set.null (Set.intersection allowed blocked))
        _ -> False
  in [ ConflictingFilters "categories" | hasConflict ]

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

-- | Encode configuration to TOML text
encodeAutoFixConfig :: AutoFixConfig -> Text
encodeAutoFixConfig = Toml.encode autoFixConfigCodec
