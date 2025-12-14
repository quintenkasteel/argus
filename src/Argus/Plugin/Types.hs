{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Argus.Plugin.Types
-- Description : Core types for the Argus plugin system
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module defines the core types for the Argus plugin system,
-- enabling users to extend Argus with custom rules, fixes, and analyzers.
--
-- == Plugin Architecture
--
-- The plugin system provides:
--
-- * Custom lint rules with pattern matching
-- * Custom auto-fix providers
-- * Custom output formatters
-- * Pre/post analysis hooks
-- * Configuration providers
--
-- == Usage
--
-- @
-- myPlugin :: ArgusPlugin
-- myPlugin = ArgusPlugin
--   { pluginId = "my-company-rules"
--   , pluginVersion = Version [1, 0, 0] []
--   , pluginRules = [myCustomRule]
--   , pluginFixes = [myCustomFix]
--   , ...
--   }
-- @
module Argus.Plugin.Types
  ( -- * Plugin Definition
    ArgusPlugin (..)
  , PluginMetadata (..)
  , PluginCapability (..)
  , PluginPriority (..)
  , PluginStatus (..)

    -- * Plugin Rules
  , PluginRule (..)
  , RuleFunction
  , RuleContext (..)
  , RuleMatch (..)
  , RuleMatchKind (..)

    -- * Plugin Fixes
  , PluginFixer (..)
  , FixerFunction
  , FixerContext (..)
  , FixerResult (..)
  , FixerMatch (..)

    -- * Plugin Hooks
  , PluginHook (..)
  , HookPoint (..)
  , HookFunction
  , HookContext (..)
  , HookResult (..)

    -- * Plugin Formatters
  , PluginFormatter (..)
  , FormatFunction
  , FormatContext (..)

    -- * Plugin Configuration
  , PluginConfigProvider (..)
  , ConfigSchema (..)
  , ConfigField (..)
  , ConfigFieldType (..)
  , ConfigValue (..)

    -- * Error Types
  , PluginError (..)
  , PluginLoadError (..)
  , PluginRuntimeError (..)

    -- * Helper Types
  , Version (..)
  , showVersion
  , parseVersion
  ) where

import Data.Aeson (ToJSON, FromJSON, Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Argus.Types
  ( SrcSpan(..)
  , Diagnostic(..)
  , Fix(..)
  , Severity(..)
  )

--------------------------------------------------------------------------------
-- Version
--------------------------------------------------------------------------------

-- | Semantic version
data Version = Version
  { versionMajor :: Int
  , versionMinor :: Int
  , versionPatch :: Int
  , versionPreRelease :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Display a version
showVersion :: Version -> Text
showVersion Version{..} =
  T.pack (show versionMajor) <> "." <>
  T.pack (show versionMinor) <> "." <>
  T.pack (show versionPatch) <>
  maybe "" ("-" <>) versionPreRelease

-- | Parse a version string
parseVersion :: Text -> Maybe Version
parseVersion txt = case T.splitOn "." txt of
  [maj, min', patch'] -> do
    let (patchNum, preRel) = T.breakOn "-" patch'
    major <- readMaybe' maj
    minor <- readMaybe' min'
    patchV <- readMaybe' patchNum
    pure Version
      { versionMajor = major
      , versionMinor = minor
      , versionPatch = patchV
      , versionPreRelease = if T.null preRel then Nothing else Just (T.drop 1 preRel)
      }
  _ -> Nothing
  where
    readMaybe' t = case reads (T.unpack t) of
      [(n, "")] -> Just n
      _ -> Nothing

--------------------------------------------------------------------------------
-- Plugin Definition
--------------------------------------------------------------------------------

-- | Core plugin type with all extensions
data ArgusPlugin = ArgusPlugin
  { pluginMetadata    :: PluginMetadata
      -- ^ Plugin identification and metadata
  , pluginRules       :: [PluginRule]
      -- ^ Custom lint rules provided by this plugin
  , pluginFixers      :: [PluginFixer]
      -- ^ Custom auto-fixers provided by this plugin
  , pluginHooks       :: [PluginHook]
      -- ^ Lifecycle hooks
  , pluginFormatters  :: [PluginFormatter]
      -- ^ Custom output formatters
  , pluginConfig      :: Maybe PluginConfigProvider
      -- ^ Configuration schema and defaults
  , pluginDependencies :: [Text]
      -- ^ Other plugins this one depends on
  , pluginInit        :: IO (Either PluginError ())
      -- ^ Initialization function
  , pluginShutdown    :: IO ()
      -- ^ Cleanup function
  }

-- | Plugin metadata
data PluginMetadata = PluginMetadata
  { pmId          :: Text
      -- ^ Unique plugin identifier
  , pmName        :: Text
      -- ^ Human-readable name
  , pmVersion     :: Version
      -- ^ Plugin version
  , pmAuthor      :: Text
      -- ^ Plugin author
  , pmDescription :: Text
      -- ^ Plugin description
  , pmHomepage    :: Maybe Text
      -- ^ Plugin homepage URL
  , pmLicense     :: Maybe Text
      -- ^ Plugin license
  , pmCapabilities :: [PluginCapability]
      -- ^ What this plugin provides
  , pmMinArgusVersion :: Maybe Version
      -- ^ Minimum Argus version required
  , pmMaxArgusVersion :: Maybe Version
      -- ^ Maximum Argus version supported
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Plugin capabilities
data PluginCapability
  = CapabilityRules         -- ^ Provides custom lint rules
  | CapabilityFixes         -- ^ Provides auto-fixes
  | CapabilityHooks         -- ^ Provides lifecycle hooks
  | CapabilityFormatters    -- ^ Provides output formatters
  | CapabilityConfig        -- ^ Provides configuration
  | CapabilityAnalysis      -- ^ Provides custom analysis
  | CapabilityReporting     -- ^ Provides custom reporting
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Plugin priority for ordering
data PluginPriority
  = PriorityFirst       -- ^ Run before built-in rules
  | PriorityEarly       -- ^ Run early
  | PriorityNormal      -- ^ Normal priority (default)
  | PriorityLate        -- ^ Run late
  | PriorityLast        -- ^ Run after all other rules
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Plugin status
data PluginStatus
  = PluginEnabled       -- ^ Plugin is active
  | PluginDisabled      -- ^ Plugin is disabled by user
  | PluginErrored Text  -- ^ Plugin failed to load/init
  | PluginDepMissing Text  -- ^ Missing dependency
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Plugin Rules
--------------------------------------------------------------------------------

-- | A custom lint rule
data PluginRule = PluginRule
  { prId          :: Text
      -- ^ Unique rule ID (scoped to plugin)
  , prName        :: Text
      -- ^ Human-readable name
  , prDescription :: Text
      -- ^ Rule description
  , prSeverity    :: Severity
      -- ^ Default severity
  , prCategory    :: Text
      -- ^ Rule category
  , prTags        :: [Text]
      -- ^ Tags for filtering
  , prEnabled     :: Bool
      -- ^ Enabled by default?
  , prPriority    :: PluginPriority
      -- ^ Execution priority
  , prFunction    :: RuleFunction
      -- ^ The actual rule checker
  }

-- | Rule execution function
type RuleFunction = RuleContext -> IO [RuleMatch]

-- | Context passed to rule functions
data RuleContext = RuleContext
  { rcFilePath    :: FilePath
      -- ^ File being analyzed
  , rcFileContent :: Text
      -- ^ File content
  , rcModuleName  :: Maybe Text
      -- ^ Module name if available
  , rcConfig      :: Map Text ConfigValue
      -- ^ Plugin configuration
  , rcPreviousMatches :: [Diagnostic]
      -- ^ Matches from earlier rules
  }
  deriving stock (Show, Generic)

-- | A rule match result
data RuleMatch = RuleMatch
  { rmSpan        :: SrcSpan
      -- ^ Location of the match
  , rmKind        :: RuleMatchKind
      -- ^ Type of match
  , rmMessage     :: Text
      -- ^ Diagnostic message
  , rmSuggestion  :: Maybe Text
      -- ^ Suggested fix text
  , rmContext     :: Map Text Value
      -- ^ Additional context for fixes
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Kind of rule match
data RuleMatchKind
  = MatchError         -- ^ Definite error
  | MatchWarning       -- ^ Warning
  | MatchSuggestion    -- ^ Improvement suggestion
  | MatchInfo          -- ^ Informational
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Plugin Fixes
--------------------------------------------------------------------------------

-- | A custom auto-fixer
data PluginFixer = PluginFixer
  { pfId          :: Text
      -- ^ Unique fixer ID
  , pfName        :: Text
      -- ^ Human-readable name
  , pfDescription :: Text
      -- ^ Fixer description
  , pfTargetRules :: [Text]
      -- ^ Rule IDs this fixer handles
  , pfPriority    :: PluginPriority
      -- ^ Execution priority
  , pfFunction    :: FixerFunction
      -- ^ The actual fixer
  }

-- | Fixer execution function
type FixerFunction = FixerContext -> IO FixerResult

-- | Context passed to fixer functions
data FixerContext = FixerContext
  { fcFilePath    :: FilePath
      -- ^ File being fixed
  , fcFileContent :: Text
      -- ^ Current file content
  , fcDiagnostic  :: Diagnostic
      -- ^ The diagnostic to fix
  , fcConfig      :: Map Text ConfigValue
      -- ^ Plugin configuration
  , fcMatchContext :: Map Text Value
      -- ^ Context from rule match
  }
  deriving stock (Show, Generic)

-- | Result of a fixer
data FixerResult
  = FixerSuccess [FixerMatch]
      -- ^ Successfully generated fix(es)
  | FixerSkipped Text
      -- ^ Skipped with reason
  | FixerFailed Text
      -- ^ Failed with error
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A single fix from a fixer
data FixerMatch = FixerMatch
  { fmFix         :: Fix
      -- ^ The fix to apply
  , fmConfidence  :: Double
      -- ^ Confidence score (0.0 - 1.0)
  , fmImports     :: [Text]
      -- ^ Imports to add
  , fmNotes       :: [Text]
      -- ^ Notes about this fix
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Plugin Hooks
--------------------------------------------------------------------------------

-- | A lifecycle hook
data PluginHook = PluginHook
  { phId          :: Text
      -- ^ Unique hook ID
  , phPoint       :: HookPoint
      -- ^ When this hook runs
  , phPriority    :: PluginPriority
      -- ^ Execution priority at this point
  , phFunction    :: HookFunction
      -- ^ The hook function
  }

-- | Hook execution points
data HookPoint
  = HookBeforeAnalysis   -- ^ Before any analysis
  | HookAfterParsing     -- ^ After file parsing
  | HookBeforeRules      -- ^ Before rule execution
  | HookAfterRules       -- ^ After rule execution
  | HookBeforeFixes      -- ^ Before fixes applied
  | HookAfterFixes       -- ^ After fixes applied
  | HookBeforeOutput     -- ^ Before output formatting
  | HookAfterAnalysis    -- ^ After all analysis
  | HookOnError          -- ^ On any error
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Hook execution function
type HookFunction = HookContext -> IO HookResult

-- | Context passed to hooks
data HookContext = HookContext
  { hcPoint       :: HookPoint
      -- ^ Current hook point
  , hcFilePaths   :: [FilePath]
      -- ^ Files being analyzed
  , hcDiagnostics :: [Diagnostic]
      -- ^ Current diagnostics (may be empty)
  , hcConfig      :: Map Text ConfigValue
      -- ^ Plugin configuration
  , hcMetadata    :: Map Text Value
      -- ^ Additional metadata
  }
  deriving stock (Show, Generic)

-- | Result of a hook
data HookResult
  = HookContinue
      -- ^ Continue normally
  | HookModifyDiagnostics [Diagnostic]
      -- ^ Replace diagnostics with these
  | HookAddDiagnostics [Diagnostic]
      -- ^ Add additional diagnostics
  | HookAbort Text
      -- ^ Abort analysis with message
  | HookSkipFile FilePath
      -- ^ Skip this file
  deriving stock (Show, Generic)

--------------------------------------------------------------------------------
-- Plugin Formatters
--------------------------------------------------------------------------------

-- | A custom output formatter
data PluginFormatter = PluginFormatter
  { pformId       :: Text
      -- ^ Unique formatter ID (used in --format option)
  , pformName     :: Text
      -- ^ Human-readable name
  , pformDescription :: Text
      -- ^ Formatter description
  , pformFileExt  :: Text
      -- ^ Default file extension for output
  , pformFunction :: FormatFunction
      -- ^ The formatting function
  }

-- | Formatter execution function
type FormatFunction = FormatContext -> IO Text

-- | Context passed to formatters
data FormatContext = FormatContext
  { fcDiags       :: [Diagnostic]
      -- ^ Diagnostics to format
  , fcFiles       :: [FilePath]
      -- ^ Files that were analyzed
  , fcStats       :: Map Text Value
      -- ^ Statistics
  , fcPConfig     :: Map Text ConfigValue
      -- ^ Plugin configuration
  }
  deriving stock (Show, Generic)

--------------------------------------------------------------------------------
-- Plugin Configuration
--------------------------------------------------------------------------------

-- | Configuration provider for a plugin
data PluginConfigProvider = PluginConfigProvider
  { pcpSchema     :: ConfigSchema
      -- ^ Configuration schema
  , pcpDefaults   :: Map Text ConfigValue
      -- ^ Default values
  , pcpValidator  :: Map Text ConfigValue -> Either Text ()
      -- ^ Custom validator
  }

-- | Configuration schema
data ConfigSchema = ConfigSchema
  { csFields      :: [ConfigField]
      -- ^ Configuration fields
  , csDescription :: Text
      -- ^ Schema description
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A configuration field
data ConfigField = ConfigField
  { cfName        :: Text
      -- ^ Field name
  , cfType        :: ConfigFieldType
      -- ^ Field type
  , cfDescription :: Text
      -- ^ Field description
  , cfRequired    :: Bool
      -- ^ Is this field required?
  , cfDefault     :: Maybe ConfigValue
      -- ^ Default value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Configuration field types
data ConfigFieldType
  = ConfigBool
  | ConfigInt
  | ConfigDouble
  | ConfigString
  | ConfigStringList
  | ConfigEnum [Text]
  | ConfigObject [ConfigField]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Configuration value
data ConfigValue
  = ConfigValBool Bool
  | ConfigValInt Int
  | ConfigValDouble Double
  | ConfigValString Text
  | ConfigValList [ConfigValue]
  | ConfigValObject (Map Text ConfigValue)
  | ConfigValNull
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

-- | Plugin-related errors
data PluginError
  = PluginLoadErr PluginLoadError
  | PluginRuntimeErr PluginRuntimeError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Errors during plugin loading
data PluginLoadError
  = PluginNotFound Text
  | PluginInvalidFormat Text
  | PluginVersionMismatch Version Version
  | PluginDependencyError Text Text
  | PluginConfigError Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Errors during plugin execution
data PluginRuntimeError
  = RuleExecutionError Text Text
  | FixerExecutionError Text Text
  | HookExecutionError Text Text
  | FormatterExecutionError Text Text
  | ConfigValidationError Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
