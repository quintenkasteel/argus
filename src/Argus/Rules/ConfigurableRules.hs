{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Argus.Rules.ConfigurableRules
-- Description : Dynamic configurable rule system for Argus
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides a fully configurable rule system inspired by HLint,
-- ESLint, Semgrep, and other best-in-class linting tools. Rules can be:
--
-- * Loaded from TOML configuration files
-- * Layered (default + project + user)
-- * Categorized with enable/disable by category
-- * Safety-classified for auto-fix control
-- * Scoped to specific modules/directories
--
-- Example configuration:
--
-- @
-- [rules]
-- performance = "warn"
-- security = "error"
--
-- [[rules.custom]]
-- id = "custom/no-head"
-- pattern = "head $X"
-- fix = "headMay $X"
-- message = "Use headMay instead of partial head"
-- severity = "warning"
-- safe = true
-- @
module Argus.Rules.ConfigurableRules
  ( -- * Rule Configuration
    RulesConfig (..)
  , defaultRulesConfig

    -- * Configurable Rules
  , ConfigurableRule (..)
  , RuleCategory
  , allCategories
  -- RuleCategory pattern synonyms (re-exported from Rules.Types.Category)
  , pattern Performance
  , pattern SpaceLeaks
  , pattern Security
  , pattern Modernize  -- Alias for Modernization
  , pattern Redundant
  , pattern Partial    -- Alias for Safety
  , pattern Imports
  , pattern Naming
  , pattern Extensions
  , pattern Complexity
  , pattern Concurrency
  , pattern ErrorHandling
  , pattern Documentation
  , pattern CustomCategory
  -- Safety types
  , FixSafety
  , pattern Safe
  , pattern Unsafe
  , pattern Manual  -- Alias for NeedsReview

    -- * Pattern Rules
  , ConfigPattern (..)
  , Metavariable (..)

    -- * Restrictions
  , RestrictionsConfig (..)
  , FunctionRestriction (..)
  , ModuleRestriction (..)
  , ExtensionRestriction (..)
  , defaultRestrictions
  , defaultModuleRestrictions
  , lookupRecommendedAlias
  , transformToQualifiedImport

    -- * Rule Override
  , RuleOverride (..)
  , CategorySeverity (..)

    -- * Scope Configuration
  , ScopeConfig (..)

    -- * Rule Loading
  , loadRulesConfig
  , mergeRulesConfig

    -- * Rule Application
  , applyConfigurableRules
  , applyASTConfigurableRules
  , checkRestrictions
  , isRuleEnabled
  , getRuleEffectiveSeverity
  , canAutoFix
  , matchPattern
  , matchesWithin

    -- * AST-Based Rules
  , convertToASTRules
  , loadAndApplyASTRules

    -- * Default Rules Library
  , defaultRulesLibrary
  , performanceRules
  , securityRules
  , modernizeRules
  , redundantRules
  , spaceLeakRules
  , partialRules

    -- * Rule Construction
  , mkRule
  , mkRuleWithImports
  , mkRuleWithMetadata
  , inferCategory

    -- * Import Conversion (TODO-004)
  , convertImportSpecToFixImport
  , convertImportSymbol
  , convertSymbolType

    -- * Rule Templates (TODO-009)
  , RuleTemplate (..)
  , TemplatedRule (..)
  , defaultTemplate
  , expandTemplatedRule
  , substituteVars
  , expandTemplates
  , lookupTemplate

    -- * Fix Dependencies (TODO-010)
  , getRuleDependencies
  , getRuleConflicts
  , buildRuleDependencyMap
  , RuleDependencyInfo (..)

    -- * Rule Versioning (TODO-011)
  , DeprecationInfo (..)
  , DeprecationWarning (..)
  , checkDeprecations
  , SchemaVersion
  , currentSchemaVersion

    -- * Module Context (TODO-012)
  , mkRuleWithModuleContext
  , hasModuleContext
  , getModuleContext
  , setModuleContext

    -- * Full Rule Builder (TODO-012)
  , FullRuleBuilder (..)
  , defaultFullRuleBuilder
  , buildFullRule
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Aeson (ToJSON (..), FromJSON (..), (.:), (.:?), (.!=), withObject, withText, object)
import qualified Data.Aeson as AE ((.=))
import Data.List (find, isInfixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, catMaybes, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import System.IO (Handle, stderr)
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import Text.Regex.TDFA ((=~))
import Toml (TomlCodec, (.=))
import Toml qualified as TOML

import Argus.Types hiding (FixSafety(..))
import Argus.Types qualified as AT (FixSafety(..))

import Argus.Rules.SideConditions.Parser qualified as SCP
import Paths_argus (getDataFileName)

import Argus.Rules.ASTMatch qualified as AST
import Argus.Rules.Types qualified as RT
  ( Rule(..), Category(..), SafetyLevel(..), SideCondition(..), RulePattern(..)
  , ImportSpec(..), RuleTarget(..), ImportSymbol(..), SymbolType(..)
  , allCategories
  )
import Argus.Plugin.Types (Version(..), showVersion, parseVersion)

--------------------------------------------------------------------------------
-- Type Aliases and Pattern Synonyms for Backwards Compatibility
--------------------------------------------------------------------------------

-- | Type alias: RuleCategory is now RT.Category
type RuleCategory = RT.Category

-- | Type alias: FixSafety is now RT.SafetyLevel
type FixSafety = RT.SafetyLevel

-- Pattern synonyms for RuleCategory (backwards compatibility)
pattern Performance :: RuleCategory
pattern Performance = RT.Performance

pattern SpaceLeaks :: RuleCategory
pattern SpaceLeaks = RT.SpaceLeaks

pattern Security :: RuleCategory
pattern Security = RT.Security

-- Modernize is an alias for RT.Modernization
pattern Modernize :: RuleCategory
pattern Modernize = RT.Modernization

pattern Redundant :: RuleCategory
pattern Redundant = RT.Redundant

-- Partial is an alias for RT.Safety
pattern Partial :: RuleCategory
pattern Partial = RT.Safety

pattern Imports :: RuleCategory
pattern Imports = RT.Imports

pattern Naming :: RuleCategory
pattern Naming = RT.Naming

pattern Extensions :: RuleCategory
pattern Extensions = RT.Extensions

pattern Complexity :: RuleCategory
pattern Complexity = RT.Complexity

pattern Concurrency :: RuleCategory
pattern Concurrency = RT.Concurrency

pattern ErrorHandling :: RuleCategory
pattern ErrorHandling = RT.ErrorHandling

pattern Documentation :: RuleCategory
pattern Documentation = RT.Documentation

pattern CustomCategory :: Text -> RuleCategory
pattern CustomCategory t = RT.Custom t

-- Pattern synonyms for FixSafety (backwards compatibility)
pattern Safe :: FixSafety
pattern Safe = RT.Safe

pattern Unsafe :: FixSafety
pattern Unsafe = RT.Unsafe

-- Manual is an alias for RT.NeedsReview
pattern Manual :: FixSafety
pattern Manual = RT.NeedsReview

-- | Re-export allCategories from RT (but it returns RT.Category now)
allCategories :: [RuleCategory]
allCategories = RT.allCategories

--------------------------------------------------------------------------------
-- Category Severity
--------------------------------------------------------------------------------

-- | Severity for a category (can also be disabled)
data CategorySeverity
  = CatOff
  | CatError
  | CatWarn
  | CatSuggestion
  | CatInfo
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON CategorySeverity where
  toJSON = \case
    CatOff -> "off"
    CatError -> "error"
    CatWarn -> "warn"
    CatSuggestion -> "suggestion"
    CatInfo -> "info"

instance FromJSON CategorySeverity where
  parseJSON = withText "CategorySeverity" $ \t -> pure $ case T.toLower t of
    "off" -> CatOff
    "error" -> CatError
    "warn" -> CatWarn
    "warning" -> CatWarn
    "suggestion" -> CatSuggestion
    "info" -> CatInfo
    _ -> CatWarn

catToSeverity :: CategorySeverity -> Maybe Severity
catToSeverity = \case
  CatOff -> Nothing
  CatError -> Just Error
  CatWarn -> Just Warning
  CatSuggestion -> Just Suggestion
  CatInfo -> Just Info

--------------------------------------------------------------------------------
-- Pattern Matching
--------------------------------------------------------------------------------

-- | A metavariable that captures part of the matched code
data Metavariable = Metavariable
  { mvName       :: Text       -- ^ Variable name (e.g., "$X", "$FUNC")
  , mvType       :: Maybe Text -- ^ Optional type constraint
  , mvCapture    :: Text       -- ^ Captured value
  , mvSpan       :: SrcSpan    -- ^ Source location
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A pattern for matching code
data ConfigPattern = ConfigPattern
  { cpPattern    :: Text           -- ^ The pattern (e.g., "head $X")
  , cpFix        :: Maybe Text     -- ^ Replacement pattern (e.g., "headMay $X")
  , cpWhere      :: Maybe Text     -- ^ Type constraint
  , cpMessage    :: Text           -- ^ User message
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Rule Templates (TODO-009)
--------------------------------------------------------------------------------

-- | A rule template for composition/inheritance
data RuleTemplate = RuleTemplate
  { rtId              :: Text            -- ^ Template ID
  , rtCategory        :: Maybe RuleCategory  -- ^ Optional default category
  , rtSeverity        :: Maybe Severity  -- ^ Optional default severity
  , rtSafety          :: Maybe FixSafety -- ^ Optional default safety
  , rtPatternTemplate :: Maybe Text      -- ^ Pattern with $VAR placeholders
  , rtFixTemplate     :: Maybe Text      -- ^ Fix with $VAR placeholders
  , rtMessageTemplate :: Maybe Text      -- ^ Message with $VAR placeholders
  , rtAddImports      :: [RT.ImportSpec] -- ^ Default imports to add
  , rtRemoveImports   :: [Text]          -- ^ Default imports to remove
  , rtTags            :: [Text]          -- ^ Default tags
  , rtWithin          :: [Text]          -- ^ Default module restrictions
  , rtExcept          :: [Text]          -- ^ Default module exclusions
  , rtNote            :: Maybe Text      -- ^ Default note
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Create a default template
defaultTemplate :: Text -> RuleTemplate
defaultTemplate templateId = RuleTemplate
  { rtId = templateId
  , rtCategory = Nothing
  , rtSeverity = Nothing
  , rtSafety = Nothing
  , rtPatternTemplate = Nothing
  , rtFixTemplate = Nothing
  , rtMessageTemplate = Nothing
  , rtAddImports = []
  , rtRemoveImports = []
  , rtTags = []
  , rtWithin = []
  , rtExcept = []
  , rtNote = Nothing
  }

-- | A rule that extends a template
data TemplatedRule = TemplatedRule
  { trExtends     :: Text                -- ^ Template ID to extend
  , trId          :: Text                -- ^ Rule ID
  , trVars        :: Map Text Text       -- ^ Variable substitutions
  , trPattern     :: Maybe Text          -- ^ Override pattern
  , trFix         :: Maybe Text          -- ^ Override fix
  , trMessage     :: Maybe Text          -- ^ Override message
  , trCategory    :: Maybe RuleCategory  -- ^ Override category
  , trSeverity    :: Maybe Severity      -- ^ Override severity
  , trSafety      :: Maybe FixSafety     -- ^ Override safety
  , trEnabled     :: Maybe Bool          -- ^ Override enabled
  , trAddImports  :: [RT.ImportSpec]     -- ^ Additional imports to add
  , trRemoveImports :: [Text]            -- ^ Additional imports to remove
  , trTags        :: [Text]              -- ^ Additional tags
  , trWithin      :: [Text]              -- ^ Override module restrictions
  , trExcept      :: [Text]              -- ^ Override module exclusions
  , trNote        :: Maybe Text          -- ^ Override note
  , trExplanation :: Maybe Text          -- ^ Override explanation
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Expand a templated rule using a template
expandTemplatedRule :: RuleTemplate -> TemplatedRule -> ConfigurableRule
expandTemplatedRule tpl tr = ConfigurableRule
  { crId = trId tr
  , crCategory = fromMaybe (fromMaybe Performance (rtCategory tpl)) (trCategory tr)
  , crPattern = ConfigPattern
      { cpPattern = substituteVars (trVars tr) $
          fromMaybe (fromMaybe "" (rtPatternTemplate tpl)) (trPattern tr)
      , cpFix = fmap (substituteVars (trVars tr)) $
          trFix tr <|> rtFixTemplate tpl
      , cpWhere = Nothing
      , cpMessage = substituteVars (trVars tr) $
          fromMaybe (fromMaybe "" (rtMessageTemplate tpl)) (trMessage tr)
      }
  , crSeverity = fromMaybe (fromMaybe Warning (rtSeverity tpl)) (trSeverity tr)
  , crSafety = fromMaybe (fromMaybe Safe (rtSafety tpl)) (trSafety tr)
  , crEnabled = fromMaybe True (trEnabled tr)
  , crWithin = if null (trWithin tr) then rtWithin tpl else trWithin tr
  , crExcept = if null (trExcept tr) then rtExcept tpl else trExcept tr
  , crNote = trNote tr <|> rtNote tpl
  , crAddImports = trAddImports tr ++ rtAddImports tpl
  , crRemoveImports = trRemoveImports tr ++ rtRemoveImports tpl
  , crTarget = Nothing
  , crTags = trTags tr ++ rtTags tpl
  , crReferences = []
  , crExplanation = trExplanation tr
  , crDependsOn = []
  , crConflictsWith = []
  -- Rule versioning (TODO-011)
  , crVersion = Nothing
  , crDeprecated = Nothing
  -- Module context (TODO-012)
  , crFromModule = Nothing
  , crToModule = Nothing
  }

-- | Substitute $VAR placeholders with values
substituteVars :: Map Text Text -> Text -> Text
substituteVars vars txt = Map.foldrWithKey replace' txt vars
  where
    replace' varName value = T.replace ("$" <> varName) value

--------------------------------------------------------------------------------
-- Rule Versioning (TODO-011)
--------------------------------------------------------------------------------

-- | Schema version for rules config files
type SchemaVersion = Text

-- | Current schema version
currentSchemaVersion :: SchemaVersion
currentSchemaVersion = "1.0"

-- | Deprecation information for a rule
--
-- When a rule is deprecated, this contains information about why it was
-- deprecated and what to use instead.
data DeprecationInfo = DeprecationInfo
  { diReason         :: Text           -- ^ Why the rule is deprecated
  , diReplacement    :: Maybe Text     -- ^ ID of the replacement rule
  , diRemovalVersion :: Maybe Version  -- ^ Version when rule will be removed
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Warning about a deprecated rule being used
data DeprecationWarning = DeprecationWarning
  { dwRuleId      :: Text            -- ^ ID of the deprecated rule
  , dwInfo        :: DeprecationInfo -- ^ Deprecation details
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Configurable Rules
--------------------------------------------------------------------------------

-- | A configurable rule definition
data ConfigurableRule = ConfigurableRule
  { crId          :: Text            -- ^ Unique rule ID (e.g., "performance/length-null")
  , crCategory    :: RuleCategory    -- ^ Rule category
  , crPattern     :: ConfigPattern   -- ^ Pattern to match
  , crSeverity    :: Severity        -- ^ Default severity
  , crSafety      :: FixSafety       -- ^ Fix safety level
  , crEnabled     :: Bool            -- ^ Is rule enabled?
  , crWithin      :: [Text]          -- ^ Module patterns where rule applies
  , crExcept      :: [Text]          -- ^ Module patterns to exclude
  , crNote        :: Maybe Text      -- ^ Additional explanation
  -- Import management (TODO-004)
  , crAddImports    :: [RT.ImportSpec]  -- ^ Imports to add when fix is applied
  , crRemoveImports :: [Text]           -- ^ Import symbols to remove
  -- Target specification (TODO-005)
  , crTarget        :: Maybe RT.RuleTarget  -- ^ Where to match (code, comments, etc.)
  -- Tags and references (TODO-007)
  , crTags          :: [Text]           -- ^ Tags for filtering
  , crReferences    :: [Text]           -- ^ Reference URLs
  -- Rule explanation (TODO-006)
  , crExplanation   :: Maybe Text       -- ^ Detailed explanation
  -- Fix ordering (TODO-010)
  , crDependsOn     :: [Text]           -- ^ Rules that must be applied first
  , crConflictsWith :: [Text]           -- ^ Rules that conflict (mutually exclusive)
  -- Rule versioning (TODO-011)
  , crVersion       :: Maybe Version    -- ^ Rule version
  , crDeprecated    :: Maybe DeprecationInfo  -- ^ Deprecation info if deprecated
  -- Module context (TODO-012)
  , crFromModule    :: Maybe Text       -- ^ Source module context (e.g., "Data.List")
  , crToModule      :: Maybe Text       -- ^ Target module context (e.g., "Data.Containers.ListUtils")
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ConfigurableRule where
  toJSON ConfigurableRule{..} = object
    [ "id" AE..= crId
    , "category" AE..= crCategory
    , "pattern" AE..= cpPattern crPattern
    , "fix" AE..= cpFix crPattern
    , "where" AE..= cpWhere crPattern
    , "message" AE..= cpMessage crPattern
    , "severity" AE..= crSeverity
    , "safety" AE..= crSafety
    , "enabled" AE..= crEnabled
    , "within" AE..= crWithin
    , "except" AE..= crExcept
    , "note" AE..= crNote
    , "add-imports" AE..= crAddImports
    , "remove-imports" AE..= crRemoveImports
    , "target" AE..= crTarget
    , "tags" AE..= crTags
    , "references" AE..= crReferences
    , "explanation" AE..= crExplanation
    , "depends-on" AE..= crDependsOn
    , "conflicts-with" AE..= crConflictsWith
    -- Rule versioning (TODO-011)
    , "version" AE..= (showVersion <$> crVersion)
    , "deprecated" AE..= crDeprecated
    -- Module context (TODO-012)
    , "from-module" AE..= crFromModule
    , "to-module" AE..= crToModule
    ]

instance FromJSON ConfigurableRule where
  parseJSON = withObject "ConfigurableRule" $ \o -> do
    crId <- o .: "id"
    crCategory <- o .:? "category" .!= inferCategory crId
    patternText <- o .: "pattern"
    fixText <- o .:? "fix"
    whereText <- o .:? "where"
    messageText <- o .: "message"
    let crPattern = ConfigPattern patternText fixText whereText messageText
    crSeverity <- o .:? "severity" .!= Warning
    crSafety <- o .:? "safety" .!= Safe
    crEnabled <- o .:? "enabled" .!= True
    crWithin <- o .:? "within" .!= []
    crExcept <- o .:? "except" .!= []
    crNote <- o .:? "note"
    -- New fields for TODO-004 through TODO-007
    crAddImports <- o .:? "add-imports" .!= []
    crRemoveImports <- o .:? "remove-imports" .!= []
    crTarget <- o .:? "target"
    crTags <- o .:? "tags" .!= []
    crReferences <- o .:? "references" .!= []
    crExplanation <- o .:? "explanation"
    -- Fix ordering fields (TODO-010)
    crDependsOn <- o .:? "depends-on" .!= []
    crConflictsWith <- o .:? "conflicts-with" .!= []
    -- Rule versioning fields (TODO-011)
    versionText <- o .:? "version"
    let crVersion = versionText >>= parseVersion
    crDeprecated <- o .:? "deprecated"
    -- Module context fields (TODO-012)
    crFromModule <- o .:? "from-module"
    crToModule <- o .:? "to-module"
    pure ConfigurableRule{..}

-- | Infer category from rule ID (e.g., "performance/foo" -> Performance)
inferCategory :: Text -> RuleCategory
inferCategory ruleId = case T.takeWhile (/= '/') ruleId of
  "performance" -> Performance
  "space-leaks" -> SpaceLeaks
  "security" -> Security
  "modernize" -> Modernize
  "redundant" -> Redundant
  "partial" -> Partial
  "imports" -> Imports
  "naming" -> Naming
  "extensions" -> Extensions
  "complexity" -> Complexity
  other -> CustomCategory other

--------------------------------------------------------------------------------
-- Restrictions
--------------------------------------------------------------------------------

-- | Function restriction configuration
data FunctionRestriction = FunctionRestriction
  { frName    :: Text       -- ^ Function name (can be qualified)
  , frWithin  :: [Text]     -- ^ Allowed modules (empty = nowhere)
  , frMessage :: Maybe Text -- ^ Custom message
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FunctionRestriction where
  toJSON FunctionRestriction{..} = object
    [ "name" AE..= frName
    , "within" AE..= frWithin
    , "message" AE..= frMessage
    ]

instance FromJSON FunctionRestriction where
  parseJSON = withObject "FunctionRestriction" $ \o -> do
    frName <- o .: "name"
    frWithin <- o .:? "within" .!= []
    frMessage <- o .:? "message"
    pure FunctionRestriction{..}

-- | Module restriction configuration
-- | Module restriction configuration (hlint-compatible)
--
-- Example TOML:
-- @
-- [[restrictions.modules]]
-- names = ["Data.Map", "Data.Map.Strict"]
-- as = "Map"
--
-- [[restrictions.modules]]
-- names = ["Data.Set"]
-- as = "Set"
--
-- [[restrictions.modules]]
-- names = ["System.IO.Unsafe"]
-- within = []  # Banned everywhere
-- message = "Use safe alternatives instead of System.IO.Unsafe"
-- @
data ModuleRestriction = ModuleRestriction
  { mrNames   :: [Text]     -- ^ Module names to restrict
  , mrAs      :: Maybe Text -- ^ Required qualified import name (e.g., "Map" for Data.Map)
  , mrWithin  :: [Text]     -- ^ Allowed modules (empty = banned everywhere, Nothing = allowed everywhere)
  , mrMessage :: Maybe Text -- ^ Custom message
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ModuleRestriction where
  toJSON ModuleRestriction{..} = object
    [ "names" AE..= mrNames
    , "as" AE..= mrAs
    , "within" AE..= mrWithin
    , "message" AE..= mrMessage
    ]

instance FromJSON ModuleRestriction where
  parseJSON = withObject "ModuleRestriction" $ \o -> do
    mrNames <- o .:? "names" .!= []
    mrAs <- o .:? "as"
    mrWithin <- o .:? "within" .!= []
    mrMessage <- o .:? "message"
    pure ModuleRestriction{..}

-- | Extension restriction configuration
data ExtensionRestriction = ExtensionRestriction
  { erName    :: Text       -- ^ Extension name
  , erWithin  :: [Text]     -- ^ Allowed modules (empty = nowhere)
  , erMessage :: Maybe Text -- ^ Custom message
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ExtensionRestriction where
  toJSON ExtensionRestriction{..} = object
    [ "name" AE..= erName
    , "within" AE..= erWithin
    , "message" AE..= erMessage
    ]

instance FromJSON ExtensionRestriction where
  parseJSON = withObject "ExtensionRestriction" $ \o -> do
    erName <- o .: "name"
    erWithin <- o .:? "within" .!= []
    erMessage <- o .:? "message"
    pure ExtensionRestriction{..}

-- | All restrictions
data RestrictionsConfig = RestrictionsConfig
  { rcFunctions  :: [FunctionRestriction]
  , rcModules    :: [ModuleRestriction]
  , rcExtensions :: [ExtensionRestriction]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RestrictionsConfig where
  toJSON RestrictionsConfig{..} = object
    [ "functions" AE..= rcFunctions
    , "modules" AE..= rcModules
    , "extensions" AE..= rcExtensions
    ]

instance FromJSON RestrictionsConfig where
  parseJSON = withObject "RestrictionsConfig" $ \o -> do
    rcFunctions <- o .:? "functions" .!= []
    rcModules <- o .:? "modules" .!= []
    rcExtensions <- o .:? "extensions" .!= []
    pure RestrictionsConfig{..}

-- | Default restrictions configuration with common best-practice rules
-- inspired by hlint's module restrictions.
-- These can be overridden in project configuration.
defaultRestrictions :: RestrictionsConfig
defaultRestrictions = RestrictionsConfig
  { rcFunctions = []
  , rcModules = defaultModuleRestrictions
  , rcExtensions = []
  }

-- | Default module restrictions
-- By default, no module restrictions are enforced. Users can add their own
-- in their project's argus.toml configuration file.
defaultModuleRestrictions :: [ModuleRestriction]
defaultModuleRestrictions = []

-- | Look up the recommended alias for a module from the restrictions config.
-- This is the single source of truth for module aliases, used by both
-- ConfigurableRules and Imports modules.
--
-- >>> lookupRecommendedAlias defaultModuleRestrictions "Data.Text"
-- Just "T"
-- >>> lookupRecommendedAlias defaultModuleRestrictions "Data.Map.Strict"
-- Just "Map"
-- >>> lookupRecommendedAlias defaultModuleRestrictions "Unknown.Module"
-- Nothing
lookupRecommendedAlias :: [ModuleRestriction] -> Text -> Maybe Text
lookupRecommendedAlias restrictions moduleName =
  mrAs =<< find (elem moduleName . mrNames) restrictions

--------------------------------------------------------------------------------
-- Rule Override
--------------------------------------------------------------------------------

-- | Override settings for a specific rule
data RuleOverride = RuleOverride
  { roRuleId   :: Text
  , roSeverity :: Maybe Severity
  , roEnabled  :: Maybe Bool
  , roSafety   :: Maybe FixSafety
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RuleOverride where
  toJSON RuleOverride{..} = object
    [ "id" AE..= roRuleId
    , "severity" AE..= roSeverity
    , "enabled" AE..= roEnabled
    , "safety" AE..= roSafety
    ]

instance FromJSON RuleOverride where
  parseJSON = withObject "RuleOverride" $ \o -> do
    roRuleId <- o .: "id"
    roSeverity <- o .:? "severity"
    roEnabled <- o .:? "enabled"
    roSafety <- o .:? "safety"
    pure RuleOverride{..}

--------------------------------------------------------------------------------
-- Scope Configuration
--------------------------------------------------------------------------------

-- | Module-scoped configuration
data ScopeConfig = ScopeConfig
  { scModules   :: [Text]                      -- ^ Module patterns (glob)
  , scOverrides :: Map Text CategorySeverity   -- ^ Category overrides
  , scIgnore    :: [Text]                      -- ^ Rules to ignore
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ScopeConfig where
  toJSON ScopeConfig{..} = object
    [ "modules" AE..= scModules
    , "overrides" AE..= scOverrides
    , "ignore" AE..= scIgnore
    ]

instance FromJSON ScopeConfig where
  parseJSON = withObject "ScopeConfig" $ \o -> do
    scModules <- o .:? "modules" .!= []
    scOverrides <- o .:? "overrides" .!= Map.empty
    scIgnore <- o .:? "ignore" .!= []
    pure ScopeConfig{..}

--------------------------------------------------------------------------------
-- Main Rules Configuration
--------------------------------------------------------------------------------

-- | Complete rules configuration
data RulesConfig = RulesConfig
  { rcInheritDefaults    :: Bool                          -- ^ Inherit built-in rules
  , rcCategories         :: Map RuleCategory CategorySeverity  -- ^ Category severities
  , rcCustomRules        :: [ConfigurableRule]            -- ^ User-defined rules
  , rcIgnoreRules        :: [Text]                        -- ^ Rules to ignore
  , rcIgnoreWithin       :: [Text]                        -- ^ Module patterns to ignore
  , rcOverrides          :: [RuleOverride]                -- ^ Rule-specific overrides
  , rcRestrictions       :: RestrictionsConfig            -- ^ Restrictions
  , rcScopes             :: [ScopeConfig]                 -- ^ Module-scoped configs
  , rcFixSafe            :: [Text]                        -- ^ Rules safe to auto-fix
  , rcFixUnsafe          :: [Text]                        -- ^ Rules needing confirmation
  , rcFixDisabled        :: [Text]                        -- ^ Rules never to auto-fix
  -- TODO-009: Rule composition/templates
  , rcTemplates          :: [RuleTemplate]                -- ^ Rule templates
  , rcTemplatedRules     :: [TemplatedRule]               -- ^ Rules extending templates
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RulesConfig where
  toJSON RulesConfig{..} = object
    [ "inherit-defaults" AE..= rcInheritDefaults
    , "categories" AE..= rcCategories
    , "custom" AE..= rcCustomRules
    , "ignore" AE..= rcIgnoreRules
    , "ignore-within" AE..= rcIgnoreWithin
    , "overrides" AE..= rcOverrides
    , "restrictions" AE..= rcRestrictions
    , "scopes" AE..= rcScopes
    , "fix-safe" AE..= rcFixSafe
    , "fix-unsafe" AE..= rcFixUnsafe
    , "fix-disabled" AE..= rcFixDisabled
    , "templates" AE..= rcTemplates
    , "templated-rules" AE..= rcTemplatedRules
    ]

instance FromJSON RulesConfig where
  parseJSON = withObject "RulesConfig" $ \o -> do
    rcInheritDefaults <- o .:? "inherit-defaults" .!= True
    rcCategories <- o .:? "categories" .!= defaultCategories
    rcCustomRules <- o .:? "custom" .!= []
    rcIgnoreRules <- o .:? "ignore" .!= []
    rcIgnoreWithin <- o .:? "ignore-within" .!= []
    rcOverrides <- o .:? "overrides" .!= []
    rcRestrictions <- o .:? "restrictions" .!= defaultRestrictions
    rcScopes <- o .:? "scopes" .!= []
    rcFixSafe <- o .:? "fix-safe" .!= []
    rcFixUnsafe <- o .:? "fix-unsafe" .!= []
    rcFixDisabled <- o .:? "fix-disabled" .!= []
    rcTemplates <- o .:? "templates" .!= []
    rcTemplatedRules <- o .:? "templated-rules" .!= []
    pure RulesConfig{..}

-- | Default category severities
defaultCategories :: Map RuleCategory CategorySeverity
defaultCategories = Map.fromList
  [ (Performance, CatWarn)
  , (SpaceLeaks, CatWarn)
  , (Security, CatError)
  , (Modernize, CatSuggestion)
  , (Redundant, CatWarn)
  , (Partial, CatWarn)
  , (Imports, CatSuggestion)
  , (Naming, CatSuggestion)
  , (Extensions, CatInfo)
  , (Complexity, CatWarn)
  ]

-- | Default rules configuration
defaultRulesConfig :: RulesConfig
defaultRulesConfig = RulesConfig
  { rcInheritDefaults = True
  , rcCategories = defaultCategories
  , rcCustomRules = []
  , rcIgnoreRules = []
  , rcIgnoreWithin = []
  , rcOverrides = []
  , rcRestrictions = defaultRestrictions
  , rcScopes = []
  , rcFixSafe = []
  , rcFixUnsafe = []
  , rcFixDisabled = []
  , rcTemplates = []
  , rcTemplatedRules = []
  }

--------------------------------------------------------------------------------
-- Configuration Loading
--------------------------------------------------------------------------------

-- | Load rules configuration from standard locations
loadRulesConfig :: Maybe FilePath -> IO RulesConfig
loadRulesConfig mpath = do
  -- Load default config
  defaultCfg <- loadDefaultRulesConfig

  -- Load project config
  projectCfg <- loadProjectRulesConfig

  -- Load user config
  userCfg <- loadUserRulesConfig

  -- Load explicit path config
  explicitCfg <- case mpath of
    Just path -> loadRulesConfigFromFile path
    Nothing -> pure Nothing

  -- Merge configs: defaults <- project <- user <- explicit
  let merged = foldr mergeRulesConfig defaultCfg
                 (catMaybes [projectCfg, userCfg, explicitCfg])

  -- Expand all templated rules (TODO-009)
  let expanded = expandTemplates merged

  -- Check for deprecated rules and emit warnings (IMPL-006)
  let warnings = checkDeprecations (rcCustomRules expanded)
  mapM_ (emitDeprecationWarning stderr) warnings

  pure expanded

-- | Load default rules configuration
--
-- Loads rules from data/default-rules.toml bundled with the package.
-- Falls back to the hardcoded defaults if the file is not found.
loadDefaultRulesConfig :: IO RulesConfig
loadDefaultRulesConfig = do
  -- Try to load from the package's data directory
  defaultPath <- getDataFileName "data/default-rules.toml"
  mConfig <- loadRulesConfigFromFile defaultPath
  case mConfig of
    Just cfg -> pure $ mergeRulesConfig cfg defaultRulesConfig
    Nothing -> pure defaultRulesConfig

-- | Load project-level rules config
loadProjectRulesConfig :: IO (Maybe RulesConfig)
loadProjectRulesConfig = do
  let candidates = ["argus.toml", ".argus.toml", "linter.toml", ".linter.toml"]
  findAndLoadFirst candidates

-- | Load user-level rules config
loadUserRulesConfig :: IO (Maybe RulesConfig)
loadUserRulesConfig = do
  home <- getHomeDirectory
  let userConfig = home </> ".config" </> "argus" </> "rules.toml"
  exists <- doesFileExist userConfig
  if exists
    then loadRulesConfigFromFile userConfig
    else pure Nothing

-- | Find and load the first existing config file
findAndLoadFirst :: [FilePath] -> IO (Maybe RulesConfig)
findAndLoadFirst [] = pure Nothing
findAndLoadFirst (f:fs) = do
  exists <- doesFileExist f
  if exists
    then loadRulesConfigFromFile f
    else findAndLoadFirst fs

-- | Load rules config from a specific file
loadRulesConfigFromFile :: FilePath -> IO (Maybe RulesConfig)
loadRulesConfigFromFile path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      content <- TIO.readFile path
      case TOML.decode rulesConfigCodec content of
        Left _err -> pure Nothing  -- Silently fail, could log warning
        Right cfg -> pure (Just cfg)

-- | Merge two rules configurations (second overrides first)
mergeRulesConfig :: RulesConfig -> RulesConfig -> RulesConfig
mergeRulesConfig override base = RulesConfig
  { rcInheritDefaults = rcInheritDefaults override
  , rcCategories = Map.union (rcCategories override) (rcCategories base)
  , rcCustomRules = rcCustomRules override ++ rcCustomRules base
  , rcIgnoreRules = rcIgnoreRules override ++ rcIgnoreRules base
  , rcIgnoreWithin = rcIgnoreWithin override ++ rcIgnoreWithin base
  , rcOverrides = rcOverrides override ++ rcOverrides base
  , rcRestrictions = mergeRestrictions (rcRestrictions override) (rcRestrictions base)
  , rcScopes = rcScopes override ++ rcScopes base
  , rcFixSafe = rcFixSafe override ++ rcFixSafe base
  , rcFixUnsafe = rcFixUnsafe override ++ rcFixUnsafe base
  , rcFixDisabled = rcFixDisabled override ++ rcFixDisabled base
  , rcTemplates = rcTemplates override ++ rcTemplates base
  , rcTemplatedRules = rcTemplatedRules override ++ rcTemplatedRules base
  }

mergeRestrictions :: RestrictionsConfig -> RestrictionsConfig -> RestrictionsConfig
mergeRestrictions a b = RestrictionsConfig
  { rcFunctions = rcFunctions a ++ rcFunctions b
  , rcModules = rcModules a ++ rcModules b
  , rcExtensions = rcExtensions a ++ rcExtensions b
  }

--------------------------------------------------------------------------------
-- TOML Codec
--------------------------------------------------------------------------------

rulesConfigCodec :: TomlCodec RulesConfig
rulesConfigCodec = RulesConfig
  <$> boolWithDefault True "inherit-defaults" .= rcInheritDefaults
  <*> TOML.dimap Just (fromMaybe defaultCategories)
        (TOML.dioptional (TOML.table categoriesCodec "categories")) .= rcCategories
  <*> listWithDefault [] customRuleCodec "custom" .= rcCustomRules
  <*> textArrayWithDefault [] "ignore" .= rcIgnoreRules
  <*> textArrayWithDefault [] "ignore-within" .= rcIgnoreWithin
  <*> listWithDefault [] overrideCodec "overrides" .= rcOverrides
  <*> TOML.dimap Just (fromMaybe defaultRestrictions)
        (TOML.dioptional (TOML.table restrictionsCodec "restrictions")) .= rcRestrictions
  <*> listWithDefault [] scopeCodec "scopes" .= rcScopes
  <*> textArrayWithDefault [] "fix-safe" .= rcFixSafe
  <*> textArrayWithDefault [] "fix-unsafe" .= rcFixUnsafe
  <*> textArrayWithDefault [] "fix-disabled" .= rcFixDisabled
  <*> listWithDefault [] ruleTemplateCodec "rule-templates" .= rcTemplates
  <*> listWithDefault [] templatedRuleCodec "templated-rules" .= rcTemplatedRules

categoriesCodec :: TomlCodec (Map RuleCategory CategorySeverity)
categoriesCodec = TOML.dimap toList fromList $ TOML.list categoryPairCodec "category"
  where
    toList = Map.toList
    fromList = Map.fromList

categoryPairCodec :: TomlCodec (RuleCategory, CategorySeverity)
categoryPairCodec = (,)
  <$> categoryCodec "name" .= fst
  <*> catSeverityCodec "level" .= snd

categoryCodec :: TOML.Key -> TomlCodec RuleCategory
categoryCodec key = TOML.textBy toText fromText key
  where
    toText = \case
      RT.Performance -> "performance"
      RT.SpaceLeaks -> "space-leaks"
      RT.Security -> "security"
      RT.Modernization -> "modernize"
      RT.Redundant -> "redundant"
      RT.Safety -> "partial"
      RT.Imports -> "imports"
      RT.Naming -> "naming"
      RT.Extensions -> "extensions"
      RT.Complexity -> "complexity"
      RT.Concurrency -> "concurrency"
      RT.ErrorHandling -> "error-handling"
      RT.Documentation -> "documentation"
      RT.Custom t -> t
      RT.Style -> "style"
      RT.Correctness -> "correctness"
    fromText t = Right $ case T.toLower t of
      "performance" -> RT.Performance
      "space-leaks" -> RT.SpaceLeaks
      "security" -> RT.Security
      "modernize" -> RT.Modernization
      "redundant" -> RT.Redundant
      "partial" -> RT.Safety
      "safety" -> RT.Safety
      "imports" -> RT.Imports
      "naming" -> RT.Naming
      "extensions" -> RT.Extensions
      "complexity" -> RT.Complexity
      "concurrency" -> RT.Concurrency
      "error-handling" -> RT.ErrorHandling
      "errorhandling" -> RT.ErrorHandling
      "documentation" -> RT.Documentation
      "style" -> RT.Style
      "correctness" -> RT.Correctness
      other -> RT.Custom other

catSeverityCodec :: TOML.Key -> TomlCodec CategorySeverity
catSeverityCodec key = TOML.textBy toText fromText key
  where
    toText = \case
      CatOff -> "off"
      CatError -> "error"
      CatWarn -> "warn"
      CatSuggestion -> "suggestion"
      CatInfo -> "info"
    fromText t = Right $ case T.toLower t of
      "off" -> CatOff
      "error" -> CatError
      "warn" -> CatWarn
      "warning" -> CatWarn
      "suggestion" -> CatSuggestion
      "info" -> CatInfo
      _ -> CatWarn

customRuleCodec :: TomlCodec ConfigurableRule
customRuleCodec = ConfigurableRule
  <$> TOML.text "id" .= crId
  <*> categoryCodec "category" .= crCategory
  <*> patternCodec .= crPattern
  <*> severityCodec "severity" .= crSeverity
  <*> safetyCodec "safety" .= crSafety
  <*> boolWithDefault True "enabled" .= crEnabled
  <*> textArrayWithDefault [] "within" .= crWithin
  <*> textArrayWithDefault [] "except" .= crExcept
  <*> TOML.dioptional (TOML.text "note") .= crNote
  -- New fields for TODO-004 through TODO-007
  <*> listWithDefault [] importSpecCodec "add-imports" .= crAddImports
  <*> textArrayWithDefault [] "remove-imports" .= crRemoveImports
  <*> TOML.dioptional (ruleTargetCodec "target") .= crTarget
  <*> textArrayWithDefault [] "tags" .= crTags
  <*> textArrayWithDefault [] "references" .= crReferences
  <*> TOML.dioptional (TOML.text "explanation") .= crExplanation
  -- Fix ordering fields (TODO-010)
  <*> textArrayWithDefault [] "depends-on" .= crDependsOn
  <*> textArrayWithDefault [] "conflicts-with" .= crConflictsWith
  -- Rule versioning fields (TODO-011)
  <*> TOML.dioptional (versionCodec "version") .= crVersion
  <*> TOML.dioptional (TOML.table deprecationCodec "deprecated") .= crDeprecated
  -- Module context fields (TODO-012)
  <*> TOML.dioptional (TOML.text "from-module") .= crFromModule
  <*> TOML.dioptional (TOML.text "to-module") .= crToModule

-- | Codec for Version
versionCodec :: TOML.Key -> TomlCodec Version
versionCodec key = TOML.dimap showVersion parseVersionUnsafe (TOML.text key)
  where
    parseVersionUnsafe :: Text -> Version
    parseVersionUnsafe t = fromMaybe (Version 0 0 0 Nothing) (parseVersion t)

-- | Codec for DeprecationInfo
deprecationCodec :: TomlCodec DeprecationInfo
deprecationCodec = DeprecationInfo
  <$> TOML.text "reason" .= diReason
  <*> TOML.dioptional (TOML.text "replacement") .= diReplacement
  <*> TOML.dioptional (versionCodec "removal") .= diRemovalVersion

-- | Codec for ImportSpec
importSpecCodec :: TomlCodec RT.ImportSpec
importSpecCodec = RT.ImportSpec
  <$> TOML.text "module" .= RT.impModule
  <*> listWithDefault [] importSymbolCodec "symbols" .= RT.impSymbols
  <*> TOML.dioptional (TOML.text "qualified") .= RT.impQualified
  <*> boolWithDefault False "hiding" .= RT.impHiding
  <*> TOML.dioptional (TOML.text "package") .= RT.impPackage

-- | Codec for ImportSymbol
importSymbolCodec :: TomlCodec RT.ImportSymbol
importSymbolCodec = RT.ImportSymbol
  <$> TOML.text "name" .= RT.symName
  <*> symbolTypeCodec "kind" .= RT.symType
  <*> textArrayWithDefault [] "children" .= RT.symChildren

-- | Codec for SymbolType
symbolTypeCodec :: TOML.Key -> TomlCodec RT.SymbolType
symbolTypeCodec key = TOML.textBy toText fromText key
  where
    toText = \case
      RT.SymFunction -> "function"
      RT.SymType -> "type"
      RT.SymClass -> "class"
      RT.SymPattern -> "pattern"
      RT.SymOperator -> "operator"
      RT.SymConstructor -> "constructor"
    fromText t = Right $ case T.toLower t of
      "function" -> RT.SymFunction
      "type" -> RT.SymType
      "class" -> RT.SymClass
      "pattern" -> RT.SymPattern
      "operator" -> RT.SymOperator
      "constructor" -> RT.SymConstructor
      _ -> RT.SymFunction

-- | Codec for RuleTarget
ruleTargetCodec :: TOML.Key -> TomlCodec RT.RuleTarget
ruleTargetCodec key = TOML.textBy toText fromText key
  where
    toText = \case
      RT.TargetCode -> "code"
      RT.TargetComments -> "comments"
      RT.TargetDocumentation -> "documentation"
      RT.TargetPragmas -> "pragmas"
      RT.TargetStrings -> "strings"
      RT.TargetAll -> "all"
    fromText t = Right $ case T.toLower t of
      "code" -> RT.TargetCode
      "comments" -> RT.TargetComments
      "documentation" -> RT.TargetDocumentation
      "docs" -> RT.TargetDocumentation
      "pragmas" -> RT.TargetPragmas
      "strings" -> RT.TargetStrings
      "all" -> RT.TargetAll
      _ -> RT.TargetCode

patternCodec :: TomlCodec ConfigPattern
patternCodec = ConfigPattern
  <$> TOML.text "pattern" .= cpPattern
  <*> TOML.dioptional (TOML.text "fix") .= cpFix
  <*> TOML.dioptional (TOML.text "where") .= cpWhere
  <*> TOML.text "message" .= cpMessage

severityCodec :: TOML.Key -> TomlCodec Severity
severityCodec key = TOML.textBy toText fromText key
  where
    toText = \case
      Error -> "error"
      Warning -> "warning"
      Suggestion -> "suggestion"
      Info -> "info"
    fromText t = Right $ case T.toLower t of
      "error" -> Error
      "warning" -> Warning
      "warn" -> Warning
      "suggestion" -> Suggestion
      "info" -> Info
      _ -> Warning

safetyCodec :: TOML.Key -> TomlCodec FixSafety
safetyCodec key = TOML.textBy toText fromText key
  where
    toText = \case
      RT.Safe -> "safe"
      RT.MostlySafe -> "mostly-safe"
      RT.NeedsReview -> "manual"
      RT.Unsafe -> "unsafe"
    fromText t = Right $ case T.toLower t of
      "safe" -> RT.Safe
      "mostly-safe" -> RT.MostlySafe
      "mostlysafe" -> RT.MostlySafe
      "manual" -> RT.NeedsReview
      "needs-review" -> RT.NeedsReview
      "unsafe" -> RT.Unsafe
      _ -> RT.Safe

overrideCodec :: TomlCodec RuleOverride
overrideCodec = RuleOverride
  <$> TOML.text "id" .= roRuleId
  <*> TOML.dioptional (severityCodec "severity") .= roSeverity
  <*> TOML.dioptional (TOML.bool "enabled") .= roEnabled
  <*> TOML.dioptional (safetyCodec "safety") .= roSafety

restrictionsCodec :: TomlCodec RestrictionsConfig
restrictionsCodec = RestrictionsConfig
  <$> listWithDefault [] functionRestrictionCodec "functions" .= rcFunctions
  <*> listWithDefault [] moduleRestrictionCodec "modules" .= rcModules
  <*> listWithDefault [] extensionRestrictionCodec "extensions" .= rcExtensions

functionRestrictionCodec :: TomlCodec FunctionRestriction
functionRestrictionCodec = FunctionRestriction
  <$> TOML.text "name" .= frName
  <*> textArrayWithDefault [] "within" .= frWithin
  <*> TOML.dioptional (TOML.text "message") .= frMessage

moduleRestrictionCodec :: TomlCodec ModuleRestriction
moduleRestrictionCodec = ModuleRestriction
  <$> textArrayWithDefault [] "names" .= mrNames
  <*> TOML.dioptional (TOML.text "as") .= mrAs
  <*> textArrayWithDefault [] "within" .= mrWithin
  <*> TOML.dioptional (TOML.text "message") .= mrMessage

extensionRestrictionCodec :: TomlCodec ExtensionRestriction
extensionRestrictionCodec = ExtensionRestriction
  <$> TOML.text "name" .= erName
  <*> textArrayWithDefault [] "within" .= erWithin
  <*> TOML.dioptional (TOML.text "message") .= erMessage

scopeCodec :: TomlCodec ScopeConfig
scopeCodec = ScopeConfig
  <$> textArrayWithDefault [] "modules" .= scModules
  <*> TOML.dimap Just (fromMaybe Map.empty)
        (TOML.dioptional (TOML.table textCategoriesCodec "overrides")) .= scOverrides
  <*> textArrayWithDefault [] "ignore" .= scIgnore

-- | Codec for Text -> CategorySeverity map (for scope overrides)
textCategoriesCodec :: TomlCodec (Map Text CategorySeverity)
textCategoriesCodec = TOML.dimap toList fromList $ TOML.list textCategoryPairCodec "override"
  where
    toList = Map.toList
    fromList = Map.fromList

textCategoryPairCodec :: TomlCodec (Text, CategorySeverity)
textCategoryPairCodec = (,)
  <$> TOML.text "name" .= fst
  <*> catSeverityCodec "level" .= snd

-- | Helper for text array with default
textArrayWithDefault :: [Text] -> TOML.Key -> TomlCodec [Text]
textArrayWithDefault def key =
  TOML.dimap Just (fromMaybe def) $ TOML.dioptional (TOML.arrayOf TOML._Text key)

-- | Helper for bool with default
boolWithDefault :: Bool -> TOML.Key -> TomlCodec Bool
boolWithDefault def key =
  TOML.dimap Just (fromMaybe def) $ TOML.dioptional (TOML.bool key)

-- | Helper for list with default
listWithDefault :: [a] -> TomlCodec a -> TOML.Key -> TomlCodec [a]
listWithDefault def codec key =
  TOML.dimap Just (fromMaybe def) $ TOML.dioptional (TOML.list codec key)

--------------------------------------------------------------------------------
-- Rule Template Codecs (TODO-009)
--------------------------------------------------------------------------------

-- | Codec for RuleTemplate
ruleTemplateCodec :: TomlCodec RuleTemplate
ruleTemplateCodec = RuleTemplate
  <$> TOML.text "id" .= rtId
  <*> TOML.dioptional (categoryCodec "category") .= rtCategory
  <*> TOML.dioptional (severityCodec "severity") .= rtSeverity
  <*> TOML.dioptional (safetyCodec "safety") .= rtSafety
  <*> TOML.dioptional (TOML.text "pattern-template") .= rtPatternTemplate
  <*> TOML.dioptional (TOML.text "fix-template") .= rtFixTemplate
  <*> TOML.dioptional (TOML.text "message-template") .= rtMessageTemplate
  <*> listWithDefault [] importSpecCodec "add-imports" .= rtAddImports
  <*> textArrayWithDefault [] "remove-imports" .= rtRemoveImports
  <*> textArrayWithDefault [] "tags" .= rtTags
  <*> textArrayWithDefault [] "within" .= rtWithin
  <*> textArrayWithDefault [] "except" .= rtExcept
  <*> TOML.dioptional (TOML.text "note") .= rtNote

-- | Codec for TemplatedRule
templatedRuleCodec :: TomlCodec TemplatedRule
templatedRuleCodec = TemplatedRule
  <$> TOML.text "extends" .= trExtends
  <*> TOML.text "id" .= trId
  <*> varsMapCodec .= trVars
  <*> TOML.dioptional (TOML.text "pattern") .= trPattern
  <*> TOML.dioptional (TOML.text "fix") .= trFix
  <*> TOML.dioptional (TOML.text "message") .= trMessage
  <*> TOML.dioptional (categoryCodec "category") .= trCategory
  <*> TOML.dioptional (severityCodec "severity") .= trSeverity
  <*> TOML.dioptional (safetyCodec "safety") .= trSafety
  <*> TOML.dioptional (TOML.bool "enabled") .= trEnabled
  <*> listWithDefault [] importSpecCodec "add-imports" .= trAddImports
  <*> textArrayWithDefault [] "remove-imports" .= trRemoveImports
  <*> textArrayWithDefault [] "tags" .= trTags
  <*> textArrayWithDefault [] "within" .= trWithin
  <*> textArrayWithDefault [] "except" .= trExcept
  <*> TOML.dioptional (TOML.text "note") .= trNote
  <*> TOML.dioptional (TOML.text "explanation") .= trExplanation

-- | Codec for vars map (key-value pairs)
varsMapCodec :: TomlCodec (Map Text Text)
varsMapCodec = TOML.dimap toList fromList $ listWithDefault [] varPairCodec "vars"
  where
    toList = Map.toList
    fromList = Map.fromList

varPairCodec :: TomlCodec (Text, Text)
varPairCodec = (,)
  <$> TOML.text "name" .= fst
  <*> TOML.text "value" .= snd

--------------------------------------------------------------------------------
-- Template Expansion
--------------------------------------------------------------------------------

-- | Expand all templated rules in a config
expandTemplates :: RulesConfig -> RulesConfig
expandTemplates cfg =
  let templateMap = Map.fromList [(rtId t, t) | t <- rcTemplates cfg]
      expandedRules = mapMaybe (expandIfValid templateMap) (rcTemplatedRules cfg)
  in cfg { rcCustomRules = rcCustomRules cfg ++ expandedRules }
  where
    expandIfValid tplMap tr =
      case Map.lookup (trExtends tr) tplMap of
        Just tpl -> Just (expandTemplatedRule tpl tr)
        Nothing  -> Nothing  -- Skip rules with missing templates

-- | Find a template by ID
lookupTemplate :: [RuleTemplate] -> Text -> Maybe RuleTemplate
lookupTemplate templates templateId =
  find (\t -> rtId t == templateId) templates

--------------------------------------------------------------------------------
-- Rule Application
--------------------------------------------------------------------------------

-- | Check if a rule is enabled for a given module
isRuleEnabled :: RulesConfig -> ConfigurableRule -> Text -> Bool
isRuleEnabled cfg rule moduleName =
  -- First check if rule is globally enabled
  crEnabled rule
  -- Check if rule is in ignore list
  && crId rule `notElem` rcIgnoreRules cfg
  -- Check category is enabled
  && categoryEnabled (rcCategories cfg) (crCategory rule)
  -- Check within constraints (empty = all modules)
  && matchesWithin (crWithin rule) moduleName
  -- Check except constraints (empty = exclude nothing)
  && not (matchesAnyPattern (crExcept rule) moduleName)
  -- Check ignore-within patterns (empty = ignore nothing)
  && not (matchesAnyPattern (rcIgnoreWithin cfg) moduleName)
  -- Check scope-specific ignores
  && not (scopeIgnores cfg rule moduleName)
  where
    categoryEnabled cats cat =
      case Map.lookup cat cats of
        Just CatOff -> False
        _ -> True

-- | Check if module matches any pattern in the list
-- For "within" constraints: empty list means all modules match
matchesWithin :: [Text] -> Text -> Bool
matchesWithin [] _ = True  -- Empty list means all
matchesWithin patterns modName = any (matchPattern modName) patterns

-- | Check if module matches any pattern in the list
-- For "except"/"ignore" constraints: empty list means none match
matchesAnyPattern :: [Text] -> Text -> Bool
matchesAnyPattern [] _ = False  -- Empty list means none
matchesAnyPattern patterns modName = any (matchPattern modName) patterns

-- | Check if a pattern matches a module name (supports glob)
matchPattern :: Text -> Text -> Bool
matchPattern modName pat
  | "*" `T.isPrefixOf` pat = T.isSuffixOf (T.drop 1 pat) modName
  | "*" `T.isSuffixOf` pat = T.isPrefixOf (T.dropEnd 1 pat) modName
  | ".*" `T.isInfixOf` pat = modName =~ T.unpack pat
  | otherwise = modName == pat

-- | Check if any scope ignores this rule
scopeIgnores :: RulesConfig -> ConfigurableRule -> Text -> Bool
scopeIgnores cfg rule modName = any checkScope (rcScopes cfg)
  where
    checkScope sc =
      matchesWithin (scModules sc) modName
      && crId rule `elem` scIgnore sc

-- | Get the effective severity for a rule
getRuleEffectiveSeverity :: RulesConfig -> ConfigurableRule -> Text -> Maybe Severity
getRuleEffectiveSeverity cfg rule modName = do
  -- Check if rule is enabled first
  guard (isRuleEnabled cfg rule modName)

  -- Check for rule-specific override
  let mOverride = find (\o -> roRuleId o == crId rule) (rcOverrides cfg)
  case mOverride >>= roSeverity of
    Just sev -> Just sev
    Nothing -> do
      -- Check category severity
      let catSev = Map.lookup (crCategory rule) (rcCategories cfg)
      case catSev >>= catToSeverity of
        Just sev -> Just sev
        Nothing -> Just (crSeverity rule)

-- | Apply configurable rules to source code
applyConfigurableRules :: RulesConfig -> FilePath -> Text -> Text -> [Diagnostic]
applyConfigurableRules cfg filepath moduleName content =
  let allRules = getAllRules cfg
      enabledRules = filter (\r -> isRuleEnabled cfg r moduleName) allRules
  in concatMap (applyRule cfg filepath moduleName content) enabledRules

-- | Get all rules (default + custom)
getAllRules :: RulesConfig -> [ConfigurableRule]
getAllRules cfg =
  if rcInheritDefaults cfg
    then defaultRulesLibrary ++ rcCustomRules cfg
    else rcCustomRules cfg

-- | Apply a single rule to content
-- Respects the rule's target specification (code, comments, docs, etc.)
applyRule :: RulesConfig -> FilePath -> Text -> Text -> ConfigurableRule -> [Diagnostic]
applyRule cfg filepath moduleName content rule =
  let target = fromMaybe RT.TargetCode (crTarget rule)
      matches = findPatternMatchesWithTarget target (cpPattern $ crPattern rule) content
      filtered = filterBySideConditions (crPattern rule) matches
  in mapMaybe (matchToDiagnostic cfg rule filepath moduleName) filtered

-- | Filter matches by side conditions defined in the pattern's 'where' field
--
-- This enables TOML rules to use type-aware predicates like:
-- @
-- [[rules.custom]]
-- pattern = "head $X"
-- fix = "headMay $X"
-- where = "isList $X"
-- @
filterBySideConditions :: ConfigPattern -> [(Int, Int, Text, Map Text Text)] -> [(Int, Int, Text, Map Text Text)]
filterBySideConditions pat matches = case cpWhere pat of
  Nothing -> matches
  Just whereText -> case SCP.parseSideConditions whereText of
    Nothing -> matches  -- Invalid condition, be permissive
    Just cond -> filter (checkSideConditionMatch cond) matches

-- | Check if a single match satisfies the side condition
checkSideConditionMatch :: RT.SideCondition -> (Int, Int, Text, Map Text Text) -> Bool
checkSideConditionMatch cond (_, _, _, metavars) =
  SCP.evaluateSideCondition metavars cond

-- | Find pattern matches in content respecting target specification
-- Target controls WHERE in the source the rule should match:
--   TargetCode         - Match only in code (skip comments/strings) - DEFAULT
--   TargetComments     - Match only inside comments
--   TargetDocumentation - Match only inside Haddock docs (-- | or {-| -})
--   TargetStrings      - Match only inside string literals
--   TargetPragmas      - Match only inside pragmas ({-# ... #-})
findPatternMatchesWithTarget :: RT.RuleTarget -> Text -> Text -> [(Int, Int, Text, Map Text Text)]
findPatternMatchesWithTarget target patternText content =
  let lines' = zip [1..] (T.lines content)
      metavars = extractMetavars patternText
      regexPat = patternToCapturingRegex patternText metavars
  in concatMap (findInLineWithTarget target regexPat metavars) lines'
  where
    findInLineWithTarget :: RT.RuleTarget -> Text -> [Text] -> (Int, Text) -> [(Int, Int, Text, Map Text Text)]
    findInLineWithTarget tgt regexPat mvars (lineNum, lineText) =
      case captureMetavarsWithTarget tgt regexPat mvars lineText of
        Nothing -> []
        Just (col, matchedText, captures) ->
          [(lineNum, col, matchedText, captures)]

-- | Find pattern matches in content (default: TargetCode)
_findPatternMatches :: Text -> Text -> [(Int, Int, Text, Map Text Text)]
_findPatternMatches = findPatternMatchesWithTarget RT.TargetCode

-- | Capture metavariables from a line using the regex pattern with target awareness
-- Filters matches based on whether they're in code, comments, strings, docs, or pragmas
captureMetavarsWithTarget :: RT.RuleTarget -> Text -> [Text] -> Text -> Maybe (Int, Text, Map Text Text)
captureMetavarsWithTarget target regexPat metavars lineText =
  let lineStr = T.unpack lineText
      result :: (String, String, String, [String])
      result = lineStr =~ T.unpack regexPat
  in case result of
    (before, match, after, groups)
      | not (null match) ->
          let col = length before + 1
              captures = Map.fromList $ zip metavars (map T.pack groups)
              atWordBoundary = isWordBoundaryMatch before match after
              inComment = isInsideComment lineStr col
              inString = isInsideString lineStr col
              inHaddock = isInsideHaddock lineStr col
              inPragma = isInsidePragma lineStr col
              -- Check if match location is appropriate for target
              targetMatches = case target of
                RT.TargetAll           -> True  -- Match anywhere
                RT.TargetCode          -> not inComment && not inString && not inPragma
                RT.TargetComments      -> inComment && not inHaddock
                RT.TargetDocumentation -> inHaddock
                RT.TargetStrings       -> inString
                RT.TargetPragmas       -> inPragma
          in if targetMatches && atWordBoundary
             then Just (col, T.pack match, captures)
             else Nothing
      | otherwise -> Nothing

-- | Capture metavariables from a line using the regex pattern
-- Now also checks that matches are not inside string literals, comments,
-- and that identifier patterns appear at word boundaries (default: TargetCode)
_captureMetavars :: Text -> [Text] -> Text -> Maybe (Int, Text, Map Text Text)
_captureMetavars = captureMetavarsWithTarget RT.TargetCode

-- | Check if a column position is inside a comment
-- Handles line comments (--) and block comments ({- -})
isInsideComment :: String -> Int -> Bool
isInsideComment line col = go 0 1 line
  where
    go :: Int -> Int -> String -> Bool  -- Int tracks block comment depth
    go blockDepth currentCol []
      | currentCol >= col = blockDepth > 0  -- Inside block comment at target
      | otherwise = False
    go blockDepth currentCol (c:rest)
      | currentCol >= col = blockDepth > 0  -- Inside block comment
      | c == '-' && nextIs '-' rest && blockDepth == 0 = True  -- Line comment
      | c == '{' && nextIs '-' rest = go (blockDepth + 1) (currentCol + 2) (drop 1 rest)
      | c == '-' && nextIs '}' rest && blockDepth > 0 = go (blockDepth - 1) (currentCol + 2) (drop 1 rest)
      | otherwise = go blockDepth (currentCol + 1) rest

    nextIs :: Char -> String -> Bool
    nextIs _ [] = False
    nextIs expected (c:_) = c == expected

-- | Check if a match is at a word boundary (not inside an identifier)
isWordBoundaryMatch :: String -> String -> String -> Bool
isWordBoundaryMatch before match after =
  let beforeOk = case before of
        [] -> True
        _  -> not (isIdentChar (last before))
      afterOk = case after of
        [] -> True
        (c:_) -> not (isIdentChar c)
      -- Only apply word boundary check for identifier-like patterns
      isIdentLike = all isIdentCharOrSpace match
  in not isIdentLike || (beforeOk && afterOk)
  where
    isIdentChar c = isAlphaNum c || c == '_' || c == '\''
    isIdentCharOrSpace c = isAlphaNum c || c == '_' || c == '\'' || c == ' '
    isAlphaNum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

-- | Check if a column position is inside a string literal
-- This handles escaped quotes and character literals
isInsideString :: String -> Int -> Bool
isInsideString line col = go False 1 line
  where
    go :: Bool -> Int -> String -> Bool
    go inString currentCol [] = inString && currentCol <= col
    go inString currentCol (c:rest)
      | currentCol >= col = inString  -- Reached target position
      | c == '"' = if inString
                   then go False (currentCol + 1) rest  -- End string
                   else go True (currentCol + 1) rest   -- Start string
      | c == '\\' && inString = go inString (currentCol + 2) (drop 1 rest)  -- Skip escape
      | c == '\'' && not inString = skipCharLiteral (currentCol + 1) rest  -- Skip char literal
      | otherwise = go inString (currentCol + 1) rest

    -- Skip character literals like 'a', '\'', '\n'
    skipCharLiteral currentCol ('\\':_:('\'':rest)) = go False (currentCol + 3) rest
    skipCharLiteral currentCol (_:('\'':rest)) = go False (currentCol + 2) rest
    skipCharLiteral currentCol rest = go False currentCol rest  -- Not a valid char literal

-- | Check if a column position is inside a Haddock documentation comment
-- Haddock comments start with "-- |", "-- ^", "{-|", or "{-^"
isInsideHaddock :: String -> Int -> Bool
isInsideHaddock line col =
  let lineStr = dropWhile (== ' ') line
  in case lineStr of
       -- Line Haddock: "-- |" or "-- ^"
       ('-':'-':' ':'|':_) -> col > 4
       ('-':'-':' ':'^':_) -> col > 4
       ('-':'-':'|':_)     -> col > 3
       ('-':'-':'^':_)     -> col > 3
       -- Block Haddock: "{-|" or "{-^"
       ('{':'-':'|':_)     -> isInsideBlockHaddock line col
       ('{':'-':'^':_)     -> isInsideBlockHaddock line col
       _                   -> False
  where
    isInsideBlockHaddock :: String -> Int -> Bool
    isInsideBlockHaddock ln c =
      let before = take (c - 1) ln
          after = drop (c - 1) ln
          hasStart = "{-|" `isInfixOf` before || "{-^" `isInfixOf` before
          hasEnd = "-}" `isInfixOf` before
      in hasStart && not hasEnd || ("{-|" `isInfixOf` ln || "{-^" `isInfixOf` ln) && not ("-}" `isInfixOf` after)

-- | Check if a column position is inside a pragma
-- Pragmas are delimited by {-# and #-}
isInsidePragma :: String -> Int -> Bool
isInsidePragma line col = go False 1 line
  where
    go :: Bool -> Int -> String -> Bool
    go inPragma currentCol []
      | currentCol >= col = inPragma
      | otherwise = False
    go inPragma currentCol ('{':'-':'#':rest)
      | currentCol + 2 < col = go True (currentCol + 3) rest
      | otherwise = inPragma
    go inPragma currentCol ('#':'-':'}':rest)
      | inPragma && currentCol + 2 < col = go False (currentCol + 3) rest
      | otherwise = inPragma
    go inPragma currentCol (_:rest)
      | currentCol >= col = inPragma
      | otherwise = go inPragma (currentCol + 1) rest

-- | Convert pattern to a capturing regex (with groups for metavariables)
patternToCapturingRegex :: Text -> [Text] -> Text
patternToCapturingRegex pat metavars =
  -- First, create a regex with capturing groups for each metavariable
  let escaped = escapeRegexLiteral pat
      -- Replace metavariables with capturing groups
      withCaptures = foldr replaceMetavar escaped metavars
  in withCaptures
  where
    replaceMetavar :: Text -> Text -> Text
    replaceMetavar mv txt =
      let escapedMv = "\\" <> mv  -- Pattern like \$X after escaping
          captureGroup = metavarToCapture mv
      in T.replace escapedMv captureGroup txt

    -- Convert metavariable to appropriate capture group
    metavarToCapture :: Text -> Text
    metavarToCapture mv = case mv of
      "$X"    -> "(.+)"                    -- Capture any expression
      "$Y"    -> "(.+)"                    -- Capture any expression
      "$Z"    -> "(.+)"                    -- Capture any expression
      "$F"    -> "([a-z][a-zA-Z0-9'_]*)"   -- Capture function name
      "$FUNC" -> "([a-z][a-zA-Z0-9'_]*)"   -- Capture function name
      "$N"    -> "([0-9]+)"                -- Capture number
      _       -> "(.+)"                    -- Default: capture anything

-- | Extract metavariables from a pattern (e.g., "$X", "$FUNC")
extractMetavars :: Text -> [Text]
extractMetavars pat =
  map T.pack $ getAllTextMatches (T.unpack pat =~ ("\\$[A-Z][A-Z0-9_]*" :: String))

-- | Convert pattern to regex
-- First replace metavariables with placeholders, then escape, then replace placeholders with regex
_patternToRegex :: Text -> Text
_patternToRegex pat =
  -- Use simple substring matching for literal parts
  -- Replace metavariables with .* for loose matching
  let escaped = escapeRegexLiteral pat
  in T.replace "\\$X" ".*"
   $ T.replace "\\$Y" ".*"
   $ T.replace "\\$Z" ".*"
   $ T.replace "\\$F" "[a-z][a-zA-Z0-9'_]*"
   $ T.replace "\\$FUNC" "[a-z][a-zA-Z0-9'_]*"
   $ T.replace "\\$N" "[0-9]+"
   $ escaped

-- | Escape regex special characters in literal pattern text
escapeRegexLiteral :: Text -> Text
escapeRegexLiteral = T.concatMap escape
  where
    escape c
      | c `elem` (".^*+?{}[]\\|()" :: String) = T.pack ['\\', c]
      | c == '$' = "\\$"  -- Escape $ but we'll replace metavar escapes later
      | otherwise = T.singleton c

-- | Convert a match to a diagnostic
matchToDiagnostic :: RulesConfig -> ConfigurableRule -> FilePath -> Text -> (Int, Int, Text, Map Text Text) -> Maybe Diagnostic
matchToDiagnostic cfg rule filepath moduleName (line, col, matchedText, metavars) = do
  severity <- getRuleEffectiveSeverity cfg rule moduleName
  let srcSpan = mkSrcSpanRaw filepath line col line (col + T.length matchedText)
      msg = interpolateMetavars (cpMessage $ crPattern rule) metavars
      fix' = mkRuleFix cfg rule srcSpan matchedText metavars
  pure Diagnostic
    { diagSpan = srcSpan
    , diagSeverity = severity
    , diagKind = CodePattern
    , diagMessage = msg
    , diagCode = Just (crId rule)
    , diagFixes = maybeToList fix'
    , diagRelated = []
    }

-- | Interpolate metavariables in message
interpolateMetavars :: Text -> Map Text Text -> Text
interpolateMetavars msg vars = Map.foldrWithKey replace msg vars
  where
    replace k v = T.replace k v

-- | Create a fix for the rule
mkRuleFix :: RulesConfig -> ConfigurableRule -> SrcSpan -> Text -> Map Text Text -> Maybe Fix
mkRuleFix cfg rule srcSpan _matchedText metavars = do
  -- Check if fix is allowed
  guard (canAutoFix cfg rule)

  -- Get fix pattern
  fixPat <- cpFix (crPattern rule)

  -- Apply metavariables to fix pattern
  let fixText = interpolateMetavars fixPat metavars

  pure Fix
    { fixTitle = "Apply: " <> crId rule
    , fixEdits = [FixEdit srcSpan fixText]
    , fixIsPreferred = crSafety rule == Safe
    -- Convert RT.ImportSpec to FixImport (TODO-004)
    , fixAddImports = map convertImportSpecToFixImport (crAddImports rule)
    , fixRemoveImports = crRemoveImports rule
    , fixCategory = ruleCategoryToFixCategory (crCategory rule)
    , fixSafety = ruleSafetyToTypeSafety (crSafety rule)
    }

-- | Convert RT.ImportSpec to FixImport (TODO-004)
convertImportSpecToFixImport :: RT.ImportSpec -> FixImport
convertImportSpecToFixImport imp = FixImport
  { fimpModule = RT.impModule imp
  , fimpSymbols = map convertImportSymbol (RT.impSymbols imp)
  , fimpQualified = RT.impQualified imp
  , fimpHiding = RT.impHiding imp
  , fimpPackage = RT.impPackage imp
  }

-- | Convert RT.ImportSymbol to Types.ImportSymbol
convertImportSymbol :: RT.ImportSymbol -> ImportSymbol
convertImportSymbol sym = ImportSymbol
  { isymName = RT.symName sym
  , isymType = convertSymbolType (RT.symType sym)
  , isymChildren = RT.symChildren sym
  }

-- | Convert RT.SymbolType to ImportSymbolType
convertSymbolType :: RT.SymbolType -> ImportSymbolType
convertSymbolType = \case
  RT.SymFunction -> ISTFunction
  RT.SymType -> ISTType
  RT.SymClass -> ISTClass
  RT.SymPattern -> ISTPattern
  RT.SymOperator -> ISTOperator
  RT.SymConstructor -> ISTConstructor

-- | Check if a rule can be auto-fixed
canAutoFix :: RulesConfig -> ConfigurableRule -> Bool
canAutoFix cfg rule =
  -- Check if rule has a fix
  case cpFix (crPattern rule) of
    Nothing -> False
    Just _ ->
      -- Check override lists
      crId rule `notElem` rcFixDisabled cfg
      && (crSafety rule == Safe || crId rule `elem` rcFixSafe cfg)
      && crId rule `notElem` rcFixUnsafe cfg

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- | Regex match helper
getAllTextMatches :: (String, String, String, [String]) -> [String]
getAllTextMatches (_, _, _, matches) = matches

--------------------------------------------------------------------------------
-- Restriction Checking
--------------------------------------------------------------------------------

-- | Check restrictions and generate diagnostics
checkRestrictions :: RulesConfig -> FilePath -> Text -> Text -> [Diagnostic]
checkRestrictions cfg filepath moduleName content =
  checkFunctionRestrictions (rcFunctions $ rcRestrictions cfg) filepath moduleName content
  ++ checkModuleRestrictions (rcModules $ rcRestrictions cfg) filepath moduleName content
  ++ checkExtensionRestrictions (rcExtensions $ rcRestrictions cfg) filepath moduleName content

checkFunctionRestrictions :: [FunctionRestriction] -> FilePath -> Text -> Text -> [Diagnostic]
checkFunctionRestrictions restrictions filepath moduleName content =
  concatMap (checkFnRestriction filepath moduleName content) restrictions

checkFnRestriction :: FilePath -> Text -> Text -> FunctionRestriction -> [Diagnostic]
checkFnRestriction filepath moduleName content FunctionRestriction{..}
  -- If module is in within list, function is allowed
  | matchesWithin frWithin moduleName = []
  -- Otherwise, find usages and report
  | otherwise = findFunctionUsages filepath frName frMessage content

findFunctionUsages :: FilePath -> Text -> Maybe Text -> Text -> [Diagnostic]
findFunctionUsages filepath funcName mMessage content =
  let lines' = zip [1..] (T.lines content)
      msg = fromMaybe ("Restricted function '" <> funcName <> "' is not allowed here") mMessage
  in mapMaybe (findUsage funcName msg filepath) lines'
  where
    findUsage :: Text -> Text -> FilePath -> (Int, Text) -> Maybe Diagnostic
    findUsage name msgText fp (lineNum, lineText)
      | hasWordBoundary name lineText = Just Diagnostic
          { diagSpan = mkSrcSpanRaw fp lineNum 1 lineNum (T.length lineText + 1)
          , diagSeverity = Error
          , diagKind = SecurityIssue
          , diagMessage = msgText
          , diagCode = Just "restriction/function"
          , diagFixes = []
          , diagRelated = []
          }
      | otherwise = Nothing

-- | Check if text contains word with proper boundaries
hasWordBoundary :: Text -> Text -> Bool
hasWordBoundary word text =
  let pat = "\\b" <> T.unpack word <> "\\b"
  in T.unpack text =~ pat

checkModuleRestrictions :: [ModuleRestriction] -> FilePath -> Text -> Text -> [Diagnostic]
checkModuleRestrictions restrictions filepath moduleName content =
  concatMap (checkModRestriction filepath moduleName content) restrictions

checkModRestriction :: FilePath -> Text -> Text -> ModuleRestriction -> [Diagnostic]
checkModRestriction filepath moduleName content ModuleRestriction{..} =
  let lines' = zip [1..] (T.lines content)
  in concatMap (checkImportLine filepath moduleName mrNames mrAs mrWithin mrMessage) lines'

checkImportLine :: FilePath -> Text -> [Text] -> Maybe Text -> [Text] -> Maybe Text -> (Int, Text) -> [Diagnostic]
checkImportLine filepath currentModule modules requiredAs withinModules mMessage (lineNum, lineText)
  | "import" `T.isPrefixOf` T.stripStart lineText =
      mapMaybe (checkModule filepath currentModule lineNum lineText requiredAs withinModules mMessage) modules
  | otherwise = []

checkModule :: FilePath -> Text -> Int -> Text -> Maybe Text -> [Text] -> Maybe Text -> Text -> Maybe Diagnostic
checkModule filepath currentModule lineNum lineText requiredAs withinModules mMessage modName
  -- Check if module is even in this import line
  | not (modName `T.isInfixOf` lineText) = Nothing
  -- Check within restriction (empty list means banned everywhere)
  | not (null withinModules) && not (matchesWithin withinModules currentModule) =
      Just $ mkBannedModuleDiag filepath lineNum lineText modName mMessage
  -- Check required alias
  | Just qualifier <- requiredAs =
      if isProperlyQualified lineText modName qualifier
        then Nothing
        else Just $ mkModuleDiag filepath lineNum lineText modName qualifier mMessage
  | otherwise = Nothing

-- | Check if an import line has the module properly qualified with the required alias
isProperlyQualified :: Text -> Text -> Text -> Bool
isProperlyQualified lineText _modName qualifier =
  -- Must have "qualified" keyword and correct "as" alias
  ("qualified" `T.isInfixOf` lineText) && hasCorrectAlias lineText qualifier
  where
    hasCorrectAlias t q = (" as " <> q <> " ") `T.isInfixOf` (t <> " ")
                       || (" as " <> q) `T.isSuffixOf` t

-- | Generate diagnostic for module that needs qualified import
mkModuleDiag :: FilePath -> Int -> Text -> Text -> Text -> Maybe Text -> Diagnostic
mkModuleDiag filepath lineNum lineText modName qualifier mMessage = Diagnostic
  { diagSpan = mkSrcSpanRaw filepath lineNum 1 lineNum (T.length lineText + 1)
  , diagSeverity = Warning
  , diagKind = ImportStyle
  , diagMessage = fromMaybe defaultMsg mMessage
  , diagCode = Just "restriction/module-alias"
  , diagFixes = [mkQualifiedImportFix filepath lineNum lineText modName qualifier]
  , diagRelated = []
  }
  where
    defaultMsg = "Module '" <> modName <> "' should be imported qualified as '" <> qualifier <> "'"

-- | Generate diagnostic for banned module
mkBannedModuleDiag :: FilePath -> Int -> Text -> Text -> Maybe Text -> Diagnostic
mkBannedModuleDiag filepath lineNum lineText modName mMessage = Diagnostic
  { diagSpan = mkSrcSpanRaw filepath lineNum 1 lineNum (T.length lineText + 1)
  , diagSeverity = Error
  , diagKind = ImportStyle
  , diagMessage = fromMaybe defaultMsg mMessage
  , diagCode = Just "restriction/module-banned"
  , diagFixes = []  -- No fix for banned modules
  , diagRelated = []
  }
  where
    defaultMsg = "Module '" <> modName <> "' is not allowed in this module"

-- | Generate a fix that transforms an import to qualified with alias
-- Example: "import Data.Map" -> "import qualified Data.Map as Map"
mkQualifiedImportFix :: FilePath -> Int -> Text -> Text -> Text -> Fix
mkQualifiedImportFix filepath lineNum lineText modName qualifier =
  Fix
    { fixTitle = "Import qualified as " <> qualifier
    , fixEdits = [FixEdit srcSpan replacement]
    , fixIsPreferred = True
    , fixAddImports = []
    , fixRemoveImports = []
    , fixCategory = FCImports
    , fixSafety = AT.FSAlways
    }
  where
    srcSpan = mkSrcSpanRaw filepath lineNum 1 lineNum (T.length lineText + 1)
    replacement = transformToQualifiedImport lineText modName qualifier

-- | Transform an import line to qualified form
-- Handles various import forms:
--   "import Data.Map" -> "import qualified Data.Map as Map"
--   "import Data.Map (lookup)" -> "import qualified Data.Map as Map"
--   "import qualified Data.Map" -> "import qualified Data.Map as Map"
--   "import qualified Data.Map as M" -> "import qualified Data.Map as Map"
transformToQualifiedImport :: Text -> Text -> Text -> Text
transformToQualifiedImport lineText _modName qualifier
  | "qualified" `T.isInfixOf` lineText =
      -- Already qualified, just fix the alias
      let (beforeAs, _) = T.breakOn " as " lineText
          hasAs = " as " `T.isInfixOf` lineText
      in if hasAs
           then T.stripEnd beforeAs <> " as " <> qualifier
           else T.stripEnd lineText <> " as " <> qualifier
  | otherwise =
      -- Need to add qualified keyword and alias
      let stripped = T.stripEnd lineText
          -- Remove any import list like "(lookup, insert)"
          withoutList = T.stripEnd $ T.takeWhile (/= '(') stripped
          -- Build new import: "import qualified Module as Alias"
          parts = T.words withoutList
      in case parts of
           ("import" : rest) -> "import qualified " <> T.unwords rest <> " as " <> qualifier
           _ -> stripped <> " -- could not transform"

checkExtensionRestrictions :: [ExtensionRestriction] -> FilePath -> Text -> Text -> [Diagnostic]
checkExtensionRestrictions restrictions filepath moduleName content =
  concatMap (checkExtRestriction filepath moduleName content) restrictions

checkExtRestriction :: FilePath -> Text -> Text -> ExtensionRestriction -> [Diagnostic]
checkExtRestriction filepath moduleName content ExtensionRestriction{..}
  | matchesWithin erWithin moduleName = []
  | otherwise = findExtensionUsages filepath erName erMessage content

findExtensionUsages :: FilePath -> Text -> Maybe Text -> Text -> [Diagnostic]
findExtensionUsages filepath extName mMessage content =
  let lines' = zip [1..] (T.lines content)
      msg = fromMaybe ("Extension '" <> extName <> "' is not allowed in this module") mMessage
  in mapMaybe (findExt extName msg filepath) lines'
  where
    findExt :: Text -> Text -> FilePath -> (Int, Text) -> Maybe Diagnostic
    findExt name msgText fp (lineNum, lineText)
      | "LANGUAGE" `T.isInfixOf` lineText && name `T.isInfixOf` lineText = Just Diagnostic
          { diagSpan = mkSrcSpanRaw fp lineNum 1 lineNum (T.length lineText + 1)
          , diagSeverity = Warning
          , diagKind = Custom "extension-restriction"
          , diagMessage = msgText
          , diagCode = Just "restriction/extension"
          , diagFixes = []
          , diagRelated = []
          }
      | otherwise = Nothing

--------------------------------------------------------------------------------
-- Default Rules Library
--------------------------------------------------------------------------------

-- | Complete default rules library
defaultRulesLibrary :: [ConfigurableRule]
defaultRulesLibrary = concat
  [ performanceRules
  , securityRules
  , modernizeRules
  , redundantRules
  , spaceLeakRules
  , partialRules
  ]

-- | Performance rules
performanceRules :: [ConfigurableRule]
performanceRules =
  [ mkRule "performance/length-null" Performance
      "length $X == 0" (Just "null $X")
      "Use null for O(1) emptiness check instead of length"
      Safe

  , mkRule "performance/length-gt-0" Performance
      "length $X > 0" (Just "not (null $X)")
      "Use not . null instead of length > 0"
      Safe

  , mkRule "performance/concat-map" Performance
      "concat $ map $F $X" (Just "concatMap $F $X")
      "Use concatMap instead of concat . map"
      Safe

  , mkRule "performance/concat-map-compose" Performance
      "concat . map $F" (Just "concatMap $F")
      "Use concatMap instead of concat . map"
      Safe

  , mkRule "performance/mconcat-map" Performance
      "mconcat $ map $F $X" (Just "foldMap $F $X")
      "Use foldMap instead of mconcat . map"
      Safe

  , mkRule "performance/head-sort" Performance
      "head $ sort $X" (Just "minimum $X")
      "Use minimum instead of head . sort"
      Safe

  , mkRule "performance/last-sort" Performance
      "last $ sort $X" (Just "maximum $X")
      "Use maximum instead of last . sort"
      Safe

  , mkRule "performance/nub" Performance
      "nub $X" (Just "ordNub $X")
      "nub is O(n^2), use ordNub or nubOrd for O(n log n)"
      Unsafe

  , mkRule "performance/foldr-concat" Performance
      "foldr (++) []" (Just "concat")
      "Use concat instead of foldr (++) []"
      Safe

  , mkRule "performance/map-id" Performance
      "map id $X" (Just "$X")
      "map id is redundant"
      Safe

    -- IORef strict variants
  , mkRule "performance/modifyIORef-strict" Performance
      "modifyIORef $REF $F" (Just "modifyIORef' $REF $F")
      "modifyIORef can accumulate thunks. Use modifyIORef' for strict updates"
      Safe

  , mkRule "performance/atomicModifyIORef-strict" Performance
      "atomicModifyIORef $REF $F" (Just "atomicModifyIORef' $REF $F")
      "atomicModifyIORef can accumulate thunks. Use atomicModifyIORef' for strict updates"
      Safe

    -- MVar strict variants
  , mkRule "performance/modifyMVar-strict" Performance
      "modifyMVar $MV $F" (Just "modifyMVar' $MV $F")
      "modifyMVar can accumulate thunks. Use modifyMVar' for strict updates"
      Safe

  , mkRule "performance/modifyMVar_-strict" Performance
      "modifyMVar_ $MV $F" (Just "modifyMVar_' $MV $F")
      "modifyMVar_ can accumulate thunks. Use modifyMVar_' for strict updates"
      Safe

    -- STRef strict variant
  , mkRule "performance/modifySTRef-strict" Performance
      "modifySTRef $REF $F" (Just "modifySTRef' $REF $F")
      "modifySTRef can accumulate thunks. Use modifySTRef' for strict updates"
      Safe

    -- TVar strict variants
  , mkRule "performance/modifyTVar-strict" Performance
      "modifyTVar $TV $F" (Just "modifyTVar' $TV $F")
      "modifyTVar can accumulate thunks. Use modifyTVar' for strict updates"
      Safe

    -- Fusion patterns (eliminate intermediate data structures)
  , mkRule "performance/sum-map" Performance
      "sum $ map $F $X" (Just "foldl' (\\acc x -> acc + $F x) 0 $X")
      "Fuse sum and map to avoid intermediate list allocation"
      Safe

  , mkRule "performance/product-map" Performance
      "product $ map $F $X" (Just "foldl' (\\acc x -> acc * $F x) 1 $X")
      "Fuse product and map to avoid intermediate list allocation"
      Safe

  , mkRule "performance/any-map" Performance
      "any $P $ map $F $X" (Just "any (\\x -> $P ($F x)) $X")
      "Fuse any and map to avoid intermediate list"
      Safe

  , mkRule "performance/all-map" Performance
      "all $P $ map $F $X" (Just "all (\\x -> $P ($F x)) $X")
      "Fuse all and map to avoid intermediate list"
      Safe

  , mkRule "performance/any-id" Performance
      "any id $X" (Just "or $X")
      "any id is equivalent to or"
      Safe

  , mkRule "performance/all-id" Performance
      "all id $X" (Just "and $X")
      "all id is equivalent to and"
      Safe

  , mkRule "performance/filter-filter" Performance
      "filter $P $ filter $Q $X" (Just "filter (\\x -> $Q x && $P x) $X")
      "Fuse consecutive filters to avoid intermediate list"
      Safe

  , mkRule "performance/map-map" Performance
      "map $F $ map $G $X" (Just "map ($F . $G) $X")
      "Fuse consecutive maps to avoid intermediate list"
      Safe

    -- Text/String conversion optimizations
  , mkRule "performance/pack-unpack" Performance
      "T.pack $ T.unpack $X" (Just "$X")
      "pack . unpack is identity for Text"
      Safe

  , mkRule "performance/unpack-pack" Performance
      "T.unpack $ T.pack $X" (Just "$X")
      "unpack . pack is identity for String"
      Safe

    -- List conversion optimizations
  , mkRule "performance/toList-fromList" Performance
      "toList $ fromList $X" (Just "$X")
      "toList . fromList is identity"
      Safe

  , mkRule "performance/fromList-toList" Performance
      "fromList $ toList $X" (Just "$X")
      "fromList . toList is identity"
      Safe

    -- Redundant operations
  , mkRule "performance/reverse-reverse" Performance
      "reverse $ reverse $X" (Just "$X")
      "reverse . reverse is identity"
      Safe

  , mkRule "performance/sort-sort" Performance
      "sort $ sort $X" (Just "sort $X")
      "sort . sort is just sort (idempotent)"
      Safe

  , mkRule "performance/nub-nub" Performance
      "nub $ nub $X" (Just "nub $X")
      "nub . nub is just nub (idempotent)"
      Safe

    -- Common algorithmic improvements
  , mkRule "performance/elem-set" Performance
      "elem $X $ Set.toList $S" (Just "Set.member $X $S")
      "Use Set.member for O(log n) lookup instead of O(n) elem"
      Safe

  , mkRule "performance/notElem-set" Performance
      "notElem $X $ Set.toList $S" (Just "not (Set.member $X $S)")
      "Use Set.member for O(log n) lookup"
      Safe

  , mkRule "performance/length-set" Performance
      "length $ Set.toList $S" (Just "Set.size $S")
      "Use Set.size for O(1) size instead of O(n) length"
      Safe

  , mkRule "performance/null-set" Performance
      "null $ Set.toList $S" (Just "Set.null $S")
      "Use Set.null for O(1) check"
      Safe
  ]

-- | Security rules
securityRules :: [ConfigurableRule]
securityRules =
  -- Debug trace removal (safe to remove, just removes logging)
  [ mkRule "security/trace" Security
      "trace $MSG $X" (Just "$X")
      "Debug trace found - remove before production"
      Safe  -- Safe: trace x y = y, just removes debug output

  , mkRule "security/traceShow" Security
      "traceShow $VAL $X" (Just "$X")
      "Debug traceShow found - remove before production"
      Safe  -- Safe: traceShow v x = x, just removes debug output

  , mkRule "security/traceShowId" Security
      "traceShowId $X" (Just "$X")
      "Debug traceShowId found - remove before production"
      Safe  -- Safe: traceShowId x = x, identity with side effect

  , mkRule "security/traceM" Security
      "traceM $MSG" (Just "pure ()")
      "Debug traceM found - remove before production"
      Safe  -- Safe: replaces debug print with no-op

  , mkRule "security/traceShowM" Security
      "traceShowM $X" (Just "pure ()")
      "Debug traceShowM found - remove before production"
      Safe

  , mkRule "security/traceId" Security
      "traceId $X" (Just "$X")
      "Debug traceId found - remove before production"
      Safe  -- Safe: traceId x = x

  , mkRule "security/traceStack" Security
      "traceStack $MSG $X" (Just "$X")
      "Debug traceStack found - remove before production"
      Safe

    -- Unsafe operations (need manual review)
  , mkRule "security/unsafePerformIO" Security
      "unsafePerformIO" Nothing
      "unsafePerformIO breaks referential transparency"
      Manual

  , mkRule "security/unsafeCoerce" Security
      "unsafeCoerce" Nothing
      "unsafeCoerce can cause memory corruption"
      Manual

  , mkRule "security/undefined" Security
      "undefined" Nothing
      "undefined will crash at runtime"
      Manual

  , mkRule "security/error-call" Security
      "error $X" Nothing
      "error will crash at runtime - consider proper error handling"
      Manual

    -- Interop safety
  , mkRule "security/unsafeDupablePerformIO" Security
      "unsafeDupablePerformIO" Nothing
      "unsafeDupablePerformIO can duplicate effects"
      Manual

  , mkRule "security/unsafeInterleaveIO" Security
      "unsafeInterleaveIO" Nothing
      "unsafeInterleaveIO breaks IO ordering guarantees"
      Manual
  ]

-- | Modernization rules
modernizeRules :: [ConfigurableRule]
modernizeRules =
  [ mkRule "modernize/return-pure" Modernize
      "return $X" (Just "pure $X")
      "Use pure instead of return (Applicative is more general)"
      Safe

  , mkRule "modernize/liftM-fmap" Modernize
      "liftM $F $X" (Just "fmap $F $X")
      "Use fmap instead of liftM"
      Safe

  , mkRule "modernize/liftM2-liftA2" Modernize
      "liftM2 $F $X $Y" (Just "liftA2 $F $X $Y")
      "Use liftA2 instead of liftM2"
      Safe

  , mkRule "modernize/mappend" Modernize
      "mappend $X $Y" (Just "$X <> $Y")
      "Use (<>) instead of mappend (Semigroup)"
      Safe

  , mkRule "modernize/mapM-traverse" Modernize
      "mapM $F $X" (Just "traverse $F $X")
      "Use traverse instead of mapM"
      Safe

  , mkRule "modernize/mapM_-traverse_" Modernize
      "mapM_ $F $X" (Just "traverse_ $F $X")
      "Use traverse_ instead of mapM_"
      Safe

  , mkRule "modernize/forM-for" Modernize
      "forM $X $F" (Just "for $X $F")
      "Use for instead of forM"
      Safe

  , mkRule "modernize/forM_-for_" Modernize
      "forM_ $X $F" (Just "for_ $X $F")
      "Use for_ instead of forM_"
      Safe

  , mkRule "modernize/sequence-sequenceA" Modernize
      "sequence $X" (Just "sequenceA $X")
      "Use sequenceA instead of sequence"
      Safe

  , mkRule "modernize/foldMap-id" Modernize
      "foldMap id $X" (Just "fold $X")
      "Use fold instead of foldMap id"
      Safe
  ]

-- | Redundant code rules
redundantRules :: [ConfigurableRule]
redundantRules =
  [ mkRule "redundant/id" Redundant
      "id $X" (Just "$X")
      "Redundant use of id"
      Safe

  , mkRule "redundant/const-id" Redundant
      "const id" (Just "const id")  -- Can't really fix this
      "const id is equivalent to flip const"
      Manual

  , mkRule "redundant/double-not" Redundant
      "not $ not $X" (Just "$X")
      "Double negation is redundant"
      Safe

  , mkRule "redundant/double-reverse" Redundant
      "reverse $ reverse $X" (Just "$X")
      "Double reverse is redundant"
      Safe

  , mkRule "redundant/if-then-else-bool" Redundant
      "if $X then True else False" (Just "$X")
      "Redundant if-then-else, use the condition directly"
      Safe

  , mkRule "redundant/if-then-else-bool-not" Redundant
      "if $X then False else True" (Just "not $X")
      "Redundant if-then-else, use not"
      Safe

  , mkRule "redundant/fromMaybe-Nothing" Redundant
      "fromMaybe undefined $X" Nothing
      "fromMaybe undefined defeats the purpose of Maybe"
      Manual

  , mkRule "redundant/maybe-Just-id" Redundant
      "maybe Nothing Just $X" (Just "$X")
      "maybe Nothing Just is equivalent to id for Maybe"
      Safe

  , mkRule "redundant/either-Left-Right" Redundant
      "either Left Right $X" (Just "$X")
      "either Left Right is equivalent to id for Either"
      Safe
  ]

-- | Space leak rules
spaceLeakRules :: [ConfigurableRule]
spaceLeakRules =
  [ mkRule "space-leaks/foldl" SpaceLeaks
      "foldl $F $Z $X" (Just "foldl' $F $Z $X")
      "Use foldl' to avoid space leaks"
      Safe  -- Safe: foldl' is drop-in replacement, always more memory efficient

  , mkRule "space-leaks/foldr-strict" SpaceLeaks
      "foldr $F $Z $X" Nothing
      "Consider using foldl' for strict left folds or foldr' for strict right folds"
      Manual

    -- NOTE: Import-based space leak rules (lazy-state, lazy-writer, lazy-map,
    -- lazy-intmap, lazy-rws, lazy-hashmap) removed because simple pattern matching
    -- cannot distinguish "import Data.Map" from "import Data.Map.Strict", causing
    -- infinite appending of ".Strict". These need AST-based import analysis.

    -- Monadic folds (still works - not import-based)
  , mkRule "space-leaks/foldM" SpaceLeaks
      "foldM $F $Z $X" (Just "foldM' $F $Z $X")
      "foldM can accumulate thunks. Use foldM' from Control.Monad for strict monadic folds"
      Safe

    -- Lazy IO (suggest strict alternatives)
  , mkRule "space-leaks/lazy-readFile" SpaceLeaks
      "readFile $F" (Just "TIO.readFile $F")
      "readFile is lazy. Use Data.Text.IO.readFile for strict reading"
      Manual  -- Requires import and type change

  , mkRule "space-leaks/lazy-getContents" SpaceLeaks
      "getContents" (Just "TIO.getContents")
      "getContents is lazy. Use Data.Text.IO.getContents for strict reading"
      Manual

  , mkRule "space-leaks/lazy-hGetContents" SpaceLeaks
      "hGetContents $H" (Just "TIO.hGetContents $H")
      "hGetContents is lazy. Use Data.Text.IO.hGetContents for strict reading"
      Manual

    -- Accumulator patterns
  , mkRule "space-leaks/concat-append" SpaceLeaks
      "foldl (++) [] $X" (Just "concat $X")
      "Use concat instead of foldl (++) [] to avoid quadratic behavior"
      Safe

  , mkRule "space-leaks/foldr-append" SpaceLeaks
      "foldr (++) [] $X" (Just "concat $X")
      "Use concat instead of foldr (++) []"
      Safe
  ]

-- | Partial function rules
-- Safe alternatives use functions that don't crash on empty input
partialRules :: [ConfigurableRule]
partialRules =
  -- Safe alternatives (always succeed, no crash possible)
  [ mkRule "partial/tail-to-drop" Partial
      "tail $X" (Just "drop 1 $X")
      "tail crashes on empty list. Use drop 1 which returns [] for empty input"
      Safe  -- drop 1 [] = [], always safe!

  , mkRule "partial/init-to-dropEnd" Partial
      "init $X" (Just "dropEnd 1 $X")
      "init crashes on empty list. Use dropEnd 1 which returns [] for empty input"
      Safe  -- dropEnd 1 [] = [], always safe!

    -- Idiom replacements (mathematically equivalent, always safe)
  , mkRule "partial/head-reverse" Partial
      "head (reverse $X)" (Just "last $X")
      "head (reverse xs) is equivalent to last xs"
      Safe

  , mkRule "partial/last-reverse" Partial
      "last (reverse $X)" (Just "head $X")
      "last (reverse xs) is equivalent to head xs"
      Safe

  , mkRule "partial/head-sort" Partial
      "head (sort $X)" (Just "minimum $X")
      "head (sort xs) is minimum xs with better performance"
      Safe  -- Both partial, but minimum is O(n) vs O(n log n)

  , mkRule "partial/last-sort" Partial
      "last (sort $X)" (Just "maximum $X")
      "last (sort xs) is maximum xs with better performance"
      Safe  -- Both partial, but maximum is O(n) vs O(n log n)

    -- Safe package alternatives (require import but are correct)
  , mkRule "partial/head" Partial
      "head $X" (Just "headMay $X")
      "head crashes on empty list. Use headMay from safe package or pattern match"
      Manual  -- Manual because requires import Safe (headMay)

  , mkRule "partial/last" Partial
      "last $X" (Just "lastMay $X")
      "last crashes on empty list. Use lastMay from safe package or pattern match"
      Manual  -- Manual because requires import Safe (lastMay)

  , mkRule "partial/fromJust" Partial
      "fromJust $X" (Just "fromMaybe (error \"fromJust Nothing\") $X")
      "fromJust crashes on Nothing. Use fromMaybe with explicit error or pattern match"
      Manual

  , mkRule "partial/read" Partial
      "read $X" (Just "readMaybe $X")
      "read crashes on invalid input. Use readMaybe from Text.Read"
      Manual  -- Manual because requires import Text.Read (readMaybe)

  , mkRule "partial/minimum" Partial
      "minimum $X" (Just "minimumMay $X")
      "minimum crashes on empty list. Use minimumMay from safe package"
      Manual

  , mkRule "partial/maximum" Partial
      "maximum $X" (Just "maximumMay $X")
      "maximum crashes on empty list. Use maximumMay from safe package"
      Manual

  , mkRule "partial/foldr1" Partial
      "foldr1 $F $X" Nothing
      "foldr1 crashes on empty list. Use foldr with explicit base case"
      Manual

  , mkRule "partial/foldl1" Partial
      "foldl1 $F $X" Nothing
      "foldl1 crashes on empty list. Use foldl' with explicit base case"
      Manual

  , mkRule "partial/bang-index" Partial
      "$X !! $N" Nothing
      "(!!) crashes on out-of-bounds. Consider atMay from safe package or bounds check"
      Manual

    -- cycle and repeat are partial in some contexts
  , mkRule "partial/cycle-empty" Partial
      "cycle []" Nothing
      "cycle [] crashes. Ensure list is non-empty or use guard"
      Manual
  ]

-- | Helper to create a rule
mkRule :: Text -> RuleCategory -> Text -> Maybe Text -> Text -> FixSafety -> ConfigurableRule
mkRule ruleId cat pat fix msg safety = ConfigurableRule
  { crId = ruleId
  , crCategory = cat
  , crPattern = ConfigPattern pat fix Nothing msg
  , crSeverity = categoryToSeverity cat
  , crSafety = safety
  , crEnabled = True
  , crWithin = []
  , crExcept = []
  , crNote = Nothing
  -- New fields (TODO-004 through TODO-007)
  , crAddImports = []
  , crRemoveImports = []
  , crTarget = Nothing
  , crTags = []
  , crReferences = []
  , crExplanation = Nothing
  -- Fix ordering (TODO-010)
  , crDependsOn = []
  , crConflictsWith = []
  -- Rule versioning (TODO-011)
  , crVersion = Nothing
  , crDeprecated = Nothing
  -- Module context (TODO-012)
  , crFromModule = Nothing
  , crToModule = Nothing
  }

-- | Helper to create a rule with import management (TODO-004)
mkRuleWithImports
  :: Text                    -- ^ Rule ID
  -> RuleCategory            -- ^ Category
  -> Text                    -- ^ Pattern
  -> Maybe Text              -- ^ Fix pattern
  -> Text                    -- ^ Message
  -> FixSafety               -- ^ Safety level
  -> [RT.ImportSpec]         -- ^ Imports to add
  -> [Text]                  -- ^ Imports to remove
  -> ConfigurableRule
mkRuleWithImports ruleId cat pat fix msg safety addImps remImps =
  (mkRule ruleId cat pat fix msg safety)
    { crAddImports = addImps
    , crRemoveImports = remImps
    }

-- | Helper to create a rule with tags and references (TODO-007)
mkRuleWithMetadata
  :: Text                    -- ^ Rule ID
  -> RuleCategory            -- ^ Category
  -> Text                    -- ^ Pattern
  -> Maybe Text              -- ^ Fix pattern
  -> Text                    -- ^ Message
  -> FixSafety               -- ^ Safety level
  -> [Text]                  -- ^ Tags
  -> [Text]                  -- ^ Reference URLs
  -> Maybe Text              -- ^ Detailed explanation
  -> ConfigurableRule
mkRuleWithMetadata ruleId cat pat fix msg safety tags refs explanation =
  (mkRule ruleId cat pat fix msg safety)
    { crTags = tags
    , crReferences = refs
    , crExplanation = explanation
    }

-- | Helper to create a rule with module context (TODO-012)
mkRuleWithModuleContext
  :: Text                    -- ^ Rule ID
  -> RuleCategory            -- ^ Category
  -> Text                    -- ^ Pattern
  -> Maybe Text              -- ^ Fix pattern
  -> Text                    -- ^ Message
  -> FixSafety               -- ^ Safety level
  -> Maybe Text              -- ^ Source module (from-module)
  -> Maybe Text              -- ^ Target module (to-module)
  -> ConfigurableRule
mkRuleWithModuleContext ruleId cat pat fix msg safety fromMod toMod =
  (mkRule ruleId cat pat fix msg safety)
    { crFromModule = fromMod
    , crToModule = toMod
    }

-- | Full rule builder record for comprehensive rule creation (TODO-012)
-- This allows setting all TOML-supported fields in a single record.
data FullRuleBuilder = FullRuleBuilder
  { frbId             :: Text
  , frbCategory       :: RuleCategory
  , frbPattern        :: Text
  , frbFix            :: Maybe Text
  , frbWhere          :: Maybe Text
  , frbMessage        :: Text
  , frbSeverity       :: Severity
  , frbSafety         :: FixSafety
  , frbEnabled        :: Bool
  , frbWithin         :: [Text]
  , frbExcept         :: [Text]
  , frbNote           :: Maybe Text
  , frbAddImports     :: [RT.ImportSpec]
  , frbRemoveImports  :: [Text]
  , frbTarget         :: Maybe RT.RuleTarget
  , frbTags           :: [Text]
  , frbReferences     :: [Text]
  , frbExplanation    :: Maybe Text
  , frbDependsOn      :: [Text]
  , frbConflictsWith  :: [Text]
  , frbVersion        :: Maybe Version
  , frbDeprecated     :: Maybe DeprecationInfo
  , frbFromModule     :: Maybe Text
  , frbToModule       :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

-- | Default full rule builder with minimal required fields
defaultFullRuleBuilder :: Text -> RuleCategory -> Text -> Text -> FullRuleBuilder
defaultFullRuleBuilder ruleId cat pat msg = FullRuleBuilder
  { frbId = ruleId
  , frbCategory = cat
  , frbPattern = pat
  , frbFix = Nothing
  , frbWhere = Nothing
  , frbMessage = msg
  , frbSeverity = categoryToSeverity cat
  , frbSafety = Safe
  , frbEnabled = True
  , frbWithin = []
  , frbExcept = []
  , frbNote = Nothing
  , frbAddImports = []
  , frbRemoveImports = []
  , frbTarget = Nothing
  , frbTags = []
  , frbReferences = []
  , frbExplanation = Nothing
  , frbDependsOn = []
  , frbConflictsWith = []
  , frbVersion = Nothing
  , frbDeprecated = Nothing
  , frbFromModule = Nothing
  , frbToModule = Nothing
  }

-- | Build a ConfigurableRule from a FullRuleBuilder
buildFullRule :: FullRuleBuilder -> ConfigurableRule
buildFullRule FullRuleBuilder{..} = ConfigurableRule
  { crId = frbId
  , crCategory = frbCategory
  , crPattern = ConfigPattern frbPattern frbFix frbWhere frbMessage
  , crSeverity = frbSeverity
  , crSafety = frbSafety
  , crEnabled = frbEnabled
  , crWithin = frbWithin
  , crExcept = frbExcept
  , crNote = frbNote
  , crAddImports = frbAddImports
  , crRemoveImports = frbRemoveImports
  , crTarget = frbTarget
  , crTags = frbTags
  , crReferences = frbReferences
  , crExplanation = frbExplanation
  , crDependsOn = frbDependsOn
  , crConflictsWith = frbConflictsWith
  , crVersion = frbVersion
  , crDeprecated = frbDeprecated
  , crFromModule = frbFromModule
  , crToModule = frbToModule
  }

-- | Check if a ConfigurableRule has module context specified
hasModuleContext :: ConfigurableRule -> Bool
hasModuleContext rule = isJust (crFromModule rule) || isJust (crToModule rule)

-- | Get module context as a tuple
getModuleContext :: ConfigurableRule -> (Maybe Text, Maybe Text)
getModuleContext rule = (crFromModule rule, crToModule rule)

-- | Set module context on an existing rule
setModuleContext :: Maybe Text -> Maybe Text -> ConfigurableRule -> ConfigurableRule
setModuleContext fromMod toMod rule = rule
  { crFromModule = fromMod
  , crToModule = toMod
  }

-- | Default severity for a category
categoryToSeverity :: RuleCategory -> Severity
categoryToSeverity = \case
  RT.Performance -> Warning
  RT.SpaceLeaks -> Warning
  RT.Security -> Error
  RT.Modernization -> Suggestion
  RT.Redundant -> Warning
  RT.Safety -> Warning
  RT.Imports -> Suggestion
  RT.Naming -> Suggestion
  RT.Extensions -> Info
  RT.Complexity -> Warning
  RT.Concurrency -> Warning
  RT.ErrorHandling -> Warning
  RT.Documentation -> Suggestion
  RT.Custom _ -> Warning
  RT.Style -> Suggestion
  RT.Correctness -> Error

--------------------------------------------------------------------------------
-- AST-Based Rule Integration
--------------------------------------------------------------------------------

-- | Parse side conditions from a ConfigPattern's 'where' field
-- The where field can contain one or more conditions separated by semicolons or commas.
-- Each condition is parsed using the ASTMatch side condition parser.
-- Multiple conditions are combined with @And@.
--
-- Example 'where' values:
--   "isLiteral $X"
--   "isVar $X; isNumeric $X"
--   "notEq $X $Y, isPure $X"
parseSideConditionFromConfig :: ConfigPattern -> Maybe AST.SideCondition
parseSideConditionFromConfig configPat = case cpWhere configPat of
  Nothing -> Nothing
  Just whereText ->
    let trimmed = T.strip whereText
    in if T.null trimmed
       then Nothing
       else parseConditions trimmed
  where
    -- Parse multiple conditions separated by ';' or ','
    parseConditions :: Text -> Maybe AST.SideCondition
    parseConditions text =
      let -- Split by semicolons and commas
          parts = concatMap (T.splitOn ",") (T.splitOn ";" text)
          -- Strip whitespace and filter empty
          cleaned = filter (not . T.null) $ map T.strip parts
          -- Parse each condition
          parsed = mapMaybe AST.parseSideCondition cleaned
      in case parsed of
           [] -> Nothing
           [single] -> Just single
           multiple -> Just $ AST.And multiple  -- Unified And takes a list

-- | Convert ConfigurableRules to unified Rule type
-- This creates Rule values with ASTPatternSpec patterns
convertToASTRules :: [ConfigurableRule] -> IO [RT.Rule]
convertToASTRules rules = pure $ map convertRule rules
  where
    convertRule :: ConfigurableRule -> RT.Rule
    convertRule rule = RT.Rule
      { RT.ruleId = crId rule
      , RT.ruleCategory = ruleCategoryToUnified (crCategory rule)
      , RT.ruleSeverity = crSeverity rule
      , RT.ruleMessage = cpMessage (crPattern rule)
      -- Use crExplanation if available, fall back to crNote (TODO-006)
      , RT.ruleExplanation = crExplanation rule <|> crNote rule
      , RT.rulePattern = RT.ASTPatternSpec (cpPattern (crPattern rule))
      , RT.ruleReplacement = cpFix (crPattern rule)
      , RT.ruleConditions = case parseSideConditionFromConfig (crPattern rule) of
          Nothing -> []
          Just sc -> [sc]
      , RT.ruleSafety = fixSafetyToUnified (crSafety rule)
      -- Use crAddImports directly (TODO-004)
      , RT.ruleAddImports = crAddImports rule
      , RT.ruleRemoveImports = crRemoveImports rule
      , RT.ruleEnabled = crEnabled rule
      , RT.ruleWithin = crWithin rule
      , RT.ruleExcept = crExcept rule
      , RT.ruleDeprecated = Nothing
      -- Use tags from rule (TODO-007)
      , RT.ruleTags = crTags rule
      , RT.ruleReferences = crReferences rule
      , RT.ruleFixDescription = Nothing
      , RT.ruleNote = crNote rule
      , RT.ruleSourceModule = Nothing
      , RT.ruleTargetModule = Nothing
      -- Use target from rule (TODO-005)
      , RT.ruleTarget = crTarget rule
      }

-- | Convert old RuleCategory to unified Category
-- Note: Since RuleCategory = RT.Category, this is identity
ruleCategoryToUnified :: RuleCategory -> RT.Category
ruleCategoryToUnified = id

-- | Convert old FixSafety to unified SafetyLevel
-- Note: Since FixSafety = RT.SafetyLevel, this is identity
fixSafetyToUnified :: FixSafety -> RT.SafetyLevel
fixSafetyToUnified = id

-- | Apply AST-based configurable rules to a parsed module
-- This provides more accurate matching than text-based patterns
applyASTConfigurableRules :: RulesConfig -> FilePath -> Text -> AST.HsModule AST.GhcPs -> IO [Diagnostic]
applyASTConfigurableRules cfg filepath moduleName hsmod = do
  let allRules = getAllRules cfg
      enabledRules = filter (\r -> isRuleEnabled cfg r moduleName) allRules

  -- Convert to unified Rule type
  astRules <- convertToASTRules enabledRules

  -- Apply AST rules (returns IO [Diagnostic])
  AST.applyASTRules astRules filepath moduleName hsmod

-- | Load rules from config and apply AST-based matching
-- This is the recommended way to apply configurable rules for accurate results
loadAndApplyASTRules :: RulesConfig -> FilePath -> Text -> AST.HsModule AST.GhcPs -> IO [Diagnostic]
loadAndApplyASTRules = applyASTConfigurableRules

--------------------------------------------------------------------------------
-- Type Conversion Helpers
--------------------------------------------------------------------------------

-- | Convert RuleCategory to FixCategory
ruleCategoryToFixCategory :: RuleCategory -> FixCategory
ruleCategoryToFixCategory = \case
  RT.Performance -> FCPerformance
  RT.SpaceLeaks -> FCSpaceLeaks
  RT.Security -> FCSecurity
  RT.Modernization -> FCModernize
  RT.Redundant -> FCRedundant
  RT.Safety -> FCSafety
  RT.Imports -> FCImports
  RT.Naming -> FCStyle
  RT.Extensions -> FCStyle
  RT.Complexity -> FCStyle
  RT.Concurrency -> FCSafety
  RT.ErrorHandling -> FCSafety
  RT.Documentation -> FCStyle
  RT.Custom t -> FCCustom t
  RT.Style -> FCStyle
  RT.Correctness -> FCSafety

-- | Convert rule-level safety (RuleTypes) to fix-level safety (Types)
ruleSafetyToTypeSafety :: FixSafety -> AT.FixSafety
ruleSafetyToTypeSafety RT.Safe = AT.FSAlways
ruleSafetyToTypeSafety RT.MostlySafe = AT.FSMostly
ruleSafetyToTypeSafety RT.NeedsReview = AT.FSReview
ruleSafetyToTypeSafety RT.Unsafe = AT.FSUnsafe

--------------------------------------------------------------------------------
-- Fix Dependencies (TODO-010)
--------------------------------------------------------------------------------

-- | Dependency information for a rule
--
-- This contains the rule ID along with its dependencies and conflicts,
-- which can be used by the AutoFix system when ordering fixes.
data RuleDependencyInfo = RuleDependencyInfo
  { rdiRuleId      :: Text           -- ^ Rule ID
  , rdiDependsOn   :: [Text]         -- ^ Rules that must be applied first
  , rdiConflicts   :: [Text]         -- ^ Rules that conflict (mutually exclusive)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Get dependencies for a rule as a set of rule IDs
--
-- Returns the list of rule IDs that must be applied before this rule.
getRuleDependencies :: ConfigurableRule -> [Text]
getRuleDependencies = crDependsOn

-- | Get conflicts for a rule as a set of rule IDs
--
-- Returns the list of rule IDs that conflict with this rule.
getRuleConflicts :: ConfigurableRule -> [Text]
getRuleConflicts = crConflictsWith

-- | Build a map from rule ID to dependency information
--
-- This is useful for the AutoFix system when it needs to look up
-- dependencies for multiple rules at once.
--
-- @
-- -- Build dependency map from all custom rules
-- let depMap = buildRuleDependencyMap (rcCustomRules config)
--
-- -- Look up dependencies for a specific rule
-- case Map.lookup "partial/head" depMap of
--   Just info -> rdiDependsOn info  -- ["imports/add-safe"]
--   Nothing -> []
-- @
buildRuleDependencyMap :: [ConfigurableRule] -> Map Text RuleDependencyInfo
buildRuleDependencyMap rules = Map.fromList
  [ (crId rule, mkRuleDepInfo rule)
  | rule <- rules
  ]
  where
    mkRuleDepInfo :: ConfigurableRule -> RuleDependencyInfo
    mkRuleDepInfo rule = RuleDependencyInfo
      { rdiRuleId = crId rule
      , rdiDependsOn = crDependsOn rule
      , rdiConflicts = crConflictsWith rule
      }

--------------------------------------------------------------------------------
-- Rule Deprecation Checking (TODO-011)
--------------------------------------------------------------------------------

-- | Check all rules for deprecation warnings
--
-- Returns a list of warnings for any deprecated rules that are being used.
-- This can be used to notify users during linting that they should migrate
-- to replacement rules.
--
-- @
-- -- Check for deprecated rules
-- let warnings = checkDeprecations (rcCustomRules config)
-- mapM_ printWarning warnings
-- @
checkDeprecations :: [ConfigurableRule] -> [DeprecationWarning]
checkDeprecations rules =
  [ DeprecationWarning (crId rule) info
  | rule <- rules
  , Just info <- [crDeprecated rule]
  ]

-- | Emit a deprecation warning to the specified handle
--
-- Formats and prints a deprecation warning message to stderr (or another handle).
-- The warning includes the rule ID, reason for deprecation, suggested replacement
-- (if any), and the version when the rule will be removed (if specified).
emitDeprecationWarning :: Handle -> DeprecationWarning -> IO ()
emitDeprecationWarning h (DeprecationWarning ruleId info) = do
  TIO.hPutStrLn h $ T.concat
    [ "⚠️  Warning: Rule '", ruleId, "' is deprecated."
    ]
  TIO.hPutStrLn h $ T.concat
    [ "    Reason: ", diReason info
    ]
  case diReplacement info of
    Just replacement ->
      TIO.hPutStrLn h $ T.concat
        [ "    Use '", replacement, "' instead."
        ]
    Nothing -> pure ()
  case diRemovalVersion info of
    Just version ->
      TIO.hPutStrLn h $ T.concat
        [ "    Will be removed in version ", showVersion version, "."
        ]
    Nothing -> pure ()
