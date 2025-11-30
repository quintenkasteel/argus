{-# LANGUAGE StrictData #-}
{-# LANGUAGE ApplicativeDo #-}

-- |
-- Module      : Linter.Config
-- Description : Configuration loading and parsing
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module handles configuration for the linter, supporting both
-- TOML (preferred) and YAML formats for backwards compatibility.
module Linter.Config
  ( -- * Configuration types
    Config (..)
  , GeneralConfig (..)
  , OutputConfig (..)
  , UnusedConfig (..)
  , NamingConfig (..)
  , PatternsConfig (..)
  , ImportsConfig (..)
  , FixConfig (..)

    -- * Rule definitions
  , TypeRule (..)
  , VariableRule (..)
  , PatternRule (..)
  , RuleSeverity (..)

    -- * Loading configuration
  , loadConfig
  , loadConfigFromFile
  , defaultConfig

    -- * Legacy YAML support
  , loadLegacyConfig
  , convertLegacyConfig
  , LegacyConfig (..)
  , LegacySignature (..)
  , LegacyVariable (..)
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.!=), withObject, object)
import Data.Aeson qualified as Aeson
import qualified Data.Aeson as AE ((.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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

import Linter.Types (Severity (..))

--------------------------------------------------------------------------------
-- Rule Severity (used in config)
--------------------------------------------------------------------------------

data RuleSeverity = RSError | RSWarning | RSSuggestion | RSInfo
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

toSeverity :: RuleSeverity -> Severity
toSeverity = \case
  RSError      -> Error
  RSWarning    -> Warning
  RSSuggestion -> Suggestion
  RSInfo       -> Info

--------------------------------------------------------------------------------
-- Configuration Types
--------------------------------------------------------------------------------

-- | Main configuration
data Config = Config
  { cfgGeneral  :: GeneralConfig
  , cfgOutput   :: OutputConfig
  , cfgUnused   :: UnusedConfig
  , cfgNaming   :: NamingConfig
  , cfgPatterns :: PatternsConfig
  , cfgImports  :: ImportsConfig
  , cfgFix      :: FixConfig
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
  { unusedEnabled        :: Bool
  , unusedCheckFunctions :: Bool
  , unusedCheckTypes     :: Bool
  , unusedCheckImports   :: Bool
  , unusedCheckExports   :: Bool
  , unusedRoots          :: [Text]    -- ^ Regex patterns for roots
  , unusedThRoots        :: [Text]    -- ^ TH-referenced roots
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
  , patternsRules   :: [PatternRule]
  }
  deriving stock (Eq, Show, Generic)

-- | Pattern rule
data PatternRule = PatternRule
  { prName        :: Text        -- ^ Rule name
  , prMatch       :: Text        -- ^ Pattern to match (Haskell syntax)
  , prFix         :: Maybe Text  -- ^ Replacement pattern
  , prWhere       :: Maybe Text  -- ^ Type constraint
  , prSeverity    :: RuleSeverity
  , prMessage     :: Text        -- ^ Message to show
  }
  deriving stock (Eq, Show, Generic)

-- | Import configuration
data ImportsConfig = ImportsConfig
  { importsRemoveUnused   :: Bool
  , importsSuggestQualified :: [Text]  -- ^ Modules that should be qualified
  , importsRequireExplicit  :: Bool    -- ^ Require explicit import lists
  , importsCombine          :: Bool    -- ^ Combine fragmented imports
  }
  deriving stock (Eq, Show, Generic)

-- | Fix configuration
data FixConfig = FixConfig
  { fixEnabled  :: Bool
  , fixSafeOnly :: Bool  -- ^ Only apply safe fixes
  , fixPreview  :: Bool  -- ^ Show preview before applying
  , fixBackup   :: Bool  -- ^ Create .bak files
  }
  deriving stock (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Default Configuration
--------------------------------------------------------------------------------

defaultConfig :: Config
defaultConfig = Config
  { cfgGeneral = GeneralConfig
      { genDirectories = ["src", "app"]
      , genExclude = ["Generated/**", "*.gen.hs"]
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
      , unusedRoots = ["^Main.main$", "^Paths_.*"]
      , unusedThRoots = ["parseJSON", "toJSON", "makeLenses"]
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
      , importsRequireExplicit = False
      , importsCombine = True
      }
  , cfgFix = FixConfig
      { fixEnabled = True
      , fixSafeOnly = True
      , fixPreview = True
      , fixBackup = True
      }
  }

-- | Default pattern rules (common anti-patterns)
defaultPatternRules :: [PatternRule]
defaultPatternRules =
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
    ]

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    cfgGeneral  <- o .:? "general"  .!= cfgGeneral defaultConfig
    cfgOutput   <- o .:? "output"   .!= cfgOutput defaultConfig
    cfgUnused   <- o .:? "unused"   .!= cfgUnused defaultConfig
    cfgNaming   <- o .:? "naming"   .!= cfgNaming defaultConfig
    cfgPatterns <- o .:? "patterns" .!= cfgPatterns defaultConfig
    cfgImports  <- o .:? "imports"  .!= cfgImports defaultConfig
    cfgFix      <- o .:? "fix"      .!= cfgFix defaultConfig
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
    , "roots" AE..= unusedRoots
    , "th-roots" AE..= unusedThRoots
    ]

instance FromJSON UnusedConfig where
  parseJSON = withObject "UnusedConfig" $ \o -> do
    unusedEnabled        <- o .:? "enabled"         .!= True
    unusedCheckFunctions <- o .:? "check-functions" .!= True
    unusedCheckTypes     <- o .:? "check-types"     .!= True
    unusedCheckImports   <- o .:? "check-imports"   .!= True
    unusedCheckExports   <- o .:? "check-exports"   .!= True
    unusedRoots          <- o .:? "roots"           .!= ["^Main.main$"]
    unusedThRoots        <- o .:? "th-roots"        .!= []
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
    , "rules" AE..= patternsRules
    ]

instance FromJSON PatternsConfig where
  parseJSON = withObject "PatternsConfig" $ \o -> do
    patternsEnabled <- o .:? "enabled" .!= True
    patternsRules   <- o .:? "rules"   .!= []
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
    ]

instance FromJSON ImportsConfig where
  parseJSON = withObject "ImportsConfig" $ \o -> do
    importsRemoveUnused     <- o .:? "remove-unused"     .!= True
    importsSuggestQualified <- o .:? "suggest-qualified" .!= []
    importsRequireExplicit  <- o .:? "require-explicit"  .!= False
    importsCombine          <- o .:? "combine"           .!= True
    pure ImportsConfig{..}

instance ToJSON FixConfig where
  toJSON FixConfig{..} = object
    [ "enabled" AE..= fixEnabled
    , "safe-only" AE..= fixSafeOnly
    , "preview" AE..= fixPreview
    , "backup" AE..= fixBackup
    ]

instance FromJSON FixConfig where
  parseJSON = withObject "FixConfig" $ \o -> do
    fixEnabled  <- o .:? "enabled"   .!= True
    fixSafeOnly <- o .:? "safe-only" .!= True
    fixPreview  <- o .:? "preview"   .!= True
    fixBackup   <- o .:? "backup"    .!= True
    pure FixConfig{..}

--------------------------------------------------------------------------------
-- Configuration Loading
--------------------------------------------------------------------------------

-- | Load configuration from default locations or specific file
loadConfig :: Maybe FilePath -> IO Config
loadConfig mpath = case mpath of
  Just path -> loadConfigFromFile path
  Nothing   -> findAndLoadConfig

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
  <*> TOML.arrayOf TOML._Text "roots" .= unusedRoots
  <*> TOML.arrayOf TOML._Text "th-roots" .= unusedThRoots

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
patternsCodec = PatternsConfig
  <$> TOML.bool "enabled" .= patternsEnabled
  <*> TOML.list patternRuleCodec "rules" .= patternsRules

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
  <*> TOML.bool "require-explicit" .= importsRequireExplicit
  <*> TOML.bool "combine" .= importsCombine

fixCodec :: TomlCodec FixConfig
fixCodec = FixConfig
  <$> TOML.bool "enabled" .= fixEnabled
  <*> TOML.bool "safe-only" .= fixSafeOnly
  <*> TOML.bool "preview" .= fixPreview
  <*> TOML.bool "backup" .= fixBackup

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
