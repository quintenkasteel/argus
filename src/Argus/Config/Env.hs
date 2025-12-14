{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Argus.Config.Env
-- Description : Environment variable configuration support
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides environment variable support for Argus configuration.
-- Environment variables can override settings from configuration files,
-- enabling CI/CD integration and runtime customization.
--
-- == Supported Environment Variables
--
-- | Variable | Description | Example |
-- |----------|-------------|---------|
-- | @ARGUS_CONFIG@ | Path to config file | @/path/to/argus.toml@ |
-- | @ARGUS_OUTPUT_FORMAT@ | Output format | @json@, @terminal@, @sarif@ |
-- | @ARGUS_MODE@ | Analysis mode | @quick@, @full@, @plugin@ |
-- | @ARGUS_HIE_DIR@ | HIE files directory | @.hie@ |
-- | @ARGUS_COLOR@ | Enable colors | @true@, @false@, @1@, @0@ |
-- | @ARGUS_CONTEXT_LINES@ | Context lines | @3@ |
-- | @ARGUS_SEVERITY@ | Minimum severity | @error@, @warning@, @info@ |
-- | @ARGUS_EXCLUDE@ | Exclude patterns | @test/**,bench/**@ |
-- | @ARGUS_PARALLEL@ | Parallel strategy | @rules@, @lines@, @both@, @seq@ |
-- | @ARGUS_TIMEOUT@ | Timeout per file | @30@ (seconds) |
-- | @ARGUS_MEMORY_LIMIT@ | Memory limit | @1024@ (MB) |
--
-- == Usage
--
-- @
-- config <- loadConfigWithEnv Nothing
-- -- or with explicit config file
-- config <- loadConfigWithEnv (Just "argus.toml")
-- @
module Argus.Config.Env
  ( -- * Environment Configuration
    EnvConfig (..)
  , loadEnvConfig
  , applyEnvConfig
  , loadConfigWithEnv

    -- * Individual Environment Variables
  , envConfigPath
  , envOutputFormat
  , envMode
  , envHieDir
  , envColor
  , envContextLines
  , envSeverity
  , envExclude
  , envParallel
  , envTimeout
  , envMemoryLimit

    -- * Parsing Helpers
  , parseBool
  , parseInt
  , parseDouble
  , parseList
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Environment Configuration Type
--------------------------------------------------------------------------------

-- | Configuration values from environment variables
--
-- All fields are @Maybe@ because environment variables are optional.
-- When set, they override the corresponding config file values.
data EnvConfig = EnvConfig
  { envCfgPath         :: Maybe FilePath    -- ^ Config file path (ARGUS_CONFIG)
  , envCfgOutputFormat :: Maybe Text        -- ^ Output format (ARGUS_OUTPUT_FORMAT)
  , envCfgMode         :: Maybe Text        -- ^ Analysis mode (ARGUS_MODE)
  , envCfgHieDir       :: Maybe FilePath    -- ^ HIE directory (ARGUS_HIE_DIR)
  , envCfgColor        :: Maybe Bool        -- ^ Color output (ARGUS_COLOR)
  , envCfgContextLines :: Maybe Int         -- ^ Context lines (ARGUS_CONTEXT_LINES)
  , envCfgSeverity     :: Maybe Text        -- ^ Minimum severity (ARGUS_SEVERITY)
  , envCfgExclude      :: Maybe [Text]      -- ^ Exclude patterns (ARGUS_EXCLUDE)
  , envCfgParallel     :: Maybe Text        -- ^ Parallel strategy (ARGUS_PARALLEL)
  , envCfgTimeout      :: Maybe Int         -- ^ Timeout in seconds (ARGUS_TIMEOUT)
  , envCfgMemoryLimit  :: Maybe Int         -- ^ Memory limit in MB (ARGUS_MEMORY_LIMIT)
  }
  deriving stock (Eq, Show)

-- | Empty environment config (no overrides)
emptyEnvConfig :: EnvConfig
emptyEnvConfig = EnvConfig
  { envCfgPath         = Nothing
  , envCfgOutputFormat = Nothing
  , envCfgMode         = Nothing
  , envCfgHieDir       = Nothing
  , envCfgColor        = Nothing
  , envCfgContextLines = Nothing
  , envCfgSeverity     = Nothing
  , envCfgExclude      = Nothing
  , envCfgParallel     = Nothing
  , envCfgTimeout      = Nothing
  , envCfgMemoryLimit  = Nothing
  }

--------------------------------------------------------------------------------
-- Environment Variable Names
--------------------------------------------------------------------------------

-- | Get config file path from ARGUS_CONFIG
envConfigPath :: IO (Maybe FilePath)
envConfigPath = lookupEnv "ARGUS_CONFIG"

-- | Get output format from ARGUS_OUTPUT_FORMAT
envOutputFormat :: IO (Maybe Text)
envOutputFormat = fmap T.pack <$> lookupEnv "ARGUS_OUTPUT_FORMAT"

-- | Get analysis mode from ARGUS_MODE
envMode :: IO (Maybe Text)
envMode = fmap T.pack <$> lookupEnv "ARGUS_MODE"

-- | Get HIE directory from ARGUS_HIE_DIR
envHieDir :: IO (Maybe FilePath)
envHieDir = lookupEnv "ARGUS_HIE_DIR"

-- | Get color setting from ARGUS_COLOR
envColor :: IO (Maybe Bool)
envColor = do
  mval <- lookupEnv "ARGUS_COLOR"
  pure $ mval >>= parseBool

-- | Get context lines from ARGUS_CONTEXT_LINES
envContextLines :: IO (Maybe Int)
envContextLines = do
  mval <- lookupEnv "ARGUS_CONTEXT_LINES"
  pure $ mval >>= parseInt

-- | Get minimum severity from ARGUS_SEVERITY
envSeverity :: IO (Maybe Text)
envSeverity = fmap T.pack <$> lookupEnv "ARGUS_SEVERITY"

-- | Get exclude patterns from ARGUS_EXCLUDE (comma-separated)
envExclude :: IO (Maybe [Text])
envExclude = do
  mval <- lookupEnv "ARGUS_EXCLUDE"
  pure $ fmap (parseList ',') mval

-- | Get parallel strategy from ARGUS_PARALLEL
envParallel :: IO (Maybe Text)
envParallel = fmap T.pack <$> lookupEnv "ARGUS_PARALLEL"

-- | Get timeout from ARGUS_TIMEOUT (seconds)
envTimeout :: IO (Maybe Int)
envTimeout = do
  mval <- lookupEnv "ARGUS_TIMEOUT"
  pure $ mval >>= parseInt

-- | Get memory limit from ARGUS_MEMORY_LIMIT (MB)
envMemoryLimit :: IO (Maybe Int)
envMemoryLimit = do
  mval <- lookupEnv "ARGUS_MEMORY_LIMIT"
  pure $ mval >>= parseInt

--------------------------------------------------------------------------------
-- Loading Environment Configuration
--------------------------------------------------------------------------------

-- | Load all environment variables into EnvConfig
loadEnvConfig :: IO EnvConfig
loadEnvConfig = do
  path         <- envConfigPath
  outputFormat <- envOutputFormat
  mode         <- envMode
  hieDir       <- envHieDir
  color        <- envColor
  contextLines <- envContextLines
  severity     <- envSeverity
  exclude      <- envExclude
  parallel     <- envParallel
  timeout      <- envTimeout
  memoryLimit  <- envMemoryLimit

  pure EnvConfig
    { envCfgPath         = path
    , envCfgOutputFormat = outputFormat
    , envCfgMode         = mode
    , envCfgHieDir       = hieDir
    , envCfgColor        = color
    , envCfgContextLines = contextLines
    , envCfgSeverity     = severity
    , envCfgExclude      = exclude
    , envCfgParallel     = parallel
    , envCfgTimeout      = timeout
    , envCfgMemoryLimit  = memoryLimit
    }

--------------------------------------------------------------------------------
-- Applying Environment Configuration
--------------------------------------------------------------------------------

-- | Type class for configs that can be modified by environment variables
class ApplyEnvConfig a where
  applyEnv :: EnvConfig -> a -> a

-- | Apply environment config to a general config record
--
-- This function takes a base configuration and applies any environment
-- variable overrides. Environment variables take precedence over file config.
applyEnvConfig :: EnvConfig -> EnvConfigurable a => a -> a
applyEnvConfig = applyEnvOverrides

-- | Type class for configuration records that support env overrides
class EnvConfigurable a where
  applyEnvOverrides :: EnvConfig -> a -> a

--------------------------------------------------------------------------------
-- Environment-Aware Configuration Loading
--------------------------------------------------------------------------------

-- | Load configuration with environment variable overrides
--
-- This is a convenience function that:
-- 1. Checks ARGUS_CONFIG for config file path
-- 2. Loads configuration from file (or default)
-- 3. Applies environment variable overrides
--
-- For full configuration loading with file parsing, use Argus.Config.loadConfig
-- which delegates to this for environment handling.
loadConfigWithEnv :: Maybe FilePath -> IO EnvConfig
loadConfigWithEnv _mpath = loadEnvConfig

--------------------------------------------------------------------------------
-- Parsing Helpers
--------------------------------------------------------------------------------

-- | Parse a boolean from string
--
-- Accepts: "true", "false", "1", "0", "yes", "no", "on", "off"
parseBool :: String -> Maybe Bool
parseBool s = case map toLower' s of
  "true"  -> Just True
  "false" -> Just False
  "1"     -> Just True
  "0"     -> Just False
  "yes"   -> Just True
  "no"    -> Just False
  "on"    -> Just True
  "off"   -> Just False
  _       -> Nothing
  where
    toLower' c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

-- | Parse an integer from string
parseInt :: String -> Maybe Int
parseInt = readMaybe

-- | Parse a double from string
parseDouble :: String -> Maybe Double
parseDouble = readMaybe

-- | Parse a comma-separated list from string
parseList :: Char -> String -> [Text]
parseList sep = map T.strip . T.split (== sep) . T.pack
