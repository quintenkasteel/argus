{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Config.Validation
-- Description : Configuration validation and error reporting
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides validation for Argus configuration files.
-- It checks for common errors and provides helpful error messages.
--
-- == Validation Checks
--
-- * Regex patterns compile successfully
-- * Enum values are valid (mode, format, group-by)
-- * Numeric thresholds are sensible (warning < error)
-- * File paths are valid
-- * Required fields are present
--
-- == Usage
--
-- @
-- config <- loadConfig Nothing
-- case validateConfig config of
--   ValidationSuccess -> putStrLn "Config is valid"
--   ValidationFailure errs -> mapM_ print errs
-- @
module Argus.Config.Validation
  ( -- * Validation Types
    ValidationResult (..)
  , ValidationError (..)
  , ValidationSeverity (..)
  , ValidationContext (..)

    -- * Validation Functions
  , validateConfig
  , validateConfigStrict
  , validateConfigWithContext

    -- * Individual Validators
  , validateGeneralConfig
  , validateOutputConfig
  , validateUnusedConfig
  , validateNamingConfig
  , validatePatternsConfig
  , validateImportsConfig
  , validateComplexityConfig
  , validateResourceConfig
  , validateArchitectureConfig

    -- * Utilities
  , isValidRegex
  , isValidGlobPattern
  , formatValidationErrors
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist)
import Text.Regex.TDFA (Regex, makeRegexM)

import Argus.Config
import Argus.Rules.Types qualified as RT

--------------------------------------------------------------------------------
-- Validation Types
--------------------------------------------------------------------------------

-- | Result of configuration validation
data ValidationResult
  = ValidationSuccess
    -- ^ Configuration is valid
  | ValidationWarnings [ValidationError]
    -- ^ Configuration is valid but has warnings
  | ValidationFailure [ValidationError]
    -- ^ Configuration has errors that prevent use
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A validation error or warning
data ValidationError = ValidationError
  { veSection   :: Text            -- ^ Config section (e.g., "general", "imports")
  , veField     :: Text            -- ^ Field name
  , veSeverity  :: ValidationSeverity
  , veMessage   :: Text            -- ^ Human-readable error message
  , veSuggestion :: Maybe Text     -- ^ Suggested fix
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Severity of validation issues
data ValidationSeverity
  = VSError     -- ^ Invalid configuration, cannot proceed
  | VSWarning   -- ^ Potentially problematic, but usable
  | VSInfo      -- ^ Informational note
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Context for validation (e.g., paths to check existence)
data ValidationContext = ValidationContext
  { vcCheckPaths    :: Bool   -- ^ Whether to check if paths exist
  , vcBaseDir       :: FilePath  -- ^ Base directory for relative paths
  , vcStrictMode    :: Bool   -- ^ Treat warnings as errors
  }
  deriving stock (Eq, Show)

-- | Default validation context
defaultValidationContext :: ValidationContext
defaultValidationContext = ValidationContext
  { vcCheckPaths = True
  , vcBaseDir = "."
  , vcStrictMode = False
  }

--------------------------------------------------------------------------------
-- Main Validation Functions
--------------------------------------------------------------------------------

-- | Validate a configuration with default context
validateConfig :: Config -> IO ValidationResult
validateConfig = validateConfigWithContext defaultValidationContext

-- | Validate a configuration in strict mode (warnings become errors)
validateConfigStrict :: Config -> IO ValidationResult
validateConfigStrict = validateConfigWithContext defaultValidationContext { vcStrictMode = True }

-- | Validate a configuration with custom context
validateConfigWithContext :: ValidationContext -> Config -> IO ValidationResult
validateConfigWithContext ctx cfg = do
  -- Collect errors from all sections
  generalErrs <- validateGeneralConfig ctx (cfgGeneral cfg)
  let outputErrs = validateOutputConfig (cfgOutput cfg)
      unusedErrs = validateUnusedConfig (cfgUnused cfg)
      namingErrs = validateNamingConfig (cfgNaming cfg)
      patternsErrs = validatePatternsConfig (cfgPatterns cfg)
      importsErrs = validateImportsConfig (cfgImports cfg)
      complexityErrs = validateComplexityConfig (cfgComplexity cfg)

  let resourceErrs = validateResourceConfig (cfgResource cfg)
      architectureErrs = validateArchitectureConfig (cfgArchitecture cfg)
      allErrors = generalErrs ++ outputErrs ++ unusedErrs ++ namingErrs
                  ++ patternsErrs ++ importsErrs ++ complexityErrs ++ resourceErrs
                  ++ architectureErrs

  pure $ case (hasErrors allErrors, hasWarnings allErrors, vcStrictMode ctx) of
    (True, _, _)     -> ValidationFailure allErrors
    (_, True, True)  -> ValidationFailure allErrors  -- Strict mode: warnings are errors
    (_, True, False) -> ValidationWarnings allErrors
    _                -> ValidationSuccess

  where
    hasErrors = any (\e -> veSeverity e == VSError)
    hasWarnings = any (\e -> veSeverity e == VSWarning)

--------------------------------------------------------------------------------
-- Section Validators
--------------------------------------------------------------------------------

-- | Validate general configuration
validateGeneralConfig :: ValidationContext -> GeneralConfig -> IO [ValidationError]
validateGeneralConfig ctx GeneralConfig{..} = do
  -- Check directories exist if enabled
  dirErrors <- if vcCheckPaths ctx
    then concat <$> mapM checkDirectory genDirectories
    else pure []

  -- Validate mode
  let modeErrors = validateMode genMode

  -- Validate exclude patterns
  let excludeErrors = concatMap validateGlobPattern (zip [1..] genExclude)

  pure $ dirErrors ++ modeErrors ++ excludeErrors
  where
    checkDirectory :: FilePath -> IO [ValidationError]
    checkDirectory dir = do
      exists <- doesDirectoryExist dir
      pure $ if exists
        then []
        else [ValidationError
          { veSection = "general"
          , veField = "directories"
          , veSeverity = VSWarning
          , veMessage = "Directory does not exist: " <> T.pack dir
          , veSuggestion = Just "Check the path or create the directory"
          }]

    validateMode :: Text -> [ValidationError]
    validateMode mode
      | mode `elem` ["quick", "full", "plugin"] = []
      | otherwise = [ValidationError
          { veSection = "general"
          , veField = "mode"
          , veSeverity = VSError
          , veMessage = "Invalid mode: " <> mode
          , veSuggestion = Just "Use one of: quick, full, plugin"
          }]

    validateGlobPattern :: (Int, Text) -> [ValidationError]
    validateGlobPattern (idx, pat)
      | isValidGlobPattern pat = []
      | otherwise = [ValidationError
          { veSection = "general"
          , veField = "exclude[" <> T.pack (show idx) <> "]"
          , veSeverity = VSWarning
          , veMessage = "Invalid glob pattern: " <> pat
          , veSuggestion = Just "Check for unmatched brackets or invalid wildcards"
          }]

-- | Validate output configuration
validateOutputConfig :: OutputConfig -> [ValidationError]
validateOutputConfig OutputConfig{..} =
  validateFormat outFormat
  ++ validateGroupBy outGroupBy
  ++ validateContextLines outContextLines
  where
    validateFormat :: Text -> [ValidationError]
    validateFormat fmt
      | fmt `elem` validFormats = []
      | otherwise = [ValidationError
          { veSection = "output"
          , veField = "format"
          , veSeverity = VSError
          , veMessage = "Invalid output format: " <> fmt
          , veSuggestion = Just $ "Use one of: " <> T.intercalate ", " validFormats
          }]
      where
        validFormats = ["terminal", "json", "html", "sarif", "junit", "codeclimate", "checkstyle"]

    validateGroupBy :: Text -> [ValidationError]
    validateGroupBy grp
      | grp `elem` ["file", "rule", "severity"] = []
      | otherwise = [ValidationError
          { veSection = "output"
          , veField = "group-by"
          , veSeverity = VSError
          , veMessage = "Invalid group-by value: " <> grp
          , veSuggestion = Just "Use one of: file, rule, severity"
          }]

    validateContextLines :: Int -> [ValidationError]
    validateContextLines n
      | n >= 0 && n <= 20 = []
      | n < 0 = [ValidationError
          { veSection = "output"
          , veField = "context-lines"
          , veSeverity = VSError
          , veMessage = "context-lines cannot be negative"
          , veSuggestion = Just "Use a value between 0 and 20"
          }]
      | otherwise = [ValidationError
          { veSection = "output"
          , veField = "context-lines"
          , veSeverity = VSWarning
          , veMessage = "Large context-lines value may produce verbose output"
          , veSuggestion = Just "Consider using a smaller value (2-5)"
          }]

-- | Validate unused code detection configuration
validateUnusedConfig :: UnusedConfig -> [ValidationError]
validateUnusedConfig UnusedConfig{..} =
  concatMap (validateRegexField "unused" "roots") (zip [0..] unusedRoots)
  ++ concatMap (validateRegexField "unused" "th-roots") (zip [0..] unusedThRoots)

-- | Validate naming configuration
validateNamingConfig :: NamingConfig -> [ValidationError]
validateNamingConfig NamingConfig{..} =
  concatMap validateTypeRule (zip [0..] namingTypes)
  ++ concatMap validateVariableRule (zip [0..] namingVariables)
  where
    validateTypeRule :: (Int, TypeRule) -> [ValidationError]
    validateTypeRule (idx, TypeRule{..}) =
      let fieldPrefix = "types[" <> T.pack (show idx) <> "]"
      in validatePatternField "naming" (fieldPrefix <> ".pattern") trPattern
         ++ maybe [] (validatePatternField "naming" (fieldPrefix <> ".within")) trWithin

    validateVariableRule :: (Int, VariableRule) -> [ValidationError]
    validateVariableRule (idx, VariableRule{..}) =
      let fieldPrefix = "variables[" <> T.pack (show idx) <> "]"
      in validatePatternField "naming" (fieldPrefix <> ".type") vrType

-- | Validate patterns configuration
validatePatternsConfig :: PatternsConfig -> [ValidationError]
validatePatternsConfig PatternsConfig{..} =
  concatMap validatePatternRule (zip [0..] patternsRules)
  where
    validatePatternRule :: (Int, RT.Rule) -> [ValidationError]
    validatePatternRule (idx, rule) =
      let fieldPrefix = "rules[" <> T.pack (show idx) <> "]"
          patText = RT.rulePatternToText (RT.rulePattern rule)
      in checkRuleNameUnique idx (RT.ruleId rule)
         ++ checkMatchNotEmpty fieldPrefix patText
         ++ maybe [] (checkFixValid fieldPrefix patText) (RT.ruleReplacement rule)

    checkRuleNameUnique :: Int -> Text -> [ValidationError]
    checkRuleNameUnique idx name =
      let allNames = map RT.ruleId patternsRules
          duplicates = length (filter (== name) allNames) > 1
      in if duplicates && idx == 0  -- Only report once
         then [ValidationError
           { veSection = "patterns"
           , veField = "rules"
           , veSeverity = VSWarning
           , veMessage = "Duplicate rule name: " <> name
           , veSuggestion = Just "Use unique rule names for clarity"
           }]
         else []

    checkMatchNotEmpty :: Text -> Text -> [ValidationError]
    checkMatchNotEmpty field match
      | T.null (T.strip match) = [ValidationError
          { veSection = "patterns"
          , veField = field <> ".match"
          , veSeverity = VSError
          , veMessage = "Pattern match cannot be empty"
          , veSuggestion = Nothing
          }]
      | otherwise = []

    checkFixValid :: Text -> Text -> Text -> [ValidationError]
    checkFixValid field _match fix
      | T.null (T.strip fix) = [ValidationError
          { veSection = "patterns"
          , veField = field <> ".fix"
          , veSeverity = VSWarning
          , veMessage = "Empty fix pattern - this will delete matched code"
          , veSuggestion = Just "Use Nothing/null for fix if no replacement is intended"
          }]
      | otherwise = []

-- | Validate imports configuration
validateImportsConfig :: ImportsConfig -> [ValidationError]
validateImportsConfig ImportsConfig{..} =
  validateModuleNames
  ++ concatMap (validateRegexField "imports" "th-roots") (zip [0..] importsThRoots)
  where
    validateModuleNames :: [ValidationError]
    validateModuleNames = concatMap validateModuleName (zip [0..] importsSuggestQualified)

    validateModuleName :: (Int, Text) -> [ValidationError]
    validateModuleName (idx, name)
      | isValidModuleName name = []
      | otherwise = [ValidationError
          { veSection = "imports"
          , veField = "suggest-qualified[" <> T.pack (show idx) <> "]"
          , veSeverity = VSWarning
          , veMessage = "Potentially invalid module name: " <> name
          , veSuggestion = Just "Module names should be capitalized and dot-separated"
          }]

    isValidModuleName :: Text -> Bool
    isValidModuleName name =
      let parts = T.splitOn "." name
      in all isValidPart parts && not (null parts)
      where
        isValidPart p = not (T.null p) && T.all isValidChar p && isUpper (T.head p)
        isValidChar c = c == '_' || c == '\'' || (c >= 'a' && c <= 'z')
                        || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
        isUpper c = c >= 'A' && c <= 'Z'

-- | Validate complexity configuration
validateComplexityConfig :: ComplexityConfig -> [ValidationError]
validateComplexityConfig ComplexityConfig{..} =
  validateThresholdPair "cyclomatic" compCyclomaticWarning compCyclomaticError
  ++ validateThresholdPair "cognitive" compCognitiveWarning compCognitiveError
  ++ validatePositive "line-length-warning" compLineLengthWarning
  ++ validatePositive "nesting-warning" compNestingWarning
  ++ validatePositive "parameter-warning" compParameterWarning
  ++ validatePositive "pattern-branch-warning" compPatternBranchWarning
  where
    validateThresholdPair :: Text -> Int -> Int -> [ValidationError]
    validateThresholdPair name warning err =
      let warningField = name <> "-warning"
          errorField = name <> "-error"
      in
        if warning > err
        then [ValidationError
          { veSection = "complexity"
          , veField = warningField
          , veSeverity = VSError
          , veMessage = warningField <> " (" <> T.pack (show warning)
                        <> ") should not be greater than " <> errorField
                        <> " (" <> T.pack (show err) <> ")"
          , veSuggestion = Just "Swap the values or adjust thresholds"
          }]
        else if warning < 1
        then [ValidationError
          { veSection = "complexity"
          , veField = warningField
          , veSeverity = VSWarning
          , veMessage = "Very low " <> warningField <> " threshold may cause excessive warnings"
          , veSuggestion = Just "Consider using a value of at least 5"
          }]
        else []

    validatePositive :: Text -> Int -> [ValidationError]
    validatePositive field value
      | value > 0 = []
      | otherwise = [ValidationError
          { veSection = "complexity"
          , veField = field
          , veSeverity = VSError
          , veMessage = field <> " must be positive"
          , veSuggestion = Nothing
          }]

-- | Validate resource configuration
validateResourceConfig :: ResourceConfig -> [ValidationError]
validateResourceConfig ResourceConfig{..} =
  validateTimeout resTimeoutSeconds
  ++ validateMemory resMaxMemoryMB
  ++ validateRetries resMaxRetries
  ++ validateSlowWarning resWarnSlowFiles resTimeoutSeconds
  where
    validateTimeout :: Maybe Int -> [ValidationError]
    validateTimeout Nothing = []  -- No timeout is valid
    validateTimeout (Just secs)
      | secs < 1 = [ValidationError
          { veSection = "resource"
          , veField = "timeout-seconds"
          , veSeverity = VSError
          , veMessage = "Timeout must be at least 1 second"
          , veSuggestion = Just "Use null/Nothing to disable timeout"
          }]
      | secs < 5 = [ValidationError
          { veSection = "resource"
          , veField = "timeout-seconds"
          , veSeverity = VSWarning
          , veMessage = "Very short timeout may cause false failures for large files"
          , veSuggestion = Just "Consider using at least 10 seconds"
          }]
      | secs > 600 = [ValidationError
          { veSection = "resource"
          , veField = "timeout-seconds"
          , veSeverity = VSWarning
          , veMessage = "Very long timeout: " <> T.pack (show secs) <> " seconds"
          , veSuggestion = Just "Consider using a shorter timeout for faster feedback"
          }]
      | otherwise = []

    validateMemory :: Maybe Int -> [ValidationError]
    validateMemory Nothing = []  -- No limit is valid
    validateMemory (Just mb)
      | mb < 128 = [ValidationError
          { veSection = "resource"
          , veField = "max-memory-mb"
          , veSeverity = VSWarning
          , veMessage = "Very low memory limit may prevent analysis of large files"
          , veSuggestion = Just "Consider at least 256MB for reliable operation"
          }]
      | mb > 16384 = [ValidationError
          { veSection = "resource"
          , veField = "max-memory-mb"
          , veSeverity = VSWarning
          , veMessage = "Very high memory limit: " <> T.pack (show mb) <> "MB"
          , veSuggestion = Just "Consider if this is intentional"
          }]
      | otherwise = []

    validateRetries :: Int -> [ValidationError]
    validateRetries retries
      | retries < 0 = [ValidationError
          { veSection = "resource"
          , veField = "max-retries"
          , veSeverity = VSError
          , veMessage = "Retries cannot be negative"
          , veSuggestion = Nothing
          }]
      | retries > 5 = [ValidationError
          { veSection = "resource"
          , veField = "max-retries"
          , veSeverity = VSWarning
          , veMessage = "High retry count may slow down analysis significantly"
          , veSuggestion = Just "Consider using 1-3 retries"
          }]
      | otherwise = []

    validateSlowWarning :: Maybe Double -> Maybe Int -> [ValidationError]
    validateSlowWarning Nothing _ = []  -- No warning is valid
    validateSlowWarning (Just warnSecs) mTimeout
      | warnSecs <= 0 = [ValidationError
          { veSection = "resource"
          , veField = "warn-slow-files"
          , veSeverity = VSError
          , veMessage = "Slow file warning threshold must be positive"
          , veSuggestion = Just "Use null/Nothing to disable slow file warnings"
          }]
      | Just timeoutSecs <- mTimeout, warnSecs > fromIntegral timeoutSecs =
          [ValidationError
            { veSection = "resource"
            , veField = "warn-slow-files"
            , veSeverity = VSWarning
            , veMessage = "Slow file warning (" <> T.pack (show warnSecs)
                          <> "s) is greater than timeout (" <> T.pack (show timeoutSecs) <> "s)"
            , veSuggestion = Just "Set warn-slow-files lower than timeout-seconds"
            }]
      | otherwise = []

-- | Validate architecture configuration
validateArchitectureConfig :: ArchitectureConfig -> [ValidationError]
validateArchitectureConfig ArchitectureConfig{..} =
  validateMaxCycleLength acMaxCycleLength
  ++ validateInstabilityThreshold acInstabilityThreshold
  ++ validateCouplingThreshold acCouplingThreshold
  ++ validateLayers acLayers
  where
    validateMaxCycleLength :: Int -> [ValidationError]
    validateMaxCycleLength n
      | n < 2 = [ValidationError
          { veSection = "architecture"
          , veField = "max-cycle-length"
          , veSeverity = VSError
          , veMessage = "max-cycle-length must be at least 2 (minimum for a cycle)"
          , veSuggestion = Just "Use a value of 2 or higher"
          }]
      | n > 20 = [ValidationError
          { veSection = "architecture"
          , veField = "max-cycle-length"
          , veSeverity = VSWarning
          , veMessage = "Very high max-cycle-length (" <> T.pack (show n)
                        <> ") may slow down analysis"
          , veSuggestion = Just "Consider using a value between 5 and 15"
          }]
      | otherwise = []

    validateInstabilityThreshold :: Double -> [ValidationError]
    validateInstabilityThreshold threshold
      | threshold < 0.0 = [ValidationError
          { veSection = "architecture"
          , veField = "instability-threshold"
          , veSeverity = VSError
          , veMessage = "instability-threshold cannot be negative"
          , veSuggestion = Just "Use a value between 0.0 and 1.0"
          }]
      | threshold > 1.0 = [ValidationError
          { veSection = "architecture"
          , veField = "instability-threshold"
          , veSeverity = VSError
          , veMessage = "instability-threshold cannot be greater than 1.0"
          , veSuggestion = Just "Use a value between 0.0 and 1.0"
          }]
      | threshold < 0.3 = [ValidationError
          { veSection = "architecture"
          , veField = "instability-threshold"
          , veSeverity = VSWarning
          , veMessage = "Very low instability-threshold (" <> T.pack (show threshold)
                        <> ") may cause excessive warnings"
          , veSuggestion = Just "Consider using a value between 0.5 and 0.8"
          }]
      | otherwise = []

    validateCouplingThreshold :: Int -> [ValidationError]
    validateCouplingThreshold n
      | n < 1 = [ValidationError
          { veSection = "architecture"
          , veField = "coupling-threshold"
          , veSeverity = VSError
          , veMessage = "coupling-threshold must be at least 1"
          , veSuggestion = Just "Use a positive value"
          }]
      | n < 5 = [ValidationError
          { veSection = "architecture"
          , veField = "coupling-threshold"
          , veSeverity = VSWarning
          , veMessage = "Very low coupling-threshold (" <> T.pack (show n)
                        <> ") may cause excessive warnings"
          , veSuggestion = Just "Consider using a value between 10 and 20"
          }]
      | n > 50 = [ValidationError
          { veSection = "architecture"
          , veField = "coupling-threshold"
          , veSeverity = VSWarning
          , veMessage = "Very high coupling-threshold (" <> T.pack (show n)
                        <> ") may miss significant coupling issues"
          , veSuggestion = Just "Consider using a value between 10 and 30"
          }]
      | otherwise = []

    validateLayers :: [LayerConfig] -> [ValidationError]
    validateLayers layers =
      concatMap (validateLayer layerNames) (zip [0..] layers)
      ++ validateLayerNamesUnique layers
      where
        layerNames = map lcName layers

    validateLayer :: [Text] -> (Int, LayerConfig) -> [ValidationError]
    validateLayer allLayerNames (idx, LayerConfig{..}) =
      let fieldPrefix = "layers[" <> T.pack (show idx) <> "]"
      in validateLayerName fieldPrefix lcName
         ++ validateLayerPatterns fieldPrefix lcPatterns
         ++ validateLayerCanImport fieldPrefix allLayerNames lcCanImport

    validateLayerName :: Text -> Text -> [ValidationError]
    validateLayerName field name
      | T.null (T.strip name) = [ValidationError
          { veSection = "architecture"
          , veField = field <> ".name"
          , veSeverity = VSError
          , veMessage = "Layer name cannot be empty"
          , veSuggestion = Nothing
          }]
      | otherwise = []

    validateLayerPatterns :: Text -> [Text] -> [ValidationError]
    validateLayerPatterns field patterns
      | null patterns = [ValidationError
          { veSection = "architecture"
          , veField = field <> ".patterns"
          , veSeverity = VSWarning
          , veMessage = "Layer has no module patterns - will not match any modules"
          , veSuggestion = Just "Add patterns like \"Module.*\" or \"*.Types\""
          }]
      | otherwise = concatMap validatePattern (zip ([0..] :: [Int]) patterns)
      where
        validatePattern (pidx, pat)
          | T.null (T.strip pat) = [ValidationError
              { veSection = "architecture"
              , veField = field <> ".patterns[" <> T.pack (show pidx) <> "]"
              , veSeverity = VSError
              , veMessage = "Pattern cannot be empty"
              , veSuggestion = Nothing
              }]
          | not (isValidGlobPattern pat) = [ValidationError
              { veSection = "architecture"
              , veField = field <> ".patterns[" <> T.pack (show pidx) <> "]"
              , veSeverity = VSWarning
              , veMessage = "Potentially invalid pattern: " <> pat
              , veSuggestion = Just "Check for unmatched brackets"
              }]
          | otherwise = []

    validateLayerCanImport :: Text -> [Text] -> [Text] -> [ValidationError]
    validateLayerCanImport field allLayerNames canImport =
      concatMap validateImportRef (zip ([0..] :: [Int]) canImport)
      where
        validateImportRef (iidx, importName)
          | T.null (T.strip importName) = [ValidationError
              { veSection = "architecture"
              , veField = field <> ".can-import[" <> T.pack (show iidx) <> "]"
              , veSeverity = VSError
              , veMessage = "Import reference cannot be empty"
              , veSuggestion = Nothing
              }]
          | importName `notElem` allLayerNames = [ValidationError
              { veSection = "architecture"
              , veField = field <> ".can-import[" <> T.pack (show iidx) <> "]"
              , veSeverity = VSWarning
              , veMessage = "Layer reference '" <> importName
                            <> "' does not match any defined layer"
              , veSuggestion = Just $ "Defined layers: " <> T.intercalate ", " allLayerNames
              }]
          | otherwise = []

    validateLayerNamesUnique :: [LayerConfig] -> [ValidationError]
    validateLayerNamesUnique layers =
      let names = map lcName layers
          duplicates = findDuplicates names
      in if null duplicates
         then []
         else [ValidationError
           { veSection = "architecture"
           , veField = "layers"
           , veSeverity = VSError
           , veMessage = "Duplicate layer names: " <> T.intercalate ", " duplicates
           , veSuggestion = Just "Each layer must have a unique name"
           }]
      where
        findDuplicates xs = [x | (x, count) <- countOccurrences xs, count > 1]
        countOccurrences xs = [(x, length (filter (== x) xs)) | x <- nub xs]
        nub [] = []
        nub (x:xs) = x : nub (filter (/= x) xs)

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Validate a regex pattern field
validateRegexField :: Text -> Text -> (Int, Text) -> [ValidationError]
validateRegexField section field (idx, regexPat)
  | isValidRegex regexPat = []
  | otherwise = [ValidationError
      { veSection = section
      , veField = field <> "[" <> T.pack (show idx) <> "]"
      , veSeverity = VSError
      , veMessage = "Invalid regex pattern: " <> regexPat
      , veSuggestion = Just "Check regex syntax - common issues: unescaped brackets, unmatched parens"
      }]

-- | Validate a pattern field (simple wildcards, not full regex)
validatePatternField :: Text -> Text -> Text -> [ValidationError]
validatePatternField section field patField
  | T.null (T.strip patField) = [ValidationError
      { veSection = section
      , veField = field
      , veSeverity = VSError
      , veMessage = "Pattern cannot be empty"
      , veSuggestion = Nothing
      }]
  | otherwise = []

-- | Check if a string is a valid regex
-- Uses pure regex compilation via makeRegexM from TDFA
isValidRegex :: Text -> Bool
isValidRegex pat = isJust (makeRegexM pat :: Maybe Regex)

-- | Check if a string is a valid glob pattern
-- Validates basic glob pattern structure without evaluating
isValidGlobPattern :: Text -> Bool
isValidGlobPattern pat =
  checkBrackets (T.unpack pat) 0
  where
    checkBrackets :: String -> Int -> Bool
    checkBrackets [] depth = depth == 0
    checkBrackets ('[':cs) depth = checkBrackets cs (depth + 1)
    checkBrackets (']':cs) depth = depth > 0 && checkBrackets cs (depth - 1)
    checkBrackets ('\\':_:cs) depth = checkBrackets cs depth  -- Escaped char
    checkBrackets (_:cs) depth = checkBrackets cs depth

-- | Format validation errors for display
formatValidationErrors :: [ValidationError] -> Text
formatValidationErrors [] = "No validation errors"
formatValidationErrors errs = T.unlines $ map formatError errs
  where
    formatError ValidationError{..} = T.concat
      [ severityPrefix veSeverity
      , " ["
      , veSection
      , "."
      , veField
      , "] "
      , veMessage
      , case veSuggestion of
          Just s  -> "\n  Suggestion: " <> s
          Nothing -> ""
      ]

    severityPrefix VSError   = "ERROR"
    severityPrefix VSWarning = "WARNING"
    severityPrefix VSInfo    = "INFO"
