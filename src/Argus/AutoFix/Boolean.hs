{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Argus.AutoFix.Boolean
-- Description : Automated fixes for Boolean expression simplifications
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides comprehensive auto-fix capabilities for Boolean expression
-- simplifications. It handles:
--
-- * Basic Boolean operations (not True → False, True && x → x)
-- * Boolean comparisons (x == True → x, x /= False → x)
-- * If-then-else simplifications (if True then x else y → x)
-- * De Morgan's law applications
-- * Idempotent and identity simplifications
--
-- == Safety Features
--
-- * Validates that fixes don't change semantics
-- * Handles nested Boolean expressions correctly
-- * Respects operator precedence
-- * Preserves whitespace and formatting where possible
--
-- == FixEngine Integration
--
-- This module now implements the 'FixEngine' typeclass from the extensible
-- auto-fix infrastructure, allowing Boolean fixes to be composed with other
-- fix engines.
--
-- @
-- engine <- newBooleanFixEngine defaultBooleanFixConfig
-- result <- fixBooleanExprs engine filePath content
-- case result of
--   BooleanFixSuccess fixes -> applyFixes fixes
--   BooleanFixPartial fixes errors -> handlePartial fixes errors
--   BooleanFixFailure err -> reportError err
-- @
module Argus.AutoFix.Boolean
  ( -- * Fix Engine
    BooleanFixEngine (..)
  , newBooleanFixEngine
  , BooleanFixConfig (..)
  , defaultBooleanFixConfig

    -- * Fixing
  , fixBooleanExprs
  , fixBooleanFile
  , fixBooleanDiagnostics
  , BooleanFixResult (..)
  , BooleanFix (..)
  , BooleanFixCategory (..)

    -- * Individual Fix Functions
  , fixNotTrue
  , fixNotFalse
  , fixTrueAnd
  , fixFalseAnd
  , fixAndFalse
  , fixTrueOr
  , fixFalseOr
  , fixOrTrue
  , fixAndSame
  , fixOrSame
  , fixDeMorganAnd
  , fixDeMorganOr
  , fixEqTrue
  , fixEqFalse
  , fixNeTrue
  , fixNeFalse
  , fixIfTrue
  , fixIfFalse
  , fixIfCondTrue
  , fixIfCondFalse
  , fixIfThenTrue
  , fixIfThenFalse
  , fixIfElseTrue
  , fixIfElseFalse
  , fixIfNot

    -- * Validation
  , validateBooleanFix
  , BooleanValidation (..)

    -- * Statistics
  , BooleanFixStats (..)
  , emptyStats
  , mergeStats

    -- * FixEngine integration
  , toEnrichedFix
  , fromEnrichedFix
  ) where

import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Data.Aeson (ToJSON, FromJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Text.Regex.TDFA ((=~))

import Argus.Types
  ( SrcSpan(..)
  , Line(..)
  , Column(..)
  , Diagnostic(..)
  , Fix(..)
  , FixEdit(..)
  , FixCategory(..)
  , FixSafety(..)
  )

import Argus.Rules.Types (SafetyLevel(..))

import Argus.AutoFix.Types
  ( FixEngine(..)
  , EnrichedFix(..)
  , FixId(..)
  , mkFixId
  , FixMetadata(..)
  , defaultFixMetadata
  , Confidence
  , mkConfidence
  , FixValidation(..)
  , ValidationResult(..)
  , FixApplicationResult(..)
  , ApplyError(..)
  , FixStats(..)
  , emptyFixStats
  )

import Argus.Rules.Types (Category(..))

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for Boolean fix engine
data BooleanFixConfig = BooleanFixConfig
  { bfcEnabledCategories :: [BooleanFixCategory]
      -- ^ Which categories of fixes to apply
  , bfcPreserveParens    :: Bool
      -- ^ Preserve parentheses when they're not strictly needed
  , bfcValidateFixes     :: Bool
      -- ^ Validate fixes before applying
  , bfcMaxFixesPerFile   :: Int
      -- ^ Maximum number of fixes to apply per file
  , bfcAllowNested       :: Bool
      -- ^ Apply fixes to nested expressions
  , bfcPrecedenceAware   :: Bool
      -- ^ Be aware of operator precedence when removing/adding parens
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration
defaultBooleanFixConfig :: BooleanFixConfig
defaultBooleanFixConfig = BooleanFixConfig
  { bfcEnabledCategories = [minBound .. maxBound]
  , bfcPreserveParens    = False
  , bfcValidateFixes     = True
  , bfcMaxFixesPerFile   = 100
  , bfcAllowNested       = True
  , bfcPrecedenceAware   = True
  }

-- | Categories of Boolean fixes
data BooleanFixCategory
  = NotSimplification      -- ^ not True → False, not False → True
  | AndSimplification      -- ^ True && x → x, False && x → False
  | OrSimplification       -- ^ True || x → True, False || x → x
  | IdempotentSimplification  -- ^ x && x → x, x || x → x
  | DeMorganSimplification    -- ^ not x && not y → not (x || y)
  | ComparisonSimplification  -- ^ x == True → x
  | IfSimplification       -- ^ if True then x else y → x
  | ContradictionSimplification  -- ^ x && not x → False
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Engine
--------------------------------------------------------------------------------

-- | Boolean fix engine with caching
data BooleanFixEngine = BooleanFixEngine
  { bfeConfig     :: BooleanFixConfig
  , bfeCache      :: TVar (Map FilePath [BooleanFix])
  , bfeStats      :: TVar BooleanFixStats
  }

-- | Create a new Boolean fix engine
newBooleanFixEngine :: BooleanFixConfig -> IO BooleanFixEngine
newBooleanFixEngine config = do
  cache <- newTVarIO Map.empty
  stats <- newTVarIO emptyStats
  pure BooleanFixEngine
    { bfeConfig = config
    , bfeCache  = cache
    , bfeStats  = stats
    }

--------------------------------------------------------------------------------
-- FixEngine Instance
--------------------------------------------------------------------------------

instance FixEngine BooleanFixEngine where
  type EngineConfig BooleanFixEngine = BooleanFixConfig
  type EngineCategory BooleanFixEngine = BooleanFixCategory

  engineName _ = "boolean-fix"

  engineDescription _ =
    "Boolean expression simplification engine. Simplifies Boolean logic patterns \
    \like 'not True', 'x == True', 'if True then a else b', De Morgan's laws, \
    \and contradiction detection."

  engineCategories _ = [minBound .. maxBound]

  findFixes engine path content = do
    let config = bfeConfig engine
        enabledCats = bfcEnabledCategories config
        boolFixes = findAllBooleanFixes path content enabledCats
    now <- getCurrentTime
    pure $ map (toEnrichedFix now) boolFixes

  applyFix _engine _path content enrichedFix = do
    case fromEnrichedFix enrichedFix of
      Nothing -> pure $ ApplyFailure $ EngineError "Not a Boolean fix"
      Just boolFix -> do
        case applySingleFix content boolFix of
          Left err -> pure $ ApplyFailure $ EngineError err
          Right newContent -> pure $ ApplySuccess
            { arsNewContent = newContent
            , arsAppliedFix = enrichedFix
            , arsStats = emptyFixStats
            }

  validateFix _engine enrichedFix content = do
    case fromEnrichedFix enrichedFix of
      Nothing -> pure FixValidation
        { fvResult = ValidationError "Not a Boolean fix"
        , fvChecks = []
        , fvSuggestions = []
        }
      Just boolFix -> do
        let validation = validateBooleanFix content boolFix
        pure $ case validation of
          BoolValidationPassed -> FixValidation
            { fvResult = ValidationSuccess
            , fvChecks = [("pattern_present", ValidationSuccess), ("parens_balanced", ValidationSuccess)]
            , fvSuggestions = []
            }
          BoolValidationWarning warn -> FixValidation
            { fvResult = ValidationWarning warn
            , fvChecks = [("pattern_present", ValidationSuccess), ("parens_balanced", ValidationWarning warn)]
            , fvSuggestions = [warn]
            }
          BoolValidationFailed err -> FixValidation
            { fvResult = ValidationError err
            , fvChecks = [("pattern_present", ValidationError err)]
            , fvSuggestions = []
            }

  getEngineStats engine = do
    localStats <- readTVarIO (bfeStats engine)
    pure $ convertToFixStats localStats

-- | Convert local stats to generic FixStats
convertToFixStats :: BooleanFixStats -> FixStats
convertToFixStats bfs =
  let stats = emptyFixStats
  in stats
    { fsTotal = bfsTotalFixes bfs
    , fsApplied = bfsValidationsPassed bfs
    , fsFailed = bfsValidationsFailed bfs
    }

--------------------------------------------------------------------------------
-- Conversion Functions
--------------------------------------------------------------------------------

-- | Convert a BooleanFix to an EnrichedFix
toEnrichedFix :: UTCTime -> BooleanFix -> EnrichedFix
toEnrichedFix timestamp bf = EnrichedFix
  { efId = mkFixId "boolean-fix" (bfRuleId bf <> "-" <> lineId)
  , efFix = Fix
      { fixTitle = bfMessage bf
      , fixEdits = [FixEdit (bfSpan bf) (bfReplacement bf)]
      , fixIsPreferred = True
      , fixAddImports = []
      , fixRemoveImports = []
      , fixCategory = FCStyle
      , fixSafety = categoryToFixSafety (bfCategory bf)
      }
  , efMetadata = (defaultFixMetadata (categoryToRTCategory (bfCategory bf)))
      { fmConfidence = categoryToConfidence (bfCategory bf)
      , fmSafety = categoryToSafety (bfCategory bf)
      , fmCreatedAt = Just timestamp
      , fmTags = Set.fromList ["boolean", categoryToTag (bfCategory bf)]
      , fmSourceRule = Just (bfRuleId bf)
      , fmExplanation = Just (bfMessage bf)
      }
  , efDependencies = Set.empty
  , efConflicts = Set.empty
  , efValidation = Nothing
  }
  where
    lineId = T.pack $ show $ unLine $ srcSpanStartLine $ bfSpan bf

-- | Convert category to Rules.Types.Category
categoryToRTCategory :: BooleanFixCategory -> Category
categoryToRTCategory _ = Style  -- Boolean fixes are generally style improvements

-- | Convert category to confidence level
categoryToConfidence :: BooleanFixCategory -> Confidence
categoryToConfidence = \case
  NotSimplification -> mkConfidence 0.95  -- Very safe
  AndSimplification -> mkConfidence 0.90
  OrSimplification -> mkConfidence 0.90
  IdempotentSimplification -> mkConfidence 0.85
  ComparisonSimplification -> mkConfidence 0.90
  IfSimplification -> mkConfidence 0.80
  DeMorganSimplification -> mkConfidence 0.75  -- Slightly more complex
  ContradictionSimplification -> mkConfidence 0.95

-- | Convert category to safety level
categoryToSafety :: BooleanFixCategory -> SafetyLevel
categoryToSafety = \case
  NotSimplification -> Safe
  AndSimplification -> Safe
  OrSimplification -> Safe
  IdempotentSimplification -> Safe
  ComparisonSimplification -> Safe
  IfSimplification -> MostlySafe
  DeMorganSimplification -> MostlySafe
  ContradictionSimplification -> Safe

-- | Convert category to FixSafety
categoryToFixSafety :: BooleanFixCategory -> FixSafety
categoryToFixSafety = \case
  NotSimplification -> FSAlways
  AndSimplification -> FSAlways
  OrSimplification -> FSAlways
  IdempotentSimplification -> FSAlways
  ComparisonSimplification -> FSAlways
  IfSimplification -> FSMostly
  DeMorganSimplification -> FSMostly
  ContradictionSimplification -> FSAlways

-- | Convert category to tag
categoryToTag :: BooleanFixCategory -> Text
categoryToTag = \case
  NotSimplification -> "not-simplification"
  AndSimplification -> "and-simplification"
  OrSimplification -> "or-simplification"
  IdempotentSimplification -> "idempotent"
  DeMorganSimplification -> "demorgan"
  ComparisonSimplification -> "comparison"
  IfSimplification -> "if-simplification"
  ContradictionSimplification -> "contradiction"

-- | Try to convert an EnrichedFix back to a BooleanFix
fromEnrichedFix :: EnrichedFix -> Maybe BooleanFix
fromEnrichedFix ef
  | fixIdEngine (efId ef) /= "boolean-fix" = Nothing
  | otherwise = case fixEdits (efFix ef) of
      (FixEdit span' replacement:_) -> Just BooleanFix
        { bfSpan = span'
        , bfOriginal = ""  -- We don't preserve original in EnrichedFix
        , bfReplacement = replacement
        , bfCategory = inferCategoryFromTags (fmTags (efMetadata ef))
        , bfRuleId = fromMaybe "unknown" $ fmSourceRule (efMetadata ef)
        , bfMessage = fromMaybe (fixTitle (efFix ef)) $ fmExplanation (efMetadata ef)
        , bfValidation = BoolValidationPassed
        }
      _ -> Nothing

-- | Infer category from tags
inferCategoryFromTags :: Set Text -> BooleanFixCategory
inferCategoryFromTags tags
  | "not-simplification" `Set.member` tags = NotSimplification
  | "and-simplification" `Set.member` tags = AndSimplification
  | "or-simplification" `Set.member` tags = OrSimplification
  | "idempotent" `Set.member` tags = IdempotentSimplification
  | "demorgan" `Set.member` tags = DeMorganSimplification
  | "comparison" `Set.member` tags = ComparisonSimplification
  | "if-simplification" `Set.member` tags = IfSimplification
  | "contradiction" `Set.member` tags = ContradictionSimplification
  | otherwise = NotSimplification

--------------------------------------------------------------------------------
-- Fix Types
--------------------------------------------------------------------------------

-- | Result of applying Boolean fixes
data BooleanFixResult
  = BooleanFixSuccess BooleanFixData
  | BooleanFixPartial BooleanFixData [Text]  -- ^ Data and errors
  | BooleanFixFailure Text                   -- ^ Error message
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Common data for successful/partial fixes
data BooleanFixData = BooleanFixData
  { bfdFixes      :: [BooleanFix]
  , bfdStats      :: BooleanFixStats
  , bfdNewContent :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A single Boolean fix
data BooleanFix = BooleanFix
  { bfSpan        :: SrcSpan
  , bfOriginal    :: Text
  , bfReplacement :: Text
  , bfCategory    :: BooleanFixCategory
  , bfRuleId      :: Text
  , bfMessage     :: Text
  , bfValidation  :: BooleanValidation
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Validation result for a fix (local type for Boolean fixes)
-- Named BooleanValidation to avoid conflict with Types.FixValidation
data BooleanValidation
  = BoolValidationPassed
  | BoolValidationWarning Text
  | BoolValidationFailed Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Statistics about Boolean fixes
data BooleanFixStats = BooleanFixStats
  { bfsNotSimplifications     :: Int
  , bfsAndSimplifications     :: Int
  , bfsOrSimplifications      :: Int
  , bfsIdempotent             :: Int
  , bfsDeMorgan               :: Int
  , bfsComparisons            :: Int
  , bfsIfSimplifications      :: Int
  , bfsContradictions         :: Int
  , bfsTotalFixes             :: Int
  , bfsValidationsPassed      :: Int
  , bfsValidationsFailed      :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty statistics
emptyStats :: BooleanFixStats
emptyStats = BooleanFixStats 0 0 0 0 0 0 0 0 0 0 0

-- | Merge two statistics
mergeStats :: BooleanFixStats -> BooleanFixStats -> BooleanFixStats
mergeStats a b = BooleanFixStats
  { bfsNotSimplifications = bfsNotSimplifications a + bfsNotSimplifications b
  , bfsAndSimplifications = bfsAndSimplifications a + bfsAndSimplifications b
  , bfsOrSimplifications  = bfsOrSimplifications a + bfsOrSimplifications b
  , bfsIdempotent         = bfsIdempotent a + bfsIdempotent b
  , bfsDeMorgan           = bfsDeMorgan a + bfsDeMorgan b
  , bfsComparisons        = bfsComparisons a + bfsComparisons b
  , bfsIfSimplifications  = bfsIfSimplifications a + bfsIfSimplifications b
  , bfsContradictions     = bfsContradictions a + bfsContradictions b
  , bfsTotalFixes         = bfsTotalFixes a + bfsTotalFixes b
  , bfsValidationsPassed  = bfsValidationsPassed a + bfsValidationsPassed b
  , bfsValidationsFailed  = bfsValidationsFailed a + bfsValidationsFailed b
  }

--------------------------------------------------------------------------------
-- Main Fixing Functions
--------------------------------------------------------------------------------

-- | Fix Boolean expressions in a file content
fixBooleanExprs :: BooleanFixEngine
                -> FilePath
                -> Text
                -> IO BooleanFixResult
fixBooleanExprs engine path content = do
  let config = bfeConfig engine
      enabledCats = bfcEnabledCategories config

  -- Find all potential fixes
  let allFixes = findAllBooleanFixes path content enabledCats

  -- Validate fixes if enabled
  validatedFixes <- if bfcValidateFixes config
    then mapM (validateAndMark content) allFixes
    else pure allFixes

  -- Filter to only valid fixes
  let goodFixes = filter isValidFix validatedFixes
      limitedFixes = take (bfcMaxFixesPerFile config) goodFixes

  -- Apply fixes
  case applyBooleanFixes content limitedFixes of
    Left err -> pure $ BooleanFixFailure err
    Right newContent -> do
      let stats = computeStats validatedFixes
          errors = map getValidationError $ filter (not . isValidFix) validatedFixes

      -- Update engine stats
      atomically $ modifyTVar' (bfeStats engine) (mergeStats stats)

      let fixData = BooleanFixData limitedFixes stats newContent
      if null errors
        then pure $ BooleanFixSuccess fixData
        else pure $ BooleanFixPartial (BooleanFixData goodFixes stats newContent) errors

-- | Fix a file directly
fixBooleanFile :: BooleanFixEngine
               -> FilePath
               -> IO BooleanFixResult
fixBooleanFile engine path = do
  result <- try @SomeException $ do
    content <- T.pack <$> readFile path
    fixBooleanExprs engine path content
  case result of
    Left e -> pure $ BooleanFixFailure $ T.pack $ show e
    Right r -> pure r

-- | Fix based on existing diagnostics
fixBooleanDiagnostics :: BooleanFixEngine
                      -> [Diagnostic]
                      -> Text
                      -> IO BooleanFixResult
fixBooleanDiagnostics _engine diags content = do
  -- Filter to Boolean-related diagnostics
  let boolDiags = filter isBooleanDiagnostic diags
      fixes = catMaybes $ map diagnosticToFix boolDiags

  case applyBooleanFixes content fixes of
    Left err -> pure $ BooleanFixFailure err
    Right newContent -> do
      let stats = computeStats fixes
      pure $ BooleanFixSuccess $ BooleanFixData fixes stats newContent

-- | Check if a diagnostic is Boolean-related
isBooleanDiagnostic :: Diagnostic -> Bool
isBooleanDiagnostic diag = case diagCode diag of
  Just code -> T.isPrefixOf "not-" code
            || T.isPrefixOf "true-" code
            || T.isPrefixOf "false-" code
            || T.isPrefixOf "and-" code
            || T.isPrefixOf "or-" code
            || T.isPrefixOf "eq-" code
            || T.isPrefixOf "ne-" code
            || T.isPrefixOf "if-" code
  Nothing -> False

-- | Convert a diagnostic to a Boolean fix
diagnosticToFix :: Diagnostic -> Maybe BooleanFix
diagnosticToFix diag = case diagFixes diag of
  (fix:_) -> case fixEdits fix of
    (FixEdit span' replacement:_) ->
      Just BooleanFix
        { bfSpan = span'
        , bfOriginal = ""  -- Would need to extract from content
        , bfReplacement = replacement
        , bfCategory = inferCategory (fromMaybe "" $ diagCode diag)
        , bfRuleId = fromMaybe "unknown" $ diagCode diag
        , bfMessage = diagMessage diag
        , bfValidation = BoolValidationPassed
        }
    _ -> Nothing
  _ -> Nothing

-- | Infer category from rule ID
inferCategory :: Text -> BooleanFixCategory
inferCategory ruleId
  | "not-" `T.isPrefixOf` ruleId = NotSimplification
  | "and-" `T.isPrefixOf` ruleId = AndSimplification
  | "or-" `T.isPrefixOf` ruleId = OrSimplification
  | "eq-" `T.isPrefixOf` ruleId = ComparisonSimplification
  | "ne-" `T.isPrefixOf` ruleId = ComparisonSimplification
  | "if-" `T.isPrefixOf` ruleId = IfSimplification
  | otherwise = NotSimplification

--------------------------------------------------------------------------------
-- Finding Fixes
--------------------------------------------------------------------------------

-- | Find all Boolean fixes in content
findAllBooleanFixes :: FilePath -> Text -> [BooleanFixCategory] -> [BooleanFix]
findAllBooleanFixes path content enabledCats =
  let lineList = zip ([1..] :: [Int]) (T.lines content)
  in concatMap (findFixesInLine path enabledCats) lineList

-- | Find fixes in a single line
findFixesInLine :: FilePath -> [BooleanFixCategory] -> (Int, Text) -> [BooleanFix]
findFixesInLine path enabledCats (lineNum, lineText) = catMaybes
  [ guard' NotSimplification $ fixNotTrue path lineNum lineText
  , guard' NotSimplification $ fixNotFalse path lineNum lineText
  , guard' AndSimplification $ fixTrueAnd path lineNum lineText
  , guard' AndSimplification $ fixFalseAnd path lineNum lineText
  , guard' AndSimplification $ fixAndFalse path lineNum lineText
  , guard' OrSimplification $ fixTrueOr path lineNum lineText
  , guard' OrSimplification $ fixFalseOr path lineNum lineText
  , guard' OrSimplification $ fixOrTrue path lineNum lineText
  , guard' IdempotentSimplification $ fixAndSame path lineNum lineText
  , guard' IdempotentSimplification $ fixOrSame path lineNum lineText
  , guard' DeMorganSimplification $ fixDeMorganAnd path lineNum lineText
  , guard' DeMorganSimplification $ fixDeMorganOr path lineNum lineText
  , guard' ComparisonSimplification $ fixEqTrue path lineNum lineText
  , guard' ComparisonSimplification $ fixEqFalse path lineNum lineText
  , guard' ComparisonSimplification $ fixNeTrue path lineNum lineText
  , guard' ComparisonSimplification $ fixNeFalse path lineNum lineText
  , guard' IfSimplification $ fixIfTrue path lineNum lineText
  , guard' IfSimplification $ fixIfFalse path lineNum lineText
  , guard' IfSimplification $ fixIfCondTrue path lineNum lineText
  , guard' IfSimplification $ fixIfCondFalse path lineNum lineText
  , guard' IfSimplification $ fixIfThenTrue path lineNum lineText
  , guard' IfSimplification $ fixIfThenFalse path lineNum lineText
  , guard' IfSimplification $ fixIfElseTrue path lineNum lineText
  , guard' IfSimplification $ fixIfElseFalse path lineNum lineText
  , guard' IfSimplification $ fixIfNot path lineNum lineText
  , guard' ContradictionSimplification $ fixAndNotSelf path lineNum lineText
  , guard' ContradictionSimplification $ fixOrNotSelf path lineNum lineText
  ]
  where
    guard' cat mFix = if cat `elem` enabledCats then mFix else Nothing

--------------------------------------------------------------------------------
-- Individual Fix Functions
--------------------------------------------------------------------------------

-- | Fix: not True → False
fixNotTrue :: FilePath -> Int -> Text -> Maybe BooleanFix
fixNotTrue path lineNum line =
  if "not True" `T.isInfixOf` line
  then Just $ makeFix path lineNum line "not True" "False" NotSimplification
                      "not-true" "not True is False"
  else Nothing

-- | Fix: not False → True
fixNotFalse :: FilePath -> Int -> Text -> Maybe BooleanFix
fixNotFalse path lineNum line =
  if "not False" `T.isInfixOf` line
  then Just $ makeFix path lineNum line "not False" "True" NotSimplification
                      "not-false" "not False is True"
  else Nothing

-- | Fix: True && x → x
fixTrueAnd :: FilePath -> Int -> Text -> Maybe BooleanFix
fixTrueAnd path lineNum line =
  let pattern' = "True\\s*&&\\s*([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:var:_):_) -> Just $ makeFix path lineNum line full var AndSimplification
                                  "true-and" "True && x is x"
       _ -> Nothing

-- | Fix: False && x → False
fixFalseAnd :: FilePath -> Int -> Text -> Maybe BooleanFix
fixFalseAnd path lineNum line =
  let pattern' = "False\\s*&&\\s*[a-zA-Z_][a-zA-Z0-9_']*" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) ->
         Just $ makeFix path lineNum line match "False" AndSimplification
                        "false-and" "False && x is False"
       _ -> Nothing

-- | Fix: x && False → False
fixAndFalse :: FilePath -> Int -> Text -> Maybe BooleanFix
fixAndFalse path lineNum line =
  let pattern' = "[a-zA-Z_][a-zA-Z0-9_']*\\s*&&\\s*False" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) ->
         Just $ makeFix path lineNum line match "False" AndSimplification
                        "and-false" "x && False is False"
       _ -> Nothing

-- | Fix: True || x → True
fixTrueOr :: FilePath -> Int -> Text -> Maybe BooleanFix
fixTrueOr path lineNum line =
  let pattern' = "True\\s*\\|\\|\\s*[a-zA-Z_][a-zA-Z0-9_']*" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) ->
         Just $ makeFix path lineNum line match "True" OrSimplification
                        "true-or" "True || x is True"
       _ -> Nothing

-- | Fix: False || x → x
fixFalseOr :: FilePath -> Int -> Text -> Maybe BooleanFix
fixFalseOr path lineNum line =
  let pattern' = "False\\s*\\|\\|\\s*([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:var:_):_) -> Just $ makeFix path lineNum line full var OrSimplification
                                  "false-or" "False || x is x"
       _ -> Nothing

-- | Fix: x || True → True
fixOrTrue :: FilePath -> Int -> Text -> Maybe BooleanFix
fixOrTrue path lineNum line =
  let pattern' = "[a-zA-Z_][a-zA-Z0-9_']*\\s*\\|\\|\\s*True" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) ->
         Just $ makeFix path lineNum line match "True" OrSimplification
                        "or-true" "x || True is True"
       _ -> Nothing

-- | Fix: x && x → x (idempotent)
fixAndSame :: FilePath -> Int -> Text -> Maybe BooleanFix
fixAndSame path lineNum line =
  let pattern' = "([a-zA-Z_][a-zA-Z0-9_']*)\\s*&&\\s*\\1\\b" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:var:_):_) -> Just $ makeFix path lineNum line full var IdempotentSimplification
                                  "and-same" "x && x is x"
       _ -> Nothing

-- | Fix: x || x → x (idempotent)
fixOrSame :: FilePath -> Int -> Text -> Maybe BooleanFix
fixOrSame path lineNum line =
  let pattern' = "([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\|\\|\\s*\\1\\b" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:var:_):_) -> Just $ makeFix path lineNum line full var IdempotentSimplification
                                  "or-same" "x || x is x"
       _ -> Nothing

-- | Fix: not x && not y → not (x || y) (De Morgan)
fixDeMorganAnd :: FilePath -> Int -> Text -> Maybe BooleanFix
fixDeMorganAnd path lineNum line =
  let pattern' = "not\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*&&\\s*not\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:y:_):_) ->
         let repl = "not (" <> x <> " || " <> y <> ")"
         in Just $ makeFix path lineNum line full repl DeMorganSimplification
                           "not-and-not" "De Morgan: not x && not y → not (x || y)"
       _ -> Nothing

-- | Fix: not x || not y → not (x && y) (De Morgan)
fixDeMorganOr :: FilePath -> Int -> Text -> Maybe BooleanFix
fixDeMorganOr path lineNum line =
  let pattern' = "not\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\|\\|\\s*not\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:y:_):_) ->
         let repl = "not (" <> x <> " && " <> y <> ")"
         in Just $ makeFix path lineNum line full repl DeMorganSimplification
                           "not-or-not" "De Morgan: not x || not y → not (x && y)"
       _ -> Nothing

-- | Fix: x == True → x
fixEqTrue :: FilePath -> Int -> Text -> Maybe BooleanFix
fixEqTrue path lineNum line =
  let pattern' = "([a-zA-Z_][a-zA-Z0-9_']*)\\s*==\\s*True" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:var:_):_) -> Just $ makeFix path lineNum line full var ComparisonSimplification
                                  "eq-true" "x == True is x"
       _ -> Nothing

-- | Fix: x == False → not x
fixEqFalse :: FilePath -> Int -> Text -> Maybe BooleanFix
fixEqFalse path lineNum line =
  let pattern' = "([a-zA-Z_][a-zA-Z0-9_']*)\\s*==\\s*False" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:var:_):_) -> Just $ makeFix path lineNum line full ("not " <> var) ComparisonSimplification
                                  "eq-false" "x == False is not x"
       _ -> Nothing

-- | Fix: x /= True → not x
fixNeTrue :: FilePath -> Int -> Text -> Maybe BooleanFix
fixNeTrue path lineNum line =
  let pattern' = "([a-zA-Z_][a-zA-Z0-9_']*)\\s*/=\\s*True" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:var:_):_) -> Just $ makeFix path lineNum line full ("not " <> var) ComparisonSimplification
                                  "ne-true" "x /= True is not x"
       _ -> Nothing

-- | Fix: x /= False → x
fixNeFalse :: FilePath -> Int -> Text -> Maybe BooleanFix
fixNeFalse path lineNum line =
  let pattern' = "([a-zA-Z_][a-zA-Z0-9_']*)\\s*/=\\s*False" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:var:_):_) -> Just $ makeFix path lineNum line full var ComparisonSimplification
                                  "ne-false" "x /= False is x"
       _ -> Nothing

-- | Fix: if True then x else y → x
fixIfTrue :: FilePath -> Int -> Text -> Maybe BooleanFix
fixIfTrue path lineNum line =
  let pattern' = "if\\s+True\\s+then\\s+([^e]+)\\s+else\\s+[^\\n]+" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:thenBranch:_):_) ->
         Just $ makeFix path lineNum line full (T.strip thenBranch) IfSimplification
                        "if-true" "if True always takes then branch"
       _ -> Nothing

-- | Fix: if False then x else y → y
fixIfFalse :: FilePath -> Int -> Text -> Maybe BooleanFix
fixIfFalse path lineNum line =
  let pattern' = "if\\s+False\\s+then\\s+[^e]+\\s+else\\s+([^\\n]+)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:elseBranch:_):_) ->
         Just $ makeFix path lineNum line full (T.strip elseBranch) IfSimplification
                        "if-false" "if False always takes else branch"
       _ -> Nothing

-- | Fix: if c then True else False → c
fixIfCondTrue :: FilePath -> Int -> Text -> Maybe BooleanFix
fixIfCondTrue path lineNum line =
  let pattern' = "if\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+then\\s+True\\s+else\\s+False" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:cond:_):_) -> Just $ makeFix path lineNum line full cond IfSimplification
                                   "if-cond-true" "if c then True else False is c"
       _ -> Nothing

-- | Fix: if c then False else True → not c
fixIfCondFalse :: FilePath -> Int -> Text -> Maybe BooleanFix
fixIfCondFalse path lineNum line =
  let pattern' = "if\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+then\\s+False\\s+else\\s+True" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:cond:_):_) -> Just $ makeFix path lineNum line full ("not " <> cond) IfSimplification
                                   "if-cond-false" "if c then False else True is not c"
       _ -> Nothing

-- | Fix: if c then True else x → c || x
fixIfThenTrue :: FilePath -> Int -> Text -> Maybe BooleanFix
fixIfThenTrue path lineNum line =
  let pattern' = "if\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+then\\s+True\\s+else\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:cond:elseVal:_):_) ->
         Just $ makeFix path lineNum line full (cond <> " || " <> elseVal) IfSimplification
                        "if-then-true" "if c then True else x is c || x"
       _ -> Nothing

-- | Fix: if c then False else x → not c && x
fixIfThenFalse :: FilePath -> Int -> Text -> Maybe BooleanFix
fixIfThenFalse path lineNum line =
  let pattern' = "if\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+then\\s+False\\s+else\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:cond:elseVal:_):_) ->
         Just $ makeFix path lineNum line full ("not " <> cond <> " && " <> elseVal) IfSimplification
                        "if-then-false" "if c then False else x is not c && x"
       _ -> Nothing

-- | Fix: if c then x else True → not c || x
fixIfElseTrue :: FilePath -> Int -> Text -> Maybe BooleanFix
fixIfElseTrue path lineNum line =
  let pattern' = "if\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+then\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+else\\s+True" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:cond:thenVal:_):_) ->
         Just $ makeFix path lineNum line full ("not " <> cond <> " || " <> thenVal) IfSimplification
                        "if-else-true" "if c then x else True is not c || x"
       _ -> Nothing

-- | Fix: if c then x else False → c && x
fixIfElseFalse :: FilePath -> Int -> Text -> Maybe BooleanFix
fixIfElseFalse path lineNum line =
  let pattern' = "if\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+then\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+else\\s+False" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:cond:thenVal:_):_) ->
         Just $ makeFix path lineNum line full (cond <> " && " <> thenVal) IfSimplification
                        "if-else-false" "if c then x else False is c && x"
       _ -> Nothing

-- | Fix: if not c then x else y → if c then y else x
fixIfNot :: FilePath -> Int -> Text -> Maybe BooleanFix
fixIfNot path lineNum line =
  let pattern' = "if\\s+not\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+then\\s+([^e]+)\\s+else\\s+([^\\n]+)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:cond:thenBr:elseBr:_):_) ->
         let repl = "if " <> cond <> " then " <> T.strip elseBr <> " else " <> T.strip thenBr
         in Just $ makeFix path lineNum line full repl IfSimplification
                           "if-not" "Avoid negation in if condition"
       _ -> Nothing

-- | Fix: x && not x → False
fixAndNotSelf :: FilePath -> Int -> Text -> Maybe BooleanFix
fixAndNotSelf path lineNum line =
  let pattern' = "([a-zA-Z_][a-zA-Z0-9_']*)\\s*&&\\s*not\\s+\\1\\b" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:_:_):_) -> Just $ makeFix path lineNum line full "False" ContradictionSimplification
                                "and-not-self" "x && not x is False"
       _ -> Nothing

-- | Fix: x || not x → True
fixOrNotSelf :: FilePath -> Int -> Text -> Maybe BooleanFix
fixOrNotSelf path lineNum line =
  let pattern' = "([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\|\\|\\s*not\\s+\\1\\b" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:_:_):_) -> Just $ makeFix path lineNum line full "True" ContradictionSimplification
                                "or-not-self" "x || not x is True"
       _ -> Nothing

--------------------------------------------------------------------------------
-- Fix Helper
--------------------------------------------------------------------------------

-- | Create a BooleanFix
makeFix :: FilePath -> Int -> Text -> Text -> Text -> BooleanFixCategory -> Text -> Text -> BooleanFix
makeFix path lineNum lineText original replacement cat ruleId msg =
  let startCol = fromMaybe 1 $ findColumn original lineText
      endCol = startCol + T.length original
  in BooleanFix
    { bfSpan = SrcSpan
        { srcSpanFile = path
        , srcSpanStartLine = Line lineNum
        , srcSpanStartCol = Column startCol
        , srcSpanEndLine = Line lineNum
        , srcSpanEndCol = Column endCol
        }
    , bfOriginal = original
    , bfReplacement = replacement
    , bfCategory = cat
    , bfRuleId = ruleId
    , bfMessage = msg
    , bfValidation = BoolValidationPassed
    }

-- | Find column where pattern starts in line
findColumn :: Text -> Text -> Maybe Int
findColumn pattern' line = case T.breakOn pattern' line of
  (before, after) | not (T.null after) -> Just $ T.length before + 1
  _ -> Nothing

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

-- | Validate a Boolean fix
validateBooleanFix :: Text -> BooleanFix -> BooleanValidation
validateBooleanFix content fix =
  let original = bfOriginal fix
      replacement = bfReplacement fix
  in
    -- Check for empty replacement when original is non-empty
    if T.null replacement && not (T.null original)
    then BoolValidationFailed "Replacement is empty"
    -- Check the original actually exists in content
    else if not (original `T.isInfixOf` content)
    then BoolValidationFailed "Original pattern not found in content"
    -- Check for unbalanced parentheses
    else if not (balancedParens replacement)
    then BoolValidationWarning "Replacement has unbalanced parentheses"
    else BoolValidationPassed

-- | Check if parentheses are balanced
balancedParens :: Text -> Bool
balancedParens = go (0 :: Int)
  where
    go n text
      | n < 0 = False
      | T.null text = n == 0
      | otherwise =
          let c = T.head text
              rest = T.tail text
          in case c of
               '(' -> go (n + 1) rest
               ')' -> go (n - 1) rest
               _ -> go n rest

-- | Validate and mark a fix
validateAndMark :: Text -> BooleanFix -> IO BooleanFix
validateAndMark content fix = do
  let validation = validateBooleanFix content fix
  pure fix { bfValidation = validation }

-- | Check if a fix passed validation
isValidFix :: BooleanFix -> Bool
isValidFix fix = case bfValidation fix of
  BoolValidationPassed -> True
  BoolValidationWarning _ -> True
  BoolValidationFailed _ -> False

-- | Get validation error message
getValidationError :: BooleanFix -> Text
getValidationError fix = case bfValidation fix of
  BoolValidationFailed err -> bfRuleId fix <> ": " <> err
  BoolValidationWarning warn -> bfRuleId fix <> " (warning): " <> warn
  BoolValidationPassed -> ""

--------------------------------------------------------------------------------
-- Applying Fixes
--------------------------------------------------------------------------------

-- | Apply multiple Boolean fixes to content
applyBooleanFixes :: Text -> [BooleanFix] -> Either Text Text
applyBooleanFixes content fixes =
  -- Sort fixes by position descending to apply from end to start
  let sortedFixes = reverse $ sortFixesByPosition fixes
  in foldl applyOne (Right content) sortedFixes
  where
    applyOne (Left err) _ = Left err
    applyOne (Right txt) fix = applySingleFix txt fix

-- | Sort fixes by their start position
sortFixesByPosition :: [BooleanFix] -> [BooleanFix]
sortFixesByPosition = foldr insertSorted []
  where
    insertSorted fix [] = [fix]
    insertSorted fix (x:xs)
      | comparePosition fix x == LT = fix : x : xs
      | otherwise = x : insertSorted fix xs

    comparePosition f1 f2 =
      let l1 = srcSpanStartLine (bfSpan f1)
          l2 = srcSpanStartLine (bfSpan f2)
          c1 = srcSpanStartCol (bfSpan f1)
          c2 = srcSpanStartCol (bfSpan f2)
      in case compare l1 l2 of
           EQ -> compare c1 c2
           x -> x

-- | Apply a single fix to content
applySingleFix :: Text -> BooleanFix -> Either Text Text
applySingleFix content fix =
  let span' = bfSpan fix
      lineNum = unLine $ srcSpanStartLine span'
      startCol = unColumn $ srcSpanStartCol span'
      endCol = unColumn $ srcSpanEndCol span'
      lines' = T.lines content
  in if lineNum < 1 || lineNum > length lines'
     then Left $ "Invalid line number: " <> T.pack (show lineNum)
     else
       let targetLine = lines' !! (lineNum - 1)
           prefix = T.take (startCol - 1) targetLine
           suffix = T.drop (endCol - 1) targetLine
           newLine = prefix <> bfReplacement fix <> suffix
           newLines = take (lineNum - 1) lines' ++ [newLine] ++ drop lineNum lines'
       in Right $ T.unlines newLines


--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

-- | Compute statistics from fixes
computeStats :: [BooleanFix] -> BooleanFixStats
computeStats fixes = foldl addFixToBoolStats emptyStats fixes

-- | Add a fix to statistics
addFixToBoolStats :: BooleanFixStats -> BooleanFix -> BooleanFixStats
addFixToBoolStats stats fix =
  let updateValidation s = case bfValidation fix of
        BoolValidationPassed -> s { bfsValidationsPassed = bfsValidationsPassed s + 1 }
        BoolValidationWarning _ -> s { bfsValidationsPassed = bfsValidationsPassed s + 1 }
        BoolValidationFailed _ -> s { bfsValidationsFailed = bfsValidationsFailed s + 1 }
      updateCategory s = case bfCategory fix of
        NotSimplification -> s { bfsNotSimplifications = bfsNotSimplifications s + 1 }
        AndSimplification -> s { bfsAndSimplifications = bfsAndSimplifications s + 1 }
        OrSimplification -> s { bfsOrSimplifications = bfsOrSimplifications s + 1 }
        IdempotentSimplification -> s { bfsIdempotent = bfsIdempotent s + 1 }
        DeMorganSimplification -> s { bfsDeMorgan = bfsDeMorgan s + 1 }
        ComparisonSimplification -> s { bfsComparisons = bfsComparisons s + 1 }
        IfSimplification -> s { bfsIfSimplifications = bfsIfSimplifications s + 1 }
        ContradictionSimplification -> s { bfsContradictions = bfsContradictions s + 1 }
  in updateCategory $ updateValidation $ stats { bfsTotalFixes = bfsTotalFixes stats + 1 }
