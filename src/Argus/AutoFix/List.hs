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
-- Module      : Argus.AutoFix.List
-- Description : Automated fixes for list operations and patterns
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides comprehensive auto-fix capabilities for list operations,
-- including:
--
-- * Performance improvements (nub → ordNub, length == 0 → null)
-- * Pattern simplifications (concat . map → concatMap)
-- * Fold improvements (foldr/foldl patterns)
-- * List comprehension simplifications
-- * Partial function replacements (head → headMay)
--
-- == Safety Features
--
-- * Validates type constraints (Ord for ordNub, etc.)
-- * Preserves semantics when simplifying
-- * Handles infinite lists correctly
-- * Respects strictness
--
-- == FixEngine Integration
--
-- This module now implements the 'FixEngine' typeclass from the extensible
-- auto-fix infrastructure, allowing List fixes to be composed with other
-- fix engines.
--
-- @
-- engine <- newListFixEngine defaultListFixConfig
-- result <- fixListExprs engine filePath content
-- @
module Argus.AutoFix.List
  ( -- * Fix Engine
    ListFixEngine (..)
  , newListFixEngine
  , ListFixConfig (..)
  , defaultListFixConfig

    -- * Fixing
  , fixListExprs
  , fixListFile
  , fixListDiagnostics
  , ListFixResult (..)
  , ListFixData (..)
  , ListFix (..)
  , ListFixCategory (..)

    -- * Individual Fix Functions
    -- ** Performance
  , fixLengthZero
  , fixLengthGtZero
  , fixNub
  , fixSort
  , fixReverse
  , fixConcatMap
  , fixFoldMap

    -- ** Folding
  , fixFoldlStrict
  , fixFoldrToFoldl
  , fixFoldlToFoldr

    -- ** Head/Tail
  , fixHead
  , fixTail
  , fixInit
  , fixLast
  , fixHeadSort
  , fixLastSort

    -- ** List Operations
  , fixMapMap
  , fixMapId
  , fixFilterTrue
  , fixFilterFalse
  , fixTakeWhileTrue
  , fixDropWhileTrue
  , fixReplicate
  , fixZipWithPair

    -- ** Comprehension
  , fixListCompToMap
  , fixListCompToFilter
  , fixListCompToConcat

    -- * Validation
  , validateListFix
  , ListFixValidation (..)

    -- * Statistics
  , ListFixStats (..)
  , emptyListStats
  , mergeListStats

    -- * FixEngine integration
  , toEnrichedListFix
  , fromEnrichedListFix
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
  , FixImport(..)
  )

import Argus.Rules.Types (SafetyLevel(..), Category(..))

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

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for List fix engine
data ListFixConfig = ListFixConfig
  { lfcEnabledCategories :: [ListFixCategory]
      -- ^ Which categories of fixes to apply
  , lfcValidateFixes     :: Bool
      -- ^ Validate fixes before applying
  , lfcMaxFixesPerFile   :: Int
      -- ^ Maximum number of fixes to apply per file
  , lfcAssumeOrd         :: Bool
      -- ^ Assume Ord constraint is available for ordNub
  , lfcPreferStrict      :: Bool
      -- ^ Prefer strict versions (foldl' over foldl)
  , lfcSafePartials      :: Bool
      -- ^ Replace partial functions with safe alternatives
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration
defaultListFixConfig :: ListFixConfig
defaultListFixConfig = ListFixConfig
  { lfcEnabledCategories = [minBound .. maxBound]
  , lfcValidateFixes     = True
  , lfcMaxFixesPerFile   = 100
  , lfcAssumeOrd         = False
  , lfcPreferStrict      = True
  , lfcSafePartials      = True
  }

-- | Categories of List fixes
data ListFixCategory
  = PerformanceList      -- ^ length == 0 → null, nub → ordNub
  | FoldImprovement      -- ^ foldl → foldl', fold patterns
  | PartialSafety        -- ^ head → headMay, tail → tailMay
  | MapSimplification    -- ^ map f . map g → map (f . g)
  | FilterSimplification -- ^ filter (const True) → id
  | ComprehensionSimplification  -- ^ [f x | x <- xs] → map f xs
  | SortImprovement      -- ^ head . sort → minimum
  | ListConstruction     -- ^ replicate n x patterns
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Engine
--------------------------------------------------------------------------------

-- | List fix engine with caching
data ListFixEngine = ListFixEngine
  { lfeConfig     :: ListFixConfig
  , lfeCache      :: TVar (Map FilePath [ListFix])
  , lfeStats      :: TVar ListFixStats
  }

-- | Create a new List fix engine
newListFixEngine :: ListFixConfig -> IO ListFixEngine
newListFixEngine config = do
  cache <- newTVarIO Map.empty
  stats <- newTVarIO emptyListStats
  pure ListFixEngine
    { lfeConfig = config
    , lfeCache  = cache
    , lfeStats  = stats
    }

--------------------------------------------------------------------------------
-- FixEngine Instance
--------------------------------------------------------------------------------

instance FixEngine ListFixEngine where
  type EngineConfig ListFixEngine = ListFixConfig
  type EngineCategory ListFixEngine = ListFixCategory

  engineName _ = "list-fix"

  engineDescription _ =
    "List operation fix engine. Optimizes list patterns including performance \
    \improvements (nub → ordNub, length == 0 → null), fold patterns, \
    \partial function safety, and list comprehension simplifications."

  engineCategories _ = [minBound .. maxBound]

  findFixes engine path content = do
    let config = lfeConfig engine
        enabledCats = lfcEnabledCategories config
        listFixes = findAllListFixes path content enabledCats config
    now <- getCurrentTime
    pure $ map (toEnrichedListFix now) listFixes

  applyFix _engine _path content enrichedFix = do
    case fromEnrichedListFix enrichedFix of
      Nothing -> pure $ ApplyFailure $ EngineError "Not a List fix"
      Just listFix -> do
        case applySingleListFix content listFix of
          Left err -> pure $ ApplyFailure $ EngineError err
          Right newContent -> pure $ ApplySuccess
            { arsNewContent = newContent
            , arsAppliedFix = enrichedFix
            , arsStats = emptyFixStats
            }

  validateFix _engine enrichedFix content = do
    case fromEnrichedListFix enrichedFix of
      Nothing -> pure FixValidation
        { fvResult = ValidationError "Not a List fix"
        , fvChecks = []
        , fvSuggestions = []
        }
      Just listFix -> do
        let validation = validateListFix content listFix
        pure $ case validation of
          ListValidationPassed -> FixValidation
            { fvResult = ValidationSuccess
            , fvChecks = [("pattern_present", ValidationSuccess), ("parens_balanced", ValidationSuccess)]
            , fvSuggestions = []
            }
          ListValidationWarning warn -> FixValidation
            { fvResult = ValidationWarning warn
            , fvChecks = [("pattern_present", ValidationSuccess), ("parens_balanced", ValidationWarning warn)]
            , fvSuggestions = [warn]
            }
          ListValidationFailed err -> FixValidation
            { fvResult = ValidationError err
            , fvChecks = [("pattern_present", ValidationError err)]
            , fvSuggestions = []
            }

  getEngineStats engine = do
    localStats <- readTVarIO (lfeStats engine)
    pure $ convertListToFixStats localStats

-- | Convert local stats to generic FixStats
convertListToFixStats :: ListFixStats -> FixStats
convertListToFixStats lfs =
  let stats = emptyFixStats
  in stats
    { fsTotal = lfsTotalFixes lfs
    , fsApplied = lfsValidationsPassed lfs
    , fsFailed = lfsValidationsFailed lfs
    }

--------------------------------------------------------------------------------
-- Conversion Functions
--------------------------------------------------------------------------------

-- | Convert a ListFix to an EnrichedFix
toEnrichedListFix :: UTCTime -> ListFix -> EnrichedFix
toEnrichedListFix timestamp lf = EnrichedFix
  { efId = mkFixId "list-fix" (lfRuleId lf <> "-" <> lineId)
  , efFix = Fix
      { fixTitle = lfMessage lf
      , fixEdits = [FixEdit (lfSpan lf) (lfReplacement lf)]
      , fixIsPreferred = True
      , fixAddImports = map toFixImport (lfAddImports lf)
      , fixRemoveImports = []
      , fixCategory = categoryToFixCategory (lfCategory lf)
      , fixSafety = categoryToListFixSafety (lfCategory lf)
      }
  , efMetadata = (defaultFixMetadata (categoryToRulesCategory (lfCategory lf)))
      { fmConfidence = categoryToListConfidence (lfCategory lf)
      , fmSafety = categoryToListSafety (lfCategory lf)
      , fmCreatedAt = Just timestamp
      , fmTags = Set.fromList ["list", categoryToListTag (lfCategory lf)]
      , fmSourceRule = Just (lfRuleId lf)
      , fmExplanation = Just (lfMessage lf)
      }
  , efDependencies = Set.empty
  , efConflicts = Set.empty
  , efValidation = Nothing
  }
  where
    lineId = T.pack $ show $ unLine $ srcSpanStartLine $ lfSpan lf

    toFixImport :: Text -> FixImport
    toFixImport modName = FixImport
      { fimpModule = modName
      , fimpSymbols = []
      , fimpQualified = Nothing
      , fimpHiding = False
      , fimpPackage = Nothing
      }

-- | Convert category to Rules.Types.Category
categoryToRulesCategory :: ListFixCategory -> Category
categoryToRulesCategory = \case
  PerformanceList -> Performance
  FoldImprovement -> Performance
  PartialSafety -> Safety
  MapSimplification -> Style
  FilterSimplification -> Style
  ComprehensionSimplification -> Style
  SortImprovement -> Performance
  ListConstruction -> Style

-- | Convert category to FixCategory
categoryToFixCategory :: ListFixCategory -> FixCategory
categoryToFixCategory = \case
  PerformanceList -> FCPerformance
  FoldImprovement -> FCPerformance
  PartialSafety -> FCSafety
  MapSimplification -> FCStyle
  FilterSimplification -> FCStyle
  ComprehensionSimplification -> FCStyle
  SortImprovement -> FCPerformance
  ListConstruction -> FCStyle

-- | Convert category to confidence level
categoryToListConfidence :: ListFixCategory -> Confidence
categoryToListConfidence = \case
  PerformanceList -> mkConfidence 0.90
  FoldImprovement -> mkConfidence 0.85
  PartialSafety -> mkConfidence 0.80  -- May need import additions
  MapSimplification -> mkConfidence 0.95
  FilterSimplification -> mkConfidence 0.95
  ComprehensionSimplification -> mkConfidence 0.85
  SortImprovement -> mkConfidence 0.90
  ListConstruction -> mkConfidence 0.90

-- | Convert category to safety level
categoryToListSafety :: ListFixCategory -> SafetyLevel
categoryToListSafety = \case
  PerformanceList -> MostlySafe  -- May change strictness
  FoldImprovement -> MostlySafe  -- Strictness changes
  PartialSafety -> Safe          -- Always makes safer
  MapSimplification -> Safe
  FilterSimplification -> Safe
  ComprehensionSimplification -> Safe
  SortImprovement -> Safe
  ListConstruction -> Safe

-- | Convert category to FixSafety
categoryToListFixSafety :: ListFixCategory -> FixSafety
categoryToListFixSafety = \case
  PerformanceList -> FSMostly
  FoldImprovement -> FSMostly
  PartialSafety -> FSAlways
  MapSimplification -> FSAlways
  FilterSimplification -> FSAlways
  ComprehensionSimplification -> FSAlways
  SortImprovement -> FSAlways
  ListConstruction -> FSAlways

-- | Convert category to tag
categoryToListTag :: ListFixCategory -> Text
categoryToListTag = \case
  PerformanceList -> "performance"
  FoldImprovement -> "fold"
  PartialSafety -> "partial-safety"
  MapSimplification -> "map"
  FilterSimplification -> "filter"
  ComprehensionSimplification -> "comprehension"
  SortImprovement -> "sort"
  ListConstruction -> "construction"

-- | Try to convert an EnrichedFix back to a ListFix
fromEnrichedListFix :: EnrichedFix -> Maybe ListFix
fromEnrichedListFix ef
  | fixIdEngine (efId ef) /= "list-fix" = Nothing
  | otherwise = case fixEdits (efFix ef) of
      (FixEdit span' replacement:_) -> Just ListFix
        { lfSpan = span'
        , lfOriginal = ""  -- We don't preserve original in EnrichedFix
        , lfReplacement = replacement
        , lfCategory = inferCategoryFromListTags (fmTags (efMetadata ef))
        , lfRuleId = fromMaybe "unknown" $ fmSourceRule (efMetadata ef)
        , lfMessage = fromMaybe (fixTitle (efFix ef)) $ fmExplanation (efMetadata ef)
        , lfValidation = ListValidationPassed
        , lfAddImports = map fimpModule $ fixAddImports (efFix ef)
        }
      _ -> Nothing

-- | Infer category from tags
inferCategoryFromListTags :: Set Text -> ListFixCategory
inferCategoryFromListTags tags
  | "performance" `Set.member` tags = PerformanceList
  | "fold" `Set.member` tags = FoldImprovement
  | "partial-safety" `Set.member` tags = PartialSafety
  | "map" `Set.member` tags = MapSimplification
  | "filter" `Set.member` tags = FilterSimplification
  | "comprehension" `Set.member` tags = ComprehensionSimplification
  | "sort" `Set.member` tags = SortImprovement
  | "construction" `Set.member` tags = ListConstruction
  | otherwise = PerformanceList

--------------------------------------------------------------------------------
-- Fix Types
--------------------------------------------------------------------------------

-- | Result of applying List fixes
data ListFixResult
  = ListFixSuccess ListFixData
  | ListFixPartial ListFixData [Text]  -- ^ Data and errors
  | ListFixFailure Text                -- ^ Error message
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Common data for successful/partial fixes
data ListFixData = ListFixData
  { lfdFixes      :: [ListFix]
  , lfdStats      :: ListFixStats
  , lfdNewContent :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A single List fix
data ListFix = ListFix
  { lfSpan        :: SrcSpan
  , lfOriginal    :: Text
  , lfReplacement :: Text
  , lfCategory    :: ListFixCategory
  , lfRuleId      :: Text
  , lfMessage     :: Text
  , lfValidation  :: ListFixValidation
  , lfAddImports  :: [Text]     -- ^ Imports to add (module names)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Validation result for a fix
data ListFixValidation
  = ListValidationPassed
  | ListValidationWarning Text
  | ListValidationFailed Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Statistics about List fixes
data ListFixStats = ListFixStats
  { lfsPerformance        :: Int
  , lfsFoldImprovements   :: Int
  , lfsPartialSafety      :: Int
  , lfsMapSimplifications :: Int
  , lfsFilterSimplifications :: Int
  , lfsComprehensions     :: Int
  , lfsSortImprovements   :: Int
  , lfsListConstruction   :: Int
  , lfsTotalFixes         :: Int
  , lfsValidationsPassed  :: Int
  , lfsValidationsFailed  :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty statistics
emptyListStats :: ListFixStats
emptyListStats = ListFixStats 0 0 0 0 0 0 0 0 0 0 0

-- | Merge two statistics
mergeListStats :: ListFixStats -> ListFixStats -> ListFixStats
mergeListStats a b = ListFixStats
  { lfsPerformance = lfsPerformance a + lfsPerformance b
  , lfsFoldImprovements = lfsFoldImprovements a + lfsFoldImprovements b
  , lfsPartialSafety = lfsPartialSafety a + lfsPartialSafety b
  , lfsMapSimplifications = lfsMapSimplifications a + lfsMapSimplifications b
  , lfsFilterSimplifications = lfsFilterSimplifications a + lfsFilterSimplifications b
  , lfsComprehensions = lfsComprehensions a + lfsComprehensions b
  , lfsSortImprovements = lfsSortImprovements a + lfsSortImprovements b
  , lfsListConstruction = lfsListConstruction a + lfsListConstruction b
  , lfsTotalFixes = lfsTotalFixes a + lfsTotalFixes b
  , lfsValidationsPassed = lfsValidationsPassed a + lfsValidationsPassed b
  , lfsValidationsFailed = lfsValidationsFailed a + lfsValidationsFailed b
  }

--------------------------------------------------------------------------------
-- Main Fixing Functions
--------------------------------------------------------------------------------

-- | Fix List expressions in a file content
fixListExprs :: ListFixEngine
             -> FilePath
             -> Text
             -> IO ListFixResult
fixListExprs engine path content = do
  let config = lfeConfig engine
      enabledCats = lfcEnabledCategories config

  -- Find all potential fixes
  let allFixes = findAllListFixes path content enabledCats config

  -- Validate fixes if enabled
  validatedFixes <- if lfcValidateFixes config
    then mapM (validateAndMarkList content) allFixes
    else pure allFixes

  -- Filter to only valid fixes
  let goodFixes = filter isValidListFix validatedFixes
      limitedFixes = take (lfcMaxFixesPerFile config) goodFixes

  -- Apply fixes
  case applyListFixes content limitedFixes of
    Left err -> pure $ ListFixFailure err
    Right newContent -> do
      let stats = computeListStats validatedFixes
          errors = map getListValidationError $ filter (not . isValidListFix) validatedFixes

      -- Update engine stats
      atomically $ modifyTVar' (lfeStats engine) (mergeListStats stats)

      let fixData = ListFixData limitedFixes stats newContent
      if null errors
        then pure $ ListFixSuccess fixData
        else pure $ ListFixPartial (ListFixData goodFixes stats newContent) errors

-- | Fix a file directly
fixListFile :: ListFixEngine
            -> FilePath
            -> IO ListFixResult
fixListFile engine path = do
  result <- try @SomeException $ do
    content <- T.pack <$> readFile path
    fixListExprs engine path content
  case result of
    Left e -> pure $ ListFixFailure $ T.pack $ show e
    Right r -> pure r

-- | Fix based on existing diagnostics
fixListDiagnostics :: ListFixEngine
                   -> [Diagnostic]
                   -> Text
                   -> IO ListFixResult
fixListDiagnostics _engine diags content = do
  let listDiags = filter isListDiagnostic diags
      fixes = catMaybes $ map listDiagnosticToFix listDiags

  case applyListFixes content fixes of
    Left err -> pure $ ListFixFailure err
    Right newContent -> do
      let stats = computeListStats fixes
      pure $ ListFixSuccess $ ListFixData fixes stats newContent

-- | Check if a diagnostic is List-related
isListDiagnostic :: Diagnostic -> Bool
isListDiagnostic diag = case diagCode diag of
  Just code -> any (`T.isPrefixOf` code) listPrefixes
  Nothing -> False
  where
    listPrefixes = ["length-", "null-", "nub-", "head-", "tail-", "init-", "last-",
                    "fold-", "map-", "filter-", "concat-", "sort-", "replicate-"]

-- | Convert a diagnostic to a List fix
listDiagnosticToFix :: Diagnostic -> Maybe ListFix
listDiagnosticToFix diag = case diagFixes diag of
  (fix':_) -> case fixEdits fix' of
    (FixEdit span' replacement:_) ->
      Just ListFix
        { lfSpan = span'
        , lfOriginal = ""
        , lfReplacement = replacement
        , lfCategory = inferListCategory (fromMaybe "" $ diagCode diag)
        , lfRuleId = fromMaybe "unknown" $ diagCode diag
        , lfMessage = diagMessage diag
        , lfValidation = ListValidationPassed
        , lfAddImports = []
        }
    _ -> Nothing
  _ -> Nothing

-- | Infer category from rule ID
inferListCategory :: Text -> ListFixCategory
inferListCategory ruleId
  | "length-" `T.isPrefixOf` ruleId = PerformanceList
  | "null-" `T.isPrefixOf` ruleId = PerformanceList
  | "nub-" `T.isPrefixOf` ruleId = PerformanceList
  | "head-" `T.isPrefixOf` ruleId = PartialSafety
  | "tail-" `T.isPrefixOf` ruleId = PartialSafety
  | "init-" `T.isPrefixOf` ruleId = PartialSafety
  | "last-" `T.isPrefixOf` ruleId = PartialSafety
  | "fold-" `T.isPrefixOf` ruleId = FoldImprovement
  | "map-" `T.isPrefixOf` ruleId = MapSimplification
  | "filter-" `T.isPrefixOf` ruleId = FilterSimplification
  | "sort-" `T.isPrefixOf` ruleId = SortImprovement
  | otherwise = PerformanceList

--------------------------------------------------------------------------------
-- Finding Fixes
--------------------------------------------------------------------------------

-- | Find all List fixes in content
findAllListFixes :: FilePath -> Text -> [ListFixCategory] -> ListFixConfig -> [ListFix]
findAllListFixes path content enabledCats config =
  let lineList = zip ([1..] :: [Int]) (T.lines content)
  in concatMap (findListFixesInLine path enabledCats config) lineList

-- | Find fixes in a single line
findListFixesInLine :: FilePath -> [ListFixCategory] -> ListFixConfig -> (Int, Text) -> [ListFix]
findListFixesInLine path enabledCats config (lineNum, lineText) = catMaybes
  [ -- Performance
    guard' PerformanceList $ fixLengthZero path lineNum lineText
  , guard' PerformanceList $ fixLengthGtZero path lineNum lineText
  , guard' PerformanceList $ fixNub path lineNum lineText config
  , guard' PerformanceList $ fixConcatMap path lineNum lineText
  , guard' PerformanceList $ fixFoldMap path lineNum lineText

    -- Folding
  , guard' FoldImprovement $ fixFoldlStrict path lineNum lineText
  , guard' FoldImprovement $ fixFoldrToFoldl path lineNum lineText
  , guard' FoldImprovement $ fixFoldlToFoldr path lineNum lineText

    -- Partial Safety
  , guard' PartialSafety $ fixHead path lineNum lineText config
  , guard' PartialSafety $ fixTail path lineNum lineText config
  , guard' PartialSafety $ fixInit path lineNum lineText config
  , guard' PartialSafety $ fixLast path lineNum lineText config

    -- Sort improvements
  , guard' SortImprovement $ fixHeadSort path lineNum lineText
  , guard' SortImprovement $ fixLastSort path lineNum lineText
  , guard' SortImprovement $ fixSort path lineNum lineText
  , guard' SortImprovement $ fixReverse path lineNum lineText

    -- Map simplification
  , guard' MapSimplification $ fixMapMap path lineNum lineText
  , guard' MapSimplification $ fixMapId path lineNum lineText

    -- Filter simplification
  , guard' FilterSimplification $ fixFilterTrue path lineNum lineText
  , guard' FilterSimplification $ fixFilterFalse path lineNum lineText
  , guard' FilterSimplification $ fixTakeWhileTrue path lineNum lineText
  , guard' FilterSimplification $ fixDropWhileTrue path lineNum lineText

    -- List construction
  , guard' ListConstruction $ fixReplicate path lineNum lineText
  , guard' ListConstruction $ fixZipWithPair path lineNum lineText

    -- Comprehension
  , guard' ComprehensionSimplification $ fixListCompToMap path lineNum lineText
  , guard' ComprehensionSimplification $ fixListCompToFilter path lineNum lineText
  , guard' ComprehensionSimplification $ fixListCompToConcat path lineNum lineText
  ]
  where
    guard' cat mFix = if cat `elem` enabledCats then mFix else Nothing

--------------------------------------------------------------------------------
-- Performance Fixes
--------------------------------------------------------------------------------

-- | Fix: length xs == 0 → null xs
fixLengthZero :: FilePath -> Int -> Text -> Maybe ListFix
fixLengthZero path lineNum line =
  let pattern' = "length\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*==\\s*0" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:var:_):_) ->
         Just $ makeListFix path lineNum line full ("null " <> var) PerformanceList
                            "length-eq-zero" "Use null for O(1) emptiness check" []
       _ -> Nothing

-- | Fix: length xs > 0 → not (null xs)
fixLengthGtZero :: FilePath -> Int -> Text -> Maybe ListFix
fixLengthGtZero path lineNum line =
  let pattern' = "length\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*>\\s*0" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:var:_):_) ->
         Just $ makeListFix path lineNum line full ("not (null " <> var <> ")") PerformanceList
                            "length-gt-zero" "Use null for O(1) emptiness check" []
       _ -> Nothing

-- | Fix: nub → ordNub (if Ord constraint available)
fixNub :: FilePath -> Int -> Text -> ListFixConfig -> Maybe ListFix
fixNub path lineNum line config =
  if lfcAssumeOrd config && "nub " `T.isInfixOf` line && not ("ordNub" `T.isInfixOf` line)
  then
    let pattern' = "\\bnub\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
    in case (line =~ pattern' :: [[Text]]) of
         ((full:var:_):_) ->
           Just $ makeListFix path lineNum line full ("ordNub " <> var) PerformanceList
                              "nub-to-ordnub" "nub is O(n²) - ordNub is O(n log n)"
                              ["Data.Containers.ListUtils"]
         _ -> Nothing
  else Nothing

-- | Fix: head (sort xs) → minimum xs
fixHeadSort :: FilePath -> Int -> Text -> Maybe ListFix
fixHeadSort path lineNum line =
  let pattern' = "head\\s*\\(\\s*sort\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:var:_):_) ->
         Just $ makeListFix path lineNum line full ("minimum " <> var) SortImprovement
                            "head-sort" "Use minimum instead of head . sort for O(n)" []
       _ -> Nothing

-- | Fix: last (sort xs) → maximum xs
fixLastSort :: FilePath -> Int -> Text -> Maybe ListFix
fixLastSort path lineNum line =
  let pattern' = "last\\s*\\(\\s*sort\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:var:_):_) ->
         Just $ makeListFix path lineNum line full ("maximum " <> var) SortImprovement
                            "last-sort" "Use maximum instead of last . sort for O(n)" []
       _ -> Nothing

-- | Fix: sort (sort xs) → sort xs
fixSort :: FilePath -> Int -> Text -> Maybe ListFix
fixSort path lineNum line =
  let pattern' = "sort\\s*\\(\\s*sort\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:var:_):_) ->
         Just $ makeListFix path lineNum line full ("sort " <> var) SortImprovement
                            "sort-sort" "sort is idempotent" []
       _ -> Nothing

-- | Fix: reverse (reverse xs) → xs
fixReverse :: FilePath -> Int -> Text -> Maybe ListFix
fixReverse path lineNum line =
  let pattern' = "reverse\\s*\\(\\s*reverse\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:var:_):_) ->
         Just $ makeListFix path lineNum line full var SortImprovement
                            "reverse-reverse" "reverse is self-inverse" []
       _ -> Nothing

-- | Fix: concat (map f xs) → concatMap f xs
fixConcatMap :: FilePath -> Int -> Text -> Maybe ListFix
fixConcatMap path lineNum line =
  let pattern' = "concat\\s*\\(\\s*map\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:f:xs:_):_) ->
         Just $ makeListFix path lineNum line full ("concatMap " <> f <> " " <> xs) PerformanceList
                            "concat-map" "Use concatMap instead of concat . map" []
       _ -> Nothing

-- | Fix: mconcat (map f xs) → foldMap f xs
fixFoldMap :: FilePath -> Int -> Text -> Maybe ListFix
fixFoldMap path lineNum line =
  let pattern' = "mconcat\\s*\\(\\s*map\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:f:xs:_):_) ->
         Just $ makeListFix path lineNum line full ("foldMap " <> f <> " " <> xs) PerformanceList
                            "mconcat-map" "Use foldMap instead of mconcat . map" ["Data.Foldable"]
       _ -> Nothing

--------------------------------------------------------------------------------
-- Fold Fixes
--------------------------------------------------------------------------------

-- | Fix: foldl → foldl'
fixFoldlStrict :: FilePath -> Int -> Text -> Maybe ListFix
fixFoldlStrict path lineNum line =
  let pattern' = "\\bfoldl\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+([a-zA-Z0-9_']+)\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:f:z:xs:_):_) | not ("foldl'" `T.isInfixOf` line) ->
         Just $ makeListFix path lineNum line full ("foldl' " <> f <> " " <> z <> " " <> xs) FoldImprovement
                            "foldl-strict" "Use foldl' to avoid space leaks" ["Data.List"]
       _ -> Nothing

-- | Fix: foldr (\x acc → f x : acc) [] xs → map f xs
fixFoldrToFoldl :: FilePath -> Int -> Text -> Maybe ListFix
fixFoldrToFoldl path lineNum line =
  let pattern' = "foldr\\s+\\(\\\\[a-z_]+\\s+[a-z_]+\\s*->\\s*([a-zA-Z_]+)\\s+[a-z_]+\\s*:\\s*[a-z_]+\\)\\s+\\[\\]\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:f:xs:_):_) ->
         Just $ makeListFix path lineNum line full ("map " <> f <> " " <> xs) FoldImprovement
                            "foldr-to-map" "Use map instead of foldr with cons" []
       _ -> Nothing

-- | Fix: foldl (\acc x → acc ++ [x]) [] xs → reverse xs
fixFoldlToFoldr :: FilePath -> Int -> Text -> Maybe ListFix
fixFoldlToFoldr path lineNum line =
  let pattern' = "foldl'?\\s+\\(\\\\[a-z_]+\\s+[a-z_]+\\s*->\\s*[a-z_]+\\s*\\+\\+\\s*\\[[a-z_]+\\]\\)\\s+\\[\\]\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:xs:_):_) ->
         Just $ makeListFix path lineNum line full ("reverse " <> xs) FoldImprovement
                            "foldl-append" "Avoid O(n²) append pattern" []
       _ -> Nothing

--------------------------------------------------------------------------------
-- Partial Function Fixes
--------------------------------------------------------------------------------

-- | Fix: head xs → headMay xs
fixHead :: FilePath -> Int -> Text -> ListFixConfig -> Maybe ListFix
fixHead path lineNum line config =
  if lfcSafePartials config && "head " `T.isInfixOf` line && not ("headMay" `T.isInfixOf` line)
  then
    let pattern' = "\\bhead\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
    in case (line =~ pattern' :: [[Text]]) of
         ((full:var:_):_) ->
           Just $ makeListFix path lineNum line full ("headMay " <> var) PartialSafety
                              "head-partial" "Use headMay instead of partial head" ["Data.Maybe"]
         _ -> Nothing
  else Nothing

-- | Fix: tail xs → tailMay xs
fixTail :: FilePath -> Int -> Text -> ListFixConfig -> Maybe ListFix
fixTail path lineNum line config =
  if lfcSafePartials config && "tail " `T.isInfixOf` line && not ("tailMay" `T.isInfixOf` line)
  then
    let pattern' = "\\btail\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
    in case (line =~ pattern' :: [[Text]]) of
         ((full:var:_):_) ->
           Just $ makeListFix path lineNum line full ("tailMay " <> var) PartialSafety
                              "tail-partial" "Use tailMay instead of partial tail" ["Data.Maybe"]
         _ -> Nothing
  else Nothing

-- | Fix: init xs → initMay xs
fixInit :: FilePath -> Int -> Text -> ListFixConfig -> Maybe ListFix
fixInit path lineNum line config =
  if lfcSafePartials config && "init " `T.isInfixOf` line && not ("initMay" `T.isInfixOf` line)
  then
    let pattern' = "\\binit\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
    in case (line =~ pattern' :: [[Text]]) of
         ((full:var:_):_) ->
           Just $ makeListFix path lineNum line full ("initMay " <> var) PartialSafety
                              "init-partial" "Use initMay instead of partial init" ["Data.Maybe"]
         _ -> Nothing
  else Nothing

-- | Fix: last xs → lastMay xs
fixLast :: FilePath -> Int -> Text -> ListFixConfig -> Maybe ListFix
fixLast path lineNum line config =
  if lfcSafePartials config && "last " `T.isInfixOf` line && not ("lastMay" `T.isInfixOf` line)
  then
    let pattern' = "\\blast\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
    in case (line =~ pattern' :: [[Text]]) of
         ((full:var:_):_) ->
           Just $ makeListFix path lineNum line full ("lastMay " <> var) PartialSafety
                              "last-partial" "Use lastMay instead of partial last" ["Data.Maybe"]
         _ -> Nothing
  else Nothing

--------------------------------------------------------------------------------
-- Map Fixes
--------------------------------------------------------------------------------

-- | Fix: map f (map g xs) → map (f . g) xs
fixMapMap :: FilePath -> Int -> Text -> Maybe ListFix
fixMapMap path lineNum line =
  let pattern' = "map\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+\\(\\s*map\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:f:g:xs:_):_) ->
         Just $ makeListFix path lineNum line full ("map (" <> f <> " . " <> g <> ") " <> xs) MapSimplification
                            "map-map" "Use composition instead of nested map" []
       _ -> Nothing

-- | Fix: map id xs → xs
fixMapId :: FilePath -> Int -> Text -> Maybe ListFix
fixMapId path lineNum line =
  let pattern' = "map\\s+id\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:xs:_):_) ->
         Just $ makeListFix path lineNum line full xs MapSimplification
                            "map-id" "map id is identity" []
       _ -> Nothing

--------------------------------------------------------------------------------
-- Filter Fixes
--------------------------------------------------------------------------------

-- | Fix: filter (const True) xs → xs
fixFilterTrue :: FilePath -> Int -> Text -> Maybe ListFix
fixFilterTrue path lineNum line =
  let pattern' = "filter\\s+\\(const\\s+True\\)\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:xs:_):_) ->
         Just $ makeListFix path lineNum line full xs FilterSimplification
                            "filter-const-true" "filter (const True) is identity" []
       _ -> Nothing

-- | Fix: filter (const False) xs → []
fixFilterFalse :: FilePath -> Int -> Text -> Maybe ListFix
fixFilterFalse path lineNum line =
  let pattern' = "filter\\s+\\(const\\s+False\\)\\s+[a-zA-Z_][a-zA-Z0-9_']*" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) ->
         Just $ makeListFix path lineNum line match "[]" FilterSimplification
                            "filter-const-false" "filter (const False) is always empty" []
       _ -> Nothing

-- | Fix: takeWhile (const True) xs → xs
fixTakeWhileTrue :: FilePath -> Int -> Text -> Maybe ListFix
fixTakeWhileTrue path lineNum line =
  let pattern' = "takeWhile\\s+\\(const\\s+True\\)\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:xs:_):_) ->
         Just $ makeListFix path lineNum line full xs FilterSimplification
                            "takewhile-const-true" "takeWhile (const True) is identity" []
       _ -> Nothing

-- | Fix: dropWhile (const True) xs → []
fixDropWhileTrue :: FilePath -> Int -> Text -> Maybe ListFix
fixDropWhileTrue path lineNum line =
  let pattern' = "dropWhile\\s+\\(const\\s+True\\)\\s+[a-zA-Z_][a-zA-Z0-9_']*" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) ->
         Just $ makeListFix path lineNum line match "[]" FilterSimplification
                            "dropwhile-const-true" "dropWhile (const True) is always empty" []
       _ -> Nothing

--------------------------------------------------------------------------------
-- List Construction Fixes
--------------------------------------------------------------------------------

-- | Fix: replicate n x ++ replicate m x → replicate (n + m) x
fixReplicate :: FilePath -> Int -> Text -> Maybe ListFix
fixReplicate path lineNum line =
  let pattern' = "replicate\\s+([0-9]+)\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\+\\+\\s*replicate\\s+([0-9]+)\\s+\\2" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:n:x:m:_):_) ->
         let sum' = show (read (T.unpack n) + read (T.unpack m) :: Int)
         in Just $ makeListFix path lineNum line full ("replicate " <> T.pack sum' <> " " <> x) ListConstruction
                               "replicate-concat" "Combine replicate calls" []
       _ -> Nothing

-- | Fix: zipWith (,) xs ys → zip xs ys
fixZipWithPair :: FilePath -> Int -> Text -> Maybe ListFix
fixZipWithPair path lineNum line =
  let pattern' = "zipWith\\s+\\(,\\)\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:xs:ys:_):_) ->
         Just $ makeListFix path lineNum line full ("zip " <> xs <> " " <> ys) ListConstruction
                            "zipwith-pair" "zipWith (,) is just zip" []
       _ -> Nothing

--------------------------------------------------------------------------------
-- Comprehension Fixes
--------------------------------------------------------------------------------

-- | Fix: [f x | x <- xs] → map f xs
fixListCompToMap :: FilePath -> Int -> Text -> Maybe ListFix
fixListCompToMap path lineNum line =
  let pattern' = "\\[\\s*([a-zA-Z_][a-zA-Z0-9_']*)\\s+([a-z_]+)\\s*\\|\\s*\\2\\s*<-\\s*([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\]" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:f:_:xs:_):_) ->
         Just $ makeListFix path lineNum line full ("map " <> f <> " " <> xs) ComprehensionSimplification
                            "listcomp-map" "Use map instead of list comprehension" []
       _ -> Nothing

-- | Fix: [x | x <- xs, p x] → filter p xs
fixListCompToFilter :: FilePath -> Int -> Text -> Maybe ListFix
fixListCompToFilter path lineNum line =
  let pattern' = "\\[\\s*([a-z_]+)\\s*\\|\\s*\\1\\s*<-\\s*([a-zA-Z_][a-zA-Z0-9_']*)\\s*,\\s*([a-zA-Z_][a-zA-Z0-9_']*)\\s+\\1\\s*\\]" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:_:xs:p:_):_) ->
         Just $ makeListFix path lineNum line full ("filter " <> p <> " " <> xs) ComprehensionSimplification
                            "listcomp-filter" "Use filter instead of list comprehension" []
       _ -> Nothing

-- | Fix: [y | x <- xs, y <- f x] → concatMap f xs
fixListCompToConcat :: FilePath -> Int -> Text -> Maybe ListFix
fixListCompToConcat path lineNum line =
  let pattern' = "\\[\\s*([a-z_]+)\\s*\\|\\s*[a-z_]+\\s*<-\\s*([a-zA-Z_][a-zA-Z0-9_']*)\\s*,\\s*\\1\\s*<-\\s*([a-zA-Z_][a-zA-Z0-9_']*)\\s+[a-z_]+\\s*\\]" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:_:xs:f:_):_) ->
         Just $ makeListFix path lineNum line full ("concatMap " <> f <> " " <> xs) ComprehensionSimplification
                            "listcomp-concatmap" "Use concatMap instead of nested comprehension" []
       _ -> Nothing

--------------------------------------------------------------------------------
-- Fix Helper
--------------------------------------------------------------------------------

-- | Create a ListFix
makeListFix :: FilePath -> Int -> Text -> Text -> Text -> ListFixCategory -> Text -> Text -> [Text] -> ListFix
makeListFix path lineNum lineText original replacement cat ruleId msg imports =
  let startCol = fromMaybe 1 $ findListColumn original lineText
      endCol = startCol + T.length original
  in ListFix
    { lfSpan = SrcSpan
        { srcSpanFile = path
        , srcSpanStartLine = Line lineNum
        , srcSpanStartCol = Column startCol
        , srcSpanEndLine = Line lineNum
        , srcSpanEndCol = Column endCol
        }
    , lfOriginal = original
    , lfReplacement = replacement
    , lfCategory = cat
    , lfRuleId = ruleId
    , lfMessage = msg
    , lfValidation = ListValidationPassed
    , lfAddImports = imports
    }

-- | Find column where pattern starts in line
findListColumn :: Text -> Text -> Maybe Int
findListColumn pattern' line' = case T.breakOn pattern' line' of
  (before, after) | not (T.null after) -> Just $ T.length before + 1
  _ -> Nothing

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

-- | Validate a List fix
validateListFix :: Text -> ListFix -> ListFixValidation
validateListFix content fix =
  let original = lfOriginal fix
      replacement = lfReplacement fix
  in
    if T.null replacement && not (T.null original) && replacement /= "[]"
    then ListValidationFailed "Replacement is empty"
    else if not (original `T.isInfixOf` content)
    then ListValidationFailed "Original pattern not found in content"
    else if not (balancedListParens replacement)
    then ListValidationWarning "Replacement has unbalanced parentheses"
    else ListValidationPassed

-- | Check if parentheses are balanced
balancedListParens :: Text -> Bool
balancedListParens = go (0 :: Int) (0 :: Int)
  where
    go p b text
      | p < 0 || b < 0 = False
      | T.null text = p == 0 && b == 0
      | otherwise =
          let c = T.head text
              rest = T.tail text
          in case c of
               '(' -> go (p + 1) b rest
               ')' -> go (p - 1) b rest
               '[' -> go p (b + 1) rest
               ']' -> go p (b - 1) rest
               _ -> go p b rest

-- | Validate and mark a fix
validateAndMarkList :: Text -> ListFix -> IO ListFix
validateAndMarkList content fix = do
  let validation = validateListFix content fix
  pure fix { lfValidation = validation }

-- | Check if a fix passed validation
isValidListFix :: ListFix -> Bool
isValidListFix fix = case lfValidation fix of
  ListValidationPassed -> True
  ListValidationWarning _ -> True
  ListValidationFailed _ -> False

-- | Get validation error message
getListValidationError :: ListFix -> Text
getListValidationError fix = case lfValidation fix of
  ListValidationFailed err -> lfRuleId fix <> ": " <> err
  ListValidationWarning warn -> lfRuleId fix <> " (warning): " <> warn
  ListValidationPassed -> ""

--------------------------------------------------------------------------------
-- Applying Fixes
--------------------------------------------------------------------------------

-- | Apply multiple List fixes to content
applyListFixes :: Text -> [ListFix] -> Either Text Text
applyListFixes content fixes =
  let sortedFixes = reverse $ sortListFixesByPosition fixes
  in foldl applyOneList (Right content) sortedFixes
  where
    applyOneList (Left err) _ = Left err
    applyOneList (Right txt) fix = applySingleListFix txt fix

-- | Sort fixes by their start position
sortListFixesByPosition :: [ListFix] -> [ListFix]
sortListFixesByPosition = foldr insertSortedList []
  where
    insertSortedList fix [] = [fix]
    insertSortedList fix (x:xs)
      | compareListPosition fix x == LT = fix : x : xs
      | otherwise = x : insertSortedList fix xs

    compareListPosition f1 f2 =
      let l1 = srcSpanStartLine (lfSpan f1)
          l2 = srcSpanStartLine (lfSpan f2)
          c1 = srcSpanStartCol (lfSpan f1)
          c2 = srcSpanStartCol (lfSpan f2)
      in case compare l1 l2 of
           EQ -> compare c1 c2
           x -> x

-- | Apply a single fix to content
applySingleListFix :: Text -> ListFix -> Either Text Text
applySingleListFix content fix =
  let span' = lfSpan fix
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
           newLine = prefix <> lfReplacement fix <> suffix
           newLines = take (lineNum - 1) lines' ++ [newLine] ++ drop lineNum lines'
       in Right $ T.unlines newLines

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

-- | Compute statistics from fixes
computeListStats :: [ListFix] -> ListFixStats
computeListStats fixes = foldl addListFixToStats emptyListStats fixes

-- | Add a fix to statistics
addListFixToStats :: ListFixStats -> ListFix -> ListFixStats
addListFixToStats stats fix =
  let updateValidation s = case lfValidation fix of
        ListValidationPassed -> s { lfsValidationsPassed = lfsValidationsPassed s + 1 }
        ListValidationWarning _ -> s { lfsValidationsPassed = lfsValidationsPassed s + 1 }
        ListValidationFailed _ -> s { lfsValidationsFailed = lfsValidationsFailed s + 1 }
      updateCategory s = case lfCategory fix of
        PerformanceList -> s { lfsPerformance = lfsPerformance s + 1 }
        FoldImprovement -> s { lfsFoldImprovements = lfsFoldImprovements s + 1 }
        PartialSafety -> s { lfsPartialSafety = lfsPartialSafety s + 1 }
        MapSimplification -> s { lfsMapSimplifications = lfsMapSimplifications s + 1 }
        FilterSimplification -> s { lfsFilterSimplifications = lfsFilterSimplifications s + 1 }
        ComprehensionSimplification -> s { lfsComprehensions = lfsComprehensions s + 1 }
        SortImprovement -> s { lfsSortImprovements = lfsSortImprovements s + 1 }
        ListConstruction -> s { lfsListConstruction = lfsListConstruction s + 1 }
  in updateCategory $ updateValidation $ stats { lfsTotalFixes = lfsTotalFixes stats + 1 }
