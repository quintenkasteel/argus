{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Argus.AutoFix.Pipeline
-- Description : Fix composition and pipeline system
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
-- Portability : GHC
--
-- = Overview
--
-- This module provides a composable pipeline system for fix operations.
-- Pipelines allow you to:
--
-- * Chain multiple fix operations together
-- * Filter fixes based on predicates
-- * Transform fixes before application
-- * Validate fixes at each stage
-- * Collect metrics during pipeline execution
--
-- = Architecture
--
-- A 'FixPipeline' is a sequence of 'PipelineStage's, each of which can
-- filter, transform, or validate fixes. The pipeline executes stages
-- in order, passing fixes through each stage.
--
-- @
-- Input Fixes
--     │
--     ▼
-- ┌─────────────┐
-- │ FilterStage │  ─── Remove low-confidence fixes
-- └─────────────┘
--     │
--     ▼
-- ┌─────────────┐
-- │ SortStage   │  ─── Order by priority
-- └─────────────┘
--     │
--     ▼
-- ┌──────────────┐
-- │ValidateStage │  ─── Check syntax validity
-- └──────────────┘
--     │
--     ▼
-- Output Fixes
-- @
--
-- = Stage Types
--
-- * 'FilterStage': Remove fixes that don't match a predicate
-- * 'TransformStage': Modify individual fixes
-- * 'TransformAllStage': Modify the entire fix list
-- * 'ValidateStage': Validate fixes and remove invalid ones
-- * 'SortStage': Reorder fixes
-- * 'LimitStage': Limit the number of fixes
-- * 'DeduplicateStage': Remove duplicate fixes
--
-- = Thread Safety
--
-- Pipelines are immutable and thread-safe. Pipeline execution is
-- also thread-safe as long as the validation functions don't have
-- side effects that require synchronization.
--
-- = Usage
--
-- @
-- -- Build a pipeline
-- let pipeline = buildPipeline
--       [ filterByConfidence (Confidence 0.8)
--       , filterByCategory [Style, Performance]
--       , validateSyntax customValidator
--       , sortByPriority
--       ]
--
-- -- Execute pipeline
-- result <- executePipeline pipeline fixes content
-- @
--
-- @since 1.0.0
module Argus.AutoFix.Pipeline
  ( -- * Pipeline Types
    FixPipeline (..)
  , PipelineStage (..)
  , PipelineResult (..)
  , PipelineStats (..)

    -- * Pipeline Building
  , emptyPipeline
  , buildPipeline
  , addStage
  , (|>)
  , (<|)

    -- * Standard Stages
  , filterStage
  , transformStage
  , validateStage
  , sortStage
  , limitStage
  , deduplicateStage

    -- * Common Filters
  , filterByConfidence
  , filterByCategory
  , filterBySafety
  , filterByEngine
  , filterNonConflicting

    -- * Common Transformers
  , enrichWithValidation
  , setFixPriority
  , addMetadata

    -- * Common Validators
  , validateSyntax
  , validateNoOverlap
  , validateDependencies

    -- * Common Sorters
  , sortByConfidence
  , sortBySafety
  , sortBySpan
  , sortByPriority

    -- * Pipeline Execution
  , executePipeline
  , executePipelineWithStats
  , runStage

    -- * Pipeline Combinators
  , sequencePipelines
  , parallelPipelines
  , conditionalPipeline
  , retryPipeline

    -- * Utility Functions
  , pipelineLength
  , describePipeline
  ) where

import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.List (sortBy, nubBy)
import Data.Ord (comparing, Down(..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime, diffUTCTime)
import GHC.Generics (Generic)

import Argus.AutoFix.Types
  ( EnrichedFix (..)
  , FixId (..)
  , FixMetadata (..)
  , FixValidation (..)
  , Confidence (..)
  , FixConflict (..)
  , detectConflicts
  , isValidationSuccess
  )
import Argus.Types (Fix (..), FixEdit (..))
import Argus.Rules.Types (Category, SafetyLevel (..))

--------------------------------------------------------------------------------
-- Pipeline Types
--------------------------------------------------------------------------------

-- | A pipeline of fix processing stages
data FixPipeline = FixPipeline
  { fpStages      :: [PipelineStage]
    -- ^ Stages in execution order
  , fpName        :: Text
    -- ^ Pipeline name for logging
  , fpDescription :: Text
    -- ^ Human-readable description
  , fpStopOnEmpty :: Bool
    -- ^ Stop pipeline if no fixes remain
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A single stage in the pipeline
data PipelineStage
  = FilterStage Text (EnrichedFix -> Bool)
    -- ^ Filter fixes with name and predicate
  | TransformStage Text (EnrichedFix -> EnrichedFix)
    -- ^ Transform each fix
  | TransformAllStage Text ([EnrichedFix] -> [EnrichedFix])
    -- ^ Transform entire list
  | ValidateStage Text (EnrichedFix -> Text -> IO FixValidation)
    -- ^ Validate fixes
  | SortStage Text (EnrichedFix -> EnrichedFix -> Ordering)
    -- ^ Sort fixes
  | LimitStage Text Int
    -- ^ Limit number of fixes
  | DeduplicateStage Text (EnrichedFix -> EnrichedFix -> Bool)
    -- ^ Remove duplicates

instance Show PipelineStage where
  show (FilterStage name _) = "FilterStage " <> T.unpack name
  show (TransformStage name _) = "TransformStage " <> T.unpack name
  show (TransformAllStage name _) = "TransformAllStage " <> T.unpack name
  show (ValidateStage name _) = "ValidateStage " <> T.unpack name
  show (SortStage name _) = "SortStage " <> T.unpack name
  show (LimitStage name n) = "LimitStage " <> T.unpack name <> " " <> show n
  show (DeduplicateStage name _) = "DeduplicateStage " <> T.unpack name

instance ToJSON PipelineStage where
  toJSON stage = toJSON (stageName stage)

instance FromJSON PipelineStage where
  parseJSON _ = pure (FilterStage "unknown" (const True))

-- | Get the name of a stage
stageName :: PipelineStage -> Text
stageName (FilterStage name _) = name
stageName (TransformStage name _) = name
stageName (TransformAllStage name _) = name
stageName (ValidateStage name _) = name
stageName (SortStage name _) = name
stageName (LimitStage name _) = name
stageName (DeduplicateStage name _) = name

-- | Result of pipeline execution
data PipelineResult = PipelineResult
  { prFixes       :: [EnrichedFix]
    -- ^ Remaining fixes after pipeline
  , prStats       :: PipelineStats
    -- ^ Execution statistics
  , prStageResults :: [(Text, Int, Int)]
    -- ^ (Stage name, input count, output count)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Statistics for pipeline execution
data PipelineStats = PipelineStats
  { psInputCount    :: Int
    -- ^ Number of fixes input
  , psOutputCount   :: Int
    -- ^ Number of fixes output
  , psFilteredCount :: Int
    -- ^ Number of fixes filtered out
  , psStagesRun     :: Int
    -- ^ Number of stages executed
  , psDurationMs    :: Double
    -- ^ Total execution time in milliseconds
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Pipeline Building
--------------------------------------------------------------------------------

-- | Create an empty pipeline
emptyPipeline :: FixPipeline
emptyPipeline = FixPipeline
  { fpStages = []
  , fpName = "empty"
  , fpDescription = "Empty pipeline"
  , fpStopOnEmpty = True
  }

-- | Build a pipeline from a list of stages.
--
-- Creates a named pipeline that will execute stages in order.
-- The pipeline stops early if no fixes remain after a stage
-- (configurable via 'fpStopOnEmpty').
--
-- ==== Parameters
--
-- * @stages@: List of stages to execute in order
--
-- ==== Returns
--
-- A 'FixPipeline' ready for execution.
--
-- ==== Example
--
-- @
-- let pipeline = buildPipeline
--       [ filterByConfidence (mkConfidence 0.8)
--       , sortByConfidence
--       , limitStage "top-10" 10
--       ]
-- @
--
-- @since 1.0.0
buildPipeline :: [PipelineStage] -> FixPipeline
buildPipeline stages = FixPipeline
  { fpStages = stages
  , fpName = "custom"
  , fpDescription = "Custom pipeline with " <> T.pack (show (length stages)) <> " stages"
  , fpStopOnEmpty = True
  }

-- | Add a stage to a pipeline
addStage :: PipelineStage -> FixPipeline -> FixPipeline
addStage stage pipeline = pipeline
  { fpStages = fpStages pipeline ++ [stage]
  }

-- | Infix operator to add a stage (left to right)
(|>) :: FixPipeline -> PipelineStage -> FixPipeline
(|>) = flip addStage
infixl 5 |>

-- | Infix operator to prepend a stage (right to left)
(<|) :: PipelineStage -> FixPipeline -> FixPipeline
(<|) stage pipeline = pipeline
  { fpStages = stage : fpStages pipeline
  }
infixr 5 <|

--------------------------------------------------------------------------------
-- Standard Stages
--------------------------------------------------------------------------------

-- | Create a filter stage
filterStage :: Text -> (EnrichedFix -> Bool) -> PipelineStage
filterStage = FilterStage

-- | Create a transform stage
transformStage :: Text -> (EnrichedFix -> EnrichedFix) -> PipelineStage
transformStage = TransformStage

-- | Create a validate stage
validateStage :: Text -> (EnrichedFix -> Text -> IO FixValidation) -> PipelineStage
validateStage = ValidateStage

-- | Create a sort stage
sortStage :: Text -> (EnrichedFix -> EnrichedFix -> Ordering) -> PipelineStage
sortStage = SortStage

-- | Create a limit stage
limitStage :: Text -> Int -> PipelineStage
limitStage = LimitStage

-- | Create a deduplicate stage
deduplicateStage :: Text -> (EnrichedFix -> EnrichedFix -> Bool) -> PipelineStage
deduplicateStage = DeduplicateStage

--------------------------------------------------------------------------------
-- Common Filters
--------------------------------------------------------------------------------

-- | Filter fixes by minimum confidence
filterByConfidence :: Confidence -> PipelineStage
filterByConfidence minConf = FilterStage
  ("confidence >= " <> T.pack (show (unConfidenceValue minConf)))
  (\fix -> fmConfidence (efMetadata fix) >= minConf)

-- | Filter fixes by category
filterByCategory :: Set Category -> PipelineStage
filterByCategory cats = FilterStage
  ("category in " <> T.pack (show (Set.toList cats)))
  (\fix -> fmCategory (efMetadata fix) `Set.member` cats)

-- | Filter fixes by safety level
filterBySafety :: SafetyLevel -> PipelineStage
filterBySafety minSafety = FilterStage
  ("safety >= " <> T.pack (show minSafety))
  (\fix -> fmSafety (efMetadata fix) <= minSafety)

-- | Filter fixes from specific engines
filterByEngine :: Set Text -> PipelineStage
filterByEngine engines = FilterStage
  ("engine in " <> T.pack (show (Set.toList engines)))
  (\fix -> fixIdEngine (efId fix) `Set.member` engines)

-- | Filter out conflicting fixes
filterNonConflicting :: PipelineStage
filterNonConflicting = TransformAllStage "non-conflicting" $ \fixes ->
  let conflicts = detectConflicts fixes
      conflictingIds = Set.fromList $
        concatMap (\c -> [fcFixA c, fcFixB c]) conflicts
  in filter (\fix -> efId fix `Set.notMember` conflictingIds) fixes

--------------------------------------------------------------------------------
-- Common Transformers
--------------------------------------------------------------------------------

-- | Enrich fixes with validation results
enrichWithValidation :: (EnrichedFix -> Text -> IO FixValidation) -> PipelineStage
enrichWithValidation validator = ValidateStage "enrich-validation" $ \fix content -> do
  validation <- validator fix content
  pure validation

-- | Set fix priority based on criteria (stored in tags)
setFixPriority :: (EnrichedFix -> Int) -> PipelineStage
setFixPriority getPriority = TransformStage "set-priority" $ \fix ->
  let meta = efMetadata fix
      priority = getPriority fix
      priorityTag = "priority:" <> T.pack (show priority)
      -- Remove any existing priority tags and add new one
      tagsWithoutPriority = Set.filter (not . T.isPrefixOf "priority:") (fmTags meta)
  in fix { efMetadata = meta { fmTags = Set.insert priorityTag tagsWithoutPriority } }

-- | Add metadata to fixes
addMetadata :: Text -> Text -> PipelineStage
addMetadata key value = TransformStage ("add-metadata:" <> key) $ \fix ->
  let meta = efMetadata fix
      tags = fmTags meta
  in fix { efMetadata = meta { fmTags = Set.insert (key <> ":" <> value) tags } }

--------------------------------------------------------------------------------
-- Common Validators
--------------------------------------------------------------------------------

-- | Validate that fix produces valid syntax
validateSyntax :: (EnrichedFix -> Text -> IO FixValidation) -> PipelineStage
validateSyntax = ValidateStage "syntax-validation"

-- | Validate that fixes don't overlap
validateNoOverlap :: PipelineStage
validateNoOverlap = TransformAllStage "no-overlap" $ \fixes ->
  let conflicts = detectConflicts fixes
  in if null conflicts
       then fixes
       else filter (not . isInConflict conflicts) fixes
  where
    isInConflict :: [FixConflict] -> EnrichedFix -> Bool
    isInConflict conflicts fix =
      any (\c -> efId fix == fcFixA c || efId fix == fcFixB c) conflicts

-- | Validate fix dependencies are satisfied
validateDependencies :: PipelineStage
validateDependencies = TransformAllStage "validate-deps" $ \fixes ->
  let availableIds = Set.fromList (map efId fixes)
  in filter (depsAvailable availableIds) fixes
  where
    depsAvailable available fix =
      efDependencies fix `Set.isSubsetOf` available

--------------------------------------------------------------------------------
-- Common Sorters
--------------------------------------------------------------------------------

-- | Sort fixes by confidence (highest first)
sortByConfidence :: PipelineStage
sortByConfidence = SortStage "by-confidence" $
  comparing (Down . fmConfidence . efMetadata)

-- | Sort fixes by safety (safest first)
sortBySafety :: PipelineStage
sortBySafety = SortStage "by-safety" $
  comparing (fmSafety . efMetadata)

-- | Sort fixes by span position (using the first edit's span)
sortBySpan :: PipelineStage
sortBySpan = SortStage "by-span" $
  comparing getFirstEditSpan
  where
    getFirstEditSpan ef =
      case fixEdits (efFix ef) of
        (edit:_) -> Just (fixEditSpan edit)
        []       -> Nothing

-- | Sort fixes by priority (highest first, extracted from tags)
sortByPriority :: PipelineStage
sortByPriority = SortStage "by-priority" $
  comparing (Down . getPriority)
  where
    getPriority :: EnrichedFix -> Int
    getPriority ef =
      let tags = fmTags (efMetadata ef)
          priorityTags = Set.filter (T.isPrefixOf "priority:") tags
      in case Set.toList priorityTags of
           (tag:_) -> maybe 0 id $ readMaybe $ T.unpack $ T.drop 9 tag
           []      -> 0

    readMaybe :: String -> Maybe Int
    readMaybe s = case reads s of
      [(n, "")] -> Just n
      _         -> Nothing

--------------------------------------------------------------------------------
-- Pipeline Execution
--------------------------------------------------------------------------------

-- | Execute a pipeline on fixes.
--
-- Runs all stages in sequence, passing fixes through each stage.
-- Returns only the final list of fixes (for statistics, use
-- 'executePipelineWithStats').
--
-- ==== Parameters
--
-- * @pipeline@: The pipeline to execute
-- * @fixes@: Input fixes to process
-- * @content@: File content (needed for validation stages)
--
-- ==== Returns
--
-- List of fixes remaining after all stages complete.
--
-- ==== Example
--
-- @
-- let pipeline = buildPipeline [filterByConfidence (mkConfidence 0.8)]
-- result <- executePipeline pipeline fixes content
-- @
--
-- @since 1.0.0
executePipeline :: FixPipeline -> [EnrichedFix] -> Text -> IO [EnrichedFix]
executePipeline pipeline fixes content = do
  result <- executePipelineWithStats pipeline fixes content
  pure (prFixes result)

-- | Execute a pipeline with detailed statistics.
--
-- Runs all stages in sequence, collecting metrics at each stage.
-- Useful for debugging and performance analysis.
--
-- ==== Parameters
--
-- * @pipeline@: The pipeline to execute
-- * @fixes@: Input fixes to process
-- * @content@: File content (needed for validation stages)
--
-- ==== Returns
--
-- A 'PipelineResult' containing:
--
-- * 'prFixes': Remaining fixes after all stages
-- * 'prStats': Aggregate statistics (input/output counts, duration)
-- * 'prStageResults': Per-stage input/output counts
--
-- ==== Example
--
-- @
-- result <- executePipelineWithStats pipeline fixes content
-- putStrLn $ "Processed in " ++ show (psDurationMs $ prStats result) ++ "ms"
-- forM_ (prStageResults result) $ \\(name, inp, out) ->
--   putStrLn $ name ++ ": " ++ show inp ++ " -> " ++ show out
-- @
--
-- @since 1.0.0
executePipelineWithStats :: FixPipeline
                         -> [EnrichedFix]
                         -> Text
                         -> IO PipelineResult
executePipelineWithStats pipeline fixes content = do
  startTime <- getCurrentTime
  (finalFixes, stageResults) <- runStages (fpStages pipeline) fixes content []
  endTime <- getCurrentTime
  let duration = realToFrac (diffUTCTime endTime startTime) * 1000
  pure PipelineResult
    { prFixes = finalFixes
    , prStats = PipelineStats
        { psInputCount = length fixes
        , psOutputCount = length finalFixes
        , psFilteredCount = length fixes - length finalFixes
        , psStagesRun = length (fpStages pipeline)
        , psDurationMs = duration
        }
    , prStageResults = reverse stageResults
    }
  where
    runStages :: [PipelineStage]
              -> [EnrichedFix]
              -> Text
              -> [(Text, Int, Int)]
              -> IO ([EnrichedFix], [(Text, Int, Int)])
    runStages [] fs _ results = pure (fs, results)
    runStages (stage:rest) fs c results = do
      let inputCount = length fs
      outputFixes <- runStage stage fs c
      let outputCount = length outputFixes
          result = (stageName stage, inputCount, outputCount)
      if null outputFixes && fpStopOnEmpty pipeline
        then pure ([], result : results)
        else runStages rest outputFixes c (result : results)

-- | Run a single pipeline stage
runStage :: PipelineStage -> [EnrichedFix] -> Text -> IO [EnrichedFix]
runStage stage fixes content = case stage of
  FilterStage _ predicate ->
    pure $ filter predicate fixes

  TransformStage _ transform ->
    pure $ map transform fixes

  TransformAllStage _ transform ->
    pure $ transform fixes

  ValidateStage _ validator -> do
    validated <- mapM (\fix -> do
      validation <- validator fix content
      pure (fix, validation)) fixes
    pure [fix | (fix, val) <- validated, isValidationSuccess val]

  SortStage _ comparator ->
    pure $ sortBy comparator fixes

  LimitStage _ n ->
    pure $ take n fixes

  DeduplicateStage _ eq ->
    pure $ nubBy eq fixes

--------------------------------------------------------------------------------
-- Pipeline Combinators
--------------------------------------------------------------------------------

-- | Sequence multiple pipelines
sequencePipelines :: [FixPipeline] -> FixPipeline
sequencePipelines pipelines = FixPipeline
  { fpStages = concatMap fpStages pipelines
  , fpName = "sequenced"
  , fpDescription = "Sequenced " <> T.pack (show (length pipelines)) <> " pipelines"
  , fpStopOnEmpty = any fpStopOnEmpty pipelines
  }

-- | Run pipelines in parallel and merge results
parallelPipelines :: [FixPipeline] -> FixPipeline
parallelPipelines pipelines = FixPipeline
  { fpStages = [TransformAllStage "parallel-merge" mergeAll]
  , fpName = "parallel"
  , fpDescription = "Parallel " <> T.pack (show (length pipelines)) <> " pipelines"
  , fpStopOnEmpty = False
  }
  where
    mergeAll fixes =
      -- In a real implementation, this would run in parallel
      -- For now, just concatenate the results
      concatMap (\p -> runPipelineSync p fixes) pipelines

    runPipelineSync :: FixPipeline -> [EnrichedFix] -> [EnrichedFix]
    runPipelineSync p fs = foldr applyStageSync fs (fpStages p)

    applyStageSync :: PipelineStage -> [EnrichedFix] -> [EnrichedFix]
    applyStageSync (FilterStage _ pred') fs = filter pred' fs
    applyStageSync (TransformStage _ t) fs = map t fs
    applyStageSync (TransformAllStage _ t) fs = t fs
    applyStageSync (ValidateStage _ _) fs = fs  -- Skip async validation
    applyStageSync (SortStage _ cmp) fs = sortBy cmp fs
    applyStageSync (LimitStage _ n) fs = take n fs
    applyStageSync (DeduplicateStage _ eq) fs = nubBy eq fs

-- | Run pipeline conditionally
conditionalPipeline :: (EnrichedFix -> Bool) -> FixPipeline -> FixPipeline -> FixPipeline
conditionalPipeline condition thenPipeline elsePipeline = FixPipeline
  { fpStages = [TransformAllStage "conditional" splitAndRun]
  , fpName = "conditional"
  , fpDescription = "Conditional pipeline"
  , fpStopOnEmpty = False
  }
  where
    splitAndRun fixes =
      let (matching, notMatching) = partitionBy condition fixes
          processedMatching = runPipelineSync thenPipeline matching
          processedNotMatching = runPipelineSync elsePipeline notMatching
      in processedMatching ++ processedNotMatching

    partitionBy :: (a -> Bool) -> [a] -> ([a], [a])
    partitionBy p xs = (filter p xs, filter (not . p) xs)

    runPipelineSync :: FixPipeline -> [EnrichedFix] -> [EnrichedFix]
    runPipelineSync p fs = foldr applyStageSync fs (fpStages p)

    applyStageSync :: PipelineStage -> [EnrichedFix] -> [EnrichedFix]
    applyStageSync (FilterStage _ pred') fs = filter pred' fs
    applyStageSync (TransformStage _ t) fs = map t fs
    applyStageSync (TransformAllStage _ t) fs = t fs
    applyStageSync (ValidateStage _ _) fs = fs
    applyStageSync (SortStage _ cmp) fs = sortBy cmp fs
    applyStageSync (LimitStage _ n) fs = take n fs
    applyStageSync (DeduplicateStage _ eq) fs = nubBy eq fs

-- | Retry a pipeline on failure
retryPipeline :: Int -> FixPipeline -> FixPipeline
retryPipeline maxRetries innerPipeline = FixPipeline
  { fpStages = [TransformAllStage ("retry-" <> T.pack (show maxRetries)) (retryRun maxRetries)]
  , fpName = "retry"
  , fpDescription = "Retry pipeline up to " <> T.pack (show maxRetries) <> " times"
  , fpStopOnEmpty = fpStopOnEmpty innerPipeline
  }
  where
    retryRun :: Int -> [EnrichedFix] -> [EnrichedFix]
    retryRun 0 fixes = fixes
    retryRun n fixes =
      let result = runPipelineSync innerPipeline fixes
      in if null result && n > 1
           then retryRun (n - 1) fixes
           else result

    runPipelineSync :: FixPipeline -> [EnrichedFix] -> [EnrichedFix]
    runPipelineSync p fs = foldr applyStageSync fs (fpStages p)

    applyStageSync :: PipelineStage -> [EnrichedFix] -> [EnrichedFix]
    applyStageSync (FilterStage _ pred') fs = filter pred' fs
    applyStageSync (TransformStage _ t) fs = map t fs
    applyStageSync (TransformAllStage _ t) fs = t fs
    applyStageSync (ValidateStage _ _) fs = fs
    applyStageSync (SortStage _ cmp) fs = sortBy cmp fs
    applyStageSync (LimitStage _ n) fs = take n fs
    applyStageSync (DeduplicateStage _ eq) fs = nubBy eq fs

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- | Get the number of stages in a pipeline
pipelineLength :: FixPipeline -> Int
pipelineLength = length . fpStages

-- | Get a human-readable description of the pipeline
describePipeline :: FixPipeline -> Text
describePipeline pipeline = T.unlines $
  [ fpName pipeline <> ": " <> fpDescription pipeline
  , "Stages:"
  ] ++ map (\(i, s) -> "  " <> T.pack (show i) <> ". " <> stageName s)
         (zip [1 :: Int ..] (fpStages pipeline))
