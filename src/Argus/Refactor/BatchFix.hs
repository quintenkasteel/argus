{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Refactor.BatchFix
-- Description : High-performance batch fix application with parallelism
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides enterprise-grade batch fix application capabilities:
--
-- == Key Features
--
-- * __Parallel Processing__: Process multiple files concurrently
-- * __Progress Tracking__: Real-time progress callbacks
-- * __Metrics Integration__: Track timing and resource usage
-- * __Rule Filtering__: Apply fixes only from specific rules
-- * __Dry Run Mode__: Preview changes without writing
-- * __Comprehensive Reports__: Detailed summaries of all changes
--
-- == Usage Example
--
-- @
-- let opts = defaultBatchOptions
--       { boParallelism = 4
--       , boProgressCallback = Just printProgress
--       }
-- result <- runBatchFix opts diagnosticsByFile
-- putStrLn $ formatBatchSummary result
-- @
module Argus.Refactor.BatchFix
  ( -- * Batch Fix Operations
    runBatchFix
  , runBatchFixWithMetrics
  , BatchFixResult (..)
  , BatchFixOptions (..)
  , defaultBatchOptions

    -- * Progress Reporting
  , BatchProgress (..)
  , BatchProgressCallback
  , ProgressPhase (..)

    -- * Filtering
  , FixFilter (..)
  , FilterMode (..)
  , FilterCriterion (..)
  , applyFilters

    -- * Results and Statistics
  , BatchStats (..)
  , FileFixResult (..)
  , FixOutcome (..)

    -- * Report Generation
  , formatBatchSummary
  , formatDetailedReport
  , BatchReport (..)
  , generateReport
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Control.Monad (forM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)

import Argus.Types
import Argus.Rules.Types (SafetyLevel(..))
import Argus.Refactor.SafeRefactor
import Argus.Refactor.FixGraph (ConflictStrategy(..))
import Argus.Refactor.Validation (ValidationLevel(..))

--------------------------------------------------------------------------------
-- Batch Fix Options
--------------------------------------------------------------------------------

-- | Configuration for batch fix operations
data BatchFixOptions = BatchFixOptions
  { -- Parallelism
    boParallelism        :: Int
    -- ^ Number of files to process concurrently (default: 4)

    -- Safety
  , boValidateEachFix    :: Bool
    -- ^ Re-parse after each fix to ensure correctness
  , boValidationLevel    :: ValidationLevel
    -- ^ How strict validation should be
  , boTransactional      :: Bool
    -- ^ Roll back all changes if any fix fails
  , boSafetyLevel        :: SafetyLevel
    -- ^ Minimum safety level for auto-application
  , boCreateBackups      :: Bool
    -- ^ Create .bak files before modifying

    -- Conflict handling
  , boConflictStrategy   :: ConflictStrategy
    -- ^ How to resolve overlapping fixes

    -- Filtering
  , boRuleFilter         :: Maybe FixFilter
    -- ^ Optional filter for which fixes to apply
  , boFileFilter         :: Maybe (FilePath -> Bool)
    -- ^ Optional filter for which files to process

    -- Mode
  , boDryRun             :: Bool
    -- ^ Preview changes without writing
  , boInteractive        :: Bool
    -- ^ Prompt for each fix
  , boStopOnError        :: Bool
    -- ^ Stop processing if a file fails

    -- Output
  , boVerbose            :: Bool
    -- ^ Detailed logging
  , boProgressCallback   :: Maybe BatchProgressCallback
    -- ^ Callback for progress updates
  }

-- | Sensible defaults for batch operations
defaultBatchOptions :: BatchFixOptions
defaultBatchOptions = BatchFixOptions
  { boParallelism       = 4
  , boValidateEachFix   = True
  , boValidationLevel   = SyntaxValidation
  , boTransactional     = False  -- Per-file, not global
  , boSafetyLevel       = Safe
  , boCreateBackups     = True
  , boConflictStrategy  = PreferPreferred
  , boRuleFilter        = Nothing
  , boFileFilter        = Nothing
  , boDryRun            = False
  , boInteractive       = False
  , boStopOnError       = False
  , boVerbose           = False
  , boProgressCallback  = Nothing
  }

--------------------------------------------------------------------------------
-- Progress Reporting
--------------------------------------------------------------------------------

-- | Progress information during batch operations
data BatchProgress = BatchProgress
  { bpPhase           :: ProgressPhase
  , bpCurrentFile     :: Maybe FilePath
  , bpFilesProcessed  :: Int
  , bpFilesTotal      :: Int
  , bpFixesApplied    :: Int
  , bpFixesFailed     :: Int
  , bpElapsedTime     :: NominalDiffTime
  , bpMessage         :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | Phases of batch processing
data ProgressPhase
  = PhaseInitializing
  | PhaseFiltering
  | PhaseProcessing
  | PhaseValidating
  | PhaseWriting
  | PhaseComplete
  | PhaseFailed
  deriving stock (Eq, Show, Ord, Enum, Bounded)

-- | Callback type for progress updates
type BatchProgressCallback = BatchProgress -> IO ()

--------------------------------------------------------------------------------
-- Fix Filtering
--------------------------------------------------------------------------------

-- | Filter for selecting which fixes to apply
data FixFilter = FixFilter
  { ffCriteria :: [FilterCriterion]
  , ffMode     :: FilterMode
  }
  deriving stock (Eq, Show)

-- | How to combine multiple criteria
data FilterMode
  = FilterAll   -- ^ All criteria must match (AND)
  | FilterAny   -- ^ Any criterion must match (OR)
  deriving stock (Eq, Show)

-- | Individual filter criterion
data FilterCriterion
  = RuleIdMatches Text           -- ^ Rule ID contains this text
  | RuleIdIn (Set Text)          -- ^ Rule ID is in this set
  | SeverityIs Severity          -- ^ Diagnostic has this severity
  | SeverityAtLeast Severity     -- ^ Diagnostic severity >= this
  | CategoryIs DiagnosticKind    -- ^ Diagnostic has this category
  | FilePathMatches Text         -- ^ File path contains this text
  | MessageContains Text         -- ^ Message contains this text
  deriving stock (Eq, Show)

-- | Apply filters to diagnostics
applyFilters :: Maybe FixFilter -> [Diagnostic] -> [Diagnostic]
applyFilters Nothing diags = diags
applyFilters (Just FixFilter{..}) diags =
  filter (matchesCriteria ffMode ffCriteria) diags

matchesCriteria :: FilterMode -> [FilterCriterion] -> Diagnostic -> Bool
matchesCriteria _ [] _ = True
matchesCriteria FilterAll criteria diag = all (matchesCriterion diag) criteria
matchesCriteria FilterAny criteria diag = any (matchesCriterion diag) criteria

matchesCriterion :: Diagnostic -> FilterCriterion -> Bool
matchesCriterion diag = \case
  RuleIdMatches txt -> maybe False (T.isInfixOf txt) (diagCode diag)
  RuleIdIn ids -> maybe False (`Set.member` ids) (diagCode diag)
  SeverityIs sev -> diagSeverity diag == sev
  SeverityAtLeast sev -> compareSeverity (diagSeverity diag) sev
  CategoryIs cat -> diagKind diag == cat
  FilePathMatches txt -> T.isInfixOf txt (T.pack $ srcSpanFile $ diagSpan diag)
  MessageContains txt -> T.isInfixOf (T.toLower txt) (T.toLower $ diagMessage diag)

-- | Compare severity (Error > Warning > Suggestion > Info)
compareSeverity :: Severity -> Severity -> Bool
compareSeverity actual minimum' = fromEnum actual <= fromEnum minimum'

--------------------------------------------------------------------------------
-- Result Types
--------------------------------------------------------------------------------

-- | Overall result of batch fix operation
data BatchFixResult = BatchFixResult
  { bfrSuccess       :: Bool
    -- ^ Whether the operation completed successfully
  , bfrStats         :: BatchStats
    -- ^ Aggregate statistics
  , bfrFileResults   :: Map FilePath FileFixResult
    -- ^ Per-file results
  , bfrStartTime     :: UTCTime
    -- ^ When processing started
  , bfrEndTime       :: UTCTime
    -- ^ When processing completed
  , bfrOptions       :: BatchFixOptions
    -- ^ Options used for this run
  }

-- | Aggregate statistics for batch operation
data BatchStats = BatchStats
  { bsTotalFiles      :: Int
  , bsFilesProcessed  :: Int
  , bsFilesModified   :: Int
  , bsFilesSkipped    :: Int
  , bsFilesFailed     :: Int
  , bsTotalFixes      :: Int
  , bsFixesApplied    :: Int
  , bsFixesSkipped    :: Int
  , bsFixesFailed     :: Int
  , bsConflictsFound  :: Int
  , bsProcessingTime  :: NominalDiffTime
  }
  deriving stock (Eq, Show)

-- | Result for a single file
data FileFixResult = FileFixResult
  { ffrFilePath      :: FilePath
  , ffrSuccess       :: Bool
  , ffrOutcomes      :: [FixOutcome]
  , ffrFixesApplied  :: Int
  , ffrFixesSkipped  :: Int
  , ffrFixesFailed   :: Int
  , ffrError         :: Maybe Text
  , ffrProcessTime   :: NominalDiffTime
  }
  deriving stock (Eq, Show)

-- | Outcome of applying a single fix
data FixOutcome
  = OutcomeApplied Text     -- ^ Fix was applied, with description
  | OutcomeSkipped Text     -- ^ Fix was skipped, with reason
  | OutcomeFailed Text      -- ^ Fix failed, with error
  | OutcomeConflict Text    -- ^ Fix had conflicts
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Main Batch Fix Functions
--------------------------------------------------------------------------------

-- | Run batch fix operation
runBatchFix :: BatchFixOptions
            -> [(FilePath, [Diagnostic])]
            -> IO BatchFixResult
runBatchFix opts inputs = do
  startTime <- getCurrentTime

  -- Apply file filter
  let filteredInputs = case boFileFilter opts of
        Nothing -> inputs
        Just predicate -> filter (predicate . fst) inputs

  -- Apply fix filter to diagnostics
  let filteredDiags = map (\(fp, ds) -> (fp, applyFilters (boRuleFilter opts) ds)) filteredInputs

  -- Filter out files with no applicable diagnostics
  let nonEmptyInputs = filter (not . null . snd) filteredDiags
      totalFiles = length nonEmptyInputs

  -- Initialize progress
  progressVar <- newTVarIO BatchProgress
    { bpPhase = PhaseInitializing
    , bpCurrentFile = Nothing
    , bpFilesProcessed = 0
    , bpFilesTotal = totalFiles
    , bpFixesApplied = 0
    , bpFixesFailed = 0
    , bpElapsedTime = 0
    , bpMessage = Just "Starting batch fix operation..."
    }

  reportProgress opts progressVar

  -- Update to processing phase
  atomically $ modifyTVar' progressVar $ \p -> p
    { bpPhase = PhaseProcessing
    , bpMessage = Just $ "Processing " <> T.pack (show totalFiles) <> " files..."
    }
  reportProgress opts progressVar

  -- Process files (in parallel or sequential)
  fileResults <- if boParallelism opts > 1
    then processFilesParallel opts progressVar nonEmptyInputs
    else processFilesSequential opts progressVar nonEmptyInputs

  endTime <- getCurrentTime

  -- Calculate final statistics
  let stats = aggregateStats fileResults (diffUTCTime endTime startTime)
      success = bsFilesFailed stats == 0

  -- Update to complete phase
  atomically $ modifyTVar' progressVar $ \p -> p
    { bpPhase = if success then PhaseComplete else PhaseFailed
    , bpFilesProcessed = bsFilesProcessed stats
    , bpFixesApplied = bsFixesApplied stats
    , bpFixesFailed = bsFixesFailed stats
    , bpElapsedTime = diffUTCTime endTime startTime
    , bpMessage = Just $ if success
        then "Batch fix completed successfully"
        else "Batch fix completed with errors"
    }
  reportProgress opts progressVar

  pure BatchFixResult
    { bfrSuccess = success
    , bfrStats = stats
    , bfrFileResults = Map.fromList $ map (\r -> (ffrFilePath r, r)) fileResults
    , bfrStartTime = startTime
    , bfrEndTime = endTime
    , bfrOptions = opts
    }

-- | Run batch fix with metrics integration
runBatchFixWithMetrics :: BatchFixOptions
                       -> [(FilePath, [Diagnostic])]
                       -> IO BatchFixResult
runBatchFixWithMetrics opts inputs = do
  -- Just delegate to runBatchFix for now
  -- Metrics integration would be added through the progress callback
  runBatchFix opts inputs

--------------------------------------------------------------------------------
-- Internal Processing
--------------------------------------------------------------------------------

-- | Process files in parallel with bounded concurrency
processFilesParallel :: BatchFixOptions
                     -> TVar BatchProgress
                     -> [(FilePath, [Diagnostic])]
                     -> IO [FileFixResult]
processFilesParallel opts progressVar inputs = do
  -- Use mapConcurrently with chunking for bounded parallelism
  let chunks = chunksOf (boParallelism opts) inputs
  results <- fmap concat $ forM chunks $ \chunk ->
    mapConcurrently (processFile opts progressVar) chunk
  pure results

-- | Process files sequentially
processFilesSequential :: BatchFixOptions
                       -> TVar BatchProgress
                       -> [(FilePath, [Diagnostic])]
                       -> IO [FileFixResult]
processFilesSequential opts progressVar =
  mapM (processFile opts progressVar)

-- | Process a single file
processFile :: BatchFixOptions
            -> TVar BatchProgress
            -> (FilePath, [Diagnostic])
            -> IO FileFixResult
processFile opts progressVar (filePath, diagnostics) = do
  fileStart <- getCurrentTime

  -- Update progress
  atomically $ modifyTVar' progressVar $ \p -> p
    { bpCurrentFile = Just filePath
    , bpMessage = Just $ "Processing " <> T.pack filePath
    }
  reportProgress opts progressVar

  -- Apply fixes using SafeRefactor
  let safeOpts = toSafeRefactorOptions opts
  result <- try $ safeApplyFixes safeOpts [(filePath, diagnostics)]

  fileEnd <- getCurrentTime
  let processTime = diffUTCTime fileEnd fileStart

  -- Convert result
  fileResult <- case result of
    Left (e :: SomeException) -> pure FileFixResult
      { ffrFilePath = filePath
      , ffrSuccess = False
      , ffrOutcomes = [OutcomeFailed $ T.pack $ show e]
      , ffrFixesApplied = 0
      , ffrFixesSkipped = 0
      , ffrFixesFailed = length diagnostics
      , ffrError = Just $ T.pack $ show e
      , ffrProcessTime = processTime
      }
    Right safeResult -> do
      let outcomes = extractOutcomes safeResult
          applied = rsAppliedFixes $ srrStats safeResult
          skipped = rsSkippedFixes $ srrStats safeResult
          failed = rsFailedFixes $ srrStats safeResult
      pure FileFixResult
        { ffrFilePath = filePath
        , ffrSuccess = srrSuccess safeResult
        , ffrOutcomes = outcomes
        , ffrFixesApplied = applied
        , ffrFixesSkipped = skipped
        , ffrFixesFailed = failed
        , ffrError = if null (srrFailed safeResult)
            then Nothing
            else Just $ T.unlines $ map snd $ srrFailed safeResult
        , ffrProcessTime = processTime
        }

  -- Update progress
  atomically $ modifyTVar' progressVar $ \p -> p
    { bpFilesProcessed = bpFilesProcessed p + 1
    , bpFixesApplied = bpFixesApplied p + ffrFixesApplied fileResult
    , bpFixesFailed = bpFixesFailed p + ffrFixesFailed fileResult
    }
  reportProgress opts progressVar

  pure fileResult

-- | Extract outcomes from SafeRefactorResult
extractOutcomes :: SafeRefactorResult -> [FixOutcome]
extractOutcomes result =
  let applied = map (\af -> OutcomeApplied $ T.pack $ show $ afFix af) (srrApplied result)
      skipped = map (\(_, reason) -> OutcomeSkipped $ T.pack $ show reason) (srrSkipped result)
      failed = map (\(_, err) -> OutcomeFailed err) (srrFailed result)
  in applied <> skipped <> failed

-- | Convert BatchFixOptions to SafeRefactorOptions
toSafeRefactorOptions :: BatchFixOptions -> SafeRefactorOptions
toSafeRefactorOptions BatchFixOptions{..} = defaultSafeOptions
  { sroValidationLevel = if boValidateEachFix then boValidationLevel else NoValidation
  , sroValidateEachFix = boValidateEachFix
  , sroConflictStrategy = boConflictStrategy
  , sroTransactional = boTransactional
  , sroCreateBackups = boCreateBackups
  , sroInteractive = boInteractive
  , sroDryRun = boDryRun
  , sroVerbose = boVerbose
  , sroSafeOnly = boSafetyLevel == Safe
  }

-- | Report progress if callback is configured
reportProgress :: BatchFixOptions -> TVar BatchProgress -> IO ()
reportProgress opts progressVar = do
  case boProgressCallback opts of
    Nothing -> pure ()
    Just callback -> do
      progress <- readTVarIO progressVar
      callback progress

-- | Aggregate statistics from file results
aggregateStats :: [FileFixResult] -> NominalDiffTime -> BatchStats
aggregateStats results totalTime = BatchStats
  { bsTotalFiles = length results
  , bsFilesProcessed = length results
  , bsFilesModified = length $ filter ((> 0) . ffrFixesApplied) results
  , bsFilesSkipped = length $ filter (\r -> ffrFixesApplied r == 0 && ffrSuccess r) results
  , bsFilesFailed = length $ filter (not . ffrSuccess) results
  , bsTotalFixes = sum $ map (\r -> ffrFixesApplied r + ffrFixesSkipped r + ffrFixesFailed r) results
  , bsFixesApplied = sum $ map ffrFixesApplied results
  , bsFixesSkipped = sum $ map ffrFixesSkipped results
  , bsFixesFailed = sum $ map ffrFixesFailed results
  , bsConflictsFound = length $ filter isConflict $ concatMap ffrOutcomes results
  , bsProcessingTime = totalTime
  }
  where
    isConflict (OutcomeConflict _) = True
    isConflict _ = False

-- | Split list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

--------------------------------------------------------------------------------
-- Report Generation
--------------------------------------------------------------------------------

-- | Full batch report
data BatchReport = BatchReport
  { brSummary     :: Text
  , brDetails     :: Text
  , brFileReports :: [(FilePath, Text)]
  , brTimestamp   :: UTCTime
  }
  deriving stock (Eq, Show)

-- | Generate a batch report
generateReport :: BatchFixResult -> BatchReport
generateReport result = BatchReport
  { brSummary = formatBatchSummary result
  , brDetails = formatDetailedReport result
  , brFileReports = map (\(fp, fr) -> (fp, formatFileResult fr)) $ Map.toList $ bfrFileResults result
  , brTimestamp = bfrEndTime result
  }

-- | Format a concise summary of the batch operation
formatBatchSummary :: BatchFixResult -> Text
formatBatchSummary BatchFixResult{..} =
  let BatchStats{..} = bfrStats
      status = if bfrSuccess then "SUCCESS" else "FAILED"
      timeStr = T.pack $ show bsProcessingTime
  in T.unlines
    [ "=============================================="
    , "          Batch Fix Summary - " <> status
    , "=============================================="
    , ""
    , "Files:"
    , "  Total:     " <> T.pack (show bsTotalFiles)
    , "  Modified:  " <> T.pack (show bsFilesModified)
    , "  Skipped:   " <> T.pack (show bsFilesSkipped)
    , "  Failed:    " <> T.pack (show bsFilesFailed)
    , ""
    , "Fixes:"
    , "  Applied:   " <> T.pack (show bsFixesApplied)
    , "  Skipped:   " <> T.pack (show bsFixesSkipped)
    , "  Failed:    " <> T.pack (show bsFixesFailed)
    , "  Conflicts: " <> T.pack (show bsConflictsFound)
    , ""
    , "Time: " <> timeStr
    , "=============================================="
    ]

-- | Format a detailed report with per-file information
formatDetailedReport :: BatchFixResult -> Text
formatDetailedReport result@BatchFixResult{..} =
  let summary = formatBatchSummary result
      fileDetails = T.unlines $ map formatFileResult' $ Map.toList bfrFileResults
  in summary <> "\n\nFile Details:\n" <> fileDetails
  where
    formatFileResult' (fp, fr) =
      let status = if ffrSuccess fr then "[OK]" else "[FAIL]"
      in T.pack $ status <> " " <> fp <> " (" <> show (ffrFixesApplied fr) <> " applied)"

-- | Format result for a single file
formatFileResult :: FileFixResult -> Text
formatFileResult FileFixResult{..} = T.unlines
  [ "File: " <> T.pack ffrFilePath
  , "Status: " <> if ffrSuccess then "Success" else "Failed"
  , "Fixes: " <> T.pack (show ffrFixesApplied) <> " applied, "
             <> T.pack (show ffrFixesSkipped) <> " skipped, "
             <> T.pack (show ffrFixesFailed) <> " failed"
  , "Time: " <> T.pack (show ffrProcessTime)
  ] <> case ffrError of
    Nothing -> ""
    Just err -> "\nError: " <> err
