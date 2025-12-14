{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : BatchFixSpec
-- Description : Tests for batch fix application
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Comprehensive tests for the Argus.Refactor.BatchFix module
-- covering parallel processing, filtering, and progress tracking.
module BatchFixSpec (spec) where

import Control.Concurrent.STM
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Test.Hspec

import Argus.Refactor.BatchFix
import Argus.Types

spec :: Spec
spec = do
  describe "BatchFixOptions" $ do
    defaultOptionsSpec

  describe "FixFilter" $ do
    filterCriterionSpec
    filterModeSpec

  describe "BatchProgress" $ do
    progressPhaseSpec
    progressTrackingSpec

  describe "BatchStats" $ do
    statsAggregationSpec

  describe "BatchReport" $ do
    reportGenerationSpec

  describe "runBatchFix" $ do
    batchFixExecutionSpec

--------------------------------------------------------------------------------
-- Default Options Tests
--------------------------------------------------------------------------------

defaultOptionsSpec :: Spec
defaultOptionsSpec = do
  it "has sensible default parallelism" $ do
    boParallelism defaultBatchOptions `shouldBe` 4

  it "enables validation by default" $ do
    boValidateEachFix defaultBatchOptions `shouldBe` True

  it "creates backups by default" $ do
    boCreateBackups defaultBatchOptions `shouldBe` True

  it "is not in dry run mode by default" $ do
    boDryRun defaultBatchOptions `shouldBe` False

  it "is not interactive by default" $ do
    boInteractive defaultBatchOptions `shouldBe` False

  it "does not stop on error by default" $ do
    boStopOnError defaultBatchOptions `shouldBe` False

  it "is not verbose by default" $ do
    boVerbose defaultBatchOptions `shouldBe` False

  it "has no progress callback by default" $ do
    case boProgressCallback defaultBatchOptions of
      Nothing -> pure ()
      Just _ -> expectationFailure "Expected no progress callback"

  it "has no file filter by default" $ do
    case boFileFilter defaultBatchOptions of
      Nothing -> pure ()
      Just _ -> expectationFailure "Expected no file filter"

  it "has no rule filter by default" $ do
    boRuleFilter defaultBatchOptions `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Filter Criterion Tests
--------------------------------------------------------------------------------

filterCriterionSpec :: Spec
filterCriterionSpec = do
  describe "RuleIdMatches" $ do
    it "matches when rule ID contains text" $ do
      let diag = mkDiag "partial-head-usage" Error
          criterion = RuleIdMatches "partial"
          result = applyFilters (Just $ FixFilter [criterion] FilterAll) [diag]
      length result `shouldBe` 1

    it "does not match when rule ID does not contain text" $ do
      let diag = mkDiag "naming-convention" Warning
          criterion = RuleIdMatches "partial"
          result = applyFilters (Just $ FixFilter [criterion] FilterAll) [diag]
      length result `shouldBe` 0

  describe "RuleIdIn" $ do
    it "matches when rule ID is in set" $ do
      let diag = mkDiag "security-sql-injection" Error
          ids = Set.fromList ["security-sql-injection", "security-xss"]
          criterion = RuleIdIn ids
          result = applyFilters (Just $ FixFilter [criterion] FilterAll) [diag]
      length result `shouldBe` 1

    it "does not match when rule ID is not in set" $ do
      let diag = mkDiag "naming-convention" Warning
          ids = Set.fromList ["security-sql-injection", "security-xss"]
          criterion = RuleIdIn ids
          result = applyFilters (Just $ FixFilter [criterion] FilterAll) [diag]
      length result `shouldBe` 0

  describe "SeverityIs" $ do
    it "matches exact severity" $ do
      let diag = mkDiag "test-rule" Error
          criterion = SeverityIs Error
          result = applyFilters (Just $ FixFilter [criterion] FilterAll) [diag]
      length result `shouldBe` 1

    it "does not match different severity" $ do
      let diag = mkDiag "test-rule" Warning
          criterion = SeverityIs Error
          result = applyFilters (Just $ FixFilter [criterion] FilterAll) [diag]
      length result `shouldBe` 0

  describe "SeverityAtLeast" $ do
    it "matches higher severity than minimum" $ do
      let diag = mkDiag "test-rule" Error
          criterion = SeverityAtLeast Warning
          result = applyFilters (Just $ FixFilter [criterion] FilterAll) [diag]
      length result `shouldBe` 1

    it "matches exact minimum severity" $ do
      let diag = mkDiag "test-rule" Warning
          criterion = SeverityAtLeast Warning
          result = applyFilters (Just $ FixFilter [criterion] FilterAll) [diag]
      length result `shouldBe` 1

  describe "CategoryIs" $ do
    it "matches diagnostic category" $ do
      let diag = mkDiagWithKind "test-rule" Error SecurityIssue
          criterion = CategoryIs SecurityIssue
          result = applyFilters (Just $ FixFilter [criterion] FilterAll) [diag]
      length result `shouldBe` 1

  describe "FilePathMatches" $ do
    it "matches when file path contains text" $ do
      let diag = mkDiagWithPath "test-rule" Error "src/Foo/Bar.hs"
          criterion = FilePathMatches "Foo"
          result = applyFilters (Just $ FixFilter [criterion] FilterAll) [diag]
      length result `shouldBe` 1

  describe "MessageContains" $ do
    it "matches when message contains text (case insensitive)" $ do
      let diag = mkDiagWithMessage "test-rule" Error "Use foldr instead of explicit recursion"
          criterion = MessageContains "FOLDR"
          result = applyFilters (Just $ FixFilter [criterion] FilterAll) [diag]
      length result `shouldBe` 1

--------------------------------------------------------------------------------
-- Filter Mode Tests
--------------------------------------------------------------------------------

filterModeSpec :: Spec
filterModeSpec = do
  describe "FilterAll (AND)" $ do
    it "requires all criteria to match" $ do
      let diag = mkDiag "partial-head" Error
          criteria = [RuleIdMatches "partial", SeverityIs Error]
          result = applyFilters (Just $ FixFilter criteria FilterAll) [diag]
      length result `shouldBe` 1

    it "fails if any criterion does not match" $ do
      let diag = mkDiag "partial-head" Warning  -- Wrong severity
          criteria = [RuleIdMatches "partial", SeverityIs Error]
          result = applyFilters (Just $ FixFilter criteria FilterAll) [diag]
      length result `shouldBe` 0

  describe "FilterAny (OR)" $ do
    it "matches if any criterion matches" $ do
      let diag = mkDiag "naming-error" Warning
          criteria = [RuleIdMatches "naming", RuleIdMatches "security"]
          result = applyFilters (Just $ FixFilter criteria FilterAny) [diag]
      length result `shouldBe` 1

    it "fails only if no criteria match" $ do
      let diag = mkDiag "performance-issue" Warning
          criteria = [RuleIdMatches "naming", RuleIdMatches "security"]
          result = applyFilters (Just $ FixFilter criteria FilterAny) [diag]
      length result `shouldBe` 0

  describe "Empty filter" $ do
    it "returns all diagnostics when filter is Nothing" $ do
      let diags = [mkDiag "rule1" Error, mkDiag "rule2" Warning]
          result = applyFilters Nothing diags
      length result `shouldBe` 2

    it "returns all diagnostics when criteria list is empty" $ do
      let diags = [mkDiag "rule1" Error, mkDiag "rule2" Warning]
          result = applyFilters (Just $ FixFilter [] FilterAll) diags
      length result `shouldBe` 2

--------------------------------------------------------------------------------
-- Progress Phase Tests
--------------------------------------------------------------------------------

progressPhaseSpec :: Spec
progressPhaseSpec = do
  it "PhaseInitializing is the starting phase" $ do
    PhaseInitializing `shouldBe` minBound

  it "PhaseComplete is near the end" $ do
    PhaseComplete > PhaseProcessing `shouldBe` True

  it "PhaseFailed is the final phase" $ do
    PhaseFailed `shouldBe` maxBound

  it "phases have correct ordering" $ do
    PhaseInitializing < PhaseFiltering `shouldBe` True
    PhaseFiltering < PhaseProcessing `shouldBe` True
    PhaseProcessing < PhaseValidating `shouldBe` True
    PhaseValidating < PhaseWriting `shouldBe` True
    PhaseWriting < PhaseComplete `shouldBe` True

--------------------------------------------------------------------------------
-- Progress Tracking Tests
--------------------------------------------------------------------------------

progressTrackingSpec :: Spec
progressTrackingSpec = do
  it "BatchProgress can be created with initial values" $ do
    let progress = BatchProgress
          { bpPhase = PhaseInitializing
          , bpCurrentFile = Nothing
          , bpFilesProcessed = 0
          , bpFilesTotal = 10
          , bpFixesApplied = 0
          , bpFixesFailed = 0
          , bpElapsedTime = 0
          , bpMessage = Just "Starting..."
          }
    bpFilesTotal progress `shouldBe` 10
    bpPhase progress `shouldBe` PhaseInitializing

  it "progress can track file being processed" $ do
    let progress = BatchProgress
          { bpPhase = PhaseProcessing
          , bpCurrentFile = Just "src/Foo.hs"
          , bpFilesProcessed = 5
          , bpFilesTotal = 10
          , bpFixesApplied = 15
          , bpFixesFailed = 2
          , bpElapsedTime = 5.5
          , bpMessage = Just "Processing src/Foo.hs"
          }
    bpCurrentFile progress `shouldBe` Just "src/Foo.hs"
    bpFilesProcessed progress `shouldBe` 5

--------------------------------------------------------------------------------
-- Stats Aggregation Tests
--------------------------------------------------------------------------------

statsAggregationSpec :: Spec
statsAggregationSpec = do
  it "BatchStats correctly counts files" $ do
    let stats = BatchStats
          { bsTotalFiles = 10
          , bsFilesProcessed = 10
          , bsFilesModified = 7
          , bsFilesSkipped = 2
          , bsFilesFailed = 1
          , bsTotalFixes = 50
          , bsFixesApplied = 40
          , bsFixesSkipped = 8
          , bsFixesFailed = 2
          , bsConflictsFound = 3
          , bsProcessingTime = 10.5
          }
    bsTotalFiles stats `shouldBe` 10
    bsFilesModified stats + bsFilesSkipped stats + bsFilesFailed stats `shouldBe` 10

  it "BatchStats fix counts should sum correctly" $ do
    let stats = BatchStats
          { bsTotalFiles = 5
          , bsFilesProcessed = 5
          , bsFilesModified = 4
          , bsFilesSkipped = 1
          , bsFilesFailed = 0
          , bsTotalFixes = 100
          , bsFixesApplied = 90
          , bsFixesSkipped = 5
          , bsFixesFailed = 5
          , bsConflictsFound = 2
          , bsProcessingTime = 5.0
          }
    bsFixesApplied stats + bsFixesSkipped stats + bsFixesFailed stats `shouldBe` bsTotalFixes stats

--------------------------------------------------------------------------------
-- Report Generation Tests
--------------------------------------------------------------------------------

reportGenerationSpec :: Spec
reportGenerationSpec = do
  it "formatBatchSummary produces non-empty output" $ do
    result <- mkMockBatchResult True
    let summary = formatBatchSummary result
    T.length summary `shouldSatisfy` (> 0)

  it "formatBatchSummary includes status" $ do
    successResult <- mkMockBatchResult True
    failResult <- mkMockBatchResult False
    formatBatchSummary successResult `shouldSatisfy` T.isInfixOf "SUCCESS"
    formatBatchSummary failResult `shouldSatisfy` T.isInfixOf "FAILED"

  it "formatBatchSummary includes file counts" $ do
    result <- mkMockBatchResult True
    let summary = formatBatchSummary result
    summary `shouldSatisfy` T.isInfixOf "Files:"
    summary `shouldSatisfy` T.isInfixOf "Total:"

  it "formatBatchSummary includes fix counts" $ do
    result <- mkMockBatchResult True
    let summary = formatBatchSummary result
    summary `shouldSatisfy` T.isInfixOf "Fixes:"
    summary `shouldSatisfy` T.isInfixOf "Applied:"

  it "formatDetailedReport includes per-file information" $ do
    result <- mkMockBatchResult True
    let detailed = formatDetailedReport result
    detailed `shouldSatisfy` T.isInfixOf "File Details:"

  it "generateReport creates complete report" $ do
    result <- mkMockBatchResult True
    let report = generateReport result
    T.length (brSummary report) `shouldSatisfy` (> 0)
    T.length (brDetails report) `shouldSatisfy` (> 0)

--------------------------------------------------------------------------------
-- Batch Fix Execution Tests
--------------------------------------------------------------------------------

batchFixExecutionSpec :: Spec
batchFixExecutionSpec = do
  it "dry run mode does not modify files" $ do
    let opts = defaultBatchOptions { boDryRun = True }
    result <- runBatchFix opts []
    bfrSuccess result `shouldBe` True
    bsTotalFiles (bfrStats result) `shouldBe` 0

  it "handles empty input gracefully" $ do
    result <- runBatchFix defaultBatchOptions []
    bfrSuccess result `shouldBe` True
    Map.null (bfrFileResults result) `shouldBe` True

  it "file filter is applied correctly" $ do
    let opts = defaultBatchOptions
          { boFileFilter = Just (T.isInfixOf "Foo" . T.pack)
          , boDryRun = True
          }
        inputs = [ ("src/Foo.hs", [mkDiag "test" Warning])
                 , ("src/Bar.hs", [mkDiag "test" Warning])
                 ]
    result <- runBatchFix opts inputs
    -- Only Foo.hs should be processed
    bsTotalFiles (bfrStats result) `shouldBe` 1

  it "rule filter is applied correctly" $ do
    let filterDef = FixFilter [RuleIdMatches "partial"] FilterAll
        opts = defaultBatchOptions
          { boRuleFilter = Just filterDef
          , boDryRun = True
          }
        inputs = [ ("src/Test.hs", [ mkDiag "partial-head" Warning
                                    , mkDiag "naming-issue" Info
                                    ])
                 ]
    result <- runBatchFix opts inputs
    -- Only partial-head diagnostic should be considered
    bsTotalFiles (bfrStats result) `shouldBe` 1

  it "parallel processing option is respected" $ do
    let opts = defaultBatchOptions
          { boParallelism = 2
          , boDryRun = True
          }
    -- Just verify it doesn't crash with parallelism setting
    result <- runBatchFix opts []
    bfrSuccess result `shouldBe` True

  it "progress callback is invoked" $ do
    progressUpdates <- newTVarIO (0 :: Int)
    let callback _ = atomically $ modifyTVar' progressUpdates (+ 1)
        opts = defaultBatchOptions
          { boProgressCallback = Just callback
          , boDryRun = True
          }
    _ <- runBatchFix opts []
    updates <- readTVarIO progressUpdates
    updates `shouldSatisfy` (>= 2)  -- At least start and end

  it "records start and end times" $ do
    result <- runBatchFix defaultBatchOptions []
    -- End time should be >= start time
    bfrEndTime result >= bfrStartTime result `shouldBe` True

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Create a basic diagnostic for testing
mkDiag :: Text -> Severity -> Diagnostic
mkDiag ruleId severity = Diagnostic
  { diagSpan = SrcSpan
      { srcSpanFile = "Test.hs"
      , srcSpanStartLine = 1
      , srcSpanStartCol = 1
      , srcSpanEndLine = 1
      , srcSpanEndCol = 10
      }
  , diagSeverity = severity
  , diagCode = Just ruleId
  , diagMessage = "Test diagnostic message"
  , diagKind = CodePattern
  , diagFixes = []
  , diagRelated = []
  }

-- | Create a diagnostic with specific kind
mkDiagWithKind :: Text -> Severity -> DiagnosticKind -> Diagnostic
mkDiagWithKind ruleId severity kind = (mkDiag ruleId severity) { diagKind = kind }

-- | Create a diagnostic with specific file path
mkDiagWithPath :: Text -> Severity -> FilePath -> Diagnostic
mkDiagWithPath ruleId severity path =
  let base = mkDiag ruleId severity
      span' = (diagSpan base) { srcSpanFile = path }
  in base { diagSpan = span' }

-- | Create a diagnostic with specific message
mkDiagWithMessage :: Text -> Severity -> Text -> Diagnostic
mkDiagWithMessage ruleId severity msg = (mkDiag ruleId severity) { diagMessage = msg }

-- | Create a mock BatchFixResult for testing report generation
mkMockBatchResult :: Bool -> IO BatchFixResult
mkMockBatchResult success = do
  now <- getCurrentTime
  let stats = BatchStats
        { bsTotalFiles = 5
        , bsFilesProcessed = 5
        , bsFilesModified = 3
        , bsFilesSkipped = 1
        , bsFilesFailed = if success then 0 else 1
        , bsTotalFixes = 20
        , bsFixesApplied = 15
        , bsFixesSkipped = 3
        , bsFixesFailed = 2
        , bsConflictsFound = 1
        , bsProcessingTime = 2.5
        }
      fileResult = FileFixResult
        { ffrFilePath = "src/Test.hs"
        , ffrSuccess = success
        , ffrOutcomes = [OutcomeApplied "Test fix"]
        , ffrFixesApplied = 3
        , ffrFixesSkipped = 1
        , ffrFixesFailed = 0
        , ffrError = Nothing
        , ffrProcessTime = 0.5
        }
  pure BatchFixResult
    { bfrSuccess = success
    , bfrStats = stats
    , bfrFileResults = Map.singleton "src/Test.hs" fileResult
    , bfrStartTime = now
    , bfrEndTime = now
    , bfrOptions = defaultBatchOptions
    }
