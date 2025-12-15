{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Baseline
-- Description : Baseline comparison system for CI/CD regression detection
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides the baseline comparison system for Argus.
-- Baselines capture known issues at a point in time, enabling:
--
-- * Detection of new regressions in CI pipelines
-- * Tracking of resolved issues over time
-- * Gradual codebase improvement with deadline enforcement
--
-- == Quick Start
--
-- @
-- -- Create a baseline from current diagnostics
-- baseline <- createBaseline diagnostics (Just "Initial baseline") Nothing
-- saveBaseline ".argus-baseline.json" baseline
--
-- -- Later, compare against baseline
-- baseline <- loadBaseline ".argus-baseline.json"
-- let diff = diffBaseline baseline currentDiagnostics
--
-- -- Check for regressions
-- if dsNewCount (bdStats diff) > 0
--   then exitFailure
--   else exitSuccess
-- @
--
-- == CI Integration
--
-- Use 'runCICheck' for easy CI integration:
--
-- @
-- result <- runCICheck options diagnostics
-- exitWith (crExitCode result)
-- @
module Argus.Baseline
  ( -- * Re-exports
    module Argus.Baseline.Types
  , module Argus.Baseline.Diff
  , module Argus.Baseline.IO

    -- * CI Integration
  , runCICheck
  , evaluateCIThresholds
  , formatCIResult

    -- * Convenience Functions
  , quickDiff
  , hasRegressions
  , getRegressions
  , getImprovements
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime, UTCTime)

import Argus.Types (Diagnostic (..), Severity (..))
import Argus.Baseline.Types
import Argus.Baseline.Diff
import Argus.Baseline.IO

--------------------------------------------------------------------------------
-- CI Integration
--------------------------------------------------------------------------------

-- | Run a complete CI check with baseline comparison
--
-- This function:
-- 1. Loads the baseline if specified
-- 2. Computes the diff against current diagnostics
-- 3. Evaluates thresholds and determines exit status
-- 4. Returns a structured result with exit code and message
runCICheck :: CIOptions -> [Diagnostic] -> IO CIResult
runCICheck opts diagnostics = do
  now <- getCurrentTime

  -- Load baseline if specified
  mBaseline <- case ciBaselinePath opts of
    Nothing -> pure Nothing
    Just path -> do
      result <- loadBaseline path
      case result of
        Left _err -> pure Nothing  -- No baseline, treat as fresh start
        Right b -> pure (Just b)

  -- Compute diff
  let diff = case mBaseline of
        Nothing -> emptyDiff { bdNew = [DiffEntry DiffNew (Just d) Nothing | d <- diagnostics]
                             , bdStats = computeDiffStats (length diagnostics) 0 0 0 (length diagnostics) 0
                             }
        Just b -> diffBaselineWithTime b diagnostics (Just now)

  -- Evaluate thresholds
  let (reason, exitCode) = evaluateCIThresholds opts diff diagnostics

  -- Update baseline if requested
  case (ciUpdateBaseline opts, ciBaselinePath opts) of
    (True, Just path) -> do
      newBaseline <- case mBaseline of
        Nothing -> createBaseline diagnostics Nothing Nothing
        Just b -> updateBaseline b diagnostics False Nothing
      _ <- saveBaseline path newBaseline
      pure ()
    _ -> pure ()

  pure CIResult
    { crExitReason = reason
    , crExitCode = exitCode
    , crDiff = Just diff
    , crMessage = formatExitReason reason diff
    }

-- | Evaluate CI thresholds and determine exit status
evaluateCIThresholds :: CIOptions -> BaselineDiff -> [Diagnostic] -> (ExitReason, Int)
evaluateCIThresholds opts diff diagnostics
  -- Check fail-on-new first
  | ciFailOnNew opts && dsNewCount stats > 0 =
      (ExitNewIssues (dsNewCount stats), 1)

  -- Check severity threshold
  | Just sev <- ciFailOnSeverity opts
  , any (meetsSeverity sev) diagnostics =
      (ExitSeverityThreshold, 1)

  -- Check count threshold
  | Just maxCount <- ciFailOnCount opts
  , length diagnostics > maxCount =
      (ExitCountThreshold (length diagnostics), 1)

  -- Check delta threshold
  | Just maxDelta <- ciFailOnDelta opts
  , dsDelta stats > maxDelta =
      (ExitDeltaThreshold (dsDelta stats), 1)

  -- All checks passed
  | otherwise =
      (ExitSuccess, 0)
  where
    stats = bdStats diff

    meetsSeverity threshold diag = case (threshold, diagSeverity diag) of
      (Error, Error) -> True
      (Warning, Error) -> True
      (Warning, Warning) -> True
      (Suggestion, Error) -> True
      (Suggestion, Warning) -> True
      (Suggestion, Suggestion) -> True
      (Info, _) -> True
      _ -> False

-- | Format a CI result for display
formatCIResult :: CIResult -> Text
formatCIResult result = T.unlines
  [ crMessage result
  , ""
  , case crDiff result of
      Nothing -> ""
      Just diff -> summarizeDiff diff
  ]

-- | Format an exit reason with diff context
formatExitReason :: ExitReason -> BaselineDiff -> Text
formatExitReason reason diff = case reason of
  ExitSuccess ->
    "✓ All checks passed"

  ExitIssuesFound ->
    "✗ Issues found: " <> T.pack (show (dsTotalCurrent (bdStats diff)))

  ExitNewIssues count ->
    "✗ " <> T.pack (show count) <> " new issues detected (regression)"

  ExitSeverityThreshold ->
    "✗ Issues exceed severity threshold"

  ExitCountThreshold count ->
    "✗ Issue count (" <> T.pack (show count) <> ") exceeds threshold"

  ExitDeltaThreshold delta ->
    "✗ Issue delta (+" <> T.pack (show delta) <> ") exceeds threshold"

  ExitConfigError msg ->
    "✗ Configuration error: " <> msg

  ExitParseError msg ->
    "✗ Parse error: " <> msg

--------------------------------------------------------------------------------
-- Convenience Functions
--------------------------------------------------------------------------------

-- | Quick diff without loading from file
--
-- Useful for in-memory baseline comparisons.
-- Uses a dummy timestamp since it's not used in diffing.
quickDiff :: [BaselineEntry] -> [Diagnostic] -> UTCTime -> BaselineDiff
quickDiff entries diagnostics timestamp =
  let baseline = Baseline
        { baselineVersion = currentBaselineVersion
        , baselineCreated = timestamp
        , baselineDescription = Nothing
        , baselineCommit = Nothing
        , baselineEntries = entries
        , baselineMetadata = mempty
        }
  in diffBaseline baseline diagnostics

-- | Check if there are any regressions (new issues)
hasRegressions :: BaselineDiff -> Bool
hasRegressions diff = dsNewCount (bdStats diff) > 0

-- | Get all regression diagnostics
getRegressions :: BaselineDiff -> [Diagnostic]
getRegressions diff =
  [d | DiffEntry DiffNew (Just d) _ <- bdNew diff]

-- | Get all resolved diagnostics (improvements)
getImprovements :: BaselineDiff -> [BaselineEntry]
getImprovements diff =
  [e | DiffEntry DiffResolved _ (Just e) <- bdResolved diff]
