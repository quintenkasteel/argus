{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Argus.Baseline.Diff
-- Description : Baseline diffing engine for regression detection
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides the core diffing algorithm for comparing current
-- analysis results against a baseline. It detects new issues (regressions),
-- resolved issues (improvements), and tracks expired baseline entries.
module Argus.Baseline.Diff
  ( -- * Diffing Functions
    diffBaseline
  , diffBaselineWithTime
  , categorizeIssues

    -- * Matching Functions
  , findMatchingEntry
  , findMatchingDiagnostic
  , isExpired

    -- * Statistics
  , computeDiffStats
  , summarizeDiff
  ) where

import Data.List (partition, find)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)

import Argus.Types (Diagnostic (..), Severity (..), SrcSpan (..))
import Argus.Baseline.Types

--------------------------------------------------------------------------------
-- Core Diffing Algorithm
--------------------------------------------------------------------------------

-- | Diff current diagnostics against a baseline
--
-- This is the main entry point for baseline comparison. It categorizes
-- all current diagnostics into:
-- - New issues (not in baseline)
-- - Existing issues (match baseline entries)
-- - Resolved issues (in baseline but not current)
-- - Expired issues (baseline entries past expiration)
diffBaseline :: Baseline -> [Diagnostic] -> BaselineDiff
diffBaseline baseline diagnostics =
  diffBaselineWithTime baseline diagnostics Nothing

-- | Diff with explicit current time for expiration checking
diffBaselineWithTime :: Baseline -> [Diagnostic] -> Maybe UTCTime -> BaselineDiff
diffBaselineWithTime baseline diagnostics mNow =
  let entries = baselineEntries baseline

      -- Compute fingerprints for all current diagnostics
      fingerprintedDiags = [(d, computeFingerprint d) | d <- diagnostics]

      -- Partition diagnostics into matched and unmatched
      (matched, unmatched) = partitionDiagnostics entries fingerprintedDiags

      -- Find resolved issues (in baseline but not in current)
      matchedEntries = Set.fromList [e | (_, e) <- matched]
      resolved = filter (`Set.notMember` matchedEntries) entries

      -- Check for expired entries
      (expired, existing) = case mNow of
        Nothing -> ([], matched)
        Just now -> partition (isExpiredEntry now . snd) matched

      -- Build diff entries
      newEntries = [DiffEntry DiffNew (Just d) Nothing | (d, _) <- unmatched]
      existingEntries = [DiffEntry DiffExisting (Just d) (Just e) | (d, e) <- existing]
      resolvedEntries = [DiffEntry DiffResolved Nothing (Just e) | e <- resolved]
      expiredEntries = [DiffEntry DiffExpired (Just d) (Just e) | (d, e) <- expired]

      -- Compute statistics
      stats = computeDiffStats
        (length newEntries)
        (length resolvedEntries)
        (length existingEntries)
        (length expiredEntries)
        (length diagnostics)
        (length entries)
  in BaselineDiff
    { bdNew = newEntries
    , bdResolved = resolvedEntries
    , bdExisting = existingEntries
    , bdExpired = expiredEntries
    , bdStats = stats
    }

-- | Partition diagnostics into those matching baseline and those not
partitionDiagnostics
  :: [BaselineEntry]
  -> [(Diagnostic, DiagnosticFingerprint)]
  -> ([(Diagnostic, BaselineEntry)], [(Diagnostic, DiagnosticFingerprint)])
partitionDiagnostics entries fingerprintedDiags =
  go entries fingerprintedDiags [] []
  where
    go _ [] matched unmatched = (matched, unmatched)
    go remainingEntries ((d, fp):rest) matched unmatched =
      case findMatchingEntry fp remainingEntries of
        Just entry ->
          -- Remove matched entry to prevent double-matching
          let remaining = filter (/= entry) remainingEntries
          in go remaining rest ((d, entry):matched) unmatched
        Nothing ->
          go remainingEntries rest matched ((d, fp):unmatched)

--------------------------------------------------------------------------------
-- Entry Matching
--------------------------------------------------------------------------------

-- | Find a baseline entry matching a diagnostic fingerprint
findMatchingEntry :: DiagnosticFingerprint -> [BaselineEntry] -> Maybe BaselineEntry
findMatchingEntry fp = find (fingerprintMatches fp)

-- | Find a diagnostic matching a baseline entry
findMatchingDiagnostic :: BaselineEntry -> [Diagnostic] -> Maybe Diagnostic
findMatchingDiagnostic entry diagnostics =
  find (\d -> fingerprintMatches (computeFingerprint d) entry) diagnostics

-- | Check if a baseline entry is expired
isExpired :: UTCTime -> BaselineEntry -> Bool
isExpired now entry = case beExpires entry of
  Nothing -> False
  Just expires -> now > expires

-- | Check if a matched entry is expired (internal helper)
isExpiredEntry :: UTCTime -> BaselineEntry -> Bool
isExpiredEntry = isExpired

--------------------------------------------------------------------------------
-- Issue Categorization
--------------------------------------------------------------------------------

-- | Categorize a list of diagnostics based on a baseline
--
-- Returns a tuple of (new issues, baselined issues)
categorizeIssues :: Baseline -> [Diagnostic] -> ([Diagnostic], [Diagnostic])
categorizeIssues baseline diagnostics =
  let entries = baselineEntries baseline
      isBaselined d = isJust $ findMatchingEntry (computeFingerprint d) entries
  in partition (not . isBaselined) diagnostics

--------------------------------------------------------------------------------
-- Statistics Computation
--------------------------------------------------------------------------------

-- | Compute diff statistics from counts
computeDiffStats :: Int -> Int -> Int -> Int -> Int -> Int -> DiffStats
computeDiffStats newCount resolvedCount existingCount expiredCount totalCurrent totalBaseline =
  DiffStats
    { dsNewCount = newCount
    , dsResolvedCount = resolvedCount
    , dsExistingCount = existingCount
    , dsExpiredCount = expiredCount
    , dsDelta = newCount - resolvedCount
    , dsTotalCurrent = totalCurrent
    , dsTotalBaseline = totalBaseline
    }

-- | Generate a human-readable summary of a diff
summarizeDiff :: BaselineDiff -> Text
summarizeDiff diff =
  let stats = bdStats diff
      delta = dsDelta stats
      deltaText
        | delta > 0 = " (+" <> T.pack (show delta) <> " net new)"
        | delta < 0 = " (" <> T.pack (show delta) <> " net improvement)"
        | otherwise = " (no net change)"
  in T.unlines
    [ "Baseline Comparison Summary"
    , "=========================="
    , ""
    , "New issues:      " <> T.pack (show (dsNewCount stats))
    , "Resolved:        " <> T.pack (show (dsResolvedCount stats))
    , "Existing:        " <> T.pack (show (dsExistingCount stats))
    , "Expired:         " <> T.pack (show (dsExpiredCount stats))
    , ""
    , "Current total:   " <> T.pack (show (dsTotalCurrent stats))
    , "Baseline total:  " <> T.pack (show (dsTotalBaseline stats))
    , "Delta:           " <> T.pack (show delta) <> deltaText
    ]
