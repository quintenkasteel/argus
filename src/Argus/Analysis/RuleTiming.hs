{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Analysis.RuleTiming
-- Description : Detailed rule timing tracking and export
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides detailed timing instrumentation for individual lint rules.
-- It supports:
--
-- * Per-rule execution time tracking
-- * Aggregated statistics (mean, median, percentiles)
-- * Export to JSON, CSV, and flamegraph formats
-- * Rule performance comparison reports
--
-- == Usage
--
-- @
-- -- Create a timing tracker
-- tracker <- newRuleTimingTracker
--
-- -- Record rule executions
-- timeRuleExecution tracker "use-fold" 0.5   -- 0.5ms
-- timeRuleExecution tracker "use-fold" 0.3
-- timeRuleExecution tracker "avoid-head" 1.2
--
-- -- Get report
-- report <- getRuleTimingReport tracker
--
-- -- Export
-- exportRuleTimingJSON tracker "timing.json"
-- exportRuleTimingCSV tracker "timing.csv"
-- exportRuleTimingFlamegraph tracker "timing.folded"
-- @
module Argus.Analysis.RuleTiming
  ( -- * Configuration
    RuleTimingConfig (..)
  , defaultRuleTimingConfig

    -- * Timing Tracker
  , RuleTimingTracker
  , newRuleTimingTracker
  , newRuleTimingTrackerWithConfig
  , resetRuleTimingTracker

    -- * Recording
  , timeRuleExecution
  , timeRuleExecutionIO
  , recordRuleTime
  , startRuleTimer
  , stopRuleTimer

    -- * Timing Data Types
  , RuleTimingSample (..)
  , RuleTimingStats (..)
  , RuleTimingReport (..)
  , TimingBreakdown (..)

    -- * Queries
  , getRuleTimingReport
  , getRuleTimingStats
  , getSlowestRules
  , getFastestRules
  , getRulesAboveThreshold
  , getTimingBreakdown

    -- * Export Functions
  , exportRuleTimingJSON
  , exportRuleTimingCSV
  , exportRuleTimingFlamegraph
  , exportRuleTimingMarkdown
  , exportRuleTimingText

    -- * Analysis
  , analyzeRulePerformance
  , detectSlowRules
  , PerformanceIssue (..)

    -- * Utilities
  , formatDuration
  , formatDurationMs
  , percentile
  ) where

import Control.Concurrent.STM
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON(..), FromJSON(..), encode)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortBy, groupBy)
import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing, Down(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for rule timing tracking
data RuleTimingConfig = RuleTimingConfig
  { rtcEnabled           :: Bool     -- ^ Whether timing is enabled
  , rtcMaxSamplesPerRule :: Int      -- ^ Maximum samples to keep per rule
  , rtcSlowThresholdMs   :: Double   -- ^ Threshold for "slow" rules (ms)
  , rtcWarnThresholdMs   :: Double   -- ^ Threshold for warnings (ms)
  , rtcIncludeCallStacks :: Bool     -- ^ Include call stack info (expensive)
  , rtcTrackFileContext  :: Bool     -- ^ Track which file each timing came from
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default timing configuration
defaultRuleTimingConfig :: RuleTimingConfig
defaultRuleTimingConfig = RuleTimingConfig
  { rtcEnabled = True
  , rtcMaxSamplesPerRule = 10000
  , rtcSlowThresholdMs = 10.0
  , rtcWarnThresholdMs = 50.0
  , rtcIncludeCallStacks = False
  , rtcTrackFileContext = True
  }

--------------------------------------------------------------------------------
-- Timing Data Types
--------------------------------------------------------------------------------

-- | A single timing sample for a rule execution
data RuleTimingSample = RuleTimingSample
  { rtsDurationMs   :: Double         -- ^ Duration in milliseconds
  , rtsTimestamp    :: UTCTime        -- ^ When this sample was recorded
  , rtsFilePath     :: Maybe FilePath -- ^ File being analyzed (if tracked)
  , rtsMatchCount   :: Int            -- ^ Number of matches found
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Aggregated statistics for a single rule
data RuleTimingStats = RuleTimingStats
  { rtsRuleId       :: Text           -- ^ Rule identifier
  , rtsCategory     :: Text           -- ^ Rule category
  , rtsTotalTimeMs  :: Double         -- ^ Total execution time
  , rtsCallCount    :: Int            -- ^ Number of times called
  , rtsMatchTotal   :: Int            -- ^ Total matches found
  , rtsMeanMs       :: Double         -- ^ Mean execution time
  , rtsMedianMs     :: Double         -- ^ Median execution time
  , rtsMinMs        :: Double         -- ^ Minimum execution time
  , rtsMaxMs        :: Double         -- ^ Maximum execution time
  , rtsStdDevMs     :: Double         -- ^ Standard deviation
  , rtsP50Ms        :: Double         -- ^ 50th percentile
  , rtsP90Ms        :: Double         -- ^ 90th percentile
  , rtsP95Ms        :: Double         -- ^ 95th percentile
  , rtsP99Ms        :: Double         -- ^ 99th percentile
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Timing breakdown by category
data TimingBreakdown = TimingBreakdown
  { tbCategory      :: Text           -- ^ Category name
  , tbTotalTimeMs   :: Double         -- ^ Total time for this category
  , tbPercentage    :: Double         -- ^ Percentage of total time
  , tbRuleCount     :: Int            -- ^ Number of rules in category
  , tbCallCount     :: Int            -- ^ Total calls in category
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Complete timing report
data RuleTimingReport = RuleTimingReport
  { rtrTotalTimeMs    :: Double           -- ^ Total analysis time
  , rtrTotalCalls     :: Int              -- ^ Total rule invocations
  , rtrTotalMatches   :: Int              -- ^ Total matches found
  , rtrRulesTracked   :: Int              -- ^ Number of rules tracked
  , rtrRuleStats      :: [RuleTimingStats] -- ^ Per-rule statistics
  , rtrCategoryBreakdown :: [TimingBreakdown] -- ^ Breakdown by category
  , rtrSlowestRules   :: [RuleTimingStats] -- ^ Top 10 slowest rules
  , rtrMostCalled     :: [RuleTimingStats] -- ^ Top 10 most called rules
  , rtrGeneratedAt    :: UTCTime          -- ^ When report was generated
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Performance issue detected
data PerformanceIssue = PerformanceIssue
  { piRuleId        :: Text           -- ^ Affected rule
  , piIssueType     :: Text           -- ^ Type of issue
  , piSeverity      :: Text           -- ^ "warning" or "error"
  , piDescription   :: Text           -- ^ Human-readable description
  , piSuggestion    :: Text           -- ^ Suggested fix
  , piMetricValue   :: Double         -- ^ The metric value that triggered this
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

--------------------------------------------------------------------------------
-- Timing Tracker
--------------------------------------------------------------------------------

-- | Internal state for timing tracking
data TimingState = TimingState
  { tsConfig        :: RuleTimingConfig
  , tsStartTime     :: UTCTime
  , tsSamples       :: Map Text [RuleTimingSample]  -- ^ Rule ID -> samples
  , tsCategories    :: Map Text Text                 -- ^ Rule ID -> category
  , tsActiveTimers  :: Map Text UTCTime              -- ^ Active timers
  }

-- | Rule timing tracker handle
newtype RuleTimingTracker = RuleTimingTracker (TVar TimingState)

-- | Create a new timing tracker with default config
newRuleTimingTracker :: IO RuleTimingTracker
newRuleTimingTracker = newRuleTimingTrackerWithConfig defaultRuleTimingConfig

-- | Create a new timing tracker with custom config
newRuleTimingTrackerWithConfig :: RuleTimingConfig -> IO RuleTimingTracker
newRuleTimingTrackerWithConfig config = do
  now <- getCurrentTime
  let initialState = TimingState
        { tsConfig = config
        , tsStartTime = now
        , tsSamples = Map.empty
        , tsCategories = Map.empty
        , tsActiveTimers = Map.empty
        }
  RuleTimingTracker <$> newTVarIO initialState

-- | Reset the timing tracker
resetRuleTimingTracker :: RuleTimingTracker -> IO ()
resetRuleTimingTracker (RuleTimingTracker tvar) = do
  now <- getCurrentTime
  atomically $ modifyTVar' tvar $ \st ->
    st { tsStartTime = now
       , tsSamples = Map.empty
       , tsActiveTimers = Map.empty
       }

--------------------------------------------------------------------------------
-- Recording Functions
--------------------------------------------------------------------------------

-- | Record a rule execution timing
recordRuleTime
  :: RuleTimingTracker
  -> Text           -- ^ Rule ID
  -> Text           -- ^ Category
  -> Double         -- ^ Duration in milliseconds
  -> Int            -- ^ Match count
  -> Maybe FilePath -- ^ File path (optional)
  -> IO ()
recordRuleTime (RuleTimingTracker tvar) ruleId category durationMs matchCount mFilePath = do
  now <- getCurrentTime
  atomically $ modifyTVar' tvar $ \st ->
    if rtcEnabled (tsConfig st)
      then
        let sample = RuleTimingSample
              { rtsDurationMs = durationMs
              , rtsTimestamp = now
              , rtsFilePath = mFilePath
              , rtsMatchCount = matchCount
              }
            maxSamples = rtcMaxSamplesPerRule (tsConfig st)
            samples' = Map.insertWith (\new old -> take maxSamples (new ++ old))
                                      ruleId [sample] (tsSamples st)
            categories' = Map.insert ruleId category (tsCategories st)
        in st { tsSamples = samples', tsCategories = categories' }
      else st

-- | Time a pure rule execution (wrapper that measures duration)
timeRuleExecution
  :: RuleTimingTracker
  -> Text           -- ^ Rule ID
  -> Text           -- ^ Category
  -> Double         -- ^ Duration in milliseconds
  -> Int            -- ^ Match count
  -> Maybe FilePath -- ^ File path
  -> IO ()
timeRuleExecution = recordRuleTime

-- | Time an IO action for a rule
timeRuleExecutionIO
  :: RuleTimingTracker
  -> Text           -- ^ Rule ID
  -> Text           -- ^ Category
  -> Maybe FilePath -- ^ File path
  -> IO (Int, a)    -- ^ Action returning (match count, result)
  -> IO a
timeRuleExecutionIO tracker ruleId category mFilePath action = do
  startTime <- getPOSIXTime
  (matchCount, result) <- action
  endTime <- getPOSIXTime
  let durationMs = realToFrac (endTime - startTime) * 1000
  recordRuleTime tracker ruleId category durationMs matchCount mFilePath
  pure result

-- | Start a timer for a rule (for bracket-style timing)
startRuleTimer :: RuleTimingTracker -> Text -> IO ()
startRuleTimer (RuleTimingTracker tvar) ruleId = do
  now <- getCurrentTime
  atomically $ modifyTVar' tvar $ \st ->
    st { tsActiveTimers = Map.insert ruleId now (tsActiveTimers st) }

-- | Stop a timer and record the duration
stopRuleTimer
  :: RuleTimingTracker
  -> Text           -- ^ Rule ID
  -> Text           -- ^ Category
  -> Int            -- ^ Match count
  -> Maybe FilePath -- ^ File path
  -> IO ()
stopRuleTimer (RuleTimingTracker tvar) ruleId category matchCount mFilePath = do
  now <- getCurrentTime
  atomically $ modifyTVar' tvar $ \st ->
    case Map.lookup ruleId (tsActiveTimers st) of
      Nothing -> st
      Just startTime ->
        let durationMs = realToFrac (diffUTCTime now startTime) * 1000
            sample = RuleTimingSample
              { rtsDurationMs = durationMs
              , rtsTimestamp = now
              , rtsFilePath = mFilePath
              , rtsMatchCount = matchCount
              }
            maxSamples = rtcMaxSamplesPerRule (tsConfig st)
            samples' = Map.insertWith (\new old -> take maxSamples (new ++ old))
                                      ruleId [sample] (tsSamples st)
            categories' = Map.insert ruleId category (tsCategories st)
            timers' = Map.delete ruleId (tsActiveTimers st)
        in st { tsSamples = samples'
              , tsCategories = categories'
              , tsActiveTimers = timers'
              }

--------------------------------------------------------------------------------
-- Query Functions
--------------------------------------------------------------------------------

-- | Get complete timing report
getRuleTimingReport :: RuleTimingTracker -> IO RuleTimingReport
getRuleTimingReport tracker@(RuleTimingTracker tvar) = do
  now <- getCurrentTime
  st <- readTVarIO tvar
  allStats <- getRuleTimingStats tracker

  let totalTimeMs = sum $ map rtsTotalTimeMs allStats
      totalCalls = sum $ map rtsCallCount allStats
      totalMatches = sum $ map rtsMatchTotal allStats
      rulesTracked = length allStats

      -- Category breakdown
      categoryGroups = groupByCategory st allStats
      categoryBreakdown = map (mkCategoryBreakdown totalTimeMs) categoryGroups

      -- Top lists
      slowest = take 10 $ sortBy (comparing (Down . rtsTotalTimeMs)) allStats
      mostCalled = take 10 $ sortBy (comparing (Down . rtsCallCount)) allStats

  pure RuleTimingReport
    { rtrTotalTimeMs = totalTimeMs
    , rtrTotalCalls = totalCalls
    , rtrTotalMatches = totalMatches
    , rtrRulesTracked = rulesTracked
    , rtrRuleStats = sortBy (comparing (Down . rtsTotalTimeMs)) allStats
    , rtrCategoryBreakdown = sortBy (comparing (Down . tbTotalTimeMs)) categoryBreakdown
    , rtrSlowestRules = slowest
    , rtrMostCalled = mostCalled
    , rtrGeneratedAt = now
    }
  where
    groupByCategory :: TimingState -> [RuleTimingStats] -> [(Text, [RuleTimingStats])]
    groupByCategory st stats =
      let withCategory s = (Map.findWithDefault "uncategorized" (rtsRuleId s) (tsCategories st), s)
          grouped = groupBy (\a b -> fst a == fst b) $
                    sortBy (comparing fst) $
                    map withCategory stats
          extractGroup :: [(Text, RuleTimingStats)] -> Maybe (Text, [RuleTimingStats])
          extractGroup [] = Nothing
          extractGroup ((cat, s):rest) = Just (cat, s : map snd rest)
      in mapMaybe extractGroup grouped

    mkCategoryBreakdown :: Double -> (Text, [RuleTimingStats]) -> TimingBreakdown
    mkCategoryBreakdown total (cat, stats) =
      let catTotal = sum $ map rtsTotalTimeMs stats
          pct = if total > 0 then (catTotal / total) * 100 else 0
      in TimingBreakdown
        { tbCategory = cat
        , tbTotalTimeMs = catTotal
        , tbPercentage = pct
        , tbRuleCount = length stats
        , tbCallCount = sum $ map rtsCallCount stats
        }

-- | Get statistics for all rules
getRuleTimingStats :: RuleTimingTracker -> IO [RuleTimingStats]
getRuleTimingStats (RuleTimingTracker tvar) = do
  st <- readTVarIO tvar
  pure $ map (computeStats st) $ Map.toList (tsSamples st)
  where
    computeStats :: TimingState -> (Text, [RuleTimingSample]) -> RuleTimingStats
    computeStats st (ruleId, samples) =
      let durations = map rtsDurationMs samples
          n = length durations
          totalTime = sum durations
          meanTime = if n > 0 then totalTime / fromIntegral n else 0
          sortedDurations = sortBy compare durations
          medianTime = if n > 0 then percentile 50 sortedDurations else 0
          minTime = if null durations then 0 else minimum durations
          maxTime = if null durations then 0 else maximum durations
          variance = if n > 1
                     then sum [(d - meanTime) ^ (2 :: Int) | d <- durations] / fromIntegral (n - 1)
                     else 0
          stdDev = sqrt variance
          category = Map.findWithDefault "uncategorized" ruleId (tsCategories st)
          totalMatches = sum $ map rtsMatchCount samples
      in RuleTimingStats
        { rtsRuleId = ruleId
        , rtsCategory = category
        , rtsTotalTimeMs = totalTime
        , rtsCallCount = n
        , rtsMatchTotal = totalMatches
        , rtsMeanMs = meanTime
        , rtsMedianMs = medianTime
        , rtsMinMs = minTime
        , rtsMaxMs = maxTime
        , rtsStdDevMs = stdDev
        , rtsP50Ms = percentile 50 sortedDurations
        , rtsP90Ms = percentile 90 sortedDurations
        , rtsP95Ms = percentile 95 sortedDurations
        , rtsP99Ms = percentile 99 sortedDurations
        }

-- | Get N slowest rules by total time
getSlowestRules :: RuleTimingTracker -> Int -> IO [RuleTimingStats]
getSlowestRules tracker n = do
  stats <- getRuleTimingStats tracker
  pure $ take n $ sortBy (comparing (Down . rtsTotalTimeMs)) stats

-- | Get N fastest rules by mean time
getFastestRules :: RuleTimingTracker -> Int -> IO [RuleTimingStats]
getFastestRules tracker n = do
  stats <- getRuleTimingStats tracker
  let withCalls = filter (\s -> rtsCallCount s > 0) stats
  pure $ take n $ sortBy (comparing rtsMeanMs) withCalls

-- | Get rules above a time threshold
getRulesAboveThreshold :: RuleTimingTracker -> Double -> IO [RuleTimingStats]
getRulesAboveThreshold tracker thresholdMs = do
  stats <- getRuleTimingStats tracker
  pure $ filter (\s -> rtsMeanMs s > thresholdMs) stats

-- | Get timing breakdown by category
getTimingBreakdown :: RuleTimingTracker -> IO [TimingBreakdown]
getTimingBreakdown tracker = do
  report <- getRuleTimingReport tracker
  pure $ rtrCategoryBreakdown report

--------------------------------------------------------------------------------
-- Export Functions
--------------------------------------------------------------------------------

-- | Export timing data to JSON file
exportRuleTimingJSON :: RuleTimingTracker -> FilePath -> IO ()
exportRuleTimingJSON tracker path = do
  report <- getRuleTimingReport tracker
  LBS.writeFile path (encode report)

-- | Export timing data to CSV file
exportRuleTimingCSV :: RuleTimingTracker -> FilePath -> IO ()
exportRuleTimingCSV tracker path = do
  stats <- getRuleTimingStats tracker
  let header = "rule_id,category,total_ms,call_count,matches,mean_ms,median_ms,min_ms,max_ms,stddev_ms,p90_ms,p95_ms,p99_ms"
      rows = map formatRow stats
      csv = T.unlines (header : rows)
  TIO.writeFile path csv
  where
    formatRow RuleTimingStats{..} = T.intercalate "," $
      [ rtsRuleId
      , rtsCategory
      , T.pack $ printf "%.3f" rtsTotalTimeMs
      , T.pack $ show rtsCallCount
      , T.pack $ show rtsMatchTotal
      , T.pack $ printf "%.3f" rtsMeanMs
      , T.pack $ printf "%.3f" rtsMedianMs
      , T.pack $ printf "%.3f" rtsMinMs
      , T.pack $ printf "%.3f" rtsMaxMs
      , T.pack $ printf "%.3f" rtsStdDevMs
      , T.pack $ printf "%.3f" rtsP90Ms
      , T.pack $ printf "%.3f" rtsP95Ms
      , T.pack $ printf "%.3f" rtsP99Ms
      ]

-- | Export timing data in flamegraph-compatible folded stack format
-- Format: "argus;category;rule_id count"
exportRuleTimingFlamegraph :: RuleTimingTracker -> FilePath -> IO ()
exportRuleTimingFlamegraph tracker path = do
  stats <- getRuleTimingStats tracker
  let lines' = map formatFlamegraph stats
  TIO.writeFile path (T.unlines lines')
  where
    formatFlamegraph RuleTimingStats{..} =
      -- Count is in microseconds (flamegraph expects integer)
      let countUs = round (rtsTotalTimeMs * 1000) :: Int
      in "argus;" <> rtsCategory <> ";" <> rtsRuleId <> " " <> T.pack (show countUs)

-- | Export timing data as Markdown report
exportRuleTimingMarkdown :: RuleTimingTracker -> FilePath -> IO ()
exportRuleTimingMarkdown tracker path = do
  report <- getRuleTimingReport tracker
  let md = formatMarkdownReport report
  TIO.writeFile path md

-- | Export timing data as plain text report
exportRuleTimingText :: RuleTimingTracker -> FilePath -> IO ()
exportRuleTimingText tracker path = do
  report <- getRuleTimingReport tracker
  let txt = formatTextReport report
  TIO.writeFile path txt

-- | Format report as Markdown
formatMarkdownReport :: RuleTimingReport -> Text
formatMarkdownReport RuleTimingReport{..} = T.unlines $
  [ "# Argus Rule Timing Report"
  , ""
  , "## Summary"
  , ""
  , "| Metric | Value |"
  , "|--------|-------|"
  , "| Total Time | " <> formatDurationMs rtrTotalTimeMs <> " |"
  , "| Total Rule Calls | " <> T.pack (show rtrTotalCalls) <> " |"
  , "| Total Matches | " <> T.pack (show rtrTotalMatches) <> " |"
  , "| Rules Tracked | " <> T.pack (show rtrRulesTracked) <> " |"
  , ""
  , "## Category Breakdown"
  , ""
  , "| Category | Time | % | Rules | Calls |"
  , "|----------|------|---|-------|-------|"
  ] ++
  map formatCategoryRow rtrCategoryBreakdown ++
  [ ""
  , "## Slowest Rules (by total time)"
  , ""
  , "| Rule | Category | Total Time | Calls | Mean | P95 |"
  , "|------|----------|------------|-------|------|-----|"
  ] ++
  map formatSlowRuleRow rtrSlowestRules ++
  [ ""
  , "## Most Called Rules"
  , ""
  , "| Rule | Category | Calls | Total Time | Mean |"
  , "|------|----------|-------|------------|------|"
  ] ++
  map formatMostCalledRow rtrMostCalled
  where
    formatCategoryRow TimingBreakdown{..} =
      "| " <> tbCategory <>
      " | " <> formatDurationMs tbTotalTimeMs <>
      " | " <> T.pack (printf "%.1f%%" tbPercentage) <>
      " | " <> T.pack (show tbRuleCount) <>
      " | " <> T.pack (show tbCallCount) <> " |"

    formatSlowRuleRow RuleTimingStats{..} =
      "| `" <> rtsRuleId <> "`" <>
      " | " <> rtsCategory <>
      " | " <> formatDurationMs rtsTotalTimeMs <>
      " | " <> T.pack (show rtsCallCount) <>
      " | " <> formatDurationMs rtsMeanMs <>
      " | " <> formatDurationMs rtsP95Ms <> " |"

    formatMostCalledRow RuleTimingStats{..} =
      "| `" <> rtsRuleId <> "`" <>
      " | " <> rtsCategory <>
      " | " <> T.pack (show rtsCallCount) <>
      " | " <> formatDurationMs rtsTotalTimeMs <>
      " | " <> formatDurationMs rtsMeanMs <> " |"

-- | Format report as plain text
formatTextReport :: RuleTimingReport -> Text
formatTextReport RuleTimingReport{..} = T.unlines $
  [ "═══════════════════════════════════════════════════════════════════════"
  , "                      ARGUS RULE TIMING REPORT"
  , "═══════════════════════════════════════════════════════════════════════"
  , ""
  , "SUMMARY"
  , "───────────────────────────────────────────────────────────────────────"
  , "  Total Time:        " <> formatDurationMs rtrTotalTimeMs
  , "  Total Rule Calls:  " <> T.pack (show rtrTotalCalls)
  , "  Total Matches:     " <> T.pack (show rtrTotalMatches)
  , "  Rules Tracked:     " <> T.pack (show rtrRulesTracked)
  , ""
  , "CATEGORY BREAKDOWN"
  , "───────────────────────────────────────────────────────────────────────"
  ] ++
  map formatCategoryLine rtrCategoryBreakdown ++
  [ ""
  , "SLOWEST RULES (by total time)"
  , "───────────────────────────────────────────────────────────────────────"
  ] ++
  zipWith formatSlowRuleLine [1..] rtrSlowestRules ++
  [ ""
  , "═══════════════════════════════════════════════════════════════════════"
  ]
  where
    formatCategoryLine TimingBreakdown{..} =
      let padded = T.justifyLeft 20 ' ' tbCategory
          timePadded = T.justifyRight 12 ' ' (formatDurationMs tbTotalTimeMs)
          pctPadded = T.justifyRight 6 ' ' (T.pack (printf "%.1f%%" tbPercentage))
      in "  " <> padded <> timePadded <> "  " <> pctPadded <>
         "  (" <> T.pack (show tbRuleCount) <> " rules)"

    formatSlowRuleLine :: Int -> RuleTimingStats -> Text
    formatSlowRuleLine n RuleTimingStats{..} =
      let numStr = T.justifyRight 2 ' ' (T.pack (show n))
          rulePadded = T.justifyLeft 30 ' ' rtsRuleId
          timePadded = T.justifyRight 12 ' ' (formatDurationMs rtsTotalTimeMs)
          callsPadded = T.justifyRight 6 ' ' (T.pack (show rtsCallCount))
      in "  " <> numStr <> ". " <> rulePadded <> timePadded <>
         "  (" <> callsPadded <> " calls)"

--------------------------------------------------------------------------------
-- Performance Analysis
--------------------------------------------------------------------------------

-- | Analyze rule performance and detect issues
analyzeRulePerformance :: RuleTimingTracker -> IO [PerformanceIssue]
analyzeRulePerformance (RuleTimingTracker tvar) = do
  st <- readTVarIO tvar
  stats <- getRuleTimingStats (RuleTimingTracker tvar)

  let config = tsConfig st
      slowThreshold = rtcSlowThresholdMs config
      warnThreshold = rtcWarnThresholdMs config

      -- Find slow rules
      slowRules = filter (\s -> rtsMeanMs s > slowThreshold) stats
      slowIssues = map (mkSlowIssue "warning" slowThreshold) slowRules

      -- Find very slow rules (errors)
      verySlowRules = filter (\s -> rtsMeanMs s > warnThreshold) stats
      verySlowIssues = map (mkSlowIssue "error" warnThreshold) verySlowRules

      -- Find high-variance rules
      highVarianceRules = filter (\s -> rtsStdDevMs s > rtsMeanMs s && rtsCallCount s > 10) stats
      varianceIssues = map mkVarianceIssue highVarianceRules

  pure $ slowIssues ++ verySlowIssues ++ varianceIssues
  where
    mkSlowIssue severity threshold RuleTimingStats{..} = PerformanceIssue
      { piRuleId = rtsRuleId
      , piIssueType = "slow-execution"
      , piSeverity = severity
      , piDescription = "Rule '" <> rtsRuleId <> "' has mean execution time of " <>
                        formatDurationMs rtsMeanMs <> " (threshold: " <>
                        formatDurationMs threshold <> ")"
      , piSuggestion = "Consider optimizing the rule pattern or enabling caching"
      , piMetricValue = rtsMeanMs
      }

    mkVarianceIssue RuleTimingStats{..} = PerformanceIssue
      { piRuleId = rtsRuleId
      , piIssueType = "high-variance"
      , piSeverity = "warning"
      , piDescription = "Rule '" <> rtsRuleId <> "' has high execution time variance " <>
                        "(stddev: " <> formatDurationMs rtsStdDevMs <>
                        ", mean: " <> formatDurationMs rtsMeanMs <> ")"
      , piSuggestion = "Investigate why execution time varies significantly"
      , piMetricValue = rtsStdDevMs
      }

-- | Detect slow rules above threshold
detectSlowRules :: RuleTimingTracker -> Double -> IO [RuleTimingStats]
detectSlowRules = getRulesAboveThreshold

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- | Format duration in human-readable form
formatDuration :: Double -> Text
formatDuration seconds
  | seconds < 0.001 = T.pack $ printf "%.0fµs" (seconds * 1000000)
  | seconds < 1.0   = T.pack $ printf "%.1fms" (seconds * 1000)
  | seconds < 60.0  = T.pack $ printf "%.2fs" seconds
  | otherwise       = T.pack $ printf "%.1fm" (seconds / 60)

-- | Format duration from milliseconds
formatDurationMs :: Double -> Text
formatDurationMs ms
  | ms < 0.001 = T.pack $ printf "%.0fns" (ms * 1000000)
  | ms < 1.0   = T.pack $ printf "%.1fµs" (ms * 1000)
  | ms < 1000  = T.pack $ printf "%.2fms" ms
  | otherwise  = T.pack $ printf "%.2fs" (ms / 1000)

-- | Calculate percentile from sorted list
percentile :: Int -> [Double] -> Double
percentile _ [] = 0
percentile p xs =
  let n = length xs
      k = (p * (n - 1)) `div` 100
      f = fromIntegral (p * (n - 1)) / 100 - fromIntegral k
      lower = xs !! k
      upper = if k + 1 < n then xs !! (k + 1) else lower
  in lower + f * (upper - lower)
