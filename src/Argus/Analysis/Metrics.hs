{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Argus.Analysis.Metrics
-- Description : Performance monitoring and metrics collection for Argus
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides comprehensive performance monitoring capabilities
-- for tracking memory usage, timing, and analysis metrics.
--
-- == Features
--
-- * Real-time memory monitoring during analysis
-- * Timing instrumentation for individual phases
-- * File-level and rule-level performance tracking
-- * Summary statistics generation
-- * Export to various formats (JSON, Prometheus, StatsD)
--
-- == Usage
--
-- @
-- import Argus.Analysis.Metrics
--
-- -- Create a metrics collector
-- collector <- newMetricsCollector defaultMetricsConfig
--
-- -- Time an analysis phase
-- result <- withTiming collector "syntactic-analysis" $ do
--   analyzeSyntactic files
--
-- -- Record custom metrics
-- recordCounter collector "files.analyzed" 42
-- recordGauge collector "memory.peak.mb" 256
--
-- -- Get summary
-- summary <- getMetricsSummary collector
-- @
module Argus.Analysis.Metrics
  ( -- * Configuration
    MetricsConfig (..)
  , defaultMetricsConfig

    -- * Metrics Collector
  , MetricsCollector
  , newMetricsCollector
  , closeMetricsCollector

    -- * Timing
  , withTiming
  , startTimer
  , stopTimer
  , recordDuration

    -- * Memory Monitoring
  , recordMemoryUsage
  , getMemoryStats
  , MemoryStats (..)

    -- * Counters and Gauges
  , recordCounter
  , incrementCounter
  , recordGauge
  , recordHistogram

    -- * File Metrics
  , recordFileAnalysis
  , FileMetrics (..)

    -- * Rule Metrics
  , recordRuleExecution
  , RuleMetrics (..)

    -- * Summary and Export
  , getMetricsSummary
  , MetricsSummary (..)
  , exportMetricsJSON
  , exportMetricsPrometheus
  , formatMetricsReport

    -- * Analysis Session
  , AnalysisSession (..)
  , startAnalysisSession
  , endAnalysisSession
  , withAnalysisSession
  ) where

import Control.Concurrent.STM
import Control.DeepSeq (NFData)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Aeson (ToJSON(..), encode)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing, Down(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock
import GHC.Generics (Generic)
import GHC.Stats qualified as GHC
import System.IO (hFlush, stdout)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for the metrics collector
data MetricsConfig = MetricsConfig
  { mcEnabled         :: Bool         -- ^ Whether metrics collection is enabled
  , mcVerbose         :: Bool         -- ^ Print metrics as they're collected
  , mcTrackMemory     :: Bool         -- ^ Track GHC memory statistics
  , mcTrackTiming     :: Bool         -- ^ Track timing for operations
  , mcTrackRules      :: Bool         -- ^ Track per-rule performance
  , mcTrackFiles      :: Bool         -- ^ Track per-file performance
  , mcSampleInterval  :: Int          -- ^ Memory sampling interval (ms)
  , mcMaxSamples      :: Int          -- ^ Maximum memory samples to keep
  , mcHistogramBuckets :: [Double]    -- ^ Histogram bucket boundaries (seconds)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Default metrics configuration
defaultMetricsConfig :: MetricsConfig
defaultMetricsConfig = MetricsConfig
  { mcEnabled = True
  , mcVerbose = False
  , mcTrackMemory = True
  , mcTrackTiming = True
  , mcTrackRules = True
  , mcTrackFiles = True
  , mcSampleInterval = 100  -- 100ms
  , mcMaxSamples = 1000
  , mcHistogramBuckets = [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1.0, 5.0, 10.0]
  }

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

-- | Memory statistics snapshot
data MemoryStats = MemoryStats
  { msTimestamp       :: UTCTime        -- ^ When this snapshot was taken
  , msCurrentBytes    :: Int            -- ^ Current heap size
  , msMaxBytes        :: Int            -- ^ Maximum heap size seen
  , msLiveBytes       :: Int            -- ^ Current live bytes
  , msAllocatedBytes  :: Int            -- ^ Total bytes allocated
  , msGcCount         :: Int            -- ^ Number of GC runs
  , msGcCpuTime       :: Double         -- ^ CPU time spent in GC (seconds)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, NFData)

-- | Metrics for a single file analysis
data FileMetrics = FileMetrics
  { fmPath            :: FilePath       -- ^ File path
  , fmLines           :: Int            -- ^ Number of lines
  , fmDurationMs      :: Double         -- ^ Analysis duration (milliseconds)
  , fmDiagnostics     :: Int            -- ^ Number of diagnostics found
  , fmRulesApplied    :: Int            -- ^ Number of rules evaluated
  , fmMemoryDelta     :: Int            -- ^ Memory change (bytes)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, NFData)

-- | Metrics for a single rule execution
data RuleMetrics = RuleMetrics
  { rmRuleId          :: Text           -- ^ Rule identifier
  , rmExecutions      :: Int            -- ^ Number of times executed
  , rmMatches         :: Int            -- ^ Number of matches found
  , rmTotalTimeMs     :: Double         -- ^ Total execution time (ms)
  , rmAvgTimeMs       :: Double         -- ^ Average execution time (ms)
  , rmMaxTimeMs       :: Double         -- ^ Maximum execution time (ms)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, NFData)

-- | Summary of all collected metrics
data MetricsSummary = MetricsSummary
  { msTotalDuration   :: Double         -- ^ Total analysis duration (seconds)
  , msFilesAnalyzed   :: Int            -- ^ Number of files analyzed
  , msTotalLines      :: Int            -- ^ Total lines of code
  , msTotalDiagnostics :: Int           -- ^ Total diagnostics found
  , msRulesEvaluated  :: Int            -- ^ Total rule evaluations
  , msPeakMemoryMB    :: Double         -- ^ Peak memory usage (MB)
  , msAvgMemoryMB     :: Double         -- ^ Average memory usage (MB)
  , msGcTimeSeconds   :: Double         -- ^ Total GC time (seconds)
  , msLinesPerSecond  :: Double         -- ^ Analysis throughput
  , msSlowestFiles    :: [FileMetrics]  -- ^ Top 10 slowest files
  , msSlowestRules    :: [RuleMetrics]  -- ^ Top 10 slowest rules
  , msTimingBreakdown :: Map Text Double -- ^ Time by phase (seconds)
  , msCounters        :: Map Text Int   -- ^ Custom counters
  , msGauges          :: Map Text Double -- ^ Custom gauges
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

--------------------------------------------------------------------------------
-- Metrics Collector
--------------------------------------------------------------------------------

-- | Internal state for metrics collection
data MetricsState = MetricsState
  { stConfig          :: MetricsConfig
  , stStartTime       :: UTCTime
  , stTimings         :: Map Text [Double]    -- ^ Phase -> durations
  , stCounters        :: Map Text Int
  , stGauges          :: Map Text Double
  , stHistograms      :: Map Text [Double]    -- ^ Name -> values
  , stMemorySamples   :: [MemoryStats]
  , stFileMetrics     :: [FileMetrics]
  , stRuleMetrics     :: Map Text (Int, Int, Double, Double)  -- ^ executions, matches, total, max
  , stActiveTimers    :: Map Text UTCTime
  }

-- | Metrics collector handle
newtype MetricsCollector = MetricsCollector (TVar MetricsState)

-- | Create a new metrics collector
newMetricsCollector :: MetricsConfig -> IO MetricsCollector
newMetricsCollector config = do
  now <- getCurrentTime
  let initialState = MetricsState
        { stConfig = config
        , stStartTime = now
        , stTimings = Map.empty
        , stCounters = Map.empty
        , stGauges = Map.empty
        , stHistograms = Map.empty
        , stMemorySamples = []
        , stFileMetrics = []
        , stRuleMetrics = Map.empty
        , stActiveTimers = Map.empty
        }
  MetricsCollector <$> newTVarIO initialState

-- | Close the metrics collector and cleanup resources
closeMetricsCollector :: MetricsCollector -> IO ()
closeMetricsCollector _ = pure ()  -- No cleanup needed currently

--------------------------------------------------------------------------------
-- Timing Operations
--------------------------------------------------------------------------------

-- | Run an action with timing instrumentation
--
-- @
-- result <- withTiming collector "parsing" $ parseFiles files
-- @
withTiming :: MetricsCollector -> Text -> IO a -> IO a
withTiming collector@(MetricsCollector tvar) phase action = do
  config <- stConfig <$> readTVarIO tvar
  if mcEnabled config && mcTrackTiming config
    then bracket (startTimer collector phase)
                 (\_ -> stopTimer collector phase)
                 (\_ -> action)
    else action

-- | Start a timer for a phase
startTimer :: MetricsCollector -> Text -> IO ()
startTimer (MetricsCollector tvar) phase = do
  now <- getCurrentTime
  atomically $ modifyTVar' tvar $ \st ->
    st { stActiveTimers = Map.insert phase now (stActiveTimers st) }

-- | Stop a timer and record the duration
stopTimer :: MetricsCollector -> Text -> IO ()
stopTimer (MetricsCollector tvar) phase = do
  now <- getCurrentTime
  atomically $ modifyTVar' tvar $ \st ->
    case Map.lookup phase (stActiveTimers st) of
      Nothing -> st
      Just start ->
        let duration = realToFrac (diffUTCTime now start) :: Double
            timings' = Map.insertWith (++) phase [duration] (stTimings st)
            timers' = Map.delete phase (stActiveTimers st)
        in st { stTimings = timings', stActiveTimers = timers' }

-- | Record a duration directly (in seconds)
recordDuration :: MetricsCollector -> Text -> Double -> IO ()
recordDuration (MetricsCollector tvar) phase duration = do
  atomically $ modifyTVar' tvar $ \st ->
    st { stTimings = Map.insertWith (++) phase [duration] (stTimings st) }

--------------------------------------------------------------------------------
-- Memory Monitoring
--------------------------------------------------------------------------------

-- | Record current memory usage
recordMemoryUsage :: MetricsCollector -> IO ()
recordMemoryUsage (MetricsCollector tvar) = do
  config <- stConfig <$> readTVarIO tvar
  when (mcEnabled config && mcTrackMemory config) $ do
    stats <- getMemoryStats
    atomically $ modifyTVar' tvar $ \st ->
      let samples = take (mcMaxSamples config) (stats : stMemorySamples st)
      in st { stMemorySamples = samples }

-- | Get current memory statistics from GHC runtime
getMemoryStats :: IO MemoryStats
getMemoryStats = do
  now <- getCurrentTime
  enabled <- GHC.getRTSStatsEnabled
  if enabled
    then do
      stats <- GHC.getRTSStats
      pure MemoryStats
        { msTimestamp = now
        , msCurrentBytes = fromIntegral $ GHC.gcdetails_mem_in_use_bytes (GHC.gc stats)
        , msMaxBytes = fromIntegral $ GHC.max_mem_in_use_bytes stats
        , msLiveBytes = fromIntegral $ GHC.gcdetails_live_bytes (GHC.gc stats)
        , msAllocatedBytes = fromIntegral $ GHC.allocated_bytes stats
        , msGcCount = fromIntegral $ GHC.gcs stats
        , msGcCpuTime = fromIntegral (GHC.gc_cpu_ns stats) / 1e9
        }
    else
      -- RTS stats not enabled, return zeros
      pure MemoryStats
        { msTimestamp = now
        , msCurrentBytes = 0
        , msMaxBytes = 0
        , msLiveBytes = 0
        , msAllocatedBytes = 0
        , msGcCount = 0
        , msGcCpuTime = 0
        }

--------------------------------------------------------------------------------
-- Counters and Gauges
--------------------------------------------------------------------------------

-- | Record a counter value (overwrites)
recordCounter :: MetricsCollector -> Text -> Int -> IO ()
recordCounter (MetricsCollector tvar) name value =
  atomically $ modifyTVar' tvar $ \st ->
    st { stCounters = Map.insert name value (stCounters st) }

-- | Increment a counter by 1
incrementCounter :: MetricsCollector -> Text -> IO ()
incrementCounter (MetricsCollector tvar) name =
  atomically $ modifyTVar' tvar $ \st ->
    st { stCounters = Map.insertWith (+) name 1 (stCounters st) }

-- | Record a gauge value (point-in-time measurement)
recordGauge :: MetricsCollector -> Text -> Double -> IO ()
recordGauge (MetricsCollector tvar) name value =
  atomically $ modifyTVar' tvar $ \st ->
    st { stGauges = Map.insert name value (stGauges st) }

-- | Record a histogram value
recordHistogram :: MetricsCollector -> Text -> Double -> IO ()
recordHistogram (MetricsCollector tvar) name value =
  atomically $ modifyTVar' tvar $ \st ->
    st { stHistograms = Map.insertWith (++) name [value] (stHistograms st) }

--------------------------------------------------------------------------------
-- File Metrics
--------------------------------------------------------------------------------

-- | Record metrics for a file analysis
recordFileAnalysis :: MetricsCollector -> FileMetrics -> IO ()
recordFileAnalysis (MetricsCollector tvar) metrics = do
  config <- stConfig <$> readTVarIO tvar
  when (mcEnabled config && mcTrackFiles config) $
    atomically $ modifyTVar' tvar $ \st ->
      st { stFileMetrics = metrics : stFileMetrics st }

--------------------------------------------------------------------------------
-- Rule Metrics
--------------------------------------------------------------------------------

-- | Record a rule execution
recordRuleExecution
  :: MetricsCollector
  -> Text      -- ^ Rule ID
  -> Int       -- ^ Number of matches
  -> Double    -- ^ Duration (ms)
  -> IO ()
recordRuleExecution (MetricsCollector tvar) ruleId matches durationMs = do
  config <- stConfig <$> readTVarIO tvar
  when (mcEnabled config && mcTrackRules config) $
    atomically $ modifyTVar' tvar $ \st ->
      let update (execs, totalMatches, totalTime, maxTime) =
            (execs + 1, totalMatches + matches, totalTime + durationMs, max maxTime durationMs)
          ruleMetrics' = Map.insertWith (\_ old -> update old) ruleId (1, matches, durationMs, durationMs) (stRuleMetrics st)
      in st { stRuleMetrics = ruleMetrics' }

--------------------------------------------------------------------------------
-- Summary and Export
--------------------------------------------------------------------------------

-- | Get a summary of all collected metrics
getMetricsSummary :: MetricsCollector -> IO MetricsSummary
getMetricsSummary (MetricsCollector tvar) = do
  now <- getCurrentTime
  st <- readTVarIO tvar

  let totalDuration = realToFrac (diffUTCTime now (stStartTime st)) :: Double
      fileMetrics = stFileMetrics st
      filesAnalyzed = length fileMetrics
      totalLines = sum $ map fmLines fileMetrics
      totalDiagnostics = sum $ map fmDiagnostics fileMetrics
      rulesEvaluated = sum $ map fmRulesApplied fileMetrics

      -- Memory stats
      memorySamples = stMemorySamples st
      peakMemoryMB = if null memorySamples
                     then 0
                     else fromIntegral (maximum $ map msMaxBytes memorySamples) / (1024 * 1024)
      avgMemoryMB = if null memorySamples
                    then 0
                    else fromIntegral (sum $ map msCurrentBytes memorySamples)
                         / (fromIntegral (length memorySamples) * 1024 * 1024)
      gcTime = case memorySamples of
                 (s:_) -> msGcCpuTime s
                 [] -> 0

      -- Throughput
      linesPerSecond = if totalDuration > 0
                       then fromIntegral totalLines / totalDuration
                       else 0

      -- Top 10 slowest files
      slowestFiles = take 10 $ sortBy (comparing (Down . fmDurationMs)) fileMetrics

      -- Convert rule metrics to list
      ruleMetricsList = map toRuleMetrics $ Map.toList (stRuleMetrics st)
      slowestRules = take 10 $ sortBy (comparing (Down . rmTotalTimeMs)) ruleMetricsList

      -- Timing breakdown (aggregate by phase)
      timingBreakdown = Map.map sum (stTimings st)

  pure MetricsSummary
    { msTotalDuration = totalDuration
    , msFilesAnalyzed = filesAnalyzed
    , msTotalLines = totalLines
    , msTotalDiagnostics = totalDiagnostics
    , msRulesEvaluated = rulesEvaluated
    , msPeakMemoryMB = peakMemoryMB
    , msAvgMemoryMB = avgMemoryMB
    , msGcTimeSeconds = gcTime
    , msLinesPerSecond = linesPerSecond
    , msSlowestFiles = slowestFiles
    , msSlowestRules = slowestRules
    , msTimingBreakdown = timingBreakdown
    , msCounters = stCounters st
    , msGauges = stGauges st
    }
  where
    toRuleMetrics (ruleId, (execs, matches, totalTime, maxTime)) =
      RuleMetrics
        { rmRuleId = ruleId
        , rmExecutions = execs
        , rmMatches = matches
        , rmTotalTimeMs = totalTime
        , rmAvgTimeMs = if execs > 0 then totalTime / fromIntegral execs else 0
        , rmMaxTimeMs = maxTime
        }

-- | Export metrics as JSON
exportMetricsJSON :: MetricsCollector -> IO LBS.ByteString
exportMetricsJSON collector = do
  summary <- getMetricsSummary collector
  pure $ encode summary

-- | Export metrics in Prometheus format
exportMetricsPrometheus :: MetricsCollector -> IO Text
exportMetricsPrometheus collector = do
  summary <- getMetricsSummary collector
  pure $ T.unlines $ concat
    [ -- Gauges
      [ "# HELP argus_analysis_duration_seconds Total analysis duration"
      , "# TYPE argus_analysis_duration_seconds gauge"
      , "argus_analysis_duration_seconds " <> showT (msTotalDuration summary)
      , ""
      , "# HELP argus_files_analyzed Total files analyzed"
      , "# TYPE argus_files_analyzed gauge"
      , "argus_files_analyzed " <> showT (msFilesAnalyzed summary)
      , ""
      , "# HELP argus_lines_total Total lines of code"
      , "# TYPE argus_lines_total gauge"
      , "argus_lines_total " <> showT (msTotalLines summary)
      , ""
      , "# HELP argus_diagnostics_total Total diagnostics found"
      , "# TYPE argus_diagnostics_total gauge"
      , "argus_diagnostics_total " <> showT (msTotalDiagnostics summary)
      , ""
      , "# HELP argus_memory_peak_mb Peak memory usage in MB"
      , "# TYPE argus_memory_peak_mb gauge"
      , "argus_memory_peak_mb " <> showT (msPeakMemoryMB summary)
      , ""
      , "# HELP argus_throughput_lines_per_second Analysis throughput"
      , "# TYPE argus_throughput_lines_per_second gauge"
      , "argus_throughput_lines_per_second " <> showT (msLinesPerSecond summary)
      ]
    , -- Custom counters
      concatMap formatCounter $ Map.toList (msCounters summary)
    , -- Custom gauges
      concatMap formatGauge $ Map.toList (msGauges summary)
    ]
  where
    showT :: Show a => a -> Text
    showT = T.pack . show

    formatCounter (name, value) =
      [ "# TYPE argus_" <> sanitizeName name <> " counter"
      , "argus_" <> sanitizeName name <> " " <> showT value
      ]

    formatGauge (name, value) =
      [ "# TYPE argus_" <> sanitizeName name <> " gauge"
      , "argus_" <> sanitizeName name <> " " <> showT value
      ]

    sanitizeName = T.replace "." "_" . T.replace "-" "_"

-- | Format a human-readable metrics report
formatMetricsReport :: MetricsCollector -> IO Text
formatMetricsReport collector = do
  summary <- getMetricsSummary collector
  let headerLines =
        [ "╔══════════════════════════════════════════════════════════════════════╗"
        , "║                        ARGUS METRICS REPORT                          ║"
        , "╠══════════════════════════════════════════════════════════════════════╣"
        , "║ OVERVIEW                                                             ║"
        , "╟──────────────────────────────────────────────────────────────────────╢"
        , formatLine "Total Duration" (printf "%.3f seconds" (msTotalDuration summary))
        , formatLine "Files Analyzed" (show $ msFilesAnalyzed summary)
        , formatLine "Total Lines" (show $ msTotalLines summary)
        , formatLine "Diagnostics Found" (show $ msTotalDiagnostics summary)
        , formatLine "Throughput" (printf "%.0f lines/second" (msLinesPerSecond summary))
        , "╟──────────────────────────────────────────────────────────────────────╢"
        , "║ MEMORY                                                               ║"
        , "╟──────────────────────────────────────────────────────────────────────╢"
        , formatLine "Peak Memory" (printf "%.2f MB" (msPeakMemoryMB summary))
        , formatLine "Average Memory" (printf "%.2f MB" (msAvgMemoryMB summary))
        , formatLine "GC Time" (printf "%.3f seconds" (msGcTimeSeconds summary))
        , "╟──────────────────────────────────────────────────────────────────────╢"
        , "║ TIMING BREAKDOWN                                                     ║"
        , "╟──────────────────────────────────────────────────────────────────────╢"
        ]
      timingLines =
        [ formatLine (T.unpack phase) (printf "%.3f seconds" time)
        | (phase, time) <- Map.toList (msTimingBreakdown summary)
        ]
      filesHeader =
        [ "╟──────────────────────────────────────────────────────────────────────╢"
        , "║ SLOWEST FILES                                                        ║"
        , "╟──────────────────────────────────────────────────────────────────────╢"
        ]
      filesLines =
        [ formatLine (takeEnd 40 $ fmPath fm) (printf "%.1f ms (%d lines)" (fmDurationMs fm) (fmLines fm))
        | fm <- take 5 (msSlowestFiles summary)
        ]
      footer =
        [ "╚══════════════════════════════════════════════════════════════════════╝"
        ]
  pure $ T.unlines $ concat [headerLines, timingLines, filesHeader, filesLines, footer]
  where
    formatLine label value =
      let labelPadded = T.pack $ take 30 $ label ++ repeat ' '
          valuePadded = T.pack $ take 35 $ value ++ repeat ' '
      in "║ " <> labelPadded <> " │ " <> valuePadded <> " ║"

    takeEnd n xs = drop (max 0 (length xs - n)) xs

--------------------------------------------------------------------------------
-- Analysis Session
--------------------------------------------------------------------------------

-- | Analysis session with metrics
data AnalysisSession = AnalysisSession
  { asCollector   :: MetricsCollector
  , asStartTime   :: UTCTime
  , asConfig      :: MetricsConfig
  }

-- | Start a new analysis session with metrics collection
startAnalysisSession :: MetricsConfig -> IO AnalysisSession
startAnalysisSession config = do
  collector <- newMetricsCollector config
  now <- getCurrentTime
  recordMemoryUsage collector
  pure AnalysisSession
    { asCollector = collector
    , asStartTime = now
    , asConfig = config
    }

-- | End an analysis session and return the summary
endAnalysisSession :: AnalysisSession -> IO MetricsSummary
endAnalysisSession AnalysisSession{..} = do
  recordMemoryUsage asCollector
  summary <- getMetricsSummary asCollector
  when (mcVerbose asConfig) $ do
    report <- formatMetricsReport asCollector
    putStrLn $ T.unpack report
    hFlush stdout
  closeMetricsCollector asCollector
  pure summary

-- | Run an action within an analysis session
withAnalysisSession :: MetricsConfig -> (AnalysisSession -> IO a) -> IO (a, MetricsSummary)
withAnalysisSession config action = do
  session <- startAnalysisSession config
  result <- action session
  summary <- endAnalysisSession session
  pure (result, summary)
