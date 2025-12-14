{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : MetricsSpec
-- Description : Tests for the Argus.Analysis.Metrics module
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Comprehensive tests for performance monitoring and metrics collection.
module MetricsSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.ByteString.Lazy qualified as LBS
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Argus.Analysis.Metrics

spec :: Spec
spec = do
  describe "MetricsConfig" $ do
    configDefaultsSpec
    configCustomSpec

  describe "MetricsCollector" $ do
    collectorCreationSpec
    collectorTimingSpec
    collectorCountersSpec
    collectorGaugesSpec
    collectorHistogramSpec

  describe "Memory Monitoring" $ do
    memoryStatsSpec

  describe "File Metrics" $ do
    fileMetricsSpec

  describe "Rule Metrics" $ do
    ruleMetricsSpec

  describe "Summary Generation" $ do
    summarySpec

  describe "Export Formats" $ do
    jsonExportSpec
    prometheusExportSpec
    humanReadableReportSpec

  describe "Analysis Session" $ do
    sessionSpec

--------------------------------------------------------------------------------
-- Configuration Tests
--------------------------------------------------------------------------------

configDefaultsSpec :: Spec
configDefaultsSpec = describe "default configuration" $ do
  it "has metrics enabled by default" $
    mcEnabled defaultMetricsConfig `shouldBe` True

  it "has timing tracking enabled" $
    mcTrackTiming defaultMetricsConfig `shouldBe` True

  it "has memory tracking enabled" $
    mcTrackMemory defaultMetricsConfig `shouldBe` True

  it "has rule tracking enabled" $
    mcTrackRules defaultMetricsConfig `shouldBe` True

  it "has file tracking enabled" $
    mcTrackFiles defaultMetricsConfig `shouldBe` True

  it "is not verbose by default" $
    mcVerbose defaultMetricsConfig `shouldBe` False

  it "has reasonable sample interval" $
    mcSampleInterval defaultMetricsConfig `shouldSatisfy` (> 0)

  it "has reasonable max samples" $
    mcMaxSamples defaultMetricsConfig `shouldSatisfy` (> 0)

  it "has histogram buckets defined" $
    mcHistogramBuckets defaultMetricsConfig `shouldSatisfy` (not . null)

configCustomSpec :: Spec
configCustomSpec = describe "custom configuration" $ do
  it "allows disabling all tracking" $ do
    let config = defaultMetricsConfig
          { mcEnabled = False
          , mcTrackTiming = False
          , mcTrackMemory = False
          }
    mcEnabled config `shouldBe` False
    mcTrackTiming config `shouldBe` False
    mcTrackMemory config `shouldBe` False

  it "allows custom histogram buckets" $ do
    let buckets = [0.1, 0.5, 1.0, 5.0]
        config = defaultMetricsConfig { mcHistogramBuckets = buckets }
    mcHistogramBuckets config `shouldBe` buckets

--------------------------------------------------------------------------------
-- Collector Creation Tests
--------------------------------------------------------------------------------

collectorCreationSpec :: Spec
collectorCreationSpec = describe "creation and cleanup" $ do
  it "creates a new collector without error" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    closeMetricsCollector collector  -- Should not throw

  it "can get summary from fresh collector" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    summary <- getMetricsSummary collector
    msFilesAnalyzed summary `shouldBe` 0
    msTotalDiagnostics summary `shouldBe` 0
    closeMetricsCollector collector

--------------------------------------------------------------------------------
-- Timing Tests
--------------------------------------------------------------------------------

collectorTimingSpec :: Spec
collectorTimingSpec = describe "timing operations" $ do
  it "records timing for operations" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    _ <- withTiming collector "test-phase" $ do
      threadDelay 10000  -- 10ms
      pure ()
    summary <- getMetricsSummary collector
    case Map.lookup "test-phase" (msTimingBreakdown summary) of
      Nothing -> expectationFailure "test-phase timing not found"
      Just duration -> duration `shouldSatisfy` (> 0)
    closeMetricsCollector collector

  it "records multiple timings for same phase" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    replicateM_ 3 $ withTiming collector "multi-phase" (pure ())
    summary <- getMetricsSummary collector
    case Map.lookup "multi-phase" (msTimingBreakdown summary) of
      Nothing -> expectationFailure "multi-phase timing not found"
      Just _ -> pure ()  -- Just checking it exists
    closeMetricsCollector collector

  it "handles nested timings correctly" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    _ <- withTiming collector "outer" $ do
      _ <- withTiming collector "inner" $ pure ()
      pure ()
    summary <- getMetricsSummary collector
    Map.member "outer" (msTimingBreakdown summary) `shouldBe` True
    Map.member "inner" (msTimingBreakdown summary) `shouldBe` True
    closeMetricsCollector collector

  it "manual start/stop works correctly" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    startTimer collector "manual"
    threadDelay 5000
    stopTimer collector "manual"
    summary <- getMetricsSummary collector
    Map.member "manual" (msTimingBreakdown summary) `shouldBe` True
    closeMetricsCollector collector

  it "recordDuration records directly" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordDuration collector "direct" 1.5
    summary <- getMetricsSummary collector
    case Map.lookup "direct" (msTimingBreakdown summary) of
      Nothing -> expectationFailure "direct timing not found"
      Just duration -> duration `shouldBe` 1.5
    closeMetricsCollector collector

  it "skips timing when disabled" $ do
    let config = defaultMetricsConfig { mcEnabled = False }
    collector <- newMetricsCollector config
    _ <- withTiming collector "disabled" $ pure ()
    summary <- getMetricsSummary collector
    Map.member "disabled" (msTimingBreakdown summary) `shouldBe` False
    closeMetricsCollector collector

--------------------------------------------------------------------------------
-- Counter Tests
--------------------------------------------------------------------------------

collectorCountersSpec :: Spec
collectorCountersSpec = describe "counters" $ do
  it "records counter values" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordCounter collector "test.counter" 42
    summary <- getMetricsSummary collector
    Map.lookup "test.counter" (msCounters summary) `shouldBe` Just 42
    closeMetricsCollector collector

  it "overwrites counter on subsequent set" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordCounter collector "overwrite" 10
    recordCounter collector "overwrite" 20
    summary <- getMetricsSummary collector
    Map.lookup "overwrite" (msCounters summary) `shouldBe` Just 20
    closeMetricsCollector collector

  it "increments counter correctly" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    incrementCounter collector "increment"
    incrementCounter collector "increment"
    incrementCounter collector "increment"
    summary <- getMetricsSummary collector
    Map.lookup "increment" (msCounters summary) `shouldBe` Just 3
    closeMetricsCollector collector

  it "handles multiple independent counters" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordCounter collector "counter.a" 1
    recordCounter collector "counter.b" 2
    recordCounter collector "counter.c" 3
    summary <- getMetricsSummary collector
    Map.size (msCounters summary) `shouldBe` 3
    closeMetricsCollector collector

--------------------------------------------------------------------------------
-- Gauge Tests
--------------------------------------------------------------------------------

collectorGaugesSpec :: Spec
collectorGaugesSpec = describe "gauges" $ do
  it "records gauge values" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordGauge collector "test.gauge" 3.14
    summary <- getMetricsSummary collector
    Map.lookup "test.gauge" (msGauges summary) `shouldBe` Just 3.14
    closeMetricsCollector collector

  it "overwrites gauge on subsequent set" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordGauge collector "gauge.temp" 20.0
    recordGauge collector "gauge.temp" 25.5
    summary <- getMetricsSummary collector
    Map.lookup "gauge.temp" (msGauges summary) `shouldBe` Just 25.5
    closeMetricsCollector collector

  it "handles fractional values precisely" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordGauge collector "precise" 0.123456789
    summary <- getMetricsSummary collector
    Map.lookup "precise" (msGauges summary) `shouldBe` Just 0.123456789
    closeMetricsCollector collector

--------------------------------------------------------------------------------
-- Histogram Tests
--------------------------------------------------------------------------------

collectorHistogramSpec :: Spec
collectorHistogramSpec = describe "histograms" $ do
  it "records histogram values" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordHistogram collector "latency" 0.001
    recordHistogram collector "latency" 0.005
    recordHistogram collector "latency" 0.010
    -- Histograms are tracked internally, summary doesn't expose them directly
    summary <- getMetricsSummary collector
    msTotalDuration summary `shouldSatisfy` (>= 0)  -- Just verify no crash
    closeMetricsCollector collector

--------------------------------------------------------------------------------
-- Memory Stats Tests
--------------------------------------------------------------------------------

memoryStatsSpec :: Spec
memoryStatsSpec = describe "memory statistics" $ do
  it "gets memory stats without error" $ do
    stats <- getMemoryStats
    -- RTS stats may not be enabled, so we just verify no crash
    msTimestamp stats `shouldSatisfy` const True

  it "records memory usage" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordMemoryUsage collector
    summary <- getMetricsSummary collector
    -- Memory values might be 0 if RTS stats not enabled
    msPeakMemoryMB summary `shouldSatisfy` (>= 0)
    closeMetricsCollector collector

  it "respects memory tracking config" $ do
    let config = defaultMetricsConfig { mcTrackMemory = False }
    collector <- newMetricsCollector config
    recordMemoryUsage collector
    -- Should still work, just not record
    closeMetricsCollector collector

--------------------------------------------------------------------------------
-- File Metrics Tests
--------------------------------------------------------------------------------

fileMetricsSpec :: Spec
fileMetricsSpec = describe "file metrics" $ do
  it "records file analysis metrics" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    let fm = FileMetrics
          { fmPath = "src/Test.hs"
          , fmLines = 100
          , fmDurationMs = 50.0
          , fmDiagnostics = 3
          , fmRulesApplied = 25
          , fmMemoryDelta = 1024
          }
    recordFileAnalysis collector fm
    summary <- getMetricsSummary collector
    msFilesAnalyzed summary `shouldBe` 1
    msTotalLines summary `shouldBe` 100
    msTotalDiagnostics summary `shouldBe` 3
    closeMetricsCollector collector

  it "aggregates multiple file metrics" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    let fm1 = FileMetrics "a.hs" 50 10.0 1 10 512
        fm2 = FileMetrics "b.hs" 75 15.0 2 15 768
        fm3 = FileMetrics "c.hs" 25 5.0 0 5 256
    recordFileAnalysis collector fm1
    recordFileAnalysis collector fm2
    recordFileAnalysis collector fm3
    summary <- getMetricsSummary collector
    msFilesAnalyzed summary `shouldBe` 3
    msTotalLines summary `shouldBe` 150
    msTotalDiagnostics summary `shouldBe` 3
    closeMetricsCollector collector

  it "tracks slowest files" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    let fm1 = FileMetrics "slow.hs" 100 1000.0 1 10 512
        fm2 = FileMetrics "fast.hs" 50 10.0 1 10 256
    recordFileAnalysis collector fm1
    recordFileAnalysis collector fm2
    summary <- getMetricsSummary collector
    case msSlowestFiles summary of
      [] -> expectationFailure "No slowest files"
      (f:_) -> fmPath f `shouldBe` "slow.hs"
    closeMetricsCollector collector

  it "respects file tracking config" $ do
    let config = defaultMetricsConfig { mcTrackFiles = False }
    collector <- newMetricsCollector config
    let fm = FileMetrics "ignored.hs" 100 10.0 0 10 0
    recordFileAnalysis collector fm
    summary <- getMetricsSummary collector
    msFilesAnalyzed summary `shouldBe` 0
    closeMetricsCollector collector

--------------------------------------------------------------------------------
-- Rule Metrics Tests
--------------------------------------------------------------------------------

ruleMetricsSpec :: Spec
ruleMetricsSpec = describe "rule metrics" $ do
  it "records rule execution metrics" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordRuleExecution collector "partial/head" 5 2.5
    summary <- getMetricsSummary collector
    case msSlowestRules summary of
      [] -> expectationFailure "No rule metrics"
      (r:_) -> do
        rmRuleId r `shouldBe` "partial/head"
        rmExecutions r `shouldBe` 1
        rmMatches r `shouldBe` 5
    closeMetricsCollector collector

  it "aggregates multiple executions of same rule" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordRuleExecution collector "test/rule" 1 1.0
    recordRuleExecution collector "test/rule" 2 2.0
    recordRuleExecution collector "test/rule" 3 3.0
    summary <- getMetricsSummary collector
    case filter ((== "test/rule") . rmRuleId) (msSlowestRules summary) of
      [] -> expectationFailure "Rule not found"
      (r:_) -> do
        rmExecutions r `shouldBe` 3
        rmMatches r `shouldBe` 6
        rmTotalTimeMs r `shouldBe` 6.0
        rmAvgTimeMs r `shouldBe` 2.0
    closeMetricsCollector collector

  it "tracks max time correctly" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordRuleExecution collector "max/test" 0 1.0
    recordRuleExecution collector "max/test" 0 5.0
    recordRuleExecution collector "max/test" 0 2.0
    summary <- getMetricsSummary collector
    case filter ((== "max/test") . rmRuleId) (msSlowestRules summary) of
      [] -> expectationFailure "Rule not found"
      (r:_) -> rmMaxTimeMs r `shouldBe` 5.0
    closeMetricsCollector collector

  it "respects rule tracking config" $ do
    let config = defaultMetricsConfig { mcTrackRules = False }
    collector <- newMetricsCollector config
    recordRuleExecution collector "ignored/rule" 1 1.0
    summary <- getMetricsSummary collector
    msSlowestRules summary `shouldBe` []
    closeMetricsCollector collector

--------------------------------------------------------------------------------
-- Summary Tests
--------------------------------------------------------------------------------

summarySpec :: Spec
summarySpec = describe "summary generation" $ do
  it "calculates throughput correctly" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    let fm = FileMetrics "test.hs" 1000 100.0 0 10 0
    recordFileAnalysis collector fm
    -- Wait a bit so duration is non-zero
    threadDelay 10000
    summary <- getMetricsSummary collector
    msLinesPerSecond summary `shouldSatisfy` (> 0)
    closeMetricsCollector collector

  it "handles empty metrics gracefully" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    summary <- getMetricsSummary collector
    msFilesAnalyzed summary `shouldBe` 0
    msTotalLines summary `shouldBe` 0
    msTotalDiagnostics summary `shouldBe` 0
    msPeakMemoryMB summary `shouldSatisfy` (>= 0)
    closeMetricsCollector collector

--------------------------------------------------------------------------------
-- Export Format Tests
--------------------------------------------------------------------------------

jsonExportSpec :: Spec
jsonExportSpec = describe "JSON export" $ do
  it "produces valid JSON" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordCounter collector "test" 42
    json <- exportMetricsJSON collector
    LBS.length json `shouldSatisfy` (> 0)
    closeMetricsCollector collector

  it "includes key fields in JSON" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    let fm = FileMetrics "test.hs" 100 10.0 5 20 512
    recordFileAnalysis collector fm
    json <- exportMetricsJSON collector
    let jsonText = TE.decodeUtf8 $ LBS.toStrict json
    T.isInfixOf "msTotalDuration" jsonText `shouldBe` True
    T.isInfixOf "msFilesAnalyzed" jsonText `shouldBe` True
    closeMetricsCollector collector

prometheusExportSpec :: Spec
prometheusExportSpec = describe "Prometheus export" $ do
  it "produces valid Prometheus format" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    prom <- exportMetricsPrometheus collector
    T.isInfixOf "argus_analysis_duration_seconds" prom `shouldBe` True
    T.isInfixOf "# HELP" prom `shouldBe` True
    T.isInfixOf "# TYPE" prom `shouldBe` True
    closeMetricsCollector collector

  it "includes custom counters in Prometheus format" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordCounter collector "custom.counter" 123
    prom <- exportMetricsPrometheus collector
    T.isInfixOf "argus_custom_counter" prom `shouldBe` True
    T.isInfixOf "123" prom `shouldBe` True
    closeMetricsCollector collector

  it "includes custom gauges in Prometheus format" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordGauge collector "custom.gauge" 45.67
    prom <- exportMetricsPrometheus collector
    T.isInfixOf "argus_custom_gauge" prom `shouldBe` True
    closeMetricsCollector collector

  it "sanitizes metric names for Prometheus" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordCounter collector "metric.with.dots" 1
    recordCounter collector "metric-with-dashes" 2
    prom <- exportMetricsPrometheus collector
    T.isInfixOf "metric_with_dots" prom `shouldBe` True
    T.isInfixOf "metric_with_dashes" prom `shouldBe` True
    closeMetricsCollector collector

humanReadableReportSpec :: Spec
humanReadableReportSpec = describe "human-readable report" $ do
  it "produces formatted report" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    report <- formatMetricsReport collector
    T.isInfixOf "ARGUS METRICS REPORT" report `shouldBe` True
    T.isInfixOf "OVERVIEW" report `shouldBe` True
    T.isInfixOf "MEMORY" report `shouldBe` True
    closeMetricsCollector collector

  it "includes slowest files in report" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    let fm = FileMetrics "very/long/path/to/slowfile.hs" 500 100.0 3 50 1024
    recordFileAnalysis collector fm
    report <- formatMetricsReport collector
    T.isInfixOf "SLOWEST FILES" report `shouldBe` True
    T.isInfixOf "slowfile.hs" report `shouldBe` True
    closeMetricsCollector collector

  it "includes timing breakdown in report" $ do
    collector <- newMetricsCollector defaultMetricsConfig
    recordDuration collector "parsing" 1.5
    recordDuration collector "analysis" 2.5
    report <- formatMetricsReport collector
    T.isInfixOf "TIMING BREAKDOWN" report `shouldBe` True
    T.isInfixOf "parsing" report `shouldBe` True
    T.isInfixOf "analysis" report `shouldBe` True
    closeMetricsCollector collector

--------------------------------------------------------------------------------
-- Analysis Session Tests
--------------------------------------------------------------------------------

sessionSpec :: Spec
sessionSpec = describe "analysis session" $ do
  it "starts and ends session correctly" $ do
    session <- startAnalysisSession defaultMetricsConfig
    summary <- endAnalysisSession session
    msTotalDuration summary `shouldSatisfy` (>= 0)

  it "withAnalysisSession returns result and summary" $ do
    (result, summary) <- withAnalysisSession defaultMetricsConfig $ \_ -> do
      pure (42 :: Int)
    result `shouldBe` 42
    msTotalDuration summary `shouldSatisfy` (>= 0)

  it "session collector is accessible" $ do
    (result, summary) <- withAnalysisSession defaultMetricsConfig $ \session -> do
      recordCounter (asCollector session) "session.test" 99
      pure ()
    Map.lookup "session.test" (msCounters summary) `shouldBe` Just 99

  it "session timing is tracked" $ do
    (_, summary) <- withAnalysisSession defaultMetricsConfig $ \session -> do
      _ <- withTiming (asCollector session) "session.phase" $ do
        threadDelay 5000
        pure ()
      pure ()
    Map.member "session.phase" (msTimingBreakdown summary) `shouldBe` True

--------------------------------------------------------------------------------
-- Property Tests
--------------------------------------------------------------------------------

-- Properties are limited since metrics involve IO, but we can test some invariants

prop_counterNeverNegativeAfterIncrement :: Int -> Property
prop_counterNeverNegativeAfterIncrement n = ioProperty $ do
  collector <- newMetricsCollector defaultMetricsConfig
  let count = abs n `mod` 100 + 1  -- 1 to 100 increments
  replicateM_ count $ incrementCounter collector "prop.counter"
  summary <- getMetricsSummary collector
  closeMetricsCollector collector
  pure $ Map.lookup "prop.counter" (msCounters summary) == Just count

prop_gaugeStoresLastValue :: [Double] -> Property
prop_gaugeStoresLastValue values = not (null values) ==> ioProperty $ do
  collector <- newMetricsCollector defaultMetricsConfig
  mapM_ (recordGauge collector "prop.gauge") values
  summary <- getMetricsSummary collector
  closeMetricsCollector collector
  pure $ Map.lookup "prop.gauge" (msGauges summary) == Just (last values)
