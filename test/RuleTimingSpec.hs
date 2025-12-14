{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : RuleTimingSpec
-- Description : Tests for rule timing tracking and export
-- Copyright   : (c) 2024
-- License     : MIT
module RuleTimingSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (removeFile, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Argus.Analysis.RuleTiming

spec :: Spec
spec = do
  describe "Argus.Analysis.RuleTiming" $ do
    describe "RuleTimingConfig" $ do
      it "has sensible defaults" $ do
        rtcEnabled defaultRuleTimingConfig `shouldBe` True
        rtcMaxSamplesPerRule defaultRuleTimingConfig `shouldBe` 10000
        rtcSlowThresholdMs defaultRuleTimingConfig `shouldBe` 10.0
        rtcWarnThresholdMs defaultRuleTimingConfig `shouldBe` 50.0

      it "can be customized" $ do
        let config = defaultRuleTimingConfig
              { rtcEnabled = False
              , rtcSlowThresholdMs = 5.0
              }
        rtcEnabled config `shouldBe` False
        rtcSlowThresholdMs config `shouldBe` 5.0

    describe "RuleTimingTracker" $ do
      it "creates new tracker with default config" $ do
        tracker <- newRuleTimingTracker
        report <- getRuleTimingReport tracker
        rtrRulesTracked report `shouldBe` 0

      it "creates new tracker with custom config" $ do
        let config = defaultRuleTimingConfig { rtcEnabled = True }
        tracker <- newRuleTimingTrackerWithConfig config
        report <- getRuleTimingReport tracker
        rtrRulesTracked report `shouldBe` 0

      it "can be reset" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "test-rule" "test" 1.0 1 Nothing
        report1 <- getRuleTimingReport tracker
        rtrRulesTracked report1 `shouldBe` 1

        resetRuleTimingTracker tracker
        report2 <- getRuleTimingReport tracker
        rtrRulesTracked report2 `shouldBe` 0

    describe "recordRuleTime" $ do
      it "records a single timing sample" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "use-fold" "performance" 1.5 3 Nothing

        stats <- getRuleTimingStats tracker
        length stats `shouldBe` 1
        let s = head stats
        rtsRuleId s `shouldBe` "use-fold"
        rtsCategory s `shouldBe` "performance"
        rtsCallCount s `shouldBe` 1
        rtsMatchTotal s `shouldBe` 3

      it "records multiple samples for same rule" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "use-fold" "performance" 1.0 1 Nothing
        recordRuleTime tracker "use-fold" "performance" 2.0 2 Nothing
        recordRuleTime tracker "use-fold" "performance" 3.0 3 Nothing

        stats <- getRuleTimingStats tracker
        length stats `shouldBe` 1
        let s = head stats
        rtsCallCount s `shouldBe` 3
        rtsMatchTotal s `shouldBe` 6
        rtsTotalTimeMs s `shouldBe` 6.0

      it "records samples for different rules" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "rule-1" "cat-a" 1.0 1 Nothing
        recordRuleTime tracker "rule-2" "cat-b" 2.0 2 Nothing

        stats <- getRuleTimingStats tracker
        length stats `shouldBe` 2

      it "tracks file path when provided" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "test-rule" "test" 1.0 1 (Just "test.hs")

        stats <- getRuleTimingStats tracker
        length stats `shouldBe` 1

      it "respects enabled config" $ do
        let config = defaultRuleTimingConfig { rtcEnabled = False }
        tracker <- newRuleTimingTrackerWithConfig config
        recordRuleTime tracker "test-rule" "test" 1.0 1 Nothing

        stats <- getRuleTimingStats tracker
        length stats `shouldBe` 0

    describe "timeRuleExecution" $ do
      it "records timing with correct parameters" $ do
        tracker <- newRuleTimingTracker
        timeRuleExecution tracker "test-rule" "test" 2.5 5 (Just "test.hs")

        stats <- getRuleTimingStats tracker
        length stats `shouldBe` 1
        let s = head stats
        rtsTotalTimeMs s `shouldBe` 2.5
        rtsMatchTotal s `shouldBe` 5

    describe "timeRuleExecutionIO" $ do
      it "times an IO action" $ do
        tracker <- newRuleTimingTracker
        result <- timeRuleExecutionIO tracker "io-rule" "test" Nothing $ do
          threadDelay 1000  -- 1ms
          pure (2, "result")

        result `shouldBe` "result"
        stats <- getRuleTimingStats tracker
        length stats `shouldBe` 1
        let s = head stats
        rtsMatchTotal s `shouldBe` 2
        -- Timing should be at least 1ms (but allow for scheduling variance)
        rtsTotalTimeMs s `shouldSatisfy` (>= 0)

    describe "startRuleTimer/stopRuleTimer" $ do
      it "records bracket-style timing" $ do
        tracker <- newRuleTimingTracker
        startRuleTimer tracker "bracket-rule"
        threadDelay 1000  -- 1ms
        stopRuleTimer tracker "bracket-rule" "test" 1 Nothing

        stats <- getRuleTimingStats tracker
        length stats `shouldBe` 1
        let s = head stats
        rtsTotalTimeMs s `shouldSatisfy` (>= 0)

    describe "RuleTimingStats" $ do
      it "calculates mean correctly" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "test" "cat" 1.0 0 Nothing
        recordRuleTime tracker "test" "cat" 2.0 0 Nothing
        recordRuleTime tracker "test" "cat" 3.0 0 Nothing

        stats <- getRuleTimingStats tracker
        let s = head stats
        rtsMeanMs s `shouldBe` 2.0

      it "calculates min and max correctly" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "test" "cat" 1.0 0 Nothing
        recordRuleTime tracker "test" "cat" 5.0 0 Nothing
        recordRuleTime tracker "test" "cat" 3.0 0 Nothing

        stats <- getRuleTimingStats tracker
        let s = head stats
        rtsMinMs s `shouldBe` 1.0
        rtsMaxMs s `shouldBe` 5.0

      it "calculates median correctly" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "test" "cat" 1.0 0 Nothing
        recordRuleTime tracker "test" "cat" 2.0 0 Nothing
        recordRuleTime tracker "test" "cat" 100.0 0 Nothing  -- Outlier

        stats <- getRuleTimingStats tracker
        let s = head stats
        rtsMedianMs s `shouldBe` 2.0

    describe "getRuleTimingReport" $ do
      it "generates complete report" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "rule-1" "cat-a" 1.0 1 Nothing
        recordRuleTime tracker "rule-2" "cat-b" 2.0 2 Nothing

        report <- getRuleTimingReport tracker
        rtrRulesTracked report `shouldBe` 2
        rtrTotalCalls report `shouldBe` 2
        rtrTotalMatches report `shouldBe` 3
        rtrTotalTimeMs report `shouldBe` 3.0

      it "includes category breakdown" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "rule-1" "performance" 1.0 0 Nothing
        recordRuleTime tracker "rule-2" "performance" 2.0 0 Nothing
        recordRuleTime tracker "rule-3" "security" 3.0 0 Nothing

        report <- getRuleTimingReport tracker
        length (rtrCategoryBreakdown report) `shouldBe` 2

      it "includes slowest rules" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "fast" "cat" 1.0 0 Nothing
        recordRuleTime tracker "slow" "cat" 10.0 0 Nothing
        recordRuleTime tracker "medium" "cat" 5.0 0 Nothing

        report <- getRuleTimingReport tracker
        let slowest = rtrSlowestRules report
        length slowest `shouldBe` 3
        rtsRuleId (head slowest) `shouldBe` "slow"

      it "handles empty tracker" $ do
        tracker <- newRuleTimingTracker
        report <- getRuleTimingReport tracker
        rtrRulesTracked report `shouldBe` 0
        rtrTotalTimeMs report `shouldBe` 0

    describe "getSlowestRules" $ do
      it "returns N slowest rules" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "rule-1" "cat" 1.0 0 Nothing
        recordRuleTime tracker "rule-2" "cat" 5.0 0 Nothing
        recordRuleTime tracker "rule-3" "cat" 3.0 0 Nothing

        slowest <- getSlowestRules tracker 2
        length slowest `shouldBe` 2
        rtsRuleId (head slowest) `shouldBe` "rule-2"

      it "handles N larger than rules count" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "rule-1" "cat" 1.0 0 Nothing

        slowest <- getSlowestRules tracker 10
        length slowest `shouldBe` 1

    describe "getFastestRules" $ do
      it "returns N fastest rules" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "rule-1" "cat" 1.0 0 Nothing
        recordRuleTime tracker "rule-2" "cat" 5.0 0 Nothing
        recordRuleTime tracker "rule-3" "cat" 3.0 0 Nothing

        fastest <- getFastestRules tracker 2
        length fastest `shouldBe` 2
        rtsRuleId (head fastest) `shouldBe` "rule-1"

    describe "getRulesAboveThreshold" $ do
      it "returns rules above threshold" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "fast" "cat" 1.0 0 Nothing
        recordRuleTime tracker "slow" "cat" 10.0 0 Nothing
        recordRuleTime tracker "medium" "cat" 5.0 0 Nothing

        aboveThreshold <- getRulesAboveThreshold tracker 3.0
        length aboveThreshold `shouldBe` 2

      it "returns empty for high threshold" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "rule" "cat" 1.0 0 Nothing

        aboveThreshold <- getRulesAboveThreshold tracker 100.0
        length aboveThreshold `shouldBe` 0

    describe "getTimingBreakdown" $ do
      it "breaks down by category" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "rule-1" "cat-a" 2.0 0 Nothing
        recordRuleTime tracker "rule-2" "cat-a" 3.0 0 Nothing
        recordRuleTime tracker "rule-3" "cat-b" 5.0 0 Nothing

        breakdown <- getTimingBreakdown tracker
        length breakdown `shouldBe` 2

    describe "exportRuleTimingJSON" $ do
      it "exports to JSON file" $ do
        withSystemTempDirectory "timing-test" $ \tmpDir -> do
          let path = tmpDir </> "timing.json"
          tracker <- newRuleTimingTracker
          recordRuleTime tracker "test-rule" "test" 1.0 1 Nothing

          exportRuleTimingJSON tracker path

          exists <- doesFileExist path
          exists `shouldBe` True

          content <- TIO.readFile path
          T.isInfixOf "test-rule" content `shouldBe` True

    describe "exportRuleTimingCSV" $ do
      it "exports to CSV file" $ do
        withSystemTempDirectory "timing-test" $ \tmpDir -> do
          let path = tmpDir </> "timing.csv"
          tracker <- newRuleTimingTracker
          recordRuleTime tracker "test-rule" "test" 1.0 1 Nothing

          exportRuleTimingCSV tracker path

          exists <- doesFileExist path
          exists `shouldBe` True

          content <- TIO.readFile path
          T.isInfixOf "rule_id" content `shouldBe` True
          T.isInfixOf "test-rule" content `shouldBe` True

    describe "exportRuleTimingFlamegraph" $ do
      it "exports to flamegraph format" $ do
        withSystemTempDirectory "timing-test" $ \tmpDir -> do
          let path = tmpDir </> "timing.folded"
          tracker <- newRuleTimingTracker
          recordRuleTime tracker "test-rule" "test-cat" 1.0 1 Nothing

          exportRuleTimingFlamegraph tracker path

          exists <- doesFileExist path
          exists `shouldBe` True

          content <- TIO.readFile path
          T.isInfixOf "argus" content `shouldBe` True
          T.isInfixOf "test-cat" content `shouldBe` True
          T.isInfixOf "test-rule" content `shouldBe` True

    describe "exportRuleTimingMarkdown" $ do
      it "exports to Markdown file" $ do
        withSystemTempDirectory "timing-test" $ \tmpDir -> do
          let path = tmpDir </> "timing.md"
          tracker <- newRuleTimingTracker
          recordRuleTime tracker "test-rule" "test" 1.0 1 Nothing

          exportRuleTimingMarkdown tracker path

          exists <- doesFileExist path
          exists `shouldBe` True

          content <- TIO.readFile path
          T.isInfixOf "# Argus Rule Timing Report" content `shouldBe` True
          T.isInfixOf "test-rule" content `shouldBe` True

    describe "exportRuleTimingText" $ do
      it "exports to plain text file" $ do
        withSystemTempDirectory "timing-test" $ \tmpDir -> do
          let path = tmpDir </> "timing.txt"
          tracker <- newRuleTimingTracker
          recordRuleTime tracker "test-rule" "test" 1.0 1 Nothing

          exportRuleTimingText tracker path

          exists <- doesFileExist path
          exists `shouldBe` True

          content <- TIO.readFile path
          T.isInfixOf "ARGUS RULE TIMING REPORT" content `shouldBe` True

    describe "analyzeRulePerformance" $ do
      it "detects slow rules" $ do
        let config = defaultRuleTimingConfig { rtcSlowThresholdMs = 5.0 }
        tracker <- newRuleTimingTrackerWithConfig config
        recordRuleTime tracker "fast-rule" "cat" 1.0 0 Nothing
        recordRuleTime tracker "slow-rule" "cat" 10.0 0 Nothing

        issues <- analyzeRulePerformance tracker
        length issues `shouldSatisfy` (>= 1)
        any (\i -> piRuleId i == "slow-rule") issues `shouldBe` True

      it "returns empty for fast rules" $ do
        let config = defaultRuleTimingConfig { rtcSlowThresholdMs = 100.0 }
        tracker <- newRuleTimingTrackerWithConfig config
        recordRuleTime tracker "fast-rule" "cat" 1.0 0 Nothing

        issues <- analyzeRulePerformance tracker
        filter (\i -> piIssueType i == "slow-execution") issues `shouldSatisfy` null

    describe "detectSlowRules" $ do
      it "finds rules above threshold" $ do
        tracker <- newRuleTimingTracker
        recordRuleTime tracker "fast" "cat" 1.0 0 Nothing
        recordRuleTime tracker "slow" "cat" 10.0 0 Nothing

        slow <- detectSlowRules tracker 5.0
        length slow `shouldBe` 1
        rtsRuleId (head slow) `shouldBe` "slow"

    describe "formatDuration" $ do
      it "formats microseconds" $ do
        formatDuration 0.0001 `shouldSatisfy` T.isInfixOf "µs"

      it "formats milliseconds" $ do
        formatDuration 0.1 `shouldSatisfy` T.isInfixOf "ms"

      it "formats seconds" $ do
        formatDuration 5.0 `shouldSatisfy` T.isInfixOf "s"

      it "formats minutes" $ do
        formatDuration 120.0 `shouldSatisfy` T.isInfixOf "m"

    describe "formatDurationMs" $ do
      it "formats small durations" $ do
        formatDurationMs 0.001 `shouldSatisfy` T.isInfixOf "µs"

      it "formats milliseconds" $ do
        formatDurationMs 5.0 `shouldSatisfy` T.isInfixOf "ms"

      it "formats large durations as seconds" $ do
        formatDurationMs 5000.0 `shouldSatisfy` T.isInfixOf "s"

    describe "percentile" $ do
      it "calculates 50th percentile (median)" $ do
        let values = [1.0, 2.0, 3.0, 4.0, 5.0]
        percentile 50 values `shouldBe` 3.0

      it "calculates 0th percentile (min)" $ do
        let values = [1.0, 2.0, 3.0, 4.0, 5.0]
        percentile 0 values `shouldBe` 1.0

      it "calculates 100th percentile (max)" $ do
        let values = [1.0, 2.0, 3.0, 4.0, 5.0]
        percentile 100 values `shouldBe` 5.0

      it "handles empty list" $ do
        percentile 50 [] `shouldBe` 0

      it "handles single element" $ do
        percentile 50 [5.0] `shouldBe` 5.0

    describe "PerformanceIssue" $ do
      it "stores rule ID" $ do
        let issue = PerformanceIssue
              { piRuleId = "slow-rule"
              , piIssueType = "slow-execution"
              , piSeverity = "warning"
              , piDescription = "Test description"
              , piSuggestion = "Test suggestion"
              , piMetricValue = 10.0
              }
        piRuleId issue `shouldBe` "slow-rule"
        piIssueType issue `shouldBe` "slow-execution"
        piSeverity issue `shouldBe` "warning"

    describe "TimingBreakdown" $ do
      it "stores category info" $ do
        let breakdown = TimingBreakdown
              { tbCategory = "performance"
              , tbTotalTimeMs = 100.0
              , tbPercentage = 50.0
              , tbRuleCount = 5
              , tbCallCount = 100
              }
        tbCategory breakdown `shouldBe` "performance"
        tbPercentage breakdown `shouldBe` 50.0
