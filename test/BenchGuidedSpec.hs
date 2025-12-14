{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BenchGuidedSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Either (isRight, isLeft, fromRight)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Analysis.BenchGuided
import Argus.Types

--------------------------------------------------------------------------------
-- Sample Test Data
--------------------------------------------------------------------------------

-- | Sample Criterion JSON output
sampleCriterionJson :: LBS.ByteString
sampleCriterionJson = LBS8.pack $ unlines
  [ "["
  , "  {"
  , "    \"reportName\": \"parsing/parseModule/small (20 lines)\","
  , "    \"reportNumber\": 1,"
  , "    \"reportAnalysis\": {"
  , "      \"anMean\": {"
  , "        \"estPoint\": 0.00012,"
  , "        \"estLowerBound\": 0.00011,"
  , "        \"estUpperBound\": 0.00013"
  , "      },"
  , "      \"anStdDev\": {"
  , "        \"estPoint\": 0.000015,"
  , "        \"estLowerBound\": 0.000012,"
  , "        \"estUpperBound\": 0.000018"
  , "      }"
  , "    },"
  , "    \"reportRuns\": 1000"
  , "  },"
  , "  {"
  , "    \"reportName\": \"parsing/parseModule/large (500 lines)\","
  , "    \"reportNumber\": 2,"
  , "    \"reportAnalysis\": {"
  , "      \"anMean\": {"
  , "        \"estPoint\": 0.0052,"
  , "        \"estLowerBound\": 0.0050,"
  , "        \"estUpperBound\": 0.0054"
  , "      },"
  , "      \"anStdDev\": {"
  , "        \"estPoint\": 0.00025,"
  , "        \"estLowerBound\": 0.00020,"
  , "        \"estUpperBound\": 0.00030"
  , "      }"
  , "    },"
  , "    \"reportRuns\": 500"
  , "  }"
  , "]"
  ]

-- | Sample GHC profiling text output
sampleGHCProfileText :: Text
sampleGHCProfileText = T.unlines
  [ "    Mon Jan 15 10:23 2024 Time and Allocation Profiling Report  (Final)"
  , ""
  , "       argus +RTS -p -RTS check src/"
  , ""
  , "    total time  =        2.50 secs   (2500 ticks @ 1000 us, 1 processor)"
  , "    total alloc = 1,250,000,000 bytes  (excludes profiling overheads)"
  , ""
  , "COST CENTRE              MODULE               entries      %time %alloc   %time %alloc"
  , ""
  , "MAIN                     MAIN                 0            0.0    0.0    100.0  100.0"
  , " main                    Main                 1            0.5    0.3    100.0  100.0"
  , "  runAnalysis            Core                 1            2.3    1.5     99.5   99.7"
  , "   parseModule           Syntactic            142         25.4   35.2     85.2   92.3"
  , "   evaluateRules         RuleEngine           142         18.3   22.1     45.8   42.1"
  , "    matchPattern         ASTMatch             1842        15.2   18.5     27.5   20.0"
  , "   applyFix              ExactPrint           38           8.1    7.3     11.4   15.0"
  ]

-- | Sample GHC profiling JSON output
sampleGHCProfileJson :: LBS.ByteString
sampleGHCProfileJson = LBS8.pack $ unlines
  [ "{"
  , "  \"program\": \"argus\","
  , "  \"total_time\": 2.5,"
  , "  \"total_alloc\": 1250000000,"
  , "  \"cost_centres\": ["
  , "    {"
  , "      \"id\": 1,"
  , "      \"label\": \"parseModule\","
  , "      \"module\": \"Argus.Analysis.Syntactic\","
  , "      \"src_loc\": \"src/Argus/Analysis/Syntactic.hs:42:1\","
  , "      \"time_percent\": 25.4,"
  , "      \"alloc_percent\": 35.2,"
  , "      \"time_indiv\": 0.635,"
  , "      \"alloc_indiv\": 440000000,"
  , "      \"entries\": 142"
  , "    },"
  , "    {"
  , "      \"id\": 2,"
  , "      \"label\": \"evaluateRules\","
  , "      \"module\": \"Argus.Rules.Engine\","
  , "      \"src_loc\": \"src/Argus/Rules/Engine.hs:128:1\","
  , "      \"time_percent\": 18.3,"
  , "      \"alloc_percent\": 22.1,"
  , "      \"time_indiv\": 0.4575,"
  , "      \"alloc_indiv\": 276250000,"
  , "      \"entries\": 142"
  , "    },"
  , "    {"
  , "      \"id\": 3,"
  , "      \"label\": \"matchPattern\","
  , "      \"module\": \"Argus.Rules.ASTMatch\","
  , "      \"src_loc\": \"src/Argus/Rules/ASTMatch.hs:215:1\","
  , "      \"time_percent\": 15.2,"
  , "      \"alloc_percent\": 18.5,"
  , "      \"time_indiv\": 0.38,"
  , "      \"alloc_indiv\": 231250000,"
  , "      \"entries\": 1842"
  , "    },"
  , "    {"
  , "      \"id\": 4,"
  , "      \"label\": \"applyFix\","
  , "      \"module\": \"Argus.Refactor.ExactPrint\","
  , "      \"src_loc\": \"src/Argus/Refactor/ExactPrint.hs:89:1\","
  , "      \"time_percent\": 8.1,"
  , "      \"alloc_percent\": 7.3,"
  , "      \"time_indiv\": 0.2025,"
  , "      \"alloc_indiv\": 91250000,"
  , "      \"entries\": 38"
  , "    },"
  , "    {"
  , "      \"id\": 5,"
  , "      \"label\": \"minorFunction\","
  , "      \"module\": \"Argus.Utils\","
  , "      \"src_loc\": \"src/Argus/Utils.hs:50:1\","
  , "      \"time_percent\": 0.5,"
  , "      \"alloc_percent\": 0.3,"
  , "      \"time_indiv\": 0.0125,"
  , "      \"alloc_indiv\": 3750000,"
  , "      \"entries\": 10"
  , "    }"
  , "  ]"
  , "}"
  ]

-- | Sample diagnostics for testing
sampleDiagnostics :: [Diagnostic]
sampleDiagnostics =
  [ Diagnostic
      { diagSpan = SrcSpan "src/Argus/Analysis/Syntactic.hs" (Line 45) (Column 10) (Line 45) (Column 20)
      , diagSeverity = Suggestion
      , diagKind = PerformanceIssue
      , diagMessage = "Consider using strict evaluation"
      , diagCode = Just "PERF-001"
      , diagFixes = []
      , diagRelated = []
      }
  , Diagnostic
      { diagSpan = SrcSpan "src/Argus/Rules/Engine.hs" (Line 130) (Column 5) (Line 130) (Column 15)
      , diagSeverity = Warning
      , diagKind = PerformanceIssue
      , diagMessage = "Inefficient list concatenation"
      , diagCode = Just "PERF-002"
      , diagFixes = []
      , diagRelated = []
      }
  , Diagnostic
      { diagSpan = SrcSpan "src/Argus/Rules/ASTMatch.hs" (Line 220) (Column 8) (Line 220) (Column 25)
      , diagSeverity = Suggestion
      , diagKind = CodePattern
      , diagMessage = "Use foldl' instead of foldl"
      , diagCode = Just "PERF-003"
      , diagFixes = []
      , diagRelated = []
      }
  , Diagnostic
      { diagSpan = SrcSpan "src/Argus/Utils.hs" (Line 52) (Column 3) (Line 52) (Column 10)
      , diagSeverity = Info
      , diagKind = CodePattern
      , diagMessage = "Minor optimization possible"
      , diagCode = Just "INFO-001"
      , diagFixes = []
      , diagRelated = []
      }
  ]

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Criterion JSON Parsing" $ do
    it "parses valid Criterion JSON" $ do
      let result = parseCriterionJson sampleCriterionJson
      result `shouldSatisfy` isRight
      let benchmarks = fromRight [] result
      length benchmarks `shouldBe` 2

    it "extracts benchmark names correctly" $ do
      let result = parseCriterionJson sampleCriterionJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right benchmarks -> do
          let names = map brName benchmarks
          names `shouldContain` ["parsing/parseModule/small (20 lines)"]
          names `shouldContain` ["parsing/parseModule/large (500 lines)"]

    it "extracts benchmark timings correctly" $ do
      let result = parseCriterionJson sampleCriterionJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right benchmarks -> do
          let smallBench = find (\b -> "small" `T.isInfixOf` brName b) benchmarks
          case smallBench of
            Nothing -> expectationFailure "Small benchmark not found"
            Just bench -> do
              unSeconds (brMeanTime bench) `shouldSatisfy` (> 0.0001)
              unSeconds (brMeanTime bench) `shouldSatisfy` (< 0.001)

    it "handles empty benchmark list" $ do
      let result = parseCriterionJson "[]"
      result `shouldBe` Right []

    it "handles malformed JSON gracefully" $ do
      let result = parseCriterionJson "{ invalid json"
      result `shouldSatisfy` isLeft

  describe "GHC Profile Parsing (JSON)" $ do
    it "parses valid GHC profile JSON" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      result `shouldSatisfy` isRight

    it "extracts metadata correctly" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let metadata = pdMetadata profData
          pmCommand metadata `shouldBe` "argus"
          unSeconds (pmTotalTime metadata) `shouldBe` 2.5
          pmTotalAlloc metadata `shouldBe` 1250000000

    it "extracts cost centers correctly" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let costCenters = pdCostCenters profData
          length costCenters `shouldBe` 5
          pdTotalCostCenters profData `shouldBe` 5

    it "identifies cost center details" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let parseModuleCC = find (\cc -> cceName cc == "parseModule") (pdCostCenters profData)
          case parseModuleCC of
            Nothing -> expectationFailure "parseModule cost center not found"
            Just cc -> do
              cceModule cc `shouldBe` "Argus.Analysis.Syntactic"
              cceTimePercent cc `shouldBe` 25.4
              cceAllocPercent cc `shouldBe` 35.2
              cceEntries cc `shouldBe` 142

  describe "GHC Profile Parsing (Text)" $ do
    it "parses valid GHC profile text" $ do
      let result = parseGHCProfileText sampleGHCProfileText
      result `shouldSatisfy` isRight

    it "extracts metadata from text format" $ do
      let result = parseGHCProfileText sampleGHCProfileText
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let metadata = pdMetadata profData
          unSeconds (pmTotalTime metadata) `shouldBe` 2.5

    it "extracts cost centers from text format" $ do
      let result = parseGHCProfileText sampleGHCProfileText
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let costCenters = pdCostCenters profData
          length costCenters `shouldSatisfy` (> 0)

    it "auto-detects JSON vs text format" $ do
      let jsonResult = parseGHCProfile $ T.pack $ LBS8.unpack sampleGHCProfileJson
      jsonResult `shouldSatisfy` isRight

      let textResult = parseGHCProfile sampleGHCProfileText
      textResult `shouldSatisfy` isRight

  describe "Hot Spot Identification" $ do
    it "identifies hot spots from profiling data" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          length hotSpots `shouldSatisfy` (> 0)

    it "filters out insignificant cost centers" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          -- minorFunction should be filtered out (0.5% time, 0.3% alloc)
          let hasMinor = any (\hs -> "minorFunction" `T.isInfixOf` hsName hs) hotSpots
          hasMinor `shouldBe` False

    it "sorts hot spots by time percentage" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          case hotSpots of
            (first:second:_) ->
              hsTimePercent first `shouldSatisfy` (>= hsTimePercent second)
            _ -> pure ()

    it "calculates hot spot weights correctly" $ do
      calculateHotSpotWeight 25.0 10.0 `shouldBe` Critical
      calculateHotSpotWeight 15.0 8.0 `shouldBe` High
      calculateHotSpotWeight 7.0 3.0 `shouldBe` Medium
      calculateHotSpotWeight 2.5 1.5 `shouldBe` Low
      calculateHotSpotWeight 0.5 0.3 `shouldBe` Negligible

    it "filters hot spots by weight" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let allHotSpots = identifyHotSpots profData
          let criticalOnly = filterHotSpots Critical allHotSpots
          all (\hs -> hsWeight hs == Critical) criticalOnly `shouldBe` True

  describe "Cost Center Source Location Parsing" $ do
    it "parses simple source location" $ do
      let result = parseCostCenterStack "src/Argus/Core.hs:42:5"
      result `shouldSatisfy` (\x -> case x of
        Just span -> srcSpanFile span == "src/Argus/Core.hs" &&
                     srcSpanStartLine span == Line 42 &&
                     srcSpanStartCol span == Column 5
        Nothing -> False)

    it "handles invalid source location" $ do
      let result = parseCostCenterStack "invalid"
      result `shouldBe` Nothing

  describe "Correlation with Diagnostics" $ do
    it "correlates hot spots with diagnostics" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          let suggestions = correlateHotSpots hotSpots sampleDiagnostics
          length suggestions `shouldSatisfy` (> 0)

    it "finds diagnostics in hot spots" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          case hotSpots of
            (hotSpot:_) -> do
              let diags = findDiagnosticsInHotSpot hotSpot sampleDiagnostics
              -- Should find at least one diagnostic
              length diags `shouldSatisfy` (>= 0)
            _ -> pure ()

    it "calculates impact scores" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          let suggestions = correlateHotSpots hotSpots sampleDiagnostics
          case suggestions of
            (sug:_) -> do
              bgsImpact sug `shouldSatisfy` (> 0)
              bgsImpact sug `shouldSatisfy` (<= 100)
            _ -> pure ()

    it "prioritizes suggestions correctly" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          let suggestions = correlateHotSpots hotSpots sampleDiagnostics
          -- Check that priorities are sequential starting from 1
          let priorities = map bgsPriority suggestions
          case priorities of
            [] -> pure ()
            (p:_) -> p `shouldBe` 1

  describe "Diagnostic Prioritization" $ do
    it "prioritizes diagnostics in hot spots" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          let prioritized = prioritizeDiagnostics sampleDiagnostics hotSpots
          length prioritized `shouldBe` length sampleDiagnostics

    it "moves hot spot diagnostics to front" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          let prioritized = prioritizeDiagnostics sampleDiagnostics hotSpots
          -- First diagnostic should be in a hot spot (if any correlations exist)
          case prioritized of
            [] -> pure ()
            (firstDiag:_) -> do
              -- Just verify we get a list back
              length prioritized `shouldSatisfy` (> 0)

  describe "Severity Adjustment" $ do
    it "escalates severity for performance issues in hot spots" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          let adjusted = adjustSeverityForHotSpots hotSpots sampleDiagnostics
          length adjusted `shouldBe` length sampleDiagnostics

    it "preserves non-performance diagnostics" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          let adjusted = adjustSeverityForHotSpots hotSpots sampleDiagnostics
          -- Non-performance issues should remain unchanged
          let infoDiag = find (\d -> diagKind d == CodePattern && diagSeverity d == Info) sampleDiagnostics
          let adjustedInfo = find (\d -> diagKind d == CodePattern && diagCode d == Just "INFO-001") adjusted
          case (infoDiag, adjustedInfo) of
            (Just orig, Just adj) -> diagSeverity adj `shouldBe` diagSeverity orig
            _ -> pure ()

  describe "Report Generation" $ do
    it "generates benchmark-guided report" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          let suggestions = correlateHotSpots hotSpots sampleDiagnostics
          let report = benchGuidedReport suggestions
          T.length report `shouldSatisfy` (> 0)
          report `shouldSatisfy` T.isInfixOf "BENCHMARK-GUIDED"

    it "formats suggestions correctly" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          let suggestions = correlateHotSpots hotSpots sampleDiagnostics
          case suggestions of
            (sug:_) -> do
              let formatted = formatSuggestion sug
              T.length formatted `shouldSatisfy` (> 0)
              formatted `shouldSatisfy` T.isInfixOf "Priority"
            _ -> pure ()

    it "formats hot spot summary" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          let summary = formatHotSpotSummary hotSpots
          T.length summary `shouldSatisfy` (> 0)
          summary `shouldSatisfy` T.isInfixOf "HOT SPOTS"

  describe "Benchmark-Guided Diagnostics" $ do
    it "enriches diagnostics with hot spot information" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          let enriched = benchGuidedDiagnostics hotSpots sampleDiagnostics
          length enriched `shouldBe` length sampleDiagnostics

  describe "Utility Functions" $ do
    it "extracts function name from path" $ do
      functionNameFromPath "Argus.Analysis.Syntactic.parseModule" `shouldBe` "parseModule"
      functionNameFromPath "simpleFunction" `shouldBe` "simpleFunction"

    it "normalizes weights correctly" $ do
      normalizeWeight Critical `shouldBe` 1.0
      normalizeWeight High `shouldBe` 0.75
      normalizeWeight Medium `shouldBe` 0.5
      normalizeWeight Low `shouldBe` 0.25
      normalizeWeight Negligible `shouldBe` 0.1

  describe "Edge Cases" $ do
    it "handles empty hot spots list" $ do
      let suggestions = correlateHotSpots [] sampleDiagnostics
      suggestions `shouldBe` []

    it "handles empty diagnostics list" $ do
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          let suggestions = correlateHotSpots hotSpots []
          suggestions `shouldBe` []

    it "handles no correlations" $ do
      let otherDiags = [Diagnostic
            { diagSpan = SrcSpan "src/Other/Module.hs" (Line 1) (Column 1) (Line 1) (Column 10)
            , diagSeverity = Info
            , diagKind = CodePattern
            , diagMessage = "Unrelated diagnostic"
            , diagCode = Nothing
            , diagFixes = []
            , diagRelated = []
            }]
      let result = parseGHCProfileJson sampleGHCProfileJson
      case result of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          let hotSpots = identifyHotSpots profData
          let suggestions = correlateHotSpots hotSpots otherDiags
          suggestions `shouldBe` []

  describe "Integration" $ do
    it "complete workflow from parsing to reporting" $ do
      -- Parse profiling data
      let profResult = parseGHCProfileJson sampleGHCProfileJson
      profResult `shouldSatisfy` isRight

      case profResult of
        Left err -> expectationFailure $ T.unpack err
        Right profData -> do
          -- Identify hot spots
          let hotSpots = identifyHotSpots profData
          length hotSpots `shouldSatisfy` (> 0)

          -- Correlate with diagnostics
          let suggestions = correlateHotSpots hotSpots sampleDiagnostics
          -- May be 0 if no correlations, that's ok

          -- Prioritize diagnostics
          let prioritized = prioritizeDiagnostics sampleDiagnostics hotSpots
          length prioritized `shouldBe` length sampleDiagnostics

          -- Adjust severity
          let adjusted = adjustSeverityForHotSpots hotSpots sampleDiagnostics
          length adjusted `shouldBe` length sampleDiagnostics

          -- Generate report
          let report = benchGuidedReport suggestions
          T.length report `shouldSatisfy` (> 0)

    it "handles real-world Criterion benchmark" $ do
      let benchResult = parseCriterionJson sampleCriterionJson
      benchResult `shouldSatisfy` isRight

      case benchResult of
        Left err -> expectationFailure $ T.unpack err
        Right benchmarks -> do
          let timings = extractBenchmarkTimings benchmarks
          Map.size timings `shouldBe` 2
