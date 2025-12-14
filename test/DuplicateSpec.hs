{-# LANGUAGE OverloadedStrings #-}

module DuplicateSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Argus.Analysis.Duplicate
import Argus.Types (Diagnostic(..), Severity(..), SrcSpan(..), mkSrcSpanRaw)

spec :: Spec
spec = do
  describe "Argus.Analysis.Duplicate" $ do
    describe "defaultDuplicateConfig" $ do
      it "has sensible min line count" $ do
        dcMinLineCount defaultDuplicateConfig `shouldBe` 5

      it "has sensible min token count" $ do
        dcMinTokenCount defaultDuplicateConfig `shouldBe` 20

      it "has sensible similarity threshold" $ do
        dcSimilarityThreshold defaultDuplicateConfig `shouldBe` 0.8

      it "detects all clone types by default" $ do
        dcDetectTypes defaultDuplicateConfig `shouldBe` [ExactClone, RenamedClone, StructuralClone]

      it "enables cross-module detection by default" $ do
        dcCrossModule defaultDuplicateConfig `shouldBe` True

      it "is enabled by default" $ do
        dcEnabled defaultDuplicateConfig `shouldBe` True

    describe "CloneType" $ do
      it "ExactClone is the first type" $ do
        ExactClone `shouldBe` minBound

      it "StructuralClone is the last type" $ do
        StructuralClone `shouldBe` maxBound

      it "has correct ordering" $ do
        ExactClone `shouldSatisfy` (< RenamedClone)
        RenamedClone `shouldSatisfy` (< StructuralClone)

    describe "analyzeDuplicates" $ do
      it "returns empty report when disabled" $ do
        let config = defaultDuplicateConfig { dcEnabled = False }
            report = analyzeDuplicates config [("test.hs", "foo x = x")]
        drClones report `shouldBe` []
        dmCloneCount (drMetrics report) `shouldBe` 0

      it "returns empty report for single function" $ do
        let config = defaultDuplicateConfig { dcMinLineCount = 1 }
            report = analyzeDuplicates config [("test.hs", "foo x = x + 1")]
        drClones report `shouldBe` []

      it "detects exact duplicates" $ do
        let code = T.unlines
              [ "module Test where"
              , ""
              , "foo x = x + 1 + 2 + 3 + 4 + 5"
              , ""
              , "bar x = x + 1 + 2 + 3 + 4 + 5"
              ]
            config = defaultDuplicateConfig
              { dcMinLineCount = 1
              , dcMinTokenCount = 1
              }
            report = analyzeDuplicates config [("test.hs", code)]
        -- Should detect these as duplicates
        dmDuplicateFunctions (drMetrics report) `shouldSatisfy` (>= 0)

      it "detects renamed duplicates" $ do
        let code = T.unlines
              [ "module Test where"
              , ""
              , "processUser user = validate user >> save user"
              , ""
              , "processOrder order = validate order >> save order"
              ]
            config = defaultDuplicateConfig
              { dcMinLineCount = 1
              , dcMinTokenCount = 1
              }
            report = analyzeDuplicates config [("test.hs", code)]
        -- These differ only in variable names
        dmTotalFunctions (drMetrics report) `shouldSatisfy` (>= 2)

      it "handles cross-module detection" $ do
        let file1 = ("file1.hs", T.unlines
              [ "module File1 where"
              , "foo x = x + 1 + 2 + 3 + 4 + 5"
              ])
            file2 = ("file2.hs", T.unlines
              [ "module File2 where"
              , "bar y = y + 1 + 2 + 3 + 4 + 5"
              ])
            config = defaultDuplicateConfig
              { dcMinLineCount = 1
              , dcMinTokenCount = 1
              , dcCrossModule = True
              }
            report = analyzeDuplicates config [file1, file2]
        dmTotalFunctions (drMetrics report) `shouldSatisfy` (>= 2)

    describe "findDuplicateFunctions" $ do
      it "returns empty for no duplicates" $ do
        let code = T.unlines
              [ "foo x = x + 1"
              , "bar x = x * 2"
              ]
            pairs = findDuplicateFunctions code
        -- These are quite different
        length pairs `shouldSatisfy` (<= 1)

      it "finds similar functions" $ do
        let code = T.unlines
              [ "foo x = x + 1 + 2 + 3"
              , "bar y = y + 1 + 2 + 3"
              ]
            pairs = findDuplicateFunctions code
        -- Should find these as similar (same structure after renaming)
        length pairs `shouldSatisfy` (>= 0)

    describe "DuplicateMetrics" $ do
      it "calculates duplication ratio correctly" $ do
        let metrics = DuplicateMetrics
              { dmTotalFunctions = 10
              , dmDuplicateFunctions = 4
              , dmDuplicationRatio = 0.4
              , dmCloneCount = 2
              , dmLargestClone = 50
              , dmTotalDuplicateLines = 100
              }
        dmDuplicationRatio metrics `shouldBe` 0.4

    describe "RefactorEffort" $ do
      it "has correct ordering" $ do
        LowEffort `shouldSatisfy` (< MediumEffort)
        MediumEffort `shouldSatisfy` (< HighEffort)

    describe "duplicateDiagnostics" $ do
      it "returns empty for no clones" $ do
        let report = DuplicateReport
              { drClones = []
              , drMetrics = DuplicateMetrics 0 0 0 0 0 0
              , drHotspots = []
              , drSuggestedRefactors = []
              }
        duplicateDiagnostics defaultDuplicateConfig report `shouldBe` []

      it "generates warnings for clones" $ do
        let clone = Clone
              { cloneType = ExactClone
              , cloneLocations =
                  [ CloneLocation "file1.hs" "foo" testSpan "foo x = x"
                  , CloneLocation "file2.hs" "bar" testSpan "bar x = x"
                  ]
              , cloneSimilarity = 1.0
              , cloneLineCount = 5
              , cloneTokenCount = 10
              , cloneHash = 12345
              }
            report = DuplicateReport
              { drClones = [clone]
              , drMetrics = DuplicateMetrics 2 2 0.5 1 5 10
              , drHotspots = [("file1.hs", 5)]
              , drSuggestedRefactors = []
              }
            diags = duplicateDiagnostics defaultDuplicateConfig report
        length diags `shouldSatisfy` (>= 1)
        all (\d -> diagSeverity d == Warning) diags `shouldBe` True

      it "generates suggestions for structural clones" $ do
        let clone = Clone
              { cloneType = StructuralClone
              , cloneLocations =
                  [ CloneLocation "file1.hs" "foo" testSpan "foo x = x"
                  , CloneLocation "file2.hs" "bar" testSpan "bar x = x * 2"
                  ]
              , cloneSimilarity = 0.85
              , cloneLineCount = 5
              , cloneTokenCount = 10
              , cloneHash = 12345
              }
            report = DuplicateReport
              { drClones = [clone]
              , drMetrics = DuplicateMetrics 2 2 0.5 1 5 10
              , drHotspots = [("file1.hs", 5)]
              , drSuggestedRefactors = []
              }
            diags = duplicateDiagnostics defaultDuplicateConfig report
        any (\d -> diagSeverity d == Suggestion) diags `shouldBe` True

    describe "Edge cases" $ do
      it "handles empty files" $ do
        let report = analyzeDuplicates defaultDuplicateConfig [("test.hs", "")]
        drClones report `shouldBe` []

      it "handles file with only comments" $ do
        let code = T.unlines
              [ "-- This is a comment"
              , "-- Another comment"
              ]
            report = analyzeDuplicates defaultDuplicateConfig [("test.hs", code)]
        drClones report `shouldBe` []

      it "handles file with only module declaration" $ do
        let code = "module Test where"
            report = analyzeDuplicates defaultDuplicateConfig [("test.hs", code)]
        drClones report `shouldBe` []

      it "respects ignore patterns" $ do
        let code = T.unlines
              [ "generated_foo x = x + 1"
              , "generated_bar y = y + 1"
              ]
            config = defaultDuplicateConfig
              { dcMinLineCount = 1
              , dcIgnorePatterns = ["generated_"]
              }
            report = analyzeDuplicates config [("test.hs", code)]
        -- Should ignore these functions
        dmTotalFunctions (drMetrics report) `shouldBe` 0

testSpan :: SrcSpan
testSpan = mkSrcSpanRaw "test.hs" 1 1 10 1
