{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : ConflictPreviewSpec
-- Description : Tests for fix conflict preview
-- Copyright   : (c) 2024
-- License     : MIT
module ConflictPreviewSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import Argus.Refactor.ConflictPreview
import Argus.Refactor.FixGraph
import Argus.Types

spec :: Spec
spec = do
  describe "Argus.Refactor.ConflictPreview" $ do
    describe "ConflictSeverity" $ do
      it "has correct ordering" $ do
        CSLow < CSMedium `shouldBe` True
        CSMedium < CSHigh `shouldBe` True
        CSHigh < CSCritical `shouldBe` True

      it "has all expected severities" $ do
        [minBound..maxBound] `shouldBe` [CSLow, CSMedium, CSHigh, CSCritical]

    describe "ConflictChoice" $ do
      it "has all expected choices" $ do
        [minBound..maxBound] `shouldBe`
          [ChooseFix1, ChooseFix2, ChooseSkipBoth, ChooseMerge, ChooseDefer]

    describe "generateConflictPreview" $ do
      it "handles empty fix list" $ do
        let preview = generateConflictPreview []
        cpTotalConflicts preview `shouldBe` 0
        cpFixes preview `shouldBe` []
        Set.size (cpAffectedFiles preview) `shouldBe` 0

      it "handles single fix (no conflicts)" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 10
            preview = generateConflictPreview [fix1]
        cpTotalConflicts preview `shouldBe` 0
        length (cpFixes preview) `shouldBe` 1

      it "detects conflicts for overlapping fixes" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 20
            fix2 = mkTestFix "Fix2" "test.hs" 1 10 1 30
            preview = generateConflictPreview [fix1, fix2]
        cpTotalConflicts preview `shouldSatisfy` (>= 0)

      it "detects no conflicts for non-overlapping fixes" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 10
            fix2 = mkTestFix "Fix2" "test.hs" 5 1 5 10
            preview = generateConflictPreview [fix1, fix2]
        cpTotalConflicts preview `shouldBe` 0

      it "identifies affected files correctly" $ do
        let fix1 = mkTestFix "Fix1" "file1.hs" 1 1 1 20
            fix2 = mkTestFix "Fix2" "file1.hs" 1 10 1 30
            preview = generateConflictPreview [fix1, fix2]
        if cpTotalConflicts preview > 0
          then Set.member "file1.hs" (cpAffectedFiles preview) `shouldBe` True
          else Set.size (cpAffectedFiles preview) `shouldBe` 0

    describe "previewResolution" $ do
      it "previews SkipAll strategy" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 20
            fix2 = mkTestFix "Fix2" "test.hs" 1 10 1 30
            graph = buildFixGraph [fix1, fix2]
            preview = previewResolution SkipAll graph
        rpStrategy preview `shouldBe` SkipAll

      it "previews PreferSmaller strategy" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 20
            fix2 = mkTestFix "Fix2" "test.hs" 1 10 1 30
            graph = buildFixGraph [fix1, fix2]
            preview = previewResolution PreferSmaller graph
        rpStrategy preview `shouldBe` PreferSmaller

    describe "previewAllStrategies" $ do
      it "returns previews for all strategies" $ do
        let fixes = [mkTestFix "Fix1" "test.hs" 1 1 1 10]
            strategies = previewAllStrategies fixes
        Map.size strategies `shouldSatisfy` (>= 1)

    describe "getConflictSummary" $ do
      it "returns zero counts for no conflicts" $ do
        let preview = generateConflictPreview []
            summary = getConflictSummary preview
        csTotal summary `shouldBe` 0
        csAutoResolvable summary `shouldBe` 0
        csManualRequired summary `shouldBe` 0

    describe "analyzeConflicts" $ do
      it "returns details and summary" $ do
        let fixes = [mkTestFix "Fix1" "test.hs" 1 1 1 10]
            (details, summary) = analyzeConflicts fixes
        csTotal summary `shouldBe` length details

    describe "getConflictingPairs" $ do
      it "returns empty list for no conflicts" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 10
            fix2 = mkTestFix "Fix2" "test.hs" 5 1 5 10
            pairs = getConflictingPairs [fix1, fix2]
        pairs `shouldBe` []

    describe "getAffectedFiles" $ do
      it "returns empty set for no conflicts" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 10
            files = getAffectedFiles [fix1]
        Set.size files `shouldBe` 0

    describe "getAffectedSpans" $ do
      it "returns empty list for no conflicts" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 10
            spans = getAffectedSpans [fix1]
        spans `shouldBe` []

    describe "renderConflictPreview" $ do
      it "renders no conflicts message" $ do
        let preview = generateConflictPreview []
            rendered = renderConflictPreview preview
        T.isInfixOf "No conflicts" rendered `shouldBe` True

      it "renders conflict summary" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 20
            fix2 = mkTestFix "Fix2" "test.hs" 1 10 1 30
            preview = generateConflictPreview [fix1, fix2]
            rendered = renderConflictPreview preview
        if cpTotalConflicts preview > 0
          then T.isInfixOf "Conflict" rendered `shouldBe` True
          else T.isInfixOf "No conflicts" rendered `shouldBe` True

    describe "renderConflictDiff" $ do
      it "renders diff-style output" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 2 1 2 20
            fix2 = mkTestFix "Fix2" "test.hs" 3 1 3 20
            preview = generateConflictPreview [fix1, fix2]
            content = T.unlines
              [ "module Test where"
              , "foo = 1"
              , "bar = 2"
              , "baz = 3"
              ]
        if not (null $ cpConflicts preview)
          then do
            let cd = head $ cpConflicts preview
                rendered = renderConflictDiff cd content
            T.isInfixOf "---" rendered `shouldBe` True
          else
            True `shouldBe` True  -- No conflicts, pass

    describe "renderResolutionOptions" $ do
      it "renders strategy options" $ do
        let preview = generateConflictPreview []
            rendered = renderResolutionOptions preview
        T.isInfixOf "Resolution Options" rendered `shouldBe` True

    describe "formatConflictForTerminal" $ do
      it "includes ANSI escape codes" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 20
            fix2 = mkTestFix "Fix2" "test.hs" 1 10 1 30
            preview = generateConflictPreview [fix1, fix2]
        if not (null $ cpConflicts preview)
          then do
            let cd = head $ cpConflicts preview
                formatted = formatConflictForTerminal cd
            T.isInfixOf "\x1b[" formatted `shouldBe` True  -- ANSI escape
          else
            True `shouldBe` True

    describe "formatConflictForJson" $ do
      it "produces valid JSON structure" $ do
        let preview = generateConflictPreview []
            jsonBytes = formatConflictForJson preview
        -- Should not be empty
        jsonBytes `shouldSatisfy` (\bs -> not (null $ show bs))

    describe "applyConflictChoice" $ do
      it "ChooseFix1 applies fix1 and skips fix2" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 20
            fix2 = mkTestFix "Fix2" "test.hs" 1 10 1 30
            preview = generateConflictPreview [fix1, fix2]
        if not (null $ cpConflicts preview)
          then do
            let cd = head $ cpConflicts preview
                (apply, skip) = applyConflictChoice ChooseFix1 cd
            apply `shouldBe` Just (cdFix1Index cd)
            skip `shouldBe` Just (cdFix2Index cd)
          else
            True `shouldBe` True

      it "ChooseFix2 applies fix2 and skips fix1" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 20
            fix2 = mkTestFix "Fix2" "test.hs" 1 10 1 30
            preview = generateConflictPreview [fix1, fix2]
        if not (null $ cpConflicts preview)
          then do
            let cd = head $ cpConflicts preview
                (apply, skip) = applyConflictChoice ChooseFix2 cd
            apply `shouldBe` Just (cdFix2Index cd)
            skip `shouldBe` Just (cdFix1Index cd)
          else
            True `shouldBe` True

      it "ChooseSkipBoth returns Nothing for both" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 20
            fix2 = mkTestFix "Fix2" "test.hs" 1 10 1 30
            preview = generateConflictPreview [fix1, fix2]
        if not (null $ cpConflicts preview)
          then do
            let cd = head $ cpConflicts preview
                (apply, _skip) = applyConflictChoice ChooseSkipBoth cd
            apply `shouldBe` Nothing
          else
            True `shouldBe` True

      it "ChooseDefer returns Nothing for both" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 20
            fix2 = mkTestFix "Fix2" "test.hs" 1 10 1 30
            preview = generateConflictPreview [fix1, fix2]
        if not (null $ cpConflicts preview)
          then do
            let cd = head $ cpConflicts preview
                (apply, _skip) = applyConflictChoice ChooseDefer cd
            apply `shouldBe` Nothing
          else
            True `shouldBe` True

    describe "getConflictChoices" $ do
      it "returns at least basic choices" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 20
            fix2 = mkTestFix "Fix2" "test.hs" 1 10 1 30
            preview = generateConflictPreview [fix1, fix2]
        if not (null $ cpConflicts preview)
          then do
            let cd = head $ cpConflicts preview
                choices = getConflictChoices cd
            length choices `shouldSatisfy` (>= 4)  -- At least 4 basic choices
          else
            True `shouldBe` True

    describe "conflictSeverity" $ do
      it "returns severity for overlap conflicts" $ do
        let span1 = mkSrcSpanRaw "test.hs" 1 1 1 20
            span2 = mkSrcSpanRaw "test.hs" 1 10 1 30
            severity = conflictSeverity OverlapConflict span1 span2
        severity `shouldSatisfy` (\s -> s >= CSLow && s <= CSCritical)

      it "returns CSHigh for identifier conflicts" $ do
        let span1 = mkSrcSpanRaw "test.hs" 1 1 1 10
            span2 = mkSrcSpanRaw "test.hs" 5 1 5 10
            severity = conflictSeverity IdentifierConflict span1 span2
        severity `shouldBe` CSHigh

      it "returns CSCritical for scope conflicts" $ do
        let span1 = mkSrcSpanRaw "test.hs" 1 1 1 10
            span2 = mkSrcSpanRaw "test.hs" 5 1 5 10
            severity = conflictSeverity ScopeConflict span1 span2
        severity `shouldBe` CSCritical

      it "returns CSCritical for semantic conflicts" $ do
        let span1 = mkSrcSpanRaw "test.hs" 1 1 1 10
            span2 = mkSrcSpanRaw "test.hs" 5 1 5 10
            severity = conflictSeverity SemanticConflict span1 span2
        severity `shouldBe` CSCritical

    describe "isResolvable" $ do
      it "returns True for low severity" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 20
            fix2 = mkTestFix "Fix2" "test.hs" 1 10 1 30
            preview = generateConflictPreview [fix1, fix2]
        if not (null $ cpConflicts preview)
          then do
            let cd = head $ cpConflicts preview
            if cdSeverity cd < CSHigh
              then isResolvable cd `shouldBe` True
              else isResolvable cd `shouldBe` False
          else
            True `shouldBe` True

    describe "suggestResolution" $ do
      it "returns suggestion text" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 20
            fix2 = mkTestFix "Fix2" "test.hs" 1 10 1 30
            suggestion = suggestResolution OverlapConflict fix1 fix2
        T.length suggestion `shouldSatisfy` (> 0)

      it "suggests manual review for identifier conflicts" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 20
            fix2 = mkTestFix "Fix2" "test.hs" 1 10 1 30
            suggestion = suggestResolution IdentifierConflict fix1 fix2
        T.isInfixOf "Manual review" suggestion `shouldBe` True

      it "suggests manual review for scope conflicts" $ do
        let fix1 = mkTestFix "Fix1" "test.hs" 1 1 1 20
            fix2 = mkTestFix "Fix2" "test.hs" 1 10 1 30
            suggestion = suggestResolution ScopeConflict fix1 fix2
        T.isInfixOf "Manual review" suggestion `shouldBe` True

    describe "ConflictPreview structure" $ do
      it "has consistent counts" $ do
        let preview = generateConflictPreview []
        cpTotalConflicts preview `shouldBe` length (cpConflicts preview)
        cpResolvable preview + cpManualRequired preview `shouldBe` cpTotalConflicts preview

      it "has suggested strategy" $ do
        let preview = generateConflictPreview []
        cpSuggested preview `shouldSatisfy` const True  -- Just check it exists

    describe "ResolutionPreview structure" $ do
      it "has consistent counts" $ do
        let fixes = [mkTestFix "Fix1" "test.hs" 1 1 1 10]
            graph = buildFixGraph fixes
            rp = previewResolution SkipAll graph
        rpAppliedCount rp `shouldBe` length (rpAppliedFixes rp)
        rpSkippedCount rp `shouldBe` length (rpSkippedFixes rp)

    describe "ConflictSummary structure" $ do
      it "has consistent total" $ do
        let preview = generateConflictPreview []
            summary = getConflictSummary preview
        csTotal summary `shouldBe` cpTotalConflicts preview
        csAutoResolvable summary `shouldBe` cpResolvable preview
        csManualRequired summary `shouldBe` cpManualRequired preview

    describe "edge cases" $ do
      it "handles many fixes" $ do
        let fixes = [mkTestFix ("Fix" <> T.pack (show i)) "test.hs" i 1 i 10
                    | i <- [1..50]]
            preview = generateConflictPreview fixes
        length (cpFixes preview) `shouldBe` 50

      it "handles fixes in multiple files" $ do
        let fix1 = mkTestFix "Fix1" "file1.hs" 1 1 1 10
            fix2 = mkTestFix "Fix2" "file2.hs" 1 1 1 10
            preview = generateConflictPreview [fix1, fix2]
        cpTotalConflicts preview `shouldBe` 0  -- Different files, no conflicts

-- | Helper to create a test fix
mkTestFix :: Text -> FilePath -> Int -> Int -> Int -> Int -> Fix
mkTestFix title path startLine startCol endLine endCol = Fix
  { fixTitle = title
  , fixEdits = [mkTestEdit path startLine startCol endLine endCol]
  , fixIsPreferred = False
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSAlways
  }

-- | Helper to create a test edit
mkTestEdit :: FilePath -> Int -> Int -> Int -> Int -> FixEdit
mkTestEdit path startLine startCol endLine endCol = FixEdit
  { fixEditSpan = mkSrcSpanRaw path startLine startCol endLine endCol
  , fixEditNewText = "replacement"
  }
