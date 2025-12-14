{-# LANGUAGE OverloadedStrings #-}

module SuppressionSpec (spec) where

import Test.Hspec
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

import Argus.Suppression
import Argus.Types

-- | Create a test diagnostic at a specific line
mkTestDiag :: Int -> Text -> Text -> Diagnostic
mkTestDiag line code msg = Diagnostic
  { diagSpan = mkSrcSpanRaw "test.hs" line 1 line 10
  , diagSeverity = Warning
  , diagKind = Custom "test"
  , diagMessage = msg
  , diagCode = Just code
  , diagFixes = []
  , diagRelated = []
  }

spec :: Spec
spec = do
  describe "Suppression parsing" $ do
    describe "parseSuppressions" $ do
      it "parses argus:ignore comment" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- argus:ignore"
              , "foo = head xs"
              ]
        let supps = parseSuppressions "test.hs" source
        length supps `shouldBe` 1
        suppKind (head supps) `shouldBe` IgnoreNext
        suppScope (head supps) `shouldBe` ScopeLine 3

      it "parses argus:ignore with specific rules" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- argus:ignore avoid-head, partial-function"
              , "foo = head xs"
              ]
        let supps = parseSuppressions "test.hs" source
        length supps `shouldBe` 1
        suppRules (head supps) `shouldBe` Just (Set.fromList ["avoid-head", "partial-function"])

      it "parses argus:ignore-line comment" $ do
        let source = T.unlines
              [ "module Test where"
              , "foo = head xs -- argus:ignore-line"
              ]
        let supps = parseSuppressions "test.hs" source
        length supps `shouldBe` 1
        suppKind (head supps) `shouldBe` IgnoreLine
        suppScope (head supps) `shouldBe` ScopeLine 2

      it "parses argus:ignore-file comment" $ do
        let source = T.unlines
              [ "-- argus:ignore-file"
              , "module Test where"
              , "foo = head xs"
              , "bar = tail ys"
              ]
        let supps = parseSuppressions "test.hs" source
        length supps `shouldBe` 1
        suppKind (head supps) `shouldBe` IgnoreFile
        suppScope (head supps) `shouldBe` ScopeFile

      it "parses argus:disable comment" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- argus:disable avoid-head"
              , "foo = head xs"
              , "bar = head ys"
              ]
        let supps = parseSuppressions "test.hs" source
        length supps `shouldBe` 1
        suppKind (head supps) `shouldBe` DisableRule

      it "parses ARGUS: prefix (uppercase)" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- ARGUS:ignore"
              , "foo = head xs"
              ]
        let supps = parseSuppressions "test.hs" source
        length supps `shouldBe` 1

      it "parses block comment suppressions" $ do
        let source = T.unlines
              [ "module Test where"
              , "{- argus:ignore -}"
              , "foo = head xs"
              ]
        let supps = parseSuppressions "test.hs" source
        length supps `shouldBe` 1
        suppKind (head supps) `shouldBe` IgnoreNext

      it "parses multiple suppressions" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- argus:ignore"
              , "foo = head xs"
              , "-- argus:ignore"
              , "bar = tail ys"
              ]
        let supps = parseSuppressions "test.hs" source
        length supps `shouldBe` 2

      it "ignores non-suppression comments" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- This is a regular comment"
              , "foo = head xs"
              ]
        let supps = parseSuppressions "test.hs" source
        length supps `shouldBe` 0

  describe "Suppression application" $ do
    describe "matchesSuppression" $ do
      it "matches IgnoreNext suppression" $ do
        let supp = Suppression
              { suppKind = IgnoreNext
              , suppScope = ScopeLine 3
              , suppRules = Nothing
              , suppReason = Nothing
              , suppLocation = mkSrcSpanRaw "test.hs" 2 1 2 20
              }
        let diag = mkTestDiag 3 "avoid-head" "Use of head"
        matchesSuppression supp diag `shouldBe` True

      it "does not match wrong line" $ do
        let supp = Suppression
              { suppKind = IgnoreNext
              , suppScope = ScopeLine 3
              , suppRules = Nothing
              , suppReason = Nothing
              , suppLocation = mkSrcSpanRaw "test.hs" 2 1 2 20
              }
        let diag = mkTestDiag 5 "avoid-head" "Use of head"
        matchesSuppression supp diag `shouldBe` False

      it "matches specific rule" $ do
        let supp = Suppression
              { suppKind = IgnoreNext
              , suppScope = ScopeLine 3
              , suppRules = Just (Set.singleton "avoid-head")
              , suppReason = Nothing
              , suppLocation = mkSrcSpanRaw "test.hs" 2 1 2 20
              }
        let diag = mkTestDiag 3 "avoid-head" "Use of head"
        matchesSuppression supp diag `shouldBe` True

      it "does not match different rule" $ do
        let supp = Suppression
              { suppKind = IgnoreNext
              , suppScope = ScopeLine 3
              , suppRules = Just (Set.singleton "avoid-head")
              , suppReason = Nothing
              , suppLocation = mkSrcSpanRaw "test.hs" 2 1 2 20
              }
        let diag = mkTestDiag 3 "avoid-tail" "Use of tail"
        matchesSuppression supp diag `shouldBe` False

      it "matches file-wide suppression" $ do
        let supp = Suppression
              { suppKind = IgnoreFile
              , suppScope = ScopeFile
              , suppRules = Nothing
              , suppReason = Nothing
              , suppLocation = mkSrcSpanRaw "test.hs" 1 1 1 20
              }
        let diag = mkTestDiag 100 "avoid-head" "Use of head"
        matchesSuppression supp diag `shouldBe` True

      it "matches range suppression" $ do
        let supp = Suppression
              { suppKind = DisableRule
              , suppScope = ScopeRange 5 15
              , suppRules = Nothing
              , suppReason = Nothing
              , suppLocation = mkSrcSpanRaw "test.hs" 5 1 5 20
              }
        let diagIn = mkTestDiag 10 "avoid-head" "Use of head"
        let diagOut = mkTestDiag 20 "avoid-head" "Use of head"
        matchesSuppression supp diagIn `shouldBe` True
        matchesSuppression supp diagOut `shouldBe` False

    describe "applySuppressions" $ do
      it "filters suppressed diagnostics" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- argus:ignore"
              , "foo = head xs"
              , "bar = tail ys"
              ]
        let supps = parseSuppressions "test.hs" source
        let diags =
              [ mkTestDiag 3 "avoid-head" "Use of head"
              , mkTestDiag 4 "avoid-tail" "Use of tail"
              ]
        let filtered = applySuppressions supps diags
        length filtered `shouldBe` 1
        diagCode (head filtered) `shouldBe` Just "avoid-tail"

      it "suppresses all diagnostics with file-wide suppression" $ do
        let source = T.unlines
              [ "-- argus:ignore-file"
              , "module Test where"
              , "foo = head xs"
              , "bar = tail ys"
              ]
        let supps = parseSuppressions "test.hs" source
        let diags =
              [ mkTestDiag 3 "avoid-head" "Use of head"
              , mkTestDiag 4 "avoid-tail" "Use of tail"
              ]
        let filtered = applySuppressions supps diags
        length filtered `shouldBe` 0

      it "only suppresses matching rules" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- argus:ignore avoid-head"
              , "foo = head $ tail xs"
              ]
        let supps = parseSuppressions "test.hs" source
        let diags =
              [ mkTestDiag 3 "avoid-head" "Use of head"
              , mkTestDiag 3 "avoid-tail" "Use of tail"
              ]
        let filtered = applySuppressions supps diags
        length filtered `shouldBe` 1
        diagCode (head filtered) `shouldBe` Just "avoid-tail"

    describe "filterSuppressed" $ do
      it "combines parsing and filtering" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- argus:ignore"
              , "foo = head xs"
              ]
        let diags = [mkTestDiag 3 "avoid-head" "Use of head"]
        let filtered = filterSuppressed "test.hs" source diags
        length filtered `shouldBe` 0

  describe "Baseline operations" $ do
    describe "createBaseline" $ do
      it "creates a baseline from diagnostics" $ do
        let diags = [mkTestDiag 10 "avoid-head" "Use of head"]
        baseline <- createBaseline [("test.hs", diags)]
        blVersion baseline `shouldBe` 1
        length (blEntries baseline) `shouldBe` 1
        beFile (head (blEntries baseline)) `shouldBe` "test.hs"
        beLine (head (blEntries baseline)) `shouldBe` 10
        beRule (head (blEntries baseline)) `shouldBe` "avoid-head"

    describe "baselineContains" $ do
      it "matches diagnostic in baseline" $ do
        let diag = mkTestDiag 10 "avoid-head" "Use of head"
        baseline <- createBaseline [("test.hs", [diag])]
        baselineContains baseline diag "test.hs" `shouldBe` True

      it "does not match diagnostic not in baseline" $ do
        let diag1 = mkTestDiag 10 "avoid-head" "Use of head"
        let diag2 = mkTestDiag 20 "avoid-tail" "Use of tail"
        baseline <- createBaseline [("test.hs", [diag1])]
        baselineContains baseline diag2 "test.hs" `shouldBe` False

    describe "loadBaseline and saveBaseline" $ do
      it "round-trips a baseline through file" $ do
        withSystemTempDirectory "argus-test" $ \tmpDir -> do
          let path = tmpDir </> ".argus-baseline.json"
          let diag = mkTestDiag 10 "avoid-head" "Use of head"
          original <- createBaseline [("test.hs", [diag])]
          saveBaseline path original
          loaded <- loadBaseline path
          loaded `shouldSatisfy` \case
            Just bl -> length (blEntries bl) == 1
            Nothing -> False

      it "returns Nothing for non-existent file" $ do
        result <- loadBaseline "/non/existent/path.json"
        result `shouldBe` Nothing

    describe "mergeBaselines" $ do
      it "merges two baselines keeping newer entries" $ do
        let diag1 = mkTestDiag 10 "avoid-head" "Use of head"
        let diag2 = mkTestDiag 20 "avoid-tail" "Use of tail"
        old <- createBaseline [("test.hs", [diag1])]
        new <- createBaseline [("test.hs", [diag2])]
        let merged = mergeBaselines old new
        length (blEntries merged) `shouldBe` 2

      it "removes duplicates" $ do
        let diag = mkTestDiag 10 "avoid-head" "Use of head"
        old <- createBaseline [("test.hs", [diag])]
        new <- createBaseline [("test.hs", [diag])]
        let merged = mergeBaselines old new
        length (blEntries merged) `shouldBe` 1

  describe "hashDiagnostic" $ do
    it "produces consistent hash for same diagnostic" $ do
      let diag = mkTestDiag 10 "avoid-head" "Use of head"
      let hash1 = hashDiagnostic diag
      let hash2 = hashDiagnostic diag
      hash1 `shouldBe` hash2

    it "produces different hash for different diagnostics" $ do
      let diag1 = mkTestDiag 10 "avoid-head" "Use of head"
      let diag2 = mkTestDiag 10 "avoid-tail" "Use of tail"
      let hash1 = hashDiagnostic diag1
      let hash2 = hashDiagnostic diag2
      hash1 `shouldNotBe` hash2

  describe "Integration scenarios" $ do
    it "handles inline comments with reasons" $ do
      let source = T.unlines
            [ "module Test where"
            , "-- argus:ignore avoid-head -- Legacy code"
            , "foo = head xs"
            ]
      let supps = parseSuppressions "test.hs" source
      length supps `shouldBe` 1
      suppReason (head supps) `shouldBe` Just "Legacy code"

    it "handles multiple comma-separated rules" $ do
      let source = T.unlines
            [ "module Test where"
            , "-- argus:ignore avoid-head, avoid-tail, partial-function"
            , "foo = head xs"
            ]
      let supps = parseSuppressions "test.hs" source
      length supps `shouldBe` 1
      suppRules (head supps) `shouldBe` Just (Set.fromList ["avoid-head", "avoid-tail", "partial-function"])

    it "handles code with mixed suppression types" $ do
      let source = T.unlines
            [ "-- argus:ignore-file partial-function"
            , "module Test where"
            , "foo = head xs"
            , "-- argus:ignore"
            , "bar = tail ys"
            ]
      let supps = parseSuppressions "test.hs" source
      length supps `shouldBe` 2

  describe "New directive support (TODO-005)" $ do
    describe "disable-next-line" $ do
      it "parses argus:disable-next-line directive" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- argus:disable-next-line avoid-head"
              , "foo = head xs"
              ]
        let supps = parseSuppressions "test.hs" source
        length supps `shouldBe` 1
        suppKind (head supps) `shouldBe` DisableNextLine
        suppScope (head supps) `shouldBe` ScopeLine 3

      it "suppresses diagnostic on next line" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- argus:disable-next-line avoid-head"
              , "foo = head xs"
              , "bar = head ys"
              ]
        let supps = parseSuppressions "test.hs" source
        let diags =
              [ mkTestDiag 3 "avoid-head" "Use of head"
              , mkTestDiag 4 "avoid-head" "Use of head"
              ]
        let filtered = applySuppressions supps diags
        length filtered `shouldBe` 1
        diagCode (head filtered) `shouldBe` Just "avoid-head"
        srcSpanStartLineRaw (diagSpan (head filtered)) `shouldBe` 4

    describe "disable-line" $ do
      it "parses argus:disable-line directive" $ do
        let source = T.unlines
              [ "module Test where"
              , "foo = head xs -- argus:disable-line avoid-head"
              ]
        let supps = parseSuppressions "test.hs" source
        length supps `shouldBe` 1
        suppKind (head supps) `shouldBe` DisableLine
        suppScope (head supps) `shouldBe` ScopeLine 2

      it "suppresses diagnostic on same line" $ do
        let source = T.unlines
              [ "module Test where"
              , "foo = head xs -- argus:disable-line avoid-head"
              , "bar = head ys"
              ]
        let supps = parseSuppressions "test.hs" source
        let diags =
              [ mkTestDiag 2 "avoid-head" "Use of head"
              , mkTestDiag 3 "avoid-head" "Use of head"
              ]
        let filtered = applySuppressions supps diags
        length filtered `shouldBe` 1
        srcSpanStartLineRaw (diagSpan (head filtered)) `shouldBe` 3

    describe "disable and enable pairs" $ do
      it "parses argus:enable directive" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- argus:disable avoid-head"
              , "foo = head xs"
              , "-- argus:enable avoid-head"
              , "bar = head ys"
              ]
        let supps = parseSuppressions "test.hs" source
        length supps `shouldBe` 2
        let disableSupp = head supps
        let enableSupp = supps !! 1
        suppKind disableSupp `shouldBe` DisableRule
        suppKind enableSupp `shouldBe` EnableRule

      it "limits disable range with matching enable" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- argus:disable avoid-head"
              , "foo = head xs"
              , "bar = head ys"
              , "-- argus:enable avoid-head"
              , "baz = head zs"
              ]
        let supps = parseSuppressions "test.hs" source
        let diags =
              [ mkTestDiag 3 "avoid-head" "Use of head"
              , mkTestDiag 4 "avoid-head" "Use of head"
              , mkTestDiag 6 "avoid-head" "Use of head"
              ]
        let filtered = applySuppressions supps diags
        length filtered `shouldBe` 1
        srcSpanStartLineRaw (diagSpan (head filtered)) `shouldBe` 6

      it "disables all rules and re-enables all" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- argus:disable"
              , "foo = head xs"
              , "bar = tail ys"
              , "-- argus:enable"
              , "baz = head zs"
              ]
        let supps = parseSuppressions "test.hs" source
        let diags =
              [ mkTestDiag 3 "avoid-head" "Use of head"
              , mkTestDiag 4 "avoid-tail" "Use of tail"
              , mkTestDiag 6 "avoid-head" "Use of head"
              ]
        let filtered = applySuppressions supps diags
        length filtered `shouldBe` 1
        diagCode (head filtered) `shouldBe` Just "avoid-head"

      it "handles multiple disable/enable pairs" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- argus:disable avoid-head"
              , "foo = head xs"
              , "-- argus:enable avoid-head"
              , "bar = head ys"
              , "-- argus:disable avoid-head"
              , "baz = head zs"
              , "-- argus:enable avoid-head"
              , "qux = head ws"
              ]
        let supps = parseSuppressions "test.hs" source
        let diags =
              [ mkTestDiag 3 "avoid-head" "Use of head"
              , mkTestDiag 5 "avoid-head" "Use of head"
              , mkTestDiag 7 "avoid-head" "Use of head"
              , mkTestDiag 9 "avoid-head" "Use of head"
              ]
        let filtered = applySuppressions supps diags
        length filtered `shouldBe` 2
        -- Lines 5 and 9 should remain (not suppressed)
        map (srcSpanStartLineRaw . diagSpan) filtered `shouldBe` [5, 9]

      it "handles disable without matching enable" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- argus:disable avoid-head"
              , "foo = head xs"
              , "bar = head ys"
              , "baz = head zs"
              ]
        let supps = parseSuppressions "test.hs" source
        let diags =
              [ mkTestDiag 3 "avoid-head" "Use of head"
              , mkTestDiag 4 "avoid-head" "Use of head"
              , mkTestDiag 5 "avoid-head" "Use of head"
              ]
        let filtered = applySuppressions supps diags
        length filtered `shouldBe` 0

    describe "Compatibility with existing directives" $ do
      it "disable-next-line works like ignore" $ do
        let source1 = "-- argus:disable-next-line avoid-head"
        let source2 = "-- argus:ignore avoid-head"
        let supp1 = head $ parseSuppressions "test.hs" source1
        let supp2 = head $ parseSuppressions "test.hs" source2
        suppScope supp1 `shouldBe` suppScope supp2
        suppRules supp1 `shouldBe` suppRules supp2

      it "disable-line works like ignore-line" $ do
        let source1 = "foo = head xs -- argus:disable-line avoid-head"
        let source2 = "foo = head xs -- argus:ignore-line avoid-head"
        let supp1 = head $ parseSuppressions "test.hs" source1
        let supp2 = head $ parseSuppressions "test.hs" source2
        suppScope supp1 `shouldBe` suppScope supp2
        suppRules supp1 `shouldBe` suppRules supp2
