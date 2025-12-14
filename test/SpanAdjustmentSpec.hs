{-# LANGUAGE OverloadedStrings #-}

module SpanAdjustmentSpec (spec) where

import Test.Hspec
import Data.Text qualified as T

import Argus.Types (SrcSpan(..), Fix(..), FixEdit(..), Line(..), Column(..), FixCategory(..), FixSafety(..))
import Argus.Refactor.SpanAdjustment

-- | Create a simple SrcSpan for testing
mkSpan :: FilePath -> Int -> Int -> Int -> Int -> SrcSpan
mkSpan file sl sc el ec = SrcSpan
  { srcSpanFile = file
  , srcSpanStartLine = Line sl
  , srcSpanStartCol = Column sc
  , srcSpanEndLine = Line el
  , srcSpanEndCol = Column ec
  }

-- | Create a simple Fix for testing
mkFix :: FilePath -> Int -> Int -> Int -> Int -> T.Text -> Fix
mkFix file sl sc el ec newText = Fix
  { fixTitle = "Test fix"
  , fixEdits = [FixEdit (mkSpan file sl sc el ec) newText]
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCRedundant
  , fixSafety = FSAlways
  }

spec :: Spec
spec = do
  describe "Argus.Refactor.SpanAdjustment" $ do
    describe "computeDelta" $ do
      it "computes zero delta for same-length replacement" $ do
        let span = mkSpan "test.hs" 5 10 5 14  -- "test" (4 chars)
            delta = computeDelta span "test" "abcd"  -- same length
        sdLineDelta delta `shouldBe` 0
        sdColDelta delta `shouldBe` 0

      it "computes positive column delta for longer replacement" $ do
        let span = mkSpan "test.hs" 5 10 5 14  -- 4 chars
            delta = computeDelta span "test" "testing"  -- 7 chars (+3)
        sdLineDelta delta `shouldBe` 0
        sdColDelta delta `shouldBe` 3

      it "computes negative column delta for shorter replacement" $ do
        let span = mkSpan "test.hs" 5 10 5 14  -- 4 chars
            delta = computeDelta span "test" "xy"  -- 2 chars (-2)
        sdLineDelta delta `shouldBe` 0
        sdColDelta delta `shouldBe` (-2)

      it "computes positive line delta for multi-line addition" $ do
        let span = mkSpan "test.hs" 5 1 5 10
            delta = computeDelta span "singleLine" "line1\nline2\nline3"  -- +2 lines
        sdLineDelta delta `shouldBe` 2

      it "computes negative line delta for multi-line removal" $ do
        let span = mkSpan "test.hs" 5 1 7 5  -- spans 3 lines
            delta = computeDelta span "line1\nline2\nline3" "single"  -- -2 lines
        sdLineDelta delta `shouldBe` (-2)

    describe "adjustSpan" $ do
      it "returns unchanged span for different file" $ do
        let delta = SpanDelta
              { sdFile = "other.hs"
              , sdEditStart = (5, 1)
              , sdEditEnd = (5, 10)
              , sdLineDelta = 1
              , sdColDelta = 0
              , sdNewEndLine = 6
              , sdNewEndCol = 1
              }
            span = mkSpan "test.hs" 10 5 10 20
        adjustSpan delta span `shouldBe` Just span

      it "returns unchanged span if before the delta" $ do
        let delta = SpanDelta
              { sdFile = "test.hs"
              , sdEditStart = (10, 1)
              , sdEditEnd = (10, 10)
              , sdLineDelta = 1
              , sdColDelta = 0
              , sdNewEndLine = 11
              , sdNewEndCol = 1
              }
            span = mkSpan "test.hs" 5 1 5 20  -- line 5, before line 10
        adjustSpan delta span `shouldBe` Just span

      it "returns Nothing for overlapping spans" $ do
        let delta = SpanDelta
              { sdFile = "test.hs"
              , sdEditStart = (5, 10)
              , sdEditEnd = (5, 20)
              , sdLineDelta = 0
              , sdColDelta = 5
              , sdNewEndLine = 5
              , sdNewEndCol = 25
              }
            span = mkSpan "test.hs" 5 15 5 25  -- overlaps with edit
        adjustSpan delta span `shouldBe` Nothing

      it "adjusts span after delta by line delta" $ do
        let delta = SpanDelta
              { sdFile = "test.hs"
              , sdEditStart = (5, 1)
              , sdEditEnd = (5, 10)
              , sdLineDelta = 2  -- added 2 lines
              , sdColDelta = 0
              , sdNewEndLine = 7
              , sdNewEndCol = 10
              }
            span = mkSpan "test.hs" 10 5 10 20
            expected = mkSpan "test.hs" 12 5 12 20  -- shifted down by 2
        adjustSpan delta span `shouldBe` Just expected

    describe "adjustFix" $ do
      it "adjusts all edits in a fix" $ do
        let delta = SpanDelta
              { sdFile = "test.hs"
              , sdEditStart = (5, 1)
              , sdEditEnd = (5, 10)
              , sdLineDelta = 1
              , sdColDelta = 0
              , sdNewEndLine = 6
              , sdNewEndCol = 10
              }
            fix = mkFix "test.hs" 10 5 10 20 "newText"
        case adjustFix [delta] fix of
          Nothing -> expectationFailure "Expected fix to be adjusted"
          Just adjusted -> do
            let edit = head (fixEdits adjusted)
                span = fixEditSpan edit
            srcSpanStartLine span `shouldBe` Line 11  -- shifted by 1

      it "returns Nothing if any edit span is invalidated" $ do
        let delta = SpanDelta
              { sdFile = "test.hs"
              , sdEditStart = (10, 5)
              , sdEditEnd = (10, 15)
              , sdLineDelta = 0
              , sdColDelta = 0
              , sdNewEndLine = 10
              , sdNewEndCol = 15
              }
            fix = mkFix "test.hs" 10 8 10 20 "newText"  -- overlaps
        adjustFix [delta] fix `shouldBe` Nothing

    describe "adjustFixes" $ do
      it "filters out invalidated fixes" $ do
        let delta = SpanDelta
              { sdFile = "test.hs"
              , sdEditStart = (5, 1)
              , sdEditEnd = (5, 20)
              , sdLineDelta = 0
              , sdColDelta = 0
              , sdNewEndLine = 5
              , sdNewEndCol = 20
              }
            fix1 = mkFix "test.hs" 5 10 5 15 "new1"  -- overlaps
            fix2 = mkFix "test.hs" 10 1 10 10 "new2"  -- safe
            fixes = [fix1, fix2]
        length (adjustFixes [delta] fixes) `shouldBe` 1

    describe "DeltaAccumulator" $ do
      it "starts empty" $ do
        getDeltas emptyAccumulator `shouldBe` []

      it "accumulates deltas" $ do
        let delta1 = SpanDelta "test.hs" (5, 1) (5, 10) 1 0 6 10
            delta2 = SpanDelta "test.hs" (10, 1) (10, 5) 0 3 10 8
            acc = addDelta delta2 $ addDelta delta1 emptyAccumulator
        length (getDeltas acc) `shouldBe` 2

    describe "spanAffectedByDelta" $ do
      it "returns False for different files" $ do
        let delta = SpanDelta "other.hs" (5, 1) (5, 10) 1 0 6 10
            span = mkSpan "test.hs" 10 1 10 20
        spanAffectedByDelta delta span `shouldBe` False

      it "returns False for span entirely before delta" $ do
        let delta = SpanDelta "test.hs" (10, 1) (10, 10) 1 0 11 10
            span = mkSpan "test.hs" 5 1 5 20
        spanAffectedByDelta delta span `shouldBe` False

      it "returns True for span after delta" $ do
        let delta = SpanDelta "test.hs" (5, 1) (5, 10) 1 0 6 10
            span = mkSpan "test.hs" 10 1 10 20
        spanAffectedByDelta delta span `shouldBe` True

    describe "real-world scenario" $ do
      it "handles sequential edits correctly" $ do
        -- Scenario: Two fixes on lines 5 and 10
        -- Fix 1 adds a line at position 5
        -- Fix 2 should be adjusted to line 11
        let content = T.unlines
              [ "module Test where"
              , ""
              , "foo = 1"
              , ""
              , "bar = 2"  -- line 5 - fix1 targets this
              , ""
              , "baz = 3"
              , ""
              , ""
              , "qux = 4"  -- line 10 - fix2 targets this
              ]

        let fix1Span = mkSpan "test.hs" 5 1 5 8
            fix1OldText = "bar = 2"
            fix1NewText = "bar = 2\nbar' = 22"  -- adds 1 line

        let delta = computeDelta fix1Span fix1OldText fix1NewText
        sdLineDelta delta `shouldBe` 1

        -- Fix 2 should be adjusted
        let fix2 = mkFix "test.hs" 10 1 10 8 "qux = 5"
        case adjustFix [delta] fix2 of
          Nothing -> expectationFailure "Fix2 should be adjustable"
          Just adjusted -> do
            let span = fixEditSpan (head (fixEdits adjusted))
            srcSpanStartLine span `shouldBe` Line 11  -- moved from 10 to 11
