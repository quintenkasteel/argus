{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : BaselineSpec
-- Description : Tests for baseline comparison system
-- Copyright   : (c) 2024
-- License     : MIT
module BaselineSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Data.Text qualified as T
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

import Argus.Types (Diagnostic(..), Severity(..), SrcSpan(..), Line(..), Column(..), DiagnosticKind(..))
import Argus.Baseline
import Argus.Baseline.Types
import Argus.Baseline.Diff
import Argus.Baseline.IO

-- | Helper to create a test diagnostic
mkTestDiag :: FilePath -> Int -> Text -> Text -> Diagnostic
mkTestDiag file line rule msg = Diagnostic
  { diagSpan = SrcSpan file (Line line) (Column 1) (Line line) (Column 10)
  , diagSeverity = Warning
  , diagKind = CodePattern
  , diagMessage = msg
  , diagCode = Just rule
  , diagFixes = []
  , diagRelated = []
  }

-- | Helper to create a test baseline entry
mkTestEntry :: FilePath -> Int -> Text -> Text -> BaselineEntry
mkTestEntry file line rule msg = BaselineEntry
  { beFile = file
  , beLine = line
  , beColumn = 1
  , beRule = rule
  , beMessage = msg
  , beSeverity = Warning
  , beFingerprint = T.pack $ show (file, line, rule)
  , beReason = Nothing
  , beExpires = Nothing
  }

spec :: Spec
spec = do
  describe "Baseline.Types" $ do
    describe "BaselineVersion" $ do
      it "has current version >= 1" $ do
        currentBaselineVersion `shouldSatisfy` (>= 1)

    describe "emptyBaseline" $ do
      it "creates an empty baseline with no entries" $ do
        now <- getCurrentTime
        let bl = emptyBaseline now
        baselineEntries bl `shouldBe` []

      it "uses the current version" $ do
        now <- getCurrentTime
        let bl = emptyBaseline now
        baselineVersion bl `shouldBe` currentBaselineVersion

    describe "mkBaselineEntry" $ do
      it "creates entry from diagnostic" $ do
        let diag = mkTestDiag "Test.hs" 10 "test-rule" "Test message"
            entry = mkBaselineEntry diag Nothing
        beFile entry `shouldBe` "Test.hs"
        beLine entry `shouldBe` 10
        beRule entry `shouldBe` "test-rule"

      it "preserves reason if provided" $ do
        let diag = mkTestDiag "Test.hs" 10 "test-rule" "Test message"
            entry = mkBaselineEntry diag (Just "Known issue")
        beReason entry `shouldBe` Just "Known issue"

    describe "DiagnosticFingerprint" $ do
      it "computes fingerprint from diagnostic" $ do
        let diag = mkTestDiag "Test.hs" 10 "test-rule" "Test message"
            fp = computeFingerprint diag
        dfFile fp `shouldBe` "Test.hs"
        dfRule fp `shouldBe` "test-rule"
        dfMessage fp `shouldBe` "Test message"

      it "creates line range for fuzzy matching" $ do
        let diag = mkTestDiag "Test.hs" 10 "test-rule" "Test message"
            fp = computeFingerprint diag
        dfLineRange fp `shouldBe` (7, 13)  -- 10 +/- 3

  describe "Baseline.Diff" $ do
    describe "diffBaseline" $ do
      it "detects new issues" $ do
        now <- getCurrentTime
        let baseline = emptyBaseline now
            diags = [mkTestDiag "Test.hs" 10 "test-rule" "New issue"]
            diff = diffBaseline baseline diags
        dsNewCount (bdStats diff) `shouldBe` 1
        dsResolvedCount (bdStats diff) `shouldBe` 0

      it "detects resolved issues" $ do
        now <- getCurrentTime
        let entry = mkTestEntry "Test.hs" 10 "test-rule" "Old issue"
            baseline = (emptyBaseline now) { baselineEntries = [entry] }
            diags = []  -- No current issues
            diff = diffBaseline baseline diags
        dsNewCount (bdStats diff) `shouldBe` 0
        dsResolvedCount (bdStats diff) `shouldBe` 1

      it "matches existing issues" $ do
        now <- getCurrentTime
        let diag = mkTestDiag "Test.hs" 10 "test-rule" "Same issue"
            entry = mkBaselineEntry diag Nothing
            baseline = (emptyBaseline now) { baselineEntries = [entry] }
            diff = diffBaseline baseline [diag]
        dsNewCount (bdStats diff) `shouldBe` 0
        dsExistingCount (bdStats diff) `shouldBe` 1

      it "computes delta correctly" $ do
        now <- getCurrentTime
        let oldEntry = mkTestEntry "Test.hs" 10 "old-rule" "Old issue"
            newDiag = mkTestDiag "Test.hs" 20 "new-rule" "New issue"
            baseline = (emptyBaseline now) { baselineEntries = [oldEntry] }
            diff = diffBaseline baseline [newDiag]
        dsDelta (bdStats diff) `shouldBe` 0  -- 1 new - 1 resolved = 0

    describe "fingerprintMatches" $ do
      it "matches by fingerprint hash" $ do
        let diag = mkTestDiag "Test.hs" 10 "test-rule" "Test message"
            entry = mkBaselineEntry diag Nothing
            fp = computeFingerprint diag
        fingerprintMatches fp entry `shouldBe` True

      it "matches by fuzzy line range" $ do
        let diag1 = mkTestDiag "Test.hs" 10 "test-rule" "Test message"
            diag2 = mkTestDiag "Test.hs" 12 "test-rule" "Test message"  -- Within range
            entry = mkBaselineEntry diag1 Nothing
            fp2 = computeFingerprint diag2
        -- The fuzzy match should work for same file, rule, and within line range
        fingerprintMatches fp2 entry `shouldBe` True

      it "does not match different files" $ do
        let diag1 = mkTestDiag "Test1.hs" 10 "test-rule" "Test message"
            diag2 = mkTestDiag "Test2.hs" 10 "test-rule" "Test message"
            entry = mkBaselineEntry diag1 Nothing
            fp2 = computeFingerprint diag2
        fingerprintMatches fp2 entry `shouldBe` False

    describe "categorizeIssues" $ do
      it "separates new from baselined issues" $ do
        now <- getCurrentTime
        let existingDiag = mkTestDiag "Test.hs" 10 "existing" "Existing"
            newDiag = mkTestDiag "Test.hs" 20 "new" "New"
            entry = mkBaselineEntry existingDiag Nothing
            baseline = (emptyBaseline now) { baselineEntries = [entry] }
            (new, baselined) = categorizeIssues baseline [existingDiag, newDiag]
        length new `shouldBe` 1
        length baselined `shouldBe` 1

    describe "summarizeDiff" $ do
      it "produces summary text" $ do
        let diff = emptyDiff { bdStats = DiffStats 2 1 3 0 1 6 4 }
            summary = summarizeDiff diff
        summary `shouldSatisfy` T.isInfixOf "New issues:      2"
        summary `shouldSatisfy` T.isInfixOf "Resolved:        1"

  describe "Baseline.IO" $ do
    describe "saveBaseline and loadBaseline" $ do
      it "round-trips a baseline" $ do
        withSystemTempDirectory "argus-baseline-test" $ \tmpDir -> do
          now <- getCurrentTime
          let path = tmpDir </> "test-baseline.json"
              baseline = (emptyBaseline now)
                { baselineDescription = Just "Test baseline"
                , baselineEntries = [mkTestEntry "Test.hs" 10 "test" "message"]
                }

          -- Save
          saveResult <- saveBaseline path baseline
          saveResult `shouldBe` Right ()

          -- Load
          loadResult <- loadBaseline path
          case loadResult of
            Left err -> expectationFailure $ "Failed to load: " <> show err
            Right loaded -> do
              baselineVersion loaded `shouldBe` baselineVersion baseline
              length (baselineEntries loaded) `shouldBe` 1
              baselineDescription loaded `shouldBe` Just "Test baseline"

      it "returns error for missing file" $ do
        result <- loadBaseline "/nonexistent/path/baseline.json"
        case result of
          Left (BaselineNotFound _) -> pure ()
          _ -> expectationFailure "Expected BaselineNotFound error"

    describe "baselineExists" $ do
      it "returns False for non-existent file" $ do
        exists <- baselineExists "/nonexistent/file.json"
        exists `shouldBe` False

      it "returns True for existing file" $ do
        withSystemTempDirectory "argus-baseline-test" $ \tmpDir -> do
          now <- getCurrentTime
          let path = tmpDir </> "exists.json"
          _ <- saveBaseline path (emptyBaseline now)
          exists <- baselineExists path
          exists `shouldBe` True

    describe "createBaseline" $ do
      it "creates baseline from diagnostics" $ do
        let diags = [ mkTestDiag "A.hs" 1 "rule1" "Issue 1"
                    , mkTestDiag "B.hs" 2 "rule2" "Issue 2"
                    ]
        baseline <- createBaseline diags (Just "Test") Nothing
        length (baselineEntries baseline) `shouldBe` 2
        baselineDescription baseline `shouldBe` Just "Test"

    describe "updateBaseline" $ do
      it "adds new issues to baseline" $ do
        now <- getCurrentTime
        let oldDiag = mkTestDiag "Old.hs" 1 "old" "Old issue"
            newDiag = mkTestDiag "New.hs" 1 "new" "New issue"
            oldEntry = mkBaselineEntry oldDiag Nothing
            baseline = (emptyBaseline now) { baselineEntries = [oldEntry] }

        updated <- updateBaseline baseline [oldDiag, newDiag] True Nothing
        length (baselineEntries updated) `shouldBe` 2

      it "removes resolved issues when not keeping" $ do
        now <- getCurrentTime
        let oldDiag = mkTestDiag "Old.hs" 1 "old" "Old issue"
            oldEntry = mkBaselineEntry oldDiag Nothing
            baseline = (emptyBaseline now) { baselineEntries = [oldEntry] }

        -- Update with empty diagnostics list, not keeping resolved
        updated <- updateBaseline baseline [] False Nothing
        length (baselineEntries updated) `shouldBe` 0

  describe "CI Integration" $ do
    describe "evaluateCIThresholds" $ do
      it "succeeds with no new issues" $ do
        let opts = CIOptions
              { ciBaselinePath = Nothing
              , ciFailOnNew = True
              , ciFailOnSeverity = Nothing
              , ciFailOnCount = Nothing
              , ciFailOnDelta = Nothing
              , ciUpdateBaseline = False
              , ciQuiet = False
              }
            diff = emptyDiff { bdStats = (DiffStats 0 0 5 0 0 5 5) }
            (reason, code) = evaluateCIThresholds opts diff []
        code `shouldBe` 0

      it "fails when new issues found with fail-on-new" $ do
        let opts = CIOptions
              { ciBaselinePath = Nothing
              , ciFailOnNew = True
              , ciFailOnSeverity = Nothing
              , ciFailOnCount = Nothing
              , ciFailOnDelta = Nothing
              , ciUpdateBaseline = False
              , ciQuiet = False
              }
            diff = emptyDiff { bdStats = (DiffStats 2 0 3 0 2 5 3) }
            (reason, code) = evaluateCIThresholds opts diff []
        code `shouldBe` 1

      it "fails when severity threshold exceeded" $ do
        let opts = CIOptions
              { ciBaselinePath = Nothing
              , ciFailOnNew = False
              , ciFailOnSeverity = Just Error
              , ciFailOnCount = Nothing
              , ciFailOnDelta = Nothing
              , ciUpdateBaseline = False
              , ciQuiet = False
              }
            errorDiag = (mkTestDiag "Test.hs" 1 "error" "Error") { diagSeverity = Error }
            (reason, code) = evaluateCIThresholds opts emptyDiff [errorDiag]
        code `shouldBe` 1

      it "fails when count threshold exceeded" $ do
        let opts = CIOptions
              { ciBaselinePath = Nothing
              , ciFailOnNew = False
              , ciFailOnSeverity = Nothing
              , ciFailOnCount = Just 5
              , ciFailOnDelta = Nothing
              , ciUpdateBaseline = False
              , ciQuiet = False
              }
            diags = replicate 10 (mkTestDiag "Test.hs" 1 "rule" "Issue")
            (reason, code) = evaluateCIThresholds opts emptyDiff diags
        code `shouldBe` 1

      it "fails when delta threshold exceeded" $ do
        let opts = CIOptions
              { ciBaselinePath = Nothing
              , ciFailOnNew = False
              , ciFailOnSeverity = Nothing
              , ciFailOnCount = Nothing
              , ciFailOnDelta = Just 5
              , ciUpdateBaseline = False
              , ciQuiet = False
              }
            diff = emptyDiff { bdStats = (DiffStats 10 1 0 0 9 10 1) }
            (reason, code) = evaluateCIThresholds opts diff []
        code `shouldBe` 1

    describe "hasRegressions" $ do
      it "returns True when new issues exist" $ do
        let diff = emptyDiff { bdStats = (DiffStats 1 0 0 0 1 1 0) }
        hasRegressions diff `shouldBe` True

      it "returns False when no new issues" $ do
        let diff = emptyDiff { bdStats = (DiffStats 0 5 10 0 (-5) 10 15) }
        hasRegressions diff `shouldBe` False

    describe "getRegressions" $ do
      it "extracts regression diagnostics" $ do
        let diag = mkTestDiag "Test.hs" 1 "rule" "New issue"
            entry = DiffEntry DiffNew (Just diag) Nothing
            diff = emptyDiff { bdNew = [entry] }
        getRegressions diff `shouldBe` [diag]

    describe "getImprovements" $ do
      it "extracts improvement entries" $ do
        let entry = mkTestEntry "Test.hs" 1 "rule" "Fixed issue"
            diffEntry = DiffEntry DiffResolved Nothing (Just entry)
            diff = emptyDiff { bdResolved = [diffEntry] }
        getImprovements diff `shouldBe` [entry]

  describe "Expiration" $ do
    describe "isExpired" $ do
      it "returns False when no expiration set" $ do
        now <- getCurrentTime
        let entry = mkTestEntry "Test.hs" 1 "rule" "message"
        isExpired now entry `shouldBe` False

      it "returns False when not yet expired" $ do
        now <- getCurrentTime
        let future = addUTCTime 3600 now  -- 1 hour in future
            entry = (mkTestEntry "Test.hs" 1 "rule" "message") { beExpires = Just future }
        isExpired now entry `shouldBe` False

      it "returns True when expired" $ do
        now <- getCurrentTime
        let past = addUTCTime (-3600) now  -- 1 hour in past
            entry = (mkTestEntry "Test.hs" 1 "rule" "message") { beExpires = Just past }
        isExpired now entry `shouldBe` True
