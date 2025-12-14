{-# LANGUAGE OverloadedStrings #-}

module HIEIncrementalValidationSpec (spec) where

import Test.Hspec
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)

import Argus.Types
  ( Fix(..), FixEdit(..), FixCategory(..), FixSafety(..)
  , mkSrcSpanRaw
  )
import Argus.Refactor.IncrementalValidation

spec :: Spec
spec = do
  describe "Argus.Refactor.IncrementalValidation" $ do
    describe "ValidationStatus" $ do
      it "has correct ordering" $ do
        VSValid `shouldSatisfy` (< VSWarnings)
        VSWarnings `shouldSatisfy` (< VSInvalid)
        VSInvalid `shouldSatisfy` (< VSPending)
        VSPending `shouldSatisfy` (< VSStale)

      it "is bounded" $ do
        minBound `shouldBe` VSValid
        maxBound `shouldBe` VSStale

      it "serializes to JSON" $ do
        Aeson.encode VSValid `shouldSatisfy` (/= "")
        Aeson.encode VSInvalid `shouldSatisfy` (/= "")

      it "deserializes from JSON" $ do
        Aeson.decode (Aeson.encode VSValid) `shouldBe` Just VSValid
        Aeson.decode (Aeson.encode VSWarnings) `shouldBe` Just VSWarnings

    describe "HIEStaleness" $ do
      it "has correct ordering" $ do
        HIECurrent `shouldSatisfy` (< HIEStale)
        HIEStale `shouldSatisfy` (< HIEMissing)

      it "is bounded" $ do
        minBound `shouldBe` HIECurrent
        maxBound `shouldBe` HIEMissing

      it "serializes to JSON" $ do
        Aeson.encode HIECurrent `shouldSatisfy` (/= "")
        Aeson.encode HIEMissing `shouldSatisfy` (/= "")

    describe "CachedValidation" $ do
      it "stores validation result" $ do
        now <- getCurrentTime
        let cv = CachedValidation
              { cvFixHash = 12345
              , cvResult = VSValid
              , cvErrors = []
              , cvWarnings = []
              , cvValidatedAt = now
              , cvHIETimestamp = now
              , cvAffectedFiles = ["test.hs"]
              }
        cvResult cv `shouldBe` VSValid
        cvFixHash cv `shouldBe` 12345
        cvAffectedFiles cv `shouldBe` ["test.hs"]

      it "stores errors and warnings" $ do
        now <- getCurrentTime
        let cv = CachedValidation
              { cvFixHash = 0
              , cvResult = VSWarnings
              , cvErrors = ["error1"]
              , cvWarnings = ["warning1", "warning2"]
              , cvValidatedAt = now
              , cvHIETimestamp = now
              , cvAffectedFiles = []
              }
        cvErrors cv `shouldBe` ["error1"]
        length (cvWarnings cv) `shouldBe` 2

      it "serializes to JSON" $ do
        now <- getCurrentTime
        let cv = CachedValidation 0 VSValid [] [] now now []
        Aeson.encode cv `shouldSatisfy` (/= "")

    describe "ValidationBatch" $ do
      it "stores batch information" $ do
        let batch = ValidationBatch
              { vbFixes = [mkTestFix "fix1", mkTestFix "fix2"]
              , vbBatchId = 1
              , vbPriority = 10
              }
        length (vbFixes batch) `shouldBe` 2
        vbBatchId batch `shouldBe` 1
        vbPriority batch `shouldBe` 10

      it "serializes to JSON" $ do
        let batch = ValidationBatch [] 0 0
        Aeson.encode batch `shouldSatisfy` (/= "")

    describe "groupIndependentFixes" $ do
      it "groups non-overlapping fixes" $ do
        let fix1 = mkTestFixForFile "a.hs"
            fix2 = mkTestFixForFile "b.hs"
            fix3 = mkTestFixForFile "c.hs"
            groups = groupIndependentFixes [fix1, fix2, fix3]
        -- All fixes affect different files, so they can all be in one group
        length groups `shouldBe` 1
        length (head groups) `shouldBe` 3

      it "separates overlapping fixes" $ do
        let fix1 = mkTestFixForFile "shared.hs"
            fix2 = mkTestFixForFile "shared.hs"
            groups = groupIndependentFixes [fix1, fix2]
        -- Same file, so they must be in separate groups
        length groups `shouldBe` 2

      it "handles empty list" $ do
        groupIndependentFixes [] `shouldBe` []

      it "handles single fix" $ do
        let groups = groupIndependentFixes [mkTestFix "single"]
        length groups `shouldBe` 1
        length (head groups) `shouldBe` 1

    describe "HIE Staleness" $ do
      describe "checkHIEStaleness" $ do
        it "returns HIEMissing for non-existent file" $ do
          result <- checkHIEStaleness "/nonexistent/path.hs"
          result `shouldBe` HIEMissing

      describe "isHIECurrent" $ do
        it "returns False for missing file" $ do
          result <- isHIECurrent "/nonexistent/path.hs"
          result `shouldBe` False

      describe "getHIETimestamp" $ do
        it "returns Nothing for missing file" $ do
          result <- getHIETimestamp "/nonexistent/path.hs"
          result `shouldBe` Nothing

    describe "HIE File Staleness Functions" $ do
      it "checks staleness for missing file" $ do
        result <- checkHIEStaleness "/nonexistent/test.hs"
        result `shouldBe` HIEMissing

      it "reports missing HIE as not current" $ do
        result <- isHIECurrent "/nonexistent/test.hs"
        result `shouldBe` False

    describe "mergeValidationResults" $ do
      it "preserves order" $ do
        let fix1 = mkTestFix "fix1"
            fix2 = mkTestFix "fix2"
            fixes = [fix1, fix2]
            results = mergeValidationResults fixes [] [] []
        length results `shouldBe` 2

      it "handles empty input" $ do
        let results = mergeValidationResults [] [] [] []
        results `shouldBe` []

    describe "Edge Cases" $ do
      it "handles fix with no edits" $ do
        let fix = mkEmptyFix
            groups = groupIndependentFixes [fix]
        length groups `shouldBe` 1

      it "handles fix affecting multiple files" $ do
        let fix = mkMultiFileFix ["a.hs", "b.hs"]
            groups = groupIndependentFixes [fix]
        length groups `shouldBe` 1

-- Helper functions

mkTestFix :: Text -> Fix
mkTestFix title = Fix
  { fixTitle = title
  , fixEdits = [FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 10) "newCode"]
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSAlways
  }

mkTestFixForFile :: FilePath -> Fix
mkTestFixForFile file = Fix
  { fixTitle = T.pack $ "Fix for " ++ file
  , fixEdits = [FixEdit (mkSrcSpanRaw file 1 1 1 10) "newCode"]
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSAlways
  }

mkEmptyFix :: Fix
mkEmptyFix = Fix
  { fixTitle = "Empty"
  , fixEdits = []
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSAlways
  }

mkMultiFileFix :: [FilePath] -> Fix
mkMultiFileFix files = Fix
  { fixTitle = "Multi-file fix"
  , fixEdits = [FixEdit (mkSrcSpanRaw f 1 1 1 10) "code" | f <- files]
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSAlways
  }
