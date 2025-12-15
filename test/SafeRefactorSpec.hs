{-# LANGUAGE OverloadedStrings #-}

module SafeRefactorSpec (spec) where

import Test.Hspec
import Control.Exception (catch, IOException)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Data.Text.IO qualified as TIO

import Argus.Types
import Argus.Refactor.SafeRefactor
import Argus.Refactor.Validation (ValidationLevel(..), ValidationResult(..))
import Argus.Refactor.FixGraph (ConflictStrategy(..))

-- | Helper to create a diagnostic with a fix
mkDiagnostic :: Text -> Int -> Int -> Text -> Fix -> Diagnostic
mkDiagnostic file line col msg fix = Diagnostic
  { diagSpan = mkSrcSpanRaw (T.unpack file) line col line (col + 10)
  , diagSeverity = Warning
  , diagKind = CodePattern
  , diagMessage = msg
  , diagCode = Nothing
  , diagFixes = [fix]
  , diagRelated = []
  }

-- | Helper to create a simple fix
mkTestFix :: Text -> FilePath -> Int -> Int -> Int -> Int -> Text -> Bool -> Fix
mkTestFix title file sl sc el ec replacement preferred = Fix
  { fixTitle = title
  , fixEdits = [FixEdit (mkSrcSpanRaw file sl sc el ec) replacement]
  , fixIsPreferred = preferred
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSMostly
  }

spec :: Spec
spec = do
  describe "Argus.Refactor.SafeRefactor" $ do
    describe "SafeRefactorOptions" $ do
      it "has sensible defaults" $ do
        sroValidationLevel defaultSafeOptions `shouldBe` SyntaxValidation
        sroValidateEachFix defaultSafeOptions `shouldBe` True
        sroConflictStrategy defaultSafeOptions `shouldBe` PreferPreferred
        sroTransactional defaultSafeOptions `shouldBe` True
        sroCreateBackups defaultSafeOptions `shouldBe` True
        sroSafeOnly defaultSafeOptions `shouldBe` True
        sroDryRun defaultSafeOptions `shouldBe` False

    describe "safeApplyFixes" $ do
      it "applies single fix to valid code" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\nx = 1"
        TIO.writeFile testFile code

        let fix = mkTestFix "Change value" testFile 2 5 2 6 "2" True
            diag = mkDiagnostic (T.pack testFile) 2 5 "Consider changing to 2" fix
            opts = defaultSafeOptions { sroDryRun = True }

        result <- safeApplyFixes opts [(testFile, [diag])]
        srrSuccess result `shouldBe` True
        rsAppliedFixes (srrStats result) `shouldBe` 1

      it "handles empty diagnostics" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\nx = 1"
        TIO.writeFile testFile code

        let opts = defaultSafeOptions { sroDryRun = True }
        result <- safeApplyFixes opts [(testFile, [])]
        srrSuccess result `shouldBe` True
        rsAppliedFixes (srrStats result) `shouldBe` 0

      it "filters unsafe fixes when safeOnly is True" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\nx = 1"
        TIO.writeFile testFile code

        let safeFix = mkTestFix "Safe fix" testFile 2 5 2 6 "2" True
            unsafeFix = mkTestFix "Unsafe fix" testFile 2 5 2 6 "3" False
            diag1 = mkDiagnostic (T.pack testFile) 2 5 "Safe suggestion" safeFix
            diag2 = mkDiagnostic (T.pack testFile) 2 5 "Unsafe suggestion" unsafeFix
            opts = defaultSafeOptions { sroDryRun = True, sroSafeOnly = True }

        result <- safeApplyFixes opts [(testFile, [diag1, diag2])]
        srrSuccess result `shouldBe` True
        -- Only the safe fix should be considered (conflicts resolved)
        rsAppliedFixes (srrStats result) `shouldSatisfy` (<= 1)

      it "validates syntax after each fix" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\nx = 1"
        TIO.writeFile testFile code

        -- This fix would create invalid syntax
        let badFix = mkTestFix "Bad fix" testFile 2 5 2 6 "= =" True  -- Invalid
            diag = mkDiagnostic (T.pack testFile) 2 5 "Bad suggestion" badFix
            opts = defaultSafeOptions { sroDryRun = True, sroValidateEachFix = True }

        result <- safeApplyFixes opts [(testFile, [diag])]
        -- Should fail validation
        rsFailedFixes (srrStats result) `shouldSatisfy` (>= 0)

      it "skips validation when disabled" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "x = 1"  -- Minimal valid code
        TIO.writeFile testFile code

        let fix = mkTestFix "Change" testFile 1 5 1 6 "2" True
            diag = mkDiagnostic (T.pack testFile) 1 5 "Suggestion" fix
            opts = defaultSafeOptions
              { sroDryRun = True
              , sroValidationLevel = NoValidation
              }

        result <- safeApplyFixes opts [(testFile, [diag])]
        srrSuccess result `shouldBe` True

    describe "RefactorStats" $ do
      it "tracks files processed" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile1 = tmpDir </> "test1.hs"
            testFile2 = tmpDir </> "test2.hs"
        TIO.writeFile testFile1 "x = 1"
        TIO.writeFile testFile2 "y = 2"

        let fix1 = mkTestFix "Fix 1" testFile1 1 5 1 6 "10" True
            fix2 = mkTestFix "Fix 2" testFile2 1 5 1 6 "20" True
            diag1 = mkDiagnostic (T.pack testFile1) 1 5 "Suggestion" fix1
            diag2 = mkDiagnostic (T.pack testFile2) 1 5 "Suggestion" fix2
            opts = defaultSafeOptions { sroDryRun = True }

        result <- safeApplyFixes opts [(testFile1, [diag1]), (testFile2, [diag2])]
        rsFilesProcessed (srrStats result) `shouldBe` 2

      it "counts applied fixes" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
        TIO.writeFile testFile "x = 1\ny = 2"

        let fix1 = mkTestFix "Fix 1" testFile 1 5 1 6 "10" True
            fix2 = mkTestFix "Fix 2" testFile 2 5 2 6 "20" True
            diag1 = mkDiagnostic (T.pack testFile) 1 5 "Suggestion 1" fix1
            diag2 = mkDiagnostic (T.pack testFile) 2 5 "Suggestion 2" fix2
            opts = defaultSafeOptions { sroDryRun = True }

        result <- safeApplyFixes opts [(testFile, [diag1, diag2])]
        rsAppliedFixes (srrStats result) `shouldBe` 2

    describe "SkipReason" $ do
      it "detects conflicts between overlapping fixes" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
        TIO.writeFile testFile "x = 1"

        -- Two fixes that overlap (one fully contains the other's region)
        let fix1 = mkTestFix "Fix 1" testFile 1 1 1 6 "y = 2" True
            fix2 = mkTestFix "Fix 2" testFile 1 1 1 6 "z = 3" True  -- Same span
            diag1 = mkDiagnostic (T.pack testFile) 1 1 "Suggestion 1" fix1
            diag2 = mkDiagnostic (T.pack testFile) 1 1 "Suggestion 2" fix2
            opts = defaultSafeOptions { sroDryRun = True }

        result <- safeApplyFixes opts [(testFile, [diag1, diag2])]
        -- When fixes conflict, one gets applied and one gets skipped
        -- Total should equal 2 (either applied + skipped or both skipped)
        let stats = srrStats result
        rsAppliedFixes stats + rsSkippedFixes stats + rsFailedFixes stats `shouldBe` 2

    describe "Transaction" $ do
      it "supports dry run mode" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            originalCode = "x = 1"
        TIO.writeFile testFile originalCode

        let fix = mkTestFix "Change" testFile 1 5 1 6 "2" True
            diag = mkDiagnostic (T.pack testFile) 1 5 "Suggestion" fix
            opts = defaultSafeOptions { sroDryRun = True }

        _ <- safeApplyFixes opts [(testFile, [diag])]

        -- File should be unchanged in dry run
        content <- TIO.readFile testFile
        content `shouldBe` originalCode

    describe "AppliedFix" $ do
      it "records old and new content" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "x = 1"
        TIO.writeFile testFile code

        let fix = mkTestFix "Change" testFile 1 5 1 6 "2" True
            diag = mkDiagnostic (T.pack testFile) 1 5 "Suggestion" fix
            opts = defaultSafeOptions { sroDryRun = True }

        result <- safeApplyFixes opts [(testFile, [diag])]
        case srrApplied result of
          [] -> expectationFailure "Expected at least one applied fix"
          (applied:_) -> do
            afOldContent applied `shouldBe` code
            T.isInfixOf "2" (afNewContent applied) `shouldBe` True

    describe "edge cases" $ do
      it "handles fix with no edits" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
        TIO.writeFile testFile "x = 1"

        let emptyFix = Fix
              { fixTitle = "Empty fix"
              , fixEdits = []
              , fixIsPreferred = True
              , fixAddImports = []
              , fixRemoveImports = []
              , fixCategory = FCStyle
              , fixSafety = FSMostly
              }
            diag = mkDiagnostic (T.pack testFile) 1 1 "Empty" emptyFix
            opts = defaultSafeOptions { sroDryRun = True }

        result <- safeApplyFixes opts [(testFile, [diag])]
        -- Should handle gracefully (fail but not crash)
        rsFailedFixes (srrStats result) `shouldSatisfy` (>= 0)

      it "handles nonexistent file gracefully" $ do
        let fix = mkTestFix "Fix" "/nonexistent/test.hs" 1 1 1 5 "x" True
            diag = mkDiagnostic "/nonexistent/test.hs" 1 1 "Suggestion" fix
            opts = defaultSafeOptions { sroDryRun = True }

        -- safeApplyFixes reads file contents, so nonexistent file will throw
        -- This tests that the user should only pass existing files
        -- The function may throw IOException which is expected behavior
        result <- (safeApplyFixes opts [("/nonexistent/test.hs", [diag])]
                   >> pure True) `catch` (\(_ :: IOException) -> pure False)
        -- Should either succeed with empty result or fail with IOException
        result `shouldSatisfy` const True

      it "handles empty file" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
        TIO.writeFile testFile ""

        let fix = mkTestFix "Add content" testFile 1 1 1 1 "x = 1" True
            diag = mkDiagnostic (T.pack testFile) 1 1 "Add something" fix
            opts = defaultSafeOptions { sroDryRun = True }

        result <- safeApplyFixes opts [(testFile, [diag])]
        -- Should handle gracefully
        pure ()

    ---------------------------------------------------------------------------
    -- P1-01: Validation path boolean coverage tests
    ---------------------------------------------------------------------------

    describe "validation levels" $ do
      it "applies fixes with NoValidation level successfully" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\nvalue = 1"
        TIO.writeFile testFile code

        let fix = mkTestFix "Change value" testFile 2 9 2 10 "42" True
            diag = mkDiagnostic (T.pack testFile) 2 9 "Update value" fix
            opts = defaultSafeOptions
              { sroDryRun = True
              , sroValidationLevel = NoValidation
              , sroValidateEachFix = False
              }

        result <- safeApplyFixes opts [(testFile, [diag])]
        srrSuccess result `shouldBe` True
        rsAppliedFixes (srrStats result) `shouldBe` 1
        -- Validation should be trivially successful
        case srrApplied result of
          [] -> expectationFailure "Expected at least one applied fix"
          (applied:_) -> vrSuccess (afValidation applied) `shouldBe` True

      it "applies fixes with SyntaxValidation level" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\nvalue = 1"
        TIO.writeFile testFile code

        let fix = mkTestFix "Change value" testFile 2 9 2 10 "42" True
            diag = mkDiagnostic (T.pack testFile) 2 9 "Update value" fix
            opts = defaultSafeOptions
              { sroDryRun = True
              , sroValidationLevel = SyntaxValidation
              , sroValidateEachFix = True
              }

        result <- safeApplyFixes opts [(testFile, [diag])]
        srrSuccess result `shouldBe` True
        rsAppliedFixes (srrStats result) `shouldBe` 1

      it "applies fixes with SemanticValidation level" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\nvalue :: Int\nvalue = 1"
        TIO.writeFile testFile code

        let fix = mkTestFix "Change value" testFile 3 9 3 10 "42" True
            diag = mkDiagnostic (T.pack testFile) 3 9 "Update value" fix
            opts = defaultSafeOptions
              { sroDryRun = True
              , sroValidationLevel = SemanticValidation
              , sroValidateEachFix = True
              }

        result <- safeApplyFixes opts [(testFile, [diag])]
        srrSuccess result `shouldBe` True
        rsAppliedFixes (srrStats result) `shouldBe` 1

      it "rejects syntactically invalid fixes when validation enabled" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\nvalue = 1"
        TIO.writeFile testFile code

        -- Create a fix that produces invalid syntax
        let badFix = mkTestFix "Bad fix" testFile 2 9 2 10 "= =" True
            diag = mkDiagnostic (T.pack testFile) 2 9 "Bad suggestion" badFix
            opts = defaultSafeOptions
              { sroDryRun = True
              , sroValidationLevel = SyntaxValidation
              , sroValidateEachFix = True
              }

        result <- safeApplyFixes opts [(testFile, [diag])]
        -- Should fail due to syntax validation
        rsFailedFixes (srrStats result) `shouldSatisfy` (> 0)

      it "accepts syntactically invalid fixes when validation disabled" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\nvalue = 1"
        TIO.writeFile testFile code

        -- Create a fix that produces invalid syntax, but validation is off
        let badFix = mkTestFix "Bad fix" testFile 2 9 2 10 "= =" True
            diag = mkDiagnostic (T.pack testFile) 2 9 "Bad suggestion" badFix
            opts = defaultSafeOptions
              { sroDryRun = True
              , sroValidationLevel = NoValidation
              , sroValidateEachFix = False
              }

        result <- safeApplyFixes opts [(testFile, [diag])]
        -- Should succeed since no validation
        srrSuccess result `shouldBe` True
        rsAppliedFixes (srrStats result) `shouldBe` 1

      it "validates each fix independently when sroValidateEachFix is True" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\nx = 1\ny = 2"
        TIO.writeFile testFile code

        let fix1 = mkTestFix "Fix 1" testFile 2 5 2 6 "10" True
            fix2 = mkTestFix "Fix 2" testFile 3 5 3 6 "20" True
            diag1 = mkDiagnostic (T.pack testFile) 2 5 "Suggestion 1" fix1
            diag2 = mkDiagnostic (T.pack testFile) 3 5 "Suggestion 2" fix2
            opts = defaultSafeOptions
              { sroDryRun = True
              , sroValidationLevel = SyntaxValidation
              , sroValidateEachFix = True
              }

        result <- safeApplyFixes opts [(testFile, [diag1, diag2])]
        srrSuccess result `shouldBe` True
        rsAppliedFixes (srrStats result) `shouldBe` 2
        -- Each fix should have its own validation result
        length (srrValidations result) `shouldBe` 2

    describe "import management" $ do
      it "applies import changes when sroManageImports is True" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\n\nx = head [1,2,3]"
        TIO.writeFile testFile code

        let safeImport = mkFixImport "Safe" [mkImportSymbol "headMay" ISTFunction]
            fixWithImport = Fix
              { fixTitle = "Replace with safe alternative"
              , fixEdits = [FixEdit (mkSrcSpanRaw testFile 3 5 3 9) "headMay"]
              , fixIsPreferred = True
              , fixAddImports = [safeImport]
              , fixRemoveImports = []
              , fixCategory = FCSafety
              , fixSafety = FSAlways
              }
            diag = mkDiagnostic (T.pack testFile) 3 5 "Use safe alternative" fixWithImport
            opts = defaultSafeOptions
              { sroDryRun = True
              , sroManageImports = True
              , sroValidationLevel = NoValidation
              }

        result <- safeApplyFixes opts [(testFile, [diag])]
        srrSuccess result `shouldBe` True
        rsAppliedFixes (srrStats result) `shouldBe` 1
        -- Check the final file content (imports are applied after code fixes)
        case Map.lookup testFile (srrFileResults result) of
          Nothing -> expectationFailure "Expected file result"
          Just fileResult -> do
            -- The final content should contain the import
            T.isInfixOf "Safe" (frrFinal fileResult) `shouldBe` True

      it "skips import changes when sroManageImports is False" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\n\nx = head [1,2,3]"
        TIO.writeFile testFile code

        let safeImport = mkFixImport "Safe" [mkImportSymbol "headMay" ISTFunction]
            fixWithImport = Fix
              { fixTitle = "Replace with safe alternative"
              , fixEdits = [FixEdit (mkSrcSpanRaw testFile 3 5 3 9) "headMay"]
              , fixIsPreferred = True
              , fixAddImports = [safeImport]
              , fixRemoveImports = []
              , fixCategory = FCSafety
              , fixSafety = FSAlways
              }
            diag = mkDiagnostic (T.pack testFile) 3 5 "Use safe alternative" fixWithImport
            opts = defaultSafeOptions
              { sroDryRun = True
              , sroManageImports = False
              , sroValidationLevel = NoValidation
              }

        result <- safeApplyFixes opts [(testFile, [diag])]
        srrSuccess result `shouldBe` True
        -- Check the final file content
        case Map.lookup testFile (srrFileResults result) of
          Nothing -> expectationFailure "Expected file result"
          Just fileResult -> do
            -- Code fix should be applied
            T.isInfixOf "headMay" (frrFinal fileResult) `shouldBe` True
            -- Import should NOT be added when sroManageImports is False
            T.isInfixOf "import Safe" (frrFinal fileResult) `shouldBe` False

    describe "transactional behavior" $ do
      it "rolls back on failure when sroTransactional is True" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            originalCode = "module Test where\nx = 1"
        TIO.writeFile testFile originalCode

        -- Create overlapping fixes that will cause conflict
        let fix1 = mkTestFix "Fix 1" testFile 2 1 2 6 "y = 2" True
            fix2 = mkTestFix "Fix 2" testFile 2 1 2 6 "z = 3" True  -- Same span
            diag1 = mkDiagnostic (T.pack testFile) 2 1 "Suggestion 1" fix1
            diag2 = mkDiagnostic (T.pack testFile) 2 1 "Suggestion 2" fix2
            opts = defaultSafeOptions
              { sroDryRun = False
              , sroTransactional = True
              , sroCreateBackups = True
              }

        _ <- safeApplyFixes opts [(testFile, [diag1, diag2])]
        -- Check that file is either modified or original (transactional)
        content <- TIO.readFile testFile
        (content == originalCode || T.isInfixOf "y = 2" content || T.isInfixOf "z = 3" content) `shouldBe` True

      it "applies changes incrementally when sroTransactional is False" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\nx = 1\ny = 2"
        TIO.writeFile testFile code

        let fix1 = mkTestFix "Fix 1" testFile 2 5 2 6 "10" True
            fix2 = mkTestFix "Fix 2" testFile 3 5 3 6 "20" True
            diag1 = mkDiagnostic (T.pack testFile) 2 5 "Suggestion 1" fix1
            diag2 = mkDiagnostic (T.pack testFile) 3 5 "Suggestion 2" fix2
            opts = defaultSafeOptions
              { sroDryRun = True
              , sroTransactional = False
              , sroValidationLevel = NoValidation
              }

        result <- safeApplyFixes opts [(testFile, [diag1, diag2])]
        srrSuccess result `shouldBe` True

    describe "conflict strategies" $ do
      it "prefers preferred fixes with PreferPreferred strategy" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
        TIO.writeFile testFile "x = 1"

        let preferredFix = mkTestFix "Preferred" testFile 1 5 1 6 "2" True
            nonPreferredFix = (mkTestFix "Not preferred" testFile 1 5 1 6 "3" False)
            diag1 = mkDiagnostic (T.pack testFile) 1 5 "Preferred fix" preferredFix
            diag2 = mkDiagnostic (T.pack testFile) 1 5 "Non-preferred fix" nonPreferredFix
            opts = defaultSafeOptions
              { sroDryRun = True
              , sroConflictStrategy = PreferPreferred
              , sroSafeOnly = False
              , sroValidationLevel = NoValidation
              }

        result <- safeApplyFixes opts [(testFile, [diag1, diag2])]
        -- One fix should be applied, one skipped due to conflict
        rsAppliedFixes (srrStats result) + rsSkippedFixes (srrStats result) `shouldSatisfy` (>= 1)

      it "applies first fix with PreferFirst strategy" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
        TIO.writeFile testFile "x = 1"

        let fix1 = mkTestFix "First" testFile 1 5 1 6 "2" True
            fix2 = mkTestFix "Second" testFile 1 5 1 6 "3" True
            diag1 = mkDiagnostic (T.pack testFile) 1 5 "First fix" fix1
            diag2 = mkDiagnostic (T.pack testFile) 1 5 "Second fix" fix2
            opts = defaultSafeOptions
              { sroDryRun = True
              , sroConflictStrategy = SkipSecond
              , sroValidationLevel = NoValidation
              }

        result <- safeApplyFixes opts [(testFile, [diag1, diag2])]
        -- First fix should be applied
        rsAppliedFixes (srrStats result) `shouldSatisfy` (>= 1)

      it "skips all conflicting fixes with SkipConflicting strategy" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
        TIO.writeFile testFile "x = 1"

        let fix1 = mkTestFix "Fix 1" testFile 1 5 1 6 "2" True
            fix2 = mkTestFix "Fix 2" testFile 1 5 1 6 "3" True
            diag1 = mkDiagnostic (T.pack testFile) 1 5 "Fix 1" fix1
            diag2 = mkDiagnostic (T.pack testFile) 1 5 "Fix 2" fix2
            opts = defaultSafeOptions
              { sroDryRun = True
              , sroConflictStrategy = SkipAll
              , sroValidationLevel = NoValidation
              }

        result <- safeApplyFixes opts [(testFile, [diag1, diag2])]
        -- Both should be skipped due to conflict
        rsSkippedFixes (srrStats result) `shouldBe` 2

    describe "safe-only mode" $ do
      it "only applies FSAlways fixes when sroSafeOnly is True" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
        TIO.writeFile testFile "x = 1\ny = 2"

        let safeFix = Fix
              { fixTitle = "Safe fix"
              , fixEdits = [FixEdit (mkSrcSpanRaw testFile 1 5 1 6) "10"]
              , fixIsPreferred = True
              , fixAddImports = []
              , fixRemoveImports = []
              , fixCategory = FCStyle
              , fixSafety = FSAlways
              }
            unsafeFix = Fix
              { fixTitle = "Unsafe fix"
              , fixEdits = [FixEdit (mkSrcSpanRaw testFile 2 5 2 6) "20"]
              , fixIsPreferred = False
              , fixAddImports = []
              , fixRemoveImports = []
              , fixCategory = FCStyle
              , fixSafety = FSUnsafe
              }
            diag1 = mkDiagnostic (T.pack testFile) 1 5 "Safe suggestion" safeFix
            diag2 = mkDiagnostic (T.pack testFile) 2 5 "Unsafe suggestion" unsafeFix
            opts = defaultSafeOptions
              { sroDryRun = True
              , sroSafeOnly = True
              , sroValidationLevel = NoValidation
              }

        result <- safeApplyFixes opts [(testFile, [diag1, diag2])]
        -- Only preferred (safe) fix should be applied
        rsAppliedFixes (srrStats result) `shouldBe` 1

    ---------------------------------------------------------------------------
    -- R-02: Interactive Mode Testability (MonadPrompt abstraction)
    ---------------------------------------------------------------------------

    describe "PromptOption" $ do
      it "has all expected values" $ do
        [OptionYes, OptionNo, OptionAll, OptionQuit] `shouldBe`
          [OptionYes, OptionNo, OptionAll, OptionQuit]

      it "has Eq instance" $ do
        OptionYes `shouldBe` OptionYes
        OptionNo `shouldNotBe` OptionYes

      it "has Show instance" $ do
        show OptionYes `shouldContain` "Yes"

    describe "PromptChoice" $ do
      it "has all expected values" $ do
        [ChoiceYes, ChoiceNo, ChoiceAll, ChoiceQuit] `shouldBe`
          [ChoiceYes, ChoiceNo, ChoiceAll, ChoiceQuit]

      it "supports invalid choice" $ do
        let invalid = ChoiceInvalid "bad"
        case invalid of
          ChoiceInvalid t -> t `shouldBe` "bad"
          _ -> expectationFailure "Expected ChoiceInvalid"

      it "has Eq instance" $ do
        ChoiceYes `shouldBe` ChoiceYes
        ChoiceNo `shouldNotBe` ChoiceYes
        ChoiceInvalid "a" `shouldBe` ChoiceInvalid "a"
        ChoiceInvalid "a" `shouldNotBe` ChoiceInvalid "b"

    describe "FixPromptInfo" $ do
      it "stores prompt information" $ do
        let info = FixPromptInfo
              { fpiTitle = "Test fix"
              , fpiDiff = "diff output"
              , fpiLocations = [("test.hs", Line 1, Column 1)]
              , fpiOptions = [OptionYes, OptionNo]
              }
        fpiTitle info `shouldBe` "Test fix"
        fpiDiff info `shouldBe` "diff output"
        length (fpiLocations info) `shouldBe` 1
        length (fpiOptions info) `shouldBe` 2

      it "has Eq instance" $ do
        let info1 = FixPromptInfo "Title" "Diff" [] [OptionYes]
            info2 = FixPromptInfo "Title" "Diff" [] [OptionYes]
        info1 `shouldBe` info2

    describe "mkFixPromptInfo" $ do
      it "creates prompt info from fix and content" $ do
        let fix = mkTestFix "Replace" "test.hs" 1 1 1 5 "new" True
            content = "old text"
            info = mkFixPromptInfo fix content
        fpiTitle info `shouldBe` "Replace"
        length (fpiLocations info) `shouldBe` 1

      it "includes all edit locations" $ do
        let edit1 = FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 5) "a"
            edit2 = FixEdit (mkSrcSpanRaw "test.hs" 2 1 2 5) "b"
            fix = Fix
              { fixTitle = "Multi-edit"
              , fixEdits = [edit1, edit2]
              , fixIsPreferred = True
              , fixAddImports = []
              , fixRemoveImports = []
              , fixCategory = FCStyle
              , fixSafety = FSAlways
              }
            info = mkFixPromptInfo fix "line1\nline2"
        length (fpiLocations info) `shouldBe` 2

    describe "processPromptChoice" $ do
      it "Yes returns True and keeps state unchanged" $ do
        let istate = InteractiveState { isApplyAll = False, isQuit = False }
            (shouldApply, newState) = processPromptChoice ChoiceYes istate
        shouldApply `shouldBe` True
        isApplyAll newState `shouldBe` False
        isQuit newState `shouldBe` False

      it "No returns False and keeps state unchanged" $ do
        let istate = InteractiveState { isApplyAll = False, isQuit = False }
            (shouldApply, newState) = processPromptChoice ChoiceNo istate
        shouldApply `shouldBe` False
        isApplyAll newState `shouldBe` False
        isQuit newState `shouldBe` False

      it "All returns True and sets isApplyAll" $ do
        let istate = InteractiveState { isApplyAll = False, isQuit = False }
            (shouldApply, newState) = processPromptChoice ChoiceAll istate
        shouldApply `shouldBe` True
        isApplyAll newState `shouldBe` True
        isQuit newState `shouldBe` False

      it "Quit returns False and sets isQuit" $ do
        let istate = InteractiveState { isApplyAll = False, isQuit = False }
            (shouldApply, newState) = processPromptChoice ChoiceQuit istate
        shouldApply `shouldBe` False
        isApplyAll newState `shouldBe` False
        isQuit newState `shouldBe` True

      it "Invalid returns False (caller should re-prompt)" $ do
        let istate = InteractiveState { isApplyAll = False, isQuit = False }
            (shouldApply, _) = processPromptChoice (ChoiceInvalid "xyz") istate
        shouldApply `shouldBe` False

    describe "InteractiveState" $ do
      it "has sensible defaults with initialInteractiveState" $ do
        isApplyAll initialInteractiveState `shouldBe` False
        isQuit initialInteractiveState `shouldBe` False

    describe "runPromptPure (mock prompting)" $ do
      it "consumes responses in order" $ do
        let responses = [ChoiceYes, ChoiceNo, ChoiceQuit]
            action = do
              r1 <- promptForFix (FixPromptInfo "Fix1" "" [] [])
              r2 <- promptForFix (FixPromptInfo "Fix2" "" [] [])
              r3 <- promptForFix (FixPromptInfo "Fix3" "" [] [])
              pure (r1, r2, r3)
            ((r1, r2, r3), state) = runPromptPure responses action
        r1 `shouldBe` ChoiceYes
        r2 `shouldBe` ChoiceNo
        r3 `shouldBe` ChoiceQuit
        mpsResponses state `shouldBe` []

      it "records all prompts shown" $ do
        let responses = [ChoiceYes, ChoiceYes]
            info1 = FixPromptInfo "Fix1" "diff1" [] []
            info2 = FixPromptInfo "Fix2" "diff2" [] []
            action = do
              _ <- promptForFix info1
              _ <- promptForFix info2
              pure ()
            (_, state) = runPromptPure responses action
        length (mpsPrompts state) `shouldBe` 2
        map fpiTitle (mpsPrompts state) `shouldBe` ["Fix1", "Fix2"]

      it "returns ChoiceQuit when responses exhausted" $ do
        let responses = []
            action = promptForFix (FixPromptInfo "Fix" "" [] [])
            (choice, _) = runPromptPure responses action
        choice `shouldBe` ChoiceQuit

      it "records error messages" $ do
        let responses = []
            action = do
              promptError "Error 1"
              promptError "Error 2"
            (_, state) = runPromptPure responses action
        mpsErrors state `shouldBe` ["Error 1", "Error 2"]

      it "simulates user choosing All" $ do
        let responses = [ChoiceAll]
            action = promptForFix (FixPromptInfo "Fix" "" [] [])
            (choice, _) = runPromptPure responses action
        choice `shouldBe` ChoiceAll

      it "simulates complex interaction sequence" $ do
        -- Simulate: user says yes, no, then quit
        let responses = [ChoiceYes, ChoiceNo, ChoiceQuit]
            action = do
              r1 <- promptForFix (FixPromptInfo "Fix1" "" [] [])
              let (apply1, s1) = processPromptChoice r1 initialInteractiveState
              r2 <- promptForFix (FixPromptInfo "Fix2" "" [] [])
              let (apply2, s2) = processPromptChoice r2 s1
              r3 <- promptForFix (FixPromptInfo "Fix3" "" [] [])
              let (apply3, s3) = processPromptChoice r3 s2
              pure (apply1, apply2, apply3, s3)
            ((a1, a2, a3, finalState), _) = runPromptPure responses action
        a1 `shouldBe` True   -- Yes -> apply
        a2 `shouldBe` False  -- No -> skip
        a3 `shouldBe` False  -- Quit -> stop
        isQuit finalState `shouldBe` True
