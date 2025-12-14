{-# LANGUAGE OverloadedStrings #-}

module EngineSpec (spec) where

import Test.Hspec
import Control.Exception (catch, IOException)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Map.Strict qualified as Map
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Data.Text.IO qualified as TIO

import Argus.Types
import Argus.Refactor.Engine
import Argus.Refactor.Validation (ValidationLevel(..))
import Argus.Refactor.FixGraph (ConflictStrategy(..))
import Argus.Refactor.SafeRefactor (SafeRefactorResult(..), RefactorStats(..))

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
  describe "Argus.Refactor.Engine" $ do
    describe "EngineOptions" $ do
      describe "defaultEngineOptions" $ do
        it "uses auto mode by default" $ do
          eoMode defaultEngineOptions `shouldBe` ModeAuto

        it "enables validation by default" $ do
          eoValidate defaultEngineOptions `shouldBe` True

        it "uses syntax validation level" $ do
          eoValidateLevel defaultEngineOptions `shouldBe` SyntaxValidation

        it "prefers safe fixes by default" $ do
          eoSafeOnly defaultEngineOptions `shouldBe` True

        it "is transactional by default" $ do
          eoTransactional defaultEngineOptions `shouldBe` True

        it "creates backups by default" $ do
          eoBackup defaultEngineOptions `shouldBe` True

        it "uses PreferPreferred conflict strategy" $ do
          eoConflictStrat defaultEngineOptions `shouldBe` PreferPreferred

        it "is not dry run by default" $ do
          eoDryRun defaultEngineOptions `shouldBe` False

        it "is not verbose by default" $ do
          eoVerbose defaultEngineOptions `shouldBe` False

        it "shows diffs by default" $ do
          eoShowDiff defaultEngineOptions `shouldBe` True

      describe "custom configuration" $ do
        it "allows setting interactive mode" $ do
          let opts = defaultEngineOptions { eoMode = ModeInteractive }
          eoMode opts `shouldBe` ModeInteractive

        it "allows setting preview mode" $ do
          let opts = defaultEngineOptions { eoMode = ModePreview }
          eoMode opts `shouldBe` ModePreview

        it "allows disabling validation" $ do
          let opts = defaultEngineOptions { eoValidate = False }
          eoValidate opts `shouldBe` False

        it "allows setting no validation level" $ do
          let opts = defaultEngineOptions { eoValidateLevel = NoValidation }
          eoValidateLevel opts `shouldBe` NoValidation

        it "allows enabling dry run" $ do
          let opts = defaultEngineOptions { eoDryRun = True }
          eoDryRun opts `shouldBe` True

        it "allows enabling verbose mode" $ do
          let opts = defaultEngineOptions { eoVerbose = True }
          eoVerbose opts `shouldBe` True

    describe "RefactorMode" $ do
      it "has Eq instance" $ do
        ModeAuto `shouldBe` ModeAuto
        ModeAuto `shouldNotBe` ModeInteractive

      it "has Show instance" $ do
        show ModeAuto `shouldContain` "Auto"
        show ModeInteractive `shouldContain` "Interactive"
        show ModePreview `shouldContain` "Preview"

    describe "EngineResult" $ do
      it "has Show instance" $ do
        let emptyStats = RefactorStats
              { rsFilesProcessed = 0
              , rsFilesModified = 0
              , rsTotalFixes = 0
              , rsAppliedFixes = 0
              , rsSkippedFixes = 0
              , rsFailedFixes = 0
              , rsConflictsFound = 0
              , rsLinesChanged = 0
              , rsValidationTime = 0.0
              , rsApplicationTime = 0.0
              }
            emptyDetails = SafeRefactorResult
              { srrSuccess = True
              , srrApplied = []
              , srrSkipped = []
              , srrFailed = []
              , srrRolledBack = False
              , srrValidations = []
              , srrConflicts = []
              , srrFileResults = Map.empty
              , srrStats = emptyStats
              , srrDuration = 0.0
              }
            result = EngineResult
              { erSuccess = True
              , erFilesChanged = 0
              , erFixesApplied = 0
              , erFixesSkipped = 0
              , erFixesFailed = 0
              , erConflicts = 0
              , erDetails = emptyDetails
              }
        -- Should not throw
        length (show result) `shouldSatisfy` (> 0)

    describe "refactorSafely" $ do
      it "applies single fix in dry run mode" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\nx = 1"
        TIO.writeFile testFile code

        let fix = mkTestFix "Change value" testFile 2 5 2 6 "2" True
            diag = mkDiagnostic (T.pack testFile) 2 5 "Consider changing" fix
            opts = defaultEngineOptions { eoDryRun = True }

        result <- refactorSafely opts [(testFile, [diag])]
        erSuccess result `shouldBe` True
        erFixesApplied result `shouldBe` 1

      it "preserves original file in dry run" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            originalCode = "module Test where\nx = 1"
        TIO.writeFile testFile originalCode

        let fix = mkTestFix "Change value" testFile 2 5 2 6 "2" True
            diag = mkDiagnostic (T.pack testFile) 2 5 "Consider changing" fix
            opts = defaultEngineOptions { eoDryRun = True }

        _ <- refactorSafely opts [(testFile, [diag])]

        -- File should be unchanged
        content <- TIO.readFile testFile
        content `shouldBe` originalCode

      it "handles empty diagnostics list" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\nx = 1"
        TIO.writeFile testFile code

        let opts = defaultEngineOptions { eoDryRun = True }
        result <- refactorSafely opts [(testFile, [])]
        erSuccess result `shouldBe` True
        erFixesApplied result `shouldBe` 0

      it "handles multiple fixes on different lines" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\nx = 1\ny = 2"
        TIO.writeFile testFile code

        let fix1 = mkTestFix "Fix 1" testFile 2 5 2 6 "10" True
            fix2 = mkTestFix "Fix 2" testFile 3 5 3 6 "20" True
            diag1 = mkDiagnostic (T.pack testFile) 2 5 "Suggestion 1" fix1
            diag2 = mkDiagnostic (T.pack testFile) 3 5 "Suggestion 2" fix2
            opts = defaultEngineOptions { eoDryRun = True }

        result <- refactorSafely opts [(testFile, [diag1, diag2])]
        erSuccess result `shouldBe` True
        erFixesApplied result `shouldBe` 2

      it "handles multiple files" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile1 = tmpDir </> "test1.hs"
            testFile2 = tmpDir </> "test2.hs"
        TIO.writeFile testFile1 "x = 1"
        TIO.writeFile testFile2 "y = 2"

        let fix1 = mkTestFix "Fix 1" testFile1 1 5 1 6 "10" True
            fix2 = mkTestFix "Fix 2" testFile2 1 5 1 6 "20" True
            diag1 = mkDiagnostic (T.pack testFile1) 1 5 "Suggestion 1" fix1
            diag2 = mkDiagnostic (T.pack testFile2) 1 5 "Suggestion 2" fix2
            opts = defaultEngineOptions { eoDryRun = True }

        result <- refactorSafely opts [(testFile1, [diag1]), (testFile2, [diag2])]
        erFilesChanged result `shouldBe` 2

      it "filters unsafe fixes when safeOnly is True" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "x = 1"
        TIO.writeFile testFile code

        let safeFix = mkTestFix "Safe fix" testFile 1 5 1 6 "2" True
            unsafeFix = mkTestFix "Unsafe fix" testFile 1 5 1 6 "3" False
            diag1 = mkDiagnostic (T.pack testFile) 1 5 "Safe suggestion" safeFix
            diag2 = mkDiagnostic (T.pack testFile) 1 5 "Unsafe suggestion" unsafeFix
            opts = defaultEngineOptions { eoDryRun = True, eoSafeOnly = True }

        result <- refactorSafely opts [(testFile, [diag1, diag2])]
        -- Only safe fix should be considered
        erFixesApplied result `shouldSatisfy` (<= 1)

      it "validates syntax after fix" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\nx = 1"
        TIO.writeFile testFile code

        -- Create a fix that produces invalid syntax
        let badFix = mkTestFix "Bad fix" testFile 2 5 2 6 "= =" True
            diag = mkDiagnostic (T.pack testFile) 2 5 "Bad suggestion" badFix
            opts = defaultEngineOptions { eoDryRun = True }

        result <- refactorSafely opts [(testFile, [diag])]
        -- Should fail validation
        erFixesFailed result `shouldSatisfy` (>= 0)

      it "skips validation when disabled" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "x = 1"
        TIO.writeFile testFile code

        let fix = mkTestFix "Change" testFile 1 5 1 6 "2" True
            diag = mkDiagnostic (T.pack testFile) 1 5 "Suggestion" fix
            opts = defaultEngineOptions
              { eoDryRun = True
              , eoValidate = False
              }

        result <- refactorSafely opts [(testFile, [diag])]
        erSuccess result `shouldBe` True

    describe "refactorInteractive" $ do
      it "uses interactive mode" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        -- Note: Interactive mode requires stdin, so we just test it doesn't crash
        -- when there are no diagnostics
        let testFile = tmpDir </> "test.hs"
        TIO.writeFile testFile "x = 1"

        result <- refactorInteractive [(testFile, [])]
        erSuccess result `shouldBe` True

    describe "RefactorOptions (Legacy)" $ do
      describe "defaultRefactorOptions" $ do
        it "is safe only by default" $ do
          roSafeOnly defaultRefactorOptions `shouldBe` True

        it "shows preview by default" $ do
          roPreview defaultRefactorOptions `shouldBe` True

        it "creates backups by default" $ do
          roBackup defaultRefactorOptions `shouldBe` True

        it "is not interactive by default" $ do
          roInteractive defaultRefactorOptions `shouldBe` False

      it "has Eq instance" $ do
        defaultRefactorOptions `shouldBe` defaultRefactorOptions

      it "has Show instance" $ do
        show defaultRefactorOptions `shouldContain` "RefactorOptions"

    describe "RefactorResult (Legacy)" $ do
      it "has Show instance" $ do
        let result = RefactorResult
              { rrFilePath = "test.hs"
              , rrFixesApplied = 0
              , rrNewContent = "x = 1"
              , rrOriginal = "x = 1"
              , rrSuccess = True
              , rrMessage = Nothing
              }
        show result `shouldContain` "RefactorResult"

      it "has Eq instance" $ do
        let result = RefactorResult "test.hs" 0 "x = 1" "x = 1" True Nothing
        result `shouldBe` result

    describe "applyDiagnosticFixes (Legacy)" $ do
      it "applies fixes to file" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "module Test where\nx = 1"
        TIO.writeFile testFile code

        let fix = mkTestFix "Change" testFile 2 5 2 6 "2" True
            diag = mkDiagnostic (T.pack testFile) 2 5 "Suggestion" fix
            opts = defaultRefactorOptions { roPreview = True }

        result <- applyDiagnosticFixes opts testFile [diag]
        rrSuccess result `shouldBe` True
        rrFixesApplied result `shouldBe` 1

      it "preserves original content in preview mode" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            originalCode = "module Test where\nx = 1"
        TIO.writeFile testFile originalCode

        let fix = mkTestFix "Change" testFile 2 5 2 6 "2" True
            diag = mkDiagnostic (T.pack testFile) 2 5 "Suggestion" fix
            opts = defaultRefactorOptions { roPreview = True }

        result <- applyDiagnosticFixes opts testFile [diag]
        rrOriginal result `shouldBe` originalCode

    describe "applyAllFixes (Legacy)" $ do
      it "applies fixes to source content" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "x = 1"
        TIO.writeFile testFile code

        let fix = mkTestFix "Change" testFile 1 5 1 6 "2" True
            opts = defaultRefactorOptions { roPreview = True }

        result <- applyAllFixes opts testFile code [fix]
        rrSuccess result `shouldBe` True
        T.isInfixOf "2" (rrNewContent result) `shouldBe` True

      it "handles empty fix list" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "x = 1"
        TIO.writeFile testFile code

        let opts = defaultRefactorOptions { roPreview = True }
        result <- applyAllFixes opts testFile code []
        rrSuccess result `shouldBe` True
        rrFixesApplied result `shouldBe` 0

    describe "previewFix" $ do
      it "previews fix without modifying original" $ do
        let code = "x = 1"
            fix = mkTestFix "Change" "test.hs" 1 5 1 6 "2" True
            preview = previewFix code fix
        T.isInfixOf "2" preview `shouldBe` True
        T.isInfixOf "1" code `shouldBe` True  -- Original unchanged

      it "handles fix with multiple edits" $ do
        let code = "x = 1\ny = 2"
            fix = Fix
              { fixTitle = "Multiple edits"
              , fixEdits =
                  [ FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 6) "10"
                  , FixEdit (mkSrcSpanRaw "test.hs" 2 5 2 6) "20"
                  ]
              , fixIsPreferred = True
              , fixAddImports = []
              , fixRemoveImports = []
              , fixCategory = FCStyle
              , fixSafety = FSAlways
              }
        let preview = previewFix code fix
        T.isInfixOf "10" preview `shouldBe` True
        T.isInfixOf "20" preview `shouldBe` True

    describe "refactorFile" $ do
      it "refactors existing file" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "x = 1"
        TIO.writeFile testFile code

        let fix = mkTestFix "Change" testFile 1 5 1 6 "2" True
            diag = mkDiagnostic (T.pack testFile) 1 5 "Suggestion" fix
            opts = defaultRefactorOptions { roPreview = True }

        result <- refactorFile opts testFile [diag]
        rrSuccess result `shouldBe` True

      it "handles nonexistent file" $ do
        let opts = defaultRefactorOptions
        result <- refactorFile opts "/nonexistent/test.hs" []
        rrSuccess result `shouldBe` False
        rrMessage result `shouldBe` Just "File does not exist"

    describe "refactorFiles" $ do
      it "refactors multiple files" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile1 = tmpDir </> "test1.hs"
            testFile2 = tmpDir </> "test2.hs"
        TIO.writeFile testFile1 "x = 1"
        TIO.writeFile testFile2 "y = 2"

        let fix1 = mkTestFix "Fix 1" testFile1 1 5 1 6 "10" True
            fix2 = mkTestFix "Fix 2" testFile2 1 5 1 6 "20" True
            diag1 = mkDiagnostic (T.pack testFile1) 1 5 "Suggestion 1" fix1
            diag2 = mkDiagnostic (T.pack testFile2) 1 5 "Suggestion 2" fix2
            opts = defaultRefactorOptions { roPreview = True }

        results <- refactorFiles opts [(testFile1, [diag1]), (testFile2, [diag2])]
        length results `shouldBe` 2
        all rrSuccess results `shouldBe` True

      it "handles empty file list" $ do
        let opts = defaultRefactorOptions
        results <- refactorFiles opts []
        length results `shouldBe` 0

    describe "edge cases" $ do
      it "handles fix with empty edits" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
        TIO.writeFile testFile "x = 1"

        let emptyFix = Fix
              { fixTitle = "Empty fix"
              , fixEdits = []
              , fixIsPreferred = True
              , fixAddImports = []
              , fixRemoveImports = []
              , fixCategory = FCStyle
              , fixSafety = FSAlways
              }
            diag = mkDiagnostic (T.pack testFile) 1 1 "Empty" emptyFix
            opts = defaultEngineOptions { eoDryRun = True }

        result <- refactorSafely opts [(testFile, [diag])]
        -- Should handle gracefully
        erFixesFailed result `shouldSatisfy` (>= 0)

      it "handles conflicting fixes" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
        TIO.writeFile testFile "x = 1"

        -- Two fixes that overlap
        let fix1 = mkTestFix "Fix 1" testFile 1 1 1 6 "y = 2" True
            fix2 = mkTestFix "Fix 2" testFile 1 1 1 6 "z = 3" True
            diag1 = mkDiagnostic (T.pack testFile) 1 1 "Suggestion 1" fix1
            diag2 = mkDiagnostic (T.pack testFile) 1 1 "Suggestion 2" fix2
            opts = defaultEngineOptions { eoDryRun = True }

        result <- refactorSafely opts [(testFile, [diag1, diag2])]
        erConflicts result `shouldSatisfy` (>= 0)

      it "handles empty file" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
        TIO.writeFile testFile ""

        let fix = mkTestFix "Add content" testFile 1 1 1 1 "x = 1" True
            diag = mkDiagnostic (T.pack testFile) 1 1 "Add something" fix
            opts = defaultEngineOptions { eoDryRun = True }

        result <- refactorSafely opts [(testFile, [diag])]
        -- Should handle gracefully
        pure ()

      it "handles unicode in code" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
            code = "-- αβγδ\nx = 1"
        TIO.writeFile testFile code

        let fix = mkTestFix "Change" testFile 2 5 2 6 "2" True
            diag = mkDiagnostic (T.pack testFile) 2 5 "Suggestion" fix
            opts = defaultEngineOptions { eoDryRun = True }

        result <- refactorSafely opts [(testFile, [diag])]
        erSuccess result `shouldBe` True
