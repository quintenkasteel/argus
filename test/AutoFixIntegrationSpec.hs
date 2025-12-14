{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : AutoFixIntegrationSpec
-- Description : Comprehensive integration tests for auto-fix functionality
--
-- These tests verify that:
-- 1. Fixes correctly replace text at the right positions
-- 2. The resulting code is valid Haskell that compiles
-- 3. Actual file writes work correctly (not just dry-run)
-- 4. Multi-line and complex fixes work properly
-- 5. Import management works correctly
module AutoFixIntegrationSpec (spec) where

import Control.Monad (forM_, when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Argus.Types
import Argus.Refactor.ExactPrint (applyFix, applyEdits, applyFixes)
import Argus.Refactor.Validation
import Argus.Refactor.SafeRefactor
import Argus.Refactor.Engine
import Argus.Refactor.FixGraph (ConflictStrategy(..))

spec :: Spec
spec = do
  describe "Auto-Fix Integration Tests" $ do
    exactReplacementSpec
    compilationVerificationSpec
    actualFileWriteSpec
    multiLineFixSpec
    importManagementSpec
    realWorldFixSpec
    edgeCaseSpec
    complexRefactoringSpec
    variableRenamingSpec
    importManipulationSpec
    qualifiedNameSpec
    typeSignatureSpec
    pragmaSpec
    patternMatchSpec

--------------------------------------------------------------------------------
-- Exact Replacement Tests
--------------------------------------------------------------------------------

exactReplacementSpec :: Spec
exactReplacementSpec = describe "Exact Text Replacement" $ do
  it "replaces single character at exact position" $ do
    let source = "x = 1"
        fix = testFix "Change 1 to 2" [FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 6) "2"]
        result = applyFix source fix
    result `shouldBe` "x = 2"

  it "replaces multi-character expression" $ do
    let source = "result = foo bar"
        fix = testFix "Replace foo with baz" [FixEdit (mkSrcSpanRaw "test.hs" 1 10 1 13) "baz"]
        result = applyFix source fix
    result `shouldBe` "result = baz bar"

  it "replaces with longer text" $ do
    let source = "x = f y"
        fix = testFix "Expand function" [FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 8) "myFunction argument"]
        result = applyFix source fix
    result `shouldBe` "x = myFunction argument"

  it "replaces with shorter text" $ do
    let source = "x = veryLongFunctionName"
        fix = testFix "Shorten name" [FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 25) "f"]
        result = applyFix source fix
    result `shouldBe` "x = f"

  it "replaces at line start (column 1)" $ do
    let source = "oldName = 1"
        fix = testFix "Rename" [FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 8) "newName"]
        result = applyFix source fix
    result `shouldBe` "newName = 1"

  it "preserves content before replacement" $ do
    let source = "prefix x = 1 suffix"
        fix = testFix "Change x" [FixEdit (mkSrcSpanRaw "test.hs" 1 12 1 13) "2"]
        result = applyFix source fix
    T.isPrefixOf "prefix x = " result `shouldBe` True
    T.isInfixOf "2" result `shouldBe` True

  it "preserves content after replacement" $ do
    let source = "x = 1 + y"
        fix = testFix "Change 1" [FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 6) "2"]
        result = applyFix source fix
    T.isSuffixOf "+ y" result `shouldBe` True

  it "handles multiple edits in correct order" $ do
    let source = "a = 1\nb = 2\nc = 3"
        edits = [ FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 6) "10"
                , FixEdit (mkSrcSpanRaw "test.hs" 2 5 2 6) "20"
                , FixEdit (mkSrcSpanRaw "test.hs" 3 5 3 6) "30"
                ]
        result = applyEdits source edits
    T.isInfixOf "a = 10" result `shouldBe` True
    T.isInfixOf "b = 20" result `shouldBe` True
    T.isInfixOf "c = 30" result `shouldBe` True

  it "applies edits from end to start to preserve positions" $ do
    -- When editing "abc" to "XYZ" at different positions,
    -- we must apply from end to start so positions don't shift
    let source = "a + b + c"
        edits = [ FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 2) "X"  -- a -> X
                , FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 6) "Y"  -- b -> Y
                , FixEdit (mkSrcSpanRaw "test.hs" 1 9 1 10) "Z" -- c -> Z
                ]
        result = applyEdits source edits
    result `shouldBe` "X + Y + Z"

--------------------------------------------------------------------------------
-- Compilation Verification Tests
--------------------------------------------------------------------------------

compilationVerificationSpec :: Spec
compilationVerificationSpec = describe "Code Compiles After Fix" $ do
  it "verifies simple fix produces valid syntax" $ do
    let original = "module Test where\nx = 1"
        fix = testFix "Change value" [FixEdit (mkSrcSpanRaw "Test.hs" 2 5 2 6) "42"]
        transformed = applyFix original fix

    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "verifies function rename produces valid syntax" $ do
    let original = "module Test where\nfoo x = x + 1\nbar = foo 5"
        -- Rename 'foo' to 'myFunc' on line 2
        fix = testFix "Rename function" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 2 4) "myFunc"]
        transformed = applyFix original fix

    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()
    T.isInfixOf "myFunc x = x + 1" transformed `shouldBe` True

  it "verifies type signature fix produces valid syntax" $ do
    let original = "module Test where\nfoo :: Int -> Int\nfoo x = x"
        fix = testFix "Change type" [FixEdit (mkSrcSpanRaw "Test.hs" 2 8 2 11) "Integer"]
        transformed = applyFix original fix

    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()
    T.isInfixOf "Integer -> Int" transformed `shouldBe` True

  it "verifies lambda rewrite produces valid syntax" $ do
    let original = "module Test where\nf = \\x -> x + 1"
        fix = testFix "Use section" [FixEdit (mkSrcSpanRaw "Test.hs" 2 5 2 16) "(+ 1)"]
        transformed = applyFix original fix

    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()
    T.isInfixOf "f = (+ 1)" transformed `shouldBe` True

  it "verifies parentheses addition produces valid syntax" $ do
    let original = "module Test where\nf = a $ b $ c"
        fix = testFix "Add parens" [FixEdit (mkSrcSpanRaw "Test.hs" 2 5 2 14) "a (b c)"]
        transformed = applyFix original fix

    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "verifies guard to case conversion produces valid syntax" $ do
    let original = T.unlines
          [ "module Test where"
          , "f x"
          , "  | x > 0 = 1"
          , "  | otherwise = 0"
          ]
        caseExpr = T.unlines
          [ "f x = case x > 0 of"
          , "  True -> 1"
          , "  False -> 0"
          ]
        fix = testFix "Convert to case" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 5 1) caseExpr]
        transformed = applyFix original fix

    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "rejects fix that creates invalid syntax" $ do
    let original = "module Test where\nx = 1"
        -- This creates invalid syntax: x = = 1
        fix = testFix "Bad fix" [FixEdit (mkSrcSpanRaw "Test.hs" 2 5 2 5) "= "]
        transformed = applyFix original fix

    result <- validateSyntax "Test.hs" transformed
    result `shouldSatisfy` \case
      Left _ -> True
      Right _ -> False

  it "rejects fix that creates unbalanced brackets" $ do
    let original = "module Test where\nf = (x + y)"
        -- Remove closing paren
        fix = testFix "Bad fix" [FixEdit (mkSrcSpanRaw "Test.hs" 2 11 2 12) ""]
        transformed = applyFix original fix

    structResult <- validateStructure original transformed
    structResult `shouldSatisfy` \case
      Left _ -> True
      Right _ -> False

--------------------------------------------------------------------------------
-- Actual File Write Tests
--------------------------------------------------------------------------------

actualFileWriteSpec :: Spec
actualFileWriteSpec = describe "Actual File Writes" $ do
  it "writes fix to file (not dry-run)" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
    let testFile = tmpDir </> "Test.hs"
        originalCode = "module Test where\nx = 1"
    TIO.writeFile testFile originalCode

    let fix = mkPreferredFix "Change value" testFile 2 5 2 6 "42"
        diag = mkDiagnosticWithFix (T.pack testFile) 2 5 "Change to 42" fix
        opts = defaultSafeOptions
          { sroDryRun = False  -- Actually write!
          , sroValidationLevel = SyntaxValidation
          , sroSafeOnly = True
          }

    result <- safeApplyFixes opts [(testFile, [diag])]
    srrSuccess result `shouldBe` True

    -- Verify file was actually modified
    finalContent <- TIO.readFile testFile
    T.isInfixOf "x = 42" finalContent `shouldBe` True

  it "writes multiple fixes to same file" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
    let testFile = tmpDir </> "Test.hs"
        originalCode = "module Test where\na = 1\nb = 2\nc = 3"
    TIO.writeFile testFile originalCode

    let fix1 = mkPreferredFix "Change a" testFile 2 5 2 6 "10"
        fix2 = mkPreferredFix "Change b" testFile 3 5 3 6 "20"
        fix3 = mkPreferredFix "Change c" testFile 4 5 4 6 "30"
        diag1 = mkDiagnosticWithFix (T.pack testFile) 2 5 "Msg" fix1
        diag2 = mkDiagnosticWithFix (T.pack testFile) 3 5 "Msg" fix2
        diag3 = mkDiagnosticWithFix (T.pack testFile) 4 5 "Msg" fix3
        opts = defaultSafeOptions
          { sroDryRun = False
          , sroValidationLevel = SyntaxValidation
          }

    result <- safeApplyFixes opts [(testFile, [diag1, diag2, diag3])]
    srrSuccess result `shouldBe` True
    rsAppliedFixes (srrStats result) `shouldBe` 3

    finalContent <- TIO.readFile testFile
    T.isInfixOf "a = 10" finalContent `shouldBe` True
    T.isInfixOf "b = 20" finalContent `shouldBe` True
    T.isInfixOf "c = 30" finalContent `shouldBe` True

  it "writes fixes to multiple files" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
    let file1 = tmpDir </> "A.hs"
        file2 = tmpDir </> "B.hs"
    TIO.writeFile file1 "module A where\nx = 1"
    TIO.writeFile file2 "module B where\ny = 2"

    let fix1 = mkPreferredFix "Fix A" file1 2 5 2 6 "100"
        fix2 = mkPreferredFix "Fix B" file2 2 5 2 6 "200"
        diag1 = mkDiagnosticWithFix (T.pack file1) 2 5 "Msg" fix1
        diag2 = mkDiagnosticWithFix (T.pack file2) 2 5 "Msg" fix2
        opts = defaultSafeOptions { sroDryRun = False }

    result <- safeApplyFixes opts [(file1, [diag1]), (file2, [diag2])]
    srrSuccess result `shouldBe` True
    rsFilesModified (srrStats result) `shouldBe` 2

    content1 <- TIO.readFile file1
    content2 <- TIO.readFile file2
    T.isInfixOf "x = 100" content1 `shouldBe` True
    T.isInfixOf "y = 200" content2 `shouldBe` True

  it "dry-run does not modify files" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
    let testFile = tmpDir </> "Test.hs"
        originalCode = "module Test where\nx = 1"
    TIO.writeFile testFile originalCode

    let fix = mkPreferredFix "Change value" testFile 2 5 2 6 "999"
        diag = mkDiagnosticWithFix (T.pack testFile) 2 5 "Msg" fix
        opts = defaultSafeOptions { sroDryRun = True }

    _ <- safeApplyFixes opts [(testFile, [diag])]

    -- File should be unchanged
    finalContent <- TIO.readFile testFile
    finalContent `shouldBe` originalCode

  it "validates after write and file is still valid" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
    let testFile = tmpDir </> "Test.hs"
        originalCode = "module Test where\nfoo :: Int\nfoo = 1"
    TIO.writeFile testFile originalCode

    let fix = mkPreferredFix "Change type" testFile 2 8 2 11 "Integer"
        diag = mkDiagnosticWithFix (T.pack testFile) 2 8 "Msg" fix
        opts = defaultSafeOptions
          { sroDryRun = False
          , sroValidationLevel = SyntaxValidation
          , sroValidateEachFix = True
          }

    result <- safeApplyFixes opts [(testFile, [diag])]
    srrSuccess result `shouldBe` True

    -- Verify the file still compiles
    finalContent <- TIO.readFile testFile
    syntaxResult <- validateSyntax testFile finalContent
    syntaxResult `shouldBe` Right ()

--------------------------------------------------------------------------------
-- Multi-line Fix Tests
--------------------------------------------------------------------------------

multiLineFixSpec :: Spec
multiLineFixSpec = describe "Multi-line Fixes" $ do
  it "replaces multi-line expression" $ do
    let original = T.unlines
          [ "module Test where"
          , "f = do"
          , "  x <- getLine"
          , "  return x"
          ]
        -- Replace entire do block with fmap
        fix = testFix "Use fmap" [FixEdit (mkSrcSpanRaw "Test.hs" 2 5 5 1) "fmap id getLine\n"]
        transformed = applyFix original fix

    T.isInfixOf "fmap id getLine" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "replaces function with multiple clauses" $ do
    let original = T.unlines
          [ "module Test where"
          , "fib 0 = 0"
          , "fib 1 = 1"
          , "fib n = fib (n-1) + fib (n-2)"
          ]
        newFib = T.unlines
          [ "fib n"
          , "  | n <= 1   = n"
          , "  | otherwise = fib (n-1) + fib (n-2)"
          ]
        fix = testFix "Use guards" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 5 1) newFib]
        transformed = applyFix original fix

    T.isInfixOf "| n <= 1" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "handles fix spanning many lines" $ do
    let original = T.unlines
          [ "module Test where"
          , "longFunction = do"
          , "  line1"
          , "  line2"
          , "  line3"
          , "  line4"
          , "  line5"
          , "  line6"
          , "  line7"
          , "  line8"
          , "  line9"
          , "  line10"
          ]
        replacement = "longFunction = pure ()"
        fix = testFix "Simplify" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 13 1) (replacement <> "\n")]
        transformed = applyFix original fix

    T.isInfixOf "longFunction = pure ()" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "inserts multi-line code" $ do
    let original = "module Test where\nx = 1"
        newCode = T.unlines
          [ "helper :: Int -> Int"
          , "helper n = n + 1"
          , ""
          , "x = 1"
          ]
        fix = testFix "Add helper" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 2 6) newCode]
        transformed = applyFix original fix

    T.isInfixOf "helper :: Int -> Int" transformed `shouldBe` True
    T.isInfixOf "helper n = n + 1" transformed `shouldBe` True

--------------------------------------------------------------------------------
-- Import Management Tests
--------------------------------------------------------------------------------

importManagementSpec :: Spec
importManagementSpec = describe "Import Management" $ do
  it "fix with addImports includes import info" $ do
    let importSym = ImportSymbol "sort" ISTFunction []
        importInfo = FixImport
          { fimpModule = "Data.List"
          , fimpSymbols = [importSym]
          , fimpQualified = Nothing
          , fimpHiding = False
          , fimpPackage = Nothing
          }
        fix = Fix
          { fixTitle = "Use sort"
          , fixEdits = [FixEdit (mkSrcSpanRaw "Test.hs" 3 5 3 10) "sort xs"]
          , fixIsPreferred = True
          , fixAddImports = [importInfo]
          , fixRemoveImports = []
          , fixCategory = FCStyle
          , fixSafety = FSAlways
          }
    fixAddImports fix `shouldSatisfy` (not . null)
    length (fixAddImports fix) `shouldBe` 1

  it "fix with removeImports includes removal info" $ do
    let fix = Fix
          { fixTitle = "Remove unused import"
          , fixEdits = []
          , fixIsPreferred = True
          , fixAddImports = []
          , fixRemoveImports = ["Data.Maybe"]
          , fixCategory = FCStyle
          , fixSafety = FSAlways
          }
    fixRemoveImports fix `shouldSatisfy` (not . null)
    length (fixRemoveImports fix) `shouldBe` 1

--------------------------------------------------------------------------------
-- Real-World Fix Scenarios
--------------------------------------------------------------------------------

realWorldFixSpec :: Spec
realWorldFixSpec = describe "Real-World Fix Scenarios" $ do
  it "replaces head with safe pattern match" $ do
    let original = T.unlines
          [ "module Test where"
          , "first xs = head xs"
          ]
        fix = testFix "Use pattern match" [FixEdit (mkSrcSpanRaw "Test.hs" 2 12 2 19) "case xs of { (x:_) -> x; [] -> error \"empty\" }"]
        transformed = applyFix original fix

    T.isInfixOf "case xs of" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "replaces foldl with foldl'" $ do
    let original = T.unlines
          [ "module Test where"
          , "sumList = foldl (+) 0"
          ]
        fix = testFix "Use foldl'" [FixEdit (mkSrcSpanRaw "Test.hs" 2 11 2 16) "foldl'"]
        transformed = applyFix original fix

    T.isInfixOf "foldl'" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "adds bang pattern for strictness" $ do
    let original = T.unlines
          [ "{-# LANGUAGE BangPatterns #-}"
          , "module Test where"
          , "f x = x + 1"
          ]
        fix = testFix "Add bang" [FixEdit (mkSrcSpanRaw "Test.hs" 3 3 3 4) "!x"]
        transformed = applyFix original fix

    T.isInfixOf "f !x = x + 1" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "converts if-then-else to guard" $ do
    let original = T.unlines
          [ "module Test where"
          , "f x = if x > 0 then 1 else 0"
          ]
        newCode = T.unlines
          [ "f x"
          , "  | x > 0    = 1"
          , "  | otherwise = 0"
          ]
        fix = testFix "Use guards" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 3 1) newCode]
        transformed = applyFix original fix

    T.isInfixOf "| x > 0" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "simplifies boolean expression" $ do
    let original = T.unlines
          [ "module Test where"
          , "f x = if x == True then True else False"
          ]
        fix = testFix "Simplify bool" [FixEdit (mkSrcSpanRaw "Test.hs" 2 7 2 40) "x"]
        transformed = applyFix original fix

    T.isInfixOf "f x = x" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "replaces fromJust with pattern match" $ do
    let original = T.unlines
          [ "module Test where"
          , "import Data.Maybe"
          , "f mx = fromJust mx"
          ]
        fix = testFix "Use pattern match"
          [FixEdit (mkSrcSpanRaw "Test.hs" 3 8 3 19)
            "case mx of { Just x -> x; Nothing -> error \"Nothing\" }"]
        transformed = applyFix original fix

    T.isInfixOf "case mx of" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "eta-reduces lambda" $ do
    let original = T.unlines
          [ "module Test where"
          , "f = \\x -> succ x"
          ]
        fix = testFix "Eta reduce" [FixEdit (mkSrcSpanRaw "Test.hs" 2 5 2 17) "succ"]
        transformed = applyFix original fix

    T.isInfixOf "f = succ" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "replaces concat map with concatMap" $ do
    let original = T.unlines
          [ "module Test where"
          , "f xs = concat (map g xs)"
          ]
        fix = testFix "Use concatMap" [FixEdit (mkSrcSpanRaw "Test.hs" 2 8 2 25) "concatMap g xs"]
        transformed = applyFix original fix

    T.isInfixOf "concatMap g xs" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

--------------------------------------------------------------------------------
-- Edge Cases
--------------------------------------------------------------------------------

edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge Cases" $ do
  it "handles empty source" $ do
    let source = ""
        fix = testFix "Add content" [FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 1) "x = 1"]
        result = applyFix source fix
    T.isInfixOf "x = 1" result `shouldBe` True

  it "handles fix at column 0 (edge)" $ do
    -- Column indices are 1-based, so column 1 is the start
    let source = "x = 1"
        fix = testFix "Replace" [FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 2) "y"]
        result = applyFix source fix
    result `shouldBe` "y = 1"

  it "handles unicode in source" $ do
    let source = "msg = \"こんにちは\""
        fix = testFix "Change msg" [FixEdit (mkSrcSpanRaw "test.hs" 1 7 1 14) "\"Hello\""]
        result = applyFix source fix
    T.isInfixOf "Hello" result `shouldBe` True

  it "handles unicode in replacement" $ do
    let source = "msg = \"Hello\""
        fix = testFix "Translate" [FixEdit (mkSrcSpanRaw "test.hs" 1 7 1 14) "\"Привет\""]
        result = applyFix source fix
    T.isInfixOf "Привет" result `shouldBe` True

  it "handles tabs in source" $ do
    let source = "f\t=\t1"
        fix = testFix "Change" [FixEdit (mkSrcSpanRaw "test.hs" 1 4 1 5) "2"]
        result = applyFix source fix
    T.isInfixOf "2" result `shouldBe` True
    T.isInfixOf "\t" result `shouldBe` True

  it "handles Windows line endings (CRLF)" $ do
    let source = "x = 1\r\ny = 2\r\n"
        fix = testFix "Change x" [FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 6) "10"]
        result = applyFix source fix
    T.isInfixOf "10" result `shouldBe` True

  it "handles trailing newlines" $ do
    let source = "x = 1\n\n\n"
        fix = testFix "Change" [FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 6) "2"]
        result = applyFix source fix
    T.isInfixOf "x = 2" result `shouldBe` True

  it "handles very long lines" $ do
    let longValue = T.replicate 500 "x"
        source = "val = " <> longValue
        fix = testFix "Shorten" [FixEdit (mkSrcSpanRaw "test.hs" 1 7 1 507) "short"]
        result = applyFix source fix
    T.isInfixOf "val = short" result `shouldBe` True
    T.length result < T.length source `shouldBe` True

  it "handles deeply nested expressions" $ do
    let source = "f = ((((x))))"
        fix = testFix "Simplify" [FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 14) "x"]
        result = applyFix source fix
    result `shouldBe` "f = x"

  it "handles empty fix edits list" $ do
    let source = "x = 1"
        fix = testFix "Empty" []
        result = applyFix source fix
    result `shouldBe` source

  it "handles overlapping edits by applying in reverse order" $ do
    -- This tests the internal ordering mechanism
    let source = "abc"
        edits = [ FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 2) "X"  -- a -> X
                , FixEdit (mkSrcSpanRaw "test.hs" 1 3 1 4) "Z"  -- c -> Z
                ]
        result = applyEdits source edits
    result `shouldBe` "XbZ"

--------------------------------------------------------------------------------
-- Complex Refactoring Scenarios
--------------------------------------------------------------------------------

complexRefactoringSpec :: Spec
complexRefactoringSpec = describe "Complex Refactorings" $ do
  it "applies multiple related fixes" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
    let testFile = tmpDir </> "Test.hs"
        code = T.unlines
          [ "module Test where"
          , "foo = 1"
          , "bar = foo + 1"
          , "baz = bar + 1"
          ]
    TIO.writeFile testFile code

    -- Chain of related fixes
    let fix1 = mkPreferredFix "Change foo" testFile 2 7 2 8 "10"
        diag = mkDiagnosticWithFix (T.pack testFile) 2 7 "Update" fix1
        opts = defaultSafeOptions { sroDryRun = False }

    result <- safeApplyFixes opts [(testFile, [diag])]
    srrSuccess result `shouldBe` True

    finalContent <- TIO.readFile testFile
    T.isInfixOf "foo = 10" finalContent `shouldBe` True
    -- Verify file still compiles
    syntaxResult <- validateSyntax testFile finalContent
    syntaxResult `shouldBe` Right ()

  it "handles fix that adds new declarations" $ do
    let original = T.unlines
          [ "module Test where"
          , "f = undefined"
          ]
        newDecl = T.unlines
          [ "helper :: Int -> Int"
          , "helper x = x + 1"
          , ""
          , "f = helper 0"
          ]
        fix = testFix "Implement f" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 3 1) newDecl]
        transformed = applyFix original fix

    T.isInfixOf "helper :: Int -> Int" transformed `shouldBe` True
    T.isInfixOf "f = helper 0" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "handles fix that removes declarations" $ do
    let original = T.unlines
          [ "module Test where"
          , "unused = 1"
          , "used = 2"
          ]
        -- Remove unused declaration
        fix = testFix "Remove unused" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 3 1) ""]
        transformed = applyFix original fix

    T.isInfixOf "unused" transformed `shouldBe` False
    T.isInfixOf "used = 2" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "applies fix to record syntax" $ do
    let original = T.unlines
          [ "module Test where"
          , "data Person = Person { name :: String, age :: Int }"
          , "getName p = name p"
          ]
        fix = testFix "Use record accessor" [FixEdit (mkSrcSpanRaw "Test.hs" 3 13 3 19) "name"]
        transformed = applyFix original fix

    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "handles qualified name fixes" $ do
    let original = T.unlines
          [ "module Test where"
          , "import qualified Data.Map as M"
          , "f = M.empty"
          ]
        fix = testFix "Use unqualified" [FixEdit (mkSrcSpanRaw "Test.hs" 3 5 3 12) "mempty"]
        transformed = applyFix original fix

    T.isInfixOf "f = mempty" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

--------------------------------------------------------------------------------
-- Variable Renaming Tests
--------------------------------------------------------------------------------

variableRenamingSpec :: Spec
variableRenamingSpec = describe "Variable Renaming" $ do
  it "renames single variable occurrence" $ do
    let original = "module Test where\nfoo = 1"
        fix = testFix "Rename foo to bar" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 2 4) "bar"]
        transformed = applyFix original fix

    T.isInfixOf "bar = 1" transformed `shouldBe` True
    T.isInfixOf "foo" transformed `shouldBe` False
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "renames multiple occurrences of same variable" $ do
    -- Each occurrence on separate lines to avoid column position issues
    let original = T.unlines
          [ "module Test where"
          , "foo = 1"
          , "bar = foo"
          , "baz = foo"
          ]
        -- Multiple edits to rename all occurrences (each on separate line)
        edits =
          [ FixEdit (mkSrcSpanRaw "Test.hs" 2 1 2 4) "renamed"   -- definition
          , FixEdit (mkSrcSpanRaw "Test.hs" 3 7 3 10) "renamed"  -- use in bar
          , FixEdit (mkSrcSpanRaw "Test.hs" 4 7 4 10) "renamed"  -- use in baz
          ]
        fix = testFix "Rename foo to renamed" edits
        transformed = applyFix original fix

    T.isInfixOf "renamed = 1" transformed `shouldBe` True
    T.isInfixOf "bar = renamed" transformed `shouldBe` True
    T.isInfixOf "baz = renamed" transformed `shouldBe` True
    T.isInfixOf "foo" transformed `shouldBe` False
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "renames function and its type signature" $ do
    let original = T.unlines
          [ "module Test where"
          , "oldFunc :: Int -> Int"
          , "oldFunc x = x + 1"
          ]
        edits =
          [ FixEdit (mkSrcSpanRaw "Test.hs" 2 1 2 8) "newFunc"
          , FixEdit (mkSrcSpanRaw "Test.hs" 3 1 3 8) "newFunc"
          ]
        fix = testFix "Rename oldFunc to newFunc" edits
        transformed = applyFix original fix

    T.isInfixOf "newFunc :: Int -> Int" transformed `shouldBe` True
    T.isInfixOf "newFunc x = x + 1" transformed `shouldBe` True
    T.isInfixOf "oldFunc" transformed `shouldBe` False
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "renames local variable in let binding" $ do
    let original = T.unlines
          [ "module Test where"
          , "f = let x = 1 in x + x"
          ]
        edits =
          [ FixEdit (mkSrcSpanRaw "Test.hs" 2 9 2 10) "y"   -- binding
          , FixEdit (mkSrcSpanRaw "Test.hs" 2 18 2 19) "y"  -- first use
          , FixEdit (mkSrcSpanRaw "Test.hs" 2 22 2 23) "y"  -- second use
          ]
        fix = testFix "Rename x to y" edits
        transformed = applyFix original fix

    T.isInfixOf "let y = 1 in y + y" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "renames parameter in lambda" $ do
    let original = T.unlines
          [ "module Test where"
          , "f = \\oldParam -> oldParam + 1"
          ]
        edits =
          [ FixEdit (mkSrcSpanRaw "Test.hs" 2 6 2 14) "newParam"
          , FixEdit (mkSrcSpanRaw "Test.hs" 2 18 2 26) "newParam"
          ]
        fix = testFix "Rename oldParam to newParam" edits
        transformed = applyFix original fix

    T.isInfixOf "\\newParam -> newParam + 1" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "renames type variable" $ do
    let original = T.unlines
          [ "module Test where"
          , "identity :: a -> a"
          , "identity x = x"
          ]
        edits =
          [ FixEdit (mkSrcSpanRaw "Test.hs" 2 13 2 14) "t"
          , FixEdit (mkSrcSpanRaw "Test.hs" 2 18 2 19) "t"
          ]
        fix = testFix "Rename type var a to t" edits
        transformed = applyFix original fix

    T.isInfixOf "identity :: t -> t" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "renames pattern variable in case" $ do
    -- Each edit on a separate line to avoid column position interference
    let original = T.unlines
          [ "module Test where"
          , "f mx = case mx of"
          , "  Just x ->"
          , "    x + 1"
          , "  Nothing -> 0"
          ]
        edits =
          [ FixEdit (mkSrcSpanRaw "Test.hs" 3 8 3 9) "val"   -- pattern on line 3
          , FixEdit (mkSrcSpanRaw "Test.hs" 4 5 4 6) "val"   -- use on line 4
          ]
        fix = testFix "Rename x to val" edits
        transformed = applyFix original fix

    T.isInfixOf "Just val ->" transformed `shouldBe` True
    T.isInfixOf "val + 1" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "renames record field" $ do
    let original = T.unlines
          [ "module Test where"
          , "data Person = Person { oldName :: String }"
          ]
        fix = testFix "Rename field" [FixEdit (mkSrcSpanRaw "Test.hs" 2 24 2 31) "newName"]
        transformed = applyFix original fix

    T.isInfixOf "newName :: String" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "renames module-level constant across uses" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
    let testFile = tmpDir </> "Test.hs"
        code = T.unlines
          [ "module Test where"
          , "magicNumber = 42"
          , "f x = x + magicNumber"
          , "g = magicNumber * 2"
          ]
    TIO.writeFile testFile code

    let fix1 = mkPreferredFix "Rename def" testFile 2 1 2 12 "theAnswer"
        fix2 = mkPreferredFix "Rename use1" testFile 3 11 3 22 "theAnswer"
        fix3 = mkPreferredFix "Rename use2" testFile 4 5 4 16 "theAnswer"
        diags =
          [ mkDiagnosticWithFix (T.pack testFile) 2 1 "Rename" fix1
          , mkDiagnosticWithFix (T.pack testFile) 3 11 "Rename" fix2
          , mkDiagnosticWithFix (T.pack testFile) 4 5 "Rename" fix3
          ]
        opts = defaultSafeOptions { sroDryRun = False }

    result <- safeApplyFixes opts [(testFile, diags)]
    srrSuccess result `shouldBe` True

    finalContent <- TIO.readFile testFile
    T.isInfixOf "theAnswer = 42" finalContent `shouldBe` True
    T.isInfixOf "x + theAnswer" finalContent `shouldBe` True
    T.isInfixOf "theAnswer * 2" finalContent `shouldBe` True
    syntaxResult <- validateSyntax testFile finalContent
    syntaxResult `shouldBe` Right ()

--------------------------------------------------------------------------------
-- Import Manipulation Tests
--------------------------------------------------------------------------------

importManipulationSpec :: Spec
importManipulationSpec = describe "Import Manipulation" $ do
  it "adds simple import" $ do
    let original = T.unlines
          [ "module Test where"
          , ""
          , "f = sort [3, 1, 2]"
          ]
        fix = testFix "Add import" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 2 1) "import Data.List (sort)\n"]
        transformed = applyFix original fix

    T.isInfixOf "import Data.List (sort)" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "adds qualified import" $ do
    let original = T.unlines
          [ "module Test where"
          , ""
          , "f = M.empty"
          ]
        fix = testFix "Add qualified import" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 2 1) "import qualified Data.Map as M\n"]
        transformed = applyFix original fix

    T.isInfixOf "import qualified Data.Map as M" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "removes unused import" $ do
    let original = T.unlines
          [ "module Test where"
          , "import Data.List"
          , "import Data.Maybe"  -- unused
          , ""
          , "f = sort [1, 2, 3]"
          ]
        -- Remove the entire import line
        fix = testFix "Remove unused import" [FixEdit (mkSrcSpanRaw "Test.hs" 3 1 4 1) ""]
        transformed = applyFix original fix

    T.isInfixOf "import Data.Maybe" transformed `shouldBe` False
    T.isInfixOf "import Data.List" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "adds specific import symbol" $ do
    let original = T.unlines
          [ "module Test where"
          , "import Data.List"
          , ""
          , "f = foldl' (+) 0 [1, 2, 3]"
          ]
        -- Change import to include specific symbol
        fix = testFix "Add foldl' to import" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 3 1) "import Data.List (foldl', sort)\n"]
        transformed = applyFix original fix

    T.isInfixOf "import Data.List (foldl', sort)" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "converts import to qualified" $ do
    let original = T.unlines
          [ "module Test where"
          , "import Data.Map"
          , ""
          , "f = empty"
          ]
        edits =
          [ FixEdit (mkSrcSpanRaw "Test.hs" 2 1 3 1) "import qualified Data.Map as M\n"
          , FixEdit (mkSrcSpanRaw "Test.hs" 4 5 4 10) "M.empty"
          ]
        fix = testFix "Qualify import" edits
        transformed = applyFix original fix

    T.isInfixOf "import qualified Data.Map as M" transformed `shouldBe` True
    T.isInfixOf "f = M.empty" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "adds hiding clause to import" $ do
    let original = T.unlines
          [ "module Test where"
          , "import Prelude"
          , ""
          , "head = \"custom head\""
          ]
        fix = testFix "Add hiding" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 3 1) "import Prelude hiding (head)\n"]
        transformed = applyFix original fix

    T.isInfixOf "import Prelude hiding (head)" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "replaces import with explicit list" $ do
    let original = T.unlines
          [ "module Test where"
          , "import Data.List"
          , ""
          , "f xs = sort xs"
          ]
        fix = testFix "Explicit import" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 3 1) "import Data.List (sort)\n"]
        transformed = applyFix original fix

    T.isInfixOf "import Data.List (sort)" transformed `shouldBe` True
    T.isInfixOf "import Data.List\n" transformed `shouldBe` False
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "merges duplicate imports" $ do
    let original = T.unlines
          [ "module Test where"
          , "import Data.List (sort)"
          , "import Data.List (nub)"
          , ""
          , "f = sort . nub"
          ]
        -- Replace both imports with merged one
        fix = testFix "Merge imports" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 4 1) "import Data.List (nub, sort)\n"]
        transformed = applyFix original fix

    T.isInfixOf "import Data.List (nub, sort)" transformed `shouldBe` True
    -- Verify only one Data.List import
    T.count "import Data.List" transformed `shouldBe` 1
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "adds import for operator" $ do
    let original = T.unlines
          [ "module Test where"
          , ""
          , "f = [1] <> [2]"
          ]
        fix = testFix "Add Semigroup import" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 2 1) "import Data.Semigroup ((<>))\n"]
        transformed = applyFix original fix

    T.isInfixOf "import Data.Semigroup ((<>))" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

--------------------------------------------------------------------------------
-- Qualified Name Conversion Tests
--------------------------------------------------------------------------------

qualifiedNameSpec :: Spec
qualifiedNameSpec = describe "Qualified Name Conversion" $ do
  it "qualifies unqualified name" $ do
    let original = T.unlines
          [ "module Test where"
          , "import qualified Data.Map as M"
          , ""
          , "f = empty"  -- Should be M.empty
          ]
        fix = testFix "Qualify name" [FixEdit (mkSrcSpanRaw "Test.hs" 4 5 4 10) "M.empty"]
        transformed = applyFix original fix

    T.isInfixOf "f = M.empty" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "unqualifies qualified name" $ do
    let original = T.unlines
          [ "module Test where"
          , "import Data.Map (empty)"
          , ""
          , "f = Data.Map.empty"
          ]
        fix = testFix "Unqualify name" [FixEdit (mkSrcSpanRaw "Test.hs" 4 5 4 19) "empty"]
        transformed = applyFix original fix

    T.isInfixOf "f = empty" transformed `shouldBe` True
    T.isInfixOf "Data.Map.empty" transformed `shouldBe` False
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "changes qualifier" $ do
    let original = T.unlines
          [ "module Test where"
          , "import qualified Data.Map.Strict as MS"
          , ""
          , "f = M.empty"
          ]
        -- Fix the qualifier from M to MS
        fix = testFix "Fix qualifier" [FixEdit (mkSrcSpanRaw "Test.hs" 4 5 4 12) "MS.empty"]
        transformed = applyFix original fix

    T.isInfixOf "f = MS.empty" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "qualifies multiple occurrences" $ do
    -- Each edit on a separate line to avoid column position interference
    let original = T.unlines
          [ "module Test where"
          , "import qualified Data.Text as T"
          , ""
          , "f = pack \"hello\""
          , "g = unpack s"
          , "  where s = pack \"world\""
          ]
        edits =
          [ FixEdit (mkSrcSpanRaw "Test.hs" 4 5 4 9) "T.pack"     -- line 4
          , FixEdit (mkSrcSpanRaw "Test.hs" 5 5 5 11) "T.unpack"  -- line 5
          , FixEdit (mkSrcSpanRaw "Test.hs" 6 13 6 17) "T.pack"   -- line 6
          ]
        fix = testFix "Qualify all" edits
        transformed = applyFix original fix

    T.isInfixOf "f = T.pack" transformed `shouldBe` True
    T.isInfixOf "g = T.unpack" transformed `shouldBe` True
    T.isInfixOf "s = T.pack" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "converts to full module qualification" $ do
    let original = T.unlines
          [ "module Test where"
          , "import Data.Text (pack)"
          , ""
          , "f = pack \"test\""
          ]
        edits =
          [ FixEdit (mkSrcSpanRaw "Test.hs" 2 1 3 1) "import qualified Data.Text\n"
          , FixEdit (mkSrcSpanRaw "Test.hs" 4 5 4 9) "Data.Text.pack"
          ]
        fix = testFix "Full qualify" edits
        transformed = applyFix original fix

    T.isInfixOf "import qualified Data.Text\n" transformed `shouldBe` True
    T.isInfixOf "f = Data.Text.pack" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "qualifies operator" $ do
    let original = T.unlines
          [ "module Test where"
          , "import qualified Data.Map as M"
          , ""
          , "f m = m ! \"key\""  -- Should be M.!
          ]
        fix = testFix "Qualify operator" [FixEdit (mkSrcSpanRaw "Test.hs" 4 9 4 10) "(M.!)"]
        -- Note: Operator needs special handling
        transformed = applyFix original fix

    T.isInfixOf "(M.!)" transformed `shouldBe` True

  it "converts alias to different alias" $ do
    let original = T.unlines
          [ "module Test where"
          , "import qualified Data.ByteString as B"
          , ""
          , "f = B.empty"
          ]
        edits =
          [ FixEdit (mkSrcSpanRaw "Test.hs" 2 1 3 1) "import qualified Data.ByteString as BS\n"
          , FixEdit (mkSrcSpanRaw "Test.hs" 4 5 4 12) "BS.empty"
          ]
        fix = testFix "Change alias" edits
        transformed = applyFix original fix

    T.isInfixOf "import qualified Data.ByteString as BS" transformed `shouldBe` True
    T.isInfixOf "f = BS.empty" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

--------------------------------------------------------------------------------
-- Type Signature Fix Tests
--------------------------------------------------------------------------------

typeSignatureSpec :: Spec
typeSignatureSpec = describe "Type Signature Fixes" $ do
  it "adds missing type signature" $ do
    let original = T.unlines
          [ "module Test where"
          , "f x = x + 1"
          ]
        fix = testFix "Add signature" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 2 1) "f :: Int -> Int\n"]
        transformed = applyFix original fix

    T.isInfixOf "f :: Int -> Int" transformed `shouldBe` True
    T.isInfixOf "f x = x + 1" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "generalizes type signature" $ do
    let original = T.unlines
          [ "module Test where"
          , "f :: Int -> Int"
          , "f x = x"
          ]
        fix = testFix "Generalize" [FixEdit (mkSrcSpanRaw "Test.hs" 2 6 2 16) "a -> a"]
        transformed = applyFix original fix

    T.isInfixOf "f :: a -> a" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "adds constraint to type signature" $ do
    let original = T.unlines
          [ "module Test where"
          , "f :: a -> a -> a"
          , "f x y = x + y"
          ]
        fix = testFix "Add Num constraint" [FixEdit (mkSrcSpanRaw "Test.hs" 2 6 2 17) "Num a => a -> a -> a"]
        transformed = applyFix original fix

    T.isInfixOf "f :: Num a => a -> a -> a" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "specializes polymorphic type" $ do
    let original = T.unlines
          [ "module Test where"
          , "f :: Foldable t => t a -> Int"
          , "f = length"
          ]
        fix = testFix "Specialize to list" [FixEdit (mkSrcSpanRaw "Test.hs" 2 6 2 30) "[a] -> Int"]
        transformed = applyFix original fix

    T.isInfixOf "f :: [a] -> Int" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "fixes incorrect return type" $ do
    let original = T.unlines
          [ "module Test where"
          , "f :: Int -> Bool"
          , "f x = x + 1"  -- Actually returns Int
          ]
        fix = testFix "Fix return type" [FixEdit (mkSrcSpanRaw "Test.hs" 2 13 2 17) "Int"]
        transformed = applyFix original fix

    T.isInfixOf "f :: Int -> Int" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

--------------------------------------------------------------------------------
-- Pragma and Extension Fixes
--------------------------------------------------------------------------------

pragmaSpec :: Spec
pragmaSpec = describe "Pragma and Extension Fixes" $ do
  it "adds language extension pragma" $ do
    let original = T.unlines
          [ "module Test where"
          , "f !x = x"  -- Needs BangPatterns
          ]
        fix = testFix "Add pragma" [FixEdit (mkSrcSpanRaw "Test.hs" 1 1 1 1) "{-# LANGUAGE BangPatterns #-}\n"]
        transformed = applyFix original fix

    T.isInfixOf "{-# LANGUAGE BangPatterns #-}" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "adds multiple language extensions" $ do
    let original = T.unlines
          [ "module Test where"
          , "f = undefined"
          ]
        pragma = "{-# LANGUAGE OverloadedStrings #-}\n{-# LANGUAGE TypeApplications #-}\n"
        fix = testFix "Add pragmas" [FixEdit (mkSrcSpanRaw "Test.hs" 1 1 1 1) pragma]
        transformed = applyFix original fix

    T.isInfixOf "OverloadedStrings" transformed `shouldBe` True
    T.isInfixOf "TypeApplications" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "removes unnecessary extension" $ do
    let original = T.unlines
          [ "{-# LANGUAGE OverloadedStrings #-}"
          , "{-# LANGUAGE TypeFamilies #-}"  -- Not needed
          , "module Test where"
          , "f :: String"
          , "f = \"test\""
          ]
        fix = testFix "Remove TypeFamilies" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 3 1) ""]
        transformed = applyFix original fix

    T.isInfixOf "TypeFamilies" transformed `shouldBe` False
    T.isInfixOf "OverloadedStrings" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "adds INLINE pragma" $ do
    let original = T.unlines
          [ "module Test where"
          , "f :: Int -> Int"
          , "f x = x + 1"
          ]
        fix = testFix "Add INLINE" [FixEdit (mkSrcSpanRaw "Test.hs" 3 1 3 1) "{-# INLINE f #-}\n"]
        transformed = applyFix original fix

    T.isInfixOf "{-# INLINE f #-}" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

--------------------------------------------------------------------------------
-- Pattern Matching Fixes
--------------------------------------------------------------------------------

patternMatchSpec :: Spec
patternMatchSpec = describe "Pattern Matching Fixes" $ do
  it "adds missing pattern" $ do
    let original = T.unlines
          [ "module Test where"
          , "f (Just x) = x"
          ]
        newCode = T.unlines
          [ "f (Just x) = x"
          , "f Nothing = error \"Nothing\""
          ]
        fix = testFix "Add Nothing case" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 3 1) newCode]
        transformed = applyFix original fix

    T.isInfixOf "f Nothing = error" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "converts partial function to total with Maybe" $ do
    let original = T.unlines
          [ "module Test where"
          , "safeHead :: [a] -> a"
          , "safeHead (x:_) = x"
          ]
        newCode = T.unlines
          [ "safeHead :: [a] -> Maybe a"
          , "safeHead (x:_) = Just x"
          , "safeHead [] = Nothing"
          ]
        fix = testFix "Make total" [FixEdit (mkSrcSpanRaw "Test.hs" 2 1 4 1) newCode]
        transformed = applyFix original fix

    T.isInfixOf "Maybe a" transformed `shouldBe` True
    T.isInfixOf "Just x" transformed `shouldBe` True
    T.isInfixOf "safeHead [] = Nothing" transformed `shouldBe` True
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "simplifies nested pattern" $ do
    let original = T.unlines
          [ "module Test where"
          , "f (Just (Just x)) = x"
          , "f _ = 0"
          ]
        fix = testFix "Use pattern synonym" [FixEdit (mkSrcSpanRaw "Test.hs" 2 3 2 18) "Just x"]
        -- Note: This would need proper nested Just handling
        transformed = applyFix original fix

    T.isInfixOf "f Just x = x" transformed `shouldBe` True

  it "adds as-pattern" $ do
    let original = T.unlines
          [ "module Test where"
          , "f (x:xs) = (x:xs) ++ [x]"
          ]
        -- Using as-pattern to avoid reconstructing
        fix = testFix "Use as-pattern" [FixEdit (mkSrcSpanRaw "Test.hs" 2 3 2 8) "list@(x:xs)"]
        transformed = applyFix original fix

    T.isInfixOf "f list@(x:xs)" transformed `shouldBe` True

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Create a simple fix for tests
testFix :: Text -> [FixEdit] -> Fix
testFix title edits = Fix
  { fixTitle = title
  , fixEdits = edits
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSAlways
  }

-- | Create a preferred fix with specific span
mkPreferredFix :: Text -> FilePath -> Int -> Int -> Int -> Int -> Text -> Fix
mkPreferredFix title file sl sc el ec replacement = Fix
  { fixTitle = title
  , fixEdits = [FixEdit (mkSrcSpanRaw file sl sc el ec) replacement]
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSMostly
  }

-- | Create a diagnostic with a fix
mkDiagnosticWithFix :: Text -> Int -> Int -> Text -> Fix -> Diagnostic
mkDiagnosticWithFix file line col msg fix = Diagnostic
  { diagSpan = mkSrcSpanRaw (T.unpack file) line col line (col + 10)
  , diagSeverity = Warning
  , diagKind = CodePattern
  , diagMessage = msg
  , diagCode = Nothing
  , diagFixes = [fix]
  , diagRelated = []
  }
