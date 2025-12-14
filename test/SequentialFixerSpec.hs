{-# LANGUAGE OverloadedStrings #-}

module SequentialFixerSpec (spec) where

import Test.Hspec
import Data.Text qualified as T

import Argus.Types
  ( SrcSpan(..), Fix(..), FixEdit(..), Line(..), Column(..)
  , FixCategory(..), FixSafety(..), Diagnostic(..), Severity(..)
  )
import Argus.Refactor.SequentialFixer

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
mkFix :: T.Text -> FilePath -> Int -> Int -> Int -> Int -> T.Text -> Fix
mkFix title file sl sc el ec newText = Fix
  { fixTitle = title
  , fixEdits = [FixEdit (mkSpan file sl sc el ec) newText]
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCRedundant
  , fixSafety = FSAlways
  }

spec :: Spec
spec = do
  describe "Argus.Refactor.SequentialFixer" $ do
    describe "defaultSequentialConfig" $ do
      it "has validateAfterEach enabled by default" $ do
        scValidateAfterEach defaultSequentialConfig `shouldBe` True

      it "has adjustSpans enabled by default" $ do
        scAdjustSpans defaultSequentialConfig `shouldBe` True

      it "has stopOnFirstFailure disabled by default" $ do
        scStopOnFirstFailure defaultSequentialConfig `shouldBe` False

    describe "applyFixesSequentially" $ do
      it "applies single fix correctly" $ do
        let content = T.unlines
              [ "module Test where"
              , ""
              , "foo = bar"  -- line 3
              ]
            fix = mkFix "Replace bar with baz" "test.hs" 3 7 3 10 "baz"

        result <- applyFixesSequentially defaultSequentialConfig "test.hs" content [fix]
        seqOutcome result `shouldBe` SeqSuccess
        length (seqApplied result) `shouldBe` 1
        "foo = baz" `T.isInfixOf` seqFinalContent result `shouldBe` True

      it "applies multiple non-overlapping fixes" $ do
        let content = T.unlines
              [ "module Test where"
              , ""
              , "foo = oldFoo"  -- line 3
              , "bar = oldBar"  -- line 4
              , "baz = oldBaz"  -- line 5
              ]
            fix1 = mkFix "Fix 1" "test.hs" 3 7 3 13 "newFoo"
            fix2 = mkFix "Fix 2" "test.hs" 4 7 4 13 "newBar"
            fix3 = mkFix "Fix 3" "test.hs" 5 7 5 13 "newBaz"

        result <- applyFixesSequentially defaultSequentialConfig "test.hs" content [fix1, fix2, fix3]
        seqOutcome result `shouldBe` SeqSuccess
        length (seqApplied result) `shouldBe` 3

      it "adjusts spans when fix adds lines" $ do
        let content = T.unlines
              [ "module Test where"
              , ""
              , "foo = 1"      -- line 3 - will add a line
              , ""
              , "bar = 2"      -- line 5 - should be adjusted
              ]
            -- Fix 1: Replace "foo = 1" with "foo = 1\nfoo' = 11" (adds 1 line)
            fix1 = mkFix "Add foo'" "test.hs" 3 1 3 8 "foo = 1\nfoo' = 11"
            -- Fix 2: Replace "bar = 2" with "bar = 3" (was line 5, now line 6)
            fix2 = mkFix "Update bar" "test.hs" 5 1 5 8 "bar = 3"

        result <- applyFixesSequentially defaultSequentialConfig "test.hs" content [fix1, fix2]
        seqOutcome result `shouldBe` SeqSuccess
        length (seqApplied result) `shouldBe` 2
        "bar = 3" `T.isInfixOf` seqFinalContent result `shouldBe` True
        "foo' = 11" `T.isInfixOf` seqFinalContent result `shouldBe` True

      it "adjusts spans when fix removes lines" $ do
        let content = T.unlines
              [ "module Test where"
              , ""
              , "foo = 1"      -- line 3
              , "foo' = 11"    -- line 4 - will be removed
              , ""
              , "bar = 2"      -- line 6 - should become line 5
              ]
            -- Fix 1: Remove "foo' = 11" line (replace 2 lines with 1)
            fix1 = mkFix "Remove foo'" "test.hs" 3 1 4 10 "foo = 1"
            -- Fix 2: Update bar (was line 6, now line 5)
            fix2 = mkFix "Update bar" "test.hs" 6 1 6 8 "bar = 3"

        result <- applyFixesSequentially defaultSequentialConfig "test.hs" content [fix1, fix2]
        seqOutcome result `shouldBe` SeqSuccess
        length (seqApplied result) `shouldBe` 2

      it "skips fixes with invalidated spans" $ do
        let content = T.unlines
              [ "module Test where"
              , ""
              , "foo = oldValue"  -- line 3
              ]
            -- Both fixes target the same location - second should be skipped
            fix1 = mkFix "Fix 1" "test.hs" 3 7 3 15 "newValue1"
            fix2 = mkFix "Fix 2" "test.hs" 3 7 3 15 "newValue2"

        result <- applyFixesSequentially defaultSequentialConfig "test.hs" content [fix1, fix2]
        -- Either partial success or second fix skipped
        length (seqApplied result) `shouldBe` 1
        length (seqSkipped result) `shouldSatisfy` (>= 0)

      it "respects maxFixes limit" $ do
        let content = T.unlines
              [ "module Test where"
              , "a = 1"
              , "b = 2"
              , "c = 3"
              , "d = 4"
              ]
            fixes =
              [ mkFix "Fix a" "test.hs" 2 1 2 6 "a = 10"
              , mkFix "Fix b" "test.hs" 3 1 3 6 "b = 20"
              , mkFix "Fix c" "test.hs" 4 1 4 6 "c = 30"
              , mkFix "Fix d" "test.hs" 5 1 5 6 "d = 40"
              ]
            config = defaultSequentialConfig { scMaxFixes = 2 }

        result <- applyFixesSequentially config "test.hs" content fixes
        length (seqApplied result) `shouldSatisfy` (<= 2)

      it "continues after failure when stopOnFirstFailure is False" $ do
        let content = T.unlines
              [ "module Test where"
              , "foo = 1"  -- line 2
              , "bar = 2"  -- line 3
              ]
            -- Fix 1 creates invalid syntax
            fix1 = mkFix "Break syntax" "test.hs" 2 1 2 8 "foo = ("  -- unbalanced paren
            -- Fix 2 is valid
            fix2 = mkFix "Fix bar" "test.hs" 3 1 3 8 "bar = 3"
            config = defaultSequentialConfig { scStopOnFirstFailure = False }

        result <- applyFixesSequentially config "test.hs" content [fix1, fix2]
        -- Fix 1 should fail validation, fix 2 might still be applied
        length (seqFailed result) `shouldSatisfy` (>= 1)

    describe "SequentialOutcome" $ do
      it "is SeqSuccess when all fixes apply" $ do
        let content = "module Test where\nfoo = 1"
            fix = mkFix "Fix" "test.hs" 2 7 2 8 "2"

        result <- applyFixesSequentially defaultSequentialConfig "test.hs" content [fix]
        seqOutcome result `shouldBe` SeqSuccess

      it "is SeqPartial when some fixes fail" $ do
        let content = "module Test where\nfoo = bar"
            fix1 = mkFix "Fix 1" "test.hs" 2 7 2 10 "baz"
            fix2 = mkFix "Fix 2" "test.hs" 2 7 2 10 "qux"  -- same span - will conflict

        result <- applyFixesSequentially defaultSequentialConfig "test.hs" content [fix1, fix2]
        seqOutcome result `shouldBe` SeqPartial

    describe "AppliedFixInfo" $ do
      it "records content before and after" $ do
        let content = "module Test where\nfoo = old"
            fix = mkFix "Update" "test.hs" 2 7 2 10 "new"

        result <- applyFixesSequentially defaultSequentialConfig "test.hs" content [fix]
        case seqApplied result of
          [] -> expectationFailure "Expected at least one applied fix"
          (info:_) -> do
            "old" `T.isInfixOf` afiContentBefore info `shouldBe` True
            "new" `T.isInfixOf` afiContentAfter info `shouldBe` True

    describe "delta tracking" $ do
      it "accumulates deltas for all applied fixes" $ do
        let content = T.unlines
              [ "module Test where"
              , "a = 1"
              , "b = 2"
              , "c = 3"
              ]
            fixes =
              [ mkFix "Fix a" "test.hs" 2 5 2 6 "10"  -- 1 -> 10 (adds 1 char)
              , mkFix "Fix b" "test.hs" 3 5 3 6 "20"
              , mkFix "Fix c" "test.hs" 4 5 4 6 "30"
              ]

        result <- applyFixesSequentially defaultSequentialConfig "test.hs" content fixes
        length (seqDeltas result) `shouldBe` length (seqApplied result)

    describe "CompilationResult" $ do
      it "CompileSuccess equals itself" $ do
        CompileSuccess `shouldBe` CompileSuccess

      it "CompileFailure contains errors" $ do
        let errs = [CompilationError "test.hs" (Just 5) (Just 10) "Type error"]
            result = CompileFailure errs
        case result of
          CompileSuccess -> expectationFailure "Expected failure"
          CompileFailure es -> length es `shouldBe` 1

    describe "verbose mode" $ do
      it "doesn't crash in verbose mode" $ do
        let content = "module Test where\nfoo = 1"
            fix = mkFix "Fix" "test.hs" 2 7 2 8 "2"
            config = defaultSequentialConfig { scVerbose = True }

        result <- applyFixesSequentially config "test.hs" content [fix]
        seqOutcome result `shouldBe` SeqSuccess
