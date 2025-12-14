{-# LANGUAGE OverloadedStrings #-}

module ExactPrintSpec (spec) where

import Test.Hspec
import Data.Functor.Identity (runIdentity)
import Data.Text (Text)
import Data.Text qualified as T
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Data.Text.IO qualified as TIO

import Argus.Types
import Argus.Refactor.ExactPrint

spec :: Spec
spec = do
  describe "Argus.Refactor.ExactPrint" $ do
    describe "TransformState" $ do
      it "tracks source content" $ do
        let state = TransformState "x = 1" False
        tsSource state `shouldBe` "x = 1"

      it "tracks modified status" $ do
        let state = TransformState "x = 1" True
        tsModified state `shouldBe` True

    describe "runTransform" $ do
      it "runs transformation and returns result" $ do
        let initial = TransformState "x = 1" False
            (result, final) = runTransform (pure 42) initial
        result `shouldBe` 42
        tsSource final `shouldBe` "x = 1"

      it "preserves state through transformation" $ do
        let initial = TransformState "original" False
            transform = do
              replaceExpr (mkSrcSpanRaw "test.hs" 1 1 1 8) "modified"
              pure ()
            (_, final) = runTransform transform initial
        tsModified final `shouldBe` True

    describe "replaceExpr" $ do
      it "replaces expression at span" $ do
        let initial = TransformState "x = 1" False
            (_, final) = runTransform
              (replaceExpr (mkSrcSpanRaw "test.hs" 1 5 1 6) "2")
              initial
        tsSource final `shouldBe` "x = 2"
        tsModified final `shouldBe` True

      it "handles multi-character replacement" $ do
        let initial = TransformState "x = foo" False
            (_, final) = runTransform
              (replaceExpr (mkSrcSpanRaw "test.hs" 1 5 1 8) "bar")
              initial
        tsSource final `shouldBe` "x = bar"

      it "handles replacement with different length" $ do
        let initial = TransformState "x = 1" False
            (_, final) = runTransform
              (replaceExpr (mkSrcSpanRaw "test.hs" 1 5 1 6) "longer text")
              initial
        T.isInfixOf "longer text" (tsSource final) `shouldBe` True

    describe "replaceType" $ do
      it "replaces type at span (same as replaceExpr)" $ do
        let initial = TransformState "x :: Int" False
            (_, final) = runTransform
              (replaceType (mkSrcSpanRaw "test.hs" 1 6 1 9) "String")
              initial
        T.isInfixOf "String" (tsSource final) `shouldBe` True

    describe "replaceName" $ do
      it "replaces name at span (same as replaceExpr)" $ do
        let initial = TransformState "foo = 1" False
            (_, final) = runTransform
              (replaceName (mkSrcSpanRaw "test.hs" 1 1 1 4) "bar")
              initial
        T.isPrefixOf "bar" (tsSource final) `shouldBe` True

    describe "insertImport" $ do
      it "inserts import after module declaration" $ do
        let initial = TransformState "module Test where\n\nx = 1" False
            (_, final) = runTransform
              (insertImport "import Data.List")
              initial
        T.isInfixOf "import Data.List" (tsSource final) `shouldBe` True

      it "inserts import after existing imports" $ do
        let initial = TransformState "module Test where\nimport Data.Maybe\n\nx = 1" False
            (_, final) = runTransform
              (insertImport "import Data.List")
              initial
        T.isInfixOf "import Data.List" (tsSource final) `shouldBe` True

      it "handles file without module declaration" $ do
        let initial = TransformState "x = 1" False
            (_, final) = runTransform
              (insertImport "import Data.List")
              initial
        T.isInfixOf "import Data.List" (tsSource final) `shouldBe` True

    describe "removeDecl" $ do
      it "removes declaration at span" $ do
        let initial = TransformState "x = 1\ny = 2" False
            (_, final) = runTransform
              (removeDecl (mkSrcSpanRaw "test.hs" 1 1 1 6))
              initial
        T.isInfixOf "y = 2" (tsSource final) `shouldBe` True

    describe "applyEdits" $ do
      it "applies empty edit list" $ do
        let source = "x = 1"
            result = applyEdits source []
        result `shouldBe` source

      it "applies single edit" $ do
        let source = "x = 1"
            edit = FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 6) "2"
            result = applyEdits source [edit]
        result `shouldBe` "x = 2"

      it "applies multiple non-overlapping edits" $ do
        let source = "x = 1\ny = 2"
            edits =
              [ FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 6) "10"
              , FixEdit (mkSrcSpanRaw "test.hs" 2 5 2 6) "20"
              ]
            result = applyEdits source edits
        T.isInfixOf "10" result `shouldBe` True
        T.isInfixOf "20" result `shouldBe` True

      it "applies edits in reverse order (end to start)" $ do
        -- This ensures later edits don't affect positions of earlier ones
        let source = "a b c"
            edits =
              [ FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 2) "X"
              , FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 6) "Z"
              ]
            result = applyEdits source edits
        T.isInfixOf "X" result `shouldBe` True
        T.isInfixOf "Z" result `shouldBe` True

    describe "applyFix" $ do
      it "applies fix with single edit" $ do
        let source = "x = 1"
            fix = Fix
              { fixTitle = "Test fix"
              , fixEdits = [FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 6) "2"]
              , fixIsPreferred = True
              , fixAddImports = []
              , fixRemoveImports = []
              , fixCategory = FCStyle
              , fixSafety = FSAlways
              }
            result = applyFix source fix
        result `shouldBe` "x = 2"

      it "applies fix with multiple edits" $ do
        let source = "x = 1\ny = 2"
            fix = Fix
              { fixTitle = "Multi-edit fix"
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
            result = applyFix source fix
        T.isInfixOf "10" result `shouldBe` True
        T.isInfixOf "20" result `shouldBe` True

      it "handles fix with empty edits" $ do
        let source = "x = 1"
            fix = Fix
              { fixTitle = "Empty fix"
              , fixEdits = []
              , fixIsPreferred = True
              , fixAddImports = []
              , fixRemoveImports = []
              , fixCategory = FCStyle
              , fixSafety = FSAlways
              }
            result = applyFix source fix
        result `shouldBe` source

    describe "applyFixes" $ do
      it "applies empty fix list" $ do
        let source = "x = 1"
            result = applyFixes source []
        result `shouldBe` source

      it "applies single fix" $ do
        let source = "x = 1"
            fix = Fix "Fix" [FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 6) "2"] True [] [] FCStyle FSAlways
            result = applyFixes source [fix]
        result `shouldBe` "x = 2"

      it "applies multiple fixes sequentially" $ do
        let source = "x = 1"
            fix1 = Fix "Fix 1" [FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 6) "2"] True [] [] FCStyle FSAlways
            fix2 = Fix "Fix 2" [FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 6) "3"] True [] [] FCStyle FSAlways
            result = applyFixes source [fix1, fix2]
        -- Second fix replaces the result of first fix
        result `shouldBe` "x = 3"

    describe "parseAndTransform" $ do
      it "reads and transforms file" $ withSystemTempDirectory "argus-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.hs"
        TIO.writeFile testFile "x = 1"

        result <- parseAndTransform testFile $ \state ->
          pure state { tsSource = "y = 2", tsModified = True }

        case result of
          Right content -> content `shouldBe` "y = 2"
          Left err -> expectationFailure $ "Expected success: " <> T.unpack err

    describe "printExactly" $ do
      it "returns source from transform state" $ do
        let state = TransformState "x = 1" True
        printExactly state `shouldBe` "x = 1"

    describe "edge cases" $ do
      it "handles empty source" $ do
        let source = ""
            edit = FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 1) "x = 1"
            result = applyEdits source [edit]
        T.isInfixOf "x = 1" result `shouldBe` True

      it "handles multiline edits" $ do
        let source = "x = 1\ny = 2\nz = 3"
            edit = FixEdit (mkSrcSpanRaw "test.hs" 1 1 2 6) "replaced"
            result = applyEdits source [edit]
        T.isPrefixOf "replaced" result `shouldBe` True
        T.isInfixOf "z = 3" result `shouldBe` True

      it "handles edit at end of file" $ do
        let source = "x = 1"
            edit = FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 6) "999"
            result = applyEdits source [edit]
        T.isSuffixOf "999" result `shouldBe` True

      it "handles edit at start of file" $ do
        let source = "x = 1"
            edit = FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 2) "y"
            result = applyEdits source [edit]
        T.isPrefixOf "y" result `shouldBe` True

      it "handles unicode content" $ do
        let source = "x = \"αβγ\""
            edit = FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 10) "\"δεζ\""
            result = applyEdits source [edit]
        T.isInfixOf "δεζ" result `shouldBe` True

      it "handles tabs in content" $ do
        let source = "x\t=\t1"
            edit = FixEdit (mkSrcSpanRaw "test.hs" 1 4 1 5) "2"
            result = applyEdits source [edit]
        -- Tabs should be preserved
        T.isInfixOf "\t" result `shouldBe` True

      it "handles very long lines" $ do
        let longValue = T.replicate 1000 "x"
            source = "y = " <> longValue
            edit = FixEdit (mkSrcSpanRaw "test.hs" 1 5 1 1005) "short"
            result = applyEdits source [edit]
        T.isInfixOf "short" result `shouldBe` True

      it "handles multiple lines with same indentation" $ do
        let source = "  x = 1\n  y = 2"
            edit = FixEdit (mkSrcSpanRaw "test.hs" 1 7 1 8) "10"
            result = applyEdits source [edit]
        T.isInfixOf "10" result `shouldBe` True
        T.isInfixOf "  y = 2" result `shouldBe` True

    describe "import insertion point finding" $ do
      it "finds correct position after module declaration" $ do
        let source = "module Test where\n\nx = 1"
            initial = TransformState source False
            (_, final) = runTransform
              (insertImport "import Data.List")
              initial
        let lines' = T.lines (tsSource final)
        -- Import should be after module but before code
        any (\l -> "import Data.List" `T.isInfixOf` l) lines' `shouldBe` True

      it "finds correct position after existing imports" $ do
        let source = "module Test where\nimport Data.Maybe\n\nx = 1"
            initial = TransformState source False
            (_, final) = runTransform
              (insertImport "import Data.List")
              initial
        let lines' = T.lines (tsSource final)
        any (\l -> "import Data.List" `T.isInfixOf` l) lines' `shouldBe` True

      it "handles no module declaration" $ do
        let source = "import Data.Maybe\n\nx = 1"
            initial = TransformState source False
            (_, final) = runTransform
              (insertImport "import Data.List")
              initial
        T.isInfixOf "import Data.List" (tsSource final) `shouldBe` True

      it "handles file with only code" $ do
        let source = "x = 1"
            initial = TransformState source False
            (_, final) = runTransform
              (insertImport "import Data.List")
              initial
        T.isInfixOf "import Data.List" (tsSource final) `shouldBe` True

    describe "runTransformIO" $ do
      it "runs transformation in IO" $ do
        let initial = TransformState "x = 1" False
        (result, final) <- runTransformIO (pure 42) initial
        result `shouldBe` 42
        tsSource final `shouldBe` "x = 1"

      it "allows IO actions in transformation" $ do
        let initial = TransformState "x = 1" False
        (result, _) <- runTransformIO (pure "from IO") initial
        result `shouldBe` "from IO"
