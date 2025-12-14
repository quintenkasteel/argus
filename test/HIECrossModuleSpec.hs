{-# LANGUAGE OverloadedStrings #-}

module HIECrossModuleSpec (spec) where

import Test.Hspec
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)

import Argus.Types (mkSrcSpanRaw)
import Argus.HIE.Types
import Argus.HIE.CrossModule

spec :: Spec
spec = do
  describe "Argus.HIE.CrossModule" $ do
    describe "MultiFileFix" $ do
      it "builds from file spans" $ do
        let spans1 = [mkSrcSpanRaw "src/A.hs" 1 1 1 10]
            spans2 = [mkSrcSpanRaw "src/B.hs" 5 1 5 10]
            mff = buildMultiFileFix "oldName" "newName" [("src/A.hs", spans1), ("src/B.hs", spans2)]
        mffSymbol mff `shouldBe` "oldName"
        Map.size (mffFixes mff) `shouldBe` 2

      it "handles empty spans for a file" $ do
        let mff = buildMultiFileFix "sym" "new" [("src/A.hs", [])]
        Map.size (mffFixes mff) `shouldBe` 0

      it "includes dependency order" $ do
        let spans = [mkSrcSpanRaw "src/A.hs" 1 1 1 10]
            mff = buildMultiFileFix "sym" "new" [("src/A.hs", spans)]
        mffDependencyOrder mff `shouldSatisfy` (elem "src/A.hs")

    describe "RenameResult" $ do
      it "has success and failure states" $ do
        let success = RenameResult True Map.empty [] [] [] []
            failure = RenameResult False Map.empty [] [] [] ["error"]
        rrSuccess success `shouldBe` True
        rrSuccess failure `shouldBe` False
        rrErrors failure `shouldBe` ["error"]

      it "tracks affected files" $ do
        let result = RenameResult True (Map.singleton "src/A.hs" []) ["src/A.hs"] [] [] []
        rrAffectedFiles result `shouldBe` ["src/A.hs"]

      it "tracks export changes" $ do
        let result = RenameResult True Map.empty [] [("Module", "old", "new")] [] []
        rrExportChanges result `shouldBe` [("Module", "old", "new")]

    describe "QualifiedRef" $ do
      it "stores qualified reference info" $ do
        let span = mkSrcSpanRaw "test.hs" 1 1 1 10
            ref = SymbolReference "map" "Data.List" "test.hs" span RefUsage (Just "L")
            qref = QualifiedRef ref (Just "L") False "Data.List"
        qrQualifier qref `shouldBe` Just "L"
        qrIsReExport qref `shouldBe` False
        qrOrigModule qref `shouldBe` "Data.List"

      it "identifies re-exports" $ do
        let span = mkSrcSpanRaw "test.hs" 1 1 1 10
            ref = SymbolReference "map" "Prelude" "test.hs" span RefExport Nothing
            qref = QualifiedRef ref Nothing True "Data.List"
        qrIsReExport qref `shouldBe` True

    describe "EnhancedRenameResult" $ do
      it "wraps base result" $ do
        let baseResult = RenameResult True Map.empty [] [] [] []
            enhanced = EnhancedRenameResult baseResult Map.empty Map.empty Map.empty Map.empty
        rrSuccess (errBaseResult enhanced) `shouldBe` True

      it "includes qualified edits" $ do
        let baseResult = RenameResult True Map.empty [] [] [] []
            edits = Map.singleton "test.hs" []
            enhanced = EnhancedRenameResult baseResult edits Map.empty Map.empty Map.empty
        Map.size (errQualifiedEdits enhanced) `shouldBe` 1

    describe "RenameSafetyResult" $ do
      it "indicates safe rename" $ do
        let result = RenameSafetyResult True [] [] 5 10
        rsrSafe result `shouldBe` True
        rsrAffectedFiles result `shouldBe` 5
        rsrAffectedRefs result `shouldBe` 10

      it "indicates unsafe with errors" $ do
        let result = RenameSafetyResult False [] ["conflict"] 1 1
        rsrSafe result `shouldBe` False
        rsrErrors result `shouldBe` ["conflict"]

      it "tracks warnings" $ do
        let result = RenameSafetyResult True ["potential issue"] [] 2 5
        rsrWarnings result `shouldBe` ["potential issue"]

    describe "BatchRenameResult" $ do
      it "tracks successful and failed renames" $ do
        let success = ("old1", "new1", RenameResult True Map.empty [] [] [] [])
            failed = ("old2", "new2", ["error"])
            result = BatchRenameResult [success] [failed] 1 2
        length (brrSuccessful result) `shouldBe` 1
        length (brrFailed result) `shouldBe` 1
        brrTotalFiles result `shouldBe` 1
        brrTotalEdits result `shouldBe` 2

    describe "Reference Tracking Types" $ do
      describe "SymbolReference" $ do
        it "stores reference location" $ do
          let span = mkSrcSpanRaw "test.hs" 10 5 10 15
              ref = SymbolReference "foo" "Test" "test.hs" span RefUsage Nothing
          srSymbol ref `shouldBe` "foo"
          srModule ref `shouldBe` "Test"
          srFile ref `shouldBe` "test.hs"
          srKind ref `shouldBe` RefUsage

        it "tracks qualifier" $ do
          let span = mkSrcSpanRaw "test.hs" 10 5 10 15
              ref = SymbolReference "map" "Prelude" "test.hs" span RefUsage (Just "P")
          srQualified ref `shouldBe` Just "P"

      describe "ReferenceKind" $ do
        it "has all kinds" $ do
          let kinds = [RefUsage, RefDefinition, RefImport, RefExport,
                       RefTypeSignature, RefInstanceHead, RefPattern]
          length kinds `shouldBe` 7

        it "distinguishes definition from usage" $ do
          RefDefinition `shouldSatisfy` (/= RefUsage)

    describe "ModuleSymbols" $ do
      it "stores module exports" $ do
        let ms = ModuleSymbols
              { msModule = "Data.List"
              , msFile = Just "src/Data/List.hs"
              , msExports = Set.fromList ["map", "filter", "fold"]
              , msImports = Map.empty
              , msDefinitions = []
              }
        msModule ms `shouldBe` "Data.List"
        Set.member "map" (msExports ms) `shouldBe` True

      it "tracks imports by module" $ do
        let ms = ModuleSymbols
              { msModule = "MyModule"
              , msFile = Just "src/MyModule.hs"
              , msExports = Set.empty
              , msImports = Map.fromList [("Data.List", ["map", "filter"])]
              , msDefinitions = []
              }
        Map.lookup "Data.List" (msImports ms) `shouldBe` Just ["map", "filter"]

    describe "Edge Cases" $ do
      it "handles empty rename" $ do
        let result = RenameResult True Map.empty [] [] [] []
        null (rrAffectedFiles result) `shouldBe` True

      it "handles rename with no conflicts" $ do
        let safety = RenameSafetyResult True [] [] 0 0
        rsrSafe safety `shouldBe` True
        rsrAffectedFiles safety `shouldBe` 0

      it "handles multi-file fix with single file" $ do
        let spans = [mkSrcSpanRaw "only.hs" 1 1 1 5]
            mff = buildMultiFileFix "x" "y" [("only.hs", spans)]
        length (mffDependencyOrder mff) `shouldBe` 1
