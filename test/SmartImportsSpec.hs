{-# LANGUAGE OverloadedStrings #-}

module SmartImportsSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Argus.Rules.SmartImports
import Argus.Types (Fix(..))

spec :: Spec
spec = do
  describe "Argus.Rules.SmartImports" $ do
    describe "ImportSuggestion" $ do
      it "stores module name" $ do
        let s = ImportSuggestion "Data.Text" (Just "T") "Text" True True
        isModuleName s `shouldBe` "Data.Text"

      it "stores qualifier" $ do
        let s = ImportSuggestion "Data.Text" (Just "T") "Text" True True
        isQualifier s `shouldBe` Just "T"

      it "stores symbol name" $ do
        let s = ImportSuggestion "Data.Text" (Just "T") "pack" True True
        isSymbol s `shouldBe` "pack"

      it "indicates if qualified" $ do
        let s = ImportSuggestion "Data.Text" (Just "T") "pack" True True
        isQualified s `shouldBe` True

      it "indicates if preferred" $ do
        let s = ImportSuggestion "Data.Text" Nothing "pack" False True
        isPreferred s `shouldBe` True

    describe "defaultImportDB" $ do
      it "contains Data.Text symbols" $ do
        Map.member "Text" defaultImportDB `shouldBe` True
        Map.member "pack" defaultImportDB `shouldBe` True
        Map.member "unpack" defaultImportDB `shouldBe` True

      it "contains Data.Map symbols" $ do
        Map.member "Map" defaultImportDB `shouldBe` True
        Map.member "lookup" defaultImportDB `shouldBe` True
        Map.member "insert" defaultImportDB `shouldBe` True

      it "contains Data.Set symbols" $ do
        Map.member "Set" defaultImportDB `shouldBe` True
        Map.member "singleton" defaultImportDB `shouldBe` True

      it "contains Control.Monad symbols" $ do
        Map.member "when" defaultImportDB `shouldBe` True
        Map.member "unless" defaultImportDB `shouldBe` True
        Map.member "void" defaultImportDB `shouldBe` True
        Map.member "forM" defaultImportDB `shouldBe` True

      it "contains Data.Maybe symbols" $ do
        Map.member "fromMaybe" defaultImportDB `shouldBe` True
        Map.member "catMaybes" defaultImportDB `shouldBe` True
        Map.member "mapMaybe" defaultImportDB `shouldBe` True

      it "contains Data.Either symbols" $ do
        Map.member "either" defaultImportDB `shouldBe` True
        Map.member "lefts" defaultImportDB `shouldBe` True
        Map.member "rights" defaultImportDB `shouldBe` True

      it "contains Data.List symbols" $ do
        Map.member "sort" defaultImportDB `shouldBe` True
        Map.member "sortBy" defaultImportDB `shouldBe` True
        Map.member "nub" defaultImportDB `shouldBe` True

      it "contains Control.Exception symbols" $ do
        Map.member "Exception" defaultImportDB `shouldBe` True
        Map.member "catch" defaultImportDB `shouldBe` True
        Map.member "try" defaultImportDB `shouldBe` True
        Map.member "bracket" defaultImportDB `shouldBe` True

      it "contains Control.Concurrent symbols" $ do
        Map.member "forkIO" defaultImportDB `shouldBe` True
        Map.member "threadDelay" defaultImportDB `shouldBe` True

      it "contains STM symbols" $ do
        Map.member "STM" defaultImportDB `shouldBe` True
        Map.member "atomically" defaultImportDB `shouldBe` True
        Map.member "TVar" defaultImportDB `shouldBe` True

      it "has non-trivial size" $ do
        Map.size defaultImportDB `shouldSatisfy` (> 50)

    describe "lookupSymbol" $ do
      it "finds symbol in database" $ do
        let results = lookupSymbol "Text" defaultImportDB
        length results `shouldSatisfy` (> 0)

      it "returns empty for unknown symbol" $ do
        let results = lookupSymbol "unknownXYZ123" defaultImportDB
        results `shouldBe` []

      it "returns all suggestions for ambiguous symbol" $ do
        -- 'intercalate' is in both Data.Text and Data.List
        let results = lookupSymbol "intercalate" defaultImportDB
        length results `shouldSatisfy` (> 1)

    describe "suggestImport" $ do
      it "suggests Data.Text for Text" $ do
        let results = suggestImport "Text"
        any (\s -> isModuleName s == "Data.Text") results `shouldBe` True

      it "suggests Data.Map.Strict for Map" $ do
        let results = suggestImport "Map"
        any (\s -> isModuleName s == "Data.Map.Strict") results `shouldBe` True

      it "suggests Control.Monad for when" $ do
        let results = suggestImport "when"
        any (\s -> isModuleName s == "Control.Monad") results `shouldBe` True

      it "suggests T qualifier for Data.Text" $ do
        let results = suggestImport "Text"
            textSuggestions = filter (\s -> isModuleName s == "Data.Text") results
        any (\s -> isQualifier s == Just "T") textSuggestions `shouldBe` True

      it "suggests Map qualifier for Data.Map.Strict" $ do
        let results = suggestImport "Map"
            mapSuggestions = filter (\s -> isModuleName s == "Data.Map.Strict") results
        any (\s -> isQualifier s == Just "Map") mapSuggestions `shouldBe` True

    describe "strictImportFixes" $ do
      it "contains Data.Map fix" $ do
        any (\(lazy, _) -> lazy == "Data.Map") strictImportFixes `shouldBe` True

      it "maps Data.Map to Data.Map.Strict" $ do
        case lookup "Data.Map" strictImportFixes of
          Nothing -> expectationFailure "Expected Data.Map in fixes"
          Just strict -> strict `shouldBe` "Data.Map.Strict"

      it "contains Data.IntMap fix" $ do
        any (\(lazy, _) -> lazy == "Data.IntMap") strictImportFixes `shouldBe` True

      it "contains State monad fixes" $ do
        any (\(lazy, _) -> lazy == "Control.Monad.State") strictImportFixes `shouldBe` True

      it "contains Writer monad fixes" $ do
        any (\(lazy, _) -> lazy == "Control.Monad.Writer") strictImportFixes `shouldBe` True

      it "contains RWS monad fixes" $ do
        any (\(lazy, _) -> lazy == "Control.Monad.RWS") strictImportFixes `shouldBe` True

    describe "suggestStrictImport" $ do
      it "suggests strict version for Data.Map" $ do
        case suggestStrictImport "Data.Map" of
          Nothing -> expectationFailure "Expected suggestion"
          Just (strictMod, _) -> strictMod `shouldBe` "Data.Map.Strict"

      it "suggests strict version for Data.IntMap" $ do
        case suggestStrictImport "Data.IntMap" of
          Nothing -> expectationFailure "Expected suggestion"
          Just (strictMod, _) -> strictMod `shouldBe` "Data.IntMap.Strict"

      it "returns Nothing for already strict module" $ do
        suggestStrictImport "Data.Map.Strict" `shouldBe` Nothing

      it "returns Nothing for non-lazy module" $ do
        suggestStrictImport "Data.Text" `shouldBe` Nothing

      it "returns fix with preferred flag" $ do
        case suggestStrictImport "Data.Map" of
          Nothing -> expectationFailure "Expected suggestion"
          Just (_, fix) -> fixIsPreferred fix `shouldBe` True

    describe "ImportGroup" $ do
      it "has correct ordering" $ do
        PreludeGroup < BaseGroup `shouldBe` True
        BaseGroup < ExternalGroup `shouldBe` True
        ExternalGroup < InternalGroup `shouldBe` True

      it "PreludeGroup is minimum" $ do
        minBound `shouldBe` PreludeGroup

      it "InternalGroup is maximum" $ do
        maxBound `shouldBe` InternalGroup

    describe "groupImports" $ do
      it "groups base imports together" $ do
        let modules = ["Data.Text", "Data.Map", "Control.Monad"]
            groups = groupImports modules
        any (\g -> igrGroup g == BaseGroup) groups `shouldBe` True

      it "groups external imports together" $ do
        let modules = ["Aeson", "Network.HTTP"]
            groups = groupImports modules
        any (\g -> igrGroup g == ExternalGroup) groups `shouldBe` True

      it "groups internal imports together" $ do
        let modules = ["MyApp.Core", "MyApp.Utils"]
            groups = groupImports modules
        any (\g -> igrGroup g == InternalGroup) groups `shouldBe` True

      it "separates Prelude from other base" $ do
        let modules = ["Prelude", "Data.Text"]
            groups = groupImports modules
        any (\g -> igrGroup g == PreludeGroup) groups `shouldBe` True
        any (\g -> igrGroup g == BaseGroup) groups `shouldBe` True

      it "handles empty list" $ do
        let groups = groupImports []
        groups `shouldBe` []

    describe "organizeImports" $ do
      it "returns sorted modules" $ do
        let modules = ["Data.Text", "Control.Monad", "Data.Map"]
            organized = organizeImports modules
        -- Should contain all modules (possibly reordered)
        all (`elem` organized) modules `shouldBe` True

      it "handles empty list" $ do
        organizeImports [] `shouldBe` []

      it "handles single module" $ do
        let organized = organizeImports ["Data.Text"]
        length organized `shouldBe` 1

    describe "module group classification" $ do
      it "classifies Prelude as PreludeGroup" $ do
        let groups = groupImports ["Prelude"]
        any (\g -> igrGroup g == PreludeGroup) groups `shouldBe` True

      it "classifies Data.* as BaseGroup" $ do
        let groups = groupImports ["Data.Text", "Data.Map", "Data.Set"]
        all (\g -> igrGroup g == BaseGroup) groups `shouldBe` True

      it "classifies Control.* as BaseGroup" $ do
        let groups = groupImports ["Control.Monad", "Control.Exception"]
        all (\g -> igrGroup g == BaseGroup) groups `shouldBe` True

      it "classifies System.* as BaseGroup" $ do
        let groups = groupImports ["System.IO", "System.Directory"]
        all (\g -> igrGroup g == BaseGroup) groups `shouldBe` True

      it "classifies GHC.* as BaseGroup" $ do
        let groups = groupImports ["GHC.Generics", "GHC.TypeLits"]
        all (\g -> igrGroup g == BaseGroup) groups `shouldBe` True

      it "classifies Text.* as BaseGroup" $ do
        let groups = groupImports ["Text.Printf", "Text.Read"]
        all (\g -> igrGroup g == BaseGroup) groups `shouldBe` True

      it "classifies Aeson as ExternalGroup" $ do
        let groups = groupImports ["Data.Aeson"]
        any (\g -> igrGroup g == ExternalGroup) groups `shouldBe` True

      it "classifies Lens as ExternalGroup" $ do
        let groups = groupImports ["Control.Lens"]
        any (\g -> igrGroup g == ExternalGroup) groups `shouldBe` True

      it "classifies Network.* as ExternalGroup" $ do
        let groups = groupImports ["Network.HTTP", "Network.Socket"]
        all (\g -> igrGroup g == ExternalGroup) groups `shouldBe` True

      it "classifies Database.* as ExternalGroup" $ do
        let groups = groupImports ["Database.PostgreSQL"]
        all (\g -> igrGroup g == ExternalGroup) groups `shouldBe` True

      it "classifies Test.* as ExternalGroup" $ do
        let groups = groupImports ["Test.Hspec", "Test.QuickCheck"]
        all (\g -> igrGroup g == ExternalGroup) groups `shouldBe` True

      it "classifies unknown modules as InternalGroup" $ do
        let groups = groupImports ["MyProject.Core", "MyProject.Utils"]
        all (\g -> igrGroup g == InternalGroup) groups `shouldBe` True

    describe "symbol qualifiers" $ do
      it "suggests T for Data.Text functions" $ do
        let results = suggestImport "pack"
            textSuggestions = filter (\s -> isModuleName s == "Data.Text") results
        any (\s -> isQualifier s == Just "T") textSuggestions `shouldBe` True

      it "suggests Map for Data.Map.Strict functions" $ do
        let results = suggestImport "insert"
            mapSuggestions = filter (\s -> isModuleName s == "Data.Map.Strict") results
        any (\s -> isQualifier s == Just "Map") mapSuggestions `shouldBe` True

      it "suggests Set for Data.Set functions" $ do
        let results = suggestImport "singleton"
            setSuggestions = filter (\s -> isModuleName s == "Data.Set") results
        any (\s -> isQualifier s == Just "Set") setSuggestions `shouldBe` True

      it "suggests BS for Data.ByteString functions" $ do
        let results = suggestImport "ByteString"
            bsSuggestions = filter (\s -> isModuleName s == "Data.ByteString") results
        any (\s -> isQualifier s == Just "BS") bsSuggestions `shouldBe` True

    describe "ImportGroupResult" $ do
      it "stores group and modules" $ do
        let result = ImportGroupResult BaseGroup ["Data.Text", "Data.Map"]
        igrGroup result `shouldBe` BaseGroup
        igrModules result `shouldBe` ["Data.Text", "Data.Map"]

