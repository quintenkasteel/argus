{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : DeclarationSortSpec
-- Description : Tests for declaration sorting
-- Copyright   : (c) 2024
-- License     : MIT
module DeclarationSortSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import Argus.Refactor.DeclarationSort
import Argus.Types (mkSrcSpanRaw)

spec :: Spec
spec = do
  describe "Argus.Refactor.DeclarationSort" $ do
    describe "SortConfig" $ do
      it "has sensible defaults" $ do
        scStrategy defaultSortConfig `shouldBe` TypeFirst
        scGroupingStyle defaultSortConfig `shouldBe` GroupByCategory
        scPreserveGroups defaultSortConfig `shouldBe` True
        scBlankLinesBetweenGroups defaultSortConfig `shouldBe` 1
        scSortWithinGroups defaultSortConfig `shouldBe` True
        scKeepSignaturesWithBindings defaultSortConfig `shouldBe` True

      it "can be customized" $ do
        let config = defaultSortConfig
              { scStrategy = Alphabetical
              , scGroupingStyle = NoGrouping
              , scBlankLinesBetweenGroups = 2
              }
        scStrategy config `shouldBe` Alphabetical
        scGroupingStyle config `shouldBe` NoGrouping
        scBlankLinesBetweenGroups config `shouldBe` 2

    describe "SortStrategy" $ do
      it "has all expected strategies" $ do
        [minBound..maxBound] `shouldBe`
          [Alphabetical, TypeFirst, DependencyOrder, CustomOrder, NoSort]

    describe "GroupingStyle" $ do
      it "has all expected styles" $ do
        [minBound..maxBound] `shouldBe`
          [NoGrouping, GroupByCategory, GroupByPrefix, GroupByDependency]

    describe "DeclCategory" $ do
      it "has all expected categories" $ do
        [minBound..maxBound] `shouldBe`
          [DCTypeSig, DCTypeDecl, DCDataDecl, DCNewtypeDecl,
           DCClassDecl, DCFunctionDecl, DCInstanceDecl, DCOtherDecl]

      it "has correct ordering" $ do
        DCTypeSig < DCTypeDecl `shouldBe` True
        DCTypeDecl < DCDataDecl `shouldBe` True
        DCDataDecl < DCNewtypeDecl `shouldBe` True
        DCFunctionDecl < DCInstanceDecl `shouldBe` True

    describe "defaultCategoryOrder" $ do
      it "starts with type signatures" $ do
        head defaultCategoryOrder `shouldBe` DCTypeSig

      it "ends with other declarations" $ do
        last defaultCategoryOrder `shouldBe` DCOtherDecl

      it "has types before functions" $ do
        let typeIdx = length $ takeWhile (/= DCTypeDecl) defaultCategoryOrder
            funcIdx = length $ takeWhile (/= DCFunctionDecl) defaultCategoryOrder
        typeIdx < funcIdx `shouldBe` True

    describe "DeclarationInfo" $ do
      it "can be created with all fields" $ do
        let info = DeclarationInfo
              { diName = "foo"
              , diCategory = DCFunctionDecl
              , diSpan = mkSrcSpanRaw "test.hs" 1 1 1 10
              , diStartLine = 1
              , diEndLine = 3
              , diText = "foo :: Int -> Int\nfoo x = x + 1"
              , diDependencies = Set.fromList ["bar", "baz"]
              , diAssociatedSig = Nothing
              , diCommentBefore = Just "-- | Foo function"
              }
        diName info `shouldBe` "foo"
        diCategory info `shouldBe` DCFunctionDecl
        Set.size (diDependencies info) `shouldBe` 2

    describe "sortAlphabetical" $ do
      it "sorts by name" $ do
        let decls =
              [ mkDecl "zoo" DCFunctionDecl
              , mkDecl "apple" DCFunctionDecl
              , mkDecl "banana" DCFunctionDecl
              ]
            sorted = sortAlphabetical decls
        map diName sorted `shouldBe` ["apple", "banana", "zoo"]

      it "handles empty list" $ do
        sortAlphabetical [] `shouldBe` []

      it "handles single element" $ do
        let decls = [mkDecl "foo" DCFunctionDecl]
        sortAlphabetical decls `shouldBe` decls

    describe "sortTypeFirst" $ do
      it "puts types before functions" $ do
        let decls =
              [ mkDecl "myFunc" DCFunctionDecl
              , mkDecl "MyData" DCDataDecl
              , mkDecl "MyClass" DCClassDecl
              ]
            sorted = sortTypeFirst decls
        map diCategory sorted `shouldBe` [DCDataDecl, DCClassDecl, DCFunctionDecl]

      it "puts type signatures first" $ do
        let decls =
              [ mkDecl "foo" DCFunctionDecl
              , mkDecl "foo" DCTypeSig
              ]
            sorted = sortTypeFirst decls
        map diCategory sorted `shouldBe` [DCTypeSig, DCFunctionDecl]

      it "puts instances last" $ do
        let decls =
              [ mkDecl "(instance)" DCInstanceDecl
              , mkDecl "foo" DCFunctionDecl
              , mkDecl "Bar" DCDataDecl
              ]
            sorted = sortTypeFirst decls
        last (map diCategory sorted) `shouldBe` DCInstanceDecl

    describe "sortCustom" $ do
      it "respects custom category order" $ do
        let customOrder = [DCFunctionDecl, DCDataDecl, DCTypeSig]
            config = defaultSortConfig { scCategoryOrder = customOrder }
            decls =
              [ mkDecl "foo" DCTypeSig
              , mkDecl "foo" DCFunctionDecl
              , mkDecl "Foo" DCDataDecl
              ]
            sorted = sortCustom config decls
        map diCategory sorted `shouldBe` [DCFunctionDecl, DCDataDecl, DCTypeSig]

    describe "groupByCategory" $ do
      it "groups declarations by category" $ do
        let decls =
              [ mkDecl "foo" DCFunctionDecl
              , mkDecl "bar" DCFunctionDecl
              , mkDecl "Baz" DCDataDecl
              ]
            grouped = groupByCategory decls
        Map.size grouped `shouldBe` 2
        length (Map.findWithDefault [] DCFunctionDecl grouped) `shouldBe` 2
        length (Map.findWithDefault [] DCDataDecl grouped) `shouldBe` 1

      it "handles empty list" $ do
        let grouped = groupByCategory []
        Map.size grouped `shouldBe` 0

    describe "SortResult" $ do
      it "has expected fields" $ do
        let result = SortResult
              { srSuccess = True
              , srFix = Nothing
              , srChanges = 0
              , srGroups = 3
              , srWarnings = []
              , srPreview = Nothing
              }
        srSuccess result `shouldBe` True
        srChanges result `shouldBe` 0

    describe "DeclarationAnalysis" $ do
      it "tracks if module is well ordered" $ do
        let analysis = DeclarationAnalysis
              { daDeclarations = []
              , daCategories = Map.empty
              , daDependencies = Map.empty
              , daCurrentOrder = ["a", "b", "c"]
              , daSuggestedOrder = ["a", "b", "c"]
              , daIsWellOrdered = True
              }
        daIsWellOrdered analysis `shouldBe` True

      it "detects when not well ordered" $ do
        let analysis = DeclarationAnalysis
              { daDeclarations = []
              , daCategories = Map.empty
              , daDependencies = Map.empty
              , daCurrentOrder = ["c", "b", "a"]
              , daSuggestedOrder = ["a", "b", "c"]
              , daIsWellOrdered = False
              }
        daIsWellOrdered analysis `shouldBe` False

    describe "renderDeclarations" $ do
      it "renders without grouping" $ do
        let config = defaultSortConfig { scGroupingStyle = NoGrouping }
            decls = [mkDeclWithText "foo" DCFunctionDecl "foo = 1\n"]
            rendered = renderDeclarations config "" decls
        T.isInfixOf "foo = 1" rendered `shouldBe` True

    describe "edge cases" $ do
      it "handles declarations with same name different category" $ do
        let decls =
              [ mkDecl "foo" DCTypeSig
              , mkDecl "foo" DCFunctionDecl
              ]
            sorted = sortTypeFirst decls
        length sorted `shouldBe` 2

      it "handles unicode in names" $ do
        let decls = [mkDecl "cafetería" DCFunctionDecl]
        sortAlphabetical decls `shouldBe` decls

-- | Helper to create a minimal DeclarationInfo for testing
mkDecl :: Text -> DeclCategory -> DeclarationInfo
mkDecl name category = DeclarationInfo
  { diName = name
  , diCategory = category
  , diSpan = mkSrcSpanRaw "test.hs" 1 1 1 10
  , diStartLine = 1
  , diEndLine = 1
  , diText = name <> " = undefined\n"
  , diDependencies = Set.empty
  , diAssociatedSig = Nothing
  , diCommentBefore = Nothing
  }

-- | Helper to create DeclarationInfo with specific text
mkDeclWithText :: Text -> DeclCategory -> Text -> DeclarationInfo
mkDeclWithText name category text = (mkDecl name category) { diText = text }
