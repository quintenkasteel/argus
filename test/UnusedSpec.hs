{-# LANGUAGE OverloadedStrings #-}

module UnusedSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Hspec

import Argus.Analysis.DepGraph (DepGraph, DepNode(..), addNode, emptyGraph, dgRoots)
import Argus.Analysis.Unused
import Argus.Types (QualifiedName(..), Symbol(..), SymbolKind(..), noSrcSpan)

spec :: Spec
spec = do
  describe "Argus.Analysis.Unused" $ do
    describe "defaultUnusedConfig" $ do
      it "has main root pattern" $ do
        any (T.isInfixOf "main") (ucRoots defaultUnusedConfig) `shouldBe` True

      it "has Paths_ root pattern" $ do
        any (T.isInfixOf "Paths_") (ucRoots defaultUnusedConfig) `shouldBe` True

      it "has TH roots for JSON deriving" $ do
        any (T.isInfixOf "parseJSON") (ucThRoots defaultUnusedConfig) `shouldBe` True
        any (T.isInfixOf "toJSON") (ucThRoots defaultUnusedConfig) `shouldBe` True

      it "has makeLenses TH root" $ do
        any (T.isInfixOf "makeLenses") (ucThRoots defaultUnusedConfig) `shouldBe` True

      it "enables all checks by default" $ do
        ucCheckFunctions defaultUnusedConfig `shouldBe` True
        ucCheckTypes defaultUnusedConfig `shouldBe` True
        ucCheckImports defaultUnusedConfig `shouldBe` True
        ucCheckExports defaultUnusedConfig `shouldBe` True

    describe "UnusedKind" $ do
      it "has all expected kinds" $ do
        (UnusedFunction :: UnusedKind) `shouldSatisfy` const True
        (UnusedType :: UnusedKind) `shouldSatisfy` const True
        (UnusedConstructor :: UnusedKind) `shouldSatisfy` const True
        (UnusedImportItem :: UnusedKind) `shouldSatisfy` const True
        (UnusedExport :: UnusedKind) `shouldSatisfy` const True
        (UnusedTypeClass :: UnusedKind) `shouldSatisfy` const True
        (UnusedInstance :: UnusedKind) `shouldSatisfy` const True

    describe "UnusedItem" $ do
      it "stores all fields correctly" $ do
        let item = UnusedItem
              { uiName = mkQName "unused" "Main"
              , uiKind = UnusedFunction
              , uiSpan = noSrcSpan
              , uiMessage = "Function 'unused' appears to be unused"
              , uiConfidence = 0.9
              , uiSuggestion = Just "Remove the function"
              , uiRelated = []
              , uiCanAutoFix = True
              }
        uiKind item `shouldBe` UnusedFunction
        uiConfidence item `shouldBe` 0.9
        uiCanAutoFix item `shouldBe` True

    describe "UnusedResult" $ do
      it "stores graph and sets" $ do
        let emptyStats = UnusedStats
              { usTotal = 0
              , usByKind = Map.empty
              , usHighConfidence = 0
              , usAutoFixable = 0
              }
            result = UnusedResult
              { urItems = []
              , urGraph = emptyGraph
              , urReachable = Set.empty
              , urUnreachable = Set.empty
              , urRoots = Set.empty
              , urStats = emptyStats
              }
        urItems result `shouldBe` []
        Set.null (urReachable result) `shouldBe` True
        Set.null (urUnreachable result) `shouldBe` True
        Set.null (urRoots result) `shouldBe` True

    describe "detectUnusedFunctions" $ do
      it "returns empty for empty unreachable set" $ do
        let graph = emptyGraph
        detectUnusedFunctions Set.empty graph `shouldBe` []

      it "detects unreachable functions" $ do
        let name = mkQName "unused" "Main"
            node = mkDepNode "unused" Function "Main"
            graph = addNode name node emptyGraph
            unreachable = Set.singleton name
            items = detectUnusedFunctions unreachable graph
        length items `shouldBe` 1
        uiKind (head items) `shouldBe` UnusedFunction

      it "does not flag reachable functions" $ do
        let name = mkQName "used" "Main"
            node = mkDepNode "used" Function "Main"
            graph = addNode name node emptyGraph
            unreachable = Set.empty  -- Not in unreachable set
            items = detectUnusedFunctions unreachable graph
        items `shouldBe` []

    describe "detectUnusedTypes" $ do
      it "returns empty for empty unreachable set" $ do
        let graph = emptyGraph
        detectUnusedTypes Set.empty graph `shouldBe` []

      it "detects unreachable types" $ do
        let name = mkQName "UnusedType" "Main"
            node = mkDepNode "UnusedType" TypeConstructor "Main"
            graph = addNode name node emptyGraph
            unreachable = Set.singleton name
            items = detectUnusedTypes unreachable graph
        length items `shouldBe` 1
        uiKind (head items) `shouldBe` UnusedType

      it "detects unreachable type classes" $ do
        let name = mkQName "UnusedClass" "Main"
            node = mkDepNode "UnusedClass" TypeClass "Main"
            graph = addNode name node emptyGraph
            unreachable = Set.singleton name
            items = detectUnusedTypes unreachable graph
        length items `shouldBe` 1

    describe "detectUnused" $ do
      it "respects ucCheckFunctions = False" $ do
        let config = defaultUnusedConfig { ucCheckFunctions = False }
            name = mkQName "unused" "Main"
            node = mkDepNode "unused" Function "Main"
            graph = addNode name node emptyGraph
            result = detectUnused config graph []
        -- With functions disabled, should still report types but not functions
        all (\i -> uiKind i /= UnusedFunction) (urItems result) `shouldBe` True

      it "respects ucCheckTypes = False" $ do
        let config = defaultUnusedConfig { ucCheckTypes = False }
            name = mkQName "UnusedType" "Main"
            node = mkDepNode "UnusedType" TypeConstructor "Main"
            graph = addNode name node emptyGraph
            result = detectUnused config graph []
        -- With types disabled, should not report types
        all (\i -> uiKind i /= UnusedType) (urItems result) `shouldBe` True

      it "returns graph with roots added" $ do
        let config = defaultUnusedConfig
            name = mkQName "main" "Main"
            node = mkDepNode "main" Function "Main"
            graph = addNode name node emptyGraph
            result = detectUnused config graph []
        -- The main function should be recognized as a root
        name `Set.member` dgRoots (urGraph result) `shouldBe` True

      it "handles empty graph" $ do
        let config = defaultUnusedConfig
            result = detectUnused config emptyGraph []
        urItems result `shouldBe` []

-- Helper functions
mkQName :: T.Text -> T.Text -> QualifiedName
mkQName name modName = QualifiedName
  { qnName = name
  , qnModule = Just modName
  }

mkDepNode :: T.Text -> SymbolKind -> T.Text -> DepNode
mkDepNode name kind modName = DepNode
  { dnSymbol = Symbol
      { symbolName = mkQName name modName
      , symbolKind = kind
      , symbolSpan = noSrcSpan
      , symbolExported = True
      , symbolType = Nothing
      }
  , dnIsRoot = False
  , dnIsThGen = False
  , dnModule = modName
  }
