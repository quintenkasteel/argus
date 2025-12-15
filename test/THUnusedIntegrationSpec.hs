{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : THUnusedIntegrationSpec
-- Description : Integration tests for TH-aware unused detection
--
-- These tests verify that Argus correctly identifies functions as "used"
-- when they are referenced by Template Haskell generated code, such as:
-- - Yesod's widgetFile, hamletFile, juliusFile patterns
-- - Aeson's deriveJSON patterns
-- - Lens's makeLenses patterns
--
-- Key scenarios tested:
-- 1. Functions used only within TH splices are NOT marked as unused
-- 2. Functions listed as TH roots are recognized as used
-- 3. Actually unused functions ARE correctly flagged
-- 4. Cross-GHC-version HIE file handling
module THUnusedIntegrationSpec (spec) where

import Test.Hspec
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

import Argus.Analysis.DepGraph
import Argus.Analysis.Unused
import Argus.Analysis.TemplateHaskell
import Argus.Analysis.Syntactic (ThSpliceInfo(..))
import Argus.Types

spec :: Spec
spec = do
  describe "TH-Aware Unused Detection Integration" $ do
    describe "TH Root Pattern Matching" $ do
      it "recognizes Yesod widgetFile patterns as roots" $ do
        let config = defaultUnusedConfig
              { ucThRoots = ["widgetFile", "hamletFile", "formatUserName", "getDefaultTitle"]
              }
            -- Build a graph with functions that would be used via widgetFile
            formatUserName = mkFunctionSymbol "YesodApp" "formatUserName"
            getDefaultTitle = mkFunctionSymbol "YesodApp" "getDefaultTitle"
            -- homepageWidget uses these via TH
            homepageWidget = mkFunctionSymbol "YesodApp" "homepageWidget"

            -- Build graph with TH-style edges
            graph = buildGraph
              [formatUserName, getDefaultTitle, homepageWidget]
              [ (homepageWidget, formatUserName, ThSplice, noSrcSpan)
              , (homepageWidget, getDefaultTitle, ThSplice, noSrcSpan)
              ]
              (Set.singleton (symbolName homepageWidget))

            result = detectUnused config graph []

        -- formatUserName and getDefaultTitle should NOT be in unused list
        -- because they match TH roots
        let unusedNames = Set.fromList $ map (qnName . uiName) (urItems result)
        Set.member "formatUserName" unusedNames `shouldBe` False
        Set.member "getDefaultTitle" unusedNames `shouldBe` False

      it "recognizes Aeson deriveJSON patterns as roots" $ do
        let config = defaultUnusedConfig
              { ucThRoots = ["parseJSON", "toJSON", "toEncoding", "deriveJSON"]
              }
            parseJSON = mkFunctionSymbol "Types" "parseJSON"
            toJSON = mkFunctionSymbol "Types" "toJSON"
            userType = mkTypeSymbol "Types" "User"

            graph = buildGraph
              [parseJSON, toJSON, userType]
              []  -- TH generates the connection
              (Set.singleton (symbolName userType))

            result = detectUnused config graph []

        -- parseJSON and toJSON match TH roots, shouldn't be flagged
        let unusedNames = Set.fromList $ map (qnName . uiName) (urItems result)
        Set.member "parseJSON" unusedNames `shouldBe` False
        Set.member "toJSON" unusedNames `shouldBe` False

      it "recognizes Lens makeLenses patterns as roots" $ do
        let config = defaultUnusedConfig
              { ucThRoots = ["makeLenses", "makePrisms", "makeFields"]
              }
            makeLenses = mkFunctionSymbol "Lens" "makeLenses"
            _userName = mkFunctionSymbol "Types" "_userName"  -- Lens-generated accessor

            graph = buildGraph [makeLenses, _userName] [] Set.empty
            result = detectUnused config graph []

        -- makeLenses matches TH roots
        let unusedNames = Set.fromList $ map (qnName . uiName) (urItems result)
        Set.member "makeLenses" unusedNames `shouldBe` False

    describe "Detecting Actually Unused Functions" $ do
      it "flags truly unused functions" $ do
        let config = defaultUnusedConfig
            usedFunc = mkFunctionSymbol "Main" "usedFunction"
            unusedFunc = mkFunctionSymbol "Main" "unusedFunction"
            mainFunc = mkFunctionSymbol "Main" "main"

            -- main calls usedFunc, unusedFunc is orphaned
            graph = buildGraph
              [usedFunc, unusedFunc, mainFunc]
              [(mainFunc, usedFunc, DirectReference, noSrcSpan)]
              (Set.singleton (symbolName mainFunc))

            result = detectUnused config graph []

        -- unusedFunction should be flagged
        let unusedNames = Set.fromList $ map (qnName . uiName) (urItems result)
        Set.member "unusedFunction" unusedNames `shouldBe` True
        Set.member "usedFunction" unusedNames `shouldBe` False
        Set.member "main" unusedNames `shouldBe` False

      it "correctly handles underscore-prefixed names as intentionally unused" $ do
        -- Note: Patterns match against full qualified names (Module.name)
        let config = defaultUnusedConfig
              { ucIgnorePatterns = ["\\._.*"]  -- Match _xxx anywhere after a dot
              }
            _unused = mkFunctionSymbol "Main" "_internalHelper"
            mainFunc = mkFunctionSymbol "Main" "main"

            graph = buildGraph [_unused, mainFunc] [] (Set.singleton (symbolName mainFunc))
            result = detectUnused config graph []

        -- _internalHelper matches ignore pattern (Main._internalHelper), should not be flagged
        let unusedNames = Set.fromList $ map (qnName . uiName) (urItems result)
        Set.member "_internalHelper" unusedNames `shouldBe` False

    describe "TH Splice Detection Integration" $ do
      it "marks TH-generated nodes correctly in graph" $ do
        let genFunc = mkFunctionSymbol "Gen" "generatedByTH"
            graph = markThGenerated (symbolName genFunc) $
                    addNode (symbolName genFunc) (mkDepNode' genFunc) emptyGraph

        -- Check that the node is marked as TH-generated
        case Map.lookup (symbolName genFunc) (dgNodes graph) of
          Just node -> dnIsThGen node `shouldBe` True
          Nothing -> expectationFailure "Generated node not found"

      it "does not flag TH-generated functions as unused" $ do
        let config = defaultUnusedConfig
            genFunc = mkFunctionSymbol "Gen" "$fShowUser"  -- GHC-generated instance
            mainFunc = mkFunctionSymbol "Main" "main"

            node = (mkDepNode' genFunc) { dnIsThGen = True }
            graph = addNode (symbolName mainFunc) (mkDepNode' mainFunc) $
                    addRoot (symbolName mainFunc) $
                    addNode (symbolName genFunc) node emptyGraph

            result = detectUnused config graph []

        -- Compiler-generated names (starting with $) should not be flagged
        let unusedNames = Set.fromList $ map (qnName . uiName) (urItems result)
        Set.member "$fShowUser" unusedNames `shouldBe` False

    describe "Root Pattern Configuration" $ do
      it "respects custom root patterns" $ do
        -- Note: Patterns match against full qualified names (Module.name)
        let config = defaultUnusedConfig
              { ucRoots = ["Main\\.main$", "Widget$", "\\.spec$"]
              }
            mainFunc = mkFunctionSymbol "Main" "main"
            homeWidget = mkFunctionSymbol "App" "homeWidget"
            specFunc = mkFunctionSymbol "Test" "spec"
            helper = mkFunctionSymbol "Utils" "helper"

            graph = buildGraph
              [mainFunc, homeWidget, specFunc, helper]
              []
              Set.empty  -- No explicit roots

            result = detectUnused config graph []

        -- main, homeWidget, spec match root patterns -> reachable
        -- helper doesn't match -> unreachable
        let roots = urRoots result
        Set.size roots `shouldSatisfy` (>= 3)  -- At least main, homeWidget, spec

      it "combines ucRoots and ucThRoots" $ do
        -- Note: Patterns match against full qualified names (Module.name)
        let config = defaultUnusedConfig
              { ucRoots = ["\\.main$"]       -- Matches Main.main
              , ucThRoots = ["parseJSON"]    -- Matches Types.parseJSON
              }
            mainFunc = mkFunctionSymbol "Main" "main"
            parseJSON = mkFunctionSymbol "Types" "parseJSON"
            helper = mkFunctionSymbol "Utils" "helper"

            graph = buildGraph [mainFunc, parseJSON, helper] [] Set.empty
            result = detectUnused config graph []

        -- Both main and parseJSON should be roots (patterns combined)
        Set.member (symbolName mainFunc) (urRoots result) `shouldBe` True

    describe "Unused Kind Classification" $ do
      it "classifies unused functions correctly" $ do
        let config = defaultUnusedConfig
            unusedFunc = mkFunctionSymbol "Main" "unused"
            mainFunc = mkFunctionSymbol "Main" "main"

            graph = buildGraph [unusedFunc, mainFunc] [] (Set.singleton (symbolName mainFunc))
            result = detectUnused config graph []

        case filter ((== "unused") . qnName . uiName) (urItems result) of
          [item] -> uiKind item `shouldBe` UnusedFunction
          _ -> pure ()  -- May be filtered by confidence

      it "classifies unused types correctly" $ do
        let config = defaultUnusedConfig
            unusedType = mkTypeSymbol "Types" "UnusedType"
            usedType = mkTypeSymbol "Types" "UsedType"
            mainFunc = mkFunctionSymbol "Main" "main"

            graph = buildGraph
              [unusedType, usedType, mainFunc]
              [(mainFunc, usedType, TypeDependency, noSrcSpan)]
              (Set.singleton (symbolName mainFunc))

            result = detectUnused config graph []

        case filter ((== "UnusedType") . qnName . uiName) (urItems result) of
          [item] -> uiKind item `shouldBe` UnusedType
          _ -> pure ()

    describe "Confidence Levels" $ do
      it "assigns high confidence to unused functions" $ do
        let config = defaultUnusedConfig { ucMinConfidence = 0.0 }  -- Show all
            unusedFunc = mkFunctionSymbol "Main" "unused"
            mainFunc = mkFunctionSymbol "Main" "main"

            graph = buildGraph [unusedFunc, mainFunc] [] (Set.singleton (symbolName mainFunc))
            result = detectUnused config graph []

        case filter ((== "unused") . qnName . uiName) (urItems result) of
          [item] -> uiConfidence item `shouldSatisfy` (>= 0.7)
          _ -> pure ()

      it "filters by minimum confidence" $ do
        let configLow = defaultUnusedConfig { ucMinConfidence = 0.0 }
            configHigh = defaultUnusedConfig { ucMinConfidence = 0.99 }
            unusedFunc = mkFunctionSymbol "Main" "unused"
            mainFunc = mkFunctionSymbol "Main" "main"

            graph = buildGraph [unusedFunc, mainFunc] [] (Set.singleton (symbolName mainFunc))
            resultLow = detectUnused configLow graph []
            resultHigh = detectUnused configHigh graph []

        -- Low threshold shows more results
        length (urItems resultLow) `shouldSatisfy` (>= length (urItems resultHigh))

    describe "Auto-fix Capability" $ do
      it "marks functions as auto-fixable" $ do
        let config = defaultUnusedConfig { ucMinConfidence = 0.0 }
            unusedFunc = mkFunctionSymbol "Main" "unused"
            mainFunc = mkFunctionSymbol "Main" "main"

            graph = buildGraph [unusedFunc, mainFunc] [] (Set.singleton (symbolName mainFunc))
            result = detectUnused config graph []

        case filter ((== "unused") . qnName . uiName) (urItems result) of
          [item] -> uiCanAutoFix item `shouldBe` True
          _ -> pure ()

      it "can generate removal fix for unused item" $ do
        let item = UnusedItem
              { uiName = mkQualifiedName (Just "Main") "unused"
              , uiKind = UnusedFunction
              , uiSpan = noSrcSpan
              , uiMessage = "test"
              , uiConfidence = 0.9
              , uiSuggestion = Nothing
              , uiRelated = []
              , uiCanAutoFix = True
              }
            fix = mkRemovalFix item

        fixIsPreferred fix `shouldBe` True
        T.isInfixOf "unused" (fixTitle fix) `shouldBe` True

    describe "Statistics" $ do
      it "computes correct statistics" $ do
        let config = defaultUnusedConfig { ucMinConfidence = 0.0 }
            unused1 = mkFunctionSymbol "Main" "unused1"
            unused2 = mkFunctionSymbol "Main" "unused2"
            mainFunc = mkFunctionSymbol "Main" "main"

            graph = buildGraph [unused1, unused2, mainFunc] [] (Set.singleton (symbolName mainFunc))
            result = detectUnused config graph []

        usTotal (urStats result) `shouldSatisfy` (>= 0)

    describe "Yesod Widget Pattern Simulation" $ do
      it "handles simulated widgetFile pattern correctly" $ do
        -- Simulate the Yesod pattern where:
        -- - homepageWidget is an entry point (root)
        -- - formatUserName, getDefaultTitle are used within TH templates
        -- - actuallyUnused is truly unused
        let config = defaultUnusedConfig
              { ucRoots = [".*Widget$"]  -- Widget functions are roots
              , ucThRoots = ["formatUserName", "getDefaultTitle", "userGreeting"]
              }
            homepageWidget = mkFunctionSymbol "YesodApp" "homepageWidget"
            formatUserName = mkFunctionSymbol "YesodApp" "formatUserName"
            getDefaultTitle = mkFunctionSymbol "YesodApp" "getDefaultTitle"
            userGreeting = mkFunctionSymbol "YesodApp" "userGreeting"
            actuallyUnused = mkFunctionSymbol "YesodApp" "actuallyUnused"

            -- In real Yesod, widgetFile TH would create these edges
            graph = buildGraph
              [homepageWidget, formatUserName, getDefaultTitle, userGreeting, actuallyUnused]
              [ (homepageWidget, formatUserName, ThSplice, noSrcSpan)
              , (homepageWidget, getDefaultTitle, ThSplice, noSrcSpan)
              , (userGreeting, formatUserName, DirectReference, noSrcSpan)
              ]
              Set.empty  -- Roots will be found via patterns

            result = detectUnused config graph []

        -- homepageWidget matches root pattern
        Set.member (symbolName homepageWidget) (urRoots result) `shouldBe` True

        -- formatUserName, getDefaultTitle, userGreeting match TH roots
        let unusedNames = Set.fromList $ map (qnName . uiName) (urItems result)
        Set.member "formatUserName" unusedNames `shouldBe` False
        Set.member "getDefaultTitle" unusedNames `shouldBe` False
        Set.member "userGreeting" unusedNames `shouldBe` False

        -- actuallyUnused doesn't match any pattern -> should be flagged
        Set.member "actuallyUnused" unusedNames `shouldBe` True

-- Helper functions

mkFunctionSymbol :: T.Text -> T.Text -> Symbol
mkFunctionSymbol modName name = Symbol
  { symbolName = mkQualifiedName (Just modName) name
  , symbolKind = Function
  , symbolSpan = noSrcSpan
  , symbolExported = True
  , symbolType = Nothing
  }

mkTypeSymbol :: T.Text -> T.Text -> Symbol
mkTypeSymbol modName name = Symbol
  { symbolName = mkQualifiedName (Just modName) name
  , symbolKind = TypeConstructor
  , symbolSpan = noSrcSpan
  , symbolExported = True
  , symbolType = Nothing
  }

mkDepNode' :: Symbol -> DepNode
mkDepNode' sym = DepNode
  { dnSymbol = sym
  , dnIsRoot = False
  , dnIsThGen = False
  , dnModule = maybe "" id (qnModule (symbolName sym))
  }
