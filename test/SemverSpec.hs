{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : SemverSpec
-- Description : Tests for Argus.Analysis.Semver
--
-- Comprehensive tests for semantic versioning detection.
-- Tests API extraction, comparison, and diagnostic generation.
module SemverSpec (spec) where

import Test.Hspec
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

import Argus.Analysis.Semver
import Argus.Types (Diagnostic(..), Severity(..), DiagnosticKind(..))

spec :: Spec
spec = do
  describe "Argus.Analysis.Semver" $ do
    describe "APISignature" $ do
      it "creates empty API signature" $ do
        let api = emptyAPISignature "Data.MyLib"
        apiModule api `shouldBe` "Data.MyLib"
        apiVersion api `shouldBe` Nothing
        Map.null (apiFunctions api) `shouldBe` True
        Map.null (apiTypes api) `shouldBe` True
        Map.null (apiClasses api) `shouldBe` True

      it "stores function signatures" $ do
        let funcSig = FunctionSig
              { fsType = "Int -> String"
              , fsConstraints = []
              , fsParamCount = 1
              , fsReturnType = "String"
              }
            api = (emptyAPISignature "Test")
              { apiFunctions = Map.singleton "foo" funcSig }
        Map.lookup "foo" (apiFunctions api) `shouldBe` Just funcSig

      it "stores type signatures" $ do
        let typeSig = TypeSig
              { tsKind = "*"
              , tsConstructors = ["A", "B", "C"]
              , tsFields = []
              , tsIsNewtype = False
              }
            api = (emptyAPISignature "Test")
              { apiTypes = Map.singleton "MyType" typeSig }
        Map.lookup "MyType" (apiTypes api) `shouldBe` Just typeSig

      it "stores class signatures" $ do
        let classSig = ClassSig
              { csMethods = [("foo", "a -> a"), ("bar", "a -> Bool")]
              , csConstraints = ["Show a"]
              , csAssocTypes = []
              }
            api = (emptyAPISignature "Test")
              { apiClasses = Map.singleton "MyClass" classSig }
        Map.lookup "MyClass" (apiClasses api) `shouldBe` Just classSig

    describe "SymbolSignature" $ do
      it "FunctionSig has correct fields" $ do
        let sig = FunctionSig
              { fsType = "a -> b -> c"
              , fsConstraints = ["Ord a", "Show b"]
              , fsParamCount = 2
              , fsReturnType = "c"
              }
        fsType sig `shouldBe` "a -> b -> c"
        fsConstraints sig `shouldBe` ["Ord a", "Show b"]
        fsParamCount sig `shouldBe` 2
        fsReturnType sig `shouldBe` "c"

      it "TypeSig has correct fields" $ do
        let sig = TypeSig
              { tsKind = "* -> *"
              , tsConstructors = ["Nil", "Cons"]
              , tsFields = ["head", "tail"]
              , tsIsNewtype = False
              }
        tsKind sig `shouldBe` "* -> *"
        tsConstructors sig `shouldBe` ["Nil", "Cons"]
        tsFields sig `shouldBe` ["head", "tail"]
        tsIsNewtype sig `shouldBe` False

      it "ClassSig has correct fields" $ do
        let sig = ClassSig
              { csMethods = [("method1", "a -> b"), ("method2", "b -> a")]
              , csConstraints = ["Eq a", "Show b"]
              , csAssocTypes = ["AssocType"]
              }
        csMethods sig `shouldBe` [("method1", "a -> b"), ("method2", "b -> a")]
        csConstraints sig `shouldBe` ["Eq a", "Show b"]
        csAssocTypes sig `shouldBe` ["AssocType"]

      it "PatternSig has correct fields" $ do
        let sig = PatternSig { psType = "MyType a" }
        psType sig `shouldBe` "MyType a"

      it "TypeFamilySig has correct fields" $ do
        let sig = TypeFamilySig { tfsKind = "* -> *", tfsInjective = True }
        tfsKind sig `shouldBe` "* -> *"
        tfsInjective sig `shouldBe` True

    describe "APIChange" $ do
      it "detects symbol addition" $ do
        let change = APIChange
              { acSymbol = "newFunction"
              , acKind = SymbolAdded
              , acOldSignature = Nothing
              , acNewSignature = Just $ FunctionSig "Int -> Int" [] 1 "Int"
              , acBreaking = Nothing
              }
        acSymbol change `shouldBe` "newFunction"
        acKind change `shouldBe` SymbolAdded
        isBreakingChange change `shouldBe` False

      it "detects symbol removal as breaking" $ do
        let breaking = BreakingChange
              { bcReason = "Symbol removed"
              , bcMitigation = Just "Use alternative function"
              , bcSeverity = BreakingMajor
              }
            change = APIChange
              { acSymbol = "oldFunction"
              , acKind = SymbolRemoved
              , acOldSignature = Just $ FunctionSig "Int -> Int" [] 1 "Int"
              , acNewSignature = Nothing
              , acBreaking = Just breaking
              }
        isBreakingChange change `shouldBe` True
        acKind change `shouldBe` SymbolRemoved

      it "detects signature change" $ do
        let oldSig = FunctionSig "Int -> String" [] 1 "String"
            newSig = FunctionSig "Int -> Text" [] 1 "Text"
            change = APIChange
              { acSymbol = "convert"
              , acKind = SignatureChanged
              , acOldSignature = Just oldSig
              , acNewSignature = Just newSig
              , acBreaking = Nothing
              }
        acKind change `shouldBe` SignatureChanged
        acOldSignature change `shouldBe` Just oldSig
        acNewSignature change `shouldBe` Just newSig

    describe "BreakingChange" $ do
      it "has correct severity levels" $ do
        BreakingMinor < BreakingMajor `shouldBe` True
        BreakingMajor < BreakingCritical `shouldBe` True

      it "stores reason and mitigation" $ do
        let bc = BreakingChange
              { bcReason = "Parameter removed"
              , bcMitigation = Just "Add default value"
              , bcSeverity = BreakingMajor
              }
        bcReason bc `shouldBe` "Parameter removed"
        bcMitigation bc `shouldBe` Just "Add default value"
        bcSeverity bc `shouldBe` BreakingMajor

    describe "SemverBump" $ do
      it "has correct ordering" $ do
        PatchBump < MinorBump `shouldBe` True
        MinorBump < MajorBump `shouldBe` True

      it "can be compared" $ do
        maximum [PatchBump, MinorBump, PatchBump] `shouldBe` MinorBump
        maximum [MajorBump, MinorBump, PatchBump] `shouldBe` MajorBump

    describe "compareAPIs" $ do
      it "detects no changes when APIs are identical" $ do
        let api = (emptyAPISignature "Test")
              { apiFunctions = Map.singleton "foo" (FunctionSig "Int -> Int" [] 1 "Int") }
        compareAPIs api api `shouldBe` []

      it "detects added functions" $ do
        let oldAPI = emptyAPISignature "Test"
            newAPI = (emptyAPISignature "Test")
              { apiFunctions = Map.singleton "newFunc" (FunctionSig "String -> String" [] 1 "String") }
            changes = compareAPIs oldAPI newAPI
        length changes `shouldBe` 1
        acKind (head changes) `shouldBe` SymbolAdded
        acSymbol (head changes) `shouldBe` "newFunc"

      it "detects removed functions as breaking" $ do
        let oldAPI = (emptyAPISignature "Test")
              { apiFunctions = Map.singleton "oldFunc" (FunctionSig "Int -> Int" [] 1 "Int") }
            newAPI = emptyAPISignature "Test"
            changes = compareAPIs oldAPI newAPI
        length changes `shouldBe` 1
        acKind (head changes) `shouldBe` SymbolRemoved
        isBreakingChange (head changes) `shouldBe` True

      it "detects removed type constructors as breaking" $ do
        let oldType = TypeSig "*" ["A", "B", "C"] [] False
            newType = TypeSig "*" ["A", "B"] [] False
            oldAPI = (emptyAPISignature "Test") { apiTypes = Map.singleton "MyType" oldType }
            newAPI = (emptyAPISignature "Test") { apiTypes = Map.singleton "MyType" newType }
            changes = compareAPIs oldAPI newAPI
        length changes `shouldBe` 1
        acKind (head changes) `shouldBe` ConstructorRemoved
        isBreakingChange (head changes) `shouldBe` True

      it "detects added type constructors as non-breaking" $ do
        let oldType = TypeSig "*" ["A", "B"] [] False
            newType = TypeSig "*" ["A", "B", "C"] [] False
            oldAPI = (emptyAPISignature "Test") { apiTypes = Map.singleton "MyType" oldType }
            newAPI = (emptyAPISignature "Test") { apiTypes = Map.singleton "MyType" newType }
            changes = compareAPIs oldAPI newAPI
        length changes `shouldBe` 1
        acKind (head changes) `shouldBe` ConstructorAdded
        isBreakingChange (head changes) `shouldBe` False

      it "detects parameter count changes as breaking" $ do
        let oldFunc = FunctionSig "Int -> String" [] 1 "String"
            newFunc = FunctionSig "Int -> Bool -> String" [] 2 "String"
            oldAPI = (emptyAPISignature "Test") { apiFunctions = Map.singleton "foo" oldFunc }
            newAPI = (emptyAPISignature "Test") { apiFunctions = Map.singleton "foo" newFunc }
            changes = compareAPIs oldAPI newAPI
        length changes `shouldBe` 1
        acKind (head changes) `shouldBe` ParameterCountChanged
        isBreakingChange (head changes) `shouldBe` True

      it "detects added constraints as breaking" $ do
        let oldFunc = FunctionSig "a -> a" [] 1 "a"
            newFunc = FunctionSig "a -> a" ["Ord a"] 1 "a"
            oldAPI = (emptyAPISignature "Test") { apiFunctions = Map.singleton "foo" oldFunc }
            newAPI = (emptyAPISignature "Test") { apiFunctions = Map.singleton "foo" newFunc }
            changes = compareAPIs oldAPI newAPI
        length changes `shouldBe` 1
        acKind (head changes) `shouldBe` ConstraintAdded
        isBreakingChange (head changes) `shouldBe` True

      it "detects removed constraints as non-breaking" $ do
        let oldFunc = FunctionSig "a -> a" ["Ord a", "Show a"] 1 "a"
            newFunc = FunctionSig "a -> a" ["Ord a"] 1 "a"
            oldAPI = (emptyAPISignature "Test") { apiFunctions = Map.singleton "foo" oldFunc }
            newAPI = (emptyAPISignature "Test") { apiFunctions = Map.singleton "foo" newFunc }
            changes = compareAPIs oldAPI newAPI
        length changes `shouldBe` 1
        acKind (head changes) `shouldBe` ConstraintRemoved
        isBreakingChange (head changes) `shouldBe` False

      it "detects removed class methods as breaking" $ do
        let oldClass = ClassSig [("foo", "a -> a"), ("bar", "a -> Bool")] [] []
            newClass = ClassSig [("foo", "a -> a")] [] []
            oldAPI = (emptyAPISignature "Test") { apiClasses = Map.singleton "MyClass" oldClass }
            newAPI = (emptyAPISignature "Test") { apiClasses = Map.singleton "MyClass" newClass }
            changes = compareAPIs oldAPI newAPI
        length changes `shouldBe` 1
        acKind (head changes) `shouldBe` MethodRemoved
        isBreakingChange (head changes) `shouldBe` True

      it "detects added class methods as non-breaking" $ do
        let oldClass = ClassSig [("foo", "a -> a")] [] []
            newClass = ClassSig [("foo", "a -> a"), ("bar", "a -> Bool")] [] []
            oldAPI = (emptyAPISignature "Test") { apiClasses = Map.singleton "MyClass" oldClass }
            newAPI = (emptyAPISignature "Test") { apiClasses = Map.singleton "MyClass" newClass }
            changes = compareAPIs oldAPI newAPI
        length changes `shouldBe` 1
        acKind (head changes) `shouldBe` MethodAdded
        isBreakingChange (head changes) `shouldBe` False

    describe "classifyChange" $ do
      it "classifies symbol addition as minor bump" $ do
        let change = APIChange "foo" SymbolAdded Nothing (Just $ FunctionSig "" [] 0 "") Nothing
        classifyChange change `shouldBe` MinorBump

      it "classifies symbol removal as major bump" $ do
        let breaking = BreakingChange "Removed" Nothing BreakingMajor
            change = APIChange "foo" SymbolRemoved (Just $ FunctionSig "" [] 0 "") Nothing (Just breaking)
        classifyChange change `shouldBe` MajorBump

      it "classifies constraint addition as major bump" $ do
        let breaking = BreakingChange "Constraint added" Nothing BreakingMinor
            change = APIChange "foo" ConstraintAdded (Just $ FunctionSig "" [] 0 "") (Just $ FunctionSig "" ["Ord a"] 0 "") (Just breaking)
        classifyChange change `shouldBe` MajorBump

      it "classifies constraint removal as minor bump" $ do
        let change = APIChange "foo" ConstraintRemoved (Just $ FunctionSig "" ["Ord a"] 0 "") (Just $ FunctionSig "" [] 0 "") Nothing
        classifyChange change `shouldBe` MinorBump

    describe "classifyChanges" $ do
      it "returns patch bump for empty changes" $ do
        classifyChanges [] `shouldBe` PatchBump

      it "returns minor bump for additions only" $ do
        let changes =
              [ APIChange "foo" SymbolAdded Nothing (Just $ FunctionSig "" [] 0 "") Nothing
              , APIChange "bar" ConstructorAdded Nothing (Just $ TypeSig "" [] [] False) Nothing
              ]
        classifyChanges changes `shouldBe` MinorBump

      it "returns major bump when any breaking change exists" $ do
        let breaking = BreakingChange "Removed" Nothing BreakingMajor
            changes =
              [ APIChange "foo" SymbolAdded Nothing (Just $ FunctionSig "" [] 0 "") Nothing
              , APIChange "bar" SymbolRemoved (Just $ FunctionSig "" [] 0 "") Nothing (Just breaking)
              ]
        classifyChanges changes `shouldBe` MajorBump

    describe "summarizeChanges" $ do
      it "counts zero changes correctly" $ do
        summarizeChanges [] `shouldBe` (0, 0, 0)

      it "counts additions correctly" $ do
        let changes =
              [ APIChange "foo" SymbolAdded Nothing Nothing Nothing
              , APIChange "bar" SymbolAdded Nothing Nothing Nothing
              ]
        let (breaking, additions, other) = summarizeChanges changes
        breaking `shouldBe` 0
        additions `shouldBe` 2
        other `shouldBe` 0

      it "counts breaking changes correctly" $ do
        let bc = BreakingChange "test" Nothing BreakingMajor
            changes =
              [ APIChange "foo" SymbolRemoved Nothing Nothing (Just bc)
              , APIChange "bar" SymbolRemoved Nothing Nothing (Just bc)
              ]
        let (breaking, additions, other) = summarizeChanges changes
        breaking `shouldBe` 2
        additions `shouldBe` 0
        other `shouldBe` 0

      it "counts mixed changes correctly" $ do
        let bc = BreakingChange "test" Nothing BreakingMajor
            changes =
              [ APIChange "foo" SymbolRemoved Nothing Nothing (Just bc)
              , APIChange "bar" SymbolAdded Nothing Nothing Nothing
              , APIChange "baz" ConstraintRemoved Nothing Nothing Nothing
              ]
        let (breaking, additions, other) = summarizeChanges changes
        breaking `shouldBe` 1
        additions `shouldBe` 1
        other `shouldBe` 1

    describe "extractExportedTypes" $ do
      it "extracts all exported types" $ do
        let typeSig = TypeSig "*" ["A"] [] False
            api = (emptyAPISignature "Test")
              { apiTypes = Map.fromList [("Type1", typeSig), ("Type2", typeSig)] }
        length (extractExportedTypes api) `shouldBe` 2

      it "returns empty list when no types" $ do
        let api = emptyAPISignature "Test"
        extractExportedTypes api `shouldBe` []

    describe "extractExportedFunctions" $ do
      it "extracts all exported functions" $ do
        let funcSig = FunctionSig "Int -> Int" [] 1 "Int"
            api = (emptyAPISignature "Test")
              { apiFunctions = Map.fromList [("foo", funcSig), ("bar", funcSig)] }
        length (extractExportedFunctions api) `shouldBe` 2

      it "returns empty list when no functions" $ do
        let api = emptyAPISignature "Test"
        extractExportedFunctions api `shouldBe` []

    describe "extractExportedClasses" $ do
      it "extracts all exported classes" $ do
        let classSig = ClassSig [] [] []
            api = (emptyAPISignature "Test")
              { apiClasses = Map.fromList [("Class1", classSig), ("Class2", classSig)] }
        length (extractExportedClasses api) `shouldBe` 2

      it "returns empty list when no classes" $ do
        let api = emptyAPISignature "Test"
        extractExportedClasses api `shouldBe` []

    describe "extractDataConstructors" $ do
      it "extracts constructors from type signature" $ do
        let sig = TypeSig "*" ["A", "B", "C"] [] False
        extractDataConstructors sig `shouldBe` ["A", "B", "C"]

      it "returns empty list for function signature" $ do
        let sig = FunctionSig "Int -> Int" [] 1 "Int"
        extractDataConstructors sig `shouldBe` []

    describe "extractClassMethods" $ do
      it "extracts methods from class signature" $ do
        let sig = ClassSig [("foo", "a -> a"), ("bar", "a -> Bool")] [] []
        extractClassMethods sig `shouldBe` [("foo", "a -> a"), ("bar", "a -> Bool")]

      it "returns empty list for function signature" $ do
        let sig = FunctionSig "Int -> Int" [] 1 "Int"
        extractClassMethods sig `shouldBe` []

    describe "apiChangesToDiagnostics" $ do
      it "generates diagnostic for breaking change" $ do
        let bc = BreakingChange "Symbol removed" Nothing BreakingMajor
            change = APIChange "foo" SymbolRemoved (Just $ FunctionSig "" [] 0 "") Nothing (Just bc)
            diags = apiChangesToDiagnostics "MyModule" [change]
        length diags `shouldBe` 1
        diagSeverity (head diags) `shouldBe` Error
        diagKind (head diags) `shouldBe` ArchitecturalIssue
        diagCode (head diags) `shouldBe` Just "semver/removed-symbol"

      it "generates info diagnostic for addition" $ do
        let change = APIChange "foo" SymbolAdded Nothing (Just $ FunctionSig "" [] 0 "") Nothing
            diags = apiChangesToDiagnostics "MyModule" [change]
        length diags `shouldBe` 1
        diagSeverity (head diags) `shouldBe` Info
        diagCode (head diags) `shouldBe` Just "semver/symbol-added"

      it "generates appropriate codes for different change kinds" $ do
        let bc = BreakingChange "test" Nothing BreakingMajor
            changes =
              [ APIChange "a" SymbolRemoved Nothing Nothing (Just bc)
              , APIChange "b" ParameterCountChanged Nothing Nothing (Just bc)
              , APIChange "c" ConstructorRemoved Nothing Nothing (Just bc)
              , APIChange "d" MethodRemoved Nothing Nothing (Just bc)
              ]
            diags = apiChangesToDiagnostics "Test" changes
            codes = map diagCode diags
        codes `shouldContain` [Just "semver/removed-symbol"]
        codes `shouldContain` [Just "semver/parameter-count-changed"]
        codes `shouldContain` [Just "semver/constructor-removed"]
        codes `shouldContain` [Just "semver/method-removed"]

    describe "suggestVersionBump" $ do
      it "suggests patch for no changes" $ do
        suggestVersionBump [] `shouldSatisfy` T.isPrefixOf "PATCH"

      it "suggests minor for additions" $ do
        let changes = [APIChange "foo" SymbolAdded Nothing Nothing Nothing]
        suggestVersionBump changes `shouldSatisfy` T.isPrefixOf "MINOR"

      it "suggests major for breaking changes" $ do
        let bc = BreakingChange "test" Nothing BreakingMajor
            changes = [APIChange "foo" SymbolRemoved Nothing Nothing (Just bc)]
        suggestVersionBump changes `shouldSatisfy` T.isPrefixOf "MAJOR"

      it "includes change counts in major bump suggestion" $ do
        let bc = BreakingChange "test" Nothing BreakingMajor
            changes =
              [ APIChange "removed" SymbolRemoved Nothing Nothing (Just bc)
              , APIChange "added" SymbolAdded Nothing Nothing Nothing
              ]
            suggestion = suggestVersionBump changes
        suggestion `shouldSatisfy` T.isInfixOf "1 breaking"
        suggestion `shouldSatisfy` T.isInfixOf "1 addition"

    describe "ChangeKind" $ do
      it "has all expected variants" $ do
        let kinds = [minBound .. maxBound] :: [ChangeKind]
        kinds `shouldContain` [SymbolAdded, SymbolRemoved, SignatureChanged]
        kinds `shouldContain` [ConstraintAdded, ConstraintRemoved]
        kinds `shouldContain` [ConstructorAdded, ConstructorRemoved]
        kinds `shouldContain` [MethodAdded, MethodRemoved]
        kinds `shouldContain` [ParameterCountChanged, ReturnTypeChanged]

      it "can be compared for equality" $ do
        SymbolAdded `shouldBe` SymbolAdded
        SymbolAdded `shouldNotBe` SymbolRemoved

    describe "compareSymbolSignatures" $ do
      it "detects signature type change as critical breaking" $ do
        let oldSig = FunctionSig "Int -> Int" [] 1 "Int"
            newSig = TypeSig "*" [] [] False
            change = compareSymbolSignatures "foo" oldSig newSig
        isBreakingChange change `shouldBe` True
        case acBreaking change of
          Just bc -> bcSeverity bc `shouldBe` BreakingCritical
          Nothing -> expectationFailure "Expected breaking change"

      it "detects function type changes" $ do
        let oldSig = FunctionSig "Int -> String" [] 1 "String"
            newSig = FunctionSig "Int -> Text" [] 1 "Text"
            change = compareSymbolSignatures "convert" oldSig newSig
        acKind change `shouldBe` SignatureChanged
        isBreakingChange change `shouldBe` True
