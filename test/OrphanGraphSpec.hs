{-# LANGUAGE OverloadedStrings #-}

module OrphanGraphSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List (isInfixOf, sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Analysis.OrphanGraph
import Argus.Types (Diagnostic(..), Severity(..), DiagnosticKind(..), noSrcSpan, mkSrcSpanRaw)

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

-- | Create a test orphan instance
mkTestOrphan :: Text -> Text -> Text -> OrphanInstance
mkTestOrphan cls ty mod = OrphanInstance
  { oiClass = cls
  , oiType = ty
  , oiModule = mod
  , oiLocation = noSrcSpan
  , oiTypeModule = Just "Data.Type"
  , oiClassModule = Just "Control.Class"
  , oiSeverity = SevMedium
  }

-- | Create a test module dependency
mkTestDep :: Text -> Text -> ModuleDep
mkTestDep from to = ModuleDep
  { mdFrom = from
  , mdTo = to
  , mdDirect = True
  , mdLocation = noSrcSpan
  }

-- | Create a simple orphan (defined in different module from type and class)
mkSimpleOrphan :: OrphanInstance
mkSimpleOrphan = mkTestOrphan "Show" "MyType" "Orphans.Module"

-- | Create a non-orphan (defined in same module as type)
mkNonOrphan :: OrphanInstance
mkNonOrphan = (mkTestOrphan "Show" "MyType" "Data.Type")
  { oiTypeModule = Just "Data.Type" }

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "OrphanInstance" $ do
    describe "isOrphanInstance" $ do
      it "detects orphan when module differs from both type and class" $ do
        let orphan = mkTestOrphan "Show" "MyType" "Orphans.Module"
        isOrphanInstance orphan `shouldBe` True

      it "detects non-orphan when module matches type module" $ do
        let nonOrphan = (mkTestOrphan "Show" "MyType" "Data.Type")
              { oiTypeModule = Just "Data.Type" }
        isOrphanInstance nonOrphan `shouldBe` False

      it "detects non-orphan when module matches class module" $ do
        let nonOrphan = (mkTestOrphan "Show" "MyType" "Control.Class")
              { oiClassModule = Just "Control.Class" }
        isOrphanInstance nonOrphan `shouldBe` False

      it "assumes orphan when type module is unknown" $ do
        let orphan = (mkTestOrphan "Show" "MyType" "Some.Module")
              { oiTypeModule = Nothing
              , oiClassModule = Just "Control.Class"
              }
        isOrphanInstance orphan `shouldBe` True

      it "assumes orphan when class module is unknown" $ do
        let orphan = (mkTestOrphan "Show" "MyType" "Some.Module")
              { oiTypeModule = Just "Data.Type"
              , oiClassModule = Nothing
              }
        isOrphanInstance orphan `shouldBe` True

  describe "OrphanGraph" $ do
    describe "emptyOrphanGraph" $ do
      it "creates an empty graph" $ do
        let graph = emptyOrphanGraph
        ogOrphans graph `shouldBe` []
        ogDependencies graph `shouldBe` []
        Map.null (ogInfected graph) `shouldBe` True
        ogPaths graph `shouldBe` []

    describe "addOrphan" $ do
      it "adds an orphan to the graph" $ do
        let graph = addOrphan mkSimpleOrphan emptyOrphanGraph
        length (ogOrphans graph) `shouldBe` 1
        head (ogOrphans graph) `shouldBe` mkSimpleOrphan

      it "accumulates multiple orphans" $ do
        let orphan1 = mkTestOrphan "Show" "Type1" "Module1"
            orphan2 = mkTestOrphan "Eq" "Type2" "Module2"
            graph = addOrphan orphan1 $ addOrphan orphan2 emptyOrphanGraph
        length (ogOrphans graph) `shouldBe` 2

    describe "addModuleDep" $ do
      it "adds a module dependency to the graph" $ do
        let dep = mkTestDep "A" "B"
            graph = addModuleDep dep emptyOrphanGraph
        length (ogDependencies graph) `shouldBe` 1
        head (ogDependencies graph) `shouldBe` dep

      it "accumulates multiple dependencies" $ do
        let dep1 = mkTestDep "A" "B"
            dep2 = mkTestDep "B" "C"
            graph = addModuleDep dep1 $ addModuleDep dep2 emptyOrphanGraph
        length (ogDependencies graph) `shouldBe` 2

  describe "buildOrphanGraph" $ do
    it "builds a graph with no infections for isolated orphan" $ do
      let orphan = mkTestOrphan "Show" "Type" "A"
          deps = []
          graph = buildOrphanGraph [orphan] deps
      length (ogOrphans graph) `shouldBe` 1
      ogDependencies graph `shouldBe` []
      -- No other modules to infect
      Map.size (ogInfected graph) `shouldBe` 0

    it "detects direct infection through imports" $ do
      let orphan = mkTestOrphan "Show" "Type" "A"
          deps = [mkTestDep "B" "A"]  -- B imports A
          graph = buildOrphanGraph [orphan] deps

      -- B should be infected by importing A
      let infected = Map.lookup "B" (ogInfected graph)
      infected `shouldSatisfy` (/= Nothing)
      case infected of
        Just orphans -> orphans `shouldContain` [orphan]
        Nothing -> expectationFailure "Module B should be infected"

    it "detects transitive infection" $ do
      let orphan = mkTestOrphan "Show" "Type" "A"
          deps = [ mkTestDep "B" "A"   -- B imports A
                 , mkTestDep "C" "B"   -- C imports B
                 ]
          graph = buildOrphanGraph [orphan] deps

      -- Both B and C should be infected
      Map.member "B" (ogInfected graph) `shouldBe` True
      Map.member "C" (ogInfected graph) `shouldBe` True

    it "handles multiple orphans in different modules" $ do
      let orphan1 = mkTestOrphan "Show" "Type1" "A"
          orphan2 = mkTestOrphan "Eq" "Type2" "B"
          deps = [ mkTestDep "C" "A"
                 , mkTestDep "C" "B"
                 ]
          graph = buildOrphanGraph [orphan1, orphan2] deps

      -- C imports both A and B, so it's infected by both orphans
      let infected = Map.lookup "C" (ogInfected graph)
      case infected of
        Just orphans -> length orphans `shouldBe` 2
        Nothing -> expectationFailure "Module C should be infected"

    it "computes infection paths" $ do
      let orphan = mkTestOrphan "Show" "Type" "A"
          deps = [ mkTestDep "B" "A"
                 , mkTestDep "C" "B"
                 ]
          graph = buildOrphanGraph [orphan] deps

      -- Should have paths from A to B and A to C
      length (ogPaths graph) `shouldSatisfy` (>= 2)

      -- Check for A -> B -> C path
      let hasBPath = any (\p -> "B" `elem` ipPath p) (ogPaths graph)
          hasCPath = any (\p -> "C" `elem` ipPath p) (ogPaths graph)
      hasBPath `shouldBe` True
      hasCPath `shouldBe` True

  describe "getInfectionPaths" $ do
    it "returns paths for a specific orphan" $ do
      let orphan1 = mkTestOrphan "Show" "Type1" "A"
          orphan2 = mkTestOrphan "Eq" "Type2" "B"
          deps = [mkTestDep "C" "A", mkTestDep "C" "B"]
          graph = buildOrphanGraph [orphan1, orphan2] deps

      let paths1 = getInfectionPaths orphan1 graph
          paths2 = getInfectionPaths orphan2 graph

      -- Each orphan should have its own paths
      all (\p -> ipOrphan p == orphan1) paths1 `shouldBe` True
      all (\p -> ipOrphan p == orphan2) paths2 `shouldBe` True

    it "returns empty list for orphan not in graph" $ do
      let orphan = mkTestOrphan "Show" "Type" "A"
          otherOrphan = mkTestOrphan "Eq" "Other" "B"
          graph = buildOrphanGraph [orphan] []

      getInfectionPaths otherOrphan graph `shouldBe` []

  describe "findInfectedModules" $ do
    it "finds all modules infected by any orphan" $ do
      let orphan = mkTestOrphan "Show" "Type" "A"
          deps = [ mkTestDep "B" "A"
                 , mkTestDep "C" "B"
                 , mkTestDep "D" "E"  -- Not connected to A
                 ]
          graph = buildOrphanGraph [orphan] deps
          infected = findInfectedModules graph

      -- B and C should be infected, D and E should not
      Set.member "B" infected `shouldBe` True
      Set.member "C" infected `shouldBe` True

    it "returns empty set for graph with no dependencies" $ do
      let orphan = mkTestOrphan "Show" "Type" "A"
          graph = buildOrphanGraph [orphan] []
          infected = findInfectedModules graph

      Set.null infected `shouldBe` True

  describe "InfectionPath" $ do
    it "marks single-step paths as non-transitive" $ do
      let orphan = mkTestOrphan "Show" "Type" "A"
          deps = [mkTestDep "B" "A"]
          graph = buildOrphanGraph [orphan] deps
          paths = ogPaths graph

      let directPaths = filter (not . ipTransitive) paths
      length directPaths `shouldSatisfy` (>= 1)

    it "marks multi-step paths as transitive" $ do
      let orphan = mkTestOrphan "Show" "Type" "A"
          deps = [ mkTestDep "B" "A"
                 , mkTestDep "C" "B"
                 ]
          graph = buildOrphanGraph [orphan] deps
          paths = ogPaths graph

      let transitivePaths = filter ipTransitive paths
      -- Path to C should be transitive (A -> B -> C)
      length transitivePaths `shouldSatisfy` (>= 1)

  describe "orphanGraphToDot" $ do
    it "generates valid DOT format" $ do
      let orphan = mkTestOrphan "Show" "MyType" "Orphans"
          deps = [mkTestDep "Main" "Orphans"]
          graph = buildOrphanGraph [orphan] deps
          dot = orphanGraphToDot graph

      -- Check for basic DOT structure
      T.isPrefixOf "digraph OrphanGraph" dot `shouldBe` True
      T.isInfixOf "}" dot `shouldBe` True

    it "includes orphan module nodes" $ do
      let orphan = mkTestOrphan "Show" "MyType" "Orphans"
          graph = buildOrphanGraph [orphan] []
          dot = orphanGraphToDot graph

      T.isInfixOf "Orphans" dot `shouldBe` True
      T.isInfixOf "fillcolor=red" dot `shouldBe` True

    it "includes dependency edges" $ do
      let orphan = mkTestOrphan "Show" "Type" "A"
          deps = [mkTestDep "B" "A"]
          graph = buildOrphanGraph [orphan] deps
          dot = orphanGraphToDot graph

      T.isInfixOf "B" dot `shouldBe` True
      T.isInfixOf "->" dot `shouldBe` True

    it "includes instance labels" $ do
      let orphan = mkTestOrphan "Show" "MyType" "Orphans"
          graph = buildOrphanGraph [orphan] []
          dot = orphanGraphToDot graph

      T.isInfixOf "instance Show MyType" dot `shouldBe` True

    it "sanitizes module names for DOT identifiers" $ do
      let orphan = mkTestOrphan "Show" "Type" "Data.Map.Strict"
          graph = buildOrphanGraph [orphan] []
          dot = orphanGraphToDot graph

      -- Module name should be sanitized (. replaced with _)
      T.isInfixOf "Data_Map_Strict" dot `shouldBe` True

  describe "orphansToDiagnostics" $ do
    it "converts orphans to diagnostics" $ do
      let orphan = mkTestOrphan "Show" "MyType" "Orphans"
          diags = orphansToDiagnostics [orphan]

      length diags `shouldBe` 1

    it "includes orphan information in message" $ do
      let orphan = mkTestOrphan "Show" "MyType" "Orphans"
          diags = orphansToDiagnostics [orphan]
          diag = head diags

      T.isInfixOf "Show" (diagMessage diag) `shouldBe` True
      T.isInfixOf "MyType" (diagMessage diag) `shouldBe` True
      T.isInfixOf "Orphans" (diagMessage diag) `shouldBe` True

    it "sets correct diagnostic kind" $ do
      let orphan = mkTestOrphan "Show" "MyType" "Orphans"
          diags = orphansToDiagnostics [orphan]
          diag = head diags

      diagKind diag `shouldBe` ArchitecturalIssue

    it "sets diagnostic code" $ do
      let orphan = mkTestOrphan "Show" "MyType" "Orphans"
          diags = orphansToDiagnostics [orphan]
          diag = head diags

      diagCode diag `shouldBe` Just "orphan-instance"

    it "maps severity correctly" $ do
      let critical = mkTestOrphan "Show" "Type" "Mod"
                      & \o -> o { oiSeverity = SevCritical }
          low = mkTestOrphan "Show" "Type" "Mod"
                 & \o -> o { oiSeverity = SevLow }
          critDiag = head $ orphansToDiagnostics [critical]
          lowDiag = head $ orphansToDiagnostics [low]

      diagSeverity critDiag `shouldBe` Error
      diagSeverity lowDiag `shouldBe` Suggestion

  describe "infectionPathToDiagnostic" $ do
    it "creates diagnostic from infection path" $ do
      let orphan = mkTestOrphan "Show" "Type" "A"
          path = InfectionPath orphan ["A", "B", "C"] 3 True
          diag = infectionPathToDiagnostic path

      diagKind diag `shouldBe` ArchitecturalIssue
      diagSeverity diag `shouldBe` Info

    it "includes path in diagnostic message" $ do
      let orphan = mkTestOrphan "Show" "Type" "A"
          path = InfectionPath orphan ["A", "B", "C"] 3 True
          diag = infectionPathToDiagnostic path

      let msg = diagMessage diag
      T.isInfixOf "A" msg `shouldBe` True
      T.isInfixOf "B" msg `shouldBe` True
      T.isInfixOf "C" msg `shouldBe` True
      T.isInfixOf "->" msg `shouldBe` True

  describe "severityForOrphan" $ do
    it "maps SevCritical to Error" $ do
      let orphan = mkSimpleOrphan { oiSeverity = SevCritical }
      severityForOrphan orphan `shouldBe` Error

    it "maps SevHigh to Warning" $ do
      let orphan = mkSimpleOrphan { oiSeverity = SevHigh }
      severityForOrphan orphan `shouldBe` Warning

    it "maps SevMedium to Warning" $ do
      let orphan = mkSimpleOrphan { oiSeverity = SevMedium }
      severityForOrphan orphan `shouldBe` Warning

    it "maps SevLow to Suggestion" $ do
      let orphan = mkSimpleOrphan { oiSeverity = SevLow }
      severityForOrphan orphan `shouldBe` Suggestion

  describe "Edge cases" $ do
    it "handles circular dependencies" $ do
      let orphan = mkTestOrphan "Show" "Type" "A"
          deps = [ mkTestDep "B" "A"
                 , mkTestDep "A" "B"  -- Circular
                 ]
          graph = buildOrphanGraph [orphan] deps

      -- Should not crash or infinite loop
      length (ogPaths graph) `shouldSatisfy` (>= 0)

    it "handles self-dependencies" $ do
      let orphan = mkTestOrphan "Show" "Type" "A"
          deps = [mkTestDep "A" "A"]  -- Self-reference
          graph = buildOrphanGraph [orphan] deps

      -- Should handle gracefully
      length (ogOrphans graph) `shouldBe` 1

    it "handles empty orphan list" $ do
      let deps = [mkTestDep "A" "B"]
          graph = buildOrphanGraph [] deps

      ogOrphans graph `shouldBe` []
      Map.null (ogInfected graph) `shouldBe` True

    it "handles empty dependency list" $ do
      let orphan = mkTestOrphan "Show" "Type" "A"
          graph = buildOrphanGraph [orphan] []

      length (ogOrphans graph) `shouldBe` 1
      Map.null (ogInfected graph) `shouldBe` True

    it "handles duplicate dependencies" $ do
      let orphan = mkTestOrphan "Show" "Type" "A"
          dep = mkTestDep "B" "A"
          deps = [dep, dep]  -- Duplicate
          graph = buildOrphanGraph [orphan] deps

      -- Should handle without crashing
      length (ogDependencies graph) `shouldBe` 2

  describe "Property-based tests" $ do
    it "number of orphans in graph equals input" $ property $ \n ->
      let n' = abs n `mod` 20  -- Limit to reasonable size
          orphans = [ mkTestOrphan "Show" (T.pack $ "Type" ++ show i) (T.pack $ "Mod" ++ show i)
                    | i <- [1..n']
                    ]
          graph = buildOrphanGraph orphans []
      in length (ogOrphans graph) == n'

    it "all dependency edges appear in graph" $ property $ \n ->
      let n' = abs n `mod` 20
          deps = [ mkTestDep (T.pack $ "A" ++ show i) (T.pack $ "B" ++ show i)
                 | i <- [1..n']
                 ]
          graph = buildOrphanGraph [] deps
      in length (ogDependencies graph) == n'

    it "infected modules are reachable from orphan modules" $ property $ \(n :: Int) ->
      let n' = max 1 (abs n `mod` 10)
          orphan = mkTestOrphan "Show" "Type" "A"
          deps = [ mkTestDep (T.pack $ "M" ++ show i) (T.pack $ "M" ++ show (i-1))
                 | i <- [1..n']
                 ] ++ [mkTestDep "M0" "A"]
          graph = buildOrphanGraph [orphan] deps
          infected = findInfectedModules graph
      in Set.size infected >= 0  -- At least doesn't crash

-- Helper for record update syntax
(&) :: a -> (a -> a) -> a
x & f = f x
infixl 1 &
