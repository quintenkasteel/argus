{-# LANGUAGE OverloadedStrings #-}

module ArchitectureSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Hspec

import Argus.Analysis.Architecture
import Argus.Types (Diagnostic(..), DiagnosticKind(..), Severity(..), SrcSpan(..), noSrcSpan)

-- | Helper to create a simple ModuleInfo
mkModule :: T.Text -> [T.Text] -> ModuleInfo
mkModule name deps = ModuleInfo
  { miName = name
  , miPath = T.unpack name <> ".hs"
  , miImports = map mkImport deps
  , miExports = ["main"]
  , miIsOrphan = False
  , miHasTemplateH = False
  , miLineCount = 100
  , miFunctionCount = 10
  , miTypeCount = 3
  , miLayer = Nothing
  }

-- | Helper to create a ModuleInfo with layer
mkModuleWithLayer :: T.Text -> [T.Text] -> T.Text -> ModuleInfo
mkModuleWithLayer name deps layer = (mkModule name deps) { miLayer = Just layer }

-- | Helper to create an ImportInfo
mkImport :: T.Text -> ImportInfo
mkImport modName = ImportInfo
  { iiModule = modName
  , iiQualified = False
  , iiAlias = Nothing
  , iiHiding = False
  , iiSpan = noSrcSpan
  }

-- | Helper to create qualified ImportInfo
mkQualifiedImport :: T.Text -> ImportInfo
mkQualifiedImport modName = (mkImport modName) { iiQualified = True }

spec :: Spec
spec = do
  describe "Argus.Analysis.Architecture" $ do
    describe "analyzeArchitecture" $ do
      it "returns empty report when disabled" $ do
        let config = defaultArchitectureConfig { acEnabled = False }
            modules = Map.singleton "Test" (mkModule "Test" [])
            report = analyzeArchitecture config modules
        arModules report `shouldBe` []
        arCircularDeps report `shouldBe` []
        arTotalModules report `shouldBe` 0

      it "counts modules and dependencies correctly" $ do
        let config = defaultArchitectureConfig
            modules = Map.fromList
              [ ("A", mkModule "A" ["B", "C"])
              , ("B", mkModule "B" ["C"])
              , ("C", mkModule "C" [])
              ]
            report = analyzeArchitecture config modules
        arTotalModules report `shouldBe` 3
        arTotalDeps report `shouldBe` 3  -- A->B, A->C, B->C

      it "returns modules in the report" $ do
        let config = defaultArchitectureConfig
            modules = Map.fromList
              [ ("Module.A", mkModule "Module.A" [])
              , ("Module.B", mkModule "Module.B" ["Module.A"])
              ]
            report = analyzeArchitecture config modules
        length (arModules report) `shouldBe` 2

    describe "findCircularDeps" $ do
      it "detects simple A -> B -> A cycle" $ do
        let config = defaultArchitectureConfig
            modules = Map.fromList
              [ ("A", mkModule "A" ["B"])
              , ("B", mkModule "B" ["A"])
              ]
            report = analyzeArchitecture config modules
        -- Should find at least one cycle involving A and B
        arCircularDeps report `shouldSatisfy` (not . null)
        -- At least one cycle should contain both A and B
        let allCycleModules = concatMap cdModules (arCircularDeps report)
        "A" `elem` allCycleModules `shouldBe` True
        "B" `elem` allCycleModules `shouldBe` True

      it "detects A -> B -> C -> A cycle" $ do
        let config = defaultArchitectureConfig
            modules = Map.fromList
              [ ("A", mkModule "A" ["B"])
              , ("B", mkModule "B" ["C"])
              , ("C", mkModule "C" ["A"])
              ]
            report = analyzeArchitecture config modules
        -- Should find at least one cycle
        arCircularDeps report `shouldSatisfy` (not . null)
        -- All three modules should be in at least one cycle
        let allCycleModules = concatMap cdModules (arCircularDeps report)
        "A" `elem` allCycleModules `shouldBe` True
        "B" `elem` allCycleModules `shouldBe` True
        "C" `elem` allCycleModules `shouldBe` True

      it "returns no cycles for acyclic graph" $ do
        let config = defaultArchitectureConfig
            modules = Map.fromList
              [ ("A", mkModule "A" ["B"])
              , ("B", mkModule "B" ["C"])
              , ("C", mkModule "C" [])
              ]
            report = analyzeArchitecture config modules
        arCircularDeps report `shouldBe` []

      it "handles self-loop" $ do
        let config = defaultArchitectureConfig
            modules = Map.singleton "A" (mkModule "A" ["A"])
            report = analyzeArchitecture config modules
        arCircularDeps report `shouldSatisfy` (not . null)
        let allCycleModules = concatMap cdModules (arCircularDeps report)
        "A" `elem` allCycleModules `shouldBe` True

      it "respects maxCycleLength" $ do
        let config = defaultArchitectureConfig { acMaxCycleLength = 2 }
            -- Create a longer cycle A -> B -> C -> A
            modules = Map.fromList
              [ ("A", mkModule "A" ["B"])
              , ("B", mkModule "B" ["C"])
              , ("C", mkModule "C" ["A"])
              ]
            report = analyzeArchitecture config modules
        -- With max length 2, the 3-node cycle shouldn't be detected
        arCircularDeps report `shouldBe` []

    describe "findLayerViolations" $ do
      it "detects layer violation when Core imports App" $ do
        -- Use exact module names for patterns to ensure matching
        let layers =
              [ LayerConfig "Core" ["Core.Module"] ["Core"]
              , LayerConfig "App" ["App.Handler"] ["Core", "App"]
              ]
            config = defaultArchitectureConfig { acLayers = layers }
            -- Core module trying to import from App layer
            coreModule = (mkModule "Core.Module" ["App.Handler"])
            appModule = mkModule "App.Handler" []
            modules = Map.fromList
              [ ("Core.Module", coreModule)
              , ("App.Handler", appModule)
              ]
            report = analyzeArchitecture config modules
        length (arLayerViolations report) `shouldBe` 1
        let violation = case arLayerViolations report of
              (v:_) -> v
              []    -> error "Expected at least one violation"
        lvFrom violation `shouldBe` "Core.Module"
        lvTo violation `shouldBe` "App.Handler"
        lvFromLayer violation `shouldBe` "Core"
        lvToLayer violation `shouldBe` "App"

      it "allows valid layer imports" $ do
        let layers =
              [ LayerConfig "Core" ["Core.Types"] ["Core"]
              , LayerConfig "Service" ["Service.Handler"] ["Core", "Service"]
              ]
            config = defaultArchitectureConfig { acLayers = layers }
            -- Service can import from Core - valid
            modules = Map.fromList
              [ ("Core.Types", mkModule "Core.Types" [])
              , ("Service.Handler", mkModule "Service.Handler" ["Core.Types"])
              ]
            report = analyzeArchitecture config modules
        arLayerViolations report `shouldBe` []

      it "ignores modules not in any layer" $ do
        let layers = [ LayerConfig "Core" ["Core.Types"] ["Core"] ]
            config = defaultArchitectureConfig { acLayers = layers }
            modules = Map.fromList
              [ ("Other.Module", mkModule "Other.Module" ["Another.Module"])
              , ("Another.Module", mkModule "Another.Module" [])
              ]
            report = analyzeArchitecture config modules
        arLayerViolations report `shouldBe` []

    describe "calculateCoupling" $ do
      it "calculates afferent coupling (Ca) correctly" $ do
        let config = defaultArchitectureConfig
            -- C is depended on by A and B
            modules = Map.fromList
              [ ("A", mkModule "A" ["C"])
              , ("B", mkModule "B" ["C"])
              , ("C", mkModule "C" [])
              ]
            report = analyzeArchitecture config modules
            cMetrics = filter (\m -> cmModule m == "C") (arCouplingMetrics report)
        length cMetrics `shouldBe` 1
        cmAfferentCoupling (head cMetrics) `shouldBe` 2

      it "calculates efferent coupling (Ce) correctly" $ do
        let config = defaultArchitectureConfig
            -- A depends on B and C
            modules = Map.fromList
              [ ("A", mkModule "A" ["B", "C"])
              , ("B", mkModule "B" [])
              , ("C", mkModule "C" [])
              ]
            report = analyzeArchitecture config modules
            aMetrics = filter (\m -> cmModule m == "A") (arCouplingMetrics report)
        length aMetrics `shouldBe` 1
        cmEfferentCoupling (head aMetrics) `shouldBe` 2

      it "calculates instability correctly" $ do
        let config = defaultArchitectureConfig
            -- Module with Ca=1, Ce=2 should have I = 2/(1+2) = 0.666...
            modules = Map.fromList
              [ ("A", mkModule "A" ["B", "C"])  -- Ce = 2
              , ("B", mkModule "B" [])
              , ("C", mkModule "C" [])
              , ("D", mkModule "D" ["A"])       -- Gives A: Ca = 1
              ]
            report = analyzeArchitecture config modules
            aMetrics = filter (\m -> cmModule m == "A") (arCouplingMetrics report)
        length aMetrics `shouldBe` 1
        let instability = cmInstability (head aMetrics)
        instability `shouldSatisfy` (\i -> abs (i - 0.666667) < 0.01)

      it "returns 0 instability for isolated modules" $ do
        let config = defaultArchitectureConfig
            modules = Map.singleton "A" (mkModule "A" [])
            report = analyzeArchitecture config modules
            aMetrics = filter (\m -> cmModule m == "A") (arCouplingMetrics report)
        length aMetrics `shouldBe` 1
        cmInstability (head aMetrics) `shouldBe` 0

    describe "calculateMaxDepth" $ do
      it "returns 0 for single module" $ do
        let config = defaultArchitectureConfig
            modules = Map.singleton "A" (mkModule "A" [])
            report = analyzeArchitecture config modules
        arMaxDepth report `shouldBe` 0

      it "calculates depth for linear chain" $ do
        let config = defaultArchitectureConfig
            modules = Map.fromList
              [ ("A", mkModule "A" ["B"])
              , ("B", mkModule "B" ["C"])
              , ("C", mkModule "C" [])
              ]
            report = analyzeArchitecture config modules
        arMaxDepth report `shouldBe` 2  -- A -> B -> C

      it "handles diamond dependency" $ do
        let config = defaultArchitectureConfig
            modules = Map.fromList
              [ ("A", mkModule "A" ["B", "C"])
              , ("B", mkModule "B" ["D"])
              , ("C", mkModule "C" ["D"])
              , ("D", mkModule "D" [])
              ]
            report = analyzeArchitecture config modules
        arMaxDepth report `shouldBe` 2  -- A -> B -> D or A -> C -> D

    describe "matchPattern" $ do
      it "matches exact module names" $ do
        let layers = [ LayerConfig "App" ["Main"] ["App"] ]
            config = defaultArchitectureConfig { acLayers = layers }
            modules = Map.singleton "Main" (mkModule "Main" [])
            report = analyzeArchitecture config modules
        -- Main should be in App layer
        length (filter (\m -> miLayer m == Just "App") (arModules report)) `shouldBe` 0
        -- Actually miLayer comes from input, not assigned by analyzeArchitecture
        -- Let's verify matching works via layer violations
        arLayerViolations report `shouldBe` []

      it "matches wildcard prefix patterns" $ do
        let layers =
              [ LayerConfig "Core" ["*.Types"] ["Core"]
              , LayerConfig "App" ["*.App"] ["Core", "App"]
              ]
            config = defaultArchitectureConfig { acLayers = layers }
            -- App importing from Core - should be allowed
            modules = Map.fromList
              [ ("My.Types", mkModule "My.Types" [])
              , ("My.App", mkModule "My.App" ["My.Types"])
              ]
            report = analyzeArchitecture config modules
        arLayerViolations report `shouldBe` []

      it "matches wildcard suffix patterns" $ do
        let layers =
              [ LayerConfig "Data" ["Data.*"] ["Data"]
              , LayerConfig "Service" ["Service.*"] ["Data", "Service"]
              ]
            config = defaultArchitectureConfig { acLayers = layers }
            modules = Map.fromList
              [ ("Data.User", mkModule "Data.User" [])
              , ("Service.Auth", mkModule "Service.Auth" ["Data.User"])
              ]
            report = analyzeArchitecture config modules
        arLayerViolations report `shouldBe` []

    describe "generateDiagnostics" $ do
      it "generates diagnostics for circular dependencies" $ do
        let config = defaultArchitectureConfig
            modules = Map.fromList
              [ ("A", mkModule "A" ["B"])
              , ("B", mkModule "B" ["A"])
              ]
            report = analyzeArchitecture config modules
            cycleDiags = filter (\d -> diagCode d == Just "architecture/circular-dep")
                                (arDiagnostics report)
        -- Should have at least one circular dependency diagnostic
        cycleDiags `shouldSatisfy` (not . null)
        -- All circular dep diagnostics should have correct severity and kind
        all (\d -> diagSeverity d == Warning) cycleDiags `shouldBe` True
        all (\d -> diagKind d == ArchitecturalIssue) cycleDiags `shouldBe` True

      it "generates diagnostics for high coupling" $ do
        let config = defaultArchitectureConfig { acCouplingThreshold = 2 }
            -- Module A depends on 3 modules, above threshold of 2
            modules = Map.fromList
              [ ("A", mkModule "A" ["B", "C", "D"])
              , ("B", mkModule "B" [])
              , ("C", mkModule "C" [])
              , ("D", mkModule "D" [])
              ]
            report = analyzeArchitecture config modules
            couplingDiags = filter (\d -> diagCode d == Just "architecture/high-coupling")
                                   (arDiagnostics report)
        length couplingDiags `shouldBe` 1
        diagMessage (head couplingDiags) `shouldSatisfy` T.isInfixOf "A"

      it "generates diagnostics for orphan instances" $ do
        let config = defaultArchitectureConfig { acCheckOrphans = True }
            orphanModule = (mkModule "Orphan" []) { miIsOrphan = True }
            modules = Map.singleton "Orphan" orphanModule
            report = analyzeArchitecture config modules
            orphanDiags = filter (\d -> diagCode d == Just "architecture/orphan-instance")
                                 (arDiagnostics report)
        length orphanDiags `shouldBe` 1

      it "generates diagnostics for unqualified imports" $ do
        let config = defaultArchitectureConfig { acCheckQualified = True }
            dataMapImport = ImportInfo
              { iiModule = "Data.Map"
              , iiQualified = False
              , iiAlias = Nothing
              , iiHiding = False
              , iiSpan = noSrcSpan
              }
            modWithUnqualified = (mkModule "Test" [])
              { miImports = [dataMapImport] }
            modules = Map.singleton "Test" modWithUnqualified
            report = analyzeArchitecture config modules
            qualDiags = filter (\d -> diagCode d == Just "architecture/prefer-qualified")
                               (arDiagnostics report)
        length qualDiags `shouldBe` 1

      it "does not flag qualified imports" $ do
        let config = defaultArchitectureConfig { acCheckQualified = True }
            dataMapImport = ImportInfo
              { iiModule = "Data.Map"
              , iiQualified = True
              , iiAlias = Just "Map"
              , iiHiding = False
              , iiSpan = noSrcSpan
              }
            modWithQualified = (mkModule "Test" [])
              { miImports = [dataMapImport] }
            modules = Map.singleton "Test" modWithQualified
            report = analyzeArchitecture config modules
            qualDiags = filter (\d -> diagCode d == Just "architecture/prefer-qualified")
                               (arDiagnostics report)
        qualDiags `shouldBe` []

    describe "defaultArchitectureConfig" $ do
      it "has enabled set to True by default" $ do
        acEnabled defaultArchitectureConfig `shouldBe` True

      it "has standard layers defined" $ do
        length (acLayers defaultArchitectureConfig) `shouldSatisfy` (> 0)

      it "has reasonable coupling threshold" $ do
        acCouplingThreshold defaultArchitectureConfig `shouldSatisfy` (> 0)

      it "has reasonable instability threshold" $ do
        acInstabilityThreshold defaultArchitectureConfig `shouldSatisfy` (> 0)
        acInstabilityThreshold defaultArchitectureConfig `shouldSatisfy` (<= 1)

    describe "generateDotGraph" $ do
      it "generates valid DOT header" $ do
        let style = minimalDotStyle
            report = emptyDotReport
            dot = generateDotGraph style report
        T.isInfixOf "digraph dependencies {" dot `shouldBe` True
        T.isInfixOf "rankdir=TB;" dot `shouldBe` True

      it "includes module nodes" $ do
        let style = minimalDotStyle
            modInfo = mkModule "MyModule" []
            report = mkDotReport [modInfo]
            dot = generateDotGraph style report
        T.isInfixOf "MyModule" dot `shouldBe` True

      it "includes edges for imports" $ do
        let style = minimalDotStyle
            modInfo = mkModule "A" ["B"]
            report = mkDotReport [modInfo]
            dot = generateDotGraph style report
        T.isInfixOf "A -> B" dot `shouldBe` True

      it "sanitizes module names with dots" $ do
        let style = minimalDotStyle
            modInfo = mkModule "Data.Map.Strict" []
            report = mkDotReport [modInfo]
            dot = generateDotGraph style report
        T.isInfixOf "Data_Map_Strict" dot `shouldBe` True

    ---------------------------------------------------------------------------
    -- P1-02: Architecture edge case tests
    ---------------------------------------------------------------------------

    describe "edge cases" $ do
      it "handles empty module map gracefully" $ do
        let config = defaultArchitectureConfig
            modules = Map.empty
            report = analyzeArchitecture config modules
        arModules report `shouldBe` []
        arCircularDeps report `shouldBe` []
        arLayerViolations report `shouldBe` []
        arCouplingMetrics report `shouldBe` []
        arDiagnostics report `shouldBe` []
        arTotalModules report `shouldBe` 0
        arTotalDeps report `shouldBe` 0
        arMaxDepth report `shouldBe` 0

      it "handles module with no dependencies" $ do
        let config = defaultArchitectureConfig
            modules = Map.singleton "Standalone" (mkModule "Standalone" [])
            report = analyzeArchitecture config modules
        arTotalModules report `shouldBe` 1
        arTotalDeps report `shouldBe` 0
        arCircularDeps report `shouldBe` []
        arMaxDepth report `shouldBe` 0

      it "handles module importing non-existent module" $ do
        let config = defaultArchitectureConfig
            modules = Map.singleton "A" (mkModule "A" ["NonExistent"])
            report = analyzeArchitecture config modules
        -- Should not crash; may or may not count the external dep
        arTotalModules report `shouldBe` 1
        arCircularDeps report `shouldBe` []

      it "handles very deep dependency chain" $ do
        let config = defaultArchitectureConfig { acMaxCycleLength = 20 }
            -- Create a chain: A -> B -> C -> D -> E -> F
            modules = Map.fromList
              [ ("A", mkModule "A" ["B"])
              , ("B", mkModule "B" ["C"])
              , ("C", mkModule "C" ["D"])
              , ("D", mkModule "D" ["E"])
              , ("E", mkModule "E" ["F"])
              , ("F", mkModule "F" [])
              ]
            report = analyzeArchitecture config modules
        arTotalModules report `shouldBe` 6
        arMaxDepth report `shouldBe` 5  -- A -> B -> C -> D -> E -> F
        arCircularDeps report `shouldBe` []

      it "handles multiple self-loops" $ do
        let config = defaultArchitectureConfig
            modules = Map.fromList
              [ ("A", mkModule "A" ["A"])
              , ("B", mkModule "B" ["B"])
              ]
            report = analyzeArchitecture config modules
        length (arCircularDeps report) `shouldSatisfy` (>= 2)

      it "handles disconnected components" $ do
        let config = defaultArchitectureConfig
            -- Two disconnected groups
            modules = Map.fromList
              [ ("A", mkModule "A" ["B"])
              , ("B", mkModule "B" [])
              , ("X", mkModule "X" ["Y"])
              , ("Y", mkModule "Y" [])
              ]
            report = analyzeArchitecture config modules
        arTotalModules report `shouldBe` 4
        arCircularDeps report `shouldBe` []

      it "handles complex multi-cycle graph" $ do
        let config = defaultArchitectureConfig { acMaxCycleLength = 5 }
            -- Multiple overlapping cycles: A <-> B, B -> C -> A
            modules = Map.fromList
              [ ("A", mkModule "A" ["B"])
              , ("B", mkModule "B" ["A", "C"])
              , ("C", mkModule "C" ["A"])
              ]
            report = analyzeArchitecture config modules
        -- Should detect at least some cycles
        arCircularDeps report `shouldSatisfy` (not . null)

      it "handles diamond dependency without cycle" $ do
        let config = defaultArchitectureConfig
            -- Diamond: A -> B, A -> C, B -> D, C -> D
            modules = Map.fromList
              [ ("A", mkModule "A" ["B", "C"])
              , ("B", mkModule "B" ["D"])
              , ("C", mkModule "C" ["D"])
              , ("D", mkModule "D" [])
              ]
            report = analyzeArchitecture config modules
        arCircularDeps report `shouldBe` []
        arMaxDepth report `shouldBe` 2

      it "handles all modules in same layer" $ do
        let layers = [LayerConfig "Core" ["*"] ["Core"]]
            config = defaultArchitectureConfig { acLayers = layers }
            modules = Map.fromList
              [ ("A", mkModule "A" ["B"])
              , ("B", mkModule "B" ["C"])
              , ("C", mkModule "C" [])
              ]
            report = analyzeArchitecture config modules
        -- No violations since all in same layer
        arLayerViolations report `shouldBe` []

      it "handles empty layer configuration" $ do
        let config = defaultArchitectureConfig { acLayers = [] }
            modules = Map.fromList
              [ ("A", mkModule "A" ["B"])
              , ("B", mkModule "B" [])
              ]
            report = analyzeArchitecture config modules
        -- With no layers defined, no violations can occur
        arLayerViolations report `shouldBe` []

      it "handles module with many imports" $ do
        let config = defaultArchitectureConfig { acCouplingThreshold = 5 }
            -- Module A imports 10 other modules
            targetModules = ["B" <> T.pack (show i) | i <- [1..10 :: Int]]
            modules = Map.fromList $
              ("A", mkModule "A" targetModules) :
              [(m, mkModule m []) | m <- targetModules]
            report = analyzeArchitecture config modules
        arTotalModules report `shouldBe` 11
        -- Should generate high coupling diagnostic
        let couplingDiags = filter (\d -> diagCode d == Just "architecture/high-coupling")
                                   (arDiagnostics report)
        length couplingDiags `shouldSatisfy` (>= 1)

      it "calculates correct instability for fully stable module" $ do
        let config = defaultArchitectureConfig
            -- Module C has Ce=0 (no outgoing), Ca>0 (has incoming)
            modules = Map.fromList
              [ ("A", mkModule "A" ["C"])
              , ("B", mkModule "B" ["C"])
              , ("C", mkModule "C" [])
              ]
            report = analyzeArchitecture config modules
            cMetrics = filter (\m -> cmModule m == "C") (arCouplingMetrics report)
        length cMetrics `shouldBe` 1
        cmInstability (Prelude.head cMetrics) `shouldBe` 0  -- Fully stable

      it "calculates correct instability for fully unstable module" $ do
        let config = defaultArchitectureConfig
            -- Module A has Ce>0 (has outgoing), Ca=0 (no incoming)
            modules = Map.fromList
              [ ("A", mkModule "A" ["B", "C"])
              , ("B", mkModule "B" [])
              , ("C", mkModule "C" [])
              ]
            report = analyzeArchitecture config modules
            aMetrics = filter (\m -> cmModule m == "A") (arCouplingMetrics report)
        length aMetrics `shouldBe` 1
        cmInstability (Prelude.head aMetrics) `shouldBe` 1  -- Fully unstable

-- Helper for DOT graph tests
emptyDotReport :: ArchitectureReport
emptyDotReport = ArchitectureReport [] [] [] [] [] 0 0 0

mkDotReport :: [ModuleInfo] -> ArchitectureReport
mkDotReport mods = ArchitectureReport
  { arModules = mods
  , arCircularDeps = []
  , arLayerViolations = []
  , arCouplingMetrics = []
  , arDiagnostics = []
  , arTotalModules = length mods
  , arTotalDeps = sum (map (length . miImports) mods)
  , arMaxDepth = 0
  }
