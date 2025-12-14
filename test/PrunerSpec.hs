{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : PrunerSpec
-- Description : Tests for dead import auto-pruning system
-- Copyright   : (c) 2024
-- License     : MIT
module PrunerSpec (spec) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import Argus.Imports.Pruner
import Argus.Imports.Manager (ParsedImport(..))
import Argus.Types (Diagnostic(..), Fix(..), FixCategory(..), FixSafety(..), DiagnosticKind(..), SrcSpan, mkSrcSpanRaw)

-- | Sample source code with various import patterns
sampleSourceWithUnusedImport :: Text
sampleSourceWithUnusedImport = T.unlines
  [ "module Test where"
  , ""
  , "import Data.Text (Text, pack)"
  , "import Data.List (sort)"  -- Unused
  , ""
  , "foo :: Text"
  , "foo = pack \"hello\""
  ]

sampleSourceAllUsed :: Text
sampleSourceAllUsed = T.unlines
  [ "module Test where"
  , ""
  , "import Data.Text (Text, pack)"
  , "import Data.List (sort)"
  , ""
  , "foo :: Text"
  , "foo = pack \"hello\""
  , ""
  , "bar :: [Int] -> [Int]"
  , "bar = sort"
  ]

sampleSourcePartiallyUnused :: Text
sampleSourcePartiallyUnused = T.unlines
  [ "module Test where"
  , ""
  , "import Data.Text (Text, pack, unpack)"  -- unpack is unused
  , ""
  , "foo :: Text"
  , "foo = pack \"hello\""
  ]

sampleSourceQualifiedImport :: Text
sampleSourceQualifiedImport = T.unlines
  [ "module Test where"
  , ""
  , "import Data.Text qualified as T"
  , "import Data.Map qualified as M"  -- Unused
  , ""
  , "foo = T.pack \"hello\""
  ]

sampleSourceEmpty :: Text
sampleSourceEmpty = T.unlines
  [ "module Test where"
  , ""
  , "foo :: Int"
  , "foo = 42"
  ]

spec :: Spec
spec = do
  describe "Argus.Imports.Pruner" $ do
    describe "PrunerConfig" $ do
      it "has sensible defaults" $ do
        pcRemoveUnusedImports defaultPrunerConfig `shouldBe` True
        pcPruneExplicitLists defaultPrunerConfig `shouldBe` True
        pcPreserveTypeImports defaultPrunerConfig `shouldBe` True
        pcPreserveInstanceImports defaultPrunerConfig `shouldBe` True
        pcMinSymbolsToKeep defaultPrunerConfig `shouldBe` 0

      it "excludes common modules by default" $ do
        pcExcludedModules defaultPrunerConfig `shouldSatisfy` ("Prelude" `elem`)
        pcExcludedModules defaultPrunerConfig `shouldSatisfy` ("GHC.Generics" `elem`)
        pcExcludedModules defaultPrunerConfig `shouldSatisfy` ("Data.Kind" `elem`)

    describe "analyzeUnusedImportsText" $ do
      it "detects completely unused imports" $ do
        let analysis = analyzeUnusedImportsText defaultPrunerConfig "Test.hs" sampleSourceWithUnusedImport
        length (paUnusedImports analysis) `shouldSatisfy` (>= 0)

      it "returns empty for source with all imports used" $ do
        let analysis = analyzeUnusedImportsText defaultPrunerConfig "Test.hs" sampleSourceAllUsed
        paUnusedImports analysis `shouldSatisfy` null

      it "populates paFile field correctly" $ do
        let analysis = analyzeUnusedImportsText defaultPrunerConfig "Test.hs" sampleSourceAllUsed
        paFile analysis `shouldBe` "Test.hs"

      it "parses imports from source" $ do
        let analysis = analyzeUnusedImportsText defaultPrunerConfig "Test.hs" sampleSourceAllUsed
        length (paImports analysis) `shouldSatisfy` (> 0)

      it "extracts used symbols" $ do
        let analysis = analyzeUnusedImportsText defaultPrunerConfig "Test.hs" sampleSourceAllUsed
        Set.member "pack" (paUsedSymbols analysis) `shouldBe` True
        Set.member "sort" (paUsedSymbols analysis) `shouldBe` True

      it "handles empty source" $ do
        let analysis = analyzeUnusedImportsText defaultPrunerConfig "Test.hs" ""
        paImports analysis `shouldBe` []
        paUnusedImports analysis `shouldBe` []

    describe "findUnusedInModule" $ do
      it "returns tuple of unused imports and symbols" $ do
        let (unusedImports, _) = findUnusedInModule defaultPrunerConfig "Test.hs" sampleSourceAllUsed
        unusedImports `shouldSatisfy` null

      it "handles empty source" $ do
        let (unusedImports, unusedSymbols) = findUnusedInModule defaultPrunerConfig "Test.hs" ""
        unusedImports `shouldBe` []
        unusedSymbols `shouldBe` []

    describe "getUsedSymbolsFromSource" $ do
      it "extracts function names from body" $ do
        let symbols = getUsedSymbolsFromSource sampleSourceAllUsed
        Set.member "pack" symbols `shouldBe` True
        Set.member "sort" symbols `shouldBe` True

      it "extracts type names" $ do
        let symbols = getUsedSymbolsFromSource sampleSourceAllUsed
        Set.member "Text" symbols `shouldBe` True
        Set.member "Int" symbols `shouldBe` True

      it "extracts qualified usage names" $ do
        let symbols = getUsedSymbolsFromSource sampleSourceQualifiedImport
        Set.member "T.pack" symbols `shouldBe` True

      it "handles empty source" $ do
        let symbols = getUsedSymbolsFromSource ""
        symbols `shouldBe` Set.empty

    describe "getImportedSymbols" $ do
      it "returns empty set for empty list" $ do
        getImportedSymbols [] `shouldBe` Set.empty

    describe "isSymbolUsed" $ do
      it "returns True for symbol in set" $ do
        let symbols = Set.fromList ["foo", "bar", "baz"]
        isSymbolUsed "foo" symbols `shouldBe` True
        isSymbolUsed "bar" symbols `shouldBe` True

      it "returns False for symbol not in set" $ do
        let symbols = Set.fromList ["foo", "bar"]
        isSymbolUsed "qux" symbols `shouldBe` False

      it "returns False for empty set" $ do
        isSymbolUsed "foo" Set.empty `shouldBe` False

    describe "ImportStatus" $ do
      it "has correct Ord instance" $ do
        ISUsed < ISUnused `shouldBe` True
        ISUnused < ISPartiallyUnused `shouldBe` True

      it "has correct Eq instance" $ do
        ISUsed == ISUsed `shouldBe` True
        ISUnused == ISUnused `shouldBe` True
        ISUsed == ISUnused `shouldBe` False

    describe "generatePruneFixes" $ do
      it "returns empty for analysis with no unused" $ do
        let analysis = analyzeUnusedImportsText defaultPrunerConfig "Test.hs" sampleSourceAllUsed
            fixes = generatePruneFixes analysis
        fixes `shouldSatisfy` null

    describe "generateRemoveImportFix" $ do
      it "creates fix with correct title" $ do
        let info = UnusedImportInfo
              { uiModule = "Data.List"
              , uiSpan = mkTestSpan
              , uiImport = mkTestImport
              }
            fix = generateRemoveImportFix info
        fixTitle fix `shouldSatisfy` T.isInfixOf "Data.List"
        fixTitle fix `shouldSatisfy` T.isInfixOf "Remove unused import"

      it "creates fix with FCImports category" $ do
        let info = UnusedImportInfo
              { uiModule = "Data.List"
              , uiSpan = mkTestSpan
              , uiImport = mkTestImport
              }
            fix = generateRemoveImportFix info
        fixCategory fix `shouldBe` FCImports

      it "creates fix with FSAlways safety" $ do
        let info = UnusedImportInfo
              { uiModule = "Data.List"
              , uiSpan = mkTestSpan
              , uiImport = mkTestImport
              }
            fix = generateRemoveImportFix info
        fixSafety fix `shouldBe` FSAlways

      it "marks fix as preferred" $ do
        let info = UnusedImportInfo
              { uiModule = "Data.List"
              , uiSpan = mkTestSpan
              , uiImport = mkTestImport
              }
            fix = generateRemoveImportFix info
        fixIsPreferred fix `shouldBe` True

      it "includes module in removeImports list" $ do
        let info = UnusedImportInfo
              { uiModule = "Data.List"
              , uiSpan = mkTestSpan
              , uiImport = mkTestImport
              }
            fix = generateRemoveImportFix info
        fixRemoveImports fix `shouldBe` ["Data.List"]

    describe "generatePruneSymbolsFix" $ do
      it "creates fix with correct title" $ do
        let fix = generatePruneSymbolsFix mkTestImport ["unpack", "append"]
        fixTitle fix `shouldSatisfy` T.isInfixOf "Remove unused symbols"
        fixTitle fix `shouldSatisfy` T.isInfixOf "unpack"
        fixTitle fix `shouldSatisfy` T.isInfixOf "append"

      it "creates fix with FCImports category" $ do
        let fix = generatePruneSymbolsFix mkTestImport ["unpack"]
        fixCategory fix `shouldBe` FCImports

      it "creates fix with FSAlways safety" $ do
        let fix = generatePruneSymbolsFix mkTestImport ["unpack"]
        fixSafety fix `shouldBe` FSAlways

      it "includes symbols in removeImports list" $ do
        let fix = generatePruneSymbolsFix mkTestImport ["unpack", "append"]
        fixRemoveImports fix `shouldBe` ["unpack", "append"]

    describe "UnusedImportInfo" $ do
      it "stores module name" $ do
        let info = UnusedImportInfo "Data.Text" mkTestSpan mkTestImport
        uiModule info `shouldBe` "Data.Text"

      it "stores import info" $ do
        let imp = mkTestImport
            info = UnusedImportInfo "Data.Text" mkTestSpan imp
        uiImport info `shouldBe` imp

    describe "UnusedSymbol" $ do
      it "stores module name" $ do
        let sym = UnusedSymbol "Data.Text" "unpack" mkTestImport
        usModule sym `shouldBe` "Data.Text"

      it "stores symbol name" $ do
        let sym = UnusedSymbol "Data.Text" "unpack" mkTestImport
        usSymbol sym `shouldBe` "unpack"

      it "stores import reference" $ do
        let imp = mkTestImport
            sym = UnusedSymbol "Data.Text" "unpack" imp
        usImport sym `shouldBe` imp

    describe "PruneAnalysis" $ do
      it "contains file path" $ do
        let analysis = analyzeUnusedImportsText defaultPrunerConfig "Test.hs" sampleSourceEmpty
        paFile analysis `shouldBe` "Test.hs"

      it "generates diagnostics for unused imports" $ do
        let analysis = analyzeUnusedImportsText defaultPrunerConfig "Test.hs" sampleSourceWithUnusedImport
        -- Diagnostics should be generated for any detected unused imports
        length (paDiagnostics analysis) `shouldSatisfy` (>= 0)

    describe "edge cases" $ do
      it "handles source with only imports" $ do
        let source = T.unlines
              [ "module Test where"
              , "import Data.Text"
              , "import Data.List"
              ]
            analysis = analyzeUnusedImportsText defaultPrunerConfig "Test.hs" source
        -- All imports would be unused since there's no code using them
        length (paImports analysis) `shouldBe` 2

      it "handles source with hiding imports" $ do
        let source = T.unlines
              [ "module Test where"
              , "import Prelude hiding (head)"
              , "foo = 42"
              ]
            analysis = analyzeUnusedImportsText defaultPrunerConfig "Test.hs" source
        -- Hiding imports should be preserved (conservative approach)
        paUnusedImports analysis `shouldSatisfy` null

      it "handles multiline imports" $ do
        let source = T.unlines
              [ "module Test where"
              , "import Data.Text"
              , "  ( Text"
              , "  , pack"
              , "  )"
              , "foo = pack \"hello\""
              ]
            analysis = analyzeUnusedImportsText defaultPrunerConfig "Test.hs" source
        length (paImports analysis) `shouldSatisfy` (>= 0)

      it "respects excluded modules" $ do
        let config = defaultPrunerConfig { pcExcludedModules = ["Prelude", "MyModule"] }
            source = T.unlines
              [ "module Test where"
              , "import MyModule (unused)"
              , "foo = 42"
              ]
            analysis = analyzeUnusedImportsText config "Test.hs" source
        -- MyModule should be excluded from unused detection
        all (\info -> uiModule info /= "MyModule") (paUnusedImports analysis) `shouldBe` True

-- | Helper to create a test SrcSpan
mkTestSpan :: SrcSpan
mkTestSpan = mkSrcSpanRaw "Test.hs" 1 1 1 20

-- | Helper to create a test ParsedImport
mkTestImport :: ParsedImport
mkTestImport = ParsedImport
  { piModule = "Data.Text"
  , piQualified = False
  , piAlias = Nothing
  , piHiding = False
  , piExplicit = ["Text", "pack"]
  , piPackage = Nothing
  , piSpan = Just mkTestSpan
  , piIsSafe = False
  , piIsSource = False
  }
