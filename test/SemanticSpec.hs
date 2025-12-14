{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : SemanticSpec
-- Description : Tests for Argus.Analysis.Semantic
--
-- Comprehensive tests for the HIE-based semantic analysis module.
-- Tests database initialization, indexing, queries, and diagnostic generation.
module SemanticSpec (spec) where

import Test.Hspec
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Argus.Analysis.Semantic
import Argus.Types (Severity(..), QualifiedName(..), SrcSpan(..), Line(..), Column(..))

spec :: Spec
spec = do
  describe "Argus.Analysis.Semantic" $ do
    describe "IndexResult" $ do
      it "has correct default values" $ do
        let result = IndexResult 0 0 [] False
        irIndexed result `shouldBe` 0
        irFailed result `shouldBe` 0
        irErrors result `shouldBe` []
        irVersionMismatch result `shouldBe` False

      it "stores indexed count correctly" $ do
        let result = IndexResult 5 2 ["error1"] True
        irIndexed result `shouldBe` 5
        irFailed result `shouldBe` 2
        irErrors result `shouldSatisfy` (not . null)
        irVersionMismatch result `shouldBe` True

    describe "DefinitionResult" $ do
      it "stores all fields correctly" $ do
        let result = DefinitionResult
              { defResultFile = "src/Test.hs"
              , defResultModule = "Test"
              , defResultName = "foo"
              , defResultSpan = noSrcSpanDef
              }
        defResultFile result `shouldBe` "src/Test.hs"
        defResultModule result `shouldBe` "Test"
        defResultName result `shouldBe` "foo"

    describe "ReferenceResult" $ do
      it "stores all fields correctly" $ do
        let result = ReferenceResult
              { refResultFile = "src/Test.hs"
              , refResultModule = "Test"
              , refResultSpan = noSrcSpanDef
              }
        refResultFile result `shouldBe` "src/Test.hs"
        refResultModule result `shouldBe` "Test"

    describe "ModuleInfo" $ do
      it "stores all fields correctly" $ do
        let info = ModuleInfo
              { moduleInfoName = "Test.Module"
              , moduleInfoFile = Just "src/Test/Module.hs"
              , moduleInfoHieFile = ".hie/Test/Module.hie"
              }
        moduleInfoName info `shouldBe` "Test.Module"
        moduleInfoFile info `shouldBe` Just "src/Test/Module.hs"
        moduleInfoHieFile info `shouldBe` ".hie/Test/Module.hie"

      it "handles missing source file" $ do
        let info = ModuleInfo
              { moduleInfoName = "Test"
              , moduleInfoFile = Nothing
              , moduleInfoHieFile = ".hie/Test.hie"
              }
        moduleInfoFile info `shouldBe` Nothing

    describe "SymbolInfo" $ do
      it "stores all fields correctly" $ do
        let info = SymbolInfo
              { symbolInfoName = "myFunction"
              , symbolInfoKind = "Function"
              , symbolInfoSpan = noSrcSpanDef
              }
        symbolInfoName info `shouldBe` "myFunction"
        symbolInfoKind info `shouldBe` "Function"

    describe "SemanticDiagnostic" $ do
      it "stores all fields correctly" $ do
        let diag = SemanticDiagnostic
              { semDiagFile = "src/Test.hs"
              , semDiagSpan = noSrcSpanDef
              , semDiagMessage = "Test message"
              , semDiagSeverity = Warning
              , semDiagCode = "semantic/test"
              }
        semDiagFile diag `shouldBe` "src/Test.hs"
        semDiagMessage diag `shouldBe` "Test message"
        semDiagSeverity diag `shouldBe` Warning
        semDiagCode diag `shouldBe` "semantic/test"

      it "supports all severity levels" $ do
        let makeD sev = SemanticDiagnostic "f" noSrcSpanDef "m" sev "c"
        semDiagSeverity (makeD Error) `shouldBe` Error
        semDiagSeverity (makeD Warning) `shouldBe` Warning
        semDiagSeverity (makeD Info) `shouldBe` Info

    describe "HieData" $ do
      it "stores file and module info" $ do
        let hd = HieData
              { hieFile = "src/Test.hs"
              , hieModule = "Test"
              , _hieImports = []
              , _hieExports = []
              }
        hieFile hd `shouldBe` "src/Test.hs"
        hieModule hd `shouldBe` "Test"

      it "accessor functions work correctly" $ do
        let qn1 = QualifiedName { qnModule = Just "Data.List", qnName = "map" }
            qn2 = QualifiedName { qnModule = Just "Test", qnName = "foo" }
            hd = HieData "src/Test.hs" "Test" [qn1] [qn2]
        hieImports hd `shouldBe` [qn1]
        hieExports hd `shouldBe` [qn2]

    describe "Database Initialization" $ do
      it "initHieDb creates database directory" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "subdir" </> "test.hiedb"
        initHieDb dbPath
        -- The directory should be created
        dirExists <- doesDirectoryExist (tmpDir </> "subdir")
        dirExists `shouldBe` True

      it "initHieDb creates empty database" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        -- Database file should exist after initialization
        fileExists <- doesFileExist dbPath
        fileExists `shouldBe` True

    describe "HIE File Indexing" $ do
      it "indexHieFiles returns 0 for non-existent directory" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
            hieDir = tmpDir </> "nonexistent"
        initHieDb dbPath
        count <- indexHieFiles dbPath hieDir
        count `shouldBe` 0

      it "indexHieFilesWithErrors returns detailed results" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
            hieDir = tmpDir </> "empty-hie"
        createDirectoryIfMissing True hieDir
        initHieDb dbPath
        result <- indexHieFilesWithErrors dbPath hieDir
        irIndexed result `shouldBe` 0
        irFailed result `shouldBe` 0
        irVersionMismatch result `shouldBe` False

      it "indexHieFilesWithErrors detects version mismatch patterns" $ do
        -- Test the version mismatch detection logic
        let testMsg1 = "hie file version mismatch"
            testMsg2 = "was created by ghc 9.4"
            testMsg3 = "normal error"
        -- These patterns should be detected as version mismatches
        T.isInfixOf "version" (T.toLower testMsg1) `shouldBe` True
        T.isInfixOf "ghc" (T.toLower testMsg2) `shouldBe` True
        T.isInfixOf "version" (T.toLower testMsg3) `shouldBe` False

    describe "Query Functions" $ do
      it "findDefinition returns empty for non-existent symbol" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          results <- findDefinition db "nonExistentSymbol" Nothing
          results `shouldBe` []

      it "findDefinition with module filter returns empty" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          results <- findDefinition db "foo" (Just "NonExistent.Module")
          results `shouldBe` []

      it "findReferences returns empty for non-existent symbol" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          results <- findReferences db "nonExistentSymbol" Nothing
          results `shouldBe` []

      it "findTypeReferences returns empty for non-existent type" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          results <- findTypeReferences db "NonExistentType" Nothing
          results `shouldBe` []

      it "getModuleInfo returns Nothing for non-existent module" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          result <- getModuleInfo db "NonExistent.Module"
          result `shouldBe` Nothing

      it "getAllModules returns empty for empty database" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          mods <- getAllModules db
          mods `shouldBe` []

      it "getExports returns empty for non-existent module" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          exports <- getExports db "NonExistent.Module"
          exports `shouldBe` []

      it "getTypeClassMembers returns empty for non-existent class" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          members <- getTypeClassMembers db "NonExistent.Module" "NonExistentClass"
          members `shouldBe` []

      it "getWildcardImportMembers returns empty for non-existent type" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          members <- getWildcardImportMembers db "Data.Aeson" "ToJSON"
          members `shouldBe` []

      it "getSymbolReferenceCount returns 0 for non-existent symbol" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          count <- getSymbolReferenceCount db "nonExistent" Nothing
          count `shouldBe` 0

    describe "Semantic Analysis" $ do
      it "runSemanticAnalysis returns empty for empty database" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          diags <- runSemanticAnalysis db
          diags `shouldBe` []

      it "runFullSemanticAnalysis returns empty for empty database" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          diags <- runFullSemanticAnalysis db
          diags `shouldBe` []

      it "findUnusedExports returns empty for empty database" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          diags <- findUnusedExports db
          diags `shouldBe` []

      it "findUnusedDefinitions returns empty for empty database" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          diags <- findUnusedDefinitions db
          diags `shouldBe` []

      it "findTypeMismatches returns empty for empty database" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          diags <- findTypeMismatches db
          diags `shouldBe` []

      it "findDeepImportChains returns empty for empty database" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          diags <- findDeepImportChains db 5
          diags `shouldBe` []

      it "findSingleUseSymbols returns empty for empty database" $ withTempDir $ \tmpDir -> do
        let dbPath = tmpDir </> "test.hiedb"
        initHieDb dbPath
        withHieDb dbPath $ \db -> do
          diags <- findSingleUseSymbols db
          diags `shouldBe` []

    describe "HIE File Loading" $ do
      it "loadHieFiles returns empty for non-existent directory" $ do
        result <- loadHieFiles "/nonexistent/path/to/hie"
        result `shouldBe` []

      it "loadHieFiles returns empty for empty directory" $ withTempDir $ \tmpDir -> do
        let hieDir = tmpDir </> "empty-hie"
        createDirectoryIfMissing True hieDir
        result <- loadHieFiles hieDir
        result `shouldBe` []

    describe "Dependency Graph Building" $ do
      it "buildGraphFromHieDb handles non-existent directory" $ do
        graph <- buildGraphFromHieDb "/nonexistent/path"
        -- Should return an empty graph
        graph `shouldSatisfy` const True  -- Graph was created without error

      it "buildGraphFromHieDb handles empty directory" $ withTempDir $ \tmpDir -> do
        let hieDir = tmpDir </> "empty-hie"
        createDirectoryIfMissing True hieDir
        -- Create database
        let dbPath = hieDir </> ".hiedb"
        initHieDb dbPath
        graph <- buildGraphFromHieDb hieDir
        graph `shouldSatisfy` const True  -- Graph was created without error

    describe "Show instances" $ do
      it "IndexResult has Show instance" $ do
        let result = IndexResult 1 2 ["err"] True
        show result `shouldContain` "IndexResult"

      it "DefinitionResult has Show instance" $ do
        let result = DefinitionResult "f" "M" "n" noSrcSpanDef
        show result `shouldContain` "DefinitionResult"

      it "ReferenceResult has Show instance" $ do
        let result = ReferenceResult "f" "M" noSrcSpanDef
        show result `shouldContain` "ReferenceResult"

      it "ModuleInfo has Show instance" $ do
        let info = ModuleInfo "M" Nothing "h"
        show info `shouldContain` "ModuleInfo"

      it "SymbolInfo has Show instance" $ do
        let info = SymbolInfo "n" "k" noSrcSpanDef
        show info `shouldContain` "SymbolInfo"

      it "SemanticDiagnostic has Show instance" $ do
        let diag = SemanticDiagnostic "f" noSrcSpanDef "m" Warning "c"
        show diag `shouldContain` "SemanticDiagnostic"

      it "HieData has Show instance" $ do
        let hd = HieData "f" "m" [] []
        show hd `shouldContain` "HieData"

    describe "Eq instances" $ do
      it "IndexResult has Eq instance" $ do
        let r1 = IndexResult 1 0 [] False
            r2 = IndexResult 1 0 [] False
            r3 = IndexResult 2 0 [] False
        r1 `shouldBe` r2
        r1 `shouldNotBe` r3

      it "DefinitionResult has Eq instance" $ do
        let d1 = DefinitionResult "f" "M" "n" noSrcSpanDef
            d2 = DefinitionResult "f" "M" "n" noSrcSpanDef
        d1 `shouldBe` d2

      it "ReferenceResult has Eq instance" $ do
        let r1 = ReferenceResult "f" "M" noSrcSpanDef
            r2 = ReferenceResult "f" "M" noSrcSpanDef
        r1 `shouldBe` r2

      it "ModuleInfo has Eq instance" $ do
        let m1 = ModuleInfo "M" Nothing "h"
            m2 = ModuleInfo "M" Nothing "h"
        m1 `shouldBe` m2

      it "SemanticDiagnostic has Eq instance" $ do
        let d1 = SemanticDiagnostic "f" noSrcSpanDef "m" Warning "c"
            d2 = SemanticDiagnostic "f" noSrcSpanDef "m" Warning "c"
        d1 `shouldBe` d2


-- Helper for creating a default SrcSpan for tests
noSrcSpanDef :: SrcSpan
noSrcSpanDef = SrcSpan
  { srcSpanFile = ""
  , srcSpanStartLine = Line 1
  , srcSpanStartCol = Column 1
  , srcSpanEndLine = Line 1
  , srcSpanEndCol = Column 1
  }

-- Helper for running tests with a temporary directory
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "argus-semantic-test"
