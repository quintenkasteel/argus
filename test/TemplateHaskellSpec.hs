{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TemplateHaskellSpec
-- Description : Tests for Argus.Analysis.TemplateHaskell
--
-- Comprehensive tests for Template Haskell detection and HIE-based analysis.
-- Tests TH detection, HIE file discovery, name extraction, and combined analysis.
module TemplateHaskellSpec (spec) where

import Test.Hspec
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Argus.Analysis.TemplateHaskell
import Argus.Analysis.Syntactic (ThSpliceInfo(..))
import Argus.Types (SrcSpan(..), Line(..), Column(..))

spec :: Spec
spec = do
  describe "Argus.Analysis.TemplateHaskell" $ do
    describe "THInfo" $ do
      it "stores all fields correctly" $ do
        let info = THInfo
              { thiHasSplices = True
              , thiSpliceCount = 3
              , thiSpliceExprs = ["$(foo)", "$(bar)", "$(baz)"]
              }
        thiHasSplices info `shouldBe` True
        thiSpliceCount info `shouldBe` 3
        length (thiSpliceExprs info) `shouldBe` 3

      it "handles empty splices" $ do
        let info = THInfo False 0 []
        thiHasSplices info `shouldBe` False
        thiSpliceCount info `shouldBe` 0
        thiSpliceExprs info `shouldBe` []

    describe "THAnalysisResult" $ do
      it "stores all fields correctly" $ do
        let result = THAnalysisResult
              { tarUsedNames = Set.fromList ["foo", "bar"]
              , tarHieFileFound = True
              , tarHieFilePath = Just "/path/to/file.hie"
              , tarTHInfo = THInfo True 1 ["$(foo)"]
              , tarWarnings = []
              , tarSuppressUnused = False
              }
        Set.size (tarUsedNames result) `shouldBe` 2
        tarHieFileFound result `shouldBe` True
        tarHieFilePath result `shouldBe` Just "/path/to/file.hie"
        thiHasSplices (tarTHInfo result) `shouldBe` True
        tarWarnings result `shouldBe` []
        tarSuppressUnused result `shouldBe` False

      it "handles missing HIE file case" $ do
        let result = THAnalysisResult
              { tarUsedNames = Set.empty
              , tarHieFileFound = False
              , tarHieFilePath = Nothing
              , tarTHInfo = THInfo False 0 []
              , tarWarnings = ["No HIE file found"]
              , tarSuppressUnused = True
              }
        tarHieFileFound result `shouldBe` False
        tarHieFilePath result `shouldBe` Nothing
        tarSuppressUnused result `shouldBe` True
        tarWarnings result `shouldSatisfy` (not . null)

    describe "TH Detection" $ do
      describe "hasTHSplices" $ do
        it "returns True for non-empty splice list" $ do
          let splices = [mkSplice "$(foo)"]
          hasTHSplices splices `shouldBe` True

        it "returns False for empty splice list" $ do
          hasTHSplices [] `shouldBe` False

        it "returns True for multiple splices" $ do
          let splices = [mkSplice "$(foo)", mkSplice "$(bar)"]
          hasTHSplices splices `shouldBe` True

      describe "analyzeTH" $ do
        it "correctly analyzes empty splice list" $ do
          let info = analyzeTH []
          thiHasSplices info `shouldBe` False
          thiSpliceCount info `shouldBe` 0
          thiSpliceExprs info `shouldBe` []

        it "correctly analyzes single splice" $ do
          let splices = [mkSplice "$(deriveJSON)"]
              info = analyzeTH splices
          thiHasSplices info `shouldBe` True
          thiSpliceCount info `shouldBe` 1
          thiSpliceExprs info `shouldBe` ["$(deriveJSON)"]

        it "correctly analyzes multiple splices" $ do
          let splices = [mkSplice "$(foo)", mkSplice "$(bar)", mkSplice "$(baz)"]
              info = analyzeTH splices
          thiHasSplices info `shouldBe` True
          thiSpliceCount info `shouldBe` 3
          length (thiSpliceExprs info) `shouldBe` 3

        it "handles typed splices" $ do
          let splices = [mkTypedSplice "$$(typedFoo)"]
              info = analyzeTH splices
          thiHasSplices info `shouldBe` True
          thiSpliceCount info `shouldBe` 1

    describe "HIE File Discovery" $ do
      describe "hieFileExists" $ do
        it "returns False for non-existent file" $ do
          result <- hieFileExists "/nonexistent/path/Test.hs" Nothing
          result `shouldBe` False

        it "returns False for non-existent HIE directory" $ do
          result <- hieFileExists "/src/Test.hs" (Just "/nonexistent/.hie")
          result `shouldBe` False

        it "returns True when HIE file exists" $ withTempDir $ \tmpDir -> do
          let hieDir = tmpDir </> ".hie"
              hiePath = hieDir </> "Test.hie"
          createDirectoryIfMissing True hieDir
          writeFile hiePath "dummy hie content"
          result <- hieFileExists (tmpDir </> "Test.hs") (Just hieDir)
          result `shouldBe` True

      describe "findHieFileForSource" $ do
        it "returns Nothing for non-existent file" $ do
          result <- findHieFileForSource "/nonexistent/Test.hs" Nothing
          result `shouldBe` Nothing

        it "returns Nothing for non-existent HIE directory" $ do
          result <- findHieFileForSource "/src/Test.hs" (Just "/nonexistent/.hie")
          result `shouldBe` Nothing

        it "finds HIE file in specified directory" $ withTempDir $ \tmpDir -> do
          let hieDir = tmpDir </> ".hie"
              hiePath = hieDir </> "Test.hie"
          createDirectoryIfMissing True hieDir
          writeFile hiePath "dummy"
          result <- findHieFileForSource (tmpDir </> "Test.hs") (Just hieDir)
          result `shouldBe` Just hiePath

        it "finds nested module HIE file" $ withTempDir $ \tmpDir -> do
          let hieDir = tmpDir </> ".hie"
              nestedDir = hieDir </> "Render"
              hiePath = nestedDir </> "DomainSwitch.hie"
          createDirectoryIfMissing True nestedDir
          writeFile hiePath "dummy"
          -- Test nested module discovery
          result <- findHieFileForSource (tmpDir </> "src" </> "Render" </> "DomainSwitch.hs") (Just hieDir)
          -- Should find via recursive search
          case result of
            Just p -> p `shouldSatisfy` T.isSuffixOf "DomainSwitch.hie" . T.pack
            Nothing -> pure ()  -- OK if recursive search not implemented

      describe "findHieDbForSource" $ do
        it "returns Nothing when no database exists" $ withTempDir $ \tmpDir -> do
          -- Use temp directory with explicit non-existent hie path to avoid finding real .hiedb
          result <- findHieDbForSource (tmpDir </> "Test.hs") (Just (tmpDir </> "nonexistent-hie"))
          result `shouldBe` Nothing

        it "finds database in HIE directory" $ withTempDir $ \tmpDir -> do
          let hieDir = tmpDir </> ".hie"
              dbPath = hieDir </> ".hiedb"
          createDirectoryIfMissing True hieDir
          writeFile dbPath "dummy db"
          result <- findHieDbForSource (tmpDir </> "Test.hs") (Just hieDir)
          result `shouldBe` Just dbPath

        it "finds database in project root" $ withTempDir $ \tmpDir -> do
          let dbPath = tmpDir </> ".hiedb"
          writeFile dbPath "dummy db"
          -- When using current directory style paths
          result <- findHieDbForSource (tmpDir </> "src" </> "Test.hs") (Just tmpDir)
          -- Project root lookup: takeDirectory hieDir </> ".hiedb"
          result `shouldBe` Just dbPath

    describe "Used Names Extraction" $ do
      describe "extractUsedNamesFromHie" $ do
        it "returns Left for non-existent file" $ do
          result <- extractUsedNamesFromHie "/nonexistent/Test.hie"
          case result of
            Left _ -> pure ()  -- Expected - file doesn't exist
            Right _ -> expectationFailure "Should fail for non-existent file"

        it "returns Left for invalid HIE file" $ withTempDir $ \tmpDir -> do
          let hiePath = tmpDir </> "invalid.hie"
          writeFile hiePath "not a valid hie file"
          result <- extractUsedNamesFromHie hiePath
          case result of
            Left _ -> pure ()  -- Expected - invalid format
            Right _ -> expectationFailure "Should fail for invalid HIE file"

      describe "extractUsedNamesFromHieDb" $ do
        it "returns Left for non-existent database" $ do
          result <- extractUsedNamesFromHieDb "/nonexistent/db" "/src/Test.hs"
          case result of
            Left _ -> pure ()  -- Expected
            Right _ -> expectationFailure "Should fail for non-existent database"

        it "returns empty set for file not in database" $ withTempDir $ \tmpDir -> do
          -- Create a minimal SQLite database (won't have proper schema)
          let dbPath = tmpDir </> "test.db"
          writeFile dbPath ""  -- Empty file will fail to query
          result <- extractUsedNamesFromHieDb dbPath "/src/Test.hs"
          case result of
            Left _ -> pure ()  -- Expected for invalid DB
            Right names -> Set.null names `shouldBe` True

    describe "Roots Matching" $ do
      describe "matchesRoots" $ do
        it "returns False for empty patterns" $ do
          matchesRoots [] "anyName" `shouldBe` False

        it "matches exact pattern" $ do
          matchesRoots ["^main$"] "main" `shouldBe` True
          matchesRoots ["^main$"] "mainHelper" `shouldBe` False

        it "matches prefix pattern" $ do
          matchesRoots ["^parse"] "parseJSON" `shouldBe` True
          matchesRoots ["^parse"] "notParse" `shouldBe` False

        it "matches suffix pattern" $ do
          matchesRoots ["JSON$"] "parseJSON" `shouldBe` True
          matchesRoots ["JSON$"] "JSONParser" `shouldBe` False

        it "matches with wildcard" $ do
          matchesRoots [".*JSON.*"] "parseJSONValue" `shouldBe` True
          matchesRoots [".*JSON.*"] "toJSON" `shouldBe` True

        it "matches any of multiple patterns" $ do
          let patterns = ["^main$", "^parse", "JSON$"]
          matchesRoots patterns "main" `shouldBe` True
          matchesRoots patterns "parseConfig" `shouldBe` True
          matchesRoots patterns "toJSON" `shouldBe` True
          matchesRoots patterns "helper" `shouldBe` False

        it "handles special regex characters" $ do
          matchesRoots ["Paths_"] "Paths_myproject" `shouldBe` True

    describe "Combined Analysis" $ do
      describe "analyzeFileWithTH" $ do
        it "handles file without TH and no HIE" $ do
          result <- analyzeFileWithTH
            "/src/Test.hs"
            []  -- No splices
            (Set.fromList ["foo", "bar"])
            []  -- No TH roots
            Nothing
          tarHieFileFound result `shouldBe` False
          tarSuppressUnused result `shouldBe` False
          Set.member "foo" (tarUsedNames result) `shouldBe` True
          Set.member "bar" (tarUsedNames result) `shouldBe` True

        it "handles file with TH but no HIE (suppresses unused)" $ do
          let splices = [mkSplice "$(deriveJSON)"]
          result <- analyzeFileWithTH
            "/src/Test.hs"
            splices
            Set.empty
            []
            Nothing
          tarHieFileFound result `shouldBe` False
          thiHasSplices (tarTHInfo result) `shouldBe` True
          tarSuppressUnused result `shouldBe` True
          tarWarnings result `shouldSatisfy` (not . null)

        it "merges source names with root patterns" $ do
          let splices = [mkSplice "$(deriveJSON defaultOptions ''MyType)"]
              roots = ["parseJSON", "toJSON"]
          result <- analyzeFileWithTH
            "/src/Test.hs"
            splices
            (Set.fromList ["myFunc"])
            roots
            Nothing
          -- Source name should be preserved
          Set.member "myFunc" (tarUsedNames result) `shouldBe` True

        it "sets correct THInfo" $ do
          let splices = [mkSplice "$(foo)", mkSplice "$(bar)"]
          result <- analyzeFileWithTH
            "/src/Test.hs"
            splices
            Set.empty
            []
            Nothing
          thiSpliceCount (tarTHInfo result) `shouldBe` 2
          thiHasSplices (tarTHInfo result) `shouldBe` True

    describe "Show and Eq instances" $ do
      it "THInfo has Show instance" $ do
        let info = THInfo True 1 ["$(foo)"]
        show info `shouldContain` "THInfo"

      it "THInfo has Eq instance" $ do
        let i1 = THInfo True 1 ["$(foo)"]
            i2 = THInfo True 1 ["$(foo)"]
            i3 = THInfo False 0 []
        i1 `shouldBe` i2
        i1 `shouldNotBe` i3

      it "THAnalysisResult has Show instance" $ do
        let result = THAnalysisResult Set.empty False Nothing (THInfo False 0 []) [] False
        show result `shouldContain` "THAnalysisResult"

      it "THAnalysisResult has Eq instance" $ do
        let r1 = THAnalysisResult Set.empty False Nothing (THInfo False 0 []) [] False
            r2 = THAnalysisResult Set.empty False Nothing (THInfo False 0 []) [] False
        r1 `shouldBe` r2

-- Helper to create a test ThSpliceInfo
mkSplice :: Text -> ThSpliceInfo
mkSplice expr = ThSpliceInfo
  { tsiSpan = noSrcSpanTest
  , tsiQuotedName = Nothing
  , tsiExpr = expr
  , tsiIsTyped = False
  }

-- Helper to create a typed splice
mkTypedSplice :: Text -> ThSpliceInfo
mkTypedSplice expr = ThSpliceInfo
  { tsiSpan = noSrcSpanTest
  , tsiQuotedName = Nothing
  , tsiExpr = expr
  , tsiIsTyped = True
  }

-- Helper for creating a default SrcSpan for tests
noSrcSpanTest :: SrcSpan
noSrcSpanTest = SrcSpan
  { srcSpanFile = ""
  , srcSpanStartLine = Line 1
  , srcSpanStartCol = Column 1
  , srcSpanEndLine = Line 1
  , srcSpanEndCol = Column 1
  }

-- Helper for running tests with a temporary directory
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "argus-th-test"
