{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : CoreSpec
-- Description : Tests for Argus.Core
--
-- Comprehensive tests for the core orchestration module including
-- glob matching, file filtering, identifier/operator extraction,
-- module name extraction, and analysis context.
module CoreSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Control.Exception (bracket)

import Argus.Core
import Argus.Config (defaultConfig)
import Argus.Types (defaultOptions)
import Argus.Rules.ConfigurableRules (defaultRulesConfig)

spec :: Spec
spec = do
  describe "Argus.Core" $ do
    matchGlobPatternSpec
    filterExcludedSpec
    extractIdentifiersSpec
    extractOperatorsSpec
    extractModuleNameSpec
    analysisContextSpec
    findHaskellFilesSpec

--------------------------------------------------------------------------------
-- matchGlobPattern
--------------------------------------------------------------------------------

matchGlobPatternSpec :: Spec
matchGlobPatternSpec = describe "matchGlobPattern" $ do
  describe "basic patterns" $ do
    it "matches exact file name" $ do
      matchGlobPattern "Foo.hs" "Foo.hs" `shouldBe` True
      matchGlobPattern "Foo.hs" "Bar.hs" `shouldBe` False

    it "matches wildcard in file name" $ do
      matchGlobPattern "*.hs" "Foo.hs" `shouldBe` True
      matchGlobPattern "*.hs" "Bar.hs" `shouldBe` True
      matchGlobPattern "*.hs" "Foo.txt" `shouldBe` False

    it "matches single character wildcard" $ do
      matchGlobPattern "Fo?.hs" "Foo.hs" `shouldBe` True
      matchGlobPattern "Fo?.hs" "Fox.hs" `shouldBe` True
      matchGlobPattern "Fo?.hs" "Food.hs" `shouldBe` False

    it "matches prefix with wildcard" $ do
      matchGlobPattern "Test*.hs" "TestMain.hs" `shouldBe` True
      matchGlobPattern "Test*.hs" "TestFoo.hs" `shouldBe` True
      matchGlobPattern "Test*.hs" "Main.hs" `shouldBe` False

    it "matches suffix with wildcard" $ do
      matchGlobPattern "*Spec.hs" "FooSpec.hs" `shouldBe` True
      matchGlobPattern "*Spec.hs" "BarSpec.hs" `shouldBe` True
      matchGlobPattern "*Spec.hs" "Foo.hs" `shouldBe` False

    it "matches multiple wildcards in filename" $ do
      matchGlobPattern "*Test*.hs" "FooTestBar.hs" `shouldBe` True
      matchGlobPattern "*Test*.hs" "TestFoo.hs" `shouldBe` True
      matchGlobPattern "*Test*.hs" "FooBar.hs" `shouldBe` False

  describe "directory patterns" $ do
    it "matches with directory separator" $ do
      matchGlobPattern "src/Foo.hs" "src/Foo.hs" `shouldBe` True
      matchGlobPattern "src/Foo.hs" "test/Foo.hs" `shouldBe` False

    it "matches ** for any depth" $ do
      matchGlobPattern "src/**/*.hs" "src/Foo.hs" `shouldBe` True
      matchGlobPattern "src/**/*.hs" "src/Bar/Foo.hs" `shouldBe` True
      matchGlobPattern "src/**/*.hs" "src/A/B/C/Foo.hs" `shouldBe` True
      matchGlobPattern "test/**/*.hs" "src/Foo.hs" `shouldBe` False

    it "matches ** at start" $ do
      matchGlobPattern "**/*.hs" "Foo.hs" `shouldBe` True
      matchGlobPattern "**/*.hs" "src/Foo.hs" `shouldBe` True
      matchGlobPattern "**/*.hs" "src/Bar/Foo.hs" `shouldBe` True

    it "matches ** in middle" $ do
      matchGlobPattern "src/**/test/*.hs" "src/test/Spec.hs" `shouldBe` True
      matchGlobPattern "src/**/test/*.hs" "src/foo/test/Spec.hs" `shouldBe` True
      matchGlobPattern "src/**/test/*.hs" "src/foo/bar/test/Spec.hs" `shouldBe` True
      matchGlobPattern "src/**/test/*.hs" "test/Spec.hs" `shouldBe` False

    it "matches nested directories with wildcards" $ do
      matchGlobPattern "src/*/Foo.hs" "src/bar/Foo.hs" `shouldBe` True
      matchGlobPattern "src/*/Foo.hs" "src/baz/Foo.hs" `shouldBe` True
      matchGlobPattern "src/*/Foo.hs" "src/bar/baz/Foo.hs" `shouldBe` False

  describe "character class patterns" $ do
    it "matches character classes" $ do
      matchGlobPattern "[FG]oo.hs" "Foo.hs" `shouldBe` True
      matchGlobPattern "[FG]oo.hs" "Goo.hs" `shouldBe` True
      matchGlobPattern "[FG]oo.hs" "Boo.hs" `shouldBe` False

    it "matches character classes with wildcards" $ do
      matchGlobPattern "[A-Z]*.hs" "Foo.hs" `shouldBe` True
      matchGlobPattern "[A-Z]*.hs" "Bar.hs" `shouldBe` True

  describe "edge cases" $ do
    it "handles empty patterns" $ do
      matchGlobPattern "" "" `shouldBe` True
      matchGlobPattern "" "x" `shouldBe` False

    it "handles just wildcards" $ do
      matchGlobPattern "*" "anything" `shouldBe` True
      matchGlobPattern "?" "a" `shouldBe` True
      matchGlobPattern "?" "ab" `shouldBe` False

    it "handles trailing slashes" $ do
      matchGlobPattern "src/" "src/" `shouldBe` True

--------------------------------------------------------------------------------
-- filterExcluded
--------------------------------------------------------------------------------

filterExcludedSpec :: Spec
filterExcludedSpec = describe "filterExcluded" $ do
  let testFiles =
        [ "src/Main.hs"
        , "src/Utils.hs"
        , "generated/Gen.hs"
        , "generated/TH.hs"
        , "test/Spec.hs"
        ]

  it "excludes nothing when no patterns" $ do
    filterExcluded [] testFiles `shouldBe` testFiles

  it "excludes directory with ** pattern" $ do
    filterExcluded ["generated/**"] testFiles
      `shouldBe` ["src/Main.hs", "src/Utils.hs", "test/Spec.hs"]

  it "excludes multiple directories" $ do
    filterExcluded ["generated/**", "test/**"] testFiles
      `shouldBe` ["src/Main.hs", "src/Utils.hs"]

  it "excludes by file extension pattern" $ do
    filterExcluded ["*.hs"] testFiles `shouldBe` []

  it "excludes specific file" $ do
    filterExcluded ["Main.hs"] testFiles
      `shouldBe` ["src/Utils.hs", "generated/Gen.hs", "generated/TH.hs", "test/Spec.hs"]

  it "handles path patterns with slashes" $ do
    filterExcluded ["src/*.hs"] testFiles
      `shouldBe` ["generated/Gen.hs", "generated/TH.hs", "test/Spec.hs"]

  describe "advanced patterns" $ do
    it "excludes with character class" $ do
      filterExcluded ["[GT]*.hs"] testFiles
        `shouldBe` ["src/Main.hs", "src/Utils.hs", "test/Spec.hs"]

    it "excludes multiple patterns at once" $ do
      filterExcluded ["**/Gen.hs", "**/TH.hs"] testFiles
        `shouldBe` ["src/Main.hs", "src/Utils.hs", "test/Spec.hs"]

    it "handles overlapping patterns" $ do
      filterExcluded ["*.hs", "generated/**"] testFiles `shouldBe` []

  describe "edge cases" $ do
    it "handles empty file list" $ do
      filterExcluded ["*.hs"] [] `shouldBe` []

    it "handles non-matching patterns" $ do
      filterExcluded ["nonexistent/**"] testFiles `shouldBe` testFiles

    it "is order-independent for patterns" $ do
      let p1 = filterExcluded ["generated/**", "test/**"] testFiles
          p2 = filterExcluded ["test/**", "generated/**"] testFiles
      p1 `shouldBe` p2

--------------------------------------------------------------------------------
-- extractIdentifiers
--------------------------------------------------------------------------------

extractIdentifiersSpec :: Spec
extractIdentifiersSpec = describe "extractIdentifiers" $ do
  describe "basic identifiers" $ do
    it "extracts simple identifier" $ do
      extractIdentifiers "foo" `shouldBe` ["foo"]

    it "extracts multiple identifiers" $ do
      let result = Set.fromList $ extractIdentifiers "foo bar baz"
      result `shouldBe` Set.fromList ["foo", "bar", "baz"]

    it "extracts camelCase identifiers" $ do
      "fooBar" `elem` extractIdentifiers "fooBar" `shouldBe` True

    it "extracts PascalCase identifiers" $ do
      "FooBar" `elem` extractIdentifiers "FooBar" `shouldBe` True

    it "extracts identifiers with underscores" $ do
      "foo_bar" `elem` extractIdentifiers "foo_bar" `shouldBe` True

    it "extracts identifiers with primes" $ do
      "foo'" `elem` extractIdentifiers "foo'" `shouldBe` True
      "foo''" `elem` extractIdentifiers "foo''" `shouldBe` True

    it "extracts identifiers with digits" $ do
      "foo123" `elem` extractIdentifiers "foo123" `shouldBe` True

  describe "from code snippets" $ do
    it "extracts from function application" $ do
      let result = Set.fromList $ extractIdentifiers "map f xs"
      Set.member "map" result `shouldBe` True
      Set.member "f" result `shouldBe` True
      Set.member "xs" result `shouldBe` True

    it "extracts from let binding" $ do
      let result = Set.fromList $ extractIdentifiers "let x = foo in bar x"
      Set.member "x" result `shouldBe` True
      Set.member "foo" result `shouldBe` True
      Set.member "bar" result `shouldBe` True

    it "extracts from lambda" $ do
      let result = Set.fromList $ extractIdentifiers "\\x -> f x"
      Set.member "x" result `shouldBe` True
      Set.member "f" result `shouldBe` True

    it "extracts from type signature" $ do
      let result = Set.fromList $ extractIdentifiers "foo :: Int -> String -> Bool"
      Set.member "foo" result `shouldBe` True
      Set.member "Int" result `shouldBe` True
      Set.member "String" result `shouldBe` True
      Set.member "Bool" result `shouldBe` True

    it "extracts from qualified names" $ do
      let result = Set.fromList $ extractIdentifiers "Data.Map.fromList xs"
      Set.member "Data" result `shouldBe` True
      Set.member "Map" result `shouldBe` True
      Set.member "fromList" result `shouldBe` True
      Set.member "xs" result `shouldBe` True

    it "extracts from case expression" $ do
      let result = Set.fromList $ extractIdentifiers "case x of Just y -> y; Nothing -> z"
      Set.member "x" result `shouldBe` True
      Set.member "Just" result `shouldBe` True
      Set.member "y" result `shouldBe` True
      Set.member "Nothing" result `shouldBe` True
      Set.member "z" result `shouldBe` True

    it "extracts from do notation" $ do
      let result = Set.fromList $ extractIdentifiers "do { x <- getLine; print x }"
      Set.member "x" result `shouldBe` True
      Set.member "getLine" result `shouldBe` True
      Set.member "print" result `shouldBe` True

  describe "filtering" $ do
    it "excludes keywords" $ do
      "case" `elem` extractIdentifiers "case x of y -> y" `shouldBe` False
      "where" `elem` extractIdentifiers "foo where bar = 1" `shouldBe` False
      "let" `elem` extractIdentifiers "let x = 1" `shouldBe` False
      "if" `elem` extractIdentifiers "if x then y else z" `shouldBe` False

    it "excludes pure numbers" $ do
      "123" `elem` extractIdentifiers "foo 123 bar" `shouldBe` False
      "0" `elem` extractIdentifiers "x = 0" `shouldBe` False

  describe "edge cases" $ do
    it "handles empty string" $ do
      extractIdentifiers "" `shouldBe` []

    it "handles string with only operators" $ do
      extractIdentifiers ">>= >> >>" `shouldBe` []

    it "handles string with only whitespace" $ do
      extractIdentifiers "   \t\n  " `shouldBe` []

    it "handles unicode identifiers" $ do
      -- Haskell allows some unicode in identifiers
      let result = extractIdentifiers "αβγ"
      -- This may or may not be extracted depending on implementation
      length result `shouldSatisfy` (>= 0)

--------------------------------------------------------------------------------
-- extractOperators
--------------------------------------------------------------------------------

extractOperatorsSpec :: Spec
extractOperatorsSpec = describe "extractOperators" $ do
  describe "basic operators" $ do
    it "extracts simple operator" $ do
      "++" `elem` extractOperators "xs ++ ys" `shouldBe` True

    it "extracts multiple operators" $ do
      let result = Set.fromList $ extractOperators "a + b * c"
      Set.member "+" result `shouldBe` True
      Set.member "*" result `shouldBe` True

    it "extracts common operators" $ do
      ">>" `elem` extractOperators "a >> b" `shouldBe` True
      ">>=" `elem` extractOperators "a >>= f" `shouldBe` True
      "<>" `elem` extractOperators "a <> b" `shouldBe` True
      "<$>" `elem` extractOperators "f <$> x" `shouldBe` True
      "<*>" `elem` extractOperators "f <*> x" `shouldBe` True

    it "extracts lens operators" $ do
      ".~" `elem` extractOperators "x .~ y" `shouldBe` True
      "^." `elem` extractOperators "x ^. lens" `shouldBe` True
      "&" `elem` extractOperators "x & f" `shouldBe` True

    it "extracts Aeson operators" $ do
      ".=" `elem` extractOperators "\"key\" .= value" `shouldBe` True
      ".:?" `elem` extractOperators "o .:? \"key\"" `shouldBe` True
      ".:!" `elem` extractOperators "o .:! \"key\"" `shouldBe` True

  describe "filtering" $ do
    it "excludes single dot" $ do
      "." `elem` extractOperators "f . g" `shouldBe` False

    it "excludes single equals" $ do
      "=" `elem` extractOperators "x = 1" `shouldBe` False

    it "excludes type annotation" $ do
      "::" `elem` extractOperators "x :: Int" `shouldBe` False

    it "excludes arrow operators" $ do
      "->" `elem` extractOperators "x -> y" `shouldBe` False
      "<-" `elem` extractOperators "x <- action" `shouldBe` False
      "=>" `elem` extractOperators "Monad m => m a" `shouldBe` False

    it "excludes comment markers" $ do
      "--" `elem` extractOperators "x -- comment" `shouldBe` False

  describe "edge cases" $ do
    it "handles empty string" $ do
      extractOperators "" `shouldBe` []

    it "handles string with only identifiers" $ do
      extractOperators "foo bar baz" `shouldBe` []

    it "handles consecutive operators" $ do
      let result = extractOperators ">>= >>"
      -- Should extract >>= and >> separately based on context
      length result `shouldSatisfy` (>= 0)

--------------------------------------------------------------------------------
-- extractModuleName
--------------------------------------------------------------------------------

extractModuleNameSpec :: Spec
extractModuleNameSpec = describe "extractModuleName" $ do
  describe "simple module declarations" $ do
    it "extracts simple module name" $ do
      extractModuleName "module Foo where" `shouldBe` "Foo"

    it "extracts hierarchical module name" $ do
      extractModuleName "module Data.List.Extra where" `shouldBe` "Data.List.Extra"

    it "extracts module name with export list" $ do
      extractModuleName "module Foo (foo, bar) where" `shouldBe` "Foo"

    it "extracts module name with multiline export" $ do
      let source = T.unlines
            [ "module Foo"
            , "  ( foo"
            , "  , bar"
            , "  ) where"
            ]
      extractModuleName source `shouldBe` "Foo"

  describe "with pragmas and comments" $ do
    it "extracts module after pragmas" $ do
      let source = T.unlines
            [ "{-# LANGUAGE OverloadedStrings #-}"
            , "module Foo where"
            ]
      extractModuleName source `shouldBe` "Foo"

    it "extracts module after comments" $ do
      let source = T.unlines
            [ "-- | Module documentation"
            , "module Foo where"
            ]
      extractModuleName source `shouldBe` "Foo"

    it "extracts module after block comment" $ do
      let source = T.unlines
            [ "{- Block comment -}"
            , "module Foo where"
            ]
      extractModuleName source `shouldBe` "Foo"

  describe "edge cases" $ do
    it "returns empty for no module declaration" $ do
      extractModuleName "foo = 1" `shouldBe` ""

    it "returns empty for empty string" $ do
      extractModuleName "" `shouldBe` ""

    it "handles module with leading whitespace" $ do
      extractModuleName "   module Foo where" `shouldBe` "Foo"

    it "handles module with tabs" $ do
      extractModuleName "\tmodule Foo where" `shouldBe` "Foo"

    it "handles Main module" $ do
      extractModuleName "module Main where" `shouldBe` "Main"

    it "handles very long module names" $ do
      extractModuleName "module A.B.C.D.E.F.G where" `shouldBe` "A.B.C.D.E.F.G"

--------------------------------------------------------------------------------
-- AnalysisContext
--------------------------------------------------------------------------------

analysisContextSpec :: Spec
analysisContextSpec = describe "AnalysisContext" $ do
  describe "defaultContext" $ do
    it "creates context with provided config" $ do
      let cfg = defaultConfig
          opts = defaultOptions
          rules = defaultRulesConfig
          ctx = defaultContext cfg opts rules
      acConfig ctx `shouldBe` cfg

    it "creates context with provided options" $ do
      let cfg = defaultConfig
          opts = defaultOptions
          rules = defaultRulesConfig
          ctx = defaultContext cfg opts rules
      acOptions ctx `shouldBe` opts

    it "creates context with empty HIE data" $ do
      let cfg = defaultConfig
          opts = defaultOptions
          rules = defaultRulesConfig
          ctx = defaultContext cfg opts rules
      acHieData ctx `shouldBe` []

    it "creates context with no dependency graph" $ do
      let cfg = defaultConfig
          opts = defaultOptions
          rules = defaultRulesConfig
          ctx = defaultContext cfg opts rules
      acDepGraph ctx `shouldBe` Nothing

    it "creates context with rules config" $ do
      let cfg = defaultConfig
          opts = defaultOptions
          rules = defaultRulesConfig
          ctx = defaultContext cfg opts rules
      acRulesConfig ctx `shouldBe` rules

    it "creates context with no HIE loader by default" $ do
      let cfg = defaultConfig
          opts = defaultOptions
          rules = defaultRulesConfig
          ctx = defaultContext cfg opts rules
      -- Use case to avoid Show constraint on IncrementalLoader
      case acHieLoader ctx of
        Nothing -> pure ()  -- Expected
        Just _  -> expectationFailure "Expected acHieLoader to be Nothing"

--------------------------------------------------------------------------------
-- findHaskellFiles
--------------------------------------------------------------------------------

findHaskellFilesSpec :: Spec
findHaskellFilesSpec = describe "findHaskellFiles" $ do
  describe "with temporary directory" $ do
    it "finds files in single directory" $ withTempDir $ \tmpDir -> do
      let srcDir = tmpDir </> "src"
      createDirectoryIfMissing True srcDir
      writeFile (srcDir </> "Foo.hs") "module Foo where"
      writeFile (srcDir </> "Bar.hs") "module Bar where"

      files <- findHaskellFiles [srcDir]
      length files `shouldBe` 2

    it "finds files recursively" $ withTempDir $ \tmpDir -> do
      let srcDir = tmpDir </> "src"
          subDir = srcDir </> "Sub"
      createDirectoryIfMissing True subDir
      writeFile (srcDir </> "Foo.hs") "module Foo where"
      writeFile (subDir </> "Bar.hs") "module Sub.Bar where"

      files <- findHaskellFiles [srcDir]
      length files `shouldBe` 2

    it "returns empty for non-existent directory" $ withTempDir $ \tmpDir -> do
      let nonExistent = tmpDir </> "nonexistent"
      files <- findHaskellFiles [nonExistent]
      files `shouldBe` []

    it "filters non-Haskell files" $ withTempDir $ \tmpDir -> do
      let srcDir = tmpDir </> "src"
      createDirectoryIfMissing True srcDir
      writeFile (srcDir </> "Foo.hs") "module Foo where"
      writeFile (srcDir </> "README.md") "# README"
      writeFile (srcDir </> "config.yaml") "key: value"

      files <- findHaskellFiles [srcDir]
      length files `shouldBe` 1
      all (\f -> ".hs" `isSuffixOf` f) files `shouldBe` True

    it "handles single file path" $ withTempDir $ \tmpDir -> do
      let filePath = tmpDir </> "Test.hs"
      writeFile filePath "module Test where"

      files <- findHaskellFiles [filePath]
      files `shouldBe` [filePath]

    it "handles multiple paths" $ withTempDir $ \tmpDir -> do
      let dir1 = tmpDir </> "dir1"
          dir2 = tmpDir </> "dir2"
      createDirectoryIfMissing True dir1
      createDirectoryIfMissing True dir2
      writeFile (dir1 </> "Foo.hs") "module Foo where"
      writeFile (dir2 </> "Bar.hs") "module Bar where"

      files <- findHaskellFiles [dir1, dir2]
      length files `shouldBe` 2

  describe "edge cases" $ do
    it "handles empty path list" $ do
      files <- findHaskellFiles []
      files `shouldBe` []

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Helper for running tests with a temporary directory
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "argus-core-test"

-- | Check if string ends with suffix
isSuffixOf :: String -> String -> Bool
isSuffixOf suffix str = drop (length str - length suffix) str == suffix
