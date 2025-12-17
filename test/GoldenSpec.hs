{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : GoldenSpec
-- Description : Golden tests for output formatters
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Golden tests ensure output format consistency across changes.
-- Each test compares actual output against expected "golden" files.
module GoldenSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.Environment (lookupEnv)
import Test.Hspec

import Argus.Output.Checkstyle
import Argus.Output.CodeClimate
import Argus.Output.Html
import Argus.Output.Json
import Argus.Output.JUnit
import Argus.Output.Sarif
import Argus.Output.Terminal
import Argus.Output.Types
import Argus.Types
  ( mkSrcSpanRaw
  , Diagnostic(..)
  , Severity(..)
  , SrcSpan(..)
  , DiagnosticKind(..)
  , AnalysisResult(..)
  , FileResult(..)
  , emptyAnalysisResult
  , Fix(..)
  , FixEdit(..)
  , FixImport(..)
  , ImportSymbol(..)
  , ImportSymbolType(..)
  , FixCategory(..)
  , FixSafety(..)
  , QualifiedName(..)
  , mkQualifiedName
  )

--------------------------------------------------------------------------------
-- Golden Test Infrastructure
--------------------------------------------------------------------------------

-- | Directory containing golden files
goldenDir :: FilePath
goldenDir = "test/golden"

-- | Compare output against golden file
goldenTest :: FilePath -> Text -> IO ()
goldenTest name actual = do
  let goldenPath = goldenDir <> "/" <> name
  createDirectoryIfMissing True goldenDir

  -- Check if we should update golden files
  updateMode <- lookupEnv "UPDATE_GOLDEN"

  exists <- doesFileExist goldenPath
  case (exists, updateMode) of
    (_, Just _) -> do
      -- Update mode: write actual output to golden file
      TIO.writeFile goldenPath actual
    (False, Nothing) -> do
      -- Golden file doesn't exist, create it
      TIO.writeFile goldenPath actual
      pendingWith $ "Golden file created: " <> goldenPath
    (True, Nothing) -> do
      -- Compare against existing golden file
      expected <- TIO.readFile goldenPath
      normalizeOutput actual `shouldBe` normalizeOutput expected

-- | Normalize output for comparison (handle minor whitespace differences)
normalizeOutput :: Text -> Text
normalizeOutput = T.unlines . map T.stripEnd . T.lines

--------------------------------------------------------------------------------
-- Test Data
--------------------------------------------------------------------------------

-- | Standard test diagnostic for golden tests
goldenDiagnostic :: Diagnostic
goldenDiagnostic = Diagnostic
  { diagSpan = mkSrcSpanRaw "src/Example.hs" 42 5 42 15
  , diagSeverity = Warning
  , diagKind = PartialFunction
  , diagMessage = "Use of partial function 'head': consider using 'headMay' from Data.Maybe or pattern matching"
  , diagCode = Just "partial/head"
  , diagFixes = [goldenFix]
  , diagRelated = []
  }

-- | Standard fix for golden tests
goldenFix :: Fix
goldenFix = Fix
  { fixTitle = "Replace with safe alternative"
  , fixEdits = [FixEdit (mkSrcSpanRaw "src/Example.hs" 42 5 42 9) "headMay"]
  , fixIsPreferred = True
  , fixAddImports = [FixImport
      { fimpModule = "Data.Maybe"
      , fimpSymbols = [ImportSymbol "listToMaybe" ISTFunction []]
      , fimpQualified = Nothing
      , fimpHiding = False
      , fimpPackage = Nothing
      }]
  , fixRemoveImports = []
  , fixCategory = FCSafety
  , fixSafety = FSAlways
  }

-- | Error diagnostic
goldenErrorDiagnostic :: Diagnostic
goldenErrorDiagnostic = Diagnostic
  { diagSpan = mkSrcSpanRaw "src/Security.hs" 15 1 15 30
  , diagSeverity = Error
  , diagKind = SecurityIssue
  , diagMessage = "Potential SQL injection: use parameterized queries instead of string concatenation"
  , diagCode = Just "security/sql-injection"
  , diagFixes = []
  , diagRelated = []
  }

-- | Suggestion diagnostic
goldenSuggestionDiagnostic :: Diagnostic
goldenSuggestionDiagnostic = Diagnostic
  { diagSpan = mkSrcSpanRaw "src/Utils.hs" 100 10 100 25
  , diagSeverity = Suggestion
  , diagKind = CodePattern
  , diagMessage = "Consider using 'foldl'' instead of 'foldl' for better performance with strict accumulator"
  , diagCode = Just "performance/foldl"
  , diagFixes = []
  , diagRelated = []
  }

-- | Standard analysis result for golden tests
goldenResult :: AnalysisResult
goldenResult = AnalysisResult
  { resultFiles = Map.fromList
      [ ("src/Example.hs", FileResult
          { fileResultPath = "src/Example.hs"
          , fileResultDiagnostics = [goldenDiagnostic]
          , fileResultSymbols = []
          , fileResultImports = [mkQualifiedName (Just "Data") "List", mkQualifiedName (Just "Data") "Map"]
          , fileResultExports = [mkQualifiedName Nothing "exampleFunction", mkQualifiedName Nothing "helperFunction"]
          })
      , ("src/Security.hs", FileResult
          { fileResultPath = "src/Security.hs"
          , fileResultDiagnostics = [goldenErrorDiagnostic]
          , fileResultSymbols = []
          , fileResultImports = [mkQualifiedName (Just "Database") "PostgreSQL"]
          , fileResultExports = [mkQualifiedName Nothing "runQuery"]
          })
      , ("src/Utils.hs", FileResult
          { fileResultPath = "src/Utils.hs"
          , fileResultDiagnostics = [goldenSuggestionDiagnostic]
          , fileResultSymbols = []
          , fileResultImports = [mkQualifiedName (Just "Data") "Foldable"]
          , fileResultExports = [mkQualifiedName Nothing "sumList"]
          })
      ]
  , resultUnusedCode = Set.singleton (mkQualifiedName (Just "src/Unused.hs") "unusedHelper")
  , resultDiagCount = Map.fromList [(Warning, 1), (Error, 1), (Suggestion, 1)]
  }

-- | Output options for golden tests (no colors for deterministic output)
goldenOutputOptions :: OutputOptions
goldenOutputOptions = OutputOptions
  { ooFormat = TerminalFormat
  , ooColor = False
  , ooGroupBy = "file"
  , ooShowContext = False
  , ooContextLines = 0
  , ooSourceCache = Map.empty
  }

--------------------------------------------------------------------------------
-- Golden Tests
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Golden Tests for Output Formatters" $ do
    jsonGoldenTests
    sarifGoldenTests
    junitGoldenTests
    checkstyleGoldenTests
    codeClimateGoldenTests
    htmlGoldenTests
    terminalGoldenTests

--------------------------------------------------------------------------------
-- JSON Golden Tests
--------------------------------------------------------------------------------

jsonGoldenTests :: Spec
jsonGoldenTests = describe "JSON output" $ do
  it "produces expected JSON output (compact)" $ do
    let output = renderJson goldenOutputOptions goldenResult
    goldenTest "json_compact.json" output

  it "produces expected JSON output (pretty)" $ do
    let output = renderJsonPretty goldenOutputOptions goldenResult
    goldenTest "json_pretty.json" output

  it "produces expected JSON for empty result" $ do
    let output = renderJson goldenOutputOptions emptyAnalysisResult
    goldenTest "json_empty.json" output

--------------------------------------------------------------------------------
-- SARIF Golden Tests
--------------------------------------------------------------------------------

sarifGoldenTests :: Spec
sarifGoldenTests = describe "SARIF output" $ do
  it "produces expected SARIF output" $ do
    let output = renderSarif goldenOutputOptions goldenResult
    goldenTest "sarif_standard.sarif" output

  it "produces expected SARIF for empty result" $ do
    let output = renderSarif goldenOutputOptions emptyAnalysisResult
    goldenTest "sarif_empty.sarif" output

  it "SARIF version is 2.1.0" $ do
    let output = renderSarif goldenOutputOptions goldenResult
    T.isInfixOf "\"version\":\"2.1.0\"" output `shouldBe` True

  it "SARIF includes schema reference" $ do
    let output = renderSarif goldenOutputOptions goldenResult
    T.isInfixOf "sarif-schema" output `shouldBe` True

--------------------------------------------------------------------------------
-- JUnit Golden Tests
--------------------------------------------------------------------------------

junitGoldenTests :: Spec
junitGoldenTests = describe "JUnit output" $ do
  it "produces expected JUnit XML output" $ do
    let Output{outText} = renderJUnit goldenOutputOptions goldenResult
    goldenTest "junit_standard.xml" outText

  it "produces expected JUnit XML for empty result" $ do
    let Output{outText} = renderJUnit goldenOutputOptions emptyAnalysisResult
    goldenTest "junit_empty.xml" outText

  it "JUnit includes XML declaration" $ do
    let Output{outText} = renderJUnit goldenOutputOptions goldenResult
    T.isPrefixOf "<?xml version=\"1.0\"" outText `shouldBe` True

  it "JUnit includes testsuites element" $ do
    let Output{outText} = renderJUnit goldenOutputOptions goldenResult
    T.isInfixOf "<testsuites" outText `shouldBe` True
    T.isInfixOf "</testsuites>" outText `shouldBe` True

--------------------------------------------------------------------------------
-- Checkstyle Golden Tests
--------------------------------------------------------------------------------

checkstyleGoldenTests :: Spec
checkstyleGoldenTests = describe "Checkstyle output" $ do
  it "produces expected Checkstyle XML output" $ do
    let Output{outText} = renderCheckstyle goldenOutputOptions goldenResult
    goldenTest "checkstyle_standard.xml" outText

  it "produces expected Checkstyle XML for empty result" $ do
    let Output{outText} = renderCheckstyle goldenOutputOptions emptyAnalysisResult
    goldenTest "checkstyle_empty.xml" outText

  it "Checkstyle includes XML declaration" $ do
    let Output{outText} = renderCheckstyle goldenOutputOptions goldenResult
    T.isPrefixOf "<?xml version=\"1.0\"" outText `shouldBe` True

  it "Checkstyle includes checkstyle element with version" $ do
    let Output{outText} = renderCheckstyle goldenOutputOptions goldenResult
    T.isInfixOf "<checkstyle version=\"4.3\">" outText `shouldBe` True

--------------------------------------------------------------------------------
-- CodeClimate Golden Tests
--------------------------------------------------------------------------------

codeClimateGoldenTests :: Spec
codeClimateGoldenTests = describe "CodeClimate output" $ do
  it "produces expected CodeClimate JSON output" $ do
    let Output{outText} = renderCodeClimate goldenOutputOptions goldenResult
    goldenTest "codeclimate_standard.json" outText

  it "produces expected CodeClimate JSON for empty result" $ do
    let Output{outText} = renderCodeClimate goldenOutputOptions emptyAnalysisResult
    goldenTest "codeclimate_empty.json" outText

  it "CodeClimate output is a JSON array" $ do
    let Output{outText} = renderCodeClimate goldenOutputOptions goldenResult
    T.isPrefixOf "[" outText `shouldBe` True
    T.isSuffixOf "]" (T.stripEnd outText) `shouldBe` True

  it "CodeClimate includes issue type" $ do
    let Output{outText} = renderCodeClimate goldenOutputOptions goldenResult
    T.isInfixOf "\"type\":\"issue\"" outText `shouldBe` True

--------------------------------------------------------------------------------
-- HTML Golden Tests
--------------------------------------------------------------------------------

htmlGoldenTests :: Spec
htmlGoldenTests = describe "HTML output" $ do
  it "produces expected HTML output" $ do
    let output = renderHtml goldenOutputOptions goldenResult
    goldenTest "html_standard.html" output

  it "produces expected HTML for empty result" $ do
    let output = renderHtml goldenOutputOptions emptyAnalysisResult
    goldenTest "html_empty.html" output

  it "HTML includes DOCTYPE" $ do
    let output = renderHtml goldenOutputOptions goldenResult
    T.isPrefixOf "<!DOCTYPE html>" output `shouldBe` True

  it "HTML includes proper structure" $ do
    let output = renderHtml goldenOutputOptions goldenResult
    T.isInfixOf "<html" output `shouldBe` True
    T.isInfixOf "</html>" output `shouldBe` True
    T.isInfixOf "<head>" output `shouldBe` True
    T.isInfixOf "<body>" output `shouldBe` True

  it "HTML includes embedded CSS" $ do
    let output = renderHtml goldenOutputOptions goldenResult
    T.isInfixOf "<style>" output `shouldBe` True

--------------------------------------------------------------------------------
-- Terminal Golden Tests
--------------------------------------------------------------------------------

terminalGoldenTests :: Spec
terminalGoldenTests = describe "Terminal output" $ do
  it "produces expected terminal output (no color)" $ do
    let output = renderTerminal goldenOutputOptions goldenResult
    goldenTest "terminal_nocolor.txt" output

  it "produces expected terminal output for empty result" $ do
    let output = renderTerminal goldenOutputOptions emptyAnalysisResult
    goldenTest "terminal_empty.txt" output

  it "terminal output includes file paths" $ do
    let output = renderTerminal goldenOutputOptions goldenResult
    T.isInfixOf "src/Example.hs" output `shouldBe` True
    T.isInfixOf "src/Security.hs" output `shouldBe` True
    T.isInfixOf "src/Utils.hs" output `shouldBe` True

  it "terminal output includes severity indicators" $ do
    let output = renderTerminal goldenOutputOptions goldenResult
    T.isInfixOf "Warning" output || T.isInfixOf "warning" output `shouldBe` True
    T.isInfixOf "Error" output || T.isInfixOf "error" output `shouldBe` True

  it "terminal output includes messages" $ do
    let output = renderTerminal goldenOutputOptions goldenResult
    T.isInfixOf "partial function" output `shouldBe` True
    T.isInfixOf "SQL injection" output `shouldBe` True

--------------------------------------------------------------------------------
-- Output Stability Tests
--------------------------------------------------------------------------------

-- | These tests verify that the output structure remains stable
-- across versions, which is important for CI/CD integrations.

outputStabilityTests :: Spec
outputStabilityTests = describe "Output stability" $ do
  it "JSON schema structure is stable" $ do
    let output = renderJson goldenOutputOptions goldenResult
    -- Required top-level fields
    T.isInfixOf "\"version\"" output `shouldBe` True
    T.isInfixOf "\"files\"" output `shouldBe` True
    T.isInfixOf "\"summary\"" output `shouldBe` True

  it "SARIF schema structure is stable" $ do
    let output = renderSarif goldenOutputOptions goldenResult
    -- Required SARIF fields
    T.isInfixOf "\"$schema\"" output `shouldBe` True
    T.isInfixOf "\"version\"" output `shouldBe` True
    T.isInfixOf "\"runs\"" output `shouldBe` True
    T.isInfixOf "\"tool\"" output `shouldBe` True
    T.isInfixOf "\"results\"" output `shouldBe` True

  it "JUnit schema structure is stable" $ do
    let Output{outText} = renderJUnit goldenOutputOptions goldenResult
    -- Required JUnit elements
    T.isInfixOf "<testsuites" outText `shouldBe` True
    T.isInfixOf "<testsuite" outText `shouldBe` True
    T.isInfixOf "<testcase" outText `shouldBe` True
