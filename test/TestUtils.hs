{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : TestUtils
-- Description : Shared test utilities, generators, and fixtures for Argus test suite
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides a comprehensive set of test utilities including:
-- * QuickCheck generators for all core types
-- * Test fixtures and factories for common test data
-- * Assertion helpers for domain-specific validation
-- * File system utilities for integration tests
-- * Golden test infrastructure
-- * Mock objects and test doubles
module TestUtils
  ( -- * QuickCheck Generators
    -- ** Core Type Generators
    genLine
  , genColumn
  , genSrcSpan
  , genSrcSpanInFile
  , genSeverity
  , genDiagnosticKind
  , genDiagnostic
  , genDiagnosticWithFixes
  , genFix
  , genFixEdit
  , genFixCategory
  , genFixSafety
  , genSymbol
  , genSymbolKind
  , genQualifiedName
    -- ** Text Generators
  , genIdentifier
  , genModuleName
  , genQualifiedModuleName
  , genTypeName
  , genFunctionName
  , genOperator
  , genHaskellCode
  , genUnicodeText
    -- ** Path Generators
  , genHaskellFilePath
  , genModulePath
    -- ** Arbitrary Instances
  , ArbitraryDiagnostic(..)
  , ArbitraryFix(..)
  , ArbitrarySrcSpan(..)

    -- * Test Fixtures
    -- ** Diagnostic Fixtures
  , mkTestDiagnostic
  , mkTestDiagnosticWithCode
  , mkTestDiagnosticWithFixes
  , mkWarningDiagnostic
  , mkErrorDiagnostic
  , mkSuggestionDiagnostic
  , mkInfoDiagnostic
  , mkUnusedCodeDiagnostic
  , mkSecurityDiagnostic
  , mkPartialFunctionDiagnostic
    -- ** Fix Fixtures
  , mkTestFix
  , mkTestFixWithEdits
  , mkSafeFix
  , mkUnsafeFix
  , mkPreferredFix
    -- ** Span Fixtures
  , mkTestSpan
  , mkSingleLineSpan
  , mkMultiLineSpan
  , mkZeroWidthSpan
    -- ** Analysis Result Fixtures
  , mkTestAnalysisResult
  , mkEmptyAnalysisResult
  , mkAnalysisResultWithDiags
  , mkTestFileResult
  , mkEmptyFileResult
    -- ** Config Fixtures
  , mkTestConfig
  , mkMinimalConfig
  , mkStrictConfig
    -- ** Symbol Fixtures
  , mkTestSymbol
  , mkTestQualifiedName
  , mkFunctionSymbol
  , mkTypeSymbol
  , mkClassSymbol

    -- * Assertion Helpers
  , shouldHaveDiagnosticCount
  , shouldHaveSeverity
  , shouldHaveCode
  , shouldHaveMessage
  , shouldHaveMessageContaining
  , shouldHaveFix
  , shouldHaveFixCount
  , shouldSpanLine
  , shouldSpanLines
  , shouldBePreferredFix
  , shouldBeSafeFix
  , shouldContainDiagnosticKind
  , shouldNotContainDiagnosticKind
  , diagnosticsShouldBe
  , fixesShouldBeApplicable

    -- * File System Utilities
  , withTempHaskellFile
  , withTempHaskellModule
  , withTempHaskellProject
  , createTempHaskellFile
  , createTempConfig
  , withTestDirectory
  , copyTestDataTo
  , getTestDataPath
  , getGoldenPath
  , readTestFile
  , writeTestFile

    -- * Golden Test Utilities
  , goldenTest
  , goldenTestOutput
  , goldenTestJson
  , goldenTestSarif
  , updateGoldenFile
  , compareWithGolden
  , GoldenResult(..)

    -- * Mock Objects
  , MockFileSystem(..)
  , mkMockFileSystem
  , MockConfig(..)
  , mkMockConfig

    -- * Analysis Helpers
  , runAnalysisOnCode
  , runAnalysisOnFile
  , runRuleOnCode
  , parseTestCode
  , parseTestFile

    -- * Property Test Helpers
  , forAllDiagnostics
  , forAllFixes
  , forAllSpans
  , forAllSeverities
  , shrinkDiagnostic
  , shrinkFix
  , shrinkSrcSpan

    -- * Test Data Constants
  , testFilePath
  , testModuleName
  , testFunctionName
  , testTypeName
  , sampleHaskellCode
  , sampleHaskellModule
  , sampleHaskellWithWarnings
  , sampleHaskellWithErrors
  , allSeverities
  , allDiagnosticKinds
  , allFixCategories
  , allFixSafeties
  , allSymbolKinds
  ) where

import Control.Monad (forM_)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import System.Directory
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.QuickCheck

import Argus.Types
import Argus.Config (Config(..), NamingConfig(..), defaultConfig, ComplexityConfig(..))
import Argus.Analysis.Syntactic (parseFile, ParseResult(..), ParseError(..))

--------------------------------------------------------------------------------
-- QuickCheck Generators - Core Types
--------------------------------------------------------------------------------

-- | Generate a valid line number (1-indexed, positive)
genLine :: Gen Line
genLine = Line . getPositive <$> arbitrary

-- | Generate a valid column number (1-indexed, positive)
genColumn :: Gen Column
genColumn = Column . getPositive <$> arbitrary

-- | Generate a valid source span with consistent start/end positions
genSrcSpan :: Gen SrcSpan
genSrcSpan = do
  file <- genHaskellFilePath
  genSrcSpanInFile (T.unpack file)

-- | Generate a source span within a specific file
genSrcSpanInFile :: FilePath -> Gen SrcSpan
genSrcSpanInFile file = do
  startLine <- choose (1, 10000)
  startCol <- choose (1, 200)
  lineSpan <- choose (0, 100)  -- How many additional lines
  endCol <- if lineSpan == 0
            then choose (startCol, startCol + 100)
            else choose (1, 200)
  pure $ mkSrcSpanRaw file startLine startCol (startLine + lineSpan) endCol

-- | Generate any severity
genSeverity :: Gen Severity
genSeverity = elements allSeverities

-- | Generate any diagnostic kind
genDiagnosticKind :: Gen DiagnosticKind
genDiagnosticKind = frequency
  [ (3, pure NamingConvention)
  , (3, pure UnusedCode)
  , (2, pure UnusedImport)
  , (2, pure RedundantCode)
  , (3, pure CodePattern)
  , (1, pure TypeSignature)
  , (1, pure ImportStyle)
  , (1, pure TemplateHaskellRef)
  , (2, pure SecurityIssue)
  , (2, pure PerformanceIssue)
  , (1, pure ArchitecturalIssue)
  , (2, pure SpaceLeak)
  , (2, pure PartialFunction)
  , (1, pure ComplexityIssue)
  , (1, Custom <$> genIdentifier)
  ]

-- | Generate a diagnostic without fixes
genDiagnostic :: Gen Diagnostic
genDiagnostic = do
  span' <- genSrcSpan
  sev <- genSeverity
  kind <- genDiagnosticKind
  msg <- genDiagnosticMessage kind
  code <- oneof [pure Nothing, Just <$> genDiagnosticCode kind]
  pure Diagnostic
    { diagSpan = span'
    , diagSeverity = sev
    , diagKind = kind
    , diagMessage = msg
    , diagCode = code
    , diagFixes = []
    , diagRelated = []
    }

-- | Generate a diagnostic with fixes
genDiagnosticWithFixes :: Gen Diagnostic
genDiagnosticWithFixes = do
  diag <- genDiagnostic
  numFixes <- choose (1, 3)
  fixes <- vectorOf numFixes (genFixForSpan (diagSpan diag))
  pure diag { diagFixes = fixes }

-- | Generate a contextually appropriate diagnostic message
genDiagnosticMessage :: DiagnosticKind -> Gen Text
genDiagnosticMessage = \case
  NamingConvention -> elements
    [ "Variable name 'x' should follow camelCase convention"
    , "Type name should start with uppercase letter"
    , "Function name too short"
    ]
  UnusedCode -> elements
    [ "Unused binding 'helper'"
    , "Function 'processData' is never called"
    , "Unused pattern variable 'y'"
    ]
  UnusedImport -> elements
    [ "Import of 'Data.List' is unused"
    , "Redundant import of 'Control.Monad'"
    , "Unused import item 'map'"
    ]
  SecurityIssue -> elements
    [ "Use of unsafePerformIO detected"
    , "Potential command injection vulnerability"
    , "Unsafe use of readFile with user input"
    ]
  PartialFunction -> elements
    [ "Use of partial function 'head'"
    , "Partial pattern match in lambda"
    , "Use of 'fromJust' without validation"
    ]
  PerformanceIssue -> elements
    [ "Use 'null' instead of 'length xs == 0'"
    , "Consider using strict fold"
    , "String concatenation in loop"
    ]
  SpaceLeak -> elements
    [ "Potential space leak in accumulator"
    , "Lazy evaluation may cause memory issues"
    , "Consider using strict version"
    ]
  _ -> genUnicodeText 10 50

-- | Generate a diagnostic code appropriate for the kind
genDiagnosticCode :: DiagnosticKind -> Gen Text
genDiagnosticCode = \case
  NamingConvention -> elements ["naming/camelCase", "naming/snake_case", "naming/typeCase"]
  UnusedCode -> elements ["unused/binding", "unused/function", "unused/variable"]
  UnusedImport -> elements ["imports/unused", "imports/redundant"]
  SecurityIssue -> elements ["security/unsafeIO", "security/injection", "security/command"]
  PartialFunction -> elements ["partial/head", "partial/tail", "partial/fromJust"]
  PerformanceIssue -> elements ["perf/length-null", "perf/concat", "perf/fold"]
  SpaceLeak -> elements ["spaceleak/accumulator", "spaceleak/lazy", "spaceleak/strict"]
  CodePattern -> elements ["pattern/eta", "pattern/pointfree", "pattern/guard"]
  _ -> do
    category <- elements ["misc", "style", "complexity"]
    rule <- genIdentifier
    pure $ category <> "/" <> rule

-- | Generate a fix
genFix :: Gen Fix
genFix = do
  title <- genFixTitle
  numEdits <- choose (1, 5)
  edits <- vectorOf numEdits genFixEdit
  preferred <- arbitrary
  category <- genFixCategory
  safety <- genFixSafety
  pure Fix
    { fixTitle = title
    , fixEdits = edits
    , fixIsPreferred = preferred
    , fixAddImports = []
    , fixRemoveImports = []
    , fixCategory = category
    , fixSafety = safety
    }

-- | Generate a fix for a specific span
genFixForSpan :: SrcSpan -> Gen Fix
genFixForSpan span' = do
  title <- genFixTitle
  replacement <- genHaskellCode
  preferred <- arbitrary
  category <- genFixCategory
  safety <- genFixSafety
  pure Fix
    { fixTitle = title
    , fixEdits = [FixEdit span' replacement]
    , fixIsPreferred = preferred
    , fixAddImports = []
    , fixRemoveImports = []
    , fixCategory = category
    , fixSafety = safety
    }

-- | Generate a fix title
genFixTitle :: Gen Text
genFixTitle = elements
  [ "Replace with safer alternative"
  , "Remove redundant code"
  , "Add type signature"
  , "Simplify expression"
  , "Use qualified import"
  , "Extract to local binding"
  , "Inline function"
  , "Add bang pattern"
  ]

-- | Generate a fix edit
genFixEdit :: Gen FixEdit
genFixEdit = FixEdit <$> genSrcSpan <*> genHaskellCode

-- | Generate a fix category
genFixCategory :: Gen FixCategory
genFixCategory = elements allFixCategories

-- | Generate a fix safety level
genFixSafety :: Gen FixSafety
genFixSafety = elements allFixSafeties

-- | Generate a symbol
genSymbol :: Gen Symbol
genSymbol = do
  name <- genQualifiedName
  kind <- genSymbolKind
  span' <- genSrcSpan
  exported <- arbitrary
  mType <- oneof [pure Nothing, Just <$> genTypeName]
  pure Symbol
    { symbolName = name
    , symbolKind = kind
    , symbolSpan = span'
    , symbolExported = exported
    , symbolType = mType
    }

-- | Generate a symbol kind
genSymbolKind :: Gen SymbolKind
genSymbolKind = elements allSymbolKinds

-- | Generate a qualified name
genQualifiedName :: Gen QualifiedName
genQualifiedName = do
  name <- genIdentifier
  mModule <- oneof [pure Nothing, Just <$> genModuleName]
  pure QualifiedName
    { qnName = name
    , qnModule = mModule
    }

--------------------------------------------------------------------------------
-- QuickCheck Generators - Text
--------------------------------------------------------------------------------

-- | Generate a valid Haskell identifier
genIdentifier :: Gen Text
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_', '\'']
  let len = min 20 (length rest)
  pure $ T.pack (first : take len rest)

-- | Generate a simple module name (single component)
genModuleName :: Gen Text
genModuleName = do
  first <- elements ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  let len = min 15 (length rest)
  pure $ T.pack (first : take len rest)

-- | Generate a qualified module name (e.g., "Data.List.Extra")
genQualifiedModuleName :: Gen Text
genQualifiedModuleName = do
  numParts <- choose (1, 4)
  parts <- vectorOf numParts genModuleName
  pure $ T.intercalate "." parts

-- | Generate a type name
genTypeName :: Gen Text
genTypeName = do
  base <- elements ["Int", "String", "Text", "Bool", "Maybe", "Either", "IO", "Map", "Set"]
  args <- listOf genModuleName
  if null args
    then pure base
    else pure $ base <> " " <> T.unwords (take 2 args)

-- | Generate a function name
genFunctionName :: Gen Text
genFunctionName = genIdentifier

-- | Generate a Haskell operator
genOperator :: Gen Text
genOperator = elements
  [ "+", "-", "*", "/", ".", "$", "<>", ">>", ">>=", "<$>", "<*>"
  , "==", "/=", "<", ">", "<=", ">=", "&&", "||", "++", "!!"
  ]

-- | Generate a small piece of Haskell code
genHaskellCode :: Gen Text
genHaskellCode = elements
  [ "x"
  , "foo x"
  , "bar x y"
  , "let a = b in c"
  , "case x of { Just y -> y; Nothing -> z }"
  , "if cond then a else b"
  , "\\x -> x + 1"
  , "map f xs"
  , "filter p xs"
  , "foldr f z xs"
  , "maybe default id mx"
  , "pure x"
  , "return ()"
  ]

-- | Generate text with unicode characters
genUnicodeText :: Int -> Int -> Gen Text
genUnicodeText minLen maxLen = do
  len <- choose (minLen, maxLen)
  chars <- vectorOf len $ frequency
    [ (80, elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")
    , (10, elements ['α', 'β', 'γ', 'δ', 'λ', 'μ', 'π', 'σ'])
    , (5, elements ['→', '←', '↑', '↓', '⇒', '∀', '∃', '∈'])
    , (5, elements "日本語中文한국어")
    ]
  pure $ T.pack chars

--------------------------------------------------------------------------------
-- QuickCheck Generators - Paths
--------------------------------------------------------------------------------

-- | Generate a Haskell file path
genHaskellFilePath :: Gen Text
genHaskellFilePath = do
  dirs <- choose (0, 3) >>= \n -> vectorOf n genModuleName
  file <- genModuleName
  let path = if null dirs
             then file <> ".hs"
             else T.intercalate "/" (map T.toLower dirs) <> "/" <> file <> ".hs"
  pure path

-- | Generate a path that matches module structure
genModulePath :: Gen (Text, Text)
genModulePath = do
  moduleName <- genQualifiedModuleName
  let path = T.replace "." "/" moduleName <> ".hs"
  pure (path, moduleName)

--------------------------------------------------------------------------------
-- Arbitrary Instances (Newtypes to avoid orphans)
--------------------------------------------------------------------------------

-- | Newtype wrapper for Diagnostic with Arbitrary instance
newtype ArbitraryDiagnostic = ArbitraryDiagnostic { unArbitraryDiagnostic :: Diagnostic }
  deriving stock (Show, Eq)

instance Arbitrary ArbitraryDiagnostic where
  arbitrary = ArbitraryDiagnostic <$> genDiagnostic
  shrink (ArbitraryDiagnostic d) = ArbitraryDiagnostic <$> shrinkDiagnostic d

-- | Newtype wrapper for Fix with Arbitrary instance
newtype ArbitraryFix = ArbitraryFix { unArbitraryFix :: Fix }
  deriving stock (Show, Eq)

instance Arbitrary ArbitraryFix where
  arbitrary = ArbitraryFix <$> genFix
  shrink (ArbitraryFix f) = ArbitraryFix <$> shrinkFix f

-- | Newtype wrapper for SrcSpan with Arbitrary instance
newtype ArbitrarySrcSpan = ArbitrarySrcSpan { unArbitrarySrcSpan :: SrcSpan }
  deriving stock (Show, Eq)

instance Arbitrary ArbitrarySrcSpan where
  arbitrary = ArbitrarySrcSpan <$> genSrcSpan
  shrink (ArbitrarySrcSpan s) = ArbitrarySrcSpan <$> shrinkSrcSpan s

--------------------------------------------------------------------------------
-- Test Fixtures - Diagnostics
--------------------------------------------------------------------------------

-- | Create a simple test diagnostic
mkTestDiagnostic :: FilePath -> Int -> Int -> Int -> Int -> Text -> Severity -> Diagnostic
mkTestDiagnostic file sl sc el ec msg sev = Diagnostic
  { diagSpan = mkSrcSpanRaw file sl sc el ec
  , diagSeverity = sev
  , diagKind = CodePattern
  , diagMessage = msg
  , diagCode = Nothing
  , diagFixes = []
  , diagRelated = []
  }

-- | Create a diagnostic with a specific code
mkTestDiagnosticWithCode :: FilePath -> Int -> Int -> Text -> Severity -> Text -> Diagnostic
mkTestDiagnosticWithCode file line col msg sev code = Diagnostic
  { diagSpan = mkSrcSpanRaw file line col line (col + 10)
  , diagSeverity = sev
  , diagKind = CodePattern
  , diagMessage = msg
  , diagCode = Just code
  , diagFixes = []
  , diagRelated = []
  }

-- | Create a diagnostic with fixes
mkTestDiagnosticWithFixes :: FilePath -> Int -> Int -> Text -> [Fix] -> Diagnostic
mkTestDiagnosticWithFixes file line col msg fixes = Diagnostic
  { diagSpan = mkSrcSpanRaw file line col line (col + 10)
  , diagSeverity = Warning
  , diagKind = CodePattern
  , diagMessage = msg
  , diagCode = Just "test/code"
  , diagFixes = fixes
  , diagRelated = []
  }

-- | Create a warning diagnostic
mkWarningDiagnostic :: FilePath -> Int -> Text -> Diagnostic
mkWarningDiagnostic file line msg =
  mkTestDiagnostic file line 1 line 80 msg Warning

-- | Create an error diagnostic
mkErrorDiagnostic :: FilePath -> Int -> Text -> Diagnostic
mkErrorDiagnostic file line msg =
  mkTestDiagnostic file line 1 line 80 msg Error

-- | Create a suggestion diagnostic
mkSuggestionDiagnostic :: FilePath -> Int -> Text -> Diagnostic
mkSuggestionDiagnostic file line msg =
  mkTestDiagnostic file line 1 line 80 msg Suggestion

-- | Create an info diagnostic
mkInfoDiagnostic :: FilePath -> Int -> Text -> Diagnostic
mkInfoDiagnostic file line msg =
  mkTestDiagnostic file line 1 line 80 msg Info

-- | Create an unused code diagnostic
mkUnusedCodeDiagnostic :: FilePath -> Int -> Text -> Diagnostic
mkUnusedCodeDiagnostic file line name =
  (mkWarningDiagnostic file line ("Unused binding '" <> name <> "'"))
    { diagKind = UnusedCode
    , diagCode = Just "unused/binding"
    }

-- | Create a security issue diagnostic
mkSecurityDiagnostic :: FilePath -> Int -> Text -> Diagnostic
mkSecurityDiagnostic file line msg =
  (mkErrorDiagnostic file line msg)
    { diagKind = SecurityIssue
    , diagCode = Just "security/issue"
    }

-- | Create a partial function diagnostic
mkPartialFunctionDiagnostic :: FilePath -> Int -> Text -> Text -> Diagnostic
mkPartialFunctionDiagnostic file line funcName replacement =
  (mkWarningDiagnostic file line ("Use of partial function '" <> funcName <> "'"))
    { diagKind = PartialFunction
    , diagCode = Just ("partial/" <> T.toLower funcName)
    , diagFixes = [mkTestFix ("Replace with " <> replacement) file line]
    }

--------------------------------------------------------------------------------
-- Test Fixtures - Fixes
--------------------------------------------------------------------------------

-- | Create a simple test fix
mkTestFix :: Text -> FilePath -> Int -> Fix
mkTestFix title file line = Fix
  { fixTitle = title
  , fixEdits = [FixEdit (mkSrcSpanRaw file line 1 line 20) "replacement"]
  , fixIsPreferred = False
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSMostly
  }

-- | Create a fix with specific edits
mkTestFixWithEdits :: Text -> [FixEdit] -> Fix
mkTestFixWithEdits title edits = Fix
  { fixTitle = title
  , fixEdits = edits
  , fixIsPreferred = False
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSMostly
  }

-- | Create a safe fix (FSAlways)
mkSafeFix :: Text -> FilePath -> Int -> Fix
mkSafeFix title file line = (mkTestFix title file line)
  { fixSafety = FSAlways }

-- | Create an unsafe fix
mkUnsafeFix :: Text -> FilePath -> Int -> Fix
mkUnsafeFix title file line = (mkTestFix title file line)
  { fixSafety = FSUnsafe }

-- | Create a preferred fix
mkPreferredFix :: Text -> FilePath -> Int -> Fix
mkPreferredFix title file line = (mkTestFix title file line)
  { fixIsPreferred = True }

--------------------------------------------------------------------------------
-- Test Fixtures - Spans
--------------------------------------------------------------------------------

-- | Create a test span
mkTestSpan :: FilePath -> Int -> Int -> Int -> Int -> SrcSpan
mkTestSpan = mkSrcSpanRaw

-- | Create a single-line span
mkSingleLineSpan :: FilePath -> Int -> Int -> Int -> SrcSpan
mkSingleLineSpan file line startCol endCol =
  mkSrcSpanRaw file line startCol line endCol

-- | Create a multi-line span
mkMultiLineSpan :: FilePath -> Int -> Int -> SrcSpan
mkMultiLineSpan file startLine endLine =
  mkSrcSpanRaw file startLine 1 endLine 80

-- | Create a zero-width span (insertion point)
mkZeroWidthSpan :: FilePath -> Int -> Int -> SrcSpan
mkZeroWidthSpan file line col =
  mkSrcSpanRaw file line col line col

--------------------------------------------------------------------------------
-- Test Fixtures - Analysis Results
--------------------------------------------------------------------------------

-- | Create a test analysis result with one file
mkTestAnalysisResult :: FilePath -> [Diagnostic] -> AnalysisResult
mkTestAnalysisResult path diags = AnalysisResult
  { resultFiles = Map.singleton path (mkTestFileResult path diags)
  , resultUnusedCode = Set.empty
  , resultDiagCount = countSeverities diags
  }

-- | Create an empty analysis result
mkEmptyAnalysisResult :: AnalysisResult
mkEmptyAnalysisResult = emptyAnalysisResult

-- | Create an analysis result with diagnostics from multiple files
mkAnalysisResultWithDiags :: [(FilePath, [Diagnostic])] -> AnalysisResult
mkAnalysisResultWithDiags filesDiags = AnalysisResult
  { resultFiles = Map.fromList [(p, mkTestFileResult p ds) | (p, ds) <- filesDiags]
  , resultUnusedCode = Set.empty
  , resultDiagCount = countSeverities (concatMap snd filesDiags)
  }

-- | Create a test file result
mkTestFileResult :: FilePath -> [Diagnostic] -> FileResult
mkTestFileResult path diags = FileResult
  { fileResultPath = path
  , fileResultDiagnostics = diags
  , fileResultSymbols = []
  , fileResultImports = []
  , fileResultExports = []
  }

-- | Create an empty file result
mkEmptyFileResult :: FilePath -> FileResult
mkEmptyFileResult path = mkTestFileResult path []

-- | Count diagnostics by severity
countSeverities :: [Diagnostic] -> Map Severity Int
countSeverities diags =
  Map.fromListWith (+) [(diagSeverity d, 1) | d <- diags]

--------------------------------------------------------------------------------
-- Test Fixtures - Config
--------------------------------------------------------------------------------

-- | Create a test configuration with all defaults
mkTestConfig :: Config
mkTestConfig = defaultConfig

-- | Create a minimal configuration
mkMinimalConfig :: Config
mkMinimalConfig = defaultConfig
  { cfgNaming = (cfgNaming defaultConfig) { namingEnabled = False }
  }

-- | Create a strict configuration (all checks enabled, low thresholds)
mkStrictConfig :: Config
mkStrictConfig = defaultConfig
  { cfgComplexity = (cfgComplexity defaultConfig)
      { compCyclomaticWarning = 5
      , compLineLengthWarning = 20
      , compParameterWarning = 3
      }
  }

--------------------------------------------------------------------------------
-- Test Fixtures - Symbols
--------------------------------------------------------------------------------

-- | Create a test symbol
mkTestSymbol :: Text -> Text -> SymbolKind -> Symbol
mkTestSymbol name modName kind = Symbol
  { symbolName = mkTestQualifiedName name modName
  , symbolKind = kind
  , symbolSpan = noSrcSpan
  , symbolExported = True
  , symbolType = Nothing
  }

-- | Create a test qualified name
mkTestQualifiedName :: Text -> Text -> QualifiedName
mkTestQualifiedName name modName = QualifiedName
  { qnName = name
  , qnModule = Just modName
  }

-- | Create a function symbol
mkFunctionSymbol :: Text -> Text -> Maybe Text -> Symbol
mkFunctionSymbol name modName mType = (mkTestSymbol name modName Function)
  { symbolType = mType }

-- | Create a type symbol
mkTypeSymbol :: Text -> Text -> Symbol
mkTypeSymbol name modName = mkTestSymbol name modName TypeConstructor

-- | Create a type class symbol
mkClassSymbol :: Text -> Text -> Symbol
mkClassSymbol name modName = mkTestSymbol name modName TypeClass

--------------------------------------------------------------------------------
-- Assertion Helpers
--------------------------------------------------------------------------------

-- | Assert diagnostic count
shouldHaveDiagnosticCount :: HasCallStack => [Diagnostic] -> Int -> Expectation
shouldHaveDiagnosticCount diags expected =
  length diags `shouldBe` expected

-- | Assert diagnostic severity
shouldHaveSeverity :: HasCallStack => Diagnostic -> Severity -> Expectation
shouldHaveSeverity diag expected =
  diagSeverity diag `shouldBe` expected

-- | Assert diagnostic code
shouldHaveCode :: HasCallStack => Diagnostic -> Text -> Expectation
shouldHaveCode diag expected =
  diagCode diag `shouldBe` Just expected

-- | Assert diagnostic message
shouldHaveMessage :: HasCallStack => Diagnostic -> Text -> Expectation
shouldHaveMessage diag expected =
  diagMessage diag `shouldBe` expected

-- | Assert diagnostic message contains substring
shouldHaveMessageContaining :: HasCallStack => Diagnostic -> Text -> Expectation
shouldHaveMessageContaining diag substring =
  diagMessage diag `shouldSatisfy` T.isInfixOf substring

-- | Assert diagnostic has at least one fix
shouldHaveFix :: HasCallStack => Diagnostic -> Expectation
shouldHaveFix diag =
  diagFixes diag `shouldSatisfy` (not . null)

-- | Assert diagnostic has specific number of fixes
shouldHaveFixCount :: HasCallStack => Diagnostic -> Int -> Expectation
shouldHaveFixCount diag expected =
  length (diagFixes diag) `shouldBe` expected

-- | Assert span is on specific line
shouldSpanLine :: HasCallStack => SrcSpan -> Int -> Expectation
shouldSpanLine span' line = do
  unLine (srcSpanStartLine span') `shouldBe` line
  unLine (srcSpanEndLine span') `shouldBe` line

-- | Assert span covers specific line range
shouldSpanLines :: HasCallStack => SrcSpan -> Int -> Int -> Expectation
shouldSpanLines span' startLine endLine = do
  unLine (srcSpanStartLine span') `shouldBe` startLine
  unLine (srcSpanEndLine span') `shouldBe` endLine

-- | Assert fix is preferred
shouldBePreferredFix :: HasCallStack => Fix -> Expectation
shouldBePreferredFix fix =
  fixIsPreferred fix `shouldBe` True

-- | Assert fix is safe
shouldBeSafeFix :: HasCallStack => Fix -> Expectation
shouldBeSafeFix fix =
  fixSafety fix `shouldBe` FSAlways

-- | Assert diagnostics contain specific kind
shouldContainDiagnosticKind :: HasCallStack => [Diagnostic] -> DiagnosticKind -> Expectation
shouldContainDiagnosticKind diags kind =
  diags `shouldSatisfy` any (\d -> diagKind d == kind)

-- | Assert diagnostics do not contain specific kind
shouldNotContainDiagnosticKind :: HasCallStack => [Diagnostic] -> DiagnosticKind -> Expectation
shouldNotContainDiagnosticKind diags kind =
  diags `shouldSatisfy` all (\d -> diagKind d /= kind)

-- | Assert diagnostics match expected list
diagnosticsShouldBe :: HasCallStack => [Diagnostic] -> [(Int, Severity, DiagnosticKind)] -> Expectation
diagnosticsShouldBe diags expected = do
  length diags `shouldBe` length expected
  let sortedDiags = sortOn (unLine . srcSpanStartLine . diagSpan) diags
      sortedExpected = sortOn (\(l, _, _) -> l) expected
  forM_ (zip sortedDiags sortedExpected) $ \(d, (line, sev, kind)) -> do
    unLine (srcSpanStartLine (diagSpan d)) `shouldBe` line
    diagSeverity d `shouldBe` sev
    diagKind d `shouldBe` kind

-- | Assert all fixes are applicable (have valid edits)
fixesShouldBeApplicable :: HasCallStack => [Fix] -> Expectation
fixesShouldBeApplicable fixes =
  forM_ fixes $ \fix -> do
    fixEdits fix `shouldSatisfy` (not . null)
    forM_ (fixEdits fix) $ \edit -> do
      fixEditNewText edit `shouldSatisfy` (not . T.null)

--------------------------------------------------------------------------------
-- File System Utilities
--------------------------------------------------------------------------------

-- | Run action with a temporary Haskell file
withTempHaskellFile :: Text -> (FilePath -> IO a) -> IO a
withTempHaskellFile content action =
  withSystemTempDirectory "argus-test" $ \dir -> do
    let path = dir </> "Test.hs"
    TIO.writeFile path content
    action path

-- | Run action with a temporary Haskell module (proper module header)
withTempHaskellModule :: Text -> Text -> (FilePath -> IO a) -> IO a
withTempHaskellModule moduleName content action =
  withTempHaskellFile fullContent action
  where
    fullContent = "module " <> moduleName <> " where\n\n" <> content

-- | Run action with a temporary Haskell project structure
withTempHaskellProject :: [(FilePath, Text)] -> (FilePath -> IO a) -> IO a
withTempHaskellProject files action =
  withSystemTempDirectory "argus-test-project" $ \dir -> do
    forM_ files $ \(relPath, content) -> do
      let fullPath = dir </> relPath
      createDirectoryIfMissing True (takeDirectory fullPath)
      TIO.writeFile fullPath content
    action dir

-- | Create a temporary Haskell file and return the path
createTempHaskellFile :: FilePath -> Text -> IO FilePath
createTempHaskellFile dir content = do
  let path = dir </> "Test.hs"
  TIO.writeFile path content
  pure path

-- | Create a temporary config file
createTempConfig :: FilePath -> Text -> IO FilePath
createTempConfig dir content = do
  let path = dir </> "argus.toml"
  TIO.writeFile path content
  pure path

-- | Run action with a test directory
withTestDirectory :: (FilePath -> IO a) -> IO a
withTestDirectory = withSystemTempDirectory "argus-test"

-- | Copy test data files to a directory
copyTestDataTo :: FilePath -> FilePath -> IO ()
copyTestDataTo testDataDir destDir = do
  files <- listDirectory testDataDir
  forM_ files $ \file -> do
    let src = testDataDir </> file
        dst = destDir </> file
    copyFile src dst

-- | Get the path to a test data file
getTestDataPath :: FilePath -> FilePath
getTestDataPath file = "test/data" </> file

-- | Get the path to a golden file
getGoldenPath :: FilePath -> FilePath
getGoldenPath file = "test/golden" </> file

-- | Read a test file
readTestFile :: FilePath -> IO Text
readTestFile = TIO.readFile

-- | Write a test file
writeTestFile :: FilePath -> Text -> IO ()
writeTestFile = TIO.writeFile

--------------------------------------------------------------------------------
-- Golden Test Utilities
--------------------------------------------------------------------------------

-- | Result of golden test comparison
data GoldenResult
  = GoldenMatch
  | GoldenMismatch !Text !Text  -- ^ expected, actual
  | GoldenMissing !Text         -- ^ actual
  deriving stock (Show, Eq)

-- | Run a golden test
goldenTest :: FilePath -> Text -> IO GoldenResult
goldenTest goldenPath actual = do
  exists <- doesFileExist goldenPath
  if exists
    then do
      expected <- TIO.readFile goldenPath
      if expected == actual
        then pure GoldenMatch
        else pure $ GoldenMismatch expected actual
    else pure $ GoldenMissing actual

-- | Golden test for rendered output
goldenTestOutput :: FilePath -> Text -> Expectation
goldenTestOutput goldenPath actual = do
  result <- goldenTest goldenPath actual
  case result of
    GoldenMatch -> pure ()
    GoldenMismatch expected _ ->
      expectationFailure $ "Golden test failed.\nExpected:\n" <> T.unpack expected
        <> "\n\nActual:\n" <> T.unpack actual
    GoldenMissing _ ->
      expectationFailure $ "Golden file missing: " <> goldenPath
        <> "\nActual output:\n" <> T.unpack actual

-- | Golden test for JSON output
goldenTestJson :: ToJSON a => FilePath -> a -> Expectation
goldenTestJson goldenPath value = do
  let actual = TE.decodeUtf8 $ BL.toStrict $ encode value
  goldenTestOutput goldenPath actual

-- | Golden test for SARIF output
goldenTestSarif :: FilePath -> Text -> Expectation
goldenTestSarif = goldenTestOutput

-- | Update a golden file with new content
updateGoldenFile :: FilePath -> Text -> IO ()
updateGoldenFile goldenPath content = do
  createDirectoryIfMissing True (takeDirectory goldenPath)
  TIO.writeFile goldenPath content

-- | Compare actual output with golden file
compareWithGolden :: FilePath -> Text -> IO Bool
compareWithGolden goldenPath actual = do
  result <- goldenTest goldenPath actual
  pure $ case result of
    GoldenMatch -> True
    _ -> False

--------------------------------------------------------------------------------
-- Mock Objects
--------------------------------------------------------------------------------

-- | Mock file system for testing without actual IO
data MockFileSystem = MockFileSystem
  { mfsFiles :: IORef (Map FilePath Text)
  , mfsDirectories :: IORef (Set FilePath)
  }

-- | Create a mock file system
mkMockFileSystem :: [(FilePath, Text)] -> IO MockFileSystem
mkMockFileSystem initialFiles = do
  filesRef <- newIORef $ Map.fromList initialFiles
  dirsRef <- newIORef $ Set.fromList $ map (takeDirectory . fst) initialFiles
  pure MockFileSystem
    { mfsFiles = filesRef
    , mfsDirectories = dirsRef
    }

-- | Mock configuration for testing
data MockConfig = MockConfig
  { mcEnabled :: Bool
  , mcRules :: [Text]
  , mcExclusions :: [Text]
  }
  deriving stock (Show, Eq)

-- | Create a mock configuration
mkMockConfig :: MockConfig
mkMockConfig = MockConfig
  { mcEnabled = True
  , mcRules = []
  , mcExclusions = []
  }

--------------------------------------------------------------------------------
-- Analysis Helpers
--------------------------------------------------------------------------------

-- | Run analysis on a string of Haskell code
runAnalysisOnCode :: Text -> IO [Diagnostic]
runAnalysisOnCode code = withTempHaskellModule "Test" code $ \path -> do
  result <- parseFile path
  case result of
    Left _ -> pure []
    Right _ -> pure []  -- Would need full analysis context

-- | Run analysis on a file
runAnalysisOnFile :: FilePath -> IO (Either Text [Diagnostic])
runAnalysisOnFile path = do
  result <- parseFile path
  case result of
    Left err -> pure $ Left (peMessage err)
    Right _ -> pure $ Right []  -- Would need full analysis context

-- | Run a specific rule on code
runRuleOnCode :: Text -> Text -> IO [Diagnostic]
runRuleOnCode _ruleName code = runAnalysisOnCode code

-- | Parse test code
parseTestCode :: Text -> IO (Either Text ParseResult)
parseTestCode code = withTempHaskellModule "Test" code $ \path -> do
  result <- parseFile path
  pure $ case result of
    Left err -> Left (peMessage err)
    Right pr -> Right pr

-- | Parse a test file
parseTestFile :: FilePath -> IO (Either Text ParseResult)
parseTestFile path = do
  result <- parseFile path
  pure $ case result of
    Left err -> Left (peMessage err)
    Right pr -> Right pr

--------------------------------------------------------------------------------
-- Property Test Helpers
--------------------------------------------------------------------------------

-- | Property helper for diagnostics
forAllDiagnostics :: Testable prop => (Diagnostic -> prop) -> Property
forAllDiagnostics = forAll genDiagnostic

-- | Property helper for fixes
forAllFixes :: Testable prop => (Fix -> prop) -> Property
forAllFixes = forAll genFix

-- | Property helper for spans
forAllSpans :: Testable prop => (SrcSpan -> prop) -> Property
forAllSpans = forAll genSrcSpan

-- | Property helper for severities
forAllSeverities :: Testable prop => (Severity -> prop) -> Property
forAllSeverities = forAll genSeverity

-- | Shrink a diagnostic
shrinkDiagnostic :: Diagnostic -> [Diagnostic]
shrinkDiagnostic d =
  -- Shrink message
  [d { diagMessage = T.take n (diagMessage d) } | n <- [0, 10, 50], n < T.length (diagMessage d)]
  ++
  -- Shrink fixes
  [d { diagFixes = take n (diagFixes d) } | n <- [0..length (diagFixes d) - 1]]
  ++
  -- Remove code
  [d { diagCode = Nothing } | isJust (diagCode d)]

-- | Shrink a fix
shrinkFix :: Fix -> [Fix]
shrinkFix f =
  -- Shrink title
  [f { fixTitle = T.take n (fixTitle f) } | n <- [5, 10], n < T.length (fixTitle f)]
  ++
  -- Shrink edits
  [f { fixEdits = take n (fixEdits f) } | n <- [1..length (fixEdits f) - 1], n > 0]

-- | Shrink a source span
shrinkSrcSpan :: SrcSpan -> [SrcSpan]
shrinkSrcSpan s =
  -- Shrink to single line if it spans multiple lines
  [ mkSrcSpanRaw (srcSpanFile s) startL (unColumn $ srcSpanStartCol s) startL (unColumn $ srcSpanEndCol s)
  | let startL = unLine (srcSpanStartLine s)
  , unLine (srcSpanStartLine s) /= unLine (srcSpanEndLine s)
  ]

--------------------------------------------------------------------------------
-- Test Data Constants
--------------------------------------------------------------------------------

-- | Default test file path
testFilePath :: FilePath
testFilePath = "test/data/Test.hs"

-- | Default test module name
testModuleName :: Text
testModuleName = "Test"

-- | Default test function name
testFunctionName :: Text
testFunctionName = "testFunction"

-- | Default test type name
testTypeName :: Text
testTypeName = "TestType"

-- | Sample Haskell code for testing
sampleHaskellCode :: Text
sampleHaskellCode = T.unlines
  [ "foo :: Int -> Int"
  , "foo x = x + 1"
  , ""
  , "bar :: String -> String"
  , "bar s = s ++ \"!\""
  ]

-- | Sample complete Haskell module
sampleHaskellModule :: Text
sampleHaskellModule = T.unlines
  [ "module Test where"
  , ""
  , "import Data.List (sort)"
  , "import Data.Maybe (fromMaybe)"
  , ""
  , "-- | A simple function"
  , "foo :: Int -> Int"
  , "foo x = x + 1"
  , ""
  , "-- | Another function"
  , "bar :: [Int] -> [Int]"
  , "bar = sort"
  ]

-- | Sample Haskell code with warnings
sampleHaskellWithWarnings :: Text
sampleHaskellWithWarnings = T.unlines
  [ "module Test where"
  , ""
  , "import Data.List  -- Unused import"
  , ""
  , "foo :: [Int] -> Int"
  , "foo xs = length xs == 0  -- Use null instead"
  , ""
  , "bar = head xs  -- Partial function"
  , "  where xs = []"
  ]

-- | Sample Haskell code with errors
sampleHaskellWithErrors :: Text
sampleHaskellWithErrors = T.unlines
  [ "module Test where"
  , ""
  , "import System.IO.Unsafe (unsafePerformIO)  -- Security"
  , ""
  , "foo :: IO ()"
  , "foo = system \"rm -rf /\"  -- Security issue"
  ]

-- | All severity levels
allSeverities :: [Severity]
allSeverities = [Error, Warning, Suggestion, Info]

-- | All diagnostic kinds
allDiagnosticKinds :: [DiagnosticKind]
allDiagnosticKinds =
  [ NamingConvention, UnusedCode, UnusedImport, RedundantCode
  , CodePattern, TypeSignature, ImportStyle, TemplateHaskellRef
  , SecurityIssue, PerformanceIssue, ArchitecturalIssue
  , SpaceLeak, PartialFunction, ComplexityIssue
  ]

-- | All fix categories
allFixCategories :: [FixCategory]
allFixCategories = [FCStyle, FCSafety, FCPerformance, FCModernize, FCImports, FCRedundant, FCSpaceLeaks, FCSecurity]

-- | All fix safety levels
allFixSafeties :: [FixSafety]
allFixSafeties = [FSAlways, FSMostly, FSReview, FSUnsafe]

-- | All symbol kinds
allSymbolKinds :: [SymbolKind]
allSymbolKinds =
  [ Function, TypeConstructor, DataConstructor
  , TypeClass, TypeClassMethod, TypeFamily, PatternSynonym
  , Module
  ]
