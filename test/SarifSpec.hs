{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : SarifSpec
-- Description : Tests for Argus.Output.Sarif
--
-- Comprehensive tests for SARIF output formatting including types,
-- ToJSON instances, and the main renderSarif function.
module SarifSpec (spec) where

import Test.Hspec
import Data.Aeson (decode)
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Argus.Output.Sarif
import Argus.Output.Types (OutputOptions(..), OutputFormat(..))
import Argus.Types

spec :: Spec
spec = do
  describe "Argus.Output.Sarif" $ do
    sarifTypesSpec
    renderSarifSpec

--------------------------------------------------------------------------------
-- SARIF Types
--------------------------------------------------------------------------------

sarifTypesSpec :: Spec
sarifTypesSpec = describe "SARIF Types" $ do
  describe "SarifLog" $ do
    it "has correct fields" $ do
      let log = SarifLog
            { slVersion = "2.1.0"
            , slSchema = "https://schema.example.com"
            , slRuns = []
            }
      slVersion log `shouldBe` "2.1.0"
      slSchema log `shouldBe` "https://schema.example.com"
      slRuns log `shouldBe` []

    it "has Eq instance" $ do
      let log1 = SarifLog "2.1.0" "schema" []
          log2 = SarifLog "2.1.0" "schema" []
          log3 = SarifLog "2.0.0" "schema" []
      log1 `shouldBe` log2
      log1 `shouldNotBe` log3

    it "has Show instance" $ do
      let log = SarifLog "2.1.0" "schema" []
      show log `shouldContain` "SarifLog"

  describe "SarifRun" $ do
    it "stores tool and results" $ do
      let run = SarifRun mkTestTool []
      srResults run `shouldBe` []

    it "has Eq instance" $ do
      let run1 = SarifRun mkTestTool []
          run2 = SarifRun mkTestTool []
      run1 `shouldBe` run2

  describe "SarifTool" $ do
    it "stores driver" $ do
      let tool = mkTestTool
      sdName (stDriver tool) `shouldBe` "test-tool"

  describe "SarifDriver" $ do
    it "stores all fields" $ do
      let driver = mkTestDriver
      sdName driver `shouldBe` "test-tool"
      sdVersion driver `shouldBe` "1.0.0"
      sdInformationUri driver `shouldBe` "https://example.com"
      sdRules driver `shouldBe` []

  describe "SarifMessage" $ do
    it "stores text" $ do
      let msg = SarifMessage "Test message"
      smText msg `shouldBe` "Test message"

    it "has Eq instance" $ do
      let m1 = SarifMessage "hello"
          m2 = SarifMessage "hello"
          m3 = SarifMessage "world"
      m1 `shouldBe` m2
      m1 `shouldNotBe` m3

  describe "SarifResult" $ do
    it "stores all fields correctly" $ do
      let result = mkTestResult "rule-1" "error" "Test message"
      sresRuleId result `shouldBe` "rule-1"
      sresLevel result `shouldBe` "error"
      smText (sresMessage result) `shouldBe` "Test message"

    it "has Eq instance" $ do
      let res1 = mkTestResult "rule-1" "error" "msg"
          res2 = mkTestResult "rule-1" "error" "msg"
          res3 = mkTestResult "rule-2" "error" "msg"
      res1 `shouldBe` res2
      res1 `shouldNotBe` res3

  describe "SarifLocation" $ do
    it "wraps physical location" $ do
      let loc = mkTestLocation "src/Test.hs" 1 1 1 10
      let physLoc = slPhysicalLocation loc
      salUri (splArtifactLocation physLoc) `shouldBe` "src/Test.hs"

    it "has Eq instance" $ do
      let l1 = mkTestLocation "a.hs" 1 1 1 10
          l2 = mkTestLocation "a.hs" 1 1 1 10
          l3 = mkTestLocation "b.hs" 1 1 1 10
      l1 `shouldBe` l2
      l1 `shouldNotBe` l3

  describe "SarifPhysicalLocation" $ do
    it "stores artifact and region" $ do
      let physLoc = SarifPhysicalLocation
            { splArtifactLocation = SarifArtifactLocation "test.hs"
            , splRegion = SarifRegion 1 2 3 4
            }
      salUri (splArtifactLocation physLoc) `shouldBe` "test.hs"
      sregStartLine (splRegion physLoc) `shouldBe` 1
      sregStartColumn (splRegion physLoc) `shouldBe` 2
      sregEndLine (splRegion physLoc) `shouldBe` 3
      sregEndColumn (splRegion physLoc) `shouldBe` 4

  describe "SarifRegion" $ do
    it "stores line and column info" $ do
      let region = SarifRegion 10 20 30 40
      sregStartLine region `shouldBe` 10
      sregStartColumn region `shouldBe` 20
      sregEndLine region `shouldBe` 30
      sregEndColumn region `shouldBe` 40

    it "has Eq instance" $ do
      let r1 = SarifRegion 1 2 3 4
          r2 = SarifRegion 1 2 3 4
          r3 = SarifRegion 1 2 3 5
      r1 `shouldBe` r2
      r1 `shouldNotBe` r3

  describe "SarifRule" $ do
    it "stores rule info" $ do
      let rule = SarifRule
            { sruId = "rule-001"
            , sruName = "Test Rule"
            , sruShortDescription = SarifMessage "A test rule"
            , sruDefaultLevel = "warning"
            }
      sruId rule `shouldBe` "rule-001"
      sruName rule `shouldBe` "Test Rule"
      sruDefaultLevel rule `shouldBe` "warning"

  describe "SarifFingerprints" $ do
    it "stores fingerprint data" $ do
      let fp = SarifFingerprints
            { sfPrimaryLocationLineHash = "abc123"
            , sfPrimaryLocationStartColumnFingerprint = "def456"
            }
      sfPrimaryLocationLineHash fp `shouldBe` "abc123"
      sfPrimaryLocationStartColumnFingerprint fp `shouldBe` "def456"

--------------------------------------------------------------------------------
-- renderSarif
--------------------------------------------------------------------------------

renderSarifSpec :: Spec
renderSarifSpec = describe "renderSarif" $ do
  describe "empty results" $ do
    it "renders empty analysis result" $ do
      let result = emptyAnalysisResult
          opts = testOutputOptions
          output = renderSarif opts result
      -- Should be valid JSON
      (decode (BL.fromStrict $ TE.encodeUtf8 output) :: Maybe A.Value) `shouldSatisfy` (/= Nothing)

    it "contains version 2.1.0" $ do
      let result = emptyAnalysisResult
          opts = testOutputOptions
          output = renderSarif opts result
      T.isInfixOf "2.1.0" output `shouldBe` True

    it "contains schema URL" $ do
      let result = emptyAnalysisResult
          opts = testOutputOptions
          output = renderSarif opts result
      T.isInfixOf "sarif-schema" output `shouldBe` True

    it "contains tool information" $ do
      let result = emptyAnalysisResult
          opts = testOutputOptions
          output = renderSarif opts result
      T.isInfixOf "argus" output `shouldBe` True

  describe "with diagnostics" $ do
    it "includes diagnostic messages" $ do
      let diag = mkTestDiagnostic "src/Test.hs" 5 1 5 10 "Test warning" Warning
          fileResult = FileResult
            { fileResultPath = "src/Test.hs"
            , fileResultDiagnostics = [diag]
            , fileResultSymbols = []
            , fileResultImports = []
            , fileResultExports = []
            }
          result = AnalysisResult
            { resultFiles = Map.singleton "src/Test.hs" fileResult
            , resultUnusedCode = Set.empty
            , resultDiagCount = Map.singleton Warning 1
            }
          opts = testOutputOptions
          output = renderSarif opts result
      T.isInfixOf "Test warning" output `shouldBe` True

    it "converts severity correctly" $ do
      let diag = mkTestDiagnostic "test.hs" 1 1 1 1 "Error msg" Error
          fileResult = FileResult "test.hs" [diag] [] [] []
          result = AnalysisResult
            (Map.singleton "test.hs" fileResult)
            Set.empty
            (Map.singleton Error 1)
          opts = testOutputOptions
          output = renderSarif opts result
      T.isInfixOf "error" output `shouldBe` True

    it "includes file path in locations" $ do
      let diag = mkTestDiagnostic "src/Module/File.hs" 10 5 10 20 "Warning" Warning
          fileResult = FileResult "src/Module/File.hs" [diag] [] [] []
          result = AnalysisResult
            (Map.singleton "src/Module/File.hs" fileResult)
            Set.empty
            (Map.singleton Warning 1)
          opts = testOutputOptions
          output = renderSarif opts result
      T.isInfixOf "src/Module/File.hs" output `shouldBe` True

    it "includes line and column numbers" $ do
      let diag = mkTestDiagnostic "test.hs" 42 15 42 30 "msg" Warning
          fileResult = FileResult "test.hs" [diag] [] [] []
          result = AnalysisResult
            (Map.singleton "test.hs" fileResult)
            Set.empty
            (Map.singleton Warning 1)
          opts = testOutputOptions
          output = renderSarif opts result
      -- Line 42 should appear in the output
      T.isInfixOf "42" output `shouldBe` True
      T.isInfixOf "15" output `shouldBe` True

  describe "with fixes" $ do
    it "includes fix information" $ do
      let fix = Fix
            { fixTitle = "Replace head with headMay"
            , fixEdits = [FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 5) "headMay"]
            , fixIsPreferred = True
            , fixAddImports = []
            , fixRemoveImports = []
            , fixCategory = FCSafety
            , fixSafety = FSAlways
            }
          diag = (mkTestDiagnostic "test.hs" 1 1 1 5 "Use safe function" Warning)
            { diagFixes = [fix] }
          fileResult = FileResult "test.hs" [diag] [] [] []
          result = AnalysisResult
            (Map.singleton "test.hs" fileResult)
            Set.empty
            (Map.singleton Warning 1)
          opts = testOutputOptions
          output = renderSarif opts result
      T.isInfixOf "Replace head with headMay" output `shouldBe` True
      T.isInfixOf "headMay" output `shouldBe` True

  describe "multiple files" $ do
    it "includes results from multiple files" $ do
      let diag1 = mkTestDiagnostic "file1.hs" 1 1 1 10 "Warning 1" Warning
          diag2 = mkTestDiagnostic "file2.hs" 2 1 2 10 "Warning 2" Warning
          fr1 = FileResult "file1.hs" [diag1] [] [] []
          fr2 = FileResult "file2.hs" [diag2] [] [] []
          result = AnalysisResult
            (Map.fromList [("file1.hs", fr1), ("file2.hs", fr2)])
            Set.empty
            (Map.singleton Warning 2)
          opts = testOutputOptions
          output = renderSarif opts result
      T.isInfixOf "file1.hs" output `shouldBe` True
      T.isInfixOf "file2.hs" output `shouldBe` True
      T.isInfixOf "Warning 1" output `shouldBe` True
      T.isInfixOf "Warning 2" output `shouldBe` True

  describe "valid JSON output" $ do
    it "produces parseable JSON for complex results" $ do
      let diag = mkTestDiagnostic "test.hs" 1 1 1 10 "Message with \"quotes\"" Warning
          fileResult = FileResult "test.hs" [diag] [] [] []
          result = AnalysisResult
            (Map.singleton "test.hs" fileResult)
            Set.empty
            (Map.singleton Warning 1)
          opts = testOutputOptions
          output = renderSarif opts result
      (decode (BL.fromStrict $ TE.encodeUtf8 output) :: Maybe A.Value) `shouldSatisfy` (/= Nothing)

    it "handles unicode in messages" $ do
      let diag = mkTestDiagnostic "test.hs" 1 1 1 10 "Unicode: λ → ∀" Warning
          fileResult = FileResult "test.hs" [diag] [] [] []
          result = AnalysisResult
            (Map.singleton "test.hs" fileResult)
            Set.empty
            (Map.singleton Warning 1)
          opts = testOutputOptions
          output = renderSarif opts result
      (decode (BL.fromStrict $ TE.encodeUtf8 output) :: Maybe A.Value) `shouldSatisfy` (/= Nothing)

  describe "diagnostic kinds" $ do
    it "handles all diagnostic kinds" $ do
      let kinds = [NamingConvention, UnusedCode, UnusedImport, RedundantCode,
                   CodePattern, TypeSignature, ImportStyle, TemplateHaskellRef,
                   SecurityIssue, PerformanceIssue, ArchitecturalIssue,
                   SpaceLeak, PartialFunction, ComplexityIssue, Custom "custom"]
          diags = zipWith (\i k -> (mkTestDiagnostic "test.hs" i 1 i 10 ("Kind: " <> T.pack (show k)) Warning) { diagKind = k }) [1..] kinds
          fileResult = FileResult "test.hs" diags [] [] []
          result = AnalysisResult
            (Map.singleton "test.hs" fileResult)
            Set.empty
            (Map.singleton Warning (length kinds))
          opts = testOutputOptions
          output = renderSarif opts result
      -- Should produce valid JSON
      (decode (BL.fromStrict $ TE.encodeUtf8 output) :: Maybe A.Value) `shouldSatisfy` (/= Nothing)

  describe "severity levels" $ do
    it "handles all severity levels" $ do
      let sev1 = mkTestDiagnostic "test.hs" 1 1 1 10 "Error" Error
          sev2 = mkTestDiagnostic "test.hs" 2 1 2 10 "Warning" Warning
          sev3 = mkTestDiagnostic "test.hs" 3 1 3 10 "Suggestion" Suggestion
          sev4 = mkTestDiagnostic "test.hs" 4 1 4 10 "Info" Info
          fileResult = FileResult "test.hs" [sev1, sev2, sev3, sev4] [] [] []
          result = AnalysisResult
            (Map.singleton "test.hs" fileResult)
            Set.empty
            (Map.fromList [(Error, 1), (Warning, 1), (Suggestion, 1), (Info, 1)])
          opts = testOutputOptions
          output = renderSarif opts result
      T.isInfixOf "error" output `shouldBe` True
      T.isInfixOf "warning" output `shouldBe` True
      T.isInfixOf "note" output `shouldBe` True  -- Suggestion and Info map to "note"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Create a test diagnostic
mkTestDiagnostic :: Text -> Int -> Int -> Int -> Int -> Text -> Severity -> Diagnostic
mkTestDiagnostic file sl sc el ec msg sev = Diagnostic
  { diagSpan = mkSrcSpanRaw (T.unpack file) sl sc el ec
  , diagSeverity = sev
  , diagKind = CodePattern
  , diagMessage = msg
  , diagCode = Just "test/rule"
  , diagFixes = []
  , diagRelated = []
  }

-- | Create test driver info
mkTestDriver :: SarifDriver
mkTestDriver = SarifDriver
  { sdName = "test-tool"
  , sdVersion = "1.0.0"
  , sdInformationUri = "https://example.com"
  , sdRules = []
  }

-- | Create test tool info
mkTestTool :: SarifTool
mkTestTool = SarifTool
  { stDriver = mkTestDriver
  }

-- | Create test result
mkTestResult :: Text -> Text -> Text -> SarifResult
mkTestResult ruleId level msg = SarifResult
  { sresRuleId = ruleId
  , sresLevel = level
  , sresMessage = SarifMessage msg
  , sresLocations = [mkTestLocation "test.hs" 1 1 1 10]
  , sresFixes = []
  , sresPartialFingerprints = Nothing
  }

-- | Create test location
mkTestLocation :: Text -> Int -> Int -> Int -> Int -> SarifLocation
mkTestLocation uri sl sc el ec = SarifLocation
  { slPhysicalLocation = SarifPhysicalLocation
      { splArtifactLocation = SarifArtifactLocation uri
      , splRegion = SarifRegion sl sc el ec
      }
  }

-- | Test output options
testOutputOptions :: OutputOptions
testOutputOptions = OutputOptions
  { ooFormat = SarifFormat
  , ooColor = False
  , ooGroupBy = "file"
  , ooShowContext = False
  , ooContextLines = 0
  , ooVerbose = False
  , ooSourceCache = Map.empty
  }
