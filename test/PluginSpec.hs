{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : PluginSpec
-- Description : Tests for Argus.Plugin GHC plugin functionality
-- Copyright   : (c) 2024
-- License     : MIT

module PluginSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Test.Hspec

import Argus.Plugin
import Argus.Types (Severity(..), SrcSpan(..), noSrcSpan, DiagnosticKind(..), mkSrcSpanRaw)

spec :: Spec
spec = do
  describe "Argus.Plugin" $ do
    configParsingSpec
    defaultConfigSpec
    diagnosticFormattingSpec
    fixTypesSpec
    typeInfoJsonSpec
    constraintsSpec
    pluginDiagnosticSpec
    configComparisonSpec
    jsonDiagnosticSpec

--------------------------------------------------------------------------------
-- Configuration Parsing Tests
--------------------------------------------------------------------------------

configParsingSpec :: Spec
configParsingSpec = describe "parsePluginOptions" $ do
  it "parses empty options to default config" $ do
    let cfg = parsePluginOptions []
    cfg `shouldBe` defaultPluginConfig

  it "parses config file option" $ do
    let cfg = parsePluginOptions ["config=/path/to/config.yaml"]
    pcConfigFile cfg `shouldBe` Just "/path/to/config.yaml"

  it "parses severity=error option" $ do
    let cfg = parsePluginOptions ["severity=error"]
    pcMinSeverity cfg `shouldBe` Error

  it "parses severity=warning option" $ do
    let cfg = parsePluginOptions ["severity=warning"]
    pcMinSeverity cfg `shouldBe` Warning

  it "parses severity=suggestion option" $ do
    let cfg = parsePluginOptions ["severity=suggestion"]
    pcMinSeverity cfg `shouldBe` Suggestion

  it "parses severity=info option" $ do
    let cfg = parsePluginOptions ["severity=info"]
    pcMinSeverity cfg `shouldBe` Info

  it "defaults to warning for unknown severity" $ do
    let cfg = parsePluginOptions ["severity=unknown"]
    pcMinSeverity cfg `shouldBe` Warning

  it "parses output format option" $ do
    let cfg = parsePluginOptions ["output=json"]
    pcOutputFormat cfg `shouldBe` "json"

  it "parses fail-on-error flag" $ do
    let cfg = parsePluginOptions ["fail-on-error"]
    pcFailOnError cfg `shouldBe` True

  it "parses verbose flag" $ do
    let cfg = parsePluginOptions ["verbose"]
    pcVerbose cfg `shouldBe` True

  it "parses capture-types flag" $ do
    let cfg = parsePluginOptions ["capture-types"]
    pcCaptureTypes cfg `shouldBe` True

  it "parses type-output option" $ do
    let cfg = parsePluginOptions ["type-output=/path/to/types.json"]
    pcTypeOutput cfg `shouldBe` Just "/path/to/types.json"

  it "parses detect-thunks flag" $ do
    let cfg = parsePluginOptions ["detect-thunks"]
    pcDetectThunks cfg `shouldBe` True

  it "parses no-detect-thunks flag" $ do
    let cfg = parsePluginOptions ["no-detect-thunks"]
    pcDetectThunks cfg `shouldBe` False

  it "parses strictness-check flag" $ do
    let cfg = parsePluginOptions ["strictness-check"]
    pcStrictnessCheck cfg `shouldBe` True

  it "parses no-strictness-check flag" $ do
    let cfg = parsePluginOptions ["no-strictness-check"]
    pcStrictnessCheck cfg `shouldBe` False

  it "parses multiple options" $ do
    let cfg = parsePluginOptions
          [ "config=/etc/argus.yaml"
          , "severity=error"
          , "output=json"
          , "verbose"
          , "fail-on-error"
          ]
    pcConfigFile cfg `shouldBe` Just "/etc/argus.yaml"
    pcMinSeverity cfg `shouldBe` Error
    pcOutputFormat cfg `shouldBe` "json"
    pcVerbose cfg `shouldBe` True
    pcFailOnError cfg `shouldBe` True

  it "earlier options override later ones (foldr semantics)" $ do
    let cfg = parsePluginOptions
          [ "severity=error"
          , "severity=warning"
          ]
    -- With foldr, the first element is applied last, so it wins
    pcMinSeverity cfg `shouldBe` Error

  it "ignores unknown options" $ do
    let cfg = parsePluginOptions ["unknown-option=value", "severity=error"]
    pcMinSeverity cfg `shouldBe` Error
    -- Other fields should remain at defaults
    pcVerbose cfg `shouldBe` False

--------------------------------------------------------------------------------
-- Default Configuration Tests
--------------------------------------------------------------------------------

defaultConfigSpec :: Spec
defaultConfigSpec = describe "defaultPluginConfig" $ do
  it "has no config file by default" $ do
    pcConfigFile defaultPluginConfig `shouldBe` Nothing

  it "has Warning as default severity" $ do
    pcMinSeverity defaultPluginConfig `shouldBe` Warning

  it "has text as default output format" $ do
    pcOutputFormat defaultPluginConfig `shouldBe` "text"

  it "has fail-on-error disabled by default" $ do
    pcFailOnError defaultPluginConfig `shouldBe` False

  it "has verbose disabled by default" $ do
    pcVerbose defaultPluginConfig `shouldBe` False

  it "has capture-types disabled by default" $ do
    pcCaptureTypes defaultPluginConfig `shouldBe` False

  it "has no type output path by default" $ do
    pcTypeOutput defaultPluginConfig `shouldBe` Nothing

  it "has detect-thunks enabled by default" $ do
    pcDetectThunks defaultPluginConfig `shouldBe` True

  it "has strictness-check enabled by default" $ do
    pcStrictnessCheck defaultPluginConfig `shouldBe` True

--------------------------------------------------------------------------------
-- Diagnostic Formatting Tests
--------------------------------------------------------------------------------

diagnosticFormattingSpec :: Spec
diagnosticFormattingSpec = describe "formatPluginDiagnostic" $ do
  it "formats error diagnostic correctly" $ do
    let diag = PluginDiagnostic
          { pdSpan = mkTestSpan "Test.hs" 10 5 10 20
          , pdSeverity = Error
          , pdKind = PartialFunction
          , pdMessage = "Use of partial function 'head'"
          , pdCode = Just "plugin/partial/head"
          , pdFixes = []
          }
        formatted = formatPluginDiagnostic diag
    T.isInfixOf "Test.hs:10:5" formatted `shouldBe` True
    T.isInfixOf "error" formatted `shouldBe` True
    T.isInfixOf "[plugin/partial/head]" formatted `shouldBe` True
    T.isInfixOf "Use of partial function 'head'" formatted `shouldBe` True

  it "formats warning diagnostic correctly" $ do
    let diag = PluginDiagnostic
          { pdSpan = mkTestSpan "Lib.hs" 42 1 42 50
          , pdSeverity = Warning
          , pdKind = SecurityIssue
          , pdMessage = "Potentially unsafe operation"
          , pdCode = Just "plugin/unsafe/coerce"
          , pdFixes = []
          }
        formatted = formatPluginDiagnostic diag
    T.isInfixOf "Lib.hs:42:1" formatted `shouldBe` True
    T.isInfixOf "warning" formatted `shouldBe` True
    T.isInfixOf "[plugin/unsafe/coerce]" formatted `shouldBe` True

  it "formats suggestion diagnostic correctly" $ do
    let diag = PluginDiagnostic
          { pdSpan = mkTestSpan "Main.hs" 5 10 5 20
          , pdSeverity = Suggestion
          , pdKind = SpaceLeak
          , pdMessage = "Consider using foldl' instead"
          , pdCode = Just "plugin/spaceleak/foldl"
          , pdFixes = []
          }
        formatted = formatPluginDiagnostic diag
    T.isInfixOf "suggestion" formatted `shouldBe` True

  it "formats info diagnostic correctly" $ do
    let diag = PluginDiagnostic
          { pdSpan = mkTestSpan "Util.hs" 100 3 100 15
          , pdSeverity = Info
          , pdKind = PerformanceIssue
          , pdMessage = "Performance hint"
          , pdCode = Nothing
          , pdFixes = []
          }
        formatted = formatPluginDiagnostic diag
    T.isInfixOf "info" formatted `shouldBe` True
    T.isInfixOf "Performance hint" formatted `shouldBe` True

  it "handles diagnostic without code" $ do
    let diag = PluginDiagnostic
          { pdSpan = mkTestSpan "Test.hs" 1 1 1 10
          , pdSeverity = Warning
          , pdKind = NamingConvention
          , pdMessage = "Consider renaming"
          , pdCode = Nothing
          , pdFixes = []
          }
        formatted = formatPluginDiagnostic diag
    -- Should not have empty brackets
    T.isInfixOf "[]" formatted `shouldBe` False
    T.isInfixOf "Consider renaming" formatted `shouldBe` True

  it "formats diagnostic with fixes" $ do
    let fix = PluginFix
          { pfTitle = "Replace 'head' with 'headMay'"
          , pfSpan = mkTestSpan "Test.hs" 10 5 10 9
          , pfReplacement = "headMay"
          , pfImportAdd = ["Safe"]
          , pfIsPreferred = True
          , pfCategory = FCPSafety
          , pfSafety = FSPAlways
          }
        diag = PluginDiagnostic
          { pdSpan = mkTestSpan "Test.hs" 10 5 10 9
          , pdSeverity = Warning
          , pdKind = PartialFunction
          , pdMessage = "Use of partial function 'head'"
          , pdCode = Just "plugin/partial/head"
          , pdFixes = [fix]
          }
        formatted = formatPluginDiagnostic diag
    T.isInfixOf "Fixes available" formatted `shouldBe` True
    T.isInfixOf "[safe]" formatted `shouldBe` True
    T.isInfixOf "Replace 'head' with 'headMay'" formatted `shouldBe` True
    T.isInfixOf "Safe" formatted `shouldBe` True

  it "formats diagnostic with multiple fixes" $ do
    let fix1 = PluginFix
          { pfTitle = "Use headMay"
          , pfSpan = mkTestSpan "Test.hs" 10 5 10 9
          , pfReplacement = "headMay"
          , pfImportAdd = ["Safe"]
          , pfIsPreferred = True
          , pfCategory = FCPSafety
          , pfSafety = FSPAlways
          }
        fix2 = PluginFix
          { pfTitle = "Use pattern matching"
          , pfSpan = mkTestSpan "Test.hs" 10 5 10 9
          , pfReplacement = "case xs of { (x:_) -> x; [] -> error \"empty\" }"
          , pfImportAdd = []
          , pfIsPreferred = False
          , pfCategory = FCPSafety
          , pfSafety = FSPReview
          }
        diag = PluginDiagnostic
          { pdSpan = mkTestSpan "Test.hs" 10 5 10 9
          , pdSeverity = Warning
          , pdKind = PartialFunction
          , pdMessage = "Use of partial function 'head'"
          , pdCode = Just "plugin/partial/head"
          , pdFixes = [fix1, fix2]
          }
        formatted = formatPluginDiagnostic diag
    T.isInfixOf "Use headMay" formatted `shouldBe` True
    T.isInfixOf "Use pattern matching" formatted `shouldBe` True
    T.isInfixOf "[review]" formatted `shouldBe` True

--------------------------------------------------------------------------------
-- Fix Types Tests
--------------------------------------------------------------------------------

fixTypesSpec :: Spec
fixTypesSpec = describe "Fix types" $ do
  describe "PluginFix" $ do
    it "constructs with all safety levels" $ do
      let mkFix safety = PluginFix
            { pfTitle = "Test fix"
            , pfSpan = noSrcSpan
            , pfReplacement = "replacement"
            , pfImportAdd = []
            , pfIsPreferred = True
            , pfCategory = FCPSafety
            , pfSafety = safety
            }
      pfSafety (mkFix FSPAlways) `shouldBe` FSPAlways
      pfSafety (mkFix FSPMostly) `shouldBe` FSPMostly
      pfSafety (mkFix FSPReview) `shouldBe` FSPReview

    it "constructs with all category types" $ do
      let mkFix cat = PluginFix
            { pfTitle = "Test fix"
            , pfSpan = noSrcSpan
            , pfReplacement = "replacement"
            , pfImportAdd = []
            , pfIsPreferred = True
            , pfCategory = cat
            , pfSafety = FSPAlways
            }
      pfCategory (mkFix FCPSafety) `shouldBe` FCPSafety
      pfCategory (mkFix FCPPerformance) `shouldBe` FCPPerformance
      pfCategory (mkFix FCPSecurity) `shouldBe` FCPSecurity
      pfCategory (mkFix FCPStyle) `shouldBe` FCPStyle

    it "handles multiple imports" $ do
      let fix = PluginFix
            { pfTitle = "Complex fix"
            , pfSpan = noSrcSpan
            , pfReplacement = "newCode"
            , pfImportAdd = ["Data.List", "Data.Maybe", "Control.Monad"]
            , pfIsPreferred = False
            , pfCategory = FCPPerformance
            , pfSafety = FSPMostly
            }
      length (pfImportAdd fix) `shouldBe` 3

    it "round-trips through JSON" $ do
      let fix = PluginFix
            { pfTitle = "Replace foldl with foldl'"
            , pfSpan = mkTestSpan "Lib.hs" 15 3 15 8
            , pfReplacement = "foldl'"
            , pfImportAdd = ["Data.List"]
            , pfIsPreferred = True
            , pfCategory = FCPPerformance
            , pfSafety = FSPAlways
            }
      Aeson.decode (Aeson.encode fix) `shouldBe` Just fix

  describe "FixCategoryP" $ do
    it "all categories are distinct" $ do
      FCPSafety `shouldNotBe` FCPPerformance
      FCPPerformance `shouldNotBe` FCPSecurity
      FCPSecurity `shouldNotBe` FCPStyle
      FCPStyle `shouldNotBe` FCPSafety

    it "round-trips through JSON" $ do
      let categories = [FCPSafety, FCPPerformance, FCPSecurity, FCPStyle]
      forM_ categories $ \cat ->
        Aeson.decode (Aeson.encode cat) `shouldBe` Just cat

  describe "FixSafetyP" $ do
    it "all safety levels are distinct" $ do
      FSPAlways `shouldNotBe` FSPMostly
      FSPMostly `shouldNotBe` FSPReview
      FSPReview `shouldNotBe` FSPAlways

    it "round-trips through JSON" $ do
      let levels = [FSPAlways, FSPMostly, FSPReview]
      forM_ levels $ \level ->
        Aeson.decode (Aeson.encode level) `shouldBe` Just level

--------------------------------------------------------------------------------
-- Type Info JSON Serialization Tests
--------------------------------------------------------------------------------

typeInfoJsonSpec :: Spec
typeInfoJsonSpec = describe "Type Info JSON serialization" $ do
  describe "CapturedTypeInfo" $ do
    it "serializes to JSON correctly" $ do
      let info = CapturedTypeInfo
            { ctiModuleName = "Data.Text"
            , ctiSymbols = []
            , ctiImports = ["Prelude", "Data.List"]
            , ctiExports = ["pack", "unpack"]
            }
          encoded = Aeson.encode info
      BL.length encoded `shouldSatisfy` (> 0)
      -- Should contain all fields
      containsText "Data.Text" encoded `shouldBe` True
      containsText "pack" encoded `shouldBe` True

    it "round-trips through JSON" $ do
      let info = CapturedTypeInfo
            { ctiModuleName = "Test.Module"
            , ctiSymbols = []
            , ctiImports = ["A", "B"]
            , ctiExports = ["foo", "bar"]
            }
      Aeson.decode (Aeson.encode info) `shouldBe` Just info

  describe "CapturedSymbol" $ do
    it "serializes to JSON correctly" $ do
      let symbol = CapturedSymbol
            { csName = "myFunction"
            , csType = "Int -> String -> IO ()"
            , csSpan = mkTestSpan "Lib.hs" 20 1 20 50
            , csConstraints = emptyConstraints
            , csIsStrict = False
            , csArity = 2
            , csIsExported = True
            }
          encoded = Aeson.encode symbol
      containsText "myFunction" encoded `shouldBe` True
      containsText "Int -> String -> IO ()" encoded `shouldBe` True

    it "round-trips through JSON" $ do
      let symbol = CapturedSymbol
            { csName = "helper"
            , csType = "a -> b"
            , csSpan = noSrcSpan
            , csConstraints = emptyConstraints { scEq = True, scShow = True }
            , csIsStrict = True
            , csArity = 1
            , csIsExported = False
            }
      Aeson.decode (Aeson.encode symbol) `shouldBe` Just symbol

  describe "SymbolConstraints" $ do
    it "serializes empty constraints" $ do
      let encoded = Aeson.encode emptyConstraints
      BL.length encoded `shouldSatisfy` (> 0)

    it "round-trips with various constraints" $ do
      let constraints = SymbolConstraints
            { scEq = True
            , scOrd = True
            , scShow = False
            , scRead = False
            , scMonoid = True
            , scSemigroup = True
            , scHashable = False
            , scNFData = True
            , scOther = ["Typeable", "Generic"]
            }
      Aeson.decode (Aeson.encode constraints) `shouldBe` Just constraints

  describe "CapturedTypeInfo with symbols" $ do
    it "serializes complete type info" $ do
      let symbol1 = CapturedSymbol
            { csName = "main"
            , csType = "IO ()"
            , csSpan = mkTestSpan "Main.hs" 1 1 1 4
            , csConstraints = emptyConstraints
            , csIsStrict = False
            , csArity = 0
            , csIsExported = True
            }
          symbol2 = CapturedSymbol
            { csName = "helper"
            , csType = "(Eq a, Show a) => a -> String"
            , csSpan = mkTestSpan "Main.hs" 10 1 10 20
            , csConstraints = emptyConstraints { scEq = True, scShow = True }
            , csIsStrict = False
            , csArity = 1
            , csIsExported = False
            }
          info = CapturedTypeInfo
            { ctiModuleName = "Main"
            , ctiSymbols = [symbol1, symbol2]
            , ctiImports = ["Prelude"]
            , ctiExports = ["main"]
            }
      Aeson.decode (Aeson.encode info) `shouldBe` Just info

--------------------------------------------------------------------------------
-- Constraints Tests
--------------------------------------------------------------------------------

constraintsSpec :: Spec
constraintsSpec = describe "emptyConstraints" $ do
  it "has no Eq constraint" $ do
    scEq emptyConstraints `shouldBe` False

  it "has no Ord constraint" $ do
    scOrd emptyConstraints `shouldBe` False

  it "has no Show constraint" $ do
    scShow emptyConstraints `shouldBe` False

  it "has no Read constraint" $ do
    scRead emptyConstraints `shouldBe` False

  it "has no Monoid constraint" $ do
    scMonoid emptyConstraints `shouldBe` False

  it "has no Semigroup constraint" $ do
    scSemigroup emptyConstraints `shouldBe` False

  it "has no Hashable constraint" $ do
    scHashable emptyConstraints `shouldBe` False

  it "has no NFData constraint" $ do
    scNFData emptyConstraints `shouldBe` False

  it "has empty other constraints list" $ do
    scOther emptyConstraints `shouldBe` []

--------------------------------------------------------------------------------
-- Plugin Diagnostic Type Tests
--------------------------------------------------------------------------------

pluginDiagnosticSpec :: Spec
pluginDiagnosticSpec = describe "PluginDiagnostic" $ do
  it "can be constructed with all diagnostic kinds" $ do
    let kinds = [PartialFunction, SecurityIssue, SpaceLeak, PerformanceIssue, NamingConvention]
    forM_ kinds $ \kind -> do
      let diag = PluginDiagnostic
            { pdSpan = noSrcSpan
            , pdSeverity = Warning
            , pdKind = kind
            , pdMessage = "Test message"
            , pdCode = Just "test/code"
            , pdFixes = []
            }
      pdKind diag `shouldBe` kind

  it "supports all severity levels" $ do
    let severities = [Error, Warning, Suggestion, Info]
    forM_ severities $ \sev -> do
      let diag = PluginDiagnostic
            { pdSpan = noSrcSpan
            , pdSeverity = sev
            , pdKind = PartialFunction
            , pdMessage = "Test"
            , pdCode = Nothing
            , pdFixes = []
            }
      pdSeverity diag `shouldBe` sev

  it "equality works correctly" $ do
    let diag1 = PluginDiagnostic
          { pdSpan = mkTestSpan "A.hs" 1 1 1 10
          , pdSeverity = Error
          , pdKind = PartialFunction
          , pdMessage = "Same message"
          , pdCode = Just "same/code"
          , pdFixes = []
          }
        diag2 = diag1
        diag3 = diag1 { pdMessage = "Different message" }
    diag1 `shouldBe` diag2
    diag1 `shouldNotBe` diag3

  it "handles empty file path in span" $ do
    let diag = PluginDiagnostic
          { pdSpan = mkTestSpan "" 0 0 0 0
          , pdSeverity = Warning
          , pdKind = NamingConvention
          , pdMessage = "Test"
          , pdCode = Nothing
          , pdFixes = []
          }
        formatted = formatPluginDiagnostic diag
    T.isInfixOf ":0:0" formatted `shouldBe` True

  it "round-trips through JSON with fixes" $ do
    let fix = PluginFix
          { pfTitle = "Test fix"
          , pfSpan = mkTestSpan "Test.hs" 5 1 5 10
          , pfReplacement = "safeVersion"
          , pfImportAdd = ["Safe"]
          , pfIsPreferred = True
          , pfCategory = FCPSafety
          , pfSafety = FSPAlways
          }
        diag = PluginDiagnostic
          { pdSpan = mkTestSpan "Test.hs" 5 1 5 10
          , pdSeverity = Warning
          , pdKind = PartialFunction
          , pdMessage = "Partial function usage"
          , pdCode = Just "partial/test"
          , pdFixes = [fix]
          }
    Aeson.decode (Aeson.encode diag) `shouldBe` Just diag

--------------------------------------------------------------------------------
-- Configuration Comparison Tests
--------------------------------------------------------------------------------

configComparisonSpec :: Spec
configComparisonSpec = describe "PluginConfig equality" $ do
  it "default config equals itself" $ do
    defaultPluginConfig `shouldBe` defaultPluginConfig

  it "configs with different severity are not equal" $ do
    let cfg1 = defaultPluginConfig { pcMinSeverity = Error }
        cfg2 = defaultPluginConfig { pcMinSeverity = Warning }
    cfg1 `shouldNotBe` cfg2

  it "configs with different options are not equal" $ do
    let cfg1 = defaultPluginConfig { pcVerbose = True }
        cfg2 = defaultPluginConfig { pcVerbose = False }
    cfg1 `shouldNotBe` cfg2

--------------------------------------------------------------------------------
-- JSON Diagnostic Output Tests
--------------------------------------------------------------------------------

jsonDiagnosticSpec :: Spec
jsonDiagnosticSpec = describe "formatPluginDiagnosticJson" $ do
  it "produces valid JSON output" $ do
    let diag = PluginDiagnostic
          { pdSpan = mkTestSpan "Test.hs" 10 5 10 20
          , pdSeverity = Warning
          , pdKind = PartialFunction
          , pdMessage = "Use of partial function 'head'"
          , pdCode = Just "plugin/partial/head"
          , pdFixes = []
          }
        jsonOutput = formatPluginDiagnosticJson diag
    -- Should be parseable back to the same structure
    let decoded = Aeson.decode (BL.fromStrict $ TE.encodeUtf8 jsonOutput) :: Maybe PluginDiagnostic
    decoded `shouldBe` Just diag

  it "includes fix information in JSON" $ do
    let fix = PluginFix
          { pfTitle = "Replace 'head' with 'headMay'"
          , pfSpan = mkTestSpan "Test.hs" 10 5 10 9
          , pfReplacement = "headMay"
          , pfImportAdd = ["Safe"]
          , pfIsPreferred = True
          , pfCategory = FCPSafety
          , pfSafety = FSPAlways
          }
        diag = PluginDiagnostic
          { pdSpan = mkTestSpan "Test.hs" 10 5 10 9
          , pdSeverity = Warning
          , pdKind = PartialFunction
          , pdMessage = "Use of partial function 'head'"
          , pdCode = Just "plugin/partial/head"
          , pdFixes = [fix]
          }
        jsonOutput = formatPluginDiagnosticJson diag
    T.isInfixOf "headMay" jsonOutput `shouldBe` True
    T.isInfixOf "Safe" jsonOutput `shouldBe` True
    T.isInfixOf "FCPSafety" jsonOutput `shouldBe` True

  it "handles diagnostic without fixes" $ do
    let diag = PluginDiagnostic
          { pdSpan = mkTestSpan "Lib.hs" 5 1 5 10
          , pdSeverity = Info
          , pdKind = PerformanceIssue
          , pdMessage = "Performance hint"
          , pdCode = Nothing
          , pdFixes = []
          }
        jsonOutput = formatPluginDiagnosticJson diag
    T.isInfixOf "pdFixes" jsonOutput `shouldBe` True
    T.isInfixOf "[]" jsonOutput `shouldBe` True

  it "handles multiple fixes in JSON" $ do
    let fix1 = PluginFix
          { pfTitle = "Option 1"
          , pfSpan = noSrcSpan
          , pfReplacement = "option1"
          , pfImportAdd = []
          , pfIsPreferred = True
          , pfCategory = FCPSafety
          , pfSafety = FSPAlways
          }
        fix2 = PluginFix
          { pfTitle = "Option 2"
          , pfSpan = noSrcSpan
          , pfReplacement = "option2"
          , pfImportAdd = ["Module.A", "Module.B"]
          , pfIsPreferred = False
          , pfCategory = FCPPerformance
          , pfSafety = FSPReview
          }
        diag = PluginDiagnostic
          { pdSpan = noSrcSpan
          , pdSeverity = Warning
          , pdKind = SpaceLeak
          , pdMessage = "Choose a fix"
          , pdCode = Just "multi/fix"
          , pdFixes = [fix1, fix2]
          }
        jsonOutput = formatPluginDiagnosticJson diag
    T.isInfixOf "Option 1" jsonOutput `shouldBe` True
    T.isInfixOf "Option 2" jsonOutput `shouldBe` True
    T.isInfixOf "Module.A" jsonOutput `shouldBe` True

  it "escapes special characters in JSON" $ do
    let diag = PluginDiagnostic
          { pdSpan = noSrcSpan
          , pdSeverity = Warning
          , pdKind = NamingConvention
          , pdMessage = "Message with \"quotes\" and \\backslash"
          , pdCode = Just "test/escape"
          , pdFixes = []
          }
        jsonOutput = formatPluginDiagnosticJson diag
    -- JSON should properly escape these characters
    let decoded = Aeson.decode (BL.fromStrict $ TE.encodeUtf8 jsonOutput) :: Maybe PluginDiagnostic
    fmap pdMessage decoded `shouldBe` Just "Message with \"quotes\" and \\backslash"

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Create a test SrcSpan
mkTestSpan :: FilePath -> Int -> Int -> Int -> Int -> SrcSpan
mkTestSpan = mkSrcSpanRaw

-- | Check if a Text is contained in a Lazy ByteString (converted via UTF-8)
containsText :: T.Text -> BL.ByteString -> Bool
containsText needle haystack =
  let haystackText = TE.decodeUtf8 $ BL.toStrict haystack
  in T.isInfixOf needle haystackText

-- | Helper to iterate over items in tests
forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_
