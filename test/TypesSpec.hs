{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TypesSpec
-- Description : Tests for Argus.Types
--
-- Comprehensive tests for core types, source spans, diagnostics, fixes,
-- symbols, analysis results, and JSON serialization.
module TypesSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, FromJSON, ToJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Maybe (isJust, isNothing)

import Argus.Types

spec :: Spec
spec = do
  describe "Argus.Types" $ do
    newtypeSpec
    srcSpanSpec
    severitySpec
    diagnosticKindSpec
    importSymbolTypeSpec
    importSymbolSpec
    fixImportSpec
    fixCategorySpec
    fixSafetySpec
    fixSpec
    diagnosticSpec
    symbolKindSpec
    qualifiedNameSpec
    symbolSpec
    fileResultSpec
    analysisResultSpec
    analysisModeSpec
    argusOptionsSpec

--------------------------------------------------------------------------------
-- Newtypes
--------------------------------------------------------------------------------

newtypeSpec :: Spec
newtypeSpec = describe "Newtypes" $ do
  describe "Line" $ do
    it "wraps Int correctly" $ do
      unLine (Line 42) `shouldBe` 42

    it "supports Num operations" $ do
      Line 10 + Line 5 `shouldBe` Line 15
      Line 10 - Line 3 `shouldBe` Line 7
      Line 3 * Line 4 `shouldBe` Line 12

    it "supports Ord comparison" $ do
      Line 5 < Line 10 `shouldBe` True
      Line 10 > Line 5 `shouldBe` True
      Line 5 == Line 5 `shouldBe` True

    it "supports Enum" $ do
      succ (Line 1) `shouldBe` Line 2
      pred (Line 5) `shouldBe` Line 4

    it "has JSON roundtrip" $ do
      shouldRoundTrip (Line 100)

  describe "Column" $ do
    it "wraps Int correctly" $ do
      unColumn (Column 10) `shouldBe` 10

    it "supports Num operations" $ do
      Column 5 + Column 3 `shouldBe` Column 8

    it "has JSON roundtrip" $ do
      shouldRoundTrip (Column 50)

  describe "Seconds" $ do
    it "wraps Double correctly" $ do
      unSeconds (Seconds 3.14) `shouldBe` 3.14

    it "supports Fractional operations" $ do
      Seconds 10 / Seconds 2 `shouldBe` Seconds 5

    it "has JSON roundtrip" $ do
      shouldRoundTrip (Seconds 1.5)

  describe "Milliseconds" $ do
    it "wraps Double correctly" $ do
      unMilliseconds (Milliseconds 500.0) `shouldBe` 500.0

    it "supports Fractional operations" $ do
      Milliseconds 100 * Milliseconds 2 `shouldBe` Milliseconds 200

    it "has JSON roundtrip" $ do
      shouldRoundTrip (Milliseconds 250.5)

--------------------------------------------------------------------------------
-- SrcSpan
--------------------------------------------------------------------------------

srcSpanSpec :: Spec
srcSpanSpec = describe "SrcSpan" $ do
  describe "mkSrcSpan" $ do
    it "creates span from two SrcLocs" $ do
      let start = SrcLoc "test.hs" (Line 1) (Column 1)
          end = SrcLoc "test.hs" (Line 1) (Column 10)
          span' = mkSrcSpan start end
      srcSpanFile span' `shouldBe` "test.hs"
      srcSpanStartLine span' `shouldBe` Line 1
      srcSpanStartCol span' `shouldBe` Column 1
      srcSpanEndLine span' `shouldBe` Line 1
      srcSpanEndCol span' `shouldBe` Column 10

  describe "mkSrcSpanRaw" $ do
    it "creates span from raw Int values" $ do
      let span' = mkSrcSpanRaw "test.hs" 5 10 5 25
      srcSpanStartLineRaw span' `shouldBe` 5
      srcSpanStartColRaw span' `shouldBe` 10
      srcSpanEndLineRaw span' `shouldBe` 5
      srcSpanEndColRaw span' `shouldBe` 25

    it "preserves file path" $ do
      let span' = mkSrcSpanRaw "/path/to/Module.hs" 1 1 10 20
      srcSpanFile span' `shouldBe` "/path/to/Module.hs"

  describe "srcSpanStart" $ do
    it "extracts start location" $ do
      let span' = mkSrcSpanRaw "test.hs" 3 5 10 20
          start = srcSpanStart span'
      srcLocFile start `shouldBe` "test.hs"
      srcLocLine start `shouldBe` Line 3
      srcLocColumn start `shouldBe` Column 5

  describe "srcSpanEnd" $ do
    it "extracts end location" $ do
      let span' = mkSrcSpanRaw "test.hs" 3 5 10 20
          end = srcSpanEnd span'
      srcLocFile end `shouldBe` "test.hs"
      srcLocLine end `shouldBe` Line 10
      srcLocColumn end `shouldBe` Column 20

  describe "noSrcSpan" $ do
    it "has empty file path" $ do
      srcSpanFile noSrcSpan `shouldBe` ""

    it "has zero positions" $ do
      srcSpanStartLineRaw noSrcSpan `shouldBe` 0
      srcSpanStartColRaw noSrcSpan `shouldBe` 0
      srcSpanEndLineRaw noSrcSpan `shouldBe` 0
      srcSpanEndColRaw noSrcSpan `shouldBe` 0

  describe "Raw accessors" $ do
    it "srcSpanStartLineRaw returns Int" $ do
      let span' = mkSrcSpanRaw "f.hs" 42 1 42 10
      srcSpanStartLineRaw span' `shouldBe` 42

    it "srcSpanStartColRaw returns Int" $ do
      let span' = mkSrcSpanRaw "f.hs" 1 15 1 20
      srcSpanStartColRaw span' `shouldBe` 15

    it "srcSpanEndLineRaw returns Int" $ do
      let span' = mkSrcSpanRaw "f.hs" 1 1 99 1
      srcSpanEndLineRaw span' `shouldBe` 99

    it "srcSpanEndColRaw returns Int" $ do
      let span' = mkSrcSpanRaw "f.hs" 1 1 1 55
      srcSpanEndColRaw span' `shouldBe` 55

  describe "JSON" $ do
    it "has JSON roundtrip" $ do
      shouldRoundTrip (mkSrcSpanRaw "test.hs" 1 2 3 4)

    it "has SrcLoc JSON roundtrip" $ do
      shouldRoundTrip (SrcLoc "module.hs" (Line 10) (Column 5))

--------------------------------------------------------------------------------
-- Severity
--------------------------------------------------------------------------------

severitySpec :: Spec
severitySpec = describe "Severity" $ do
  it "has derived ordering (constructor order)" $ do
    -- Derived Ord uses constructor declaration order
    Error < Warning `shouldBe` True
    Warning < Suggestion `shouldBe` True
    Suggestion < Info `shouldBe` True

  it "Error is minBound (first constructor)" $ do
    (minBound :: Severity) `shouldBe` Error

  it "Info is maxBound (last constructor)" $ do
    (maxBound :: Severity) `shouldBe` Info

  it "has all constructors in Enum" $ do
    [Error .. Info] `shouldBe` [Error, Warning, Suggestion, Info]

  it "has JSON roundtrip" $ do
    mapM_ shouldRoundTrip [Error, Warning, Suggestion, Info]

  it "has ToJSONKey instance" $ do
    -- Just verify it exists by using it
    let m = Map.singleton Error (42 :: Int)
    shouldRoundTrip m

--------------------------------------------------------------------------------
-- DiagnosticKind
--------------------------------------------------------------------------------

diagnosticKindSpec :: Spec
diagnosticKindSpec = describe "DiagnosticKind" $ do
  it "all standard kinds are distinct" $ do
    let kinds = [ NamingConvention, UnusedCode, UnusedImport, RedundantCode
                , CodePattern, TypeSignature, ImportStyle, TemplateHaskellRef
                , SecurityIssue, PerformanceIssue, ArchitecturalIssue
                , SpaceLeak, PartialFunction, ComplexityIssue
                ]
    length (filter (== NamingConvention) kinds) `shouldBe` 1
    length (filter (== UnusedCode) kinds) `shouldBe` 1

  it "Custom kind holds text" $ do
    let custom = Custom "my-rule"
    case custom of
      Custom t -> t `shouldBe` "my-rule"
      _ -> expectationFailure "Expected Custom"

  it "Custom kinds with different text are different" $ do
    Custom "a" /= Custom "b" `shouldBe` True

  it "has JSON roundtrip for standard kinds" $ do
    mapM_ shouldRoundTrip [ NamingConvention, UnusedCode, SecurityIssue ]

  it "has JSON roundtrip for Custom" $ do
    shouldRoundTrip (Custom "custom-check")

--------------------------------------------------------------------------------
-- ImportSymbolType
--------------------------------------------------------------------------------

importSymbolTypeSpec :: Spec
importSymbolTypeSpec = describe "ImportSymbolType" $ do
  it "has all constructors" $ do
    let all' = [minBound .. maxBound] :: [ImportSymbolType]
    length all' `shouldBe` 6
    ISTFunction `elem` all' `shouldBe` True
    ISTOperator `elem` all' `shouldBe` True
    ISTType `elem` all' `shouldBe` True
    ISTClass `elem` all' `shouldBe` True
    ISTConstructor `elem` all' `shouldBe` True
    ISTPattern `elem` all' `shouldBe` True

  it "has JSON roundtrip" $ do
    mapM_ shouldRoundTrip [ISTFunction, ISTOperator, ISTType, ISTClass, ISTConstructor, ISTPattern]

  it "serializes to expected strings" $ do
    encode ISTFunction `shouldBe` "\"function\""
    encode ISTOperator `shouldBe` "\"operator\""
    encode ISTType `shouldBe` "\"type\""
    encode ISTClass `shouldBe` "\"class\""
    encode ISTConstructor `shouldBe` "\"constructor\""
    encode ISTPattern `shouldBe` "\"pattern\""

--------------------------------------------------------------------------------
-- ImportSymbol
--------------------------------------------------------------------------------

importSymbolSpec :: Spec
importSymbolSpec = describe "ImportSymbol" $ do
  describe "mkImportSymbol" $ do
    it "creates symbol with empty children" $ do
      let sym = mkImportSymbol "foldl'" ISTFunction
      isymName sym `shouldBe` "foldl'"
      isymType sym `shouldBe` ISTFunction
      isymChildren sym `shouldBe` []

    it "creates type symbol" $ do
      let sym = mkImportSymbol "Map" ISTType
      isymType sym `shouldBe` ISTType

  describe "JSON" $ do
    it "has roundtrip" $ do
      shouldRoundTrip (mkImportSymbol "test" ISTFunction)

    it "roundtrips with children" $ do
      let sym = ImportSymbol "Maybe" ISTType ["Just", "Nothing"]
      shouldRoundTrip sym

    it "defaults type to function when missing" $ do
      let json = "{\"name\":\"foo\"}"
          parsed = decode (BL.fromStrict $ TE.encodeUtf8 $ T.pack json) :: Maybe ImportSymbol
      case parsed of
        Just s -> isymType s `shouldBe` ISTFunction
        Nothing -> expectationFailure "Failed to parse"

--------------------------------------------------------------------------------
-- FixImport
--------------------------------------------------------------------------------

fixImportSpec :: Spec
fixImportSpec = describe "FixImport" $ do
  describe "mkFixImport" $ do
    it "creates basic import" $ do
      let imp = mkFixImport "Data.Text" [mkImportSymbol "Text" ISTType]
      fimpModule imp `shouldBe` "Data.Text"
      length (fimpSymbols imp) `shouldBe` 1
      fimpQualified imp `shouldBe` Nothing
      fimpHiding imp `shouldBe` False
      fimpPackage imp `shouldBe` Nothing

  describe "Full FixImport" $ do
    it "supports qualified import" $ do
      let imp = FixImport "Data.Map" [] (Just "M") False Nothing
      fimpQualified imp `shouldBe` Just "M"

    it "supports hiding import" $ do
      let imp = FixImport "Prelude" [mkImportSymbol "head" ISTFunction] Nothing True Nothing
      fimpHiding imp `shouldBe` True

    it "supports package specification" $ do
      let imp = FixImport "Data.ByteString" [] Nothing False (Just "bytestring")
      fimpPackage imp `shouldBe` Just "bytestring"

  describe "JSON" $ do
    it "has roundtrip" $ do
      shouldRoundTrip (mkFixImport "Data.List" [mkImportSymbol "sort" ISTFunction])

    it "roundtrips qualified import" $ do
      shouldRoundTrip (FixImport "Data.Map.Strict" [] (Just "Map") False Nothing)

--------------------------------------------------------------------------------
-- FixCategory
--------------------------------------------------------------------------------

fixCategorySpec :: Spec
fixCategorySpec = describe "FixCategory" $ do
  it "has all standard categories" $ do
    let cats = [FCPerformance, FCModernize, FCSafety, FCStyle, FCImports, FCRedundant, FCSpaceLeaks, FCSecurity]
    length cats `shouldBe` 8

  it "Custom category holds text" $ do
    let custom = FCCustom "my-category"
    case custom of
      FCCustom t -> t `shouldBe` "my-category"
      _ -> expectationFailure "Expected FCCustom"

  it "serializes to expected strings" $ do
    encode FCPerformance `shouldBe` "\"performance\""
    encode FCModernize `shouldBe` "\"modernize\""
    encode FCSafety `shouldBe` "\"safety\""
    encode FCStyle `shouldBe` "\"style\""
    encode FCImports `shouldBe` "\"imports\""
    encode FCRedundant `shouldBe` "\"redundant\""
    encode FCSpaceLeaks `shouldBe` "\"space-leaks\""
    encode FCSecurity `shouldBe` "\"security\""

  it "has JSON roundtrip" $ do
    mapM_ shouldRoundTrip [FCPerformance, FCModernize, FCSafety, FCStyle, FCImports, FCRedundant, FCSpaceLeaks, FCSecurity]

  it "Custom roundtrips to itself" $ do
    -- Custom categories parse back as FCCustom with same text
    shouldRoundTrip (FCCustom "testing")

--------------------------------------------------------------------------------
-- FixSafety
--------------------------------------------------------------------------------

fixSafetySpec :: Spec
fixSafetySpec = describe "FixSafety" $ do
  it "has correct ordering" $ do
    FSAlways < FSMostly `shouldBe` True
    FSMostly < FSReview `shouldBe` True
    FSReview < FSUnsafe `shouldBe` True

  it "FSAlways is minBound" $ do
    (minBound :: FixSafety) `shouldBe` FSAlways

  it "FSUnsafe is maxBound" $ do
    (maxBound :: FixSafety) `shouldBe` FSUnsafe

  it "serializes to expected strings" $ do
    encode FSAlways `shouldBe` "\"safe\""
    encode FSMostly `shouldBe` "\"mostly-safe\""
    encode FSReview `shouldBe` "\"review\""
    encode FSUnsafe `shouldBe` "\"unsafe\""

  it "has JSON roundtrip" $ do
    mapM_ shouldRoundTrip [FSAlways, FSMostly, FSReview, FSUnsafe]

  it "defaults to FSAlways for unknown values" $ do
    let json = "\"unknown-safety\""
        parsed = decode (BL.fromStrict $ TE.encodeUtf8 $ T.pack json) :: Maybe FixSafety
    parsed `shouldBe` Just FSAlways

--------------------------------------------------------------------------------
-- Fix
--------------------------------------------------------------------------------

fixSpec :: Spec
fixSpec = describe "Fix" $ do
  describe "mkFixSafe" $ do
    it "returns Nothing for empty edits" $ do
      mkFixSafe "title" [] True `shouldBe` Nothing

    it "returns Just for non-empty edits" $ do
      let edit = FixEdit noSrcSpan "replacement"
      mkFixSafe "title" [edit] True `shouldSatisfy` isJust

    it "sets default values" $ do
      let edit = FixEdit noSrcSpan "replacement"
          Just fix' = mkFixSafe "title" [edit] True
      fixCategory fix' `shouldBe` FCStyle
      fixSafety fix' `shouldBe` FSAlways
      fixAddImports fix' `shouldBe` []
      fixRemoveImports fix' `shouldBe` []

  describe "mkFix" $ do
    it "creates fix with defaults" $ do
      let edit = FixEdit noSrcSpan "new"
          fix' = mkFix "title" [edit] False
      fixTitle fix' `shouldBe` "title"
      fixIsPreferred fix' `shouldBe` False
      fixCategory fix' `shouldBe` FCStyle

    it "allows empty edits (backward compat)" $ do
      -- mkFix doesn't validate, unlike mkFixSafe
      let fix' = mkFix "title" [] True
      fixEdits fix' `shouldBe` []

  describe "mkFixWithImports" $ do
    it "creates fix with all fields" $ do
      let edit = FixEdit noSrcSpan "new code"
          imp = mkFixImport "Data.List" []
          fix' = mkFixWithImports "Add import" [edit] True [imp] ["oldFunc"] FCModernize FSMostly
      fixTitle fix' `shouldBe` "Add import"
      fixIsPreferred fix' `shouldBe` True
      length (fixAddImports fix') `shouldBe` 1
      fixRemoveImports fix' `shouldBe` ["oldFunc"]
      fixCategory fix' `shouldBe` FCModernize
      fixSafety fix' `shouldBe` FSMostly

  describe "JSON" $ do
    it "has roundtrip for simple fix" $ do
      let edit = FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 5) "newText"
          fix' = mkFix "Replace" [edit] True
      shouldRoundTrip fix'

    it "has roundtrip for complex fix" $ do
      let edit = FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 5) "newText"
          imp = mkFixImport "Data.Text" [mkImportSymbol "Text" ISTType]
          fix' = mkFixWithImports "title" [edit] True [imp] ["old"] FCPerformance FSReview
      shouldRoundTrip fix'

--------------------------------------------------------------------------------
-- Diagnostic
--------------------------------------------------------------------------------

diagnosticSpec :: Spec
diagnosticSpec = describe "Diagnostic" $ do
  it "can be created with all fields" $ do
    let diag = mkTestDiagnostic
    diagMessage diag `shouldBe` "Test message"
    diagSeverity diag `shouldBe` Warning
    diagKind diag `shouldBe` CodePattern
    diagCode diag `shouldBe` Just "TEST001"

  it "supports related locations" $ do
    let related = [(mkSrcSpanRaw "other.hs" 5 1 5 10, "See also")]
        diag = Diagnostic noSrcSpan Warning CodePattern "msg" Nothing [] related
    length (diagRelated diag) `shouldBe` 1

  it "has JSON roundtrip" $ do
    shouldRoundTrip mkTestDiagnostic

  it "roundtrips with fixes" $ do
    let edit = FixEdit noSrcSpan "fixed"
        fix' = mkFix "Fix it" [edit] True
        diag = Diagnostic noSrcSpan Warning CodePattern "msg" Nothing [fix'] []
    shouldRoundTrip diag

--------------------------------------------------------------------------------
-- SymbolKind
--------------------------------------------------------------------------------

symbolKindSpec :: Spec
symbolKindSpec = describe "SymbolKind" $ do
  it "has all constructors" $ do
    let all' = [minBound .. maxBound] :: [SymbolKind]
    length all' `shouldBe` 8
    Function `elem` all' `shouldBe` True
    TypeConstructor `elem` all' `shouldBe` True
    DataConstructor `elem` all' `shouldBe` True
    TypeClass `elem` all' `shouldBe` True
    TypeClassMethod `elem` all' `shouldBe` True
    TypeFamily `elem` all' `shouldBe` True
    PatternSynonym `elem` all' `shouldBe` True
    Module `elem` all' `shouldBe` True

  it "has JSON roundtrip" $ do
    mapM_ shouldRoundTrip [Function, TypeConstructor, DataConstructor, TypeClass]

--------------------------------------------------------------------------------
-- QualifiedName
--------------------------------------------------------------------------------

qualifiedNameSpec :: Spec
qualifiedNameSpec = describe "QualifiedName" $ do
  describe "mkQualifiedName" $ do
    it "creates qualified name" $ do
      let qn = mkQualifiedName (Just "Data.Map") "lookup"
      qnModule qn `shouldBe` Just "Data.Map"
      qnName qn `shouldBe` "lookup"

    it "creates unqualified name" $ do
      let qn = mkQualifiedName Nothing "localFunc"
      qnModule qn `shouldBe` Nothing
      qnName qn `shouldBe` "localFunc"

  it "has JSON roundtrip" $ do
    shouldRoundTrip (mkQualifiedName (Just "Mod") "name")
    shouldRoundTrip (mkQualifiedName Nothing "local")

  it "supports ordering" $ do
    let qn1 = mkQualifiedName (Just "A") "a"
        qn2 = mkQualifiedName (Just "B") "b"
    qn1 < qn2 `shouldBe` True

--------------------------------------------------------------------------------
-- Symbol
--------------------------------------------------------------------------------

symbolSpec :: Spec
symbolSpec = describe "Symbol" $ do
  it "can be created with all fields" $ do
    let sym = mkTestSymbol
    symbolKind sym `shouldBe` Function
    symbolExported sym `shouldBe` True

  it "has JSON roundtrip" $ do
    shouldRoundTrip mkTestSymbol

  it "supports ordering based on name" $ do
    let sym1 = Symbol (mkQualifiedName Nothing "aaa") Function noSrcSpan True Nothing
        sym2 = Symbol (mkQualifiedName Nothing "bbb") Function noSrcSpan True Nothing
    sym1 < sym2 `shouldBe` True

--------------------------------------------------------------------------------
-- FileResult
--------------------------------------------------------------------------------

fileResultSpec :: Spec
fileResultSpec = describe "FileResult" $ do
  it "can be created with all fields" $ do
    let fr = FileResult "test.hs" [mkTestDiagnostic] [mkTestSymbol] [] []
    fileResultPath fr `shouldBe` "test.hs"
    length (fileResultDiagnostics fr) `shouldBe` 1
    length (fileResultSymbols fr) `shouldBe` 1

  it "has JSON roundtrip" $ do
    let fr = FileResult "test.hs" [] [] [] []
    shouldRoundTrip fr

  it "roundtrips with diagnostics and symbols" $ do
    let fr = FileResult "test.hs" [mkTestDiagnostic] [mkTestSymbol]
               [mkQualifiedName (Just "Data.Text") "Text"]
               [mkQualifiedName Nothing "myExport"]
    shouldRoundTrip fr

--------------------------------------------------------------------------------
-- AnalysisResult
--------------------------------------------------------------------------------

analysisResultSpec :: Spec
analysisResultSpec = describe "AnalysisResult" $ do
  describe "emptyAnalysisResult" $ do
    it "has empty files" $ do
      resultFiles emptyAnalysisResult `shouldBe` Map.empty

    it "has empty unused code" $ do
      resultUnusedCode emptyAnalysisResult `shouldBe` Set.empty

    it "has empty diag count" $ do
      resultDiagCount emptyAnalysisResult `shouldBe` Map.empty

  describe "mergeResults" $ do
    it "merges file maps" $ do
      let fr1 = FileResult "a.hs" [] [] [] []
          fr2 = FileResult "b.hs" [] [] [] []
          r1 = emptyAnalysisResult { resultFiles = Map.singleton "a.hs" fr1 }
          r2 = emptyAnalysisResult { resultFiles = Map.singleton "b.hs" fr2 }
          merged = mergeResults r1 r2
      Map.size (resultFiles merged) `shouldBe` 2

    it "merges unused code sets" $ do
      let qn1 = mkQualifiedName Nothing "unused1"
          qn2 = mkQualifiedName Nothing "unused2"
          r1 = emptyAnalysisResult { resultUnusedCode = Set.singleton qn1 }
          r2 = emptyAnalysisResult { resultUnusedCode = Set.singleton qn2 }
          merged = mergeResults r1 r2
      Set.size (resultUnusedCode merged) `shouldBe` 2

    it "sums diag counts" $ do
      let r1 = emptyAnalysisResult { resultDiagCount = Map.singleton Warning 5 }
          r2 = emptyAnalysisResult { resultDiagCount = Map.singleton Warning 3 }
          merged = mergeResults r1 r2
      Map.lookup Warning (resultDiagCount merged) `shouldBe` Just 8

    it "merges different severity counts" $ do
      let r1 = emptyAnalysisResult { resultDiagCount = Map.singleton Warning 5 }
          r2 = emptyAnalysisResult { resultDiagCount = Map.singleton Error 2 }
          merged = mergeResults r1 r2
      Map.lookup Warning (resultDiagCount merged) `shouldBe` Just 5
      Map.lookup Error (resultDiagCount merged) `shouldBe` Just 2

  describe "JSON" $ do
    it "has roundtrip for empty result" $ do
      shouldRoundTrip emptyAnalysisResult

    it "has roundtrip for populated result" $ do
      let fr = FileResult "test.hs" [mkTestDiagnostic] [] [] []
          result = AnalysisResult
            { resultFiles = Map.singleton "test.hs" fr
            , resultUnusedCode = Set.singleton (mkQualifiedName Nothing "unused")
            , resultDiagCount = Map.fromList [(Warning, 1), (Error, 0)]
            }
      shouldRoundTrip result

--------------------------------------------------------------------------------
-- AnalysisMode
--------------------------------------------------------------------------------

analysisModeSpec :: Spec
analysisModeSpec = describe "AnalysisMode" $ do
  it "has all constructors" $ do
    let all' = [minBound .. maxBound] :: [AnalysisMode]
    length all' `shouldBe` 3
    QuickMode `elem` all' `shouldBe` True
    FullMode `elem` all' `shouldBe` True
    PluginMode `elem` all' `shouldBe` True

  it "has correct ordering" $ do
    QuickMode < FullMode `shouldBe` True
    FullMode < PluginMode `shouldBe` True

  it "has JSON roundtrip" $ do
    mapM_ shouldRoundTrip [QuickMode, FullMode, PluginMode]

--------------------------------------------------------------------------------
-- ArgusOptions
--------------------------------------------------------------------------------

argusOptionsSpec :: Spec
argusOptionsSpec = describe "ArgusOptions" $ do
  describe "defaultOptions" $ do
    it "has QuickMode" $ do
      optMode defaultOptions `shouldBe` QuickMode

    it "has no config file" $ do
      optConfigFile defaultOptions `shouldBe` Nothing

    it "has current dir as target" $ do
      optTargetPaths defaultOptions `shouldBe` ["."]

    it "has terminal format" $ do
      optOutputFormat defaultOptions `shouldBe` "terminal"

    it "has fix options disabled" $ do
      optApplyFixes defaultOptions `shouldBe` False
      optInteractive defaultOptions `shouldBe` False
      optPreview defaultOptions `shouldBe` False

    it "has reasonable parallel setting" $ do
      optParallel defaultOptions `shouldBe` 4

  describe "JSON" $ do
    it "has roundtrip for default options" $ do
      shouldRoundTrip defaultOptions

    it "has roundtrip for custom options" $ do
      let opts = ArgusOptions
            { optMode = FullMode
            , optConfigFile = Just "argus.toml"
            , optTargetPaths = ["src/", "app/"]
            , optHieDir = Just ".hie"
            , optOutputFormat = "json"
            , optApplyFixes = True
            , optInteractive = True
            , optPreview = True
            , optVerbosity = Verbose
            , optNoColor = True
            , optParallel = 8
            }
      shouldRoundTrip opts

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Test JSON roundtrip
shouldRoundTrip :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Expectation
shouldRoundTrip x = decode (encode x) `shouldBe` Just x

-- | Create a test diagnostic
mkTestDiagnostic :: Diagnostic
mkTestDiagnostic = Diagnostic
  { diagSpan = mkSrcSpanRaw "test.hs" 10 1 10 20
  , diagSeverity = Warning
  , diagKind = CodePattern
  , diagMessage = "Test message"
  , diagCode = Just "TEST001"
  , diagFixes = []
  , diagRelated = []
  }

-- | Create a test symbol
mkTestSymbol :: Symbol
mkTestSymbol = Symbol
  { symbolName = mkQualifiedName (Just "TestModule") "testFunc"
  , symbolKind = Function
  , symbolSpan = mkSrcSpanRaw "test.hs" 5 1 5 15
  , symbolExported = True
  , symbolType = Just "Int -> String"
  }
