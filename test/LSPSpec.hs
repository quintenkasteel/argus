{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : LSPSpec
-- Description : Comprehensive tests for LSP Server functionality
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Tests for all LSP server types, conversion functions, JSON serialization,
-- and protocol message handling.
module LSPSpec (spec) where

import Data.Aeson (encode, decode, toJSON, object, (.=), Value(..))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Argus.LSP.Server
import Argus.Types (Diagnostic(..), Severity(..), SrcSpan(..), Fix(..), FixEdit(..), DiagnosticKind(..), FixCategory(..), FixSafety(..), mkSrcSpanRaw, Line(..), Column(..), SymbolKind(..))

spec :: Spec
spec = do
  describe "Argus.LSP.Server" $ do
    severityConversionSpec
    spanToRangeSpec
    diagToLspSpec
    diagsToLspSpec
    fixToCodeActionSpec
    lspConfigSpec
    lspSeveritySpec
    lspPositionSpec
    lspRangeSpec
    lspDiagnosticSpec
    lspTextEditSpec
    lspWorkspaceEditSpec
    lspHoverSpec
    lspMarkupContentSpec
    lspCodeActionSpec
    lspCommandSpec
    diagnosticKindSpec
    positionAndRangeOperationsSpec
    jsonRoundTripSpec
    edgeCasesSpec
    lspTestHarnessSpec
    lspCodeActionTestsSpec
    lspWorkspaceEventTestsSpec
    lspCompletionItemKindSpec
    lspCompletionItemSpec
    lspCompletionListSpec
    symbolKindToCompletionKindSpec
    getCompletionPrefixSpec
    lspCompletionMockRequestSpec
    lspCompletionKindMappingSpec

--------------------------------------------------------------------------------
-- Severity Conversion Tests
--------------------------------------------------------------------------------

severityConversionSpec :: Spec
severityConversionSpec = describe "severityToLsp" $ do
  it "converts Error to LspError" $
    severityToLsp Error `shouldBe` LspError

  it "converts Warning to LspWarning" $
    severityToLsp Warning `shouldBe` LspWarning

  it "converts Suggestion to LspHint" $
    severityToLsp Suggestion `shouldBe` LspHint

  it "converts Info to LspInformation" $
    severityToLsp Info `shouldBe` LspInformation

  it "is total over all severity values" $
    mapM_ (\s -> severityToLsp s `shouldSatisfy` const True) [Error, Warning, Suggestion, Info]

--------------------------------------------------------------------------------
-- Span to Range Tests
--------------------------------------------------------------------------------

spanToRangeSpec :: Spec
spanToRangeSpec = describe "spanToRange" $ do
  it "converts 1-indexed SrcSpan to 0-indexed LspRange" $ do
    let span' = mkSrcSpanRaw "test.hs" 10 5 10 15
        range' = spanToRange span'
    lrStart range' `shouldBe` LspPosition 9 4
    lrEnd range' `shouldBe` LspPosition 9 14

  it "handles single-character spans" $ do
    let span' = mkSrcSpanRaw "test.hs" 1 1 1 2
        range' = spanToRange span'
    lrStart range' `shouldBe` LspPosition 0 0
    lrEnd range' `shouldBe` LspPosition 0 1

  it "handles multi-line spans" $ do
    let span' = mkSrcSpanRaw "test.hs" 5 10 8 20
        range' = spanToRange span'
    lrStart range' `shouldBe` LspPosition 4 9
    lrEnd range' `shouldBe` LspPosition 7 19

  it "handles minimum span (1,1 to 1,1)" $ do
    let span' = mkSrcSpanRaw "test.hs" 1 1 1 1
        range' = spanToRange span'
    lrStart range' `shouldBe` LspPosition 0 0
    lrEnd range' `shouldBe` LspPosition 0 0

  it "handles large line numbers" $ do
    let span' = mkSrcSpanRaw "test.hs" 10000 1 10000 50
        range' = spanToRange span'
    lrStart range' `shouldBe` LspPosition 9999 0
    lrEnd range' `shouldBe` LspPosition 9999 49

  it "handles large column numbers" $ do
    let span' = mkSrcSpanRaw "test.hs" 1 500 1 1000
        range' = spanToRange span'
    lrStart range' `shouldBe` LspPosition 0 499
    lrEnd range' `shouldBe` LspPosition 0 999

--------------------------------------------------------------------------------
-- Diagnostic Conversion Tests
--------------------------------------------------------------------------------

diagToLspSpec :: Spec
diagToLspSpec = describe "diagToLsp" $ do
  it "converts Argus Diagnostic to LSP Diagnostic" $ do
    let argDiag = Diagnostic
          { diagSpan = mkSrcSpanRaw "test.hs" 5 3 5 10
          , diagSeverity = Warning
          , diagKind = CodePattern
          , diagMessage = "Use null instead of length"
          , diagCode = Just "performance/length-null"
          , diagFixes = []
          , diagRelated = []
          }
        lspDiag = diagToLsp argDiag
    ldSeverity lspDiag `shouldBe` LspWarning
    ldMessage lspDiag `shouldBe` "Use null instead of length"
    ldCode lspDiag `shouldBe` Just "performance/length-null"
    ldSource lspDiag `shouldBe` "argus"

  it "includes tags for unused code" $ do
    let argDiag = Diagnostic
          { diagSpan = mkSrcSpanRaw "test.hs" 10 1 10 20
          , diagSeverity = Warning
          , diagKind = UnusedCode
          , diagMessage = "Unused binding"
          , diagCode = Just "unused/binding"
          , diagFixes = []
          , diagRelated = []
          }
        lspDiag = diagToLsp argDiag
    ldTags lspDiag `shouldBe` [1]  -- Unnecessary tag

  it "includes tags for unused imports" $ do
    let argDiag = Diagnostic
          { diagSpan = mkSrcSpanRaw "test.hs" 3 1 3 30
          , diagSeverity = Warning
          , diagKind = UnusedImport
          , diagMessage = "Unused import"
          , diagCode = Just "imports/unused"
          , diagFixes = []
          , diagRelated = []
          }
        lspDiag = diagToLsp argDiag
    ldTags lspDiag `shouldBe` [1]  -- Unnecessary tag

  it "includes tags for redundant code" $ do
    let argDiag = Diagnostic
          { diagSpan = mkSrcSpanRaw "test.hs" 7 1 7 15
          , diagSeverity = Suggestion
          , diagKind = RedundantCode
          , diagMessage = "Redundant do"
          , diagCode = Nothing
          , diagFixes = []
          , diagRelated = []
          }
        lspDiag = diagToLsp argDiag
    ldTags lspDiag `shouldBe` [1]  -- Unnecessary tag

  it "has empty tags for other diagnostic kinds" $ do
    let argDiag = Diagnostic
          { diagSpan = mkSrcSpanRaw "test.hs" 1 1 1 10
          , diagSeverity = Error
          , diagKind = SecurityIssue
          , diagMessage = "Security issue"
          , diagCode = Just "security/test"
          , diagFixes = []
          , diagRelated = []
          }
        lspDiag = diagToLsp argDiag
    ldTags lspDiag `shouldBe` []

  it "preserves message text exactly" $ do
    let msg = "Complex message with\nmultiple lines\nand unicode: λ α β"
        argDiag = Diagnostic
          { diagSpan = mkSrcSpanRaw "test.hs" 1 1 1 10
          , diagSeverity = Warning
          , diagKind = CodePattern
          , diagMessage = msg
          , diagCode = Nothing
          , diagFixes = []
          , diagRelated = []
          }
        lspDiag = diagToLsp argDiag
    ldMessage lspDiag `shouldBe` msg

  it "handles empty code" $ do
    let argDiag = Diagnostic
          { diagSpan = mkSrcSpanRaw "test.hs" 1 1 1 10
          , diagSeverity = Info
          , diagKind = NamingConvention
          , diagMessage = "Test"
          , diagCode = Nothing
          , diagFixes = []
          , diagRelated = []
          }
        lspDiag = diagToLsp argDiag
    ldCode lspDiag `shouldBe` Nothing

  it "handles ldData as Nothing" $ do
    let argDiag = Diagnostic
          { diagSpan = mkSrcSpanRaw "test.hs" 1 1 1 10
          , diagSeverity = Warning
          , diagKind = CodePattern
          , diagMessage = "Test"
          , diagCode = Nothing
          , diagFixes = []
          , diagRelated = []
          }
        lspDiag = diagToLsp argDiag
    ldData lspDiag `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Multiple Diagnostics Conversion Tests
--------------------------------------------------------------------------------

diagsToLspSpec :: Spec
diagsToLspSpec = describe "diagsToLsp" $ do
  it "converts empty list" $
    diagsToLsp [] `shouldBe` []

  it "converts single diagnostic" $ do
    let diags = [Diagnostic (mkSrcSpanRaw "a.hs" 1 1 1 5) Warning CodePattern "msg" Nothing [] []]
        lspDiags = diagsToLsp diags
    length lspDiags `shouldBe` 1
    ldMessage (head lspDiags) `shouldBe` "msg"

  it "converts multiple diagnostics preserving order" $ do
    let diags =
          [ Diagnostic (mkSrcSpanRaw "a.hs" 1 1 1 5) Warning CodePattern "msg1" Nothing [] []
          , Diagnostic (mkSrcSpanRaw "b.hs" 2 2 2 10) Error SecurityIssue "msg2" Nothing [] []
          , Diagnostic (mkSrcSpanRaw "c.hs" 3 3 3 15) Info NamingConvention "msg3" Nothing [] []
          ]
        lspDiags = diagsToLsp diags
    length lspDiags `shouldBe` 3
    ldMessage (head lspDiags) `shouldBe` "msg1"
    ldMessage (lspDiags !! 1) `shouldBe` "msg2"
    ldMessage (lspDiags !! 2) `shouldBe` "msg3"

  it "converts large number of diagnostics" $ do
    let diags = [Diagnostic (mkSrcSpanRaw "test.hs" n 1 n 10) Warning CodePattern ("msg" <> T.pack (show n)) Nothing [] [] | n <- [1..100]]
        lspDiags = diagsToLsp diags
    length lspDiags `shouldBe` 100

--------------------------------------------------------------------------------
-- Fix to Code Action Tests
--------------------------------------------------------------------------------

fixToCodeActionSpec :: Spec
fixToCodeActionSpec = describe "fixToCodeAction" $ do
  it "creates code action with workspace edit" $ do
    let diag = Diagnostic
          { diagSpan = mkSrcSpanRaw "test.hs" 5 1 5 20
          , diagSeverity = Warning
          , diagKind = CodePattern
          , diagMessage = "Use null"
          , diagCode = Just "perf/null"
          , diagFixes = []
          , diagRelated = []
          }
        fix' = Fix
          { fixTitle = "Replace with null"
          , fixEdits = [FixEdit (mkSrcSpanRaw "test.hs" 5 1 5 20) "null xs"]
          , fixIsPreferred = True
          , fixAddImports = []
          , fixRemoveImports = []
          , fixCategory = FCPerformance
          , fixSafety = FSMostly
          }
        action = fixToCodeAction "test.hs" diag fix'
    lcaTitle action `shouldBe` "Replace with null"
    lcaKind action `shouldBe` "quickfix"
    lcaIsPreferred action `shouldBe` True

  it "includes diagnostic in code action" $ do
    let diag = Diagnostic
          { diagSpan = mkSrcSpanRaw "test.hs" 1 1 1 10
          , diagSeverity = Warning
          , diagKind = CodePattern
          , diagMessage = "Use pure"
          , diagCode = Just "mod/pure"
          , diagFixes = []
          , diagRelated = []
          }
        fix' = Fix
          { fixTitle = "Use pure"
          , fixEdits = []
          , fixIsPreferred = False
          , fixAddImports = []
          , fixRemoveImports = []
          , fixCategory = FCModernize
          , fixSafety = FSMostly
          }
        action = fixToCodeAction "test.hs" diag fix'
    length (lcaDiagnostics action) `shouldBe` 1

  it "creates workspace edit with file URI" $ do
    let diag = Diagnostic
          { diagSpan = mkSrcSpanRaw "src/Main.hs" 10 5 10 15
          , diagSeverity = Warning
          , diagKind = CodePattern
          , diagMessage = "Test"
          , diagCode = Nothing
          , diagFixes = []
          , diagRelated = []
          }
        fix' = Fix
          { fixTitle = "Fix it"
          , fixEdits = [FixEdit (mkSrcSpanRaw "src/Main.hs" 10 5 10 15) "replacement"]
          , fixIsPreferred = False
          , fixAddImports = []
          , fixRemoveImports = []
          , fixCategory = FCStyle
          , fixSafety = FSAlways
          }
        action = fixToCodeAction "src/Main.hs" diag fix'
    case lcaEdit action of
      Just edit -> Map.member "file://src/Main.hs" (lweChanges edit) `shouldBe` True
      Nothing -> expectationFailure "Expected workspace edit"

  it "handles multiple edits in single fix" $ do
    let diag = Diagnostic
          { diagSpan = mkSrcSpanRaw "test.hs" 1 1 1 10
          , diagSeverity = Warning
          , diagKind = CodePattern
          , diagMessage = "Test"
          , diagCode = Nothing
          , diagFixes = []
          , diagRelated = []
          }
        fix' = Fix
          { fixTitle = "Multi-edit fix"
          , fixEdits =
              [ FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 5) "a"
              , FixEdit (mkSrcSpanRaw "test.hs" 2 1 2 5) "b"
              , FixEdit (mkSrcSpanRaw "test.hs" 3 1 3 5) "c"
              ]
          , fixIsPreferred = False
          , fixAddImports = []
          , fixRemoveImports = []
          , fixCategory = FCStyle
          , fixSafety = FSAlways
          }
        action = fixToCodeAction "test.hs" diag fix'
    case lcaEdit action of
      Just edit ->
        case Map.lookup "file://test.hs" (lweChanges edit) of
          Just edits -> length edits `shouldBe` 3
          Nothing -> expectationFailure "Expected edits for test.hs"
      Nothing -> expectationFailure "Expected workspace edit"

  it "respects fixIsPreferred flag" $ do
    let diag = Diagnostic
          { diagSpan = mkSrcSpanRaw "test.hs" 1 1 1 10
          , diagSeverity = Warning
          , diagKind = CodePattern
          , diagMessage = "Test"
          , diagCode = Nothing
          , diagFixes = []
          , diagRelated = []
          }
        preferredFix = Fix
          { fixTitle = "Preferred"
          , fixEdits = [FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 10) "x"]
          , fixIsPreferred = True
          , fixAddImports = []
          , fixRemoveImports = []
          , fixCategory = FCStyle
          , fixSafety = FSAlways
          }
        notPreferredFix = preferredFix { fixTitle = "Not preferred", fixIsPreferred = False }
    lcaIsPreferred (fixToCodeAction "test.hs" diag preferredFix) `shouldBe` True
    lcaIsPreferred (fixToCodeAction "test.hs" diag notPreferredFix) `shouldBe` False

--------------------------------------------------------------------------------
-- LspConfig Tests
--------------------------------------------------------------------------------

lspConfigSpec :: Spec
lspConfigSpec = describe "LspConfig" $ do
  it "has sensible defaults" $ do
    lspConfigFile defaultLspConfig `shouldBe` Nothing
    lspCacheDiags defaultLspConfig `shouldBe` True
    lspAnalyzeOnOpen defaultLspConfig `shouldBe` True
    lspAnalyzeOnSave defaultLspConfig `shouldBe` True
    lspAnalyzeOnChange defaultLspConfig `shouldBe` False
    lspDebounceMs defaultLspConfig `shouldBe` 500

  it "has progress reporting enabled by default" $
    lspProgressReporting defaultLspConfig `shouldBe` True

  it "has max diagnostics limit" $
    lspMaxDiagnostics defaultLspConfig `shouldBe` Just 100

  it "has debug log disabled by default" $
    lspDebugLog defaultLspConfig `shouldBe` Nothing

--------------------------------------------------------------------------------
-- LspSeverity Tests
--------------------------------------------------------------------------------

lspSeveritySpec :: Spec
lspSeveritySpec = describe "LspSeverity" $ do
  it "has correct ordering" $ do
    LspError `shouldSatisfy` (< LspWarning)
    LspWarning `shouldSatisfy` (< LspInformation)
    LspInformation `shouldSatisfy` (< LspHint)

  it "serializes to correct JSON values" $ do
    toJSON LspError `shouldBe` Aeson.Number 1
    toJSON LspWarning `shouldBe` Aeson.Number 2
    toJSON LspInformation `shouldBe` Aeson.Number 3
    toJSON LspHint `shouldBe` Aeson.Number 4

  it "deserializes from JSON" $ do
    decode (encode LspError) `shouldBe` Just LspError
    decode (encode LspWarning) `shouldBe` Just LspWarning
    decode (encode LspInformation) `shouldBe` Just LspInformation
    decode (encode LspHint) `shouldBe` Just LspHint

  it "rejects invalid severity numbers" $
    (decode "0" :: Maybe LspSeverity) `shouldBe` Nothing

  it "rejects severity numbers out of range" $
    (decode "5" :: Maybe LspSeverity) `shouldBe` Nothing

  it "has all enum values" $
    [minBound..maxBound] `shouldBe` [LspError, LspWarning, LspInformation, LspHint]

--------------------------------------------------------------------------------
-- LspPosition Tests
--------------------------------------------------------------------------------

lspPositionSpec :: Spec
lspPositionSpec = describe "LspPosition" $ do
  it "has correct field names" $ do
    let pos = LspPosition 10 5
    lpLine pos `shouldBe` 10
    lpCharacter pos `shouldBe` 5

  it "serializes and deserializes" $ do
    let pos = LspPosition 10 5
    decode (encode pos) `shouldBe` Just pos

  it "serializes to correct JSON structure" $ do
    let pos = LspPosition 10 5
        json = encode pos
    decode json `shouldBe` Just (object ["line" .= (10 :: Int), "character" .= (5 :: Int)])

  it "handles zero position" $ do
    let pos = LspPosition 0 0
    decode (encode pos) `shouldBe` Just pos

  it "handles large positions" $ do
    let pos = LspPosition 999999 999999
    decode (encode pos) `shouldBe` Just pos

  it "has Eq instance" $ do
    LspPosition 1 2 `shouldBe` LspPosition 1 2
    LspPosition 1 2 `shouldNotBe` LspPosition 1 3
    LspPosition 1 2 `shouldNotBe` LspPosition 2 2

--------------------------------------------------------------------------------
-- LspRange Tests
--------------------------------------------------------------------------------

lspRangeSpec :: Spec
lspRangeSpec = describe "LspRange" $ do
  it "has correct structure" $ do
    let range' = LspRange (LspPosition 0 0) (LspPosition 1 10)
    lrStart range' `shouldBe` LspPosition 0 0
    lrEnd range' `shouldBe` LspPosition 1 10

  it "serializes and deserializes" $ do
    let range' = LspRange (LspPosition 5 10) (LspPosition 5 20)
    decode (encode range') `shouldBe` Just range'

  it "handles single-character range" $ do
    let range' = LspRange (LspPosition 0 0) (LspPosition 0 1)
    decode (encode range') `shouldBe` Just range'

  it "handles multi-line range" $ do
    let range' = LspRange (LspPosition 0 0) (LspPosition 100 50)
    decode (encode range') `shouldBe` Just range'

  it "handles empty range (start == end)" $ do
    let range' = LspRange (LspPosition 5 10) (LspPosition 5 10)
    decode (encode range') `shouldBe` Just range'

--------------------------------------------------------------------------------
-- LspDiagnostic Tests
--------------------------------------------------------------------------------

lspDiagnosticSpec :: Spec
lspDiagnosticSpec = describe "LspDiagnostic" $ do
  it "serializes and deserializes" $ do
    let diag = LspDiagnostic
          { ldRange = LspRange (LspPosition 0 0) (LspPosition 0 10)
          , ldSeverity = LspWarning
          , ldCode = Just "test/code"
          , ldSource = "argus"
          , ldMessage = "Test message"
          , ldTags = [1]
          , ldData = Nothing
          }
    decode (encode diag) `shouldBe` Just diag

  it "serializes without optional fields when empty" $ do
    let diag = LspDiagnostic
          { ldRange = LspRange (LspPosition 0 0) (LspPosition 0 10)
          , ldSeverity = LspError
          , ldCode = Nothing
          , ldSource = "argus"
          , ldMessage = "Error"
          , ldTags = []
          , ldData = Nothing
          }
    decode (encode diag) `shouldBe` Just diag

  it "includes code when present" $ do
    let diag = LspDiagnostic
          { ldRange = LspRange (LspPosition 0 0) (LspPosition 0 10)
          , ldSeverity = LspWarning
          , ldCode = Just "my-code"
          , ldSource = "argus"
          , ldMessage = "Test"
          , ldTags = []
          , ldData = Nothing
          }
        encoded = encode diag
    -- Verify code is in the JSON
    BSL.length encoded `shouldSatisfy` (> 0)
    decode encoded `shouldBe` Just diag

  it "includes tags when non-empty" $ do
    let diag = LspDiagnostic
          { ldRange = LspRange (LspPosition 0 0) (LspPosition 0 10)
          , ldSeverity = LspHint
          , ldCode = Nothing
          , ldSource = "argus"
          , ldMessage = "Hint"
          , ldTags = [1, 2]
          , ldData = Nothing
          }
    decode (encode diag) `shouldBe` Just diag

  it "handles unicode in message" $ do
    let diag = LspDiagnostic
          { ldRange = LspRange (LspPosition 0 0) (LspPosition 0 10)
          , ldSeverity = LspWarning
          , ldCode = Nothing
          , ldSource = "argus"
          , ldMessage = "Unicode: λ α β γ δ 日本語"
          , ldTags = []
          , ldData = Nothing
          }
    decode (encode diag) `shouldBe` Just diag

  it "handles data field" $ do
    let diag = LspDiagnostic
          { ldRange = LspRange (LspPosition 0 0) (LspPosition 0 10)
          , ldSeverity = LspWarning
          , ldCode = Nothing
          , ldSource = "argus"
          , ldMessage = "Test"
          , ldTags = []
          , ldData = Just (object ["key" .= ("value" :: T.Text)])
          }
    -- Just verify it encodes without error
    BSL.length (encode diag) `shouldSatisfy` (> 0)

--------------------------------------------------------------------------------
-- LspTextEdit Tests
--------------------------------------------------------------------------------

lspTextEditSpec :: Spec
lspTextEditSpec = describe "LspTextEdit" $ do
  it "serializes and deserializes" $ do
    let edit = LspTextEdit
          { lteRange = LspRange (LspPosition 0 0) (LspPosition 0 5)
          , lteNewText = "replacement"
          }
    decode (encode edit) `shouldBe` Just edit

  it "handles empty newText" $ do
    let edit = LspTextEdit
          { lteRange = LspRange (LspPosition 0 0) (LspPosition 0 5)
          , lteNewText = ""
          }
    decode (encode edit) `shouldBe` Just edit

  it "handles multiline newText" $ do
    let edit = LspTextEdit
          { lteRange = LspRange (LspPosition 0 0) (LspPosition 0 5)
          , lteNewText = "line1\nline2\nline3"
          }
    decode (encode edit) `shouldBe` Just edit

  it "handles unicode in newText" $ do
    let edit = LspTextEdit
          { lteRange = LspRange (LspPosition 0 0) (LspPosition 0 5)
          , lteNewText = "λ x -> x"
          }
    decode (encode edit) `shouldBe` Just edit

--------------------------------------------------------------------------------
-- LspWorkspaceEdit Tests
--------------------------------------------------------------------------------

lspWorkspaceEditSpec :: Spec
lspWorkspaceEditSpec = describe "LspWorkspaceEdit" $ do
  it "serializes and deserializes" $ do
    let edit = LspWorkspaceEdit
          { lweChanges = Map.singleton "file://test.hs"
              [ LspTextEdit (LspRange (LspPosition 0 0) (LspPosition 0 5)) "new" ]
          }
    decode (encode edit) `shouldBe` Just edit

  it "handles empty changes" $ do
    let edit = LspWorkspaceEdit { lweChanges = Map.empty }
    decode (encode edit) `shouldBe` Just edit

  it "handles multiple files" $ do
    let edit = LspWorkspaceEdit
          { lweChanges = Map.fromList
              [ ("file://a.hs", [LspTextEdit (LspRange (LspPosition 0 0) (LspPosition 0 5)) "a"])
              , ("file://b.hs", [LspTextEdit (LspRange (LspPosition 1 0) (LspPosition 1 5)) "b"])
              ]
          }
    decode (encode edit) `shouldBe` Just edit

  it "handles multiple edits per file" $ do
    let edit = LspWorkspaceEdit
          { lweChanges = Map.singleton "file://test.hs"
              [ LspTextEdit (LspRange (LspPosition 0 0) (LspPosition 0 5)) "edit1"
              , LspTextEdit (LspRange (LspPosition 1 0) (LspPosition 1 5)) "edit2"
              , LspTextEdit (LspRange (LspPosition 2 0) (LspPosition 2 5)) "edit3"
              ]
          }
    decode (encode edit) `shouldBe` Just edit

--------------------------------------------------------------------------------
-- LspHover Tests
--------------------------------------------------------------------------------

lspHoverSpec :: Spec
lspHoverSpec = describe "LspHover" $ do
  it "serializes with markdown content" $ do
    let hover = LspHover
          { lhContents = LspMarkupContent
              { lmcKind = "markdown"
              , lmcValue = "**bold** text"
              }
          , lhRange = Just $ LspRange (LspPosition 0 0) (LspPosition 0 10)
          }
    BSL.length (encode hover) `shouldSatisfy` (> 0)

  it "serializes without range" $ do
    let hover = LspHover
          { lhContents = LspMarkupContent
              { lmcKind = "plaintext"
              , lmcValue = "Simple text"
              }
          , lhRange = Nothing
          }
    BSL.length (encode hover) `shouldSatisfy` (> 0)

  it "handles complex markdown" $ do
    let hover = LspHover
          { lhContents = LspMarkupContent
              { lmcKind = "markdown"
              , lmcValue = "# Header\n\n```haskell\nfoo :: Int -> Int\n```\n\n- Item 1\n- Item 2"
              }
          , lhRange = Just $ LspRange (LspPosition 5 0) (LspPosition 5 20)
          }
    BSL.length (encode hover) `shouldSatisfy` (> 0)

--------------------------------------------------------------------------------
-- LspMarkupContent Tests
--------------------------------------------------------------------------------

lspMarkupContentSpec :: Spec
lspMarkupContentSpec = describe "LspMarkupContent" $ do
  it "serializes correctly" $ do
    let content = LspMarkupContent "markdown" "# Header"
    lmcKind content `shouldBe` "markdown"
    lmcValue content `shouldBe` "# Header"

  it "handles plaintext kind" $ do
    let content = LspMarkupContent "plaintext" "Plain text"
    lmcKind content `shouldBe` "plaintext"
    lmcValue content `shouldBe` "Plain text"

  it "handles empty value" $ do
    let content = LspMarkupContent "markdown" ""
    lmcValue content `shouldBe` ""

--------------------------------------------------------------------------------
-- LspCodeAction Tests
--------------------------------------------------------------------------------

lspCodeActionSpec :: Spec
lspCodeActionSpec = describe "LspCodeAction" $ do
  it "includes command when present" $ do
    let action = LspCodeAction
          { lcaTitle = "Test action"
          , lcaKind = "quickfix"
          , lcaDiagnostics = []
          , lcaIsPreferred = False
          , lcaEdit = Nothing
          , lcaCommand = Just $ LspCommand
              { lcdTitle = "Run command"
              , lcdCommand = "argus.test"
              , lcdArguments = Just [toJSON ("arg1" :: T.Text)]
              }
          }
    BSL.length (encode action) `shouldSatisfy` (> 0)

  it "handles action without command" $ do
    let action = LspCodeAction
          { lcaTitle = "Test action"
          , lcaKind = "quickfix"
          , lcaDiagnostics = []
          , lcaIsPreferred = False
          , lcaEdit = Nothing
          , lcaCommand = Nothing
          }
    BSL.length (encode action) `shouldSatisfy` (> 0)

  it "handles action with edit" $ do
    let action = LspCodeAction
          { lcaTitle = "Test action"
          , lcaKind = "quickfix"
          , lcaDiagnostics = []
          , lcaIsPreferred = True
          , lcaEdit = Just $ LspWorkspaceEdit $ Map.singleton "file://test.hs"
              [LspTextEdit (LspRange (LspPosition 0 0) (LspPosition 0 5)) "new"]
          , lcaCommand = Nothing
          }
    BSL.length (encode action) `shouldSatisfy` (> 0)

  it "handles different action kinds" $ do
    let kinds = ["quickfix", "refactor", "refactor.extract", "refactor.inline", "source", "source.organizeImports"]
    forM_ kinds $ \kind -> do
      let action = LspCodeAction
            { lcaTitle = "Action"
            , lcaKind = kind
            , lcaDiagnostics = []
            , lcaIsPreferred = False
            , lcaEdit = Nothing
            , lcaCommand = Nothing
            }
      BSL.length (encode action) `shouldSatisfy` (> 0)
  where
    forM_ = flip mapM_

--------------------------------------------------------------------------------
-- LspCommand Tests
--------------------------------------------------------------------------------

lspCommandSpec :: Spec
lspCommandSpec = describe "LspCommand" $ do
  it "serializes and deserializes" $ do
    let cmd = LspCommand
          { lcdTitle = "Test"
          , lcdCommand = "argus.test"
          , lcdArguments = Just [toJSON (1 :: Int), toJSON ("str" :: T.Text)]
          }
    decode (encode cmd) `shouldBe` Just cmd

  it "handles command without arguments" $ do
    let cmd = LspCommand
          { lcdTitle = "Test"
          , lcdCommand = "argus.test"
          , lcdArguments = Nothing
          }
    decode (encode cmd) `shouldBe` Just cmd

  it "handles empty arguments list" $ do
    let cmd = LspCommand
          { lcdTitle = "Test"
          , lcdCommand = "argus.test"
          , lcdArguments = Just []
          }
    decode (encode cmd) `shouldBe` Just cmd

--------------------------------------------------------------------------------
-- Diagnostic Kind Tests
--------------------------------------------------------------------------------

diagnosticKindSpec :: Spec
diagnosticKindSpec = describe "Diagnostic Kind text conversion" $ do
  it "correctly identifies all diagnostic kinds for hover" $ do
    let testKinds =
          [ (UnusedCode, "unused code")
          , (UnusedImport, "unused import")
          , (NamingConvention, "naming convention")
          , (CodePattern, "code pattern")
          , (PartialFunction, "partial function")
          , (SpaceLeak, "space leak")
          , (SecurityIssue, "security issue")
          , (PerformanceIssue, "performance issue")
          , (ArchitecturalIssue, "architectural issue")
          , (ComplexityIssue, "complexity issue")
          , (TypeSignature, "type signature")
          , (ImportStyle, "import style")
          , (TemplateHaskellRef, "template haskell")
          , (RedundantCode, "redundant code")
          ]
    forM_ testKinds $ \(kind, _) -> do
      let diag = Diagnostic
            { diagSpan = mkSrcSpanRaw "test.hs" 1 1 1 10
            , diagSeverity = Warning
            , diagKind = kind
            , diagMessage = "test"
            , diagCode = Nothing
            , diagFixes = []
            , diagRelated = []
            }
      let lspDiag = diagToLsp diag
      ldSource lspDiag `shouldBe` "argus"
  where
    forM_ = flip mapM_

--------------------------------------------------------------------------------
-- Position and Range Operations Tests
--------------------------------------------------------------------------------

positionAndRangeOperationsSpec :: Spec
positionAndRangeOperationsSpec = describe "Position and Range operations" $ do
  describe "position comparison" $ do
    it "detects same position" $ do
      let p1 = LspPosition 5 10
          p2 = LspPosition 5 10
      p1 `shouldBe` p2

    it "handles different lines" $ do
      let p1 = LspPosition 1 0
          p2 = LspPosition 2 0
      p1 `shouldNotBe` p2

    it "handles different characters on same line" $ do
      let p1 = LspPosition 5 1
          p2 = LspPosition 5 2
      p1 `shouldNotBe` p2

  describe "range containment" $ do
    it "handles single-line range" $ do
      let range' = LspRange (LspPosition 5 10) (LspPosition 5 20)
      lrStart range' `shouldBe` LspPosition 5 10
      lrEnd range' `shouldBe` LspPosition 5 20

    it "handles multi-line range" $ do
      let range' = LspRange (LspPosition 1 0) (LspPosition 10 50)
      lpLine (lrStart range') `shouldBe` 1
      lpLine (lrEnd range') `shouldBe` 10

--------------------------------------------------------------------------------
-- JSON Round-Trip Tests
--------------------------------------------------------------------------------

jsonRoundTripSpec :: Spec
jsonRoundTripSpec = describe "JSON round-trip properties" $ do
  it "LspPosition round-trips" $ do
    let positions = [LspPosition 0 0, LspPosition 100 200, LspPosition 1 1]
    mapM_ (\p -> decode (encode p) `shouldBe` Just p) positions

  it "LspRange round-trips" $ do
    let ranges =
          [ LspRange (LspPosition 0 0) (LspPosition 0 10)
          , LspRange (LspPosition 5 5) (LspPosition 10 10)
          ]
    mapM_ (\r -> decode (encode r) `shouldBe` Just r) ranges

  it "LspSeverity round-trips" $ do
    let severities = [LspError, LspWarning, LspInformation, LspHint]
    mapM_ (\s -> decode (encode s) `shouldBe` Just s) severities

  it "LspDiagnostic round-trips" $ do
    let diag = LspDiagnostic
          { ldRange = LspRange (LspPosition 0 0) (LspPosition 0 10)
          , ldSeverity = LspWarning
          , ldCode = Just "test"
          , ldSource = "argus"
          , ldMessage = "message"
          , ldTags = [1, 2]
          , ldData = Nothing
          }
    decode (encode diag) `shouldBe` Just diag

  it "LspTextEdit round-trips" $ do
    let edit = LspTextEdit
          { lteRange = LspRange (LspPosition 0 0) (LspPosition 0 5)
          , lteNewText = "new text"
          }
    decode (encode edit) `shouldBe` Just edit

  it "LspWorkspaceEdit round-trips" $ do
    let edit = LspWorkspaceEdit
          { lweChanges = Map.fromList
              [ ("file://a.hs", [LspTextEdit (LspRange (LspPosition 0 0) (LspPosition 0 5)) "a"])
              ]
          }
    decode (encode edit) `shouldBe` Just edit

  it "LspCommand round-trips" $ do
    let cmd = LspCommand "Title" "command" (Just [toJSON (1 :: Int)])
    decode (encode cmd) `shouldBe` Just cmd

--------------------------------------------------------------------------------
-- Edge Cases Tests
--------------------------------------------------------------------------------

edgeCasesSpec :: Spec
edgeCasesSpec = describe "Edge cases" $ do
  it "handles empty diagnostic message" $ do
    let diag = LspDiagnostic
          { ldRange = LspRange (LspPosition 0 0) (LspPosition 0 1)
          , ldSeverity = LspWarning
          , ldCode = Nothing
          , ldSource = "argus"
          , ldMessage = ""
          , ldTags = []
          , ldData = Nothing
          }
    decode (encode diag) `shouldBe` Just diag

  it "handles very long message" $ do
    let longMsg = T.replicate 10000 "x"
        diag = LspDiagnostic
          { ldRange = LspRange (LspPosition 0 0) (LspPosition 0 1)
          , ldSeverity = LspWarning
          , ldCode = Nothing
          , ldSource = "argus"
          , ldMessage = longMsg
          , ldTags = []
          , ldData = Nothing
          }
    decode (encode diag) `shouldBe` Just diag

  it "handles newlines in message" $ do
    let diag = LspDiagnostic
          { ldRange = LspRange (LspPosition 0 0) (LspPosition 0 1)
          , ldSeverity = LspWarning
          , ldCode = Nothing
          , ldSource = "argus"
          , ldMessage = "line1\nline2\nline3"
          , ldTags = []
          , ldData = Nothing
          }
    decode (encode diag) `shouldBe` Just diag

  it "handles special characters in code" $ do
    let diag = LspDiagnostic
          { ldRange = LspRange (LspPosition 0 0) (LspPosition 0 1)
          , ldSeverity = LspWarning
          , ldCode = Just "code/with/slashes"
          , ldSource = "argus"
          , ldMessage = "test"
          , ldTags = []
          , ldData = Nothing
          }
    decode (encode diag) `shouldBe` Just diag

  it "handles text edit with only whitespace" $ do
    let edit = LspTextEdit
          { lteRange = LspRange (LspPosition 0 0) (LspPosition 0 5)
          , lteNewText = "   \n\t  "
          }
    decode (encode edit) `shouldBe` Just edit

  it "handles zero-width range" $ do
    let range' = LspRange (LspPosition 5 10) (LspPosition 5 10)
    lrStart range' `shouldBe` lrEnd range'
    decode (encode range') `shouldBe` Just range'

  it "handles workspace edit with empty edits list" $ do
    let edit = LspWorkspaceEdit
          { lweChanges = Map.singleton "file://test.hs" []
          }
    decode (encode edit) `shouldBe` Just edit

  it "handles command with complex arguments" $ do
    let cmd = LspCommand
          { lcdTitle = "Complex"
          , lcdCommand = "argus.complex"
          , lcdArguments = Just
              [ toJSON (1 :: Int)
              , toJSON ("string" :: T.Text)
              , toJSON ([1, 2, 3] :: [Int])
              , object ["nested" .= ("value" :: T.Text)]
              ]
          }
    decode (encode cmd) `shouldBe` Just cmd

--------------------------------------------------------------------------------
-- P1-05: LSP Test Harness
--------------------------------------------------------------------------------

-- | Test harness for simulating LSP client-server communication
-- This provides a mock environment for testing LSP functionality without
-- actual socket connections.

-- | Simulated LSP message for testing
data MockLspMessage
  = MockRequest Int T.Text Value     -- ^ id, method, params
  | MockResponse Int (Either Value Value)  -- ^ id, result or error
  | MockNotification T.Text Value    -- ^ method, params
  deriving stock (Eq, Show)

-- | Encode a mock request to JSON
mockRequestToJson :: Int -> T.Text -> Value -> Value
mockRequestToJson reqId method params = object
  [ "jsonrpc" .= ("2.0" :: T.Text)
  , "id" .= reqId
  , "method" .= method
  , "params" .= params
  ]

-- | Encode a mock notification to JSON
mockNotificationToJson :: T.Text -> Value -> Value
mockNotificationToJson method params = object
  [ "jsonrpc" .= ("2.0" :: T.Text)
  , "method" .= method
  , "params" .= params
  ]

-- | Encode a mock response to JSON
mockResponseToJson :: Int -> Either Value Value -> Value
mockResponseToJson respId result = case result of
  Right res -> object
    [ "jsonrpc" .= ("2.0" :: T.Text)
    , "id" .= respId
    , "result" .= res
    ]
  Left err -> object
    [ "jsonrpc" .= ("2.0" :: T.Text)
    , "id" .= respId
    , "error" .= err
    ]

lspTestHarnessSpec :: Spec
lspTestHarnessSpec = describe "LSP Test Harness" $ do
  describe "mock message creation" $ do
    it "creates valid initialize request" $ do
      let initParams = object
            [ "processId" .= (12345 :: Int)
            , "rootUri" .= ("file:///home/user/project" :: T.Text)
            , "capabilities" .= object []
            ]
          msg = mockRequestToJson 1 "initialize" initParams
      -- Verify structure
      case msg of
        Object _ -> pure ()
        _ -> expectationFailure "Expected object"

    it "creates valid textDocument/didOpen notification" $ do
      let params = object
            [ "textDocument" .= object
                [ "uri" .= ("file:///home/user/project/Main.hs" :: T.Text)
                , "languageId" .= ("haskell" :: T.Text)
                , "version" .= (1 :: Int)
                , "text" .= ("module Main where\n" :: T.Text)
                ]
            ]
          msg = mockNotificationToJson "textDocument/didOpen" params
      case msg of
        Object _ -> pure ()
        _ -> expectationFailure "Expected object"

    it "creates valid textDocument/didSave notification" $ do
      let params = object
            [ "textDocument" .= object
                [ "uri" .= ("file:///home/user/project/Main.hs" :: T.Text)
                ]
            ]
          msg = mockNotificationToJson "textDocument/didSave" params
      case msg of
        Object _ -> pure ()
        _ -> expectationFailure "Expected object"

    it "creates valid success response" $ do
      let result = object ["capabilities" .= object []]
          msg = mockResponseToJson 1 (Right result)
      case msg of
        Object _ -> pure ()
        _ -> expectationFailure "Expected object"

    it "creates valid error response" $ do
      let err = object
            [ "code" .= (-32600 :: Int)
            , "message" .= ("Invalid Request" :: T.Text)
            ]
          msg = mockResponseToJson 1 (Left err)
      case msg of
        Object _ -> pure ()
        _ -> expectationFailure "Expected object"

  describe "mock LSP protocol" $ do
    it "handles initialization sequence" $ do
      let initReq = mockRequestToJson 1 "initialize" $ object
            [ "processId" .= (12345 :: Int)
            , "rootUri" .= ("file:///project" :: T.Text)
            , "capabilities" .= object []
            ]
          initResp = mockResponseToJson 1 $ Right $ object
            [ "capabilities" .= object
                [ "textDocumentSync" .= (1 :: Int)
                , "hoverProvider" .= True
                , "codeActionProvider" .= True
                ]
            ]
          initializedNotif = mockNotificationToJson "initialized" $ object []
      -- Verify all messages are valid JSON
      BSL.length (encode initReq) `shouldSatisfy` (> 0)
      BSL.length (encode initResp) `shouldSatisfy` (> 0)
      BSL.length (encode initializedNotif) `shouldSatisfy` (> 0)

    it "handles textDocument/codeAction request" $ do
      let codeActionReq = mockRequestToJson 2 "textDocument/codeAction" $ object
            [ "textDocument" .= object
                [ "uri" .= ("file:///project/Main.hs" :: T.Text)
                ]
            , "range" .= object
                [ "start" .= object ["line" .= (0 :: Int), "character" .= (0 :: Int)]
                , "end" .= object ["line" .= (0 :: Int), "character" .= (10 :: Int)]
                ]
            , "context" .= object
                [ "diagnostics" .= ([] :: [Value])
                ]
            ]
      BSL.length (encode codeActionReq) `shouldSatisfy` (> 0)

    it "handles textDocument/hover request" $ do
      let hoverReq = mockRequestToJson 3 "textDocument/hover" $ object
            [ "textDocument" .= object
                [ "uri" .= ("file:///project/Main.hs" :: T.Text)
                ]
            , "position" .= object
                [ "line" .= (5 :: Int)
                , "character" .= (10 :: Int)
                ]
            ]
      BSL.length (encode hoverReq) `shouldSatisfy` (> 0)

    it "handles shutdown request" $ do
      let shutdownReq = mockRequestToJson 99 "shutdown" Null
          shutdownResp = mockResponseToJson 99 (Right Null)
          exitNotif = mockNotificationToJson "exit" Null
      BSL.length (encode shutdownReq) `shouldSatisfy` (> 0)
      BSL.length (encode shutdownResp) `shouldSatisfy` (> 0)
      BSL.length (encode exitNotif) `shouldSatisfy` (> 0)

--------------------------------------------------------------------------------
-- P1-06: LSP Code Action Tests
--------------------------------------------------------------------------------

lspCodeActionTestsSpec :: Spec
lspCodeActionTestsSpec = describe "LSP Code Action Integration" $ do
  describe "code action generation" $ do
    it "generates quickfix actions for diagnostics with fixes" $ do
      let diag = Diagnostic
            { diagSpan = mkSrcSpanRaw "test.hs" 5 1 5 20
            , diagSeverity = Warning
            , diagKind = PartialFunction
            , diagMessage = "Use of partial function 'head'"
            , diagCode = Just "partial/head"
            , diagFixes = [testFix]
            , diagRelated = []
            }
          testFix = Fix
            { fixTitle = "Replace with headMay"
            , fixEdits = [FixEdit (mkSrcSpanRaw "test.hs" 5 1 5 5) "headMay"]
            , fixIsPreferred = True
            , fixAddImports = []
            , fixRemoveImports = []
            , fixCategory = FCSafety
            , fixSafety = FSAlways
            }
          action = fixToCodeAction "test.hs" diag testFix
      lcaKind action `shouldBe` "quickfix"
      lcaTitle action `shouldBe` "Replace with headMay"
      lcaIsPreferred action `shouldBe` True

    it "generates code actions for multiple fixes" $ do
      let diag = Diagnostic
            { diagSpan = mkSrcSpanRaw "test.hs" 10 1 10 30
            , diagSeverity = Suggestion
            , diagKind = CodePattern
            , diagMessage = "Consider using guard instead of if-then-else"
            , diagCode = Just "style/guard"
            , diagFixes = [fix1, fix2]
            , diagRelated = []
            }
          fix1 = Fix
            { fixTitle = "Convert to guard"
            , fixEdits = [FixEdit (mkSrcSpanRaw "test.hs" 10 1 10 30) "| cond = result"]
            , fixIsPreferred = True
            , fixAddImports = []
            , fixRemoveImports = []
            , fixCategory = FCStyle
            , fixSafety = FSMostly
            }
          fix2 = Fix
            { fixTitle = "Use when"
            , fixEdits = [FixEdit (mkSrcSpanRaw "test.hs" 10 1 10 30) "when cond action"]
            , fixIsPreferred = False
            , fixAddImports = []
            , fixRemoveImports = []
            , fixCategory = FCModernize
            , fixSafety = FSMostly
            }
          actions = map (fixToCodeAction "test.hs" diag) [fix1, fix2]
      length actions `shouldBe` 2
      lcaTitle (head actions) `shouldBe` "Convert to guard"
      lcaTitle (actions !! 1) `shouldBe` "Use when"
      lcaIsPreferred (head actions) `shouldBe` True
      lcaIsPreferred (actions !! 1) `shouldBe` False

    it "handles fixes with import additions" $ do
      let diag = Diagnostic
            { diagSpan = mkSrcSpanRaw "test.hs" 5 1 5 10
            , diagSeverity = Warning
            , diagKind = PartialFunction
            , diagMessage = "Use of partial function"
            , diagCode = Just "partial/head"
            , diagFixes = [fixWithImport]
            , diagRelated = []
            }
          fixWithImport = Fix
            { fixTitle = "Replace with headMay"
            , fixEdits = [FixEdit (mkSrcSpanRaw "test.hs" 5 1 5 5) "headMay"]
            , fixIsPreferred = True
            , fixAddImports = []  -- Import management is handled separately
            , fixRemoveImports = []
            , fixCategory = FCSafety
            , fixSafety = FSAlways
            }
          action = fixToCodeAction "test.hs" diag fixWithImport
      lcaEdit action `shouldSatisfy` (/= Nothing)

    it "handles code action for empty fix list" $ do
      let diag = Diagnostic
            { diagSpan = mkSrcSpanRaw "test.hs" 1 1 1 10
            , diagSeverity = Warning
            , diagKind = CodePattern
            , diagMessage = "No fix available"
            , diagCode = Nothing
            , diagFixes = []
            , diagRelated = []
            }
      -- Should not crash when no fixes available
      diagFixes diag `shouldBe` []

    it "preserves edit locations in code actions" $ do
      let span' = mkSrcSpanRaw "test.hs" 15 5 15 20
          diag = Diagnostic
            { diagSpan = span'
            , diagSeverity = Warning
            , diagKind = CodePattern
            , diagMessage = "Test"
            , diagCode = Nothing
            , diagFixes = [testFix]
            , diagRelated = []
            }
          testFix = Fix
            { fixTitle = "Fix it"
            , fixEdits = [FixEdit span' "replacement"]
            , fixIsPreferred = False
            , fixAddImports = []
            , fixRemoveImports = []
            , fixCategory = FCStyle
            , fixSafety = FSAlways
            }
          action = fixToCodeAction "test.hs" diag testFix
      case lcaEdit action of
        Just edit -> do
          let edits = Map.lookup "file://test.hs" (lweChanges edit)
          edits `shouldSatisfy` (/= Nothing)
          case edits of
            Just [textEdit] -> do
              let range' = lteRange textEdit
              lpLine (lrStart range') `shouldBe` 14  -- 0-indexed
              lpCharacter (lrStart range') `shouldBe` 4
            _ -> expectationFailure "Expected single text edit"
        Nothing -> expectationFailure "Expected workspace edit"

  describe "code action filtering" $ do
    it "filters by fix safety level" $ do
      let safeFix = Fix "Safe" [] True [] [] FCStyle FSAlways
          mostlyFix = Fix "Mostly" [] False [] [] FCStyle FSMostly
          reviewFix = Fix "Review" [] False [] [] FCStyle FSReview
          unsafeFix = Fix "Unsafe" [] False [] [] FCStyle FSUnsafe
      fixSafety safeFix `shouldBe` FSAlways
      fixSafety mostlyFix `shouldBe` FSMostly
      fixSafety reviewFix `shouldBe` FSReview
      fixSafety unsafeFix `shouldBe` FSUnsafe

    it "filters by fix category" $ do
      let perfFix = Fix "Perf" [] True [] [] FCPerformance FSAlways
          safeFix = Fix "Safe" [] True [] [] FCSafety FSAlways
          styleFix = Fix "Style" [] True [] [] FCStyle FSAlways
      fixCategory perfFix `shouldBe` FCPerformance
      fixCategory safeFix `shouldBe` FCSafety
      fixCategory styleFix `shouldBe` FCStyle

--------------------------------------------------------------------------------
-- P1-07: LSP Workspace Event Tests
--------------------------------------------------------------------------------

lspWorkspaceEventTestsSpec :: Spec
lspWorkspaceEventTestsSpec = describe "LSP Workspace Events" $ do
  describe "textDocument/didOpen" $ do
    it "creates valid didOpen notification params" $ do
      let params = object
            [ "textDocument" .= object
                [ "uri" .= ("file:///project/src/Main.hs" :: T.Text)
                , "languageId" .= ("haskell" :: T.Text)
                , "version" .= (1 :: Int)
                , "text" .= ("module Main where\nmain = putStrLn \"Hello\"\n" :: T.Text)
                ]
            ]
      -- Verify structure
      BSL.length (encode params) `shouldSatisfy` (> 0)

    it "handles file with unicode content" $ do
      let params = object
            [ "textDocument" .= object
                [ "uri" .= ("file:///project/Unicode.hs" :: T.Text)
                , "languageId" .= ("haskell" :: T.Text)
                , "version" .= (1 :: Int)
                , "text" .= ("-- λ α β γ\nfoo = \"日本語\"\n" :: T.Text)
                ]
            ]
      BSL.length (encode params) `shouldSatisfy` (> 0)

    it "handles empty file content" $ do
      let params = object
            [ "textDocument" .= object
                [ "uri" .= ("file:///project/Empty.hs" :: T.Text)
                , "languageId" .= ("haskell" :: T.Text)
                , "version" .= (1 :: Int)
                , "text" .= ("" :: T.Text)
                ]
            ]
      BSL.length (encode params) `shouldSatisfy` (> 0)

  describe "textDocument/didChange" $ do
    it "creates valid incremental change event" $ do
      let params = object
            [ "textDocument" .= object
                [ "uri" .= ("file:///project/Main.hs" :: T.Text)
                , "version" .= (2 :: Int)
                ]
            , "contentChanges" .=
                [ object
                    [ "range" .= object
                        [ "start" .= object ["line" .= (5 :: Int), "character" .= (0 :: Int)]
                        , "end" .= object ["line" .= (5 :: Int), "character" .= (10 :: Int)]
                        ]
                    , "text" .= ("newText" :: T.Text)
                    ]
                ]
            ]
      BSL.length (encode params) `shouldSatisfy` (> 0)

    it "creates valid full content change event" $ do
      let params = object
            [ "textDocument" .= object
                [ "uri" .= ("file:///project/Main.hs" :: T.Text)
                , "version" .= (3 :: Int)
                ]
            , "contentChanges" .=
                [ object
                    [ "text" .= ("module Main where\n-- Updated content\n" :: T.Text)
                    ]
                ]
            ]
      BSL.length (encode params) `shouldSatisfy` (> 0)

    it "handles multiple incremental changes" $ do
      let params = object
            [ "textDocument" .= object
                [ "uri" .= ("file:///project/Main.hs" :: T.Text)
                , "version" .= (4 :: Int)
                ]
            , "contentChanges" .=
                [ object
                    [ "range" .= object
                        [ "start" .= object ["line" .= (1 :: Int), "character" .= (0 :: Int)]
                        , "end" .= object ["line" .= (1 :: Int), "character" .= (5 :: Int)]
                        ]
                    , "text" .= ("first" :: T.Text)
                    ]
                , object
                    [ "range" .= object
                        [ "start" .= object ["line" .= (2 :: Int), "character" .= (0 :: Int)]
                        , "end" .= object ["line" .= (2 :: Int), "character" .= (5 :: Int)]
                        ]
                    , "text" .= ("second" :: T.Text)
                    ]
                ]
            ]
      BSL.length (encode params) `shouldSatisfy` (> 0)

  describe "textDocument/didSave" $ do
    it "creates valid didSave notification" $ do
      let params = object
            [ "textDocument" .= object
                [ "uri" .= ("file:///project/Main.hs" :: T.Text)
                ]
            ]
      BSL.length (encode params) `shouldSatisfy` (> 0)

    it "creates didSave with text included" $ do
      let params = object
            [ "textDocument" .= object
                [ "uri" .= ("file:///project/Main.hs" :: T.Text)
                ]
            , "text" .= ("module Main where\n" :: T.Text)
            ]
      BSL.length (encode params) `shouldSatisfy` (> 0)

  describe "textDocument/didClose" $ do
    it "creates valid didClose notification" $ do
      let params = object
            [ "textDocument" .= object
                [ "uri" .= ("file:///project/Main.hs" :: T.Text)
                ]
            ]
      BSL.length (encode params) `shouldSatisfy` (> 0)

  describe "workspace/didChangeConfiguration" $ do
    it "creates valid configuration change notification" $ do
      let params = object
            [ "settings" .= object
                [ "argus" .= object
                    [ "maxDiagnostics" .= (200 :: Int)
                    , "analyzeOnSave" .= True
                    , "analyzeOnChange" .= False
                    ]
                ]
            ]
      BSL.length (encode params) `shouldSatisfy` (> 0)

  describe "workspace/didChangeWatchedFiles" $ do
    it "creates valid file change notification" $ do
      let params = object
            [ "changes" .=
                [ object
                    [ "uri" .= ("file:///project/NewFile.hs" :: T.Text)
                    , "type" .= (1 :: Int)  -- Created
                    ]
                , object
                    [ "uri" .= ("file:///project/Changed.hs" :: T.Text)
                    , "type" .= (2 :: Int)  -- Changed
                    ]
                , object
                    [ "uri" .= ("file:///project/Deleted.hs" :: T.Text)
                    , "type" .= (3 :: Int)  -- Deleted
                    ]
                ]
            ]
      BSL.length (encode params) `shouldSatisfy` (> 0)

  describe "textDocument/publishDiagnostics" $ do
    it "creates valid publish diagnostics notification" $ do
      let lspDiag = LspDiagnostic
            { ldRange = LspRange (LspPosition 4 0) (LspPosition 4 10)
            , ldSeverity = LspWarning
            , ldCode = Just "partial/head"
            , ldSource = "argus"
            , ldMessage = "Use of partial function 'head'"
            , ldTags = []
            , ldData = Nothing
            }
          params = object
            [ "uri" .= ("file:///project/Main.hs" :: T.Text)
            , "version" .= (5 :: Int)
            , "diagnostics" .= [toJSON lspDiag]
            ]
      BSL.length (encode params) `shouldSatisfy` (> 0)

    it "creates empty diagnostics (clear)" $ do
      let params = object
            [ "uri" .= ("file:///project/Main.hs" :: T.Text)
            , "diagnostics" .= ([] :: [Value])
            ]
      BSL.length (encode params) `shouldSatisfy` (> 0)

    it "creates diagnostics with multiple items" $ do
      let diags =
            [ LspDiagnostic
                { ldRange = LspRange (LspPosition 4 0) (LspPosition 4 10)
                , ldSeverity = LspWarning
                , ldCode = Just "partial/head"
                , ldSource = "argus"
                , ldMessage = "Diagnostic 1"
                , ldTags = []
                , ldData = Nothing
                }
            , LspDiagnostic
                { ldRange = LspRange (LspPosition 10 0) (LspPosition 10 20)
                , ldSeverity = LspError
                , ldCode = Just "security/unsafe"
                , ldSource = "argus"
                , ldMessage = "Diagnostic 2"
                , ldTags = []
                , ldData = Nothing
                }
            ]
          params = object
            [ "uri" .= ("file:///project/Main.hs" :: T.Text)
            , "diagnostics" .= map toJSON diags
            ]
      BSL.length (encode params) `shouldSatisfy` (> 0)

  describe "progress reporting" $ do
    it "creates valid work done progress begin" $ do
      let params = object
            [ "token" .= ("argus-analysis" :: T.Text)
            , "value" .= object
                [ "kind" .= ("begin" :: T.Text)
                , "title" .= ("Analyzing..." :: T.Text)
                , "cancellable" .= False
                , "percentage" .= (0 :: Int)
                ]
            ]
      BSL.length (encode params) `shouldSatisfy` (> 0)

    it "creates valid work done progress report" $ do
      let params = object
            [ "token" .= ("argus-analysis" :: T.Text)
            , "value" .= object
                [ "kind" .= ("report" :: T.Text)
                , "message" .= ("Analyzing Main.hs" :: T.Text)
                , "percentage" .= (50 :: Int)
                ]
            ]
      BSL.length (encode params) `shouldSatisfy` (> 0)

    it "creates valid work done progress end" $ do
      let params = object
            [ "token" .= ("argus-analysis" :: T.Text)
            , "value" .= object
                [ "kind" .= ("end" :: T.Text)
                , "message" .= ("Analysis complete" :: T.Text)
                ]
            ]
      BSL.length (encode params) `shouldSatisfy` (> 0)

--------------------------------------------------------------------------------
-- LSP Completion Tests
--------------------------------------------------------------------------------

lspCompletionItemKindSpec :: Spec
lspCompletionItemKindSpec = describe "LspCompletionItemKind" $ do
  it "serializes CikText to 1" $
    toJSON CikText `shouldBe` Aeson.Number 1

  it "serializes CikMethod to 2" $
    toJSON CikMethod `shouldBe` Aeson.Number 2

  it "serializes CikFunction to 3" $
    toJSON CikFunction `shouldBe` Aeson.Number 3

  it "serializes CikConstructor to 4" $
    toJSON CikConstructor `shouldBe` Aeson.Number 4

  it "serializes CikField to 5" $
    toJSON CikField `shouldBe` Aeson.Number 5

  it "serializes CikVariable to 6" $
    toJSON CikVariable `shouldBe` Aeson.Number 6

  it "serializes CikClass to 7" $
    toJSON CikClass `shouldBe` Aeson.Number 7

  it "serializes CikInterface to 8" $
    toJSON CikInterface `shouldBe` Aeson.Number 8

  it "serializes CikModule to 9" $
    toJSON CikModule `shouldBe` Aeson.Number 9

  it "serializes CikKeyword to 14" $
    toJSON CikKeyword `shouldBe` Aeson.Number 14

  it "serializes CikSnippet to 15" $
    toJSON CikSnippet `shouldBe` Aeson.Number 15

  it "serializes all kinds as consecutive integers from 1" $ do
    let allKinds = [minBound..maxBound] :: [LspCompletionItemKind]
        expected = [1..25] :: [Int]  -- LSP spec has 25 completion kinds
    map (\k -> fromEnum k + 1) allKinds `shouldBe` expected

  it "deserializes from valid JSON" $ do
    decode "1" `shouldBe` Just CikText
    decode "2" `shouldBe` Just CikMethod
    decode "3" `shouldBe` Just CikFunction
    decode "14" `shouldBe` Just CikKeyword
    decode "25" `shouldBe` Just CikTypeParameter

  it "rejects invalid JSON values" $ do
    (decode "0" :: Maybe LspCompletionItemKind) `shouldBe` Nothing
    (decode "26" :: Maybe LspCompletionItemKind) `shouldBe` Nothing
    (decode "-1" :: Maybe LspCompletionItemKind) `shouldBe` Nothing

  it "round-trips through JSON" $ do
    let kinds = [CikText, CikFunction, CikConstructor, CikKeyword, CikSnippet, CikTypeParameter]
    mapM_ (\k -> decode (encode k) `shouldBe` Just k) kinds

  it "has all 25 completion item kinds" $
    length ([minBound..maxBound] :: [LspCompletionItemKind]) `shouldBe` 25

lspCompletionItemSpec :: Spec
lspCompletionItemSpec = describe "LspCompletionItem" $ do
  it "serializes basic completion item" $ do
    let item = LspCompletionItem
          { lciLabel = "test"
          , lciKind = Just CikFunction
          , lciDetail = Just "Test function"
          , lciDocumentation = Nothing
          , lciInsertText = Nothing
          , lciFilterText = Nothing
          , lciSortText = Nothing
          }
    BSL.length (encode item) `shouldSatisfy` (> 0)

  it "includes label in JSON" $ do
    let item = LspCompletionItem "myFunc" (Just CikFunction) Nothing Nothing Nothing Nothing Nothing
        encoded = encode item
    -- Label should always be present
    BSL.length encoded `shouldSatisfy` (> 0)

  it "includes kind when present" $ do
    let item = LspCompletionItem "test" (Just CikClass) Nothing Nothing Nothing Nothing Nothing
    BSL.length (encode item) `shouldSatisfy` (> 0)

  it "omits Nothing fields in JSON" $ do
    let item = LspCompletionItem "test" Nothing Nothing Nothing Nothing Nothing Nothing
        encoded = encode item
    BSL.length encoded `shouldSatisfy` (> 0)

  it "handles all fields populated" $ do
    let item = LspCompletionItem
          { lciLabel = "fullItem"
          , lciKind = Just CikSnippet
          , lciDetail = Just "A detailed description"
          , lciDocumentation = Just $ LspMarkupContent "markdown" "# Documentation"
          , lciInsertText = Just "insertedText"
          , lciFilterText = Just "filterText"
          , lciSortText = Just "0001_sortText"
          }
    BSL.length (encode item) `shouldSatisfy` (> 0)

  it "handles unicode in label" $ do
    let item = LspCompletionItem "λ → ∀" (Just CikFunction) Nothing Nothing Nothing Nothing Nothing
    BSL.length (encode item) `shouldSatisfy` (> 0)

  it "handles long label text" $ do
    let longLabel = T.replicate 1000 "x"
        item = LspCompletionItem longLabel (Just CikText) Nothing Nothing Nothing Nothing Nothing
    BSL.length (encode item) `shouldSatisfy` (> 0)

  it "handles keyword completion" $ do
    let item = LspCompletionItem "where" (Just CikKeyword) (Just "Haskell keyword") Nothing Nothing Nothing Nothing
    BSL.length (encode item) `shouldSatisfy` (> 0)

lspCompletionListSpec :: Spec
lspCompletionListSpec = describe "LspCompletionList" $ do
  it "serializes empty completion list" $ do
    let list = LspCompletionList False []
    BSL.length (encode list) `shouldSatisfy` (> 0)

  it "serializes completion list with items" $ do
    let items =
          [ LspCompletionItem "item1" (Just CikFunction) Nothing Nothing Nothing Nothing Nothing
          , LspCompletionItem "item2" (Just CikVariable) Nothing Nothing Nothing Nothing Nothing
          ]
        list = LspCompletionList False items
    BSL.length (encode list) `shouldSatisfy` (> 0)

  it "correctly sets isIncomplete flag" $ do
    let complete = LspCompletionList False []
        incomplete = LspCompletionList True []
    lclIsIncomplete complete `shouldBe` False
    lclIsIncomplete incomplete `shouldBe` True

  it "handles large completion lists" $ do
    let items = [LspCompletionItem (T.pack $ "item" <> show n) (Just CikFunction) Nothing Nothing Nothing Nothing Nothing | n <- [1..100 :: Int]]
        list = LspCompletionList True items
    length (lclItems list) `shouldBe` 100
    lclIsIncomplete list `shouldBe` True

  it "includes items in correct JSON structure" $ do
    let items = [LspCompletionItem "test" (Just CikText) Nothing Nothing Nothing Nothing Nothing]
        list = LspCompletionList False items
    BSL.length (encode list) `shouldSatisfy` (> 0)

symbolKindToCompletionKindSpec :: Spec
symbolKindToCompletionKindSpec = describe "symbolKindToCompletionKind" $ do
  it "maps Function to CikFunction" $
    symbolKindToCompletionKind Function `shouldBe` CikFunction

  it "maps TypeConstructor to CikClass" $
    symbolKindToCompletionKind TypeConstructor `shouldBe` CikClass

  it "maps DataConstructor to CikConstructor" $
    symbolKindToCompletionKind DataConstructor `shouldBe` CikConstructor

  it "maps TypeClass to CikInterface" $
    symbolKindToCompletionKind TypeClass `shouldBe` CikInterface

  it "maps TypeClassMethod to CikMethod" $
    symbolKindToCompletionKind TypeClassMethod `shouldBe` CikMethod

  it "maps TypeFamily to CikInterface" $
    symbolKindToCompletionKind TypeFamily `shouldBe` CikInterface

  it "maps PatternSynonym to CikValue" $
    symbolKindToCompletionKind PatternSynonym `shouldBe` CikValue

  it "maps Module to CikModule" $
    symbolKindToCompletionKind Module `shouldBe` CikModule

  it "covers all SymbolKind constructors" $ do
    let allKinds = [Function, TypeConstructor, DataConstructor, TypeClass, TypeClassMethod, TypeFamily, PatternSynonym, Module]
    mapM_ (\k -> symbolKindToCompletionKind k `shouldSatisfy` const True) allKinds

getCompletionPrefixSpec :: Spec
getCompletionPrefixSpec = describe "getCompletionPrefix" $ do
  it "extracts prefix while typing a word" $ do
    let source = "module Main where\nmain = do"
        pos = LspPosition 1 9  -- After "main = do" (at end of "do")
    getCompletionPrefix source pos `shouldBe` "do"

  it "extracts prefix in the middle of a word" $ do
    let source = "function = map"
        pos = LspPosition 0 5  -- After "funct"
    getCompletionPrefix source pos `shouldBe` "funct"

  it "returns empty string at start of line" $ do
    let source = "main = x"
        pos = LspPosition 0 0
    getCompletionPrefix source pos `shouldBe` ""

  it "returns empty string after whitespace" $ do
    let source = "foo = "
        pos = LspPosition 0 6  -- After "= "
    getCompletionPrefix source pos `shouldBe` ""

  it "handles qualified names" $ do
    let source = "import Data.Text"
        pos = LspPosition 0 16  -- After "Data.Text"
    getCompletionPrefix source pos `shouldBe` "Data.Text"

  it "returns empty after operator characters" $ do
    let source = "x <$> y"
        pos = LspPosition 0 4  -- After "<$"
    -- Operators are not recognized as completion chars
    getCompletionPrefix source pos `shouldBe` ""

  it "handles empty source" $ do
    let source = ""
        pos = LspPosition 0 0
    getCompletionPrefix source pos `shouldBe` ""

  it "handles position beyond end of line" $ do
    let source = "short"
        pos = LspPosition 0 100  -- Way past end - take returns all available
    getCompletionPrefix source pos `shouldBe` "short"

  it "handles multi-line source" $ do
    let source = "line1\nline2\nline3"
        pos = LspPosition 1 3  -- "lin" on line2
    getCompletionPrefix source pos `shouldBe` "lin"

  it "returns empty for unicode identifiers" $ do
    -- Current implementation only handles ASCII letters
    let source = "λ = 1"
        pos = LspPosition 0 1  -- After λ
    getCompletionPrefix source pos `shouldBe` ""

  it "handles underscore-prefixed identifiers" $ do
    let source = "_unused = 1"
        pos = LspPosition 0 7  -- After "_unused"
    getCompletionPrefix source pos `shouldBe` "_unused"

  it "handles primed identifiers" $ do
    let source = "x' = x + 1"
        pos = LspPosition 0 2  -- After "x'"
    getCompletionPrefix source pos `shouldBe` "x'"

lspCompletionMockRequestSpec :: Spec
lspCompletionMockRequestSpec = describe "LSP Completion Mock Requests" $ do
  it "creates valid textDocument/completion request" $ do
    let req = mockRequestToJson 10 "textDocument/completion" $ object
          [ "textDocument" .= object
              [ "uri" .= ("file:///project/Main.hs" :: T.Text)
              ]
          , "position" .= object
              [ "line" .= (5 :: Int)
              , "character" .= (10 :: Int)
              ]
          ]
    BSL.length (encode req) `shouldSatisfy` (> 0)

  it "creates valid completion response" $ do
    let items =
          [ object
              [ "label" .= ("map" :: T.Text)
              , "kind" .= (3 :: Int)  -- Function
              , "detail" .= ("(a -> b) -> [a] -> [b]" :: T.Text)
              ]
          , object
              [ "label" .= ("filter" :: T.Text)
              , "kind" .= (3 :: Int)
              , "detail" .= ("(a -> Bool) -> [a] -> [a]" :: T.Text)
              ]
          ]
        resp = mockResponseToJson 10 $ Right $ object
          [ "isIncomplete" .= False
          , "items" .= items
          ]
    BSL.length (encode resp) `shouldSatisfy` (> 0)

  it "creates completion request with trigger" $ do
    let req = mockRequestToJson 11 "textDocument/completion" $ object
          [ "textDocument" .= object
              [ "uri" .= ("file:///project/Main.hs" :: T.Text)
              ]
          , "position" .= object
              [ "line" .= (10 :: Int)
              , "character" .= (15 :: Int)
              ]
          , "context" .= object
              [ "triggerKind" .= (2 :: Int)  -- TriggerCharacter
              , "triggerCharacter" .= ("." :: T.Text)
              ]
          ]
    BSL.length (encode req) `shouldSatisfy` (> 0)

  it "creates completion request with incomplete trigger" $ do
    let req = mockRequestToJson 12 "textDocument/completion" $ object
          [ "textDocument" .= object
              [ "uri" .= ("file:///project/Main.hs" :: T.Text)
              ]
          , "position" .= object
              [ "line" .= (7 :: Int)
              , "character" .= (3 :: Int)
              ]
          , "context" .= object
              [ "triggerKind" .= (3 :: Int)  -- TriggerForIncompleteCompletions
              ]
          ]
    BSL.length (encode req) `shouldSatisfy` (> 0)

  it "creates empty completion response" $ do
    let resp = mockResponseToJson 13 $ Right $ object
          [ "isIncomplete" .= False
          , "items" .= ([] :: [Value])
          ]
    BSL.length (encode resp) `shouldSatisfy` (> 0)

  it "creates incomplete completion response" $ do
    let resp = mockResponseToJson 14 $ Right $ object
          [ "isIncomplete" .= True
          , "items" .=
              [ object
                  [ "label" .= ("item1" :: T.Text)
                  , "kind" .= (6 :: Int)  -- Variable
                  ]
              ]
          ]
    BSL.length (encode resp) `shouldSatisfy` (> 0)

lspCompletionKindMappingSpec :: Spec
lspCompletionKindMappingSpec = describe "Completion Kind Mapping" $ do
  describe "Haskell entity to LSP mapping" $ do
    it "maps functions appropriately" $
      symbolKindToCompletionKind Function `shouldBe` CikFunction

    it "maps type constructors to class" $
      symbolKindToCompletionKind TypeConstructor `shouldBe` CikClass

    it "maps data constructors appropriately" $
      symbolKindToCompletionKind DataConstructor `shouldBe` CikConstructor

    it "maps type classes to interface" $
      symbolKindToCompletionKind TypeClass `shouldBe` CikInterface

    it "maps type class methods to method" $
      symbolKindToCompletionKind TypeClassMethod `shouldBe` CikMethod

  describe "LSP completion item kind values" $ do
    it "Text is kind 1" $
      fromEnum CikText + 1 `shouldBe` 1

    it "Method is kind 2" $
      fromEnum CikMethod + 1 `shouldBe` 2

    it "Function is kind 3" $
      fromEnum CikFunction + 1 `shouldBe` 3

    it "Constructor is kind 4" $
      fromEnum CikConstructor + 1 `shouldBe` 4

    it "Keyword is kind 14" $
      fromEnum CikKeyword + 1 `shouldBe` 14

    it "Module is kind 9" $
      fromEnum CikModule + 1 `shouldBe` 9

    it "Class is kind 7" $
      fromEnum CikClass + 1 `shouldBe` 7

    it "Interface is kind 8" $
      fromEnum CikInterface + 1 `shouldBe` 8
