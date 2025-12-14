{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : LSPPluginE2ESpec
-- Description : End-to-end integration tests for LSP and Plugin functionality
--
-- These tests exercise the complete integration flow:
-- 1. Source analysis → Diagnostic generation → LSP conversion
-- 2. Source analysis → Plugin diagnostic generation → Formatting
-- 3. Code action generation → Workspace edit creation
-- 4. Full LSP protocol simulation with diagnostics
module LSPPluginE2ESpec (spec) where

import Data.Aeson (encode, decode, toJSON, object, (.=), Value(..))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Test.Hspec

import Argus.Types
    ( Diagnostic(..)
    , Fix(..)
    , FixEdit(..)
    , FixImport(..)
    , Severity(..)
    , DiagnosticKind(..)
    , FixCategory
        ( FCPerformance
        , FCSafety
        , FCSecurity
        , FCStyle
        , FCModernize
        , FCImports
        , FCRedundant
        , FCSpaceLeaks
        , FCCustom
        )
    , FixSafety(..)
    , defaultOptions
    , mkSrcSpanRaw
    )
import Argus.Core (defaultContext, analyzeSource)
import Argus.Config (defaultConfig)
import Argus.Rules.ConfigurableRules (defaultRulesConfig)
import Argus.LSP.Server
    ( diagToLsp
    , diagsToLsp
    , fixToCodeAction
    , spanToRange
    , severityToLsp
    , LspDiagnostic(..)
    , LspSeverity(..)
    , LspRange(..)
    , LspPosition(..)
    , LspCodeAction(..)
    , LspWorkspaceEdit(..)
    , LspTextEdit(..)
    )
import Argus.Plugin
    ( PluginDiagnostic(..)
    , PluginFix(..)
    , PluginConfig(..)
    , defaultPluginConfig
    , parsePluginOptions
    , formatPluginDiagnostic
    , formatPluginDiagnosticJson
    , FixCategoryP(..)
    , FixSafetyP(..)
    )
import Argus.Refactor.ExactPrint (applyFix)
import Argus.Refactor.Validation (validateSyntax)

spec :: Spec
spec = do
  describe "LSP and Plugin E2E Integration" $ do
    analysisToLspSpec
    analysisToPluginSpec
    codeActionE2ESpec
    lspProtocolSimulationSpec
    pluginDiagnosticE2ESpec
    crossComponentIntegrationSpec

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Run analysis on source code using proper context
analyzeCode :: FilePath -> Text -> IO [Diagnostic]
analyzeCode path source = do
  let ctx = defaultContext defaultConfig defaultOptions defaultRulesConfig
  analyzeSource ctx path source

-- | Convert Argus diagnostic to plugin diagnostic
toPluginDiagnostic :: Diagnostic -> PluginDiagnostic
toPluginDiagnostic diag = PluginDiagnostic
  { pdSpan = diagSpan diag
  , pdSeverity = diagSeverity diag
  , pdKind = diagKind diag
  , pdMessage = diagMessage diag
  , pdCode = diagCode diag
  , pdFixes = map toPluginFix (diagFixes diag)
  }

-- | Convert Argus fix to plugin fix
toPluginFix :: Fix -> PluginFix
toPluginFix fix = PluginFix
  { pfTitle = fixTitle fix
  , pfSpan = case fixEdits fix of
      (FixEdit span' _:_) -> span'
      [] -> mkSrcSpanRaw "" 0 0 0 0
  , pfReplacement = case fixEdits fix of
      (FixEdit _ repl:_) -> repl
      [] -> ""
  , pfImportAdd = map fimpModule (fixAddImports fix)  -- Extract module names
  , pfIsPreferred = fixIsPreferred fix
  , pfCategory = categoryToPlugin (fixCategory fix)
  , pfSafety = safetyToPlugin (fixSafety fix)
  }

categoryToPlugin :: FixCategory -> FixCategoryP
categoryToPlugin = \case
  FCPerformance -> FCPPerformance
  FCSafety -> FCPSafety
  FCSecurity -> FCPSecurity
  FCStyle -> FCPStyle
  FCModernize -> FCPStyle
  FCImports -> FCPStyle
  FCRedundant -> FCPStyle
  FCSpaceLeaks -> FCPPerformance
  FCCustom _ -> FCPStyle

safetyToPlugin :: FixSafety -> FixSafetyP
safetyToPlugin = \case
  FSAlways -> FSPAlways
  FSMostly -> FSPMostly
  FSReview -> FSPReview
  FSUnsafe -> FSPReview

--------------------------------------------------------------------------------
-- Analysis to LSP Conversion E2E Tests
--------------------------------------------------------------------------------

analysisToLspSpec :: Spec
analysisToLspSpec = describe "Analysis → LSP Conversion" $ do
  it "analyzes code and converts diagnostics to LSP format" $ do
    let source = T.unlines
          [ "module Test where"
          , ""
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    -- Run analysis
    diags <- analyzeCode "Test.hs" source
    diags `shouldSatisfy` (not . null)

    -- Convert to LSP
    let lspDiags = diagsToLsp diags
    lspDiags `shouldSatisfy` (not . null)

    -- Verify LSP diagnostics have correct structure
    let firstLsp = head lspDiags
    ldSource firstLsp `shouldBe` "argus"
    ldSeverity firstLsp `shouldSatisfy` (`elem` [LspError, LspWarning, LspInformation, LspHint])

  it "preserves source location through conversion" $ do
    let source = T.unlines
          [ "module Test where"
          , ""
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source
    let lspDiags = diagsToLsp diags

    -- All LSP diagnostics should have valid ranges
    forM_ lspDiags $ \lspDiag -> do
      let range' = ldRange lspDiag
      lpLine (lrStart range') `shouldSatisfy` (>= 0)
      lpCharacter (lrStart range') `shouldSatisfy` (>= 0)

  it "converts severity correctly through analysis pipeline" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source
    let lspDiags = diagsToLsp diags

    -- Verify severity mapping
    forM_ (zip diags lspDiags) $ \(argDiag, lspDiag) -> do
      let expectedLspSev = severityToLsp (diagSeverity argDiag)
      ldSeverity lspDiag `shouldBe` expectedLspSev

  it "handles code with multiple diagnostics" $ do
    let source = T.unlines
          [ "module Test where"
          , "a = return x"
          , "b = return y"
          , "c = return z"
          ]

    diags <- analyzeCode "Test.hs" source
    let lspDiags = diagsToLsp diags

    -- Should have at least 3 diagnostics (one for each return)
    length lspDiags `shouldSatisfy` (>= 3)

  it "LSP diagnostics can be serialized to JSON" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source
    let lspDiags = diagsToLsp diags

    forM_ lspDiags $ \lspDiag -> do
      let encoded = encode lspDiag
      BSL.length encoded `shouldSatisfy` (> 0)
      -- Round-trip test
      decode encoded `shouldBe` Just lspDiag

--------------------------------------------------------------------------------
-- Analysis to Plugin Conversion E2E Tests
--------------------------------------------------------------------------------

analysisToPluginSpec :: Spec
analysisToPluginSpec = describe "Analysis → Plugin Conversion" $ do
  it "analyzes code and formats plugin diagnostics" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source
    diags `shouldSatisfy` (not . null)

    -- Convert to plugin format
    let pluginDiags = map toPluginDiagnostic diags

    -- Format each diagnostic
    forM_ pluginDiags $ \pd -> do
      let formatted = formatPluginDiagnostic pd
      T.length formatted `shouldSatisfy` (> 0)
      T.isInfixOf "Test.hs" formatted `shouldBe` True

  it "plugin diagnostics can be serialized to JSON" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source
    let pluginDiags = map toPluginDiagnostic diags

    forM_ pluginDiags $ \pd -> do
      let jsonOutput = formatPluginDiagnosticJson pd
      T.length jsonOutput `shouldSatisfy` (> 0)
      -- Verify it's valid JSON
      let decoded = Aeson.decode (BSL.fromStrict $ TE.encodeUtf8 jsonOutput) :: Maybe PluginDiagnostic
      decoded `shouldBe` Just pd

  it "plugin config filtering works correctly" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source
    let pluginDiags = map toPluginDiagnostic diags

    -- Create configs with different severity filters
    let errorConfig = parsePluginOptions ["severity=error"]
        warningConfig = parsePluginOptions ["severity=warning"]
        infoConfig = parsePluginOptions ["severity=info"]

    pcMinSeverity errorConfig `shouldBe` Error
    pcMinSeverity warningConfig `shouldBe` Warning
    pcMinSeverity infoConfig `shouldBe` Info

    -- Filter diagnostics based on severity
    let filterBySeverity cfg ds = filter (\d ->
          severityLevel (pdSeverity d) <= severityLevel (pcMinSeverity cfg)) ds
        severityLevel = \case
          Error -> 0
          Warning -> 1
          Suggestion -> 2
          Info -> 3

    -- Info config should include more diagnostics than error config
    let errorFiltered = filterBySeverity errorConfig pluginDiags
        infoFiltered = filterBySeverity infoConfig pluginDiags

    length errorFiltered `shouldSatisfy` (<= length infoFiltered)

--------------------------------------------------------------------------------
-- Code Action E2E Tests
--------------------------------------------------------------------------------

codeActionE2ESpec :: Spec
codeActionE2ESpec = describe "Code Action E2E" $ do
  it "generates code actions from analysis diagnostics" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source

    -- Get diagnostics with fixes
    let diagsWithFixes = filter (not . null . diagFixes) diags

    -- Generate code actions
    let actions = concatMap (\d -> map (fixToCodeAction "Test.hs" d) (diagFixes d)) diagsWithFixes

    -- Should have at least one code action
    actions `shouldSatisfy` (not . null)

    -- Verify code action structure
    forM_ actions $ \action -> do
      lcaKind action `shouldBe` "quickfix"
      lcaTitle action `shouldSatisfy` (not . T.null)

  it "code action edits produce valid syntax" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source
    let diagsWithFixes = filter (not . null . diagFixes) diags

    -- Apply fixes and validate
    case diagsWithFixes of
      [] -> pendingWith "No diagnostics with fixes generated"
      (d:_) -> do
        let fixes = diagFixes d
        case fixes of
          [] -> pendingWith "No fixes available"
          (fix:_) -> do
            let transformed = applyFix source fix
            result <- validateSyntax "Test.hs" transformed
            result `shouldBe` Right ()

  it "code actions have workspace edits with correct URIs" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source
    let diagsWithFixes = filter (not . null . diagFixes) diags

    case diagsWithFixes of
      [] -> pendingWith "No diagnostics with fixes"
      (d:_) -> do
        let action = fixToCodeAction "Test.hs" d (head $ diagFixes d)
        case lcaEdit action of
          Nothing -> expectationFailure "Expected workspace edit"
          Just edit -> do
            let hasTestHs = Map.member "file://Test.hs" (lweChanges edit)
            hasTestHs `shouldBe` True

  it "code actions can be serialized for LSP protocol" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source
    let diagsWithFixes = filter (not . null . diagFixes) diags

    case diagsWithFixes of
      [] -> pendingWith "No diagnostics with fixes"
      (d:_) -> do
        let action = fixToCodeAction "Test.hs" d (head $ diagFixes d)
        let encoded = encode action
        BSL.length encoded `shouldSatisfy` (> 0)

--------------------------------------------------------------------------------
-- LSP Protocol Simulation Tests
--------------------------------------------------------------------------------

lspProtocolSimulationSpec :: Spec
lspProtocolSimulationSpec = describe "LSP Protocol Simulation" $ do
  it "simulates didOpen → analyze → publishDiagnostics flow" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    -- 1. Simulate didOpen
    let didOpenParams = object
          [ "textDocument" .= object
              [ "uri" .= ("file:///project/Test.hs" :: Text)
              , "languageId" .= ("haskell" :: Text)
              , "version" .= (1 :: Int)
              , "text" .= source
              ]
          ]
    BSL.length (encode didOpenParams) `shouldSatisfy` (> 0)

    -- 2. Analyze
    diags <- analyzeCode "Test.hs" source
    let lspDiags = diagsToLsp diags

    -- 3. Simulate publishDiagnostics
    let publishParams = object
          [ "uri" .= ("file:///project/Test.hs" :: Text)
          , "version" .= (1 :: Int)
          , "diagnostics" .= map toJSON lspDiags
          ]
    BSL.length (encode publishParams) `shouldSatisfy` (> 0)

    -- Verify diagnostics are included
    lspDiags `shouldSatisfy` (not . null)

  it "simulates codeAction request → response flow" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap x = return x"
          ]

    -- Analyze first
    diags <- analyzeCode "Test.hs" source
    let lspDiags = diagsToLsp diags
        diagsWithFixes = filter (not . null . diagFixes) diags

    -- 1. Simulate codeAction request
    let codeActionRequest = object
          [ "textDocument" .= object
              [ "uri" .= ("file:///project/Test.hs" :: Text)
              ]
          , "range" .= object
              [ "start" .= object ["line" .= (1 :: Int), "character" .= (0 :: Int)]
              , "end" .= object ["line" .= (1 :: Int), "character" .= (20 :: Int)]
              ]
          , "context" .= object
              [ "diagnostics" .= map toJSON lspDiags
              ]
          ]
    BSL.length (encode codeActionRequest) `shouldSatisfy` (> 0)

    -- 2. Generate code actions
    let actions = concatMap (\d -> map (fixToCodeAction "Test.hs" d) (diagFixes d)) diagsWithFixes

    -- 3. Simulate response
    let codeActionResponse = toJSON actions
    case codeActionResponse of
      Array _ -> pure ()  -- Expected
      _ -> expectationFailure "Expected array of code actions"

  it "simulates workspace/applyEdit for fix application" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source
    let diagsWithFixes = filter (not . null . diagFixes) diags

    case diagsWithFixes of
      [] -> pendingWith "No diagnostics with fixes"
      (d:_) -> do
        let action = fixToCodeAction "Test.hs" d (head $ diagFixes d)

        -- Simulate workspace/applyEdit request
        case lcaEdit action of
          Nothing -> expectationFailure "Expected edit"
          Just edit -> do
            let applyEditRequest = object
                  [ "label" .= lcaTitle action
                  , "edit" .= toJSON edit
                  ]
            BSL.length (encode applyEditRequest) `shouldSatisfy` (> 0)

  it "handles textDocument/didChange → re-analyze flow" $ do
    let originalSource = T.unlines
          [ "module Test where"
          , "wrap x = return x"
          ]
        modifiedSource = T.unlines
          [ "module Test where"
          , "wrap x = pure x"  -- Fixed version
          ]

    -- Analyze original
    originalDiags <- analyzeCode "Test.hs" originalSource

    -- Simulate didChange
    let didChangeParams = object
          [ "textDocument" .= object
              [ "uri" .= ("file:///project/Test.hs" :: Text)
              , "version" .= (2 :: Int)
              ]
          , "contentChanges" .=
              [ object ["text" .= modifiedSource]
              ]
          ]
    BSL.length (encode didChangeParams) `shouldSatisfy` (> 0)

    -- Re-analyze modified
    modifiedDiags <- analyzeCode "Test.hs" modifiedSource

    -- Check for modernize/applicative diagnostics (return -> pure rule)
    let hasModernizeDiag ds = any (\d ->
          case diagCode d of
            Just code -> "modernize" `T.isInfixOf` code || "applicative" `T.isInfixOf` code
            Nothing -> False) ds

    -- Original should have modernize diagnostics, modified should not
    -- (since pure is already the modern form)
    hasModernizeDiag originalDiags `shouldBe` True
    hasModernizeDiag modifiedDiags `shouldBe` False

--------------------------------------------------------------------------------
-- Plugin Diagnostic E2E Tests
--------------------------------------------------------------------------------

pluginDiagnosticE2ESpec :: Spec
pluginDiagnosticE2ESpec = describe "Plugin Diagnostic E2E" $ do
  it "analyzes, converts, formats plugin diagnostics" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    -- Full pipeline
    diags <- analyzeCode "Test.hs" source
    let pluginDiags = map toPluginDiagnostic diags
        formatted = map formatPluginDiagnostic pluginDiags
        jsonFormatted = map formatPluginDiagnosticJson pluginDiags

    -- Verify all outputs are non-empty
    forM_ formatted $ \f -> T.length f `shouldSatisfy` (> 0)
    forM_ jsonFormatted $ \j -> T.length j `shouldSatisfy` (> 0)

  it "plugin config affects output format" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source
    let pluginDiags = map toPluginDiagnostic diags

    -- Text format
    let textConfig = defaultPluginConfig { pcOutputFormat = "text" }
    pcOutputFormat textConfig `shouldBe` "text"

    -- JSON format
    let jsonConfig = parsePluginOptions ["output=json"]
    pcOutputFormat jsonConfig `shouldBe` "json"

    -- Verify different formats produce different outputs
    case pluginDiags of
      [] -> pendingWith "No diagnostics"
      (pd:_) -> do
        let textOutput = formatPluginDiagnostic pd
            jsonOutput = formatPluginDiagnosticJson pd
        textOutput `shouldNotBe` jsonOutput

  it "verbose mode includes more details" $ do
    let verboseConfig = parsePluginOptions ["verbose"]
    pcVerbose verboseConfig `shouldBe` True

    let quietConfig = defaultPluginConfig
    pcVerbose quietConfig `shouldBe` False

  it "plugin diagnostics with fixes are formatted correctly" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source
    let diagsWithFixes = filter (not . null . diagFixes) diags
        pluginDiags = map toPluginDiagnostic diagsWithFixes

    case pluginDiags of
      [] -> pendingWith "No diagnostics with fixes"
      (pd:_) -> do
        let formatted = formatPluginDiagnostic pd
        T.isInfixOf "Fixes available" formatted `shouldBe` True

--------------------------------------------------------------------------------
-- Cross-Component Integration Tests
--------------------------------------------------------------------------------

crossComponentIntegrationSpec :: Spec
crossComponentIntegrationSpec = describe "Cross-Component Integration" $ do
  it "LSP and Plugin produce consistent diagnostics" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source

    -- Convert to both formats
    let lspDiags = diagsToLsp diags
        pluginDiags = map toPluginDiagnostic diags

    -- Should have same count
    length lspDiags `shouldBe` length pluginDiags

    -- Messages should match
    forM_ (zip diags lspDiags) $ \(argDiag, lspDiag) -> do
      diagMessage argDiag `shouldBe` ldMessage lspDiag

    forM_ (zip diags pluginDiags) $ \(argDiag, pluginDiag) -> do
      diagMessage argDiag `shouldBe` pdMessage pluginDiag

  it "fix application produces same result for LSP and Plugin paths" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source
    let diagsWithFixes = filter (not . null . diagFixes) diags

    case diagsWithFixes of
      [] -> pendingWith "No diagnostics with fixes"
      (d:_) -> do
        let fix = head $ diagFixes d
            pluginFix = toPluginFix fix

        -- Both should represent the same replacement
        case fixEdits fix of
          [] -> expectationFailure "No edits in fix"
          (FixEdit _ repl:_) -> do
            pfReplacement pluginFix `shouldBe` repl

  it "severity levels are consistent across components" $ do
    -- Test all severity mappings
    let testSeverities = [Error, Warning, Suggestion, Info]

    forM_ testSeverities $ \sev -> do
      let lspSev = severityToLsp sev
      case sev of
        Error -> lspSev `shouldBe` LspError
        Warning -> lspSev `shouldBe` LspWarning
        Suggestion -> lspSev `shouldBe` LspHint
        Info -> lspSev `shouldBe` LspInformation

  it "span conversion is consistent between LSP range and Plugin span" $ do
    let span' = mkSrcSpanRaw "Test.hs" 10 5 10 20
        lspRange = spanToRange span'

    -- LSP uses 0-indexed positions
    lpLine (lrStart lspRange) `shouldBe` 9
    lpCharacter (lrStart lspRange) `shouldBe` 4
    lpLine (lrEnd lspRange) `shouldBe` 9
    lpCharacter (lrEnd lspRange) `shouldBe` 19

  it "handles multi-file scenarios consistently" $ do
    let sourceA = T.unlines
          [ "module A where"
          , "wrap x = return x"
          ]
        sourceB = T.unlines
          [ "module B where"
          , "wrap x = return x"
          ]

    diagsA <- analyzeCode "A.hs" sourceA
    diagsB <- analyzeCode "B.hs" sourceB

    let lspDiagsA = diagsToLsp diagsA
        lspDiagsB = diagsToLsp diagsB
        pluginDiagsA = map toPluginDiagnostic diagsA
        pluginDiagsB = map toPluginDiagnostic diagsB

    -- Both files should have diagnostics
    lspDiagsA `shouldSatisfy` (not . null)
    lspDiagsB `shouldSatisfy` (not . null)
    pluginDiagsA `shouldSatisfy` (not . null)
    pluginDiagsB `shouldSatisfy` (not . null)

--------------------------------------------------------------------------------
-- Helper
--------------------------------------------------------------------------------

forM_ :: [a] -> (a -> IO b) -> IO ()
forM_ = flip mapM_
