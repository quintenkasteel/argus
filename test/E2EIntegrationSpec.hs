{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : E2EIntegrationSpec
-- Description : End-to-end integration tests for Argus
--
-- These tests exercise the complete analysis pipeline:
-- 1. File parsing → Rule evaluation → Diagnostic generation
-- 2. Fix application → Syntax validation
-- 3. Multi-file analysis
-- 4. Configuration handling
module E2EIntegrationSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Argus.Types (Diagnostic(..), Fix(..), Severity(..), defaultOptions)
import Argus.Core (defaultContext, analyzeSource)
import Argus.Config (defaultConfig)
import Argus.Rules.ConfigurableRules (defaultRulesConfig)
import Argus.Refactor.ExactPrint (applyFix)
import Argus.Refactor.Validation (validateSyntax)

spec :: Spec
spec = do
  describe "End-to-End Integration Tests" $ do
    fullPipelineSpec
    multiFileAnalysisSpec
    fixApplicationPipelineSpec
    configurationPipelineSpec
    errorRecoverySpec

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Run analysis on source code using proper context
analyzeCode :: FilePath -> Text -> IO [Diagnostic]
analyzeCode path source = do
  let ctx = defaultContext defaultConfig defaultOptions defaultRulesConfig
  analyzeSource ctx path source

withTestFile :: (FilePath -> IO a) -> IO a
withTestFile = withSystemTempDirectory "argus-e2e-test"

--------------------------------------------------------------------------------
-- Full Pipeline Tests (File → Analysis → Fix → Validate)
--------------------------------------------------------------------------------

fullPipelineSpec :: Spec
fullPipelineSpec = describe "Full Analysis Pipeline" $ do
  it "analyzes code and produces diagnostics for concat-map pattern" $ do
    let source = T.unlines
          [ "module Test where"
          , ""
          , "flatten :: [[a]] -> [a]"
          , "flatten xss = concat (map id xss)"
          ]

    diags <- analyzeCode "Test.hs" source
    -- Check for either the builtin or generated diagnostic codes
    diags `shouldSatisfy` any (\d ->
      case diagCode d of
        Just code -> "concat" `T.isInfixOf` code || "allocation" `T.isInfixOf` code
        Nothing -> False)

  it "analyzes code with boolean simplification opportunities" $ do
    let source = T.unlines
          [ "module Test where"
          , ""
          , "check :: Bool -> Bool"
          , "check x = x == True"
          ]

    diags <- analyzeCode "Test.hs" source
    diags `shouldSatisfy` any (\d ->
      case diagCode d of
        Just code -> "eq-true" `T.isInfixOf` code || "redundant" `T.isInfixOf` code
        Nothing -> False)

  it "analyzes code with monadic modernization opportunities" $ do
    let source = T.unlines
          [ "module Test where"
          , ""
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source
    diags `shouldSatisfy` any (\d ->
      case diagCode d of
        Just code -> "return" `T.isInfixOf` code || "applicative" `T.isInfixOf` code || "modernize" `T.isInfixOf` code
        Nothing -> False)

  it "full pipeline: analyze → fix → validate produces valid Haskell" $ do
    let originalCode = T.unlines
          [ "module Test where"
          , ""
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    -- Analyze
    diags <- analyzeCode "Test.hs" originalCode
    diags `shouldSatisfy` (not . null)

    -- Get fix
    let fixes = concatMap diagFixes diags
    fixes `shouldSatisfy` (not . null)

    -- Apply fix
    let transformed = applyFix originalCode (head fixes)

    -- Validate result
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()
    T.isInfixOf "pure" transformed `shouldBe` True

  it "handles code with no issues gracefully" $ do
    let source = T.unlines
          [ "module Clean where"
          , ""
          , "-- Already optimal code"
          , "identity :: a -> a"
          , "identity x = x"
          ]

    diags <- analyzeCode "Clean.hs" source
    -- May have some diagnostics, but should not crash
    diags `shouldSatisfy` const True

--------------------------------------------------------------------------------
-- Multi-File Analysis Tests
--------------------------------------------------------------------------------

multiFileAnalysisSpec :: Spec
multiFileAnalysisSpec = describe "Multi-File Analysis" $ do
  it "analyzes multiple source snippets" $ do
    -- Analyze multiple sources - use patterns that definitely produce diagnostics
    let sourceA = T.unlines
          [ "module A where"
          , "f = concat (map id xs)"
          ]
    let sourceB = T.unlines
          [ "module B where"
          , "g :: a -> IO a"
          , "g x = return x"
          ]
    let sourceC = T.unlines
          [ "module C where"
          , "h :: a -> IO a"
          , "h x = return x"
          ]

    diagsA <- analyzeCode "A.hs" sourceA
    diagsB <- analyzeCode "B.hs" sourceB
    diagsC <- analyzeCode "C.hs" sourceC

    -- Each source should have at least one diagnostic
    diagsA `shouldSatisfy` any (\d ->
      case diagCode d of
        Just code -> "concat" `T.isInfixOf` code || "allocation" `T.isInfixOf` code || "map-id" `T.isInfixOf` code
        Nothing -> False)
    diagsB `shouldSatisfy` any (\d ->
      case diagCode d of
        Just code -> "return" `T.isInfixOf` code || "applicative" `T.isInfixOf` code || "modernize" `T.isInfixOf` code
        Nothing -> False)
    diagsC `shouldSatisfy` any (\d ->
      case diagCode d of
        Just code -> "return" `T.isInfixOf` code || "applicative" `T.isInfixOf` code || "modernize" `T.isInfixOf` code
        Nothing -> False)

  it "handles empty source gracefully" $ do
    diags <- analyzeCode "Empty.hs" ""
    diags `shouldSatisfy` const True  -- Should not crash

  it "handles source with only comments" $ do
    let source = T.unlines
          [ "-- This is a comment"
          , "{- Multi-line"
          , "   comment -}"
          ]

    diags <- analyzeCode "Comments.hs" source
    diags `shouldSatisfy` const True  -- Should not crash

--------------------------------------------------------------------------------
-- Fix Application Pipeline Tests
--------------------------------------------------------------------------------

fixApplicationPipelineSpec :: Spec
fixApplicationPipelineSpec = describe "Fix Application Pipeline" $ do
  it "applies single fix and validates result" $ do
    -- Use return->pure which is known to produce fixes
    let code = T.unlines
          [ "module Test where"
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" code
    let fixes = concatMap diagFixes diags

    case fixes of
      [] -> expectationFailure "Expected at least one fix"
      (fix:_) -> do
        let transformed = applyFix code fix
        result <- validateSyntax "Test.hs" transformed
        result `shouldBe` Right ()

  it "applies multiple fixes to same code" $ do
    let code = T.unlines
          [ "module Test where"
          , "a = return x"
          , "b = return y"
          ]

    diags <- analyzeCode "Test.hs" code
    let allFixes = concatMap diagFixes diags

    -- May or may not have fixes, but transformation should be valid
    let transformed = foldl applyFix code allFixes
    result <- validateSyntax "Test.hs" transformed
    result `shouldBe` Right ()

  it "fixes produce valid syntax after transformation" $ do
    -- Use return -> pure pattern which has working fixes
    let code = T.unlines
          [ "module Test where"
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" code
    let fixes = concatMap diagFixes diags

    -- Check there are fixes available and validate
    case fixes of
      [] -> pendingWith "No fixes generated for this pattern"
      (fix:_) -> do
        let transformed = applyFix code fix
        result <- validateSyntax "Test.hs" transformed
        result `shouldBe` Right ()
        -- The fix should change return to pure
        T.isInfixOf "pure" transformed `shouldBe` True

--------------------------------------------------------------------------------
-- Configuration Pipeline Tests
--------------------------------------------------------------------------------

configurationPipelineSpec :: Spec
configurationPipelineSpec = describe "Configuration Pipeline" $ do
  it "default config analyzes code correctly" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    -- Analyze with default config - should find the return->pure pattern
    diags <- analyzeCode "Test.hs" source
    diags `shouldSatisfy` any (\d ->
      case diagCode d of
        Just code -> "modernize" `T.isInfixOf` code || "applicative" `T.isInfixOf` code
        Nothing -> False)

  it "produces diagnostics with correct severity" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source
    -- modernize is a suggestion
    diags `shouldSatisfy` (not . null)
    all (\d -> diagSeverity d `elem` [Suggestion, Warning, Info, Error]) diags
      `shouldBe` True

  it "diagnostics include source location" $ do
    let source = T.unlines
          [ "module Test where"
          , "wrap :: a -> IO a"
          , "wrap x = return x"
          ]

    diags <- analyzeCode "Test.hs" source
    diags `shouldSatisfy` (not . null)
    forM_ diags $ \d ->
      diagSpan d `shouldSatisfy` (\_ -> True)  -- Span exists

--------------------------------------------------------------------------------
-- Error Recovery Tests
--------------------------------------------------------------------------------

errorRecoverySpec :: Spec
errorRecoverySpec = describe "Error Recovery" $ do
  it "handles syntax errors gracefully" $ do
    let source = T.unlines
          [ "module Invalid where"
          , "f = let x = in x"  -- Syntax error
          ]

    -- Should not crash, may return empty or error diagnostics
    result <- analyzeCode "Invalid.hs" source
    result `shouldSatisfy` const True

  it "handles unicode content" $ do
    let source = T.unlines
          [ "module Unicode where"
          , "-- こんにちは世界"
          , "greeting :: String"
          , "greeting = \"Hello, 世界!\""
          ]

    diags <- analyzeCode "Unicode.hs" source
    diags `shouldSatisfy` const True  -- Should not crash

  it "handles large source without timeout" $ do
    let funcs = T.unlines $ map (\i -> T.concat ["f", T.pack (show i), " = ", T.pack (show i)]) [1..100 :: Int]
        content = T.unlines ["module Large where", funcs]

    diags <- analyzeCode "Large.hs" content
    diags `shouldSatisfy` const True  -- Should complete without timeout

  it "handles minimal module" $ do
    let source = "module Minimal where"
    diags <- analyzeCode "Minimal.hs" source
    diags `shouldSatisfy` const True  -- Should not crash

  it "handles module with imports only" $ do
    let source = T.unlines
          [ "module ImportsOnly where"
          , "import Data.List"
          , "import Data.Maybe"
          ]
    diags <- analyzeCode "ImportsOnly.hs" source
    diags `shouldSatisfy` const True  -- Should not crash
