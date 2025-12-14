{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Spec
-- Description : Test specifications for the Haskell linter
-- Copyright   : (c) 2024
-- License     : MIT
module Spec where

import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Argus.Config
import Argus.Core
import Argus.Types
import Argus.Rules.Naming
import Argus.Rules.ConfigurableRules (defaultRulesConfig)
import Argus.Analysis.Syntactic

--------------------------------------------------------------------------------
-- Main Test Spec
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Argus.Config" configSpec
  describe "Argus.Analysis.Syntactic" syntacticSpec
  describe "Argus.Rules.Naming" namingSpec
  describe "Argus.Core" coreSpec
  describe "Integration Tests" integrationSpec

--------------------------------------------------------------------------------
-- Config Tests
--------------------------------------------------------------------------------

configSpec :: Spec
configSpec = do
  describe "loadConfig" $ do
    it "loads default config when no file exists" $ do
      cfg <- loadConfig Nothing
      cfgNaming cfg `shouldSatisfy` namingEnabled

  describe "loadLegacyConfig" $ do
    it "loads legacy YAML format" $ do
      legacy <- loadLegacyConfig "test/config.yaml"
      legacySignatures legacy `shouldSatisfy` (not . null)

--------------------------------------------------------------------------------
-- Syntactic Analysis Tests
--------------------------------------------------------------------------------

syntacticSpec :: Spec
syntacticSpec = do
  describe "parseFile" $ do
    it "parses valid Haskell file" $ do
      result <- parseFile "test/data/Test1.hs"
      case result of
        Left err -> expectationFailure $ "Parse failed: " <> show (peMessage err)
        Right _  -> pure ()

    it "returns error for invalid file" $ do
      result <- parseFile "nonexistent.hs"
      case result of
        Left _  -> pure ()
        Right _ -> expectationFailure "Should have failed"

  describe "extractFunctions" $ do
    it "extracts function information from parsed module" $ do
      result <- parseFile "test/data/Test1.hs"
      case result of
        Left err -> expectationFailure $ "Parse failed: " <> show (peMessage err)
        Right pr -> do
          let fns = extractFunctions "test/data/Test1.hs" (prSource pr) (prModule pr)
          fns `shouldSatisfy` (not . null)

--------------------------------------------------------------------------------
-- Naming Rules Tests
--------------------------------------------------------------------------------

namingSpec :: Spec
namingSpec = do
  describe "matchWildcardPattern" $ do
    it "matches exact text" $ do
      matchWildcardPattern "LocationId" "LocationId" `shouldBe` True

    it "matches wildcard *" $ do
      matchWildcardPattern "*Id" "LocationId" `shouldBe` True
      matchWildcardPattern "*Id" "UserId" `shouldBe` True

    it "does not match different text" $ do
      matchWildcardPattern "LocationId" "UserId" `shouldBe` False

    it "matches multi-word patterns" $ do
      matchWildcardPattern "Entity *" "Entity Page" `shouldBe` True
      matchWildcardPattern "Key *" "Key Location" `shouldBe` True

  describe "matchVariableRule" $ do
    it "matches variable by type" $ do
      let rule = VariableRule
            { vrType = "Key Location"
            , vrFrom = Just "*"
            , vrTo = "locationK"
            , vrSeverity = RSWarning
            , vrMessage = Nothing
            }
          arg = ArgumentInfo
            { aiName = "loc"
            , aiType = Just "Key Location"
            , aiSpan = noSrcSpan
            , aiTypeSpan = Nothing
            }
      matchVariableRule rule arg `shouldBe` True

    it "does not match when name is already correct" $ do
      let rule = VariableRule
            { vrType = "Key Location"
            , vrFrom = Just "*"
            , vrTo = "locationK"
            , vrSeverity = RSWarning
            , vrMessage = Nothing
            }
          arg = ArgumentInfo
            { aiName = "locationK"
            , aiType = Just "Key Location"
            , aiSpan = noSrcSpan
            , aiTypeSpan = Nothing
            }
      matchVariableRule rule arg `shouldBe` False

--------------------------------------------------------------------------------
-- Core Tests
--------------------------------------------------------------------------------

coreSpec :: Spec
coreSpec = do
  describe "findHaskellFiles" $ do
    it "finds .hs files in directory" $ do
      files <- findHaskellFiles ["test/data"]
      files `shouldSatisfy` (not . null)
      all (\f -> T.isSuffixOf ".hs" (T.pack f)) files `shouldBe` True

  describe "analyzeFile" $ do
    it "analyzes a valid Haskell file" $ do
      cfg <- loadConfig (Just "test/config.yaml")
      let ctx = AnalysisContext cfg defaultOptions [] Nothing defaultRulesConfig Nothing
      result <- analyzeFile ctx "test/data/Test1.hs"
      fileResultPath result `shouldBe` "test/data/Test1.hs"

--------------------------------------------------------------------------------
-- Integration Tests
--------------------------------------------------------------------------------

integrationSpec :: Spec
integrationSpec = do
  describe "Full linting workflow" $ do
    testFiles <- runIO $ listDirectory "test/data"

    forM_ (filter (T.isSuffixOf ".hs" . T.pack) testFiles) $ \testFile -> do
      let inputFile = "test/data" </> testFile
          resultFile = "test/data-result" </> testFile

      it ("correctly processes " ++ testFile) $ do
        cfg <- loadLegacyConfig "test/config.yaml"
        let config = convertLegacyConfig cfg
            ctx = AnalysisContext config defaultOptions [] Nothing defaultRulesConfig Nothing

        result <- analyzeFile ctx inputFile

        -- Check that analysis completes without error
        fileResultPath result `shouldBe` inputFile

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Find Haskell files in test directories (simple, non-recursive)
findTestHsFiles :: [FilePath] -> IO [FilePath]
findTestHsFiles paths = concat <$> mapM findInPath paths
  where
    findInPath path = do
      contents <- listDirectory path
      let hsFiles = filter (\f -> T.isSuffixOf ".hs" (T.pack f)) contents
      pure $ map (path </>) hsFiles
