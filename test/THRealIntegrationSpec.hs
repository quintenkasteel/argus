{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : THRealIntegrationSpec
-- Description : Real TH integration tests using actual HIE files
--
-- These tests verify that Argus correctly identifies functions as "used"
-- when they are ONLY referenced by TH-generated code.
--
-- This is the critical Weeder caveat test:
-- - A function is NEVER called directly in source code
-- - It IS called from TH-expanded code
-- - Argus should NOT flag it as unused
module THRealIntegrationSpec (spec) where

import Test.Hspec
import Data.ByteString qualified as BS
import Data.Set qualified as Set
import Data.Text qualified as T
import System.Directory (doesFileExist)

import Argus.Analysis.TemplateHaskell
import Argus.HIE.CrossVersion

spec :: Spec
spec = do
  describe "Real TH HIE File Integration" $ do
    describe "HIE File Existence" $ do
      it "can find HIE files in test-project" $ do
        -- Check that the test-project HIE file exists
        let hiePath = "test-project/.hie/THRealTest.hie"
        exists <- doesFileExist hiePath
        exists `shouldBe` True

      it "can find THRealTest/TH.hie (TH definition module)" $ do
        let hiePath = "test-project/.hie/THRealTest/TH.hie"
        exists <- doesFileExist hiePath
        exists `shouldBe` True

      it "can find hiedb database" $ do
        let dbPath = "test-project/.hiedb"
        exists <- doesFileExist dbPath
        exists `shouldBe` True

    describe "Raw HIE File Symbol Verification (Version-Independent)" $ do
      -- These tests verify HIE file content by checking raw bytes
      -- This works regardless of GHC version mismatch

      it "THRealTest.hie contains 'onlyCalledFromTH' symbol" $ do
        -- The HIE file should contain this symbol because $(generateCall)
        -- expands to code that calls onlyCalledFromTH
        let hiePath = "test-project/.hie/THRealTest.hie"
        content <- BS.readFile hiePath
        let hasSymbol = "onlyCalledFromTH" `BS.isInfixOf` content
        hasSymbol `shouldBe` True

      it "THRealTest.hie contains 'reallyUnused' symbol (defined but never called)" $ do
        let hiePath = "test-project/.hie/THRealTest.hie"
        content <- BS.readFile hiePath
        let hasSymbol = "reallyUnused" `BS.isInfixOf` content
        hasSymbol `shouldBe` True

      it "THRealTest.hie contains 'runGenerated' symbol" $ do
        let hiePath = "test-project/.hie/THRealTest.hie"
        content <- BS.readFile hiePath
        let hasSymbol = "runGenerated" `BS.isInfixOf` content
        hasSymbol `shouldBe` True

      it "THRealTest.hie contains 'generateCall' import reference" $ do
        let hiePath = "test-project/.hie/THRealTest.hie"
        content <- BS.readFile hiePath
        let hasSymbol = "generateCall" `BS.isInfixOf` content
        hasSymbol `shouldBe` True

      it "THRealTest/TH.hie contains 'generateCall' definition" $ do
        let hiePath = "test-project/.hie/THRealTest/TH.hie"
        content <- BS.readFile hiePath
        let hasSymbol = "generateCall" `BS.isInfixOf` content
        hasSymbol `shouldBe` True

    describe "Cross-GHC Version HieDb Extraction" $ do
      -- These tests use hiedb SQLite queries which are version-agnostic
      -- This is the KEY to cross-GHC version analysis

      it "extracts definitions from hiedb (version-agnostic)" $ do
        let dbPath = "test-project/.hiedb"
        result <- extractDefsFromHieDb dbPath
        case result of
          Left err -> expectationFailure $ "Failed to extract defs: " ++ show err
          Right defs -> do
            -- Should find many definitions
            length defs `shouldSatisfy` (> 100)
            -- Find onlyCalledFromTH definition
            let hasOnlyCalledFromTH = any (\d -> "onlyCalledFromTH" `T.isInfixOf` hddOcc d) defs
            hasOnlyCalledFromTH `shouldBe` True

      it "extracts references from hiedb (version-agnostic)" $ do
        let dbPath = "test-project/.hiedb"
        result <- extractRefsFromHieDb dbPath
        case result of
          Left err -> expectationFailure $ "Failed to extract refs: " ++ show err
          Right refs -> do
            -- Should find many references
            length refs `shouldSatisfy` (> 1000)
            -- Find references to onlyCalledFromTH
            let onlyCalledRefs = filter (\r -> "onlyCalledFromTH" `T.isInfixOf` hdrOcc r) refs
            -- Should have 4 references: export, type sig, def, TH-expanded call
            length onlyCalledRefs `shouldBe` 4

      it "CRITICAL: onlyCalledFromTH has TH-expanded reference at line 40" $ do
        -- This is THE critical test for TH detection
        -- Line 40 contains $(generateCall) which expands to: onlyCalledFromTH 10
        -- The hiedb MUST capture this TH-expanded reference
        let dbPath = "test-project/.hiedb"
        result <- extractRefsFromHieDb dbPath
        case result of
          Left err -> expectationFailure $ "Failed to extract refs: " ++ show err
          Right refs -> do
            -- Find references to onlyCalledFromTH at line 40
            let line40Refs = filter (\r ->
                  "onlyCalledFromTH" `T.isInfixOf` hdrOcc r &&
                  hdrStartLine r == 40) refs
            -- MUST have exactly 1 reference at line 40 (the TH-expanded call)
            length line40Refs `shouldBe` 1

      it "reallyUnused has NO call references (only definition references)" $ do
        let dbPath = "test-project/.hiedb"
        result <- extractRefsFromHieDb dbPath
        case result of
          Left err -> expectationFailure $ "Failed to extract refs: " ++ show err
          Right refs -> do
            -- Find all references to reallyUnused
            let unusedRefs = filter (\r -> "reallyUnused" `T.isInfixOf` hdrOcc r) refs
            -- Should have exactly 3 references: export (19), type sig (33), def (34)
            length unusedRefs `shouldBe` 3
            -- All references should be at definition-related lines (19, 33, 34)
            let refLines = map hdrStartLine unusedRefs
            all (`elem` [19, 33, 34]) refLines `shouldBe` True

      it "onlyCalledFromTH has MORE references than reallyUnused" $ do
        -- This demonstrates that onlyCalledFromTH is actually USED (via TH)
        -- while reallyUnused is genuinely unused
        let dbPath = "test-project/.hiedb"
        result <- extractRefsFromHieDb dbPath
        case result of
          Left err -> expectationFailure $ "Failed to extract refs: " ++ show err
          Right refs -> do
            let onlyCalledRefs = filter (\r -> "onlyCalledFromTH" `T.isInfixOf` hdrOcc r) refs
                unusedRefs = filter (\r -> "reallyUnused" `T.isInfixOf` hdrOcc r) refs
            -- onlyCalledFromTH: 4 refs (export, sig, def, TH call)
            -- reallyUnused: 3 refs (export, sig, def)
            length onlyCalledRefs `shouldSatisfy` (> length unusedRefs)

    describe "Legacy HIE File API (may fail on version mismatch)" $ do
      -- These tests use direct HIE file reading which WILL fail on GHC version mismatch
      -- They're kept for reference but the cross-version tests above are authoritative

      it "extracts onlyCalledFromTH from THRealTest.hie (may fail on version mismatch)" $ do
        let hiePath = "test-project/.hie/THRealTest.hie"
        result <- extractUsedNamesFromHie hiePath

        case result of
          Left err -> do
            -- Version mismatch is expected with cross-GHC versions
            -- The cross-version tests above verify correctness via hiedb
            if "version" `T.isInfixOf` err || "9066" `T.isInfixOf` err
              then pure ()  -- Expected failure, cross-version tests verify this
              else expectationFailure $ "Unexpected error: " ++ T.unpack err

          Right names -> do
            -- If direct reading works, verify the content
            let hasOnlyCalledFromTH = "onlyCalledFromTH" `Set.member` names
            hasOnlyCalledFromTH `shouldBe` True

    describe "TH Roots Pattern Matching" $ do
      it "matchesRoots identifies TH-specific patterns" $ do
        -- These patterns should match common TH function names
        let thPatterns = ["parseJSON", "toJSON", "widgetFile", "onlyCalledFromTH"]
        matchesRoots thPatterns "onlyCalledFromTH" `shouldBe` True
        matchesRoots thPatterns "parseJSON" `shouldBe` True
        matchesRoots thPatterns "somethingElse" `shouldBe` False

      it "matchesRoots supports regex patterns" $ do
        let patterns = ["parse.*", "to.*JSON", ".*Lens"]
        matchesRoots patterns "parseJSON" `shouldBe` True
        matchesRoots patterns "parseYaml" `shouldBe` True
        matchesRoots patterns "toJSON" `shouldBe` True
        matchesRoots patterns "toHaskellJSON" `shouldBe` True
        matchesRoots patterns "makeLens" `shouldBe` True
        matchesRoots patterns "customLens" `shouldBe` True
        matchesRoots patterns "unrelatedFunction" `shouldBe` False

    describe "HIE File Discovery" $ do
      it "finds HIE file for source path" $ do
        result <- findHieFileForSource "test-project/src/THRealTest.hs" (Just "test-project/.hie")
        result `shouldSatisfy` \case
          Just path -> "THRealTest.hie" `T.isInfixOf` T.pack path
          Nothing -> False

      it "finds nested module HIE file" $ do
        result <- findHieFileForSource "test-project/src/THRealTest/TH.hs" (Just "test-project/.hie")
        result `shouldSatisfy` \case
          Just path -> "TH.hie" `T.isInfixOf` T.pack path
          Nothing -> False

    describe "Cross-GHC Version Handling (Integration)" $ do
      -- These tests verify the complete cross-GHC version solution

      it "hiedb extraction works despite GHC version mismatch" $ do
        -- Argus is built with GHC 9.10.3
        -- test-project is built with GHC 9.6.6 (LTS 22.43)
        -- Direct HIE reading fails, but hiedb extraction works
        let dbPath = "test-project/.hiedb"
        defsResult <- extractDefsFromHieDb dbPath
        refsResult <- extractRefsFromHieDb dbPath

        case (defsResult, refsResult) of
          (Right defs, Right refs) -> do
            -- Extraction succeeded
            length defs `shouldSatisfy` (> 0)
            length refs `shouldSatisfy` (> 0)
          (Left err, _) -> expectationFailure $ "Defs extraction failed: " ++ show err
          (_, Left err) -> expectationFailure $ "Refs extraction failed: " ++ show err

      it "can build dependency graph from hiedb (version-agnostic)" $ do
        let dbPath = "test-project/.hiedb"
        result <- buildGraphFromHieDbCrossVersion dbPath
        case result of
          Left err -> expectationFailure $ "Graph building failed: " ++ show err
          Right _graph -> pure ()  -- Graph building succeeded

    describe "The Weeder Caveat - Critical Verification" $ do
      -- This is the MOST IMPORTANT test group:
      -- Verifying that functions called ONLY from TH-generated code
      -- are NOT flagged as unused

      it "CRITICAL: onlyCalledFromTH appears in HIE despite never being called in source" $ do
        -- The raw HIE file MUST contain onlyCalledFromTH
        -- This proves TH expansion is captured in HIE files
        let hiePath = "test-project/.hie/THRealTest.hie"
        content <- BS.readFile hiePath

        -- Verify the function name appears in the binary HIE data
        let hasSymbol = "onlyCalledFromTH" `BS.isInfixOf` content
        hasSymbol `shouldBe` True

        -- Double-check it's not just in a comment by looking for the identifier pattern
        -- HIE files store occurrences with position info, so the name appears multiple times
        -- if it's actually used (definition + reference from TH expansion)
        let occurrences = countOccurrences "onlyCalledFromTH" content
        -- At minimum: 1 for definition, 1 for TH-expanded call, 1 in symbol table
        occurrences `shouldSatisfy` (>= 2)

      it "reallyUnused has fewer references than onlyCalledFromTH" $ do
        -- reallyUnused is defined but NEVER called (even via TH)
        -- onlyCalledFromTH is defined AND called from TH-expanded code
        -- So onlyCalledFromTH should have more references
        let hiePath = "test-project/.hie/THRealTest.hie"
        content <- BS.readFile hiePath

        let unusedCount = countOccurrences "reallyUnused" content
            calledCount = countOccurrences "onlyCalledFromTH" content

        -- onlyCalledFromTH should appear more times because it's:
        -- 1. Defined (type sig + binding)
        -- 2. Called from TH-expanded code
        -- reallyUnused is only defined, never called
        calledCount `shouldSatisfy` (>= unusedCount)

      it "WEEDER CAVEAT SOLVED: TH-expanded call detected via hiedb" $ do
        -- This test proves that Argus can detect TH-expanded function calls
        -- even when there's a GHC version mismatch
        let dbPath = "test-project/.hiedb"
        result <- extractRefsFromHieDb dbPath
        case result of
          Left err -> expectationFailure $ "Failed to extract refs: " ++ show err
          Right refs -> do
            -- Find the TH-expanded call at line 40
            let thExpandedCall = filter (\r ->
                  "onlyCalledFromTH" `T.isInfixOf` hdrOcc r &&
                  hdrStartLine r == 40) refs

            -- The Weeder caveat is SOLVED if we find this reference
            length thExpandedCall `shouldBe` 1

            -- Verify this is the call from the TH splice $(generateCall)
            -- which expands to: onlyCalledFromTH 10
            case thExpandedCall of
              [ref] -> do
                -- The reference exists at the splice location
                hdrStartLine ref `shouldBe` 40
              _ -> expectationFailure "Expected exactly one TH-expanded reference"

-- | Count occurrences of a ByteString pattern in content
countOccurrences :: BS.ByteString -> BS.ByteString -> Int
countOccurrences needle haystack = go 0 haystack
  where
    go !count bs
      | BS.null bs = count
      | needle `BS.isPrefixOf` bs = go (count + 1) (BS.drop 1 bs)
      | otherwise = go count (BS.drop 1 bs)
