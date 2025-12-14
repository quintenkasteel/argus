{-# LANGUAGE OverloadedStrings #-}

module CacheSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Argus.Analysis.Cache
import Argus.Types (Diagnostic(..), Severity(..), SrcSpan(..), DiagnosticKind(..), mkSrcSpanRaw)

spec :: Spec
spec = do
  describe "Argus.Analysis.Cache" $ do
    describe "emptyCache" $ do
      it "creates a cache with no entries" $ do
        cache <- emptyCache
        Map.null (acEntries cache) `shouldBe` True

      it "sets version to 1.0.0" $ do
        cache <- emptyCache
        acVersion cache `shouldBe` "1.0.0"

      it "initializes with zero files" $ do
        cache <- emptyCache
        acTotalFiles cache `shouldBe` 0

      it "initializes with zero hits and misses" $ do
        cache <- emptyCache
        acTotalHits cache `shouldBe` 0
        acTotalMisses cache `shouldBe` 0

    describe "hashContent" $ do
      it "produces consistent hashes for same content" $ do
        let content = "module Test where"
        hashContent (BS.pack $ map (fromIntegral . fromEnum) content)
          `shouldBe` hashContent (BS.pack $ map (fromIntegral . fromEnum) content)

      it "produces different hashes for different content" $ do
        let content1 = BS.pack $ map (fromIntegral . fromEnum) ("foo" :: String)
            content2 = BS.pack $ map (fromIntegral . fromEnum) ("bar" :: String)
        hashContent content1 `shouldNotBe` hashContent content2

      it "produces non-empty hash" $ do
        let content = BS.pack $ map (fromIntegral . fromEnum) ("test" :: String)
        T.null (hashContent content) `shouldBe` False

      it "handles empty content" $ do
        let content = BS.empty
        T.null (hashContent content) `shouldBe` False

    describe "hashFile" $ do
      it "hashes file contents successfully" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let testFile = tmpDir </> "test.hs"
          writeFile testFile "module Test where"
          result <- hashFile testFile
          case result of
            Left _ -> expectationFailure "Expected successful hash"
            Right hash -> T.null hash `shouldBe` False

      it "returns error for non-existent file" $ do
        result <- hashFile "/nonexistent/file.hs"
        case result of
          Left _ -> pure ()  -- Expected
          Right _ -> expectationFailure "Expected error for non-existent file"

      it "produces same hash for unchanged file" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let testFile = tmpDir </> "test.hs"
          writeFile testFile "module Test where"
          Right hash1 <- hashFile testFile
          Right hash2 <- hashFile testFile
          hash1 `shouldBe` hash2

      it "produces different hash after file change" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let testFile = tmpDir </> "test.hs"
          writeFile testFile "module Test where"
          Right hash1 <- hashFile testFile
          writeFile testFile "module Test where\n-- modified"
          Right hash2 <- hashFile testFile
          hash1 `shouldNotBe` hash2

    describe "saveCache and loadCache" $ do
      it "can save and load empty cache" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let cachePath = tmpDir </> "cache.json"
          cache <- emptyCache
          saveCache cachePath cache
          loaded <- loadCache cachePath
          case loaded of
            Nothing -> expectationFailure "Expected to load cache"
            Just c -> acVersion c `shouldBe` "1.0.0"

      it "creates parent directories" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let cachePath = tmpDir </> "deep" </> "nested" </> "cache.json"
          cache <- emptyCache
          saveCache cachePath cache
          exists <- doesFileExist cachePath
          exists `shouldBe` True

      it "returns Nothing for non-existent file" $ do
        loaded <- loadCache "/nonexistent/cache.json"
        loaded `shouldBe` Nothing

      it "preserves cache entries after save/load" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let cachePath = tmpDir </> "cache.json"
              testFile = tmpDir </> "test.hs"
          writeFile testFile "module Test where"
          cache <- emptyCache
          Right fileHash <- hashFile testFile
          let entry = CacheEntry
                { ceFilePath = testFile
                , ceFileHash = fileHash
                , ceModTime = acCreated cache
                , ceDiagnostics = []
                , ceAnalysisTime = 0.5
                , ceArgusVersion = "1.0.0"
                }
              cacheWithEntry = insertCache entry cache
          saveCache cachePath cacheWithEntry
          loaded <- loadCache cachePath
          case loaded of
            Nothing -> expectationFailure "Expected to load cache"
            Just c -> Map.member testFile (acEntries c) `shouldBe` True

    describe "lookupCache" $ do
      it "returns Nothing for missing file" $ do
        cache <- emptyCache
        lookupCache "missing.hs" "somehash" cache `shouldBe` Nothing

      it "returns entry when hash matches" $ do
        cache <- emptyCache
        let entry = CacheEntry
              { ceFilePath = "test.hs"
              , ceFileHash = "hash123"
              , ceModTime = acCreated cache
              , ceDiagnostics = []
              , ceAnalysisTime = 0.5
              , ceArgusVersion = "1.0.0"
              }
            cacheWithEntry = insertCache entry cache
        case lookupCache "test.hs" "hash123" cacheWithEntry of
          Nothing -> expectationFailure "Expected to find entry"
          Just e -> ceFilePath e `shouldBe` "test.hs"

      it "returns Nothing when hash differs" $ do
        cache <- emptyCache
        let entry = CacheEntry
              { ceFilePath = "test.hs"
              , ceFileHash = "hash123"
              , ceModTime = acCreated cache
              , ceDiagnostics = []
              , ceAnalysisTime = 0.5
              , ceArgusVersion = "1.0.0"
              }
            cacheWithEntry = insertCache entry cache
        lookupCache "test.hs" "differenthash" cacheWithEntry `shouldBe` Nothing

    describe "insertCache" $ do
      it "adds entry to cache" $ do
        cache <- emptyCache
        let entry = CacheEntry
              { ceFilePath = "test.hs"
              , ceFileHash = "hash123"
              , ceModTime = acCreated cache
              , ceDiagnostics = []
              , ceAnalysisTime = 0.5
              , ceArgusVersion = "1.0.0"
              }
            newCache = insertCache entry cache
        Map.member "test.hs" (acEntries newCache) `shouldBe` True

      it "updates total file count" $ do
        cache <- emptyCache
        let entry = CacheEntry
              { ceFilePath = "test.hs"
              , ceFileHash = "hash123"
              , ceModTime = acCreated cache
              , ceDiagnostics = []
              , ceAnalysisTime = 0.5
              , ceArgusVersion = "1.0.0"
              }
            newCache = insertCache entry cache
        acTotalFiles newCache `shouldBe` 1

      it "overwrites existing entry" $ do
        cache <- emptyCache
        let entry1 = CacheEntry
              { ceFilePath = "test.hs"
              , ceFileHash = "hash1"
              , ceModTime = acCreated cache
              , ceDiagnostics = []
              , ceAnalysisTime = 0.5
              , ceArgusVersion = "1.0.0"
              }
            entry2 = entry1 { ceFileHash = "hash2" }
            cache1 = insertCache entry1 cache
            cache2 = insertCache entry2 cache1
        case lookupCache "test.hs" "hash2" cache2 of
          Nothing -> expectationFailure "Expected entry with new hash"
          Just e -> ceFileHash e `shouldBe` "hash2"

    describe "invalidateCache" $ do
      it "clears all entries" $ do
        cache <- emptyCache
        let entry = CacheEntry
              { ceFilePath = "test.hs"
              , ceFileHash = "hash123"
              , ceModTime = acCreated cache
              , ceDiagnostics = []
              , ceAnalysisTime = 0.5
              , ceArgusVersion = "1.0.0"
              }
            cacheWithEntry = insertCache entry cache
            cleared = invalidateCache cacheWithEntry
        Map.null (acEntries cleared) `shouldBe` True

      it "resets file count to zero" $ do
        cache <- emptyCache
        let entry = CacheEntry
              { ceFilePath = "test.hs"
              , ceFileHash = "hash123"
              , ceModTime = acCreated cache
              , ceDiagnostics = []
              , ceAnalysisTime = 0.5
              , ceArgusVersion = "1.0.0"
              }
            cacheWithEntry = insertCache entry cache
            cleared = invalidateCache cacheWithEntry
        acTotalFiles cleared `shouldBe` 0

    describe "invalidateFile" $ do
      it "removes specific file from cache" $ do
        cache <- emptyCache
        let entry1 = CacheEntry
              { ceFilePath = "test1.hs"
              , ceFileHash = "hash1"
              , ceModTime = acCreated cache
              , ceDiagnostics = []
              , ceAnalysisTime = 0.5
              , ceArgusVersion = "1.0.0"
              }
            entry2 = entry1 { ceFilePath = "test2.hs", ceFileHash = "hash2" }
            cacheWith2 = insertCache entry2 $ insertCache entry1 cache
            invalidated = invalidateFile "test1.hs" cacheWith2
        Map.member "test1.hs" (acEntries invalidated) `shouldBe` False
        Map.member "test2.hs" (acEntries invalidated) `shouldBe` True

      it "updates file count" $ do
        cache <- emptyCache
        let entry = CacheEntry
              { ceFilePath = "test.hs"
              , ceFileHash = "hash123"
              , ceModTime = acCreated cache
              , ceDiagnostics = []
              , ceAnalysisTime = 0.5
              , ceArgusVersion = "1.0.0"
              }
            cacheWithEntry = insertCache entry cache
            invalidated = invalidateFile "test.hs" cacheWithEntry
        acTotalFiles invalidated `shouldBe` 0

    describe "getChangedFiles" $ do
      it "returns all files when cache is empty" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let file1 = tmpDir </> "test1.hs"
              file2 = tmpDir </> "test2.hs"
          writeFile file1 "module Test1 where"
          writeFile file2 "module Test2 where"
          cache <- emptyCache
          changed <- getChangedFiles cache [file1, file2]
          length changed `shouldBe` 2

      it "excludes non-existent files" $ do
        cache <- emptyCache
        changed <- getChangedFiles cache ["/nonexistent/file.hs"]
        changed `shouldBe` []

      it "returns empty for unchanged cached files" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let testFile = tmpDir </> "test.hs"
          writeFile testFile "module Test where"
          cache <- emptyCache
          Right fileHash <- hashFile testFile
          let entry = CacheEntry
                { ceFilePath = testFile
                , ceFileHash = fileHash
                , ceModTime = acCreated cache
                , ceDiagnostics = []
                , ceAnalysisTime = 0.5
                , ceArgusVersion = "1.0.0"
                }
              cacheWithEntry = insertCache entry cache
          changed <- getChangedFiles cacheWithEntry [testFile]
          changed `shouldBe` []

      it "returns file when content changes" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let testFile = tmpDir </> "test.hs"
          writeFile testFile "module Test where"
          cache <- emptyCache
          Right oldHash <- hashFile testFile
          let entry = CacheEntry
                { ceFilePath = testFile
                , ceFileHash = oldHash
                , ceModTime = acCreated cache
                , ceDiagnostics = []
                , ceAnalysisTime = 0.5
                , ceArgusVersion = "1.0.0"
                }
              cacheWithEntry = insertCache entry cache
          -- Modify file
          writeFile testFile "module Test where\n-- changed"
          changed <- getChangedFiles cacheWithEntry [testFile]
          changed `shouldBe` [testFile]

    describe "isFileCached" $ do
      it "returns False for uncached file" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let testFile = tmpDir </> "test.hs"
          writeFile testFile "module Test where"
          cache <- emptyCache
          cached <- isFileCached testFile cache
          cached `shouldBe` False

      it "returns True for cached file with matching hash" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let testFile = tmpDir </> "test.hs"
          writeFile testFile "module Test where"
          cache <- emptyCache
          Right fileHash <- hashFile testFile
          let entry = CacheEntry
                { ceFilePath = testFile
                , ceFileHash = fileHash
                , ceModTime = acCreated cache
                , ceDiagnostics = []
                , ceAnalysisTime = 0.5
                , ceArgusVersion = "1.0.0"
                }
              cacheWithEntry = insertCache entry cache
          cached <- isFileCached testFile cacheWithEntry
          cached `shouldBe` True

      it "returns False when file content changed" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let testFile = tmpDir </> "test.hs"
          writeFile testFile "module Test where"
          cache <- emptyCache
          Right oldHash <- hashFile testFile
          let entry = CacheEntry
                { ceFilePath = testFile
                , ceFileHash = oldHash
                , ceModTime = acCreated cache
                , ceDiagnostics = []
                , ceAnalysisTime = 0.5
                , ceArgusVersion = "1.0.0"
                }
              cacheWithEntry = insertCache entry cache
          writeFile testFile "module Test where\n-- changed"
          cached <- isFileCached testFile cacheWithEntry
          cached `shouldBe` False

    describe "getCachedDiagnostics" $ do
      it "returns Nothing for uncached file" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let testFile = tmpDir </> "test.hs"
          writeFile testFile "module Test where"
          cache <- emptyCache
          result <- getCachedDiagnostics testFile cache
          result `shouldBe` Nothing

      it "returns diagnostics for cached file" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let testFile = tmpDir </> "test.hs"
              diag = Diagnostic
                { diagSpan = mkSrcSpanRaw testFile 1 1 1 10
                , diagMessage = "Test diagnostic"
                , diagSeverity = Warning
                , diagKind = CodePattern
                , diagCode = Just "test/diag"
                , diagFixes = []
                , diagRelated = []
                }
          writeFile testFile "module Test where"
          cache <- emptyCache
          Right fileHash <- hashFile testFile
          let entry = CacheEntry
                { ceFilePath = testFile
                , ceFileHash = fileHash
                , ceModTime = acCreated cache
                , ceDiagnostics = [diag]
                , ceAnalysisTime = 0.5
                , ceArgusVersion = "1.0.0"
                }
              cacheWithEntry = insertCache entry cache
          result <- getCachedDiagnostics testFile cacheWithEntry
          result `shouldBe` Just [diag]

    describe "updateCacheWithResults" $ do
      it "adds results to cache" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let testFile = tmpDir </> "test.hs"
          writeFile testFile "module Test where"
          cache <- emptyCache
          newCache <- updateCacheWithResults cache [(testFile, [], 0.5)]
          Map.member testFile (acEntries newCache) `shouldBe` True

      it "stores diagnostics in entry" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let testFile = tmpDir </> "test.hs"
              diag = Diagnostic
                { diagSpan = mkSrcSpanRaw testFile 1 1 1 10
                , diagMessage = "Test diagnostic"
                , diagSeverity = Warning
                , diagKind = CodePattern
                , diagCode = Just "test/diag"
                , diagFixes = []
                , diagRelated = []
                }
          writeFile testFile "module Test where"
          cache <- emptyCache
          newCache <- updateCacheWithResults cache [(testFile, [diag], 0.5)]
          case Map.lookup testFile (acEntries newCache) of
            Nothing -> expectationFailure "Expected entry in cache"
            Just entry -> ceDiagnostics entry `shouldBe` [diag]

      it "stores analysis time in entry" $ do
        withSystemTempDirectory "argus-cache-test" $ \tmpDir -> do
          let testFile = tmpDir </> "test.hs"
          writeFile testFile "module Test where"
          cache <- emptyCache
          newCache <- updateCacheWithResults cache [(testFile, [], 1.25)]
          case Map.lookup testFile (acEntries newCache) of
            Nothing -> expectationFailure "Expected entry in cache"
            Just entry -> ceAnalysisTime entry `shouldBe` 1.25

    describe "CacheEntry" $ do
      it "stores all fields correctly" $ do
        cache <- emptyCache
        let entry = CacheEntry
              { ceFilePath = "test.hs"
              , ceFileHash = "abc123"
              , ceModTime = acCreated cache
              , ceDiagnostics = []
              , ceAnalysisTime = 2.5
              , ceArgusVersion = "1.0.0"
              }
        ceFilePath entry `shouldBe` "test.hs"
        ceFileHash entry `shouldBe` "abc123"
        ceAnalysisTime entry `shouldBe` 2.5
        ceArgusVersion entry `shouldBe` "1.0.0"

    describe "CacheKey" $ do
      it "has Eq instance" $ do
        let key1 = CacheKey "test.hs" "hash1"
            key2 = CacheKey "test.hs" "hash1"
            key3 = CacheKey "test.hs" "hash2"
        key1 `shouldBe` key2
        key1 `shouldNotBe` key3

      it "has Ord instance" $ do
        let key1 = CacheKey "a.hs" "hash"
            key2 = CacheKey "b.hs" "hash"
        compare key1 key2 `shouldBe` LT
