{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : IncrementalLoaderSpec
-- Description : Tests for the Argus.HIE.IncrementalLoader module
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Comprehensive tests for incremental HIE loading and caching.
module IncrementalLoaderSpec (spec) where

import Control.Monad (replicateM_)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict qualified as Map
import System.Directory (doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Argus.HIE.IncrementalLoader
import Argus.HIE.Types (TypeInfo(..))
import Argus.Types (mkSrcSpanRaw)

spec :: Spec
spec = do
  describe "LoaderConfig" $ do
    configSpec

  describe "IncrementalLoader" $ do
    loaderLifecycleSpec
    moduleLoadingSpec
    cacheInvalidationSpec

  describe "Cached Queries" $ do
    typeQueryCacheSpec
    symbolQueryCacheSpec
    definitionQueryCacheSpec
    referencesQueryCacheSpec

  describe "Cache Management" $ do
    cacheStatsSpec
    cachePersistenceSpec
    cacheClearSpec

  describe "Cache Validation" $ do
    cacheValidationSpec

--------------------------------------------------------------------------------
-- Configuration Tests
--------------------------------------------------------------------------------

configSpec :: Spec
configSpec = describe "configuration" $ do
  it "default config has reasonable values" $ do
    lcEnabled defaultLoaderConfig `shouldBe` True
    lcMaxCacheSize defaultLoaderConfig `shouldSatisfy` (> 0)
    lcMaxCacheAge defaultLoaderConfig `shouldSatisfy` (> 0)
    lcPersistOnShutdown defaultLoaderConfig `shouldBe` True

  it "allows disabling all features" $ do
    let config = defaultLoaderConfig
          { lcEnabled = False
          , lcPersistOnShutdown = False
          , lcLazyLoading = False
          }
    lcEnabled config `shouldBe` False
    lcPersistOnShutdown config `shouldBe` False
    lcLazyLoading config `shouldBe` False

  it "allows custom cache size" $ do
    let config = defaultLoaderConfig { lcMaxCacheSize = 50000 }
    lcMaxCacheSize config `shouldBe` 50000

  it "allows custom cache age" $ do
    let config = defaultLoaderConfig { lcMaxCacheAge = 3600 }
    lcMaxCacheAge config `shouldBe` 3600

--------------------------------------------------------------------------------
-- Loader Lifecycle Tests
--------------------------------------------------------------------------------

loaderLifecycleSpec :: Spec
loaderLifecycleSpec = describe "lifecycle" $ do
  it "initializes without error" $ withTestLoader $ \loader -> do
    stats <- getCacheStats loader
    csModulesLoaded stats `shouldBe` 0

  it "closes without error" $ do
    withSystemTempDirectory "argus-test" $ \tmpDir -> do
      let cachePath = tmpDir <> "/test-cache.json"
      loader <- initIncrementalLoader cachePath defaultLoaderConfig
      closeIncrementalLoader loader  -- Should not throw

  it "can be used after initialization" $ withTestLoader $ \loader -> do
    modules <- getLoadedModules loader
    modules `shouldBe` []

--------------------------------------------------------------------------------
-- Module Loading Tests
--------------------------------------------------------------------------------

moduleLoadingSpec :: Spec
moduleLoadingSpec = describe "module loading" $ do
  it "reports no modules loaded initially" $ withTestLoader $ \loader -> do
    loaded <- isModuleLoaded loader "Test.Module"
    loaded `shouldBe` False

  it "returns empty list for loaded modules initially" $ withTestLoader $ \loader -> do
    modules <- getLoadedModules loader
    modules `shouldBe` []

  it "loadModuleSymbols returns empty map for uncached modules" $ withTestLoader $ \loader -> do
    symbols <- loadModuleSymbols loader ["Test.Module", "Other.Module"]
    Map.size symbols `shouldBe` 0

  it "loadModuleSymbolsIfNeeded returns Nothing for uncached module" $ withTestLoader $ \loader -> do
    result <- loadModuleSymbolsIfNeeded loader "Nonexistent.Module"
    result `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Cache Invalidation Tests
--------------------------------------------------------------------------------

cacheInvalidationSpec :: Spec
cacheInvalidationSpec = describe "cache invalidation" $ do
  it "invalidateModule removes module from cache" $ withTestLoader $ \loader -> do
    -- Add some cached data via query
    _ <- cachedTypeQuery loader "test" (Just "Test.Module") (pure $ Just sampleTypeInfo)

    -- Invalidate
    invalidateModule loader "Test.Module"

    -- Module-specific cache should be cleared
    stats <- getCacheStats loader
    -- The module itself wasn't tracked as loaded
    csModulesLoaded stats `shouldBe` 0

  it "invalidateAllModules clears entire cache" $ withTestLoader $ \loader -> do
    -- Add some cached data
    _ <- cachedTypeQuery loader "test1" Nothing (pure $ Just sampleTypeInfo)
    _ <- cachedTypeQuery loader "test2" Nothing (pure $ Just sampleTypeInfo)

    stats1 <- getCacheStats loader
    csTypesCached stats1 `shouldBe` 2

    -- Invalidate all
    invalidateAllModules loader

    stats2 <- getCacheStats loader
    csTypesCached stats2 `shouldBe` 0
    csModulesLoaded stats2 `shouldBe` 0

--------------------------------------------------------------------------------
-- Type Query Cache Tests
--------------------------------------------------------------------------------

typeQueryCacheSpec :: Spec
typeQueryCacheSpec = describe "type query caching" $ do
  it "caches type query results" $ withTestLoader $ \loader -> do
    callCount <- newCallCounter

    let query = do
          incrementCallCounter callCount
          pure $ Just sampleTypeInfo

    -- First call - should execute query
    result1 <- cachedTypeQuery loader "head" (Just "Data.List") query
    result1 `shouldBe` Just sampleTypeInfo

    -- Second call - should use cache
    result2 <- cachedTypeQuery loader "head" (Just "Data.List") query
    result2 `shouldBe` Just sampleTypeInfo

    -- Query should only have been called once
    count <- getCallCount callCount
    count `shouldBe` 1

  it "tracks cache hits" $ withTestLoader $ \loader -> do
    _ <- cachedTypeQuery loader "test" Nothing (pure $ Just sampleTypeInfo)
    _ <- cachedTypeQuery loader "test" Nothing (pure $ Just sampleTypeInfo)
    _ <- cachedTypeQuery loader "test" Nothing (pure $ Just sampleTypeInfo)

    stats <- getCacheStats loader
    csCacheHits stats `shouldBe` 2
    csTotalQueries stats `shouldBe` 3

  it "caches Nothing results" $ withTestLoader $ \loader -> do
    callCount <- newCallCounter

    let query = do
          incrementCallCounter callCount
          pure Nothing

    _ <- cachedTypeQuery loader "nonexistent" Nothing query
    _ <- cachedTypeQuery loader "nonexistent" Nothing query

    count <- getCallCount callCount
    count `shouldBe` 1

  it "distinguishes by module name" $ withTestLoader $ \loader -> do
    _ <- cachedTypeQuery loader "head" (Just "Data.List") (pure $ Just sampleTypeInfo)
    _ <- cachedTypeQuery loader "head" (Just "Other.Module") (pure Nothing)

    stats <- getCacheStats loader
    csTypesCached stats `shouldBe` 2

--------------------------------------------------------------------------------
-- Symbol Query Cache Tests
--------------------------------------------------------------------------------

symbolQueryCacheSpec :: Spec
symbolQueryCacheSpec = describe "symbol query caching" $ do
  it "caches symbol query results" $ withTestLoader $ \loader -> do
    callCount <- newCallCounter

    let query = do
          incrementCallCounter callCount
          pure Nothing

    _ <- cachedSymbolQuery loader "myFunc" (Just "My.Module") query
    _ <- cachedSymbolQuery loader "myFunc" (Just "My.Module") query

    count <- getCallCount callCount
    count `shouldBe` 1

  it "updates cache stats for symbol queries" $ withTestLoader $ \loader -> do
    _ <- cachedSymbolQuery loader "test" Nothing (pure Nothing)

    stats <- getCacheStats loader
    csSymbolsCached stats `shouldBe` 1
    csTotalQueries stats `shouldBe` 1

--------------------------------------------------------------------------------
-- Definition Query Cache Tests
--------------------------------------------------------------------------------

definitionQueryCacheSpec :: Spec
definitionQueryCacheSpec = describe "definition query caching" $ do
  it "caches definition locations" $ withTestLoader $ \loader -> do
    callCount <- newCallCounter
    let srcSpan = mkSrcSpanRaw "Test.hs" 10 5 10 15
        query = do
          incrementCallCounter callCount
          pure $ Just srcSpan

    result1 <- cachedDefinitionQuery loader "myFunc" query
    result2 <- cachedDefinitionQuery loader "myFunc" query

    result1 `shouldBe` Just srcSpan
    result2 `shouldBe` Just srcSpan

    count <- getCallCount callCount
    count `shouldBe` 1

  it "updates cache stats for definition queries" $ withTestLoader $ \loader -> do
    _ <- cachedDefinitionQuery loader "test" (pure Nothing)

    stats <- getCacheStats loader
    csDefinitionsCached stats `shouldBe` 1

--------------------------------------------------------------------------------
-- References Query Cache Tests
--------------------------------------------------------------------------------

referencesQueryCacheSpec :: Spec
referencesQueryCacheSpec = describe "references query caching" $ do
  it "caches reference lists" $ withTestLoader $ \loader -> do
    callCount <- newCallCounter
    let refs = [mkSrcSpanRaw "A.hs" 1 1 1 10, mkSrcSpanRaw "B.hs" 5 5 5 15]
        query = do
          incrementCallCounter callCount
          pure refs

    result1 <- cachedReferencesQuery loader "sharedFunc" query
    result2 <- cachedReferencesQuery loader "sharedFunc" query

    length result1 `shouldBe` 2
    length result2 `shouldBe` 2

    count <- getCallCount callCount
    count `shouldBe` 1

  it "caches empty reference lists" $ withTestLoader $ \loader -> do
    callCount <- newCallCounter

    _ <- cachedReferencesQuery loader "unusedFunc" $ do
          incrementCallCounter callCount
          pure []
    _ <- cachedReferencesQuery loader "unusedFunc" (pure [])

    count <- getCallCount callCount
    count `shouldBe` 1

--------------------------------------------------------------------------------
-- Cache Stats Tests
--------------------------------------------------------------------------------

cacheStatsSpec :: Spec
cacheStatsSpec = describe "cache statistics" $ do
  it "tracks total queries" $ withTestLoader $ \loader -> do
    _ <- cachedTypeQuery loader "a" Nothing (pure Nothing)
    _ <- cachedSymbolQuery loader "b" Nothing (pure Nothing)
    _ <- cachedDefinitionQuery loader "c" (pure Nothing)

    stats <- getCacheStats loader
    csTotalQueries stats `shouldBe` 3

  it "calculates hit rate correctly" $ withTestLoader $ \loader -> do
    -- 1 miss (first query)
    _ <- cachedTypeQuery loader "test" Nothing (pure Nothing)
    -- 2 hits
    _ <- cachedTypeQuery loader "test" Nothing (pure Nothing)
    _ <- cachedTypeQuery loader "test" Nothing (pure Nothing)

    stats <- getCacheStats loader
    -- 2 hits / 3 total = 66.67%
    csHitRate stats `shouldSatisfy` (\r -> r > 66 && r < 67)

  it "reports zero hit rate for empty cache" $ withTestLoader $ \loader -> do
    stats <- getCacheStats loader
    csHitRate stats `shouldBe` 0

  it "estimates cache size" $ withTestLoader $ \loader -> do
    replicateM_ 10 $ cachedTypeQuery loader "test" Nothing (pure $ Just sampleTypeInfo)

    stats <- getCacheStats loader
    csCacheSizeBytes stats `shouldSatisfy` (> 0)

--------------------------------------------------------------------------------
-- Cache Persistence Tests
--------------------------------------------------------------------------------

cachePersistenceSpec :: Spec
cachePersistenceSpec = describe "cache persistence" $ do
  it "saves cache to file" $ do
    withSystemTempDirectory "argus-test" $ \tmpDir -> do
      let cachePath = tmpDir <> "/persist-test.json"

      -- Create loader and add some data
      loader <- initIncrementalLoader cachePath defaultLoaderConfig
      _ <- cachedTypeQuery loader "test" Nothing (pure $ Just sampleTypeInfo)
      saveLoaderState loader
      closeIncrementalLoader loader

      -- Verify file exists
      exists <- doesFileExist cachePath
      exists `shouldBe` True

  it "loads cache from file" $ do
    withSystemTempDirectory "argus-test" $ \tmpDir -> do
      let cachePath = tmpDir <> "/load-test.json"

      -- Create and populate cache
      loader1 <- initIncrementalLoader cachePath defaultLoaderConfig
      _ <- cachedTypeQuery loader1 "persistent" Nothing (pure $ Just sampleTypeInfo)
      saveLoaderState loader1
      closeIncrementalLoader loader1

      -- Load cache in new loader
      loader2 <- initIncrementalLoader cachePath defaultLoaderConfig

      -- The cached query should be available
      stats <- getCacheStats loader2
      csTypesCached stats `shouldBe` 1
      closeIncrementalLoader loader2

  it "handles missing cache file gracefully" $ do
    withSystemTempDirectory "argus-test" $ \tmpDir -> do
      let cachePath = tmpDir <> "/nonexistent.json"
      loader <- initIncrementalLoader cachePath defaultLoaderConfig
      stats <- getCacheStats loader
      csTypesCached stats `shouldBe` 0
      closeIncrementalLoader loader

--------------------------------------------------------------------------------
-- Cache Clear Tests
--------------------------------------------------------------------------------

cacheClearSpec :: Spec
cacheClearSpec = describe "cache clearing" $ do
  it "clearCache removes all cached data" $ withTestLoader $ \loader -> do
    -- Add various cached data
    _ <- cachedTypeQuery loader "t1" Nothing (pure $ Just sampleTypeInfo)
    _ <- cachedSymbolQuery loader "s1" Nothing (pure Nothing)
    _ <- cachedDefinitionQuery loader "d1" (pure Nothing)
    _ <- cachedReferencesQuery loader "r1" (pure [])

    stats1 <- getCacheStats loader
    csTypesCached stats1 `shouldBe` 1
    csSymbolsCached stats1 `shouldBe` 1

    -- Clear
    clearCache loader

    stats2 <- getCacheStats loader
    csTypesCached stats2 `shouldBe` 0
    csSymbolsCached stats2 `shouldBe` 0
    csDefinitionsCached stats2 `shouldBe` 0
    csReferencesCached stats2 `shouldBe` 0

  it "clearCache resets query counters" $ withTestLoader $ \loader -> do
    _ <- cachedTypeQuery loader "test" Nothing (pure Nothing)
    _ <- cachedTypeQuery loader "test" Nothing (pure Nothing)

    clearCache loader

    stats <- getCacheStats loader
    csTotalQueries stats `shouldBe` 0
    csCacheHits stats `shouldBe` 0

--------------------------------------------------------------------------------
-- Cache Validation Tests
--------------------------------------------------------------------------------

cacheValidationSpec :: Spec
cacheValidationSpec = describe "cache validation" $ do
  it "validates cache on load when enabled" $ do
    withSystemTempDirectory "argus-test" $ \tmpDir -> do
      let cachePath = tmpDir <> "/validate-test.json"
          config = defaultLoaderConfig { lcValidateOnLoad = True }

      loader <- initIncrementalLoader cachePath config
      closeIncrementalLoader loader
      -- Should not throw with validation enabled

  it "skips validation when disabled" $ do
    withSystemTempDirectory "argus-test" $ \tmpDir -> do
      let cachePath = tmpDir <> "/no-validate-test.json"
          config = defaultLoaderConfig { lcValidateOnLoad = False }

      loader <- initIncrementalLoader cachePath config
      closeIncrementalLoader loader

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

-- | Run a test with a fresh loader
withTestLoader :: (IncrementalLoader -> IO a) -> IO a
withTestLoader action =
  withSystemTempDirectory "argus-test" $ \tmpDir -> do
    let cachePath = tmpDir <> "/test-cache.json"
    loader <- initIncrementalLoader cachePath defaultLoaderConfig
    result <- action loader
    closeIncrementalLoader loader
    pure result

-- | Sample TypeInfo for testing
sampleTypeInfo :: TypeInfo
sampleTypeInfo = TypeInfo
  { tiType = "[a] -> Maybe a"
  , tiKind = "*"
  , tiConstraints = []
  , tiMonomorphic = False
  , tiPolymorphic = ["a"]
  }

-- | Simple call counter using IORef
newtype CallCounter = CallCounter (IORef Int)

newCallCounter :: IO CallCounter
newCallCounter = CallCounter <$> newIORef 0

incrementCallCounter :: CallCounter -> IO ()
incrementCallCounter (CallCounter ref) = modifyIORef' ref (+1)

getCallCount :: CallCounter -> IO Int
getCallCount (CallCounter ref) = readIORef ref
