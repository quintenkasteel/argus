{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Analysis.IncrementalCache
-- Description : Incremental analysis caching system
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- This module provides an incremental caching system for analysis results
-- that minimizes re-analysis when files change. It implements:
--
-- * Content-based cache invalidation
-- * Dependency tracking between files
-- * Fine-grained caching at expression/declaration level
-- * Persistent cache storage
-- * Cache statistics and debugging
--
-- == Architecture
--
-- The cache operates at multiple granularities:
--
-- 1. **File level**: Cache entire file analysis results
-- 2. **Declaration level**: Cache individual function/type analyses
-- 3. **Expression level**: Cache type inference and pattern match results
--
-- == Cache Key Strategy
--
-- Cache keys combine:
-- * File content hash (SHA256)
-- * Analysis configuration hash
-- * Dependency hashes (for cross-file analysis)
--
-- == Example Usage
--
-- @
-- -- Initialize cache
-- cache <- initCache defaultCacheConfig
--
-- -- Check if analysis is cached
-- mResult <- lookupAnalysis cache filePath content config
-- case mResult of
--   Just result -> pure result  -- Use cached
--   Nothing -> do
--     result <- runAnalysis filePath content config
--     storeAnalysis cache filePath content config result
--     pure result
-- @
module Argus.Analysis.IncrementalCache
  ( -- * Cache System
    AnalysisCache (..)
  , CacheConfig (..)
  , defaultCacheConfig
  , initCache
  , shutdownCache
  , clearCache
  , compactCache

    -- * Cache Keys
  , CacheKey (..)
  , computeKey
  , computeFileKey
  , computeDeclKey
  , KeyComponents (..)

    -- * Cache Operations
  , lookupAnalysis
  , storeAnalysis
  , invalidateFile
  , invalidateFiles
  , invalidateDependents
  , CacheResult (..)

    -- * Cache Entry Types
  , CacheEntry (..)
  , EntryMetadata (..)
  , EntryStatus (..)

    -- * File-Level Cache
  , FileCache (..)
  , lookupFileCache
  , storeFileCache
  , invalidateFileCache

    -- * Declaration-Level Cache
  , DeclCache (..)
  , lookupDeclCache
  , storeDeclCache
  , invalidateDeclCache

    -- * Expression-Level Cache
  , ExprCache (..)
  , lookupExprCache
  , storeExprCache

    -- * Dependency Tracking
  , DependencyTracker (..)
  , addDependency
  , removeDependency
  , getDependents
  , getDependencies
  , rebuildDependencyGraph

    -- * Cache Persistence
  , saveCache
  , loadCache
  , CachePersistence (..)
  , PersistenceFormat (..)

    -- * Hash Computation
  , ContentHash (..)
  , computeContentHash
  , computeConfigHash
  , HashAlgorithm (..)

    -- * Cache Statistics
  , CacheStats (..)
  , getCacheStats
  , resetCacheStats

    -- * Cache Events
  , CacheEvent (..)
  , CacheEventCallback
  , registerEventCallback

    -- * Eviction Policies
  , EvictionPolicy (..)
  , LRUPolicy (..)
  , TTLPolicy (..)
  , SizePolicy (..)
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVar, readTVarIO, writeTVar, modifyTVar', atomically)
import Control.Exception (try, SomeException)
import Control.Monad (forM_, when)
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Aeson (ToJSON (..), FromJSON (..), ToJSONKey (..), FromJSONKey (..), encode, decode)
import Data.Aeson.Types (toJSONKeyText, FromJSONKeyFunction (..))
import Data.ByteString.Lazy qualified as BL
import Data.Hashable (Hashable, hashWithSalt)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)

import Argus.Types (Diagnostic, SrcSpan)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for the analysis cache
data CacheConfig = CacheConfig
  { ccStoragePath      :: Maybe FilePath
    -- ^ Path for persistent cache storage (Nothing for in-memory only)
  , ccMaxEntries       :: Int
    -- ^ Maximum cache entries
  , ccMaxMemoryMB      :: Int
    -- ^ Maximum memory usage in MB
  , ccTTLSeconds       :: Int
    -- ^ Time-to-live for entries in seconds
  , ccEvictionPolicy   :: EvictionPolicy
    -- ^ Policy for evicting old entries
  , ccTrackDependencies :: Bool
    -- ^ Track inter-file dependencies
  , ccHashAlgorithm    :: HashAlgorithm
    -- ^ Algorithm for computing content hashes
  , ccPersistenceFormat :: PersistenceFormat
    -- ^ Format for persistent storage
  , ccAutoSave         :: Bool
    -- ^ Auto-save cache periodically
  , ccSaveIntervalSecs :: Int
    -- ^ Save interval in seconds
  , ccEnableCompression :: Bool
    -- ^ Compress cached data
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default cache configuration
defaultCacheConfig :: CacheConfig
defaultCacheConfig = CacheConfig
  { ccStoragePath = Just ".argus/cache"
  , ccMaxEntries = 10000
  , ccMaxMemoryMB = 256
  , ccTTLSeconds = 3600  -- 1 hour
  , ccEvictionPolicy = EPComposite [EPLRU, EPTTL]
  , ccTrackDependencies = True
  , ccHashAlgorithm = HASHA256
  , ccPersistenceFormat = PFBinary
  , ccAutoSave = True
  , ccSaveIntervalSecs = 300
  , ccEnableCompression = True
  }

--------------------------------------------------------------------------------
-- Analysis Cache
--------------------------------------------------------------------------------

-- | The main cache structure
data AnalysisCache = AnalysisCache
  { acConfig       :: CacheConfig
  , acFileCache    :: TVar (Map CacheKey FileCache)
  , acDeclCache    :: TVar (Map CacheKey DeclCache)
  , acExprCache    :: TVar (Map CacheKey ExprCache)
  , acDependencies :: TVar DependencyTracker
  , acStats        :: TVar CacheStats
  , acLastSave     :: TVar UTCTime
  , acEventCallbacks :: IORef [CacheEventCallback]
  }

-- | Initialize the cache
initCache :: CacheConfig -> IO AnalysisCache
initCache config = do
  fileCacheVar <- newTVarIO Map.empty
  declCacheVar <- newTVarIO Map.empty
  exprCacheVar <- newTVarIO Map.empty
  depsVar <- newTVarIO emptyDependencyTracker
  statsVar <- newTVarIO emptyCacheStats
  now <- getCurrentTime
  lastSaveVar <- newTVarIO now
  callbacksRef <- newIORef []

  let cache = AnalysisCache
        { acConfig = config
        , acFileCache = fileCacheVar
        , acDeclCache = declCacheVar
        , acExprCache = exprCacheVar
        , acDependencies = depsVar
        , acStats = statsVar
        , acLastSave = lastSaveVar
        , acEventCallbacks = callbacksRef
        }

  -- Try to load persistent cache
  case ccStoragePath config of
    Just path -> do
      _ <- try @SomeException $ loadCache cache path
      pure ()
    Nothing -> pure ()

  pure cache

-- | Shutdown the cache
shutdownCache :: AnalysisCache -> IO ()
shutdownCache cache = do
  when (ccAutoSave $ acConfig cache) $ do
    case ccStoragePath (acConfig cache) of
      Just path -> saveCache cache path
      Nothing -> pure ()

-- | Clear all cache entries
clearCache :: AnalysisCache -> IO ()
clearCache cache = do
  atomically $ do
    writeTVar (acFileCache cache) Map.empty
    writeTVar (acDeclCache cache) Map.empty
    writeTVar (acExprCache cache) Map.empty
    writeTVar (acDependencies cache) emptyDependencyTracker

  emitEvent cache CECleared

-- | Compact the cache (remove expired entries)
compactCache :: AnalysisCache -> IO Int
compactCache cache = do
  now <- getCurrentTime
  let config = acConfig cache
      ttl = fromIntegral (ccTTLSeconds config)

  removed <- atomically $ do
    fileCache <- readTVar (acFileCache cache)
    let (keep, expire) = Map.partition (isValid now ttl) fileCache
    writeTVar (acFileCache cache) keep
    pure $ Map.size expire

  emitEvent cache (CECompacted removed)
  pure removed
  where
    isValid :: UTCTime -> NominalDiffTime -> FileCache -> Bool
    isValid now ttl fc =
      let age = diffUTCTime now (emCreatedAt $ fcMetadata fc)
      in age < ttl

--------------------------------------------------------------------------------
-- Cache Keys
--------------------------------------------------------------------------------

-- | A cache key uniquely identifying cached content
data CacheKey = CacheKey
  { ckHash       :: ContentHash
  , ckComponents :: KeyComponents
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | Convert CacheKey to text for JSON map key
cacheKeyToText :: CacheKey -> Text
cacheKeyToText CacheKey{..} =
  unContentHash ckHash <> ":" <> T.pack (kcFilePath ckComponents)

-- | Parse CacheKey from text (simplified)
textToCacheKey :: Text -> Maybe CacheKey
textToCacheKey t = case T.splitOn ":" t of
  [hashPart, pathPart] -> Just $ CacheKey
    { ckHash = ContentHash hashPart
    , ckComponents = KeyComponents
        { kcFilePath = T.unpack pathPart
        , kcConfigHash = ContentHash ""
        , kcDepsHash = Nothing
        , kcGranularity = CGFile
        }
    }
  _ -> Nothing

instance ToJSONKey CacheKey where
  toJSONKey = toJSONKeyText cacheKeyToText

instance FromJSONKey CacheKey where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    case textToCacheKey t of
      Just k -> pure k
      Nothing -> fail $ "Invalid CacheKey: " <> T.unpack t

-- | Components that make up a cache key
data KeyComponents = KeyComponents
  { kcFilePath    :: FilePath
  , kcConfigHash  :: ContentHash
  , kcDepsHash    :: Maybe ContentHash
  , kcGranularity :: CacheGranularity
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | Granularity level of the cache
data CacheGranularity
  = CGFile           -- ^ Whole file
  | CGDeclaration    -- ^ Single declaration
  | CGExpression     -- ^ Single expression
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | Compute a cache key
computeKey :: FilePath -> Text -> ContentHash -> CacheGranularity -> CacheKey
computeKey path content configHash granularity =
  let contentHash = computeContentHash content
      components = KeyComponents
        { kcFilePath = path
        , kcConfigHash = configHash
        , kcDepsHash = Nothing
        , kcGranularity = granularity
        }
  in CacheKey contentHash components

-- | Compute file-level cache key
computeFileKey :: FilePath -> Text -> ContentHash -> CacheKey
computeFileKey path content configHash =
  computeKey path content configHash CGFile

-- | Compute declaration-level cache key
computeDeclKey :: FilePath -> Text -> Text -> ContentHash -> CacheKey
computeDeclKey path declName declContent configHash =
  let combined = declName <> ":" <> declContent
  in computeKey path combined configHash CGDeclaration

--------------------------------------------------------------------------------
-- Cache Operations
--------------------------------------------------------------------------------

-- | Result of a cache lookup
data CacheResult a
  = CacheHit a               -- ^ Entry found and valid
  | CacheMiss                -- ^ Entry not found
  | CacheStale a             -- ^ Entry found but stale
  | CacheError Text          -- ^ Error accessing cache
  deriving stock (Eq, Show, Functor)

-- | Look up analysis result in cache
lookupAnalysis :: AnalysisCache -> CacheKey -> IO (CacheResult [Diagnostic])
lookupAnalysis cache key = do
  mEntry <- atomically $ Map.lookup key <$> readTVar (acFileCache cache)

  case mEntry of
    Nothing -> do
      recordMiss cache
      pure CacheMiss
    Just entry -> do
      now <- getCurrentTime
      let config = acConfig cache
          ttl = fromIntegral (ccTTLSeconds config)
          age = diffUTCTime now (emCreatedAt $ fcMetadata entry)

      if age > ttl
        then do
          recordStale cache
          pure $ CacheStale (fcDiagnostics entry)
        else do
          recordHit cache
          pure $ CacheHit (fcDiagnostics entry)

-- | Store analysis result in cache
storeAnalysis :: AnalysisCache -> CacheKey -> [Diagnostic] -> IO ()
storeAnalysis cache key diagnostics = do
  now <- getCurrentTime

  let entry = FileCache
        { fcDiagnostics = diagnostics
        , fcDeclarations = []
        , fcImports = []
        , fcExports = []
        , fcMetadata = EntryMetadata
            { emCreatedAt = now
            , emLastAccess = now
            , emAccessCount = 1
            , emSize = estimateSize diagnostics
            , emStatus = ESValid
            }
        }

  atomically $ modifyTVar' (acFileCache cache) $ Map.insert key entry

  -- Check if eviction is needed
  checkEviction cache

  emitEvent cache (CEStored (kcFilePath $ ckComponents key))
  where
    estimateSize :: [Diagnostic] -> Int
    estimateSize diags = length diags * 100  -- Rough estimate

-- | Invalidate a file from cache
invalidateFile :: AnalysisCache -> FilePath -> IO ()
invalidateFile cache path = do
  let matchesPath key = kcFilePath (ckComponents key) == path

  atomically $ do
    modifyTVar' (acFileCache cache) $ Map.filterWithKey (\k _ -> not $ matchesPath k)
    modifyTVar' (acDeclCache cache) $ Map.filterWithKey (\k _ -> not $ matchesPath k)
    modifyTVar' (acExprCache cache) $ Map.filterWithKey (\k _ -> not $ matchesPath k)

  emitEvent cache (CEInvalidated path)

-- | Invalidate multiple files
invalidateFiles :: AnalysisCache -> [FilePath] -> IO ()
invalidateFiles cache = mapM_ (invalidateFile cache)

-- | Invalidate all files that depend on a given file
invalidateDependents :: AnalysisCache -> FilePath -> IO [FilePath]
invalidateDependents cache path = do
  deps <- readTVarIO (acDependencies cache)
  let dependents = getDependents deps path
  invalidateFiles cache dependents
  pure dependents

--------------------------------------------------------------------------------
-- Cache Entry Types
--------------------------------------------------------------------------------

-- | Metadata about a cache entry
data EntryMetadata = EntryMetadata
  { emCreatedAt   :: UTCTime
  , emLastAccess  :: UTCTime
  , emAccessCount :: Int
  , emSize        :: Int
  , emStatus      :: EntryStatus
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Status of a cache entry
data EntryStatus
  = ESValid           -- ^ Entry is valid
  | ESStale           -- ^ Entry is stale (dependencies changed)
  | ESExpired         -- ^ Entry has expired (TTL)
  | ESInvalidated     -- ^ Entry was explicitly invalidated
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A cached entry with metadata
data CacheEntry a = CacheEntry
  { ceData     :: a
  , ceMetadata :: EntryMetadata
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- File-Level Cache
--------------------------------------------------------------------------------

-- | Cache entry for a file
data FileCache = FileCache
  { fcDiagnostics   :: [Diagnostic]
  , fcDeclarations  :: [Text]
  , fcImports       :: [Text]
  , fcExports       :: [Text]
  , fcMetadata      :: EntryMetadata
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Look up file cache
lookupFileCache :: AnalysisCache -> CacheKey -> IO (Maybe FileCache)
lookupFileCache cache key = do
  atomically $ Map.lookup key <$> readTVar (acFileCache cache)

-- | Store file cache
storeFileCache :: AnalysisCache -> CacheKey -> FileCache -> IO ()
storeFileCache cache key entry = do
  atomically $ modifyTVar' (acFileCache cache) $ Map.insert key entry

-- | Invalidate file cache
invalidateFileCache :: AnalysisCache -> CacheKey -> IO ()
invalidateFileCache cache key = do
  atomically $ modifyTVar' (acFileCache cache) $ Map.delete key

--------------------------------------------------------------------------------
-- Declaration-Level Cache
--------------------------------------------------------------------------------

-- | Cache entry for a declaration
data DeclCache = DeclCache
  { dcName        :: Text
  , dcType        :: Maybe Text
  , dcDiagnostics :: [Diagnostic]
  , dcDependsOn   :: [Text]
  , dcMetadata    :: EntryMetadata
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Look up declaration cache
lookupDeclCache :: AnalysisCache -> CacheKey -> IO (Maybe DeclCache)
lookupDeclCache cache key = do
  atomically $ Map.lookup key <$> readTVar (acDeclCache cache)

-- | Store declaration cache
storeDeclCache :: AnalysisCache -> CacheKey -> DeclCache -> IO ()
storeDeclCache cache key entry = do
  atomically $ modifyTVar' (acDeclCache cache) $ Map.insert key entry

-- | Invalidate declaration cache
invalidateDeclCache :: AnalysisCache -> CacheKey -> IO ()
invalidateDeclCache cache key = do
  atomically $ modifyTVar' (acDeclCache cache) $ Map.delete key

--------------------------------------------------------------------------------
-- Expression-Level Cache
--------------------------------------------------------------------------------

-- | Cache entry for an expression
data ExprCache = ExprCache
  { ecSpan       :: SrcSpan
  , ecType       :: Maybe Text
  , ecDiagnostics :: [Diagnostic]
  , ecMetadata   :: EntryMetadata
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Look up expression cache
lookupExprCache :: AnalysisCache -> CacheKey -> IO (Maybe ExprCache)
lookupExprCache cache key = do
  atomically $ Map.lookup key <$> readTVar (acExprCache cache)

-- | Store expression cache
storeExprCache :: AnalysisCache -> CacheKey -> ExprCache -> IO ()
storeExprCache cache key entry = do
  atomically $ modifyTVar' (acExprCache cache) $ Map.insert key entry

--------------------------------------------------------------------------------
-- Dependency Tracking
--------------------------------------------------------------------------------

-- | Tracker for inter-file dependencies
data DependencyTracker = DependencyTracker
  { dtForward  :: Map FilePath (Set FilePath)  -- ^ A -> {B, C} means A depends on B, C
  , dtReverse  :: Map FilePath (Set FilePath)  -- ^ B -> {A} means A depends on B
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty dependency tracker
emptyDependencyTracker :: DependencyTracker
emptyDependencyTracker = DependencyTracker Map.empty Map.empty

-- | Add a dependency (A depends on B)
addDependency :: DependencyTracker -> FilePath -> FilePath -> DependencyTracker
addDependency tracker from to = tracker
  { dtForward = Map.insertWith Set.union from (Set.singleton to) (dtForward tracker)
  , dtReverse = Map.insertWith Set.union to (Set.singleton from) (dtReverse tracker)
  }

-- | Remove a dependency
removeDependency :: DependencyTracker -> FilePath -> FilePath -> DependencyTracker
removeDependency tracker from to = tracker
  { dtForward = Map.adjust (Set.delete to) from (dtForward tracker)
  , dtReverse = Map.adjust (Set.delete from) to (dtReverse tracker)
  }

-- | Get all files that depend on a given file
getDependents :: DependencyTracker -> FilePath -> [FilePath]
getDependents tracker path =
  Set.toList $ Map.findWithDefault Set.empty path (dtReverse tracker)

-- | Get all files a given file depends on
getDependencies :: DependencyTracker -> FilePath -> [FilePath]
getDependencies tracker path =
  Set.toList $ Map.findWithDefault Set.empty path (dtForward tracker)

-- | Rebuild the dependency graph from scratch
rebuildDependencyGraph :: AnalysisCache -> [FilePath] -> IO ()
rebuildDependencyGraph cache _files = do
  -- Would parse imports from each file and rebuild
  atomically $ writeTVar (acDependencies cache) emptyDependencyTracker

--------------------------------------------------------------------------------
-- Cache Persistence
--------------------------------------------------------------------------------

-- | Persistence operations
data CachePersistence = CachePersistence
  { cpPath      :: FilePath
  , cpFormat    :: PersistenceFormat
  , cpVersion   :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Format for persistent storage
data PersistenceFormat
  = PFJson           -- ^ JSON format (human-readable)
  | PFBinary         -- ^ Binary format (compact)
  | PFCompressed     -- ^ Compressed binary
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Save cache to disk
saveCache :: AnalysisCache -> FilePath -> IO ()
saveCache cache path = do
  createDirectoryIfMissing True (takeDirectory path)

  fileCache <- readTVarIO (acFileCache cache)
  deps <- readTVarIO (acDependencies cache)

  let cacheData = (fileCache, deps)

  BL.writeFile (path </> "cache.json") (encode cacheData)

  now <- getCurrentTime
  atomically $ writeTVar (acLastSave cache) now

  emitEvent cache (CESaved path)

-- | Load cache from disk
loadCache :: AnalysisCache -> FilePath -> IO ()
loadCache cache path = do
  let cachePath = path </> "cache.json"
  exists <- doesFileExist cachePath

  when exists $ do
    mData <- decode <$> BL.readFile cachePath
    case mData of
      Just (fileCache, deps) -> do
        atomically $ do
          writeTVar (acFileCache cache) fileCache
          writeTVar (acDependencies cache) deps
        emitEvent cache (CELoaded path)
      Nothing -> pure ()

--------------------------------------------------------------------------------
-- Hash Computation
--------------------------------------------------------------------------------

-- | A content hash
newtype ContentHash = ContentHash { unContentHash :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Hashable ContentHash where
  hashWithSalt salt (ContentHash h) = hashWithSalt salt h

-- | Hash algorithm
data HashAlgorithm
  = HASHA256         -- ^ SHA-256
  | HASHXXH64        -- ^ xxHash64 (faster)
  | HASHSimple       -- ^ Simple Haskell hash (fastest)
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Compute content hash
computeContentHash :: Text -> ContentHash
computeContentHash content =
  let bytes = TE.encodeUtf8 content
      digest = hashWith SHA256 bytes
  in ContentHash $ T.pack $ show digest

-- | Compute configuration hash
computeConfigHash :: ToJSON a => a -> ContentHash
computeConfigHash config =
  let bytes = BL.toStrict $ encode config
      digest = hashWith SHA256 bytes
  in ContentHash $ T.pack $ show digest

--------------------------------------------------------------------------------
-- Cache Statistics
--------------------------------------------------------------------------------

-- | Statistics about cache usage
data CacheStats = CacheStats
  { csHits         :: Int
  , csMisses       :: Int
  , csStale        :: Int
  , csEvictions    :: Int
  , csInvalidations :: Int
  , csTotalEntries :: Int
  , csTotalSize    :: Int
  , csHitRate      :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty statistics
emptyCacheStats :: CacheStats
emptyCacheStats = CacheStats 0 0 0 0 0 0 0 0.0

-- | Get cache statistics
getCacheStats :: AnalysisCache -> IO CacheStats
getCacheStats cache = do
  stats <- readTVarIO (acStats cache)
  fileCache <- readTVarIO (acFileCache cache)
  let totalEntries = Map.size fileCache
      hitRate = if csHits stats + csMisses stats == 0
                then 0.0
                else fromIntegral (csHits stats) / fromIntegral (csHits stats + csMisses stats)
  pure stats
    { csTotalEntries = totalEntries
    , csHitRate = hitRate
    }

-- | Reset cache statistics
resetCacheStats :: AnalysisCache -> IO ()
resetCacheStats cache = do
  atomically $ writeTVar (acStats cache) emptyCacheStats

-- | Record a cache hit
recordHit :: AnalysisCache -> IO ()
recordHit cache = do
  atomically $ modifyTVar' (acStats cache) $ \s ->
    s { csHits = csHits s + 1 }

-- | Record a cache miss
recordMiss :: AnalysisCache -> IO ()
recordMiss cache = do
  atomically $ modifyTVar' (acStats cache) $ \s ->
    s { csMisses = csMisses s + 1 }

-- | Record a stale hit
recordStale :: AnalysisCache -> IO ()
recordStale cache = do
  atomically $ modifyTVar' (acStats cache) $ \s ->
    s { csStale = csStale s + 1 }

--------------------------------------------------------------------------------
-- Cache Events
--------------------------------------------------------------------------------

-- | Events that can occur in the cache
data CacheEvent
  = CEHit FilePath            -- ^ Cache hit
  | CEMiss FilePath           -- ^ Cache miss
  | CEStored FilePath         -- ^ Entry stored
  | CEInvalidated FilePath    -- ^ Entry invalidated
  | CEEvicted FilePath        -- ^ Entry evicted
  | CECompacted Int           -- ^ Cache compacted (entries removed)
  | CECleared                 -- ^ Cache cleared
  | CESaved FilePath          -- ^ Cache saved to disk
  | CELoaded FilePath         -- ^ Cache loaded from disk
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Callback for cache events
type CacheEventCallback = CacheEvent -> IO ()

-- | Register an event callback
registerEventCallback :: AnalysisCache -> CacheEventCallback -> IO ()
registerEventCallback cache callback = do
  modifyIORef' (acEventCallbacks cache) (callback :)

-- | Emit a cache event
emitEvent :: AnalysisCache -> CacheEvent -> IO ()
emitEvent cache event = do
  callbacks <- readIORef (acEventCallbacks cache)
  forM_ callbacks $ \cb -> cb event

--------------------------------------------------------------------------------
-- Eviction Policies
--------------------------------------------------------------------------------

-- | Policy for evicting cache entries
data EvictionPolicy
  = EPLRU              -- ^ Least Recently Used
  | EPTTL              -- ^ Time To Live
  | EPSize             -- ^ Size-based
  | EPComposite [EvictionPolicy]  -- ^ Combine multiple policies
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | LRU eviction policy state
data LRUPolicy = LRUPolicy
  { lruOrder :: [CacheKey]
  , lruMax   :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | TTL eviction policy state
data TTLPolicy = TTLPolicy
  { ttlDefault :: Int
  , ttlCustom  :: Map CacheKey Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Size-based eviction policy state
data SizePolicy = SizePolicy
  { spMaxSize    :: Int
  , spCurrentSize :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Check if eviction is needed and perform it
checkEviction :: AnalysisCache -> IO ()
checkEviction cache = do
  let config = acConfig cache
      maxEntries = ccMaxEntries config

  fileCache <- readTVarIO (acFileCache cache)

  when (Map.size fileCache > maxEntries) $ do
    -- Evict oldest entries
    let toEvict = Map.size fileCache - maxEntries
    evictEntries cache toEvict

-- | Evict a number of entries
evictEntries :: AnalysisCache -> Int -> IO ()
evictEntries cache count = do
  fileCache <- readTVarIO (acFileCache cache)

  -- Sort by last access time and evict oldest
  let entries = Map.toList fileCache
      sorted = sortByLastAccess entries
      toRemove = take count sorted
      keys = map fst toRemove

  atomically $ modifyTVar' (acFileCache cache) $ \m ->
    foldr Map.delete m keys

  atomically $ modifyTVar' (acStats cache) $ \s ->
    s { csEvictions = csEvictions s + count }

  forM_ keys $ \key ->
    emitEvent cache (CEEvicted $ kcFilePath $ ckComponents key)
  where
    sortByLastAccess :: [(CacheKey, FileCache)] -> [(CacheKey, FileCache)]
    sortByLastAccess = map snd . sortBy compareTime . map withTime

    withTime :: (CacheKey, FileCache) -> (UTCTime, (CacheKey, FileCache))
    withTime entry@(_, fc) = (emLastAccess $ fcMetadata fc, entry)

    compareTime :: (UTCTime, a) -> (UTCTime, a) -> Ordering
    compareTime (t1, _) (t2, _) = compare t1 t2

    sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    sortBy _ [] = []
    sortBy cmp (x:xs) =
      sortBy cmp [y | y <- xs, cmp y x == LT] ++
      [x] ++
      sortBy cmp [y | y <- xs, cmp y x /= LT]
