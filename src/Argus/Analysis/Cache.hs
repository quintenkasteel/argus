{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Analysis.Cache
-- Description : Incremental analysis caching for Argus
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides caching capabilities for incremental analysis,
-- storing analysis results and only re-analyzing changed files.
module Argus.Analysis.Cache
  ( -- * Cache Types
    AnalysisCache (..)
  , CacheEntry (..)
  , CacheKey (..)

    -- * Cache Operations
  , emptyCache
  , loadCache
  , saveCache
  , lookupCache
  , insertCache
  , invalidateCache
  , invalidateFile

    -- * File Hashing
  , hashFile
  , hashContent
  , FileHash

    -- * Incremental Analysis
  , getChangedFiles
  , isFileCached
  , getCachedDiagnostics
  , updateCacheWithResults
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Numeric (showHex)
import System.Directory (doesFileExist, getModificationTime, createDirectoryIfMissing)
import System.FilePath (takeDirectory) -- removed </> - unused

import Argus.Types (Diagnostic(..)) -- removed SrcSpan - unused

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Hash of file contents for change detection
type FileHash = Text

-- | Key for cache lookup
data CacheKey = CacheKey
  { ckFilePath :: FilePath
  , ckFileHash :: FileHash
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A single cache entry with metadata
data CacheEntry = CacheEntry
  { ceFilePath      :: FilePath
  , ceFileHash      :: FileHash
  , ceModTime       :: UTCTime
  , ceDiagnostics   :: [Diagnostic]
  , ceAnalysisTime  :: Double      -- ^ Time taken for analysis in seconds
  , ceArgusVersion  :: Text        -- ^ Version of Argus that created this entry
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The analysis cache
data AnalysisCache = AnalysisCache
  { acEntries       :: Map FilePath CacheEntry
  , acVersion       :: Text        -- ^ Cache format version
  , acCreated       :: UTCTime
  , acLastUpdated   :: UTCTime
  , acTotalFiles    :: Int
  , acTotalHits     :: Int         -- ^ Cache hit count for statistics
  , acTotalMisses   :: Int         -- ^ Cache miss count for statistics
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Cache Creation and Persistence
--------------------------------------------------------------------------------

-- | Create an empty cache
emptyCache :: IO AnalysisCache
emptyCache = do
  now <- getCurrentTime
  pure AnalysisCache
    { acEntries = Map.empty
    , acVersion = "1.0.0"
    , acCreated = now
    , acLastUpdated = now
    , acTotalFiles = 0
    , acTotalHits = 0
    , acTotalMisses = 0
    }

-- | Load cache from disk
loadCache :: FilePath -> IO (Maybe AnalysisCache)
loadCache cachePath = do
  exists <- doesFileExist cachePath
  if not exists
    then pure Nothing
    else do
      result <- try @SomeException $ BL.readFile cachePath
      case result of
        Left _ -> pure Nothing
        Right content -> pure $ decode content

-- | Save cache to disk
saveCache :: FilePath -> AnalysisCache -> IO ()
saveCache cachePath cache = do
  createDirectoryIfMissing True (takeDirectory cachePath)
  now <- getCurrentTime
  let updatedCache = cache { acLastUpdated = now }
  BL.writeFile cachePath (encode updatedCache)

--------------------------------------------------------------------------------
-- Cache Lookups
--------------------------------------------------------------------------------

-- | Look up a file in the cache
lookupCache :: FilePath -> FileHash -> AnalysisCache -> Maybe CacheEntry
lookupCache path fileHash cache =
  case Map.lookup path (acEntries cache) of
    Nothing -> Nothing
    Just entry ->
      if ceFileHash entry == fileHash
        then Just entry
        else Nothing  -- Hash mismatch, file changed

-- | Insert an entry into the cache
insertCache :: CacheEntry -> AnalysisCache -> AnalysisCache
insertCache entry cache = cache
  { acEntries = Map.insert (ceFilePath entry) entry (acEntries cache)
  , acTotalFiles = Map.size newEntries
  }
  where
    newEntries = Map.insert (ceFilePath entry) entry (acEntries cache)

-- | Invalidate entire cache
invalidateCache :: AnalysisCache -> AnalysisCache
invalidateCache cache = cache { acEntries = Map.empty, acTotalFiles = 0 }

-- | Invalidate a specific file in the cache
invalidateFile :: FilePath -> AnalysisCache -> AnalysisCache
invalidateFile path cache = cache
  { acEntries = Map.delete path (acEntries cache)
  , acTotalFiles = Map.size newEntries
  }
  where
    newEntries = Map.delete path (acEntries cache)

--------------------------------------------------------------------------------
-- File Hashing
--------------------------------------------------------------------------------

-- | Hash a file's contents
hashFile :: FilePath -> IO (Either Text FileHash)
hashFile path = do
  result <- try @SomeException $ BS.readFile path
  case result of
    Left err -> pure $ Left $ T.pack $ show err
    Right content -> pure $ Right $ hashContent content

-- | Hash raw content using DJB2 algorithm (fast, non-cryptographic)
hashContent :: BS.ByteString -> FileHash
hashContent content =
  -- DJB2 hash - fast and sufficient for file change detection
  let hashVal = BS.foldl' (\h b -> h * 33 + fromIntegral b) (5381 :: Integer) content
  in T.pack $ showHex (abs hashVal `mod` (2^(64 :: Int))) ""

--------------------------------------------------------------------------------
-- Incremental Analysis
--------------------------------------------------------------------------------

-- | Get list of files that have changed since last analysis
getChangedFiles :: AnalysisCache -> [FilePath] -> IO [FilePath]
getChangedFiles cache files = do
  results <- mapM (checkFileChanged cache) files
  pure $ mapMaybe id results

-- | Check if a single file has changed
checkFileChanged :: AnalysisCache -> FilePath -> IO (Maybe FilePath)
checkFileChanged cache path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing  -- File doesn't exist
    else do
      hashResult <- hashFile path
      case hashResult of
        Left _ -> pure $ Just path  -- Error reading, treat as changed
        Right fileHash ->
          case lookupCache path fileHash cache of
            Nothing -> pure $ Just path  -- Not in cache or hash changed
            Just _ -> pure Nothing       -- Cached and unchanged

-- | Check if a file is cached with current content
isFileCached :: FilePath -> AnalysisCache -> IO Bool
isFileCached path cache = do
  hashResult <- hashFile path
  case hashResult of
    Left _ -> pure False
    Right fileHash ->
      case lookupCache path fileHash cache of
        Nothing -> pure False
        Just _ -> pure True

-- | Get cached diagnostics for a file if available
getCachedDiagnostics :: FilePath -> AnalysisCache -> IO (Maybe [Diagnostic])
getCachedDiagnostics path cache = do
  hashResult <- hashFile path
  case hashResult of
    Left _ -> pure Nothing
    Right fileHash ->
      pure $ fmap ceDiagnostics $ lookupCache path fileHash cache

-- | Update cache with new analysis results
updateCacheWithResults :: AnalysisCache
                       -> [(FilePath, [Diagnostic], Double)]  -- ^ (file, diagnostics, analysis time)
                       -> IO AnalysisCache
updateCacheWithResults cache results = do
  now <- getCurrentTime
  entries <- mapM (mkEntry now) results
  pure $ foldr insertCache cache entries
  where
    mkEntry _now (path, diags, time) = do
      hashResult <- hashFile path
      let fileHash = either (const "") id hashResult
      modTime <- getModificationTime path
      pure CacheEntry
        { ceFilePath = path
        , ceFileHash = fileHash
        , ceModTime = modTime
        , ceDiagnostics = diags
        , ceAnalysisTime = time
        , ceArgusVersion = "1.0.0"
        }

--------------------------------------------------------------------------------
-- Cache Statistics
--------------------------------------------------------------------------------

-- | Record a cache hit (unused top level binding - commented out)
-- recordHit :: AnalysisCache -> AnalysisCache
-- recordHit cache = cache { acTotalHits = acTotalHits cache + 1 }

-- | Record a cache miss (unused top level binding - commented out)
-- recordMiss :: AnalysisCache -> AnalysisCache
-- recordMiss cache = cache { acTotalMisses = acTotalMisses cache + 1 }

-- | Get cache hit rate (unused top level binding - commented out)
-- cacheHitRate :: AnalysisCache -> Double
-- cacheHitRate cache =
--   let total = acTotalHits cache + acTotalMisses cache
--   in if total == 0 then 0 else fromIntegral (acTotalHits cache) / fromIntegral total
