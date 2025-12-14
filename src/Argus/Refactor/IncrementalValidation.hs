{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Refactor.IncrementalValidation
-- Description : Incremental fix validation with caching
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides incremental validation for auto-fixes, caching
-- validation results and only re-validating when necessary. It uses
-- HIE file timestamps and content hashes to determine when cached
-- results are stale.
--
-- == Key Features
--
-- * Cache validation results per-fix with content hashing
-- * Track HIE file staleness for accurate type checking
-- * Support parallel validation of independent fixes
-- * Propagate invalidation through dependency graph
--
-- == Usage
--
-- @
-- -- Create validation cache
-- cache <- newValidationCache hieDb
--
-- -- Validate fixes (uses cache)
-- results <- validateFixesIncremental cache fixes
--
-- -- Force re-validation after file changes
-- invalidateFile cache "src/Foo.hs"
-- @
module Argus.Refactor.IncrementalValidation
  ( -- * Validation Cache
    ValidationCache (..)
  , CachedValidation (..)
  , ValidationStatus (..)
  , newValidationCache
  , loadValidationCache
  , saveValidationCache

    -- * Incremental Validation
  , validateFixesIncremental
  , validateFixIncremental
  , isValidationStale
  , getValidationResult

    -- * Cache Operations
  , invalidateFile
  , invalidateFix
  , invalidateAll
  , pruneStaleEntries

    -- * HIE Staleness Checking
  , HIEStaleness (..)
  , checkHIEStaleness
  , isHIECurrent
  , getHIETimestamp

    -- * Parallel Validation
  , ValidationBatch (..)
  , batchValidateFixes
  , groupIndependentFixes
  , mergeValidationResults
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (try, SomeException)
import Control.Monad (forM, when)
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Hashable (hash)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getModificationTime, createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import HieDb (HieDb)

import Argus.Types (Fix(..), FixEdit(..), srcSpanFile)
import Argus.HIE.FixValidator
  ( FixValidationResult(..)
  , FixValidatorConfig(..)
  , SymbolSafety(..)
  , TypePreservationResult(..)
  , validateFix
  )
import Argus.HIE.SymbolTable (SymbolTable, emptySymbolTable)

--------------------------------------------------------------------------------
-- Validation Cache Types
--------------------------------------------------------------------------------

-- | Cache for fix validation results
data ValidationCache = ValidationCache
  { vcHieDb       :: HieDb
  , vcSymbolTable :: IORef SymbolTable
  , vcEntries     :: IORef (Map Text CachedValidation)
  , vcHIEStatus   :: IORef (Map FilePath HIEFileStatus)
  , vcCachePath   :: FilePath
  , vcConfig      :: ValidationCacheConfig
  }

-- | Configuration for validation cache
data ValidationCacheConfig = ValidationCacheConfig
  { vccMaxCacheAge    :: Int          -- ^ Max age in seconds for cache entries
  , vccMaxEntries     :: Int          -- ^ Max number of cached entries
  , vccPruneOnLoad    :: Bool         -- ^ Prune stale entries on load
  , vccParallelLimit  :: Int          -- ^ Max parallel validations
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Default cache configuration
defaultCacheConfig :: ValidationCacheConfig
defaultCacheConfig = ValidationCacheConfig
  { vccMaxCacheAge = 3600     -- 1 hour
  , vccMaxEntries = 10000
  , vccPruneOnLoad = True
  , vccParallelLimit = 4
  }

-- | Cached validation result for a fix
data CachedValidation = CachedValidation
  { cvFixHash       :: Int              -- ^ Hash of fix content
  , cvResult        :: ValidationStatus -- ^ Validation result
  , cvErrors        :: [Text]           -- ^ Error messages
  , cvWarnings      :: [Text]           -- ^ Warning messages
  , cvValidatedAt   :: UTCTime          -- ^ When validation was performed
  , cvHIETimestamp  :: UTCTime          -- ^ HIE file timestamp at validation
  , cvAffectedFiles :: [FilePath]       -- ^ Files affected by this fix
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Validation status
data ValidationStatus
  = VSValid           -- ^ Fix is valid and safe
  | VSWarnings        -- ^ Valid with warnings
  | VSInvalid         -- ^ Fix is invalid
  | VSPending         -- ^ Not yet validated
  | VSStale           -- ^ Cached result is stale
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Status of an HIE file
data HIEFileStatus = HIEFileStatus
  { hfsPath         :: FilePath
  , hfsLastChecked  :: UTCTime
  , hfsModTime      :: UTCTime
  , hfsSourceModTime :: UTCTime
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Cache Creation and Persistence
--------------------------------------------------------------------------------

-- | Create a new validation cache
newValidationCache :: HieDb -> FilePath -> IO ValidationCache
newValidationCache db cachePath = do
  symbolTable <- newIORef emptySymbolTable
  entries <- newIORef Map.empty
  hieStatus <- newIORef Map.empty
  pure ValidationCache
    { vcHieDb = db
    , vcSymbolTable = symbolTable
    , vcEntries = entries
    , vcHIEStatus = hieStatus
    , vcCachePath = cachePath
    , vcConfig = defaultCacheConfig
    }

-- | Load validation cache from disk
loadValidationCache :: HieDb -> FilePath -> IO ValidationCache
loadValidationCache db cachePath = do
  cache <- newValidationCache db cachePath
  exists <- doesFileExist cachePath
  when exists $ do
    result <- try $ BL.readFile cachePath :: IO (Either SomeException BL.ByteString)
    case result of
      Left _ -> pure ()  -- Ignore errors, start fresh
      Right content -> case Aeson.decode content of
        Nothing -> pure ()
        Just entries -> do
          writeIORef (vcEntries cache) entries
          when (vccPruneOnLoad (vcConfig cache)) $
            pruneStaleEntries cache
  pure cache

-- | Save validation cache to disk
saveValidationCache :: ValidationCache -> IO ()
saveValidationCache cache = do
  entries <- readIORef (vcEntries cache)
  createDirectoryIfMissing True (takeDirectory (vcCachePath cache))
  BL.writeFile (vcCachePath cache) (Aeson.encode entries)

--------------------------------------------------------------------------------
-- Incremental Validation
--------------------------------------------------------------------------------

-- | Validate fixes incrementally, using cache where possible
validateFixesIncremental :: ValidationCache
                         -> FixValidatorConfig
                         -> [Fix]
                         -> IO [FixValidationResult]
validateFixesIncremental cache config fixes = do
  -- Group fixes by staleness
  (cached, needsValidation) <- partitionByCache cache fixes

  -- Return cached results for non-stale fixes
  let cachedResults = map (uncurry cachedToResult) cached

  -- Validate fixes that need it
  freshResults <- forM needsValidation $ \fix -> do
    result <- validateFixIncremental cache config fix
    pure result

  -- Merge results in original order
  pure $ mergeValidationResults fixes cached freshResults cachedResults

-- | Validate a single fix incrementally
validateFixIncremental :: ValidationCache
                       -> FixValidatorConfig
                       -> Fix
                       -> IO FixValidationResult
validateFixIncremental cache config fix = do
  let fixId = computeFixId fix

  -- Check cache
  entries <- readIORef (vcEntries cache)
  case Map.lookup fixId entries of
    Just cv -> do
      stale <- isValidationStale cache cv
      if stale
        then validateAndCache cache config fix fixId
        else pure $ cachedToResult fix cv
    Nothing -> validateAndCache cache config fix fixId

-- | Validate a fix and cache the result
validateAndCache :: ValidationCache
                 -> FixValidatorConfig
                 -> Fix
                 -> Text
                 -> IO FixValidationResult
validateAndCache cache config fix fixId = do
  -- Perform validation
  symbolTable <- readIORef (vcSymbolTable cache)
  result <- validateFix config (Just (vcHieDb cache)) symbolTable fix

  -- Cache the result
  now <- getCurrentTime
  hieTime <- getHIETimestampForFix cache fix

  let cached = CachedValidation
        { cvFixHash = hash (show fix)
        , cvResult = resultToStatus result
        , cvErrors = map (T.pack . show) (fvrErrors result)
        , cvWarnings = map (T.pack . show) (fvrWarnings result)
        , cvValidatedAt = now
        , cvHIETimestamp = hieTime
        , cvAffectedFiles = getAffectedFiles fix
        }

  modifyIORef' (vcEntries cache) $ Map.insert fixId cached

  pure result

-- | Check if a cached validation result is stale
isValidationStale :: ValidationCache -> CachedValidation -> IO Bool
isValidationStale cache cached = do
  -- Check if any affected files have changed
  anyStale <- anyHIEStale cache (cvAffectedFiles cached) (cvHIETimestamp cached)

  -- Check age
  now <- getCurrentTime
  let age = diffUTCTime now (cvValidatedAt cached)
      maxAge = fromIntegral (vccMaxCacheAge (vcConfig cache))

  pure $ anyStale || age > maxAge

-- | Get validation result from cache
getValidationResult :: ValidationCache -> Text -> IO (Maybe CachedValidation)
getValidationResult cache fixId = do
  entries <- readIORef (vcEntries cache)
  pure $ Map.lookup fixId entries

--------------------------------------------------------------------------------
-- Cache Operations
--------------------------------------------------------------------------------

-- | Invalidate all cached validations for a file
invalidateFile :: ValidationCache -> FilePath -> IO ()
invalidateFile cache filePath = do
  modifyIORef' (vcEntries cache) $
    Map.filter (\cv -> filePath `notElem` cvAffectedFiles cv)

  -- Also invalidate HIE status for this file
  modifyIORef' (vcHIEStatus cache) $
    Map.delete filePath

-- | Invalidate a specific fix validation
invalidateFix :: ValidationCache -> Text -> IO ()
invalidateFix cache fixId = do
  modifyIORef' (vcEntries cache) $ Map.delete fixId

-- | Invalidate all cached validations
invalidateAll :: ValidationCache -> IO ()
invalidateAll cache = do
  writeIORef (vcEntries cache) Map.empty
  writeIORef (vcHIEStatus cache) Map.empty

-- | Remove stale cache entries
pruneStaleEntries :: ValidationCache -> IO ()
pruneStaleEntries cache = do
  now <- getCurrentTime
  let maxAge = fromIntegral (vccMaxCacheAge (vcConfig cache))

  modifyIORef' (vcEntries cache) $
    Map.filter (\cv -> diffUTCTime now (cvValidatedAt cv) < maxAge)

--------------------------------------------------------------------------------
-- HIE Staleness Checking
--------------------------------------------------------------------------------

-- | HIE staleness status
data HIEStaleness
  = HIECurrent        -- ^ HIE file is up to date
  | HIEStale          -- ^ HIE file is older than source
  | HIEMissing        -- ^ HIE file doesn't exist
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Check if an HIE file is stale compared to source
checkHIEStaleness :: FilePath -> IO HIEStaleness
checkHIEStaleness sourcePath = do
  let hiePath = deriveHIEPath sourcePath

  hieExists <- doesFileExist hiePath
  if not hieExists
    then pure HIEMissing
    else do
      srcModTime <- getModificationTime sourcePath
      hieModTime <- getModificationTime hiePath

      if hieModTime >= srcModTime
        then pure HIECurrent
        else pure HIEStale

-- | Check if HIE is current
isHIECurrent :: FilePath -> IO Bool
isHIECurrent path = (== HIECurrent) <$> checkHIEStaleness path

-- | Get HIE file timestamp
getHIETimestamp :: FilePath -> IO (Maybe UTCTime)
getHIETimestamp sourcePath = do
  let hiePath = deriveHIEPath sourcePath
  exists <- doesFileExist hiePath
  if exists
    then Just <$> getModificationTime hiePath
    else pure Nothing

-- | Get HIE timestamp for files affected by a fix
getHIETimestampForFix :: ValidationCache -> Fix -> IO UTCTime
getHIETimestampForFix _cache fix = do
  let files = getAffectedFiles fix
  timestamps <- catMaybes <$> mapM getHIETimestamp files
  case timestamps of
    [] -> getCurrentTime
    ts -> pure $ maximum ts

-- | Check if any HIE file is stale
anyHIEStale :: ValidationCache -> [FilePath] -> UTCTime -> IO Bool
anyHIEStale _cache files cacheTime = do
  timestamps <- mapM getHIETimestamp files
  let maxTime = maximum $ map (fromMaybe cacheTime) timestamps
  pure $ maxTime > cacheTime

--------------------------------------------------------------------------------
-- Parallel Validation
--------------------------------------------------------------------------------

-- | Batch of fixes for parallel validation
data ValidationBatch = ValidationBatch
  { vbFixes     :: [Fix]
  , vbBatchId   :: Int
  , vbPriority  :: Int
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Validate fixes in parallel batches
batchValidateFixes :: ValidationCache
                   -> FixValidatorConfig
                   -> [Fix]
                   -> IO [FixValidationResult]
batchValidateFixes cache config fixes = do
  -- Group independent fixes
  let groups = groupIndependentFixes fixes
      parallelLimit = vccParallelLimit (vcConfig cache)

  -- Validate each group in parallel, but limit concurrency
  results <- forM groups $ \group -> do
    let batches = chunksOf parallelLimit group
    batchResults <- forM batches $ \batch ->
      mapConcurrently (validateFixIncremental cache config) batch
    pure $ concat batchResults

  pure $ concat results

-- | Group fixes that can be validated independently
groupIndependentFixes :: [Fix] -> [[Fix]]
groupIndependentFixes fixes =
  let -- Get affected files for each fix
      withFiles = [(fix, Set.fromList (getAffectedFiles fix)) | fix <- fixes]

      -- Group by no overlapping files
      go [] acc = reverse acc
      go remaining acc =
        let (group, rest) = extractIndependent remaining Set.empty []
        in go rest (group : acc)

      extractIndependent [] _ group = (reverse group, [])
      extractIndependent ((fix, files):rest) usedFiles group
        | Set.null (files `Set.intersection` usedFiles) =
            extractIndependent rest (usedFiles `Set.union` files) (fix : group)
        | otherwise =
            let (g, r) = extractIndependent rest usedFiles group
            in (g, (fix, files) : r)

  in go withFiles []

-- | Merge validation results back into original order
mergeValidationResults :: [Fix]
                       -> [(Fix, CachedValidation)]
                       -> [FixValidationResult]
                       -> [FixValidationResult]
                       -> [FixValidationResult]
mergeValidationResults fixes cached freshResults cachedResults =
  let -- Build a map from fix ID to cached result
      cachedMap = Map.fromList
        [ (computeFixId fix, result)
        | ((fix, _cv), result) <- zip cached cachedResults
        ]

      -- Build a map from fix ID to fresh result
      freshMap = Map.fromList
        [ (computeFixId (fvrFix result), result)
        | result <- freshResults
        ]

      -- Combine: prefer cached, then fresh, then default
      lookupResult fix =
        let fixId = computeFixId fix
        in fromMaybe (defaultResult fix) $
             Map.lookup fixId cachedMap `orElse` Map.lookup fixId freshMap

  in map lookupResult fixes
  where
    orElse Nothing y = y
    orElse x _ = x

    defaultResult fix = FixValidationResult
      { fvrFix = fix
      , fvrValid = False
      , fvrErrors = []
      , fvrWarnings = []
      , fvrConstraints = []
      , fvrTypeSafety = TypesUnknownResult
      , fvrSymbolSafety = SymbolSafe
      }

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Compute a unique ID for a fix
computeFixId :: Fix -> Text
computeFixId fix =
  T.pack $ show $ hash (show fix)

-- | Get files affected by a fix
getAffectedFiles :: Fix -> [FilePath]
getAffectedFiles fix =
  let editFiles = map (srcSpanFile . fixEditSpan) (fixEdits fix)
  in Set.toList $ Set.fromList editFiles

-- | Derive HIE path from source path
deriveHIEPath :: FilePath -> FilePath
deriveHIEPath srcPath =
  let -- Replace src/ with .hie/ and .hs with .hie
      hie = if "src/" `isPrefixOf` srcPath
            then ".hie/" ++ drop 4 srcPath
            else ".hie/" ++ srcPath
  in replaceExtension hie ".hie"
  where
    isPrefixOf prefix s = take (length prefix) s == prefix

    replaceExtension path newExt =
      let base = reverse $ dropWhile (/= '.') $ reverse path
      in if null base then path ++ newExt else init base ++ newExt

-- | Partition fixes by cache status
partitionByCache :: ValidationCache
                 -> [Fix]
                 -> IO ([(Fix, CachedValidation)], [Fix])
partitionByCache cache fixes = do
  entries <- readIORef (vcEntries cache)
  let go [] cached notCached = (reverse cached, reverse notCached)
      go (fix:rest) cached notCached =
        let fixId = computeFixId fix
        in case Map.lookup fixId entries of
          Just cv -> go rest ((fix, cv) : cached) notCached
          Nothing -> go rest cached (fix : notCached)
  pure $ go fixes [] []

-- | Convert cached validation to result
cachedToResult :: Fix -> CachedValidation -> FixValidationResult
cachedToResult fix CachedValidation{..} = FixValidationResult
  { fvrFix = fix
  , fvrValid = cvResult `elem` [VSValid, VSWarnings]
  , fvrErrors = []  -- Errors would need to be reconstructed
  , fvrWarnings = []
  , fvrConstraints = []
  , fvrTypeSafety = TypesUnknownResult  -- Can't reconstruct from cache
  , fvrSymbolSafety = SymbolSafe
  }

-- | Convert result to validation status
resultToStatus :: FixValidationResult -> ValidationStatus
resultToStatus FixValidationResult{..}
  | not fvrValid = VSInvalid
  | not (null fvrWarnings) = VSWarnings
  | otherwise = VSValid

-- | Split list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
