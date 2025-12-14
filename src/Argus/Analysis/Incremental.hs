{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Argus.Analysis.Incremental
-- Description : Incremental analysis engine for efficient re-analysis
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides an incremental analysis engine that tracks file
-- modifications and only re-analyzes changed files, significantly improving
-- performance for large codebases.
module Argus.Analysis.Incremental
  ( -- * Incremental State
    IncrementalState (..)
  , FileState (..)
  , AnalysisCache (..)

    -- * Cache Operations
  , newIncrementalState
  , loadIncrementalState
  , saveIncrementalState

    -- * Analysis Operations
  , getChangedFiles
  , getCachedDiagnostics
  , updateCache
  , invalidateFile
  , invalidateDependents

    -- * Dependency Tracking
  , FileDependencies (..)
  , buildDependencyGraph
  , getDependents
  , getTransitiveDependents
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (ToJSON (..), FromJSON (..), encode, decode, object, (.=), (.:), (.:?), withObject)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getModificationTime, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)

import Argus.Types

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | State of a single file in the cache
data FileState = FileState
  { fsModTime     :: UTCTime           -- ^ Last modification time when analyzed
  , fsContentHash :: Text              -- ^ Hash of file content for change detection
  , fsDiagnostics :: [Diagnostic]      -- ^ Cached diagnostics
  , fsDependsOn   :: Set FilePath      -- ^ Files this file imports/depends on
  , fsAnalyzedAt  :: UTCTime           -- ^ When the file was last analyzed
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FileState where
  toJSON FileState{..} = object
    [ "modTime"     .= utcTimeToPOSIXSeconds fsModTime
    , "contentHash" .= fsContentHash
    , "diagnostics" .= fsDiagnostics
    , "dependsOn"   .= Set.toList fsDependsOn
    , "analyzedAt"  .= utcTimeToPOSIXSeconds fsAnalyzedAt
    ]

instance FromJSON FileState where
  parseJSON = withObject "FileState" $ \v -> do
    modTimeSeconds <- v .: "modTime"
    contentHash <- v .: "contentHash"
    diagnostics <- v .: "diagnostics"
    dependsOnList <- v .:? "dependsOn"
    analyzedAtSeconds <- v .: "analyzedAt"
    pure FileState
      { fsModTime = posixSecondsToUTCTime modTimeSeconds
      , fsContentHash = contentHash
      , fsDiagnostics = diagnostics
      , fsDependsOn = maybe Set.empty Set.fromList dependsOnList
      , fsAnalyzedAt = posixSecondsToUTCTime analyzedAtSeconds
      }

-- | Analysis cache containing per-file state
data AnalysisCache = AnalysisCache
  { acFiles       :: Map FilePath FileState  -- ^ Per-file cached state
  , acVersion     :: Int                      -- ^ Cache format version
  , acConfigHash  :: Text                     -- ^ Hash of configuration (invalidate if changed)
  , acLastUpdated :: UTCTime                  -- ^ When cache was last updated
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON AnalysisCache where
  toJSON AnalysisCache{..} = object
    [ "files"       .= acFiles
    , "version"     .= acVersion
    , "configHash"  .= acConfigHash
    , "lastUpdated" .= utcTimeToPOSIXSeconds acLastUpdated
    ]

instance FromJSON AnalysisCache where
  parseJSON = withObject "AnalysisCache" $ \v -> do
    files <- v .: "files"
    version <- v .: "version"
    configHash <- v .: "configHash"
    lastUpdatedSeconds <- v .: "lastUpdated"
    pure AnalysisCache
      { acFiles = files
      , acVersion = version
      , acConfigHash = configHash
      , acLastUpdated = posixSecondsToUTCTime lastUpdatedSeconds
      }

-- | Incremental analysis state
data IncrementalState = IncrementalState
  { isCache         :: AnalysisCache           -- ^ The analysis cache
  , isCachePath     :: FilePath                -- ^ Path to cache file
  , isDependencyMap :: Map FilePath (Set FilePath)  -- ^ Reverse dependency map
  }
  deriving stock (Eq, Show)

-- | File dependencies for a module
data FileDependencies = FileDependencies
  { fdPath    :: FilePath       -- ^ Path to the file
  , fdImports :: Set FilePath   -- ^ Files this file imports
  }
  deriving stock (Eq, Show)

-- | Cache format version
cacheVersion :: Int
cacheVersion = 1

--------------------------------------------------------------------------------
-- Cache Operations
--------------------------------------------------------------------------------

-- | Create a new empty incremental state
newIncrementalState :: FilePath -> Text -> IO IncrementalState
newIncrementalState cachePath configHash = do
  now <- getCurrentTime
  let cache = AnalysisCache
        { acFiles = Map.empty
        , acVersion = cacheVersion
        , acConfigHash = configHash
        , acLastUpdated = now
        }
  pure IncrementalState
    { isCache = cache
    , isCachePath = cachePath
    , isDependencyMap = Map.empty
    }

-- | Load incremental state from disk, or create new if not found or incompatible
loadIncrementalState :: FilePath -> Text -> IO IncrementalState
loadIncrementalState cachePath configHash = do
  exists <- doesFileExist cachePath
  if not exists
    then newIncrementalState cachePath configHash
    else do
      result <- try @SomeException $ BL.readFile cachePath
      case result of
        Left _ -> newIncrementalState cachePath configHash
        Right contents ->
          case decode contents of
            Nothing -> newIncrementalState cachePath configHash
            Just cache
              -- Check version and config compatibility
              | acVersion cache /= cacheVersion ->
                  newIncrementalState cachePath configHash
              | acConfigHash cache /= configHash ->
                  newIncrementalState cachePath configHash
              | otherwise -> do
                  let depMap = buildReverseDependencyMap (acFiles cache)
                  pure IncrementalState
                    { isCache = cache
                    , isCachePath = cachePath
                    , isDependencyMap = depMap
                    }

-- | Save incremental state to disk
saveIncrementalState :: IncrementalState -> IO ()
saveIncrementalState state = do
  now <- getCurrentTime
  let cache = (isCache state) { acLastUpdated = now }
      path = isCachePath state
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (encode cache)

--------------------------------------------------------------------------------
-- Analysis Operations
--------------------------------------------------------------------------------

-- | Get list of files that need re-analysis
getChangedFiles :: IncrementalState -> [FilePath] -> IO [FilePath]
getChangedFiles state files = do
  -- Check each file to see if it has changed
  changedList <- mapM (checkFileChanged (acFiles (isCache state))) files
  pure $ concat changedList
  where
    checkFileChanged :: Map FilePath FileState -> FilePath -> IO [FilePath]
    checkFileChanged cache path = do
      exists <- doesFileExist path
      if not exists
        then pure []  -- Skip non-existent files
        else case Map.lookup path cache of
          Nothing -> pure [path]  -- Not in cache, needs analysis
          Just fs -> do
            currentModTime <- getModificationTime path
            if currentModTime > fsModTime fs
              then pure [path]  -- Modified since last analysis
              else pure []      -- No change

-- | Get cached diagnostics for a file (if still valid)
getCachedDiagnostics :: IncrementalState -> FilePath -> IO (Maybe [Diagnostic])
getCachedDiagnostics state path = do
  case Map.lookup path (acFiles (isCache state)) of
    Nothing -> pure Nothing
    Just fs -> do
      exists <- doesFileExist path
      if not exists
        then pure Nothing
        else do
          currentModTime <- getModificationTime path
          if currentModTime > fsModTime fs
            then pure Nothing  -- File changed, cache invalid
            else pure $ Just (fsDiagnostics fs)

-- | Update cache with new analysis results for a file
updateCache :: IncrementalState -> FilePath -> [Diagnostic] -> Set FilePath -> Text -> IO IncrementalState
updateCache state path diagnostics dependencies contentHash = do
  now <- getCurrentTime
  modTime <- getModificationTime path

  let fileState = FileState
        { fsModTime = modTime
        , fsContentHash = contentHash
        , fsDiagnostics = diagnostics
        , fsDependsOn = dependencies
        , fsAnalyzedAt = now
        }

      newFiles = Map.insert path fileState (acFiles (isCache state))
      newCache = (isCache state) { acFiles = newFiles }
      newDepMap = updateReverseDependency (isDependencyMap state) path dependencies

  pure state
    { isCache = newCache
    , isDependencyMap = newDepMap
    }

-- | Invalidate cache for a specific file
invalidateFile :: IncrementalState -> FilePath -> IncrementalState
invalidateFile state path =
  let newFiles = Map.delete path (acFiles (isCache state))
      newCache = (isCache state) { acFiles = newFiles }
  in state { isCache = newCache }

-- | Invalidate a file and all files that depend on it
invalidateDependents :: IncrementalState -> FilePath -> IncrementalState
invalidateDependents state path =
  let dependents = getTransitiveDependents state path
      allToInvalidate = Set.insert path dependents
      newFiles = foldr Map.delete (acFiles (isCache state)) (Set.toList allToInvalidate)
      newCache = (isCache state) { acFiles = newFiles }
  in state { isCache = newCache }

--------------------------------------------------------------------------------
-- Dependency Tracking
--------------------------------------------------------------------------------

-- | Build a dependency graph from a list of file dependencies
buildDependencyGraph :: [FileDependencies] -> Map FilePath (Set FilePath)
buildDependencyGraph deps =
  Map.fromList [(fdPath d, fdImports d) | d <- deps]

-- | Get direct dependents of a file (files that import it)
getDependents :: IncrementalState -> FilePath -> Set FilePath
getDependents state path =
  Map.findWithDefault Set.empty path (isDependencyMap state)

-- | Get transitive dependents of a file
getTransitiveDependents :: IncrementalState -> FilePath -> Set FilePath
getTransitiveDependents state path =
  go Set.empty (Set.singleton path)
  where
    go visited frontier
      | Set.null frontier = visited
      | otherwise =
          let newDeps = Set.unions [getDependents state p | p <- Set.toList frontier]
              unvisited = Set.difference newDeps visited
          in go (Set.union visited frontier) unvisited

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Build reverse dependency map (file -> files that depend on it)
buildReverseDependencyMap :: Map FilePath FileState -> Map FilePath (Set FilePath)
buildReverseDependencyMap fileStates =
  Map.foldrWithKey addReverseDeps Map.empty fileStates
  where
    addReverseDeps :: FilePath -> FileState -> Map FilePath (Set FilePath) -> Map FilePath (Set FilePath)
    addReverseDeps path fs acc =
      foldr (addDep path) acc (Set.toList (fsDependsOn fs))

    addDep :: FilePath -> FilePath -> Map FilePath (Set FilePath) -> Map FilePath (Set FilePath)
    addDep dependent dependency acc =
      Map.insertWith Set.union dependency (Set.singleton dependent) acc

-- | Update reverse dependency map when a file's dependencies change
updateReverseDependency :: Map FilePath (Set FilePath) -> FilePath -> Set FilePath -> Map FilePath (Set FilePath)
updateReverseDependency depMap path newDeps =
  -- Add path as dependent of each of its dependencies
  foldr addDep depMap (Set.toList newDeps)
  where
    addDep dependency acc =
      Map.insertWith Set.union dependency (Set.singleton path) acc
