{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Argus.HIE.Incremental
-- Description : Incremental HIE file watching and analysis
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
-- Portability : GHC
--
-- = Overview
--
-- This module provides incremental HIE file watching capabilities for efficient
-- re-analysis when HIE files change. It tracks HIE file modifications, maintains
-- a dependency graph, and intelligently invalidates only affected files.
--
-- = Architecture
--
-- @
-- ┌────────────────────────────────────────────────────────────────────┐
-- │                   HIEIncrementalState                              │
-- │  ┌────────────────────┐  ┌───────────────────────────────────┐    │
-- │  │    HIEFileState    │  │       Dependency Graph            │    │
-- │  │  - Path            │  │  Module A ──► imports ──► [B, C]  │    │
-- │  │  - ModTime         │  │  Module B ──► imports ──► [D]     │    │
-- │  │  - ContentHash     │  │                                   │    │
-- │  │  - Imports         │  │  Reverse: D depended on by [B]    │    │
-- │  │  - Diagnostics     │  │           B depended on by [A]    │    │
-- │  └────────────────────┘  └───────────────────────────────────┘    │
-- └────────────────────────────────────────────────────────────────────┘
-- @
--
-- = Features
--
-- * Tracks HIE file modification times and content hashes
-- * Builds dependency graphs from module imports
-- * Invalidates transitive dependents when a HIE file changes
-- * Caches HIE analysis results for unchanged files
-- * Integrates with file system watching
--
-- = Incremental Analysis Flow
--
-- @
-- File Change ──► Detect Changes ──► Invalidate Dependents ──► Re-analyze
--                      │                     │
--                      ▼                     ▼
--               Compare ModTime        Build Transitive
--               Check Hash             Closure
-- @
--
-- = File System Monitoring
--
-- The 'watchHIEDirectory' function provides polling-based monitoring:
--
-- * Tracks modification times for change detection (avoids false positives)
-- * Notifies on created, modified, and deleted HIE files
-- * Returns a stop action for cleanup
--
-- = Thread Safety
--
-- 'HIEIncrementalState' is immutable; modifications return new states.
-- File system watching uses IORef internally and should be used from
-- a single thread.
--
-- @since 1.0.0
module Argus.HIE.Incremental
  ( -- * Incremental State
    HIEIncrementalState (..)
  , HIEFileState (..)
  , HIEModuleInfo (..)

    -- * State Operations
  , newHIEIncrementalState
  , loadHIEIncrementalState
  , saveHIEIncrementalState

    -- * Change Detection
  , detectHIEChanges
  , getChangedHIEFiles
  , invalidateHIEFile
  , invalidateHIEDependents

    -- * Dependency Tracking
  , buildHIEDependencyGraph
  , getHIEDependents
  , getTransitiveHIEDependents
  , extractHIEDependencies

    -- * Analysis Integration
  , analyzeHIEIncremental
  , getCachedHIEAnalysis
  , updateHIECache

    -- * File System Monitoring
  , HIEWatchEvent (..)
  , watchHIEDirectory
  , scanHIEFiles
  ) where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Exception (try, SomeException)
import Control.Monad (when, forM, forM_, filterM)
import Data.Maybe (mapMaybe)
import Crypto.Hash (SHA256(..), hashWith)
import Data.Aeson (ToJSON (..), FromJSON (..), encode, decode, object, (.=), (.:), (.:?), withObject)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import System.Directory
  ( doesFileExist
  , doesDirectoryExist
  , listDirectory
  , getModificationTime
  , createDirectoryIfMissing
  )
import System.FilePath ((</>), takeExtension, takeDirectory)

-- GHC API for HIE file reading
import "ghc" GHC.Iface.Ext.Binary (readHieFile, hie_file_result)
import "ghc" GHC.Iface.Ext.Types
  ( HieFile(..)
  , HieASTs(..)
  , HieAST(..)
  , NodeInfo(..)
  , getSourcedNodeInfo
  )
import "ghc" GHC.Types.Name (Name, nameModule_maybe)
import "ghc" GHC.Unit.Module (moduleNameString, moduleName)

import HieDb.Utils (makeNc)

import Argus.Types (Diagnostic)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | State of a single HIE file
data HIEFileState = HIEFileState
  { hfsPath         :: FilePath           -- ^ Path to the HIE file
  , hfsModuleName   :: Text               -- ^ Module name from HIE file
  , hfsModTime      :: UTCTime            -- ^ Last modification time
  , hfsContentHash  :: Text               -- ^ SHA256 hash of HIE file content
  , hfsImports      :: Set Text           -- ^ Module names this module imports
  , hfsDiagnostics  :: [Diagnostic]       -- ^ Cached analysis results
  , hfsAnalyzedAt   :: UTCTime            -- ^ When analysis was performed
  , hfsSourceFile   :: Maybe FilePath     -- ^ Corresponding source file
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON HIEFileState where
  toJSON HIEFileState{..} = object
    [ "path"         .= hfsPath
    , "moduleName"   .= hfsModuleName
    , "modTime"      .= utcTimeToPOSIXSeconds hfsModTime
    , "contentHash"  .= hfsContentHash
    , "imports"      .= Set.toList hfsImports
    , "diagnostics"  .= hfsDiagnostics
    , "analyzedAt"   .= utcTimeToPOSIXSeconds hfsAnalyzedAt
    , "sourceFile"   .= hfsSourceFile
    ]

instance FromJSON HIEFileState where
  parseJSON = withObject "HIEFileState" $ \v -> do
    path <- v .: "path"
    moduleName <- v .: "moduleName"
    modTimeSeconds <- v .: "modTime"
    contentHash <- v .: "contentHash"
    importsList <- v .: "imports"
    diagnostics <- v .: "diagnostics"
    analyzedAtSeconds <- v .: "analyzedAt"
    sourceFile <- v .:? "sourceFile"
    pure HIEFileState
      { hfsPath = path
      , hfsModuleName = moduleName
      , hfsModTime = posixSecondsToUTCTime modTimeSeconds
      , hfsContentHash = contentHash
      , hfsImports = Set.fromList importsList
      , hfsDiagnostics = diagnostics
      , hfsAnalyzedAt = posixSecondsToUTCTime analyzedAtSeconds
      , hfsSourceFile = sourceFile
      }

-- | Module information extracted from HIE file
data HIEModuleInfo = HIEModuleInfo
  { hmiModuleName :: Text               -- ^ Module name
  , hmiSourceFile :: Maybe FilePath     -- ^ Source file path
  , hmiImports    :: Set Text           -- ^ Imported modules
  , hmiExports    :: Set Text           -- ^ Exported symbols
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Incremental state for HIE file watching
data HIEIncrementalState = HIEIncrementalState
  { hisFiles           :: Map FilePath HIEFileState      -- ^ HIE file states
  , hisDependencyMap   :: Map Text (Set Text)            -- ^ Module -> modules that import it
  , hisModuleToPath    :: Map Text FilePath              -- ^ Module name -> HIE file path
  , hisCachePath       :: FilePath                       -- ^ Path to cache file
  , hisVersion         :: Int                            -- ^ Cache format version
  , hisLastUpdated     :: UTCTime                        -- ^ Last cache update
  }
  deriving stock (Eq, Show)

-- | HIE watch events
data HIEWatchEvent
  = HIEFileCreated FilePath
  | HIEFileModified FilePath
  | HIEFileDeleted FilePath
  | HIEAnalysisStarted FilePath
  | HIEAnalysisCompleted FilePath [Diagnostic]
  deriving stock (Eq, Show)

-- | Cache format version
hieIncrementalVersion :: Int
hieIncrementalVersion = 1

--------------------------------------------------------------------------------
-- State Operations
--------------------------------------------------------------------------------

-- | Create a new empty HIE incremental state
newHIEIncrementalState :: FilePath -> IO HIEIncrementalState
newHIEIncrementalState cachePath = do
  now <- getCurrentTime
  pure HIEIncrementalState
    { hisFiles = Map.empty
    , hisDependencyMap = Map.empty
    , hisModuleToPath = Map.empty
    , hisCachePath = cachePath
    , hisVersion = hieIncrementalVersion
    , hisLastUpdated = now
    }

-- | Load HIE incremental state from disk
loadHIEIncrementalState :: FilePath -> IO HIEIncrementalState
loadHIEIncrementalState cachePath = do
  exists <- doesFileExist cachePath
  if not exists
    then newHIEIncrementalState cachePath
    else do
      result <- try @SomeException $ BL.readFile cachePath
      case result of
        Left _ -> newHIEIncrementalState cachePath
        Right contents ->
          case decode contents of
            Nothing -> newHIEIncrementalState cachePath
            Just state@HIEIncrementalState{..}
              | hisVersion /= hieIncrementalVersion ->
                  newHIEIncrementalState cachePath
              | otherwise ->
                  pure state

-- | Save HIE incremental state to disk
saveHIEIncrementalState :: HIEIncrementalState -> IO ()
saveHIEIncrementalState state = do
  now <- getCurrentTime
  let state' = state { hisLastUpdated = now }
      path = hisCachePath state'
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (encode state')

instance ToJSON HIEIncrementalState where
  toJSON HIEIncrementalState{..} = object
    [ "files"         .= hisFiles
    , "dependencyMap" .= Map.mapKeys T.unpack hisDependencyMap
    , "moduleToPath"  .= Map.mapKeys T.unpack hisModuleToPath
    , "version"       .= hisVersion
    , "lastUpdated"   .= utcTimeToPOSIXSeconds hisLastUpdated
    ]

instance FromJSON HIEIncrementalState where
  parseJSON = withObject "HIEIncrementalState" $ \v -> do
    files <- v .: "files"
    depMapRaw <- v .: "dependencyMap"
    modToPathRaw <- v .: "moduleToPath"
    version <- v .: "version"
    lastUpdatedSeconds <- v .: "lastUpdated"
    cachePath <- pure ".argus-hie-cache"  -- Default, will be overridden
    pure HIEIncrementalState
      { hisFiles = files
      , hisDependencyMap = Map.mapKeys T.pack depMapRaw
      , hisModuleToPath = Map.mapKeys T.pack modToPathRaw
      , hisCachePath = cachePath
      , hisVersion = version
      , hisLastUpdated = posixSecondsToUTCTime lastUpdatedSeconds
      }

--------------------------------------------------------------------------------
-- Change Detection
--------------------------------------------------------------------------------

-- | Detect which HIE files have changed since last analysis
detectHIEChanges :: HIEIncrementalState -> [FilePath] -> IO [FilePath]
detectHIEChanges state hiePaths = do
  changed <- forM hiePaths $ \path -> do
    exists <- doesFileExist path
    if not exists
      then pure Nothing
      else case Map.lookup path (hisFiles state) of
        Nothing -> pure (Just path)  -- New file
        Just hfs -> do
          currentModTime <- getModificationTime path
          if currentModTime > hfsModTime hfs
            then pure (Just path)  -- Modified
            else pure Nothing      -- Unchanged
  pure [p | Just p <- changed]

-- | Get list of HIE files that need re-analysis
getChangedHIEFiles :: HIEIncrementalState -> [FilePath] -> IO [FilePath]
getChangedHIEFiles = detectHIEChanges

-- | Invalidate cache for a specific HIE file
invalidateHIEFile :: HIEIncrementalState -> FilePath -> HIEIncrementalState
invalidateHIEFile state path =
  case Map.lookup path (hisFiles state) of
    Nothing -> state  -- File not in cache
    Just hfs ->
      let moduleName = hfsModuleName hfs
          newFiles = Map.delete path (hisFiles state)
          newModuleToPath = Map.delete moduleName (hisModuleToPath state)
      in state
           { hisFiles = newFiles
           , hisModuleToPath = newModuleToPath
           }

-- | Invalidate a HIE file and all modules that depend on it
invalidateHIEDependents :: HIEIncrementalState -> FilePath -> HIEIncrementalState
invalidateHIEDependents state path =
  case Map.lookup path (hisFiles state) of
    Nothing -> state
    Just hfs ->
      let moduleName = hfsModuleName hfs
          allDependents = getTransitiveHIEDependents state moduleName
          allToInvalidate = Set.insert moduleName allDependents

          -- Find all HIE files for these modules
          pathsToInvalidate = Set.fromList
            [ p
            | (modName, p) <- Map.toList (hisModuleToPath state)
            , modName `Set.member` allToInvalidate
            ]

          newFiles = foldr Map.delete (hisFiles state) (Set.toList pathsToInvalidate)
          newModuleToPath = foldr Map.delete (hisModuleToPath state) (Set.toList allToInvalidate)
      in state
           { hisFiles = newFiles
           , hisModuleToPath = newModuleToPath
           }

--------------------------------------------------------------------------------
-- Dependency Tracking
--------------------------------------------------------------------------------

-- | Build dependency graph from HIE file states
buildHIEDependencyGraph :: Map FilePath HIEFileState -> Map Text (Set Text)
buildHIEDependencyGraph fileStates =
  Map.foldr addReverseDeps Map.empty fileStates
  where
    addReverseDeps :: HIEFileState -> Map Text (Set Text) -> Map Text (Set Text)
    addReverseDeps hfs acc =
      let moduleName = hfsModuleName hfs
          imports = hfsImports hfs
      in foldr (addDep moduleName) acc (Set.toList imports)

    addDep :: Text -> Text -> Map Text (Set Text) -> Map Text (Set Text)
    addDep importer imported acc =
      Map.insertWith Set.union imported (Set.singleton importer) acc

-- | Get modules that directly depend on the given module
getHIEDependents :: HIEIncrementalState -> Text -> Set Text
getHIEDependents state moduleName =
  Map.findWithDefault Set.empty moduleName (hisDependencyMap state)

-- | Get all modules that transitively depend on the given module
getTransitiveHIEDependents :: HIEIncrementalState -> Text -> Set Text
getTransitiveHIEDependents state moduleName =
  go Set.empty (Set.singleton moduleName)
  where
    go visited frontier
      | Set.null frontier = visited
      | otherwise =
          let newDeps = Set.unions
                [ getHIEDependents state m
                | m <- Set.toList frontier
                ]
              unvisited = Set.difference newDeps visited
          in go (Set.union visited frontier) unvisited

-- | Extract module information from a HIE file
extractHIEDependencies :: FilePath -> IO (Either Text HIEModuleInfo)
extractHIEDependencies hiePath = do
  result <- try @SomeException $ do
    nc <- makeNc
    hieResult <- readHieFile nc hiePath
    let hieFile = hie_file_result hieResult
        hieModule = hie_module hieFile
        modName = T.pack $ moduleNameString $ moduleName hieModule
        sourceFile = hie_hs_file hieFile
        -- Extract imports from the HIE file AST
        -- We traverse all nodes and collect modules that identifiers come from
        imports = extractImportsFromHIE hieFile modName
    pure HIEModuleInfo
      { hmiModuleName = modName
      , hmiSourceFile = Just sourceFile
      , hmiImports = imports
      , hmiExports = Set.empty  -- Exports would need deeper parsing
      }
  case result of
    Left err -> pure $ Left $ "Failed to read HIE file: " <> T.pack (show err)
    Right info -> pure $ Right info

-- | Extract imported module names from a HIE file by traversing the AST
-- and collecting all Names that come from external modules
extractImportsFromHIE :: HieFile -> Text -> Set Text
extractImportsFromHIE hieFile selfModuleName =
  let asts = getAsts $ hie_asts hieFile
      -- Collect all identifiers from all ASTs
      allNames = concatMap collectNamesFromAST (Map.elems asts)
      -- Get module names from Names, filtering out our own module
      moduleNames = mapMaybe getModuleFromName allNames
      -- Filter out self-module and GHC internal modules
      externalModules = filter (isExternalModule selfModuleName) moduleNames
  in Set.fromList externalModules

-- | Collect all Names from a HIE AST
collectNamesFromAST :: HieAST a -> [Name]
collectNamesFromAST ast =
  let nodeNames = collectNamesFromNode ast
      childNames = concatMap collectNamesFromAST (nodeChildren ast)
  in nodeNames ++ childNames

-- | Collect Names from a single node
collectNamesFromNode :: HieAST a -> [Name]
collectNamesFromNode ast =
  let snInfo = sourcedNodeInfo ast
      nodeInfoMap = getSourcedNodeInfo snInfo
      -- Get identifiers from all node info sources
      allIdentifiers = concatMap (Map.keys . nodeIdentifiers) (Map.elems nodeInfoMap)
  in [n | Right n <- allIdentifiers]  -- Filter to just Names (not ModuleNames)

-- | Get the module name from a Name (if it has one)
getModuleFromName :: Name -> Maybe Text
getModuleFromName name = do
  mdl <- nameModule_maybe name
  pure $ T.pack $ moduleNameString $ moduleName mdl

-- | Check if a module is external (not self-module, not GHC internal)
isExternalModule :: Text -> Text -> Bool
isExternalModule selfMod modName =
  modName /= selfMod &&
  not (T.isPrefixOf "GHC." modName) &&
  not (T.isPrefixOf "ghc-" modName) &&
  modName /= "Main"

--------------------------------------------------------------------------------
-- Analysis Integration
--------------------------------------------------------------------------------

-- | Analyze a HIE file incrementally (using cache when possible)
analyzeHIEIncremental
  :: HIEIncrementalState
  -> FilePath
  -> (FilePath -> IO [Diagnostic])  -- ^ Analysis function
  -> IO (HIEIncrementalState, [Diagnostic])
analyzeHIEIncremental state hiePath analyzeFunc = do
  -- Check if file needs analysis
  needsAnalysis <- case Map.lookup hiePath (hisFiles state) of
    Nothing -> pure True  -- Not in cache
    Just hfs -> do
      exists <- doesFileExist hiePath
      if not exists
        then pure True
        else do
          currentModTime <- getModificationTime hiePath
          pure (currentModTime > hfsModTime hfs)

  if not needsAnalysis
    then do
      -- Return cached results
      case Map.lookup hiePath (hisFiles state) of
        Just hfs -> pure (state, hfsDiagnostics hfs)
        Nothing -> pure (state, [])  -- Shouldn't happen, but handle gracefully
    else do
      -- Need to analyze
      diagnostics <- analyzeFunc hiePath

      -- Extract module info and update cache
      moduleInfoResult <- extractHIEDependencies hiePath
      case moduleInfoResult of
        Left _err -> pure (state, diagnostics)  -- Keep old state on error
        Right moduleInfo -> do
          contentHash <- hashHIEFile hiePath
          now <- getCurrentTime
          modTime <- getModificationTime hiePath

          let hfs = HIEFileState
                { hfsPath = hiePath
                , hfsModuleName = hmiModuleName moduleInfo
                , hfsModTime = modTime
                , hfsContentHash = contentHash
                , hfsImports = hmiImports moduleInfo
                , hfsDiagnostics = diagnostics
                , hfsAnalyzedAt = now
                , hfsSourceFile = hmiSourceFile moduleInfo
                }

          newState <- updateHIECache state hiePath hfs
          pure (newState, diagnostics)

-- | Get cached analysis for a HIE file (if valid)
getCachedHIEAnalysis :: HIEIncrementalState -> FilePath -> IO (Maybe [Diagnostic])
getCachedHIEAnalysis state hiePath = do
  case Map.lookup hiePath (hisFiles state) of
    Nothing -> pure Nothing
    Just hfs -> do
      exists <- doesFileExist hiePath
      if not exists
        then pure Nothing
        else do
          currentModTime <- getModificationTime hiePath
          if currentModTime > hfsModTime hfs
            then pure Nothing  -- File changed
            else pure $ Just (hfsDiagnostics hfs)

-- | Update cache with new HIE file state
updateHIECache :: HIEIncrementalState -> FilePath -> HIEFileState -> IO HIEIncrementalState
updateHIECache state hiePath hfs = do
  let moduleName = hfsModuleName hfs
      newFiles = Map.insert hiePath hfs (hisFiles state)
      newModuleToPath = Map.insert moduleName hiePath (hisModuleToPath state)
      newDepMap = buildHIEDependencyGraph newFiles

  pure state
    { hisFiles = newFiles
    , hisDependencyMap = newDepMap
    , hisModuleToPath = newModuleToPath
    }

--------------------------------------------------------------------------------
-- File System Monitoring
--------------------------------------------------------------------------------

-- | Watch a directory for HIE file changes
--
-- Uses proper modification time checking to detect real file changes,
-- avoiding false positives from treating all files as modified.
watchHIEDirectory
  :: FilePath                           -- ^ Directory to watch
  -> (HIEWatchEvent -> IO ())           -- ^ Event callback
  -> IO (IO ())                         -- ^ Returns action to stop watching
watchHIEDirectory hieDir callback = do
  -- Track both paths and their modification times
  stateRef <- newIORef (Map.empty :: Map FilePath UTCTime)
  runningRef <- newIORef True

  -- Initial scan with modification times
  initialFiles <- scanHIEFiles hieDir
  initialState <- scanFilesWithModTimes initialFiles
  writeIORef stateRef initialState

  -- Start polling thread
  tid <- forkIO $ watchLoop stateRef runningRef

  -- Return stop action
  pure $ do
    writeIORef runningRef False
    killThread tid

  where
    -- | Scan files and get their modification times
    scanFilesWithModTimes :: [FilePath] -> IO (Map FilePath UTCTime)
    scanFilesWithModTimes paths = do
      modTimes <- forM paths $ \path -> do
        exists <- doesFileExist path
        if exists
          then do
            modTime <- getModificationTime path
            pure (Just (path, modTime))
          else pure Nothing
      pure $ Map.fromList [ (p, t) | Just (p, t) <- modTimes ]

    watchLoop stateRef runningRef = do
      running <- readIORef runningRef
      when running $ do
        -- Wait before next check
        threadDelay 1000000  -- 1 second

        -- Get current files with modification times
        currentFiles <- scanHIEFiles hieDir
        currentState <- scanFilesWithModTimes currentFiles
        knownState <- readIORef stateRef

        let currentPaths = Map.keysSet currentState
            knownPaths = Map.keysSet knownState
            createdPaths = Set.difference currentPaths knownPaths
            deletedPaths = Set.difference knownPaths currentPaths
            existingPaths = Set.intersection currentPaths knownPaths

        -- Check for actual modifications using modification time comparison
        modifiedPaths <- detectModifiedPaths existingPaths currentState knownState

        -- Notify callbacks for created files
        forM_ (Set.toList createdPaths) $ \path ->
          callback (HIEFileCreated path)

        -- Notify callbacks for modified files (only those that actually changed)
        forM_ (Set.toList modifiedPaths) $ \path ->
          callback (HIEFileModified path)

        -- Notify callbacks for deleted files
        forM_ (Set.toList deletedPaths) $ \path ->
          callback (HIEFileDeleted path)

        -- Update state with current modification times
        writeIORef stateRef currentState

        -- Continue loop
        watchLoop stateRef runningRef

    -- | Detect files that have actually been modified by comparing modification times
    detectModifiedPaths
      :: Set FilePath                     -- ^ Paths that exist in both old and new state
      -> Map FilePath UTCTime             -- ^ Current state with mod times
      -> Map FilePath UTCTime             -- ^ Previous state with mod times
      -> IO (Set FilePath)
    detectModifiedPaths existing currentState knownState = do
      modified <- forM (Set.toList existing) $ \path -> do
        let currentModTime = Map.lookup path currentState
            knownModTime = Map.lookup path knownState
        case (currentModTime, knownModTime) of
          (Just curr, Just known) ->
            -- File modified if modification time is later
            pure $ if curr > known then Just path else Nothing
          (Just _, Nothing) ->
            -- File is new to us (shouldn't happen in existing set, but handle it)
            pure $ Just path
          _ ->
            -- File no longer exists (shouldn't happen in existing set)
            pure Nothing
      pure $ Set.fromList [ p | Just p <- modified ]

-- | Scan a directory for HIE files recursively
scanHIEFiles :: FilePath -> IO [FilePath]
scanHIEFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- listDirectory dir
      let paths = map (dir </>) entries

      -- Get HIE files in this directory
      hieFiles <- filterM isHIEFile paths

      -- Recursively scan subdirectories
      subdirs <- filterM doesDirectoryExist paths
      subFiles <- concat <$> mapM scanHIEFiles subdirs

      pure (hieFiles ++ subFiles)
  where
    isHIEFile :: FilePath -> IO Bool
    isHIEFile path = do
      exists <- doesFileExist path
      pure (exists && takeExtension path == ".hie")

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Compute SHA256 hash of a HIE file
hashHIEFile :: FilePath -> IO Text
hashHIEFile path = do
  contents <- BS.readFile path
  let digest = hashWith SHA256 contents
  pure $ T.pack $ show digest
