{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Argus.Watch
-- Description : File system watching for continuous analysis
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides watch mode functionality for continuous analysis.
-- It monitors file system changes and triggers incremental re-analysis
-- when Haskell source files are modified.
module Argus.Watch
  ( -- * Watch Mode
    WatchConfig (..)
  , WatchState (..)
  , WatchEvent (..)
  , WatchCallback
  , defaultWatchConfig

    -- * Running Watch Mode
  , runWatchMode
  , stopWatchMode

    -- * Event Handling
  , defaultWatchCallback

    -- * File Scanning (exported for testing)
  , scanDirectories
  , isHaskellFile
  , matchPattern
  , matchesExclude
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay, killThread)
import Control.Concurrent.STM (TVar, newTVarIO, readTVar, writeTVar, atomically, modifyTVar')
import Control.Exception (try, SomeException)
import Control.Monad (when, unless, forM_)
import Crypto.Hash (SHA256(..), hashWith)
import Data.ByteString.Char8 qualified as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, getModificationTime)
import System.FilePath ((</>), takeExtension)
import System.IO (hFlush, stdout)

import Argus.Analysis.Incremental
import Argus.Config (Config)
import Argus.Core (AnalysisContext, analyzeFile, defaultContext)
import Argus.Rules.ConfigurableRules (defaultRulesConfig)
import Argus.Output.Terminal (renderTerminal)
import Argus.Output.Types (OutputOptions (..), OutputFormat (..))
import Argus.Types
import Argus.HIE.Incremental
  ( HIEIncrementalState
  , loadHIEIncrementalState
  , saveHIEIncrementalState
  , detectHIEChanges
  , invalidateHIEDependents
  , scanHIEFiles
  )

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Watch mode configuration
data WatchConfig = WatchConfig
  { wcDirectories    :: [FilePath]    -- ^ Directories to watch
  , wcExcludePatterns :: [Text]       -- ^ Patterns to exclude
  , wcDebounceMs     :: Int           -- ^ Debounce time in milliseconds
  , wcPollIntervalMs :: Int           -- ^ Polling interval in milliseconds
  , wcClearScreen    :: Bool          -- ^ Clear screen on each update
  , wcShowTimestamp  :: Bool          -- ^ Show timestamp on each update
  , wcVerbose        :: Bool          -- ^ Verbose output
  , wcWatchHIE       :: Bool          -- ^ Watch HIE files for changes
  , wcHIEDirectory   :: Maybe FilePath -- ^ HIE directory to watch (default: .hie)
  }
  deriving stock (Eq, Show)

-- | Default watch configuration
defaultWatchConfig :: WatchConfig
defaultWatchConfig = WatchConfig
  { wcDirectories = ["."]
  , wcExcludePatterns = ["dist-newstyle/**", ".stack-work/**", ".git/**"]
  , wcDebounceMs = 500
  , wcPollIntervalMs = 1000
  , wcClearScreen = True
  , wcShowTimestamp = True
  , wcVerbose = False
  , wcWatchHIE = True
  , wcHIEDirectory = Nothing  -- Use default .hie
  }

-- | Watch state
data WatchState = WatchState
  { wsFileModTimes  :: TVar (Map FilePath UTCTime)  -- ^ Last known modification times
  , wsIncremental   :: TVar IncrementalState        -- ^ Incremental analysis state
  , wsHIEIncremental :: TVar HIEIncrementalState    -- ^ HIE incremental state
  , wsLastAnalysis  :: TVar UTCTime                 -- ^ Time of last analysis run
  , wsPendingFiles  :: TVar (Set FilePath)          -- ^ Files pending analysis
  , wsPendingHIE    :: TVar (Set FilePath)          -- ^ HIE files pending re-analysis
  , wsRunning       :: TVar Bool                    -- ^ Whether watch mode is running
  , wsWatchThread   :: IORef (Maybe ThreadId)       -- ^ Watch thread ID
  , wsHIEWatchThread :: IORef (Maybe ThreadId)      -- ^ HIE watch thread ID
  }

-- | Watch events
data WatchEvent
  = FileModified FilePath
  | FileCreated FilePath
  | FileDeleted FilePath
  | HIEFileModified FilePath
  | HIEFileCreated FilePath
  | HIEFileDeleted FilePath
  | AnalysisStarted [FilePath]
  | AnalysisCompleted [Diagnostic] Float  -- ^ Diagnostics and time taken
  | WatchError Text
  deriving stock (Eq, Show)

-- | Callback for watch events
type WatchCallback = WatchEvent -> IO ()

--------------------------------------------------------------------------------
-- Watch Mode Operations
--------------------------------------------------------------------------------

-- | Run watch mode with the given configuration
runWatchMode :: Config -> WatchConfig -> WatchCallback -> IO WatchState
runWatchMode cfg watchCfg callback = do
  -- Initialize state
  now <- getCurrentTime
  fileModTimes <- newTVarIO Map.empty
  incState <- newIncrementalState ".argus-cache" (configHash cfg)
  incVar <- newTVarIO incState

  -- Initialize HIE incremental state
  let hieDir = case wcHIEDirectory watchCfg of
        Just dir -> dir
        Nothing -> ".hie"
  hieIncState <- loadHIEIncrementalState ".argus-hie-cache"
  hieIncVar <- newTVarIO hieIncState

  lastAnalysis <- newTVarIO now
  pendingFiles <- newTVarIO Set.empty
  pendingHIE <- newTVarIO Set.empty
  running <- newTVarIO True
  watchThread <- newIORef Nothing
  hieWatchThread <- newIORef Nothing

  let state = WatchState
        { wsFileModTimes = fileModTimes
        , wsIncremental = incVar
        , wsHIEIncremental = hieIncVar
        , wsLastAnalysis = lastAnalysis
        , wsPendingFiles = pendingFiles
        , wsPendingHIE = pendingHIE
        , wsRunning = running
        , wsWatchThread = watchThread
        , wsHIEWatchThread = hieWatchThread
        }

  -- Initial scan
  initialFiles <- scanDirectories (wcDirectories watchCfg) (wcExcludePatterns watchCfg)
  modTimes <- getFileModTimes initialFiles
  atomically $ writeTVar fileModTimes modTimes

  -- Initial analysis
  when (wcVerbose watchCfg) $
    TIO.putStrLn $ "Watching " <> T.pack (show (length initialFiles)) <> " files..."

  runAnalysis cfg watchCfg state callback initialFiles

  -- Start watch thread
  tid <- forkIO $ watchLoop cfg watchCfg state callback
  writeIORef watchThread (Just tid)

  -- Start HIE watch thread if enabled
  when (wcWatchHIE watchCfg) $ do
    when (wcVerbose watchCfg) $
      TIO.putStrLn $ "Watching HIE files in " <> T.pack hieDir
    hieTid <- forkIO $ hieWatchLoop watchCfg state callback hieDir
    writeIORef hieWatchThread (Just hieTid)

  pure state

-- | Stop watch mode
stopWatchMode :: WatchState -> IO ()
stopWatchMode state = do
  atomically $ writeTVar (wsRunning state) False

  -- Stop watch threads
  mTid <- readIORef (wsWatchThread state)
  forM_ mTid killThread

  mHieTid <- readIORef (wsHIEWatchThread state)
  forM_ mHieTid killThread

  -- Save incremental states
  incState <- atomically $ readTVar (wsIncremental state)
  saveIncrementalState incState

  hieIncState <- atomically $ readTVar (wsHIEIncremental state)
  saveHIEIncrementalState hieIncState

--------------------------------------------------------------------------------
-- Internal Watch Loop
--------------------------------------------------------------------------------

-- | Main watch loop
watchLoop :: Config -> WatchConfig -> WatchState -> WatchCallback -> IO ()
watchLoop cfg watchCfg state callback = do
  running <- atomically $ readTVar (wsRunning state)
  when running $ do
    -- Check for changes
    changedFiles <- detectChanges watchCfg state

    unless (null changedFiles) $ do
      -- Add to pending files
      atomically $ modifyTVar' (wsPendingFiles state) (Set.union (Set.fromList changedFiles))

      -- Notify about file changes
      forM_ changedFiles $ \f ->
        callback (FileModified f)

    -- Check if we should run analysis (debounce)
    shouldAnalyze <- checkDebounce watchCfg state

    when shouldAnalyze $ do
      pending <- atomically $ do
        p <- readTVar (wsPendingFiles state)
        writeTVar (wsPendingFiles state) Set.empty
        pure p

      unless (Set.null pending) $ do
        -- Clear screen if configured
        when (wcClearScreen watchCfg) $
          TIO.putStr "\ESC[2J\ESC[H"

        runAnalysis cfg watchCfg state callback (Set.toList pending)

    -- Wait before next iteration
    threadDelay (wcPollIntervalMs watchCfg * 1000)
    watchLoop cfg watchCfg state callback

-- | Detect file changes
detectChanges :: WatchConfig -> WatchState -> IO [FilePath]
detectChanges watchCfg state = do
  -- Get current files
  currentFiles <- scanDirectories (wcDirectories watchCfg) (wcExcludePatterns watchCfg)
  currentModTimes <- getFileModTimes currentFiles

  -- Compare with known state
  knownModTimes <- atomically $ readTVar (wsFileModTimes state)

  let changedFiles =
        [ f
        | (f, newTime) <- Map.toList currentModTimes
        , case Map.lookup f knownModTimes of
            Nothing -> True  -- New file
            Just oldTime -> newTime > oldTime  -- Modified
        ]

  -- Update known mod times
  atomically $ writeTVar (wsFileModTimes state) currentModTimes

  pure changedFiles

-- | Check if enough time has passed since last file change (debounce)
checkDebounce :: WatchConfig -> WatchState -> IO Bool
checkDebounce watchCfg state = do
  pending <- atomically $ readTVar (wsPendingFiles state)
  if Set.null pending
    then pure False
    else do
      now <- getCurrentTime
      lastTime <- atomically $ readTVar (wsLastAnalysis state)
      let elapsed = diffUTCTime now lastTime
          debounceSeconds = fromIntegral (wcDebounceMs watchCfg) / 1000
      pure $ elapsed >= debounceSeconds

-- | Run analysis on the given files
runAnalysis :: Config -> WatchConfig -> WatchState -> WatchCallback -> [FilePath] -> IO ()
runAnalysis cfg watchCfg state callback files = do
  startTime <- getCurrentTime
  callback (AnalysisStarted files)

  -- Get incremental state
  incState <- atomically $ readTVar (wsIncremental state)

  -- Determine which files actually need analysis
  filesToAnalyze <- getChangedFiles incState files

  -- Analyze files
  let opts = defaultOptions { optMode = QuickMode }
      ctx = defaultContext cfg opts defaultRulesConfig

  allDiags <- analyzeFilesIncremental ctx incState filesToAnalyze

  -- Update last analysis time
  atomically $ writeTVar (wsLastAnalysis state) startTime

  -- Calculate time taken
  endTime <- getCurrentTime
  let elapsed = realToFrac (diffUTCTime endTime startTime) :: Float

  -- Report results
  callback (AnalysisCompleted allDiags elapsed)

  -- Show output
  when (wcShowTimestamp watchCfg) $ do
    TIO.putStrLn $ "Analysis completed at " <> T.pack (show endTime)
    TIO.putStrLn $ "Analyzed " <> T.pack (show (length filesToAnalyze))
                 <> " file(s) in " <> T.pack (show elapsed) <> "s"
    TIO.putStrLn ""

  -- Display diagnostics
  unless (null allDiags) $ do
    let result = mockAnalysisResult files allDiags
        outputOpts = OutputOptions
          { ooFormat = TerminalFormat
          , ooColor = True
          , ooGroupBy = "file"
          , ooShowContext = True
          , ooContextLines = 2
          , ooVerbose = wcVerbose watchCfg
          , ooSourceCache = Map.empty
          }
        output = renderTerminal outputOpts result
    TIO.putStrLn output

  hFlush stdout

-- | Analyze files using incremental analysis
analyzeFilesIncremental :: AnalysisContext -> IncrementalState -> [FilePath] -> IO [Diagnostic]
analyzeFilesIncremental ctx incState files = do
  -- For each file, check cache first
  results <- mapM (analyzeFileOrCache ctx incState) files
  pure $ concat results

-- | Analyze a single file, using cache if available
analyzeFileOrCache :: AnalysisContext -> IncrementalState -> FilePath -> IO [Diagnostic]
analyzeFileOrCache ctx incState path = do
  cached <- getCachedDiagnostics incState path
  case cached of
    Just diags -> pure diags  -- Use cached results
    Nothing -> do
      -- Need to analyze
      result <- try @SomeException $ analyzeFile ctx path
      case result of
        Left _ -> pure []
        Right fileResult -> pure (fileResultDiagnostics fileResult)

--------------------------------------------------------------------------------
-- Default Callback
--------------------------------------------------------------------------------

-- | Default watch callback that prints events to terminal
defaultWatchCallback :: WatchEvent -> IO ()
defaultWatchCallback event = case event of
  FileModified path ->
    TIO.putStrLn $ "  Modified: " <> T.pack path

  FileCreated path ->
    TIO.putStrLn $ "  Created: " <> T.pack path

  FileDeleted path ->
    TIO.putStrLn $ "  Deleted: " <> T.pack path

  AnalysisStarted files ->
    TIO.putStrLn $ "Analyzing " <> T.pack (show (length files)) <> " file(s)..."

  AnalysisCompleted diags elapsed -> do
    let errorCount = length $ filter (\d -> diagSeverity d == Error) diags
        warnCount = length $ filter (\d -> diagSeverity d == Warning) diags
    TIO.putStrLn $ "Found " <> T.pack (show errorCount) <> " error(s), "
                 <> T.pack (show warnCount) <> " warning(s) in "
                 <> T.pack (show elapsed) <> "s"

  WatchError msg ->
    TIO.putStrLn $ "Watch error: " <> msg

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Scan directories for Haskell source files
scanDirectories :: [FilePath] -> [Text] -> IO [FilePath]
scanDirectories dirs excludePatterns = do
  allFiles <- concat <$> mapM scanDirectory dirs
  pure $ filter (not . matchesExclude excludePatterns) allFiles

-- | Scan a single directory recursively for Haskell files
scanDirectory :: FilePath -> IO [FilePath]
scanDirectory dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      contents <- listDirectory dir
      let entries = map (dir </>) contents
      files <- filterM isHaskellFile entries
      subdirs <- filterM doesDirectoryExist entries
      subFiles <- concat <$> mapM scanDirectory subdirs
      pure $ files ++ subFiles
  where
    filterM :: (a -> IO Bool) -> [a] -> IO [a]
    filterM p = foldr (\x acc -> do
      b <- p x
      rest <- acc
      pure $ if b then x:rest else rest) (pure [])

-- | Check if a file is a Haskell source file
isHaskellFile :: FilePath -> IO Bool
isHaskellFile path = do
  exists <- doesFileExist path
  pure $ exists && takeExtension path == ".hs"

-- | Check if a path matches any exclude pattern
matchesExclude :: [Text] -> FilePath -> Bool
matchesExclude patterns path =
  let pathText = T.pack path
  in any (`matchPattern` pathText) patterns

-- | Simple glob pattern matching
matchPattern :: Text -> Text -> Bool
matchPattern pat path
  | "**" `T.isInfixOf` pat = T.isInfixOf (T.replace "**" "" pat) path
  | "*" `T.isInfixOf` pat =
      let (prefix, rest) = T.breakOn "*" pat
          suffix = T.drop 1 rest
      in T.isPrefixOf prefix path && T.isSuffixOf suffix path
  | otherwise = pat == path

-- | Get modification times for a list of files
getFileModTimes :: [FilePath] -> IO (Map FilePath UTCTime)
getFileModTimes files = do
  pairs <- mapM getModTimePair files
  pure $ Map.fromList pairs
  where
    getModTimePair path = do
      modTime <- getModificationTime path
      pure (path, modTime)

-- | Hash configuration for cache invalidation
-- Uses SHA256 to create a deterministic hash of the configuration
-- that changes when any configuration option changes.
configHash :: Config -> Text
configHash cfg =
  let configStr = show cfg  -- Use Show instance for serialization
      configBytes = BS.pack configStr
      digest = hashWith SHA256 configBytes
  in T.pack (show digest)

-- | Create a mock AnalysisResult from diagnostics for rendering
mockAnalysisResult :: [FilePath] -> [Diagnostic] -> AnalysisResult
mockAnalysisResult _files diags = AnalysisResult
  { resultFiles = groupByFile diags
  , resultUnusedCode = Set.empty
  , resultDiagCount = countBySeverity diags
  }
  where
    groupByFile :: [Diagnostic] -> Map FilePath FileResult
    groupByFile ds =
      let byFile = foldr addToFile Map.empty ds
      in byFile

    addToFile :: Diagnostic -> Map FilePath FileResult -> Map FilePath FileResult
    addToFile d m =
      let path = srcSpanFile (diagSpan d)
          fr = Map.findWithDefault (emptyFileResult path) path m
          newFr = fr { fileResultDiagnostics = d : fileResultDiagnostics fr }
      in Map.insert path newFr m

    emptyFileResult :: FilePath -> FileResult
    emptyFileResult path = FileResult
      { fileResultPath = path
      , fileResultDiagnostics = []
      , fileResultSymbols = []
      , fileResultImports = []
      , fileResultExports = []
      }

    countBySeverity :: [Diagnostic] -> Map Severity Int
    countBySeverity ds =
      foldr (\d m -> Map.insertWith (+) (diagSeverity d) 1 m) Map.empty ds

--------------------------------------------------------------------------------
-- HIE Watch Loop
--------------------------------------------------------------------------------

-- | Watch loop for HIE file changes
hieWatchLoop :: WatchConfig -> WatchState -> WatchCallback -> FilePath -> IO ()
hieWatchLoop watchCfg state callback hieDir = do
  running <- atomically $ readTVar (wsRunning state)
  when running $ do
    -- Check for HIE file changes
    hieIncState <- atomically $ readTVar (wsHIEIncremental state)
    hieFiles <- scanHIEFiles hieDir
    changedHIE <- detectHIEChanges hieIncState hieFiles

    unless (null changedHIE) $ do
      -- Add to pending HIE files
      atomically $ modifyTVar' (wsPendingHIE state) (Set.union (Set.fromList changedHIE))

      -- Notify about changes
      forM_ changedHIE $ \f ->
        callback (HIEFileModified f)

      -- Invalidate dependents for changed HIE files
      forM_ changedHIE $ \hiePath -> do
        hieState <- atomically $ readTVar (wsHIEIncremental state)
        let newHieState = invalidateHIEDependents hieState hiePath
        atomically $ writeTVar (wsHIEIncremental state) newHieState

      when (wcVerbose watchCfg) $
        TIO.putStrLn $ "  Detected " <> T.pack (show (length changedHIE)) <> " HIE file change(s)"

    -- Wait before next iteration
    threadDelay (wcPollIntervalMs watchCfg * 1000)
    hieWatchLoop watchCfg state callback hieDir
