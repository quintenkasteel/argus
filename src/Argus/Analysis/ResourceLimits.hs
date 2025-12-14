{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Analysis.ResourceLimits
-- Description : Timeout and resource management for analysis
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides resource management for Argus analysis, including:
--
-- * Configurable per-file timeouts
-- * Memory usage monitoring
-- * Graceful timeout handling with partial results
-- * Resource statistics collection
--
-- == Usage
--
-- @
-- result <- withTimeout config filePath $ do
--   analyze file
-- @
module Argus.Analysis.ResourceLimits
  ( -- * Configuration
    ResourceConfig (..)
  , defaultResourceConfig

    -- * Timeout Operations
  , withTimeout
  , withTimeoutMicros
  , TimeoutResult (..)

    -- * Memory Monitoring
  , MemoryStats (..)
  , getMemoryStats
  , checkMemoryLimit

    -- * Resource Tracking
  , ResourceTracker
  , newResourceTracker
  , recordFileStart
  , recordFileComplete
  , recordFileTimeout
  , recordFileError
  , getResourceStats

    -- * Resource Statistics
  , ResourceStats (..)
  , FileResourceRecord (..)

    -- * Utilities
  , formatDuration
  , formatBytes
  , estimateComplexity
  ) where

import Control.Concurrent.STM
import Control.DeepSeq (NFData)
import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.Aeson (ToJSON, FromJSON)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import GHC.Generics (Generic)
import GHC.Stats qualified as GHC
import System.Mem (performGC)
import System.Timeout (timeout)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for resource limits
data ResourceConfig = ResourceConfig
  { rcTimeoutSeconds     :: Maybe Int    -- ^ Per-file timeout in seconds (Nothing = no timeout)
  , rcMaxMemoryMB        :: Maybe Int    -- ^ Maximum memory usage in MB (Nothing = no limit)
  , rcForceGCInterval    :: Maybe Int    -- ^ Force GC every N files (Nothing = never)
  , rcTrackPerFile       :: Bool         -- ^ Track per-file resource usage
  , rcWarnSlowFiles      :: Maybe Double -- ^ Warn if file takes longer than N seconds
  , rcMaxRetries         :: Int          -- ^ Maximum retries for failed files
  , rcKillOnTimeout      :: Bool         -- ^ Kill analysis thread on timeout (vs. let it complete)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Default resource configuration
defaultResourceConfig :: ResourceConfig
defaultResourceConfig = ResourceConfig
  { rcTimeoutSeconds     = Just 60       -- 60 second timeout per file
  , rcMaxMemoryMB        = Just 2048     -- 2GB max memory
  , rcForceGCInterval    = Just 100      -- GC every 100 files
  , rcTrackPerFile       = True          -- Track per-file stats
  , rcWarnSlowFiles      = Just 10.0     -- Warn if file takes > 10s
  , rcMaxRetries         = 1             -- One retry on failure
  , rcKillOnTimeout      = True          -- Kill thread on timeout
  }

--------------------------------------------------------------------------------
-- Timeout Operations
--------------------------------------------------------------------------------

-- | Result of a timed operation
data TimeoutResult a
  = Completed a Double        -- ^ Completed successfully with elapsed seconds
  | TimedOut Double           -- ^ Timed out after N seconds
  | Failed Text Double        -- ^ Failed with error message after N seconds
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance NFData a => NFData (TimeoutResult a)

-- | Run an action with a timeout in seconds
withTimeout
  :: NFData a
  => ResourceConfig
  -> IO a
  -> IO (TimeoutResult a)
withTimeout config action = case rcTimeoutSeconds config of
  Nothing -> do
    start <- getCurrentTime
    result <- catch (Right <$> action) $ \(e :: SomeException) ->
      pure $ Left $ T.pack $ show e
    end <- getCurrentTime
    let elapsed = realToFrac (diffUTCTime end start) :: Double
    pure $ case result of
      Right a  -> Completed a elapsed
      Left err -> Failed err elapsed
  Just secs -> withTimeoutMicros (secs * 1_000_000) action

-- | Run an action with a timeout in microseconds
withTimeoutMicros
  :: Int          -- ^ Timeout in microseconds
  -> IO a         -- ^ Action to run
  -> IO (TimeoutResult a)
withTimeoutMicros micros action = do
  start <- getCurrentTime
  -- Run timeout first, then catch exceptions on the result
  -- This ensures async exceptions from timeout are not caught
  let timedAction = do
        result <- timeout micros action
        end <- getCurrentTime
        let elapsed = realToFrac (diffUTCTime end start) :: Double
        case result of
          Nothing -> pure $ TimedOut elapsed
          Just a  -> pure $ Completed a elapsed
  timedAction `catch` \(e :: SomeException) -> do
    end' <- getCurrentTime
    let elapsed' = realToFrac (diffUTCTime end' start) :: Double
    pure $ Failed (T.pack $ show e) elapsed'

--------------------------------------------------------------------------------
-- Memory Monitoring
--------------------------------------------------------------------------------

-- | Memory usage statistics
data MemoryStats = MemoryStats
  { msHeapSize      :: Int      -- ^ Current heap size in bytes
  , msHeapLive      :: Int      -- ^ Live bytes on heap
  , msHeapAllocated :: Int      -- ^ Total bytes allocated
  , msGCCount       :: Int      -- ^ Number of GCs
  , msPeakMemory    :: Int      -- ^ Peak memory usage
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Get current memory statistics
-- Returns Nothing if RTS stats are not enabled
getMemoryStats :: IO (Maybe MemoryStats)
getMemoryStats = do
  enabled <- GHC.getRTSStatsEnabled
  if not enabled
    then pure Nothing
    else do
      stats <- GHC.getRTSStats
      pure $ Just MemoryStats
        { msHeapSize      = fromIntegral $ GHC.gcdetails_mem_in_use_bytes $ GHC.gc stats
        , msHeapLive      = fromIntegral $ GHC.gcdetails_live_bytes $ GHC.gc stats
        , msHeapAllocated = fromIntegral $ GHC.allocated_bytes stats
        , msGCCount       = fromIntegral $ GHC.gcs stats
        , msPeakMemory    = fromIntegral $ GHC.max_live_bytes stats
        }

-- | Check if memory usage exceeds the limit
checkMemoryLimit :: ResourceConfig -> IO (Maybe Text)
checkMemoryLimit config = case rcMaxMemoryMB config of
  Nothing -> pure Nothing
  Just limitMB -> do
    mstats <- getMemoryStats
    case mstats of
      Nothing -> pure Nothing  -- Can't check without RTS stats
      Just stats -> do
        let usedMB = msHeapLive stats `div` (1024 * 1024)
        if usedMB > limitMB
          then pure $ Just $ "Memory limit exceeded: " <> T.pack (show usedMB) <> "MB > " <> T.pack (show limitMB) <> "MB"
          else pure Nothing

--------------------------------------------------------------------------------
-- Resource Tracking
--------------------------------------------------------------------------------

-- | Tracker for resource usage across multiple files
data ResourceTracker = ResourceTracker
  { rtConfig      :: ResourceConfig
  , rtStartTime   :: UTCTime
  , rtFileRecords :: TVar (Map FilePath FileResourceRecord)
  , rtStats       :: IORef ResourceStats
  , rtFileCount   :: IORef Int
  }

-- | Per-file resource record
data FileResourceRecord = FileResourceRecord
  { frrFilePath    :: FilePath
  , frrStartTime   :: UTCTime
  , frrEndTime     :: Maybe UTCTime
  , frrElapsed     :: Maybe Double
  , frrStatus      :: FileStatus
  , frrMemoryStart :: Maybe Int
  , frrMemoryEnd   :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Status of file analysis
data FileStatus
  = StatusPending
  | StatusRunning
  | StatusCompleted
  | StatusTimedOut
  | StatusFailed Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Aggregate resource statistics
data ResourceStats = ResourceStats
  { rsTotalFiles      :: Int
  , rsCompletedFiles  :: Int
  , rsTimedOutFiles   :: Int
  , rsFailedFiles     :: Int
  , rsTotalElapsed    :: Double
  , rsAvgElapsed      :: Double
  , rsMaxElapsed      :: Double
  , rsMinElapsed      :: Double
  , rsPeakMemory      :: Maybe Int
  , rsGCCount         :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Create a new resource tracker
newResourceTracker :: ResourceConfig -> IO ResourceTracker
newResourceTracker config = do
  startTime <- getCurrentTime
  fileRecords <- newTVarIO Map.empty
  stats <- newIORef emptyStats
  fileCount <- newIORef 0
  pure ResourceTracker
    { rtConfig = config
    , rtStartTime = startTime
    , rtFileRecords = fileRecords
    , rtStats = stats
    , rtFileCount = fileCount
    }
  where
    emptyStats = ResourceStats
      { rsTotalFiles = 0
      , rsCompletedFiles = 0
      , rsTimedOutFiles = 0
      , rsFailedFiles = 0
      , rsTotalElapsed = 0
      , rsAvgElapsed = 0
      , rsMaxElapsed = 0
      , rsMinElapsed = 1e9
      , rsPeakMemory = Nothing
      , rsGCCount = 0
      }

-- | Record the start of file analysis
recordFileStart :: ResourceTracker -> FilePath -> IO ()
recordFileStart tracker filePath = do
  now <- getCurrentTime
  memStats <- getMemoryStats
  let record = FileResourceRecord
        { frrFilePath = filePath
        , frrStartTime = now
        , frrEndTime = Nothing
        , frrElapsed = Nothing
        , frrStatus = StatusRunning
        , frrMemoryStart = msHeapLive <$> memStats
        , frrMemoryEnd = Nothing
        }
  atomically $ modifyTVar' (rtFileRecords tracker) $ Map.insert filePath record

  -- Check if we need to force GC
  when (rcTrackPerFile $ rtConfig tracker) $ do
    count <- atomicModifyIORef' (rtFileCount tracker) $ \n -> (n + 1, n + 1)
    case rcForceGCInterval (rtConfig tracker) of
      Just interval | count `mod` interval == 0 -> performGC
      _ -> pure ()

-- | Record successful completion
recordFileComplete :: ResourceTracker -> FilePath -> Double -> IO ()
recordFileComplete tracker filePath elapsed = do
  now <- getCurrentTime
  memStats <- getMemoryStats

  -- Update file record
  atomically $ modifyTVar' (rtFileRecords tracker) $ \records ->
    Map.adjust (\r -> r
      { frrEndTime = Just now
      , frrElapsed = Just elapsed
      , frrStatus = StatusCompleted
      , frrMemoryEnd = msHeapLive <$> memStats
      }) filePath records

  -- Update aggregate stats
  atomicModifyIORef' (rtStats tracker) $ \stats ->
    ( updateStatsCompleted stats elapsed memStats
    , ()
    )

-- | Record a timeout
recordFileTimeout :: ResourceTracker -> FilePath -> Double -> IO ()
recordFileTimeout tracker filePath elapsed = do
  now <- getCurrentTime

  atomically $ modifyTVar' (rtFileRecords tracker) $ \records ->
    Map.adjust (\r -> r
      { frrEndTime = Just now
      , frrElapsed = Just elapsed
      , frrStatus = StatusTimedOut
      }) filePath records

  atomicModifyIORef' (rtStats tracker) $ \stats ->
    ( stats
        { rsTotalFiles = rsTotalFiles stats + 1
        , rsTimedOutFiles = rsTimedOutFiles stats + 1
        , rsTotalElapsed = rsTotalElapsed stats + elapsed
        }
    , ()
    )

-- | Record an error
recordFileError :: ResourceTracker -> FilePath -> Text -> Double -> IO ()
recordFileError tracker filePath errMsg elapsed = do
  now <- getCurrentTime

  atomically $ modifyTVar' (rtFileRecords tracker) $ \records ->
    Map.adjust (\r -> r
      { frrEndTime = Just now
      , frrElapsed = Just elapsed
      , frrStatus = StatusFailed errMsg
      }) filePath records

  atomicModifyIORef' (rtStats tracker) $ \stats ->
    ( stats
        { rsTotalFiles = rsTotalFiles stats + 1
        , rsFailedFiles = rsFailedFiles stats + 1
        , rsTotalElapsed = rsTotalElapsed stats + elapsed
        }
    , ()
    )

-- | Get current resource statistics
getResourceStats :: ResourceTracker -> IO ResourceStats
getResourceStats tracker = do
  stats <- readIORef (rtStats tracker)
  memStats <- getMemoryStats
  pure $ stats
    { rsPeakMemory = max (rsPeakMemory stats) (msPeakMemory <$> memStats)
    , rsGCCount = maybe 0 msGCCount memStats
    }

-- | Update stats for a completed file
updateStatsCompleted :: ResourceStats -> Double -> Maybe MemoryStats -> ResourceStats
updateStatsCompleted stats elapsed memStats = stats
  { rsTotalFiles = rsTotalFiles stats + 1
  , rsCompletedFiles = rsCompletedFiles stats + 1
  , rsTotalElapsed = rsTotalElapsed stats + elapsed
  , rsAvgElapsed = newTotal / fromIntegral newCount
  , rsMaxElapsed = max (rsMaxElapsed stats) elapsed
  , rsMinElapsed = min (rsMinElapsed stats) elapsed
  , rsPeakMemory = max (rsPeakMemory stats) (msPeakMemory <$> memStats)
  }
  where
    newCount = rsCompletedFiles stats + 1
    newTotal = rsTotalElapsed stats + elapsed

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Format a duration in human-readable form
formatDuration :: Double -> Text
formatDuration secs
  | secs < 0.001   = T.pack $ show (round (secs * 1_000_000) :: Int) <> "μs"
  | secs < 1.0     = T.pack $ show (round (secs * 1_000) :: Int) <> "ms"
  | secs < 60.0    = T.pack $ show (round (secs * 10) `div` 10 :: Int) <> "." <>
                      show (round (secs * 10) `mod` 10 :: Int) <> "s"
  | secs < 3600.0  = let mins = floor secs `div` 60 :: Int
                         remainSecs = round secs `mod` 60 :: Int
                     in T.pack $ show mins <> "m " <> show remainSecs <> "s"
  | otherwise      = let hours = floor secs `div` 3600 :: Int
                         mins = (floor secs `mod` 3600) `div` 60 :: Int
                     in T.pack $ show hours <> "h " <> show mins <> "m"

-- | Format bytes in human-readable form
formatBytes :: Int -> Text
formatBytes bytes
  | bytes < 1024        = T.pack $ show bytes <> " B"
  | bytes < 1024 * 1024 = T.pack $ show (bytes `div` 1024) <> " KB"
  | bytes < 1024 * 1024 * 1024 = T.pack $ show (bytes `div` (1024 * 1024)) <> " MB"
  | otherwise           = T.pack $ show (bytes `div` (1024 * 1024 * 1024)) <> " GB"

-- | Estimate complexity of a file based on size and imports
-- Returns a multiplier for the timeout
estimateComplexity :: Text -> Double
estimateComplexity content =
  let lineCount = length $ T.lines content
      importCount = length $ filter (T.isPrefixOf "import ") $ T.lines content

      -- Heuristics for complexity
      lineFactor = if lineCount > 1000 then 2.0
                   else if lineCount > 500 then 1.5
                   else 1.0

      importFactor = if importCount > 50 then 1.5
                     else if importCount > 30 then 1.2
                     else 1.0

      -- Template Haskell often increases complexity
      thFactor = if "TemplateHaskell" `T.isInfixOf` content ||
                    "$(" `T.isInfixOf` content ||
                    "[|" `T.isInfixOf` content
                 then 2.0
                 else 1.0

  in lineFactor * importFactor * thFactor
