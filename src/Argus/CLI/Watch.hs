{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Watch
-- Description : Watch command implementation
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module implements the watch command for continuous file monitoring.
module Argus.CLI.Watch
  ( runWatch
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesDirectoryExist)

import Argus.CLI.Types
import Argus.CLI.Common (mkProgressConfig)
import Argus.Config (loadConfig)
import Argus.Output.Progress qualified as Progress
import Argus.Watch
  ( WatchConfig (..)
  , WatchEvent (..)
  , WatchCallback
  , defaultWatchConfig
  , runWatchMode
  , stopWatchMode
  )

-- | Run watch command - continuously monitor files for issues
runWatch :: GlobalOptions -> WatchOptions -> IO ()
runWatch global opts = do
  let targets = if null (woTargets opts) then ["."] else woTargets opts

  -- Validate directories exist
  mapM_ validateTarget targets

  let progressCfg = mkProgressConfig global True

  when (Progress.pcEnabled progressCfg) $ do
    Progress.printInfo progressCfg $ "Starting file watch mode on: " <> T.intercalate ", " (map T.pack targets)
    Progress.printInfo progressCfg $ "Debounce: " <> T.pack (show $ woDebounceMs opts) <> "ms"
    Progress.printInfo progressCfg "Press Ctrl+C to stop"

  -- Load configuration (loadConfig returns IO Config directly)
  config <- loadConfig (goConfigFile global)

  -- Create watch configuration
  let watchConfig = defaultWatchConfig
        { wcDirectories = targets
        , wcDebounceMs = woDebounceMs opts
        , wcClearScreen = woClearScreen opts
        , wcShowTimestamp = woShowTimestamp opts
        , wcVerbose = goVerbose global
        }

  -- Create callback for handling events
  let callback = watchCallback (goVerbose global)

  -- Run the watch loop
  TIO.putStrLn $ "Watching " <> T.pack (show $ length targets) <> " directories..."
  TIO.putStrLn "Press Ctrl+C to stop.\n"

  state <- runWatchMode config watchConfig callback

  -- Keep running until interrupted (in practice, runWatchMode handles this)
  waitForever

  -- Cleanup
  stopWatchMode state
  TIO.putStrLn "\nWatch mode stopped."

-- | Callback for watch events
watchCallback :: Bool -> WatchCallback
watchCallback verbose event = case event of
  FileModified path ->
    when verbose $
      TIO.putStrLn $ colorBlue "Modified: " <> T.pack path

  FileCreated path ->
    when verbose $
      TIO.putStrLn $ colorGreen "Created: " <> T.pack path

  FileDeleted path ->
    when verbose $
      TIO.putStrLn $ colorRed "Deleted: " <> T.pack path

  HIEFileModified path ->
    when verbose $
      TIO.putStrLn $ colorBlue "HIE Modified: " <> T.pack path

  HIEFileCreated path ->
    when verbose $
      TIO.putStrLn $ colorGreen "HIE Created: " <> T.pack path

  HIEFileDeleted path ->
    when verbose $
      TIO.putStrLn $ colorRed "HIE Deleted: " <> T.pack path

  AnalysisStarted files ->
    TIO.putStrLn $ colorBold "Analyzing " <> T.pack (show $ length files) <> " files..."

  AnalysisCompleted diags duration -> do
    let count = length diags
    if count == 0
      then TIO.putStrLn $ colorGreen "✓ No issues found" <> " (" <> formatDuration duration <> ")"
      else TIO.putStrLn $ colorYellow ("Found " <> T.pack (show count) <> " issues") <> " (" <> formatDuration duration <> ")"

  WatchError err ->
    TIO.putStrLn $ colorRed "Error: " <> err

-- | Validate that a target exists
validateTarget :: FilePath -> IO ()
validateTarget target = do
  exists <- doesDirectoryExist target
  when (not exists) $
    TIO.putStrLn $ "Warning: Directory does not exist: " <> T.pack target

-- | Wait forever (blocking)
waitForever :: IO ()
waitForever = forever $ threadDelay 1000000  -- 1 second

-- | Format duration in seconds
formatDuration :: Float -> Text
formatDuration secs
  | secs < 1.0 = T.pack (show (round (secs * 1000) :: Int)) <> "ms"
  | otherwise = T.pack (show (round secs :: Int)) <> "s"

-- | ANSI color helpers
colorBold :: Text -> Text
colorBold t = "\ESC[1m" <> t <> "\ESC[0m"

colorRed :: Text -> Text
colorRed t = "\ESC[31m" <> t <> "\ESC[0m"

colorYellow :: Text -> Text
colorYellow t = "\ESC[33m" <> t <> "\ESC[0m"

colorGreen :: Text -> Text
colorGreen t = "\ESC[32m" <> t <> "\ESC[0m"

colorBlue :: Text -> Text
colorBlue t = "\ESC[34m" <> t <> "\ESC[0m"
