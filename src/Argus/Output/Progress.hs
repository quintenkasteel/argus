{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Output.Progress
-- Description : Terminal progress indicators
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Progress bars, spinners, and status indicators for CLI operations.
module Argus.Output.Progress
  ( -- * Progress configuration
    ProgressConfig (..)
  , defaultProgressConfig

    -- * Spinner
  , Spinner (..)
  , SpinnerStyle (..)
  , withSpinner
  , startSpinner
  , stopSpinner
  , updateSpinnerMessage

    -- * Progress bar
  , ProgressBar (..)
  , ProgressBarStyle (..)
  , withProgressBar
  , startProgressBar
  , updateProgress
  , stopProgressBar

    -- * Status messages
  , StatusStyle (..)
  , printStatus
  , printSuccess
  , printWarning
  , printError
  , printInfo

    -- * File scanning progress
  , ScanProgress (..)
  , withScanProgress

    -- * Analysis progress
  , AnalysisProgress (..)
  , withAnalysisProgress
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever, when, unless)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout)
import System.Console.ANSI (hideCursor, showCursor, clearLine,
                            setCursorColumn, Color(..), ColorIntensity(..),
                            ConsoleLayer(..), SGR(..))

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Progress indicator configuration
data ProgressConfig = ProgressConfig
  { pcEnabled     :: Bool      -- ^ Whether progress is enabled
  , pcColor       :: Bool      -- ^ Whether to use colors
  , pcUnicode     :: Bool      -- ^ Whether to use unicode characters
  , pcInteractive :: Bool      -- ^ Whether terminal is interactive (TTY)
  , pcWidth       :: Int       -- ^ Terminal width for progress bars
  }
  deriving stock (Eq, Show)

-- | Default configuration
defaultProgressConfig :: ProgressConfig
defaultProgressConfig = ProgressConfig
  { pcEnabled = True
  , pcColor = True
  , pcUnicode = True
  , pcInteractive = True
  , pcWidth = 80
  }

--------------------------------------------------------------------------------
-- Spinner
--------------------------------------------------------------------------------

-- | Spinner styles
data SpinnerStyle
  = SpinnerDots      -- ^ ⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏
  | SpinnerLine      -- ^ ⎺⎻⎼⎽⎼⎻
  | SpinnerCircle    -- ^ ◐◓◑◒
  | SpinnerArrow     -- ^ ←↖↑↗→↘↓↙
  | SpinnerAscii     -- ^ |/-\
  deriving stock (Eq, Show)

-- | Get spinner frames for a style
spinnerFrames :: SpinnerStyle -> [Text]
spinnerFrames SpinnerDots   = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
spinnerFrames SpinnerLine   = ["⎺", "⎻", "⎼", "⎽", "⎼", "⎻"]
spinnerFrames SpinnerCircle = ["◐", "◓", "◑", "◒"]
spinnerFrames SpinnerArrow  = ["←", "↖", "↑", "↗", "→", "↘", "↓", "↙"]
spinnerFrames SpinnerAscii  = ["|", "/", "-", "\\"]

-- | Running spinner state
data Spinner = Spinner
  { spinnerThread   :: TVar (Maybe ThreadId)
  , spinnerMessage  :: TVar Text
  , spinnerConfig   :: ProgressConfig
  , spinnerStyle    :: SpinnerStyle
  , spinnerRunning  :: TVar Bool
  }

-- | Run an action with a spinner
withSpinner :: ProgressConfig -> SpinnerStyle -> Text -> IO a -> IO a
withSpinner config style msg action = do
  spinner <- startSpinner config style msg
  action `finally` stopSpinner spinner

-- | Start a spinner
startSpinner :: ProgressConfig -> SpinnerStyle -> Text -> IO Spinner
startSpinner config style msg = do
  threadVar <- newTVarIO Nothing
  msgVar <- newTVarIO msg
  runningVar <- newTVarIO True

  let spinner = Spinner
        { spinnerThread = threadVar
        , spinnerMessage = msgVar
        , spinnerConfig = config
        , spinnerStyle = style
        , spinnerRunning = runningVar
        }

  when (pcEnabled config && pcInteractive config) $ do
    hideCursor
    tid <- forkIO $ spinnerLoop spinner
    atomically $ writeTVar threadVar (Just tid)

  pure spinner

-- | Stop a spinner
stopSpinner :: Spinner -> IO ()
stopSpinner spinner = do
  atomically $ writeTVar (spinnerRunning spinner) False

  mTid <- atomically $ readTVar (spinnerThread spinner)
  case mTid of
    Just tid -> do
      killThread tid
      atomically $ writeTVar (spinnerThread spinner) Nothing
    Nothing -> pure ()

  when (pcEnabled (spinnerConfig spinner) && pcInteractive (spinnerConfig spinner)) $ do
    clearLine
    setCursorColumn 0
    showCursor
    hFlush stdout

-- | Update spinner message
updateSpinnerMessage :: Spinner -> Text -> IO ()
updateSpinnerMessage spinner msg =
  atomically $ writeTVar (spinnerMessage spinner) msg

-- | Spinner animation loop
spinnerLoop :: Spinner -> IO ()
spinnerLoop spinner = do
  frameRef <- newIORef (0 :: Int)
  let frames = spinnerFrames (spinnerStyle spinner)
      numFrames = length frames
      config = spinnerConfig spinner

  forever $ do
    running <- atomically $ readTVar (spinnerRunning spinner)
    unless running $ pure ()

    frameIdx <- readIORef frameRef
    msg <- atomically $ readTVar (spinnerMessage spinner)

    let frame = frames !! (frameIdx `mod` numFrames)
        output = if pcColor config
                 then colorText Cyan frame <> " " <> msg
                 else frame <> " " <> msg

    clearLine
    setCursorColumn 0
    TIO.putStr output
    hFlush stdout

    modifyIORef' frameRef (+ 1)
    threadDelay 80000  -- 80ms between frames

--------------------------------------------------------------------------------
-- Progress Bar
--------------------------------------------------------------------------------

-- | Progress bar styles
data ProgressBarStyle
  = ProgressBarBlock    -- ^ █░
  | ProgressBarShade    -- ^ ▓▒░
  | ProgressBarArrow    -- ^ =>-
  | ProgressBarAscii    -- ^ #-
  deriving stock (Eq, Show)

-- | Progress bar state
data ProgressBar = ProgressBar
  { pbThread     :: TVar (Maybe ThreadId)
  , pbCurrent    :: TVar Int
  , pbTotal      :: TVar Int
  , pbMessage    :: TVar Text
  , pbConfig     :: ProgressConfig
  , pbStyle      :: ProgressBarStyle
  , pbRunning    :: TVar Bool
  , pbStartTime  :: IORef UTCTime
  }

-- | Run an action with a progress bar
withProgressBar :: ProgressConfig -> ProgressBarStyle -> Text -> Int -> IO a -> IO a
withProgressBar config style msg total action = do
  pb <- startProgressBar config style msg total
  action `finally` stopProgressBar pb

-- | Start a progress bar
startProgressBar :: ProgressConfig -> ProgressBarStyle -> Text -> Int -> IO ProgressBar
startProgressBar config style msg total = do
  threadVar <- newTVarIO Nothing
  currentVar <- newTVarIO 0
  totalVar <- newTVarIO total
  msgVar <- newTVarIO msg
  runningVar <- newTVarIO True
  startTime <- getCurrentTime
  startTimeRef <- newIORef startTime

  let pb = ProgressBar
        { pbThread = threadVar
        , pbCurrent = currentVar
        , pbTotal = totalVar
        , pbMessage = msgVar
        , pbConfig = config
        , pbStyle = style
        , pbRunning = runningVar
        , pbStartTime = startTimeRef
        }

  when (pcEnabled config && pcInteractive config) $ do
    hideCursor
    tid <- forkIO $ progressBarLoop pb
    atomically $ writeTVar threadVar (Just tid)

  pure pb

-- | Update progress
updateProgress :: ProgressBar -> Int -> IO ()
updateProgress pb current =
  atomically $ writeTVar (pbCurrent pb) current

-- | Stop progress bar
stopProgressBar :: ProgressBar -> IO ()
stopProgressBar pb = do
  atomically $ writeTVar (pbRunning pb) False

  mTid <- atomically $ readTVar (pbThread pb)
  case mTid of
    Just tid -> do
      killThread tid
      atomically $ writeTVar (pbThread pb) Nothing
    Nothing -> pure ()

  when (pcEnabled (pbConfig pb) && pcInteractive (pbConfig pb)) $ do
    clearLine
    setCursorColumn 0
    showCursor
    hFlush stdout

-- | Progress bar animation loop
progressBarLoop :: ProgressBar -> IO ()
progressBarLoop pb = forever $ do
  running <- atomically $ readTVar (pbRunning pb)
  unless running $ pure ()

  current <- atomically $ readTVar (pbCurrent pb)
  total <- atomically $ readTVar (pbTotal pb)
  msg <- atomically $ readTVar (pbMessage pb)
  startTime <- readIORef (pbStartTime pb)
  now <- getCurrentTime

  let config = pbConfig pb
      style = pbStyle pb
      percentage = if total > 0
                   then (current * 100) `div` total
                   else 0
      elapsed = diffUTCTime now startTime
      eta = if current > 0 && current < total
            then let rate = fromIntegral current / realToFrac elapsed :: Double
                     remaining = fromIntegral (total - current) / rate
                 in Just remaining
            else Nothing

      barWidth = min 40 (pcWidth config - 20)
      filled = (current * barWidth) `div` max 1 total
      empty = barWidth - filled

      (filledChar, emptyChar) = case style of
        ProgressBarBlock -> ("█", "░")
        ProgressBarShade -> ("▓", "░")
        ProgressBarArrow -> ("=", "-")
        ProgressBarAscii -> ("#", "-")

      bar = T.replicate filled filledChar <> T.replicate empty emptyChar
      pctText = T.pack (show percentage) <> "%"
      etaText = case eta of
        Just secs -> " ETA: " <> formatSeconds secs
        Nothing   -> ""

      output = if pcColor config
               then msg <> " " <> colorText Green "[" <> colorText Cyan bar <> colorText Green "]"
                    <> " " <> pctText <> etaText
               else msg <> " [" <> bar <> "] " <> pctText <> etaText

  clearLine
  setCursorColumn 0
  TIO.putStr output
  hFlush stdout

  threadDelay 100000  -- 100ms between updates

-- | Format seconds as human-readable time
formatSeconds :: Double -> Text
formatSeconds secs
  | secs < 60 = T.pack (show (round secs :: Int)) <> "s"
  | secs < 3600 = T.pack (show (round (secs / 60) :: Int)) <> "m"
  | otherwise = T.pack (show (round (secs / 3600) :: Int)) <> "h"

--------------------------------------------------------------------------------
-- Status Messages
--------------------------------------------------------------------------------

-- | Status message style
data StatusStyle
  = StatusSuccess
  | StatusWarning
  | StatusError
  | StatusInfo
  | StatusProgress
  deriving stock (Eq, Show)

-- | Print a status message
printStatus :: ProgressConfig -> StatusStyle -> Text -> IO ()
printStatus config style msg = do
  let (symbol, colorF) = case style of
        StatusSuccess  -> ("✓", colorText Green)
        StatusWarning  -> ("⚠", colorText Yellow)
        StatusError    -> ("✗", colorText Red)
        StatusInfo     -> ("ℹ", colorText Blue)
        StatusProgress -> ("→", colorText Cyan)

      output = if pcColor config
               then colorF symbol <> " " <> msg
               else (if pcUnicode config then symbol else textSymbol style) <> " " <> msg

  TIO.putStrLn output
  hFlush stdout
  where
    textSymbol StatusSuccess  = "[OK]"
    textSymbol StatusWarning  = "[WARN]"
    textSymbol StatusError    = "[ERR]"
    textSymbol StatusInfo     = "[INFO]"
    textSymbol StatusProgress = "[..]"

-- | Print success message
printSuccess :: ProgressConfig -> Text -> IO ()
printSuccess config = printStatus config StatusSuccess

-- | Print warning message
printWarning :: ProgressConfig -> Text -> IO ()
printWarning config = printStatus config StatusWarning

-- | Print error message
printError :: ProgressConfig -> Text -> IO ()
printError config = printStatus config StatusError

-- | Print info message
printInfo :: ProgressConfig -> Text -> IO ()
printInfo config = printStatus config StatusInfo

--------------------------------------------------------------------------------
-- File Scanning Progress
--------------------------------------------------------------------------------

-- | Scan progress state
data ScanProgress = ScanProgress
  { spFilesScanned :: TVar Int
  , spCurrentDir   :: TVar Text
  , spSpinner      :: Maybe Spinner
  }

-- | Run file scanning with progress
withScanProgress :: ProgressConfig -> (ScanProgress -> IO a) -> IO a
withScanProgress config action = do
  filesVar <- newTVarIO 0
  dirVar <- newTVarIO ""

  if pcEnabled config && pcInteractive config
    then do
      spinner <- startSpinner config SpinnerDots "Scanning files..."
      let sp = ScanProgress filesVar dirVar (Just spinner)
      result <- action sp `finally` stopSpinner spinner
      -- Print final count
      finalCount <- atomically $ readTVar filesVar
      printSuccess config $ "Found " <> T.pack (show finalCount) <> " Haskell files"
      pure result
    else do
      let sp = ScanProgress filesVar dirVar Nothing
      action sp

--------------------------------------------------------------------------------
-- Analysis Progress
--------------------------------------------------------------------------------

-- | Analysis progress state
data AnalysisProgress = AnalysisProgress
  { apFilesAnalyzed :: TVar Int
  , apTotalFiles    :: TVar Int
  , apCurrentFile   :: TVar Text
  , apProgressBar   :: Maybe ProgressBar
  , apConfig        :: ProgressConfig
  }

-- | Run analysis with progress
withAnalysisProgress :: ProgressConfig -> Int -> (AnalysisProgress -> IO a) -> IO a
withAnalysisProgress config totalFiles action = do
  analyzedVar <- newTVarIO 0
  totalVar <- newTVarIO totalFiles
  currentVar <- newTVarIO ""

  if pcEnabled config && pcInteractive config
    then do
      pb <- startProgressBar config ProgressBarBlock "Analyzing" totalFiles
      let ap = AnalysisProgress analyzedVar totalVar currentVar (Just pb) config
      result <- action ap `finally` stopProgressBar pb

      -- Print completion message
      printSuccess config $ "Analyzed " <> T.pack (show totalFiles) <> " files"
      pure result
    else do
      let ap = AnalysisProgress analyzedVar totalVar currentVar Nothing config
      action ap

--------------------------------------------------------------------------------
-- Color Helpers
--------------------------------------------------------------------------------

colorText :: Color -> Text -> Text
colorText c txt = T.pack (setSGRCode [SetColor Foreground Vivid c])
               <> txt
               <> T.pack (setSGRCode [Reset])

setSGRCode :: [SGR] -> String
setSGRCode sgrs = "\ESC[" <> codes <> "m"
  where
    codes = case sgrs of
      [] -> "0"
      _  -> concatMap sgrToCode sgrs

    sgrToCode (SetColor Foreground Vivid Red)     = "91"
    sgrToCode (SetColor Foreground Vivid Green)   = "92"
    sgrToCode (SetColor Foreground Vivid Yellow)  = "93"
    sgrToCode (SetColor Foreground Vivid Blue)    = "94"
    sgrToCode (SetColor Foreground Vivid Magenta) = "95"
    sgrToCode (SetColor Foreground Vivid Cyan)    = "96"
    sgrToCode Reset                               = "0"
    sgrToCode _                                   = ""
