{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Daemon
-- Description : Daemon mode for fast repeated analysis
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides daemon mode functionality for Argus, enabling:
--
-- * Fast repeated analysis by keeping parsed files in memory
-- * Socket-based IPC for client requests
-- * Automatic cache invalidation on file changes
-- * Graceful shutdown and signal handling
--
-- == Usage
--
-- Start the daemon:
--
-- @
-- argus daemon start --port 9999
-- @
--
-- Send analysis requests:
--
-- @
-- argus daemon check src/
-- @
module Argus.Daemon
  ( -- * Daemon Configuration
    DaemonConfig (..)
  , defaultDaemonConfig

    -- * Daemon State
  , DaemonState (..)
  , DaemonStats (..)
  , DaemonStatusInfo (..)

    -- * Running the Daemon
  , runDaemon
  , stopDaemon
  , getDaemonStatus

    -- * Client Operations
  , sendRequest
  , DaemonRequest (..)
  , DaemonResponse (..)
  , AnalyzeRequest (..)
  , AnalyzeResponse (..)

    -- * Protocol
  , DaemonMessage (..)
  , encodeMessage
  , decodeMessage

    -- * Socket Path
  , getDefaultSocketPath
  , getDaemonPidFile
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.DeepSeq (NFData)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forever, when, void, forM_)
import Data.Aeson (ToJSON (..), FromJSON (..), encode, decode)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import GHC.Generics (Generic)
import Network.Socket
import Network.Socket.ByteString qualified as NBS
import System.Directory (doesFileExist, removeFile, getXdgDirectory, XdgDirectory(..), createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (installHandler, Handler(..), sigINT, sigTERM)

import Argus.Analysis.Cache (AnalysisCache, CacheEntry(..), emptyCache, lookupCache, hashFile)
import Argus.Analysis.Cache qualified as Cache
import Argus.Analysis.ResourceLimits qualified as RL
import System.Timeout (timeout)
import Argus.Config (Config, loadConfig)
import Argus.Core (analyzeFile, defaultContext, AnalysisContext(..))
import Argus.Rules.ConfigurableRules (RulesConfig, loadRulesConfig)
import Argus.Types

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Daemon configuration
data DaemonConfig = DaemonConfig
  { dcSocketPath       :: Maybe FilePath  -- ^ Unix socket path (Nothing = use default)
  , dcPort             :: Maybe Int       -- ^ TCP port (alternative to socket)
  , dcHost             :: Text            -- ^ Host for TCP (default: localhost)
  , dcMaxConnections   :: Int             -- ^ Maximum concurrent connections
  , dcIdleTimeout      :: Maybe Int       -- ^ Shutdown after N seconds of inactivity
  , dcCacheSize        :: Int             -- ^ Maximum number of files to cache
  , dcAutoReload       :: Bool            -- ^ Automatically reload config on change
  , dcResourceConfig   :: RL.ResourceConfig  -- ^ Resource limits for analysis
  , dcPidFile          :: Maybe FilePath  -- ^ PID file path
  , dcLogFile          :: Maybe FilePath  -- ^ Log file path
  , dcVerbose          :: Bool            -- ^ Verbose logging
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Default daemon configuration
defaultDaemonConfig :: DaemonConfig
defaultDaemonConfig = DaemonConfig
  { dcSocketPath       = Nothing          -- Use default socket path
  , dcPort             = Nothing          -- No TCP by default
  , dcHost             = "127.0.0.1"
  , dcMaxConnections   = 10
  , dcIdleTimeout      = Nothing          -- No idle timeout
  , dcCacheSize        = 1000             -- Cache up to 1000 files
  , dcAutoReload       = True
  , dcResourceConfig   = RL.defaultResourceConfig
  , dcPidFile          = Nothing
  , dcLogFile          = Nothing
  , dcVerbose          = False
  }

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

-- | Daemon state
data DaemonState = DaemonState
  { dsConfig        :: TVar DaemonConfig     -- ^ Current configuration
  , dsLinterConfig  :: TVar Config           -- ^ Linter configuration
  , dsRulesConfig   :: TVar RulesConfig      -- ^ Rules configuration
  , dsCache         :: TVar AnalysisCache    -- ^ Analysis cache
  , dsRunning       :: TVar Bool             -- ^ Is daemon running
  , dsLastActivity  :: TVar UTCTime          -- ^ Last activity time
  , dsStats         :: TVar DaemonStats      -- ^ Statistics
  , dsConnections   :: TVar Int              -- ^ Active connection count
  , dsSocket        :: TVar (Maybe Socket)   -- ^ Server socket
  , dsThreads       :: IORef [ThreadId]      -- ^ Worker threads
  }

-- | Daemon statistics
data DaemonStats = DaemonStats
  { dsTotalRequests    :: Int
  , dsTotalAnalyses    :: Int
  , dsCacheHits        :: Int
  , dsCacheMisses      :: Int
  , dsTotalFiles       :: Int
  , dsStartTime        :: UTCTime
  , dsLastAnalysisTime :: Maybe UTCTime
  , dsAvgAnalysisMs    :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Create initial daemon state
newDaemonState :: DaemonConfig -> Config -> RulesConfig -> IO DaemonState
newDaemonState cfg lintCfg rulesCfg = do
  now <- getCurrentTime
  cache <- emptyCache
  configVar <- newTVarIO cfg
  lintVar <- newTVarIO lintCfg
  rulesVar <- newTVarIO rulesCfg
  cacheVar <- newTVarIO cache
  runningVar <- newTVarIO False
  lastActivityVar <- newTVarIO now
  statsVar <- newTVarIO DaemonStats
    { dsTotalRequests = 0
    , dsTotalAnalyses = 0
    , dsCacheHits = 0
    , dsCacheMisses = 0
    , dsTotalFiles = 0
    , dsStartTime = now
    , dsLastAnalysisTime = Nothing
    , dsAvgAnalysisMs = 0
    }
  connectionsVar <- newTVarIO 0
  socketVar <- newTVarIO Nothing
  threadsRef <- newIORef []
  pure DaemonState
    { dsConfig = configVar
    , dsLinterConfig = lintVar
    , dsRulesConfig = rulesVar
    , dsCache = cacheVar
    , dsRunning = runningVar
    , dsLastActivity = lastActivityVar
    , dsStats = statsVar
    , dsConnections = connectionsVar
    , dsSocket = socketVar
    , dsThreads = threadsRef
    }

--------------------------------------------------------------------------------
-- Protocol
--------------------------------------------------------------------------------

-- | Daemon message envelope
data DaemonMessage = DaemonMessage
  { dmId     :: Maybe Int      -- ^ Request ID for matching responses
  , dmMethod :: Text           -- ^ Method name
  , dmParams :: Maybe Aeson.Value -- ^ Parameters
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Request from client to daemon
data DaemonRequest
  = ReqAnalyze AnalyzeRequest    -- ^ Analyze files
  | ReqStatus                    -- ^ Get daemon status
  | ReqStats                     -- ^ Get statistics
  | ReqReload                    -- ^ Reload configuration
  | ReqClearCache                -- ^ Clear analysis cache
  | ReqShutdown                  -- ^ Shutdown daemon
  | ReqPing                      -- ^ Health check
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Response from daemon to client
data DaemonResponse
  = RespAnalyze AnalyzeResponse    -- ^ Analysis results
  | RespStatus DaemonStatusInfo    -- ^ Status information
  | RespStats DaemonStats          -- ^ Statistics
  | RespOk Text                    -- ^ Success message
  | RespError Text                 -- ^ Error message
  | RespPong UTCTime               -- ^ Pong with server time
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Analysis request
data AnalyzeRequest = AnalyzeRequest
  { arPaths     :: [FilePath]      -- ^ Files/directories to analyze
  , arForce     :: Bool            -- ^ Force re-analysis (ignore cache)
  , arQuiet     :: Bool            -- ^ Only return counts, not full diagnostics
  , arTimeout   :: Maybe Int       -- ^ Per-file timeout override
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Analysis response
data AnalyzeResponse = AnalyzeResponse
  { arDiagnostics    :: [Diagnostic]
  , arFileCount      :: Int
  , arErrorCount     :: Int
  , arWarningCount   :: Int
  , arCacheHits      :: Int
  , arCacheMisses    :: Int
  , arElapsedMs      :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Daemon status information
data DaemonStatusInfo = DaemonStatusInfo
  { dsiRunning       :: Bool
  , dsiUptime        :: Double        -- ^ Seconds since start
  , dsiConnections   :: Int
  , dsiCachedFiles   :: Int
  , dsiVersion       :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Encode a message for transmission
encodeMessage :: Aeson.ToJSON a => a -> BS.ByteString
encodeMessage msg =
  let body = BL.toStrict $ encode msg
      len = BS.length body
      header = TE.encodeUtf8 $ T.pack $ show len <> "\n"
  in header <> body

-- | Decode a message from bytes
decodeMessage :: Aeson.FromJSON a => BS.ByteString -> Maybe a
decodeMessage bs = decode $ BL.fromStrict bs

--------------------------------------------------------------------------------
-- Socket Path
--------------------------------------------------------------------------------

-- | Get the default socket path
getDefaultSocketPath :: IO FilePath
getDefaultSocketPath = do
  cacheDir <- getXdgDirectory XdgCache "argus"
  createDirectoryIfMissing True cacheDir
  pure $ cacheDir </> "daemon.sock"

-- | Get the PID file path
getDaemonPidFile :: IO FilePath
getDaemonPidFile = do
  cacheDir <- getXdgDirectory XdgCache "argus"
  createDirectoryIfMissing True cacheDir
  pure $ cacheDir </> "daemon.pid"

--------------------------------------------------------------------------------
-- Daemon Operations
--------------------------------------------------------------------------------

-- | Run the daemon
runDaemon :: DaemonConfig -> IO ()
runDaemon cfg = do
  -- Load configurations
  lintCfg <- loadConfig Nothing
  rulesCfg <- loadRulesConfig Nothing

  -- Create state
  state <- newDaemonState cfg lintCfg rulesCfg

  -- Set up signal handlers
  void $ installHandler sigINT (Catch $ handleSignal state) Nothing
  void $ installHandler sigTERM (Catch $ handleSignal state) Nothing

  -- Get socket path
  socketPath <- case dcSocketPath cfg of
    Just p -> pure p
    Nothing -> getDefaultSocketPath

  -- Write PID file
  pidFile <- case dcPidFile cfg of
    Just p -> pure p
    Nothing -> getDaemonPidFile
  writeFile pidFile ""  -- Placeholder - actual PID writing handled by caller

  -- Clean up old socket
  socketExists <- doesFileExist socketPath
  when socketExists $ removeFile socketPath

  -- Start server
  logDaemon cfg "Starting Argus daemon..."

  case dcPort cfg of
    Just port -> runTcpServer state port
    Nothing   -> runUnixServer state socketPath

-- | Run Unix domain socket server
runUnixServer :: DaemonState -> FilePath -> IO ()
runUnixServer state socketPath = do
  cfg <- readTVarIO (dsConfig state)
  bracket (createUnixSocket socketPath) close $ \sock -> do
    atomically $ do
      writeTVar (dsSocket state) (Just sock)
      writeTVar (dsRunning state) True

    logDaemon cfg $ "Listening on: " <> T.pack socketPath

    -- Start idle timeout checker if configured
    case dcIdleTimeout cfg of
      Just idleTO -> void $ forkIO $ idleTimeoutChecker state idleTO
      Nothing -> pure ()

    -- Accept connections
    acceptLoop state sock

-- | Run TCP server
runTcpServer :: DaemonState -> Int -> IO ()
runTcpServer state port = do
  cfg <- readTVarIO (dsConfig state)
  let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just $ T.unpack $ dcHost cfg) (Just $ show port)
  bracket (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock (dcMaxConnections cfg)

    atomically $ do
      writeTVar (dsSocket state) (Just sock)
      writeTVar (dsRunning state) True

    logDaemon cfg $ "Listening on: " <> dcHost cfg <> ":" <> T.pack (show port)

    -- Start idle timeout checker if configured
    case dcIdleTimeout cfg of
      Just idleTO -> void $ forkIO $ idleTimeoutChecker state idleTO
      Nothing -> pure ()

    -- Accept connections
    acceptLoop state sock

-- | Create a Unix domain socket
createUnixSocket :: FilePath -> IO Socket
createUnixSocket path = do
  sock <- socket AF_UNIX Stream defaultProtocol
  bind sock (SockAddrUnix path)
  listen sock 5
  pure sock

-- | Accept connection loop
acceptLoop :: DaemonState -> Socket -> IO ()
acceptLoop state sock = do
  running <- readTVarIO (dsRunning state)
  when running $ do
    result <- try @SomeException $ accept sock
    case result of
      Left _ -> pure ()  -- Socket closed or error
      Right (conn, _) -> do
        -- Spawn handler thread
        tid <- forkIO $ handleConnection state conn
        atomicModifyIORef' (dsThreads state) $ \ts -> (tid:ts, ())
        acceptLoop state sock

-- | Handle a single connection
handleConnection :: DaemonState -> Socket -> IO ()
handleConnection state conn = do
  atomically $ modifyTVar' (dsConnections state) (+1)

  bracket (pure conn) close $ \_ -> do
    -- Read message length and body
    result <- try @SomeException $ readMessage conn
    case result of
      Left _ -> pure ()
      Right Nothing -> pure ()
      Right (Just msg) -> do
        -- Process request
        response <- processRequest state msg
        -- Send response
        let respBytes = encodeMessage response
        void $ try @SomeException $ NBS.sendAll conn respBytes

  atomically $ modifyTVar' (dsConnections state) (\n -> max 0 (n - 1))

-- | Read a framed message from socket
readMessage :: Socket -> IO (Maybe DaemonRequest)
readMessage sock = do
  -- Read length line
  lenBytes <- readLine sock
  case reads (T.unpack $ TE.decodeUtf8 lenBytes) of
    [(len, "")] -> do
      body <- recvExactly sock len
      pure $ decodeMessage body
    _ -> pure Nothing

-- | Read until newline
readLine :: Socket -> IO BS.ByteString
readLine sock = go []
  where
    go acc = do
      byte <- NBS.recv sock 1
      if BS.null byte
        then pure $ BS.concat (reverse acc)
        else if byte == "\n"
          then pure $ BS.concat (reverse acc)
          else go (byte : acc)

-- | Receive exactly n bytes
recvExactly :: Socket -> Int -> IO BS.ByteString
recvExactly sock n = go n []
  where
    go 0 acc = pure $ BS.concat (reverse acc)
    go remaining acc = do
      chunk <- NBS.recv sock (min remaining 4096)
      if BS.null chunk
        then pure $ BS.concat (reverse acc)
        else go (remaining - BS.length chunk) (chunk : acc)

-- | Process a request and return response
processRequest :: DaemonState -> DaemonRequest -> IO DaemonResponse
processRequest state req = do
  -- Update last activity
  now <- getCurrentTime
  atomically $ do
    writeTVar (dsLastActivity state) now
    modifyTVar' (dsStats state) $ \s -> s { dsTotalRequests = dsTotalRequests s + 1 }

  case req of
    ReqAnalyze analyzeReq -> processAnalyze state analyzeReq
    ReqStatus -> processStatus state
    ReqStats -> processStats state
    ReqReload -> processReload state
    ReqClearCache -> processClearCache state
    ReqShutdown -> processShutdown state
    ReqPing -> do
      now' <- getCurrentTime
      pure $ RespPong now'

-- | Process analyze request
processAnalyze :: DaemonState -> AnalyzeRequest -> IO DaemonResponse
processAnalyze state req = do
  startTime <- getCurrentTime

  cfg <- readTVarIO (dsConfig state)
  lintCfg <- readTVarIO (dsLinterConfig state)
  rulesCfg <- readTVarIO (dsRulesConfig state)
  cache <- readTVarIO (dsCache state)

  let opts = defaultOptions { optMode = QuickMode }
      ctx = defaultContext lintCfg opts rulesCfg
      baseResourceCfg = dcResourceConfig cfg
      resourceCfg = case arTimeout req of
        Just t -> baseResourceCfg { RL.rcTimeoutSeconds = Just t }
        Nothing -> baseResourceCfg

  -- Analyze each file
  (results, hits, misses) <- analyzeFilesWithCache ctx cache resourceCfg (arForce req) (arPaths req)

  -- Update cache
  atomically $ do
    modifyTVar' (dsStats state) $ \s -> s
      { dsTotalAnalyses = dsTotalAnalyses s + length (arPaths req)
      , dsCacheHits = dsCacheHits s + hits
      , dsCacheMisses = dsCacheMisses s + misses
      , dsTotalFiles = dsTotalFiles s + length results
      , dsLastAnalysisTime = Just startTime
      }

  endTime <- getCurrentTime
  let elapsed = realToFrac (diffUTCTime endTime startTime) * 1000 :: Double
      allDiags = concatMap snd results
      errors = length $ filter (\d -> diagSeverity d == Error) allDiags
      warnings = length $ filter (\d -> diagSeverity d == Warning) allDiags

  pure $ RespAnalyze AnalyzeResponse
    { arDiagnostics = if arQuiet req then [] else allDiags
    , arFileCount = length results
    , arErrorCount = errors
    , arWarningCount = warnings
    , arCacheHits = hits
    , arCacheMisses = misses
    , arElapsedMs = elapsed
    }

-- | Analyze files using cache
analyzeFilesWithCache
  :: AnalysisContext
  -> AnalysisCache
  -> RL.ResourceConfig
  -> Bool                   -- ^ Force re-analysis
  -> [FilePath]
  -> IO ([(FilePath, [Diagnostic])], Int, Int)  -- ^ (results, cache hits, cache misses)
analyzeFilesWithCache ctx cache resourceCfg force paths = do
  results <- mapM analyzeOne paths
  let hits = length $ filter (\(_, _, hit) -> hit) results
      misses = length $ filter (\(_, _, hit) -> not hit) results
  pure (map (\(p, d, _) -> (p, d)) results, hits, misses)
  where
    analyzeOne path = do
      -- Check cache first
      if not force
        then do
          hashResult <- hashFile path
          case hashResult of
            Left _ -> analyzeAndCache path
            Right fileHash ->
              case lookupCache path fileHash cache of
                Just entry -> pure (path, ceDiagnostics entry, True)
                Nothing -> analyzeAndCache path
        else analyzeAndCache path

    analyzeAndCache path = do
      let timeoutMicros = case RL.rcTimeoutSeconds resourceCfg of
            Just secs -> secs * 1_000_000
            Nothing -> 60 * 1_000_000  -- Default 60 seconds
      result <- try @SomeException $ timeout timeoutMicros $ analyzeFile ctx path
      case result of
        Right (Just fileResult) ->
          pure (path, fileResultDiagnostics fileResult, False)
        Right Nothing ->
          pure (path, [timeoutDiag path], False)
        Left err ->
          pure (path, [errorDiag path (T.pack $ show err)], False)

    timeoutDiag path = Diagnostic
      { diagSpan = mkSrcSpanRaw path 1 1 1 2
      , diagSeverity = Warning
      , diagKind = Custom "daemon"
      , diagMessage = "Analysis timed out"
      , diagCode = Just "daemon/timeout"
      , diagFixes = []
      , diagRelated = []
      }

    errorDiag path msg = Diagnostic
      { diagSpan = mkSrcSpanRaw path 1 1 1 2
      , diagSeverity = Error
      , diagKind = Custom "daemon"
      , diagMessage = msg
      , diagCode = Just "daemon/error"
      , diagFixes = []
      , diagRelated = []
      }

-- | Process status request
processStatus :: DaemonState -> IO DaemonResponse
processStatus state = do
  now <- getCurrentTime
  running <- readTVarIO (dsRunning state)
  stats <- readTVarIO (dsStats state)
  conns <- readTVarIO (dsConnections state)
  cache <- readTVarIO (dsCache state)

  let uptime = realToFrac (diffUTCTime now (dsStartTime stats)) :: Double
      cachedFiles = Map.size (Cache.acEntries cache)

  pure $ RespStatus DaemonStatusInfo
    { dsiRunning = running
    , dsiUptime = uptime
    , dsiConnections = conns
    , dsiCachedFiles = cachedFiles
    , dsiVersion = "1.0.0"
    }

-- | Process stats request
processStats :: DaemonState -> IO DaemonResponse
processStats state = do
  stats <- readTVarIO (dsStats state)
  pure $ RespStats stats

-- | Process reload request
processReload :: DaemonState -> IO DaemonResponse
processReload state = do
  cfg <- readTVarIO (dsConfig state)
  result <- try @SomeException $ do
    lintCfg <- loadConfig Nothing
    rulesCfg <- loadRulesConfig Nothing
    atomically $ do
      writeTVar (dsLinterConfig state) lintCfg
      writeTVar (dsRulesConfig state) rulesCfg
    pure ()
  case result of
    Left err -> pure $ RespError $ "Reload failed: " <> T.pack (show err)
    Right () -> do
      logDaemon cfg "Configuration reloaded"
      pure $ RespOk "Configuration reloaded"

-- | Process clear cache request
processClearCache :: DaemonState -> IO DaemonResponse
processClearCache state = do
  cache <- emptyCache
  atomically $ writeTVar (dsCache state) cache
  pure $ RespOk "Cache cleared"

-- | Process shutdown request
processShutdown :: DaemonState -> IO DaemonResponse
processShutdown state = do
  cfg <- readTVarIO (dsConfig state)
  logDaemon cfg "Shutdown requested"
  atomically $ writeTVar (dsRunning state) False

  -- Close socket
  mSock <- readTVarIO (dsSocket state)
  forM_ mSock $ \sock -> close sock

  -- Kill worker threads
  threads <- readIORef (dsThreads state)
  forM_ threads $ \tid -> killThread tid

  pure $ RespOk "Shutting down"

-- | Handle signal
handleSignal :: DaemonState -> IO ()
handleSignal state = void $ processShutdown state

-- | Idle timeout checker
idleTimeoutChecker :: DaemonState -> Int -> IO ()
idleTimeoutChecker state idleTimeout = forever $ do
  threadDelay (1000000 * 60)  -- Check every minute
  now <- getCurrentTime
  lastActivity <- readTVarIO (dsLastActivity state)
  let idle = realToFrac (diffUTCTime now lastActivity) :: Double
  when (idle > fromIntegral idleTimeout) $ do
    cfg <- readTVarIO (dsConfig state)
    logDaemon cfg $ "Idle timeout reached (" <> T.pack (show idleTimeout) <> "s)"
    void $ processShutdown state

-- | Stop the daemon
stopDaemon :: DaemonState -> IO ()
stopDaemon state = void $ processShutdown state

-- | Get daemon status
getDaemonStatus :: DaemonState -> IO DaemonStatusInfo
getDaemonStatus state = do
  RespStatus info <- processStatus state
  pure info

--------------------------------------------------------------------------------
-- Client Functions
--------------------------------------------------------------------------------

-- | Send a request to the daemon
sendRequest :: FilePath -> DaemonRequest -> IO (Either Text DaemonResponse)
sendRequest socketPath req = do
  result <- try @SomeException $ do
    sock <- socket AF_UNIX Stream defaultProtocol
    connect sock (SockAddrUnix socketPath)

    -- Send request
    let reqBytes = encodeMessage req
    NBS.sendAll sock reqBytes

    -- Read response
    respBytes <- readAll sock
    close sock

    case decodeMessage respBytes of
      Nothing -> pure $ Left "Failed to decode response"
      Just resp -> pure $ Right resp

  case result of
    Left err -> pure $ Left $ "Connection failed: " <> T.pack (show err)
    Right resp -> pure resp
  where
    readAll sock = go []
      where
        go acc = do
          chunk <- NBS.recv sock 4096
          if BS.null chunk
            then pure $ BS.concat (reverse acc)
            else go (chunk : acc)

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

-- | Log a message
logDaemon :: DaemonConfig -> Text -> IO ()
logDaemon cfg msg = when (dcVerbose cfg) $ do
  now <- getCurrentTime
  let logLine = T.pack (show now) <> " [argus-daemon] " <> msg
  case dcLogFile cfg of
    Nothing -> hPutStrLn stderr $ T.unpack logLine
    Just path -> appendFile path $ T.unpack logLine <> "\n"
