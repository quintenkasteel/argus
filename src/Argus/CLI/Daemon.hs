{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Daemon
-- Description : Daemon mode command
-- Copyright   : (c) 2024
-- License     : MIT
--
-- = Overview
--
-- This module implements the @argus daemon@ command, which runs Argus as
-- a background service providing fast analysis responses.
--
-- = Architecture
--
-- @
-- ┌─────────────────────────────────────────────────────────────────────┐
-- │                        Daemon Architecture                          │
-- │                                                                     │
-- │  ┌─────────────┐    ┌──────────────────────────────────────────┐   │
-- │  │   Client    │    │              Argus Daemon                 │   │
-- │  │             │    │  ┌─────────────────────────────────────┐  │   │
-- │  │ argus daemon│───►│  │           Request Handler           │  │   │
-- │  │   check ... │    │  └──────────────────┬──────────────────┘  │   │
-- │  └─────────────┘    │                     │                     │   │
-- │       Unix Socket   │       ┌─────────────┼─────────────┐       │   │
-- │       or TCP Port   │       ▼             ▼             ▼       │   │
-- │                     │  ┌────────┐   ┌──────────┐   ┌────────┐   │   │
-- │                     │  │ Cache  │   │ Analyzer │   │ Config │   │   │
-- │                     │  └────────┘   └──────────┘   └────────┘   │   │
-- │                     └──────────────────────────────────────────┘   │
-- └─────────────────────────────────────────────────────────────────────┘
-- @
--
-- = Commands
--
-- * @start@: Start the daemon process
-- * @stop@: Stop a running daemon
-- * @status@: Query daemon status and statistics
-- * @reload@: Reload configuration without restart
-- * @check@: Analyze files through the daemon
--
-- = Performance Benefits
--
-- * Cached parsed files avoid re-parsing
-- * Warm JIT for faster analysis
-- * Reduced startup overhead
--
-- @since 1.0.0
module Argus.CLI.Daemon
  ( -- * Command Entry Point
    runDaemon
  ) where

import Control.Monad (forM_, unless, when)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Exit (exitFailure)

import Argus.CLI.Types
import Argus.Daemon qualified as Daemon
import Argus.Types (diagMessage)

-- | Run the daemon command.
--
-- Dispatches to the appropriate daemon action (start, stop, status,
-- reload, or check).
--
-- @since 1.0.0
runDaemon :: GlobalOptions -> DaemonOptions -> IO ()
runDaemon _ opts = do
  socketPath <- case daSocketPath opts of
    Just p -> pure p
    Nothing -> Daemon.getDefaultSocketPath

  case daAction opts of
    DaemonStart -> do
      TIO.putStrLn "Starting Argus daemon..."
      let cfg = Daemon.defaultDaemonConfig
            { Daemon.dcSocketPath = daSocketPath opts
            , Daemon.dcPort = daPort opts
            , Daemon.dcIdleTimeout = daIdleTimeout opts
            , Daemon.dcVerbose = daVerbose opts
            }
      Daemon.runDaemon cfg

    DaemonStop -> do
      TIO.putStrLn "Stopping Argus daemon..."
      result <- Daemon.sendRequest socketPath Daemon.ReqShutdown
      case result of
        Right (Daemon.RespOk msg) -> TIO.putStrLn msg
        Right (Daemon.RespError err) -> do
          TIO.putStrLn $ "Error: " <> err
          exitFailure
        Left err -> do
          TIO.putStrLn $ "Failed to connect: " <> err
          exitFailure
        _ -> TIO.putStrLn "Unexpected response"

    DaemonStatus -> do
      result <- Daemon.sendRequest socketPath Daemon.ReqStatus
      case result of
        Right (Daemon.RespStatus statusInfo) -> do
          TIO.putStrLn "Argus Daemon Status:"
          TIO.putStrLn $ "  Running: " <> T.pack (show $ Daemon.dsiRunning statusInfo)
          TIO.putStrLn $ "  Uptime: " <> T.pack (show $ Daemon.dsiUptime statusInfo) <> " seconds"
          TIO.putStrLn $ "  Connections: " <> T.pack (show $ Daemon.dsiConnections statusInfo)
          TIO.putStrLn $ "  Cached files: " <> T.pack (show $ Daemon.dsiCachedFiles statusInfo)
          TIO.putStrLn $ "  Version: " <> Daemon.dsiVersion statusInfo
        Left err -> do
          TIO.putStrLn $ "Daemon not running: " <> err
        _ -> TIO.putStrLn "Unexpected response"

    DaemonReload -> do
      TIO.putStrLn "Reloading daemon configuration..."
      result <- Daemon.sendRequest socketPath Daemon.ReqReload
      case result of
        Right (Daemon.RespOk msg) -> TIO.putStrLn msg
        Right (Daemon.RespError err) -> do
          TIO.putStrLn $ "Error: " <> err
          exitFailure
        Left err -> do
          TIO.putStrLn $ "Failed to connect: " <> err
          exitFailure
        _ -> TIO.putStrLn "Unexpected response"

    DaemonCheck paths -> do
      let req = Daemon.AnalyzeRequest
            { Daemon.arPaths = if null paths then ["."] else paths
            , Daemon.arForce = False
            , Daemon.arQuiet = False
            , Daemon.arTimeout = Nothing
            }
      result <- Daemon.sendRequest socketPath (Daemon.ReqAnalyze req)
      case result of
        Right (Daemon.RespAnalyze resp) -> do
          TIO.putStrLn $ "Analyzed " <> T.pack (show $ Daemon.arFileCount resp) <> " files"
          TIO.putStrLn $ "  Errors: " <> T.pack (show $ Daemon.arErrorCount resp)
          TIO.putStrLn $ "  Warnings: " <> T.pack (show $ Daemon.arWarningCount resp)
          TIO.putStrLn $ "  Cache hits: " <> T.pack (show $ Daemon.arCacheHits resp)
          TIO.putStrLn $ "  Cache misses: " <> T.pack (show $ Daemon.arCacheMisses resp)
          TIO.putStrLn $ "  Elapsed: " <> T.pack (show $ Daemon.arElapsedMs resp) <> " ms"
          unless (null $ Daemon.arDiagnostics resp) $ do
            TIO.putStrLn ""
            forM_ (Daemon.arDiagnostics resp) $ \diag ->
              TIO.putStrLn $ diagMessage diag
          when (Daemon.arErrorCount resp > 0) exitFailure
        Right (Daemon.RespError err) -> do
          TIO.putStrLn $ "Error: " <> err
          exitFailure
        Left err -> do
          TIO.putStrLn $ "Failed to connect: " <> err
          exitFailure
        _ -> TIO.putStrLn "Unexpected response"
