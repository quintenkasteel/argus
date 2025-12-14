{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Plugin.HotReload
-- Description : Hot-reload support for Argus plugins
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- This module provides hot-reload functionality for Argus plugins,
-- allowing plugins to be added, updated, or removed without restarting
-- the analysis engine.
module Argus.Plugin.HotReload
  ( -- * Hot Reload Manager
    HotReloadManager
  , newHotReloadManager
  , shutdownManager
  , withHotReloadManager

    -- * Plugin Management
  , loadPlugin
  , unloadPlugin
  , reloadPlugin
  , reloadAllPlugins
  , listPlugins

    -- * File Watching
  , watchPluginDirectory
  , stopWatching
  , WatchConfig (..)
  , defaultWatchConfig

    -- * Events
  , PluginEvent (..)
  , EventCallback
  , registerEventCallback
  , unregisterEventCallback

    -- * Status
  , PluginStatus (..)
  , getPluginStatus
  , getManagerStatus
  , ManagerStatus (..)
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.DeepSeq (NFData)
import Control.Exception (SomeException, catch, try)
import Control.Monad (forM, forM_, void, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime)
import System.FilePath (takeExtension, takeFileName)
import System.FSNotify qualified as FS

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Hot reload manager state
data HotReloadManager = HotReloadManager
  { hrmPlugins :: TVar (Map Text LoadedPlugin)
  , hrmWatchManager :: MVar (Maybe FS.WatchManager)
  , hrmWatchStops :: IORef [FS.StopListening]
  , hrmCallbacks :: TVar [EventCallback]
  , hrmConfig :: WatchConfig
  , hrmStatus :: TVar ManagerStatus
  , hrmLastReload :: TVar (Map Text UTCTime)
  }

-- | A loaded plugin
data LoadedPlugin = LoadedPlugin
  { lpId :: Text
  , lpPath :: FilePath
  , lpStatus :: PluginStatus
  , lpLoadTime :: UTCTime
  , lpLastModified :: UTCTime
  , lpChecksum :: Text
  , lpDependencies :: [Text]
  , lpExports :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Plugin status
data PluginStatus
  = PluginLoaded
      -- ^ Successfully loaded and active
  | PluginLoading
      -- ^ Currently being loaded
  | PluginUnloading
      -- ^ Currently being unloaded
  | PluginError Text
      -- ^ Failed with error message
  | PluginDisabled
      -- ^ Manually disabled
  | PluginOutdated
      -- ^ File changed, needs reload
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Manager status
data ManagerStatus = ManagerStatus
  { msRunning :: Bool
  , msPluginCount :: Int
  , msLoadedCount :: Int
  , msErrorCount :: Int
  , msWatching :: Bool
  , msWatchedDirs :: [FilePath]
  , msLastActivity :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Plugin event
data PluginEvent
  = PluginAdded Text FilePath
      -- ^ New plugin detected
  | PluginRemoved Text
      -- ^ Plugin removed
  | PluginReloaded Text
      -- ^ Plugin reloaded
  | PluginFailed Text Text
      -- ^ Plugin failed with error
  | PluginModified Text FilePath
      -- ^ Plugin file modified
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Event callback type
type EventCallback = PluginEvent -> IO ()

-- | Watch configuration
data WatchConfig = WatchConfig
  { wcDebounceMs :: Int
      -- ^ Debounce period in milliseconds
  , wcExtensions :: [String]
      -- ^ File extensions to watch
  , wcIgnorePatterns :: [Text]
      -- ^ Patterns to ignore
  , wcAutoReload :: Bool
      -- ^ Automatically reload on change
  , wcMaxConcurrentLoads :: Int
      -- ^ Maximum concurrent plugin loads
  , wcLoadTimeout :: Int
      -- ^ Load timeout in seconds
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Default watch configuration
defaultWatchConfig :: WatchConfig
defaultWatchConfig = WatchConfig
  { wcDebounceMs = 500
  , wcExtensions = [".hs", ".toml", ".yaml"]
  , wcIgnorePatterns = [".*", "_*", "dist-*", ".stack-work"]
  , wcAutoReload = True
  , wcMaxConcurrentLoads = 4
  , wcLoadTimeout = 30
  }

--------------------------------------------------------------------------------
-- Manager Lifecycle
--------------------------------------------------------------------------------

-- | Create a new hot reload manager
newHotReloadManager :: WatchConfig -> IO HotReloadManager
newHotReloadManager config = do
  plugins <- newTVarIO Map.empty
  watchMgr <- newMVar Nothing
  watchStops <- newIORef []
  callbacks <- newTVarIO []
  status <- newTVarIO $ ManagerStatus
    { msRunning = True
    , msPluginCount = 0
    , msLoadedCount = 0
    , msErrorCount = 0
    , msWatching = False
    , msWatchedDirs = []
    , msLastActivity = Nothing
    }
  lastReload <- newTVarIO Map.empty

  return HotReloadManager
    { hrmPlugins = plugins
    , hrmWatchManager = watchMgr
    , hrmWatchStops = watchStops
    , hrmCallbacks = callbacks
    , hrmConfig = config
    , hrmStatus = status
    , hrmLastReload = lastReload
    }

-- | Shutdown the manager and release resources
shutdownManager :: HotReloadManager -> IO ()
shutdownManager mgr = do
  -- Stop all file watchers
  stops <- readIORef (hrmWatchStops mgr)
  mapM_ id stops
  writeIORef (hrmWatchStops mgr) []

  -- Stop watch manager
  modifyMVar_ (hrmWatchManager mgr) $ \mWm -> do
    case mWm of
      Just wm -> FS.stopManager wm
      Nothing -> return ()
    return Nothing

  -- Update status
  atomically $ modifyTVar' (hrmStatus mgr) $ \s -> s
    { msRunning = False
    , msWatching = False
    }

-- | Run an action with a hot reload manager
withHotReloadManager :: WatchConfig -> (HotReloadManager -> IO a) -> IO a
withHotReloadManager config action = do
  mgr <- newHotReloadManager config
  result <- action mgr `catch` \(e :: SomeException) -> do
    shutdownManager mgr
    error $ "Hot reload manager error: " <> show e
  shutdownManager mgr
  return result

--------------------------------------------------------------------------------
-- Plugin Management
--------------------------------------------------------------------------------

-- | Load a plugin from file
loadPlugin :: HotReloadManager -> FilePath -> IO (Either Text Text)
loadPlugin mgr path = do
  let pluginId = T.pack $ takeFileName path

  -- Check if already loaded
  existing <- atomically $ Map.lookup pluginId <$> readTVar (hrmPlugins mgr)
  case existing of
    Just lp | lpStatus lp == PluginLoaded ->
      return $ Left $ "Plugin already loaded: " <> pluginId
    _ -> doLoad pluginId
  where
    doLoad pluginId = do
      now <- getCurrentTime

      -- Check file exists
      exists <- doesFileExist path
      if not exists
        then return $ Left $ "Plugin file not found: " <> T.pack path
        else do
          modTime <- getModificationTime path

          -- Create plugin entry with Loading status
          let plugin = LoadedPlugin
                { lpId = pluginId
                , lpPath = path
                , lpStatus = PluginLoading
                , lpLoadTime = now
                , lpLastModified = modTime
                , lpChecksum = T.pack $ show modTime
                , lpDependencies = []
                , lpExports = []
                }

          atomically $ modifyTVar' (hrmPlugins mgr) $ Map.insert pluginId plugin

          -- Attempt to load
          loadResult <- tryLoadPlugin path
          case loadResult of
            Left err -> do
              let failedPlugin = plugin
                    { lpStatus = PluginError err
                    }
              atomically $ modifyTVar' (hrmPlugins mgr) $ Map.insert pluginId failedPlugin
              notifyEvent mgr $ PluginFailed pluginId err
              updateStatus mgr
              return $ Left err

            Right (deps, exports) -> do
              let loadedPlugin = plugin
                    { lpStatus = PluginLoaded
                    , lpDependencies = deps
                    , lpExports = exports
                    }
              atomically $ modifyTVar' (hrmPlugins mgr) $ Map.insert pluginId loadedPlugin
              notifyEvent mgr $ PluginAdded pluginId path
              updateStatus mgr
              return $ Right pluginId

-- | Try to load a plugin file
tryLoadPlugin :: FilePath -> IO (Either Text ([Text], [Text]))
tryLoadPlugin path = do
  let ext = takeExtension path
  case ext of
    ".toml" -> loadTomlPlugin path
    ".yaml" -> loadYamlPlugin path
    ".hs" -> loadHaskellPlugin path
    _ -> return $ Left $ "Unknown plugin extension: " <> T.pack ext

-- | Load a TOML plugin
loadTomlPlugin :: FilePath -> IO (Either Text ([Text], [Text]))
loadTomlPlugin path = do
  result <- try $ readFile path
  case result of
    Left (e :: SomeException) -> return $ Left $ T.pack $ show e
    Right content ->
      -- Parse TOML and extract plugin info
      return $ Right ([], ["rules"])

-- | Load a YAML plugin
loadYamlPlugin :: FilePath -> IO (Either Text ([Text], [Text]))
loadYamlPlugin path = do
  result <- try $ readFile path
  case result of
    Left (e :: SomeException) -> return $ Left $ T.pack $ show e
    Right content ->
      return $ Right ([], ["rules"])

-- | Load a Haskell plugin
loadHaskellPlugin :: FilePath -> IO (Either Text ([Text], [Text]))
loadHaskellPlugin path = do
  -- In a real implementation, this would use hint or plugins package
  -- For now, just check if the file is valid Haskell
  result <- try $ readFile path
  case result of
    Left (e :: SomeException) -> return $ Left $ T.pack $ show e
    Right content -> do
      let hasModule = "module " `T.isInfixOf` T.pack content
      if hasModule
        then return $ Right (["base"], ["plugin"])
        else return $ Left "Invalid Haskell plugin: no module declaration"

-- | Unload a plugin
unloadPlugin :: HotReloadManager -> Text -> IO (Either Text ())
unloadPlugin mgr pluginId = do
  mPlugin <- atomically $ Map.lookup pluginId <$> readTVar (hrmPlugins mgr)
  case mPlugin of
    Nothing -> return $ Left $ "Plugin not found: " <> pluginId
    Just plugin -> do
      -- Set to unloading status
      atomically $ modifyTVar' (hrmPlugins mgr) $
        Map.adjust (\p -> p { lpStatus = PluginUnloading }) pluginId

      -- Perform cleanup
      cleanup plugin

      -- Remove from registry
      atomically $ modifyTVar' (hrmPlugins mgr) $ Map.delete pluginId
      notifyEvent mgr $ PluginRemoved pluginId
      updateStatus mgr
      return $ Right ()
  where
    cleanup _ = return ()  -- Placeholder for actual cleanup

-- | Reload a plugin
reloadPlugin :: HotReloadManager -> Text -> IO (Either Text ())
reloadPlugin mgr pluginId = do
  mPlugin <- atomically $ Map.lookup pluginId <$> readTVar (hrmPlugins mgr)
  case mPlugin of
    Nothing -> return $ Left $ "Plugin not found: " <> pluginId
    Just plugin -> do
      -- Check debounce
      now <- getCurrentTime
      lastReloads <- atomically $ readTVar (hrmLastReload mgr)
      let debounceTime = fromIntegral (wcDebounceMs (hrmConfig mgr)) / 1000
      case Map.lookup pluginId lastReloads of
        Just lastTime | diffUTCTime now lastTime < debounceTime ->
          return $ Left "Reload debounced"
        _ -> do
          atomically $ modifyTVar' (hrmLastReload mgr) $ Map.insert pluginId now

          -- Unload and reload
          unloadResult <- unloadPlugin mgr pluginId
          case unloadResult of
            Left err -> return $ Left err
            Right () -> do
              loadResult <- loadPlugin mgr (lpPath plugin)
              case loadResult of
                Left err -> return $ Left err
                Right _ -> do
                  notifyEvent mgr $ PluginReloaded pluginId
                  return $ Right ()

-- | Reload all plugins
reloadAllPlugins :: HotReloadManager -> IO [(Text, Either Text ())]
reloadAllPlugins mgr = do
  plugins <- atomically $ readTVar (hrmPlugins mgr)
  forM (Map.keys plugins) $ \pluginId -> do
    result <- reloadPlugin mgr pluginId
    return (pluginId, result)

-- | List all plugins
listPlugins :: HotReloadManager -> IO [LoadedPlugin]
listPlugins mgr = Map.elems <$> atomically (readTVar (hrmPlugins mgr))

--------------------------------------------------------------------------------
-- File Watching
--------------------------------------------------------------------------------

-- | Watch a directory for plugin changes
watchPluginDirectory :: HotReloadManager -> FilePath -> IO (Either Text ())
watchPluginDirectory mgr dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return $ Left $ "Directory not found: " <> T.pack dir
    else do
      -- Initialize watch manager if needed
      modifyMVar (hrmWatchManager mgr) $ \mWm -> do
        wm <- case mWm of
          Just w -> return w
          Nothing -> FS.startManager

        -- Create watcher
        stop <- FS.watchDir wm dir shouldWatch (handleEvent mgr)
        modifyIORef' (hrmWatchStops mgr) (stop :)

        -- Update status
        atomically $ modifyTVar' (hrmStatus mgr) $ \s -> s
          { msWatching = True
          , msWatchedDirs = dir : msWatchedDirs s
          }

        return (Just wm, Right ())
  where
    shouldWatch event =
      let path = FS.eventPath event
          ext = takeExtension path
          name = takeFileName path
          config = hrmConfig mgr
          validExt = ext `elem` wcExtensions config
          notIgnored = not $ any (`T.isPrefixOf` T.pack name) (wcIgnorePatterns config)
      in validExt && notIgnored

-- | Handle file system event
handleEvent :: HotReloadManager -> FS.Event -> IO ()
handleEvent mgr event = do
  let path = FS.eventPath event
      pluginId = T.pack $ takeFileName path

  case event of
    FS.Added _ _ _ -> when (wcAutoReload $ hrmConfig mgr) $ do
      void $ loadPlugin mgr path

    FS.Modified _ _ _ -> do
      -- Mark as outdated
      atomically $ modifyTVar' (hrmPlugins mgr) $
        Map.adjust (\p -> p { lpStatus = PluginOutdated }) pluginId
      notifyEvent mgr $ PluginModified pluginId path

      -- Auto-reload if configured
      when (wcAutoReload $ hrmConfig mgr) $
        void $ reloadPlugin mgr pluginId

    FS.Removed _ _ _ -> do
      void $ unloadPlugin mgr pluginId

    _ -> return ()

-- | Stop watching all directories
stopWatching :: HotReloadManager -> IO ()
stopWatching mgr = do
  stops <- readIORef (hrmWatchStops mgr)
  mapM_ id stops
  writeIORef (hrmWatchStops mgr) []

  atomically $ modifyTVar' (hrmStatus mgr) $ \s -> s
    { msWatching = False
    , msWatchedDirs = []
    }

--------------------------------------------------------------------------------
-- Event System
--------------------------------------------------------------------------------

-- | Register an event callback
registerEventCallback :: HotReloadManager -> EventCallback -> IO ()
registerEventCallback mgr callback =
  atomically $ modifyTVar' (hrmCallbacks mgr) (callback :)

-- | Unregister all event callbacks
unregisterEventCallback :: HotReloadManager -> IO ()
unregisterEventCallback mgr =
  atomically $ writeTVar (hrmCallbacks mgr) []

-- | Notify all callbacks of an event
notifyEvent :: HotReloadManager -> PluginEvent -> IO ()
notifyEvent mgr event = do
  callbacks <- atomically $ readTVar (hrmCallbacks mgr)
  forM_ callbacks $ \callback ->
    callback event `catch` \(_ :: SomeException) -> return ()

--------------------------------------------------------------------------------
-- Status
--------------------------------------------------------------------------------

-- | Get plugin status
getPluginStatus :: HotReloadManager -> Text -> IO (Maybe PluginStatus)
getPluginStatus mgr pluginId = do
  mPlugin <- atomically $ Map.lookup pluginId <$> readTVar (hrmPlugins mgr)
  return $ lpStatus <$> mPlugin

-- | Get manager status
getManagerStatus :: HotReloadManager -> IO ManagerStatus
getManagerStatus mgr = atomically $ readTVar (hrmStatus mgr)

-- | Update manager status
updateStatus :: HotReloadManager -> IO ()
updateStatus mgr = do
  now <- getCurrentTime
  plugins <- atomically $ readTVar (hrmPlugins mgr)
  let pluginCount = Map.size plugins
      loadedCount = length $ filter ((== PluginLoaded) . lpStatus) $ Map.elems plugins
      errorCount = length $ filter isError $ Map.elems plugins

  atomically $ modifyTVar' (hrmStatus mgr) $ \s -> s
    { msPluginCount = pluginCount
    , msLoadedCount = loadedCount
    , msErrorCount = errorCount
    , msLastActivity = Just now
    }
  where
    isError lp = case lpStatus lp of
      PluginError _ -> True
      _ -> False
