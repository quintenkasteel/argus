{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Plugin.Registry
-- Description : Plugin registry and lifecycle management
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module manages the plugin lifecycle including:
--
-- * Plugin registration and discovery
-- * Dependency resolution
-- * Plugin initialization and shutdown
-- * Rule/fixer aggregation
--
-- == Usage
--
-- @
-- registry <- newPluginRegistry defaultRegistryConfig
-- loadPlugin registry myPlugin
-- rules <- getActiveRules registry
-- @
module Argus.Plugin.Registry
  ( -- * Registry
    PluginRegistry (..)
  , newPluginRegistry
  , RegistryConfig (..)
  , defaultRegistryConfig

    -- * Plugin Management
  , registerPlugin
  , unregisterPlugin
  , enablePlugin
  , disablePlugin
  , reloadPlugin

    -- * Plugin Discovery
  , discoverPlugins
  , loadPluginFromFile
  , loadPluginFromDirectory

    -- * Query Functions
  , getActivePlugins
  , getPluginById
  , getPluginStatus
  , getAllRules
  , getAllFixers
  , getAllHooks
  , getAllFormatters

    -- * Execution
  , executeRules
  , executeFixes
  , executeHooks
  , formatOutput

    -- * Registry State
  , RegistryState (..)
  , getRegistryState
  , PluginInfo (..)
  ) where

import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Control.Monad (forM, forM_, when)
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Ord (Down(..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)

import Argus.Types
  ( Diagnostic(..)
  , DiagnosticKind(..)
  , Severity(..)
  , SrcSpan(..)
  , Line(..)
  , Column(..)
  )
import Argus.Plugin.Types

--------------------------------------------------------------------------------
-- Registry Configuration
--------------------------------------------------------------------------------

-- | Configuration for the plugin registry
data RegistryConfig = RegistryConfig
  { rcPluginDirs        :: [FilePath]
      -- ^ Directories to search for plugins
  , rcAutoDiscover      :: Bool
      -- ^ Auto-discover plugins in plugin directories
  , rcEnableByDefault   :: Bool
      -- ^ Enable newly loaded plugins by default
  , rcMaxPlugins        :: Int
      -- ^ Maximum number of plugins to load
  , rcAllowDuplicates   :: Bool
      -- ^ Allow plugins with same ID (use latest version)
  , rcStrictDependencies :: Bool
      -- ^ Fail if dependencies are missing
  , rcArgusVersion      :: Version
      -- ^ Current Argus version for compatibility
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default registry configuration
defaultRegistryConfig :: RegistryConfig
defaultRegistryConfig = RegistryConfig
  { rcPluginDirs = [".argus/plugins", "~/.config/argus/plugins"]
  , rcAutoDiscover = True
  , rcEnableByDefault = True
  , rcMaxPlugins = 100
  , rcAllowDuplicates = False
  , rcStrictDependencies = False
  , rcArgusVersion = Version 1 0 0 Nothing
  }

--------------------------------------------------------------------------------
-- Registry State
--------------------------------------------------------------------------------

-- | Plugin registry
data PluginRegistry = PluginRegistry
  { prConfig    :: RegistryConfig
  , prState     :: TVar RegistryState
  }

-- | Registry state
data RegistryState = RegistryState
  { rsPlugins       :: Map Text PluginInfo
      -- ^ All registered plugins by ID
  , rsEnabledIds    :: Set Text
      -- ^ IDs of enabled plugins
  , rsLoadOrder     :: [Text]
      -- ^ Order in which plugins were loaded
  , rsRuleIndex     :: Map Text (Text, PluginRule)
      -- ^ Rule ID -> (Plugin ID, Rule)
  , rsFixerIndex    :: Map Text (Text, PluginFixer)
      -- ^ Fixer ID -> (Plugin ID, Fixer)
  , rsHookIndex     :: Map HookPoint [(Text, PluginHook)]
      -- ^ Hooks by execution point
  , rsFormatterIndex :: Map Text (Text, PluginFormatter)
      -- ^ Formatter ID -> (Plugin ID, Formatter)
  , rsErrors        :: [PluginError]
      -- ^ Accumulated errors
  }
  deriving stock (Generic)

-- | Information about a loaded plugin
data PluginInfo = PluginInfo
  { piPlugin    :: ArgusPlugin
  , piStatus    :: PluginStatus
  , piConfig    :: Map Text ConfigValue
  , piLoadTime  :: Integer  -- UTC timestamp in microseconds
  }

--------------------------------------------------------------------------------
-- Registry Creation
--------------------------------------------------------------------------------

-- | Create a new plugin registry
newPluginRegistry :: RegistryConfig -> IO PluginRegistry
newPluginRegistry config = do
  state <- newTVarIO emptyRegistryState
  let registry = PluginRegistry config state

  -- Auto-discover plugins if enabled
  when (rcAutoDiscover config) $ do
    forM_ (rcPluginDirs config) $ \dir -> do
      exists <- doesDirectoryExist dir
      when exists $ do
        _ <- discoverPlugins registry dir
        pure ()

  pure registry

-- | Empty registry state
emptyRegistryState :: RegistryState
emptyRegistryState = RegistryState
  { rsPlugins = Map.empty
  , rsEnabledIds = Set.empty
  , rsLoadOrder = []
  , rsRuleIndex = Map.empty
  , rsFixerIndex = Map.empty
  , rsHookIndex = Map.empty
  , rsFormatterIndex = Map.empty
  , rsErrors = []
  }

--------------------------------------------------------------------------------
-- Plugin Management
--------------------------------------------------------------------------------

-- | Register a plugin
registerPlugin :: PluginRegistry -> ArgusPlugin -> IO (Either PluginError ())
registerPlugin registry plugin = do
  let pluginId = pmId $ pluginMetadata plugin
      config = prConfig registry

  -- Check version compatibility
  case checkVersionCompatibility config (pluginMetadata plugin) of
    Just err -> pure $ Left err
    Nothing -> do
      -- Check dependencies
      depResult <- checkDependencies registry plugin
      case depResult of
        Left err -> pure $ Left err
        Right () -> do
          -- Initialize the plugin
          initResult <- pluginInit plugin
          case initResult of
            Left err -> pure $ Left err
            Right () -> do
              -- Add to registry
              timestamp <- getCurrentTimeMicros
              let info = PluginInfo
                    { piPlugin = plugin
                    , piStatus = if rcEnableByDefault config
                                 then PluginEnabled
                                 else PluginDisabled
                    , piConfig = maybe Map.empty pcpDefaults (pluginConfig plugin)
                    , piLoadTime = timestamp
                    }

              atomically $ modifyTVar' (prState registry) $ \st ->
                let newPlugins = Map.insert pluginId info (rsPlugins st)
                    newEnabled = if rcEnableByDefault config
                                 then Set.insert pluginId (rsEnabledIds st)
                                 else rsEnabledIds st
                    newOrder = rsLoadOrder st ++ [pluginId]
                in rebuildIndices st
                     { rsPlugins = newPlugins
                     , rsEnabledIds = newEnabled
                     , rsLoadOrder = newOrder
                     }

              pure $ Right ()

-- | Unregister a plugin
unregisterPlugin :: PluginRegistry -> Text -> IO ()
unregisterPlugin registry pluginId = do
  mPlugin <- atomically $ do
    st <- readTVar (prState registry)
    case Map.lookup pluginId (rsPlugins st) of
      Nothing -> pure Nothing
      Just info -> do
        let newPlugins = Map.delete pluginId (rsPlugins st)
            newEnabled = Set.delete pluginId (rsEnabledIds st)
            newOrder = filter (/= pluginId) (rsLoadOrder st)
        writeTVar (prState registry) $ rebuildIndices st
          { rsPlugins = newPlugins
          , rsEnabledIds = newEnabled
          , rsLoadOrder = newOrder
          }
        pure $ Just (piPlugin info)

  -- Shutdown the plugin
  case mPlugin of
    Nothing -> pure ()
    Just plugin -> pluginShutdown plugin

-- | Enable a plugin
enablePlugin :: PluginRegistry -> Text -> IO Bool
enablePlugin registry pluginId = atomically $ do
  st <- readTVar (prState registry)
  if Map.member pluginId (rsPlugins st)
    then do
      let newEnabled = Set.insert pluginId (rsEnabledIds st)
          newPlugins = Map.adjust (\pi' -> pi' { piStatus = PluginEnabled }) pluginId (rsPlugins st)
      writeTVar (prState registry) $ rebuildIndices st
        { rsPlugins = newPlugins
        , rsEnabledIds = newEnabled
        }
      pure True
    else pure False

-- | Disable a plugin
disablePlugin :: PluginRegistry -> Text -> IO Bool
disablePlugin registry pluginId = atomically $ do
  st <- readTVar (prState registry)
  if Map.member pluginId (rsPlugins st)
    then do
      let newEnabled = Set.delete pluginId (rsEnabledIds st)
          newPlugins = Map.adjust (\pi' -> pi' { piStatus = PluginDisabled }) pluginId (rsPlugins st)
      writeTVar (prState registry) $ rebuildIndices st
        { rsPlugins = newPlugins
        , rsEnabledIds = newEnabled
        }
      pure True
    else pure False

-- | Reload a plugin
reloadPlugin :: PluginRegistry -> Text -> ArgusPlugin -> IO (Either PluginError ())
reloadPlugin registry pluginId newPlugin = do
  unregisterPlugin registry pluginId
  registerPlugin registry newPlugin

--------------------------------------------------------------------------------
-- Plugin Discovery
--------------------------------------------------------------------------------

-- | Discover plugins in a directory
discoverPlugins :: PluginRegistry -> FilePath -> IO [Either PluginError ArgusPlugin]
discoverPlugins registry dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- listDirectory dir
      let pluginFiles = filter isPluginFile entries
      forM pluginFiles $ \file ->
        loadPluginFromFile registry (dir </> file)

-- | Check if a file looks like a plugin
isPluginFile :: FilePath -> Bool
isPluginFile path =
  takeExtension path == ".argus-plugin" ||
  takeExtension path == ".json"

-- | Load a plugin from a file (plugin manifest)
loadPluginFromFile :: PluginRegistry -> FilePath -> IO (Either PluginError ArgusPlugin)
loadPluginFromFile _registry path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ PluginLoadErr $ PluginNotFound $ T.pack path
    else do
      content <- BL.readFile path
      case Aeson.decode content of
        Nothing -> pure $ Left $ PluginLoadErr $ PluginInvalidFormat $ T.pack path
        Just manifest -> pure $ manifestToPlugin manifest

-- | Load plugins from a directory
loadPluginFromDirectory :: PluginRegistry -> FilePath -> IO [Either PluginError ArgusPlugin]
loadPluginFromDirectory = discoverPlugins

-- | Plugin manifest (JSON serializable plugin definition)
data PluginManifest = PluginManifest
  { manifestId          :: Text
  , manifestName        :: Text
  , manifestVersion     :: Text
  , manifestAuthor      :: Text
  , manifestDescription :: Text
  , manifestHomepage    :: Maybe Text
  , manifestLicense     :: Maybe Text
  , manifestRules       :: [RuleManifest]
  , manifestDependencies :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Rule manifest
data RuleManifest = RuleManifest
  { ruleId          :: Text
  , ruleName        :: Text
  , ruleDescription :: Text
  , ruleSeverity    :: Text
  , ruleCategory    :: Text
  , ruleTags        :: [Text]
  , rulePattern     :: Maybe Text
  , ruleMessage     :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Convert manifest to plugin
manifestToPlugin :: PluginManifest -> Either PluginError ArgusPlugin
manifestToPlugin PluginManifest{..} = do
  version <- case parseVersion manifestVersion of
    Nothing -> Left $ PluginLoadErr $ PluginInvalidFormat "Invalid version"
    Just v -> Right v

  let metadata = PluginMetadata
        { pmId = manifestId
        , pmName = manifestName
        , pmVersion = version
        , pmAuthor = manifestAuthor
        , pmDescription = manifestDescription
        , pmHomepage = manifestHomepage
        , pmLicense = manifestLicense
        , pmCapabilities = [CapabilityRules]
        , pmMinArgusVersion = Nothing
        , pmMaxArgusVersion = Nothing
        }

      rules = map manifestRuleToRule manifestRules

  Right ArgusPlugin
    { pluginMetadata = metadata
    , pluginRules = rules
    , pluginFixers = []
    , pluginHooks = []
    , pluginFormatters = []
    , pluginConfig = Nothing
    , pluginDependencies = manifestDependencies
    , pluginInit = pure $ Right ()
    , pluginShutdown = pure ()
    }

-- | Convert rule manifest to plugin rule
manifestRuleToRule :: RuleManifest -> PluginRule
manifestRuleToRule RuleManifest{..} = PluginRule
  { prId = ruleId
  , prName = ruleName
  , prDescription = ruleDescription
  , prSeverity = parseSeverity ruleSeverity
  , prCategory = ruleCategory
  , prTags = ruleTags
  , prEnabled = True
  , prPriority = PriorityNormal
  , prFunction = patternMatcher rulePattern ruleMessage
  }
  where
    parseSeverity "error" = Error
    parseSeverity "warning" = Warning
    parseSeverity "info" = Info
    parseSeverity "hint" = Suggestion
    parseSeverity _ = Warning

    patternMatcher Nothing _ _ = pure []
    patternMatcher (Just pattern') msg ctx = do
      let content = rcFileContent ctx
          path = rcFilePath ctx
      -- Simple pattern matching (could be enhanced with regex)
      if pattern' `T.isInfixOf` content
        then pure [RuleMatch
          { rmSpan = defaultSpanForPath path
          , rmKind = MatchWarning
          , rmMessage = msg
          , rmSuggestion = Nothing
          , rmContext = Map.empty
          }]
        else pure []

    defaultSpanForPath path' = SrcSpan
      { srcSpanFile = path'
      , srcSpanStartLine = Line 1
      , srcSpanStartCol = Column 1
      , srcSpanEndLine = Line 1
      , srcSpanEndCol = Column 1
      }

--------------------------------------------------------------------------------
-- Query Functions
--------------------------------------------------------------------------------

-- | Get all active plugins
getActivePlugins :: PluginRegistry -> IO [ArgusPlugin]
getActivePlugins registry = atomically $ do
  st <- readTVar (prState registry)
  let enabledIds = rsEnabledIds st
      plugins = rsPlugins st
  pure $ mapMaybe (\pid -> piPlugin <$> Map.lookup pid plugins) $
         filter (`Set.member` enabledIds) (rsLoadOrder st)

-- | Get a plugin by ID
getPluginById :: PluginRegistry -> Text -> IO (Maybe ArgusPlugin)
getPluginById registry pluginId = atomically $ do
  st <- readTVar (prState registry)
  pure $ piPlugin <$> Map.lookup pluginId (rsPlugins st)

-- | Get plugin status
getPluginStatus :: PluginRegistry -> Text -> IO (Maybe PluginStatus)
getPluginStatus registry pluginId = atomically $ do
  st <- readTVar (prState registry)
  pure $ piStatus <$> Map.lookup pluginId (rsPlugins st)

-- | Get all active rules
getAllRules :: PluginRegistry -> IO [PluginRule]
getAllRules registry = atomically $ do
  st <- readTVar (prState registry)
  let enabledIds = rsEnabledIds st
      rules = [(pid, r) | (_, (pid, r)) <- Map.toList (rsRuleIndex st)]
      enabledRules = filter (\(pid, _) -> Set.member pid enabledIds) rules
  pure $ map snd $ sortOn (rulePriorityOrd . snd) enabledRules

-- | Get all active fixers
getAllFixers :: PluginRegistry -> IO [PluginFixer]
getAllFixers registry = atomically $ do
  st <- readTVar (prState registry)
  let enabledIds = rsEnabledIds st
      fixers = [(pid, f) | (_, (pid, f)) <- Map.toList (rsFixerIndex st)]
      enabledFixers = filter (\(pid, _) -> Set.member pid enabledIds) fixers
  pure $ map snd $ sortOn (fixerPriorityOrd . snd) enabledFixers

-- | Get all active hooks for a point
getAllHooks :: PluginRegistry -> HookPoint -> IO [PluginHook]
getAllHooks registry point = atomically $ do
  st <- readTVar (prState registry)
  let enabledIds = rsEnabledIds st
      hooks = fromMaybe [] $ Map.lookup point (rsHookIndex st)
      enabledHooks = filter (\(pid, _) -> Set.member pid enabledIds) hooks
  pure $ map snd $ sortOn (hookPriorityOrd . snd) enabledHooks

-- | Get all active formatters
getAllFormatters :: PluginRegistry -> IO [PluginFormatter]
getAllFormatters registry = atomically $ do
  st <- readTVar (prState registry)
  let enabledIds = rsEnabledIds st
      formatters = [(pid, f) | (_, (pid, f)) <- Map.toList (rsFormatterIndex st)]
      enabledFormatters = filter (\(pid, _) -> Set.member pid enabledIds) formatters
  pure $ map snd enabledFormatters

-- | Get current registry state
getRegistryState :: PluginRegistry -> IO RegistryState
getRegistryState registry = readTVarIO (prState registry)

--------------------------------------------------------------------------------
-- Execution
--------------------------------------------------------------------------------

-- | Execute all active rules
executeRules :: PluginRegistry -> RuleContext -> IO [Diagnostic]
executeRules registry ctx = do
  rules <- getAllRules registry
  results <- forM rules $ \rule -> do
    result <- try @SomeException $ prFunction rule ctx
    case result of
      Left e -> do
        let err = PluginRuntimeErr $ RuleExecutionError (prId rule) (T.pack $ show e)
        atomically $ modifyTVar' (prState registry) $ \st ->
          st { rsErrors = err : rsErrors st }
        pure []
      Right matches -> pure $ map (ruleMatchToDiagnostic rule) matches
  pure $ concat results

-- | Execute fixers for a diagnostic
executeFixes :: PluginRegistry -> FixerContext -> IO [FixerMatch]
executeFixes registry ctx = do
  fixers <- getAllFixers registry
  let targetRule = diagCode (fcDiagnostic ctx)
  let applicableFixers = filter (fixerMatchesRule targetRule) fixers
  results <- forM applicableFixers $ \fixer -> do
    result <- try @SomeException $ pfFunction fixer ctx
    case result of
      Left e -> do
        let err = PluginRuntimeErr $ FixerExecutionError (pfId fixer) (T.pack $ show e)
        atomically $ modifyTVar' (prState registry) $ \st ->
          st { rsErrors = err : rsErrors st }
        pure []
      Right (FixerSuccess matches) -> pure matches
      Right _ -> pure []
  pure $ concat results

-- | Execute hooks at a point
executeHooks :: PluginRegistry -> HookContext -> IO HookResult
executeHooks registry ctx = do
  hooks <- getAllHooks registry (hcPoint ctx)
  executeHooksSequence hooks ctx

-- | Execute hooks in sequence
executeHooksSequence :: [PluginHook] -> HookContext -> IO HookResult
executeHooksSequence [] _ = pure HookContinue
executeHooksSequence (hook:rest) ctx = do
  result <- try @SomeException $ phFunction hook ctx
  case result of
    Left _ -> executeHooksSequence rest ctx  -- Continue on error
    Right HookContinue -> executeHooksSequence rest ctx
    Right (HookAddDiagnostics diags) ->
      let ctx' = ctx { hcDiagnostics = hcDiagnostics ctx ++ diags }
      in executeHooksSequence rest ctx'
    Right r -> pure r  -- Abort or ModifyDiagnostics stops the chain

-- | Format output using a specific formatter
formatOutput :: PluginRegistry -> Text -> FormatContext -> IO (Either Text Text)
formatOutput registry formatterId ctx = do
  st <- readTVarIO (prState registry)
  case Map.lookup formatterId (rsFormatterIndex st) of
    Nothing -> pure $ Left $ "Formatter not found: " <> formatterId
    Just (_, formatter) -> do
      result <- try @SomeException $ pformFunction formatter ctx
      case result of
        Left e -> pure $ Left $ T.pack $ show e
        Right output -> pure $ Right output

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Rebuild indices after state change
rebuildIndices :: RegistryState -> RegistryState
rebuildIndices st =
  let allPlugins = Map.toList (rsPlugins st)
      enabledPlugins = filter (\(pid, _) -> Set.member pid (rsEnabledIds st)) allPlugins

      ruleIdx = Map.fromList
        [ (pluginId <> ":" <> prId r, (pluginId, r))
        | (pluginId, info) <- enabledPlugins
        , r <- pluginRules (piPlugin info)
        ]

      fixerIdx = Map.fromList
        [ (pluginId <> ":" <> pfId f, (pluginId, f))
        | (pluginId, info) <- enabledPlugins
        , f <- pluginFixers (piPlugin info)
        ]

      hookIdx = Map.fromListWith (++)
        [ (phPoint h, [(pluginId, h)])
        | (pluginId, info) <- enabledPlugins
        , h <- pluginHooks (piPlugin info)
        ]

      formatterIdx = Map.fromList
        [ (pformId f, (pluginId, f))
        | (pluginId, info) <- enabledPlugins
        , f <- pluginFormatters (piPlugin info)
        ]
  in st
    { rsRuleIndex = ruleIdx
    , rsFixerIndex = fixerIdx
    , rsHookIndex = hookIdx
    , rsFormatterIndex = formatterIdx
    }

-- | Check version compatibility
checkVersionCompatibility :: RegistryConfig -> PluginMetadata -> Maybe PluginError
checkVersionCompatibility config meta =
  let argusVersion = rcArgusVersion config
      minVersion = pmMinArgusVersion meta
      maxVersion = pmMaxArgusVersion meta
  in case (minVersion, maxVersion) of
       (Just minV, _) | argusVersion < minV ->
         Just $ PluginLoadErr $ PluginVersionMismatch argusVersion minV
       (_, Just maxV) | argusVersion > maxV ->
         Just $ PluginLoadErr $ PluginVersionMismatch argusVersion maxV
       _ -> Nothing

-- | Check dependencies
checkDependencies :: PluginRegistry -> ArgusPlugin -> IO (Either PluginError ())
checkDependencies registry plugin = do
  let deps = pluginDependencies plugin
  st <- readTVarIO (prState registry)
  let loaded = Map.keysSet (rsPlugins st)
      missing = filter (`Set.notMember` loaded) deps
  if null missing || not (rcStrictDependencies (prConfig registry))
    then pure $ Right ()
    else pure $ Left $ PluginLoadErr $
           PluginDependencyError (pmId $ pluginMetadata plugin) (T.intercalate ", " missing)

-- | Convert rule match to diagnostic
ruleMatchToDiagnostic :: PluginRule -> RuleMatch -> Diagnostic
ruleMatchToDiagnostic rule RuleMatch{..} = Diagnostic
  { diagSpan = rmSpan
  , diagMessage = rmMessage
  , diagSeverity = matchKindToSeverity rmKind
  , diagKind = Custom $ "plugin:" <> prCategory rule
  , diagCode = Just $ prId rule
  , diagFixes = []
  , diagRelated = []
  }

-- | Convert match kind to severity
matchKindToSeverity :: RuleMatchKind -> Severity
matchKindToSeverity MatchError = Error
matchKindToSeverity MatchWarning = Warning
matchKindToSeverity MatchSuggestion = Suggestion
matchKindToSeverity MatchInfo = Info

-- | Check if fixer matches a rule
fixerMatchesRule :: Maybe Text -> PluginFixer -> Bool
fixerMatchesRule Nothing _ = False
fixerMatchesRule (Just ruleId) fixer =
  any (`T.isInfixOf` ruleId) (pfTargetRules fixer) ||
  null (pfTargetRules fixer)

-- | Priority ordering for rules
rulePriorityOrd :: PluginRule -> (Down PluginPriority, Text)
rulePriorityOrd r = (Down (prPriority r), prId r)

-- | Priority ordering for fixers
fixerPriorityOrd :: PluginFixer -> (Down PluginPriority, Text)
fixerPriorityOrd f = (Down (pfPriority f), pfId f)

-- | Priority ordering for hooks
hookPriorityOrd :: PluginHook -> (Down PluginPriority, Text)
hookPriorityOrd h = (Down (phPriority h), phId h)

-- | Get current time in microseconds (simple implementation)
getCurrentTimeMicros :: IO Integer
getCurrentTimeMicros = pure 0  -- Simplified; in production use Data.Time
