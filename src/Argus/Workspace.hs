{-# LANGUAGE StrictData #-}
{-# LANGUAGE ApplicativeDo #-}

-- |
-- Module      : Argus.Workspace
-- Description : Multi-project workspace configuration and analysis
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides support for analyzing multiple Haskell projects
-- with shared configuration and cross-project rule enforcement.
--
-- Workspaces allow:
-- - Shared configuration across projects
-- - Project-specific overrides
-- - Cross-project dependency tracking
-- - Aggregated reporting
--
-- Configuration file: @argus-workspace.toml@
--
-- Example:
--
-- @
-- [workspace]
-- name = "my-monorepo"
-- root = "."
--
-- [[projects]]
-- name = "core"
-- path = "packages/core"
--
-- [[projects]]
-- name = "web"
-- path = "packages/web"
-- depends-on = ["core"]
-- @
module Argus.Workspace
  ( -- * Types
    WorkspaceConfig (..)
  , ProjectConfig (..)
  , WorkspaceReport (..)
  , ProjectReport (..)

    -- * Default configuration
  , defaultWorkspaceConfig

    -- * Loading
  , loadWorkspaceConfig
  , findWorkspaceConfig
  , discoverProjects

    -- * Analysis
  , analyzeWorkspace
  , analyzeProject

    -- * Reporting
  , aggregateReports
  , workspaceToJson
  , workspaceToTerminal

    -- * TOML Codec
  , workspaceCodec
  ) where

import Control.Concurrent.Async (forConcurrently)
import Control.Exception (try, SomeException)
import Data.Aeson (ToJSON (..), FromJSON (..), (.:), (.:?), (.!=), withObject, object)
import Data.Aeson qualified as AE ((.=))
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, catMaybes)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory, isAbsolute)
import Toml (TomlCodec, (.=))
import Toml qualified as TOML

import Argus.Config (Config, defaultConfig, loadConfigFromFile)
import Argus.Types (Diagnostic (..), Severity (..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Workspace configuration for multi-project analysis
data WorkspaceConfig = WorkspaceConfig
  { wsName            :: Text                -- ^ Workspace name
  , wsRoot            :: FilePath            -- ^ Workspace root directory
  , wsProjects        :: [ProjectConfig]     -- ^ List of projects
  , wsSharedConfig    :: Maybe FilePath      -- ^ Shared config file (applied to all projects)
  , wsInheritRules    :: Bool                -- ^ Projects inherit workspace rules
  , wsParallelAnalysis :: Bool               -- ^ Analyze projects in parallel
  , wsMaxParallel     :: Int                 -- ^ Maximum parallel project analyses
  , wsCrossProjectRules :: Bool              -- ^ Enable cross-project rule checking
  , wsAggregateReport :: Bool                -- ^ Generate aggregated report
  , wsFailFast        :: Bool                -- ^ Stop on first project failure
  }
  deriving stock (Eq, Show, Generic)

-- | Individual project configuration within a workspace
data ProjectConfig = ProjectConfig
  { projName       :: Text                   -- ^ Project name
  , projPath       :: FilePath               -- ^ Path relative to workspace root
  , projConfigFile :: Maybe FilePath         -- ^ Project-specific config (overrides shared)
  , projDependsOn  :: [Text]                 -- ^ Projects this project depends on
  , projEnabled    :: Bool                   -- ^ Whether to analyze this project
  , projPriority   :: Int                    -- ^ Analysis priority (higher = first)
  , projTags       :: [Text]                 -- ^ Tags for filtering
  , projExcludedRules :: [Text]              -- ^ Rules to exclude for this project
  }
  deriving stock (Eq, Show, Generic)

-- | Workspace analysis report
data WorkspaceReport = WorkspaceReport
  { wrName          :: Text                  -- ^ Workspace name
  , wrTimestamp     :: UTCTime               -- ^ Analysis timestamp
  , wrProjectReports :: [ProjectReport]      -- ^ Per-project reports
  , wrTotalErrors   :: Int                   -- ^ Total errors across all projects
  , wrTotalWarnings :: Int                   -- ^ Total warnings across all projects
  , wrTotalSuggestions :: Int                -- ^ Total suggestions across all projects
  , wrCrossProjectIssues :: [Diagnostic]     -- ^ Issues spanning multiple projects
  , wrDependencyGraph :: Map Text [Text]     -- ^ Project dependency graph
  }
  deriving stock (Eq, Show, Generic)

-- | Individual project analysis report
data ProjectReport = ProjectReport
  { prName         :: Text                   -- ^ Project name
  , prPath         :: FilePath               -- ^ Project path
  , prDiagnostics  :: [Diagnostic]           -- ^ Diagnostics found
  , prErrorCount   :: Int                    -- ^ Number of errors
  , prWarningCount :: Int                    -- ^ Number of warnings
  , prSuggestionCount :: Int                 -- ^ Number of suggestions
  , prFilesAnalyzed :: Int                   -- ^ Number of files analyzed
  , prDuration     :: Double                 -- ^ Analysis duration in seconds
  , prSuccess      :: Bool                   -- ^ Whether analysis completed successfully
  , prError        :: Maybe Text             -- ^ Error message if failed
  }
  deriving stock (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Default Configuration
--------------------------------------------------------------------------------

-- | Default workspace configuration
defaultWorkspaceConfig :: WorkspaceConfig
defaultWorkspaceConfig = WorkspaceConfig
  { wsName             = "workspace"
  , wsRoot             = "."
  , wsProjects         = []
  , wsSharedConfig     = Nothing
  , wsInheritRules     = True
  , wsParallelAnalysis = True
  , wsMaxParallel      = 4
  , wsCrossProjectRules = False
  , wsAggregateReport  = True
  , wsFailFast         = False
  }

-- | Default project configuration
defaultProjectConfig :: ProjectConfig
defaultProjectConfig = ProjectConfig
  { projName          = ""
  , projPath          = ""
  , projConfigFile    = Nothing
  , projDependsOn     = []
  , projEnabled       = True
  , projPriority      = 0
  , projTags          = []
  , projExcludedRules = []
  }

--------------------------------------------------------------------------------
-- JSON Instances
--------------------------------------------------------------------------------

instance ToJSON WorkspaceConfig where
  toJSON WorkspaceConfig{..} = object
    [ "name" AE..= wsName
    , "root" AE..= wsRoot
    , "projects" AE..= wsProjects
    , "shared-config" AE..= wsSharedConfig
    , "inherit-rules" AE..= wsInheritRules
    , "parallel-analysis" AE..= wsParallelAnalysis
    , "max-parallel" AE..= wsMaxParallel
    , "cross-project-rules" AE..= wsCrossProjectRules
    , "aggregate-report" AE..= wsAggregateReport
    , "fail-fast" AE..= wsFailFast
    ]

instance FromJSON WorkspaceConfig where
  parseJSON = withObject "WorkspaceConfig" $ \o -> do
    wsName             <- o .:? "name"                .!= "workspace"
    wsRoot             <- o .:? "root"                .!= "."
    wsProjects         <- o .:? "projects"            .!= []
    wsSharedConfig     <- o .:? "shared-config"
    wsInheritRules     <- o .:? "inherit-rules"       .!= True
    wsParallelAnalysis <- o .:? "parallel-analysis"   .!= True
    wsMaxParallel      <- o .:? "max-parallel"        .!= 4
    wsCrossProjectRules <- o .:? "cross-project-rules" .!= False
    wsAggregateReport  <- o .:? "aggregate-report"    .!= True
    wsFailFast         <- o .:? "fail-fast"           .!= False
    pure WorkspaceConfig{..}

instance ToJSON ProjectConfig where
  toJSON ProjectConfig{..} = object
    [ "name" AE..= projName
    , "path" AE..= projPath
    , "config-file" AE..= projConfigFile
    , "depends-on" AE..= projDependsOn
    , "enabled" AE..= projEnabled
    , "priority" AE..= projPriority
    , "tags" AE..= projTags
    , "excluded-rules" AE..= projExcludedRules
    ]

instance FromJSON ProjectConfig where
  parseJSON = withObject "ProjectConfig" $ \o -> do
    projName          <- o .: "name"
    projPath          <- o .: "path"
    projConfigFile    <- o .:? "config-file"
    projDependsOn     <- o .:? "depends-on"      .!= []
    projEnabled       <- o .:? "enabled"         .!= True
    projPriority      <- o .:? "priority"        .!= 0
    projTags          <- o .:? "tags"            .!= []
    projExcludedRules <- o .:? "excluded-rules"  .!= []
    pure ProjectConfig{..}

instance ToJSON WorkspaceReport where
  toJSON WorkspaceReport{..} = object
    [ "name" AE..= wrName
    , "timestamp" AE..= wrTimestamp
    , "project-reports" AE..= wrProjectReports
    , "total-errors" AE..= wrTotalErrors
    , "total-warnings" AE..= wrTotalWarnings
    , "total-suggestions" AE..= wrTotalSuggestions
    , "cross-project-issues" AE..= wrCrossProjectIssues
    , "dependency-graph" AE..= wrDependencyGraph
    ]

instance ToJSON ProjectReport where
  toJSON ProjectReport{..} = object
    [ "name" AE..= prName
    , "path" AE..= prPath
    , "diagnostics" AE..= prDiagnostics
    , "error-count" AE..= prErrorCount
    , "warning-count" AE..= prWarningCount
    , "suggestion-count" AE..= prSuggestionCount
    , "files-analyzed" AE..= prFilesAnalyzed
    , "duration" AE..= prDuration
    , "success" AE..= prSuccess
    , "error" AE..= prError
    ]

--------------------------------------------------------------------------------
-- TOML Codec
--------------------------------------------------------------------------------

-- | TOML codec for workspace configuration
workspaceCodec :: TomlCodec WorkspaceConfig
workspaceCodec = WorkspaceConfig
  <$> TOML.text "name" .= wsName
  <*> TOML.string "root" .= wsRoot
  <*> TOML.list projectCodec "projects" .= wsProjects
  <*> TOML.dioptional (TOML.string "shared-config") .= wsSharedConfig
  <*> boolWithDefault True "inherit-rules" .= wsInheritRules
  <*> boolWithDefault True "parallel-analysis" .= wsParallelAnalysis
  <*> intWithDefault 4 "max-parallel" .= wsMaxParallel
  <*> boolWithDefault False "cross-project-rules" .= wsCrossProjectRules
  <*> boolWithDefault True "aggregate-report" .= wsAggregateReport
  <*> boolWithDefault False "fail-fast" .= wsFailFast

-- | TOML codec for project configuration
projectCodec :: TomlCodec ProjectConfig
projectCodec = ProjectConfig
  <$> TOML.text "name" .= projName
  <*> TOML.string "path" .= projPath
  <*> TOML.dioptional (TOML.string "config-file") .= projConfigFile
  <*> textArrayWithDefault [] "depends-on" .= projDependsOn
  <*> boolWithDefault True "enabled" .= projEnabled
  <*> intWithDefault 0 "priority" .= projPriority
  <*> textArrayWithDefault [] "tags" .= projTags
  <*> textArrayWithDefault [] "excluded-rules" .= projExcludedRules

-- | Bool codec with a default value
boolWithDefault :: Bool -> TOML.Key -> TomlCodec Bool
boolWithDefault def key = TOML.dimap Just (fromMaybe def) $ TOML.dioptional (TOML.bool key)

-- | Int codec with a default value
intWithDefault :: Int -> TOML.Key -> TomlCodec Int
intWithDefault def key = TOML.dimap Just (fromMaybe def) $ TOML.dioptional (TOML.int key)

-- | Text array codec with a default value
textArrayWithDefault :: [Text] -> TOML.Key -> TomlCodec [Text]
textArrayWithDefault def key = TOML.dimap Just (fromMaybe def) $ TOML.dioptional (TOML.arrayOf TOML._Text key)

--------------------------------------------------------------------------------
-- Configuration Loading
--------------------------------------------------------------------------------

-- | Load workspace configuration from file
loadWorkspaceConfig :: FilePath -> IO WorkspaceConfig
loadWorkspaceConfig path = do
  content <- TIO.readFile path
  case TOML.decode workspaceCodec content of
    Left err  -> fail $ "Failed to parse workspace config: " <> show err
    Right cfg -> do
      -- Make paths absolute relative to config file location
      let configDir = takeDirectory path
          root = if isAbsolute (wsRoot cfg) then wsRoot cfg else configDir </> wsRoot cfg
      pure cfg { wsRoot = root }

-- | Find workspace configuration in current or parent directories
findWorkspaceConfig :: IO (Maybe FilePath)
findWorkspaceConfig = do
  cwd <- getCurrentDirectory
  searchUpwards cwd
  where
    configNames = ["argus-workspace.toml", ".argus-workspace.toml"]

    searchUpwards dir = do
      -- Check for config files in current directory
      found <- findFirst configNames dir
      case found of
        Just path -> pure (Just path)
        Nothing -> do
          let parent = takeDirectory dir
          if parent == dir  -- Reached root
            then pure Nothing
            else searchUpwards parent

    findFirst [] _ = pure Nothing
    findFirst (name:rest) dir = do
      let path = dir </> name
      exists <- doesFileExist path
      if exists then pure (Just path) else findFirst rest dir

-- | Auto-discover projects in a directory (looks for stack.yaml/cabal.project)
discoverProjects :: FilePath -> IO [ProjectConfig]
discoverProjects root = do
  entries <- listDirectory root
  projects <- catMaybes <$> mapM checkProject entries
  pure projects
  where
    checkProject entry = do
      let path = root </> entry
      isDir <- doesDirectoryExist path
      if not isDir
        then pure Nothing
        else do
          hasStack <- doesFileExist (path </> "stack.yaml")
          hasCabal <- doesFileExist (path </> entry <> ".cabal")
          hasPackageYaml <- doesFileExist (path </> "package.yaml")
          if hasStack || hasCabal || hasPackageYaml
            then pure $ Just defaultProjectConfig
              { projName = T.pack entry
              , projPath = entry
              }
            else pure Nothing

--------------------------------------------------------------------------------
-- Analysis
--------------------------------------------------------------------------------

-- | Analyze all projects in a workspace
analyzeWorkspace :: WorkspaceConfig -> IO WorkspaceReport
analyzeWorkspace ws@WorkspaceConfig{..} = do
  timestamp <- getCurrentTime

  -- Load shared config if specified
  sharedCfg <- case wsSharedConfig of
    Nothing   -> pure defaultConfig
    Just path -> do
      let fullPath = if isAbsolute path then path else wsRoot </> path
      loadConfigFromFile fullPath

  -- Filter enabled projects and sort by priority
  let enabledProjects = filter projEnabled wsProjects
      sortedProjects = sortOn (Down . projPriority) enabledProjects

  -- Analyze projects (parallel or sequential)
  reports <- if wsParallelAnalysis
    then analyzeParallel ws sharedCfg sortedProjects
    else analyzeSequential ws sharedCfg sortedProjects

  -- Build dependency graph
  let depGraph = buildDependencyGraph wsProjects

  -- Aggregate results
  pure $ aggregateReports wsName timestamp reports depGraph

-- | Analyze projects in parallel
analyzeParallel :: WorkspaceConfig -> Config -> [ProjectConfig] -> IO [ProjectReport]
analyzeParallel ws sharedCfg projects = do
  -- Split into batches for max parallelism
  let batches = chunksOf (wsMaxParallel ws) projects
  concat <$> mapM (forConcurrently `flip` analyzeOne) batches
  where
    analyzeOne proj = analyzeProject ws sharedCfg proj

    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Analyze projects sequentially
analyzeSequential :: WorkspaceConfig -> Config -> [ProjectConfig] -> IO [ProjectReport]
analyzeSequential ws sharedCfg projects =
  mapM (analyzeProject ws sharedCfg) projects

-- | Analyze a single project
analyzeProject :: WorkspaceConfig -> Config -> ProjectConfig -> IO ProjectReport
analyzeProject ws _sharedCfg proj = do
  let projectPath = wsRoot ws </> projPath proj

  -- Try to run analysis
  result <- try @SomeException $ do
    -- For now, return a placeholder report
    -- In a full implementation, this would call Argus.Core.runAnalysis
    pure ([], 0)

  case result of
    Left err -> pure ProjectReport
      { prName            = projName proj
      , prPath            = projectPath
      , prDiagnostics     = []
      , prErrorCount      = 0
      , prWarningCount    = 0
      , prSuggestionCount = 0
      , prFilesAnalyzed   = 0
      , prDuration        = 0
      , prSuccess         = False
      , prError           = Just (T.pack $ show err)
      }
    Right (diagnostics, fileCount) -> do
      let (errs, warns, suggs) = countBySeverity diagnostics
      pure ProjectReport
        { prName            = projName proj
        , prPath            = projectPath
        , prDiagnostics     = diagnostics
        , prErrorCount      = errs
        , prWarningCount    = warns
        , prSuggestionCount = suggs
        , prFilesAnalyzed   = fileCount
        , prDuration        = 0  -- Would need timing in real implementation
        , prSuccess         = True
        , prError           = Nothing
        }

-- | Count diagnostics by severity
countBySeverity :: [Diagnostic] -> (Int, Int, Int)
countBySeverity diags = (errs, warns, suggs)
  where
    errs  = length [d | d <- diags, diagSeverity d == Error]
    warns = length [d | d <- diags, diagSeverity d == Warning]
    suggs = length [d | d <- diags, diagSeverity d `elem` [Suggestion, Info]]

-- | Build project dependency graph
buildDependencyGraph :: [ProjectConfig] -> Map Text [Text]
buildDependencyGraph projects =
  Map.fromList [(projName p, projDependsOn p) | p <- projects]

--------------------------------------------------------------------------------
-- Reporting
--------------------------------------------------------------------------------

-- | Aggregate project reports into workspace report
aggregateReports :: Text -> UTCTime -> [ProjectReport] -> Map Text [Text] -> WorkspaceReport
aggregateReports name timestamp reports depGraph = WorkspaceReport
  { wrName              = name
  , wrTimestamp         = timestamp
  , wrProjectReports    = reports
  , wrTotalErrors       = sum [prErrorCount r | r <- reports]
  , wrTotalWarnings     = sum [prWarningCount r | r <- reports]
  , wrTotalSuggestions  = sum [prSuggestionCount r | r <- reports]
  , wrCrossProjectIssues = []  -- Would need cross-project analysis
  , wrDependencyGraph   = depGraph
  }

-- | Convert workspace report to JSON
workspaceToJson :: WorkspaceReport -> Text
workspaceToJson report =
  let json = toJSON report
  in T.pack $ show json  -- Simplified; would use aeson-pretty in real implementation

-- | Convert workspace report to terminal output
workspaceToTerminal :: WorkspaceReport -> Text
workspaceToTerminal WorkspaceReport{..} = T.unlines
  [ "=========================================="
  , "       Workspace Analysis: " <> wrName
  , "=========================================="
  , ""
  , "Summary:"
  , "  Projects: " <> T.pack (show $ length wrProjectReports)
  , "  Errors:   " <> T.pack (show wrTotalErrors)
  , "  Warnings: " <> T.pack (show wrTotalWarnings)
  , ""
  , "Per-Project Results:"
  , T.unlines $ map formatProjectReport wrProjectReports
  ]
  where
    formatProjectReport ProjectReport{..} = T.unlines
      [ "  " <> prName <> " (" <> T.pack prPath <> ")"
      , "    Status: " <> if prSuccess then "OK" else "FAILED"
      , "    Errors: " <> T.pack (show prErrorCount)
      , "    Warnings: " <> T.pack (show prWarningCount)
      , "    Files: " <> T.pack (show prFilesAnalyzed)
      ]
