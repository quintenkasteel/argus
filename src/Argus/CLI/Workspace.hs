{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Workspace
-- Description : Workspace command implementation
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module implements the workspace command for multi-project analysis.
--
-- Usage:
--
-- @
-- # Analyze all projects in workspace
-- argus workspace analyze
--
-- # Initialize workspace configuration
-- argus workspace init
--
-- # List projects in workspace
-- argus workspace list
--
-- # Analyze specific projects
-- argus workspace analyze --projects core,web
--
-- # Analyze projects with specific tags
-- argus workspace analyze --tags backend
-- @
module Argus.CLI.Workspace
  ( -- * Entry point
    runWorkspace
  ) where

import Control.Monad (when, forM_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (getCurrentDirectory, doesDirectoryExist)
import System.FilePath ((</>))

import Argus.CLI.Types (GlobalOptions(..), WorkspaceOptions(..), WorkspaceAction(..))
import Argus.Workspace
  ( WorkspaceConfig(..)
  , ProjectConfig(..)
  , loadWorkspaceConfig
  , findWorkspaceConfig
  , discoverProjects
  , analyzeWorkspace
  , workspaceToJson
  , workspaceToTerminal
  )

--------------------------------------------------------------------------------
-- Entry Point
--------------------------------------------------------------------------------

-- | Run workspace command
runWorkspace :: GlobalOptions -> WorkspaceOptions -> IO ()
runWorkspace _global opts = case woAction opts of
  WSAnalyze  -> runAnalyze opts
  WSInit     -> runInit opts
  WSList     -> runList opts
  WSDiscover -> runDiscover opts
  WSValidate -> runValidate opts

--------------------------------------------------------------------------------
-- Command Implementations
--------------------------------------------------------------------------------

-- | Run workspace analysis
runAnalyze :: WorkspaceOptions -> IO ()
runAnalyze opts = do
  -- Find or load workspace config
  configPath <- case woConfigFile opts of
    Just path -> pure (Just path)
    Nothing   -> findWorkspaceConfig

  case configPath of
    Nothing -> do
      TIO.putStrLn $ colorRed "Error: " <> "No workspace configuration found."
      TIO.putStrLn "Run 'argus workspace init' to create one, or specify with --config"
      TIO.putStrLn ""
      TIO.putStrLn "Looking for: argus-workspace.toml or .argus-workspace.toml"

    Just path -> do
      when (woVerbose opts) $
        TIO.putStrLn $ "Loading workspace config from: " <> T.pack path

      ws <- loadWorkspaceConfig path

      -- Filter projects by name or tag
      let filteredWs = filterProjects opts ws

      TIO.putStrLn $ colorBold "Workspace: " <> wsName filteredWs
      TIO.putStrLn $ "Projects: " <> T.pack (show $ length $ wsProjects filteredWs)
      TIO.putStrLn ""

      -- Run analysis
      report <- analyzeWorkspace filteredWs

      -- Output results
      case woOutputFormat opts of
        "json" -> TIO.putStrLn $ workspaceToJson report
        _      -> TIO.putStrLn $ workspaceToTerminal report

-- | Initialize workspace configuration
runInit :: WorkspaceOptions -> IO ()
runInit opts = do
  cwd <- getCurrentDirectory
  let configPath = fromMaybe (cwd </> "argus-workspace.toml") (woConfigFile opts)

  TIO.putStrLn $ colorBold "Initializing workspace configuration..."
  TIO.putStrLn ""

  -- Discover projects
  projects <- discoverProjects cwd

  if null projects
    then do
      TIO.putStrLn "No Haskell projects found in current directory."
      TIO.putStrLn "Creating empty workspace configuration."
    else do
      TIO.putStrLn $ "Found " <> T.pack (show $ length projects) <> " project(s):"
      forM_ projects $ \proj ->
        TIO.putStrLn $ "  - " <> projName proj <> " (" <> T.pack (projPath proj) <> ")"
      TIO.putStrLn ""

  -- Generate config file content
  let configContent = generateWorkspaceConfig projects

  TIO.writeFile configPath configContent
  TIO.putStrLn $ colorGreen "Created: " <> T.pack configPath
  TIO.putStrLn ""
  TIO.putStrLn "Edit this file to customize your workspace settings."

-- | List projects in workspace
runList :: WorkspaceOptions -> IO ()
runList opts = do
  configPath <- case woConfigFile opts of
    Just path -> pure (Just path)
    Nothing   -> findWorkspaceConfig

  case configPath of
    Nothing -> TIO.putStrLn "No workspace configuration found."
    Just path -> do
      ws <- loadWorkspaceConfig path

      TIO.putStrLn $ colorBold "Workspace: " <> wsName ws
      TIO.putStrLn $ "Root: " <> T.pack (wsRoot ws)
      TIO.putStrLn ""

      TIO.putStrLn $ colorBold "Projects:"
      TIO.putStrLn ""

      forM_ (wsProjects ws) $ \proj -> do
        let status = if projEnabled proj then colorGreen "[enabled]" else colorYellow "[disabled]"
        let deps = if null (projDependsOn proj)
                     then ""
                     else " -> " <> T.intercalate ", " (projDependsOn proj)
        TIO.putStrLn $ "  " <> status <> " " <> projName proj
        TIO.putStrLn $ "      Path: " <> T.pack (projPath proj) <> deps
        when (not $ null $ projTags proj) $
          TIO.putStrLn $ "      Tags: " <> T.intercalate ", " (projTags proj)
        TIO.putStrLn ""

-- | Auto-discover projects
runDiscover :: WorkspaceOptions -> IO ()
runDiscover _opts = do
  cwd <- getCurrentDirectory

  TIO.putStrLn $ colorBold "Discovering Haskell projects..."
  TIO.putStrLn ""

  projects <- discoverProjects cwd

  if null projects
    then TIO.putStrLn "No Haskell projects found."
    else do
      TIO.putStrLn $ "Found " <> T.pack (show $ length projects) <> " project(s):"
      TIO.putStrLn ""
      forM_ projects $ \proj -> do
        let fullPath = cwd </> projPath proj
        hasStack <- doesDirectoryExist (fullPath </> ".stack-work")
        hasCabal <- doesDirectoryExist (fullPath </> "dist-newstyle")
        let buildSystem = case (hasStack, hasCabal) of
              (True, _)  -> "[stack]"
              (_, True)  -> "[cabal]"
              _          -> "[unknown]"
        TIO.putStrLn $ "  " <> projName proj <> " " <> buildSystem
        TIO.putStrLn $ "      " <> T.pack (projPath proj)
        TIO.putStrLn ""

-- | Validate workspace configuration
runValidate :: WorkspaceOptions -> IO ()
runValidate opts = do
  configPath <- case woConfigFile opts of
    Just path -> pure (Just path)
    Nothing   -> findWorkspaceConfig

  case configPath of
    Nothing -> do
      TIO.putStrLn $ colorRed "Error: " <> "No workspace configuration found."

    Just path -> do
      TIO.putStrLn $ colorBold "Validating workspace configuration..."
      TIO.putStrLn $ "Config: " <> T.pack path
      TIO.putStrLn ""

      ws <- loadWorkspaceConfig path
      let issues = validateWorkspace ws

      if null issues
        then do
          TIO.putStrLn $ colorGreen "Validation passed!"
          TIO.putStrLn $ "  " <> T.pack (show $ length $ wsProjects ws) <> " project(s) configured"
        else do
          TIO.putStrLn $ colorRed "Validation failed with " <> T.pack (show $ length issues) <> " issue(s):"
          forM_ issues $ \issue ->
            TIO.putStrLn $ "  - " <> issue

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Filter projects based on options
filterProjects :: WorkspaceOptions -> WorkspaceConfig -> WorkspaceConfig
filterProjects opts ws = ws { wsProjects = filtered }
  where
    projects = wsProjects ws

    -- Filter by project names
    byName = case woProjects opts of
      [] -> projects
      names -> filter (\p -> projName p `elem` names) projects

    -- Filter by tags
    filtered = case woTags opts of
      [] -> byName
      tags -> filter (\p -> any (`elem` projTags p) tags) byName

-- | Generate workspace configuration TOML
generateWorkspaceConfig :: [ProjectConfig] -> Text
generateWorkspaceConfig projects = T.unlines $
  [ "# Argus Workspace Configuration"
  , "# Generated automatically - customize as needed"
  , ""
  , "[workspace]"
  , "name = \"my-workspace\""
  , "root = \".\""
  , ""
  , "# Shared configuration (applied to all projects)"
  , "# shared-config = \"argus.toml\""
  , ""
  , "# Analysis options"
  , "inherit-rules = true"
  , "parallel-analysis = true"
  , "max-parallel = 4"
  , "aggregate-report = true"
  , "fail-fast = false"
  , ""
  ] ++ concatMap projectToToml projects

-- | Convert project to TOML section
projectToToml :: ProjectConfig -> [Text]
projectToToml proj =
  [ "[[projects]]"
  , "name = \"" <> projName proj <> "\""
  , "path = \"" <> T.pack (projPath proj) <> "\""
  , "enabled = " <> if projEnabled proj then "true" else "false"
  , "priority = " <> T.pack (show $ projPriority proj)
  , ""
  ]

-- | Validate workspace configuration
validateWorkspace :: WorkspaceConfig -> [Text]
validateWorkspace ws = concat
  [ validateRoot
  , validateProjects
  , validateDependencies
  ]
  where
    validateRoot
      | null (wsRoot ws) = ["Workspace root is empty"]
      | otherwise = []

    validateProjects = concat
      [ [ "Project '" <> projName p <> "' has empty name" | p <- wsProjects ws, T.null (projName p) ]
      , [ "Project '" <> projName p <> "' has empty path" | p <- wsProjects ws, null (projPath p) ]
      ]

    validateDependencies = concat
      [ [ "Project '" <> projName p <> "' depends on unknown project '" <> dep <> "'"
        | p <- wsProjects ws
        , dep <- projDependsOn p
        , dep `notElem` map projName (wsProjects ws)
        ]
      ]

-- | ANSI color helpers
colorBold :: Text -> Text
colorBold t = "\ESC[1m" <> t <> "\ESC[0m"

colorRed :: Text -> Text
colorRed t = "\ESC[31m" <> t <> "\ESC[0m"

colorGreen :: Text -> Text
colorGreen t = "\ESC[32m" <> t <> "\ESC[0m"

colorYellow :: Text -> Text
colorYellow t = "\ESC[33m" <> t <> "\ESC[0m"
