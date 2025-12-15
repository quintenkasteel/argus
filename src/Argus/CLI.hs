{-# LANGUAGE StrictData #-}
{-# LANGUAGE ApplicativeDo #-}

-- |
-- Module      : Argus.CLI
-- Description : Command-line interface for Argus
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides the command-line interface for Argus,
-- including argument parsing and subcommand handling.
module Argus.CLI
  ( -- * Main entry point
    main
  , runCLI

    -- * Options
  , Command (..)
  , GlobalOptions (..)
  , CheckOptions (..)
  , FixOptions (..)
  , UnusedOptions (..)
  , InitOptions (..)
  , IndexOptions (..)
  , WatchOptions (..)
  , DiffOptions (..)
  , BaselineOptions (..)
  , StatsOptions (..)
  , DaemonOptions (..)
  , DaemonAction (..)
  , LspOptions (..)
  , ArchitectureOptions (..)
  , PackOptions (..)
  , PackAction (..)
  , WorkspaceOptions (..)
  , WorkspaceAction (..)
  , RuleOptions (..)
  , RuleAction (..)

    -- * Parsers
  , parseCommand
  , parseGlobalOptions
  ) where

import Options.Applicative

-- Import types
import Argus.CLI.Types

-- Import parsers
import Argus.CLI.Common
  ( parseGlobalOptions
  , parseCheckOptions
  , parseFixOptions
  , parseUnusedOptions
  , parseInitOptions
  , parseIndexOptions
  , parseWatchOptions
  , parseDiffOptions
  , parseBaselineOptions
  , parseStatsOptions
  , parseDaemonOptions
  , parseLspOptions
  , parseArchitectureOptions
  , parsePackOptions
  , parseWorkspaceOptions
  , parseRuleOptions
  )

-- Import command implementations
import Argus.CLI.Check (runCheck)
import Argus.CLI.Fix (runFix)
import Argus.CLI.Unused (runUnused)
import Argus.CLI.Init (runInit)
import Argus.CLI.Index (runIndex)
import Argus.CLI.Watch (runWatch)
import Argus.CLI.Diff (runDiff)
import Argus.CLI.Baseline (runBaseline)
import Argus.CLI.Stats (runStats)
import Argus.CLI.Daemon (runDaemon)
import Argus.CLI.LSP (runLsp)
import Argus.CLI.Architecture (runArchitecture)
import Argus.CLI.Pack (runPack)
import Argus.CLI.Workspace (runWorkspace)
import Argus.CLI.Rule (runRule)

-- Main entry point
main :: IO ()
main = runCLI

-- | Run the CLI
runCLI :: IO ()
runCLI = do
  cmd <- execParser parseCommandWithInfo
  case cmd of
    CmdCheck global opts    -> runCheck global opts
    CmdFix global opts      -> runFix global opts
    CmdUnused global opts   -> runUnused global opts
    CmdInit global opts     -> runInit global opts
    CmdIndex global opts    -> runIndex global opts
    CmdWatch global opts    -> runWatch global opts
    CmdDiff global opts     -> runDiff global opts
    CmdBaseline global opts -> runBaseline global opts
    CmdStats global opts    -> runStats global opts
    CmdDaemon global opts   -> runDaemon global opts
    CmdLsp global opts      -> runLsp global opts
    CmdArchitecture global opts -> runArchitecture global opts
    CmdPack global opts     -> runPack global opts
    CmdWorkspace global opts -> runWorkspace global opts
    CmdRule global opts      -> runRule global opts

-- | Parse command
parseCommand :: Parser Command
parseCommand = hsubparser
  ( command "check" (info (CmdCheck <$> parseGlobalOptions <*> parseCheckOptions)
      (progDesc "Check files for issues"))
  <> command "fix" (info (CmdFix <$> parseGlobalOptions <*> parseFixOptions)
      (progDesc "Fix issues in files"))
  <> command "unused" (info (CmdUnused <$> parseGlobalOptions <*> parseUnusedOptions)
      (progDesc "Detect unused code"))
  <> command "init" (info (CmdInit <$> parseGlobalOptions <*> parseInitOptions)
      (progDesc "Initialize configuration file"))
  <> command "index" (info (CmdIndex <$> parseGlobalOptions <*> parseIndexOptions)
      (progDesc "Build project and index HIE files for cross-GHC-version analysis"))
  <> command "watch" (info (CmdWatch <$> parseGlobalOptions <*> parseWatchOptions)
      (progDesc "Watch files for changes and continuously analyze"))
  <> command "diff" (info (CmdDiff <$> parseGlobalOptions <*> parseDiffOptions)
      (progDesc "Compare analysis results with a baseline or git ref"))
  <> command "baseline" (info (CmdBaseline <$> parseGlobalOptions <*> parseBaselineOptions)
      (progDesc "Create a baseline of current issues"))
  <> command "stats" (info (CmdStats <$> parseGlobalOptions <*> parseStatsOptions)
      (progDesc "Show statistics about code issues"))
  <> command "daemon" (info (CmdDaemon <$> parseGlobalOptions <*> parseDaemonOptions)
      (progDesc "Run as daemon for fast repeated analysis"))
  <> command "lsp" (info (CmdLsp <$> parseGlobalOptions <*> parseLspOptions)
      (progDesc "Run as Language Server Protocol (LSP) server"))
  <> command "architecture" (info (CmdArchitecture <$> parseGlobalOptions <*> parseArchitectureOptions)
      (progDesc "Analyze module architecture (layers, coupling, cycles)"))
  <> command "pack" (info (CmdPack <$> parseGlobalOptions <*> parsePackOptions)
      (progDesc "Manage rule packs (list, show, export, import)"))
  <> command "workspace" (info (CmdWorkspace <$> parseGlobalOptions <*> parseWorkspaceOptions)
      (progDesc "Analyze multi-project workspaces"))
  <> command "rule" (info (CmdRule <$> parseGlobalOptions <*> parseRuleOptions)
      (progDesc "Author and manage custom rules"))
  )

-- | Full parser with info
parseCommandWithInfo :: ParserInfo Command
parseCommandWithInfo = info (parseCommand <**> helper)
  ( fullDesc
  <> progDesc "The All-Seeing Haskell Static Analyzer"
  <> header "argus - Comprehensive Haskell static analysis and refactoring"
  )
