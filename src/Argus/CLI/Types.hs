{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Types
-- Description : CLI command types and options
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides types for CLI commands and their options.
module Argus.CLI.Types
  ( -- * Global options
    GlobalOptions (..)

    -- * Command types
  , Command (..)
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
  ) where

import Data.Text (Text)
import Argus.Refactor.Validation (ValidationLevel(..))
import Argus.Refactor.FixGraph (ConflictStrategy(..))

--------------------------------------------------------------------------------
-- Global Options
--------------------------------------------------------------------------------

-- | Global options that apply to all commands
data GlobalOptions = GlobalOptions
  { goConfigFile :: Maybe FilePath
  , goVerbose    :: Bool
  , goNoColor    :: Bool
  , goParallel   :: Int
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Command Options
--------------------------------------------------------------------------------

-- | Check command options
data CheckOptions = CheckOptions
  { coTargets         :: [FilePath]
  , coMode            :: Text  -- ^ "quick", "full", "plugin"
  , coHieDir          :: Maybe FilePath
  , coOutputFormat    :: Text  -- ^ "terminal", "json", "sarif"
  , coGroupBy         :: Text  -- ^ "file", "rule", "severity"
  , coShowContext     :: Bool
  , coContextLines    :: Int
    -- CI mode options
  , coBaselinePath    :: Maybe FilePath  -- ^ Baseline file for comparison
  , coFailOnNew       :: Bool            -- ^ Fail if new issues vs baseline
  , coFailOnSeverity  :: Maybe Text      -- ^ Fail if issues at this severity
  , coFailOnCount     :: Maybe Int       -- ^ Fail if issue count exceeds
  , coFailOnDelta     :: Maybe Int       -- ^ Fail if delta exceeds threshold
  , coUpdateBaseline  :: Bool            -- ^ Update baseline after analysis
  , coCIQuiet         :: Bool            -- ^ Quiet mode for CI
  }
  deriving stock (Eq, Show)

-- | Fix command options
data FixOptions = FixOptions
  { foTargets          :: [FilePath]
  , foInteractive      :: Bool
  , foPreview          :: Bool
  , foRule             :: Maybe Text
  , foSafeOnly         :: Bool
  , foBackup           :: Bool
    -- New safe refactoring options
  , foValidate         :: Bool              -- ^ Enable validation after each fix
  , foValidateLevel    :: ValidationLevel   -- ^ How strict to validate
  , foTransactional    :: Bool              -- ^ All-or-nothing semantics
  , foConflictStrategy :: ConflictStrategy  -- ^ How to handle conflicts
  , foDryRun           :: Bool              -- ^ Don't write files, just show what would change
  , foShowDiff         :: Bool              -- ^ Show colored diffs
  , foVerbose          :: Bool              -- ^ Verbose output
  }
  deriving stock (Eq, Show)

-- | Unused command options
data UnusedOptions = UnusedOptions
  { uoTargets      :: [FilePath]
  , uoMode         :: Text
  , uoRoots        :: [Text]
  , uoOutputFormat :: Text  -- ^ "terminal", "json", "sarif"
  , uoShowContext  :: Bool
  , uoContextLines :: Int
  }
  deriving stock (Eq, Show)

-- | Init command options
data InitOptions = InitOptions
  { ioForce :: Bool
  }
  deriving stock (Eq, Show)

-- | Index command options
data IndexOptions = IndexOptions
  { ixTargetDir   :: Maybe FilePath  -- ^ Project directory (default: current)
  , ixBuild       :: Bool            -- ^ Build the project first
  , ixClean       :: Bool            -- ^ Clean before building
  , ixDbPath      :: Maybe FilePath  -- ^ Custom database path
  , ixSymlink     :: Bool            -- ^ Create .hie symlink
  , ixYes         :: Bool            -- ^ Auto-accept prompts (install hiedb)
  }
  deriving stock (Eq, Show)

-- | Watch command options
data WatchOptions = WatchOptions
  { woTargets       :: [FilePath]    -- ^ Directories to watch
  , woDebounceMs    :: Int           -- ^ Debounce delay in milliseconds
  , woPollIntervalMs :: Int          -- ^ Polling interval
  , woClearScreen   :: Bool          -- ^ Clear screen on updates
  , woShowTimestamp :: Bool          -- ^ Show timestamps
  }
  deriving stock (Eq, Show)

-- | Diff command options (compare with baseline or git diff)
data DiffOptions = DiffOptions
  { dfTargets       :: [FilePath]    -- ^ Files to analyze
  , dfBaseline      :: Maybe FilePath -- ^ Baseline file to compare against
  , dfGitRef        :: Maybe Text     -- ^ Git ref to compare against
  , dfShowNew       :: Bool           -- ^ Show only new issues
  , dfShowFixed     :: Bool           -- ^ Show only fixed issues
  , dfOutputFormat  :: Text           -- ^ Output format
  }
  deriving stock (Eq, Show)

-- | Baseline command options
data BaselineOptions = BaselineOptions
  { blTargets       :: [FilePath]    -- ^ Files to analyze
  , blOutputFile    :: FilePath       -- ^ Where to write baseline
  , blForce         :: Bool           -- ^ Overwrite existing baseline
  }
  deriving stock (Eq, Show)

-- | Stats command options
data StatsOptions = StatsOptions
  { stTargets       :: [FilePath]    -- ^ Files to analyze
  , stByRule        :: Bool           -- ^ Group by rule
  , stByFile        :: Bool           -- ^ Group by file
  , stBySeverity    :: Bool           -- ^ Group by severity
  , stOutputFormat  :: Text           -- ^ Output format (terminal, json)
  }
  deriving stock (Eq, Show)

-- | Daemon command options
data DaemonOptions = DaemonOptions
  { daAction        :: DaemonAction    -- ^ Action to perform
  , daSocketPath    :: Maybe FilePath  -- ^ Custom socket path
  , daPort          :: Maybe Int       -- ^ TCP port (alternative to socket)
  , daIdleTimeout   :: Maybe Int       -- ^ Idle timeout in seconds
  , daVerbose       :: Bool            -- ^ Verbose logging
  }
  deriving stock (Eq, Show)

-- | Daemon actions
data DaemonAction
  = DaemonStart            -- ^ Start the daemon
  | DaemonStop             -- ^ Stop the daemon
  | DaemonStatus           -- ^ Check daemon status
  | DaemonReload           -- ^ Reload configuration
  | DaemonCheck [FilePath] -- ^ Analyze files via daemon
  deriving stock (Eq, Show)

-- | LSP server options
data LspOptions = LspOptions
  { loDebugLog       :: Maybe FilePath  -- ^ Debug log file
  , loAnalyzeOnChange :: Bool           -- ^ Analyze on every change (expensive)
  , loDebounceMs     :: Int             -- ^ Debounce time for change analysis
  , loProgressReporting :: Bool         -- ^ Enable progress reporting
  }
  deriving stock (Eq, Show)

-- | Architecture analysis options
data ArchitectureOptions = ArchitectureOptions
  { aoTargets       :: [FilePath]     -- ^ Files or directories to analyze
  , aoOutputFormat  :: Text           -- ^ Output format: terminal, json, dot
  , aoShowGraph     :: Bool           -- ^ Generate DOT graph output
  , aoShowMetrics   :: Bool           -- ^ Show coupling/instability metrics
  , aoShowViolations :: Bool          -- ^ Show layer violations
  , aoShowCycles    :: Bool           -- ^ Show circular dependencies
  , aoGraphOutput   :: Maybe FilePath -- ^ File to write DOT graph
  }
  deriving stock (Eq, Show)

-- | Pack management action
data PackAction
  = PackList                        -- ^ List all available packs
  | PackShow Text                   -- ^ Show details of a specific pack
  | PackValidate (Maybe FilePath)   -- ^ Validate a pack (from file or builtin)
  | PackExport Text FilePath        -- ^ Export a pack to file
  | PackImport FilePath             -- ^ Import a pack from file
  | PackCreate Text                 -- ^ Create a new custom pack
  deriving stock (Eq, Show)

-- | Pack management options
data PackOptions = PackOptions
  { poAction      :: PackAction       -- ^ The action to perform
  , poVersion     :: Maybe Text       -- ^ Version for export operations
  , poAuthor      :: Maybe Text       -- ^ Author for export operations
  , poOutputJson  :: Bool             -- ^ Output as JSON instead of text
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Main Command Type
--------------------------------------------------------------------------------

-- | Commands
data Command
  = CmdCheck GlobalOptions CheckOptions
  | CmdFix GlobalOptions FixOptions
  | CmdUnused GlobalOptions UnusedOptions
  | CmdInit GlobalOptions InitOptions
  | CmdIndex GlobalOptions IndexOptions
  | CmdWatch GlobalOptions WatchOptions
  | CmdDiff GlobalOptions DiffOptions
  | CmdBaseline GlobalOptions BaselineOptions
  | CmdStats GlobalOptions StatsOptions
  | CmdDaemon GlobalOptions DaemonOptions
  | CmdLsp GlobalOptions LspOptions
  | CmdArchitecture GlobalOptions ArchitectureOptions
  | CmdPack GlobalOptions PackOptions
  deriving stock (Eq, Show)
