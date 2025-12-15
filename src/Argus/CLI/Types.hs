{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Types
-- Description : CLI command types and options
-- Copyright   : (c) 2024
-- License     : MIT
--
-- = Overview
--
-- This module defines all command-line interface types for Argus, including
-- global options, command-specific options, and action types. These types
-- are parsed from command-line arguments and used to configure the various
-- Argus commands.
--
-- = Architecture
--
-- @
-- ┌─────────────────────────────────────────────────────────────────────┐
-- │                           Command                                    │
-- │  ┌───────────────┐    ┌─────────────────────────────────────────┐   │
-- │  │ GlobalOptions │────│ CommandOptions (Check|Fix|Index|...)   │   │
-- │  │               │    │                                         │   │
-- │  │ • config      │    │ • targets                               │   │
-- │  │ • verbose     │    │ • mode                                  │   │
-- │  │ • parallel    │    │ • format                                │   │
-- │  │ • noColor     │    │ • command-specific flags                │   │
-- │  └───────────────┘    └─────────────────────────────────────────┘   │
-- └─────────────────────────────────────────────────────────────────────┘
-- @
--
-- = Command Structure
--
-- Each command is represented as a constructor of the 'Command' type,
-- carrying both 'GlobalOptions' and command-specific options:
--
-- * 'CmdCheck' - Static analysis with configurable output formats
-- * 'CmdFix' - Auto-fix issues with validation and conflict resolution
-- * 'CmdIndex' - Build HIE database for semantic analysis
-- * 'CmdWatch' - Watch mode for continuous analysis
-- * 'CmdDaemon' - Background service mode
-- * 'CmdLsp' - Language Server Protocol server
-- * 'CmdArchitecture' - Dependency and architecture analysis
--
-- = Usage
--
-- @
-- -- Parse command line and dispatch
-- cmd <- parseCommand
-- case cmd of
--   CmdCheck global opts -> runCheck global opts
--   CmdFix global opts -> runFix global opts
--   ...
-- @
--
-- @since 1.0.0
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
  , WorkspaceOptions (..)
  , WorkspaceAction (..)
  , RuleOptions (..)
  , RuleAction (..)
  ) where

import Data.Text (Text)
import Argus.Refactor.Validation (ValidationLevel(..))
import Argus.Refactor.FixGraph (ConflictStrategy(..))

--------------------------------------------------------------------------------
-- Global Options
--------------------------------------------------------------------------------

-- | Global options that apply to all commands.
--
-- These options are parsed before the subcommand and affect the behavior
-- of all commands. They control cross-cutting concerns like verbosity,
-- parallelism, and output formatting.
--
-- = Fields
--
-- [@goConfigFile@]: Path to argus.toml configuration file. If 'Nothing',
--   Argus searches for configuration in standard locations.
--
-- [@goVerbose@]: Enable verbose output including debug information,
--   timing statistics, and detailed progress.
--
-- [@goNoColor@]: Disable ANSI color codes in output. Useful for
--   CI environments or piping to files.
--
-- [@goParallel@]: Number of parallel jobs for file analysis.
--   Higher values improve throughput on multi-core systems.
--
-- @since 1.0.0
data GlobalOptions = GlobalOptions
  { goConfigFile :: Maybe FilePath
    -- ^ Path to configuration file (argus.toml)
  , goVerbose    :: Bool
    -- ^ Enable verbose output
  , goNoColor    :: Bool
    -- ^ Disable colored output
  , goParallel   :: Int
    -- ^ Number of parallel analysis jobs
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Command Options
--------------------------------------------------------------------------------

-- | Check command options for static analysis.
--
-- Controls the behavior of the @argus check@ command, which performs
-- static analysis on Haskell source files and reports diagnostics.
--
-- = Analysis Modes
--
-- * @\"quick\"@ - Syntactic analysis only, fast but less thorough
-- * @\"full\"@ - Includes HIE-based semantic analysis
-- * @\"plugin\"@ - Uses GHC plugin for Template Haskell support
--
-- = Output Formats
--
-- * @\"terminal\"@ - Human-readable colored output
-- * @\"json\"@ - Machine-readable JSON format
-- * @\"sarif\"@ - SARIF format for GitHub integration
-- * @\"junit\"@ - JUnit XML for CI systems
-- * @\"codeclimate\"@ - Code Climate format
--
-- = CI Mode
--
-- When 'coBaselinePath' is set, the command operates in CI mode,
-- comparing results against a baseline and failing based on
-- configured thresholds.
--
-- @since 1.0.0
data CheckOptions = CheckOptions
  { coTargets         :: [FilePath]
    -- ^ Files or directories to analyze
  , coMode            :: Text
    -- ^ Analysis mode: \"quick\", \"full\", \"plugin\"
  , coHieDir          :: Maybe FilePath
    -- ^ Custom HIE directory for full mode
  , coOutputFormat    :: Text
    -- ^ Output format: \"terminal\", \"json\", \"sarif\", etc.
  , coGroupBy         :: Text
    -- ^ Grouping: \"file\", \"rule\", \"severity\"
  , coShowContext     :: Bool
    -- ^ Show source code context around diagnostics
  , coContextLines    :: Int
    -- ^ Number of context lines to display
    -- CI mode options
  , coBaselinePath    :: Maybe FilePath
    -- ^ Baseline file for comparison (enables CI mode)
  , coFailOnNew       :: Bool
    -- ^ Exit non-zero if new issues found vs baseline
  , coFailOnSeverity  :: Maybe Text
    -- ^ Exit non-zero if issues at or above this severity
  , coFailOnCount     :: Maybe Int
    -- ^ Exit non-zero if issue count exceeds threshold
  , coFailOnDelta     :: Maybe Int
    -- ^ Exit non-zero if net new issues exceed threshold
  , coUpdateBaseline  :: Bool
    -- ^ Update baseline file after analysis
  , coCIQuiet         :: Bool
    -- ^ Minimal output for CI pipelines
  }
  deriving stock (Eq, Show)

-- | Fix command options for auto-fixing issues.
--
-- Controls the behavior of the @argus fix@ command, which automatically
-- applies fixes for detected issues with configurable safety levels.
--
-- = Fix Modes
--
-- * __Interactive__ ('foInteractive'): Prompt before each fix with diff preview
-- * __Preview/Dry-run__ ('foDryRun'): Show changes without applying them
-- * __Auto__ (default): Apply all safe fixes automatically
--
-- = Safety Features
--
-- * __Validation__ ('foValidateLevel'): Verify fixes don't break syntax
-- * __Transactional__ ('foTransactional'): Rollback all changes on any failure
-- * __Backup__ ('foBackup'): Create .bak files before modifying
-- * __Safe-only__ ('foSafeOnly'): Only apply fixes marked as safe
--
-- = Conflict Resolution
--
-- When multiple fixes overlap, 'foConflictStrategy' determines which to apply:
--
-- * 'SkipAll' - Skip all conflicting fixes
-- * 'PreferPreferred' - Apply the preferred (higher priority) fix
-- * 'PreferSeverity' - Apply fix for higher severity issue
-- * 'PreferSmaller' - Apply the smaller, more focused fix
--
-- @since 1.0.0
data FixOptions = FixOptions
  { foTargets          :: [FilePath]
    -- ^ Files or directories to fix
  , foInteractive      :: Bool
    -- ^ Prompt before each fix with diff preview
  , foPreview          :: Bool
    -- ^ Preview mode (deprecated, use --dry-run)
  , foRule             :: Maybe Text
    -- ^ Apply only fixes from this rule
  , foSafeOnly         :: Bool
    -- ^ Only apply fixes marked as safe
  , foBackup           :: Bool
    -- ^ Create backup files before modifying
    -- Safe refactoring options
  , foValidate         :: Bool
    -- ^ Enable syntax validation after each fix
  , foValidateLevel    :: ValidationLevel
    -- ^ Validation strictness level
  , foTransactional    :: Bool
    -- ^ Rollback all changes on any failure
  , foConflictStrategy :: ConflictStrategy
    -- ^ How to handle overlapping fixes
  , foDryRun           :: Bool
    -- ^ Show changes without writing files
  , foShowDiff         :: Bool
    -- ^ Display colored diff output
  , foVerbose          :: Bool
    -- ^ Verbose progress output
  }
  deriving stock (Eq, Show)

-- | Unused code detection options.
--
-- Controls the @argus unused@ command which finds dead code including
-- unused functions, types, and imports.
--
-- @since 1.0.0
data UnusedOptions = UnusedOptions
  { uoTargets      :: [FilePath]
    -- ^ Files or directories to analyze
  , uoMode         :: Text
    -- ^ Analysis mode: \"full\", \"plugin\"
  , uoRoots        :: [Text]
    -- ^ Additional root patterns (entry points)
  , uoOutputFormat :: Text
    -- ^ Output format: \"terminal\", \"json\", \"sarif\"
  , uoShowContext  :: Bool
    -- ^ Show source context around findings
  , uoContextLines :: Int
    -- ^ Number of context lines
  }
  deriving stock (Eq, Show)

-- | Init command options for project setup.
--
-- Creates a default argus.toml configuration file.
--
-- @since 1.0.0
data InitOptions = InitOptions
  { ioForce :: Bool
    -- ^ Overwrite existing configuration
  }
  deriving stock (Eq, Show)

-- | Index command options for HIE database building.
--
-- The @argus index@ command builds the project with HIE file generation
-- enabled and creates an hiedb database for semantic analysis.
--
-- = Workflow
--
-- 1. Detect project type (Stack or Cabal)
-- 2. Build with @-fwrite-ide-info@ flag
-- 3. Index HIE files with hiedb
-- 4. Optionally create @.hie@ symlink
--
-- @since 1.0.0
data IndexOptions = IndexOptions
  { ixTargetDir   :: Maybe FilePath
    -- ^ Project directory (default: current)
  , ixBuild       :: Bool
    -- ^ Build project with HIE generation
  , ixClean       :: Bool
    -- ^ Clean before building
  , ixDbPath      :: Maybe FilePath
    -- ^ Custom database path (default: .hiedb)
  , ixSymlink     :: Bool
    -- ^ Create .hie symlink in project root
  , ixYes         :: Bool
    -- ^ Auto-accept prompts (e.g., install hiedb)
  }
  deriving stock (Eq, Show)

-- | Watch mode options for continuous analysis.
--
-- Monitors file system changes and re-analyzes affected files.
-- Uses debouncing to avoid excessive analysis during rapid edits.
--
-- @since 1.0.0
data WatchOptions = WatchOptions
  { woTargets       :: [FilePath]
    -- ^ Directories to watch
  , woDebounceMs    :: Int
    -- ^ Debounce delay in milliseconds
  , woPollIntervalMs :: Int
    -- ^ File system polling interval
  , woClearScreen   :: Bool
    -- ^ Clear screen on each update
  , woShowTimestamp :: Bool
    -- ^ Show timestamps in output
  }
  deriving stock (Eq, Show)

-- | Diff command options for baseline comparison.
--
-- Compares current analysis results against a baseline or git ref
-- to show new, fixed, or unchanged issues.
--
-- @since 1.0.0
data DiffOptions = DiffOptions
  { dfTargets       :: [FilePath]
    -- ^ Files or directories to analyze
  , dfBaseline      :: Maybe FilePath
    -- ^ Baseline JSON file to compare against
  , dfGitRef        :: Maybe Text
    -- ^ Git ref to compare against (e.g., \"HEAD~1\", \"main\")
  , dfShowNew       :: Bool
    -- ^ Show only new issues
  , dfShowFixed     :: Bool
    -- ^ Show only fixed issues
  , dfOutputFormat  :: Text
    -- ^ Output format: \"terminal\", \"json\"
  }
  deriving stock (Eq, Show)

-- | Baseline command options for snapshot creation.
--
-- Creates a baseline snapshot of current issues for future comparison.
-- Useful for tracking progress on legacy codebases.
--
-- @since 1.0.0
data BaselineOptions = BaselineOptions
  { blTargets       :: [FilePath]
    -- ^ Files or directories to analyze
  , blOutputFile    :: FilePath
    -- ^ Output file for baseline (default: .argus-baseline.json)
  , blForce         :: Bool
    -- ^ Overwrite existing baseline file
  }
  deriving stock (Eq, Show)

-- | Statistics command options.
--
-- Generates aggregate statistics about detected issues grouped
-- by various dimensions.
--
-- @since 1.0.0
data StatsOptions = StatsOptions
  { stTargets       :: [FilePath]
    -- ^ Files or directories to analyze
  , stByRule        :: Bool
    -- ^ Group statistics by rule ID
  , stByFile        :: Bool
    -- ^ Group statistics by file path
  , stBySeverity    :: Bool
    -- ^ Group statistics by severity level
  , stOutputFormat  :: Text
    -- ^ Output format: \"terminal\", \"json\"
  }
  deriving stock (Eq, Show)

-- | Daemon mode options.
--
-- The Argus daemon runs as a background service, caching parsed files
-- and providing fast analysis responses over Unix socket or TCP.
--
-- = Communication
--
-- * Unix socket (default): Faster, local-only
-- * TCP port: Network accessible, useful for remote analysis
--
-- @since 1.0.0
data DaemonOptions = DaemonOptions
  { daAction        :: DaemonAction
    -- ^ Action to perform on the daemon
  , daSocketPath    :: Maybe FilePath
    -- ^ Custom Unix socket path
  , daPort          :: Maybe Int
    -- ^ TCP port (alternative to socket)
  , daIdleTimeout   :: Maybe Int
    -- ^ Auto-shutdown after idle seconds
  , daVerbose       :: Bool
    -- ^ Enable verbose logging
  }
  deriving stock (Eq, Show)

-- | Daemon control actions.
--
-- @since 1.0.0
data DaemonAction
  = DaemonStart
    -- ^ Start the daemon process
  | DaemonStop
    -- ^ Stop a running daemon
  | DaemonStatus
    -- ^ Query daemon status and statistics
  | DaemonReload
    -- ^ Reload configuration without restart
  | DaemonCheck [FilePath]
    -- ^ Analyze files via running daemon
  deriving stock (Eq, Show)

-- | LSP server options.
--
-- Runs Argus as a Language Server Protocol server for IDE integration.
-- Supports real-time diagnostics, code actions, and hover information.
--
-- = Performance Considerations
--
-- The 'loAnalyzeOnChange' option triggers analysis on every keystroke,
-- which can be expensive. For large projects, rely on debouncing or
-- save-triggered analysis instead.
--
-- @since 1.0.0
data LspOptions = LspOptions
  { loDebugLog       :: Maybe FilePath
    -- ^ Write debug information to file
  , loAnalyzeOnChange :: Bool
    -- ^ Analyze on every change (expensive)
  , loDebounceMs     :: Int
    -- ^ Debounce delay for change analysis
  , loProgressReporting :: Bool
    -- ^ Send progress notifications to client
  }
  deriving stock (Eq, Show)

-- | Architecture analysis options.
--
-- Analyzes module dependencies, detects circular imports, computes
-- coupling metrics, and validates architectural layer constraints.
--
-- = Output Formats
--
-- * @\"terminal\"@ - Text summary with metrics
-- * @\"json\"@ - Machine-readable JSON
-- * @\"dot\"@ - GraphViz DOT format for visualization
--
-- @since 1.0.0
data ArchitectureOptions = ArchitectureOptions
  { aoTargets       :: [FilePath]
    -- ^ Files or directories to analyze
  , aoOutputFormat  :: Text
    -- ^ Output format: \"terminal\", \"json\", \"dot\"
  , aoShowGraph     :: Bool
    -- ^ Generate DOT graph output
  , aoShowMetrics   :: Bool
    -- ^ Show coupling/instability metrics
  , aoShowViolations :: Bool
    -- ^ Show architectural layer violations
  , aoShowCycles    :: Bool
    -- ^ Show circular dependency chains
  , aoGraphOutput   :: Maybe FilePath
    -- ^ File to write DOT graph
  }
  deriving stock (Eq, Show)

-- | Rule pack management actions.
--
-- Rule packs are collections of related rules that can be enabled/disabled
-- as a group, exported for sharing, or imported from external sources.
--
-- @since 1.0.0
data PackAction
  = PackList
    -- ^ List all available packs (builtin and custom)
  | PackShow Text
    -- ^ Show details of a specific pack
  | PackValidate (Maybe FilePath)
    -- ^ Validate pack definition (file or builtin)
  | PackExport Text FilePath
    -- ^ Export pack to TOML file
  | PackImport FilePath
    -- ^ Import pack from TOML file
  | PackCreate Text
    -- ^ Create a new custom pack template
  deriving stock (Eq, Show)

-- | Pack management command options.
--
-- @since 1.0.0
data PackOptions = PackOptions
  { poAction      :: PackAction
    -- ^ Action to perform
  , poVersion     :: Maybe Text
    -- ^ Version string for exported packs
  , poAuthor      :: Maybe Text
    -- ^ Author name for exported packs
  , poOutputJson  :: Bool
    -- ^ Output as JSON instead of text
  }
  deriving stock (Eq, Show)

-- | Multi-project workspace actions.
--
-- @since 1.0.0
data WorkspaceAction
  = WSAnalyze
    -- ^ Analyze all projects in workspace
  | WSInit
    -- ^ Create workspace configuration file
  | WSList
    -- ^ List configured projects
  | WSDiscover
    -- ^ Auto-discover Haskell projects
  | WSValidate
    -- ^ Validate workspace configuration
  deriving stock (Eq, Show)

-- | Workspace (multi-project) command options.
--
-- Workspaces allow analyzing multiple related projects together,
-- with shared configuration and aggregated reporting.
--
-- @since 1.0.0
data WorkspaceOptions = WorkspaceOptions
  { woAction       :: WorkspaceAction
    -- ^ Action to perform
  , woConfigFile   :: Maybe FilePath
    -- ^ Path to workspace config (default: argus-workspace.toml)
  , woProjects     :: [Text]
    -- ^ Filter to specific projects
  , woTags         :: [Text]
    -- ^ Filter projects by tag
  , woOutputFormat :: Text
    -- ^ Output format: \"terminal\", \"json\", \"sarif\"
  , woParallel     :: Bool
    -- ^ Analyze projects in parallel
  , woFailFast     :: Bool
    -- ^ Stop on first project failure
  , woVerbose      :: Bool
    -- ^ Verbose output
  }
  deriving stock (Eq, Show)

-- | Rule authoring and inspection actions.
--
-- @since 1.0.0
data RuleAction
  = RuleNew Text
    -- ^ Create new rule from template
  | RuleValidate FilePath
    -- ^ Validate rule file syntax
  | RuleTest FilePath
    -- ^ Test rule against sample code
  | RuleList
    -- ^ List all available rules
  | RuleExplain Text
    -- ^ Show detailed rule documentation
  | RuleDocs
    -- ^ Export all rule documentation
  deriving stock (Eq, Show)

-- | Rule authoring command options.
--
-- Tools for creating, testing, and documenting custom rules.
--
-- = Rule Types
--
-- * @\"pattern\"@ - Simple text pattern matching
-- * @\"ast\"@ - GHC AST pattern matching
-- * @\"semantic\"@ - HIE-based semantic rules
--
-- @since 1.0.0
data RuleOptions = RuleOptions
  { roAction       :: RuleAction
    -- ^ Action to perform
  , roRulesDir     :: Maybe FilePath
    -- ^ Custom rules directory (default: .argus/rules)
  , roSampleFile   :: Maybe FilePath
    -- ^ Sample file for rule testing
  , roRuleType     :: Text
    -- ^ Rule type: \"pattern\", \"ast\", \"semantic\"
  , roCategory     :: Maybe Text
    -- ^ Filter rules by category
  , roOutputFormat :: Text
    -- ^ Output format: \"terminal\", \"json\", \"markdown\", \"html\"
  , roVerbose      :: Bool
    -- ^ Verbose output
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Main Command Type
--------------------------------------------------------------------------------

-- | Top-level command type.
--
-- Each constructor represents a CLI subcommand, pairing 'GlobalOptions'
-- with command-specific options. This is the type returned by the
-- command-line parser and dispatched to the appropriate handler.
--
-- @since 1.0.0
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
  | CmdWorkspace GlobalOptions WorkspaceOptions
  | CmdRule GlobalOptions RuleOptions
  deriving stock (Eq, Show)
