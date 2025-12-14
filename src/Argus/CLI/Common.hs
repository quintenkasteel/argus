{-# LANGUAGE StrictData #-}
{-# LANGUAGE ApplicativeDo #-}

-- |
-- Module      : Argus.CLI.Common
-- Description : Common utilities for CLI commands
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides common utilities, parsers, and helper functions
-- used across CLI commands.
module Argus.CLI.Common
  ( -- * Parsers
    parseGlobalOptions
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
  , parseValidationLevel
  , parseConflictStrategy

    -- * Helpers
  , parseMode
  , parseOutputFormat
  , mkProgressConfig
  , parseSeverity
  , defaultConfigToml
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
import Argus.CLI.Types
import Argus.Types (AnalysisMode(..), Severity(..))
import Argus.Output.Types (OutputFormat(..))
import Argus.Output.Progress qualified as Progress
import Argus.Refactor.Validation (ValidationLevel(..))
import Argus.Refactor.FixGraph (ConflictStrategy(..))

--------------------------------------------------------------------------------
-- Global Options Parser
--------------------------------------------------------------------------------

-- | Parse global options
parseGlobalOptions :: Parser GlobalOptions
parseGlobalOptions = do
  goConfigFile <- optional $ strOption
    ( long "config"
    <> short 'c'
    <> metavar "FILE"
    <> help "Path to configuration file"
    )
  goVerbose <- switch
    ( long "verbose"
    <> short 'v'
    <> help "Enable verbose output"
    )
  goNoColor <- switch
    ( long "no-color"
    <> help "Disable colored output"
    )
  goParallel <- option auto
    ( long "parallel"
    <> short 'j'
    <> metavar "N"
    <> value 4
    <> help "Number of parallel jobs (default: 4)"
    )
  pure GlobalOptions{..}

--------------------------------------------------------------------------------
-- Command Options Parsers
--------------------------------------------------------------------------------

-- | Parse check command options
parseCheckOptions :: Parser CheckOptions
parseCheckOptions = do
  coTargets <- many $ argument str
    ( metavar "TARGETS..."
    <> help "Files or directories to analyze"
    )
  coMode <- strOption
    ( long "mode"
    <> short 'm'
    <> metavar "MODE"
    <> value "quick"
    <> help "Analysis mode: quick, full, plugin (default: quick)"
    )
  coHieDir <- optional $ strOption
    ( long "hie-dir"
    <> metavar "DIR"
    <> help "Directory containing HIE files (for full mode)"
    )
  coOutputFormat <- strOption
    ( long "format"
    <> short 'f'
    <> metavar "FORMAT"
    <> value "terminal"
    <> help "Output format: terminal, json, sarif, html, junit, codeclimate, checkstyle (default: terminal)"
    )
  coGroupBy <- strOption
    ( long "group-by"
    <> metavar "GROUP"
    <> value "file"
    <> help "Group results by: file, rule, severity (default: file)"
    )
  coShowContext <- switch
    ( long "context"
    <> help "Show source context"
    )
  coContextLines <- option auto
    ( long "context-lines"
    <> metavar "N"
    <> value 2
    <> help "Number of context lines (default: 2)"
    )
  -- CI mode options
  coBaselinePath <- optional $ strOption
    ( long "baseline"
    <> short 'b'
    <> metavar "FILE"
    <> help "Baseline file for comparison (enables CI mode)"
    )
  coFailOnNew <- switch
    ( long "fail-on-new"
    <> help "Exit with error if new issues found vs baseline"
    )
  coFailOnSeverity <- optional $ strOption
    ( long "fail-on-severity"
    <> metavar "LEVEL"
    <> help "Exit with error if issues at or above severity: error, warning, suggestion, info"
    )
  coFailOnCount <- optional $ option auto
    ( long "fail-on-count"
    <> metavar "N"
    <> help "Exit with error if total issue count exceeds N"
    )
  coFailOnDelta <- optional $ option auto
    ( long "fail-on-delta"
    <> metavar "N"
    <> help "Exit with error if net new issues exceeds N"
    )
  coUpdateBaseline <- switch
    ( long "update-baseline"
    <> help "Update baseline file after analysis"
    )
  coCIQuiet <- switch
    ( long "ci-quiet"
    <> help "Minimal output for CI pipelines"
    )
  pure CheckOptions{..}

-- | Parse fix command options
parseFixOptions :: Parser FixOptions
parseFixOptions = do
  foTargets <- many $ argument str
    ( metavar "TARGETS..."
    <> help "Files or directories to fix"
    )
  foInteractive <- switch
    ( long "interactive"
    <> short 'i'
    <> help "Prompt before each fix with colored diff"
    )
  foPreview <- switch
    ( long "preview"
    <> short 'p'
    <> help "Show preview without applying (deprecated, use --dry-run)"
    )
  foRule <- optional $ strOption
    ( long "rule"
    <> short 'r'
    <> metavar "RULE"
    <> help "Apply only this rule"
    )
  foSafeOnly <- flag True False
    ( long "unsafe"
    <> help "Allow unsafe fixes (not recommended)"
    )
  foBackup <- flag True False
    ( long "no-backup"
    <> help "Don't create backup files"
    )
  -- New safe refactoring options
  foValidate <- flag True False
    ( long "no-validate"
    <> help "Skip syntax validation after each fix (faster but unsafe)"
    )
  foValidateLevel <- option parseValidationLevel
    ( long "validate-level"
    <> metavar "LEVEL"
    <> value SyntaxValidation
    <> help "Validation level: none, structural, syntax, semantic (default: syntax)"
    )
  foTransactional <- flag True False
    ( long "no-transactional"
    <> help "Don't rollback on failure (partial fixes allowed)"
    )
  foConflictStrategy <- option parseConflictStrategy
    ( long "conflict-strategy"
    <> metavar "STRATEGY"
    <> value PreferPreferred
    <> help "How to handle conflicts: skip, preferred, first, severity, smaller (default: preferred)"
    )
  foDryRun <- switch
    ( long "dry-run"
    <> short 'n'
    <> help "Show what would be changed without modifying files"
    )
  foShowDiff <- flag True False
    ( long "no-diff"
    <> help "Don't show colored diffs"
    )
  foVerbose <- switch
    ( long "verbose-fix"
    <> help "Show detailed fix progress"
    )
  pure FixOptions{..}

-- | Parse validation level
parseValidationLevel :: ReadM ValidationLevel
parseValidationLevel = eitherReader $ \s -> case s of
  "none"       -> Right NoValidation
  "structural" -> Right StructuralOnly
  "syntax"     -> Right SyntaxValidation
  "semantic"   -> Right SemanticValidation
  _            -> Left $ "Unknown validation level: " <> s <> ". Use: none, structural, syntax, semantic"

-- | Parse conflict strategy
parseConflictStrategy :: ReadM ConflictStrategy
parseConflictStrategy = eitherReader $ \s -> case s of
  "skip"      -> Right SkipAll
  "preferred" -> Right PreferPreferred
  "first"     -> Right SkipSecond
  "severity"  -> Right PreferSeverity
  "smaller"   -> Right PreferSmaller
  _           -> Left $ "Unknown conflict strategy: " <> s <> ". Use: skip, preferred, first, severity, smaller"

-- | Parse unused command options
parseUnusedOptions :: Parser UnusedOptions
parseUnusedOptions = do
  uoTargets <- many $ argument str
    ( metavar "TARGETS..."
    <> help "Files or directories to analyze"
    )
  uoMode <- strOption
    ( long "mode"
    <> short 'm'
    <> metavar "MODE"
    <> value "full"
    <> help "Analysis mode: full, plugin (default: full)"
    )
  uoRoots <- many $ strOption
    ( long "root"
    <> metavar "PATTERN"
    <> help "Additional root patterns"
    )
  uoOutputFormat <- strOption
    ( long "format"
    <> short 'f'
    <> metavar "FORMAT"
    <> value "terminal"
    <> help "Output format: terminal, json, sarif, html, junit, codeclimate, checkstyle (default: terminal)"
    )
  uoShowContext <- switch
    ( long "context"
    <> help "Show source context"
    )
  uoContextLines <- option auto
    ( long "context-lines"
    <> metavar "N"
    <> value 2
    <> help "Number of context lines (default: 2)"
    )
  pure UnusedOptions{..}

-- | Parse init command options
parseInitOptions :: Parser InitOptions
parseInitOptions = do
  ioForce <- switch
    ( long "force"
    <> short 'f'
    <> help "Overwrite existing configuration"
    )
  pure InitOptions{..}

-- | Parse index command options
parseIndexOptions :: Parser IndexOptions
parseIndexOptions = do
  ixTargetDir <- optional $ argument str
    ( metavar "DIR"
    <> help "Project directory (default: current directory)"
    )
  ixBuild <- flag True False
    ( long "no-build"
    <> help "Skip building the project"
    )
  ixClean <- switch
    ( long "clean"
    <> help "Clean before building"
    )
  ixDbPath <- optional $ strOption
    ( long "db"
    <> short 'd'
    <> metavar "PATH"
    <> help "Custom database path (default: .hiedb)"
    )
  ixSymlink <- flag True False
    ( long "no-symlink"
    <> help "Don't create .hie symlink"
    )
  ixYes <- switch
    ( long "yes"
    <> short 'y'
    <> help "Auto-accept prompts (e.g., install hiedb if missing)"
    )
  pure IndexOptions{..}

-- | Parse watch command options
parseWatchOptions :: Parser WatchOptions
parseWatchOptions = do
  woTargets <- many $ argument str
    ( metavar "DIRS..."
    <> help "Directories to watch (default: current directory)"
    )
  woDebounceMs <- option auto
    ( long "debounce"
    <> metavar "MS"
    <> value 500
    <> help "Debounce delay in milliseconds (default: 500)"
    )
  woPollIntervalMs <- option auto
    ( long "poll-interval"
    <> metavar "MS"
    <> value 1000
    <> help "Polling interval in milliseconds (default: 1000)"
    )
  woClearScreen <- flag True False
    ( long "no-clear"
    <> help "Don't clear screen on updates"
    )
  woShowTimestamp <- flag True False
    ( long "no-timestamp"
    <> help "Don't show timestamps"
    )
  pure WatchOptions{..}

-- | Parse diff command options
parseDiffOptions :: Parser DiffOptions
parseDiffOptions = do
  dfTargets <- many $ argument str
    ( metavar "TARGETS..."
    <> help "Files or directories to analyze"
    )
  dfBaseline <- optional $ strOption
    ( long "baseline"
    <> short 'b'
    <> metavar "FILE"
    <> help "Baseline file to compare against"
    )
  dfGitRef <- optional $ strOption
    ( long "git-ref"
    <> short 'g'
    <> metavar "REF"
    <> help "Git ref to compare against (e.g., HEAD~1, main)"
    )
  dfShowNew <- switch
    ( long "new-only"
    <> help "Show only new issues"
    )
  dfShowFixed <- switch
    ( long "fixed-only"
    <> help "Show only fixed issues"
    )
  dfOutputFormat <- strOption
    ( long "format"
    <> short 'f'
    <> metavar "FORMAT"
    <> value "terminal"
    <> help "Output format: terminal, json (default: terminal)"
    )
  pure DiffOptions{..}

-- | Parse baseline command options
parseBaselineOptions :: Parser BaselineOptions
parseBaselineOptions = do
  blTargets <- many $ argument str
    ( metavar "TARGETS..."
    <> help "Files or directories to analyze"
    )
  blOutputFile <- strOption
    ( long "output"
    <> short 'o'
    <> metavar "FILE"
    <> value ".argus-baseline.json"
    <> help "Output file for baseline (default: .argus-baseline.json)"
    )
  blForce <- switch
    ( long "force"
    <> short 'f'
    <> help "Overwrite existing baseline file"
    )
  pure BaselineOptions{..}

-- | Parse stats command options
parseStatsOptions :: Parser StatsOptions
parseStatsOptions = do
  stTargets <- many $ argument str
    ( metavar "TARGETS..."
    <> help "Files or directories to analyze"
    )
  stByRule <- switch
    ( long "by-rule"
    <> help "Group statistics by rule"
    )
  stByFile <- switch
    ( long "by-file"
    <> help "Group statistics by file"
    )
  stBySeverity <- switch
    ( long "by-severity"
    <> help "Group statistics by severity"
    )
  stOutputFormat <- strOption
    ( long "format"
    <> short 'f'
    <> metavar "FORMAT"
    <> value "terminal"
    <> help "Output format: terminal, json (default: terminal)"
    )
  pure StatsOptions{..}

-- | Parse daemon options
parseDaemonOptions :: Parser DaemonOptions
parseDaemonOptions = do
  daAction <- parseDaemonAction
  daSocketPath <- optional $ strOption
    ( long "socket"
    <> metavar "PATH"
    <> help "Unix socket path for daemon communication"
    )
  daPort <- optional $ option auto
    ( long "port"
    <> metavar "PORT"
    <> help "TCP port (alternative to Unix socket)"
    )
  daIdleTimeout <- optional $ option auto
    ( long "idle-timeout"
    <> metavar "SECONDS"
    <> help "Shutdown daemon after N seconds of inactivity"
    )
  daVerbose <- switch
    ( long "verbose"
    <> short 'v'
    <> help "Enable verbose logging"
    )
  pure DaemonOptions{..}

-- | Parse daemon action
parseDaemonAction :: Parser DaemonAction
parseDaemonAction = hsubparser
  ( command "start" (info (pure DaemonStart)
      (progDesc "Start the daemon"))
  <> command "stop" (info (pure DaemonStop)
      (progDesc "Stop the daemon"))
  <> command "status" (info (pure DaemonStatus)
      (progDesc "Check daemon status"))
  <> command "reload" (info (pure DaemonReload)
      (progDesc "Reload configuration"))
  <> command "check" (info (DaemonCheck <$> many (argument str (metavar "FILES...")))
      (progDesc "Analyze files via daemon"))
  )

-- | Parse LSP server options
parseLspOptions :: Parser LspOptions
parseLspOptions = do
  loDebugLog <- optional $ strOption
    ( long "debug-log"
    <> metavar "FILE"
    <> help "Write debug log to file"
    )
  loAnalyzeOnChange <- switch
    ( long "analyze-on-change"
    <> help "Analyze files on every change (expensive, use with caution)"
    )
  loDebounceMs <- option auto
    ( long "debounce"
    <> metavar "MS"
    <> value 500
    <> help "Debounce time in milliseconds for change analysis (default: 500)"
    )
  loProgressReporting <- flag True False
    ( long "no-progress"
    <> help "Disable progress reporting to the client"
    )
  pure LspOptions{..}

-- | Parse architecture options
parseArchitectureOptions :: Parser ArchitectureOptions
parseArchitectureOptions = do
  aoTargets <- many $ argument str
    ( metavar "TARGETS..."
    <> help "Files or directories to analyze"
    )
  aoOutputFormat <- strOption
    ( long "format"
    <> short 'f'
    <> metavar "FORMAT"
    <> value "terminal"
    <> help "Output format: terminal, json, dot (default: terminal)"
    )
  aoShowGraph <- switch
    ( long "graph"
    <> short 'g'
    <> help "Generate DOT graph output"
    )
  aoShowMetrics <- flag True False
    ( long "no-metrics"
    <> help "Don't show coupling/instability metrics"
    )
  aoShowViolations <- flag True False
    ( long "no-violations"
    <> help "Don't show layer violations"
    )
  aoShowCycles <- flag True False
    ( long "no-cycles"
    <> help "Don't show circular dependencies"
    )
  aoGraphOutput <- optional $ strOption
    ( long "graph-output"
    <> short 'o'
    <> metavar "FILE"
    <> help "Write DOT graph to file (implies --graph)"
    )
  pure ArchitectureOptions{..}

-- | Parse pack options
parsePackOptions :: Parser PackOptions
parsePackOptions = do
  poAction <- hsubparser
    ( command "list" (info (pure PackList)
        (progDesc "List all available rule packs"))
    <> command "show" (info (PackShow <$> packNameArg)
        (progDesc "Show details of a specific pack"))
    <> command "validate" (info (PackValidate <$> optional packFileArg)
        (progDesc "Validate a pack definition"))
    <> command "export" (info (PackExport <$> packNameArg <*> outputFileArg)
        (progDesc "Export a pack to a file"))
    <> command "import" (info (PackImport <$> packFileArg)
        (progDesc "Import a pack from a file"))
    <> command "create" (info (PackCreate <$> packNameArg)
        (progDesc "Create a new custom pack"))
    )
  poVersion <- optional $ strOption
    ( long "version"
    <> short 'V'
    <> metavar "VERSION"
    <> help "Version for the pack (e.g., 1.0.0)"
    )
  poAuthor <- optional $ strOption
    ( long "author"
    <> short 'a'
    <> metavar "AUTHOR"
    <> help "Author name for the pack"
    )
  poOutputJson <- switch
    ( long "json"
    <> help "Output results as JSON"
    )
  pure PackOptions{..}
  where
    packNameArg = argument str
      ( metavar "PACK"
      <> help "Name of the rule pack"
      )
    packFileArg = argument str
      ( metavar "FILE"
      <> help "Path to pack file"
      )
    outputFileArg = argument str
      ( metavar "OUTPUT"
      <> help "Output file path"
      )

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Parse analysis mode from string
parseMode :: Text -> AnalysisMode
parseMode "quick"  = QuickMode
parseMode "full"   = FullMode
parseMode "plugin" = PluginMode
parseMode _        = QuickMode

-- | Parse output format from string
parseOutputFormat :: Text -> OutputFormat
parseOutputFormat "json"        = JsonFormat
parseOutputFormat "sarif"       = SarifFormat
parseOutputFormat "html"        = HtmlFormat
parseOutputFormat "plain"       = PlainFormat
parseOutputFormat "terminal"    = TerminalFormat
parseOutputFormat "junit"       = JUnitFormat
parseOutputFormat "codeclimate" = CodeClimateFormat
parseOutputFormat "checkstyle"  = CheckstyleFormat
parseOutputFormat _             = TerminalFormat

-- | Parse severity from text
parseSeverity :: Text -> Severity
parseSeverity t = case T.toLower t of
  "error"      -> Error
  "warning"    -> Warning
  "suggestion" -> Suggestion
  "info"       -> Info
  _            -> Warning

-- | Create progress config from global options
mkProgressConfig :: GlobalOptions -> Bool -> Progress.ProgressConfig
mkProgressConfig global isTerminal = Progress.ProgressConfig
  { Progress.pcEnabled = not (goNoColor global) && isTerminal
  , Progress.pcColor = not (goNoColor global)
  , Progress.pcUnicode = True
  , Progress.pcInteractive = isTerminal
  , Progress.pcWidth = 80
  }

-- | Default TOML configuration
defaultConfigToml :: Text
defaultConfigToml = T.unlines
  [ "# Argus Configuration - The All-Seeing Haskell Static Analyzer"
  , ""
  , "[general]"
  , "directories = [\"src\", \"app\"]"
  , "exclude = [\"Generated/**\"]"
  , "mode = \"quick\""
  , ""
  , "[output]"
  , "format = \"terminal\""
  , "color = true"
  , "group-by = \"file\""
  , "show-context = true"
  , "context-lines = 2"
  , ""
  , "[unused]"
  , "enabled = true"
  , "check-functions = true"
  , "check-types = true"
  , "check-imports = true"
  , "roots = [\"^Main.main$\", \"^Paths_.*\"]"
  , ""
  , "[naming]"
  , "enabled = true"
  , ""
  , "[[naming.types]]"
  , "pattern = \"*Id\""
  , "replacement = \"Key *\""
  , "severity = \"warning\""
  , ""
  , "[patterns]"
  , "enabled = true"
  , ""
  , "[imports]"
  , "remove-unused = true"
  , "suggest-qualified = [\"Data.Map\", \"Data.Set\", \"Data.Text\"]"
  , ""
  , "[fix]"
  , "enabled = true"
  , "safe-only = true"
  , "preview = true"
  , "backup = true"
  ]
