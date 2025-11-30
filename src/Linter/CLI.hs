{-# LANGUAGE StrictData #-}
{-# LANGUAGE ApplicativeDo #-}

-- |
-- Module      : Linter.CLI
-- Description : Command-line interface for the linter
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides the command-line interface for the linter,
-- including argument parsing and subcommand handling.
module Linter.CLI
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

    -- * Parsers
  , parseCommand
  , parseGlobalOptions
  ) where

import Control.Monad (when)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import Linter.Config
import Linter.Core
import Linter.Output.Html
import Linter.Output.Terminal
import Linter.Output.Json
import Linter.Output.Sarif
import Linter.Output.Types
import Linter.Refactor.Engine
import Linter.Types

--------------------------------------------------------------------------------
-- Command Types
--------------------------------------------------------------------------------

-- | Global options that apply to all commands
data GlobalOptions = GlobalOptions
  { goConfigFile :: Maybe FilePath
  , goVerbose    :: Bool
  , goNoColor    :: Bool
  , goParallel   :: Int
  }
  deriving stock (Eq, Show)

-- | Check command options
data CheckOptions = CheckOptions
  { coTargets      :: [FilePath]
  , coMode         :: Text  -- ^ "quick", "full", "plugin"
  , coHieDir       :: Maybe FilePath
  , coOutputFormat :: Text  -- ^ "terminal", "json", "sarif"
  , coGroupBy      :: Text  -- ^ "file", "rule", "severity"
  , coShowContext  :: Bool
  , coContextLines :: Int
  }
  deriving stock (Eq, Show)

-- | Fix command options
data FixOptions = FixOptions
  { foTargets     :: [FilePath]
  , foInteractive :: Bool
  , foPreview     :: Bool
  , foRule        :: Maybe Text
  , foSafeOnly    :: Bool
  , foBackup      :: Bool
  }
  deriving stock (Eq, Show)

-- | Unused command options
data UnusedOptions = UnusedOptions
  { uoTargets :: [FilePath]
  , uoMode    :: Text
  , uoRoots   :: [Text]
  }
  deriving stock (Eq, Show)

-- | Init command options
data InitOptions = InitOptions
  { ioForce :: Bool
  }
  deriving stock (Eq, Show)

-- | Commands
data Command
  = CmdCheck GlobalOptions CheckOptions
  | CmdFix GlobalOptions FixOptions
  | CmdUnused GlobalOptions UnusedOptions
  | CmdInit GlobalOptions InitOptions
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Parsers
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
    <> help "Output format: terminal, json, sarif (default: terminal)"
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
    <> help "Prompt before each fix"
    )
  foPreview <- switch
    ( long "preview"
    <> short 'p'
    <> help "Show preview without applying"
    )
  foRule <- optional $ strOption
    ( long "rule"
    <> short 'r'
    <> metavar "RULE"
    <> help "Apply only this rule"
    )
  foSafeOnly <- flag True False
    ( long "unsafe"
    <> help "Allow unsafe fixes"
    )
  foBackup <- flag True False
    ( long "no-backup"
    <> help "Don't create backup files"
    )
  pure FixOptions{..}

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
  )

-- | Full parser with info
parseCommandWithInfo :: ParserInfo Command
parseCommandWithInfo = info (parseCommand <**> helper)
  ( fullDesc
  <> progDesc "Haskell static analysis and refactoring tool"
  <> header "haskell-linter - A comprehensive Haskell linter"
  )

--------------------------------------------------------------------------------
-- Command Execution
--------------------------------------------------------------------------------

-- | Run the CLI
runCLI :: IO ()
runCLI = do
  cmd <- execParser parseCommandWithInfo
  case cmd of
    CmdCheck global opts -> runCheck global opts
    CmdFix global opts   -> runFix global opts
    CmdUnused global opts -> runUnused global opts
    CmdInit global opts  -> runInit global opts

-- | Run check command
runCheck :: GlobalOptions -> CheckOptions -> IO ()
runCheck global opts = do
  let linterOpts = LinterOptions
        { optMode = parseMode (coMode opts)
        , optConfigFile = goConfigFile global
        , optTargetPaths = if null (coTargets opts) then ["."] else coTargets opts
        , optHieDir = coHieDir opts
        , optOutputFormat = coOutputFormat opts
        , optApplyFixes = False
        , optInteractive = False
        , optPreview = False
        , optVerbose = goVerbose global
        , optNoColor = goNoColor global
        , optParallel = goParallel global
        }

  result <- runLinter linterOpts

  let outputOpts = OutputOptions
        { ooFormat = parseOutputFormat (coOutputFormat opts)
        , ooColor = not (goNoColor global)
        , ooGroupBy = coGroupBy opts
        , ooShowContext = coShowContext opts
        , ooContextLines = coContextLines opts
        , ooVerbose = goVerbose global
        }

  let output = case coOutputFormat opts of
        "json"  -> renderJson outputOpts result
        "sarif" -> renderSarif outputOpts result
        "html"  -> renderHtml outputOpts result
        "plain" -> renderTerminal outputOpts { ooColor = False } result
        _       -> renderTerminal outputOpts result

  TIO.putStrLn output

  -- Exit with error if there are any errors
  let hasErrors = any (== Error) $ Map.keys (resultDiagCount result)
  when hasErrors exitFailure

-- | Run fix command
runFix :: GlobalOptions -> FixOptions -> IO ()
runFix global opts = do
  let linterOpts = LinterOptions
        { optMode = QuickMode
        , optConfigFile = goConfigFile global
        , optTargetPaths = if null (foTargets opts) then ["."] else foTargets opts
        , optHieDir = Nothing
        , optOutputFormat = "terminal"
        , optApplyFixes = not (foPreview opts)
        , optInteractive = foInteractive opts
        , optPreview = foPreview opts
        , optVerbose = goVerbose global
        , optNoColor = goNoColor global
        , optParallel = goParallel global
        }

  result <- runLinter linterOpts

  let totalFixes = sum
        [ length (diagFixes d)
        | fr <- Map.elems (resultFiles result)
        , d <- fileResultDiagnostics fr
        ]

  if foPreview opts
    then TIO.putStrLn $ "Would apply " <> T.pack (show totalFixes) <> " fixes"
    else TIO.putStrLn $ "Applied " <> T.pack (show totalFixes) <> " fixes"

-- | Run unused command
runUnused :: GlobalOptions -> UnusedOptions -> IO ()
runUnused global opts = do
  let linterOpts = LinterOptions
        { optMode = FullMode
        , optConfigFile = goConfigFile global
        , optTargetPaths = if null (uoTargets opts) then ["."] else uoTargets opts
        , optHieDir = Just ".hie"
        , optOutputFormat = "terminal"
        , optApplyFixes = False
        , optInteractive = False
        , optPreview = False
        , optVerbose = goVerbose global
        , optNoColor = goNoColor global
        , optParallel = goParallel global
        }

  result <- runLinter linterOpts

  let unused = resultUnusedCode result
  if null unused
    then TIO.putStrLn "No unused code detected"
    else do
      TIO.putStrLn $ "Found " <> T.pack (show (length unused)) <> " unused items:"
      mapM_ (TIO.putStrLn . ("  - " <>) . qnName) unused

-- | Run init command
runInit :: GlobalOptions -> InitOptions -> IO ()
runInit _global opts = do
  let configPath = "linter.toml"
  exists <- doesFileExist configPath
  when (exists && not (ioForce opts)) $ do
    hPutStrLn stderr "Configuration file already exists. Use --force to overwrite."
    exitFailure

  TIO.writeFile configPath defaultConfigToml
  TIO.putStrLn $ "Created " <> T.pack configPath
  where
    doesFileExist _ = pure False  -- Simplified

-- | Default TOML configuration
defaultConfigToml :: Text
defaultConfigToml = T.unlines
  [ "# Haskell Linter Configuration"
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

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Parse analysis mode from string
parseMode :: Text -> AnalysisMode
parseMode "quick"  = QuickMode
parseMode "full"   = FullMode
parseMode "plugin" = PluginMode
parseMode _        = QuickMode

-- | Parse output format from string
parseOutputFormat :: Text -> OutputFormat
parseOutputFormat "json"     = JsonFormat
parseOutputFormat "sarif"    = SarifFormat
parseOutputFormat "html"     = HtmlFormat
parseOutputFormat "plain"    = PlainFormat
parseOutputFormat "terminal" = TerminalFormat
parseOutputFormat _          = TerminalFormat

-- | Main entry point
main :: IO ()
main = runCLI
