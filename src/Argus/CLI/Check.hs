{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Check
-- Description : Check command implementation
-- Copyright   : (c) 2024
-- License     : MIT
--
-- = Overview
--
-- This module implements the @argus check@ command, which performs static
-- analysis on Haskell source files and reports diagnostics.
--
-- = Execution Flow
--
-- @
-- ┌─────────────────────────────────────────────────────────────────────┐
-- │                         runCheck                                     │
-- │                                                                      │
-- │  1. Build Options ──► 2. Run Analysis ──► 3. Format Output          │
-- │                              │                    │                  │
-- │                              ▼                    ▼                  │
-- │                       ArgusResult           OutputFormat             │
-- │                              │                    │                  │
-- │                              └────────┬───────────┘                  │
-- │                                       │                              │
-- │                              4. CI Mode Check                        │
-- │                                       │                              │
-- │                                       ▼                              │
-- │                              5. Exit Code                            │
-- └─────────────────────────────────────────────────────────────────────┘
-- @
--
-- = CI Mode
--
-- When a baseline path is provided, the command operates in CI mode:
--
-- 1. Compares results against the baseline
-- 2. Reports new, fixed, and unchanged issues
-- 3. Exits with non-zero status based on thresholds:
--    - @--fail-on-new@: New issues vs baseline
--    - @--fail-on-severity@: Issues at threshold severity
--    - @--fail-on-count@: Total issue count
--    - @--fail-on-delta@: Net new issues
--
-- = Output Formats
--
-- * Terminal: Human-readable with ANSI colors
-- * JSON: Machine-readable for tooling
-- * SARIF: GitHub code scanning integration
-- * JUnit: CI system test reports
-- * CodeClimate: Code Climate integration
-- * Checkstyle: Checkstyle XML format
--
-- @since 1.0.0
module Argus.CLI.Check
  ( -- * Command Entry Point
    runCheck
  ) where

import Control.Monad (when, unless)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Exit (exitFailure, exitWith, ExitCode(ExitFailure))

import Argus.CLI.Types
import Argus.CLI.Common (parseMode, parseOutputFormat, parseSeverity, mkProgressConfig)
import Argus.Core
import Argus.Types
import Argus.Output.Types
import Argus.Output.Terminal
import Argus.Output.Json
import Argus.Output.Sarif
import Argus.Output.Html
import Argus.Output.JUnit
import Argus.Output.CodeClimate
import Argus.Output.Checkstyle
import Argus.Output.Progress qualified as Progress
import Argus.Baseline qualified as Baseline
import Argus.Baseline.Types (CIOptions(..), CIResult(..))

-- | Run the check command.
--
-- Performs static analysis on the specified targets and outputs
-- diagnostics in the requested format. In CI mode, compares against
-- a baseline and applies failure thresholds.
--
-- = Exit Codes
--
-- * 0: Success (no errors, or within thresholds)
-- * 1: Analysis found errors
-- * 2: CI threshold exceeded
--
-- @since 1.0.0
runCheck :: GlobalOptions -> CheckOptions -> IO ()
runCheck global opts = do
  let progressCfg = mkProgressConfig global (coOutputFormat opts == "terminal")
      linterOpts = ArgusOptions
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
        , optParallel = fromIntegral (goParallel global)
        }

  -- Show analyzing message if terminal output
  when (coOutputFormat opts == "terminal" && Progress.pcEnabled progressCfg) $
    Progress.printInfo progressCfg "Analyzing..."

  result <- runArgus linterOpts

  -- Show success/warning message based on results
  when (coOutputFormat opts == "terminal" && Progress.pcEnabled progressCfg) $ do
    let totalDiags = sum $ Map.elems (resultDiagCount result)
    if totalDiags == 0
      then Progress.printSuccess progressCfg "No issues found"
      else Progress.printWarning progressCfg $ T.pack (show totalDiags) <> " issue(s) found"

  let outputOpts = OutputOptions
        { ooFormat = parseOutputFormat (coOutputFormat opts)
        , ooColor = not (goNoColor global)
        , ooGroupBy = coGroupBy opts
        , ooShowContext = coShowContext opts
        , ooContextLines = coContextLines opts
        , ooVerbose = goVerbose global
        , ooSourceCache = Map.empty
        }

  let output = case coOutputFormat opts of
        "json"        -> renderJson outputOpts result
        "sarif"       -> renderSarif outputOpts result
        "html"        -> renderHtml outputOpts result
        "plain"       -> renderTerminal outputOpts { ooColor = False } result
        "junit"       -> outText (renderJUnit outputOpts result)
        "codeclimate" -> outText (renderCodeClimate outputOpts result)
        "checkstyle"  -> outText (renderCheckstyle outputOpts result)
        _             -> renderTerminal outputOpts result

  TIO.putStrLn output

  -- Collect all diagnostics for CI mode
  let allDiags = concatMap fileResultDiagnostics $ Map.elems (resultFiles result)

  -- CI mode: check baseline if provided
  case coBaselinePath opts of
    Just baselinePath -> do
      -- Build CI options from check options
      let ciOpts = CIOptions
            { ciBaselinePath = Just baselinePath
            , ciFailOnNew = coFailOnNew opts
            , ciFailOnSeverity = parseSeverity <$> coFailOnSeverity opts
            , ciFailOnCount = coFailOnCount opts
            , ciFailOnDelta = coFailOnDelta opts
            , ciUpdateBaseline = coUpdateBaseline opts
            , ciQuiet = coCIQuiet opts
            }

      -- Run CI check
      ciResult <- Baseline.runCICheck ciOpts allDiags

      -- Print CI result unless quiet
      unless (coCIQuiet opts) $ do
        TIO.putStrLn ""
        TIO.putStrLn $ Baseline.formatCIResult ciResult

      -- Exit based on CI result
      when (crExitCode ciResult /= 0) $
        exitWith (ExitFailure (crExitCode ciResult))

    Nothing -> do
      -- No baseline mode - use traditional exit logic
      let hasErrors = any (== Error) $ Map.keys (resultDiagCount result)

      -- Check fail-on-severity even without baseline
      case coFailOnSeverity opts of
        Just sevText -> do
          let threshold = parseSeverity sevText
          when (any (\d -> diagSeverity d >= threshold) allDiags) exitFailure
        Nothing -> pure ()

      -- Check fail-on-count even without baseline
      case coFailOnCount opts of
        Just maxCount -> when (length allDiags > maxCount) exitFailure
        Nothing -> pure ()

      when hasErrors exitFailure

