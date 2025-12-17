{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Baseline
-- Description : Baseline command implementation
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module implements the baseline command for creating and managing
-- baseline files that track known issues.
module Argus.CLI.Baseline
  ( runBaseline
  ) where

import Control.Monad (when)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import System.Directory (doesFileExist)
import System.Exit (exitFailure, exitSuccess)

import Argus.CLI.Types
import Argus.CLI.Common (mkProgressConfig)
import Argus.Core
import Argus.Types
import Argus.Output.Progress qualified as Progress
import Argus.Baseline.Types
  ( Baseline (..)
  , mkBaselineEntry
  , emptyBaseline
  )
import Argus.Baseline.IO
  ( saveBaseline
  , formatBaselineError
  )

-- | Run baseline command - create a baseline file
runBaseline :: GlobalOptions -> BaselineOptions -> IO ()
runBaseline global opts = do
  let targets = if null (blTargets opts) then ["."] else blTargets opts
      outputFile = blOutputFile opts
      progressCfg = mkProgressConfig global True

  -- Check if baseline exists and force flag
  exists <- doesFileExist outputFile
  when (exists && not (blForce opts)) $ do
    TIO.putStrLn $ "Baseline file already exists: " <> T.pack outputFile
    TIO.putStrLn "Use --force to overwrite"
    exitFailure

  when (Progress.pcEnabled progressCfg) $
    Progress.printInfo progressCfg $ "Creating baseline for: " <> T.intercalate ", " (map T.pack targets)

  -- Run analysis
  let linterOpts = ArgusOptions
        { optMode = FullMode
        , optConfigFile = goConfigFile global
        , optTargetPaths = targets
        , optHieDir = Nothing
        , optOutputFormat = "terminal"
        , optApplyFixes = False
        , optInteractive = False
        , optPreview = False
        , optVerbosity = goVerbosity global
        , optNoColor = goNoColor global
        , optParallel = fromIntegral (goParallel global)
        }

  analysisResult <- runArgus linterOpts

  -- Extract diagnostics from AnalysisResult
  let diags = concatMap fileResultDiagnostics (Map.elems (resultFiles analysisResult))

  -- Create baseline from diagnostics
  now <- getCurrentTime
  let entries = map (\d -> mkBaselineEntry d Nothing) diags
      baseline = (emptyBaseline now)
        { baselineEntries = entries
        }

  -- Save baseline
  saveResult <- saveBaseline outputFile baseline
  case saveResult of
    Left err -> do
      TIO.putStrLn $ formatBaselineError err
      exitFailure
    Right () -> do
      when (Progress.pcEnabled progressCfg) $ do
        Progress.printSuccess progressCfg $ "Baseline created: " <> T.pack outputFile
        Progress.printInfo progressCfg $ "Total issues: " <> T.pack (show $ length entries)

      -- Print summary
      TIO.putStrLn $ "\n" <> colorGreen "✓ " <> "Baseline created with " <>
        T.pack (show $ length entries) <> " entries"
      TIO.putStrLn $ "  Saved to: " <> T.pack outputFile
      exitSuccess

-- | ANSI color helpers
colorGreen :: Text -> Text
colorGreen t = "\ESC[32m" <> t <> "\ESC[0m"
