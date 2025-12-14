{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Fix
-- Description : Fix command implementation
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module implements the fix command for auto-fixing issues.
module Argus.CLI.Fix
  ( runFix
  ) where

import Control.Monad (when)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Exit (exitFailure)

import Argus.CLI.Types
import Argus.CLI.Common (mkProgressConfig)
import Argus.Core
import Argus.Types
import Argus.Refactor.Engine
import Argus.Output.Progress qualified as Progress

-- | Run fix command
runFix :: GlobalOptions -> FixOptions -> IO ()
runFix global opts = do
  let progressCfg = mkProgressConfig global True

  -- First, run linter to get diagnostics
  when (Progress.pcEnabled progressCfg) $
    Progress.printInfo progressCfg "Scanning for fixable issues..."

  let linterOpts = ArgusOptions
        { optMode = QuickMode
        , optConfigFile = goConfigFile global
        , optTargetPaths = if null (foTargets opts) then ["."] else foTargets opts
        , optHieDir = Nothing
        , optOutputFormat = "terminal"
        , optApplyFixes = False  -- We handle fixes ourselves now
        , optInteractive = False
        , optPreview = False
        , optVerbose = goVerbose global
        , optNoColor = goNoColor global
        , optParallel = fromIntegral (goParallel global)
        }

  result <- runArgus linterOpts

  -- Collect all diagnostics with fixes
  let filesDiags =
        [ (path, fileResultDiagnostics fr)
        | (path, fr) <- Map.toList (resultFiles result)
        , not (null (fileResultDiagnostics fr))
        , any (not . null . diagFixes) (fileResultDiagnostics fr)
        ]

  let totalDiags = sum [length ds | (_, ds) <- filesDiags]
      totalFixes = sum [length (diagFixes d) | (_, ds) <- filesDiags, d <- ds]

  if null filesDiags
    then TIO.putStrLn "No fixes available."
    else do
      -- Determine mode
      let isDryRun = foDryRun opts || foPreview opts
          mode = if foInteractive opts
                 then ModeInteractive
                 else if isDryRun
                      then ModePreview
                      else ModeAuto

      -- Build engine options from CLI options
      let engineOpts = EngineOptions
            { eoMode          = mode
            , eoValidate      = foValidate opts
            , eoValidateLevel = foValidateLevel opts
            , eoSafeOnly      = foSafeOnly opts
            , eoTransactional = foTransactional opts
            , eoBackup        = foBackup opts
            , eoConflictStrat = foConflictStrategy opts
            , eoDryRun        = isDryRun
            , eoVerbose       = foVerbose opts || goVerbose global
            , eoShowDiff      = foShowDiff opts && not (goNoColor global)
            }

      -- Show summary before applying
      when (foVerbose opts || goVerbose global) $ do
        TIO.putStrLn $ "Found " <> T.pack (show totalDiags) <> " diagnostics with "
                     <> T.pack (show totalFixes) <> " available fixes across "
                     <> T.pack (show (length filesDiags)) <> " file(s)"
        TIO.putStrLn ""
        TIO.putStrLn $ "Options:"
        TIO.putStrLn $ "  Mode: " <> T.pack (show mode)
        TIO.putStrLn $ "  Validation: " <> T.pack (show (foValidateLevel opts))
        TIO.putStrLn $ "  Transactional: " <> T.pack (show (foTransactional opts))
        TIO.putStrLn $ "  Conflict strategy: " <> T.pack (show (foConflictStrategy opts))
        TIO.putStrLn ""

      -- Apply fixes using the safe engine
      engineResult <- refactorSafely engineOpts filesDiags

      -- Report results
      if erSuccess engineResult
        then do
          if isDryRun
            then do
              TIO.putStrLn $ "✓ Dry run complete. Would apply "
                           <> T.pack (show (erFixesApplied engineResult))
                           <> " fix(es) to "
                           <> T.pack (show (erFilesChanged engineResult))
                           <> " file(s)"
            else do
              TIO.putStrLn $ "✓ Successfully applied "
                           <> T.pack (show (erFixesApplied engineResult))
                           <> " fix(es) to "
                           <> T.pack (show (erFilesChanged engineResult))
                           <> " file(s)"

          when (erFixesSkipped engineResult > 0) $
            TIO.putStrLn $ "  (skipped " <> T.pack (show (erFixesSkipped engineResult))
                         <> " fix(es) due to conflicts)"

          when (erConflicts engineResult > 0) $
            TIO.putStrLn $ "  (resolved " <> T.pack (show (erConflicts engineResult))
                         <> " conflict(s))"

        else do
          TIO.putStrLn $ "✗ Fix operation failed"
          TIO.putStrLn $ "  Applied: " <> T.pack (show (erFixesApplied engineResult))
          TIO.putStrLn $ "  Failed: " <> T.pack (show (erFixesFailed engineResult))
          TIO.putStrLn $ "  Skipped: " <> T.pack (show (erFixesSkipped engineResult))

          when (foTransactional opts) $
            TIO.putStrLn "  (Changes rolled back due to transactional mode)"

          exitFailure
