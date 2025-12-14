{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Diff
-- Description : Diff command implementation
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module implements the diff command for comparing analysis results.
module Argus.CLI.Diff
  ( runDiff
  ) where

import Control.Monad (when, unless)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Exit (exitFailure)

import Argus.CLI.Types
import Argus.CLI.Common (mkProgressConfig)
import Argus.Core
import Argus.Types
import Argus.Output.Progress qualified as Progress
import Argus.Baseline.Types
  ( BaselineDiff (..)
  , DiffEntry (..)
  , BaselineEntry (..)
  )
import Argus.Baseline.IO
  ( loadBaseline
  , defaultBaselinePath
  , formatBaselineError
  )
import Argus.Baseline.Diff
  ( diffBaseline
  , summarizeDiff
  )

-- | Run diff command - compare current analysis with baseline or git ref
runDiff :: GlobalOptions -> DiffOptions -> IO ()
runDiff global opts = do
  let targets = if null (dfTargets opts) then ["."] else dfTargets opts
      progressCfg = mkProgressConfig global (dfOutputFormat opts == "terminal")

  when (Progress.pcEnabled progressCfg) $
    Progress.printInfo progressCfg "Running diff analysis..."

  -- Determine baseline path
  let baselinePath = case dfBaseline opts of
        Just p -> p
        Nothing -> defaultBaselinePath

  -- Load baseline
  baselineResult <- loadBaseline baselinePath
  case baselineResult of
    Left err -> do
      TIO.putStrLn $ formatBaselineError err
      exitFailure
    Right baseline -> do
      -- Run current analysis
      let linterOpts = ArgusOptions
            { optMode = FullMode
            , optConfigFile = goConfigFile global
            , optTargetPaths = targets
            , optHieDir = Nothing
            , optOutputFormat = dfOutputFormat opts
            , optApplyFixes = False
            , optInteractive = False
            , optPreview = False
            , optVerbose = goVerbose global
            , optNoColor = goNoColor global
            , optParallel = fromIntegral (goParallel global)
            }

      analysisResult <- runArgus linterOpts

      -- Extract diagnostics from AnalysisResult
      let currentDiags = concatMap fileResultDiagnostics (Map.elems (resultFiles analysisResult))

      -- Compute diff
      let diff = diffBaseline baseline currentDiags

      -- Output results based on format
      case dfOutputFormat opts of
        "json" -> outputJsonDiff diff
        _ -> outputTerminalDiff progressCfg diff (dfShowNew opts) (dfShowFixed opts)

-- | Output diff in terminal format
outputTerminalDiff :: Progress.ProgressConfig -> BaselineDiff -> Bool -> Bool -> IO ()
outputTerminalDiff _progressCfg diff showNew showFixed = do
  TIO.putStrLn $ "\n" <> colorBold "BASELINE COMPARISON"
  TIO.putStrLn $ T.replicate 50 "="

  -- Summary
  TIO.putStrLn $ "\n" <> colorBold "Summary:"
  TIO.putStrLn $ "  " <> summarizeDiff diff

  -- New issues (regressions)
  when (showNew || not showFixed) $
    unless (null (bdNew diff)) $ do
      TIO.putStrLn $ "\n" <> colorRed "New Issues (Regressions):" <> " [" <> T.pack (show $ length $ bdNew diff) <> "]"
      mapM_ (printDiffEntry colorRed "+") (bdNew diff)

  -- Resolved issues
  when (showFixed || not showNew) $
    unless (null (bdResolved diff)) $ do
      TIO.putStrLn $ "\n" <> colorGreen "Resolved Issues:" <> " [" <> T.pack (show $ length $ bdResolved diff) <> "]"
      mapM_ (printDiffEntry colorGreen "-") (bdResolved diff)

  -- Expired baselined issues
  unless (null (bdExpired diff)) $ do
    TIO.putStrLn $ "\n" <> colorYellow "Expired Baseline Entries:" <> " [" <> T.pack (show $ length $ bdExpired diff) <> "]"
    mapM_ (printDiffEntry colorYellow "!") (bdExpired diff)

  -- Existing (still matching baseline)
  when (not (null (bdExisting diff))) $
    TIO.putStrLn $ "\n" <> colorBlue "Existing (baselined): " <> T.pack (show $ length $ bdExisting diff) <> " issues"

  -- Final status
  TIO.putStrLn ""
  if null (bdNew diff)
    then TIO.putStrLn $ colorGreen "✓ No regressions detected"
    else TIO.putStrLn $ colorRed "✗ " <> T.pack (show $ length $ bdNew diff) <> " regression(s) detected"

-- | Output diff in JSON format
outputJsonDiff :: BaselineDiff -> IO ()
outputJsonDiff diff = do
  TIO.putStrLn "{"
  TIO.putStrLn $ "  \"newIssues\": " <> T.pack (show $ length $ bdNew diff) <> ","
  TIO.putStrLn $ "  \"resolvedIssues\": " <> T.pack (show $ length $ bdResolved diff) <> ","
  TIO.putStrLn $ "  \"existingIssues\": " <> T.pack (show $ length $ bdExisting diff) <> ","
  TIO.putStrLn $ "  \"expiredIssues\": " <> T.pack (show $ length $ bdExpired diff) <> ","
  TIO.putStrLn $ "  \"hasRegressions\": " <> (if null (bdNew diff) then "false" else "true")
  TIO.putStrLn "}"

-- | Print a diff entry
printDiffEntry :: (Text -> Text) -> Text -> DiffEntry -> IO ()
printDiffEntry colorFn prefix entry = do
  let location = case deDiagnostic entry of
        Just diag ->
          let span' = diagSpan diag
          in T.pack (srcSpanFile span') <> ":" <>
             T.pack (show $ unLine $ srcSpanStartLine span')
        Nothing -> case deBaseline entry of
          Just be -> T.pack (beFile be) <> ":" <> T.pack (show $ beLine be)
          Nothing -> "<unknown>"

      message = case deDiagnostic entry of
        Just diag -> diagMessage diag
        Nothing -> case deBaseline entry of
          Just be -> beMessage be
          Nothing -> ""

      rule = case deDiagnostic entry of
        Just diag -> maybe "" id (diagCode diag)
        Nothing -> case deBaseline entry of
          Just be -> beRule be
          Nothing -> ""

  TIO.putStrLn $ "  " <> colorFn prefix <> " " <> location <> " [" <> rule <> "]"
  TIO.putStrLn $ "    " <> message

-- | ANSI color helpers
colorBold :: Text -> Text
colorBold t = "\ESC[1m" <> t <> "\ESC[0m"

colorRed :: Text -> Text
colorRed t = "\ESC[31m" <> t <> "\ESC[0m"

colorYellow :: Text -> Text
colorYellow t = "\ESC[33m" <> t <> "\ESC[0m"

colorGreen :: Text -> Text
colorGreen t = "\ESC[32m" <> t <> "\ESC[0m"

colorBlue :: Text -> Text
colorBlue t = "\ESC[34m" <> t <> "\ESC[0m"
