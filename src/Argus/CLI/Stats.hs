{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Stats
-- Description : Stats command implementation
-- Copyright   : (c) 2024
-- License     : MIT
--
-- = Overview
--
-- This module implements the @argus stats@ command, which generates
-- aggregate statistics about detected issues.
--
-- = Output
--
-- Statistics can be grouped by:
--
-- * __Severity__: Error, Warning, Info, Suggestion counts
-- * __Rule__: Top 20 most frequently triggered rules
-- * __File__: Top 20 files with most issues
--
-- = Visualization
--
-- Terminal output includes bar charts:
--
-- @
-- By Severity:
--   Error         15 ( 5%)  ███
--   Warning      200 (67%)  ████████████████████████████████████████
--   Suggestion    84 (28%)  ████████████████
-- @
--
-- @since 1.0.0
module Argus.CLI.Stats
  ( -- * Command Entry Point
    runStats
  ) where

import Control.Monad (when)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing, Down(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

import Argus.CLI.Types
import Argus.CLI.Common (mkProgressConfig)
import Argus.Core
import Argus.Types
import Argus.Output.Progress qualified as Progress

-- | Extract rule/code from a Diagnostic
getDiagRule :: Diagnostic -> Text
getDiagRule d = fromMaybe "(no-rule)" (diagCode d)

-- | Extract file path from a Diagnostic
getDiagPath :: Diagnostic -> Text
getDiagPath d = T.pack (srcSpanFile (diagSpan d))

-- | Run the stats command.
--
-- Runs analysis and outputs aggregate statistics in the requested format.
--
-- @since 1.0.0
runStats :: GlobalOptions -> StatsOptions -> IO ()
runStats global opts = do
  let targets = if null (stTargets opts) then ["."] else stTargets opts
      progressCfg = mkProgressConfig global (stOutputFormat opts == "terminal")

  when (Progress.pcEnabled progressCfg) $
    Progress.printInfo progressCfg "Gathering statistics..."

  -- Run analysis
  let linterOpts = ArgusOptions
        { optMode = FullMode
        , optConfigFile = goConfigFile global
        , optTargetPaths = targets
        , optHieDir = Nothing
        , optOutputFormat = stOutputFormat opts
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

  case stOutputFormat opts of
    "json" -> outputJsonStats opts diags
    _ -> outputTerminalStats opts progressCfg diags

-- | Output statistics in terminal format
outputTerminalStats :: StatsOptions -> Progress.ProgressConfig -> [Diagnostic] -> IO ()
outputTerminalStats opts _ diags = do
  TIO.putStrLn $ "\n" <> colorBold "ANALYSIS STATISTICS"
  TIO.putStrLn $ T.replicate 50 "="

  -- Overall summary
  TIO.putStrLn $ "\n" <> colorBold "Summary:"
  TIO.putStrLn $ "  Total issues: " <> T.pack (show $ length diags)
  TIO.putStrLn $ "  Files with issues: " <> T.pack (show $ length $ uniqueFiles diags)

  -- By severity
  when (stBySeverity opts || not anyGrouping) $ do
    TIO.putStrLn $ "\n" <> colorBold "By Severity:"
    let severityCounts = countBy diagSeverity diags
    mapM_ (printSeverityRow $ length diags) $ sortBy (comparing (Down . snd)) $ Map.toList severityCounts

  -- By rule
  when (stByRule opts || not anyGrouping) $ do
    TIO.putStrLn $ "\n" <> colorBold "By Rule (top 20):"
    let ruleCounts = countBy getDiagRule diags
        topRules = take 20 $ sortBy (comparing (Down . snd)) $ Map.toList ruleCounts
    mapM_ (printRuleRow $ length diags) topRules

  -- By file
  when (stByFile opts) $ do
    TIO.putStrLn $ "\n" <> colorBold "By File (top 20):"
    let fileCounts = countBy getDiagPath diags
        topFiles = take 20 $ sortBy (comparing (Down . snd)) $ Map.toList fileCounts
    mapM_ (printFileRow $ length diags) topFiles

  -- Print bar chart legend
  TIO.putStrLn $ "\n" <> "Legend: " <> colorRed "█" <> " = 5 issues"

  where
    anyGrouping = stByRule opts || stByFile opts || stBySeverity opts

-- | Output statistics in JSON format
outputJsonStats :: StatsOptions -> [Diagnostic] -> IO ()
outputJsonStats _ diags = do
  let severityCounts = countBy diagSeverity diags
      ruleCounts = countBy getDiagRule diags
      fileCounts = countBy getDiagPath diags

  TIO.putStrLn "{"
  TIO.putStrLn $ "  \"total\": " <> T.pack (show $ length diags) <> ","
  TIO.putStrLn $ "  \"filesWithIssues\": " <> T.pack (show $ length $ uniqueFiles diags) <> ","
  TIO.putStrLn "  \"bySeverity\": {"
  printJsonMap severityCounts severityToText
  TIO.putStrLn "  },"
  TIO.putStrLn "  \"byRule\": {"
  printJsonMapText ruleCounts
  TIO.putStrLn "  },"
  TIO.putStrLn "  \"byFile\": {"
  printJsonMapText fileCounts
  TIO.putStrLn "  }"
  TIO.putStrLn "}"

-- | Count items by a key function
countBy :: Ord k => (a -> k) -> [a] -> Map k Int
countBy f = foldr (\x m -> Map.insertWith (+) (f x) 1 m) Map.empty

-- | Get unique files from diagnostics
uniqueFiles :: [Diagnostic] -> [Text]
uniqueFiles = Map.keys . countBy getDiagPath

-- | Print severity row with bar chart
printSeverityRow :: Int -> (Severity, Int) -> IO ()
printSeverityRow total (sev, count) = do
  let pct = (fromIntegral count / fromIntegral total * 100) :: Double
      bar = T.replicate (count `div` 5) "█"
      sevText = severityToText sev
      coloredBar = case sev of
        Error -> colorRed bar
        Warning -> colorYellow bar
        Info -> colorBlue bar
        Suggestion -> colorGreen bar
  TIO.putStrLn $ "  " <> padRight 12 sevText <> " " <>
    padLeft 5 (T.pack $ show count) <> " (" <> padLeft 5 (T.pack $ show (round pct :: Int) <> "%") <> ") " <> coloredBar

-- | Print rule row with bar chart
printRuleRow :: Int -> (Text, Int) -> IO ()
printRuleRow total (rule, count) = do
  let pct = (fromIntegral count / fromIntegral total * 100) :: Double
      bar = T.replicate (count `div` 5) "█"
  TIO.putStrLn $ "  " <> padRight 40 (T.take 38 rule) <> " " <>
    padLeft 5 (T.pack $ show count) <> " (" <> padLeft 5 (T.pack $ show (round pct :: Int) <> "%") <> ") " <> colorBlue bar

-- | Print file row with bar chart
printFileRow :: Int -> (Text, Int) -> IO ()
printFileRow total (file, count) = do
  let pct = (fromIntegral count / fromIntegral total * 100) :: Double
      bar = T.replicate (count `div` 5) "█"
      shortFile = if T.length file > 45 then "..." <> T.takeEnd 42 file else file
  TIO.putStrLn $ "  " <> padRight 48 shortFile <> " " <>
    padLeft 5 (T.pack $ show count) <> " (" <> padLeft 5 (T.pack $ show (round pct :: Int) <> "%") <> ") " <> colorYellow bar

-- | Print JSON map
printJsonMap :: Map k Int -> (k -> Text) -> IO ()
printJsonMap m toText = do
  let items = Map.toList m
  mapM_ (printJsonItem (length items)) (zip [1..] items)
  where
    printJsonItem total (i, (k, v)) = do
      let comma = if i < total then "," else ""
      TIO.putStrLn $ "    \"" <> toText k <> "\": " <> T.pack (show v) <> comma

-- | Print JSON map with text keys
printJsonMapText :: Map Text Int -> IO ()
printJsonMapText m = do
  let items = Map.toList m
  mapM_ (printJsonItem (length items)) (zip [1..] items)
  where
    printJsonItem total (i, (k, v)) = do
      let comma = if i < total then "," else ""
      TIO.putStrLn $ "    \"" <> k <> "\": " <> T.pack (show v) <> comma

-- | Convert severity to text
severityToText :: Severity -> Text
severityToText Error = "Error"
severityToText Warning = "Warning"
severityToText Info = "Info"
severityToText Suggestion = "Suggestion"

-- | Pad text to the right
padRight :: Int -> Text -> Text
padRight n t = t <> T.replicate (max 0 (n - T.length t)) " "

-- | Pad text to the left
padLeft :: Int -> Text -> Text
padLeft n t = T.replicate (max 0 (n - T.length t)) " " <> t

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
