{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Analysis.ProfilingAggregation
-- Description : Rule profiling aggregation, visualization data, and trend analysis
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides advanced profiling aggregation capabilities including:
--
-- * Historical profile storage and comparison
-- * Heatmap data generation (file × rule timing matrix)
-- * Trend analysis across multiple runs
-- * Performance regression detection
-- * Cross-session aggregation
-- * Visualization data formats (SVG heatmaps, charts)
--
-- == Usage
--
-- @
-- -- Create a profiling session
-- session <- newProfilingSession "my-project"
--
-- -- Record profiling data during analysis
-- recordProfilingData session tracker
--
-- -- Save session for historical comparison
-- saveProfilingSession session "profiles/run-001.json"
--
-- -- Load historical sessions and compare
-- sessions <- loadProfilingSessions "profiles/"
-- trend <- analyzeTrend sessions
--
-- -- Generate heatmap
-- heatmap <- generateHeatmap tracker
-- writeHeatmapSVG heatmap "heatmap.svg"
-- @
module Argus.Analysis.ProfilingAggregation
  ( -- * Configuration
    ProfilingConfig (..)
  , defaultProfilingConfig

    -- * Profiling Session
  , ProfilingSession (..)
  , SessionMetadata (..)
  , newProfilingSession
  , recordProfilingData
  , finalizeSession

    -- * Persistence
  , saveProfilingSession
  , loadProfilingSession
  , loadProfilingSessions

    -- * Heatmap Generation
  , Heatmap (..)
  , HeatmapCell (..)
  , HeatmapRow (..)
  , generateHeatmap
  , generateFileRuleHeatmap
  , generateCategoryHeatmap
  , writeHeatmapSVG
  , writeHeatmapHTML
  , writeHeatmapJSON

    -- * Trend Analysis
  , TrendAnalysis (..)
  , RuleTrend (..)
  , TrendDirection (..)
  , analyzeTrend
  , detectRegressions
  , compareSessions
  , SessionComparison (..)
  , RuleComparison (..)

    -- * Aggregation
  , AggregatedProfile (..)
  , aggregateProfiles
  , aggregateByCategory
  , aggregateByFile
  , aggregateByTimeWindow

    -- * Visualization Data
  , ChartData (..)
  , ChartSeries (..)
  , generateTimeSeriesChart
  , generateBarChart
  , generatePieChart
  , writeChartJSON

    -- * Performance Baselines
  , PerformanceBaseline (..)
  , createBaseline
  , checkAgainstBaseline
  , BaselineViolation (..)
  , saveBaseline
  , loadBaseline

    -- * Utilities
  , formatHeatmapValue
  , colorForValue
  , percentageChange
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, catMaybes)
import Data.Ord (comparing, Down(..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock
import Data.Time.Format (formatTime, defaultTimeLocale)
import GHC.Generics (Generic)
import System.Directory (listDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeExtension)
import Text.Printf (printf)

import Argus.Analysis.RuleTiming
  ( RuleTimingTracker, RuleTimingReport(..), RuleTimingStats(..)
  , TimingBreakdown(..), getRuleTimingReport, getRuleTimingStats
  )

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for profiling aggregation
data ProfilingConfig = ProfilingConfig
  { pcHistoryLimit      :: Int      -- ^ Max sessions to keep in history
  , pcRegressionPct     :: Double   -- ^ Percentage change to flag as regression
  , pcHeatmapMaxFiles   :: Int      -- ^ Max files to include in heatmap
  , pcHeatmapMaxRules   :: Int      -- ^ Max rules to include in heatmap
  , pcTrendWindowSize   :: Int      -- ^ Number of sessions for trend analysis
  , pcBaselineFile      :: FilePath -- ^ Default baseline file path
  , pcAutoSaveProfiles  :: Bool     -- ^ Auto-save profiles after analysis
  , pcProfileDirectory  :: FilePath -- ^ Directory for profile storage
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default profiling configuration
defaultProfilingConfig :: ProfilingConfig
defaultProfilingConfig = ProfilingConfig
  { pcHistoryLimit = 100
  , pcRegressionPct = 10.0
  , pcHeatmapMaxFiles = 50
  , pcHeatmapMaxRules = 100
  , pcTrendWindowSize = 10
  , pcBaselineFile = ".argus/baseline.json"
  , pcAutoSaveProfiles = True
  , pcProfileDirectory = ".argus/profiles"
  }

--------------------------------------------------------------------------------
-- Profiling Session
--------------------------------------------------------------------------------

-- | Metadata about a profiling session
data SessionMetadata = SessionMetadata
  { smSessionId     :: Text        -- ^ Unique session identifier
  , smProjectName   :: Text        -- ^ Project being analyzed
  , smStartTime     :: UTCTime     -- ^ When session started
  , smEndTime       :: Maybe UTCTime -- ^ When session ended
  , smGitCommit     :: Maybe Text  -- ^ Git commit hash if available
  , smBranch        :: Maybe Text  -- ^ Git branch if available
  , smFilesAnalyzed :: Int         -- ^ Number of files analyzed
  , smRulesEnabled  :: Int         -- ^ Number of rules enabled
  , smHostname      :: Maybe Text  -- ^ Machine hostname
  , smArgusVersion  :: Text        -- ^ Argus version
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | A complete profiling session
data ProfilingSession = ProfilingSession
  { psMetadata    :: SessionMetadata
  , psReport      :: RuleTimingReport
  , psFileTimings :: Map FilePath (Map Text Double)  -- ^ File -> Rule -> Time
  , psCategoryTotals :: Map Text Double              -- ^ Category -> Total time
  , psRuleMatchCounts :: Map Text Int                -- ^ Rule -> Match count
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Create a new profiling session
newProfilingSession :: Text -> IO ProfilingSession
newProfilingSession projectName = do
  now <- getCurrentTime
  let sessionId = T.pack $ formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now
      metadata = SessionMetadata
        { smSessionId = sessionId
        , smProjectName = projectName
        , smStartTime = now
        , smEndTime = Nothing
        , smGitCommit = Nothing
        , smBranch = Nothing
        , smFilesAnalyzed = 0
        , smRulesEnabled = 0
        , smHostname = Nothing
        , smArgusVersion = "1.0.0"
        }
      emptyReport = RuleTimingReport
        { rtrTotalTimeMs = 0
        , rtrTotalCalls = 0
        , rtrTotalMatches = 0
        , rtrRulesTracked = 0
        , rtrRuleStats = []
        , rtrCategoryBreakdown = []
        , rtrSlowestRules = []
        , rtrMostCalled = []
        , rtrGeneratedAt = now
        }
  pure ProfilingSession
    { psMetadata = metadata
    , psReport = emptyReport
    , psFileTimings = Map.empty
    , psCategoryTotals = Map.empty
    , psRuleMatchCounts = Map.empty
    }

-- | Record profiling data from a timing tracker
recordProfilingData :: ProfilingSession -> RuleTimingTracker -> IO ProfilingSession
recordProfilingData session tracker = do
  report <- getRuleTimingReport tracker
  stats <- getRuleTimingStats tracker

  let fileTimings = buildFileTimings stats
      categoryTotals = Map.fromList
        [ (tbCategory tb, tbTotalTimeMs tb)
        | tb <- rtrCategoryBreakdown report
        ]
      matchCounts = Map.fromList
        [ (rtsRuleId s, rtsMatchTotal s)
        | s <- stats
        ]
      updatedMeta = (psMetadata session)
        { smRulesEnabled = rtrRulesTracked report
        , smFilesAnalyzed = Map.size fileTimings
        }

  pure session
    { psMetadata = updatedMeta
    , psReport = report
    , psFileTimings = fileTimings
    , psCategoryTotals = categoryTotals
    , psRuleMatchCounts = matchCounts
    }
  where
    -- Build file -> rule -> timing map from stats
    buildFileTimings :: [RuleTimingStats] -> Map FilePath (Map Text Double)
    buildFileTimings stats =
      -- For now, we use total times per rule
      -- In a more complete implementation, we'd track per-file timings
      let ruleTimings = Map.fromList [(rtsRuleId s, rtsTotalTimeMs s) | s <- stats]
      in Map.singleton "<aggregate>" ruleTimings

-- | Finalize a session (set end time)
finalizeSession :: ProfilingSession -> IO ProfilingSession
finalizeSession session = do
  now <- getCurrentTime
  pure session
    { psMetadata = (psMetadata session) { smEndTime = Just now }
    }

--------------------------------------------------------------------------------
-- Persistence
--------------------------------------------------------------------------------

-- | Save a profiling session to disk
saveProfilingSession :: ProfilingSession -> FilePath -> IO ()
saveProfilingSession session path = do
  createDirectoryIfMissing True (takeDirectory path)
  LBS.writeFile path (AesonPretty.encodePretty session)
  where
    takeDirectory = reverse . dropWhile (/= '/') . reverse

-- | Load a profiling session from disk
loadProfilingSession :: FilePath -> IO (Either Text ProfilingSession)
loadProfilingSession path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ "Profile file not found: " <> T.pack path
    else do
      contents <- LBS.readFile path
      pure $ case Aeson.eitherDecode contents of
        Left err -> Left $ "Failed to parse profile: " <> T.pack err
        Right session -> Right session

-- | Load all profiling sessions from a directory
loadProfilingSessions :: FilePath -> IO [ProfilingSession]
loadProfilingSessions dir = do
  files <- listDirectory dir
  let jsonFiles = filter ((== ".json") . takeExtension) files
  results <- mapM (loadProfilingSession . (dir </>)) jsonFiles
  pure $ [s | Right s <- results]

--------------------------------------------------------------------------------
-- Heatmap Generation
--------------------------------------------------------------------------------

-- | A single cell in the heatmap
data HeatmapCell = HeatmapCell
  { hcValue       :: Double        -- ^ The timing value
  , hcNormalized  :: Double        -- ^ Normalized value (0-1)
  , hcLabel       :: Text          -- ^ Display label
  , hcColor       :: Text          -- ^ Color code (hex)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | A row in the heatmap
data HeatmapRow = HeatmapRow
  { hrLabel :: Text                -- ^ Row label (e.g., file name)
  , hrCells :: [HeatmapCell]       -- ^ Cells for each column
  , hrTotal :: Double              -- ^ Row total
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Complete heatmap data structure
data Heatmap = Heatmap
  { hmTitle       :: Text          -- ^ Heatmap title
  , hmDescription :: Text          -- ^ Description
  , hmColumnLabels :: [Text]       -- ^ Column headers (e.g., rule names)
  , hmRows        :: [HeatmapRow]  -- ^ Data rows
  , hmMinValue    :: Double        -- ^ Minimum value in heatmap
  , hmMaxValue    :: Double        -- ^ Maximum value in heatmap
  , hmTotalValue  :: Double        -- ^ Sum of all values
  , hmGeneratedAt :: UTCTime       -- ^ Generation timestamp
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Generate a heatmap from timing data
generateHeatmap :: RuleTimingTracker -> IO Heatmap
generateHeatmap tracker = do
  now <- getCurrentTime
  stats <- getRuleTimingStats tracker

  let ruleIds = take 50 $ map rtsRuleId $
                sortBy (comparing (Down . rtsTotalTimeMs)) stats
      values = [rtsTotalTimeMs s | s <- stats, rtsRuleId s `elem` ruleIds]
      minVal = if null values then 0 else minimum values
      maxVal = if null values then 0 else maximum values
      totalVal = sum values

      -- Create a single "aggregate" row
      cells = map (mkCell minVal maxVal) [(s, rtsTotalTimeMs s) | s <- stats, rtsRuleId s `elem` ruleIds]
      row = HeatmapRow
        { hrLabel = "All Files"
        , hrCells = cells
        , hrTotal = sum $ map hcValue cells
        }

  pure Heatmap
    { hmTitle = "Rule Execution Time Heatmap"
    , hmDescription = "Execution time distribution across rules"
    , hmColumnLabels = ruleIds
    , hmRows = [row]
    , hmMinValue = minVal
    , hmMaxValue = maxVal
    , hmTotalValue = totalVal
    , hmGeneratedAt = now
    }
  where
    mkCell minV maxV (_stat, val) =
      let normalized = if maxV > minV then (val - minV) / (maxV - minV) else 0
      in HeatmapCell
        { hcValue = val
        , hcNormalized = normalized
        , hcLabel = formatHeatmapValue val
        , hcColor = colorForValue normalized
        }

-- | Generate file × rule heatmap
generateFileRuleHeatmap :: ProfilingSession -> Int -> Int -> Heatmap
generateFileRuleHeatmap session maxFiles maxRules =
  let fileTimings = psFileTimings session
      report = psReport session
      now = rtrGeneratedAt report

      -- Get top rules by time
      topRules = take maxRules $ map rtsRuleId $
                 sortBy (comparing (Down . rtsTotalTimeMs)) (rtrRuleStats report)

      -- Get files with most activity
      fileList = take maxFiles $ Map.keys fileTimings

      -- Build rows
      rows = map (mkRow topRules fileTimings) fileList

      allValues = concatMap (map hcValue . hrCells) rows
      minVal = if null allValues then 0 else minimum allValues
      maxVal = if null allValues then 0 else maximum allValues

  in Heatmap
    { hmTitle = "File × Rule Timing Heatmap"
    , hmDescription = "Execution time breakdown by file and rule"
    , hmColumnLabels = topRules
    , hmRows = rows
    , hmMinValue = minVal
    , hmMaxValue = maxVal
    , hmTotalValue = sum allValues
    , hmGeneratedAt = now
    }
  where
    mkRow rules timings path =
      let pathTimings = Map.findWithDefault Map.empty path timings
          cells = map (mkCell pathTimings) rules
      in HeatmapRow
        { hrLabel = T.pack $ shortPath path
        , hrCells = cells
        , hrTotal = sum $ map hcValue cells
        }

    mkCell pathTimings ruleId =
      let val = Map.findWithDefault 0 ruleId pathTimings
      in HeatmapCell
        { hcValue = val
        , hcNormalized = 0  -- Will be normalized later
        , hcLabel = formatHeatmapValue val
        , hcColor = "#ffffff"
        }

    shortPath p = let parts = T.splitOn "/" (T.pack p)
                  in T.unpack $ T.intercalate "/" $ reverse $ take 2 $ reverse parts

-- | Generate category-based heatmap
generateCategoryHeatmap :: ProfilingSession -> Heatmap
generateCategoryHeatmap session =
  let report = psReport session
      now = rtrGeneratedAt report
      categories = rtrCategoryBreakdown report

      allValues = map tbTotalTimeMs categories
      minVal = if null allValues then 0 else minimum allValues
      maxVal = if null allValues then 0 else maximum allValues

      cells = map (mkCategoryCell minVal maxVal) categories
      row = HeatmapRow
        { hrLabel = "Categories"
        , hrCells = cells
        , hrTotal = sum allValues
        }

  in Heatmap
    { hmTitle = "Category Timing Heatmap"
    , hmDescription = "Execution time by rule category"
    , hmColumnLabels = map tbCategory categories
    , hmRows = [row]
    , hmMinValue = minVal
    , hmMaxValue = maxVal
    , hmTotalValue = sum allValues
    , hmGeneratedAt = now
    }
  where
    mkCategoryCell minV maxV tb =
      let val = tbTotalTimeMs tb
          normalized = if maxV > minV then (val - minV) / (maxV - minV) else 0
      in HeatmapCell
        { hcValue = val
        , hcNormalized = normalized
        , hcLabel = formatHeatmapValue val <> " (" <> T.pack (printf "%.1f%%" (tbPercentage tb)) <> ")"
        , hcColor = colorForValue normalized
        }

-- | Write heatmap as SVG
writeHeatmapSVG :: Heatmap -> FilePath -> IO ()
writeHeatmapSVG hm path = do
  let svg = generateHeatmapSVG hm
  TIO.writeFile path svg

-- | Write heatmap as interactive HTML
writeHeatmapHTML :: Heatmap -> FilePath -> IO ()
writeHeatmapHTML hm path = do
  let html = generateHeatmapHTML hm
  TIO.writeFile path html

-- | Write heatmap as JSON for custom visualization
writeHeatmapJSON :: Heatmap -> FilePath -> IO ()
writeHeatmapJSON hm path =
  LBS.writeFile path (AesonPretty.encodePretty hm)

-- | Generate SVG for heatmap
generateHeatmapSVG :: Heatmap -> Text
generateHeatmapSVG Heatmap{..} =
  let numCols = length hmColumnLabels
      numRows = length hmRows
      cellWidth = 60 :: Int
      cellHeight = 30 :: Int
      labelWidth = 150 :: Int
      headerHeight = 100 :: Int
      svgWidth = labelWidth + numCols * cellWidth + 50
      svgHeight = headerHeight + numRows * cellHeight + 50

      header = T.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" <> T.pack (show svgWidth) <>
          "\" height=\"" <> T.pack (show svgHeight) <> "\">"
        , "<style>"
        , "  .cell { stroke: #ccc; stroke-width: 1; }"
        , "  .label { font-family: monospace; font-size: 10px; }"
        , "  .header { font-family: monospace; font-size: 10px; writing-mode: tb-rl; }"
        , "  .title { font-family: sans-serif; font-size: 14px; font-weight: bold; }"
        , "</style>"
        , "<text x=\"10\" y=\"20\" class=\"title\">" <> escapeXml hmTitle <> "</text>"
        ]

      -- Column headers (rotated)
      colHeaders = T.unlines
        [ "<text x=\"" <> T.pack (show (labelWidth + i * cellWidth + cellWidth `div` 2)) <>
          "\" y=\"" <> T.pack (show (headerHeight - 5)) <>
          "\" class=\"header\" transform=\"rotate(-45 " <>
          T.pack (show (labelWidth + i * cellWidth + cellWidth `div` 2)) <> "," <>
          T.pack (show (headerHeight - 5)) <> ")\">" <>
          escapeXml (T.take 15 col) <> "</text>"
        | (i, col) <- zip [0..] hmColumnLabels
        ]

      -- Data rows
      dataRows = T.unlines
        [ generateSVGRow labelWidth cellWidth cellHeight headerHeight rowIdx row
        | (rowIdx, row) <- zip [0..] hmRows
        ]

      footer = "</svg>"

  in header <> colHeaders <> dataRows <> footer

-- | Generate SVG for a single row
generateSVGRow :: Int -> Int -> Int -> Int -> Int -> HeatmapRow -> Text
generateSVGRow labelWidth cellWidth cellHeight headerHeight rowIdx HeatmapRow{..} =
  let y = headerHeight + rowIdx * cellHeight
      labelElem = "<text x=\"5\" y=\"" <> T.pack (show (y + cellHeight `div` 2 + 3)) <>
                  "\" class=\"label\">" <> escapeXml (T.take 20 hrLabel) <> "</text>"
      cellElems = T.unlines
        [ "<rect x=\"" <> T.pack (show (labelWidth + colIdx * cellWidth)) <>
          "\" y=\"" <> T.pack (show y) <>
          "\" width=\"" <> T.pack (show cellWidth) <>
          "\" height=\"" <> T.pack (show cellHeight) <>
          "\" fill=\"" <> hcColor cell <> "\" class=\"cell\"/>" <>
          "<text x=\"" <> T.pack (show (labelWidth + colIdx * cellWidth + cellWidth `div` 2)) <>
          "\" y=\"" <> T.pack (show (y + cellHeight `div` 2 + 3)) <>
          "\" text-anchor=\"middle\" class=\"label\">" <>
          (if hcValue cell > 0.01 then formatHeatmapValue (hcValue cell) else "") <> "</text>"
        | (colIdx, cell) <- zip [0..] hrCells
        ]
  in labelElem <> cellElems

-- | Generate interactive HTML for heatmap
generateHeatmapHTML :: Heatmap -> Text
generateHeatmapHTML hm = T.unlines
  [ "<!DOCTYPE html>"
  , "<html><head>"
  , "<title>" <> escapeXml (hmTitle hm) <> "</title>"
  , "<style>"
  , "body { font-family: sans-serif; margin: 20px; }"
  , "table { border-collapse: collapse; }"
  , "th, td { border: 1px solid #ccc; padding: 4px 8px; text-align: center; }"
  , "th { background: #f5f5f5; font-size: 11px; }"
  , ".cell { min-width: 50px; font-size: 10px; }"
  , ".row-label { text-align: left; font-weight: bold; }"
  , ".tooltip { position: relative; cursor: pointer; }"
  , ".tooltip:hover::after { content: attr(data-tooltip); position: absolute;"
  , "  background: #333; color: white; padding: 4px 8px; border-radius: 4px;"
  , "  white-space: nowrap; z-index: 1; top: 100%; left: 50%; transform: translateX(-50%); }"
  , "</style></head><body>"
  , "<h1>" <> escapeXml (hmTitle hm) <> "</h1>"
  , "<p>" <> escapeXml (hmDescription hm) <> "</p>"
  , "<table>"
  , "<tr><th></th>" <> T.concat ["<th>" <> escapeXml (T.take 15 c) <> "</th>" | c <- hmColumnLabels hm] <> "</tr>"
  , T.concat [generateHTMLRow row | row <- hmRows hm]
  , "</table>"
  , "<p><small>Generated at " <> T.pack (show (hmGeneratedAt hm)) <> "</small></p>"
  , "</body></html>"
  ]

-- | Generate HTML for a row
generateHTMLRow :: HeatmapRow -> Text
generateHTMLRow HeatmapRow{..} =
  "<tr><td class=\"row-label\">" <> escapeXml hrLabel <> "</td>" <>
  T.concat
    [ "<td class=\"cell tooltip\" style=\"background-color: " <> hcColor c <>
      "\" data-tooltip=\"" <> hcLabel c <> "\">" <>
      (if hcValue c > 0.01 then formatHeatmapValue (hcValue c) else "-") <> "</td>"
    | c <- hrCells
    ] <>
  "</tr>"

--------------------------------------------------------------------------------
-- Trend Analysis
--------------------------------------------------------------------------------

-- | Direction of a trend
data TrendDirection
  = TrendImproving    -- ^ Getting faster
  | TrendStable       -- ^ Within acceptable variation
  | TrendDegrading    -- ^ Getting slower
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Trend information for a single rule
data RuleTrend = RuleTrend
  { rtRuleId          :: Text
  , rtDirection       :: TrendDirection
  , rtChangePercent   :: Double        -- ^ Percentage change (positive = slower)
  , rtRecentMean      :: Double        -- ^ Recent average time
  , rtHistoricalMean  :: Double        -- ^ Historical average time
  , rtVariance        :: Double        -- ^ Variance in timing
  , rtDataPoints      :: Int           -- ^ Number of data points
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Complete trend analysis
data TrendAnalysis = TrendAnalysis
  { taOverallDirection   :: TrendDirection
  , taOverallChange      :: Double
  , taTotalTimeRecent    :: Double
  , taTotalTimeHistorical :: Double
  , taRuleTrends         :: [RuleTrend]
  , taRegressions        :: [RuleTrend]
  , taImprovements       :: [RuleTrend]
  , taSessionsAnalyzed   :: Int
  , taTimespan           :: (UTCTime, UTCTime)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Analyze trends across profiling sessions
analyzeTrend :: [ProfilingSession] -> TrendAnalysis
analyzeTrend sessions
  | length sessions < 2 = emptyTrend
  | otherwise =
    let sorted = sortBy (comparing (smStartTime . psMetadata)) sessions
        (recent, historical) = splitAt (length sorted `div` 2) sorted

        recentTotals = map (rtrTotalTimeMs . psReport) recent
        histTotals = map (rtrTotalTimeMs . psReport) historical

        recentMean = avg recentTotals
        histMean = avg histTotals
        overallChange = percentageChange histMean recentMean

        -- Analyze per-rule trends
        allRules = Set.toList $ Set.unions
          [ Set.fromList $ map rtsRuleId (rtrRuleStats $ psReport s)
          | s <- sessions
          ]
        ruleTrends = map (analyzeRuleTrend recent historical) allRules
        regressions = filter ((== TrendDegrading) . rtDirection) ruleTrends
        improvements = filter ((== TrendImproving) . rtDirection) ruleTrends

        direction
          | overallChange > 10 = TrendDegrading
          | overallChange < -10 = TrendImproving
          | otherwise = TrendStable

        times = map (smStartTime . psMetadata) sorted
        timespan = (minimum times, maximum times)

    in TrendAnalysis
      { taOverallDirection = direction
      , taOverallChange = overallChange
      , taTotalTimeRecent = recentMean
      , taTotalTimeHistorical = histMean
      , taRuleTrends = sortBy (comparing (Down . abs . rtChangePercent)) ruleTrends
      , taRegressions = sortBy (comparing (Down . rtChangePercent)) regressions
      , taImprovements = sortBy (comparing rtChangePercent) improvements
      , taSessionsAnalyzed = length sessions
      , taTimespan = timespan
      }
  where
    avg [] = 0
    avg xs = sum xs / fromIntegral (length xs)

    emptyTrend = TrendAnalysis
      { taOverallDirection = TrendStable
      , taOverallChange = 0
      , taTotalTimeRecent = 0
      , taTotalTimeHistorical = 0
      , taRuleTrends = []
      , taRegressions = []
      , taImprovements = []
      , taSessionsAnalyzed = 0
      , taTimespan = (read "2000-01-01 00:00:00 UTC", read "2000-01-01 00:00:00 UTC")
      }

-- | Analyze trend for a specific rule
analyzeRuleTrend :: [ProfilingSession] -> [ProfilingSession] -> Text -> RuleTrend
analyzeRuleTrend recentSessions histSessions ruleId =
  let recentTimes = mapMaybe (getRuleTiming ruleId) recentSessions
      histTimes = mapMaybe (getRuleTiming ruleId) histSessions

      recentMean = if null recentTimes then 0 else sum recentTimes / fromIntegral (length recentTimes)
      histMean = if null histTimes then 0 else sum histTimes / fromIntegral (length histTimes)
      change = percentageChange histMean recentMean

      allTimes = recentTimes ++ histTimes
      variance = if length allTimes > 1
                 then let m = sum allTimes / fromIntegral (length allTimes)
                      in sum [(t - m)^(2::Int) | t <- allTimes] / fromIntegral (length allTimes - 1)
                 else 0

      direction
        | change > 10 = TrendDegrading
        | change < -10 = TrendImproving
        | otherwise = TrendStable

  in RuleTrend
    { rtRuleId = ruleId
    , rtDirection = direction
    , rtChangePercent = change
    , rtRecentMean = recentMean
    , rtHistoricalMean = histMean
    , rtVariance = variance
    , rtDataPoints = length allTimes
    }
  where
    getRuleTiming rid session =
      let stats = rtrRuleStats $ psReport session
      in case filter ((== rid) . rtsRuleId) stats of
        (s:_) -> Just (rtsTotalTimeMs s)
        [] -> Nothing

-- | Detect performance regressions
detectRegressions :: Double -> [ProfilingSession] -> [RuleTrend]
detectRegressions threshold sessions =
  let trend = analyzeTrend sessions
  in filter (\rt -> rtChangePercent rt > threshold) (taRuleTrends trend)

-- | Comparison between two sessions
data SessionComparison = SessionComparison
  { scBefore       :: SessionMetadata
  , scAfter        :: SessionMetadata
  , scTotalChange  :: Double
  , scRuleChanges  :: [RuleComparison]
  , scNewRules     :: [Text]
  , scRemovedRules :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Comparison for a single rule
data RuleComparison = RuleComparison
  { rcRuleId      :: Text
  , rcTimeBefore  :: Double
  , rcTimeAfter   :: Double
  , rcChange      :: Double
  , rcChangeDir   :: TrendDirection
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Compare two profiling sessions
compareSessions :: ProfilingSession -> ProfilingSession -> SessionComparison
compareSessions before after =
  let beforeStats = Map.fromList [(rtsRuleId s, s) | s <- rtrRuleStats (psReport before)]
      afterStats = Map.fromList [(rtsRuleId s, s) | s <- rtrRuleStats (psReport after)]

      commonRules = Set.intersection (Map.keysSet beforeStats) (Map.keysSet afterStats)
      newRules = Set.toList $ Map.keysSet afterStats `Set.difference` Map.keysSet beforeStats
      removedRules = Set.toList $ Map.keysSet beforeStats `Set.difference` Map.keysSet afterStats

      ruleChanges = mapMaybe (compareRule beforeStats afterStats) (Set.toList commonRules)

      totalBefore = rtrTotalTimeMs (psReport before)
      totalAfter = rtrTotalTimeMs (psReport after)
      totalChange = percentageChange totalBefore totalAfter

  in SessionComparison
    { scBefore = psMetadata before
    , scAfter = psMetadata after
    , scTotalChange = totalChange
    , scRuleChanges = sortBy (comparing (Down . abs . rcChange)) ruleChanges
    , scNewRules = newRules
    , scRemovedRules = removedRules
    }
  where
    compareRule bStats aStats ruleId = do
      bStat <- Map.lookup ruleId bStats
      aStat <- Map.lookup ruleId aStats
      let timeBefore = rtsTotalTimeMs bStat
          timeAfter = rtsTotalTimeMs aStat
          change = percentageChange timeBefore timeAfter
          dir | change > 10 = TrendDegrading
              | change < -10 = TrendImproving
              | otherwise = TrendStable
      pure RuleComparison
        { rcRuleId = ruleId
        , rcTimeBefore = timeBefore
        , rcTimeAfter = timeAfter
        , rcChange = change
        , rcChangeDir = dir
        }

--------------------------------------------------------------------------------
-- Aggregation
--------------------------------------------------------------------------------

-- | Aggregated profile across multiple sessions
data AggregatedProfile = AggregatedProfile
  { apSessionCount     :: Int
  , apTotalTimeMs      :: Double
  , apMeanTimeMs       :: Double
  , apMinTimeMs        :: Double
  , apMaxTimeMs        :: Double
  , apRuleAggregates   :: Map Text RuleAggregate
  , apCategoryTotals   :: Map Text Double
  , apTimeRange        :: (UTCTime, UTCTime)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Aggregate data for a single rule
data RuleAggregate = RuleAggregate
  { raMeanMs       :: Double
  , raMinMs        :: Double
  , raMaxMs        :: Double
  , raTotalMs      :: Double
  , raCallCount    :: Int
  , raMatchCount   :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Aggregate multiple profiles
aggregateProfiles :: [ProfilingSession] -> AggregatedProfile
aggregateProfiles [] = AggregatedProfile
  { apSessionCount = 0
  , apTotalTimeMs = 0
  , apMeanTimeMs = 0
  , apMinTimeMs = 0
  , apMaxTimeMs = 0
  , apRuleAggregates = Map.empty
  , apCategoryTotals = Map.empty
  , apTimeRange = (read "2000-01-01 00:00:00 UTC", read "2000-01-01 00:00:00 UTC")
  }
aggregateProfiles sessions =
  let totalTimes = map (rtrTotalTimeMs . psReport) sessions
      totalTime = sum totalTimes
      meanTime = totalTime / fromIntegral (length sessions)

      -- Aggregate rule stats
      allRuleStats = concatMap (rtrRuleStats . psReport) sessions
      grouped = groupByKey rtsRuleId allRuleStats
      ruleAggs = Map.map aggregateRuleStats grouped

      -- Aggregate categories
      allCategories = concatMap (rtrCategoryBreakdown . psReport) sessions
      catGroups = groupByKey tbCategory allCategories
      catTotals = Map.map (sum . map tbTotalTimeMs) catGroups

      times = map (smStartTime . psMetadata) sessions
      timeRange = (minimum times, maximum times)

  in AggregatedProfile
    { apSessionCount = length sessions
    , apTotalTimeMs = totalTime
    , apMeanTimeMs = meanTime
    , apMinTimeMs = if null totalTimes then 0 else minimum totalTimes
    , apMaxTimeMs = if null totalTimes then 0 else maximum totalTimes
    , apRuleAggregates = ruleAggs
    , apCategoryTotals = catTotals
    , apTimeRange = timeRange
    }
  where
    aggregateRuleStats :: [RuleTimingStats] -> RuleAggregate
    aggregateRuleStats stats =
      let times = map rtsTotalTimeMs stats
          calls = sum $ map rtsCallCount stats
          matches = sum $ map rtsMatchTotal stats
      in RuleAggregate
        { raMeanMs = if null times then 0 else sum times / fromIntegral (length times)
        , raMinMs = if null times then 0 else minimum times
        , raMaxMs = if null times then 0 else maximum times
        , raTotalMs = sum times
        , raCallCount = calls
        , raMatchCount = matches
        }

-- | Aggregate by category
aggregateByCategory :: [ProfilingSession] -> Map Text AggregatedProfile
aggregateByCategory sessions =
  let allStats = concatMap (rtrRuleStats . psReport) sessions
      byCategory = groupByKey rtsCategory allStats
  in Map.mapWithKey (\_ stats -> aggregateRuleStatsToProfile stats) byCategory
  where
    aggregateRuleStatsToProfile stats =
      let times = map rtsTotalTimeMs stats
      in AggregatedProfile
        { apSessionCount = length stats
        , apTotalTimeMs = sum times
        , apMeanTimeMs = if null times then 0 else sum times / fromIntegral (length times)
        , apMinTimeMs = if null times then 0 else minimum times
        , apMaxTimeMs = if null times then 0 else maximum times
        , apRuleAggregates = Map.empty
        , apCategoryTotals = Map.empty
        , apTimeRange = (read "2000-01-01 00:00:00 UTC", read "2000-01-01 00:00:00 UTC")
        }

-- | Aggregate by file
aggregateByFile :: [ProfilingSession] -> Map FilePath AggregatedProfile
aggregateByFile sessions =
  let allFileTimings = map psFileTimings sessions
      mergedTimings = foldl' (Map.unionWith (Map.unionWith (+))) Map.empty allFileTimings
  in Map.mapWithKey (\_ timings -> timingsToProfile timings) mergedTimings
  where
    timingsToProfile timings =
      let times = Map.elems timings
          total = sum times
      in AggregatedProfile
        { apSessionCount = 1
        , apTotalTimeMs = total
        , apMeanTimeMs = if null times then 0 else total / fromIntegral (length times)
        , apMinTimeMs = if null times then 0 else minimum times
        , apMaxTimeMs = if null times then 0 else maximum times
        , apRuleAggregates = Map.empty
        , apCategoryTotals = Map.empty
        , apTimeRange = (read "2000-01-01 00:00:00 UTC", read "2000-01-01 00:00:00 UTC")
        }

-- | Aggregate by time window (e.g., daily, weekly)
aggregateByTimeWindow :: NominalDiffTime -> [ProfilingSession] -> [(UTCTime, AggregatedProfile)]
aggregateByTimeWindow windowSize sessions =
  let sorted = sortBy (comparing (smStartTime . psMetadata)) sessions
      grouped = groupByWindow windowSize sorted
  in map (\(t, ss) -> (t, aggregateProfiles ss)) grouped
  where
    groupByWindow :: NominalDiffTime -> [ProfilingSession] -> [(UTCTime, [ProfilingSession])]
    groupByWindow _ [] = []
    groupByWindow ws (s:ss) =
      let startTime = smStartTime (psMetadata s)
          (inWindow, rest) = span (\x -> diffUTCTime (smStartTime (psMetadata x)) startTime < ws) (s:ss)
      in (startTime, inWindow) : groupByWindow ws rest

--------------------------------------------------------------------------------
-- Visualization Data
--------------------------------------------------------------------------------

-- | A series in a chart
data ChartSeries = ChartSeries
  { csName   :: Text
  , csColor  :: Text
  , csValues :: [Double]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Complete chart data
data ChartData = ChartData
  { cdTitle      :: Text
  , cdType       :: Text           -- ^ "line", "bar", "pie"
  , cdLabels     :: [Text]         -- ^ X-axis labels
  , cdSeries     :: [ChartSeries]
  , cdXAxisLabel :: Maybe Text
  , cdYAxisLabel :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Generate time series chart data
generateTimeSeriesChart :: Text -> [(UTCTime, Double)] -> ChartData
generateTimeSeriesChart title dataPoints =
  let (times, values) = unzip dataPoints
      labels = map (T.pack . formatTime defaultTimeLocale "%Y-%m-%d") times
      series = ChartSeries
        { csName = title
        , csColor = "#3498db"
        , csValues = values
        }
  in ChartData
    { cdTitle = title
    , cdType = "line"
    , cdLabels = labels
    , cdSeries = [series]
    , cdXAxisLabel = Just "Date"
    , cdYAxisLabel = Just "Time (ms)"
    }

-- | Generate bar chart data
generateBarChart :: Text -> [(Text, Double)] -> ChartData
generateBarChart title dataPoints =
  let (labels, values) = unzip dataPoints
      series = ChartSeries
        { csName = title
        , csColor = "#2ecc71"
        , csValues = values
        }
  in ChartData
    { cdTitle = title
    , cdType = "bar"
    , cdLabels = labels
    , cdSeries = [series]
    , cdXAxisLabel = Nothing
    , cdYAxisLabel = Just "Time (ms)"
    }

-- | Generate pie chart data
generatePieChart :: Text -> [(Text, Double)] -> ChartData
generatePieChart title dataPoints =
  let (labels, values) = unzip dataPoints
      colors = ["#3498db", "#2ecc71", "#e74c3c", "#f39c12", "#9b59b6",
                "#1abc9c", "#e67e22", "#34495e", "#95a5a6", "#d35400"]
      series = zipWith3 mkSeries labels values (cycle colors)
  in ChartData
    { cdTitle = title
    , cdType = "pie"
    , cdLabels = labels
    , cdSeries = series
    , cdXAxisLabel = Nothing
    , cdYAxisLabel = Nothing
    }
  where
    mkSeries name val color = ChartSeries
      { csName = name
      , csColor = color
      , csValues = [val]
      }

-- | Write chart data as JSON for visualization
writeChartJSON :: ChartData -> FilePath -> IO ()
writeChartJSON chart path =
  LBS.writeFile path (AesonPretty.encodePretty chart)

--------------------------------------------------------------------------------
-- Performance Baselines
--------------------------------------------------------------------------------

-- | A performance baseline for comparison
data PerformanceBaseline = PerformanceBaseline
  { pbCreatedAt     :: UTCTime
  , pbDescription   :: Text
  , pbTotalTimeMs   :: Double
  , pbRuleLimits    :: Map Text Double  -- ^ Max acceptable time per rule
  , pbCategoryLimits :: Map Text Double -- ^ Max acceptable time per category
  , pbP95Limit      :: Double           -- ^ Max acceptable P95 time
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | A baseline violation
data BaselineViolation = BaselineViolation
  { bvType        :: Text           -- ^ "rule", "category", "total", "p95"
  , bvName        :: Text           -- ^ What violated
  , bvLimit       :: Double         -- ^ The limit
  , bvActual      :: Double         -- ^ Actual value
  , bvExcessPct   :: Double         -- ^ How much over (percentage)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Create a baseline from a profiling session
createBaseline :: Text -> ProfilingSession -> PerformanceBaseline
createBaseline description session =
  let report = psReport session
      stats = rtrRuleStats report
      ruleLimits = Map.fromList
        [(rtsRuleId s, rtsTotalTimeMs s * 1.2) | s <- stats]  -- 20% headroom
      catLimits = Map.fromList
        [(tbCategory tb, tbTotalTimeMs tb * 1.2) | tb <- rtrCategoryBreakdown report]
      p95 = if null stats then 0 else maximum $ map rtsP95Ms stats
  in PerformanceBaseline
    { pbCreatedAt = rtrGeneratedAt report
    , pbDescription = description
    , pbTotalTimeMs = rtrTotalTimeMs report * 1.2
    , pbRuleLimits = ruleLimits
    , pbCategoryLimits = catLimits
    , pbP95Limit = p95 * 1.2
    }

-- | Check a session against a baseline
checkAgainstBaseline :: PerformanceBaseline -> ProfilingSession -> [BaselineViolation]
checkAgainstBaseline baseline session =
  let report = psReport session
      stats = rtrRuleStats report

      -- Check total time
      totalViolation = if rtrTotalTimeMs report > pbTotalTimeMs baseline
        then [BaselineViolation
          { bvType = "total"
          , bvName = "Total analysis time"
          , bvLimit = pbTotalTimeMs baseline
          , bvActual = rtrTotalTimeMs report
          , bvExcessPct = percentageChange (pbTotalTimeMs baseline) (rtrTotalTimeMs report)
          }]
        else []

      -- Check per-rule limits
      ruleViolations = catMaybes
        [ checkRuleLimit (pbRuleLimits baseline) s | s <- stats ]

      -- Check category limits
      catViolations = catMaybes
        [ checkCategoryLimit (pbCategoryLimits baseline) tb
        | tb <- rtrCategoryBreakdown report
        ]

      -- Check P95
      p95Actual = if null stats then 0 else maximum $ map rtsP95Ms stats
      p95Violation = if p95Actual > pbP95Limit baseline
        then [BaselineViolation
          { bvType = "p95"
          , bvName = "P95 rule execution time"
          , bvLimit = pbP95Limit baseline
          , bvActual = p95Actual
          , bvExcessPct = percentageChange (pbP95Limit baseline) p95Actual
          }]
        else []

  in totalViolation ++ ruleViolations ++ catViolations ++ p95Violation
  where
    checkRuleLimit limits stat =
      case Map.lookup (rtsRuleId stat) limits of
        Nothing -> Nothing
        Just limit
          | rtsTotalTimeMs stat > limit -> Just BaselineViolation
              { bvType = "rule"
              , bvName = rtsRuleId stat
              , bvLimit = limit
              , bvActual = rtsTotalTimeMs stat
              , bvExcessPct = percentageChange limit (rtsTotalTimeMs stat)
              }
          | otherwise -> Nothing

    checkCategoryLimit limits tb =
      case Map.lookup (tbCategory tb) limits of
        Nothing -> Nothing
        Just limit
          | tbTotalTimeMs tb > limit -> Just BaselineViolation
              { bvType = "category"
              , bvName = tbCategory tb
              , bvLimit = limit
              , bvActual = tbTotalTimeMs tb
              , bvExcessPct = percentageChange limit (tbTotalTimeMs tb)
              }
          | otherwise -> Nothing

-- | Save baseline to file
saveBaseline :: PerformanceBaseline -> FilePath -> IO ()
saveBaseline baseline path = do
  createDirectoryIfMissing True (takeDirectory path)
  LBS.writeFile path (AesonPretty.encodePretty baseline)
  where
    takeDirectory = reverse . dropWhile (/= '/') . reverse

-- | Load baseline from file
loadBaseline :: FilePath -> IO (Either Text PerformanceBaseline)
loadBaseline path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ "Baseline file not found: " <> T.pack path
    else do
      contents <- LBS.readFile path
      pure $ case Aeson.eitherDecode contents of
        Left err -> Left $ "Failed to parse baseline: " <> T.pack err
        Right baseline -> Right baseline

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- | Format a heatmap value for display
formatHeatmapValue :: Double -> Text
formatHeatmapValue ms
  | ms < 0.01  = ""
  | ms < 1     = T.pack $ printf "%.1fµs" (ms * 1000)
  | ms < 1000  = T.pack $ printf "%.1fms" ms
  | otherwise  = T.pack $ printf "%.1fs" (ms / 1000)

-- | Get color for a normalized value (0-1)
colorForValue :: Double -> Text
colorForValue v
  | v < 0.2   = "#e8f5e9"  -- Light green
  | v < 0.4   = "#c8e6c9"  -- Green
  | v < 0.6   = "#fff9c4"  -- Yellow
  | v < 0.8   = "#ffcc80"  -- Orange
  | otherwise = "#ef9a9a"  -- Red

-- | Calculate percentage change between two values
percentageChange :: Double -> Double -> Double
percentageChange old new
  | old == 0  = if new == 0 then 0 else 100
  | otherwise = ((new - old) / old) * 100

-- | Group list by key
groupByKey :: Ord k => (a -> k) -> [a] -> Map k [a]
groupByKey f = foldl' (\m x -> Map.insertWith (++) (f x) [x] m) Map.empty

-- | Escape XML special characters
escapeXml :: Text -> Text
escapeXml = T.replace "<" "&lt;"
          . T.replace ">" "&gt;"
          . T.replace "&" "&amp;"
          . T.replace "\"" "&quot;"
          . T.replace "'" "&apos;"
