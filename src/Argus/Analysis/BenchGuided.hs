{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Analysis.BenchGuided
-- Description : Benchmark-guided rule suggestions and hot-spot correlation
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides benchmark-guided analysis that correlates profiling
-- and benchmark data with Argus diagnostics to prioritize performance-critical
-- issues and suggest optimizations based on empirical performance data.
--
-- == Features
--
-- * Parse Criterion JSON benchmark results
-- * Parse GHC profiling output (both text and JSON formats)
-- * Identify performance hot spots in code
-- * Correlate hot spots with Argus diagnostics
-- * Prioritize diagnostics based on hot-spot proximity
-- * Adjust severity levels for performance issues in hot code
-- * Generate benchmark-guided reports
--
-- == Workflow
--
-- @
-- -- 1. Parse benchmark results
-- benchData <- parseCriterionJson benchJsonBytes
--
-- -- 2. Parse profiling data
-- profData <- parseGHCProfile profOutput
--
-- -- 3. Identify hot spots
-- let hotSpots = identifyHotSpots profData
--
-- -- 4. Correlate with diagnostics
-- let suggestions = correlateHotSpots hotSpots diagnostics
--
-- -- 5. Prioritize diagnostics
-- let prioritized = prioritizeDiagnostics diagnostics hotSpots
--
-- -- 6. Generate report
-- report <- benchGuidedReport suggestions
-- @
--
-- == Criterion JSON Format
--
-- Criterion outputs benchmark results in JSON format with the following structure:
--
-- @
-- [
--   {
--     "reportName": "parsing/parseModule/small (20 lines)",
--     "reportNumber": 1,
--     "reportAnalysis": {
--       "anMean": { "estPoint": 0.00012, ... },
--       "anStdDev": { "estPoint": 0.000015, ... }
--     },
--     "reportOutliers": { ... }
--   }
-- ]
-- @
module Argus.Analysis.BenchGuided
  ( -- * Types
    BenchmarkResult (..)
  , HotSpot (..)
  , ProfileData (..)
  , BenchGuidedSuggestion (..)
  , CostCenterEntry (..)
  , ProfileMetadata (..)
  , HotSpotWeight (..)

    -- * Benchmark Parsing
  , parseCriterionJson
  , parseCriterionReport
  , extractBenchmarkTimings

    -- * Profiling Parsing
  , parseGHCProfile
  , parseGHCProfileJson
  , parseGHCProfileText
  , parseCostCenterStack

    -- * Hot Spot Analysis
  , identifyHotSpots
  , calculateHotSpotWeight
  , filterHotSpots
  , topHotSpots

    -- * Correlation
  , correlateHotSpots
  , prioritizeDiagnostics
  , adjustSeverityForHotSpots
  , findDiagnosticsInHotSpot

    -- * Output
  , benchGuidedReport
  , benchGuidedDiagnostics
  , formatSuggestion
  , formatHotSpotSummary

    -- * Utilities
  , sourceLocationInSpan
  , functionNameFromPath
  , normalizeWeight
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..), FromJSON (..), (.:), (.:?), (.!=), withObject)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseMaybe, parseEither)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortBy, find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Ord (comparing, Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Argus.Types
  ( Diagnostic (..), Severity (..), DiagnosticKind (..)
  , SrcSpan (..), Line (..), Column (..)
  , Seconds (..)
  )

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

-- | Metadata about a profiling run
data ProfileMetadata = ProfileMetadata
  { pmCommand        :: Text           -- ^ Command that was profiled
  , pmTotalTime      :: Seconds        -- ^ Total execution time
  , pmTotalAlloc     :: Integer        -- ^ Total allocations (bytes)
  , pmTimestamp      :: Maybe Text     -- ^ When profile was generated
  , pmRtsOptions     :: [Text]         -- ^ RTS options used
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | A single cost center entry from GHC profiling
data CostCenterEntry = CostCenterEntry
  { cceId            :: Int            -- ^ Cost center ID
  , cceName          :: Text           -- ^ Cost center name (usually function name)
  , cceModule        :: Text           -- ^ Module name
  , cceSrcLoc        :: Maybe Text     -- ^ Source location (file:line:col)
  , cceTimePercent   :: Double         -- ^ Percentage of total time
  , cceAllocPercent  :: Double         -- ^ Percentage of total allocations
  , cceTimeIndiv     :: Double         -- ^ Individual time (seconds)
  , cceAllocIndiv    :: Integer        -- ^ Individual allocations (bytes)
  , cceEntries       :: Integer        -- ^ Number of entries to this cost center
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Complete profiling data from GHC
data ProfileData = ProfileData
  { pdMetadata       :: ProfileMetadata
  , pdCostCenters    :: [CostCenterEntry]
  , pdTotalCostCenters :: Int          -- ^ Number of cost centers
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | A benchmark result from Criterion
data BenchmarkResult = BenchmarkResult
  { brName           :: Text           -- ^ Benchmark name
  , brMeanTime       :: Seconds        -- ^ Mean execution time
  , brStdDev         :: Seconds        -- ^ Standard deviation
  , brAllocations    :: Maybe Integer  -- ^ Total allocations (if available)
  , brRuns           :: Maybe Int      -- ^ Number of runs
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Weight/importance of a hot spot
data HotSpotWeight
  = Critical   -- ^ >20% of total time/alloc
  | High       -- ^ 10-20%
  | Medium     -- ^ 5-10%
  | Low        -- ^ 1-5%
  | Negligible -- ^ <1%
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | A performance hot spot identified in the code
data HotSpot = HotSpot
  { hsName           :: Text           -- ^ Function/module name
  , hsLocation       :: SrcSpan        -- ^ Source location
  , hsTimePercent    :: Double         -- ^ Percentage of total execution time
  , hsAllocPercent   :: Double         -- ^ Percentage of total allocations
  , hsWeight         :: HotSpotWeight  -- ^ Computed weight/importance
  , hsEntries        :: Integer        -- ^ Number of times entered
  , hsSource         :: Text           -- ^ "profiling" or "benchmark"
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | A benchmark-guided suggestion linking hot spots with diagnostics
data BenchGuidedSuggestion = BenchGuidedSuggestion
  { bgsHotSpot       :: HotSpot
  , bgsDiagnostics   :: [Diagnostic]   -- ^ Related diagnostics in this hot spot
  , bgsImpact        :: Double         -- ^ Estimated performance impact (0-100)
  , bgsReason        :: Text           -- ^ Why this is suggested
  , bgsPriority      :: Int            -- ^ Priority ranking (1 = highest)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

--------------------------------------------------------------------------------
-- Benchmark Parsing (Criterion JSON)
--------------------------------------------------------------------------------

-- | Parse Criterion JSON benchmark output
--
-- Criterion outputs an array of report objects. We extract timing information
-- from each benchmark.
parseCriterionJson :: ByteString -> Either Text [BenchmarkResult]
parseCriterionJson bs =
  case Aeson.eitherDecode bs of
    Left err -> Left $ "Failed to parse Criterion JSON: " <> T.pack err
    Right reports -> Right $ mapMaybe parseCriterionReport reports

-- | Parse a single Criterion report object
parseCriterionReport :: Aeson.Value -> Maybe BenchmarkResult
parseCriterionReport = parseMaybe $ withObject "CriterionReport" $ \obj -> do
  name <- obj .: "reportName"
  analysis <- obj .: "reportAnalysis"

  -- Extract mean time from analysis
  meanObj <- analysis .: "anMean"
  meanTime <- meanObj .: "estPoint"

  -- Extract standard deviation
  stdDevObj <- analysis .: "anStdDev"
  stdDev <- stdDevObj .: "estPoint"

  -- Optional allocations (not always present)
  allocs <- obj .:? "reportAllocs"
  runs <- obj .:? "reportRuns"

  pure BenchmarkResult
    { brName = name
    , brMeanTime = Seconds meanTime
    , brStdDev = Seconds stdDev
    , brAllocations = allocs
    , brRuns = runs
    }

-- | Extract benchmark timings as a map from name to time
extractBenchmarkTimings :: [BenchmarkResult] -> Map Text Seconds
extractBenchmarkTimings = Map.fromList . map (\br -> (brName br, brMeanTime br))

--------------------------------------------------------------------------------
-- GHC Profiling Parsing
--------------------------------------------------------------------------------

-- | Parse GHC profiling output (auto-detects JSON vs text format)
parseGHCProfile :: Text -> Either Text ProfileData
parseGHCProfile input
  | "{" `T.isPrefixOf` T.strip input =
      parseGHCProfileJson (LBS.fromStrict $ TE.encodeUtf8 input)
  | otherwise =
      parseGHCProfileText input

-- | Parse GHC profiling JSON format (.prof.json)
parseGHCProfileJson :: ByteString -> Either Text ProfileData
parseGHCProfileJson bs =
  case Aeson.eitherDecode bs of
    Left err -> Left $ "Failed to parse GHC profile JSON: " <> T.pack err
    Right profObj -> case parseProfileObject profObj of
      Left err -> Left $ T.pack err
      Right result -> Right result
  where
    parseProfileObject :: Aeson.Value -> Either String ProfileData
    parseProfileObject = parseEither $ withObject "Profile" $ \obj -> do
      -- Extract metadata
      totalTime <- obj .:? "total_time" .!= 0
      totalAlloc <- obj .:? "total_alloc" .!= 0
      cmd <- obj .:? "program" .!= "unknown"

      let metadata = ProfileMetadata
            { pmCommand = cmd
            , pmTotalTime = Seconds totalTime
            , pmTotalAlloc = totalAlloc
            , pmTimestamp = Nothing
            , pmRtsOptions = []
            }

      -- Extract cost centers
      costCenters <- obj .: "cost_centres"
      entries <- mapM parseCostCenter costCenters

      pure ProfileData
        { pdMetadata = metadata
        , pdCostCenters = entries
        , pdTotalCostCenters = length entries
        }

    parseCostCenter :: Aeson.Value -> Parser CostCenterEntry
    parseCostCenter = withObject "CostCenter" $ \obj -> do
      ccId <- obj .: "id"
      ccName <- obj .: "label"
      ccModule <- obj .: "module"
      ccSrc <- obj .:? "src_loc"
      ccTime <- obj .:? "time_percent" .!= 0
      ccAlloc <- obj .:? "alloc_percent" .!= 0
      ccTimeIndiv <- obj .:? "time_indiv" .!= 0
      ccAllocIndiv <- obj .:? "alloc_indiv" .!= 0
      ccEntries <- obj .:? "entries" .!= 0

      pure CostCenterEntry
        { cceId = ccId
        , cceName = ccName
        , cceModule = ccModule
        , cceSrcLoc = ccSrc
        , cceTimePercent = ccTime
        , cceAllocPercent = ccAlloc
        , cceTimeIndiv = ccTimeIndiv
        , cceAllocIndiv = fromInteger ccAllocIndiv
        , cceEntries = ccEntries
        }

-- | Parse GHC profiling text format (.prof)
--
-- The text format has a structure like:
--
-- @
--     individual     inherited
-- COST CENTRE  MODULE  entries  %time %alloc   %time %alloc
--
-- MAIN         MAIN    0        0.0   0.0      100.0 100.0
--  main        Main    1        5.2   3.1      100.0 100.0
--   parseFile  Parser  42       15.3  22.5     94.8  96.9
-- @
parseGHCProfileText :: Text -> Either Text ProfileData
parseGHCProfileText input =
  let ls = T.lines input
      metadata = extractMetadata ls
      costCenters = parseCostCenterLines ls
  in Right ProfileData
    { pdMetadata = metadata
    , pdCostCenters = costCenters
    , pdTotalCostCenters = length costCenters
    }
  where
    extractMetadata :: [Text] -> ProfileMetadata
    extractMetadata ls =
      let totalTime = extractTotalTime ls
          totalAlloc = extractTotalAlloc ls
          cmd = extractCommand ls
      in ProfileMetadata
        { pmCommand = cmd
        , pmTotalTime = Seconds totalTime
        , pmTotalAlloc = totalAlloc
        , pmTimestamp = Nothing
        , pmRtsOptions = []
        }

    extractTotalTime :: [Text] -> Double
    extractTotalTime ls =
      case find (T.isInfixOf "total time") ls of
        Nothing -> 0
        Just line ->
          -- Format: "total time  =        2.50 secs"
          -- Extract the numeric value after "="
          case T.splitOn "=" line of
            [_, rest] ->
              let parts = T.words rest
              in case parts of
                (timeStr:_) -> fromMaybe 0 (readMaybe $ T.unpack timeStr)
                _ -> 0
            _ -> 0

    extractTotalAlloc :: [Text] -> Integer
    extractTotalAlloc ls =
      case find (T.isInfixOf "total alloc") ls of
        Nothing -> 0
        Just line ->
          -- Format: "total alloc = 1,250,000,000 bytes"
          -- Extract the numeric value after "="
          case T.splitOn "=" line of
            [_, rest] ->
              let parts = T.words rest
              in case parts of
                (allocStr:_) -> fromMaybe 0 (readMaybe $ T.unpack $ T.replace "," "" allocStr)
                _ -> 0
            _ -> 0

    extractCommand :: [Text] -> Text
    extractCommand ls =
      case find (\l -> "+" `T.isInfixOf` l && "RTS" `T.isInfixOf` l) ls of
        Nothing -> "unknown"
        Just line -> T.strip $ T.takeWhile (/= '+') line

    parseCostCenterLines :: [Text] -> [CostCenterEntry]
    parseCostCenterLines ls =
      let dataLines = dropWhile (not . isCostCenterHeader) ls
          entries = drop 2 dataLines  -- Skip header and separator
      in mapMaybe parseCostCenterLine (zip [1..] entries)

    isCostCenterHeader :: Text -> Bool
    isCostCenterHeader line = "COST CENTRE" `T.isInfixOf` line

    parseCostCenterLine :: (Int, Text) -> Maybe CostCenterEntry
    parseCostCenterLine (idx, line)
      | T.null (T.strip line) = Nothing
      | otherwise =
          -- Format: "COST CENTRE  MODULE  entries  %time %alloc  %time %alloc"
          -- Column order: name, module, entries, individual %time, individual %alloc,
          --               inherited %time, inherited %alloc
          let parts = T.words line
          in case parts of
            (name:modName:entriesStr:timeIndiv:allocIndiv:_) ->
              Just CostCenterEntry
                { cceId = idx
                , cceName = name
                , cceModule = modName
                , cceSrcLoc = Nothing
                , cceTimePercent = fromMaybe 0 (readMaybe $ T.unpack timeIndiv)
                , cceAllocPercent = fromMaybe 0 (readMaybe $ T.unpack allocIndiv)
                , cceTimeIndiv = fromMaybe 0 (readMaybe $ T.unpack timeIndiv)
                , cceAllocIndiv = 0
                , cceEntries = fromMaybe 0 (readMaybe $ T.unpack entriesStr)
                }
            _ -> Nothing

-- | Parse cost center source location string (format: "file:line:col")
parseCostCenterStack :: Text -> Maybe SrcSpan
parseCostCenterStack locStr =
  case T.splitOn ":" locStr of
    [file, lineStr, colStr] -> do
      line <- readMaybe (T.unpack lineStr)
      col <- readMaybe (T.unpack colStr)
      Just $ SrcSpan
        { srcSpanFile = T.unpack file
        , srcSpanStartLine = Line line
        , srcSpanStartCol = Column col
        , srcSpanEndLine = Line line
        , srcSpanEndCol = Column (col + 10)  -- Approximate end
        }
    [file, lineStr, colStr, endLineStr, endColStr] -> do
      startLine <- readMaybe (T.unpack lineStr)
      startCol <- readMaybe (T.unpack colStr)
      endLine <- readMaybe (T.unpack endLineStr)
      endCol <- readMaybe (T.unpack endColStr)
      Just $ SrcSpan
        { srcSpanFile = T.unpack file
        , srcSpanStartLine = Line startLine
        , srcSpanStartCol = Column startCol
        , srcSpanEndLine = Line endLine
        , srcSpanEndCol = Column endCol
        }
    _ -> Nothing

--------------------------------------------------------------------------------
-- Hot Spot Identification
--------------------------------------------------------------------------------

-- | Identify hot spots from profiling data
--
-- Hot spots are functions or code regions that consume significant
-- time or allocations. We identify them based on configurable thresholds.
identifyHotSpots :: ProfileData -> [HotSpot]
identifyHotSpots ProfileData{..} =
  let costCenters = pdCostCenters
      hotSpots = mapMaybe (costCenterToHotSpot pdMetadata) costCenters
  in sortBy (comparing (Down . hsTimePercent)) hotSpots

-- | Convert a cost center entry to a hot spot
costCenterToHotSpot :: ProfileMetadata -> CostCenterEntry -> Maybe HotSpot
costCenterToHotSpot _metadata CostCenterEntry{..}
  | cceTimePercent < 1.0 && cceAllocPercent < 1.0 = Nothing  -- Filter insignificant
  | otherwise =
      let location = case cceSrcLoc of
            Just locStr -> fromMaybe noHotSpotLoc (parseCostCenterStack locStr)
            Nothing -> noHotSpotLoc
          weight = calculateHotSpotWeight cceTimePercent cceAllocPercent
      in Just HotSpot
        { hsName = cceName <> " (" <> cceModule <> ")"
        , hsLocation = location
        , hsTimePercent = cceTimePercent
        , hsAllocPercent = cceAllocPercent
        , hsWeight = weight
        , hsEntries = cceEntries
        , hsSource = "profiling"
        }
  where
    noHotSpotLoc = SrcSpan "" (Line 0) (Column 0) (Line 0) (Column 0)

-- | Calculate hot spot weight based on time and allocation percentages
calculateHotSpotWeight :: Double -> Double -> HotSpotWeight
calculateHotSpotWeight timePercent allocPercent =
  let maxPercent = max timePercent allocPercent
  in if maxPercent > 20 then Critical
     else if maxPercent > 10 then High
     else if maxPercent > 5 then Medium
     else if maxPercent > 1 then Low
     else Negligible

-- | Filter hot spots by minimum weight (Critical is highest priority)
-- HotSpotWeight ordering: Critical < High < Medium < Low < Negligible
-- So to filter for "at least Critical", we want weight <= Critical
filterHotSpots :: HotSpotWeight -> [HotSpot] -> [HotSpot]
filterHotSpots minWeight = filter (\hs -> hsWeight hs <= minWeight)

-- | Get top N hot spots by time percentage
topHotSpots :: Int -> [HotSpot] -> [HotSpot]
topHotSpots n = take n . sortBy (comparing (Down . hsTimePercent))

--------------------------------------------------------------------------------
-- Correlation with Diagnostics
--------------------------------------------------------------------------------

-- | Correlate hot spots with diagnostics to generate suggestions
--
-- For each hot spot, we find diagnostics that occur within or near
-- the hot spot's source location, and create suggestions for prioritization.
correlateHotSpots :: [HotSpot] -> [Diagnostic] -> [BenchGuidedSuggestion]
correlateHotSpots hotSpots diagnostics =
  let suggestions = mapMaybe (correlateSingleHotSpot diagnostics) hotSpots
      ranked = zip [1..] $ sortBy (comparing (Down . bgsImpact)) suggestions
  in map (\(priority, sug) -> sug { bgsPriority = priority }) ranked

-- | Correlate a single hot spot with diagnostics
correlateSingleHotSpot :: [Diagnostic] -> HotSpot -> Maybe BenchGuidedSuggestion
correlateSingleHotSpot diagnostics hotSpot =
  let relatedDiags = findDiagnosticsInHotSpot hotSpot diagnostics
      impact = calculateImpact hotSpot relatedDiags
      reason = generateReason hotSpot relatedDiags
  in if null relatedDiags
     then Nothing
     else Just BenchGuidedSuggestion
       { bgsHotSpot = hotSpot
       , bgsDiagnostics = relatedDiags
       , bgsImpact = impact
       , bgsReason = reason
       , bgsPriority = 0  -- Will be set during ranking
       }

-- | Find diagnostics that occur in or near a hot spot
findDiagnosticsInHotSpot :: HotSpot -> [Diagnostic] -> [Diagnostic]
findDiagnosticsInHotSpot hotSpot = filter (diagInHotSpot hotSpot)
  where
    diagInHotSpot :: HotSpot -> Diagnostic -> Bool
    diagInHotSpot hs diag =
      let diagLoc = diagSpan diag
          hotLoc = hsLocation hs
      in spansOverlap diagLoc hotLoc || spansNearby diagLoc hotLoc

    spansOverlap :: SrcSpan -> SrcSpan -> Bool
    spansOverlap s1 s2 =
      srcSpanFile s1 == srcSpanFile s2 &&
      not (srcSpanEndLine s1 < srcSpanStartLine s2 ||
           srcSpanStartLine s1 > srcSpanEndLine s2)

    spansNearby :: SrcSpan -> SrcSpan -> Bool
    spansNearby s1 s2 =
      srcSpanFile s1 == srcSpanFile s2 &&
      abs (unLine (srcSpanStartLine s1) - unLine (srcSpanStartLine s2)) <= 10

-- | Calculate impact score for a hot spot with diagnostics
calculateImpact :: HotSpot -> [Diagnostic] -> Double
calculateImpact hotSpot diagnostics =
  let baseImpact = hsTimePercent hotSpot + (hsAllocPercent hotSpot * 0.5)
      perfDiagCount = length $ filter isPerfDiagnostic diagnostics
      perfMultiplier = 1.0 + (fromIntegral perfDiagCount * 0.2)
  in min 100.0 (baseImpact * perfMultiplier)
  where
    isPerfDiagnostic :: Diagnostic -> Bool
    isPerfDiagnostic diag = diagKind diag == PerformanceIssue

-- | Generate reason text for a suggestion
generateReason :: HotSpot -> [Diagnostic] -> Text
generateReason hotSpot diagnostics =
  let timeText = T.pack $ printf "%.1f%%" (hsTimePercent hotSpot)
      allocText = T.pack $ printf "%.1f%%" (hsAllocPercent hotSpot)
      diagCount = length diagnostics
      perfCount = length $ filter (\d -> diagKind d == PerformanceIssue) diagnostics
  in T.unwords
    [ "Hot spot consuming", timeText, "of time and", allocText, "of allocations."
    , "Found", T.pack (show diagCount), "diagnostic(s) in this area"
    , if perfCount > 0
      then "(" <> T.pack (show perfCount) <> " performance-related)."
      else "."
    , "Fixing issues here will have high impact."
    ]

-- | Prioritize diagnostics based on hot spots
--
-- Diagnostics in hot spots are moved to the front of the list
-- and sorted by the hot spot's importance.
prioritizeDiagnostics :: [Diagnostic] -> [HotSpot] -> [Diagnostic]
prioritizeDiagnostics diagnostics hotSpots =
  let (inHotSpots, notInHotSpots) = partitionByHotSpots diagnostics hotSpots
      sortedInHotSpots = sortBy (comparing diagHotSpotPriority) inHotSpots
  in sortedInHotSpots ++ notInHotSpots
  where
    partitionByHotSpots :: [Diagnostic] -> [HotSpot] -> ([Diagnostic], [Diagnostic])
    partitionByHotSpots diags hss =
      let isInHotSpot diag = any (\hs -> diag `elem` findDiagnosticsInHotSpot hs diags) hss
      in (filter isInHotSpot diags, filter (not . isInHotSpot) diags)

    diagHotSpotPriority :: Diagnostic -> Double
    diagHotSpotPriority diag =
      let matchingHotSpots = filter (\hs -> diag `elem` findDiagnosticsInHotSpot hs diagnostics) hotSpots
      in if null matchingHotSpots
         then 0
         else negate $ maximum $ map hsTimePercent matchingHotSpots

-- | Adjust severity of diagnostics based on hot spot proximity
--
-- Performance issues in hot spots are escalated to higher severity.
adjustSeverityForHotSpots :: [HotSpot] -> [Diagnostic] -> [Diagnostic]
adjustSeverityForHotSpots hotSpots = map adjustDiag
  where
    adjustDiag :: Diagnostic -> Diagnostic
    adjustDiag diag =
      let matchingHotSpots = filter (\hs -> diag `elem` findDiagnosticsInHotSpot hs [diag]) hotSpots
          maxWeight = if null matchingHotSpots
                      then Negligible
                      else maximum $ map hsWeight matchingHotSpots
          newSeverity = escalateSeverity (diagSeverity diag) maxWeight (diagKind diag)
      in diag { diagSeverity = newSeverity }

    escalateSeverity :: Severity -> HotSpotWeight -> DiagnosticKind -> Severity
    escalateSeverity currentSev weight kind
      | kind == PerformanceIssue && weight >= High = max currentSev Warning
      | kind == PerformanceIssue && weight == Critical = Error
      | kind == SpaceLeak && weight >= Medium = max currentSev Warning
      | otherwise = currentSev

--------------------------------------------------------------------------------
-- Output and Reporting
--------------------------------------------------------------------------------

-- | Generate a benchmark-guided report
benchGuidedReport :: [BenchGuidedSuggestion] -> Text
benchGuidedReport suggestions =
  let header = T.unlines
        [ "╔══════════════════════════════════════════════════════════════════════╗"
        , "║               BENCHMARK-GUIDED OPTIMIZATION REPORT                   ║"
        , "╚══════════════════════════════════════════════════════════════════════╝"
        , ""
        ]
      summary = formatSummary suggestions
      details = T.intercalate "\n\n" $ map formatSuggestion suggestions
  in header <> summary <> "\n\n" <> details

-- | Format summary statistics
formatSummary :: [BenchGuidedSuggestion] -> Text
formatSummary suggestions =
  let total = length suggestions
      critical = length $ filter (\s -> hsWeight (bgsHotSpot s) == Critical) suggestions
      high = length $ filter (\s -> hsWeight (bgsHotSpot s) == High) suggestions
      totalDiags = sum $ map (length . bgsDiagnostics) suggestions
  in T.unlines
    [ "SUMMARY:"
    , "  Total hot spots with issues: " <> T.pack (show total)
    , "  Critical hot spots:          " <> T.pack (show critical)
    , "  High-priority hot spots:     " <> T.pack (show high)
    , "  Total diagnostics in hot spots: " <> T.pack (show totalDiags)
    , ""
    , "Recommendations are sorted by impact (highest first)."
    ]

-- | Format a single suggestion
formatSuggestion :: BenchGuidedSuggestion -> Text
formatSuggestion BenchGuidedSuggestion{..} =
  let HotSpot{..} = bgsHotSpot
      priorityBadge = "[Priority " <> T.pack (show bgsPriority) <> "]"
      weightBadge = case hsWeight of
        Critical -> "[CRITICAL]"
        High -> "[HIGH]"
        Medium -> "[MEDIUM]"
        Low -> "[LOW]"
        Negligible -> "[LOW]"
      location = formatLocation hsLocation
      diagList = T.unlines $ map formatDiagnosticShort bgsDiagnostics
  in T.unlines
    [ "────────────────────────────────────────────────────────────────────────"
    , priorityBadge <> " " <> weightBadge <> " " <> hsName
    , "  Location:  " <> location
    , "  Time:      " <> T.pack (printf "%.1f%%" hsTimePercent)
    , "  Alloc:     " <> T.pack (printf "%.1f%%" hsAllocPercent)
    , "  Impact:    " <> T.pack (printf "%.1f" bgsImpact)
    , "  Entries:   " <> T.pack (show hsEntries)
    , ""
    , "  Reason: " <> bgsReason
    , ""
    , "  Issues found:"
    , diagList
    ]

-- | Format a diagnostic in short form
formatDiagnosticShort :: Diagnostic -> Text
formatDiagnosticShort Diagnostic{..} =
  let sev = case diagSeverity of
        Error -> "ERROR"
        Warning -> "WARN"
        Suggestion -> "SUGG"
        Info -> "INFO"
      loc = formatLocation diagSpan
  in "    [" <> sev <> "] " <> diagMessage <> " @ " <> loc

-- | Format a source location
formatLocation :: SrcSpan -> Text
formatLocation SrcSpan{..} =
  T.pack srcSpanFile <> ":" <>
  T.pack (show $ unLine srcSpanStartLine) <> ":" <>
  T.pack (show $ unColumn srcSpanStartCol)

-- | Format hot spot summary for quick overview
formatHotSpotSummary :: [HotSpot] -> Text
formatHotSpotSummary hotSpots =
  let header = T.unlines
        [ "HOT SPOTS SUMMARY:"
        , "  Name                              Time%    Alloc%   Weight"
        , "  ────────────────────────────────  ───────  ───────  ────────"
        ]
      rows = map formatHotSpotRow (take 20 hotSpots)
  in header <> T.unlines rows
  where
    formatHotSpotRow :: HotSpot -> Text
    formatHotSpotRow HotSpot{..} =
      let name = T.take 32 $ T.justifyLeft 32 ' ' hsName
          time = T.pack $ printf "%6.2f%%" hsTimePercent
          alloc = T.pack $ printf "%6.2f%%" hsAllocPercent
          weight = T.pack $ show hsWeight
      in "  " <> name <> "  " <> time <> "  " <> alloc <> "  " <> weight

-- | Generate diagnostics enriched with hot spot information
benchGuidedDiagnostics :: [HotSpot] -> [Diagnostic] -> [Diagnostic]
benchGuidedDiagnostics hotSpots diagnostics =
  map (enrichDiagnostic hotSpots) diagnostics
  where
    enrichDiagnostic :: [HotSpot] -> Diagnostic -> Diagnostic
    enrichDiagnostic hss diag =
      let matchingHotSpots = filter (\hs -> diag `elem` findDiagnosticsInHotSpot hs [diag]) hss
      in if null matchingHotSpots
         then diag
         else
           let hotSpot = head matchingHotSpots
               perfNote = "\n[Performance Hot Spot: " <>
                         T.pack (printf "%.1f%%" (hsTimePercent hotSpot)) <>
                         " of execution time]"
               newMessage = diagMessage diag <> perfNote
           in diag { diagMessage = newMessage }

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Check if a source location is within a span
sourceLocationInSpan :: SrcSpan -> FilePath -> Int -> Int -> Bool
sourceLocationInSpan SrcSpan{..} file line col =
  srcSpanFile == file &&
  (unLine srcSpanStartLine < line ||
   (unLine srcSpanStartLine == line && unColumn srcSpanStartCol <= col)) &&
  (unLine srcSpanEndLine > line ||
   (unLine srcSpanEndLine == line && unColumn srcSpanEndCol >= col))

-- | Extract function name from a module path or identifier
functionNameFromPath :: Text -> Text
functionNameFromPath path =
  case T.splitOn "." path of
    [] -> path
    parts -> last parts

-- | Normalize a weight to 0-1 range
normalizeWeight :: HotSpotWeight -> Double
normalizeWeight = \case
  Critical -> 1.0
  High -> 0.75
  Medium -> 0.5
  Low -> 0.25
  Negligible -> 0.1
