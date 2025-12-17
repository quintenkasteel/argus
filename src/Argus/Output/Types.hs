{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Output.Types
-- Description : Output formatting types
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module defines types for formatting linter output in various formats.
module Argus.Output.Types
  ( -- * Output types
    Output (..)
  , OutputFormat (..)
  , OutputOptions (..)

    -- * Source cache
  , SourceCache
  , lookupSourceLine

    -- * Formatters
  , Formatter (..)

    -- * Summary
  , Summary (..)
  , mkSummary
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)

import Argus.Types

--------------------------------------------------------------------------------
-- Output Types
--------------------------------------------------------------------------------

-- | Output format
data OutputFormat
  = TerminalFormat   -- ^ Colored terminal output
  | JsonFormat       -- ^ JSON output
  | SarifFormat      -- ^ SARIF format for GitHub
  | HtmlFormat       -- ^ HTML report
  | PlainFormat      -- ^ Plain text (no colors)
  | JUnitFormat      -- ^ JUnit XML format for CI
  | CodeClimateFormat -- ^ Code Climate JSON format
  | CheckstyleFormat -- ^ Checkstyle XML format
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Source file cache - maps file paths to their line contents
type SourceCache = Map FilePath [Text]

-- | Output options
data OutputOptions = OutputOptions
  { ooFormat       :: OutputFormat
  , ooColor        :: Bool
  , ooGroupBy      :: Text  -- ^ "file", "rule", "severity"
  , ooShowContext  :: Bool
  , ooContextLines :: Int
  , ooSourceCache  :: SourceCache  -- ^ Cached source file lines for context display
  }
  deriving stock (Eq, Show)

-- | Lookup a source line from the cache
-- Returns Nothing if file not in cache or line out of range
lookupSourceLine :: SourceCache -> FilePath -> Int -> Maybe Text
lookupSourceLine cache path lineNum =
  case Map.lookup path cache of
    Nothing -> Nothing
    Just lines' ->
      if lineNum > 0 && lineNum <= length lines'
        then Just (lines' !! (lineNum - 1))
        else Nothing

-- | Formatted output
data Output = Output
  { outText    :: Text       -- ^ Rendered text
  , outSummary :: Summary    -- ^ Summary information
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Summary
--------------------------------------------------------------------------------

-- | Summary of linting results
data Summary = Summary
  { sumTotalFiles    :: Int
  , sumFilesWithIssues :: Int
  , sumTotalDiagnostics :: Int
  , sumByKind        :: Map DiagnosticKind Int
  , sumBySeverity    :: Map Severity Int
  , sumFixesAvailable :: Int
  , sumFixesApplied  :: Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Summary where
  toJSON Summary{..} = object
    [ "totalFiles"       .= sumTotalFiles
    , "filesWithIssues"  .= sumFilesWithIssues
    , "totalDiagnostics" .= sumTotalDiagnostics
    , "fixesAvailable"   .= sumFixesAvailable
    , "fixesApplied"     .= sumFixesApplied
    ]

-- | Create a summary from analysis results
mkSummary :: AnalysisResult -> Summary
mkSummary result = Summary
  { sumTotalFiles = Map.size (resultFiles result)
  , sumFilesWithIssues = length $ filter hasIssues $ Map.elems (resultFiles result)
  , sumTotalDiagnostics = sum $ map (length . fileResultDiagnostics) $ Map.elems (resultFiles result)
  , sumByKind = countByKind result
  , sumBySeverity = resultDiagCount result
  , sumFixesAvailable = countFixes result
  , sumFixesApplied = 0
  }
  where
    hasIssues fr = not $ null $ fileResultDiagnostics fr

    countByKind res = foldr countDiag Map.empty allDiags
      where
        allDiags = concatMap fileResultDiagnostics $ Map.elems $ resultFiles res
        countDiag d = Map.insertWith (+) (diagKind d) 1

    countFixes res = sum
      [ length (diagFixes d)
      | fr <- Map.elems (resultFiles res)
      , d <- fileResultDiagnostics fr
      ]

--------------------------------------------------------------------------------
-- Formatter
--------------------------------------------------------------------------------

-- | A formatter for output
data Formatter = Formatter
  { fmtFormat    :: OutputFormat
  , fmtRender    :: OutputOptions -> AnalysisResult -> Output
  }

