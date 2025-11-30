{-# LANGUAGE StrictData #-}

-- |
-- Module      : Linter.Output.Types
-- Description : Output formatting types
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module defines types for formatting linter output in various formats.
module Linter.Output.Types
  ( -- * Output types
    Output (..)
  , OutputFormat (..)
  , OutputOptions (..)

    -- * Formatters
  , Formatter (..)
  , mkFormatter

    -- * Summary
  , Summary (..)
  , mkSummary
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Linter.Types

--------------------------------------------------------------------------------
-- Output Types
--------------------------------------------------------------------------------

-- | Output format
data OutputFormat
  = TerminalFormat  -- ^ Colored terminal output
  | JsonFormat      -- ^ JSON output
  | SarifFormat     -- ^ SARIF format for GitHub
  | HtmlFormat      -- ^ HTML report
  | PlainFormat     -- ^ Plain text (no colors)
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Output options
data OutputOptions = OutputOptions
  { ooFormat       :: OutputFormat
  , ooColor        :: Bool
  , ooGroupBy      :: Text  -- ^ "file", "rule", "severity"
  , ooShowContext  :: Bool
  , ooContextLines :: Int
  , ooVerbose      :: Bool
  }
  deriving stock (Eq, Show)

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

-- | Create a formatter for a given format
mkFormatter :: OutputFormat -> Formatter
mkFormatter format = Formatter
  { fmtFormat = format
  , fmtRender = \opts result -> case format of
      TerminalFormat -> Output (renderTerminal opts result) (mkSummary result)
      JsonFormat     -> Output (renderJson opts result) (mkSummary result)
      SarifFormat    -> Output (renderSarif opts result) (mkSummary result)
      HtmlFormat     -> Output (renderHtml opts result) (mkSummary result)
      PlainFormat    -> Output (renderPlain opts result) (mkSummary result)
  }
  where
    -- Placeholder implementations (TODO: move to proper modules)
    renderTerminal _ _ = ""
    renderJson _ _ = "{}"
    renderSarif _ _ = "{}"
    renderPlain _ _ = ""
    -- Note: Full HTML implementation is in Linter.Output.Html
    -- This is an inline version to avoid circular imports
    renderHtml opts result = renderHtmlInline opts result

-- | Inline HTML rendering to avoid circular imports
-- For the full implementation with CSS and better styling, use Linter.Output.Html directly
renderHtmlInline :: OutputOptions -> AnalysisResult -> Text
renderHtmlInline _ result = T.concat
  [ "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n"
  , "  <meta charset=\"UTF-8\">\n"
  , "  <title>Haskell Linter Report</title>\n"
  , "  <style>\n"
  , "    body { font-family: sans-serif; margin: 2rem; background: #1a1a2e; color: #eee; }\n"
  , "    .summary { background: #16213e; padding: 1rem; border-radius: 8px; margin-bottom: 2rem; }\n"
  , "    .diagnostic { background: #0f3460; padding: 1rem; margin: 0.5rem 0; border-radius: 6px; }\n"
  , "    .error { border-left: 4px solid #e74c3c; }\n"
  , "    .warning { border-left: 4px solid #f39c12; }\n"
  , "    .suggestion { border-left: 4px solid #3498db; }\n"
  , "    .info { border-left: 4px solid #9b59b6; }\n"
  , "  </style>\n</head>\n<body>\n"
  , "<h1>Haskell Linter Report</h1>\n"
  , renderSummaryHtml summary
  , renderDiagnosticsHtml result
  , "</body>\n</html>\n"
  ]
  where
    summary = mkSummary result

    renderSummaryHtml Summary{..} = T.concat
      [ "<div class=\"summary\">\n"
      , "  <p><strong>Files analyzed:</strong> ", T.pack $ show sumTotalFiles, "</p>\n"
      , "  <p><strong>Files with issues:</strong> ", T.pack $ show sumFilesWithIssues, "</p>\n"
      , "  <p><strong>Total diagnostics:</strong> ", T.pack $ show sumTotalDiagnostics, "</p>\n"
      , "  <p><strong>Fixes available:</strong> ", T.pack $ show sumFixesAvailable, "</p>\n"
      , "</div>\n"
      ]

    renderDiagnosticsHtml res =
      let diags = [ (fileResultPath fr, d)
                  | fr <- Map.elems (resultFiles res)
                  , d <- fileResultDiagnostics fr
                  ]
      in if null diags
         then "<p>No issues found!</p>\n"
         else T.concat $ map renderDiag diags

    renderDiag (path, diag) = T.concat
      [ "<div class=\"diagnostic ", severityClass (diagSeverity diag), "\">\n"
      , "  <p><strong>", T.pack path, ":", T.pack $ show $ srcSpanStartLine (diagSpan diag)
      , ":</strong> ", escapeHtml (diagMessage diag), "</p>\n"
      , case diagCode diag of
          Just code -> "  <p><em>Rule: " <> escapeHtml code <> "</em></p>\n"
          Nothing   -> ""
      , "</div>\n"
      ]

    severityClass Error      = "error"
    severityClass Warning    = "warning"
    severityClass Suggestion = "suggestion"
    severityClass Info       = "info"

    escapeHtml = T.concatMap escape
      where
        escape '<'  = "&lt;"
        escape '>'  = "&gt;"
        escape '&'  = "&amp;"
        escape '"'  = "&quot;"
        escape c    = T.singleton c
