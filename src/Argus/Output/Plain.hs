{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Output.Plain
-- Description : Plain text output format (no colors)
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides plain text output format without ANSI colors.
-- Suitable for piping to files, CI logs, and environments without
-- color support. The format mirrors the terminal output but without
-- any styling.
module Argus.Output.Plain
  ( -- * Rendering
    renderPlain
  , renderDiagnosticPlain
  , renderSummaryPlain

    -- * Formatter
  , plainFormatter
  ) where

import Data.List (sortBy, groupBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Output.Types
import Argus.Types

--------------------------------------------------------------------------------
-- Formatter
--------------------------------------------------------------------------------

-- | Plain text formatter
plainFormatter :: Formatter
plainFormatter = Formatter
  { fmtFormat = PlainFormat
  , fmtRender = renderPlain
  }

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- | Render analysis result as plain text (no colors)
renderPlain :: OutputOptions -> AnalysisResult -> Output
renderPlain opts result = Output
  { outText = renderPlainText opts result
  , outSummary = mkSummary result
  }

-- | Render the complete plain text output
renderPlainText :: OutputOptions -> AnalysisResult -> Text
renderPlainText opts result =
  let cache = ooSourceCache opts
      grouped = groupDiagnostics (ooGroupBy opts) result
      renderedGroups = map (renderGroup cache) grouped
      summary = renderSummaryPlain (mkSummary result)
  in T.unlines (renderedGroups ++ ["", summary])

-- | Group diagnostics by file, rule, or severity
groupDiagnostics :: Text -> AnalysisResult -> [(Text, [Diagnostic])]
groupDiagnostics groupMode result =
  let allDiags = [ (T.pack $ fileResultPath fr, d)
                 | fr <- Map.elems (resultFiles result)
                 , d <- fileResultDiagnostics fr
                 ]
  in case groupMode of
       "file"     -> groupByFile allDiags
       "rule"     -> groupByRule allDiags
       "severity" -> groupBySeverity allDiags
       _          -> groupByFile allDiags
  where
    groupByFile diags =
      mapMaybe extractGroup $
        groupBy (\a b -> fst a == fst b) $
          sortBy (comparing fst) diags

    groupByRule diags =
      let byRule = groupBy (\a b -> diagCode (snd a) == diagCode (snd b)) $
                   sortBy (comparing (diagCode . snd)) diags
      in mapMaybe extractGroupByCode byRule

    groupBySeverity diags =
      let bySev = groupBy (\a b -> diagSeverity (snd a) == diagSeverity (snd b)) $
                  sortBy (comparing (diagSeverity . snd)) diags
      in mapMaybe extractGroupBySeverity bySev

    extractGroup [] = Nothing
    extractGroup (d:ds) = Just (fst d, map snd (d:ds))

    extractGroupByCode [] = Nothing
    extractGroupByCode (d:ds) = Just (fromCode $ diagCode $ snd d, map snd (d:ds))

    extractGroupBySeverity [] = Nothing
    extractGroupBySeverity (d:ds) = Just (T.pack $ show $ diagSeverity $ snd d, map snd (d:ds))

    fromCode Nothing  = "unknown"
    fromCode (Just c) = c

-- | Render a group of diagnostics
renderGroup :: SourceCache -> (Text, [Diagnostic]) -> Text
renderGroup cache (_header, diags) =
  T.intercalate "\n" (map (renderDiagnosticPlainWithCache cache) diags)

--------------------------------------------------------------------------------
-- Diagnostic Rendering
--------------------------------------------------------------------------------

-- | Render a single diagnostic in plain text format (no source context)
-- Format: file:line:col: severity: [code]
--     Message here
renderDiagnosticPlain :: Diagnostic -> Text
renderDiagnosticPlain = renderDiagnosticPlainWithCache Map.empty

-- | Render a single diagnostic in plain text format with source cache
-- Format: file:line:col: severity: [code]
--     Message here
--     |
--  10 | source code
--     | ^^^^^^^^^^^^
renderDiagnosticPlainWithCache :: SourceCache -> Diagnostic -> Text
renderDiagnosticPlainWithCache cache diag =
  let span' = diagSpan diag
      sev = diagSeverity diag
      lineNum = srcSpanStartLineRaw span'
      startCol = srcSpanStartColRaw span'
      endCol = srcSpanEndColRaw span'
      file = srcSpanFile span'

      -- Line 1: file:line:col: severity: [code]
      locStr = T.pack file <> ":" <> showT lineNum <> ":" <> showT startCol <> ":"
      sevStr = severityText sev <> ":"
      codeStr = case diagCode diag of
        Just code -> "[" <> code <> "]"
        Nothing   -> ""
      headerLine = locStr <> " " <> sevStr <> " " <> codeStr

      -- Line 2: indented message
      msgLine = "    " <> diagMessage diag

      -- Source context (using cache)
      contextLines = renderSourceContextWithCache cache lineNum startCol endCol file

  in T.intercalate "\n" $ filter (not . T.null) [headerLine, msgLine, contextLines, ""]

-- | Render source context without colors, using source cache
renderSourceContextWithCache :: SourceCache -> Int -> Int -> Int -> FilePath -> Text
renderSourceContextWithCache cache lineNum startCol endCol file =
  case lookupSourceLine cache file lineNum of
    Nothing -> ""
    Just sourceLine ->
      let gutterWidth = length (show lineNum)
          gutter = T.replicate (gutterWidth + 1) " " <> "|"
          lineGutter = padLeft gutterWidth (show lineNum) <> " |"

          -- Source line
          srcLine = "    " <> lineGutter <> " " <> sourceLine

          -- Caret line
          caretLen = max 1 (endCol - startCol)
          caretPadding = T.replicate (startCol - 1) " "
          carets = T.replicate caretLen "^"
          caretLine = "    " <> gutter <> " " <> caretPadding <> carets

          -- Empty gutter line
          emptyLine = "    " <> gutter

      in T.intercalate "\n" [emptyLine, srcLine, caretLine]

-- | Convert severity to text
severityText :: Severity -> Text
severityText Error      = "error"
severityText Warning    = "warning"
severityText Suggestion = "suggestion"
severityText Info       = "info"

--------------------------------------------------------------------------------
-- Summary Rendering
--------------------------------------------------------------------------------

-- | Render a summary in plain text
renderSummaryPlain :: Summary -> Text
renderSummaryPlain Summary{..} =
  let divider = T.replicate 50 "-"
  in T.unlines
    [ divider
    , "Summary"
    , divider
    , "  Files analyzed:     " <> showT sumTotalFiles
    , "  Files with issues:  " <> showT sumFilesWithIssues
    , "  Total diagnostics:  " <> showT sumTotalDiagnostics
    , "  Fixes available:    " <> showT sumFixesAvailable
    ]

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- | Show a value as Text
showT :: Show a => a -> Text
showT = T.pack . show

-- | Left-pad a string
padLeft :: Int -> String -> Text
padLeft n s = T.pack $ replicate (n - length s) ' ' ++ s
