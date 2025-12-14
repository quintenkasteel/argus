{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Output.Terminal
-- Description : Terminal output formatting (GHC style)
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Clean, readable terminal output styled after GHC compiler messages.
module Argus.Output.Terminal
  ( -- * Rendering
    renderTerminal
  , renderDiagnostic
  , renderSummary

    -- * Colors
  , severityColor
  , kindColor

    -- * Options
  , TerminalOptions (..)
  , defaultTerminalOptions
  ) where

import Data.List (sortBy, groupBy)
import Data.Maybe (mapMaybe)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import Prettyprinter.Render.Terminal

import Argus.Output.Types
import Argus.Types

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

-- | Terminal output options
data TerminalOptions = TerminalOptions
  { toColor        :: Bool
  , toShowContext  :: Bool  -- Always True by default now
  , toContextLines :: Int
  , toGroupBy      :: Text
  , toShowSummary  :: Bool
  }
  deriving stock (Eq, Show)

-- | Default terminal options - context ON by default
defaultTerminalOptions :: TerminalOptions
defaultTerminalOptions = TerminalOptions
  { toColor = True
  , toShowContext = True  -- ON by default
  , toContextLines = 2
  , toGroupBy = "file"
  , toShowSummary = True
  }

--------------------------------------------------------------------------------
-- Colors
--------------------------------------------------------------------------------

severityColor :: Severity -> AnsiStyle
severityColor Error      = color Red <> bold
severityColor Warning    = color Yellow <> bold
severityColor Suggestion = color Cyan <> bold
severityColor Info       = color Blue <> bold

kindColor :: DiagnosticKind -> AnsiStyle
kindColor NamingConvention   = color Magenta
kindColor UnusedCode         = color Yellow
kindColor UnusedImport       = color Yellow
kindColor RedundantCode      = color Cyan
kindColor CodePattern        = color Green
kindColor TypeSignature      = color Blue
kindColor ImportStyle        = color Cyan
kindColor TemplateHaskellRef = color Red
kindColor SecurityIssue      = color Red <> bold
kindColor PerformanceIssue   = color Yellow <> bold
kindColor ArchitecturalIssue = color Magenta <> bold
kindColor SpaceLeak          = color Red
kindColor PartialFunction    = color Yellow
kindColor ComplexityIssue    = color Cyan <> bold
kindColor (Custom _)         = mempty

-- Styles
gutterStyle :: AnsiStyle
gutterStyle = color Blue

locStyle :: AnsiStyle
locStyle = bold

codeStyle :: AnsiStyle
codeStyle = color Magenta

_helpStyle :: AnsiStyle
_helpStyle = color Cyan

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

renderTerminal :: OutputOptions -> AnalysisResult -> Text
renderTerminal opts result =
  let termOpts = TerminalOptions
        { toColor = ooColor opts
        , toShowContext = True  -- Always show context for terminal
        , toContextLines = ooContextLines opts
        , toGroupBy = ooGroupBy opts
        , toShowSummary = True
        }
      cache = ooSourceCache opts
      doc = renderResult termOpts cache result
  in if ooColor opts
     then renderStrict $ layoutPretty defaultLayoutOptions doc
     else T.pack $ show $ unAnnotate doc

renderResult :: TerminalOptions -> SourceCache -> AnalysisResult -> Doc AnsiStyle
renderResult opts cache result =
  let grouped = groupDiagnostics (toGroupBy opts) result
      renderedGroups = map (renderGroup opts cache) grouped
      summary = renderSummary opts (mkSummary result)
  in vsep renderedGroups <> line <> summary

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

-- | Render a group - just diagnostics, no separate file header
renderGroup :: TerminalOptions -> SourceCache -> (Text, [Diagnostic]) -> Doc AnsiStyle
renderGroup opts cache (_header, diags) =
  vsep (map (renderDiagnosticClean opts cache) diags) <> line

-- | Clean diagnostic format like GHC:
-- file:line:col: severity: [code]
--     Message here
--     |
--  10 | source code
--     | ^^^^^^^^^^^^
--     = help: fix suggestion
renderDiagnosticClean :: TerminalOptions -> SourceCache -> Diagnostic -> Doc AnsiStyle
renderDiagnosticClean opts cache diag =
  let span' = diagSpan diag
      sev = diagSeverity diag
      lineNum = srcSpanStartLineRaw span'
      startCol = srcSpanStartColRaw span'
      endCol = srcSpanEndColRaw span'
      file = srcSpanFile span'

      -- Line 1: file:line:col: severity: [code]
      locDoc = annotate locStyle $
               pretty file <> ":" <> pretty lineNum <> ":" <> pretty startCol <> ":"
      sevDoc = annotate (severityColor sev) $ pretty (severityText sev) <> ":"
      codeDoc = case diagCode diag of
        Just code -> annotate codeStyle $ brackets (pretty code)
        Nothing   -> mempty
      headerLine = locDoc <+> sevDoc <+> codeDoc

      -- Line 2: indented message
      msgLine = indent 4 $ pretty (diagMessage diag)

      -- Source context (using cache)
      contextDoc = renderSourceContext' opts cache lineNum startCol endCol file sev

      -- Fix suggestion
      fixDoc = renderFixSuggestion' diag

  in headerLine <> line <> msgLine <> contextDoc <> fixDoc <> line

-- | Render source context using source cache
renderSourceContext' :: TerminalOptions -> SourceCache -> Int -> Int -> Int -> FilePath -> Severity -> Doc AnsiStyle
renderSourceContext' _opts cache lineNum startCol endCol file sev =
  case lookupSourceLine cache file lineNum of
    Nothing -> mempty
    Just sourceLine ->
      let gutterWidth = length (show lineNum)
          gutter = annotate gutterStyle $ pretty (replicate (gutterWidth + 1) ' ') <> "|"
          lineGutter = annotate gutterStyle $ pretty (padLeft gutterWidth (show lineNum)) <+> "|"

          -- Source line
          srcLine = lineGutter <+> pretty sourceLine

          -- Caret line
          caretLen = max 1 (endCol - startCol)
          caretPadding = T.replicate (startCol - 1) " "
          carets = T.replicate caretLen "^"
          caretLine = gutter <+> annotate (severityColor sev) (pretty caretPadding <> pretty carets)

      in line <> "    " <> gutter <> line <>
         "    " <> srcLine <> line <>
         "    " <> caretLine

-- | Render fix suggestion (only if not already in message)
renderFixSuggestion' :: Diagnostic -> Doc AnsiStyle
renderFixSuggestion' _diag = mempty  -- Message already contains the suggestion

padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

-- Compatibility (uses empty cache - no source context displayed)
renderDiagnostic :: TerminalOptions -> Diagnostic -> Doc AnsiStyle
renderDiagnostic opts = renderDiagnosticClean opts Map.empty

_renderLocation :: SrcSpan -> Doc AnsiStyle
_renderLocation SrcSpan{..} =
  annotate locStyle $
    pretty srcSpanFile <> colon <>
    pretty (unLine srcSpanStartLine) <> colon <>
    pretty (unColumn srcSpanStartCol)

severityText :: Severity -> Text
severityText Error      = "error"
severityText Warning    = "warning"
severityText Suggestion = "suggestion"
severityText Info       = "info"

-- | Clean summary
renderSummary :: TerminalOptions -> Summary -> Doc AnsiStyle
renderSummary _opts Summary{..} =
  let divider = annotate (color White) $ pretty (replicate 50 '─')
  in line <> divider <> line <>
     annotate bold "Summary" <> line <> divider <> line <>
     renderStat "Files analyzed" sumTotalFiles Nothing <>
     renderStat "Files with issues" sumFilesWithIssues (Just $ sumFilesWithIssues > 0) <>
     renderStat "Diagnostics" sumTotalDiagnostics (Just $ sumTotalDiagnostics > 0) <>
     renderStat "Fixes available" sumFixesAvailable Nothing <>
     line
  where
    renderStat :: Text -> Int -> Maybe Bool -> Doc AnsiStyle
    renderStat label value mHighlight =
      let valStyle = case mHighlight of
            Just True  -> color Yellow <> bold
            Just False -> color Green <> bold
            Nothing    -> bold
      in "  " <> pretty label <> ":" <+> annotate valStyle (pretty value) <> line
