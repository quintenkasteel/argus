{-# LANGUAGE StrictData #-}

-- |
-- Module      : Linter.Output.Terminal
-- Description : Terminal output formatting
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides colored terminal output for linting results.
module Linter.Output.Terminal
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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import Prettyprinter.Render.Terminal

import Linter.Output.Types
import Linter.Types

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

-- | Terminal output options
data TerminalOptions = TerminalOptions
  { toColor        :: Bool
  , toShowContext  :: Bool
  , toContextLines :: Int
  , toGroupBy      :: Text
  , toShowSummary  :: Bool
  }
  deriving stock (Eq, Show)

-- | Default terminal options
defaultTerminalOptions :: TerminalOptions
defaultTerminalOptions = TerminalOptions
  { toColor = True
  , toShowContext = True
  , toContextLines = 2
  , toGroupBy = "file"
  , toShowSummary = True
  }

--------------------------------------------------------------------------------
-- Colors
--------------------------------------------------------------------------------

-- | Get color for severity
severityColor :: Severity -> AnsiStyle
severityColor Error      = color Red <> bold
severityColor Warning    = color Yellow
severityColor Suggestion = color Cyan
severityColor Info       = color Blue

-- | Get color for diagnostic kind
kindColor :: DiagnosticKind -> AnsiStyle
kindColor NamingConvention   = color Magenta
kindColor UnusedCode         = color Yellow
kindColor UnusedImport       = color Yellow
kindColor RedundantCode      = color Cyan
kindColor CodePattern        = color Green
kindColor TypeSignature      = color Blue
kindColor ImportStyle        = color Cyan
kindColor TemplateHaskellRef = color Red
kindColor (Custom _)         = mempty

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- | Render analysis result for terminal
renderTerminal :: OutputOptions -> AnalysisResult -> Text
renderTerminal opts result =
  let termOpts = TerminalOptions
        { toColor = ooColor opts
        , toShowContext = ooShowContext opts
        , toContextLines = ooContextLines opts
        , toGroupBy = ooGroupBy opts
        , toShowSummary = True
        }
      doc = renderResult termOpts result
  in if ooColor opts
     then renderStrict $ layoutPretty defaultLayoutOptions doc
     else T.pack $ show $ unAnnotate doc

-- | Render complete result
renderResult :: TerminalOptions -> AnalysisResult -> Doc AnsiStyle
renderResult opts result =
  let grouped = groupDiagnostics (toGroupBy opts) result
      renderedGroups = map (renderGroup opts) grouped
      summary = renderSummary opts (mkSummary result)
  in vsep renderedGroups <> line <> line <> summary

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
      map (\ds -> (fst (head ds), map snd ds)) $
        groupBy (\a b -> fst a == fst b) $
          sortBy (comparing fst) diags

    groupByRule diags =
      let byRule = groupBy (\a b -> diagCode (snd a) == diagCode (snd b)) $
                   sortBy (comparing (diagCode . snd)) diags
      in map (\ds -> (fromCode $ diagCode $ snd $ head ds, map snd ds)) byRule

    groupBySeverity diags =
      let bySev = groupBy (\a b -> diagSeverity (snd a) == diagSeverity (snd b)) $
                  sortBy (comparing (diagSeverity . snd)) diags
      in map (\ds -> (T.pack $ show $ diagSeverity $ snd $ head ds, map snd ds)) bySev

    fromCode Nothing  = "unknown"
    fromCode (Just c) = c

-- | Render a group of diagnostics
renderGroup :: TerminalOptions -> (Text, [Diagnostic]) -> Doc AnsiStyle
renderGroup opts (header, diags) =
  annotate bold (pretty header) <> colon <> line <>
  indent 2 (vsep $ map (renderDiagnostic opts) diags)

-- | Render a single diagnostic
renderDiagnostic :: TerminalOptions -> Diagnostic -> Doc AnsiStyle
renderDiagnostic opts diag =
  let sevStyle = severityColor (diagSeverity diag)
      locDoc = renderLocation (diagSpan diag)
      sevDoc = annotate sevStyle $ pretty $ severityText (diagSeverity diag)
      msgDoc = pretty (diagMessage diag)
      codeDoc = case diagCode diag of
        Just code -> brackets (pretty code)
        Nothing   -> mempty
      fixDoc = if null (diagFixes diag)
               then mempty
               else line <> indent 2 (annotate (color Green) "fix available")
  in locDoc <+> sevDoc <> codeDoc <> colon <+> msgDoc <> fixDoc

-- | Render source location
renderLocation :: SrcSpan -> Doc AnsiStyle
renderLocation SrcSpan{..} =
  annotate (color White) $
    pretty srcSpanFile <> colon <>
    pretty srcSpanStartLine <> colon <>
    pretty srcSpanStartCol

-- | Get text for severity
severityText :: Severity -> Text
severityText Error      = "error"
severityText Warning    = "warning"
severityText Suggestion = "suggestion"
severityText Info       = "info"

-- | Render summary
renderSummary :: TerminalOptions -> Summary -> Doc AnsiStyle
renderSummary _opts Summary{..} =
  annotate bold "Summary:" <> line <>
  indent 2 (vsep
    [ "Files analyzed:" <+> pretty sumTotalFiles
    , "Files with issues:" <+> pretty sumFilesWithIssues
    , "Total diagnostics:" <+> pretty sumTotalDiagnostics
    , "Fixes available:" <+> pretty sumFixesAvailable
    ])
