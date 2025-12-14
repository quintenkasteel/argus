{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Argus.Output.Checkstyle
-- Description : Checkstyle XML output format
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides Checkstyle XML output format.
-- Checkstyle format is widely used by Java tools and is supported
-- by many CI systems and code review tools.
module Argus.Output.Checkstyle
  ( renderCheckstyle
  , checkstyleFormatter
  ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Output.Types
import Argus.Types

-- | Checkstyle formatter
checkstyleFormatter :: Formatter
checkstyleFormatter = Formatter
  { fmtFormat = CheckstyleFormat
  , fmtRender = renderCheckstyle
  }

-- | Render analysis result as Checkstyle XML
renderCheckstyle :: OutputOptions -> AnalysisResult -> Output
renderCheckstyle _opts result = Output
  { outText = renderCheckstyleXml result
  , outSummary = mkSummary result
  }

-- | Render Checkstyle XML document
renderCheckstyleXml :: AnalysisResult -> Text
renderCheckstyleXml result =
  let files = Map.toList (resultFiles result)
      fileElements = map renderFile files
  in T.unlines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<checkstyle version=\"4.3\">"
    , T.concat fileElements
    , "</checkstyle>"
    ]

-- | Render a file element
renderFile :: (FilePath, FileResult) -> Text
renderFile (path, fr) =
  let diags = fileResultDiagnostics fr
      errorElements = map renderError diags
  in if null diags
     then ""  -- Skip files with no issues
     else T.unlines
       [ "  <file name=\"" <> escapeXml (T.pack path) <> "\">"
       , T.concat errorElements
       , "  </file>"
       ]

-- | Render a diagnostic as an error element
renderError :: Diagnostic -> Text
renderError diag =
  let line = srcSpanStartLineRaw (diagSpan diag)
      col = srcSpanStartColRaw (diagSpan diag)
      severity = severityToCheckstyle (diagSeverity diag)
      message = diagMessage diag
      source = fromMaybe "argus" (diagCode diag)
  in "    <error line=\"" <> showT line
       <> "\" column=\"" <> showT col
       <> "\" severity=\"" <> severity
       <> "\" message=\"" <> escapeXml message
       <> "\" source=\"" <> escapeXml source
       <> "\"/>\n"

-- | Convert severity to Checkstyle severity
severityToCheckstyle :: Severity -> Text
severityToCheckstyle Error      = "error"
severityToCheckstyle Warning    = "warning"
severityToCheckstyle Suggestion = "info"
severityToCheckstyle Info       = "info"

-- | Escape XML special characters
escapeXml :: Text -> Text
escapeXml = T.concatMap escape
  where
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '&' = "&amp;"
    escape '"' = "&quot;"
    escape '\'' = "&apos;"
    escape '\n' = "&#10;"
    escape '\r' = "&#13;"
    escape '\t' = "&#9;"
    escape c = T.singleton c

-- | Show a value as Text
showT :: Show a => a -> Text
showT = T.pack . show

-- | Maybe helper
fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x
