{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Argus.Output.JUnit
-- Description : JUnit XML output format
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides JUnit XML output format for CI integration.
-- JUnit format is widely supported by CI systems like Jenkins, GitLab CI,
-- GitHub Actions, and CircleCI.
module Argus.Output.JUnit
  ( renderJUnit
  , junitFormatter
  ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Output.Types
import Argus.Types

-- | JUnit formatter
junitFormatter :: Formatter
junitFormatter = Formatter
  { fmtFormat = JUnitFormat
  , fmtRender = renderJUnit
  }

-- | Render analysis result as JUnit XML
renderJUnit :: OutputOptions -> AnalysisResult -> Output
renderJUnit _opts result = Output
  { outText = renderJUnitXml result
  , outSummary = mkSummary result
  }

-- | Render JUnit XML document
renderJUnitXml :: AnalysisResult -> Text
renderJUnitXml result =
  let files = Map.toList (resultFiles result)
      testsuites = map renderTestSuite files
      totalTests = sum $ map (length . fileResultDiagnostics . snd) files
      totalFailures = sum
        [ 1
        | (_, fr) <- files
        , d <- fileResultDiagnostics fr
        , diagSeverity d == Error
        ]
      totalErrors = 0  -- We treat errors as failures
      totalTime = 0.0 :: Double  -- We don't track execution time
  in T.unlines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<testsuites name=\"argus\" tests=\"" <> showT totalTests
        <> "\" failures=\"" <> showT totalFailures
        <> "\" errors=\"" <> showT totalErrors
        <> "\" time=\"" <> T.pack (show totalTime) <> "\">"
    , T.concat testsuites
    , "</testsuites>"
    ]

-- | Render a file as a test suite
renderTestSuite :: (FilePath, FileResult) -> Text
renderTestSuite (path, fr) =
  let diags = fileResultDiagnostics fr
      testCount = length diags
      failCount = length $ filter (\d -> diagSeverity d == Error) diags
      testcases = map (renderTestCase path) diags
  in if null diags
     then ""  -- Skip files with no issues
     else T.unlines
       [ "  <testsuite name=\"" <> escapeXml (T.pack path)
           <> "\" tests=\"" <> showT testCount
           <> "\" failures=\"" <> showT failCount
           <> "\" errors=\"0\" time=\"0\">"
       , T.concat testcases
       , "  </testsuite>"
       ]

-- | Render a diagnostic as a test case
renderTestCase :: FilePath -> Diagnostic -> Text
renderTestCase _path diag =
  let name = fromMaybe "unnamed-check" (diagCode diag)
      className = T.pack (srcSpanFile (diagSpan diag))
      line = srcSpanStartLine (diagSpan diag)
      severity = diagSeverity diag
      message = diagMessage diag
  in case severity of
       Error -> T.unlines
         [ "    <testcase name=\"" <> escapeXml name
             <> "\" classname=\"" <> escapeXml className
             <> "\" time=\"0\">"
         , "      <failure message=\"" <> escapeXml message
             <> "\" type=\"Error\">"
         , "        <![CDATA[" <> message <> " at line " <> showT line <> "]]>"
         , "      </failure>"
         , "    </testcase>"
         ]
       Warning -> T.unlines
         [ "    <testcase name=\"" <> escapeXml name
             <> "\" classname=\"" <> escapeXml className
             <> "\" time=\"0\">"
         , "      <failure message=\"" <> escapeXml message
             <> "\" type=\"Warning\">"
         , "        <![CDATA[" <> message <> " at line " <> showT line <> "]]>"
         , "      </failure>"
         , "    </testcase>"
         ]
       _ -> T.unlines
         [ "    <testcase name=\"" <> escapeXml name
             <> "\" classname=\"" <> escapeXml className
             <> "\" time=\"0\">"
         , "      <system-out><![CDATA[" <> message
             <> " at line " <> showT line <> "]]></system-out>"
         , "    </testcase>"
         ]

-- | Escape XML special characters
escapeXml :: Text -> Text
escapeXml = T.concatMap escape
  where
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '&' = "&amp;"
    escape '"' = "&quot;"
    escape '\'' = "&apos;"
    escape c = T.singleton c

-- | Show a value as Text
showT :: Show a => a -> Text
showT = T.pack . show

-- | Maybe helper
fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x
