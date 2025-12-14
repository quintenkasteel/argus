{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Output.Html
-- Description : HTML output formatting
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides HTML report output for linting results.
module Argus.Output.Html
  ( -- * Rendering
    renderHtml
  , renderHtmlReport

    -- * Options
  , HtmlOptions (..)
  , defaultHtmlOptions
  ) where

import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Output.Types
import Argus.Types

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

-- | HTML output options
data HtmlOptions = HtmlOptions
  { hoShowContext  :: Bool
  , hoContextLines :: Int
  , hoGroupBy      :: Text
  , hoShowSummary  :: Bool
  , hoEmbedCss     :: Bool
  }
  deriving stock (Eq, Show)

-- | Default HTML options
defaultHtmlOptions :: HtmlOptions
defaultHtmlOptions = HtmlOptions
  { hoShowContext = True
  , hoContextLines = 2
  , hoGroupBy = "file"
  , hoShowSummary = True
  , hoEmbedCss = True
  }

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- | Render analysis result as HTML
renderHtml :: OutputOptions -> AnalysisResult -> Text
renderHtml opts result =
  let htmlOpts = HtmlOptions
        { hoShowContext = ooShowContext opts
        , hoContextLines = ooContextLines opts
        , hoGroupBy = ooGroupBy opts
        , hoShowSummary = True
        , hoEmbedCss = True
        }
  in renderHtmlReport htmlOpts result

-- | Render complete HTML report
renderHtmlReport :: HtmlOptions -> AnalysisResult -> Text
renderHtmlReport opts result =
  let summary = mkSummary result
      allDiags = collectDiagnostics result
      grouped = groupDiagnostics (hoGroupBy opts) allDiags
  in T.concat
    [ htmlHeader
    , cssStyles
    , "</head>\n<body>\n"
    , renderTitle
    , renderSummaryHtml summary
    , renderControls
    , renderDiagnosticsHtml grouped
    , jsScript
    , htmlFooter
    ]

-- | Collect all diagnostics with file paths
collectDiagnostics :: AnalysisResult -> [(FilePath, Diagnostic)]
collectDiagnostics result =
  [ (fileResultPath fr, d)
  | fr <- Map.elems (resultFiles result)
  , d <- fileResultDiagnostics fr
  ]

-- | Group diagnostics by file, rule, or severity
groupDiagnostics :: Text -> [(FilePath, Diagnostic)] -> [(Text, [(FilePath, Diagnostic)])]
groupDiagnostics groupMode diags =
  case groupMode of
    "file"     -> groupByFile diags
    "rule"     -> groupByRule diags
    "severity" -> groupBySeverity diags
    _          -> groupByFile diags
  where
    groupByFile :: [(FilePath, Diagnostic)] -> [(Text, [(FilePath, Diagnostic)])]
    groupByFile ds =
      let sorted = sortBy (comparing fst) ds
          files = uniqueList $ map fst sorted
      in [(T.pack f, filter ((== f) . fst) sorted) | f <- files]

    groupByRule :: [(FilePath, Diagnostic)] -> [(Text, [(FilePath, Diagnostic)])]
    groupByRule ds =
      let sorted = sortBy (comparing (diagCode . snd)) ds
          codes = uniqueListText $ map (fromMaybeT "unknown" . diagCode . snd) sorted
      in [(c, filter ((== Just c) . diagCode . snd) sorted) | c <- codes]

    groupBySeverity :: [(FilePath, Diagnostic)] -> [(Text, [(FilePath, Diagnostic)])]
    groupBySeverity ds =
      let sorted = sortBy (comparing (diagSeverity . snd)) ds
          sevs = uniqueListSev $ map (diagSeverity . snd) sorted
      in [(T.pack $ show s, filter ((== s) . diagSeverity . snd) sorted) | s <- sevs]

    uniqueList :: [FilePath] -> [FilePath]
    uniqueList = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

    uniqueListText :: [Text] -> [Text]
    uniqueListText = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

    uniqueListSev :: [Severity] -> [Severity]
    uniqueListSev = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

    fromMaybeT :: Text -> Maybe Text -> Text
    fromMaybeT d Nothing  = d
    fromMaybeT _ (Just x) = x

--------------------------------------------------------------------------------
-- HTML Components
--------------------------------------------------------------------------------

htmlHeader :: Text
htmlHeader = T.concat
  [ "<!DOCTYPE html>\n"
  , "<html lang=\"en\">\n"
  , "<head>\n"
  , "  <meta charset=\"UTF-8\">\n"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
  , "  <title>Argus Report</title>\n"
  ]

cssStyles :: Text
cssStyles = T.concat
  [ "  <style>\n"
  , "    :root {\n"
  , "      --bg-primary: #1a1a2e;\n"
  , "      --bg-secondary: #16213e;\n"
  , "      --bg-card: #0f3460;\n"
  , "      --text-primary: #eaeaea;\n"
  , "      --text-secondary: #a0a0a0;\n"
  , "      --error: #e74c3c;\n"
  , "      --warning: #f39c12;\n"
  , "      --suggestion: #3498db;\n"
  , "      --info: #9b59b6;\n"
  , "      --success: #27ae60;\n"
  , "      --border: #2c3e50;\n"
  , "    }\n"
  , "    * { margin: 0; padding: 0; box-sizing: border-box; }\n"
  , "    body {\n"
  , "      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;\n"
  , "      background: var(--bg-primary);\n"
  , "      color: var(--text-primary);\n"
  , "      line-height: 1.6;\n"
  , "      padding: 2rem;\n"
  , "    }\n"
  , "    .container { max-width: 1200px; margin: 0 auto; }\n"
  , "    h1 {\n"
  , "      font-size: 2rem;\n"
  , "      margin-bottom: 1.5rem;\n"
  , "      border-bottom: 2px solid var(--border);\n"
  , "      padding-bottom: 0.5rem;\n"
  , "    }\n"
  , "    .controls {\n"
  , "      background: var(--bg-secondary);\n"
  , "      padding: 1rem;\n"
  , "      border-radius: 8px;\n"
  , "      margin-bottom: 1.5rem;\n"
  , "      display: flex;\n"
  , "      flex-wrap: wrap;\n"
  , "      gap: 0.75rem;\n"
  , "      align-items: center;\n"
  , "    }\n"
  , "    .controls label { color: var(--text-secondary); font-size: 0.9rem; }\n"
  , "    .filter-btn {\n"
  , "      padding: 0.4rem 0.8rem;\n"
  , "      border: 1px solid var(--border);\n"
  , "      border-radius: 4px;\n"
  , "      background: var(--bg-card);\n"
  , "      color: var(--text-primary);\n"
  , "      cursor: pointer;\n"
  , "      transition: all 0.2s;\n"
  , "    }\n"
  , "    .filter-btn:hover { background: var(--bg-secondary); }\n"
  , "    .filter-btn.active { border-color: var(--success); background: var(--bg-secondary); }\n"
  , "    .filter-btn.active-error { border-color: var(--error); }\n"
  , "    .filter-btn.active-warning { border-color: var(--warning); }\n"
  , "    .filter-btn.active-suggestion { border-color: var(--suggestion); }\n"
  , "    .filter-btn.active-info { border-color: var(--info); }\n"
  , "    .search-input {\n"
  , "      padding: 0.4rem 0.8rem;\n"
  , "      border: 1px solid var(--border);\n"
  , "      border-radius: 4px;\n"
  , "      background: var(--bg-card);\n"
  , "      color: var(--text-primary);\n"
  , "      min-width: 200px;\n"
  , "    }\n"
  , "    .search-input:focus { outline: none; border-color: var(--suggestion); }\n"
  , "    .summary {\n"
  , "      background: var(--bg-secondary);\n"
  , "      padding: 1.5rem;\n"
  , "      border-radius: 8px;\n"
  , "      margin-bottom: 2rem;\n"
  , "      display: grid;\n"
  , "      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));\n"
  , "      gap: 1rem;\n"
  , "    }\n"
  , "    .stat {\n"
  , "      text-align: center;\n"
  , "      padding: 1rem;\n"
  , "      background: var(--bg-card);\n"
  , "      border-radius: 6px;\n"
  , "    }\n"
  , "    .stat-value { font-size: 2rem; font-weight: bold; }\n"
  , "    .stat-label { color: var(--text-secondary); font-size: 0.9rem; }\n"
  , "    .group {\n"
  , "      margin-bottom: 2rem;\n"
  , "    }\n"
  , "    .group-header {\n"
  , "      font-size: 1.2rem;\n"
  , "      font-weight: 600;\n"
  , "      padding: 0.75rem 1rem;\n"
  , "      background: var(--bg-secondary);\n"
  , "      border-radius: 6px 6px 0 0;\n"
  , "      border-bottom: 1px solid var(--border);\n"
  , "      cursor: pointer;\n"
  , "      user-select: none;\n"
  , "      display: flex;\n"
  , "      justify-content: space-between;\n"
  , "      align-items: center;\n"
  , "    }\n"
  , "    .group-header:hover { background: var(--bg-card); }\n"
  , "    .group-header .toggle { transition: transform 0.2s; }\n"
  , "    .group.collapsed .group-header .toggle { transform: rotate(-90deg); }\n"
  , "    .group.collapsed .group-content { display: none; }\n"
  , "    .diagnostic {\n"
  , "      padding: 1rem;\n"
  , "      background: var(--bg-card);\n"
  , "      border-bottom: 1px solid var(--border);\n"
  , "    }\n"
  , "    .diagnostic.hidden { display: none; }\n"
  , "    .diagnostic:last-child {\n"
  , "      border-radius: 0 0 6px 6px;\n"
  , "      border-bottom: none;\n"
  , "    }\n"
  , "    .diag-header {\n"
  , "      display: flex;\n"
  , "      align-items: center;\n"
  , "      gap: 0.75rem;\n"
  , "      margin-bottom: 0.5rem;\n"
  , "    }\n"
  , "    .severity {\n"
  , "      padding: 0.2rem 0.6rem;\n"
  , "      border-radius: 4px;\n"
  , "      font-size: 0.8rem;\n"
  , "      font-weight: 600;\n"
  , "      text-transform: uppercase;\n"
  , "    }\n"
  , "    .severity-error { background: var(--error); color: white; }\n"
  , "    .severity-warning { background: var(--warning); color: black; }\n"
  , "    .severity-suggestion { background: var(--suggestion); color: white; }\n"
  , "    .severity-info { background: var(--info); color: white; }\n"
  , "    .location {\n"
  , "      font-family: 'Fira Code', 'Consolas', monospace;\n"
  , "      font-size: 0.85rem;\n"
  , "      color: var(--text-secondary);\n"
  , "    }\n"
  , "    .code {\n"
  , "      font-family: 'Fira Code', 'Consolas', monospace;\n"
  , "      font-size: 0.8rem;\n"
  , "      padding: 0.2rem 0.4rem;\n"
  , "      background: var(--bg-secondary);\n"
  , "      border-radius: 3px;\n"
  , "    }\n"
  , "    .message { margin: 0.5rem 0; }\n"
  , "    .fix {\n"
  , "      color: var(--success);\n"
  , "      font-size: 0.85rem;\n"
  , "      margin-top: 0.5rem;\n"
  , "      cursor: pointer;\n"
  , "    }\n"
  , "    .fix:hover { text-decoration: underline; }\n"
  , "    .fix-preview {\n"
  , "      margin-top: 0.75rem;\n"
  , "      padding: 0.75rem;\n"
  , "      background: var(--bg-secondary);\n"
  , "      border-radius: 4px;\n"
  , "      border-left: 3px solid var(--success);\n"
  , "      font-family: 'Fira Code', 'Consolas', monospace;\n"
  , "      font-size: 0.85rem;\n"
  , "      white-space: pre-wrap;\n"
  , "      display: none;\n"
  , "    }\n"
  , "    .fix-preview.visible { display: block; }\n"
  , "    .no-issues {\n"
  , "      text-align: center;\n"
  , "      padding: 3rem;\n"
  , "      background: var(--bg-secondary);\n"
  , "      border-radius: 8px;\n"
  , "      color: var(--success);\n"
  , "    }\n"
  , "    footer {\n"
  , "      margin-top: 2rem;\n"
  , "      text-align: center;\n"
  , "      color: var(--text-secondary);\n"
  , "      font-size: 0.85rem;\n"
  , "    }\n"
  , "    .visible-count {\n"
  , "      color: var(--text-secondary);\n"
  , "      font-size: 0.9rem;\n"
  , "      margin-left: auto;\n"
  , "    }\n"
  , "  </style>\n"
  ]

renderTitle :: Text
renderTitle = "<div class=\"container\">\n<h1>Argus Report</h1>\n"

renderControls :: Text
renderControls = T.concat
  [ "<div class=\"controls\">\n"
  , "  <label>Filter by severity:</label>\n"
  , "  <button class=\"filter-btn active\" data-severity=\"all\">All</button>\n"
  , "  <button class=\"filter-btn\" data-severity=\"error\">Errors</button>\n"
  , "  <button class=\"filter-btn\" data-severity=\"warning\">Warnings</button>\n"
  , "  <button class=\"filter-btn\" data-severity=\"suggestion\">Suggestions</button>\n"
  , "  <button class=\"filter-btn\" data-severity=\"info\">Info</button>\n"
  , "  <input type=\"text\" class=\"search-input\" placeholder=\"Search messages...\" id=\"search-input\">\n"
  , "  <span class=\"visible-count\" id=\"visible-count\"></span>\n"
  , "</div>\n"
  ]

renderSummaryHtml :: Summary -> Text
renderSummaryHtml Summary{..} = T.concat
  [ "<div class=\"summary\">\n"
  , statBox (T.pack $ show sumTotalFiles) "Files Analyzed"
  , statBox (T.pack $ show sumFilesWithIssues) "Files with Issues"
  , statBox (T.pack $ show sumTotalDiagnostics) "Total Diagnostics"
  , statBox (T.pack $ show sumFixesAvailable) "Fixes Available"
  , "</div>\n"
  ]
  where
    statBox value label = T.concat
      [ "  <div class=\"stat\">\n"
      , "    <div class=\"stat-value\">", value, "</div>\n"
      , "    <div class=\"stat-label\">", label, "</div>\n"
      , "  </div>\n"
      ]

renderDiagnosticsHtml :: [(Text, [(FilePath, Diagnostic)])] -> Text
renderDiagnosticsHtml [] = "<div class=\"no-issues\">No issues found!</div>\n"
renderDiagnosticsHtml groups = T.concat $ map renderGroup groups
  where
    renderGroup (header, diags) = T.concat
      [ "<div class=\"group\">\n"
      , "  <div class=\"group-header\" onclick=\"toggleGroup(this.parentElement)\">"
      , "    <span>", escapeHtml header
      , " (", T.pack $ show $ length diags, " issue", plural (length diags), ")</span>"
      , "    <span class=\"toggle\">▼</span>"
      , "  </div>\n"
      , "  <div class=\"group-content\">\n"
      , T.concat $ map (renderDiagnosticHtml . snd) diags
      , "  </div>\n"
      , "</div>\n"
      ]

    plural 1 = ""
    plural _ = "s"

renderDiagnosticHtml :: Diagnostic -> Text
renderDiagnosticHtml diag = T.concat
  [ "  <div class=\"diagnostic\" data-severity=\"", severityData (diagSeverity diag), "\">\n"
  , "    <div class=\"diag-header\">\n"
  , "      <span class=\"severity ", severityClass (diagSeverity diag), "\">", severityText (diagSeverity diag), "</span>\n"
  , "      <span class=\"location\">", escapeHtml $ renderSpan (diagSpan diag), "</span>\n"
  , case diagCode diag of
      Just code -> "      <span class=\"code\">" <> escapeHtml code <> "</span>\n"
      Nothing   -> ""
  , "    </div>\n"
  , "    <div class=\"message\">", escapeHtml (diagMessage diag), "</div>\n"
  , renderFixInfo (diagFixes diag)
  , "  </div>\n"
  ]

-- | Render fix information with preview
renderFixInfo :: [Fix] -> Text
renderFixInfo [] = ""
renderFixInfo fixes = T.concat
  [ "    <div class=\"fix\" onclick=\"toggleFix(this)\">Fix available (click to preview)</div>\n"
  , "    <div class=\"fix-preview\">\n"
  , T.concat $ map renderFix fixes
  , "    </div>\n"
  ]
  where
    renderFix fix = T.concat
      [ "      <div><strong>", escapeHtml (fixTitle fix), "</strong></div>\n"
      , T.concat $ map renderEdit (fixEdits fix)
      ]
    renderEdit edit = T.concat
      [ "      <div>Replace with: <code>", escapeHtml (fixEditNewText edit), "</code></div>\n"
      ]

-- | Get severity as lowercase data attribute value
severityData :: Severity -> Text
severityData Error      = "error"
severityData Warning    = "warning"
severityData Suggestion = "suggestion"
severityData Info       = "info"

severityClass :: Severity -> Text
severityClass Error      = "severity-error"
severityClass Warning    = "severity-warning"
severityClass Suggestion = "severity-suggestion"
severityClass Info       = "severity-info"

severityText :: Severity -> Text
severityText Error      = "Error"
severityText Warning    = "Warning"
severityText Suggestion = "Suggestion"
severityText Info       = "Info"

renderSpan :: SrcSpan -> Text
renderSpan srcSpan = T.concat
  [ T.pack (srcSpanFile srcSpan)
  , ":"
  , T.pack $ show (srcSpanStartLineRaw srcSpan)
  , ":"
  , T.pack $ show (srcSpanStartColRaw srcSpan)
  ]

htmlFooter :: Text
htmlFooter = T.concat
  [ "</div>\n"
  , "<footer>Generated by Argus</footer>\n"
  , "</body>\n"
  , "</html>\n"
  ]

-- | Escape HTML special characters
escapeHtml :: Text -> Text
escapeHtml = T.concatMap escape
  where
    escape '<'  = "&lt;"
    escape '>'  = "&gt;"
    escape '&'  = "&amp;"
    escape '"'  = "&quot;"
    escape '\'' = "&#39;"
    escape c    = T.singleton c

-- | JavaScript for interactivity
jsScript :: Text
jsScript = T.concat
  [ "<script>\n"
  , "// Toggle group collapse\n"
  , "function toggleGroup(group) {\n"
  , "  group.classList.toggle('collapsed');\n"
  , "}\n"
  , "\n"
  , "// Toggle fix preview\n"
  , "function toggleFix(fixEl) {\n"
  , "  const preview = fixEl.nextElementSibling;\n"
  , "  if (preview) preview.classList.toggle('visible');\n"
  , "}\n"
  , "\n"
  , "// Filter by severity\n"
  , "let activeSeverity = 'all';\n"
  , "let searchText = '';\n"
  , "\n"
  , "function filterDiagnostics() {\n"
  , "  const diagnostics = document.querySelectorAll('.diagnostic');\n"
  , "  let visibleCount = 0;\n"
  , "  diagnostics.forEach(d => {\n"
  , "    const severity = d.getAttribute('data-severity');\n"
  , "    const message = d.querySelector('.message').textContent.toLowerCase();\n"
  , "    const code = d.querySelector('.code')?.textContent.toLowerCase() || '';\n"
  , "    const matchesSeverity = activeSeverity === 'all' || severity === activeSeverity;\n"
  , "    const matchesSearch = searchText === '' || message.includes(searchText) || code.includes(searchText);\n"
  , "    if (matchesSeverity && matchesSearch) {\n"
  , "      d.classList.remove('hidden');\n"
  , "      visibleCount++;\n"
  , "    } else {\n"
  , "      d.classList.add('hidden');\n"
  , "    }\n"
  , "  });\n"
  , "  document.getElementById('visible-count').textContent = \n"
  , "    `Showing ${visibleCount} of ${diagnostics.length} issues`;\n"
  , "}\n"
  , "\n"
  , "// Setup filter buttons\n"
  , "document.querySelectorAll('.filter-btn').forEach(btn => {\n"
  , "  btn.addEventListener('click', () => {\n"
  , "    document.querySelectorAll('.filter-btn').forEach(b => {\n"
  , "      b.classList.remove('active', 'active-error', 'active-warning', 'active-suggestion', 'active-info');\n"
  , "    });\n"
  , "    const sev = btn.getAttribute('data-severity');\n"
  , "    activeSeverity = sev;\n"
  , "    btn.classList.add('active');\n"
  , "    if (sev !== 'all') btn.classList.add('active-' + sev);\n"
  , "    filterDiagnostics();\n"
  , "  });\n"
  , "});\n"
  , "\n"
  , "// Setup search\n"
  , "document.getElementById('search-input').addEventListener('input', (e) => {\n"
  , "  searchText = e.target.value.toLowerCase();\n"
  , "  filterDiagnostics();\n"
  , "});\n"
  , "\n"
  , "// Initial count\n"
  , "filterDiagnostics();\n"
  , "</script>\n"
  ]
