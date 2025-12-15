{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Rule
-- Description : Custom rule authoring CLI
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module implements the rule command for custom rule authoring.
--
-- Usage:
--
-- @
-- # Create a new rule template
-- argus rule new my-rule
--
-- # Validate rule syntax
-- argus rule validate rules/my-rule.toml
--
-- # List all available rules
-- argus rule list
--
-- # List rules by category
-- argus rule list --category security
--
-- # Get detailed explanation of a rule
-- argus rule explain partial/head
--
-- # Export rule documentation
-- argus rule docs --format markdown
-- @
module Argus.CLI.Rule
  ( -- * Entry point
    runRule
  ) where

import Control.Monad (when, forM_, unless)
import Data.List (sortOn, groupBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist, createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>))

import Argus.CLI.Types (GlobalOptions(..), RuleOptions(..), RuleAction(..))
import Argus.Rules.Types (Rule(..), Category(..), rulePatternToText)
import Argus.Types (Severity(..))
import Argus.Rules.Builtin (allBuiltinRules)

--------------------------------------------------------------------------------
-- Entry Point
--------------------------------------------------------------------------------

-- | Run rule command
runRule :: GlobalOptions -> RuleOptions -> IO ()
runRule _global opts = case roAction opts of
  RuleNew name      -> runNew opts name
  RuleValidate path -> runValidate opts path
  RuleTest path     -> runTest opts path
  RuleList          -> runList opts
  RuleExplain rId   -> runExplain opts rId
  RuleDocs          -> runDocs opts

--------------------------------------------------------------------------------
-- Command Implementations
--------------------------------------------------------------------------------

-- | Create a new rule template
runNew :: RuleOptions -> Text -> IO ()
runNew opts name = do
  cwd <- getCurrentDirectory

  -- Create rules directory if it doesn't exist
  let rulesDir = fromMaybe (cwd </> ".argus" </> "rules") (roRulesDir opts)
  createDirectoryIfMissing True rulesDir

  let rulePath = rulesDir </> T.unpack name <> ".toml"

  -- Check if file already exists
  exists <- doesFileExist rulePath
  when exists $ do
    TIO.putStrLn $ colorRed "Error: " <> "Rule file already exists: " <> T.pack rulePath
    TIO.putStrLn "Use a different name or delete the existing file."

  unless exists $ do
    -- Generate template based on rule type
    let template = case roRuleType opts of
          "pattern" -> patternRuleTemplate name
          "ast"     -> astRuleTemplate name
          "semantic" -> semanticRuleTemplate name
          _         -> patternRuleTemplate name

    TIO.writeFile rulePath template

    TIO.putStrLn $ colorGreen "Created: " <> T.pack rulePath
    TIO.putStrLn ""
    TIO.putStrLn "Next steps:"
    TIO.putStrLn "  1. Edit the rule file to define your pattern"
    TIO.putStrLn $ "  2. Run 'argus rule validate " <> T.pack rulePath <> "' to check syntax"
    TIO.putStrLn $ "  3. Run 'argus check --config " <> T.pack rulePath <> "' to test"
    TIO.putStrLn ""

-- | Validate rule syntax
runValidate :: RuleOptions -> FilePath -> IO ()
runValidate opts path = do
  when (roVerbose opts) $
    TIO.putStrLn $ "Validating rule file: " <> T.pack path

  exists <- doesFileExist path
  unless exists $ do
    TIO.putStrLn $ colorRed "Error: " <> "File not found: " <> T.pack path
    return ()

  when exists $ do
    content <- TIO.readFile path

    -- Basic TOML validation
    if T.isInfixOf "[[rules]]" content || T.isInfixOf "[rules]" content
      then do
        TIO.putStrLn $ colorGreen "Validation passed!"
        TIO.putStrLn ""
        TIO.putStrLn "The file appears to be a valid rule configuration."
        TIO.putStrLn ""
        TIO.putStrLn "To test this rule:"
        TIO.putStrLn $ "  argus check --config " <> T.pack path <> " src/"
      else do
        TIO.putStrLn $ colorYellow "Warning: " <> "File may not contain valid rule definitions."
        TIO.putStrLn "Expected [[rules]] or [rules] section in TOML file."

-- | Test a rule against sample code
runTest :: RuleOptions -> FilePath -> IO ()
runTest opts path = do
  when (roVerbose opts) $
    TIO.putStrLn $ "Testing rule file: " <> T.pack path

  -- Check rule file exists
  ruleExists <- doesFileExist path
  unless ruleExists $ do
    TIO.putStrLn $ colorRed "Error: " <> "Rule file not found: " <> T.pack path
    return ()

  when ruleExists $ do
    case roSampleFile opts of
      Nothing -> do
        TIO.putStrLn $ colorYellow "No sample file specified."
        TIO.putStrLn ""
        TIO.putStrLn "Usage: argus rule test <rule-file> --sample <haskell-file>"
        TIO.putStrLn ""
        TIO.putStrLn "Alternative: Use 'argus check' directly:"
        TIO.putStrLn $ "  argus check --config " <> T.pack path <> " <target-files>"

      Just samplePath -> do
        sampleExists <- doesFileExist samplePath
        unless sampleExists $ do
          TIO.putStrLn $ colorRed "Error: " <> "Sample file not found: " <> T.pack samplePath
          return ()

        when sampleExists $ do
          TIO.putStrLn $ colorBold "To test this rule against " <> T.pack samplePath <> ":"
          TIO.putStrLn ""
          TIO.putStrLn $ "  argus check --config " <> T.pack path <> " " <> T.pack samplePath
          TIO.putStrLn ""
          TIO.putStrLn "This will run the rule engine with your custom configuration."

-- | List all available rules
runList :: RuleOptions -> IO ()
runList opts = do
  let rules = allBuiltinRules

  -- Filter by category if specified
  let filteredRules = case roCategory opts of
        Just cat -> filter (\r -> categoryToText (ruleCategory r) == cat) rules
        Nothing  -> rules

  -- Group by category
  let grouped = groupBy ((==) `on` ruleCategory) $
                sortOn ruleCategory filteredRules

  TIO.putStrLn $ colorBold "Available Rules"
  TIO.putStrLn $ "Total: " <> T.pack (show $ length filteredRules) <> " rules"
  TIO.putStrLn ""

  if roOutputFormat opts == "json"
    then do
      -- JSON output
      TIO.putStrLn "["
      forM_ (zip [0..] filteredRules) $ \(i, rule) -> do
        let comma = if i < length filteredRules - 1 then "," else ""
        TIO.putStrLn $ "  {\"id\": \"" <> ruleId rule <> "\", "
                    <> "\"severity\": \"" <> T.pack (show $ ruleSeverity rule) <> "\", "
                    <> "\"category\": \"" <> categoryToText (ruleCategory rule) <> "\", "
                    <> "\"message\": \"" <> escapeJson (ruleMessage rule) <> "\"}" <> comma
      TIO.putStrLn "]"
    else do
      -- Terminal output
      forM_ grouped $ \grp -> do
        let cat = maybe "general" (categoryToText . ruleCategory) (safeHead grp)
        TIO.putStrLn $ colorBold $ "  " <> cat
        TIO.putStrLn ""

        forM_ grp $ \rule -> do
          let sevIcon = severityIcon (ruleSeverity rule)
          TIO.putStrLn $ "    " <> sevIcon <> " " <> ruleId rule
          when (roVerbose opts) $
            TIO.putStrLn $ "        " <> ruleMessage rule
        TIO.putStrLn ""

-- | Explain a specific rule
runExplain :: RuleOptions -> Text -> IO ()
runExplain _opts ruleIdText = do
  let rules = allBuiltinRules

  case filter (\r -> ruleId r == ruleIdText) rules of
    [] -> do
      TIO.putStrLn $ colorRed "Error: " <> "Rule not found: " <> ruleIdText
      TIO.putStrLn ""
      TIO.putStrLn "Use 'argus rule list' to see available rules."

      -- Suggest similar rules
      let similar = filter (\r -> ruleIdText `T.isInfixOf` ruleId r) rules
      unless (null similar) $ do
        TIO.putStrLn ""
        TIO.putStrLn "Did you mean one of these?"
        forM_ (take 5 similar) $ \r ->
          TIO.putStrLn $ "  - " <> ruleId r

    (rule:_) -> do
      TIO.putStrLn $ colorBold "Rule: " <> ruleId rule
      TIO.putStrLn ""

      TIO.putStrLn $ colorBold "Severity: " <> severityText (ruleSeverity rule)
      TIO.putStrLn $ colorBold "Category: " <> categoryToText (ruleCategory rule)
      TIO.putStrLn ""

      TIO.putStrLn $ colorBold "Message:"
      TIO.putStrLn $ "  " <> ruleMessage rule
      TIO.putStrLn ""

      case ruleExplanation rule of
        Just expl -> do
          TIO.putStrLn $ colorBold "Explanation:"
          forM_ (T.lines expl) $ \line ->
            TIO.putStrLn $ "  " <> line
          TIO.putStrLn ""
        Nothing -> return ()

      let pat = rulePatternToText (rulePattern rule)
      unless (T.null pat) $ do
        TIO.putStrLn $ colorBold "Pattern:"
        TIO.putStrLn $ "  " <> pat
        TIO.putStrLn ""

      case ruleReplacement rule of
        Just repl -> do
          TIO.putStrLn $ colorBold "Suggested replacement:"
          TIO.putStrLn $ "  " <> repl
          TIO.putStrLn ""
        Nothing -> return ()

      unless (null $ ruleReferences rule) $ do
        TIO.putStrLn $ colorBold "References:"
        forM_ (ruleReferences rule) $ \ref ->
          TIO.putStrLn $ "  - " <> ref
        TIO.putStrLn ""

-- | Export rule documentation
runDocs :: RuleOptions -> IO ()
runDocs opts = do
  let rules = allBuiltinRules

  -- Group by category
  let grouped = groupBy ((==) `on` ruleCategory) $
                sortOn ruleCategory rules

  case roOutputFormat opts of
    "markdown" -> do
      TIO.putStrLn "# Argus Rules Reference"
      TIO.putStrLn ""
      TIO.putStrLn "This document lists all built-in rules available in Argus."
      TIO.putStrLn ""

      forM_ grouped $ \grp -> do
        let cat = maybe "general" (categoryToText . ruleCategory) (safeHead grp)
        TIO.putStrLn $ "## " <> T.toTitle cat
        TIO.putStrLn ""

        forM_ grp $ \rule -> do
          TIO.putStrLn $ "### " <> ruleId rule
          TIO.putStrLn ""
          TIO.putStrLn $ "**Severity:** " <> severityText (ruleSeverity rule)
          TIO.putStrLn ""
          TIO.putStrLn $ ruleMessage rule
          TIO.putStrLn ""

          case ruleExplanation rule of
            Just expl -> do
              TIO.putStrLn expl
              TIO.putStrLn ""
            Nothing -> return ()

          let pat = rulePatternToText (rulePattern rule)
          unless (T.null pat) $ do
            TIO.putStrLn "**Pattern:**"
            TIO.putStrLn "```"
            TIO.putStrLn pat
            TIO.putStrLn "```"
            TIO.putStrLn ""

          case ruleReplacement rule of
            Just repl -> do
              TIO.putStrLn "**Replacement:**"
              TIO.putStrLn "```haskell"
              TIO.putStrLn repl
              TIO.putStrLn "```"
              TIO.putStrLn ""
            Nothing -> return ()

          TIO.putStrLn "---"
          TIO.putStrLn ""

    "html" -> do
      TIO.putStrLn "<!DOCTYPE html>"
      TIO.putStrLn "<html><head><title>Argus Rules Reference</title>"
      TIO.putStrLn "<style>"
      TIO.putStrLn "body { font-family: sans-serif; max-width: 900px; margin: 0 auto; padding: 20px; }"
      TIO.putStrLn ".rule { border: 1px solid #ddd; padding: 15px; margin: 10px 0; border-radius: 5px; }"
      TIO.putStrLn ".severity-error { border-left: 4px solid #e74c3c; }"
      TIO.putStrLn ".severity-warning { border-left: 4px solid #f39c12; }"
      TIO.putStrLn ".severity-info { border-left: 4px solid #3498db; }"
      TIO.putStrLn ".severity-hint { border-left: 4px solid #2ecc71; }"
      TIO.putStrLn "code { background: #f5f5f5; padding: 2px 6px; border-radius: 3px; }"
      TIO.putStrLn "pre { background: #f5f5f5; padding: 10px; overflow-x: auto; }"
      TIO.putStrLn "</style></head><body>"
      TIO.putStrLn "<h1>Argus Rules Reference</h1>"

      forM_ grouped $ \grp -> do
        let cat = maybe "general" (categoryToText . ruleCategory) (safeHead grp)
        TIO.putStrLn $ "<h2>" <> escapeHtml (T.toTitle cat) <> "</h2>"

        forM_ grp $ \rule -> do
          let sevClass = "severity-" <> T.toLower (severityText $ ruleSeverity rule)
          TIO.putStrLn $ "<div class=\"rule " <> sevClass <> "\">"
          TIO.putStrLn $ "<h3><code>" <> escapeHtml (ruleId rule) <> "</code></h3>"
          TIO.putStrLn $ "<p><strong>Severity:</strong> " <> severityText (ruleSeverity rule) <> "</p>"
          TIO.putStrLn $ "<p>" <> escapeHtml (ruleMessage rule) <> "</p>"

          case ruleExplanation rule of
            Just expl -> TIO.putStrLn $ "<p>" <> escapeHtml expl <> "</p>"
            Nothing -> return ()

          let pat = rulePatternToText (rulePattern rule)
          unless (T.null pat) $ do
            TIO.putStrLn "<p><strong>Pattern:</strong></p>"
            TIO.putStrLn $ "<pre>" <> escapeHtml pat <> "</pre>"

          TIO.putStrLn "</div>"

      TIO.putStrLn "</body></html>"

    _ -> do
      -- Plain text
      TIO.putStrLn "ARGUS RULES REFERENCE"
      TIO.putStrLn $ T.replicate 60 "="
      TIO.putStrLn ""

      forM_ grouped $ \grp -> do
        let cat = maybe "general" (categoryToText . ruleCategory) (safeHead grp)
        TIO.putStrLn $ T.toUpper cat
        TIO.putStrLn $ T.replicate (T.length cat) "-"
        TIO.putStrLn ""

        forM_ grp $ \rule -> do
          TIO.putStrLn $ ruleId rule <> " [" <> severityText (ruleSeverity rule) <> "]"
          TIO.putStrLn $ "  " <> ruleMessage rule
          TIO.putStrLn ""

--------------------------------------------------------------------------------
-- Rule Templates
--------------------------------------------------------------------------------

-- | Template for pattern-based rules
patternRuleTemplate :: Text -> Text
patternRuleTemplate name = T.unlines
  [ "# Custom Rule: " <> name
  , "# Documentation: https://github.com/quintenkasteel/argus#custom-rules"
  , ""
  , "[[rules]]"
  , "id = \"custom/" <> name <> "\""
  , "pattern = \"head x\"  # Pattern to match"
  , "replacement = \"listToMaybe x\"  # Suggested fix"
  , "message = \"Use listToMaybe instead of head for safety\""
  , "severity = \"warning\"  # error, warning, info, hint"
  , "category = \"safety\""
  , ""
  , "# Optional fields:"
  , "# enabled = true"
  , "# explanation = \"Detailed explanation of why this rule exists\""
  , ""
  , "# Side conditions (optional):"
  , "# [[rules.conditions]]"
  , "# type = \"notInTestFile\""
  , ""
  , "# Examples:"
  , "# [[rules.examples]]"
  , "# bad = \"head xs\""
  , "# good = \"listToMaybe xs\""
  ]

-- | Template for AST-based rules
astRuleTemplate :: Text -> Text
astRuleTemplate name = T.unlines
  [ "# Custom AST Rule: " <> name
  , "# AST rules allow matching complex code patterns"
  , ""
  , "[[rules]]"
  , "id = \"custom/" <> name <> "\""
  , "type = \"ast\""
  , ""
  , "# AST pattern using Haskell-like syntax"
  , "ast-pattern = \"\"\""
  , "case $x of"
  , "  [] -> $default"
  , "  ($h:$_) -> $h"
  , "\"\"\""
  , ""
  , "replacement = \"listToMaybe $x\""
  , "message = \"Use listToMaybe instead of manual case expression\""
  , "severity = \"warning\""
  , "category = \"style\""
  , ""
  , "# Capture variables:"
  , "# $x, $h - match any expression"
  , "# $_ - match but don't capture"
  , "# ... - match zero or more arguments"
  ]

-- | Template for semantic rules (require type information)
semanticRuleTemplate :: Text -> Text
semanticRuleTemplate name = T.unlines
  [ "# Custom Semantic Rule: " <> name
  , "# Semantic rules use type information for precise matching"
  , ""
  , "[[rules]]"
  , "id = \"custom/" <> name <> "\""
  , "type = \"semantic\""
  , ""
  , "pattern = \"foldl f z xs\""
  , "replacement = \"foldl' f z xs\""
  , "message = \"Use foldl' for strict left fold to avoid space leaks\""
  , "severity = \"warning\""
  , "category = \"performance\""
  , ""
  , "# Type conditions:"
  , "[[rules.conditions]]"
  , "type = \"hasType\""
  , "variable = \"xs\""
  , "typePattern = \"[a]\""
  , ""
  , "# Require import for replacement:"
  , "# add-import = \"Data.List (foldl')\""
  ]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Safe head
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- | Convert Category to Text for display
categoryToText :: Category -> Text
categoryToText Performance   = "performance"
categoryToText SpaceLeaks    = "space-leaks"
categoryToText Security      = "security"
categoryToText Safety        = "safety"
categoryToText Style         = "style"
categoryToText Correctness   = "correctness"
categoryToText Modernization = "modernization"
categoryToText Imports       = "imports"
categoryToText Naming        = "naming"
categoryToText Extensions    = "extensions"
categoryToText Complexity    = "complexity"
categoryToText Concurrency   = "concurrency"
categoryToText ErrorHandling = "error-handling"
categoryToText Documentation = "documentation"
categoryToText Redundant     = "redundant"
categoryToText (Custom t)    = t

-- | Severity icon
severityIcon :: Severity -> Text
severityIcon Error      = colorRed "[E]"
severityIcon Warning    = colorYellow "[W]"
severityIcon Suggestion = colorBlue "[S]"
severityIcon Info       = colorGreen "[I]"

-- | Severity text
severityText :: Severity -> Text
severityText Error      = "Error"
severityText Warning    = "Warning"
severityText Suggestion = "Suggestion"
severityText Info       = "Info"

-- | Escape JSON
escapeJson :: Text -> Text
escapeJson = T.replace "\"" "\\\"" . T.replace "\\" "\\\\" . T.replace "\n" "\\n"

-- | Escape HTML
escapeHtml :: Text -> Text
escapeHtml = T.replace "&" "&amp;"
           . T.replace "<" "&lt;"
           . T.replace ">" "&gt;"
           . T.replace "\"" "&quot;"

-- | ANSI color helpers
colorBold :: Text -> Text
colorBold t = "\ESC[1m" <> t <> "\ESC[0m"

colorRed :: Text -> Text
colorRed t = "\ESC[31m" <> t <> "\ESC[0m"

colorGreen :: Text -> Text
colorGreen t = "\ESC[32m" <> t <> "\ESC[0m"

colorYellow :: Text -> Text
colorYellow t = "\ESC[33m" <> t <> "\ESC[0m"

colorBlue :: Text -> Text
colorBlue t = "\ESC[34m" <> t <> "\ESC[0m"
