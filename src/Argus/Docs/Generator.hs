{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Docs.Generator
-- Description : API documentation generator for Argus rules and modules
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides automatic documentation generation for Argus rules,
-- generating comprehensive API docs in multiple formats (Markdown, HTML, JSON).
--
-- == Features
--
-- * Generate rule catalogs with examples
-- * Generate category hierarchies
-- * Export searchable rule indexes
-- * Create integration guides
-- * Produce changelog diffs
--
-- == Usage
--
-- @
-- docs <- generateDocumentation defaultDocConfig
-- writeMarkdown "docs/rules.md" docs
-- @
module Argus.Docs.Generator
  ( -- * Documentation Types
    DocConfig (..)
  , defaultDocConfig
  , DocBundle (..)
  , RuleDoc (..)
  , CategoryDoc (..)
  , ModuleDoc (..)
  , ExampleDoc (..)
  , ChangelogEntry (..)

    -- * Generation Functions
  , generateDocumentation
  , generateRuleCatalog
  , generateCategoryIndex
  , generateIntegrationGuide
  , generateChangelog

    -- * Output Formats
  , renderMarkdown
  , renderHtml
  , renderJson
  , renderManPage

    -- * Rule Extraction
  , extractRuleDoc
  , extractCategoryDoc
  , groupByCategory

    -- * Index Generation
  , SearchIndex (..)
  , SearchEntry (..)
  , buildSearchIndex
  , searchRules
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON(..), FromJSON(..), encode)
import Data.ByteString.Lazy qualified as BL
import Data.Char (toUpper)
import Data.List (sortOn, nub)
import Data.Maybe (isJust)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)

import Argus.Rules.Types
  ( Rule(..)
  , RulePattern(..)
  , SideCondition(..)
  , Category(..)
  , SafetyLevel(..)
  )
import Argus.Types (Severity(..))

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Documentation generation configuration
data DocConfig = DocConfig
  { dcTitle           :: Text           -- ^ Documentation title
  , dcVersion         :: Text           -- ^ Argus version
  , dcIncludeExamples :: Bool           -- ^ Include code examples
  , dcIncludeFixes    :: Bool           -- ^ Include fix information
  , dcIncludeStats    :: Bool           -- ^ Include rule statistics
  , dcBaseUrl         :: Maybe Text     -- ^ Base URL for links
  , dcOutputDir       :: FilePath       -- ^ Output directory
  , dcFormat          :: DocFormat      -- ^ Output format
  , dcCategories      :: [Category]     -- ^ Categories to include (empty = all)
  , dcSeverities      :: [Severity]     -- ^ Severities to include (empty = all)
  , dcShowDeprecated  :: Bool           -- ^ Show deprecated rules
  , dcTableOfContents :: Bool           -- ^ Generate table of contents
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Documentation output format
data DocFormat
  = FormatMarkdown
  | FormatHtml
  | FormatJson
  | FormatManPage
  | FormatRst          -- ^ reStructuredText
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Default documentation configuration
defaultDocConfig :: DocConfig
defaultDocConfig = DocConfig
  { dcTitle = "Argus Rule Documentation"
  , dcVersion = "1.0.0"
  , dcIncludeExamples = True
  , dcIncludeFixes = True
  , dcIncludeStats = True
  , dcBaseUrl = Nothing
  , dcOutputDir = "docs"
  , dcFormat = FormatMarkdown
  , dcCategories = []
  , dcSeverities = []
  , dcShowDeprecated = False
  , dcTableOfContents = True
  }

--------------------------------------------------------------------------------
-- Documentation Types
--------------------------------------------------------------------------------

-- | Complete documentation bundle
data DocBundle = DocBundle
  { docTitle       :: Text              -- ^ Document title
  , docVersion     :: Text              -- ^ Version
  , docGenerated   :: UTCTime           -- ^ Generation timestamp
  , docCategories  :: [CategoryDoc]     -- ^ Category documentation
  , docRules       :: [RuleDoc]         -- ^ All rule documentation
  , docModules     :: [ModuleDoc]       -- ^ Module documentation
  , docStats       :: DocStats          -- ^ Statistics
  , docChangelog   :: [ChangelogEntry]  -- ^ Recent changes
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Documentation for a single rule
data RuleDoc = RuleDoc
  { rdId          :: Text               -- ^ Rule ID
  , rdName        :: Text               -- ^ Human-readable name
  , rdDescription :: Text               -- ^ Full description
  , rdCategory    :: Category           -- ^ Rule category
  , rdSeverity    :: Severity           -- ^ Default severity
  , rdEnabled     :: Bool               -- ^ Enabled by default
  , rdPattern     :: Text               -- ^ Pattern description
  , rdPatternType :: Text               -- ^ Pattern type (text, regex, AST)
  , rdConditions  :: [Text]             -- ^ Side conditions
  , rdHasFix      :: Bool               -- ^ Has auto-fix
  , rdFixSafety   :: Maybe SafetyLevel  -- ^ Fix safety level
  , rdExamples    :: [ExampleDoc]       -- ^ Usage examples
  , rdSeeAlso     :: [Text]             -- ^ Related rules
  , rdSince       :: Maybe Text         -- ^ Version introduced
  , rdDeprecated  :: Maybe Text         -- ^ Deprecation message
  , rdTags        :: [Text]             -- ^ Tags for searching
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Documentation for a category
data CategoryDoc = CategoryDoc
  { cdName        :: Text               -- ^ Category name
  , cdDescription :: Text               -- ^ Category description
  , cdIcon        :: Maybe Text         -- ^ Icon (emoji or icon name)
  , cdRuleCount   :: Int                -- ^ Number of rules
  , cdRuleIds     :: [Text]             -- ^ Rule IDs in this category
  , cdSubcategories :: [CategoryDoc]    -- ^ Subcategories
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Documentation for a module
data ModuleDoc = ModuleDoc
  { mdName        :: Text               -- ^ Module name
  , mdSynopsis    :: Text               -- ^ One-line description
  , mdDescription :: Text               -- ^ Full description
  , mdExports     :: [Text]             -- ^ Exported items
  , mdDependencies :: [Text]            -- ^ Module dependencies
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Example for documentation
data ExampleDoc = ExampleDoc
  { exTitle       :: Text               -- ^ Example title
  , exBefore      :: Text               -- ^ Code before (problematic)
  , exAfter       :: Maybe Text         -- ^ Code after (fixed)
  , exExplanation :: Text               -- ^ Explanation
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Documentation statistics
data DocStats = DocStats
  { dsRuleCount       :: Int            -- ^ Total rules
  , dsByCategory      :: Map Text Int   -- ^ Rules per category
  , dsBySeverity      :: Map Text Int   -- ^ Rules per severity
  , dsWithFixes       :: Int            -- ^ Rules with auto-fix
  , dsEnabledDefault  :: Int            -- ^ Enabled by default
  , dsDeprecated      :: Int            -- ^ Deprecated rules
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Changelog entry
data ChangelogEntry = ChangelogEntry
  { ceVersion     :: Text               -- ^ Version
  , ceDate        :: Text               -- ^ Release date
  , ceAdded       :: [Text]             -- ^ Added rules
  , ceChanged     :: [Text]             -- ^ Changed rules
  , ceRemoved     :: [Text]             -- ^ Removed rules
  , ceFixed       :: [Text]             -- ^ Fixed rules
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Generation Functions
--------------------------------------------------------------------------------

-- | Generate complete documentation from rules
generateDocumentation :: DocConfig -> [Rule] -> IO DocBundle
generateDocumentation config rules = do
  now <- getCurrentTime

  let filteredRules = filterRules config rules
      ruleDocs = map extractRuleDoc filteredRules
      categoryDocs = extractCategoryDocs filteredRules
      stats = computeStats filteredRules

  pure DocBundle
    { docTitle = dcTitle config
    , docVersion = dcVersion config
    , docGenerated = now
    , docCategories = categoryDocs
    , docRules = ruleDocs
    , docModules = []  -- Would be populated from Haddock or source analysis
    , docStats = stats
    , docChangelog = []  -- Would be populated from CHANGELOG
    }

-- | Filter rules based on config
filterRules :: DocConfig -> [Rule] -> [Rule]
filterRules DocConfig{..} rules = filter matches rules
  where
    matches rule =
      (null dcCategories || ruleCategory rule `elem` dcCategories) &&
      (null dcSeverities || ruleSeverity rule `elem` dcSeverities) &&
      (dcShowDeprecated || not (isDeprecated rule))

    isDeprecated Rule{..} = case ruleDeprecated of
      Just _ -> True
      Nothing -> False

-- | Generate rule catalog (markdown list of all rules)
generateRuleCatalog :: DocConfig -> [Rule] -> Text
generateRuleCatalog config rules =
  let doc = generateDocumentation' config rules
  in renderRuleCatalogMarkdown doc

-- | Helper for sync generation
generateDocumentation' :: DocConfig -> [Rule] -> DocBundle
generateDocumentation' config rules = DocBundle
  { docTitle = dcTitle config
  , docVersion = dcVersion config
  , docGenerated = zeroTime
  , docCategories = extractCategoryDocs rules
  , docRules = map extractRuleDoc rules
  , docModules = []
  , docStats = computeStats rules
  , docChangelog = []
  }
  where
    zeroTime = read "2024-01-01 00:00:00 UTC"

-- | Generate category index
generateCategoryIndex :: [Rule] -> Text
generateCategoryIndex rules =
  let grouped = groupByCategory rules
      categoryLines = map renderCategoryLine (Map.toList grouped)
  in T.unlines $
       [ "# Rule Categories"
       , ""
       , "| Category | Rules | Description |"
       , "|----------|-------|-------------|"
       ] ++ categoryLines
  where
    renderCategoryLine (cat, rs) = T.concat
      [ "| ", categoryName cat, " | ", T.pack (show (length rs)), " | "
      , categoryDescription cat, " |"
      ]

-- | Generate integration guide
generateIntegrationGuide :: DocConfig -> Text
generateIntegrationGuide DocConfig{..} = T.unlines
  [ "# Argus Integration Guide"
  , ""
  , "Version: " <> dcVersion
  , ""
  , "## Quick Start"
  , ""
  , "### Installation"
  , ""
  , "```bash"
  , "# Using Stack"
  , "stack install argus"
  , ""
  , "# Using Cabal"
  , "cabal install argus"
  , "```"
  , ""
  , "### Basic Usage"
  , ""
  , "```bash"
  , "# Analyze a project"
  , "argus check src/"
  , ""
  , "# Auto-fix issues"
  , "argus fix src/"
  , ""
  , "# Watch mode"
  , "argus watch src/"
  , "```"
  , ""
  , "## Configuration"
  , ""
  , "Create an `argus.toml` in your project root:"
  , ""
  , "```toml"
  , "[general]"
  , "directories = [\"src\", \"app\", \"test\"]"
  , "exclude = [\"dist-newstyle\", \".stack-work\"]"
  , ""
  , "[rules]"
  , "# Set severity levels"
  , "\"partial/*\" = \"error\""
  , "\"style/*\" = \"suggestion\""
  , ""
  , "# Disable specific rules"
  , "\"naming/unused-variable\" = \"off\""
  , "```"
  , ""
  , "## IDE Integration"
  , ""
  , "### VS Code"
  , ""
  , "Install the Argus extension and add to settings.json:"
  , ""
  , "```json"
  , "{"
  , "  \"argus.enable\": true,"
  , "  \"argus.autoFix\": true"
  , "}"
  , "```"
  , ""
  , "### Vim/Neovim"
  , ""
  , "Add to your LSP configuration:"
  , ""
  , "```lua"
  , "require('lspconfig').argus.setup{}"
  , "```"
  , ""
  , "## CI/CD Integration"
  , ""
  , "### GitHub Actions"
  , ""
  , "```yaml"
  , "- name: Run Argus"
  , "  uses: argus-haskell/argus-action@v1"
  , "  with:"
  , "    path: src/"
  , "    format: sarif"
  , "```"
  , ""
  , "### GitLab CI"
  , ""
  , "```yaml"
  , "argus:"
  , "  script:"
  , "    - argus check src/ --format=codeclimate > gl-code-quality.json"
  , "  artifacts:"
  , "    reports:"
  , "      codequality: gl-code-quality.json"
  , "```"
  ]

-- | Generate changelog from version history
generateChangelog :: [(Text, [ChangelogEntry])] -> Text
generateChangelog versions = T.unlines $
  ["# Changelog", ""] ++
  concatMap renderVersion versions
  where
    renderVersion (version, entries) =
      [ "## " <> version
      , ""
      ] ++ concatMap renderEntry entries ++ [""]

    renderEntry ChangelogEntry{..} = concat
      [ if null ceAdded then [] else
          ["### Added", ""] ++ map ("- " <>) ceAdded ++ [""]
      , if null ceChanged then [] else
          ["### Changed", ""] ++ map ("- " <>) ceChanged ++ [""]
      , if null ceRemoved then [] else
          ["### Removed", ""] ++ map ("- " <>) ceRemoved ++ [""]
      , if null ceFixed then [] else
          ["### Fixed", ""] ++ map ("- " <>) ceFixed ++ [""]
      ]

--------------------------------------------------------------------------------
-- Output Formats
--------------------------------------------------------------------------------

-- | Render documentation as Markdown
renderMarkdown :: DocBundle -> Text
renderMarkdown DocBundle{..} = TL.toStrict $ TB.toLazyText $ mconcat
  [ TB.fromText "# " <> TB.fromText docTitle <> TB.fromText "\n\n"
  , TB.fromText "Version: " <> TB.fromText docVersion <> TB.fromText "\n\n"

  -- Table of contents
  , TB.fromText "## Table of Contents\n\n"
  , mconcat [TB.fromText $ "- [" <> cdName cd <> "](#" <> slugify (cdName cd) <> ")\n"
            | cd <- docCategories]
  , TB.fromText "\n"

  -- Statistics
  , TB.fromText "## Statistics\n\n"
  , TB.fromText $ "- Total rules: " <> T.pack (show (dsRuleCount docStats)) <> "\n"
  , TB.fromText $ "- Rules with auto-fix: " <> T.pack (show (dsWithFixes docStats)) <> "\n"
  , TB.fromText $ "- Enabled by default: " <> T.pack (show (dsEnabledDefault docStats)) <> "\n\n"

  -- Categories
  , mconcat $ map renderCategoryMarkdown docCategories

  -- All rules
  , TB.fromText "## All Rules\n\n"
  , mconcat $ map renderRuleMarkdown docRules
  ]

-- | Render a category in Markdown
renderCategoryMarkdown :: CategoryDoc -> TB.Builder
renderCategoryMarkdown CategoryDoc{..} = mconcat
  [ TB.fromText "## " <> TB.fromText cdName <> TB.fromText "\n\n"
  , TB.fromText cdDescription <> TB.fromText "\n\n"
  , TB.fromText $ "Rules in this category: " <> T.pack (show cdRuleCount) <> "\n\n"
  , TB.fromText "| Rule ID | Description |\n"
  , TB.fromText "|---------|-------------|\n"
  , mconcat [TB.fromText $ "| `" <> rid <> "` | ... |\n" | rid <- cdRuleIds]
  , TB.fromText "\n"
  ]

-- | Render a rule in Markdown
renderRuleMarkdown :: RuleDoc -> TB.Builder
renderRuleMarkdown RuleDoc{..} = mconcat
  [ TB.fromText "### " <> TB.fromText rdId <> TB.fromText "\n\n"
  , TB.fromText rdDescription <> TB.fromText "\n\n"
  , TB.fromText "**Severity:** " <> TB.fromText (T.pack $ show rdSeverity) <> TB.fromText "\n\n"
  , TB.fromText "**Category:** " <> TB.fromText (T.pack $ show rdCategory) <> TB.fromText "\n\n"
  , TB.fromText "**Pattern:** `" <> TB.fromText rdPattern <> TB.fromText "` (" <> TB.fromText rdPatternType <> TB.fromText ")\n\n"
  , if rdHasFix
      then TB.fromText "**Auto-fix:** Yes" <> maybe "" (\s -> " (" <> TB.fromText (T.pack $ show s) <> ")") rdFixSafety <> TB.fromText "\n\n"
      else TB.fromText "**Auto-fix:** No\n\n"
  , if null rdExamples then mempty else
      TB.fromText "**Examples:**\n\n" <>
      mconcat (map renderExampleMarkdown rdExamples)
  , TB.fromText "---\n\n"
  ]

-- | Render an example in Markdown
renderExampleMarkdown :: ExampleDoc -> TB.Builder
renderExampleMarkdown ExampleDoc{..} = mconcat
  [ TB.fromText "#### " <> TB.fromText exTitle <> TB.fromText "\n\n"
  , TB.fromText "Before:\n```haskell\n" <> TB.fromText exBefore <> TB.fromText "\n```\n\n"
  , case exAfter of
      Just after -> TB.fromText "After:\n```haskell\n" <> TB.fromText after <> TB.fromText "\n```\n\n"
      Nothing -> mempty
  , TB.fromText exExplanation <> TB.fromText "\n\n"
  ]

-- | Render documentation as HTML
renderHtml :: DocBundle -> Text
renderHtml DocBundle{..} = T.unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "  <meta charset=\"UTF-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
  , "  <title>" <> docTitle <> "</title>"
  , "  <style>"
  , "    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; max-width: 1200px; margin: 0 auto; padding: 2rem; }"
  , "    .rule { border: 1px solid #ddd; border-radius: 8px; padding: 1rem; margin: 1rem 0; }"
  , "    .rule-id { font-family: monospace; background: #f5f5f5; padding: 0.2rem 0.5rem; border-radius: 4px; }"
  , "    .severity { display: inline-block; padding: 0.2rem 0.5rem; border-radius: 4px; font-size: 0.9rem; }"
  , "    .severity-error { background: #fee; color: #c00; }"
  , "    .severity-warning { background: #ffc; color: #990; }"
  , "    .severity-suggestion { background: #e8f5e9; color: #2e7d32; }"
  , "    .severity-info { background: #e3f2fd; color: #1565c0; }"
  , "    code { background: #f5f5f5; padding: 0.2rem 0.4rem; border-radius: 3px; }"
  , "    pre { background: #1e1e1e; color: #d4d4d4; padding: 1rem; border-radius: 8px; overflow-x: auto; }"
  , "    .stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 1rem; }"
  , "    .stat { background: #f9f9f9; padding: 1rem; border-radius: 8px; text-align: center; }"
  , "    .stat-value { font-size: 2rem; font-weight: bold; color: #333; }"
  , "    .stat-label { color: #666; }"
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <h1>" <> docTitle <> "</h1>"
  , "  <p>Version: " <> docVersion <> "</p>"
  , ""
  , "  <h2>Statistics</h2>"
  , "  <div class=\"stats\">"
  , "    <div class=\"stat\"><div class=\"stat-value\">" <> T.pack (show (dsRuleCount docStats)) <> "</div><div class=\"stat-label\">Total Rules</div></div>"
  , "    <div class=\"stat\"><div class=\"stat-value\">" <> T.pack (show (dsWithFixes docStats)) <> "</div><div class=\"stat-label\">With Auto-fix</div></div>"
  , "    <div class=\"stat\"><div class=\"stat-value\">" <> T.pack (show (dsEnabledDefault docStats)) <> "</div><div class=\"stat-label\">Enabled by Default</div></div>"
  , "  </div>"
  , ""
  , "  <h2>Categories</h2>"
  , T.unlines $ map renderCategoryHtml docCategories
  , ""
  , "  <h2>All Rules</h2>"
  , T.unlines $ map renderRuleHtml docRules
  , "</body>"
  , "</html>"
  ]

-- | Render category as HTML
renderCategoryHtml :: CategoryDoc -> Text
renderCategoryHtml CategoryDoc{..} = T.unlines
  [ "  <div class=\"category\">"
  , "    <h3>" <> cdName <> " <small>(" <> T.pack (show cdRuleCount) <> " rules)</small></h3>"
  , "    <p>" <> cdDescription <> "</p>"
  , "  </div>"
  ]

-- | Render rule as HTML
renderRuleHtml :: RuleDoc -> Text
renderRuleHtml RuleDoc{..} = T.unlines
  [ "  <div class=\"rule\" id=\"" <> slugify rdId <> "\">"
  , "    <h3><span class=\"rule-id\">" <> rdId <> "</span></h3>"
  , "    <p>" <> rdDescription <> "</p>"
  , "    <p><span class=\"severity severity-" <> T.toLower (T.pack $ show rdSeverity) <> "\">" <> T.pack (show rdSeverity) <> "</span></p>"
  , "    <p><strong>Pattern:</strong> <code>" <> rdPattern <> "</code></p>"
  , if rdHasFix then "    <p>✓ Auto-fix available</p>" else ""
  , "  </div>"
  ]

-- | Render documentation as JSON
renderJson :: DocBundle -> Text
renderJson doc = TE.decodeUtf8 $ BL.toStrict $ encode doc

-- | Render documentation as man page
renderManPage :: DocBundle -> Text
renderManPage DocBundle{..} = T.unlines
  [ ".TH ARGUS-RULES 7 \"\" \"Argus " <> docVersion <> "\" \"Argus Manual\""
  , ".SH NAME"
  , "argus-rules \\- Argus lint rules reference"
  , ".SH DESCRIPTION"
  , "This manual page describes the lint rules available in Argus."
  , ".SH CATEGORIES"
  , T.unlines $ map renderCategoryMan docCategories
  , ".SH RULES"
  , T.unlines $ map renderRuleMan docRules
  , ".SH SEE ALSO"
  , "argus(1), argus.toml(5)"
  ]

-- | Render category for man page
renderCategoryMan :: CategoryDoc -> Text
renderCategoryMan CategoryDoc{..} = T.unlines
  [ ".SS " <> cdName
  , cdDescription
  , "Rules: " <> T.intercalate ", " cdRuleIds
  ]

-- | Render rule for man page
renderRuleMan :: RuleDoc -> Text
renderRuleMan RuleDoc{..} = T.unlines
  [ ".TP"
  , ".B " <> rdId
  , rdDescription
  , ".br"
  , "Severity: " <> T.pack (show rdSeverity)
  , ".br"
  , "Pattern: " <> rdPattern
  ]

--------------------------------------------------------------------------------
-- Rule Extraction
--------------------------------------------------------------------------------

-- | Extract documentation from a rule
extractRuleDoc :: Rule -> RuleDoc
extractRuleDoc Rule{..} = RuleDoc
  { rdId = ruleId
  , rdName = ruleNameFromId ruleId
  , rdDescription = ruleMessage
  , rdCategory = ruleCategory
  , rdSeverity = ruleSeverity
  , rdEnabled = ruleEnabled
  , rdPattern = extractPatternText rulePattern
  , rdPatternType = patternTypeName rulePattern
  , rdConditions = map conditionName ruleConditions
  , rdHasFix = ruleReplacement /= Nothing
  , rdFixSafety = Just ruleSafety
  , rdExamples = []  -- Would need separate example storage
  , rdSeeAlso = []
  , rdSince = Nothing
  , rdDeprecated = ruleDeprecated
  , rdTags = ruleTags
  }

-- | Derive a human-readable name from a rule ID
-- e.g. "performance/length-null" -> "Length Null"
ruleNameFromId :: Text -> Text
ruleNameFromId rid =
  let name = case T.breakOnEnd "/" rid of
               (_, after) | not (T.null after) -> after
               _ -> rid
      words' = T.splitOn "-" name
      capitalize w = case T.uncons w of
        Just (c, rest) -> T.cons (toUpper c) rest
        Nothing -> w
  in T.unwords (map capitalize words')

-- | Extract pattern text
extractPatternText :: RulePattern -> Text
extractPatternText = \case
  TextPatternSpec t -> t
  RegexPatternSpec t -> t
  ASTPatternSpec t -> t

-- | Get pattern type name
patternTypeName :: RulePattern -> Text
patternTypeName = \case
  TextPatternSpec _ -> "text"
  RegexPatternSpec _ -> "regex"
  ASTPatternSpec _ -> "AST"

-- | Get condition name
conditionName :: SideCondition -> Text
conditionName = \case
  NotInComment -> "not-in-comment"
  NotInString -> "not-in-string"
  NotInImport -> "not-in-import"
  InFunctionBody -> "in-function-body"
  InCommentType _ -> "in-comment-type"
  InStringLiteral -> "in-string-literal"
  HasType var ty -> "has-type(" <> var <> ", " <> ty <> ")"
  HasTypeClass var tc -> "has-typeclass(" <> var <> ", " <> tc <> ")"
  TypeMatches var pat -> "type-matches(" <> var <> ", " <> pat <> ")"
  IsNumeric var -> "is-numeric(" <> var <> ")"
  IsString var -> "is-string(" <> var <> ")"
  IsList var -> "is-list(" <> var <> ")"
  IsMaybe var -> "is-maybe(" <> var <> ")"
  IsMonad var m -> "is-monad(" <> var <> ", " <> m <> ")"
  IsPure var -> "is-pure(" <> var <> ")"
  IsLiteral var -> "is-literal(" <> var <> ")"
  IsVariable var -> "is-variable(" <> var <> ")"
  IsApplication var -> "is-application(" <> var <> ")"
  IsLambda var -> "is-lambda(" <> var <> ")"
  IsAtomic var -> "is-atomic(" <> var <> ")"
  IsConstructor var -> "is-constructor(" <> var <> ")"
  NotEqual v1 v2 -> "not-equal(" <> v1 <> ", " <> v2 <> ")"
  FreeIn v1 v2 -> "free-in(" <> v1 <> ", " <> v2 <> ")"
  NotFreeIn v1 v2 -> "not-free-in(" <> v1 <> ", " <> v2 <> ")"
  ComplexityLT var n -> "complexity-lt(" <> var <> ", " <> T.pack (show n) <> ")"
  ComplexityGT var n -> "complexity-gt(" <> var <> ", " <> T.pack (show n) <> ")"
  ComplexityCond var ord n -> "complexity(" <> var <> ", " <> T.pack (show ord) <> ", " <> T.pack (show n) <> ")"
  NotIn var lst -> "not-in(" <> var <> ", [" <> T.intercalate ", " lst <> "])"
  TypeContains var ty -> "type-contains(" <> var <> ", " <> ty <> ")"
  HasImport m -> "has-import(" <> m <> ")"
  HasPragma p -> "has-pragma(" <> p <> ")"
  InModule pat -> "in-module(" <> pat <> ")"
  InTestFile -> "in-test-file"
  NotInTestFile -> "not-in-test-file"
  NotBind var -> "not-bind(" <> var <> ")"
  IsEtaReducible v1 v2 -> "is-eta-reducible(" <> v1 <> ", " <> v2 <> ")"
  NoDerivingStrategy -> "no-deriving-strategy"
  WildcardNotLast -> "wildcard-not-last"
  HasPatternOverlap -> "has-pattern-overlap"
  IsPatternIncomplete -> "is-pattern-incomplete"
  HasAmbiguousType -> "has-ambiguous-type"
  UsesDefaultOptions -> "uses-default-options"
  InContext ctx -> "in-context(" <> ctx <> ")"
  And conds -> "and(" <> T.intercalate ", " (map conditionName conds) <> ")"
  Or conds -> "or(" <> T.intercalate ", " (map conditionName conds) <> ")"
  Not cond -> "not(" <> conditionName cond <> ")"
  Always -> "always"
  Never -> "never"

-- | Extract category documentation
extractCategoryDoc :: Category -> [Rule] -> CategoryDoc
extractCategoryDoc cat rules = CategoryDoc
  { cdName = categoryName cat
  , cdDescription = categoryDescription cat
  , cdIcon = categoryIcon cat
  , cdRuleCount = length rules
  , cdRuleIds = map ruleId rules
  , cdSubcategories = []
  }

-- | Extract all category docs
extractCategoryDocs :: [Rule] -> [CategoryDoc]
extractCategoryDocs rules =
  let grouped = groupByCategory rules
  in [extractCategoryDoc cat rs | (cat, rs) <- Map.toList grouped]

-- | Group rules by category
groupByCategory :: [Rule] -> Map Category [Rule]
groupByCategory = foldr insert Map.empty
  where
    insert rule = Map.insertWith (++) (ruleCategory rule) [rule]

--------------------------------------------------------------------------------
-- Category Metadata
--------------------------------------------------------------------------------

-- | Get category name
categoryName :: Category -> Text
categoryName = \case
  Performance -> "Performance"
  SpaceLeaks -> "Space Leaks"
  Security -> "Security"
  Safety -> "Safety"
  Style -> "Style"
  Correctness -> "Correctness"
  Modernization -> "Modernization"
  Imports -> "Imports"
  Naming -> "Naming"
  Extensions -> "Extensions"
  Complexity -> "Complexity"
  Concurrency -> "Concurrency"
  ErrorHandling -> "Error Handling"
  Documentation -> "Documentation"
  Redundant -> "Redundant"
  Custom name -> name

-- | Get category description
categoryDescription :: Category -> Text
categoryDescription = \case
  Performance -> "Rules that identify potential performance issues"
  SpaceLeaks -> "Rules that detect space leak patterns"
  Security -> "Rules that find security vulnerabilities"
  Safety -> "Rules that detect unsafe patterns like partial functions"
  Style -> "Rules that enforce consistent coding style"
  Correctness -> "Rules that detect logical errors and bugs"
  Modernization -> "Rules suggesting modern APIs and patterns"
  Imports -> "Rules about import organization"
  Naming -> "Rules about naming conventions"
  Extensions -> "Rules about language extension usage"
  Complexity -> "Rules about code complexity metrics"
  Concurrency -> "Rules for concurrent programming patterns"
  ErrorHandling -> "Rules about error handling best practices"
  Documentation -> "Rules about documentation coverage"
  Redundant -> "Rules detecting redundant/unnecessary code"
  Custom _ -> "Custom rules"

-- | Get category icon
categoryIcon :: Category -> Maybe Text
categoryIcon = \case
  Performance -> Just "⚡"
  SpaceLeaks -> Just "💾"
  Security -> Just "🔒"
  Safety -> Just "⚠️"
  Style -> Just "🎨"
  Correctness -> Just "✅"
  Modernization -> Just "🆕"
  Imports -> Just "📦"
  Naming -> Just "🏷️"
  Extensions -> Just "🧩"
  Complexity -> Just "📊"
  Concurrency -> Just "🔄"
  ErrorHandling -> Just "🚨"
  Documentation -> Just "📚"
  Redundant -> Just "🗑️"
  Custom _ -> Nothing

--------------------------------------------------------------------------------
-- Search Index
--------------------------------------------------------------------------------

-- | Search index for quick rule lookup
data SearchIndex = SearchIndex
  { siEntries   :: [SearchEntry]        -- ^ All searchable entries
  , siByKeyword :: Map Text [Text]      -- ^ Keyword to rule IDs
  , siByTag     :: Map Text [Text]      -- ^ Tag to rule IDs
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A single searchable entry
data SearchEntry = SearchEntry
  { seRuleId    :: Text                 -- ^ Rule ID
  , seKeywords  :: [Text]               -- ^ Keywords for searching
  , seTags      :: [Text]               -- ^ Tags
  , seWeight    :: Double               -- ^ Search weight
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Build search index from rules
buildSearchIndex :: [Rule] -> SearchIndex
buildSearchIndex rules =
  let entries = map ruleToEntry rules
      byKeyword = buildKeywordIndex entries
      byTag = buildTagIndex entries
  in SearchIndex entries byKeyword byTag
  where
    ruleToEntry Rule{..} = SearchEntry
      { seRuleId = ruleId
      , seKeywords = extractKeywords ruleId (ruleNameFromId ruleId) ruleMessage
      , seTags = ruleTags
      , seWeight = 1.0
      }

    extractKeywords rid name msg =
      nub $ T.words (T.toLower rid) ++
            T.words (T.toLower name) ++
            take 10 (T.words (T.toLower msg))

    buildKeywordIndex entries =
      foldr addKeywords Map.empty entries

    addKeywords entry acc =
      foldr (\kw -> Map.insertWith (++) kw [seRuleId entry]) acc (seKeywords entry)

    buildTagIndex entries =
      foldr addTags Map.empty entries

    addTags entry acc =
      foldr (\tag -> Map.insertWith (++) tag [seRuleId entry]) acc (seTags entry)

-- | Search rules by query
searchRules :: SearchIndex -> Text -> [Text]
searchRules SearchIndex{..} query =
  let keywords = T.words (T.toLower query)
      matches = concatMap (\kw -> Map.findWithDefault [] kw siByKeyword) keywords
  in nub matches

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Compute documentation statistics
computeStats :: [Rule] -> DocStats
computeStats rules = DocStats
  { dsRuleCount = length rules
  , dsByCategory = Map.fromListWith (+)
      [(categoryName (ruleCategory r), 1) | r <- rules]
  , dsBySeverity = Map.fromListWith (+)
      [(T.pack (show (ruleSeverity r)), 1) | r <- rules]
  , dsWithFixes = length [r | r <- rules, isJust (ruleReplacement r)]
  , dsEnabledDefault = length [r | r <- rules, ruleEnabled r]
  , dsDeprecated = length [r | r <- rules, isJust (ruleDeprecated r)]
  }

-- | Convert text to URL slug
slugify :: Text -> Text
slugify = T.toLower . T.intercalate "-" . T.words . T.filter isSlugChar
  where
    isSlugChar c = c == ' ' || c == '-' || c == '_' ||
                   (c >= 'a' && c <= 'z') ||
                   (c >= 'A' && c <= 'Z') ||
                   (c >= '0' && c <= '9')

-- | Render rule catalog in markdown format
renderRuleCatalogMarkdown :: DocBundle -> Text
renderRuleCatalogMarkdown DocBundle{..} = T.unlines $
  [ "# Rule Catalog"
  , ""
  , "## Overview"
  , ""
  , "Total rules: " <> T.pack (show (dsRuleCount docStats))
  , ""
  ] ++
  concatMap renderCategorySection (sortOn cdName docCategories)
  where
    renderCategorySection CategoryDoc{..} =
      [ "## " <> cdName
      , ""
      , cdDescription
      , ""
      ] ++ map (\rid -> "- `" <> rid <> "`") cdRuleIds ++ [""]
