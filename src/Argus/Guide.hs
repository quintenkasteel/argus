{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Guide
-- Description : Developer guide for extending Argus
-- Copyright   : (c) 2024
-- License     : MIT
--
-- = Argus Developer Guide
--
-- This module provides comprehensive documentation for developers who want to
-- extend Argus with custom rules, analysis passes, or output formatters.
--
-- == Getting Started
--
-- Argus is organized into several key subsystems:
--
-- * __Rules__: Define patterns to detect code issues
-- * __Analysis__: Examine code structure and semantics
-- * __Refactoring__: Apply safe code transformations
-- * __Output__: Format and present diagnostics
--
-- == Module Structure
--
-- @
-- src/Argus/
-- ├── Types.hs           -- Core types (Diagnostic, Fix, SrcSpan)
-- ├── Core.hs            -- Main orchestration
-- ├── Config.hs          -- Configuration loading
-- ├── Utils.hs           -- Common utilities
-- │
-- ├── Rules/             -- Rule system
-- │   ├── Types.hs       -- Rule, SideCondition types
-- │   ├── Engine.hs      -- Rule evaluation engine
-- │   ├── ASTMatch.hs    -- AST pattern matching
-- │   ├── DSL.hs         -- Rule definition DSL
-- │   └── Builtin/       -- 30+ built-in rule categories
-- │
-- ├── Analysis/          -- Analysis passes
-- │   ├── Syntactic.hs   -- Text-based analysis
-- │   ├── Semantic.hs    -- HIE-based analysis
-- │   ├── Complexity.hs  -- Complexity metrics
-- │   └── ...
-- │
-- ├── Refactor/          -- Safe refactoring
-- │   ├── Engine.hs      -- Refactoring orchestration
-- │   ├── ExactPrint.hs  -- Format-preserving edits
-- │   └── SafeRefactor.hs-- Transactional refactoring
-- │
-- └── Output/            -- Output formatters
--     ├── Types.hs       -- Output types
--     ├── Terminal.hs    -- Terminal output
--     ├── Json.hs        -- JSON format
--     └── Sarif.hs       -- SARIF format (GitHub compatible)
-- @
--
-- == Adding a New Rule
--
-- === Step 1: Choose the Rule Type
--
-- Argus supports several pattern types:
--
-- [@TextPatternSpec@] Simple text patterns with word boundary matching
--
-- [@RegexPatternSpec@] Regular expression patterns
--
-- [@ASTPatternSpec@] Full AST pattern matching with metavariables
--
-- === Step 2: Define the Rule
--
-- @
-- myRule :: 'Rule'
-- myRule = 'defaultRule'
--   { 'ruleId'       = "my-category/my-rule"
--   , 'ruleName'     = "My Custom Rule"
--   , 'ruleSeverity' = 'Warning'
--   , 'ruleCategory' = 'Custom' "my-category"
--   , 'rulePattern'  = 'TextPatternSpec' "unsafeFunction"
--   , 'ruleMessage'  = "Consider using safeFunction instead"
--   , 'ruleEnabled'  = True
--   }
-- @
--
-- === Step 3: Add Side Conditions
--
-- Side conditions restrict where the rule matches:
--
-- @
-- ruleWithConditions :: 'Rule'
-- ruleWithConditions = myRule
--   { 'ruleSideConditions' = ['NotInComment', 'NotInString', 'NotInImport']
--   }
-- @
--
-- === Step 4: Provide a Fix
--
-- @
-- ruleWithFix :: 'Rule'
-- ruleWithFix = ruleWithConditions
--   { 'ruleFix' = Just $ 'Fix'
--       { 'fixTitle'         = "Replace with safe alternative"
--       , 'fixEdits'         = []  -- Will be generated from pattern
--       , 'fixIsPreferred'   = True
--       , 'fixAddImports'    = []
--       , 'fixRemoveImports' = []
--       , 'fixCategory'      = 'FCSafety'
--       , 'fixSafety'        = 'FSAlways'
--       }
--   }
-- @
--
-- === Step 5: Register the Rule
--
-- Add your rule to the appropriate builtin module or create a new one:
--
-- @
-- -- In Argus.Rules.Builtin.MyCategory
-- module Argus.Rules.Builtin.MyCategory (rules) where
--
-- rules :: ['Rule']
-- rules = [myRule, otherRule]
-- @
--
-- == AST Pattern Matching
--
-- AST patterns use metavariables (prefixed with @$@) to capture parts of the code:
--
-- @
-- -- Match: map f (map g xs)
-- -- Suggest: map (f . g) xs
-- mapMapFusion :: 'Rule'
-- mapMapFusion = 'defaultRule'
--   { 'ruleId'      = "performance/map-map"
--   , 'rulePattern' = 'ASTPatternSpec' "map $f (map $g $xs)"
--   , 'ruleMessage' = "Fuse consecutive maps: map ($f . $g) $xs"
--   }
-- @
--
-- === Available Metavariables
--
-- [@$x, $y, $z@] Match any single expression
--
-- [@$xs, $ys@] Match lists (in list comprehensions)
--
-- [@$f, $g@] Match functions
--
-- [@$_@] Wildcard - matches anything but doesn't capture
--
-- == Side Conditions Reference
--
-- === Location Predicates
--
-- [@NotInComment@] Match must not be inside a comment
--
-- [@NotInString@] Match must not be inside a string literal
--
-- [@NotInImport@] Match must not be in an import statement
--
-- [@InFunctionBody@] Match must be inside a function body (indented)
--
-- === Type Predicates
--
-- [@HasType var type@] Variable must have the specified type
--
-- [@HasTypeClass var class@] Variable must have the typeclass instance
--
-- [@IsNumeric var@] Variable must be numeric
--
-- [@IsList var@] Variable must be a list type
--
-- === Expression Predicates
--
-- [@IsLiteral var@] Variable must be a literal value
--
-- [@IsVariable var@] Variable must be an identifier
--
-- [@IsAtomic var@] Variable must be literal or identifier
--
-- [@NotEqual var1 var2@] Variables must have different values
--
-- === Combinators
--
-- [@And [cond1, cond2]@] All conditions must be true
--
-- [@Or [cond1, cond2]@] Any condition must be true
--
-- [@Not cond@] Condition must be false
--
-- == Creating Custom Analysis Passes
--
-- To add a new analysis pass:
--
-- @
-- module Argus.Analysis.MyAnalysis where
--
-- import Argus.Types
--
-- -- | Analyze a file and produce diagnostics
-- analyzeFile :: FilePath -> Text -> [Diagnostic]
-- analyzeFile path content = ...
-- @
--
-- Then register in @Argus.Core.runAnalysis@.
--
-- == Output Formatters
--
-- All output formatters implement a common interface:
--
-- @
-- renderMyFormat :: 'OutputOptions' -> 'AnalysisResult' -> 'Output'
-- renderMyFormat opts result = Output
--   { outText = ...
--   , outSummary = ...
--   , outExitCode = ...
--   }
-- @
--
-- == Safe Refactoring
--
-- The refactoring engine provides transactional semantics:
--
-- @
-- import Argus.Refactor.Engine
--
-- -- Apply fixes with validation
-- result <- 'refactorSafely' 'defaultEngineOptions' diagnostics content
-- case result of
--   EngineSuccess newContent changes -> ...
--   EngineFailure errors -> ...
--   EnginePartial newContent applied failed -> ...
-- @
--
-- == HIE Integration
--
-- For type-aware analysis, use the HIE modules:
--
-- @
-- import Argus.HIE.Query
-- import Argus.HIE.TypeInfo
--
-- -- Query type information
-- typeInfo <- getTypeAtPoint hieFile srcSpan
-- @
--
-- == Configuration
--
-- Argus reads configuration from @argus.toml@ or @linter.toml@:
--
-- @
-- [general]
-- directories = ["src", "app"]
-- exclude = ["dist-newstyle", ".stack-work"]
-- mode = "full"
--
-- [rules]
-- "partial/head" = "error"
-- "style/*" = "suggestion"
--
-- [naming]
-- type_pattern = "^[A-Z][a-zA-Z0-9]*$"
-- function_pattern = "^[a-z][a-zA-Z0-9]*$"
-- @
--
-- == Testing Rules
--
-- Use the test utilities for rule testing:
--
-- @
-- import Test.Hspec
-- import Argus.Rules.Engine
--
-- spec :: Spec
-- spec = describe "my rule" $ do
--   it "detects the pattern" $ do
--     let diags = evaluateRules (mkRuleEngine [myRule]) "Test.hs" "Test" "code"
--     length diags \`shouldBe\` 1
--
--   it "respects side conditions" $ do
--     let diags = evaluateRules (mkRuleEngine [myRule]) "Test.hs" "Test" "-- comment"
--     length diags \`shouldBe\` 0
-- @
--
-- == Performance Considerations
--
-- * Use @TextPatternSpec@ for simple patterns (fastest)
-- * Use @RegexPatternSpec@ for complex text patterns
-- * Use @ASTPatternSpec@ only when needed (requires parsing)
-- * Enable caching for repeated analyses
-- * Use incremental analysis for large codebases
--
-- == Error Handling
--
-- Argus uses @Either@ for recoverable errors:
--
-- @
-- analyzeWithError :: FilePath -> Either AnalysisError [Diagnostic]
-- @
--
-- And exceptions only for truly exceptional cases (file not found, etc.)
--
-- == Further Reading
--
-- * "Argus.Types" - Core type definitions
-- * "Argus.Rules.Engine" - Rule evaluation engine
-- * "Argus.Rules.ASTMatch" - AST pattern matching details
-- * "Argus.Refactor.Engine" - Safe refactoring API
-- * "Argus.HIE.Query" - HIE file queries
--
module Argus.Guide
  ( -- * Rule Creation Helpers
    quickRule
  , textRule
  , regexRule
  , astRule

    -- * Common Side Condition Sets
  , standardConditions
  , codeOnlyConditions
  , expressionConditions

    -- * Rule Testing Utilities
  , testRuleMatches
  , testRuleDoesNotMatch
  , assertDiagnosticCount

    -- * Example Rules
  , examplePartialHeadRule
  , exampleMapFusionRule
  , exampleNamingRule
  ) where

import Data.Text (Text)
import Data.Text qualified as T

import Argus.Rules.Types
  ( Rule(..)
  , defaultRule
  , RulePattern(..)
  , SideCondition(..)
  , Category(..)
  , ImportSpec(..)
  , mkImportSymbol
  , SymbolType(..)
  , SafetyLevel(..)
  )
import Argus.Rules.Engine (mkRuleEngine, evaluateRules)
import Argus.Types (Severity(..))

--------------------------------------------------------------------------------
-- Rule Creation Helpers
--------------------------------------------------------------------------------

-- | Create a quick rule with minimal configuration
--
-- @
-- rule = quickRule "my/rule" "warn" "unsafeFunc" "Use safeFunc instead"
-- @
quickRule
  :: Text    -- ^ Rule ID (e.g., "category/name")
  -> Text    -- ^ Severity ("error", "warn", "suggestion", "info")
  -> Text    -- ^ Pattern to match
  -> Text    -- ^ Warning message
  -> Rule
quickRule ruleId' severity pat msg = defaultRule
  { ruleId = ruleId'
  , ruleSeverity = parseSeverity severity
  , rulePattern = TextPatternSpec pat
  , ruleMessage = msg
  , ruleEnabled = True
  , ruleConditions = standardConditions
  }

-- | Create a text pattern rule
textRule :: Text -> Text -> Text -> Text -> Rule
textRule = quickRule

-- | Create a regex pattern rule
regexRule :: Text -> Text -> Text -> Text -> Rule
regexRule ruleId' severity pat msg = (quickRule ruleId' severity pat msg)
  { rulePattern = RegexPatternSpec pat
  }

-- | Create an AST pattern rule
astRule :: Text -> Text -> Text -> Text -> Rule
astRule ruleId' severity pat msg = (quickRule ruleId' severity pat msg)
  { rulePattern = ASTPatternSpec pat
  }

-- | Parse severity from string
parseSeverity :: Text -> Severity
parseSeverity s = case T.toLower s of
  "error"      -> Error
  "err"        -> Error
  "warning"    -> Warning
  "warn"       -> Warning
  "suggestion" -> Suggestion
  "info"       -> Info
  _            -> Warning

--------------------------------------------------------------------------------
-- Common Side Condition Sets
--------------------------------------------------------------------------------

-- | Standard conditions for most rules
--
-- Excludes matches in comments, strings, and imports
standardConditions :: [SideCondition]
standardConditions =
  [ NotInComment
  , NotInString
  , NotInImport
  ]

-- | Conditions for code-only matches
--
-- Same as standard, plus must be in function body
codeOnlyConditions :: [SideCondition]
codeOnlyConditions = standardConditions ++
  [ InFunctionBody
  ]

-- | Conditions for expression-level rules
--
-- Standard conditions only (no function body requirement)
expressionConditions :: [SideCondition]
expressionConditions = standardConditions

--------------------------------------------------------------------------------
-- Rule Testing Utilities
--------------------------------------------------------------------------------

-- | Test that a rule matches the given code
--
-- @
-- testRuleMatches myRule "head xs" `shouldBe` True
-- @
testRuleMatches :: Rule -> Text -> Bool
testRuleMatches rule code =
  not $ null $ evaluateRules (mkRuleEngine [rule]) "Test.hs" "Test" code

-- | Test that a rule does NOT match the given code
testRuleDoesNotMatch :: Rule -> Text -> Bool
testRuleDoesNotMatch rule code = not $ testRuleMatches rule code

-- | Assert the exact number of diagnostics
--
-- @
-- assertDiagnosticCount myRule "head a + head b" 2 `shouldBe` True
-- @
assertDiagnosticCount :: Rule -> Text -> Int -> Bool
assertDiagnosticCount rule code expectedCount =
  length (evaluateRules (mkRuleEngine [rule]) "Test.hs" "Test" code) == expectedCount

--------------------------------------------------------------------------------
-- Example Rules
--------------------------------------------------------------------------------

-- | Example rule for detecting partial head function
--
-- @
-- -- Matches: head xs
-- -- Message: Use 'headMay' instead of partial 'head'
-- @
examplePartialHeadRule :: Rule
examplePartialHeadRule = defaultRule
  { ruleId = "partial/head"
  , ruleSeverity = Warning
  , ruleCategory = Safety
  , rulePattern = TextPatternSpec "head"
  , ruleMessage = "Use 'headMay' from Data.Maybe or pattern match instead of partial 'head'"
  , ruleEnabled = True
  , ruleConditions = standardConditions
  , ruleReplacement = Just "headMay"
  , ruleSafety = Safe
  , ruleAddImports = [ImportSpec
      { impModule = "Data.Maybe"
      , impSymbols = [mkImportSymbol "listToMaybe" SymFunction]
      , impQualified = Nothing
      , impHiding = False
      , impPackage = Nothing
      }]
  }

-- | Example rule for map fusion
--
-- @
-- -- Matches: map f (map g xs)
-- -- Suggest: map (f . g) xs
-- @
exampleMapFusionRule :: Rule
exampleMapFusionRule = defaultRule
  { ruleId = "performance/map-fusion"
  , ruleSeverity = Suggestion
  , ruleCategory = Performance
  , rulePattern = ASTPatternSpec "map $f (map $g $xs)"
  , ruleMessage = "Fuse consecutive maps: use 'map ($f . $g) $xs' for better performance"
  , ruleEnabled = True
  , ruleConditions = standardConditions
  }

-- | Example naming rule
--
-- @
-- -- Matches: functions starting with uppercase
-- @
exampleNamingRule :: Rule
exampleNamingRule = defaultRule
  { ruleId = "naming/function-case"
  , ruleSeverity = Suggestion
  , ruleCategory = Style
  , rulePattern = RegexPatternSpec "^[A-Z][a-zA-Z0-9]*\\s*::"
  , ruleMessage = "Functions should start with lowercase letter"
  , ruleEnabled = True
  , ruleConditions = [NotInComment]
  }
