{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Argus.Rules.Types
-- Description : Unified type system for Argus rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
-- Portability : GHC
--
-- = Overview
--
-- This module is the __single source of truth__ for all rule-related types in Argus.
-- All rules, whether defined in TOML configuration or Haskell DSL, use these types.
--
-- = Architecture
--
-- The unified type system eliminates the previous fragmentation across modules:
--
-- * One 'Category' enum (replaces DSL.Category, RuleTypes.RuleCategory, Types.FixCategory)
-- * One 'SafetyLevel' enum (replaces DSL.SafetyLevel, RuleTypes.FixSafety, Types.FixSafety)
-- * One 'SideCondition' type (replaces DSL.SideCondition, ASTMatch.SideCondition)
-- * One 'Pattern' type (replaces DSL.Pattern, Rules\/Types.Pattern)
-- * One 'ImportSpec' type (replaces DSL.ImportSpec, Types.FixImport)
-- * One 'Rule' type (replaces DSL.Rule, Rules\/Types.Rule, ASTMatch.ASTRule)
--
-- = Rule Lifecycle
--
-- 1. __Definition__: Rules are defined in TOML config or Haskell DSL
-- 2. __Parsing__: TOML is parsed into 'Rule' values; DSL produces 'Rule' directly
-- 3. __Registration__: Rules are registered in the engine's rule registry
-- 4. __Matching__: Engine evaluates 'rulePattern' against source code
-- 5. __Conditions__: Side conditions ('ruleConditions') are checked
-- 6. __Diagnosis__: Matching rules produce 'Diagnostic' values
-- 7. __Fixing__: If 'ruleReplacement' is set, a 'Fix' is generated
--
-- = Usage
--
-- Import this module in any code that defines or evaluates rules:
--
-- @
-- import Argus.Rules.Types
--
-- -- Define a rule
-- myRule :: Rule
-- myRule = defaultRule
--   { ruleId = "my-rule"
--   , ruleCategory = Performance
--   , ruleMessage = "Prefer foldl' over foldl"
--   , rulePattern = TextPatternSpec "foldl $F $Z $XS"
--   , ruleReplacement = Just "foldl' $F $Z $XS"
--   }
-- @
--
-- = Thread Safety
--
-- All types in this module are immutable and safe for concurrent access.
--
-- @since 1.0.0
module Argus.Rules.Types
  ( -- * The Unified Rule Type
    Rule (..)
  , defaultRule

    -- * Rule Categories
  , Category (..)
  , allCategories
  , categoryToText
  , textToCategory

    -- * Severity Levels
    -- | Re-exported from Argus.Types for convenience
  , Severity (..)

    -- * Safety Levels
  , SafetyLevel (..)
  , safetyToText
  , textToSafety

    -- * Rule Targets
  , RuleTarget (..)
  , targetToText
  , textToTarget
  , categoryDefaultTarget
  , effectiveTarget

    -- * Comment Types
  , CommentType (..)

    -- * Side Conditions
  , SideCondition (..)

    -- * Rule Pattern Specifications
    -- | How rules specify patterns to match
  , RulePattern (..)
  , rulePatternToText

    -- * Pattern AST (for pattern matching)
    -- | Internal representation of parsed patterns
  , PatternAST (..)
  , Pattern  -- ^ Backward-compatible alias for PatternAST
  , PatternVar (..)
  , TypePattern (..)

    -- * Bindings (for pattern substitution)
  , Binding (..)
  , Bindings
  , mkBinding
  , mkBindingWithSpan
  , emptyBindings
  , addBinding'
  , lookupBinding

    -- * Import Specifications
  , ImportSpec (..)
  , ImportSymbol (..)
  , SymbolType (..)
  , mkImportSpec
  , mkImportSymbol

    -- * Conversion Functions (for migration)
    -- These will be removed after full migration
  , categoryToFixCategory
  , fixCategoryToCategory
  , safetyToFixSafety
  , fixSafetyToSafety
  , importSpecToFixImport
  , fixImportToImportSpec
  ) where

import Data.Aeson (ToJSON (..), FromJSON (..), ToJSONKey (..), FromJSONKey (..))
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AT
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Argus.Types (Severity(..))
import Argus.Types qualified as AT

--------------------------------------------------------------------------------
-- Rule Categories
--------------------------------------------------------------------------------

-- | Unified rule categories for grouping and bulk enable\/disable.
--
-- Categories serve multiple purposes:
--
-- * __Filtering__: Users can enable\/disable entire categories
-- * __Reporting__: Diagnostics are grouped by category in reports
-- * __Targeting__: Some categories imply default targets (e.g., 'Documentation' → Haddock)
--
-- __CLI Usage__:
--
-- @
-- # Enable only security and performance rules
-- argus check --category security,performance src/
--
-- # Disable documentation rules
-- argus check --disable-category documentation src/
-- @
--
-- __TOML Configuration__:
--
-- @
-- [rules]
-- # Disable entire categories
-- disable_categories = ["style", "naming"]
--
-- # Per-rule category override
-- [[rules.custom]]
-- id = "my-rule"
-- category = "performance"
-- @
--
-- @since 1.0.0
data Category
  = Performance
    -- ^ Performance anti-patterns (O(n²), unnecessary allocations, inefficient patterns).
  | SpaceLeaks
    -- ^ Space leak patterns (lazy accumulation, thunk buildup, missing strictness).
  | Security
    -- ^ Security vulnerabilities (injection, unsafe IO, hardcoded secrets).
  | Safety
    -- ^ Partial functions, unsafe operations that can crash at runtime.
  | Style
    -- ^ Code style and formatting (readability, idioms, consistency).
  | Correctness
    -- ^ Logic errors, typeclass law violations, semantic bugs.
  | Modernization
    -- ^ Use newer APIs\/patterns (Applicative over Monad, Text over String).
  | Imports
    -- ^ Import organization, qualification, and redundancy.
  | Naming
    -- ^ Naming convention violations (camelCase, PascalCase, etc.).
  | Extensions
    -- ^ Language extension usage (unnecessary, missing, or dangerous extensions).
  | Complexity
    -- ^ Code complexity metrics (cyclomatic complexity, nesting depth).
  | Concurrency
    -- ^ Concurrency anti-patterns (STM misuse, deadlock risks, race conditions).
  | ErrorHandling
    -- ^ Error handling best practices (exception safety, partial functions).
  | Documentation
    -- ^ Documentation completeness and quality (Haddock coverage).
    -- Rules with this category target documentation comments by default.
  | Redundant
    -- ^ Redundant\/unnecessary code (dead code, identity operations).
  | Custom Text
    -- ^ User-defined category from plugins or custom rules.
  deriving stock (Eq, Ord, Show, Generic)

-- | Convert category to text representation for serialization
categoryToText :: Category -> Text
categoryToText = \case
  Performance -> "performance"
  SpaceLeaks -> "space-leaks"
  Security -> "security"
  Safety -> "safety"
  Style -> "style"
  Correctness -> "correctness"
  Modernization -> "modernization"
  Imports -> "imports"
  Naming -> "naming"
  Extensions -> "extensions"
  Complexity -> "complexity"
  Concurrency -> "concurrency"
  ErrorHandling -> "error-handling"
  Documentation -> "documentation"
  Redundant -> "redundant"
  Custom t -> t

-- | Parse category from text
textToCategory :: Text -> Category
textToCategory t = case T.toLower t of
  "performance" -> Performance
  "space-leaks" -> SpaceLeaks
  "spaceleaks" -> SpaceLeaks
  "security" -> Security
  "safety" -> Safety
  "partial" -> Safety  -- Legacy alias
  "style" -> Style
  "correctness" -> Correctness
  "modernization" -> Modernization
  "modernize" -> Modernization  -- Legacy alias
  "imports" -> Imports
  "naming" -> Naming
  "extensions" -> Extensions
  "complexity" -> Complexity
  "concurrency" -> Concurrency
  "error-handling" -> ErrorHandling
  "errorhandling" -> ErrorHandling
  "documentation" -> Documentation
  "redundant" -> Redundant
  other -> Custom other

instance ToJSON Category where
  toJSON = AE.String . categoryToText

instance FromJSON Category where
  parseJSON = AE.withText "Category" $ pure . textToCategory

instance ToJSONKey Category where
  toJSONKey = AT.toJSONKeyText categoryToText

instance FromJSONKey Category where
  fromJSONKey = AT.FromJSONKeyTextParser $ pure . textToCategory

-- | All built-in categories (excludes Custom)
allCategories :: [Category]
allCategories =
  [ Performance
  , SpaceLeaks
  , Security
  , Safety
  , Style
  , Correctness
  , Modernization
  , Imports
  , Naming
  , Extensions
  , Complexity
  , Concurrency
  , ErrorHandling
  , Documentation
  , Redundant
  ]

--------------------------------------------------------------------------------
-- Safety Levels
--------------------------------------------------------------------------------

-- | Safety level of an automated fix.
--
-- Determines whether a fix can be applied automatically or requires human review.
-- The @--safe-only@ CLI flag restricts auto-fix to 'Safe' level only.
--
-- __Ordering__: 'Safe' < 'MostlySafe' < 'NeedsReview' < 'Unsafe'
-- (safest to least safe).
--
-- __CLI Integration__:
--
-- @
-- # Apply only guaranteed-safe fixes
-- argus fix --safe-only src/
--
-- # Apply safe and mostly-safe fixes
-- argus fix --safety-level mostly-safe src/
--
-- # Preview unsafe fixes without applying
-- argus fix --preview --safety-level unsafe src/
-- @
--
-- __Rule Definition__:
--
-- @
-- myRule = defaultRule
--   { ruleId = "use-foldl-prime"
--   , ruleSafety = MostlySafe  -- Safe unless laziness is intentional
--   , ...
--   }
-- @
--
-- @since 1.0.0
data SafetyLevel
  = Safe
    -- ^ Always safe to apply automatically. Guaranteed not to change
    -- program semantics. Example: @id x@ → @x@, removing redundant parens.
  | MostlySafe
    -- ^ Safe in the vast majority of cases; rare edge cases may exist.
    -- Example: @foldl@ → @foldl'@ (safe unless laziness is intentional).
  | NeedsReview
    -- ^ Requires human review before applying. The fix is likely correct
    -- but context-dependent. Example: removing apparently unused code.
  | Unsafe
    -- ^ May change program semantics. Should only be applied with explicit
    -- user confirmation. Example: changing evaluation order.
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

-- | Convert safety level to text for serialization
safetyToText :: SafetyLevel -> Text
safetyToText = \case
  Safe -> "safe"
  MostlySafe -> "mostly-safe"
  NeedsReview -> "needs-review"
  Unsafe -> "unsafe"

-- | Parse safety level from text
textToSafety :: Text -> SafetyLevel
textToSafety t = case T.toLower t of
  "safe" -> Safe
  "always" -> Safe  -- Legacy alias (FSAlways)
  "mostly-safe" -> MostlySafe
  "mostlysafe" -> MostlySafe
  "mostly" -> MostlySafe  -- Legacy alias (FSMostly)
  "needs-review" -> NeedsReview
  "needsreview" -> NeedsReview
  "review" -> NeedsReview  -- Legacy alias (FSReview)
  "manual" -> NeedsReview  -- Legacy alias (Manual)
  "manualreview" -> NeedsReview
  "unsafe" -> Unsafe
  _ -> Safe  -- Default to safe

instance ToJSON SafetyLevel where
  toJSON = AE.String . safetyToText

instance FromJSON SafetyLevel where
  parseJSON = AE.withText "SafetyLevel" $ pure . textToSafety

--------------------------------------------------------------------------------
-- Rule Targets
--------------------------------------------------------------------------------

-- | What a rule targets for matching.
--
-- This determines WHERE in the source code a rule looks for matches.
-- Most code rules should use 'TargetCode' (the default), while documentation
-- rules should use 'TargetDocumentation'.
--
-- The key insight: instead of per-rule boolean flags, we use a single
-- high-level target specification. The engine infers the appropriate
-- target from the rule's category when not explicitly specified.
data RuleTarget
  = TargetCode           -- ^ Code only: skip comments, strings, pragmas (default for most rules)
  | TargetComments       -- ^ All comments: match inside -- and {- -} comments
  | TargetDocumentation  -- ^ Haddock only: match inside -- | and {-| -} documentation
  | TargetPragmas        -- ^ Pragmas only: match inside {-# ... #-} pragmas
  | TargetStrings        -- ^ String literals: match inside "..." strings
  | TargetAll            -- ^ Everything: no filtering, match anywhere
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Convert target to text for serialization
targetToText :: RuleTarget -> Text
targetToText = \case
  TargetCode -> "code"
  TargetComments -> "comments"
  TargetDocumentation -> "documentation"
  TargetPragmas -> "pragmas"
  TargetStrings -> "strings"
  TargetAll -> "all"

-- | Parse target from text
textToTarget :: Text -> RuleTarget
textToTarget t = case T.toLower t of
  "code" -> TargetCode
  "comments" -> TargetComments
  "documentation" -> TargetDocumentation
  "docs" -> TargetDocumentation
  "haddock" -> TargetDocumentation
  "pragmas" -> TargetPragmas
  "strings" -> TargetStrings
  "all" -> TargetAll
  _ -> TargetCode  -- Default to code

-- | Infer the default target based on rule category.
--
-- This enables clean category-implicit targeting:
-- * Documentation category → checks documentation comments
-- * All other categories → check code (skip comments)
categoryDefaultTarget :: Category -> RuleTarget
categoryDefaultTarget = \case
  Documentation -> TargetDocumentation
  _             -> TargetCode

-- | Get the effective target for a rule.
--
-- If the rule has an explicit target, use that.
-- Otherwise, infer from the category.
effectiveTarget :: Rule -> RuleTarget
effectiveTarget r = case ruleTarget r of
  Just target -> target
  Nothing     -> categoryDefaultTarget (ruleCategory r)

--------------------------------------------------------------------------------
-- Comment Types (for targeting)
--------------------------------------------------------------------------------

-- | Comment types for targeting specific comment kinds.
--
-- Used by 'InCommentType' side condition and internally by Engine
-- to determine where rules should match.
data CommentType
  = CTLineComment       -- ^ Single line comment: -- ...
  | CTBlockComment      -- ^ Block comment: {- ... -}
  | CTHaddockLine       -- ^ Haddock line: -- | or -- ^
  | CTHaddockBlock      -- ^ Haddock block: {-| ... -} or {-^ ... -}
  | CTPragma            -- ^ GHC pragma: {-# ... #-}
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Side Conditions
--------------------------------------------------------------------------------

-- | Side conditions for constraining when a rule matches.
--
-- After a pattern matches, side conditions are evaluated to determine if
-- the match should produce a diagnostic. This allows rules to be precise
-- about when they apply, reducing false positives.
--
-- __Evaluation__:
--
-- Side conditions are evaluated in order. Evaluation short-circuits on
-- the first failure (for 'And') or success (for 'Or').
--
-- __HIE Dependency__:
--
-- Some conditions require HIE (Haskell Interface Extended) data for accurate
-- evaluation. In 'QuickMode', these conditions are evaluated conservatively:
--
-- * Type conditions ('HasType', 'HasTypeClass', 'IsMonad') → assumed true
-- * Location conditions ('NotInComment', 'InFunctionBody') → evaluated from source
--
-- __Example Usage__:
--
-- @
-- -- Rule that only matches if $X is not a literal
-- myRule = defaultRule
--   { rulePattern = TextPatternSpec "length $X == 0"
--   , ruleReplacement = Just "null $X"
--   , ruleConditions = [Not (IsLiteral "$X")]
--   }
--
-- -- Rule with type constraint (requires FullMode for accuracy)
-- typedRule = defaultRule
--   { rulePattern = TextPatternSpec "show $X"
--   , ruleConditions = [HasType "$X" "Int"]
--   }
-- @
--
-- @since 1.0.0
data SideCondition
  -- Location predicates (where in the code)
  = NotInComment                    -- ^ Match must not be inside a comment
  | NotInString                     -- ^ Match must not be inside a string literal
  | NotInImport                     -- ^ Match must not be in an import statement
  | InFunctionBody                  -- ^ Match must be inside a function body
  | InCommentType CommentType       -- ^ Match must be inside specific comment type
  | InStringLiteral                 -- ^ Match must be inside a string literal

  -- Type predicates (require HIE data for full accuracy)
  | HasType Text Text               -- ^ Variable has exact type: HasType "$X" "Int"
  | HasTypeClass Text Text          -- ^ Type has class instance: HasTypeClass "$X" "Ord"
  | TypeMatches Text Text           -- ^ Type matches pattern: TypeMatches "$X" "Maybe _"
  | IsNumeric Text                  -- ^ Variable has numeric type
  | IsString Text                   -- ^ Variable has String/Text type
  | IsList Text                     -- ^ Variable has list type
  | IsMaybe Text                    -- ^ Variable has Maybe type
  | IsMonad Text Text               -- ^ Variable is in specific monad: IsMonad "$X" "IO"
  | IsPure Text                     -- ^ Expression is pure (no IO side effects)

  -- Expression predicates
  | IsLiteral Text                  -- ^ Variable must be a literal value
  | IsVariable Text                 -- ^ Variable must be a simple identifier
  | IsApplication Text              -- ^ Variable must be a function application
  | IsLambda Text                   -- ^ Variable must be a lambda expression
  | IsAtomic Text                   -- ^ Variable must be atomic (no subexpressions)
  | IsConstructor Text              -- ^ Variable must be a data constructor
  | NotEqual Text Text              -- ^ Two metavars must bind to different expressions
  | FreeIn Text Text                -- ^ First var is free in second's expression
  | NotFreeIn Text Text             -- ^ First var is NOT free in second's expression
  | ComplexityLT Text Int           -- ^ Expression complexity less than N
  | ComplexityGT Text Int           -- ^ Expression complexity greater than N
  | ComplexityCond Text Ordering Int -- ^ Expression complexity comparison (EQ, LT, GT)

  -- Name predicates
  | NotIn Text [Text]              -- ^ Variable name must not be in given list

  -- Type contains predicate
  | TypeContains Text Text         -- ^ Variable's type contains given type (for polymorphic)

  -- Context predicates
  | HasImport Text                  -- ^ Module has import for given module
  | HasPragma Text                  -- ^ File has specific LANGUAGE pragma
  | InModule Text                   -- ^ Code is in module matching pattern
  | InTestFile                      -- ^ Code is in a test file (*Spec.hs, *Test.hs)
  | NotInTestFile                   -- ^ Code is NOT in a test file

  -- Expression structure predicates (for pattern analysis)
  | NotBind Text                      -- ^ Expression is not a monadic bind (>>=, >>, <-)
  | IsEtaReducible Text Text          -- ^ Function can be eta-reduced: var not free in func

  -- Deriving and pattern analysis predicates
  | NoDerivingStrategy                -- ^ Deriving clause has no explicit strategy
  | WildcardNotLast                   -- ^ Wildcard pattern is not the last case
  | HasPatternOverlap                 -- ^ Case patterns have overlap
  | IsPatternIncomplete               -- ^ Pattern match is incomplete
  | HasAmbiguousType                  -- ^ Expression has ambiguous type
  | UsesDefaultOptions                -- ^ Uses default aeson/etc options

  -- Context predicates (semantic context detection)
  | InContext Text                    -- ^ In a named context: "parallel", "test", "unsafe"

  -- Combinators
  | And [SideCondition]             -- ^ All conditions must be true
  | Or [SideCondition]              -- ^ At least one condition must be true
  | Not SideCondition               -- ^ Negation of condition
  | Always                          -- ^ Always true (no condition)
  | Never                           -- ^ Always false
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Rule Pattern Specifications
--------------------------------------------------------------------------------

-- | How a rule specifies what code to match.
--
-- This is used in rule definitions to specify the match pattern:
-- * TextPatternSpec: Simple text with metavariables ($X, $F, etc.)
-- * RegexPatternSpec: Regular expression for text matching
-- * ASTPatternSpec: Full AST pattern (parsed as Haskell, allows structural matching)
data RulePattern
  = TextPatternSpec Text           -- ^ Simple text with metavars: "head $X"
  | RegexPatternSpec Text          -- ^ Regex pattern for text matching
  | ASTPatternSpec Text            -- ^ Full AST pattern (parsed by GHC)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Convert rule pattern to text representation
rulePatternToText :: RulePattern -> Text
rulePatternToText (TextPatternSpec t) = t
rulePatternToText (RegexPatternSpec t) = t
rulePatternToText (ASTPatternSpec t) = t

--------------------------------------------------------------------------------
-- Import Specifications
--------------------------------------------------------------------------------

-- | Type of symbol being imported
data SymbolType
  = SymFunction     -- ^ Regular function or value
  | SymOperator     -- ^ Infix operator (needs parens in import)
  | SymType         -- ^ Type or type synonym
  | SymClass        -- ^ Type class
  | SymConstructor  -- ^ Data constructor
  | SymPattern      -- ^ Pattern synonym
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A single symbol to import
data ImportSymbol = ImportSymbol
  { symName     :: Text           -- ^ Symbol name (e.g., "foldl'", "Map", "(<>)")
  , symType     :: SymbolType     -- ^ Type of symbol
  , symChildren :: [Text]         -- ^ Child items for types/classes (constructors/methods)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Create a simple import symbol
mkImportSymbol :: Text -> SymbolType -> ImportSymbol
mkImportSymbol name stype = ImportSymbol
  { symName = name
  , symType = stype
  , symChildren = []
  }

-- | Import specification required by a fix.
--
-- This unified type replaces DSL.ImportSpec and Types.FixImport.
data ImportSpec = ImportSpec
  { impModule    :: Text              -- ^ Module to import (e.g., "Data.Foldable")
  , impSymbols   :: [ImportSymbol]    -- ^ Specific symbols to import (empty = whole module)
  , impQualified :: Maybe Text        -- ^ Qualifier alias (e.g., Just "F" for "as F")
  , impHiding    :: Bool              -- ^ Is this a hiding import?
  , impPackage   :: Maybe Text        -- ^ Package name if ambiguous
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Create a simple import spec
mkImportSpec :: Text -> [Text] -> ImportSpec
mkImportSpec modName symbols = ImportSpec
  { impModule = modName
  , impSymbols = map (\s -> mkImportSymbol s SymFunction) symbols
  , impQualified = Nothing
  , impHiding = False
  , impPackage = Nothing
  }

--------------------------------------------------------------------------------
-- The Unified Rule Type
--------------------------------------------------------------------------------

-- | The unified rule type for Argus.
--
-- This is the single rule type used throughout the entire system:
--
-- * TOML configuration parses into this type
-- * Haskell DSL produces this type
-- * Engine evaluates this type
-- * All builtin rules use this type
--
-- __Invariants__:
--
-- * 'ruleId' must be unique across all rules
-- * 'ruleId' should follow the pattern @\"category\/name\"@ (e.g., @\"performance\/length-null\"@)
-- * 'rulePattern' must be parseable by the pattern engine
-- * If 'ruleReplacement' is set, metavariables must match those in 'rulePattern'
--
-- __Lifecycle__:
--
-- 1. Created via TOML parsing, DSL, or direct construction
-- 2. Validated for pattern syntax and condition consistency
-- 3. Registered in the rule engine
-- 4. Evaluated against source files during analysis
-- 5. Produces 'Diagnostic' values when matched
--
-- __Construction__:
--
-- Use 'defaultRule' as a starting point and override fields:
--
-- @
-- myRule :: Rule
-- myRule = defaultRule
--   { ruleId = "performance/length-null"
--   , ruleCategory = Performance
--   , ruleSeverity = Warning
--   , ruleMessage = "Use null instead of length comparison"
--   , rulePattern = TextPatternSpec "length $X == 0"
--   , ruleReplacement = Just "null $X"
--   , ruleSafety = Safe
--   }
-- @
--
-- __TOML Definition__:
--
-- @
-- [[rules.custom]]
-- id = "performance/length-null"
-- category = "performance"
-- severity = "warning"
-- message = "Use null instead of length comparison"
-- pattern = "length $X == 0"
-- replacement = "null $X"
-- safety = "safe"
-- @
--
-- @since 1.0.0
data Rule = Rule
  { -- | Unique rule identifier (e.g., "performance/length-null")
    ruleId            :: Text

    -- | Rule category for grouping and filtering
  , ruleCategory      :: Category

    -- | Severity level (Error, Warning, Suggestion, Info)
  , ruleSeverity      :: Severity

    -- | Human-readable message shown to user
  , ruleMessage       :: Text

    -- | Optional detailed explanation
  , ruleExplanation   :: Maybe Text

    -- Pattern matching
    -- | Pattern specification for matching source code
  , rulePattern       :: RulePattern

    -- | Optional replacement pattern for fixes
  , ruleReplacement   :: Maybe Text

    -- | Side conditions that must be satisfied
  , ruleConditions    :: [SideCondition]

    -- Fix metadata
    -- | Safety level for automatic fix application
  , ruleSafety        :: SafetyLevel

    -- | Imports to add when fix is applied
  , ruleAddImports    :: [ImportSpec]

    -- | Module names to remove from imports
  , ruleRemoveImports :: [Text]

    -- Scope
    -- | Whether rule is enabled
  , ruleEnabled       :: Bool

    -- | Module patterns where rule applies (empty = all)
  , ruleWithin        :: [Text]

    -- | Module patterns to exclude
  , ruleExcept        :: [Text]

    -- Optional metadata
    -- | Deprecation message if rule is deprecated
  , ruleDeprecated    :: Maybe Text

    -- | Tags for filtering (e.g., ["performance", "list"])
  , ruleTags          :: [Text]

    -- | Reference URLs (docs, CWE codes, etc.)
  , ruleReferences    :: [Text]

    -- | Additional note/explanation
  , ruleNote          :: Maybe Text

    -- | Description of what the fix does
  , ruleFixDescription :: Maybe Text

    -- | Source module for matched pattern (e.g., "Data.List")
  , ruleSourceModule  :: Maybe Text

    -- | Target module for replacement (e.g., "Data.Containers.ListUtils")
  , ruleTargetModule  :: Maybe Text

    -- | Target specification (where in source code to match)
    --
    -- When Nothing, the target is inferred from the category:
    -- * Documentation category → TargetDocumentation
    -- * All other categories → TargetCode (skip comments, strings, pragmas)
    --
    -- Set explicitly to override the default for special rules.
  , ruleTarget        :: Maybe RuleTarget
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default rule with all optional fields set to sensible defaults.
--
-- Use this as a starting point for rule definitions:
--
-- @
-- myRule = defaultRule
--   { ruleId = "my-category/my-rule"
--   , ruleMessage = "Description of the issue"
--   , rulePattern = TextPatternSpec "pattern to match"
--   }
-- @
--
-- __Default Values__:
--
-- * 'ruleCategory': 'Style'
-- * 'ruleSeverity': 'Warning'
-- * 'ruleSafety': 'Safe'
-- * 'ruleEnabled': 'True'
-- * 'ruleTarget': 'Nothing' (inferred from category)
--
-- @since 1.0.0
defaultRule :: Rule
defaultRule = Rule
  { ruleId = ""
  , ruleCategory = Style
  , ruleSeverity = Warning
  , ruleMessage = ""
  , ruleExplanation = Nothing
  , rulePattern = TextPatternSpec ""
  , ruleReplacement = Nothing
  , ruleConditions = []
  , ruleSafety = Safe
  , ruleAddImports = []
  , ruleRemoveImports = []
  , ruleEnabled = True
  , ruleWithin = []
  , ruleExcept = []
  , ruleDeprecated = Nothing
  , ruleTags = []
  , ruleReferences = []
  , ruleNote = Nothing
  , ruleFixDescription = Nothing
  , ruleSourceModule = Nothing
  , ruleTargetModule = Nothing
  , ruleTarget = Nothing  -- Inferred from category (Documentation→Docs, others→Code)
  }

--------------------------------------------------------------------------------
-- Conversion Functions (for migration - to be removed after full migration)
--------------------------------------------------------------------------------

-- | Convert unified Category to old Types.FixCategory
categoryToFixCategory :: Category -> AT.FixCategory
categoryToFixCategory = \case
  Performance -> AT.FCPerformance
  SpaceLeaks -> AT.FCSpaceLeaks
  Security -> AT.FCSecurity
  Safety -> AT.FCSafety
  Style -> AT.FCStyle
  Correctness -> AT.FCStyle  -- No direct mapping, use Style
  Modernization -> AT.FCModernize
  Imports -> AT.FCImports
  Naming -> AT.FCStyle  -- No direct mapping
  Extensions -> AT.FCStyle  -- No direct mapping
  Complexity -> AT.FCStyle  -- No direct mapping
  Concurrency -> AT.FCStyle  -- No direct mapping
  ErrorHandling -> AT.FCStyle  -- No direct mapping
  Documentation -> AT.FCStyle  -- No direct mapping
  Redundant -> AT.FCRedundant
  Custom t -> AT.FCCustom t

-- | Convert old Types.FixCategory to unified Category
fixCategoryToCategory :: AT.FixCategory -> Category
fixCategoryToCategory = \case
  AT.FCPerformance -> Performance
  AT.FCModernize -> Modernization
  AT.FCSafety -> Safety
  AT.FCStyle -> Style
  AT.FCImports -> Imports
  AT.FCRedundant -> Redundant
  AT.FCSpaceLeaks -> SpaceLeaks
  AT.FCSecurity -> Security
  AT.FCCustom t -> Custom t

-- | Convert unified SafetyLevel to old Types.FixSafety
safetyToFixSafety :: SafetyLevel -> AT.FixSafety
safetyToFixSafety = \case
  Safe -> AT.FSAlways
  MostlySafe -> AT.FSMostly
  NeedsReview -> AT.FSReview
  Unsafe -> AT.FSUnsafe

-- | Convert old Types.FixSafety to unified SafetyLevel
fixSafetyToSafety :: AT.FixSafety -> SafetyLevel
fixSafetyToSafety = \case
  AT.FSAlways -> Safe
  AT.FSMostly -> MostlySafe
  AT.FSReview -> NeedsReview
  AT.FSUnsafe -> Unsafe

-- | Convert unified ImportSpec to old Types.FixImport
importSpecToFixImport :: ImportSpec -> AT.FixImport
importSpecToFixImport ImportSpec{..} = AT.FixImport
  { AT.fimpModule = impModule
  , AT.fimpSymbols = map symbolToOld impSymbols
  , AT.fimpQualified = impQualified
  , AT.fimpHiding = impHiding
  , AT.fimpPackage = impPackage
  }
  where
    symbolToOld :: ImportSymbol -> AT.ImportSymbol
    symbolToOld ImportSymbol{..} = AT.ImportSymbol
      { AT.isymName = symName
      , AT.isymType = symbolTypeToOld symType
      , AT.isymChildren = symChildren
      }

    symbolTypeToOld :: SymbolType -> AT.ImportSymbolType
    symbolTypeToOld = \case
      SymFunction -> AT.ISTFunction
      SymOperator -> AT.ISTOperator
      SymType -> AT.ISTType
      SymClass -> AT.ISTClass
      SymConstructor -> AT.ISTConstructor
      SymPattern -> AT.ISTPattern

-- | Convert old Types.FixImport to unified ImportSpec
fixImportToImportSpec :: AT.FixImport -> ImportSpec
fixImportToImportSpec AT.FixImport{..} = ImportSpec
  { impModule = fimpModule
  , impSymbols = map symbolFromOld fimpSymbols
  , impQualified = fimpQualified
  , impHiding = fimpHiding
  , impPackage = fimpPackage
  }
  where
    symbolFromOld :: AT.ImportSymbol -> ImportSymbol
    symbolFromOld AT.ImportSymbol{..} = ImportSymbol
      { symName = isymName
      , symType = symbolTypeFromOld isymType
      , symChildren = isymChildren
      }

    symbolTypeFromOld :: AT.ImportSymbolType -> SymbolType
    symbolTypeFromOld = \case
      AT.ISTFunction -> SymFunction
      AT.ISTOperator -> SymOperator
      AT.ISTType -> SymType
      AT.ISTClass -> SymClass
      AT.ISTConstructor -> SymConstructor
      AT.ISTPattern -> SymPattern

--------------------------------------------------------------------------------
-- Pattern AST Types (for internal pattern matching)
--------------------------------------------------------------------------------

-- | Pattern variable with optional type constraint.
--
-- Represents a metavariable in a pattern that can match and capture
-- expressions from the source code.
--
-- __Naming Convention__:
--
-- * @$X@, @$Y@, @$Z@: General expression metavariables
-- * @$F@, @$G@: Function metavariables
-- * @$XS@, @$YS@: List\/collection metavariables
-- * @$$X@: Repeated metavariable (must match same expression)
--
-- @since 1.0.0
data PatternVar = PatternVar
  { pvName       :: Text
    -- ^ Variable name (e.g., @\"$X\"@, @\"xs\"@).
  , pvConstraint :: Maybe Text
    -- ^ Optional type constraint for type-aware matching.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Pattern AST for parsed patterns.
--
-- This is the internal representation of patterns after parsing.
-- It's different from 'RulePattern' which specifies how patterns
-- are defined in rules (TextPattern, RegexPattern, ASTPattern).
--
-- __Pattern Matching__:
--
-- Each constructor matches the corresponding Haskell syntax:
--
-- @
-- PWildcard         → _              (matches anything)
-- PVar (PatternVar "$X" Nothing) → captures expression as $X
-- PLiteral "42"     → 42             (matches literal)
-- PApplication f x  → f x            (function application)
-- PInfix x "+" y    → x + y          (infix operator)
-- @
--
-- __Example__:
--
-- The pattern @\"foldl $F $Z $XS\"@ parses to:
--
-- @
-- PApplication
--   (PApplication
--     (PApplication (PVar "foldl") (PVar "$F"))
--     (PVar "$Z"))
--   (PVar "$XS")
-- @
--
-- @since 1.0.0
data PatternAST
  = PWildcard                              -- ^ Wildcard: _ (matches anything)
  | PVar PatternVar                        -- ^ Variable: $X, x (captures binding)
  | PLiteral Text                          -- ^ Literal: 42, "hello"
  | PConstructor Text [PatternAST]         -- ^ Constructor: Just x, Nothing
  | PApplication PatternAST PatternAST     -- ^ Application: f x
  | PInfix PatternAST Text PatternAST      -- ^ Infix: x + y
  | PTuple [PatternAST]                    -- ^ Tuple: (x, y, z)
  | PList [PatternAST]                     -- ^ List: [x, y, z]
  | PParens PatternAST                     -- ^ Parenthesized: (x)
  | PTyped PatternAST TypePattern          -- ^ Typed: x :: Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Type pattern for type constraints
data TypePattern
  = TPWildcard                             -- ^ Type wildcard: _
  | TPVar Text                             -- ^ Type variable: a
  | TPConstructor Text [TypePattern]       -- ^ Type constructor: Maybe a
  | TPFunction TypePattern TypePattern     -- ^ Function type: a -> b
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Backward-compatible alias: Pattern = PatternAST
--
-- Modules that use Pattern with PWildcard, PVar, etc. constructors
-- should continue to work. For rule specifications, use RulePattern.
type Pattern = PatternAST

--------------------------------------------------------------------------------
-- Bindings (for pattern substitution)
--------------------------------------------------------------------------------

-- | A single binding from a metavariable to its captured value.
--
-- When a pattern matches, each metavariable (e.g., @$X@) captures the
-- corresponding source text. These bindings are used for:
--
-- * Substitution in replacement patterns
-- * Side condition evaluation
-- * Fix generation
--
-- @since 1.0.0
data Binding = Binding
  { bindingValue :: Text
    -- ^ The captured source text.
  , bindingSpan  :: Maybe (Int, Int)
    -- ^ Optional source span (start offset, end offset) for precise location.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Map of metavariable names to their bindings.
--
-- Used during pattern matching to accumulate captured expressions,
-- and during replacement to substitute metavariables with their values.
--
-- __Example__:
--
-- For pattern @\"foldl $F $Z $XS\"@ matching @\"foldl (+) 0 xs\"@:
--
-- @
-- bindings = Map.fromList
--   [ ("$F", Binding "(+)" Nothing)
--   , ("$Z", Binding "0" Nothing)
--   , ("$XS", Binding "xs" Nothing)
--   ]
-- @
--
-- @since 1.0.0
type Bindings = Map Text Binding

-- | Create a simple binding without span information.
--
-- @since 1.0.0
mkBinding :: Text -> Binding
mkBinding val = Binding
  { bindingValue = val
  , bindingSpan = Nothing
  }

-- | Create a binding with source span information.
--
-- The span is byte offsets into the source file.
--
-- @since 1.0.0
mkBindingWithSpan :: Text -> Int -> Int -> Binding
mkBindingWithSpan val start end = Binding
  { bindingValue = val
  , bindingSpan = Just (start, end)
  }

-- | Empty bindings map.
--
-- Starting point for pattern matching accumulation.
--
-- @since 1.0.0
emptyBindings :: Bindings
emptyBindings = Map.empty

-- | Add a binding to the map.
--
-- If the name already exists, the new value replaces the old.
--
-- @since 1.0.0
addBinding' :: Text  -- ^ Metavariable name (e.g., @\"$X\"@)
            -> Text  -- ^ Captured value
            -> Bindings -> Bindings
addBinding' name val = Map.insert name (mkBinding val)

-- | Lookup a binding's value by metavariable name.
--
-- Returns 'Nothing' if the metavariable was not captured.
--
-- @since 1.0.0
lookupBinding :: Text -> Bindings -> Maybe Text
lookupBinding name bindings = bindingValue <$> Map.lookup name bindings
