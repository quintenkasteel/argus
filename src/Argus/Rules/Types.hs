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
--
-- This module is the SINGLE SOURCE OF TRUTH for all rule-related types in Argus.
-- All rules, whether defined in TOML configuration or Haskell DSL, use these types.
--
-- == Architecture
--
-- The unified type system eliminates the previous fragmentation across modules:
--
-- * One 'Category' enum (replaces DSL.Category, RuleTypes.RuleCategory, Types.FixCategory)
-- * One 'SafetyLevel' enum (replaces DSL.SafetyLevel, RuleTypes.FixSafety, Types.FixSafety)
-- * One 'SideCondition' type (replaces DSL.SideCondition, ASTMatch.SideCondition)
-- * One 'Pattern' type (replaces DSL.Pattern, Rules/Types.Pattern)
-- * One 'ImportSpec' type (replaces DSL.ImportSpec, Types.FixImport)
-- * One 'Rule' type (replaces DSL.Rule, Rules/Types.Rule, ASTMatch.ASTRule)
--
-- == Usage
--
-- Import this module in any code that defines or evaluates rules:
--
-- @
-- import Argus.Rules.Types
--
-- -- Define a rule
-- myRule :: Rule
-- myRule = Rule { ruleId = "my-rule", ... }
-- @
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

-- | Unified rule categories for grouping and bulk enable/disable.
--
-- This enum combines all previously separate category types:
-- * DSL.Category (9 values)
-- * RuleTypes.RuleCategory (13 values)
-- * Types.FixCategory (9 values)
data Category
  = Performance      -- ^ Performance anti-patterns (O(n²), unnecessary allocations)
  | SpaceLeaks       -- ^ Space leak patterns (lazy accumulation, thunk buildup)
  | Security         -- ^ Security vulnerabilities (injection, unsafe IO)
  | Safety           -- ^ Partial functions, unsafe operations
  | Style            -- ^ Code style and formatting
  | Correctness      -- ^ Logic errors, law violations
  | Modernization    -- ^ Use newer APIs/patterns (Applicative over Monad)
  | Imports          -- ^ Import organization and style
  | Naming           -- ^ Naming conventions
  | Extensions       -- ^ Language extension usage
  | Complexity       -- ^ Code complexity (cyclomatic, nesting depth)
  | Concurrency      -- ^ Concurrency anti-patterns (STM, async, race conditions)
  | ErrorHandling    -- ^ Error handling best practices
  | Documentation    -- ^ Documentation completeness and quality
  | Redundant        -- ^ Redundant/unnecessary code
  | Custom Text      -- ^ User-defined category
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
-- This enum combines all previously separate safety types:
-- * DSL.SafetyLevel (Safe, MostlySafe, Unsafe, ManualReview)
-- * RuleTypes.FixSafety (Safe, Unsafe, Manual)
-- * Types.FixSafety (FSAlways, FSMostly, FSReview, FSUnsafe)
data SafetyLevel
  = Safe           -- ^ Always safe to apply automatically
  | MostlySafe     -- ^ Safe in most contexts, rare edge cases
  | NeedsReview    -- ^ Requires human review before applying
  | Unsafe         -- ^ May change semantics, manual only
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
-- This is the comprehensive union of all side condition types:
-- * DSL.SideCondition (TypeCondition, ContextCondition, ExprCondition)
-- * ASTMatch.SideCondition (IsAtom, IsLiteral, HasType, etc.)
--
-- Side conditions allow rules to be more precise about when they apply.
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

-- | THE unified rule type for Argus.
--
-- This is the SINGLE rule type used throughout the system:
-- * TOML configuration parses into this type
-- * Haskell DSL produces this type
-- * Engine evaluates this type
-- * All builtin rules use this type
--
-- Previously there were 4 different Rule types:
-- * DSL.Rule
-- * Rules/Types.Rule
-- * ASTMatch.ASTRule
-- * Config.PatternRule
--
-- All are now unified into this single type.
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

-- | Default rule with all optional fields set to sensible defaults
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

-- | Pattern variable with optional type constraint
data PatternVar = PatternVar
  { pvName       :: Text           -- ^ Variable name (e.g., "$X", "xs")
  , pvConstraint :: Maybe Text     -- ^ Optional type constraint
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Pattern AST for parsed patterns.
--
-- This is the internal representation of patterns after parsing.
-- It's different from the 'Pattern' type which specifies how patterns
-- are defined in rules (TextPattern, RegexPattern, ASTPattern).
--
-- These constructors represent the structure of Haskell expressions:
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

-- | A single binding from a metavariable to its captured value
data Binding = Binding
  { bindingValue :: Text           -- ^ The captured text
  , bindingSpan  :: Maybe (Int, Int)  -- ^ Optional source span (start, end)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Map of metavariable names to their bindings
type Bindings = Map Text Binding

-- | Create a simple binding (no span info)
mkBinding :: Text -> Binding
mkBinding val = Binding
  { bindingValue = val
  , bindingSpan = Nothing
  }

-- | Create a binding with span information
mkBindingWithSpan :: Text -> Int -> Int -> Binding
mkBindingWithSpan val start end = Binding
  { bindingValue = val
  , bindingSpan = Just (start, end)
  }

-- | Empty bindings
emptyBindings :: Bindings
emptyBindings = Map.empty

-- | Add a binding
addBinding' :: Text -> Text -> Bindings -> Bindings
addBinding' name val = Map.insert name (mkBinding val)

-- | Lookup a binding's value
lookupBinding :: Text -> Bindings -> Maybe Text
lookupBinding name bindings = bindingValue <$> Map.lookup name bindings
