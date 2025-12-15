{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : Argus.Rules.DSL
-- Description : Domain-specific language for defining lint rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
-- Portability : GHC
--
-- = Overview
--
-- This module provides an embedded DSL for defining Argus lint rules in Haskell.
-- Rules are defined with a declarative syntax similar to HLint, making it easy
-- to express code transformations and constraints.
--
-- The DSL produces unified 'Rule' values from "Argus.Rules.Types", ensuring
-- consistency between DSL-defined and TOML-defined rules.
--
-- = Architecture
--
-- The DSL is built around three main concepts:
--
-- 1. __Match expressions__: Define what code pattern to match and its replacement
-- 2. __Side conditions__: Constrain when a rule should fire
-- 3. __Rule modifiers__: Configure severity, category, imports, etc.
--
-- @
-- MatchExpr → RuleBuilder → Rule
-- @
--
-- = Key Types
--
-- * 'MatchExpr': Pattern and replacement with conditions
-- * 'RuleBuilder': Intermediate builder for rule construction
-- * 'DSLSideCondition': Constraints on when rules apply
-- * 'DSLImportSpec': Import management for fixes
--
-- = Pattern Syntax
--
-- Patterns use metavariables to capture code fragments:
--
-- * @$X@, @$Y@, @$Z@: Match any identifier
-- * @$F@, @$FUNC@: Match function names
-- * @$T@, @$TYPE@: Match type names
-- * @$N@: Match numeric literals
-- * @$XS@: Match list expressions
--
-- = Thread Safety
--
-- All DSL functions are pure and thread-safe. The resulting 'Rule' values
-- are immutable and can be shared across threads.
--
-- == Basic Usage
--
-- @
-- myRules :: [Rule]
-- myRules =
--   [ rule "avoid-head" $
--       match ("head $X" ==> "headMay $X")
--       & severity Warning
--       & message "Use headMay instead of partial head"
--
--   , rule "prefer-foldl'" $
--       match ("foldl $F $Z $XS" ==> "foldl' $F $Z $XS")
--       & severity Suggestion
--       & message "Use foldl' to avoid space leaks"
--       & category Performance
--   ]
-- @
--
-- == Advanced Usage with Type Constraints
--
-- @
-- advancedRules :: [Rule]
-- advancedRules =
--   [ rule "prefer-traverse" $
--       match ("mapM $F $XS" ==> "traverse $F $XS"
--              \`where_\` (typeOf "$F" \`returns\` "IO _")
--              \`unless\` inContext "parallel")
--       & message "Use traverse instead of mapM for Applicative"
--       & severity Suggestion
--
--   , rule "use-foldMap" $
--       match ("mconcat (map $F $XS)" ==> "foldMap $F $XS"
--              \`where_\` hasClass "$A" "Monoid")
--       & message "Use foldMap for better performance"
--       & category Performance
--
--   , rule "avoid-nub" $
--       match ("nub" ==> "ordNub"
--              \`where_\` hasClass "$X" "Ord")
--       & severity Suggestion
--       & message "nub is O(n²) - use ordNub for O(n log n)"
--   ]
-- @
--
-- == Boolean Condition Combinators
--
-- @
-- -- Combine conditions with .&& and .||
-- rule "complex-condition" $
--   match ("foo $X $Y" ==> "bar $X $Y"
--          \`where_\` (hasClass "$X" "Ord" .&& isNumeric "$Y"))
-- @
module Argus.Rules.DSL
  ( -- * Rule Definition (unified types from Argus.Rules.Types)
    Rule (..)
    -- Category without Complexity (which conflicts with ExprPredicate.Complexity)
  , Category
    ( Performance, SpaceLeaks, Security, Safety, Style
    , Correctness, Modernization, Imports, Naming, Extensions
    , Concurrency, ErrorHandling, Documentation, Redundant, Custom
    )
  , SafetyLevel (..)
  , Severity (..)
  , RuleBuilder
  , rule
  , match
  , matchText
  , (&)

    -- * Match Expressions
  , MatchExpr (..)
  , pat
  , (==>)
  , where_
  , unless
  , when_

    -- * Module Context
  , fromModule
  , toModule
  , fromTo

    -- * Import Management (unified ImportSpec from Argus.Rules.Types)
  , DSLImportSpec (..)  -- DSL builder type
  , importSymbols
  , importQualified
  , addImport
  , addQualifiedImport
  , withImports
  , removeImport
  , removeImports

    -- * Side Condition Combinators
  , (.&&)
  , (.||)
  , neg
  , require

    -- * Rule Modifiers
  , severity
  , message
  , note
  , category
  , fixDescription
  , safetyLevel
  , disabled
  , deprecated
  , within
  , except

    -- * Target Modifiers
  , targetCode
  , targetComments
  , targetDocumentation
  , targetPragmas
  , targetStrings
  , targetAll

    -- * Side Conditions (DSL builder types)
  , DSLSideCondition (..)

    -- * Type Predicates
  , TypePredicate (..)
  , typeOf
  , returns
  , hasType
  , hasClass
  , isNumeric
  , isString
  , isList
  , isMaybe
  , isMonad
  , isPure

    -- * Context Predicates
  , ContextPredicate (..)
  , inContext
  , hasImport
  , hasPragma
  , inModule
  , inTestFile
  , inMainFile

    -- * Expression Predicates
  , ExprPredicate (..)
  , isLiteral
  , isVariable
  , isApplication
  , isLambda
  , isAtomic
  , isSimple
  , notEqual
  , freeIn
  , notFreeIn
  , notUsedIn
  , isEtaReducible
  , notBind
  , complexity
    -- ** Context-aware predicates
  , noDerivingStrategy
  , wildcardNotLast
  , hasOverlap
  , isIncomplete
  , hasAmbiguousType
  , usesDefaultOptions

    -- * Pattern Language (DSL builder types)
  , DSLPattern (..)
  , PatternExpr (..)
  , var
  , lit
  , app
  , op
  , wildcard
  , list
  , tuple
  , guard

    -- * Rule Compilation
  , compileRule
  , compileRules
  , ruleToPatternRule
  , dslSideConditionToUnified
  , dslImportSpecToUnified

    -- * Built-in Rule Sets
  , defaultRuleSet
  , strictRuleSet
  , performanceRuleSet
  , securityRuleSet
  , typeAwareRuleSet

    -- * Legacy Exports (for backwards compatibility)
  , SideCondition
  , Pattern
  , ImportSpec
  , pattern ManualReview
  , sideConditionToAST
  , importSpecToFixImport
  , ruleFixImports
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import Argus.Config (PatternRule(..), RuleSeverity(..))
import Argus.Types (Severity(..), FixImport(..), ImportSymbol(..), ImportSymbolType(..))
import Argus.Rules.Types qualified as RT
import Argus.Rules.Types
  ( Rule(..), SafetyLevel(..)
  -- Category imported with hiding to avoid conflict with ExprPredicate.Complexity
  , Category
    ( Performance, SpaceLeaks, Security, Safety, Style
    , Correctness, Modernization, Imports, Naming, Extensions
    , Concurrency, ErrorHandling, Documentation, Redundant, Custom
    -- NOTE: Complexity is NOT imported - use RT.Complexity if needed
    )
  )
import Argus.Rules.ASTMatch qualified as AST

-- | Legacy alias: ManualReview -> NeedsReview
pattern ManualReview :: SafetyLevel
pattern ManualReview = NeedsReview

--------------------------------------------------------------------------------
-- DSL Types (internal to DSL, produce unified Rule from Argus.Rules.Types)
--------------------------------------------------------------------------------

-- | A DSL pattern for matching code.
--
-- Used during rule building, then converted to 'RulePattern' for evaluation.
-- Most rules use 'PatternText' for simple patterns with metavariables.
--
-- __Example__:
--
-- @
-- PatternText \"head $X\"       -- Matches: head foo, head (bar baz)
-- PatternExpr (PApp (PVar \"f\") (PVar \"x\"))  -- Structured pattern
-- @
--
-- @since 1.0.0
data DSLPattern
  = PatternText Text
    -- ^ Simple text pattern with optional metavariables (@$X@, @$F@, etc.).
  | PatternExpr PatternExpr
    -- ^ Structured expression pattern for complex matching.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Legacy alias for DSLPattern (backwards compatibility)
type Pattern = DSLPattern

-- | Structured pattern expression
data PatternExpr
  = PVar Text                  -- ^ Variable (matches any expression)
  | PLit Text                  -- ^ Literal value
  | PApp PatternExpr PatternExpr  -- ^ Function application
  | POp Text PatternExpr PatternExpr  -- ^ Binary operator
  | PWildcard                  -- ^ Wildcard (matches anything)
  | PList [PatternExpr]        -- ^ List pattern
  | PTuple [PatternExpr]       -- ^ Tuple pattern
  | PGuard PatternExpr PatternExpr  -- ^ Pattern with guard
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- DSL Side Conditions (internal to DSL, converted to unified SideCondition)
--------------------------------------------------------------------------------

-- | DSL side conditions for constraining when a rule matches.
--
-- Side conditions are evaluated after a pattern matches to determine
-- if the rule should fire. They're converted to unified 'SideCondition'
-- from "Argus.Rules.Types" during rule compilation.
--
-- __Condition Types__:
--
-- * 'TypeCondition': Type-based constraints (requires HIE for accuracy)
-- * 'ContextCondition': File\/module context checks
-- * 'ExprCondition': Expression structure checks
--
-- __Combinators__:
--
-- Use '.&&', '.||', and 'neg' to combine conditions:
--
-- @
-- hasClass \"$X\" \"Ord\" .&& notEqual \"$X\" \"$Y\"
-- isNumeric \"$N\" .|| isString \"$S\"
-- neg (isLiteral \"$X\")
-- @
--
-- @since 1.0.0
data DSLSideCondition
  = TypeCondition TypePredicate
    -- ^ Type-based constraint (e.g., @hasClass \"$X\" \"Ord\"@).
  | ContextCondition ContextPredicate
    -- ^ File or module context check (e.g., @inTestFile@).
  | ExprCondition ExprPredicate
    -- ^ Expression structure check (e.g., @isLiteral \"$X\"@).
  | AndCondition DSLSideCondition DSLSideCondition
    -- ^ Both conditions must be true.
  | OrCondition DSLSideCondition DSLSideCondition
    -- ^ At least one condition must be true.
  | NotCondition DSLSideCondition
    -- ^ Negation of a condition.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Legacy alias for DSLSideCondition (backwards compatibility)
type SideCondition = DSLSideCondition

-- | Type-level predicates for variables
data TypePredicate
  = TypeOf Text Text              -- ^ Variable has exact type: typeOf "x" "Int"
  | TypeReturns Text Text         -- ^ Function returns type: typeOf "f" `returns` "IO a"
  | TypeHasClass Text Text        -- ^ Type has class: hasClass "x" "Ord"
  | TypeIsNumeric Text            -- ^ Type is numeric
  | TypeIsString Text             -- ^ Type is String/Text
  | TypeIsList Text               -- ^ Type is a list
  | TypeIsMaybe Text              -- ^ Type is Maybe
  | TypeIsMonad Text Text         -- ^ Type is in specific monad
  | TypeIsPure Text               -- ^ Expression is pure (no IO)
  | TypeMatches Text Text         -- ^ Type matches pattern with wildcards (_)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Context predicates for where the code appears
data ContextPredicate
  = InContext Text                -- ^ Generic context: "parallel", "test", "unsafe"
  | HasImport Text                -- ^ Module has import: hasImport "Data.List"
  | HasPragma Text                -- ^ File has pragma: hasPragma "OverloadedStrings"
  | InModule Text                 -- ^ In specific module pattern: inModule "Test.*"
  | InTestFile                    -- ^ In a test file (*Spec.hs, *Test.hs)
  | InMainFile                    -- ^ In main production code
  | NotInContext ContextPredicate -- ^ Negation
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Expression-level predicates
data ExprPredicate
  = IsLiteral Text                -- ^ Variable is a literal
  | IsVariable Text               -- ^ Variable is a simple identifier
  | IsApplication Text            -- ^ Variable is a function application
  | IsLambda Text                 -- ^ Variable is a lambda
  | IsAtomic Text                 -- ^ Variable is atomic (no subexpressions)
  | IsSimple Text                 -- ^ Variable is simple (no complex subexprs)
  | NotEqual Text Text            -- ^ Two variables are not equal
  | FreeIn Text Text              -- ^ First var is free in second's expression
  | NotFreeIn Text Text           -- ^ First var is NOT free in second's expression
  | NotUsedIn Text Text           -- ^ First var is not used in second's expression
  | IsEtaReducible Text Text      -- ^ Function can be eta-reduced
  | NotBind Text                  -- ^ Expression is not a monadic bind
  | Complexity Text Ordering Int  -- ^ Expression complexity constraint
  -- Context-aware predicates (for pattern matching, deriving, etc.)
  | NoDerivingStrategy            -- ^ Deriving clause has no explicit strategy
  | WildcardNotLast               -- ^ Wildcard pattern is not the last case
  | HasOverlap                    -- ^ Case patterns have overlap
  | IsIncomplete                  -- ^ Pattern match is incomplete
  | HasAmbiguousType              -- ^ Expression has ambiguous type
  | UsesDefaultOptions            -- ^ Uses default aeson/etc options
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Match Expression Type
--------------------------------------------------------------------------------

-- | A match expression holding pattern, optional replacement, conditions,
-- and module/import context for complete refactoring support.
--
-- This is the core of the DSL - conditions and module context are attached
-- to the match expression itself, enabling automatic import management.
--
-- == Basic Usage
-- @
--   match ("nub" ==> "ordNub" `where_` hasClass "$X" "Ord")
--   & severity Suggestion
-- @
--
-- == With Module Context
-- @
--   match ("nub $X" ==> "ordNub $X"
--          `fromModule` "Data.List"
--          `toModule` "Data.Containers.ListUtils")
--   & severity Suggestion
-- @
--
-- == With Import Management
-- @
--   match ("foldl $F $Z $X" ==> "foldl' $F $Z $X")
--   & addImport "Data.List" ["foldl'"]
-- @
data MatchExpr = MatchExpr
  { mePattern       :: Text
  , meReplacement   :: Maybe Text
  , meConditions    :: [DSLSideCondition]
  , meSourceModule  :: Maybe Text       -- ^ Module the matched function is from
  , meTargetModule  :: Maybe Text       -- ^ Module the replacement is from
  , meAddImports    :: [DSLImportSpec]  -- ^ Imports to add when fix is applied
  , meRemoveImports :: [Text]           -- ^ Module names to remove from imports
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | DSL import specification (converted to unified ImportSpec)
data DSLImportSpec = DSLImportSpec
  { isModule    :: Text           -- ^ Module name (e.g., "Data.List")
  , isSymbols   :: [Text]         -- ^ Symbols to import (empty = entire module)
  , isQualified :: Maybe Text     -- ^ Qualifier (e.g., Just "L" for "qualified as L")
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Legacy alias for DSLImportSpec (backwards compatibility)
type ImportSpec = DSLImportSpec

-- | Create an import spec for a module with specific symbols
importSymbols :: Text -> [Text] -> DSLImportSpec
importSymbols modName syms = DSLImportSpec modName syms Nothing

-- | Create a qualified import spec
importQualified :: Text -> Text -> DSLImportSpec
importQualified modName qualifier = DSLImportSpec modName [] (Just qualifier)

-- | Create a match expression from a pattern (no replacement)
pat :: Text -> MatchExpr
pat p = MatchExpr
  { mePattern = p
  , meReplacement = Nothing
  , meConditions = []
  , meSourceModule = Nothing
  , meTargetModule = Nothing
  , meAddImports = []
  , meRemoveImports = []
  }

-- | Specify a replacement for a pattern
-- Usage: "head $X" ==> "headMay $X"
(==>) :: Text -> Text -> MatchExpr
pattern' ==> replacement = MatchExpr
  { mePattern = pattern'
  , meReplacement = Just replacement
  , meConditions = []
  , meSourceModule = Nothing
  , meTargetModule = Nothing
  , meAddImports = []
  , meRemoveImports = []
  }

infixl 4 ==>

--------------------------------------------------------------------------------
-- Module Context Operators
--------------------------------------------------------------------------------

-- | Specify the source module for the matched pattern
-- Usage: "nub $X" ==> "ordNub $X" `fromModule` "Data.List"
fromModule :: MatchExpr -> Text -> MatchExpr
fromModule me modName = me { meSourceModule = Just modName }

infixl 3 `fromModule`

-- | Specify the target module for the replacement
-- Usage: "nub $X" ==> "ordNub $X" `toModule` "Data.Containers.ListUtils"
toModule :: MatchExpr -> Text -> MatchExpr
toModule me modName = me
  { meTargetModule = Just modName
  -- Auto-add import for the target module if we have a replacement
  , meAddImports = case meReplacement me of
      Just repl ->
        let funcName = extractFunctionName repl
        in if T.null funcName
           then meAddImports me
           else DSLImportSpec modName [funcName] Nothing : meAddImports me
      Nothing -> meAddImports me
  }

infixl 3 `toModule`

-- | Extract the first function name from a replacement pattern
extractFunctionName :: Text -> Text
extractFunctionName repl =
  let tokens = T.words repl
  in case tokens of
       (t:_) | not (T.isPrefixOf "$" t) -> t
       _ -> ""

-- | Specify both source and target modules
-- Usage: "nub $X" ==> "ordNub $X" `fromTo` ("Data.List", "Data.Containers.ListUtils")
fromTo :: MatchExpr -> (Text, Text) -> MatchExpr
fromTo me (srcMod, tgtMod) = me `fromModule` srcMod `toModule` tgtMod

infixl 3 `fromTo`

-- | Add a side condition to a match expression
-- Usage: "nub $X" ==> "ordNub $X" `where_` hasClass "$X" "Ord"
where_ :: MatchExpr -> SideCondition -> MatchExpr
where_ me cond = me { meConditions = cond : meConditions me }

infixl 3 `where_`

-- | Add a negated side condition to a match expression
-- Usage: "head $X" ==> "headMay $X" `unless` isLiteral "$X"
unless :: MatchExpr -> SideCondition -> MatchExpr
unless me cond = me { meConditions = NotCondition cond : meConditions me }

infixl 3 `unless`

-- | Alias for where_ (reads naturally in some cases)
when_ :: MatchExpr -> SideCondition -> MatchExpr
when_ = where_

infixl 3 `when_`

--------------------------------------------------------------------------------
-- Side Condition Combinators
--------------------------------------------------------------------------------

-- | Combine two conditions with AND
-- Usage: hasClass "$X" "Ord" .&& notFreeIn "$Y" "$X"
(.&&) :: SideCondition -> SideCondition -> SideCondition
(.&&) = AndCondition

infixr 3 .&&

-- | Combine two conditions with OR
-- Usage: isNumeric "$X" .|| isString "$X"
(.||) :: SideCondition -> SideCondition -> SideCondition
(.||) = OrCondition

infixr 2 .||

-- | Negate a condition
-- Usage: neg (isLiteral "$X")
neg :: SideCondition -> SideCondition
neg = NotCondition

-- | Add multiple conditions to a MatchExpr (all must be true)
-- Usage: "nub $X" ==> "ordNub $X" `require` [hasClass "$X" "Ord", notFreeIn "$Y" "$X"]
require :: MatchExpr -> [SideCondition] -> MatchExpr
require me conds = me { meConditions = conds ++ meConditions me }

--------------------------------------------------------------------------------
-- Import Management Operators
--------------------------------------------------------------------------------

-- | Add an import to a match expression
-- Usage: "nub $X" ==> "ordNub $X" `addImport` ("Data.Containers.ListUtils", ["ordNub"])
addImport :: MatchExpr -> (Text, [Text]) -> MatchExpr
addImport me (modName, syms) = me
  { meAddImports = DSLImportSpec modName syms Nothing : meAddImports me }

infixl 3 `addImport`

-- | Add a qualified import to a match expression
-- Usage: "nub $X" ==> "M.sort $X" `addQualifiedImport` ("Data.Map.Strict", "M")
addQualifiedImport :: MatchExpr -> (Text, Text) -> MatchExpr
addQualifiedImport me (modName, qualifier) = me
  { meAddImports = DSLImportSpec modName [] (Just qualifier) : meAddImports me }

infixl 3 `addQualifiedImport`

-- | Add multiple imports to a match expression
-- Usage: "foo" ==> "bar" `withImports` [importSymbols "Data.List" ["nub"], importQualified "Data.Map" "M"]
withImports :: MatchExpr -> [DSLImportSpec] -> MatchExpr
withImports me imports = me
  { meAddImports = imports ++ meAddImports me }

infixl 3 `withImports`

-- | Remove a module from imports when the fix is applied
-- Usage: "unsafePerformIO" ==> "" `removeImport` "System.IO.Unsafe"
removeImport :: MatchExpr -> Text -> MatchExpr
removeImport me modName = me
  { meRemoveImports = modName : meRemoveImports me }

infixl 3 `removeImport`

-- | Remove multiple modules from imports
removeImports :: MatchExpr -> [Text] -> MatchExpr
removeImports me modNames = me
  { meRemoveImports = modNames ++ meRemoveImports me }

infixl 3 `removeImports`

--------------------------------------------------------------------------------
-- Rule Builder DSL
--------------------------------------------------------------------------------

-- | Builder for constructing rules (intermediate DSL type).
--
-- 'RuleBuilder' accumulates rule configuration through modifier functions.
-- Create with 'match' or 'matchText', modify with '&', finalize with 'rule'.
--
-- __Construction Pattern__:
--
-- @
-- rule \"my-rule\" $
--   match (\"oldCode\" ==> \"newCode\")
--   & severity Warning
--   & message \"Use newCode instead\"
--   & category Performance
-- @
--
-- __Default Values__:
--
-- * Severity: 'Warning'
-- * Category: 'Style'
-- * Safety: 'Safe'
-- * Enabled: 'True'
--
-- @since 1.0.0
data RuleBuilder = RuleBuilder
  { rbName          :: Text
    -- ^ Rule identifier (set by 'rule').
  , rbPattern       :: DSLPattern
    -- ^ Pattern to match in source code.
  , rbReplacement   :: Maybe DSLPattern
    -- ^ Optional replacement for auto-fix.
  , rbSeverity      :: Severity
    -- ^ Diagnostic severity level.
  , rbMessage       :: Text
    -- ^ User-facing diagnostic message.
  , rbNote          :: Maybe Text
    -- ^ Additional explanation or guidance.
  , rbCategory      :: Category
    -- ^ Rule category for filtering.
  , rbFixDesc       :: Maybe Text
    -- ^ Description for the fix action.
  , rbSafetyLevel   :: SafetyLevel
    -- ^ How safe is the auto-fix.
  , rbEnabled       :: Bool
    -- ^ Whether rule is active.
  , rbDeprecated    :: Maybe Text
    -- ^ Deprecation message if rule is deprecated.
  , rbConditions    :: [DSLSideCondition]
    -- ^ Side conditions that must hold for match.
  , rbWithin        :: [Text]
    -- ^ Module patterns where rule applies.
  , rbExcept        :: [Text]
    -- ^ Module patterns to exclude.
  , rbSourceModule  :: Maybe Text
    -- ^ Expected source module of matched function.
  , rbTargetModule  :: Maybe Text
    -- ^ Module containing the replacement.
  , rbAddImports    :: [DSLImportSpec]
    -- ^ Imports to add when fix is applied.
  , rbRemoveImports :: [Text]
    -- ^ Modules to remove from imports.
  , rbTarget        :: Maybe RT.RuleTarget
    -- ^ Where to match: code, comments, pragmas, etc.
    -- 'Nothing' = infer from category.
  }

-- | Create a rule from a name and builder.
--
-- This is the primary entry point for defining rules. It converts the
-- DSL-specific 'RuleBuilder' into a unified 'Rule' from "Argus.Rules.Types".
--
-- __Parameters__:
--
-- * @name@: Unique rule identifier (e.g., @\"avoid-head\"@, @\"prefer-foldl\'\"@)
-- * @builder@: Configured rule builder from 'match' and modifiers
--
-- __Returns__:
--
-- A unified 'Rule' ready for use in the engine.
--
-- __Example__:
--
-- @
-- avoidHead :: Rule
-- avoidHead = rule \"avoid-head\" $
--   match (\"head $X\" ==> \"headMay $X\")
--   & severity Warning
--   & message \"Use headMay to handle empty lists safely\"
--   & category Safety
-- @
--
-- @since 1.0.0
rule :: Text -> RuleBuilder -> Rule
rule name builder = Rule
  { ruleId = name
  , ruleCategory = rbCategory builder
  , ruleSeverity = rbSeverity builder
  , ruleMessage = rbMessage builder
  , ruleExplanation = Nothing
  , rulePattern = dslPatternToRulePattern (rbPattern builder)
  , ruleReplacement = fmap dslPatternToText (rbReplacement builder)
  , ruleConditions = map dslSideConditionToUnified (rbConditions builder)
  , ruleSafety = rbSafetyLevel builder
  , ruleAddImports = map dslImportSpecToUnified (rbAddImports builder)
  , ruleRemoveImports = rbRemoveImports builder
  , ruleEnabled = rbEnabled builder
  , ruleWithin = rbWithin builder
  , ruleExcept = rbExcept builder
  , ruleDeprecated = rbDeprecated builder
  , ruleTags = []
  , ruleReferences = []
  , ruleNote = rbNote builder
  , ruleFixDescription = rbFixDesc builder
  , ruleSourceModule = rbSourceModule builder
  , ruleTargetModule = rbTargetModule builder
  , ruleTarget = rbTarget builder  -- Nothing = infer from category
  }

-- | Convert a 'MatchExpr' to a 'RuleBuilder'.
--
-- This is the standard way to start building a rule. The 'MatchExpr'
-- contains the pattern, optional replacement, and any side conditions.
--
-- __Usage Patterns__:
--
-- @
-- -- Simple replacement
-- match (\"head\" ==> \"headMay\")
--
-- -- With conditions
-- match (\"nub $X\" ==> \"ordNub $X\" \`where_\` hasClass \"$X\" \"Ord\")
--
-- -- With module context
-- match (\"foldl\" ==> \"foldl'\" \`fromModule\` \"Prelude\" \`toModule\` \"Data.List\")
--
-- -- Detection only (no fix)
-- match (pat \"unsafePerformIO\")
-- @
--
-- @since 1.0.0
match :: MatchExpr -> RuleBuilder
match MatchExpr{..} = RuleBuilder
  { rbName = ""
  , rbPattern = PatternText mePattern
  , rbReplacement = fmap PatternText meReplacement
  , rbSeverity = Warning
  , rbMessage = "Pattern matched: " <> mePattern
  , rbNote = Nothing
  , rbCategory = Style
  , rbFixDesc = Nothing
  , rbSafetyLevel = Safe
  , rbEnabled = True
  , rbDeprecated = Nothing
  , rbConditions = meConditions
  , rbWithin = []
  , rbExcept = []
  , rbSourceModule = meSourceModule
  , rbTargetModule = meTargetModule
  , rbAddImports = meAddImports
  , rbRemoveImports = meRemoveImports
  , rbTarget = Nothing  -- Inferred from category
  }

-- | Create a match expression from just a pattern text (convenience)
-- This allows: match (pat "head")
-- Or just use the MatchExpr directly: match ("head" ==> "headMay")
matchText :: Text -> RuleBuilder
matchText p = match (pat p)

-- | Chain modifier (standard function application)
(&) :: a -> (a -> b) -> b
x & f = f x
{-# INLINE (&) #-}

infixl 1 &

--------------------------------------------------------------------------------
-- Type Predicate Builders
--------------------------------------------------------------------------------

-- | Create a type predicate for a variable
-- Usage: typeOf "f" `returns` "IO _"
-- Usage: typeOf "x" "Int"
typeOf :: Text -> TypePredicateBuilder
typeOf varName = TypePredicateBuilder varName

-- | Intermediate builder for typeOf
data TypePredicateBuilder = TypePredicateBuilder Text
  deriving stock (Eq, Show)

-- | Specify the return type of a function variable
-- Usage: typeOf "f" `returns` "IO _"
returns :: TypePredicateBuilder -> Text -> SideCondition
returns (TypePredicateBuilder varName) returnType =
  TypeCondition $ TypeReturns varName returnType

infixl 5 `returns`

-- | Directly specify a type for a variable
-- Usage: hasType "x" "Int"
hasType :: Text -> Text -> SideCondition
hasType varName typeName = TypeCondition $ TypeOf varName typeName

-- | Check if a variable's type has a typeclass instance
-- Usage: hasClass "x" "Ord"
hasClass :: Text -> Text -> SideCondition
hasClass varName className = TypeCondition $ TypeHasClass varName className

-- | Check if a variable has numeric type
isNumeric :: Text -> SideCondition
isNumeric varName = TypeCondition $ TypeIsNumeric varName

-- | Check if a variable has String/Text type
isString :: Text -> SideCondition
isString varName = TypeCondition $ TypeIsString varName

-- | Check if a variable has list type
isList :: Text -> SideCondition
isList varName = TypeCondition $ TypeIsList varName

-- | Check if a variable has Maybe type
isMaybe :: Text -> SideCondition
isMaybe varName = TypeCondition $ TypeIsMaybe varName

-- | Check if a variable is in a specific monad
-- Usage: isMonad "x" "IO"
isMonad :: Text -> Text -> SideCondition
isMonad varName monadName = TypeCondition $ TypeIsMonad varName monadName

-- | Check if an expression is pure (no IO)
isPure :: Text -> SideCondition
isPure varName = TypeCondition $ TypeIsPure varName

--------------------------------------------------------------------------------
-- Context Predicate Builders
--------------------------------------------------------------------------------

-- | Check if code is in a specific context
-- Supported contexts: "parallel", "test", "unsafe", "lens", "mtl"
inContext :: Text -> SideCondition
inContext ctx = ContextCondition $ InContext ctx

-- | Check if module has a specific import
hasImport :: Text -> SideCondition
hasImport modName = ContextCondition $ HasImport modName

-- | Check if file has a specific LANGUAGE pragma
hasPragma :: Text -> SideCondition
hasPragma pragma = ContextCondition $ HasPragma pragma

-- | Check if in a specific module (pattern)
inModule :: Text -> SideCondition
inModule modPattern = ContextCondition $ InModule modPattern

-- | Check if in a test file
inTestFile :: SideCondition
inTestFile = ContextCondition InTestFile

-- | Check if in main (non-test) file
inMainFile :: SideCondition
inMainFile = ContextCondition InMainFile

--------------------------------------------------------------------------------
-- Expression Predicate Builders
--------------------------------------------------------------------------------

-- | Check if a variable is bound to a literal
isLiteral :: Text -> SideCondition
isLiteral varName = ExprCondition $ IsLiteral varName

-- | Check if a variable is a simple identifier
isVariable :: Text -> SideCondition
isVariable varName = ExprCondition $ IsVariable varName

-- | Check if a variable is a function application
isApplication :: Text -> SideCondition
isApplication varName = ExprCondition $ IsApplication varName

-- | Check if a variable is a lambda expression
isLambda :: Text -> SideCondition
isLambda varName = ExprCondition $ IsLambda varName

-- | Check if a variable is atomic (no subexpressions)
isAtomic :: Text -> SideCondition
isAtomic varName = ExprCondition $ IsAtomic varName

-- | Check that two variables are not equal
notEqual :: Text -> Text -> SideCondition
notEqual var1 var2 = ExprCondition $ NotEqual var1 var2

-- | Check that first variable is free in second's expression
freeIn :: Text -> Text -> SideCondition
freeIn var1 var2 = ExprCondition $ FreeIn var1 var2

-- | Check that first variable is NOT free in second's expression
notFreeIn :: Text -> Text -> SideCondition
notFreeIn var1 var2 = ExprCondition $ NotFreeIn var1 var2

-- | Check expression complexity
-- Usage: complexity "x" LT 5 (expression must have complexity < 5)
complexity :: Text -> Ordering -> Int -> SideCondition
complexity varName ord limit = ExprCondition $ Complexity varName ord limit

-- | Check if a variable is simple (no complex subexpressions)
isSimple :: Text -> SideCondition
isSimple varName = ExprCondition $ IsSimple varName

-- | Check that first variable is not used in second's expression
-- Usage: notUsedIn "$F" "$X" (F doesn't contain X)
notUsedIn :: Text -> Text -> SideCondition
notUsedIn var1 var2 = ExprCondition $ NotUsedIn var1 var2

-- | Check if a function can be eta-reduced
-- Usage: isEtaReducible "$F" "$X"
isEtaReducible :: Text -> Text -> SideCondition
isEtaReducible funcVar argVar = ExprCondition $ IsEtaReducible funcVar argVar

-- | Check that expression is not a monadic bind
-- Usage: notBind "$X"
notBind :: Text -> SideCondition
notBind varName = ExprCondition $ NotBind varName

-- | Check that deriving clause has no explicit strategy
noDerivingStrategy :: SideCondition
noDerivingStrategy = ExprCondition NoDerivingStrategy

-- | Check that wildcard pattern is not the last case
wildcardNotLast :: SideCondition
wildcardNotLast = ExprCondition WildcardNotLast

-- | Check that case patterns have overlap
hasOverlap :: SideCondition
hasOverlap = ExprCondition HasOverlap

-- | Check that pattern match is incomplete
isIncomplete :: SideCondition
isIncomplete = ExprCondition IsIncomplete

-- | Check that expression has ambiguous type
hasAmbiguousType :: SideCondition
hasAmbiguousType = ExprCondition HasAmbiguousType

-- | Check that uses default options (for Aeson, etc.)
usesDefaultOptions :: SideCondition
usesDefaultOptions = ExprCondition UsesDefaultOptions

--------------------------------------------------------------------------------
-- Rule Modifiers
--------------------------------------------------------------------------------

-- | Set the severity level
severity :: Severity -> RuleBuilder -> RuleBuilder
severity s builder = builder { rbSeverity = s }

-- | Set the message
message :: Text -> RuleBuilder -> RuleBuilder
message m builder = builder { rbMessage = m }

-- | Add a note/explanation
note :: Text -> RuleBuilder -> RuleBuilder
note n builder = builder { rbNote = Just n }

-- | Set the category
category :: Category -> RuleBuilder -> RuleBuilder
category c builder = builder { rbCategory = c }

-- | Set the fix description
fixDescription :: Text -> RuleBuilder -> RuleBuilder
fixDescription d builder = builder { rbFixDesc = Just d }

-- | Set the safety level
safetyLevel :: SafetyLevel -> RuleBuilder -> RuleBuilder
safetyLevel s builder = builder { rbSafetyLevel = s }

-- | Disable the rule
disabled :: RuleBuilder -> RuleBuilder
disabled builder = builder { rbEnabled = False }

-- | Mark as deprecated with reason
deprecated :: Text -> RuleBuilder -> RuleBuilder
deprecated reason builder = builder { rbDeprecated = Just reason }

-- | Set module patterns where rule applies
within :: [Text] -> RuleBuilder -> RuleBuilder
within patterns builder = builder { rbWithin = patterns }

-- | Set module patterns to exclude
except :: [Text] -> RuleBuilder -> RuleBuilder
except patterns builder = builder { rbExcept = patterns }

--------------------------------------------------------------------------------
-- Target Modifiers (for special rules that override category-based inference)
--------------------------------------------------------------------------------

-- | Explicitly target code only (skip comments, strings, pragmas)
-- This is the default for most categories except Documentation.
targetCode :: RuleBuilder -> RuleBuilder
targetCode builder = builder { rbTarget = Just RT.TargetCode }

-- | Target all comments (line and block)
-- Use this for rules that check comment content.
targetComments :: RuleBuilder -> RuleBuilder
targetComments builder = builder { rbTarget = Just RT.TargetComments }

-- | Target documentation comments (Haddock)
-- This is the default for the Documentation category.
targetDocumentation :: RuleBuilder -> RuleBuilder
targetDocumentation builder = builder { rbTarget = Just RT.TargetDocumentation }

-- | Target pragma comments ({-# ... #-})
-- Use for rules about LANGUAGE pragmas, OPTIONS, etc.
targetPragmas :: RuleBuilder -> RuleBuilder
targetPragmas builder = builder { rbTarget = Just RT.TargetPragmas }

-- | Target string literals
-- Use for rules that check string content.
targetStrings :: RuleBuilder -> RuleBuilder
targetStrings builder = builder { rbTarget = Just RT.TargetStrings }

-- | Target everything (no filtering)
-- Use sparingly - matches in comments, strings, and code.
targetAll :: RuleBuilder -> RuleBuilder
targetAll builder = builder { rbTarget = Just RT.TargetAll }

--------------------------------------------------------------------------------
-- Pattern Constructors
--------------------------------------------------------------------------------

-- | Variable pattern (matches any expression and binds it)
var :: Text -> PatternExpr
var = PVar

-- | Literal pattern
lit :: Text -> PatternExpr
lit = PLit

-- | Application pattern
app :: PatternExpr -> PatternExpr -> PatternExpr
app = PApp

-- | Operator pattern
op :: Text -> PatternExpr -> PatternExpr -> PatternExpr
op = POp

-- | Wildcard pattern
wildcard :: PatternExpr
wildcard = PWildcard

-- | List pattern
list :: [PatternExpr] -> PatternExpr
list = PList

-- | Tuple pattern
tuple :: [PatternExpr] -> PatternExpr
tuple = PTuple

-- | Guard pattern
guard :: PatternExpr -> PatternExpr -> PatternExpr
guard = PGuard

--------------------------------------------------------------------------------
-- Rule Compilation & Conversion Functions
--------------------------------------------------------------------------------

-- | Convert DSLPattern to unified RulePattern
dslPatternToRulePattern :: DSLPattern -> RT.RulePattern
dslPatternToRulePattern (PatternText t) = RT.TextPatternSpec t
dslPatternToRulePattern (PatternExpr e) = RT.TextPatternSpec (exprToText e)

-- | Convert DSLPattern to text
dslPatternToText :: DSLPattern -> Text
dslPatternToText (PatternText t) = t
dslPatternToText (PatternExpr e) = exprToText e

-- | Convert DSLSideCondition to unified SideCondition
dslSideConditionToUnified :: DSLSideCondition -> RT.SideCondition
dslSideConditionToUnified = \case
  TypeCondition tp -> typePredToUnified tp
  ContextCondition cp -> contextPredToUnified cp
  ExprCondition ep -> exprPredToUnified ep
  AndCondition c1 c2 -> RT.And [dslSideConditionToUnified c1, dslSideConditionToUnified c2]
  OrCondition c1 c2 -> RT.Or [dslSideConditionToUnified c1, dslSideConditionToUnified c2]
  NotCondition c -> RT.Not (dslSideConditionToUnified c)

-- | Convert TypePredicate to unified SideCondition
typePredToUnified :: TypePredicate -> RT.SideCondition
typePredToUnified = \case
  TypeOf varName ty -> RT.HasType varName ty
  TypeReturns varName ty -> RT.TypeMatches varName ("* -> " <> ty)
  TypeHasClass varName cls -> RT.HasTypeClass varName cls
  TypeIsNumeric varName -> RT.IsNumeric varName
  TypeIsString varName -> RT.IsString varName
  TypeIsList varName -> RT.IsList varName
  TypeIsMaybe varName -> RT.IsMaybe varName
  TypeIsMonad varName monad -> RT.IsMonad varName monad
  TypeIsPure varName -> RT.IsPure varName
  TypeMatches varName patText -> RT.TypeMatches varName patText

-- | Convert ContextPredicate to unified SideCondition
contextPredToUnified :: ContextPredicate -> RT.SideCondition
contextPredToUnified = \case
  InContext ctx -> RT.InContext ctx
  HasImport modName -> RT.HasImport modName
  HasPragma p -> RT.HasPragma p
  InModule m -> RT.InModule m
  InTestFile -> RT.InTestFile
  InMainFile -> RT.NotInTestFile
  NotInContext cp -> RT.Not (contextPredToUnified cp)

-- | Convert ExprPredicate to unified SideCondition
exprPredToUnified :: ExprPredicate -> RT.SideCondition
exprPredToUnified = \case
  IsLiteral varName -> RT.IsLiteral varName
  IsVariable varName -> RT.IsVariable varName
  IsApplication varName -> RT.IsApplication varName
  IsLambda varName -> RT.IsLambda varName
  IsAtomic varName -> RT.IsAtomic varName
  IsSimple varName -> RT.IsAtomic varName  -- Simple ≈ Atomic
  NotEqual v1 v2 -> RT.NotEqual v1 v2
  FreeIn v1 v2 -> RT.FreeIn v1 v2
  NotFreeIn v1 v2 -> RT.NotFreeIn v1 v2
  NotUsedIn v1 v2 -> RT.NotFreeIn v1 v2  -- Same semantics
  IsEtaReducible f x -> RT.IsEtaReducible f x
  NotBind varName -> RT.NotBind varName
  Complexity varName ord limit -> case ord of
    LT -> RT.ComplexityLT varName limit
    GT -> RT.ComplexityGT varName limit
    EQ -> RT.And [RT.ComplexityLT varName (limit + (1 :: Int)), RT.ComplexityGT varName (limit - (1 :: Int))]
  NoDerivingStrategy -> RT.NoDerivingStrategy
  WildcardNotLast -> RT.WildcardNotLast
  HasOverlap -> RT.HasPatternOverlap
  IsIncomplete -> RT.IsPatternIncomplete
  HasAmbiguousType -> RT.HasAmbiguousType
  UsesDefaultOptions -> RT.UsesDefaultOptions

-- | Convert DSLImportSpec to unified ImportSpec
dslImportSpecToUnified :: DSLImportSpec -> RT.ImportSpec
dslImportSpecToUnified DSLImportSpec{..} = RT.ImportSpec
  { RT.impModule = isModule
  , RT.impSymbols = map mkUnifiedSymbol isSymbols
  , RT.impQualified = isQualified
  , RT.impHiding = False
  , RT.impPackage = Nothing
  }
  where
    mkUnifiedSymbol :: Text -> RT.ImportSymbol
    mkUnifiedSymbol name = RT.ImportSymbol
      { RT.symName = name
      , RT.symType = inferUnifiedSymbolType name
      , RT.symChildren = []
      }

    inferUnifiedSymbolType :: Text -> RT.SymbolType
    inferUnifiedSymbolType name
      | T.null name = RT.SymFunction
      | T.any (`elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)) name = RT.SymOperator
      | T.head name >= 'A' && T.head name <= 'Z' = RT.SymType
      | otherwise = RT.SymFunction

-- | Convert a unified Rule to a PatternRule for the pattern checker
ruleToPatternRule :: Rule -> PatternRule
ruleToPatternRule Rule{..} = PatternRule
  { prName = ruleId
  , prMatch = RT.rulePatternToText rulePattern
  , prFix = ruleReplacement
  , prWhere = unifiedConditionsToWhere ruleConditions
  , prSeverity = severityToRS ruleSeverity
  , prMessage = ruleMessage
  }

-- | Convert unified side conditions to a where clause text
unifiedConditionsToWhere :: [RT.SideCondition] -> Maybe Text
unifiedConditionsToWhere [] = Nothing
unifiedConditionsToWhere conds = Just $ T.intercalate ", " (map unifiedConditionToText conds)

-- | Convert a unified condition to text (for PatternRule)
unifiedConditionToText :: RT.SideCondition -> Text
unifiedConditionToText = \case
  RT.NotInComment -> "notInComment"
  RT.NotInString -> "notInString"
  RT.NotInImport -> "notInImport"
  RT.InFunctionBody -> "inFunctionBody"
  RT.InCommentType ct -> "inCommentType " <> commentTypeToText ct
  RT.InStringLiteral -> "inStringLiteral"
  RT.HasType varName ty -> varName <> " :: " <> ty
  RT.HasTypeClass varName cls -> varName <> " has " <> cls
  RT.TypeMatches varName patText -> varName <> " ~ " <> patText
  RT.IsNumeric varName -> "isNumeric " <> varName
  RT.IsString varName -> "isString " <> varName
  RT.IsList varName -> "isList " <> varName
  RT.IsMaybe varName -> "isMaybe " <> varName
  RT.IsMonad varName monad -> varName <> " in " <> monad
  RT.IsPure varName -> "isPure " <> varName
  RT.IsLiteral varName -> "isLiteral " <> varName
  RT.IsVariable varName -> "isVariable " <> varName
  RT.IsApplication varName -> "isApplication " <> varName
  RT.IsLambda varName -> "isLambda " <> varName
  RT.IsAtomic varName -> "isAtomic " <> varName
  RT.IsConstructor varName -> "isConstructor " <> varName
  RT.NotEqual v1 v2 -> v1 <> " /= " <> v2
  RT.FreeIn v1 v2 -> v1 <> " freeIn " <> v2
  RT.NotFreeIn v1 v2 -> v1 <> " notFreeIn " <> v2
  RT.ComplexityLT varName n -> "complexity " <> varName <> " < " <> T.pack (show n)
  RT.ComplexityGT varName n -> "complexity " <> varName <> " > " <> T.pack (show n)
  RT.HasImport modName -> "hasImport " <> modName
  RT.HasPragma pragma -> "hasPragma " <> pragma
  RT.InModule moduleName -> "inModule " <> moduleName
  RT.InTestFile -> "inTestFile"
  RT.NotInTestFile -> "notInTestFile"
  RT.And cs -> "(" <> T.intercalate " && " (map unifiedConditionToText cs) <> ")"
  RT.Or cs -> "(" <> T.intercalate " || " (map unifiedConditionToText cs) <> ")"
  RT.Not c -> "!(" <> unifiedConditionToText c <> ")"
  RT.Always -> "always"
  RT.Never -> "never"
  RT.NotIn varName vals -> varName <> " notIn " <> T.intercalate "," vals
  RT.TypeContains varName ty -> varName <> " contains " <> ty
  RT.ComplexityCond varName ord n -> "complexity " <> varName <> " " <> showOrd ord <> " " <> T.pack (show n)
  -- New expression structure predicates
  RT.NotBind varName -> "notBind " <> varName
  RT.IsEtaReducible f x -> "isEtaReducible " <> f <> " " <> x
  -- Deriving and pattern analysis predicates
  RT.NoDerivingStrategy -> "noDerivingStrategy"
  RT.WildcardNotLast -> "wildcardNotLast"
  RT.HasPatternOverlap -> "hasOverlap"
  RT.IsPatternIncomplete -> "isIncomplete"
  RT.HasAmbiguousType -> "hasAmbiguousType"
  RT.UsesDefaultOptions -> "usesDefaultOptions"
  -- Context predicates
  RT.InContext ctx -> "inContext " <> ctx
  where
    showOrd LT = "<"
    showOrd EQ = "=="
    showOrd GT = ">"

-- | Convert comment type to text
commentTypeToText :: RT.CommentType -> Text
commentTypeToText = \case
  RT.CTLineComment -> "line"
  RT.CTBlockComment -> "block"
  RT.CTHaddockLine -> "haddockLine"
  RT.CTHaddockBlock -> "haddockBlock"
  RT.CTPragma -> "pragma"

-- | Convert DSL side conditions to a where clause text (for legacy compat)
_conditionsToWhere :: [DSLSideCondition] -> Maybe Text
_conditionsToWhere [] = Nothing
_conditionsToWhere conds = Just $ T.intercalate ", " (map _conditionToText conds)

-- | Convert a single DSL condition to text (legacy)
_conditionToText :: DSLSideCondition -> Text
_conditionToText = \case
  TypeCondition tp -> _typePredToText tp
  ContextCondition cp -> _contextPredToText cp
  ExprCondition ep -> _exprPredToText ep
  AndCondition c1 c2 -> "(" <> _conditionToText c1 <> " && " <> _conditionToText c2 <> ")"
  OrCondition c1 c2 -> "(" <> _conditionToText c1 <> " || " <> _conditionToText c2 <> ")"
  NotCondition c -> "!(" <> _conditionToText c <> ")"

_typePredToText :: TypePredicate -> Text
_typePredToText = \case
  TypeOf varName ty -> varName <> " :: " <> ty
  TypeReturns varName ty -> varName <> " returns " <> ty
  TypeHasClass varName cls -> varName <> " has " <> cls
  TypeIsNumeric varName -> "isNumeric " <> varName
  TypeIsString varName -> "isString " <> varName
  TypeIsList varName -> "isList " <> varName
  TypeIsMaybe varName -> "isMaybe " <> varName
  TypeIsMonad varName monad -> varName <> " in " <> monad
  TypeIsPure varName -> "isPure " <> varName
  TypeMatches varName patText -> varName <> " ~ " <> patText

_contextPredToText :: ContextPredicate -> Text
_contextPredToText = \case
  InContext ctx -> "inContext " <> ctx
  HasImport modName -> "hasImport " <> modName
  HasPragma pragma -> "hasPragma " <> pragma
  InModule moduleName -> "inModule " <> moduleName
  InTestFile -> "inTestFile"
  InMainFile -> "inMainFile"
  NotInContext cp -> "!(" <> _contextPredToText cp <> ")"

_exprPredToText :: ExprPredicate -> Text
_exprPredToText = \case
  IsLiteral varName -> "isLiteral " <> varName
  IsVariable varName -> "isVariable " <> varName
  IsApplication varName -> "isApplication " <> varName
  IsLambda varName -> "isLambda " <> varName
  IsAtomic varName -> "isAtomic " <> varName
  IsSimple varName -> "isSimple " <> varName
  NotEqual v1 v2 -> v1 <> " /= " <> v2
  FreeIn v1 v2 -> v1 <> " freeIn " <> v2
  NotFreeIn v1 v2 -> v1 <> " notFreeIn " <> v2
  NotUsedIn v1 v2 -> v1 <> " notUsedIn " <> v2
  IsEtaReducible f x -> "isEtaReducible " <> f <> " " <> x
  NotBind varName -> "notBind " <> varName
  Complexity varName ord limit ->
    "complexity " <> varName <> " " <> showOrd ord <> " " <> T.pack (show limit)
  NoDerivingStrategy -> "noDerivingStrategy"
  WildcardNotLast -> "wildcardNotLast"
  HasOverlap -> "hasOverlap"
  IsIncomplete -> "isIncomplete"
  HasAmbiguousType -> "hasAmbiguousType"
  UsesDefaultOptions -> "usesDefaultOptions"
  where
    showOrd LT = "<"
    showOrd EQ = "=="
    showOrd GT = ">"

-- | Convert DSL side condition to AST side condition
sideConditionToAST :: SideCondition -> AST.SideCondition
sideConditionToAST = \case
  TypeCondition tp -> typePredToAST tp
  ContextCondition cp -> contextPredToAST cp
  ExprCondition ep -> exprPredToAST ep
  AndCondition c1 c2 -> AST.And [sideConditionToAST c1, sideConditionToAST c2]
  OrCondition c1 c2 -> AST.Or [sideConditionToAST c1, sideConditionToAST c2]
  NotCondition c -> AST.Not (sideConditionToAST c)

typePredToAST :: TypePredicate -> AST.SideCondition
typePredToAST = \case
  TypeOf varName ty -> AST.HasType varName ty
  TypeReturns varName ty -> AST.TypeMatches varName ("* -> " <> ty)
  TypeHasClass varName cls -> AST.HasTypeClass varName cls
  TypeIsNumeric varName -> AST.IsNumeric varName
  TypeIsString varName -> AST.IsString varName
  TypeIsList varName -> AST.IsList varName
  TypeIsMaybe varName -> AST.IsMaybe varName
  TypeIsMonad varName monad -> AST.IsMonad varName monad
  TypeIsPure varName -> AST.IsPure varName
  TypeMatches varName patText -> AST.TypeMatches varName patText

contextPredToAST :: ContextPredicate -> AST.SideCondition
contextPredToAST = \case
  -- Context predicates are properly mapped to unified SideCondition
  -- The Engine/SideConditions module handles evaluation with file context
  InContext ctx -> AST.InContext ctx
  HasImport modName -> AST.HasImport modName
  HasPragma p -> AST.HasPragma p
  InModule m -> AST.InModule m
  InTestFile -> AST.InTestFile
  InMainFile -> AST.NotInTestFile
  NotInContext cp -> AST.Not (contextPredToAST cp)

exprPredToAST :: ExprPredicate -> AST.SideCondition
exprPredToAST = \case
  IsLiteral varN -> AST.IsLiteral varN
  IsVariable varN -> AST.IsVariable varN
  IsApplication varN -> AST.IsApplication varN
  IsLambda varN -> AST.IsLambda varN
  IsAtomic varN -> AST.IsAtomic varN
  IsSimple varN -> AST.IsAtomic varN    -- Simple ≈ Atomic for now
  NotEqual varA varB -> AST.NotEqual varA varB
  FreeIn varA varB -> AST.FreeIn varA varB
  NotFreeIn varA varB -> AST.NotFreeIn varA varB
  NotUsedIn varA varB -> AST.NotFreeIn varA varB  -- Same semantics
  IsEtaReducible funcN argN -> AST.IsEtaReducible funcN argN
  NotBind varN -> AST.NotBind varN
  Complexity varN ord lim -> AST.ComplexityCond varN ord lim
  -- Deriving and pattern analysis predicates
  NoDerivingStrategy -> AST.NoDerivingStrategy
  WildcardNotLast -> AST.WildcardNotLast
  HasOverlap -> AST.HasPatternOverlap
  IsIncomplete -> AST.IsPatternIncomplete
  HasAmbiguousType -> AST.HasAmbiguousType
  UsesDefaultOptions -> AST.UsesDefaultOptions

-- | Convert DSLPattern to text representation
_patternToText :: DSLPattern -> Text
_patternToText (PatternText t) = t
_patternToText (PatternExpr e) = exprToText e

-- | Convert PatternExpr to text representation
exprToText :: PatternExpr -> Text
exprToText (PVar v) = v
exprToText (PLit l) = l
exprToText (PApp f x) = exprToText f <> " " <> exprToText x
exprToText (POp o l r) = exprToText l <> " " <> o <> " " <> exprToText r
exprToText PWildcard = "_"
exprToText (PList xs) = "[" <> T.intercalate ", " (map exprToText xs) <> "]"
exprToText (PTuple xs) = "(" <> T.intercalate ", " (map exprToText xs) <> ")"
exprToText (PGuard p g) = exprToText p <> " | " <> exprToText g

-- | Convert Severity to RuleSeverity
severityToRS :: Severity -> RuleSeverity
severityToRS Error = RSError
severityToRS Warning = RSWarning
severityToRS Suggestion = RSSuggestion
severityToRS Info = RSInfo

-- | Convert DSLImportSpec to FixImport (legacy Argus.Types type)
importSpecToFixImport :: DSLImportSpec -> FixImport
importSpecToFixImport DSLImportSpec{..} = FixImport
  { fimpModule = isModule
  , fimpSymbols = map textToImportSymbol isSymbols
  , fimpQualified = isQualified
  , fimpHiding = False
  , fimpPackage = Nothing
  }

-- | Convert a text symbol name to ImportSymbol
-- Infers the symbol type from naming conventions
textToImportSymbol :: Text -> ImportSymbol
textToImportSymbol name = ImportSymbol
  { isymName = name
  , isymType = inferSymbolType name
  , isymChildren = []
  }

-- | Infer the import symbol type from naming conventions
inferSymbolType :: Text -> ImportSymbolType
inferSymbolType name
  | T.null name = ISTFunction
  | isOperatorName name = ISTOperator
  | isUpperCase (T.head name) = ISTType  -- Could be Type or Constructor
  | otherwise = ISTFunction
  where
    isOperatorName n = T.any (`elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)) n
    isUpperCase c = c >= 'A' && c <= 'Z'

-- | Get all FixImports from a unified Rule
ruleFixImports :: Rule -> [FixImport]
ruleFixImports = map RT.importSpecToFixImport . ruleAddImports

-- | Compile a single rule
compileRule :: Rule -> PatternRule
compileRule = ruleToPatternRule

-- | Compile a list of rules
compileRules :: [Rule] -> [PatternRule]
compileRules = map ruleToPatternRule . filter ruleEnabled

--------------------------------------------------------------------------------
-- Built-in Rule Sets
--------------------------------------------------------------------------------

-- | Default rule set with common patterns
defaultRuleSet :: [Rule]
defaultRuleSet =
  [ rule "avoid-head" $
      match ("head" ==> "headMay")
      & severity Warning
      & message "Use headMay instead of partial head function"
      & category Safety
      & safetyLevel Safe

  , rule "avoid-tail" $
      match ("tail" ==> "tailMay")
      & severity Warning
      & message "Use tailMay instead of partial tail function"
      & category Safety

  , rule "avoid-fromJust" $
      match (pat "fromJust")
      & severity Warning
      & message "fromJust is partial - use pattern matching or fromMaybe"
      & category Safety

  , rule "prefer-pure" $
      match ("return" ==> "pure")
      & severity Info
      & message "Consider using pure instead of return"
      & category Modernization

  , rule "redundant-do" $
      match ("do return" ==> "pure")
      & severity Suggestion
      & message "Redundant do with return"
      & category Style
  ]

-- | Strict rule set with additional safety checks
strictRuleSet :: [Rule]
strictRuleSet = defaultRuleSet ++
  [ rule "avoid-error" $
      match (pat "error")
      & severity Error
      & message "Avoid using error - use proper error handling"
      & category Safety
      & safetyLevel ManualReview

  , rule "avoid-undefined" $
      match (pat "undefined")
      & severity Error
      & message "undefined must not be used in production code"
      & category Safety
      & safetyLevel ManualReview

  , rule "avoid-unsafePerformIO" $
      match (pat "unsafePerformIO")
      & severity Error
      & message "unsafePerformIO breaks referential transparency"
      & category Security
      & safetyLevel Unsafe
  ]

-- | Performance-focused rule set
performanceRuleSet :: [Rule]
performanceRuleSet =
  [ rule "prefer-foldl'" $
      match ("foldl" ==> "foldl'"
             `fromModule` "Prelude"
             `addImport` ("Data.Foldable", ["foldl'"]))
      & severity Suggestion
      & message "Use foldl' (strict) instead of foldl to avoid space leaks"
      & category Performance

  , rule "avoid-nub" $
      match ("nub" ==> "ordNub"
             `fromModule` "Data.List"
             `toModule` "Data.Containers.ListUtils"
             `where_` hasClass "$X" "Ord")
      & severity Suggestion
      & message "nub is O(n²) - use ordNub for O(n log n)"
      & category Performance

  , rule "avoid-length-eq-0" $
      match ("length $XS == 0" ==> "null $XS")
      & severity Suggestion
      & message "Use null for O(1) emptiness check instead of length"
      & category Performance

  , rule "concat-map" $
      match ("concat $ map $F $XS" ==> "concatMap $F $XS")
      & severity Suggestion
      & message "Use concatMap instead of concat . map"
      & category Performance

  , rule "mconcat-map" $
      match ("mconcat $ map $F $XS" ==> "foldMap $F $XS"
             `addImport` ("Data.Foldable", ["foldMap"]))
      & severity Suggestion
      & message "Use foldMap instead of mconcat . map"
      & category Performance
  ]

-- | Security-focused rule set
securityRuleSet :: [Rule]
securityRuleSet =
  [ rule "avoid-unsafeCoerce" $
      match (pat "unsafeCoerce")
      & severity Error
      & message "unsafeCoerce can cause memory corruption"
      & category Security
      & safetyLevel Unsafe

  , rule "avoid-unsafeInterleaveIO" $
      match (pat "unsafeInterleaveIO")
      & severity Warning
      & message "unsafeInterleaveIO can cause unpredictable behavior"
      & category Security

  , rule "avoid-inlinePerformIO" $
      match (pat "inlinePerformIO")
      & severity Error
      & message "inlinePerformIO is extremely unsafe"
      & category Security
      & safetyLevel Unsafe
  ]

-- | Type-aware rules demonstrating advanced DSL features
typeAwareRuleSet :: [Rule]
typeAwareRuleSet =
  [ rule "prefer-traverse" $
      match ("mapM $F $XS" ==> "traverse $F $XS"
             `where_` (typeOf "$F" `returns` "IO _")
             `unless` inContext "parallel")
      & severity Suggestion
      & message "Use traverse instead of mapM for Applicative"
      & category Modernization
      & note "traverse is more general and works with any Applicative"

  , rule "use-foldMap" $
      match ("mconcat (map $F $XS)" ==> "foldMap $F $XS"
             `where_` hasClass "$A" "Monoid")
      & severity Suggestion
      & message "Use foldMap for better performance"
      & category Performance

  , rule "use-void" $
      match ("fmap (const ()) $X" ==> "void $X")
      & severity Suggestion
      & message "Use void instead of fmap (const ())"
      & category Modernization

  , rule "use-when" $
      match ("if $B then $ACTION else pure ()" ==> "when $B $ACTION"
             `where_` isPure "$B")
      & severity Suggestion
      & message "Use when for conditional actions"
      & category Modernization

  , rule "avoid-head-sort" $
      match ("head (sort $XS)" ==> "minimum $XS"
             `unless` isAtomic "$XS")
      & severity Suggestion
      & message "Use minimum instead of head . sort for O(n)"
      & category Performance

  , rule "avoid-last-sort" $
      match ("last (sort $XS)" ==> "maximum $XS")
      & severity Suggestion
      & message "Use maximum instead of last . sort for O(n)"
      & category Performance

  , rule "sequence-map" $
      match ("sequence (map $F $XS)" ==> "traverse $F $XS")
      & severity Suggestion
      & message "Use traverse instead of sequence . map"
      & category Performance

  , rule "join-fmap" $
      match ("join (fmap $F $X)" ==> "$X >>= $F")
      & severity Suggestion
      & message "Use >>= instead of join . fmap"
      & category Modernization

  , rule "fmap-pure" $
      match ("fmap $F (pure $X)" ==> "pure ($F $X)")
      & severity Suggestion
      & message "Apply f directly to x"
      & category Performance

  , rule "concat-replicate" $
      match ("concat (replicate $N $XS)" ==> "stimes $N $XS"
             `where_` isNumeric "$N")
      & severity Info
      & message "Consider using stimes from Semigroup for efficiency"
      & category Performance
  ]
