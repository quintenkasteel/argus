{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin
-- Description : Built-in lint rules for Haskell code analysis
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- This module re-exports all built-in lint rules organized by category.
-- These rules are designed to catch common issues, encourage best practices,
-- and improve code quality.
--
-- == Rule Categories
--
-- * "Argus.Rules.Builtin.Safety" - Avoid partial functions and unsafe operations
-- * "Argus.Rules.Builtin.Performance" - Optimize algorithms and prevent space leaks
-- * "Argus.Rules.Builtin.Security" - Detect security vulnerabilities
-- * "Argus.Rules.Builtin.Style" - Encourage modern idioms and clean code
-- * "Argus.Rules.Builtin.Correctness" - Find logic errors and law violations
-- * "Argus.Rules.Builtin.Brackets" - Bracket/fixity and readability rules
-- * "Argus.Rules.Builtin.TypeAnnotations" - Type signature and annotation rules
-- * "Argus.Rules.Builtin.CodeSmells" - Code smell and architectural quality detection
-- * "Argus.Rules.Builtin.Lambda" - Lambda expression and pattern matching refactoring
-- * "Argus.Rules.Builtin.Monadic" - Monad/Applicative modernization rules
-- * "Argus.Rules.Builtin.ListRecursion" - List recursion pattern detection
-- * "Argus.Rules.Builtin.TypeclassLaws" - Typeclass law violation detection
-- * "Argus.Rules.Builtin.Foldable" - Fold and traversal optimization rules
-- * "Argus.Rules.Builtin.Numeric" - Numeric computation and precision rules
-- * "Argus.Rules.Builtin.Strings" - String and Text manipulation rules
-- * "Argus.Rules.Builtin.Records" - Record syntax and data type rules
-- * "Argus.Rules.Builtin.Prelude" - Prelude function improvement rules
--
-- == Quick Start
--
-- Import all rules:
--
-- @
-- import Argus.Rules.Builtin (allBuiltinRules)
--
-- -- Or specific categories
-- import Argus.Rules.Builtin (safetyRules, performanceRules)
-- @
--
-- == Rule Severity
--
-- Rules use these severity levels:
--
-- * __Error__: Likely bugs or serious issues
-- * __Warning__: Potential problems worth investigating
-- * __Suggestion__: Improvements for better code
-- * __Info__: Informational hints
--
-- == Custom Rule Sets
--
-- You can create custom rule sets by combining built-in rules:
--
-- @
-- myRules :: [Rule]
-- myRules = safetyRules ++ performanceRules
--         ++ filter isWarningOrHigher securityRules
--
-- isWarningOrHigher :: Rule -> Bool
-- isWarningOrHigher r = ruleSeverity r >= Warning
-- @
--
-- == References
--
-- * <https://github.com/ndmitchell/hlint HLint>
-- * <https://rust-lang.github.io/rust-clippy/master/ Clippy>
-- * <https://wiki.haskell.org/Performance Haskell Performance Wiki>

module Argus.Rules.Builtin
  ( -- * All Rules
    allBuiltinRules
  , builtinRuleCount

    -- * Rule Categories
    -- ** Safety Rules
  , module Argus.Rules.Builtin.Safety

    -- ** Performance Rules
  , module Argus.Rules.Builtin.Performance

    -- ** Security Rules
  , module Argus.Rules.Builtin.Security

    -- ** Style Rules
  , module Argus.Rules.Builtin.Style

    -- ** Correctness Rules
  , module Argus.Rules.Builtin.Correctness

    -- ** Bracket/Fixity Rules
  , module Argus.Rules.Builtin.Brackets

    -- ** Type Annotation Rules
  , module Argus.Rules.Builtin.TypeAnnotations

    -- ** Code Smell Rules
  , module Argus.Rules.Builtin.CodeSmells

    -- ** Lambda/Pattern Rules
  , module Argus.Rules.Builtin.Lambda

    -- ** Monadic Rules
  , module Argus.Rules.Builtin.Monadic

    -- ** List Recursion Rules
  , module Argus.Rules.Builtin.ListRecursion

    -- ** Typeclass Law Rules
  , module Argus.Rules.Builtin.TypeclassLaws

    -- ** Foldable/Traversal Rules
  , module Argus.Rules.Builtin.Foldable

    -- ** Numeric Rules
  , module Argus.Rules.Builtin.Numeric

    -- ** String/Text Rules
  , module Argus.Rules.Builtin.Strings

    -- ** Record/Data Rules
  , module Argus.Rules.Builtin.Records

    -- ** Prelude Rules
  , module Argus.Rules.Builtin.Prelude

    -- ** IO Rules
  , module Argus.Rules.Builtin.IO

    -- ** Container Rules
  , module Argus.Rules.Builtin.Containers

    -- ** Transformer Rules
  , module Argus.Rules.Builtin.Transformers

    -- ** Testing Rules
  , module Argus.Rules.Builtin.Testing

    -- ** FFI Rules
  , module Argus.Rules.Builtin.FFI

    -- ** Type Family Rules
  , module Argus.Rules.Builtin.TypeFamilies

    -- ** GADT Rules
  , module Argus.Rules.Builtin.GADTs

    -- ** Deriving Rules
  , module Argus.Rules.Builtin.Deriving

    -- ** Lens/Optics Rules
  , module Argus.Rules.Builtin.Lenses

    -- ** Async/Concurrency Rules
  , module Argus.Rules.Builtin.Async

    -- ** List Rules
  , module Argus.Rules.Builtin.List

    -- ** Maybe Rules
  , module Argus.Rules.Builtin.Maybe

    -- ** Either Rules
  , module Argus.Rules.Builtin.Either

    -- ** Boolean Rules
  , module Argus.Rules.Builtin.Boolean

    -- ** Comparison Rules
  , module Argus.Rules.Builtin.Comparison

    -- ** Tuple Rules
  , module Argus.Rules.Builtin.Tuple

    -- ** Composition Rules
  , module Argus.Rules.Builtin.Composition

    -- ** Arrow Rules
  , module Argus.Rules.Builtin.Arrow

    -- ** Monoid/Semigroup Rules
  , module Argus.Rules.Builtin.Monoid

    -- ** Pattern Matching Rules
  , module Argus.Rules.Builtin.Pattern

    -- ** Import/Extension Rules
  , module Argus.Rules.Builtin.Imports

    -- ** OWASP Security Rules
  , module Argus.Rules.Builtin.OWASP

    -- * Rule Set Combinators
  , defaultRules
  , strictRules
  , minimalRules
  , rulesByCategory
  , rulesBySeverity

    -- * Rule Metadata
  , categoryDescription
  , severityDescription
  ) where

import Data.Text (Text)

import Argus.Rules.DSL (Rule(..), pattern ManualReview)
import Argus.Rules.Types (Category(..))
import Argus.Types (Severity(..))

import Argus.Rules.Builtin.Safety
import Argus.Rules.Builtin.Performance
import Argus.Rules.Builtin.Security
import Argus.Rules.Builtin.Style
import Argus.Rules.Builtin.Correctness
import Argus.Rules.Builtin.Brackets
import Argus.Rules.Builtin.TypeAnnotations
import Argus.Rules.Builtin.CodeSmells
import Argus.Rules.Builtin.Lambda
import Argus.Rules.Builtin.Monadic
import Argus.Rules.Builtin.ListRecursion
import Argus.Rules.Builtin.TypeclassLaws
import Argus.Rules.Builtin.Foldable
import Argus.Rules.Builtin.Numeric
import Argus.Rules.Builtin.Strings
import Argus.Rules.Builtin.Records
import Argus.Rules.Builtin.Prelude
import Argus.Rules.Builtin.IO
import Argus.Rules.Builtin.Containers
import Argus.Rules.Builtin.Transformers
import Argus.Rules.Builtin.Testing
import Argus.Rules.Builtin.FFI
import Argus.Rules.Builtin.TypeFamilies
import Argus.Rules.Builtin.GADTs
import Argus.Rules.Builtin.Deriving
import Argus.Rules.Builtin.Lenses
import Argus.Rules.Builtin.Async
import Argus.Rules.Builtin.List
import Argus.Rules.Builtin.Maybe
import Argus.Rules.Builtin.Either
import Argus.Rules.Builtin.Boolean
import Argus.Rules.Builtin.Comparison
import Argus.Rules.Builtin.Tuple
import Argus.Rules.Builtin.Composition
import Argus.Rules.Builtin.Arrow
import Argus.Rules.Builtin.Monoid
import Argus.Rules.Builtin.Pattern
import Argus.Rules.Builtin.Imports
import Argus.Rules.Builtin.OWASP

--------------------------------------------------------------------------------
-- All Rules
--------------------------------------------------------------------------------

-- | All built-in rules from all categories.
--
-- This comprehensive set includes every rule Argus provides.
-- Consider using more targeted sets for specific use cases.
--
-- Currently includes __@builtinRuleCount@__ rules.
allBuiltinRules :: [Rule]
allBuiltinRules = mconcat
  [ safetyRules
  , performanceRules
  , securityRules
  , styleRules
  , correctnessRules
  , bracketRules
  , typeAnnotationRules
  , codeSmellRules
  , lambdaRules
  , monadicRules
  , listRecursionRules
  , typeclassLawRules
  , foldableRules
  , numericRules
  , stringRules
  , recordRules
  , preludeRules
  , ioRules
  , containerRules
  , mtlTransformerRules
  , testingRules
  , ffiRules
  , typeFamilyRules
  , gadtRules
  , derivingRules
  , opticsRules
  , asyncRules
  , listRules
  , maybeRules
  , eitherRules
  , boolSimplifyRules
  , ordComparisonRules
  , tupleRules
  , compositionRules
  , arrowRules
  , monoidRules
  , patternMatchRules
  , importRules
  , allOWASPRules
  ]

-- | Total count of all built-in rules.
builtinRuleCount :: Int
builtinRuleCount = length allBuiltinRules

--------------------------------------------------------------------------------
-- Rule Set Presets
--------------------------------------------------------------------------------

-- | Default rule set for most projects.
--
-- Includes:
--
-- * All safety rules (avoid partial functions)
-- * All correctness rules (find bugs)
-- * High-severity performance rules
-- * Non-pedantic style rules
--
-- Excludes:
--
-- * Info-level rules
-- * Rules with ManualReview safety level
-- * Highly opinionated style rules
defaultRules :: [Rule]
defaultRules = filter isDefault allBuiltinRules
  where
    isDefault r =
      ruleSeverity r >= Suggestion &&
      ruleSafety r /= ManualReview

-- | Strict rule set for high-quality codebases.
--
-- Enables all rules at Warning or above.
-- Use for production code, libraries, and reviewed contributions.
strictRules :: [Rule]
strictRules = filter isStrict allBuiltinRules
  where
    isStrict r = ruleSeverity r >= Warning

-- | Minimal rule set for quick checks.
--
-- Only Error-level rules that indicate definite bugs.
-- Fast and low false-positive rate.
minimalRules :: [Rule]
minimalRules = filter isMinimal allBuiltinRules
  where
    isMinimal r = ruleSeverity r == Error

--------------------------------------------------------------------------------
-- Rule Filtering
--------------------------------------------------------------------------------

-- | Get rules by category.
--
-- @
-- performanceOnlyRules = rulesByCategory Performance allBuiltinRules
-- @
rulesByCategory :: Category -> [Rule] -> [Rule]
rulesByCategory cat = filter (\r -> ruleCategory r == cat)

-- | Get rules at or above a severity level.
--
-- @
-- warningsAndErrors = rulesBySeverity Warning allBuiltinRules
-- @
rulesBySeverity :: Severity -> [Rule] -> [Rule]
rulesBySeverity minSev = filter (\r -> ruleSeverity r >= minSev)

--------------------------------------------------------------------------------
-- Metadata
--------------------------------------------------------------------------------

-- | Human-readable description of each rule category.
categoryDescription :: Category -> Text
categoryDescription cat = case cat of
  Safety -> "Rules for avoiding partial functions and unsafe operations"
  Performance -> "Rules for optimizing performance and preventing space leaks"
  SpaceLeaks -> "Rules for detecting space leak patterns and thunk buildup"
  Security -> "Rules for detecting security vulnerabilities and unsafe patterns"
  Style -> "Rules for code style, modernization, and idioms"
  Correctness -> "Rules for detecting bugs, logic errors, and law violations"
  Modernization -> "Rules for updating to modern Haskell patterns"
  Naming -> "Rules for naming conventions"
  Imports -> "Rules for import organization"
  Extensions -> "Rules for language extension usage and best practices"
  Complexity -> "Rules for code complexity and cyclomatic depth"
  Concurrency -> "Rules for concurrency patterns, STM, and async"
  ErrorHandling -> "Rules for error handling best practices"
  Documentation -> "Rules for documentation completeness and quality"
  Redundant -> "Rules for detecting redundant or unnecessary code"
  Custom name -> "Custom category: " <> name

-- | Human-readable description of each severity level.
severityDescription :: Severity -> Text
severityDescription sev = case sev of
  Error -> "Definite bugs or critical issues that must be fixed"
  Warning -> "Likely problems that should be investigated"
  Suggestion -> "Improvements for better, cleaner code"
  Info -> "Informational hints and minor suggestions"
