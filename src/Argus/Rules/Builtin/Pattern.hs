{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Pattern
-- Description : Pattern matching rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for pattern matching simplifications and improvements.

module Argus.Rules.Builtin.Pattern
  ( -- * Rule Sets
    patternMatchRules
  , patternSimplifyRules
  , patternOptimizeRules

    -- * Pattern Simplifications
  , caseWildcard
  , caseIdentity
  , caseBoolId
  , caseEitherIdId
  , casePairId
  , caseEmptyList
  , caseNonEmptyList
  , caseNothingBool
  , caseJustBool

    -- * Pattern Optimizations
  , patternGuardSimple
  , patternOverlap
  , patternNonExhaustive
  , patternRedundant
  , patternMultiEval
  , patternGADT
  , patternView
  , patternSynonym
  , patternAsUnused
  , patternLazyUnused
  , nestedCase

    -- * Rule Count
  , patternRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All pattern-related rules.
patternMatchRules :: [Rule]
patternMatchRules = mconcat
  [ patternSimplifyRules
  , patternOptimizeRules
  ]

-- | Total count of pattern rules.
patternRuleCount :: Int
patternRuleCount = length patternMatchRules

--------------------------------------------------------------------------------
-- Pattern Simplification Rules
--------------------------------------------------------------------------------

-- | Rules for pattern simplifications.
patternSimplifyRules :: [Rule]
patternSimplifyRules =
  [ caseWildcard
  , caseIdentity
  , caseBoolId
  , caseEitherIdId
  , casePairId
  , caseEmptyList
  , caseNonEmptyList
  , caseNothingBool
  , caseJustBool
  ]

-- | case x of { _ -> y } ==> y
caseWildcard :: Rule
caseWildcard =
  rule "case-wildcard" $
    matchText "case\\s+[^o]+of\\s*\\{?\\s*_\\s*->\\s*[^}]+\\}?\\s*$"
    & category Style
    & severity Suggestion
    & message "case with only wildcard can be simplified"

-- | case x of { x -> f x } ==> f x
caseIdentity :: Rule
caseIdentity =
  rule "case-identity" $
    matchText "case\\s+([a-z_]+)\\s+of\\s*\\{?\\s*\\1\\s*->"
    & category Style
    & severity Suggestion
    & message "case matching same variable is redundant"

-- | case x of { True -> True; False -> False } ==> x
caseBoolId :: Rule
caseBoolId =
  rule "case-bool-id" $
    matchText "case\\s+[^o]+of\\s*\\{?\\s*True\\s*->\\s*True\\s*;\\s*False\\s*->\\s*False"
    & category Style
    & severity Suggestion
    & message "case True -> True; False -> False is identity"

-- | case x of { Left l -> Left l; Right r -> Right r } ==> x
caseEitherIdId :: Rule
caseEitherIdId =
  rule "case-either-id" $
    matchText "case\\s+[^o]+of\\s*\\{?\\s*Left\\s+([a-z_]+)\\s*->\\s*Left\\s+\\1"
    & category Style
    & severity Suggestion
    & message "case Left l -> Left l; Right r -> Right r is identity"

-- | case x of { (a, b) -> (a, b) } ==> x
casePairId :: Rule
casePairId =
  rule "case-pair-id" $
    matchText "case\\s+[^o]+of\\s*\\{?\\s*\\(\\s*([a-z_]+)\\s*,\\s*([a-z_]+)\\s*\\)\\s*->\\s*\\(\\s*\\1\\s*,\\s*\\2\\s*\\)"
    & category Style
    & severity Suggestion
    & message "case (a, b) -> (a, b) is identity"

-- | case x of { [] -> True; _ -> False } ==> null x
caseEmptyList :: Rule
caseEmptyList =
  rule "case-empty-list" $
    matchText "case\\s+[^o]+of\\s*\\{?\\s*\\[\\]\\s*->\\s*True\\s*;\\s*_\\s*->\\s*False"
    & category Style
    & severity Suggestion
    & message "case [] -> True; _ -> False is null x"

-- | case x of { [] -> False; _ -> True } ==> not (null x)
caseNonEmptyList :: Rule
caseNonEmptyList =
  rule "case-nonempty-list" $
    matchText "case\\s+[^o]+of\\s*\\{?\\s*\\[\\]\\s*->\\s*False\\s*;\\s*_\\s*->\\s*True"
    & category Style
    & severity Suggestion
    & message "case [] -> False; _ -> True is not (null x)"

-- | case x of { Nothing -> True; Just _ -> False } ==> isNothing x
caseNothingBool :: Rule
caseNothingBool =
  rule "case-nothing-bool" $
    matchText "case\\s+[^o]+of\\s*\\{?\\s*Nothing\\s*->\\s*True\\s*;\\s*Just\\s*_\\s*->\\s*False"
    & category Style
    & severity Suggestion
    & message "case Nothing -> True; Just _ -> False is isNothing x"

-- | case x of { Nothing -> False; Just _ -> True } ==> isJust x
caseJustBool :: Rule
caseJustBool =
  rule "case-just-bool" $
    matchText "case\\s+[^o]+of\\s*\\{?\\s*Nothing\\s*->\\s*False\\s*;\\s*Just\\s*_\\s*->\\s*True"
    & category Style
    & severity Suggestion
    & message "case Nothing -> False; Just _ -> True is isJust x"

--------------------------------------------------------------------------------
-- Pattern Optimization Rules
--------------------------------------------------------------------------------

-- | Rules for pattern optimizations.
patternOptimizeRules :: [Rule]
patternOptimizeRules =
  [ patternGuardSimple
  , patternOverlap
  , patternNonExhaustive
  , patternRedundant
  , patternMultiEval
  , patternGADT
  , patternView
  , patternSynonym
  , patternAsUnused
  , patternLazyUnused
  , nestedCase
  ]

-- | case x of { _ | p x -> a; _ -> b } ==> if p x then a else b
patternGuardSimple :: Rule
patternGuardSimple =
  rule "pattern-guard-simple" $
    matchText "case\\s+[^o]+of\\s*\\{?\\s*_\\s*\\|"
    & category Style
    & severity Info
    & message "case with guard on wildcard could be if-then-else"
    & safetyLevel ManualReview

-- | Overlapping patterns warning.
patternOverlap :: Rule
patternOverlap =
  rule "pattern-overlap" $
    matchText "case\\s+[^o]+of.*_\\s*->.*[A-Z][a-zA-Z]*\\s*->"
    & category Correctness
    & severity Warning
    & message "Potential overlapping patterns - unreachable code"
    & safetyLevel ManualReview

-- | Non-exhaustive patterns warning.
patternNonExhaustive :: Rule
patternNonExhaustive =
  rule "pattern-non-exhaustive" $
    matchText "case\\s+[^o]+of\\s*\\{?\\s*[A-Z][a-zA-Z]*\\s+->[^}]+$"
    & category Correctness
    & severity Warning
    & message "Potentially non-exhaustive pattern match"
    & safetyLevel ManualReview

-- | Redundant pattern warning.
patternRedundant :: Rule
patternRedundant =
  rule "pattern-redundant" $
    matchText "case\\s+[^o]+of.*->.*_\\s*->[^}]*_\\s*->"
    & category Correctness
    & severity Warning
    & message "Redundant pattern after wildcard"
    & safetyLevel ManualReview

-- | case (f x, g x) multiple evaluation.
patternMultiEval :: Rule
patternMultiEval =
  rule "pattern-multi-eval" $
    matchText "case\\s*\\([^,]+,[^)]+\\)\\s*of"
    & category Performance
    & severity Info
    & message "Tuple in case - components evaluated separately"
    & safetyLevel ManualReview

-- | Pattern match on GADT without type annotation.
patternGADT :: Rule
patternGADT =
  rule "pattern-gadt" $
    matchText "case\\s+[^o]+of.*::\\s*[A-Z]"
    & category Style
    & severity Info
    & message "GADT pattern match - type annotation may be needed"
    & safetyLevel ManualReview

-- | View pattern suggestion.
patternView :: Rule
patternView =
  rule "pattern-view" $
    matchText "case\\s+[a-z_]+\\s+[a-z_]+\\s+of"
    & category Style
    & severity Info
    & message "Consider ViewPatterns for function application in patterns"
    & safetyLevel ManualReview

-- | Pattern synonym suggestion.
patternSynonym :: Rule
patternSynonym =
  rule "pattern-synonym" $
    matchText "pattern\\s+[A-Z]"
    & category Style
    & severity Info
    & message "Using pattern synonym"
    & safetyLevel ManualReview

-- | As-pattern unused warning.
patternAsUnused :: Rule
patternAsUnused =
  rule "pattern-as-unused" $
    matchText "@\\s*\\("
    & category Style
    & severity Info
    & message "As-pattern - ensure the bound name is used"
    & safetyLevel ManualReview

-- | Lazy pattern potentially unnecessary.
patternLazyUnused :: Rule
patternLazyUnused =
  rule "pattern-lazy-unused" $
    matchText "~\\s*\\("
    & category Performance
    & severity Info
    & message "Lazy pattern - ensure laziness is needed"
    & safetyLevel ManualReview

-- | Nested case expressions.
nestedCase :: Rule
nestedCase =
  rule "nested-case" $
    matchText "case\\s+[^o]+of[^c]+case\\s+[^o]+of"
    & category Style
    & severity Info
    & message "Nested case expressions - consider pattern guards or refactoring"
    & safetyLevel ManualReview
