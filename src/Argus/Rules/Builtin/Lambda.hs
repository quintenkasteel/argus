{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Lambda
-- Description : Lambda expression and pattern matching refactoring rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Comprehensive rules for lambda expressions, eta reduction, tuple sections,
-- pattern matching simplification, and point-free style. These rules help
-- write more idiomatic and concise Haskell.
--
-- == Rule Categories
--
-- * __Eta Reduction__: Simplify lambda expressions
-- * __Tuple Sections__: Use TupleSections extension
-- * __Lambda Case__: Use LambdaCase extension
-- * __Pattern Refactoring__: Simplify pattern matching
-- * __Point-Free__: Function composition suggestions
--
-- == References
--
-- * <https://wiki.haskell.org/Eta_conversion Eta Conversion>
-- * <https://wiki.haskell.org/Pointfree Point-Free Style>

module Argus.Rules.Builtin.Lambda
  ( -- * Rule Sets
    lambdaRules
  , etaRules
  , tupleSectionRules
  , lambdaCaseRules
  , patternRules
  , pointFreeRules

    -- * Eta Reduction
  , etaReduceSimple
  , etaReduceOperator
  , etaReduceFlip
  , etaReduceCompose
  , etaReduceMultiArg
  , etaExpandNeeded

    -- * Tuple Sections
  , tupleSectionLeft
  , tupleSectionRight
  , tupleSectionBoth
  , nestedTupleSection

    -- * Lambda Case
  , lambdaToCaseSimple
  , lambdaToCaseMulti
  , lambdaToLambdaCase
  , caseToLambdaCase

    -- * Pattern Refactoring
  , guardToPattern
  , caseOfCase
  , singleBranchCase
  , asPattern
  , viewPattern
  , patternGuard
  , bangPattern
  , lazyPattern

    -- * Point-Free Style
  , composeSimple
  , composeChain
  , flipCompose
  , onCompose
  , applyCompose

    -- * Rule Count
  , lambdaRuleCount
  ) where

import Argus.Rules.DSL hiding (Complexity)
import Argus.Rules.Types (Category(Complexity))

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All lambda and pattern refactoring rules.
lambdaRules :: [Rule]
lambdaRules = mconcat
  [ etaRules
  , tupleSectionRules
  , lambdaCaseRules
  , patternRules
  , pointFreeRules
  ]

-- | Total count of lambda rules.
lambdaRuleCount :: Int
lambdaRuleCount = length lambdaRules

--------------------------------------------------------------------------------
-- Eta Reduction Rules
--------------------------------------------------------------------------------

-- | Rules for eta-reducing lambda expressions.
etaRules :: [Rule]
etaRules =
  [ etaReduceSimple
  , etaReduceOperator
  , etaReduceFlip
  , etaReduceCompose
  , etaReduceMultiArg
  , etaExpandNeeded
  ]

-- | Simple eta reduction.
--
-- @
-- \\x -> f x  ==>  f
-- @
etaReduceSimple :: Rule
etaReduceSimple =
  rule "eta-reduce-simple" $
    match (("\\x -> _f x" ==> "_f") `where_` isVariable "_f")
    & category Style
    & severity Suggestion
    & message "Eta reduce: \\x -> f x  ==>  f"

-- | Eta reduction with operator.
--
-- @
-- \\x -> x + 1  ==>  (+ 1)
-- \\x -> 1 + x  ==>  (1 +)
-- @
etaReduceOperator :: Rule
etaReduceOperator =
  rule "eta-reduce-operator" $
    match ("\\x -> x + _n" ==> "(+ _n)")
    & category Style
    & severity Suggestion
    & message "Use operator section: \\x -> x + n  ==>  (+ n)"

-- | Eta reduction with flip.
--
-- @
-- \\x -> f y x  ==>  flip f y
-- @
etaReduceFlip :: Rule
etaReduceFlip =
  rule "eta-reduce-flip" $
    match (("\\x -> _f _y x" ==> "flip _f _y") `where_` isVariable "_y")
    & category Style
    & severity Suggestion
    & message "Use flip: \\x -> f y x  ==>  flip f y"

-- | Eta reduction with composition.
--
-- @
-- \\x -> f (g x)  ==>  f . g
-- @
etaReduceCompose :: Rule
etaReduceCompose =
  rule "eta-reduce-compose" $
    match ("\\x -> _f (_g x)" ==> "_f . _g")
    & category Style
    & severity Suggestion
    & message "Use composition: \\x -> f (g x)  ==>  f . g"

-- | Multi-argument eta reduction.
--
-- @
-- \\x y -> f x y  ==>  f
-- @
etaReduceMultiArg :: Rule
etaReduceMultiArg =
  rule "eta-reduce-multi-arg" $
    match (("\\x y -> _f x y" ==> "_f") `where_` isVariable "_f")
    & category Style
    & severity Suggestion
    & message "Eta reduce: \\x y -> f x y  ==>  f"

-- | Warning when eta expansion might be needed.
--
-- @
-- map f  -- In some contexts, may need \\x -> map f x
-- @
etaExpandNeeded :: Rule
etaExpandNeeded =
  rule "eta-expand-needed" $
    match ("map _f" ==> "map _f")
    & category Style
    & severity Info
    & message "Partially applied map - ensure this is intentional"
    & note "Partial application is fine, but verify arity matches"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Tuple Section Rules
--------------------------------------------------------------------------------

-- | Rules for using TupleSections extension.
tupleSectionRules :: [Rule]
tupleSectionRules =
  [ tupleSectionLeft
  , tupleSectionRight
  , tupleSectionBoth
  , nestedTupleSection
  ]

-- | Left tuple section.
--
-- @
-- \\x -> (x, y)  ==>  (, y)
-- @
tupleSectionLeft :: Rule
tupleSectionLeft =
  rule "tuple-section-left" $
    match ("\\x -> (x, _y)" ==> "(, _y)")
    & category Style
    & severity Suggestion
    & message "Use tuple section: \\x -> (x, y)  ==>  (, y)"
    & note "Requires TupleSections extension"

-- | Right tuple section.
--
-- @
-- \\x -> (y, x)  ==>  (y,)
-- @
tupleSectionRight :: Rule
tupleSectionRight =
  rule "tuple-section-right" $
    match ("\\x -> (_y, x)" ==> "(_y,)")
    & category Style
    & severity Suggestion
    & message "Use tuple section: \\x -> (y, x)  ==>  (y,)"
    & note "Requires TupleSections extension"

-- | Both elements tuple construction.
--
-- @
-- \\x -> (x, x)  ==>  join (,)  or  \\x -> (x, x)
-- @
tupleSectionBoth :: Rule
tupleSectionBoth =
  rule "tuple-section-both" $
    match ("\\x -> (x, x)" ==> "join (,)")
    & category Style
    & severity Info
    & message "Duplicate in tuple - consider join (,) or keeping explicit"
    & safetyLevel ManualReview

-- | Nested tuple section.
--
-- @
-- \\x -> (x, (x, y))  ==>  \\x -> (x, (x, y))  -- Keep for clarity
-- @
nestedTupleSection :: Rule
nestedTupleSection =
  rule "nested-tuple-section" $
    match ("\\x -> (x, (_y, x))" ==> "\\x -> (x, (_y, x))")
    & category Style
    & severity Info
    & message "Nested tuple with repeated variable - consider named binding"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Lambda Case Rules
--------------------------------------------------------------------------------

-- | Rules for using LambdaCase extension.
lambdaCaseRules :: [Rule]
lambdaCaseRules =
  [ lambdaToCaseSimple
  , lambdaToCaseMulti
  , lambdaToLambdaCase
  , caseToLambdaCase
  ]

-- | Simple lambda to case.
--
-- @
-- \\x -> case x of { ... }  ==>  \\case { ... }
-- @
lambdaToCaseSimple :: Rule
lambdaToCaseSimple =
  rule "lambda-to-case-simple" $
    match ("\\x -> case x of { _p -> _e }" ==> "\\case { _p -> _e }")
    & category Modernization
    & severity Suggestion
    & message "Use LambdaCase: \\x -> case x of ...  ==>  \\case ..."
    & note "Requires LambdaCase extension"

-- | Multi-branch lambda case.
--
-- @
-- \\x -> case x of
--   Just y  -> y
--   Nothing -> 0
-- ==>
-- \\case
--   Just y  -> y
--   Nothing -> 0
-- @
lambdaToCaseMulti :: Rule
lambdaToCaseMulti =
  rule "lambda-to-case-multi" $
    matchText "\\\\[a-z]+ -> case [a-z]+ of"
    & category Modernization
    & severity Suggestion
    & message "Use LambdaCase for lambda immediately followed by case"
    & note "Requires LambdaCase extension"

-- | Lambda with pattern to LambdaCase.
--
-- @
-- \\(Just x) -> f x  ==>  \\case Just x -> f x  (with more cases)
-- @
lambdaToLambdaCase :: Rule
lambdaToLambdaCase =
  rule "lambda-to-lambda-case" $
    matchText "\\\\\\(Just [a-z]+\\)"
    & category Style
    & severity Info
    & message "Pattern in lambda - consider LambdaCase with explicit Nothing case"
    & safetyLevel ManualReview

-- | Standalone case to lambda case in composition.
--
-- @
-- map (\\x -> case x of ...) xs  ==>  map (\\case ...) xs
-- @
caseToLambdaCase :: Rule
caseToLambdaCase =
  rule "case-to-lambda-case" $
    matchText "map \\(\\\\[a-z]+ -> case [a-z]+ of"
    & category Modernization
    & severity Suggestion
    & message "Use LambdaCase in map"

--------------------------------------------------------------------------------
-- Pattern Refactoring Rules
--------------------------------------------------------------------------------

-- | Rules for simplifying pattern matching.
patternRules :: [Rule]
patternRules =
  [ guardToPattern
  , caseOfCase
  , singleBranchCase
  , asPattern
  , viewPattern
  , patternGuard
  , bangPattern
  , lazyPattern
  ]

-- | Guard can be pattern.
--
-- @
-- foo x | x == Nothing = ...  ==>  foo Nothing = ...
-- @
guardToPattern :: Rule
guardToPattern =
  rule "guard-to-pattern" $
    matchText "\\| [a-z]+ == Nothing"
    & category Style
    & severity Suggestion
    & message "Guard on equality - use pattern match instead"
    & note "foo x | x == Nothing = ... ==> foo Nothing = ..."

-- | Nested case-of-case.
--
-- @
-- case x of
--   A -> case y of ...
--   B -> case y of ...  -- Same inner case
-- @
caseOfCase :: Rule
caseOfCase =
  rule "case-of-case" $
    matchText "case .+ of.+case .+ of.+case .+ of"
    & category Complexity
    & severity Warning
    & message "Nested case expressions - consider restructuring"
    & note "Factor out common inner cases or use guards"

-- | Redundant single-branch case.
--
-- @
-- case x of y -> f y  ==>  f x
-- @
singleBranchCase :: Rule
singleBranchCase =
  rule "single-branch-case" $
    match ("case _x of { _y -> _f _y }" ==> "_f _x")
    & category Style
    & severity Suggestion
    & message "Redundant case with single catch-all pattern"

-- | Suggest as-pattern.
--
-- @
-- foo (x:xs) = bar (x:xs)  ==>  foo lst@(x:xs) = bar lst
-- @
asPattern :: Rule
asPattern =
  rule "as-pattern" $
    matchText "\\([a-z]+:[a-z]+\\) = .+ \\([a-z]+:[a-z]+\\)"
    & category Style
    & severity Suggestion
    & message "Use as-pattern to avoid reconstructing the value"
    & note "foo (x:xs) = ... (x:xs) ... ==> foo lst@(x:xs) = ... lst ..."

-- | Suggest view pattern.
--
-- @
-- foo x | length x > 0 = ...  ==>  foo (length -> n) | n > 0 = ...
-- @
viewPattern :: Rule
viewPattern =
  rule "view-pattern" $
    matchText "\\| length [a-z]+ >"
    & category Modernization
    & severity Info
    & message "Consider ViewPatterns for computed pattern guards"
    & note "Requires ViewPatterns extension"
    & safetyLevel ManualReview

-- | Suggest pattern guard.
--
-- @
-- foo x = case g x of Just y -> f y; Nothing -> z
-- ==>
-- foo x | Just y <- g x = f y | otherwise = z
-- @
patternGuard :: Rule
patternGuard =
  rule "pattern-guard" $
    matchText "case [a-z]+ [a-z]+ of.+Just.+Nothing"
    & category Style
    & severity Info
    & message "Consider pattern guards instead of case"
    & note "Pattern guards can be more concise"
    & safetyLevel ManualReview

-- | Suggest bang pattern for strict evaluation.
--
-- @
-- foo x = let !y = expensive x in ...  -- Use bang pattern
-- @
bangPattern :: Rule
bangPattern =
  rule "bang-pattern" $
    matchText "let ![a-z]+ ="
    & category Performance
    & severity Info
    & message "Using bang pattern for strict let binding"
    & note "Good for preventing space leaks, ensure it's intentional"

-- | Warn about lazy pattern in strict context.
--
-- @
-- foo ~(x, y) = ...  -- Lazy pattern may cause space leak
-- @
lazyPattern :: Rule
lazyPattern =
  rule "lazy-pattern" $
    matchText "~\\("
    & category Performance
    & severity Info
    & message "Lazy pattern (~) may cause space leaks"
    & note "Lazy patterns defer evaluation, which can accumulate thunks"

--------------------------------------------------------------------------------
-- Point-Free Style Rules
--------------------------------------------------------------------------------

-- | Rules for point-free composition.
pointFreeRules :: [Rule]
pointFreeRules =
  [ composeSimple
  , composeChain
  , flipCompose
  , onCompose
  , applyCompose
  ]

-- | Simple composition suggestion.
--
-- @
-- \\x -> f (g x)  ==>  f . g
-- @
composeSimple :: Rule
composeSimple =
  rule "compose-simple" $
    match ("\\x -> _f (_g x)" ==> "_f . _g")
    & category Style
    & severity Suggestion
    & message "Use function composition"

-- | Chain composition.
--
-- @
-- \\x -> f (g (h x))  ==>  f . g . h
-- @
composeChain :: Rule
composeChain =
  rule "compose-chain" $
    match ("\\x -> _f (_g (_h x))" ==> "_f . _g . _h")
    & category Style
    & severity Suggestion
    & message "Use composition chain"

-- | Composition with flip.
--
-- @
-- \\x y -> f (g x) y  ==>  flip f . g  (sometimes)
-- @
flipCompose :: Rule
flipCompose =
  rule "flip-compose" $
    match ("\\x y -> _f y (_g x)" ==> "(. _g) . _f")
    & category Style
    & severity Info
    & message "Complex composition - consider named binding"
    & safetyLevel ManualReview

-- | Use @on@ combinator.
--
-- @
-- \\x y -> f (g x) (g y)  ==>  f \`on\` g
-- @
onCompose :: Rule
onCompose =
  rule "on-compose" $
    match ("\\x y -> _f (_g x) (_g y)" ==> "_f `on` _g")
    & category Style
    & severity Suggestion
    & message "Use 'on' combinator from Data.Function"
    & note "on f g x y = f (g x) (g y)"

-- | Apply in composition.
--
-- @
-- \\x -> (f x, g x)  ==>  f &&& g  (with Control.Arrow)
-- @
applyCompose :: Rule
applyCompose =
  rule "apply-compose" $
    match ("\\x -> (_f x, _g x)" ==> "_f &&& _g")
    & category Style
    & severity Info
    & message "Consider (&&&) from Control.Arrow"
    & note "(f &&& g) x = (f x, g x)"
    & safetyLevel ManualReview
