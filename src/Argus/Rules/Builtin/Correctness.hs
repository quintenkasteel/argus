{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Correctness
-- Description : Correctness rules for detecting logical errors
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- This module provides lint rules for detecting correctness issues
-- and potential logic errors in Haskell code. These rules focus on
-- common mistakes that lead to incorrect behavior.
--
-- == Rule Categories
--
-- * __Equality Issues__: Floating point, reference equality pitfalls
-- * __Monad Laws__: Violations and misuse
-- * __Type Errors__: Common type-related mistakes
-- * __Logic Errors__: Inverted conditions, off-by-one errors
--
-- == Severity
--
-- Correctness rules typically have higher severity than style rules
-- because they indicate actual bugs, not just style preferences.
--
-- == References
--
-- * <https://wiki.haskell.org/Common_Misconceptions Common Misconceptions>
-- * <https://www.haskell.org/definition/haskell2010.pdf Haskell 2010 Report>

module Argus.Rules.Builtin.Correctness
  ( -- * Rule Sets
    correctnessRules
  , equalityRules
  , monadRules
  , typeRules
  , logicRules

    -- * Individual Rules
    -- ** Equality Issues
  , avoidFloatEq
  , avoidDoubleEq
  , avoidStmRefEq
  , avoidWeakPtrEq

    -- ** Monad Laws
  , monadLeftIdentity
  , monadRightIdentity
  , monadAssociativity
  , funmapId
  , functorComposition
  , applicativeIdentity
  , applicativeHomomorphism

    -- ** Type Issues
  , avoidShowRead
  , avoidGenericFromJSON
  , typeDefaulting
  , ambiguousTypeVariable

    -- ** Logic Errors
  , invertedCondition
  , constantCondition
  , constantFalseCondition
  , unreachablePattern
  , overlappingPattern
  , incompletePattern
  , unusedBinding
  , infiniteRecursion
  , nanComparison
  , ordCompareToSelf
  , divByZero

    -- * Rule Metadata
  , correctnessRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All correctness rules combined.
--
-- These rules detect actual bugs and logic errors.
-- Enable by default and investigate all findings.
correctnessRules :: [Rule]
correctnessRules = equalityRules ++ monadRules ++ typeRules ++ logicRules

-- | Rules for equality-related correctness issues.
--
-- Equality comparison can be subtle, especially with:
--
-- * Floating point numbers
-- * Reference types
-- * Polymorphic comparisons
equalityRules :: [Rule]
equalityRules =
  [ avoidFloatEq
  , avoidDoubleEq
  , avoidStmRefEq
  , avoidWeakPtrEq
  ]

-- | Rules based on typeclass laws.
--
-- Violating monad/functor laws leads to unpredictable behavior.
-- These rules detect common law violations.
monadRules :: [Rule]
monadRules =
  [ monadLeftIdentity
  , monadRightIdentity
  , functorComposition
  ]

-- | Rules for type-related correctness issues.
typeRules :: [Rule]
typeRules =
  [ avoidShowRead
  , avoidGenericFromJSON
  ]

-- | Rules for logic errors and unreachable code.
logicRules :: [Rule]
logicRules =
  [ invertedCondition
  , constantCondition
  , constantFalseCondition
  , infiniteRecursion
  , nanComparison
  , ordCompareToSelf
  , divByZero
  ]

--------------------------------------------------------------------------------
-- Equality Issues
--------------------------------------------------------------------------------

-- | Avoid direct equality comparison on Float.
--
-- == Problem
--
-- Floating point equality is almost never what you want:
--
-- @
-- 0.1 + 0.2 == 0.3  -- False!
--
-- let x = 0.1 + 0.2
--     y = 0.3
-- in x == y  -- False due to representation
-- @
--
-- == Solution
--
-- Use approximate comparison:
--
-- @
-- nearlyEqual :: Float -> Float -> Float -> Bool
-- nearlyEqual epsilon x y = abs (x - y) < epsilon
--
-- nearlyEqual 1e-6 (0.1 + 0.2) 0.3  -- True
-- @
--
-- Or use exact types like 'Rational' or 'Scientific'.
avoidFloatEq :: Rule
avoidFloatEq = rule "correctness/avoid-float-eq" $
  match (pat "(==) :: Float -> Float -> Bool")
  & severity Warning
  & message "Floating point equality is unreliable - use approximate comparison"
  & note "Floating point cannot exactly represent many decimals; use epsilon comparison"
  & category Correctness
  & safetyLevel ManualReview

-- | Avoid direct equality comparison on Double.
avoidDoubleEq :: Rule
avoidDoubleEq = rule "correctness/avoid-double-eq" $
  match (pat "(==) :: Double -> Double -> Bool")
  & severity Warning
  & message "Floating point equality is unreliable - use approximate comparison"
  & note "Consider using Scientific for exact decimal math, or epsilon comparison"
  & category Correctness
  & safetyLevel ManualReview

-- | Avoid STRef/IORef reference equality.
--
-- == Problem
--
-- Comparing mutable references compares addresses, not contents:
--
-- @
-- do
--   ref1 <- newIORef 42
--   ref2 <- newIORef 42
--   print (ref1 == ref2)  -- False! Different addresses
-- @
--
-- == Solution
--
-- Compare contents explicitly:
--
-- @
-- do
--   ref1 <- newIORef 42
--   ref2 <- newIORef 42
--   v1 <- readIORef ref1
--   v2 <- readIORef ref2
--   print (v1 == v2)  -- True
-- @
avoidStmRefEq :: Rule
avoidStmRefEq = rule "correctness/avoid-ref-eq" $
  match (pat "== :: IORef"
         `fromModule` "Data.IORef")
  & severity Warning
  & message "IORef equality compares references, not values - read and compare values"
  & note "To compare contents, use readIORef and compare the results"
  & category Correctness
  & safetyLevel ManualReview

-- | Avoid WeakPtr equality.
avoidWeakPtrEq :: Rule
avoidWeakPtrEq = rule "correctness/avoid-weakptr-eq" $
  match (pat "== :: Weak")
  & severity Warning
  & message "Weak pointer equality is reference-based and may behave unexpectedly"
  & category Correctness
  & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Monad Laws
--------------------------------------------------------------------------------

-- | Monad left identity: return a >>= f  ===  f a
--
-- == The Law
--
-- @
-- return a >>= f  ===  f a
-- @
--
-- This rule detects violations and suggests the simpler form.
monadLeftIdentity :: Rule
monadLeftIdentity = rule "correctness/monad-left-identity" $
  match ("return $X >>= $F" ==> "$F $X")
  & severity Warning
  & message "By monad left identity: return x >>= f = f x"
  & note "Wrapping in return only to immediately bind is redundant"
  & category Correctness
  & safetyLevel Safe

-- | Monad right identity: m >>= return  ===  m
--
-- == The Law
--
-- @
-- m >>= return  ===  m
-- @
monadRightIdentity :: Rule
monadRightIdentity = rule "correctness/monad-right-identity" $
  match ("$X >>= return" ==> "$X")
  & severity Warning
  & message "By monad right identity: x >>= return = x"
  & note "Binding to return is always redundant"
  & category Correctness
  & safetyLevel Safe

-- | Monad associativity.
--
-- == The Law
--
-- @
-- (m >>= f) >>= g  ===  m >>= (\\x -> f x >>= g)
-- @
monadAssociativity :: Rule
monadAssociativity = rule "correctness/monad-associativity" $
  match (pat "(>>= $F) >>= $G")
  & severity Info
  & message "Consider monad associativity for potential simplification"
  & note "(m >>= f) >>= g = m >>= (\\x -> f x >>= g)"
  & category Correctness
  & safetyLevel ManualReview

-- | Functor identity: fmap id  ===  id
--
-- == The Law
--
-- @
-- fmap id  ===  id
-- @
funmapId :: Rule
funmapId = rule "correctness/fmap-id" $
  match ("fmap id $X" ==> "$X")
  & severity Warning
  & message "By functor identity: fmap id x = x"
  & note "fmap id is always equivalent to id"
  & category Correctness
  & safetyLevel Safe

-- | Functor composition: fmap (f . g)  ===  fmap f . fmap g
--
-- == The Law
--
-- @
-- fmap (f . g)  ===  fmap f . fmap g
-- @
--
-- The left side is more efficient (single traversal).
functorComposition :: Rule
functorComposition = rule "correctness/fmap-composition" $
  match ("fmap $F $ fmap $G $X" ==> "fmap ($F . $G) $X")
  & severity Suggestion
  & message "Use functor composition: fmap f . fmap g = fmap (f . g)"
  & note "Single fmap is more efficient than two traversals"
  & category Correctness
  & safetyLevel Safe

-- | Applicative identity: pure id <*> v  ===  v
applicativeIdentity :: Rule
applicativeIdentity = rule "correctness/applicative-identity" $
  match ("pure id <*> $X" ==> "$X")
  & severity Warning
  & message "By applicative identity: pure id <*> x = x"
  & category Correctness
  & safetyLevel Safe

-- | Applicative homomorphism: pure f <*> pure x  ===  pure (f x)
applicativeHomomorphism :: Rule
applicativeHomomorphism = rule "correctness/applicative-homomorphism" $
  match ("pure $F <*> pure $X" ==> "pure ($F $X)")
  & severity Suggestion
  & message "By applicative homomorphism: pure f <*> pure x = pure (f x)"
  & category Correctness
  & safetyLevel Safe

--------------------------------------------------------------------------------
-- Type Issues
--------------------------------------------------------------------------------

-- | Avoid show/read roundtrip.
--
-- == Problem
--
-- Using @read . show@ for serialization is fragile:
--
-- @
-- read (show x) :: MyType  -- Fragile!
-- @
--
-- Issues:
--
-- * Show/Read instances may not roundtrip
-- * Performance is poor
-- * No error handling
--
-- == Solution
--
-- Use proper serialization:
--
-- @
-- import Data.Aeson (encode, decode)
--
-- -- Or for binary data
-- import Data.Serialize (encode, decode)
-- @
avoidShowRead :: Rule
avoidShowRead = rule "correctness/avoid-show-read" $
  match ("read $ show $X" ==> "$X")
  & severity Warning
  & message "show/read roundtrip is fragile and inefficient"
  & note "Use proper serialization (Aeson, cereal) or derive instances carefully"
  & category Correctness
  & safetyLevel MostlySafe

-- | Warn about generic FromJSON with default options.
--
-- == Problem
--
-- Generic FromJSON with default options can be dangerous:
--
-- @
-- data User = User { userName :: String, userAge :: Int }
--   deriving (Generic)
--
-- instance FromJSON User  -- Default options!
-- @
--
-- Default options use unmodified field names, which may:
--
-- * Expose implementation details
-- * Be inconsistent with API contracts
-- * Change if field names change
--
-- == Solution
--
-- Explicitly specify options:
--
-- @
-- instance FromJSON User where
--   parseJSON = genericParseJSON defaultOptions
--     { fieldLabelModifier = drop 4  -- Remove \"user\" prefix
--     }
-- @
avoidGenericFromJSON :: Rule
avoidGenericFromJSON = rule "correctness/explicit-json-options" $
  match (pat "instance FromJSON"
         `where_` usesDefaultOptions)
  & severity Info
  & message "Consider explicit JSON options instead of default for safety"
  & note "Default options may not match your API contract"
  & category Correctness
  & safetyLevel ManualReview

-- | Warn about ambiguous type defaulting.
typeDefaulting :: Rule
typeDefaulting = rule "correctness/type-defaulting" $
  match (pat "default (")
  & severity Info
  & message "Type defaulting can lead to unexpected concrete types"
  & note "Consider explicit type annotations for clarity"
  & category Correctness
  & safetyLevel ManualReview

-- | Warn about ambiguous type variables in certain contexts.
ambiguousTypeVariable :: Rule
ambiguousTypeVariable = rule "correctness/ambiguous-type" $
  match (pat ":: forall"
         `where_` hasAmbiguousType)
  & severity Info
  & message "Potential ambiguous type variable - ensure type is determined"
  & category Correctness
  & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Logic Errors
--------------------------------------------------------------------------------

-- | Detect potentially inverted conditions.
--
-- == Problem
--
-- @
-- if null xs then doSomethingWithXs else handleEmpty
-- -- Probably inverted!
-- @
--
-- == Solution
--
-- @
-- if null xs then handleEmpty else doSomethingWithXs
-- @
invertedCondition :: Rule
invertedCondition = rule "correctness/inverted-condition" $
  match (pat "if not $COND then $A else $B")
  & severity Info
  & message "Consider flipping branches to remove 'not' in condition"
  & note "if not c then a else b = if c then b else a"
  & category Correctness
  & safetyLevel MostlySafe

-- | Detect constant conditions.
--
-- == Problem
--
-- @
-- if True then x else y  -- Always takes True branch
-- if False then x else y  -- Always takes False branch
-- @
--
-- == Solution
--
-- Remove the dead code:
--
-- @
-- x  -- for if True
-- y  -- for if False
-- @
constantCondition :: Rule
constantCondition = rule "correctness/constant-condition" $
  match ("if True then $X else $Y" ==> "$X")
  & severity Warning
  & message "Condition is always True - else branch is dead code"
  & note "This may indicate a logic error or unfinished code"
  & category Correctness
  & safetyLevel Safe

-- | Detect unreachable patterns in case expressions.
unreachablePattern :: Rule
unreachablePattern = rule "correctness/unreachable-pattern" $
  match (pat "case $X of { _ -> $A; $PAT -> $B }"
         `where_` wildcardNotLast)
  & severity Warning
  & message "Pattern after wildcard is unreachable"
  & note "Wildcard _ matches everything; subsequent patterns never match"
  & category Correctness
  & safetyLevel ManualReview

-- | Detect overlapping patterns.
overlappingPattern :: Rule
overlappingPattern = rule "correctness/overlapping-pattern" $
  match (pat "case"
         `where_` hasOverlap)
  & severity Warning
  & message "Patterns may overlap - later patterns may be unreachable"
  & note "Check that each pattern handles distinct cases"
  & category Correctness
  & safetyLevel ManualReview

-- | Detect incomplete pattern matches.
--
-- == Problem
--
-- @
-- data Color = Red | Green | Blue
--
-- colorName :: Color -> String
-- colorName Red = \"red\"
-- colorName Green = \"green\"
-- -- Missing Blue case!
-- @
incompletePattern :: Rule
incompletePattern = rule "correctness/incomplete-pattern" $
  match (pat "case"
         `where_` isIncomplete)
  & severity Warning
  & message "Pattern match may be incomplete"
  & note "Add missing cases or use wildcard with explicit error"
  & category Correctness
  & safetyLevel ManualReview

-- | Detect unused let bindings.
unusedBinding :: Rule
unusedBinding = rule "correctness/unused-binding" $
  match (pat "let $X = $Y in $Z"
         `where_` notUsedIn "$Z" "$X")
  & severity Warning
  & message "Let binding appears to be unused"
  & note "Unused bindings may indicate dead code or unfinished implementation"
  & category Correctness
  & safetyLevel ManualReview

-- | Detect obvious infinite recursion.
--
-- == Problem
--
-- @
-- f x = f x  -- Infinite loop!
--
-- g = g  -- Also infinite
-- @
--
-- == Solution
--
-- Ensure recursion has a base case:
--
-- @
-- f [] = []
-- f (x:xs) = x : f xs  -- Terminates on empty list
-- @
infiniteRecursion :: Rule
infiniteRecursion = rule "correctness/infinite-recursion" $
  match (pat "$F = $F")
  & severity Error
  & message "Obvious infinite recursion detected - function calls itself with no change"
  & note "Ensure recursive functions have base cases and progress toward them"
  & category Correctness
  & safetyLevel ManualReview

-- | Detect constant False condition.
--
-- == Problem
--
-- @
-- if False then x else y  -- Always takes else branch
-- @
--
-- == Solution
--
-- @
-- y  -- Just use the else branch
-- @
constantFalseCondition :: Rule
constantFalseCondition = rule "correctness/constant-false-condition" $
  match ("if False then $X else $Y" ==> "$Y")
  & severity Warning
  & message "Condition is always False - then branch is dead code"
  & note "This may indicate a logic error or commented-out code"
  & category Correctness
  & safetyLevel Safe

-- | Detect NaN comparison.
--
-- == Problem
--
-- In IEEE floating point, NaN comparisons are always False:
--
-- @
-- let nan = 0/0 :: Double
-- nan == nan     -- False!
-- nan /= nan     -- True!
-- nan < nan      -- False!
-- @
--
-- == Solution
--
-- Use 'isNaN' to check for NaN:
--
-- @
-- import Data.Number.Extras (isNaN)
--
-- if isNaN x then handleNaN else useValue x
-- @
nanComparison :: Rule
nanComparison = rule "correctness/nan-comparison" $
  match (pat "isNaN $X == True")
  & severity Warning
  & message "Use isNaN directly rather than comparing to True"
  & note "isNaN already returns a Bool; comparing to True is redundant"
  & category Correctness
  & safetyLevel Safe

-- | Detect compare x x pattern.
--
-- == Problem
--
-- @
-- compare x x  -- Always EQ (for most Ord instances)
-- @
ordCompareToSelf :: Rule
ordCompareToSelf = rule "correctness/compare-to-self" $
  match ("compare $X $X" ==> "EQ")
  & severity Warning
  & message "compare x x is always EQ"
  & note "This is likely a bug - did you mean to compare different values?"
  & category Correctness
  & safetyLevel MostlySafe

-- | Detect division by zero (literal).
--
-- == Problem
--
-- @
-- x `div` 0   -- Runtime exception!
-- x `quot` 0  -- Runtime exception!
-- x / 0       -- Infinity or NaN for floating point
-- @
--
-- == Note
--
-- Only catches literal zero. Dynamic division by zero requires
-- runtime checks.
divByZero :: Rule
divByZero = rule "correctness/div-by-zero" $
  match (pat "$X `div` 0")
  & severity Error
  & message "Division by zero will throw an exception"
  & note "Add a guard to check for zero before dividing"
  & category Correctness
  & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Metadata
--------------------------------------------------------------------------------

-- | Total number of correctness rules.
correctnessRuleCount :: Int
correctnessRuleCount = length correctnessRules
