{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Style
-- Description : Style and modernization rules for Haskell code
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- This module provides lint rules for improving code style and encouraging
-- modern Haskell idioms. These rules focus on readability, consistency,
-- and leveraging newer language features appropriately.
--
-- == Rule Categories
--
-- * __Applicative Style__: Using Applicative over Monad when possible
-- * __Redundant Code__: Removing unnecessary constructs
-- * __Modern Idioms__: Using current best practices
-- * __Simplification__: Making code clearer
--
-- == Philosophy
--
-- Style rules aim for:
--
-- * Readability over cleverness
-- * Consistency within a codebase
-- * Gradual modernization
--
-- These are suggestions, not requirements. Team style guides take precedence.
--
-- == References
--
-- * <https://wiki.haskell.org/Applicative_functor Applicative Functors>
-- * <https://www.fpcomplete.com/haskell/tutorial/applicative-syntax/ Applicative Syntax>
-- * <https://wiki.haskell.org/Do_notation_considered_harmful Do Notation Considered Harmful>

module Argus.Rules.Builtin.Style
  ( -- * Rule Sets
    styleRules
  , applicativeRules
  , redundantCodeRules
  , modernizationRules
  , simplificationRules

    -- * Individual Rules
    -- ** Applicative Style
  , preferPure
  , preferApplicative
  , preferTraverse_
  , preferFor_
  , applicativeInDo
  , monadToApplicative

    -- ** Redundant Code
  , redundantDo
  , redundantBracket
  , redundantReturn
  , redundantBind
  , redundantId
  , redundantGuard
  , redundantIf
  , redundantCase

    -- ** Modernization
  , useDerivingStrategies
  , useRecordWildCards
  , useNamedFieldPuns
  , useLambdaCase
  , useMultiWayIf
  , useBlockArguments

    -- ** Simplification
  , simplifyLambda
  , simplifyComposition
  , simplifyFlip
  , simplifyConst
  , etaReduce
  , pointFree
  , fmapFmapFusion
  , simplifyApplicativeId
  , maybeJustNothing
  , ifThenNot
  , notNot
  , andTrue
  , orFalse
  , ifSameBranches
  , compareToSelf

    -- * Rule Metadata
  , styleRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All style rules combined.
--
-- These rules improve code clarity and modernization.
-- Apply selectively based on team style guidelines.
styleRules :: [Rule]
styleRules = applicativeRules ++ redundantCodeRules ++ modernizationRules ++ simplificationRules

-- | Rules promoting Applicative style over Monadic.
--
-- When Applicative suffices, prefer it over Monad:
--
-- * Clearer structure (effects don't depend on values)
-- * Better parallelization potential
-- * Works with more types (ApplicativeDo)
applicativeRules :: [Rule]
applicativeRules =
  [ preferPure
  , preferApplicative
  , preferTraverse_
  , preferFor_
  , applicativeInDo
  ]

-- | Rules detecting redundant code constructs.
--
-- Redundant code obscures intent and adds noise.
-- These rules flag common redundancies.
redundantCodeRules :: [Rule]
redundantCodeRules =
  [ redundantDo
  , redundantBracket
  , redundantReturn
  , redundantBind
  , redundantId
  , redundantGuard
  , redundantIf
  , redundantCase
  ]

-- | Rules for modernizing code to use newer idioms.
--
-- These rules suggest newer language extensions and patterns
-- that can improve code clarity.
modernizationRules :: [Rule]
modernizationRules =
  [ useDerivingStrategies
  , useLambdaCase
  ]

-- | Rules for simplifying code expressions.
--
-- These rules suggest simpler, more idiomatic ways to express
-- the same computation.
simplificationRules :: [Rule]
simplificationRules =
  [ simplifyLambda
  , simplifyComposition
  , etaReduce
  , fmapFmapFusion
  , simplifyApplicativeId
  , maybeJustNothing
  , ifThenNot
  , notNot
  , andTrue
  , orFalse
  , ifSameBranches
  , compareToSelf
  ]

--------------------------------------------------------------------------------
-- Applicative Style
--------------------------------------------------------------------------------

-- | Prefer 'pure' over 'return'.
--
-- == Problem
--
-- 'return' is the old Monad-specific way to lift a value:
--
-- @
-- addOne :: Int -> IO Int
-- addOne x = return (x + 1)  -- Old style
-- @
--
-- == Solution
--
-- Use 'pure' from Applicative (re-exported by Prelude):
--
-- @
-- addOne :: Int -> IO Int
-- addOne x = pure (x + 1)  -- Modern style
-- @
--
-- == Rationale
--
-- * 'pure' works for any Applicative, not just Monad
-- * Consistent with Functor/Applicative/Monad hierarchy
-- * 'return' is essentially deprecated style since GHC 7.10
preferPure :: Rule
preferPure = rule "style/prefer-pure" $
  match ("return $X" ==> "pure $X")
  & severity Suggestion
  & message "Use 'pure' instead of 'return' for modern Applicative style"
  & note "Since GHC 7.10, 'pure' is the preferred way to lift values"
  & category Style
  & safetyLevel Safe
  & fixDescription "Replace 'return' with 'pure'"

-- | Prefer Applicative operators over monadic bind when possible.
--
-- == Problem
--
-- Using monadic bind when Applicative suffices:
--
-- @
-- do
--   x <- getX
--   y <- getY
--   return (x + y)  -- Order doesn't matter!
-- @
--
-- == Solution
--
-- Use Applicative style:
--
-- @
-- (+) <$> getX <*> getY
--
-- -- Or with ApplicativeDo:
-- do
--   x <- getX
--   y <- getY
--   pure (x + y)  -- ApplicativeDo desugars efficiently
-- @
preferApplicative :: Rule
preferApplicative = rule "style/prefer-applicative" $
  match ("$X >>= \\$Y -> return ($F $Y)" ==> "$F <$> $X")
  & severity Suggestion
  & message "Use Applicative (<$>) instead of monadic bind for simple mapping"
  & note "When the bound value is only used once in a pure expression, use fmap"
  & category Style
  & safetyLevel Safe

-- | Prefer @traverse_@ over @mapM_@ for consistency.
--
-- == Rationale
--
-- @traverse_@ is the Applicative version, making it clear
-- that we only need Applicative, not full Monad power.
preferTraverse_ :: Rule
preferTraverse_ = rule "style/prefer-traverse_" $
  match ("mapM_ $F $XS" ==> "traverse_ $F $XS"
         `addImport` ("Data.Foldable", ["traverse_"]))
  & severity Suggestion
  & message "Use traverse_ instead of mapM_ for Applicative consistency"
  & note "traverse_ only requires Applicative; mapM_ historically required Monad"
  & category Style
  & safetyLevel Safe

-- | Prefer @for_@ over @forM_@ for consistency.
preferFor_ :: Rule
preferFor_ = rule "style/prefer-for_" $
  match ("forM_ $XS $F" ==> "for_ $XS $F"
         `addImport` ("Data.Foldable", ["for_"]))
  & severity Suggestion
  & message "Use for_ instead of forM_ for Applicative consistency"
  & note "for_ is traverse_ with arguments flipped; only needs Applicative"
  & category Style
  & safetyLevel Safe

-- | Detect Applicative patterns in do blocks.
--
-- @
-- do
--   x <- action
--   pure x
-- -- Can be simplified to:
-- action
-- @
applicativeInDo :: Rule
applicativeInDo = rule "style/applicative-in-do" $
  match ("do { $X <- $ACTION; pure $X }" ==> "$ACTION")
  & severity Suggestion
  & message "This do block is equivalent to just the action"
  & note "Binding only to return the same value is redundant"
  & category Style
  & safetyLevel Safe

-- | Detect patterns that can use Applicative instead of Monad.
monadToApplicative :: Rule
monadToApplicative = rule "style/monad-to-applicative" $
  match ("$X >>= return" ==> "$X")
  & severity Suggestion
  & message "x >>= return is equivalent to x"
  & note "This is the right identity monad law"
  & category Style
  & safetyLevel Safe

--------------------------------------------------------------------------------
-- Redundant Code
--------------------------------------------------------------------------------

-- | Detect redundant do notation.
--
-- == Problem
--
-- @
-- do
--   action  -- Only one statement!
-- @
--
-- == Solution
--
-- @
-- action
-- @
redundantDo :: Rule
redundantDo = rule "style/redundant-do" $
  match (pat "do { $X }"
         `where_` notBind "$X")
  & severity Suggestion
  & message "Redundant do - single action doesn't need do notation"
  & note "Use do only for sequencing multiple actions"
  & category Style
  & safetyLevel Safe

-- | Detect redundant brackets in function application.
--
-- == Problem
--
-- @
-- f (x)  -- Redundant brackets
-- @
--
-- == Solution
--
-- @
-- f x
-- @
redundantBracket :: Rule
redundantBracket = rule "style/redundant-bracket" $
  match ("($X)" ==> "$X"
         `where_` isSimple "$X")
  & severity Suggestion
  & message "Redundant brackets around simple expression"
  & category Style
  & safetyLevel Safe

-- | Detect redundant return after action.
--
-- == Problem
--
-- @
-- do
--   x <- action
--   return x  -- Just return what you got
-- @
--
-- == Solution
--
-- @
-- action
-- @
redundantReturn :: Rule
redundantReturn = rule "style/redundant-return" $
  match ("$X >>= return" ==> "$X")
  & severity Suggestion
  & message "x >>= return is equivalent to x (right identity)"
  & category Style
  & safetyLevel Safe

-- | Detect redundant bind.
--
-- == Problem
--
-- @
-- return x >>= f  -- Unnecessary wrapping
-- @
--
-- == Solution
--
-- @
-- f x
-- @
redundantBind :: Rule
redundantBind = rule "style/redundant-bind" $
  match ("return $X >>= $F" ==> "$F $X")
  & severity Suggestion
  & message "return x >>= f is equivalent to f x (left identity)"
  & category Style
  & safetyLevel Safe

-- | Detect redundant id application.
redundantId :: Rule
redundantId = rule "style/redundant-id" $
  match ("id $X" ==> "$X")
  & severity Suggestion
  & message "id is redundant - just use the value directly"
  & category Style
  & safetyLevel Safe

-- | Detect redundant guard True.
redundantGuard :: Rule
redundantGuard = rule "style/redundant-guard" $
  match (pat "guard True")
  & severity Suggestion
  & message "guard True always succeeds - can be removed"
  & category Style
  & safetyLevel Safe

-- | Detect redundant if expressions.
--
-- == Problem
--
-- @
-- if condition then True else False  -- Just use the condition!
-- @
--
-- == Solution
--
-- @
-- condition
-- @
redundantIf :: Rule
redundantIf = rule "style/redundant-if" $
  match ("if $COND then True else False" ==> "$COND")
  & severity Suggestion
  & message "if x then True else False is equivalent to x"
  & category Style
  & safetyLevel Safe

-- | Detect redundant case expressions.
redundantCase :: Rule
redundantCase = rule "style/redundant-case" $
  match ("case $X of { True -> True; False -> False }" ==> "$X")
  & severity Suggestion
  & message "This case expression is equivalent to the scrutinee"
  & category Style
  & safetyLevel Safe

--------------------------------------------------------------------------------
-- Modernization
--------------------------------------------------------------------------------

-- | Suggest DerivingStrategies extension.
--
-- == Problem
--
-- Without DerivingStrategies, it's unclear how instances are derived:
--
-- @
-- data Foo = Foo Int deriving (Show, Eq)
-- @
--
-- == Solution
--
-- Use explicit strategies:
--
-- @
-- {-# LANGUAGE DerivingStrategies #-}
--
-- data Foo = Foo Int
--   deriving stock (Show, Eq)
--   deriving newtype (Num)  -- For newtypes
--   deriving (ToJSON) via Foo  -- With DerivingVia
-- @
useDerivingStrategies :: Rule
useDerivingStrategies = rule "style/use-DerivingStrategies" $
  match (pat "deriving ("
         `where_` noDerivingStrategy)
  & severity Info
  & message "Consider DerivingStrategies for explicit deriving clarity"
  & note "DerivingStrategies makes it clear how instances are derived (stock, newtype, anyclass, via)"
  & category Style
  & safetyLevel ManualReview

-- | Suggest RecordWildCards extension.
--
-- == Problem
--
-- Verbose record access:
--
-- @
-- f record = foo record + bar record + baz record
-- @
--
-- == Solution
--
-- @
-- {-# LANGUAGE RecordWildCards #-}
--
-- f Record{..} = foo + bar + baz
-- @
useRecordWildCards :: Rule
useRecordWildCards = rule "style/use-RecordWildCards" $
  match (pat "RecordWildCards")
  & severity Info
  & message "RecordWildCards can simplify record field access"
  & note "Use Record{..} to bring all fields into scope"
  & category Style
  & safetyLevel ManualReview

-- | Suggest NamedFieldPuns extension.
useNamedFieldPuns :: Rule
useNamedFieldPuns = rule "style/use-NamedFieldPuns" $
  match (pat "NamedFieldPuns")
  & severity Info
  & message "NamedFieldPuns allows { fieldName } instead of { fieldName = fieldName }"
  & category Style
  & safetyLevel ManualReview

-- | Suggest LambdaCase for case-analyzing lambdas.
--
-- == Problem
--
-- @
-- \\x -> case x of
--   Nothing -> ...
--   Just y  -> ...
-- @
--
-- == Solution
--
-- @
-- {-# LANGUAGE LambdaCase #-}
--
-- \\case
--   Nothing -> ...
--   Just y  -> ...
-- @
useLambdaCase :: Rule
useLambdaCase = rule "style/use-LambdaCase" $
  match ("\\$X -> case $X of { $ALTS }" ==> "\\case { $ALTS }")
  & severity Suggestion
  & message "Use LambdaCase for cleaner case-analyzing lambdas"
  & note "Requires LambdaCase extension"
  & category Style
  & safetyLevel MostlySafe

-- | Suggest MultiWayIf for complex conditionals.
useMultiWayIf :: Rule
useMultiWayIf = rule "style/use-MultiWayIf" $
  match (pat "if $A then $X else if $B then $Y else $Z")
  & severity Suggestion
  & message "Consider MultiWayIf for cleaner nested conditionals"
  & note "if | cond1 -> x | cond2 -> y | otherwise -> z"
  & category Style
  & safetyLevel ManualReview

-- | Suggest BlockArguments for cleaner syntax.
useBlockArguments :: Rule
useBlockArguments = rule "style/use-BlockArguments" $
  match (pat "BlockArguments")
  & severity Info
  & message "BlockArguments allows 'when condition do' instead of 'when condition $ do'"
  & category Style
  & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Simplification
--------------------------------------------------------------------------------

-- | Simplify trivial lambda expressions.
--
-- == Problem
--
-- @
-- \\x -> f x  -- Just f!
-- @
--
-- == Solution
--
-- @
-- f
-- @
simplifyLambda :: Rule
simplifyLambda = rule "style/simplify-lambda" $
  match ("\\$X -> $F $X" ==> "$F"
         `where_` notUsedIn "$F" "$X")
  & severity Suggestion
  & message "Lambda can be eta-reduced to just the function"
  & note "\\x -> f x is equivalent to f (eta reduction)"
  & category Style
  & safetyLevel MostlySafe

-- | Simplify composition chains.
--
-- == Problem
--
-- @
-- f . (g . h)  -- Unnecessary brackets
-- @
--
-- == Solution
--
-- @
-- f . g . h  -- (.) is associative
-- @
simplifyComposition :: Rule
simplifyComposition = rule "style/simplify-composition" $
  match ("$F . ($G . $H)" ==> "$F . $G . $H")
  & severity Suggestion
  & message "Composition is associative - parentheses not needed"
  & category Style
  & safetyLevel Safe

-- | Simplify flip usage.
--
-- == Problem
--
-- @
-- flip ($)  -- Has a simpler name
-- @
--
-- == Solution
--
-- @
-- (&)  -- from Data.Function
-- @
simplifyFlip :: Rule
simplifyFlip = rule "style/simplify-flip" $
  match ("flip ($)" ==> "(&)"
         `addImport` ("Data.Function", ["(&)"]))
  & severity Suggestion
  & message "Use (&) instead of flip ($)"
  & note "(&) is the flipped application operator from Data.Function"
  & category Style
  & safetyLevel Safe

-- | Simplify const usage.
--
-- == Problem
--
-- @
-- const x y  -- Just x
-- @
--
-- == Solution
--
-- @
-- x
-- @
simplifyConst :: Rule
simplifyConst = rule "style/simplify-const" $
  match ("const $X $Y" ==> "$X")
  & severity Suggestion
  & message "const x y is just x"
  & category Style
  & safetyLevel Safe

-- | Suggest eta reduction for cleaner definitions.
--
-- == Problem
--
-- @
-- foo x = bar x
-- @
--
-- == Solution
--
-- @
-- foo = bar
-- @
--
-- == Caveats
--
-- Eta reduction can change:
--
-- * Strictness (monomorphism restriction)
-- * Polymorphism
-- * Sharing behavior
etaReduce :: Rule
etaReduce = rule "style/eta-reduce" $
  match ("\\$X -> $F $X" ==> "$F"
         `where_` isEtaReducible "$F" "$X")
  & severity Suggestion
  & message "Consider eta reduction: \\x -> f x = f"
  & note "Be aware of strictness and polymorphism implications"
  & category Style
  & safetyLevel MostlySafe

-- | Suggest point-free style where clear.
--
-- == When Point-Free Helps
--
-- @
-- -- Before
-- \\x -> length (filter p x)
--
-- -- After
-- length . filter p
-- @
--
-- == When Point-Free Hurts
--
-- @
-- -- Before (clear)
-- \\x y -> x + y * 2
--
-- -- After (unclear)
-- (+) . (2*)  -- What does this even mean?
-- @
pointFree :: Rule
pointFree = rule "style/point-free" $
  match ("\\$X -> $F ($G $X)" ==> "$F . $G"
         `where_` notUsedIn "$F" "$X")
  & severity Suggestion
  & message "Consider point-free composition: \\x -> f (g x) = f . g"
  & note "Only use point-free when it improves readability"
  & category Style
  & safetyLevel MostlySafe

-- | Fuse fmap compositions using Functor laws.
--
-- == Problem
--
-- @
-- fmap f (fmap g xs)  -- Two traversals
-- @
--
-- == Solution
--
-- @
-- fmap (f . g) xs  -- Single traversal
-- @
--
-- This is the Functor composition law: fmap f . fmap g = fmap (f . g)
fmapFmapFusion :: Rule
fmapFmapFusion = rule "style/fmap-fusion" $
  match ("fmap $F $ fmap $G $X" ==> "fmap ($F . $G) $X")
  & severity Suggestion
  & message "Fuse fmap f . fmap g to fmap (f . g) using Functor laws"
  & note "fmap f (fmap g x) = fmap (f . g) x by Functor composition law"
  & category Style
  & safetyLevel Safe

-- | Simplify Applicative identity.
--
-- == Problem
--
-- @
-- pure id <*> x  -- Unnecessary lifting
-- @
--
-- == Solution
--
-- @
-- x  -- Applicative identity law
-- @
simplifyApplicativeId :: Rule
simplifyApplicativeId = rule "style/simplify-applicative-id" $
  match ("pure id <*> $X" ==> "$X")
  & severity Suggestion
  & message "pure id <*> x is just x (Applicative identity law)"
  & category Style
  & safetyLevel Safe

-- | Simplify maybe Just Nothing to id.
--
-- == Problem
--
-- @
-- maybe Nothing Just x  -- Just returns x wrapped/unwrapped
-- @
--
-- == Solution
--
-- @
-- x  -- It's already a Maybe!
-- @
maybeJustNothing :: Rule
maybeJustNothing = rule "style/maybe-just-nothing" $
  match ("maybe Nothing Just $X" ==> "$X")
  & severity Suggestion
  & message "maybe Nothing Just x is just x"
  & note "maybe Nothing Just is id for Maybe values"
  & category Style
  & safetyLevel Safe

-- | Simplify if-then-not patterns.
--
-- == Problem
--
-- @
-- if x then False else True
-- @
--
-- == Solution
--
-- @
-- not x
-- @
ifThenNot :: Rule
ifThenNot = rule "style/if-then-not" $
  match ("if $X then False else True" ==> "not $X")
  & severity Suggestion
  & message "if x then False else True is just 'not x'"
  & category Style
  & safetyLevel Safe

-- | Simplify double negation.
--
-- == Problem
--
-- @
-- not (not x)  -- Double negation
-- @
--
-- == Solution
--
-- @
-- x
-- @
notNot :: Rule
notNot = rule "style/not-not" $
  match ("not $ not $X" ==> "$X")
  & severity Suggestion
  & message "not (not x) is just x - double negation cancels"
  & category Style
  & safetyLevel Safe

-- | Simplify x && True.
--
-- @
-- x && True = x
-- @
andTrue :: Rule
andTrue = rule "style/and-true" $
  match ("$X && True" ==> "$X")
  & severity Suggestion
  & message "x && True is just x"
  & note "True is the identity for (&&)"
  & category Style
  & safetyLevel Safe

-- | Simplify x || False.
--
-- @
-- x || False = x
-- @
orFalse :: Rule
orFalse = rule "style/or-false" $
  match ("$X || False" ==> "$X")
  & severity Suggestion
  & message "x || False is just x"
  & note "False is the identity for (||)"
  & category Style
  & safetyLevel Safe

-- | Detect if with same branches.
--
-- == Problem
--
-- @
-- if condition then x else x  -- Both branches identical!
-- @
--
-- == Solution
--
-- @
-- x
-- @
ifSameBranches :: Rule
ifSameBranches = rule "style/if-same-branches" $
  match ("if $COND then $X else $X" ==> "$X")
  & severity Warning
  & message "Both if branches are identical - condition is irrelevant"
  & note "This may indicate copy-paste error or dead code"
  & category Style
  & safetyLevel Safe

-- | Detect comparing to self.
--
-- == Problem
--
-- @
-- x == x  -- Always True (for most types)
-- @
--
-- == Note
--
-- This is usually a bug, except for NaN detection (which doesn't
-- work this way in Haskell anyway due to IEEE semantics).
compareToSelf :: Rule
compareToSelf = rule "style/compare-to-self" $
  match ("$X == $X" ==> "True")
  & severity Warning
  & message "Comparing a value to itself is always True"
  & note "This is likely a bug - did you mean to compare different values?"
  & category Style
  & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Metadata
--------------------------------------------------------------------------------

-- | Total number of style rules.
styleRuleCount :: Int
styleRuleCount = length styleRules
