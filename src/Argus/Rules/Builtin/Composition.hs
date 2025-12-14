{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Composition
-- Description : Function composition rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for function composition, application, and lambda simplifications.

module Argus.Rules.Builtin.Composition
  ( -- * Rule Sets
    compositionRules
  , composeRules
  , applicationRules
  , lambdaSimplifyRules

    -- * Composition
  , composeId
  , idCompose
  , composeConst
  , constCompose
  , composeApply
  , flipConst
  , flipConstAlt
  , flipFlip
  , flipDollar
  , flipDot

    -- * Application
  , dollarApply
  , dollarSimple
  , idApply
  , constApply
  , fixConst
  , fixNonRec
  , onApply
  , onEq
  , onCompareF
  , flipOn

    -- * Lambda Simplifications
  , lambdaId
  , lambdaConst
  , lambdaEta
  , lambdaFlip
  , lambdaCompose
  , lambdaSwap
  , lambdaFst
  , lambdaSnd
  , lambdaDup
  , lambdaDollar

    -- * Rule Count
  , compositionRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All composition-related rules.
compositionRules :: [Rule]
compositionRules = mconcat
  [ composeRules
  , applicationRules
  , lambdaSimplifyRules
  ]

-- | Total count of composition rules.
compositionRuleCount :: Int
compositionRuleCount = length compositionRules

--------------------------------------------------------------------------------
-- Composition Rules
--------------------------------------------------------------------------------

-- | Rules for function composition.
composeRules :: [Rule]
composeRules =
  [ composeId
  , idCompose
  , composeConst
  , constCompose
  , composeApply
  , flipConst
  , flipConstAlt
  , flipFlip
  , flipDollar
  , flipDot
  ]

-- | f . id ==> f
composeId :: Rule
composeId =
  rule "compose-id" $
    match ("_f . id" ==> "_f")
    & category Style
    & severity Suggestion
    & message "f . id is f"

-- | id . f ==> f
idCompose :: Rule
idCompose =
  rule "id-compose" $
    match ("id . _f" ==> "_f")
    & category Style
    & severity Suggestion
    & message "id . f is f"

-- | f . const x ==> const (f x)
composeConst :: Rule
composeConst =
  rule "compose-const" $
    match ("_f . const _x" ==> "const (_f _x)")
    & category Style
    & severity Suggestion
    & message "f . const x is const (f x)"

-- | const x . f ==> const x
constCompose :: Rule
constCompose =
  rule "const-compose" $
    match ("const _x . _f" ==> "const _x")
    & category Style
    & severity Suggestion
    & message "const x . f is const x"

-- | (f . g) x ==> f (g x)
composeApply :: Rule
composeApply =
  rule "compose-apply" $
    match ("(_f . _g) _x" ==> "_f (_g _x)")
    & category Style
    & severity Info
    & message "(f . g) x is f (g x)"
    & safetyLevel ManualReview

-- | flip const x y ==> y
flipConst :: Rule
flipConst =
  rule "flip-const" $
    match ("flip const _x _y" ==> "_y")
    & category Style
    & severity Suggestion
    & message "flip const x y is y"

-- | flip const ==> const id
flipConstAlt :: Rule
flipConstAlt =
  rule "flip-const-alt" $
    match ("flip const" ==> "const id")
    & category Style
    & severity Suggestion
    & message "flip const is const id"

-- | flip flip x f ==> f x
flipFlip :: Rule
flipFlip =
  rule "flip-flip" $
    match ("flip flip _x _f" ==> "_f _x")
    & category Style
    & severity Suggestion
    & message "flip flip x f is f x"

-- | flip ($) ==> (&)
flipDollar :: Rule
flipDollar =
  rule "flip-dollar" $
    match ("flip ($)" ==> "(&)")
    & category Style
    & severity Suggestion
    & message "flip ($) is (&)"

-- | flip (.) pattern.
flipDot :: Rule
flipDot =
  rule "flip-dot" $
    match ("flip (.)" ==> "flip (.)")
    & category Style
    & severity Info
    & message "flip (.) swaps composition order"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Application Rules
--------------------------------------------------------------------------------

-- | Rules for function application.
applicationRules :: [Rule]
applicationRules =
  [ dollarApply
  , dollarSimple
  , idApply
  , constApply
  , fixConst
  , fixNonRec
  , onApply
  , onEq
  , onCompareF
  , flipOn
  ]

-- | ($) f x ==> f x
dollarApply :: Rule
dollarApply =
  rule "dollar-apply" $
    match ("($) _f _x" ==> "_f _x")
    & category Style
    & severity Suggestion
    & message "($) f x is f x"

-- | f $ x in simple cases.
dollarSimple :: Rule
dollarSimple =
  rule "dollar-simple" $
    matchText "\\$\\s*[a-z_][a-z0-9_]*\\s*$"
    & category Style
    & severity Info
    & message "$ with simple argument can be parentheses"
    & safetyLevel ManualReview

-- | id x ==> x
idApply :: Rule
idApply =
  rule "id-apply" $
    match ("id _x" ==> "_x")
    & category Style
    & severity Suggestion
    & message "id x is x"

-- | const x y ==> x
constApply :: Rule
constApply =
  rule "const-apply" $
    match ("const _x _y" ==> "_x")
    & category Style
    & severity Suggestion
    & message "const x y is x"

-- | fix (const x) ==> x
fixConst :: Rule
fixConst =
  rule "fix-const" $
    match ("fix (const _x)" ==> "_x")
    & category Style
    & severity Suggestion
    & message "fix (const x) is x"

-- | fix (\f -> x) when non-recursive.
fixNonRec :: Rule
fixNonRec =
  rule "fix-non-rec" $
    matchText "fix\\s*\\(\\s*\\\\\\s*_\\s*->"
    & category Style
    & severity Info
    & message "fix with unused argument - consider simplifying"
    & safetyLevel ManualReview

-- | on f g x y ==> f (g x) (g y)
onApply :: Rule
onApply =
  rule "on-apply" $
    match ("on _f _g _x _y" ==> "_f (_g _x) (_g _y)")
    & category Style
    & severity Info
    & message "on f g x y is f (g x) (g y)"
    & safetyLevel ManualReview

-- | on (==) f x y ==> f x == f y
onEq :: Rule
onEq =
  rule "on-eq" $
    match ("on (==) _f _x _y" ==> "_f _x == _f _y")
    & category Style
    & severity Suggestion
    & message "on (==) f x y is f x == f y"

-- | on compare f ==> comparing f
onCompareF :: Rule
onCompareF =
  rule "on-compare-f" $
    match ("on compare _f" ==> "comparing _f")
    & category Style
    & severity Suggestion
    & message "on compare f is comparing f"

-- | flip on f g ==> on (flip f) g
flipOn :: Rule
flipOn =
  rule "flip-on" $
    match ("flip on _f _g" ==> "on (flip _f) _g")
    & category Style
    & severity Info
    & message "flip on f g is on (flip f) g"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Lambda Simplification Rules
--------------------------------------------------------------------------------

-- | Rules for lambda simplifications.
lambdaSimplifyRules :: [Rule]
lambdaSimplifyRules =
  [ lambdaId
  , lambdaConst
  , lambdaEta
  , lambdaFlip
  , lambdaCompose
  , lambdaSwap
  , lambdaFst
  , lambdaSnd
  , lambdaDup
  , lambdaDollar
  ]

-- | \x -> x ==> id
lambdaId :: Rule
lambdaId =
  rule "lambda-id" $
    matchText "\\\\\\s*([a-z_]+)\\s*->\\s*\\1\\s*$"
    & category Style
    & severity Suggestion
    & message "\\x -> x is id"

-- | \x -> y ==> const y (when x not free in y)
lambdaConst :: Rule
lambdaConst =
  rule "lambda-const" $
    matchText "\\\\\\s*_\\s*->"
    & category Style
    & severity Suggestion
    & message "\\_ -> y is const y"

-- | \x -> f x ==> f (eta reduction)
lambdaEta :: Rule
lambdaEta =
  rule "lambda-eta" $
    matchText "\\\\\\s*([a-z_]+)\\s*->\\s*[a-z_]+\\s+\\1\\s*$"
    & category Style
    & severity Info
    & message "Consider eta reduction: \\x -> f x is f"
    & safetyLevel ManualReview

-- | \x y -> f y x ==> flip f
lambdaFlip :: Rule
lambdaFlip =
  rule "lambda-flip" $
    matchText "\\\\\\s*([a-z_]+)\\s+([a-z_]+)\\s*->\\s*[a-z_]+\\s+\\2\\s+\\1"
    & category Style
    & severity Suggestion
    & message "\\x y -> f y x is flip f"

-- | \x -> f (g x) ==> f . g
lambdaCompose :: Rule
lambdaCompose =
  rule "lambda-compose" $
    matchText "\\\\\\s*([a-z_]+)\\s*->\\s*[a-z_]+\\s*\\(\\s*[a-z_]+\\s+\\1\\s*\\)"
    & category Style
    & severity Suggestion
    & message "\\x -> f (g x) is f . g"

-- | \(x, y) -> (y, x) ==> swap
lambdaSwap :: Rule
lambdaSwap =
  rule "lambda-swap" $
    matchText "\\\\\\s*\\(\\s*([a-z_]+)\\s*,\\s*([a-z_]+)\\s*\\)\\s*->\\s*\\(\\s*\\2\\s*,\\s*\\1\\s*\\)"
    & category Style
    & severity Suggestion
    & message "\\(x, y) -> (y, x) is swap"

-- | \(x, y) -> x ==> fst
lambdaFst :: Rule
lambdaFst =
  rule "lambda-fst" $
    matchText "\\\\\\s*\\(\\s*([a-z_]+)\\s*,\\s*[a-z_]+\\s*\\)\\s*->\\s*\\1\\s*$"
    & category Style
    & severity Suggestion
    & message "\\(x, y) -> x is fst"

-- | \(x, y) -> y ==> snd
lambdaSnd :: Rule
lambdaSnd =
  rule "lambda-snd" $
    matchText "\\\\\\s*\\(\\s*[a-z_]+\\s*,\\s*([a-z_]+)\\s*\\)\\s*->\\s*\\1\\s*$"
    & category Style
    & severity Suggestion
    & message "\\(x, y) -> y is snd"

-- | \x -> (x, x) ==> join (,)
lambdaDup :: Rule
lambdaDup =
  rule "lambda-dup" $
    matchText "\\\\\\s*([a-z_]+)\\s*->\\s*\\(\\s*\\1\\s*,\\s*\\1\\s*\\)"
    & category Style
    & severity Suggestion
    & message "\\x -> (x, x) is join (,)"

-- | \f -> f x ==> ($ x)
lambdaDollar :: Rule
lambdaDollar =
  rule "lambda-dollar" $
    matchText "\\\\\\s*([a-z_]+)\\s*->\\s*\\1\\s+[a-z_]+"
    & category Style
    & severity Info
    & message "\\f -> f x is ($ x)"
    & safetyLevel ManualReview
