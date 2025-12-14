{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Boolean
-- Description : Boolean operation rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Comprehensive rules for Boolean operations and simplifications.

module Argus.Rules.Builtin.Boolean
  ( -- * Rule Sets
    boolSimplifyRules
  , basicBoolRules
  , boolComparisonRules
  , ifThenElseRules

    -- * Basic Boolean
  , notTrue
  , notFalse
  , trueAnd
  , falseAnd
  , andFalse
  , trueOr
  , falseOr
  , orTrue
  , andSame
  , orSame
  , notAndNot
  , notOrNot

    -- * Boolean Comparisons
  , eqTrue
  , eqFalse
  , trueEq
  , falseEq
  , neTrue
  , neFalse
  , andNotSelf
  , orNotSelf
  , notAndSelf
  , notOrSelf

    -- * If-then-else
  , ifTrue
  , ifFalse
  , ifCondTrue
  , ifCondFalse
  , ifSameBranch
  , ifThenTrue
  , ifThenFalse
  , ifElseTrue
  , ifElseFalse
  , ifNot
  , ifThenElseSame
  , ifJustNothing
  , ifNothingJust
  , ifLeftRight
  , boolFunction

    -- * Rule Count
  , booleanRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All boolean-related rules.
boolSimplifyRules :: [Rule]
boolSimplifyRules = mconcat
  [ basicBoolRules
  , boolComparisonRules
  , ifThenElseRules
  ]

-- | Total count of boolean rules.
booleanRuleCount :: Int
booleanRuleCount = length boolSimplifyRules

--------------------------------------------------------------------------------
-- Basic Boolean Rules
--------------------------------------------------------------------------------

-- | Rules for basic boolean operations.
basicBoolRules :: [Rule]
basicBoolRules =
  [ notTrue
  , notFalse
  , trueAnd
  , falseAnd
  , andFalse
  , trueOr
  , falseOr
  , orTrue
  , andSame
  , orSame
  , notAndNot
  , notOrNot
  ]

-- | not True ==> False
notTrue :: Rule
notTrue =
  rule "not-true" $
    match ("not True" ==> "False")
    & category Style
    & severity Suggestion
    & message "not True is False"

-- | not False ==> True
notFalse :: Rule
notFalse =
  rule "not-false" $
    match ("not False" ==> "True")
    & category Style
    & severity Suggestion
    & message "not False is True"

-- | True && x ==> x
trueAnd :: Rule
trueAnd =
  rule "true-and" $
    match ("True && _x" ==> "_x")
    & category Style
    & severity Suggestion
    & message "True && x is x"

-- | False && x ==> False
falseAnd :: Rule
falseAnd =
  rule "false-and" $
    match ("False && _x" ==> "False")
    & category Style
    & severity Suggestion
    & message "False && x is False"

-- | x && False ==> False
andFalse :: Rule
andFalse =
  rule "and-false" $
    match ("_x && False" ==> "False")
    & category Style
    & severity Suggestion
    & message "x && False is False"

-- | True || x ==> True
trueOr :: Rule
trueOr =
  rule "true-or" $
    match ("True || _x" ==> "True")
    & category Style
    & severity Suggestion
    & message "True || x is True"

-- | False || x ==> x
falseOr :: Rule
falseOr =
  rule "false-or" $
    match ("False || _x" ==> "_x")
    & category Style
    & severity Suggestion
    & message "False || x is x"

-- | x || True ==> True
orTrue :: Rule
orTrue =
  rule "or-true" $
    match ("_x || True" ==> "True")
    & category Style
    & severity Suggestion
    & message "x || True is True"

-- | x && x ==> x
andSame :: Rule
andSame =
  rule "and-same" $
    matchText "([a-z_][a-z0-9_]*)\\s*&&\\s*\\1\\b"
    & category Style
    & severity Suggestion
    & message "x && x is x"

-- | x || x ==> x
orSame :: Rule
orSame =
  rule "or-same" $
    matchText "([a-z_][a-z0-9_]*)\\s*\\|\\|\\s*\\1\\b"
    & category Style
    & severity Suggestion
    & message "x || x is x"

-- | not x && not y ==> not (x || y)
notAndNot :: Rule
notAndNot =
  rule "not-and-not" $
    match ("not _x && not _y" ==> "not (_x || _y)")
    & category Style
    & severity Suggestion
    & message "De Morgan: not x && not y is not (x || y)"

-- | not x || not y ==> not (x && y)
notOrNot :: Rule
notOrNot =
  rule "not-or-not" $
    match ("not _x || not _y" ==> "not (_x && _y)")
    & category Style
    & severity Suggestion
    & message "De Morgan: not x || not y is not (x && y)"

--------------------------------------------------------------------------------
-- Boolean Comparison Rules
--------------------------------------------------------------------------------

-- | Rules for boolean comparisons.
boolComparisonRules :: [Rule]
boolComparisonRules =
  [ eqTrue
  , eqFalse
  , trueEq
  , falseEq
  , neTrue
  , neFalse
  , andNotSelf
  , orNotSelf
  , notAndSelf
  , notOrSelf
  ]

-- | x == True ==> x
eqTrue :: Rule
eqTrue =
  rule "eq-true" $
    match ("_x == True" ==> "_x")
    & category Style
    & severity Suggestion
    & message "x == True is just x"

-- | x == False ==> not x
eqFalse :: Rule
eqFalse =
  rule "eq-false" $
    match ("_x == False" ==> "not _x")
    & category Style
    & severity Suggestion
    & message "x == False is not x"

-- | True == x ==> x
trueEq :: Rule
trueEq =
  rule "true-eq" $
    match ("True == _x" ==> "_x")
    & category Style
    & severity Suggestion
    & message "True == x is x"

-- | False == x ==> not x
falseEq :: Rule
falseEq =
  rule "false-eq" $
    match ("False == _x" ==> "not _x")
    & category Style
    & severity Suggestion
    & message "False == x is not x"

-- | x /= True ==> not x
neTrue :: Rule
neTrue =
  rule "ne-true" $
    match ("_x /= True" ==> "not _x")
    & category Style
    & severity Suggestion
    & message "x /= True is not x"

-- | x /= False ==> x
neFalse :: Rule
neFalse =
  rule "ne-false" $
    match ("_x /= False" ==> "_x")
    & category Style
    & severity Suggestion
    & message "x /= False is x"

-- | x && not x ==> False
andNotSelf :: Rule
andNotSelf =
  rule "and-not-self" $
    matchText "([a-z_][a-z0-9_]*)\\s*&&\\s*not\\s+\\1\\b"
    & category Style
    & severity Suggestion
    & message "x && not x is False"

-- | x || not x ==> True
orNotSelf :: Rule
orNotSelf =
  rule "or-not-self" $
    matchText "([a-z_][a-z0-9_]*)\\s*\\|\\|\\s*not\\s+\\1\\b"
    & category Style
    & severity Suggestion
    & message "x || not x is True"

-- | not x && x ==> False
notAndSelf :: Rule
notAndSelf =
  rule "not-and-self" $
    matchText "not\\s+([a-z_][a-z0-9_]*)\\s*&&\\s*\\1\\b"
    & category Style
    & severity Suggestion
    & message "not x && x is False"

-- | not x || x ==> True
notOrSelf :: Rule
notOrSelf =
  rule "not-or-self" $
    matchText "not\\s+([a-z_][a-z0-9_]*)\\s*\\|\\|\\s*\\1\\b"
    & category Style
    & severity Suggestion
    & message "not x || x is True"

--------------------------------------------------------------------------------
-- If-then-else Rules
--------------------------------------------------------------------------------

-- | Rules for if-then-else.
ifThenElseRules :: [Rule]
ifThenElseRules =
  [ ifTrue
  , ifFalse
  , ifCondTrue
  , ifCondFalse
  , ifSameBranch
  , ifThenTrue
  , ifThenFalse
  , ifElseTrue
  , ifElseFalse
  , ifNot
  , ifThenElseSame
  , ifJustNothing
  , ifNothingJust
  , ifLeftRight
  , boolFunction
  ]

-- | if True then x else y ==> x
ifTrue :: Rule
ifTrue =
  rule "if-true" $
    match ("if True then _x else _y" ==> "_x")
    & category Style
    & severity Suggestion
    & message "if True always takes then branch"

-- | if False then x else y ==> y
ifFalse :: Rule
ifFalse =
  rule "if-false" $
    match ("if False then _x else _y" ==> "_y")
    & category Style
    & severity Suggestion
    & message "if False always takes else branch"

-- | if c then True else False ==> c
ifCondTrue :: Rule
ifCondTrue =
  rule "if-cond-true" $
    match ("if _c then True else False" ==> "_c")
    & category Style
    & severity Suggestion
    & message "if c then True else False is just c"

-- | if c then False else True ==> not c
ifCondFalse :: Rule
ifCondFalse =
  rule "if-cond-false" $
    match ("if _c then False else True" ==> "not _c")
    & category Style
    & severity Suggestion
    & message "if c then False else True is not c"

-- | if c then x else x ==> x
ifSameBranch :: Rule
ifSameBranch =
  rule "if-same-branch" $
    matchText "if\\s+[^t]+then\\s+([a-z_]+)\\s+else\\s+\\1\\b"
    & category Style
    & severity Suggestion
    & message "if with same branches can be simplified"

-- | if c then True else x ==> c || x
ifThenTrue :: Rule
ifThenTrue =
  rule "if-then-true" $
    match ("if _c then True else _x" ==> "_c || _x")
    & category Style
    & severity Suggestion
    & message "if c then True else x is c || x"

-- | if c then False else x ==> not c && x
ifThenFalse :: Rule
ifThenFalse =
  rule "if-then-false" $
    match ("if _c then False else _x" ==> "not _c && _x")
    & category Style
    & severity Suggestion
    & message "if c then False else x is not c && x"

-- | if c then x else True ==> not c || x
ifElseTrue :: Rule
ifElseTrue =
  rule "if-else-true" $
    match ("if _c then _x else True" ==> "not _c || _x")
    & category Style
    & severity Suggestion
    & message "if c then x else True is not c || x"

-- | if c then x else False ==> c && x
ifElseFalse :: Rule
ifElseFalse =
  rule "if-else-false" $
    match ("if _c then _x else False" ==> "_c && _x")
    & category Style
    & severity Suggestion
    & message "if c then x else False is c && x"

-- | if not c then x else y ==> if c then y else x
ifNot :: Rule
ifNot =
  rule "if-not" $
    match ("if not _c then _x else _y" ==> "if _c then _y else _x")
    & category Style
    & severity Suggestion
    & message "Avoid negation in if condition"

-- | if c then f x else f y ==> f (if c then x else y)
ifThenElseSame :: Rule
ifThenElseSame =
  rule "if-then-else-same" $
    matchText "if\\s+[^t]+then\\s+([a-z_]+)\\s+[^e]+else\\s+\\1\\s"
    & category Style
    & severity Info
    & message "Same function in both branches - consider factoring out"
    & safetyLevel ManualReview

-- | if c then Just x else Nothing
ifJustNothing :: Rule
ifJustNothing =
  rule "if-just-nothing" $
    matchText "if\\s+[^t]+then\\s+Just\\s+[^e]+else\\s+Nothing"
    & category Style
    & severity Info
    & message "if with Just/Nothing - consider guard or mfilter"
    & safetyLevel ManualReview

-- | if c then Nothing else Just x
ifNothingJust :: Rule
ifNothingJust =
  rule "if-nothing-just" $
    matchText "if\\s+[^t]+then\\s+Nothing\\s+else\\s+Just"
    & category Style
    & severity Info
    & message "if with Nothing/Just - consider guard or mfilter"
    & safetyLevel ManualReview

-- | if c then Left x else Right y
ifLeftRight :: Rule
ifLeftRight =
  rule "if-left-right" $
    matchText "if\\s+[^t]+then\\s+Left\\s+[^e]+else\\s+Right"
    & category Style
    & severity Info
    & message "if with Left/Right - consider bool or Either combinators"
    & safetyLevel ManualReview

-- | bool x y c ==> if c then y else x
boolFunction :: Rule
boolFunction =
  rule "bool-function" $
    match ("bool _x _y _c" ==> "bool _x _y _c")
    & category Style
    & severity Info
    & message "bool x y c is if c then y else x"
    & safetyLevel ManualReview
