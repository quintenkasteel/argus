{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Brackets
-- Description : Bracket, parenthesis, and operator fixity rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for detecting redundant brackets, unnecessary dollar operators,
-- and operator precedence issues. These rules improve code readability
-- by removing visual noise while respecting Haskell's fixity rules.
--
-- == Rule Categories
--
-- * __Redundant Brackets__: Unnecessary parentheses around expressions
-- * __Dollar Operator__: Misuse or overuse of ($)
-- * __Operator Fixity__: Precedence and associativity issues
-- * __Section Syntax__: Operator section improvements
--
-- == References
--
-- * <https://www.haskell.org/onlinereport/decls.html#sect4.4.2 Haskell Fixity>
-- * <https://wiki.haskell.org/Operator_fixity Operator Fixity Wiki>

module Argus.Rules.Builtin.Brackets
  ( -- * Rule Sets
    bracketRules
  , redundantParenRules
  , dollarRules
  , fixityRules
  , sectionRules

    -- * Redundant Parentheses
  , redundantOuterParen
  , redundantInnerParen
  , redundantParenInApp
  , redundantParenAroundVar
  , redundantParenAroundLit
  , redundantParenInList
  , redundantParenInTuple
  , redundantParenInDo

    -- * Dollar Operator
  , dollarInsteadOfParen
  , redundantDollar
  , doubleDollar
  , dollarInApp
  , preferParenOverDollar

    -- * Operator Fixity
  , nonAssocOperatorChain
  , lowPrecedenceOp
  , highPrecedenceOp
  , mixedAssociativity
  , redundantParenInOp

    -- * Section Syntax
  , redundantSection
  , preferSection
  , flipSection
  , compositionSection

    -- * Rule Count
  , bracketRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All bracket and fixity related rules.
bracketRules :: [Rule]
bracketRules = mconcat
  [ redundantParenRules
  , dollarRules
  , fixityRules
  , sectionRules
  ]

-- | Total count of bracket rules.
bracketRuleCount :: Int
bracketRuleCount = length bracketRules

--------------------------------------------------------------------------------
-- Redundant Parentheses Rules
--------------------------------------------------------------------------------

-- | Rules for removing unnecessary parentheses.
redundantParenRules :: [Rule]
redundantParenRules =
  [ redundantOuterParen
  , redundantInnerParen
  , redundantParenInApp
  , redundantParenAroundVar
  , redundantParenAroundLit
  , redundantParenInList
  , redundantParenInTuple
  , redundantParenInDo
  ]

-- | Redundant outer parentheses around entire expression.
--
-- @
-- (f x)  ==>  f x
-- ((a + b))  ==>  a + b
-- @
redundantOuterParen :: Rule
redundantOuterParen =
  rule "redundant-outer-paren" $
    match ("((_x))" ==> "_x")
    & category Style
    & severity Suggestion
    & message "Redundant outer parentheses"
    & note "Parentheses around the entire expression are unnecessary"

-- | Redundant inner parentheses in nested parens.
--
-- @
-- f ((x))  ==>  f (x)
-- @
redundantInnerParen :: Rule
redundantInnerParen =
  rule "redundant-inner-paren" $
    match ("((_x))" ==> "(_x)")
    & category Style
    & severity Suggestion
    & message "Redundant nested parentheses"

-- | Redundant parentheses around function application.
--
-- @
-- (f x) y  ==>  f x y
-- g (f x)  ==>  g (f x)  -- This one is needed, so we're careful
-- @
redundantParenInApp :: Rule
redundantParenInApp =
  rule "redundant-paren-in-app" $
    match ("(f) _x" ==> "f _x")
    & category Style
    & severity Suggestion
    & message "Redundant parentheses around function"

-- | Redundant parentheses around a simple variable.
--
-- @
-- f (x)  ==>  f x
-- (x) + y  ==>  x + y
-- @
redundantParenAroundVar :: Rule
redundantParenAroundVar =
  rule "redundant-paren-around-var" $
    match (("(_x)" ==> "_x") `where_` isVariable "_x")
    & category Style
    & severity Suggestion
    & message "Redundant parentheses around variable"

-- | Redundant parentheses around a literal.
--
-- @
-- f (1)  ==>  f 1
-- (True) && x  ==>  True && x
-- @
redundantParenAroundLit :: Rule
redundantParenAroundLit =
  rule "redundant-paren-around-lit" $
    match (("(_x)" ==> "_x") `where_` isLiteral "_x")
    & category Style
    & severity Suggestion
    & message "Redundant parentheses around literal"

-- | Redundant parentheses inside list literals.
--
-- @
-- [(x), (y)]  ==>  [x, y]
-- @
redundantParenInList :: Rule
redundantParenInList =
  rule "redundant-paren-in-list" $
    match ("[(_x)]" ==> "[_x]")
    & category Style
    & severity Suggestion
    & message "Redundant parentheses inside list"

-- | Redundant parentheses inside tuple.
--
-- @
-- ((x), y)  ==>  (x, y)
-- @
redundantParenInTuple :: Rule
redundantParenInTuple =
  rule "redundant-paren-in-tuple" $
    match ("((_x), _y)" ==> "(_x, _y)")
    & category Style
    & severity Suggestion
    & message "Redundant parentheses inside tuple"

-- | Redundant parentheses in do-notation.
--
-- @
-- do { (x) <- action; ... }  ==>  do { x <- action; ... }
-- @
redundantParenInDo :: Rule
redundantParenInDo =
  rule "redundant-paren-in-do" $
    match ("do { (_x) }" ==> "do { _x }")
    & category Style
    & severity Suggestion
    & message "Redundant parentheses in do-notation"

--------------------------------------------------------------------------------
-- Dollar Operator Rules
--------------------------------------------------------------------------------

-- | Rules for proper dollar operator usage.
dollarRules :: [Rule]
dollarRules =
  [ dollarInsteadOfParen
  , redundantDollar
  , doubleDollar
  , dollarInApp
  , preferParenOverDollar
  ]

-- | Suggest dollar instead of parentheses for trailing argument.
--
-- @
-- f (g (h x))  ==>  f $ g $ h x
-- putStrLn (show x)  ==>  putStrLn $ show x
-- @
dollarInsteadOfParen :: Rule
dollarInsteadOfParen =
  rule "dollar-instead-of-paren" $
    match ("_f (_g _x)" ==> "_f $ _g _x")
    & category Style
    & severity Info
    & message "Consider using ($) instead of parentheses"
    & note "Dollar operator can reduce nesting for single trailing arguments"
    & safetyLevel MostlySafe

-- | Redundant dollar at end of expression.
--
-- @
-- f $ x  ==>  f x  (when x is simple)
-- @
redundantDollar :: Rule
redundantDollar =
  rule "redundant-dollar" $
    match (("_f $ _x" ==> "_f _x") `where_` isAtomic "_x")
    & category Style
    & severity Suggestion
    & message "Redundant ($) - argument is atomic"
    & note "Dollar is unnecessary when the argument doesn't need grouping"

-- | Double dollar is redundant.
--
-- @
-- f $ $ x  ==>  f $ x
-- @
doubleDollar :: Rule
doubleDollar =
  rule "double-dollar" $
    match ("_f $ $ _x" ==> "_f $ _x")
    & category Style
    & severity Warning
    & message "Double ($) is redundant"

-- | Dollar used inside function application.
--
-- @
-- (f $ x) y  -- Probably wrong, $ has low precedence
-- @
dollarInApp :: Rule
dollarInApp =
  rule "dollar-in-app" $
    match ("(_f $ _x) _y" ==> "_f (_x _y)")
    & category Correctness
    & severity Warning
    & message "($) inside function application - check precedence"
    & note "($) has very low precedence (0), this may not parse as expected"

-- | Prefer parentheses for readability in some contexts.
--
-- @
-- f $ g $ h $ i $ x  -- Too many dollars, use parens
-- @
preferParenOverDollar :: Rule
preferParenOverDollar =
  rule "prefer-paren-over-dollar" $
    match ("_f $ _g $ _h $ _i $ _x" ==> "_f (_g (_h (_i _x)))")
    & category Style
    & severity Info
    & message "Consider parentheses for deeply nested ($)"
    & note "More than 3 nested ($) operators can harm readability"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Operator Fixity Rules
--------------------------------------------------------------------------------

-- | Rules for operator precedence and associativity.
fixityRules :: [Rule]
fixityRules =
  [ nonAssocOperatorChain
  , lowPrecedenceOp
  , highPrecedenceOp
  , mixedAssociativity
  , redundantParenInOp
  ]

-- | Chain of non-associative operators.
--
-- @
-- x == y == z  -- ERROR: (==) is non-associative
-- @
nonAssocOperatorChain :: Rule
nonAssocOperatorChain =
  rule "non-assoc-operator-chain" $
    match ("_x == _y == _z" ==> "_x == _y && _y == _z")
    & category Correctness
    & severity Error
    & message "Chaining non-associative operator (==)"
    & note "(==) is non-associative. Use (&&) to combine comparisons."

-- | Warn about low precedence custom operators.
--
-- @
-- x `op` y + z  -- If op has low precedence, this may surprise
-- @
lowPrecedenceOp :: Rule
lowPrecedenceOp =
  rule "low-precedence-op" $
    match ("_x `seq` _y + _z" ==> "_x `seq` (_y + _z)")
    & category Style
    & severity Info
    & message "seq has low precedence - consider explicit parentheses"
    & note "seq has precedence 0, arithmetic has precedence 6-7"

-- | Warn about high precedence function application.
--
-- @
-- f . g x  ==>  f . (g x)  -- Watch out: (.) binds tighter than application
-- @
highPrecedenceOp :: Rule
highPrecedenceOp =
  rule "high-precedence-op" $
    match ("_f . _g _x" ==> "(_f . _g) _x")
    & category Correctness
    & severity Warning
    & message "Composition (.) binds tighter than application"
    & note "f . g x means f . (g x), not (f . g) x"
    & safetyLevel ManualReview

-- | Mixed associativity warning.
--
-- @
-- x >> y >>= f  -- (>>) and (>>=) have same precedence but different assoc
-- @
mixedAssociativity :: Rule
mixedAssociativity =
  rule "mixed-associativity" $
    match ("_x >> _y >>= _f" ==> "(_x >> _y) >>= _f")
    & category Style
    & severity Info
    & message "Mixing (>>) and (>>=) - consider explicit grouping"
    & note "Both have precedence 1, but different associativity"

-- | Redundant parentheses around operator with higher precedence.
--
-- @
-- (a * b) + c  ==>  a * b + c  -- (*) has higher precedence than (+)
-- @
redundantParenInOp :: Rule
redundantParenInOp =
  rule "redundant-paren-in-op" $
    match ("(_x * _y) + _z" ==> "_x * _y + _z")
    & category Style
    & severity Suggestion
    & message "Redundant parentheses - (*) has higher precedence than (+)"

--------------------------------------------------------------------------------
-- Section Syntax Rules
--------------------------------------------------------------------------------

-- | Rules for operator section improvements.
sectionRules :: [Rule]
sectionRules =
  [ redundantSection
  , preferSection
  , flipSection
  , compositionSection
  ]

-- | Redundant section syntax.
--
-- @
-- ($ x)  ==>  ($ x)  -- OK
-- (($) f)  ==>  f  -- Redundant
-- @
redundantSection :: Rule
redundantSection =
  rule "redundant-section" $
    match ("(($) _f)" ==> "_f")
    & category Style
    & severity Suggestion
    & message "Redundant section syntax"

-- | Prefer section over lambda.
--
-- @
-- \\x -> x + 1  ==>  (+ 1)
-- \\x -> 1 + x  ==>  (1 +)
-- @
preferSection :: Rule
preferSection =
  rule "prefer-section" $
    match ("\\x -> x + _n" ==> "(+ _n)")
    & category Style
    & severity Suggestion
    & message "Use operator section instead of lambda"
    & note "Sections are more idiomatic for simple operations"

-- | Use flip for right section.
--
-- @
-- \\x -> _n - x  ==>  subtract _n  (or flip (-) _n)
-- @
flipSection :: Rule
flipSection =
  rule "flip-section" $
    match ("\\x -> _n - x" ==> "subtract _n")
    & category Style
    & severity Suggestion
    & message "Use subtract for right subtraction section"
    & note "(-) as a section means negate, use subtract instead"

-- | Composition section.
--
-- @
-- \\x -> f (g x)  ==>  f . g
-- @
compositionSection :: Rule
compositionSection =
  rule "composition-section" $
    match ("\\x -> _f (_g x)" ==> "_f . _g")
    & category Style
    & severity Suggestion
    & message "Use function composition"
    & note "Point-free style using (.) is more idiomatic"
