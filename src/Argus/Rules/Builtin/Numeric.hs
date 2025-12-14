{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Numeric
-- Description : Numeric computation optimization rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for numeric computation patterns, avoiding common pitfalls
-- and suggesting more efficient alternatives.
--
-- == Rule Categories
--
-- * __Arithmetic__: Arithmetic operation simplification
-- * __Comparison__: Numeric comparison patterns
-- * __Conversion__: Type conversion rules
-- * __Precision__: Floating point precision issues

module Argus.Rules.Builtin.Numeric
  ( -- * Rule Sets
    numericRules
  , arithmeticRules
  , comparisonRules
  , numericConversionRules
  , precisionRules

    -- * Arithmetic
  , addZero
  , subtractZero
  , multiplyOne
  , multiplyZero
  , divideOne
  , doubleNegate
  , subtractSelf
  , absNegate
  , negateAbs
  , powerOne
  , powerZero

    -- * Comparison
  , compareToZero
  , signumCompare
  , absCompare
  , equalityToCompare
  , inequalityChain
  , compareOrdering

    -- * Conversion
  , fromIntegralId
  , realToFracId
  , truncateFloor
  , ceilingRound
  , toIntegerFromIntegral
  , doubleToRational

    -- * Precision
  , floatEquality
  , divisionPrecision
  , numericInfiniteRecursion
  , overflowWarning
  , underflowWarning

    -- * Rule Count
  , numericRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All numeric rules.
numericRules :: [Rule]
numericRules = mconcat
  [ arithmeticRules
  , comparisonRules
  , numericConversionRules
  , precisionRules
  ]

-- | Total count of numeric rules.
numericRuleCount :: Int
numericRuleCount = length numericRules

--------------------------------------------------------------------------------
-- Arithmetic Rules
--------------------------------------------------------------------------------

-- | Rules for arithmetic simplification.
arithmeticRules :: [Rule]
arithmeticRules =
  [ addZero
  , subtractZero
  , multiplyOne
  , multiplyZero
  , divideOne
  , doubleNegate
  , subtractSelf
  , absNegate
  , negateAbs
  , powerOne
  , powerZero
  ]

-- | Adding zero is identity.
--
-- @
-- x + 0  ==>  x
-- 0 + x  ==>  x
-- @
addZero :: Rule
addZero =
  rule "add-zero" $
    match ("_x + 0" ==> "_x")
    & category Style
    & severity Suggestion
    & message "Adding zero is redundant"

-- | Subtracting zero is identity.
--
-- @
-- x - 0  ==>  x
-- @
subtractZero :: Rule
subtractZero =
  rule "subtract-zero" $
    match ("_x - 0" ==> "_x")
    & category Style
    & severity Suggestion
    & message "Subtracting zero is redundant"

-- | Multiplying by one is identity.
--
-- @
-- x * 1  ==>  x
-- 1 * x  ==>  x
-- @
multiplyOne :: Rule
multiplyOne =
  rule "multiply-one" $
    match ("_x * 1" ==> "_x")
    & category Style
    & severity Suggestion
    & message "Multiplying by one is redundant"

-- | Multiplying by zero is zero.
--
-- @
-- x * 0  ==>  0
-- 0 * x  ==>  0
-- @
multiplyZero :: Rule
multiplyZero =
  rule "multiply-zero" $
    match ("_x * 0" ==> "0")
    & category Style
    & severity Warning
    & message "Multiplying by zero always yields zero"
    & note "If side effects matter, this simplification may not apply"
    & safetyLevel ManualReview

-- | Dividing by one is identity.
--
-- @
-- x / 1  ==>  x
-- @
divideOne :: Rule
divideOne =
  rule "divide-one" $
    match ("_x / 1" ==> "_x")
    & category Style
    & severity Suggestion
    & message "Dividing by one is redundant"

-- | Double negation cancels.
--
-- @
-- negate (negate x)  ==>  x
-- - (- x)  ==>  x
-- @
doubleNegate :: Rule
doubleNegate =
  rule "double-negate" $
    match ("negate (negate _x)" ==> "_x")
    & category Style
    & severity Suggestion
    & message "Double negation cancels out"

-- | Subtracting from self is zero.
--
-- @
-- x - x  ==>  0
-- @
subtractSelf :: Rule
subtractSelf =
  rule "subtract-self" $
    match ("_x - _x" ==> "0")
    & category Style
    & severity Suggestion
    & message "Subtracting a value from itself is always 0"

-- | abs of negate is abs.
--
-- @
-- abs (negate x)  ==>  abs x
-- @
absNegate :: Rule
absNegate =
  rule "abs-negate" $
    match ("abs (negate _x)" ==> "abs _x")
    & category Style
    & severity Suggestion
    & message "abs (negate x) is the same as abs x"

-- | negate of abs is negate abs.
--
-- @
-- negate (abs x)  -- Same as - (abs x)
-- @
negateAbs :: Rule
negateAbs =
  rule "negate-abs" $
    match ("negate (abs _x)" ==> "- abs _x")
    & category Style
    & severity Info
    & message "negate (abs x) can be written as - abs x"

-- | Raising to power 1 is identity.
--
-- @
-- x ^ 1  ==>  x
-- x ** 1  ==>  x
-- @
powerOne :: Rule
powerOne =
  rule "power-one" $
    match ("_x ^ 1" ==> "_x")
    & category Style
    & severity Suggestion
    & message "Raising to power 1 is redundant"

-- | Raising to power 0 is 1.
--
-- @
-- x ^ 0  ==>  1
-- x ** 0  ==>  1
-- @
powerZero :: Rule
powerZero =
  rule "power-zero" $
    match ("_x ^ 0" ==> "1")
    & category Style
    & severity Suggestion
    & message "Raising to power 0 is always 1"
    & note "Exception: 0^0 is mathematically undefined but Haskell returns 1"

--------------------------------------------------------------------------------
-- Comparison Rules
--------------------------------------------------------------------------------

-- | Rules for numeric comparison patterns.
comparisonRules :: [Rule]
comparisonRules =
  [ compareToZero
  , signumCompare
  , absCompare
  , equalityToCompare
  , inequalityChain
  , compareOrdering
  ]

-- | Compare to zero with signum.
--
-- @
-- x > 0   ==>  signum x == 1  (sometimes)
-- x >= 0  ==>  signum x /= -1
-- @
compareToZero :: Rule
compareToZero =
  rule "compare-to-zero" $
    match ("_x > 0" ==> "_x > 0")
    & category Style
    & severity Info
    & message "Consider using signum for sign checks"
    & safetyLevel ManualReview

-- | Use signum for sign comparison.
--
-- @
-- if x < 0 then -1 else if x > 0 then 1 else 0  ==>  signum x
-- @
signumCompare :: Rule
signumCompare =
  rule "signum-compare" $
    matchText "if .+ < 0 then -1 else if .+ > 0 then 1 else 0"
    & category Style
    & severity Suggestion
    & message "Use signum for sign detection"

-- | abs comparison simplification.
--
-- @
-- abs x < abs y  -- Direct comparison may be clearer
-- @
absCompare :: Rule
absCompare =
  rule "abs-compare" $
    match ("abs _x == abs _y" ==> "abs _x == abs _y")
    & category Style
    & severity Info
    & message "Comparing absolute values"
    & safetyLevel ManualReview

-- | Use compare instead of equality.
--
-- @
-- if x < y then LT else if x > y then GT else EQ  ==>  compare x y
-- @
equalityToCompare :: Rule
equalityToCompare =
  rule "equality-to-compare" $
    matchText "if .+ < .+ then LT else if .+ > .+ then GT else EQ"
    & category Style
    & severity Suggestion
    & message "Use compare instead of manual Ordering construction"

-- | Chained inequalities.
--
-- @
-- x <= y && y <= z  -- Represents x <= y <= z
-- @
inequalityChain :: Rule
inequalityChain =
  rule "inequality-chain" $
    match ("_x <= _y && _y <= _z" ==> "_x <= _y && _y <= _z")
    & category Style
    & severity Info
    & message "Chained comparison - ensure bounds checking is intentional"
    & safetyLevel ManualReview

-- | Use comparing for Ordering.
--
-- @
-- compare (f x) (f y)  ==>  comparing f x y
-- @
compareOrdering :: Rule
compareOrdering =
  rule "compare-to-comparing" $
    match ("compare (_f _x) (_f _y)" ==> "comparing _f _x _y")
    & category Style
    & severity Suggestion
    & message "Use comparing for comparing by a function"

--------------------------------------------------------------------------------
-- Conversion Rules
--------------------------------------------------------------------------------

-- | Rules for type conversion.
numericConversionRules :: [Rule]
numericConversionRules =
  [ fromIntegralId
  , realToFracId
  , truncateFloor
  , ceilingRound
  , toIntegerFromIntegral
  , doubleToRational
  ]

-- | fromIntegral to same type is id.
--
-- @
-- fromIntegral x :: Int  -- where x :: Int, this is id
-- @
fromIntegralId :: Rule
fromIntegralId =
  rule "fromIntegral-id" $
    matchText "fromIntegral \\([a-z]+ :: Int\\) :: Int"
    & category Redundant
    & severity Warning
    & message "fromIntegral to same type is redundant"
    & note "Check if the types actually differ"

-- | realToFrac to same type is id.
--
-- @
-- realToFrac x :: Double  -- where x :: Double
-- @
realToFracId :: Rule
realToFracId =
  rule "realToFrac-id" $
    matchText "realToFrac \\([a-z]+ :: Double\\) :: Double"
    & category Redundant
    & severity Warning
    & message "realToFrac to same type is redundant"

-- | truncate vs floor.
--
-- @
-- truncate x  -- Different from floor for negatives
-- @
truncateFloor :: Rule
truncateFloor =
  rule "truncate-vs-floor" $
    match ("truncate _x" ==> "truncate _x")
    & category Style
    & severity Info
    & message "Note: truncate rounds toward zero; floor rounds toward negative infinity"
    & safetyLevel ManualReview

-- | ceiling vs round.
--
-- @
-- ceiling x  -- Different from round
-- @
ceilingRound :: Rule
ceilingRound =
  rule "ceiling-vs-round" $
    match ("ceiling _x" ==> "ceiling _x")
    & category Style
    & severity Info
    & message "Note: ceiling rounds up; round rounds to nearest even"
    & safetyLevel ManualReview

-- | toInteger from fromIntegral.
--
-- @
-- toInteger (fromIntegral x)  -- May lose precision
-- @
toIntegerFromIntegral :: Rule
toIntegerFromIntegral =
  rule "toInteger-fromIntegral" $
    match ("toInteger (fromIntegral _x)" ==> "toInteger _x")
    & category Redundant
    & severity Suggestion
    & message "toInteger . fromIntegral can often be simplified to toInteger"

-- | Double to Rational conversion.
--
-- @
-- toRational (x :: Double)  -- May have precision issues
-- @
doubleToRational :: Rule
doubleToRational =
  rule "double-to-rational" $
    matchText "toRational \\(.+ :: Double\\)"
    & category Safety
    & severity Info
    & message "Converting Double to Rational may not preserve expected precision"
    & note "Double has limited precision; Rational is exact"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Precision Rules
--------------------------------------------------------------------------------

-- | Rules for floating point precision.
precisionRules :: [Rule]
precisionRules =
  [ floatEquality
  , divisionPrecision
  , numericInfiniteRecursion
  , overflowWarning
  , underflowWarning
  ]

-- | Floating point equality is dangerous.
--
-- @
-- x == y  -- where x, y are Double/Float
-- @
floatEquality :: Rule
floatEquality =
  rule "float-equality" $
    matchText "\\(.+ :: Double\\) == \\(.+ :: Double\\)"
    & category Safety
    & severity Warning
    & message "Direct floating-point equality comparison"
    & note "Consider using approximate comparison with epsilon tolerance"

-- | Division precision warning.
--
-- @
-- x / y * y  -- May not equal x due to precision
-- @
divisionPrecision :: Rule
divisionPrecision =
  rule "division-precision" $
    match ("_x / _y * _y" ==> "_x / _y * _y")
    & category Safety
    & severity Info
    & message "Division followed by multiplication may lose precision"
    & note "x / y * y may not equal x for floating-point numbers"
    & safetyLevel ManualReview

-- | Infinite recursion warning.
--
-- @
-- let x = x + 1  -- Infinite loop
-- @
numericInfiniteRecursion :: Rule
numericInfiniteRecursion =
  rule "numeric-infinite-recursion" $
    matchText "let [a-z]+ = [a-z]+ [+*/-]"
    & category Correctness
    & severity Error
    & message "Possible infinite recursion in numeric binding"
    & safetyLevel ManualReview

-- | Integer overflow warning.
--
-- @
-- maxBound + 1  -- Overflow!
-- @
overflowWarning :: Rule
overflowWarning =
  rule "overflow-warning" $
    match ("maxBound + _n" ==> "maxBound + _n")
    & category Safety
    & severity Warning
    & message "Adding to maxBound will overflow"
    & note "For bounded types, maxBound + n overflows"
    & safetyLevel ManualReview

-- | Integer underflow warning.
--
-- @
-- minBound - 1  -- Underflow!
-- @
underflowWarning :: Rule
underflowWarning =
  rule "underflow-warning" $
    match ("minBound - _n" ==> "minBound - _n")
    & category Safety
    & severity Warning
    & message "Subtracting from minBound will underflow"
    & note "For bounded types, minBound - n underflows"
    & safetyLevel ManualReview
