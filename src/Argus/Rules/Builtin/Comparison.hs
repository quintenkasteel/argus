{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Comparison
-- Description : Comparison and ordering rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for comparison operations, ordering, and related simplifications.

module Argus.Rules.Builtin.Comparison
  ( -- * Rule Sets
    ordComparisonRules
  , basicComparisonRules
  , orderingRules

    -- * Basic Comparisons
  , eqSelf
  , neSelf
  , ltSelf
  , gtSelf
  , leSelf
  , geSelf
  , compareSelf
  , compareLt
  , compareGt
  , compareEq
  , compareNe
  , notEq
  , notNe
  , notLt
  , notGt
  , notLe
  , notGe

    -- * Ordering Functions
  , maxSelf
  , minSelf
  , maxEq
  , minEq
  , comparingPattern
  , onCompare
  , sortByComparing
  , maximumByComparing
  , minimumByComparing
  , sortOnPattern
  , groupByEq

    -- * Rule Count
  , comparisonRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All comparison-related rules.
ordComparisonRules :: [Rule]
ordComparisonRules = mconcat
  [ basicComparisonRules
  , orderingRules
  ]

-- | Total count of comparison rules.
comparisonRuleCount :: Int
comparisonRuleCount = length ordComparisonRules

--------------------------------------------------------------------------------
-- Basic Comparison Rules
--------------------------------------------------------------------------------

-- | Rules for basic comparisons.
basicComparisonRules :: [Rule]
basicComparisonRules =
  [ eqSelf
  , neSelf
  , ltSelf
  , gtSelf
  , leSelf
  , geSelf
  , compareSelf
  , compareLt
  , compareGt
  , compareEq
  , compareNe
  , notEq
  , notNe
  , notLt
  , notGt
  , notLe
  , notGe
  ]

-- | x == x ==> True (with NaN warning)
eqSelf :: Rule
eqSelf =
  rule "eq-self" $
    matchText "([a-z_][a-z0-9_]*)\\s*==\\s*\\1\\b"
    & category Style
    & severity Suggestion
    & message "x == x is True"
    & note "Warning: NaN /= NaN for floating point"

-- | x /= x ==> False (with NaN warning)
neSelf :: Rule
neSelf =
  rule "ne-self" $
    matchText "([a-z_][a-z0-9_]*)\\s*/=\\s*\\1\\b"
    & category Style
    & severity Suggestion
    & message "x /= x is False"
    & note "Warning: NaN /= NaN is True for floating point"

-- | x < x ==> False
ltSelf :: Rule
ltSelf =
  rule "lt-self" $
    matchText "([a-z_][a-z0-9_]*)\\s*<\\s*\\1\\b"
    & category Style
    & severity Suggestion
    & message "x < x is False"

-- | x > x ==> False
gtSelf :: Rule
gtSelf =
  rule "gt-self" $
    matchText "([a-z_][a-z0-9_]*)\\s*>\\s*\\1\\b"
    & category Style
    & severity Suggestion
    & message "x > x is False"

-- | x <= x ==> True
leSelf :: Rule
leSelf =
  rule "le-self" $
    matchText "([a-z_][a-z0-9_]*)\\s*<=\\s*\\1\\b"
    & category Style
    & severity Suggestion
    & message "x <= x is True"

-- | x >= x ==> True
geSelf :: Rule
geSelf =
  rule "ge-self" $
    matchText "([a-z_][a-z0-9_]*)\\s*>=\\s*\\1\\b"
    & category Style
    & severity Suggestion
    & message "x >= x is True"

-- | compare x x ==> EQ
compareSelf :: Rule
compareSelf =
  rule "compare-self" $
    matchText "compare\\s+([a-z_][a-z0-9_]*)\\s+\\1\\b"
    & category Style
    & severity Suggestion
    & message "compare x x is EQ"

-- | compare x y == LT ==> x < y
compareLt :: Rule
compareLt =
  rule "compare-lt" $
    match ("compare _x _y == LT" ==> "_x < _y")
    & category Style
    & severity Suggestion
    & message "compare x y == LT is x < y"

-- | compare x y == GT ==> x > y
compareGt :: Rule
compareGt =
  rule "compare-gt" $
    match ("compare _x _y == GT" ==> "_x > _y")
    & category Style
    & severity Suggestion
    & message "compare x y == GT is x > y"

-- | compare x y == EQ ==> x == y
compareEq :: Rule
compareEq =
  rule "compare-eq" $
    match ("compare _x _y == EQ" ==> "_x == _y")
    & category Style
    & severity Suggestion
    & message "compare x y == EQ is x == y"

-- | compare x y /= EQ ==> x /= y
compareNe :: Rule
compareNe =
  rule "compare-ne" $
    match ("compare _x _y /= EQ" ==> "_x /= _y")
    & category Style
    & severity Suggestion
    & message "compare x y /= EQ is x /= y"

-- | not (x == y) ==> x /= y
notEq :: Rule
notEq =
  rule "not-eq" $
    match ("not (_x == _y)" ==> "_x /= _y")
    & category Style
    & severity Suggestion
    & message "not (x == y) is x /= y"

-- | not (x /= y) ==> x == y
notNe :: Rule
notNe =
  rule "not-ne" $
    match ("not (_x /= _y)" ==> "_x == _y")
    & category Style
    & severity Suggestion
    & message "not (x /= y) is x == y"

-- | not (x < y) ==> x >= y
notLt :: Rule
notLt =
  rule "not-lt" $
    match ("not (_x < _y)" ==> "_x >= _y")
    & category Style
    & severity Suggestion
    & message "not (x < y) is x >= y"

-- | not (x > y) ==> x <= y
notGt :: Rule
notGt =
  rule "not-gt" $
    match ("not (_x > _y)" ==> "_x <= _y")
    & category Style
    & severity Suggestion
    & message "not (x > y) is x <= y"

-- | not (x <= y) ==> x > y
notLe :: Rule
notLe =
  rule "not-le" $
    match ("not (_x <= _y)" ==> "_x > _y")
    & category Style
    & severity Suggestion
    & message "not (x <= y) is x > y"

-- | not (x >= y) ==> x < y
notGe :: Rule
notGe =
  rule "not-ge" $
    match ("not (_x >= _y)" ==> "_x < _y")
    & category Style
    & severity Suggestion
    & message "not (x >= y) is x < y"

--------------------------------------------------------------------------------
-- Ordering Function Rules
--------------------------------------------------------------------------------

-- | Rules for ordering functions.
orderingRules :: [Rule]
orderingRules =
  [ maxSelf
  , minSelf
  , maxEq
  , minEq
  , comparingPattern
  , onCompare
  , sortByComparing
  , maximumByComparing
  , minimumByComparing
  , sortOnPattern
  , groupByEq
  ]

-- | max x x ==> x
maxSelf :: Rule
maxSelf =
  rule "max-self" $
    matchText "max\\s+([a-z_][a-z0-9_]*)\\s+\\1\\b"
    & category Style
    & severity Suggestion
    & message "max x x is x"

-- | min x x ==> x
minSelf :: Rule
minSelf =
  rule "min-self" $
    matchText "min\\s+([a-z_][a-z0-9_]*)\\s+\\1\\b"
    & category Style
    & severity Suggestion
    & message "min x x is x"

-- | max x y == x ==> x >= y
maxEq :: Rule
maxEq =
  rule "max-eq" $
    matchText "max\\s+([a-z_]+)\\s+[a-z_]+\\s*==\\s*\\1"
    & category Style
    & severity Info
    & message "max x y == x implies x >= y"
    & safetyLevel ManualReview

-- | min x y == x ==> x <= y
minEq :: Rule
minEq =
  rule "min-eq" $
    matchText "min\\s+([a-z_]+)\\s+[a-z_]+\\s*==\\s*\\1"
    & category Style
    & severity Info
    & message "min x y == x implies x <= y"
    & safetyLevel ManualReview

-- | comparing f x y ==> compare (f x) (f y)
comparingPattern :: Rule
comparingPattern =
  rule "comparing-pattern" $
    match ("comparing _f _x _y" ==> "compare (_f _x) (_f _y)")
    & category Style
    & severity Info
    & message "comparing f x y is compare (f x) (f y)"
    & safetyLevel ManualReview

-- | on compare f x y ==> comparing f x y
onCompare :: Rule
onCompare =
  rule "on-compare" $
    match ("on compare _f" ==> "comparing _f")
    & category Style
    & severity Suggestion
    & message "on compare f is comparing f"

-- | sortBy (comparing f) ==> sortOn f
sortByComparing :: Rule
sortByComparing =
  rule "sortBy-comparing" $
    match ("sortBy (comparing _f)" ==> "sortOn _f")
    & category Performance
    & severity Suggestion
    & message "sortBy (comparing f) can be sortOn f"
    & note "sortOn is more efficient for expensive key functions"

-- | maximumBy (comparing f) pattern.
maximumByComparing :: Rule
maximumByComparing =
  rule "maximumBy-comparing" $
    match ("maximumBy (comparing _f)" ==> "maximumBy (comparing _f)")
    & category Style
    & severity Info
    & message "Using maximumBy with comparing"
    & safetyLevel ManualReview

-- | minimumBy (comparing f) pattern.
minimumByComparing :: Rule
minimumByComparing =
  rule "minimumBy-comparing" $
    match ("minimumBy (comparing _f)" ==> "minimumBy (comparing _f)")
    & category Style
    & severity Info
    & message "Using minimumBy with comparing"
    & safetyLevel ManualReview

-- | sortOn pattern.
sortOnPattern :: Rule
sortOnPattern =
  rule "sortOn-pattern" $
    matchText "\\bsortOn\\b"
    & category Style
    & severity Info
    & message "sortOn caches key - efficient for expensive keys"
    & safetyLevel ManualReview

-- | groupBy (==) pattern.
groupByEq :: Rule
groupByEq =
  rule "groupBy-eq" $
    match ("groupBy (==)" ==> "group")
    & category Style
    & severity Suggestion
    & message "groupBy (==) is just group"
