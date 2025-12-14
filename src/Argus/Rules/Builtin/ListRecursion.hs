{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.ListRecursion
-- Description : List recursion pattern detection and refactoring
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for detecting explicit recursion over lists that can be replaced
-- with higher-order functions like map, fold, and filter. Manual recursion
-- is error-prone and less efficient than library functions.
--
-- == Rule Categories
--
-- * __Map Patterns__: Recursion → map
-- * __Fold Patterns__: Recursion → fold
-- * __Filter Patterns__: Recursion → filter
-- * __Specialized Patterns__: Common recursion idioms
--
-- == References
--
-- * <https://wiki.haskell.org/Fold Fold Wiki>
-- * <https://wiki.haskell.org/Higher_order_function Higher Order Functions>

module Argus.Rules.Builtin.ListRecursion
  ( -- * Rule Sets
    listRecursionRules
  , mapPatternRules
  , foldPatternRules
  , filterPatternRules
  , specialPatternRules

    -- * Map Patterns
  , explicitMap
  , explicitMapMaybe
  , explicitConcatMap
  , explicitZipWith

    -- * Fold Patterns
  , explicitFoldr
  , explicitFoldl
  , explicitSum
  , explicitProduct
  , explicitLength
  , explicitConcat
  , explicitAnd
  , explicitOr

    -- * Filter Patterns
  , explicitFilter
  , explicitPartition
  , explicitTakeWhile
  , explicitDropWhile

    -- * Specialized Patterns
  , tailRecursion
  , accumulatorPattern
  , workerWrapper
  , unfoldPattern

    -- * Rule Count
  , listRecursionRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All list recursion pattern rules.
listRecursionRules :: [Rule]
listRecursionRules = mconcat
  [ mapPatternRules
  , foldPatternRules
  , filterPatternRules
  , specialPatternRules
  ]

-- | Total count of list recursion rules.
listRecursionRuleCount :: Int
listRecursionRuleCount = length listRecursionRules

--------------------------------------------------------------------------------
-- Map Pattern Rules
--------------------------------------------------------------------------------

-- | Rules for detecting map-like recursion.
mapPatternRules :: [Rule]
mapPatternRules =
  [ explicitMap
  , explicitMapMaybe
  , explicitConcatMap
  , explicitZipWith
  ]

-- | Explicit recursion that's just map.
--
-- @
-- f [] = []
-- f (x:xs) = g x : f xs
-- ==>
-- f = map g
-- @
explicitMap :: Rule
explicitMap =
  rule "explicit-map" $
    matchText "\\[\\] = \\[\\].+:\\) = .+ : [a-z]+ [a-z]+"
    & category Performance
    & severity Suggestion
    & message "Explicit recursion looks like 'map'"
    & note "f [] = []; f (x:xs) = g x : f xs  ==>  f = map g"

-- | Explicit recursion that's mapMaybe.
--
-- @
-- f [] = []
-- f (x:xs) = case g x of
--   Just y  -> y : f xs
--   Nothing -> f xs
-- ==>
-- f = mapMaybe g
-- @
explicitMapMaybe :: Rule
explicitMapMaybe =
  rule "explicit-map-maybe" $
    matchText "\\[\\] = \\[\\].+Just.+Nothing.+: [a-z]+ [a-z]+"
    & category Performance
    & severity Suggestion
    & message "Explicit recursion looks like 'mapMaybe'"

-- | Explicit recursion that's concatMap.
--
-- @
-- f [] = []
-- f (x:xs) = g x ++ f xs
-- ==>
-- f = concatMap g
-- @
explicitConcatMap :: Rule
explicitConcatMap =
  rule "explicit-concat-map" $
    matchText "\\[\\] = \\[\\].+\\+\\+ [a-z]+ [a-z]+"
    & category Performance
    & severity Suggestion
    & message "Explicit recursion looks like 'concatMap'"
    & note "f [] = []; f (x:xs) = g x ++ f xs  ==>  f = concatMap g"

-- | Explicit recursion that's zipWith.
--
-- @
-- f [] _ = []
-- f _ [] = []
-- f (x:xs) (y:ys) = g x y : f xs ys
-- ==>
-- f = zipWith g
-- @
explicitZipWith :: Rule
explicitZipWith =
  rule "explicit-zip-with" $
    matchText "\\[\\] _ = \\[\\].+_ \\[\\] = \\[\\].+: [a-z]+ [a-z]+ [a-z]+"
    & category Performance
    & severity Suggestion
    & message "Explicit recursion looks like 'zipWith'"

--------------------------------------------------------------------------------
-- Fold Pattern Rules
--------------------------------------------------------------------------------

-- | Rules for detecting fold-like recursion.
foldPatternRules :: [Rule]
foldPatternRules =
  [ explicitFoldr
  , explicitFoldl
  , explicitSum
  , explicitProduct
  , explicitLength
  , explicitConcat
  , explicitAnd
  , explicitOr
  ]

-- | Explicit recursion that's foldr.
--
-- @
-- f [] = z
-- f (x:xs) = x `op` f xs
-- ==>
-- f = foldr op z
-- @
explicitFoldr :: Rule
explicitFoldr =
  rule "explicit-foldr" $
    matchText "\\[\\] = [a-z0-9]+.+`[a-z]+` [a-z]+ [a-z]+"
    & category Performance
    & severity Suggestion
    & message "Explicit recursion looks like 'foldr'"
    & note "f [] = z; f (x:xs) = x `op` f xs  ==>  f = foldr op z"

-- | Explicit recursion that's foldl' (with accumulator).
--
-- @
-- f acc [] = acc
-- f acc (x:xs) = f (acc `op` x) xs
-- ==>
-- f = foldl' op
-- @
explicitFoldl :: Rule
explicitFoldl =
  rule "explicit-foldl" $
    matchText "[a-z]+ \\[\\] = [a-z]+.+[a-z]+ \\([a-z]+"
    & category Performance
    & severity Suggestion
    & message "Explicit recursion with accumulator looks like 'foldl''"
    & note "Use foldl' (strict) to prevent space leaks"

-- | Explicit sum implementation.
--
-- @
-- sum [] = 0
-- sum (x:xs) = x + sum xs
-- ==>
-- sum = foldl' (+) 0
-- @
explicitSum :: Rule
explicitSum =
  rule "explicit-sum" $
    matchText "\\[\\] = 0.+\\+ [a-z]+ [a-z]+"
    & category Performance
    & severity Warning
    & message "Explicit sum - use 'sum' or 'foldl\\' (+) 0'"
    & note "Library sum is optimized and handles edge cases"

-- | Explicit product implementation.
--
-- @
-- prod [] = 1
-- prod (x:xs) = x * prod xs
-- ==>
-- prod = foldl' (*) 1
-- @
explicitProduct :: Rule
explicitProduct =
  rule "explicit-product" $
    matchText "\\[\\] = 1.+\\* [a-z]+ [a-z]+"
    & category Performance
    & severity Warning
    & message "Explicit product - use 'product' or 'foldl\\' (*) 1'"

-- | Explicit length implementation.
--
-- @
-- len [] = 0
-- len (_:xs) = 1 + len xs
-- ==>
-- len = length
-- @
explicitLength :: Rule
explicitLength =
  rule "explicit-length" $
    matchText "\\[\\] = 0.+1 \\+ [a-z]+ [a-z]+"
    & category Performance
    & severity Warning
    & message "Explicit length - use 'length'"
    & note "Library length may be optimized for specific containers"

-- | Explicit concat implementation.
--
-- @
-- cat [] = []
-- cat (x:xs) = x ++ cat xs
-- ==>
-- cat = concat
-- @
explicitConcat :: Rule
explicitConcat =
  rule "explicit-concat" $
    matchText "\\[\\] = \\[\\].+\\+\\+ [a-z]+ [a-z]+$"
    & category Performance
    & severity Suggestion
    & message "Explicit concatenation - use 'concat'"

-- | Explicit and implementation.
--
-- @
-- allTrue [] = True
-- allTrue (x:xs) = x && allTrue xs
-- ==>
-- allTrue = and
-- @
explicitAnd :: Rule
explicitAnd =
  rule "explicit-and" $
    matchText "\\[\\] = True.+&& [a-z]+ [a-z]+"
    & category Performance
    & severity Suggestion
    & message "Explicit conjunction - use 'and'"

-- | Explicit or implementation.
--
-- @
-- anyTrue [] = False
-- anyTrue (x:xs) = x || anyTrue xs
-- ==>
-- anyTrue = or
-- @
explicitOr :: Rule
explicitOr =
  rule "explicit-or" $
    matchText "\\[\\] = False.+\\|\\| [a-z]+ [a-z]+"
    & category Performance
    & severity Suggestion
    & message "Explicit disjunction - use 'or'"

--------------------------------------------------------------------------------
-- Filter Pattern Rules
--------------------------------------------------------------------------------

-- | Rules for detecting filter-like recursion.
filterPatternRules :: [Rule]
filterPatternRules =
  [ explicitFilter
  , explicitPartition
  , explicitTakeWhile
  , explicitDropWhile
  ]

-- | Explicit recursion that's filter.
--
-- @
-- f [] = []
-- f (x:xs) | p x = x : f xs
--          | otherwise = f xs
-- ==>
-- f = filter p
-- @
explicitFilter :: Rule
explicitFilter =
  rule "explicit-filter" $
    matchText "\\[\\] = \\[\\].+\\| .+ = .+ : [a-z]+ [a-z]+.+otherwise = [a-z]+ [a-z]+"
    & category Performance
    & severity Suggestion
    & message "Explicit recursion looks like 'filter'"

-- | Explicit recursion that's partition.
--
-- @
-- f [] = ([], [])
-- f (x:xs) | p x = (x:as, bs)
--          | otherwise = (as, x:bs)
--   where (as, bs) = f xs
-- ==>
-- f = partition p
-- @
explicitPartition :: Rule
explicitPartition =
  rule "explicit-partition" $
    matchText "\\[\\] = \\(\\[\\], \\[\\]\\)"
    & category Performance
    & severity Suggestion
    & message "Explicit recursion looks like 'partition'"

-- | Explicit recursion that's takeWhile.
--
-- @
-- f [] = []
-- f (x:xs) | p x = x : f xs
--          | otherwise = []
-- ==>
-- f = takeWhile p
-- @
explicitTakeWhile :: Rule
explicitTakeWhile =
  rule "explicit-take-while" $
    matchText "\\[\\] = \\[\\].+\\| .+ = .+ : [a-z]+ [a-z]+.+otherwise = \\[\\]"
    & category Performance
    & severity Suggestion
    & message "Explicit recursion looks like 'takeWhile'"

-- | Explicit recursion that's dropWhile.
--
-- @
-- f [] = []
-- f (x:xs) | p x = f xs
--          | otherwise = x:xs
-- ==>
-- f = dropWhile p
-- @
explicitDropWhile :: Rule
explicitDropWhile =
  rule "explicit-drop-while" $
    matchText "\\[\\] = \\[\\].+\\| .+ = [a-z]+ [a-z]+.+otherwise = .+:"
    & category Performance
    & severity Suggestion
    & message "Explicit recursion looks like 'dropWhile'"

--------------------------------------------------------------------------------
-- Specialized Pattern Rules
--------------------------------------------------------------------------------

-- | Rules for specialized recursion patterns.
specialPatternRules :: [Rule]
specialPatternRules =
  [ tailRecursion
  , accumulatorPattern
  , workerWrapper
  , unfoldPattern
  ]

-- | Non-tail recursion warning.
--
-- @
-- f (x:xs) = x : f xs  -- Not tail recursive, builds thunks
-- @
tailRecursion :: Rule
tailRecursion =
  rule "tail-recursion" $
    matchText ": [a-z]+ [a-z]+$"
    & category Performance
    & severity Info
    & message "Non-tail recursive cons - may build up thunks"
    & note "Consider using an accumulator or difference list"
    & safetyLevel ManualReview

-- | Missing accumulator pattern.
--
-- @
-- rev [] = []
-- rev (x:xs) = rev xs ++ [x]  -- O(n²)!
-- ==>
-- rev = foldl' (flip (:)) []  -- O(n)
-- @
accumulatorPattern :: Rule
accumulatorPattern =
  rule "accumulator-pattern" $
    matchText "[a-z]+ [a-z]+ \\+\\+ \\["
    & category Performance
    & severity Warning
    & message "Appending single element is O(n) - use accumulator"
    & note "xs ++ [x] in recursion is O(n²), use foldl' with (:)"

-- | Worker-wrapper pattern suggestion.
--
-- @
-- f x = go x []
--   where
--     go [] acc = acc
--     go (y:ys) acc = go ys (y:acc)
-- @
workerWrapper :: Rule
workerWrapper =
  rule "worker-wrapper" $
    matchText "where.+go \\[\\] [a-z]+ =.+go \\(.+:\\)"
    & category Performance
    & severity Info
    & message "Using worker-wrapper pattern - good for efficiency"
    & note "Ensure the worker is strict in the accumulator"
    & safetyLevel ManualReview

-- | Unfold pattern suggestion.
--
-- @
-- f 0 = []
-- f n = n : f (n - 1)
-- ==>
-- f = unfoldr (\n -> if n == 0 then Nothing else Just (n, n-1))
-- @
unfoldPattern :: Rule
unfoldPattern =
  rule "unfold-pattern" $
    matchText "0 = \\[\\].+: [a-z]+ \\([a-z]+ - 1\\)"
    & category Style
    & severity Info
    & message "Countdown recursion - consider 'unfoldr'"
    & note "unfoldr generates lists from seed values"
    & safetyLevel ManualReview
