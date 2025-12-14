{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Foldable
-- Description : Foldable and Traversable optimization rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for optimizing Foldable and Traversable operations.
-- These rules suggest more efficient or idiomatic alternatives.
--
-- == Rule Categories
--
-- * __Fold Optimization__: More efficient fold patterns
-- * __Traversal Optimization__: Traverse instead of map/sequence
-- * __Specialized Functions__: Use specialized variants
-- * __List to Foldable__: Generalize list operations

module Argus.Rules.Builtin.Foldable
  ( -- * Rule Sets
    foldableRules
  , foldOptimizationRules
  , foldableTraversalRules
  , specializedRules
  , listToFoldableRules

    -- * Fold Optimization
  , foldlToFoldl'
  , foldrToFoldl'
  , foldMapToFold
  , concatMapToFoldMap
  , anyToOr
  , allToAnd
  , sumToFoldl'
  , productToFoldl'
  , lengthToFold
  , nullToFold

    -- * Traversal
  , foldableMapMToTraverse
  , foldableForMToFor
  , sequenceAToTraverse
  , traverseToFor
  , mapMToTraverse_
  , forMToFor_
  , sequenceToSequence_

    -- * Specialized Functions
  , elemToMember
  , notElemToNotMember
  , findToLookup
  , maximumToFoldl1
  , minimumToFoldl1
  , maximumByToFold
  , minimumByToFold

    -- * List to Foldable
  , listConcatToFold
  , listAnyToAny
  , listAllToAll
  , listNullToNull
  , listLengthToLength
  , listElemToElem
  , listSumToSum
  , listProductToProduct

    -- * Rule Count
  , foldableRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All Foldable and Traversable rules.
foldableRules :: [Rule]
foldableRules = mconcat
  [ foldOptimizationRules
  , foldableTraversalRules
  , specializedRules
  , listToFoldableRules
  ]

-- | Total count of Foldable rules.
foldableRuleCount :: Int
foldableRuleCount = length foldableRules

--------------------------------------------------------------------------------
-- Fold Optimization Rules
--------------------------------------------------------------------------------

-- | Rules for optimizing fold patterns.
foldOptimizationRules :: [Rule]
foldOptimizationRules =
  [ foldlToFoldl'
  , foldrToFoldl'
  , foldMapToFold
  , concatMapToFoldMap
  , anyToOr
  , allToAnd
  , sumToFoldl'
  , productToFoldl'
  , lengthToFold
  , nullToFold
  ]

-- | Use foldl' for strict left folds.
--
-- @
-- foldl f z xs  ==>  foldl' f z xs
-- @
foldlToFoldl' :: Rule
foldlToFoldl' =
  rule "foldl-to-foldl'" $
    match ("foldl _f _z _xs" ==> "foldl' _f _z _xs")
    & category Performance
    & severity Warning
    & message "Use foldl' to avoid space leaks"
    & note "foldl builds up thunks; foldl' is strict in the accumulator"

-- | foldr can often be foldl' for strict operations.
--
-- @
-- foldr (+) 0 xs  -- If commutative, can use foldl'
-- @
foldrToFoldl' :: Rule
foldrToFoldl' =
  rule "foldr-to-foldl'-commutative" $
    match ("foldr (+) _z _xs" ==> "foldl' (+) _z _xs")
    & category Performance
    & severity Info
    & message "Consider foldl' for commutative strict operations"
    & note "For (+), (*), and other commutative operations, foldl' is more efficient"
    & safetyLevel ManualReview

-- | Use fold instead of foldMap id.
--
-- @
-- foldMap id xs  ==>  fold xs
-- @
foldMapToFold :: Rule
foldMapToFold =
  rule "foldMap-to-fold" $
    match ("foldMap id _xs" ==> "fold _xs")
    & category Style
    & severity Suggestion
    & message "Use fold instead of foldMap id"

-- | Use foldMap instead of concat . map.
--
-- @
-- concat (map f xs)  ==>  foldMap f xs
-- @
concatMapToFoldMap :: Rule
concatMapToFoldMap =
  rule "concat-map-to-foldMap" $
    match ("concat (map _f _xs)" ==> "foldMap _f _xs")
    & category Performance
    & severity Suggestion
    & message "Use foldMap instead of concat . map"
    & note "foldMap avoids intermediate list creation"

-- | Use or instead of foldr (||) False.
--
-- @
-- foldr (||) False xs  ==>  or xs
-- @
anyToOr :: Rule
anyToOr =
  rule "foldr-or-to-or" $
    match ("foldr (||) False _xs" ==> "or _xs")
    & category Style
    & severity Suggestion
    & message "Use or instead of foldr (||) False"

-- | Use and instead of foldr (&&) True.
--
-- @
-- foldr (&&) True xs  ==>  and xs
-- @
allToAnd :: Rule
allToAnd =
  rule "foldr-and-to-and" $
    match ("foldr (&&) True _xs" ==> "and _xs")
    & category Style
    & severity Suggestion
    & message "Use and instead of foldr (&&) True"

-- | Use sum instead of manual fold.
--
-- @
-- foldl' (+) 0 xs  ==>  sum xs
-- @
sumToFoldl' :: Rule
sumToFoldl' =
  rule "foldl-plus-to-sum" $
    match ("foldl' (+) 0 _xs" ==> "sum _xs")
    & category Style
    & severity Suggestion
    & message "Use sum instead of foldl' (+) 0"

-- | Use product instead of manual fold.
--
-- @
-- foldl' (*) 1 xs  ==>  product xs
-- @
productToFoldl' :: Rule
productToFoldl' =
  rule "foldl-mult-to-product" $
    match ("foldl' (*) 1 _xs" ==> "product _xs")
    & category Style
    & severity Suggestion
    & message "Use product instead of foldl' (*) 1"

-- | Suggest using length directly.
--
-- @
-- foldl' (\\n _ -> n + 1) 0 xs  ==>  length xs
-- @
lengthToFold :: Rule
lengthToFold =
  rule "fold-to-length" $
    match ("foldr (\\_ n -> n + 1) 0 _xs" ==> "length _xs")
    & category Style
    & severity Suggestion
    & message "Use length instead of manual counting fold"

-- | Use null instead of length comparison.
--
-- @
-- length xs == 0  ==>  null xs
-- @
nullToFold :: Rule
nullToFold =
  rule "length-to-null" $
    match ("length _xs == 0" ==> "null _xs")
    & category Performance
    & severity Warning
    & message "Use null instead of length comparison"
    & note "null is O(1) for most containers; length may be O(n)"

--------------------------------------------------------------------------------
-- Traversal Rules
--------------------------------------------------------------------------------

-- | Rules for Traversable optimization.
foldableTraversalRules :: [Rule]
foldableTraversalRules =
  [ foldableMapMToTraverse
  , foldableForMToFor
  , sequenceAToTraverse
  , traverseToFor
  , mapMToTraverse_
  , forMToFor_
  , sequenceToSequence_
  ]

-- | Use traverse instead of mapM.
--
-- @
-- mapM f xs  ==>  traverse f xs
-- @
foldableMapMToTraverse :: Rule
foldableMapMToTraverse =
  rule "foldable-mapM-to-traverse" $
    match ("mapM _f _xs" ==> "traverse _f _xs")
    & category Modernization
    & severity Suggestion
    & message "Use traverse instead of mapM"
    & note "traverse only requires Applicative, not Monad"

-- | Use for instead of forM.
--
-- @
-- forM xs f  ==>  for xs f
-- @
foldableForMToFor :: Rule
foldableForMToFor =
  rule "foldable-forM-to-for" $
    match ("forM _xs _f" ==> "for _xs _f")
    & category Modernization
    & severity Suggestion
    & message "Use for instead of forM"
    & note "for only requires Applicative, not Monad"

-- | Use traverse instead of sequenceA . fmap.
--
-- @
-- sequenceA (fmap f xs)  ==>  traverse f xs
-- @
sequenceAToTraverse :: Rule
sequenceAToTraverse =
  rule "sequenceA-fmap-to-traverse" $
    match ("sequenceA (fmap _f _xs)" ==> "traverse _f _xs")
    & category Style
    & severity Suggestion
    & message "Use traverse instead of sequenceA . fmap"

-- | Use for_ instead of traverse when ignoring results.
--
-- @
-- void (traverse f xs)  ==>  traverse_ f xs
-- @
traverseToFor :: Rule
traverseToFor =
  rule "void-traverse-to-traverse_" $
    match ("void (traverse _f _xs)" ==> "traverse_ _f _xs")
    & category Style
    & severity Suggestion
    & message "Use traverse_ when ignoring results"

-- | Use traverse_ instead of mapM_.
--
-- @
-- mapM_ f xs  ==>  traverse_ f xs
-- @
mapMToTraverse_ :: Rule
mapMToTraverse_ =
  rule "mapM_-to-traverse_" $
    match ("mapM_ _f _xs" ==> "traverse_ _f _xs")
    & category Modernization
    & severity Suggestion
    & message "Use traverse_ instead of mapM_"

-- | Use for_ instead of forM_.
--
-- @
-- forM_ xs f  ==>  for_ xs f
-- @
forMToFor_ :: Rule
forMToFor_ =
  rule "forM_-to-for_" $
    match ("forM_ _xs _f" ==> "for_ _xs _f")
    & category Modernization
    & severity Suggestion
    & message "Use for_ instead of forM_"

-- | Use sequence_ instead of void . sequence.
--
-- @
-- void (sequence xs)  ==>  sequence_ xs
-- @
sequenceToSequence_ :: Rule
sequenceToSequence_ =
  rule "void-sequence-to-sequence_" $
    match ("void (sequence _xs)" ==> "sequence_ _xs")
    & category Style
    & severity Suggestion
    & message "Use sequence_ when ignoring results"

--------------------------------------------------------------------------------
-- Specialized Functions Rules
--------------------------------------------------------------------------------

-- | Rules for using specialized container functions.
specializedRules :: [Rule]
specializedRules =
  [ elemToMember
  , notElemToNotMember
  , findToLookup
  , maximumToFoldl1
  , minimumToFoldl1
  , maximumByToFold
  , minimumByToFold
  ]

-- | Use Set.member instead of elem for sets.
--
-- @
-- elem x (Set.fromList xs)  -- Use Set.member instead
-- @
elemToMember :: Rule
elemToMember =
  rule "elem-to-member" $
    match ("elem _x (Set.fromList _xs)" ==> "Set.member _x (Set.fromList _xs)")
    & category Performance
    & severity Warning
    & message "Use Set.member for O(log n) lookup"
    & note "elem is O(n); Set.member is O(log n)"

-- | Use Set.notMember instead of notElem for sets.
--
-- @
-- notElem x (Set.toList s)  ==>  Set.notMember x s
-- @
notElemToNotMember :: Rule
notElemToNotMember =
  rule "notElem-to-notMember" $
    match ("notElem _x (Set.toList _s)" ==> "Set.notMember _x _s")
    & category Performance
    & severity Warning
    & message "Use Set.notMember instead of notElem on Set.toList"

-- | Use Map.lookup instead of find.
--
-- @
-- find (\\(k,v) -> k == key) (Map.toList m)  -- Use Map.lookup
-- @
findToLookup :: Rule
findToLookup =
  rule "find-to-lookup" $
    matchText "find .+ Map.toList"
    & category Performance
    & severity Info
    & message "Consider Map.lookup for O(log n) lookup"
    & safetyLevel ManualReview

-- | maximum can be foldl1' max.
--
-- @
-- foldl1 max xs  ==>  maximum xs
-- @
maximumToFoldl1 :: Rule
maximumToFoldl1 =
  rule "foldl1-max-to-maximum" $
    match ("foldl1 max _xs" ==> "maximum _xs")
    & category Style
    & severity Suggestion
    & message "Use maximum instead of foldl1 max"

-- | minimum can be foldl1' min.
--
-- @
-- foldl1 min xs  ==>  minimum xs
-- @
minimumToFoldl1 :: Rule
minimumToFoldl1 =
  rule "foldl1-min-to-minimum" $
    match ("foldl1 min _xs" ==> "minimum _xs")
    & category Style
    & severity Suggestion
    & message "Use minimum instead of foldl1 min"

-- | Use maximumBy for custom comparison.
--
-- @
-- head (sortBy (comparing f) xs)  -- Consider maximumBy
-- @
maximumByToFold :: Rule
maximumByToFold =
  rule "sort-head-to-maximumBy" $
    match ("head (sortBy _cmp _xs)" ==> "maximumBy _cmp _xs")
    & category Performance
    & severity Suggestion
    & message "Use maximumBy instead of head . sortBy"
    & note "maximumBy is O(n); sortBy is O(n log n)"

-- | Use minimumBy for custom comparison.
--
-- @
-- last (sortBy (comparing f) xs)  -- Consider minimumBy
-- @
minimumByToFold :: Rule
minimumByToFold =
  rule "sort-last-to-minimumBy" $
    match ("last (sortBy _cmp _xs)" ==> "minimumBy _cmp _xs")
    & category Performance
    & severity Suggestion
    & message "Use minimumBy instead of last . sortBy"
    & note "minimumBy is O(n); sortBy is O(n log n)"

--------------------------------------------------------------------------------
-- List to Foldable Rules
--------------------------------------------------------------------------------

-- | Rules for generalizing list operations to Foldable.
listToFoldableRules :: [Rule]
listToFoldableRules =
  [ listConcatToFold
  , listAnyToAny
  , listAllToAll
  , listNullToNull
  , listLengthToLength
  , listElemToElem
  , listSumToSum
  , listProductToProduct
  ]

-- | concat on lists generalizes to fold.
--
-- @
-- concat xss  ==>  fold xss  (when Foldable)
-- @
listConcatToFold :: Rule
listConcatToFold =
  rule "concat-to-fold" $
    match ("concat _xss" ==> "fold _xss")
    & category Style
    & severity Info
    & message "Consider fold for more general Foldable"
    & note "fold works on any Foldable of Monoid"
    & safetyLevel ManualReview

-- | Prelude any generalizes to Foldable any.
--
-- @
-- Data.List.any p xs  ==>  any p xs  (Foldable)
-- @
listAnyToAny :: Rule
listAnyToAny =
  rule "list-any-to-foldable" $
    match ("Data.List.any _p _xs" ==> "any _p _xs")
    & category Modernization
    & severity Suggestion
    & message "Use Foldable any from Prelude"

-- | Prelude all generalizes to Foldable all.
--
-- @
-- Data.List.all p xs  ==>  all p xs  (Foldable)
-- @
listAllToAll :: Rule
listAllToAll =
  rule "list-all-to-foldable" $
    match ("Data.List.all _p _xs" ==> "all _p _xs")
    & category Modernization
    & severity Suggestion
    & message "Use Foldable all from Prelude"

-- | Data.List.null generalizes to Foldable null.
--
-- @
-- Data.List.null xs  ==>  null xs
-- @
listNullToNull :: Rule
listNullToNull =
  rule "list-null-to-foldable" $
    match ("Data.List.null _xs" ==> "null _xs")
    & category Modernization
    & severity Suggestion
    & message "Use Foldable null from Prelude"

-- | Data.List.length generalizes to Foldable length.
--
-- @
-- Data.List.length xs  ==>  length xs
-- @
listLengthToLength :: Rule
listLengthToLength =
  rule "list-length-to-foldable" $
    match ("Data.List.length _xs" ==> "length _xs")
    & category Modernization
    & severity Suggestion
    & message "Use Foldable length from Prelude"

-- | Data.List.elem generalizes to Foldable elem.
--
-- @
-- Data.List.elem x xs  ==>  elem x xs
-- @
listElemToElem :: Rule
listElemToElem =
  rule "list-elem-to-foldable" $
    match ("Data.List.elem _x _xs" ==> "elem _x _xs")
    & category Modernization
    & severity Suggestion
    & message "Use Foldable elem from Prelude"

-- | Data.List.sum generalizes to Foldable sum.
--
-- @
-- Data.List.sum xs  ==>  sum xs
-- @
listSumToSum :: Rule
listSumToSum =
  rule "list-sum-to-foldable" $
    match ("Data.List.sum _xs" ==> "sum _xs")
    & category Modernization
    & severity Suggestion
    & message "Use Foldable sum from Prelude"

-- | Data.List.product generalizes to Foldable product.
--
-- @
-- Data.List.product xs  ==>  product xs
-- @
listProductToProduct :: Rule
listProductToProduct =
  rule "list-product-to-foldable" $
    match ("Data.List.product _xs" ==> "product _xs")
    & category Modernization
    & severity Suggestion
    & message "Use Foldable product from Prelude"
