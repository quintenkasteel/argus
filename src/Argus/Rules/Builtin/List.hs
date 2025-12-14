{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.List
-- Description : List operation rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Comprehensive rules for list operations, simplifications,
-- and performance improvements.

module Argus.Rules.Builtin.List
  ( -- * Rule Sets
    listRules
  , listSimplificationRules
  , listComprehensionRules
  , listConstructionRules
  , listIndexingRules
  , listTransformRules

    -- * List Simplifications
  , concatMapRule
  , concatSingleton
  , concatSingleList
  , headReverse
  , lastReverse
  , takeRepeat
  , mapRepeat
  , mapReplicate
  , lengthZero
  , lengthNotZero
  , lengthGtZero
  , lengthLtOne
  , lengthGeOne
  , zeroEqLength
  , zeroNeLength
  , headDrop
  , headSortBy
  , lastSortBy
  , headSort
  , lastSort

    -- * List Comprehensions
  , listCompId
  , listCompMap
  , listCompFilter
  , listCompMapFilter
  , listCompProduct
  , listCompTrue
  , listCompFalse
  , listCompConcat
  , listCompVoid

    -- * List Construction
  , singletonCons
  , doubleCons
  , emptyAppend
  , appendEmpty
  , singletonAppend
  , appendSingletonWarn
  , prependPartial
  , cycleReplicate
  , intercalateSpace
  , intercalateNewline

    -- * List Indexing
  , indexZero
  , takeOneWarn
  , dropOneWarn
  , takeDropSlice
  , indexLast
  , takeLengthMinusOne
  , dropLength
  , splitAtZero
  , splitAtEmpty

    -- * List Transforms
  , filterTrue
  , filterFalse
  , takeZero
  , dropZero
  , zipSame
  , zipWithConst
  , replicateZero
  , cycleEmpty
  , nubOrd

    -- * Rule Count
  , listRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All list-related rules.
listRules :: [Rule]
listRules = mconcat
  [ listSimplificationRules
  , listComprehensionRules
  , listConstructionRules
  , listIndexingRules
  , listTransformRules
  ]

-- | Total count of list rules.
listRuleCount :: Int
listRuleCount = length listRules

--------------------------------------------------------------------------------
-- List Simplifications
--------------------------------------------------------------------------------

-- | Rules for list simplifications.
listSimplificationRules :: [Rule]
listSimplificationRules =
  [ concatMapRule
  , concatSingleton
  , concatSingleList
  , headReverse
  , lastReverse
  , takeRepeat
  , mapRepeat
  , mapReplicate
  , lengthZero
  , lengthNotZero
  , lengthGtZero
  , lengthLtOne
  , lengthGeOne
  , zeroEqLength
  , zeroNeLength
  , headDrop
  , headSortBy
  , lastSortBy
  , headSort
  , lastSort
  ]

-- | concat (map f xs) ==> concatMap f xs
concatMapRule :: Rule
concatMapRule =
  rule "concat-map" $
    match ("concat (map _f _xs)" ==> "concatMap _f _xs")
    & category Performance
    & severity Suggestion
    & message "Use concatMap instead of concat (map ...)"

-- | concat [[x]] ==> [x]
concatSingleton :: Rule
concatSingleton =
  rule "concat-singleton" $
    match ("concat [[_x]]" ==> "[_x]")
    & category Style
    & severity Suggestion
    & message "concat of singleton list is the element"

-- | concat [xs] ==> xs
concatSingleList :: Rule
concatSingleList =
  rule "concat-single-list" $
    match ("concat [_xs]" ==> "_xs")
    & category Style
    & severity Suggestion
    & message "concat of single list is identity"

-- | head (reverse xs) ==> last xs
headReverse :: Rule
headReverse =
  rule "head-reverse" $
    match ("head (reverse _xs)" ==> "last _xs")
    & category Performance
    & severity Suggestion
    & message "head . reverse is last"

-- | last (reverse xs) ==> head xs
lastReverse :: Rule
lastReverse =
  rule "last-reverse" $
    match ("last (reverse _xs)" ==> "head _xs")
    & category Performance
    & severity Suggestion
    & message "last . reverse is head"

-- | take n (repeat x) ==> replicate n x
takeRepeat :: Rule
takeRepeat =
  rule "take-repeat" $
    match ("take _n (repeat _x)" ==> "replicate _n _x")
    & category Style
    & severity Suggestion
    & message "take n (repeat x) is replicate n x"

-- | map f (repeat x) ==> repeat (f x)
mapRepeat :: Rule
mapRepeat =
  rule "map-repeat" $
    match ("map _f (repeat _x)" ==> "repeat (_f _x)")
    & category Style
    & severity Suggestion
    & message "map over repeat can be simplified"

-- | map f (replicate n x) ==> replicate n (f x)
mapReplicate :: Rule
mapReplicate =
  rule "map-replicate" $
    match ("map _f (replicate _n _x)" ==> "replicate _n (_f _x)")
    & category Performance
    & severity Suggestion
    & message "map over replicate can be simplified"

-- | length xs == 0 ==> null xs
lengthZero :: Rule
lengthZero =
  rule "length-zero" $
    match ("length _xs == 0" ==> "null _xs")
    & category Performance
    & severity Warning
    & message "Use null instead of length == 0"

-- | length xs /= 0 ==> not (null xs)
lengthNotZero :: Rule
lengthNotZero =
  rule "length-not-zero" $
    match ("length _xs /= 0" ==> "not (null _xs)")
    & category Performance
    & severity Warning
    & message "Use not (null xs) instead of length /= 0"

-- | length xs > 0 ==> not (null xs)
lengthGtZero :: Rule
lengthGtZero =
  rule "length-gt-zero" $
    match ("length _xs > 0" ==> "not (null _xs)")
    & category Performance
    & severity Warning
    & message "Use not (null xs) instead of length > 0"

-- | length xs < 1 ==> null xs
lengthLtOne :: Rule
lengthLtOne =
  rule "length-lt-one" $
    match ("length _xs < 1" ==> "null _xs")
    & category Performance
    & severity Warning
    & message "Use null instead of length < 1"

-- | length xs >= 1 ==> not (null xs)
lengthGeOne :: Rule
lengthGeOne =
  rule "length-ge-one" $
    match ("length _xs >= 1" ==> "not (null _xs)")
    & category Performance
    & severity Warning
    & message "Use not (null xs) instead of length >= 1"

-- | 0 == length xs ==> null xs
zeroEqLength :: Rule
zeroEqLength =
  rule "zero-eq-length" $
    match ("0 == length _xs" ==> "null _xs")
    & category Performance
    & severity Warning
    & message "Use null instead of 0 == length"

-- | 0 /= length xs ==> not (null xs)
zeroNeLength :: Rule
zeroNeLength =
  rule "zero-ne-length" $
    match ("0 /= length _xs" ==> "not (null _xs)")
    & category Performance
    & severity Warning
    & message "Use not (null xs) instead of 0 /= length"

-- | head (drop n xs) ==> xs !! n
headDrop :: Rule
headDrop =
  rule "head-drop" $
    match ("head (drop _n _xs)" ==> "_xs !! _n")
    & category Style
    & severity Suggestion
    & message "head (drop n xs) is xs !! n"
    & note "Both are partial - consider safe alternatives"

-- | head (sortBy f xs) ==> minimumBy f xs
headSortBy :: Rule
headSortBy =
  rule "head-sortBy" $
    match ("head (sortBy _f _xs)" ==> "minimumBy _f _xs")
    & category Performance
    & severity Suggestion
    & message "head . sortBy is minimumBy"

-- | last (sortBy f xs) ==> maximumBy f xs
lastSortBy :: Rule
lastSortBy =
  rule "last-sortBy" $
    match ("last (sortBy _f _xs)" ==> "maximumBy _f _xs")
    & category Performance
    & severity Suggestion
    & message "last . sortBy is maximumBy"

-- | head (sort xs) ==> minimum xs
headSort :: Rule
headSort =
  rule "head-sort" $
    match ("head (sort _xs)" ==> "minimum _xs")
    & category Performance
    & severity Suggestion
    & message "head . sort is minimum"

-- | last (sort xs) ==> maximum xs
lastSort :: Rule
lastSort =
  rule "last-sort" $
    match ("last (sort _xs)" ==> "maximum _xs")
    & category Performance
    & severity Suggestion
    & message "last . sort is maximum"

--------------------------------------------------------------------------------
-- List Comprehensions
--------------------------------------------------------------------------------

-- | Rules for list comprehensions.
listComprehensionRules :: [Rule]
listComprehensionRules =
  [ listCompId
  , listCompMap
  , listCompFilter
  , listCompMapFilter
  , listCompProduct
  , listCompTrue
  , listCompFalse
  , listCompConcat
  , listCompVoid
  ]

-- | [x | x <- xs] ==> xs
listCompId :: Rule
listCompId =
  rule "list-comp-id" $
    matchText "\\[\\s*([a-z_][a-z0-9_]*)\\s*\\|\\s*\\1\\s*<-\\s*[^]]+\\s*\\]"
    & category Style
    & severity Suggestion
    & message "[x | x <- xs] is just xs"

-- | [f x | x <- xs] ==> map f xs
listCompMap :: Rule
listCompMap =
  rule "list-comp-map" $
    matchText "\\[[^|]+\\s*\\|\\s*[a-z_]+\\s*<-"
    & category Style
    & severity Info
    & message "List comprehension could be map"
    & safetyLevel ManualReview

-- | [x | x <- xs, p x] ==> filter p xs
listCompFilter :: Rule
listCompFilter =
  rule "list-comp-filter" $
    matchText "\\[\\s*[a-z_]+\\s*\\|\\s*[a-z_]+\\s*<-[^,]+,\\s*[^]]+\\]"
    & category Style
    & severity Info
    & message "List comprehension could be filter"
    & safetyLevel ManualReview

-- | [f x | x <- xs, p x] ==> map f (filter p xs)
listCompMapFilter :: Rule
listCompMapFilter =
  rule "list-comp-map-filter" $
    matchText "\\[[^|]+\\|[^,]+<-[^,]+,[^]]+\\]"
    & category Style
    & severity Info
    & message "List comprehension could be map . filter"
    & safetyLevel ManualReview

-- | [(a,b) | a <- xs, b <- ys] ==> liftA2 (,) xs ys
listCompProduct :: Rule
listCompProduct =
  rule "list-comp-product" $
    matchText "\\[\\s*\\([^)]+\\)\\s*\\|[^,]+<-[^,]+,[^,]+<-[^]]+\\]"
    & category Style
    & severity Info
    & message "Cartesian product can use liftA2"
    & safetyLevel ManualReview

-- | [x | x <- xs, True] ==> xs
listCompTrue :: Rule
listCompTrue =
  rule "list-comp-true" $
    matchText "\\[[^|]+\\|[^,]+,\\s*True\\s*\\]"
    & category Style
    & severity Suggestion
    & message "Guard True is always satisfied"

-- | [x | x <- xs, False] ==> []
listCompFalse :: Rule
listCompFalse =
  rule "list-comp-false" $
    matchText "\\[[^|]+\\|[^,]+,\\s*False\\s*\\]"
    & category Style
    & severity Warning
    & message "Guard False makes comprehension empty"

-- | concat list comprehension pattern.
listCompConcat :: Rule
listCompConcat =
  rule "list-comp-concat" $
    matchText "concat\\s*\\["
    & category Style
    & severity Info
    & message "Consider concatMap instead of concat with comprehension"
    & safetyLevel ManualReview

-- | [() | _ <- xs] ==> void xs
listCompVoid :: Rule
listCompVoid =
  rule "list-comp-void" $
    matchText "\\[\\s*\\(\\)\\s*\\|\\s*_\\s*<-"
    & category Style
    & severity Suggestion
    & message "[() | _ <- xs] can be void xs"

--------------------------------------------------------------------------------
-- List Construction
--------------------------------------------------------------------------------

-- | Rules for list construction.
listConstructionRules :: [Rule]
listConstructionRules =
  [ singletonCons
  , doubleCons
  , emptyAppend
  , appendEmpty
  , singletonAppend
  , appendSingletonWarn
  , prependPartial
  , cycleReplicate
  , intercalateSpace
  , intercalateNewline
  ]

-- | x : [] ==> [x]
singletonCons :: Rule
singletonCons =
  rule "singleton-cons" $
    match ("_x : []" ==> "[_x]")
    & category Style
    & severity Suggestion
    & message "Use list literal instead of cons with empty"

-- | x : y : [] ==> [x, y]
doubleCons :: Rule
doubleCons =
  rule "double-cons" $
    match ("_x : _y : []" ==> "[_x, _y]")
    & category Style
    & severity Suggestion
    & message "Use list literal instead of multiple cons"

-- | [] ++ xs ==> xs
emptyAppend :: Rule
emptyAppend =
  rule "empty-append" $
    match ("[] ++ _xs" ==> "_xs")
    & category Style
    & severity Suggestion
    & message "Appending to empty list is identity"

-- | xs ++ [] ==> xs
appendEmpty :: Rule
appendEmpty =
  rule "append-empty" $
    match ("_xs ++ []" ==> "_xs")
    & category Style
    & severity Suggestion
    & message "Appending empty list is identity"

-- | [x] ++ xs ==> x : xs
singletonAppend :: Rule
singletonAppend =
  rule "singleton-append" $
    match ("[_x] ++ _xs" ==> "_x : _xs")
    & category Performance
    & severity Suggestion
    & message "Use cons instead of singleton append"

-- | xs ++ [x] warning about O(n).
appendSingletonWarn :: Rule
appendSingletonWarn =
  rule "append-singleton-warn" $
    matchText "[a-z_]+\\s*\\+\\+\\s*\\[[^]]+\\]"
    & category Performance
    & severity Info
    & message "Appending singleton is O(n)"
    & note "Consider using a different data structure"
    & safetyLevel ManualReview

-- | (++) [x] ==> (x :)
prependPartial :: Rule
prependPartial =
  rule "prepend-partial" $
    match ("(++) [_x]" ==> "(_x :)")
    & category Style
    & severity Suggestion
    & message "Partial application of (++) with singleton"

-- | take n (cycle [x]) ==> replicate n x
cycleReplicate :: Rule
cycleReplicate =
  rule "cycle-replicate" $
    match ("take _n (cycle [_x])" ==> "replicate _n _x")
    & category Performance
    & severity Suggestion
    & message "take n (cycle [x]) is replicate n x"

-- | intercalate " " xs ==> unwords xs
intercalateSpace :: Rule
intercalateSpace =
  rule "intercalate-space" $
    match ("intercalate \" \" _xs" ==> "unwords _xs")
    & category Style
    & severity Suggestion
    & message "Use unwords instead of intercalate \" \""

-- | intercalate "\n" xs ==> unlines xs
intercalateNewline :: Rule
intercalateNewline =
  rule "intercalate-newline" $
    match ("intercalate \"\\n\" _xs" ==> "unlines _xs")
    & category Style
    & severity Suggestion
    & message "Use unlines instead of intercalate \"\\n\""

--------------------------------------------------------------------------------
-- List Indexing
--------------------------------------------------------------------------------

-- | Rules for list indexing.
listIndexingRules :: [Rule]
listIndexingRules =
  [ indexZero
  , takeOneWarn
  , dropOneWarn
  , takeDropSlice
  , indexLast
  , takeLengthMinusOne
  , dropLength
  , splitAtZero
  , splitAtEmpty
  ]

-- | (!!) xs 0 ==> head xs
indexZero :: Rule
indexZero =
  rule "index-zero" $
    match ("_xs !! 0" ==> "head _xs")
    & category Style
    & severity Suggestion
    & message "xs !! 0 is head xs"
    & note "Both are partial"

-- | take 1 xs - suggest listToMaybe.
takeOneWarn :: Rule
takeOneWarn =
  rule "take-one-warn" $
    match ("take 1 _xs" ==> "take 1 _xs")
    & category Style
    & severity Info
    & message "Consider listToMaybe for safe first element"
    & safetyLevel ManualReview

-- | drop 1 xs - suggest tail alternative.
dropOneWarn :: Rule
dropOneWarn =
  rule "drop-one-warn" $
    match ("drop 1 _xs" ==> "drop 1 _xs")
    & category Style
    & severity Info
    & message "drop 1 is like tail but safe for empty lists"
    & safetyLevel ManualReview

-- | take n (drop m xs) slice pattern.
takeDropSlice :: Rule
takeDropSlice =
  rule "take-drop-slice" $
    match ("take _n (drop _m _xs)" ==> "take _n (drop _m _xs)")
    & category Style
    & severity Info
    & message "Consider slice function or Vector for frequent slicing"
    & safetyLevel ManualReview

-- | xs !! (length xs - 1) ==> last xs
indexLast :: Rule
indexLast =
  rule "index-last" $
    match ("_xs !! (length _xs - 1)" ==> "last _xs")
    & category Performance
    & severity Suggestion
    & message "Use last instead of indexing at length - 1"

-- | take (length xs - 1) xs ==> init xs
takeLengthMinusOne :: Rule
takeLengthMinusOne =
  rule "take-length-minus-one" $
    match ("take (length _xs - 1) _xs" ==> "init _xs")
    & category Performance
    & severity Suggestion
    & message "Use init instead of take (length - 1)"

-- | drop (length xs) ys warning.
dropLength :: Rule
dropLength =
  rule "drop-length" $
    matchText "drop\\s*\\(\\s*length\\s+([a-z_]+)\\s*\\)\\s*\\1"
    & category Style
    & severity Suggestion
    & message "drop (length xs) xs is always []"

-- | splitAt 0 xs ==> ([], xs)
splitAtZero :: Rule
splitAtZero =
  rule "splitAt-zero" $
    match ("splitAt 0 _xs" ==> "([], _xs)")
    & category Style
    & severity Suggestion
    & message "splitAt 0 returns ([], xs)"

-- | splitAt n [] ==> ([], [])
splitAtEmpty :: Rule
splitAtEmpty =
  rule "splitAt-empty" $
    match ("splitAt _n []" ==> "([], [])")
    & category Style
    & severity Suggestion
    & message "splitAt on empty list returns ([], [])"

--------------------------------------------------------------------------------
-- List Transforms
--------------------------------------------------------------------------------

-- | Rules for list transformations.
listTransformRules :: [Rule]
listTransformRules =
  [ filterTrue
  , filterFalse
  , takeZero
  , dropZero
  , zipSame
  , zipWithConst
  , replicateZero
  , cycleEmpty
  , nubOrd
  ]

-- | filter (const True) xs ==> xs
filterTrue :: Rule
filterTrue =
  rule "filter-true" $
    match ("filter (const True) _xs" ==> "_xs")
    & category Style
    & severity Suggestion
    & message "filter (const True) is identity"

-- | filter (const False) xs ==> []
filterFalse :: Rule
filterFalse =
  rule "filter-false" $
    match ("filter (const False) _xs" ==> "[]")
    & category Style
    & severity Suggestion
    & message "filter (const False) returns empty list"

-- | take 0 xs ==> []
takeZero :: Rule
takeZero =
  rule "take-zero" $
    match ("take 0 _xs" ==> "[]")
    & category Style
    & severity Suggestion
    & message "take 0 always returns []"

-- | drop 0 xs ==> xs
dropZero :: Rule
dropZero =
  rule "drop-zero" $
    match ("drop 0 _xs" ==> "_xs")
    & category Style
    & severity Suggestion
    & message "drop 0 is identity"

-- | zip xs xs pattern.
zipSame :: Rule
zipSame =
  rule "zip-same" $
    matchText "zip\\s+([a-z_]+)\\s+\\1\\b"
    & category Style
    & severity Info
    & message "zip xs xs creates pairs of same element"
    & note "Consider map (\\x -> (x, x)) or join (,)"
    & safetyLevel ManualReview

-- | zipWith const xs ys ==> take (length ys) xs
zipWithConst :: Rule
zipWithConst =
  rule "zipWith-const" $
    match ("zipWith const _xs _ys" ==> "take (length _ys) _xs")
    & category Style
    & severity Suggestion
    & message "zipWith const just takes from first list"

-- | replicate 0 x ==> []
replicateZero :: Rule
replicateZero =
  rule "replicate-zero" $
    match ("replicate 0 _x" ==> "[]")
    & category Style
    & severity Suggestion
    & message "replicate 0 always returns []"

-- | cycle [] warning.
cycleEmpty :: Rule
cycleEmpty =
  rule "cycle-empty" $
    match ("cycle []" ==> "cycle []")
    & category Correctness
    & severity Error
    & message "cycle [] is an error - empty list has no elements to cycle"

-- | nub with Ord constraint.
nubOrd :: Rule
nubOrd =
  rule "nub-ord" $
    matchText "\\bnub\\b"
    & category Performance
    & severity Suggestion
    & message "nub is O(n^2)"
    & note "Consider nubOrd from containers or Set.toList . Set.fromList for Ord types"
