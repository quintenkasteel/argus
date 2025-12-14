{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Containers
-- Description : Container and data structure optimization rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for efficient use of containers from the containers package
-- and other common data structures.
--
-- == Rule Categories
--
-- * __List__: List operation optimization
-- * __Map__: Map efficiency patterns
-- * __Set__: Set operation patterns
-- * __Seq__: Sequence patterns
-- * __Vector__: Vector patterns

module Argus.Rules.Builtin.Containers
  ( -- * Rule Sets
    containerRules
  , listContainerRules
  , mapContainerRules
  , setContainerRules
  , seqContainerRules
  , vectorContainerRules

    -- * List Rules
  , listHeadTail
  , listAppendSingleton
  , listReverse
  , listNub
  , listSort
  , listGroup
  , listPartition
  , listIntersperse

    -- * Map Rules
  , mapFromList
  , mapToList
  , mapLookup
  , mapInsert
  , mapDelete
  , mapUnion
  , mapIntersection
  , mapFilter

    -- * Set Rules
  , setFromList
  , setToList
  , setMember
  , setInsert
  , setDelete
  , setUnion
  , setIntersection
  , setDifference

    -- * Seq Rules
  , seqFromList
  , seqIndex
  , seqAppend
  , seqTake
  , seqDrop
  , seqSplitAt
  , seqReverse
  , seqSort

    -- * Vector Rules
  , vectorFromList
  , vectorIndex
  , vectorLength
  , vectorMap
  , vectorFold
  , vectorGenerate
  , vectorSlice
  , vectorFreeze

    -- * Rule Count
  , containerRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All container-related rules.
containerRules :: [Rule]
containerRules = mconcat
  [ listContainerRules
  , mapContainerRules
  , setContainerRules
  , seqContainerRules
  , vectorContainerRules
  ]

-- | Total count of container rules.
containerRuleCount :: Int
containerRuleCount = length containerRules

--------------------------------------------------------------------------------
-- List Rules
--------------------------------------------------------------------------------

-- | Rules for list operations.
listContainerRules :: [Rule]
listContainerRules =
  [ listHeadTail
  , listAppendSingleton
  , listReverse
  , listNub
  , listSort
  , listGroup
  , listPartition
  , listIntersperse
  ]

-- | head and tail are partial.
--
-- @
-- head xs  -- Throws on empty list
-- @
listHeadTail :: Rule
listHeadTail =
  rule "list-head-tail" $
    match ("head _xs" ==> "listToMaybe _xs")
    & category Safety
    & severity Warning
    & message "head/tail are partial - throw on empty list"
    & note "Use listToMaybe, uncons, or pattern matching"

-- | Appending singleton is cons.
--
-- @
-- xs ++ [x]  ==>  xs ++ [x]  (but prefer xs <> [x] or reverse (x : reverse xs))
-- @
listAppendSingleton :: Rule
listAppendSingleton =
  rule "list-append-singleton" $
    match ("_xs ++ [_x]" ==> "_xs ++ [_x]")
    & category Performance
    & severity Info
    & message "Appending singleton to end is O(n)"
    & note "Consider difference lists or Seq for frequent appends"
    & safetyLevel ManualReview

-- | reverse twice is identity.
--
-- @
-- reverse (reverse xs)  ==>  xs
-- @
listReverse :: Rule
listReverse =
  rule "list-reverse-reverse" $
    match ("reverse (reverse _xs)" ==> "_xs")
    & category Style
    & severity Suggestion
    & message "reverse . reverse is identity"

-- | nub is O(n²).
--
-- @
-- nub xs  -- O(n²) for Eq constraint
-- @
listNub :: Rule
listNub =
  rule "list-nub-slow" $
    match ("nub _xs" ==> "nub _xs")
    & category Performance
    & severity Warning
    & message "nub is O(n²) - consider nubOrd or Set.toList . Set.fromList"
    & note "For Ord types, use Data.List.Extra.nubOrd"
    & safetyLevel ManualReview

-- | sort is O(n log n) stable.
--
-- @
-- sort xs  -- Stable O(n log n) sort
-- @
listSort :: Rule
listSort =
  rule "list-sort" $
    match ("sort _xs" ==> "sort _xs")
    & category Style
    & severity Info
    & message "Using list sort - stable O(n log n)"
    & note "Consider sortBy/sortOn for custom comparison"
    & safetyLevel ManualReview

-- | group requires sorted input for unique groups.
--
-- @
-- group xs  -- Groups consecutive equal elements
-- @
listGroup :: Rule
listGroup =
  rule "list-group-sorted" $
    match ("group _xs" ==> "group _xs")
    & category Safety
    & severity Info
    & message "group only groups consecutive equal elements"
    & note "Sort first if you want all equal elements grouped"
    & safetyLevel ManualReview

-- | partition vs filter.
--
-- @
-- (filter p xs, filter (not . p) xs)  ==>  partition p xs
-- @
listPartition :: Rule
listPartition =
  rule "list-partition" $
    match ("(filter _p _xs, filter (not . _p) _xs)" ==> "partition _p _xs")
    & category Performance
    & severity Suggestion
    & message "Use partition instead of two filter passes"

-- | intersperse followed by concat.
--
-- @
-- concat (intersperse sep xs)  ==>  intercalate sep xs
-- @
listIntersperse :: Rule
listIntersperse =
  rule "intersperse-concat" $
    match ("concat (intersperse _sep _xs)" ==> "intercalate _sep _xs")
    & category Style
    & severity Suggestion
    & message "Use intercalate instead of concat . intersperse"

--------------------------------------------------------------------------------
-- Map Rules
--------------------------------------------------------------------------------

-- | Rules for Map operations.
mapContainerRules :: [Rule]
mapContainerRules =
  [ mapFromList
  , mapToList
  , mapLookup
  , mapInsert
  , mapDelete
  , mapUnion
  , mapIntersection
  , mapFilter
  ]

-- | Map.fromList efficiency.
--
-- @
-- Map.fromList pairs  -- O(n log n) from unsorted
-- @
mapFromList :: Rule
mapFromList =
  rule "map-fromList" $
    match ("Map.fromList _pairs" ==> "Map.fromList _pairs")
    & category Performance
    & severity Info
    & message "Map.fromList is O(n log n) - use fromAscList for sorted input"
    & safetyLevel ManualReview

-- | Map.toList ordering.
--
-- @
-- Map.toList m  -- Returns ascending order by key
-- @
mapToList :: Rule
mapToList =
  rule "map-toList" $
    match ("Map.toList _m" ==> "Map.toList _m")
    & category Style
    & severity Info
    & message "Map.toList returns pairs in ascending key order"
    & safetyLevel ManualReview

-- | Map.lookup vs (!).
--
-- @
-- m ! k  -- Throws on missing key
-- @
mapLookup :: Rule
mapLookup =
  rule "map-lookup-safe" $
    match ("_m Map.! _k" ==> "Map.lookup _k _m")
    & category Safety
    & severity Warning
    & message "(!) throws on missing key - use lookup for safe access"

-- | Map.insert overwrites.
--
-- @
-- Map.insert k v m  -- Overwrites existing value
-- @
mapInsert :: Rule
mapInsert =
  rule "map-insert" $
    match ("Map.insert _k _v _m" ==> "Map.insert _k _v _m")
    & category Style
    & severity Info
    & message "Map.insert overwrites existing values"
    & note "Use insertWith to combine values"
    & safetyLevel ManualReview

-- | Map.delete on missing key.
--
-- @
-- Map.delete k m  -- Safe on missing key
-- @
mapDelete :: Rule
mapDelete =
  rule "map-delete" $
    match ("Map.delete _k _m" ==> "Map.delete _k _m")
    & category Style
    & severity Info
    & message "Map.delete is safe on missing keys"
    & safetyLevel ManualReview

-- | Map.union preference.
--
-- @
-- Map.union m1 m2  -- Left-biased on key collision
-- @
mapUnion :: Rule
mapUnion =
  rule "map-union" $
    match ("Map.union _m1 _m2" ==> "Map.union _m1 _m2")
    & category Style
    & severity Info
    & message "Map.union is left-biased - m1 values take precedence"
    & note "Use unionWith to combine values on collision"
    & safetyLevel ManualReview

-- | Map.intersection keeps left values.
--
-- @
-- Map.intersection m1 m2  -- Keeps values from m1
-- @
mapIntersection :: Rule
mapIntersection =
  rule "map-intersection" $
    match ("Map.intersection _m1 _m2" ==> "Map.intersection _m1 _m2")
    & category Style
    & severity Info
    & message "Map.intersection keeps values from first map"
    & note "Use intersectionWith to combine values"
    & safetyLevel ManualReview

-- | Map.filter vs filterWithKey.
--
-- @
-- Map.filter p m  -- Filters by value only
-- @
mapFilter :: Rule
mapFilter =
  rule "map-filter" $
    match ("Map.filter _p _m" ==> "Map.filter _p _m")
    & category Style
    & severity Info
    & message "Map.filter filters by value - use filterWithKey for key access"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Set Rules
--------------------------------------------------------------------------------

-- | Rules for Set operations.
setContainerRules :: [Rule]
setContainerRules =
  [ setFromList
  , setToList
  , setMember
  , setInsert
  , setDelete
  , setUnion
  , setIntersection
  , setDifference
  ]

-- | Set.fromList for deduplication.
--
-- @
-- Set.toList (Set.fromList xs)  -- Dedup and sort
-- @
setFromList :: Rule
setFromList =
  rule "set-fromList-dedup" $
    match ("Set.toList (Set.fromList _xs)" ==> "nubOrd (sort _xs)")
    & category Style
    & severity Info
    & message "Set.toList . Set.fromList deduplicates and sorts"
    & note "Consider if sorting is needed; nubOrd may be faster"
    & safetyLevel ManualReview

-- | Set.toList ordering.
--
-- @
-- Set.toList s  -- Returns ascending order
-- @
setToList :: Rule
setToList =
  rule "set-toList" $
    match ("Set.toList _s" ==> "Set.toList _s")
    & category Style
    & severity Info
    & message "Set.toList returns elements in ascending order"
    & safetyLevel ManualReview

-- | Set.member vs elem.
--
-- @
-- elem x (Set.toList s)  ==>  Set.member x s
-- @
setMember :: Rule
setMember =
  rule "set-member" $
    match ("elem _x (Set.toList _s)" ==> "Set.member _x _s")
    & category Performance
    & severity Warning
    & message "Use Set.member for O(log n) lookup instead of O(n) elem"

-- | Set.insert idempotent.
--
-- @
-- Set.insert x s  -- Idempotent, no effect if present
-- @
setInsert :: Rule
setInsert =
  rule "set-insert" $
    match ("Set.insert _x _s" ==> "Set.insert _x _s")
    & category Style
    & severity Info
    & message "Set.insert is idempotent - safe to call multiple times"
    & safetyLevel ManualReview

-- | Set.delete safe.
--
-- @
-- Set.delete x s  -- Safe on missing element
-- @
setDelete :: Rule
setDelete =
  rule "set-delete" $
    match ("Set.delete _x _s" ==> "Set.delete _x _s")
    & category Style
    & severity Info
    & message "Set.delete is safe on missing elements"
    & safetyLevel ManualReview

-- | Set.union is idempotent.
--
-- @
-- Set.union s s  ==>  s
-- @
setUnion :: Rule
setUnion =
  rule "set-union-self" $
    match ("Set.union _s _s" ==> "_s")
    & category Style
    & severity Suggestion
    & message "Set.union s s is just s"

-- | Set.intersection idempotent.
--
-- @
-- Set.intersection s s  ==>  s
-- @
setIntersection :: Rule
setIntersection =
  rule "set-intersection-self" $
    match ("Set.intersection _s _s" ==> "_s")
    & category Style
    & severity Suggestion
    & message "Set.intersection s s is just s"

-- | Set.difference with self.
--
-- @
-- Set.difference s s  ==>  Set.empty
-- @
setDifference :: Rule
setDifference =
  rule "set-difference-self" $
    match ("Set.difference _s _s" ==> "Set.empty")
    & category Style
    & severity Suggestion
    & message "Set.difference s s is empty"

--------------------------------------------------------------------------------
-- Seq Rules
--------------------------------------------------------------------------------

-- | Rules for Seq operations.
seqContainerRules :: [Rule]
seqContainerRules =
  [ seqFromList
  , seqIndex
  , seqAppend
  , seqTake
  , seqDrop
  , seqSplitAt
  , seqReverse
  , seqSort
  ]

-- | Seq.fromList for efficient append.
--
-- @
-- Seq.fromList xs  -- O(n) conversion
-- @
seqFromList :: Rule
seqFromList =
  rule "seq-fromList" $
    match ("Seq.fromList _xs" ==> "Seq.fromList _xs")
    & category Style
    & severity Info
    & message "Seq.fromList is O(n) - efficient for building sequences"
    & safetyLevel ManualReview

-- | Seq.index partial.
--
-- @
-- Seq.index s i  -- Throws on out of bounds
-- @
seqIndex :: Rule
seqIndex =
  rule "seq-index" $
    match ("Seq.index _s _i" ==> "Seq.lookup _i _s")
    & category Safety
    & severity Warning
    & message "Seq.index throws on out-of-bounds - use lookup for safe access"

-- | Seq append is efficient.
--
-- @
-- s1 >< s2  -- O(log(min(n,m))) concatenation
-- @
seqAppend :: Rule
seqAppend =
  rule "seq-append" $
    match ("_s1 Seq.>< _s2" ==> "_s1 Seq.>< _s2")
    & category Style
    & severity Info
    & message "Seq concatenation (><) is O(log(min(n,m)))"
    & safetyLevel ManualReview

-- | Seq.take safe.
--
-- @
-- Seq.take n s  -- Safe, returns empty if n < 0
-- @
seqTake :: Rule
seqTake =
  rule "seq-take" $
    match ("Seq.take _n _s" ==> "Seq.take _n _s")
    & category Style
    & severity Info
    & message "Seq.take is safe - returns empty seq if n < 0"
    & safetyLevel ManualReview

-- | Seq.drop safe.
--
-- @
-- Seq.drop n s  -- Safe, returns empty if n > length
-- @
seqDrop :: Rule
seqDrop =
  rule "seq-drop" $
    match ("Seq.drop _n _s" ==> "Seq.drop _n _s")
    & category Style
    & severity Info
    & message "Seq.drop is safe - returns empty seq if n > length"
    & safetyLevel ManualReview

-- | Seq.splitAt efficient.
--
-- @
-- (Seq.take n s, Seq.drop n s)  ==>  Seq.splitAt n s
-- @
seqSplitAt :: Rule
seqSplitAt =
  rule "seq-splitAt" $
    match ("(Seq.take _n _s, Seq.drop _n _s)" ==> "Seq.splitAt _n _s")
    & category Performance
    & severity Suggestion
    & message "Use Seq.splitAt instead of take/drop pair"

-- | Seq.reverse is O(n).
--
-- @
-- Seq.reverse s  -- O(n) reversal
-- @
seqReverse :: Rule
seqReverse =
  rule "seq-reverse" $
    match ("Seq.reverse _s" ==> "Seq.reverse _s")
    & category Style
    & severity Info
    & message "Seq.reverse is O(n)"
    & safetyLevel ManualReview

-- | Seq.sort stable.
--
-- @
-- Seq.sort s  -- Stable O(n log n) sort
-- @
seqSort :: Rule
seqSort =
  rule "seq-sort" $
    match ("Seq.sort _s" ==> "Seq.sort _s")
    & category Style
    & severity Info
    & message "Seq.sort is stable O(n log n)"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Vector Rules
--------------------------------------------------------------------------------

-- | Rules for Vector operations.
vectorContainerRules :: [Rule]
vectorContainerRules =
  [ vectorFromList
  , vectorIndex
  , vectorLength
  , vectorMap
  , vectorFold
  , vectorGenerate
  , vectorSlice
  , vectorFreeze
  ]

-- | Vector.fromList overhead.
--
-- @
-- V.fromList xs  -- O(n) conversion
-- @
vectorFromList :: Rule
vectorFromList =
  rule "vector-fromList" $
    match ("V.fromList _xs" ==> "V.fromList _xs")
    & category Style
    & severity Info
    & message "V.fromList is O(n) - use for bulk conversion"
    & safetyLevel ManualReview

-- | Vector indexing.
--
-- @
-- v V.! i  -- Throws on out of bounds
-- @
vectorIndex :: Rule
vectorIndex =
  rule "vector-index" $
    match ("_v V.! _i" ==> "V.indexM _v _i")
    & category Safety
    & severity Warning
    & message "(!) throws on out-of-bounds - use (!?) for safe access"

-- | Vector.length is O(1).
--
-- @
-- V.length v  -- O(1) length query
-- @
vectorLength :: Rule
vectorLength =
  rule "vector-length" $
    match ("V.length _v" ==> "V.length _v")
    & category Style
    & severity Info
    & message "V.length is O(1) for all vector types"
    & safetyLevel ManualReview

-- | Vector.map fusion.
--
-- @
-- V.map f (V.map g v)  ==>  V.map (f . g) v
-- @
vectorMap :: Rule
vectorMap =
  rule "vector-map-fusion" $
    match ("V.map _f (V.map _g _v)" ==> "V.map (_f . _g) _v")
    & category Performance
    & severity Suggestion
    & message "Fuse map operations for better performance"

-- | Vector fold efficiency.
--
-- @
-- V.foldr f z v  -- May fuse with generation
-- @
vectorFold :: Rule
vectorFold =
  rule "vector-fold" $
    match ("V.foldr _f _z _v" ==> "V.foldr _f _z _v")
    & category Style
    & severity Info
    & message "Vector folds may fuse with generation operations"
    & safetyLevel ManualReview

-- | Vector.generate efficient.
--
-- @
-- V.generate n f  -- Efficient index-based generation
-- @
vectorGenerate :: Rule
vectorGenerate =
  rule "vector-generate" $
    match ("V.generate _n _f" ==> "V.generate _n _f")
    & category Style
    & severity Info
    & message "V.generate is efficient for index-based vector creation"
    & safetyLevel ManualReview

-- | Vector.slice shares memory.
--
-- @
-- V.slice i n v  -- O(1) slice, shares memory
-- @
vectorSlice :: Rule
vectorSlice =
  rule "vector-slice" $
    match ("V.slice _i _n _v" ==> "V.slice _i _n _v")
    & category Style
    & severity Info
    & message "V.slice is O(1) but shares memory with original"
    & note "Use V.force to copy if original shouldn't be retained"
    & safetyLevel ManualReview

-- | Vector freeze/thaw.
--
-- @
-- V.freeze mv  -- Copies mutable to immutable
-- @
vectorFreeze :: Rule
vectorFreeze =
  rule "vector-freeze" $
    match ("V.freeze _mv" ==> "V.freeze _mv")
    & category Style
    & severity Info
    & message "V.freeze copies mutable to immutable - use unsafeFreeze for O(1)"
    & note "unsafeFreeze is unsafe if mutable vector is used after"
    & safetyLevel ManualReview
