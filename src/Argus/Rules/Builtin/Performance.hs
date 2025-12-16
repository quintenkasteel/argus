{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Performance
-- Description : Performance optimization rules for Haskell code
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- This module provides lint rules for detecting common performance issues
-- and suggesting more efficient alternatives. These rules focus on algorithmic
-- improvements, space leak prevention, and avoiding unnecessary work.
--
-- == Rule Categories
--
-- * __Algorithmic Complexity__: O(n²) to O(n log n) improvements
-- * __Space Leaks__: Strict evaluation where needed
-- * __Redundant Traversals__: Fusing multiple passes
-- * __Efficient Alternatives__: Using specialized functions
--
-- == Important Notes
--
-- Performance rules suggest improvements that may not always apply:
--
-- * Small inputs may not benefit from algorithmic improvements
-- * Some suggestions require additional type class constraints
-- * Profile before optimizing - measure, don't assume
--
-- == References
--
-- * <https://wiki.haskell.org/Performance Haskell Performance Wiki>
-- * <https://www.well-typed.com/blog/2014/09/understanding-the-stack/ Understanding the Stack>
-- * <https://haskell.foundation/hs-opt-handbook.github.io/ Haskell Optimization Handbook>

module Argus.Rules.Builtin.Performance
  ( -- * Rule Sets
    performanceRules
  , algorithmicRules
  , spaceLeakRules
  , fusionRules
  , efficiencyRules

    -- * Individual Rules
    -- ** Algorithmic Improvements
  , avoidNub
  , avoidSortNub
  , preferIntMap
  , preferIntSet
  , preferHashMap
  , preferHashSet

    -- ** Space Leak Prevention
  , preferFoldl'
  , strictState
  , strictWriter

    -- ** Fusion Opportunities
  , useConcatMap
  , useFoldMap

    -- ** Efficiency
  , avoidLengthEq0
  , avoidLengthGt0
  , avoidHeadSort
  , avoidLastSort
  , mapId
  , joinFmap
  , fmapPure
  , sequenceMap
  , reverseReverse
  , filterFusion
  , anyEqElem
  , allNeNotElem
  , sumPair
  , productPair
  , takeLength
  , mapFmapFusion
  , appendNil
  , nilAppend

    -- * Rule Metadata
  , performanceRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All performance rules combined.
--
-- This comprehensive set includes all performance-related suggestions.
-- For production code, consider enabling the full set and addressing
-- high-impact rules first.
performanceRules :: [Rule]
performanceRules = algorithmicRules ++ spaceLeakRules ++ fusionRules ++ efficiencyRules

-- | Rules for algorithmic complexity improvements.
--
-- These rules suggest better data structures or algorithms that improve
-- asymptotic complexity.
algorithmicRules :: [Rule]
algorithmicRules =
  [ avoidNub
  , avoidSortNub
  , preferIntMap
  , preferIntSet
  , preferHashMap
  , preferHashSet
  ]

-- | Rules for preventing space leaks.
--
-- Space leaks occur when thunks accumulate on the heap. These rules
-- suggest strict evaluation patterns where appropriate.
spaceLeakRules :: [Rule]
spaceLeakRules =
  [ preferFoldl'
  , strictState
  , strictWriter
  ]

-- | Rules for fusion opportunities.
--
-- GHC's fusion framework can eliminate intermediate data structures.
-- These rules suggest patterns that fuse well.
fusionRules :: [Rule]
fusionRules =
  [ useConcatMap
  , useFoldMap
  ]

-- | Rules for general efficiency improvements.
efficiencyRules :: [Rule]
efficiencyRules =
  [ avoidLengthEq0
  , avoidLengthGt0
  , avoidHeadSort
  , avoidLastSort
  , mapId
  , joinFmap
  , fmapPure
  , sequenceMap
  , reverseReverse
  , filterFusion
  , anyEqElem
  , allNeNotElem
  , sumPair
  , productPair
  , takeLength
  , mapFmapFusion
  , appendNil
  , nilAppend
  ]

--------------------------------------------------------------------------------
-- Algorithmic Improvements
--------------------------------------------------------------------------------

-- | Detect O(n²) @nub@ and suggest O(n log n) alternative.
--
-- == Problem
--
-- The standard @nub@ function has O(n²) complexity because it compares
-- each element with all previously seen elements:
--
-- @
-- nub [1..10000]  -- Slow! Compares each element with all previous
-- @
--
-- == Solution
--
-- Use @ordNub@ from @containers@ which requires @Ord@ but runs in O(n log n):
--
-- @
-- import Data.Containers.ListUtils (nubOrd)
--
-- nubOrd [1..10000]  -- Fast! Uses Set internally
-- @
--
-- == Trade-offs
--
-- * Requires @Ord@ instance (not just @Eq@)
-- * May change order of elements (use @nubOrdOn fst@ to preserve)
-- * For very small lists, the overhead may not be worth it
--
-- == Metadata
--
-- * __Complexity improvement__: O(n²) → O(n log n)
-- * __Constraint added__: Ord
avoidNub :: Rule
avoidNub = rule "performance/avoid-nub" $
  match ("nub $XS" ==> "nubOrd $XS"
         `fromModule` "Data.List"
         `toModule` "Data.Containers.ListUtils"
         `where_` hasClass "$XS" "Ord")
  & severity Suggestion
  & message "nub is O(n²) - use nubOrd for O(n log n) with Ord constraint"
  & note "nubOrd requires Ord; for Hashable types, consider nubHash"
  & category Performance
  & safetyLevel MostlySafe
  & fixDescription "Replace nub with nubOrd from Data.Containers.ListUtils"

-- | Detect @nub . sort@ anti-pattern.
--
-- == Problem
--
-- @nub . sort@ or @nub $ sort xs@ does redundant work:
--
-- @
-- nub (sort xs)  -- Sorts, then removes duplicates in O(n²)
-- @
--
-- == Solution
--
-- Use @ordNub@ directly, or @sort@ followed by adjacent duplicate removal:
--
-- @
-- -- Best: ordNub already sorts internally
-- ordNub xs
--
-- -- If you need sorted output
-- sort (ordNub xs)
-- @
avoidSortNub :: Rule
avoidSortNub = rule "performance/avoid-sort-nub" $
  match ("nub $ sort $XS" ==> "nubOrd $XS"
         `addImport` ("Data.Containers.ListUtils", ["nubOrd"]))
  & severity Suggestion
  & message "nub after sort is redundant - use nubOrd for O(n log n)"
  & note "nubOrd uses a Set internally and handles duplicates efficiently"
  & category Performance
  & safetyLevel Safe

-- | Suggest IntMap for Int-keyed maps.
--
-- == Problem
--
-- 'Map Int v' uses tree-based storage with O(log n) operations.
--
-- == Solution
--
-- @IntMap@ uses a specialized representation with better constant factors
-- and cache locality.
preferIntMap :: Rule
preferIntMap = rule "performance/prefer-IntMap" $
  match (pat "Map Int"
         `fromModule` "Data.Map")
  & severity Suggestion
  & message "Consider IntMap for Int keys - better cache performance"
  & note "IntMap has specialized representation with smaller constant factors"
  & category Performance
  & safetyLevel ManualReview

-- | Suggest IntSet for Int sets.
preferIntSet :: Rule
preferIntSet = rule "performance/prefer-IntSet" $
  match (pat "Set Int"
         `fromModule` "Data.Set")
  & severity Suggestion
  & message "Consider IntSet for Int elements - better cache performance"
  & note "IntSet has specialized representation with smaller constant factors"
  & category Performance
  & safetyLevel ManualReview

-- | Suggest HashMap for hashable keys.
--
-- == Problem
--
-- 'Map k v' provides O(log n) lookups.
--
-- == Solution
--
-- @HashMap@ provides expected O(1) lookups with good hash functions:
--
-- @
-- import Data.HashMap.Strict (HashMap)
-- import qualified Data.HashMap.Strict as HM
-- @
preferHashMap :: Rule
preferHashMap = rule "performance/prefer-HashMap" $
  match (pat "Map.lookup"
         `fromModule` "Data.Map")
  & severity Info
  & message "For O(1) lookups, consider HashMap with Hashable keys"
  & note "HashMap trades memory for speed; profile to determine benefit"
  & category Performance
  & safetyLevel ManualReview

-- | Suggest HashSet for hashable elements.
preferHashSet :: Rule
preferHashSet = rule "performance/prefer-HashSet" $
  match (pat "Set.member"
         `fromModule` "Data.Set")
  & severity Info
  & message "For O(1) membership, consider HashSet with Hashable elements"
  & note "HashSet trades memory for speed; profile to determine benefit"
  & category Performance
  & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Space Leak Prevention
--------------------------------------------------------------------------------

-- | Prefer strict 'foldl'' over lazy 'foldl'.
--
-- == Problem
--
-- Lazy 'foldl' builds up thunks that can cause stack overflow:
--
-- @
-- foldl (+) 0 [1..1000000]  -- Builds million thunks, then evaluates
-- @
--
-- == Solution
--
-- Use strict 'foldl'' from 'Data.List' or 'Data.Foldable':
--
-- @
-- import Data.List (foldl')
--
-- foldl' (+) 0 [1..1000000]  -- Evaluates as it goes, constant space
-- @
--
-- == When lazy 'foldl' is correct
--
-- * When the combining function is lazy in its first argument
-- * When you need to short-circuit evaluation
-- * When building a lazy data structure
preferFoldl' :: Rule
preferFoldl' = rule "performance/prefer-foldl'" $
  match ("foldl $F $Z $XS" ==> "foldl' $F $Z $XS"
         `fromModule` "Prelude"
         `addImport` ("Data.Foldable", ["foldl'"]))
  & severity Suggestion
  & message "Use foldl' (strict) instead of foldl to avoid space leaks"
  & note "foldl builds thunks; foldl' evaluates strictly. Use foldl only when laziness is needed"
  & category Performance
  & safetyLevel Safe
  & fixDescription "Replace foldl with foldl' for strict evaluation"

-- | Prefer strict State monad.
--
-- == Problem
--
-- The lazy @State@ monad can accumulate thunks in the state:
--
-- @
-- import Control.Monad.State
--
-- -- This can build up thunks
-- runState (replicateM 1000000 (modify (+1))) 0
-- @
--
-- == Solution
--
-- Use 'Control.Monad.State.Strict':
--
-- @
-- import Control.Monad.State.Strict
-- @
strictState :: Rule
strictState = rule "performance/strict-State" $
  match ("import Control.Monad.State " ==> "import Control.Monad.State.Strict")
  & severity Suggestion
  & message "Consider Control.Monad.State.Strict to avoid space leaks"
  & note "Lazy State can accumulate thunks; Strict evaluates state eagerly"
  & category Performance
  & safetyLevel MostlySafe

-- | Prefer strict Writer monad.
--
-- == Problem
--
-- The lazy @Writer@ monad accumulates thunks in the log.
--
-- == Solution
--
-- Use @Control.Monad.Writer.Strict@, or better yet, @Accum@ or @WriterT@.
strictWriter :: Rule
strictWriter = rule "performance/strict-Writer" $
  match ("import Control.Monad.Writer " ==> "import Control.Monad.Writer.Strict")
  & severity Warning
  & message "Control.Monad.Writer is notoriously leaky - use Strict variant"
  & note "Even Strict Writer can leak; consider WriterT CPS or Accum instead"
  & category Performance
  & safetyLevel MostlySafe

--------------------------------------------------------------------------------
-- Fusion Opportunities
--------------------------------------------------------------------------------

-- | Fuse concat with map into concatMap.
--
-- == Problem
--
-- @concat . map@ creates an intermediate list:
--
-- @
-- concat (map f xs)  -- Creates intermediate [[a]]
-- @
--
-- == Solution
--
-- Use 'concatMap' which fuses away the intermediate list:
--
-- @
-- concatMap f xs  -- No intermediate list
-- @
useConcatMap :: Rule
useConcatMap = rule "performance/concatMap" $
  match ("concat $ map $F $XS" ==> "concatMap $F $XS")
  & severity Suggestion
  & message "Use concatMap instead of concat . map to avoid intermediate list"
  & note "concatMap fuses with list production, eliminating allocation"
  & category Performance
  & safetyLevel Safe

-- | Fuse mconcat with map into foldMap.
--
-- == Problem
--
-- @mconcat . map@ creates an intermediate list:
--
-- @
-- mconcat (map f xs)  -- Creates intermediate [m]
-- @
--
-- == Solution
--
-- Use 'foldMap' which fuses and works with any 'Foldable':
--
-- @
-- import Data.Foldable (foldMap)
--
-- foldMap f xs  -- Direct fold, no intermediate structure
-- @
useFoldMap :: Rule
useFoldMap = rule "performance/foldMap" $
  match ("mconcat $ map $F $XS" ==> "foldMap $F $XS"
         `addImport` ("Data.Foldable", ["foldMap"]))
  & severity Suggestion
  & message "Use foldMap instead of mconcat . map"
  & note "foldMap combines mapping and folding without intermediate list"
  & category Performance
  & safetyLevel Safe

--------------------------------------------------------------------------------
-- Efficiency
--------------------------------------------------------------------------------

-- | Avoid @length xs == 0@ for emptiness check.
--
-- == Problem
--
-- @length xs == 0@ traverses the entire list just to check emptiness:
--
-- @
-- length [1..1000000] == 0  -- Traverses entire list: O(n)
-- @
--
-- == Solution
--
-- Use 'null' for O(1) emptiness check:
--
-- @
-- null [1..1000000]  -- Checks first element only: O(1)
-- @
avoidLengthEq0 :: Rule
avoidLengthEq0 = rule "performance/avoid-length-0" $
  match ("length $XS == 0" ==> "null $XS")
  & severity Suggestion
  & message "Use null for O(1) emptiness check instead of length"
  & note "length traverses entire list; null just checks for first element"
  & category Performance
  & safetyLevel Safe

-- | Avoid @length xs > 0@ for non-emptiness check.
avoidLengthGt0 :: Rule
avoidLengthGt0 = rule "performance/avoid-length-gt-0" $
  match ("length $XS > 0" ==> "not (null $XS)")
  & severity Suggestion
  & message "Use 'not (null xs)' for O(1) non-emptiness check"
  & note "length traverses entire list; null just checks for first element"
  & category Performance
  & safetyLevel Safe

-- | Avoid @head (sort xs)@ - use minimum.
--
-- == Problem
--
-- @head (sort xs)@ sorts the entire list just to get the smallest:
--
-- @
-- head (sort xs)  -- O(n log n) when O(n) suffices
-- @
--
-- == Solution
--
-- Use 'minimum' for O(n) finding of smallest element:
--
-- @
-- minimum xs  -- Single pass: O(n)
-- @
avoidHeadSort :: Rule
avoidHeadSort = rule "performance/avoid-head-sort" $
  match ("head $ sort $XS" ==> "minimum $XS")
  & severity Suggestion
  & message "Use minimum instead of head . sort for O(n) vs O(n log n)"
  & note "minimum finds smallest in one pass without full sort"
  & category Performance
  & safetyLevel MostlySafe

-- | Avoid @last (sort xs)@ - use maximum.
avoidLastSort :: Rule
avoidLastSort = rule "performance/avoid-last-sort" $
  match ("last $ sort $XS" ==> "maximum $XS")
  & severity Suggestion
  & message "Use maximum instead of last . sort for O(n) vs O(n log n)"
  & note "maximum finds largest in one pass without full sort"
  & category Performance
  & safetyLevel MostlySafe

-- | Detect redundant @map id@.
--
-- @map id xs@ is just @xs@ - remove the unnecessary traversal.
mapId :: Rule
mapId = rule "performance/map-id" $
  match ("map id $XS" ==> "$XS")
  & severity Suggestion
  & message "map id is redundant - just use the list directly"
  & category Performance
  & safetyLevel Safe

-- | Simplify @join (fmap f x)@ to @x >>= f@.
joinFmap :: Rule
joinFmap = rule "performance/join-fmap" $
  match ("join $ fmap $F $X" ==> "$X >>= $F")
  & severity Suggestion
  & message "Use (>>=) instead of join . fmap"
  & note "x >>= f is the definition of join (fmap f x)"
  & category Performance
  & safetyLevel Safe

-- | Simplify @fmap f (pure x)@ to @pure (f x)@.
fmapPure :: Rule
fmapPure = rule "performance/fmap-pure" $
  match ("fmap $F $ pure $X" ==> "pure ($F $X)")
  & severity Suggestion
  & message "Simplify fmap over pure"
  & note "fmap f (pure x) = pure (f x) by Functor/Applicative laws"
  & category Performance
  & safetyLevel Safe

-- | Simplify @sequence (map f xs)@ to @traverse f xs@.
sequenceMap :: Rule
sequenceMap = rule "performance/sequence-map" $
  match ("sequence $ map $F $XS" ==> "traverse $F $XS")
  & severity Suggestion
  & message "Use traverse instead of sequence . map"
  & note "traverse combines mapping and sequencing in one pass"
  & category Performance
  & safetyLevel Safe

-- | Detect redundant @reverse (reverse xs)@.
--
-- == Problem
--
-- @reverse (reverse xs)@ does two O(n) traversals to produce the original list:
--
-- @
-- reverse (reverse [1,2,3])  -- Two traversals, returns [1,2,3]
-- @
--
-- == Solution
--
-- Just use the original list directly.
reverseReverse :: Rule
reverseReverse = rule "performance/reverse-reverse" $
  match ("reverse $ reverse $XS" ==> "$XS")
  & severity Suggestion
  & message "reverse (reverse xs) is just xs - remove redundant traversals"
  & note "Two reverse operations cancel out"
  & category Performance
  & safetyLevel Safe

-- | Fuse consecutive filter operations.
--
-- == Problem
--
-- @filter p (filter q xs)@ traverses the list twice:
--
-- @
-- filter even (filter (> 0) xs)  -- Two traversals
-- @
--
-- == Solution
--
-- Combine predicates into a single filter:
--
-- @
-- filter (\\x -> x > 0 && even x) xs  -- Single traversal
-- @
filterFusion :: Rule
filterFusion = rule "performance/filter-fusion" $
  match ("filter $P $ filter $Q $XS" ==> "filter (\\x -> $Q x && $P x) $XS")
  & severity Suggestion
  & message "Fuse consecutive filters into one for single traversal"
  & note "filter p . filter q = filter (\\x -> q x && p x)"
  & category Performance
  & safetyLevel MostlySafe

-- | Simplify @any (== x)@ to @elem x@.
--
-- == Problem
--
-- @any (== x) xs@ creates a partial application:
--
-- @
-- any (== 5) [1,2,3,4,5]  -- Works but creates closure
-- @
--
-- == Solution
--
-- Use 'elem' which is specialized for equality:
--
-- @
-- elem 5 [1,2,3,4,5]  -- More direct
-- @
anyEqElem :: Rule
anyEqElem = rule "performance/any-eq-elem" $
  match ("any (== $X) $XS" ==> "elem $X $XS")
  & severity Suggestion
  & message "Use elem instead of any (== x)"
  & note "elem is the standard idiom for membership testing"
  & category Performance
  & safetyLevel Safe

-- | Simplify @all (/= x)@ to @notElem x@.
allNeNotElem :: Rule
allNeNotElem = rule "performance/all-ne-notElem" $
  match ("all (/= $X) $XS" ==> "notElem $X $XS")
  & severity Suggestion
  & message "Use notElem instead of all (/= x)"
  & note "notElem is the standard idiom for non-membership testing"
  & category Performance
  & safetyLevel Safe

-- | Simplify @sum [x, y]@ to @x + y@.
--
-- == Problem
--
-- @sum [x, y]@ creates a list just to add two numbers:
--
-- @
-- sum [a, b]  -- Allocates list, then folds
-- @
--
-- == Solution
--
-- Use direct addition:
--
-- @
-- a + b  -- No allocation
-- @
sumPair :: Rule
sumPair = rule "performance/sum-pair" $
  match ("sum [$X, $Y]" ==> "$X + $Y")
  & severity Suggestion
  & message "Use + directly instead of sum for two values"
  & note "sum is for collections; use + for two known values"
  & category Performance
  & safetyLevel Safe

-- | Simplify @product [x, y]@ to @x * y@.
productPair :: Rule
productPair = rule "performance/product-pair" $
  match ("product [$X, $Y]" ==> "$X * $Y")
  & severity Suggestion
  & message "Use * directly instead of product for two values"
  & note "product is for collections; use * for two known values"
  & category Performance
  & safetyLevel Safe

-- | Simplify @take (length xs)@ to identity.
--
-- == Problem
--
-- @take (length xs) xs@ traverses the list to get length, then takes all:
--
-- @
-- take (length xs) xs  -- Traverses twice, returns same list
-- @
--
-- == Solution
--
-- Just use the list directly:
--
-- @
-- xs  -- No traversal needed
-- @
takeLength :: Rule
takeLength = rule "performance/take-length" $
  match ("take (length $XS) $XS" ==> "$XS")
  & severity Suggestion
  & message "take (length xs) xs is just xs"
  & note "Taking all elements is the same as the original list"
  & category Performance
  & safetyLevel Safe

-- | Fuse @map f (map g xs)@ to @map (f . g) xs@.
--
-- == Problem
--
-- @map f (map g xs)@ traverses the list twice:
--
-- @
-- map show (map (+1) [1,2,3])  -- Two traversals
-- @
--
-- == Solution
--
-- Compose the functions and traverse once:
--
-- @
-- map (show . (+1)) [1,2,3]  -- Single traversal
-- @
--
-- Note: GHC's fusion usually handles this, but explicit composition
-- is clearer and guaranteed to fuse.
mapFmapFusion :: Rule
mapFmapFusion = rule "performance/map-map-fusion" $
  match ("map $F $ map $G $XS" ==> "map ($F . $G) $XS")
  & severity Suggestion
  & message "Fuse map f . map g to map (f . g) for single traversal"
  & note "GHC usually fuses this, but explicit composition is clearer"
  & category Performance
  & safetyLevel Safe

-- | Simplify @xs ++ []@ to @xs@.
appendNil :: Rule
appendNil = rule "performance/append-nil" $
  match ("$XS ++ []" ==> "$XS")
  & severity Suggestion
  & message "xs ++ [] is just xs - remove redundant append"
  & note "Appending empty list is identity"
  & category Performance
  & safetyLevel Safe

-- | Simplify @[] ++ xs@ to @xs@.
nilAppend :: Rule
nilAppend = rule "performance/nil-append" $
  match ("[] ++ $XS" ==> "$XS")
  & severity Suggestion
  & message "[] ++ xs is just xs - remove redundant prepend"
  & note "Prepending empty list is identity"
  & category Performance
  & safetyLevel Safe

--------------------------------------------------------------------------------
-- Metadata
--------------------------------------------------------------------------------

-- | Total number of performance rules.
performanceRuleCount :: Int
performanceRuleCount = length performanceRules
