{-# LANGUAGE ImportQualifiedPost #-}

module FoldablePatterns where

import Data.Foldable (fold, foldMap, foldr, foldl, foldl', any, all, sum, product, length, null, elem, notElem, maximum, minimum, maximumBy, minimumBy, traverse_, for_, sequenceA_)
import Data.Traversable (traverse, for, sequenceA)
import Data.Functor (void, (<$>))
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Map qualified as Map
import Control.Monad (mapM, mapM_, forM, forM_, sequence, sequence_)
import Data.Ord (comparing)

--------------------------------------------------------------------------------
-- Fold Optimization Patterns
--------------------------------------------------------------------------------

-- Should use foldl' instead of foldl (space leak)
sumWithFoldl :: Num a => [a] -> a
sumWithFoldl xs = foldl (+) 0 xs

-- Should use foldl' instead of foldl
productWithFoldl :: Num a => [a] -> a
productWithFoldl xs = foldl (*) 1 xs

-- foldr with commutative operation could use foldl'
sumWithFoldr :: Num a => [a] -> a
sumWithFoldr xs = foldr (+) 0 xs

-- Should use fold instead of foldMap id
foldIdentity :: Monoid a => [a] -> a
foldIdentity xs = foldMap id xs

-- Should use foldMap instead of concat . map
flattenMap :: (a -> [b]) -> [a] -> [b]
flattenMap f xs = concat (map f xs)

-- Should use or instead of foldr (||) False
anyWithFoldr :: [Bool] -> Bool
anyWithFoldr xs = foldr (||) False xs

-- Should use and instead of foldr (&&) True
allWithFoldr :: [Bool] -> Bool
allWithFoldr xs = foldr (&&) True xs

-- Should use sum instead of foldl' (+) 0
manualSum :: Num a => [a] -> a
manualSum xs = foldl' (+) 0 xs

-- Should use product instead of foldl' (*) 1
manualProduct :: Num a => [a] -> a
manualProduct xs = foldl' (*) 1 xs

-- Should use length instead of manual counting
countElements :: [a] -> Int
countElements xs = foldr (\_ n -> n + 1) 0 xs

-- Should use null instead of length == 0 (performance)
isEmptyList :: [a] -> Bool
isEmptyList xs = length xs == 0

-- Alternative empty check
isEmptyList2 :: [a] -> Bool
isEmptyList2 xs = 0 == length xs

-- Length comparison with zero
lengthZeroCompare :: [a] -> Bool
lengthZeroCompare xs = length xs > 0

--------------------------------------------------------------------------------
-- Traversal Modernization
--------------------------------------------------------------------------------

-- Should use traverse instead of mapM
processWithMapM :: Monad m => (a -> m b) -> [a] -> m [b]
processWithMapM f xs = mapM f xs

-- Should use for instead of forM
processWithForM :: Monad m => [a] -> (a -> m b) -> m [b]
processWithForM xs f = forM xs f

-- Should use traverse instead of sequenceA . fmap
sequenceFmap :: Applicative f => (a -> f b) -> [a] -> f [b]
sequenceFmap f xs = sequenceA (fmap f xs)

-- Should use traverse_ instead of void . traverse
voidTraverse :: Applicative f => (a -> f b) -> [a] -> f ()
voidTraverse f xs = void (traverse f xs)

-- Should use traverse_ instead of mapM_
processWithMapM_ :: Monad m => (a -> m b) -> [a] -> m ()
processWithMapM_ f xs = mapM_ f xs

-- Should use for_ instead of forM_
processWithForM_ :: Monad m => [a] -> (a -> m b) -> m ()
processWithForM_ xs f = forM_ xs f

-- Should use sequence_ instead of void . sequence
voidSequence :: Monad m => [m a] -> m ()
voidSequence xs = void (sequence xs)

-- Alternative: sequenceA usage
useSequenceA :: Applicative f => (a -> f b) -> [a] -> f [b]
useSequenceA f xs = sequenceA (map f xs)

--------------------------------------------------------------------------------
-- Specialized Container Functions
--------------------------------------------------------------------------------

-- Should use Set.member instead of elem on Set.fromList
elemInSet :: Ord a => a -> [a] -> Bool
elemInSet x xs = elem x (Set.fromList xs)

-- Should use Set.notMember instead of notElem on Set.toList
notElemInSet :: Ord a => a -> Set.Set a -> Bool
notElemInSet x s = notElem x (Set.toList s)

-- Should use maximum instead of foldl1 max
findMaximum :: Ord a => [a] -> a
findMaximum xs = foldl1 max xs

-- Should use minimum instead of foldl1 min
findMinimum :: Ord a => [a] -> a
findMinimum xs = foldl1 min xs

-- Should use maximumBy instead of head . sortBy (O(n) vs O(n log n))
findLargest :: Ord b => (a -> b) -> [a] -> a
findLargest f xs = head (List.sortBy (comparing f) xs)

-- Should use minimumBy instead of last . sortBy
findSmallest :: Ord b => (a -> b) -> [a] -> a
findSmallest f xs = last (List.sortBy (comparing f) xs)

--------------------------------------------------------------------------------
-- List to Foldable Generalization
--------------------------------------------------------------------------------

-- Should use Foldable any instead of Data.List.any
anyFromList :: (a -> Bool) -> [a] -> Bool
anyFromList p xs = List.any p xs

-- Should use Foldable all instead of Data.List.all
allFromList :: (a -> Bool) -> [a] -> Bool
allFromList p xs = List.all p xs

-- Should use Foldable null instead of Data.List.null
nullFromList :: [a] -> Bool
nullFromList xs = List.null xs

-- Should use Foldable length instead of Data.List.length
lengthFromList :: [a] -> Int
lengthFromList xs = List.length xs

-- Should use Foldable elem instead of Data.List.elem
elemFromList :: Eq a => a -> [a] -> Bool
elemFromList x xs = List.elem x xs

-- Should use Foldable sum instead of Data.List.sum
sumFromList :: Num a => [a] -> a
sumFromList xs = List.sum xs

-- Should use Foldable product instead of Data.List.product
productFromList :: Num a => [a] -> a
productFromList xs = List.product xs

-- Should consider fold instead of concat for Foldable
concatLists :: [[a]] -> [a]
concatLists xss = concat xss

--------------------------------------------------------------------------------
-- Complex Realistic Examples
--------------------------------------------------------------------------------

-- Calculate total cost with space leak
calculateTotal :: [Double] -> Double
calculateTotal prices = foldl (+) 0.0 prices

-- Process and count items (inefficient check)
processIfNotEmpty :: [a] -> (a -> b) -> Maybe [b]
processIfNotEmpty items f =
  if length items == 0
    then Nothing
    else Just (map f items)

-- Find best item inefficiently
findBestItem :: Ord b => (a -> b) -> [a] -> Maybe a
findBestItem scorer items =
  if length items > 0
    then Just (head (List.sortBy (comparing scorer) items))
    else Nothing

-- Combine monoids inefficiently
combineResults :: Monoid a => [a] -> a
combineResults xs = foldMap id xs

-- Manual traversal
applyActions :: Monad m => (a -> m b) -> [a] -> m [b]
applyActions action items = mapM action items

-- Check membership in collection
isMember :: Ord a => a -> [a] -> Bool
isMember x collection = elem x (Set.fromList collection)

-- Aggregate with wrong fold
aggregateScores :: [Int] -> Int
aggregateScores scores = foldl (*) 1 scores

-- Count items manually
countItems :: [a] -> Int
countItems items = foldr (\_ acc -> acc + 1) 0 items

-- Check if any condition matches
hasMatchingElement :: (a -> Bool) -> [a] -> Bool
hasMatchingElement predicate xs = foldr (||) False (map predicate xs)

-- Check if all conditions match
allElementsMatch :: (a -> Bool) -> [a] -> Bool
allElementsMatch predicate xs = foldr (&&) True (map predicate xs)

-- Process items with effects and discard results
processAndDiscard :: Monad m => (a -> m b) -> [a] -> m ()
processAndDiscard processor items = void (mapM processor items)

-- Traverse with flipped arguments
forEachItem :: Monad m => [a] -> (a -> m b) -> m [b]
forEachItem items action = forM items action
