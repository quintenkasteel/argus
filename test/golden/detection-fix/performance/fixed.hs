{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module PerformanceIssues where

import Control.Monad (replicateM)
import Data.List ((++, elem, foldl, foldl', nub, sort)
import qualified Data.Map as Map
import qualified Data.Text.IO as TIO

-- PERFORMANCE ISSUE 1: Using length for emptiness check instead of null
-- Should use null for O(1) check instead of O(n)
checkEmpty :: [a] -> Bool
checkEmpty xs = null xs

checkNonEmpty :: [a] -> Bool
checkNonEmpty xs = not (null xs)

anotherEmptyCheck :: [a] -> Bool
anotherEmptyCheck items = not (null items)

-- PERFORMANCE ISSUE 2: Using ++ in left fold pattern (O(n²))
-- Should use difference lists or foldr
concatenateAll :: [[a]] -> [a]
concatenateAll = foldl' (++) []

-- PERFORMANCE ISSUE 3: Inefficient list operations
-- concat . map should be concatMap
flattenWithMap :: (a -> [b]) -> [a] -> [b]
flattenWithMap f xs = concatMap f xs

-- Alternative form: concat $ map
flattenWithDollar :: (a -> [b]) -> [a] -> [b]
flattenWithDollar f xs = concatMap f xs

-- PERFORMANCE ISSUE 4: mconcat . map should be foldMap
combineWithMap :: Monoid m => (a -> m) -> [a] -> m
combineWithMap f xs = mconcatMap f xs

alternativeCombine :: Monoid m => (a -> m) -> [a] -> m
alternativeCombine f xs = mconcatMap f xs

-- PERFORMANCE ISSUE 5: head . sort is O(n log n), should use minimum O(n)
findSmallest :: Ord a => [a] -> a
findSmallest xs = head (sort xs)

alternativeSmallest :: Ord a => [a] -> a
alternativeSmallest = minimum

-- PERFORMANCE ISSUE 6: last . sort should be maximum
findLargest :: Ord a => [a] -> a
findLargest xs = last (sort xs)

alternativeLargest :: Ord a => [a] -> a
alternativeLargest = maximum

-- PERFORMANCE ISSUE 7: nub is O(n²), should use nubOrd
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates xs = nub xs

-- PERFORMANCE ISSUE 8: Using String instead of Text
-- (Note: In production this would be flagged if Data.Text is not imported)
readConfig :: FilePath -> IO String
readConfig path = TIO.readFile path

processString :: String -> String -> String
processString prefix str = prefix ++ str

-- PERFORMANCE ISSUE 9: Lazy fold should be strict foldl'
sumNumbers :: [Int] -> Int
sumNumbers = foldl' (+) 0

productNumbers :: [Int] -> Int
productNumbers = foldl' (*) 1

-- PERFORMANCE ISSUE 10: Inefficient data structure - using list lookup
-- Should use Map for O(log n) or HashMap for O(1)
lookupValue :: Eq k => k -> [(k, v)] -> Maybe v
lookupValue k pairs = lookup k pairs

-- PERFORMANCE ISSUE 11: Using elem repeatedly (O(n) each time)
-- Should use Set.member for O(log n) or HashSet.member for O(1)
filterAllowed :: [String] -> [String] -> [String]
filterAllowed allowed items = filter (`elem` allowed) items

-- PERFORMANCE ISSUE 12: Chained map operations (allocates intermediate list)
processData :: [Int] -> [String]
processData = map show . map (*2) . map (+1)

-- Alternative form with $
processDataDollar :: [Int] -> [String]
processDataDollar xs = map show $ map (*2) $ map (+1) xs

-- PERFORMANCE ISSUE 13: Chained filter operations
filterPositiveEvens :: [Int] -> [Int]
filterPositiveEvens = filter even . filter (> 0)

-- PERFORMANCE ISSUE 14: reverse . sort should be sortBy (flip compare)
sortDescending :: Ord a => [a] -> [a]
sortDescending xs = reverse (sort xs)

alternativeDescending :: Ord a => [a] -> [a]
alternativeDescending = sortBy (flip compare)

-- PERFORMANCE ISSUE 15: sequence . map should be traverse
collectResults :: Monad m => (a -> m b) -> [a] -> m [b]
collectResults f xs = sequenceA (map f xs)

alternativeCollect :: Monad m => (a -> m b) -> [a] -> m [b]
alternativeCollect f xs = sequenceA $ map f xs

-- PERFORMANCE ISSUE 16: Using lazy Map instead of strict Map
-- Could accumulate thunks
buildMap :: [Int] -> Map.Map Int Int
buildMap xs = Map.fromList [(x, x * 2) | x <- xs]

-- PERFORMANCE ISSUE 17: any id → or
checkAny :: [Bool] -> Bool
checkAny xs = or xs

-- PERFORMANCE ISSUE 18: all id → and
checkAll :: [Bool] -> Bool
checkAll xs = and xs

-- PERFORMANCE ISSUE 19: Redundant operations
-- reverse . reverse is identity
reverseReverse :: [a] -> [a]
reverseReverse = id

-- map id is identity
mapIdentity :: [a] -> [a]
mapIdentity xs = xs

-- xs ++ [] is identity
appendEmpty :: [a] -> [a]
appendEmpty xs = xs ++ []

-- [] ++ xs is identity
prependEmpty :: [a] -> [a]
prependEmpty xs = [] ++ xs

-- PERFORMANCE ISSUE 20: replicateM can consume lots of memory
-- Should use replicateM_ if results not needed
doManyActions :: Monad m => Int -> m a -> m [a]
doManyActions n action = replicateM n action

-- PERFORMANCE ISSUE 21: Using Integer for numeric computations
-- Should use Int, Int64, or Word64 if range is known
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)
