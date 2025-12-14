{-# LANGUAGE OverloadedStrings #-}

module ContainerIssues where

import Data.List (elem, nub, sort, partition)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

-- CONTAINER ISSUE 1: Using list lookup instead of Map lookup
-- Association lists are O(n), should use Map for O(log n)
findUserAge :: String -> [(String, Int)] -> Maybe Int
findUserAge name userList = lookup name userList

-- Multiple lookups on same association list
getUserInfo :: String -> [(String, (Int, String))] -> Maybe (Int, String)
getUserInfo userId users = lookup userId users

-- CONTAINER ISSUE 2: Using elem on lists instead of Set membership
-- elem is O(n), Set.member is O(log n)
isAllowedUser :: String -> [String] -> Bool
isAllowedUser user allowedUsers = elem user allowedUsers

-- Using elem in a filter (called repeatedly)
filterAllowed :: [String] -> [String] -> [String]
filterAllowed whitelist items = filter (\x -> elem x whitelist) items

-- Using elem with not
isBlocked :: String -> [String] -> Bool
isBlocked user blocklist = not (elem user blocklist)

-- CONTAINER ISSUE 3: Using !! for indexing instead of safer alternatives
-- (!!) is partial and O(n) for lists
getElement :: [a] -> Int -> a
getElement xs index = xs !! index

-- Multiple indexing operations
getElements :: [a] -> (a, a, a)
getElements xs = (xs !! 0, xs !! 5, xs !! 10)

-- CONTAINER ISSUE 4: Using nub which is O(n²)
-- Should use Set for deduplication when order doesn't matter
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates items = nub items

-- Dedup and sort pattern
uniqueSorted :: Ord a => [a] -> [a]
uniqueSorted xs = nub (sort xs)

-- CONTAINER ISSUE 5: Inefficient Map operations
-- Using Map ! which is partial
getMapValue :: Map.Map String Int -> String -> Int
getMapValue m key = m Map.! key

-- Better to use lookup for safety
getMapValueSafe :: Map.Map String Int -> String -> Maybe Int
getMapValueSafe m key = Map.lookup key m

-- CONTAINER ISSUE 6: Converting Set to list just to use elem
-- elem (Set.toList s) is O(n), Set.member is O(log n)
isInSet :: Ord a => a -> Set.Set a -> Bool
isInSet x s = elem x (Set.toList s)

-- Similar pattern with null check
isSetEmpty :: Set.Set a -> Bool
isSetEmpty s = null (Set.toList s)

-- CONTAINER ISSUE 7: Partition with two filters
-- Should use partition instead of filtering twice
splitEvenOdd :: [Int] -> ([Int], [Int])
splitEvenOdd xs = (filter even xs, filter odd xs)

-- CONTAINER ISSUE 8: Using list operations when Set would be better
-- Finding intersection of two lists
findCommon :: Eq a => [a] -> [a] -> [a]
findCommon xs ys = [x | x <- xs, elem x ys]

-- Finding difference
findMissing :: Eq a => [a] -> [a] -> [a]
findMissing xs ys = [x | x <- xs, not (elem x ys)]

-- CONTAINER ISSUE 9: Using Seq.index instead of Seq.lookup
-- Seq.index is partial
getSeqElement :: Seq.Seq a -> Int -> a
getSeqElement s i = Seq.index s i

-- CONTAINER ISSUE 10: List append in a fold (creates O(n²) behavior)
-- Should use proper concatenation or difference lists
combineAll :: [[a]] -> [a]
combineAll lists = foldr (++) [] lists

-- CONTAINER ISSUE 11: Using list reverse when not needed
-- reverse . sort when sortBy would work
sortDescending :: Ord a => [a] -> [a]
sortDescending xs = reverse (sort xs)

-- CONTAINER ISSUE 12: Double reverse (identity)
-- reverse . reverse is just id
doubleReverse :: [a] -> [a]
doubleReverse xs = reverse (reverse xs)

-- CONTAINER ISSUE 13: Map.union usage (informational about left-bias)
-- Map.union is left-biased, may want unionWith
mergeConfigs :: Map.Map String String -> Map.Map String String -> Map.Map String String
mergeConfigs defaults custom = Map.union defaults custom

-- CONTAINER ISSUE 14: Set union with itself
-- Set.union s s is just s
duplicateUnion :: Set.Set Int -> Set.Set Int
duplicateUnion s = Set.union s s

-- CONTAINER ISSUE 15: Set intersection with itself
-- Set.intersection s s is just s
duplicateIntersection :: Set.Set Int -> Set.Set Int
duplicateIntersection s = Set.intersection s s

-- CONTAINER ISSUE 16: Set difference with itself
-- Set.difference s s is Set.empty
emptyDifference :: Set.Set Int -> Set.Set Int
emptyDifference s = Set.difference s s

-- CONTAINER ISSUE 17: Using concat . intersperse
-- Should use intercalate
joinWithSeparator :: String -> [String] -> String
joinWithSeparator sep strs = concat (intersperse sep strs)

-- CONTAINER ISSUE 18: Seq take/drop pair
-- Should use Seq.splitAt
splitSequence :: Int -> Seq.Seq a -> (Seq.Seq a, Seq.Seq a)
splitSequence n s = (Seq.take n s, Seq.drop n s)

-- CONTAINER ISSUE 19: Vector map composition
-- Should fuse map operations
processVector :: [Int] -> [String]
processVector xs = map show (map (*2) (map (+1) xs))

-- CONTAINER ISSUE 20: Using list when Map would be better for lookups
data Config = Config
  { configPort :: Int
  , configHost :: String
  , configTimeout :: Int
  }

-- Searching through a list of configs by key
findConfig :: String -> [(String, Config)] -> Maybe Config
findConfig key configs = lookup key configs

-- Multiple config lookups
getPort :: String -> [(String, Config)] -> Maybe Int
getPort key configs = fmap configPort (lookup key configs)

getHost :: String -> [(String, Config)] -> Maybe String
getHost key configs = fmap configHost (lookup key configs)

-- Helper that should be imported
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs
