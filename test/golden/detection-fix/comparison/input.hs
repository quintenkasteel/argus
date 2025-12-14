{-# LANGUAGE OverloadedStrings #-}

module ComparisonTests where

import Data.Ord (comparing)
import Data.List (sortBy, maximumBy, minimumBy, sortOn, groupBy, group)

-- Self-comparison patterns (always constant)

-- x == x -> True
eqSelfCheck :: Int -> Bool
eqSelfCheck value = value == value

-- x /= x -> False
neSelfCheck :: Int -> Bool
neSelfCheck value = value /= value

-- x < x -> False
ltSelfCheck :: Int -> Bool
ltSelfCheck value = value < value

-- x > x -> False
gtSelfCheck :: Int -> Bool
gtSelfCheck value = value > value

-- x <= x -> True
leSelfCheck :: Int -> Bool
leSelfCheck value = value <= value

-- x >= x -> True
geSelfCheck :: Int -> Bool
geSelfCheck value = value >= value

-- compare x x -> EQ
compareSelfCheck :: Int -> Ordering
compareSelfCheck value = compare value value

-- Compare with ordering patterns

-- compare x y == LT -> x < y
compareLtPattern :: Int -> Int -> Bool
compareLtPattern x y = compare x y == LT

-- compare x y == GT -> x > y
compareGtPattern :: Int -> Int -> Bool
compareGtPattern x y = compare x y == GT

-- compare x y == EQ -> x == y
compareEqPattern :: Int -> Int -> Bool
compareEqPattern x y = compare x y == EQ

-- compare x y /= EQ -> x /= y
compareNePattern :: Int -> Int -> Bool
compareNePattern x y = compare x y /= EQ

-- Negated comparisons

-- not (x == y) -> x /= y
notEqPattern :: Int -> Int -> Bool
notEqPattern x y = not (x == y)

-- not (x /= y) -> x == y
notNePattern :: Int -> Int -> Bool
notNePattern x y = not (x /= y)

-- not (x < y) -> x >= y
notLtPattern :: Int -> Int -> Bool
notLtPattern x y = not (x < y)

-- not (x > y) -> x <= y
notGtPattern :: Int -> Int -> Bool
notGtPattern x y = not (x > y)

-- not (x <= y) -> x > y
notLePattern :: Int -> Int -> Bool
notLePattern x y = not (x <= y)

-- not (x >= y) -> x < y
notGePattern :: Int -> Int -> Bool
notGePattern x y = not (x >= y)

-- Max/Min with same arguments

-- max x x -> x
maxSelfPattern :: Int -> Int
maxSelfPattern value = max value value

-- min x x -> x
minSelfPattern :: Int -> Int
minSelfPattern value = min value value

-- Ordering function simplifications

-- on compare f -> comparing f
onComparePattern :: Ord b => (a -> b) -> a -> a -> Ordering
onComparePattern f = on compare f
  where
    on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
    on g h x y = g (h x) (h y)

-- sortBy (comparing f) -> sortOn f
sortByComparingPattern :: Ord b => (a -> b) -> [a] -> [a]
sortByComparingPattern f = sortBy (comparing f)

-- groupBy (==) -> group
groupByEqPattern :: Eq a => [a] -> [[a]]
groupByEqPattern = groupBy (==)

-- Complex real-world examples

-- Redundant self-comparisons in conditional logic
checkRange :: Int -> Bool
checkRange n = n >= n && n <= 100

-- Multiple compare patterns in guards
classifyOrdering :: Int -> Int -> String
classifyOrdering x y
  | compare x y == LT = "less"
  | compare x y == GT = "greater"
  | compare x y == EQ = "equal"
  | otherwise = "unknown"

-- Negated comparisons in complex expressions
validateBounds :: Int -> Int -> Int -> Bool
validateBounds value minVal maxVal =
  not (value < minVal) && not (value > maxVal)

-- Self-max/min in calculations
normalizeValue :: Int -> Int
normalizeValue n = max n n + min n n

-- Sorting with comparing pattern
sortUsers :: [(String, Int)] -> [(String, Int)]
sortUsers = sortBy (comparing snd)

-- Multiple comparison simplifications in one function
complexComparison :: Int -> Int -> Int -> Bool
complexComparison a b c =
  not (a == b) && compare b c == LT && c <= c

-- Nested negations with comparisons
confusingComparison :: Int -> Int -> Bool
confusingComparison x y = not (not (x < y))

-- Self-comparison in filter predicates
filterValid :: [Int] -> [Int]
filterValid xs = filter (\n -> n == n) xs

-- Compare with redundant ordering check
isLessThan :: Int -> Int -> Bool
isLessThan a b = compare a b == LT

-- Max/min in arithmetic expressions
averageSelf :: Int -> Int
averageSelf x = (max x x + min x x) `div` 2

-- Group by equality (can use group)
groupConsecutive :: Eq a => [a] -> [[a]]
groupConsecutive = groupBy (==)

-- Multiple self-comparisons
multipleSelfChecks :: Int -> Bool
multipleSelfChecks n = n < n || n > n || n /= n

-- Negation of equality in guards
checkNotEqual :: Int -> Int -> String
checkNotEqual x y
  | not (x == y) = "different"
  | otherwise = "same"

-- Complex boolean with comparisons
validateRange :: Int -> Int -> Int -> Bool
validateRange val low high =
  not (val < low) && not (val >= high)

-- Compare patterns in case expressions
describeDifference :: Int -> Int -> String
describeDifference x y = case compare x y of
  ord | ord == LT -> "x is less"
      | ord == GT -> "x is greater"
      | ord == EQ -> "equal"
      | otherwise -> "impossible"

-- Self-comparison with different operators
redundantChecks :: Int -> Bool
redundantChecks n = n <= n && n >= n

-- Sorting by comparing snd
sortBySecond :: [(String, Int)] -> [(String, Int)]
sortBySecond = sortBy (comparing snd)

-- Using maximumBy and minimumBy with comparing
findMaxBy :: Ord b => (a -> b) -> [a] -> a
findMaxBy f = maximumBy (comparing f)

findMinBy :: Ord b => (a -> b) -> [a] -> a
findMinBy f = minimumBy (comparing f)

-- Negated comparison in where clause
isWithinBounds :: Int -> Bool
isWithinBounds x = withinLower && withinUpper
  where
    withinLower = not (x < 0)
    withinUpper = not (x > 100)

-- Compare with /= EQ
isDifferent :: Int -> Int -> Bool
isDifferent a b = compare a b /= EQ

-- Self-equality in complex expression
calculateValue :: Int -> Int -> Int
calculateValue x y = if x == x then x + y else 0

-- Multiple negated comparisons
complexValidation :: Int -> Int -> Bool
complexValidation a b =
  not (a > b) && not (b < a)

-- sortOn pattern already optimal (informational check)
sortByKey :: Ord b => (a -> b) -> [a] -> [a]
sortByKey = sortOn
