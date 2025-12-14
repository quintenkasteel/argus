{-# LANGUAGE OverloadedStrings #-}

module CorrectnessExamples where

import Data.List (foldl')
--------------------------------------------------------------------------------
-- Division by zero
--------------------------------------------------------------------------------

-- Should error: division by zero
divByZero :: Int -> Int
divByZero x = x `div` 0

-- Should error: mod by zero
modByZero :: Int -> Int
modByZero x = x `mod` 0

-- Safe: division with guard
safeDivision :: Int -> Int -> Maybe Int
safeDivision _ 0 = Nothing
safeDivision x y = Just (x `div` y)

--------------------------------------------------------------------------------
-- Infinite recursion
--------------------------------------------------------------------------------

-- Should error: obvious infinite recursion
infiniteLoop :: Int -> Int
infiniteLoop x = infiniteLoop x

-- Should error: mutual infinite recursion
mutualA :: Int -> Int
mutualA x = mutualB x

mutualB :: Int -> Int
mutualB x = mutualA x

-- Safe: recursion with base case
safeRecursion :: Int -> Int
safeRecursion 0 = 0
safeRecursion n = 1 + safeRecursion (n - 1)

--------------------------------------------------------------------------------
-- Unreachable code
--------------------------------------------------------------------------------

-- Should warn: unreachable code after error
unreachableAfterError :: Int -> Int
unreachableAfterError x =
  if x < 0
    then error "negative"
    else x
  where
    _ = x + 1  -- unreachable if error was thrown

-- Should warn: unreachable pattern
unreachablePattern :: Bool -> Int
unreachablePattern b = case b of
  True -> 1
  False -> 2
  _ -> 3  -- unreachable

--------------------------------------------------------------------------------
-- Always true/false conditions
--------------------------------------------------------------------------------

-- Should warn: always true
alwaysTrue :: Int -> Bool
alwaysTrue x = x == x

-- Should warn: always false
alwaysFalse :: Int -> Bool
alwaysFalse x = x /= x

-- Should warn: redundant comparison
redundantComparison :: Int -> Bool
redundantComparison x = x > x

--------------------------------------------------------------------------------
-- Empty list operations
--------------------------------------------------------------------------------

-- Should warn: head of potentially empty list
headEmpty :: [Int] -> Int
headEmpty xs = if null xs then 0 else head (filter (> 0) xs)

-- Should warn: last of potentially empty list
lastEmpty :: [Int] -> Int
lastEmpty xs = if null xs then 0 else last (filter (> 0) xs)

--------------------------------------------------------------------------------
-- Type coercion issues
--------------------------------------------------------------------------------

-- Should warn: lossy conversion
lossyConversion :: Double -> Int
lossyConversion x = truncate x  -- loses precision

-- Should warn: overflow possibility
overflowPossible :: Integer -> Int
overflowPossible x = fromInteger x  -- may overflow

--------------------------------------------------------------------------------
-- Strictness issues
--------------------------------------------------------------------------------

-- Should warn: non-strict fold can stack overflow
nonStrictFold :: [Int] -> Int
nonStrictFold = foldl' (+) 0

-- Better: strict fold
strictFold :: [Int] -> Int
strictFold = foldl' (+) 0
  where foldl' f z [] = z
        foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

--------------------------------------------------------------------------------
-- Partial pattern matching
--------------------------------------------------------------------------------

-- Should warn: partial pattern
partialPattern :: Either String Int -> Int
partialPattern (Right n) = n  -- missing Left case

-- Complete pattern
completePattern :: Either String Int -> Int
completePattern (Left _) = 0
completePattern (Right n) = n

--------------------------------------------------------------------------------
-- Redundant operations
--------------------------------------------------------------------------------

-- Should warn: self-subtraction always zero
selfSubtract :: Int -> Int
selfSubtract x = x - x

-- Should warn: self-division always one
selfDivide :: Double -> Double
selfDivide x = x / x  -- also undefined for x=0

--------------------------------------------------------------------------------
-- Logic errors
--------------------------------------------------------------------------------

-- Should warn: condition can never be satisfied
impossibleCondition :: Int -> Bool
impossibleCondition x = x > 10 && x < 5

-- Should warn: tautology
tautology :: Int -> Bool
tautology x = x > 10 || x <= 10
