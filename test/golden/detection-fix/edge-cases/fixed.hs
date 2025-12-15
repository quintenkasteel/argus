{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE UnicodeSyntax #-}
module EdgeCases where

import Data.List (foldl, foldl')

-- Unicode identifiers with issues
αβγ :: Bool -> Bool
αβγ x = x

-- Multi-line expression with issue
multiLine :: [Int] -> Int
multiLine xs = foldl
  (+)
  0
  xs

-- Pattern guards with issue (returns wrong type intentionally for demo)
patternGuard :: Maybe Int -> Bool
patternGuard mx
  | Just x <- mx, x > 0 = True
  | otherwise = False

-- Nested expressions - multiple foldl issues
nested :: [[Int]] -> Int
nested xss = foldl' (+) 0 (foldl (++) [] xss)

-- Where clause with issue
withWhere :: [Int] -> Int
withWhere xs = result
  where
    result = foldl' (+) 0 xs

-- Let binding with issue
withLet :: [Int] -> Int
withLet xs =
  let total = foldl' (+) 0 xs
  in total

-- Operator section (should not trigger false positives)
addOne :: [Int] -> [Int]
addOne = map (+1)

-- Point-free with issue
pointFree :: [Int] -> Int
pointFree = foldl' (+) 0

-- Complex nested boolean
complexBool :: Bool -> Bool -> Bool
complexBool a b = (not (not a))
