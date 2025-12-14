{-# LANGUAGE BangPatterns #-}

module ListRecursionExamples where

import Data.List (foldl')

--------------------------------------------------------------------------------
-- Use foldl' instead of manual left recursion
--------------------------------------------------------------------------------

-- Should suggest: use foldl' instead of manual recursion
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- Good: using fold
sumListFold :: [Int] -> Int
sumListFold = foldl' (+) 0

--------------------------------------------------------------------------------
-- Use foldr for right-associative operations
--------------------------------------------------------------------------------

-- Should suggest: use foldr
concatLists :: [[a]] -> [a]
concatLists [] = []
concatLists (x:xs) = x ++ concatLists xs

-- Good: using foldr
concatListsFoldr :: [[a]] -> [a]
concatListsFoldr = concat

--------------------------------------------------------------------------------
-- Non-tail recursive with accumulator pattern
--------------------------------------------------------------------------------

-- Should warn: accumulator not strict, may cause space leak
lengthBad :: [a] -> Int
lengthBad = go 0
  where
    go acc [] = acc
    go acc (_:xs) = go (acc + 1) xs  -- acc not forced

-- Good: strict accumulator
lengthGood :: [a] -> Int
lengthGood = go 0
  where
    go !acc [] = acc
    go !acc (_:xs) = go (acc + 1) xs

--------------------------------------------------------------------------------
-- Use map instead of manual mapping
--------------------------------------------------------------------------------

-- Should suggest: use map
doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x:xs) = (x * 2) : doubleAll xs

-- Good: using map
doubleAllMap :: [Int] -> [Int]
doubleAllMap = map (* 2)

--------------------------------------------------------------------------------
-- Use filter instead of manual filtering
--------------------------------------------------------------------------------

-- Should suggest: use filter
evensOnly :: [Int] -> [Int]
evensOnly [] = []
evensOnly (x:xs)
  | even x = x : evensOnly xs
  | otherwise = evensOnly xs

-- Good: using filter
evensOnlyFilter :: [Int] -> [Int]
evensOnlyFilter = filter even

--------------------------------------------------------------------------------
-- Use concatMap instead of map + concat
--------------------------------------------------------------------------------

-- Should suggest: use concatMap
flattenMap :: (a -> [b]) -> [a] -> [b]
flattenMap f xs = concatMap f xs

-- Good: using concatMap
flattenMapConcat :: (a -> [b]) -> [a] -> [b]
flattenMapConcat = concatMap

--------------------------------------------------------------------------------
-- Use zipWith instead of manual zip + map
--------------------------------------------------------------------------------

-- Should suggest: use zipWith
addPairs :: [Int] -> [Int] -> [Int]
addPairs xs ys = map (uncurry (+)) (zip xs ys)

-- Good: using zipWith
addPairsZipWith :: [Int] -> [Int] -> [Int]
addPairsZipWith = zipWith (+)

--------------------------------------------------------------------------------
-- Use takeWhile/dropWhile instead of manual recursion
--------------------------------------------------------------------------------

-- Should suggest: use takeWhile
takePositive :: [Int] -> [Int]
takePositive [] = []
takePositive (x:xs)
  | x > 0 = x : takePositive xs
  | otherwise = []

-- Good: using takeWhile
takePositiveTW :: [Int] -> [Int]
takePositiveTW = takeWhile (> 0)

--------------------------------------------------------------------------------
-- Use span instead of takeWhile + dropWhile
--------------------------------------------------------------------------------

-- Should suggest: use span
splitAtFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitAtFirst p xs = (takeWhile p xs, dropWhile p xs)

-- Good: using span
splitAtFirstSpan :: (a -> Bool) -> [a] -> ([a], [a])
splitAtFirstSpan = span

--------------------------------------------------------------------------------
-- Use unfoldr for generating lists
--------------------------------------------------------------------------------

-- Should suggest: use unfoldr
countDown :: Int -> [Int]
countDown n
  | n <= 0 = []
  | otherwise = n : countDown (n - 1)

-- Good: using unfoldr
countDownUnfoldr :: Int -> [Int]
countDownUnfoldr = unfoldr (\n -> if n <= 0 then Nothing else Just (n, n - 1))
  where unfoldr f b = case f b of
          Nothing -> []
          Just (a, b') -> a : unfoldr f b'

--------------------------------------------------------------------------------
-- Use iterate for infinite sequences
--------------------------------------------------------------------------------

-- Should suggest: use iterate
powers :: Int -> [Int]
powers n = go 1
  where go x = x : go (x * n)

-- Good: using iterate
powersIterate :: Int -> [Int]
powersIterate n = iterate (* n) 1

--------------------------------------------------------------------------------
-- Use scanl for running totals
--------------------------------------------------------------------------------

-- Should suggest: use scanl
runningSum :: [Int] -> [Int]
runningSum = go 0
  where
    go _ [] = []
    go acc (x:xs) = let acc' = acc + x in acc' : go acc' xs

-- Good: using scanl
runningSumScanl :: [Int] -> [Int]
runningSumScanl = tail . scanl (+) 0

--------------------------------------------------------------------------------
-- Avoid (++) in left-associative position
--------------------------------------------------------------------------------

-- Should warn: quadratic complexity from left-associative (++)
reverseNaive :: [a] -> [a]
reverseNaive [] = []
reverseNaive (x:xs) = reverseNaive xs ++ [x]

-- Good: using accumulator
reverseAcc :: [a] -> [a]
reverseAcc = go []
  where
    go acc [] = acc
    go acc (x:xs) = go (x:acc) xs

--------------------------------------------------------------------------------
-- Use partition instead of two filters
--------------------------------------------------------------------------------

-- Should suggest: use partition
splitEvenOdd :: [Int] -> ([Int], [Int])
splitEvenOdd xs = (filter even xs, filter odd xs)

-- Good: using partition
splitEvenOddPartition :: [Int] -> ([Int], [Int])
splitEvenOddPartition = partition even
  where partition p xs = (filter p xs, filter (not . p) xs)