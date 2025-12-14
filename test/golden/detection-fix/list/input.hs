{-# LANGUAGE ImportQualifiedPost #-}

module ListPatterns where

import Data.List (sort, sortBy, reverse)

--------------------------------------------------------------------------------
-- List Sorting Simplifications
--------------------------------------------------------------------------------

-- Should simplify: head (sort xs)  ==>  minimum xs
getSmallest :: Ord a => [a] -> a
getSmallest xs = head (sort xs)

-- Should simplify: last (sort xs)  ==>  maximum xs
getLargest :: Ord a => [a] -> a
getLargest xs = last (sort xs)

-- Should simplify: head (sortBy cmp xs)  ==>  minimumBy cmp xs
getSmallestBy :: (a -> a -> Ordering) -> [a] -> a
getSmallestBy cmp xs = head (sortBy cmp xs)

-- Should simplify: last (sortBy cmp xs)  ==>  maximumBy cmp xs
getLargestBy :: (a -> a -> Ordering) -> [a] -> a
getLargestBy cmp xs = last (sortBy cmp xs)

--------------------------------------------------------------------------------
-- Reverse Simplifications
--------------------------------------------------------------------------------

-- Should simplify: reverse (reverse xs)  ==>  xs
doubleReverse :: [a] -> [a]
doubleReverse xs = reverse (reverse xs)

-- Should simplify: head (reverse xs)  ==>  last xs
getLastElement :: [a] -> a
getLastElement xs = head (reverse xs)

-- Should simplify: last (reverse xs)  ==>  head xs
getFirstElement :: [a] -> a
getFirstElement xs = last (reverse xs)

--------------------------------------------------------------------------------
-- Concat Simplifications
--------------------------------------------------------------------------------

-- Should simplify: concat [[x]]  ==>  [x]
concatSingletonList :: a -> [a]
concatSingletonList x = concat [[x]]

-- Should simplify: concat [xs]  ==>  xs
concatSingleList :: [a] -> [a]
concatSingleList xs = concat [xs]

-- Should simplify: concat (map f xs)  ==>  concatMap f xs
flatMapPattern :: (a -> [b]) -> [a] -> [b]
flatMapPattern f xs = concat (map f xs)

--------------------------------------------------------------------------------
-- Null and Length Patterns
--------------------------------------------------------------------------------

-- Should simplify: length xs == 0  ==>  null xs
checkEmpty1 :: [a] -> Bool
checkEmpty1 xs = length xs == 0

-- Should simplify: length xs /= 0  ==>  not (null xs)
checkNotEmpty1 :: [a] -> Bool
checkNotEmpty1 xs = length xs /= 0

-- Should simplify: length xs > 0  ==>  not (null xs)
checkNotEmpty2 :: [a] -> Bool
checkNotEmpty2 xs = length xs > 0

-- Should simplify: length xs < 1  ==>  null xs
checkEmpty2 :: [a] -> Bool
checkEmpty2 xs = length xs < 1

-- Should simplify: length xs >= 1  ==>  not (null xs)
checkNotEmpty3 :: [a] -> Bool
checkNotEmpty3 xs = length xs >= 1

-- Should simplify: 0 == length xs  ==>  null xs
checkEmpty3 :: [a] -> Bool
checkEmpty3 xs = 0 == length xs

-- Should simplify: 0 /= length xs  ==>  not (null xs)
checkNotEmpty4 :: [a] -> Bool
checkNotEmpty4 xs = 0 /= length xs

-- Note: null (filter p xs) patterns would require additional rules
-- Could be optimized: null (filter p xs)  ==>  all (not . p) xs
-- Could be optimized: not (null (filter p xs))  ==>  any p xs

--------------------------------------------------------------------------------
-- List Construction Patterns
--------------------------------------------------------------------------------

-- Should simplify: x : []  ==>  [x]
singletonCons :: a -> [a]
singletonCons x = x : []

-- Should simplify: x : y : []  ==>  [x, y]
pairCons :: a -> a -> [a]
pairCons x y = x : y : []

-- Should simplify: [] ++ xs  ==>  xs
emptyAppendLeft :: [a] -> [a]
emptyAppendLeft xs = [] ++ xs

-- Should simplify: xs ++ []  ==>  xs
emptyAppendRight :: [a] -> [a]
emptyAppendRight xs = xs ++ []

-- Should simplify: [x] ++ xs  ==>  x : xs
singletonAppend :: a -> [a] -> [a]
singletonAppend x xs = [x] ++ xs

--------------------------------------------------------------------------------
-- List Transform Patterns
--------------------------------------------------------------------------------

-- Should simplify: take 0 xs  ==>  []
takeZero :: [a] -> [a]
takeZero xs = take 0 xs

-- Should simplify: drop 0 xs  ==>  xs
dropZero :: [a] -> [a]
dropZero xs = drop 0 xs

-- Should simplify: filter (const True) xs  ==>  xs
filterTrue :: [a] -> [a]
filterTrue xs = filter (const True) xs

-- Should simplify: filter (const False) xs  ==>  []
filterFalse :: [a] -> [a]
filterFalse xs = filter (const False) xs

-- Should simplify: replicate 0 x  ==>  []
replicateZero :: a -> [a]
replicateZero x = replicate 0 x

-- Should simplify: take n (repeat x)  ==>  replicate n x
takeRepeat :: Int -> a -> [a]
takeRepeat n x = take n (repeat x)

-- Should simplify: map f (repeat x)  ==>  repeat (f x)
mapRepeat :: (a -> b) -> a -> [b]
mapRepeat f x = map f (repeat x)

-- Should simplify: map f (replicate n x)  ==>  replicate n (f x)
mapReplicate :: (a -> b) -> Int -> a -> [b]
mapReplicate f n x = map f (replicate n x)

--------------------------------------------------------------------------------
-- List Indexing Patterns
--------------------------------------------------------------------------------

-- Should simplify: xs !! 0  ==>  head xs
indexZero :: [a] -> a
indexZero xs = xs !! 0

-- Should simplify: head (drop n xs)  ==>  xs !! n
headDrop :: Int -> [a] -> a
headDrop n xs = head (drop n xs)

-- Should simplify: splitAt 0 xs  ==>  ([], xs)
splitAtZero :: [a] -> ([a], [a])
splitAtZero xs = splitAt 0 xs

-- Should simplify: splitAt n []  ==>  ([], [])
splitAtEmpty :: Int -> ([a], [a])
splitAtEmpty n = splitAt n []

--------------------------------------------------------------------------------
-- Zip Patterns
--------------------------------------------------------------------------------

-- Should simplify: zipWith const xs ys  ==>  take (length ys) xs
zipWithConst :: [a] -> [b] -> [a]
zipWithConst xs ys = zipWith const xs ys

--------------------------------------------------------------------------------
-- Complex Realistic Examples
--------------------------------------------------------------------------------

-- Find minimum element inefficiently
findMin :: Ord a => [a] -> a
findMin numbers = head (sort numbers)

-- Find maximum element inefficiently
findMax :: Ord a => [a] -> a
findMax numbers = last (sort numbers)

-- Double reverse is identity
normalizeList :: [a] -> [a]
normalizeList items = reverse (reverse items)

-- Check if list is empty the slow way
isEmptyList :: [a] -> Bool
isEmptyList items = length items == 0

-- Check if list has elements the slow way
hasElements :: [a] -> Bool
hasElements items = length items > 0

-- Flatten nested lists inefficiently
flattenAll :: [[a]] -> [a]
flattenAll lists = concat (map id lists)

-- Build singleton list inefficiently
makeSingleton :: a -> [a]
makeSingleton value = value : []

-- Append to empty list
appendToEmpty :: [a] -> [a]
appendToEmpty items = [] ++ items

-- Get first element inefficiently
getFirst :: [a] -> a
getFirst items = items !! 0
