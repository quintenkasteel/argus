{-# LANGUAGE ImportQualifiedPost #-}

module MonoidPatterns where

import Data.Monoid (Sum(..), Product(..), All(..), Any(..), First(..), Last(..), Dual(..), Endo(..), appEndo)
import Data.Foldable (fold)
import Data.Semigroup (stimes)

--------------------------------------------------------------------------------
-- Monoid Simplification Patterns
--------------------------------------------------------------------------------

-- Should simplify: mempty <> x  ==>  x
leftIdentity :: String -> String
leftIdentity x = mempty <> x

-- Should simplify: x <> mempty  ==>  x
rightIdentity :: String -> String
rightIdentity x = x <> mempty

-- Should simplify: mconcat []  ==>  mempty
emptyMconcat :: String
emptyMconcat = mconcat []

-- Should simplify: mconcat [x]  ==>  x
singletonMconcat :: String -> String
singletonMconcat x = mconcat [x]

-- Should simplify: mconcat [x, y]  ==>  x <> y
pairMconcat :: String -> String -> String
pairMconcat x y = mconcat [x, y]

-- Should simplify: fold []  ==>  mempty
emptyFold :: [Int]
emptyFold = fold []

-- Should simplify: fold [x]  ==>  x
singletonFold :: [Int] -> [Int]
singletonFold x = fold [x]

-- Should simplify: foldMap id xs  ==>  fold xs
foldMapWithId :: [[Int]] -> [Int]
foldMapWithId xs = foldMap id xs

-- Should simplify: foldMap (const mempty) xs  ==>  mempty
foldMapConstMempty :: [String] -> String
foldMapConstMempty xs = foldMap (const mempty) xs

-- Should simplify: stimes 0 x  ==>  mempty
stimesWithZero :: String -> String
stimesWithZero x = stimes 0 x

--------------------------------------------------------------------------------
-- Semigroup Wrapper Patterns
--------------------------------------------------------------------------------

-- Should simplify: getSum (Sum x <> Sum y)  ==>  x + y
sumAppend :: Int -> Int -> Int
sumAppend x y = getSum (Sum x <> Sum y)

-- Should simplify: getProduct (Product x <> Product y)  ==>  x * y
productAppend :: Int -> Int -> Int
productAppend x y = getProduct (Product x <> Product y)

-- Should simplify: getAll (All x <> All y)  ==>  x && y
allAppend :: Bool -> Bool -> Bool
allAppend x y = getAll (All x <> All y)

-- Should simplify: getAny (Any x <> Any y)  ==>  x || y
anyAppend :: Bool -> Bool -> Bool
anyAppend x y = getAny (Any x <> Any y)

-- Should simplify: getDual (Dual x <> Dual y)  ==>  y <> x
dualAppend :: String -> String -> String
dualAppend x y = getDual (Dual x <> Dual y)

-- Endo composition is handled via appEndo
-- (Pattern getEndo doesn't exist in standard lib, but rule may match similar patterns)

-- Should simplify: appEndo (Endo f) x  ==>  f x
endoApply :: (Int -> Int) -> Int -> Int
endoApply f x = appEndo (Endo f) x

--------------------------------------------------------------------------------
-- First/Last Wrapper Patterns
--------------------------------------------------------------------------------

-- First with Just at beginning (manual review)
firstWithJust :: Int -> Maybe Int -> Maybe Int
firstWithJust x y = getFirst (First (Just x) <> First y)

-- Last with Just at end (manual review)
lastWithJust :: Maybe Int -> Int -> Maybe Int
lastWithJust x y = getLast (Last x <> Last (Just y))

--------------------------------------------------------------------------------
-- Complex Realistic Examples
--------------------------------------------------------------------------------

-- Combine multiple strings
combineStrings :: [String] -> String
combineStrings strs = mconcat strs

-- Sum with wrapper
calculateSum :: [Int] -> Int
calculateSum nums = getSum (mconcat (map Sum nums))

-- Product with wrapper
calculateProduct :: [Int] -> Int
calculateProduct nums = getProduct (mconcat (map Product nums))

-- Check all conditions
allTrue :: [Bool] -> Bool
allTrue conditions = getAll (mconcat (map All conditions))

-- Check any condition
anyTrue :: [Bool] -> Bool
anyTrue conditions = getAny (mconcat (map Any conditions))

-- Combine with identity
appendToEmpty :: String -> String
appendToEmpty s = mempty <> s <> mempty

-- Nested monoid operations
nestedMonoid :: String -> String -> String
nestedMonoid x y = mconcat [mempty, x, mempty, y, mempty]

-- Single element fold
singleFoldExample :: [Int]
singleFoldExample = fold [[1, 2, 3]]

-- Identity foldMap
identityFoldMap :: [[String]] -> [String]
identityFoldMap xss = foldMap id xss

-- Const mempty foldMap
constMemptyFoldMap :: [Int] -> String
constMemptyFoldMap xs = foldMap (const mempty) xs

-- Multiple wrapper simplifications
wrapperComposition :: Int -> Int -> Int
wrapperComposition x y = getSum (Sum x <> Sum y) + getProduct (Product x <> Product y)

-- Boolean wrapper combinations
booleanCombinations :: Bool -> Bool -> Bool -> Bool
booleanCombinations a b c = getAll (All a <> All b) && getAny (Any b <> Any c)

-- Endo composition chain
endoChain :: (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> Int -> Int
endoChain f g h x = appEndo (Endo f <> Endo g <> Endo h) x

-- Dual reversal
dualReversal :: String -> String -> String -> String
dualReversal x y z = getDual (Dual x <> Dual y <> Dual z)

-- Empty mconcat in expression
emptyMconcatInExpr :: String -> String
emptyMconcatInExpr x = x <> mconcat []

-- Singleton mconcat in expression
singletonMconcatInExpr :: String -> String -> String
singletonMconcatInExpr x y = x <> mconcat [y]

-- Pair mconcat in expression
pairMconcatInExpr :: String -> String -> String -> String
pairMconcatInExpr x y z = x <> mconcat [y, z]

-- Stimes with zero in expression
stimesZeroInExpr :: String -> String
stimesZeroInExpr x = x <> stimes 0 x
