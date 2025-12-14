{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module LambdaExamples where

import Data.Function (on)
import Control.Arrow ((&&&))
import Control.Monad (join)

--------------------------------------------------------------------------------
-- Eta Reduction Examples
--------------------------------------------------------------------------------

-- Should eta reduce: \x -> f x  ==>  f
etaSimple1 :: (Int -> Int) -> [Int] -> [Int]
etaSimple1 f xs = map (\x -> f x) xs

-- Should eta reduce: \x -> negate x  ==>  negate
etaSimple2 :: [Int] -> [Int]
etaSimple2 xs = map (\x -> negate x) xs

-- Should eta reduce multi-arg: \x y -> max x y  ==>  max
etaMultiArg :: Int -> Int -> Int
etaMultiArg = \x y -> max x y

-- Should use composition: \x -> f (g x)  ==>  f . g
etaCompose1 :: [Int] -> [Bool]
etaCompose1 xs = map (\x -> not (even x)) xs

-- Should use composition chain: \x -> f (g (h x))  ==>  f . g . h
etaComposeChain :: [String] -> [Int]
etaComposeChain xs = map (\s -> length (reverse (words s))) xs

-- Should use flip: \x -> f y x  ==>  flip f y
etaFlip :: Char -> [String] -> [String]
etaFlip c xs = map (\x -> replicate 3 c ++ x) xs

--------------------------------------------------------------------------------
-- Operator Section Examples
--------------------------------------------------------------------------------

-- Should use operator section: \x -> x + 1  ==>  (+ 1)
operatorSection1 :: [Int] -> [Int]
operatorSection1 xs = map (\x -> x + 1) xs

-- Should use operator section: \x -> 2 * x  ==>  (2 *)
operatorSection2 :: [Int] -> [Int]
operatorSection2 xs = map (\x -> 2 * x) xs

-- Should use operator section: \x -> x == 0  ==>  (== 0)
operatorSection3 :: [Int] -> [Bool]
operatorSection3 xs = map (\x -> x == 0) xs

--------------------------------------------------------------------------------
-- Tuple Section Examples
--------------------------------------------------------------------------------

-- Should use tuple section: \x -> (x, 42)  ==>  (, 42)
tupleLeft :: [Int] -> [(Int, Int)]
tupleLeft xs = map (\x -> (x, 42)) xs

-- Should use tuple section: \x -> ("prefix", x)  ==>  ("prefix",)
tupleRight :: [String] -> [(String, String)]
tupleRight xs = map (\x -> ("prefix", x)) xs

-- Duplicate in tuple - could use join (,)
tupleBoth :: [Int] -> [(Int, Int)]
tupleBoth xs = map (\x -> (x, x)) xs

--------------------------------------------------------------------------------
-- LambdaCase Examples
--------------------------------------------------------------------------------

-- Should use LambdaCase: \x -> case x of ...  ==>  \case ...
lambdaToCaseSimple :: [Maybe Int] -> [Int]
lambdaToCaseSimple xs = map (\x -> case x of
                                Just n  -> n
                                Nothing -> 0) xs

-- Should use LambdaCase with multiple patterns
lambdaToCaseMulti :: [Either String Int] -> String
lambdaToCaseMulti xs = concatMap (\x -> case x of
                                    Left err -> "Error: " ++ err
                                    Right n  -> "Value: " ++ show n) xs

-- Nested case in lambda
lambdaNestedCase :: [(Maybe Int, Maybe Int)] -> [Int]
lambdaNestedCase pairs = map (\p -> case p of
                                (Just a, Just b) -> a + b
                                (Just a, Nothing) -> a
                                (Nothing, Just b) -> b
                                (Nothing, Nothing) -> 0) pairs

--------------------------------------------------------------------------------
-- Point-Free Style Examples
--------------------------------------------------------------------------------

-- Should use composition: \x -> abs (negate x)  ==>  abs . negate
pointFreeSimple :: [Int] -> [Int]
pointFreeSimple xs = map (\x -> abs (negate x)) xs

-- Should use 'on' combinator: \x y -> compare (length x) (length y)  ==>  compare `on` length
pointFreeOn :: [String] -> [String] -> Ordering
pointFreeOn = \x y -> compare (length x) (length y)

-- Should use (&&&): \x -> (f x, g x)  ==>  f &&& g
pointFreeArrow :: Int -> (Int, Bool)
pointFreeArrow = \x -> (abs x, even x)

--------------------------------------------------------------------------------
-- Redundant Lambda Examples
--------------------------------------------------------------------------------

-- Redundant lambda wrapping function call
redundantLambda1 :: [String] -> [Int]
redundantLambda1 xs = map (\s -> length s) xs

-- Redundant lambda in composition
redundantLambda2 :: [Int] -> [Int]
redundantLambda2 xs = filter (\n -> even n) (map (\x -> x * 2) xs)

-- Redundant nested lambdas
redundantNested :: Int -> Int -> Int -> Int
redundantNested = \x -> \y -> \z -> x + y + z

--------------------------------------------------------------------------------
-- Complex Lambda Patterns
--------------------------------------------------------------------------------

-- Multiple redundant transformations
complexExample1 :: [Maybe String] -> [Int]
complexExample1 xs = map (\x -> case x of
                            Just s  -> length (reverse s)
                            Nothing -> 0) xs

-- Lambda with tuple and case
complexExample2 :: [(Int, Maybe String)] -> [String]
complexExample2 pairs = map (\p -> case p of
                               (n, Just s)  -> show n ++ ": " ++ s
                               (n, Nothing) -> show n ++ ": empty") pairs

-- Unnecessary parentheses and lambda
complexExample3 :: [Int] -> [Int]
complexExample3 xs = map (\x -> (abs (x))) xs

--------------------------------------------------------------------------------
-- Single-Branch Case
--------------------------------------------------------------------------------

-- Should simplify: case x of y -> f y  ==>  f x
singleBranchCase :: Maybe Int -> Int
singleBranchCase mx = case mx of x -> maybe 0 id x

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

processData :: String -> Int
processData = length . words

transformValue :: Int -> Bool -> (Int, Bool)
transformValue n b = (n * 2, not b)
