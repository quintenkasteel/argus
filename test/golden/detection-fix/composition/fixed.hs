{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module CompositionExamples where

import Control.Monad (join)
import Data.Function ((&, flip, on)
import Data.Ord (comparing)

--------------------------------------------------------------------------------
-- Composition with id
--------------------------------------------------------------------------------

-- f . id ==> f
composeIdRight :: (Int -> Int) -> [Int] -> [Int]
composeIdRight f xs = map f xs

-- id . f ==> f
composeIdLeft :: (Int -> Int) -> [Int] -> [Int]
composeIdLeft f xs = map f xs

-- More complex: id . id ==> id
doubleIdCompose :: [Int] -> [Int]
doubleIdCompose = map id

--------------------------------------------------------------------------------
-- Composition with const
--------------------------------------------------------------------------------

-- f . const x ==> const (f x)
composeConst :: Int -> [a] -> [Int]
composeConst n xs = map (negate . const n) xs

-- const x . f ==> const x
constCompose :: [Int] -> [Int]
constCompose xs = map (const 42 . abs) xs

-- const x . const y ==> const x
doubleConst :: [a] -> [Int]
doubleConst xs = map (const 10 . const 20) xs

--------------------------------------------------------------------------------
-- Composition apply patterns
--------------------------------------------------------------------------------

-- (f . g) x ==> f (g x)
composeApply1 :: Int -> Int
composeApply1 x = (negate . abs) x

composeApply2 :: String -> Int
composeApply2 s = (length . words) s

composeApply3 :: [Int] -> [Bool]
composeApply3 xs = map (\x -> (not . even) x) xs

--------------------------------------------------------------------------------
-- flip with const
--------------------------------------------------------------------------------

-- flip const x y ==> y
flipConstFull :: Int -> Int -> Int
flipConstFull x y = flip const x y

flipConstApplied :: Int -> Int -> Int
flipConstApplied = flip const

-- flip const partial
flipConstPartial :: a -> b -> b
flipConstPartial = flip const

--------------------------------------------------------------------------------
-- flip with flip
--------------------------------------------------------------------------------

-- Double flip pattern - noted for information
-- flip (flip f) == f for any binary function f
flipFlipNote :: String
flipFlipNote = "flip (flip f) is equivalent to f"

--------------------------------------------------------------------------------
-- flip with ($)
--------------------------------------------------------------------------------

-- flip ($) ==> (&)
flipDollarExample :: Int -> (Int -> Int) -> Int
flipDollarExample x f = flip ($) x f

flipDollarPartial :: a -> (a -> b) -> b
flipDollarPartial = flip ($)

-- Using flip ($) in pipeline
pipelineExample :: Int -> Int
pipelineExample x = flip ($) x $ \n -> n * 2

--------------------------------------------------------------------------------
-- flip with (.)
--------------------------------------------------------------------------------

-- flip (.) f g ==> g . f (reverse composition order)
flipDotExample1 :: Int -> Int
flipDotExample1 = flip (.) abs negate

flipDotExample2 :: String -> Int
flipDotExample2 = flip (.) words length

-- flip (.) with lambdas - swaps composition order
flipDotLambda :: [Int] -> [Int]
flipDotLambda = map (flip (.) abs negate)

--------------------------------------------------------------------------------
-- Application patterns
--------------------------------------------------------------------------------

-- ($) f x ==> f x
dollarApplyExample :: Int -> Int
dollarApplyExample x = ($) negate x

dollarApplyFunc :: (Int -> Int) -> Int -> Int
dollarApplyFunc f x = ($) f x

dollarApplyNested :: Int -> Int
dollarApplyNested x = ($) abs (($) negate x)

--------------------------------------------------------------------------------
-- id application
--------------------------------------------------------------------------------

-- id x ==> x
idApplySimple :: Int -> Int
idApplySimple x = x

idApplyList :: [Int] -> [Int]
idApplyList xs = xs

idApplyComposed :: [Int] -> [Int]
idApplyComposed xs = filter even xs

--------------------------------------------------------------------------------
-- const application
--------------------------------------------------------------------------------

-- const x y ==> x
constApplyFull :: Int -> String -> Int
constApplyFull x y = const x y

constApplyPartial :: Int -> String -> Int
constApplyPartial x = const x

constApplyInMap :: [String] -> [Int]
constApplyInMap xs = map (\s -> const 42 s) xs

--------------------------------------------------------------------------------
-- on combinator patterns
--------------------------------------------------------------------------------

-- on (==) f x y ==> f x == f y
onEqExample :: String -> String -> Bool
onEqExample x y = on (==) length x y

onEqPartial :: Eq b => (a -> b) -> a -> a -> Bool
onEqPartial f = on (==) f

-- on compare f ==> comparing f
onCompareExample :: String -> String -> Ordering
onCompareExample x y = on compare length x y

onComparePartial :: Ord b => (a -> b) -> a -> a -> Ordering
onComparePartial f = on compare f

-- flip on f g ==> on (flip f) g
-- This is an informational pattern note
flipOnInfo :: String
flipOnInfo = "flip on swaps the order of arguments to the comparison function"

--------------------------------------------------------------------------------
-- Lambda simplifications to id
--------------------------------------------------------------------------------

-- \x -> x ==> id
lambdaIdSimple :: (Int -> Int)
lambdaIdSimple = \x -> x

lambdaIdInMap :: [Int] -> [Int]
lambdaIdInMap xs = map (\y -> y) xs

-- Multiple lambdas with same body
lambdaIdNested :: Int -> Int -> Int
lambdaIdNested = \x -> \y -> y

--------------------------------------------------------------------------------
-- Lambda simplifications to const
--------------------------------------------------------------------------------

-- \x -> y ==> const y (when x not in y)
lambdaConstSimple :: Int -> Int
lambdaConstSimple = \_ -> 42

lambdaConstInMap :: [String] -> [Int]
lambdaConstInMap xs = map (\_ -> 10) xs

-- More complex const patterns
lambdaConstExpr :: String -> Int -> Int
lambdaConstExpr s = \_ -> length s

--------------------------------------------------------------------------------
-- Lambda to composition
--------------------------------------------------------------------------------

-- \x -> f (g x) ==> f . g
lambdaComposeSimple :: Int -> Bool
lambdaComposeSimple = \x -> not (even x)

lambdaComposeInMap :: [Int] -> [Int]
lambdaComposeInMap xs = map (\n -> abs (negate n)) xs

lambdaComposeChain :: [String] -> [Int]
lambdaComposeChain xs = map (\s -> length (reverse s)) xs

--------------------------------------------------------------------------------
-- Lambda to flip
--------------------------------------------------------------------------------

-- \x y -> f y x ==> flip f
lambdaFlipSimple :: Int -> Int -> Bool
lambdaFlipSimple = \x y -> (<) y x

lambdaFlipWithFunc :: (Int -> Int -> Int) -> Int -> Int -> Int
lambdaFlipWithFunc f = \a b -> f b a

--------------------------------------------------------------------------------
-- Lambda with tuples
--------------------------------------------------------------------------------

-- \(x, y) -> (y, x) ==> swap
lambdaSwapSimple :: [(Int, String)] -> [(String, Int)]
lambdaSwapSimple xs = map (\(a, b) -> (b, a)) xs

-- \(x, y) -> x ==> fst
lambdaFstSimple :: [(Int, String)] -> [Int]
lambdaFstSimple xs = map (\(a, b) -> a) xs

-- \(x, y) -> y ==> snd
lambdaSndSimple :: [(Int, String)] -> [String]
lambdaSndSimple xs = map (\(a, b) -> b) xs

-- \x -> (x, x) ==> join (,)
lambdaDupSimple :: [Int] -> [(Int, Int)]
lambdaDupSimple xs = map (\n -> (n, n)) xs

--------------------------------------------------------------------------------
-- Lambda to ($ x)
--------------------------------------------------------------------------------

-- \f -> f x ==> ($ x)
lambdaDollarSimple :: Int -> (Int -> Int) -> Int
lambdaDollarSimple x = \f -> f x

lambdaDollarInMap :: [Int] -> [(Int -> Int)] -> [Int]
lambdaDollarInMap xs fs = map (\f -> f 10) fs

--------------------------------------------------------------------------------
-- Complex composition chains
--------------------------------------------------------------------------------

-- Multiple opportunities for simplification
complexChain1 :: [Int] -> [Bool]
complexChain1 xs = map id . not . id . even xs

complexChain2 :: [String] -> [Int]
complexChain2 xs = map (\s -> length . id . words s) xs

-- Composition with const
complexChainConst :: [Int] -> [Int]
complexChainConst xs = map ((+1) . const 10 . abs) xs

--------------------------------------------------------------------------------
-- Unnecessary flip patterns
--------------------------------------------------------------------------------

-- Flip that can be simplified
unnecessaryFlip1 :: Int -> Int -> Bool
unnecessaryFlip1 = flip (flip (<))

-- flip . flip pattern (double flip)
unnecessaryFlipComposed :: (Int -> Int -> Bool) -> Int -> Int -> Bool
unnecessaryFlipComposed f = flip . flip $ f

--------------------------------------------------------------------------------
-- Mixed patterns
--------------------------------------------------------------------------------

-- Combining multiple inefficiencies
mixedPattern1 :: [Int] -> [Int]
mixedPattern1 xs = map ((\ x -> x) . (+1)) xs

mixedPattern2 :: [String] -> [Int]
mixedPattern2 xs = map (\s -> length s) xs

-- const and id together
mixedConstId :: [a] -> [Int]
mixedConstId xs = map id . const 5 xs

-- Composition in wrong order
mixedFlipCompose :: Int -> Int
mixedFlipCompose x = flip (.) abs negate x

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

helperFunc1 :: Int -> Int -> Int
helperFunc1 x y = x + y

helperFunc2 :: String -> Int
helperFunc2 = length

helperFunc3 :: Bool -> Bool
helperFunc3 = not
