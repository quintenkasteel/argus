{-# LANGUAGE ImportQualifiedPost #-}

module TuplePatterns where

import Data.Bifunctor (Bifunctor(bimap, first, second))
import Data.Tuple (swap)

-- Helper function for 'both'
both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

--------------------------------------------------------------------------------
-- Basic fst/snd Simplifications
--------------------------------------------------------------------------------

-- Should simplify: fst (a, b)  ==>  a
getFirst :: (Int, Int) -> Int
getFirst pair = fst (1, 2)

-- Should simplify: snd (a, b)  ==>  b
getSecond :: (Bool, Bool) -> Bool
getSecond pair = snd (True, False)

-- Should simplify: (fst x, snd x)  ==>  x
reconstructPair :: (a, b) -> (a, b)
reconstructPair x = (fst x, snd x)

--------------------------------------------------------------------------------
-- Swap Simplifications
--------------------------------------------------------------------------------

-- Should simplify: swap (swap x)  ==>  x
doubleSwap :: (a, b) -> (a, b)
doubleSwap x = swap (swap x)

-- Should simplify: fst (swap (x, y))  ==>  y
firstSwapped :: (String, String) -> String
firstSwapped pair = fst (swap ("hello", "world"))

-- Should simplify: snd (swap (x, y))  ==>  x
secondSwapped :: (Int, Int) -> Int
secondSwapped pair = snd (swap (42, 99))

--------------------------------------------------------------------------------
-- Curry/Uncurry Simplifications
--------------------------------------------------------------------------------

-- Should simplify: uncurry f (a, b)  ==>  f a b
applyUncurried :: (Int -> Int -> Int) -> (Int, Int) -> Int
applyUncurried f pair = uncurry (+) (10, 20)

-- Should simplify: curry (uncurry f)  ==>  f
curryUncurryId :: (a -> b -> c) -> a -> b -> c
curryUncurryId f = curry (uncurry f)

-- Should simplify: uncurry (curry f)  ==>  f
uncurryCurryId :: ((a, b) -> c) -> (a, b) -> c
uncurryCurryId f = uncurry (curry f)

-- Should simplify: curry fst  ==>  const
curryFirst :: a -> b -> a
curryFirst = curry fst

-- Should simplify: curry snd  ==>  const id
currySecond :: a -> b -> b
currySecond = curry snd

-- Should simplify: uncurry (,)  ==>  id
uncurryPairConstructor :: (a, b) -> (a, b)
uncurryPairConstructor = uncurry (,)

--------------------------------------------------------------------------------
-- Bifunctor Patterns (bimap, first, second)
--------------------------------------------------------------------------------

-- Should simplify: bimap f g (x, y)  ==>  (f x, g y)
bimapOnPair :: (Int -> Int) -> (Int -> Int) -> (Int, Int) -> (Int, Int)
bimapOnPair f g pair = bimap (*2) (+1) (5, 10)

-- Should simplify: first f (x, y)  ==>  (f x, y)
firstOnPair :: (Int -> Int) -> (Int, String) -> (Int, String)
firstOnPair f pair = first (+1) (100, "test")

-- Should simplify: second f (x, y)  ==>  (x, f y)
secondOnPair :: (Int -> Int) -> (String, Int) -> (String, Int)
secondOnPair f pair = second (*2) ("key", 42)

-- Should simplify: bimap id f  ==>  second f
bimapWithId1 :: (b -> d) -> (a, b) -> (a, d)
bimapWithId1 f = bimap id f

-- Should simplify: bimap f id  ==>  first f
bimapWithId2 :: (a -> c) -> (a, b) -> (c, b)
bimapWithId2 f = bimap f id

-- Should simplify: bimap id id  ==>  id
bimapIdentity :: (a, b) -> (a, b)
bimapIdentity = bimap id id

-- Should simplify: both f (x, y)  ==>  (f x, f y)
bothOnPair :: (Int -> Int) -> (Int, Int) -> (Int, Int)
bothOnPair f pair = both (*2) (3, 7)

--------------------------------------------------------------------------------
-- Complex Realistic Examples
--------------------------------------------------------------------------------

-- Extract first component inefficiently
extractFirst :: Int
extractFirst = fst (42, "answer")

-- Extract second component inefficiently
extractSecond :: Char
extractSecond = snd (True, 'x')

-- Reconstruct pair from fst/snd
identityPair :: (a, b) -> (a, b)
identityPair tuple = (fst tuple, snd tuple)

-- Double swap cancels out
normalizeSwap :: (String, Int)
normalizeSwap = swap (swap ("test", 123))

-- Apply function to tuple elements
applyToBoth :: Int
applyToBoth = uncurry (*) (6, 7)

-- Curry/uncurry round trip 1
roundTrip1 :: (Int -> Int -> Int) -> Int -> Int -> Int
roundTrip1 func = curry (uncurry func)

-- Curry/uncurry round trip 2
roundTrip2 :: ((Int, Int) -> Int) -> (Int, Int) -> Int
roundTrip2 func = uncurry (curry func)

-- Use curry with fst
pickFirst :: Int -> String -> Int
pickFirst = curry fst

-- Use curry with snd
pickSecond :: Int -> String -> String
pickSecond = curry snd

-- Bifunctor on concrete pair
transformPair :: (Int, String)
transformPair = bimap (*10) (++ "!") (5, "test")

-- Apply same function to both elements
duplicateAndTransform :: Int -> (Int, Int)
duplicateAndTransform x = both (+1) (x, x)

-- Pattern: uncurry with literal tuple
addNumbers :: Int
addNumbers = uncurry (+) (15, 27)

-- Bimap with identity functions
noOpTransform :: (a, b) -> (a, b)
noOpTransform = bimap id id

-- First component transformation
incrementFirst :: (Int, String)
incrementFirst = first (+1) (100, "test")

-- Second component transformation
uppercaseSecond :: (String, String)
uppercaseSecond = second (map toUpper) ("key", "value")
  where
    toUpper :: Char -> Char
    toUpper c = c  -- Simplified for example
