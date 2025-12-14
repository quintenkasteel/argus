{-# LANGUAGE OverloadedStrings #-}
-- Test file for autofix patterns
module AutofixTest where

import Control.Monad.State

-- Performance issues
testPerformance :: [Int] -> Bool
testPerformance xs =
  if null xs  -- Should suggest null
  then True
else minimum xs > 0  -- Should suggest minimum

testConcat :: [[Int]] -> [Int]
testConcat xs = concatMap f xs  -- Should suggest concatMap
  where f x = [x]

testMconcat :: [String] -> String
testMconcat xs = mconcatMap show xs  -- Should suggest foldMap

-- Redundant patterns
testRedundant :: Bool -> Bool
testRedundant x = if x then True else False  -- Should suggest just x

testDoubleNot :: Bool -> Bool
testDoubleNot x = not . not $ x  -- Should suggest id

-- Modernization
testModernize :: [Int] -> [Int]
testModernize xs = fmap (+1) xs  -- Should suggest fmap

testMappend :: String -> String -> String
testMappend a b = (<>) a b  -- Should suggest <>

testMapM :: Monad m => (a -> m b) -> [a] -> m [b]
testMapM = mapM  -- Should suggest traverse

-- Space leak patterns
testFold :: [Int] -> Int
testFold = foldl (+) 0  -- Should suggest foldl'

-- Security patterns
testDebug :: Int -> Int
testDebug x = traceShowId x + 1  -- Should suggest removal
