{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Examples of performance issues that Argus should detect
module Performance where

import Control.Monad (liftM)
import Control.Monad.State.Strict (State, execState, modify)
import Data.List (foldl', nub, sort)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T (Text)
-- =============================================================================
-- String vs Text performance
-- =============================================================================

-- | Using String for text processing - should suggest Text
processString :: String -> String
processString s = reverse $ filter (/= ' ') s

-- | String concatenation in a loop - very slow
buildStringLoop :: [String] -> String
buildStringLoop = foldr (++) ""

-- | Should use Text.concat or Text builder
slowStringConcat :: [String] -> String
slowStringConcat xs = foldl (++) "" xs

-- =============================================================================
-- List as map/set (O(n) lookup)
-- =============================================================================

-- | Using list for membership check - should use Set
slowMembership :: Eq a => a -> [a] -> Bool
slowMembership x xs = x `elem` xs

-- | Using list lookup - should use Map
slowLookup :: Eq k => k -> [(k, v)] -> Maybe v
slowLookup k pairs = lookup k pairs

-- | Multiple elem checks - definitely should use Set
multipleElemChecks :: [Int] -> [Int] -> [Int]
multipleElemChecks xs ys = filter (`elem` ys) xs

-- | Using nub on large lists - O(n^2), should use Set
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = nub

-- =============================================================================
-- Inefficient patterns
-- =============================================================================

-- | null == 0 instead of null
checkEmpty :: [a] -> Bool
checkEmpty xs = null xs

-- | not (null > 0 instead of not null)
checkNonEmpty :: [a] -> Bool
checkNonEmpty xs = not (null xs)

-- | Safe minimum - consider using minimum directly
getMinimum :: Ord a => [a] -> Maybe a
getMinimum []     = Nothing
getMinimum (x:xs) = Just (foldr min x xs)

-- | Safe maximum - consider using maximum directly
getMaximum :: Ord a => [a] -> Maybe a
getMaximum []     = Nothing
getMaximum (x:xs) = Just (foldr max x xs)

-- | concatMap instead of concatMap
flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f xs = concat (map f xs)

-- | map then filter instead of mapMaybe - use mapMaybe from Data.Maybe
mapThenFilter :: (a -> Maybe b) -> [a] -> [b]
mapThenFilter f xs = foldr go [] xs
  where
    go a acc = case f a of
      Nothing -> acc
      Just b  -> b : acc

-- | mconcatMap instead of foldMap
combineAll :: Monoid m => (a -> m) -> [a] -> m
combineAll f xs = mconcat (map f xs)

-- =============================================================================
-- Lazy State issues
-- =============================================================================

-- | Using lazy State when Strict would be better
countOccurrences :: [Int] -> Int
countOccurrences xs = execState (mapM_ count xs) 0
  where
    count :: Int -> State Int ()
    count _ = modify (+1)

-- =============================================================================
-- Fusion-breaking patterns
-- =============================================================================

-- | Intermediate list that could be fused
sumSquares :: [Int] -> Int
sumSquares xs = sum $ map (^2) xs

-- | Multiple traversals that could be one
multipleTraversals :: [Int] -> (Int, Int)
multipleTraversals xs = (sum xs, length xs)

-- =============================================================================
-- Monadic inefficiencies
-- =============================================================================

-- | fmap instead of fmap
usesLiftM :: Monad m => (a -> b) -> m a -> m b
usesLiftM f xs = fmap f xs

-- | pure then bind instead of fmap
returnThenBind :: Monad m => (a -> b) -> m a -> m b
returnThenBind f ma = ma >>= \a -> pure (f a)

-- | mapM on list when traverse would be clearer
listMapM :: Monad m => (a -> m b) -> [a] -> m [b]
listMapM = mapM

-- | sequence . map instead of traverse
sequenceMap :: Monad m => (a -> m b) -> [a] -> m [b]
sequenceMap f xs = sequence (map f xs)

-- =============================================================================
-- Strict evaluation issues
-- =============================================================================

-- | Bang pattern might be needed
strictSum :: [Int] -> Int
strictSum = go 0
  where
    go acc [] = acc
    go acc (x:xs) = go (acc + x) xs  -- acc not forced

-- | Should use $! for strict application
applyLoose :: (a -> b) -> a -> b
applyLoose f x = f x  -- f $ x when f $! x might be needed