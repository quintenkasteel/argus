{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Examples demonstrating safe alternatives to partial functions
-- These patterns are what Argus should suggest as replacements
module PartialFunctions where

import Data.Maybe (fromMaybe, listToMaybe)
import Text.Read (readMaybe)
import qualified Data.Map as M
import qualified Data.Text as T

-- | Safe head using pattern matching
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- | Safe head using listToMaybe
safeHeadAlt :: [a] -> Maybe a
safeHeadAlt = listToMaybe

-- | Safe tail using pattern matching
safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

-- | Safe init using pattern matching
safeInit :: [a] -> Maybe [a]
safeInit []     = Nothing
safeInit [_]    = Just []
safeInit (x:xs) = (x:) <$> safeInit xs

-- | Safe last using pattern matching
safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs

-- | Safe index using pattern matching
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _         = Nothing
safeIndex (x:_) 0      = Just x
safeIndex (_:xs) n
  | n < 0     = Nothing
  | otherwise = safeIndex xs (n - 1)

-- | Safe fromMaybe with default value
safeFromMaybe :: a -> Maybe a -> a
safeFromMaybe = fromMaybe

-- | Safe combination of list operations
safeMultipleOps :: [Int] -> Maybe Int
safeMultipleOps xs = do
  h <- safeHead xs
  l <- safeLast xs
  i <- safeIndex xs 2
  pure (h + l + i)

-- | Safe read using readMaybe
safeRead :: String -> Maybe Int
safeRead = readMaybe

-- | Safe Map lookup
safeLookup :: Ord k => k -> M.Map k v -> Maybe v
safeLookup = M.lookup

-- | Safe Map lookup with default
safeLookupDefault :: Ord k => v -> k -> M.Map k v -> v
safeLookupDefault def k m = fromMaybe def (M.lookup k m)

-- | Using Either for error handling instead of error/undefined
safeComputation :: Int -> Either String Int
safeComputation x
  | x < 0     = Left "negative input not allowed"
  | otherwise = Right (x * 2)

-- | Pattern matching on Maybe instead of fromJust
processOptional :: Maybe Int -> Int
processOptional Nothing  = 0
processOptional (Just x) = x * 2

-- | Using maybe combinator
processWithMaybe :: Maybe Int -> Int
processWithMaybe = maybe 0 (* 2)

-- | Folding over Maybe
foldOptional :: Maybe Int -> Int
foldOptional = foldr const 0

-- | Safe minimum using NonEmpty or Maybe
safeMinimum :: Ord a => [a] -> Maybe a
safeMinimum []     = Nothing
safeMinimum (x:xs) = Just (foldr min x xs)

-- | Safe maximum using NonEmpty or Maybe
safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum []     = Nothing
safeMaximum (x:xs) = Just (foldr max x xs)
