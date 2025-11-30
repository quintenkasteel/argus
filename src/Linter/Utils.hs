{-# LANGUAGE StrictData #-}

-- |
-- Module      : Linter.Utils
-- Description : Common utility functions for the linter
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides common utility functions used throughout the linter,
-- including identifier character classification and word boundary detection.
module Linter.Utils
  ( -- * Identifier character classification
    isIdentChar
  , isIdentStart
  , isOperatorChar

    -- * Word boundary detection
  , isWordBoundary
  , isWordStart
  , isWordEnd
  , atWordBoundary

    -- * Safe list operations
  , headMay
  , tailMay
  , lastMay
  , initMay

    -- * Text utilities
  , replaceWordBoundary
  , replaceWordBoundaryPreserve
  , findWordBoundaryMatch
  , matchesAtWordBoundary
  ) where

import Data.Char (isAlphaNum, isAlpha)
import Data.Text (Text)
import Data.Text qualified as T

--------------------------------------------------------------------------------
-- Identifier Character Classification
--------------------------------------------------------------------------------

-- | Check if a character can appear in a Haskell identifier
-- This includes alphanumeric characters, underscores, and primes
isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''
{-# INLINE isIdentChar #-}

-- | Check if a character can start a Haskell identifier
-- Identifiers start with letters or underscores
isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_'
{-# INLINE isIdentStart #-}

-- | Check if a character is a Haskell operator character
isOperatorChar :: Char -> Bool
isOperatorChar c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)
{-# INLINE isOperatorChar #-}

--------------------------------------------------------------------------------
-- Word Boundary Detection
--------------------------------------------------------------------------------

-- | Check if we're at a word boundary given the character before and after
-- A word boundary exists when the previous character is not an identifier
-- character (or is Nothing for start of string) AND the next character
-- is not an identifier character (or is Nothing for end of string)
isWordBoundary :: Maybe Char -> Maybe Char -> Bool
isWordBoundary before after = isWordStart before && isWordEnd after
{-# INLINE isWordBoundary #-}

-- | Check if position is at the start of a word
-- (previous character is not an identifier character)
isWordStart :: Maybe Char -> Bool
isWordStart Nothing  = True  -- Start of string
isWordStart (Just c) = not (isIdentChar c)
{-# INLINE isWordStart #-}

-- | Check if position is at the end of a word
-- (next character is not an identifier character)
isWordEnd :: Maybe Char -> Bool
isWordEnd Nothing  = True  -- End of string
isWordEnd (Just c) = not (isIdentChar c)
{-# INLINE isWordEnd #-}

-- | Check if we're at a word boundary given the string before and after
atWordBoundary :: String -> String -> Bool
atWordBoundary before after =
  isWordStart (lastMay before) && isWordEnd (headMay after)
{-# INLINE atWordBoundary #-}

--------------------------------------------------------------------------------
-- Safe List Operations
--------------------------------------------------------------------------------

-- | Safe head - returns Nothing for empty list
headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (x:_) = Just x
{-# INLINE headMay #-}

-- | Safe tail - returns Nothing for empty list
tailMay :: [a] -> Maybe [a]
tailMay []     = Nothing
tailMay (_:xs) = Just xs
{-# INLINE tailMay #-}

-- | Safe last - returns Nothing for empty list
lastMay :: [a] -> Maybe a
lastMay []     = Nothing
lastMay [x]    = Just x
lastMay (_:xs) = lastMay xs
{-# INLINE lastMay #-}

-- | Safe init - returns Nothing for empty list
initMay :: [a] -> Maybe [a]
initMay []     = Nothing
initMay [_]    = Just []
initMay (x:xs) = (x:) <$> initMay xs
{-# INLINE initMay #-}

--------------------------------------------------------------------------------
-- Text Utilities
--------------------------------------------------------------------------------

-- | Replace text respecting word boundaries
-- Only replaces when 'from' appears as a complete identifier
replaceWordBoundary :: Text -> Text -> Text -> Text
replaceWordBoundary from to text =
  T.pack $ go Nothing (T.unpack text) (T.unpack from) (T.unpack to)
  where
    go :: Maybe Char -> String -> String -> String -> String
    go _ [] _ _ = []
    go prev haystack@(h:hs) fromStr toStr
      | fromStr `isPrefixOf` haystack && checkBoundary =
          toStr ++ go (lastMay toStr) (drop (length fromStr) haystack) fromStr toStr
      | otherwise = h : go (Just h) hs fromStr toStr
      where
        afterMatch = drop (length fromStr) haystack
        checkBoundary = isWordStart prev && isWordEnd (headMay afterMatch)

    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (p:ps) (x:xs) = p == x && isPrefixOf ps xs

-- | Find a word-boundary-aware match in a string and return the remaining string
findWordBoundaryMatch :: String -> String -> Maybe String
findWordBoundaryMatch needle = go Nothing
  where
    go :: Maybe Char -> String -> Maybe String
    go _ [] = Nothing
    go prev haystack@(h:hs)
      | needle `isPrefixOf` haystack && checkBoundary =
          Just afterMatch
      | otherwise = go (Just h) hs
      where
        afterMatch = drop (length needle) haystack
        checkBoundary = isWordStart prev && isWordEnd (headMay afterMatch)

    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (p:ps) (x:xs) = p == x && isPrefixOf ps xs

-- | Check if a pattern matches text at a word boundary
matchesAtWordBoundary :: Text -> Text -> Bool
matchesAtWordBoundary pat text = go Nothing (T.unpack text) (T.unpack pat)
  where
    go :: Maybe Char -> String -> String -> Bool
    go _ [] _ = False
    go prev haystack@(h:hs) patStr
      | patStr `isPrefixOf` haystack && checkBoundary = True
      | otherwise = go (Just h) hs patStr
      where
        afterMatch = drop (length patStr) haystack
        checkBoundary = isWordStart prev && isWordEnd (headMay afterMatch)

    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (a:as) (b:bs) = a == b && isPrefixOf as bs

--------------------------------------------------------------------------------
-- String and Comment Preserving Replacement
--------------------------------------------------------------------------------

-- | Context for tracking whether we're inside strings or comments
data ParseContext
  = Normal           -- ^ Normal code
  | InString         -- ^ Inside a string literal "..."
  | InChar           -- ^ Inside a character literal '...'
  | InLineComment    -- ^ Inside a line comment --
  | InBlockComment   -- ^ Inside a block comment {- -}
  deriving stock (Eq, Show)

-- | Replace text respecting word boundaries while preserving strings and comments
-- This version skips over string literals and comments, not replacing inside them
replaceWordBoundaryPreserve :: Text -> Text -> Text -> Text
replaceWordBoundaryPreserve from to text =
  T.pack $ goNormal Nothing (T.unpack text) (T.unpack from) (T.unpack to)
  where
    -- Process normal code
    goNormal :: Maybe Char -> String -> String -> String -> String
    goNormal _ [] _ _ = []
    goNormal prev haystack@(h:hs) fromStr toStr
      -- Start of string literal
      | h == '"' =
          h : goString prev hs fromStr toStr
      -- Start of character literal
      | h == '\'' =
          h : goChar prev hs fromStr toStr
      -- Start of line comment
      | h == '-' && startsWithDash hs =
          h : '-' : goLineComment (drop 1 hs) fromStr toStr
      -- Start of block comment
      | h == '{' && startsWithDash hs =
          h : '-' : goBlockComment 1 (drop 1 hs) fromStr toStr
      -- Check for pattern match at word boundary
      | fromStr `isPrefixOf` haystack && checkBoundary =
          toStr ++ goNormal (lastMay toStr) (drop (length fromStr) haystack) fromStr toStr
      -- Normal character
      | otherwise =
          h : goNormal (Just h) hs fromStr toStr
      where
        afterMatch = drop (length fromStr) haystack
        checkBoundary = isWordStart prev && isWordEnd (headMay afterMatch)

    -- Process string literal (looking for closing ")
    goString :: Maybe Char -> String -> String -> String -> String
    goString _ [] _ _ = []  -- Unterminated string
    goString prev (h:hs) fromStr toStr
      -- Escape sequence
      | h == '\\' && not (null hs) =
          h : Prelude.head hs : goString (Just (Prelude.head hs)) (Prelude.tail hs) fromStr toStr
      -- End of string
      | h == '"' =
          h : goNormal (Just h) hs fromStr toStr
      -- Character inside string
      | otherwise =
          h : goString (Just h) hs fromStr toStr

    -- Process character literal (looking for closing ')
    goChar :: Maybe Char -> String -> String -> String -> String
    goChar _ [] _ _ = []  -- Unterminated char
    goChar prev (h:hs) fromStr toStr
      -- Escape sequence
      | h == '\\' && not (null hs) =
          h : Prelude.head hs : goChar (Just (Prelude.head hs)) (Prelude.tail hs) fromStr toStr
      -- End of char literal
      | h == '\'' =
          h : goNormal (Just h) hs fromStr toStr
      -- Character inside literal
      | otherwise =
          h : goChar (Just h) hs fromStr toStr

    -- Process line comment (until newline)
    goLineComment :: String -> String -> String -> String
    goLineComment [] _ _ = []
    goLineComment (h:hs) fromStr toStr
      -- End of line comment
      | h == '\n' =
          h : goNormal (Just h) hs fromStr toStr
      -- Character inside comment
      | otherwise =
          h : goLineComment hs fromStr toStr

    -- Process block comment (handle nesting)
    goBlockComment :: Int -> String -> String -> String -> String
    goBlockComment _ [] _ _ = []  -- Unterminated block comment
    goBlockComment depth (h:hs) fromStr toStr
      -- Nested block comment start
      | h == '{' && startsWithDash hs =
          h : '-' : goBlockComment (depth + 1) (drop 1 hs) fromStr toStr
      -- Block comment end
      | h == '-' && startsWithClose hs =
          if depth == 1
            then h : '}' : goNormal (Just '}') (drop 1 hs) fromStr toStr
            else h : '}' : goBlockComment (depth - 1) (drop 1 hs) fromStr toStr
      -- Character inside block comment
      | otherwise =
          h : goBlockComment depth hs fromStr toStr

    -- Helper to check for '-' at start of string
    startsWithDash :: String -> Bool
    startsWithDash ('-':_) = True
    startsWithDash _       = False

    -- Helper to check for '}' at start of string
    startsWithClose :: String -> Bool
    startsWithClose ('}':_) = True
    startsWithClose _       = False

    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (p:ps) (x:xs) = p == x && isPrefixOf ps xs
