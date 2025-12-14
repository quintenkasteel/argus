{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Analysis.TextProcessing
-- Description : Common text processing utilities for source code analysis
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides shared utilities for processing Haskell source code text.
-- These functions handle common operations like:
--
-- * Detecting code vs comments/strings
-- * Stripping string literals and comments
-- * Pattern matching in code portions only
-- * Line classification
--
-- By centralizing these utilities, we avoid code duplication across rule modules
-- and ensure consistent behavior for text-based analysis.
--
-- == Usage
--
-- @
-- import Argus.Analysis.TextProcessing
--
-- -- Check if pattern appears in actual code
-- let hasUnsafeOp = patternInCode "unsafePerformIO" line
--
-- -- Get only the code portion of a line
-- let codePart = extractCode "let x = 1 -- comment"
-- -- codePart == "let x = 1 "
--
-- -- Count occurrences in code (not in strings/comments)
-- let count = countInCode "map" "map f $ map g xs -- uses map twice"
-- -- count == 2
-- @
module Argus.Analysis.TextProcessing
  ( -- * Line Classification
    isCodeLine
  , isCommentLine
  , isBlankLine
  , isInsideBlockComment

    -- * String/Comment Stripping
  , stripStrings
  , stripComments
  , extractCode

    -- * Pattern Matching
  , patternInCode
  , countInCode
  , wordInCode

    -- * Advanced Utilities
  , detectChainedFunction
  , stripStringsAndComments
  ) where

import Data.Text (Text)
import Data.Text qualified as T

--------------------------------------------------------------------------------
-- Line Classification
--------------------------------------------------------------------------------

-- | Check if a line is actual code (not a comment or inside a string)
-- A line is considered code if it:
--   * Is not empty/blank
--   * Does not start with a line comment (--)
--   * Does not start with a block comment ({-)
--   * Is not detected as inside a block comment
isCodeLine :: Text -> Bool
isCodeLine line =
  let stripped = T.stripStart line
  in not (T.null stripped) &&
     not (T.isPrefixOf "--" stripped) &&
     not (T.isPrefixOf "{-" stripped) &&
     not (isInsideBlockComment stripped)

-- | Check if a line is a comment line
isCommentLine :: Text -> Bool
isCommentLine line =
  let stripped = T.stripStart line
  in T.isPrefixOf "--" stripped || T.isPrefixOf "{-" stripped

-- | Check if a line is blank (empty or whitespace only)
isBlankLine :: Text -> Bool
isBlankLine = T.null . T.strip

-- | Basic heuristic to detect if we're inside a block comment
-- This is a simplified check - proper parsing would need state tracking.
-- Returns False by default as accurate detection requires multi-line context.
isInsideBlockComment :: Text -> Bool
isInsideBlockComment _ = False  -- Simplified; real impl would track {- -}

--------------------------------------------------------------------------------
-- String/Comment Stripping
--------------------------------------------------------------------------------

-- | Strip string literals from a line, replacing them with spaces.
-- This prevents matching patterns inside string literals while
-- preserving column positions for accurate span reporting.
--
-- @
-- stripStrings "let msg = \"hello world\" in msg"
-- -- Result: "let msg =               in msg"
-- @
stripStrings :: Text -> Text
stripStrings = T.pack . go False . T.unpack
  where
    go :: Bool -> String -> String
    go _ [] = []
    go inStr ('"':cs)
      | inStr     = ' ' : go False cs    -- End of string
      | otherwise = ' ' : go True cs     -- Start of string
    go True ('\\':_:cs) = ' ' : ' ' : go True cs  -- Escape sequence (preserve both chars as spaces)
    go True (_:cs) = ' ' : go True cs    -- Inside string - replace with space
    go False (c:cs) = c : go False cs    -- Outside string - keep char

-- | Strip line comments (-- ...) from a line
-- Everything after -- is removed.
stripComments :: Text -> Text
stripComments line =
  let (codePart, _) = T.breakOn "--" line
  in codePart

-- | Get only the code portion of a line (no comments, no string contents).
-- This combines 'stripComments' and 'stripStrings' for complete sanitization.
--
-- @
-- extractCode "let x = \"str\" -- comment"
-- -- Result: "let x =       "
-- @
extractCode :: Text -> Text
extractCode = stripStrings . stripComments

-- | Alternative name for 'extractCode' for compatibility with existing code
stripStringsAndComments :: Text -> Text
stripStringsAndComments = extractCode

--------------------------------------------------------------------------------
-- Pattern Matching
--------------------------------------------------------------------------------

-- | Check if a pattern appears in actual code (not in a comment or string).
-- This is the primary function for safe pattern matching in source analysis.
--
-- @
-- patternInCode "undefined" "let x = undefined -- bad"
-- -- Result: True (found in code)
--
-- patternInCode "undefined" "-- undefined is bad"
-- -- Result: False (only in comment)
--
-- patternInCode "undefined" "let msg = \"undefined\""
-- -- Result: False (only in string)
-- @
patternInCode :: Text -> Text -> Bool
patternInCode needle line =
  needle `T.isInfixOf` extractCode line

-- | Count pattern occurrences in code portion only (excluding comments/strings).
--
-- @
-- countInCode "map" "map f $ map g xs -- map comment"
-- -- Result: 2
-- @
countInCode :: Text -> Text -> Int
countInCode needle line = T.count needle (extractCode line)

-- | Check if a word appears in code with word boundaries.
-- More precise than 'patternInCode' as it won't match substrings.
--
-- @
-- wordInCode "map" "mapping is not map"
-- -- 'patternInCode' would match "map" in "mapping"
-- -- 'wordInCode' only matches the standalone "map"
-- @
wordInCode :: Text -> Text -> Bool
wordInCode needle line =
  let code = extractCode line
      -- Split on the needle and check boundaries
  in checkWordBoundaries needle code

-- | Check if needle appears as a word (with proper boundaries) in text
checkWordBoundaries :: Text -> Text -> Bool
checkWordBoundaries needle text
  | T.null text = False
  | otherwise = go text
  where
    go t =
      case T.breakOn needle t of
        (_, rest) | T.null rest -> False
        (before, after) ->
          let afterNeedle = T.drop (T.length needle) after
              beforeOk = case T.unsnoc before of
                Nothing -> True
                Just (_, c) -> isWordBoundary c
              afterOk = case T.uncons afterNeedle of
                Nothing -> True
                Just (c, _) -> isWordBoundary c
          in (beforeOk && afterOk) || go (T.drop 1 after)

    isWordBoundary c = not (isWordChar c)
    isWordChar c = c `elem` ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'" :: String)

--------------------------------------------------------------------------------
-- Advanced Utilities
--------------------------------------------------------------------------------

-- | Detect chained function calls like "map f $ map g $ xs" or "filter p . filter q".
-- This identifies when the same function appears twice with composition/application operators.
--
-- @
-- detectChainedFunction "map" "map f $ map g xs"
-- -- Result: True
--
-- detectChainedFunction "filter" "filter p . filter q"
-- -- Result: True
--
-- detectChainedFunction "map" "map f xs"
-- -- Result: False (only one occurrence)
-- @
detectChainedFunction :: Text -> Text -> Bool
detectChainedFunction funcName line =
  let codePart = extractCode line
      funcWithSpace = funcName <> " "
      occurrences = countInCode funcWithSpace line
  in occurrences >= 2 &&
     -- Ensure there's composition or application between them
     (". " `T.isInfixOf` codePart ||    -- f . g composition
      " . " `T.isInfixOf` codePart ||   -- spaced composition
      "$ " `T.isInfixOf` codePart ||    -- f $ x application
      " $ " `T.isInfixOf` codePart)     -- spaced application
