{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.SideConditionHelpers
-- Description : Shared helper functions for side condition evaluation
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides shared text-based helper functions used for
-- side condition evaluation across different contexts (text-based,
-- HIE-backed IO, and AST-based evaluation).
module Argus.Rules.SideConditionHelpers
  ( -- * Literal Detection
    isLiteralValue
  , isNumericLiteral
  , isStringLiteral
  , isBooleanLiteral
  , isCharLiteral

    -- * Value Classification
  , isListValue
  , isTupleValue
  , isIdentifier
  , isConstructor
  , isOperator

    -- * Character Classification
  , isUpperChar
  , isLowerChar
  , isIdentChar
  , isOperatorChar

    -- * Text Pattern Matching
  , matchesGlob
  , matchesPrefix
  , matchesSuffix
  , matchesInfix

    -- * Context Detection
  , isImportLine
  , isTopLevel
  , isTestFile
  , isTypeSignatureLine
  , isCommentLine

    -- * Type Helpers
  , extractBaseType
  , isMonadicType
  , isPrimitiveType
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

--------------------------------------------------------------------------------
-- Literal Detection
--------------------------------------------------------------------------------

-- | Check if a value is any kind of literal (numeric, string, bool, char)
isLiteralValue :: Text -> Bool
isLiteralValue val =
  isNumericLiteral val ||
  isStringLiteral val ||
  isBooleanLiteral val ||
  isCharLiteral val

-- | Check if a value is a numeric literal (integer, float, scientific)
-- Handles: 42, -17, 3.14, 1e10, 1.5E-3, 1_000_000
isNumericLiteral :: Text -> Bool
isNumericLiteral val
  | T.null val = False
  | otherwise =
      let stripped = T.dropWhile (== '-') val
      in not (T.null stripped) &&
         T.all isNumericChar stripped &&
         -- Must have at least one digit
         T.any isDigitChar stripped
  where
    isNumericChar c = isDigitChar c || c == '.' || c == 'e' || c == 'E' || c == '_' || c == '-' || c == '+'
    isDigitChar c = c >= '0' && c <= '9'

-- | Check if a value is a string literal (double-quoted)
isStringLiteral :: Text -> Bool
isStringLiteral val =
  T.length val >= 2 &&
  "\"" `T.isPrefixOf` val &&
  "\"" `T.isSuffixOf` val

-- | Check if a value is a boolean literal
isBooleanLiteral :: Text -> Bool
isBooleanLiteral val = val == "True" || val == "False"

-- | Check if a value is a character literal (single-quoted char)
isCharLiteral :: Text -> Bool
isCharLiteral val =
  T.length val >= 3 &&
  "'" `T.isPrefixOf` val &&
  "'" `T.isSuffixOf` val

--------------------------------------------------------------------------------
-- Value Classification
--------------------------------------------------------------------------------

-- | Check if a value looks like a list literal
isListValue :: Text -> Bool
isListValue val =
  T.length val >= 2 &&
  "[" `T.isPrefixOf` val &&
  "]" `T.isSuffixOf` val

-- | Check if a value looks like a tuple literal
isTupleValue :: Text -> Bool
isTupleValue val =
  T.length val >= 2 &&
  "(" `T.isPrefixOf` val &&
  ")" `T.isSuffixOf` val &&
  "," `T.isInfixOf` val

-- | Check if a value is a valid identifier (starts with letter or underscore)
isIdentifier :: Text -> Bool
isIdentifier val =
  not (T.null val) &&
  let c = T.head val
  in (isLowerChar c || isUpperChar c || c == '_') &&
     T.all isIdentChar val

-- | Check if a value is a constructor (starts with uppercase letter)
isConstructor :: Text -> Bool
isConstructor val =
  not (T.null val) &&
  isUpperChar (T.head val) &&
  T.all isIdentChar val

-- | Check if a value is an operator (contains only operator characters)
isOperator :: Text -> Bool
isOperator val =
  not (T.null val) &&
  T.all isOperatorChar val

--------------------------------------------------------------------------------
-- Character Classification
--------------------------------------------------------------------------------

-- | Check if character is uppercase
isUpperChar :: Char -> Bool
isUpperChar c = c >= 'A' && c <= 'Z'

-- | Check if character is lowercase
isLowerChar :: Char -> Bool
isLowerChar c = c >= 'a' && c <= 'z'

-- | Check if character is valid in an identifier (letter, digit, underscore, quote)
isIdentChar :: Char -> Bool
isIdentChar c =
  isLowerChar c ||
  isUpperChar c ||
  (c >= '0' && c <= '9') ||
  c == '_' ||
  c == '\''

-- | Check if character is valid in an operator
isOperatorChar :: Char -> Bool
isOperatorChar c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

--------------------------------------------------------------------------------
-- Text Pattern Matching
--------------------------------------------------------------------------------

-- | Match text against a glob pattern with * wildcards
-- Supports: *suffix, prefix*, *middle*, exact
matchesGlob :: Text -> Text -> Bool
matchesGlob text pat
  -- Handle *middle* (infix match)
  | "*" `T.isPrefixOf` pat && "*" `T.isSuffixOf` pat =
      let middle = T.dropEnd 1 (T.drop 1 pat)
      in T.isInfixOf middle text
  -- Handle *suffix
  | "*" `T.isPrefixOf` pat = T.isSuffixOf (T.drop 1 pat) text
  -- Handle prefix*
  | "*" `T.isSuffixOf` pat = T.isPrefixOf (T.dropEnd 1 pat) text
  -- Exact match
  | otherwise = text == pat

-- | Check if text matches a prefix pattern
matchesPrefix :: Text -> Text -> Bool
matchesPrefix text prefix = T.isPrefixOf prefix text

-- | Check if text matches a suffix pattern
matchesSuffix :: Text -> Text -> Bool
matchesSuffix text suffix = T.isSuffixOf suffix text

-- | Check if text contains a pattern
matchesInfix :: Text -> Text -> Bool
matchesInfix text pat = T.isInfixOf pat text

--------------------------------------------------------------------------------
-- Context Detection
--------------------------------------------------------------------------------

-- | Check if a line is an import statement
isImportLine :: Text -> Bool
isImportLine line = "import " `T.isPrefixOf` T.stripStart line

-- | Check if a line is at top level (not indented)
isTopLevel :: Text -> Bool
isTopLevel line =
  let stripped = T.stripStart line
  in not (T.null stripped) && T.head stripped /= ' '

-- | Check if a file path indicates a test file
isTestFile :: FilePath -> Bool
isTestFile fp =
  let t = T.pack fp
  in "Test" `T.isInfixOf` t ||
     "Spec" `T.isInfixOf` t ||
     "test/" `T.isInfixOf` t ||
     "tests/" `T.isInfixOf` t

-- | Check if a line is a type signature
isTypeSignatureLine :: Text -> Bool
isTypeSignatureLine line =
  let stripped = T.strip line
  in " :: " `T.isInfixOf` stripped ||
     ":: " `T.isPrefixOf` stripped

-- | Check if a line is a comment
isCommentLine :: Text -> Bool
isCommentLine line =
  let stripped = T.stripStart line
  in "--" `T.isPrefixOf` stripped ||
     "{-" `T.isPrefixOf` stripped

--------------------------------------------------------------------------------
-- Type Helpers
--------------------------------------------------------------------------------

-- | Extract the base type from a wrapper type
-- Handles: [a] -> a, Maybe a -> a, Either e a -> a, IO a -> a
extractBaseType :: Text -> Text
extractBaseType ty
  | "[" `T.isPrefixOf` ty && "]" `T.isSuffixOf` ty =
      T.drop 1 (T.dropEnd 1 ty)
  | "Maybe " `T.isPrefixOf` ty =
      T.drop 6 ty
  | "Either " `T.isPrefixOf` ty =
      fromMaybe ty $ extractSecondArg ty
  | "IO " `T.isPrefixOf` ty =
      T.drop 3 ty
  | otherwise = ty
  where
    extractSecondArg t =
      let parts = T.words t
      in if length parts >= 3
         then Just (T.unwords (drop 2 parts))
         else Nothing

-- | Check if a type looks like a monadic type
isMonadicType :: Text -> Bool
isMonadicType ty =
  "IO " `T.isPrefixOf` ty ||
  "Maybe " `T.isPrefixOf` ty ||
  "Either " `T.isPrefixOf` ty ||
  "StateT " `T.isPrefixOf` ty ||
  "ReaderT " `T.isPrefixOf` ty ||
  "ExceptT " `T.isPrefixOf` ty

-- | Check if a type is a primitive type
isPrimitiveType :: Text -> Bool
isPrimitiveType ty =
  ty `elem` ["Int", "Integer", "Float", "Double", "Bool", "Char", "String", "Text", "()", "Void"]
