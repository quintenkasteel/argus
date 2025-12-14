{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Rules.Parser
-- Description : Rule pattern parsing
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module parses rule patterns from text, supporting both
-- simple wildcard patterns and Haskell expression patterns.
module Argus.Rules.Parser
  ( -- * Parsing
    parsePattern
  , parseTypePattern
  , parseRuleFromText

    -- * Pattern constructors
  , mkWildcardPattern
  , mkVariablePattern
  , mkApplicationPattern
  ) where

import Data.Char (isAlphaNum, isUpper) -- removed isLower - unused
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Rules.Types

--------------------------------------------------------------------------------
-- Pattern Parsing
--------------------------------------------------------------------------------

-- | Parse a pattern from text
parsePattern :: Text -> Either Text Pattern
parsePattern text = case T.unpack (T.strip text) of
  "" -> Left "Empty pattern"
  "*" -> Right PWildcard
  "_" -> Right PWildcard
  s@(c:_) | all isSimpleChar s -> Right $ if isUpper c
                                          then PConstructor (T.pack s) []
                                          else PVar (PatternVar (T.pack s) Nothing)
  s -> parseComplexPattern (T.pack s)
  where
    isSimpleChar c = isAlphaNum c || c == '_' || c == '\''

-- | Parse a complex pattern (with spaces, constructors, etc.)
parseComplexPattern :: Text -> Either Text Pattern
parseComplexPattern text =
  let parts = T.words text
  in case parts of
    [] -> Left "Empty pattern"
    [p] -> parsePattern p
    (c:args) | isConstructor c ->
      PConstructor c <$> traverse parsePattern args
    (f:x:xs) ->
      let app = PApplication <$> parsePattern f <*> parsePattern x
      in foldl (\acc arg -> PApplication <$> acc <*> parsePattern arg) app xs
    -- Redundant pattern: commented out to avoid warning
    -- _ -> Left $ "Cannot parse pattern: " <> text
  where
    isConstructor t = case T.unpack t of
      (c:_) -> isUpper c
      _ -> False

-- | Parse a type pattern
parseTypePattern :: Text -> Either Text TypePattern
parseTypePattern text = case T.unpack (T.strip text) of
  "" -> Left "Empty type pattern"
  "*" -> Right TPWildcard
  s@(c:_) | all isSimpleChar s ->
      Right $ if isUpper c
              then TPConstructor (T.pack s) []
              else TPVar (T.pack s)
  s -> parseComplexTypePattern (T.pack s)
  where
    isSimpleChar c = isAlphaNum c || c == '_' || c == '\''

-- | Parse a complex type pattern
parseComplexTypePattern :: Text -> Either Text TypePattern
parseComplexTypePattern text =
  -- Handle function types
  case T.breakOn "->" text of
    (left, right) | not (T.null right) ->
      TPFunction <$> parseTypePattern (T.strip left)
                 <*> parseTypePattern (T.strip $ T.drop 2 right)
    _ ->
      -- Handle type application
      let parts = T.words text
      in case parts of
        [] -> Left "Empty type pattern"
        [p] -> parseTypePattern p
        (c:args) -> TPConstructor c <$> traverse parseTypePattern args

--------------------------------------------------------------------------------
-- Rule Parsing from Text
--------------------------------------------------------------------------------

-- | Parse a rule from equation syntax: "lhs = rhs"
parseRuleFromText :: Text -> Text -> Either Text (Pattern, Pattern)
parseRuleFromText name text =
  case T.breakOn "=" text of
    (_, right) | T.null right ->
      Left $ "Rule '" <> name <> "' is missing '=' separator"
    (left, right) -> do
      lhs <- parsePattern (T.strip left)
      rhs <- parsePattern (T.strip $ T.drop 1 right)
      Right (lhs, rhs)

--------------------------------------------------------------------------------
-- Pattern Constructors
--------------------------------------------------------------------------------

-- | Create a wildcard pattern
mkWildcardPattern :: Pattern
mkWildcardPattern = PWildcard

-- | Create a variable pattern
mkVariablePattern :: Text -> Maybe Text -> Pattern
mkVariablePattern name constraint = PVar (PatternVar name constraint)

-- | Create an application pattern
mkApplicationPattern :: Pattern -> [Pattern] -> Pattern
mkApplicationPattern f [] = f
mkApplicationPattern f (x:xs) = mkApplicationPattern (PApplication f x) xs
