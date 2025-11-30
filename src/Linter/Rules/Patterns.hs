{-# LANGUAGE StrictData #-}

-- |
-- Module      : Linter.Rules.Patterns
-- Description : Code pattern matching and suggestions
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module implements pattern-based linting rules, similar to HLint
-- but with project-specific patterns.
module Linter.Rules.Patterns
  ( -- * Pattern checking
    checkPatterns
  , matchPattern
  , applyPatternRule

    -- * Built-in patterns
  , defaultPatterns
  ) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Linter.Analysis.Syntactic
import Linter.Config
import Linter.Types
import Linter.Utils (isIdentChar, isWordStart, isWordEnd, headMay)

--------------------------------------------------------------------------------
-- Pattern Checking
--------------------------------------------------------------------------------

-- | Check code against pattern rules
checkPatterns :: PatternsConfig -> FilePath -> Text -> [FunctionInfo] -> [Diagnostic]
checkPatterns cfg path source functions
  | not (patternsEnabled cfg) = []
  | otherwise = concatMap (checkFunction (patternsRules cfg)) functions
  where
    checkFunction rules fi = concatMap (checkBodyLine path fi) (fiBody fi)

    checkBodyLine path fi (lineNum, lineText) =
      mapMaybe (tryMatchRule path fi lineNum lineText) (patternsRules cfg)

-- | Try to match a pattern rule against a line of code
tryMatchRule :: FilePath -> FunctionInfo -> Int -> Text -> PatternRule -> Maybe Diagnostic
tryMatchRule path fi lineNum lineText rule =
  if matchPattern (prMatch rule) lineText
  then Just $ mkPatternDiagnostic path fi lineNum lineText rule
  else Nothing

-- | Pattern matching for code patterns with word boundary awareness
-- Supports:
--   - Exact identifier matching with word boundaries
--   - Simple wildcard patterns: "*" matches any expression
--   - Patterns like "head *" or "* $ head"
matchPattern :: Text -> Text -> Bool
matchPattern pat text
  | T.null pat = False
  | containsWildcard pat = matchWildcardPattern pat text
  | otherwise = matchIdentifier pat text

-- | Check if pattern contains wildcards
containsWildcard :: Text -> Bool
containsWildcard = T.isInfixOf "*"

-- | Match an identifier with word boundary checking
-- "head" should not match "headMay" or "overhead"
matchIdentifier :: Text -> Text -> Bool
matchIdentifier ident text = go Nothing (T.unpack text) (T.unpack ident)
  where
    go :: Maybe Char -> String -> String -> Bool
    go _ [] _ = False
    go prev haystack@(h:hs) needle
      | needle `startsWithAt` haystack =
          let afterMatch = drop (length needle) haystack
          in isWordStart prev && isWordEnd (headMay afterMatch)
      | otherwise = go (Just h) hs needle

    startsWithAt [] _ = True
    startsWithAt _ [] = False
    startsWithAt (n:ns) (x:xs) = n == x && startsWithAt ns xs

-- | Match a pattern with wildcards against text
-- "*" matches zero or more characters/expressions
matchWildcardPattern :: Text -> Text -> Bool
matchWildcardPattern pat text = matchParts (tokenize pat) Nothing (T.unpack text)
  where
    -- Tokenize pattern into parts (literals and wildcards)
    tokenize :: Text -> [PatPart]
    tokenize t = case T.breakOn "*" t of
      (before, after)
        | T.null after -> [Literal before | not (T.null before)]
        | otherwise ->
            [Literal before | not (T.null before)]
            ++ [Wildcard]
            ++ tokenize (T.drop 1 after)

    -- Match pattern parts against string, tracking previous character
    matchParts :: [PatPart] -> Maybe Char -> String -> Bool
    matchParts [] _ str = all isWhitespace str
    matchParts [Wildcard] _ _ = True  -- Trailing wildcard matches anything
    matchParts (Wildcard : rest) _ str =
      any (\(prev, s) -> matchParts rest prev s) (suffixesWithPrev str)
    matchParts (Literal lit : rest) prev str =
      let litStr = T.unpack (T.strip lit)
      in case findWordBoundaryMatch prev litStr str of
           Nothing -> False
           Just (newPrev, remaining) -> matchParts rest newPrev remaining

    -- Find a word-boundary-aware match and return (last char of match, remaining string)
    findWordBoundaryMatch :: Maybe Char -> String -> String -> Maybe (Maybe Char, String)
    findWordBoundaryMatch _ [] str = Just (Nothing, str)
    findWordBoundaryMatch _ _ [] = Nothing
    findWordBoundaryMatch prev needle haystack@(h:hs)
      | startsWithAtWord needle prev haystack =
          let afterMatch = drop (length needle) haystack
              lastNeedle = if null needle then prev else Just (last needle)
          in Just (lastNeedle, afterMatch)
      | otherwise = findWordBoundaryMatch (Just h) needle hs

    startsWithAtWord :: String -> Maybe Char -> String -> Bool
    startsWithAtWord [] _ _ = True
    startsWithAtWord _ _ [] = False
    startsWithAtWord needle prev haystack =
      let needleLen = length needle
          match = take needleLen haystack
          afterMatch = drop needleLen haystack
      in needle == match &&
         isWordStart prev &&
         isWordEnd (headMay afterMatch)

    isWhitespace :: Char -> Bool
    isWhitespace c = c `elem` [' ', '\t', '\n', '\r']

    -- Generate suffixes with the previous character
    suffixesWithPrev :: String -> [(Maybe Char, String)]
    suffixesWithPrev = go Nothing
      where
        go prev [] = [(prev, [])]
        go prev str@(x:xs) = (prev, str) : go (Just x) xs

-- | Pattern part for tokenized patterns
data PatPart = Literal Text | Wildcard
  deriving (Eq, Show)

-- | Create a diagnostic for a pattern match
mkPatternDiagnostic :: FilePath -> FunctionInfo -> Int -> Text -> PatternRule -> Diagnostic
mkPatternDiagnostic path fi lineNum lineText rule = Diagnostic
  { diagSpan = SrcSpan
      { srcSpanFile = path
      , srcSpanStartLine = lineNum
      , srcSpanStartCol = 1
      , srcSpanEndLine = lineNum
      , srcSpanEndCol = T.length lineText + 1
      }
  , diagSeverity = toSeverity (prSeverity rule)
  , diagKind = CodePattern
  , diagMessage = prMessage rule
  , diagCode = Just $ "pattern/" <> prName rule
  , diagFixes = case prFix rule of
      Nothing -> []
      Just fix -> [Fix
        { fixTitle = "Apply: " <> fix
        , fixEdits = [FixEdit
            (SrcSpan path lineNum 1 lineNum (T.length lineText + 1))
            (applyPatternFix (prMatch rule) fix lineText)]
        , fixIsPreferred = True
        }]
  , diagRelated = []
  }
  where
    toSeverity RSError = Error
    toSeverity RSWarning = Warning
    toSeverity RSSuggestion = Suggestion
    toSeverity RSInfo = Info

-- | Apply a pattern fix to a line of code
applyPatternFix :: Text -> Text -> Text -> Text
applyPatternFix pat replacement text =
  T.replace pat replacement text

-- | Apply a pattern rule to transform code
applyPatternRule :: PatternRule -> Text -> Maybe Text
applyPatternRule rule text =
  if matchPattern (prMatch rule) text
  then Just $ maybe text (\fix -> applyPatternFix (prMatch rule) fix text) (prFix rule)
  else Nothing

--------------------------------------------------------------------------------
-- Default Patterns
--------------------------------------------------------------------------------

-- | Default pattern rules (common anti-patterns)
defaultPatterns :: [PatternRule]
defaultPatterns =
  [ PatternRule
      { prName = "avoid-head"
      , prMatch = "head"
      , prFix = Just "headMay"
      , prWhere = Nothing
      , prSeverity = RSWarning
      , prMessage = "Use headMay instead of partial head function"
      }
  , PatternRule
      { prName = "avoid-tail"
      , prMatch = "tail"
      , prFix = Just "tailMay"
      , prWhere = Nothing
      , prSeverity = RSWarning
      , prMessage = "Use tailMay instead of partial tail function"
      }
  , PatternRule
      { prName = "avoid-fromJust"
      , prMatch = "fromJust"
      , prFix = Nothing
      , prWhere = Nothing
      , prSeverity = RSWarning
      , prMessage = "fromJust is partial - consider pattern matching or fromMaybe"
      }
  , PatternRule
      { prName = "avoid-read"
      , prMatch = "read"
      , prFix = Just "readMaybe"
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Consider using readMaybe for safer parsing"
      }
  , PatternRule
      { prName = "prefer-foldl-prime"
      , prMatch = "foldl "
      , prFix = Just "foldl' "
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Use foldl' (strict) instead of foldl to avoid space leaks"
      }
  , PatternRule
      { prName = "redundant-do"
      , prMatch = "do return"
      , prFix = Just "pure"
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Redundant do with return - use pure directly"
      }
  ]
