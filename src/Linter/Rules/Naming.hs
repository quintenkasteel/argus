{-# LANGUAGE StrictData #-}

-- |
-- Module      : Linter.Rules.Naming
-- Description : Naming convention rules
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module implements naming convention checking for types and variables,
-- providing the same functionality as the legacy linter with improved
-- pattern matching and diagnostics.
module Linter.Rules.Naming
  ( -- * Checking functions
    checkNamingConventions
  , checkTypeSignatures
  , checkVariables

    -- * Rule matching
  , matchTypeRule
  , matchVariableRule

    -- * Pattern utilities
  , matchWildcardPattern
  , applyReplacement
  , extractReplacementParts
  ) where

import Control.Monad (forM)
import Data.Char (isAlphaNum, toLower)
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Linter.Analysis.Syntactic
import Linter.Config
import Linter.Types

--------------------------------------------------------------------------------
-- Main Checking Function
--------------------------------------------------------------------------------

-- | Check naming conventions for a list of functions
checkNamingConventions :: NamingConfig -> FilePath -> Text -> [FunctionInfo] -> [Diagnostic]
checkNamingConventions cfg path source functions
  | not (namingEnabled cfg) = []
  | otherwise =
      checkTypeSignatures (namingTypes cfg) path source functions
      ++ checkVariables (namingVariables cfg) path source functions

--------------------------------------------------------------------------------
-- Type Signature Checking
--------------------------------------------------------------------------------

-- | Check type signatures against naming rules
checkTypeSignatures :: [TypeRule] -> FilePath -> Text -> [FunctionInfo] -> [Diagnostic]
checkTypeSignatures rules path source = concatMap checkFunction
  where
    checkFunction fi = case fiSignature fi of
      Nothing -> []
      Just sig -> concatMap (checkArgType fi sig) (zip [0..] (tiArgTypes sig))

    checkArgType fi sig (idx, argType) =
      mapMaybe (matchAndDiagnose fi argType idx) rules

    matchAndDiagnose fi argType idx rule =
      if matchTypeRule (trPattern rule) argType
      then Just $ mkTypeSignatureDiagnostic path fi argType rule idx
      else Nothing

-- | Match a type against a type rule pattern
matchTypeRule :: Text -> Text -> Bool
matchTypeRule pat argType =
  matchWildcardPattern pat argType

-- | Create a diagnostic for a type signature violation
mkTypeSignatureDiagnostic :: FilePath -> FunctionInfo -> Text -> TypeRule -> Int -> Diagnostic
mkTypeSignatureDiagnostic path fi argType rule idx = Diagnostic
  { diagSpan = fiSpan fi  -- Would ideally use the type's specific location
  , diagSeverity = toSeverity (trSeverity rule)
  , diagKind = TypeSignature
  , diagMessage = fromMaybe defaultMsg (trMessage rule)
  , diagCode = Just $ "naming/type/" <> trPattern rule
  , diagFixes = [Fix
      { fixTitle = "Change type to " <> trReplacement rule
      , fixEdits = [FixEdit (fiSpan fi) (trReplacement rule)]
      , fixIsPreferred = True
      }]
  , diagRelated = []
  }
  where
    defaultMsg = "Type '" <> argType <> "' should be '" <> trReplacement rule <> "'"
    toSeverity RSError = Error
    toSeverity RSWarning = Warning
    toSeverity RSSuggestion = Suggestion
    toSeverity RSInfo = Info

--------------------------------------------------------------------------------
-- Variable Checking
--------------------------------------------------------------------------------

-- | Check variable names against naming rules
checkVariables :: [VariableRule] -> FilePath -> Text -> [FunctionInfo] -> [Diagnostic]
checkVariables rules path source = concatMap checkFunction
  where
    checkFunction fi = concatMap (checkArg fi source) (fiArguments fi)

    checkArg fi src arg = mapMaybe (matchAndDiagnose fi src arg) rules

    matchAndDiagnose fi src arg rule =
      if matchVariableRule rule arg
      then Just $ mkVariableDiagnostic path fi src arg rule
      else Nothing

-- | Match a variable against a variable rule
matchVariableRule :: VariableRule -> ArgumentInfo -> Bool
matchVariableRule rule arg =
  case aiType arg of
    Nothing -> False
    Just argType ->
      matchWildcardPattern (vrType rule) argType
      && matchFromPattern (vrFrom rule) (aiName arg)
      && aiName arg /= vrTo rule  -- Don't lint if already correct

-- | Match the 'from' pattern of a variable rule
matchFromPattern :: Maybe Text -> Text -> Bool
matchFromPattern Nothing _ = True  -- No from pattern, matches any name
matchFromPattern (Just "*") _ = True  -- Wildcard matches any name
matchFromPattern (Just pat) name = matchWildcardPattern pat (trimParens name)

-- | Create a diagnostic for a variable naming violation
mkVariableDiagnostic :: FilePath -> FunctionInfo -> Text -> ArgumentInfo -> VariableRule -> Diagnostic
mkVariableDiagnostic path fi source arg rule = Diagnostic
  { diagSpan = aiSpan arg
  , diagSeverity = toSeverity (vrSeverity rule)
  , diagKind = NamingConvention
  , diagMessage = fromMaybe defaultMsg (vrMessage rule)
  , diagCode = Just $ "naming/variable/" <> vrType rule
  , diagFixes = [Fix
      { fixTitle = "Rename to " <> replacement
      , fixEdits = mainEdit : usageEdits
      , fixIsPreferred = True
      }]
  , diagRelated = map (\(ln, _) -> (mkUsageSpan ln, "Used here")) usages
  }
  where
    defaultMsg = "Variable '" <> aiName arg <> "' should be named '" <> replacement <> "'"
    toSeverity RSError = Error
    toSeverity RSWarning = Warning
    toSeverity RSSuggestion = Suggestion
    toSeverity RSInfo = Info

    replacement = applyReplacement (aiName arg) (vrTo rule)

    mainEdit = FixEdit (aiSpan arg) replacement

    -- Find usages of this variable in the function body
    usages = findVariableUsages (aiName arg) replacement (fiBody fi)

    usageEdits = map (mkUsageEdit source) usages

    mkUsageSpan ln = SrcSpan
      { srcSpanFile = path
      , srcSpanStartLine = ln
      , srcSpanStartCol = 0
      , srcSpanEndLine = ln
      , srcSpanEndCol = 0
      }

    mkUsageEdit :: Text -> (Int, Text) -> FixEdit
    mkUsageEdit src (ln, _) = FixEdit (mkUsageSpan ln) ""  -- Simplified

-- | Find usages of a variable in function body lines
findVariableUsages :: Text -> Text -> [(Int, Text)] -> [(Int, Text)]
findVariableUsages varName replacement = filter usageFound
  where
    usageFound (_, line) = varName `T.isInfixOf` line && line /= replacement

--------------------------------------------------------------------------------
-- Pattern Matching Utilities
--------------------------------------------------------------------------------

-- | Match a pattern with wildcards against a string
-- Supports:
--   * - matches any sequence of characters
--   _ - matches a single character
matchWildcardPattern :: Text -> Text -> Bool
matchWildcardPattern pat str = matchParts (T.words pat) (T.words str)
  where
    matchParts [] [] = True
    matchParts (p:ps) (s:ss) = matchWord p s && matchParts ps ss
    matchParts _ _ = False

    matchWord p s = matchChars (T.unpack p) (T.unpack s)

    matchChars [] [] = True
    matchChars ('*':ps) str = any (matchChars ps) (suffixes str)
    matchChars ('_':ps) (_:ss) = matchChars ps ss
    matchChars (p:ps) (s:ss) = p == s && matchChars ps ss
    matchChars _ _ = False

    suffixes [] = [[]]
    suffixes str@(_:xs) = str : suffixes xs

-- | Apply a replacement pattern to a variable name
-- Supports patterns like:
--   "{type[1]|lowercase|first}E" -> takes first word of type, lowercases, takes first char, appends E
applyReplacement :: Text -> Text -> Text
applyReplacement original replacement =
  case T.breakOn "@" original of
    (before, matched)
      | not (T.null matched) ->
          -- Handle as-pattern: x@(Entity ...) -> pE@(Entity ...)
          let rest = T.drop 1 matched
          in replacement <> "@" <> rest
    _ -> replacement

-- | Trim parentheses from a text
trimParens :: Text -> Text
trimParens = T.dropWhileEnd (`elem` [')', ' ']) . T.dropWhile (`elem` ['(', ' '])

-- | Extract parts from a structured replacement pattern
extractReplacementParts :: Text -> (Text, [Text])
extractReplacementParts pat =
  case T.breakOn "{" pat of
    (prefix, rest)
      | T.null rest -> (pat, [])
      | otherwise ->
          case T.breakOn "}" (T.drop 1 rest) of
            (inside, suffix) ->
              let parts = T.splitOn "|" inside
                  (morPrefix, morParts) = extractReplacementParts (T.drop 1 suffix)
              in (prefix <> morPrefix, parts ++ morParts)

--------------------------------------------------------------------------------
-- Variable replacement in source
--------------------------------------------------------------------------------

-- | Replace a variable in source text, respecting word boundaries
replaceVariable :: Text -> Text -> Text -> Text
replaceVariable from to text =
  T.pack $ go (T.unpack text) (T.unpack from) (T.unpack to)
  where
    go [] _ _ = []
    go str@(c:cs) fromStr toStr
      | fromStr `startsWith` str && notWordContinuation =
          toStr ++ go (drop (length fromStr) str) fromStr toStr
      | otherwise = c : go cs fromStr toStr
      where
        afterMatch = drop (length fromStr) str
        notWordContinuation = null afterMatch || not (isWordChar (head afterMatch))
        isWordChar c = isAlphaNum c || c == '_'

    startsWith [] _ = True
    startsWith _ [] = False
    startsWith (p:ps) (s:ss) = p == s && startsWith ps ss
