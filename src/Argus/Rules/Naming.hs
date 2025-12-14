{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Rules.Naming
-- Description : Naming convention rules
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module implements naming convention checking for types and variables,
-- providing the same functionality as the legacy linter with improved
-- pattern matching and diagnostics.
module Argus.Rules.Naming
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

import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Analysis.Syntactic
import Argus.Config
import Argus.Types

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
checkTypeSignatures rules path _source = concatMap checkFunction
  where
    checkFunction fi = case fiSignature fi of
      Nothing -> []
      Just sig -> concatMap (checkArgType fi sig) (zip3 [0..] (tiArgTypes sig) (tiArgSpans sig))

    checkArgType fi sig (idx, argType, argSpan) =
      mapMaybe (matchAndDiagnose fi sig argType argSpan idx) rules

    matchAndDiagnose fi sig argType argSpan idx rule =
      if matchTypeRule (trPattern rule) argType
      then Just $ mkTypeSignatureDiagnostic path fi sig argType argSpan rule idx
      else Nothing

-- | Match a type against a type rule pattern
matchTypeRule :: Text -> Text -> Bool
matchTypeRule pat argType =
  matchWildcardPattern pat argType

-- | Create a diagnostic for a type signature violation
mkTypeSignatureDiagnostic :: FilePath -> FunctionInfo -> TypeInfo -> Text -> SrcSpan -> TypeRule -> Int -> Diagnostic
mkTypeSignatureDiagnostic _path _fi _sig argType argSpan rule _idx = Diagnostic
  { diagSpan = argSpan  -- Use the specific argument type's location
  , diagSeverity = toSeverity (trSeverity rule)
  , diagKind = TypeSignature
  , diagMessage = fromMaybe defaultMsg (trMessage rule)
  , diagCode = Just $ "naming/type/" <> trPattern rule
  , diagFixes = [Fix
      { fixTitle = "Change type to " <> trReplacement rule
      , fixEdits = [FixEdit argSpan (trReplacement rule)]
      , fixIsPreferred = True
      , fixAddImports = []
      , fixRemoveImports = []
      , fixCategory = FCStyle
      , fixSafety = FSMostly
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
      && not (isNameCorrect (aiName arg) (vrTo rule))  -- Don't lint if already correct

-- | Check if a variable name is already correct (or uses valid underscore prefix)
isNameCorrect :: Text -> Text -> Bool
isNameCorrect name expected =
  name == expected
  || T.stripPrefix "_" name == Just expected  -- _locationK matches locationK

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
      , fixAddImports = []
      , fixRemoveImports = []
      , fixCategory = FCStyle
      , fixSafety = FSMostly
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
      , srcSpanStartLine = Line ln
      , srcSpanStartCol = Column 0
      , srcSpanEndLine = Line ln
      , srcSpanEndCol = Column 0
      }

    mkUsageEdit :: Text -> (Int, Text) -> FixEdit
    mkUsageEdit _src (ln, _) = FixEdit (mkUsageSpan ln) ""  -- Simplified

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
    matchChars ('*':ps) str2 = any (matchChars ps) (suffixes str2)
    matchChars ('_':ps) (_:ss) = matchChars ps ss
    matchChars (p:ps) (s:ss) = p == s && matchChars ps ss
    matchChars _ _ = False

    suffixes [] = [[]]
    suffixes s@(_:xs) = s : suffixes xs

-- | Apply a replacement pattern to a variable name
-- Preserves underscore prefixes from the original when parts are "_"
-- E.g., "Entity _ uV" with target "Entity uK uV" -> "Entity _uK uV"
applyReplacement :: Text -> Text -> Text
applyReplacement original replacement =
  case T.breakOn "@" original of
    (_before, matched)
      | not (T.null matched) ->
          -- Handle as-pattern: x@(Entity ...) -> pE@(Entity ...)
          let rest = T.drop 1 matched
          in replacement <> "@" <> rest
    _ -> preserveUnderscores (trimParens original) replacement

-- | Preserve underscore prefixes from original when parts are "_"
-- "Entity _ uV" + "Entity uK uV" -> "Entity _uK uV"
preserveUnderscores :: Text -> Text -> Text
preserveUnderscores original replacement =
  let origParts = T.words original
      replParts = T.words replacement
  in if length origParts == length replParts
     then T.unwords $ zipWith mergeUnderscores origParts replParts
     else replacement
  where
    -- If original part is "_", prefix the replacement with "_"
    mergeUnderscores orig repl
      | orig == "_" = "_" <> repl
      | otherwise = repl

-- | Trim parentheses from a text
trimParens :: Text -> Text
trimParens = T.dropWhileEnd (`elem` [')', ' ']) . T.dropWhile (`elem` ['(', ' '])

-- | Extract parts from a structured replacement pattern
extractReplacementParts :: Text -> (Text, [Text])
extractReplacementParts pat =
  let (prefix, rest) = T.breakOn "{" pat
  in if T.null rest
     then (pat, [])
     else
       let (inside, suffix) = T.breakOn "}" (T.drop 1 rest)
           parts = T.splitOn "|" inside
           (morPrefix, morParts) = extractReplacementParts (T.drop 1 suffix)
              in (prefix <> morPrefix, parts ++ morParts)

--------------------------------------------------------------------------------
-- Variable replacement in source
--------------------------------------------------------------------------------

-- | Replace a variable in source text, respecting word boundaries (unused - commented out)
-- replaceVariable :: Text -> Text -> Text -> Text
_replaceVariable :: Text -> Text -> Text -> Text
_replaceVariable from to text =
  T.pack $ go (T.unpack text) (T.unpack from) (T.unpack to)
  where
    go [] _ _ = []
    go str@(c:cs) fromStr toStr
      | fromStr `startsWith` str && notWordContinuation =
          toStr ++ go (drop (length fromStr) str) fromStr toStr
      | otherwise = c : go cs fromStr toStr
      where
        afterMatch = drop (length fromStr) str
        notWordContinuation = case afterMatch of
          [] -> True
          (h:_) -> not (isWordChar h)
        isWordChar ch = isAlphaNum ch || ch == '_'

    startsWith [] _ = True
    startsWith _ [] = False
    startsWith (p:ps) (s:ss) = p == s && startsWith ps ss
