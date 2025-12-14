{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Rules.Patterns
-- Description : Code pattern matching and suggestions
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module implements pattern-based linting rules, similar to HLint
-- but with project-specific patterns.
module Argus.Rules.Patterns
  ( -- * Pattern checking
    checkPatterns
  , matchPattern
  , applyPatternRule

    -- * Built-in patterns
  , defaultPatterns

    -- * Rule categories
  , partialFunctionRules
  , performanceRules
  , styleRules
  , securityRules
  , modernizationRules
  ) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Analysis.Syntactic
import Argus.Config (PatternsConfig(..), PatternRule(..), RuleSeverity(..), patternRuleToRule)
import Argus.Types
import Argus.Rules.Types qualified as RT
import Argus.Utils (isWordStart, isWordEnd, headMay)

--------------------------------------------------------------------------------
-- Pattern Checking
--------------------------------------------------------------------------------

-- | Check code against pattern rules
checkPatterns :: PatternsConfig -> FilePath -> Text -> [FunctionInfo] -> [Diagnostic]
checkPatterns cfg path _source functions
  | not (patternsEnabled cfg) = []
  | otherwise = concatMap (checkFunction (patternsRules cfg)) functions
  where
    checkFunction _rules fi = concatMap (checkBodyLine path fi) (fiBody fi)

    checkBodyLine filePath fi (lineNum, lineText) =
      mapMaybe (tryMatchRule filePath fi lineNum lineText) (patternsRules cfg)

-- | Try to match a pattern rule against a line of code
tryMatchRule :: FilePath -> FunctionInfo -> Int -> Text -> RT.Rule -> Maybe Diagnostic
tryMatchRule path fi lineNum lineText rule =
  -- Strip string literals before matching to avoid false positives on test strings
  if matchPattern (RT.rulePatternToText (RT.rulePattern rule)) (stripStringLiterals lineText)
  then Just $ mkPatternDiagnostic path fi lineNum lineText rule
  else Nothing

-- | Strip content from string literals, preserving the structure
-- "foo head bar" -> "foo  bar"
-- This prevents false positives when patterns like "head" appear in test strings
stripStringLiterals :: Text -> Text
stripStringLiterals = T.pack . go False . T.unpack
  where
    go _ [] = []
    go False ('\\':'"':rest) = '\\' : '"' : go False rest  -- escaped quote outside string
    go False ('"':rest) = '"' : go True rest               -- enter string
    go True ('\\':'"':rest) = ' ' : ' ' : go True rest     -- escaped quote inside string
    go True ('"':rest) = '"' : go False rest               -- exit string
    go True (_:rest) = ' ' : go True rest                  -- inside string, replace with space
    go inStr (c:rest) = c : go inStr rest                  -- outside string, keep char

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
  deriving stock (Eq, Show)

-- | Create a diagnostic for a pattern match
mkPatternDiagnostic :: FilePath -> FunctionInfo -> Int -> Text -> RT.Rule -> Diagnostic
mkPatternDiagnostic path _fi lineNum lineText rule =
  let patText = RT.rulePatternToText (RT.rulePattern rule)
      -- Find the actual column where the pattern matches
      (startCol, matchLen) = case findPatternPosition patText lineText of
        Just (col, len) -> (col, len)
        Nothing -> (1, T.length lineText)  -- Fallback to full line
      endCol = startCol + matchLen
  in Diagnostic
    { diagSpan = mkSrcSpanRaw path lineNum startCol lineNum endCol
    , diagSeverity = RT.ruleSeverity rule
    , diagKind = CodePattern
    , diagMessage = RT.ruleMessage rule
    , diagCode = Just $ RT.ruleId rule
    , diagFixes = case RT.ruleReplacement rule of
        Nothing -> []
        Just fix -> [Fix
          { fixTitle = "Apply: " <> fix
          , fixEdits = [FixEdit
              (mkSrcSpanRaw path lineNum startCol lineNum endCol)
              fix]  -- Just the replacement text, not the entire line
          , fixIsPreferred = True
          , fixAddImports = map RT.importSpecToFixImport (RT.ruleAddImports rule)
          , fixRemoveImports = RT.ruleRemoveImports rule
          , fixCategory = RT.categoryToFixCategory (RT.ruleCategory rule)
          , fixSafety = RT.safetyToFixSafety (RT.ruleSafety rule)
          }]
    , diagRelated = []
    }

-- | Find the position and length of a pattern match in text
-- Returns (1-indexed column, length of match)
findPatternPosition :: Text -> Text -> Maybe (Int, Int)
findPatternPosition pat text
  | containsWildcard pat = findWildcardPosition pat text
  | otherwise = findIdentifierPosition pat text

-- | Find position of an identifier pattern (no wildcards)
findIdentifierPosition :: Text -> Text -> Maybe (Int, Int)
findIdentifierPosition ident text = go 0 Nothing (T.unpack text) (T.unpack ident)
  where
    identLen = T.length ident
    go :: Int -> Maybe Char -> String -> String -> Maybe (Int, Int)
    go _ _ [] _ = Nothing
    go pos prev haystack@(h:hs) needle
      | needle `startsWithAt` haystack =
          let afterMatch = drop (length needle) haystack
          in if isWordStart prev && isWordEnd (headMay afterMatch)
             then Just (pos + 1, identLen)  -- 1-indexed column
             else go (pos + 1) (Just h) hs needle
      | otherwise = go (pos + 1) (Just h) hs needle

    startsWithAt [] _ = True
    startsWithAt _ [] = False
    startsWithAt (n:ns) (x:xs) = n == x && startsWithAt ns xs

-- | Find position of a wildcard pattern
-- For wildcard patterns, find the first concrete part's position
findWildcardPosition :: Text -> Text -> Maybe (Int, Int)
findWildcardPosition pat text =
  -- Extract the first literal part from the pattern
  let parts = tokenizePattern pat
      firstLiteral = headMay [lit | Literal lit <- parts, not (T.null (T.strip lit))]
  in case firstLiteral of
    Just lit -> do
      (col, _) <- findIdentifierPosition (T.strip lit) text
      -- For wildcard patterns, we match the entire expression
      -- Return position of first part and estimate total match length
      pure (col, estimateMatchLength pat text col)
    Nothing -> Nothing  -- All wildcards, can't determine position

-- | Estimate the length of a wildcard pattern match
estimateMatchLength :: Text -> Text -> Int -> Int
estimateMatchLength pat text startCol =
  let parts = tokenizePattern pat
      -- If pattern ends with wildcard, match to end of meaningful content
      -- Otherwise, find where the last literal part ends
      textFromStart = T.drop (startCol - 1) text
  in case reverse parts of
    (Wildcard : _) -> T.length textFromStart  -- Match to end
    (Literal lit : _) ->
      -- Find where the last literal ends
      case T.breakOn (T.strip lit) textFromStart of
        (before, after)
          | not (T.null after) -> T.length before + T.length (T.strip lit)
          | otherwise -> T.length textFromStart
    [] -> T.length textFromStart

-- | Tokenize pattern into parts for analysis
tokenizePattern :: Text -> [PatPart]
tokenizePattern t = case T.breakOn "*" t of
  (before, after)
    | T.null after -> [Literal before | not (T.null before)]
    | otherwise ->
        [Literal before | not (T.null before)]
        ++ [Wildcard]
        ++ tokenizePattern (T.drop 1 after)

-- | Apply a pattern fix to a line of code
applyPatternFix :: Text -> Text -> Text -> Text
applyPatternFix pat replacement text =
  T.replace pat replacement text

-- | Apply a pattern rule to transform code
applyPatternRule :: RT.Rule -> Text -> Maybe Text
applyPatternRule rule text =
  let patText = RT.rulePatternToText (RT.rulePattern rule)
  in if matchPattern patText text
     then Just $ maybe text (\fix -> applyPatternFix patText fix text) (RT.ruleReplacement rule)
     else Nothing

--------------------------------------------------------------------------------
-- Default Patterns
--------------------------------------------------------------------------------

-- | Default pattern rules (common anti-patterns)
defaultPatterns :: [RT.Rule]
defaultPatterns = concat
  [ partialFunctionRules
  , performanceRules
  , styleRules
  , securityRules
  , modernizationRules
  ]

-- | Rules for partial functions
partialFunctionRules :: [RT.Rule]
partialFunctionRules = map patternRuleToRule
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
      { prName = "avoid-init"
      , prMatch = "init"
      , prFix = Just "initMay"
      , prWhere = Nothing
      , prSeverity = RSWarning
      , prMessage = "Use initMay instead of partial init function"
      }
  , PatternRule
      { prName = "avoid-last"
      , prMatch = "last"
      , prFix = Just "lastMay"
      , prWhere = Nothing
      , prSeverity = RSWarning
      , prMessage = "Use lastMay instead of partial last function"
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
      { prName = "avoid-maximum"
      , prMatch = "maximum"
      , prFix = Just "maximumMay"
      , prWhere = Nothing
      , prSeverity = RSWarning
      , prMessage = "maximum is partial on empty lists - use maximumMay or maximumDef"
      }
  , PatternRule
      { prName = "avoid-minimum"
      , prMatch = "minimum"
      , prFix = Just "minimumMay"
      , prWhere = Nothing
      , prSeverity = RSWarning
      , prMessage = "minimum is partial on empty lists - use minimumMay or minimumDef"
      }
  , PatternRule
      { prName = "avoid-foldr1"
      , prMatch = "foldr1"
      , prFix = Just "foldr1May"
      , prWhere = Nothing
      , prSeverity = RSWarning
      , prMessage = "foldr1 is partial on empty lists - use foldr with a default or foldr1May"
      }
  , PatternRule
      { prName = "avoid-foldl1"
      , prMatch = "foldl1"
      , prFix = Just "foldl1May"
      , prWhere = Nothing
      , prSeverity = RSWarning
      , prMessage = "foldl1 is partial on empty lists - use foldl' with a default"
      }
  , PatternRule
      { prName = "avoid-error"
      , prMatch = "error"
      , prFix = Nothing
      , prWhere = Nothing
      , prSeverity = RSWarning
      , prMessage = "Avoid error - consider using Either, Maybe, or MonadError for error handling"
      }
  , PatternRule
      { prName = "avoid-undefined"
      , prMatch = "undefined"
      , prFix = Nothing
      , prWhere = Nothing
      , prSeverity = RSError
      , prMessage = "undefined should not be used in production code"
      }
  , PatternRule
      { prName = "avoid-bang-bang"
      , prMatch = "!!"
      , prFix = Just "atMay"
      , prWhere = Nothing
      , prSeverity = RSWarning
      , prMessage = "(!!) is partial - use atMay or safe indexing"
      }
  , PatternRule
      { prName = "avoid-cycle"
      , prMatch = "cycle"
      , prFix = Nothing
      , prWhere = Nothing
      , prSeverity = RSWarning
      , prMessage = "cycle produces an infinite list - ensure bounded consumption"
      }
  ]

-- | Performance-related rules
performanceRules :: [RT.Rule]
performanceRules = map patternRuleToRule
  [ PatternRule
      { prName = "prefer-foldl-prime"
      , prMatch = "foldl "
      , prFix = Just "foldl' "
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Use foldl' (strict) instead of foldl to avoid space leaks"
      }
  , PatternRule
      { prName = "avoid-length-0"
      , prMatch = "length * == 0"
      , prFix = Just "null"
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Use null instead of length == 0 for O(1) emptiness check"
      }
  , PatternRule
      { prName = "avoid-length-comparison"
      , prMatch = "length * > 0"
      , prFix = Just "not . null"
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Use 'not . null' instead of 'length > 0' for O(1) check"
      }
  , PatternRule
      { prName = "prefer-text-pack"
      , prMatch = "T.pack $ show"
      , prFix = Just "tshow"
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Consider using tshow for more efficient Text conversion"
      }
  , PatternRule
      { prName = "avoid-concat-map"
      , prMatch = "concat $ map"
      , prFix = Just "concatMap"
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Use concatMap instead of concat . map"
      }
  , PatternRule
      { prName = "avoid-map-head"
      , prMatch = "map * $ filter"
      , prFix = Nothing
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Consider using mapMaybe for combined filtering and mapping"
      }
  , PatternRule
      { prName = "avoid-nub"
      , prMatch = "nub"
      , prFix = Just "ordNub"
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "nub is O(n²) - use ordNub from containers-extra for O(n log n)"
      }
  , PatternRule
      { prName = "avoid-string-concat"
      , prMatch = "++ \""
      , prFix = Nothing
      , prWhere = Nothing
      , prSeverity = RSInfo
      , prMessage = "String concatenation is slow - consider using Text or Builder"
      }
  , PatternRule
      { prName = "prefer-strict-state"
      , prMatch = "StateT"
      , prFix = Just "StateT (from Control.Monad.Trans.State.Strict)"
      , prWhere = Nothing
      , prSeverity = RSInfo
      , prMessage = "Consider using strict StateT to avoid space leaks"
      }
  ]

-- | Style and code quality rules
styleRules :: [RT.Rule]
styleRules = map patternRuleToRule
  [ PatternRule
      { prName = "redundant-do"
      , prMatch = "do return"
      , prFix = Just "pure"
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Redundant do with return - use pure directly"
      }
  , PatternRule
      { prName = "redundant-dollar"
      , prMatch = "$ id"
      , prFix = Nothing
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Redundant $ id - remove it"
      }
  , PatternRule
      { prName = "prefer-pure"
      , prMatch = "return"
      , prFix = Just "pure"
      , prWhere = Nothing
      , prSeverity = RSInfo
      , prMessage = "Consider using pure instead of return (Applicative is more general)"
      }
  , PatternRule
      { prName = "prefer-void"
      , prMatch = "_ <- "
      , prFix = Just "void $ "
      , prWhere = Nothing
      , prSeverity = RSInfo
      , prMessage = "Consider using void to discard the result"
      }
  , PatternRule
      { prName = "prefer-when"
      , prMatch = "if * then * else pure ()"
      , prFix = Just "when"
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Use when instead of if-then-else with pure ()"
      }
  , PatternRule
      { prName = "prefer-unless"
      , prMatch = "if not * then * else pure ()"
      , prFix = Just "unless"
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Use unless instead of if not then else pure ()"
      }
  , PatternRule
      { prName = "redundant-lambda"
      , prMatch = "\\x -> f x"
      , prFix = Just "f"
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Redundant lambda - use point-free style"
      }
  , PatternRule
      { prName = "prefer-maybe-fold"
      , prMatch = "maybe * id"
      , prFix = Just "fromMaybe"
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Use fromMaybe instead of maybe default id"
      }
  , PatternRule
      { prName = "prefer-bool"
      , prMatch = "if * then True else False"
      , prFix = Nothing
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Redundant if - just use the condition directly"
      }
  , PatternRule
      { prName = "avoid-if-not"
      , prMatch = "if not *"
      , prFix = Nothing
      , prWhere = Nothing
      , prSeverity = RSInfo
      , prMessage = "Consider swapping if branches to avoid negation"
      }
  ]

-- | Security-related rules
securityRules :: [RT.Rule]
securityRules = map patternRuleToRule
  [ PatternRule
      { prName = "avoid-unsafePerformIO"
      , prMatch = "unsafePerformIO"
      , prFix = Nothing
      , prWhere = Nothing
      , prSeverity = RSError
      , prMessage = "unsafePerformIO breaks referential transparency - avoid unless absolutely necessary"
      }
  , PatternRule
      { prName = "avoid-unsafeCoerce"
      , prMatch = "unsafeCoerce"
      , prFix = Nothing
      , prWhere = Nothing
      , prSeverity = RSError
      , prMessage = "unsafeCoerce is unsafe and can cause runtime crashes"
      }
  , PatternRule
      { prName = "avoid-unsafeInterleaveIO"
      , prMatch = "unsafeInterleaveIO"
      , prFix = Nothing
      , prWhere = Nothing
      , prSeverity = RSWarning
      , prMessage = "unsafeInterleaveIO can cause unpredictable behavior"
      }
  , PatternRule
      { prName = "avoid-inlinePerformIO"
      , prMatch = "inlinePerformIO"
      , prFix = Nothing
      , prWhere = Nothing
      , prSeverity = RSError
      , prMessage = "inlinePerformIO is extremely unsafe"
      }
  , PatternRule
      { prName = "avoid-accursedUnutterablePerformIO"
      , prMatch = "accursedUnutterablePerformIO"
      , prFix = Nothing
      , prWhere = Nothing
      , prSeverity = RSError
      , prMessage = "accursedUnutterablePerformIO is named that way for a reason"
      }
  ]

-- | Modernization rules (prefer newer APIs)
modernizationRules :: [RT.Rule]
modernizationRules = map patternRuleToRule
  [ PatternRule
      { prName = "prefer-traverse_"
      , prMatch = "mapM_"
      , prFix = Just "traverse_"
      , prWhere = Nothing
      , prSeverity = RSInfo
      , prMessage = "Consider using traverse_ (more general Applicative constraint)"
      }
  , PatternRule
      { prName = "prefer-for_"
      , prMatch = "forM_"
      , prFix = Just "for_"
      , prWhere = Nothing
      , prSeverity = RSInfo
      , prMessage = "Consider using for_ (more general Applicative constraint)"
      }
  , PatternRule
      { prName = "prefer-foldMap"
      , prMatch = "mconcat $ map"
      , prFix = Just "foldMap"
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Use foldMap instead of mconcat . map"
      }
  , PatternRule
      { prName = "prefer-bimap"
      , prMatch = "first * $ second"
      , prFix = Just "bimap"
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Use bimap instead of first and second"
      }
  , PatternRule
      { prName = "prefer-coerce"
      , prMatch = "runIdentity $ Identity"
      , prFix = Just "coerce"
      , prWhere = Nothing
      , prSeverity = RSSuggestion
      , prMessage = "Use coerce for zero-cost newtype conversions"
      }
  ]
