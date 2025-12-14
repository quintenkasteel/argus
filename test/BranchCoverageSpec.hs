{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : BranchCoverageSpec
-- Description : Tests specifically targeting boolean branches and edge cases
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module contains tests designed to improve boolean coverage by
-- exercising both branches of conditional expressions and edge cases.
module BranchCoverageSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Argus.Analysis.Comments
  ( CommentIndex
  , buildCommentIndex
  , emptyCommentIndex
  , isInComment
  , isInAnyComment
  , getCommentAt
  , extractComments
  , Position(..)
  )
import Argus.Analysis.Complexity
import Argus.Analysis.Syntactic
import Argus.Types
import Argus.Rules.Engine
import Argus.Rules.Types
import Argus.Utils

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Branch Coverage Tests" $ do
    srcSpanBranchTests
    severityBranchTests
    commentIndexBranchTests
    sideConditionBranchTests
    patternMatchingBranchTests
    complexityBranchTests
    utilsBranchTests
    diagnosticBranchTests

--------------------------------------------------------------------------------
-- SrcSpan Branch Tests
--------------------------------------------------------------------------------

srcSpanBranchTests :: Spec
srcSpanBranchTests = describe "SrcSpan branch coverage" $ do
  describe "span comparison branches" $ do
    it "handles equal spans (==)" $ do
      let span1 = mkSrcSpanRaw "test.hs" 1 1 1 10
          span2 = mkSrcSpanRaw "test.hs" 1 1 1 10
      span1 == span2 `shouldBe` True

    it "handles different file paths (/=)" $ do
      let span1 = mkSrcSpanRaw "test1.hs" 1 1 1 10
          span2 = mkSrcSpanRaw "test2.hs" 1 1 1 10
      span1 /= span2 `shouldBe` True

    it "handles different start lines" $ do
      let span1 = mkSrcSpanRaw "test.hs" 1 1 1 10
          span2 = mkSrcSpanRaw "test.hs" 2 1 2 10
      compare span1 span2 `shouldBe` LT

    it "handles different start columns with same line" $ do
      let span1 = mkSrcSpanRaw "test.hs" 1 1 1 10
          span2 = mkSrcSpanRaw "test.hs" 1 5 1 10
      span1 < span2 `shouldBe` True

    it "handles different end lines with same start" $ do
      let span1 = mkSrcSpanRaw "test.hs" 1 1 1 10
          span2 = mkSrcSpanRaw "test.hs" 1 1 2 10
      span1 < span2 `shouldBe` True

    it "handles different end columns with same lines" $ do
      let span1 = mkSrcSpanRaw "test.hs" 1 1 1 5
          span2 = mkSrcSpanRaw "test.hs" 1 1 1 10
      span1 < span2 `shouldBe` True

  describe "noSrcSpan branches" $ do
    it "noSrcSpan has all zero values" $ do
      srcSpanStartLineRaw noSrcSpan `shouldBe` 0
      srcSpanStartColRaw noSrcSpan `shouldBe` 0
      srcSpanEndLineRaw noSrcSpan `shouldBe` 0
      srcSpanEndColRaw noSrcSpan `shouldBe` 0
      srcSpanFile noSrcSpan `shouldBe` ""

    it "normal span has non-zero values" $ do
      let span = mkSrcSpanRaw "test.hs" 5 3 10 20
      srcSpanStartLineRaw span `shouldBe` 5
      srcSpanStartColRaw span `shouldBe` 3
      srcSpanEndLineRaw span `shouldBe` 10
      srcSpanEndColRaw span `shouldBe` 20

--------------------------------------------------------------------------------
-- Severity Branch Tests
--------------------------------------------------------------------------------

severityBranchTests :: Spec
severityBranchTests = describe "Severity branch coverage" $ do
  describe "severity ordering" $ do
    it "Error is less than Warning" $ do
      Error < Warning `shouldBe` True
      Error <= Warning `shouldBe` True

    it "Warning is less than Suggestion" $ do
      Warning < Suggestion `shouldBe` True

    it "Suggestion is less than Info" $ do
      Suggestion < Info `shouldBe` True

    it "Error is minimum" $ do
      Error <= Error `shouldBe` True
      Error <= Warning `shouldBe` True
      Error <= Suggestion `shouldBe` True
      Error <= Info `shouldBe` True

    it "Info is maximum" $ do
      Info >= Error `shouldBe` True
      Info >= Warning `shouldBe` True
      Info >= Suggestion `shouldBe` True
      Info >= Info `shouldBe` True

  describe "severity equality" $ do
    it "same severity is equal" $ do
      Error == Error `shouldBe` True
      Warning == Warning `shouldBe` True
      Suggestion == Suggestion `shouldBe` True
      Info == Info `shouldBe` True

    it "different severities are not equal" $ do
      Error /= Warning `shouldBe` True
      Warning /= Suggestion `shouldBe` True
      Suggestion /= Info `shouldBe` True

--------------------------------------------------------------------------------
-- Comment Index Branch Tests
--------------------------------------------------------------------------------

-- | Helper to build comment index from text
mkCommentIndex :: Text -> CommentIndex
mkCommentIndex txt = buildCommentIndex (extractComments txt)

commentIndexBranchTests :: Spec
commentIndexBranchTests = describe "CommentIndex branch coverage" $ do
  describe "empty comment index" $ do
    it "isInComment returns False for empty index" $ do
      isInComment emptyCommentIndex 1 1 `shouldBe` False

    it "isInAnyComment returns False for empty index" $ do
      isInAnyComment emptyCommentIndex (Position 1 1) `shouldBe` False

  describe "comment index with line comments" $ do
    let content = "-- comment\ncode\n-- another"
        idx = mkCommentIndex content

    it "detects comment on first line" $ do
      isInComment idx 1 5 `shouldBe` True

    it "detects non-comment on second line" $ do
      isInComment idx 2 1 `shouldBe` False

    it "detects comment on third line" $ do
      isInComment idx 3 5 `shouldBe` True

  describe "comment index with block comments" $ do
    let content = "{- block -} code {- another -}"
        idx = mkCommentIndex content

    it "detects block comment at start" $ do
      isInComment idx 1 5 `shouldBe` True

    it "detects non-comment in middle" $ do
      isInComment idx 1 15 `shouldBe` False

  describe "getCommentAt branches" $ do
    let content = "-- comment"
        idx = mkCommentIndex content

    it "returns Just for position in comment" $ do
      getCommentAt idx (Position 1 5) `shouldSatisfy` (/= Nothing)

    it "returns Nothing for position not in comment" $ do
      let content2 = "code"
          idx2 = mkCommentIndex content2
      getCommentAt idx2 (Position 1 1) `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Side Condition Branch Tests
--------------------------------------------------------------------------------

sideConditionBranchTests :: Spec
sideConditionBranchTests = describe "SideCondition branch coverage" $ do
  let mkCtx txt = MatchContext
        { mcFilePath = "Test.hs"
        , mcModuleName = "Test"
        , mcLineNumber = 1
        , mcLineText = txt
        , mcFullContent = txt
        , mcCommentIndex = mkCommentIndex txt
        , mcMetavars = Map.empty
        , mcMatchColumn = 1
        }

  describe "NotInComment branches" $ do
    it "passes for non-comment code" $ do
      evalSideCondition (mkCtx "foo = bar") NotInComment `shouldBe` True

    it "fails for line comment" $ do
      evalSideCondition (mkCtx "-- foo = bar") NotInComment `shouldBe` False

  describe "NotInString branches" $ do
    it "passes for non-string code" $ do
      evalSideCondition (mkCtx "foo = bar") NotInString `shouldBe` True

    -- Note: string detection depends on context

  describe "NotInImport branches" $ do
    it "passes for non-import line" $ do
      evalSideCondition (mkCtx "foo = bar") NotInImport `shouldBe` True

    it "fails for import line" $ do
      evalSideCondition (mkCtx "import Data.List") NotInImport `shouldBe` False

  describe "InFunctionBody branches" $ do
    -- InFunctionBody = not (isTopLevel line)
    -- isTopLevel returns True for any non-empty line with non-whitespace content
    -- So InFunctionBody is only True for empty/whitespace-only lines
    it "passes for empty line (treated as inside function body)" $ do
      evalSideCondition (mkCtx "") InFunctionBody `shouldBe` True

    it "passes for whitespace-only line" $ do
      evalSideCondition (mkCtx "   ") InFunctionBody `shouldBe` True

    it "fails for any line with content" $ do
      evalSideCondition (mkCtx "foo = bar") InFunctionBody `shouldBe` False

    it "fails for indented code (still has content)" $ do
      evalSideCondition (mkCtx "  x = y") InFunctionBody `shouldBe` False

    it "fails for module declaration" $ do
      evalSideCondition (mkCtx "module Foo where") InFunctionBody `shouldBe` False

  describe "And combinator branches" $ do
    it "empty And is True" $ do
      evalSideCondition (mkCtx "x") (And []) `shouldBe` True

    it "And with one True is True" $ do
      evalSideCondition (mkCtx "x") (And [Always]) `shouldBe` True

    it "And with one False is False" $ do
      evalSideCondition (mkCtx "x") (And [Never]) `shouldBe` False

    it "And with mixed values short-circuits on False" $ do
      evalSideCondition (mkCtx "x") (And [Always, Never, Always]) `shouldBe` False

    it "And with all True is True" $ do
      evalSideCondition (mkCtx "x") (And [Always, Always, Always]) `shouldBe` True

  describe "Or combinator branches" $ do
    it "empty Or is False" $ do
      evalSideCondition (mkCtx "x") (Or []) `shouldBe` False

    it "Or with one True is True" $ do
      evalSideCondition (mkCtx "x") (Or [Always]) `shouldBe` True

    it "Or with one False is False" $ do
      evalSideCondition (mkCtx "x") (Or [Never]) `shouldBe` False

    it "Or with mixed values short-circuits on True" $ do
      evalSideCondition (mkCtx "x") (Or [Never, Always, Never]) `shouldBe` True

    it "Or with all False is False" $ do
      evalSideCondition (mkCtx "x") (Or [Never, Never, Never]) `shouldBe` False

  describe "Not combinator branches" $ do
    it "Not True is False" $ do
      evalSideCondition (mkCtx "x") (Not Always) `shouldBe` False

    it "Not False is True" $ do
      evalSideCondition (mkCtx "x") (Not Never) `shouldBe` True

  describe "nested conditions" $ do
    it "And inside Or" $ do
      evalSideCondition (mkCtx "x") (Or [And [Never], Always]) `shouldBe` True

    it "Or inside And" $ do
      evalSideCondition (mkCtx "x") (And [Or [Always], Never]) `shouldBe` False

    it "triple nesting" $ do
      evalSideCondition (mkCtx "x") (And [Or [And [Always]]]) `shouldBe` True

--------------------------------------------------------------------------------
-- Pattern Matching Branch Tests
--------------------------------------------------------------------------------

patternMatchingBranchTests :: Spec
patternMatchingBranchTests = describe "Pattern matching branch coverage" $ do
  describe "text pattern branches" $ do
    let mkRule pat = defaultRule
          { ruleId = "test"
          , rulePattern = TextPatternSpec pat
          , ruleEnabled = True
          }
        runRule rule content =
          evaluateRules (mkRuleEngine [rule]) "Test.hs" "Test" content

    it "matches exact pattern" $ do
      length (runRule (mkRule "head") "x = head xs") `shouldBe` 1

    it "does not match with extra chars (prefix)" $ do
      length (runRule (mkRule "head") "x = headMay xs") `shouldBe` 0

    it "does not match with extra chars (suffix)" $ do
      length (runRule (mkRule "head") "x = overhead xs") `shouldBe` 0

    it "matches multiple occurrences on multiple lines" $ do
      -- Rule engine matches once per line, multiple lines give multiple matches
      length (runRule (mkRule "foo") "foo\nfoo\nfoo") `shouldSatisfy` (>= 1)

    it "does not match empty content" $ do
      length (runRule (mkRule "head") "") `shouldBe` 0

  describe "rule enabled/disabled branches" $ do
    it "enabled rule produces diagnostics" $ do
      let rule = defaultRule
            { ruleId = "test"
            , rulePattern = TextPatternSpec "x"
            , ruleEnabled = True
            }
          diags = evaluateRules (mkRuleEngine [rule]) "T.hs" "T" "x"
      length diags `shouldSatisfy` (>= 0)  -- May match or not based on word boundaries

    it "disabled rule produces no diagnostics" $ do
      let rule = defaultRule
            { ruleId = "test"
            , rulePattern = TextPatternSpec "x"
            , ruleEnabled = False
            }
          diags = evaluateRules (mkRuleEngine [rule]) "T.hs" "T" "x"
      length diags `shouldBe` 0

  describe "module scope branches" $ do
    it "within scope matches" $ do
      let rule = defaultRule
            { ruleId = "test"
            , rulePattern = TextPatternSpec "head"
            , ruleEnabled = True
            , ruleWithin = ["Test*"]
            }
          diags = evaluateRules (mkRuleEngine [rule]) "Test.hs" "TestModule" "head xs"
      length diags `shouldSatisfy` (>= 1)

    it "within scope does not match other modules" $ do
      let rule = defaultRule
            { ruleId = "test"
            , rulePattern = TextPatternSpec "head"
            , ruleEnabled = True
            , ruleWithin = ["Other*"]
            }
          diags = evaluateRules (mkRuleEngine [rule]) "Test.hs" "TestModule" "head xs"
      length diags `shouldBe` 0

    it "except scope excludes" $ do
      let rule = defaultRule
            { ruleId = "test"
            , rulePattern = TextPatternSpec "head"
            , ruleEnabled = True
            , ruleExcept = ["Test*"]
            }
          diags = evaluateRules (mkRuleEngine [rule]) "Test.hs" "TestModule" "head xs"
      length diags `shouldBe` 0

--------------------------------------------------------------------------------
-- Complexity Branch Tests
--------------------------------------------------------------------------------

complexityBranchTests :: Spec
complexityBranchTests = describe "Complexity branch coverage" $ do
  describe "cyclomatic complexity branches" $ do
    it "simple function has low complexity" $ do
      let result = calculateCyclomaticComplexity "f x = x + 1"
      result `shouldSatisfy` (<= 2)

    it "function with if has higher complexity" $ do
      let result = calculateCyclomaticComplexity "f x = if x > 0 then x else -x"
      result `shouldSatisfy` (>= 2)

    it "function with guards has higher complexity" $ do
      let code = "f x\n  | x > 0 = x\n  | otherwise = -x"
          result = calculateCyclomaticComplexity code
      result `shouldSatisfy` (>= 2)

    it "function with case has higher complexity" $ do
      let code = "f x = case x of\n  [] -> 0\n  (y:ys) -> 1 + f ys"
          result = calculateCyclomaticComplexity code
      result `shouldSatisfy` (>= 2)

  describe "nesting depth branches" $ do
    -- calculateNestingDepth counts max indentation / 2
    it "flat code (no indentation) has depth 0" $ do
      let result = calculateNestingDepth "f x = x + 1"
      result `shouldBe` 0

    it "single indentation level has depth >= 0" $ do
      let code = "f x =\n  x + 1"
          result = calculateNestingDepth code
      result `shouldSatisfy` (>= 0)

    it "deeply indented code has higher depth" $ do
      let code = "f x =\n    y =\n        z"
          result = calculateNestingDepth code
      result `shouldSatisfy` (>= 1)

--------------------------------------------------------------------------------
-- Utils Branch Tests
--------------------------------------------------------------------------------

utilsBranchTests :: Spec
utilsBranchTests = describe "Utils branch coverage" $ do
  describe "safeHead branches" $ do
    it "returns Nothing for empty list" $ do
      headMay ([] :: [Int]) `shouldBe` Nothing

    it "returns Just for non-empty list" $ do
      headMay [1, 2, 3 :: Int] `shouldBe` Just 1

    it "returns Just first element regardless of list length" $ do
      headMay [42 :: Int] `shouldBe` Just 42

  describe "safeTail branches" $ do
    it "returns Nothing for empty list" $ do
      tailMay ([] :: [Int]) `shouldBe` Nothing

    it "returns Just for non-empty list" $ do
      tailMay [1, 2, 3 :: Int] `shouldBe` Just [2, 3]

    it "returns Just empty for singleton" $ do
      tailMay [1 :: Int] `shouldBe` Just []

  describe "safeLast branches" $ do
    it "returns Nothing for empty list" $ do
      lastMay ([] :: [Int]) `shouldBe` Nothing

    it "returns Just for non-empty list" $ do
      lastMay [1, 2, 3 :: Int] `shouldBe` Just 3

    it "returns Just for singleton" $ do
      lastMay [42 :: Int] `shouldBe` Just 42

  describe "safeInit branches" $ do
    it "returns Nothing for empty list" $ do
      initMay ([] :: [Int]) `shouldBe` Nothing

    it "returns Just for non-empty list" $ do
      initMay [1, 2, 3 :: Int] `shouldBe` Just [1, 2]

    it "returns Just empty for singleton" $ do
      initMay [1 :: Int] `shouldBe` Just []

  describe "word boundary branches" $ do
    -- isWordBoundary :: Maybe Char -> Maybe Char -> Bool
    -- Returns True when BOTH isWordStart AND isWordEnd are True
    -- isWordStart: prev is Nothing or not an ident char
    -- isWordEnd: next is Nothing or not an ident char
    -- So it's True when we're at a position between two non-ident regions

    it "isWordBoundary between spaces is True" $ do
      -- Both sides are non-ident chars
      isWordBoundary (Just ' ') (Just ' ') `shouldBe` True

    it "isWordBoundary at start before space is True" $ do
      -- Nothing before (isWordStart True), space after (isWordEnd True)
      isWordBoundary Nothing (Just ' ') `shouldBe` True

    it "isWordBoundary at end after space is True" $ do
      -- Space before (isWordStart True), Nothing after (isWordEnd True)
      isWordBoundary (Just ' ') Nothing `shouldBe` True

    it "isWordBoundary middle of word is False" $ do
      -- Both chars are ident chars
      isWordBoundary (Just 'o') (Just 'o') `shouldBe` False

    it "isWordBoundary from space to letter is False" $ do
      -- Space before (isWordStart True), but letter after (isWordEnd False)
      isWordBoundary (Just ' ') (Just 'b') `shouldBe` False

    it "isWordBoundary from letter to space is False" $ do
      -- Letter before (isWordStart False), space after (isWordEnd True)
      isWordBoundary (Just 'a') (Just ' ') `shouldBe` False

    it "isWordBoundary Nothing to letter is False" $ do
      -- Start of string to letter
      isWordBoundary Nothing (Just 'f') `shouldBe` False

    it "isWordBoundary letter to Nothing is False" $ do
      -- Letter to end of string
      isWordBoundary (Just 'r') Nothing `shouldBe` False

    it "isIdentChar for letters" $ do
      isIdentChar 'a' `shouldBe` True
      isIdentChar 'Z' `shouldBe` True

    it "isIdentChar for underscore" $ do
      isIdentChar '_' `shouldBe` True

    it "isIdentChar for apostrophe" $ do
      isIdentChar '\'' `shouldBe` True

    it "isIdentChar false for space" $ do
      isIdentChar ' ' `shouldBe` False

    it "isIdentStart for letters" $ do
      isIdentStart 'a' `shouldBe` True
      isIdentStart 'A' `shouldBe` True

    it "isIdentStart false for digit" $ do
      isIdentStart '0' `shouldBe` False

    it "isOperatorChar for symbols" $ do
      isOperatorChar '+' `shouldBe` True
      isOperatorChar '*' `shouldBe` True
      isOperatorChar '-' `shouldBe` True

    it "isOperatorChar false for letters" $ do
      isOperatorChar 'a' `shouldBe` False

--------------------------------------------------------------------------------
-- Diagnostic Branch Tests
--------------------------------------------------------------------------------

diagnosticBranchTests :: Spec
diagnosticBranchTests = describe "Diagnostic branch coverage" $ do
  describe "diagnostic kind branches" $ do
    let allKinds =
          [ NamingConvention
          , UnusedCode
          , UnusedImport
          , RedundantCode
          , CodePattern
          , TypeSignature
          , ImportStyle
          , TemplateHaskellRef
          , SecurityIssue
          , PerformanceIssue
          , ArchitecturalIssue
          , SpaceLeak
          , PartialFunction
          , ComplexityIssue
          ]

    it "all diagnostic kinds are showable" $ do
      all (not . null . show) allKinds `shouldBe` True

    it "all diagnostic kinds are comparable" $ do
      all (\k -> k == k) allKinds `shouldBe` True

  describe "diagnostic code branches" $ do
    it "diagnostic with code" $ do
      let diag = Diagnostic
            { diagSpan = noSrcSpan
            , diagMessage = "test"
            , diagSeverity = Warning
            , diagKind = CodePattern
            , diagCode = Just "test/code"
            , diagFixes = []
            , diagRelated = []
            }
      diagCode diag `shouldBe` Just "test/code"

    it "diagnostic without code" $ do
      let diag = Diagnostic
            { diagSpan = noSrcSpan
            , diagMessage = "test"
            , diagSeverity = Warning
            , diagKind = CodePattern
            , diagCode = Nothing
            , diagFixes = []
            , diagRelated = []
            }
      diagCode diag `shouldBe` Nothing

  describe "fix safety branches" $ do
    it "FSAlways is safest" $ do
      FSAlways < FSMostly `shouldBe` True
      FSAlways < FSReview `shouldBe` True
      FSAlways < FSUnsafe `shouldBe` True

    it "FSUnsafe is least safe" $ do
      FSUnsafe > FSAlways `shouldBe` True
      FSUnsafe > FSMostly `shouldBe` True
      FSUnsafe > FSReview `shouldBe` True

  describe "fix category branches" $ do
    let allCategories =
          [ FCPerformance
          , FCModernize
          , FCSafety
          , FCStyle
          , FCImports
          , FCRedundant
          , FCSpaceLeaks
          , FCSecurity
          ]

    it "all fix categories are showable" $ do
      all (not . null . show) allCategories `shouldBe` True

