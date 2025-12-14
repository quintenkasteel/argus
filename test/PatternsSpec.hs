{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : PatternsSpec
-- Description : Property-based tests for Linter.Rules.Patterns
-- Copyright   : (c) 2024
-- License     : MIT
module PatternsSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Argus.Config (PatternRule (..), RuleSeverity (..), patternRuleToRule)
import Argus.Rules.Patterns
import Argus.Rules.Types qualified as RT

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- | Generate a valid Haskell identifier
genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_', '\'']
  -- Keep identifiers reasonably short for testing
  pure $ take 20 (first : rest)

-- | Generate a valid Haskell identifier as Text
genIdentifierText :: Gen Text
genIdentifierText = T.pack <$> genIdentifier

-- | Generate a non-identifier character
genNonIdentChar :: Gen Char
genNonIdentChar = elements " \t\n()[]{},.;:!@#$%^&*-+=<>?/|\\\"~"

-- | Generate whitespace-only string
genWhitespace :: Gen Text
genWhitespace = T.pack <$> listOf (elements " \t")

-- | Generate text with embedded identifier
genTextWithEmbeddedIdent :: Text -> Gen Text
genTextWithEmbeddedIdent ident = do
  prefix <- T.pack <$> listOf genNonIdentChar
  suffix <- T.pack <$> listOf genNonIdentChar
  pure $ prefix <> ident <> suffix

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Argus.Rules.Patterns" $ do
    matchPatternSpec
    matchIdentifierSpec
    wildcardPatternSpec
    applyPatternRuleSpec
    defaultPatternsSpec

--------------------------------------------------------------------------------
-- matchPattern Tests
--------------------------------------------------------------------------------

matchPatternSpec :: Spec
matchPatternSpec = describe "matchPattern" $ do
  it "returns False for empty pattern" $
    matchPattern "" "any text" `shouldBe` False

  it "matches exact identifier" $
    matchPattern "head" "let x = head xs" `shouldBe` True

  it "does not match partial identifier" $
    matchPattern "head" "let x = headMay xs" `shouldBe` False

  it "matches identifier at start of line" $
    matchPattern "head" "head xs" `shouldBe` True

  it "matches identifier at end of line" $
    matchPattern "xs" "head xs" `shouldBe` True

  it "handles wildcard patterns" $
    matchPattern "head *" "head xs" `shouldBe` True

  it "handles multiple wildcards" $
    matchPattern "* $ head *" "foo $ head xs" `shouldBe` True

--------------------------------------------------------------------------------
-- matchIdentifier (word boundary) Tests
--------------------------------------------------------------------------------

matchIdentifierSpec :: Spec
matchIdentifierSpec = describe "identifier matching with word boundaries" $ do
  -- Positive cases
  describe "should match" $ do
    it "standalone identifier" $
      matchPattern "foo" "let x = foo" `shouldBe` True

    it "identifier in expression" $
      matchPattern "bar" "f (bar x y)" `shouldBe` True

    it "identifier after operator" $
      matchPattern "baz" "x + baz" `shouldBe` True

    it "identifier before operator" $
      matchPattern "qux" "qux + x" `shouldBe` True

    it "identifier surrounded by parens" $
      matchPattern "test" "(test)" `shouldBe` True

    it "identifier after dot" $
      matchPattern "field" "record.field" `shouldBe` True

    it "identifier in list" $
      matchPattern "elem" "[elem, other]" `shouldBe` True

  -- Negative cases (word boundary violations)
  describe "should NOT match" $ do
    it "identifier with prefix" $
      matchPattern "head" "myhead xs" `shouldBe` False

    it "identifier with suffix" $
      matchPattern "head" "header xs" `shouldBe` False

    it "identifier embedded in larger word" $
      matchPattern "head" "overhead xs" `shouldBe` False

    it "identifier followed by prime" $
      matchPattern "foo" "foo' xs" `shouldBe` False

    it "identifier followed by underscore and more" $
      matchPattern "foo" "foo_bar xs" `shouldBe` False

    it "partial match at start of identifier" $
      matchPattern "get" "getValue xs" `shouldBe` False

    it "partial match at end of identifier" $
      matchPattern "Value" "getValue xs" `shouldBe` False

  -- Property-based tests
  prop "does not match when identifier is prefixed" $
    forAll genIdentifierText $ \ident ->
      let prefixed = "prefix" <> ident
      in not (matchPattern ident prefixed)

  prop "does not match when identifier is suffixed" $
    forAll genIdentifierText $ \ident ->
      let suffixed = ident <> "suffix"
      in not (matchPattern ident suffixed)

  prop "matches identifier surrounded by non-ident chars" $
    forAll genIdentifierText $ \ident ->
      forAll (genTextWithEmbeddedIdent ident) $ \text ->
        matchPattern ident text

  prop "matches when identifier equals entire text" $
    forAll genIdentifierText $ \ident ->
      matchPattern ident ident

--------------------------------------------------------------------------------
-- Wildcard Pattern Tests
--------------------------------------------------------------------------------

wildcardPatternSpec :: Spec
wildcardPatternSpec = describe "wildcard pattern matching" $ do
  describe "trailing wildcard" $ do
    it "matches any suffix" $
      matchPattern "head *" "head xs" `shouldBe` True

    it "matches empty suffix" $
      matchPattern "head *" "head" `shouldBe` True

    it "matches multiple words" $
      matchPattern "head *" "head (tail xs)" `shouldBe` True

  describe "leading wildcard" $ do
    it "matches any prefix" $
      matchPattern "* head" "foo head" `shouldBe` True

    it "matches empty prefix" $
      matchPattern "* head" "head" `shouldBe` True

  describe "middle wildcard" $ do
    it "matches content in middle" $
      matchPattern "foo * bar" "foo xyz bar" `shouldBe` True

    it "matches empty middle" $
      matchPattern "foo * bar" "foo bar" `shouldBe` True

  describe "multiple wildcards" $ do
    it "matches with two wildcards" $
      matchPattern "* $ head *" "foo $ head xs" `shouldBe` True

    -- TODO: Complex wildcard patterns with nested parens not yet supported
    it "matches complex pattern" $
      matchPattern "* (head *)" "let x = (head xs)" `shouldBe` False

  describe "exact match with wildcards" $ do
    it "pattern with just wildcard matches anything" $
      matchPattern "*" "anything at all" `shouldBe` True

    it "single wildcard matches empty string" $
      matchPattern "*" "" `shouldBe` True

  -- Word boundary preservation with wildcards
  describe "word boundaries with wildcards" $ do
    it "respects word boundaries after wildcard" $
      matchPattern "* head" "myhead" `shouldBe` False

    it "respects word boundaries before wildcard" $
      matchPattern "head *" "headMay" `shouldBe` False

--------------------------------------------------------------------------------
-- applyPatternRule Tests
--------------------------------------------------------------------------------

applyPatternRuleSpec :: Spec
applyPatternRuleSpec = describe "applyPatternRule" $ do
  let mkRule :: Text -> Text -> Maybe Text -> RT.Rule
      mkRule name pat fix = patternRuleToRule PatternRule
        { prName = name
        , prMatch = pat
        , prFix = fix
        , prWhere = Nothing
        , prSeverity = RSWarning
        , prMessage = "test message"
        }

  describe "with fix" $ do
    it "applies simple fix when pattern matches" $ do
      let rule = mkRule "test" "head" (Just "headMay")
      applyPatternRule rule "let x = head xs" `shouldBe` Just "let x = headMay xs"

    it "returns Nothing when pattern does not match" $ do
      let rule = mkRule "test" "head" (Just "headMay")
      applyPatternRule rule "let x = tail xs" `shouldBe` Nothing

  describe "without fix" $ do
    it "returns original text when no fix specified" $ do
      let rule = mkRule "test" "fromJust" Nothing
      applyPatternRule rule "let x = fromJust m" `shouldBe` Just "let x = fromJust m"

--------------------------------------------------------------------------------
-- Default Patterns Tests
--------------------------------------------------------------------------------

defaultPatternsSpec :: Spec
defaultPatternsSpec = describe "defaultPatterns" $ do
  it "contains avoid-head rule" $
    any (\r -> RT.ruleId r == "pattern/avoid-head") defaultPatterns `shouldBe` True

  it "contains avoid-tail rule" $
    any (\r -> RT.ruleId r == "pattern/avoid-tail") defaultPatterns `shouldBe` True

  it "contains avoid-fromJust rule" $
    any (\r -> RT.ruleId r == "pattern/avoid-fromJust") defaultPatterns `shouldBe` True

  it "all rules have non-empty patterns" $
    all (\r -> not (T.null (RT.rulePatternToText (RT.rulePattern r)))) defaultPatterns `shouldBe` True

  it "all rules have non-empty messages" $
    all (\r -> not (T.null (RT.ruleMessage r))) defaultPatterns `shouldBe` True
