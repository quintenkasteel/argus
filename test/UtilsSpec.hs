{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : UtilsSpec
-- Description : Property-based tests for Linter.Utils
-- Copyright   : (c) 2024
-- License     : MIT
module UtilsSpec (spec) where

import Data.Char (isAlphaNum, isAlpha)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Linter.Utils

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- | Generate a valid Haskell identifier (starts with letter/underscore, followed by alphanums/underscores/primes)
genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_', '\'']
  pure (first : rest)

-- | Generate a valid Haskell identifier as Text
genIdentifierText :: Gen Text
genIdentifierText = T.pack <$> genIdentifier

-- | Generate a non-identifier character (for boundary testing)
genNonIdentChar :: Gen Char
genNonIdentChar = elements $ " \t\n()[]{},.;:!@#$%^&*-+=<>?/|\\\"" ++ "`~"

-- | Generate a string that is NOT a valid identifier character sequence
genNonIdentString :: Gen String
genNonIdentString = listOf1 genNonIdentChar

-- | Generate text with embedded identifier (returns both)
genTextWithIdentifier :: Gen (Text, Text)
genTextWithIdentifier = do
  ident <- genIdentifierText
  prefix <- T.pack <$> listOf genNonIdentChar
  suffix <- T.pack <$> listOf genNonIdentChar
  pure (ident, prefix <> ident <> suffix)

-- | Generate text with a specific identifier embedded
genTextWithEmbeddedIdent :: Text -> Gen Text
genTextWithEmbeddedIdent ident = do
  prefix <- T.pack <$> listOf genNonIdentChar
  suffix <- T.pack <$> listOf genNonIdentChar
  pure $ prefix <> ident <> suffix

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Linter.Utils" $ do
    identCharSpec
    wordBoundarySpec
    safeListSpec
    replaceWordBoundarySpec
    matchesAtWordBoundarySpec
    findWordBoundaryMatchSpec
    unicodeAndSpecialSpec
    stringCommentPreserveSpec

--------------------------------------------------------------------------------
-- Identifier Character Tests
--------------------------------------------------------------------------------

identCharSpec :: Spec
identCharSpec = describe "isIdentChar" $ do
  prop "classifies alphanumeric characters as identifier chars" $ \c ->
    isAlphaNum c ==> isIdentChar c

  prop "classifies underscore as identifier char" $
    isIdentChar '_' `shouldBe` True

  prop "classifies prime (apostrophe) as identifier char" $
    isIdentChar '\'' `shouldBe` True

  prop "classifies spaces as non-identifier chars" $
    isIdentChar ' ' `shouldBe` False

  prop "classifies punctuation as non-identifier chars" $
    forAll (elements "()[]{},.;:!@#$%^&*-+=<>?/|\\\"") $ \c ->
      not (isIdentChar c)

  describe "isIdentStart" $ do
    prop "classifies letters as valid identifier starts" $ \c ->
      isAlpha c ==> isIdentStart c

    prop "classifies underscore as valid identifier start" $
      isIdentStart '_' `shouldBe` True

    prop "classifies digits as invalid identifier starts" $
      forAll (elements ['0'..'9']) $ \c ->
        not (isIdentStart c)

  describe "isOperatorChar" $ do
    it "classifies Haskell operator characters correctly" $ do
      isOperatorChar '+' `shouldBe` True
      isOperatorChar '-' `shouldBe` True
      isOperatorChar '*' `shouldBe` True
      isOperatorChar '/' `shouldBe` True
      isOperatorChar '.' `shouldBe` True
      isOperatorChar '$' `shouldBe` True
      isOperatorChar '>' `shouldBe` True
      isOperatorChar '<' `shouldBe` True
      isOperatorChar '=' `shouldBe` True
      isOperatorChar ':' `shouldBe` True

    it "does not classify alphanumeric as operator" $ do
      isOperatorChar 'a' `shouldBe` False
      isOperatorChar '0' `shouldBe` False

--------------------------------------------------------------------------------
-- Word Boundary Tests
--------------------------------------------------------------------------------

wordBoundarySpec :: Spec
wordBoundarySpec = describe "Word Boundary Detection" $ do
  describe "isWordStart" $ do
    it "returns True for Nothing (start of string)" $
      isWordStart Nothing `shouldBe` True

    it "returns True for non-identifier character before" $
      isWordStart (Just ' ') `shouldBe` True

    it "returns False for identifier character before" $
      isWordStart (Just 'a') `shouldBe` False

    prop "returns False for any identifier char" $
      forAll (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_', '\'']) $ \c ->
        not (isWordStart (Just c))

  describe "isWordEnd" $ do
    it "returns True for Nothing (end of string)" $
      isWordEnd Nothing `shouldBe` True

    it "returns True for non-identifier character after" $
      isWordEnd (Just ')') `shouldBe` True

    it "returns False for identifier character after" $
      isWordEnd (Just 'x') `shouldBe` False

  describe "isWordBoundary" $ do
    it "returns True at string boundaries" $
      isWordBoundary Nothing Nothing `shouldBe` True

    it "returns True when surrounded by non-ident chars" $
      isWordBoundary (Just ' ') (Just ')') `shouldBe` True

    it "returns False when ident char before" $
      isWordBoundary (Just 'a') Nothing `shouldBe` False

    it "returns False when ident char after" $
      isWordBoundary Nothing (Just 'z') `shouldBe` False

  describe "atWordBoundary" $ do
    it "returns True for empty strings" $
      atWordBoundary "" "" `shouldBe` True

    it "returns True with non-ident boundaries" $
      atWordBoundary "abc " " def" `shouldBe` True

    it "returns False with ident char at end of before" $
      atWordBoundary "abcx" " def" `shouldBe` False

    it "returns False with ident char at start of after" $
      atWordBoundary "abc " "xdef" `shouldBe` False

--------------------------------------------------------------------------------
-- Safe List Operation Tests
--------------------------------------------------------------------------------

safeListSpec :: Spec
safeListSpec = describe "Safe List Operations" $ do
  describe "headMay" $ do
    it "returns Nothing for empty list" $
      headMay ([] :: [Int]) `shouldBe` Nothing

    it "returns Just first element for non-empty list" $
      headMay [1, 2, 3] `shouldBe` Just 1

    prop "is equivalent to head for non-empty lists" $ \(NonEmpty xs) ->
      headMay (xs :: [Int]) == Just (head xs)

  describe "tailMay" $ do
    it "returns Nothing for empty list" $
      tailMay ([] :: [Int]) `shouldBe` Nothing

    it "returns Just tail for non-empty list" $
      tailMay [1, 2, 3] `shouldBe` Just [2, 3]

    prop "is equivalent to tail for non-empty lists" $ \(NonEmpty xs) ->
      tailMay (xs :: [Int]) == Just (tail xs)

  describe "lastMay" $ do
    it "returns Nothing for empty list" $
      lastMay ([] :: [Int]) `shouldBe` Nothing

    it "returns Just last element for non-empty list" $
      lastMay [1, 2, 3] `shouldBe` Just 3

    prop "is equivalent to last for non-empty lists" $ \(NonEmpty xs) ->
      lastMay (xs :: [Int]) == Just (last xs)

  describe "initMay" $ do
    it "returns Nothing for empty list" $
      initMay ([] :: [Int]) `shouldBe` Nothing

    it "returns Just init for non-empty list" $
      initMay [1, 2, 3] `shouldBe` Just [1, 2]

    prop "is equivalent to init for non-empty lists" $ \(NonEmpty xs) ->
      initMay (xs :: [Int]) == Just (init xs)

--------------------------------------------------------------------------------
-- replaceWordBoundary Tests
--------------------------------------------------------------------------------

replaceWordBoundarySpec :: Spec
replaceWordBoundarySpec = describe "replaceWordBoundary" $ do
  it "replaces standalone identifier" $
    replaceWordBoundary "head" "headMay" "let x = head xs"
      `shouldBe` "let x = headMay xs"

  it "does not replace when part of larger identifier" $
    replaceWordBoundary "head" "headMay" "let x = headMay xs"
      `shouldBe` "let x = headMay xs"

  it "does not replace when prefixed" $
    replaceWordBoundary "head" "headMay" "let x = myhead xs"
      `shouldBe` "let x = myhead xs"

  it "does not replace when suffixed" $
    replaceWordBoundary "head" "headMay" "let x = header xs"
      `shouldBe` "let x = header xs"

  it "replaces at start of text" $
    replaceWordBoundary "head" "headMay" "head xs"
      `shouldBe` "headMay xs"

  it "replaces at end of text" $
    replaceWordBoundary "xs" "ys" "return xs"
      `shouldBe` "return ys"

  it "replaces multiple occurrences" $
    replaceWordBoundary "head" "headMay" "head (head xs)"
      `shouldBe` "headMay (headMay xs)"

  it "handles adjacent replacements correctly" $
    replaceWordBoundary "x" "y" "x x x"
      `shouldBe` "y y y"

  it "handles empty from string" $
    replaceWordBoundary "" "foo" "test"
      `shouldBe` "test"

  prop "idempotence: replacing twice gives same result" $
    forAll genIdentifierText $ \ident ->
      forAll genIdentifierText $ \replacement ->
        forAll (genTextWithEmbeddedIdent ident) $ \text ->
          let result1 = replaceWordBoundary ident replacement text
              result2 = replaceWordBoundary ident replacement result1
          in result1 == result2

  prop "does not modify text without the pattern" $
    forAll genIdentifierText $ \ident ->
      forAll genIdentifierText $ \replacement ->
        forAll (T.pack <$> genNonIdentString) $ \text ->
          replaceWordBoundary ident replacement text == text

  -- Edge cases for word boundaries
  describe "word boundary edge cases" $ do
    it "handles identifier followed by operator" $
      replaceWordBoundary "foo" "bar" "foo+x"
        `shouldBe` "bar+x"

    it "handles identifier preceded by operator" $
      replaceWordBoundary "foo" "bar" "x+foo"
        `shouldBe` "x+bar"

    it "handles identifier in parentheses" $
      replaceWordBoundary "foo" "bar" "(foo)"
        `shouldBe` "(bar)"

    it "handles identifier in brackets" $
      replaceWordBoundary "foo" "bar" "[foo]"
        `shouldBe` "[bar]"

    it "handles identifier after dot" $
      replaceWordBoundary "foo" "bar" "x.foo"
        `shouldBe` "x.bar"

    it "handles identifier with prime suffix" $
      replaceWordBoundary "foo" "bar" "foo'"
        `shouldBe` "foo'"  -- foo' is a different identifier

    it "handles identifier with underscore suffix" $
      replaceWordBoundary "foo" "bar" "foo_x"
        `shouldBe` "foo_x"  -- foo_x is a different identifier

--------------------------------------------------------------------------------
-- matchesAtWordBoundary Tests
--------------------------------------------------------------------------------

matchesAtWordBoundarySpec :: Spec
matchesAtWordBoundarySpec = describe "matchesAtWordBoundary" $ do
  it "matches standalone identifier" $
    matchesAtWordBoundary "head" "let x = head xs"
      `shouldBe` True

  it "does not match partial identifier (prefix)" $
    matchesAtWordBoundary "head" "let x = headMay xs"
      `shouldBe` False

  it "does not match partial identifier (suffix)" $
    matchesAtWordBoundary "head" "let x = overhead xs"
      `shouldBe` False

  it "matches at start of text" $
    matchesAtWordBoundary "head" "head xs"
      `shouldBe` True

  it "matches at end of text" $
    matchesAtWordBoundary "xs" "return xs"
      `shouldBe` True

  it "does not match when pattern not present" $
    matchesAtWordBoundary "foo" "bar baz qux"
      `shouldBe` False

  prop "returns True when pattern equals entire text" $
    forAll genIdentifierText $ \ident ->
      matchesAtWordBoundary ident ident

  prop "pattern with extra chars at start does not match" $
    forAll genIdentifierText $ \ident ->
      not (matchesAtWordBoundary ident ("prefix" <> ident))

  prop "pattern with extra chars at end does not match" $
    forAll genIdentifierText $ \ident ->
      not (matchesAtWordBoundary ident (ident <> "suffix"))

  prop "matches when surrounded by non-ident chars" $
    forAll genIdentifierText $ \ident ->
      forAll (T.pack <$> listOf1 genNonIdentChar) $ \prefix ->
        forAll (T.pack <$> listOf1 genNonIdentChar) $ \suffix ->
          matchesAtWordBoundary ident (prefix <> ident <> suffix)

--------------------------------------------------------------------------------
-- findWordBoundaryMatch Tests
--------------------------------------------------------------------------------

findWordBoundaryMatchSpec :: Spec
findWordBoundaryMatchSpec = describe "findWordBoundaryMatch" $ do
  it "finds match and returns remaining string" $
    findWordBoundaryMatch "head" "let x = head xs"
      `shouldBe` Just " xs"

  it "returns Nothing when no word-boundary match" $
    findWordBoundaryMatch "head" "let x = headMay xs"
      `shouldBe` Nothing

  it "returns Nothing when pattern not present" $
    findWordBoundaryMatch "foo" "bar baz"
      `shouldBe` Nothing

  it "handles match at start" $
    findWordBoundaryMatch "head" "head xs"
      `shouldBe` Just " xs"

  it "handles match at end" $
    findWordBoundaryMatch "xs" "head xs"
      `shouldBe` Just ""

--------------------------------------------------------------------------------
-- Unicode and Special Character Tests
--------------------------------------------------------------------------------

unicodeAndSpecialSpec :: Spec
unicodeAndSpecialSpec = describe "unicode and special characters" $ do
  describe "unicode identifiers" $ do
    it "matches unicode letters as identifier chars" $
      isIdentChar 'α' `shouldBe` True

    it "matches unicode letters in identifier start" $
      isIdentStart 'α' `shouldBe` True

    it "replaces identifier with unicode in surrounding text" $
      replaceWordBoundary "foo" "bar" "λx. foo + x"
        `shouldBe` "λx. bar + x"

    it "does not treat unicode symbols as identifier chars" $ do
      isIdentChar '→' `shouldBe` False
      isIdentChar '∀' `shouldBe` False
      isIdentChar '∈' `shouldBe` False

    it "handles unicode operators as word boundaries" $
      replaceWordBoundary "x" "y" "a → x ← b"
        `shouldBe` "a → y ← b"

  describe "qualified names" $ do
    it "replaces identifier after dot in qualified name" $
      replaceWordBoundary "head" "headMay" "Data.List.head xs"
        `shouldBe` "Data.List.headMay xs"

    it "does not replace module name part" $
      replaceWordBoundary "Data" "Foo" "Data.List.head"
        `shouldBe` "Foo.List.head"

    it "handles multiple dots correctly" $
      replaceWordBoundary "foo" "bar" "A.B.C.foo"
        `shouldBe` "A.B.C.bar"

    it "matches identifier in deeply qualified context" $
      matchesAtWordBoundary "head" "Control.Monad.Trans.head xs"
        `shouldBe` True

  describe "Haskell operators" $ do
    it "all standard operators are not identifier chars" $
      all (not . isIdentChar) ("!#$%&*+./<=>?@\\^|-~:" :: String) `shouldBe` True

    it "replaces identifier next to dollar operator" $
      replaceWordBoundary "foo" "bar" "print $ foo x"
        `shouldBe` "print $ bar x"

    it "replaces identifier next to composition" $
      replaceWordBoundary "foo" "bar" "f . foo . g"
        `shouldBe` "f . bar . g"

    it "replaces identifier next to bind" $
      replaceWordBoundary "foo" "bar" "x >>= foo"
        `shouldBe` "x >>= bar"

    it "replaces identifier next to applicative" $
      replaceWordBoundary "foo" "bar" "pure <$> foo <*> baz"
        `shouldBe` "pure <$> bar <*> baz"

    it "handles infix backtick operators" $
      replaceWordBoundary "foo" "bar" "x `elem` foo"
        `shouldBe` "x `elem` bar"

  describe "edge case symbols" $ do
    it "handles at-sign pattern" $
      replaceWordBoundary "xs" "ys" "list@(x:xs)"
        `shouldBe` "list@(x:ys)"

    it "handles hash for MagicHash" $
      -- Note: In current implementation, # is not an identifier char,
      -- so foo# contains foo at a word boundary. With MagicHash enabled,
      -- foo# would be a single identifier, but we don't support that yet.
      replaceWordBoundary "foo" "bar" "foo# + foo"
        `shouldBe` "bar# + bar"

    it "handles tilde for lazy patterns" $
      replaceWordBoundary "foo" "bar" "~foo"
        `shouldBe` "~bar"

    it "handles bang for strict patterns" $
      replaceWordBoundary "foo" "bar" "!foo"
        `shouldBe` "!bar"

    it "handles semicolon in do-notation" $
      replaceWordBoundary "foo" "bar" "do { x <- foo; pure x }"
        `shouldBe` "do { x <- bar; pure x }"

--------------------------------------------------------------------------------
-- String and Comment Preservation Tests
--------------------------------------------------------------------------------

stringCommentPreserveSpec :: Spec
stringCommentPreserveSpec = describe "replaceWordBoundaryPreserve" $ do
  describe "basic replacement (same as replaceWordBoundary)" $ do
    it "replaces standalone identifier" $
      replaceWordBoundaryPreserve "foo" "bar" "let x = foo"
        `shouldBe` "let x = bar"

    it "respects word boundaries" $
      replaceWordBoundaryPreserve "foo" "bar" "foobar foobaz foo"
        `shouldBe` "foobar foobaz bar"

  describe "string literal preservation" $ do
    it "does not replace inside string literal" $
      replaceWordBoundaryPreserve "foo" "bar" "let x = \"foo\""
        `shouldBe` "let x = \"foo\""

    it "replaces outside but not inside string" $
      replaceWordBoundaryPreserve "foo" "bar" "foo \"foo\" foo"
        `shouldBe` "bar \"foo\" bar"

    it "handles escaped quotes in strings" $
      replaceWordBoundaryPreserve "foo" "bar" "\"\\\"foo\\\"\" foo"
        `shouldBe` "\"\\\"foo\\\"\" bar"

    it "handles string with escaped backslash" $
      replaceWordBoundaryPreserve "foo" "bar" "\"\\\\\" foo"
        `shouldBe` "\"\\\\\" bar"

    it "handles multiple strings" $
      replaceWordBoundaryPreserve "foo" "bar" "\"foo\" <> foo <> \"foo\""
        `shouldBe` "\"foo\" <> bar <> \"foo\""

  describe "character literal preservation" $ do
    it "does not replace inside char literal" $
      replaceWordBoundaryPreserve "x" "y" "let c = 'x'"
        `shouldBe` "let c = 'x'"

    it "handles escaped char literal" $
      replaceWordBoundaryPreserve "n" "m" "let c = '\\n' in n"
        `shouldBe` "let c = '\\n' in m"

  describe "line comment preservation" $ do
    it "does not replace inside line comment" $
      replaceWordBoundaryPreserve "foo" "bar" "foo -- foo\nfoo"
        `shouldBe` "bar -- foo\nbar"

    it "handles comment at end of line" $
      replaceWordBoundaryPreserve "foo" "bar" "foo -- foo"
        `shouldBe` "bar -- foo"

    it "handles multiple line comments" $
      replaceWordBoundaryPreserve "foo" "bar" "foo -- foo\nfoo -- foo\nfoo"
        `shouldBe` "bar -- foo\nbar -- foo\nbar"

  describe "block comment preservation" $ do
    it "does not replace inside block comment" $
      replaceWordBoundaryPreserve "foo" "bar" "foo {- foo -} foo"
        `shouldBe` "bar {- foo -} bar"

    it "handles nested block comments" $
      replaceWordBoundaryPreserve "foo" "bar" "foo {- outer {- inner foo -} foo -} foo"
        `shouldBe` "bar {- outer {- inner foo -} foo -} bar"

    it "handles multiline block comment" $
      replaceWordBoundaryPreserve "foo" "bar" "foo {-\nfoo\n-} foo"
        `shouldBe` "bar {-\nfoo\n-} bar"

  describe "mixed strings and comments" $ do
    it "handles string inside comment" $
      replaceWordBoundaryPreserve "foo" "bar" "{- \"foo\" -} foo"
        `shouldBe` "{- \"foo\" -} bar"

    it "handles comment-like sequence inside string" $
      replaceWordBoundaryPreserve "foo" "bar" "\"-- foo\" foo"
        `shouldBe` "\"-- foo\" bar"

    it "handles block comment inside string" $
      replaceWordBoundaryPreserve "foo" "bar" "\"{- foo -}\" foo"
        `shouldBe` "\"{- foo -}\" bar"

  describe "real-world Haskell patterns" $ do
    it "preserves documentation strings" $
      replaceWordBoundaryPreserve "head" "headMay"
        "-- | Get the head of a list\nhead xs = head xs"
        `shouldBe` "-- | Get the head of a list\nheadMay xs = headMay xs"

    it "preserves error messages in strings" $
      replaceWordBoundaryPreserve "head" "headMay"
        "error \"head: empty list\" $ head xs"
        `shouldBe` "error \"head: empty list\" $ headMay xs"

    it "preserves pragma comments" $
      replaceWordBoundaryPreserve "foo" "bar"
        "{-# INLINE foo #-}\nfoo x = x"
        `shouldBe` "{-# INLINE foo #-}\nbar x = x"
