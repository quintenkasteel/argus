{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : SubstitutionSpec
-- Description : Property-based tests for Linter.Refactor.Substitution
-- Copyright   : (c) 2024
-- License     : MIT
module SubstitutionSpec (spec) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Linter.Refactor.Substitution

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- | Generate a valid Haskell identifier
genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_', '\'']
  pure $ take 15 (first : rest)

-- | Generate a valid Haskell identifier as Text
genIdentifierText :: Gen Text
genIdentifierText = T.pack <$> genIdentifier

-- | Generate a non-identifier character
genNonIdentChar :: Gen Char
genNonIdentChar = elements " \t\n()[]{},.;:!@#$%^&*-+=<>?/|\\\"~"

-- | Generate text with embedded identifier at word boundary
genTextWithIdent :: Text -> Gen Text
genTextWithIdent ident = do
  prefix <- T.pack <$> listOf genNonIdentChar
  suffix <- T.pack <$> listOf genNonIdentChar
  pure $ prefix <> ident <> suffix

-- | Generate a substitution map with unique keys
genSubstitutionMap :: Gen (Map Text Text)
genSubstitutionMap = do
  pairs <- listOf $ (,) <$> genIdentifierText <*> genIdentifierText
  -- Ensure keys are unique and different from values
  pure $ Map.fromList [(k, v) | (k, v) <- pairs, k /= v]

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Linter.Refactor.Substitution" $ do
    substituteExprSpec
    edgeCasesSpec

--------------------------------------------------------------------------------
-- substituteExpr Tests
--------------------------------------------------------------------------------

substituteExprSpec :: Spec
substituteExprSpec = describe "substituteExpr" $ do
  describe "single substitution" $ do
    it "replaces standalone identifier" $ do
      let subs = Map.singleton "foo" "bar"
      substituteExpr subs "let x = foo" `shouldBe` "let x = bar"

    it "does not replace prefixed identifier" $ do
      let subs = Map.singleton "foo" "bar"
      substituteExpr subs "let x = myfoo" `shouldBe` "let x = myfoo"

    it "does not replace suffixed identifier" $ do
      let subs = Map.singleton "foo" "bar"
      substituteExpr subs "let x = foobar" `shouldBe` "let x = foobar"

    it "replaces at start of text" $ do
      let subs = Map.singleton "foo" "bar"
      substituteExpr subs "foo x" `shouldBe` "bar x"

    it "replaces at end of text" $ do
      let subs = Map.singleton "foo" "bar"
      substituteExpr subs "x foo" `shouldBe` "x bar"

    it "replaces multiple occurrences" $ do
      let subs = Map.singleton "foo" "bar"
      substituteExpr subs "foo + foo * foo" `shouldBe` "bar + bar * bar"

  describe "multiple substitutions" $ do
    it "applies all substitutions" $ do
      let subs = Map.fromList [("foo", "bar"), ("baz", "qux")]
      substituteExpr subs "foo + baz" `shouldBe` "bar + qux"

    -- NOTE: Current implementation applies substitutions sequentially,
    -- which can cause chaining. With Map key ordering (bar < foo),
    -- foldr processes: foo->bar first, then bar->baz on result.
    it "substitutions may chain (current behavior)" $ do
      let subs = Map.fromList [("foo", "bar"), ("bar", "baz")]
      substituteExpr subs "foo" `shouldBe` "baz"  -- Chains: foo->bar->baz

    it "handles overlapping patterns correctly" $ do
      let subs = Map.fromList [("head", "headMay"), ("tail", "tailMay")]
      substituteExpr subs "head (tail xs)" `shouldBe` "headMay (tailMay xs)"

  describe "empty substitution map" $ do
    it "returns original text unchanged" $ do
      substituteExpr Map.empty "let x = foo bar" `shouldBe` "let x = foo bar"

    prop "preserves any text with empty map" $
      forAll (T.pack <$> listOf arbitrary) $ \text ->
        substituteExpr Map.empty text == text

  -- Property-based tests
  describe "properties" $ do
    prop "idempotence: applying twice equals applying once" $
      forAll genIdentifierText $ \from ->
        forAll genIdentifierText $ \to ->
          from /= to ==>
            forAll (genTextWithIdent from) $ \text ->
              let subs = Map.singleton from to
                  once = substituteExpr subs text
                  twice = substituteExpr subs once
              in once == twice

    prop "does not modify text without matching identifiers" $
      forAll genIdentifierText $ \from ->
        forAll genIdentifierText $ \to ->
          forAll (T.pack <$> listOf genNonIdentChar) $ \text ->
            substituteExpr (Map.singleton from to) text == text

--------------------------------------------------------------------------------
-- Edge Cases and Regression Tests
--------------------------------------------------------------------------------

edgeCasesSpec :: Spec
edgeCasesSpec = describe "edge cases" $ do
  describe "special characters around identifiers" $ do
    it "handles identifier after operator" $ do
      let subs = Map.singleton "foo" "bar"
      substituteExpr subs "x+foo" `shouldBe` "x+bar"

    it "handles identifier before operator" $ do
      let subs = Map.singleton "foo" "bar"
      substituteExpr subs "foo+x" `shouldBe` "bar+x"

    it "handles identifier in parentheses" $ do
      let subs = Map.singleton "foo" "bar"
      substituteExpr subs "(foo)" `shouldBe` "(bar)"

    it "handles identifier in brackets" $ do
      let subs = Map.singleton "foo" "bar"
      substituteExpr subs "[foo]" `shouldBe` "[bar]"

    it "handles identifier in function application" $ do
      let subs = Map.singleton "head" "headMay"
      substituteExpr subs "f (head xs)" `shouldBe` "f (headMay xs)"

  describe "identifier-like patterns that should NOT match" $ do
    it "does not match identifier followed by prime" $ do
      let subs = Map.singleton "foo" "bar"
      substituteExpr subs "foo'" `shouldBe` "foo'"

    it "does not match identifier with underscore suffix" $ do
      let subs = Map.singleton "foo" "bar"
      substituteExpr subs "foo_x" `shouldBe` "foo_x"

    it "does not match identifier with numeric suffix" $ do
      let subs = Map.singleton "foo" "bar"
      substituteExpr subs "foo2" `shouldBe` "foo2"

    it "does not match camelCase continuation" $ do
      let subs = Map.singleton "head" "headMay"
      substituteExpr subs "headMay xs" `shouldBe` "headMay xs"

  describe "Haskell-specific patterns" $ do
    it "replaces in qualified calls" $ do
      let subs = Map.singleton "head" "headMay"
      substituteExpr subs "Prelude.head xs" `shouldBe` "Prelude.headMay xs"

    it "replaces in do notation" $ do
      let subs = Map.singleton "foo" "bar"
      substituteExpr subs "do\n  x <- foo\n  pure x"
        `shouldBe` "do\n  x <- bar\n  pure x"

    it "replaces in lambda" $ do
      let subs = Map.singleton "f" "g"
      substituteExpr subs "\\x -> f x" `shouldBe` "\\x -> g x"

    it "replaces in case expression" $ do
      let subs = Map.singleton "foo" "bar"
      substituteExpr subs "case foo of\n  _ -> foo"
        `shouldBe` "case bar of\n  _ -> bar"

  describe "real-world refactoring scenarios" $ do
    it "replaces head with headMay safely" $ do
      let subs = Map.singleton "head" "headMay"
      substituteExpr subs "let first = head list in print first"
        `shouldBe` "let first = headMay list in print first"

    it "replaces tail with tailMay safely" $ do
      let subs = Map.singleton "tail" "tailMay"
      substituteExpr subs "rest = tail items"
        `shouldBe` "rest = tailMay items"

    it "replaces fromJust safely" $ do
      let subs = Map.singleton "fromJust" "fromMaybe defaultVal"
      substituteExpr subs "value = fromJust maybeVal"
        `shouldBe` "value = fromMaybe defaultVal maybeVal"

    it "multiple safe replacements" $ do
      let subs = Map.fromList
            [ ("head", "headMay")
            , ("tail", "tailMay")
            , ("last", "lastMay")
            ]
      substituteExpr subs "f xs = (head xs, tail xs, last xs)"
        `shouldBe` "f xs = (headMay xs, tailMay xs, lastMay xs)"
