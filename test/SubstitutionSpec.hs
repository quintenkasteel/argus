{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : SubstitutionSpec
-- Description : Comprehensive tests for Argus.Refactor.Substitution
-- Copyright   : (c) 2024
-- License     : MIT
module SubstitutionSpec (spec) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Argus.Refactor.Substitution
import Argus.Rules.Types
  ( PatternAST(..)
  , Pattern
  , PatternVar(..)
  , TypePattern(..)
  , Binding(..)
  , Bindings
  , mkBinding
  )

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
  describe "Argus.Refactor.Substitution" $ do
    substituteExprSpec
    edgeCasesSpec
    scopeSpec
    substitutePatternSpec
    substituteTypeSpec
    substituteNameSpec
    freshenNameSpec
    avoidCaptureSpec
    instancesSpec

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

--------------------------------------------------------------------------------
-- Scope Tests
--------------------------------------------------------------------------------

scopeSpec :: Spec
scopeSpec = describe "Scope" $ do
  describe "emptyScope" $ do
    it "has no bindings" $ do
      scopeBindings emptyScope `shouldBe` Set.empty

    it "has no free variables" $ do
      scopeFree emptyScope `shouldBe` Set.empty

    it "starts counter at 0" $ do
      scopeCounter emptyScope `shouldBe` 0

  describe "inScope" $ do
    it "returns False for empty scope" $ do
      inScope "x" emptyScope `shouldBe` False

    it "returns True after adding binding" $ do
      let scope = addBinding "x" emptyScope
      inScope "x" scope `shouldBe` True

    it "returns False for different name" $ do
      let scope = addBinding "x" emptyScope
      inScope "y" scope `shouldBe` False

  describe "addBinding" $ do
    it "adds name to scope bindings" $ do
      let scope = addBinding "foo" emptyScope
      Set.member "foo" (scopeBindings scope) `shouldBe` True

    it "preserves existing bindings" $ do
      let scope = addBinding "bar" $ addBinding "foo" emptyScope
      Set.member "foo" (scopeBindings scope) `shouldBe` True
      Set.member "bar" (scopeBindings scope) `shouldBe` True

    it "preserves counter" $ do
      let scope = addBinding "x" emptyScope
      scopeCounter scope `shouldBe` 0

--------------------------------------------------------------------------------
-- substitute Tests (Pattern AST)
--------------------------------------------------------------------------------

substitutePatternSpec :: Spec
substitutePatternSpec = describe "substitute" $ do
  describe "PWildcard" $ do
    it "renders as underscore" $ do
      substitute Map.empty PWildcard `shouldBe` "_"

  describe "PVar" $ do
    it "returns bound value" $ do
      let bindings = Map.singleton "$X" (mkBinding "myValue")
          pv = PatternVar "$X" Nothing
      substitute bindings (PVar pv) `shouldBe` "myValue"

    it "returns variable name when not bound" $ do
      let pv = PatternVar "$X" Nothing
      substitute Map.empty (PVar pv) `shouldBe` "$X"

  describe "PLiteral" $ do
    it "returns literal text unchanged" $ do
      substitute Map.empty (PLiteral "42") `shouldBe` "42"
      substitute Map.empty (PLiteral "\"hello\"") `shouldBe` "\"hello\""

  describe "PConstructor" $ do
    it "renders constructor with no args" $ do
      substitute Map.empty (PConstructor "Nothing" []) `shouldBe` "Nothing "

    it "renders constructor with args" $ do
      let bindings = Map.singleton "$X" (mkBinding "value")
          pat = PConstructor "Just" [PVar (PatternVar "$X" Nothing)]
      substitute bindings pat `shouldBe` "Just value"

  describe "PApplication" $ do
    it "renders simple application" $ do
      let pat = PApplication (PLiteral "f") (PLiteral "x")
      substitute Map.empty pat `shouldBe` "f x"

    it "wraps nested application in parens" $ do
      let inner = PApplication (PLiteral "g") (PLiteral "y")
          pat = PApplication (PLiteral "f") inner
      substitute Map.empty pat `shouldBe` "f (g y)"

  describe "PInfix" $ do
    it "renders infix operator" $ do
      let pat = PInfix (PLiteral "x") "+" (PLiteral "y")
      substitute Map.empty pat `shouldBe` "x + y"

    it "substitutes in operands" $ do
      let bindings = Map.fromList [("$L", mkBinding "a"), ("$R", mkBinding "b")]
          pat = PInfix (PVar (PatternVar "$L" Nothing)) "<>" (PVar (PatternVar "$R" Nothing))
      substitute bindings pat `shouldBe` "a <> b"

  describe "PTuple" $ do
    it "renders tuple" $ do
      let pat = PTuple [PLiteral "x", PLiteral "y", PLiteral "z"]
      substitute Map.empty pat `shouldBe` "(x, y, z)"

    it "renders empty tuple" $ do
      substitute Map.empty (PTuple []) `shouldBe` "()"

  describe "PList" $ do
    it "renders list" $ do
      let pat = PList [PLiteral "1", PLiteral "2", PLiteral "3"]
      substitute Map.empty pat `shouldBe` "[1, 2, 3]"

    it "renders empty list" $ do
      substitute Map.empty (PList []) `shouldBe` "[]"

  describe "PParens" $ do
    it "renders parenthesized expression" $ do
      let pat = PParens (PLiteral "x")
      substitute Map.empty pat `shouldBe` "(x)"

  describe "PTyped" $ do
    it "renders expression without type annotation" $ do
      let pat = PTyped (PLiteral "x") TPWildcard
      substitute Map.empty pat `shouldBe` "x"

--------------------------------------------------------------------------------
-- substituteType Tests
--------------------------------------------------------------------------------

substituteTypeSpec :: Spec
substituteTypeSpec = describe "substituteType" $ do
  it "behaves like substituteExpr" $ do
    let subs = Map.singleton "Int" "Integer"
    substituteType subs "x :: Int" `shouldBe` substituteExpr subs "x :: Int"

  it "replaces type names" $ do
    let subs = Map.singleton "String" "Text"
    substituteType subs "foo :: String -> IO String" `shouldBe` "foo :: Text -> IO Text"

--------------------------------------------------------------------------------
-- substituteName Tests
--------------------------------------------------------------------------------

substituteNameSpec :: Spec
substituteNameSpec = describe "substituteName" $ do
  it "replaces exact match" $ do
    substituteName "foo" "bar" "foo" `shouldBe` "bar"

  it "does not replace non-matching" $ do
    substituteName "foo" "bar" "baz" `shouldBe` "baz"

  it "does not replace partial match" $ do
    substituteName "foo" "bar" "foobar" `shouldBe` "foobar"

  it "handles empty strings" $ do
    substituteName "" "bar" "" `shouldBe` "bar"
    substituteName "foo" "bar" "" `shouldBe` ""

--------------------------------------------------------------------------------
-- freshenName Tests
--------------------------------------------------------------------------------

freshenNameSpec :: Spec
freshenNameSpec = describe "freshenName" $ do
  it "generates unique name with counter" $ do
    let (name, _) = freshenName "x" emptyScope
    name `shouldBe` "x0"

  it "increments counter" $ do
    let (_, scope1) = freshenName "x" emptyScope
        (name2, _) = freshenName "y" scope1
    name2 `shouldBe` "y1"

  it "adds name to bindings" $ do
    let (name, scope) = freshenName "x" emptyScope
    inScope name scope `shouldBe` True

  it "preserves base name prefix" $ do
    let (name, _) = freshenName "myVar" emptyScope
    T.isPrefixOf "myVar" name `shouldBe` True

--------------------------------------------------------------------------------
-- avoidCapture Tests
--------------------------------------------------------------------------------

avoidCaptureSpec :: Spec
avoidCaptureSpec = describe "avoidCapture" $ do
  it "returns empty renames when no capture" $ do
    let bound = Set.fromList ["x", "y"]
        free = Set.fromList ["a", "b"]
        bindings = Map.fromList [("$A", mkBinding "a"), ("$B", mkBinding "b")]
        (_, renames) = avoidCapture bound free bindings
    Map.null renames `shouldBe` True

  it "renames captured variables" $ do
    let bound = Set.fromList ["x", "y"]
        free = Set.fromList ["x", "z"]  -- "x" is both bound and free
        bindings = Map.singleton "$X" (mkBinding "x")
        (_, renames) = avoidCapture bound free bindings
    Map.member "x" renames `shouldBe` True

  it "updates binding values with renames" $ do
    let bound = Set.fromList ["x"]
        free = Set.fromList ["x"]
        bindings = Map.singleton "$X" (mkBinding "x")
        (newBindings, _) = avoidCapture bound free bindings
    case Map.lookup "$X" newBindings of
      Just b -> bindingValue b `shouldSatisfy` (/= "x")
      Nothing -> expectationFailure "Expected binding to exist"

  it "generates fresh names not in bound or free sets" $ do
    let bound = Set.fromList ["x", "x0", "x1"]
        free = Set.fromList ["x"]
        bindings = Map.singleton "$X" (mkBinding "x")
        (_, renames) = avoidCapture bound free bindings
    case Map.lookup "x" renames of
      Just newName -> do
        Set.member newName bound `shouldBe` False
        Set.member newName free `shouldBe` False
      Nothing -> expectationFailure "Expected rename for x"

--------------------------------------------------------------------------------
-- Eq and Show instances
--------------------------------------------------------------------------------

instancesSpec :: Spec
instancesSpec = describe "instances" $ do
  describe "Scope" $ do
    it "has Eq instance" $ do
      emptyScope `shouldBe` emptyScope
      addBinding "x" emptyScope `shouldNotBe` emptyScope

    it "has Show instance" $ do
      let s = show emptyScope
      s `shouldContain` "Scope"
