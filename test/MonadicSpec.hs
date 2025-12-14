{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : MonadicSpec
-- Description : Tests for Argus.Rules.Builtin.Monadic
--
-- Comprehensive tests for monadic modernization rules.
module MonadicSpec (spec) where

import Test.Hspec
import Data.List (nub)
import Data.Text qualified as T

import Argus.Rules.Builtin.Monadic
import Argus.Rules.DSL (Rule(..), Category(..), SafetyLevel(..), Severity(..))
import Argus.Rules.Engine (mkRuleEngine, evaluateRules)
import Argus.Types (Diagnostic(..))

spec :: Spec
spec = do
  describe "Argus.Rules.Builtin.Monadic" $ do
    monadicRulesStructureSpec
    applicativeModernRulesSpec
    traversalRulesSpec
    doNotationRulesSpec
    transformerRulesSpec
    bindRulesSpec
    monadicRulesBehaviorSpec

--------------------------------------------------------------------------------
-- Monadic Rules Structure
--------------------------------------------------------------------------------

monadicRulesStructureSpec :: Spec
monadicRulesStructureSpec = describe "monadicRules" $ do
  it "is non-empty" $ do
    monadicRules `shouldSatisfy` (not . null)

  it "contains many rules" $ do
    length monadicRules `shouldSatisfy` (> 15)

  it "has mostly unique IDs" $ do
    let ids = map ruleId monadicRules
        uniqueIds = nub ids
    (fromIntegral (length uniqueIds) / fromIntegral (length ids) :: Double) `shouldSatisfy` (> 0.95)

  it "all rules have non-empty IDs" $ do
    all (not . T.null . ruleId) monadicRules `shouldBe` True

  it "all rules have non-empty messages" $ do
    all (not . T.null . ruleMessage) monadicRules `shouldBe` True

  it "rule count matches length" $ do
    monadicRuleCount `shouldBe` length monadicRules

--------------------------------------------------------------------------------
-- Applicative Modernization Rules
--------------------------------------------------------------------------------

applicativeModernRulesSpec :: Spec
applicativeModernRulesSpec = describe "applicativeModernRules" $ do
  it "is non-empty" $ do
    applicativeModernRules `shouldSatisfy` (not . null)

  it "contains return-to-pure rule" $ do
    any (\r -> ruleId r == "return-to-pure") applicativeModernRules `shouldBe` True

  it "contains liftm-to-fmap rule" $ do
    any (\r -> ruleId r == "liftm-to-fmap") applicativeModernRules `shouldBe` True

  describe "returnToPure" $ do
    it "has Modernization category" $ do
      ruleCategory returnToPure `shouldBe` Modernization

    it "has Suggestion severity" $ do
      ruleSeverity returnToPure `shouldBe` Suggestion

  describe "liftMToFmap" $ do
    it "has Modernization category" $ do
      ruleCategory liftMToFmap `shouldBe` Modernization

  describe "liftM2ToLiftA2" $ do
    it "has correct ID" $ do
      ruleId liftM2ToLiftA2 `shouldBe` "liftm2-to-lifta2"

    it "has Modernization category" $ do
      ruleCategory liftM2ToLiftA2 `shouldBe` Modernization

--------------------------------------------------------------------------------
-- Traversal Rules
--------------------------------------------------------------------------------

traversalRulesSpec :: Spec
traversalRulesSpec = describe "traversalRules" $ do
  it "is non-empty" $ do
    traversalRules `shouldSatisfy` (not . null)

  it "contains mapm-to-traverse rule" $ do
    any (\r -> ruleId r == "mapm-to-traverse") traversalRules `shouldBe` True

  describe "mapMToTraverse" $ do
    it "has Modernization category" $ do
      ruleCategory mapMToTraverse `shouldBe` Modernization

  describe "sequenceToSequenceA" $ do
    it "has correct ID" $ do
      ruleId sequenceToSequenceA `shouldBe` "sequence-to-sequencea"

    it "has Modernization category" $ do
      ruleCategory sequenceToSequenceA `shouldBe` Modernization

--------------------------------------------------------------------------------
-- Do-Notation Rules
--------------------------------------------------------------------------------

doNotationRulesSpec :: Spec
doNotationRulesSpec = describe "doNotationRules" $ do
  it "is non-empty" $ do
    doNotationRules `shouldSatisfy` (not . null)

  it "all have valid categories" $ do
    let validCategories = [Style, Modernization, Correctness]
    all (\r -> ruleCategory r `elem` validCategories) doNotationRules `shouldBe` True

--------------------------------------------------------------------------------
-- Transformer Rules
--------------------------------------------------------------------------------

transformerRulesSpec :: Spec
transformerRulesSpec = describe "transformerRules" $ do
  it "is non-empty" $ do
    transformerRules `shouldSatisfy` (not . null)

  describe "liftToMonadIO" $ do
    it "has correct ID" $ do
      ruleId liftToMonadIO `shouldBe` "lift-to-monadio"

    it "has Style category" $ do
      ruleCategory liftToMonadIO `shouldBe` Style

  describe "askToReader" $ do
    it "has correct ID" $ do
      ruleId askToReader `shouldBe` "ask-to-reader"

--------------------------------------------------------------------------------
-- Bind Rules
--------------------------------------------------------------------------------

bindRulesSpec :: Spec
bindRulesSpec = describe "bindRules" $ do
  it "is non-empty" $ do
    bindRules `shouldSatisfy` (not . null)

  it "all have valid categories" $ do
    let validCategories = [Style, Modernization, Performance]
    all (\r -> ruleCategory r `elem` validCategories) bindRules `shouldBe` True

--------------------------------------------------------------------------------
-- Monadic Rules Behavior (selected tests)
--------------------------------------------------------------------------------

monadicRulesBehaviorSpec :: Spec
monadicRulesBehaviorSpec = describe "Monadic Rules Behavior" $ do
  describe "returnToPure" $ do
    let engine = mkRuleEngine [returnToPure]

    it "matches 'return x'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = return x"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "return-to-pure"

    it "does not match 'pure x'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = pure x"
      length diags `shouldBe` 0

  describe "liftMToFmap" $ do
    let engine = mkRuleEngine [liftMToFmap]

    it "matches 'liftM f m'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = liftM f m"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "liftm-to-fmap"

  describe "mapMToTraverse" $ do
    let engine = mkRuleEngine [mapMToTraverse]

    it "matches 'mapM f xs'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = mapM f xs"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "mapm-to-traverse"

  describe "liftM2ToLiftA2" $ do
    let engine = mkRuleEngine [liftM2ToLiftA2]

    it "matches 'liftM2 f m1 m2'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = liftM2 f m1 m2"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "liftm2-to-lifta2"

  describe "sequenceToSequenceA" $ do
    let engine = mkRuleEngine [sequenceToSequenceA]

    it "matches 'sequence xs'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = sequence xs"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "sequence-to-sequencea"
