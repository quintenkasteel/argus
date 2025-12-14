{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : BooleanSpec
-- Description : Tests for Argus.Rules.Builtin.Boolean
--
-- Comprehensive tests for boolean operation rules.
module BooleanSpec (spec) where

import Test.Hspec
import Data.List (nub)
import Data.Text qualified as T

import Argus.Rules.Builtin.Boolean
import Argus.Rules.DSL (Rule(..), Category(..), SafetyLevel(..), Severity(..))
import Argus.Rules.Engine (mkRuleEngine, evaluateRules)
import Argus.Types (Diagnostic(..))

spec :: Spec
spec = do
  describe "Argus.Rules.Builtin.Boolean" $ do
    boolRulesStructureSpec
    basicBoolRulesSpec
    boolComparisonRulesSpec
    ifThenElseRulesSpec
    boolRulesBehaviorSpec

--------------------------------------------------------------------------------
-- Boolean Rules Structure
--------------------------------------------------------------------------------

boolRulesStructureSpec :: Spec
boolRulesStructureSpec = describe "boolSimplifyRules (all)" $ do
  it "has many rules" $ do
    length boolSimplifyRules `shouldSatisfy` (> 20)

  it "all rules have non-empty IDs" $ do
    all (not . T.null . ruleId) boolSimplifyRules `shouldBe` True

  it "all rules have non-empty messages" $ do
    all (not . T.null . ruleMessage) boolSimplifyRules `shouldBe` True

  it "rule count matches combined length" $ do
    booleanRuleCount `shouldBe` length boolSimplifyRules

--------------------------------------------------------------------------------
-- Basic Bool Rules
--------------------------------------------------------------------------------

basicBoolRulesSpec :: Spec
basicBoolRulesSpec = describe "basicBoolRules" $ do
  it "is non-empty" $ do
    basicBoolRules `shouldSatisfy` (not . null)

  it "contains not-true rule" $ do
    any (\r -> ruleId r == "not-true") basicBoolRules `shouldBe` True

  it "contains not-false rule" $ do
    any (\r -> ruleId r == "not-false") basicBoolRules `shouldBe` True

  describe "notTrue" $ do
    it "has Style category" $ do
      ruleCategory notTrue `shouldBe` Style

  describe "notFalse" $ do
    it "has Style category" $ do
      ruleCategory notFalse `shouldBe` Style

  describe "trueAnd" $ do
    it "has Style category" $ do
      ruleCategory trueAnd `shouldBe` Style

  describe "falseOr" $ do
    it "has Style category" $ do
      ruleCategory falseOr `shouldBe` Style

--------------------------------------------------------------------------------
-- Bool Comparison Rules
--------------------------------------------------------------------------------

boolComparisonRulesSpec :: Spec
boolComparisonRulesSpec = describe "boolComparisonRules" $ do
  it "is non-empty" $ do
    boolComparisonRules `shouldSatisfy` (not . null)

  it "contains eq-true rule" $ do
    any (\r -> ruleId r == "eq-true") boolComparisonRules `shouldBe` True

  it "contains eq-false rule" $ do
    any (\r -> ruleId r == "eq-false") boolComparisonRules `shouldBe` True

  describe "eqTrue" $ do
    it "has Style category" $ do
      ruleCategory eqTrue `shouldBe` Style

  describe "eqFalse" $ do
    it "has Style category" $ do
      ruleCategory eqFalse `shouldBe` Style

  describe "neTrue" $ do
    it "has Style category" $ do
      ruleCategory neTrue `shouldBe` Style

  describe "neFalse" $ do
    it "has Style category" $ do
      ruleCategory neFalse `shouldBe` Style

--------------------------------------------------------------------------------
-- If-Then-Else Rules
--------------------------------------------------------------------------------

ifThenElseRulesSpec :: Spec
ifThenElseRulesSpec = describe "ifThenElseRules" $ do
  it "is non-empty" $ do
    ifThenElseRules `shouldSatisfy` (not . null)

  it "contains if-true rule" $ do
    any (\r -> ruleId r == "if-true") ifThenElseRules `shouldBe` True

  it "contains if-false rule" $ do
    any (\r -> ruleId r == "if-false") ifThenElseRules `shouldBe` True

  describe "ifTrue" $ do
    it "has Style category" $ do
      ruleCategory ifTrue `shouldBe` Style

  describe "ifFalse" $ do
    it "has Style category" $ do
      ruleCategory ifFalse `shouldBe` Style

  describe "ifSameBranch" $ do
    it "has Style category" $ do
      ruleCategory ifSameBranch `shouldBe` Style

    it "has Suggestion severity" $ do
      ruleSeverity ifSameBranch `shouldBe` Suggestion

--------------------------------------------------------------------------------
-- Boolean Rules Behavior (selected tests)
--------------------------------------------------------------------------------

boolRulesBehaviorSpec :: Spec
boolRulesBehaviorSpec = describe "Boolean Rules Behavior" $ do
  describe "notTrue" $ do
    let engine = mkRuleEngine [notTrue]

    it "matches 'not True'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = not True"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "not-true"

  describe "notFalse" $ do
    let engine = mkRuleEngine [notFalse]

    it "matches 'not False'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = not False"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "not-false"

  describe "trueAnd" $ do
    let engine = mkRuleEngine [trueAnd]

    it "matches 'True && x'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = True && x"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "true-and"

  describe "falseOr" $ do
    let engine = mkRuleEngine [falseOr]

    it "matches 'False || x'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = False || x"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "false-or"

  describe "eqTrue" $ do
    let engine = mkRuleEngine [eqTrue]

    it "matches 'x == True'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = x == True"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "eq-true"

  describe "eqFalse" $ do
    let engine = mkRuleEngine [eqFalse]

    it "matches 'x == False'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = x == False"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "eq-false"
