{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : MaybeSpec
-- Description : Tests for Argus.Rules.Builtin.Maybe
--
-- Comprehensive tests for Maybe operation rules.
module MaybeSpec (spec) where

import Test.Hspec
import Data.List (nub)
import Data.Text qualified as T

import Argus.Rules.Builtin.Maybe
import Argus.Rules.DSL (Rule(..), Category(..), SafetyLevel(..), Severity(..))
import Argus.Rules.Engine (mkRuleEngine, evaluateRules)
import Argus.Types (Diagnostic(..))

spec :: Spec
spec = do
  describe "Argus.Rules.Builtin.Maybe" $ do
    maybeRulesStructureSpec
    maybeSimplificationRulesSpec
    maybeListRulesSpec
    maybePatternRulesSpec
    maybeRulesBehaviorSpec

--------------------------------------------------------------------------------
-- Maybe Rules Structure
--------------------------------------------------------------------------------

maybeRulesStructureSpec :: Spec
maybeRulesStructureSpec = describe "maybeRules" $ do
  it "is non-empty" $ do
    maybeRules `shouldSatisfy` (not . null)

  it "contains many rules" $ do
    length maybeRules `shouldSatisfy` (> 20)

  it "has mostly unique IDs" $ do
    let ids = map ruleId maybeRules
        uniqueIds = nub ids
    (fromIntegral (length uniqueIds) / fromIntegral (length ids) :: Double) `shouldSatisfy` (> 0.95)

  it "all rules have non-empty IDs" $ do
    all (not . T.null . ruleId) maybeRules `shouldBe` True

  it "all rules have non-empty messages" $ do
    all (not . T.null . ruleMessage) maybeRules `shouldBe` True

  it "rule count matches length" $ do
    maybeRuleCount `shouldBe` length maybeRules

--------------------------------------------------------------------------------
-- Maybe Simplification Rules
--------------------------------------------------------------------------------

maybeSimplificationRulesSpec :: Spec
maybeSimplificationRulesSpec = describe "maybeSimplificationRules" $ do
  it "is non-empty" $ do
    maybeSimplificationRules `shouldSatisfy` (not . null)

  describe "fromMaybeNothing" $ do
    it "has Style category" $ do
      ruleCategory fromMaybeNothing `shouldBe` Style

  describe "fromMaybeJust" $ do
    it "has Style category" $ do
      ruleCategory fromMaybeJust `shouldBe` Style

  describe "isJustJust" $ do
    it "has Style category" $ do
      ruleCategory isJustJust `shouldBe` Style

  describe "isJustNothing" $ do
    it "has Style category" $ do
      ruleCategory isJustNothing `shouldBe` Style

  describe "isNothingJust" $ do
    it "has Style category" $ do
      ruleCategory isNothingJust `shouldBe` Style

  describe "isNothingNothing" $ do
    it "has Style category" $ do
      ruleCategory isNothingNothing `shouldBe` Style

--------------------------------------------------------------------------------
-- Maybe/List Rules
--------------------------------------------------------------------------------

maybeListRulesSpec :: Spec
maybeListRulesSpec = describe "maybeListRules" $ do
  it "is non-empty" $ do
    maybeListRules `shouldSatisfy` (not . null)

  describe "listToMaybeEmpty" $ do
    it "has Style category" $ do
      ruleCategory listToMaybeEmpty `shouldBe` Style

  describe "listToMaybeSingleton" $ do
    it "has Style category" $ do
      ruleCategory listToMaybeSingleton `shouldBe` Style

  describe "maybeToListJust" $ do
    it "has Style category" $ do
      ruleCategory maybeToListJust `shouldBe` Style

  describe "maybeToListNothing" $ do
    it "has Style category" $ do
      ruleCategory maybeToListNothing `shouldBe` Style

  describe "catMaybesMapJust" $ do
    it "has Style category" $ do
      ruleCategory catMaybesMapJust `shouldBe` Style

--------------------------------------------------------------------------------
-- Maybe Pattern Rules
--------------------------------------------------------------------------------

maybePatternRulesSpec :: Spec
maybePatternRulesSpec = describe "maybePatternRules" $ do
  it "is non-empty" $ do
    maybePatternRules `shouldSatisfy` (not . null)

  describe "fmapJust" $ do
    it "has Style category" $ do
      ruleCategory fmapJust `shouldBe` Style

  describe "fmapNothing" $ do
    it "has Style category" $ do
      ruleCategory fmapNothing `shouldBe` Style

  describe "fromJustJust" $ do
    it "has Style category" $ do
      ruleCategory fromJustJust `shouldBe` Style

--------------------------------------------------------------------------------
-- Maybe Rules Behavior (selected tests)
--------------------------------------------------------------------------------

maybeRulesBehaviorSpec :: Spec
maybeRulesBehaviorSpec = describe "Maybe Rules Behavior" $ do
  describe "fmapJust" $ do
    let engine = mkRuleEngine [fmapJust]

    it "matches 'fmap f (Just x)'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = fmap f (Just x)"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "fmap-just"

    it "does not match 'fmap f Nothing'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = fmap f Nothing"
      length diags `shouldBe` 0

  describe "fmapNothing" $ do
    let engine = mkRuleEngine [fmapNothing]

    it "matches 'fmap f Nothing'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = fmap f Nothing"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "fmap-nothing"

  describe "maybeToListNothing" $ do
    let engine = mkRuleEngine [maybeToListNothing]

    it "matches 'maybeToList Nothing'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = maybeToList Nothing"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "maybeToList-nothing"

  describe "isJustNothing" $ do
    let engine = mkRuleEngine [isJustNothing]

    it "matches 'isJust Nothing'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = isJust Nothing"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "isJust-nothing"

  describe "isNothingJust" $ do
    let engine = mkRuleEngine [isNothingJust]

    it "matches 'isNothing (Just x)'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = isNothing (Just x)"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "isNothing-just"

  describe "listToMaybeEmpty" $ do
    let engine = mkRuleEngine [listToMaybeEmpty]

    it "matches 'listToMaybe []'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = listToMaybe []"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "listToMaybe-empty"
