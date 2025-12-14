{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ListSpec
-- Description : Tests for Argus.Rules.Builtin.List
--
-- Comprehensive tests for list operation rules.
module ListSpec (spec) where

import Test.Hspec
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Text qualified as T

import Argus.Rules.Builtin.List
import Argus.Rules.DSL (Rule(..), Category(..), SafetyLevel(..), Severity(..))
import Argus.Rules.Engine (mkRuleEngine, evaluateRules)
import Argus.Types (Diagnostic(..))

spec :: Spec
spec = do
  describe "Argus.Rules.Builtin.List" $ do
    listRulesStructureSpec
    listSimplificationRulesSpec
    listComprehensionRulesSpec
    listConstructionRulesSpec
    listIndexingRulesSpec
    listTransformRulesSpec
    listRulesBehaviorSpec

--------------------------------------------------------------------------------
-- List Rules Structure
--------------------------------------------------------------------------------

listRulesStructureSpec :: Spec
listRulesStructureSpec = describe "listRules" $ do
  it "is non-empty" $ do
    listRules `shouldSatisfy` (not . null)

  it "contains many rules" $ do
    length listRules `shouldSatisfy` (> 30)

  it "has mostly unique IDs" $ do
    let ids = map ruleId listRules
        uniqueIds = nub ids
    (fromIntegral (length uniqueIds) / fromIntegral (length ids) :: Double) `shouldSatisfy` (> 0.95)

  it "all rules have non-empty IDs" $ do
    all (not . T.null . ruleId) listRules `shouldBe` True

  it "all rules have non-empty messages" $ do
    all (not . T.null . ruleMessage) listRules `shouldBe` True

  it "all rules have valid categories" $ do
    let validCategories = [Style, Performance, Correctness, Safety, Modernization]
    all (\r -> ruleCategory r `elem` validCategories) listRules `shouldBe` True

  it "rule count matches length" $ do
    listRuleCount `shouldBe` length listRules

--------------------------------------------------------------------------------
-- List Simplification Rules
--------------------------------------------------------------------------------

listSimplificationRulesSpec :: Spec
listSimplificationRulesSpec = describe "listSimplificationRules" $ do
  it "is non-empty" $ do
    listSimplificationRules `shouldSatisfy` (not . null)

  it "contains concat-map rule" $ do
    any (\r -> ruleId r == "concat-map") listSimplificationRules `shouldBe` True

  it "contains length-zero rule" $ do
    any (\r -> ruleId r == "length-zero") listSimplificationRules `shouldBe` True

  it "contains head-reverse rule" $ do
    any (\r -> ruleId r == "head-reverse") listSimplificationRules `shouldBe` True

  it "contains last-reverse rule" $ do
    any (\r -> ruleId r == "last-reverse") listSimplificationRules `shouldBe` True

  describe "concatMapRule" $ do
    it "has Performance category" $ do
      ruleCategory concatMapRule `shouldBe` Performance

    it "has Suggestion severity" $ do
      ruleSeverity concatMapRule `shouldBe` Suggestion

    it "has correct ID" $ do
      ruleId concatMapRule `shouldBe` "concat-map"

  describe "lengthZero" $ do
    it "has correct ID" $ do
      ruleId lengthZero `shouldBe` "length-zero"

    it "has Performance category" $ do
      ruleCategory lengthZero `shouldBe` Performance

  describe "headReverse" $ do
    it "has Performance category" $ do
      ruleCategory headReverse `shouldBe` Performance

    it "suggests last" $ do
      T.isInfixOf "last" (ruleMessage headReverse) `shouldBe` True

--------------------------------------------------------------------------------
-- List Comprehension Rules
--------------------------------------------------------------------------------

listComprehensionRulesSpec :: Spec
listComprehensionRulesSpec = describe "listComprehensionRules" $ do
  it "is non-empty" $ do
    listComprehensionRules `shouldSatisfy` (not . null)

  it "contains list-comp-id rule" $ do
    any (\r -> ruleId r == "list-comp-id") listComprehensionRules `shouldBe` True

  it "contains list-comp-map rule" $ do
    any (\r -> ruleId r == "list-comp-map") listComprehensionRules `shouldBe` True

  describe "listCompId" $ do
    it "has Style category" $ do
      ruleCategory listCompId `shouldBe` Style

  describe "listCompMap" $ do
    it "has Style category" $ do
      ruleCategory listCompMap `shouldBe` Style

  describe "listCompFilter" $ do
    it "has Style category" $ do
      ruleCategory listCompFilter `shouldBe` Style

--------------------------------------------------------------------------------
-- List Construction Rules
--------------------------------------------------------------------------------

listConstructionRulesSpec :: Spec
listConstructionRulesSpec = describe "listConstructionRules" $ do
  it "is non-empty" $ do
    listConstructionRules `shouldSatisfy` (not . null)

  it "contains singleton-cons rule" $ do
    any (\r -> ruleId r == "singleton-cons") listConstructionRules `shouldBe` True

  it "contains empty-append rule" $ do
    any (\r -> ruleId r == "empty-append") listConstructionRules `shouldBe` True

  describe "singletonCons" $ do
    it "has Style category" $ do
      ruleCategory singletonCons `shouldBe` Style

    it "has Suggestion severity" $ do
      ruleSeverity singletonCons `shouldBe` Suggestion

  describe "emptyAppend" $ do
    it "has Style category" $ do
      ruleCategory emptyAppend `shouldBe` Style

  describe "appendEmpty" $ do
    it "has Style category" $ do
      ruleCategory appendEmpty `shouldBe` Style

--------------------------------------------------------------------------------
-- List Indexing Rules
--------------------------------------------------------------------------------

listIndexingRulesSpec :: Spec
listIndexingRulesSpec = describe "listIndexingRules" $ do
  it "is non-empty" $ do
    listIndexingRules `shouldSatisfy` (not . null)

  it "contains index-zero rule" $ do
    any (\r -> ruleId r == "index-zero") listIndexingRules `shouldBe` True

  describe "indexZero" $ do
    it "has Style category" $ do
      ruleCategory indexZero `shouldBe` Style

    it "suggests head" $ do
      T.isInfixOf "head" (ruleMessage indexZero) `shouldBe` True

  describe "takeOneWarn" $ do
    it "has correct ID" $ do
      ruleId takeOneWarn `shouldBe` "take-one-warn"

  describe "dropOneWarn" $ do
    it "has correct ID" $ do
      ruleId dropOneWarn `shouldBe` "drop-one-warn"

--------------------------------------------------------------------------------
-- List Transform Rules
--------------------------------------------------------------------------------

listTransformRulesSpec :: Spec
listTransformRulesSpec = describe "listTransformRules" $ do
  it "is non-empty" $ do
    listTransformRules `shouldSatisfy` (not . null)

  it "contains filter-true rule" $ do
    any (\r -> ruleId r == "filter-true") listTransformRules `shouldBe` True

  it "contains take-zero rule" $ do
    any (\r -> ruleId r == "take-zero") listTransformRules `shouldBe` True

  it "contains drop-zero rule" $ do
    any (\r -> ruleId r == "drop-zero") listTransformRules `shouldBe` True

  describe "filterTrue" $ do
    it "has Style category" $ do
      ruleCategory filterTrue `shouldBe` Style

  describe "filterFalse" $ do
    it "has Style category" $ do
      ruleCategory filterFalse `shouldBe` Style

  describe "takeZero" $ do
    it "has Style category" $ do
      ruleCategory takeZero `shouldBe` Style

  describe "dropZero" $ do
    it "has Style category" $ do
      ruleCategory dropZero `shouldBe` Style

  describe "nubOrd" $ do
    it "has Performance category" $ do
      ruleCategory nubOrd `shouldBe` Performance

    it "warns about O(n^2)" $ do
      T.isInfixOf "O(n^2)" (ruleMessage nubOrd) `shouldBe` True

--------------------------------------------------------------------------------
-- List Rules Behavior (selected tests)
--------------------------------------------------------------------------------

listRulesBehaviorSpec :: Spec
listRulesBehaviorSpec = describe "List Rules Behavior" $ do
  describe "concatMapRule" $ do
    let engine = mkRuleEngine [concatMapRule]

    it "matches 'concat (map f xs)'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = concat (map f xs)"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "concat-map"

    it "does not match 'concatMap f xs'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "result = concatMap f xs"
      length diags `shouldBe` 0

  describe "headReverse" $ do
    let engine = mkRuleEngine [headReverse]

    it "matches 'head (reverse xs)'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "first = head (reverse items)"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "head-reverse"

  describe "lastReverse" $ do
    let engine = mkRuleEngine [lastReverse]

    it "matches 'last (reverse xs)'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "first = last (reverse items)"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "last-reverse"

  describe "concatSingleList" $ do
    let engine = mkRuleEngine [concatSingleList]

    it "matches 'concat [xs]'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "flat = concat [items]"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "concat-single-list"

  describe "takeRepeat" $ do
    let engine = mkRuleEngine [takeRepeat]

    it "matches 'take n (repeat x)'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "xs = take n (repeat x)"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "take-repeat"

  describe "cycleEmpty" $ do
    let engine = mkRuleEngine [cycleEmpty]

    it "matches 'cycle []'" $ do
      let diags = evaluateRules engine "Test.hs" "Test" "infinite = cycle []"
      length diags `shouldBe` 1
      diagCode (head diags) `shouldBe` Just "cycle-empty"

    it "has Error severity" $ do
      ruleSeverity cycleEmpty `shouldBe` Error

    it "has Correctness category" $ do
      ruleCategory cycleEmpty `shouldBe` Correctness
