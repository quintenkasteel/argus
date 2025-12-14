{-# LANGUAGE OverloadedStrings #-}

module LSPHoverSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (isJust, fromJust)

import Argus.LSP.RuleInfo
import Argus.Rules.Types (Category(..), SafetyLevel(..))

spec :: Spec
spec = describe "LSP Hover Rule Explanations" $ do
  ruleExplanationTests
  ruleFormattingTests
  ruleQueryTests
  ruleContentTests

ruleExplanationTests :: Spec
ruleExplanationTests = describe "Rule Explanation Database" $ do
  it "should have explanations for at least 50 rules" $ do
    length getAllRuleInfos `shouldSatisfy` (>= 50)

  it "should have safety rules" $ do
    let safetyRules = getRulesByCategory Safety
    length safetyRules `shouldSatisfy` (>= 10)

  it "should have performance rules" $ do
    let perfRules = getRulesByCategory Performance
    length perfRules `shouldSatisfy` (>= 5)

  it "should have modernization rules" $ do
    let modernRules = getRulesByCategory Modernization
    length modernRules `shouldSatisfy` (>= 5)

ruleFormattingTests :: Spec
ruleFormattingTests = describe "Rule Formatting" $ do
  it "should format rule info as markdown" $ do
    case getRuleExplanation "avoid-head" of
      Nothing -> expectationFailure "avoid-head rule not found"
      Just info -> do
        let formatted = formatRuleInfo info
        formatted `shouldSatisfy` T.isInfixOf "## avoid-head"
        formatted `shouldSatisfy` T.isInfixOf "Category"
        formatted `shouldSatisfy` T.isInfixOf "```haskell"

  it "should format short summary" $ do
    case getRuleExplanation "avoid-head" of
      Nothing -> expectationFailure "avoid-head rule not found"
      Just info -> do
        let summary = formatShortSummary info
        summary `shouldSatisfy` T.isInfixOf "avoid-head"
        summary `shouldSatisfy` (not . T.isInfixOf "```")

  it "should include code examples in formatted output" $ do
    case getRuleExplanation "avoid-head" of
      Nothing -> expectationFailure "avoid-head rule not found"
      Just info -> do
        let formatted = formatRuleInfo info
        formatted `shouldSatisfy` T.isInfixOf "Problematic Code"
        formatted `shouldSatisfy` T.isInfixOf "Recommended Alternative"

ruleQueryTests :: Spec
ruleQueryTests = describe "Rule Query Functions" $ do
  it "should find rule by ID" $ do
    getRuleExplanation "avoid-head" `shouldSatisfy` isJust

  it "should return Nothing for unknown rule" $ do
    getRuleExplanation "nonexistent-rule" `shouldBe` Nothing

  it "should find rule by code" $ do
    getRuleExplanationByCode "SAFETY-001" `shouldSatisfy` isJust

  it "should return all rules" $ do
    getAllRuleInfos `shouldSatisfy` (not . null)

  it "should filter rules by category" $ do
    let safetyRules = getRulesByCategory Safety
    all (\r -> riCategory r == Safety) safetyRules `shouldBe` True

ruleContentTests :: Spec
ruleContentTests = describe "Rule Content Quality" $ do
  describe "avoid-head rule" $ do
    let mInfo = getRuleExplanation "avoid-head"

    it "should exist" $ do
      mInfo `shouldSatisfy` isJust

    it "should have proper ID" $ do
      riRuleId (fromJust mInfo) `shouldBe` "avoid-head"

    it "should have category" $ do
      riCategory (fromJust mInfo) `shouldBe` Safety

    it "should have short description" $ do
      riShortDesc (fromJust mInfo) `shouldSatisfy` (not . T.null)

    it "should have long description" $ do
      riLongDesc (fromJust mInfo) `shouldSatisfy` (not . T.null)

    it "should have rationale" $ do
      riRationale (fromJust mInfo) `shouldSatisfy` (not . T.null)

    it "should have examples" $ do
      riExamples (fromJust mInfo) `shouldSatisfy` (not . null)

    it "should have bad example" $ do
      let examples = riExamples (fromJust mInfo)
      any (\ex -> reType ex == BadExample) examples `shouldBe` True

    it "should have good example" $ do
      let examples = riExamples (fromJust mInfo)
      any (\ex -> reType ex == GoodExample) examples `shouldBe` True

  describe "prefer-foldl' rule" $ do
    let mInfo = getRuleExplanation "prefer-foldl'"

    it "should exist" $ do
      mInfo `shouldSatisfy` isJust

    it "should be categorized as space leak" $ do
      riCategory (fromJust mInfo) `shouldBe` SpaceLeaks

    it "should mention performance impact" $ do
      let longDesc = riLongDesc (fromJust mInfo)
      longDesc `shouldSatisfy` T.isInfixOf "thunk"

  describe "use-fmap rule" $ do
    let mInfo = getRuleExplanation "use-fmap"

    it "should exist" $ do
      mInfo `shouldSatisfy` isJust

    it "should be a modernization rule" $ do
      riCategory (fromJust mInfo) `shouldBe` Modernization

    it "should have fix available" $ do
      riFixAvailable (fromJust mInfo) `shouldBe` True

    it "should be safe to apply" $ do
      riSafetyLevel (fromJust mInfo) `shouldBe` Safe

  describe "avoid-nub rule" $ do
    let mInfo = getRuleExplanation "avoid-nub"

    it "should exist" $ do
      mInfo `shouldSatisfy` isJust

    it "should be a performance rule" $ do
      riCategory (fromJust mInfo) `shouldBe` Performance

    it "should mention complexity" $ do
      let longDesc = riLongDesc (fromJust mInfo)
      longDesc `shouldSatisfy` T.isInfixOf "O(n"

  describe "use-maybe rule" $ do
    let mInfo = getRuleExplanation "use-maybe"

    it "should exist" $ do
      mInfo `shouldSatisfy` isJust

    it "should be a style rule" $ do
      riCategory (fromJust mInfo) `shouldBe` Style

    it "should have related rules" $ do
      riRelatedRules (fromJust mInfo) `shouldSatisfy` (not . null)

  describe "use-applicative rule" $ do
    let mInfo = getRuleExplanation "use-applicative"

    it "should exist" $ do
      mInfo `shouldSatisfy` isJust

    it "should be a modernization rule" $ do
      riCategory (fromJust mInfo) `shouldBe` Modernization

    it "should have documentation links" $ do
      let info = fromJust mInfo
      riDocLinks info `shouldSatisfy` (not . null)
      head (riDocLinks info) `shouldSatisfy` T.isInfixOf "wiki.haskell.org"

  describe "avoid-lazy-io rule" $ do
    let mInfo = getRuleExplanation "avoid-lazy-io"

    it "should exist" $ do
      mInfo `shouldSatisfy` isJust

    it "should be a space leak rule" $ do
      riCategory (fromJust mInfo) `shouldBe` SpaceLeaks

    it "should mention resource management" $ do
      let rationale = riRationale (fromJust mInfo)
      rationale `shouldSatisfy` T.isInfixOf "resource"

  describe "All rules" $ do
    it "should have unique IDs" $ do
      let nub :: Eq a => [a] -> [a]
          nub [] = []
          nub (x:xs) = x : nub (filter (/= x) xs)
          ruleIds = map riRuleId getAllRuleInfos
      length ruleIds `shouldBe` length (nub ruleIds)

    it "should have unique codes (where present)" $ do
      let nub :: Eq a => [a] -> [a]
          nub [] = []
          nub (x:xs) = x : nub (filter (/= x) xs)
          codes = [c | Just c <- map riRuleCode getAllRuleInfos]
      length codes `shouldBe` length (nub codes)

    it "should have non-empty descriptions" $ do
      all (not . T.null . riShortDesc) getAllRuleInfos `shouldBe` True

    it "should have at least one example" $ do
      all (not . null . riExamples) getAllRuleInfos `shouldBe` True

  describe "Markdown formatting" $ do
    it "should properly escape code blocks" $ do
      case getRuleExplanation "avoid-head" of
        Nothing -> expectationFailure "avoid-head rule not found"
        Just info -> do
          let formatted = formatRuleInfo info
          -- Should have opening and closing code blocks
          let codeBlockCount = length $ T.splitOn "```haskell" formatted
          codeBlockCount `shouldSatisfy` (> 1)

    it "should format related rules as links" $ do
      case getRuleExplanation "avoid-head" of
        Nothing -> expectationFailure "avoid-head rule not found"
        Just info | not (null $ riRelatedRules info) -> do
          let formatted = formatRuleInfo info
          formatted `shouldSatisfy` T.isInfixOf "Related Rules"
          formatted `shouldSatisfy` T.isInfixOf "`"
        _ -> pure ()

    it "should format documentation links" $ do
      case getRuleExplanation "avoid-head" of
        Nothing -> expectationFailure "avoid-head rule not found"
        Just info | not (null $ riDocLinks info) -> do
          let formatted = formatRuleInfo info
          formatted `shouldSatisfy` T.isInfixOf "Documentation"
          formatted `shouldSatisfy` T.isInfixOf "https://"
        _ -> pure ()

  describe "Coverage by category" $ do
    it "should have Safety rules" $ do
      let orP f g x = f x || g x
          rules = getRulesByCategory Safety
      length rules `shouldSatisfy` (>= 10)
      mapM_ (\r -> riRuleId r `shouldSatisfy` T.isInfixOf "avoid" `orP` T.isPrefixOf "avoid-") rules

    it "should have Performance rules" $ do
      let rules = getRulesByCategory Performance
      length rules `shouldSatisfy` (>= 5)

    it "should have Modernization rules" $ do
      let rules = getRulesByCategory Modernization
      length rules `shouldSatisfy` (>= 5)

    it "should have Style rules" $ do
      let rules = getRulesByCategory Style
      length rules `shouldSatisfy` (>= 5)

    it "should have Space Leak rules" $ do
      let rules = getRulesByCategory SpaceLeaks
      length rules `shouldSatisfy` (>= 2)
