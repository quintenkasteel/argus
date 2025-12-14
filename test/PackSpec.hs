{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : PackSpec
-- Description : Tests for rule pack system
-- Copyright   : (c) 2024
-- License     : MIT
module PackSpec (spec) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import Argus.Rules.Pack

spec :: Spec
spec = do
  describe "Argus.Rules.Pack" $ do
    describe "PackConfig" $ do
      it "has sensible defaults" $ do
        pcEnabledPacks defaultPackConfig `shouldBe` ["default"]
        pcDisabledPacks defaultPackConfig `shouldBe` []
        pcDefaultEnabled defaultPackConfig `shouldBe` False

      it "can be customized" $ do
        let config = defaultPackConfig
              { pcEnabledPacks = ["security", "performance"]
              , pcDisabledRules = ["avoid-head"]
              }
        pcEnabledPacks config `shouldBe` ["security", "performance"]
        pcDisabledRules config `shouldBe` ["avoid-head"]

    describe "RulePack" $ do
      it "securityPack has expected properties" $ do
        rpName securityPack `shouldBe` "security"
        rpPriority securityPack `shouldBe` 100
        rpCategory securityPack `shouldBe` "safety"
        rpRuleIds securityPack `shouldSatisfy` (not . null)

      it "performancePack has expected properties" $ do
        rpName performancePack `shouldBe` "performance"
        rpPriority performancePack `shouldBe` 80
        rpRuleIds performancePack `shouldSatisfy` (not . null)

      it "stylePack is disabled by default" $ do
        rpEnabled stylePack `shouldBe` False

      it "correctnessPack is enabled by default" $ do
        rpEnabled correctnessPack `shouldBe` True

      it "strictPack includes other packs" $ do
        rpIncludes strictPack `shouldSatisfy` ("security" `elem`)
        rpIncludes strictPack `shouldSatisfy` ("performance" `elem`)

    describe "builtinPacks" $ do
      it "contains expected packs" $ do
        let packNames = map rpName builtinPacks
        "security" `elem` packNames `shouldBe` True
        "performance" `elem` packNames `shouldBe` True
        "style" `elem` packNames `shouldBe` True
        "correctness" `elem` packNames `shouldBe` True

      it "has no duplicate pack names" $ do
        let packNames = map rpName builtinPacks
        length packNames `shouldBe` length (Set.toList $ Set.fromList packNames)

    describe "PackRegistry" $ do
      it "creates new registry with builtin packs" $ do
        registry <- newPackRegistry
        packs <- listPacks registry
        length packs `shouldSatisfy` (>= length builtinPacks)

      it "can get pack by name" $ do
        registry <- newPackRegistry
        mPack <- getPackByName registry "security"
        mPack `shouldSatisfy` (\case Just p -> rpName p == "security"; Nothing -> False)

      it "returns Nothing for unknown pack" $ do
        registry <- newPackRegistry
        mPack <- getPackByName registry "nonexistent-pack"
        mPack `shouldBe` Nothing

      it "can register custom pack" $ do
        registry <- newPackRegistry
        let customPack = RulePack
              { rpName = "my-custom-pack"
              , rpDescription = "Test pack"
              , rpRuleIds = ["rule-1", "rule-2"]
              , rpIncludes = []
              , rpExcludes = []
              , rpPriority = 50
              , rpCategory = "custom"
              , rpTags = ["test"]
              , rpEnabled = True
              }
        registerPack registry customPack
        mPack <- getPackByName registry "my-custom-pack"
        mPack `shouldSatisfy` (\case Just p -> rpName p == "my-custom-pack"; Nothing -> False)

      it "can unregister custom pack" $ do
        registry <- newPackRegistry
        let customPack = RulePack
              { rpName = "temp-pack"
              , rpDescription = "Temporary"
              , rpRuleIds = []
              , rpIncludes = []
              , rpExcludes = []
              , rpPriority = 50
              , rpCategory = "custom"
              , rpTags = []
              , rpEnabled = True
              }
        registerPack registry customPack
        unregisterPack registry "temp-pack"
        mPack <- getPackByName registry "temp-pack"
        mPack `shouldBe` Nothing

    describe "validatePack" $ do
      it "returns empty for valid pack" $ do
        registry <- newPackRegistry
        let validPack = RulePack
              { rpName = "valid"
              , rpDescription = "Valid pack"
              , rpRuleIds = ["some-rule"]
              , rpIncludes = []
              , rpExcludes = []
              , rpPriority = 50
              , rpCategory = "test"
              , rpTags = []
              , rpEnabled = True
              }
        warnings <- validatePack registry validPack
        warnings `shouldBe` []

      it "warns about unknown includes" $ do
        registry <- newPackRegistry
        let invalidPack = RulePack
              { rpName = "invalid"
              , rpDescription = "Invalid pack"
              , rpRuleIds = []
              , rpIncludes = ["nonexistent-pack"]
              , rpExcludes = []
              , rpPriority = 50
              , rpCategory = "test"
              , rpTags = []
              , rpEnabled = True
              }
        warnings <- validatePack registry invalidPack
        length warnings `shouldSatisfy` (> 0)
        any (T.isInfixOf "Unknown pack") warnings `shouldBe` True

      it "warns about self-include" $ do
        registry <- newPackRegistry
        let selfIncludePack = RulePack
              { rpName = "self-include"
              , rpDescription = "Self include"
              , rpRuleIds = []
              , rpIncludes = ["self-include"]
              , rpExcludes = []
              , rpPriority = 50
              , rpCategory = "test"
              , rpTags = []
              , rpEnabled = True
              }
        warnings <- validatePack registry selfIncludePack
        any (T.isInfixOf "includes itself") warnings `shouldBe` True

      it "warns about empty pack" $ do
        registry <- newPackRegistry
        let emptyPack = RulePack
              { rpName = "empty"
              , rpDescription = "Empty pack"
              , rpRuleIds = []
              , rpIncludes = []
              , rpExcludes = []
              , rpPriority = 50
              , rpCategory = "test"
              , rpTags = []
              , rpEnabled = True
              }
        warnings <- validatePack registry emptyPack
        any (T.isInfixOf "no rules") warnings `shouldBe` True

    describe "getPackRules" $ do
      it "returns rules for enabled pack" $ do
        registry <- newPackRegistry
        let config = defaultPackConfig { pcEnabledPacks = ["security"] }
        rules <- getPackRules registry config
        Set.size rules `shouldSatisfy` (> 0)

      it "returns empty for disabled pack" $ do
        registry <- newPackRegistry
        let config = defaultPackConfig { pcEnabledPacks = [] }
        rules <- getPackRules registry config
        Set.size rules `shouldBe` 0

      it "combines rules from multiple packs" $ do
        registry <- newPackRegistry
        let config = defaultPackConfig
              { pcEnabledPacks = ["security", "performance"] }
        rules <- getPackRules registry config
        -- Should have rules from both packs
        Set.size rules `shouldSatisfy` (> length (rpRuleIds securityPack))

      it "respects pcDisabledRules override" $ do
        registry <- newPackRegistry
        let config = defaultPackConfig
              { pcEnabledPacks = ["security"]
              , pcDisabledRules = ["avoid-unsafe-perform-io"]
              }
        rules <- getPackRules registry config
        "avoid-unsafe-perform-io" `Set.member` rules `shouldBe` False

      it "respects pcEnabledRules override" $ do
        registry <- newPackRegistry
        let config = defaultPackConfig
              { pcEnabledPacks = []
              , pcEnabledRules = ["my-custom-rule"]
              }
        rules <- getPackRules registry config
        "my-custom-rule" `Set.member` rules `shouldBe` True

    describe "resolvePacks" $ do
      it "returns PackResolution with expected fields" $ do
        registry <- newPackRegistry
        let config = defaultPackConfig { pcEnabledPacks = ["security"] }
        resolution <- resolvePacks registry config
        prActivePacks resolution `shouldBe` ["security"]
        Set.size (prEnabledRules resolution) `shouldSatisfy` (> 0)

      it "tracks warnings for unknown packs" $ do
        registry <- newPackRegistry
        let config = defaultPackConfig { pcEnabledPacks = ["nonexistent"] }
        resolution <- resolvePacks registry config
        any (T.isInfixOf "Unknown pack") (prWarnings resolution) `shouldBe` True

      it "expands included packs" $ do
        registry <- newPackRegistry
        let config = defaultPackConfig { pcEnabledPacks = ["strict"] }
        resolution <- resolvePacks registry config
        -- strict includes security, which has rules
        Set.size (prEnabledRules resolution) `shouldSatisfy` (> 0)

      it "applies excludes correctly" $ do
        registry <- newPackRegistry
        let customPack = RulePack
              { rpName = "exclude-test"
              , rpDescription = "Test excludes"
              , rpRuleIds = ["rule-a", "rule-b"]
              , rpIncludes = []
              , rpExcludes = ["rule-a"]
              , rpPriority = 50
              , rpCategory = "test"
              , rpTags = []
              , rpEnabled = True
              }
        registerPack registry customPack
        let config = defaultPackConfig { pcEnabledPacks = ["exclude-test"] }
        rules <- getPackRules registry config
        "rule-a" `Set.member` rules `shouldBe` False
        "rule-b" `Set.member` rules `shouldBe` True

    describe "expandPack" $ do
      it "expands single pack" $ do
        registry <- newPackRegistry
        rules <- expandPack registry "security"
        Set.size rules `shouldSatisfy` (> 0)

      it "returns empty for unknown pack" $ do
        registry <- newPackRegistry
        rules <- expandPack registry "nonexistent"
        Set.size rules `shouldBe` 0

      it "includes rules from included packs" $ do
        registry <- newPackRegistry
        rules <- expandPack registry "strict"
        -- strict includes security, so should have security rules
        "avoid-unsafe-perform-io" `Set.member` rules `shouldBe` True

    describe "mergePackRules" $ do
      it "merges rules from multiple packs" $ do
        let pack1 = securityPack { rpRuleIds = ["rule-1", "rule-2"] }
            pack2 = performancePack { rpRuleIds = ["rule-2", "rule-3"] }
            merged = mergePackRules [pack1, pack2]
        Set.size merged `shouldBe` 3

      it "applies excludes" $ do
        let pack1 = securityPack { rpRuleIds = ["rule-1", "rule-2"], rpExcludes = [] }
            pack2 = performancePack { rpRuleIds = ["rule-3"], rpExcludes = ["rule-1"] }
            merged = mergePackRules [pack1, pack2]
        "rule-1" `Set.member` merged `shouldBe` False

      it "handles empty pack list" $ do
        mergePackRules [] `shouldBe` Set.empty

    describe "isRuleInPack" $ do
      it "returns True for rule in pack" $ do
        registry <- newPackRegistry
        result <- isRuleInPack registry "avoid-unsafe-perform-io" "security"
        result `shouldBe` True

      it "returns False for rule not in pack" $ do
        registry <- newPackRegistry
        result <- isRuleInPack registry "nonexistent-rule" "security"
        result `shouldBe` False

    describe "getRulesByPack" $ do
      it "returns rules for valid pack" $ do
        registry <- newPackRegistry
        rules <- getRulesByPack registry "security"
        length rules `shouldSatisfy` (> 0)

      it "returns empty for unknown pack" $ do
        registry <- newPackRegistry
        rules <- getRulesByPack registry "nonexistent"
        length rules `shouldBe` 0

    describe "getPacksByRule" $ do
      it "finds packs containing rule" $ do
        registry <- newPackRegistry
        packs <- getPacksByRule registry "avoid-unsafe-perform-io"
        "security" `elem` packs `shouldBe` True

      it "returns empty for unknown rule" $ do
        registry <- newPackRegistry
        packs <- getPacksByRule registry "nonexistent-rule-xyz"
        length packs `shouldBe` 0

    describe "getPackInfo" $ do
      it "returns pack info for valid pack" $ do
        registry <- newPackRegistry
        mInfo <- getPackInfo registry "security"
        case mInfo of
          Nothing -> expectationFailure "Expected Just"
          Just (pack, count) -> do
            rpName pack `shouldBe` "security"
            count `shouldSatisfy` (> 0)

      it "returns Nothing for unknown pack" $ do
        registry <- newPackRegistry
        mInfo <- getPackInfo registry "nonexistent"
        mInfo `shouldBe` Nothing

    describe "PackProfile" $ do
      it "defaultProfile has expected settings" $ do
        ppName defaultProfile `shouldBe` "default"
        ppStrict defaultProfile `shouldBe` False
        ppAutoFix defaultProfile `shouldBe` False

      it "strictProfile enables strict mode" $ do
        ppName strictProfile `shouldBe` "strict"
        ppStrict strictProfile `shouldBe` True

      it "ciProfile is optimized for CI" $ do
        ppName ciProfile `shouldBe` "ci"
        ppStrict ciProfile `shouldBe` True
        "style" `elem` pcDisabledPacks (ppConfig ciProfile) `shouldBe` True

    describe "applyProfile" $ do
      it "extracts config from profile" $ do
        let config = applyProfile defaultProfile
        config `shouldBe` ppConfig defaultProfile

    describe "PackResolution" $ do
      it "tracks rule sources" $ do
        registry <- newPackRegistry
        let config = defaultPackConfig { pcEnabledPacks = ["security"] }
        resolution <- resolvePacks registry config
        -- Should track which pack rules came from
        -- (at least some rules should have source info)
        length (prActivePacks resolution) `shouldSatisfy` (> 0)

    describe "edge cases" $ do
      it "handles pcDefaultEnabled" $ do
        registry <- newPackRegistry
        let config = defaultPackConfig
              { pcDefaultEnabled = True
              , pcDisabledPacks = ["style"]  -- Disable one pack
              }
        rules <- getPackRules registry config
        -- Should have rules from most packs
        Set.size rules `shouldSatisfy` (> 0)

      it "handles custom packs in config" $ do
        registry <- newPackRegistry
        let customPack = RulePack
              { rpName = "inline-custom"
              , rpDescription = "Inline custom pack"
              , rpRuleIds = ["custom-rule-1", "custom-rule-2"]
              , rpIncludes = []
              , rpExcludes = []
              , rpPriority = 50
              , rpCategory = "custom"
              , rpTags = []
              , rpEnabled = True
              }
            config = defaultPackConfig
              { pcEnabledPacks = ["inline-custom"]
              , pcCustomPacks = [customPack]
              }
        rules <- getPackRules registry config
        "custom-rule-1" `Set.member` rules `shouldBe` True
        "custom-rule-2" `Set.member` rules `shouldBe` True

      it "handles circular includes gracefully" $ do
        registry <- newPackRegistry
        let pack1 = RulePack
              { rpName = "circular-1"
              , rpDescription = "Circular 1"
              , rpRuleIds = ["rule-1"]
              , rpIncludes = ["circular-2"]
              , rpExcludes = []
              , rpPriority = 50
              , rpCategory = "test"
              , rpTags = []
              , rpEnabled = True
              }
            pack2 = RulePack
              { rpName = "circular-2"
              , rpDescription = "Circular 2"
              , rpRuleIds = ["rule-2"]
              , rpIncludes = ["circular-1"]
              , rpExcludes = []
              , rpPriority = 50
              , rpCategory = "test"
              , rpTags = []
              , rpEnabled = True
              }
        registerPack registry pack1
        registerPack registry pack2
        let config = defaultPackConfig { pcEnabledPacks = ["circular-1"] }
        resolution <- resolvePacks registry config
        -- Should detect circular dependency and warn
        any (T.isInfixOf "Circular") (prWarnings resolution) `shouldBe` True
        -- Should still include available rules
        "rule-1" `Set.member` prEnabledRules resolution `shouldBe` True
