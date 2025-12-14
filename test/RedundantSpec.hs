{-# LANGUAGE OverloadedStrings #-}

module RedundantSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Argus.Rules.Redundant
import Argus.Types (Diagnostic(..), Severity(..))

spec :: Spec
spec = do
  describe "Argus.Rules.Redundant" $ do
    describe "defaultRedundantConfig" $ do
      it "is enabled by default" $ do
        redEnabled defaultRedundantConfig `shouldBe` True

      it "has boolean checks enabled" $ do
        redCheckBoolean defaultRedundantConfig `shouldBe` True

      it "has redundant construct checks enabled" $ do
        redCheckRedundant defaultRedundantConfig `shouldBe` True

      it "has eta reduction checks disabled by default" $ do
        redCheckEta defaultRedundantConfig `shouldBe` False

      it "has control flow checks enabled" $ do
        redCheckControlFlow defaultRedundantConfig `shouldBe` True

      it "has identity operation checks enabled" $ do
        redCheckIdentity defaultRedundantConfig `shouldBe` True

    describe "detectRedundant" $ do
      describe "when disabled" $ do
        it "returns empty list" $ do
          let config = defaultRedundantConfig { redEnabled = False }
              code = "result = if x then True else False"
          detectRedundant config "test.hs" code `shouldBe` []

      describe "boolean simplifications" $ do
        it "detects 'if x then True else False'" $ do
          let code = "result = if condition then True else False"
              diags = detectRedundant defaultRedundantConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "True else False" . diagMessage) diags `shouldBe` True

        it "detects 'if x then False else True'" $ do
          let code = "result = if condition then False else True"
              diags = detectRedundant defaultRedundantConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects 'not (not x)'" $ do
          let code = "result = not (not value)"
              diags = detectRedundant defaultRedundantConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "not" . diagMessage) diags `shouldBe` True

        it "detects 'x == True'" $ do
          let code = "result = condition == True"
              diags = detectRedundant defaultRedundantConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects 'x == False'" $ do
          let code = "result = condition == False"
              diags = detectRedundant defaultRedundantConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "respects redCheckBoolean = False" $ do
          let config = defaultRedundantConfig { redCheckBoolean = False }
              code = "result = if condition then True else False"
          detectRedundant config "test.hs" code `shouldBe` []

      describe "redundant constructs" $ do
        -- Text-based detection patterns for redundant constructs
        it "detects 'maybe Nothing Just x'" $ do
          let code = "result = maybe Nothing Just maybeVal"
              diags = detectRedundant defaultRedundantConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects 'either Left Right x'" $ do
          let code = "result = either Left Right eitherVal"
              diags = detectRedundant defaultRedundantConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "respects redCheckRedundant = False" $ do
          let config = defaultRedundantConfig { redCheckRedundant = False }
              code = "result = maybe Nothing Just maybeVal"
          detectRedundant config "test.hs" code `shouldBe` []

      describe "identity operations" $ do
        it "detects 'map id x'" $ do
          let code = "result = map id xs"
              diags = detectRedundant defaultRedundantConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects 'fmap id x'" $ do
          let code = "result = fmap id xs"
              diags = detectRedundant defaultRedundantConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects 'filter (const True) x'" $ do
          let code = "result = filter (const True) xs"
              diags = detectRedundant defaultRedundantConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects 'reverse . reverse'" $ do
          let code = "result = reverse . reverse"
              diags = detectRedundant defaultRedundantConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects 'reverse $ reverse x'" $ do
          let code = "result = reverse $ reverse xs"
              diags = detectRedundant defaultRedundantConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "respects redCheckIdentity = False" $ do
          let config = defaultRedundantConfig { redCheckIdentity = False }
              code = "result = map id xs"
          detectRedundant config "test.hs" code `shouldBe` []

      describe "control flow simplifications" $ do
        it "detects 'if c then action else pure ()'" $ do
          let code = "foo = if condition then doAction else pure ()"
              diags = detectRedundant defaultRedundantConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects 'if c then action else return ()'" $ do
          let code = "foo = if condition then doAction else return ()"
              diags = detectRedundant defaultRedundantConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "respects redCheckControlFlow = False" $ do
          let config = defaultRedundantConfig { redCheckControlFlow = False }
              code = "foo = if condition then doAction else pure ()"
          detectRedundant config "test.hs" code `shouldBe` []

    describe "severity levels" $ do
      it "assigns Warning severity to boolean simplifications" $ do
        let code = "result = if condition then True else False"
            diags = detectRedundant defaultRedundantConfig "test.hs" code
        any (\d -> diagSeverity d == Warning) diags `shouldBe` True

    describe "diagnostic codes" $ do
      it "uses redundant/ prefix for diagnostic codes" $ do
        let code = "result = if condition then True else False"
            diags = detectRedundant defaultRedundantConfig "test.hs" code
        all (maybe False (T.isPrefixOf "redundant/") . diagCode) diags `shouldBe` True

    describe "RedundantCategory" $ do
      it "has all expected categories" $ do
        minBound `shouldBe` BooleanSimplification
        maxBound `shouldBe` IdentityOperation

    describe "AST-based rules" $ do
      it "loadRedundantRules returns rules" $ do
        rules <- loadRedundantRules
        length rules `shouldSatisfy` (> 20)

      it "redundantASTRules respects configuration" $ do
        let disabledConfig = defaultRedundantConfig { redEnabled = False }
        rules <- redundantASTRules disabledConfig
        length rules `shouldBe` 0

      it "redundantASTRules filters by category" $ do
        let boolOnlyConfig = defaultRedundantConfig
              { redCheckRedundant = False
              , redCheckControlFlow = False
              , redCheckIdentity = False
              , redCheckEta = False
              }
        rules <- redundantASTRules boolOnlyConfig
        -- Should only have boolean rules
        length rules `shouldSatisfy` (> 0)
        length rules `shouldSatisfy` (< 15)  -- Less than all rules

    describe "rule definitions" $ do
      it "has boolean simplification rules" $ do
        rules <- loadRedundantRules
        let boolRules = filter (\r -> "if-true-false" `T.isInfixOf` T.pack (show r)
                                   || "double-not" `T.isInfixOf` T.pack (show r)
                                   || "eq-true" `T.isInfixOf` T.pack (show r))
                               rules
        length boolRules `shouldSatisfy` (> 0)

      it "has identity operation rules" $ do
        rules <- loadRedundantRules
        let idRules = filter (\r -> "map-id" `T.isInfixOf` T.pack (show r)
                                 || "fmap-id" `T.isInfixOf` T.pack (show r)
                                 || "reverse" `T.isInfixOf` T.pack (show r))
                             rules
        length idRules `shouldSatisfy` (> 0)

      it "has control flow rules" $ do
        rules <- loadRedundantRules
        let ctrlRules = filter (\r -> "if-pure" `T.isInfixOf` T.pack (show r)
                                   || "if-return" `T.isInfixOf` T.pack (show r))
                               rules
        length ctrlRules `shouldSatisfy` (> 0)

      it "has redundant construct rules" $ do
        rules <- loadRedundantRules
        let redRules = filter (\r -> "maybe-nothing" `T.isInfixOf` T.pack (show r)
                                  || "either-left" `T.isInfixOf` T.pack (show r)
                                  || "id-apply" `T.isInfixOf` T.pack (show r))
                              rules
        length redRules `shouldSatisfy` (> 0)
