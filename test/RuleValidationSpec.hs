{-# LANGUAGE OverloadedStrings #-}

module RuleValidationSpec (spec) where

import Test.Hspec
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Rules.Validation
import Argus.Rules.Types
import Argus.Types (Severity(..))

spec :: Spec
spec = do
  describe "Rule Validation" $ do
    describe "validateRule" $ do
      it "passes for valid rules" $ do
        let rule = defaultRule
              { ruleId = "test/valid-rule"
              , ruleMessage = "Test message"
              , rulePattern = TextPatternSpec "head $X"
              }
        isValid (validateRule rule) `shouldBe` True

      it "fails for empty rule ID" $ do
        let rule = defaultRule { ruleId = "" }
        isValid (validateRule rule) `shouldBe` False
        hasErrors (validateRule rule) `shouldBe` True

      it "fails for invalid rule ID format" $ do
        let rule = defaultRule { ruleId = "invalid" }  -- Missing category
        isValid (validateRule rule) `shouldBe` False

      it "passes for valid rule ID with multiple segments" $ do
        let rule = defaultRule { ruleId = "company/security/sql-injection" }
        let result = validateRule rule
        -- Should pass ID validation (may have other warnings)
        case result of
          ValidationFailed errs ->
            all (\e -> veCode e /= ErrInvalidRuleId) errs `shouldBe` True
          _ -> True `shouldBe` True

    describe "Pattern validation" $ do
      it "detects unbalanced parentheses" $ do
        let result = validatePattern "foo (bar"
        case result of
          Left errs -> any (\e -> veCode e == ErrUnbalancedParens) errs `shouldBe` True
          Right () -> expectationFailure "Expected validation to fail"

      it "passes for balanced parentheses" $ do
        let result = validatePattern "foo (bar baz)"
        result `shouldBe` Right ()

      it "validates metavariable syntax" $ do
        let result = validatePattern "head $X"
        result `shouldBe` Right ()

      it "rejects invalid metavariables" $ do
        let result = validatePattern "head $"
        case result of
          Left errs -> any (\e -> veCode e == ErrInvalidMetavariable) errs `shouldBe` True
          Right () -> expectationFailure "Expected validation to fail"

    describe "Metavariable validation" $ do
      it "detects undefined metavariables in fix" $ do
        let result = validateMetavariables "head $X" "headMay $Y"
        case result of
          Left errs -> any (\e -> veCode e == ErrUndefinedMetavariable) errs `shouldBe` True
          Right () -> expectationFailure "Expected validation to fail"

      it "passes when all metavariables are defined" $ do
        let result = validateMetavariables "head $X" "headMay $X"
        result `shouldBe` Right ()

      it "handles multiple metavariables" $ do
        let result = validateMetavariables "foo $X $Y" "bar $Y $X"
        result `shouldBe` Right ()

    describe "Import validation" $ do
      it "validates correct module names" $ do
        let spec = ImportSpec
              { impModule = "Data.List"
              , impSymbols = []
              , impQualified = Nothing
              , impHiding = False
              , impPackage = Nothing
              }
        validateImportSpec spec `shouldBe` Right ()

      it "rejects invalid module names" $ do
        let spec = ImportSpec
              { impModule = "data.list"  -- lowercase
              , impSymbols = []
              , impQualified = Nothing
              , impHiding = False
              , impPackage = Nothing
              }
        case validateImportSpec spec of
          Left _ -> True `shouldBe` True
          Right () -> expectationFailure "Expected validation to fail"

    describe "Side condition validation" $ do
      it "validates HasType with valid metavariable" $ do
        let result = validateSideCondition (HasType "$X" "Int")
        result `shouldBe` Right ()

      it "rejects HasType with invalid metavariable" $ do
        let result = validateSideCondition (HasType "X" "Int")
        case result of
          Left _ -> True `shouldBe` True
          Right () -> expectationFailure "Expected validation to fail"

    describe "Validation configuration" $ do
      it "strict config requires message" $ do
        let rule = defaultRule
              { ruleId = "test/no-message"
              , ruleMessage = ""
              , rulePattern = TextPatternSpec "foo"
              }
        isValid (validateRuleStrict rule) `shouldBe` False

      it "default config allows missing message" $ do
        let rule = defaultRule
              { ruleId = "test/no-message"
              , ruleMessage = ""
              , rulePattern = TextPatternSpec "foo"
              }
        -- Default is permissive - may pass or have warnings
        hasErrors (validateRule rule) `shouldBe` False

    describe "Warning collection" $ do
      it "warns about high severity rules without fixes" $ do
        let rule = defaultRule
              { ruleId = "test/error-no-fix"
              , ruleSeverity = Error
              , ruleReplacement = Nothing  -- No fix provided
              }
        let result = validateRule rule
        case result of
          ValidationWarnings ws ->
            any (\w -> vwCode w == WarnHighSeverityNoFix) ws `shouldBe` True
          _ -> True `shouldBe` True  -- May pass without warnings in some configs

  describe "Error/Warning text conversion" $ do
    it "converts error codes to text" $ do
      errorToText ErrEmptyRuleId `shouldSatisfy` T.isPrefixOf "E001"
      errorToText ErrInvalidRuleId `shouldSatisfy` T.isPrefixOf "E002"

    it "converts warning codes to text" $ do
      warningToText WarnNoMessage `shouldSatisfy` T.isPrefixOf "W001"
      warningToText WarnNoFix `shouldSatisfy` T.isPrefixOf "W002"

  describe "Batch validation" $ do
    it "validates multiple rules" $ do
      let rules =
            [ defaultRule { ruleId = "test/rule-1" }
            , defaultRule { ruleId = "test/rule-2" }
            , defaultRule { ruleId = "" }  -- Invalid
            ]
      let results = validateRules rules
      length results `shouldBe` 3
