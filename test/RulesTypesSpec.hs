{-# LANGUAGE OverloadedStrings #-}

module RulesTypesSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Argus.Rules.Types
import Argus.Types (Severity(..))

spec :: Spec
spec = do
  describe "Argus.Rules.Types" $ do
    describe "Category" $ do
      it "has all expected categories" $ do
        length allCategories `shouldSatisfy` (>= 15)

      it "converts to text correctly" $ do
        categoryToText Performance `shouldBe` "performance"
        categoryToText SpaceLeaks `shouldBe` "space-leaks"
        categoryToText Security `shouldBe` "security"
        categoryToText Safety `shouldBe` "safety"

      it "parses from text correctly" $ do
        textToCategory "performance" `shouldBe` Performance
        textToCategory "space-leaks" `shouldBe` SpaceLeaks
        textToCategory "spaceleaks" `shouldBe` SpaceLeaks
        textToCategory "SECURITY" `shouldBe` Security

      it "handles legacy aliases" $ do
        textToCategory "partial" `shouldBe` Safety
        textToCategory "modernize" `shouldBe` Modernization

      it "creates custom category for unknown text" $ do
        textToCategory "my-custom" `shouldBe` Custom "my-custom"

      it "supports equality" $ do
        Performance == Performance `shouldBe` True
        Performance == Security `shouldBe` False

      it "supports ordering" $ do
        Performance < Security `shouldBe` True

    describe "SafetyLevel" $ do
      it "converts to text correctly" $ do
        safetyToText Safe `shouldBe` "safe"
        safetyToText MostlySafe `shouldBe` "mostly-safe"
        safetyToText NeedsReview `shouldBe` "needs-review"
        safetyToText Unsafe `shouldBe` "unsafe"

      it "parses from text correctly" $ do
        textToSafety "safe" `shouldBe` Safe
        textToSafety "mostly-safe" `shouldBe` MostlySafe
        textToSafety "needs-review" `shouldBe` NeedsReview
        textToSafety "unsafe" `shouldBe` Unsafe

      it "handles legacy aliases" $ do
        textToSafety "always" `shouldBe` Safe
        textToSafety "mostly" `shouldBe` MostlySafe
        textToSafety "review" `shouldBe` NeedsReview
        textToSafety "manual" `shouldBe` NeedsReview

      it "defaults to Safe for unknown values" $ do
        textToSafety "unknown" `shouldBe` Safe

      it "supports enum operations" $ do
        succ Safe `shouldBe` MostlySafe
        pred Unsafe `shouldBe` NeedsReview
        minBound `shouldBe` Safe
        maxBound `shouldBe` Unsafe

    describe "PatternVar" $ do
      it "stores name and constraint" $ do
        let pv = PatternVar "x" (Just "Int")
        pvName pv `shouldBe` "x"
        pvConstraint pv `shouldBe` Just "Int"

      it "allows Nothing constraint" $ do
        let pv = PatternVar "xs" Nothing
        pvConstraint pv `shouldBe` Nothing

    describe "PatternAST" $ do
      it "has PWildcard constructor" $ do
        PWildcard `shouldSatisfy` const True

      it "has PVar constructor" $ do
        let pat = PVar (PatternVar "x" Nothing)
        pat `shouldSatisfy` const True

      it "has PLiteral constructor" $ do
        let pat = PLiteral "42"
        pat `shouldSatisfy` const True

      it "has PConstructor constructor" $ do
        let pat = PConstructor "Just" [PVar (PatternVar "x" Nothing)]
        pat `shouldSatisfy` const True

      it "has PApplication constructor" $ do
        let pat = PApplication (PLiteral "f") (PLiteral "x")
        pat `shouldSatisfy` const True

      it "has PInfix constructor" $ do
        let pat = PInfix (PLiteral "a") "+" (PLiteral "b")
        pat `shouldSatisfy` const True

      it "has PTuple constructor" $ do
        let pat = PTuple [PLiteral "a", PLiteral "b"]
        pat `shouldSatisfy` const True

      it "has PList constructor" $ do
        let pat = PList [PLiteral "1", PLiteral "2"]
        pat `shouldSatisfy` const True

      it "has PParens constructor" $ do
        let pat = PParens (PLiteral "x")
        pat `shouldSatisfy` const True

      it "has PTyped constructor" $ do
        let pat = PTyped (PLiteral "x") (TPVar "Int")
        pat `shouldSatisfy` const True

      it "supports equality" $ do
        PWildcard == PWildcard `shouldBe` True
        PWildcard == PLiteral "_" `shouldBe` False

    describe "TypePattern" $ do
      it "has TPWildcard constructor" $ do
        TPWildcard `shouldSatisfy` const True

      it "has TPVar constructor" $ do
        TPVar "a" `shouldSatisfy` const True

      it "has TPConstructor constructor" $ do
        let tp = TPConstructor "Maybe" [TPVar "a"]
        tp `shouldSatisfy` const True

      it "has TPFunction constructor" $ do
        let tp = TPFunction (TPVar "a") (TPVar "b")
        tp `shouldSatisfy` const True

      it "supports equality" $ do
        TPWildcard == TPWildcard `shouldBe` True
        TPVar "a" == TPVar "b" `shouldBe` False

    describe "Binding" $ do
      it "stores value correctly" $ do
        let binding = Binding "42" Nothing
        bindingValue binding `shouldBe` "42"

      it "stores span correctly" $ do
        let binding = Binding "foo" (Just (1, 10))
        bindingSpan binding `shouldBe` Just (1, 10)

      it "can be created with mkBinding" $ do
        let binding = mkBinding "value"
        bindingValue binding `shouldBe` "value"
        bindingSpan binding `shouldBe` Nothing

      it "can be created with mkBindingWithSpan" $ do
        let binding = mkBindingWithSpan "value" 5 15
        bindingValue binding `shouldBe` "value"
        bindingSpan binding `shouldBe` Just (5, 15)

    describe "Bindings" $ do
      it "starts empty" $ do
        Map.null emptyBindings `shouldBe` True

      it "supports addBinding'" $ do
        let bindings = addBinding' "x" "42" emptyBindings
        lookupBinding "x" bindings `shouldBe` Just "42"

      it "supports lookupBinding" $ do
        let bindings = addBinding' "y" "hello" emptyBindings
        lookupBinding "y" bindings `shouldBe` Just "hello"
        lookupBinding "z" bindings `shouldBe` Nothing

    describe "RulePattern" $ do
      it "has TextPatternSpec constructor" $ do
        let pat = TextPatternSpec "head $X"
        rulePatternToText pat `shouldBe` "head $X"

      it "has RegexPatternSpec constructor" $ do
        let pat = RegexPatternSpec "head.*"
        rulePatternToText pat `shouldBe` "head.*"

      it "has ASTPatternSpec constructor" $ do
        let pat = ASTPatternSpec "head xs"
        rulePatternToText pat `shouldBe` "head xs"

    describe "ImportSpec" $ do
      it "can be created with mkImportSpec" $ do
        let spec = mkImportSpec "Data.List" ["foldl'", "sort"]
        impModule spec `shouldBe` "Data.List"
        length (impSymbols spec) `shouldBe` 2

      it "defaults to not qualified" $ do
        let spec = mkImportSpec "Data.Map" []
        impQualified spec `shouldBe` Nothing

      it "defaults to not hiding" $ do
        let spec = mkImportSpec "Data.Map" []
        impHiding spec `shouldBe` False

    describe "ImportSymbol" $ do
      it "can be created with mkImportSymbol" $ do
        let sym = mkImportSymbol "foldl'" SymFunction
        symName sym `shouldBe` "foldl'"
        symType sym `shouldBe` SymFunction
        symChildren sym `shouldBe` []

    describe "SideCondition" $ do
      it "has location predicates" $ do
        NotInComment `shouldSatisfy` const True
        NotInString `shouldSatisfy` const True
        NotInImport `shouldSatisfy` const True
        InFunctionBody `shouldSatisfy` const True

      it "has type predicates" $ do
        HasType "$X" "Int" `shouldSatisfy` const True
        HasTypeClass "$X" "Ord" `shouldSatisfy` const True
        IsNumeric "$X" `shouldSatisfy` const True

      it "has expression predicates" $ do
        IsLiteral "$X" `shouldSatisfy` const True
        IsVariable "$X" `shouldSatisfy` const True
        NotEqual "$X" "$Y" `shouldSatisfy` const True

      it "has combinators" $ do
        And [NotInComment, NotInString] `shouldSatisfy` const True
        Or [IsLiteral "$X", IsVariable "$X"] `shouldSatisfy` const True
        Not NotInComment `shouldSatisfy` const True
        Always `shouldSatisfy` const True
        Never `shouldSatisfy` const True

    describe "Rule" $ do
      it "has all required fields" $ do
        let rule = defaultRule
              { ruleId = "test/rule"
              , ruleCategory = Performance
              , ruleSeverity = Warning
              , ruleMessage = "Test message"
              , rulePattern = TextPatternSpec "head xs"
              }
        ruleId rule `shouldBe` "test/rule"
        ruleCategory rule `shouldBe` Performance
        ruleSeverity rule `shouldBe` Warning
        ruleMessage rule `shouldBe` "Test message"

      it "has sensible defaults" $ do
        ruleCategory defaultRule `shouldBe` Style
        ruleSeverity defaultRule `shouldBe` Warning
        ruleSafety defaultRule `shouldBe` Safe
        ruleEnabled defaultRule `shouldBe` True
        ruleConditions defaultRule `shouldBe` []
        ruleAddImports defaultRule `shouldBe` []
        ruleRemoveImports defaultRule `shouldBe` []

      it "allows optional fields" $ do
        let rule = defaultRule
              { ruleExplanation = Just "Detailed explanation"
              , ruleReplacement = Just "headMay xs"
              , ruleNote = Just "A note"
              }
        ruleExplanation rule `shouldBe` Just "Detailed explanation"
        ruleReplacement rule `shouldBe` Just "headMay xs"
        ruleNote rule `shouldBe` Just "A note"

      it "allows disabled rules" $ do
        let rule = defaultRule { ruleEnabled = False }
        ruleEnabled rule `shouldBe` False

      it "supports scoping with within/except" $ do
        let rule = defaultRule
              { ruleWithin = ["src/*"]
              , ruleExcept = ["*Test*", "*Spec*"]
              }
        ruleWithin rule `shouldBe` ["src/*"]
        ruleExcept rule `shouldBe` ["*Test*", "*Spec*"]

      it "supports tags and references" $ do
        let rule = defaultRule
              { ruleTags = ["performance", "list"]
              , ruleReferences = ["https://wiki.haskell.org/Performance"]
              }
        ruleTags rule `shouldBe` ["performance", "list"]
        length (ruleReferences rule) `shouldBe` 1

      it "supports deprecation" $ do
        let rule = defaultRule { ruleDeprecated = Just "Use new-rule instead" }
        ruleDeprecated rule `shouldBe` Just "Use new-rule instead"

    describe "Conversion functions" $ do
      it "converts categories round-trip" $ do
        fixCategoryToCategory (categoryToFixCategory Performance) `shouldBe` Performance
        fixCategoryToCategory (categoryToFixCategory Security) `shouldBe` Security

      it "converts safety levels round-trip" $ do
        fixSafetyToSafety (safetyToFixSafety Safe) `shouldBe` Safe
        fixSafetyToSafety (safetyToFixSafety Unsafe) `shouldBe` Unsafe

      it "converts import specs" $ do
        let spec = mkImportSpec "Data.List" ["head"]
            converted = fixImportToImportSpec (importSpecToFixImport spec)
        impModule converted `shouldBe` "Data.List"
