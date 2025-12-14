{-# LANGUAGE OverloadedStrings #-}

module DSLSpec (spec) where

import Test.Hspec

import Argus.Config (PatternRule(..))
import Argus.Rules.DSL
import Argus.Rules.Types (RulePattern(..))  -- Unified pattern type
import Argus.Types (Severity(..))

spec :: Spec
spec = do
  describe "Argus.Rules.DSL" $ do
    describe "Rule construction" $ do
      describe "rule" $ do
        it "creates a rule with given name" $ do
          let r = rule "test-rule" $ match (pat "foo")
          ruleId r `shouldBe` "test-rule"

        it "uses default severity of Warning" $ do
          let r = rule "test-rule" $ match (pat "foo")
          ruleSeverity r `shouldBe` Warning

        it "uses default category of Style" $ do
          let r = rule "test-rule" $ match (pat "foo")
          ruleCategory r `shouldBe` Style

        it "is enabled by default" $ do
          let r = rule "test-rule" $ match (pat "foo")
          ruleEnabled r `shouldBe` True

        it "has no replacement by default" $ do
          let r = rule "test-rule" $ match (pat "foo")
          ruleReplacement r `shouldBe` Nothing

      describe "match and ==>" $ do
        it "creates pattern from text" $ do
          let r = rule "test" $ match (pat "head xs")
          rulePattern r `shouldBe` TextPatternSpec "head xs"

        it "sets replacement pattern" $ do
          let r = rule "test" $ match ("foo" ==> "bar")
          -- Replacement is now just Text in the unified type
          ruleReplacement r `shouldBe` Just "bar"

    describe "Rule modifiers" $ do
      describe "severity" $ do
        it "sets Error severity" $ do
          let r = rule "test" $ match (pat "foo") & severity Error
          ruleSeverity r `shouldBe` Error

        it "sets Warning severity" $ do
          let r = rule "test" $ match (pat "foo") & severity Warning
          ruleSeverity r `shouldBe` Warning

        it "sets Suggestion severity" $ do
          let r = rule "test" $ match (pat "foo") & severity Suggestion
          ruleSeverity r `shouldBe` Suggestion

        it "sets Info severity" $ do
          let r = rule "test" $ match (pat "foo") & severity Info
          ruleSeverity r `shouldBe` Info

      describe "message" $ do
        it "sets custom message" $ do
          let r = rule "test" $ match (pat "foo") & message "Custom message"
          ruleMessage r `shouldBe` "Custom message"

      describe "note" $ do
        it "adds note to rule" $ do
          let r = rule "test" $ match (pat "foo") & note "Extra explanation"
          ruleNote r `shouldBe` Just "Extra explanation"

      describe "category" $ do
        it "sets Performance category" $ do
          let r = rule "test" $ match (pat "foo") & category Performance
          ruleCategory r `shouldBe` Performance

        it "sets Security category" $ do
          let r = rule "test" $ match (pat "foo") & category Security
          ruleCategory r `shouldBe` Security

        it "sets Safety category" $ do
          let r = rule "test" $ match (pat "foo") & category Safety
          ruleCategory r `shouldBe` Safety

        it "sets Custom category" $ do
          let r = rule "test" $ match (pat "foo") & category (Custom "MyCategory")
          ruleCategory r `shouldBe` Custom "MyCategory"

      describe "fixDescription" $ do
        it "sets fix description" $ do
          let r = rule "test" $ match ("foo" ==> "bar") & fixDescription "Replace foo with bar"
          ruleFixDescription r `shouldBe` Just "Replace foo with bar"

      describe "safetyLevel" $ do
        it "sets Safe level" $ do
          let r = rule "test" $ match (pat "foo") & safetyLevel Safe
          ruleSafety r `shouldBe` Safe

        it "sets MostlySafe level" $ do
          let r = rule "test" $ match (pat "foo") & safetyLevel MostlySafe
          ruleSafety r `shouldBe` MostlySafe

        it "sets Unsafe level" $ do
          let r = rule "test" $ match (pat "foo") & safetyLevel Unsafe
          ruleSafety r `shouldBe` Unsafe

        it "sets ManualReview level" $ do
          let r = rule "test" $ match (pat "foo") & safetyLevel ManualReview
          ruleSafety r `shouldBe` ManualReview

      describe "disabled" $ do
        it "disables the rule" $ do
          let r = rule "test" $ match (pat "foo") & disabled
          ruleEnabled r `shouldBe` False

      describe "deprecated" $ do
        it "marks rule as deprecated" $ do
          let r = rule "test" $ match (pat "foo") & deprecated "Use newer pattern"
          ruleDeprecated r `shouldBe` Just "Use newer pattern"

    describe "Modifier chaining" $ do
      it "chains multiple modifiers" $ do
        let r = rule "test" $
                  match ("foldl" ==> "foldl'")
                  & severity Suggestion
                  & message "Use strict foldl'"
                  & category Performance
                  & safetyLevel Safe
        ruleSeverity r `shouldBe` Suggestion
        ruleMessage r `shouldBe` "Use strict foldl'"
        ruleCategory r `shouldBe` Performance
        ruleSafety r `shouldBe` Safe

    describe "Pattern constructors" $ do
      describe "var" $ do
        it "creates variable pattern" $ do
          var "x" `shouldBe` PVar "x"

      describe "lit" $ do
        it "creates literal pattern" $ do
          lit "42" `shouldBe` PLit "42"

      describe "app" $ do
        it "creates application pattern" $ do
          let p = app (var "f") (var "x")
          p `shouldBe` PApp (PVar "f") (PVar "x")

      describe "op" $ do
        it "creates operator pattern" $ do
          let p = op "+" (var "x") (var "y")
          p `shouldBe` POp "+" (PVar "x") (PVar "y")

      describe "wildcard" $ do
        it "creates wildcard pattern" $ do
          wildcard `shouldBe` PWildcard

      describe "list" $ do
        it "creates list pattern" $ do
          let p = list [var "x", var "y"]
          p `shouldBe` PList [PVar "x", PVar "y"]

        it "handles empty list" $ do
          list [] `shouldBe` PList []

      describe "tuple" $ do
        it "creates tuple pattern" $ do
          let p = tuple [var "a", var "b"]
          p `shouldBe` PTuple [PVar "a", PVar "b"]

      describe "guard" $ do
        it "creates guarded pattern" $ do
          let p = guard (var "x") (lit "True")
          p `shouldBe` PGuard (PVar "x") (PLit "True")

    describe "Rule compilation" $ do
      describe "ruleToPatternRule" $ do
        it "converts rule to PatternRule" $ do
          let r = rule "test" $ match ("head" ==> "headMay") & severity Warning
              pr = ruleToPatternRule r
          prName pr `shouldBe` "test"
          prMatch pr `shouldBe` "head"
          prFix pr `shouldBe` Just "headMay"

      describe "compileRules" $ do
        it "filters disabled rules" $ do
          let rules = [ rule "enabled" $ match (pat "foo")
                      , rule "disabled" $ match (pat "bar") & disabled
                      ]
              compiled = compileRules rules
          length compiled `shouldBe` 1

        it "compiles all enabled rules" $ do
          let rules = [ rule "r1" $ match (pat "a")
                      , rule "r2" $ match (pat "b")
                      , rule "r3" $ match (pat "c")
                      ]
              compiled = compileRules rules
          length compiled `shouldBe` 3

    describe "Built-in rule sets" $ do
      describe "defaultRuleSet" $ do
        it "contains multiple rules" $ do
          length defaultRuleSet `shouldSatisfy` (> 0)

        it "includes avoid-head rule" $ do
          any (\r -> ruleId r == "avoid-head") defaultRuleSet `shouldBe` True

        it "includes prefer-pure rule" $ do
          any (\r -> ruleId r == "prefer-pure") defaultRuleSet `shouldBe` True

      describe "strictRuleSet" $ do
        it "extends default rules" $ do
          length strictRuleSet `shouldSatisfy` (> length defaultRuleSet)

        it "includes avoid-error rule" $ do
          any (\r -> ruleId r == "avoid-error") strictRuleSet `shouldBe` True

        it "includes avoid-undefined rule" $ do
          any (\r -> ruleId r == "avoid-undefined") strictRuleSet `shouldBe` True

      describe "performanceRuleSet" $ do
        it "contains performance rules" $ do
          length performanceRuleSet `shouldSatisfy` (> 0)

        it "includes prefer-foldl' rule" $ do
          any (\r -> ruleId r == "prefer-foldl'") performanceRuleSet `shouldBe` True

        it "all rules are Performance category" $ do
          all (\r -> ruleCategory r == Performance) performanceRuleSet `shouldBe` True

      describe "securityRuleSet" $ do
        it "contains security rules" $ do
          length securityRuleSet `shouldSatisfy` (> 0)

        it "includes avoid-unsafeCoerce rule" $ do
          any (\r -> ruleId r == "avoid-unsafeCoerce") securityRuleSet `shouldBe` True

        it "all rules are Security category" $ do
          all (\r -> ruleCategory r == Security) securityRuleSet `shouldBe` True

    describe "SafetyLevel" $ do
      it "has correct ordering" $ do
        -- Unified SafetyLevel ordering: Safe < MostlySafe < NeedsReview < Unsafe
        Safe < MostlySafe `shouldBe` True
        MostlySafe < NeedsReview `shouldBe` True
        NeedsReview < Unsafe `shouldBe` True

      it "Safe is minimum" $ do
        minBound `shouldBe` Safe

      it "Unsafe is maximum" $ do
        -- In unified type, Unsafe is the most dangerous level
        maxBound `shouldBe` Unsafe

      it "ManualReview equals NeedsReview" $ do
        -- ManualReview is a pattern synonym for backwards compatibility
        ManualReview `shouldBe` NeedsReview

    describe "Category" $ do
      it "supports all built-in categories" $ do
        Style `shouldBe` Style
        Performance `shouldBe` Performance
        Safety `shouldBe` Safety
        Security `shouldBe` Security
        Correctness `shouldBe` Correctness
        Modernization `shouldBe` Modernization
        Naming `shouldBe` Naming
        Imports `shouldBe` Imports

      it "supports custom categories" $ do
        Custom "MyCustom" `shouldBe` Custom "MyCustom"
        Custom "A" `shouldNotBe` Custom "B"

    describe "MatchExpr" $ do
      describe "pat" $ do
        it "creates a match expression without replacement" $ do
          let me = pat "head"
          mePattern me `shouldBe` "head"
          meReplacement me `shouldBe` Nothing
          meConditions me `shouldBe` []

      describe "==>" $ do
        it "creates a match expression with replacement" $ do
          let me = "head" ==> "headMay"
          mePattern me `shouldBe` "head"
          meReplacement me `shouldBe` Just "headMay"
          meConditions me `shouldBe` []

      describe "where_" $ do
        it "adds a condition to MatchExpr" $ do
          let me = "nub" ==> "ordNub" `where_` hasClass "$X" "Ord"
          meConditions me `shouldSatisfy` (not . null)

        it "can chain multiple where_ clauses" $ do
          let me = "foo $X $Y" ==> "bar $X $Y"
                   `where_` hasClass "$X" "Ord"
                   `where_` isNumeric "$Y"
          length (meConditions me) `shouldBe` 2

      describe "unless" $ do
        it "adds a negated condition to MatchExpr" $ do
          let me = "head $X" ==> "headMay $X" `unless` isLiteral "$X"
              conds = meConditions me
          length conds `shouldBe` 1
          case conds of
            [NotCondition _] -> pure ()
            _ -> expectationFailure "Expected NotCondition"

      describe "when_" $ do
        it "is an alias for where_" $ do
          let me1 = "foo" ==> "bar" `where_` isPure "$X"
              me2 = "foo" ==> "bar" `when_` isPure "$X"
          meConditions me1 `shouldBe` meConditions me2

      describe "require" $ do
        it "adds multiple conditions at once" $ do
          let me = ("foo $X $Y" ==> "bar $X $Y")
                   `require` [hasClass "$X" "Ord", isNumeric "$Y", isPure "$X"]
          length (meConditions me) `shouldBe` 3

    describe "Side Condition Combinators" $ do
      describe ".&&" $ do
        it "creates an AND condition" $ do
          let cond = hasClass "$X" "Ord" .&& isNumeric "$Y"
          case cond of
            AndCondition _ _ -> pure ()
            _ -> expectationFailure "Expected AndCondition"

        it "associates to the right" $ do
          let cond = hasClass "$X" "Ord" .&& isNumeric "$Y" .&& isPure "$Z"
          case cond of
            AndCondition _ (AndCondition _ _) -> pure ()
            _ -> expectationFailure "Expected right-associative .&&"

      describe ".||" $ do
        it "creates an OR condition" $ do
          let cond = isNumeric "$X" .|| isString "$X"
          case cond of
            OrCondition _ _ -> pure ()
            _ -> expectationFailure "Expected OrCondition"

        it "associates to the right" $ do
          let cond = isNumeric "$X" .|| isString "$X" .|| isList "$X"
          case cond of
            OrCondition _ (OrCondition _ _) -> pure ()
            _ -> expectationFailure "Expected right-associative .||"

      describe "neg" $ do
        it "creates a NOT condition" $ do
          let cond = neg (isLiteral "$X")
          case cond of
            NotCondition _ -> pure ()
            _ -> expectationFailure "Expected NotCondition"

      describe "combined conditions" $ do
        it "handles complex combinations" $ do
          let cond = (hasClass "$X" "Ord" .&& isNumeric "$Y") .|| isPure "$Z"
          case cond of
            OrCondition (AndCondition _ _) _ -> pure ()
            _ -> expectationFailure "Expected (A && B) || C structure"

        it "integrates with MatchExpr" $ do
          let me = "foo $X $Y" ==> "bar $X $Y"
                   `where_` (hasClass "$X" "Ord" .&& isNumeric "$Y")
          length (meConditions me) `shouldBe` 1

    describe "Full rule construction with new syntax" $ do
      it "creates rules with where_ condition" $ do
        let r = rule "avoid-nub" $
                  match ("nub" ==> "ordNub" `where_` hasClass "$X" "Ord")
                  & severity Suggestion
                  & message "nub is O(n²)"
                  & category Performance
        ruleId r `shouldBe` "avoid-nub"
        ruleSeverity r `shouldBe` Suggestion
        ruleConditions r `shouldSatisfy` (not . null)

      it "creates rules with multiple conditions" $ do
        let r = rule "complex-rule" $
                  match ("mapM $F $XS" ==> "traverse $F $XS"
                         `where_` (typeOf "$F" `returns` "IO _")
                         `unless` inContext "parallel")
                  & severity Suggestion
        length (ruleConditions r) `shouldBe` 2

      it "creates rules with boolean condition combinators" $ do
        let r = rule "typed-rule" $
                  match ("transform $X" ==> "optimizedTransform $X"
                         `where_` (isNumeric "$X" .|| isString "$X"))
                  & severity Suggestion
        length (ruleConditions r) `shouldBe` 1

    describe "Module context operators" $ do
      describe "fromModule" $ do
        it "sets source module on MatchExpr" $ do
          let me = "nub $X" ==> "ordNub $X" `fromModule` "Data.List"
          meSourceModule me `shouldBe` Just "Data.List"

        it "preserves other MatchExpr fields" $ do
          let me = "foo" ==> "bar" `fromModule` "Data.Foo"
          mePattern me `shouldBe` "foo"
          meReplacement me `shouldBe` Just "bar"

      describe "toModule" $ do
        it "sets target module on MatchExpr" $ do
          let me = "nub $X" ==> "ordNub $X" `toModule` "Data.Containers.ListUtils"
          meTargetModule me `shouldBe` Just "Data.Containers.ListUtils"

        it "auto-adds import for target module" $ do
          let me = "nub $X" ==> "ordNub $X" `toModule` "Data.Containers.ListUtils"
          length (meAddImports me) `shouldBe` 1
          isModule (head (meAddImports me)) `shouldBe` "Data.Containers.ListUtils"
          isSymbols (head (meAddImports me)) `shouldBe` ["ordNub"]

        it "extracts function name from replacement" $ do
          let me = "foo" ==> "bar baz" `toModule` "Data.Bar"
          isSymbols (head (meAddImports me)) `shouldBe` ["bar"]

      describe "fromTo" $ do
        it "sets both source and target modules" $ do
          let me = "nub" ==> "ordNub" `fromTo` ("Data.List", "Data.Containers.ListUtils")
          meSourceModule me `shouldBe` Just "Data.List"
          meTargetModule me `shouldBe` Just "Data.Containers.ListUtils"

    describe "Import management operators" $ do
      describe "addImport" $ do
        it "adds import to MatchExpr" $ do
          let me = "foldl" ==> "foldl'" `addImport` ("Data.List", ["foldl'"])
          length (meAddImports me) `shouldBe` 1
          isModule (head (meAddImports me)) `shouldBe` "Data.List"

        it "can chain multiple imports" $ do
          let me = "foo" ==> "bar"
                   `addImport` ("Data.Foo", ["foo"])
                   `addImport` ("Data.Bar", ["bar"])
          length (meAddImports me) `shouldBe` 2

      describe "addQualifiedImport" $ do
        it "adds qualified import with alias" $ do
          let me = "foo" ==> "M.bar" `addQualifiedImport` ("Data.Map.Strict", "M")
          length (meAddImports me) `shouldBe` 1
          isModule (head (meAddImports me)) `shouldBe` "Data.Map.Strict"
          isQualified (head (meAddImports me)) `shouldBe` Just "M"

      describe "withImports" $ do
        it "adds multiple imports at once" $ do
          let me = "foo" ==> "bar" `withImports`
                   [ importSymbols "Data.List" ["sort", "nub"]
                   , importQualified "Data.Map" "M"
                   ]
          length (meAddImports me) `shouldBe` 2

      describe "removeImport" $ do
        it "adds module to removal list" $ do
          let me = "unsafePerformIO" ==> "" `removeImport` "System.IO.Unsafe"
          length (meRemoveImports me) `shouldBe` 1
          head (meRemoveImports me) `shouldBe` "System.IO.Unsafe"

      describe "removeImports" $ do
        it "adds multiple modules to removal list" $ do
          let me = "foo" ==> "bar" `removeImports` ["Data.Foo", "Data.Bar"]
          length (meRemoveImports me) `shouldBe` 2

    describe "ImportSpec constructors" $ do
      describe "importSymbols" $ do
        it "creates ImportSpec with symbols" $ do
          let is = importSymbols "Data.List" ["sort", "nub"]
          isModule is `shouldBe` "Data.List"
          isSymbols is `shouldBe` ["sort", "nub"]
          isQualified is `shouldBe` Nothing

      describe "importQualified" $ do
        it "creates qualified ImportSpec" $ do
          let is = importQualified "Data.Map" "M"
          isModule is `shouldBe` "Data.Map"
          isSymbols is `shouldBe` []
          isQualified is `shouldBe` Just "M"

    describe "Import management in rules" $ do
      it "passes import info through rule construction" $ do
        let r = rule "use-ordNub" $
                  match ("nub $X" ==> "ordNub $X"
                         `toModule` "Data.Containers.ListUtils")
                  & severity Suggestion
        length (ruleAddImports r) `shouldBe` 1
        ruleTargetModule r `shouldBe` Just "Data.Containers.ListUtils"

      it "handles full module context in rules" $ do
        let r = rule "nub-migration" $
                  match ("nub" ==> "ordNub"
                         `fromTo` ("Data.List", "Data.Containers.ListUtils"))
                  & severity Suggestion
                  & message "nub is O(n²), use ordNub for O(n log n)"
        ruleSourceModule r `shouldBe` Just "Data.List"
        ruleTargetModule r `shouldBe` Just "Data.Containers.ListUtils"

      it "handles explicit imports with conditions" $ do
        let r = rule "strict-foldl" $
                  match ("foldl $F $Z $X" ==> "foldl' $F $Z $X"
                         `addImport` ("Data.Foldable", ["foldl'"])
                         `where_` isNumeric "$Z")
                  & severity Suggestion
        length (ruleAddImports r) `shouldBe` 1
        length (ruleConditions r) `shouldBe` 1

    describe "New side condition constructors" $ do
      describe "Context predicates" $ do
        it "creates InContext condition for test context" $ do
          let cond = inContext "test"
          case cond of
            ContextCondition (InContext "test") -> pure ()
            _ -> expectationFailure "Expected ContextCondition (InContext test)"

        it "creates InContext condition for parallel context" $ do
          let cond = inContext "parallel"
          case cond of
            ContextCondition (InContext "parallel") -> pure ()
            _ -> expectationFailure "Expected ContextCondition (InContext parallel)"

        it "can be used with where_ in MatchExpr" $ do
          let me = "mapM $F $X" ==> "traverse $F $X" `unless` inContext "parallel"
          length (meConditions me) `shouldBe` 1

      describe "Expression predicates" $ do
        it "creates NotBind condition" $ do
          let cond = notBind "$X"
          case cond of
            ExprCondition (NotBind "$X") -> pure ()
            _ -> expectationFailure "Expected ExprCondition (NotBind)"

        it "creates IsEtaReducible condition" $ do
          let cond = isEtaReducible "$F" "$X"
          case cond of
            ExprCondition (IsEtaReducible "$F" "$X") -> pure ()
            _ -> expectationFailure "Expected ExprCondition (IsEtaReducible)"

        it "can combine notBind with other conditions" $ do
          let me = "return $X >>= $F" ==> "$F $X"
                   `where_` (notBind "$X" .&& isPure "$F")
          length (meConditions me) `shouldBe` 1

      describe "Pattern analysis predicates" $ do
        it "creates NoDerivingStrategy condition" $ do
          let cond = noDerivingStrategy
          case cond of
            ExprCondition NoDerivingStrategy -> pure ()
            _ -> expectationFailure "Expected ExprCondition NoDerivingStrategy"

        it "creates WildcardNotLast condition" $ do
          let cond = wildcardNotLast
          case cond of
            ExprCondition WildcardNotLast -> pure ()
            _ -> expectationFailure "Expected ExprCondition WildcardNotLast"

        it "creates HasPatternOverlap condition" $ do
          let cond = hasOverlap
          case cond of
            ExprCondition HasOverlap -> pure ()
            _ -> expectationFailure "Expected ExprCondition HasOverlap"

        it "creates IsPatternIncomplete condition" $ do
          let cond = isIncomplete
          case cond of
            ExprCondition IsIncomplete -> pure ()
            _ -> expectationFailure "Expected ExprCondition IsIncomplete"

      describe "Type analysis predicates" $ do
        it "creates HasAmbiguousType condition" $ do
          let cond = hasAmbiguousType
          case cond of
            ExprCondition HasAmbiguousType -> pure ()
            _ -> expectationFailure "Expected ExprCondition HasAmbiguousType"

        it "creates UsesDefaultOptions condition" $ do
          let cond = usesDefaultOptions
          case cond of
            ExprCondition UsesDefaultOptions -> pure ()
            _ -> expectationFailure "Expected ExprCondition UsesDefaultOptions"

      describe "Integration with rule construction" $ do
        it "creates rule with notBind condition" $ do
          let r = rule "avoid-bind-pure" $
                    match ("return $X >>= $F" ==> "$F $X"
                           `where_` notBind "$X")
                    & severity Suggestion
                    & category Performance
          ruleId r `shouldBe` "avoid-bind-pure"
          length (ruleConditions r) `shouldBe` 1

        it "creates rule with eta-reducible condition" $ do
          let r = rule "eta-reduce" $
                    match ("\\$X -> $F $X" ==> "$F"
                           `where_` isEtaReducible "$F" "$X")
                    & severity Suggestion
                    & category Style
          ruleId r `shouldBe` "eta-reduce"
          length (ruleConditions r) `shouldBe` 1

        it "creates rule with context condition" $ do
          let r = rule "no-unsafe-io" $
                    match ("unsafePerformIO $X" ==> ""
                           `unless` inContext "test")
                    & severity Warning
                    & category Security
          ruleId r `shouldBe` "no-unsafe-io"
          length (ruleConditions r) `shouldBe` 1

        it "creates rule with default options condition" $ do
          let r = rule "explicit-aeson-options" $
                    match ("deriveJSON defaultOptions ''$T" ==> ""
                           `where_` usesDefaultOptions)
                    & severity Suggestion
                    & message "Use explicit Aeson options"
          ruleId r `shouldBe` "explicit-aeson-options"
          length (ruleConditions r) `shouldBe` 1
