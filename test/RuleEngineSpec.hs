{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : RuleEngineSpec
-- Description : Tests for the unified rule evaluation engine
module RuleEngineSpec (spec) where

import Test.Hspec
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

import Argus.Types
import Argus.Rules.Engine
import Argus.Analysis.Comments
import Argus.Rules.SideConditions qualified as SC

spec :: Spec
spec = do
  describe "Comment Handling" $ do
    describe "extractComments" $ do
      it "detects line comments" $ do
        let content = T.unlines
              [ "foo = 1 -- comment here"
              , "bar = 2"
              , "-- full line comment"
              ]
            comments = extractComments content
        -- Should find 2 line comments (one inline, one full-line)
        length (filter (\c -> commentType c == LineComment) comments) `shouldBe` 2

      it "detects block comments" $ do
        let content = "foo = 1 {- block -} + 2"
            comments = extractComments content
        length (filter (\c -> commentType c == BlockComment) comments) `shouldBe` 1

      it "handles empty content" $ do
        let comments = extractComments ""
        null comments `shouldBe` True

    describe "isInComment" $ do
      it "returns True for position inside comment" $ do
        let content = "foo = 1 {- block -} + 2"
            idx = buildCommentIndex $ extractComments content
        -- Position 10 is inside the block comment {- block -}
        isInComment idx 1 10 `shouldBe` True

      it "returns False for position outside comment" $ do
        let content = "foo = 1 {- block -} + 2"
            idx = buildCommentIndex $ extractComments content
        -- Position 5 is before the comment (in "foo = ")
        isInComment idx 1 5 `shouldBe` False

      it "returns False for line without comments" $ do
        let content = "foo = 1\nbar = 2"
            idx = buildCommentIndex $ extractComments content
        isInComment idx 2 3 `shouldBe` False

  describe "Side Conditions" $ do
    describe "evalSideCondition" $ do
      let baseCtx = MatchContext
            { mcFilePath = "Test.hs"
            , mcModuleName = "Test"
            , mcLineNumber = 1
            , mcLineText = "foo = head xs"
            , mcFullContent = "foo = head xs"
            , mcCommentIndex = emptyCommentIndex
            , mcMetavars = Map.fromList [("$X", "xs"), ("$Y", "foo")]
            , mcMatchColumn = 1
            }

      it "NotInComment returns True when not in comment" $ do
        evalSideCondition baseCtx NotInComment `shouldBe` True

      it "NotInComment returns False when in comment" $ do
        -- Create a comment index with a comment on line 1, cols 1-20
        let commentContent = "-- foo = head xs"  -- A comment covering the line
            commentIdx = buildCommentIndex $ extractComments commentContent
            ctx = baseCtx { mcCommentIndex = commentIdx }
        evalSideCondition ctx NotInComment `shouldBe` False

      it "NotInImport returns True for non-import line" $ do
        evalSideCondition baseCtx NotInImport `shouldBe` True

      it "NotInImport returns False for import line" $ do
        let ctx = baseCtx { mcLineText = "import Data.List" }
        evalSideCondition ctx NotInImport `shouldBe` False

      it "IsVariable returns True for identifier metavar" $ do
        let ctx = baseCtx { mcMetavars = Map.fromList [("$X", "myVar")] }
        evalSideCondition ctx (IsVariable "$X") `shouldBe` True

      it "IsLiteral returns True for numeric literal" $ do
        let ctx = baseCtx { mcMetavars = Map.fromList [("$X", "42")] }
        evalSideCondition ctx (IsLiteral "$X") `shouldBe` True

      it "IsLiteral returns True for string literal" $ do
        let ctx = baseCtx { mcMetavars = Map.fromList [("$X", "\"hello\"")] }
        evalSideCondition ctx (IsLiteral "$X") `shouldBe` True

      it "NotEqual returns True for different values" $ do
        evalSideCondition baseCtx (NotEqual "$X" "$Y") `shouldBe` True

      it "NotEqual returns False for equal values" $ do
        let ctx = baseCtx { mcMetavars = Map.fromList [("$X", "same"), ("$Y", "same")] }
        evalSideCondition ctx (NotEqual "$X" "$Y") `shouldBe` False

      it "InModule works with exact match" $ do
        evalSideCondition baseCtx (InModule "Test") `shouldBe` True
        evalSideCondition baseCtx (InModule "Other") `shouldBe` False

      it "InModule works with glob patterns" $ do
        let ctx = baseCtx { mcModuleName = "Test.Helpers" }
        evalSideCondition ctx (InModule "Test.*") `shouldBe` True
        evalSideCondition ctx (InModule "*Helpers") `shouldBe` True

      it "And requires all conditions" $ do
        evalSideCondition baseCtx (And [NotInComment, NotInImport]) `shouldBe` True
        let ctx = baseCtx { mcLineText = "import Test" }
        evalSideCondition ctx (And [NotInComment, NotInImport]) `shouldBe` False

      it "Or requires any condition" $ do
        let ctx = baseCtx { mcLineText = "import Test" }
        evalSideCondition ctx (Or [NotInComment, NotInImport]) `shouldBe` True

      it "Not negates condition" $ do
        evalSideCondition baseCtx (Not NotInComment) `shouldBe` False

  describe "Pattern Matching" $ do
    describe "TextPatternSpec" $ do
      it "matches text with word boundaries" $ do
        let rule = mkTestRule "test" (TextPatternSpec "head")
            engine = mkRuleEngine [rule]
            diagnostics = evaluateRules engine "Test.hs" "Test" "foo = head xs"
        length diagnostics `shouldBe` 1

      it "does not match partial words" $ do
        let rule = mkTestRule "test" (TextPatternSpec "head")
            engine = mkRuleEngine [rule]
            diagnostics = evaluateRules engine "Test.hs" "Test" "headMay = safe"
        length diagnostics `shouldBe` 0

      it "does not match in comments" $ do
        let rule = mkTestRule "test" (TextPatternSpec "head")
            engine = mkRuleEngine [rule]
            diagnostics = evaluateRules engine "Test.hs" "Test" "foo = bar -- head is bad"
        length diagnostics `shouldBe` 0

    describe "TextPatternSpec with metavariables" $ do
      it "captures metavariables" $ do
        let rule = mkTestRuleWithPattern "test" "head $X" (Just "headMay $X")
            engine = mkRuleEngine [rule]
            diagnostics = evaluateRules engine "Test.hs" "Test" "foo = head xs"
        length diagnostics `shouldBe` 1
        case diagFixes (Prelude.head diagnostics) of
          [fix'] -> fixTitle fix' `shouldSatisfy` T.isInfixOf "test"
          _ -> expectationFailure "Expected exactly one fix"

      it "interpolates metavariables in fix" $ do
        let rule = mkTestRuleWithPattern "test" "length $X == 0" (Just "null $X")
            engine = mkRuleEngine [rule]
            diagnostics = evaluateRules engine "Test.hs" "Test" "if length xs == 0 then"
        length diagnostics `shouldBe` 1

  describe "Rule Engine" $ do
    describe "mkRuleEngine" $ do
      it "creates engine with all categories enabled by default" $ do
        let engine = mkRuleEngine []
        -- All standard categories should be enabled
        Set.size (reEnabledCategories engine) `shouldSatisfy` (> 10)

      it "respects disabled rules" $ do
        let rule = mkTestRule "test/rule" (TextPatternSpec "head")
            engine = (mkRuleEngine [rule]) { reDisabledRules = Set.singleton "test/rule" }
            diagnostics = evaluateRules engine "Test.hs" "Test" "foo = head xs"
        length diagnostics `shouldBe` 0

      it "respects severity overrides" $ do
        let rule = mkTestRule "test/rule" (TextPatternSpec "head")
            engine = (mkRuleEngine [rule]) { reOverrides = Map.singleton "test/rule" Error }
            diagnostics = evaluateRules engine "Test.hs" "Test" "foo = head xs"
        length diagnostics `shouldBe` 1
        diagSeverity (Prelude.head diagnostics) `shouldBe` Error

    describe "evaluateRules" $ do
      it "returns empty list for empty content" $ do
        let engine = defaultEngine
            diagnostics = evaluateRules engine "Test.hs" "Test" ""
        length diagnostics `shouldBe` 0

      it "evaluates multiple rules" $ do
        let rule1 = mkTestRule "test/rule1" (TextPatternSpec "head")
            rule2 = mkTestRule "test/rule2" (TextPatternSpec "tail")
            engine = mkRuleEngine [rule1, rule2]
            diagnostics = evaluateRules engine "Test.hs" "Test" "foo = head xs; bar = tail ys"
        length diagnostics `shouldBe` 2

      it "respects within scope" $ do
        let rule = (mkTestRule "test/rule" (TextPatternSpec "head"))
              { ruleWithin = ["Test.Internal.*"] }
            engine = mkRuleEngine [rule]
        evaluateRules engine "Foo.hs" "Foo" "foo = head xs" `shouldBe` []
        length (evaluateRules engine "Foo.hs" "Test.Internal.Utils" "foo = head xs") `shouldBe` 1

      it "respects except scope" $ do
        let rule = (mkTestRule "test/rule" (TextPatternSpec "head"))
              { ruleExcept = ["*Test*", "*Spec*"] }
            engine = mkRuleEngine [rule]
        length (evaluateRules engine "Foo.hs" "Foo" "foo = head xs") `shouldBe` 1
        evaluateRules engine "FooTest.hs" "FooTest" "foo = head xs" `shouldBe` []
        evaluateRules engine "FooSpec.hs" "FooSpec" "foo = head xs" `shouldBe` []

  describe "HIE-Backed Side Condition Evaluation" $ do
    describe "evalSideConditionIO" $ do
      let baseCtx = MatchContext
            { mcFilePath = "Test.hs"
            , mcModuleName = "Test"
            , mcLineNumber = 1
            , mcLineText = "foo = head xs"
            , mcFullContent = "foo = head xs"
            , mcCommentIndex = emptyCommentIndex
            , mcMetavars = Map.fromList [("$X", "xs"), ("$Y", "foo")]
            , mcMatchColumn = 1
            }

      it "NotInComment works with HIE evaluator" $ do
        result <- evalSideConditionIO emptyHIEContext baseCtx NotInComment
        result `shouldBe` True

      it "NotInImport works with HIE evaluator" $ do
        result <- evalSideConditionIO emptyHIEContext baseCtx NotInImport
        result `shouldBe` True

      it "IsLiteral returns True for numeric literal in HIE evaluator" $ do
        let ctx = baseCtx { mcMetavars = Map.fromList [("$X", "42")] }
        result <- evalSideConditionIO emptyHIEContext ctx (IsLiteral "$X")
        result `shouldBe` True

      it "IsVariable returns True for identifier in HIE evaluator" $ do
        let ctx = baseCtx { mcMetavars = Map.fromList [("$X", "myVar")] }
        result <- evalSideConditionIO emptyHIEContext ctx (IsVariable "$X")
        result `shouldBe` True

      it "NotEqual works with HIE evaluator" $ do
        result <- evalSideConditionIO emptyHIEContext baseCtx (NotEqual "$X" "$Y")
        result `shouldBe` True

      it "InModule works with HIE evaluator" $ do
        result <- evalSideConditionIO emptyHIEContext baseCtx (InModule "Test")
        result `shouldBe` True

      it "And combinator works with HIE evaluator" $ do
        result <- evalSideConditionIO emptyHIEContext baseCtx (And [NotInComment, NotInImport])
        result `shouldBe` True

      it "Or combinator works with HIE evaluator" $ do
        let ctx = baseCtx { mcLineText = "import Test" }
        result <- evalSideConditionIO emptyHIEContext ctx (Or [NotInComment, NotInImport])
        result `shouldBe` True

      it "Not combinator works with HIE evaluator" $ do
        result <- evalSideConditionIO emptyHIEContext baseCtx (Not NotInComment)
        result `shouldBe` False

      it "InTestFile works correctly" $ do
        let testCtx = baseCtx { mcFilePath = "test/FooSpec.hs" }
        result <- evalSideConditionIO emptyHIEContext testCtx InTestFile
        result `shouldBe` True
        -- "Test.hs" contains "Test" so it's considered a test file
        let normalCtx = baseCtx { mcFilePath = "src/Foo.hs" }
        resultNormal <- evalSideConditionIO emptyHIEContext normalCtx InTestFile
        resultNormal `shouldBe` False

      it "IsNumeric returns True for numeric strings" $ do
        let ctx = baseCtx { mcMetavars = Map.fromList [("$X", "123.45")] }
        result <- evalSideConditionIO emptyHIEContext ctx (IsNumeric "$X")
        result `shouldBe` True

      it "IsString returns True for quoted strings" $ do
        let ctx = baseCtx { mcMetavars = Map.fromList [("$X", "\"hello\"")] }
        result <- evalSideConditionIO emptyHIEContext ctx (IsString "$X")
        result `shouldBe` True

      it "IsList returns True for list-like values" $ do
        let ctx = baseCtx { mcMetavars = Map.fromList [("$X", "[1,2,3]")] }
        result <- evalSideConditionIO emptyHIEContext ctx (IsList "$X")
        result `shouldBe` True

      it "IsConstructor returns True for capitalized identifiers" $ do
        let ctx = baseCtx { mcMetavars = Map.fromList [("$X", "Just")] }
        result <- evalSideConditionIO emptyHIEContext ctx (IsConstructor "$X")
        result `shouldBe` True

    describe "HIE Context" $ do
      it "emptyHIEContext has empty symbol types" $ do
        SC.hieSymbolTypes emptyHIEContext `shouldBe` Map.empty

      it "emptyHIEContext has default pure functions set" $ do
        SC.hieKnownPure emptyHIEContext `shouldSatisfy` \s -> Set.member "map" s

    describe "Type Class Checks (known types database)" $ do
      it "checkTypeClass returns True for known Ord instances" $ do
        result <- SC.checkTypeClass "Ord" "Int"
        result `shouldBe` True

      it "checkTypeClass returns True for known Eq instances" $ do
        result <- SC.checkTypeClass "Eq" "Text"
        result `shouldBe` True

      it "checkTypeClass returns True for known Monoid instances" $ do
        -- Use "Text" instead of "[a]" because extractBaseType transforms "[a]" to "a"
        result <- SC.checkTypeClass "Monoid" "Text"
        result `shouldBe` True

  describe "Built-in Rules" $ do
    describe "defaultEngine rules" $ do
      it "contains rules for safety category" $ do
        let safetyRules = filter (\r -> ruleCategory r == Safety) (reRules defaultEngine)
        length safetyRules `shouldSatisfy` (> 0)

      it "contains rules for performance category" $ do
        let perfRules = filter (\r -> ruleCategory r == Performance) (reRules defaultEngine)
        length perfRules `shouldSatisfy` (> 0)

      it "defaultEngine has builtin rules loaded" $ do
        length (reRules defaultEngine) `shouldSatisfy` (> 0)

  describe "Metavariable Interpolation" $ do
    describe "interpolateMessage" $ do
      it "substitutes single metavariable" $ do
        let msg = "Use headMay instead of $F"
            vars = Map.fromList [("$F", "head")]
        interpolateMessage msg vars `shouldBe` "Use headMay instead of head"

      it "substitutes multiple metavariables" $ do
        let msg = "Replace $F $X with safer $F' $X"
            vars = Map.fromList [("$F", "head"), ("$X", "xs")]
        interpolateMessage msg vars `shouldBe` "Replace head xs with safer head' xs"

      it "returns original message when metavar map is empty" $ do
        let msg = "This has $X but no substitutions"
        interpolateMessage msg Map.empty `shouldBe` msg

      it "handles replacement values containing pattern-like text" $ do
        -- This was the bug: if $F="f" and that appeared elsewhere, it would corrupt
        let msg = "concatMap $F $XS"
            vars = Map.fromList [("$F", "f"), ("$XS", "xs")]
        interpolateMessage msg vars `shouldBe` "concatMap f xs"

      it "does not corrupt when replacement value overlaps with metavar name" $ do
        -- Bug case: if replacement for $XS contains "$F"
        let msg = "Replace $F with $XS"
            vars = Map.fromList [("$F", "func"), ("$XS", "$F_improved")]
        -- The $F in $F_improved should NOT be substituted
        interpolateMessage msg vars `shouldBe` "Replace func with $F_improved"

      it "respects metavariable boundaries ($XS vs $X)" $ do
        let msg = "Use $X and $XS separately"
            vars = Map.fromList [("$X", "x"), ("$XS", "xs")]
        interpolateMessage msg vars `shouldBe` "Use x and xs separately"

      it "handles metavariables appearing multiple times" $ do
        let msg = "$X is used here and $X is used again"
            vars = Map.fromList [("$X", "myVar")]
        interpolateMessage msg vars `shouldBe` "myVar is used here and myVar is used again"

      it "preserves non-matching text unchanged" $ do
        let msg = "No metavariables here at all"
            vars = Map.fromList [("$X", "x")]
        interpolateMessage msg vars `shouldBe` "No metavariables here at all"

      it "handles complex replacements with special characters" $ do
        let msg = "Transform $EXPR"
            vars = Map.fromList [("$EXPR", "foo (bar + baz)")]
        interpolateMessage msg vars `shouldBe` "Transform foo (bar + baz)"

      it "correctly handles underscore-style metavariables" $ do
        let msg = "Replace _f _xs with better version"
            vars = Map.fromList [("_f", "myFunc"), ("_xs", "myList")]
        interpolateMessage msg vars `shouldBe` "Replace myFunc myList with better version"

    describe "Metavariable boundary detection" $ do
      it "does not match $X inside $XSOMETHING" $ do
        let msg = "$XSOMETHING should not be affected by $X"
            vars = Map.fromList [("$X", "replaced")]
        -- $XSOMETHING should stay as-is, only $X at end should be replaced
        interpolateMessage msg vars `shouldBe` "$XSOMETHING should not be affected by replaced"

      it "correctly handles adjacent metavariables" $ do
        let msg = "$X$Y"
            vars = Map.fromList [("$X", "a"), ("$Y", "b")]
        interpolateMessage msg vars `shouldBe` "ab"

-- Helper functions for creating test rules using unified Rule type
mkTestRule :: T.Text -> RulePattern -> Rule
mkTestRule rId pattern' = Rule
  { ruleId = rId
  , ruleCategory = Performance
  , ruleSeverity = Warning
  , ruleMessage = "Test rule matched"
  , ruleExplanation = Nothing
  , rulePattern = pattern'
  , ruleReplacement = Nothing
  , ruleConditions = []
  , ruleSafety = Safe
  , ruleAddImports = []
  , ruleRemoveImports = []
  , ruleEnabled = True
  , ruleWithin = []
  , ruleExcept = []
  , ruleDeprecated = Nothing
  , ruleTags = []
  , ruleReferences = []
  , ruleFixDescription = Nothing
  , ruleNote = Nothing
  , ruleSourceModule = Nothing
  , ruleTargetModule = Nothing
  , ruleTarget = Nothing  -- Use category default
  }

mkTestRuleWithPattern :: T.Text -> T.Text -> Maybe T.Text -> Rule
mkTestRuleWithPattern rId patText mFix = Rule
  { ruleId = rId
  , ruleCategory = Performance
  , ruleSeverity = Warning
  , ruleMessage = "Test rule matched"
  , ruleExplanation = Nothing
  , rulePattern = TextPatternSpec patText
  , ruleReplacement = mFix
  , ruleConditions = []
  , ruleSafety = Safe
  , ruleAddImports = []
  , ruleRemoveImports = []
  , ruleEnabled = True
  , ruleWithin = []
  , ruleExcept = []
  , ruleDeprecated = Nothing
  , ruleTags = []
  , ruleReferences = []
  , ruleFixDescription = Nothing
  , ruleNote = Nothing
  , ruleSourceModule = Nothing
  , ruleTargetModule = Nothing
  , ruleTarget = Nothing  -- Use category default
  }
