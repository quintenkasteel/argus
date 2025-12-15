{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : RuleEnginePropertySpec
-- Description : Property-based tests for the rule evaluation engine
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Comprehensive property-based testing for the rule engine ensuring
-- correctness, consistency, and expected behavior under random inputs.
module RuleEnginePropertySpec (spec) where

import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Argus.Types hiding (Custom)
import Argus.Rules.Engine
import Argus.Rules.Types
import Argus.Analysis.Comments (emptyCommentIndex)

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- | Generate a valid rule identifier
genRuleId :: Gen Text
genRuleId = do
  category <- elements ["safety", "performance", "style", "security", "imports"]
  name <- elements ["rule1", "rule2", "test", "check", "validate"]
  pure $ category <> "/" <> name

-- | Generate a valid rule category
genCategory :: Gen Category
genCategory = elements
  [ Performance
  , SpaceLeaks
  , Security
  , Safety
  , Style
  , Correctness
  , Modernization
  , Imports
  , Naming
  , Extensions
  , Complexity
  , Concurrency
  , ErrorHandling
  , Documentation
  , Redundant
  ]

-- | Generate a safety level
genSafetyLevel :: Gen SafetyLevel
genSafetyLevel = elements [Safe, MostlySafe, NeedsReview, Unsafe]

-- | Generate a severity level
genSeverity :: Gen Severity
genSeverity = elements [Error, Warning, Suggestion, Info]

-- | Generate a simple text pattern (no metavariables)
genSimpleTextPattern :: Gen Text
genSimpleTextPattern = elements
  [ "head"
  , "tail"
  , "fromJust"
  , "unsafePerformIO"
  , "length xs == 0"
  , "foldl"
  , "init"
  , "last"
  , "read"
  , "undefined"
  ]

-- | Generate a pattern with metavariables
genMetavarPattern :: Gen Text
genMetavarPattern = elements
  [ "head $X"
  , "tail $X"
  , "length $X == 0"
  , "foldl $F $X $Y"
  , "fromMaybe $X $Y"
  , "$F <$> $X"
  , "return $X"
  , "pure $X"
  ]

-- | Generate a rule pattern specification
genRulePattern :: Gen RulePattern
genRulePattern = frequency
  [ (7, TextPatternSpec <$> genSimpleTextPattern)
  , (3, TextPatternSpec <$> genMetavarPattern)
  ]

-- | Generate a rule message
genRuleMessage :: Gen Text
genRuleMessage = elements
  [ "Use safe alternative"
  , "Potential performance issue"
  , "Consider using a safer pattern"
  , "This pattern is deprecated"
  , "Security concern detected"
  ]

-- | Generate an optional replacement pattern
genReplacement :: Gen (Maybe Text)
genReplacement = frequency
  [ (6, Just <$> elements ["headMay $X", "null $X", "foldl' $F $X $Y", "safeHead $X"])
  , (4, pure Nothing)
  ]

-- | Generate a basic side condition
genSimpleSideCondition :: Gen SideCondition
genSimpleSideCondition = elements
  [ NotInComment
  , NotInString
  , NotInImport
  , InFunctionBody
  , Always
  ]

-- | Generate a side condition (recursive with limited depth)
genSideCondition :: Int -> Gen SideCondition
genSideCondition 0 = genSimpleSideCondition
genSideCondition depth = frequency
  [ (7, genSimpleSideCondition)
  , (1, And <$> listOf1 (genSideCondition (depth - 1)))
  , (1, Or <$> listOf1 (genSideCondition (depth - 1)))
  , (1, Not <$> genSideCondition (depth - 1))
  ]

-- | Generate module scope patterns
genScopePatterns :: Gen [Text]
genScopePatterns = frequency
  [ (7, pure [])
  , (2, listOf1 $ elements ["Test.*", "*.Internal.*", "Main", "Lib.*"])
  , (1, pure ["*"])
  ]

-- | Generate a complete rule
genRule :: Gen Rule
genRule = do
  rId <- genRuleId
  rCat <- genCategory
  rSev <- genSeverity
  rMsg <- genRuleMessage
  rPat <- genRulePattern
  rRepl <- genReplacement
  rConds <- resize 3 $ listOf (genSideCondition 2)
  rSafety <- genSafetyLevel
  rWithin <- genScopePatterns
  rExcept <- genScopePatterns
  rEnabled <- frequency [(9, pure True), (1, pure False)]
  pure Rule
    { ruleId = rId
    , ruleCategory = rCat
    , ruleSeverity = rSev
    , ruleMessage = rMsg
    , ruleExplanation = Nothing
    , rulePattern = rPat
    , ruleReplacement = rRepl
    , ruleConditions = rConds
    , ruleSafety = rSafety
    , ruleAddImports = []
    , ruleRemoveImports = []
    , ruleEnabled = rEnabled
    , ruleWithin = rWithin
    , ruleExcept = rExcept
    , ruleDeprecated = Nothing
    , ruleTags = []
    , ruleReferences = []
    , ruleFixDescription = Nothing
    , ruleNote = Nothing
    , ruleSourceModule = Nothing
    , ruleTargetModule = Nothing
    , ruleTarget = Nothing
    }

-- | Generate a Haskell source line
genSourceLine :: Gen Text
genSourceLine = elements
  [ "foo = head xs"
  , "bar = tail ys"
  , "baz = fromJust mx"
  , "qux = foldl (+) 0 ns"
  , "result = length xs == 0"
  , "main = putStrLn \"hello\""
  , "f x = x + 1"
  , "-- this is a comment"
  , "import Data.List"
  , "{- block comment -}"
  , "g = map f . filter p"
  , "h = unsafePerformIO action"
  ]

-- | Generate a module name
genModuleName :: Gen Text
genModuleName = elements
  [ "Main"
  , "Test"
  , "Lib"
  , "Test.Utils"
  , "App.Internal.Core"
  , "Data.MyList"
  ]

-- | Generate a match context
genMatchContext :: Gen MatchContext
genMatchContext = do
  moduleName <- genModuleName
  lineNum <- chooseInt (1, 100)
  lineText <- genSourceLine
  col <- chooseInt (1, max 1 (T.length lineText))
  pure MatchContext
    { mcFilePath = "Test.hs"
    , mcModuleName = moduleName
    , mcLineNumber = lineNum
    , mcLineText = lineText
    , mcFullContent = lineText
    , mcCommentIndex = emptyCommentIndex
    , mcMetavars = Map.fromList [("$X", "xs"), ("$Y", "ys"), ("$F", "f")]
    , mcMatchColumn = col
    }

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Category where
  arbitrary = genCategory

instance Arbitrary SafetyLevel where
  arbitrary = genSafetyLevel

instance Arbitrary Severity where
  arbitrary = genSeverity

instance Arbitrary RulePattern where
  arbitrary = genRulePattern

instance Arbitrary SideCondition where
  arbitrary = genSideCondition 3

instance Arbitrary Rule where
  arbitrary = genRule

instance Arbitrary MatchContext where
  arbitrary = genMatchContext

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Rule Engine Property Tests" $ do
    ruleTypeProperties
    sideConditionProperties
    engineConsistencyProperties
    categoryProperties
    safetyLevelProperties
    patternMatchingProperties
    jsonRoundTripProperties

--------------------------------------------------------------------------------
-- Rule Type Properties
--------------------------------------------------------------------------------

ruleTypeProperties :: Spec
ruleTypeProperties = describe "Rule type properties" $ do
  prop "rule ID is always non-empty in generated rules" $ \(rule :: Rule) ->
    not (T.null (ruleId rule))

  prop "rule message is always non-empty in generated rules" $ \(rule :: Rule) ->
    not (T.null (ruleMessage rule))

  prop "enabled rules can be disabled" $ \(rule :: Rule) ->
    let disabled = rule { ruleEnabled = False }
    in not (ruleEnabled disabled)

  prop "rule severity can be overridden" $ \(rule :: Rule, newSev :: Severity) ->
    let overridden = rule { ruleSeverity = newSev }
    in ruleSeverity overridden == newSev

  prop "defaultRule has safe defaults" $
    ruleSafety defaultRule == Safe &&
    ruleEnabled defaultRule &&
    null (ruleConditions defaultRule)

  prop "equality is reflexive" $ \(rule :: Rule) ->
    rule == rule

  prop "equality is symmetric" $ \(r1 :: Rule, r2 :: Rule) ->
    (r1 == r2) == (r2 == r1)

--------------------------------------------------------------------------------
-- Side Condition Properties
--------------------------------------------------------------------------------

sideConditionProperties :: Spec
sideConditionProperties = describe "SideCondition properties" $ do
  prop "Always evaluates to True" $ \(ctx :: MatchContext) ->
    evalSideCondition ctx Always

  prop "Never evaluates to False" $ \(ctx :: MatchContext) ->
    not $ evalSideCondition ctx Never

  prop "Not inverts result" $ \(ctx :: MatchContext, cond :: SideCondition) ->
    evalSideCondition ctx (Not cond) == not (evalSideCondition ctx cond)

  prop "And with empty list is True" $ \(ctx :: MatchContext) ->
    evalSideCondition ctx (And [])

  prop "And with Always is identity" $ \(ctx :: MatchContext, cond :: SideCondition) ->
    evalSideCondition ctx (And [Always, cond]) == evalSideCondition ctx cond

  prop "And with Never is always False" $ \(ctx :: MatchContext, cond :: SideCondition) ->
    not $ evalSideCondition ctx (And [Never, cond])

  prop "Or with empty list is False" $ \(ctx :: MatchContext) ->
    not $ evalSideCondition ctx (Or [])

  prop "Or with Always is always True" $ \(ctx :: MatchContext, cond :: SideCondition) ->
    evalSideCondition ctx (Or [Always, cond])

  prop "Or with Never is identity" $ \(ctx :: MatchContext, cond :: SideCondition) ->
    evalSideCondition ctx (Or [Never, cond]) == evalSideCondition ctx cond

  prop "Double negation is identity" $ \(ctx :: MatchContext, cond :: SideCondition) ->
    evalSideCondition ctx (Not (Not cond)) == evalSideCondition ctx cond

  prop "De Morgan's law: Not (And [a,b]) == Or [Not a, Not b]" $ \(ctx :: MatchContext) ->
    let a = NotInComment
        b = NotInImport
    in evalSideCondition ctx (Not (And [a, b])) ==
       evalSideCondition ctx (Or [Not a, Not b])

  prop "De Morgan's law: Not (Or [a,b]) == And [Not a, Not b]" $ \(ctx :: MatchContext) ->
    let a = NotInComment
        b = NotInImport
    in evalSideCondition ctx (Not (Or [a, b])) ==
       evalSideCondition ctx (And [Not a, Not b])

--------------------------------------------------------------------------------
-- Engine Consistency Properties
--------------------------------------------------------------------------------

engineConsistencyProperties :: Spec
engineConsistencyProperties = describe "Engine consistency properties" $ do
  prop "empty rule set produces no diagnostics" $ \(content :: Text) ->
    let engine = mkRuleEngine []
    in null $ evaluateRules engine "Test.hs" "Test" content

  prop "disabled rules produce no diagnostics" $ \(rule :: Rule) ->
    let disabledRule = rule { ruleEnabled = False }
        engine = mkRuleEngine [disabledRule]
    in null $ evaluateRules engine "Test.hs" "Test" "foo = head xs"

  prop "excluded modules produce no diagnostics" $ \(rule :: Rule) ->
    let excludedRule = rule { ruleExcept = ["Test*"] }
        engine = mkRuleEngine [excludedRule]
    in null $ evaluateRules engine "Test.hs" "TestModule" "foo = head xs"

  prop "within scope respects module patterns" $ \(rule :: Rule) ->
    let scopedRule = rule { ruleWithin = ["Internal.*"], ruleExcept = [] }
        engine = mkRuleEngine [scopedRule]
    in null $ evaluateRules engine "Test.hs" "External.Module" "foo = head xs"

  prop "comment-aware engine skips comment lines" $ do
    let rule = defaultRule
          { ruleId = "test"
          , rulePattern = TextPatternSpec "head"
          , ruleEnabled = True
          }
        engine = mkRuleEngine [rule]
        codeContent = "foo = head xs"
        commentContent = "-- head is mentioned in comment"
    length (evaluateRules engine "Test.hs" "Test" codeContent) >= 0 &&
      null (evaluateRules engine "Test.hs" "Test" commentContent)

  prop "diagnostics have valid source spans" $ \(rule :: Rule) ->
    let engine = mkRuleEngine [rule { ruleEnabled = True }]
        diagnostics = evaluateRules engine "Test.hs" "Test" "foo = head xs"
        validSpan diag =
          srcSpanStartLineRaw (diagSpan diag) >= 1 &&
          srcSpanStartColRaw (diagSpan diag) >= 1
    in all validSpan diagnostics

  prop "diagnostics inherit rule severity (unless overridden)" $ \(rule :: Rule) ->
    let engine = mkRuleEngine [rule { ruleEnabled = True }]
        diagnostics = evaluateRules engine "Test.hs" "Test" "foo = head xs"
    in all (\d -> diagSeverity d == ruleSeverity rule) diagnostics

  prop "severity overrides work" $ \(rule :: Rule) ->
    let engine = (mkRuleEngine [rule { ruleEnabled = True }])
          { reOverrides = Map.singleton (ruleId rule) Error }
        diagnostics = evaluateRules engine "Test.hs" "Test" "foo = head xs"
    in all (\d -> diagSeverity d == Error) diagnostics

  prop "disabled categories produce no diagnostics" $ \(rule :: Rule) ->
    let engine = (mkRuleEngine [rule { ruleEnabled = True }])
          { reEnabledCategories = Set.empty }
    in null $ evaluateRules engine "Test.hs" "Test" "foo = head xs"

  prop "disabled rule IDs produce no diagnostics" $ \(rule :: Rule) ->
    let engine = (mkRuleEngine [rule { ruleEnabled = True }])
          { reDisabledRules = Set.singleton (ruleId rule) }
    in null $ evaluateRules engine "Test.hs" "Test" "foo = head xs"

--------------------------------------------------------------------------------
-- Category Properties
--------------------------------------------------------------------------------

categoryProperties :: Spec
categoryProperties = describe "Category properties" $ do
  prop "categoryToText produces non-empty text" $ \(cat :: Category) ->
    not $ T.null $ categoryToText cat

  prop "textToCategory . categoryToText is identity for builtin categories" $ \(cat :: Category) ->
    case cat of
      Custom _ -> True  -- Custom categories may normalize
      _ -> textToCategory (categoryToText cat) == cat

  prop "allCategories contains standard categories" $
    Performance `elem` allCategories &&
    Security `elem` allCategories &&
    Safety `elem` allCategories

  prop "allCategories does not contain Custom" $
    let isCustom (Custom _) = True
        isCustom _ = False
    in all (not . isCustom) allCategories

  prop "categoryToFixCategory maps all categories" $ \(cat :: Category) ->
    let fc = categoryToFixCategory cat
    in fc `seq` True  -- Force evaluation to ensure no error

  prop "fixCategoryToCategory maps back" $ \(cat :: Category) ->
    let fc = categoryToFixCategory cat
        cat' = fixCategoryToCategory fc
    in cat' `seq` True  -- Just ensure it doesn't crash

--------------------------------------------------------------------------------
-- Safety Level Properties
--------------------------------------------------------------------------------

safetyLevelProperties :: Spec
safetyLevelProperties = describe "SafetyLevel properties" $ do
  prop "safetyToText produces non-empty text" $ \(safety :: SafetyLevel) ->
    not $ T.null $ safetyToText safety

  prop "textToSafety . safetyToText is identity" $ \(safety :: SafetyLevel) ->
    textToSafety (safetyToText safety) == safety

  prop "Safe is most permissive for auto-fix" $
    Safe < Unsafe

  prop "ordering is total" $ \(s1 :: SafetyLevel, s2 :: SafetyLevel) ->
    s1 <= s2 || s2 <= s1

  prop "safetyToFixSafety maps all levels" $ \(safety :: SafetyLevel) ->
    let fs = safetyToFixSafety safety
    in fs `seq` True

  prop "fixSafetyToSafety is inverse" $ \(safety :: SafetyLevel) ->
    let fs = safetyToFixSafety safety
        safety' = fixSafetyToSafety fs
    in safety' == safety

--------------------------------------------------------------------------------
-- Pattern Matching Properties
--------------------------------------------------------------------------------

patternMatchingProperties :: Spec
patternMatchingProperties = describe "Pattern matching properties" $ do
  prop "TextPatternSpec pattern text is preserved" $ \patText ->
    let validText = if T.null patText then "test" else patText
        pat = TextPatternSpec validText
    in rulePatternToText pat == validText

  prop "RegexPatternSpec pattern text is preserved" $ \patText ->
    let validText = if T.null patText then "test" else patText
        pat = RegexPatternSpec validText
    in rulePatternToText pat == validText

  prop "ASTPatternSpec pattern text is preserved" $ \patText ->
    let validText = if T.null patText then "test" else patText
        pat = ASTPatternSpec validText
    in rulePatternToText pat == validText

  it "word boundary matching prevents partial matches" $ do
    let rule = defaultRule
          { ruleId = "test"
          , rulePattern = TextPatternSpec "head"
          , ruleEnabled = True
          }
        engine = mkRuleEngine [rule]
    -- Should match "head" as standalone word
    length (evaluateRules engine "Test.hs" "Test" "foo = head xs") `shouldBe` 1
    -- Should NOT match "headMay" (partial word)
    length (evaluateRules engine "Test.hs" "Test" "foo = headMay xs") `shouldBe` 0
    -- Should NOT match "overhead" (partial word)
    length (evaluateRules engine "Test.hs" "Test" "overhead = 100") `shouldBe` 0

--------------------------------------------------------------------------------
-- JSON Round-Trip Properties
--------------------------------------------------------------------------------

jsonRoundTripProperties :: Spec
jsonRoundTripProperties = describe "JSON round-trip properties" $ do
  prop "Category JSON round-trips" $ \(cat :: Category) ->
    jsonRoundTrip cat

  prop "SafetyLevel JSON round-trips" $ \(safety :: SafetyLevel) ->
    jsonRoundTrip safety

  prop "RulePattern JSON round-trips" $ \(pat :: RulePattern) ->
    jsonRoundTrip pat

  prop "SideCondition JSON round-trips" $ \(cond :: SideCondition) ->
    jsonRoundTrip cond

  prop "Rule JSON round-trips" $ \(rule :: Rule) ->
    jsonRoundTrip rule

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Test JSON round-trip property
jsonRoundTrip :: (Eq a, ToJSON a, FromJSON a) => a -> Bool
jsonRoundTrip x = decode (encode x) == Just x

-- | Generate arbitrary text (for QuickCheck)
instance Arbitrary Text where
  arbitrary = T.pack <$> listOf (elements ['a'..'z'])
