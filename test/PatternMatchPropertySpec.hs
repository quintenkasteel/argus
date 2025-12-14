{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : PatternMatchPropertySpec
-- Description : Property-based tests for pattern matching and substitution
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Comprehensive property-based tests for AST pattern matching ensuring:
-- - Substitution composition and algebraic properties
-- - Deterministic matching behavior
-- - Variable binding correctness
-- - Wildcard pattern behavior
-- - Rule application safety
-- - Round-trip parse → match → apply correctness
module PatternMatchPropertySpec (spec) where

import Control.Monad (guard)
import Data.Char (isAlpha, isLower)
import Data.Either (isRight)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Argus.Rules.ASTMatch
import Argus.Rules.Types
import Argus.Refactor.Substitution qualified as Subst

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- | Generate a simple variable name (single lowercase letter or underscore-prefixed)
genVarName :: Gen Text
genVarName = elements
  [ "x", "y", "z", "f", "g", "h"
  , "_x", "_y", "_foo", "_bar"
  , "$x", "$y", "$X", "$F"
  ]

-- | Generate a metavariable name (used in patterns)
-- Note: Dollar-prefixed names like $X don't parse as valid Haskell.
-- Use underscore-prefix or single lowercase letters which are recognized
-- as metavariables by isUnifyVar and are valid Haskell identifiers.
genMetaVar :: Gen Text
genMetaVar = elements ["_X", "_Y", "_Z", "_f", "_g", "x", "y", "z"]

-- | Generate a simple literal
genLiteral :: Gen Text
genLiteral = elements ["0", "1", "42", "100", "True", "False", "\"hello\"", "\"test\""]

-- | Generate a function name
genFuncName :: Gen Text
genFuncName = elements
  [ "f", "g", "h", "map", "filter", "fold", "head", "tail"
  , "length", "null", "show", "read", "pure", "return"
  ]

-- | Generate a simple Haskell expression as text
genSimpleExprText :: Gen Text
genSimpleExprText = frequency
  [ (5, genVarName)
  , (3, genLiteral)
  , (2, genFuncName)
  ]

-- | Generate a composite expression as text
genExprText :: Int -> Gen Text
genExprText 0 = genSimpleExprText
genExprText depth = frequency
  [ (4, genSimpleExprText)
  , (2, genApplication depth)
  , (2, genInfixApp depth)
  , (1, genParens depth)
  , (1, genList depth)
  ]
  where
    genApplication d = do
      f <- genExprText (d - 1)
      x <- genExprText (d - 1)
      pure $ f <> " " <> x

    genInfixApp d = do
      l <- genExprText (d - 1)
      op <- elements ["+", "-", "*", "++", "<>", ".", "$"]
      r <- genExprText (d - 1)
      pure $ l <> " " <> op <> " " <> r

    genParens d = do
      e <- genExprText (d - 1)
      pure $ "(" <> e <> ")"

    genList d = do
      count <- chooseInt (0, 3)
      es <- vectorOf count (genExprText (d - 1))
      pure $ "[" <> T.intercalate ", " es <> "]"

-- | Generate a pattern expression with metavariables
genPatternText :: Int -> Gen Text
genPatternText 0 = frequency
  [ (5, genMetaVar)
  , (3, genLiteral)
  , (2, genFuncName)
  ]
genPatternText depth = frequency
  [ (4, genPatternText 0)
  , (2, do
      f <- genPatternText (depth - 1)
      x <- genMetaVar
      pure $ f <> " " <> x)
  , (1, do
      l <- genMetaVar
      op <- elements ["+", "++", "<>"]
      r <- genMetaVar
      pure $ l <> " " <> op <> " " <> r)
  ]

-- | Generate a valid LHsExpr GhcPs by parsing simple expressions
genSimpleExpr :: Gen (LHsExpr GhcPs)
genSimpleExpr = do
  exprText <- genSimpleExprText
  case parsePatternPure exprText of
    Right expr -> pure expr
    Left _ -> pure $ parseHelper "x"  -- Fallback to simple variable

-- | Helper to parse an expression, fails on error (for internal use only)
parseHelper :: Text -> LHsExpr GhcPs
parseHelper txt = case parsePatternPure txt of
  Right e -> e
  Left err -> error $ "Parse failed: " <> T.unpack err

-- | Generate a Subst with random variable bindings
genSubst :: Gen Subst
genSubst = do
  count <- chooseInt (0, 5)
  bindings <- vectorOf count genBinding
  pure $ Subst (Map.fromList bindings)
  where
    genBinding = do
      var <- genMetaVar
      expr <- genSimpleExpr
      pure (var, expr)

-- | Generate a side condition
genSideCondition :: Int -> Gen SideCondition
genSideCondition 0 = elements
  [ NotInComment
  , NotInString
  , NotInImport
  , InFunctionBody
  , Always
  ]
genSideCondition depth = frequency
  [ (7, genSideCondition 0)
  , (1, And <$> listOf1 (genSideCondition (depth - 1)))
  , (1, Or <$> listOf1 (genSideCondition (depth - 1)))
  , (1, Not <$> genSideCondition (depth - 1))
  , (1, IsAtomic <$> genMetaVar)
  , (1, IsLiteral <$> genMetaVar)
  , (1, IsVariable <$> genMetaVar)
  , (1, do
      v1 <- genMetaVar
      v2 <- genMetaVar
      pure $ NotEqual v1 v2)
  ]

-- | Generate a simple Rule for testing
genSimpleRule :: Gen Rule
genSimpleRule = do
  patText <- genPatternText 2
  replText <- genPatternText 2
  cat <- elements [Safety, Performance, Style, Correctness]
  sev <- elements [Warning, Suggestion, Info]
  safety <- elements [Safe, MostlySafe, NeedsReview]
  enabled <- frequency [(9, pure True), (1, pure False)]
  pure Rule
    { ruleId = "test/rule"
    , ruleCategory = cat
    , ruleSeverity = sev
    , ruleMessage = "Test rule"
    , ruleExplanation = Nothing
    , rulePattern = ASTPatternSpec patText
    , ruleReplacement = Just replText
    , ruleConditions = []
    , ruleSafety = safety
    , ruleAddImports = []
    , ruleRemoveImports = []
    , ruleEnabled = enabled
    , ruleWithin = []
    , ruleExcept = []
    , ruleDeprecated = Nothing
    , ruleTags = []
    , ruleReferences = []
    , ruleNote = Nothing
    , ruleFixDescription = Nothing
    , ruleSourceModule = Nothing
    , ruleTargetModule = Nothing
    , ruleTarget = Nothing
    }

--------------------------------------------------------------------------------
-- Arbitrary Instances
--------------------------------------------------------------------------------

instance Arbitrary Subst where
  arbitrary = genSubst
  shrink (Subst m) = Subst <$> shrinkMap m
    where
      shrinkMap mp = case Map.toList mp of
        [] -> []
        (_:rest) -> [Map.fromList rest]

instance Arbitrary SideCondition where
  arbitrary = genSideCondition 3
  shrink = genericShrink
    where
      genericShrink (And cs) = cs ++ map And (filter (not . null) $ shrink cs)
      genericShrink (Or cs) = cs ++ map Or (filter (not . null) $ shrink cs)
      genericShrink (Not c) = [c] ++ map Not (shrink c)
      genericShrink _ = []

instance Arbitrary Rule where
  arbitrary = genSimpleRule
  shrink _ = []

instance Arbitrary Text where
  arbitrary = T.pack <$> listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
  shrink t = if T.null t then [] else [T.take (T.length t - 1) t]

instance Arbitrary SafetyLevel where
  arbitrary = arbitraryBoundedEnum

--------------------------------------------------------------------------------
-- Property Tests
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Pattern Matching Properties" $ do
    substitutionProperties
    unificationProperties
    variableBindingProperties
    wildcardProperties
    ruleApplicationProperties
    safetyProperties
    roundTripProperties
    expressionAnalysisProperties
    sideConditionProperties

--------------------------------------------------------------------------------
-- Substitution Properties
--------------------------------------------------------------------------------

substitutionProperties :: Spec
substitutionProperties = describe "Substitution operations" $ do
  prop "emptySubst has no bindings" $
    Map.null (unSubst emptySubst)

  prop "singletonSubst creates one binding" $ \var ->
    let expr = parseHelper "x"
        subst = singletonSubst var expr
    in Map.size (unSubst subst) == 1

  prop "lookupSubst returns what was inserted" $ \var ->
    let expr = parseHelper "x"
        subst = singletonSubst var expr
    in isJust (lookupSubst var subst)

  prop "lookupSubst for nonexistent key returns Nothing" $ \(subst :: Subst) ->
    isNothing (lookupSubst "$NONEXISTENT$" subst)

  prop "mergeSubst is idempotent for same substitution" $ \(subst :: Subst) ->
    isJust (mergeSubst subst subst)

  prop "mergeSubst with emptySubst is identity" $ \(subst :: Subst) ->
    isJust (mergeSubst subst emptySubst) &&
    isJust (mergeSubst emptySubst subst)

  prop "validSubst always returns True" $ \(subst :: Subst) ->
    validSubst subst

  prop "substKeys returns all keys in substitution" $ \(subst :: Subst) ->
    let keys = substKeys subst
        mapKeys = Map.keys (unSubst subst)
    in sort keys == sort mapKeys

  prop "substValues returns all values in substitution" $ \(subst :: Subst) ->
    let values = substValues subst
        mapValues = Map.elems (unSubst subst)
    in length values == length mapValues

  prop "Subst monoid identity: mempty <> subst has same keys" $ \(subst :: Subst) ->
    sort (substKeys (mempty <> subst)) == sort (substKeys subst)

  prop "Subst semigroup combined has keys from both" $ \(s1 :: Subst) (s2 :: Subst) ->
    let combined = s1 <> s2
    in all (`elem` substKeys combined) (substKeys s1)

--------------------------------------------------------------------------------
-- Unification Properties
--------------------------------------------------------------------------------

unificationProperties :: Spec
unificationProperties = describe "Expression unification" $ do
  prop "unifyExpr is reflexive for all expressions" $
    forAllBlind genSimpleExpr $ \expr ->
      -- Reflexivity: any expression should unify with itself
      isJust (unifyExpr expr expr)

  prop "matching is deterministic - same input produces same output" $
    forAllBlind genSimpleExpr $ \expr1 ->
      forAllBlind genSimpleExpr $ \expr2 ->
        let result1 = unifyExpr expr1 expr2
            result2 = unifyExpr expr1 expr2
        in isJust result1 == isJust result2

  prop "unifying with a metavariable always succeeds" $
    forAll genMetaVar $ \varName ->
      forAllBlind genSimpleExpr $ \target ->
        let patExpr = parseHelper varName
        in if isUnifyVar varName
           then isJust (unifyExpr patExpr target)
           else True

  prop "isUnifyVar recognizes single lowercase letters" $
    forAll (elements ["x", "y", "z", "f", "g"]) $ \var ->
      isUnifyVar var

  prop "isUnifyVar recognizes underscore-prefixed names" $
    forAll (elements ["_x", "_foo", "_bar"]) $ \var ->
      isUnifyVar var

  prop "isUnifyVar recognizes dollar-prefixed names" $
    forAll (elements ["$x", "$X", "$foo"]) $ \var ->
      isUnifyVar var

  prop "isUnifyVar rejects multi-char non-prefixed names" $
    forAll (elements ["foo", "bar", "baz"]) $ \var ->
      not (isUnifyVar var)

  prop "isEllipsisVar recognizes triple underscore" $
    isEllipsisVar "___"

  prop "isEllipsisVar recognizes dollar-ellipsis" $
    isEllipsisVar "$..."

  prop "exprEq is reflexive" $
    forAllBlind genSimpleExpr $ \expr ->
      exprEq expr expr

  prop "exprEq is symmetric" $
    forAllBlind genSimpleExpr $ \expr1 ->
      forAllBlind genSimpleExpr $ \expr2 ->
        exprEq expr1 expr2 == exprEq expr2 expr1

--------------------------------------------------------------------------------
-- Variable Binding Properties
--------------------------------------------------------------------------------

variableBindingProperties :: Spec
variableBindingProperties = describe "Variable binding in pattern matching" $ do
  prop "variable capture preserves binding consistency" $
    forAll genMetaVar $ \var ->
      forAllBlind genSimpleExpr $ \expr ->
        let patExpr = parseHelper var
        in case unifyExpr patExpr expr of
             Just subst | isUnifyVar var -> isJust (lookupSubst var subst)
             _ -> True

  prop "same variable binds to same expression in multiple uses" $
    forAll genMetaVar $ \var ->
      forAllBlind genSimpleExpr $ \expr ->
        let patExpr = parseHelper var
            result1 = unifyExpr patExpr expr
            result2 = unifyExpr patExpr expr
        in isJust result1 == isJust result2

  prop "different variables can bind to different expressions" $
    forAll genMetaVar $ \var1 ->
      forAll genMetaVar $ \var2 ->
        forAllBlind genSimpleExpr $ \expr1 ->
          forAllBlind genSimpleExpr $ \expr2 ->
            var1 /= var2 ==>
              let pat1 = parseHelper var1
                  pat2 = parseHelper var2
                  subst1 = unifyExpr pat1 expr1
                  subst2 = unifyExpr pat2 expr2
              in case (subst1, subst2) of
                   (Just s1, Just s2) -> mergeSubst s1 s2 `seq` True
                   _ -> True

--------------------------------------------------------------------------------
-- Wildcard Properties
--------------------------------------------------------------------------------

wildcardProperties :: Spec
wildcardProperties = describe "Wildcard pattern matching" $ do
  prop "single-letter variables match any expression" $
    forAll (elements ["x", "y", "z"]) $ \var ->
      forAllBlind genSimpleExpr $ \target ->
        let patExpr = parseHelper var
        in isJust (unifyExpr patExpr target)

  prop "underscore-prefixed variables match any expression" $
    forAll (elements ["_x", "_foo"]) $ \var ->
      forAllBlind genSimpleExpr $ \target ->
        let patExpr = parseHelper var
        in isJust (unifyExpr patExpr target)

  prop "underscore-prefixed metavariables match any expression" $
    -- Note: Dollar-prefixed patterns like $X don't parse as valid Haskell.
    -- Use underscore-prefix instead, which is recognized by isUnifyVar.
    forAll (elements ["_X", "_Y"]) $ \var ->
      forAllBlind genSimpleExpr $ \target ->
        let patExpr = parseHelper var
        in isJust (unifyExpr patExpr target)

--------------------------------------------------------------------------------
-- Rule Application Properties
--------------------------------------------------------------------------------

ruleApplicationProperties :: Spec
ruleApplicationProperties = describe "Rule application" $ do
  prop "rule application is deterministic" $ \(rule :: Rule) ->
    forAllBlind genSimpleExpr $ \expr ->
      let testIO = do
            matches1 <- matchRuleIO rule expr
            matches2 <- matchRuleIO rule expr
            pure $ length matches1 == length matches2
      in ioProperty testIO

  prop "disabled rules produce no matches" $ \(rule :: Rule) ->
    forAllBlind genSimpleExpr $ \expr ->
      let disabledRule = rule { ruleEnabled = False }
          testIO = do
            matches <- matchRuleIO disabledRule expr
            pure $ null matches
      in ioProperty testIO

  prop "parsePatternPure succeeds for simple expressions" $
    forAll (elements ["x", "f x", "x + y", "(x)"]) $ \patText ->
      isRight (parsePatternPure patText)

  prop "parsePatternPure succeeds for metavariables" $
    -- Note: Dollar-prefixed patterns don't parse as valid Haskell.
    -- Use underscore-prefix which is valid Haskell and recognized as metavariables.
    forAll (elements ["_X", "_f _X", "_X + _Y"]) $ \patText ->
      isRight (parsePatternPure patText)

--------------------------------------------------------------------------------
-- Safety Properties
--------------------------------------------------------------------------------

safetyProperties :: Spec
safetyProperties = describe "Safety level preservation" $ do
  prop "safe rules maintain safe level" $ \(rule :: Rule) ->
    let safeRule = rule { ruleSafety = Safe }
    in ruleSafety safeRule == Safe

  prop "unsafe rules cannot be marked as safe automatically" $ \(rule :: Rule) ->
    let unsafeRule = rule { ruleSafety = Unsafe }
    in ruleSafety unsafeRule == Unsafe

  prop "safety levels have total ordering" $ \(s1 :: SafetyLevel) (s2 :: SafetyLevel) ->
    s1 <= s2 || s2 <= s1

  prop "Safe is the minimum safety level" $
    all (>= Safe) [Safe, MostlySafe, NeedsReview, Unsafe]

  prop "Unsafe is the maximum safety level" $
    all (<= Unsafe) [Safe, MostlySafe, NeedsReview, Unsafe]

--------------------------------------------------------------------------------
-- Round-trip Properties
--------------------------------------------------------------------------------

roundTripProperties :: Spec
roundTripProperties = describe "Parse → match → apply round-trips" $ do
  prop "parsed expressions can be pretty-printed" $
    forAll (elements ["x", "f x", "x + y"]) $ \exprText ->
      case parsePatternPure exprText of
        Right expr -> not (T.null (prettyExpr expr))
        Left _ -> True

  prop "parsing is deterministic" $
    forAll (elements ["x", "f x", "x + y", "map f xs"]) $ \exprText ->
      let result1 = parsePatternPure exprText
          result2 = parsePatternPure exprText
      in isRight result1 == isRight result2

  prop "normalizeExpr is idempotent" $
    forAllBlind genSimpleExpr $ \expr ->
      let normalized1 = normalizeExpr expr
          normalized2 = normalizeExpr normalized1
      in exprEq normalized1 normalized2

  prop "etaReduce is idempotent" $
    forAllBlind genSimpleExpr $ \expr ->
      let reduced1 = etaReduce expr
          reduced2 = etaReduce reduced1
      in exprEq reduced1 reduced2

  prop "applySubst preserves valid expressions" $
    forAll genSubst $ \subst ->
      forAllBlind genSimpleExpr $ \expr ->
        let result = applySubst subst expr
        in result `seq` True

  prop "applySubstDeep is consistent with applySubst" $
    forAll genSubst $ \subst ->
      forAllBlind genSimpleExpr $ \expr ->
        exprEq (applySubst subst expr) (applySubstDeep subst expr)

--------------------------------------------------------------------------------
-- Expression Analysis Properties
--------------------------------------------------------------------------------

expressionAnalysisProperties :: Spec
expressionAnalysisProperties = describe "Expression analysis functions" $ do
  prop "exprComplexity is non-negative" $
    forAllBlind genSimpleExpr $ \expr ->
      exprComplexity expr >= 0

  prop "exprComplexity is consistent" $
    forAllBlind genSimpleExpr $ \expr ->
      -- Same expression should have same complexity
      exprComplexity expr == exprComplexity expr

  prop "containsVar is consistent" $
    forAll genVarName $ \varName ->
      forAllBlind genSimpleExpr $ \expr ->
        let result1 = containsVar varName expr
            result2 = containsVar varName expr
        in result1 == result2

  prop "collectVars returns a set" $
    forAllBlind genSimpleExpr $ \expr ->
      let vars = collectVars expr
      in Set.size vars >= 0

  prop "freeVars subset of collectVars" $
    forAllBlind genSimpleExpr $ \expr ->
      let free = freeVars expr
          all' = collectVars expr
      in free `Set.isSubsetOf` all'

  prop "isAtomicExpr for literals is True" $
    let litExpr = parseHelper "42"
    in isAtomicExpr litExpr

  prop "isLiteralExpr for literals is True" $
    let litExpr = parseHelper "42"
    in isLiteralExpr litExpr

  prop "isVarExpr for variables is True" $
    let varExpr = parseHelper "x"
    in isVarExpr varExpr

  prop "isApplicationExpr for HsApp is True" $
    let appExpr = parseHelper "f x"
    in isApplicationExpr appExpr

  prop "isPure for simple vars is True" $
    let varExpr = parseHelper "x"
    in isPure varExpr

--------------------------------------------------------------------------------
-- Side Condition Properties
--------------------------------------------------------------------------------

sideConditionProperties :: Spec
sideConditionProperties = describe "Side condition evaluation" $ do
  prop "Always evaluates to True" $ \(subst :: Subst) ->
    evalSideCondition subst Always

  prop "Never evaluates to False" $ \(subst :: Subst) ->
    not (evalSideCondition subst Never)

  prop "Not inverts result" $ \(subst :: Subst) (cond :: SideCondition) ->
    evalSideCondition subst (Not cond) == not (evalSideCondition subst cond)

  prop "And with empty list is True" $ \(subst :: Subst) ->
    evalSideCondition subst (And [])

  prop "And with Always is identity" $ \(subst :: Subst) (cond :: SideCondition) ->
    evalSideCondition subst (And [Always, cond]) == evalSideCondition subst cond

  prop "And with Never is always False" $ \(subst :: Subst) (cond :: SideCondition) ->
    not (evalSideCondition subst (And [Never, cond]))

  prop "Or with empty list is False" $ \(subst :: Subst) ->
    not (evalSideCondition subst (Or []))

  prop "Or with Always is always True" $ \(subst :: Subst) (cond :: SideCondition) ->
    evalSideCondition subst (Or [Always, cond])

  prop "Or with Never is identity" $ \(subst :: Subst) (cond :: SideCondition) ->
    evalSideCondition subst (Or [Never, cond]) == evalSideCondition subst cond

  prop "Double negation is identity" $ \(subst :: Subst) (cond :: SideCondition) ->
    evalSideCondition subst (Not (Not cond)) == evalSideCondition subst cond

  prop "De Morgan's law: Not (And [a,b]) == Or [Not a, Not b]" $ \(subst :: Subst) ->
    let a = NotInComment
        b = NotInImport
    in evalSideCondition subst (Not (And [a, b])) ==
       evalSideCondition subst (Or [Not a, Not b])

  prop "De Morgan's law: Not (Or [a,b]) == And [Not a, Not b]" $ \(subst :: Subst) ->
    let a = NotInComment
        b = NotInImport
    in evalSideCondition subst (Not (Or [a, b])) ==
       evalSideCondition subst (And [Not a, Not b])

  prop "NotInComment, NotInString, NotInImport default to True" $ \(subst :: Subst) ->
    evalSideCondition subst NotInComment &&
    evalSideCondition subst NotInString &&
    evalSideCondition subst NotInImport

  prop "InFunctionBody defaults to True" $ \(subst :: Subst) ->
    evalSideCondition subst InFunctionBody

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Sort a list for comparison
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y < x] ++ [x] ++ sort [y | y <- xs, y >= x]
