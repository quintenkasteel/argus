{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : RuleOutputSpec
-- Description : Tests for validating rule output/fix correctness
--
-- These tests validate that:
-- 1. Rules generate correct fixes with proper replacements
-- 2. Metavariables are properly interpolated in replacements
-- 3. The fixes produce valid Haskell code when applied
-- 4. The semantic meaning is preserved after transformation
module RuleOutputSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (isJust, listToMaybe)

import Argus.Types (Diagnostic(..), Fix(..), FixEdit(..))
import Argus.Rules.Engine (mkRuleEngine, evaluateRules)
import Argus.Refactor.ExactPrint (applyFix)
import Argus.Refactor.Validation (validateSyntax)

-- Import builtin rules
import Argus.Rules.Builtin.List
import Argus.Rules.Builtin.Boolean
import Argus.Rules.Builtin.Maybe
import Argus.Rules.Builtin.Monadic

spec :: Spec
spec = do
  describe "Rule Output Validation" $ do
    listRuleOutputSpec
    booleanRuleOutputSpec
    maybeRuleOutputSpec
    monadicRuleOutputSpec
    metavariableInterpolationSpec

--------------------------------------------------------------------------------
-- List Rule Output Validation
--------------------------------------------------------------------------------

listRuleOutputSpec :: Spec
listRuleOutputSpec = describe "List Rule Outputs" $ do
  describe "concatMapRule" $ do
    let engine = mkRuleEngine [concatMapRule]

    it "generates correct fix for 'concat (map f xs)'" $ do
      let code = "result = concat (map f xs)"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fix = getFirstFix diags
      fix `shouldSatisfy` isJust
      let fixText = getFixReplacement fix
      fixText `shouldBe` Just "concatMap f xs"

    it "fix produces valid syntax" $ do
      let code = "module Test where\nresult = concat (map f xs)"
          diags = evaluateRules engine "Test.hs" "Test" code
          transformed = applyFirstFix code diags
      result <- validateSyntax "Test.hs" transformed
      result `shouldBe` Right ()
      T.isInfixOf "concatMap f xs" transformed `shouldBe` True

  describe "headReverse" $ do
    let engine = mkRuleEngine [headReverse]

    it "generates correct fix for 'head (reverse xs)'" $ do
      let code = "result = head (reverse xs)"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "last xs"

    it "fix produces valid syntax" $ do
      let code = "module Test where\nresult = head (reverse xs)"
          diags = evaluateRules engine "Test.hs" "Test" code
          transformed = applyFirstFix code diags
      result <- validateSyntax "Test.hs" transformed
      result `shouldBe` Right ()
      T.isInfixOf "last xs" transformed `shouldBe` True

  describe "lastReverse" $ do
    let engine = mkRuleEngine [lastReverse]

    it "generates correct fix for 'last (reverse xs)'" $ do
      let code = "result = last (reverse xs)"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "head xs"

  describe "lengthZero" $ do
    let engine = mkRuleEngine [lengthZero]

    it "generates correct fix for 'length xs == 0'" $ do
      let code = "result = length xs == 0"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "null xs"

  describe "takeRepeat" $ do
    let engine = mkRuleEngine [takeRepeat]

    it "generates correct fix for 'take n (repeat x)'" $ do
      let code = "result = take n (repeat x)"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "replicate n x"

--------------------------------------------------------------------------------
-- Boolean Rule Output Validation
--------------------------------------------------------------------------------

booleanRuleOutputSpec :: Spec
booleanRuleOutputSpec = describe "Boolean Rule Outputs" $ do
  describe "notTrue" $ do
    let engine = mkRuleEngine [notTrue]

    it "generates correct fix for 'not True'" $ do
      let code = "result = not True"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "False"

    it "fix produces valid syntax" $ do
      let code = "module Test where\nresult = not True"
          diags = evaluateRules engine "Test.hs" "Test" code
          transformed = applyFirstFix code diags
      result <- validateSyntax "Test.hs" transformed
      result `shouldBe` Right ()
      T.isInfixOf "result = False" transformed `shouldBe` True

  describe "notFalse" $ do
    let engine = mkRuleEngine [notFalse]

    it "generates correct fix for 'not False'" $ do
      let code = "result = not False"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "True"

  describe "trueAnd" $ do
    let engine = mkRuleEngine [trueAnd]

    it "generates correct fix for 'True && x'" $ do
      let code = "result = True && x"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "x"

  describe "falseOr" $ do
    let engine = mkRuleEngine [falseOr]

    it "generates correct fix for 'False || x'" $ do
      let code = "result = False || x"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "x"

  describe "eqTrue" $ do
    let engine = mkRuleEngine [eqTrue]

    it "generates correct fix for 'x == True'" $ do
      let code = "result = x == True"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "x"

  describe "eqFalse" $ do
    let engine = mkRuleEngine [eqFalse]

    it "generates correct fix for 'x == False'" $ do
      let code = "result = x == False"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "not x"

--------------------------------------------------------------------------------
-- Maybe Rule Output Validation
--------------------------------------------------------------------------------

maybeRuleOutputSpec :: Spec
maybeRuleOutputSpec = describe "Maybe Rule Outputs" $ do
  describe "fmapJust" $ do
    let engine = mkRuleEngine [fmapJust]

    it "generates correct fix for 'fmap f (Just x)'" $ do
      let code = "result = fmap f (Just x)"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "Just (f x)"

    it "fix produces valid syntax" $ do
      let code = "module Test where\nresult = fmap f (Just x)"
          diags = evaluateRules engine "Test.hs" "Test" code
          transformed = applyFirstFix code diags
      result <- validateSyntax "Test.hs" transformed
      result `shouldBe` Right ()
      T.isInfixOf "Just (f x)" transformed `shouldBe` True

  describe "fmapNothing" $ do
    let engine = mkRuleEngine [fmapNothing]

    it "generates correct fix for 'fmap f Nothing'" $ do
      let code = "result = fmap f Nothing"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "Nothing"

  describe "isJustNothing" $ do
    let engine = mkRuleEngine [isJustNothing]

    it "generates correct fix for 'isJust Nothing'" $ do
      let code = "result = isJust Nothing"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "False"

  describe "isNothingJust" $ do
    let engine = mkRuleEngine [isNothingJust]

    it "generates correct fix for 'isNothing (Just x)'" $ do
      let code = "result = isNothing (Just x)"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "False"

  describe "listToMaybeEmpty" $ do
    let engine = mkRuleEngine [listToMaybeEmpty]

    it "generates correct fix for 'listToMaybe []'" $ do
      let code = "result = listToMaybe []"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "Nothing"

  describe "maybeToListNothing" $ do
    let engine = mkRuleEngine [maybeToListNothing]

    it "generates correct fix for 'maybeToList Nothing'" $ do
      let code = "result = maybeToList Nothing"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "[]"

--------------------------------------------------------------------------------
-- Monadic Rule Output Validation
--------------------------------------------------------------------------------

monadicRuleOutputSpec :: Spec
monadicRuleOutputSpec = describe "Monadic Rule Outputs" $ do
  describe "returnToPure" $ do
    let engine = mkRuleEngine [returnToPure]

    it "generates correct fix for 'return x'" $ do
      let code = "result = return x"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "pure x"

    it "fix produces valid syntax" $ do
      let code = "module Test where\nresult = return x"
          diags = evaluateRules engine "Test.hs" "Test" code
          transformed = applyFirstFix code diags
      result <- validateSyntax "Test.hs" transformed
      result `shouldBe` Right ()
      T.isInfixOf "pure x" transformed `shouldBe` True

  describe "liftMToFmap" $ do
    let engine = mkRuleEngine [liftMToFmap]

    it "generates correct fix for 'liftM f m'" $ do
      let code = "result = liftM f m"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "fmap f m"

  describe "mapMToTraverse" $ do
    let engine = mkRuleEngine [mapMToTraverse]

    it "generates correct fix for 'mapM f xs'" $ do
      let code = "result = mapM f xs"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "traverse f xs"

  describe "liftM2ToLiftA2" $ do
    let engine = mkRuleEngine [liftM2ToLiftA2]

    it "generates correct fix for 'liftM2 f m1 m2'" $ do
      let code = "result = liftM2 f m1 m2"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "liftA2 f m1 m2"

  describe "sequenceToSequenceA" $ do
    let engine = mkRuleEngine [sequenceToSequenceA]

    it "generates correct fix for 'sequence xs'" $ do
      let code = "result = sequence xs"
          diags = evaluateRules engine "Test.hs" "Test" code
      length diags `shouldBe` 1
      let fixText = getFixReplacement (getFirstFix diags)
      fixText `shouldBe` Just "sequenceA xs"

--------------------------------------------------------------------------------
-- Metavariable Interpolation Tests
--------------------------------------------------------------------------------

metavariableInterpolationSpec :: Spec
metavariableInterpolationSpec = describe "Metavariable Interpolation" $ do
  it "correctly interpolates single metavariable" $ do
    let engine = mkRuleEngine [headReverse]
        code = "result = head (reverse myList)"
        diags = evaluateRules engine "Test.hs" "Test" code
        fixText = getFixReplacement (getFirstFix diags)
    -- The replacement should use the actual captured value
    fixText `shouldBe` Just "last myList"

  it "correctly interpolates multiple metavariables" $ do
    let engine = mkRuleEngine [concatMapRule]
        code = "result = concat (map myFunc myData)"
        diags = evaluateRules engine "Test.hs" "Test" code
        fixText = getFixReplacement (getFirstFix diags)
    fixText `shouldBe` Just "concatMap myFunc myData"

  it "correctly interpolates in takeRepeat" $ do
    let engine = mkRuleEngine [takeRepeat]
        code = "result = take count (repeat value)"
        diags = evaluateRules engine "Test.hs" "Test" code
        fixText = getFixReplacement (getFirstFix diags)
    fixText `shouldBe` Just "replicate count value"

  it "preserves identifier names in fmap transformation" $ do
    let engine = mkRuleEngine [fmapJust]
        code = "result = fmap transform (Just initial)"
        diags = evaluateRules engine "Test.hs" "Test" code
        fixText = getFixReplacement (getFirstFix diags)
    fixText `shouldBe` Just "Just (transform initial)"

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Get the first fix from a list of diagnostics
getFirstFix :: [Diagnostic] -> Maybe Fix
getFirstFix diags = listToMaybe (concatMap diagFixes diags)

-- | Get the replacement text from a fix
getFixReplacement :: Maybe Fix -> Maybe Text
getFixReplacement = \case
  Just fix -> case fixEdits fix of
    [FixEdit _ replacement] -> Just replacement
    _ -> Nothing
  Nothing -> Nothing

-- | Apply the first fix to source code
applyFirstFix :: Text -> [Diagnostic] -> Text
applyFirstFix source diags = case getFirstFix diags of
  Just fix -> applyFix source fix
  Nothing -> source
