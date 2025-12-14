{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : BuiltinRuleModulesSpec
-- Description : Comprehensive tests for all 39 builtin rule modules
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Tests every individual builtin rule module in src/Argus/Rules/Builtin/
-- to ensure all rules are properly defined, have valid IDs and messages,
-- and their rule counts are accurate.
--
-- This test suite covers all 39 builtin modules:
-- Arrow, Async, Boolean, Brackets, CodeSmells, Comparison, Composition,
-- Containers, Correctness, Deriving, Either, FFI, Foldable, GADTs, Imports,
-- IO, Lambda, Lenses, List, ListRecursion, Maybe, Monadic, Monoid, Numeric,
-- OWASP, Pattern, Performance, Prelude, Records, Safety, Security, Strings,
-- Style, Testing, Transformers, Tuple, TypeAnnotations, TypeclassLaws,
-- TypeFamilies
module BuiltinRuleModulesSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (isJust, isNothing)

import Argus.Rules.DSL (Rule(..), Category(..), SafetyLevel(..))
import Argus.Rules.Types (rulePatternToText)
import Argus.Types (Severity(..), Diagnostic(..), Fix(..), FixEdit(..))

-- Import all 39 builtin rule modules
import Argus.Rules.Builtin.Arrow qualified as Arrow
import Argus.Rules.Builtin.Async qualified as Async
import Argus.Rules.Builtin.Boolean qualified as Boolean
import Argus.Rules.Builtin.Brackets qualified as Brackets
import Argus.Rules.Builtin.CodeSmells qualified as CodeSmells
import Argus.Rules.Builtin.Comparison qualified as Comparison
import Argus.Rules.Builtin.Composition qualified as Composition
import Argus.Rules.Builtin.Containers qualified as Containers
import Argus.Rules.Builtin.Correctness qualified as Correctness
import Argus.Rules.Builtin.Deriving qualified as Deriving
import Argus.Rules.Builtin.Either qualified as Either
import Argus.Rules.Builtin.FFI qualified as FFI
import Argus.Rules.Builtin.Foldable qualified as Foldable
import Argus.Rules.Builtin.GADTs qualified as GADTs
import Argus.Rules.Builtin.Imports qualified as Imports
import Argus.Rules.Builtin.IO qualified as IO
import Argus.Rules.Builtin.Lambda qualified as Lambda
import Argus.Rules.Builtin.Lenses qualified as Lenses
import Argus.Rules.Builtin.List qualified as List
import Argus.Rules.Builtin.ListRecursion qualified as ListRecursion
import Argus.Rules.Builtin.Maybe qualified as Maybe
import Argus.Rules.Builtin.Monadic qualified as Monadic
import Argus.Rules.Builtin.Monoid qualified as Monoid
import Argus.Rules.Builtin.Numeric qualified as Numeric
import Argus.Rules.Builtin.OWASP qualified as OWASP
import Argus.Rules.Builtin.Pattern qualified as Pattern
import Argus.Rules.Builtin.Performance qualified as Performance
import Argus.Rules.Builtin.Prelude qualified as Prelude
import Argus.Rules.Builtin.Records qualified as Records
import Argus.Rules.Builtin.Safety qualified as Safety
import Argus.Rules.Builtin.Security qualified as Security
import Argus.Rules.Builtin.Strings qualified as Strings
import Argus.Rules.Builtin.Style qualified as Style
import Argus.Rules.Builtin.Testing qualified as Testing
import Argus.Rules.Builtin.Transformers qualified as Transformers
import Argus.Rules.Builtin.Tuple qualified as Tuple
import Argus.Rules.Builtin.TypeAnnotations qualified as TypeAnnotations
import Argus.Rules.Builtin.TypeclassLaws qualified as TypeclassLaws
import Argus.Rules.Builtin.TypeFamilies qualified as TypeFamilies

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

-- | Test that a rule list is non-empty
shouldBeNonEmpty :: [Rule] -> Expectation
shouldBeNonEmpty rules = rules `shouldSatisfy` (not . null)

-- | Test that all rules in a list have non-empty IDs
allShouldHaveIds :: [Rule] -> Expectation
allShouldHaveIds rules =
  all (not . T.null . ruleId) rules `shouldBe` True

-- | Test that all rules in a list have non-empty messages
allShouldHaveMessages :: [Rule] -> Expectation
allShouldHaveMessages rules =
  all (not . T.null . ruleMessage) rules `shouldBe` True

-- | Test that a rule has a valid pattern (non-empty)
hasValidPattern :: Rule -> Bool
hasValidPattern rule =
  not (T.null (rulePatternToText (rulePattern rule)))

-- | Test that all rules have valid patterns
allShouldHavePatterns :: [Rule] -> Expectation
allShouldHavePatterns rules =
  all hasValidPattern rules `shouldBe` True

-- | Test that rule count matches actual list length
countShouldMatch :: Int -> [Rule] -> Expectation
countShouldMatch expected rules =
  length rules `shouldBe` expected

-- | Test that a specific rule has the expected ID
shouldHaveRuleId :: Rule -> Text -> Expectation
shouldHaveRuleId rule expectedId =
  ruleId rule `shouldBe` expectedId

-- | Test that a specific rule has the expected category
shouldHaveCategory :: Rule -> Category -> Expectation
shouldHaveCategory rule expectedCat =
  ruleCategory rule `shouldBe` expectedCat

-- | Generic test for a module's rule structure
testModuleStructure
  :: String          -- ^ Module name for description
  -> [Rule]          -- ^ Rule list
  -> Maybe Int       -- ^ Optional expected count
  -> Spec
testModuleStructure name rules mExpectedCount = describe "module structure" $ do
  it "has non-empty rule list" $
    shouldBeNonEmpty rules

  it "all rules have IDs" $
    allShouldHaveIds rules

  it "all rules have messages" $
    allShouldHaveMessages rules

  it "all rules have valid patterns" $
    allShouldHavePatterns rules

  case mExpectedCount of
    Just expectedCount -> it "rule count matches length" $
      countShouldMatch expectedCount rules
    Nothing -> pure ()

--------------------------------------------------------------------------------
-- Main Test Suite
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Argus.Rules.Builtin.Arrow" arrowSpec
  describe "Argus.Rules.Builtin.Async" asyncSpec
  describe "Argus.Rules.Builtin.Boolean" booleanSpec
  describe "Argus.Rules.Builtin.Brackets" bracketsSpec
  describe "Argus.Rules.Builtin.CodeSmells" codeSmellsSpec
  describe "Argus.Rules.Builtin.Comparison" comparisonSpec
  describe "Argus.Rules.Builtin.Composition" compositionSpec
  describe "Argus.Rules.Builtin.Containers" containersSpec
  describe "Argus.Rules.Builtin.Correctness" correctnessSpec
  describe "Argus.Rules.Builtin.Deriving" derivingSpec
  describe "Argus.Rules.Builtin.Either" eitherSpec
  describe "Argus.Rules.Builtin.FFI" ffiSpec
  describe "Argus.Rules.Builtin.Foldable" foldableSpec
  describe "Argus.Rules.Builtin.GADTs" gadtsSpec
  describe "Argus.Rules.Builtin.Imports" importsSpec
  describe "Argus.Rules.Builtin.IO" ioSpec
  describe "Argus.Rules.Builtin.Lambda" lambdaSpec
  describe "Argus.Rules.Builtin.Lenses" lensesSpec
  describe "Argus.Rules.Builtin.List" listSpec
  describe "Argus.Rules.Builtin.ListRecursion" listRecursionSpec
  describe "Argus.Rules.Builtin.Maybe" maybeSpec
  describe "Argus.Rules.Builtin.Monadic" monadicSpec
  describe "Argus.Rules.Builtin.Monoid" monoidSpec
  describe "Argus.Rules.Builtin.Numeric" numericSpec
  describe "Argus.Rules.Builtin.OWASP" owaspSpec
  describe "Argus.Rules.Builtin.Pattern" patternSpec
  describe "Argus.Rules.Builtin.Performance" performanceSpec
  describe "Argus.Rules.Builtin.Prelude" preludeSpec
  describe "Argus.Rules.Builtin.Records" recordsSpec
  describe "Argus.Rules.Builtin.Safety" safetySpec
  describe "Argus.Rules.Builtin.Security" securitySpec
  describe "Argus.Rules.Builtin.Strings" stringsSpec
  describe "Argus.Rules.Builtin.Style" styleSpec
  describe "Argus.Rules.Builtin.Testing" testingSpec
  describe "Argus.Rules.Builtin.Transformers" transformersSpec
  describe "Argus.Rules.Builtin.Tuple" tupleSpec
  describe "Argus.Rules.Builtin.TypeAnnotations" typeAnnotationsSpec
  describe "Argus.Rules.Builtin.TypeclassLaws" typeclassLawsSpec
  describe "Argus.Rules.Builtin.TypeFamilies" typeFamiliesSpec

--------------------------------------------------------------------------------
-- Arrow Module Tests
--------------------------------------------------------------------------------

arrowSpec :: Spec
arrowSpec = do
  testModuleStructure "Arrow" Arrow.arrowRules (Just Arrow.arrowRuleCount)

  describe "individual rules" $ do
    it "arrId is defined" $
      Arrow.arrId `shouldSatisfy` hasValidPattern

    it "arrCompose is defined" $
      Arrow.arrCompose `shouldSatisfy` hasValidPattern

    it "arrowFirstId is defined" $
      Arrow.arrowFirstId `shouldSatisfy` hasValidPattern

    it "arrowSecondId is defined" $
      Arrow.arrowSecondId `shouldSatisfy` hasValidPattern

    it "firstSecond is defined" $
      Arrow.firstSecond `shouldSatisfy` hasValidPattern

    it "secondFirst is defined" $
      Arrow.secondFirst `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Async Module Tests
--------------------------------------------------------------------------------

asyncSpec :: Spec
asyncSpec = do
  testModuleStructure "Async" Async.asyncRules (Just Async.asyncRuleCount)

  describe "individual rules" $ do
    it "asyncPattern is defined" $
      Async.asyncPattern `shouldSatisfy` hasValidPattern

    it "asyncWait is defined" $
      Async.asyncWait `shouldSatisfy` hasValidPattern

    it "asyncCancel is defined" $
      Async.asyncCancel `shouldSatisfy` hasValidPattern

    it "withAsyncPattern is defined" $
      Async.withAsyncPattern `shouldSatisfy` hasValidPattern

    it "concurrently is defined" $
      Async.concurrently `shouldSatisfy` hasValidPattern

    it "race is defined" $
      Async.race `shouldSatisfy` hasValidPattern

  describe "stm rules" $ do
    it "atomically is defined" $
      Async.atomically `shouldSatisfy` hasValidPattern

    it "stmRetry is defined" $
      Async.stmRetry `shouldSatisfy` hasValidPattern

    it "stmOrElse is defined" $
      Async.stmOrElse `shouldSatisfy` hasValidPattern

  describe "mvar rules" $ do
    it "mvarNew is defined" $
      Async.mvarNew `shouldSatisfy` hasValidPattern

    it "mvarTake is defined" $
      Async.mvarTake `shouldSatisfy` hasValidPattern

    it "mvarPut is defined" $
      Async.mvarPut `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Boolean Module Tests
--------------------------------------------------------------------------------

booleanSpec :: Spec
booleanSpec = do
  testModuleStructure "Boolean" Boolean.boolSimplifyRules (Just Boolean.booleanRuleCount)

  describe "basic boolean rules" $ do
    it "notTrue is defined" $
      Boolean.notTrue `shouldSatisfy` hasValidPattern

    it "notFalse is defined" $
      Boolean.notFalse `shouldSatisfy` hasValidPattern

    it "trueAnd is defined" $
      Boolean.trueAnd `shouldSatisfy` hasValidPattern

    it "falseAnd is defined" $
      Boolean.falseAnd `shouldSatisfy` hasValidPattern

    it "trueOr is defined" $
      Boolean.trueOr `shouldSatisfy` hasValidPattern

    it "falseOr is defined" $
      Boolean.falseOr `shouldSatisfy` hasValidPattern

  describe "boolean comparison rules" $ do
    it "eqTrue is defined" $
      Boolean.eqTrue `shouldSatisfy` hasValidPattern

    it "eqFalse is defined" $
      Boolean.eqFalse `shouldSatisfy` hasValidPattern

    it "neTrue is defined" $
      Boolean.neTrue `shouldSatisfy` hasValidPattern

    it "neFalse is defined" $
      Boolean.neFalse `shouldSatisfy` hasValidPattern

  describe "if-then-else rules" $ do
    it "ifTrue is defined" $
      Boolean.ifTrue `shouldSatisfy` hasValidPattern

    it "ifFalse is defined" $
      Boolean.ifFalse `shouldSatisfy` hasValidPattern

    it "ifCondTrue is defined" $
      Boolean.ifCondTrue `shouldSatisfy` hasValidPattern

    it "ifCondFalse is defined" $
      Boolean.ifCondFalse `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Brackets Module Tests
--------------------------------------------------------------------------------

bracketsSpec :: Spec
bracketsSpec = do
  testModuleStructure "Brackets" Brackets.bracketRules (Just Brackets.bracketRuleCount)

  describe "redundant paren rules" $ do
    it "redundantOuterParen is defined" $
      Brackets.redundantOuterParen `shouldSatisfy` hasValidPattern

    it "redundantInnerParen is defined" $
      Brackets.redundantInnerParen `shouldSatisfy` hasValidPattern

    it "redundantParenInList is defined" $
      Brackets.redundantParenInList `shouldSatisfy` hasValidPattern

    it "redundantParenInTuple is defined" $
      Brackets.redundantParenInTuple `shouldSatisfy` hasValidPattern

    it "redundantParenAroundLit is defined" $
      Brackets.redundantParenAroundLit `shouldSatisfy` hasValidPattern

  describe "dollar rules" $ do
    it "dollarInsteadOfParen is defined" $
      Brackets.dollarInsteadOfParen `shouldSatisfy` hasValidPattern

    it "redundantDollar is defined" $
      Brackets.redundantDollar `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- CodeSmells Module Tests
--------------------------------------------------------------------------------

codeSmellsSpec :: Spec
codeSmellsSpec = do
  testModuleStructure "CodeSmells" CodeSmells.codeSmellRules (Just CodeSmells.codeSmellRuleCount)

  describe "bloater rules" $ do
    it "longFunction is defined" $
      CodeSmells.longFunction `shouldSatisfy` hasValidPattern

    it "longParameterList is defined" $
      CodeSmells.longParameterList `shouldSatisfy` hasValidPattern

    it "deepNesting is defined" $
      CodeSmells.deepNesting `shouldSatisfy` hasValidPattern

    it "largeCase is defined" $
      CodeSmells.largeCase `shouldSatisfy` hasValidPattern

    it "godModule is defined" $
      CodeSmells.godModule `shouldSatisfy` hasValidPattern

  describe "coupler rules" $ do
    it "featureEnvy is defined" $
      CodeSmells.featureEnvy `shouldSatisfy` hasValidPattern

    it "messageChain is defined" $
      CodeSmells.messageChain `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Comparison Module Tests
--------------------------------------------------------------------------------

comparisonSpec :: Spec
comparisonSpec = do
  testModuleStructure "Comparison" Comparison.ordComparisonRules (Just Comparison.comparisonRuleCount)

  describe "basic comparison rules" $ do
    it "eqSelf is defined" $
      Comparison.eqSelf `shouldSatisfy` hasValidPattern

    it "neSelf is defined" $
      Comparison.neSelf `shouldSatisfy` hasValidPattern

    it "ltSelf is defined" $
      Comparison.ltSelf `shouldSatisfy` hasValidPattern

    it "gtSelf is defined" $
      Comparison.gtSelf `shouldSatisfy` hasValidPattern

  describe "ordering rules" $ do
    it "compareLt is defined" $
      Comparison.compareLt `shouldSatisfy` hasValidPattern

    it "compareGt is defined" $
      Comparison.compareGt `shouldSatisfy` hasValidPattern

    it "compareEq is defined" $
      Comparison.compareEq `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Composition Module Tests
--------------------------------------------------------------------------------

compositionSpec :: Spec
compositionSpec = do
  testModuleStructure "Composition" Composition.compositionRules (Just Composition.compositionRuleCount)

  describe "compose rules" $ do
    it "idCompose is defined" $
      Composition.idCompose `shouldSatisfy` hasValidPattern

    it "composeId is defined" $
      Composition.composeId `shouldSatisfy` hasValidPattern

    it "composeConst is defined" $
      Composition.composeConst `shouldSatisfy` hasValidPattern

  describe "flip and application rules" $ do
    it "flipFlip is defined" $
      Composition.flipFlip `shouldSatisfy` hasValidPattern

    it "flipConst is defined" $
      Composition.flipConst `shouldSatisfy` hasValidPattern

    it "dollarApply is defined" $
      Composition.dollarApply `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Containers Module Tests
--------------------------------------------------------------------------------

containersSpec :: Spec
containersSpec = do
  testModuleStructure "Containers" Containers.containerRules (Just Containers.containerRuleCount)

  describe "list container rules" $ do
    it "listHeadTail is defined" $
      Containers.listHeadTail `shouldSatisfy` hasValidPattern

    it "listAppendSingleton is defined" $
      Containers.listAppendSingleton `shouldSatisfy` hasValidPattern

    it "listReverse is defined" $
      Containers.listReverse `shouldSatisfy` hasValidPattern

  describe "map container rules" $ do
    it "mapFromList is defined" $
      Containers.mapFromList `shouldSatisfy` hasValidPattern

    it "mapToList is defined" $
      Containers.mapToList `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Correctness Module Tests
--------------------------------------------------------------------------------

correctnessSpec :: Spec
correctnessSpec = do
  testModuleStructure "Correctness" Correctness.correctnessRules (Just Correctness.correctnessRuleCount)

  describe "equality rules" $ do
    it "avoidFloatEq is defined" $
      Correctness.avoidFloatEq `shouldSatisfy` hasValidPattern

    it "avoidDoubleEq is defined" $
      Correctness.avoidDoubleEq `shouldSatisfy` hasValidPattern

  describe "monad rules" $ do
    it "monadLeftIdentity is defined" $
      Correctness.monadLeftIdentity `shouldSatisfy` hasValidPattern

    it "monadRightIdentity is defined" $
      Correctness.monadRightIdentity `shouldSatisfy` hasValidPattern

  describe "functor rules" $ do
    it "funmapId is defined" $
      Correctness.funmapId `shouldSatisfy` hasValidPattern

    it "functorComposition is defined" $
      Correctness.functorComposition `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Deriving Module Tests
--------------------------------------------------------------------------------

derivingSpec :: Spec
derivingSpec = do
  testModuleStructure "Deriving" Deriving.derivingRules (Just Deriving.derivingRuleCount)

  describe "strategy rules" $ do
    it "missingStrategy is defined" $
      Deriving.missingStrategy `shouldSatisfy` hasValidPattern

    it "stockStrategy is defined" $
      Deriving.stockStrategy `shouldSatisfy` hasValidPattern

    it "newtypeStrategy is defined" $
      Deriving.newtypeStrategy `shouldSatisfy` hasValidPattern

  describe "stock rules" $ do
    it "deriveEq is defined" $
      Deriving.deriveEq `shouldSatisfy` hasValidPattern

    it "deriveOrd is defined" $
      Deriving.deriveOrd `shouldSatisfy` hasValidPattern

    it "deriveShow is defined" $
      Deriving.deriveShow `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Either Module Tests
--------------------------------------------------------------------------------

eitherSpec :: Spec
eitherSpec = do
  testModuleStructure "Either" Either.eitherRules (Just Either.eitherRuleCount)

  describe "simplification rules" $ do
    it "eitherLeftRight is defined" $
      Either.eitherLeftRight `shouldSatisfy` hasValidPattern

    it "eitherIdId is defined" $
      Either.eitherIdId `shouldSatisfy` hasValidPattern

  describe "pattern rules" $ do
    it "isLeftLeft is defined" $
      Either.isLeftLeft `shouldSatisfy` hasValidPattern

    it "isRightRight is defined" $
      Either.isRightRight `shouldSatisfy` hasValidPattern

    it "fromLeftLeft is defined" $
      Either.fromLeftLeft `shouldSatisfy` hasValidPattern

    it "fromLeftRight is defined" $
      Either.fromLeftRight `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- FFI Module Tests
--------------------------------------------------------------------------------

ffiSpec :: Spec
ffiSpec = do
  testModuleStructure "FFI" FFI.ffiRules (Just FFI.ffiRuleCount)

  describe "foreign import rules" $ do
    it "foreignImportSafe is defined" $
      FFI.foreignImportSafe `shouldSatisfy` hasValidPattern

    it "foreignImportUnsafe is defined" $
      FFI.foreignImportUnsafe `shouldSatisfy` hasValidPattern

    it "foreignImportCapi is defined" $
      FFI.foreignImportCapi `shouldSatisfy` hasValidPattern

  describe "unsafe FFI rules" $ do
    it "unsafePerformIO is defined" $
      FFI.unsafePerformIO `shouldSatisfy` hasValidPattern

    it "unsafeInterleaveIO is defined" $
      FFI.unsafeInterleaveIO `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Foldable Module Tests
--------------------------------------------------------------------------------

foldableSpec :: Spec
foldableSpec = do
  testModuleStructure "Foldable" Foldable.foldableRules (Just Foldable.foldableRuleCount)

  describe "fold optimization rules" $ do
    it "foldlToFoldl' is defined" $
      Foldable.foldlToFoldl' `shouldSatisfy` hasValidPattern

    it "foldrToFoldl' is defined" $
      Foldable.foldrToFoldl' `shouldSatisfy` hasValidPattern

    it "foldMapToFold is defined" $
      Foldable.foldMapToFold `shouldSatisfy` hasValidPattern

  describe "specialized rules" $ do
    it "anyToOr is defined" $
      Foldable.anyToOr `shouldSatisfy` hasValidPattern

    it "allToAnd is defined" $
      Foldable.allToAnd `shouldSatisfy` hasValidPattern

    it "lengthToFold is defined" $
      Foldable.lengthToFold `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- GADTs Module Tests
--------------------------------------------------------------------------------

gadtsSpec :: Spec
gadtsSpec = do
  testModuleStructure "GADTs" GADTs.gadtRules (Just GADTs.gadtRuleCount)

  describe "gadt declaration rules" $ do
    it "gadtSyntax is defined" $
      GADTs.gadtSyntax `shouldSatisfy` hasValidPattern

    it "gadtPattern is defined" $
      GADTs.gadtPattern `shouldSatisfy` hasValidPattern

    it "gadtReturn is defined" $
      GADTs.gadtReturn `shouldSatisfy` hasValidPattern

  describe "existential rules" $ do
    it "existentialData is defined" $
      GADTs.existentialData `shouldSatisfy` hasValidPattern

    it "existentialRecord is defined" $
      GADTs.existentialRecord `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Imports Module Tests
--------------------------------------------------------------------------------

importsSpec :: Spec
importsSpec = do
  testModuleStructure "Imports" Imports.importRules (Just Imports.importRuleCount)

  describe "import optimization rules" $ do
    it "duplicateImport is defined" $
      Imports.duplicateImport `shouldSatisfy` hasValidPattern

    it "emptyImport is defined" $
      Imports.emptyImport `shouldSatisfy` hasValidPattern

    it "redundantQualified is defined" $
      Imports.redundantQualified `shouldSatisfy` hasValidPattern

  describe "extension suggestion rules" $ do
    it "lambdaCaseSuggest is defined" $
      Imports.lambdaCaseSuggest `shouldSatisfy` hasValidPattern

    it "multiWayIfSuggest is defined" $
      Imports.multiWayIfSuggest `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- IO Module Tests
--------------------------------------------------------------------------------

ioSpec :: Spec
ioSpec = do
  testModuleStructure "IO" IO.ioRules (Just IO.ioRuleCount)

  describe "file handling rules" $ do
    it "readFileStrict is defined" $
      IO.readFileStrict `shouldSatisfy` hasValidPattern

    it "writeFileAtomic is defined" $
      IO.writeFileAtomic `shouldSatisfy` hasValidPattern

    it "withFileBracket is defined" $
      IO.withFileBracket `shouldSatisfy` hasValidPattern

  describe "resource management rules" $ do
    it "bracketPattern is defined" $
      IO.bracketPattern `shouldSatisfy` hasValidPattern

    it "bracketOnError is defined" $
      IO.bracketOnError `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Lambda Module Tests
--------------------------------------------------------------------------------

lambdaSpec :: Spec
lambdaSpec = do
  testModuleStructure "Lambda" Lambda.lambdaRules (Just Lambda.lambdaRuleCount)

  describe "eta rules" $ do
    it "etaReduceSimple is defined" $
      Lambda.etaReduceSimple `shouldSatisfy` hasValidPattern

    it "etaReduceOperator is defined" $
      Lambda.etaReduceOperator `shouldSatisfy` hasValidPattern

    it "etaReduceFlip is defined" $
      Lambda.etaReduceFlip `shouldSatisfy` hasValidPattern

  describe "tuple section rules" $ do
    it "tupleSectionLeft is defined" $
      Lambda.tupleSectionLeft `shouldSatisfy` hasValidPattern

    it "tupleSectionRight is defined" $
      Lambda.tupleSectionRight `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Lenses Module Tests
--------------------------------------------------------------------------------

lensesSpec :: Spec
lensesSpec = do
  testModuleStructure "Lenses" Lenses.opticsRules (Just Lenses.lensRuleCount)

  describe "lens operation rules" $ do
    it "viewOp is defined" $
      Lenses.viewOp `shouldSatisfy` hasValidPattern

    it "overOp is defined" $
      Lenses.overOp `shouldSatisfy` hasValidPattern

    it "setOp is defined" $
      Lenses.setOp `shouldSatisfy` hasValidPattern

  describe "prism operation rules" $ do
    it "previewOp is defined" $
      Lenses.previewOp `shouldSatisfy` hasValidPattern

    it "reviewOp is defined" $
      Lenses.reviewOp `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- List Module Tests
--------------------------------------------------------------------------------

listSpec :: Spec
listSpec = do
  testModuleStructure "List" List.listRules (Just List.listRuleCount)

  describe "list simplification rules" $ do
    it "concatMapRule is defined" $
      List.concatMapRule `shouldSatisfy` hasValidPattern

    it "concatSingleton is defined" $
      List.concatSingleton `shouldSatisfy` hasValidPattern

    it "headReverse is defined" $
      List.headReverse `shouldSatisfy` hasValidPattern

    it "lastReverse is defined" $
      List.lastReverse `shouldSatisfy` hasValidPattern

  describe "list indexing rules" $ do
    it "lengthZero is defined" $
      List.lengthZero `shouldSatisfy` hasValidPattern

    it "lengthNotZero is defined" $
      List.lengthNotZero `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- ListRecursion Module Tests
--------------------------------------------------------------------------------

listRecursionSpec :: Spec
listRecursionSpec = do
  testModuleStructure "ListRecursion" ListRecursion.listRecursionRules (Just ListRecursion.listRecursionRuleCount)

  describe "map pattern rules" $ do
    it "explicitMap is defined" $
      ListRecursion.explicitMap `shouldSatisfy` hasValidPattern

    it "explicitConcatMap is defined" $
      ListRecursion.explicitConcatMap `shouldSatisfy` hasValidPattern

  describe "fold pattern rules" $ do
    it "explicitFoldr is defined" $
      ListRecursion.explicitFoldr `shouldSatisfy` hasValidPattern

    it "explicitFoldl is defined" $
      ListRecursion.explicitFoldl `shouldSatisfy` hasValidPattern

    it "explicitSum is defined" $
      ListRecursion.explicitSum `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Maybe Module Tests
--------------------------------------------------------------------------------

maybeSpec :: Spec
maybeSpec = do
  testModuleStructure "Maybe" Maybe.maybeRules (Just Maybe.maybeRuleCount)

  describe "simplification rules" $ do
    it "maybeNothingJust is defined" $
      Maybe.maybeNothingJust `shouldSatisfy` hasValidPattern

    it "maybeDefaultId is defined" $
      Maybe.maybeDefaultId `shouldSatisfy` hasValidPattern

    it "fromMaybeNothing is defined" $
      Maybe.fromMaybeNothing `shouldSatisfy` hasValidPattern

    it "fromMaybeJust is defined" $
      Maybe.fromMaybeJust `shouldSatisfy` hasValidPattern

  describe "pattern rules" $ do
    it "isJustJust is defined" $
      Maybe.isJustJust `shouldSatisfy` hasValidPattern

    it "isJustNothing is defined" $
      Maybe.isJustNothing `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Monadic Module Tests
--------------------------------------------------------------------------------

monadicSpec :: Spec
monadicSpec = do
  testModuleStructure "Monadic" Monadic.monadicRules (Just Monadic.monadicRuleCount)

  describe "applicative modernization rules" $ do
    it "returnToPure is defined" $
      Monadic.returnToPure `shouldSatisfy` hasValidPattern

    it "liftMToFmap is defined" $
      Monadic.liftMToFmap `shouldSatisfy` hasValidPattern

    it "liftM2ToLiftA2 is defined" $
      Monadic.liftM2ToLiftA2 `shouldSatisfy` hasValidPattern

  describe "traversal rules" $ do
    it "mapMToTraverse is defined" $
      Monadic.mapMToTraverse `shouldSatisfy` hasValidPattern

    it "forMToFor is defined" $
      Monadic.forMToFor `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Monoid Module Tests
--------------------------------------------------------------------------------

monoidSpec :: Spec
monoidSpec = do
  testModuleStructure "Monoid" Monoid.monoidRules (Just Monoid.monoidRuleCount)

  describe "monoid simplify rules" $ do
    it "memptyAppend is defined" $
      Monoid.memptyAppend `shouldSatisfy` hasValidPattern

    it "appendMempty is defined" $
      Monoid.appendMempty `shouldSatisfy` hasValidPattern

    it "mconcatSingleton is defined" $
      Monoid.mconcatSingleton `shouldSatisfy` hasValidPattern

    it "foldMapId is defined" $
      Monoid.foldMapId `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Numeric Module Tests
--------------------------------------------------------------------------------

numericSpec :: Spec
numericSpec = do
  testModuleStructure "Numeric" Numeric.numericRules (Just Numeric.numericRuleCount)

  describe "arithmetic rules" $ do
    it "addZero is defined" $
      Numeric.addZero `shouldSatisfy` hasValidPattern

    it "subtractZero is defined" $
      Numeric.subtractZero `shouldSatisfy` hasValidPattern

    it "multiplyOne is defined" $
      Numeric.multiplyOne `shouldSatisfy` hasValidPattern

    it "divideOne is defined" $
      Numeric.divideOne `shouldSatisfy` hasValidPattern

    it "doubleNegate is defined" $
      Numeric.doubleNegate `shouldSatisfy` hasValidPattern

    it "subtractSelf is defined" $
      Numeric.subtractSelf `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- OWASP Module Tests
--------------------------------------------------------------------------------

owaspSpec :: Spec
owaspSpec = do
  testModuleStructure "OWASP" OWASP.allOWASPRules Nothing  -- No rule count exported

  describe "OWASP Top 10 categories" $ do
    it "a01BrokenAccessControl is defined" $
      OWASP.a01BrokenAccessControl `shouldSatisfy` (not . null)

    it "a02CryptographicFailures is defined" $
      OWASP.a02CryptographicFailures `shouldSatisfy` (not . null)

    it "a03Injection is defined" $
      OWASP.a03Injection `shouldSatisfy` (not . null)

    it "a04InsecureDesign is defined" $
      OWASP.a04InsecureDesign `shouldSatisfy` (not . null)

    it "a05SecurityMisconfiguration is defined" $
      OWASP.a05SecurityMisconfiguration `shouldSatisfy` (not . null)

--------------------------------------------------------------------------------
-- Pattern Module Tests
--------------------------------------------------------------------------------

patternSpec :: Spec
patternSpec = do
  testModuleStructure "Pattern" Pattern.patternMatchRules (Just Pattern.patternRuleCount)

  describe "pattern simplify rules" $ do
    it "caseWildcard is defined" $
      Pattern.caseWildcard `shouldSatisfy` hasValidPattern

    it "caseIdentity is defined" $
      Pattern.caseIdentity `shouldSatisfy` hasValidPattern

    it "caseBoolId is defined" $
      Pattern.caseBoolId `shouldSatisfy` hasValidPattern

  describe "pattern optimize rules" $ do
    it "patternOverlap is defined" $
      Pattern.patternOverlap `shouldSatisfy` hasValidPattern

    it "patternNonExhaustive is defined" $
      Pattern.patternNonExhaustive `shouldSatisfy` hasValidPattern

    it "patternRedundant is defined" $
      Pattern.patternRedundant `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Performance Module Tests
--------------------------------------------------------------------------------

performanceSpec :: Spec
performanceSpec = do
  testModuleStructure "Performance" Performance.performanceRules (Just Performance.performanceRuleCount)

  describe "algorithmic rules" $ do
    it "avoidNub is defined" $
      Performance.avoidNub `shouldSatisfy` hasValidPattern

    it "avoidSortNub is defined" $
      Performance.avoidSortNub `shouldSatisfy` hasValidPattern

  describe "efficiency rules" $ do
    it "preferIntMap is defined" $
      Performance.preferIntMap `shouldSatisfy` hasValidPattern

    it "preferFoldl' is defined" $
      Performance.preferFoldl' `shouldSatisfy` hasValidPattern

    it "strictState is defined" $
      Performance.strictState `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Prelude Module Tests
--------------------------------------------------------------------------------

preludeSpec :: Spec
preludeSpec = do
  testModuleStructure "Prelude" Prelude.preludeRules (Just Prelude.preludeRuleCount)

  describe "partial prelude rules" $ do
    it "unsafeHead is defined" $
      Prelude.unsafeHead `shouldSatisfy` hasValidPattern

    it "unsafeTail is defined" $
      Prelude.unsafeTail `shouldSatisfy` hasValidPattern

    it "unsafeFromJust is defined" $
      Prelude.unsafeFromJust `shouldSatisfy` hasValidPattern

    it "unsafeMinimum is defined" $
      Prelude.unsafeMinimum `shouldSatisfy` hasValidPattern

    it "unsafeMaximum is defined" $
      Prelude.unsafeMaximum `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Records Module Tests
--------------------------------------------------------------------------------

recordsSpec :: Spec
recordsSpec = do
  testModuleStructure "Records" Records.recordRules (Just Records.recordRuleCount)

  describe "record syntax rules" $ do
    it "recordWildcards is defined" $
      Records.recordWildcards `shouldSatisfy` hasValidPattern

    it "recordPuns is defined" $
      Records.recordPuns `shouldSatisfy` hasValidPattern

    it "recordUpdate is defined" $
      Records.recordUpdate `shouldSatisfy` hasValidPattern

  describe "data declaration rules" $ do
    it "partialRecordField is defined" $
      Records.partialRecordField `shouldSatisfy` hasValidPattern

    it "recordSelector is defined" $
      Records.recordSelector `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Safety Module Tests
--------------------------------------------------------------------------------

safetySpec :: Spec
safetySpec = do
  testModuleStructure "Safety" Safety.safetyRules (Just Safety.safetyRuleCount)

  describe "partial function rules" $ do
    it "avoidHead is defined" $
      Safety.avoidHead `shouldSatisfy` hasValidPattern

    it "avoidTail is defined" $
      Safety.avoidTail `shouldSatisfy` hasValidPattern

    it "avoidFromJust is defined" $
      Safety.avoidFromJust `shouldSatisfy` hasValidPattern

  describe "unsafe operation rules" $ do
    it "avoidBangBang is defined" $
      Safety.avoidBangBang `shouldSatisfy` hasValidPattern

    it "avoidMaximum is defined" $
      Safety.avoidMaximum `shouldSatisfy` hasValidPattern

    it "avoidMinimum is defined" $
      Safety.avoidMinimum `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Security Module Tests
--------------------------------------------------------------------------------

securitySpec :: Spec
securitySpec = do
  testModuleStructure "Security" Security.securityRules (Just Security.securityRuleCount)

  describe "unsafe operations rules" $ do
    it "avoidUnsafeCoerce is defined" $
      Security.avoidUnsafeCoerce `shouldSatisfy` hasValidPattern

    it "avoidUnsafePerformIO is defined" $
      Security.avoidUnsafePerformIO `shouldSatisfy` hasValidPattern

    it "avoidUnsafeInterleaveIO is defined" $
      Security.avoidUnsafeInterleaveIO `shouldSatisfy` hasValidPattern

  describe "injection rules" $ do
    it "avoidRawSql is defined" $
      Security.avoidRawSql `shouldSatisfy` hasValidPattern

    it "avoidSystemCommand is defined" $
      Security.avoidSystemCommand `shouldSatisfy` hasValidPattern

    it "avoidShellCommand is defined" $
      Security.avoidShellCommand `shouldSatisfy` hasValidPattern

  describe "secrets rules" $ do
    it "avoidHardcodedPassword is defined" $
      Security.avoidHardcodedPassword `shouldSatisfy` hasValidPattern

    it "avoidHardcodedSecret is defined" $
      Security.avoidHardcodedSecret `shouldSatisfy` hasValidPattern

  describe "crypto rules" $ do
    it "avoidMD5 is defined" $
      Security.avoidMD5 `shouldSatisfy` hasValidPattern

    it "avoidSHA1 is defined" $
      Security.avoidSHA1 `shouldSatisfy` hasValidPattern

    it "avoidECB is defined" $
      Security.avoidECB `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Strings Module Tests
--------------------------------------------------------------------------------

stringsSpec :: Spec
stringsSpec = do
  testModuleStructure "Strings" Strings.stringRules (Just Strings.stringRuleCount)

  describe "text conversion rules" $ do
    it "preferText is defined" $
      Strings.preferText `shouldSatisfy` hasValidPattern

    it "packUnpack is defined" $
      Strings.packUnpack `shouldSatisfy` hasValidPattern

    it "unpackPack is defined" $
      Strings.unpackPack `shouldSatisfy` hasValidPattern

    it "textShow is defined" $
      Strings.textShow `shouldSatisfy` hasValidPattern

  describe "concatenation rules" $ do
    it "concatToMconcat is defined" $
      Strings.concatToMconcat `shouldSatisfy` hasValidPattern

    it "appendToMappend is defined" $
      Strings.appendToMappend `shouldSatisfy` hasValidPattern

  describe "manipulation rules" $ do
    it "nullToIsEmpty is defined" $
      Strings.nullToIsEmpty `shouldSatisfy` hasValidPattern

    it "lengthToCompare is defined" $
      Strings.lengthToCompare `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Style Module Tests
--------------------------------------------------------------------------------

styleSpec :: Spec
styleSpec = do
  testModuleStructure "Style" Style.styleRules (Just Style.styleRuleCount)

  describe "applicative rules" $ do
    it "preferPure is defined" $
      Style.preferPure `shouldSatisfy` hasValidPattern

    it "preferApplicative is defined" $
      Style.preferApplicative `shouldSatisfy` hasValidPattern

    it "preferTraverse_ is defined" $
      Style.preferTraverse_ `shouldSatisfy` hasValidPattern

  describe "redundant code rules" $ do
    it "redundantDo is defined" $
      Style.redundantDo `shouldSatisfy` hasValidPattern

    it "redundantBracket is defined" $
      Style.redundantBracket `shouldSatisfy` hasValidPattern

    it "redundantReturn is defined" $
      Style.redundantReturn `shouldSatisfy` hasValidPattern

  describe "modernization rules" $ do
    it "useLambdaCase is defined" $
      Style.useLambdaCase `shouldSatisfy` hasValidPattern

    it "useMultiWayIf is defined" $
      Style.useMultiWayIf `shouldSatisfy` hasValidPattern

  describe "simplification rules" $ do
    it "simplifyLambda is defined" $
      Style.simplifyLambda `shouldSatisfy` hasValidPattern

    it "pointFree is defined" $
      Style.pointFree `shouldSatisfy` hasValidPattern

    it "etaReduce is defined" $
      Style.etaReduce `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Testing Module Tests
--------------------------------------------------------------------------------

testingSpec :: Spec
testingSpec = do
  testModuleStructure "Testing" Testing.testingRules (Just Testing.testingRuleCount)

  describe "assertion rules" $ do
    it "assertEqualOrder is defined" $
      Testing.assertEqualOrder `shouldSatisfy` hasValidPattern

    it "assertBoolTrue is defined" $
      Testing.assertBoolTrue `shouldSatisfy` hasValidPattern

    it "assertBoolFalse is defined" $
      Testing.assertBoolFalse `shouldSatisfy` hasValidPattern

    it "assertJust is defined" $
      Testing.assertJust `shouldSatisfy` hasValidPattern

  describe "quickcheck rules" $ do
    it "arbitraryBounds is defined" $
      Testing.arbitraryBounds `shouldSatisfy` hasValidPattern

    it "shrinkMissing is defined" $
      Testing.shrinkMissing `shouldSatisfy` hasValidPattern

    it "propertyBool is defined" $
      Testing.propertyBool `shouldSatisfy` hasValidPattern

  describe "hspec rules" $ do
    it "shouldBeBool is defined" $
      Testing.shouldBeBool `shouldSatisfy` hasValidPattern

    it "shouldBeJust is defined" $
      Testing.shouldBeJust `shouldSatisfy` hasValidPattern

    it "pendingTest is defined" $
      Testing.pendingTest `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Transformers Module Tests
--------------------------------------------------------------------------------

transformersSpec :: Spec
transformersSpec = do
  testModuleStructure "Transformers" Transformers.mtlTransformerRules (Just Transformers.transformerRuleCount)

  describe "reader rules" $ do
    it "askRedundant is defined" $
      Transformers.askRedundant `shouldSatisfy` hasValidPattern

    it "asksSimplify is defined" $
      Transformers.asksSimplify `shouldSatisfy` hasValidPattern

    it "localPattern is defined" $
      Transformers.localPattern `shouldSatisfy` hasValidPattern

    it "askReturn is defined" $
      Transformers.askReturn `shouldSatisfy` hasValidPattern

  describe "state rules" $ do
    it "getReturn is defined" $
      Transformers.getReturn `shouldSatisfy` hasValidPattern

    it "putGet is defined" $
      Transformers.putGet `shouldSatisfy` hasValidPattern

    it "modifyGet is defined" $
      Transformers.modifyGet `shouldSatisfy` hasValidPattern

  describe "writer rules" $ do
    it "tellMempty is defined" $
      Transformers.tellMempty `shouldSatisfy` hasValidPattern

    it "tellMconcat is defined" $
      Transformers.tellMconcat `shouldSatisfy` hasValidPattern

  describe "except rules" $ do
    it "throwECatch is defined" $
      Transformers.throwECatch `shouldSatisfy` hasValidPattern

    it "catchEThrow is defined" $
      Transformers.catchEThrow `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- Tuple Module Tests
--------------------------------------------------------------------------------

tupleSpec :: Spec
tupleSpec = do
  testModuleStructure "Tuple" Tuple.tupleRules (Just Tuple.tupleRuleCount)

  describe "basic tuple rules" $ do
    it "fstPair is defined" $
      Tuple.fstPair `shouldSatisfy` hasValidPattern

    it "sndPair is defined" $
      Tuple.sndPair `shouldSatisfy` hasValidPattern

    it "swapSwap is defined" $
      Tuple.swapSwap `shouldSatisfy` hasValidPattern

    it "pairFstSnd is defined" $
      Tuple.pairFstSnd `shouldSatisfy` hasValidPattern

  describe "tuple functions" $ do
    it "bimapPair is defined" $
      Tuple.bimapPair `shouldSatisfy` hasValidPattern

    it "firstPair is defined" $
      Tuple.firstPair `shouldSatisfy` hasValidPattern

    it "secondPair is defined" $
      Tuple.secondPair `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- TypeAnnotations Module Tests
--------------------------------------------------------------------------------

typeAnnotationsSpec :: Spec
typeAnnotationsSpec = do
  testModuleStructure "TypeAnnotations" TypeAnnotations.typeAnnotationRules (Just TypeAnnotations.typeAnnotationRuleCount)

  describe "signature rules" $ do
    it "missingTopLevelSig is defined" $
      TypeAnnotations.missingTopLevelSig `shouldSatisfy` hasValidPattern

    it "missingExportedSig is defined" $
      TypeAnnotations.missingExportedSig `shouldSatisfy` hasValidPattern

    it "missingInstanceSig is defined" $
      TypeAnnotations.missingInstanceSig `shouldSatisfy` hasValidPattern

  describe "constraint rules" $ do
    it "redundantEqOrd is defined" $
      TypeAnnotations.redundantEqOrd `shouldSatisfy` hasValidPattern

    it "redundantShowRead is defined" $
      TypeAnnotations.redundantShowRead `shouldSatisfy` hasValidPattern

    it "redundantMonadApplicative is defined" $
      TypeAnnotations.redundantMonadApplicative `shouldSatisfy` hasValidPattern

  describe "type improvement rules" $ do
    it "useNewtype is defined" $
      TypeAnnotations.useNewtype `shouldSatisfy` hasValidPattern

    it "useTypeAlias is defined" $
      TypeAnnotations.useTypeAlias `shouldSatisfy` hasValidPattern

    it "avoidStringly is defined" $
      TypeAnnotations.avoidStringly `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- TypeclassLaws Module Tests
--------------------------------------------------------------------------------

typeclassLawsSpec :: Spec
typeclassLawsSpec = do
  testModuleStructure "TypeclassLaws" TypeclassLaws.typeclassLawRules (Just TypeclassLaws.typeclassLawRuleCount)

  describe "functor laws" $ do
    it "functorIdentity is defined" $
      TypeclassLaws.functorIdentity `shouldSatisfy` hasValidPattern

    it "functorCompositionLaw is defined" $
      TypeclassLaws.functorCompositionLaw `shouldSatisfy` hasValidPattern

  describe "applicative laws" $ do
    it "applicativeIdentityLaw is defined" $
      TypeclassLaws.applicativeIdentityLaw `shouldSatisfy` hasValidPattern

    it "applicativeCompositionLaw is defined" $
      TypeclassLaws.applicativeCompositionLaw `shouldSatisfy` hasValidPattern

  describe "monad laws" $ do
    it "monadLeftIdentityLaw is defined" $
      TypeclassLaws.monadLeftIdentityLaw `shouldSatisfy` hasValidPattern

    it "monadRightIdentityLaw is defined" $
      TypeclassLaws.monadRightIdentityLaw `shouldSatisfy` hasValidPattern

--------------------------------------------------------------------------------
-- TypeFamilies Module Tests
--------------------------------------------------------------------------------

typeFamiliesSpec :: Spec
typeFamiliesSpec = do
  testModuleStructure "TypeFamilies" TypeFamilies.typeFamilyRules (Just TypeFamilies.typeFamilyRuleCount)

  describe "type family declaration rules" $ do
    it "closedTypeFam is defined" $
      TypeFamilies.closedTypeFam `shouldSatisfy` hasValidPattern

    it "openTypeFam is defined" $
      TypeFamilies.openTypeFam `shouldSatisfy` hasValidPattern

    it "associatedTypeFam is defined" $
      TypeFamilies.associatedTypeFam `shouldSatisfy` hasValidPattern

  describe "data family rules" $ do
    it "dataFamDecl is defined" $
      TypeFamilies.dataFamDecl `shouldSatisfy` hasValidPattern

    it "dataFamInstance is defined" $
      TypeFamilies.dataFamInstance `shouldSatisfy` hasValidPattern

  describe "type equality rules" $ do
    it "typeEquality is defined" $
      TypeFamilies.typeEquality `shouldSatisfy` hasValidPattern

    it "coercible is defined" $
      TypeFamilies.coercible `shouldSatisfy` hasValidPattern
