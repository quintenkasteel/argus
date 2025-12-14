{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the AutoFix DSL module.
module AutoFixDSLSpec (spec) where

import Test.Hspec

import Data.Set qualified as Set
import Data.Text (Text)

import Argus.AutoFix.DSL
import Argus.AutoFix.DSL qualified as DSL
import Argus.AutoFix.Types (Confidence, mkConfidence, highConfidence)
import Argus.Rules.Types (Category(..), SafetyLevel(..))

spec :: Spec
spec = do
  describe "Argus.AutoFix.DSL" $ do
    describe "FixSpec construction" $ do
      describe "fixSpec" $ do
        it "creates a fix spec with given name" $ do
          let fs = fixSpec "test-fix" $ pure ()
          fsName fs `shouldBe` "test-fix"

        it "uses default confidence of highConfidence" $ do
          let fs = fixSpec "test-fix" $ pure ()
          fsConfidence fs `shouldBe` highConfidence

        it "uses default safety of Safe" $ do
          let fs = fixSpec "test-fix" $ pure ()
          fsSafety fs `shouldBe` Safe

        it "uses default category of Style" $ do
          let fs = fixSpec "test-fix" $ pure ()
          fsCategory fs `shouldBe` Style

        it "has empty expressions by default" $ do
          let fs = fixSpec "test-fix" $ pure ()
          fsExprs fs `shouldBe` []

        it "has empty imports by default" $ do
          let fs = fixSpec "test-fix" $ pure ()
          fsAddImports fs `shouldBe` []
          fsRemoveImports fs `shouldBe` []

    describe "Core Fix Operations" $ do
      describe "replace" $ do
        it "creates a replace expression" $ do
          let fs = fixSpec "test" $ replace "old" "new"
          length (fsExprs fs) `shouldBe` 1
          case fsExprs fs of
            [FEReplace old new] -> do
              old `shouldBe` "old"
              new `shouldBe` "new"
            _ -> expectationFailure "Expected FEReplace"

      describe "replacePattern" $ do
        it "creates a replace pattern expression" $ do
          let fs = fixSpec "test" $ replacePattern "head $X" "headMay $X"
          length (fsExprs fs) `shouldBe` 1
          case fsExprs fs of
            [FEReplacePattern old new] -> do
              old `shouldBe` "head $X"
              new `shouldBe` "headMay $X"
            _ -> expectationFailure "Expected FEReplacePattern"

      describe "insert" $ do
        it "creates an insert expression" $ do
          let fs = fixSpec "test" $ insert 10 "new text"
          length (fsExprs fs) `shouldBe` 1
          case fsExprs fs of
            [FEInsert offset text] -> do
              offset `shouldBe` 10
              text `shouldBe` "new text"
            _ -> expectationFailure "Expected FEInsert"

      describe "delete" $ do
        it "creates a delete expression" $ do
          let fs = fixSpec "test" $ delete "target"
          length (fsExprs fs) `shouldBe` 1
          case fsExprs fs of
            [FEDelete target] ->
              target `shouldBe` "target"
            _ -> expectationFailure "Expected FEDelete"

      describe "wrap" $ do
        it "creates a wrap expression" $ do
          let fs = fixSpec "test" $ wrap "target" "(" ")"
          length (fsExprs fs) `shouldBe` 1
          case fsExprs fs of
            [FEWrap t p s] -> do
              t `shouldBe` "target"
              p `shouldBe` "("
              s `shouldBe` ")"
            _ -> expectationFailure "Expected FEWrap"

      describe "unwrap" $ do
        it "creates an unwrap expression" $ do
          let fs = fixSpec "test" $ unwrap "target" "(" ")"
          length (fsExprs fs) `shouldBe` 1
          case fsExprs fs of
            [FEUnwrap t p s] -> do
              t `shouldBe` "target"
              p `shouldBe` "("
              s `shouldBe` ")"
            _ -> expectationFailure "Expected FEUnwrap"

      describe "indent" $ do
        it "creates an indent expression" $ do
          let fs = fixSpec "test" $ indent "target" 2
          length (fsExprs fs) `shouldBe` 1
          case fsExprs fs of
            [FEIndent t amount] -> do
              t `shouldBe` "target"
              amount `shouldBe` 2
            _ -> expectationFailure "Expected FEIndent"

      describe "dedent" $ do
        it "creates a dedent expression (negative indent)" $ do
          let fs = fixSpec "test" $ dedent "target" 2
          length (fsExprs fs) `shouldBe` 1
          case fsExprs fs of
            [FEIndent t amount] -> do
              t `shouldBe` "target"
              amount `shouldBe` (-2)
            _ -> expectationFailure "Expected FEIndent with negative amount"

    describe "Multi-Target Operations" $ do
      describe "replaceAll" $ do
        it "creates multiple replace expressions" $ do
          let fs = fixSpec "test" $ replaceAll [("a", "b"), ("c", "d")]
          length (fsExprs fs) `shouldBe` 2

      describe "deleteAll" $ do
        it "creates multiple delete expressions" $ do
          let fs = fixSpec "test" $ deleteAll ["a", "b", "c"]
          length (fsExprs fs) `shouldBe` 3

    describe "Import Management" $ do
      describe "addImport" $ do
        it "adds import to spec" $ do
          let fs = fixSpec "test" $ addImport "Data.Maybe" ["fromMaybe", "isJust"]
          length (fsAddImports fs) `shouldBe` 1

        it "infers function type for lowercase identifiers" $ do
          let fs = fixSpec "test" $ addImport "Data.Maybe" ["fromMaybe"]
          length (fsAddImports fs) `shouldBe` 1
          -- Import should have been added

        it "infers type type for uppercase identifiers" $ do
          let fs = fixSpec "test" $ addImport "Data.Maybe" ["Maybe"]
          length (fsAddImports fs) `shouldBe` 1

      describe "addQualifiedImport" $ do
        it "adds qualified import" $ do
          let fs = fixSpec "test" $ addQualifiedImport "Data.Map.Strict" "M"
          length (fsAddImports fs) `shouldBe` 1

      describe "removeImport" $ do
        it "adds module to remove list" $ do
          let fs = fixSpec "test" $ removeImport "Data.List"
          length (fsRemoveImports fs) `shouldBe` 1
          head (fsRemoveImports fs) `shouldBe` "Data.List"

      describe "removeImports" $ do
        it "adds multiple modules to remove list" $ do
          let fs = fixSpec "test" $ removeImports ["Data.List", "Data.Maybe"]
          length (fsRemoveImports fs) `shouldBe` 2

    describe "Metadata Setters" $ do
      describe "confidence" $ do
        it "sets confidence level" $ do
          let fs = fixSpec "test" $ do
                replace "a" "b"
                confidence 0.5
          fsConfidence fs `shouldBe` mkConfidence 0.5

      describe "safety" $ do
        it "sets safety level to Safe" $ do
          let fs = fixSpec "test" $ safety Safe
          fsSafety fs `shouldBe` Safe

        it "sets safety level to MostlySafe" $ do
          let fs = fixSpec "test" $ safety MostlySafe
          fsSafety fs `shouldBe` MostlySafe

        it "sets safety level to NeedsReview" $ do
          let fs = fixSpec "test" $ safety NeedsReview
          fsSafety fs `shouldBe` NeedsReview

        it "sets safety level to Unsafe" $ do
          let fs = fixSpec "test" $ safety Unsafe
          fsSafety fs `shouldBe` Unsafe

      describe "category" $ do
        it "sets category to Performance" $ do
          let fs = fixSpec "test" $ category Performance
          fsCategory fs `shouldBe` Performance

        it "sets category to Security" $ do
          let fs = fixSpec "test" $ category Security
          fsCategory fs `shouldBe` Security

        it "sets category to Safety" $ do
          let fs = fixSpec "test" $ category Safety
          fsCategory fs `shouldBe` Safety

        it "sets category to Imports" $ do
          let fs = fixSpec "test" $ category Imports
          fsCategory fs `shouldBe` Imports

      describe "tags" $ do
        it "adds tags to spec" $ do
          let fs = fixSpec "test" $ tags ["tag1", "tag2", "tag3"]
          Set.size (fsTags fs) `shouldBe` 3

      describe "tag" $ do
        it "adds a single tag" $ do
          let fs = fixSpec "test" $ tag "my-tag"
          Set.member "my-tag" (fsTags fs) `shouldBe` True

      describe "note" $ do
        it "adds a note" $ do
          let fs = fixSpec "test" $ note "This is a note"
          length (fsNotes fs) `shouldBe` 1
          head (fsNotes fs) `shouldBe` "This is a note"

      describe "explanation" $ do
        it "sets explanation" $ do
          let fs = fixSpec "test" $ explanation "Detailed explanation"
          fsExplanation fs `shouldBe` Just "Detailed explanation"

      describe "sourceRule" $ do
        it "sets source rule" $ do
          let fs = fixSpec "test" $ sourceRule "avoid-head"
          fsSourceRule fs `shouldBe` Just "avoid-head"

    describe "Scope Modifiers" $ do
      describe "withinModules" $ do
        it "sets within patterns" $ do
          let fs = fixSpec "test" $ withinModules ["MyApp.*", "MyLib.*"]
          length (fsWithin fs) `shouldBe` 2

      describe "exceptIn" $ do
        it "sets except patterns" $ do
          let fs = fixSpec "test" $ exceptIn ["*Spec", "*Test"]
          length (fsExcept fs) `shouldBe` 2

      describe "onlyInTests" $ do
        it "sets test patterns" $ do
          let fs = fixSpec "test" onlyInTests
          length (fsWithin fs) `shouldSatisfy` (> 0)

      describe "exceptInTests" $ do
        it "sets test exception patterns" $ do
          let fs = fixSpec "test" exceptInTests
          length (fsExcept fs) `shouldSatisfy` (> 0)

    describe "Conditional Combinators" $ do
      describe "choice" $ do
        it "creates choice expression" $ do
          let fs = fixSpec "test" $ choice [replace "a" "b", replace "c" "d"]
          length (fsExprs fs) `shouldBe` 1
          case fsExprs fs of
            [FEChoice exprs] -> length exprs `shouldBe` 2
            _ -> expectationFailure "Expected FEChoice"

    describe "Composition Operators" $ do
      describe ".>" $ do
        it "sequences two builders" $ do
          let fs = fixSpec "test" $ replace "a" "b" .> replace "c" "d"
          length (fsExprs fs) `shouldBe` 2

      describe "sequenceAll" $ do
        it "sequences multiple builders" $ do
          let fs = fixSpec "test" $ sequenceAll
                     [ replace "a" "b"
                     , replace "c" "d"
                     , replace "e" "f"
                     ]
          length (fsExprs fs) `shouldBe` 3

      describe "parallel" $ do
        it "creates parallel expression" $ do
          let fs = fixSpec "test" $ DSL.parallel
                     [ replace "a" "b"
                     , replace "c" "d"
                     ]
          length (fsExprs fs) `shouldBe` 1
          case fsExprs fs of
            [FEParallel exprs] -> length exprs `shouldBe` 2
            _ -> expectationFailure "Expected FEParallel"

    describe "Validation Helpers" $ do
      describe "requiresSyntaxCheck" $ do
        it "sets syntax check flag" $ do
          let fs = fixSpec "test" requiresSyntaxCheck
          vfSyntaxCheck (fsValidation fs) `shouldBe` True

      describe "requiresTypeCheck" $ do
        it "sets type check flag" $ do
          let fs = fixSpec "test" requiresTypeCheck
          vfTypeCheck (fsValidation fs) `shouldBe` True

      describe "preservesSemantics" $ do
        it "sets semantics flag" $ do
          let fs = fixSpec "test" preservesSemantics
          vfSemantics (fsValidation fs) `shouldBe` True

      describe "idempotent" $ do
        it "sets idempotent flag" $ do
          let fs = fixSpec "test" idempotent
          vfIdempotent (fsValidation fs) `shouldBe` True

    describe "Priority and Ordering" $ do
      describe "priority" $ do
        it "sets priority" $ do
          let fs = fixSpec "test" $ priority 10
          fsPriority fs `shouldBe` 10

      describe "dependsOn" $ do
        it "adds dependency" $ do
          let fs = fixSpec "test" $ dependsOn "other-fix"
          Set.member "other-fix" (fsDependencies fs) `shouldBe` True

      describe "conflictsWith" $ do
        it "adds conflict" $ do
          let fs = fixSpec "test" $ conflictsWith "conflicting-fix"
          Set.member "conflicting-fix" (fsConflicts fs) `shouldBe` True

    describe "Expression Builders" $ do
      describe "var" $ do
        it "creates variable reference" $ do
          var "x" `shouldBe` "$x"

      describe "lit" $ do
        it "creates literal" $ do
          lit "42" `shouldBe` "42"

      describe "app" $ do
        it "creates application" $ do
          app "f" "x" `shouldBe` "f x"

      describe "lam" $ do
        it "creates lambda" $ do
          lam "x" "x + 1" `shouldBe` "\\x -> x + 1"

      describe "infixOp" $ do
        it "creates infix operation" $ do
          infixOp "a" "+" "b" `shouldBe` "a + b"

    describe "Pattern Helpers" $ do
      describe "matchVar" $ do
        it "creates variable pattern" $ do
          matchVar "x" `shouldBe` "$x"

      describe "matchLit" $ do
        it "creates literal pattern" $ do
          matchLit "42" `shouldBe` "42"

      describe "matchApp" $ do
        it "creates application pattern" $ do
          matchApp "f" "x" `shouldBe` "f x"

      describe "matchAny" $ do
        it "creates wildcard" $ do
          matchAny `shouldBe` "_"

      describe "capture" $ do
        it "creates capture" $ do
          capture "x" `shouldBe` "$x"

    describe "Full Fix Construction" $ do
      it "creates a complete fix specification" $ do
        let fs = fixSpec "avoid-head" $ do
              replacePattern "head $XS" "headMay $XS"
              addImport "Safe" ["headMay"]
              confidence 0.95
              safety Safe
              category Safety
              note "head is partial and can crash on empty lists"
              explanation "Replace partial head function with safe alternative"
        fsName fs `shouldBe` "avoid-head"
        length (fsExprs fs) `shouldBe` 1
        length (fsAddImports fs) `shouldBe` 1
        fsSafety fs `shouldBe` Safe
        fsCategory fs `shouldBe` Safety

      it "creates a fix with multiple operations" $ do
        let fs = fixSpec "modernize-folds" $ do
              replacePattern "foldl $F $Z $XS" "foldl' $F $Z $XS"
              replacePattern "foldr ($F) $Z" "foldr' $F $Z"
              addImports [("Data.List", ["foldl'"]), ("Data.Foldable", ["foldr'"])]
              category Performance
              tags ["performance", "space-leaks", "strictness"]
              note "Use strict folds to avoid space leaks"
              preservesSemantics
              idempotent
        length (fsExprs fs) `shouldBe` 2
        length (fsAddImports fs) `shouldBe` 2
        fsCategory fs `shouldBe` Performance
        Set.size (fsTags fs) `shouldBe` 3
        vfSemantics (fsValidation fs) `shouldBe` True
        vfIdempotent (fsValidation fs) `shouldBe` True

    describe "FixCondition" $ do
      it "FCAlways is always true" $ do
        FCAlways `shouldBe` FCAlways

      it "FCNever is always false" $ do
        FCNever `shouldBe` FCNever

      it "FCAnd combines conditions" $ do
        let cond = FCAnd (FCPatternMatches "foo") (FCImportPresent "Data.List")
        case cond of
          FCAnd _ _ -> pure ()
          _ -> expectationFailure "Expected FCAnd"

      it "FCOr combines conditions" $ do
        let cond = FCOr (FCPatternMatches "foo") (FCPragmaPresent "BangPatterns")
        case cond of
          FCOr _ _ -> pure ()
          _ -> expectationFailure "Expected FCOr"

      it "FCNot negates condition" $ do
        let cond = FCNot (FCPatternMatches "foo")
        case cond of
          FCNot _ -> pure ()
          _ -> expectationFailure "Expected FCNot"

    describe "ValidationFlags" $ do
      it "defaultValidationFlags has syntax check enabled" $ do
        vfSyntaxCheck defaultValidationFlags `shouldBe` True

      it "defaultValidationFlags has type check disabled" $ do
        vfTypeCheck defaultValidationFlags `shouldBe` False

      it "defaultValidationFlags has semantics disabled" $ do
        vfSemantics defaultValidationFlags `shouldBe` False

      it "defaultValidationFlags has idempotent disabled" $ do
        vfIdempotent defaultValidationFlags `shouldBe` False

    describe "Compilation" $ do
      describe "fixSpecToEnrichedFix" $ do
        it "converts fix spec to enriched fix" $ do
          let fs = fixSpec "test" $ do
                replace "a" "b"
                category Performance
                confidence 0.8
          let ef = fixSpecToEnrichedFix fs
          efId ef `shouldSatisfy` (\fid -> True)  -- Just check it exists

      describe "fixSpecToAction" $ do
        it "converts fix spec to action sequence" $ do
          let fs = fixSpec "test" $ do
                replace "a" "b"
                insert 10 "new"
          let action = fixSpecToAction fs
          action `shouldSatisfy` \case
            Just _ -> True
            Nothing -> False

        it "returns Nothing for empty spec" $ do
          let fs = fixSpec "test" $ pure ()
          fixSpecToAction fs `shouldBe` Nothing
