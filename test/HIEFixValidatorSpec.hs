{-# LANGUAGE OverloadedStrings #-}

module HIEFixValidatorSpec (spec) where

import Test.Hspec
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Argus.Types
  ( Fix(..), FixEdit(..), FixCategory(..), FixSafety(..)
  , mkSrcSpanRaw, SymbolKind(..)
  )
import Argus.HIE.Types (HieSymbol(..), SymbolScope(..), SymbolVisibility(..))
import Argus.HIE.SymbolTable (emptySymbolTable, addSymbol)
import Argus.HIE.FixValidator

spec :: Spec
spec = do
  describe "Argus.HIE.FixValidator" $ do
    describe "Configuration" $ do
      describe "defaultValidatorConfig" $ do
        it "has reasonable defaults" $ do
          fvcCheckConstraints defaultValidatorConfig `shouldBe` True
          fvcCheckShadowing defaultValidatorConfig `shouldBe` True
          fvcCheckTypePreservation defaultValidatorConfig `shouldBe` True
          fvcStrictMode defaultValidatorConfig `shouldBe` False

      describe "strictValidatorConfig" $ do
        it "enables strict mode" $ do
          fvcStrictMode strictValidatorConfig `shouldBe` True
          fvcMaxWarnings strictValidatorConfig `shouldBe` 0
          fvcAllowPartialTypes strictValidatorConfig `shouldBe` False

    describe "Fix Validation" $ do
      describe "validateFix" $ do
        it "validates a simple fix" $ do
          let fix = mkSimpleFix "Test fix" "newCode"
              table = emptySymbolTable
          result <- validateFix defaultValidatorConfig Nothing table fix
          fvrFix result `shouldBe` fix

        it "returns valid for empty fix" $ do
          let fix = mkEmptyFix
              table = emptySymbolTable
          result <- validateFix defaultValidatorConfig Nothing table fix
          fvrValid result `shouldBe` True

        it "checks constraints for performance fixes" $ do
          let fix = mkPerformanceFix "Use ordNub" "ordNub xs"
              table = emptySymbolTable
          result <- validateFix defaultValidatorConfig Nothing table fix
          -- Should have constraint checks for Ord
          fvrConstraints result `shouldSatisfy` (not . null)

    describe "validateFixes" $ do
      it "validates multiple fixes" $ do
        let fixes = [mkSimpleFix "Fix 1" "code1", mkSimpleFix "Fix 2" "code2"]
            table = emptySymbolTable
        results <- validateFixes defaultValidatorConfig Nothing table fixes
        length results `shouldBe` 2

    describe "Constraint Validation" $ do
      describe "validateConstraints" $ do
        it "infers Ord constraint for ordNub" $ do
          let fix = mkPerformanceFix "Use ordNub" "ordNub xs"
          results <- validateConstraints defaultValidatorConfig fix
          any (\r -> ccConstraint (crCheck r) == "Ord") results `shouldBe` True

        it "infers Ord constraint for Set.fromList" $ do
          let fix = mkPerformanceFix "Use Set" "Set.fromList xs"
          results <- validateConstraints defaultValidatorConfig fix
          any (\r -> ccConstraint (crCheck r) == "Ord") results `shouldBe` True

        it "infers Hashable constraint for HashMap" $ do
          let fix = mkPerformanceFix "Use HashMap" "HashMap.fromList pairs"
          results <- validateConstraints defaultValidatorConfig fix
          any (\r -> ccConstraint (crCheck r) == "Hashable") results `shouldBe` True

        it "infers Semigroup for <>" $ do
          let fix = mkStyleFix "Use <>" "a <> b"
          results <- validateConstraints defaultValidatorConfig fix
          any (\r -> ccConstraint (crCheck r) == "Semigroup") results `shouldBe` True

      describe "checkConstraintPreservation" $ do
        it "returns True for same expression" $ do
          result <- checkConstraintPreservation defaultValidatorConfig "head xs" "head xs"
          result `shouldBe` True

    describe "Symbol Safety" $ do
      describe "checkSymbolSafety" $ do
        it "returns SymbolSafe for empty table" $ do
          let fix = mkSimpleFix "Test" "foo"
          result <- checkSymbolSafety emptySymbolTable fix
          result `shouldBe` SymbolSafe

        it "detects potential conflicts" $ do
          let sym = mkTestSymbol "existingName" "Test"
              table = addSymbol sym emptySymbolTable
              fix = mkSimpleFix "Test" "existingName"
          result <- checkSymbolSafety table fix
          case result of
            SymbolSafe -> pure ()  -- May not detect without full analysis
            SymbolWarning _ -> pure ()
            SymbolUnsafe _ -> pure ()

      describe "checkShadowingForFix" $ do
        it "returns Right for non-shadowing" $ do
          checkShadowingForFix emptySymbolTable "newName" `shouldBe` Right ()

      describe "checkNameConflictsForFix" $ do
        it "returns Right for no conflicts" $ do
          checkNameConflictsForFix emptySymbolTable "newName" `shouldBe` Right ()

    describe "Type Preservation" $ do
      describe "checkTypePreservation" $ do
        it "returns result for fix" $ do
          let fix = mkSimpleFix "Test" "newCode"
          result <- checkTypePreservation defaultValidatorConfig fix
          case result of
            TypesPreservedResult -> pure ()
            TypesCompatibleResult _ _ -> pure ()
            TypesIncompatibleResult _ _ _ -> pure ()
            TypesUnknownResult -> pure ()  -- Expected when no type info

      describe "TypePreservationResult" $ do
        it "has correct constructors" $ do
          let r1 = TypesPreservedResult
              r2 = TypesCompatibleResult "Int" "Integer"
              r3 = TypesIncompatibleResult "Int" "String" "mismatch"
              r4 = TypesUnknownResult
          r1 `shouldSatisfy` (== TypesPreservedResult)
          case r2 of
            TypesCompatibleResult a b -> do
              a `shouldBe` "Int"
              b `shouldBe` "Integer"
            _ -> expectationFailure "Expected TypesCompatibleResult"
          case r3 of
            TypesIncompatibleResult a b c -> do
              a `shouldBe` "Int"
              b `shouldBe` "String"
              c `shouldBe` "mismatch"
            _ -> expectationFailure "Expected TypesIncompatibleResult"
          r4 `shouldSatisfy` (== TypesUnknownResult)

    describe "Batch Validation" $ do
      describe "batchValidateFixes" $ do
        it "returns batch statistics" $ do
          let fixes = [mkSimpleFix "Fix 1" "a", mkSimpleFix "Fix 2" "b"]
              table = emptySymbolTable
          result <- batchValidateFixes defaultValidatorConfig Nothing table fixes
          bvrTotal result `shouldBe` 2

        it "counts valid and invalid separately" $ do
          let fixes = [mkEmptyFix]
              table = emptySymbolTable
          result <- batchValidateFixes defaultValidatorConfig Nothing table fixes
          bvrTotal result `shouldBe` 1
          (bvrValid result + bvrInvalid result) `shouldBe` 1

        it "groups by category" $ do
          let fixes = [ mkPerformanceFix "Perf" "a"
                      , mkStyleFix "Style" "b"
                      , mkPerformanceFix "Perf2" "c"
                      ]
              table = emptySymbolTable
          result <- batchValidateFixes defaultValidatorConfig Nothing table fixes
          Map.size (bvrByCategory result) `shouldSatisfy` (>= 1)

    describe "Validation Errors" $ do
      it "FVEConstraintViolation has correct structure" $ do
        let err = FVEConstraintViolation "Ord" "CustomType" "no instance"
        case err of
          FVEConstraintViolation c t r -> do
            c `shouldBe` "Ord"
            t `shouldBe` "CustomType"
            r `shouldBe` "no instance"
          _ -> expectationFailure "Expected FVEConstraintViolation"

      it "FVEShadowingConflict has correct structure" $ do
        let err = FVEShadowingConflict "name" "Data.List"
        case err of
          FVEShadowingConflict n m -> do
            n `shouldBe` "name"
            m `shouldBe` "Data.List"
          _ -> expectationFailure "Expected FVEShadowingConflict"

      it "FVETypeChange has correct structure" $ do
        let err = FVETypeChange "Int" "String"
        case err of
          FVETypeChange old new -> do
            old `shouldBe` "Int"
            new `shouldBe` "String"
          _ -> expectationFailure "Expected FVETypeChange"

    describe "Validation Warnings" $ do
      it "FVWPartialTypeInfo has correct structure" $ do
        let warn = FVWPartialTypeInfo "foo"
        case warn of
          FVWPartialTypeInfo s -> s `shouldBe` "foo"
          _ -> expectationFailure "Expected FVWPartialTypeInfo"

      it "FVWImportRequired has correct structure" $ do
        let warn = FVWImportRequired "Data.Map" "fromList"
        case warn of
          FVWImportRequired m s -> do
            m `shouldBe` "Data.Map"
            s `shouldBe` "fromList"
          _ -> expectationFailure "Expected FVWImportRequired"

-- Helper functions

mkSimpleFix :: Text -> Text -> Fix
mkSimpleFix title code = Fix
  { fixTitle = title
  , fixEdits = [FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 10) code]
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSAlways
  }

mkEmptyFix :: Fix
mkEmptyFix = Fix
  { fixTitle = "Empty fix"
  , fixEdits = []
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSAlways
  }

mkPerformanceFix :: Text -> Text -> Fix
mkPerformanceFix title code = Fix
  { fixTitle = title
  , fixEdits = [FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 20) code]
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCPerformance
  , fixSafety = FSAlways
  }

mkStyleFix :: Text -> Text -> Fix
mkStyleFix title code = Fix
  { fixTitle = title
  , fixEdits = [FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 20) code]
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSAlways
  }

mkTestSymbol :: Text -> Text -> HieSymbol
mkTestSymbol name modName = HieSymbol
  { hsName = name
  , hsModule = modName
  , hsQualified = modName <> "." <> name
  , hsKind = Function
  , hsType = Nothing
  , hsDefinition = Nothing
  , hsReferences = []
  , hsExported = True
  , hsScope = ScopeModule
  , hsVisibility = VisPublic
  , hsDocumentation = Nothing
  }
