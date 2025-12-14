{-# LANGUAGE OverloadedStrings #-}

module HIETypeInfoSpec (spec) where

import Test.Hspec
import Data.Text (Text)

import Argus.Types (Fix(..), FixCategory(..), FixSafety(..))
import Argus.HIE.Types (TypeInfo(..), TypeConstraint(..), TypeValidation(..))
import Argus.HIE.TypeInfo

spec :: Spec
spec = do
  describe "Argus.HIE.TypeInfo" $ do
    describe "Type Analysis" $ do
      describe "isListType" $ do
        it "recognizes list types" $ do
          isListType "[Int]" `shouldBe` True
          isListType "[Char]" `shouldBe` True
          isListType "[[a]]" `shouldBe` True

        it "rejects non-list types" $ do
          isListType "Int" `shouldBe` False
          isListType "Maybe [Int]" `shouldBe` False
          isListType "Vector Int" `shouldBe` False

      describe "isMaybeType" $ do
        it "recognizes Maybe types" $ do
          isMaybeType "Maybe Int" `shouldBe` True
          isMaybeType "Maybe a" `shouldBe` True
          isMaybeType "Maybe" `shouldBe` True

        it "rejects non-Maybe types" $ do
          isMaybeType "Int" `shouldBe` False
          isMaybeType "Either a b" `shouldBe` False
          isMaybeType "MaybeT IO a" `shouldBe` False

      describe "isEitherType" $ do
        it "recognizes Either types" $ do
          isEitherType "Either a b" `shouldBe` True
          isEitherType "Either String Int" `shouldBe` True
          isEitherType "Either" `shouldBe` True

        it "rejects non-Either types" $ do
          isEitherType "Maybe a" `shouldBe` False
          isEitherType "ExceptT e m a" `shouldBe` False

      describe "isIOType" $ do
        it "recognizes IO types" $ do
          isIOType "IO ()" `shouldBe` True
          isIOType "IO Int" `shouldBe` True
          isIOType "IO" `shouldBe` True

        it "rejects non-IO types" $ do
          isIOType "Maybe a" `shouldBe` False
          isIOType "IORef Int" `shouldBe` False

      describe "isMonadType" $ do
        it "recognizes common monads" $ do
          isMonadType "IO a" `shouldBe` True
          isMonadType "Maybe a" `shouldBe` True
          isMonadType "Either e a" `shouldBe` True
          isMonadType "ReaderT r m a" `shouldBe` True
          isMonadType "StateT s m a" `shouldBe` True
          isMonadType "WriterT w m a" `shouldBe` True
          isMonadType "ExceptT e m a" `shouldBe` True

        it "rejects non-monad types" $ do
          isMonadType "Int" `shouldBe` False
          isMonadType "[a]" `shouldBe` False

      describe "extractListElementType" $ do
        it "extracts element type from list" $ do
          extractListElementType "[Int]" `shouldBe` Just "Int"
          extractListElementType "[Char]" `shouldBe` Just "Char"
          extractListElementType "[Maybe a]" `shouldBe` Just "Maybe a"

        it "returns Nothing for non-list" $ do
          extractListElementType "Int" `shouldBe` Nothing
          extractListElementType "Maybe Int" `shouldBe` Nothing

      describe "extractMaybeInnerType" $ do
        it "extracts inner type from Maybe" $ do
          extractMaybeInnerType "Maybe Int" `shouldBe` Just "Int"
          extractMaybeInnerType "Maybe a" `shouldBe` Just "a"
          extractMaybeInnerType "Maybe (Either a b)" `shouldBe` Just "(Either a b)"

        it "returns Nothing for non-Maybe" $ do
          extractMaybeInnerType "Int" `shouldBe` Nothing
          extractMaybeInnerType "Either a b" `shouldBe` Nothing

    describe "Type Comparison" $ do
      describe "typesEqual" $ do
        it "returns True for identical types" $ do
          typesEqual "Int" "Int" `shouldBe` True
          typesEqual "Maybe a" "Maybe a" `shouldBe` True

        it "returns False for different types" $ do
          typesEqual "Int" "Integer" `shouldBe` False
          typesEqual "Maybe a" "Maybe b" `shouldBe` False

      describe "typesCompatible" $ do
        it "recognizes compatible polymorphic types" $ do
          typesCompatible "a" "Int" `shouldBe` True
          typesCompatible "[a]" "[Int]" `shouldBe` True

        it "recognizes identical types as compatible" $ do
          typesCompatible "Int" "Int" `shouldBe` True

        it "rejects incompatible concrete types" $ do
          typesCompatible "Int" "String" `shouldBe` False

      describe "typeSubsumes" $ do
        it "polymorphic subsumes monomorphic" $ do
          typeSubsumes "a" "Int" `shouldBe` True
          typeSubsumes "[a]" "[Int]" `shouldBe` True

        it "same type subsumes itself" $ do
          typeSubsumes "Int" "Int" `shouldBe` True

        it "monomorphic does not subsume different monomorphic" $ do
          typeSubsumes "Int" "String" `shouldBe` False

    describe "Constraint Checking" $ do
      describe "hasOrdInstance" $ do
        it "returns True for types with Ord" $ do
          result <- hasOrdInstance "Int"
          result `shouldBe` True

        it "returns True for common ordered types" $ do
          hasOrdInstance "Integer" >>= (`shouldBe` True)
          hasOrdInstance "Double" >>= (`shouldBe` True)
          hasOrdInstance "Char" >>= (`shouldBe` True)
          hasOrdInstance "Text" >>= (`shouldBe` True)
          hasOrdInstance "Bool" >>= (`shouldBe` True)

        it "returns False for unknown types" $ do
          result <- hasOrdInstance "UnknownType"
          result `shouldBe` False

      describe "hasEqInstance" $ do
        it "returns True for types with Eq" $ do
          hasEqInstance "Int" >>= (`shouldBe` True)
          hasEqInstance "Text" >>= (`shouldBe` True)
          hasEqInstance "Bool" >>= (`shouldBe` True)

      describe "hasMonoidInstance" $ do
        it "returns True for Monoid types" $ do
          hasMonoidInstance "Text" >>= (`shouldBe` True)
          hasMonoidInstance "String" >>= (`shouldBe` True)
          hasMonoidInstance "[a]" >>= (`shouldBe` True)

        it "returns False for non-Monoid types" $ do
          hasMonoidInstance "Int" >>= (`shouldBe` False)

      describe "hasSemigroupInstance" $ do
        it "returns True for Semigroup types" $ do
          hasSemigroupInstance "Text" >>= (`shouldBe` True)
          hasSemigroupInstance "NonEmpty a" >>= (`shouldBe` True)

      describe "hasShowInstance" $ do
        it "returns True for Show types" $ do
          hasShowInstance "Int" >>= (`shouldBe` True)
          hasShowInstance "Bool" >>= (`shouldBe` True)

      describe "hasNumInstance" $ do
        it "returns True for Num types" $ do
          hasNumInstance "Int" >>= (`shouldBe` True)
          hasNumInstance "Integer" >>= (`shouldBe` True)
          hasNumInstance "Double" >>= (`shouldBe` True)

        it "returns False for non-Num types" $ do
          hasNumInstance "Text" >>= (`shouldBe` False)

      describe "hasHashableInstance" $ do
        it "returns True for Hashable types" $ do
          hasHashableInstance "Int" >>= (`shouldBe` True)
          hasHashableInstance "Text" >>= (`shouldBe` True)

      describe "checkConstraints" $ do
        it "returns unsatisfied constraints" $ do
          let constraints =
                [ mkConstraint "Ord" "Int"
                , mkConstraint "Num" "Text"
                , mkConstraint "Show" "Bool"
                ]
          unsatisfied <- checkConstraints constraints
          length unsatisfied `shouldBe` 1
          tcClass (head unsatisfied) `shouldBe` "Num"

    describe "Known Types Database" $ do
      describe "defaultKnownTypes" $ do
        it "contains common functions" $ do
          lookupKnownType defaultKnownTypes "head" `shouldSatisfy` (/= Nothing)
          lookupKnownType defaultKnownTypes "map" `shouldSatisfy` (/= Nothing)
          lookupKnownType defaultKnownTypes "foldl" `shouldSatisfy` (/= Nothing)

        it "has correct type info for head" $ do
          case lookupKnownType defaultKnownTypes "head" of
            Just ti -> tiType ti `shouldBe` "[a] -> a"
            Nothing -> expectationFailure "head not found"

        it "has polymorphic info for map" $ do
          case lookupKnownType defaultKnownTypes "map" of
            Just ti -> do
              tiPolymorphic ti `shouldBe` ["a", "b"]
              tiMonomorphic ti `shouldBe` False
            Nothing -> expectationFailure "map not found"

      describe "lookupKnownType" $ do
        it "finds known types" $ do
          lookupKnownType defaultKnownTypes "filter" `shouldSatisfy` (/= Nothing)

        it "returns Nothing for unknown" $ do
          lookupKnownType defaultKnownTypes "unknownFunction" `shouldBe` Nothing

      describe "registerType" $ do
        it "adds new types to database" $ do
          let newType = mkTypeInfo "CustomType -> Int" []
              updated = registerType "customFunc" newType defaultKnownTypes
          lookupKnownType updated "customFunc" `shouldBe` Just newType

        it "overwrites existing types" $ do
          let newType = mkTypeInfo "String -> String" []
              updated = registerType "head" newType defaultKnownTypes
          case lookupKnownType updated "head" of
            Just ti -> tiType ti `shouldBe` "String -> String"
            Nothing -> expectationFailure "head not found after update"

    describe "Fix Validation" $ do
      describe "validateFixTypes" $ do
        it "returns TypesPreserved for valid fix" $ do
          let fix = mkTestFix
          result <- validateFixTypes fix
          result `shouldBe` TypesPreserved

      describe "checkFixTypePreservation" $ do
        it "returns True when types match" $ do
          result <- checkFixTypePreservation "head" "head"
          result `shouldBe` True

        it "returns True when old type unknown" $ do
          result <- checkFixTypePreservation "unknownOld" "unknownNew"
          result `shouldBe` True

-- Helper functions

mkConstraint :: Text -> Text -> TypeConstraint
mkConstraint cls ty = TypeConstraint cls ty False Nothing

mkTypeInfo :: Text -> [Text] -> TypeInfo
mkTypeInfo ty vars = TypeInfo
  { tiType = ty
  , tiKind = "*"
  , tiConstraints = []
  , tiMonomorphic = null vars
  , tiPolymorphic = vars
  }

mkTestFix :: Fix
mkTestFix = Fix
  { fixTitle = "Test fix"
  , fixEdits = []
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSAlways
  }
