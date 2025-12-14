{-# LANGUAGE OverloadedStrings #-}

module HIETypesSpec (spec) where

import Test.Hspec
import Data.Aeson qualified as Aeson
import Data.Text (Text)

import Argus.Types (SymbolKind(..), mkSrcSpanRaw)
import Argus.HIE.Types

spec :: Spec
spec = do
  describe "Argus.HIE.Types" $ do
    describe "HieSymbol" $ do
      it "has correct default values" $ do
        let sym = mkTestSymbol "foo" "Test.Module" Function
        hsName sym `shouldBe` "foo"
        hsModule sym `shouldBe` "Test.Module"
        hsKind sym `shouldBe` Function

      it "computes qualified name correctly" $ do
        let sym = mkTestSymbol "bar" "Data.List" Function
        hsQualified sym `shouldBe` "Data.List.bar"

      it "serializes and deserializes with JSON" $ do
        let sym = mkTestSymbol "baz" "Control.Monad" TypeClass
        Aeson.decode (Aeson.encode sym) `shouldBe` Just sym

    describe "SymbolScope" $ do
      it "has correct ordering" $ do
        ScopeLocal `shouldSatisfy` (< ScopeModule)
        ScopeModule `shouldSatisfy` (< ScopeGlobal)

      it "is bounded" $ do
        minBound `shouldBe` ScopeLocal
        maxBound `shouldBe` ScopeGlobal

    describe "SymbolVisibility" $ do
      it "has correct ordering" $ do
        VisPrivate `shouldSatisfy` (< VisInternal)
        VisInternal `shouldSatisfy` (< VisPublic)

      it "is bounded" $ do
        minBound `shouldBe` VisPrivate
        maxBound `shouldBe` VisPublic

    describe "TypeConstraint" $ do
      it "creates type class constraints" $ do
        let tc = TypeConstraint "Ord" "Int" True Nothing
        tcClass tc `shouldBe` "Ord"
        tcType tc `shouldBe` "Int"
        tcSatisfied tc `shouldBe` True

      it "serializes to JSON" $ do
        let tc = TypeConstraint "Eq" "Text" False Nothing
        Aeson.encode tc `shouldSatisfy` (/= "")

    describe "ConstraintKind" $ do
      it "creates type class constraints" $ do
        let ck = CKTypeClass "Functor"
        case ck of
          CKTypeClass name -> name `shouldBe` "Functor"
          _ -> expectationFailure "Expected CKTypeClass"

      it "creates equality constraints" $ do
        let ck = CKEquality "a" "Int"
        case ck of
          CKEquality a b -> do
            a `shouldBe` "a"
            b `shouldBe` "Int"
          _ -> expectationFailure "Expected CKEquality"

      it "creates HasField constraints" $ do
        let ck = CKHasField "name" "Text"
        case ck of
          CKHasField field ty -> do
            field `shouldBe` "name"
            ty `shouldBe` "Text"
          _ -> expectationFailure "Expected CKHasField"

    describe "TypeInfo" $ do
      it "tracks monomorphic types" $ do
        let ti = TypeInfo "Int" "*" [] True []
        tiMonomorphic ti `shouldBe` True
        tiPolymorphic ti `shouldBe` []

      it "tracks polymorphic types" $ do
        let ti = TypeInfo "[a]" "* -> *" [] False ["a"]
        tiMonomorphic ti `shouldBe` False
        tiPolymorphic ti `shouldBe` ["a"]

    describe "InstanceInfo" $ do
      it "stores instance information" $ do
        let ii = InstanceInfo "Show" "MyType" "MyModule" False Nothing
        iiClass ii `shouldBe` "Show"
        iiType ii `shouldBe` "MyType"
        iiModule ii `shouldBe` "MyModule"
        iiOrphan ii `shouldBe` False

      it "tracks orphan instances" $ do
        let ii = InstanceInfo "Show" "Int" "MyModule" True Nothing
        iiOrphan ii `shouldBe` True

      it "tracks overlap modes" $ do
        let ii = InstanceInfo "Show" "MyType" "MyModule" False (Just "OVERLAPPING")
        iiOverlap ii `shouldBe` Just "OVERLAPPING"

    describe "ReplaceSafety" $ do
      it "identifies safe replacements" $ do
        let rs = SafeReplace
        case rs of
          SafeReplace -> pure ()
          _ -> expectationFailure "Expected SafeReplace"

      it "identifies unsafe conflicts" $ do
        let rs = UnsafeConflict "existing name"
        case rs of
          UnsafeConflict msg -> msg `shouldBe` "existing name"
          _ -> expectationFailure "Expected UnsafeConflict"

      it "identifies shadowing issues" $ do
        let rs = UnsafeShadow "Data.List" "map"
        case rs of
          UnsafeShadow modName name -> do
            modName `shouldBe` "Data.List"
            name `shouldBe` "map"
          _ -> expectationFailure "Expected UnsafeShadow"

    describe "TypeValidation" $ do
      it "indicates types preserved" $ do
        let tv = TypesPreserved
        case tv of
          TypesPreserved -> pure ()
          _ -> expectationFailure "Expected TypesPreserved"

      it "indicates compatible types" $ do
        let ti = TypeInfo "Int" "*" [] True []
            tv = TypesCompatible ti
        case tv of
          TypesCompatible info -> tiType info `shouldBe` "Int"
          _ -> expectationFailure "Expected TypesCompatible"

      it "indicates incompatible types" $ do
        let span = mkSrcSpanRaw "test.hs" 1 1 1 10
            issue = ValidationIssue "type-mismatch" "Types don't match" span
                      (Just "Int") (Just "String") Nothing
            tv = TypesIncompatible issue
        case tv of
          TypesIncompatible vi -> viKind vi `shouldBe` "type-mismatch"
          _ -> expectationFailure "Expected TypesIncompatible"

    describe "QueryResult" $ do
      it "is a functor" $ do
        let qr = QueryResult [1, 2, 3] 3 False 1.5 :: QueryResult Int
            qr' = fmap (* 2) qr
        qrResults qr' `shouldBe` [2, 4, 6]

      it "tracks truncation status" $ do
        let qr = QueryResult ["a", "b"] 100 True 2.0 :: QueryResult Text
        qrTruncated qr `shouldBe` True
        qrTotal qr `shouldBe` 100

    describe "ReferenceKind" $ do
      it "has all reference kinds" $ do
        let kinds = [RefUsage, RefDefinition, RefImport, RefExport,
                     RefTypeSignature, RefInstanceHead, RefPattern]
        length kinds `shouldBe` 7

      it "is bounded and enumerable" $ do
        minBound `shouldBe` RefUsage
        maxBound `shouldBe` RefPattern
        length [minBound..maxBound :: ReferenceKind] `shouldBe` 7

    describe "ModuleSymbols" $ do
      it "stores module information" $ do
        let ms = mkTestModuleSymbols "Data.List" (Just "src/Data/List.hs")
        msModule ms `shouldBe` "Data.List"
        msFile ms `shouldBe` Just "src/Data/List.hs"

    describe "SymbolReference" $ do
      it "stores reference information" $ do
        let span = mkSrcSpanRaw "test.hs" 10 5 10 15
            ref = SymbolReference "foo" "Test" "test.hs" span RefUsage Nothing
        srSymbol ref `shouldBe` "foo"
        srModule ref `shouldBe` "Test"
        srKind ref `shouldBe` RefUsage

      it "tracks qualified usage" $ do
        let span = mkSrcSpanRaw "test.hs" 10 5 10 15
            ref = SymbolReference "map" "Data.List" "test.hs" span RefUsage (Just "L")
        srQualified ref `shouldBe` Just "L"

-- Helper functions

mkTestSymbol :: Text -> Text -> SymbolKind -> HieSymbol
mkTestSymbol name modName kind = HieSymbol
  { hsName = name
  , hsModule = modName
  , hsQualified = modName <> "." <> name
  , hsKind = kind
  , hsType = Nothing
  , hsDefinition = Nothing
  , hsReferences = []
  , hsExported = True
  , hsScope = ScopeModule
  , hsVisibility = VisPublic
  , hsDocumentation = Nothing
  }

mkTestModuleSymbols :: Text -> Maybe FilePath -> ModuleSymbols
mkTestModuleSymbols modName mFile = ModuleSymbols
  { msModule = modName
  , msFile = mFile
  , msExports = mempty
  , msImports = mempty
  , msDefinitions = []
  }
