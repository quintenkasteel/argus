{-# LANGUAGE OverloadedStrings #-}

module HIESymbolTableSpec (spec) where

import Test.Hspec
import Data.Text (Text)

import Argus.Types (SymbolKind(..), mkSrcSpanRaw)
import Argus.HIE.Types
import Argus.HIE.SymbolTable

spec :: Spec
spec = do
  describe "Argus.HIE.SymbolTable" $ do
    describe "emptySymbolTable" $ do
      it "has no symbols" $ do
        let table = emptySymbolTable
        lookupSymbol table "anything" `shouldBe` Nothing

      it "finds nothing in scope" $ do
        findInScope emptySymbolTable "missing" `shouldBe` []

    describe "addSymbol" $ do
      it "adds a symbol to the table" $ do
        let sym = mkTestSymbol "foo" "Test"
            table = addSymbol sym emptySymbolTable
        lookupSymbol table "foo" `shouldBe` Just sym

      it "allows multiple symbols with same name" $ do
        let sym1 = mkTestSymbol "foo" "Test.A"
            sym2 = mkTestSymbol "foo" "Test.B"
            table = addSymbol sym2 $ addSymbol sym1 emptySymbolTable
        length (findInScope table "foo") `shouldBe` 2

      it "preserves order when adding" $ do
        let sym1 = mkTestSymbol "first" "A"
            sym2 = mkTestSymbol "second" "B"
            table = addSymbol sym2 $ addSymbol sym1 emptySymbolTable
        lookupSymbol table "first" `shouldBe` Just sym1
        lookupSymbol table "second" `shouldBe` Just sym2

    describe "removeSymbol" $ do
      it "removes a symbol from the table" $ do
        let sym = mkTestSymbol "foo" "Test"
            table = removeSymbol "foo" $ addSymbol sym emptySymbolTable
        lookupSymbol table "foo" `shouldBe` Nothing

      it "leaves other symbols intact" $ do
        let sym1 = mkTestSymbol "keep" "Test"
            sym2 = mkTestSymbol "remove" "Test"
            table = removeSymbol "remove" $ addSymbol sym2 $ addSymbol sym1 emptySymbolTable
        lookupSymbol table "keep" `shouldBe` Just sym1
        lookupSymbol table "remove" `shouldBe` Nothing

    describe "resolveSymbol" $ do
      it "resolves existing symbol" $ do
        let sym = mkTestSymbol "bar" "Test"
            table = addSymbol sym emptySymbolTable
        resolveSymbol table "bar" `shouldBe` Just sym

      it "returns Nothing for non-existent symbol" $ do
        resolveSymbol emptySymbolTable "nonexistent" `shouldBe` Nothing

    describe "resolveInScope" $ do
      it "resolves symbol in scope" $ do
        let sym = mkTestSymbol "baz" "Test"
            srcSpan = mkSrcSpanRaw "test.hs" 1 1 1 10
            table = addSymbol sym emptySymbolTable
        resolveInScope table "baz" srcSpan `shouldBe` Just sym

    describe "findInScope" $ do
      it "finds all matching symbols" $ do
        let sym1 = mkTestSymbol "name" "A"
            sym2 = mkTestSymbol "name" "B"
            table = addSymbol sym2 $ addSymbol sym1 emptySymbolTable
        length (findInScope table "name") `shouldBe` 2

      it "returns empty for no matches" $ do
        findInScope emptySymbolTable "missing" `shouldBe` []

    describe "canReplace" $ do
      it "allows replacing same name" $ do
        let sym = mkTestSymbol "foo" "Test"
            table = addSymbol sym emptySymbolTable
        canReplace table "foo" "foo" `shouldBe` SafeReplace

      it "reports symbol not found" $ do
        case canReplace emptySymbolTable "nonexistent" "new" of
          UnsafeSemanticChange msg -> msg `shouldSatisfy` (/= "")
          _ -> expectationFailure "Expected UnsafeSemanticChange"

    describe "checkShadowing" $ do
      it "returns Nothing when no shadowing" $ do
        checkShadowing emptySymbolTable "newName" `shouldBe` Nothing

    describe "checkConflicts" $ do
      it "returns empty for no conflicts" $ do
        checkConflicts emptySymbolTable "anyName" `shouldBe` []

    describe "mergeSymbolTables" $ do
      it "merges symbols from both tables" $ do
        let sym1 = mkTestSymbol "foo" "A"
            sym2 = mkTestSymbol "bar" "B"
            table1 = addSymbol sym1 emptySymbolTable
            table2 = addSymbol sym2 emptySymbolTable
            merged = mergeSymbolTables table1 table2
        lookupSymbol merged "foo" `shouldBe` Just sym1
        lookupSymbol merged "bar" `shouldBe` Just sym2

      it "combines same-named symbols" $ do
        let sym1 = mkTestSymbol "shared" "A"
            sym2 = mkTestSymbol "shared" "B"
            table1 = addSymbol sym1 emptySymbolTable
            table2 = addSymbol sym2 emptySymbolTable
            merged = mergeSymbolTables table1 table2
        length (findInScope merged "shared") `shouldBe` 2

    describe "filterSymbols" $ do
      it "keeps symbols matching predicate" $ do
        let sym1 = mkTestSymbol "keep" "Test"
            sym2 = (mkTestSymbol "remove" "Other") { hsExported = False }
            table = addSymbol sym2 $ addSymbol sym1 emptySymbolTable
            filtered = filterSymbols hsExported table
        lookupSymbol filtered "keep" `shouldBe` Just sym1
        lookupSymbol filtered "remove" `shouldBe` Nothing

      it "removes all when predicate always false" $ do
        let sym = mkTestSymbol "test" "Test"
            table = addSymbol sym emptySymbolTable
            filtered = filterSymbols (const False) table
        lookupSymbol filtered "test" `shouldBe` Nothing

    describe "getModuleScope" $ do
      it "returns module-level symbols" $ do
        let sym = mkTestSymbol "modLevel" "Test"
            table = addSymbol sym emptySymbolTable
        getModuleScope table `shouldSatisfy` (\ss -> any ((== "modLevel") . hsName) ss)

    describe "getGlobalScope" $ do
      it "returns exported symbols" $ do
        let exportedSym = mkTestSymbol "exported" "Test"
            privateSym = (mkTestSymbol "private" "Test") { hsExported = False }
            table = addSymbol privateSym $ addSymbol exportedSym emptySymbolTable
            globals = getGlobalScope table
        any ((== "exported") . hsName) globals `shouldBe` True
        any ((== "private") . hsName) globals `shouldBe` False

    describe "getLocalScope" $ do
      it "returns symbols at scope" $ do
        let sym = mkTestSymbol "local" "Test"
            table = addSymbol sym emptySymbolTable
            srcSpan = mkSrcSpanRaw "test.hs" 1 1 1 10
        getLocalScope table srcSpan `shouldSatisfy` (\ss -> any ((== "local") . hsName) ss)

    describe "Edge cases" $ do
      it "handles empty table operations" $ do
        getModuleScope emptySymbolTable `shouldBe` []
        getGlobalScope emptySymbolTable `shouldBe` []

      it "handles symbol with no module" $ do
        let sym = (mkTestSymbol "orphan" "") { hsModule = "" }
            table = addSymbol sym emptySymbolTable
        lookupSymbol table "orphan" `shouldBe` Just sym

-- Helper functions

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
