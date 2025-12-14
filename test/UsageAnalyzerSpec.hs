{-# LANGUAGE OverloadedStrings #-}

module UsageAnalyzerSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Hspec

import Argus.Imports.UsageAnalyzer
import Argus.Imports.ImportDB (ImportDB(..), SymbolInfo(..), SymbolKind(..), emptyDB)
import Argus.Types (FixImport(..))

spec :: Spec
spec = do
  describe "Argus.Imports.UsageAnalyzer" $ do
    describe "extractUsedSymbols" $ do
      it "extracts simple function calls" $ do
        let source = "foo = bar x + baz y"
            symbols = extractUsedSymbols source
        "bar" `elem` symbols `shouldBe` True
        "baz" `elem` symbols `shouldBe` True

      it "extracts type references" $ do
        let source = "foo :: Text -> Maybe Int"
            symbols = extractUsedSymbols source
        "Text" `elem` symbols `shouldBe` True
        "Maybe" `elem` symbols `shouldBe` True
        "Int" `elem` symbols `shouldBe` True

      it "extracts qualified names" $ do
        let source = "foo = T.pack str"
            symbols = extractUsedSymbols source
        "pack" `elem` symbols `shouldBe` True

      it "handles multiline source" $ do
        let source = T.unlines
              [ "foo :: Int -> String"
              , "foo n = show n"
              , ""
              , "bar :: Bool"
              , "bar = True"
              ]
            symbols = extractUsedSymbols source
        "show" `elem` symbols `shouldBe` True
        "Int" `elem` symbols `shouldBe` True
        "String" `elem` symbols `shouldBe` True
        "Bool" `elem` symbols `shouldBe` True

      it "handles empty source" $ do
        let symbols = extractUsedSymbols ""
        symbols `shouldBe` []

    describe "extractQualifiedUsages" $ do
      it "extracts fully qualified usage" $ do
        let source = "foo = Data.Text.pack str"
            usages = extractQualifiedUsages source
        any (\s -> suName s == "pack" && suQualifier s == Just "Data.Text") usages `shouldBe` True

      it "extracts short qualified usage" $ do
        let source = "foo = T.pack str"
            usages = extractQualifiedUsages source
        any (\s -> suName s == "pack" && suQualifier s == Just "T") usages `shouldBe` True

      it "extracts multiple qualified usages" $ do
        let source = "foo = M.lookup key m <|> T.pack \"\""
            usages = extractQualifiedUsages source
        length usages `shouldSatisfy` (>= 2)
        any (\s -> suName s == "lookup" && suQualifier s == Just "M") usages `shouldBe` True
        any (\s -> suName s == "pack" && suQualifier s == Just "T") usages `shouldBe` True

      it "handles empty source" $ do
        let usages = extractQualifiedUsages ""
        usages `shouldBe` []

    describe "extractTypeSymbols" $ do
      it "extracts type from signature" $ do
        let source = "foo :: Int -> String"
            types = extractTypeSymbols source
            names = map suName types
        "Int" `elem` names `shouldBe` True
        "String" `elem` names `shouldBe` True

      it "extracts parameterized types" $ do
        let source = "foo :: Maybe Int -> Either String Bool"
            types = extractTypeSymbols source
            names = map suName types
        "Maybe" `elem` names `shouldBe` True
        "Either" `elem` names `shouldBe` True

      it "extracts constraint types" $ do
        let source = "foo :: (Show a, Eq a) => a -> String"
            types = extractTypeSymbols source
            names = map suName types
        "Show" `elem` names `shouldBe` True
        "Eq" `elem` names `shouldBe` True

      it "returns SymbolUsage with STType" $ do
        let source = "foo :: Text"
            types = extractTypeSymbols source
        all (\s -> suType s == STType) types `shouldBe` True

    describe "extractValueSymbols" $ do
      it "extracts function applications" $ do
        let source = "foo = map f xs"
            values = extractValueSymbols source
            names = map suName values
        "map" `elem` names `shouldBe` True
        "f" `elem` names `shouldBe` True
        "xs" `elem` names `shouldBe` True

      it "returns SymbolUsage with STValue" $ do
        let source = "foo = bar baz"
            values = extractValueSymbols source
        any (\s -> suType s == STValue) values `shouldBe` True

    describe "analyzeSourceSymbols" $ do
      it "combines value and type symbols" $ do
        let source = "foo :: Int\nfoo = bar x"
            symbols = analyzeSourceSymbols source
            names = map suName symbols
        "Int" `elem` names `shouldBe` True
        "bar" `elem` names `shouldBe` True

      it "includes qualified usages" $ do
        let source = "foo = T.pack str"
            symbols = analyzeSourceSymbols source
        any (\s -> suName s == "pack" && suQualifier s == Just "T") symbols `shouldBe` True

      it "handles empty source" $ do
        let symbols = analyzeSourceSymbols ""
        symbols `shouldBe` []

    describe "analyzeUsageChange" $ do
      it "detects added symbols" $ do
        let oldSource = "foo = bar x"
            newSource = "foo = bar x + baz y"
            change = analyzeUsageChange oldSource newSource
        Set.member "baz" (ucAdded change) `shouldBe` True

      it "detects removed symbols" $ do
        let oldSource = "foo = bar x + baz y"
            newSource = "foo = bar x"
            change = analyzeUsageChange oldSource newSource
        Set.member "baz" (ucRemoved change) `shouldBe` True

      it "detects unchanged symbols" $ do
        let oldSource = "foo = bar x"
            newSource = "foo = bar x + baz y"
            change = analyzeUsageChange oldSource newSource
        Set.member "bar" (ucUnchanged change) `shouldBe` True

      it "handles identical source" $ do
        let source = "foo = bar x"
            change = analyzeUsageChange source source
        ucAdded change `shouldBe` Set.empty
        ucRemoved change `shouldBe` Set.empty
        Set.size (ucUnchanged change) `shouldSatisfy` (> 0)

      it "handles empty to non-empty" $ do
        let oldSource = ""
            newSource = "foo = bar x"
            change = analyzeUsageChange oldSource newSource
        Set.member "bar" (ucAdded change) `shouldBe` True

      it "handles non-empty to empty" $ do
        let oldSource = "foo = bar x"
            newSource = ""
            change = analyzeUsageChange oldSource newSource
        Set.member "bar" (ucRemoved change) `shouldBe` True

    describe "resolveSymbolImports" $ do
      let testDB = emptyDB
            { dbSymbols = Map.fromList
                [ ("pack", [mkSymbolInfo "Data.Text" "pack" SKFunction])
                , ("unpack", [mkSymbolInfo "Data.Text" "unpack" SKFunction])
                , ("headMay", [mkSymbolInfo "Safe" "headMay" SKFunction])
                ]
            }

      it "resolves known symbol" $ do
        let symbols = ["pack"]
            imports = resolveSymbolImports testDB symbols
        case imports of
          [imp] -> fimpModule imp `shouldBe` "Data.Text"
          _     -> expectationFailure "Expected exactly 1 import"

      it "resolves multiple symbols from same module" $ do
        let symbols = ["pack", "unpack"]
            imports = resolveSymbolImports testDB symbols
        -- Should be combined into one import
        case imports of
          [imp] -> length (fimpSymbols imp) `shouldBe` 2
          _     -> expectationFailure "Expected exactly 1 import"

      it "resolves symbols from different modules" $ do
        let symbols = ["pack", "headMay"]
            imports = resolveSymbolImports testDB symbols
        length imports `shouldBe` 2
        any (\imp -> fimpModule imp == "Data.Text") imports `shouldBe` True
        any (\imp -> fimpModule imp == "Safe") imports `shouldBe` True

      it "returns empty for unknown symbols" $ do
        let symbols = ["unknownFn"]
            imports = resolveSymbolImports testDB symbols
        imports `shouldBe` []

      it "handles empty symbol list" $ do
        let imports = resolveSymbolImports testDB []
        imports `shouldBe` []

    describe "findBestImport" $ do
      let testDB = emptyDB
            { dbSymbols = Map.fromList
                [ ("pack",
                    [ mkSymbolInfo "Data.Text" "pack" SKFunction
                    , mkSymbolInfo "Data.Text.Lazy" "pack" SKFunction
                    ])
                ]
            }

      it "finds import for known symbol" $ do
        let result = findBestImport testDB "pack"
        case result of
          Just (name, info) -> do
            name `shouldBe` "pack"
            siModule info `shouldSatisfy` (`elem` ["Data.Text", "Data.Text.Lazy"])
          Nothing -> expectationFailure "Expected to find import"

      it "returns Nothing for unknown symbol" $ do
        let result = findBestImport testDB "unknown"
        result `shouldBe` Nothing

    describe "isSymbolInScope" $ do
      let testDB = emptyDB
            { dbSymbols = Map.fromList
                [ ("pack", [mkSymbolInfo "Data.Text" "pack" SKFunction])
                , ("lookup", [mkSymbolInfo "Data.Map" "lookup" SKFunction])
                ]
            }

      it "returns True for symbol from imported module" $ do
        isSymbolInScope "pack" ["Data.Text"] testDB `shouldBe` True

      it "returns False for symbol from non-imported module" $ do
        isSymbolInScope "pack" ["Data.Map"] testDB `shouldBe` False

      it "returns False for unknown symbol" $ do
        isSymbolInScope "unknownFn" ["Data.Text"] testDB `shouldBe` False

      it "returns False for empty import list" $ do
        isSymbolInScope "pack" [] testDB `shouldBe` False

    describe "SymbolUsage" $ do
      it "has correct type field" $ do
        let usage = SymbolUsage "foo" Nothing STValue 5
        suType usage `shouldBe` STValue

      it "stores qualifier" $ do
        let usage = SymbolUsage "pack" (Just "T") STValue 1
        suQualifier usage `shouldBe` Just "T"

      it "stores occurrences" $ do
        let usage = SymbolUsage "foo" Nothing STValue 5
        suOccurrences usage `shouldBe` 5

    describe "SymbolType" $ do
      it "has Ord instance" $ do
        STType < STValue `shouldBe` True
        STValue < STOperator `shouldBe` True
        STOperator < STUnknown `shouldBe` True

      it "has Eq instance" $ do
        STType == STType `shouldBe` True
        STValue == STValue `shouldBe` True
        STType == STValue `shouldBe` False

    describe "UsageChange" $ do
      it "stores added symbols as Set" $ do
        let change = UsageChange (Set.fromList ["a", "b"]) Set.empty Set.empty
        Set.size (ucAdded change) `shouldBe` 2

      it "stores removed symbols as Set" $ do
        let change = UsageChange Set.empty (Set.fromList ["x"]) Set.empty
        Set.member "x" (ucRemoved change) `shouldBe` True

    describe "edge cases" $ do
      it "handles unicode identifiers" $ do
        let source = "foo = fmap' x"
            symbols = extractUsedSymbols source
        "fmap'" `elem` symbols `shouldBe` True

      it "handles prime in identifiers" $ do
        let source = "foo' = bar'' x"
            symbols = extractUsedSymbols source
        "bar''" `elem` symbols `shouldBe` True

      it "handles underscore identifiers" $ do
        let source = "foo_bar = baz_qux x"
            symbols = extractUsedSymbols source
        "baz_qux" `elem` symbols `shouldBe` True

      it "handles numeric suffixes" $ do
        let source = "foo1 = bar2 x"
            symbols = extractUsedSymbols source
        "bar2" `elem` symbols `shouldBe` True

      it "handles deeply qualified names" $ do
        let source = "foo = Data.Text.Internal.pack str"
            usages = extractQualifiedUsages source
        any (\s -> suName s == "pack" && suQualifier s == Just "Data.Text.Internal") usages `shouldBe` True

      it "handles many symbols in one line" $ do
        let source = "foo = a + b - c * d / e"
            symbols = extractUsedSymbols source
        length symbols `shouldSatisfy` (>= 5)

      it "handles type with many parameters" $ do
        let source = "foo :: Map String (Maybe (Either Int Bool))"
            types = extractTypeSymbols source
            names = map suName types
        "Map" `elem` names `shouldBe` True
        "String" `elem` names `shouldBe` True
        "Maybe" `elem` names `shouldBe` True
        "Either" `elem` names `shouldBe` True

-- Helper functions
mkSymbolInfo :: T.Text -> T.Text -> SymbolKind -> SymbolInfo
mkSymbolInfo modName name kind = SymbolInfo
  { siName = name
  , siKind = kind
  , siModule = modName
  , siImports = []
  , siDescription = Nothing
  , siSince = Nothing
  , siDeprecated = Nothing
  }
