{-# LANGUAGE OverloadedStrings #-}

module ImportManagerSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Argus.Imports.Manager
import Argus.Types (FixImport(..), ImportSymbol(..), ImportSymbolType(..), Fix(..), FixEdit(..), FixCategory(..), FixSafety(..))

spec :: Spec
spec = do
  describe "Argus.Imports.Manager" $ do
    describe "parseImports" $ do
      it "parses simple import" $ do
        let source = "import Data.Text"
            imports = parseImports source
        length imports `shouldBe` 1
        piModule (head imports) `shouldBe` "Data.Text"
        piQualified (head imports) `shouldBe` False

      it "parses qualified import" $ do
        let source = "import qualified Data.Map as M"
            imports = parseImports source
        length imports `shouldBe` 1
        piModule (head imports) `shouldBe` "Data.Map"
        piQualified (head imports) `shouldBe` True
        piAlias (head imports) `shouldBe` Just "M"

      it "parses import with explicit list" $ do
        let source = "import Data.Text (Text, pack, unpack)"
            imports = parseImports source
        length imports `shouldBe` 1
        piModule (head imports) `shouldBe` "Data.Text"
        piExplicit (head imports) `shouldSatisfy` (\xs -> "Text" `elem` xs && "pack" `elem` xs)

      it "parses hiding import" $ do
        let source = "import Prelude hiding (head, tail)"
            imports = parseImports source
        length imports `shouldBe` 1
        piHiding (head imports) `shouldBe` True

      it "parses multiple imports" $ do
        let source = T.unlines
              [ "module Foo where"
              , ""
              , "import Data.Text"
              , "import Data.List"
              , "import Control.Monad"
              ]
            imports = parseImports source
        length imports `shouldBe` 3

    describe "renderImport" $ do
      it "renders simple import" $ do
        let imp = mkParsedImport "Data.Text" False Nothing []
            rendered = renderImport imp
        T.isInfixOf "import" rendered `shouldBe` True
        T.isInfixOf "Data.Text" rendered `shouldBe` True

      it "renders qualified import with alias" $ do
        let imp = mkParsedImport "Data.Map" True (Just "M") []
            rendered = renderImport imp
        T.isInfixOf "qualified" rendered `shouldBe` True
        T.isInfixOf "as M" rendered `shouldBe` True

      it "renders import with explicit list" $ do
        let imp = mkParsedImport "Data.Text" False Nothing ["Text", "pack"]
            rendered = renderImport imp
        T.isInfixOf "(Text, pack)" rendered `shouldBe` True

    describe "addFixImports" $ do
      it "adds new import when not present" $ do
        let existing = [mkParsedImport "Data.Text" False Nothing ["pack"]]
            fixImport = mkFixImport "Data.Maybe" ["fromMaybe"]
            result = addFixImports existing [fixImport]
        length result `shouldBe` 2
        any (\imp -> piModule imp == "Data.Maybe") result `shouldBe` True

      it "merges symbols when import exists" $ do
        let existing = [mkParsedImport "Data.Text" False Nothing ["pack"]]
            fixImport = mkFixImport "Data.Text" ["unpack"]
            result = addFixImports existing [fixImport]
        length result `shouldBe` 1
        let textImport = head result
        "pack" `elem` piExplicit textImport `shouldBe` True
        "unpack" `elem` piExplicit textImport `shouldBe` True

    describe "removeFixImports" $ do
      it "removes symbol from import" $ do
        let existing = [mkParsedImport "Data.Text" False Nothing ["pack", "unpack"]]
            result = removeFixImports existing ["unpack"]
        length result `shouldBe` 1
        "unpack" `notElem` piExplicit (head result) `shouldBe` True
        "pack" `elem` piExplicit (head result) `shouldBe` True

      it "handles removing non-existent symbol" $ do
        let existing = [mkParsedImport "Data.Text" False Nothing ["pack"]]
            result = removeFixImports existing ["nonexistent"]
        piExplicit (head result) `shouldBe` ["pack"]

    describe "fixImportToParsed" $ do
      it "converts FixImport to ParsedImport" $ do
        let fix = FixImport
              { fimpModule = "Data.Maybe"
              , fimpSymbols = [ImportSymbol "fromMaybe" ISTFunction []]
              , fimpQualified = Nothing
              , fimpHiding = False
              , fimpPackage = Nothing
              }
            parsed = fixImportToParsed fix
        piModule parsed `shouldBe` "Data.Maybe"
        piExplicit parsed `shouldBe` ["fromMaybe"]
        piQualified parsed `shouldBe` False

      it "handles qualified import" $ do
        let fix = FixImport
              { fimpModule = "Data.Map"
              , fimpSymbols = []
              , fimpQualified = Just "M"
              , fimpHiding = False
              , fimpPackage = Nothing
              }
            parsed = fixImportToParsed fix
        piQualified parsed `shouldBe` True
        piAlias parsed `shouldBe` Just "M"

    describe "applyImportChanges" $ do
      it "adds imports from fix" $ do
        let source = T.unlines
              [ "module Test where"
              , ""
              , "import Data.Text"
              , ""
              , "foo = undefined"
              ]
            fix = mkFixWithImports [mkFixImport "Data.Maybe" ["fromMaybe"]] []
            result = applyImportChanges defaultImportConfig source fix
        T.isInfixOf "Data.Maybe" result `shouldBe` True

      it "removes imports from fix" $ do
        let source = T.unlines
              [ "module Test where"
              , ""
              , "import Data.Text (pack, unpack)"
              , ""
              , "foo = undefined"
              ]
            fix = mkFixWithImports [] ["unpack"]
            result = applyImportChanges defaultImportConfig source fix
        -- After removing unpack, pack should still be there
        T.isInfixOf "pack" result `shouldBe` True

    describe "sortImports" $ do
      it "sorts imports by module name" $ do
        let imports =
              [ mkParsedImport "Data.Text" False Nothing []
              , mkParsedImport "Control.Monad" False Nothing []
              , mkParsedImport "Data.List" False Nothing []
              ]
            sorted = sortImports imports
        map piModule sorted `shouldBe` ["Control.Monad", "Data.List", "Data.Text"]

      it "groups Prelude first" $ do
        let imports =
              [ mkParsedImport "Data.Text" False Nothing []
              , mkParsedImport "Prelude" False Nothing []
              ]
            sorted = sortImports imports
        piModule (head sorted) `shouldBe` "Prelude"

    describe "deduplicateImports" $ do
      it "merges duplicate imports" $ do
        let imports =
              [ mkParsedImport "Data.Text" False Nothing ["pack"]
              , mkParsedImport "Data.Text" False Nothing ["unpack"]
              ]
            result = deduplicateImports imports
        length result `shouldBe` 1
        piExplicit (head result) `shouldContain` ["pack"]
        piExplicit (head result) `shouldContain` ["unpack"]

      it "keeps different modules separate" $ do
        let imports =
              [ mkParsedImport "Data.Text" False Nothing ["pack"]
              , mkParsedImport "Data.List" False Nothing ["sort"]
              ]
            result = deduplicateImports imports
        length result `shouldBe` 2

    describe "ImportManagerConfig" $ do
      it "has sensible defaults" $ do
        imcAddMissingImports defaultImportConfig `shouldBe` True
        imcRemoveUnusedImports defaultImportConfig `shouldBe` True
        imcOrganizeImports defaultImportConfig `shouldBe` False
        imcExplicitImports defaultImportConfig `shouldBe` True

-- Helper functions
mkParsedImport :: T.Text -> Bool -> Maybe T.Text -> [T.Text] -> ParsedImport
mkParsedImport modName isQual alias expl = ParsedImport
  { piModule = modName
  , piQualified = isQual
  , piAlias = alias
  , piHiding = False
  , piExplicit = expl
  , piPackage = Nothing
  , piSpan = Nothing
  , piIsSafe = False
  , piIsSource = False
  }

mkFixImport :: T.Text -> [T.Text] -> FixImport
mkFixImport modName symbols = FixImport
  { fimpModule = modName
  , fimpSymbols = map (\s -> ImportSymbol s ISTFunction []) symbols
  , fimpQualified = Nothing
  , fimpHiding = False
  , fimpPackage = Nothing
  }

mkFixWithImports :: [FixImport] -> [T.Text] -> Fix
mkFixWithImports addImps removeImps = Fix
  { fixTitle = "Test fix"
  , fixEdits = []
  , fixIsPreferred = False
  , fixAddImports = addImps
  , fixRemoveImports = removeImps
  , fixCategory = FCStyle
  , fixSafety = FSMostly
  }
