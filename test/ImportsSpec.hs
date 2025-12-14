{-# LANGUAGE OverloadedStrings #-}

module ImportsSpec (spec) where

import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Hspec

import Argus.Analysis.Syntactic (ImportInfo(..), ImportItem(..))
import Argus.Config (ImportsConfig(..))
import Argus.Rules.Imports
import Argus.Types (Diagnostic(..), SrcSpan(..), noSrcSpan)

spec :: Spec
spec = do
  describe "Argus.Rules.Imports" $ do
    -- Shared config for tests
    let defaultImportsConfig = ImportsConfig
          { importsRemoveUnused = True
          , importsSuggestQualified = []
          , importsAllowUnqualifiedTypes = True
          , importsAllowUnqualifiedOperators = True
          , importsRequireExplicit = False
          , importsCombine = True
          , importsThRoots = []
          , importsSuppressForTH = True
          }

    describe "checkImports" $ do
      describe "unused import detection" $ do
        it "detects unused explicit imports" $ do
          let config = defaultImportsConfig { importsRemoveUnused = True }
              imports = [mkImportWithExplicit "Data.Text" ["pack", "unpack", "Text"]]
              usedNames = Set.fromList ["pack", "Text"]
              diags = checkImports config "src/Main.hs" imports usedNames
          length diags `shouldSatisfy` (>= 1)
          any (T.isInfixOf "unpack" . diagMessage) diags `shouldBe` True

        it "returns empty when all imports used" $ do
          let config = defaultImportsConfig { importsRemoveUnused = True }
              imports = [mkImportWithExplicit "Data.Text" ["pack", "Text"]]
              usedNames = Set.fromList ["pack", "Text"]
              diags = checkImports config "src/Main.hs" imports usedNames
          diags `shouldBe` []

        it "skips check when disabled" $ do
          let config = defaultImportsConfig { importsRemoveUnused = False }
              imports = [mkImportWithExplicit "Data.Text" ["pack", "unpack"]]
              usedNames = Set.fromList ["pack"]
              diags = checkImports config "src/Main.hs" imports usedNames
          any (T.isInfixOf "Unused" . diagMessage) diags `shouldBe` False

      describe "qualified import suggestions" $ do
        it "suggests qualified for configured modules" $ do
          let config = defaultImportsConfig { importsSuggestQualified = ["Data.Text"] }
              imports = [mkImport "Data.Text" False Nothing]
              diags = checkImports config "src/Main.hs" imports Set.empty
          length diags `shouldSatisfy` (>= 1)
          any (T.isInfixOf "qualified" . diagMessage) diags `shouldBe` True

        it "does not flag already qualified imports" $ do
          let config = defaultImportsConfig { importsSuggestQualified = ["Data.Text"] }
              imports = [mkImport "Data.Text" True (Just "T")]
              diags = checkImports config "src/Main.hs" imports Set.empty
          any (T.isInfixOf "Consider using qualified" . diagMessage) diags `shouldBe` False

        it "does not suggest for non-configured modules" $ do
          let config = defaultImportsConfig { importsSuggestQualified = ["Data.Text"] }
              imports = [mkImport "Data.List" False Nothing]
              diags = checkImports config "src/Main.hs" imports Set.empty
          any (T.isInfixOf "Consider using qualified" . diagMessage) diags `shouldBe` False

      describe "explicit import requirements" $ do
        it "flags implicit imports when required" $ do
          let config = defaultImportsConfig { importsRequireExplicit = True }
              imports = [mkImport "Data.Text" False Nothing]  -- no explicit list
              diags = checkImports config "src/Main.hs" imports Set.empty
          length diags `shouldSatisfy` (>= 1)
          any (T.isInfixOf "explicit" . diagMessage) diags `shouldBe` True

        it "does not flag imports with explicit lists" $ do
          let config = defaultImportsConfig { importsRequireExplicit = True }
              imports = [mkImportWithExplicit "Data.Text" ["pack"]]
              diags = checkImports config "src/Main.hs" imports Set.empty
          any (T.isInfixOf "explicit import list" . diagMessage) diags `shouldBe` False

        it "does not flag hiding imports" $ do
          let config = defaultImportsConfig { importsRequireExplicit = True }
              imports = [mkImportHiding "Data.Text"]
              diags = checkImports config "src/Main.hs" imports Set.empty
          any (T.isInfixOf "explicit import list" . diagMessage) diags `shouldBe` False

    describe "detectUnusedImports" $ do
      it "returns empty for implicit imports" $ do
        let imports = [mkImport "Data.Text" False Nothing]
            usedNames = Set.fromList ["foo"]
        detectUnusedImports "src/Main.hs" imports usedNames `shouldBe` []

      it "detects fully unused explicit imports" $ do
        let imports = [mkImportWithExplicit "Data.Text" ["pack", "unpack"]]
            usedNames = Set.empty
        let diags = detectUnusedImports "src/Main.hs" imports usedNames
        length diags `shouldSatisfy` (>= 1)

      it "detects partially unused explicit imports" $ do
        let imports = [mkImportWithExplicit "Data.Text" ["pack", "unpack", "Text"]]
            usedNames = Set.fromList ["pack"]
        let diags = detectUnusedImports "src/Main.hs" imports usedNames
        length diags `shouldSatisfy` (>= 1)
        any (T.isInfixOf "unpack" . diagMessage) diags `shouldBe` True
        any (T.isInfixOf "Text" . diagMessage) diags `shouldBe` True

    describe "suggestQualifiedImports" $ do
      let suggestConfig mods = defaultImportsConfig { importsSuggestQualified = mods }

      it "suggests for matching modules" $ do
        let imports = [mkImport "Data.Map" False Nothing]
            diags = suggestQualifiedImports "src/Main.hs" (suggestConfig ["Data.Map"]) imports
        length diags `shouldBe` 1
        T.isInfixOf "qualified" (diagMessage (head diags)) `shouldBe` True

      it "returns empty for non-matching modules" $ do
        let imports = [mkImport "Data.List" False Nothing]
            diags = suggestQualifiedImports "src/Main.hs" (suggestConfig ["Data.Map"]) imports
        diags `shouldBe` []

      it "returns empty for already qualified" $ do
        let imports = [mkImport "Data.Map" True (Just "M")]
            diags = suggestQualifiedImports "src/Main.hs" (suggestConfig ["Data.Map"]) imports
        diags `shouldBe` []

    describe "optimizeImports" $ do
      it "removes unused imports" $ do
        let imports =
              [ mkImportWithExplicit "Data.Text" ["pack"]
              , mkImportWithExplicit "Data.List" ["sort"]
              ]
            usedNames = Set.fromList ["pack"]
            optimized = optimizeImports imports usedNames
        length optimized `shouldBe` 1
        iiModuleName (head optimized) `shouldBe` "Data.Text"

      it "keeps implicit imports" $ do
        let imports = [mkImport "Prelude" False Nothing]
            usedNames = Set.empty
            optimized = optimizeImports imports usedNames
        length optimized `shouldBe` 1

    describe "combineImports" $ do
      it "combines imports from same module" $ do
        let imports =
              [ mkImportWithExplicit "Data.Text" ["pack"]
              , mkImportWithExplicit "Data.Text" ["unpack"]
              ]
            combined = combineImports imports
        length combined `shouldBe` 1

      it "keeps different modules separate" $ do
        let imports =
              [ mkImportWithExplicit "Data.Text" ["pack"]
              , mkImportWithExplicit "Data.List" ["sort"]
              ]
            combined = combineImports imports
        length combined `shouldBe` 2

    describe "sortImports" $ do
      it "sorts imports alphabetically" $ do
        let imports =
              [ mkImport "Data.Text" False Nothing
              , mkImport "Control.Monad" False Nothing
              , mkImport "Data.List" False Nothing
              ]
            sorted = sortImports imports
        map iiModuleName sorted `shouldBe` ["Control.Monad", "Data.List", "Data.Text"]

    describe "diagnostic properties" $ do
      it "uses imports/ prefix for unused import codes" $ do
        let imports = [mkImportWithExplicit "Data.Text" ["unused"]]
            diags = detectUnusedImports "src/Main.hs" imports Set.empty
        all (maybe False (T.isPrefixOf "imports/") . diagCode) diags `shouldBe` True

      it "uses imports/ prefix for qualified suggestion codes" $ do
        let suggestConfig = defaultImportsConfig { importsSuggestQualified = ["Data.Map"] }
            imports = [mkImport "Data.Map" False Nothing]
            diags = suggestQualifiedImports "src/Main.hs" suggestConfig imports
        all (maybe False (T.isPrefixOf "imports/") . diagCode) diags `shouldBe` True

    describe "operator import detection" $ do
      it "detects unused operator imports" $ do
        -- Operator ".=" is imported but not used
        let imports = [mkImportWithOperator "Data.Aeson" ".="]
            usedNames = Set.empty  -- operator not used
            diags = detectUnusedImports "src/Main.hs" imports usedNames
        length diags `shouldBe` 1
        T.isInfixOf ".=" (diagMessage (head diags)) `shouldBe` True

      it "does not flag used operator imports" $ do
        -- Operator ".=" is imported and used
        let imports = [mkImportWithOperator "Data.Aeson" ".="]
            usedNames = Set.fromList [".="]  -- operator is used
            diags = detectUnusedImports "src/Main.hs" imports usedNames
        diags `shouldBe` []

      it "detects unused operator among used values" $ do
        -- Mix of used and unused imports
        let imports = [mkImportMixed "Data.Aeson" ["object", "encode"] [".=", ".:"] []]
            usedNames = Set.fromList ["object", ".="]  -- encode and .: unused
            diags = detectUnusedImports "src/Main.hs" imports usedNames
        length diags `shouldBe` 1
        T.isInfixOf "encode" (diagMessage (head diags)) `shouldBe` True
        T.isInfixOf ".:" (diagMessage (head diags)) `shouldBe` True

      it "handles multiple operators correctly" $ do
        let imports = [mkImportMixed "Data.Aeson" [] [".=", ".:?", ".:"] []]
            usedNames = Set.fromList [".=", ".:?"]  -- .: unused
            diags = detectUnusedImports "src/Main.hs" imports usedNames
        length diags `shouldBe` 1
        T.isInfixOf ".:" (diagMessage (head diags)) `shouldBe` True
        T.isInfixOf ".=" (diagMessage (head diags)) `shouldBe` False  -- used

    describe "wildcard import handling" $ do
      it "does not flag wildcard imports as unused" $ do
        -- Wildcard imports like ToJSON (..) should never be flagged
        -- because we can't know what methods they bring in
        let imports = [mkImportWithWildcard "Data.Aeson" "ToJSON"]
            usedNames = Set.empty  -- even with no used names
            diags = detectUnusedImports "src/Main.hs" imports usedNames
        diags `shouldBe` []

      it "does not flag wildcard even when methods appear used" $ do
        -- Even if we happen to use a method, wildcard should still not be flagged
        let imports = [mkImportWithWildcard "Data.Aeson" "ToJSON"]
            usedNames = Set.fromList ["toJSON"]
            diags = detectUnusedImports "src/Main.hs" imports usedNames
        diags `shouldBe` []

      it "flags unused values but not wildcards in mixed imports" $ do
        -- import Data.Aeson (ToJSON (..), encode, decode)
        -- If only toJSON is used, encode/decode should be flagged but not ToJSON (..)
        let imports = [mkImportMixed "Data.Aeson" ["encode", "decode"] [] ["ToJSON"]]
            usedNames = Set.fromList ["toJSON"]  -- method from ToJSON class used
            diags = detectUnusedImports "src/Main.hs" imports usedNames
        length diags `shouldBe` 1
        T.isInfixOf "encode" (diagMessage (head diags)) `shouldBe` True
        T.isInfixOf "decode" (diagMessage (head diags)) `shouldBe` True
        T.isInfixOf "ToJSON" (diagMessage (head diags)) `shouldBe` False

      it "handles multiple wildcards correctly" $ do
        let imports = [mkImportMixed "Data.Aeson" [] [] ["ToJSON", "FromJSON"]]
            usedNames = Set.empty
            diags = detectUnusedImports "src/Main.hs" imports usedNames
        diags `shouldBe` []  -- neither wildcard should be flagged

    describe "combined operator and wildcard handling" $ do
      it "correctly handles mixed imports with operators, wildcards, and values" $ do
        -- Real-world case: import Data.Aeson (ToJSON (..), (.=), object, encode)
        let imports = [mkImportMixed "Data.Aeson" ["object", "encode"] [".=", ".:"] ["ToJSON"]]
            usedNames = Set.fromList ["toJSON", ".=", "object"]  -- encode and .: unused
            diags = detectUnusedImports "src/Main.hs" imports usedNames
        length diags `shouldBe` 1
        T.isInfixOf "encode" (diagMessage (head diags)) `shouldBe` True
        T.isInfixOf ".:" (diagMessage (head diags)) `shouldBe` True
        -- Wildcards and used items should not appear
        T.isInfixOf "ToJSON" (diagMessage (head diags)) `shouldBe` False
        T.isInfixOf ".=" (diagMessage (head diags)) `shouldBe` False
        T.isInfixOf "object" (diagMessage (head diags)) `shouldBe` False

    describe "class-with-methods import handling" $ do
      it "does not flag class when child method is used" $ do
        -- import FloHam.Class.Lens.HasTitle (HasTitle (title))
        -- Only 'title' is used in code, but HasTitle shouldn't be flagged
        let imports = [mkImportWithMethods "FloHam.Class.Lens.HasTitle" "HasTitle" ["title"]]
            usedNames = Set.fromList ["title"]  -- Only method used
            diags = detectUnusedImports "src/Main.hs" imports usedNames
        diags `shouldBe` []

      it "flags class when neither class nor children are used" $ do
        -- import FloHam.Class.Lens.HasTitle (HasTitle (title))
        -- Neither HasTitle nor title is used
        let imports = [mkImportWithMethods "FloHam.Class.Lens.HasTitle" "HasTitle" ["title"]]
            usedNames = Set.empty
            diags = detectUnusedImports "src/Main.hs" imports usedNames
        length diags `shouldBe` 1
        T.isInfixOf "HasTitle" (diagMessage (head diags)) `shouldBe` True

      it "does not flag class when class itself is used" $ do
        -- import Data.Aeson (ToJSON (toJSON))
        -- Both ToJSON and toJSON could be used in deriving or constraint
        let imports = [mkImportWithMethods "Data.Aeson" "ToJSON" ["toJSON"]]
            usedNames = Set.fromList ["ToJSON"]  -- Class used in constraint
            diags = detectUnusedImports "src/Main.hs" imports usedNames
        diags `shouldBe` []

      it "handles multiple children correctly" $ do
        -- import Control.Lens (Lens' (view, set, over))
        let imports = [mkImportWithMethods "Control.Lens" "Lens'" ["view", "set", "over"]]
            usedNames = Set.fromList ["view"]  -- Only one method used
            diags = detectUnusedImports "src/Main.hs" imports usedNames
        diags `shouldBe` []  -- Lens' should not be flagged

      it "handles class with multiple children none used" $ do
        let imports = [mkImportWithMethods "Control.Lens" "Lens'" ["view", "set", "over"]]
            usedNames = Set.fromList ["somethingElse"]  -- None of the children used
            diags = detectUnusedImports "src/Main.hs" imports usedNames
        length diags `shouldBe` 1
        T.isInfixOf "Lens'" (diagMessage (head diags)) `shouldBe` True

-- Helper functions to create test data
mkImport :: T.Text -> Bool -> Maybe T.Text -> ImportInfo
mkImport modName isQualified alias = ImportInfo
  { iiModuleName = modName
  , iiQualified = isQualified
  , iiAlias = alias
  , iiSpan = noSrcSpan
  , iiHiding = False
  , iiExplicit = Nothing
  }

mkImportWithExplicit :: T.Text -> [T.Text] -> ImportInfo
mkImportWithExplicit modName names = ImportInfo
  { iiModuleName = modName
  , iiQualified = False
  , iiAlias = Nothing
  , iiSpan = noSrcSpan
  , iiHiding = False
  , iiExplicit = Just $ map mkValueItem names
  }
  where
    mkValueItem n = ImportItem { importItemName = n, importItemIsType = False, importItemIsOperator = False, importItemIsWildcard = False, importItemChildren = [] }

mkImportHiding :: T.Text -> ImportInfo
mkImportHiding modName = ImportInfo
  { iiModuleName = modName
  , iiQualified = False
  , iiAlias = Nothing
  , iiSpan = noSrcSpan
  , iiHiding = True
  , iiExplicit = Nothing
  }

-- | Create an import with an operator
mkImportWithOperator :: T.Text -> T.Text -> ImportInfo
mkImportWithOperator modName opName = ImportInfo
  { iiModuleName = modName
  , iiQualified = False
  , iiAlias = Nothing
  , iiSpan = noSrcSpan
  , iiHiding = False
  , iiExplicit = Just [mkOperatorItem opName]
  }
  where
    mkOperatorItem n = ImportItem { importItemName = n, importItemIsType = False, importItemIsOperator = True, importItemIsWildcard = False, importItemChildren = [] }

-- | Create an import with a wildcard (like ToJSON (..))
mkImportWithWildcard :: T.Text -> T.Text -> ImportInfo
mkImportWithWildcard modName typeName = ImportInfo
  { iiModuleName = modName
  , iiQualified = False
  , iiAlias = Nothing
  , iiSpan = noSrcSpan
  , iiHiding = False
  , iiExplicit = Just [mkWildcardItem typeName]
  }
  where
    mkWildcardItem n = ImportItem { importItemName = n, importItemIsType = True, importItemIsOperator = False, importItemIsWildcard = True, importItemChildren = [] }

-- | Create an import with a class and explicit methods like HasTitle (title)
mkImportWithMethods :: T.Text -> T.Text -> [T.Text] -> ImportInfo
mkImportWithMethods modName className methods = ImportInfo
  { iiModuleName = modName
  , iiQualified = False
  , iiAlias = Nothing
  , iiSpan = noSrcSpan
  , iiHiding = False
  , iiExplicit = Just [mkClassWithMethods className methods]
  }
  where
    mkClassWithMethods n ms = ImportItem { importItemName = n, importItemIsType = True, importItemIsOperator = False, importItemIsWildcard = False, importItemChildren = ms }

-- | Create an import with mixed items (regular, operator, wildcard)
mkImportMixed :: T.Text -> [T.Text] -> [T.Text] -> [T.Text] -> ImportInfo
mkImportMixed modName values operators wildcards = ImportInfo
  { iiModuleName = modName
  , iiQualified = False
  , iiAlias = Nothing
  , iiSpan = noSrcSpan
  , iiHiding = False
  , iiExplicit = Just $ map mkValueItem values ++ map mkOperatorItem operators ++ map mkWildcardItem wildcards
  }
  where
    mkValueItem n = ImportItem { importItemName = n, importItemIsType = False, importItemIsOperator = False, importItemIsWildcard = False, importItemChildren = [] }
    mkOperatorItem n = ImportItem { importItemName = n, importItemIsType = False, importItemIsOperator = True, importItemIsWildcard = False, importItemChildren = [] }
    mkWildcardItem n = ImportItem { importItemName = n, importItemIsType = True, importItemIsOperator = False, importItemIsWildcard = True, importItemChildren = [] }
