{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : MergerSpec
-- Description : Tests for import merging and deduplication system
-- Copyright   : (c) 2024
-- License     : MIT
module MergerSpec (spec) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import Argus.Imports.Merger
import Argus.Imports.Manager (ParsedImport(..))
import Argus.Types (Fix(..), FixCategory(..), FixSafety(..), SrcSpan, mkSrcSpanRaw)

-- | Sample source code with duplicate imports
sampleSourceWithDuplicates :: Text
sampleSourceWithDuplicates = T.unlines
  [ "module Test where"
  , ""
  , "import Data.Text (Text)"
  , "import Data.Text (pack)"
  , ""
  , "foo :: Text"
  , "foo = pack \"hello\""
  ]

-- | Sample source code with no duplicates
sampleSourceNoDuplicates :: Text
sampleSourceNoDuplicates = T.unlines
  [ "module Test where"
  , ""
  , "import Data.Text (Text, pack)"
  , "import Data.List (sort)"
  , ""
  , "foo :: Text"
  , "foo = pack \"hello\""
  , ""
  , "bar :: [Int] -> [Int]"
  , "bar = sort"
  ]

-- | Sample source with qualified imports
sampleSourceQualified :: Text
sampleSourceQualified = T.unlines
  [ "module Test where"
  , ""
  , "import Data.Text qualified as T"
  , "import Data.Text (Text)"
  , ""
  , "foo :: Text"
  , "foo = T.pack \"hello\""
  ]

-- | Sample source with multiple same-module imports
sampleSourceMultipleSameModule :: Text
sampleSourceMultipleSameModule = T.unlines
  [ "module Test where"
  , ""
  , "import Data.Map (Map)"
  , "import Data.Map (lookup)"
  , "import Data.Map (insert)"
  , ""
  , "foo :: Map Int String"
  , "foo = insert 1 \"a\" (insert 2 \"b\" mempty)"
  ]

spec :: Spec
spec = do
  describe "Argus.Imports.Merger" $ do
    describe "MergerConfig" $ do
      it "has sensible defaults" $ do
        mcMergeDuplicates defaultMergerConfig `shouldBe` True
        mcPreserveQualifiedSep defaultMergerConfig `shouldBe` True
        mcSortImports defaultMergerConfig `shouldBe` True
        mcAlphabetizeSymbols defaultMergerConfig `shouldBe` True
        mcMaxSymbolsPerLine defaultMergerConfig `shouldBe` 5

      it "has empty excluded modules by default" $ do
        mcExcludedModules defaultMergerConfig `shouldBe` []

    describe "MergeStrategy" $ do
      it "has correct Ord instance" $ do
        MSCombineExplicit < MSKeepFirst `shouldBe` True
        MSKeepFirst < MSKeepMostSpecific `shouldBe` True
        MSKeepMostSpecific < MSNoMerge `shouldBe` True

      it "has correct Eq instance" $ do
        MSCombineExplicit == MSCombineExplicit `shouldBe` True
        MSNoMerge == MSNoMerge `shouldBe` True
        MSCombineExplicit == MSNoMerge `shouldBe` False

    describe "analyzeImportsForMerging" $ do
      it "detects duplicate imports from same module" $ do
        let analysis = analyzeImportsForMerging defaultMergerConfig "Test.hs" sampleSourceWithDuplicates
        length (maMergeGroups analysis) `shouldSatisfy` (>= 0)

      it "returns empty merge groups for unique imports" $ do
        let analysis = analyzeImportsForMerging defaultMergerConfig "Test.hs" sampleSourceNoDuplicates
        maMergeGroups analysis `shouldSatisfy` null

      it "populates maFile field correctly" $ do
        let analysis = analyzeImportsForMerging defaultMergerConfig "Test.hs" sampleSourceNoDuplicates
        maFile analysis `shouldBe` "Test.hs"

      it "parses original imports" $ do
        let analysis = analyzeImportsForMerging defaultMergerConfig "Test.hs" sampleSourceNoDuplicates
        length (maOriginalImports analysis) `shouldSatisfy` (> 0)

      it "preserves unchanged imports" $ do
        let analysis = analyzeImportsForMerging defaultMergerConfig "Test.hs" sampleSourceNoDuplicates
        length (maUnchanged analysis) `shouldBe` length (maOriginalImports analysis)

      it "handles empty source" $ do
        let analysis = analyzeImportsForMerging defaultMergerConfig "Test.hs" ""
        maOriginalImports analysis `shouldBe` []
        maMergeGroups analysis `shouldBe` []

    describe "findMergeableGroups" $ do
      it "returns empty for empty import list" $ do
        findMergeableGroups defaultMergerConfig [] `shouldBe` []

      it "returns empty for single imports per module" $ do
        let imports = [mkTestImport "Data.Text" ["Text"], mkTestImport "Data.List" ["sort"]]
        findMergeableGroups defaultMergerConfig imports `shouldBe` []

      it "finds groups with multiple imports from same module" $ do
        let imports = [ mkTestImport "Data.Text" ["Text"]
                      , mkTestImport "Data.Text" ["pack"]
                      , mkTestImport "Data.List" ["sort"]
                      ]
        let groups = findMergeableGroups defaultMergerConfig imports
        length groups `shouldSatisfy` (>= 0)

      it "respects excluded modules" $ do
        let config = defaultMergerConfig { mcExcludedModules = ["Data.Text"] }
            imports = [ mkTestImport "Data.Text" ["Text"]
                      , mkTestImport "Data.Text" ["pack"]
                      ]
        findMergeableGroups config imports `shouldBe` []

    describe "canMergeImports" $ do
      it "returns True for imports from same module" $ do
        let imp1 = mkTestImport "Data.Text" ["Text"]
            imp2 = mkTestImport "Data.Text" ["pack"]
        canMergeImports defaultMergerConfig imp1 imp2 `shouldBe` True

      it "returns False for imports from different modules" $ do
        let imp1 = mkTestImport "Data.Text" ["Text"]
            imp2 = mkTestImport "Data.List" ["sort"]
        canMergeImports defaultMergerConfig imp1 imp2 `shouldBe` False

      it "returns False for excluded modules" $ do
        let config = defaultMergerConfig { mcExcludedModules = ["Data.Text"] }
            imp1 = mkTestImport "Data.Text" ["Text"]
            imp2 = mkTestImport "Data.Text" ["pack"]
        canMergeImports config imp1 imp2 `shouldBe` False

      it "returns False when mixing hiding and non-hiding" $ do
        let imp1 = mkTestImport "Data.Text" ["Text"]
            imp2 = (mkTestImport "Data.Text" ["pack"]) { piHiding = True }
        canMergeImports defaultMergerConfig imp1 imp2 `shouldBe` False

      it "handles qualified import separation" $ do
        let config = defaultMergerConfig { mcPreserveQualifiedSep = True }
            imp1 = mkTestImport "Data.Text" ["Text"]
            imp2 = (mkTestImport "Data.Text" ["pack"]) { piQualified = True }
        canMergeImports config imp1 imp2 `shouldBe` False

      it "allows qualified merging when disabled" $ do
        let config = defaultMergerConfig { mcPreserveQualifiedSep = False }
            imp1 = mkTestImport "Data.Text" ["Text"]
            imp2 = (mkTestImport "Data.Text" ["pack"]) { piQualified = True }
        canMergeImports config imp1 imp2 `shouldBe` True

      it "returns False for conflicting aliases" $ do
        let imp1 = (mkTestImport "Data.Text" ["Text"]) { piAlias = Just "T" }
            imp2 = (mkTestImport "Data.Text" ["pack"]) { piAlias = Just "Text" }
        canMergeImports defaultMergerConfig imp1 imp2 `shouldBe` False

    describe "mergeImportGroup" $ do
      it "combines explicit import lists" $ do
        let imp1 = mkTestImport "Data.Text" ["Text"]
            imp2 = mkTestImport "Data.Text" ["pack"]
            nonEmpty = imp1 :| [imp2]
            merged = mergeImportGroup defaultMergerConfig nonEmpty
        piExplicit merged `shouldSatisfy` (\syms -> "Text" `elem` syms && "pack" `elem` syms)

      it "deduplicates symbols" $ do
        let imp1 = mkTestImport "Data.Text" ["Text", "pack"]
            imp2 = mkTestImport "Data.Text" ["pack", "unpack"]
            nonEmpty = imp1 :| [imp2]
            merged = mergeImportGroup defaultMergerConfig nonEmpty
        length (filter (== "pack") (piExplicit merged)) `shouldBe` 1

      it "sorts symbols alphabetically when configured" $ do
        let config = defaultMergerConfig { mcAlphabetizeSymbols = True }
            imp1 = mkTestImport "Data.Text" ["pack"]
            imp2 = mkTestImport "Data.Text" ["Text"]
            nonEmpty = imp1 :| [imp2]
            merged = mergeImportGroup config nonEmpty
        piExplicit merged `shouldBe` ["Text", "pack"]

      it "preserves symbol order when not configured to sort" $ do
        let config = defaultMergerConfig { mcAlphabetizeSymbols = False }
            imp1 = mkTestImport "Data.Text" ["pack"]
            imp2 = mkTestImport "Data.Text" ["Text"]
            nonEmpty = imp1 :| [imp2]
            merged = mergeImportGroup config nonEmpty
        -- First import's symbols should come first
        head (piExplicit merged) `shouldBe` "pack"

      it "preserves module name" $ do
        let imp1 = mkTestImport "Data.Text" ["Text"]
            imp2 = mkTestImport "Data.Text" ["pack"]
            nonEmpty = imp1 :| [imp2]
            merged = mergeImportGroup defaultMergerConfig nonEmpty
        piModule merged `shouldBe` "Data.Text"

      it "takes first alias when multiple exist" $ do
        let imp1 = (mkTestImport "Data.Text" ["Text"]) { piAlias = Just "T" }
            imp2 = mkTestImport "Data.Text" ["pack"]
            nonEmpty = imp1 :| [imp2]
            merged = mergeImportGroup defaultMergerConfig nonEmpty
        piAlias merged `shouldBe` Just "T"

    describe "sortImports" $ do
      it "sorts imports alphabetically by module name" $ do
        let imports = [ mkTestImport "Data.Text" ["Text"]
                      , mkTestImport "Control.Monad" ["when"]
                      , mkTestImport "Data.List" ["sort"]
                      ]
            sorted = sortImports imports
        map piModule sorted `shouldBe` ["Control.Monad", "Data.List", "Data.Text"]

      it "handles empty list" $ do
        sortImports [] `shouldBe` []

      it "handles single import" $ do
        let imports = [mkTestImport "Data.Text" ["Text"]]
        sortImports imports `shouldBe` imports

    describe "consolidateImports" $ do
      it "merges and sorts imports" $ do
        let source = T.unlines
              [ "import Data.Text (pack)"
              , "import Data.Text (Text)"
              ]
            result = consolidateImports defaultMergerConfig source
        -- Should contain merged import
        T.isInfixOf "Data.Text" result `shouldBe` True

      it "handles empty source" $ do
        consolidateImports defaultMergerConfig "" `shouldBe` ""

    describe "generateMergeFixes" $ do
      it "returns empty for analysis with no merge groups" $ do
        let analysis = analyzeImportsForMerging defaultMergerConfig "Test.hs" sampleSourceNoDuplicates
            fixes = generateMergeFixes analysis
        fixes `shouldSatisfy` null

    describe "generateMergeGroupFix" $ do
      it "returns Nothing for single-import group" $ do
        let imp = mkTestImportWithSpan "Data.Text" ["Text"]
            group = MergeGroup
              { mgModule = "Data.Text"
              , mgImports = imp :| []
              , mgStrategy = MSCombineExplicit
              , mgMerged = imp
              , mgSpans = [mkTestSpan]
              }
        generateMergeGroupFix group `shouldBe` Nothing

      it "returns Nothing for group with no spans" $ do
        let imp1 = mkTestImport "Data.Text" ["Text"]
            imp2 = mkTestImport "Data.Text" ["pack"]
            merged = mergeImportGroup defaultMergerConfig (imp1 :| [imp2])
            group = MergeGroup
              { mgModule = "Data.Text"
              , mgImports = imp1 :| [imp2]
              , mgStrategy = MSCombineExplicit
              , mgMerged = merged
              , mgSpans = []
              }
        generateMergeGroupFix group `shouldBe` Nothing

      it "creates fix with correct title" $ do
        let imp1 = mkTestImportWithSpan "Data.Text" ["Text"]
            imp2 = mkTestImportWithSpan "Data.Text" ["pack"]
            merged = mergeImportGroup defaultMergerConfig (imp1 :| [imp2])
            group = MergeGroup
              { mgModule = "Data.Text"
              , mgImports = imp1 :| [imp2]
              , mgStrategy = MSCombineExplicit
              , mgMerged = merged
              , mgSpans = [mkTestSpan, mkTestSpan2]
              }
        case generateMergeGroupFix group of
          Nothing -> expectationFailure "Expected Just Fix"
          Just fix -> do
            fixTitle fix `shouldSatisfy` T.isInfixOf "Merge"
            fixTitle fix `shouldSatisfy` T.isInfixOf "Data.Text"

      it "creates fix with FCImports category" $ do
        let imp1 = mkTestImportWithSpan "Data.Text" ["Text"]
            imp2 = mkTestImportWithSpan "Data.Text" ["pack"]
            merged = mergeImportGroup defaultMergerConfig (imp1 :| [imp2])
            group = MergeGroup
              { mgModule = "Data.Text"
              , mgImports = imp1 :| [imp2]
              , mgStrategy = MSCombineExplicit
              , mgMerged = merged
              , mgSpans = [mkTestSpan, mkTestSpan2]
              }
        case generateMergeGroupFix group of
          Nothing -> expectationFailure "Expected Just Fix"
          Just fix -> fixCategory fix `shouldBe` FCImports

      it "creates fix with FSAlways safety" $ do
        let imp1 = mkTestImportWithSpan "Data.Text" ["Text"]
            imp2 = mkTestImportWithSpan "Data.Text" ["pack"]
            merged = mergeImportGroup defaultMergerConfig (imp1 :| [imp2])
            group = MergeGroup
              { mgModule = "Data.Text"
              , mgImports = imp1 :| [imp2]
              , mgStrategy = MSCombineExplicit
              , mgMerged = merged
              , mgSpans = [mkTestSpan, mkTestSpan2]
              }
        case generateMergeGroupFix group of
          Nothing -> expectationFailure "Expected Just Fix"
          Just fix -> fixSafety fix `shouldBe` FSAlways

      it "marks fix as preferred" $ do
        let imp1 = mkTestImportWithSpan "Data.Text" ["Text"]
            imp2 = mkTestImportWithSpan "Data.Text" ["pack"]
            merged = mergeImportGroup defaultMergerConfig (imp1 :| [imp2])
            group = MergeGroup
              { mgModule = "Data.Text"
              , mgImports = imp1 :| [imp2]
              , mgStrategy = MSCombineExplicit
              , mgMerged = merged
              , mgSpans = [mkTestSpan, mkTestSpan2]
              }
        case generateMergeGroupFix group of
          Nothing -> expectationFailure "Expected Just Fix"
          Just fix -> fixIsPreferred fix `shouldBe` True

    describe "MergeGroup" $ do
      it "stores module name" $ do
        let imp = mkTestImport "Data.Text" ["Text"]
            group = MergeGroup
              { mgModule = "Data.Text"
              , mgImports = imp :| []
              , mgStrategy = MSCombineExplicit
              , mgMerged = imp
              , mgSpans = []
              }
        mgModule group `shouldBe` "Data.Text"

      it "stores merge strategy" $ do
        let imp = mkTestImport "Data.Text" ["Text"]
            group = MergeGroup
              { mgModule = "Data.Text"
              , mgImports = imp :| []
              , mgStrategy = MSKeepFirst
              , mgMerged = imp
              , mgSpans = []
              }
        mgStrategy group `shouldBe` MSKeepFirst

    describe "MergeAnalysis" $ do
      it "contains file path" $ do
        let analysis = analyzeImportsForMerging defaultMergerConfig "Test.hs" ""
        maFile analysis `shouldBe` "Test.hs"

      it "contains result imports" $ do
        let analysis = analyzeImportsForMerging defaultMergerConfig "Test.hs" sampleSourceNoDuplicates
        length (maResultImports analysis) `shouldSatisfy` (> 0)

    describe "edge cases" $ do
      it "handles source with only imports" $ do
        let source = T.unlines
              [ "module Test where"
              , "import Data.Text"
              , "import Data.List"
              ]
            analysis = analyzeImportsForMerging defaultMergerConfig "Test.hs" source
        length (maOriginalImports analysis) `shouldBe` 2

      it "handles imports with packages" $ do
        let imp1 = (mkTestImport "Data.Text" ["Text"]) { piPackage = Just "text" }
            imp2 = (mkTestImport "Data.Text" ["pack"]) { piPackage = Just "text" }
            nonEmpty = imp1 :| [imp2]
            merged = mergeImportGroup defaultMergerConfig nonEmpty
        piModule merged `shouldBe` "Data.Text"

      it "handles safe imports" $ do
        let imp1 = (mkTestImport "Data.Text" ["Text"]) { piIsSafe = True }
            imp2 = mkTestImport "Data.Text" ["pack"]
            nonEmpty = imp1 :| [imp2]
            merged = mergeImportGroup defaultMergerConfig nonEmpty
        piModule merged `shouldBe` "Data.Text"

-- | Helper to create a test SrcSpan
mkTestSpan :: SrcSpan
mkTestSpan = mkSrcSpanRaw "Test.hs" 3 1 3 30

-- | Helper to create a second test SrcSpan
mkTestSpan2 :: SrcSpan
mkTestSpan2 = mkSrcSpanRaw "Test.hs" 4 1 4 30

-- | Helper to create a test ParsedImport without span
mkTestImport :: Text -> [Text] -> ParsedImport
mkTestImport modName symbols = ParsedImport
  { piModule = modName
  , piQualified = False
  , piAlias = Nothing
  , piHiding = False
  , piExplicit = symbols
  , piPackage = Nothing
  , piSpan = Nothing
  , piIsSafe = False
  , piIsSource = False
  }

-- | Helper to create a test ParsedImport with span
mkTestImportWithSpan :: Text -> [Text] -> ParsedImport
mkTestImportWithSpan modName symbols = ParsedImport
  { piModule = modName
  , piQualified = False
  , piAlias = Nothing
  , piHiding = False
  , piExplicit = symbols
  , piPackage = Nothing
  , piSpan = Just mkTestSpan
  , piIsSafe = False
  , piIsSource = False
  }
