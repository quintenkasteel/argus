{-# LANGUAGE OverloadedStrings #-}

module QualifyImportSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Hspec

import Argus.Config (AliasStrategy(..), QualifyImportConfig(..), defaultQualifyImportConfig)
import Argus.Refactor.QualifyImport
import Argus.Types (Fix(..), FixEdit(..), SrcSpan(..))

spec :: Spec
spec = do
  describe "Argus.Refactor.QualifyImport" $ do
    describe "standardAliases" $ do
      it "contains standard Data.Text alias" $ do
        Map.lookup "Data.Text" standardAliases `shouldBe` Just "T"

      it "contains standard Data.ByteString alias" $ do
        Map.lookup "Data.ByteString" standardAliases `shouldBe` Just "BS"

      it "contains standard Data.Map alias" $ do
        Map.lookup "Data.Map" standardAliases `shouldBe` Just "M"

      it "contains standard Data.Map.Strict alias" $ do
        Map.lookup "Data.Map.Strict" standardAliases `shouldBe` Just "M"

      it "contains standard Data.Set alias" $ do
        Map.lookup "Data.Set" standardAliases `shouldBe` Just "S"

      it "contains standard Data.Text.Lazy alias" $ do
        Map.lookup "Data.Text.Lazy" standardAliases `shouldBe` Just "TL"

      it "contains standard Data.ByteString.Lazy alias" $ do
        Map.lookup "Data.ByteString.Lazy" standardAliases `shouldBe` Just "BL"

      it "contains Control.Concurrent.STM alias" $ do
        Map.lookup "Control.Concurrent.STM" standardAliases `shouldBe` Just "STM"

      it "contains Data.List.NonEmpty alias" $ do
        Map.lookup "Data.List.NonEmpty" standardAliases `shouldBe` Just "NE"

    describe "suggestAlias (default config with LastPart strategy)" $ do
      it "generates alias from last component" $ do
        -- Default uses LastPart strategy
        suggestAlias "Data.Text" `shouldBe` "Text"

      it "handles Strict variant by using parent component" $ do
        -- Strict/Lazy fallback to second-to-last component
        suggestAlias "Data.Map.Strict" `shouldBe` "Map"

      it "generates alias from last component for custom modules" $ do
        suggestAlias "My.Custom.Module" `shouldBe` "Module"

      it "preserves natural casing of short last components" $ do
        -- With preferUppercase = False (default), "Req" stays "Req"
        suggestAlias "Network.HTTP.Req" `shouldBe` "Req"

    describe "generateAliasFromStrategy" $ do
      describe "LastPart strategy" $ do
        it "uses last module component" $ do
          generateAliasFromStrategy LastPart "Data.Text" `shouldBe` "Text"

        it "uses last component for multi-part modules" $ do
          generateAliasFromStrategy LastPart "Control.Monad.Reader" `shouldBe` "Reader"

        it "handles Strict variant" $ do
          generateAliasFromStrategy LastPart "Data.Map.Strict" `shouldBe` "Map"

        it "handles Lazy variant" $ do
          generateAliasFromStrategy LastPart "Data.ByteString.Lazy" `shouldBe` "ByteString"

      describe "FirstLetter strategy" $ do
        it "uses first letter of last component" $ do
          generateAliasFromStrategy FirstLetter "Data.Text" `shouldBe` "T"

        it "handles multi-part modules" $ do
          generateAliasFromStrategy FirstLetter "Control.Monad.Reader" `shouldBe` "R"

        it "handles Strict variant" $ do
          generateAliasFromStrategy FirstLetter "Data.Map.Strict" `shouldBe` "M"

        it "handles Lazy variant" $ do
          generateAliasFromStrategy FirstLetter "Data.ByteString.Lazy" `shouldBe` "B"

      describe "Initials strategy" $ do
        it "uses initials of all components" $ do
          generateAliasFromStrategy Initials "Data.Text" `shouldBe` "DT"

        it "handles three-part modules" $ do
          generateAliasFromStrategy Initials "Data.Map.Strict" `shouldBe` "DMS"

        it "handles four-part modules" $ do
          generateAliasFromStrategy Initials "Control.Monad.Trans.State" `shouldBe` "CMTS"

      describe "FirstNChars strategy" $ do
        it "uses first N characters of last component" $ do
          generateAliasFromStrategy (FirstNChars 3) "Data.Text" `shouldBe` "Tex"

        it "handles short components" $ do
          generateAliasFromStrategy (FirstNChars 10) "Data.IO" `shouldBe` "IO"

        it "handles Strict variant" $ do
          generateAliasFromStrategy (FirstNChars 3) "Data.Map.Strict" `shouldBe` "Map"

    describe "suggestAliasWithConfig" $ do
      it "uses custom aliases from config" $ do
        let cfg = defaultQualifyImportConfig
              { qicCustomAliases = [("Data.Text", "T"), ("Data.ByteString", "BS")]
              }
        suggestAliasWithConfig cfg "Data.Text" `shouldBe` "T"
        suggestAliasWithConfig cfg "Data.ByteString" `shouldBe` "BS"

      it "falls back to strategy for unknown modules" $ do
        let cfg = defaultQualifyImportConfig
              { qicCustomAliases = [("Data.Text", "T")]
              }
        -- Data.Map not in custom aliases, uses LastPart strategy
        suggestAliasWithConfig cfg "Data.Map" `shouldBe` "Map"

      it "respects FirstLetter strategy" $ do
        let cfg = defaultQualifyImportConfig { qicStrategy = FirstLetter }
        suggestAliasWithConfig cfg "Data.Text" `shouldBe` "T"
        suggestAliasWithConfig cfg "Data.ByteString" `shouldBe` "B"

      it "respects Initials strategy" $ do
        let cfg = defaultQualifyImportConfig { qicStrategy = Initials }
        suggestAliasWithConfig cfg "Data.Text" `shouldBe` "DT"
        suggestAliasWithConfig cfg "Data.ByteString" `shouldBe` "DB"

      it "respects FirstNChars strategy" $ do
        let cfg = defaultQualifyImportConfig { qicStrategy = FirstNChars 4 }
        suggestAliasWithConfig cfg "Data.Text" `shouldBe` "Text"
        suggestAliasWithConfig cfg "Data.ByteString" `shouldBe` "Byte"

      it "respects max alias length" $ do
        let cfg = defaultQualifyImportConfig { qicMaxAliasLength = Just 3 }
        suggestAliasWithConfig cfg "Data.ByteString" `shouldBe` "Byt"

      it "uppercases short aliases when preferUppercase is True" $ do
        let cfg = defaultQualifyImportConfig
              { qicStrategy = FirstNChars 2
              , qicPreferUppercase = True
              }
        -- FirstNChars 2 on "Data.Text" gives "Te", uppercase -> "TE"
        suggestAliasWithConfig cfg "Data.Text" `shouldBe` "TE"
        -- FirstNChars 2 on "Data.Map" gives "Ma", uppercase -> "MA"
        suggestAliasWithConfig cfg "Data.Map" `shouldBe` "MA"

      it "does not uppercase when preferUppercase is False" $ do
        let cfg = defaultQualifyImportConfig
              { qicStrategy = FirstNChars 2
              , qicPreferUppercase = False
              }
        suggestAliasWithConfig cfg "Data.Text" `shouldBe` "Te"

      it "custom aliases take priority over strategy" $ do
        let cfg = defaultQualifyImportConfig
              { qicStrategy = Initials
              , qicCustomAliases = [("Data.Text", "Text")]
              }
        -- Custom alias should override Initials strategy
        suggestAliasWithConfig cfg "Data.Text" `shouldBe` "Text"

    describe "ensureUniqueAlias" $ do
      it "returns original alias if not in use" $ do
        let existing = Set.fromList ["M", "S"]
        ensureUniqueAlias "T" existing `shouldBe` "T"

      it "appends number if alias is in use" $ do
        let existing = Set.fromList ["T", "M", "S"]
        ensureUniqueAlias "T" existing `shouldBe` "T2"

      it "increments number until unique" $ do
        let existing = Set.fromList ["T", "T2", "T3"]
        ensureUniqueAlias "T" existing `shouldBe` "T4"

      it "handles empty existing set" $ do
        ensureUniqueAlias "Custom" Set.empty `shouldBe` "Custom"

    describe "QualifyResult" $ do
      it "can be constructed with success state" $ do
        let result = QualifyResult
              { qrSuccess = True
              , qrFix = Nothing
              , qrAffectedSpans = []
              , qrNewAlias = "T"
              , qrSymbolCount = 0
              , qrErrors = []
              , qrWarnings = []
              , qrPreview = Nothing
              }
        qrSuccess result `shouldBe` True

      it "can be constructed with failure state" $ do
        let result = QualifyResult
              { qrSuccess = False
              , qrFix = Nothing
              , qrAffectedSpans = []
              , qrNewAlias = ""
              , qrSymbolCount = 0
              , qrErrors = ["Test error"]
              , qrWarnings = []
              , qrPreview = Nothing
              }
        qrSuccess result `shouldBe` False
        length (qrErrors result) `shouldBe` 1

    describe "QualificationAnalysis" $ do
      it "can be constructed with valid analysis" $ do
        let analysis = QualificationAnalysis
              { qaImportModule = "Data.Text"
              , qaImportSpan = noSpan
              , qaExistingAlias = Nothing
              , qaExistingQualified = False
              , qaSuggestedAlias = "T"
              , qaExplicitSymbols = Just ["pack", "unpack"]
              , qaHiding = False
              , qaSymbolsToQualify = []
              , qaConflicts = []
              , qaIsValid = True
              }
        qaIsValid analysis `shouldBe` True
        qaImportModule analysis `shouldBe` "Data.Text"

    describe "SymbolUsage" $ do
      it "can represent a value usage" $ do
        let usage = SymbolUsage
              { suName = "pack"
              , suSpan = noSpan
              , suIsOperator = False
              , suContext = ValueContext
              }
        suContext usage `shouldBe` ValueContext

      it "can represent an operator usage" $ do
        let usage = SymbolUsage
              { suName = "<>"
              , suSpan = noSpan
              , suIsOperator = True
              , suContext = ValueContext
              }
        suIsOperator usage `shouldBe` True

      it "can represent a type usage" $ do
        let usage = SymbolUsage
              { suName = "Text"
              , suSpan = noSpan
              , suIsOperator = False
              , suContext = TypeContext
              }
        suContext usage `shouldBe` TypeContext

    describe "AliasConflict" $ do
      it "can represent alias already used" $ do
        let conflict = AliasAlreadyUsed "T" "Data.Text.Lazy"
        case conflict of
          AliasAlreadyUsed alias modName -> do
            alias `shouldBe` "T"
            modName `shouldBe` "Data.Text.Lazy"
          _ -> expectationFailure "Expected AliasAlreadyUsed"

      it "can represent symbol shadowed" $ do
        let conflict = SymbolShadowed "pack" noSpan
        case conflict of
          SymbolShadowed name _ -> name `shouldBe` "pack"
          _ -> expectationFailure "Expected SymbolShadowed"

      it "can represent ambiguous symbol" $ do
        let conflict = AmbiguousSymbol "map" ["Data.List", "Data.Map"]
        case conflict of
          AmbiguousSymbol name mods -> do
            name `shouldBe` "map"
            length mods `shouldBe` 2
          _ -> expectationFailure "Expected AmbiguousSymbol"

    describe "renderQualifiedImport" $ do
      it "renders basic qualified import" $ do
        let analysis = QualificationAnalysis
              { qaImportModule = "Data.Text"
              , qaImportSpan = noSpan
              , qaExistingAlias = Nothing
              , qaExistingQualified = False
              , qaSuggestedAlias = "T"
              , qaExplicitSymbols = Nothing
              , qaHiding = False
              , qaSymbolsToQualify = []
              , qaConflicts = []
              , qaIsValid = True
              }
            rendered = renderQualifiedImport analysis
        rendered `shouldBe` "import qualified Data.Text as T"

      it "renders qualified import with explicit list" $ do
        let analysis = QualificationAnalysis
              { qaImportModule = "Data.Text"
              , qaImportSpan = noSpan
              , qaExistingAlias = Nothing
              , qaExistingQualified = False
              , qaSuggestedAlias = "T"
              , qaExplicitSymbols = Just ["pack", "unpack"]
              , qaHiding = False
              , qaSymbolsToQualify = []
              , qaConflicts = []
              , qaIsValid = True
              }
            rendered = renderQualifiedImport analysis
        -- Should contain the explicit list
        T.isInfixOf "(pack, unpack)" rendered `shouldBe` True
        T.isInfixOf "qualified" rendered `shouldBe` True
        T.isInfixOf "as T" rendered `shouldBe` True

    describe "generateQualifyFix" $ do
      it "generates fix with import edit" $ do
        let analysis = QualificationAnalysis
              { qaImportModule = "Data.Text"
              , qaImportSpan = noSpan
              , qaExistingAlias = Nothing
              , qaExistingQualified = False
              , qaSuggestedAlias = "T"
              , qaExplicitSymbols = Nothing
              , qaHiding = False
              , qaSymbolsToQualify = []
              , qaConflicts = []
              , qaIsValid = True
              }
            fix = generateQualifyFix analysis
        length (fixEdits fix) `shouldBe` 1
        T.isInfixOf "qualified" (fixEditNewText (head (fixEdits fix))) `shouldBe` True

      it "generates fix with import and symbol edits" $ do
        let usage = SymbolUsage
              { suName = "pack"
              , suSpan = noSpan
              , suIsOperator = False
              , suContext = ValueContext
              }
            analysis = QualificationAnalysis
              { qaImportModule = "Data.Text"
              , qaImportSpan = noSpan
              , qaExistingAlias = Nothing
              , qaExistingQualified = False
              , qaSuggestedAlias = "T"
              , qaExplicitSymbols = Just ["pack"]
              , qaHiding = False
              , qaSymbolsToQualify = [usage]
              , qaConflicts = []
              , qaIsValid = True
              }
            fix = generateQualifyFix analysis
        -- Should have 2 edits: import + symbol
        length (fixEdits fix) `shouldBe` 2
        -- One edit should be T.pack
        any (\e -> fixEditNewText e == "T.pack") (fixEdits fix) `shouldBe` True

    describe "UsageContext" $ do
      it "distinguishes value context" $ do
        ValueContext `shouldBe` ValueContext
        ValueContext `shouldNotBe` TypeContext

      it "distinguishes type context" $ do
        TypeContext `shouldBe` TypeContext
        TypeContext `shouldNotBe` PatternContext

      it "distinguishes pattern context" $ do
        PatternContext `shouldBe` PatternContext
        PatternContext `shouldNotBe` ValueContext

    describe "SymbolOccurrence" $ do
      it "can represent unqualified occurrence" $ do
        let occ = SymbolOccurrence
              { soName = "pack"
              , soSpan = noSpan
              , soContext = ValueContext
              , soQualified = Nothing
              }
        soQualified occ `shouldBe` Nothing

      it "can represent qualified occurrence" $ do
        let occ = SymbolOccurrence
              { soName = "pack"
              , soSpan = noSpan
              , soContext = ValueContext
              , soQualified = Just "T"
              }
        soQualified occ `shouldBe` Just "T"

-- Helper for tests
noSpan :: SrcSpan
noSpan = SrcSpan
  { srcSpanFile = ""
  , srcSpanStartLine = 1
  , srcSpanStartCol = 1
  , srcSpanEndLine = 1
  , srcSpanEndCol = 1
  }
