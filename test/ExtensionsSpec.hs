{-# LANGUAGE OverloadedStrings #-}

module ExtensionsSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Data.Text qualified as T
import Data.List (find)
import Data.Map.Strict qualified as Map

import Argus.Types
import Argus.Rules.Extensions

spec :: Spec
spec = describe "Argus.Rules.Extensions" $ do
  describe "parseExtensions" $ do
    it "extracts extensions from LANGUAGE pragma" $ do
      let content = "{-# LANGUAGE OverloadedStrings #-}\nmodule Test where"
      parseExtensions content `shouldBe` ["OverloadedStrings"]

    it "extracts multiple extensions from one pragma" $ do
      let content = "{-# LANGUAGE GADTs, TypeFamilies #-}\nmodule Test where"
      parseExtensions content `shouldContain` ["GADTs"]
      parseExtensions content `shouldContain` ["TypeFamilies"]

    it "extracts extensions from multiple pragmas" $ do
      let content = "{-# LANGUAGE GADTs #-}\n{-# LANGUAGE TypeFamilies #-}\nmodule Test where"
      parseExtensions content `shouldContain` ["GADTs"]
      parseExtensions content `shouldContain` ["TypeFamilies"]

    it "returns empty list when no extensions" $ do
      let content = "module Test where"
      parseExtensions content `shouldBe` []

  describe "detectExtensionIssues" $ do
    it "detects missing LambdaCase extension" $ do
      let content = "module Test where\nfoo = \\case _ -> True"
          diags = detectExtensionIssues defaultExtensionConfig "test.hs" content
      findDiagByCode "extension/missing" diags `shouldSatisfy` \case
        Nothing -> False
        Just d -> "LambdaCase" `T.isInfixOf` diagMessage d

    it "detects missing RecordWildCards extension" $ do
      let content = "module Test where\nfoo Bar{..} = x"
          diags = detectExtensionIssues defaultExtensionConfig "test.hs" content
      findDiagByCode "extension/missing" diags `shouldSatisfy` \case
        Nothing -> False
        Just d -> "RecordWildCards" `T.isInfixOf` diagMessage d

    it "detects missing DerivingStrategies extension" $ do
      let content = "module Test where\ndata Foo = Foo deriving stock Show"
          diags = detectExtensionIssues defaultExtensionConfig "test.hs" content
      findDiagByCode "extension/missing" diags `shouldSatisfy` \case
        Nothing -> False
        Just d -> "DerivingStrategies" `T.isInfixOf` diagMessage d

    it "does not flag extensions that are present" $ do
      let content = "{-# LANGUAGE LambdaCase #-}\nmodule Test where\nfoo = \\case _ -> True"
          diags = detectExtensionIssues defaultExtensionConfig "test.hs" content
      findDiagByCode "extension/missing" diags `shouldSatisfy` \case
        Nothing -> True
        Just _ -> False

  describe "extensionDatabase" $ do
    it "contains common extensions" $ do
      Map.member "GADTs" extensionDatabase `shouldBe` True
      Map.member "OverloadedStrings" extensionDatabase `shouldBe` True
      Map.member "TypeFamilies" extensionDatabase `shouldBe` True
      Map.member "LambdaCase" extensionDatabase `shouldBe` True

    it "tracks implications correctly" $ do
      case Map.lookup "GADTs" extensionDatabase of
        Nothing -> expectationFailure "GADTs not in database"
        Just info -> extImplies info `shouldContain` ["GADTSyntax"]

    it "marks unsafe extensions correctly" $ do
      case Map.lookup "UndecidableInstances" extensionDatabase of
        Nothing -> expectationFailure "UndecidableInstances not in database"
        Just info -> extSafe info `shouldBe` False

      case Map.lookup "OverloadedStrings" extensionDatabase of
        Nothing -> expectationFailure "OverloadedStrings not in database"
        Just info -> extSafe info `shouldBe` True

    it "marks deprecated extensions" $ do
      case Map.lookup "OverlappingInstances" extensionDatabase of
        Nothing -> expectationFailure "OverlappingInstances not in database"
        Just info -> extDeprecated info `shouldBe` True

  describe "generatePragmaFix" $ do
    it "generates correct pragma text" $ do
      let fix = generatePragmaFix "test.hs" "LambdaCase"
      fixTitle fix `shouldBe` "Add {-# LANGUAGE LambdaCase #-}"

  describe "suggestExtension" $ do
    it "suggests LambdaCase for \\case pattern" $ do
      suggestExtension "\\case" `shouldBe` Just "LambdaCase"

    it "suggests BinaryLiterals for 0b pattern" $ do
      suggestExtension "x = 0b1010" `shouldBe` Just "BinaryLiterals"

  describe "configuration" $ do
    it "can disable extension checking" $ do
      let content = "module Test where\nfoo = \\case _ -> True"
          config = defaultExtensionConfig { ecEnabled = False }
          diags = detectExtensionIssues config "test.hs" content
      diags `shouldBe` []

-- | Helper to find a diagnostic by code
findDiagByCode :: Text -> [Diagnostic] -> Maybe Diagnostic
findDiagByCode code diags = find hasCode diags
  where
    hasCode d = case diagCode d of
      Just c -> code `T.isInfixOf` c
      Nothing -> False
