{-# LANGUAGE OverloadedStrings #-}

module ModernizeSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Data.Text qualified as T
import Data.List (find)

import Argus.Types
import Argus.Rules.Modernize

spec :: Spec
spec = describe "Argus.Rules.Modernize" $ do
  describe "detectModernize" $ do
    it "detects return in non-do context and suggests pure" $ do
      let content = "foo = return 42"
          diags = detectModernize defaultModernizeConfig "test.hs" content
      findDiagByCode "modernize/applicative" diags `shouldSatisfy` \case
        Nothing -> False
        Just d -> "pure" `T.isInfixOf` diagMessage d

    it "detects liftM and suggests fmap" $ do
      let content = "foo xs = liftM (+1) xs"
          diags = detectModernize defaultModernizeConfig "test.hs" content
      findDiagByCode "modernize/applicative" diags `shouldSatisfy` \case
        Nothing -> False
        Just d -> "fmap" `T.isInfixOf` diagMessage d

    it "detects liftM2 and suggests liftA2" $ do
      let content = "foo a b = liftM2 (+) a b"
          diags = detectModernize defaultModernizeConfig "test.hs" content
      findDiagByCode "modernize/applicative" diags `shouldSatisfy` \case
        Nothing -> False
        Just d -> "liftA2" `T.isInfixOf` diagMessage d

    it "detects mappend and suggests (<>)" $ do
      let content = "foo a b = mappend a b"
          diags = detectModernize defaultModernizeConfig "test.hs" content
      findDiagByCode "modernize/semigroup" diags `shouldSatisfy` \case
        Nothing -> False
        Just d -> "(<>)" `T.isInfixOf` diagMessage d

    it "detects mempty `mappend` x pattern" $ do
      let content = "foo x = mempty `mappend` x"
          diags = detectModernize defaultModernizeConfig "test.hs" content
      findDiagByCode "modernize/semigroup" diags `shouldSatisfy` \case
        Nothing -> False
        Just d -> "(<>)" `T.isInfixOf` diagMessage d || "Semigroup" `T.isInfixOf` diagMessage d

    it "detects mapM and suggests traverse" $ do
      let content = "foo f xs = mapM f xs"
          diags = detectModernize defaultModernizeConfig "test.hs" content
      findDiagByCode "modernize/traversable" diags `shouldSatisfy` \case
        Nothing -> False
        Just d -> "traverse" `T.isInfixOf` diagMessage d

    it "detects mapM_ and suggests traverse_" $ do
      let content = "foo f xs = mapM_ f xs"
          diags = detectModernize defaultModernizeConfig "test.hs" content
      findDiagByCode "modernize/traversable" diags `shouldSatisfy` \case
        Nothing -> False
        Just d -> "traverse_" `T.isInfixOf` diagMessage d

    it "detects forM and suggests for" $ do
      let content = "foo xs f = forM xs f"
          diags = detectModernize defaultModernizeConfig "test.hs" content
      findDiagByCode "modernize/traversable" diags `shouldSatisfy` \case
        Nothing -> False
        Just d -> "for" `T.isInfixOf` diagMessage d

    it "detects forM_ and suggests for_" $ do
      let content = "foo xs f = forM_ xs f"
          diags = detectModernize defaultModernizeConfig "test.hs" content
      findDiagByCode "modernize/traversable" diags `shouldSatisfy` \case
        Nothing -> False
        Just d -> "for_" `T.isInfixOf` diagMessage d

    it "detects foldMap id and suggests fold" $ do
      let content = "foo xs = foldMap id xs"
          diags = detectModernize defaultModernizeConfig "test.hs" content
      findDiagByCode "modernize/monoid" diags `shouldSatisfy` \case
        Nothing -> False
        Just d -> "fold" `T.isInfixOf` diagMessage d

    it "detects foldr (<>) mempty and suggests fold" $ do
      let content = "foo xs = foldr (<>) mempty xs"
          diags = detectModernize defaultModernizeConfig "test.hs" content
      findDiagByCode "modernize/monoid" diags `shouldSatisfy` \case
        Nothing -> False
        Just d -> "fold" `T.isInfixOf` diagMessage d

  describe "auto-fix generation" $ do
    it "generates fix for liftM -> fmap" $ do
      let content = "foo xs = liftM (+1) xs"
          diags = detectModernize defaultModernizeConfig "test.hs" content
      case findDiagByCode "modernize/applicative" diags of
        Nothing -> expectationFailure "Expected diagnostic"
        Just d -> diagFixes d `shouldSatisfy` \fixes ->
          not (null fixes) && any (("fmap" `T.isInfixOf`) . fixTitle) fixes

    it "generates fix for mappend -> (<>)" $ do
      let content = "foo a b = mappend a b"
          diags = detectModernize defaultModernizeConfig "test.hs" content
      case findDiagByCode "modernize/semigroup" diags of
        Nothing -> expectationFailure "Expected diagnostic"
        Just d -> diagFixes d `shouldSatisfy` \fixes ->
          not (null fixes) && any (("(<>)" `T.isInfixOf`) . fixTitle) fixes

    it "generates fix for mapM -> traverse" $ do
      let content = "foo f xs = mapM f xs"
          diags = detectModernize defaultModernizeConfig "test.hs" content
      case findDiagByCode "modernize/traversable" diags of
        Nothing -> expectationFailure "Expected diagnostic"
        Just d -> diagFixes d `shouldSatisfy` \fixes ->
          not (null fixes) && any (("traverse" `T.isInfixOf`) . fixTitle) fixes

  describe "configuration" $ do
    it "can disable all checks" $ do
      let content = "foo = liftM id xs"
          config = defaultModernizeConfig { modEnabled = False }
          diags = detectModernize config "test.hs" content
      diags `shouldBe` []

    it "can disable specific checks" $ do
      let content = "foo xs = liftM (+1) xs"
          config = defaultModernizeConfig { modCheckApplicative = False }
          diags = detectModernize config "test.hs" content
      findDiagByCode "modernize/applicative" diags `shouldBe` Nothing

-- | Helper to find a diagnostic by code
findDiagByCode :: Text -> [Diagnostic] -> Maybe Diagnostic
findDiagByCode code diags = find hasCode diags
  where
    hasCode d = case diagCode d of
      Just c -> code `T.isInfixOf` c
      Nothing -> False
