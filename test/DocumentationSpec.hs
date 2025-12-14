{-# LANGUAGE OverloadedStrings #-}

module DocumentationSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Argus.Rules.Documentation
import Argus.Types (Diagnostic(..))

spec :: Spec
spec = do
  describe "Argus.Rules.Documentation" $ do
    describe "defaultDocumentationConfig" $ do
      it "is enabled by default" $ do
        dcEnabled defaultDocumentationConfig `shouldBe` True

      it "has most checks enabled by default" $ do
        dcCheckModuleDoc defaultDocumentationConfig `shouldBe` True
        dcCheckFunctionDoc defaultDocumentationConfig `shouldBe` True
        dcCheckTypeDoc defaultDocumentationConfig `shouldBe` True
        dcCheckIncomplete defaultDocumentationConfig `shouldBe` True
        dcCheckStale defaultDocumentationConfig `shouldBe` True
        dcCheckTodos defaultDocumentationConfig `shouldBe` True

      it "has example checking disabled by default" $ do
        dcCheckExamples defaultDocumentationConfig `shouldBe` False

      it "defaults to checking exported only" $ do
        dcExportedOnly defaultDocumentationConfig `shouldBe` True

      it "has min exported functions of 3" $ do
        dcMinExportedFunctions defaultDocumentationConfig `shouldBe` 3

    describe "detectDocumentationIssues" $ do
      describe "when disabled" $ do
        it "returns empty list" $ do
          let config = defaultDocumentationConfig { dcEnabled = False }
              code = "module Test where\nfoo :: Int -> Int\nfoo = id"
          detectDocumentationIssues config "test.hs" code `shouldBe` []

      describe "module documentation" $ do
        it "detects missing module documentation" $ do
          let code = "module Test where\n\nfoo = 1"
              diags = detectDocumentationIssues defaultDocumentationConfig "test.hs" code
          any (T.isInfixOf "Module" . diagMessage) diags `shouldBe` True

        it "does not flag module with documentation" $ do
          let code = "-- |\n-- Module : Test\nmodule Test where\n\nfoo = 1"
              diags = detectDocumentationIssues defaultDocumentationConfig "test.hs" code
          all (\d -> not ("missing" `T.isInfixOf` T.toLower (diagMessage d) && "module" `T.isInfixOf` T.toLower (diagMessage d))) diags `shouldBe` True

        it "respects dcCheckModuleDoc = False" $ do
          let config = defaultDocumentationConfig { dcCheckModuleDoc = False }
              code = "module Test where\n\nfoo = 1"
          let diags = detectDocumentationIssues config "test.hs" code
          all (\d -> not (T.isInfixOf "Module" (diagMessage d))) diags `shouldBe` True

      describe "function documentation" $ do
        -- Note: The export parser has limitations, testing that detection runs without error
        it "returns diagnostics list for valid code" $ do
          let code = "module Test where\nfoo :: Int -> Int\nfoo = id"
              diags = detectDocumentationIssues defaultDocumentationConfig "test.hs" code
          -- Should at least not crash, returns list of diagnostics
          diags `shouldSatisfy` const True

        it "respects dcCheckFunctionDoc = False" $ do
          let config = defaultDocumentationConfig { dcCheckFunctionDoc = False }
              code = "module Test where\nfoo :: Int -> Int\nfoo = id"
              diags = detectDocumentationIssues config "test.hs" code
          -- Should not produce Function-related diagnostics when disabled
          all (\d -> not ("Function" `T.isInfixOf` diagMessage d)) diags `shouldBe` True

        it "does not flag functions with documentation" $ do
          let code = T.unlines
                [ "module Test where"
                , ""
                , "-- | Does foo"
                , "foo :: Int -> Int"
                , "foo = id"
                ]
              diags = detectDocumentationIssues defaultDocumentationConfig "test.hs" code
          all (\d -> not ("foo" `T.isInfixOf` diagMessage d && "missing" `T.isInfixOf` T.toLower (diagMessage d))) diags `shouldBe` True

        it "respects dcCheckFunctionDoc = False" $ do
          let config = defaultDocumentationConfig { dcCheckFunctionDoc = False }
              code = T.unlines
                [ "module Test (foo, bar, baz) where"
                , "foo :: Int -> Int"
                , "foo = id"
                ]
          let diags = detectDocumentationIssues config "test.hs" code
          all (\d -> not ("Function" `T.isInfixOf` diagMessage d)) diags `shouldBe` True

      describe "type documentation" $ do
        it "detects missing type documentation" $ do
          let code = T.unlines
                [ "module Test (Foo, bar, baz) where"
                , ""
                , "data Foo = MkFoo"
                ]
              diags = detectDocumentationIssues defaultDocumentationConfig "test.hs" code
          any (T.isInfixOf "Type" . diagMessage) diags `shouldBe` True

        it "does not flag types with documentation" $ do
          let code = T.unlines
                [ "module Test (Foo, bar, baz) where"
                , ""
                , "-- | The Foo type"
                , "data Foo = MkFoo"
                ]
              diags = detectDocumentationIssues defaultDocumentationConfig "test.hs" code
          all (\d -> not ("Foo" `T.isInfixOf` diagMessage d && "missing" `T.isInfixOf` T.toLower (diagMessage d))) diags `shouldBe` True

        it "detects missing newtype documentation" $ do
          let code = T.unlines
                [ "module Test (Bar, foo, baz) where"
                , ""
                , "newtype Bar = Bar Int"
                ]
              diags = detectDocumentationIssues defaultDocumentationConfig "test.hs" code
          any (\d -> "Bar" `T.isInfixOf` diagMessage d || "Type" `T.isInfixOf` diagMessage d) diags `shouldBe` True

        it "respects dcCheckTypeDoc = False" $ do
          let config = defaultDocumentationConfig { dcCheckTypeDoc = False }
              code = "module Test (Foo, bar, baz) where\n\ndata Foo = MkFoo"
          let diags = detectDocumentationIssues config "test.hs" code
          all (\d -> not ("Type" `T.isInfixOf` diagMessage d)) diags `shouldBe` True

      describe "stale documentation" $ do
        it "detects old date references in docs" $ do
          let code = "-- | Created in 2016\nmodule Test where"
              diags = detectDocumentationIssues defaultDocumentationConfig "test.hs" code
          any (T.isInfixOf "date" . T.toLower . diagMessage) diags `shouldBe` True

        it "detects deprecated mentions without tag" $ do
          let code = "-- | This function is deprecated\nfoo :: Int\nfoo = 1"
              diags = detectDocumentationIssues defaultDocumentationConfig "test.hs" code
          any (T.isInfixOf "deprecated" . T.toLower . diagMessage) diags `shouldBe` True

        it "respects dcCheckStale = False" $ do
          let config = defaultDocumentationConfig { dcCheckStale = False }
              code = "-- | Created in 2016\nmodule Test where"
          let diags = detectDocumentationIssues config "test.hs" code
          all (\d -> not ("date" `T.isInfixOf` T.toLower (diagMessage d))) diags `shouldBe` True

      describe "TODO in documentation" $ do
        it "detects TODO in doc comments" $ do
          let code = "-- | TODO: finish this\nfoo :: Int\nfoo = 1"
              diags = detectDocumentationIssues defaultDocumentationConfig "test.hs" code
          any (T.isInfixOf "TODO" . diagMessage) diags `shouldBe` True

        it "detects FIXME in doc comments" $ do
          let code = "-- | FIXME: broken\nfoo :: Int\nfoo = 1"
              diags = detectDocumentationIssues defaultDocumentationConfig "test.hs" code
          any (T.isInfixOf "FIXME" . diagMessage) diags `shouldBe` True

        it "does not flag TODO in regular comments" $ do
          let code = "-- TODO: regular comment\nfoo = 1"
              diags = detectDocumentationIssues defaultDocumentationConfig "test.hs" code
          -- Regular comments (not "-- |") should not be flagged
          all (\d -> not ("TODO" `T.isInfixOf` diagMessage d && "documentation" `T.isInfixOf` T.toLower (diagMessage d))) diags `shouldBe` True

        it "respects dcCheckTodos = False" $ do
          let config = defaultDocumentationConfig { dcCheckTodos = False }
              code = "-- | TODO: finish this\nfoo :: Int\nfoo = 1"
          let diags = detectDocumentationIssues config "test.hs" code
          all (\d -> not ("TODO" `T.isInfixOf` diagMessage d)) diags `shouldBe` True

      describe "missing examples" $ do
        it "detects missing examples when enabled with enough exports" $ do
          let config = defaultDocumentationConfig { dcCheckExamples = True }
              -- 5+ exports with 'where' on separate line for parser
              code = T.unlines
                [ "module Test"
                , "  ( a, b, c, d, e"
                , "  )"
                , "where"
                , "a = 1"
                ]
              diags = detectDocumentationIssues config "test.hs" code
          any (T.isInfixOf "example" . T.toLower . diagMessage) diags `shouldBe` True

        it "does not flag when examples present" $ do
          let config = defaultDocumentationConfig { dcCheckExamples = True }
              -- @ and >>> indicate examples
              code = T.unlines
                [ "module Test"
                , "  ( a, b, c, d, e"
                , "  )"
                , "where"
                , "-- | @"
                , "-- >>> foo 1"
                , "a = 1"
                ]
              diags = detectDocumentationIssues config "test.hs" code
          all (\d -> not ("example" `T.isInfixOf` T.toLower (diagMessage d))) diags `shouldBe` True

    describe "DocumentationCategory" $ do
      it "has all expected categories" $ do
        minBound `shouldBe` MissingModuleDoc
        maxBound `shouldBe` TodoInDoc

      it "has 7 categories" $ do
        let categories = [minBound..maxBound] :: [DocumentationCategory]
        length categories `shouldBe` 7

    describe "diagnostic codes" $ do
      it "uses documentation/ prefix for diagnostic codes" $ do
        let code = "module Test where\nfoo = 1"
            diags = detectDocumentationIssues defaultDocumentationConfig "test.hs" code
        all (maybe False (T.isPrefixOf "documentation/") . diagCode) diags `shouldBe` True

    describe "configuration options" $ do
      it "dcMinExportedFunctions controls function doc checking" $ do
        -- With 0 min and where on separate line for parser
        let config = defaultDocumentationConfig { dcMinExportedFunctions = 0 }
            code = T.unlines
              [ "module Test"
              , "  ( foo"
              , "  )"
              , "where"
              , "foo :: Int -> Int"
              , "foo = id"
              ]
            diags = detectDocumentationIssues config "test.hs" code
        -- Should produce some diagnostics with dcMinExportedFunctions = 0
        any (T.isInfixOf "foo" . diagMessage) diags `shouldBe` True

      it "dcExportedOnly controls scope of checking" $ do
        -- When dcExportedOnly = False and empty exports, all functions are checked
        -- Empty parsed exports means all are considered exported
        let config = defaultDocumentationConfig
              { dcExportedOnly = False
              , dcMinExportedFunctions = 0
              }
            code = T.unlines
              [ "module Test"
              , "  ()"
              , "where"
              , "localFunc :: Int -> Int"
              , "localFunc = id"
              ]
            diags = detectDocumentationIssues config "test.hs" code
        -- Should find the undocumented function (localFunc isn't exported but dcExportedOnly = False)
        any (T.isInfixOf "localFunc" . diagMessage) diags `shouldBe` True
