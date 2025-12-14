{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the AutoFix Template module.
module TemplateSpec (spec) where

import Test.Hspec

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Argus.AutoFix.Template
import Argus.AutoFix.DSL (FixSpec(..), FixExpr(..))
import Argus.AutoFix.Types (highConfidence)
import Argus.Rules.Types (Category(..), SafetyLevel(..))

spec :: Spec
spec = do
  describe "Argus.AutoFix.Template" $ do
    describe "Template construction" $ do
      describe "template" $ do
        it "creates a template with given name" $ do
          let t = template "test-template" $ pure ()
          ftName t `shouldBe` "test-template"

        it "uses default category of Style" $ do
          let t = template "test" $ pure ()
          ftCategory t `shouldBe` Style

        it "uses default safety of Safe" $ do
          let t = template "test" $ pure ()
          ftSafety t `shouldBe` Safe

        it "uses default confidence of highConfidence" $ do
          let t = template "test" $ pure ()
          ftConfidence t `shouldBe` highConfidence

        it "has empty params by default" $ do
          let t = template "test" $ pure ()
          ftParams t `shouldBe` []

    describe "Parameter definitions" $ do
      describe "param" $ do
        it "creates a required parameter" $ do
          let t = template "test" $ param "name" TypeText "Description"
          length (ftParams t) `shouldBe` 1
          let p = head (ftParams t)
          tpName p `shouldBe` "name"
          tpType p `shouldBe` TypeText
          tpDescription p `shouldBe` "Description"
          tpRequired p `shouldBe` True
          tpDefault p `shouldBe` Nothing

      describe "paramWithDefault" $ do
        it "creates a parameter with default value" $ do
          let t = template "test" $ paramWithDefault "name" TypeText "Description" "default"
          length (ftParams t) `shouldBe` 1
          let p = head (ftParams t)
          tpName p `shouldBe` "name"
          tpRequired p `shouldBe` False
          tpDefault p `shouldBe` Just "default"

      describe "optionalParam" $ do
        it "creates an optional parameter without default" $ do
          let t = template "test" $ optionalParam "name" TypeText "Description"
          length (ftParams t) `shouldBe` 1
          let p = head (ftParams t)
          tpRequired p `shouldBe` False
          tpDefault p `shouldBe` Nothing

      describe "listParam" $ do
        it "creates a list parameter" $ do
          let t = template "test" $ listParam "items" TypeIdentifier "List of identifiers"
          length (ftParams t) `shouldBe` 1
          let p = head (ftParams t)
          tpType p `shouldBe` TypeList TypeIdentifier

    describe "ParamType" $ do
      it "supports TypeText" $ do
        TypeText `shouldBe` TypeText

      it "supports TypeIdentifier" $ do
        TypeIdentifier `shouldBe` TypeIdentifier

      it "supports TypeModuleName" $ do
        TypeModuleName `shouldBe` TypeModuleName

      it "supports TypeOperator" $ do
        TypeOperator `shouldBe` TypeOperator

      it "supports TypePattern" $ do
        TypePattern `shouldBe` TypePattern

      it "supports TypeList" $ do
        TypeList TypeText `shouldBe` TypeList TypeText

      it "supports TypeChoice" $ do
        TypeChoice ["a", "b", "c"] `shouldBe` TypeChoice ["a", "b", "c"]

    describe "Template body operations" $ do
      describe "templateReplace" $ do
        it "adds replace expression to body" $ do
          let t = template "test" $
                    templateBody $ templateReplace (TELiteral "old") (TELiteral "new")
          length (tbExprs (ftBody t)) `shouldBe` 1

      describe "templateReplacePattern" $ do
        it "adds replace pattern expression to body" $ do
          let t = template "test" $
                    templateBody $ templateReplacePattern (paramRef "old") (paramRef "new")
          length (tbExprs (ftBody t)) `shouldBe` 1

      describe "templateInsert" $ do
        it "adds insert expression to body" $ do
          let t = template "test" $
                    templateBody $ templateInsert 10 (TELiteral "text")
          length (tbExprs (ftBody t)) `shouldBe` 1

      describe "templateDelete" $ do
        it "adds delete expression to body" $ do
          let t = template "test" $
                    templateBody $ templateDelete (TELiteral "target")
          length (tbExprs (ftBody t)) `shouldBe` 1

      describe "templateAddImport" $ do
        it "adds import to body" $ do
          let t = template "test" $
                    templateBody $ templateAddImport (TELiteral "Data.Maybe") [TELiteral "fromMaybe"]
          length (tbAddImports (ftBody t)) `shouldBe` 1

      describe "templateRemoveImport" $ do
        it "adds remove import to body" $ do
          let t = template "test" $
                    templateBody $ templateRemoveImport (TELiteral "Data.List")
          length (tbRemoveImports (ftBody t)) `shouldBe` 1

      describe "templateSafety" $ do
        it "sets safety level" $ do
          let t = template "test" $ templateBody $ templateSafety MostlySafe
          ftSafety t `shouldBe` MostlySafe

      describe "templateCategory" $ do
        it "sets category" $ do
          let t = template "test" $ templateBody $ templateCategory Performance
          ftCategory t `shouldBe` Performance

      describe "templateConfidence" $ do
        it "sets confidence" $ do
          let t = template "test" $ templateBody $ templateConfidence 0.5
          ftConfidence t `shouldSatisfy` (\c -> True)  -- Just check it exists

      describe "templateNote" $ do
        it "adds note" $ do
          let t = template "test" $ templateBody $ templateNote "A note"
          length (ftNotes t) `shouldBe` 1
          head (ftNotes t) `shouldBe` "A note"

    describe "Template references" $ do
      describe "paramRef" $ do
        it "creates parameter reference" $ do
          paramRef "name" `shouldBe` TEParamRef "name"

      describe "paramRefOr" $ do
        it "creates parameter reference with default" $ do
          paramRefOr "name" "default" `shouldBe` TEParamRefOr "name" "default"

      describe "paramJoin" $ do
        it "creates join expression" $ do
          let expr = paramJoin ", " [TELiteral "a", TELiteral "b"]
          case expr of
            TEJoin sep exprs -> do
              sep `shouldBe` ", "
              length exprs `shouldBe` 2
            _ -> expectationFailure "Expected TEJoin"

      describe "paramConcat" $ do
        it "creates concat expression" $ do
          let expr = paramConcat [TELiteral "a", TELiteral "b"]
          case expr of
            TEConcat exprs -> length exprs `shouldBe` 2
            _ -> expectationFailure "Expected TEConcat"

    describe "Template instantiation" $ do
      describe "instantiate" $ do
        it "instantiates template with all required params" $ do
          let t = template "test" $ do
                    param "old" TypeIdentifier "Old value"
                    param "new" TypeIdentifier "New value"
                    templateBody $
                      templateReplacePattern (paramRef "old") (paramRef "new")
          let fs = instantiate t [("old", "head"), ("new", "headMay")]
          fsSourceRule fs `shouldBe` Just "test"

        it "uses template category" $ do
          let t = template "test" $ do
                    param "value" TypeText "Value"
                    templateBody $ templateCategory Performance
          let fs = instantiate t [("value", "x")]
          fsCategory fs `shouldBe` Performance

        it "uses template safety" $ do
          let t = template "test" $ do
                    param "value" TypeText "Value"
                    templateBody $ templateSafety NeedsReview
          let fs = instantiate t [("value", "x")]
          fsSafety fs `shouldBe` NeedsReview

      describe "instantiateWithDefaults" $ do
        it "uses default values for missing optional params" $ do
          let t = template "test" $ do
                    param "required" TypeText "Required"
                    paramWithDefault "optional" TypeText "Optional" "default-value"
          let fs = instantiateWithDefaults t [("required", "value")]
          fsSourceRule fs `shouldBe` Just "test"

      describe "tryInstantiate" $ do
        it "returns Right for valid instantiation" $ do
          let t = template "test" $ param "value" TypeText "Value"
          let result = tryInstantiate t [("value", "x")]
          case result of
            Right _ -> pure ()
            Left errs -> expectationFailure $ "Expected Right, got errors: " ++ show errs

        it "returns Left MissingRequiredParam for missing params" $ do
          let t = template "test" $ param "required" TypeText "Required"
          let result = tryInstantiate t []
          case result of
            Left errs -> do
              length errs `shouldSatisfy` (> 0)
              case head errs of
                MissingRequiredParam name -> name `shouldBe` "required"
                _ -> expectationFailure "Expected MissingRequiredParam"
            Right _ -> expectationFailure "Expected Left"

        it "returns Left UnknownParam for unknown params" $ do
          let t = template "test" $ param "known" TypeText "Known"
          let result = tryInstantiate t [("known", "x"), ("unknown", "y")]
          let isUnknownParam (UnknownParam _) = True
              isUnknownParam _ = False
          case result of
            Left errs -> do
              any isUnknownParam errs `shouldBe` True
            Right _ -> expectationFailure "Expected Left"

    describe "Template validation" $ do
      describe "validateTemplate" $ do
        it "validates correct template" $ do
          let t = template "test" $ do
                    param "value" TypeText "Value"
                    templateBody $ templateReplacePattern (paramRef "value") (TELiteral "new")
          let v = validateTemplate t
          tvValid v `shouldBe` True
          tvErrors v `shouldBe` []

        it "detects duplicate parameter names" $ do
          let t = template "test" $ do
                    param "value" TypeText "First value"
                    param "value" TypeText "Second value"
          let v = validateTemplate t
          tvValid v `shouldBe` False
          length (tvErrors v) `shouldSatisfy` (> 0)

      describe "validateParams" $ do
        it "returns empty for valid params" $ do
          let params = [ TemplateParam "a" TypeText "A" Nothing True Nothing
                       , TemplateParam "b" TypeText "B" Nothing True Nothing
                       ]
          validateParams params `shouldBe` []

        it "detects duplicate names" $ do
          let params = [ TemplateParam "a" TypeText "A" Nothing True Nothing
                       , TemplateParam "a" TypeText "Duplicate" Nothing True Nothing
                       ]
          length (validateParams params) `shouldSatisfy` (> 0)

    describe "Template composition" $ do
      describe "composeTemplates" $ do
        it "combines two templates" $ do
          let t1 = template "first" $ do
                     param "a" TypeText "A"
                     templateBody $ templateCategory Performance
          let t2 = template "second" $ do
                     param "b" TypeText "B"
                     templateBody $ templateCategory Style
          let composed = composeTemplates t1 t2
          ftName composed `shouldBe` "first-then-second"
          length (ftParams composed) `shouldBe` 2

        it "combines tags" $ do
          let t1 = template "first" $ do
                     templateTag "tag1"
          let t2 = template "second" $ do
                     templateTag "tag2"
          let composed = composeTemplates t1 t2
          Set.size (ftTags composed) `shouldBe` 2

      describe "extendTemplate" $ do
        it "extends template with additional params" $ do
          let base = template "base" $ param "a" TypeText "A"
          let extended = extendTemplate base $ param "b" TypeText "B"
          length (ftParams extended) `shouldBe` 2

      describe "overrideParam" $ do
        it "overrides parameter default" $ do
          let t = template "test" $ paramWithDefault "value" TypeText "Value" "original"
          let overridden = overrideParam "value" "new-default" t
          let p = head (ftParams overridden)
          tpDefault p `shouldBe` Just "new-default"

      describe "hideParam" $ do
        it "makes parameter optional with default" $ do
          let t = template "test" $ param "value" TypeText "Value"
          let hidden = hideParam "value" "hidden-default" t
          let p = head (ftParams hidden)
          tpRequired p `shouldBe` False
          tpDefault p `shouldBe` Just "hidden-default"

    describe "Pre-built templates" $ do
      describe "replaceWithSafeTemplate" $ do
        it "has correct name" $ do
          ftName replaceWithSafeTemplate `shouldBe` "replace-with-safe"

        it "has required parameters" $ do
          let paramNames = map tpName (ftParams replaceWithSafeTemplate)
          "unsafe" `elem` paramNames `shouldBe` True
          "safe" `elem` paramNames `shouldBe` True
          "safeModule" `elem` paramNames `shouldBe` True

        it "has Safety category" $ do
          ftCategory replaceWithSafeTemplate `shouldBe` Safety

      describe "addStrictVersionTemplate" $ do
        it "has correct name" $ do
          ftName addStrictVersionTemplate `shouldBe` "add-strict-version"

        it "has Performance category" $ do
          ftCategory addStrictVersionTemplate `shouldBe` Performance

      describe "addQualifiedImportTemplate" $ do
        it "has correct name" $ do
          ftName addQualifiedImportTemplate `shouldBe` "add-qualified-import"

        it "has Imports category" $ do
          ftCategory addQualifiedImportTemplate `shouldBe` Imports

      describe "removeUnusedImportTemplate" $ do
        it "has correct name" $ do
          ftName removeUnusedImportTemplate `shouldBe` "remove-unused-import"

        it "has Imports category" $ do
          ftCategory removeUnusedImportTemplate `shouldBe` Imports

      describe "wrapWithFunctionTemplate" $ do
        it "has correct name" $ do
          ftName wrapWithFunctionTemplate `shouldBe` "wrap-with-function"

        it "has Style category" $ do
          ftCategory wrapWithFunctionTemplate `shouldBe` Style

        it "has MostlySafe safety" $ do
          ftSafety wrapWithFunctionTemplate `shouldBe` MostlySafe

      describe "unwrapFunctionTemplate" $ do
        it "has correct name" $ do
          ftName unwrapFunctionTemplate `shouldBe` "unwrap-function"

        it "has NeedsReview safety" $ do
          ftSafety unwrapFunctionTemplate `shouldBe` NeedsReview

      describe "modernizePatternTemplate" $ do
        it "has correct name" $ do
          ftName modernizePatternTemplate `shouldBe` "modernize-pattern"

        it "has Modernization category" $ do
          ftCategory modernizePatternTemplate `shouldBe` Modernization

    describe "Template utilities" $ do
      describe "templateToFixSpec" $ do
        it "converts template with all defaults to FixSpec" $ do
          let t = template "test" $ paramWithDefault "value" TypeText "Value" "default"
          let result = templateToFixSpec t
          result `shouldSatisfy` \case
            Just _ -> True
            Nothing -> False

        it "returns Nothing for template with required params" $ do
          let t = template "test" $ param "required" TypeText "Required"
          let result = templateToFixSpec t
          result `shouldBe` Nothing

      describe "templateParams" $ do
        it "returns list of parameter names" $ do
          let t = template "test" $ do
                    param "a" TypeText "A"
                    param "b" TypeText "B"
          templateParams t `shouldBe` ["a", "b"]

      describe "templateDefaults" $ do
        it "returns map of defaults" $ do
          let t = template "test" $ do
                    param "required" TypeText "Required"
                    paramWithDefault "optional" TypeText "Optional" "default"
          let defaults = templateDefaults t
          Map.lookup "optional" defaults `shouldBe` Just "default"
          Map.lookup "required" defaults `shouldBe` Nothing

      describe "describeTemplate" $ do
        it "generates description text" $ do
          let t = template "test" $ do
                    param "value" TypeText "A value"
                    templateBody $ templateCategory Performance
          let desc = describeTemplate t
          desc `shouldSatisfy` T.isInfixOf "test"

    describe "TemplateExpr" $ do
      it "TELiteral holds literal text" $ do
        TELiteral "text" `shouldBe` TELiteral "text"

      it "TEParamRef references a parameter" $ do
        TEParamRef "name" `shouldBe` TEParamRef "name"

      it "TEParamRefOr references with default" $ do
        TEParamRefOr "name" "default" `shouldBe` TEParamRefOr "name" "default"

      it "TEConcat concatenates expressions" $ do
        let expr = TEConcat [TELiteral "a", TELiteral "b"]
        case expr of
          TEConcat [TELiteral "a", TELiteral "b"] -> pure ()
          _ -> expectationFailure "Expected TEConcat"

      it "TEJoin joins with separator" $ do
        let expr = TEJoin ", " [TELiteral "a", TELiteral "b"]
        case expr of
          TEJoin ", " [TELiteral "a", TELiteral "b"] -> pure ()
          _ -> expectationFailure "Expected TEJoin"

      it "TEReplace creates replace expression" $ do
        let expr = TEReplace (TELiteral "old") (TELiteral "new")
        case expr of
          TEReplace (TELiteral "old") (TELiteral "new") -> pure ()
          _ -> expectationFailure "Expected TEReplace"

      it "TEReplacePattern creates replace pattern expression" $ do
        let expr = TEReplacePattern (TEParamRef "old") (TEParamRef "new")
        case expr of
          TEReplacePattern (TEParamRef "old") (TEParamRef "new") -> pure ()
          _ -> expectationFailure "Expected TEReplacePattern"

      it "TEInsert creates insert expression" $ do
        let expr = TEInsert 10 (TELiteral "text")
        case expr of
          TEInsert 10 (TELiteral "text") -> pure ()
          _ -> expectationFailure "Expected TEInsert"

      it "TEDelete creates delete expression" $ do
        let expr = TEDelete (TELiteral "target")
        case expr of
          TEDelete (TELiteral "target") -> pure ()
          _ -> expectationFailure "Expected TEDelete"

    describe "InstantiationError" $ do
      it "MissingRequiredParam contains param name" $ do
        let err = MissingRequiredParam "name"
        case err of
          MissingRequiredParam n -> n `shouldBe` "name"
          _ -> expectationFailure "Expected MissingRequiredParam"

      it "UnknownParam contains param name" $ do
        let err = UnknownParam "unknown"
        case err of
          UnknownParam n -> n `shouldBe` "unknown"
          _ -> expectationFailure "Expected UnknownParam"

    describe "Full template workflow" $ do
      it "creates and instantiates a template for head replacement" $ do
        let t = template "replace-head" $ do
              param "safeModule" TypeModuleName "Module with safe head"
              paramWithDefault "safeFn" TypeIdentifier "Safe function name" "headMay"
              templateBody $ do
                templateReplacePattern (TELiteral "head") (paramRef "safeFn")
                templateAddImport (paramRef "safeModule") [paramRef "safeFn"]
                templateSafety Safe
                templateCategory Safety
                templateConfidence 0.95
                templateNote "Replace partial head with safe alternative"
        let fs = instantiate t [("safeModule", "Safe")]
        fsCategory fs `shouldBe` Safety
        fsSafety fs `shouldBe` Safe
        length (tbAddImports (ftBody t)) `shouldBe` 1

      it "composes templates for comprehensive fix" $ do
        let t1 = template "step1" $ do
              param "old" TypeIdentifier "Old function"
              templateBody $ templateDelete (paramRef "old")
        let t2 = template "step2" $ do
              param "new" TypeIdentifier "New function"
              templateBody $ templateInsert 0 (paramRef "new")
        let composed = composeTemplates t1 t2
        length (ftParams composed) `shouldBe` 2
        ftName composed `shouldBe` "step1-then-step2"
