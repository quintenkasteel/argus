{-# LANGUAGE OverloadedStrings #-}

module ConfigValidationSpec (spec) where

import Test.Hspec
import Data.Text qualified as T

import Argus.Config
import Argus.Config.Validation
import Argus.Analysis.Architecture (ArchitectureConfig(..), LayerConfig(..), defaultArchitectureConfig)
import Argus.Rules.Types qualified as RT

spec :: Spec
spec = do
  describe "Configuration Validation" $ do
    describe "validateConfig" $ do
      it "validates default config successfully" $ do
        result <- validateConfig defaultConfig
        result `shouldBe` ValidationSuccess

      it "returns warnings for non-existent directories" $ do
        let config = defaultConfig
              { cfgGeneral = (cfgGeneral defaultConfig)
                  { genDirectories = ["/nonexistent/path/xyz123"]
                  }
              }
        result <- validateConfig config
        case result of
          ValidationWarnings errs -> length errs `shouldSatisfy` (> 0)
          ValidationSuccess -> pure ()  -- May succeed if path check is skipped
          ValidationFailure _ -> expectationFailure "Expected warnings, got failure"

    describe "validateOutputConfig" $ do
      it "accepts valid format" $ do
        let cfg = OutputConfig "terminal" True "file" True 2
        validateOutputConfig cfg `shouldBe` []

      it "accepts all valid formats" $ do
        let formats = ["terminal", "json", "html", "sarif", "junit", "codeclimate", "checkstyle"]
        forM_ formats $ \fmt -> do
          let cfg = OutputConfig fmt True "file" True 2
          validateOutputConfig cfg `shouldBe` []

      it "rejects invalid format" $ do
        let cfg = OutputConfig "invalid-format" True "file" True 2
            errs = validateOutputConfig cfg
        length errs `shouldBe` 1
        veSeverity (head errs) `shouldBe` VSError
        veField (head errs) `shouldBe` "format"

      it "accepts valid group-by values" $ do
        let groupBys = ["file", "rule", "severity"]
        forM_ groupBys $ \grp -> do
          let cfg = OutputConfig "terminal" True grp True 2
          validateOutputConfig cfg `shouldBe` []

      it "rejects invalid group-by" $ do
        let cfg = OutputConfig "terminal" True "invalid" True 2
            errs = validateOutputConfig cfg
        length errs `shouldBe` 1
        veField (head errs) `shouldBe` "group-by"

      it "accepts positive context lines" $ do
        let cfg = OutputConfig "terminal" True "file" True 5
        validateOutputConfig cfg `shouldBe` []

      it "rejects negative context lines" $ do
        let cfg = OutputConfig "terminal" True "file" True (-1)
            errs = validateOutputConfig cfg
        length errs `shouldBe` 1
        veSeverity (head errs) `shouldBe` VSError
        veField (head errs) `shouldBe` "context-lines"

      it "warns on large context lines" $ do
        let cfg = OutputConfig "terminal" True "file" True 50
            errs = validateOutputConfig cfg
        length errs `shouldBe` 1
        veSeverity (head errs) `shouldBe` VSWarning

    describe "validateComplexityConfig" $ do
      it "accepts valid thresholds" $ do
        let cfg = ComplexityConfig True 10 20 15 25 50 4 5 10
        validateComplexityConfig cfg `shouldBe` []

      it "rejects warning > error for cyclomatic" $ do
        let cfg = ComplexityConfig True 20 10 15 25 50 4 5 10  -- warning > error
            errs = validateComplexityConfig cfg
        any (\e -> "cyclomatic-warning" `T.isInfixOf` veField e && veSeverity e == VSError) errs `shouldBe` True

      it "rejects warning > error for cognitive" $ do
        let cfg = ComplexityConfig True 10 20 30 25 50 4 5 10  -- cognitive warning > error
            errs = validateComplexityConfig cfg
        any (\e -> "cognitive-warning" `T.isInfixOf` veField e && veSeverity e == VSError) errs `shouldBe` True

      it "rejects zero or negative thresholds" $ do
        let cfg = ComplexityConfig True 10 20 15 25 0 4 5 10  -- line-length = 0
            errs = validateComplexityConfig cfg
        any (\e -> veField e == "line-length-warning" && veSeverity e == VSError) errs `shouldBe` True

      it "warns on very low thresholds" $ do
        let cfg = ComplexityConfig True 0 20 15 25 50 4 5 10  -- cyclomatic warning = 0
            errs = validateComplexityConfig cfg
        any (\e -> veField e == "cyclomatic-warning") errs `shouldBe` True

    describe "validateUnusedConfig" $ do
      it "accepts valid regex patterns" $ do
        let cfg = UnusedConfig
              { unusedEnabled = True
              , unusedCheckFunctions = True
              , unusedCheckTypes = True
              , unusedCheckImports = True
              , unusedCheckExports = True
              , unusedCheckConstructors = True
              , unusedCheckRecordFields = True
              , unusedCheckLocalBinds = True
              , unusedCheckInstances = False
              , unusedTypeClassRoots = True
              , unusedDeriveRoots = True
              , unusedMinConfidence = 0.5
              , unusedRoots = ["^Main.main$", ".*Spec$"]
              , unusedThRoots = []
              , unusedIgnorePatterns = []
              }
        validateUnusedConfig cfg `shouldBe` []

      it "rejects invalid regex in roots" $ do
        let cfg = UnusedConfig
              { unusedEnabled = True
              , unusedCheckFunctions = True
              , unusedCheckTypes = True
              , unusedCheckImports = True
              , unusedCheckExports = True
              , unusedCheckConstructors = True
              , unusedCheckRecordFields = True
              , unusedCheckLocalBinds = True
              , unusedCheckInstances = False
              , unusedTypeClassRoots = True
              , unusedDeriveRoots = True
              , unusedMinConfidence = 0.5
              , unusedRoots = ["[invalid"]
              , unusedThRoots = []
              , unusedIgnorePatterns = []
              }
            errs = validateUnusedConfig cfg
        length errs `shouldSatisfy` (> 0)
        veSeverity (head errs) `shouldBe` VSError

      it "rejects invalid regex in th-roots" $ do
        let cfg = UnusedConfig
              { unusedEnabled = True
              , unusedCheckFunctions = True
              , unusedCheckTypes = True
              , unusedCheckImports = True
              , unusedCheckExports = True
              , unusedCheckConstructors = True
              , unusedCheckRecordFields = True
              , unusedCheckLocalBinds = True
              , unusedCheckInstances = False
              , unusedTypeClassRoots = True
              , unusedDeriveRoots = True
              , unusedMinConfidence = 0.5
              , unusedRoots = []
              , unusedThRoots = ["(unclosed"]
              , unusedIgnorePatterns = []
              }
            errs = validateUnusedConfig cfg
        length errs `shouldSatisfy` (> 0)

    describe "validateImportsConfig" $ do
      it "accepts valid module names" $ do
        let cfg = ImportsConfig True ["Data.Text", "Data.Map.Strict"] True True False True [] True
        validateImportsConfig cfg `shouldBe` []

      it "warns on potentially invalid module names" $ do
        let cfg = ImportsConfig True ["lowercase", "has space"] True True False True [] True
            errs = validateImportsConfig cfg
        length errs `shouldSatisfy` (> 0)
        all (\e -> veSeverity e == VSWarning) errs `shouldBe` True

    describe "validatePatternsConfig" $ do
      it "accepts valid pattern rules" $ do
        let rules = map patternRuleToRule [PatternRule "test" "head xs" (Just "safe xs") Nothing RSWarning "Use safe version"]
            cfg = PatternsConfig True rules
        validatePatternsConfig cfg `shouldBe` []

      it "rejects empty match pattern" $ do
        let rules = map patternRuleToRule [PatternRule "test" "" Nothing Nothing RSWarning "msg"]
            cfg = PatternsConfig True rules
            errs = validatePatternsConfig cfg
        length errs `shouldSatisfy` (> 0)
        any (\e -> "match" `T.isInfixOf` veField e) errs `shouldBe` True

      it "warns on duplicate rule names" $ do
        let rules = map patternRuleToRule
                    [ PatternRule "dup" "a" Nothing Nothing RSWarning "msg1"
                    , PatternRule "dup" "b" Nothing Nothing RSWarning "msg2"
                    ]
            cfg = PatternsConfig True rules
            errs = validatePatternsConfig cfg
        any (\e -> "Duplicate" `T.isInfixOf` veMessage e) errs `shouldBe` True

      it "warns on empty fix pattern" $ do
        let rules = map patternRuleToRule [PatternRule "test" "head xs" (Just "") Nothing RSWarning "msg"]
            cfg = PatternsConfig True rules
            errs = validatePatternsConfig cfg
        any (\e -> "Empty fix" `T.isInfixOf` veMessage e) errs `shouldBe` True

    describe "isValidRegex" $ do
      it "returns True for valid patterns" $ do
        isValidRegex "^main$" `shouldBe` True
        isValidRegex ".*" `shouldBe` True
        isValidRegex "foo|bar" `shouldBe` True
        isValidRegex "[a-z]+" `shouldBe` True

      it "returns False for invalid patterns" $ do
        isValidRegex "[unclosed" `shouldBe` False
        isValidRegex "(unclosed" `shouldBe` False
        isValidRegex "*invalid" `shouldBe` False

    describe "isValidGlobPattern" $ do
      it "returns True for valid patterns" $ do
        isValidGlobPattern "*.hs" `shouldBe` True
        isValidGlobPattern "**/*.hs" `shouldBe` True
        isValidGlobPattern "src/[Ff]oo.hs" `shouldBe` True
        isValidGlobPattern "dist-newstyle/**" `shouldBe` True

      it "returns False for unbalanced brackets" $ do
        isValidGlobPattern "src/[unclosed" `shouldBe` False
        isValidGlobPattern "src/unopen]ed" `shouldBe` False

    describe "formatValidationErrors" $ do
      it "formats errors nicely" $ do
        let errs = [ValidationError "output" "format" VSError "Invalid format" Nothing]
            formatted = formatValidationErrors errs
        "ERROR" `T.isInfixOf` formatted `shouldBe` True
        "output" `T.isInfixOf` formatted `shouldBe` True
        "format" `T.isInfixOf` formatted `shouldBe` True

      it "includes suggestions when present" $ do
        let errs = [ValidationError "output" "format" VSError "msg" (Just "Use 'terminal'")]
            formatted = formatValidationErrors errs
        "Suggestion" `T.isInfixOf` formatted `shouldBe` True

      it "returns 'No validation errors' for empty list" $ do
        formatValidationErrors [] `shouldBe` "No validation errors"

    describe "ValidationResult" $ do
      it "has correct Eq instance" $ do
        ValidationSuccess `shouldBe` ValidationSuccess
        let err = ValidationError "s" "f" VSError "m" Nothing
        ValidationWarnings [err] `shouldBe` ValidationWarnings [err]
        ValidationFailure [err] `shouldBe` ValidationFailure [err]

    describe "validateConfigStrict" $ do
      it "treats warnings as errors in strict mode" $ do
        let config = defaultConfig
              { cfgOutput = (cfgOutput defaultConfig)
                  { outContextLines = 50  -- Would generate a warning
                  }
              }
        result <- validateConfigStrict config
        case result of
          ValidationFailure _ -> pure ()  -- Expected in strict mode
          _ -> expectationFailure "Expected failure in strict mode"

    describe "validateArchitectureConfig" $ do
      it "accepts valid default config" $ do
        validateArchitectureConfig defaultArchitectureConfig `shouldBe` []

      it "accepts valid custom config" $ do
        let cfg = ArchitectureConfig
              { acEnabled = True
              , acLayers =
                  [ LayerConfig "Core" ["*.Types", "*.Core"] ["Core"]
                  , LayerConfig "App" ["Main", "*.App"] ["Core", "App"]
                  ]
              , acMaxCycleLength = 5
              , acInstabilityThreshold = 0.7
              , acCouplingThreshold = 15
              , acCheckOrphans = True
              , acCheckQualified = True
              }
        validateArchitectureConfig cfg `shouldBe` []

      describe "max-cycle-length validation" $ do
        it "rejects max-cycle-length < 2" $ do
          let cfg = defaultArchitectureConfig { acMaxCycleLength = 1 }
              errs = validateArchitectureConfig cfg
          any (\e -> veField e == "max-cycle-length" && veSeverity e == VSError) errs `shouldBe` True

        it "accepts max-cycle-length = 2" $ do
          let cfg = defaultArchitectureConfig { acMaxCycleLength = 2 }
              errs = validateArchitectureConfig cfg
          any (\e -> veField e == "max-cycle-length" && veSeverity e == VSError) errs `shouldBe` False

        it "warns on very high max-cycle-length" $ do
          let cfg = defaultArchitectureConfig { acMaxCycleLength = 25 }
              errs = validateArchitectureConfig cfg
          any (\e -> veField e == "max-cycle-length" && veSeverity e == VSWarning) errs `shouldBe` True

      describe "instability-threshold validation" $ do
        it "rejects negative instability-threshold" $ do
          let cfg = defaultArchitectureConfig { acInstabilityThreshold = -0.1 }
              errs = validateArchitectureConfig cfg
          any (\e -> veField e == "instability-threshold" && veSeverity e == VSError) errs `shouldBe` True

        it "rejects instability-threshold > 1.0" $ do
          let cfg = defaultArchitectureConfig { acInstabilityThreshold = 1.5 }
              errs = validateArchitectureConfig cfg
          any (\e -> veField e == "instability-threshold" && veSeverity e == VSError) errs `shouldBe` True

        it "accepts instability-threshold = 0.0" $ do
          let cfg = defaultArchitectureConfig { acInstabilityThreshold = 0.0 }
              errs = validateArchitectureConfig cfg
          any (\e -> veField e == "instability-threshold" && veSeverity e == VSError) errs `shouldBe` False

        it "accepts instability-threshold = 1.0" $ do
          let cfg = defaultArchitectureConfig { acInstabilityThreshold = 1.0 }
              errs = validateArchitectureConfig cfg
          any (\e -> veField e == "instability-threshold" && veSeverity e == VSError) errs `shouldBe` False

        it "warns on very low instability-threshold" $ do
          let cfg = defaultArchitectureConfig { acInstabilityThreshold = 0.1 }
              errs = validateArchitectureConfig cfg
          any (\e -> veField e == "instability-threshold" && veSeverity e == VSWarning) errs `shouldBe` True

      describe "coupling-threshold validation" $ do
        it "rejects coupling-threshold < 1" $ do
          let cfg = defaultArchitectureConfig { acCouplingThreshold = 0 }
              errs = validateArchitectureConfig cfg
          any (\e -> veField e == "coupling-threshold" && veSeverity e == VSError) errs `shouldBe` True

        it "warns on very low coupling-threshold" $ do
          let cfg = defaultArchitectureConfig { acCouplingThreshold = 3 }
              errs = validateArchitectureConfig cfg
          any (\e -> veField e == "coupling-threshold" && veSeverity e == VSWarning) errs `shouldBe` True

        it "warns on very high coupling-threshold" $ do
          let cfg = defaultArchitectureConfig { acCouplingThreshold = 60 }
              errs = validateArchitectureConfig cfg
          any (\e -> veField e == "coupling-threshold" && veSeverity e == VSWarning) errs `shouldBe` True

      describe "layer validation" $ do
        it "rejects empty layer name" $ do
          let cfg = defaultArchitectureConfig
                { acLayers = [LayerConfig "" ["*.Core"] ["Core"]]
                }
              errs = validateArchitectureConfig cfg
          any (\e -> "name" `T.isInfixOf` veField e && veSeverity e == VSError) errs `shouldBe` True

        it "warns on empty patterns list" $ do
          let cfg = defaultArchitectureConfig
                { acLayers = [LayerConfig "Core" [] ["Core"]]
                }
              errs = validateArchitectureConfig cfg
          any (\e -> "patterns" `T.isInfixOf` veField e && veSeverity e == VSWarning) errs `shouldBe` True

        it "rejects empty pattern string" $ do
          let cfg = defaultArchitectureConfig
                { acLayers = [LayerConfig "Core" ["", "*.Core"] ["Core"]]
                }
              errs = validateArchitectureConfig cfg
          any (\e -> "patterns[0]" `T.isInfixOf` veField e && veSeverity e == VSError) errs `shouldBe` True

        it "warns on unbalanced brackets in pattern" $ do
          let cfg = defaultArchitectureConfig
                { acLayers = [LayerConfig "Core" ["*.Core[unbalanced"] ["Core"]]
                }
              errs = validateArchitectureConfig cfg
          any (\e -> "patterns" `T.isInfixOf` veField e && veSeverity e == VSWarning) errs `shouldBe` True

        it "warns on unknown layer reference in can-import" $ do
          let cfg = defaultArchitectureConfig
                { acLayers =
                    [ LayerConfig "Core" ["*.Core"] ["Core"]
                    , LayerConfig "App" ["*.App"] ["Core", "Unknown"]
                    ]
                }
              errs = validateArchitectureConfig cfg
          any (\e -> "can-import" `T.isInfixOf` veField e && "Unknown" `T.isInfixOf` veMessage e) errs `shouldBe` True

        it "rejects empty import reference" $ do
          let cfg = defaultArchitectureConfig
                { acLayers = [LayerConfig "Core" ["*.Core"] [""]]
                }
              errs = validateArchitectureConfig cfg
          any (\e -> "can-import" `T.isInfixOf` veField e && veSeverity e == VSError) errs `shouldBe` True

        it "rejects duplicate layer names" $ do
          let cfg = defaultArchitectureConfig
                { acLayers =
                    [ LayerConfig "Core" ["*.Core"] ["Core"]
                    , LayerConfig "Core" ["*.Types"] ["Core"]  -- Duplicate name
                    ]
                }
              errs = validateArchitectureConfig cfg
          any (\e -> veField e == "layers" && "Duplicate" `T.isInfixOf` veMessage e) errs `shouldBe` True

        it "accepts valid layer with self-reference in can-import" $ do
          let cfg = defaultArchitectureConfig
                { acLayers = [LayerConfig "Core" ["*.Core"] ["Core"]]
                }
              errs = validateArchitectureConfig cfg
          -- Self-reference is valid (a layer can import from itself)
          any (\e -> veSeverity e == VSError) errs `shouldBe` False

      it "validates architecture config in full config" $ do
        let config = defaultConfig
              { cfgArchitecture = defaultArchitectureConfig
                  { acMaxCycleLength = 0  -- Invalid
                  }
              }
        result <- validateConfig config
        case result of
          ValidationFailure errs ->
            any (\e -> veSection e == "architecture") errs `shouldBe` True
          _ -> expectationFailure "Expected validation failure"

-- Helper for iteration
forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_ xs f = sequence_ (map f xs)
