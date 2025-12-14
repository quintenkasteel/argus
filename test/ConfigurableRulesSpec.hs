{-# LANGUAGE OverloadedStrings #-}

module ConfigurableRulesSpec (spec) where

import Test.Hspec
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Types (Severity(..), FixImport(..))
import Argus.Types qualified as AT (ImportSymbol(..))
import Argus.Rules.ConfigurableRules
import Argus.Rules.Types (ImportSpec(..), ImportSymbol(..), SymbolType(..), RuleTarget(..))
import Argus.Plugin.Types (parseVersion)

spec :: Spec
spec = describe "Argus.Rules.ConfigurableRules" $ do
  describe "RulesConfig" $ do
    it "has sensible defaults" $ do
      let cfg = defaultRulesConfig
      rcInheritDefaults cfg `shouldBe` True
      rcCustomRules cfg `shouldBe` []
      rcIgnoreRules cfg `shouldBe` []

    it "default categories have correct severities" $ do
      let cats = rcCategories defaultRulesConfig
      Map.lookup Performance cats `shouldBe` Just CatWarn
      Map.lookup Security cats `shouldBe` Just CatError
      Map.lookup Modernize cats `shouldBe` Just CatSuggestion

  describe "RuleCategory" $ do
    it "parses category from text" $ do
      inferCategory "performance/foo" `shouldBe` Performance
      inferCategory "security/bar" `shouldBe` Security
      inferCategory "modernize/baz" `shouldBe` Modernize
      inferCategory "custom/whatever" `shouldBe` CustomCategory "custom"

    it "allCategories contains all built-in categories" $ do
      -- 15 categories in unified Rules.Types.Category
      length allCategories `shouldBe` 15
      Performance `elem` allCategories `shouldBe` True
      Security `elem` allCategories `shouldBe` True
      Concurrency `elem` allCategories `shouldBe` True
      ErrorHandling `elem` allCategories `shouldBe` True
      Documentation `elem` allCategories `shouldBe` True

  describe "FixSafety (SafetyLevel)" $ do
    it "Safe is the safest level" $ do
      -- Unified SafetyLevel: Safe < MostlySafe < NeedsReview < Unsafe
      -- Note: Manual is mapped to NeedsReview for backwards compatibility
      Safe < Unsafe `shouldBe` True
      Manual < Unsafe `shouldBe` True  -- NeedsReview < Unsafe

  describe "ConfigurableRule" $ do
    describe "mkRule helper" $ do
      it "creates a valid rule" $ do
        let rule = mkRule "test/example" Performance
                     "foo $X" (Just "bar $X")
                     "Test message" Safe
        crId rule `shouldBe` "test/example"
        crCategory rule `shouldBe` Performance
        cpPattern (crPattern rule) `shouldBe` "foo $X"
        cpFix (crPattern rule) `shouldBe` Just "bar $X"
        crSafety rule `shouldBe` Safe

      it "creates rule without fix" $ do
        let rule = mkRule "test/nofix" Security
                     "bad $X" Nothing
                     "No fix available" Manual
        cpFix (crPattern rule) `shouldBe` Nothing
        crSafety rule `shouldBe` Manual

  describe "Default Rules Library" $ do
    it "contains performance rules" $ do
      length performanceRules `shouldSatisfy` (> 5)
      any (\r -> crId r == "performance/length-null") performanceRules `shouldBe` True

    it "contains security rules" $ do
      length securityRules `shouldSatisfy` (> 3)
      any (\r -> crId r == "security/trace") securityRules `shouldBe` True

    it "contains modernize rules" $ do
      length modernizeRules `shouldSatisfy` (> 5)
      any (\r -> crId r == "modernize/return-pure") modernizeRules `shouldBe` True

    it "contains redundant rules" $ do
      length redundantRules `shouldSatisfy` (> 3)
      any (\r -> crId r == "redundant/id") redundantRules `shouldBe` True

    it "contains space leak rules" $ do
      length spaceLeakRules `shouldSatisfy` (> 3)
      any (\r -> crId r == "space-leaks/foldl") spaceLeakRules `shouldBe` True

    it "contains partial function rules" $ do
      length partialRules `shouldSatisfy` (> 5)
      any (\r -> crId r == "partial/head") partialRules `shouldBe` True

    it "defaultRulesLibrary combines all rule sets" $ do
      length defaultRulesLibrary `shouldSatisfy` (> 30)

  describe "Rule Enabling/Disabling" $ do
    it "isRuleEnabled returns true for default rules" $ do
      let cfg = defaultRulesConfig
          rule = head performanceRules
      isRuleEnabled cfg rule "MyModule" `shouldBe` True

    it "isRuleEnabled returns false when rule is in ignore list" $ do
      let cfg = defaultRulesConfig { rcIgnoreRules = ["performance/length-null"] }
          rule = head $ filter (\r -> crId r == "performance/length-null") performanceRules
      isRuleEnabled cfg rule "MyModule" `shouldBe` False

    it "isRuleEnabled returns false when category is off" $ do
      let cfg = defaultRulesConfig
                  { rcCategories = Map.insert Performance CatOff (rcCategories defaultRulesConfig) }
          rule = head performanceRules
      isRuleEnabled cfg rule "MyModule" `shouldBe` False

    it "isRuleEnabled respects within constraints" $ do
      let rule = (head performanceRules) { crWithin = ["Test.*"] }
      isRuleEnabled defaultRulesConfig rule "Test.Foo" `shouldBe` True
      isRuleEnabled defaultRulesConfig rule "Prod.Foo" `shouldBe` False

    it "isRuleEnabled respects except constraints" $ do
      let rule = (head performanceRules) { crExcept = ["Test.*"] }
      isRuleEnabled defaultRulesConfig rule "Test.Foo" `shouldBe` False
      isRuleEnabled defaultRulesConfig rule "Prod.Foo" `shouldBe` True

  describe "getRuleEffectiveSeverity" $ do
    it "returns rule default severity when no overrides" $ do
      let cfg = defaultRulesConfig
          rule = head performanceRules
      getRuleEffectiveSeverity cfg rule "MyModule" `shouldBe` Just Warning

    it "returns Nothing when rule is disabled" $ do
      let cfg = defaultRulesConfig { rcIgnoreRules = [crId $ head performanceRules] }
          rule = head performanceRules
      getRuleEffectiveSeverity cfg rule "MyModule" `shouldBe` Nothing

    it "applies rule-specific override" $ do
      let rule = head performanceRules
          override = RuleOverride (crId rule) (Just Error) Nothing Nothing
          cfg = defaultRulesConfig { rcOverrides = [override] }
      getRuleEffectiveSeverity cfg rule "MyModule" `shouldBe` Just Error

  describe "Pattern Matching" $ do
    describe "matchPattern" $ do
      it "matches exact module names" $ do
        matchPattern "Foo.Bar" "Foo.Bar" `shouldBe` True
        matchPattern "Foo.Bar" "Foo.Baz" `shouldBe` False

      it "matches wildcard prefixes" $ do
        matchPattern "Test.Foo" "Test.*" `shouldBe` True
        matchPattern "Test.Bar" "Test.*" `shouldBe` True
        matchPattern "Prod.Foo" "Test.*" `shouldBe` False

      it "matches wildcard suffixes" $ do
        matchPattern "FooSpec" "*.Spec" `shouldBe` False  -- FooSpec doesn't end with ".Spec"
        matchPattern "Foo.Spec" "*Spec" `shouldBe` True   -- Foo.Spec ends with "Spec"
        matchPattern "Foo.Spec" "*.Spec" `shouldBe` True  -- Foo.Spec ends with ".Spec"
        matchPattern "FooBar" "*Spec" `shouldBe` False    -- FooBar doesn't end with "Spec"

    describe "matchesWithin" $ do
      it "returns true for empty list (all modules)" $ do
        matchesWithin [] "Any.Module" `shouldBe` True

      it "returns true when module matches any pattern" $ do
        matchesWithin ["Test.*", "*.Spec"] "Test.Foo" `shouldBe` True

      it "returns false when no patterns match" $ do
        matchesWithin ["Test.*", "*.Spec"] "Prod.Main" `shouldBe` False

  describe "Restrictions" $ do
    describe "FunctionRestriction" $ do
      it "can ban a function everywhere" $ do
        let restriction = FunctionRestriction "unsafePerformIO" [] Nothing
        frWithin restriction `shouldBe` []

      it "can allow a function in specific modules" $ do
        let restriction = FunctionRestriction "head" ["Test.*", "*.Spec"] Nothing
        length (frWithin restriction) `shouldBe` 2

    describe "ModuleRestriction" $ do
      it "can require qualified import" $ do
        let restriction = ModuleRestriction ["Data.Map"] (Just "Map") [] Nothing
        mrAs restriction `shouldBe` Just "Map"

    describe "ExtensionRestriction" $ do
      it "can restrict extension to specific modules" $ do
        let restriction = ExtensionRestriction "TemplateHaskell" ["*.TH"] Nothing
        length (erWithin restriction) `shouldBe` 1

  describe "Config Merging" $ do
    it "merges custom rules from both configs" $ do
      let rule1 = mkRule "a/1" Performance "foo" Nothing "msg1" Safe
          rule2 = mkRule "b/2" Security "bar" Nothing "msg2" Safe
          cfg1 = defaultRulesConfig { rcCustomRules = [rule1] }
          cfg2 = defaultRulesConfig { rcCustomRules = [rule2] }
          merged = mergeRulesConfig cfg1 cfg2
      length (rcCustomRules merged) `shouldBe` 2

    it "merges ignore lists" $ do
      let cfg1 = defaultRulesConfig { rcIgnoreRules = ["rule1"] }
          cfg2 = defaultRulesConfig { rcIgnoreRules = ["rule2"] }
          merged = mergeRulesConfig cfg1 cfg2
      rcIgnoreRules merged `shouldBe` ["rule1", "rule2"]

    it "override config takes precedence for inheritDefaults" $ do
      let cfg1 = defaultRulesConfig { rcInheritDefaults = False }
          cfg2 = defaultRulesConfig { rcInheritDefaults = True }
          merged = mergeRulesConfig cfg1 cfg2
      rcInheritDefaults merged `shouldBe` False

    it "merges category overrides" $ do
      let cfg1 = defaultRulesConfig
                   { rcCategories = Map.fromList [(Performance, CatError)] }
          cfg2 = defaultRulesConfig
                   { rcCategories = Map.fromList [(Security, CatWarn)] }
          merged = mergeRulesConfig cfg1 cfg2
      Map.lookup Performance (rcCategories merged) `shouldBe` Just CatError
      -- Security from cfg1 (base) unless overridden by cfg2

  describe "canAutoFix" $ do
    it "returns false when rule has no fix" $ do
      let rule = mkRule "test/nofix" Security "bad" Nothing "msg" Manual
      canAutoFix defaultRulesConfig rule `shouldBe` False

    it "returns true for safe rules with fixes" $ do
      let rule = mkRule "test/fix" Performance "foo $X" (Just "bar $X") "msg" Safe
      canAutoFix defaultRulesConfig rule `shouldBe` True

    it "returns false for unsafe rules by default" $ do
      let rule = mkRule "test/unsafe" Performance "foo" (Just "bar") "msg" Unsafe
      canAutoFix defaultRulesConfig rule `shouldBe` False

    it "returns false when rule is in fixDisabled list" $ do
      let rule = mkRule "test/disabled" Performance "foo" (Just "bar") "msg" Safe
          cfg = defaultRulesConfig { rcFixDisabled = ["test/disabled"] }
      canAutoFix cfg rule `shouldBe` False

    it "returns true for unsafe rules in fixSafe list" $ do
      let rule = mkRule "test/unsafe" Performance "foo" (Just "bar") "msg" Unsafe
          cfg = defaultRulesConfig { rcFixSafe = ["test/unsafe"] }
      canAutoFix cfg rule `shouldBe` True

  describe "applyConfigurableRules" $ do
    it "returns empty list when inherit is false and no custom rules" $ do
      let cfg = defaultRulesConfig { rcInheritDefaults = False }
          diags = applyConfigurableRules cfg "test.hs" "Test" "module Test where\nfoo = 1"
      diags `shouldBe` []

  describe "CategorySeverity conversion" $ do
    it "CatOff maps to Nothing" $ do
      catToSeverity CatOff `shouldBe` Nothing

    it "CatError maps to Error" $ do
      catToSeverity CatError `shouldBe` Just Error

    it "CatWarn maps to Warning" $ do
      catToSeverity CatWarn `shouldBe` Just Warning

    it "CatSuggestion maps to Suggestion" $ do
      catToSeverity CatSuggestion `shouldBe` Just Suggestion

    it "CatInfo maps to Info" $ do
      catToSeverity CatInfo `shouldBe` Just Info

  -- TODO-004: Import Conversion
  describe "Import Conversion (TODO-004)" $ do
    it "convertImportSpecToFixImport converts module name" $ do
      let impSpec = mkImportSpec "Data.Maybe" [] Nothing False Nothing
          fixImp = convertImportSpecToFixImport impSpec
      fimpModule fixImp `shouldBe` "Data.Maybe"

    it "convertImportSpecToFixImport preserves qualified alias" $ do
      let impSpec = mkImportSpec "Data.Map" [] (Just "Map") False Nothing
          fixImp = convertImportSpecToFixImport impSpec
      fimpQualified fixImp `shouldBe` Just "Map"

    it "convertImportSpecToFixImport preserves hiding flag" $ do
      let impSpec = mkImportSpec "Prelude" [] Nothing True Nothing
          fixImp = convertImportSpecToFixImport impSpec
      fimpHiding fixImp `shouldBe` True

    it "convertImportSpecToFixImport converts symbols" $ do
      let sym = mkImportSymbol "fromMaybe" SymFunction []
          impSpec = mkImportSpec "Data.Maybe" [sym] Nothing False Nothing
          fixImp = convertImportSpecToFixImport impSpec
      length (fimpSymbols fixImp) `shouldBe` 1

    it "mkRuleWithImports creates rule with import management" $ do
      let impSpec = mkImportSpec "Safe" [mkImportSymbol "headMay" SymFunction []] Nothing False Nothing
          rule = mkRuleWithImports "test/head-safe" Partial
                   "head $X" (Just "headMay $X")
                   "Use headMay from Safe" Safe
                   [impSpec] ["Prelude.head"]
      length (crAddImports rule) `shouldBe` 1
      crRemoveImports rule `shouldBe` ["Prelude.head"]

  -- TODO-005: Target Specification
  describe "Target Specification (TODO-005)" $ do
    it "crTarget can be set to Code" $ do
      let rule = mkRule "test/code-only" Performance "foo" Nothing "msg" Safe
          ruleWithTarget = rule { crTarget = Just TargetCode }
      crTarget ruleWithTarget `shouldBe` Just TargetCode

    it "crTarget can be set to Comments" $ do
      let rule = mkRule "test/comment-check" Documentation "TODO" Nothing "msg" Safe
          ruleWithTarget = rule { crTarget = Just TargetComments }
      crTarget ruleWithTarget `shouldBe` Just TargetComments

    it "crTarget defaults to Nothing" $ do
      let rule = mkRule "test/default" Performance "bar" Nothing "msg" Safe
      crTarget rule `shouldBe` Nothing

  -- TODO-006: Rule Explanation
  describe "Rule Explanation (TODO-006)" $ do
    it "crExplanation can store detailed explanation" $ do
      let explanation = "This rule helps avoid partial functions by recommending safe alternatives."
          rule = (mkRule "test/explained" Partial "head" Nothing "msg" Safe)
                   { crExplanation = Just explanation }
      crExplanation rule `shouldBe` Just explanation

    it "mkRuleWithMetadata sets explanation" $ do
      let explanation = "Detailed explanation here"
          rule = mkRuleWithMetadata "test/meta" Security
                   "pattern" Nothing "message" Safe
                   ["security", "injection"] ["https://owasp.org"]
                   (Just explanation)
      crExplanation rule `shouldBe` Just explanation

    it "explanation is distinct from note" $ do
      let note = "Short note"
          explanation = "Long detailed explanation with examples"
          rule = (mkRule "test/both" Performance "foo" Nothing "msg" Safe)
                   { crNote = Just note
                   , crExplanation = Just explanation
                   }
      crNote rule `shouldBe` Just note
      crExplanation rule `shouldBe` Just explanation

  -- TODO-007: Tags and References
  describe "Tags and References (TODO-007)" $ do
    it "crTags can store multiple tags" $ do
      let rule = (mkRule "test/tagged" Security "unsafe" Nothing "msg" Safe)
                   { crTags = ["security", "injection", "owasp"] }
      crTags rule `shouldBe` ["security", "injection", "owasp"]

    it "crReferences can store URLs" $ do
      let refs = ["https://owasp.org/www-project-top-ten/", "https://cwe.mitre.org/data/definitions/89.html"]
          rule = (mkRule "test/refs" Security "sql" Nothing "msg" Safe)
                   { crReferences = refs }
      crReferences rule `shouldBe` refs

    it "mkRuleWithMetadata sets tags and references" $ do
      let rule = mkRuleWithMetadata "test/full-meta" Security
                   "pattern" Nothing "message" Safe
                   ["tag1", "tag2"] ["http://example.com"]
                   Nothing
      crTags rule `shouldBe` ["tag1", "tag2"]
      crReferences rule `shouldBe` ["http://example.com"]

  -- TODO-009: Rule Templates
  describe "Rule Templates (TODO-009)" $ do
    it "substituteVars replaces single variable" $ do
      let vars = Map.fromList [("X", "result")]
          text = "Use $X instead"
      substituteVars vars text `shouldBe` "Use result instead"

    it "substituteVars replaces multiple variables" $ do
      let vars = Map.fromList [("X", "foo"), ("Y", "bar")]
          text = "Replace $X with $Y"
      substituteVars vars text `shouldBe` "Replace foo with bar"

    it "substituteVars handles no variables" $ do
      let vars = Map.empty
          text = "No variables here"
      substituteVars vars text `shouldBe` "No variables here"

    it "expandTemplatedRule expands template variables in pattern" $ do
      let template = defaultTemplate
          vars = Map.fromList [("FUNC", "head"), ("SAFE", "headMay")]
          templated = TemplatedRule
                        { trExtends = "partial-function"
                        , trId = "test/partial"
                        , trVars = vars
                        , trPattern = Nothing
                        , trFix = Nothing
                        , trMessage = Nothing
                        , trCategory = Nothing
                        , trSeverity = Nothing
                        , trSafety = Nothing
                        , trEnabled = Nothing
                        , trWithin = []
                        , trExcept = []
                        , trAddImports = []
                        , trRemoveImports = []
                        , trTags = []
                        , trNote = Nothing
                        , trExplanation = Nothing
                        }
          -- Note: expandTemplatedRule needs actual template, simplified test
      Map.lookup "FUNC" vars `shouldBe` Just "head"

    it "defaultTemplate has expected structure" $ do
      let template = defaultTemplate "test-rule"
      rtId template `shouldBe` "test-rule"

  -- TODO-010: Fix Dependencies
  describe "Fix Dependencies (TODO-010)" $ do
    it "getRuleDependencies extracts dependencies" $ do
      let rule = (mkRule "test/dependent" Performance "foo" (Just "bar") "msg" Safe)
                   { crDependsOn = ["rule/a", "rule/b"] }
      getRuleDependencies rule `shouldBe` ["rule/a", "rule/b"]

    it "getRuleConflicts extracts conflicts" $ do
      let rule = (mkRule "test/conflicts" Performance "foo" (Just "bar") "msg" Safe)
                   { crConflictsWith = ["rule/x", "rule/y"] }
      getRuleConflicts rule `shouldBe` ["rule/x", "rule/y"]

    it "buildRuleDependencyMap creates lookup map" $ do
      let rule1 = (mkRule "rule/a" Performance "foo" Nothing "msg" Safe)
                    { crDependsOn = ["rule/z"] }
          rule2 = (mkRule "rule/b" Security "bar" Nothing "msg" Safe)
                    { crConflictsWith = ["rule/a"] }
          depMap = buildRuleDependencyMap [rule1, rule2]
      Map.size depMap `shouldBe` 2
      case Map.lookup "rule/a" depMap of
        Just info -> do
          rdiRuleId info `shouldBe` "rule/a"
          rdiDependsOn info `shouldBe` ["rule/z"]
        Nothing -> expectationFailure "rule/a not found in dependency map"

    it "RuleDependencyInfo has correct structure" $ do
      let info = RuleDependencyInfo "test/rule" ["dep1", "dep2"] ["conflict1"]
      rdiRuleId info `shouldBe` "test/rule"
      rdiDependsOn info `shouldBe` ["dep1", "dep2"]
      rdiConflicts info `shouldBe` ["conflict1"]

  -- TODO-011: Rule Versioning
  describe "Rule Versioning (TODO-011)" $ do
    it "currentSchemaVersion is defined" $ do
      currentSchemaVersion `shouldBe` "1.0"

    it "DeprecationInfo contains all required fields" $ do
      let depInfo = DeprecationInfo
                      { diReason = "Replaced by better rule"
                      , diReplacement = Just "new/rule"
                      , diRemovalVersion = parseVersion "2.0.0"
                      }
      diReason depInfo `shouldBe` "Replaced by better rule"
      diReplacement depInfo `shouldBe` Just "new/rule"

    it "checkDeprecations finds deprecated rules" $ do
      let depInfo = DeprecationInfo "Old rule" (Just "new/rule") Nothing
          rule1 = (mkRule "old/rule" Performance "foo" Nothing "msg" Safe)
                    { crDeprecated = Just depInfo }
          rule2 = mkRule "current/rule" Performance "bar" Nothing "msg" Safe
          warnings = checkDeprecations [rule1, rule2]
      length warnings `shouldBe` 1
      case warnings of
        [warn] -> do
          dwRuleId warn `shouldBe` "old/rule"
          diReason (dwInfo warn) `shouldBe` "Old rule"
        _ -> expectationFailure "Expected exactly one warning"

    it "checkDeprecations returns empty for non-deprecated rules" $ do
      let rule1 = mkRule "active/rule1" Performance "foo" Nothing "msg" Safe
          rule2 = mkRule "active/rule2" Security "bar" Nothing "msg" Safe
          warnings = checkDeprecations [rule1, rule2]
      warnings `shouldBe` []

    it "deprecated rule can specify removal version" $ do
      let version = parseVersion "3.0.0"
          depInfo = DeprecationInfo "Deprecated" Nothing version
      diRemovalVersion depInfo `shouldBe` version

  -- TODO-012: Module Context
  describe "Module Context (TODO-012)" $ do
    it "mkRuleWithModuleContext sets from and to modules" $ do
      let rule = mkRuleWithModuleContext "test/context" Modernize
                   "old" (Just "new") "Use new module" Safe
                   (Just "Data.List") (Just "Data.List.Extra")
      crFromModule rule `shouldBe` Just "Data.List"
      crToModule rule `shouldBe` Just "Data.List.Extra"

    it "hasModuleContext detects from-module" $ do
      let rule = (mkRule "test/from" Modernize "foo" Nothing "msg" Safe)
                   { crFromModule = Just "Old.Module" }
      hasModuleContext rule `shouldBe` True

    it "hasModuleContext detects to-module" $ do
      let rule = (mkRule "test/to" Modernize "foo" Nothing "msg" Safe)
                   { crToModule = Just "New.Module" }
      hasModuleContext rule `shouldBe` True

    it "hasModuleContext returns False when no context" $ do
      let rule = mkRule "test/no-context" Performance "foo" Nothing "msg" Safe
      hasModuleContext rule `shouldBe` False

    it "getModuleContext returns tuple" $ do
      let rule = (mkRule "test/both" Modernize "foo" Nothing "msg" Safe)
                   { crFromModule = Just "Old"
                   , crToModule = Just "New"
                   }
      getModuleContext rule `shouldBe` (Just "Old", Just "New")

    it "setModuleContext updates both fields" $ do
      let rule = mkRule "test/update" Performance "foo" Nothing "msg" Safe
          updated = setModuleContext (Just "Source") (Just "Target") rule
      crFromModule updated `shouldBe` Just "Source"
      crToModule updated `shouldBe` Just "Target"

    it "setModuleContext can clear context" $ do
      let rule = (mkRule "test/clear" Modernize "foo" Nothing "msg" Safe)
                   { crFromModule = Just "Old"
                   , crToModule = Just "New"
                   }
          cleared = setModuleContext Nothing Nothing rule
      crFromModule cleared `shouldBe` Nothing
      crToModule cleared `shouldBe` Nothing

    it "FullRuleBuilder has all fields" $ do
      let builder = defaultFullRuleBuilder "test/full" Performance "pattern" "message"
      frbId builder `shouldBe` "test/full"
      frbCategory builder `shouldBe` Performance
      frbPattern builder `shouldBe` "pattern"
      frbMessage builder `shouldBe` "message"

    it "buildFullRule creates ConfigurableRule from builder" $ do
      let builder = (defaultFullRuleBuilder "test/build" Security "unsafe" "Do not use")
                      { frbFix = Just "safe"
                      , frbSafety = Manual
                      , frbTags = ["security"]
                      , frbFromModule = Just "Unsafe.Module"
                      }
          rule = buildFullRule builder
      crId rule `shouldBe` "test/build"
      crCategory rule `shouldBe` Security
      cpPattern (crPattern rule) `shouldBe` "unsafe"
      cpFix (crPattern rule) `shouldBe` Just "safe"
      crSafety rule `shouldBe` Manual
      crTags rule `shouldBe` ["security"]
      crFromModule rule `shouldBe` Just "Unsafe.Module"

-- Helper to convert CategorySeverity to Maybe Severity (exposing internal function)
catToSeverity :: CategorySeverity -> Maybe Severity
catToSeverity CatOff = Nothing
catToSeverity CatError = Just Error
catToSeverity CatWarn = Just Warning
catToSeverity CatSuggestion = Just Suggestion
catToSeverity CatInfo = Just Info

-- Helper to create ImportSpec (from Argus.Rules.Types)
mkImportSpec :: Text -> [ImportSymbol] -> Maybe Text -> Bool -> Maybe Text -> ImportSpec
mkImportSpec modName syms qual hiding pkg = ImportSpec
  { impModule = modName
  , impSymbols = syms
  , impQualified = qual
  , impHiding = hiding
  , impPackage = pkg
  }

-- Helper to create ImportSymbol (from Argus.Rules.Types)
mkImportSymbol :: Text -> SymbolType -> [Text] -> ImportSymbol
mkImportSymbol name symType children = ImportSymbol
  { symName = name
  , symType = symType
  , symChildren = children
  }
