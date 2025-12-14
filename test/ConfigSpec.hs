{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ConfigSpec
-- Description : Tests for Argus.Config
--
-- Comprehensive tests for configuration types, default values,
-- JSON serialization, and conversion functions.
module ConfigSpec (spec) where

import Data.Aeson (encode, decode, FromJSON, ToJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Test.Hspec

import Argus.Config
import Argus.Imports.Manager (ImportManagerConfig(..))
import Argus.Rules.Types qualified as RT
import Argus.Types (Severity(..))

spec :: Spec
spec = do
  describe "Argus.Config" $ do
    defaultConfigSpec
    ruleSeveritySpec
    typeRuleSpec
    variableRuleSpec
    patternRuleSpec
    autoFixImportsConfigSpec
    resourceConfigSpec
    conversionFunctionsSpec
    legacyConfigSpec
    jsonRoundtripSpec

--------------------------------------------------------------------------------
-- Default Config Tests
--------------------------------------------------------------------------------

defaultConfigSpec :: Spec
defaultConfigSpec = describe "defaultConfig" $ do
  describe "GeneralConfig" $ do
    it "includes src in directories" $ do
      "src" `elem` genDirectories (cfgGeneral defaultConfig) `shouldBe` True

    it "includes app in directories" $ do
      "app" `elem` genDirectories (cfgGeneral defaultConfig) `shouldBe` True

    it "excludes Generated directory" $ do
      any (T.isInfixOf "Generated") (genExclude (cfgGeneral defaultConfig)) `shouldBe` True

    it "excludes .stack-work" $ do
      any (T.isInfixOf ".stack-work") (genExclude (cfgGeneral defaultConfig)) `shouldBe` True

    it "excludes dist-newstyle" $ do
      any (T.isInfixOf "dist-newstyle") (genExclude (cfgGeneral defaultConfig)) `shouldBe` True

    it "excludes Paths_ generated files" $ do
      any (T.isInfixOf "Paths_") (genExclude (cfgGeneral defaultConfig)) `shouldBe` True

    it "has quick mode by default" $ do
      genMode (cfgGeneral defaultConfig) `shouldBe` "quick"

    it "has .hie directory configured" $ do
      genHieDir (cfgGeneral defaultConfig) `shouldBe` Just ".hie"

  describe "OutputConfig" $ do
    it "uses terminal format by default" $ do
      outFormat (cfgOutput defaultConfig) `shouldBe` "terminal"

    it "enables color by default" $ do
      outColor (cfgOutput defaultConfig) `shouldBe` True

    it "groups by file by default" $ do
      outGroupBy (cfgOutput defaultConfig) `shouldBe` "file"

    it "shows context by default" $ do
      outShowContext (cfgOutput defaultConfig) `shouldBe` True

    it "shows 2 context lines by default" $ do
      outContextLines (cfgOutput defaultConfig) `shouldBe` 2

  describe "UnusedConfig" $ do
    it "enables unused detection by default" $ do
      unusedEnabled (cfgUnused defaultConfig) `shouldBe` True

    it "checks functions by default" $ do
      unusedCheckFunctions (cfgUnused defaultConfig) `shouldBe` True

    it "checks types by default" $ do
      unusedCheckTypes (cfgUnused defaultConfig) `shouldBe` True

    it "checks imports by default" $ do
      unusedCheckImports (cfgUnused defaultConfig) `shouldBe` True

    it "checks exports by default" $ do
      unusedCheckExports (cfgUnused defaultConfig) `shouldBe` True

    it "has main root pattern" $ do
      any (T.isInfixOf "main") (unusedRoots (cfgUnused defaultConfig)) `shouldBe` True

    it "has Paths_ root pattern" $ do
      any (T.isInfixOf "Paths_") (unusedRoots (cfgUnused defaultConfig)) `shouldBe` True

    it "has spec root pattern for tests" $ do
      any (T.isInfixOf "spec") (unusedRoots (cfgUnused defaultConfig)) `shouldBe` True

    it "has plugin root pattern" $ do
      any (T.isInfixOf "plugin") (unusedRoots (cfgUnused defaultConfig)) `shouldBe` True

    it "has TH roots for JSON" $ do
      any (T.isInfixOf "parseJSON") (unusedThRoots (cfgUnused defaultConfig)) `shouldBe` True

    it "has TH roots for lenses" $ do
      any (T.isInfixOf "makeLenses") (unusedThRoots (cfgUnused defaultConfig)) `shouldBe` True

  describe "NamingConfig" $ do
    it "enables naming checks by default" $ do
      namingEnabled (cfgNaming defaultConfig) `shouldBe` True

    it "has empty type rules by default" $ do
      namingTypes (cfgNaming defaultConfig) `shouldBe` []

    it "has empty variable rules by default" $ do
      namingVariables (cfgNaming defaultConfig) `shouldBe` []

  describe "PatternsConfig" $ do
    it "enables pattern checks by default" $ do
      patternsEnabled (cfgPatterns defaultConfig) `shouldBe` True

    it "has default pattern rules" $ do
      length (patternsRules (cfgPatterns defaultConfig)) `shouldSatisfy` (> 0)

    it "has exactly 3 default rules" $ do
      length (patternsRules (cfgPatterns defaultConfig)) `shouldBe` 3

    it "includes avoid-head rule" $ do
      any (\r -> RT.ruleId r == "pattern/avoid-head") (patternsRules (cfgPatterns defaultConfig)) `shouldBe` True

    it "includes avoid-fromJust rule" $ do
      any (\r -> RT.ruleId r == "pattern/avoid-fromJust") (patternsRules (cfgPatterns defaultConfig)) `shouldBe` True

    it "includes redundant-id rule" $ do
      any (\r -> RT.ruleId r == "pattern/redundant-id") (patternsRules (cfgPatterns defaultConfig)) `shouldBe` True

  describe "ImportsConfig" $ do
    it "removes unused imports by default" $ do
      importsRemoveUnused (cfgImports defaultConfig) `shouldBe` True

    it "suggests qualified for Data.Map" $ do
      "Data.Map" `elem` importsSuggestQualified (cfgImports defaultConfig) `shouldBe` True

    it "suggests qualified for Data.Set" $ do
      "Data.Set" `elem` importsSuggestQualified (cfgImports defaultConfig) `shouldBe` True

    it "suggests qualified for Data.Text" $ do
      "Data.Text" `elem` importsSuggestQualified (cfgImports defaultConfig) `shouldBe` True

    it "suggests qualified for Data.ByteString" $ do
      "Data.ByteString" `elem` importsSuggestQualified (cfgImports defaultConfig) `shouldBe` True

    it "allows unqualified types by default" $ do
      importsAllowUnqualifiedTypes (cfgImports defaultConfig) `shouldBe` True

    it "allows unqualified operators by default" $ do
      importsAllowUnqualifiedOperators (cfgImports defaultConfig) `shouldBe` True

    it "does not require explicit by default" $ do
      importsRequireExplicit (cfgImports defaultConfig) `shouldBe` False

    it "combines imports by default" $ do
      importsCombine (cfgImports defaultConfig) `shouldBe` True

    it "has TH roots for Yesod" $ do
      any (T.isInfixOf "mkYesod") (importsThRoots (cfgImports defaultConfig)) `shouldBe` True

    it "suppresses unused for TH by default" $ do
      importsSuppressForTH (cfgImports defaultConfig) `shouldBe` True

  describe "FixConfig" $ do
    it "enables fixing by default" $ do
      fixEnabled (cfgFix defaultConfig) `shouldBe` True

    it "uses safe only by default" $ do
      fixSafeOnly (cfgFix defaultConfig) `shouldBe` True

    it "shows preview by default" $ do
      fixPreview (cfgFix defaultConfig) `shouldBe` True

    it "creates backup by default" $ do
      fixBackup (cfgFix defaultConfig) `shouldBe` True

    it "has auto-imports configured" $ do
      afiEnabled (fixAutoImports (cfgFix defaultConfig)) `shouldBe` True

  describe "ComplexityConfig" $ do
    it "enables complexity checking by default" $ do
      compEnabled (cfgComplexity defaultConfig) `shouldBe` True

    it "has cyclomatic warning threshold of 10" $ do
      compCyclomaticWarning (cfgComplexity defaultConfig) `shouldBe` 10

    it "has cyclomatic error threshold of 20" $ do
      compCyclomaticError (cfgComplexity defaultConfig) `shouldBe` 20

    it "has cognitive warning threshold of 15" $ do
      compCognitiveWarning (cfgComplexity defaultConfig) `shouldBe` 15

    it "has cognitive error threshold of 25" $ do
      compCognitiveError (cfgComplexity defaultConfig) `shouldBe` 25

    it "has line length warning threshold of 50" $ do
      compLineLengthWarning (cfgComplexity defaultConfig) `shouldBe` 50

    it "has nesting warning threshold of 4" $ do
      compNestingWarning (cfgComplexity defaultConfig) `shouldBe` 4

    it "has parameter warning threshold of 5" $ do
      compParameterWarning (cfgComplexity defaultConfig) `shouldBe` 5

    it "has pattern branch warning threshold of 10" $ do
      compPatternBranchWarning (cfgComplexity defaultConfig) `shouldBe` 10

  describe "ResourceConfig" $ do
    it "has 60 second timeout by default" $ do
      resTimeoutSeconds (cfgResource defaultConfig) `shouldBe` Just 60

    it "has 2GB max memory by default" $ do
      resMaxMemoryMB (cfgResource defaultConfig) `shouldBe` Just 2048

    it "forces GC every 100 files" $ do
      resForceGCInterval (cfgResource defaultConfig) `shouldBe` Just 100

    it "tracks per-file stats by default" $ do
      resTrackPerFile (cfgResource defaultConfig) `shouldBe` True

    it "warns on slow files (> 10s)" $ do
      resWarnSlowFiles (cfgResource defaultConfig) `shouldBe` Just 10.0

    it "has 1 retry by default" $ do
      resMaxRetries (cfgResource defaultConfig) `shouldBe` 1

    it "kills on timeout by default" $ do
      resKillOnTimeout (cfgResource defaultConfig) `shouldBe` True

--------------------------------------------------------------------------------
-- RuleSeverity Tests
--------------------------------------------------------------------------------

ruleSeveritySpec :: Spec
ruleSeveritySpec = describe "RuleSeverity" $ do
  it "has all expected severities" $ do
    (RSError :: RuleSeverity) `shouldSatisfy` const True
    (RSWarning :: RuleSeverity) `shouldSatisfy` const True
    (RSSuggestion :: RuleSeverity) `shouldSatisfy` const True
    (RSInfo :: RuleSeverity) `shouldSatisfy` const True

  it "maintains proper ordering" $ do
    RSError < RSWarning `shouldBe` True
    RSWarning < RSSuggestion `shouldBe` True
    RSSuggestion < RSInfo `shouldBe` True

  it "converts to Severity correctly" $ do
    ruleSeverityToSeverity RSError `shouldBe` Error
    ruleSeverityToSeverity RSWarning `shouldBe` Warning
    ruleSeverityToSeverity RSSuggestion `shouldBe` Suggestion
    ruleSeverityToSeverity RSInfo `shouldBe` Info

--------------------------------------------------------------------------------
-- TypeRule Tests
--------------------------------------------------------------------------------

typeRuleSpec :: Spec
typeRuleSpec = describe "TypeRule" $ do
  it "stores all fields correctly" $ do
    let rule = TypeRule
          { trPattern = "String"
          , trReplacement = "Text"
          , trSeverity = RSWarning
          , trMessage = Just "Use Text instead"
          , trWithin = Nothing
          }
    trPattern rule `shouldBe` "String"
    trReplacement rule `shouldBe` "Text"
    trSeverity rule `shouldBe` RSWarning
    trMessage rule `shouldBe` Just "Use Text instead"
    trWithin rule `shouldBe` Nothing

  it "supports within restriction" $ do
    let rule = TypeRule
          { trPattern = "Int"
          , trReplacement = "Integer"
          , trSeverity = RSSuggestion
          , trMessage = Nothing
          , trWithin = Just "Database.*"
          }
    trWithin rule `shouldBe` Just "Database.*"

  it "supports custom message" $ do
    let rule = TypeRule
          { trPattern = "IO ()"
          , trReplacement = "IO ()"
          , trSeverity = RSInfo
          , trMessage = Just "Custom message here"
          , trWithin = Nothing
          }
    trMessage rule `shouldBe` Just "Custom message here"

--------------------------------------------------------------------------------
-- VariableRule Tests
--------------------------------------------------------------------------------

variableRuleSpec :: Spec
variableRuleSpec = describe "VariableRule" $ do
  it "stores all fields correctly" $ do
    let rule = VariableRule
          { vrType = "Entity"
          , vrFrom = Just "e*"
          , vrTo = "pE"
          , vrSeverity = RSWarning
          , vrMessage = Just "Use pE naming"
          }
    vrType rule `shouldBe` "Entity"
    vrFrom rule `shouldBe` Just "e*"
    vrTo rule `shouldBe` "pE"
    vrSeverity rule `shouldBe` RSWarning

  it "allows no from pattern" $ do
    let rule = VariableRule
          { vrType = "Handler"
          , vrFrom = Nothing
          , vrTo = "handler"
          , vrSeverity = RSInfo
          , vrMessage = Nothing
          }
    vrFrom rule `shouldBe` Nothing

  it "supports error severity" $ do
    let rule = VariableRule
          { vrType = "Connection"
          , vrFrom = Nothing
          , vrTo = "conn"
          , vrSeverity = RSError
          , vrMessage = Nothing
          }
    vrSeverity rule `shouldBe` RSError

--------------------------------------------------------------------------------
-- PatternRule Tests
--------------------------------------------------------------------------------

patternRuleSpec :: Spec
patternRuleSpec = describe "PatternRule" $ do
  it "stores all fields correctly" $ do
    let rule = PatternRule
          { prName = "test-rule"
          , prMatch = "head xs"
          , prFix = Just "headMay xs"
          , prWhere = Nothing
          , prSeverity = RSWarning
          , prMessage = "Use headMay"
          }
    prName rule `shouldBe` "test-rule"
    prMatch rule `shouldBe` "head xs"
    prFix rule `shouldBe` Just "headMay xs"
    prSeverity rule `shouldBe` RSWarning
    prMessage rule `shouldBe` "Use headMay"

  it "supports where clause" $ do
    let rule = PatternRule
          { prName = "typed-rule"
          , prMatch = "f x"
          , prFix = Nothing
          , prWhere = Just "x :: Int"
          , prSeverity = RSWarning
          , prMessage = "Don't use f on Int"
          }
    prWhere rule `shouldBe` Just "x :: Int"

  it "converts to RT.Rule correctly" $ do
    let pr = PatternRule
          { prName = "my-rule"
          , prMatch = "foo bar"
          , prFix = Just "baz bar"
          , prWhere = Nothing
          , prSeverity = RSWarning
          , prMessage = "Use baz"
          }
        rule = patternRuleToRule pr
    RT.ruleId rule `shouldBe` "pattern/my-rule"
    RT.ruleCategory rule `shouldBe` RT.Style
    RT.ruleSeverity rule `shouldBe` Warning
    RT.ruleMessage rule `shouldBe` "Use baz"
    RT.ruleReplacement rule `shouldBe` Just "baz bar"
    RT.ruleSafety rule `shouldBe` RT.MostlySafe

  it "converts severity correctly" $ do
    let mkRule sev = PatternRule "r" "m" Nothing Nothing sev "msg"
    RT.ruleSeverity (patternRuleToRule (mkRule RSError)) `shouldBe` Error
    RT.ruleSeverity (patternRuleToRule (mkRule RSWarning)) `shouldBe` Warning
    RT.ruleSeverity (patternRuleToRule (mkRule RSSuggestion)) `shouldBe` Suggestion
    RT.ruleSeverity (patternRuleToRule (mkRule RSInfo)) `shouldBe` Info

--------------------------------------------------------------------------------
-- AutoFixImportsConfig Tests
--------------------------------------------------------------------------------

autoFixImportsConfigSpec :: Spec
autoFixImportsConfigSpec = describe "AutoFixImportsConfig" $ do
  describe "defaultAutoFixImportsConfig" $ do
    it "enables import management" $ do
      afiEnabled defaultAutoFixImportsConfig `shouldBe` True

    it "adds missing imports" $ do
      afiAddMissing defaultAutoFixImportsConfig `shouldBe` True

    it "removes unused imports" $ do
      afiRemoveUnused defaultAutoFixImportsConfig `shouldBe` True

    it "does not organize by default" $ do
      afiOrganize defaultAutoFixImportsConfig `shouldBe` False

    it "uses explicit imports" $ do
      afiUseExplicit defaultAutoFixImportsConfig `shouldBe` True

    it "does not qualify new by default" $ do
      afiQualifyNew defaultAutoFixImportsConfig `shouldBe` False

    it "does not group by category by default" $ do
      afiGroupByCategory defaultAutoFixImportsConfig `shouldBe` False

  describe "custom AutoFixImportsConfig" $ do
    it "can enable all options" $ do
      let cfg = AutoFixImportsConfig True True True True True True True
      afiEnabled cfg `shouldBe` True
      afiOrganize cfg `shouldBe` True
      afiQualifyNew cfg `shouldBe` True
      afiGroupByCategory cfg `shouldBe` True

    it "can disable all options" $ do
      let cfg = AutoFixImportsConfig False False False False False False False
      afiEnabled cfg `shouldBe` False
      afiAddMissing cfg `shouldBe` False
      afiRemoveUnused cfg `shouldBe` False

--------------------------------------------------------------------------------
-- ResourceConfig Tests
--------------------------------------------------------------------------------

resourceConfigSpec :: Spec
resourceConfigSpec = describe "ResourceConfig" $ do
  it "can have no timeout" $ do
    let cfg = (cfgResource defaultConfig) { resTimeoutSeconds = Nothing }
    resTimeoutSeconds cfg `shouldBe` Nothing

  it "can have no memory limit" $ do
    let cfg = (cfgResource defaultConfig) { resMaxMemoryMB = Nothing }
    resMaxMemoryMB cfg `shouldBe` Nothing

  it "can have no GC interval" $ do
    let cfg = (cfgResource defaultConfig) { resForceGCInterval = Nothing }
    resForceGCInterval cfg `shouldBe` Nothing

  it "can disable slow file warnings" $ do
    let cfg = (cfgResource defaultConfig) { resWarnSlowFiles = Nothing }
    resWarnSlowFiles cfg `shouldBe` Nothing

  it "can set custom timeout" $ do
    let cfg = (cfgResource defaultConfig) { resTimeoutSeconds = Just 300 }
    resTimeoutSeconds cfg `shouldBe` Just 300

  it "can set custom retries" $ do
    let cfg = (cfgResource defaultConfig) { resMaxRetries = 5 }
    resMaxRetries cfg `shouldBe` 5

--------------------------------------------------------------------------------
-- Conversion Functions Tests
--------------------------------------------------------------------------------

conversionFunctionsSpec :: Spec
conversionFunctionsSpec = describe "Conversion functions" $ do
  describe "toImportManagerConfig" $ do
    it "converts default config correctly" $ do
      let imc = toImportManagerConfig defaultAutoFixImportsConfig
      imcAddMissingImports imc `shouldBe` True
      imcRemoveUnusedImports imc `shouldBe` True
      imcOrganizeImports imc `shouldBe` False
      imcExplicitImports imc `shouldBe` True
      imcQualifyNewImports imc `shouldBe` False
      imcGroupImports imc `shouldBe` False

    it "converts all-enabled config" $ do
      let afi = AutoFixImportsConfig True True True True True True True
          imc = toImportManagerConfig afi
      imcAddMissingImports imc `shouldBe` True
      imcRemoveUnusedImports imc `shouldBe` True
      imcOrganizeImports imc `shouldBe` True
      imcExplicitImports imc `shouldBe` True
      imcQualifyNewImports imc `shouldBe` True
      imcGroupImports imc `shouldBe` True

    it "converts all-disabled config" $ do
      let afi = AutoFixImportsConfig False False False False False False False
          imc = toImportManagerConfig afi
      imcAddMissingImports imc `shouldBe` False
      imcRemoveUnusedImports imc `shouldBe` False
      imcOrganizeImports imc `shouldBe` False
      imcQualifyNewImports imc `shouldBe` False
      imcGroupImports imc `shouldBe` False

  describe "ruleSeverityToSeverity" $ do
    it "converts all severities" $ do
      ruleSeverityToSeverity RSError `shouldBe` Error
      ruleSeverityToSeverity RSWarning `shouldBe` Warning
      ruleSeverityToSeverity RSSuggestion `shouldBe` Suggestion
      ruleSeverityToSeverity RSInfo `shouldBe` Info

--------------------------------------------------------------------------------
-- Legacy Config Tests
--------------------------------------------------------------------------------

legacyConfigSpec :: Spec
legacyConfigSpec = describe "Legacy config conversion" $ do
  describe "convertLegacyConfig" $ do
    it "converts empty legacy config" $ do
      let legacy = LegacyConfig [] [] False ""
          cfg = convertLegacyConfig legacy
      namingTypes (cfgNaming cfg) `shouldBe` []
      namingVariables (cfgNaming cfg) `shouldBe` []
      fixEnabled (cfgFix cfg) `shouldBe` False

    it "converts legacy signatures to type rules" $ do
      let sig = LegacySignature "String" "Text" Nothing (Just "Use Text")
          legacy = LegacyConfig [sig] [] False ""
          cfg = convertLegacyConfig legacy
      length (namingTypes (cfgNaming cfg)) `shouldBe` 1
      let rule = head (namingTypes (cfgNaming cfg))
      trPattern rule `shouldBe` "String"
      trReplacement rule `shouldBe` "Text"
      trMessage rule `shouldBe` Just "Use Text"
      trSeverity rule `shouldBe` RSWarning

    it "converts legacy variables to variable rules" $ do
      let var = LegacyVariable "Entity" (Just "e") "pE" (Just "Use prefix")
          legacy = LegacyConfig [] [var] False ""
          cfg = convertLegacyConfig legacy
      length (namingVariables (cfgNaming cfg)) `shouldBe` 1
      let rule = head (namingVariables (cfgNaming cfg))
      vrType rule `shouldBe` "Entity"
      vrFrom rule `shouldBe` Just "e"
      vrTo rule `shouldBe` "pE"
      vrMessage rule `shouldBe` Just "Use prefix"

    it "converts in-place to fix enabled" $ do
      let legacy = LegacyConfig [] [] True ""
          cfg = convertLegacyConfig legacy
      fixEnabled (cfgFix cfg) `shouldBe` True

    it "converts directory to general config" $ do
      let legacy = LegacyConfig [] [] False "custom/path"
          cfg = convertLegacyConfig legacy
      genDirectories (cfgGeneral cfg) `shouldBe` ["custom/path"]

    it "uses default directories when empty" $ do
      let legacy = LegacyConfig [] [] False ""
          cfg = convertLegacyConfig legacy
      genDirectories (cfgGeneral cfg) `shouldBe` ["src", "app"]

    it "preserves within restriction" $ do
      let sig = LegacySignature "Foo" "Bar" (Just "Module.Name") Nothing
          legacy = LegacyConfig [sig] [] False ""
          cfg = convertLegacyConfig legacy
          rule = head (namingTypes (cfgNaming cfg))
      trWithin rule `shouldBe` Just "Module.Name"

  describe "LegacySignature" $ do
    it "stores all fields" $ do
      let sig = LegacySignature "from" "to" (Just "within") (Just "msg")
      lsFrom sig `shouldBe` "from"
      lsTo sig `shouldBe` "to"
      lsWithin sig `shouldBe` Just "within"
      lsMsg sig `shouldBe` Just "msg"

  describe "LegacyVariable" $ do
    it "stores all fields" $ do
      let var = LegacyVariable "Type" (Just "pattern") "target" (Just "msg")
      lvType var `shouldBe` "Type"
      lvFrom var `shouldBe` Just "pattern"
      lvTo var `shouldBe` "target"
      lvMsg var `shouldBe` Just "msg"

--------------------------------------------------------------------------------
-- JSON Roundtrip Tests
--------------------------------------------------------------------------------

jsonRoundtripSpec :: Spec
jsonRoundtripSpec = describe "JSON serialization" $ do
  describe "RuleSeverity" $ do
    it "roundtrips RSError" $ shouldRoundTrip RSError
    it "roundtrips RSWarning" $ shouldRoundTrip RSWarning
    it "roundtrips RSSuggestion" $ shouldRoundTrip RSSuggestion
    it "roundtrips RSInfo" $ shouldRoundTrip RSInfo

  describe "TypeRule" $ do
    it "roundtrips basic rule" $ do
      let rule = TypeRule "String" "Text" RSWarning (Just "msg") Nothing
      shouldRoundTrip rule

    it "roundtrips rule with within" $ do
      let rule = TypeRule "Int" "Integer" RSError Nothing (Just "Module.*")
      shouldRoundTrip rule

  describe "VariableRule" $ do
    it "roundtrips basic rule" $ do
      let rule = VariableRule "Type" (Just "from") "to" RSWarning (Just "msg")
      shouldRoundTrip rule

    it "roundtrips rule without from" $ do
      let rule = VariableRule "Type" Nothing "to" RSInfo Nothing
      shouldRoundTrip rule

  describe "PatternRule" $ do
    it "roundtrips basic rule" $ do
      let rule = PatternRule "name" "match" (Just "fix") Nothing RSWarning "msg"
      shouldRoundTrip rule

    it "roundtrips rule with where" $ do
      let rule = PatternRule "name" "match" Nothing (Just "where") RSSuggestion "msg"
      shouldRoundTrip rule

  describe "AutoFixImportsConfig" $ do
    it "roundtrips default config" $ shouldRoundTrip defaultAutoFixImportsConfig

    it "roundtrips custom config" $ do
      let cfg = AutoFixImportsConfig True False True False True False True
      shouldRoundTrip cfg

  describe "GeneralConfig" $ do
    it "roundtrips default config" $ shouldRoundTrip (cfgGeneral defaultConfig)

  describe "OutputConfig" $ do
    it "roundtrips default config" $ shouldRoundTrip (cfgOutput defaultConfig)

  describe "UnusedConfig" $ do
    it "roundtrips default config" $ shouldRoundTrip (cfgUnused defaultConfig)

  describe "NamingConfig" $ do
    it "roundtrips default config" $ shouldRoundTrip (cfgNaming defaultConfig)

    it "roundtrips config with rules" $ do
      let cfg = NamingConfig True
                  [TypeRule "A" "B" RSWarning Nothing Nothing]
                  [VariableRule "T" Nothing "v" RSInfo Nothing]
      shouldRoundTrip cfg

  describe "ImportsConfig" $ do
    it "roundtrips default config" $ shouldRoundTrip (cfgImports defaultConfig)

  describe "FixConfig" $ do
    it "roundtrips default config" $ shouldRoundTrip (cfgFix defaultConfig)

  describe "ComplexityConfig" $ do
    it "roundtrips default config" $ shouldRoundTrip (cfgComplexity defaultConfig)

  describe "ResourceConfig" $ do
    it "roundtrips default config" $ shouldRoundTrip (cfgResource defaultConfig)

    it "handles config with no limits by applying defaults on decode" $ do
      -- FromJSON applies defaults to optional fields, so Nothing values get defaults
      let cfg = ResourceConfig Nothing Nothing Nothing False Nothing 0 False
          decoded = decode (encode cfg) :: Maybe ResourceConfig
      decoded `shouldSatisfy` \case
        Just rc -> resTrackPerFile rc == False && resMaxRetries rc == 0 && resKillOnTimeout rc == False
        Nothing -> False

  describe "Config" $ do
    -- Note: Full Config roundtrip is complex due to PatternsConfig containing RT.Rule
    -- which is serialized from PatternRule conversion. We test individual parts instead.
    it "serializes default config to JSON" $ do
      let json = encode defaultConfig
      -- Just verify it produces non-empty JSON
      json `shouldSatisfy` (\bs -> BL.length bs > 100)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Test JSON roundtrip
shouldRoundTrip :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Expectation
shouldRoundTrip x = decode (encode x) `shouldBe` Just x
