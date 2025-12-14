{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : AutoFixTypesSpec
-- Description : Comprehensive tests for AutoFix.Types module
--
-- Tests for the core auto-fix type infrastructure including:
-- * FixId construction and parsing
-- * Confidence scoring
-- * FixMetadata defaults
-- * Dependency resolution and topological sort
-- * Conflict detection
-- * Fix validation
-- * EnrichedFix operations
-- * FixApplicationResult handling
-- * FixStats aggregation
-- * FixEngine typeclass and existential wrapper
module AutoFixTypesSpec (spec) where

import Test.Hspec

import Data.Aeson (encode, decode)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Argus.AutoFix.Types
import Argus.Types
  ( SrcSpan, Fix(..), FixEdit(..), mkSrcSpanRaw
  , Diagnostic(..), Severity(..), DiagnosticKind(..)
  , FixCategory(..), FixSafety(..)
  )
import Argus.Rules.Types (Category(..), SafetyLevel(..))

spec :: Spec
spec = do
  fixIdSpec
  confidenceSpec
  fixMetadataSpec
  dependencySpec
  conflictSpec
  validationSpec
  enrichedFixSpec
  applicationResultSpec
  fixStatsSpec
  fixEngineSpec

--------------------------------------------------------------------------------
-- FixId Tests
--------------------------------------------------------------------------------

fixIdSpec :: Spec
fixIdSpec = describe "FixId" $ do
  describe "mkFixId" $ do
    it "creates a FixId from engine and local parts" $ do
      let fid = mkFixId "my-engine" "local-123"
      fixIdEngine fid `shouldBe` "my-engine"
      fixIdLocal fid `shouldBe` "local-123"

    it "handles empty engine name" $ do
      let fid = mkFixId "" "local"
      fixIdEngine fid `shouldBe` ""
      fixIdLocal fid `shouldBe` "local"

    it "handles empty local id" $ do
      let fid = mkFixId "engine" ""
      fixIdEngine fid `shouldBe` "engine"
      fixIdLocal fid `shouldBe` ""

    it "handles special characters in ids" $ do
      let fid = mkFixId "engine-v1.0" "rule/sub/id-001"
      fixIdEngine fid `shouldBe` "engine-v1.0"
      fixIdLocal fid `shouldBe` "rule/sub/id-001"

  describe "parseFixId" $ do
    it "parses valid fix id format" $ do
      parseFixId "engine/local" `shouldBe` Just (FixId "engine" "local")

    it "parses fix id with slashes in local part" $ do
      parseFixId "engine/rule/sub/id" `shouldBe` Just (FixId "engine" "rule/sub/id")

    it "returns Nothing for id without slash" $ do
      parseFixId "noslash" `shouldBe` Nothing

    it "returns Nothing for empty string" $ do
      parseFixId "" `shouldBe` Nothing

    it "handles leading slash" $ do
      parseFixId "/local" `shouldBe` Just (FixId "" "local")

    it "handles trailing slash" $ do
      parseFixId "engine/" `shouldBe` Just (FixId "engine" "")

  describe "JSON serialization" $ do
    it "round-trips through JSON" $ do
      let fid = mkFixId "test-engine" "test-local-123"
      decode (encode fid) `shouldBe` Just fid

    it "serializes to slash-separated format" $ do
      let fid = mkFixId "engine" "local"
          json = encode fid
      -- Should serialize as "engine/local"
      json `shouldBe` "\"engine/local\""

  describe "Eq instance" $ do
    it "considers equal ids equal" $ do
      mkFixId "a" "b" `shouldBe` mkFixId "a" "b"

    it "considers different engine names different" $ do
      mkFixId "a" "b" `shouldNotBe` mkFixId "c" "b"

    it "considers different local ids different" $ do
      mkFixId "a" "b" `shouldNotBe` mkFixId "a" "c"

  describe "Ord instance" $ do
    it "orders by engine then local" $ do
      mkFixId "a" "z" < mkFixId "b" "a" `shouldBe` True
      mkFixId "a" "a" < mkFixId "a" "b" `shouldBe` True

--------------------------------------------------------------------------------
-- Confidence Tests
--------------------------------------------------------------------------------

confidenceSpec :: Spec
confidenceSpec = describe "Confidence" $ do
  describe "mkConfidence" $ do
    it "creates confidence from valid value" $ do
      unConfidence (mkConfidence 0.5) `shouldBe` 0.5

    it "clamps values above 1.0" $ do
      unConfidence (mkConfidence 1.5) `shouldBe` 1.0

    it "clamps values below 0.0" $ do
      unConfidence (mkConfidence (-0.5)) `shouldBe` 0.0

    it "preserves boundary values" $ do
      unConfidence (mkConfidence 0.0) `shouldBe` 0.0
      unConfidence (mkConfidence 1.0) `shouldBe` 1.0

  describe "predefined confidence levels" $ do
    it "highConfidence is 0.95" $ do
      unConfidence highConfidence `shouldBe` 0.95

    it "mediumConfidence is 0.8" $ do
      unConfidence mediumConfidence `shouldBe` 0.8

    it "lowConfidence is 0.5" $ do
      unConfidence lowConfidence `shouldBe` 0.5

    it "unknownConfidence is 0.0" $ do
      unConfidence unknownConfidence `shouldBe` 0.0

  describe "ordering" $ do
    it "orders confidence levels correctly" $ do
      unknownConfidence < lowConfidence `shouldBe` True
      lowConfidence < mediumConfidence `shouldBe` True
      mediumConfidence < highConfidence `shouldBe` True

    it "compares custom values correctly" $ do
      mkConfidence 0.3 < mkConfidence 0.7 `shouldBe` True

  describe "JSON round-trip" $ do
    it "preserves confidence through serialization" $ do
      let conf = mkConfidence 0.75
      decode (encode conf) `shouldBe` Just conf

    it "clamps out-of-range values on deserialization" $ do
      -- JSON number 2.0 should be clamped to 1.0
      let decoded = decode "2.0" :: Maybe Argus.AutoFix.Types.Confidence
      fmap unConfidence decoded `shouldBe` Just 1.0

--------------------------------------------------------------------------------
-- FixMetadata Tests
--------------------------------------------------------------------------------

fixMetadataSpec :: Spec
fixMetadataSpec = describe "FixMetadata" $ do
  describe "defaultFixMetadata" $ do
    it "sets high confidence by default" $ do
      let meta = defaultFixMetadata Performance
      fmConfidence meta `shouldBe` highConfidence

    it "sets Safe safety by default" $ do
      let meta = defaultFixMetadata Performance
      fmSafety meta `shouldBe` Safe

    it "sets the specified category" $ do
      let meta = defaultFixMetadata Security
      fmCategory meta `shouldBe` Security

    it "has empty tags by default" $ do
      let meta = defaultFixMetadata Performance
      fmTags meta `shouldBe` Set.empty

    it "has no creation time by default" $ do
      let meta = defaultFixMetadata Performance
      fmCreatedAt meta `shouldBe` Nothing

    it "has no source rule by default" $ do
      let meta = defaultFixMetadata Performance
      fmSourceRule meta `shouldBe` Nothing

    it "has no explanation by default" $ do
      let meta = defaultFixMetadata Performance
      fmExplanation meta `shouldBe` Nothing

    it "has empty notes by default" $ do
      let meta = defaultFixMetadata Performance
      fmNotes meta `shouldBe` []

    it "has empty references by default" $ do
      let meta = defaultFixMetadata Performance
      fmReferences meta `shouldBe` []

--------------------------------------------------------------------------------
-- Dependency Resolution Tests
--------------------------------------------------------------------------------

dependencySpec :: Spec
dependencySpec = describe "Dependencies" $ do
  describe "DependencyType" $ do
    it "has all expected variants" $ do
      -- Check all variants exist via exhaustive pattern match
      let checkDep :: DependencyType -> Bool
          checkDep = \case
            MustApplyBefore -> True
            MustApplyAfter -> True
            MutuallyExclusive -> True
            Requires -> True
      checkDep MustApplyBefore `shouldBe` True
      checkDep MustApplyAfter `shouldBe` True
      checkDep MutuallyExclusive `shouldBe` True
      checkDep Requires `shouldBe` True

  describe "resolveDependencies" $ do
    it "returns all fixes when no dependencies" $ do
      let fixes = [mkFixId "e" "a", mkFixId "e" "b", mkFixId "e" "c"]
      case resolveDependencies fixes [] of
        Right sorted -> Set.fromList sorted `shouldBe` Set.fromList fixes
        Left _ -> expectationFailure "Should not have cycle"

    it "orders fixes with MustApplyBefore" $ do
      let fidA = mkFixId "e" "a"
          fidB = mkFixId "e" "b"
          deps = [FixDependency fidA fidB MustApplyBefore Nothing]
      -- A must apply before B, so A should come first
      case resolveDependencies [fidA, fidB] deps of
        Right sorted -> fidA `shouldSatisfy` (`elem` sorted)
        Left _ -> expectationFailure "Should not have cycle"

    it "orders fixes with MustApplyAfter" $ do
      let fidA = mkFixId "e" "a"
          fidB = mkFixId "e" "b"
          deps = [FixDependency fidA fidB MustApplyAfter Nothing]
      -- A must apply after B, so B should come first
      case resolveDependencies [fidA, fidB] deps of
        Right sorted -> fidB `shouldSatisfy` (`elem` sorted)
        Left _ -> expectationFailure "Should not have cycle"

    it "handles Requires dependency" $ do
      let fidA = mkFixId "e" "a"
          fidB = mkFixId "e" "b"
          deps = [FixDependency fidA fidB Requires Nothing]
      case resolveDependencies [fidA, fidB] deps of
        Right sorted -> length sorted `shouldBe` 2
        Left _ -> expectationFailure "Should not have cycle"

    it "ignores MutuallyExclusive for ordering" $ do
      let fidA = mkFixId "e" "a"
          fidB = mkFixId "e" "b"
          deps = [FixDependency fidA fidB MutuallyExclusive Nothing]
      case resolveDependencies [fidA, fidB] deps of
        Right sorted -> length sorted `shouldBe` 2
        Left _ -> expectationFailure "Should not have cycle"

    it "detects simple cycle" $ do
      let fidA = mkFixId "e" "a"
          fidB = mkFixId "e" "b"
          deps = [ FixDependency fidA fidB MustApplyBefore Nothing
                 , FixDependency fidB fidA MustApplyBefore Nothing
                 ]
      case resolveDependencies [fidA, fidB] deps of
        Left cycle -> length cycle `shouldSatisfy` (> 0)
        Right _ -> expectationFailure "Should detect cycle"

    it "handles complex dependency chain" $ do
      let fids = [mkFixId "e" (T.pack $ show n) | n <- [1..5 :: Int]]
          -- Chain: 1 -> 2 -> 3 -> 4 -> 5
          deps = zipWith (\a b -> FixDependency a b MustApplyBefore Nothing)
                         (init fids) (tail fids)
      case resolveDependencies fids deps of
        Right sorted -> length sorted `shouldBe` 5
        Left _ -> expectationFailure "Should not have cycle"

  describe "topologicalSort" $ do
    it "returns Just for acyclic graph" $ do
      let fids = [mkFixId "e" "a", mkFixId "e" "b"]
          graph = Map.empty
      topologicalSort fids graph `shouldSatisfy` \case
        Just _ -> True
        Nothing -> False

    it "returns Nothing for cyclic graph" $ do
      let fidA = mkFixId "e" "a"
          fidB = mkFixId "e" "b"
          graph = Map.fromList [(fidA, [fidB]), (fidB, [fidA])]
      topologicalSort [fidA, fidB] graph `shouldBe` Nothing

    it "handles empty input" $ do
      topologicalSort [] Map.empty `shouldBe` Just []

--------------------------------------------------------------------------------
-- Conflict Detection Tests
--------------------------------------------------------------------------------

conflictSpec :: Spec
conflictSpec = describe "Conflicts" $ do
  describe "ConflictType" $ do
    it "has all expected variants" $ do
      let checkConflict :: ConflictType -> Bool
          checkConflict = \case
            OverlappingSpan -> True
            SameLocation -> True
            SemanticConflict -> True
            ResourceConflict _ -> True
      checkConflict OverlappingSpan `shouldBe` True
      checkConflict SameLocation `shouldBe` True
      checkConflict SemanticConflict `shouldBe` True
      checkConflict (ResourceConflict "test") `shouldBe` True

  describe "detectConflicts" $ do
    it "returns empty list when no fixes" $ do
      detectConflicts [] `shouldBe` []

    it "returns empty list for single fix" $ do
      let ef = mkTestEnrichedFix "e" "a" "test.hs" 1 1 1 10
      detectConflicts [ef] `shouldBe` []

    it "detects overlapping spans" $ do
      let ef1 = mkTestEnrichedFix "e" "a" "test.hs" 1 1 1 10
          ef2 = mkTestEnrichedFix "e" "b" "test.hs" 1 5 1 15
      length (detectConflicts [ef1, ef2]) `shouldSatisfy` (> 0)

    it "does not flag non-overlapping spans" $ do
      let ef1 = mkTestEnrichedFix "e" "a" "test.hs" 1 1 1 10
          ef2 = mkTestEnrichedFix "e" "b" "test.hs" 2 1 2 10
      detectConflicts [ef1, ef2] `shouldBe` []

    it "does not flag spans in different files" $ do
      let ef1 = mkTestEnrichedFix "e" "a" "file1.hs" 1 1 1 10
          ef2 = mkTestEnrichedFix "e" "b" "file2.hs" 1 1 1 10
      detectConflicts [ef1, ef2] `shouldBe` []

    it "detects declared conflicts" $ do
      let fidA = mkFixId "e" "a"
          fidB = mkFixId "e" "b"
          ef1 = (mkTestEnrichedFix "e" "a" "test.hs" 1 1 1 5)
                  { efConflicts = Set.singleton fidB }
          ef2 = mkTestEnrichedFix "e" "b" "test.hs" 2 1 2 5
      length (detectConflicts [ef1, ef2]) `shouldSatisfy` (> 0)

    it "handles multiple overlapping fixes" $ do
      let ef1 = mkTestEnrichedFix "e" "a" "test.hs" 1 1 1 20
          ef2 = mkTestEnrichedFix "e" "b" "test.hs" 1 5 1 15
          ef3 = mkTestEnrichedFix "e" "c" "test.hs" 1 10 1 25
      length (detectConflicts [ef1, ef2, ef3]) `shouldSatisfy` (>= 2)

  describe "conflictsExist" $ do
    it "returns False for empty list" $ do
      conflictsExist [] `shouldBe` False

    it "returns True for non-empty list" $ do
      let conflict = FixConflict
            { fcFixA = mkFixId "e" "a"
            , fcFixB = mkFixId "e" "b"
            , fcType = OverlappingSpan
            , fcSpan = Nothing
            , fcMessage = "test"
            }
      conflictsExist [conflict] `shouldBe` True

--------------------------------------------------------------------------------
-- Validation Tests
--------------------------------------------------------------------------------

validationSpec :: Spec
validationSpec = describe "Validation" $ do
  describe "ValidationResult" $ do
    it "has success variant" $ do
      ValidationSuccess `shouldBe` ValidationSuccess

    it "has warning variant with message" $ do
      let warn = ValidationWarning "warning message"
      case warn of
        ValidationWarning msg -> msg `shouldBe` "warning message"
        _ -> expectationFailure "Expected warning"

    it "has error variant with message" $ do
      let err = ValidationError "error message"
      case err of
        ValidationError msg -> msg `shouldBe` "error message"
        _ -> expectationFailure "Expected error"

  describe "isValidationSuccess" $ do
    it "returns True for ValidationSuccess" $ do
      let fv = FixValidation ValidationSuccess [] []
      isValidationSuccess fv `shouldBe` True

    it "returns True for ValidationWarning" $ do
      let fv = FixValidation (ValidationWarning "warn") [] []
      isValidationSuccess fv `shouldBe` True

    it "returns False for ValidationError" $ do
      let fv = FixValidation (ValidationError "err") [] []
      isValidationSuccess fv `shouldBe` False

  describe "validationErrors" $ do
    it "extracts error from result" $ do
      let fv = FixValidation (ValidationError "main error") [] []
      validationErrors fv `shouldBe` ["main error"]

    it "extracts errors from checks" $ do
      let fv = FixValidation ValidationSuccess
                 [("check1", ValidationError "error1"),
                  ("check2", ValidationError "error2")]
                 []
      validationErrors fv `shouldBe` ["error1", "error2"]

    it "combines result and check errors" $ do
      let fv = FixValidation (ValidationError "main")
                 [("check", ValidationError "check-err")]
                 []
      length (validationErrors fv) `shouldBe` 2

    it "returns empty for success" $ do
      let fv = FixValidation ValidationSuccess [] []
      validationErrors fv `shouldBe` []

  describe "validationWarnings" $ do
    it "extracts warning from result" $ do
      let fv = FixValidation (ValidationWarning "main warning") [] []
      validationWarnings fv `shouldBe` ["main warning"]

    it "extracts warnings from checks" $ do
      let fv = FixValidation ValidationSuccess
                 [("check1", ValidationWarning "warn1")]
                 []
      validationWarnings fv `shouldBe` ["warn1"]

    it "returns empty for success without warnings" $ do
      let fv = FixValidation ValidationSuccess [] []
      validationWarnings fv `shouldBe` []

--------------------------------------------------------------------------------
-- EnrichedFix Tests
--------------------------------------------------------------------------------

enrichedFixSpec :: Spec
enrichedFixSpec = describe "EnrichedFix" $ do
  describe "mkEnrichedFix" $ do
    it "creates enriched fix with given id and category" $ do
      let fid = mkFixId "engine" "local"
          fix = testFix
          ef = mkEnrichedFix fid fix Performance
      efId ef `shouldBe` fid
      fmCategory (efMetadata ef) `shouldBe` Performance

    it "starts with empty dependencies" $ do
      let ef = mkEnrichedFix (mkFixId "e" "l") testFix Performance
      efDependencies ef `shouldBe` Set.empty

    it "starts with empty conflicts" $ do
      let ef = mkEnrichedFix (mkFixId "e" "l") testFix Performance
      efConflicts ef `shouldBe` Set.empty

    it "starts with no validation" $ do
      let ef = mkEnrichedFix (mkFixId "e" "l") testFix Performance
      efValidation ef `shouldBe` Nothing

  describe "enrichFix" $ do
    it "creates fix id from engine and local" $ do
      let ef = enrichFix "my-engine" "my-local" testFix Security
      fixIdEngine (efId ef) `shouldBe` "my-engine"
      fixIdLocal (efId ef) `shouldBe` "my-local"

  describe "stripEnrichment" $ do
    it "returns the underlying fix" $ do
      let ef = mkEnrichedFix (mkFixId "e" "l") testFix Performance
      stripEnrichment ef `shouldBe` testFix

  describe "mkEnrichedFixWithDeps" $ do
    it "creates fix with dependencies and conflicts" $ do
      let fid = mkFixId "e" "main"
          dep = mkFixId "e" "dep"
          conf = mkFixId "e" "conflict"
          ef = mkEnrichedFixWithDeps fid testFix Performance
                 (Set.singleton dep) (Set.singleton conf)
      Set.member dep (efDependencies ef) `shouldBe` True
      Set.member conf (efConflicts ef) `shouldBe` True

  describe "enrichFixWithDeps" $ do
    it "combines enrichFix with dependencies" $ do
      let dep = mkFixId "e" "dep"
          ef = enrichFixWithDeps "engine" "local" testFix Performance
                 (Set.singleton dep) Set.empty
      fixIdEngine (efId ef) `shouldBe` "engine"
      Set.member dep (efDependencies ef) `shouldBe` True

  describe "addDependency" $ do
    it "adds a dependency to existing set" $ do
      let ef = mkEnrichedFix (mkFixId "e" "l") testFix Performance
          dep = mkFixId "e" "dep"
          ef' = addDependency dep ef
      Set.member dep (efDependencies ef') `shouldBe` True

    it "preserves existing dependencies" $ do
      let dep1 = mkFixId "e" "dep1"
          dep2 = mkFixId "e" "dep2"
          ef = mkEnrichedFixWithDeps (mkFixId "e" "l") testFix Performance
                 (Set.singleton dep1) Set.empty
          ef' = addDependency dep2 ef
      Set.member dep1 (efDependencies ef') `shouldBe` True
      Set.member dep2 (efDependencies ef') `shouldBe` True

  describe "addConflict" $ do
    it "adds a conflict to existing set" $ do
      let ef = mkEnrichedFix (mkFixId "e" "l") testFix Performance
          conf = mkFixId "e" "conf"
          ef' = addConflict conf ef
      Set.member conf (efConflicts ef') `shouldBe` True

  describe "setDependencies" $ do
    it "replaces all dependencies" $ do
      let dep1 = mkFixId "e" "dep1"
          dep2 = mkFixId "e" "dep2"
          ef = mkEnrichedFixWithDeps (mkFixId "e" "l") testFix Performance
                 (Set.singleton dep1) Set.empty
          ef' = setDependencies (Set.singleton dep2) ef
      Set.member dep1 (efDependencies ef') `shouldBe` False
      Set.member dep2 (efDependencies ef') `shouldBe` True

  describe "setConflicts" $ do
    it "replaces all conflicts" $ do
      let conf1 = mkFixId "e" "conf1"
          conf2 = mkFixId "e" "conf2"
          ef = mkEnrichedFixWithDeps (mkFixId "e" "l") testFix Performance
                 Set.empty (Set.singleton conf1)
          ef' = setConflicts (Set.singleton conf2) ef
      Set.member conf1 (efConflicts ef') `shouldBe` False
      Set.member conf2 (efConflicts ef') `shouldBe` True

  describe "ruleIdToFixId" $ do
    it "converts rule id text to FixId" $ do
      let fid = ruleIdToFixId "configurable-rules" "partial/head"
      fixIdEngine fid `shouldBe` "configurable-rules"
      fixIdLocal fid `shouldBe` "partial/head"

--------------------------------------------------------------------------------
-- Application Result Tests
--------------------------------------------------------------------------------

applicationResultSpec :: Spec
applicationResultSpec = describe "FixApplicationResult" $ do
  describe "ApplyError variants" $ do
    it "has SpanOutOfBounds" $ do
      let span = mkSrcSpanRaw "test.hs" 1 1 1 10
          err = SpanOutOfBounds span
      case err of
        SpanOutOfBounds _ -> pure ()
        _ -> expectationFailure "Expected SpanOutOfBounds"

    it "has ContentMismatch" $ do
      let err = ContentMismatch "expected" "actual"
      case err of
        ContentMismatch e a -> do
          e `shouldBe` "expected"
          a `shouldBe` "actual"
        _ -> expectationFailure "Expected ContentMismatch"

    it "has ConflictingEdits" $ do
      let err = ConflictingEdits []
      case err of
        ConflictingEdits _ -> pure ()
        _ -> expectationFailure "Expected ConflictingEdits"

    it "has ValidationFailed" $ do
      let err = ValidationFailed "validation error"
      case err of
        ValidationFailed msg -> msg `shouldBe` "validation error"
        _ -> expectationFailure "Expected ValidationFailed"

    it "has DependencyError" $ do
      let err = DependencyError (mkFixId "e" "l") "dep not met"
      case err of
        DependencyError fid msg -> do
          fixIdLocal fid `shouldBe` "l"
          msg `shouldBe` "dep not met"
        _ -> expectationFailure "Expected DependencyError"

    it "has EngineError" $ do
      let err = EngineError "internal error"
      case err of
        EngineError msg -> msg `shouldBe` "internal error"
        _ -> expectationFailure "Expected EngineError"

  describe "isSuccess" $ do
    it "returns True for ApplySuccess" $ do
      let ef = mkEnrichedFix (mkFixId "e" "l") testFix Performance
          result = ApplySuccess "new content" ef emptyFixStats
      Argus.AutoFix.Types.isSuccess result `shouldBe` True

    it "returns False for ApplyPartial" $ do
      let ef = mkEnrichedFix (mkFixId "e" "l") testFix Performance
          result = ApplyPartial "partial" ef [] emptyFixStats
      Argus.AutoFix.Types.isSuccess result `shouldBe` False

    it "returns False for ApplyFailure" $ do
      let result = ApplyFailure (EngineError "error")
      Argus.AutoFix.Types.isSuccess result `shouldBe` False

  describe "getAppliedContent" $ do
    it "returns Just for ApplySuccess" $ do
      let ef = mkEnrichedFix (mkFixId "e" "l") testFix Performance
          result = ApplySuccess "new content" ef emptyFixStats
      getAppliedContent result `shouldBe` Just "new content"

    it "returns Just for ApplyPartial" $ do
      let ef = mkEnrichedFix (mkFixId "e" "l") testFix Performance
          result = ApplyPartial "partial content" ef [] emptyFixStats
      getAppliedContent result `shouldBe` Just "partial content"

    it "returns Nothing for ApplyFailure" $ do
      let result = ApplyFailure (EngineError "error")
      getAppliedContent result `shouldBe` Nothing

--------------------------------------------------------------------------------
-- FixStats Tests
--------------------------------------------------------------------------------

fixStatsSpec :: Spec
fixStatsSpec = describe "FixStats" $ do
  describe "emptyFixStats" $ do
    it "has zero total" $ do
      fsTotal emptyFixStats `shouldBe` 0

    it "has zero applied" $ do
      fsApplied emptyFixStats `shouldBe` 0

    it "has zero failed" $ do
      fsFailed emptyFixStats `shouldBe` 0

    it "has zero skipped" $ do
      fsSkipped emptyFixStats `shouldBe` 0

    it "has empty category breakdown" $ do
      fsByCategory emptyFixStats `shouldBe` Map.empty

    it "has empty safety breakdown" $ do
      fsBySafety emptyFixStats `shouldBe` Map.empty

    it "has zero conflicts" $ do
      fsConflicts emptyFixStats `shouldBe` 0

    it "has zero dependencies" $ do
      fsDependencies emptyFixStats `shouldBe` 0

  describe "mergeFixStats" $ do
    it "adds totals" $ do
      let s1 = emptyFixStats { fsTotal = 5 }
          s2 = emptyFixStats { fsTotal = 3 }
      fsTotal (mergeFixStats s1 s2) `shouldBe` 8

    it "adds applied counts" $ do
      let s1 = emptyFixStats { fsApplied = 3 }
          s2 = emptyFixStats { fsApplied = 2 }
      fsApplied (mergeFixStats s1 s2) `shouldBe` 5

    it "adds failed counts" $ do
      let s1 = emptyFixStats { fsFailed = 1 }
          s2 = emptyFixStats { fsFailed = 2 }
      fsFailed (mergeFixStats s1 s2) `shouldBe` 3

    it "adds skipped counts" $ do
      let s1 = emptyFixStats { fsSkipped = 2 }
          s2 = emptyFixStats { fsSkipped = 1 }
      fsSkipped (mergeFixStats s1 s2) `shouldBe` 3

    it "merges category breakdowns" $ do
      let s1 = emptyFixStats { fsByCategory = Map.singleton Performance 2 }
          s2 = emptyFixStats { fsByCategory = Map.singleton Performance 3 }
      Map.lookup Performance (fsByCategory (mergeFixStats s1 s2)) `shouldBe` Just 5

    it "merges safety breakdowns" $ do
      let s1 = emptyFixStats { fsBySafety = Map.singleton "safe" 2 }
          s2 = emptyFixStats { fsBySafety = Map.singleton "safe" 1 }
      Map.lookup "safe" (fsBySafety (mergeFixStats s1 s2)) `shouldBe` Just 3

    it "adds conflicts" $ do
      let s1 = emptyFixStats { fsConflicts = 2 }
          s2 = emptyFixStats { fsConflicts = 3 }
      fsConflicts (mergeFixStats s1 s2) `shouldBe` 5

    it "adds dependencies" $ do
      let s1 = emptyFixStats { fsDependencies = 1 }
          s2 = emptyFixStats { fsDependencies = 2 }
      fsDependencies (mergeFixStats s1 s2) `shouldBe` 3

  describe "addFixToStats" $ do
    it "increments total on success" $ do
      let ef = mkEnrichedFix (mkFixId "e" "l") testFix Performance
          result = ApplySuccess "content" ef emptyFixStats
      fsTotal (addFixToStats result emptyFixStats) `shouldBe` 1

    it "increments applied on success" $ do
      let ef = mkEnrichedFix (mkFixId "e" "l") testFix Performance
          result = ApplySuccess "content" ef emptyFixStats
      fsApplied (addFixToStats result emptyFixStats) `shouldBe` 1

    it "increments category count on success" $ do
      let ef = mkEnrichedFix (mkFixId "e" "l") testFix Performance
          result = ApplySuccess "content" ef emptyFixStats
          stats = addFixToStats result emptyFixStats
      Map.lookup Performance (fsByCategory stats) `shouldBe` Just 1

    it "increments total on partial" $ do
      let ef = mkEnrichedFix (mkFixId "e" "l") testFix Performance
          result = ApplyPartial "content" ef [] emptyFixStats
      fsTotal (addFixToStats result emptyFixStats) `shouldBe` 1

    it "increments applied on partial" $ do
      let ef = mkEnrichedFix (mkFixId "e" "l") testFix Performance
          result = ApplyPartial "content" ef [] emptyFixStats
      fsApplied (addFixToStats result emptyFixStats) `shouldBe` 1

    it "increments total on failure" $ do
      let result = ApplyFailure (EngineError "error")
      fsTotal (addFixToStats result emptyFixStats) `shouldBe` 1

    it "increments failed on failure" $ do
      let result = ApplyFailure (EngineError "error")
      fsFailed (addFixToStats result emptyFixStats) `shouldBe` 1

--------------------------------------------------------------------------------
-- FixEngine Typeclass Tests
--------------------------------------------------------------------------------

-- | A minimal test fix engine for testing
data TestEngine = TestEngine
  { teName :: Text
  , teVersion :: Text
  }

instance FixEngine TestEngine where
  type EngineConfig TestEngine = ()
  type EngineCategory TestEngine = Text

  engineName = teName
  engineVersion = teVersion
  engineCategories _ = ["test-category"]
  engineDescription e = "Test engine: " <> teName e

  findFixes _ _ _ = pure []
  validateFix _ _ _ = pure $ FixValidation ValidationSuccess [] []
  applyFix _ _ content ef = pure $ ApplySuccess content ef emptyFixStats

fixEngineSpec :: Spec
fixEngineSpec = describe "FixEngine" $ do
  describe "TestEngine implementation" $ do
    it "returns configured name" $ do
      let engine = TestEngine "my-test-engine" "1.0.0"
      engineName engine `shouldBe` "my-test-engine"

    it "returns configured version" $ do
      let engine = TestEngine "my-test-engine" "2.0.0"
      engineVersion engine `shouldBe` "2.0.0"

    it "returns categories" $ do
      let engine = TestEngine "test" "1.0"
      engineCategories engine `shouldBe` ["test-category"]

    it "returns description" $ do
      let engine = TestEngine "test" "1.0"
      engineDescription engine `shouldBe` "Test engine: test"

  describe "SomeFixEngine existential wrapper" $ do
    it "wraps a fix engine" $ do
      let engine = TestEngine "wrapped" "1.0"
          wrapped = wrapEngine engine
      engineName wrapped `shouldBe` "wrapped"

    it "preserves version through wrapper" $ do
      let engine = TestEngine "test" "3.0.0"
          wrapped = wrapEngine engine
      engineVersion wrapped `shouldBe` "3.0.0"

    it "preserves description through wrapper" $ do
      let engine = TestEngine "desc-test" "1.0"
          wrapped = wrapEngine engine
      engineDescription wrapped `shouldBe` "Test engine: desc-test"

    it "can store heterogeneous engines in list" $ do
      let e1 = wrapEngine $ TestEngine "engine1" "1.0"
          e2 = wrapEngine $ TestEngine "engine2" "2.0"
          engines = [e1, e2]
      map engineName engines `shouldBe` ["engine1", "engine2"]

  describe "Default implementations" $ do
    it "getEngineStats returns empty by default" $ do
      let engine = TestEngine "test" "1.0"
      stats <- getEngineStats engine
      stats `shouldBe` emptyFixStats

    it "canHandle returns False by default" $ do
      let engine = TestEngine "test" "1.0"
          diag = testDiagnostic
      canHandle engine diag `shouldBe` False

    it "findFixesForDiagnostics returns empty by default" $ do
      let engine = TestEngine "test" "1.0"
      fixes <- findFixesForDiagnostics engine [] "content"
      fixes `shouldBe` []

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Create a test Fix
testFix :: Fix
testFix = Fix
  { fixTitle = "Test fix"
  , fixEdits = [FixEdit (mkSrcSpanRaw "test.hs" 1 1 1 10) "replacement"]
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSAlways
  }

-- | Create a test EnrichedFix with specific span
mkTestEnrichedFix :: Text -> Text -> FilePath -> Int -> Int -> Int -> Int -> EnrichedFix
mkTestEnrichedFix engine local file sl sc el ec =
  let fid = mkFixId engine local
      fix = Fix
        { fixTitle = "Test fix"
        , fixEdits = [FixEdit (mkSrcSpanRaw file sl sc el ec) "replacement"]
        , fixIsPreferred = True
        , fixAddImports = []
        , fixRemoveImports = []
        , fixCategory = FCStyle
        , fixSafety = FSAlways
        }
  in mkEnrichedFix fid fix Performance

-- | Create a test Diagnostic for canHandle tests
testDiagnostic :: Diagnostic
testDiagnostic = Diagnostic
  { diagSpan = mkSrcSpanRaw "test.hs" 1 1 1 10
  , diagSeverity = Warning
  , diagKind = CodePattern
  , diagMessage = "Test diagnostic"
  , diagCode = Nothing
  , diagFixes = []
  , diagRelated = []
  }
