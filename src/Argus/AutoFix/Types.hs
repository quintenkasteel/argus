{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Argus.AutoFix.Types
-- Description : Core types for the extensible auto-fix infrastructure
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module defines the core type infrastructure for Argus's extensible
-- auto-fix system. It provides:
--
-- * The 'FixEngine' typeclass for implementing fix engines
-- * 'EnrichedFix' for fixes with comprehensive metadata
-- * 'FixId' for type-safe fix identification
-- * 'FixValidation' for fix validation results
-- * 'FixConflict' for detecting fix conflicts
-- * 'FixDependency' for managing fix dependencies
--
-- == Architecture
--
-- The auto-fix system is built around three core concepts:
--
-- 1. **Fix Engines**: Implementations of 'FixEngine' that can find and apply
--    fixes for specific categories of issues.
--
-- 2. **Enriched Fixes**: Fixes wrapped with metadata about safety, confidence,
--    dependencies, and conflicts.
--
-- 3. **Fix Registry**: A central registry (see 'Argus.AutoFix.Registry') that
--    manages multiple fix engines.
--
-- == Example
--
-- @
-- data MyFixEngine = MyFixEngine { ... }
--
-- instance FixEngine MyFixEngine where
--   type EngineConfig MyFixEngine = MyConfig
--   type EngineCategory MyFixEngine = MyCategory
--   engineName _ = "my-fix-engine"
--   engineCategories _ = [CategoryA, CategoryB]
--   findFixes engine fp content = ...
--   applyFix engine fp content fix = ...
--   validateFix engine fix content = ...
-- @
module Argus.AutoFix.Types
  ( -- * Fix Engine Typeclass
    FixEngine (..)

    -- * Fix Identification
  , FixId (..)
  , mkFixId
  , parseFixId

    -- * Enriched Fixes
  , EnrichedFix (..)
  , mkEnrichedFix
  , mkEnrichedFixWithDeps
  , enrichFix
  , enrichFixWithDeps
  , stripEnrichment
  , addDependency
  , addConflict
  , setDependencies
  , setConflicts
  , ruleIdToFixId

    -- * Fix Metadata
  , FixMetadata (..)
  , defaultFixMetadata

    -- * Fix Validation
  , FixValidation (..)
  , ValidationResult (..)
  , isValidationSuccess
  , validationErrors
  , validationWarnings

    -- * Fix Conflicts
  , FixConflict (..)
  , ConflictType (..)
  , detectConflicts
  , conflictsExist

    -- * Fix Dependencies
  , FixDependency (..)
  , DependencyType (..)
  , resolveDependencies
  , topologicalSort

    -- * Fix Application Results
  , FixApplicationResult (..)
  , ApplyError (..)
  , isSuccess
  , getAppliedContent

    -- * Confidence Scoring
  , Confidence (..)
  , mkConfidence
  , unConfidence
  , highConfidence
  , mediumConfidence
  , lowConfidence
  , unknownConfidence

    -- * Fix Statistics
  , FixStats (..)
  , emptyFixStats
  , mergeFixStats
  , addFixToStats

    -- * Existential Wrapper
  , SomeFixEngine (..)
  , wrapEngine
  ) where

import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Aeson qualified as A
import Data.Hashable (Hashable (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Argus.Types
  ( SrcSpan (..)
  , Fix (..)
  , FixEdit (..)
  , Diagnostic (..)
  )

import Argus.Rules.Types
  ( Category
  , SafetyLevel (..)
  , safetyToText
  )

--------------------------------------------------------------------------------
-- Fix Identification
--------------------------------------------------------------------------------

-- | Type-safe fix identifier combining engine name and local ID.
--
-- Format: "engine-name/local-id"
--
-- Examples:
-- * "boolean-fix/not-true-001"
-- * "list-fix/map-id-to-id-042"
-- * "import-fix/unused-import-Data.List"
data FixId = FixId
  { fixIdEngine :: Text
    -- ^ Engine that produced this fix
  , fixIdLocal  :: Text
    -- ^ Local ID within the engine
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

instance ToJSON FixId where
  toJSON fid = A.String $ fixIdEngine fid <> "/" <> fixIdLocal fid

instance FromJSON FixId where
  parseJSON = A.withText "FixId" $ \t ->
    case parseFixId t of
      Just fid -> pure fid
      Nothing  -> fail $ "Invalid FixId format: " <> T.unpack t

-- | Create a FixId from engine name and local ID
mkFixId :: Text -> Text -> FixId
mkFixId = FixId

-- | Parse a FixId from its text representation
parseFixId :: Text -> Maybe FixId
parseFixId t = case T.breakOn "/" t of
  (engine, rest)
    | not (T.null rest) -> Just $ FixId engine (T.drop 1 rest)
    | otherwise -> Nothing

--------------------------------------------------------------------------------
-- Confidence Scoring
--------------------------------------------------------------------------------

-- | Confidence score for a fix (0.0 to 1.0)
--
-- Higher values indicate more certainty that the fix is correct.
newtype Confidence = Confidence { unConfidenceValue :: Double }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON Confidence where
  toJSON (Confidence c) = toJSON c

instance FromJSON Confidence where
  parseJSON v = Confidence . clamp <$> A.parseJSON v
    where clamp x = max 0.0 (min 1.0 x)

instance Hashable Confidence where
  hashWithSalt salt (Confidence c) = hashWithSalt salt (round (c * 1000) :: Int)

-- | Create a confidence value, clamping to [0.0, 1.0]
mkConfidence :: Double -> Confidence
mkConfidence x = Confidence $ max 0.0 (min 1.0 x)

-- | Extract confidence value
unConfidence :: Confidence -> Double
unConfidence = unConfidenceValue

-- | High confidence (>= 0.9)
highConfidence :: Confidence
highConfidence = Confidence 0.95

-- | Medium confidence (0.7 - 0.9)
mediumConfidence :: Confidence
mediumConfidence = Confidence 0.8

-- | Low confidence (< 0.7)
lowConfidence :: Confidence
lowConfidence = Confidence 0.5

-- | Unknown confidence (0.0)
unknownConfidence :: Confidence
unknownConfidence = Confidence 0.0

--------------------------------------------------------------------------------
-- Fix Metadata
--------------------------------------------------------------------------------

-- | Comprehensive metadata for a fix
data FixMetadata = FixMetadata
  { fmConfidence    :: Confidence
    -- ^ How confident we are this fix is correct
  , fmSafety        :: SafetyLevel
    -- ^ Safety level for automatic application
  , fmCategory      :: Category
    -- ^ Category of the fix
  , fmTags          :: Set Text
    -- ^ Tags for filtering and grouping
  , fmCreatedAt     :: Maybe UTCTime
    -- ^ When the fix was created
  , fmSourceRule    :: Maybe Text
    -- ^ The rule that triggered this fix
  , fmExplanation   :: Maybe Text
    -- ^ Human-readable explanation
  , fmNotes         :: [Text]
    -- ^ Additional notes about this fix
  , fmReferences    :: [Text]
    -- ^ URLs or references for more information
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default metadata with safe defaults
defaultFixMetadata :: Category -> FixMetadata
defaultFixMetadata cat = FixMetadata
  { fmConfidence  = highConfidence
  , fmSafety      = Safe
  , fmCategory    = cat
  , fmTags        = Set.empty
  , fmCreatedAt   = Nothing
  , fmSourceRule  = Nothing
  , fmExplanation = Nothing
  , fmNotes       = []
  , fmReferences  = []
  }

--------------------------------------------------------------------------------
-- Fix Dependencies
--------------------------------------------------------------------------------

-- | Type of dependency between fixes
data DependencyType
  = MustApplyBefore
    -- ^ This fix must be applied before the dependent fix
  | MustApplyAfter
    -- ^ This fix must be applied after the dependent fix
  | MutuallyExclusive
    -- ^ Only one of these fixes can be applied
  | Requires
    -- ^ This fix requires another fix to also be applied
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData, Hashable)

-- | A dependency relationship between fixes
data FixDependency = FixDependency
  { fdFrom     :: FixId
    -- ^ The fix with the dependency
  , fdTo       :: FixId
    -- ^ The fix it depends on
  , fdType     :: DependencyType
    -- ^ Type of dependency
  , fdReason   :: Maybe Text
    -- ^ Why this dependency exists
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Resolve dependencies and return fixes in order of application.
--
-- Returns Left with cycle information if dependencies form a cycle.
resolveDependencies :: [FixId] -> [FixDependency] -> Either [FixId] [FixId]
resolveDependencies fixIds deps =
  case topologicalSort fixIds (buildGraph deps) of
    Nothing -> Left $ detectCycle fixIds (buildGraph deps)
    Just sorted -> Right sorted
  where
    buildGraph :: [FixDependency] -> Map FixId [FixId]
    buildGraph = foldr addEdge Map.empty
      where
        addEdge fd acc = case fdType fd of
          MustApplyBefore ->
            Map.insertWith (++) (fdFrom fd) [fdTo fd] acc
          MustApplyAfter ->
            Map.insertWith (++) (fdTo fd) [fdFrom fd] acc
          Requires ->
            Map.insertWith (++) (fdFrom fd) [fdTo fd] acc
          MutuallyExclusive -> acc  -- Handled separately

    detectCycle :: [FixId] -> Map FixId [FixId] -> [FixId]
    detectCycle ids graph = case ids of
      [] -> []
      (x:_) -> findCycleFrom x graph Set.empty []

    findCycleFrom :: FixId -> Map FixId [FixId] -> Set FixId -> [FixId] -> [FixId]
    findCycleFrom current graph visited path
      | current `Set.member` visited = current : takeWhile (/= current) (reverse path) ++ [current]
      | otherwise =
          let neighbors = Map.findWithDefault [] current graph
              newVisited = Set.insert current visited
              newPath = current : path
          in case filter (`Set.member` visited) neighbors of
               (cyc:_) -> cyc : takeWhile (/= cyc) (reverse newPath) ++ [cyc]
               [] -> case neighbors of
                       [] -> []
                       (n:_) -> findCycleFrom n graph newVisited newPath

-- | Topological sort of fixes based on dependencies
topologicalSort :: [FixId] -> Map FixId [FixId] -> Maybe [FixId]
topologicalSort fixIds graph = go fixIds Set.empty []
  where
    go :: [FixId] -> Set FixId -> [FixId] -> Maybe [FixId]
    go [] _ result = Just $ reverse result
    go remaining visited result =
      let (ready, notReady) = partition (allDepsVisited visited) remaining
      in if null ready && not (null notReady)
         then Nothing  -- Cycle detected
         else go notReady (Set.union visited (Set.fromList ready)) (ready ++ result)

    allDepsVisited :: Set FixId -> FixId -> Bool
    allDepsVisited visited fid =
      all (`Set.member` visited) (Map.findWithDefault [] fid graph)

    partition :: (a -> Bool) -> [a] -> ([a], [a])
    partition p xs = (filter p xs, filter (not . p) xs)

--------------------------------------------------------------------------------
-- Fix Conflicts
--------------------------------------------------------------------------------

-- | Type of conflict between fixes
data ConflictType
  = OverlappingSpan
    -- ^ Fixes modify overlapping regions
  | SameLocation
    -- ^ Fixes modify the exact same location
  | SemanticConflict
    -- ^ Fixes have semantic conflict (e.g., opposite changes)
  | ResourceConflict Text
    -- ^ Fixes conflict over a resource (named)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | A conflict between two fixes
data FixConflict = FixConflict
  { fcFixA     :: FixId
    -- ^ First conflicting fix
  , fcFixB     :: FixId
    -- ^ Second conflicting fix
  , fcType     :: ConflictType
    -- ^ Type of conflict
  , fcSpan     :: Maybe SrcSpan
    -- ^ Location of conflict if applicable
  , fcMessage  :: Text
    -- ^ Human-readable conflict description
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Detect conflicts between a set of enriched fixes
detectConflicts :: [EnrichedFix] -> [FixConflict]
detectConflicts fixes = concatMap (uncurry checkPair) pairs
  where
    pairs = [(a, b) | (a:rest) <- tails fixes, b <- rest]

    tails :: [a] -> [[a]]
    tails [] = [[]]
    tails xs@(_:rest) = xs : tails rest

    checkPair :: EnrichedFix -> EnrichedFix -> [FixConflict]
    checkPair efA efB =
      let conflicts = catMaybes
            [ checkSpanOverlap efA efB
            , checkDeclaredConflict efA efB
            ]
      in conflicts

    checkSpanOverlap :: EnrichedFix -> EnrichedFix -> Maybe FixConflict
    checkSpanOverlap efA efB =
      let spansA = map fixEditSpan $ fixEdits $ efFix efA
          spansB = map fixEditSpan $ fixEdits $ efFix efB
          overlapping = [ (a, b) | a <- spansA, b <- spansB, spansOverlap a b ]
      in case overlapping of
           [] -> Nothing
           ((spanA, _):_) ->
             Just FixConflict
               { fcFixA = efId efA
               , fcFixB = efId efB
               , fcType = OverlappingSpan
               , fcSpan = Just spanA
               , fcMessage = "Fixes modify overlapping regions"
               }

    checkDeclaredConflict :: EnrichedFix -> EnrichedFix -> Maybe FixConflict
    checkDeclaredConflict efA efB
      | efId efB `Set.member` efConflicts efA =
          Just FixConflict
            { fcFixA = efId efA
            , fcFixB = efId efB
            , fcType = SemanticConflict
            , fcSpan = Nothing
            , fcMessage = "Declared conflict between fixes"
            }
      | otherwise = Nothing

    spansOverlap :: SrcSpan -> SrcSpan -> Bool
    spansOverlap a b =
      srcSpanFile a == srcSpanFile b &&
      not (srcSpanEndLine a < srcSpanStartLine b ||
           srcSpanEndLine b < srcSpanStartLine a ||
           (srcSpanEndLine a == srcSpanStartLine b &&
            srcSpanEndCol a <= srcSpanStartCol b) ||
           (srcSpanEndLine b == srcSpanStartLine a &&
            srcSpanEndCol b <= srcSpanStartCol a))

    catMaybes :: [Maybe a] -> [a]
    catMaybes [] = []
    catMaybes (Nothing:xs) = catMaybes xs
    catMaybes (Just x:xs) = x : catMaybes xs

-- | Check if any conflicts exist in a list
conflictsExist :: [FixConflict] -> Bool
conflictsExist = not . null

--------------------------------------------------------------------------------
-- Fix Validation
--------------------------------------------------------------------------------

-- | Result of validating a fix
data ValidationResult
  = ValidationSuccess
    -- ^ Fix validated successfully
  | ValidationWarning Text
    -- ^ Fix has warnings but can be applied
  | ValidationError Text
    -- ^ Fix failed validation
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Comprehensive fix validation result
data FixValidation = FixValidation
  { fvResult      :: ValidationResult
    -- ^ Overall result
  , fvChecks      :: [(Text, ValidationResult)]
    -- ^ Individual check results
  , fvSuggestions :: [Text]
    -- ^ Suggestions for improvement
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Check if validation was successful (no errors)
isValidationSuccess :: FixValidation -> Bool
isValidationSuccess fv = case fvResult fv of
  ValidationSuccess   -> True
  ValidationWarning _ -> True
  ValidationError _   -> False

-- | Extract all validation errors
validationErrors :: FixValidation -> [Text]
validationErrors fv =
  [ msg | (_, ValidationError msg) <- fvChecks fv ] ++
  case fvResult fv of
    ValidationError msg -> [msg]
    _ -> []

-- | Extract all validation warnings
validationWarnings :: FixValidation -> [Text]
validationWarnings fv =
  [ msg | (_, ValidationWarning msg) <- fvChecks fv ] ++
  case fvResult fv of
    ValidationWarning msg -> [msg]
    _ -> []

--------------------------------------------------------------------------------
-- Enriched Fixes
--------------------------------------------------------------------------------

-- | A fix enriched with comprehensive metadata.
--
-- This wraps a basic 'Fix' with additional information about:
-- * Unique identification
-- * Confidence and safety levels
-- * Dependencies on other fixes
-- * Conflicts with other fixes
-- * Required imports
data EnrichedFix = EnrichedFix
  { efId           :: FixId
    -- ^ Unique identifier
  , efFix          :: Fix
    -- ^ The underlying fix
  , efMetadata     :: FixMetadata
    -- ^ Comprehensive metadata
  , efDependencies :: Set FixId
    -- ^ Fixes this one depends on
  , efConflicts    :: Set FixId
    -- ^ Fixes this one conflicts with
  , efValidation   :: Maybe FixValidation
    -- ^ Validation result if validated
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Create an enriched fix from a basic fix
mkEnrichedFix :: FixId -> Fix -> Category -> EnrichedFix
mkEnrichedFix fid fix cat = EnrichedFix
  { efId = fid
  , efFix = fix
  , efMetadata = defaultFixMetadata cat
  , efDependencies = Set.empty
  , efConflicts = Set.empty
  , efValidation = Nothing
  }

-- | Enrich a basic fix with default metadata
enrichFix :: Text -> Text -> Fix -> Category -> EnrichedFix
enrichFix engName localId fix cat =
  mkEnrichedFix (mkFixId engName localId) fix cat

-- | Strip enrichment and return the basic fix
stripEnrichment :: EnrichedFix -> Fix
stripEnrichment = efFix

-- | Create an enriched fix with dependencies and conflicts
--
-- This is useful for creating fixes from configurable rules that specify
-- rule-level dependencies.
mkEnrichedFixWithDeps :: FixId
                      -> Fix
                      -> Category
                      -> Set FixId  -- ^ Dependencies
                      -> Set FixId  -- ^ Conflicts
                      -> EnrichedFix
mkEnrichedFixWithDeps fid fix cat deps confs = EnrichedFix
  { efId = fid
  , efFix = fix
  , efMetadata = defaultFixMetadata cat
  , efDependencies = deps
  , efConflicts = confs
  , efValidation = Nothing
  }

-- | Enrich a basic fix with dependencies and conflicts
enrichFixWithDeps :: Text         -- ^ Engine name
                  -> Text         -- ^ Local ID
                  -> Fix
                  -> Category
                  -> Set FixId    -- ^ Dependencies
                  -> Set FixId    -- ^ Conflicts
                  -> EnrichedFix
enrichFixWithDeps engName localId fix cat deps confs =
  mkEnrichedFixWithDeps (mkFixId engName localId) fix cat deps confs

-- | Add a dependency to an enriched fix
addDependency :: FixId -> EnrichedFix -> EnrichedFix
addDependency depId ef = ef { efDependencies = Set.insert depId (efDependencies ef) }

-- | Add a conflict to an enriched fix
addConflict :: FixId -> EnrichedFix -> EnrichedFix
addConflict confId ef = ef { efConflicts = Set.insert confId (efConflicts ef) }

-- | Set all dependencies for an enriched fix
setDependencies :: Set FixId -> EnrichedFix -> EnrichedFix
setDependencies deps ef = ef { efDependencies = deps }

-- | Set all conflicts for an enriched fix
setConflicts :: Set FixId -> EnrichedFix -> EnrichedFix
setConflicts confs ef = ef { efConflicts = confs }

-- | Convert a rule ID (Text) to a FixId using an engine name
--
-- This is useful for converting ConfigurableRule dependencies (which are
-- rule IDs as Text) to FixIds for the fix ordering system.
--
-- @
-- -- Example: Convert "partial/head" to FixId
-- ruleIdToFixId "configurable-rules" "partial/head"
-- -- Result: FixId { fixIdEngine = "configurable-rules", fixIdLocal = "partial/head" }
-- @
ruleIdToFixId :: Text -> Text -> FixId
ruleIdToFixId engineName ruleId = mkFixId engineName ruleId

--------------------------------------------------------------------------------
-- Fix Application Results
--------------------------------------------------------------------------------

-- | Errors that can occur when applying a fix
data ApplyError
  = SpanOutOfBounds SrcSpan
    -- ^ The fix span is outside the file bounds
  | ContentMismatch Text Text
    -- ^ Expected content doesn't match actual
  | ConflictingEdits [FixEdit]
    -- ^ Multiple edits conflict
  | ValidationFailed Text
    -- ^ Fix failed validation
  | DependencyError FixId Text
    -- ^ Dependency not satisfied
  | EngineError Text
    -- ^ Internal engine error
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Result of applying a fix
data FixApplicationResult
  = ApplySuccess
      { arsNewContent :: Text
        -- ^ The new file content
      , arsAppliedFix :: EnrichedFix
        -- ^ The fix that was applied
      , arsStats      :: FixStats
        -- ^ Statistics about the application
      }
  | ApplyPartial
      { arpNewContent  :: Text
        -- ^ Partial result
      , arpAppliedFix  :: EnrichedFix
        -- ^ The fix (partially applied)
      , arpErrors      :: [ApplyError]
        -- ^ Errors encountered
      , arpStats       :: FixStats
        -- ^ Statistics
      }
  | ApplyFailure
      { arfError :: ApplyError
        -- ^ The error that caused failure
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Check if application was successful
isSuccess :: FixApplicationResult -> Bool
isSuccess ApplySuccess{} = True
isSuccess _ = False

-- | Get the applied content if successful
getAppliedContent :: FixApplicationResult -> Maybe Text
getAppliedContent ApplySuccess{arsNewContent} = Just arsNewContent
getAppliedContent ApplyPartial{arpNewContent} = Just arpNewContent
getAppliedContent ApplyFailure{} = Nothing

--------------------------------------------------------------------------------
-- Fix Statistics
--------------------------------------------------------------------------------

-- | Statistics about fix operations
data FixStats = FixStats
  { fsTotal         :: Int
    -- ^ Total fixes attempted
  , fsApplied       :: Int
    -- ^ Successfully applied
  , fsFailed        :: Int
    -- ^ Failed to apply
  , fsSkipped       :: Int
    -- ^ Skipped (e.g., conflicts)
  , fsByCategory    :: Map Category Int
    -- ^ Breakdown by category
  , fsBySafety      :: Map Text Int
    -- ^ Breakdown by safety level (keyed by text representation)
  , fsConflicts     :: Int
    -- ^ Number of conflicts detected
  , fsDependencies  :: Int
    -- ^ Number of dependencies resolved
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty statistics
emptyFixStats :: FixStats
emptyFixStats = FixStats
  { fsTotal        = 0
  , fsApplied      = 0
  , fsFailed       = 0
  , fsSkipped      = 0
  , fsByCategory   = Map.empty
  , fsBySafety     = Map.empty
  , fsConflicts    = 0
  , fsDependencies = 0
  }

-- | Merge two statistics
mergeFixStats :: FixStats -> FixStats -> FixStats
mergeFixStats a b = FixStats
  { fsTotal        = fsTotal a + fsTotal b
  , fsApplied      = fsApplied a + fsApplied b
  , fsFailed       = fsFailed a + fsFailed b
  , fsSkipped      = fsSkipped a + fsSkipped b
  , fsByCategory   = Map.unionWith (+) (fsByCategory a) (fsByCategory b)
  , fsBySafety     = Map.unionWith (+) (fsBySafety a) (fsBySafety b)
  , fsConflicts    = fsConflicts a + fsConflicts b
  , fsDependencies = fsDependencies a + fsDependencies b
  }

-- | Add a fix result to statistics
addFixToStats :: FixApplicationResult -> FixStats -> FixStats
addFixToStats result stats = case result of
  ApplySuccess{arsAppliedFix} ->
    stats
      { fsTotal = fsTotal stats + 1
      , fsApplied = fsApplied stats + 1
      , fsByCategory = incrementCategory (efMetadata arsAppliedFix) (fsByCategory stats)
      , fsBySafety = incrementSafety (efMetadata arsAppliedFix) (fsBySafety stats)
      }
  ApplyPartial{} ->
    stats { fsTotal = fsTotal stats + 1, fsApplied = fsApplied stats + 1 }
  ApplyFailure{} ->
    stats { fsTotal = fsTotal stats + 1, fsFailed = fsFailed stats + 1 }
  where
    incrementCategory meta m =
      Map.insertWith (+) (fmCategory meta) 1 m
    incrementSafety meta m =
      Map.insertWith (+) (safetyToText $ fmSafety meta) 1 m

--------------------------------------------------------------------------------
-- Fix Engine Typeclass
--------------------------------------------------------------------------------

-- | Typeclass for fix engines.
--
-- A fix engine is responsible for:
-- * Finding fixes in source code
-- * Validating fixes before application
-- * Applying fixes to source code
--
-- Each engine has associated types for its configuration and category system.
class FixEngine engine where
  -- | Configuration type for this engine
  type EngineConfig engine

  -- | Category type for this engine's fixes
  type EngineCategory engine

  -- | Get the engine name (used in FixId)
  engineName :: engine -> Text

  -- | Get the version of this engine
  engineVersion :: engine -> Text
  engineVersion _ = "1.0.0"

  -- | Get categories this engine handles
  engineCategories :: engine -> [EngineCategory engine]

  -- | Get a description of this engine
  engineDescription :: engine -> Text
  engineDescription e = "Fix engine: " <> engineName e

  -- | Find fixes in a file
  findFixes :: engine
            -> FilePath      -- ^ File path
            -> Text          -- ^ File content
            -> IO [EnrichedFix]

  -- | Find fixes for specific diagnostics
  findFixesForDiagnostics :: engine
                          -> [Diagnostic]  -- ^ Diagnostics to fix
                          -> Text          -- ^ File content
                          -> IO [EnrichedFix]
  findFixesForDiagnostics _ _ _ = pure []

  -- | Validate a fix before application
  validateFix :: engine
              -> EnrichedFix  -- ^ Fix to validate
              -> Text         -- ^ Current file content
              -> IO FixValidation

  -- | Apply a single fix to content
  applyFix :: engine
           -> FilePath        -- ^ File path
           -> Text            -- ^ Current content
           -> EnrichedFix     -- ^ Fix to apply
           -> IO FixApplicationResult

  -- | Apply multiple fixes to content (in order)
  applyFixes :: engine
             -> FilePath          -- ^ File path
             -> Text              -- ^ Current content
             -> [EnrichedFix]     -- ^ Fixes to apply
             -> IO (Text, [FixApplicationResult])
  applyFixes engine path content fixes = go content fixes []
    where
      go curr [] results = pure (curr, reverse results)
      go curr (f:fs) results = do
        result <- applyFix engine path curr f
        case result of
          ApplySuccess{arsNewContent} ->
            go arsNewContent fs (result : results)
          ApplyPartial{arpNewContent} ->
            go arpNewContent fs (result : results)
          ApplyFailure{} ->
            go curr fs (result : results)

  -- | Check if this engine can handle a diagnostic
  canHandle :: engine -> Diagnostic -> Bool
  canHandle _ _ = False

  -- | Get engine statistics
  getEngineStats :: engine -> IO FixStats
  getEngineStats _ = pure emptyFixStats

--------------------------------------------------------------------------------
-- Existential Wrapper
--------------------------------------------------------------------------------

-- | Existential wrapper for heterogeneous fix engine collections.
--
-- This allows storing different fix engine types in a single collection:
--
-- @
-- engines :: [SomeFixEngine]
-- engines = [wrapEngine booleanEngine, wrapEngine listEngine]
-- @
data SomeFixEngine = forall e. FixEngine e => SomeFixEngine e

-- | Wrap any fix engine in the existential type
wrapEngine :: FixEngine e => e -> SomeFixEngine
wrapEngine = SomeFixEngine

instance FixEngine SomeFixEngine where
  type EngineConfig SomeFixEngine = ()
  type EngineCategory SomeFixEngine = Text

  engineName (SomeFixEngine e) = engineName e
  engineVersion (SomeFixEngine e) = engineVersion e
  engineCategories (SomeFixEngine _) = []  -- Cannot expose heterogeneous categories
  engineDescription (SomeFixEngine e) = engineDescription e

  findFixes (SomeFixEngine e) = findFixes e
  findFixesForDiagnostics (SomeFixEngine e) = findFixesForDiagnostics e
  validateFix (SomeFixEngine e) = validateFix e
  applyFix (SomeFixEngine e) = applyFix e
  applyFixes (SomeFixEngine e) = applyFixes e
  canHandle (SomeFixEngine e) = canHandle e
  getEngineStats (SomeFixEngine e) = getEngineStats e
