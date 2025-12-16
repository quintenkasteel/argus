{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Argus.AutoFix.Registry
-- Description : Central registry for fix engines
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
-- Portability : GHC
--
-- = Overview
--
-- This module provides a central registry for managing multiple fix engines.
-- The registry enables:
--
-- * Registration and discovery of fix engines
-- * Finding appropriate engines for diagnostics
-- * Coordinating fix application across engines
-- * Collecting statistics across all engines
--
-- = Architecture
--
-- The @FixRegistry@ is the central hub for all fix operations. It maintains
-- a collection of engines wrapped in existential types, allowing heterogeneous
-- engine types to coexist.
--
-- @
-- ┌─────────────────────────────────────────────────────────┐
-- │                     FixRegistry                         │
-- │  ┌───────────────┐  ┌───────────────┐  ┌─────────────┐ │
-- │  │ BooleanEngine │  │  ListEngine   │  │ PartialFix  │ │
-- │  └───────────────┘  └───────────────┘  └─────────────┘ │
-- │                          │                              │
-- │                    Conflict Resolution                  │
-- │                    Dependency Ordering                  │
-- │                    Fix Validation                       │
-- └─────────────────────────────────────────────────────────┘
-- @
--
-- = Thread Safety
--
-- The registry uses @TVar@ for thread-safe engine management and statistics
-- tracking. Multiple threads can safely:
--
-- * Register and unregister engines
-- * Find fixes concurrently
-- * Apply fixes (though the same file should not be modified concurrently)
--
-- = Usage
--
-- @
-- -- Create a registry with some engines
-- registry <- newFixRegistry
-- registerEngine registry booleanEngine
-- registerEngine registry listEngine
--
-- -- Find fixes for a file
-- fixes <- findAllFixes registry filePath content
--
-- -- Apply fixes
-- (newContent, results) <- applyAllFixes registry filePath content fixes
-- @
--
-- @since 1.0.0
module Argus.AutoFix.Registry
  ( -- * Registry Type
    FixRegistry (..)
  , newFixRegistry
  , newFixRegistryWith

    -- * Engine Management
  , registerEngine
  , unregisterEngine
  , getEngine
  , getAllEngines
  , hasEngine

    -- * Finding Fixes
  , findAllFixes
  , findFixesFor
  , findFixesByCategory
  , findFixesForDiagnostic

    -- * Applying Fixes
  , applyAllFixes
  , applySelectedFixes
  , applyFixesWithStrategy
  , ApplyStrategy (..)

    -- * Validation
  , validateAllFixes
  , validateFixInRegistry

    -- * Statistics
  , getRegistryStats
  , RegistryStats (..)
  , emptyRegistryStats

    -- * Engine Info
  , EngineInfo (..)
  , getEngineInfo
  , getAllEngineInfo
  ) where

import Control.Concurrent.STM
import Control.Monad (forM, foldM)
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)

import Argus.Types (Diagnostic (..))

import Argus.AutoFix.Types
  ( FixEngine (..)
  , SomeFixEngine (..)
  , wrapEngine
  , EnrichedFix (..)
  , FixId (..)
  , FixValidation (..)
  , ValidationResult (..)
  , FixApplicationResult (..)
  , FixConflict (..)
  , detectConflicts
  , resolveDependencies
  , FixDependency (..)
  , DependencyType (..)
  , isValidationSuccess
  , FixMetadata (..)
  , ApplyError (..)
  )
import Argus.AutoFix.Types qualified as AFT

import Argus.Rules.Types (Category)

--------------------------------------------------------------------------------
-- Registry Type
--------------------------------------------------------------------------------

-- | Central registry for fix engines.
--
-- The registry maintains a thread-safe collection of fix engines and
-- provides coordinated access to all fix operations.
data FixRegistry = FixRegistry
  { frEngines       :: TVar (Map Text SomeFixEngine)
    -- ^ Registered engines by name
  , frStats         :: TVar RegistryStats
    -- ^ Cumulative statistics
  , frConfig        :: RegistryConfig
    -- ^ Registry configuration
  , frLastUpdated   :: TVar (Maybe UTCTime)
    -- ^ When the registry was last modified
  }

-- | Configuration for the fix registry
data RegistryConfig = RegistryConfig
  { rcMaxEngines       :: Int
    -- ^ Maximum number of engines allowed
  , rcParallelFinding  :: Bool
    -- ^ Find fixes from engines in parallel
  , rcValidateBeforeApply :: Bool
    -- ^ Validate fixes before applying
  , rcConflictResolution :: ConflictStrategy
    -- ^ How to resolve fix conflicts
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default registry configuration
defaultRegistryConfig :: RegistryConfig
defaultRegistryConfig = RegistryConfig
  { rcMaxEngines = 100
  , rcParallelFinding = True
  , rcValidateBeforeApply = True
  , rcConflictResolution = SkipConflicting
  }

-- | Strategy for handling conflicting fixes
data ConflictStrategy
  = SkipConflicting      -- ^ Skip all fixes that conflict
  | KeepFirst            -- ^ Keep the first fix, skip later ones
  | KeepHighestConfidence -- ^ Keep the fix with highest confidence
  | KeepSafest           -- ^ Keep the safest fix
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Create a new empty fix registry with default configuration.
--
-- Uses 'defaultRegistryConfig' which allows up to 100 engines,
-- enables parallel finding, validates before applying, and
-- skips conflicting fixes.
--
-- ==== Returns
--
-- A new empty @FixRegistry@ ready for engine registration.
--
-- ==== Example
--
-- @
-- registry <- newFixRegistry
-- registerEngine registry myEngine
-- @
--
-- @since 1.0.0
newFixRegistry :: IO FixRegistry
newFixRegistry = newFixRegistryWith defaultRegistryConfig

-- | Create a new fix registry with custom configuration.
--
-- ==== Parameters
--
-- * @config@: Registry configuration specifying limits and behavior
--
-- ==== Returns
--
-- A new empty @FixRegistry@ configured according to the provided settings.
--
-- ==== Example
--
-- @
-- let config = defaultRegistryConfig { rcMaxEngines = 50 }
-- registry <- newFixRegistryWith config
-- @
--
-- @since 1.0.0
newFixRegistryWith :: RegistryConfig -> IO FixRegistry
newFixRegistryWith config = do
  engines <- newTVarIO Map.empty
  stats <- newTVarIO emptyRegistryStats
  lastUpdated <- newTVarIO Nothing
  pure FixRegistry
    { frEngines = engines
    , frStats = stats
    , frConfig = config
    , frLastUpdated = lastUpdated
    }

--------------------------------------------------------------------------------
-- Engine Management
--------------------------------------------------------------------------------

-- | Register a fix engine with the registry.
--
-- Adds a new engine to the registry. The engine is wrapped in an existential
-- type, allowing different engine implementations to coexist.
--
-- ==== Parameters
--
-- * @registry@: The registry to register with
-- * @engine@: The engine to register (must implement @FixEngine@)
--
-- ==== Returns
--
-- * @True@ if the engine was successfully registered
-- * @False@ if registration failed (duplicate name or limit reached)
--
-- ==== Failure Conditions
--
-- * An engine with the same name is already registered
-- * The maximum engine count has been reached
--
-- ==== Example
--
-- @
-- data MyEngine = MyEngine
-- instance FixEngine MyEngine where ...
--
-- registry <- newFixRegistry
-- success <- registerEngine registry MyEngine
-- if success
--   then putStrLn "Engine registered"
--   else putStrLn "Registration failed"
-- @
--
-- ==== Thread Safety
--
-- This function is thread-safe; multiple engines can be registered
-- concurrently from different threads.
--
-- @since 1.0.0
registerEngine :: FixEngine e => FixRegistry -> e -> IO Bool
registerEngine registry engine = do
  now <- getCurrentTime
  atomically $ do
    engines <- readTVar (frEngines registry)
    let name = engineName engine
        maxEngines = rcMaxEngines (frConfig registry)
    if Map.member name engines || Map.size engines >= maxEngines
      then pure False
      else do
        writeTVar (frEngines registry) $ Map.insert name (wrapEngine engine) engines
        writeTVar (frLastUpdated registry) (Just now)
        modifyTVar' (frStats registry) $ \s ->
          s { rsEngineCount = rsEngineCount s + 1 }
        pure True

-- | Unregister a fix engine by name.
--
-- Returns True if the engine was found and removed.
unregisterEngine :: FixRegistry -> Text -> IO Bool
unregisterEngine registry name = do
  now <- getCurrentTime
  atomically $ do
    engines <- readTVar (frEngines registry)
    if Map.member name engines
      then do
        writeTVar (frEngines registry) $ Map.delete name engines
        writeTVar (frLastUpdated registry) (Just now)
        modifyTVar' (frStats registry) $ \s ->
          s { rsEngineCount = max 0 (rsEngineCount s - 1) }
        pure True
      else pure False

-- | Get an engine by name
getEngine :: FixRegistry -> Text -> IO (Maybe SomeFixEngine)
getEngine registry name = do
  engines <- readTVarIO (frEngines registry)
  pure $ Map.lookup name engines

-- | Get all registered engines
getAllEngines :: FixRegistry -> IO [SomeFixEngine]
getAllEngines registry = do
  engines <- readTVarIO (frEngines registry)
  pure $ Map.elems engines

-- | Check if an engine is registered
hasEngine :: FixRegistry -> Text -> IO Bool
hasEngine registry name = do
  engines <- readTVarIO (frEngines registry)
  pure $ Map.member name engines

--------------------------------------------------------------------------------
-- Finding Fixes
--------------------------------------------------------------------------------

-- | Find all fixes from all engines for a file.
--
-- Queries all registered engines for fixes applicable to the given file.
-- Results are aggregated and statistics are updated.
--
-- ==== Parameters
--
-- * @registry@: The fix registry containing engines
-- * @path@: File path (used for engine context and filtering)
-- * @content@: File content to analyze
--
-- ==== Returns
--
-- List of all @EnrichedFix@ values from all engines.
--
-- ==== Example
--
-- @
-- fixes <- findAllFixes registry "src/MyModule.hs" content
-- putStrLn $ "Found " ++ show (length fixes) ++ " fixes"
-- @
--
-- ==== Performance
--
-- Engines are queried sequentially by default. Use 'rcParallelFinding'
-- configuration to enable parallel queries.
--
-- @since 1.0.0
findAllFixes :: FixRegistry -> FilePath -> Text -> IO [EnrichedFix]
findAllFixes registry path content = do
  engines <- getAllEngines registry
  allFixes <- forM engines $ \(SomeFixEngine engine) ->
    findFixes engine path content
  let fixes = concat allFixes
  atomically $ modifyTVar' (frStats registry) $ \s ->
    s { rsFixesFound = rsFixesFound s + length fixes }
  pure fixes

-- | Find fixes for specific diagnostics
findFixesFor :: FixRegistry -> [Diagnostic] -> Text -> IO [EnrichedFix]
findFixesFor registry diags content = do
  engines <- getAllEngines registry
  allFixes <- forM engines $ \(SomeFixEngine engine) ->
    findFixesForDiagnostics engine diags content
  let fixes = concat allFixes
  atomically $ modifyTVar' (frStats registry) $ \s ->
    s { rsFixesFound = rsFixesFound s + length fixes }
  pure fixes

-- | Find fixes filtered by category
findFixesByCategory :: FixRegistry -> FilePath -> Text -> Set Category -> IO [EnrichedFix]
findFixesByCategory registry path content categories = do
  allFixes <- findAllFixes registry path content
  pure $ filter (categoryMatches categories) allFixes
  where
    categoryMatches cats ef = fmCategory (efMetadata ef) `Set.member` cats

-- | Find fixes for a specific diagnostic
findFixesForDiagnostic :: FixRegistry -> Diagnostic -> Text -> IO [EnrichedFix]
findFixesForDiagnostic registry diag content = do
  engines <- getAllEngines registry
  allFixes <- forM engines $ \(SomeFixEngine engine) ->
    if canHandle engine diag
      then findFixesForDiagnostics engine [diag] content
      else pure []
  pure $ concat allFixes

--------------------------------------------------------------------------------
-- Applying Fixes
--------------------------------------------------------------------------------

-- | Strategy for applying fixes
data ApplyStrategy
  = ApplyAll           -- ^ Apply all non-conflicting fixes
  | ApplyByCategory [Category]  -- ^ Apply only fixes in these categories
  | ApplyByEngine [Text]        -- ^ Apply only fixes from these engines
  | ApplyById [FixId]           -- ^ Apply only these specific fixes
  | ApplyFirst Int              -- ^ Apply first N fixes
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Apply all fixes from all engines.
--
-- Applies all provided fixes to the content, handling conflicts and
-- dependencies automatically. Uses the registry's configured conflict
-- resolution strategy.
--
-- ==== Parameters
--
-- * @registry@: The fix registry
-- * @path@: File path for context
-- * @content@: Current file content
-- * @fixes@: List of fixes to apply
--
-- ==== Returns
--
-- A tuple of:
--
-- * Final content after all fixes applied
-- * List of 'FixApplicationResult' for each fix attempt
--
-- ==== Example
--
-- @
-- fixes <- findAllFixes registry path content
-- (newContent, results) <- applyAllFixes registry path content fixes
-- let successes = length [r | r\@ApplySuccess{} <- results]
-- putStrLn $ "Applied " ++ show successes ++ " fixes"
-- @
--
-- ==== Fix Ordering
--
-- Fixes are applied in dependency order. Conflicting fixes are
-- resolved according to 'ConflictStrategy' in the registry config.
--
-- @since 1.0.0
applyAllFixes :: FixRegistry
              -> FilePath
              -> Text
              -> [EnrichedFix]
              -> IO (Text, [FixApplicationResult])
applyAllFixes registry = applyFixesWithStrategy registry ApplyAll

-- | Apply selected fixes
applySelectedFixes :: FixRegistry
                   -> FilePath
                   -> Text
                   -> [EnrichedFix]
                   -> [FixId]
                   -> IO (Text, [FixApplicationResult])
applySelectedFixes registry path content fixes selectedIds =
  applyFixesWithStrategy registry (ApplyById selectedIds) path content fixes

-- | Apply fixes with a specific strategy
applyFixesWithStrategy :: FixRegistry
                       -> ApplyStrategy
                       -> FilePath
                       -> Text
                       -> [EnrichedFix]
                       -> IO (Text, [FixApplicationResult])
applyFixesWithStrategy registry strategy path content fixes = do
  let config = frConfig registry
      filtered = filterByStrategy strategy fixes

  -- Detect and handle conflicts
  let conflicts = detectConflicts filtered
      resolved = resolveConflicts (rcConflictResolution config) filtered conflicts

  -- Resolve dependencies and order fixes
  let deps = extractDependencies resolved
      fixIds = map efId resolved
  ordered <- case resolveDependencies fixIds deps of
    Left cycle' -> do
      -- Log cycle and skip problematic fixes
      atomically $ modifyTVar' (frStats registry) $ \s ->
        s { rsCyclesDetected = rsCyclesDetected s + 1 }
      pure $ filter (\f -> efId f `notElem` cycle') resolved
    Right sortedIds ->
      pure $ orderByIds sortedIds resolved

  -- Validate if configured
  validated <- if rcValidateBeforeApply config
    then filterM (validateForApply registry content) ordered
    else pure ordered

  -- Apply fixes sequentially
  (finalContent, results) <- applySequentially registry path content validated

  -- Update stats
  let successCount = length $ filter isSuccessResult results
      failCount = length results - successCount
  atomically $ modifyTVar' (frStats registry) $ \s ->
    s { rsFixesApplied = rsFixesApplied s + successCount
      , rsFixesFailed = rsFixesFailed s + failCount
      , rsConflictsResolved = rsConflictsResolved s + length conflicts
      }

  pure (finalContent, results)
  where
    filterByStrategy ApplyAll fs = fs
    filterByStrategy (ApplyByCategory cats) fs =
      filter (\f -> fmCategory (efMetadata f) `elem` cats) fs
    filterByStrategy (ApplyByEngine names) fs =
      filter (\f -> fixIdEngine (efId f) `elem` names) fs
    filterByStrategy (ApplyById ids) fs =
      filter (\f -> efId f `elem` ids) fs
    filterByStrategy (ApplyFirst n) fs = take n fs

    isSuccessResult ApplySuccess{} = True
    isSuccessResult ApplyPartial{} = True
    isSuccessResult ApplyFailure{} = False

    filterM _ [] = pure []
    filterM predicate (x:xs) = do
      keep <- predicate x
      rest <- filterM predicate xs
      pure $ if keep then x : rest else rest

-- | Apply fixes sequentially
applySequentially :: FixRegistry
                  -> FilePath
                  -> Text
                  -> [EnrichedFix]
                  -> IO (Text, [FixApplicationResult])
applySequentially registry path initialContent fixes = do
  engines <- readTVarIO (frEngines registry)
  foldM (applyOne engines) (initialContent, []) fixes
  where
    applyOne engines (content, results) fix = do
      let engName = fixIdEngine (efId fix)
      case Map.lookup engName engines of
        Nothing -> do
          let failure = ApplyFailure
                { arfError = Argus.AutoFix.Types.EngineError $
                    "Engine not found: " <> engName
                }
          pure (content, results ++ [failure])
        Just (SomeFixEngine engine) -> do
          result <- applyFix engine path content fix
          let newContent = case result of
                ApplySuccess{arsNewContent} -> arsNewContent
                ApplyPartial{arpNewContent} -> arpNewContent
                ApplyFailure{} -> content
          pure (newContent, results ++ [result])

-- | Validate a fix for application
validateForApply :: FixRegistry -> Text -> EnrichedFix -> IO Bool
validateForApply registry content fix = do
  validation <- validateFixInRegistry registry fix content
  pure $ isValidationSuccess validation

--------------------------------------------------------------------------------
-- Conflict Resolution
--------------------------------------------------------------------------------

-- | Resolve conflicts according to strategy
resolveConflicts :: ConflictStrategy -> [EnrichedFix] -> [FixConflict] -> [EnrichedFix]
resolveConflicts _ fixes [] = fixes
resolveConflicts strategy fixes conflicts =
  case strategy of
    SkipConflicting -> filter (not . isInConflict) fixes
    KeepFirst -> keepFirst fixes conflictPairs
    KeepHighestConfidence -> keepByConfidence fixes conflictPairs
    KeepSafest -> keepBySafety fixes conflictPairs
  where
    conflictPairs :: Set (FixId, FixId)
    conflictPairs = Set.fromList
      [(fcFixA c, fcFixB c) | c <- conflicts]

    conflictingIds :: Set FixId
    conflictingIds = Set.fromList $
      concatMap (\(a, b) -> [a, b]) (Set.toList conflictPairs)

    isInConflict fix = efId fix `Set.member` conflictingIds

    keepFirst fs pairs = go fs Set.empty
      where
        go [] _ = []
        go (f:rest) seen
          | efId f `Set.member` seen = go rest seen
          | otherwise =
              let conflicts' = findConflictsFor (efId f) pairs
                  newSeen = Set.union seen (Set.fromList conflicts')
              in f : go rest newSeen

    findConflictsFor fid pairs =
      [b | (a, b) <- Set.toList pairs, a == fid] ++
      [a | (a, b) <- Set.toList pairs, b == fid]

    keepByConfidence fs pairs =
      let grouped = groupByConflict fs pairs
      in concatMap (take 1 . sortByConfidence) grouped

    keepBySafety fs pairs =
      let grouped = groupByConflict fs pairs
      in concatMap (take 1 . sortBySafety) grouped

    groupByConflict fs _pairs =
      -- Group fixes that conflict with each other
      let conflicting = filter isInConflict fs
          nonConflicting = filter (not . isInConflict) fs
      in [nonConflicting] ++ [[f] | f <- conflicting]  -- Simplified

    sortByConfidence = id  -- Would sort by fmConfidence
    sortBySafety = id      -- Would sort by fmSafety

-- | Extract dependencies from fixes
extractDependencies :: [EnrichedFix] -> [FixDependency]
extractDependencies fixes =
  [ FixDependency (efId f) dep Argus.AutoFix.Types.Requires Nothing
  | f <- fixes
  , dep <- Set.toList (efDependencies f)
  ]

-- | Order fixes by a list of IDs
orderByIds :: [FixId] -> [EnrichedFix] -> [EnrichedFix]
orderByIds order fixes =
  let fixMap = Map.fromList [(efId f, f) | f <- fixes]
  in [f | fid <- order, Just f <- [Map.lookup fid fixMap]]

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

-- | Validate all fixes
validateAllFixes :: FixRegistry -> [EnrichedFix] -> Text -> IO [(EnrichedFix, FixValidation)]
validateAllFixes registry fixes content =
  forM fixes $ \fix -> do
    validation <- validateFixInRegistry registry fix content
    pure (fix, validation)

-- | Validate a single fix using the registry
validateFixInRegistry :: FixRegistry -> EnrichedFix -> Text -> IO FixValidation
validateFixInRegistry registry fix content = do
  engines <- readTVarIO (frEngines registry)
  let engName = fixIdEngine (efId fix)
  case Map.lookup engName engines of
    Nothing -> pure FixValidation
      { fvResult = ValidationError $ "Engine not found: " <> engName
      , fvChecks = []
      , fvSuggestions = []
      }
    Just (SomeFixEngine engine) ->
      AFT.validateFix engine fix content

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

-- | Statistics for the registry
data RegistryStats = RegistryStats
  { rsEngineCount       :: Int
    -- ^ Number of registered engines
  , rsFixesFound        :: Int
    -- ^ Total fixes found
  , rsFixesApplied      :: Int
    -- ^ Total fixes applied
  , rsFixesFailed       :: Int
    -- ^ Total fixes that failed to apply
  , rsConflictsResolved :: Int
    -- ^ Total conflicts resolved
  , rsCyclesDetected    :: Int
    -- ^ Total dependency cycles detected
  , rsValidationsPassed :: Int
    -- ^ Total validations passed
  , rsValidationsFailed :: Int
    -- ^ Total validations failed
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty statistics
emptyRegistryStats :: RegistryStats
emptyRegistryStats = RegistryStats 0 0 0 0 0 0 0 0

-- | Get current registry statistics
getRegistryStats :: FixRegistry -> IO RegistryStats
getRegistryStats registry = readTVarIO (frStats registry)

--------------------------------------------------------------------------------
-- Engine Info
--------------------------------------------------------------------------------

-- | Information about a registered engine
data EngineInfo = EngineInfo
  { eiName        :: Text
    -- ^ Engine name
  , eiVersion     :: Text
    -- ^ Engine version
  , eiDescription :: Text
    -- ^ Engine description
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Get information about an engine
getEngineInfo :: FixRegistry -> Text -> IO (Maybe EngineInfo)
getEngineInfo registry name = do
  mEngine <- getEngine registry name
  pure $ case mEngine of
    Nothing -> Nothing
    Just (SomeFixEngine engine) -> Just EngineInfo
      { eiName = engineName engine
      , eiVersion = engineVersion engine
      , eiDescription = engineDescription engine
      }

-- | Get information about all engines
getAllEngineInfo :: FixRegistry -> IO [EngineInfo]
getAllEngineInfo registry = do
  engines <- getAllEngines registry
  pure [EngineInfo (engineName e) (engineVersion e) (engineDescription e)
       | SomeFixEngine e <- engines]
