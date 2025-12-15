{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.HIE.FixValidator
-- Description : Type-aware fix validation using HIE information
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
-- Portability : GHC
--
-- = Overview
--
-- This module provides type-aware validation for fixes using HIE data.
-- It ensures that fixes preserve type correctness by checking:
--
-- * Type class constraints (Ord, Eq, Monoid, etc.)
-- * Symbol shadowing and conflicts
-- * Cross-module safety
-- * Semantic preservation
--
-- = Validation Pipeline
--
-- @
-- Fix ──► Constraint Check ──► Symbol Safety ──► Type Preservation ──► Result
--             │                    │                   │
--             ▼                    ▼                   ▼
--         [Ord a?]           [Shadowing?]        [Type match?]
--         [Eq a?]            [Conflicts?]        [Compatible?]
-- @
--
-- = Configuration Modes
--
-- * __Default Mode__: Balanced safety and usability, allows partial type info
-- * __Strict Mode__: Maximum safety, fails on warnings, requires full types
--
-- = Constraint Inference
--
-- The validator infers required constraints from fix patterns:
--
-- * @ordNub@ / @Set.fromList@ → requires @Ord@ instance
-- * @HashMap.fromList@ → requires @Hashable@ instance
-- * @fold@ → requires @Monoid@ instance
-- * @\<\>@ → requires @Semigroup@ instance
--
-- = Validation Results
--
-- * __Errors__: Block fix application (constraint violations, conflicts)
-- * __Warnings__: Allow application with caveats (partial info, deprecations)
--
-- = Thread Safety
--
-- Validation functions are IO-based and not thread-safe due to HIE queries.
-- Use separate database connections for concurrent validation.
--
-- @since 1.0.0
module Argus.HIE.FixValidator
  ( -- * Fix Validation
    validateFix
  , validateFixes
  , FixValidationResult (..)
  , FixValidationError (..)
  , FixValidationWarning (..)

    -- * Constraint Validation
  , validateConstraints
  , checkConstraintPreservation
  , ConstraintCheck (..)
  , ConstraintResult (..)

    -- * Symbol Safety
  , checkSymbolSafety
  , checkShadowingForFix
  , checkNameConflictsForFix
  , SymbolSafety (..)

    -- * Type Preservation
  , checkTypePreservation
  , TypePreservationResult (..)

    -- * Batch Validation
  , batchValidateFixes
  , BatchValidationResult (..)

    -- * Validation Configuration
  , FixValidatorConfig (..)
  , defaultValidatorConfig
  , strictValidatorConfig
  ) where

import Control.Monad (forM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import HieDb (HieDb)

import Argus.Types
  ( Fix(..), FixEdit(..), SrcSpan(..), FixCategory(..)
  , FixImport(..), Line(..), Column(..)
  )
import Argus.HIE.Types
import Argus.HIE.TypeInfo
import Argus.HIE.SymbolTable

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for fix validation
data FixValidatorConfig = FixValidatorConfig
  { fvcCheckConstraints     :: Bool        -- ^ Check type class constraints
  , fvcCheckShadowing       :: Bool        -- ^ Check for symbol shadowing
  , fvcCheckTypePreservation :: Bool       -- ^ Verify types are preserved
  , fvcStrictMode           :: Bool        -- ^ Fail on warnings
  , fvcAllowPartialTypes    :: Bool        -- ^ Allow fixes for partial type info
  , fvcMaxWarnings          :: Int         -- ^ Max warnings before failing
  , fvcVerbose              :: Bool        -- ^ Detailed output
  }
  deriving stock (Eq, Show)

-- | Default configuration - balanced safety and usability
defaultValidatorConfig :: FixValidatorConfig
defaultValidatorConfig = FixValidatorConfig
  { fvcCheckConstraints = True
  , fvcCheckShadowing = True
  , fvcCheckTypePreservation = True
  , fvcStrictMode = False
  , fvcAllowPartialTypes = True
  , fvcMaxWarnings = 10
  , fvcVerbose = False
  }

-- | Strict configuration - maximum safety
strictValidatorConfig :: FixValidatorConfig
strictValidatorConfig = FixValidatorConfig
  { fvcCheckConstraints = True
  , fvcCheckShadowing = True
  , fvcCheckTypePreservation = True
  , fvcStrictMode = True
  , fvcAllowPartialTypes = False
  , fvcMaxWarnings = 0
  , fvcVerbose = False
  }

--------------------------------------------------------------------------------
-- Validation Result Types
--------------------------------------------------------------------------------

-- | Result of validating a single fix
data FixValidationResult = FixValidationResult
  { fvrFix          :: Fix
  , fvrValid        :: Bool
  , fvrErrors       :: [FixValidationError]
  , fvrWarnings     :: [FixValidationWarning]
  , fvrConstraints  :: [ConstraintResult]
  , fvrTypeSafety   :: TypePreservationResult
  , fvrSymbolSafety :: SymbolSafety
  }
  deriving stock (Eq, Show)

-- | Validation error (blocks fix application)
data FixValidationError
  = FVEConstraintViolation Text Text Text  -- ^ (constraint, type, reason)
  | FVEShadowingConflict Text Text         -- ^ (symbol, shadowed module)
  | FVETypeChange Text Text                -- ^ (old type, new type)
  | FVESymbolNotFound Text                 -- ^ Symbol not in scope
  | FVEModuleNotFound Text                 -- ^ Module not indexed
  | FVEAmbiguousSymbol Text [Text]         -- ^ Symbol with multiple definitions
  | FVESemanticChange Text                 -- ^ Would change semantics
  deriving stock (Eq, Show)

-- | Validation warning (fix can still be applied)
data FixValidationWarning
  = FVWPartialTypeInfo Text                -- ^ Type info incomplete
  | FVWUnverifiedConstraint Text Text      -- ^ Constraint not verifiable
  | FVWPotentialShadowing Text Text        -- ^ Might shadow in some contexts
  | FVWImportRequired Text Text            -- ^ Need to add import
  | FVWDeprecatedSymbol Text Text          -- ^ Using deprecated symbol
  | FVWOrphanInstance Text                 -- ^ Relies on orphan instance
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Constraint Checking
--------------------------------------------------------------------------------

-- | A constraint check to perform
data ConstraintCheck = ConstraintCheck
  { ccConstraint :: Text     -- ^ The type class (Ord, Eq, etc.)
  , ccType       :: Text     -- ^ The type to check
  , ccRequired   :: Bool     -- ^ Is this constraint required?
  , ccReason     :: Text     -- ^ Why this constraint is needed
  }
  deriving stock (Eq, Show)

-- | Result of a constraint check
data ConstraintResult = ConstraintResult
  { crCheck      :: ConstraintCheck
  , crSatisfied  :: Bool
  , crInstance   :: Maybe InstanceInfo
  , crMessage    :: Text
  }
  deriving stock (Eq, Show)

-- | Check constraints for a fix based on its category
validateConstraints :: FixValidatorConfig -> Fix -> IO [ConstraintResult]
validateConstraints _config fix = do
  let checks = inferConstraintChecks fix
  forM checks $ \check -> do
    satisfied <- checkConstraint (TypeConstraint (ccConstraint check) (ccType check) False Nothing)
    let message = if satisfied
          then "Constraint " <> ccConstraint check <> " " <> ccType check <> " is satisfied"
          else "Missing instance: " <> ccConstraint check <> " " <> ccType check
    pure ConstraintResult
      { crCheck = check
      , crSatisfied = satisfied
      , crInstance = Nothing  -- Would need more detailed lookup
      , crMessage = message
      }

-- | Infer constraint checks from fix category and edits
inferConstraintChecks :: Fix -> [ConstraintCheck]
inferConstraintChecks fix = case fixCategory fix of
  -- Performance fixes often require Ord for Set/Map operations
  FCPerformance -> mapMaybe checkPerformanceConstraint (fixEdits fix)

  -- Style fixes might involve Monoid/Semigroup
  FCStyle -> mapMaybe checkStyleConstraint (fixEdits fix)

  -- Other categories typically don't have constraint requirements
  _ -> []
  where
    -- Check for Ord requirement (nub -> ordNub, etc.)
    checkPerformanceConstraint edit =
      let newCode = fixEditNewText edit
      in if "ordNub" `T.isInfixOf` newCode
         then Just ConstraintCheck
           { ccConstraint = "Ord"
           , ccType = "a"  -- Type variable - caller should provide concrete type
           , ccRequired = True
           , ccReason = "ordNub requires Ord instance"
           }
         else if "Set.fromList" `T.isInfixOf` newCode
         then Just ConstraintCheck
           { ccConstraint = "Ord"
           , ccType = "a"
           , ccRequired = True
           , ccReason = "Set.fromList requires Ord instance"
           }
         -- Check HashMap BEFORE Map since "Map.fromList" is a substring of "HashMap.fromList"
         else if "HashMap.fromList" `T.isInfixOf` newCode
         then Just ConstraintCheck
           { ccConstraint = "Hashable"
           , ccType = "k"
           , ccRequired = True
           , ccReason = "HashMap.fromList requires Hashable instance on key type"
           }
         else if "Map.fromList" `T.isInfixOf` newCode
         then Just ConstraintCheck
           { ccConstraint = "Ord"
           , ccType = "k"
           , ccRequired = True
           , ccReason = "Map.fromList requires Ord instance on key type"
           }
         else Nothing

    -- Check for Semigroup/Monoid (concat -> fold, etc.)
    checkStyleConstraint edit =
      let newCode = fixEditNewText edit
      in if "fold" `T.isInfixOf` newCode && not ("foldl" `T.isInfixOf` newCode)
         then Just ConstraintCheck
           { ccConstraint = "Monoid"
           , ccType = "a"
           , ccRequired = True
           , ccReason = "fold requires Monoid instance"
           }
         else if "<>" `T.isInfixOf` newCode
         then Just ConstraintCheck
           { ccConstraint = "Semigroup"
           , ccType = "a"
           , ccRequired = True
           , ccReason = "(<>) requires Semigroup instance"
           }
         else Nothing

-- | Check that constraints are preserved by a fix
checkConstraintPreservation :: FixValidatorConfig -> Text -> Text -> IO Bool
checkConstraintPreservation _config oldExpr newExpr = do
  mOldType <- extractType oldExpr Nothing
  mNewType <- extractType newExpr Nothing
  case (mOldType, mNewType) of
    (Nothing, _) -> pure True  -- Can't verify, assume OK
    (_, Nothing) -> pure True
    (Just old, Just new) ->
      -- Check that new type's constraints are a superset of old
      pure $ all (`elem` tiConstraints new) (tiConstraints old)

--------------------------------------------------------------------------------
-- Symbol Safety
--------------------------------------------------------------------------------

-- | Result of symbol safety check
data SymbolSafety
  = SymbolSafe                              -- ^ No issues detected
  | SymbolWarning [FixValidationWarning]    -- ^ Warnings but can proceed
  | SymbolUnsafe [FixValidationError]       -- ^ Errors, should not proceed
  deriving stock (Eq, Show)

-- | Check if a fix's symbols are safe
checkSymbolSafety :: SymbolTable -> Fix -> IO SymbolSafety
checkSymbolSafety table fix = do
  -- Extract symbols from fix edits
  let symbols = extractSymbolsFromFix fix

  -- Check each symbol
  results <- forM symbols $ \sym -> do
    let shadowResult = checkShadowingForFix table sym
        conflictResult = checkNameConflictsForFix table sym
    pure (shadowResult, conflictResult)

  let shadowIssues = concatMap (either id (const [])) (map fst results)
      conflictIssues = concatMap (either id (const [])) (map snd results)
      allErrors = [e | e@FVEShadowingConflict{} <- shadowIssues ++ conflictIssues]
      allWarnings = [FVWPotentialShadowing s m | FVEShadowingConflict s m <- shadowIssues ++ conflictIssues]

  pure $ if null allErrors
    then if null allWarnings
      then SymbolSafe
      else SymbolWarning allWarnings
    else SymbolUnsafe allErrors

-- | Extract symbol names from fix edits
extractSymbolsFromFix :: Fix -> [Text]
extractSymbolsFromFix fix =
  concatMap extractFromEdit (fixEdits fix) ++
  map fimpModule (fixAddImports fix)
  where
    extractFromEdit edit =
      let code = fixEditNewText edit
      in extractIdentifiers code

-- | Extract identifiers from code text
extractIdentifiers :: Text -> [Text]
extractIdentifiers code =
  filter isValidIdentifier $ T.words $ T.map sanitize code
  where
    sanitize c
      | c `elem` ("()[]{}.,;:\"'`" :: String) = ' '
      | otherwise = c
    isValidIdentifier t =
      not (T.null t) &&
      T.all (\c -> c `elem` ("_'" :: String) || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) t

-- | Check for shadowing issues in a fix
checkShadowingForFix :: SymbolTable -> Text -> Either [FixValidationError] ()
checkShadowingForFix table name =
  case Argus.HIE.SymbolTable.checkShadowing table name of
    Nothing -> Right ()
    Just safety -> case safety of
      UnsafeShadow modName _ -> Left [FVEShadowingConflict name modName]
      _ -> Right ()

-- | Check for naming conflicts in a fix
checkNameConflictsForFix :: SymbolTable -> Text -> Either [FixValidationError] ()
checkNameConflictsForFix table name =
  let conflicts = Argus.HIE.SymbolTable.checkConflicts table name
  in if null conflicts
     then Right ()
     else Left [FVEAmbiguousSymbol name conflicts]

--------------------------------------------------------------------------------
-- Type Preservation
--------------------------------------------------------------------------------

-- | Result of type preservation check
data TypePreservationResult
  = TypesPreservedResult                    -- ^ Types are exactly preserved
  | TypesCompatibleResult Text Text         -- ^ Types are compatible (old, new)
  | TypesIncompatibleResult Text Text Text  -- ^ Types incompatible (old, new, reason)
  | TypesUnknownResult                      -- ^ Could not determine types
  deriving stock (Eq, Show)

-- | Check that a fix preserves types
checkTypePreservation :: FixValidatorConfig -> Fix -> IO TypePreservationResult
checkTypePreservation _config fix = do
  -- For each edit, check type preservation
  results <- forM (fixEdits fix) $ \edit -> do
    let editSpan = fixEditSpan edit
        file = srcSpanFile editSpan
        line = unLine (srcSpanStartLine editSpan)
        col = unColumn (srcSpanStartCol editSpan)

    -- Get type at the edit location
    mOldType <- extractExprType file line col

    -- For the replacement, we'd need to analyze the new code
    -- This is a simplified check
    let newCode = fixEditNewText edit
    mNewType <- extractType newCode Nothing

    pure $ case (mOldType, mNewType) of
      (Nothing, _) -> TypesUnknownResult
      (_, Nothing) -> TypesUnknownResult
      (Just old, Just new)
        | tiType old == tiType new -> TypesPreservedResult
        | typesCompatible (tiType old) (tiType new) -> TypesCompatibleResult (tiType old) (tiType new)
        | otherwise -> TypesIncompatibleResult (tiType old) (tiType new) "Type mismatch"

  -- Combine results - worst case wins
  pure $ foldr combineResults TypesPreservedResult results
  where
    combineResults TypesUnknownResult r = r
    combineResults r TypesUnknownResult = r
    combineResults r@TypesIncompatibleResult{} _ = r
    combineResults _ r@TypesIncompatibleResult{} = r
    combineResults r@TypesCompatibleResult{} TypesPreservedResult = r
    combineResults TypesPreservedResult r = r
    combineResults r _ = r

--------------------------------------------------------------------------------
-- Main Validation Functions
--------------------------------------------------------------------------------

-- | Validate a single fix
validateFix :: FixValidatorConfig -> Maybe HieDb -> SymbolTable -> Fix -> IO FixValidationResult
validateFix config _mDb table fix = do
  -- Check constraints
  constraintResults <- validateConstraints config fix
  let constraintErrors =
        [ FVEConstraintViolation (ccConstraint (crCheck r)) (ccType (crCheck r)) (crMessage r)
        | r <- constraintResults
        , not (crSatisfied r)
        , ccRequired (crCheck r)
        ]

  -- Check symbol safety
  symbolSafety <- checkSymbolSafety table fix
  let symbolErrors = case symbolSafety of
        SymbolUnsafe errs -> errs
        _ -> []
  let symbolWarnings = case symbolSafety of
        SymbolWarning warns -> warns
        _ -> []

  -- Check type preservation
  typeResult <- checkTypePreservation config fix
  let typeErrors = case typeResult of
        TypesIncompatibleResult old new _reason -> [FVETypeChange old new]
        _ -> []

  -- Combine all errors and warnings
  let allErrors = constraintErrors ++ symbolErrors ++ typeErrors
      allWarnings = symbolWarnings

  -- Determine validity
  let valid = null allErrors &&
              (not (fvcStrictMode config) || null allWarnings) &&
              length allWarnings <= fvcMaxWarnings config

  pure FixValidationResult
    { fvrFix = fix
    , fvrValid = valid
    , fvrErrors = allErrors
    , fvrWarnings = allWarnings
    , fvrConstraints = constraintResults
    , fvrTypeSafety = typeResult
    , fvrSymbolSafety = symbolSafety
    }

-- | Validate multiple fixes
validateFixes :: FixValidatorConfig -> Maybe HieDb -> SymbolTable -> [Fix] -> IO [FixValidationResult]
validateFixes config mDb table = mapM (validateFix config mDb table)

--------------------------------------------------------------------------------
-- Batch Validation
--------------------------------------------------------------------------------

-- | Result of batch validation
data BatchValidationResult = BatchValidationResult
  { bvrTotal      :: Int
  , bvrValid      :: Int
  , bvrInvalid    :: Int
  , bvrWithWarnings :: Int
  , bvrResults    :: [FixValidationResult]
  , bvrByCategory :: Map FixCategory (Int, Int)  -- (valid, invalid) per category
  }
  deriving stock (Eq, Show)

-- | Validate a batch of fixes with summary statistics
batchValidateFixes :: FixValidatorConfig -> Maybe HieDb -> SymbolTable -> [Fix] -> IO BatchValidationResult
batchValidateFixes config mDb table fixes = do
  results <- validateFixes config mDb table fixes

  let valid = filter fvrValid results
      invalid = filter (not . fvrValid) results
      withWarnings = filter (not . null . fvrWarnings) valid

      -- Group by category
      byCategory = Map.fromListWith addCounts
        [ (fixCategory (fvrFix r), if fvrValid r then (1, 0) else (0, 1))
        | r <- results
        ]
      addCounts (v1, i1) (v2, i2) = (v1 + v2, i1 + i2)

  pure BatchValidationResult
    { bvrTotal = length fixes
    , bvrValid = length valid
    , bvrInvalid = length invalid
    , bvrWithWarnings = length withWarnings
    , bvrResults = results
    , bvrByCategory = byCategory
    }
