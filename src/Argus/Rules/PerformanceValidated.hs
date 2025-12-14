{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.PerformanceValidated
-- Description : Type-validated performance fixes using HIE information
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides type-validated performance fixes that use HIE
-- information to verify that fixes are safe to apply. It ensures that
-- fixes like nub → ordNub only apply when the Ord constraint is satisfied.
module Argus.Rules.PerformanceValidated
  ( -- * Validated Fix Generation
    generateValidatedPerfFixes
  , ValidatedPerfFix (..)
  , PerfFixContext (..)

    -- * Specific Fix Generators
  , generateNubFixes
  , generateSetFixes
  , generateHashMapFixes
  , generateFoldFixes

    -- * Constraint Checking
  , checkOrdConstraint
  , checkHashableConstraint
  , checkMonoidConstraint
  , checkEqConstraint

    -- * Fix Application
  , applyValidatedFix
  , ValidationMode (..)
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (catMaybes, listToMaybe)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import HieDb (HieDb)

import Argus.Types
import Argus.HIE.Types
import Argus.HIE.TypeInfo
import Argus.HIE.SymbolTable

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Context for generating validated performance fixes
data PerfFixContext = PerfFixContext
  { pfcDb         :: Maybe HieDb          -- ^ HIE database for type info
  , pfcSymTable   :: SymbolTable          -- ^ Symbol table for scope
  , pfcFilePath   :: FilePath             -- ^ Current file being analyzed
  , pfcContent    :: Text                 -- ^ File content
  , pfcStrict     :: Bool                 -- ^ Strict validation mode
  }

-- | A validated performance fix with constraint information
data ValidatedPerfFix = ValidatedPerfFix
  { vpfFix            :: Fix                  -- ^ The underlying fix
  , vpfConstraints    :: [ConstraintInfo]     -- ^ Required constraints
  , vpfValidated      :: Bool                 -- ^ Whether constraints are verified
  , vpfValidationMsg  :: Text                 -- ^ Validation message
  , vpfAlternatives   :: [Fix]                -- ^ Alternative fixes if primary fails
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Information about a required constraint
data ConstraintInfo = ConstraintInfo
  { ciClass      :: Text              -- ^ Type class (Ord, Eq, Hashable, etc.)
  , ciType       :: Text              -- ^ Type that needs the instance
  , ciSatisfied  :: Bool              -- ^ Whether instance exists
  , ciSource     :: Text              -- ^ Where instance comes from
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Validation mode for fixes
data ValidationMode
  = VMStrict     -- ^ Reject fixes if constraints can't be verified
  | VMLenient    -- ^ Allow fixes with warnings if constraints unclear
  | VMUnsafe     -- ^ Allow all fixes regardless of constraints
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Main Fix Generation
--------------------------------------------------------------------------------

-- | Generate validated performance fixes for a file
generateValidatedPerfFixes :: PerfFixContext -> [Diagnostic] -> IO [ValidatedPerfFix]
generateValidatedPerfFixes ctx diags = do
  -- Generate fixes for each diagnostic
  fixes <- concat <$> mapM (generateFixesForDiag ctx) diags
  pure fixes

-- | Generate validated fixes for a single diagnostic
generateFixesForDiag :: PerfFixContext -> Diagnostic -> IO [ValidatedPerfFix]
generateFixesForDiag ctx diag = case diagCode diag of
  Just code
    | "performance/algorithm" `T.isInfixOf` code -> generateAlgorithmFixes ctx diag
    | "performance/data-structure" `T.isInfixOf` code -> generateDataStructureFixes ctx diag
    | "performance/allocation" `T.isInfixOf` code -> generateAllocationFixes ctx diag
    | "space-leak/" `T.isInfixOf` code -> generateSpaceLeakFixes ctx diag
    | otherwise -> pure []
  Nothing -> pure []

--------------------------------------------------------------------------------
-- Algorithm Fixes (nub, sort patterns)
--------------------------------------------------------------------------------

-- | Generate validated fixes for algorithmic issues
generateAlgorithmFixes :: PerfFixContext -> Diagnostic -> IO [ValidatedPerfFix]
generateAlgorithmFixes ctx diag = do
  let code = extractCodeFromDiag diag
      srcSpan' = diagSpan diag

  -- Check for nub pattern
  nubFixes <- if "nub " `T.isInfixOf` code || " nub" `T.isInfixOf` code
    then generateNubFixes ctx srcSpan' code
    else pure []

  -- Check for sort patterns
  sortFixes <- if "sort " `T.isInfixOf` code
    then generateSortFixes ctx srcSpan' code
    else pure []

  pure $ nubFixes ++ sortFixes

-- | Generate validated fixes for nub (O(n²)) → ordNub/hashNub (O(n log n) or O(n))
generateNubFixes :: PerfFixContext -> SrcSpan -> Text -> IO [ValidatedPerfFix]
generateNubFixes ctx srcSpan' code = do
  -- Try to extract the list element type
  mListType <- extractListElemType ctx srcSpan'

  -- Generate ordNub fix (requires Ord)
  ordNubFix <- generateOrdNubFix ctx srcSpan' code mListType

  -- Generate hashNub fix (requires Hashable)
  hashNubFix <- generateHashNubFix ctx srcSpan' code mListType

  -- Generate Set-based fix (requires Ord)
  setFix <- generateSetFromListFix ctx srcSpan' code mListType

  pure $ catMaybes [ordNubFix, hashNubFix, setFix]

-- | Generate ordNub fix with Ord constraint check
generateOrdNubFix :: PerfFixContext -> SrcSpan -> Text -> Maybe Text -> IO (Maybe ValidatedPerfFix)
generateOrdNubFix ctx srcSpan' code mListType = do
  -- Check Ord constraint
  (satisfied, source) <- checkOrdConstraint ctx mListType

  let adjustedSpan = adjustSpanForNub srcSpan' code

      baseFix = Fix
        { fixTitle = "Use ordNub for O(n log n) performance"
        , fixEdits = [FixEdit adjustedSpan "ordNub"]
        , fixIsPreferred = satisfied
        , fixAddImports = [FixImport
            { fimpModule = "Data.List.Extra"
            , fimpSymbols = [ImportSymbol "ordNub" ISTFunction []]
            , fimpQualified = Nothing
            , fimpHiding = False
            , fimpPackage = Just "extra"
            }]
        , fixRemoveImports = []
        , fixCategory = FCPerformance
        , fixSafety = if satisfied then FSMostly else FSReview
        }

      validationMsg = if satisfied
        then "Ord constraint verified: " <> source
        else "Ord constraint could not be verified" <>
             maybe "" (\t -> " for type: " <> t) mListType

  pure $ Just ValidatedPerfFix
    { vpfFix = baseFix
    , vpfConstraints = [ConstraintInfo "Ord" (maybe "a" id mListType) satisfied source]
    , vpfValidated = satisfied
    , vpfValidationMsg = validationMsg
    , vpfAlternatives = []
    }

-- | Generate hashNub fix with Hashable constraint check
generateHashNubFix :: PerfFixContext -> SrcSpan -> Text -> Maybe Text -> IO (Maybe ValidatedPerfFix)
generateHashNubFix ctx srcSpan' code mListType = do
  -- Check Hashable constraint
  (satisfied, source) <- checkHashableConstraint ctx mListType

  let adjustedSpan = adjustSpanForNub srcSpan' code

      baseFix = Fix
        { fixTitle = "Use hashNub for O(n) performance (requires Hashable)"
        , fixEdits = [FixEdit adjustedSpan "hashNub"]
        , fixIsPreferred = False  -- Not preferred unless explicitly verified
        , fixAddImports = [FixImport
            { fimpModule = "Data.List.Extra"
            , fimpSymbols = [ImportSymbol "hashNub" ISTFunction []]
            , fimpQualified = Nothing
            , fimpHiding = False
            , fimpPackage = Just "extra"
            }]
        , fixRemoveImports = []
        , fixCategory = FCPerformance
        , fixSafety = if satisfied then FSMostly else FSReview
        }

      validationMsg = if satisfied
        then "Hashable constraint verified: " <> source
        else "Hashable constraint not verified" <>
             maybe "" (\t -> " for type: " <> t) mListType

  pure $ Just ValidatedPerfFix
    { vpfFix = baseFix
    , vpfConstraints = [ConstraintInfo "Hashable" (maybe "a" id mListType) satisfied source]
    , vpfValidated = satisfied
    , vpfValidationMsg = validationMsg
    , vpfAlternatives = []
    }

-- | Generate Set.toList . Set.fromList fix
generateSetFromListFix :: PerfFixContext -> SrcSpan -> Text -> Maybe Text -> IO (Maybe ValidatedPerfFix)
generateSetFromListFix ctx srcSpan' _code mListType = do
  -- Check Ord constraint (required for Set)
  (satisfied, source) <- checkOrdConstraint ctx mListType

  let baseFix = Fix
        { fixTitle = "Use Set.toList . Set.fromList for O(n log n)"
        , fixEdits = [FixEdit srcSpan' "Set.toList . Set.fromList"]
        , fixIsPreferred = False
        , fixAddImports = [FixImport
            { fimpModule = "Data.Set"
            , fimpSymbols = []
            , fimpQualified = Just "Set"
            , fimpHiding = False
            , fimpPackage = Nothing
            }]
        , fixRemoveImports = []
        , fixCategory = FCPerformance
        , fixSafety = FSReview  -- Changes semantics (removes order)
        }

  if satisfied
    then pure $ Just ValidatedPerfFix
      { vpfFix = baseFix
      , vpfConstraints = [ConstraintInfo "Ord" (maybe "a" id mListType) satisfied source]
      , vpfValidated = satisfied
      , vpfValidationMsg = "Ord constraint verified, but note: this may change element order"
      , vpfAlternatives = []
      }
    else pure Nothing

-- | Generate sort-related fixes
generateSortFixes :: PerfFixContext -> SrcSpan -> Text -> IO [ValidatedPerfFix]
generateSortFixes _ctx _srcSpan _code = pure []  -- Sorts already have Ord requirement

--------------------------------------------------------------------------------
-- Data Structure Fixes
--------------------------------------------------------------------------------

-- | Generate validated fixes for data structure issues
generateDataStructureFixes :: PerfFixContext -> Diagnostic -> IO [ValidatedPerfFix]
generateDataStructureFixes ctx diag = do
  let code = extractCodeFromDiag diag
      srcSpan' = diagSpan diag

  -- Check for list lookup patterns
  if "lookup " `T.isInfixOf` code && not ("Map.lookup" `T.isInfixOf` code)
    then generateMapFixes ctx srcSpan' code
    else pure []

-- | Generate Map-based fixes for list lookup
generateMapFixes :: PerfFixContext -> SrcSpan -> Text -> IO [ValidatedPerfFix]
generateMapFixes ctx _srcSpan _code = do
  -- Maps require Ord on keys
  (ordSatisfied, ordSource) <- checkOrdConstraint ctx Nothing

  let mapFix = Fix
        { fixTitle = "Consider using Data.Map for O(log n) lookup"
        , fixEdits = []  -- This is structural change, not simple replace
        , fixIsPreferred = False
        , fixAddImports = [FixImport
            { fimpModule = "Data.Map.Strict"
            , fimpSymbols = []
            , fimpQualified = Just "Map"
            , fimpHiding = False
            , fimpPackage = Nothing
            }]
        , fixRemoveImports = []
        , fixCategory = FCPerformance
        , fixSafety = FSReview  -- Structural change
        }

  pure [ValidatedPerfFix
    { vpfFix = mapFix
    , vpfConstraints = [ConstraintInfo "Ord" "k" ordSatisfied ordSource]
    , vpfValidated = ordSatisfied
    , vpfValidationMsg = "Map requires Ord on keys"
    , vpfAlternatives = []
    }]

-- | Generate validated fixes for Set usage
generateSetFixes :: PerfFixContext -> SrcSpan -> Text -> IO [ValidatedPerfFix]
generateSetFixes ctx srcSpan' code = do
  (ordSatisfied, ordSource) <- checkOrdConstraint ctx Nothing

  let elemToMember = if "elem " `T.isInfixOf` code
        then Just $ Fix
          { fixTitle = "Use Set.member for O(log n) lookup"
          , fixEdits = [FixEdit srcSpan' (T.replace "elem " "Set.member " code)]
          , fixIsPreferred = ordSatisfied
          , fixAddImports = [FixImport
              { fimpModule = "Data.Set"
              , fimpSymbols = []
              , fimpQualified = Just "Set"
              , fimpHiding = False
              , fimpPackage = Nothing
              }]
          , fixRemoveImports = []
          , fixCategory = FCPerformance
          , fixSafety = FSReview
          }
        else Nothing

  pure $ catMaybes
    [ fmap (\fix -> ValidatedPerfFix fix [ConstraintInfo "Ord" "a" ordSatisfied ordSource]
                                     ordSatisfied "Set requires Ord" []) elemToMember
    ]

-- | Generate validated fixes for HashMap usage
generateHashMapFixes :: PerfFixContext -> SrcSpan -> Text -> IO [ValidatedPerfFix]
generateHashMapFixes ctx _srcSpan _code = do
  (hashSatisfied, hashSource) <- checkHashableConstraint ctx Nothing

  let hashMapFix = Fix
        { fixTitle = "Use HashMap for O(1) average lookup"
        , fixEdits = []
        , fixIsPreferred = False
        , fixAddImports = [FixImport
            { fimpModule = "Data.HashMap.Strict"
            , fimpSymbols = []
            , fimpQualified = Just "HashMap"
            , fimpHiding = False
            , fimpPackage = Nothing
            }]
        , fixRemoveImports = []
        , fixCategory = FCPerformance
        , fixSafety = FSReview
        }

  pure [ValidatedPerfFix
    { vpfFix = hashMapFix
    , vpfConstraints = [ConstraintInfo "Hashable" "k" hashSatisfied hashSource]
    , vpfValidated = hashSatisfied
    , vpfValidationMsg = "HashMap requires Hashable on keys"
    , vpfAlternatives = []
    }]

--------------------------------------------------------------------------------
-- Allocation Fixes
--------------------------------------------------------------------------------

-- | Generate validated fixes for allocation issues
generateAllocationFixes :: PerfFixContext -> Diagnostic -> IO [ValidatedPerfFix]
generateAllocationFixes _ctx _diag = pure []  -- Basic allocation fixes don't need type checking

--------------------------------------------------------------------------------
-- Space Leak Fixes
--------------------------------------------------------------------------------

-- | Generate validated fixes for space leaks
generateSpaceLeakFixes :: PerfFixContext -> Diagnostic -> IO [ValidatedPerfFix]
generateSpaceLeakFixes _ctx _diag = pure []  -- Space leak fixes don't typically need constraint checking

--------------------------------------------------------------------------------
-- Fold Fixes
--------------------------------------------------------------------------------

-- | Generate validated fixes for fold patterns
generateFoldFixes :: PerfFixContext -> SrcSpan -> Text -> IO [ValidatedPerfFix]
generateFoldFixes ctx srcSpan' code = do
  -- Check for mconcat . map → foldMap (requires Monoid)
  if "mconcat" `T.isInfixOf` code && "map" `T.isInfixOf` code
    then do
      (monoidSatisfied, monoidSource) <- checkMonoidConstraint ctx Nothing
      let foldMapFix = Fix
            { fixTitle = "Use foldMap instead of mconcat . map"
            , fixEdits = [FixEdit srcSpan'
                (T.replace "mconcat . map" "foldMap" $
                 T.replace "mconcat $ map" "foldMap" code)]
            , fixIsPreferred = monoidSatisfied
            , fixAddImports = [FixImport
                { fimpModule = "Data.Foldable"
                , fimpSymbols = [ImportSymbol "foldMap" ISTFunction []]
                , fimpQualified = Nothing
                , fimpHiding = False
                , fimpPackage = Nothing
                }]
            , fixRemoveImports = []
            , fixCategory = FCPerformance
            , fixSafety = if monoidSatisfied then FSMostly else FSReview
            }

      pure [ValidatedPerfFix
        { vpfFix = foldMapFix
        , vpfConstraints = [ConstraintInfo "Monoid" "m" monoidSatisfied monoidSource]
        , vpfValidated = monoidSatisfied
        , vpfValidationMsg = "foldMap requires Monoid on result type"
        , vpfAlternatives = []
        }]
    else pure []

--------------------------------------------------------------------------------
-- Constraint Checking
--------------------------------------------------------------------------------

-- | Check if Ord constraint is satisfied for a type
checkOrdConstraint :: PerfFixContext -> Maybe Text -> IO (Bool, Text)
checkOrdConstraint _ctx mType = do
  -- Check common types that have Ord
  let ordTypes = ["Int", "Integer", "Double", "Float", "Char", "Bool", "Text", "String",
                  "Word", "Word8", "Word16", "Word32", "Word64",
                  "Int8", "Int16", "Int32", "Int64", "Natural"]
  case mType of
    Nothing -> pure (False, "Type unknown - Ord not verified")
    Just typ ->
      if any (`T.isInfixOf` typ) ordTypes
        then pure (True, "Base type has Ord instance")
        else do
          -- Check HIE for more complex types
          satisfied <- checkConstraint (TypeConstraint "Ord" typ False Nothing)
          if satisfied
            then pure (True, "Ord instance found via HIE")
            else pure (False, "Ord instance not found for " <> typ)

-- | Check if Hashable constraint is satisfied for a type
checkHashableConstraint :: PerfFixContext -> Maybe Text -> IO (Bool, Text)
checkHashableConstraint _ctx mType = do
  -- Check common types that have Hashable
  let hashTypes = ["Int", "Integer", "Double", "Float", "Char", "Bool", "Text", "String",
                   "Word", "Word8", "Word16", "Word32", "Word64",
                   "Int8", "Int16", "Int32", "Int64", "ByteString"]
  case mType of
    Nothing -> pure (False, "Type unknown - Hashable not verified")
    Just typ ->
      if any (`T.isInfixOf` typ) hashTypes
        then pure (True, "Base type has Hashable instance")
        else do
          satisfied <- checkConstraint (TypeConstraint "Hashable" typ False Nothing)
          if satisfied
            then pure (True, "Hashable instance found via HIE")
            else pure (False, "Hashable instance not found for " <> typ)

-- | Check if Monoid constraint is satisfied for a type
checkMonoidConstraint :: PerfFixContext -> Maybe Text -> IO (Bool, Text)
checkMonoidConstraint _ctx mType = do
  -- Check common types that have Monoid
  let monoidTypes = ["Text", "String", "[", "Sum", "Product", "All", "Any",
                     "First", "Last", "Endo", "Builder", "ByteString"]
  case mType of
    Nothing -> pure (False, "Type unknown - Monoid not verified")
    Just typ ->
      if any (`T.isInfixOf` typ) monoidTypes
        then pure (True, "Base type has Monoid instance")
        else do
          satisfied <- checkConstraint (TypeConstraint "Monoid" typ False Nothing)
          if satisfied
            then pure (True, "Monoid instance found via HIE")
            else pure (False, "Monoid instance not found for " <> typ)

-- | Check if Eq constraint is satisfied for a type
checkEqConstraint :: PerfFixContext -> Maybe Text -> IO (Bool, Text)
checkEqConstraint _ctx mType = do
  -- Almost everything has Eq
  let eqTypes = ["Int", "Integer", "Double", "Float", "Char", "Bool", "Text", "String",
                 "Word", "Maybe", "Either", "[", "("]
  case mType of
    Nothing -> pure (False, "Type unknown - Eq not verified")
    Just typ ->
      if any (`T.isInfixOf` typ) eqTypes
        then pure (True, "Base type has Eq instance")
        else do
          satisfied <- checkConstraint (TypeConstraint "Eq" typ False Nothing)
          if satisfied
            then pure (True, "Eq instance found via HIE")
            else pure (False, "Eq instance not found for " <> typ)

--------------------------------------------------------------------------------
-- Fix Application
--------------------------------------------------------------------------------

-- | Apply a validated fix with specified validation mode
applyValidatedFix :: ValidationMode -> ValidatedPerfFix -> Maybe Fix
applyValidatedFix mode vpf = case mode of
  VMStrict ->
    if vpfValidated vpf
      then Just (vpfFix vpf)
      else listToMaybe (vpfAlternatives vpf)
  VMLenient ->
    Just (vpfFix vpf)  -- Allow even unverified fixes
  VMUnsafe ->
    Just (vpfFix vpf)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Extract code snippet from diagnostic
extractCodeFromDiag :: Diagnostic -> Text
extractCodeFromDiag diag =
  -- The message often contains the code, or we use the span info
  T.takeWhile (/= '.') (diagMessage diag)

-- | Extract list element type from HIE at a location
extractListElemType :: PerfFixContext -> SrcSpan -> IO (Maybe Text)
extractListElemType _ctx srcSpan' = do
  let file = srcSpanFile srcSpan'
      line = unLine (srcSpanStartLine srcSpan')
      col = unColumn (srcSpanStartCol srcSpan')

  mTypeInfo <- extractExprType file line col
  case mTypeInfo of
    Nothing -> pure Nothing
    Just typeInfo ->
      -- Parse the list element type from the type signature
      let typ = tiType typeInfo
      in pure $ extractElementType typ

-- | Extract element type from a list type like "[Int]" or "Vector Text"
extractElementType :: Text -> Maybe Text
extractElementType typ
  | "[" `T.isPrefixOf` typ && "]" `T.isSuffixOf` typ =
      Just $ T.dropEnd 1 $ T.drop 1 typ
  | "Vector " `T.isPrefixOf` typ =
      Just $ T.drop 7 typ
  | "Set " `T.isPrefixOf` typ =
      Just $ T.drop 4 typ
  | otherwise = Nothing

-- | Adjust span to cover just the nub function call
adjustSpanForNub :: SrcSpan -> Text -> SrcSpan
adjustSpanForNub srcSpan' code =
  case (T.breakOn "nub " code, T.breakOn " nub " code) of
    ((before, rest), _)
      | not (T.null rest) && T.length before < 50 ->
          let startColRaw = srcSpanStartColRaw srcSpan' + T.length before
          in srcSpan'
            { srcSpanStartCol = Column startColRaw
            , srcSpanEndCol = Column (startColRaw + 3)  -- length of "nub"
            }
    (_, (before, rest))
      | not (T.null rest) ->
          let startColRaw = srcSpanStartColRaw srcSpan' + T.length before + 1
          in srcSpan'
            { srcSpanStartCol = Column startColRaw
            , srcSpanEndCol = Column (startColRaw + 3)
            }
    _ -> srcSpan'
