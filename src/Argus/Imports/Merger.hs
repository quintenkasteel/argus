{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Imports.Merger
-- Description : Import merging and deduplication system
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides automatic detection and merging of duplicate imports.
-- It supports:
--
-- * Detecting imports from the same module that can be merged
-- * Generating fixes to consolidate import statements
-- * Preserving import attributes (qualified, aliases, packages)
--
-- == Usage
--
-- @
-- -- Analyze a file for mergeable imports
-- result <- analyzeImportsForMerging source
--
-- -- Generate merge fixes
-- let fixes = generateMergeFixes result
--
-- -- Apply fixes to merge imports
-- forM_ fixes $ \\fix -> applyFix sourceText fix
-- @
module Argus.Imports.Merger
  ( -- * Configuration
    MergerConfig (..)
  , defaultMergerConfig

    -- * Analysis Types
  , MergeAnalysis (..)
  , MergeGroup (..)
  , MergeStrategy (..)

    -- * Analysis Functions
  , analyzeImportsForMerging
  , findMergeableGroups
  , canMergeImports

    -- * Fix Generation
  , generateMergeFixes
  , generateMergeGroupFix
  , mergeImportGroup

    -- * Utilities
  , consolidateImports
  , sortImports
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.List (sortBy, groupBy, nubBy)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Argus.Types
  ( Fix(..)
  , FixEdit(..)
  , FixCategory(..)
  , FixSafety(..)
  , SrcSpan(..)
  )
import Argus.Imports.Manager (ParsedImport(..), parseImports, renderImport)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for import merging
data MergerConfig = MergerConfig
  { mcMergeDuplicates       :: Bool    -- ^ Merge imports from same module
  , mcPreserveQualifiedSep  :: Bool    -- ^ Keep qualified imports separate
  , mcSortImports           :: Bool    -- ^ Sort imports after merging
  , mcAlphabetizeSymbols    :: Bool    -- ^ Alphabetize explicit import lists
  , mcMaxSymbolsPerLine     :: Int     -- ^ Max symbols before multiline format
  , mcExcludedModules       :: [Text]  -- ^ Modules to never merge
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration for import merging
defaultMergerConfig :: MergerConfig
defaultMergerConfig = MergerConfig
  { mcMergeDuplicates = True
  , mcPreserveQualifiedSep = True
  , mcSortImports = True
  , mcAlphabetizeSymbols = True
  , mcMaxSymbolsPerLine = 5
  , mcExcludedModules = []
  }

--------------------------------------------------------------------------------
-- Analysis Types
--------------------------------------------------------------------------------

-- | Strategy for merging imports
data MergeStrategy
  = MSCombineExplicit     -- ^ Combine explicit import lists
  | MSKeepFirst           -- ^ Keep first import, discard rest
  | MSKeepMostSpecific    -- ^ Keep most specific (explicit over open)
  | MSNoMerge             -- ^ Cannot merge (e.g., conflicting attributes)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A group of imports that can be merged
data MergeGroup = MergeGroup
  { mgModule      :: Text               -- ^ Module name
  , mgImports     :: NonEmpty ParsedImport  -- ^ Imports to merge
  , mgStrategy    :: MergeStrategy      -- ^ Recommended merge strategy
  , mgMerged      :: ParsedImport       -- ^ Result of merging
  , mgSpans       :: [SrcSpan]          -- ^ Spans of imports to remove
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Complete analysis of imports for merging
data MergeAnalysis = MergeAnalysis
  { maFile            :: FilePath        -- ^ Source file
  , maOriginalImports :: [ParsedImport]  -- ^ Original imports
  , maMergeGroups     :: [MergeGroup]    -- ^ Groups that can be merged
  , maUnchanged       :: [ParsedImport]  -- ^ Imports that don't need merging
  , maResultImports   :: [ParsedImport]  -- ^ Final merged imports
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Analysis Functions
--------------------------------------------------------------------------------

-- | Analyze source text for imports that can be merged
analyzeImportsForMerging :: MergerConfig -> FilePath -> Text -> MergeAnalysis
analyzeImportsForMerging config filePath source =
  let imports = parseImports source
      groups = findMergeableGroups config imports
      unchanged = findUnchangedImports imports groups
      merged = map mgMerged groups ++ unchanged
      sorted = if mcSortImports config
               then sortImports merged
               else merged
  in MergeAnalysis
    { maFile = filePath
    , maOriginalImports = imports
    , maMergeGroups = groups
    , maUnchanged = unchanged
    , maResultImports = sorted
    }

-- | Find groups of imports that can be merged
findMergeableGroups :: MergerConfig -> [ParsedImport] -> [MergeGroup]
findMergeableGroups config imports =
  let -- Group by module name
      grouped = groupByModule imports

      -- Filter to groups with > 1 import
      multipleImports = filter ((> 1) . length) grouped

      -- Create merge groups
      mergeGroups = mapMaybe (createMergeGroup config) multipleImports

  in mergeGroups

-- | Group imports by module name, qualification status, and alias
-- Only groups imports that can be safely merged (same module + same style)
groupByModule :: [ParsedImport] -> [[ParsedImport]]
groupByModule = groupBy sameImportStyle . sortBy (comparing importKey)
  where
    -- Key for sorting: module name, then qualified status, then alias
    importKey :: ParsedImport -> (Text, Bool, Maybe Text)
    importKey pImport = (piModule pImport, piQualified pImport, piAlias pImport)

    -- Only group imports with same module, qualification, and alias
    sameImportStyle :: ParsedImport -> ParsedImport -> Bool
    sameImportStyle a b = piModule a == piModule b
                       && piQualified a == piQualified b
                       && piAlias a == piAlias b

-- | Create a merge group if imports can be merged
createMergeGroup :: MergerConfig -> [ParsedImport] -> Maybe MergeGroup
createMergeGroup _ [] = Nothing
createMergeGroup config (i:is)
  -- Skip excluded modules
  | piModule i `elem` mcExcludedModules config = Nothing
  -- Check if all imports can be merged
  | otherwise =
      let nonEmpty = i :| is
          strategy = determineMergeStrategy config nonEmpty
      in case strategy of
        MSNoMerge -> Nothing
        _ ->
          let merged = mergeImportGroup config nonEmpty
              spans = mapMaybe piSpan (NE.toList nonEmpty)
          in Just MergeGroup
            { mgModule = piModule i
            , mgImports = nonEmpty
            , mgStrategy = strategy
            , mgMerged = merged
            , mgSpans = spans
            }

-- | Determine the merge strategy for a group of imports
determineMergeStrategy :: MergerConfig -> NonEmpty ParsedImport -> MergeStrategy
determineMergeStrategy config imports
  -- If qualified imports should be separate and group has mixed qualified
  | mcPreserveQualifiedSep config && hasMixedQualified imports = MSNoMerge
  -- If imports have conflicting hiding attributes
  | hasConflictingHiding imports = MSNoMerge
  -- If imports have conflicting aliases
  | hasConflictingAliases imports = MSNoMerge
  -- If all have explicit imports, combine them
  | all hasExplicitImports (NE.toList imports) = MSCombineExplicit
  -- If some are open, keep the most specific
  | otherwise = MSKeepMostSpecific
  where
    hasExplicitImports :: ParsedImport -> Bool
    hasExplicitImports imp = not (null (piExplicit imp)) && not (piHiding imp)

-- | Check if imports have mixed qualified/unqualified
hasMixedQualified :: NonEmpty ParsedImport -> Bool
hasMixedQualified imports =
  let qs = map piQualified (NE.toList imports)
  in any id qs && not (all id qs)

-- | Check if imports have conflicting hiding
hasConflictingHiding :: NonEmpty ParsedImport -> Bool
hasConflictingHiding imports =
  let hs = map piHiding (NE.toList imports)
  in any id hs && not (all id hs)

-- | Check if imports have conflicting aliases
hasConflictingAliases :: NonEmpty ParsedImport -> Bool
hasConflictingAliases imports =
  let aliases = mapMaybe piAlias (NE.toList imports)
  in length aliases > 1 && length (nubBy (==) aliases) > 1

-- | Check if two imports can be merged
canMergeImports :: MergerConfig -> ParsedImport -> ParsedImport -> Bool
canMergeImports config a b
  | piModule a /= piModule b = False
  | piModule a `elem` mcExcludedModules config = False
  | mcPreserveQualifiedSep config && piQualified a /= piQualified b = False
  | piHiding a /= piHiding b = False
  | case (piAlias a, piAlias b) of
      (Just x, Just y) -> x /= y
      _ -> False = False
  | otherwise = True

-- | Find imports that don't need merging
findUnchangedImports :: [ParsedImport] -> [MergeGroup] -> [ParsedImport]
findUnchangedImports imports groups =
  let mergedModules = map mgModule groups
  in filter (\i -> piModule i `notElem` mergedModules) imports

--------------------------------------------------------------------------------
-- Merging Implementation
--------------------------------------------------------------------------------

-- | Merge a group of imports into one
-- NOTE: This assumes all imports in the group have the same qualification and alias
-- (as ensured by groupByModule). We only merge explicit symbol lists.
mergeImportGroup :: MergerConfig -> NonEmpty ParsedImport -> ParsedImport
mergeImportGroup config imports =
  let base = NE.head imports
      -- Combine explicit lists
      allExplicit = concatMap piExplicit (NE.toList imports)
      sortedExplicit = if mcAlphabetizeSymbols config
                       then nubBy (==) (sortBy compare allExplicit)
                       else nubBy (==) allExplicit
      -- Take first span
      span' = piSpan base
  in base
    { piExplicit = sortedExplicit
    , piSpan = span'
    -- piQualified and piAlias are preserved from base (all in group have same values)
    }

-- | Consolidate all imports in a file (high-level function)
consolidateImports :: MergerConfig -> Text -> Text
consolidateImports config source =
  let imports = parseImports source
      grouped = groupByModuleNonEmpty imports
      merged = map (mergeImportGroup config) grouped
      sorted = if mcSortImports config
               then sortImports merged
               else merged
  in T.unlines $ map renderImport sorted
  where
    groupByModuleNonEmpty :: [ParsedImport] -> [NonEmpty ParsedImport]
    groupByModuleNonEmpty = mapMaybe NE.nonEmpty . groupByModule

-- | Sort imports alphabetically by module name
sortImports :: [ParsedImport] -> [ParsedImport]
sortImports = sortBy (comparing piModule)

--------------------------------------------------------------------------------
-- Fix Generation
--------------------------------------------------------------------------------

-- | Generate fixes for all merge groups
generateMergeFixes :: MergeAnalysis -> [Fix]
generateMergeFixes analysis =
  mapMaybe generateMergeGroupFix (maMergeGroups analysis)

-- | Generate a fix for a single merge group
generateMergeGroupFix :: MergeGroup -> Maybe Fix
generateMergeGroupFix mg
  | length (NE.toList (mgImports mg)) < 2 = Nothing
  | null (mgSpans mg) = Nothing
  | otherwise = Just Fix
    { fixTitle = "Merge " <> T.pack (show (length (NE.toList (mgImports mg)))) <>
                 " imports from '" <> mgModule mg <> "'"
    , fixEdits = createMergeEdits mg
    , fixIsPreferred = True
    , fixAddImports = []
    , fixRemoveImports = []
    , fixCategory = FCImports
    , fixSafety = FSAlways
    }

-- | Create the edits needed for a merge
createMergeEdits :: MergeGroup -> [FixEdit]
createMergeEdits mg =
  case mgSpans mg of
    [] -> []
    (firstSpan:restSpans) ->
      let -- Replace first import with merged version
          mergedText = renderImport (mgMerged mg)
          replaceFirst = FixEdit
            { fixEditSpan = firstSpan
            , fixEditNewText = mergedText
            }
          -- Remove remaining imports
          removeRest = map (\s -> FixEdit { fixEditSpan = s, fixEditNewText = "" }) restSpans
      in replaceFirst : removeRest
