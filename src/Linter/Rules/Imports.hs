{-# LANGUAGE StrictData #-}

-- |
-- Module      : Linter.Rules.Imports
-- Description : Import management and linting
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module handles import-related linting, including detecting
-- unused imports, suggesting qualified imports, and managing import style.
module Linter.Rules.Imports
  ( -- * Import checking
    checkImports
  , detectUnusedImports
  , suggestQualifiedImports

    -- * Import optimization
  , optimizeImports
  , combineImports
  , sortImports
  ) where

import Data.List (sortBy, groupBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Linter.Analysis.Syntactic
import Linter.Config
import Linter.Types

--------------------------------------------------------------------------------
-- Import Checking
--------------------------------------------------------------------------------

-- | Check imports against configuration
checkImports :: ImportsConfig -> FilePath -> [ImportInfo] -> Set Text -> [Diagnostic]
checkImports cfg path imports usedNames =
  unusedDiags ++ qualifiedDiags ++ explicitDiags
  where
    unusedDiags
      | importsRemoveUnused cfg = detectUnusedImports path imports usedNames
      | otherwise = []

    qualifiedDiags = suggestQualifiedImports path (importsSuggestQualified cfg) imports

    explicitDiags
      | importsRequireExplicit cfg = checkExplicitImports path imports
      | otherwise = []

-- | Detect unused imports
detectUnusedImports :: FilePath -> [ImportInfo] -> Set Text -> [Diagnostic]
detectUnusedImports path imports usedNames = mapMaybe checkImport imports
  where
    checkImport imp = case iiExplicit imp of
      Nothing -> Nothing  -- Can't check implicit imports without more info
      Just names ->
        let unused = filter (`Set.notMember` usedNames) names
        in if null unused
           then Nothing
           else Just $ mkUnusedImportDiagnostic path imp unused

mkUnusedImportDiagnostic :: FilePath -> ImportInfo -> [Text] -> Diagnostic
mkUnusedImportDiagnostic path imp unused = Diagnostic
  { diagSpan = iiSpan imp
  , diagSeverity = Warning
  , diagKind = UnusedImport
  , diagMessage = "Unused imports from " <> iiModuleName imp <> ": " <> T.intercalate ", " unused
  , diagCode = Just "imports/unused"
  , diagFixes = []  -- Would need more context to generate proper fix
  , diagRelated = []
  }

-- | Suggest qualified imports for certain modules
suggestQualifiedImports :: FilePath -> [Text] -> [ImportInfo] -> [Diagnostic]
suggestQualifiedImports path qualifiedModules = mapMaybe checkImport
  where
    checkImport imp =
      if iiModuleName imp `elem` qualifiedModules && not (iiQualified imp)
      then Just $ mkQualifiedDiagnostic path imp
      else Nothing

mkQualifiedDiagnostic :: FilePath -> ImportInfo -> Diagnostic
mkQualifiedDiagnostic path imp = Diagnostic
  { diagSpan = iiSpan imp
  , diagSeverity = Suggestion
  , diagKind = ImportStyle
  , diagMessage = "Consider using qualified import for " <> iiModuleName imp
  , diagCode = Just "imports/suggest-qualified"
  , diagFixes = [Fix
      { fixTitle = "Make qualified"
      , fixEdits = [FixEdit (iiSpan imp) qualifiedText]
      , fixIsPreferred = True
      }]
  , diagRelated = []
  }
  where
    qualifiedText = "import qualified " <> iiModuleName imp <> alias
    alias = case iiAlias imp of
      Just a  -> " as " <> a
      Nothing -> ""

-- | Check for explicit import lists
checkExplicitImports :: FilePath -> [ImportInfo] -> [Diagnostic]
checkExplicitImports path = mapMaybe checkImport
  where
    checkImport imp =
      if iiExplicit imp == Nothing && not (iiHiding imp)
      then Just $ mkExplicitDiagnostic path imp
      else Nothing

mkExplicitDiagnostic :: FilePath -> ImportInfo -> Diagnostic
mkExplicitDiagnostic path imp = Diagnostic
  { diagSpan = iiSpan imp
  , diagSeverity = Suggestion
  , diagKind = ImportStyle
  , diagMessage = "Consider using explicit import list for " <> iiModuleName imp
  , diagCode = Just "imports/require-explicit"
  , diagFixes = []  -- Would need analysis to suggest specific imports
  , diagRelated = []
  }

--------------------------------------------------------------------------------
-- Import Optimization
--------------------------------------------------------------------------------

-- | Optimize imports by removing unused and sorting
optimizeImports :: [ImportInfo] -> Set Text -> [ImportInfo]
optimizeImports imports usedNames = sortImports $ filter isUsed imports
  where
    isUsed imp = case iiExplicit imp of
      Nothing -> True  -- Keep implicit imports
      Just names -> any (`Set.member` usedNames) names

-- | Combine fragmented imports from the same module
combineImports :: [ImportInfo] -> [ImportInfo]
combineImports imports =
  map combineGroup $ groupBy sameModule $ sortBy (comparing iiModuleName) imports
  where
    sameModule a b = iiModuleName a == iiModuleName b

    combineGroup [] = error "impossible: empty group"
    combineGroup [x] = x
    combineGroup (x:xs) = x
      { iiExplicit = case (iiExplicit x, concatMap (maybe [] id . iiExplicit) xs) of
          (Nothing, _) -> Nothing
          (Just a, b) -> Just $ a ++ b
      }

-- | Sort imports alphabetically
sortImports :: [ImportInfo] -> [ImportInfo]
sortImports = sortBy (comparing iiModuleName)
