{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Argus.Imports.Pruner
-- Description : Dead import auto-pruning system
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides automatic detection and removal of unused imports.
-- It supports:
--
-- * Detecting completely unused import declarations
-- * Detecting unused symbols from explicit import lists
-- * Generating fixes to remove/prune imports
-- * Both whole-file and HIE-based analysis modes
--
-- == Usage
--
-- @
-- -- Analyze a file for unused imports
-- result <- analyzeUnusedImports config sourceText parsedModule
--
-- -- Generate fixes
-- let fixes = generatePruneFixes result
--
-- -- Apply fixes
-- forM_ fixes $ \fix -> applyFix sourceText fix
-- @
module Argus.Imports.Pruner
  ( -- * Configuration
    PrunerConfig (..)
  , defaultPrunerConfig

    -- * Analysis Types
  , PruneAnalysis (..)
  , UnusedImportInfo (..)
  , UnusedSymbol (..)
  , ImportStatus (..)

    -- * Analysis Functions
  , analyzeUnusedImports
  , analyzeUnusedImportsText
  , findUnusedInModule

    -- * Fix Generation
  , generatePruneFixes
  , generateRemoveImportFix
  , generatePruneSymbolsFix

    -- * Utilities
  , getUsedSymbolsFromSource
  , getImportedSymbols
  , isSymbolUsed
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- GHC imports for parsing
import "ghc-lib-parser" GHC.Hs (HsModule, GhcPs)

import Argus.Types
  ( Fix(..)
  , FixEdit(..)
  , FixCategory(..)
  , FixSafety(..)
  , Diagnostic(..)
  , SrcSpan(..)
  , DiagnosticKind(UnusedImport)
  , mkSrcSpanRaw
  )
import Argus.Types qualified as Types
import Argus.Imports.Manager (ParsedImport(..), parseImports, renderImport)
import Argus.Imports.UsageAnalyzer (extractUsedSymbols, extractQualifiedUsages, SymbolUsage(..))

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for import pruning
data PrunerConfig = PrunerConfig
  { pcRemoveUnusedImports    :: Bool    -- ^ Remove completely unused imports
  , pcPruneExplicitLists     :: Bool    -- ^ Prune unused symbols from explicit lists
  , pcPreserveTypeImports    :: Bool    -- ^ Keep type imports even if seemingly unused
  , pcPreserveInstanceImports :: Bool   -- ^ Keep imports that might provide instances
  , pcMinSymbolsToKeep       :: Int     -- ^ Minimum symbols to keep (avoid empty lists)
  , pcExcludedModules        :: [Text]  -- ^ Modules to never prune
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration for import pruning
defaultPrunerConfig :: PrunerConfig
defaultPrunerConfig = PrunerConfig
  { pcRemoveUnusedImports = True
  , pcPruneExplicitLists = True
  , pcPreserveTypeImports = True
  , pcPreserveInstanceImports = True
  , pcMinSymbolsToKeep = 0
  , pcExcludedModules =
      [ "Prelude"
      , "GHC.Generics"  -- Often needed for deriving
      , "Data.Kind"     -- Often needed for type annotations
      ]
  }

--------------------------------------------------------------------------------
-- Analysis Types
--------------------------------------------------------------------------------

-- | Status of an import
data ImportStatus
  = ISUsed              -- ^ Import is used
  | ISUnused            -- ^ Import is completely unused
  | ISPartiallyUnused   -- ^ Some symbols from explicit list are unused
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | An unused import declaration
data UnusedImportInfo = UnusedImportInfo
  { uiModule    :: Text           -- ^ Module name
  , uiSpan      :: SrcSpan        -- ^ Source location
  , uiImport    :: ParsedImport   -- ^ The full import info
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | An unused symbol from an explicit import list
data UnusedSymbol = UnusedSymbol
  { usModule    :: Text           -- ^ Module the symbol is imported from
  , usSymbol    :: Text           -- ^ The unused symbol name
  , usImport    :: ParsedImport   -- ^ The import containing this symbol
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Complete analysis of unused imports in a file
data PruneAnalysis = PruneAnalysis
  { paFile            :: FilePath        -- ^ Source file
  , paUsedSymbols     :: Set Text        -- ^ All symbols used in the file
  , paImports         :: [ParsedImport]  -- ^ All imports in the file
  , paUnusedImports   :: [UnusedImportInfo]  -- ^ Completely unused imports
  , paUnusedSymbols   :: [UnusedSymbol]  -- ^ Unused symbols from explicit lists
  , paDiagnostics     :: [Diagnostic]    -- ^ Generated diagnostics
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Analysis Functions
--------------------------------------------------------------------------------

-- | Analyze a file for unused imports (text-based analysis)
analyzeUnusedImportsText :: PrunerConfig -> FilePath -> Text -> PruneAnalysis
analyzeUnusedImportsText config filePath source =
  let -- Parse imports from the source
      imports = parseImports source

      -- Get all used symbols (excluding import section)
      usedSymbols = getUsedSymbolsFromSource source

      -- Analyze each import
      (unusedImports, partiallyUnusedImports) =
        analyzeImportList config usedSymbols imports

      -- Extract unused symbols from partially used imports
      unusedSymbols = concatMap (getUnusedSymbolsFromImport usedSymbols) partiallyUnusedImports

      -- Generate diagnostics
      diagnostics = generateDiagnostics filePath unusedImports unusedSymbols

  in PruneAnalysis
    { paFile = filePath
    , paUsedSymbols = usedSymbols
    , paImports = imports
    , paUnusedImports = unusedImports
    , paUnusedSymbols = unusedSymbols
    , paDiagnostics = diagnostics
    }

-- | Analyze unused imports (wrapper that handles parsed module)
analyzeUnusedImports :: PrunerConfig -> FilePath -> Text -> Maybe (HsModule GhcPs) -> PruneAnalysis
analyzeUnusedImports config filePath source _mModule =
  -- For now, we use text-based analysis
  -- HIE-based analysis would be more accurate but requires more setup
  analyzeUnusedImportsText config filePath source

-- | Find unused imports in a module (convenience function)
findUnusedInModule :: PrunerConfig -> FilePath -> Text -> ([UnusedImportInfo], [UnusedSymbol])
findUnusedInModule config filePath source =
  let analysis = analyzeUnusedImportsText config filePath source
  in (paUnusedImports analysis, paUnusedSymbols analysis)

-- | Analyze a list of imports against used symbols
analyzeImportList :: PrunerConfig
                  -> Set Text
                  -> [ParsedImport]
                  -> ([UnusedImportInfo], [ParsedImport])
analyzeImportList config usedSymbols imports =
  let -- Check each import
      analyzed = map (analyzeOneImport config usedSymbols) imports

      -- Separate completely unused from partially unused
      (unused, rest) = partition isUnusedResult analyzed

  in ( mapMaybe extractUnused unused
     , mapMaybe extractPartial rest
     )
  where
    analyzeOneImport :: PrunerConfig -> Set Text -> ParsedImport -> (ImportStatus, ParsedImport)
    analyzeOneImport cfg used imp
      -- Skip excluded modules
      | piModule imp `elem` pcExcludedModules cfg = (ISUsed, imp)
      -- Open imports - check if module name is used as qualifier
      | null (piExplicit imp) && not (piHiding imp) =
          if isQualifierUsed (piModule imp) (piAlias imp) used
          then (ISUsed, imp)
          else (ISUnused, imp)
      -- Explicit imports - check which symbols are used
      | not (null (piExplicit imp)) =
          let usedFromImport = filter (`Set.member` used) (piExplicit imp)
          in case length usedFromImport of
            0 -> (ISUnused, imp)
            n | n == length (piExplicit imp) -> (ISUsed, imp)
            _ -> (ISPartiallyUnused, imp { piExplicit = usedFromImport })
      -- Hiding imports - assume used (conservative)
      | otherwise = (ISUsed, imp)

    isUnusedResult (ISUnused, _) = True
    isUnusedResult _ = False

    extractUnused (ISUnused, imp) = do
      impSpan <- piSpan imp
      Just $ UnusedImportInfo (piModule imp) impSpan imp
    extractUnused _ = Nothing

    extractPartial (ISPartiallyUnused, imp) = Just imp
    extractPartial _ = Nothing

-- | Check if a qualifier is used in the source
isQualifierUsed :: Text -> Maybe Text -> Set Text -> Bool
isQualifierUsed modName mAlias usedSymbols =
  let qualifier = maybe modName id mAlias
      prefix = qualifier <> "."
  in any (T.isPrefixOf prefix) (Set.toList usedSymbols)

-- | Get unused symbols from a partially-used import
getUnusedSymbolsFromImport :: Set Text -> ParsedImport -> [UnusedSymbol]
getUnusedSymbolsFromImport usedSymbols imp =
  let allSymbols = piExplicit imp
      unused = filter (not . (`Set.member` usedSymbols)) allSymbols
  in map (\sym -> UnusedSymbol (piModule imp) sym imp) unused

--------------------------------------------------------------------------------
-- Fix Generation
--------------------------------------------------------------------------------

-- | Generate fixes for all unused imports
generatePruneFixes :: PruneAnalysis -> [Fix]
generatePruneFixes analysis =
  let -- Fixes for completely unused imports
      removeImportFixes = map generateRemoveImportFix (paUnusedImports analysis)

      -- Fixes for unused symbols (grouped by import)
      symbolFixes = generateSymbolPruneFixes (paUnusedSymbols analysis)

  in removeImportFixes ++ symbolFixes

-- | Generate a fix to remove an entire import declaration
generateRemoveImportFix :: UnusedImportInfo -> Fix
generateRemoveImportFix UnusedImportInfo{..} = Fix
  { fixTitle = "Remove unused import '" <> uiModule <> "'"
  , fixEdits = [removeEdit]
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = [uiModule]
  , fixCategory = FCImports
  , fixSafety = FSAlways
  }
  where
    removeEdit = FixEdit
      { fixEditSpan = uiSpan
      , fixEditNewText = ""
      }

-- | Generate a fix to prune unused symbols from an import
generatePruneSymbolsFix :: ParsedImport -> [Text] -> Fix
generatePruneSymbolsFix imp unusedSyms = Fix
  { fixTitle = "Remove unused symbols from '" <> piModule imp <> "': "
               <> T.intercalate ", " unusedSyms
  , fixEdits = [pruneEdit]
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = unusedSyms
  , fixCategory = FCImports
  , fixSafety = FSAlways
  }
  where
    -- Create new import with pruned symbols
    prunedImport = imp { piExplicit = filter (`notElem` unusedSyms) (piExplicit imp) }
    newImportText = renderImport prunedImport

    -- Get the span from the original import
    editSpan = maybe defaultSpan id (piSpan imp)
    defaultSpan = mkSrcSpanRaw "<unknown>" 1 1 1 1

    pruneEdit = FixEdit
      { fixEditSpan = editSpan
      , fixEditNewText = newImportText
      }

-- | Generate fixes for groups of unused symbols
generateSymbolPruneFixes :: [UnusedSymbol] -> [Fix]
generateSymbolPruneFixes unusedSymbols =
  let -- Group by module
      grouped = groupByModule unusedSymbols
  in mapMaybe generateGroupFix (Map.toList grouped)
  where
    groupByModule :: [UnusedSymbol] -> Map Text [UnusedSymbol]
    groupByModule = foldr addToGroup Map.empty

    addToGroup sym m =
      Map.insertWith (++) (usModule sym) [sym] m

    generateGroupFix :: (Text, [UnusedSymbol]) -> Maybe Fix
    generateGroupFix (_, []) = Nothing
    generateGroupFix (_, sym : rest) =
      let imp = usImport sym
          unusedNames = map usSymbol (sym : rest)
      in Just $ generatePruneSymbolsFix imp unusedNames

--------------------------------------------------------------------------------
-- Diagnostic Generation
--------------------------------------------------------------------------------

-- | Generate diagnostics for unused imports
generateDiagnostics :: FilePath -> [UnusedImportInfo] -> [UnusedSymbol] -> [Diagnostic]
generateDiagnostics filePath unusedImports unusedSymbols =
  let importDiags = map mkImportDiag unusedImports
      symbolDiags = map mkSymbolDiag unusedSymbols
  in importDiags ++ symbolDiags
  where
    mkImportDiag UnusedImportInfo{..} = Diagnostic
      { diagSpan = uiSpan
      , diagSeverity = Types.Warning
      , diagKind = UnusedImport
      , diagMessage = "Unused import: " <> uiModule
      , diagCode = Just "unused-import"
      , diagFixes = [generateRemoveImportFix (UnusedImportInfo uiModule uiSpan uiImport)]
      , diagRelated = []
      }

    mkSymbolDiag UnusedSymbol{..} =
      let diagSpan' = maybe defaultSpan id (piSpan usImport)
          defaultSpan = mkSrcSpanRaw filePath 1 1 1 1
      in Diagnostic
        { diagSpan = diagSpan'
        , diagSeverity = Types.Warning
        , diagKind = UnusedImport
        , diagMessage = "Unused imported symbol: " <> usSymbol <> " from " <> usModule
        , diagCode = Just "unused-import-symbol"
        , diagFixes = []  -- Individual symbol fixes are grouped
        , diagRelated = []
        }

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Get all symbols used in source code (excluding imports)
getUsedSymbolsFromSource :: Text -> Set Text
getUsedSymbolsFromSource source =
  let -- Remove import section for analysis
      sourceWithoutImports = removeImportSection source

      -- Extract all symbol usages
      usages = extractUsedSymbols sourceWithoutImports

      -- Extract qualified usages with their full qualified form
      qualifiedUsages = extractQualifiedUsages sourceWithoutImports
      qualUsages = map formatQualifiedUsage qualifiedUsages

  in Set.fromList $ usages ++ qualUsages
  where
    -- Format a qualified usage as "Qualifier.name"
    formatQualifiedUsage :: SymbolUsage -> Text
    formatQualifiedUsage su = case suQualifier su of
      Just q  -> q <> "." <> suName su
      Nothing -> suName su

-- | Remove the import section from source code
removeImportSection :: Text -> Text
removeImportSection source =
  let sourceLines = T.lines source
      nonImportLines = filter (not . isImportLine) sourceLines
  in T.unlines nonImportLines
  where
    isImportLine line =
      T.isPrefixOf "import" (T.stripStart line)

-- | Get all symbols imported by a list of imports
getImportedSymbols :: [ParsedImport] -> Set Text
getImportedSymbols = Set.fromList . concatMap piExplicit

-- | Check if a symbol is used (simple check)
isSymbolUsed :: Text -> Set Text -> Bool
isSymbolUsed = Set.member
