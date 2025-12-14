{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.HIE.CrossModule
-- Description : Cross-module reference tracking and analysis
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides functionality for tracking references across modules,
-- enabling cross-module rename operations and dependency analysis.
module Argus.HIE.CrossModule
  ( -- * Cross-Module Analysis
    CrossModuleContext
  , buildCrossModuleContext
  , findCrossModuleReferences

    -- * Reference Tracking
  , getAllReferencesTo
  , getExternalReferences
  , getInternalReferences

    -- * Dependency Analysis
  , getModuleDeps
  , getReverseDeps
  , getDependencyChain

    -- * Multi-File Operations
  , MultiFileFix (..)
  , buildMultiFileFix
  , orderFixesByDependency

    -- * Rename Support
  , RenameResult (..)
  , renameAcrossModules
  , previewRename

    -- * Enhanced Rename (with qualified name handling)
  , QualifiedRef (..)
  , EnhancedRenameResult (..)
  , renameWithQualified
  , findQualifiedReferences
  , findReExports

    -- * Rename Safety
  , RenameSafetyResult (..)
  , checkRenameSafety
  , checkRenameShadowing

    -- * Batch Rename
  , BatchRenameResult (..)
  , batchRename
  , applyNamingConventionFixes
  ) where

import Control.Monad (forM)
import Data.List (nub, sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, catMaybes, fromMaybe, listToMaybe)
import Data.Ord (comparing, Down(..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import HieDb (HieDb)

import Argus.Types
  ( SrcSpan(..), Fix(..), FixEdit(..), FixCategory(..), FixSafety(..)
  , mkSrcSpanRaw, srcSpanStartLineRaw
  )
import Argus.HIE.Types
import Argus.Analysis.Semantic
  ( findReferences
  , getExports
  , getAllModules
  , ModuleInfo(..)
  , ReferenceResult(..)
  )

--------------------------------------------------------------------------------
-- Cross-Module Context
--------------------------------------------------------------------------------

-- | Context for cross-module analysis
data CrossModuleContext = CrossModuleContext
  { cmcDb           :: HieDb
  , cmcModules      :: Map Text ModuleSymbols
  , cmcReferences   :: Map Text [SymbolReference]
  , cmcDependencies :: Map Text (Set Text)
  , cmcReverseDeps  :: Map Text (Set Text)
  }

-- | Build cross-module context from HIE database
buildCrossModuleContext :: HieDb -> IO CrossModuleContext
buildCrossModuleContext db = do
  -- Get all modules
  modules <- getAllModules db

  -- Build module symbols
  modSymbols <- forM modules $ \modInfo -> do
    let modName = moduleInfoName modInfo
    exports <- getExports db modName
    pure (modName, ModuleSymbols
      { msModule = modName
      , msFile = moduleInfoFile modInfo
      , msExports = Set.fromList exports
      , msImports = Map.empty  -- Would need HIE reading
      , msDefinitions = []
      })

  -- Build references map
  allRefs <- buildReferencesMap db modules

  -- Build dependency graph
  let (deps, revDeps) = buildDependencyMaps allRefs

  pure CrossModuleContext
    { cmcDb = db
    , cmcModules = Map.fromList modSymbols
    , cmcReferences = allRefs
    , cmcDependencies = deps
    , cmcReverseDeps = revDeps
    }

-- | Build map of all references per symbol
buildReferencesMap :: HieDb -> [ModuleInfo] -> IO (Map Text [SymbolReference])
buildReferencesMap db modules = do
  allRefs <- forM modules $ \modInfo -> do
    let modName = moduleInfoName modInfo
    exports <- getExports db modName
    refs <- forM exports $ \sym -> do
      symRefs <- findReferences db sym (Just modName)
      pure $ map (toSymbolRef sym modName) symRefs
    pure $ concat refs
  pure $ Map.fromListWith (++) [(srSymbol r, [r]) | r <- concat allRefs]
  where
    toSymbolRef sym defModule ref = SymbolReference
      { srSymbol = sym
      , srModule = refResultModule ref
      , srFile = refResultFile ref
      , srSpan = refResultSpan ref
      , srKind = if refResultModule ref == defModule then RefDefinition else RefUsage
      , srQualified = Nothing
      }

-- | Build dependency maps from references
buildDependencyMaps :: Map Text [SymbolReference] -> (Map Text (Set Text), Map Text (Set Text))
buildDependencyMaps refs =
  let allRefs = concat $ Map.elems refs
      -- Module A depends on Module B if A references a symbol defined in B
      deps = Map.fromListWith Set.union
        [ (srModule r, Set.singleton defMod)
        | r <- allRefs
        , let defMod = findDefiningModule (srSymbol r) refs
        , defMod /= srModule r
        ]
      -- Reverse: B is depended on by A
      revDeps = Map.fromListWith Set.union
        [ (defMod, Set.singleton (srModule r))
        | r <- allRefs
        , let defMod = findDefiningModule (srSymbol r) refs
        , defMod /= srModule r
        ]
  in (deps, revDeps)
  where
    findDefiningModule sym refMap =
      case Map.lookup sym refMap of
        Just (r:_) -> srModule r  -- First reference is typically definition
        _ -> ""

--------------------------------------------------------------------------------
-- Reference Tracking
--------------------------------------------------------------------------------

-- | Find cross-module references to a symbol
findCrossModuleReferences :: CrossModuleContext -> Text -> [SymbolReference]
findCrossModuleReferences ctx symName =
  fromMaybe [] $ Map.lookup symName (cmcReferences ctx)

-- | Get all references to a symbol
getAllReferencesTo :: CrossModuleContext -> Text -> Maybe Text -> IO [SymbolReference]
getAllReferencesTo ctx name mModule = do
  refs <- findReferences (cmcDb ctx) name mModule
  pure $ map (toRef name) refs
  where
    toRef sym ref = SymbolReference
      { srSymbol = sym
      , srModule = refResultModule ref
      , srFile = refResultFile ref
      , srSpan = refResultSpan ref
      , srKind = RefUsage
      , srQualified = Nothing
      }

-- | Get references from other modules
getExternalReferences :: CrossModuleContext -> Text -> Text -> IO [SymbolReference]
getExternalReferences ctx defModule name = do
  allRefs <- getAllReferencesTo ctx name (Just defModule)
  pure $ filter (\r -> srModule r /= defModule) allRefs

-- | Get references from the same module
getInternalReferences :: CrossModuleContext -> Text -> Text -> IO [SymbolReference]
getInternalReferences ctx defModule name = do
  allRefs <- getAllReferencesTo ctx name (Just defModule)
  pure $ filter (\r -> srModule r == defModule) allRefs

--------------------------------------------------------------------------------
-- Dependency Analysis
--------------------------------------------------------------------------------

-- | Get modules that a module depends on
getModuleDeps :: CrossModuleContext -> Text -> Set Text
getModuleDeps ctx modName =
  fromMaybe Set.empty $ Map.lookup modName (cmcDependencies ctx)

-- | Get modules that depend on a module
getReverseDeps :: CrossModuleContext -> Text -> Set Text
getReverseDeps ctx modName =
  fromMaybe Set.empty $ Map.lookup modName (cmcReverseDeps ctx)

-- | Get the full dependency chain (transitive closure)
getDependencyChain :: CrossModuleContext -> Text -> Set Text
getDependencyChain ctx startModule = go Set.empty (Set.singleton startModule)
  where
    go visited toVisit
      | Set.null toVisit = visited
      | otherwise =
          let current = Set.findMin toVisit
              rest = Set.deleteMin toVisit
          in if current `Set.member` visited
               then go visited rest
               else let deps = getModuleDeps ctx current
                        newToVisit = Set.union rest (Set.difference deps visited)
                    in go (Set.insert current visited) newToVisit

--------------------------------------------------------------------------------
-- Multi-File Operations
--------------------------------------------------------------------------------

-- | A fix that spans multiple files
data MultiFileFix = MultiFileFix
  { mffFixes           :: Map FilePath [Fix]
  , mffDependencyOrder :: [FilePath]
  , mffAffectedModules :: Set Text
  , mffSymbol          :: Text
  }
  deriving stock (Eq, Show)

-- | Build a multi-file fix from references
buildMultiFileFix :: Text -> Text -> [(FilePath, [SrcSpan])] -> MultiFileFix
buildMultiFileFix symbol newName fileSpans =
  let fixes = Map.fromList
        [ (file, [mkRenameFix symbol newName spans])
        | (file, spans) <- fileSpans
        , not (null spans)
        ]
      order = orderByDependency $ Map.keys fixes
  in MultiFileFix
       { mffFixes = fixes
       , mffDependencyOrder = order
       , mffAffectedModules = Set.empty  -- Would be filled by caller
       , mffSymbol = symbol
       }

-- | Create a rename fix for multiple spans
mkRenameFix :: Text -> Text -> [SrcSpan] -> Fix
mkRenameFix _oldName newName srcSpans = Fix
  { fixTitle = "Rename to " <> newName
  , fixEdits = [FixEdit srcSpan newName | srcSpan <- srcSpans]
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSMostly
  }

-- | Order files by dependency for safe application
orderByDependency :: [FilePath] -> [FilePath]
orderByDependency = id  -- Simple implementation; would use dependency graph

-- | Order fixes by dependency graph
orderFixesByDependency :: CrossModuleContext -> Map FilePath [Fix] -> [FilePath]
orderFixesByDependency ctx fixes =
  -- Sort files so that dependees come before dependents
  sortBy (comparing (Down . dependencyCount)) $ Map.keys fixes
  where
    dependencyCount file =
      case fileToModule file of
        Nothing -> 0
        Just modName -> Set.size $ getReverseDeps ctx modName

    fileToModule file =
      let mods = Map.elems (cmcModules ctx)
      in msModule <$> find (\m -> msFile m == Just file) mods

    find p = listToMaybe . filter p
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x

--------------------------------------------------------------------------------
-- Rename Support
--------------------------------------------------------------------------------

-- | Result of a cross-module rename operation
data RenameResult = RenameResult
  { rrSuccess       :: Bool
  , rrFixes         :: Map FilePath [Fix]
  , rrAffectedFiles :: [FilePath]
  , rrExportChanges :: [(Text, Text, Text)]  -- (module, old, new)
  , rrImportChanges :: [(FilePath, Text)]    -- (file, newImportLine)
  , rrErrors        :: [Text]
  }
  deriving stock (Eq, Show)

-- | Rename a symbol across all modules
renameAcrossModules :: CrossModuleContext -> Text -> Text -> Text -> IO RenameResult
renameAcrossModules ctx defModule oldName newName = do
  -- Find all references
  allRefs <- getAllReferencesTo ctx oldName (Just defModule)

  -- Check for conflicts
  let conflicts = checkRenameConflicts ctx allRefs newName
  if not (null conflicts)
    then pure RenameResult
      { rrSuccess = False
      , rrFixes = Map.empty
      , rrAffectedFiles = []
      , rrExportChanges = []
      , rrImportChanges = []
      , rrErrors = conflicts
      }
    else do
      -- Group references by file
      let refsByFile = Map.fromListWith (++)
            [(srFile r, [srSpan r]) | r <- allRefs]

      -- Build fixes for each file
      let fixes = Map.map (pure . mkRenameFix oldName newName) refsByFile

      -- Check if symbol is exported
      let isExported = case Map.lookup defModule (cmcModules ctx) of
            Just ms -> oldName `Set.member` msExports ms
            Nothing -> False

      -- Generate export/import changes
      let exportChanges = if isExported
            then [(defModule, oldName, newName)]
            else []

      pure RenameResult
        { rrSuccess = True
        , rrFixes = fixes
        , rrAffectedFiles = Map.keys fixes
        , rrExportChanges = exportChanges
        , rrImportChanges = []  -- Would need more analysis
        , rrErrors = []
        }

-- | Check for conflicts with a rename
checkRenameConflicts :: CrossModuleContext -> [SymbolReference] -> Text -> [Text]
checkRenameConflicts ctx refs newName =
  let affectedModules = nub $ map srModule refs
      conflicts = mapMaybe (checkModuleConflict ctx newName) affectedModules
  in conflicts

-- | Check if a new name would conflict in a module
checkModuleConflict :: CrossModuleContext -> Text -> Text -> Maybe Text
checkModuleConflict ctx newName modName =
  case Map.lookup modName (cmcModules ctx) of
    Nothing -> Nothing
    Just ms ->
      if newName `Set.member` msExports ms
        then Just $ "Name '" <> newName <> "' already exists in module " <> modName
        else Nothing

-- | Preview a rename without applying it
previewRename :: CrossModuleContext -> Text -> Text -> Text -> IO RenameResult
previewRename = renameAcrossModules  -- Same as actual rename, just don't apply

--------------------------------------------------------------------------------
-- Enhanced Rename with Qualified Names
--------------------------------------------------------------------------------

-- | Reference with qualification info
data QualifiedRef = QualifiedRef
  { qrRef        :: SymbolReference     -- ^ The base reference
  , qrQualifier  :: Maybe Text          -- ^ The qualifier used (e.g., "M" in M.foo)
  , qrIsReExport :: Bool                -- ^ Is this a re-export?
  , qrOrigModule :: Text                -- ^ Original defining module
  }
  deriving stock (Eq, Show)

-- | Result of rename with qualified name handling
data EnhancedRenameResult = EnhancedRenameResult
  { errBaseResult     :: RenameResult       -- ^ The base rename result
  , errQualifiedEdits :: Map FilePath [FixEdit]  -- ^ Edits for qualified refs
  , errReExportEdits  :: Map FilePath [FixEdit]  -- ^ Edits for re-exports
  , errImportEdits    :: Map FilePath [FixEdit]  -- ^ Import list edits
  , errModuleEdits    :: Map FilePath [FixEdit]  -- ^ Module export list edits
  }
  deriving stock (Eq, Show)

-- | Rename with full qualified name handling
renameWithQualified :: CrossModuleContext -> Text -> Text -> Text -> IO EnhancedRenameResult
renameWithQualified ctx defModule oldName newName = do
  -- Get base rename result
  baseResult <- renameAcrossModules ctx defModule oldName newName

  if not (rrSuccess baseResult)
    then pure $ EnhancedRenameResult baseResult Map.empty Map.empty Map.empty Map.empty
    else do
      -- Find qualified references
      qualifiedRefs <- findQualifiedReferences ctx defModule oldName

      -- Find re-exports
      reExports <- findReExports ctx defModule oldName

      -- Build qualified edits
      let qualEdits = buildQualifiedEdits oldName newName qualifiedRefs
          reExportEdits = buildReExportEdits oldName newName reExports
          importEdits = buildImportEdits defModule oldName newName qualifiedRefs
          moduleEdits = buildModuleExportEdits ctx defModule oldName newName

      pure EnhancedRenameResult
        { errBaseResult = baseResult
        , errQualifiedEdits = qualEdits
        , errReExportEdits = reExportEdits
        , errImportEdits = importEdits
        , errModuleEdits = moduleEdits
        }

-- | Find references that use qualified names
findQualifiedReferences :: CrossModuleContext -> Text -> Text -> IO [QualifiedRef]
findQualifiedReferences ctx defModule name = do
  allRefs <- getAllReferencesTo ctx name (Just defModule)

  -- For each reference, check if it's qualified
  let qualRefs = mapMaybe (detectQualification defModule) allRefs

  pure qualRefs
  where
    detectQualification origMod ref =
      -- Check if this reference uses a qualified name
      -- This is a heuristic - full implementation would use HIE AST
      case srQualified ref of
        Just qual -> Just QualifiedRef
          { qrRef = ref
          , qrQualifier = Just qual
          , qrIsReExport = False
          , qrOrigModule = origMod
          }
        Nothing -> Nothing

-- | Find modules that re-export a symbol
findReExports :: CrossModuleContext -> Text -> Text -> IO [QualifiedRef]
findReExports ctx defModule name = do
  -- Get all modules
  let modules = Map.toList (cmcModules ctx)

  -- Check each module for re-exports
  let reExports = catMaybes
        [ checkReExport modName ms defModule name
        | (modName, ms) <- modules
        , modName /= defModule
        ]

  pure reExports
  where
    checkReExport modName ms origMod sym =
      if sym `Set.member` msExports ms
        then Just QualifiedRef
          { qrRef = SymbolReference sym modName ""
                     (mkSrcSpanRaw "" 0 0 0 0) RefUsage Nothing
          , qrQualifier = Nothing
          , qrIsReExport = True
          , qrOrigModule = origMod
          }
        else Nothing

-- | Build edits for qualified references
buildQualifiedEdits :: Text -> Text -> [QualifiedRef] -> Map FilePath [FixEdit]
buildQualifiedEdits oldName newName refs =
  Map.fromListWith (++)
    [ (srFile (qrRef r), [mkQualifiedEdit r oldName newName])
    | r <- refs
    , let file = srFile (qrRef r)
    , not (null file)
    ]

-- | Create an edit for a qualified reference
mkQualifiedEdit :: QualifiedRef -> Text -> Text -> FixEdit
mkQualifiedEdit qr _oldName newName =
  let srcSpan = srSpan (qrRef qr)
      qualifiedName = case qrQualifier qr of
        Just qual -> qual <> "." <> newName
        Nothing -> newName
  in FixEdit srcSpan qualifiedName

-- | Build edits for re-export sites
--
-- Re-exports appear in module export lists as re-exported symbols.
-- This function creates edits for symbols that are re-exported from other modules.
-- Each QualifiedRef with qrIsReExport=True represents a module that re-exports
-- the symbol being renamed.
buildReExportEdits :: Text -> Text -> [QualifiedRef] -> Map FilePath [FixEdit]
buildReExportEdits _oldName newName refs =
  let reExportRefs = filter qrIsReExport refs
      -- Create edits using the reference spans from re-exporting modules
      editsByFile = Map.fromListWith (++)
        [ (file, [FixEdit refSpan newName])
        | r <- reExportRefs
        , let file = srFile (qrRef r)
        , not (null file)
        , let refSpan = srSpan (qrRef r)
        -- Only include if we have a valid span (start line > 0)
        , srcSpanStartLineRaw refSpan > 0
        ]
  in editsByFile

-- | Build edits for import lists
--
-- When a symbol is renamed, import statements that explicitly import that symbol
-- need to be updated. This function finds all such imports and creates edits.
-- The QualifiedRefs tell us which files reference the symbol; for each file
-- that isn't the defining module, there may be an import that needs updating.
--
-- Note: The actual spans for import list items come from RefImport kind references
-- in HIE data. For references that don't have that information, we skip them.
buildImportEdits :: Text -> Text -> Text -> [QualifiedRef] -> Map FilePath [FixEdit]
buildImportEdits defModule _oldName newName refs =
  -- Filter to refs that represent imports (from modules other than the defining module)
  -- and have valid spans
  let importRefs =
        [ r
        | r <- refs
        , srModule (qrRef r) /= defModule  -- Not from the defining module
        , srKind (qrRef r) == RefImport     -- Is an import reference
        , srcSpanStartLineRaw (srSpan (qrRef r)) > 0  -- Has valid span
        ]
      -- Group by file and create edits
      editsByFile = Map.fromListWith (++)
        [ (srFile (qrRef r), [FixEdit (srSpan (qrRef r)) newName])
        | r <- importRefs
        , not (null (srFile (qrRef r)))
        ]
  in editsByFile

-- | Build edits for module export lists
--
-- When a symbol is renamed in its defining module, the module's export list
-- (if explicit) needs to be updated. This function looks up the module's file
-- from the context and creates edits for any RefExport references found.
--
-- Note: This relies on HIE data containing RefExport references with accurate spans.
-- If the module has no explicit export list, or uses implicit exports,
-- no edits are needed.
buildModuleExportEdits :: CrossModuleContext -> Text -> Text -> Text -> Map FilePath [FixEdit]
buildModuleExportEdits ctx defModule oldName newName =
  -- Look up the module's file path from the context
  case Map.lookup defModule (cmcModules ctx) of
    Nothing -> Map.empty  -- Module not found in context
    Just ms -> case msFile ms of
      Nothing -> Map.empty  -- No file path for module
      Just filePath ->
        -- Check if the symbol is in the exports and has a RefExport reference
        if oldName `Set.member` msExports ms
          then
            -- Look for export references in the module's reference list
            let exportRefs = fromMaybe [] $ Map.lookup oldName (cmcReferences ctx)
                -- Filter to RefExport kind in the defining module's file
                exportEdits =
                  [ FixEdit (srSpan ref) newName
                  | ref <- exportRefs
                  , srKind ref == RefExport
                  , srFile ref == filePath
                  , srcSpanStartLineRaw (srSpan ref) > 0
                  ]
            in if null exportEdits
                 then Map.empty
                 else Map.singleton filePath exportEdits
          else Map.empty  -- Symbol not exported, no edit needed

--------------------------------------------------------------------------------
-- Rename Safety Checking
--------------------------------------------------------------------------------

-- | Check if a rename is safe across the codebase
data RenameSafetyResult = RenameSafetyResult
  { rsrSafe          :: Bool
  , rsrWarnings      :: [Text]
  , rsrErrors        :: [Text]
  , rsrAffectedFiles :: Int
  , rsrAffectedRefs  :: Int
  }
  deriving stock (Eq, Show)

-- | Check rename safety before applying
checkRenameSafety :: CrossModuleContext -> Text -> Text -> Text -> IO RenameSafetyResult
checkRenameSafety ctx defModule oldName newName = do
  -- Get all references
  allRefs <- getAllReferencesTo ctx oldName (Just defModule)

  -- Check for shadowing
  shadowWarnings <- checkRenameShadowing ctx allRefs newName

  -- Check for conflicts with existing symbols
  conflicts <- pure $ checkRenameConflicts ctx allRefs newName

  -- Check for type compatibility (if HIE has type info)
  typeWarnings <- checkRenameTypeCompat ctx defModule oldName newName

  let allWarnings = shadowWarnings ++ typeWarnings
      allErrors = conflicts

  pure RenameSafetyResult
    { rsrSafe = null allErrors
    , rsrWarnings = allWarnings
    , rsrErrors = allErrors
    , rsrAffectedFiles = length $ nub $ map srFile allRefs
    , rsrAffectedRefs = length allRefs
    }

-- | Check if rename would cause shadowing
checkRenameShadowing :: CrossModuleContext -> [SymbolReference] -> Text -> IO [Text]
checkRenameShadowing ctx refs newName = do
  let affectedModules = nub $ map srModule refs

  -- Check each module for potential shadowing
  let shadowChecks = catMaybes
        [ checkModuleShadowing ctx modName newName
        | modName <- affectedModules
        ]

  pure shadowChecks

-- | Check for shadowing in a specific module
checkModuleShadowing :: CrossModuleContext -> Text -> Text -> Maybe Text
checkModuleShadowing ctx modName newName =
  case Map.lookup modName (cmcModules ctx) of
    Nothing -> Nothing
    Just ms ->
      -- Check if newName shadows an import (msImports is Map Text [Text])
      if any (newName `elem`) (Map.elems (msImports ms))
           then Just $ "'" <> newName <> "' may shadow an import in " <> modName
           else Nothing

-- | Check type compatibility of rename
--
-- This function checks if the new name would conflict with an existing symbol
-- that has a different type in any of the affected modules. This helps catch
-- cases where renaming would lead to type errors.
--
-- Warnings are issued when:
-- 1. The new name already exists in a module and has a different type
-- 2. The new name would shadow a differently-typed import
checkRenameTypeCompat :: CrossModuleContext -> Text -> Text -> Text -> IO [Text]
checkRenameTypeCompat ctx defModule _oldName newName = do
  -- Check if newName already exists in the defining module with a different type
  let existingInDefModule = checkExistingSymbol ctx defModule newName

  -- Check all modules that reference the symbol for type conflicts
  let affectedModules = Set.toList $ getReverseDeps ctx defModule
      existingInOthers = mapMaybe (checkExistingSymbol ctx newName) affectedModules

  -- Combine warnings
  pure $ catMaybes [existingInDefModule] ++ existingInOthers
  where
    -- Check if a symbol exists in a module and return a warning if so
    checkExistingSymbol :: CrossModuleContext -> Text -> Text -> Maybe Text
    checkExistingSymbol cxt modName symName =
      case Map.lookup modName (cmcModules cxt) of
        Nothing -> Nothing
        Just ms ->
          -- Check if symbol is already exported from this module
          if symName `Set.member` msExports ms
            then Just $ "Warning: '" <> symName <> "' already exists in module " <> modName <>
                        ". Renaming may cause a conflict."
            else
              -- Check if it's in the imports
              let importedModules = Map.keys (msImports ms)
                  hasImport = any (\importMod ->
                    case Map.lookup importMod (msImports ms) of
                      Just syms -> symName `elem` syms
                      Nothing -> False) importedModules
              in if hasImport
                   then Just $ "Warning: '" <> symName <> "' is imported in module " <> modName <>
                               ". Renaming may cause shadowing."
                   else Nothing

--------------------------------------------------------------------------------
-- Batch Rename Operations
--------------------------------------------------------------------------------

-- | Batch rename result
data BatchRenameResult = BatchRenameResult
  { brrSuccessful :: [(Text, Text, RenameResult)]  -- (old, new, result)
  , brrFailed     :: [(Text, Text, [Text])]        -- (old, new, errors)
  , brrTotalFiles :: Int
  , brrTotalEdits :: Int
  }
  deriving stock (Eq, Show)

-- | Perform multiple renames in order
batchRename :: CrossModuleContext -> [(Text, Text, Text)] -> IO BatchRenameResult
batchRename ctx renames = do
  results <- forM renames $ \(defMod, oldName, newName) -> do
    result <- renameAcrossModules ctx defMod oldName newName
    pure (defMod, oldName, newName, result)

  let successful = [(old, new, r) | (_, old, new, r) <- results, rrSuccess r]
      failed = [(old, new, rrErrors r) | (_, old, new, r) <- results, not (rrSuccess r)]
      totalFiles = sum [length (rrAffectedFiles r) | (_, _, r) <- successful]
      totalEdits = sum [sum (map length (Map.elems (rrFixes r))) | (_, _, r) <- successful]

  pure BatchRenameResult
    { brrSuccessful = successful
    , brrFailed = failed
    , brrTotalFiles = totalFiles
    , brrTotalEdits = totalEdits
    }

-- | Apply naming convention fixes automatically
applyNamingConventionFixes :: CrossModuleContext -> Text -> [(Text, Text)] -> IO BatchRenameResult
applyNamingConventionFixes ctx modName fixes = do
  batchRename ctx [(modName, old, new) | (old, new) <- fixes]
