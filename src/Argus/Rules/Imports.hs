{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Rules.Imports
-- Description : Import management and linting
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module handles import-related linting, including detecting
-- unused imports, suggesting qualified imports, and managing import style.
module Argus.Rules.Imports
  ( -- * Import checking
    checkImports
  , checkImportsWithHIE
  , detectUnusedImports
  , detectUnusedImportsWithHIE
  , suggestQualifiedImports
  , checkExplicitImports

    -- * Import optimization
  , optimizeImports
  , combineImports
  , sortImports
  ) where

import Control.Monad (forM)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe, catMaybes)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Analysis.Semantic (HieDb, getExports, getWildcardImportMembers)
import Argus.Analysis.Syntactic
import Argus.Config
import Argus.Rules.ConfigurableRules (lookupRecommendedAlias, defaultModuleRestrictions)
import Argus.Types

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

    qualifiedDiags = suggestQualifiedImports path cfg imports

    explicitDiags
      | importsRequireExplicit cfg = checkExplicitImports path imports
      | otherwise = []

-- | Detect unused imports
-- Note: Wildcard imports like SomeClass (..) are skipped because we cannot
-- determine which methods they bring in without type information.
-- Operators ARE checked since we now extract operator usage from code.
-- Class-with-methods patterns like HasTitle (title) are considered used if
-- any of the child methods are used, even if the class itself isn't referenced.
detectUnusedImports :: FilePath -> [ImportInfo] -> Set Text -> [Diagnostic]
detectUnusedImports _path imports usedNames = mapMaybe checkImport imports
  where
    checkImport imp = case iiExplicit imp of
      Nothing -> Nothing  -- Can't check implicit imports without more info
      Just items ->
        -- Filter out wildcard imports - we can't know what methods they bring in
        -- Operators ARE now checked since extractUsedNames extracts operators too
        let checkableItems = filter canCheckUsage items
            -- Filter to only unused items (items where neither the name nor any children are used)
            unusedItems = filter (not . isItemUsed) checkableItems
            usedItems = filter isItemUsed checkableItems
            -- Keep wildcard items (they can't be checked, so assume used)
            wildcardItems = filter importItemIsWildcard items
            unusedItemNames = map importItemName unusedItems
        in if null unusedItemNames
           then Nothing
           else Just $ mkUnusedImportDiagnostic imp unusedItemNames (usedItems ++ wildcardItems)

    -- Check if an import item is used. An item is considered used if:
    -- 1. The item name itself is in usedNames, OR
    -- 2. Any of its children (methods/constructors) are in usedNames
    --    This handles the class-with-methods pattern like HasTitle (title)
    isItemUsed item =
      importItemName item `Set.member` usedNames
      || any (`Set.member` usedNames) (importItemChildren item)

    -- We can reliably check usage for:
    -- - Non-wildcard imports (we know exactly what's being imported)
    -- - Operators are now checked since we extract operator usage from code
    -- Wildcard imports like ToJSON (..) are skipped - methods unknown at parse time
    canCheckUsage item =
      not (importItemIsWildcard item)

-- | Create diagnostic for unused imports with auto-fix
-- Takes the import, unused item names, and remaining used items
mkUnusedImportDiagnostic :: ImportInfo -> [Text] -> [ImportItem] -> Diagnostic
mkUnusedImportDiagnostic imp unused usedItems = Diagnostic
  { diagSpan = iiSpan imp
  , diagSeverity = Warning
  , diagKind = UnusedImport
  , diagMessage = "Unused imports from " <> iiModuleName imp <> ": " <> T.intercalate ", " unused
  , diagCode = Just "imports/unused"
  , diagFixes = generateUnusedImportFix imp usedItems
  , diagRelated = []
  }

-- | Generate fix for unused imports
-- If no items remain used, remove the entire import line
-- If some items remain used, regenerate the import with only used items
generateUnusedImportFix :: ImportInfo -> [ImportItem] -> [Fix]
generateUnusedImportFix imp usedItems
  | null usedItems =
      -- Remove the entire import line
      [Fix
        { fixTitle = "Remove unused import"
        , fixEdits = [FixEdit (iiSpan imp) ""]
        , fixIsPreferred = True
        , fixAddImports = []
        , fixRemoveImports = [iiModuleName imp]
        , fixCategory = FCImports
        , fixSafety = FSAlways
        }]
  | otherwise =
      -- Regenerate import with only used items
      [Fix
        { fixTitle = "Remove unused imports"
        , fixEdits = [FixEdit (iiSpan imp) newImportText]
        , fixIsPreferred = True
        , fixAddImports = []
        , fixRemoveImports = []
        , fixCategory = FCImports
        , fixSafety = FSAlways
        }]
  where
    newImportText = renderImport imp usedItems

-- | Render an import statement with explicit items
renderImport :: ImportInfo -> [ImportItem] -> Text
renderImport imp items = T.unwords $ filter (not . T.null)
  [ "import"
  , if iiQualified imp then "qualified" else ""
  , iiModuleName imp
  , maybe "" (\a -> "as " <> a) (iiAlias imp)
  , if iiHiding imp then "hiding" else ""
  , "(" <> renderItems items <> ")"
  ]

-- | Render import items as a comma-separated list
renderItems :: [ImportItem] -> Text
renderItems = T.intercalate ", " . map renderItem

-- | Render a single import item
renderItem :: ImportItem -> Text
renderItem item
  | importItemIsWildcard item =
      itemName <> " (..)"
  | not (null children) =
      itemName <> " (" <> T.intercalate ", " children <> ")"
  | importItemIsOperator item =
      "(" <> itemName <> ")"
  | otherwise =
      itemName
  where
    itemName = importItemName item
    children = importItemChildren item

-- | Suggest qualified imports for certain modules
-- Respects config options to allow unqualified type-only and operator imports
suggestQualifiedImports :: FilePath -> ImportsConfig -> [ImportInfo] -> [Diagnostic]
suggestQualifiedImports path cfg = mapMaybe checkImport
  where
    checkImport imp =
      if iiModuleName imp `elem` importsSuggestQualified cfg && not (iiQualified imp)
      then
        -- Check if this import should be excluded
        if shouldExclude imp
        then Nothing
        else Just $ mkQualifiedDiagnostic path imp
      else Nothing

    -- Exclude imports that only contain types and/or operators when those options are enabled
    shouldExclude imp = case iiExplicit imp of
      Nothing -> False  -- Non-explicit imports should be qualified
      Just items ->
        not (null items) && all shouldSkipItem items

    shouldSkipItem item
      | importsAllowUnqualifiedTypes cfg && importItemIsType item = True
      | importsAllowUnqualifiedOperators cfg && importItemIsOperator item = True
      | otherwise = False

mkQualifiedDiagnostic :: FilePath -> ImportInfo -> Diagnostic
mkQualifiedDiagnostic _path imp = Diagnostic
  { diagSpan = iiSpan imp
  , diagSeverity = Suggestion
  , diagKind = ImportStyle
  , diagMessage = "Consider using qualified import for " <> iiModuleName imp
  , diagCode = Just "imports/suggest-qualified"
  , diagFixes = [Fix
      { fixTitle = "Make qualified" <> maybe "" (" as " <>) recommendedAlias
      , fixEdits = [FixEdit (iiSpan imp) qualifiedText]
      , fixIsPreferred = True
      , fixAddImports = []
      , fixRemoveImports = []
      , fixCategory = FCImports
      , fixSafety = FSMostly
      }]
  , diagRelated = []
  }
  where
    modName = iiModuleName imp
    qualifiedText = "import qualified " <> modName <> alias
    alias = case iiAlias imp of
      Just a  -> " as " <> a
      Nothing -> case recommendedAlias of
        Just a  -> " as " <> a
        Nothing -> ""
    -- Dynamic lookup from ConfigurableRules - single source of truth for aliases
    recommendedAlias = lookupRecommendedAlias defaultModuleRestrictions modName

-- | Check for explicit import lists
checkExplicitImports :: FilePath -> [ImportInfo] -> [Diagnostic]
checkExplicitImports path = mapMaybe checkImport
  where
    checkImport imp =
      if iiExplicit imp == Nothing && not (iiHiding imp)
      then Just $ mkExplicitDiagnostic path imp
      else Nothing

mkExplicitDiagnostic :: FilePath -> ImportInfo -> Diagnostic
mkExplicitDiagnostic _path imp = Diagnostic
  { diagSpan = iiSpan imp
  , diagSeverity = Suggestion
  , diagKind = ImportStyle
  , diagMessage = "Consider using explicit import list for " <> iiModuleName imp
  , diagCode = Just "imports/require-explicit"
  , diagFixes = generateExplicitImportFixes imp
  , diagRelated = []
  }

-- | Generate fixes for non-explicit imports
-- Without HIE data, we can offer to make the import qualified
-- With HIE data, checkExplicitImportsWithHIE provides actual explicit lists
generateExplicitImportFixes :: ImportInfo -> [Fix]
generateExplicitImportFixes imp =
  [Fix
    { fixTitle = "Make qualified import"
    , fixEdits = [FixEdit (iiSpan imp) qualifiedImportText]
    , fixIsPreferred = False
    , fixAddImports = []
    , fixRemoveImports = []
    , fixCategory = FCImports
    , fixSafety = FSMostly
    }]
  where
    qualifiedImportText = T.unwords $ filter (not . T.null)
      [ "import qualified"
      , iiModuleName imp
      , generateAlias (iiModuleName imp)
      ]

    -- Generate a reasonable alias from module name
    -- e.g., "Data.Map.Strict" -> "as Map" or "as M"
    generateAlias modName =
      let parts = T.splitOn "." modName
          lastName = if null parts then modName else last parts
      in "as " <> lastName

--------------------------------------------------------------------------------
-- HIE-Based Import Checking (Full Mode)
--------------------------------------------------------------------------------

-- | Check imports using HIE semantic data
-- This version can properly check wildcard imports by resolving their members
checkImportsWithHIE :: ImportsConfig -> FilePath -> [ImportInfo] -> Set Text -> HieDb -> IO [Diagnostic]
checkImportsWithHIE cfg path imports usedNames db = do
  unusedDiags <- if importsRemoveUnused cfg
                 then detectUnusedImportsWithHIE path imports usedNames db
                 else pure []
  let qualifiedDiags = suggestQualifiedImports path cfg imports
  explicitDiags <- if importsRequireExplicit cfg
                   then checkExplicitImportsWithHIE path imports usedNames db
                   else pure []
  pure $ unusedDiags ++ qualifiedDiags ++ explicitDiags

-- | Check for explicit imports with HIE data - generates proper explicit import lists
checkExplicitImportsWithHIE :: FilePath -> [ImportInfo] -> Set Text -> HieDb -> IO [Diagnostic]
checkExplicitImportsWithHIE _path imports usedNames db = do
  diags <- forM imports $ \imp ->
    if iiExplicit imp == Nothing && not (iiHiding imp)
    then do
      -- Get all exports from the module
      exports <- getExports db (iiModuleName imp)
      -- Find which exports are used
      let usedExports = filter (`Set.member` usedNames) exports
      if null usedExports
        then pure $ Just $ mkExplicitDiagnosticNoHIE imp
        else pure $ Just $ mkExplicitDiagnosticWithHIE imp usedExports
    else pure Nothing
  pure $ catMaybes diags

-- | Create explicit import diagnostic when we have HIE data
mkExplicitDiagnosticWithHIE :: ImportInfo -> [Text] -> Diagnostic
mkExplicitDiagnosticWithHIE imp usedExports = Diagnostic
  { diagSpan = iiSpan imp
  , diagSeverity = Suggestion
  , diagKind = ImportStyle
  , diagMessage = "Consider using explicit import list for " <> iiModuleName imp
  , diagCode = Just "imports/require-explicit"
  , diagFixes = generateExplicitListFix imp usedExports ++ generateExplicitImportFixes imp
  , diagRelated = []
  }

-- | Create diagnostic when no HIE data could determine exports (falls back to qualified)
mkExplicitDiagnosticNoHIE :: ImportInfo -> Diagnostic
mkExplicitDiagnosticNoHIE imp = Diagnostic
  { diagSpan = iiSpan imp
  , diagSeverity = Suggestion
  , diagKind = ImportStyle
  , diagMessage = "Consider using explicit import list for " <> iiModuleName imp
  , diagCode = Just "imports/require-explicit"
  , diagFixes = generateExplicitImportFixes imp
  , diagRelated = []
  }

-- | Generate a fix with explicit import list based on used exports
generateExplicitListFix :: ImportInfo -> [Text] -> [Fix]
generateExplicitListFix imp usedExports =
  [Fix
    { fixTitle = "Add explicit import list"
    , fixEdits = [FixEdit (iiSpan imp) explicitImportText]
    , fixIsPreferred = True
    , fixAddImports = []
    , fixRemoveImports = []
    , fixCategory = FCImports
    , fixSafety = FSAlways
    }]
  where
    explicitImportText = T.unwords $ filter (not . T.null)
      [ "import"
      , if iiQualified imp then "qualified" else ""
      , iiModuleName imp
      , maybe "" (\a -> "as " <> a) (iiAlias imp)
      , "(" <> renderExports usedExports <> ")"
      ]

    -- Render export list, wrapping operators in parentheses
    renderExports = T.intercalate ", " . map wrapOperator

    wrapOperator name
      | isOperator name = "(" <> name <> ")"
      | otherwise = name

    isOperator name = not (T.null name) && T.all isOperatorChar name
    isOperatorChar c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

-- | Detect unused imports using HIE semantic data
-- This version properly resolves wildcard imports to check if their members are used
-- Also handles class-with-methods patterns like HasTitle (title) correctly
detectUnusedImportsWithHIE :: FilePath -> [ImportInfo] -> Set Text -> HieDb -> IO [Diagnostic]
detectUnusedImportsWithHIE _path imports usedNames db = do
  diags <- forM imports $ \imp -> case iiExplicit imp of
    Nothing -> pure Nothing  -- Can't check implicit imports
    Just items -> do
      -- Partition items into wildcards and non-wildcards
      let (wildcards, regular) = partitionItems items

      -- Check regular items (including operators and class-with-methods)
      -- An item is unused if neither the name nor any children are in usedNames
      let regularUsed = filter isItemUsed regular
          regularUnused = filter (not . isItemUsed) regular

      -- For wildcards, resolve their members and check usage
      wildcardResults <- forM wildcards $ \wildcard -> do
        members <- getWildcardImportMembers db (iiModuleName imp) (importItemName wildcard)
        let usedMembers = filter (`Set.member` usedNames) members
        if null usedMembers && not (null members)
          then pure (False, wildcard)  -- Unused wildcard
          else pure (True, wildcard)   -- Used wildcard

      let usedWildcards = map snd $ filter fst wildcardResults
          unusedWildcards = map snd $ filter (not . fst) wildcardResults
          allUsedItems = regularUsed ++ usedWildcards

      -- Build diagnostic message
      if null regularUnused && null unusedWildcards
        then pure Nothing
        else do
          let regularMsg = if null regularUnused
                           then ""
                           else T.intercalate ", " $ map importItemName regularUnused
              wildcardMsg = T.intercalate ", " $
                            map (\w -> importItemName w <> " (..)") unusedWildcards
              allUnused = T.intercalate ", " $ filter (not . T.null) [regularMsg, wildcardMsg]
          pure $ Just $ mkUnusedDiagWithWildcard imp allUnused allUsedItems

  pure $ catMaybes diags
  where
    partitionItems items = (filter importItemIsWildcard items,
                            filter (not . importItemIsWildcard) items)

    -- Check if an import item is used. An item is considered used if:
    -- 1. The item name itself is in usedNames, OR
    -- 2. Any of its children (methods/constructors) are in usedNames
    isItemUsed item =
      importItemName item `Set.member` usedNames
      || any (`Set.member` usedNames) (importItemChildren item)

-- | Create a diagnostic for unused imports including wildcards with auto-fix
mkUnusedDiagWithWildcard :: ImportInfo -> Text -> [ImportItem] -> Diagnostic
mkUnusedDiagWithWildcard imp unused usedItems = Diagnostic
  { diagSpan = iiSpan imp
  , diagSeverity = Warning
  , diagKind = UnusedImport
  , diagMessage = "Unused imports from " <> iiModuleName imp <> ": " <> unused
  , diagCode = Just "imports/unused"
  , diagFixes = generateUnusedImportFix imp usedItems
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
      Just items -> any ((`Set.member` usedNames) . importItemName) items

-- | Combine fragmented imports from the same module
combineImports :: [ImportInfo] -> [ImportInfo]
combineImports imports =
  map combineGroup $ NE.groupBy sameModule $ sortBy (comparing iiModuleName) imports
  where
    sameModule a b = iiModuleName a == iiModuleName b

    -- Using NonEmpty guarantees we always have at least one element
    combineGroup :: NonEmpty ImportInfo -> ImportInfo
    combineGroup (x :| []) = x
    combineGroup (x :| xs) = x
      { iiExplicit = case (iiExplicit x, concatMap (maybe [] id . iiExplicit) xs) of
          (Nothing, _) -> Nothing
          (Just a, b) -> Just $ a ++ b
      }

-- | Sort imports alphabetically
sortImports :: [ImportInfo] -> [ImportInfo]
sortImports = sortBy (comparing iiModuleName)
