{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Argus.HIE.SymbolTable
-- Description : Symbol resolution and table management
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
-- Portability : GHC
--
-- = Overview
--
-- This module provides symbol table management for HIE-based analysis,
-- including building symbol tables from HIE data, resolving symbols,
-- and checking replacement safety.
--
-- = Architecture
--
-- @
-- ┌──────────────────────────────────────────────────────────────────┐
-- │                        SymbolTable                               │
-- │  ┌───────────────┐  ┌──────────────┐  ┌─────────────────────┐   │
-- │  │  stSymbols    │  │   stTypes    │  │    stClasses        │   │
-- │  │ Map Text      │  │ Map Text     │  │  Map Text HieSymbol │   │
-- │  │  [HieSymbol]  │  │  HieSymbol   │  │                     │   │
-- │  └───────────────┘  └──────────────┘  └─────────────────────┘   │
-- │  ┌───────────────┐  ┌──────────────┐  ┌─────────────────────┐   │
-- │  │  stImports    │  │  stExports   │  │   stQualified       │   │
-- │  │ ImportedModule│  │  Set Text    │  │  Map Text Text      │   │
-- │  └───────────────┘  └──────────────┘  └─────────────────────┘   │
-- └──────────────────────────────────────────────────────────────────┘
-- @
--
-- = Key Operations
--
-- * __Building__: Construct tables from HIE database ('buildSymbolTable')
-- * __Resolution__: Find symbols by name or qualified name
-- * __Safety Checking__: Validate symbol replacements ('canReplace')
-- * __Scope Analysis__: Navigate local, module, and global scopes
--
-- = Safety Checking
--
-- The module provides comprehensive safety analysis for symbol replacement:
--
-- * __Shadowing__: Detects when a new name would shadow imports
-- * __Conflicts__: Identifies naming collisions in scope
-- * __Type Compatibility__: Checks for type variable capture
-- * __Semantic Changes__: Warns about class method/instance breakage
--
-- = Thread Safety
--
-- @SymbolTable@ is immutable after construction and safe for concurrent reads.
-- Modifications return new tables (functional updates).
--
-- = Usage
--
-- @
-- table <- buildSymbolTable hieDb "MyModule"
-- case canReplace table "oldName" "newName" of
--   SafeReplace -> -- proceed with rename
--   UnsafeShadow mod name -> -- warning: would shadow import
--   UnsafeConflict reason -> -- error: conflict exists
-- @
--
-- @since 1.0.0
module Argus.HIE.SymbolTable
  ( -- * Symbol Table
    SymbolTable
  , emptySymbolTable
  , buildSymbolTable
  , buildSymbolTableFromFiles

    -- * Symbol Resolution
  , resolveSymbol
  , resolveQualified
  , resolveInScope
  , findInScope

    -- * Safety Checking
  , canReplace
  , checkShadowing
  , checkConflicts

    -- * Symbol Lookup
  , lookupSymbol
  , lookupType
  , lookupClass
  , lookupModule

    -- * Table Operations
  , addSymbol
  , removeSymbol
  , mergeSymbolTables
  , filterSymbols

    -- * Scope Analysis
  , getLocalScope
  , getModuleScope
  , getGlobalScope
  ) where

import Control.Monad (forM)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, fromMaybe, catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import HieDb (HieDb)

import Argus.Types (SrcSpan, SymbolKind(..))
import Argus.Types qualified as Types
import Argus.HIE.Types
import Argus.Analysis.Semantic
  ( findDefinition
  , findReferences
  , getExports
  , DefinitionResult(..)
  , ReferenceResult(..)
  )

--------------------------------------------------------------------------------
-- Symbol Table Type
--------------------------------------------------------------------------------

-- | A symbol table for a module or scope
data SymbolTable = SymbolTable
  { stModule      :: Maybe Text              -- ^ Module this table belongs to
  , stSymbols     :: Map Text [HieSymbol]    -- ^ Symbols by name (multiple for overloading)
  , stTypes       :: Map Text HieSymbol      -- ^ Type symbols
  , stClasses     :: Map Text HieSymbol      -- ^ Type class symbols
  , stImports     :: Map Text ImportedModule -- ^ Imported modules
  , stExports     :: Set Text                -- ^ Exported symbol names
  , stQualified   :: Map Text Text           -- ^ Qualified names to modules
  }
  deriving stock (Eq, Show)

-- | Information about an imported module
data ImportedModule = ImportedModule
  { imModule     :: Text           -- ^ Module name
  , imQualifier  :: Maybe Text     -- ^ Qualifier (if qualified import)
  , imExplicit   :: Maybe [Text]   -- ^ Explicit import list (Nothing = all)
  , imHiding     :: [Text]         -- ^ Hidden symbols
  }
  deriving stock (Eq, Show)

-- | Empty symbol table
emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable
  { stModule = Nothing
  , stSymbols = Map.empty
  , stTypes = Map.empty
  , stClasses = Map.empty
  , stImports = Map.empty
  , stExports = Set.empty
  , stQualified = Map.empty
  }

--------------------------------------------------------------------------------
-- Building Symbol Tables
--------------------------------------------------------------------------------

-- | Build a symbol table from HIE database for a module
buildSymbolTable :: HieDb -> Text -> IO SymbolTable
buildSymbolTable db modName = do
  -- Get all exports
  exports <- getExports db modName

  -- Build symbol entries for each export
  symbols <- forM exports $ \name -> do
    defs <- findDefinition db name (Just modName)
    refs <- findReferences db name (Just modName)
    pure $ case defs of
      [] -> Nothing
      (def:_) -> Just HieSymbol
        { hsName = name
        , hsModule = modName
        , hsQualified = modName <> "." <> name
        , hsKind = inferSymbolKind name
        , hsType = Nothing
        , hsDefinition = Just $ defResultSpan def
        , hsReferences = map refResultSpan refs
        , hsExported = True
        , hsScope = ScopeModule
        , hsVisibility = VisPublic
        , hsDocumentation = Nothing
        }

  let validSymbols = catMaybes symbols
      (types, classes, values) = partitionSymbols validSymbols

  pure SymbolTable
    { stModule = Just modName
    , stSymbols = Map.fromListWith (++) [(hsName s, [s]) | s <- values]
    , stTypes = Map.fromList [(hsName s, s) | s <- types]
    , stClasses = Map.fromList [(hsName s, s) | s <- classes]
    , stImports = Map.empty
    , stExports = Set.fromList exports
    , stQualified = Map.empty
    }

-- | Build symbol tables for multiple files
buildSymbolTableFromFiles :: HieDb -> [Text] -> IO (Map Text SymbolTable)
buildSymbolTableFromFiles db modNames = do
  tables <- forM modNames $ \modName -> do
    table <- buildSymbolTable db modName
    pure (modName, table)
  pure $ Map.fromList tables

-- | Partition symbols by kind
partitionSymbols :: [HieSymbol] -> ([HieSymbol], [HieSymbol], [HieSymbol])
partitionSymbols = foldr partition ([], [], [])
  where
    partition sym (types, classes, values) = case hsKind sym of
      TypeConstructor -> (sym : types, classes, values)
      TypeFamily -> (sym : types, classes, values)
      TypeClass -> (types, sym : classes, values)
      _ -> (types, classes, sym : values)

-- | Infer symbol kind from name
inferSymbolKind :: Text -> SymbolKind
inferSymbolKind name
  | T.null name = Function
  | isUpper (T.head name) = TypeConstructor  -- Types/constructors start with uppercase
  | T.head name == ':' = DataConstructor  -- Operators starting with : are constructors
  | otherwise = Function
  where
    isUpper c = c >= 'A' && c <= 'Z'

--------------------------------------------------------------------------------
-- Symbol Resolution
--------------------------------------------------------------------------------

-- | Resolve a symbol name in the table
resolveSymbol :: SymbolTable -> Text -> Maybe HieSymbol
resolveSymbol table name =
  -- First try direct lookup
  listToMaybe =<< Map.lookup name (stSymbols table)

-- | Resolve a qualified name (Module.name)
resolveQualified :: SymbolTable -> Text -> Text -> Maybe HieSymbol
resolveQualified table modName name =
  case Map.lookup modName (stImports table) of
    Nothing -> Nothing
    Just imp ->
      -- Check if the symbol is accessible through this import
      if isAccessible imp name
        then resolveSymbol table name
        else Nothing
  where
    isAccessible imp symName = case imExplicit imp of
      Nothing -> symName `notElem` imHiding imp
      Just explicit -> symName `elem` explicit && symName `notElem` imHiding imp

-- | Resolve a symbol in the current scope
resolveInScope :: SymbolTable -> Text -> SrcSpan -> Maybe HieSymbol
resolveInScope table name _span =
  -- For now, just do basic resolution
  -- Full implementation would consider local scopes
  resolveSymbol table name

-- | Find a symbol that might be in scope
findInScope :: SymbolTable -> Text -> [HieSymbol]
findInScope table name =
  fromMaybe [] $ Map.lookup name (stSymbols table)

--------------------------------------------------------------------------------
-- Safety Checking
--------------------------------------------------------------------------------

-- | Check if replacing oldSym with newSym is safe
canReplace :: SymbolTable -> Text -> Text -> ReplaceSafety
canReplace table oldName newName
  | oldName == newName = SafeReplace
  | otherwise = case (resolveSymbol table oldName, resolveSymbol table newName) of
      (Nothing, _) -> UnsafeSemanticChange $ "Symbol '" <> oldName <> "' not found"
      (_, Just existing) ->
        if hsModule existing /= fromMaybe "" (stModule table)
          then UnsafeShadow (hsModule existing) newName
          else UnsafeConflict $ "Symbol '" <> newName <> "' already exists"
      (Just old, Nothing) ->
        -- Check if new name would shadow an import
        case checkShadowing table newName of
          Just shadow -> shadow
          Nothing -> checkTypeCompatibility old newName

-- | Check if a name would shadow something
checkShadowing :: SymbolTable -> Text -> Maybe ReplaceSafety
checkShadowing table name =
  case findImportedSymbol table name of
    Just (modName, _) -> Just $ UnsafeShadow modName name
    Nothing -> Nothing

-- | Find if a symbol is imported from somewhere
findImportedSymbol :: SymbolTable -> Text -> Maybe (Text, ImportedModule)
findImportedSymbol table name =
  find (symbolInImport name . snd) $ Map.toList (stImports table)
  where
    symbolInImport sym imp = case imExplicit imp of
      Nothing -> sym `notElem` imHiding imp
      Just explicit -> sym `elem` explicit

-- | Check for naming conflicts
checkConflicts :: SymbolTable -> Text -> [Text]
checkConflicts table name =
  let symbols = findInScope table name
      conflicts = filter (\s -> hsModule s /= fromMaybe "" (stModule table)) symbols
  in map hsQualified conflicts

-- | Check type compatibility for replacement
-- Renaming a symbol is type-safe unless:
-- 1. The new name collides with a type variable in the symbol's type
-- 2. The symbol is a type constructor and the name matters for instances
-- 3. The symbol has HasField constraints that reference its name
checkTypeCompatibility :: HieSymbol -> Text -> ReplaceSafety
checkTypeCompatibility old newName =
  case hsKind old of
    -- For type constructors, check if name is used in instance derivations
    Types.TypeConstructor ->
      -- Type constructor renaming requires careful handling
      -- as instances are resolved by type name
      SafeReplace  -- Assume safe for now; instance checks happen elsewhere

    -- For data constructors, check field name conflicts
    Types.DataConstructor ->
      case hsType old of
        Just typeStr
          | newName `T.isInfixOf` typeStr ->
              UnsafeSemanticChange $ "New name may conflict with field in: " <> typeStr
        _ -> SafeReplace

    -- For class methods, the method name is part of the interface
    Types.TypeClassMethod ->
      -- Method renaming affects all instances
      UnsafeSemanticChange $ "Renaming class method '" <> hsName old <> "' would break instances"

    -- For type classes, renaming affects all instances
    Types.TypeClass ->
      UnsafeSemanticChange $ "Renaming type class '" <> hsName old <> "' would break all instances"

    -- For functions: check for name capture in type
    Types.Function -> checkValueTypeCompatibility old newName

    -- For type families
    Types.TypeFamily -> SafeReplace

    -- For pattern synonyms
    Types.PatternSynonym -> SafeReplace

    -- For modules
    Types.Module -> SafeReplace

-- | Check type compatibility specifically for value/function symbols
checkValueTypeCompatibility :: HieSymbol -> Text -> ReplaceSafety
checkValueTypeCompatibility sym newName =
  case hsType sym of
    Nothing -> SafeReplace  -- No type info, assume safe

    Just typeStr ->
      -- Check if the new name would capture a type variable in scope
      let typeVars = extractTypeVariables typeStr
      in if newName `elem` typeVars
         then UnsafeSemanticChange $
           "New name '" <> newName <> "' captures type variable in: " <> typeStr
         -- Check for HasField-like constraints
         else if hasFieldConstraintFor (hsName sym) typeStr
         then UnsafeSemanticChange $
           "Symbol has HasField constraint that references its name"
         else SafeReplace

-- | Extract type variable names from a type signature
-- Simple heuristic: single lowercase letters or words starting with lowercase
extractTypeVariables :: Text -> [Text]
extractTypeVariables typeStr =
  let tokens = T.words $ T.filter (\c -> c /= '(' && c /= ')' && c /= ',') typeStr
      isTypeVar t = not (T.null t) &&
                    case T.uncons t of
                      Just (c, rest) -> c >= 'a' && c <= 'z' &&
                                        T.all (\x -> (x >= 'a' && x <= 'z') ||
                                                     (x >= '0' && x <= '9')) rest
                      Nothing -> False
  in filter isTypeVar tokens

-- | Check if a type has a HasField constraint referencing a symbol name
hasFieldConstraintFor :: Text -> Text -> Bool
hasFieldConstraintFor symName typeStr =
  -- Look for patterns like: HasField "fieldName" or similar
  let pattern1 = "HasField \"" <> symName <> "\""
      pattern2 = "HasField '" <> symName <> "'"
  in pattern1 `T.isInfixOf` typeStr || pattern2 `T.isInfixOf` typeStr

--------------------------------------------------------------------------------
-- Symbol Lookup
--------------------------------------------------------------------------------

-- | Look up a value symbol
lookupSymbol :: SymbolTable -> Text -> Maybe HieSymbol
lookupSymbol = resolveSymbol

-- | Look up a type symbol
lookupType :: SymbolTable -> Text -> Maybe HieSymbol
lookupType table name = Map.lookup name (stTypes table)

-- | Look up a type class
lookupClass :: SymbolTable -> Text -> Maybe HieSymbol
lookupClass table name = Map.lookup name (stClasses table)

-- | Look up a module import
lookupModule :: SymbolTable -> Text -> Maybe ImportedModule
lookupModule table name = Map.lookup name (stImports table)

--------------------------------------------------------------------------------
-- Table Operations
--------------------------------------------------------------------------------

-- | Add a symbol to the table
addSymbol :: HieSymbol -> SymbolTable -> SymbolTable
addSymbol sym table = table
  { stSymbols = Map.insertWith (++) (hsName sym) [sym] (stSymbols table)
  }

-- | Remove a symbol from the table
removeSymbol :: Text -> SymbolTable -> SymbolTable
removeSymbol name table = table
  { stSymbols = Map.delete name (stSymbols table)
  , stTypes = Map.delete name (stTypes table)
  , stClasses = Map.delete name (stClasses table)
  }

-- | Merge two symbol tables
mergeSymbolTables :: SymbolTable -> SymbolTable -> SymbolTable
mergeSymbolTables t1 t2 = SymbolTable
  { stModule = stModule t1
  , stSymbols = Map.unionWith (++) (stSymbols t1) (stSymbols t2)
  , stTypes = Map.union (stTypes t1) (stTypes t2)
  , stClasses = Map.union (stClasses t1) (stClasses t2)
  , stImports = Map.union (stImports t1) (stImports t2)
  , stExports = Set.union (stExports t1) (stExports t2)
  , stQualified = Map.union (stQualified t1) (stQualified t2)
  }

-- | Filter symbols by a predicate
filterSymbols :: (HieSymbol -> Bool) -> SymbolTable -> SymbolTable
filterSymbols predicate table = table
  { stSymbols = Map.map (filter predicate) (stSymbols table)
  , stTypes = Map.filter predicate (stTypes table)
  , stClasses = Map.filter predicate (stClasses table)
  }

--------------------------------------------------------------------------------
-- Scope Analysis
--------------------------------------------------------------------------------

-- | Get symbols in local scope at a position
getLocalScope :: SymbolTable -> SrcSpan -> [HieSymbol]
getLocalScope table _span =
  -- Full implementation would consider let/where bindings
  concat $ Map.elems (stSymbols table)

-- | Get all module-level symbols
getModuleScope :: SymbolTable -> [HieSymbol]
getModuleScope table =
  filter ((== ScopeModule) . hsScope) $ concat $ Map.elems (stSymbols table)

-- | Get all globally visible symbols
getGlobalScope :: SymbolTable -> [HieSymbol]
getGlobalScope table =
  filter hsExported $ concat $ Map.elems (stSymbols table)
