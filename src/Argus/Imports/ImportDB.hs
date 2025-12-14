{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Imports.ImportDB
-- Description : Database for symbol-to-import resolution
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides a database that maps symbols (functions, types, classes,
-- operators) to their source modules and import specifications. This enables
-- auto-fixes to automatically add required imports.
--
-- == Features
--
-- * Symbol-to-module mapping for common Haskell libraries
-- * Support for qualified imports
-- * Prelude alternative mappings (safe-exceptions, unliftio, etc.)
-- * Configurable symbol sources
-- * Efficient trie-based lookup
--
-- == Usage
--
-- @
-- -- Lookup a symbol
-- case lookupSymbol db "headMay" of
--   Just info -> addImport (siModule info) (siSymbol info)
--   Nothing -> warn "Unknown symbol"
-- @
module Argus.Imports.ImportDB
  ( -- * Import Database
    ImportDB (..)
  , emptyDB
  , defaultDB
  , mergeDBs

    -- * Symbol Information
  , SymbolInfo (..)
  , SymbolKind (..)
  , ImportSpec (..)

    -- * Lookup
  , lookupSymbol
  , lookupSymbolExact
  , lookupModule
  , findSymbolCandidates

    -- * Database Building
  , addSymbol
  , addModule
  , loadFromFile
  , loadBuiltinDB

    -- * Module Information
  , ModuleInfo (..)
  , ModuleCategory (..)

    -- * Common Libraries
  , baseSymbols
  , safeSymbols
  , containerSymbols
  , textSymbols
  , bytestringSymbols
  , vectorSymbols
  , mtlSymbols
  ) where

import Control.Monad (forM_)
import Data.Aeson (ToJSON, FromJSON, ToJSONKey(..), FromJSONKey(..))
import Data.Aeson.Types qualified as AT
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)
import Toml (TomlCodec, (.=))
import Toml qualified as TOML

import Argus.Types (ImportSymbol(..), ImportSymbolType(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Kind of symbol in the database
data SymbolKind
  = SKFunction           -- ^ Regular function
  | SKOperator           -- ^ Operator (needs parens in import)
  | SKType               -- ^ Type
  | SKClass              -- ^ Type class
  | SKConstructor Text   -- ^ Data constructor (with parent type)
  | SKPattern            -- ^ Pattern synonym
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | How to import a symbol
data ImportSpec = ImportSpec
  { isModule      :: Text               -- ^ Module to import from
  , isQualified   :: Maybe Text         -- ^ Qualified name (if any)
  , isHiding      :: Bool               -- ^ Import hiding?
  , isExplicit    :: [ImportSymbol]     -- ^ Explicit import list
  , isPackage     :: Maybe Text         -- ^ Package constraint
  } deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Information about a symbol
data SymbolInfo = SymbolInfo
  { siName        :: Text               -- ^ Symbol name
  , siKind        :: SymbolKind         -- ^ Kind of symbol
  , siModule      :: Text               -- ^ Primary module
  , siImports     :: [ImportSpec]       -- ^ Possible import specs
  , siDescription :: Maybe Text         -- ^ Optional description
  , siSince       :: Maybe Text         -- ^ Version introduced
  , siDeprecated  :: Maybe Text         -- ^ Deprecation notice
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Category of a module
data ModuleCategory
  = MCBase              -- ^ GHC base library
  | MCContainers        -- ^ containers package
  | MCText              -- ^ text package
  | MCByteString        -- ^ bytestring package
  | MCVector            -- ^ vector package
  | MCMTL               -- ^ mtl/transformers
  | MCSafe              -- ^ safe/safe-exceptions
  | MCAsync             -- ^ async/unliftio-async
  | MCLens              -- ^ lens/optics
  | MCProject           -- ^ Project-local module
  | MCOther Text        -- ^ Other package
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Convert ModuleCategory to Text for JSON key
moduleCategoryToText :: ModuleCategory -> Text
moduleCategoryToText = \case
  MCBase -> "base"
  MCContainers -> "containers"
  MCText -> "text"
  MCByteString -> "bytestring"
  MCVector -> "vector"
  MCMTL -> "mtl"
  MCSafe -> "safe"
  MCAsync -> "async"
  MCLens -> "lens"
  MCProject -> "project"
  MCOther t -> "other:" <> t

-- | Parse ModuleCategory from Text
textToModuleCategory :: Text -> ModuleCategory
textToModuleCategory t = case t of
  "base" -> MCBase
  "containers" -> MCContainers
  "text" -> MCText
  "bytestring" -> MCByteString
  "vector" -> MCVector
  "mtl" -> MCMTL
  "safe" -> MCSafe
  "async" -> MCAsync
  "lens" -> MCLens
  "project" -> MCProject
  other | T.isPrefixOf "other:" other -> MCOther (T.drop 6 other)
        | otherwise -> MCOther other

instance ToJSONKey ModuleCategory where
  toJSONKey = AT.toJSONKeyText moduleCategoryToText

instance FromJSONKey ModuleCategory where
  fromJSONKey = AT.FromJSONKeyTextParser (pure . textToModuleCategory)

-- | Information about a module
data ModuleInfo = ModuleInfo
  { miName        :: Text               -- ^ Full module name
  , miCategory    :: ModuleCategory     -- ^ Category
  , miSymbols     :: Set Text           -- ^ Exported symbols
  , miReexports   :: [Text]             -- ^ Re-exported modules
  , miPreferred   :: Bool               -- ^ Preferred over alternatives
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The import database
data ImportDB = ImportDB
  { dbSymbols     :: Map Text [SymbolInfo]   -- ^ Symbol -> info mapping
  , dbModules     :: Map Text ModuleInfo     -- ^ Module -> info mapping
  , dbAliases     :: Map Text Text           -- ^ Alias -> canonical name
  , dbCategories  :: Map ModuleCategory (Set Text)  -- ^ Category -> modules
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Database Operations
--------------------------------------------------------------------------------

-- | Empty database
emptyDB :: ImportDB
emptyDB = ImportDB
  { dbSymbols = Map.empty
  , dbModules = Map.empty
  , dbAliases = Map.empty
  , dbCategories = Map.empty
  }

-- | Merge two databases, preferring the second on conflicts
mergeDBs :: ImportDB -> ImportDB -> ImportDB
mergeDBs db1 db2 = ImportDB
  { dbSymbols = Map.unionWith (<>) (dbSymbols db1) (dbSymbols db2)
  , dbModules = Map.union (dbModules db2) (dbModules db1)
  , dbAliases = Map.union (dbAliases db2) (dbAliases db1)
  , dbCategories = Map.unionWith Set.union (dbCategories db1) (dbCategories db2)
  }

-- | Add a symbol to the database
addSymbol :: SymbolInfo -> ImportDB -> ImportDB
addSymbol si db = db
  { dbSymbols = Map.insertWith (<>) (siName si) [si] (dbSymbols db)
  }

-- | Add a module to the database
addModule :: ModuleInfo -> ImportDB -> ImportDB
addModule mi db = db
  { dbModules = Map.insert (miName mi) mi (dbModules db)
  , dbCategories = Map.insertWith Set.union (miCategory mi)
                     (Set.singleton (miName mi)) (dbCategories db)
  }

--------------------------------------------------------------------------------
-- Lookup Operations
--------------------------------------------------------------------------------

-- | Look up a symbol by name (may return multiple candidates)
lookupSymbol :: ImportDB -> Text -> [SymbolInfo]
lookupSymbol db name = Map.findWithDefault [] name (dbSymbols db)

-- | Look up a symbol with exact module match
lookupSymbolExact :: ImportDB -> Text -> Text -> Maybe SymbolInfo
lookupSymbolExact db name modName =
  case lookupSymbol db name of
    infos -> find (\si -> siModule si == modName) infos
  where
    find _ [] = Nothing
    find p (x:xs)
      | p x = Just x
      | otherwise = find p xs

-- | Look up a module by name
lookupModule :: ImportDB -> Text -> Maybe ModuleInfo
lookupModule db name = Map.lookup name (dbModules db)

-- | Find all symbols matching a prefix
findSymbolCandidates :: ImportDB -> Text -> [SymbolInfo]
findSymbolCandidates db prefix =
  concatMap snd $ Map.toList $ Map.filterWithKey (\k _ -> prefix `T.isPrefixOf` k) (dbSymbols db)

--------------------------------------------------------------------------------
-- File Loading
--------------------------------------------------------------------------------

-- | Load database from a TOML file
--
-- The TOML file format:
--
-- @
-- [[symbols]]
-- name = "myFunction"
-- kind = "function"
-- module = "MyModule"
-- description = "A useful function"
--
-- [[modules]]
-- name = "MyModule"
-- category = "project"
-- symbols = ["myFunction", "myType"]
-- preferred = true
--
-- [aliases]
-- oldName = "newName"
-- @
loadFromFile :: FilePath -> IO (Either Text ImportDB)
loadFromFile path = do
  content <- TIO.readFile path
  case TOML.decode importDBCodec content of
    Left err  -> pure $ Left $ TOML.prettyTomlDecodeErrors err
    Right db  -> pure $ Right db

--------------------------------------------------------------------------------
-- TOML Codecs
--------------------------------------------------------------------------------

-- | Codec for the top-level ImportDB
importDBCodec :: TomlCodec ImportDB
importDBCodec = ImportDB
  <$> symbolsMapCodec                                        .= dbSymbols
  <*> modulesMapCodec                                        .= dbModules
  <*> TOML.tableMap TOML._KeyText TOML.text "aliases"        .= dbAliases
  <*> pure Map.empty                                         .= dbCategories
  where
    -- Symbols are provided as a list, we group them by name
    symbolsMapCodec :: TomlCodec (Map Text [SymbolInfo])
    symbolsMapCodec = TOML.dimap toList fromList $ TOML.list symbolInfoCodec "symbols"
      where
        toList :: Map Text [SymbolInfo] -> [SymbolInfo]
        toList = concatMap snd . Map.toList

        fromList :: [SymbolInfo] -> Map Text [SymbolInfo]
        fromList = foldr (\si m -> Map.insertWith (<>) (siName si) [si] m) Map.empty

    -- Modules are provided as a list, we key them by name
    modulesMapCodec :: TomlCodec (Map Text ModuleInfo)
    modulesMapCodec = TOML.dimap toList fromList $ TOML.list moduleInfoCodec "modules"
      where
        toList :: Map Text ModuleInfo -> [ModuleInfo]
        toList = Map.elems

        fromList :: [ModuleInfo] -> Map Text ModuleInfo
        fromList = foldr (\mi m -> Map.insert (miName mi) mi m) Map.empty

-- | Codec for SymbolInfo
symbolInfoCodec :: TomlCodec SymbolInfo
symbolInfoCodec = SymbolInfo
  <$> TOML.text "name"                                       .= siName
  <*> symbolKindCodec "kind"                                 .= siKind
  <*> TOML.text "module"                                     .= siModule
  <*> TOML.list importSpecCodec "imports"                    .= siImports
  <*> TOML.dioptional (TOML.text "description")              .= siDescription
  <*> TOML.dioptional (TOML.text "since")                    .= siSince
  <*> TOML.dioptional (TOML.text "deprecated")               .= siDeprecated

-- | Codec for SymbolKind
symbolKindCodec :: TOML.Key -> TomlCodec SymbolKind
symbolKindCodec = TOML.textBy symbolKindToText textToSymbolKind
  where
    symbolKindToText :: SymbolKind -> Text
    symbolKindToText = \case
      SKFunction       -> "function"
      SKOperator       -> "operator"
      SKType           -> "type"
      SKClass          -> "class"
      SKConstructor p  -> "constructor:" <> p
      SKPattern        -> "pattern"

    textToSymbolKind :: Text -> Either Text SymbolKind
    textToSymbolKind t = case t of
      "function" -> Right SKFunction
      "operator" -> Right SKOperator
      "type"     -> Right SKType
      "class"    -> Right SKClass
      "pattern"  -> Right SKPattern
      other | T.isPrefixOf "constructor:" other ->
        Right $ SKConstructor (T.drop 12 other)
      other -> Left $ "Unknown SymbolKind: " <> other

-- | Codec for ImportSpec
importSpecCodec :: TomlCodec ImportSpec
importSpecCodec = ImportSpec
  <$> TOML.text "module"                                     .= isModule
  <*> TOML.dioptional (TOML.text "qualified")                .= isQualified
  <*> boolWithDefault False "hiding"                         .= isHiding
  <*> listWithDefault [] importSymbolCodec "explicit"        .= isExplicit
  <*> TOML.dioptional (TOML.text "package")                  .= isPackage

-- | Codec for ImportSymbol
importSymbolCodec :: TomlCodec ImportSymbol
importSymbolCodec = ImportSymbol
  <$> TOML.text "name"                                       .= isymName
  <*> importSymbolTypeCodec "type"                           .= isymType
  <*> TOML.arrayOf TOML._Text "children"                     .= isymChildren

-- | Codec for ImportSymbolType
importSymbolTypeCodec :: TOML.Key -> TomlCodec ImportSymbolType
importSymbolTypeCodec = TOML.textBy istToText textToIST
  where
    istToText :: ImportSymbolType -> Text
    istToText = \case
      ISTFunction    -> "function"
      ISTOperator    -> "operator"
      ISTType        -> "type"
      ISTClass       -> "class"
      ISTConstructor -> "constructor"
      ISTPattern     -> "pattern"

    textToIST :: Text -> Either Text ImportSymbolType
    textToIST t = case t of
      "function"    -> Right ISTFunction
      "operator"    -> Right ISTOperator
      "type"        -> Right ISTType
      "class"       -> Right ISTClass
      "constructor" -> Right ISTConstructor
      "pattern"     -> Right ISTPattern
      other         -> Left $ "Unknown ImportSymbolType: " <> other

-- | Codec for ModuleInfo
moduleInfoCodec :: TomlCodec ModuleInfo
moduleInfoCodec = ModuleInfo
  <$> TOML.text "name"                                       .= miName
  <*> moduleCategoryCodec "category"                         .= miCategory
  <*> setWithDefault Set.empty "symbols"                     .= miSymbols
  <*> textArrayWithDefault [] "reexports"                    .= miReexports
  <*> boolWithDefault False "preferred"                      .= miPreferred

-- | Codec for ModuleCategory
moduleCategoryCodec :: TOML.Key -> TomlCodec ModuleCategory
moduleCategoryCodec = TOML.textBy moduleCategoryToText textToModuleCategoryEither
  where
    textToModuleCategoryEither :: Text -> Either Text ModuleCategory
    textToModuleCategoryEither = Right . textToModuleCategory

--------------------------------------------------------------------------------
-- TOML Helper Codecs
--------------------------------------------------------------------------------

-- | Boolean with default value
boolWithDefault :: Bool -> TOML.Key -> TomlCodec Bool
boolWithDefault def key = TOML.dimap Just (fromMaybe def) $ TOML.dioptional (TOML.bool key)

-- | Text array with default value
textArrayWithDefault :: [Text] -> TOML.Key -> TomlCodec [Text]
textArrayWithDefault def key = TOML.dimap Just (fromMaybe def) $ TOML.dioptional (TOML.arrayOf TOML._Text key)

-- | Set with default value
setWithDefault :: Set Text -> TOML.Key -> TomlCodec (Set Text)
setWithDefault def key = TOML.dimap (Just . Set.toList) (Set.fromList . fromMaybe (Set.toList def))
                       $ TOML.dioptional (TOML.arrayOf TOML._Text key)

-- | List of items with default value
listWithDefault :: [a] -> TomlCodec a -> TOML.Key -> TomlCodec [a]
listWithDefault def codec key = TOML.dimap Just (fromMaybe def) $ TOML.dioptional (TOML.list codec key)

--------------------------------------------------------------------------------
-- Built-in Database
--------------------------------------------------------------------------------

-- | Global mutable reference for lazy loading
{-# NOINLINE builtinDBRef #-}
builtinDBRef :: IORef (Maybe ImportDB)
builtinDBRef = unsafePerformIO $ newIORef Nothing

-- | Load the built-in database (cached)
loadBuiltinDB :: IO ImportDB
loadBuiltinDB = do
  cached <- readIORef builtinDBRef
  case cached of
    Just db -> pure db
    Nothing -> do
      let db = buildBuiltinDB
      modifyIORef' builtinDBRef (const (Just db))
      pure db

-- | Build the complete built-in database
buildBuiltinDB :: ImportDB
buildBuiltinDB = foldr ($) emptyDB
  [ addSymbols baseSymbols
  , addSymbols safeSymbols
  , addSymbols containerSymbols
  , addSymbols textSymbols
  , addSymbols bytestringSymbols
  , addSymbols vectorSymbols
  , addSymbols mtlSymbols
  ]
  where
    addSymbols :: [SymbolInfo] -> ImportDB -> ImportDB
    addSymbols syms db = foldr addSymbol db syms

-- | The default database (includes all built-ins)
defaultDB :: ImportDB
defaultDB = unsafePerformIO loadBuiltinDB
{-# NOINLINE defaultDB #-}

--------------------------------------------------------------------------------
-- Base Library Symbols
--------------------------------------------------------------------------------

baseSymbols :: [SymbolInfo]
baseSymbols =
  -- Data.Maybe
  [ mkSymbol "fromMaybe" SKFunction "Data.Maybe"
  , mkSymbol "maybe" SKFunction "Data.Maybe"
  , mkSymbol "isJust" SKFunction "Data.Maybe"
  , mkSymbol "isNothing" SKFunction "Data.Maybe"
  , mkSymbol "catMaybes" SKFunction "Data.Maybe"
  , mkSymbol "mapMaybe" SKFunction "Data.Maybe"
  , mkSymbol "listToMaybe" SKFunction "Data.Maybe"
  , mkSymbol "maybeToList" SKFunction "Data.Maybe"

  -- Data.Either
  , mkSymbol "either" SKFunction "Data.Either"
  , mkSymbol "isLeft" SKFunction "Data.Either"
  , mkSymbol "isRight" SKFunction "Data.Either"
  , mkSymbol "fromLeft" SKFunction "Data.Either"
  , mkSymbol "fromRight" SKFunction "Data.Either"
  , mkSymbol "lefts" SKFunction "Data.Either"
  , mkSymbol "rights" SKFunction "Data.Either"
  , mkSymbol "partitionEithers" SKFunction "Data.Either"

  -- Data.List
  , mkSymbol "sort" SKFunction "Data.List"
  , mkSymbol "sortBy" SKFunction "Data.List"
  , mkSymbol "sortOn" SKFunction "Data.List"
  , mkSymbol "group" SKFunction "Data.List"
  , mkSymbol "groupBy" SKFunction "Data.List"
  , mkSymbol "nub" SKFunction "Data.List"
  , mkSymbol "nubBy" SKFunction "Data.List"
  , mkSymbol "intercalate" SKFunction "Data.List"
  , mkSymbol "intersperse" SKFunction "Data.List"
  , mkSymbol "transpose" SKFunction "Data.List"
  , mkSymbol "subsequences" SKFunction "Data.List"
  , mkSymbol "permutations" SKFunction "Data.List"
  , mkSymbol "find" SKFunction "Data.List"
  , mkSymbol "findIndex" SKFunction "Data.List"
  , mkSymbol "findIndices" SKFunction "Data.List"
  , mkSymbol "elemIndex" SKFunction "Data.List"
  , mkSymbol "elemIndices" SKFunction "Data.List"
  , mkSymbol "partition" SKFunction "Data.List"
  , mkSymbol "stripPrefix" SKFunction "Data.List"
  , mkSymbol "isPrefixOf" SKFunction "Data.List"
  , mkSymbol "isSuffixOf" SKFunction "Data.List"
  , mkSymbol "isInfixOf" SKFunction "Data.List"
  , mkSymbol "unfoldr" SKFunction "Data.List"

  -- Data.List.NonEmpty
  , mkType "NonEmpty" "Data.List.NonEmpty"
  , mkSymbol "nonEmpty" SKFunction "Data.List.NonEmpty"
  , mkSymbol "toList" SKFunction "Data.List.NonEmpty"
  , mkOperator "|:" "Data.List.NonEmpty"

  -- Data.Foldable
  , mkSymbol "toList" SKFunction "Data.Foldable"
  , mkSymbol "for_" SKFunction "Data.Foldable"
  , mkSymbol "traverse_" SKFunction "Data.Foldable"
  , mkSymbol "sequenceA_" SKFunction "Data.Foldable"
  , mkSymbol "fold" SKFunction "Data.Foldable"
  , mkSymbol "foldMap'" SKFunction "Data.Foldable"
  , mkSymbol "forM_" SKFunction "Data.Foldable"
  , mkSymbol "mapM_" SKFunction "Data.Foldable"
  , mkSymbol "asum" SKFunction "Data.Foldable"

  -- Data.Traversable
  , mkSymbol "for" SKFunction "Data.Traversable"
  , mkSymbol "forM" SKFunction "Data.Traversable"
  , mkSymbol "mapAccumL" SKFunction "Data.Traversable"
  , mkSymbol "mapAccumR" SKFunction "Data.Traversable"

  -- Data.Function
  , mkSymbol "on" SKFunction "Data.Function"
  , mkSymbol "fix" SKFunction "Data.Function"
  , mkOperator "&" "Data.Function"

  -- Data.Functor
  , mkSymbol "void" SKFunction "Data.Functor"
  , mkOperator "<$>" "Data.Functor"
  , mkOperator "$>" "Data.Functor"
  , mkOperator "<&>" "Data.Functor"

  -- Data.Bifunctor
  , mkClass "Bifunctor" "Data.Bifunctor"
  , mkSymbol "bimap" SKFunction "Data.Bifunctor"
  , mkSymbol "first" SKFunction "Data.Bifunctor"
  , mkSymbol "second" SKFunction "Data.Bifunctor"

  -- Control.Monad
  , mkSymbol "when" SKFunction "Control.Monad"
  , mkSymbol "unless" SKFunction "Control.Monad"
  , mkSymbol "guard" SKFunction "Control.Monad"
  , mkSymbol "join" SKFunction "Control.Monad"
  , mkSymbol "filterM" SKFunction "Control.Monad"
  , mkSymbol "foldM" SKFunction "Control.Monad"
  , mkSymbol "foldM_" SKFunction "Control.Monad"
  , mkSymbol "replicateM" SKFunction "Control.Monad"
  , mkSymbol "replicateM_" SKFunction "Control.Monad"
  , mkSymbol "forever" SKFunction "Control.Monad"
  , mkSymbol "zipWithM" SKFunction "Control.Monad"
  , mkSymbol "zipWithM_" SKFunction "Control.Monad"
  , mkOperator "<=<" "Control.Monad"
  , mkOperator ">=>" "Control.Monad"

  -- Control.Applicative
  , mkSymbol "liftA2" SKFunction "Control.Applicative"
  , mkSymbol "liftA3" SKFunction "Control.Applicative"
  , mkSymbol "optional" SKFunction "Control.Applicative"
  , mkOperator "<|>" "Control.Applicative"
  , mkSymbol "empty" SKFunction "Control.Applicative"
  , mkSymbol "some" SKFunction "Control.Applicative"
  , mkSymbol "many" SKFunction "Control.Applicative"

  -- Data.Tuple
  , mkSymbol "fst" SKFunction "Data.Tuple"
  , mkSymbol "snd" SKFunction "Data.Tuple"
  , mkSymbol "curry" SKFunction "Data.Tuple"
  , mkSymbol "uncurry" SKFunction "Data.Tuple"
  , mkSymbol "swap" SKFunction "Data.Tuple"

  -- Data.Ord
  , mkSymbol "comparing" SKFunction "Data.Ord"
  , mkSymbol "Down" SKFunction "Data.Ord"
  , mkType "Down" "Data.Ord"

  -- Data.Char
  , mkSymbol "isAlpha" SKFunction "Data.Char"
  , mkSymbol "isAlphaNum" SKFunction "Data.Char"
  , mkSymbol "isDigit" SKFunction "Data.Char"
  , mkSymbol "isLower" SKFunction "Data.Char"
  , mkSymbol "isUpper" SKFunction "Data.Char"
  , mkSymbol "isSpace" SKFunction "Data.Char"
  , mkSymbol "toLower" SKFunction "Data.Char"
  , mkSymbol "toUpper" SKFunction "Data.Char"
  , mkSymbol "ord" SKFunction "Data.Char"
  , mkSymbol "chr" SKFunction "Data.Char"

  -- Text.Read
  , mkSymbol "readMaybe" SKFunction "Text.Read"
  , mkSymbol "readEither" SKFunction "Text.Read"

  -- Data.Void
  , mkType "Void" "Data.Void"
  , mkSymbol "absurd" SKFunction "Data.Void"

  -- Data.Proxy
  , mkType "Proxy" "Data.Proxy"

  -- Data.Coerce
  , mkSymbol "coerce" SKFunction "Data.Coerce"

  -- Data.IORef
  , mkType "IORef" "Data.IORef"
  , mkSymbol "newIORef" SKFunction "Data.IORef"
  , mkSymbol "readIORef" SKFunction "Data.IORef"
  , mkSymbol "writeIORef" SKFunction "Data.IORef"
  , mkSymbol "modifyIORef'" SKFunction "Data.IORef"
  , mkSymbol "atomicModifyIORef'" SKFunction "Data.IORef"

  -- Data.STRef
  , mkType "STRef" "Data.STRef"
  , mkSymbol "newSTRef" SKFunction "Data.STRef"
  , mkSymbol "readSTRef" SKFunction "Data.STRef"
  , mkSymbol "writeSTRef" SKFunction "Data.STRef"
  , mkSymbol "modifySTRef'" SKFunction "Data.STRef"

  -- Control.Concurrent
  , mkSymbol "forkIO" SKFunction "Control.Concurrent"
  , mkSymbol "forkFinally" SKFunction "Control.Concurrent"
  , mkSymbol "killThread" SKFunction "Control.Concurrent"
  , mkSymbol "threadDelay" SKFunction "Control.Concurrent"
  , mkSymbol "yield" SKFunction "Control.Concurrent"
  , mkType "ThreadId" "Control.Concurrent"

  -- Control.Concurrent.MVar
  , mkType "MVar" "Control.Concurrent.MVar"
  , mkSymbol "newMVar" SKFunction "Control.Concurrent.MVar"
  , mkSymbol "newEmptyMVar" SKFunction "Control.Concurrent.MVar"
  , mkSymbol "takeMVar" SKFunction "Control.Concurrent.MVar"
  , mkSymbol "putMVar" SKFunction "Control.Concurrent.MVar"
  , mkSymbol "readMVar" SKFunction "Control.Concurrent.MVar"
  , mkSymbol "modifyMVar_" SKFunction "Control.Concurrent.MVar"
  , mkSymbol "modifyMVar" SKFunction "Control.Concurrent.MVar"
  , mkSymbol "withMVar" SKFunction "Control.Concurrent.MVar"

  -- Control.Concurrent.STM
  , mkType "STM" "Control.Concurrent.STM"
  , mkType "TVar" "Control.Concurrent.STM"
  , mkSymbol "atomically" SKFunction "Control.Concurrent.STM"
  , mkSymbol "newTVar" SKFunction "Control.Concurrent.STM"
  , mkSymbol "readTVar" SKFunction "Control.Concurrent.STM"
  , mkSymbol "writeTVar" SKFunction "Control.Concurrent.STM"
  , mkSymbol "modifyTVar'" SKFunction "Control.Concurrent.STM"
  , mkSymbol "retry" SKFunction "Control.Concurrent.STM"
  , mkSymbol "orElse" SKFunction "Control.Concurrent.STM"
  , mkSymbol "check" SKFunction "Control.Concurrent.STM"
  , mkType "TMVar" "Control.Concurrent.STM"
  , mkType "TQueue" "Control.Concurrent.STM"
  , mkType "TBQueue" "Control.Concurrent.STM"
  , mkType "TChan" "Control.Concurrent.STM"

  -- Control.Exception
  , mkType "Exception" "Control.Exception"
  , mkType "SomeException" "Control.Exception"
  , mkSymbol "throw" SKFunction "Control.Exception"
  , mkSymbol "throwIO" SKFunction "Control.Exception"
  , mkSymbol "catch" SKFunction "Control.Exception"
  , mkSymbol "catches" SKFunction "Control.Exception"
  , mkSymbol "try" SKFunction "Control.Exception"
  , mkSymbol "bracket" SKFunction "Control.Exception"
  , mkSymbol "bracket_" SKFunction "Control.Exception"
  , mkSymbol "finally" SKFunction "Control.Exception"
  , mkSymbol "onException" SKFunction "Control.Exception"
  , mkSymbol "mask" SKFunction "Control.Exception"
  , mkSymbol "mask_" SKFunction "Control.Exception"
  , mkSymbol "uninterruptibleMask" SKFunction "Control.Exception"
  , mkSymbol "uninterruptibleMask_" SKFunction "Control.Exception"
  , mkSymbol "evaluate" SKFunction "Control.Exception"
  , mkType "IOException" "Control.Exception"
  , mkType "AsyncException" "Control.Exception"

  -- System.IO
  , mkType "Handle" "System.IO"
  , mkType "IOMode" "System.IO"
  , mkSymbol "withFile" SKFunction "System.IO"
  , mkSymbol "openFile" SKFunction "System.IO"
  , mkSymbol "hClose" SKFunction "System.IO"
  , mkSymbol "hFlush" SKFunction "System.IO"
  , mkSymbol "hGetContents" SKFunction "System.IO"
  , mkSymbol "hPutStr" SKFunction "System.IO"
  , mkSymbol "hPutStrLn" SKFunction "System.IO"
  , mkSymbol "hPrint" SKFunction "System.IO"
  , mkSymbol "stdout" SKFunction "System.IO"
  , mkSymbol "stderr" SKFunction "System.IO"
  , mkSymbol "stdin" SKFunction "System.IO"

  -- Data.Typeable
  , mkClass "Typeable" "Data.Typeable"
  , mkType "TypeRep" "Data.Typeable"
  , mkSymbol "typeOf" SKFunction "Data.Typeable"
  , mkSymbol "typeRep" SKFunction "Data.Typeable"

  -- GHC.Generics
  , mkClass "Generic" "GHC.Generics"
  , mkType "Rep" "GHC.Generics"

  -- Data.Hashable
  , mkClass "Hashable" "Data.Hashable"
  , mkSymbol "hash" SKFunction "Data.Hashable"
  , mkSymbol "hashWithSalt" SKFunction "Data.Hashable"
  ]

--------------------------------------------------------------------------------
-- Safe Library Symbols
--------------------------------------------------------------------------------

safeSymbols :: [SymbolInfo]
safeSymbols =
  -- Safe
  [ mkSymbol "headMay" SKFunction "Safe"
  , mkSymbol "tailMay" SKFunction "Safe"
  , mkSymbol "initMay" SKFunction "Safe"
  , mkSymbol "lastMay" SKFunction "Safe"
  , mkSymbol "headDef" SKFunction "Safe"
  , mkSymbol "tailDef" SKFunction "Safe"
  , mkSymbol "initDef" SKFunction "Safe"
  , mkSymbol "lastDef" SKFunction "Safe"
  , mkSymbol "headNote" SKFunction "Safe"
  , mkSymbol "tailNote" SKFunction "Safe"
  , mkSymbol "initNote" SKFunction "Safe"
  , mkSymbol "lastNote" SKFunction "Safe"
  , mkSymbol "atMay" SKFunction "Safe"
  , mkSymbol "atDef" SKFunction "Safe"
  , mkSymbol "atNote" SKFunction "Safe"
  , mkSymbol "readMay" SKFunction "Safe"
  , mkSymbol "readDef" SKFunction "Safe"
  , mkSymbol "readNote" SKFunction "Safe"
  , mkSymbol "maximumMay" SKFunction "Safe"
  , mkSymbol "minimumMay" SKFunction "Safe"
  , mkSymbol "maximumDef" SKFunction "Safe"
  , mkSymbol "minimumDef" SKFunction "Safe"
  , mkSymbol "maximumByMay" SKFunction "Safe"
  , mkSymbol "minimumByMay" SKFunction "Safe"
  , mkSymbol "foldl1May" SKFunction "Safe"
  , mkSymbol "foldl1Def" SKFunction "Safe"
  , mkSymbol "foldr1May" SKFunction "Safe"
  , mkSymbol "foldr1Def" SKFunction "Safe"
  , mkSymbol "findJust" SKFunction "Safe"
  , mkSymbol "lookupJust" SKFunction "Safe"
  , mkSymbol "assertNote" SKFunction "Safe"

  -- Safe.Foldable
  , mkSymbol "findJust" SKFunction "Safe.Foldable"
  , mkSymbol "cycleMay" SKFunction "Safe.Foldable"

  -- Safe.Exact
  , mkSymbol "takeExact" SKFunction "Safe.Exact"
  , mkSymbol "dropExact" SKFunction "Safe.Exact"
  , mkSymbol "splitAtExact" SKFunction "Safe.Exact"
  , mkSymbol "zipExact" SKFunction "Safe.Exact"
  , mkSymbol "zipWithExact" SKFunction "Safe.Exact"
  ]

--------------------------------------------------------------------------------
-- Container Library Symbols
--------------------------------------------------------------------------------

containerSymbols :: [SymbolInfo]
containerSymbols =
  -- Data.Map.Strict
  [ mkType "Map" "Data.Map.Strict"
  , mkSymbol "empty" SKFunction "Data.Map.Strict"
  , mkSymbol "singleton" SKFunction "Data.Map.Strict"
  , mkSymbol "fromList" SKFunction "Data.Map.Strict"
  , mkSymbol "toList" SKFunction "Data.Map.Strict"
  , mkSymbol "insert" SKFunction "Data.Map.Strict"
  , mkSymbol "insertWith" SKFunction "Data.Map.Strict"
  , mkSymbol "delete" SKFunction "Data.Map.Strict"
  , mkSymbol "lookup" SKFunction "Data.Map.Strict"
  , mkSymbol "member" SKFunction "Data.Map.Strict"
  , mkSymbol "notMember" SKFunction "Data.Map.Strict"
  , mkSymbol "findWithDefault" SKFunction "Data.Map.Strict"
  , mkSymbol "size" SKFunction "Data.Map.Strict"
  , mkSymbol "null" SKFunction "Data.Map.Strict"
  , mkSymbol "keys" SKFunction "Data.Map.Strict"
  , mkSymbol "elems" SKFunction "Data.Map.Strict"
  , mkSymbol "union" SKFunction "Data.Map.Strict"
  , mkSymbol "unionWith" SKFunction "Data.Map.Strict"
  , mkSymbol "unions" SKFunction "Data.Map.Strict"
  , mkSymbol "difference" SKFunction "Data.Map.Strict"
  , mkSymbol "intersection" SKFunction "Data.Map.Strict"
  , mkSymbol "intersectionWith" SKFunction "Data.Map.Strict"
  , mkSymbol "filter" SKFunction "Data.Map.Strict"
  , mkSymbol "filterWithKey" SKFunction "Data.Map.Strict"
  , mkSymbol "mapWithKey" SKFunction "Data.Map.Strict"
  , mkSymbol "traverseWithKey" SKFunction "Data.Map.Strict"
  , mkSymbol "foldrWithKey" SKFunction "Data.Map.Strict"
  , mkSymbol "foldlWithKey'" SKFunction "Data.Map.Strict"
  , mkSymbol "alter" SKFunction "Data.Map.Strict"
  , mkOperator "!?" "Data.Map.Strict"

  -- Data.Set
  , mkType "Set" "Data.Set"
  , mkSymbol "empty" SKFunction "Data.Set"
  , mkSymbol "singleton" SKFunction "Data.Set"
  , mkSymbol "fromList" SKFunction "Data.Set"
  , mkSymbol "toList" SKFunction "Data.Set"
  , mkSymbol "insert" SKFunction "Data.Set"
  , mkSymbol "delete" SKFunction "Data.Set"
  , mkSymbol "member" SKFunction "Data.Set"
  , mkSymbol "notMember" SKFunction "Data.Set"
  , mkSymbol "size" SKFunction "Data.Set"
  , mkSymbol "null" SKFunction "Data.Set"
  , mkSymbol "union" SKFunction "Data.Set"
  , mkSymbol "unions" SKFunction "Data.Set"
  , mkSymbol "difference" SKFunction "Data.Set"
  , mkSymbol "intersection" SKFunction "Data.Set"
  , mkSymbol "filter" SKFunction "Data.Set"
  , mkSymbol "map" SKFunction "Data.Set"
  , mkSymbol "foldr" SKFunction "Data.Set"
  , mkSymbol "foldl'" SKFunction "Data.Set"

  -- Data.IntMap.Strict
  , mkType "IntMap" "Data.IntMap.Strict"
  , mkSymbol "empty" SKFunction "Data.IntMap.Strict"
  , mkSymbol "singleton" SKFunction "Data.IntMap.Strict"
  , mkSymbol "fromList" SKFunction "Data.IntMap.Strict"
  , mkSymbol "toList" SKFunction "Data.IntMap.Strict"
  , mkSymbol "insert" SKFunction "Data.IntMap.Strict"
  , mkSymbol "delete" SKFunction "Data.IntMap.Strict"
  , mkSymbol "lookup" SKFunction "Data.IntMap.Strict"
  , mkSymbol "member" SKFunction "Data.IntMap.Strict"
  , mkSymbol "size" SKFunction "Data.IntMap.Strict"
  , mkSymbol "null" SKFunction "Data.IntMap.Strict"

  -- Data.IntSet
  , mkType "IntSet" "Data.IntSet"
  , mkSymbol "empty" SKFunction "Data.IntSet"
  , mkSymbol "singleton" SKFunction "Data.IntSet"
  , mkSymbol "fromList" SKFunction "Data.IntSet"
  , mkSymbol "toList" SKFunction "Data.IntSet"
  , mkSymbol "insert" SKFunction "Data.IntSet"
  , mkSymbol "delete" SKFunction "Data.IntSet"
  , mkSymbol "member" SKFunction "Data.IntSet"
  , mkSymbol "size" SKFunction "Data.IntSet"
  , mkSymbol "null" SKFunction "Data.IntSet"

  -- Data.Sequence
  , mkType "Seq" "Data.Sequence"
  , mkSymbol "empty" SKFunction "Data.Sequence"
  , mkSymbol "singleton" SKFunction "Data.Sequence"
  , mkSymbol "fromList" SKFunction "Data.Sequence"
  , mkSymbol "toList" SKFunction "Data.Sequence"
  , mkOperator "|>" "Data.Sequence"
  , mkOperator "<|" "Data.Sequence"
  , mkOperator "><" "Data.Sequence"
  , mkSymbol "length" SKFunction "Data.Sequence"
  , mkSymbol "null" SKFunction "Data.Sequence"
  , mkSymbol "index" SKFunction "Data.Sequence"
  , mkSymbol "lookup" SKFunction "Data.Sequence"
  , mkSymbol "take" SKFunction "Data.Sequence"
  , mkSymbol "drop" SKFunction "Data.Sequence"
  , mkSymbol "splitAt" SKFunction "Data.Sequence"

  -- Data.HashMap.Strict
  , mkType "HashMap" "Data.HashMap.Strict"
  , mkSymbol "empty" SKFunction "Data.HashMap.Strict"
  , mkSymbol "singleton" SKFunction "Data.HashMap.Strict"
  , mkSymbol "fromList" SKFunction "Data.HashMap.Strict"
  , mkSymbol "toList" SKFunction "Data.HashMap.Strict"
  , mkSymbol "insert" SKFunction "Data.HashMap.Strict"
  , mkSymbol "delete" SKFunction "Data.HashMap.Strict"
  , mkSymbol "lookup" SKFunction "Data.HashMap.Strict"
  , mkSymbol "member" SKFunction "Data.HashMap.Strict"
  , mkSymbol "size" SKFunction "Data.HashMap.Strict"
  , mkSymbol "null" SKFunction "Data.HashMap.Strict"
  , mkSymbol "union" SKFunction "Data.HashMap.Strict"
  , mkSymbol "unionWith" SKFunction "Data.HashMap.Strict"
  , mkOperator "!?" "Data.HashMap.Strict"

  -- Data.HashSet
  , mkType "HashSet" "Data.HashSet"
  , mkSymbol "empty" SKFunction "Data.HashSet"
  , mkSymbol "singleton" SKFunction "Data.HashSet"
  , mkSymbol "fromList" SKFunction "Data.HashSet"
  , mkSymbol "toList" SKFunction "Data.HashSet"
  , mkSymbol "insert" SKFunction "Data.HashSet"
  , mkSymbol "delete" SKFunction "Data.HashSet"
  , mkSymbol "member" SKFunction "Data.HashSet"
  , mkSymbol "size" SKFunction "Data.HashSet"
  , mkSymbol "null" SKFunction "Data.HashSet"
  , mkSymbol "union" SKFunction "Data.HashSet"
  ]

--------------------------------------------------------------------------------
-- Text Library Symbols
--------------------------------------------------------------------------------

textSymbols :: [SymbolInfo]
textSymbols =
  -- Data.Text
  [ mkType "Text" "Data.Text"
  , mkSymbol "pack" SKFunction "Data.Text"
  , mkSymbol "unpack" SKFunction "Data.Text"
  , mkSymbol "empty" SKFunction "Data.Text"
  , mkSymbol "singleton" SKFunction "Data.Text"
  , mkSymbol "cons" SKFunction "Data.Text"
  , mkSymbol "snoc" SKFunction "Data.Text"
  , mkSymbol "append" SKFunction "Data.Text"
  , mkSymbol "uncons" SKFunction "Data.Text"
  , mkSymbol "unsnoc" SKFunction "Data.Text"
  , mkSymbol "null" SKFunction "Data.Text"
  , mkSymbol "length" SKFunction "Data.Text"
  , mkSymbol "compareLength" SKFunction "Data.Text"
  , mkSymbol "map" SKFunction "Data.Text"
  , mkSymbol "intercalate" SKFunction "Data.Text"
  , mkSymbol "intersperse" SKFunction "Data.Text"
  , mkSymbol "transpose" SKFunction "Data.Text"
  , mkSymbol "reverse" SKFunction "Data.Text"
  , mkSymbol "concat" SKFunction "Data.Text"
  , mkSymbol "concatMap" SKFunction "Data.Text"
  , mkSymbol "any" SKFunction "Data.Text"
  , mkSymbol "all" SKFunction "Data.Text"
  , mkSymbol "maximum" SKFunction "Data.Text"
  , mkSymbol "minimum" SKFunction "Data.Text"
  , mkSymbol "take" SKFunction "Data.Text"
  , mkSymbol "takeEnd" SKFunction "Data.Text"
  , mkSymbol "drop" SKFunction "Data.Text"
  , mkSymbol "dropEnd" SKFunction "Data.Text"
  , mkSymbol "takeWhile" SKFunction "Data.Text"
  , mkSymbol "takeWhileEnd" SKFunction "Data.Text"
  , mkSymbol "dropWhile" SKFunction "Data.Text"
  , mkSymbol "dropWhileEnd" SKFunction "Data.Text"
  , mkSymbol "dropAround" SKFunction "Data.Text"
  , mkSymbol "strip" SKFunction "Data.Text"
  , mkSymbol "stripStart" SKFunction "Data.Text"
  , mkSymbol "stripEnd" SKFunction "Data.Text"
  , mkSymbol "splitAt" SKFunction "Data.Text"
  , mkSymbol "splitOn" SKFunction "Data.Text"
  , mkSymbol "split" SKFunction "Data.Text"
  , mkSymbol "breakOn" SKFunction "Data.Text"
  , mkSymbol "breakOnEnd" SKFunction "Data.Text"
  , mkSymbol "breakOnAll" SKFunction "Data.Text"
  , mkSymbol "lines" SKFunction "Data.Text"
  , mkSymbol "words" SKFunction "Data.Text"
  , mkSymbol "unlines" SKFunction "Data.Text"
  , mkSymbol "unwords" SKFunction "Data.Text"
  , mkSymbol "isPrefixOf" SKFunction "Data.Text"
  , mkSymbol "isSuffixOf" SKFunction "Data.Text"
  , mkSymbol "isInfixOf" SKFunction "Data.Text"
  , mkSymbol "stripPrefix" SKFunction "Data.Text"
  , mkSymbol "stripSuffix" SKFunction "Data.Text"
  , mkSymbol "filter" SKFunction "Data.Text"
  , mkSymbol "find" SKFunction "Data.Text"
  , mkSymbol "elem" SKFunction "Data.Text"
  , mkSymbol "partition" SKFunction "Data.Text"
  , mkSymbol "index" SKFunction "Data.Text"
  , mkSymbol "count" SKFunction "Data.Text"
  , mkSymbol "replace" SKFunction "Data.Text"
  , mkSymbol "toLower" SKFunction "Data.Text"
  , mkSymbol "toUpper" SKFunction "Data.Text"
  , mkSymbol "toTitle" SKFunction "Data.Text"
  , mkSymbol "toCaseFold" SKFunction "Data.Text"
  , mkSymbol "justifyLeft" SKFunction "Data.Text"
  , mkSymbol "justifyRight" SKFunction "Data.Text"
  , mkSymbol "center" SKFunction "Data.Text"
  , mkSymbol "copy" SKFunction "Data.Text"

  -- Data.Text.IO
  , mkSymbol "readFile" SKFunction "Data.Text.IO"
  , mkSymbol "writeFile" SKFunction "Data.Text.IO"
  , mkSymbol "appendFile" SKFunction "Data.Text.IO"
  , mkSymbol "hGetContents" SKFunction "Data.Text.IO"
  , mkSymbol "hGetLine" SKFunction "Data.Text.IO"
  , mkSymbol "hPutStr" SKFunction "Data.Text.IO"
  , mkSymbol "hPutStrLn" SKFunction "Data.Text.IO"
  , mkSymbol "getContents" SKFunction "Data.Text.IO"
  , mkSymbol "getLine" SKFunction "Data.Text.IO"
  , mkSymbol "putStr" SKFunction "Data.Text.IO"
  , mkSymbol "putStrLn" SKFunction "Data.Text.IO"
  , mkSymbol "interact" SKFunction "Data.Text.IO"

  -- Data.Text.Encoding
  , mkSymbol "encodeUtf8" SKFunction "Data.Text.Encoding"
  , mkSymbol "decodeUtf8" SKFunction "Data.Text.Encoding"
  , mkSymbol "decodeUtf8'" SKFunction "Data.Text.Encoding"
  , mkSymbol "decodeUtf8Lenient" SKFunction "Data.Text.Encoding"
  , mkSymbol "encodeUtf16LE" SKFunction "Data.Text.Encoding"
  , mkSymbol "decodeUtf16LE" SKFunction "Data.Text.Encoding"
  , mkSymbol "encodeUtf32LE" SKFunction "Data.Text.Encoding"
  , mkSymbol "decodeUtf32LE" SKFunction "Data.Text.Encoding"

  -- Data.Text.Lazy
  , mkType "Text" "Data.Text.Lazy"
  , mkSymbol "fromStrict" SKFunction "Data.Text.Lazy"
  , mkSymbol "toStrict" SKFunction "Data.Text.Lazy"
  ]

--------------------------------------------------------------------------------
-- ByteString Library Symbols
--------------------------------------------------------------------------------

bytestringSymbols :: [SymbolInfo]
bytestringSymbols =
  -- Data.ByteString
  [ mkType "ByteString" "Data.ByteString"
  , mkSymbol "pack" SKFunction "Data.ByteString"
  , mkSymbol "unpack" SKFunction "Data.ByteString"
  , mkSymbol "empty" SKFunction "Data.ByteString"
  , mkSymbol "singleton" SKFunction "Data.ByteString"
  , mkSymbol "cons" SKFunction "Data.ByteString"
  , mkSymbol "snoc" SKFunction "Data.ByteString"
  , mkSymbol "append" SKFunction "Data.ByteString"
  , mkSymbol "uncons" SKFunction "Data.ByteString"
  , mkSymbol "unsnoc" SKFunction "Data.ByteString"
  , mkSymbol "null" SKFunction "Data.ByteString"
  , mkSymbol "length" SKFunction "Data.ByteString"
  , mkSymbol "map" SKFunction "Data.ByteString"
  , mkSymbol "reverse" SKFunction "Data.ByteString"
  , mkSymbol "intersperse" SKFunction "Data.ByteString"
  , mkSymbol "intercalate" SKFunction "Data.ByteString"
  , mkSymbol "transpose" SKFunction "Data.ByteString"
  , mkSymbol "concat" SKFunction "Data.ByteString"
  , mkSymbol "concatMap" SKFunction "Data.ByteString"
  , mkSymbol "any" SKFunction "Data.ByteString"
  , mkSymbol "all" SKFunction "Data.ByteString"
  , mkSymbol "maximum" SKFunction "Data.ByteString"
  , mkSymbol "minimum" SKFunction "Data.ByteString"
  , mkSymbol "take" SKFunction "Data.ByteString"
  , mkSymbol "drop" SKFunction "Data.ByteString"
  , mkSymbol "splitAt" SKFunction "Data.ByteString"
  , mkSymbol "takeWhile" SKFunction "Data.ByteString"
  , mkSymbol "dropWhile" SKFunction "Data.ByteString"
  , mkSymbol "span" SKFunction "Data.ByteString"
  , mkSymbol "break" SKFunction "Data.ByteString"
  , mkSymbol "breakSubstring" SKFunction "Data.ByteString"
  , mkSymbol "split" SKFunction "Data.ByteString"
  , mkSymbol "splitWith" SKFunction "Data.ByteString"
  , mkSymbol "elem" SKFunction "Data.ByteString"
  , mkSymbol "notElem" SKFunction "Data.ByteString"
  , mkSymbol "find" SKFunction "Data.ByteString"
  , mkSymbol "filter" SKFunction "Data.ByteString"
  , mkSymbol "partition" SKFunction "Data.ByteString"
  , mkSymbol "index" SKFunction "Data.ByteString"
  , mkSymbol "elemIndex" SKFunction "Data.ByteString"
  , mkSymbol "elemIndices" SKFunction "Data.ByteString"
  , mkSymbol "count" SKFunction "Data.ByteString"
  , mkSymbol "findIndex" SKFunction "Data.ByteString"
  , mkSymbol "isPrefixOf" SKFunction "Data.ByteString"
  , mkSymbol "isSuffixOf" SKFunction "Data.ByteString"
  , mkSymbol "isInfixOf" SKFunction "Data.ByteString"
  , mkSymbol "copy" SKFunction "Data.ByteString"
  , mkSymbol "hGetContents" SKFunction "Data.ByteString"
  , mkSymbol "hGet" SKFunction "Data.ByteString"
  , mkSymbol "hGetSome" SKFunction "Data.ByteString"
  , mkSymbol "hGetNonBlocking" SKFunction "Data.ByteString"
  , mkSymbol "hPut" SKFunction "Data.ByteString"
  , mkSymbol "hPutNonBlocking" SKFunction "Data.ByteString"
  , mkSymbol "hPutStr" SKFunction "Data.ByteString"
  , mkSymbol "readFile" SKFunction "Data.ByteString"
  , mkSymbol "writeFile" SKFunction "Data.ByteString"
  , mkSymbol "appendFile" SKFunction "Data.ByteString"
  , mkSymbol "getContents" SKFunction "Data.ByteString"
  , mkSymbol "putStr" SKFunction "Data.ByteString"
  , mkSymbol "getLine" SKFunction "Data.ByteString"
  , mkSymbol "interact" SKFunction "Data.ByteString"

  -- Data.ByteString.Lazy
  , mkType "ByteString" "Data.ByteString.Lazy"
  , mkSymbol "fromStrict" SKFunction "Data.ByteString.Lazy"
  , mkSymbol "toStrict" SKFunction "Data.ByteString.Lazy"
  , mkSymbol "fromChunks" SKFunction "Data.ByteString.Lazy"
  , mkSymbol "toChunks" SKFunction "Data.ByteString.Lazy"
  ]

--------------------------------------------------------------------------------
-- Vector Library Symbols
--------------------------------------------------------------------------------

vectorSymbols :: [SymbolInfo]
vectorSymbols =
  -- Data.Vector
  [ mkType "Vector" "Data.Vector"
  , mkSymbol "empty" SKFunction "Data.Vector"
  , mkSymbol "singleton" SKFunction "Data.Vector"
  , mkSymbol "fromList" SKFunction "Data.Vector"
  , mkSymbol "toList" SKFunction "Data.Vector"
  , mkSymbol "length" SKFunction "Data.Vector"
  , mkSymbol "null" SKFunction "Data.Vector"
  , mkOperator "!" "Data.Vector"
  , mkOperator "!?" "Data.Vector"
  , mkSymbol "head" SKFunction "Data.Vector"
  , mkSymbol "last" SKFunction "Data.Vector"
  , mkSymbol "headM" SKFunction "Data.Vector"
  , mkSymbol "lastM" SKFunction "Data.Vector"
  , mkSymbol "indexM" SKFunction "Data.Vector"
  , mkSymbol "slice" SKFunction "Data.Vector"
  , mkSymbol "init" SKFunction "Data.Vector"
  , mkSymbol "tail" SKFunction "Data.Vector"
  , mkSymbol "take" SKFunction "Data.Vector"
  , mkSymbol "drop" SKFunction "Data.Vector"
  , mkSymbol "splitAt" SKFunction "Data.Vector"
  , mkSymbol "cons" SKFunction "Data.Vector"
  , mkSymbol "snoc" SKFunction "Data.Vector"
  , mkOperator "++" "Data.Vector"
  , mkSymbol "concat" SKFunction "Data.Vector"
  , mkSymbol "replicate" SKFunction "Data.Vector"
  , mkSymbol "generate" SKFunction "Data.Vector"
  , mkSymbol "iterateN" SKFunction "Data.Vector"
  , mkSymbol "replicateM" SKFunction "Data.Vector"
  , mkSymbol "generateM" SKFunction "Data.Vector"
  , mkSymbol "unfoldr" SKFunction "Data.Vector"
  , mkSymbol "unfoldrM" SKFunction "Data.Vector"
  , mkSymbol "enumFromN" SKFunction "Data.Vector"
  , mkSymbol "enumFromStepN" SKFunction "Data.Vector"
  , mkSymbol "enumFromTo" SKFunction "Data.Vector"
  , mkSymbol "enumFromThenTo" SKFunction "Data.Vector"
  , mkSymbol "update" SKFunction "Data.Vector"
  , mkSymbol "accumulate" SKFunction "Data.Vector"
  , mkSymbol "map" SKFunction "Data.Vector"
  , mkSymbol "imap" SKFunction "Data.Vector"
  , mkSymbol "concatMap" SKFunction "Data.Vector"
  , mkSymbol "mapM" SKFunction "Data.Vector"
  , mkSymbol "mapM_" SKFunction "Data.Vector"
  , mkSymbol "forM" SKFunction "Data.Vector"
  , mkSymbol "forM_" SKFunction "Data.Vector"
  , mkSymbol "zipWith" SKFunction "Data.Vector"
  , mkSymbol "zipWith3" SKFunction "Data.Vector"
  , mkSymbol "zip" SKFunction "Data.Vector"
  , mkSymbol "zip3" SKFunction "Data.Vector"
  , mkSymbol "unzip" SKFunction "Data.Vector"
  , mkSymbol "unzip3" SKFunction "Data.Vector"
  , mkSymbol "filter" SKFunction "Data.Vector"
  , mkSymbol "ifilter" SKFunction "Data.Vector"
  , mkSymbol "takeWhile" SKFunction "Data.Vector"
  , mkSymbol "dropWhile" SKFunction "Data.Vector"
  , mkSymbol "partition" SKFunction "Data.Vector"
  , mkSymbol "unstablePartition" SKFunction "Data.Vector"
  , mkSymbol "span" SKFunction "Data.Vector"
  , mkSymbol "break" SKFunction "Data.Vector"
  , mkSymbol "elem" SKFunction "Data.Vector"
  , mkSymbol "notElem" SKFunction "Data.Vector"
  , mkSymbol "find" SKFunction "Data.Vector"
  , mkSymbol "findIndex" SKFunction "Data.Vector"
  , mkSymbol "findIndices" SKFunction "Data.Vector"
  , mkSymbol "elemIndex" SKFunction "Data.Vector"
  , mkSymbol "elemIndices" SKFunction "Data.Vector"
  , mkSymbol "foldl" SKFunction "Data.Vector"
  , mkSymbol "foldl'" SKFunction "Data.Vector"
  , mkSymbol "foldl1" SKFunction "Data.Vector"
  , mkSymbol "foldl1'" SKFunction "Data.Vector"
  , mkSymbol "foldr" SKFunction "Data.Vector"
  , mkSymbol "foldr'" SKFunction "Data.Vector"
  , mkSymbol "foldr1" SKFunction "Data.Vector"
  , mkSymbol "foldr1'" SKFunction "Data.Vector"
  , mkSymbol "ifoldl" SKFunction "Data.Vector"
  , mkSymbol "ifoldl'" SKFunction "Data.Vector"
  , mkSymbol "ifoldr" SKFunction "Data.Vector"
  , mkSymbol "ifoldr'" SKFunction "Data.Vector"
  , mkSymbol "all" SKFunction "Data.Vector"
  , mkSymbol "any" SKFunction "Data.Vector"
  , mkSymbol "and" SKFunction "Data.Vector"
  , mkSymbol "or" SKFunction "Data.Vector"
  , mkSymbol "sum" SKFunction "Data.Vector"
  , mkSymbol "product" SKFunction "Data.Vector"
  , mkSymbol "maximum" SKFunction "Data.Vector"
  , mkSymbol "minimum" SKFunction "Data.Vector"
  , mkSymbol "maximumBy" SKFunction "Data.Vector"
  , mkSymbol "minimumBy" SKFunction "Data.Vector"
  , mkSymbol "minIndex" SKFunction "Data.Vector"
  , mkSymbol "maxIndex" SKFunction "Data.Vector"
  , mkSymbol "minIndexBy" SKFunction "Data.Vector"
  , mkSymbol "maxIndexBy" SKFunction "Data.Vector"
  , mkSymbol "reverse" SKFunction "Data.Vector"
  , mkSymbol "backpermute" SKFunction "Data.Vector"
  , mkSymbol "modify" SKFunction "Data.Vector"
  , mkSymbol "indexed" SKFunction "Data.Vector"

  -- Data.Vector.Unboxed
  , mkType "Vector" "Data.Vector.Unboxed"
  , mkClass "Unbox" "Data.Vector.Unboxed"

  -- Data.Vector.Storable
  , mkType "Vector" "Data.Vector.Storable"
  ]

--------------------------------------------------------------------------------
-- MTL Library Symbols
--------------------------------------------------------------------------------

mtlSymbols :: [SymbolInfo]
mtlSymbols =
  -- Control.Monad.Reader
  [ mkClass "MonadReader" "Control.Monad.Reader"
  , mkType "Reader" "Control.Monad.Reader"
  , mkType "ReaderT" "Control.Monad.Reader"
  , mkSymbol "ask" SKFunction "Control.Monad.Reader"
  , mkSymbol "asks" SKFunction "Control.Monad.Reader"
  , mkSymbol "local" SKFunction "Control.Monad.Reader"
  , mkSymbol "reader" SKFunction "Control.Monad.Reader"
  , mkSymbol "runReader" SKFunction "Control.Monad.Reader"
  , mkSymbol "runReaderT" SKFunction "Control.Monad.Reader"

  -- Control.Monad.State.Strict
  , mkClass "MonadState" "Control.Monad.State.Strict"
  , mkType "State" "Control.Monad.State.Strict"
  , mkType "StateT" "Control.Monad.State.Strict"
  , mkSymbol "get" SKFunction "Control.Monad.State.Strict"
  , mkSymbol "put" SKFunction "Control.Monad.State.Strict"
  , mkSymbol "modify" SKFunction "Control.Monad.State.Strict"
  , mkSymbol "modify'" SKFunction "Control.Monad.State.Strict"
  , mkSymbol "gets" SKFunction "Control.Monad.State.Strict"
  , mkSymbol "state" SKFunction "Control.Monad.State.Strict"
  , mkSymbol "runState" SKFunction "Control.Monad.State.Strict"
  , mkSymbol "runStateT" SKFunction "Control.Monad.State.Strict"
  , mkSymbol "evalState" SKFunction "Control.Monad.State.Strict"
  , mkSymbol "evalStateT" SKFunction "Control.Monad.State.Strict"
  , mkSymbol "execState" SKFunction "Control.Monad.State.Strict"
  , mkSymbol "execStateT" SKFunction "Control.Monad.State.Strict"

  -- Control.Monad.Writer.Strict
  , mkClass "MonadWriter" "Control.Monad.Writer.Strict"
  , mkType "Writer" "Control.Monad.Writer.Strict"
  , mkType "WriterT" "Control.Monad.Writer.Strict"
  , mkSymbol "tell" SKFunction "Control.Monad.Writer.Strict"
  , mkSymbol "listen" SKFunction "Control.Monad.Writer.Strict"
  , mkSymbol "pass" SKFunction "Control.Monad.Writer.Strict"
  , mkSymbol "censor" SKFunction "Control.Monad.Writer.Strict"
  , mkSymbol "writer" SKFunction "Control.Monad.Writer.Strict"
  , mkSymbol "runWriter" SKFunction "Control.Monad.Writer.Strict"
  , mkSymbol "runWriterT" SKFunction "Control.Monad.Writer.Strict"
  , mkSymbol "execWriter" SKFunction "Control.Monad.Writer.Strict"
  , mkSymbol "execWriterT" SKFunction "Control.Monad.Writer.Strict"

  -- Control.Monad.Except
  , mkClass "MonadError" "Control.Monad.Except"
  , mkType "Except" "Control.Monad.Except"
  , mkType "ExceptT" "Control.Monad.Except"
  , mkSymbol "throwError" SKFunction "Control.Monad.Except"
  , mkSymbol "catchError" SKFunction "Control.Monad.Except"
  , mkSymbol "runExcept" SKFunction "Control.Monad.Except"
  , mkSymbol "runExceptT" SKFunction "Control.Monad.Except"
  , mkSymbol "mapExcept" SKFunction "Control.Monad.Except"
  , mkSymbol "mapExceptT" SKFunction "Control.Monad.Except"
  , mkSymbol "withExcept" SKFunction "Control.Monad.Except"
  , mkSymbol "withExceptT" SKFunction "Control.Monad.Except"

  -- Control.Monad.Trans
  , mkClass "MonadTrans" "Control.Monad.Trans"
  , mkSymbol "lift" SKFunction "Control.Monad.Trans"

  -- Control.Monad.IO.Class
  , mkClass "MonadIO" "Control.Monad.IO.Class"
  , mkSymbol "liftIO" SKFunction "Control.Monad.IO.Class"

  -- Control.Monad.Trans.Maybe
  , mkType "MaybeT" "Control.Monad.Trans.Maybe"
  , mkSymbol "runMaybeT" SKFunction "Control.Monad.Trans.Maybe"
  , mkSymbol "mapMaybeT" SKFunction "Control.Monad.Trans.Maybe"

  -- Control.Monad.Identity
  , mkType "Identity" "Control.Monad.Identity"
  , mkSymbol "runIdentity" SKFunction "Control.Monad.Identity"
  ]

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Create a simple function symbol
mkSymbol :: Text -> SymbolKind -> Text -> SymbolInfo
mkSymbol name kind modName = SymbolInfo
  { siName = name
  , siKind = kind
  , siModule = modName
  , siImports = [mkSimpleImport modName name kind]
  , siDescription = Nothing
  , siSince = Nothing
  , siDeprecated = Nothing
  }

-- | Create a type symbol
mkType :: Text -> Text -> SymbolInfo
mkType name modName = mkSymbol name SKType modName

-- | Create a type class symbol
mkClass :: Text -> Text -> SymbolInfo
mkClass name modName = mkSymbol name SKClass modName

-- | Create an operator symbol
mkOperator :: Text -> Text -> SymbolInfo
mkOperator name modName = mkSymbol name SKOperator modName

-- | Create a simple import spec
mkSimpleImport :: Text -> Text -> SymbolKind -> ImportSpec
mkSimpleImport modName symName symKind = ImportSpec
  { isModule = modName
  , isQualified = Nothing
  , isHiding = False
  , isExplicit = [mkImportSymbol symName symKind]
  , isPackage = Nothing
  }

-- | Create an import symbol from name and kind
mkImportSymbol :: Text -> SymbolKind -> ImportSymbol
mkImportSymbol name = \case
  SKFunction -> ImportSymbol name ISTFunction []
  SKOperator -> ImportSymbol name ISTOperator []
  SKType -> ImportSymbol name ISTType []
  SKClass -> ImportSymbol name ISTClass []
  SKConstructor parent -> ImportSymbol name ISTConstructor [parent]
  SKPattern -> ImportSymbol name ISTPattern []
