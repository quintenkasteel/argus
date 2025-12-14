{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Argus.HIE.Query
-- Description : Type-safe HIE database queries
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides type-safe wrappers around HIE database queries,
-- offering enhanced functionality for symbol resolution, type information
-- extraction, and cross-module reference tracking.
module Argus.HIE.Query
  ( -- * Query Context
    HieQuery
  , runHieQuery
  , withHieQuery

    -- * Symbol Queries
  , findSymbol
  , findSymbolAt
  , findAllSymbols
  , findSymbolsInModule
  , findSymbolDefinition
  , findSymbolReferences

    -- * Type Queries
  , getSymbolType
  , getExpressionType
  , getBindingType

    -- * Module Queries
  , getModuleExports
  , getModuleImports
  , getModuleDependencies
  , getReverseDependencies

    -- * Constraint Checking
  , hasInstance
  , findInstances
  , getConstraints

    -- * Batch Queries
  , batchFindSymbols
  , batchGetTypes
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Data.List (nub, sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, catMaybes, listToMaybe)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.CPUTime (getCPUTime)
import System.FilePath (takeFileName)

-- GHC API for HIE file reading
import "ghc" GHC.Iface.Ext.Binary (readHieFile, hie_file_result)
import "ghc" GHC.Iface.Ext.Types
  ( HieFile(..)
  , HieASTs(..)
  , HieAST(..)
  , NodeInfo(..)
  , IdentifierDetails(..)
  , SourcedNodeInfo(..)
  , NodeOrigin(..)
  , ContextInfo(..)
  , DeclType(..)
  , TypeIndex
  , getAsts
  , getSourcedNodeInfo
  )
import "ghc" GHC.Iface.Ext.Utils (recoverFullType, hieTypeToIface)
import "ghc" GHC.Utils.Outputable qualified as GHC (showSDocUnsafe, ppr)
import "ghc" GHC.Types.Name (nameOccName, nameModule_maybe)
import "ghc" GHC.Types.Name.Occurrence (occNameString)
import "ghc" GHC.Unit.Module (moduleNameString, moduleName)
import "ghc" GHC.Types.SrcLoc qualified as GHC (srcSpanStartLine, srcSpanStartCol, srcSpanEndLine, srcSpanEndCol)

import HieDb (HieDb, HieModuleRow(..))
import HieDb qualified
import HieDb.Utils (makeNc)
import HieDb.Types (modInfoSrcFile)

import Argus.Types (SrcSpan(..), SymbolKind(..))
import Argus.HIE.Types
import Argus.HIE.TypeInfo
  ( extractType
  , extractBindingType
  , extractExprType
  , lookupKnownType
  , defaultKnownTypes
  )
import Argus.Analysis.Semantic
  ( DefinitionResult(..)
  , ReferenceResult(..)
  , ModuleInfo(..)
  , findDefinition
  , findReferences
  , getAllModules
  , getExports
  )

--------------------------------------------------------------------------------
-- Query Context
--------------------------------------------------------------------------------

-- | Query monad for HIE operations
type HieQuery a = ReaderT QueryContext IO a

-- | Context for running HIE queries
data QueryContext = QueryContext
  { qcDb        :: HieDb
  , qcCache     :: QueryCache
  }

-- | Cache for expensive queries
data QueryCache = QueryCache
  { cacheSymbols :: Map.Map Text [HieSymbol]
  , cacheTypes   :: Map.Map Text TypeInfo
  , cacheModules :: Map.Map Text ModuleSymbols
  }

-- | Empty query cache
emptyCache :: QueryCache
emptyCache = QueryCache Map.empty Map.empty Map.empty

-- | Run a HIE query with the given database
runHieQuery :: HieDb -> HieQuery a -> IO a
runHieQuery db query = runReaderT query (QueryContext db emptyCache)

-- | Run queries with a database connection
withHieQuery :: FilePath -> HieQuery a -> IO a
withHieQuery dbPath query =
  HieDb.withHieDb dbPath $ \db -> runHieQuery db query

--------------------------------------------------------------------------------
-- Symbol Queries
--------------------------------------------------------------------------------

-- | Find a symbol by name
findSymbol :: Text -> Maybe Text -> HieQuery (Maybe HieSymbol)
findSymbol name mModule = do
  ctx <- ask
  liftIO $ findSymbolImpl (qcDb ctx) name mModule

findSymbolImpl :: HieDb -> Text -> Maybe Text -> IO (Maybe HieSymbol)
findSymbolImpl db name mModule = do
  defs <- findDefinition db name mModule
  refs <- findReferences db name mModule
  case defs of
    [] -> pure Nothing
    (def:_) -> pure $ Just HieSymbol
      { hsName = name
      , hsModule = defResultModule def
      , hsQualified = defResultModule def <> "." <> name
      , hsKind = Function  -- Default, could be refined
      , hsType = Nothing  -- Would need type extraction
      , hsDefinition = Just (defResultSpan def)
      , hsReferences = map refResultSpan refs
      , hsExported = True  -- Assume exported if found
      , hsScope = ScopeModule
      , hsVisibility = VisPublic
      , hsDocumentation = Nothing
      }

-- | Find a symbol at a specific source location
-- This reads the HIE file directly and navigates the AST to find
-- identifiers at the given position.
findSymbolAt :: FilePath -> Int -> Int -> HieQuery (Maybe HieSymbol)
findSymbolAt srcFile line col = do
  ctx <- ask
  liftIO $ findSymbolAtImpl (qcDb ctx) srcFile line col

-- | Implementation of findSymbolAt using direct HIE file reading
findSymbolAtImpl :: HieDb -> FilePath -> Int -> Int -> IO (Maybe HieSymbol)
findSymbolAtImpl db srcFile line col = do
  -- First, try to find the HIE file for this source file via HieDb
  mHieFile <- findHieFileForSource db srcFile

  case mHieFile of
    Nothing -> pure Nothing
    Just hiePath -> do
      -- Read the HIE file
      result <- try @SomeException $ do
        nc <- makeNc
        hieResult <- readHieFile nc hiePath
        let hieFile = hie_file_result hieResult

        -- Get the AST and find nodes at the position
        let asts = getAsts $ hie_asts hieFile
            -- Find nodes that contain the target position
            nodesAtPos = concatMap (findNodesAtPosition line col) (Map.elems asts)

        -- Extract symbol info from the innermost (most specific) node
        case nodesAtPos of
          [] -> pure Nothing
          nodes ->
            -- Sort by span size to get the most specific node first
            case sortBy (comparing nodeSpanSize) nodes of
              [] -> pure Nothing
              (smallest:_) -> extractSymbolFromNode db smallest

      case result of
        Left _ -> pure Nothing
        Right sym -> pure sym

-- | Find the HIE file for a source file using HieDb
findHieFileForSource :: HieDb -> FilePath -> IO (Maybe FilePath)
findHieFileForSource db srcFile = do
  -- Get all indexed modules and find one matching this source file
  modules <- HieDb.getAllIndexedMods db
  let matching = filter (matchesSrcFile srcFile) modules
  pure $ listToMaybe $ map hieModuleHieFile matching
  where
    matchesSrcFile :: FilePath -> HieModuleRow -> Bool
    matchesSrcFile target row =
      case modInfoSrcFile (hieModInfo row) of
        Just src -> src == target || takeFileName src == takeFileName target
        Nothing -> False

-- | Find all AST nodes that contain the given position
findNodesAtPosition :: Int -> Int -> HieAST a -> [HieAST a]
findNodesAtPosition targetLine targetCol ast =
  let hieSpan = nodeSpan ast
      startLine = GHC.srcSpanStartLine hieSpan
      startCol = GHC.srcSpanStartCol hieSpan
      endLine = GHC.srcSpanEndLine hieSpan
      endCol = GHC.srcSpanEndCol hieSpan

      -- Check if position is within this node's span
      containsPos =
        (targetLine > startLine || (targetLine == startLine && targetCol >= startCol)) &&
        (targetLine < endLine || (targetLine == endLine && targetCol <= endCol))
  in if containsPos
     then ast : concatMap (findNodesAtPosition targetLine targetCol) (nodeChildren ast)
     else []

-- | Get the span size for sorting (smaller = more specific)
nodeSpanSize :: HieAST a -> Int
nodeSpanSize ast =
  let hieSpan = nodeSpan ast
      linesCount = GHC.srcSpanEndLine hieSpan - GHC.srcSpanStartLine hieSpan
      cols = if linesCount == 0
             then GHC.srcSpanEndCol hieSpan - GHC.srcSpanStartCol hieSpan
             else linesCount * 200 + GHC.srcSpanEndCol hieSpan  -- Approximate character count
  in max 1 cols

-- | Extract symbol information from a HIE AST node
extractSymbolFromNode :: HieDb -> HieAST a -> IO (Maybe HieSymbol)
extractSymbolFromNode db ast = do
  let nodeInfo = nodeInfo' ast
      identifiers = Map.toList $ nodeIdentifiers nodeInfo

  case identifiers of
    [] -> pure Nothing
    ((ident, details):_) -> do
      -- Extract symbol info from the first identifier
      case ident of
        Left _moduleName -> pure Nothing  -- Module reference, skip
        Right name -> do
          let symName = T.pack $ occNameString $ nameOccName name
              symModule = case nameModule_maybe name of
                Just m -> T.pack $ moduleNameString $ GHC.Unit.Module.moduleName m
                Nothing -> ""
              symKind = identDetailsToKind details

          -- Try to find the definition for more info
          defs <- findDefinition db symName (if T.null symModule then Nothing else Just symModule)
          refs <- findReferences db symName (if T.null symModule then Nothing else Just symModule)

          pure $ Just HieSymbol
            { hsName = symName
            , hsModule = symModule
            , hsQualified = if T.null symModule then symName else symModule <> "." <> symName
            , hsKind = symKind
            , hsType = Nothing  -- Type extraction is separate
            , hsDefinition = listToMaybe $ map defResultSpan defs
            , hsReferences = map refResultSpan refs
            , hsExported = True
            , hsScope = ScopeModule
            , hsVisibility = VisPublic
            , hsDocumentation = Nothing
            }

-- | Convert GHC identifier details to our SymbolKind
identDetailsToKind :: IdentifierDetails a -> SymbolKind
identDetailsToKind details =
  -- Check the context info to determine the kind
  -- Uses pattern matching on GHC's ContextInfo constructors
  case Set.toList $ identInfo details of
    [] -> Function  -- Default to function
    infos ->
      -- Look for specific context types using pattern matching
      if any isTypeContext infos
        then TypeConstructor
        else if any isDataContext infos
             then DataConstructor
             else if any isClassContext infos
                  then TypeClass
                  else Function
  where
    isTypeContext :: ContextInfo -> Bool
    isTypeContext TyDecl = True
    isTypeContext (Decl FamDec _) = True
    isTypeContext (Decl SynDec _) = True
    isTypeContext (Decl DataDec _) = True
    isTypeContext _ = False

    isDataContext :: ContextInfo -> Bool
    isDataContext (Decl ConDec _) = True
    isDataContext (Decl PatSynDec _) = True
    isDataContext _ = False

    isClassContext :: ContextInfo -> Bool
    isClassContext (Decl ClassDec _) = True
    isClassContext (ClassTyDecl _) = True
    isClassContext _ = False

-- | Get the node's own info (from GHC's getSourcedNodeInfo)
-- Prefers SourceInfo over GeneratedInfo when available
nodeInfo' :: HieAST a -> NodeInfo a
nodeInfo' ast =
  let snInfo = sourcedNodeInfo ast
      nodeInfoMap = getSourcedNodeInfo snInfo
  in case Map.lookup SourceInfo nodeInfoMap of
       Just ni -> ni
       Nothing -> case Map.elems nodeInfoMap of
                    (ni:_) -> ni
                    [] -> emptyNodeInfo
  where
    emptyNodeInfo = NodeInfo mempty mempty mempty

-- | Find all symbols matching a pattern
findAllSymbols :: Text -> HieQuery [HieSymbol]
findAllSymbols searchPattern = do
  ctx <- ask
  liftIO $ findAllSymbolsImpl (qcDb ctx) searchPattern

findAllSymbolsImpl :: HieDb -> Text -> IO [HieSymbol]
findAllSymbolsImpl db searchPattern = do
  -- Get all modules and search for matching symbols
  modules <- getAllModules db
  symbols <- forM modules $ \modInfo -> do
    exports <- getExports db (moduleInfoName modInfo)
    let matching = filter (searchPattern `T.isInfixOf`) exports
    forM matching $ \sym -> findSymbolImpl db sym (Just $ moduleInfoName modInfo)
  pure $ catMaybes $ concat symbols

-- | Find all symbols in a specific module
findSymbolsInModule :: Text -> HieQuery [HieSymbol]
findSymbolsInModule modName = do
  ctx <- ask
  liftIO $ findSymbolsInModuleImpl (qcDb ctx) modName

findSymbolsInModuleImpl :: HieDb -> Text -> IO [HieSymbol]
findSymbolsInModuleImpl db modName = do
  exports <- getExports db modName
  symbols <- forM exports $ \sym -> findSymbolImpl db sym (Just modName)
  pure $ catMaybes symbols

-- | Find the definition of a symbol
findSymbolDefinition :: Text -> Maybe Text -> HieQuery (Maybe SrcSpan)
findSymbolDefinition name mModule = do
  ctx <- ask
  liftIO $ do
    defs <- findDefinition (qcDb ctx) name mModule
    pure $ listToMaybe $ map defResultSpan defs

-- | Find all references to a symbol
findSymbolReferences :: Text -> Maybe Text -> HieQuery [SymbolReference]
findSymbolReferences name mModule = do
  ctx <- ask
  liftIO $ findSymbolReferencesImpl (qcDb ctx) name mModule

findSymbolReferencesImpl :: HieDb -> Text -> Maybe Text -> IO [SymbolReference]
findSymbolReferencesImpl db name mModule = do
  refs <- findReferences db name mModule
  pure $ map toSymbolRef refs
  where
    toSymbolRef ref = SymbolReference
      { srSymbol = name
      , srModule = refResultModule ref
      , srFile = refResultFile ref
      , srSpan = refResultSpan ref
      , srKind = RefUsage
      , srQualified = Nothing
      }

--------------------------------------------------------------------------------
-- Type Queries
--------------------------------------------------------------------------------

-- | Get the type of a symbol
getSymbolType :: Text -> Maybe Text -> HieQuery (Maybe TypeInfo)
getSymbolType name mModule = do
  ctx <- ask
  liftIO $ getSymbolTypeImpl (qcDb ctx) name mModule

getSymbolTypeImpl :: HieDb -> Text -> Maybe Text -> IO (Maybe TypeInfo)
getSymbolTypeImpl _db name _mModule = do
  -- First try the known types database for common functions
  case lookupKnownType defaultKnownTypes name of
    Just ti -> pure $ Just ti
    Nothing -> do
      -- Fall back to extractType which may query additional sources
      extractType name _mModule

-- | Get the type of an expression at a location
getExpressionType :: FilePath -> Int -> Int -> HieQuery (Maybe TypeInfo)
getExpressionType file line col = do
  -- Use the TypeInfo module's expression type extraction
  liftIO $ extractExprType file line col

-- | Get the type of a binding
getBindingType :: Text -> Text -> HieQuery (Maybe TypeInfo)
getBindingType modName bindingName = do
  -- First check known types, then use binding type extraction
  case lookupKnownType defaultKnownTypes bindingName of
    Just ti -> pure $ Just ti
    Nothing -> liftIO $ extractBindingType modName bindingName

--------------------------------------------------------------------------------
-- Module Queries
--------------------------------------------------------------------------------

-- | Get all exports from a module
getModuleExports :: Text -> HieQuery [Text]
getModuleExports modName = do
  ctx <- ask
  liftIO $ getExports (qcDb ctx) modName

-- | Get all imports in a module
getModuleImports :: Text -> HieQuery (Map.Map Text [Text])
getModuleImports modName = do
  ctx <- ask
  liftIO $ getModuleImportsImpl (qcDb ctx) modName

getModuleImportsImpl :: HieDb -> Text -> IO (Map.Map Text [Text])
getModuleImportsImpl db modName = do
  -- Get all references from this module and group by their defining module
  -- First get all symbols referenced in this module
  allRefs <- findReferences db "*" (Just modName)

  -- Group references by the module they come from
  let grouped = foldr groupRef Map.empty allRefs
  pure grouped
  where
    groupRef :: ReferenceResult -> Map.Map Text [Text] -> Map.Map Text [Text]
    groupRef ref acc =
      let refMod = refResultModule ref
          refName = T.takeWhileEnd (/= '.') (T.pack $ srcSpanFile $ refResultSpan ref)
      in if refMod /= modName
         then Map.insertWith (<>) refMod [refName] acc
         else acc

-- | Get modules that this module depends on
getModuleDependencies :: Text -> HieQuery [Text]
getModuleDependencies modName = do
  imports <- getModuleImports modName
  pure $ Map.keys imports

-- | Get modules that depend on this module
getReverseDependencies :: Text -> HieQuery [Text]
getReverseDependencies modName = do
  ctx <- ask
  liftIO $ getReverseDependenciesImpl (qcDb ctx) modName

getReverseDependenciesImpl :: HieDb -> Text -> IO [Text]
getReverseDependenciesImpl db modName = do
  -- Find all modules that reference symbols from this module
  exports <- getExports db modName
  allRefs <- forM exports $ \sym -> findReferences db sym (Just modName)
  let refModules = nub $ map refResultModule $ concat allRefs
  pure $ filter (/= modName) refModules

--------------------------------------------------------------------------------
-- Constraint Checking
--------------------------------------------------------------------------------

-- | Check if a type has an instance of a type class
hasInstance :: Text -> Text -> HieQuery Bool
hasInstance className typeName = do
  instances <- findInstances className
  pure $ any (\i -> iiType i == typeName) instances

-- | Find all instances of a type class
-- Queries HIE database for instance declarations, falls back to known defaults
findInstances :: Text -> HieQuery [InstanceInfo]
findInstances className = do
  ctx <- ask
  -- Query HIE for instances of this specific class
  hieInstances <- liftIO $ findInstancesInHieDb (qcDb ctx) className
  -- Combine with known defaults for this class
  let knownForClass = filter (\i -> iiClass i == className) defaultInstances
  pure $ nub $ hieInstances ++ knownForClass

-- | Search HIE database for instance declarations
findInstancesInHieDb :: HieDb -> Text -> IO [InstanceInfo]
findInstancesInHieDb db className = do
  modules <- HieDb.getAllIndexedMods db
  instancesLists <- forM modules $ \modRow -> do
    let hiePath = hieModuleHieFile modRow
    result <- try @SomeException $ do
      nc <- makeNc
      hieResult <- readHieFile nc hiePath
      let hieFile = hie_file_result hieResult
          asts = getAsts $ hie_asts hieFile
      pure $ concatMap (extractInstancesFromAST className hieFile) (Map.elems asts)
    case result of
      Left _ -> pure []
      Right instances -> pure instances
  pure $ concat instancesLists

-- | Extract instance declarations from a HIE AST
extractInstancesFromAST :: Text -> HieFile -> HieAST TypeIndex -> [InstanceInfo]
extractInstancesFromAST targetClass hieFile ast =
  let nodeInf = nodeInfo' ast
      contextInfos = concatMap (Set.toList . identInfo) $ Map.elems $ nodeIdentifiers nodeInf
      instanceDecls = mapMaybe (extractInstanceDecl nodeInf) contextInfos
      -- Recurse into children
      childInstances = concatMap (extractInstancesFromAST targetClass hieFile) (nodeChildren ast)
  in instanceDecls ++ childInstances
  where
    extractInstanceDecl :: NodeInfo TypeIndex -> ContextInfo -> Maybe InstanceInfo
    extractInstanceDecl ni (Decl InstDec _) =
      -- Found an instance declaration - extract type info
      case nodeType ni of
        (typeIdx:_) ->
          let typeArray = hie_types hieFile
              hieTypeFix = recoverFullType typeIdx typeArray
              ifaceType = hieTypeToIface hieTypeFix
              typeText = T.pack $ GHC.showSDocUnsafe $ GHC.ppr ifaceType
          in parseInstanceType targetClass typeText
        [] -> Nothing
    extractInstanceDecl _ _ = Nothing

-- | Parse an instance type signature to extract class and type
parseInstanceType :: Text -> Text -> Maybe InstanceInfo
parseInstanceType targetClass typeText =
  -- Instance types look like "Ord Int" or "Monad (Either e)"
  case T.words typeText of
    (cls:typeArgs) | cls == targetClass && not (null typeArgs) ->
      Just InstanceInfo
        { iiClass = cls
        , iiType = T.unwords typeArgs
        , iiModule = ""  -- Module info not available from type alone
        , iiOrphan = False
        , iiOverlap = Nothing
        }
    _ -> Nothing

-- | Get well-known default instances for common classes
defaultInstances :: [InstanceInfo]
defaultInstances =
  [ mkInstance "Ord" "Int" "GHC.Classes"
  , mkInstance "Ord" "Integer" "GHC.Classes"
  , mkInstance "Ord" "Double" "GHC.Classes"
  , mkInstance "Ord" "Float" "GHC.Classes"
  , mkInstance "Ord" "Char" "GHC.Classes"
  , mkInstance "Ord" "Bool" "GHC.Classes"
  , mkInstance "Ord" "Text" "Data.Text"
  , mkInstance "Ord" "ByteString" "Data.ByteString"
  , mkInstance "Eq" "Int" "GHC.Classes"
  , mkInstance "Eq" "Integer" "GHC.Classes"
  , mkInstance "Eq" "Double" "GHC.Classes"
  , mkInstance "Eq" "Float" "GHC.Classes"
  , mkInstance "Eq" "Char" "GHC.Classes"
  , mkInstance "Eq" "Bool" "GHC.Classes"
  , mkInstance "Eq" "Text" "Data.Text"
  , mkInstance "Monoid" "Text" "Data.Text"
  , mkInstance "Monoid" "[a]" "GHC.Base"
  , mkInstance "Semigroup" "Text" "Data.Text"
  , mkInstance "Semigroup" "[a]" "GHC.Base"
  , mkInstance "Show" "Int" "GHC.Show"
  , mkInstance "Show" "Integer" "GHC.Show"
  , mkInstance "Show" "Double" "GHC.Show"
  , mkInstance "Show" "Float" "GHC.Show"
  , mkInstance "Show" "Char" "GHC.Show"
  , mkInstance "Show" "Bool" "GHC.Show"
  , mkInstance "Show" "Text" "Data.Text"
  , mkInstance "Num" "Int" "GHC.Num"
  , mkInstance "Num" "Integer" "GHC.Num"
  , mkInstance "Num" "Double" "GHC.Num"
  , mkInstance "Num" "Float" "GHC.Num"
  , mkInstance "Hashable" "Int" "Data.Hashable"
  , mkInstance "Hashable" "Integer" "Data.Hashable"
  , mkInstance "Hashable" "Text" "Data.Hashable"
  , mkInstance "Hashable" "Char" "Data.Hashable"
  , mkInstance "Hashable" "Bool" "Data.Hashable"
  ]
  where
    mkInstance cls ty modName = InstanceInfo
      { iiClass = cls
      , iiType = ty
      , iiModule = modName
      , iiOrphan = False
      , iiOverlap = Nothing
      }

-- | Get constraints required for a symbol
getConstraints :: Text -> Maybe Text -> HieQuery [TypeConstraint]
getConstraints name mModule = do
  mType <- getSymbolType name mModule
  case mType of
    Nothing -> pure []
    Just ti -> pure $ tiConstraints ti

--------------------------------------------------------------------------------
-- Batch Queries
--------------------------------------------------------------------------------

-- | Find multiple symbols efficiently
batchFindSymbols :: [Text] -> HieQuery (Map.Map Text (Maybe HieSymbol))
batchFindSymbols names = do
  results <- forM names $ \name -> do
    sym <- findSymbol name Nothing
    pure (name, sym)
  pure $ Map.fromList results

-- | Get types for multiple symbols
batchGetTypes :: [(Text, Maybe Text)] -> HieQuery (Map.Map Text (Maybe TypeInfo))
batchGetTypes queries = do
  results <- forM queries $ \(name, mModule) -> do
    ti <- getSymbolType name mModule
    pure (name, ti)
  pure $ Map.fromList results

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- | Measure query time
timed :: MonadIO m => m a -> m (a, Double)
timed action = do
  start <- liftIO getCPUTime
  result <- action
  end <- liftIO getCPUTime
  let duration = fromIntegral (end - start) / (10^(9 :: Int))
  pure (result, duration)
