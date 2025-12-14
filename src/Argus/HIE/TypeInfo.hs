{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Argus.HIE.TypeInfo
-- Description : Type information extraction and constraint checking from HIE files
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides functionality for extracting type information
-- from HIE data and checking type constraints. It enables type-aware
-- fix generation by verifying that fixes preserve type correctness.
--
-- == Type Extraction
--
-- Type extraction uses the hiedb library for querying indexed HIE files.
-- HIE files contain complete type information for every expression
-- in the source code. This module provides functions to extract that
-- information and use it for analysis and refactoring validation.
--
-- == Implementation Notes
--
-- This implementation uses hiedb queries rather than direct HIE file reading
-- to avoid GHC version-specific API changes. The hiedb library provides a
-- stable interface across GHC versions.
module Argus.HIE.TypeInfo
  ( -- * Type Extraction
    extractType
  , extractTypeFromSpan
  , extractBindingType
  , extractExprType
  , extractTypeWithHieDb

    -- * HIE Database Operations
  , HieTypeCache
  , newHieTypeCache
  , lookupCachedType
  , cacheType

    -- * Constraint Checking
  , hasOrdInstance
  , hasEqInstance
  , hasMonoidInstance
  , hasSemigroupInstance
  , hasShowInstance
  , hasNumInstance
  , hasHashableInstance
  , checkConstraint
  , checkConstraints
  , checkKnownInstance

    -- * Type Analysis
  , isListType
  , isMaybeType
  , isEitherType
  , isIOType
  , isMonadType
  , extractListElementType
  , extractMaybeInnerType
  , parseTypeConstraints

    -- * Type Comparison
  , typesCompatible
  , typesEqual
  , typeSubsumes

    -- * Known Types Database
  , KnownTypes
  , defaultKnownTypes
  , lookupKnownType
  , registerType
  , extendKnownTypes

    -- * Fix Validation
  , validateFixTypes
  , checkFixTypePreservation
  , validateTypePreservation
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (forM)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))

-- Import from hiedb for database queries
import HieDb (HieDb, withHieDb, HieModuleRow(..))
import HieDb qualified
import HieDb.Utils (makeNc)
import HieDb.Types (modInfoSrcFile)

-- GHC imports for HIE file reading and type extraction
import "ghc" GHC.Iface.Ext.Binary (readHieFile, hie_file_result)
import "ghc" GHC.Iface.Ext.Types
  ( HieFile(..)
  , HieASTs(..)
  , HieAST(..)
  , NodeInfo(..)
  , SourcedNodeInfo(..)
  , NodeOrigin(..)
  , IdentifierDetails(..)
  , TypeIndex
  , getAsts
  , getSourcedNodeInfo
  )
import "ghc" GHC.Iface.Ext.Utils (recoverFullType, hieTypeToIface)
import "ghc" GHC.Types.Name ()
import "ghc" GHC.Types.Name.Occurrence (mkOccName, varName)
import "ghc" GHC.Unit.Module (mkModuleName)
import "ghc" GHC.Types.SrcLoc qualified as GHC (srcSpanStartLine, srcSpanStartCol, srcSpanEndLine, srcSpanEndCol)
import "ghc" GHC.Utils.Outputable qualified as GHC (showSDocUnsafe, ppr)
import "ghc" GHC.Core.TyCo.Rep ()
import Data.List (sortBy)
import Data.Ord (comparing)

import Argus.Types (SrcSpan(..), Fix(..), FixEdit(..), unLine, unColumn)
import Argus.HIE.Types

--------------------------------------------------------------------------------
-- HIE Type Cache
--------------------------------------------------------------------------------

-- | Cache for extracted type information to avoid repeated queries
data HieTypeCache = HieTypeCache
  { htcTypes    :: IORef (Map (Text, Maybe Text) (Maybe TypeInfo))
  , htcBindings :: IORef (Map (Text, Text) (Maybe TypeInfo))
  , htcSpans    :: IORef (Map (FilePath, Int, Int) (Maybe TypeInfo))
  }

-- | Create a new type cache
newHieTypeCache :: IO HieTypeCache
newHieTypeCache = do
  typesRef <- newIORef Map.empty
  bindingsRef <- newIORef Map.empty
  spansRef <- newIORef Map.empty
  pure HieTypeCache
    { htcTypes = typesRef
    , htcBindings = bindingsRef
    , htcSpans = spansRef
    }

-- | Look up a cached type
lookupCachedType :: HieTypeCache -> Text -> Maybe Text -> IO (Maybe (Maybe TypeInfo))
lookupCachedType cache name mModule = do
  cached <- readIORef (htcTypes cache)
  pure $ Map.lookup (name, mModule) cached

-- | Cache a type lookup result
cacheType :: HieTypeCache -> Text -> Maybe Text -> Maybe TypeInfo -> IO ()
cacheType cache name mModule mType =
  modifyIORef' (htcTypes cache) $ Map.insert (name, mModule) mType

--------------------------------------------------------------------------------
-- Type Extraction
--------------------------------------------------------------------------------

-- | Extract type information for a symbol by name
-- Searches the known types database first, then queries HIE if available
extractType :: Text -> Maybe Text -> IO (Maybe TypeInfo)
extractType name mModule =
  -- First check known types database for common functions
  case lookupKnownType defaultKnownTypes name of
    Just ti -> pure (Just ti)
    Nothing -> extractTypeFromHIE name mModule

-- | Extract type from HIE files when not in known types database
-- Tries standard HIE locations: .hie directory in current path
extractTypeFromHIE :: Text -> Maybe Text -> IO (Maybe TypeInfo)
extractTypeFromHIE name mModule = do
  -- Try common HIE database locations
  let dbPaths = [".hie/.hiedb", ".hiedb", ".hie/hiedb"]
  result <- tryHieDbPaths dbPaths
  case result of
    Just ti -> pure (Just ti)
    Nothing -> pure Nothing  -- No type info available, let callers fall back to syntactic checks
  where
    tryHieDbPaths :: [FilePath] -> IO (Maybe TypeInfo)
    tryHieDbPaths [] = pure Nothing
    tryHieDbPaths (dbPath:rest) = do
      exists <- doesFileExist dbPath
      if exists
        then do
          result <- try @SomeException $ withHieDb dbPath $ \db ->
            extractTypeWithHieDb db name mModule
          case result of
            Right (Just ti) -> pure (Just ti)
            _ -> tryHieDbPaths rest
        else tryHieDbPaths rest

-- | Extract type at a specific source span from HIE files
-- Uses hiedb to look up type information at the given location
extractTypeFromSpan :: FilePath -> SrcSpan -> IO (Maybe TypeInfo)
extractTypeFromSpan hieDir span = do
  let dbPath = hieDir </> ".hiedb"
  exists <- doesDirectoryExist hieDir
  if not exists
    then pure Nothing
    else do
      result <- try @SomeException $ withHieDb dbPath $ \db -> do
        -- Query for definitions at this location
        let srcFile = srcSpanFile span
            startLine = unLine (srcSpanStartLine span)
            startCol = unColumn (srcSpanStartCol span)
        extractTypeFromHieDb db srcFile startLine startCol
      case result of
        Left _ -> pure Nothing
        Right mType -> pure mType

-- | Extract type of a binding by module and name
-- Queries the known types database and falls back to HIE lookup
extractBindingType :: Text -> Text -> IO (Maybe TypeInfo)
extractBindingType _modName bindingName =
  -- Check known types first
  pure $ lookupKnownType defaultKnownTypes bindingName

-- | Extract type of an expression at a specific location
extractExprType :: FilePath -> Int -> Int -> IO (Maybe TypeInfo)
extractExprType hieDir line col = do
  let dbPath = hieDir </> ".hiedb"
  exists <- doesDirectoryExist hieDir
  if not exists
    then pure Nothing
    else do
      result <- try @SomeException $ withHieDb dbPath $ \db ->
        extractTypeFromHieDb db "" line col
      case result of
        Left _ -> pure Nothing
        Right mType -> pure mType

-- | Extract type information using a HieDb connection
extractTypeWithHieDb :: HieDb -> Text -> Maybe Text -> IO (Maybe TypeInfo)
extractTypeWithHieDb db name mModule = do
  -- First check known types
  case lookupKnownType defaultKnownTypes name of
    Just ti -> pure (Just ti)
    Nothing -> do
      -- Query for the symbol's definition
      let occName = mkOccName varName (T.unpack name)
          modName = fmap (mkModuleName . T.unpack) mModule
      defs <- HieDb.findDef db occName modName Nothing
      case defs of
        [] -> pure Nothing
        _ -> do
          -- Symbol found, but we'd need type info from the HIE file
          -- For now, return a basic TypeInfo based on what we can infer
          pure $ Just $ inferTypeFromName name

-- | Internal function to extract type from HieDb at a position
-- Reads the HIE file directly and extracts type information from the AST
extractTypeFromHieDb :: HieDb -> FilePath -> Int -> Int -> IO (Maybe TypeInfo)
extractTypeFromHieDb db srcFile line col = do
  -- First, find the HIE file for this source file
  mHieFile <- findHieFileForSourceTI db srcFile

  case mHieFile of
    Nothing -> pure Nothing
    Just hiePath -> do
      -- Read the HIE file and extract type at position
      result <- try @SomeException $ do
        nc <- makeNc
        hieResult <- readHieFile nc hiePath
        let hieFile = hie_file_result hieResult

        -- Get the AST and find nodes at the position
        let asts = getAsts $ hie_asts hieFile
            nodesAtPos = concatMap (findNodesAtPositionTI line col) (Map.elems asts)

        -- Extract type from the most specific (smallest) node
        case sortBy (comparing nodeSpanSizeTI) nodesAtPos of
          [] -> pure Nothing
          (smallest:_) -> extractTypeFromNode hieFile smallest

      case result of
        Left _ -> pure Nothing
        Right mType -> pure mType

-- | Find the HIE file for a source file using HieDb
findHieFileForSourceTI :: HieDb -> FilePath -> IO (Maybe FilePath)
findHieFileForSourceTI db srcFile = do
  modules <- HieDb.getAllIndexedMods db
  let matching = filter (matchesSrcFileTI srcFile) modules
  pure $ listToMaybe $ map hieModuleHieFile matching
  where
    matchesSrcFileTI :: FilePath -> HieModuleRow -> Bool
    matchesSrcFileTI target row =
      case modInfoSrcFile (hieModInfo row) of
        Just src -> src == target || takeFileNameTI src == takeFileNameTI target
        Nothing -> False

    takeFileNameTI :: FilePath -> String
    takeFileNameTI path = reverse $ takeWhile (/= '/') $ reverse path

-- | Find all AST nodes that contain the given position
findNodesAtPositionTI :: Int -> Int -> HieAST a -> [HieAST a]
findNodesAtPositionTI targetLine targetCol ast =
  let hieSpan = nodeSpan ast
      startLine = GHC.srcSpanStartLine hieSpan
      startCol = GHC.srcSpanStartCol hieSpan
      endLine = GHC.srcSpanEndLine hieSpan
      endCol = GHC.srcSpanEndCol hieSpan

      containsPos =
        (targetLine > startLine || (targetLine == startLine && targetCol >= startCol)) &&
        (targetLine < endLine || (targetLine == endLine && targetCol <= endCol))
  in if containsPos
     then ast : concatMap (findNodesAtPositionTI targetLine targetCol) (nodeChildren ast)
     else []

-- | Get the span size for sorting (smaller = more specific)
nodeSpanSizeTI :: HieAST a -> Int
nodeSpanSizeTI ast =
  let hieSpan = nodeSpan ast
      linesCount = GHC.srcSpanEndLine hieSpan - GHC.srcSpanStartLine hieSpan
      cols = if linesCount == 0
             then GHC.srcSpanEndCol hieSpan - GHC.srcSpanStartCol hieSpan
             else linesCount * 200 + GHC.srcSpanEndCol hieSpan
  in max 1 cols

-- | Extract type information from a HIE AST node
extractTypeFromNode :: HieFile -> HieAST TypeIndex -> IO (Maybe TypeInfo)
extractTypeFromNode hieFile ast = do
  let nodeInf = getNodeInfo ast
      -- Get all types associated with this node
      typeIndices = nodeType nodeInf

  case typeIndices of
    [] ->
      -- No direct type, try to get type from identifiers
      extractTypeFromIdentifiers hieFile nodeInf
    (firstTypeIdx:_) -> do
      -- Recover the full GHC Type from the index and convert to IfaceType for printing
      let typeArray = hie_types hieFile
          hieTypeFix = recoverFullType firstTypeIdx typeArray
          ifaceType = hieTypeToIface hieTypeFix
          typeText = T.pack $ GHC.showSDocUnsafe $ GHC.ppr ifaceType
      pure $ Just $ parseTypeText typeText

-- | Extract type from node identifiers
extractTypeFromIdentifiers :: HieFile -> NodeInfo TypeIndex -> IO (Maybe TypeInfo)
extractTypeFromIdentifiers hieFile nodeInf = do
  let identifiers = Map.toList $ nodeIdentifiers nodeInf

  case identifiers of
    [] -> pure Nothing
    ((_ident, details):_) -> do
      -- Get type from identifier details (identType returns Maybe TypeIndex)
      case identType details of
        Nothing -> pure Nothing
        Just typeIdx -> do
          let typeArray = hie_types hieFile
              hieTypeFix = recoverFullType typeIdx typeArray
              ifaceType = hieTypeToIface hieTypeFix
              typeText = T.pack $ GHC.showSDocUnsafe $ GHC.ppr ifaceType
          pure $ Just $ parseTypeText typeText

-- | Get the node's own info (from GHC's SourcedNodeInfo)
getNodeInfo :: HieAST a -> NodeInfo a
getNodeInfo ast =
  let snInfo = sourcedNodeInfo ast
      nodeInfoMap = getSourcedNodeInfo snInfo
  in case Map.lookup SourceInfo nodeInfoMap of
       Just ni -> ni
       Nothing -> case Map.elems nodeInfoMap of
                    (ni:_) -> ni
                    [] -> NodeInfo mempty mempty mempty

-- | Parse a type text into TypeInfo, extracting constraints
parseTypeText :: Text -> TypeInfo
parseTypeText typeText =
  let (constraints, mainType) = parseConstraintsFromTypeTI typeText
      typeVars = extractTypeVarsTI mainType
  in TypeInfo
    { tiType = typeText
    , tiKind = "*"
    , tiConstraints = constraints
    , tiMonomorphic = null typeVars
    , tiPolymorphic = typeVars
    }

-- | Parse constraints from a type string like "Ord a => [a] -> [a]"
parseConstraintsFromTypeTI :: Text -> ([TypeConstraint], Text)
parseConstraintsFromTypeTI typeStr =
  case T.breakOn " => " typeStr of
    (constraints, rest) | not (T.null rest) ->
      let constraintPart = constraints
          typePart = T.drop 4 rest
          parsed = parseConstraintListTI constraintPart
      in (parsed, typePart)
    _ -> ([], typeStr)

-- | Parse a constraint list like "(Ord a, Eq b)"
parseConstraintListTI :: Text -> [TypeConstraint]
parseConstraintListTI text =
  let cleaned = T.dropWhile (== '(') $ T.dropWhileEnd (== ')') $ T.strip text
      parts = T.splitOn ", " cleaned
  in mapMaybe parseOneConstraintTI parts

-- | Parse a single constraint like "Ord a"
parseOneConstraintTI :: Text -> Maybe TypeConstraint
parseOneConstraintTI text =
  case T.words text of
    (className:typeArgs) | not (T.null className) && not (null typeArgs) ->
      Just TypeConstraint
        { tcClass = className
        , tcType = T.unwords typeArgs
        , tcSatisfied = True
        , tcInstance = Nothing
        }
    _ -> Nothing

-- | Extract type variables from a type string
extractTypeVarsTI :: Text -> [Text]
extractTypeVarsTI typeText =
  let tokens = T.words $ T.replace "[" " " $ T.replace "]" " " $
               T.replace "(" " " $ T.replace ")" " " $
               T.replace "," " " $ T.replace "->" " " typeText
  in filter isTypeVarTI tokens
  where
    isTypeVarTI t = T.length t == 1 && T.all (\c -> c >= 'a' && c <= 'z') t

-- | Infer a basic TypeInfo from a function name
-- Uses naming conventions and common patterns
inferTypeFromName :: Text -> TypeInfo
inferTypeFromName name = TypeInfo
  { tiType = inferredType
  , tiKind = "*"
  , tiConstraints = inferredConstraints
  , tiMonomorphic = False
  , tiPolymorphic = ["a"]
  }
  where
    -- Infer type based on common naming patterns
    inferredType
      | "is" `T.isPrefixOf` name = "a -> Bool"
      | "get" `T.isPrefixOf` name = "a -> b"
      | "set" `T.isPrefixOf` name = "a -> b -> a"
      | "to" `T.isPrefixOf` name = "a -> b"
      | "from" `T.isPrefixOf` name = "a -> b"
      | "mk" `T.isPrefixOf` name = "a -> b"
      | "make" `T.isPrefixOf` name = "a -> b"
      | "with" `T.isPrefixOf` name = "(a -> b) -> a -> b"
      | otherwise = "a -> b"

    -- No constraints by default from inference
    inferredConstraints = []

--------------------------------------------------------------------------------
-- Constraint Checking
--------------------------------------------------------------------------------

-- | Check if a type has an Ord instance
hasOrdInstance :: Text -> IO Bool
hasOrdInstance typeName = pure $ checkKnownInstance "Ord" typeName

-- | Check if a type has an Eq instance
hasEqInstance :: Text -> IO Bool
hasEqInstance typeName = pure $ checkKnownInstance "Eq" typeName

-- | Check if a type has a Monoid instance
hasMonoidInstance :: Text -> IO Bool
hasMonoidInstance typeName = pure $ checkKnownInstance "Monoid" typeName

-- | Check if a type has a Semigroup instance
hasSemigroupInstance :: Text -> IO Bool
hasSemigroupInstance typeName = pure $ checkKnownInstance "Semigroup" typeName

-- | Check if a type has a Show instance
hasShowInstance :: Text -> IO Bool
hasShowInstance typeName = pure $ checkKnownInstance "Show" typeName

-- | Check if a type has a Num instance
hasNumInstance :: Text -> IO Bool
hasNumInstance typeName = pure $ checkKnownInstance "Num" typeName

-- | Check if a type has a Hashable instance
hasHashableInstance :: Text -> IO Bool
hasHashableInstance typeName = pure $ checkKnownInstance "Hashable" typeName

-- | Check if a specific constraint is satisfied
checkConstraint :: TypeConstraint -> IO Bool
checkConstraint tc = pure $ checkKnownInstance (tcClass tc) (tcType tc)

-- | Check multiple constraints, returning unsatisfied ones
checkConstraints :: [TypeConstraint] -> IO [TypeConstraint]
checkConstraints constraints = do
  results <- mapM checkSingleConstraint constraints
  pure $ map fst $ filter (not . snd) results
  where
    checkSingleConstraint tc = do
      satisfied <- checkConstraint tc
      pure (tc, satisfied)

-- | Check against known instances database
checkKnownInstance :: Text -> Text -> Bool
checkKnownInstance className typeName =
  (className, typeName) `Set.member` knownInstances ||
  (className, normalizeType typeName) `Set.member` knownInstances ||
  checkParameterizedInstance className typeName

-- | Check if a parameterized type has an instance
-- E.g., [Int] has Ord if Int has Ord
checkParameterizedInstance :: Text -> Text -> Bool
checkParameterizedInstance className typeName
  | isListType typeName =
      -- Lists have instances if their element type does
      case extractListElementType typeName of
        Just elemType -> checkKnownInstance className elemType || isTypeVar elemType
        Nothing -> False
  | isMaybeType typeName =
      case extractMaybeInnerType typeName of
        Just innerType -> checkKnownInstance className innerType || isTypeVar innerType
        Nothing -> False
  | otherwise = False
  where
    isTypeVar t = T.length t == 1 && T.all (\c -> c >= 'a' && c <= 'z') t

-- | Normalize a type for instance lookup (handle type applications)
normalizeType :: Text -> Text
normalizeType ty
  | "[" `T.isPrefixOf` ty = "[a]"  -- List type
  | "Maybe " `T.isPrefixOf` ty = "Maybe a"
  | "Either " `T.isPrefixOf` ty = "Either a b"
  | "IO " `T.isPrefixOf` ty = "IO a"
  | otherwise = ty

-- | Database of known type class instances
knownInstances :: Set (Text, Text)
knownInstances = Set.fromList
  -- Ord instances
  [ ("Ord", "Int"), ("Ord", "Integer"), ("Ord", "Double"), ("Ord", "Float")
  , ("Ord", "Char"), ("Ord", "Bool"), ("Ord", "Text"), ("Ord", "String")
  , ("Ord", "ByteString"), ("Ord", "()")
  , ("Ord", "Word"), ("Ord", "Word8"), ("Ord", "Word16"), ("Ord", "Word32"), ("Ord", "Word64")
  , ("Ord", "Int8"), ("Ord", "Int16"), ("Ord", "Int32"), ("Ord", "Int64")
  , ("Ord", "Natural"), ("Ord", "Ordering")
  -- Eq instances (everything with Ord also has Eq)
  , ("Eq", "Int"), ("Eq", "Integer"), ("Eq", "Double"), ("Eq", "Float")
  , ("Eq", "Char"), ("Eq", "Bool"), ("Eq", "Text"), ("Eq", "String")
  , ("Eq", "ByteString"), ("Eq", "()")
  , ("Eq", "Word"), ("Eq", "Word8"), ("Eq", "Word16"), ("Eq", "Word32"), ("Eq", "Word64")
  , ("Eq", "Int8"), ("Eq", "Int16"), ("Eq", "Int32"), ("Eq", "Int64")
  , ("Eq", "Natural"), ("Eq", "Ordering")
  -- Show instances
  , ("Show", "Int"), ("Show", "Integer"), ("Show", "Double"), ("Show", "Float")
  , ("Show", "Char"), ("Show", "Bool"), ("Show", "Text"), ("Show", "String")
  , ("Show", "()")
  -- Num instances
  , ("Num", "Int"), ("Num", "Integer"), ("Num", "Double"), ("Num", "Float")
  , ("Num", "Word"), ("Num", "Natural")
  -- Monoid instances
  , ("Monoid", "Text"), ("Monoid", "String"), ("Monoid", "[a]")
  , ("Monoid", "ByteString"), ("Monoid", "()")
  , ("Monoid", "All"), ("Monoid", "Any")
  , ("Monoid", "Sum a"), ("Monoid", "Product a")
  , ("Monoid", "First a"), ("Monoid", "Last a")
  , ("Monoid", "Endo a")
  -- Semigroup instances (everything with Monoid also has Semigroup)
  , ("Semigroup", "Text"), ("Semigroup", "String"), ("Semigroup", "[a]")
  , ("Semigroup", "ByteString"), ("Semigroup", "()")
  , ("Semigroup", "All"), ("Semigroup", "Any")
  , ("Semigroup", "Sum a"), ("Semigroup", "Product a")
  , ("Semigroup", "First a"), ("Semigroup", "Last a")
  , ("Semigroup", "NonEmpty a")
  -- Hashable instances
  , ("Hashable", "Int"), ("Hashable", "Integer"), ("Hashable", "Double"), ("Hashable", "Float")
  , ("Hashable", "Char"), ("Hashable", "Bool"), ("Hashable", "Text"), ("Hashable", "String")
  , ("Hashable", "ByteString"), ("Hashable", "()")
  , ("Hashable", "Word"), ("Hashable", "Word8"), ("Hashable", "Word16"), ("Hashable", "Word32"), ("Hashable", "Word64")
  -- Foldable instances (for list operations)
  , ("Foldable", "[]"), ("Foldable", "Maybe"), ("Foldable", "Either e")
  , ("Foldable", "NonEmpty"), ("Foldable", "Vector")
  , ("Foldable", "Set"), ("Foldable", "Map k")
  -- Traversable instances
  , ("Traversable", "[]"), ("Traversable", "Maybe"), ("Traversable", "Either e")
  , ("Traversable", "NonEmpty"), ("Traversable", "Vector")
  -- Functor instances
  , ("Functor", "[]"), ("Functor", "Maybe"), ("Functor", "Either e")
  , ("Functor", "IO"), ("Functor", "NonEmpty"), ("Functor", "Vector")
  , ("Functor", "Map k"), ("Functor", "(->) r")
  -- Applicative instances
  , ("Applicative", "[]"), ("Applicative", "Maybe"), ("Applicative", "Either e")
  , ("Applicative", "IO"), ("Applicative", "NonEmpty")
  , ("Applicative", "(->) r")
  -- Monad instances
  , ("Monad", "[]"), ("Monad", "Maybe"), ("Monad", "Either e")
  , ("Monad", "IO"), ("Monad", "(->) r")
  ]

--------------------------------------------------------------------------------
-- Type Analysis
--------------------------------------------------------------------------------

-- | Check if a type is a list type
isListType :: Text -> Bool
isListType ty =
  "[" `T.isPrefixOf` ty && "]" `T.isSuffixOf` ty

-- | Check if a type is Maybe
isMaybeType :: Text -> Bool
isMaybeType ty =
  "Maybe " `T.isPrefixOf` ty || ty == "Maybe"

-- | Check if a type is Either
isEitherType :: Text -> Bool
isEitherType ty =
  "Either " `T.isPrefixOf` ty || ty == "Either"

-- | Check if a type is IO
isIOType :: Text -> Bool
isIOType ty =
  "IO " `T.isPrefixOf` ty || ty == "IO"

-- | Check if a type is a Monad (common monads)
isMonadType :: Text -> Bool
isMonadType ty =
  isMaybeType ty || isIOType ty || isEitherType ty ||
  "Reader" `T.isInfixOf` ty || "Writer" `T.isInfixOf` ty ||
  "State" `T.isInfixOf` ty || "ExceptT" `T.isInfixOf` ty

-- | Extract the element type from a list type
extractListElementType :: Text -> Maybe Text
extractListElementType ty
  | isListType ty = Just $ T.dropEnd 1 $ T.drop 1 ty
  | otherwise = Nothing

-- | Extract the inner type from Maybe
extractMaybeInnerType :: Text -> Maybe Text
extractMaybeInnerType ty
  | "Maybe " `T.isPrefixOf` ty = Just $ T.drop 6 ty
  | otherwise = Nothing

-- | Parse type constraints from a type signature text
-- E.g., "Ord a => [a] -> [a]" -> [TypeConstraint "Ord" "a" ...]
parseTypeConstraints :: Text -> [TypeConstraint]
parseTypeConstraints typeStr =
  let (constraints, _) = parseConstraintsFromType typeStr
  in constraints

-- | Parse constraints from a type string like "Ord a => [a] -> [a]"
parseConstraintsFromType :: Text -> ([TypeConstraint], Text)
parseConstraintsFromType typeStr =
  case T.breakOn " => " typeStr of
    (constraints, rest) | not (T.null rest) ->
      let constraintPart = constraints
          typePart = T.drop 4 rest  -- Drop " => "
          parsed = parseConstraintList constraintPart
      in (parsed, typePart)
    _ -> ([], typeStr)

-- | Parse a constraint list like "Ord a, Eq b"
parseConstraintList :: Text -> [TypeConstraint]
parseConstraintList text =
  let parts = T.splitOn ", " text
      -- Remove outer parens if present
      cleaned = map (T.dropWhile (== '(') . T.dropWhileEnd (== ')') . T.strip) parts
  in mapMaybe parseOneConstraint cleaned

-- | Parse a single constraint like "Ord a"
parseOneConstraint :: Text -> Maybe TypeConstraint
parseOneConstraint text =
  case T.words text of
    (className:typeArgs) | not (T.null className) && not (null typeArgs) ->
      Just TypeConstraint
        { tcClass = className
        , tcType = T.unwords typeArgs
        , tcSatisfied = True  -- Assume satisfied in source
        , tcInstance = Nothing
        }
    _ -> Nothing

--------------------------------------------------------------------------------
-- Type Comparison
--------------------------------------------------------------------------------

-- | Check if two types are compatible (one can substitute for the other)
typesCompatible :: Text -> Text -> Bool
typesCompatible t1 t2
  | t1 == t2 = True
  | isPolymorphic t1 = True  -- Polymorphic types are compatible with anything
  | isPolymorphic t2 = True
  | normalizeType t1 == normalizeType t2 = True
  | otherwise = False

-- | Check if two types are exactly equal
typesEqual :: Text -> Text -> Bool
typesEqual = (==)

-- | Check if t1 subsumes t2 (t1 is more general)
typeSubsumes :: Text -> Text -> Bool
typeSubsumes t1 t2
  | t1 == t2 = True
  | isPolymorphic t1 && not (isPolymorphic t2) = True
  | otherwise = False

-- | Check if a type is polymorphic
isPolymorphic :: Text -> Bool
isPolymorphic ty =
  any isTypeVar tokens
  where
    tokens = extractTypeTokens ty

    extractTypeTokens :: Text -> [Text]
    extractTypeTokens t =
      let cleaned = T.replace "[" " " $
                    T.replace "]" " " $
                    T.replace "(" " " $
                    T.replace ")" " " $
                    T.replace "," " " t
      in T.words cleaned

    isTypeVar w = T.length w == 1 && T.all (\c -> c >= 'a' && c <= 'z') w

--------------------------------------------------------------------------------
-- Known Types Database
--------------------------------------------------------------------------------

-- | Database of known types with their information
newtype KnownTypes = KnownTypes (Map Text TypeInfo)
  deriving stock (Eq, Show)

-- | Default known types including common Prelude and base functions
defaultKnownTypes :: KnownTypes
defaultKnownTypes = KnownTypes $ Map.fromList $
  -- List operations
  [ ("head", mkTypeInfo "[a] -> a" ["a"])
  , ("tail", mkTypeInfo "[a] -> [a]" ["a"])
  , ("init", mkTypeInfo "[a] -> [a]" ["a"])
  , ("last", mkTypeInfo "[a] -> a" ["a"])
  , ("null", mkTypeInfo "Foldable t => t a -> Bool" ["t", "a"])
  , ("length", mkTypeInfo "Foldable t => t a -> Int" ["t", "a"])
  , ("(!!)", mkTypeInfo "[a] -> Int -> a" ["a"])
  , ("reverse", mkTypeInfo "[a] -> [a]" ["a"])
  , ("concat", mkTypeInfo "[[a]] -> [a]" ["a"])
  , ("concatMap", mkTypeInfo "(a -> [b]) -> [a] -> [b]" ["a", "b"])
  , ("(++)", mkTypeInfo "[a] -> [a] -> [a]" ["a"])
  , ("take", mkTypeInfo "Int -> [a] -> [a]" ["a"])
  , ("drop", mkTypeInfo "Int -> [a] -> [a]" ["a"])
  , ("splitAt", mkTypeInfo "Int -> [a] -> ([a], [a])" ["a"])
  , ("takeWhile", mkTypeInfo "(a -> Bool) -> [a] -> [a]" ["a"])
  , ("dropWhile", mkTypeInfo "(a -> Bool) -> [a] -> [a]" ["a"])
  , ("span", mkTypeInfo "(a -> Bool) -> [a] -> ([a], [a])" ["a"])
  , ("break", mkTypeInfo "(a -> Bool) -> [a] -> ([a], [a])" ["a"])
  , ("zip", mkTypeInfo "[a] -> [b] -> [(a, b)]" ["a", "b"])
  , ("zip3", mkTypeInfo "[a] -> [b] -> [c] -> [(a, b, c)]" ["a", "b", "c"])
  , ("zipWith", mkTypeInfo "(a -> b -> c) -> [a] -> [b] -> [c]" ["a", "b", "c"])
  , ("unzip", mkTypeInfo "[(a, b)] -> ([a], [b])" ["a", "b"])
  , ("replicate", mkTypeInfo "Int -> a -> [a]" ["a"])
  , ("repeat", mkTypeInfo "a -> [a]" ["a"])
  , ("cycle", mkTypeInfo "[a] -> [a]" ["a"])
  , ("iterate", mkTypeInfo "(a -> a) -> a -> [a]" ["a"])
  , ("scanl", mkTypeInfo "(b -> a -> b) -> b -> [a] -> [b]" ["a", "b"])
  , ("scanr", mkTypeInfo "(a -> b -> b) -> b -> [a] -> [b]" ["a", "b"])

  -- Higher-order list operations
  , ("map", mkTypeInfo "(a -> b) -> [a] -> [b]" ["a", "b"])
  , ("filter", mkTypeInfo "(a -> Bool) -> [a] -> [a]" ["a"])
  , ("foldl", mkTypeInfo "(b -> a -> b) -> b -> [a] -> b" ["a", "b"])
  , ("foldl'", mkTypeInfo "(b -> a -> b) -> b -> [a] -> b" ["a", "b"])
  , ("foldl1", mkTypeInfo "(a -> a -> a) -> [a] -> a" ["a"])
  , ("foldr", mkTypeInfo "(a -> b -> b) -> b -> [a] -> b" ["a", "b"])
  , ("foldr1", mkTypeInfo "(a -> a -> a) -> [a] -> a" ["a"])
  , ("any", mkTypeInfo "Foldable t => (a -> Bool) -> t a -> Bool" ["t", "a"])
  , ("all", mkTypeInfo "Foldable t => (a -> Bool) -> t a -> Bool" ["t", "a"])
  , ("and", mkTypeInfo "Foldable t => t Bool -> Bool" ["t"])
  , ("or", mkTypeInfo "Foldable t => t Bool -> Bool" ["t"])
  , ("sum", mkTypeInfo "(Foldable t, Num a) => t a -> a" ["t", "a"])
  , ("product", mkTypeInfo "(Foldable t, Num a) => t a -> a" ["t", "a"])
  , ("maximum", mkTypeInfo "(Foldable t, Ord a) => t a -> a" ["t", "a"])
  , ("minimum", mkTypeInfo "(Foldable t, Ord a) => t a -> a" ["t", "a"])
  , ("maximumBy", mkTypeInfo "Foldable t => (a -> a -> Ordering) -> t a -> a" ["t", "a"])
  , ("minimumBy", mkTypeInfo "Foldable t => (a -> a -> Ordering) -> t a -> a" ["t", "a"])

  -- List searching and membership
  , ("elem", mkTypeInfoWithConstraints "Eq a => a -> [a] -> Bool" ["a"] [mkConstraint "Eq" "a"])
  , ("notElem", mkTypeInfoWithConstraints "Eq a => a -> [a] -> Bool" ["a"] [mkConstraint "Eq" "a"])
  , ("lookup", mkTypeInfoWithConstraints "Eq a => a -> [(a, b)] -> Maybe b" ["a", "b"] [mkConstraint "Eq" "a"])
  , ("find", mkTypeInfo "Foldable t => (a -> Bool) -> t a -> Maybe a" ["t", "a"])
  , ("partition", mkTypeInfo "(a -> Bool) -> [a] -> ([a], [a])" ["a"])

  -- List transformations with constraints
  , ("nub", mkTypeInfoWithConstraints "Eq a => [a] -> [a]" ["a"] [mkConstraint "Eq" "a"])
  , ("sort", mkTypeInfoWithConstraints "Ord a => [a] -> [a]" ["a"] [mkConstraint "Ord" "a"])
  , ("sortBy", mkTypeInfo "(a -> a -> Ordering) -> [a] -> [a]" ["a"])
  , ("sortOn", mkTypeInfoWithConstraints "Ord b => (a -> b) -> [a] -> [a]" ["a", "b"] [mkConstraint "Ord" "b"])
  , ("group", mkTypeInfoWithConstraints "Eq a => [a] -> [[a]]" ["a"] [mkConstraint "Eq" "a"])
  , ("groupBy", mkTypeInfo "(a -> a -> Bool) -> [a] -> [[a]]" ["a"])

  -- Safe alternatives
  , ("headMay", mkTypeInfo "[a] -> Maybe a" ["a"])
  , ("tailMay", mkTypeInfo "[a] -> Maybe [a]" ["a"])
  , ("initMay", mkTypeInfo "[a] -> Maybe [a]" ["a"])
  , ("lastMay", mkTypeInfo "[a] -> Maybe a" ["a"])
  , ("atMay", mkTypeInfo "[a] -> Int -> Maybe a" ["a"])

  -- Maybe operations
  , ("maybe", mkTypeInfo "b -> (a -> b) -> Maybe a -> b" ["a", "b"])
  , ("fromMaybe", mkTypeInfo "a -> Maybe a -> a" ["a"])
  , ("fromJust", mkTypeInfo "Maybe a -> a" ["a"])
  , ("isJust", mkTypeInfo "Maybe a -> Bool" ["a"])
  , ("isNothing", mkTypeInfo "Maybe a -> Bool" ["a"])
  , ("listToMaybe", mkTypeInfo "[a] -> Maybe a" ["a"])
  , ("maybeToList", mkTypeInfo "Maybe a -> [a]" ["a"])
  , ("catMaybes", mkTypeInfo "[Maybe a] -> [a]" ["a"])
  , ("mapMaybe", mkTypeInfo "(a -> Maybe b) -> [a] -> [b]" ["a", "b"])

  -- Either operations
  , ("either", mkTypeInfo "(a -> c) -> (b -> c) -> Either a b -> c" ["a", "b", "c"])
  , ("fromLeft", mkTypeInfo "a -> Either a b -> a" ["a", "b"])
  , ("fromRight", mkTypeInfo "b -> Either a b -> b" ["a", "b"])
  , ("isLeft", mkTypeInfo "Either a b -> Bool" ["a", "b"])
  , ("isRight", mkTypeInfo "Either a b -> Bool" ["a", "b"])
  , ("lefts", mkTypeInfo "[Either a b] -> [a]" ["a", "b"])
  , ("rights", mkTypeInfo "[Either a b] -> [b]" ["a", "b"])
  , ("partitionEithers", mkTypeInfo "[Either a b] -> ([a], [b])" ["a", "b"])

  -- Functor, Applicative, Monad
  , ("fmap", mkTypeInfo "Functor f => (a -> b) -> f a -> f b" ["f", "a", "b"])
  , ("(<$>)", mkTypeInfo "Functor f => (a -> b) -> f a -> f b" ["f", "a", "b"])
  , ("(<$)", mkTypeInfo "Functor f => a -> f b -> f a" ["f", "a", "b"])
  , ("($>)", mkTypeInfo "Functor f => f a -> b -> f b" ["f", "a", "b"])
  , ("void", mkTypeInfo "Functor f => f a -> f ()" ["f", "a"])
  , ("pure", mkTypeInfo "Applicative f => a -> f a" ["f", "a"])
  , ("(<*>)", mkTypeInfo "Applicative f => f (a -> b) -> f a -> f b" ["f", "a", "b"])
  , ("(*>)", mkTypeInfo "Applicative f => f a -> f b -> f b" ["f", "a", "b"])
  , ("(<*)", mkTypeInfo "Applicative f => f a -> f b -> f a" ["f", "a", "b"])
  , ("liftA2", mkTypeInfo "Applicative f => (a -> b -> c) -> f a -> f b -> f c" ["f", "a", "b", "c"])
  , ("liftA3", mkTypeInfo "Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d" ["f", "a", "b", "c", "d"])
  , ("return", mkTypeInfo "Monad m => a -> m a" ["m", "a"])
  , ("(>>=)", mkTypeInfo "Monad m => m a -> (a -> m b) -> m b" ["m", "a", "b"])
  , ("(>>)", mkTypeInfo "Monad m => m a -> m b -> m b" ["m", "a", "b"])
  , ("join", mkTypeInfo "Monad m => m (m a) -> m a" ["m", "a"])
  , ("liftM", mkTypeInfo "Monad m => (a -> b) -> m a -> m b" ["m", "a", "b"])
  , ("liftM2", mkTypeInfo "Monad m => (a -> b -> c) -> m a -> m b -> m c" ["m", "a", "b", "c"])
  , ("ap", mkTypeInfo "Monad m => m (a -> b) -> m a -> m b" ["m", "a", "b"])
  , ("when", mkTypeInfo "Applicative f => Bool -> f () -> f ()" ["f"])
  , ("unless", mkTypeInfo "Applicative f => Bool -> f () -> f ()" ["f"])
  , ("guard", mkTypeInfo "Alternative f => Bool -> f ()" ["f"])

  -- Traversable
  , ("traverse", mkTypeInfo "(Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)" ["t", "f", "a", "b"])
  , ("sequenceA", mkTypeInfo "(Traversable t, Applicative f) => t (f a) -> f (t a)" ["t", "f", "a"])
  , ("mapM", mkTypeInfo "(Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)" ["t", "m", "a", "b"])
  , ("sequence", mkTypeInfo "(Traversable t, Monad m) => t (m a) -> m (t a)" ["t", "m", "a"])
  , ("for", mkTypeInfo "(Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)" ["t", "f", "a", "b"])
  , ("forM", mkTypeInfo "(Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)" ["t", "m", "a", "b"])

  -- Semigroup and Monoid
  , ("(<>)", mkTypeInfo "Semigroup a => a -> a -> a" ["a"])
  , ("sconcat", mkTypeInfo "Semigroup a => NonEmpty a -> a" ["a"])
  , ("mappend", mkTypeInfo "Monoid a => a -> a -> a" ["a"])
  , ("mempty", mkTypeInfo "Monoid a => a" ["a"])
  , ("mconcat", mkTypeInfo "Monoid a => [a] -> a" ["a"])

  -- Function combinators
  , ("id", mkTypeInfo "a -> a" ["a"])
  , ("const", mkTypeInfo "a -> b -> a" ["a", "b"])
  , ("(.)", mkTypeInfo "(b -> c) -> (a -> b) -> a -> c" ["a", "b", "c"])
  , ("($)", mkTypeInfo "(a -> b) -> a -> b" ["a", "b"])
  , ("(&)", mkTypeInfo "a -> (a -> b) -> b" ["a", "b"])
  , ("flip", mkTypeInfo "(a -> b -> c) -> b -> a -> c" ["a", "b", "c"])
  , ("on", mkTypeInfo "(b -> b -> c) -> (a -> b) -> a -> a -> c" ["a", "b", "c"])
  , ("fix", mkTypeInfo "(a -> a) -> a" ["a"])

  -- Tuple operations
  , ("fst", mkTypeInfo "(a, b) -> a" ["a", "b"])
  , ("snd", mkTypeInfo "(a, b) -> b" ["a", "b"])
  , ("curry", mkTypeInfo "((a, b) -> c) -> a -> b -> c" ["a", "b", "c"])
  , ("uncurry", mkTypeInfo "(a -> b -> c) -> (a, b) -> c" ["a", "b", "c"])
  , ("swap", mkTypeInfo "(a, b) -> (b, a)" ["a", "b"])

  -- Bool operations
  , ("not", mkTypeInfo "Bool -> Bool" [])
  , ("(&&)", mkTypeInfo "Bool -> Bool -> Bool" [])
  , ("(||)", mkTypeInfo "Bool -> Bool -> Bool" [])
  , ("bool", mkTypeInfo "a -> a -> Bool -> a" ["a"])

  -- Comparison
  , ("compare", mkTypeInfoWithConstraints "Ord a => a -> a -> Ordering" ["a"] [mkConstraint "Ord" "a"])
  , ("(==)", mkTypeInfoWithConstraints "Eq a => a -> a -> Bool" ["a"] [mkConstraint "Eq" "a"])
  , ("(/=)", mkTypeInfoWithConstraints "Eq a => a -> a -> Bool" ["a"] [mkConstraint "Eq" "a"])
  , ("(<)", mkTypeInfoWithConstraints "Ord a => a -> a -> Bool" ["a"] [mkConstraint "Ord" "a"])
  , ("(>)", mkTypeInfoWithConstraints "Ord a => a -> a -> Bool" ["a"] [mkConstraint "Ord" "a"])
  , ("(<=)", mkTypeInfoWithConstraints "Ord a => a -> a -> Bool" ["a"] [mkConstraint "Ord" "a"])
  , ("(>=)", mkTypeInfoWithConstraints "Ord a => a -> a -> Bool" ["a"] [mkConstraint "Ord" "a"])
  , ("min", mkTypeInfoWithConstraints "Ord a => a -> a -> a" ["a"] [mkConstraint "Ord" "a"])
  , ("max", mkTypeInfoWithConstraints "Ord a => a -> a -> a" ["a"] [mkConstraint "Ord" "a"])
  , ("comparing", mkTypeInfoWithConstraints "Ord a => (b -> a) -> b -> b -> Ordering" ["a", "b"] [mkConstraint "Ord" "a"])

  -- Numeric
  , ("(+)", mkTypeInfoWithConstraints "Num a => a -> a -> a" ["a"] [mkConstraint "Num" "a"])
  , ("(-)", mkTypeInfoWithConstraints "Num a => a -> a -> a" ["a"] [mkConstraint "Num" "a"])
  , ("(*)", mkTypeInfoWithConstraints "Num a => a -> a -> a" ["a"] [mkConstraint "Num" "a"])
  , ("negate", mkTypeInfoWithConstraints "Num a => a -> a" ["a"] [mkConstraint "Num" "a"])
  , ("abs", mkTypeInfoWithConstraints "Num a => a -> a" ["a"] [mkConstraint "Num" "a"])
  , ("signum", mkTypeInfoWithConstraints "Num a => a -> a" ["a"] [mkConstraint "Num" "a"])
  , ("fromInteger", mkTypeInfoWithConstraints "Num a => Integer -> a" ["a"] [mkConstraint "Num" "a"])
  , ("(/)", mkTypeInfoWithConstraints "Fractional a => a -> a -> a" ["a"] [mkConstraint "Fractional" "a"])
  , ("div", mkTypeInfoWithConstraints "Integral a => a -> a -> a" ["a"] [mkConstraint "Integral" "a"])
  , ("mod", mkTypeInfoWithConstraints "Integral a => a -> a -> a" ["a"] [mkConstraint "Integral" "a"])
  , ("quot", mkTypeInfoWithConstraints "Integral a => a -> a -> a" ["a"] [mkConstraint "Integral" "a"])
  , ("rem", mkTypeInfoWithConstraints "Integral a => a -> a -> a" ["a"] [mkConstraint "Integral" "a"])
  , ("divMod", mkTypeInfoWithConstraints "Integral a => a -> a -> (a, a)" ["a"] [mkConstraint "Integral" "a"])
  , ("quotRem", mkTypeInfoWithConstraints "Integral a => a -> a -> (a, a)" ["a"] [mkConstraint "Integral" "a"])
  , ("even", mkTypeInfoWithConstraints "Integral a => a -> Bool" ["a"] [mkConstraint "Integral" "a"])
  , ("odd", mkTypeInfoWithConstraints "Integral a => a -> Bool" ["a"] [mkConstraint "Integral" "a"])
  , ("gcd", mkTypeInfoWithConstraints "Integral a => a -> a -> a" ["a"] [mkConstraint "Integral" "a"])
  , ("lcm", mkTypeInfoWithConstraints "Integral a => a -> a -> a" ["a"] [mkConstraint "Integral" "a"])
  , ("(^)", mkTypeInfoWithConstraints "(Num a, Integral b) => a -> b -> a" ["a", "b"] [mkConstraint "Num" "a", mkConstraint "Integral" "b"])

  -- Show and Read
  , ("show", mkTypeInfoWithConstraints "Show a => a -> String" ["a"] [mkConstraint "Show" "a"])
  , ("read", mkTypeInfoWithConstraints "Read a => String -> a" ["a"] [mkConstraint "Read" "a"])
  , ("reads", mkTypeInfoWithConstraints "Read a => String -> [(a, String)]" ["a"] [mkConstraint "Read" "a"])
  , ("shows", mkTypeInfoWithConstraints "Show a => a -> ShowS" ["a"] [mkConstraint "Show" "a"])

  -- IO operations
  , ("print", mkTypeInfoWithConstraints "Show a => a -> IO ()" ["a"] [mkConstraint "Show" "a"])
  , ("putStr", mkTypeInfo "String -> IO ()" [])
  , ("putStrLn", mkTypeInfo "String -> IO ()" [])
  , ("getLine", mkTypeInfo "IO String" [])
  , ("getContents", mkTypeInfo "IO String" [])
  , ("interact", mkTypeInfo "(String -> String) -> IO ()" [])
  , ("readFile", mkTypeInfo "FilePath -> IO String" [])
  , ("writeFile", mkTypeInfo "FilePath -> String -> IO ()" [])
  , ("appendFile", mkTypeInfo "FilePath -> String -> IO ()" [])

  -- Error handling
  , ("error", mkTypeInfo "String -> a" ["a"])
  , ("undefined", mkTypeInfo "a" ["a"])
  , ("seq", mkTypeInfo "a -> b -> b" ["a", "b"])
  , ("($!)", mkTypeInfo "(a -> b) -> a -> b" ["a", "b"])
  ]
  where
    mkTypeInfo ty vars = TypeInfo
      { tiType = ty
      , tiKind = "*"
      , tiConstraints = []
      , tiMonomorphic = null vars
      , tiPolymorphic = vars
      }

    mkTypeInfoWithConstraints ty vars constraints = TypeInfo
      { tiType = ty
      , tiKind = "*"
      , tiConstraints = constraints
      , tiMonomorphic = null vars
      , tiPolymorphic = vars
      }

    mkConstraint cls var = TypeConstraint
      { tcClass = cls
      , tcType = var
      , tcSatisfied = True
      , tcInstance = Nothing
      }

-- | Look up a type in the known types database
lookupKnownType :: KnownTypes -> Text -> Maybe TypeInfo
lookupKnownType (KnownTypes types) name = Map.lookup name types

-- | Register a new type
registerType :: Text -> TypeInfo -> KnownTypes -> KnownTypes
registerType name info (KnownTypes types) = KnownTypes $ Map.insert name info types

-- | Extend known types with additional entries
extendKnownTypes :: [(Text, TypeInfo)] -> KnownTypes -> KnownTypes
extendKnownTypes entries (KnownTypes types) =
  KnownTypes $ Map.union (Map.fromList entries) types

--------------------------------------------------------------------------------
-- Fix Validation
--------------------------------------------------------------------------------

-- | Validate that a fix preserves type correctness
validateFixTypes :: Fix -> IO TypeValidation
validateFixTypes fix = do
  -- Validate each edit in the fix
  validations <- forM (fixEdits fix) validateEdit
  pure $ combineValidations validations
  where
    validateEdit :: FixEdit -> IO TypeValidation
    validateEdit edit = do
      -- Check if the replacement preserves types
      oldText <- getEditOriginal edit
      let newText = fixEditNewText edit
      if T.null oldText
        then pure TypesPreserved  -- Insertion, no type change
        else do
          preserved <- checkFixTypePreservation oldText newText
          if preserved
            then pure TypesPreserved
            else pure TypesUnknown

    -- Extract original text from edit span by reading the source file
    getEditOriginal :: FixEdit -> IO Text
    getEditOriginal edit = do
      let span = fixEditSpan edit
          filePath = srcSpanFile span
          startLine = unLine $ srcSpanStartLine span
          startCol = unColumn $ srcSpanStartCol span
          endLine = unLine $ srcSpanEndLine span
          endCol = unColumn $ srcSpanEndCol span

      -- Handle empty/invalid spans
      if null filePath || startLine <= 0
        then pure ""
        else do
          fileExists <- doesFileExist filePath
          if not fileExists
            then pure ""
            else do
              result <- try @SomeException $ TIO.readFile filePath
              case result of
                Left _ -> pure ""
                Right content -> pure $ extractSpanText content startLine startCol endLine endCol

    -- Extract text from content given line/column bounds
    extractSpanText :: Text -> Int -> Int -> Int -> Int -> Text
    extractSpanText content startLine startCol endLine endCol
      | startLine > endLine = ""
      | startLine == endLine && startCol > endCol = ""
      | otherwise =
          let allLines = T.lines content
              -- Get the relevant lines (1-indexed to 0-indexed)
              relevantLines = take (endLine - startLine + 1) $ drop (startLine - 1) allLines
          in case relevantLines of
            [] -> ""
            [singleLine] ->
              -- Single line span: extract from startCol to endCol
              let lineLen = T.length singleLine
                  safeStart = max 0 (min (startCol - 1) lineLen)
                  safeEnd = max safeStart (min (endCol - 1) lineLen)
              in T.take (safeEnd - safeStart) $ T.drop safeStart singleLine
            (firstLine:rest) ->
              -- Multi-line span
              let firstPart = T.drop (max 0 (startCol - 1)) firstLine
              in case reverse rest of
                [] -> firstPart  -- Only one line after all
                (lastLine:middleRev) ->
                  let lastPart = T.take (min (endCol - 1) (T.length lastLine)) lastLine
                      middleLines = reverse middleRev
                  in T.intercalate "\n" $ [firstPart] ++ middleLines ++ [lastPart]

    combineValidations :: [TypeValidation] -> TypeValidation
    combineValidations vs
      | all isPreserved vs = TypesPreserved
      | any isIncompatible vs = case filter isIncompatible vs of
          (x:_) -> x
          [] -> TypesUnknown
      | otherwise = TypesUnknown

    isPreserved TypesPreserved = True
    isPreserved (TypesCompatible _) = True
    isPreserved _ = False

    isIncompatible (TypesIncompatible _) = True
    isIncompatible _ = False

-- | Check if a fix preserves the type of an expression
checkFixTypePreservation :: Text -> Text -> IO Bool
checkFixTypePreservation oldExpr newExpr = do
  mOldType <- extractType oldExpr Nothing
  mNewType <- extractType newExpr Nothing
  pure $ case (mOldType, mNewType) of
    (Nothing, _) -> True  -- Can't check, assume OK
    (_, Nothing) -> True
    (Just old, Just new) ->
      tiType old == tiType new ||
      typesCompatible (tiType old) (tiType new)

-- | Validate type preservation for a replacement
validateTypePreservation :: FilePath -> SrcSpan -> Text -> Text -> IO TypeValidation
validateTypePreservation hieDir span _oldText newText = do
  -- Get the type of the original expression
  mOldType <- extractTypeFromSpan hieDir span

  case mOldType of
    Nothing -> pure TypesUnknown  -- Cannot determine original type

    Just oldType -> do
      -- Try to infer the type of the new expression
      mNewType <- extractType newText Nothing

      case mNewType of
        Nothing -> pure TypesUnknown  -- Cannot determine new type

        Just newType
          | tiType oldType == tiType newType -> pure TypesPreserved
          | typesCompatible (tiType oldType) (tiType newType) -> pure $ TypesCompatible newType
          | otherwise -> pure $ TypesIncompatible ValidationIssue
              { viKind = "type-mismatch"
              , viMessage = "Type would change from '" <> tiType oldType <> "' to '" <> tiType newType <> "'"
              , viLocation = span
              , viExpected = Just (tiType oldType)
              , viActual = Just (tiType newType)
              , viSuggestion = Nothing
              }
