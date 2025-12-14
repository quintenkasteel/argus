{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.LSP.Navigation
-- Description : LSP navigation features - go-to-definition, find-references, document symbols
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides comprehensive navigation features for the Argus LSP server:
--
-- * Go-to-definition with cross-module support
-- * Find all references (local and cross-module)
-- * Document outline/symbols
-- * Workspace symbol search
-- * Call hierarchy (incoming/outgoing)
-- * Type hierarchy
--
-- == Usage
--
-- @
-- engine <- newNavigationEngine config
-- defLoc <- getDefinition engine filePath position content
-- refs <- findReferences engine filePath position content True
-- @
module Argus.LSP.Navigation
  ( -- * Navigation Engine
    NavigationEngine (..)
  , newNavigationEngine
  , NavigationConfig (..)
  , defaultNavigationConfig

    -- * Go-to-Definition
  , DefinitionResult (..)
  , getDefinition
  , getDefinitions
  , getTypeDefinition

    -- * Find References
  , ReferenceResult (..)
  , findReferences
  , findAllReferences

    -- * Document Symbols
  , DocumentSymbol (..)
  , SymbolDetail (..)
  , getDocumentSymbols
  , getDocumentOutline

    -- * Workspace Symbols
  , WorkspaceSymbol (..)
  , searchWorkspaceSymbols

    -- * Call Hierarchy
  , CallHierarchyItem (..)
  , CallHierarchyCall (..)
  , prepareCallHierarchy
  , getIncomingCalls
  , getOutgoingCalls

    -- * Type Hierarchy
  , TypeHierarchyItem (..)
  , prepareTypeHierarchy
  , getSupertypes
  , getSubtypes

    -- * Conversion
  , definitionToLsp
  , referencesToLsp
  , symbolsToLsp
  ) where

import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Char (isUpper, isLower, isAlphaNum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Argus.Types
  ( SrcSpan(..)
  , Line(..)
  , Column(..)
  , SymbolKind(..)
  )
import Argus.HIE.Types (HieSymbol(..), SymbolReference(..), ReferenceKind(..))
import Argus.HIE.Query
  ( withHieQuery
  , findSymbolDefinition
  , findSymbolReferences
  , findAllSymbols
  )

--------------------------------------------------------------------------------
-- Navigation Configuration
--------------------------------------------------------------------------------

-- | Navigation engine configuration
data NavigationConfig = NavigationConfig
  { ncHieDbPath         :: FilePath   -- ^ Path to HIE database
  , ncFollowImports     :: Bool       -- ^ Follow imports for definitions
  , ncIncludeDeclaration :: Bool      -- ^ Include declaration in references
  , ncMaxWorkspaceSymbols :: Int      -- ^ Max symbols in workspace search
  , ncCacheTimeout      :: Int        -- ^ Cache timeout in seconds
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default navigation configuration
defaultNavigationConfig :: NavigationConfig
defaultNavigationConfig = NavigationConfig
  { ncHieDbPath          = ".hie/.hiedb"
  , ncFollowImports      = True
  , ncIncludeDeclaration = True
  , ncMaxWorkspaceSymbols = 100
  , ncCacheTimeout       = 300
  }

--------------------------------------------------------------------------------
-- Navigation Engine
--------------------------------------------------------------------------------

-- | Navigation engine with caching
data NavigationEngine = NavigationEngine
  { neConfig        :: NavigationConfig
  , neDefCache      :: TVar (Map (FilePath, Int, Int) [DefinitionResult])
  , neRefCache      :: TVar (Map (Text, Maybe Text) [ReferenceResult])
  , neSymbolCache   :: TVar (Map FilePath [DocumentSymbol])
  }

-- | Create a new navigation engine
newNavigationEngine :: NavigationConfig -> IO NavigationEngine
newNavigationEngine config = do
  defCache <- newTVarIO Map.empty
  refCache <- newTVarIO Map.empty
  symCache <- newTVarIO Map.empty
  pure NavigationEngine
    { neConfig      = config
    , neDefCache    = defCache
    , neRefCache    = refCache
    , neSymbolCache = symCache
    }

--------------------------------------------------------------------------------
-- Definition Types
--------------------------------------------------------------------------------

-- | Result of go-to-definition
data DefinitionResult = DefinitionResult
  { drLocation    :: SrcSpan          -- ^ Location of definition
  , drSymbolName  :: Text             -- ^ Name of the symbol
  , drSymbolKind  :: SymbolKind       -- ^ Kind of symbol
  , drModule      :: Maybe Text       -- ^ Module where defined
  , drOriginKind  :: DefinitionOrigin -- ^ How we found it
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | How the definition was found
data DefinitionOrigin
  = OriginHIE           -- ^ Found via HIE database
  | OriginLocal         -- ^ Found in current file
  | OriginImport        -- ^ Found via import statement
  | OriginInferred      -- ^ Inferred from context
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Go-to-Definition
--------------------------------------------------------------------------------

-- | Get definition location for symbol at position
getDefinition :: NavigationEngine
              -> FilePath
              -> (Int, Int)       -- ^ (line, column) 1-indexed
              -> Text             -- ^ File content
              -> IO (Maybe DefinitionResult)
getDefinition engine path (line, col) content = do
  defs <- getDefinitions engine path (line, col) content
  pure $ listToMaybe defs

-- | Get all possible definitions (for symbols with multiple definitions)
getDefinitions :: NavigationEngine
               -> FilePath
               -> (Int, Int)
               -> Text
               -> IO [DefinitionResult]
getDefinitions engine path (line, col) content = do
  let config = neConfig engine
      dbPath = ncHieDbPath config

  -- Check cache first
  cached <- atomically $ Map.lookup (path, line, col) <$> readTVar (neDefCache engine)
  case cached of
    Just results -> pure results
    Nothing -> do
      results <- computeDefinitions config dbPath path line col content
      atomically $ modifyTVar' (neDefCache engine) $ Map.insert (path, line, col) results
      pure results

-- | Compute definitions (not cached)
computeDefinitions :: NavigationConfig
                   -> FilePath
                   -> FilePath
                   -> Int
                   -> Int
                   -> Text
                   -> IO [DefinitionResult]
computeDefinitions config dbPath path line col content = do
  let maybeSymbol = extractSymbolAtPos content line col

  case maybeSymbol of
    Nothing -> pure []
    Just symbolName -> do
      -- Try HIE database first
      hieResult <- try @SomeException $ withHieQuery dbPath $ do
        mSpan <- findSymbolDefinition symbolName Nothing
        case mSpan of
          Nothing -> pure []
          Just span' -> pure [DefinitionResult
            { drLocation   = span'
            , drSymbolName = symbolName
            , drSymbolKind = detectKind symbolName
            , drModule     = Just $ T.pack $ srcSpanFile span'
            , drOriginKind = OriginHIE
            }]

      case hieResult of
        Right defs | not (null defs) -> pure defs
        _ -> do
          -- Fallback: search in local file
          let localDefs = findLocalDefinition content symbolName
          if not (null localDefs)
            then pure $ map (\(l, c) -> DefinitionResult
              { drLocation   = mkSpan path l c (l + 1) c
              , drSymbolName = symbolName
              , drSymbolKind = detectKind symbolName
              , drModule     = Nothing
              , drOriginKind = OriginLocal
              }) localDefs
            else do
              -- Try to find in imports
              if ncFollowImports config
                then findDefinitionInImports content symbolName
                else pure []

-- | Get type definition (for variables, get the type's definition)
getTypeDefinition :: NavigationEngine
                  -> FilePath
                  -> (Int, Int)
                  -> Text
                  -> IO (Maybe DefinitionResult)
getTypeDefinition engine path (line, col) content = do
  -- First get the symbol
  let maybeSymbol = extractSymbolAtPos content line col
  case maybeSymbol of
    Nothing -> pure Nothing
    Just _symbolName -> do
      -- For now, same as regular definition
      -- In a full implementation, would look up the type and find its definition
      getDefinition engine path (line, col) content

-- | Find local definition in current file
findLocalDefinition :: Text -> Text -> [(Int, Int)]
findLocalDefinition content symbolName =
  let allLines = zip [1..] (T.lines content)
      matches = mapMaybe (findDefInLine symbolName) allLines
  in matches

-- | Find definition in a single line
findDefInLine :: Text -> (Int, Text) -> Maybe (Int, Int)
findDefInLine symbolName (lineNum, lineText) =
  let stripped = T.stripStart lineText
      indent = T.length lineText - T.length stripped
  in
    -- Check for type signature
    if symbolName `T.isPrefixOf` stripped && " :: " `T.isInfixOf` lineText
    then Just (lineNum, indent + 1)
    -- Check for function definition
    else if symbolName `T.isPrefixOf` stripped &&
            ((" = " `T.isInfixOf` lineText) || (" " `T.isPrefixOf` T.drop (T.length symbolName) stripped))
    then Just (lineNum, indent + 1)
    -- Check for data/type declarations
    else if ("data " <> symbolName) `T.isInfixOf` lineText ||
            ("newtype " <> symbolName) `T.isInfixOf` lineText ||
            ("type " <> symbolName) `T.isInfixOf` lineText ||
            ("class " <> symbolName) `T.isInfixOf` lineText
    then Just (lineNum, indent + 1)
    else Nothing

-- | Find definition via imports
findDefinitionInImports :: Text -> Text -> IO [DefinitionResult]
findDefinitionInImports content symbolName = do
  -- Parse imports to find which module might export this symbol
  let imports = parseImports content
      qualifiedPrefix = case T.breakOn "." symbolName of
        (prefix, rest) | not (T.null rest) -> Just prefix
        _ -> Nothing

  case qualifiedPrefix of
    Just qual -> do
      -- Find module for this qualified prefix
      case lookup qual imports of
        Just moduleName -> pure [DefinitionResult
          { drLocation   = mkSpan (T.unpack moduleName <> ".hs") 1 1 1 1
          , drSymbolName = symbolName
          , drSymbolKind = Function
          , drModule     = Just moduleName
          , drOriginKind = OriginImport
          }]
        Nothing -> pure []
    Nothing -> pure []

-- | Parse import statements
parseImports :: Text -> [(Text, Text)]  -- ^ [(qualifier/module, module)]
parseImports content =
  let importLines = filter (T.isPrefixOf "import" . T.stripStart) (T.lines content)
  in mapMaybe parseImportLine importLines

-- | Parse a single import line
parseImportLine :: Text -> Maybe (Text, Text)
parseImportLine line =
  let words' = T.words line
  in case words' of
       ("import":"qualified":moduleName:rest) ->
         case findAs rest of
           Just alias -> Just (alias, moduleName)
           Nothing -> Just (moduleName, moduleName)
       ("import":moduleName:rest') | "qualified" `elem` rest' ->
         case findAs rest' of
           Just alias -> Just (alias, moduleName)
           Nothing -> Just (moduleName, moduleName)
       ("import":moduleName:_) ->
         Just (last $ T.splitOn "." moduleName, moduleName)
       _ -> Nothing
  where
    findAs ws = case dropWhile (/= "as") ws of
      ("as":alias:_) -> Just alias
      _ -> Nothing

--------------------------------------------------------------------------------
-- Find References
--------------------------------------------------------------------------------

-- | Reference result
data ReferenceResult = ReferenceResult
  { rrLocation :: SrcSpan
  , rrKind     :: ReferenceKind   -- ^ Imported from Argus.HIE.Types
  , rrContext  :: Text            -- ^ Line of code containing reference
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Find all references to symbol at position
findReferences :: NavigationEngine
               -> FilePath
               -> (Int, Int)
               -> Text
               -> Bool           -- ^ Include declaration?
               -> IO [ReferenceResult]
findReferences engine path (line, col) content includeDecl = do
  let maybeSymbol = extractSymbolAtPos content line col
  case maybeSymbol of
    Nothing -> pure []
    Just symbolName -> findAllReferences engine symbolName (Just $ T.pack path) includeDecl content

-- | Find all references to a symbol by name
findAllReferences :: NavigationEngine
                  -> Text         -- ^ Symbol name
                  -> Maybe Text   -- ^ Optional module filter
                  -> Bool         -- ^ Include declaration?
                  -> Text         -- ^ Current file content (for local fallback)
                  -> IO [ReferenceResult]
findAllReferences engine symbolName moduleFilter includeDecl content = do
  let config = neConfig engine
      dbPath = ncHieDbPath config

  -- Check cache
  cached <- atomically $ Map.lookup (symbolName, moduleFilter) <$> readTVar (neRefCache engine)
  case cached of
    Just results -> pure $ filterResults includeDecl results
    Nothing -> do
      -- Query HIE database
      hieResult <- try @SomeException $ withHieQuery dbPath $ do
        refs <- findSymbolReferences symbolName moduleFilter
        pure $ map symbolRefToResult refs

      results <- case hieResult of
        Right refs | not (null refs) -> pure refs
        _ -> do
          -- Fallback: find in current content
          pure $ findLocalReferences content symbolName

      -- Cache results
      atomically $ modifyTVar' (neRefCache engine) $ Map.insert (symbolName, moduleFilter) results
      pure $ filterResults includeDecl results
  where
    filterResults incl refs
      | incl = refs
      | otherwise = filter ((/= RefDefinition) . rrKind) refs

-- | Convert SymbolReference to ReferenceResult
symbolRefToResult :: SymbolReference -> ReferenceResult
symbolRefToResult SymbolReference{..} = ReferenceResult
  { rrLocation = srSpan
  , rrKind     = srKind
  , rrContext  = T.empty    -- Context must be fetched separately if needed
  }

-- | Find references in local content
findLocalReferences :: Text -> Text -> [ReferenceResult]
findLocalReferences content symbolName =
  let allLines = zip [1..] (T.lines content)
      refs = concatMap (findRefsInLine symbolName) allLines
  in refs

-- | Find references in a single line
findRefsInLine :: Text -> (Int, Text) -> [ReferenceResult]
findRefsInLine symbolName (lineNum, lineText) =
  let occurrences = findOccurrences symbolName lineText
      isDefLine = isDefinitionLine symbolName lineText
  in map (\col -> ReferenceResult
    { rrLocation = mkSpan "" lineNum col lineNum (col + T.length symbolName)
    , rrKind     = if isDefLine && col < 30 then RefDefinition else RefUsage
    , rrContext  = lineText
    }) occurrences

-- | Find all occurrences of symbol in line
findOccurrences :: Text -> Text -> [Int]
findOccurrences needle haystack = go 1 haystack
  where
    go col text
      | T.null text = []
      | needle `T.isPrefixOf` text =
          let afterMatch = T.drop (T.length needle) text
              validEnd = T.null afterMatch || not (isIdentChar $ T.head afterMatch)
              beforeValid = col == 1 || not (isIdentChar $ T.index haystack (col - 2))
          in if validEnd && beforeValid
             then col : go (col + T.length needle) afterMatch
             else go (col + 1) (T.tail text)
      | otherwise = go (col + 1) (T.tail text)

-- | Check if line is a definition of the symbol
isDefinitionLine :: Text -> Text -> Bool
isDefinitionLine symbolName line =
  let stripped = T.stripStart line
  in symbolName `T.isPrefixOf` stripped &&
     (" :: " `T.isInfixOf` line || " = " `T.isInfixOf` line)

--------------------------------------------------------------------------------
-- Document Symbols
--------------------------------------------------------------------------------

-- | Document symbol for outline view
data DocumentSymbol = DocumentSymbol
  { dsName           :: Text
  , dsDetail         :: Maybe Text
  , dsKind           :: SymbolKind
  , dsRange          :: SrcSpan         -- ^ Full range of symbol
  , dsSelectionRange :: SrcSpan         -- ^ Range to highlight
  , dsChildren       :: [DocumentSymbol] -- ^ Nested symbols
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Symbol detail information
data SymbolDetail = SymbolDetail
  { sdSignature :: Maybe Text
  , sdModule    :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Get document symbols for outline
getDocumentSymbols :: NavigationEngine
                   -> FilePath
                   -> Text
                   -> IO [DocumentSymbol]
getDocumentSymbols engine path content = do
  -- Check cache
  cached <- atomically $ Map.lookup path <$> readTVar (neSymbolCache engine)
  case cached of
    Just symbols -> pure symbols
    Nothing -> do
      let symbols = parseDocumentSymbols path content
      atomically $ modifyTVar' (neSymbolCache engine) $ Map.insert path symbols
      pure symbols

-- | Get document outline (flat list)
getDocumentOutline :: NavigationEngine
                   -> FilePath
                   -> Text
                   -> IO [DocumentSymbol]
getDocumentOutline = getDocumentSymbols

-- | Parse document symbols from content
parseDocumentSymbols :: FilePath -> Text -> [DocumentSymbol]
parseDocumentSymbols path content =
  let allLines = zip [1..] (T.lines content)
      topLevel = mapMaybe (parseSymbolAtLine path) allLines
  in topLevel

-- | Parse symbol at a specific line
parseSymbolAtLine :: FilePath -> (Int, Text) -> Maybe DocumentSymbol
parseSymbolAtLine path (lineNum, lineText) =
  let stripped = T.stripStart lineText
      indent = T.length lineText - T.length stripped
  in
    -- Only process top-level (no indent) or data constructor level
    if indent > 4
    then Nothing
    else parseTopLevelSymbol path lineNum lineText stripped

-- | Parse top-level symbol
parseTopLevelSymbol :: FilePath -> Int -> Text -> Text -> Maybe DocumentSymbol
parseTopLevelSymbol path lineNum fullLine stripped
  -- Module declaration
  | "module " `T.isPrefixOf` stripped =
      let moduleName = T.takeWhile (\c -> isAlphaNum c || c == '.') $ T.drop 7 stripped
      in Just $ mkSymbol moduleName Module lineNum fullLine path Nothing

  -- Data type
  | "data " `T.isPrefixOf` stripped =
      let name = extractTypeName $ T.drop 5 stripped
      in Just $ mkSymbol name DataConstructor lineNum fullLine path (Just "data")

  -- Newtype
  | "newtype " `T.isPrefixOf` stripped =
      let name = extractTypeName $ T.drop 8 stripped
      in Just $ mkSymbol name DataConstructor lineNum fullLine path (Just "newtype")

  -- Type synonym
  | "type " `T.isPrefixOf` stripped =
      let name = extractTypeName $ T.drop 5 stripped
      in Just $ mkSymbol name TypeConstructor lineNum fullLine path (Just "type")

  -- Type class
  | "class " `T.isPrefixOf` stripped =
      let name = extractClassName $ T.drop 6 stripped
      in Just $ mkSymbol name TypeClass lineNum fullLine path (Just "class")

  -- Instance
  | "instance " `T.isPrefixOf` stripped =
      let instanceDesc = T.takeWhile (/= 'w') $ T.drop 9 stripped
      in Just $ mkSymbol ("instance " <> T.strip instanceDesc) TypeClassMethod lineNum fullLine path Nothing

  -- Type signature (name :: Type)
  | " :: " `T.isInfixOf` fullLine && not (T.any (== '=') stripped) =
      let name = T.strip $ fst $ T.breakOn "::" stripped
      in if isValidFunctionName name
         then Just $ mkSymbol name Function lineNum fullLine path (Just $ T.strip $ snd $ T.breakOn "::" fullLine)
         else Nothing

  -- Function definition (name args = ...)
  | otherwise =
      case T.words stripped of
        (name:_) | isValidFunctionName name && " = " `T.isInfixOf` fullLine ->
          Just $ mkSymbol name Function lineNum fullLine path Nothing
        _ -> Nothing

-- | Create a document symbol
mkSymbol :: Text -> SymbolKind -> Int -> Text -> FilePath -> Maybe Text -> DocumentSymbol
mkSymbol name kind lineNum _lineText path detail = DocumentSymbol
  { dsName           = name
  , dsDetail         = detail
  , dsKind           = kind
  , dsRange          = mkSpan path lineNum 1 (lineNum + 1) 1
  , dsSelectionRange = mkSpan path lineNum 1 lineNum (T.length name + 1)
  , dsChildren       = []
  }

-- | Extract type name from declaration
extractTypeName :: Text -> Text
extractTypeName text =
  let firstWord = T.takeWhile isIdentChar $ T.stripStart text
  in if T.null firstWord then "(unknown)" else firstWord

-- | Extract class name from declaration
extractClassName :: Text -> Text
extractClassName text =
  -- Skip constraints: class (Eq a, Ord a) => ClassName ...
  let afterConstraint = case T.breakOn "=>" text of
        (_, rest) | not (T.null rest) -> T.drop 2 rest
        _ -> text
      stripped = T.stripStart afterConstraint
  in T.takeWhile isIdentChar stripped

-- | Check if name is a valid function name
isValidFunctionName :: Text -> Bool
isValidFunctionName name =
  not (T.null name) &&
  isLower (T.head name) &&
  T.all (\c -> isAlphaNum c || c == '_' || c == '\'') name &&
  name `notElem` keywords
  where
    keywords = ["module", "import", "data", "type", "newtype", "class", "instance",
                "where", "let", "in", "do", "case", "of", "if", "then", "else",
                "deriving", "forall"]

--------------------------------------------------------------------------------
-- Workspace Symbols
--------------------------------------------------------------------------------

-- | Workspace symbol (for Ctrl+T / Cmd+T)
data WorkspaceSymbol = WorkspaceSymbol
  { wsName      :: Text
  , wsKind      :: SymbolKind
  , wsLocation  :: SrcSpan
  , wsContainer :: Maybe Text   -- ^ Container (module, class, etc.)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Search workspace symbols
searchWorkspaceSymbols :: NavigationEngine
                       -> Text         -- ^ Query
                       -> IO [WorkspaceSymbol]
searchWorkspaceSymbols engine query = do
  let config = neConfig engine
      dbPath = ncHieDbPath config
      maxResults = ncMaxWorkspaceSymbols config

  -- Query HIE database
  result <- try @SomeException $ withHieQuery dbPath $ do
    symbols <- findAllSymbols query
    pure $ take maxResults $ map hieSymbolToWorkspace symbols

  case result of
    Left _ -> pure []
    Right symbols -> pure symbols

-- | Convert HieSymbol to WorkspaceSymbol
hieSymbolToWorkspace :: HieSymbol -> WorkspaceSymbol
hieSymbolToWorkspace HieSymbol{..} = WorkspaceSymbol
  { wsName      = hsName
  , wsKind      = hsKind
  , wsLocation  = fromMaybe defaultSpan hsDefinition
  , wsContainer = Just hsModule
  }
  where
    -- Default span when no definition is available
    defaultSpan = SrcSpan
      { srcSpanFile      = T.unpack hsModule
      , srcSpanStartLine = Line 1
      , srcSpanStartCol  = Column 1
      , srcSpanEndLine   = Line 1
      , srcSpanEndCol    = Column 1
      }

--------------------------------------------------------------------------------
-- Call Hierarchy
--------------------------------------------------------------------------------

-- | Call hierarchy item
data CallHierarchyItem = CallHierarchyItem
  { chiName       :: Text
  , chiKind       :: SymbolKind
  , chiTags       :: [Text]       -- ^ e.g., "deprecated"
  , chiDetail     :: Maybe Text
  , chiUri        :: Text
  , chiRange      :: SrcSpan
  , chiSelectionRange :: SrcSpan
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A call in the hierarchy
data CallHierarchyCall = CallHierarchyCall
  { chcFrom   :: CallHierarchyItem   -- ^ The calling function
  , chcTo     :: CallHierarchyItem   -- ^ The called function
  , chcRanges :: [SrcSpan]           -- ^ Where the calls happen
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Prepare call hierarchy at position
prepareCallHierarchy :: NavigationEngine
                     -> FilePath
                     -> (Int, Int)
                     -> Text
                     -> IO (Maybe CallHierarchyItem)
prepareCallHierarchy _engine path (line, col) content = do
  let maybeSymbol = extractSymbolAtPos content line col
  case maybeSymbol of
    Nothing -> pure Nothing
    Just symbolName -> do
      -- Find the definition to create the item
      let localDefs = findLocalDefinition content symbolName
      case localDefs of
        ((defLine, defCol):_) ->
          pure $ Just CallHierarchyItem
            { chiName          = symbolName
            , chiKind          = Function
            , chiTags          = []
            , chiDetail        = Nothing
            , chiUri           = T.pack $ "file://" ++ path
            , chiRange         = mkSpan path defLine defCol (defLine + 10) 1
            , chiSelectionRange = mkSpan path defLine defCol defLine (defCol + T.length symbolName)
            }
        [] ->
          -- No local definition, create item at cursor position
          pure $ Just CallHierarchyItem
            { chiName          = symbolName
            , chiKind          = Function
            , chiTags          = []
            , chiDetail        = Nothing
            , chiUri           = T.pack $ "file://" ++ path
            , chiRange         = mkSpan path line col (line + 1) 1
            , chiSelectionRange = mkSpan path line col line (col + T.length symbolName)
            }

-- | Get incoming calls (callers)
getIncomingCalls :: NavigationEngine
                 -> CallHierarchyItem
                 -> IO [CallHierarchyCall]
getIncomingCalls engine item = do
  -- Find all references to this function
  refs <- findAllReferences engine (chiName item) Nothing False ""
  let calls = mapMaybe (refToIncomingCall item) refs
  pure calls

-- | Convert reference to incoming call
refToIncomingCall :: CallHierarchyItem -> ReferenceResult -> Maybe CallHierarchyCall
refToIncomingCall callee ReferenceResult{..} =
  case rrKind of
    RefUsage -> Just CallHierarchyCall
      { chcFrom   = CallHierarchyItem
          { chiName          = extractCallerName rrContext
          , chiKind          = Function
          , chiTags          = []
          , chiDetail        = Nothing
          , chiUri           = T.pack $ "file://" ++ srcSpanFile rrLocation
          , chiRange         = rrLocation
          , chiSelectionRange = rrLocation
          }
      , chcTo     = callee
      , chcRanges = [rrLocation]
      }
    _ -> Nothing

-- | Extract caller name from context
extractCallerName :: Text -> Text
extractCallerName context =
  let stripped = T.stripStart context
      firstWord = T.takeWhile isIdentChar stripped
  in if isValidFunctionName firstWord then firstWord else "(anonymous)"

-- | Get outgoing calls (callees)
getOutgoingCalls :: NavigationEngine
                 -> CallHierarchyItem
                 -> IO [CallHierarchyCall]
getOutgoingCalls _engine _item = do
  -- Would need to parse the function body to find all function calls
  -- For now, return empty list
  pure []

--------------------------------------------------------------------------------
-- Type Hierarchy
--------------------------------------------------------------------------------

-- | Type hierarchy item
data TypeHierarchyItem = TypeHierarchyItem
  { thiName           :: Text
  , thiKind           :: SymbolKind
  , thiTags           :: [Text]
  , thiDetail         :: Maybe Text
  , thiUri            :: Text
  , thiRange          :: SrcSpan
  , thiSelectionRange :: SrcSpan
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Prepare type hierarchy at position
prepareTypeHierarchy :: NavigationEngine
                     -> FilePath
                     -> (Int, Int)
                     -> Text
                     -> IO (Maybe TypeHierarchyItem)
prepareTypeHierarchy _engine path (line, col) content = do
  let maybeSymbol = extractSymbolAtPos content line col
  case maybeSymbol of
    Nothing -> pure Nothing
    Just symbolName ->
      if isUpper (T.head symbolName)
      then pure $ Just TypeHierarchyItem
        { thiName           = symbolName
        , thiKind           = TypeConstructor
        , thiTags           = []
        , thiDetail         = Nothing
        , thiUri            = T.pack $ "file://" ++ path
        , thiRange          = mkSpan path line col (line + 1) 1
        , thiSelectionRange = mkSpan path line col line (col + T.length symbolName)
        }
      else pure Nothing

-- | Get supertypes (for classes: superclasses; for types: nothing)
getSupertypes :: NavigationEngine -> TypeHierarchyItem -> IO [TypeHierarchyItem]
getSupertypes _engine _item = do
  -- Would need class hierarchy analysis
  pure []

-- | Get subtypes (for classes: instances/subclasses; for types: nothing)
getSubtypes :: NavigationEngine -> TypeHierarchyItem -> IO [TypeHierarchyItem]
getSubtypes _engine _item = do
  -- Would need class hierarchy analysis
  pure []

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- | Extract symbol at position
extractSymbolAtPos :: Text -> Int -> Int -> Maybe Text
extractSymbolAtPos content line col =
  let allLines = T.lines content
  in if line > 0 && line <= length allLines
     then
       let lineText = allLines !! (line - 1)
           col' = col - 1
       in extractWordAt lineText col'
     else Nothing

-- | Extract word at column
extractWordAt :: Text -> Int -> Maybe Text
extractWordAt lineText col
  | col < 0 || col >= T.length lineText = Nothing
  | otherwise =
      let before = T.take col lineText
          after = T.drop col lineText
          wordStart = T.takeWhileEnd isIdentChar before
          wordEnd = T.takeWhile isIdentChar after
          word = wordStart <> wordEnd
      in if T.null word then Nothing else Just word

-- | Check if character is identifier character
isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

-- | Create a SrcSpan
mkSpan :: FilePath -> Int -> Int -> Int -> Int -> SrcSpan
mkSpan file startLine startCol endLine endCol = SrcSpan
  { srcSpanFile     = file
  , srcSpanStartLine = Line startLine
  , srcSpanStartCol  = Column startCol
  , srcSpanEndLine   = Line endLine
  , srcSpanEndCol    = Column endCol
  }

-- | Detect symbol kind from name
detectKind :: Text -> SymbolKind
detectKind name
  | T.null name = Function
  | isUpper (T.head name) = TypeConstructor
  | otherwise = Function

--------------------------------------------------------------------------------
-- LSP Conversion
--------------------------------------------------------------------------------

-- | Convert definition result to LSP location
definitionToLsp :: DefinitionResult -> Aeson.Value
definitionToLsp DefinitionResult{..} = object
  [ "uri" .= ("file://" <> T.pack (srcSpanFile drLocation))
  , "range" .= spanToLspRange drLocation
  ]

-- | Convert references to LSP locations
referencesToLsp :: [ReferenceResult] -> [Aeson.Value]
referencesToLsp = map referenceToLsp

-- | Convert single reference to LSP
referenceToLsp :: ReferenceResult -> Aeson.Value
referenceToLsp ReferenceResult{..} = object
  [ "uri" .= ("file://" <> T.pack (srcSpanFile rrLocation))
  , "range" .= spanToLspRange rrLocation
  ]

-- | Convert document symbols to LSP
symbolsToLsp :: [DocumentSymbol] -> [Aeson.Value]
symbolsToLsp = map symbolToLsp

-- | Convert single symbol to LSP
symbolToLsp :: DocumentSymbol -> Aeson.Value
symbolToLsp DocumentSymbol{..} = object $ catMaybes
  [ Just $ "name" .= dsName
  , ("detail" .=) <$> dsDetail
  , Just $ "kind" .= symbolKindToInt dsKind
  , Just $ "range" .= spanToLspRange dsRange
  , Just $ "selectionRange" .= spanToLspRange dsSelectionRange
  , if null dsChildren then Nothing else Just $ "children" .= symbolsToLsp dsChildren
  ]

-- | Convert symbol kind to LSP integer
symbolKindToInt :: SymbolKind -> Int
symbolKindToInt = \case
  Function        -> 12  -- Function
  TypeConstructor -> 5   -- Class
  DataConstructor -> 9   -- Constructor
  TypeClass       -> 11  -- Interface
  TypeClassMethod -> 6   -- Method
  TypeFamily      -> 11  -- Interface
  PatternSynonym  -> 22  -- EnumMember
  Module          -> 2   -- Module

-- | Convert SrcSpan to LSP range
spanToLspRange :: SrcSpan -> Aeson.Value
spanToLspRange SrcSpan{..} = object
  [ "start" .= object
      [ "line" .= (unLine srcSpanStartLine - 1)
      , "character" .= (unColumn srcSpanStartCol - 1)
      ]
  , "end" .= object
      [ "line" .= (unLine srcSpanEndLine - 1)
      , "character" .= (unColumn srcSpanEndCol - 1)
      ]
  ]
