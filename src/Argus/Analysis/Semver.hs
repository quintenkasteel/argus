{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- |
-- Module      : Argus.Analysis.Semver
-- Description : Semantic versioning detection for Haskell APIs
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides functionality for detecting breaking API changes
-- between versions by comparing exported module signatures. It extracts
-- API information from HIE files and determines the appropriate semantic
-- version bump (major, minor, patch) based on the changes detected.
--
-- == Breaking Changes Detected
--
-- * Removed exported functions, types, or type classes
-- * Changed type signatures (incompatible types)
-- * Removed constructors from exported types
-- * Changed data constructor arity
-- * Removed type class methods
-- * Added required constraints to existing functions
-- * Changed function parameter counts
--
-- == Non-Breaking Changes Detected
--
-- * Added new exports
-- * Added new constructors (if type is not strict enumeration)
-- * Added default methods to type classes
-- * Relaxed constraints
-- * Added optional parameters with defaults
--
-- == Usage
--
-- @
-- -- Compare two versions of a module
-- oldAPI <- extractModuleAPI db "OldVersion" "Data.MyLib"
-- newAPI <- extractModuleAPI db "NewVersion" "Data.MyLib"
-- changes <- compareAPIs oldAPI newAPI
-- let bump = classifyChanges changes
-- @
module Argus.Analysis.Semver
  ( -- * API Types
    ExportedSymbol (..)
  , SymbolSignature (..)
  , APISignature (..)
  , emptyAPISignature

    -- * Change Detection
  , APIChange (..)
  , ChangeKind (..)
  , BreakingChange (..)
  , BreakingSeverity (..)
  , SemverBump (..)

    -- * API Extraction
  , extractModuleAPI
  , extractModuleAPIs
  , extractExportedTypes
  , extractExportedFunctions
  , extractExportedClasses
  , extractDataConstructors
  , extractClassMethods

    -- * API Comparison
  , compareAPIs
  , compareSymbolSignatures
  , isBreakingChange
  , classifyChange
  , classifyChanges
  , summarizeChanges

    -- * Diagnostic Generation
  , apiChangesToDiagnostics
  , breakingChangeToDiagnostic
  , suggestVersionBump
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (forM)
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- GHC API imports for HIE file reading
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
  , getAsts
  , getSourcedNodeInfo
  )
import "ghc" GHC.Types.Name (Name, nameOccName)
import "ghc" GHC.Types.Name.Occurrence (occNameString)
import "ghc" GHC.Unit.Module (moduleNameString, moduleName)

-- HieDb imports
import HieDb (HieDb)
import HieDb.Utils (makeNc)

import Argus.Types
  ( Diagnostic(..)
  , DiagnosticKind(..)
  , Severity(..)
  , noSrcSpan
  )
import Argus.Analysis.Semantic
  ( getExports
  , getModuleInfo
  , ModuleInfo(..)
  )

--------------------------------------------------------------------------------
-- API Types
--------------------------------------------------------------------------------

-- | A symbol exported by a module
data ExportedSymbol = ExportedSymbol
  { esName       :: Text              -- ^ Symbol name
  , esModule     :: Text              -- ^ Module it's exported from
  , esSignature  :: SymbolSignature   -- ^ Signature details
  , esDocumentation :: Maybe Text     -- ^ Haddock documentation if available
  }
  deriving stock (Eq, Show)

-- | Signature of an exported symbol
data SymbolSignature
  = FunctionSig
      { fsType         :: Text        -- ^ Function type signature
      , fsConstraints  :: [Text]      -- ^ Type constraints
      , fsParamCount   :: Int         -- ^ Number of parameters
      , fsReturnType   :: Text        -- ^ Return type
      }
  | TypeSig
      { tsKind         :: Text        -- ^ Type kind
      , tsConstructors :: [Text]      -- ^ Data constructors
      , tsFields       :: [Text]      -- ^ Record fields if applicable
      , tsIsNewtype    :: Bool        -- ^ Is this a newtype?
      }
  | ClassSig
      { csMethods      :: [(Text, Text)] -- ^ Method names and types
      , csConstraints  :: [Text]         -- ^ Superclass constraints
      , csAssocTypes   :: [Text]         -- ^ Associated types
      }
  | PatternSig
      { psType         :: Text        -- ^ Pattern synonym type
      }
  | TypeFamilySig
      { tfsKind        :: Text        -- ^ Type family kind
      , tfsInjective   :: Bool        -- ^ Is it injective?
      }
  deriving stock (Eq, Show)

-- | Complete API signature of a module
data APISignature = APISignature
  { apiModule      :: Text                       -- ^ Module name
  , apiVersion     :: Maybe Text                 -- ^ Version if known
  , apiFunctions   :: Map Text SymbolSignature   -- ^ Exported functions
  , apiTypes       :: Map Text SymbolSignature   -- ^ Exported types
  , apiClasses     :: Map Text SymbolSignature   -- ^ Exported type classes
  , apiPatterns    :: Map Text SymbolSignature   -- ^ Exported pattern synonyms
  , apiTypeFamilies :: Map Text SymbolSignature  -- ^ Exported type families
  , apiReexports   :: Set Text                   -- ^ Re-exported modules
  }
  deriving stock (Eq, Show)

-- | Empty API signature
emptyAPISignature :: Text -> APISignature
emptyAPISignature modName = APISignature
  { apiModule = modName
  , apiVersion = Nothing
  , apiFunctions = Map.empty
  , apiTypes = Map.empty
  , apiClasses = Map.empty
  , apiPatterns = Map.empty
  , apiTypeFamilies = Map.empty
  , apiReexports = Set.empty
  }

--------------------------------------------------------------------------------
-- Change Detection Types
--------------------------------------------------------------------------------

-- | A change in the API between versions
data APIChange = APIChange
  { acSymbol       :: Text            -- ^ Symbol that changed
  , acKind         :: ChangeKind      -- ^ Kind of change
  , acOldSignature :: Maybe SymbolSignature -- ^ Old signature if exists
  , acNewSignature :: Maybe SymbolSignature -- ^ New signature if exists
  , acBreaking     :: Maybe BreakingChange  -- ^ Breaking change details if applicable
  }
  deriving stock (Eq, Show)

-- | Kind of API change
data ChangeKind
  = SymbolAdded          -- ^ New symbol added
  | SymbolRemoved        -- ^ Symbol removed (breaking)
  | SignatureChanged     -- ^ Signature modified
  | ConstraintAdded      -- ^ New constraint added (breaking)
  | ConstraintRemoved    -- ^ Constraint removed (non-breaking)
  | ConstructorAdded     -- ^ Data constructor added
  | ConstructorRemoved   -- ^ Data constructor removed (breaking)
  | MethodAdded          -- ^ Class method added
  | MethodRemoved        -- ^ Class method removed (breaking)
  | ParameterCountChanged -- ^ Function parameter count changed (breaking)
  | ReturnTypeChanged    -- ^ Return type changed (breaking)
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Details about a breaking change
data BreakingChange = BreakingChange
  { bcReason       :: Text            -- ^ Why this is breaking
  , bcMitigation   :: Maybe Text      -- ^ How to fix consuming code
  , bcSeverity     :: BreakingSeverity -- ^ How severe is the break
  }
  deriving stock (Eq, Show)

-- | Severity of a breaking change
data BreakingSeverity
  = BreakingMinor    -- ^ Minor breakage, easy to fix
  | BreakingMajor    -- ^ Major breakage, significant refactoring needed
  | BreakingCritical -- ^ Critical breakage, complete API redesign
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Semantic version bump recommendation
data SemverBump
  = PatchBump    -- ^ Only bug fixes, no API changes
  | MinorBump    -- ^ New features added, backwards compatible
  | MajorBump    -- ^ Breaking changes present
  deriving stock (Eq, Ord, Show, Enum, Bounded)

--------------------------------------------------------------------------------
-- API Extraction
--------------------------------------------------------------------------------

-- | Extract complete API signature from a module using HieDb
extractModuleAPI :: HieDb -> Text -> IO APISignature
extractModuleAPI db modName = do
  -- Get exports from the database
  exports <- getExports db modName

  -- Initialize empty API
  let api = emptyAPISignature modName

  -- Get module info to find HIE file
  mModInfo <- getModuleInfoSafe db modName

  case mModInfo of
    Nothing -> pure api  -- Module not found, return empty
    Just modInfo -> do
      -- Read HIE file and extract detailed signatures
      let hiePath = moduleInfoHieFile modInfo
      result <- try @SomeException $ extractFromHieFile hiePath exports

      case result of
        Left _ -> pure api  -- Failed to read HIE, return exports only
        Right sigs -> pure api
          { apiFunctions = Map.filterWithKey (\k _ -> k `elem` exports) (apiFunctions sigs)
          , apiTypes = Map.filterWithKey (\k _ -> k `elem` exports) (apiTypes sigs)
          , apiClasses = Map.filterWithKey (\k _ -> k `elem` exports) (apiClasses sigs)
          , apiPatterns = Map.filterWithKey (\k _ -> k `elem` exports) (apiPatterns sigs)
          , apiTypeFamilies = Map.filterWithKey (\k _ -> k `elem` exports) (apiTypeFamilies sigs)
          }

-- | Safe version of getModuleInfo that doesn't throw
getModuleInfoSafe :: HieDb -> Text -> IO (Maybe ModuleInfo)
getModuleInfoSafe db modName = do
  result <- try @SomeException $ getModuleInfo db modName
  pure $ case result of
    Left _ -> Nothing
    Right Nothing -> Nothing
    Right (Just info) -> Just info

-- | Extract signatures from a HIE file
extractFromHieFile :: FilePath -> [Text] -> IO APISignature
extractFromHieFile hiePath _exports = do
  nc <- makeNc
  hieResult <- readHieFile nc hiePath
  let hieFile = hie_file_result hieResult
      modName = T.pack $ moduleNameString $ moduleName $ hie_module hieFile

  -- Extract all definitions from the HIE AST
  let asts = getAsts $ hie_asts hieFile
      allDefs = concatMap (extractDefinitions hieFile) (Map.elems asts)

  -- Categorize definitions
  let functions = Map.fromList [(name, sig) | (name, sig@FunctionSig{}) <- allDefs]
      types = Map.fromList [(name, sig) | (name, sig@TypeSig{}) <- allDefs]
      classes = Map.fromList [(name, sig) | (name, sig@ClassSig{}) <- allDefs]
      patterns = Map.fromList [(name, sig) | (name, sig@PatternSig{}) <- allDefs]
      typeFams = Map.fromList [(name, sig) | (name, sig@TypeFamilySig{}) <- allDefs]

  pure $ APISignature
    { apiModule = modName
    , apiVersion = Nothing
    , apiFunctions = functions
    , apiTypes = types
    , apiClasses = classes
    , apiPatterns = patterns
    , apiTypeFamilies = typeFams
    , apiReexports = Set.empty
    }

-- | Extract definitions from a HIE AST
extractDefinitions :: HieFile -> HieAST a -> [(Text, SymbolSignature)]
extractDefinitions hieFile ast =
  let nodeInf = getNodeInfoFromAST ast
      identifiers = Map.toList $ nodeIdentifiers nodeInf
      defs = mapMaybe (extractDefFromIdent hieFile) identifiers
      childDefs = concatMap (extractDefinitions hieFile) (nodeChildren ast)
  in defs ++ childDefs

-- | Extract definition from an identifier
extractDefFromIdent :: HieFile -> (Either a Name, IdentifierDetails b) -> Maybe (Text, SymbolSignature)
extractDefFromIdent _hieFile (Right name, details) =
  let symName = T.pack $ occNameString $ nameOccName name
      contexts = Set.toList $ identInfo details
  in case classifyContext contexts of
       Just sig -> Just (symName, sig)
       Nothing -> Nothing
extractDefFromIdent _ _ = Nothing

-- | Classify a symbol based on its context information
classifyContext :: [ContextInfo] -> Maybe SymbolSignature
classifyContext contexts
  | any isValBind contexts = Just $ FunctionSig "" [] 0 ""
  | any isTypeDec contexts = Just $ TypeSig "*" [] [] False
  | any isClassDec contexts = Just $ ClassSig [] [] []
  | any isPatSyn contexts = Just $ PatternSig ""
  | otherwise = Nothing
  where
    isValBind ValBind{} = True
    isValBind _ = False

    isTypeDec (Decl DataDec _) = True
    isTypeDec (Decl SynDec _) = True
    isTypeDec TyDecl = True
    isTypeDec _ = False

    isClassDec ClassTyDecl{} = True
    isClassDec _ = False

    isPatSyn (Decl PatSynDec _) = True
    isPatSyn _ = False

-- | Get node info from AST
getNodeInfoFromAST :: HieAST a -> NodeInfo a
getNodeInfoFromAST ast =
  let snInfo = sourcedNodeInfo ast
      nodeInfoMap = getSourcedNodeInfo snInfo
  in case Map.lookup SourceInfo nodeInfoMap of
       Just ni -> ni
       Nothing -> case Map.elems nodeInfoMap of
                    (ni:_) -> ni
                    [] -> NodeInfo mempty mempty mempty

-- | Extract multiple module APIs efficiently
extractModuleAPIs :: HieDb -> [Text] -> IO (Map Text APISignature)
extractModuleAPIs db modNames = do
  apis <- forM modNames $ \modName -> do
    api <- extractModuleAPI db modName
    pure (modName, api)
  pure $ Map.fromList apis

-- | Extract exported type definitions
extractExportedTypes :: APISignature -> [(Text, SymbolSignature)]
extractExportedTypes api = Map.toList (apiTypes api)

-- | Extract exported function signatures
extractExportedFunctions :: APISignature -> [(Text, SymbolSignature)]
extractExportedFunctions api = Map.toList (apiFunctions api)

-- | Extract exported type classes
extractExportedClasses :: APISignature -> [(Text, SymbolSignature)]
extractExportedClasses api = Map.toList (apiClasses api)

-- | Extract data constructors from a type signature
extractDataConstructors :: SymbolSignature -> [Text]
extractDataConstructors TypeSig{..} = tsConstructors
extractDataConstructors _ = []

-- | Extract class methods from a class signature
extractClassMethods :: SymbolSignature -> [(Text, Text)]
extractClassMethods ClassSig{..} = csMethods
extractClassMethods _ = []

--------------------------------------------------------------------------------
-- API Comparison
--------------------------------------------------------------------------------

-- | Compare two API signatures and detect changes
compareAPIs :: APISignature -> APISignature -> [APIChange]
compareAPIs oldAPI newAPI =
  let functionChanges = compareSymbolMaps (apiFunctions oldAPI) (apiFunctions newAPI)
      typeChanges = compareSymbolMaps (apiTypes oldAPI) (apiTypes newAPI)
      classChanges = compareSymbolMaps (apiClasses oldAPI) (apiClasses newAPI)
      patternChanges = compareSymbolMaps (apiPatterns oldAPI) (apiPatterns newAPI)
      typeFamChanges = compareSymbolMaps (apiTypeFamilies oldAPI) (apiTypeFamilies newAPI)
  in concat [functionChanges, typeChanges, classChanges, patternChanges, typeFamChanges]

-- | Compare two maps of symbols
compareSymbolMaps :: Map Text SymbolSignature -> Map Text SymbolSignature -> [APIChange]
compareSymbolMaps oldMap newMap =
  let oldKeys = Map.keysSet oldMap
      newKeys = Map.keysSet newMap

      removed = Set.difference oldKeys newKeys
      added = Set.difference newKeys oldKeys
      common = Set.intersection oldKeys newKeys

      removedChanges = map mkRemoved (Set.toList removed)
      addedChanges = map mkAdded (Set.toList added)
      modifiedChanges = mapMaybe mkModified (Set.toList common)

  in removedChanges ++ addedChanges ++ modifiedChanges
  where
    mkRemoved sym = APIChange
      { acSymbol = sym
      , acKind = SymbolRemoved
      , acOldSignature = Map.lookup sym oldMap
      , acNewSignature = Nothing
      , acBreaking = Just $ BreakingChange
          { bcReason = "Symbol '" <> sym <> "' was removed from the public API"
          , bcMitigation = Just "Update consuming code to not use this symbol"
          , bcSeverity = BreakingMajor
          }
      }

    mkAdded sym = APIChange
      { acSymbol = sym
      , acKind = SymbolAdded
      , acOldSignature = Nothing
      , acNewSignature = Map.lookup sym newMap
      , acBreaking = Nothing
      }

    mkModified sym =
      case (Map.lookup sym oldMap, Map.lookup sym newMap) of
        (Just oldSig, Just newSig) ->
          if oldSig == newSig
            then Nothing  -- No actual change
            else Just $ compareSymbolSignatures sym oldSig newSig
        _ -> Nothing

-- | Compare two symbol signatures
compareSymbolSignatures :: Text -> SymbolSignature -> SymbolSignature -> APIChange
compareSymbolSignatures sym oldSig newSig =
  case (oldSig, newSig) of
    (FunctionSig{..}, FunctionSig{fsType = newType, fsConstraints = newConstraints, fsParamCount = newParams}) ->
      let changes = detectFunctionChanges fsType newType fsConstraints newConstraints fsParamCount newParams
      in APIChange sym (fst changes) (Just oldSig) (Just newSig) (snd changes)

    (TypeSig{..}, TypeSig{tsConstructors = newCtors}) ->
      let changes = detectTypeChanges tsConstructors newCtors
      in APIChange sym (fst changes) (Just oldSig) (Just newSig) (snd changes)

    (ClassSig{..}, ClassSig{csMethods = newMethods}) ->
      let changes = detectClassChanges csMethods newMethods
      in APIChange sym (fst changes) (Just oldSig) (Just newSig) (snd changes)

    _ ->
      -- Different signature types - this is breaking
      APIChange sym SignatureChanged (Just oldSig) (Just newSig) (Just $ BreakingChange
        { bcReason = "Symbol kind changed from " <> showSigType oldSig <> " to " <> showSigType newSig
        , bcMitigation = Just "Update consuming code to match new signature type"
        , bcSeverity = BreakingCritical
        })

-- | Detect changes in function signatures
detectFunctionChanges :: Text -> Text -> [Text] -> [Text] -> Int -> Int -> (ChangeKind, Maybe BreakingChange)
detectFunctionChanges oldType newType oldConstraints newConstraints oldParams newParams
  | oldParams /= newParams =
      (ParameterCountChanged, Just $ BreakingChange
        { bcReason = "Parameter count changed from " <> T.pack (show oldParams) <> " to " <> T.pack (show newParams)
        , bcMitigation = Just "Update call sites to match new parameter count"
        , bcSeverity = BreakingMajor
        })
  | oldType /= newType =
      (SignatureChanged, Just $ BreakingChange
        { bcReason = "Type signature changed"
        , bcMitigation = Just "Verify consuming code still type-checks"
        , bcSeverity = BreakingMajor
        })
  | hasNewConstraints oldConstraints newConstraints =
      (ConstraintAdded, Just $ BreakingChange
        { bcReason = "New type constraints added: " <> T.intercalate ", " (newConstraints `setDiff` oldConstraints)
        , bcMitigation = Just "Ensure call sites satisfy new constraints"
        , bcSeverity = BreakingMinor
        })
  | otherwise =
      (ConstraintRemoved, Nothing)  -- Relaxing constraints is non-breaking

-- | Detect changes in type definitions
detectTypeChanges :: [Text] -> [Text] -> (ChangeKind, Maybe BreakingChange)
detectTypeChanges oldCtors newCtors
  | oldCtors == newCtors = (SignatureChanged, Nothing)  -- No actual change
  | length newCtors < length oldCtors =
      (ConstructorRemoved, Just $ BreakingChange
        { bcReason = "Data constructors removed: " <> T.intercalate ", " (oldCtors `setDiff` newCtors)
        , bcMitigation = Just "Update pattern matches to handle remaining constructors"
        , bcSeverity = BreakingMajor
        })
  | otherwise =
      (ConstructorAdded, Nothing)  -- Adding constructors is generally non-breaking

-- | Detect changes in class definitions
detectClassChanges :: [(Text, Text)] -> [(Text, Text)] -> (ChangeKind, Maybe BreakingChange)
detectClassChanges oldMethods newMethods
  | oldMethods == newMethods = (SignatureChanged, Nothing)
  | length newMethods < length oldMethods =
      let removedMethods = map fst oldMethods `setDiff` map fst newMethods
      in (MethodRemoved, Just $ BreakingChange
        { bcReason = "Class methods removed: " <> T.intercalate ", " removedMethods
        , bcMitigation = Just "Update instances to remove these methods"
        , bcSeverity = BreakingMajor
        })
  | otherwise =
      (MethodAdded, Nothing)  -- Adding methods with defaults is non-breaking

-- | Helper to compute set difference for lists
setDiff :: Eq a => [a] -> [a] -> [a]
setDiff xs ys = filter (`notElem` ys) xs

-- | Check if new constraints were added
hasNewConstraints :: [Text] -> [Text] -> Bool
hasNewConstraints oldCs newCs = not $ all (`elem` oldCs) newCs

-- | Show signature type name
showSigType :: SymbolSignature -> Text
showSigType FunctionSig{} = "function"
showSigType TypeSig{} = "type"
showSigType ClassSig{} = "class"
showSigType PatternSig{} = "pattern"
showSigType TypeFamilySig{} = "type family"

-- | Check if an API change is breaking
isBreakingChange :: APIChange -> Bool
isBreakingChange APIChange{acBreaking = Just _} = True
isBreakingChange _ = False

-- | Classify a single change into a semver bump
classifyChange :: APIChange -> SemverBump
classifyChange change
  | isBreakingChange change = MajorBump
  | acKind change `elem` [SymbolAdded, ConstraintRemoved, ConstructorAdded, MethodAdded] = MinorBump
  | otherwise = PatchBump

-- | Classify all changes and return the highest bump needed
classifyChanges :: [APIChange] -> SemverBump
classifyChanges [] = PatchBump
classifyChanges changes =
  let bumps = map classifyChange changes
  in maximum bumps

-- | Summarize changes into categories
summarizeChanges :: [APIChange] -> (Int, Int, Int)
summarizeChanges changes =
  let (breaking, nonBreaking) = partition isBreakingChange changes
      (additions, _) = partition (\c -> acKind c == SymbolAdded) nonBreaking
  in (length breaking, length additions, length nonBreaking - length additions)

--------------------------------------------------------------------------------
-- Diagnostic Generation
--------------------------------------------------------------------------------

-- | Convert API changes to diagnostics
apiChangesToDiagnostics :: Text -> [APIChange] -> [Diagnostic]
apiChangesToDiagnostics modName changes =
  let breakingChanges = filter isBreakingChange changes
      additions = filter (\c -> acKind c == SymbolAdded) changes

      breakingDiags = map (breakingChangeToDiagnostic modName) breakingChanges
      additionDiags = map (additionToDiagnostic modName) additions

  in breakingDiags ++ additionDiags

-- | Convert a breaking change to a diagnostic
breakingChangeToDiagnostic :: Text -> APIChange -> Diagnostic
breakingChangeToDiagnostic modName change =
  let (severity, message) = case acBreaking change of
        Just bc -> case bcSeverity bc of
          BreakingCritical -> (Error, bcReason bc)
          BreakingMajor -> (Error, bcReason bc)
          BreakingMinor -> (Warning, bcReason bc)
        Nothing -> (Warning, "API changed")

      code = case acKind change of
        SymbolRemoved -> "semver/removed-symbol"
        ParameterCountChanged -> "semver/parameter-count-changed"
        SignatureChanged -> "semver/signature-changed"
        ConstructorRemoved -> "semver/constructor-removed"
        MethodRemoved -> "semver/method-removed"
        ConstraintAdded -> "semver/constraint-added"
        ReturnTypeChanged -> "semver/return-type-changed"
        _ -> "semver/breaking-change"

  in Diagnostic
    { diagSpan = noSrcSpan
    , diagSeverity = severity
    , diagKind = ArchitecturalIssue
    , diagMessage = "Breaking API change in " <> modName <> ": " <> message
    , diagCode = Just code
    , diagFixes = []
    , diagRelated = []
    }

-- | Convert an addition to an informational diagnostic
additionToDiagnostic :: Text -> APIChange -> Diagnostic
additionToDiagnostic modName change =
  Diagnostic
    { diagSpan = noSrcSpan
    , diagSeverity = Info
    , diagKind = ArchitecturalIssue
    , diagMessage = "New symbol added to " <> modName <> ": " <> acSymbol change
    , diagCode = Just "semver/symbol-added"
    , diagFixes = []
    , diagRelated = []
    }

-- | Suggest appropriate version bump based on changes
suggestVersionBump :: [APIChange] -> Text
suggestVersionBump changes =
  case classifyChanges changes of
    PatchBump -> "PATCH (0.0.x) - Only internal changes, no API impact"
    MinorBump -> "MINOR (0.x.0) - New features added, backwards compatible"
    MajorBump ->
      let (breaking, additions, _) = summarizeChanges changes
      in "MAJOR (x.0.0) - Breaking changes detected:\n  " <>
         T.pack (show breaking) <> " breaking change(s), " <>
         T.pack (show additions) <> " addition(s)"
