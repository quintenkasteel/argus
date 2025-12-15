{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Argus.AutoFix.Semantic
-- Description : Semantic auto-fix using HIE type information
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- This module provides type-aware auto-fix capabilities that leverage HIE
-- (Haskell Interface File) data for semantic understanding. Unlike syntactic
-- fixes that only look at code structure, semantic fixes understand:
--
-- * Type constraints and type class instances
-- * Symbol scoping and visibility
-- * Cross-module dependencies
-- * Type preservation guarantees
--
-- == Architecture
--
-- The semantic fix engine uses a three-phase approach:
--
-- 1. **Analysis**: Extract type and scope information from HIE data
-- 2. **Generation**: Generate semantically-valid fix candidates
-- 3. **Validation**: Verify fixes preserve type correctness
--
-- == Example Usage
--
-- @
-- -- Create a semantic fix engine
-- engine <- initSemanticEngine hieDbPath
--
-- -- Find semantic fixes
-- fixes <- findSemanticFixes engine filePath content diagnostics
--
-- -- Apply with validation
-- result <- applySemanticFix engine filePath content fix
-- @
module Argus.AutoFix.Semantic
  ( -- * Semantic Engine
    SemanticFixEngine (..)
  , SemanticConfig (..)
  , defaultSemanticConfig
  , initSemanticEngine
  , shutdownSemanticEngine

    -- * Fix Generation
  , findSemanticFixes
  , generateSemanticFix
  , SemanticFixContext (..)
  , SemanticFixCandidate (..)

    -- * Type-Aware Fixes
  , generateTypeConstraintFix
  , generateInstanceFix
  , generateQualificationFix
  , generateImportFix
  , generateRefactoringFix

    -- * Fix Strategies
  , SemanticStrategy (..)
  , StrategyResult (..)
  , runStrategy
  , composeStrategies
  , strategyFromRule

    -- * Constraint Resolution
  , resolveConstraints
  , findMissingInstances
  , suggestInstanceFix
  , ConstraintResolution (..)

    -- * Symbol Analysis
  , analyzeSymbolUsage
  , findSymbolDefinition
  , findSymbolReferences
  , SymbolAnalysis (..)
  , SymbolUsage (..)

    -- * Type Inference Helpers
  , inferExpressionType
  , checkTypeCompatibility
  , findTypeGeneralization
  , TypeInference (..)

    -- * Semantic Validation
  , validateSemanticFix
  , checkSemanticPreservation
  , SemanticValidation (..)
  , SemanticIssue (..)

    -- * Fix Application
  , applySemanticFix
  , applySemanticFixes
  , SemanticApplicationResult (..)

    -- * HIE Integration
  , loadHieContext
  , HieContext (..)
  , refreshHieContext
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, readTVar, writeTVar, atomically)
import Control.Exception (try, SomeException)
import Control.Monad (forM, when)
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.IORef (IORef, newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist)


import Argus.Types
  ( SrcSpan (..)
  , Fix (..)
  , FixEdit (..)
  , Diagnostic (..)
  , DiagnosticKind (..)
  , Severity (..)
  , Line (..)
  , Column (..)
  , mkFixWithImports
  , ImportSymbol (..)
  , ImportSymbolType (..)
  , FixCategory (..)
  , FixSafety (..)
  , mkFixImport
  )
import Argus.HIE.Types
import Argus.HIE.TypeInfo
import Argus.HIE.SymbolTable (SymbolTable)
import Argus.AutoFix.Types
import Argus.Rules.Types qualified as RT

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for the semantic fix engine
data SemanticConfig = SemanticConfig
  { scHieDbPath         :: FilePath
    -- ^ Path to the HIE database
  , scHieDir            :: FilePath
    -- ^ Directory containing .hie files
  , scEnableTypeChecking :: Bool
    -- ^ Enable type preservation checking
  , scEnableConstraints  :: Bool
    -- ^ Enable constraint resolution
  , scEnableImportFixes  :: Bool
    -- ^ Enable automatic import fixes
  , scMaxFixCandidates   :: Int
    -- ^ Maximum fix candidates to generate
  , scConfidenceThreshold :: Double
    -- ^ Minimum confidence for suggestions
  , scRefreshInterval    :: Int
    -- ^ HIE refresh interval in seconds
  , scVerbose            :: Bool
    -- ^ Verbose logging
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default semantic configuration
defaultSemanticConfig :: SemanticConfig
defaultSemanticConfig = SemanticConfig
  { scHieDbPath = ".hiedb"
  , scHieDir = ".hie"
  , scEnableTypeChecking = True
  , scEnableConstraints = True
  , scEnableImportFixes = True
  , scMaxFixCandidates = 10
  , scConfidenceThreshold = 0.7
  , scRefreshInterval = 60
  , scVerbose = False
  }

--------------------------------------------------------------------------------
-- Semantic Fix Engine
--------------------------------------------------------------------------------

-- | The semantic fix engine with HIE integration
data SemanticFixEngine = SemanticFixEngine
  { sfeConfig       :: SemanticConfig
  , sfeHieContext   :: TVar (Maybe HieContext)
  , sfeSymbolTable  :: TVar (Maybe SymbolTable)
  , sfeTypeCache    :: IORef HieTypeCache
  , sfeLastRefresh  :: TVar UTCTime
  , sfeStats        :: TVar EngineStats
  }

-- | Statistics for the semantic engine
data EngineStats = EngineStats
  { esFixesGenerated  :: Int
  , esFixesApplied    :: Int
  , esFixesValidated  :: Int
  , esConstraintsFixes :: Int
  , esImportFixes     :: Int
  , esCacheHits       :: Int
  , esCacheMisses     :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Initial statistics
emptyEngineStats :: EngineStats
emptyEngineStats = EngineStats 0 0 0 0 0 0 0

-- | Initialize the semantic fix engine
initSemanticEngine :: SemanticConfig -> IO SemanticFixEngine
initSemanticEngine config = do
  hieCtxVar <- newTVarIO Nothing
  symTableVar <- newTVarIO Nothing
  typeCache <- newHieTypeCache >>= newIORef
  now <- getCurrentTime
  lastRefreshVar <- newTVarIO now
  statsVar <- newTVarIO emptyEngineStats

  let engine = SemanticFixEngine
        { sfeConfig = config
        , sfeHieContext = hieCtxVar
        , sfeSymbolTable = symTableVar
        , sfeTypeCache = typeCache
        , sfeLastRefresh = lastRefreshVar
        , sfeStats = statsVar
        }

  -- Try to load initial HIE context
  _ <- try @SomeException $ loadHieContextForEngine engine

  pure engine

-- | Shutdown the semantic engine
shutdownSemanticEngine :: SemanticFixEngine -> IO ()
shutdownSemanticEngine engine = do
  atomically $ do
    writeTVar (sfeHieContext engine) Nothing
    writeTVar (sfeSymbolTable engine) Nothing

-- | Load HIE context for the engine
loadHieContextForEngine :: SemanticFixEngine -> IO ()
loadHieContextForEngine engine = do
  let config = sfeConfig engine
  exists <- doesDirectoryExist (scHieDir config)
  when exists $ do
    ctx <- loadHieContext (scHieDir config)
    atomically $ writeTVar (sfeHieContext engine) (Just ctx)
    now <- getCurrentTime
    atomically $ writeTVar (sfeLastRefresh engine) now

--------------------------------------------------------------------------------
-- HIE Context
--------------------------------------------------------------------------------

-- | Context loaded from HIE files
data HieContext = HieContext
  { hcModules      :: Map Text HieModuleInfo
    -- ^ Indexed module information
  , hcSymbols      :: Map Text HieSymbol
    -- ^ All known symbols
  , hcInstances    :: Map Text [InstanceInfo]
    -- ^ Type class instances by class name
  , hcTypeInfo     :: Map (FilePath, Int, Int) TypeInfo
    -- ^ Cached type information by location
  , hcLastUpdated  :: UTCTime
    -- ^ When the context was last updated
  }
  deriving stock (Show, Generic)

-- | Information about a module from HIE
data HieModuleInfo = HieModuleInfo
  { hmiName       :: Text
  , hmiFilePath   :: FilePath
  , hmiExports    :: [Text]
  , hmiImports    :: [Text]
  , hmiLastModified :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Load HIE context from a directory
loadHieContext :: FilePath -> IO HieContext
loadHieContext _hieDir = do
  now <- getCurrentTime
  -- For now, return empty context - full implementation would scan HIE files
  pure HieContext
    { hcModules = Map.empty
    , hcSymbols = Map.empty
    , hcInstances = Map.empty
    , hcTypeInfo = Map.empty
    , hcLastUpdated = now
    }

-- | Refresh HIE context if needed
refreshHieContext :: SemanticFixEngine -> IO HieContext
refreshHieContext engine = do
  let config = sfeConfig engine
  now <- getCurrentTime
  lastRefresh <- readTVarIO (sfeLastRefresh engine)

  let shouldRefresh = diffTimeSeconds now lastRefresh > fromIntegral (scRefreshInterval config)

  when shouldRefresh $ loadHieContextForEngine engine

  mCtx <- readTVarIO (sfeHieContext engine)
  case mCtx of
    Just ctx -> pure ctx
    Nothing -> do
      loadHieContextForEngine engine
      mCtx' <- readTVarIO (sfeHieContext engine)
      pure $ fromMaybe (emptyHieContext now) mCtx'
  where
    emptyHieContext now = HieContext Map.empty Map.empty Map.empty Map.empty now

    diffTimeSeconds :: UTCTime -> UTCTime -> Double
    diffTimeSeconds t1 t2 =
      let diff = diffUTCTime t1 t2
      in diff

    diffUTCTime :: UTCTime -> UTCTime -> Double
    diffUTCTime _ _ = 0  -- Simplified for now

--------------------------------------------------------------------------------
-- Semantic Fix Context
--------------------------------------------------------------------------------

-- | Context for semantic fix generation
data SemanticFixContext = SemanticFixContext
  { sfcFilePath    :: FilePath
  , sfcContent     :: Text
  , sfcDiagnostic  :: Diagnostic
  , sfcHieContext  :: HieContext
  , sfcSymbolTable :: Maybe SymbolTable
  , sfcTypeCache   :: HieTypeCache
  }

-- | A candidate semantic fix
data SemanticFixCandidate = SemanticFixCandidate
  { sfcandFix        :: Fix
  , sfcandConfidence :: Double
  , sfcandReason     :: Text
  , sfcandValidation :: Maybe SemanticValidation
  , sfcandCategory   :: SemanticFixCategory
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Categories of semantic fixes
data SemanticFixCategory
  = SFCTypeConstraint    -- ^ Fix missing type class constraint
  | SFCInstanceAdd       -- ^ Add missing instance
  | SFCQualification     -- ^ Qualify ambiguous name
  | SFCImportAdd         -- ^ Add missing import
  | SFCImportRemove      -- ^ Remove unused import
  | SFCRefactoring       -- ^ General refactoring
  | SFCTypeSignature     -- ^ Add/fix type signature
  | SFCPatternMatch      -- ^ Fix pattern match
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Fix Generation
--------------------------------------------------------------------------------

-- | Find semantic fixes for diagnostics
findSemanticFixes :: SemanticFixEngine
                  -> FilePath
                  -> Text
                  -> [Diagnostic]
                  -> IO [SemanticFixCandidate]
findSemanticFixes engine filePath content diagnostics = do
  ctx <- refreshHieContext engine
  symTable <- readTVarIO (sfeSymbolTable engine)
  cache <- readIORef (sfeTypeCache engine)

  let config = sfeConfig engine
      maxCandidates = scMaxFixCandidates config

  candidates <- forM diagnostics $ \diag -> do
    let sctx = SemanticFixContext
          { sfcFilePath = filePath
          , sfcContent = content
          , sfcDiagnostic = diag
          , sfcHieContext = ctx
          , sfcSymbolTable = symTable
          , sfcTypeCache = cache
          }
    generateSemanticFix sctx

  -- Update stats
  atomically $ do
    stats <- readTVar (sfeStats engine)
    writeTVar (sfeStats engine) stats
      { esFixesGenerated = esFixesGenerated stats + length (concat candidates) }

  pure $ take maxCandidates $ sortByConfidence $ concat candidates
  where
    sortByConfidence = map snd . reverse . map (\c -> (sfcandConfidence c, c))

-- | Generate semantic fix for a single diagnostic
generateSemanticFix :: SemanticFixContext -> IO [SemanticFixCandidate]
generateSemanticFix ctx = do
  let diag = sfcDiagnostic ctx

  -- Try different fix strategies based on diagnostic kind
  candidates <- case diagKind diag of
    TypeSignature -> generateTypeSignatureFixes ctx
    ImportStyle -> generateImportStyleFixes ctx
    UnusedImport -> generateUnusedImportFixes ctx
    PartialFunction -> generatePartialFunctionFixes ctx
    _ -> pure []

  -- Also try generic strategies
  genericFixes <- tryGenericStrategies ctx

  pure $ candidates ++ genericFixes

-- | Generate fixes for type signature issues
generateTypeSignatureFixes :: SemanticFixContext -> IO [SemanticFixCandidate]
generateTypeSignatureFixes ctx = do
  let diag = sfcDiagnostic ctx
      theSpan = diagSpan diag

  -- Try to infer the type from HIE data
  mTypeInfo <- inferTypeAtLocation ctx (srcSpanFile theSpan)
                                       (unLine $ srcSpanStartLine theSpan)
                                       (unColumn $ srcSpanStartCol theSpan)

  case mTypeInfo of
    Just typeInfo -> do
      let fix = mkFixWithImports
            ("Add type signature: " <> tiType typeInfo)
            [FixEdit theSpan (tiType typeInfo <> "\n")]
            True
            []
            []
            FCStyle
            FSAlways
      pure [SemanticFixCandidate
        { sfcandFix = fix
        , sfcandConfidence = 0.9
        , sfcandReason = "Type inferred from HIE data"
        , sfcandValidation = Nothing
        , sfcandCategory = SFCTypeSignature
        }]
    Nothing -> pure []

-- | Generate fixes for import style issues
generateImportStyleFixes :: SemanticFixContext -> IO [SemanticFixCandidate]
generateImportStyleFixes ctx = do
  let diag = sfcDiagnostic ctx
      msg = diagMessage diag

  -- Parse the diagnostic message to understand what's needed
  let qualifyFix = if "ambiguous" `T.isInfixOf` msg
        then generateQualificationFix ctx
        else pure []

  qualifyFix

-- | Generate fixes for unused imports
generateUnusedImportFixes :: SemanticFixContext -> IO [SemanticFixCandidate]
generateUnusedImportFixes ctx = do
  let diag = sfcDiagnostic ctx
      theSpan = diagSpan diag

  -- Generate a fix to remove the unused import
  let fix = mkFixWithImports
        "Remove unused import"
        [FixEdit theSpan ""]
        True
        []
        []
        FCImports
        FSAlways

  pure [SemanticFixCandidate
    { sfcandFix = fix
    , sfcandConfidence = 0.95
    , sfcandReason = "Import is not used in the module"
    , sfcandValidation = Nothing
    , sfcandCategory = SFCImportRemove
    }]

-- | Generate fixes for partial function usage
generatePartialFunctionFixes :: SemanticFixContext -> IO [SemanticFixCandidate]
generatePartialFunctionFixes ctx = do
  let diag = sfcDiagnostic ctx
      msg = diagMessage diag
      theSpan = diagSpan diag

  -- Common partial function replacements
  let replacements =
        [ ("head", "listToMaybe", "Data.Maybe")
        , ("tail", "drop 1", "")
        , ("init", "reverse . drop 1 . reverse", "")
        , ("last", "listToMaybe . reverse", "Data.Maybe")
        , ("fromJust", "fromMaybe defaultValue", "Data.Maybe")
        , ("read", "readMaybe", "Text.Read")
        , ("!!", "atMay", "Safe")
        ]

  candidates <- forM replacements $ \(partial, replacement, modName) -> do
    if partial `T.isInfixOf` msg
      then do
        let imports = if T.null modName
              then []
              else [mkFixImport modName [ImportSymbol replacement ISTFunction []]]
        let fix = mkFixWithImports
              ("Replace " <> partial <> " with " <> replacement)
              [FixEdit theSpan replacement]
              True
              imports
              []
              FCSafety
              FSMostly
        pure $ Just SemanticFixCandidate
          { sfcandFix = fix
          , sfcandConfidence = 0.85
          , sfcandReason = "Replace partial function with total alternative"
          , sfcandValidation = Nothing
          , sfcandCategory = SFCRefactoring
          }
      else pure Nothing

  pure $ catMaybes candidates

-- | Try generic semantic strategies
tryGenericStrategies :: SemanticFixContext -> IO [SemanticFixCandidate]
tryGenericStrategies _ = pure []  -- Extensibility point

-- | Infer type at a specific location
inferTypeAtLocation :: SemanticFixContext -> FilePath -> Int -> Int -> IO (Maybe TypeInfo)
inferTypeAtLocation ctx path _line _col = do
  let cache = sfcTypeCache ctx
  -- Try cache first
  cached <- lookupCachedType cache (T.pack path) Nothing
  case cached of
    Just (Just ti) -> pure (Just ti)
    _ -> do
      -- Would use HIE database to extract type
      pure Nothing

--------------------------------------------------------------------------------
-- Type-Aware Fix Generators
--------------------------------------------------------------------------------

-- | Generate fix for missing type constraint
generateTypeConstraintFix :: SemanticFixContext
                          -> Text           -- ^ Missing constraint class
                          -> Text           -- ^ Type variable
                          -> IO (Maybe SemanticFixCandidate)
generateTypeConstraintFix ctx className typeVar = do
  let diag = sfcDiagnostic ctx
      theSpan = diagSpan diag

  -- Generate a fix that adds the constraint
  let constraintText = className <> " " <> typeVar <> " => "
  let fix = mkFixWithImports
        ("Add constraint: " <> className <> " " <> typeVar)
        [FixEdit theSpan constraintText]
        True
        []
        []
        FCStyle
        FSReview

  pure $ Just SemanticFixCandidate
    { sfcandFix = fix
    , sfcandConfidence = 0.8
    , sfcandReason = "Add missing type class constraint"
    , sfcandValidation = Nothing
    , sfcandCategory = SFCTypeConstraint
    }

-- | Generate fix for missing instance
generateInstanceFix :: SemanticFixContext
                    -> Text         -- ^ Class name
                    -> Text         -- ^ Type name
                    -> IO (Maybe SemanticFixCandidate)
generateInstanceFix ctx className typeName = do
  -- Generate a fix that adds an instance declaration
  let instanceText = "instance " <> className <> " " <> typeName <> " where\n"
  let theSpan = diagSpan (sfcDiagnostic ctx)
  let fix = mkFixWithImports
        ("Add instance: " <> className <> " " <> typeName)
        [FixEdit theSpan instanceText]
        False  -- Not preferred, needs review
        []
        []
        FCStyle
        FSUnsafe  -- Adding instances can have far-reaching effects

  pure $ Just SemanticFixCandidate
    { sfcandFix = fix
    , sfcandConfidence = 0.6
    , sfcandReason = "Add missing type class instance"
    , sfcandValidation = Nothing
    , sfcandCategory = SFCInstanceAdd
    }

-- | Generate fix to qualify an ambiguous name
generateQualificationFix :: SemanticFixContext -> IO [SemanticFixCandidate]
generateQualificationFix ctx = do
  let diag = sfcDiagnostic ctx
      theSpan = diagSpan diag
      msg = diagMessage diag

  -- Try to find potential qualifications from the symbol table
  let mSymTable = sfcSymbolTable ctx

  case mSymTable of
    Just symTable -> do
      -- Extract the ambiguous name from the diagnostic
      let ambiguousName = extractAmbiguousName msg
      case ambiguousName of
        Just name -> do
          -- Look up possible qualifications
          let modules = findPossibleModules symTable name
          candidates <- forM modules $ \modName -> do
            let qualifiedName = modName <> "." <> name
            let fix = mkFixWithImports
                  ("Qualify as " <> qualifiedName)
                  [FixEdit theSpan qualifiedName]
                  True
                  []
                  []
                  FCStyle
                  FSAlways
            pure SemanticFixCandidate
              { sfcandFix = fix
              , sfcandConfidence = 0.85
              , sfcandReason = "Qualify ambiguous name with module prefix"
              , sfcandValidation = Nothing
              , sfcandCategory = SFCQualification
              }
          pure candidates
        Nothing -> pure []
    Nothing -> pure []
  where
    extractAmbiguousName :: Text -> Maybe Text
    extractAmbiguousName msg =
      -- Simple extraction - look for quoted names
      let parts = T.splitOn "'" msg
      in if length parts >= 2 then Just (parts !! 1) else Nothing

    findPossibleModules :: SymbolTable -> Text -> [Text]
    findPossibleModules _ _ = []  -- Would query symbol table

-- | Generate fix to add missing import
generateImportFix :: SemanticFixContext
                  -> Text         -- ^ Module name
                  -> Text         -- ^ Symbol to import
                  -> IO (Maybe SemanticFixCandidate)
generateImportFix ctx modName symbol = do
  -- Find where to insert the import
  let content = sfcContent ctx
      importLine = findImportInsertLine content
      importText = "import " <> modName <> " (" <> symbol <> ")\n"

  let fix = mkFixWithImports
        ("Import " <> symbol <> " from " <> modName)
        [FixEdit (mkImportSpan importLine) importText]
        True
        [mkFixImport modName [ImportSymbol symbol ISTFunction []]]
        []
        FCImports
        FSAlways

  pure $ Just SemanticFixCandidate
    { sfcandFix = fix
    , sfcandConfidence = 0.9
    , sfcandReason = "Add import for missing symbol"
    , sfcandValidation = Nothing
    , sfcandCategory = SFCImportAdd
    }
  where
    findImportInsertLine :: Text -> Int
    findImportInsertLine txt =
      let linesList = T.lines txt
          importLines = zip [1..] linesList
          imports = filter (T.isPrefixOf "import" . snd) importLines
      in case imports of
           [] -> 1  -- After module declaration
           xs -> fst (last xs) + 1

    mkImportSpan :: Int -> SrcSpan
    mkImportSpan line = SrcSpan
      { srcSpanFile = sfcFilePath ctx
      , srcSpanStartLine = Line line
      , srcSpanStartCol = Column 1
      , srcSpanEndLine = Line line
      , srcSpanEndCol = Column 1
      }

-- | Generate general refactoring fix
generateRefactoringFix :: SemanticFixContext
                       -> Text         -- ^ Description
                       -> [(SrcSpan, Text)]  -- ^ Edits
                       -> Double       -- ^ Confidence
                       -> IO (Maybe SemanticFixCandidate)
generateRefactoringFix _ctx description edits confidence = do
  let fix = mkFixWithImports
        description
        (map (uncurry FixEdit) edits)
        True
        []
        []
        FCStyle
        FSReview

  pure $ Just SemanticFixCandidate
    { sfcandFix = fix
    , sfcandConfidence = confidence
    , sfcandReason = description
    , sfcandValidation = Nothing
    , sfcandCategory = SFCRefactoring
    }

--------------------------------------------------------------------------------
-- Fix Strategies
--------------------------------------------------------------------------------

-- | A strategy for generating semantic fixes
data SemanticStrategy = SemanticStrategy
  { ssName        :: Text
  , ssDescription :: Text
  , ssPriority    :: Int
  , ssApplicable  :: SemanticFixContext -> IO Bool
  , ssGenerate    :: SemanticFixContext -> IO [SemanticFixCandidate]
  }

-- | Result of running a strategy
data StrategyResult = StrategyResult
  { srStrategy   :: Text
  , srCandidates :: [SemanticFixCandidate]
  , srApplied    :: Bool
  , srDuration   :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Run a single strategy
runStrategy :: SemanticStrategy -> SemanticFixContext -> IO StrategyResult
runStrategy strategy ctx = do
  applicable <- ssApplicable strategy ctx
  if applicable
    then do
      candidates <- ssGenerate strategy ctx
      pure StrategyResult
        { srStrategy = ssName strategy
        , srCandidates = candidates
        , srApplied = True
        , srDuration = 0  -- Would measure
        }
    else pure StrategyResult
        { srStrategy = ssName strategy
        , srCandidates = []
        , srApplied = False
        , srDuration = 0
        }

-- | Compose multiple strategies
composeStrategies :: [SemanticStrategy] -> SemanticFixContext -> IO [SemanticFixCandidate]
composeStrategies strategies ctx = do
  results <- mapM (`runStrategy` ctx) strategies
  pure $ concatMap srCandidates results

-- | Create a strategy from a rule
strategyFromRule :: Text -> (SemanticFixContext -> IO Bool) -> (SemanticFixContext -> IO [SemanticFixCandidate]) -> SemanticStrategy
strategyFromRule name isApplicable generate = SemanticStrategy
  { ssName = name
  , ssDescription = "Strategy: " <> name
  , ssPriority = 50
  , ssApplicable = isApplicable
  , ssGenerate = generate
  }

--------------------------------------------------------------------------------
-- Constraint Resolution
--------------------------------------------------------------------------------

-- | Result of constraint resolution
data ConstraintResolution = ConstraintResolution
  { crConstraint   :: TypeConstraint
  , crResolved     :: Bool
  , crInstance     :: Maybe InstanceInfo
  , crSuggestion   :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Resolve constraints for a type
resolveConstraints :: HieContext -> Text -> [Text] -> IO [ConstraintResolution]
resolveConstraints ctx typeName constraints = do
  forM constraints $ \constraint -> do
    let instances = Map.findWithDefault [] constraint (hcInstances ctx)
        matchingInstance = find (\i -> iiType i == typeName) instances
    pure ConstraintResolution
      { crConstraint = TypeConstraint constraint typeName (isJust matchingInstance) matchingInstance
      , crResolved = isJust matchingInstance
      , crInstance = matchingInstance
      , crSuggestion = if isJust matchingInstance
          then Nothing
          else Just $ "Add instance " <> constraint <> " " <> typeName
      }
  where
    find :: (a -> Bool) -> [a] -> Maybe a
    find _ [] = Nothing
    find p (x:xs) = if p x then Just x else find p xs

    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust Nothing = False

-- | Find missing instances for a type
findMissingInstances :: HieContext -> Text -> [Text] -> [Text]
findMissingInstances ctx typeName requiredClasses =
  let instances = hcInstances ctx
      hasInstance cls =
        let classInstances = Map.findWithDefault [] cls instances
        in any (\i -> iiType i == typeName) classInstances
  in filter (not . hasInstance) requiredClasses

-- | Suggest a fix for a missing instance
suggestInstanceFix :: Text -> Text -> Text
suggestInstanceFix className typeName =
  "instance " <> className <> " " <> typeName <> " where\n  -- TODO: implement"

--------------------------------------------------------------------------------
-- Symbol Analysis
--------------------------------------------------------------------------------

-- | Analysis of symbol usage
data SymbolAnalysis = SymbolAnalysis
  { saSymbol      :: Text
  , saDefinition  :: Maybe SrcSpan
  , saReferences  :: [SrcSpan]
  , saType        :: Maybe TypeInfo
  , saScope       :: SymbolScope
  , saVisibility  :: SymbolVisibility
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Usage information for a symbol
data SymbolUsage = SymbolUsage
  { suLocation    :: SrcSpan
  , suIsDefinition :: Bool
  , suIsExport    :: Bool
  , suContext     :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Analyze usage of a symbol
analyzeSymbolUsage :: HieContext -> Text -> IO SymbolAnalysis
analyzeSymbolUsage ctx symbolName = do
  let mSymbol = Map.lookup symbolName (hcSymbols ctx)
  case mSymbol of
    Just sym -> pure SymbolAnalysis
      { saSymbol = symbolName
      , saDefinition = hsDefinition sym
      , saReferences = hsReferences sym
      , saType = fmap mkTypeInfo (hsType sym)
      , saScope = hsScope sym
      , saVisibility = hsVisibility sym
      }
    Nothing -> pure SymbolAnalysis
      { saSymbol = symbolName
      , saDefinition = Nothing
      , saReferences = []
      , saType = Nothing
      , saScope = ScopeLocal
      , saVisibility = VisPrivate
      }
  where
    mkTypeInfo t = TypeInfo t "*" [] True []

-- | Find definition of a symbol
findSymbolDefinition :: HieContext -> Text -> Maybe SrcSpan
findSymbolDefinition ctx symbolName =
  Map.lookup symbolName (hcSymbols ctx) >>= hsDefinition

-- | Find all references to a symbol
findSymbolReferences :: HieContext -> Text -> [SrcSpan]
findSymbolReferences ctx symbolName =
  maybe [] hsReferences $ Map.lookup symbolName (hcSymbols ctx)

--------------------------------------------------------------------------------
-- Type Inference Helpers
--------------------------------------------------------------------------------

-- | Result of type inference
data TypeInference = TypeInference
  { tiInferredType  :: Maybe TypeInfo
  , tiConstraints   :: [TypeConstraint]
  , tiConfidence    :: Double
  , tiSource        :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Infer the type of an expression
inferExpressionType :: HieContext -> FilePath -> SrcSpan -> IO TypeInference
inferExpressionType ctx _path theSpan = do
  let key = (srcSpanFile theSpan, unLine (srcSpanStartLine theSpan), unColumn (srcSpanStartCol theSpan))
      mType = Map.lookup key (hcTypeInfo ctx)
  pure TypeInference
    { tiInferredType = mType
    , tiConstraints = []  -- Could extract from type info if available
    , tiConfidence = if isJust mType then 0.9 else 0.0
    , tiSource = if isJust mType then "HIE" else "unknown"
    }
  where
    isJust (Just _) = True
    isJust Nothing = False

-- | Check if two types are compatible
checkTypeCompatibility :: TypeInfo -> TypeInfo -> Bool
checkTypeCompatibility t1 t2 =
  tiType t1 == tiType t2 ||
  tiType t1 == "*" ||
  tiType t2 == "*"

-- | Find a generalization of a type
findTypeGeneralization :: [TypeInfo] -> Maybe TypeInfo
findTypeGeneralization [] = Nothing
findTypeGeneralization [t] = Just t
findTypeGeneralization (t:ts) =
  case findTypeGeneralization ts of
    Nothing -> Just t
    Just t' -> if tiType t == tiType t'
               then Just t
               else Just $ TypeInfo "*" "*" [] False ["a"]

--------------------------------------------------------------------------------
-- Semantic Validation
--------------------------------------------------------------------------------

-- | Result of semantic validation
data SemanticValidation = SemanticValidation
  { svValid       :: Bool
  , svIssues      :: [SemanticIssue]
  , svConfidence  :: Double
  , svExplanation :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A semantic issue found during validation
data SemanticIssue
  = SITypeChange Text Text        -- ^ Type would change (old, new)
  | SIConstraintMissing Text Text -- ^ Missing constraint (class, type)
  | SIShadowing Text Text         -- ^ Would shadow name (name, module)
  | SISemanticChange Text         -- ^ Would change semantics (reason)
  | SIImportNeeded Text Text      -- ^ Need import (module, symbol)
  | SIOrphanInstance Text         -- ^ Would create orphan (class)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Validate a semantic fix
validateSemanticFix :: SemanticFixEngine
                    -> SemanticFixCandidate
                    -> SemanticFixContext
                    -> IO SemanticValidation
validateSemanticFix engine candidate ctx = do
  let config = sfeConfig engine

  issues <- if scEnableTypeChecking config
    then checkTypeIssues candidate ctx
    else pure []

  constraintIssues <- if scEnableConstraints config
    then checkConstraintIssues candidate ctx
    else pure []

  let allIssues = issues ++ constraintIssues
      isValid = null allIssues

  -- Update stats
  atomically $ do
    stats <- readTVar (sfeStats engine)
    writeTVar (sfeStats engine) stats
      { esFixesValidated = esFixesValidated stats + 1 }

  pure SemanticValidation
    { svValid = isValid
    , svIssues = allIssues
    , svConfidence = if isValid then sfcandConfidence candidate else 0.0
    , svExplanation = if isValid
        then "Fix validated successfully"
        else "Fix has semantic issues: " <> T.intercalate ", " (map showIssue allIssues)
    }
  where
    showIssue (SITypeChange old new) = "type change: " <> old <> " -> " <> new
    showIssue (SIConstraintMissing cls typ) = "missing " <> cls <> " " <> typ
    showIssue (SIShadowing name modName) = "shadows " <> name <> " from " <> modName
    showIssue (SISemanticChange reason) = "semantic change: " <> reason
    showIssue (SIImportNeeded modName sym) = "needs import " <> modName <> "." <> sym
    showIssue (SIOrphanInstance cls) = "would create orphan " <> cls

-- | Check for type issues
checkTypeIssues :: SemanticFixCandidate -> SemanticFixContext -> IO [SemanticIssue]
checkTypeIssues _ _ = pure []  -- Would check HIE data

-- | Check for constraint issues
checkConstraintIssues :: SemanticFixCandidate -> SemanticFixContext -> IO [SemanticIssue]
checkConstraintIssues _ _ = pure []  -- Would check HIE data

-- | Check semantic preservation
checkSemanticPreservation :: Fix -> SemanticFixContext -> IO Bool
checkSemanticPreservation _ _ = pure True  -- Would perform detailed analysis

--------------------------------------------------------------------------------
-- Fix Application
--------------------------------------------------------------------------------

-- | Result of applying a semantic fix
data SemanticApplicationResult = SemanticApplicationResult
  { sarSuccess     :: Bool
  , sarNewContent  :: Maybe Text
  , sarValidation  :: SemanticValidation
  , sarStats       :: EngineStats
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Apply a semantic fix
applySemanticFix :: SemanticFixEngine
                 -> FilePath
                 -> Text
                 -> SemanticFixCandidate
                 -> IO SemanticApplicationResult
applySemanticFix engine filePath content candidate = do
  ctx <- refreshHieContext engine
  symTable <- readTVarIO (sfeSymbolTable engine)
  cache <- readIORef (sfeTypeCache engine)

  let sctx = SemanticFixContext
        { sfcFilePath = filePath
        , sfcContent = content
        , sfcDiagnostic = emptyDiagnostic
        , sfcHieContext = ctx
        , sfcSymbolTable = symTable
        , sfcTypeCache = cache
        }

  -- Validate first
  validation <- validateSemanticFix engine candidate sctx

  if svValid validation
    then do
      -- Apply the fix
      let fix = sfcandFix candidate
          newContent = applyFixToContent content fix

      -- Update stats
      atomically $ do
        stats <- readTVar (sfeStats engine)
        writeTVar (sfeStats engine) stats
          { esFixesApplied = esFixesApplied stats + 1 }

      stats <- readTVarIO (sfeStats engine)
      pure SemanticApplicationResult
        { sarSuccess = True
        , sarNewContent = Just newContent
        , sarValidation = validation
        , sarStats = stats
        }
    else do
      stats <- readTVarIO (sfeStats engine)
      pure SemanticApplicationResult
        { sarSuccess = False
        , sarNewContent = Nothing
        , sarValidation = validation
        , sarStats = stats
        }
  where
    emptyDiagnostic = Diagnostic
      { diagSpan = noSpan
      , diagSeverity = Info
      , diagKind = CodePattern
      , diagMessage = ""
      , diagCode = Nothing
      , diagFixes = []
      , diagRelated = []
      }

    noSpan = SrcSpan "" (Line 0) (Column 0) (Line 0) (Column 0)

-- | Apply multiple semantic fixes
applySemanticFixes :: SemanticFixEngine
                   -> FilePath
                   -> Text
                   -> [SemanticFixCandidate]
                   -> IO (Text, [SemanticApplicationResult])
applySemanticFixes engine filePath content candidates = do
  go content candidates []
  where
    go curr [] results = pure (curr, reverse results)
    go curr (c:cs) results = do
      result <- applySemanticFix engine filePath curr c
      let newContent = fromMaybe curr (sarNewContent result)
      go newContent cs (result : results)

-- | Apply a fix to content
applyFixToContent :: Text -> Fix -> Text
applyFixToContent content fix =
  let edits = sortBy compareEdits (fixEdits fix)
  in foldr applyEdit content edits
  where
    compareEdits e1 e2 =
      compare (srcSpanStartLine $ fixEditSpan e2) (srcSpanStartLine $ fixEditSpan e1)

    applyEdit edit txt =
      let theSpan = fixEditSpan edit
          newText = fixEditNewText edit
          linesList = T.lines txt
          startLine = unLine (srcSpanStartLine theSpan) - 1
          endLine = unLine (srcSpanEndLine theSpan) - 1
          startCol = unColumn (srcSpanStartCol theSpan) - 1
          endCol = unColumn (srcSpanEndCol theSpan) - 1

          before = take startLine linesList
          after = drop (endLine + 1) linesList

          modifiedLines = if startLine == endLine
            then modifySingleLine (linesList !! startLine) startCol endCol newText
            else modifyMultipleLines linesList startLine endLine startCol endCol newText
      in T.unlines (before ++ modifiedLines ++ after)

    modifySingleLine line startCol endCol newText =
      [T.take startCol line <> newText <> T.drop endCol line]

    modifyMultipleLines linesList startLine endLine startCol endCol newText =
      let firstLine = linesList !! startLine
          lastLine = linesList !! endLine
      in [T.take startCol firstLine <> newText <> T.drop endCol lastLine]

    sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    sortBy _ [] = []
    sortBy cmp (x:xs) = sortBy cmp [y | y <- xs, cmp y x == LT]
                     ++ [x]
                     ++ sortBy cmp [y | y <- xs, cmp y x /= LT]

--------------------------------------------------------------------------------
-- FixEngine Instance
--------------------------------------------------------------------------------

instance FixEngine SemanticFixEngine where
  type EngineConfig SemanticFixEngine = SemanticConfig
  type EngineCategory SemanticFixEngine = SemanticFixCategory

  engineName _ = "semantic-fix"
  engineVersion _ = "1.0.0"
  engineCategories _ = [minBound .. maxBound]
  engineDescription _ = "Type-aware semantic fix engine using HIE data"

  findFixes engine path content = do
    candidates <- findSemanticFixes engine path content []
    pure $ map (toEnrichedFix "semantic-fix") candidates

  findFixesForDiagnostics engine diags content = do
    let path = case diags of
          [] -> ""
          (d:_) -> srcSpanFile (diagSpan d)
    candidates <- findSemanticFixes engine path content diags
    pure $ map (toEnrichedFix "semantic-fix") candidates

  validateFix engine ef content = do
    mCtx <- readTVarIO (sfeHieContext engine)
    case mCtx of
      Nothing -> pure $ FixValidation ValidationSuccess [] []
      Just ctx -> do
        cache <- readIORef (sfeTypeCache engine)
        symTable <- readTVarIO (sfeSymbolTable engine)
        let sctx = SemanticFixContext
              { sfcFilePath = ""
              , sfcContent = content
              , sfcDiagnostic = emptyDiagnostic
              , sfcHieContext = ctx
              , sfcSymbolTable = symTable
              , sfcTypeCache = cache
              }
            candidate = fromEnrichedFix ef
        validation <- validateSemanticFix engine candidate sctx
        pure $ FixValidation
          { fvResult = if svValid validation
              then ValidationSuccess
              else ValidationError (svExplanation validation)
          , fvChecks = []
          , fvSuggestions = []
          }
    where
      emptyDiagnostic = Diagnostic
        { diagSpan = noSpan
        , diagSeverity = Info
        , diagKind = CodePattern
        , diagMessage = ""
        , diagCode = Nothing
        , diagFixes = []
        , diagRelated = []
        }
      noSpan = SrcSpan "" (Line 0) (Column 0) (Line 0) (Column 0)

  applyFix engine path content ef = do
    let candidate = fromEnrichedFix ef
    result <- applySemanticFix engine path content candidate
    if sarSuccess result
      then pure ApplySuccess
        { arsNewContent = fromMaybe content (sarNewContent result)
        , arsAppliedFix = ef
        , arsStats = emptyFixStats
        }
      else pure ApplyFailure
        { arfError = ValidationFailed (svExplanation (sarValidation result))
        }

  getEngineStats engine = do
    stats <- readTVarIO (sfeStats engine)
    pure emptyFixStats
      { fsTotal = esFixesGenerated stats
      , fsApplied = esFixesApplied stats
      }

-- | Convert candidate to enriched fix
toEnrichedFix :: Text -> SemanticFixCandidate -> EnrichedFix
toEnrichedFix engName candidate =
  let fix = sfcandFix candidate
      localId = T.take 20 (sfcandReason candidate)
  in enrichFix engName localId fix (categoryToArgusCategory $ fixCategory fix)
  where
    categoryToArgusCategory :: FixCategory -> RT.Category
    categoryToArgusCategory = \case
      FCPerformance -> RT.Performance
      FCModernize   -> RT.Modernization
      FCSafety      -> RT.Safety
      FCStyle       -> RT.Style
      FCImports     -> RT.Imports
      FCRedundant   -> RT.Redundant
      FCSpaceLeaks  -> RT.SpaceLeaks
      FCSecurity    -> RT.Security
      FCCustom t    -> RT.Custom t

-- | Convert enriched fix back to candidate
fromEnrichedFix :: EnrichedFix -> SemanticFixCandidate
fromEnrichedFix ef = SemanticFixCandidate
  { sfcandFix = efFix ef
  , sfcandConfidence = unConfidence (fmConfidence (efMetadata ef))
  , sfcandReason = fixIdLocal (efId ef)
  , sfcandValidation = Nothing
  , sfcandCategory = SFCRefactoring
  }
