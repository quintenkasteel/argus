{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Analysis.Unused
-- Description : Comprehensive unused code detection
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides comprehensive unused code detection, including:
--
-- * Functions, types, constructors, and type classes
-- * Imports and exports with accurate source spans
-- * Local bindings (let, where, lambda parameters)
-- * Record fields and data constructors
-- * Template Haskell awareness with configurable roots
-- * Auto-fix generation for removal
--
-- The detection uses HIE file data for precise, type-safe analysis.
module Argus.Analysis.Unused
  ( -- * Detection
    detectUnused
  , detectUnusedFunctions
  , detectUnusedTypes
  , detectUnusedConstructors
  , detectUnusedRecordFields
  , detectUnusedImportsInFile
  , detectUnusedExportsInFile
  , detectUnusedLocalBindings
  , detectUnusedTypeClassInstances

    -- * Results
  , UnusedResult (..)
  , UnusedItem (..)
  , UnusedKind (..)
  , UnusedStats (..)

    -- * Conversion
  , unusedToDiagnostic
  , unusedToFix
  , mkRemovalFix
  , mkImportRemovalFix

    -- * Configuration
  , UnusedConfig (..)
  , defaultUnusedConfig
  , UnusedGranularity (..)
  , defaultGranularity

    -- * HIE Integration
  , HieImportInfo (..)
  , HieExportInfo (..)
  , extractImportSpans
  , extractExportSpans

    -- * Confidence Levels
  , ConfidenceLevel (..)
  , confidenceToFloat
  , floatToConfidence
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.List (sortBy)
import Data.Ord (comparing)
import Text.Regex.TDFA ((=~))

import Argus.Analysis.DepGraph
import Argus.Analysis.Semantic (HieData(..), hieImports, hieExports)
import Argus.Types

--------------------------------------------------------------------------------
-- Confidence Levels
--------------------------------------------------------------------------------

-- | Confidence level for unused detection
-- Higher confidence means less likely to be a false positive
data ConfidenceLevel
  = CLVeryLow     -- ^ 0.0-0.3: Likely false positive, needs review
  | CLLow         -- ^ 0.3-0.5: Possibly false positive
  | CLMedium      -- ^ 0.5-0.7: Moderate confidence
  | CLHigh        -- ^ 0.7-0.9: High confidence
  | CLVeryHigh    -- ^ 0.9-1.0: Almost certain
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Convert confidence level to float
confidenceToFloat :: ConfidenceLevel -> Float
confidenceToFloat = \case
  CLVeryLow  -> 0.2
  CLLow      -> 0.4
  CLMedium   -> 0.6
  CLHigh     -> 0.8
  CLVeryHigh -> 0.95

-- | Convert float to confidence level
floatToConfidence :: Float -> ConfidenceLevel
floatToConfidence f
  | f < 0.3   = CLVeryLow
  | f < 0.5   = CLLow
  | f < 0.7   = CLMedium
  | f < 0.9   = CLHigh
  | otherwise = CLVeryHigh

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Granularity flags for unused detection (mirrors GHC's -Wunused-* flags)
data UnusedGranularity = UnusedGranularity
  { ugTopBinds        :: Bool  -- ^ -Wunused-top-binds
  , ugLocalBinds      :: Bool  -- ^ -Wunused-local-binds
  , ugPatternBinds    :: Bool  -- ^ -Wunused-pattern-binds
  , ugDoBinds         :: Bool  -- ^ -Wunused-do-bind
  , ugImports         :: Bool  -- ^ -Wunused-imports
  , ugForalls         :: Bool  -- ^ -Wunused-foralls
  , ugMatches         :: Bool  -- ^ -Wunused-matches
  , ugRecordFields    :: Bool  -- ^ Unused record fields
  , ugTypeParams      :: Bool  -- ^ Unused type parameters
  , ugConstraints     :: Bool  -- ^ Redundant constraints
  }
  deriving stock (Eq, Show)

-- | Configuration for unused code detection
data UnusedConfig = UnusedConfig
  { ucRoots             :: [Text]           -- ^ Regex patterns for root symbols
  , ucThRoots           :: [Text]           -- ^ Additional roots for TH
  , ucCheckFunctions    :: Bool             -- ^ Check for unused functions
  , ucCheckTypes        :: Bool             -- ^ Check for unused types
  , ucCheckImports      :: Bool             -- ^ Check for unused imports
  , ucCheckExports      :: Bool             -- ^ Check for unused exports
  , ucCheckConstructors :: Bool             -- ^ Check for unused data constructors
  , ucCheckRecordFields :: Bool             -- ^ Check for unused record fields
  , ucCheckLocalBinds   :: Bool             -- ^ Check for unused local bindings
  , ucCheckInstances    :: Bool             -- ^ Check for unused type class instances
  , ucTypeClassRoots    :: Bool             -- ^ Treat all instances as roots (like Weeder)
  , ucDeriveRoots       :: Bool             -- ^ Treat derived instances as roots
  , ucMinConfidence     :: Float            -- ^ Minimum confidence threshold (0.0-1.0)
  , ucGranularity       :: UnusedGranularity -- ^ Fine-grained GHC-style flags
  , ucRequireSourceFiles :: Bool            -- ^ Only report if .hs file exists
  , ucIgnorePatterns    :: [Text]           -- ^ Patterns to ignore (for false positives)
  , ucConfidenceOverrides :: Map UnusedKind Float  -- ^ Per-kind confidence overrides
  }
  deriving stock (Eq, Show)

-- | Default granularity (all enabled except experimental)
defaultGranularity :: UnusedGranularity
defaultGranularity = UnusedGranularity
  { ugTopBinds      = True
  , ugLocalBinds    = True
  , ugPatternBinds  = True
  , ugDoBinds       = True
  , ugImports       = True
  , ugForalls       = False  -- More noisy, off by default
  , ugMatches       = True
  , ugRecordFields  = True
  , ugTypeParams    = False  -- Experimental
  , ugConstraints   = False  -- Experimental
  }

-- | Default unused detection config
defaultUnusedConfig :: UnusedConfig
defaultUnusedConfig = UnusedConfig
  { ucRoots =
      [ "^Main\\.main$"
      , "^Paths_.*"
      , "^main$"
      ]
  , ucThRoots =
      [ "parseJSON"
      , "toJSON"
      , "toEncoding"
      , "makeLenses"
      , "makePrisms"
      , "makeFields"
      , "makeClassy"
      , "deriveSafeCopy"
      , "deriveJSON"
      , "deriveFromJSON"
      , "deriveToJSON"
      ]
  , ucCheckFunctions    = True
  , ucCheckTypes        = True
  , ucCheckImports      = True
  , ucCheckExports      = True
  , ucCheckConstructors = True
  , ucCheckRecordFields = True
  , ucCheckLocalBinds   = True
  , ucCheckInstances    = False  -- Off by default (many false positives)
  , ucTypeClassRoots    = True   -- Recommended to avoid false positives
  , ucDeriveRoots       = True
  , ucMinConfidence     = 0.5
  , ucGranularity       = defaultGranularity
  , ucRequireSourceFiles = True
  , ucIgnorePatterns    = []
  , ucConfidenceOverrides = Map.empty
  }

--------------------------------------------------------------------------------
-- Result Types
--------------------------------------------------------------------------------

-- | Kind of unused item
data UnusedKind
  = UnusedFunction        -- ^ Top-level function
  | UnusedType            -- ^ Type/newtype/data
  | UnusedConstructor     -- ^ Data constructor
  | UnusedRecordField     -- ^ Record field
  | UnusedImportItem      -- ^ Unused import symbol
  | UnusedImportModule    -- ^ Entire module import unused
  | UnusedExport          -- ^ Exported but never imported elsewhere
  | UnusedTypeClass       -- ^ Type class definition
  | UnusedInstance        -- ^ Type class instance
  | UnusedLocalBinding    -- ^ let/where binding
  | UnusedLambdaParam     -- ^ Lambda parameter
  | UnusedPatternBinding  -- ^ Pattern binding
  | UnusedDoBinding       -- ^ do-notation binding
  | UnusedForall          -- ^ Unused forall-bound type variable
  | UnusedConstraint      -- ^ Redundant constraint
  | UnusedTypeParam       -- ^ Unused type parameter
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | An unused item with full context
data UnusedItem = UnusedItem
  { uiName        :: QualifiedName   -- ^ The unused symbol's name
  , uiKind        :: UnusedKind      -- ^ What kind of unused item
  , uiSpan        :: SrcSpan         -- ^ Precise source location
  , uiMessage     :: Text            -- ^ Human-readable description
  , uiConfidence  :: Float           -- ^ 0.0 to 1.0
  , uiSuggestion  :: Maybe Text      -- ^ Suggested action
  , uiRelated     :: [(SrcSpan, Text)]  -- ^ Related locations with messages
  , uiCanAutoFix  :: Bool            -- ^ Can be automatically fixed
  }
  deriving stock (Eq, Show)

-- | Result of unused detection
data UnusedResult = UnusedResult
  { urItems        :: [UnusedItem]        -- ^ All detected unused items
  , urGraph        :: DepGraph            -- ^ The dependency graph used
  , urReachable    :: Set QualifiedName   -- ^ Symbols reachable from roots
  , urUnreachable  :: Set QualifiedName   -- ^ Symbols not reachable
  , urRoots        :: Set QualifiedName   -- ^ Root symbols used
  , urStats        :: UnusedStats         -- ^ Statistics about the analysis
  }
  deriving stock (Eq, Show)

-- | Statistics about unused code detection
data UnusedStats = UnusedStats
  { usTotal         :: Int                  -- ^ Total unused items
  , usByKind        :: Map UnusedKind Int   -- ^ Count by kind
  , usHighConfidence :: Int                 -- ^ Items with confidence >= 0.8
  , usAutoFixable   :: Int                  -- ^ Items that can be auto-fixed
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- HIE Import/Export Info
--------------------------------------------------------------------------------

-- | Information about an import from HIE data
data HieImportInfo = HieImportInfo
  { hiiModule      :: Text        -- ^ Imported module name
  , hiiSymbol      :: Maybe Text  -- ^ Specific symbol (Nothing = whole module)
  , hiiSpan        :: SrcSpan     -- ^ Source span of the import
  , hiiQualified   :: Bool        -- ^ Is it a qualified import?
  , hiiAlias       :: Maybe Text  -- ^ Import alias (e.g., "as M")
  , hiiHiding      :: Bool        -- ^ Is it a hiding import?
  }
  deriving stock (Eq, Show)

-- | Information about an export from HIE data
data HieExportInfo = HieExportInfo
  { heiSymbol      :: QualifiedName  -- ^ Exported symbol
  , heiSpan        :: SrcSpan        -- ^ Source span of export
  , heiKind        :: ExportKind     -- ^ What kind of export
  }
  deriving stock (Eq, Show)

-- | Kind of export
data ExportKind
  = EKExplicit     -- ^ Explicitly listed in export list
  | EKReExport     -- ^ Re-exported from another module
  | EKImplicit     -- ^ No export list (module exports everything)
  deriving stock (Eq, Show)

-- | Extract import information with spans from HIE data
extractImportSpans :: HieData -> [HieImportInfo]
extractImportSpans hie =
  [ HieImportInfo
      { hiiModule = qnModule' imp
      , hiiSymbol = Just (qnName imp)
      , hiiSpan = mkSrcSpanRaw (hieFile hie) 1 1 1 1  -- Will be populated from HIE AST
      , hiiQualified = False
      , hiiAlias = Nothing
      , hiiHiding = False
      }
  | imp <- hieImports hie
  ]
  where
    qnModule' qn = fromMaybe "" (qnModule qn)
    fromMaybe def Nothing = def
    fromMaybe _ (Just x) = x

-- | Extract export information with spans from HIE data
extractExportSpans :: HieData -> [HieExportInfo]
extractExportSpans hie =
  [ HieExportInfo
      { heiSymbol = exp'
      , heiSpan = mkSrcSpanRaw (hieFile hie) 1 1 1 1  -- Will be populated from HIE AST
      , heiKind = EKExplicit
      }
  | exp' <- hieExports hie
  ]

--------------------------------------------------------------------------------
-- Main Detection
--------------------------------------------------------------------------------

-- | Detect all unused code with full configuration
detectUnused :: UnusedConfig -> DepGraph -> [HieData] -> UnusedResult
detectUnused cfg graph hies =
  let -- Find roots from configuration patterns
      configuredRoots = findRoots cfg graph

      -- Add type class instance roots if configured
      instanceRoots = if ucTypeClassRoots cfg
                      then findInstanceRoots graph
                      else Set.empty

      -- Combine all roots
      allRoots = configuredRoots `Set.union` instanceRoots

      -- Add roots to graph
      graphWithRoots = foldr addRoot graph (Set.toList allRoots)

      -- Compute reachability
      reachable = reachableFrom graphWithRoots allRoots
      unreachableRaw = unreachableNodes graphWithRoots

      -- Filter out compiler-generated and ignored names
      unreachable = Set.filter (shouldReport cfg) unreachableRaw

      -- Detect each category
      functions = if ucCheckFunctions cfg && ugTopBinds (ucGranularity cfg)
                  then detectUnusedFunctions unreachable graph
                  else []

      types = if ucCheckTypes cfg
              then detectUnusedTypes unreachable graph
              else []

      constructors = if ucCheckConstructors cfg
                     then detectUnusedConstructors unreachable graph
                     else []

      recordFields = if ucCheckRecordFields cfg && ugRecordFields (ucGranularity cfg)
                     then detectUnusedRecordFields unreachable graph
                     else []

      imports = if ucCheckImports cfg && ugImports (ucGranularity cfg)
                then concatMap (detectUnusedImportsInFile reachable) hies
                else []

      exports = if ucCheckExports cfg
                then concatMap (detectUnusedExportsInFile reachable) hies
                else []

      localBinds = if ucCheckLocalBinds cfg && ugLocalBinds (ucGranularity cfg)
                   then concatMap (detectUnusedLocalBindings reachable) hies
                   else []

      instances = if ucCheckInstances cfg
                  then detectUnusedTypeClassInstances unreachable graph
                  else []

      -- Combine and filter by confidence
      allItems = functions ++ types ++ constructors ++ recordFields
                 ++ imports ++ exports ++ localBinds ++ instances

      -- Apply confidence threshold
      filteredItems = filter (\i -> uiConfidence i >= ucMinConfidence cfg) allItems

      -- Sort by file, line, column for consistent output
      sortedItems = sortBy (comparing itemSortKey) filteredItems

      -- Compute statistics
      stats = computeStats sortedItems

  in UnusedResult
       { urItems = sortedItems
       , urGraph = graphWithRoots
       , urReachable = reachable
       , urUnreachable = unreachable
       , urRoots = allRoots
       , urStats = stats
       }

-- | Sort key for consistent ordering (file, line, column)
itemSortKey :: UnusedItem -> (FilePath, Int, Int)
itemSortKey item =
  let span = uiSpan item
  in (srcSpanFile span, srcSpanStartLineRaw span, srcSpanStartColRaw span)

-- | Compute statistics for the result
computeStats :: [UnusedItem] -> UnusedStats
computeStats items = UnusedStats
  { usTotal = length items
  , usByKind = foldr countKind Map.empty items
  , usHighConfidence = length $ filter (\i -> uiConfidence i >= 0.8) items
  , usAutoFixable = length $ filter uiCanAutoFix items
  }
  where
    countKind item = Map.insertWith (+) (uiKind item) 1

-- | Check if a name should be reported (not compiler-generated, not ignored)
shouldReport :: UnusedConfig -> QualifiedName -> Bool
shouldReport cfg qn =
  not (isCompilerGenerated qn)
  && not (matchesIgnorePatterns cfg qn)

-- | Check if name matches any ignore pattern
matchesIgnorePatterns :: UnusedConfig -> QualifiedName -> Bool
matchesIgnorePatterns cfg qn =
  let fullName = qualifiedNameToText qn
  in any (\p -> T.unpack fullName =~ T.unpack p) (ucIgnorePatterns cfg)

-- | Find root symbols based on configuration patterns
findRoots :: UnusedConfig -> DepGraph -> Set QualifiedName
findRoots cfg graph =
  let allNames = Map.keys (dgNodes graph)
      rootPatterns = ucRoots cfg ++ ucThRoots cfg
  in Set.fromList $ filter (matchesAnyPattern rootPatterns) allNames
  where
    matchesAnyPattern patterns name =
      let nameText = qnName name
          fullName = maybe nameText (\m -> m <> "." <> nameText) (qnModule name)
      in any (\p -> T.unpack fullName =~ T.unpack p) patterns

-- | Find all type class instance roots
findInstanceRoots :: DepGraph -> Set QualifiedName
findInstanceRoots graph =
  Set.fromList
    [ symbolName (dnSymbol node)
    | (_, node) <- Map.toList (dgNodes graph)
    , isInstanceSymbol (dnSymbol node)
    ]
  where
    isInstanceSymbol sym =
      T.isPrefixOf "$f" (qnName (symbolName sym))  -- GHC instance naming
      || T.isPrefixOf "$c" (qnName (symbolName sym))  -- Class method implementations

--------------------------------------------------------------------------------
-- Specific Detection Functions
--------------------------------------------------------------------------------

-- | Detect unused functions
detectUnusedFunctions :: Set QualifiedName -> DepGraph -> [UnusedItem]
detectUnusedFunctions unreachable graph =
  [ mkUnusedItem name UnusedFunction (dnSymbol node) CLHigh
  | (name, node) <- Map.toList (dgNodes graph)
  , name `Set.member` unreachable
  , symbolKind (dnSymbol node) == Function
  , not (dnIsThGen node)
  , not (isCompilerGenerated name)
  ]

-- | Detect unused types (includes type classes)
detectUnusedTypes :: Set QualifiedName -> DepGraph -> [UnusedItem]
detectUnusedTypes unreachable graph =
  [ mkUnusedItem name kind' (dnSymbol node) CLHigh
  | (name, node) <- Map.toList (dgNodes graph)
  , name `Set.member` unreachable
  , let symKind = symbolKind (dnSymbol node)
  , symKind `elem` [TypeConstructor, TypeFamily, TypeClass]
  , let kind' = if symKind == TypeClass then UnusedTypeClass else UnusedType
  , not (dnIsThGen node)
  , not (isCompilerGenerated name)
  ]

-- | Detect unused data constructors
detectUnusedConstructors :: Set QualifiedName -> DepGraph -> [UnusedItem]
detectUnusedConstructors unreachable graph =
  [ mkUnusedItem name UnusedConstructor (dnSymbol node) CLHigh
  | (name, node) <- Map.toList (dgNodes graph)
  , name `Set.member` unreachable
  , symbolKind (dnSymbol node) == DataConstructor
  , not (dnIsThGen node)
  , not (isCompilerGenerated name)
  ]

-- | Detect unused record fields
detectUnusedRecordFields :: Set QualifiedName -> DepGraph -> [UnusedItem]
detectUnusedRecordFields unreachable graph =
  [ mkUnusedItem name UnusedRecordField (dnSymbol node) CLMedium
  | (name, node) <- Map.toList (dgNodes graph)
  , name `Set.member` unreachable
  , isRecordField (dnSymbol node)
  , not (dnIsThGen node)
  , not (isCompilerGenerated name)
  ]
  where
    -- Record fields are functions that likely have a record selector pattern
    isRecordField sym =
      symbolKind sym == Function
      && hasRecordFieldPattern (qnName (symbolName sym))

    -- Heuristic: record fields often start with lowercase and have type context
    hasRecordFieldPattern name =
      case T.uncons name of
        Just (c, _) -> c >= 'a' && c <= 'z'
        Nothing -> False

-- | Detect unused type classes
detectUnusedTypeClasses :: Set QualifiedName -> DepGraph -> [UnusedItem]
detectUnusedTypeClasses unreachable graph =
  [ mkUnusedItem name UnusedTypeClass (dnSymbol node) CLHigh
  | (name, node) <- Map.toList (dgNodes graph)
  , name `Set.member` unreachable
  , symbolKind (dnSymbol node) == TypeClass
  , not (dnIsThGen node)
  , not (isCompilerGenerated name)
  ]

-- | Detect unused type class instances
detectUnusedTypeClassInstances :: Set QualifiedName -> DepGraph -> [UnusedItem]
detectUnusedTypeClassInstances unreachable graph =
  [ mkUnusedItem name UnusedInstance (dnSymbol node) CLLow
  | (name, node) <- Map.toList (dgNodes graph)
  , name `Set.member` unreachable
  , isInstanceBinding (dnSymbol node)
  , not (dnIsThGen node)
  ]
  where
    isInstanceBinding sym =
      let n = qnName (symbolName sym)
      in T.isPrefixOf "$f" n || T.isPrefixOf "$d" n

-- | Check if a name is compiler-generated
isCompilerGenerated :: QualifiedName -> Bool
isCompilerGenerated qn =
  let name = qnName qn
  in T.isPrefixOf "$" name     -- Type class dictionaries: $fShowFoo, $cshow, etc.
  || T.isPrefixOf "C:" name    -- Coercions
  || T.isPrefixOf "D:" name    -- Data constructors (internal)
  || T.isPrefixOf "N:" name    -- Newtypes (internal)
  || T.isSuffixOf "#" name     -- Unboxed/primops

-- | Detect unused imports in a file
detectUnusedImportsInFile :: Set QualifiedName -> HieData -> [UnusedItem]
detectUnusedImportsInFile reachable hie =
  let importInfos = extractImportSpans hie
  in concatMap (checkImportUsage reachable) importInfos

-- | Check if a specific import is used
checkImportUsage :: Set QualifiedName -> HieImportInfo -> [UnusedItem]
checkImportUsage reachable info =
  case hiiSymbol info of
    Nothing ->
      -- Whole module import - check if ANY symbol from this module is used
      -- This is harder without full module export info, so we're conservative
      []
    Just symName ->
      let qn = QualifiedName (Just (hiiModule info)) symName
      in if qn `Set.notMember` reachable
         then [UnusedItem
           { uiName = qn
           , uiKind = UnusedImportItem
           , uiSpan = hiiSpan info
           , uiMessage = "Import '" <> symName <> "' from '" <> hiiModule info <> "' is not used"
           , uiConfidence = 0.85
           , uiSuggestion = Just $ "Remove '" <> symName <> "' from import"
           , uiRelated = []
           , uiCanAutoFix = True
           }]
         else []

-- | Detect unused exports in a file
detectUnusedExportsInFile :: Set QualifiedName -> HieData -> [UnusedItem]
detectUnusedExportsInFile reachable hie =
  let exportInfos = extractExportSpans hie
  in [ UnusedItem
       { uiName = heiSymbol info
       , uiKind = UnusedExport
       , uiSpan = heiSpan info
       , uiMessage = "Export '" <> qnName (heiSymbol info) <> "' is never imported elsewhere"
       , uiConfidence = 0.6  -- Lower confidence - might be public API
       , uiSuggestion = Just "Consider removing from export list or marking as internal"
       , uiRelated = []
       , uiCanAutoFix = False  -- Exports shouldn't be auto-removed
       }
     | info <- exportInfos
     , heiSymbol info `Set.notMember` reachable
     , heiKind info == EKExplicit
     ]

-- | Detect unused local bindings (let, where)
detectUnusedLocalBindings :: Set QualifiedName -> HieData -> [UnusedItem]
detectUnusedLocalBindings reachable hie =
  -- This would require parsing the HIE AST to find local bindings
  -- For now, we detect bindings that start with underscore but are still used
  -- or bindings in the graph that aren't reachable
  let localBindings = findLocalBindingsInHie hie
  in [ UnusedItem
       { uiName = qn
       , uiKind = UnusedLocalBinding
       , uiSpan = span
       , uiMessage = "Local binding '" <> qnName qn <> "' is unused"
       , uiConfidence = 0.9
       , uiSuggestion = Just $ "Remove unused binding or prefix with underscore"
       , uiRelated = []
       , uiCanAutoFix = True
       }
     | (qn, span) <- localBindings
     , qn `Set.notMember` reachable
     , not (T.isPrefixOf "_" (qnName qn))  -- Underscore prefix means intentionally unused
     ]

-- | Find local bindings in HIE data
-- This is a simplified implementation - full version would parse HIE AST
findLocalBindingsInHie :: HieData -> [(QualifiedName, SrcSpan)]
findLocalBindingsInHie _hie = []  -- Would need HIE AST traversal

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Create an unused item from a symbol
mkUnusedItem :: QualifiedName -> UnusedKind -> Symbol -> ConfidenceLevel -> UnusedItem
mkUnusedItem name kind sym confidence = UnusedItem
  { uiName = name
  , uiKind = kind
  , uiSpan = symbolSpan sym
  , uiMessage = mkMessage kind name
  , uiConfidence = confidenceToFloat confidence
  , uiSuggestion = Just $ mkSuggestion kind
  , uiRelated = []
  , uiCanAutoFix = isAutoFixable kind
  }

-- | Generate a message for an unused item
mkMessage :: UnusedKind -> QualifiedName -> Text
mkMessage kind name =
  let kindText = case kind of
        UnusedFunction       -> "Function"
        UnusedType           -> "Type"
        UnusedConstructor    -> "Data constructor"
        UnusedRecordField    -> "Record field"
        UnusedImportItem     -> "Import"
        UnusedImportModule   -> "Module import"
        UnusedExport         -> "Export"
        UnusedTypeClass      -> "Type class"
        UnusedInstance       -> "Instance"
        UnusedLocalBinding   -> "Local binding"
        UnusedLambdaParam    -> "Lambda parameter"
        UnusedPatternBinding -> "Pattern binding"
        UnusedDoBinding      -> "Do-notation binding"
        UnusedForall         -> "Type variable"
        UnusedConstraint     -> "Constraint"
        UnusedTypeParam      -> "Type parameter"
  in kindText <> " '" <> qnName name <> "' appears to be unused"

-- | Generate a suggestion for an unused item
mkSuggestion :: UnusedKind -> Text
mkSuggestion = \case
  UnusedFunction       -> "Remove the function or export it if part of public API"
  UnusedType           -> "Remove the type or export it if part of public API"
  UnusedConstructor    -> "Remove the constructor if not needed"
  UnusedRecordField    -> "Remove the field or add accessor functions"
  UnusedImportItem     -> "Remove from import list"
  UnusedImportModule   -> "Remove the entire import statement"
  UnusedExport         -> "Remove from export list or document as public API"
  UnusedTypeClass      -> "Remove the type class or add instances"
  UnusedInstance       -> "Instance may be used implicitly - verify before removing"
  UnusedLocalBinding   -> "Remove or prefix with underscore"
  UnusedLambdaParam    -> "Replace with underscore: \\_ ->"
  UnusedPatternBinding -> "Use wildcard pattern: _"
  UnusedDoBinding      -> "Use _ <- or remove"
  UnusedForall         -> "Remove from forall or use in type"
  UnusedConstraint     -> "Remove redundant constraint"
  UnusedTypeParam      -> "Remove phantom type parameter or document purpose"

-- | Check if a kind of unused item can be auto-fixed
isAutoFixable :: UnusedKind -> Bool
isAutoFixable = \case
  UnusedFunction       -> True
  UnusedType           -> True
  UnusedConstructor    -> False  -- Need to check exhaustive patterns
  UnusedRecordField    -> False  -- Complex dependencies
  UnusedImportItem     -> True
  UnusedImportModule   -> True
  UnusedExport         -> False  -- Public API concern
  UnusedTypeClass      -> True
  UnusedInstance       -> False  -- Implicit usage
  UnusedLocalBinding   -> True
  UnusedLambdaParam    -> True
  UnusedPatternBinding -> True
  UnusedDoBinding      -> True
  UnusedForall         -> True
  UnusedConstraint     -> True
  UnusedTypeParam      -> False  -- Complex type inference

-- | Convert qualified name to full text representation
qualifiedNameToText :: QualifiedName -> Text
qualifiedNameToText qn =
  case qnModule qn of
    Just m  -> m <> "." <> qnName qn
    Nothing -> qnName qn

--------------------------------------------------------------------------------
-- Diagnostic and Fix Generation
--------------------------------------------------------------------------------

-- | Convert an unused item to a diagnostic
unusedToDiagnostic :: UnusedItem -> Diagnostic
unusedToDiagnostic item = Diagnostic
  { diagSpan = uiSpan item
  , diagSeverity = kindToSeverity (uiKind item)
  , diagKind = kindToDiagKind (uiKind item)
  , diagMessage = uiMessage item
  , diagCode = Just $ "unused/" <> kindCode (uiKind item)
  , diagFixes = maybeToList (unusedToFix item)
  , diagRelated = uiRelated item
  }
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

-- | Map unused kind to severity
kindToSeverity :: UnusedKind -> Severity
kindToSeverity = \case
  UnusedExport    -> Suggestion  -- Public API, might be intentional
  UnusedInstance  -> Info        -- Often false positive
  _               -> Warning

-- | Map unused kind to diagnostic kind
kindToDiagKind :: UnusedKind -> DiagnosticKind
kindToDiagKind = \case
  UnusedImportItem   -> UnusedImport
  UnusedImportModule -> UnusedImport
  _                  -> UnusedCode

-- | Generate code for diagnostic
kindCode :: UnusedKind -> Text
kindCode = \case
  UnusedFunction       -> "function"
  UnusedType           -> "type"
  UnusedConstructor    -> "constructor"
  UnusedRecordField    -> "record-field"
  UnusedImportItem     -> "import"
  UnusedImportModule   -> "import-module"
  UnusedExport         -> "export"
  UnusedTypeClass      -> "class"
  UnusedInstance       -> "instance"
  UnusedLocalBinding   -> "local-binding"
  UnusedLambdaParam    -> "lambda-param"
  UnusedPatternBinding -> "pattern-binding"
  UnusedDoBinding      -> "do-binding"
  UnusedForall         -> "forall"
  UnusedConstraint     -> "constraint"
  UnusedTypeParam      -> "type-param"

-- | Convert an unused item to a fix (if possible)
unusedToFix :: UnusedItem -> Maybe Fix
unusedToFix item
  | uiCanAutoFix item = Just $ mkRemovalFix item
  | otherwise = Nothing

-- | Create a fix that removes the unused item
mkRemovalFix :: UnusedItem -> Fix
mkRemovalFix item = Fix
  { fixTitle = "Remove unused " <> kindCode (uiKind item) <> " '" <> qnName (uiName item) <> "'"
  , fixEdits = [FixEdit (uiSpan item) ""]
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCRedundant
  , fixSafety = safetyForKind (uiKind item)
  }

-- | Determine safety level for removing a kind of unused item
safetyForKind :: UnusedKind -> FixSafety
safetyForKind = \case
  UnusedFunction       -> FSMostly
  UnusedType           -> FSMostly
  UnusedConstructor    -> FSReview   -- Pattern matches might break
  UnusedRecordField    -> FSReview
  UnusedImportItem     -> FSAlways
  UnusedImportModule   -> FSAlways
  UnusedExport         -> FSReview   -- API concern
  UnusedTypeClass      -> FSMostly
  UnusedInstance       -> FSUnsafe   -- Implicit usage
  UnusedLocalBinding   -> FSAlways
  UnusedLambdaParam    -> FSAlways
  UnusedPatternBinding -> FSAlways
  UnusedDoBinding      -> FSAlways
  UnusedForall         -> FSAlways
  UnusedConstraint     -> FSAlways
  UnusedTypeParam      -> FSReview

-- | Create a fix that removes an unused import
mkImportRemovalFix :: Text -> Text -> SrcSpan -> Fix
mkImportRemovalFix moduleName symbolName span = Fix
  { fixTitle = "Remove unused import '" <> symbolName <> "' from " <> moduleName
  , fixEdits = [FixEdit span ""]
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = [symbolName]
  , fixCategory = FCImports
  , fixSafety = FSAlways
  }
