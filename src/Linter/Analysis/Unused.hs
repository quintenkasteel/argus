{-# LANGUAGE StrictData #-}

-- |
-- Module      : Linter.Analysis.Unused
-- Description : Unused code detection
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides comprehensive unused code detection, including
-- functions, types, imports, and exports. It supports cross-module
-- analysis and Template Haskell awareness.
module Linter.Analysis.Unused
  ( -- * Detection
    detectUnused
  , detectUnusedFunctions
  , detectUnusedTypes
  , detectUnusedImportsInFile
  , detectUnusedExportsInFile

    -- * Results
  , UnusedResult (..)
  , UnusedItem (..)
  , UnusedKind (..)

    -- * Configuration
  , UnusedConfig (..)
  , defaultUnusedConfig
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Text.Regex.TDFA ((=~))

import Linter.Analysis.DepGraph
import Linter.Analysis.Semantic
import Linter.Types

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for unused code detection
data UnusedConfig = UnusedConfig
  { ucRoots          :: [Text]  -- ^ Regex patterns for root symbols
  , ucThRoots        :: [Text]  -- ^ Additional roots for TH
  , ucCheckFunctions :: Bool    -- ^ Check for unused functions
  , ucCheckTypes     :: Bool    -- ^ Check for unused types
  , ucCheckImports   :: Bool    -- ^ Check for unused imports
  , ucCheckExports   :: Bool    -- ^ Check for unused exports
  }
  deriving stock (Eq, Show)

-- | Default unused detection config
defaultUnusedConfig :: UnusedConfig
defaultUnusedConfig = UnusedConfig
  { ucRoots = ["^Main.main$", "^Paths_.*"]
  , ucThRoots = ["parseJSON", "toJSON", "makeLenses"]
  , ucCheckFunctions = True
  , ucCheckTypes = True
  , ucCheckImports = True
  , ucCheckExports = True
  }

--------------------------------------------------------------------------------
-- Result Types
--------------------------------------------------------------------------------

-- | Kind of unused item
data UnusedKind
  = UnusedFunction
  | UnusedType
  | UnusedConstructor
  | UnusedImportItem
  | UnusedExport
  | UnusedTypeClass
  | UnusedInstance
  deriving stock (Eq, Ord, Show)

-- | An unused item
data UnusedItem = UnusedItem
  { uiName     :: QualifiedName
  , uiKind     :: UnusedKind
  , uiSpan     :: SrcSpan
  , uiMessage  :: Text
  , uiConfidence :: Float  -- 0.0 to 1.0
  }
  deriving stock (Eq, Show)

-- | Result of unused detection
data UnusedResult = UnusedResult
  { urItems       :: [UnusedItem]
  , urGraph       :: DepGraph
  , urReachable   :: Set QualifiedName
  , urUnreachable :: Set QualifiedName
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Main Detection
--------------------------------------------------------------------------------

-- | Detect all unused code
detectUnused :: UnusedConfig -> DepGraph -> [HieData] -> UnusedResult
detectUnused cfg graph hies =
  let roots = findRoots cfg graph
      graphWithRoots = foldr addRoot graph (Set.toList roots)
      reachable = reachableFrom graphWithRoots roots
      unreachable = unreachableNodes graphWithRoots

      functions = if ucCheckFunctions cfg
                  then detectUnusedFunctions unreachable graph
                  else []
      types = if ucCheckTypes cfg
              then detectUnusedTypes unreachable graph
              else []
      imports = if ucCheckImports cfg
                then concatMap (detectUnusedImportsInFile reachable) hies
                else []
      exports = if ucCheckExports cfg
                then concatMap (detectUnusedExportsInFile reachable) hies
                else []

  in UnusedResult
    { urItems = functions ++ types ++ imports ++ exports
    , urGraph = graphWithRoots
    , urReachable = reachable
    , urUnreachable = unreachable
    }

-- | Find root symbols based on configuration
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

--------------------------------------------------------------------------------
-- Specific Detection
--------------------------------------------------------------------------------

-- | Detect unused functions
detectUnusedFunctions :: Set QualifiedName -> DepGraph -> [UnusedItem]
detectUnusedFunctions unreachable graph =
  [ mkUnusedItem name UnusedFunction (dnSymbol node) 0.9
  | (name, node) <- Map.toList (dgNodes graph)
  , name `Set.member` unreachable
  , symbolKind (dnSymbol node) == Function
  , not (dnIsThGen node)  -- Don't flag TH-generated code
  ]

-- | Detect unused types
detectUnusedTypes :: Set QualifiedName -> DepGraph -> [UnusedItem]
detectUnusedTypes unreachable graph =
  [ mkUnusedItem name UnusedType (dnSymbol node) 0.9
  | (name, node) <- Map.toList (dgNodes graph)
  , name `Set.member` unreachable
  , symbolKind (dnSymbol node) `elem` [TypeConstructor, TypeClass, TypeFamily]
  , not (dnIsThGen node)
  ]

-- | Detect unused imports in a file
detectUnusedImportsInFile :: Set QualifiedName -> HieData -> [UnusedItem]
detectUnusedImportsInFile reachable hie =
  [ UnusedItem
      { uiName = imp
      , uiKind = UnusedImportItem
      , uiSpan = noSrcSpan  -- Would need proper span
      , uiMessage = "Import '" <> qnName imp <> "' is not used"
      , uiConfidence = 0.8
      }
  | imp <- hieImports hie
  , imp `Set.notMember` reachable
  ]

-- | Detect unused exports in a file
detectUnusedExportsInFile :: Set QualifiedName -> HieData -> [UnusedItem]
detectUnusedExportsInFile reachable hie =
  [ UnusedItem
      { uiName = exp
      , uiKind = UnusedExport
      , uiSpan = noSrcSpan
      , uiMessage = "Export '" <> qnName exp <> "' is never imported elsewhere"
      , uiConfidence = 0.6  -- Lower confidence - might be public API
      }
  | exp <- hieExports hie
  , exp `Set.notMember` reachable
  ]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Create an unused item from a symbol
mkUnusedItem :: QualifiedName -> UnusedKind -> Symbol -> Float -> UnusedItem
mkUnusedItem name kind sym confidence = UnusedItem
  { uiName = name
  , uiKind = kind
  , uiSpan = symbolSpan sym
  , uiMessage = mkMessage kind name
  , uiConfidence = confidence
  }

-- | Generate a message for an unused item
mkMessage :: UnusedKind -> QualifiedName -> Text
mkMessage kind name =
  let kindText = case kind of
        UnusedFunction    -> "Function"
        UnusedType        -> "Type"
        UnusedConstructor -> "Constructor"
        UnusedImportItem      -> "Import"
        UnusedExport      -> "Export"
        UnusedTypeClass   -> "Type class"
        UnusedInstance    -> "Instance"
  in kindText <> " '" <> qnName name <> "' appears to be unused"

-- | Convert unused items to diagnostics
unusedToDiagnostic :: UnusedItem -> Diagnostic
unusedToDiagnostic item = Diagnostic
  { diagSpan = uiSpan item
  , diagSeverity = Warning
  , diagKind = UnusedCode
  , diagMessage = uiMessage item
  , diagCode = Just $ "unused/" <> kindCode (uiKind item)
  , diagFixes = []  -- Could add "remove" fix
  , diagRelated = []
  }
  where
    kindCode UnusedFunction    = "function"
    kindCode UnusedType        = "type"
    kindCode UnusedConstructor = "constructor"
    kindCode UnusedImportItem      = "import"
    kindCode UnusedExport      = "export"
    kindCode UnusedTypeClass   = "class"
    kindCode UnusedInstance    = "instance"
