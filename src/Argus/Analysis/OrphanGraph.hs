{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Argus.Analysis.OrphanGraph
-- Description : Orphan instance visualization and infection tracking
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides functionality for detecting orphan instances and
-- tracking their "infection paths" through module dependencies. Orphan
-- instances are type class instances defined in modules that don't own
-- either the type class or the data type, which can lead to import ordering
-- issues and unpredictable behavior.
--
-- == Key Features
--
-- * Detect orphan instances from HIE files
-- * Build dependency graphs showing infection spread
-- * Generate DOT visualization files
-- * Track transitive infection paths
-- * Generate actionable diagnostics
module Argus.Analysis.OrphanGraph
  ( -- * Types
    OrphanInstance (..)
  , InfectionPath (..)
  , OrphanGraph (..)
  , ModuleDep (..)
  , OrphanSeverity (..)

    -- * Detection
  , findOrphanInstances
  , isOrphanInstance
  , extractInstances
  , extractInstancesFromHieDb

    -- * Graph Building
  , buildOrphanGraph
  , getInfectionPaths
  , findInfectedModules
  , emptyOrphanGraph
  , addOrphan
  , addModuleDep

    -- * DOT Export
  , orphanGraphToDot
  , formatOrphanNode
  , formatInfectionEdge

    -- * Diagnostics
  , orphansToDiagnostics
  , infectionPathToDiagnostic
  , severityForOrphan
  ) where

import Control.Exception (try, SomeException)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- GHC imports for HIE file analysis
import "ghc" GHC.Iface.Ext.Binary (readHieFile, hie_file_result)
import "ghc" GHC.Iface.Ext.Types
  ( HieFile(..)
  , HieASTs(..)
  , HieAST(..)
  , NodeInfo(..)
  , IdentifierDetails(..)
  , ContextInfo(..)
  , TypeIndex
  , getAsts
  , getSourcedNodeInfo
  )
import "ghc" GHC.Types.Name (occNameString)
import "ghc" GHC.Types.Name.Occurrence (occName)
import "ghc" GHC.Types.SrcLoc qualified as GHC (RealSrcSpan, srcSpanStartLine, srcSpanStartCol, srcSpanEndLine, srcSpanEndCol)
import "ghc" GHC.Unit.Module (moduleNameString, moduleName)

-- HieDb imports
import HieDb (HieDb, HieModuleRow(..))
import HieDb qualified
import HieDb.Utils (makeNc)

import Argus.Types
  ( Diagnostic(..)
  , DiagnosticKind(..)
  , Severity(..)
  , SrcSpan(..)
  , mkSrcSpanRaw
  )

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Severity of an orphan instance
data OrphanSeverity
  = SevCritical  -- ^ Critical: affects exported types or widely used classes
  | SevHigh      -- ^ High: affects multiple modules
  | SevMedium    -- ^ Medium: limited spread
  | SevLow       -- ^ Low: contained to single module
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | An orphan instance in the codebase
data OrphanInstance = OrphanInstance
  { oiClass      :: !Text        -- ^ Type class name (e.g., "Show", "Eq")
  , oiType       :: !Text        -- ^ Type receiving the instance (e.g., "MyType")
  , oiModule     :: !Text        -- ^ Module defining the orphan instance
  , oiLocation   :: !SrcSpan     -- ^ Source location of the instance
  , oiTypeModule :: Maybe Text   -- ^ Module that defines the type (if known)
  , oiClassModule :: Maybe Text  -- ^ Module that defines the class (if known)
  , oiSeverity   :: !OrphanSeverity -- ^ Severity of this orphan
  }
  deriving stock (Eq, Show)

instance Ord OrphanInstance where
  compare = comparing oiModule <> comparing oiClass <> comparing oiType

-- | A path showing how orphan infection spreads through modules
data InfectionPath = InfectionPath
  { ipOrphan    :: !OrphanInstance    -- ^ The orphan causing the infection
  , ipPath      :: ![Text]            -- ^ List of modules in the path (source to infected)
  , ipLength    :: !Int               -- ^ Path length (for sorting)
  , ipTransitive :: !Bool             -- ^ Is this a transitive infection?
  }
  deriving stock (Eq, Show)

instance Ord InfectionPath where
  compare = comparing ipLength <> comparing ipPath

-- | Module dependency relationship
data ModuleDep = ModuleDep
  { mdFrom     :: !Text      -- ^ Importing module
  , mdTo       :: !Text      -- ^ Imported module
  , mdDirect   :: !Bool      -- ^ Direct import (vs transitive)
  , mdLocation :: !SrcSpan   -- ^ Location of import statement
  }
  deriving stock (Eq, Ord, Show)

-- | Graph showing orphan instance infection
data OrphanGraph = OrphanGraph
  { ogOrphans      :: ![OrphanInstance]          -- ^ All orphan instances
  , ogDependencies :: ![ModuleDep]               -- ^ Module dependencies
  , ogInfected     :: !(Map Text [OrphanInstance]) -- ^ Infected modules
  , ogPaths        :: ![InfectionPath]           -- ^ All infection paths
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Detection
--------------------------------------------------------------------------------

-- | Find all orphan instances in a list of HIE files
findOrphanInstances :: [HieFile] -> [OrphanInstance]
findOrphanInstances hieFiles = concatMap extractInstancesFromFile hieFiles

-- | Extract instances from a single HIE file
extractInstancesFromFile :: HieFile -> [OrphanInstance]
extractInstancesFromFile hieFile =
  let modName = T.pack $ moduleNameString $ moduleName $ hie_module hieFile
      asts = Map.elems $ getAsts $ hie_asts hieFile
      instances = concatMap (extractInstancesFromAst modName hieFile) asts
  in filter isOrphanInstance instances

-- | Extract instances from a HIE AST node
extractInstancesFromAst :: Text -> HieFile -> HieAST TypeIndex -> [OrphanInstance]
extractInstancesFromAst moduleName hieFile ast =
  let nodeInf = getSourcedNodeInfo $ sourcedNodeInfo ast
      contexts = concatMap (extractContexts . snd) $ Map.toList nodeInf
      instances = mapMaybe (extractInstanceInfo moduleName hieFile ast) contexts
      childInstances = concatMap (extractInstancesFromAst moduleName hieFile) (nodeChildren ast)
  in instances ++ childInstances

-- | Extract context information from node info
extractContexts :: NodeInfo TypeIndex -> [ContextInfo]
extractContexts nodeInf =
  let identifiers = Map.elems $ nodeIdentifiers nodeInf
      allContexts = concatMap (Set.toList . identInfo) identifiers
  in allContexts

-- | Extract instance information from a context
extractInstanceInfo :: Text -> HieFile -> HieAST TypeIndex -> ContextInfo -> Maybe OrphanInstance
extractInstanceInfo moduleName hieFile ast context = case context of
  ClassTyDecl{} -> do
    -- This is an instance declaration
    let span = nodeSpan ast
        srcSpan = hieSpanToSrcSpan (hie_hs_file hieFile) span

    -- Try to extract class and type from the node
    (className, typeName) <- extractClassAndType hieFile ast

    pure OrphanInstance
      { oiClass = className
      , oiType = typeName
      , oiModule = moduleName
      , oiLocation = srcSpan
      , oiTypeModule = Nothing  -- Would need cross-module analysis
      , oiClassModule = Nothing -- Would need cross-module analysis
      , oiSeverity = SevMedium  -- Default, refined later
      }
  _ -> Nothing

-- | Extract class and type names from an instance declaration node
extractClassAndType :: HieFile -> HieAST TypeIndex -> Maybe (Text, Text)
extractClassAndType _hieFile ast =
  let nodeInf = getSourcedNodeInfo $ sourcedNodeInfo ast
      identifiers = concatMap (Map.toList . nodeIdentifiers . snd) $ Map.toList nodeInf
      names = mapMaybe extractNameFromIdent identifiers
  in case names of
    (className:typeName:_) -> Just (className, typeName)
    _ -> Nothing
  where
    extractNameFromIdent (Right name, _details) =
      Just $ T.pack $ occNameString $ occName name
    extractNameFromIdent _ = Nothing

-- | Convert HIE span to our SrcSpan type
hieSpanToSrcSpan :: FilePath -> GHC.RealSrcSpan -> SrcSpan
hieSpanToSrcSpan file span =
  mkSrcSpanRaw
    file
    (GHC.srcSpanStartLine span)
    (GHC.srcSpanStartCol span)
    (GHC.srcSpanEndLine span)
    (GHC.srcSpanEndCol span)

-- | Check if an instance is orphan
-- An instance is orphan if it's defined in a module that doesn't define
-- either the type class or the data type
isOrphanInstance :: OrphanInstance -> Bool
isOrphanInstance OrphanInstance{..} =
  case (oiTypeModule, oiClassModule) of
    (Just tyMod, Just clsMod) -> oiModule /= tyMod && oiModule /= clsMod
    _ -> True  -- Conservative: assume orphan if we don't have module info

-- | Extract all instances from a list of HIE files (synonym for findOrphanInstances)
extractInstances :: [HieFile] -> [OrphanInstance]
extractInstances = findOrphanInstances

-- | Extract orphan instances from a HieDb database
extractInstancesFromHieDb :: FilePath -> HieDb -> IO [OrphanInstance]
extractInstancesFromHieDb hieDir db = do
  modules <- HieDb.getAllIndexedMods db
  instances <- concat <$> mapM (loadModuleInstances hieDir) modules
  pure $ filter isOrphanInstance instances

-- | Load instances from a single module
loadModuleInstances :: FilePath -> HieModuleRow -> IO [OrphanInstance]
loadModuleInstances hieDir modRow = do
  let hieFile = hieModuleHieFile modRow
  result <- try @SomeException $ do
    nc <- makeNc
    hieResult <- readHieFile nc hieFile
    pure $ extractInstancesFromFile (hie_file_result hieResult)

  case result of
    Left _err -> pure []
    Right instances -> pure instances

--------------------------------------------------------------------------------
-- Graph Building
--------------------------------------------------------------------------------

-- | Empty orphan graph
emptyOrphanGraph :: OrphanGraph
emptyOrphanGraph = OrphanGraph
  { ogOrphans = []
  , ogDependencies = []
  , ogInfected = Map.empty
  , ogPaths = []
  }

-- | Add an orphan instance to the graph
addOrphan :: OrphanInstance -> OrphanGraph -> OrphanGraph
addOrphan orphan graph = graph { ogOrphans = orphan : ogOrphans graph }

-- | Add a module dependency to the graph
addModuleDep :: ModuleDep -> OrphanGraph -> OrphanGraph
addModuleDep dep graph = graph { ogDependencies = dep : ogDependencies graph }

-- | Build an orphan graph from instances and module dependencies
buildOrphanGraph :: [OrphanInstance] -> [ModuleDep] -> OrphanGraph
buildOrphanGraph orphans deps =
  let graph = OrphanGraph
        { ogOrphans = orphans
        , ogDependencies = deps
        , ogInfected = Map.empty
        , ogPaths = []
        }
      -- Compute infected modules
      infected = computeInfected orphans deps
      -- Compute all infection paths
      paths = concatMap (\o -> computeInfectionPaths o deps) orphans
  in graph
      { ogInfected = infected
      , ogPaths = paths
      }

-- | Compute which modules are infected by orphans
computeInfected :: [OrphanInstance] -> [ModuleDep] -> Map Text [OrphanInstance]
computeInfected orphans deps =
  let orphansByModule = groupBy oiModule orphans
      allModules = nub $ map mdFrom deps ++ map mdTo deps
      infected = Map.fromList
        [ (mod, findInfectingOrphans mod orphans deps)
        | mod <- allModules
        ]
  in Map.filter (not . null) infected

-- | Group items by a key function
groupBy :: Ord k => (a -> k) -> [a] -> Map k [a]
groupBy f items = Map.fromListWith (++) [(f item, [item]) | item <- items]

-- | Find which orphans infect a given module
findInfectingOrphans :: Text -> [OrphanInstance] -> [ModuleDep] -> [OrphanInstance]
findInfectingOrphans targetModule orphans deps =
  let reachableModules = findReachableModules targetModule deps
      orphansInReachable = filter (\o -> oiModule o `Set.member` reachableModules) orphans
  in orphansInReachable

-- | Find all modules reachable from a target module
findReachableModules :: Text -> [ModuleDep] -> Set Text
findReachableModules start deps = go Set.empty [start]
  where
    go visited [] = visited
    go visited (m:ms)
      | m `Set.member` visited = go visited ms
      | otherwise =
          let neighbors = [mdTo d | d <- deps, mdFrom d == m]
          in go (Set.insert m visited) (neighbors ++ ms)

-- | Compute infection paths for an orphan instance
computeInfectionPaths :: OrphanInstance -> [ModuleDep] -> [InfectionPath]
computeInfectionPaths orphan deps =
  let source = oiModule orphan
      allModules = nub $ map mdFrom deps ++ map mdTo deps
      paths = mapMaybe (findPath source deps) allModules
      infectionPaths = map (toInfectionPath orphan) paths
  in infectionPaths

-- | Find a path from source to target module
-- For orphan infection, we follow reverse dependency direction:
-- if B imports A, then orphans in A infect B. So from A, we can reach B.
findPath :: Text -> [ModuleDep] -> Text -> Maybe [Text]
findPath source deps target
  | source == target = Just [source]
  | otherwise = go Set.empty [[source]]
  where
    go _ [] = Nothing
    go visited (path:paths)
      | current `Set.member` visited = go visited paths
      | current == target = Just (reverse path)
      | otherwise =
          -- Follow reverse dependency direction: find modules that import current
          -- (i.e., where mdTo == current, the neighbor is mdFrom)
          let neighbors = [mdFrom d | d <- deps, mdTo d == current]
              newPaths = [n:path | n <- neighbors]
          in go (Set.insert current visited) (paths ++ newPaths)
      where
        current = head path

-- | Convert a module path to an infection path
toInfectionPath :: OrphanInstance -> [Text] -> InfectionPath
toInfectionPath orphan path = InfectionPath
  { ipOrphan = orphan
  , ipPath = path
  , ipLength = length path
  , ipTransitive = length path > 2
  }

-- | Get all infection paths for a specific orphan instance
getInfectionPaths :: OrphanInstance -> OrphanGraph -> [InfectionPath]
getInfectionPaths orphan graph =
  filter (\p -> ipOrphan p == orphan) (ogPaths graph)

-- | Find all modules infected by orphans
findInfectedModules :: OrphanGraph -> Set Text
findInfectedModules graph = Map.keysSet (ogInfected graph)

--------------------------------------------------------------------------------
-- DOT Export
--------------------------------------------------------------------------------

-- | Convert an orphan graph to DOT format for visualization
orphanGraphToDot :: OrphanGraph -> Text
orphanGraphToDot graph =
  let header = "digraph OrphanGraph {"
      footer = "}"

      -- Graph attributes
      graphAttrs = "  graph [rankdir=LR, splines=ortho];"
      nodeAttrs = "  node [shape=box, style=rounded];"
      edgeAttrs = "  edge [fontsize=10];"

      -- Orphan nodes (modules containing orphans)
      orphanModules = nub $ map oiModule (ogOrphans graph)
      orphanNodes = map formatOrphanNode orphanModules

      -- Infected modules (different color)
      infectedModules = Set.toList $ findInfectedModules graph
      infectedNodes = map formatInfectedNode infectedModules

      -- Dependencies as edges
      depEdges = map formatDepEdge (ogDependencies graph)

      -- Infection edges (highlight orphan spread)
      infectionEdges = concatMap formatInfectionPath (take 20 $ ogPaths graph)

      -- Instance details as labels
      instanceLabels = concatMap formatInstanceLabel (ogOrphans graph)

      allLines = concat
        [ [header]
        , ["  " <> graphAttrs, "  " <> nodeAttrs, "  " <> edgeAttrs]
        , [""]
        , ["  // Orphan-defining modules"]
        , map ("  " <>) orphanNodes
        , [""]
        , ["  // Infected modules"]
        , map ("  " <>) infectedNodes
        , [""]
        , ["  // Module dependencies"]
        , map ("  " <>) depEdges
        , [""]
        , ["  // Infection paths"]
        , map ("  " <>) infectionEdges
        , [""]
        , ["  // Instance details"]
        , map ("  " <>) instanceLabels
        , [footer]
        ]
  in T.unlines allLines

-- | Format an orphan node (module with orphans)
formatOrphanNode :: Text -> Text
formatOrphanNode modName =
  let safeId = sanitizeId modName
  in safeId <> " [label=\"" <> modName <> "\", fillcolor=red, style=\"filled,rounded\"];"

-- | Format an infected node (module importing orphans)
formatInfectedNode :: Text -> Text
formatInfectedNode modName =
  let safeId = sanitizeId modName
  in safeId <> " [label=\"" <> modName <> "\", fillcolor=orange, style=\"filled,rounded\"];"

-- | Format a dependency edge
formatDepEdge :: ModuleDep -> Text
formatDepEdge ModuleDep{..} =
  let fromId = sanitizeId mdFrom
      toId = sanitizeId mdTo
      style = if mdDirect then "" else ", style=dashed"
  in fromId <> " -> " <> toId <> " [color=gray" <> style <> "];"

-- | Format infection edges for a path
formatInfectionPath :: InfectionPath -> [Text]
formatInfectionPath InfectionPath{..} =
  let orphanLabel = oiClass (ipOrphan) <> " " <> oiType (ipOrphan)
      pairs = zip ipPath (tail ipPath)
  in map (\(from, to) -> formatInfectionEdge from to orphanLabel) pairs

-- | Format an infection edge
formatInfectionEdge :: Text -> Text -> Text -> Text
formatInfectionEdge from to label =
  let fromId = sanitizeId from
      toId = sanitizeId to
  in fromId <> " -> " <> toId <> " [color=red, label=\"" <> label <> "\", penwidth=2];"

-- | Format instance label (as a separate node with instance details)
formatInstanceLabel :: OrphanInstance -> [Text]
formatInstanceLabel OrphanInstance{..} =
  let instanceId = sanitizeId (oiModule <> "_" <> oiClass <> "_" <> oiType)
      label = "instance " <> oiClass <> " " <> oiType
      moduleId = sanitizeId oiModule
  in [ instanceId <> " [label=\"" <> label <> "\", shape=note, fillcolor=lightyellow, style=filled];"
     , moduleId <> " -> " <> instanceId <> " [style=dotted, arrowhead=none];"
     ]

-- | Sanitize a module name for use as DOT identifier
sanitizeId :: Text -> Text
sanitizeId = T.replace "." "_" . T.replace "-" "_"

--------------------------------------------------------------------------------
-- Diagnostics
--------------------------------------------------------------------------------

-- | Convert orphan instances to diagnostics
orphansToDiagnostics :: [OrphanInstance] -> [Diagnostic]
orphansToDiagnostics orphans = map orphanToDiagnostic orphans

-- | Convert a single orphan instance to a diagnostic
orphanToDiagnostic :: OrphanInstance -> Diagnostic
orphanToDiagnostic orphan@OrphanInstance{..} =
  let severity = severityForOrphan orphan
      message = "Orphan instance: instance " <> oiClass <> " " <> oiType
      explanation = mkExplanation orphan
  in Diagnostic
      { diagSpan = oiLocation
      , diagSeverity = severity
      , diagKind = ArchitecturalIssue
      , diagMessage = message <> "\n" <> explanation
      , diagCode = Just "orphan-instance"
      , diagFixes = []
      , diagRelated = []
      }

-- | Create an explanation for an orphan instance
mkExplanation :: OrphanInstance -> Text
mkExplanation OrphanInstance{..} =
  let typeModInfo = case oiTypeModule of
        Just m -> "Type " <> oiType <> " is defined in " <> m
        Nothing -> "Type " <> oiType <> " module unknown"
      classModInfo = case oiClassModule of
        Just m -> "Class " <> oiClass <> " is defined in " <> m
        Nothing -> "Class " <> oiClass <> " module unknown"
  in "This instance is orphan because it's defined in " <> oiModule <>
     ", but:\n  - " <> typeModInfo <> "\n  - " <> classModInfo <>
     "\n\nOrphan instances can cause import order issues and should be avoided."

-- | Determine severity for an orphan instance
severityForOrphan :: OrphanInstance -> Severity
severityForOrphan OrphanInstance{..} = case oiSeverity of
  SevCritical -> Error
  SevHigh     -> Warning
  SevMedium   -> Warning
  SevLow      -> Suggestion

-- | Convert an infection path to a diagnostic
infectionPathToDiagnostic :: InfectionPath -> Diagnostic
infectionPathToDiagnostic InfectionPath{..} =
  let orphan = ipOrphan
      message = "Orphan infection: " <> oiClass orphan <> " " <> oiType orphan
      pathText = T.intercalate " -> " ipPath
      explanation = "This orphan instance infects modules through the import chain:\n  " <> pathText
  in Diagnostic
      { diagSpan = oiLocation orphan
      , diagSeverity = Info
      , diagKind = ArchitecturalIssue
      , diagMessage = message <> "\n" <> explanation
      , diagCode = Just "orphan-infection"
      , diagFixes = []
      , diagRelated = []
      }
