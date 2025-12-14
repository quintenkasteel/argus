{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Analysis.Architecture
-- Description : Module architecture and dependency analysis
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides architecture-level analysis including:
-- - Circular dependency detection
-- - Module coupling metrics
-- - Layer violation detection
-- - Import pattern analysis
module Argus.Analysis.Architecture
  ( -- * Analysis
    analyzeArchitecture
  , ArchitectureReport (..)
  , ModuleInfo (..)
  , ImportInfo (..)
  , CircularDep (..)
  , LayerViolation (..)
  , CouplingMetrics (..)

    -- * Configuration
  , ArchitectureConfig (..)
  , LayerConfig (..)
  , defaultArchitectureConfig

    -- * DOT Output
  , generateDotGraph
  , generateDotLegend
  , DotStyle (..)
  , defaultDotStyle
  , minimalDotStyle
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (sortOn) -- removed groupBy - unused
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import Argus.Types

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Information about a single module
data ModuleInfo = ModuleInfo
  { miName           :: Text              -- ^ Module name
  , miPath           :: FilePath          -- ^ File path
  , miImports        :: [ImportInfo]      -- ^ What it imports
  , miExports        :: [Text]            -- ^ What it exports
  , miIsOrphan       :: Bool              -- ^ Has orphan instances
  , miHasTemplateH   :: Bool              -- ^ Uses Template Haskell
  , miLineCount      :: Int               -- ^ Lines of code
  , miFunctionCount  :: Int               -- ^ Number of functions
  , miTypeCount      :: Int               -- ^ Number of types
  , miLayer          :: Maybe Text        -- ^ Assigned layer (if any)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Import information
data ImportInfo = ImportInfo
  { iiModule     :: Text    -- ^ Imported module
  , iiQualified  :: Bool    -- ^ Is qualified import
  , iiAlias      :: Maybe Text  -- ^ Import alias
  , iiHiding     :: Bool    -- ^ Uses hiding
  , iiSpan       :: SrcSpan -- ^ Location
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A detected circular dependency
data CircularDep = CircularDep
  { cdModules :: [Text]   -- ^ Modules in the cycle
  , cdLength  :: Int      -- ^ Length of cycle
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A layer violation
data LayerViolation = LayerViolation
  { lvFrom       :: Text    -- ^ Importing module
  , lvTo         :: Text    -- ^ Imported module
  , lvFromLayer  :: Text    -- ^ Layer of importer
  , lvToLayer    :: Text    -- ^ Layer of imported
  , lvSpan       :: SrcSpan -- ^ Location of import
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Module coupling metrics
data CouplingMetrics = CouplingMetrics
  { cmModule            :: Text   -- ^ Module name
  , cmAfferentCoupling  :: Int    -- ^ Modules that depend on this (Ca)
  , cmEfferentCoupling  :: Int    -- ^ Modules this depends on (Ce)
  , cmInstability       :: Double -- ^ Ce / (Ca + Ce)
  , cmAbstractness      :: Double -- ^ Abstract types / Total types
  , cmDistance          :: Double -- ^ |A + I - 1| (distance from main sequence)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Full architecture report
data ArchitectureReport = ArchitectureReport
  { arModules        :: [ModuleInfo]       -- ^ All analyzed modules
  , arCircularDeps   :: [CircularDep]      -- ^ Detected cycles
  , arLayerViolations:: [LayerViolation]   -- ^ Layer violations
  , arCouplingMetrics:: [CouplingMetrics]  -- ^ Coupling for each module
  , arDiagnostics    :: [Diagnostic]       -- ^ Generated diagnostics
  , arTotalModules   :: Int                -- ^ Total module count
  , arTotalDeps      :: Int                -- ^ Total dependency edges
  , arMaxDepth       :: Int                -- ^ Maximum import depth
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Layer configuration for architectural boundaries
data LayerConfig = LayerConfig
  { lcName     :: Text      -- ^ Layer name
  , lcPatterns :: [Text]    -- ^ Module patterns for this layer
  , lcCanImport:: [Text]    -- ^ Layers this can import from
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Architecture analysis configuration
data ArchitectureConfig = ArchitectureConfig
  { acEnabled            :: Bool           -- ^ Enable analysis
  , acLayers             :: [LayerConfig]  -- ^ Layer definitions
  , acMaxCycleLength     :: Int            -- ^ Max cycle length to report
  , acInstabilityThreshold :: Double       -- ^ Instability warning threshold
  , acCouplingThreshold  :: Int            -- ^ Coupling warning threshold
  , acCheckOrphans       :: Bool           -- ^ Check for orphan instances
  , acCheckQualified     :: Bool           -- ^ Check import qualification
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration with common layers
defaultArchitectureConfig :: ArchitectureConfig
defaultArchitectureConfig = ArchitectureConfig
  { acEnabled = True
  , acLayers =
      [ LayerConfig "Core" ["*.Types", "*.Core"] ["Core"]
      , LayerConfig "Data" ["*.Data.*", "*.Model.*"] ["Core", "Data"]
      , LayerConfig "Service" ["*.Service.*", "*.Logic.*"] ["Core", "Data", "Service"]
      , LayerConfig "API" ["*.API.*", "*.Handler.*"] ["Core", "Data", "Service", "API"]
      , LayerConfig "App" ["Main", "*.App", "*.CLI"] ["Core", "Data", "Service", "API", "App"]
      ]
  , acMaxCycleLength = 10
  , acInstabilityThreshold = 0.8
  , acCouplingThreshold = 15
  , acCheckOrphans = True
  , acCheckQualified = True
  }

--------------------------------------------------------------------------------
-- Analysis
--------------------------------------------------------------------------------

-- | Analyze architecture from parsed module information
analyzeArchitecture :: ArchitectureConfig
                    -> Map Text ModuleInfo  -- ^ Modules keyed by name
                    -> ArchitectureReport
analyzeArchitecture config modules
  | not (acEnabled config) = emptyReport
  | otherwise = ArchitectureReport
      { arModules = Map.elems modules
      , arCircularDeps = findCircularDeps (acMaxCycleLength config) modules
      , arLayerViolations = findLayerViolations (acLayers config) modules
      , arCouplingMetrics = calculateCoupling modules
      , arDiagnostics = generateDiagnostics config modules
      , arTotalModules = Map.size modules
      , arTotalDeps = sum $ map (length . miImports) (Map.elems modules)
      , arMaxDepth = calculateMaxDepth modules
      }

emptyReport :: ArchitectureReport
emptyReport = ArchitectureReport [] [] [] [] [] 0 0 0

--------------------------------------------------------------------------------
-- Circular Dependency Detection
--------------------------------------------------------------------------------

-- | Find all circular dependencies up to given length
findCircularDeps :: Int -> Map Text ModuleInfo -> [CircularDep]
findCircularDeps maxLen modules =
  let graph = buildGraph modules
      cycles = findCycles maxLen graph
  in map (\c -> CircularDep c (length c)) cycles

-- | Build import graph
buildGraph :: Map Text ModuleInfo -> Map Text (Set Text)
buildGraph = Map.map (Set.fromList . map iiModule . miImports)

-- | Find cycles using DFS
findCycles :: Int -> Map Text (Set Text) -> [[Text]]
findCycles maxLen graph =
  let nodes = Map.keys graph
      allCycles = concatMap (findCyclesFrom maxLen graph Set.empty []) nodes
  in deduplicateCycles allCycles

findCyclesFrom :: Int -> Map Text (Set Text) -> Set Text -> [Text] -> Text -> [[Text]]
findCyclesFrom maxLen graph visited path node
  | length path > maxLen = []
  | node `Set.member` visited =
      if node `elem` path
        then [dropWhile (/= node) (path ++ [node])]
        else []
  | otherwise =
      let neighbors = fromMaybe Set.empty $ Map.lookup node graph
          visited' = Set.insert node visited
          path' = path ++ [node]
      in concatMap (findCyclesFrom maxLen graph visited' path') (Set.toList neighbors)

-- | Remove duplicate cycles (rotations and reversals)
deduplicateCycles :: [[Text]] -> [[Text]]
deduplicateCycles = mapMaybe safeHead . groupBy normalizedEq . sortOn normalize
  where
    -- Safe head that returns Nothing for empty lists
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x:_) = Just x
    normalize :: [Text] -> [Text]
    normalize [] = []
    normalize xs = let rotations = take (length xs) $ iterate rotate xs
                   in minimum rotations

    rotate :: [a] -> [a]
    rotate [] = []
    rotate (x:xs) = xs ++ [x]

    normalizedEq a b = normalize a == normalize b

    groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
    groupBy _ [] = []
    groupBy eq (x:xs) =
      let (matching, rest) = span (eq x) xs
      in (x:matching) : groupBy eq rest

--------------------------------------------------------------------------------
-- Layer Violation Detection
--------------------------------------------------------------------------------

-- | Find layer violations based on configuration
findLayerViolations :: [LayerConfig] -> Map Text ModuleInfo -> [LayerViolation]
findLayerViolations layers modules =
  concatMap (checkModuleViolations layers modules) (Map.elems modules)

checkModuleViolations :: [LayerConfig] -> Map Text ModuleInfo -> ModuleInfo -> [LayerViolation]
checkModuleViolations layers _modules modInfo =
  case findLayer layers (miName modInfo) of
    Nothing -> []  -- Module not in any layer
    Just fromLayer ->
      mapMaybe (checkImportViolation layers fromLayer modInfo) (miImports modInfo)

checkImportViolation :: [LayerConfig] -> LayerConfig -> ModuleInfo -> ImportInfo -> Maybe LayerViolation
checkImportViolation layers fromLayer modInfo imp =
  case findLayer layers (iiModule imp) of
    Nothing -> Nothing  -- Imported module not in any layer
    Just toLayer ->
      if lcName toLayer `elem` lcCanImport fromLayer
        then Nothing
        else Just LayerViolation
          { lvFrom = miName modInfo
          , lvTo = iiModule imp
          , lvFromLayer = lcName fromLayer
          , lvToLayer = lcName toLayer
          , lvSpan = iiSpan imp
          }

-- | Find which layer a module belongs to
findLayer :: [LayerConfig] -> Text -> Maybe LayerConfig
findLayer layers modName =
  let matching = filter (\l -> any (matchPattern modName) (lcPatterns l)) layers
  in case matching of
       [] -> Nothing
       (l:_) -> Just l

-- | Simple glob-like pattern matching
matchPattern :: Text -> Text -> Bool
matchPattern name pat
  | "*" `T.isPrefixOf` pat =
      T.isSuffixOf (T.drop 1 pat) name
  | "*" `T.isSuffixOf` pat =
      T.isPrefixOf (T.dropEnd 1 pat) name
  | "*" `T.isInfixOf` pat =
      let (prefix, rest) = T.breakOn "*" pat
          suffix = T.drop 1 rest
      in T.isPrefixOf prefix name && T.isSuffixOf suffix name
  | otherwise = name == pat

--------------------------------------------------------------------------------
-- Coupling Metrics
--------------------------------------------------------------------------------

-- | Calculate coupling metrics for all modules
calculateCoupling :: Map Text ModuleInfo -> [CouplingMetrics]
calculateCoupling modules =
  let -- Build reverse dependency map
      deps = Map.map (Set.fromList . map iiModule . miImports) modules
      reverseDeps = buildReverseDeps deps
  in map (calculateModuleCoupling modules deps reverseDeps) (Map.keys modules)

-- | Build reverse dependency map
buildReverseDeps :: Map Text (Set Text) -> Map Text (Set Text)
buildReverseDeps deps =
  Map.foldrWithKey addReverseDeps Map.empty deps
  where
    addReverseDeps from tos acc =
      Set.foldr (\to -> Map.insertWith Set.union to (Set.singleton from)) acc tos

-- | Calculate metrics for a single module
calculateModuleCoupling :: Map Text ModuleInfo
                        -> Map Text (Set Text)
                        -> Map Text (Set Text)
                        -> Text
                        -> CouplingMetrics
calculateModuleCoupling modules deps reverseDeps modName =
  let ca = Set.size $ fromMaybe Set.empty $ Map.lookup modName reverseDeps
      ce = Set.size $ fromMaybe Set.empty $ Map.lookup modName deps
      instability = if ca + ce == 0 then 0 else fromIntegral ce / fromIntegral (ca + ce)
      abstractness = calculateAbstractness $ Map.lookup modName modules
      distance = abs (abstractness + instability - 1)
  in CouplingMetrics
       { cmModule = modName
       , cmAfferentCoupling = ca
       , cmEfferentCoupling = ce
       , cmInstability = instability
       , cmAbstractness = abstractness
       , cmDistance = distance
       }

-- | Calculate abstractness (ratio of type exports to total exports)
calculateAbstractness :: Maybe ModuleInfo -> Double
calculateAbstractness Nothing = 0
calculateAbstractness (Just modInfo) =
  let exports = miExports modInfo
      typeExports = filter isTypeExport exports
  in if null exports
     then 0
     else fromIntegral (length typeExports) / fromIntegral (length exports)
  where
    isTypeExport name = T.any (== '.') name || startsWithUpper name
    startsWithUpper t = case T.uncons t of
      Just (c, _) -> c >= 'A' && c <= 'Z'
      Nothing -> False

--------------------------------------------------------------------------------
-- Diagnostics Generation
--------------------------------------------------------------------------------

-- | Generate diagnostics from architecture analysis
generateDiagnostics :: ArchitectureConfig -> Map Text ModuleInfo -> [Diagnostic]
generateDiagnostics config modules = concat
  [ circularDepDiagnostics config modules
  , layerViolationDiagnostics config modules
  , couplingDiagnostics config modules
  , orphanDiagnostics config modules
  , importStyleDiagnostics config modules
  ]

circularDepDiagnostics :: ArchitectureConfig -> Map Text ModuleInfo -> [Diagnostic]
circularDepDiagnostics config modules =
  let cycles = findCircularDeps (acMaxCycleLength config) modules
  in map cycleToDiagnostic cycles
  where
    cycleToDiagnostic cd = Diagnostic
      { diagSpan = defaultSpan
      , diagSeverity = Warning
      , diagKind = ArchitecturalIssue
      , diagMessage = "Circular dependency detected: " <> T.intercalate " -> " (cdModules cd)
      , diagCode = Just "architecture/circular-dep"
      , diagFixes = []
      , diagRelated = []
      }

layerViolationDiagnostics :: ArchitectureConfig -> Map Text ModuleInfo -> [Diagnostic]
layerViolationDiagnostics config modules =
  let violations = findLayerViolations (acLayers config) modules
  in map violationToDiagnostic violations
  where
    violationToDiagnostic lv = Diagnostic
      { diagSpan = lvSpan lv
      , diagSeverity = Warning
      , diagKind = ArchitecturalIssue
      , diagMessage = "Layer violation: " <> lvFromLayer lv <> " layer should not import from " <>
                      lvToLayer lv <> " layer (" <> lvFrom lv <> " imports " <> lvTo lv <> ")"
      , diagCode = Just "architecture/layer-violation"
      , diagFixes = []
      , diagRelated = []
      }

couplingDiagnostics :: ArchitectureConfig -> Map Text ModuleInfo -> [Diagnostic]
couplingDiagnostics config modules =
  let metrics = calculateCoupling modules
      highCoupling = filter (\m -> cmEfferentCoupling m > acCouplingThreshold config) metrics
      unstable = filter (\m -> cmInstability m > acInstabilityThreshold config) metrics
  in map couplingToDiagnostic highCoupling ++ map instabilityToDiagnostic unstable
  where
    couplingToDiagnostic cm = Diagnostic
      { diagSpan = defaultSpan
      , diagSeverity = Suggestion
      , diagKind = ArchitecturalIssue
      , diagMessage = "High coupling: " <> cmModule cm <> " depends on " <>
                      T.pack (show (cmEfferentCoupling cm)) <> " modules"
      , diagCode = Just "architecture/high-coupling"
      , diagFixes = []
      , diagRelated = []
      }

    instabilityToDiagnostic cm = Diagnostic
      { diagSpan = defaultSpan
      , diagSeverity = Suggestion
      , diagKind = ArchitecturalIssue
      , diagMessage = "High instability: " <> cmModule cm <> " (I=" <>
                      T.pack (show (cmInstability cm)) <> ")"
      , diagCode = Just "architecture/high-instability"
      , diagFixes = []
      , diagRelated = []
      }

orphanDiagnostics :: ArchitectureConfig -> Map Text ModuleInfo -> [Diagnostic]
orphanDiagnostics config modules
  | not (acCheckOrphans config) = []
  | otherwise =
      let orphanModules = filter miIsOrphan (Map.elems modules)
      in map orphanToDiagnostic orphanModules
  where
    orphanToDiagnostic modInfo = Diagnostic
      { diagSpan = mkSrcSpanRaw (miPath modInfo) 1 1 1 1
      , diagSeverity = Warning
      , diagKind = ArchitecturalIssue
      , diagMessage = "Module " <> miName modInfo <> " contains orphan instances"
      , diagCode = Just "architecture/orphan-instance"
      , diagFixes = []
      , diagRelated = []
      }

importStyleDiagnostics :: ArchitectureConfig -> Map Text ModuleInfo -> [Diagnostic]
importStyleDiagnostics config modules
  | not (acCheckQualified config) = []
  | otherwise =
      concatMap checkImportStyle (Map.elems modules)
  where
    checkImportStyle modInfo =
      let unqualified = filter (not . iiQualified) (miImports modInfo)
          largeImports = filter isLargeModule unqualified
      in map (unqualifiedToDiagnostic modInfo) largeImports

    isLargeModule imp = any (`T.isPrefixOf` iiModule imp)
      ["Data.Map", "Data.Set", "Data.HashMap", "Data.IntMap", "Data.Text", "Data.ByteString"]

    unqualifiedToDiagnostic _modInfo imp = Diagnostic
      { diagSpan = iiSpan imp
      , diagSeverity = Suggestion
      , diagKind = ArchitecturalIssue
      , diagMessage = "Consider using qualified import for " <> iiModule imp <>
                      " to avoid name clashes"
      , diagCode = Just "architecture/prefer-qualified"
      , diagFixes = []
      , diagRelated = []
      }

defaultSpan :: SrcSpan
defaultSpan = mkSrcSpanRaw "<unknown>" 1 1 1 1

--------------------------------------------------------------------------------
-- Max Depth Calculation
--------------------------------------------------------------------------------

-- | Calculate maximum import chain depth
calculateMaxDepth :: Map Text ModuleInfo -> Int
calculateMaxDepth modules =
  let graph = buildGraph modules
      depths = Map.mapWithKey (\k _ -> calculateDepth graph Set.empty k) graph
  in maximum (0 : Map.elems depths)

calculateDepth :: Map Text (Set Text) -> Set Text -> Text -> Int
calculateDepth graph visited node
  | node `Set.member` visited = 0  -- Cycle, stop
  | otherwise =
      let neighbors = fromMaybe Set.empty $ Map.lookup node graph
          visited' = Set.insert node visited
          childDepths = map (calculateDepth graph visited') (Set.toList neighbors)
      in if null childDepths then 0 else 1 + maximum childDepths

--------------------------------------------------------------------------------
-- DOT Graph Output
--------------------------------------------------------------------------------

-- | Style options for DOT output
data DotStyle = DotStyle
  { dsShowCycles      :: Bool   -- ^ Highlight cycles in red
  , dsShowLayers      :: Bool   -- ^ Color nodes by layer
  , dsShowCoupling    :: Bool   -- ^ Size nodes by coupling
  , dsClusterLayers   :: Bool   -- ^ Cluster modules by layer
  , dsShowViolations  :: Bool   -- ^ Highlight layer violations in red
  , dsShowQualified   :: Bool   -- ^ Style qualified imports differently
  , dsRankDirection   :: Text   -- ^ Graph direction (TB, LR, BT, RL)
  , dsNodeShape       :: Text   -- ^ Node shape (box, ellipse, etc.)
  , dsIncludeLegend   :: Bool   -- ^ Include legend subgraph
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default DOT style with all features enabled
defaultDotStyle :: DotStyle
defaultDotStyle = DotStyle
  { dsShowCycles     = True
  , dsShowLayers     = True
  , dsShowCoupling   = True
  , dsClusterLayers  = True
  , dsShowViolations = True
  , dsShowQualified  = True
  , dsRankDirection  = "TB"
  , dsNodeShape      = "box"
  , dsIncludeLegend  = True
  }

-- | Minimal DOT style for simple graphs
minimalDotStyle :: DotStyle
minimalDotStyle = DotStyle
  { dsShowCycles     = False
  , dsShowLayers     = False
  , dsShowCoupling   = False
  , dsClusterLayers  = False
  , dsShowViolations = False
  , dsShowQualified  = False
  , dsRankDirection  = "TB"
  , dsNodeShape      = "box"
  , dsIncludeLegend  = False
  }

-- | Generate DOT graph representation
generateDotGraph :: DotStyle -> ArchitectureReport -> Text
generateDotGraph style report = T.unlines $
  [ "digraph dependencies {"
  , "  rankdir=" <> dsRankDirection style <> ";"
  , "  node [shape=" <> dsNodeShape style <> ", style=filled];"
  , "  edge [arrowhead=vee];"
  , "  compound=true;"
  , "  concentrate=true;"
  , "  splines=ortho;"
  , ""
  ] ++
  [ if dsClusterLayers style then clusterOutput else nodeOutput
  , ""
  , edgeOutput
  ] ++
  (if dsIncludeLegend style then [legendOutput] else []) ++
  [ "}"
  ]
  where
    -- Build violation set for quick lookup
    violationSet :: Set (Text, Text)
    violationSet = Set.fromList
      [(lvFrom lv, lvTo lv) | lv <- arLayerViolations report]

    nodeOutput :: Text
    nodeOutput = T.unlines $ map (formatNode style report) (arModules report)

    clusterOutput :: Text
    clusterOutput = T.unlines $ map formatCluster (groupByLayer (arModules report))

    edgeOutput :: Text
    edgeOutput = T.unlines $ concatMap (formatEdges style report violationSet) (arModules report)

    legendOutput :: Text
    legendOutput = generateDotLegend style

    formatNode :: DotStyle -> ArchitectureReport -> ModuleInfo -> Text
    formatNode st rep modInfo =
      let name = sanitizeName (miName modInfo)
          color = if dsShowLayers st
                  then layerColor (miLayer modInfo)
                  else "lightblue"
          fontSize = if dsShowCoupling st
                     then couplingSize rep (miName modInfo)
                     else "10"
          cycleStyle = if dsShowCycles st && isInCycle (miName modInfo) (arCircularDeps rep)
                       then ", penwidth=3, color=red"
                       else ""
          tooltip = "\"" <> miName modInfo <> "\\n" <>
                    T.pack (show (length (miImports modInfo))) <> " imports\\n" <>
                    T.pack (show (length (miExports modInfo))) <> " exports\""
      in "  " <> name <> " [fillcolor=" <> color <>
         ", fontsize=" <> fontSize <>
         ", tooltip=" <> tooltip <>
         cycleStyle <> "];"

    formatCluster :: (Maybe Text, [ModuleInfo]) -> Text
    formatCluster (layerName, mods) =
      let clusterName = fromMaybe "Other" layerName
          clusterColor = case clusterName of
            "Core"    -> "#e8f5e9"
            "Data"    -> "#e3f2fd"
            "Service" -> "#fff8e1"
            "API"     -> "#fff3e0"
            "App"     -> "#fce4ec"
            _         -> "#f5f5f5"
      in T.unlines
           [ "  subgraph cluster_" <> sanitizeName clusterName <> " {"
           , "    label=\"" <> clusterName <> " Layer\";"
           , "    style=filled;"
           , "    color=\"" <> clusterColor <> "\";"
           , "    fontsize=14;"
           , "    fontname=\"Helvetica-Bold\";"
           , T.unlines $ map (\m -> "    " <> sanitizeName (miName m) <> ";") mods
           , "  }"
           ]

    formatEdges :: DotStyle -> ArchitectureReport -> Set (Text, Text) -> ModuleInfo -> [Text]
    formatEdges st _rep violations modInfo =
      map (formatEdge st violations modInfo) (miImports modInfo)

    formatEdge :: DotStyle -> Set (Text, Text) -> ModuleInfo -> ImportInfo -> Text
    formatEdge st violations modInfo imp =
      let fromName = sanitizeName (miName modInfo)
          toName = sanitizeName (iiModule imp)
          isViolation = dsShowViolations st &&
                        (miName modInfo, iiModule imp) `Set.member` violations
          isQualified = iiQualified imp
          edgeStyle
            | isViolation = " [color=red, penwidth=2, style=bold]"
            | dsShowQualified st && isQualified = " [style=dashed, color=gray50]"
            | otherwise = ""
      in "  " <> fromName <> " -> " <> toName <> edgeStyle <> ";"

    sanitizeName :: Text -> Text
    sanitizeName = T.replace "." "_" . T.replace "-" "_"

    layerColor :: Maybe Text -> Text
    layerColor Nothing = "white"
    layerColor (Just layer) = case layer of
      "Core"    -> "\"#c8e6c9\""
      "Data"    -> "\"#bbdefb\""
      "Service" -> "\"#ffe082\""
      "API"     -> "\"#ffcc80\""
      "App"     -> "\"#f8bbd9\""
      _         -> "white"

    couplingSize :: ArchitectureReport -> Text -> Text
    couplingSize rep modName =
      let metrics = filter (\m -> cmModule m == modName) (arCouplingMetrics rep)
      in case metrics of
           [] -> "10"
           (m:_) -> T.pack $ show $ min 24 (10 + cmEfferentCoupling m)

    isInCycle :: Text -> [CircularDep] -> Bool
    isInCycle modName cycles = any (elem modName . cdModules) cycles

    groupByLayer :: [ModuleInfo] -> [(Maybe Text, [ModuleInfo])]
    groupByLayer mods =
      let grouped = foldr (\m acc -> Map.insertWith (++) (miLayer m) [m] acc) Map.empty mods
      in Map.toList grouped

-- | Generate a DOT legend subgraph
generateDotLegend :: DotStyle -> Text
generateDotLegend style = T.unlines
  [ "  subgraph cluster_legend {"
  , "    label=\"Legend\";"
  , "    fontsize=12;"
  , "    fontname=\"Helvetica-Bold\";"
  , "    style=filled;"
  , "    color=\"#fafafa\";"
  , "    node [shape=plaintext, style=\"\"];"
  , ""
  , "    legend_title [label=\"Module Dependency Graph\", fontsize=14];"
  , ""
  , if dsShowLayers style
    then T.unlines
      [ "    legend_layers [label=<<table border=\"0\" cellspacing=\"4\">"
      , "      <tr><td bgcolor=\"#c8e6c9\">Core Layer</td></tr>"
      , "      <tr><td bgcolor=\"#bbdefb\">Data Layer</td></tr>"
      , "      <tr><td bgcolor=\"#ffe082\">Service Layer</td></tr>"
      , "      <tr><td bgcolor=\"#ffcc80\">API Layer</td></tr>"
      , "      <tr><td bgcolor=\"#f8bbd9\">App Layer</td></tr>"
      , "    </table>>];"
      ]
    else ""
  , ""
  , if dsShowCycles style || dsShowViolations style
    then T.unlines
      [ "    legend_edges [label=<<table border=\"0\" cellspacing=\"4\">"
      , if dsShowCycles style
        then "      <tr><td><font color=\"red\">Red border = In cycle</font></td></tr>"
        else ""
      , if dsShowViolations style
        then "      <tr><td><font color=\"red\">Red edge = Layer violation</font></td></tr>"
        else ""
      , if dsShowQualified style
        then "      <tr><td><font color=\"gray50\">Dashed = Qualified import</font></td></tr>"
        else ""
      , "    </table>>];"
      ]
    else ""
  , "  }"
  ]
