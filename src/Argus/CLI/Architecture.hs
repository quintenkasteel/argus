{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Architecture
-- Description : Architecture command implementation
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module implements the architecture command for analyzing
-- module dependencies, coupling metrics, and architectural violations.
module Argus.CLI.Architecture
  ( runArchitecture
  ) where

import Control.Monad (when, unless, forM)
import Data.List (sortBy, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing, Down(..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeExtension)

import Argus.CLI.Types
import Argus.CLI.Common (mkProgressConfig)
import Argus.Output.Progress qualified as Progress

-- | Simple dependency graph for module analysis
data ModuleGraph = ModuleGraph
  { mgModules     :: Set Text           -- ^ All module names
  , mgDependencies :: Map Text (Set Text) -- ^ Module -> modules it imports
  , mgDependents   :: Map Text (Set Text) -- ^ Module -> modules that import it
  }
  deriving stock (Eq, Show)

-- | Empty module graph
emptyModuleGraph :: ModuleGraph
emptyModuleGraph = ModuleGraph Set.empty Map.empty Map.empty

-- | Add a module with its imports
addModuleWithImports :: Text -> [Text] -> ModuleGraph -> ModuleGraph
addModuleWithImports modName imports graph =
  let importSet = Set.fromList imports
      -- Update modules set
      newModules = Set.insert modName (mgModules graph)
      -- Update dependencies (what this module imports)
      newDeps = Map.insertWith Set.union modName importSet (mgDependencies graph)
      -- Update dependents (what imports this module)
      newDependents = foldr
        (\imp m -> Map.insertWith Set.union imp (Set.singleton modName) m)
        (mgDependents graph)
        imports
  in graph
    { mgModules = newModules
    , mgDependencies = newDeps
    , mgDependents = newDependents
    }

-- | Get all modules
allModules :: ModuleGraph -> [Text]
allModules = Set.toList . mgModules

-- | Get dependencies of a module
dependenciesOf :: ModuleGraph -> Text -> Set Text
dependenciesOf graph modName = Map.findWithDefault Set.empty modName (mgDependencies graph)

-- | Get dependents of a module (modules that import it)
dependentsOf :: ModuleGraph -> Text -> Set Text
dependentsOf graph modName = Map.findWithDefault Set.empty modName (mgDependents graph)

-- | Find cycles in the module graph using DFS
findCycles :: ModuleGraph -> [[Text]]
findCycles graph = nub $ concatMap (findCycleFrom graph) (allModules graph)

findCycleFrom :: ModuleGraph -> Text -> [[Text]]
findCycleFrom graph start = go Set.empty [start]
  where
    go _ [] = []
    go visited (current:path)
      | current == start && length (current:path) > 2 = [reverse path]
      | current `Set.member` visited = []
      | otherwise =
          let deps = Set.toList $ dependenciesOf graph current
              newVisited = Set.insert current visited
          in concatMap (\next -> go newVisited (next:current:path)) deps

-- | Build graph from directories by scanning Haskell files
buildFromDirectories :: [FilePath] -> IO (Either Text ModuleGraph)
buildFromDirectories dirs = do
  files <- concat <$> mapM findHaskellFiles dirs
  graph <- foldM processFile emptyModuleGraph files
  pure $ Right graph
  where
    processFile :: ModuleGraph -> FilePath -> IO ModuleGraph
    processFile graph file = do
      content <- TIO.readFile file
      let modName = extractModuleName content
          imports = extractImports content
      pure $ addModuleWithImports modName imports graph

    foldM f acc [] = pure acc
    foldM f acc (x:xs) = do
      acc' <- f acc x
      foldM f acc' xs

-- | Find all Haskell files in a directory
findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- listDirectory dir
      files <- forM entries $ \entry -> do
        let path = dir </> entry
        isDir <- doesDirectoryExist path
        if isDir && not (isExcluded entry)
          then findHaskellFiles path
          else if isHaskellFile entry
            then pure [path]
            else pure []
      pure $ concat files
  where
    isHaskellFile name = takeExtension name `elem` [".hs", ".lhs"]
    isExcluded name = name `elem` [".git", ".stack-work", "dist-newstyle", ".hie", "node_modules"]

-- | Extract module name from file content
extractModuleName :: Text -> Text
extractModuleName content =
  case T.breakOn "module " content of
    (_, rest) | T.null rest -> "Main"
    (_, rest) ->
      let afterModule = T.drop 7 rest
          modName = T.takeWhile (\c -> c /= ' ' && c /= '(' && c /= '\n') afterModule
      in T.strip modName

-- | Extract imports from file content
extractImports :: Text -> [Text]
extractImports content =
  [ extractModuleFromImport line
  | line <- T.lines content
  , "import " `T.isPrefixOf` T.stripStart line
  ]
  where
    extractModuleFromImport line =
      let stripped = T.stripStart line
          afterImport = T.drop 7 stripped
          -- Handle 'import qualified'
          afterQual = if "qualified " `T.isPrefixOf` afterImport
                      then T.drop 10 afterImport
                      else afterImport
          -- Extract module name
          modName = T.takeWhile (\c -> c /= ' ' && c /= '(' && c /= '\n') afterQual
      in T.strip modName

-- | Generate DOT graph format
toDot :: ModuleGraph -> Text
toDot graph = T.unlines $
  ["digraph modules {", "  rankdir=TB;", "  node [shape=box];"] ++
  [ "  \"" <> from <> "\" -> \"" <> to <> "\";"
  | from <- allModules graph
  , to <- Set.toList (dependenciesOf graph from)
  ] ++
  ["}"]

-- | Run architecture command - analyze module dependencies and metrics
runArchitecture :: GlobalOptions -> ArchitectureOptions -> IO ()
runArchitecture global opts = do
  let targets = if null (aoTargets opts) then ["."] else aoTargets opts
      progressCfg = mkProgressConfig global (aoOutputFormat opts == "terminal")

  when (Progress.pcEnabled progressCfg) $
    Progress.printInfo progressCfg "Analyzing architecture..."

  -- Build dependency graph
  graphResult <- buildFromDirectories targets
  case graphResult of
    Left err -> do
      TIO.putStrLn $ "Error building dependency graph: " <> err
      exitFailure
    Right graph -> do
      case aoOutputFormat opts of
        "dot" -> outputDotGraph opts graph
        "json" -> outputJsonArchitecture opts graph
        _ -> outputTerminalArchitecture opts progressCfg graph

-- | Output DOT graph
outputDotGraph :: ArchitectureOptions -> ModuleGraph -> IO ()
outputDotGraph opts graph = do
  let dot = toDot graph
  case aoGraphOutput opts of
    Just outFile -> do
      TIO.writeFile outFile dot
      TIO.putStrLn $ "DOT graph written to: " <> T.pack outFile
    Nothing -> TIO.putStrLn dot

-- | Output JSON architecture analysis
outputJsonArchitecture :: ArchitectureOptions -> ModuleGraph -> IO ()
outputJsonArchitecture opts graph = do
  let modules = allModules graph
      cycles = findCycles graph
      metrics = map (computeModuleMetrics graph) modules

  TIO.putStrLn "{"
  TIO.putStrLn $ "  \"moduleCount\": " <> T.pack (show $ length modules) <> ","
  TIO.putStrLn $ "  \"cycleCount\": " <> T.pack (show $ length cycles) <> ","

  -- Module list
  TIO.putStrLn "  \"modules\": ["
  let moduleJson m = "    { \"name\": \"" <> m <> "\", \"dependencies\": " <>
        T.pack (show $ Set.size $ dependenciesOf graph m) <> ", \"dependents\": " <>
        T.pack (show $ Set.size $ dependentsOf graph m) <> " }"
  mapM_ (\(i, m) -> TIO.putStrLn $ moduleJson m <> if i < length modules then "," else "") $
    zip [1..] modules
  TIO.putStrLn "  ],"

  -- Cycles
  when (aoShowCycles opts) $ do
    TIO.putStrLn "  \"cycles\": ["
    mapM_ (\(i, c) -> TIO.putStrLn $ "    " <> T.pack (show c) <> if i < length cycles then "," else "") $
      zip [1..] cycles
    TIO.putStrLn "  ],"

  -- Metrics
  when (aoShowMetrics opts) $ do
    TIO.putStrLn "  \"metrics\": {"
    let avgCoupling = if null metrics then 0.0 else sum (map mmCoupling metrics) / fromIntegral (length metrics)
        avgInstability = if null metrics then 0.0 else sum (map mmInstability metrics) / fromIntegral (length metrics)
    TIO.putStrLn $ "    \"averageCoupling\": " <> T.pack (show avgCoupling) <> ","
    TIO.putStrLn $ "    \"averageInstability\": " <> T.pack (show avgInstability)
    TIO.putStrLn "  }"

  TIO.putStrLn "}"

-- | Output terminal architecture analysis
outputTerminalArchitecture :: ArchitectureOptions -> Progress.ProgressConfig -> ModuleGraph -> IO ()
outputTerminalArchitecture opts _progressCfg graph = do
  let modules = allModules graph
      cycles = findCycles graph
      metrics = map (computeModuleMetrics graph) modules

  TIO.putStrLn $ "\n" <> colorBold "ARCHITECTURE ANALYSIS"
  TIO.putStrLn $ T.replicate 60 "="

  -- Summary
  TIO.putStrLn $ "\n" <> colorBold "Summary:"
  TIO.putStrLn $ "  Total modules: " <> T.pack (show $ length modules)
  TIO.putStrLn $ "  Circular dependencies: " <> (if null cycles then colorGreen "0" else colorRed $ T.pack $ show $ length cycles)

  -- Show cycles if any
  when (aoShowCycles opts && not (null cycles)) $ do
    TIO.putStrLn $ "\n" <> colorBold "Circular Dependencies:" <> colorRed " (architectural violation)"
    mapM_ printCycle cycles

  -- Show metrics
  when (aoShowMetrics opts) $ do
    TIO.putStrLn $ "\n" <> colorBold "Module Metrics (top 20 by instability):"
    TIO.putStrLn $ "  " <> padRight 40 "Module" <> padLeft 10 "Coupling" <> padLeft 12 "Instability"
    TIO.putStrLn $ "  " <> T.replicate 62 "-"
    let sortedMetrics = take 20 $ sortBy (comparing (Down . mmInstability)) metrics
    mapM_ printMetricRow sortedMetrics

    -- Averages
    let avgCoupling = if null metrics then 0.0 else sum (map mmCoupling metrics) / fromIntegral (length metrics)
        avgInstability = if null metrics then 0.0 else sum (map mmInstability metrics) / fromIntegral (length metrics)
    TIO.putStrLn $ "\n  Average coupling: " <> T.pack (show (round (avgCoupling * 100) :: Int)) <> "%"
    TIO.putStrLn $ "  Average instability: " <> T.pack (show (round (avgInstability * 100) :: Int)) <> "%"

  -- Show violations
  when (aoShowViolations opts) $ do
    let violations = findViolations graph
    unless (null violations) $ do
      TIO.putStrLn $ "\n" <> colorBold "Layer Violations:"
      mapM_ printViolation violations

  -- Graph output hint
  when (aoShowGraph opts && aoGraphOutput opts == Nothing) $ do
    TIO.putStrLn $ "\n" <> colorYellow "Tip: Use --format dot to generate a DOT graph, or --graph-output FILE to save it"

-- | Module metrics
data ModuleMetrics = ModuleMetrics
  { mmModule :: Text
  , mmCoupling :: Double      -- Afferent + Efferent coupling
  , mmInstability :: Double   -- Ce / (Ca + Ce) where Ca = dependents, Ce = dependencies
  }

-- | Compute metrics for a module
computeModuleMetrics :: ModuleGraph -> Text -> ModuleMetrics
computeModuleMetrics graph modName =
  let deps = Set.size $ dependenciesOf graph modName    -- Efferent (outgoing)
      dependents = Set.size $ dependentsOf graph modName -- Afferent (incoming)
      totalModules = max 1 $ length $ allModules graph
      coupling = fromIntegral (deps + dependents) / fromIntegral totalModules
      instability = if deps + dependents == 0
                    then 0.0
                    else fromIntegral deps / fromIntegral (deps + dependents)
  in ModuleMetrics modName coupling instability

-- | Layer violation type
data LayerViolation = LayerViolation
  { lvFrom :: Text
  , lvTo :: Text
  , lvReason :: Text
  }

-- | Find layer violations (heuristic based on module names)
findViolations :: ModuleGraph -> [LayerViolation]
findViolations graph =
  [ LayerViolation from to "Presentation layer imports from Data layer"
  | from <- allModules graph
  , isPresentation from
  , to <- Set.toList $ dependenciesOf graph from
  , isData to
  ] ++
  [ LayerViolation from to "Data layer imports from Presentation layer"
  | from <- allModules graph
  , isData from
  , to <- Set.toList $ dependenciesOf graph from
  , isPresentation to
  ]
  where
    isPresentation m = any (`T.isInfixOf` m) ["Handler", "View", "Controller", "Web", "API"]
    isData m = any (`T.isInfixOf` m) ["Database", "Repository", "Model", "Entity"]

-- | Print a cycle
printCycle :: [Text] -> IO ()
printCycle cycle' = do
  let cycleText = T.intercalate " → " cycle' <> case cycle' of
        (h:_) -> " → " <> h
        [] -> ""
  TIO.putStrLn $ "  " <> colorRed "⟲" <> " " <> cycleText

-- | Print metric row
printMetricRow :: ModuleMetrics -> IO ()
printMetricRow m = do
  let couplingPct = T.pack $ show (round (mmCoupling m * 100) :: Int) <> "%"
      instabilityPct = T.pack $ show (round (mmInstability m * 100) :: Int) <> "%"
      instColor = if mmInstability m > 0.7 then colorRed else if mmInstability m > 0.5 then colorYellow else colorGreen
  TIO.putStrLn $ "  " <> padRight 40 (T.take 38 $ mmModule m) <>
    padLeft 10 couplingPct <> padLeft 12 (instColor instabilityPct)

-- | Print violation
printViolation :: LayerViolation -> IO ()
printViolation v =
  TIO.putStrLn $ "  " <> colorRed "✗" <> " " <> lvFrom v <> " → " <> lvTo v <>
    "\n    " <> colorYellow (lvReason v)

-- | Pad text to the right
padRight :: Int -> Text -> Text
padRight n t = t <> T.replicate (max 0 (n - T.length t)) " "

-- | Pad text to the left
padLeft :: Int -> Text -> Text
padLeft n t = T.replicate (max 0 (n - T.length t)) " " <> t

-- | ANSI color helpers
colorBold :: Text -> Text
colorBold t = "\ESC[1m" <> t <> "\ESC[0m"

colorRed :: Text -> Text
colorRed t = "\ESC[31m" <> t <> "\ESC[0m"

colorYellow :: Text -> Text
colorYellow t = "\ESC[33m" <> t <> "\ESC[0m"

colorGreen :: Text -> Text
colorGreen t = "\ESC[32m" <> t <> "\ESC[0m"
