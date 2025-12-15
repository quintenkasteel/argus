{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Core
-- Description : Main orchestration for the Argus static analyzer
-- Copyright   : (c) 2024
-- License     : MIT
--
-- = Overview
--
-- This module provides the main orchestration for Argus, coordinating
-- parsing, analysis, rule application, and output. It is the primary
-- entry point for programmatic use of Argus.
--
-- = Architecture
--
-- @
-- ┌─────────────────────────────────────────────────────────────────────┐
-- │                        runArgus Pipeline                            │
-- │                                                                     │
-- │  Options ──► Config ──► File Discovery ──► Parallel Analysis       │
-- │                               │                   │                 │
-- │                               ▼                   ▼                 │
-- │                         [FilePath]        ┌─────────────────┐      │
-- │                                           │  analyzeFile    │      │
-- │                                           │                 │      │
-- │                                           │ • Parse         │      │
-- │                                           │ • Syntactic     │      │
-- │                                           │ • Semantic      │      │
-- │                                           │ • Rules         │      │
-- │                                           └────────┬────────┘      │
-- │                                                    │               │
-- │                                                    ▼               │
-- │                                              ArgusResult           │
-- └─────────────────────────────────────────────────────────────────────┘
-- @
--
-- = Analysis Modes
--
-- * 'QuickMode' - Syntactic analysis only (fastest)
-- * 'FullMode' - Includes HIE-based semantic analysis
-- * 'PluginMode' - Uses GHC plugin for Template Haskell
--
-- = Usage
--
-- @
-- -- Simple analysis
-- result <- runArgus defaultOptions { optTargetPaths = ["src"] }
-- print (resultDiagCount result)
--
-- -- Single file analysis
-- diags <- runArgusOnFile "src/Foo.hs"
--
-- -- Custom context
-- let ctx = defaultContext { acConfig = customConfig }
-- diags <- analyzeFile ctx "src/Foo.hs"
-- @
--
-- @since 1.0.0
module Argus.Core
  ( -- * Main entry points
    runArgus
  , runArgusOnFile
  , runArgusOnFiles

    -- * Analysis functions
  , analyzeFile
  , analyzeSource
  , analyzeFiles
  , AnalysisContext (..)
  , defaultContext

    -- * Analysis input types
  , AnalysisInput (..)

    -- * File discovery
  , findHaskellFiles
  , filterExcluded
  , matchGlobPattern

    -- * Name extraction (for import checking)
  , extractUsedNames
  , extractIdentifiers
  , extractOperators
  , extractModuleName

    -- * Legacy compatibility
  , checkFile
  , checkFiles
  ) where

import Control.Monad (forM, forM_, when)
import Data.ByteString qualified as BS
import Data.List (find, isSuffixOf)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T  -- needed for T.unpack and other T.* functions
import Data.Text.Encoding qualified as TE
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.Directory qualified
import System.FilePath ((</>), takeExtension, splitPath, joinPath)

import Argus.Analysis.DepGraph
import Argus.Analysis.Semantic
import Argus.Analysis.Syntactic
import Argus.Analysis.TemplateHaskell
import Argus.Analysis.Unused
import Argus.Config
import Argus.HIE.IncrementalLoader
  ( IncrementalLoader
  , LoaderConfig(..)
  , defaultLoaderConfig
  , initIncrementalLoader
  , closeIncrementalLoader
  )
import Argus.Refactor.Engine
import Argus.Refactor.ExactPrint
import Argus.Rules.ConfigurableRules
import Argus.Rules.Extensions
import Argus.Rules.Imports
import Argus.Rules.Modernize
import Argus.Rules.Naming
import Argus.Rules.Patterns
import Argus.Rules.Performance
import Argus.Rules.Redundant
import Argus.Rules.Security
import Argus.Rules.SpaceLeaks
import Argus.Suppression (parseSuppressions, applySuppressions)
import Argus.Types
import Argus.Utils (isIdentChar, isIdentStart, isOperatorChar)
import Argus.Internal.DList qualified as DL

--------------------------------------------------------------------------------
-- Analysis Context
--------------------------------------------------------------------------------

-- | Context for analysis.
--
-- Carries configuration and shared state through the analysis pipeline.
-- Use 'defaultContext' to create a context with sensible defaults.
--
-- = Fields
--
-- [@acConfig@]: Loaded configuration from argus.toml
-- [@acOptions@]: Command-line options (overrides config)
-- [@acHieData@]: Pre-loaded HIE data for semantic analysis
-- [@acDepGraph@]: Module dependency graph for cross-module analysis
-- [@acRulesConfig@]: Configuration for custom/configurable rules
-- [@acHieLoader@]: Incremental HIE loader for cached queries
--
-- @since 1.0.0
data AnalysisContext = AnalysisContext
  { acConfig      :: Config
    -- ^ Project configuration
  , acOptions     :: ArgusOptions
    -- ^ Runtime options
  , acHieData     :: [HieData]
    -- ^ Pre-loaded HIE data (for full mode)
  , acDepGraph    :: Maybe DepGraph
    -- ^ Pre-built dependency graph
  , acRulesConfig :: RulesConfig
    -- ^ Configurable rules configuration
  , acHieLoader   :: Maybe IncrementalLoader
    -- ^ HIE incremental loader (for cached queries)
  }

-- | Input to the analysis pipeline - either a file path or source text
data AnalysisInput
  = FileInput FilePath            -- ^ Read source from file
  | SourceInput Text FilePath     -- ^ Source text with virtual file path
  deriving stock (Eq, Show)

-- | Create a default analysis context
defaultContext :: Config -> ArgusOptions -> RulesConfig -> AnalysisContext
defaultContext cfg opts rules = AnalysisContext
  { acConfig = cfg
  , acOptions = opts
  , acHieData = []
  , acDepGraph = Nothing
  , acRulesConfig = rules
  , acHieLoader = Nothing
  }

--------------------------------------------------------------------------------
-- Main Entry Points
--------------------------------------------------------------------------------

-- | Run Argus with given options
runArgus :: ArgusOptions -> IO AnalysisResult
runArgus opts = do
  -- Load configuration
  config <- loadConfig (optConfigFile opts)

  -- Load rules configuration (layered: default + project + user + explicit)
  rulesConfig <- loadRulesConfig (optConfigFile opts)

  -- Find all Haskell files, excluding patterns from config
  let excludePatterns = map T.unpack $ genExclude (cfgGeneral config)
  allFiles <- findHaskellFiles (optTargetPaths opts)
  let files = filterExcluded excludePatterns allFiles

  -- Create analysis context (with optional HIE loader)
  (ctx, mLoader) <- case optMode opts of
    QuickMode -> pure (defaultContext config opts rulesConfig, Nothing)
    FullMode -> do
      case optHieDir opts of
        Just dir -> do
          -- Load HIE data and build dependency graph
          hies <- loadHieFiles dir
          graph <- buildGraphFromHieDb dir

          -- Initialize incremental HIE loader for cached queries
          let cacheDir = dir </> ".argus-hie-cache"
              loaderConfig = defaultLoaderConfig
                { lcHieDbPath = Just (dir </> ".hiedb")
                , lcHieDir = Just dir
                , lcEnabled = True
                , lcLazyLoading = True
                , lcVerbose = False
                }
          loader <- initIncrementalLoader cacheDir loaderConfig

          let ctx' = AnalysisContext
                { acConfig = config
                , acOptions = opts
                , acHieData = hies
                , acDepGraph = Just graph
                , acRulesConfig = rulesConfig
                , acHieLoader = Just loader
                }
          pure (ctx', Just loader)
        Nothing -> pure (defaultContext config opts rulesConfig, Nothing)
    PluginMode -> do
      -- Plugin mode runs as a GHC plugin during compilation, not through this CLI.
      -- When invoked directly, fall back to QuickMode for convenience.
      -- Users should use: ghc -fplugin=Linter.Plugin for true plugin mode.
      pure (defaultContext config opts rulesConfig, Nothing)

  -- Run analysis
  results <- analyzeFiles ctx files

  -- Apply fixes if requested
  when (optApplyFixes opts) $ do
    let refactorOpts = RefactorOptions
          { roSafeOnly = True
          , roPreview = optPreview opts
          , roBackup = True
          , roInteractive = optInteractive opts
          }
    forM_ (Map.toList $ resultFiles results) $ \(path, fr) ->
      refactorFile refactorOpts path (fileResultDiagnostics fr)

  -- Close HIE loader to persist cache
  forM_ mLoader closeIncrementalLoader

  pure results

-- | Run linter on a single file
runArgusOnFile :: AnalysisContext -> FilePath -> IO FileResult
runArgusOnFile ctx path = analyzeFile ctx path

-- | Run linter on multiple files
runArgusOnFiles :: AnalysisContext -> [FilePath] -> IO AnalysisResult
runArgusOnFiles = analyzeFiles

--------------------------------------------------------------------------------
-- Analysis Functions
--------------------------------------------------------------------------------

-- | Analyze multiple files
analyzeFiles :: AnalysisContext -> [FilePath] -> IO AnalysisResult
analyzeFiles ctx files = do
  results <- forM files $ \f -> do
    fr <- analyzeFile ctx f
    pure (f, fr)

  let fileMap = Map.fromList results

      -- Unused code detection (for full mode) - convert to diagnostics
      (unusedDiags, unusedNames) = case acDepGraph ctx of
        Just graph ->
          let result = detectUnused (toUnusedConfig $ cfgUnused $ acConfig ctx) graph (acHieData ctx)
              diags = map unusedToDiagnostic (urItems result)
          in (diags, urUnreachable result)
        Nothing -> ([], Set.empty)

      -- Group unused diagnostics by file path
      -- Normalize paths by stripping common prefixes or using basename matching
      unusedByFile = Map.fromListWith (++)
        [ (srcSpanFile (diagSpan d), [d]) | d <- unusedDiags ]

      -- Merge unused diagnostics into file results
      -- Match by filename suffix since HIE paths are relative to project root
      fileMapWithUnused = Map.mapWithKey mergeUnused fileMap
        where
          mergeUnused path fr =
            -- Try exact match first, then suffix match
            let exactDiags = Map.findWithDefault [] path unusedByFile
                -- Try suffix matching: if path ends with the HIE path
                suffixDiags = concat [ diags
                                     | (hiePath, diags) <- Map.toList unusedByFile
                                     , not (null hiePath)
                                     , path `endsWith` hiePath
                                     ]
                extraDiags = if null exactDiags then suffixDiags else exactDiags
            in fr { fileResultDiagnostics = fileResultDiagnostics fr ++ extraDiags }
          -- Check if path ends with suffix (e.g., "test-project/src/Lib.hs" ends with "src/Lib.hs")
          endsWith fullPath suffix = ("/" ++ suffix) `isSuffixOf` fullPath || suffix == fullPath

      -- Count all diagnostics by severity (including unused)
      allDiags = concatMap fileResultDiagnostics (Map.elems fileMapWithUnused)
      sevCounts = foldr countSeverity Map.empty allDiags

  pure AnalysisResult
    { resultFiles = fileMapWithUnused
    , resultUnusedCode = unusedNames  -- Keep for backwards compatibility
    , resultDiagCount = sevCounts
    }
  where
    countSeverity d = Map.insertWith (+) (diagSeverity d) 1

    toUnusedConfig uc = Argus.Analysis.Unused.UnusedConfig
      { ucRoots = unusedRoots uc
      , ucThRoots = unusedThRoots uc
      , ucCheckFunctions = unusedCheckFunctions uc
      , ucCheckTypes = unusedCheckTypes uc
      , ucCheckImports = unusedCheckImports uc
      , ucCheckExports = unusedCheckExports uc
      , ucCheckConstructors = unusedCheckConstructors uc
      , ucCheckRecordFields = unusedCheckRecordFields uc
      , ucCheckLocalBinds = unusedCheckLocalBinds uc
      , ucCheckInstances = unusedCheckInstances uc
      , ucTypeClassRoots = unusedTypeClassRoots uc
      , ucDeriveRoots = unusedDeriveRoots uc
      , ucMinConfidence = realToFrac (unusedMinConfidence uc)
      , ucGranularity = Argus.Analysis.Unused.defaultGranularity
      , ucRequireSourceFiles = True
      , ucIgnorePatterns = unusedIgnorePatterns uc
      , ucConfidenceOverrides = Map.empty
      }

-- | Analyze a single file
analyzeFile :: AnalysisContext -> FilePath -> IO FileResult
analyzeFile ctx path = runAnalysisPipeline ctx (FileInput path)

-- | Analyze source code directly from Text content
-- This is useful for LSP integration where content is provided by the client
-- rather than read from disk.
analyzeSource :: AnalysisContext -> FilePath -> Text -> IO [Diagnostic]
analyzeSource ctx path source = do
  result <- runAnalysisPipeline ctx (SourceInput source path)
  pure $ fileResultDiagnostics result

--------------------------------------------------------------------------------
-- Internal Analysis Pipeline
--------------------------------------------------------------------------------

-- | Internal result from parsing (either from file or source)
data ParsedInput = ParsedInput
  { _piPath        :: FilePath           -- ^ File path (real or virtual)
  , _piSource      :: Text               -- ^ Source content
  , _piParseResult :: Either ParseError ParseResult  -- ^ Parse result
  }

-- | Read and parse input based on AnalysisInput type
readAndParse :: AnalysisInput -> IO ParsedInput
readAndParse (FileInput path) = do
  content <- TE.decodeUtf8 <$> BS.readFile path
  parseResult <- parseFile path
  -- For FileInput, use the source from ParseResult if available
  let source = either (const content) prSource parseResult
  pure $ ParsedInput path source parseResult
readAndParse (SourceInput source path) = do
  parseResult <- parseModule path source
  pure $ ParsedInput path source parseResult

-- | Unified analysis pipeline for both file and source inputs
-- This is the shared implementation that eliminates duplication between
-- analyzeFile and analyzeSource.
runAnalysisPipeline :: AnalysisContext -> AnalysisInput -> IO FileResult
runAnalysisPipeline ctx input = do
  -- Read and parse the input
  ParsedInput path source parseResult <- readAndParse input

  case parseResult of
    Left err -> pure FileResult
      { fileResultPath = path
      , fileResultDiagnostics = [parseErrorToDiag err]
      , fileResultSymbols = []
      , fileResultImports = []
      , fileResultExports = []
      }

    Right pr -> do
      let config = acConfig ctx
          importsConfig = cfgImports config
          m = prModule pr

          -- Extract information
          functions = extractFunctions path source m
          imports = extractImports path m
          exports = extractExports path m
          types = extractTypes path m
          thSplices = extractThSplices path m

          -- Run naming convention checks
          namingDiags = checkNamingConventions (cfgNaming config) path source functions

          -- Run pattern checks
          patternDiags = checkPatterns (cfgPatterns config) path source functions

          -- Run performance checks (with auto-fixes)
          perfDiags = detectPerformanceIssues defaultPerformanceConfig path source

          -- Run space leak checks (with auto-fixes)
          spaceLeakDiags = detectSpaceLeaks defaultSpaceLeakConfig path source

          -- Run modernization checks (with auto-fixes)
          modernizeDiags = detectModernize defaultModernizeConfig path source

          -- Run security checks (with auto-fixes for debug code)
          securityDiags = detectSecurityIssues defaultSecurityConfig path source

          -- Run extension checks (with auto-fixes for missing pragmas)
          extensionDiags = detectExtensionIssues defaultExtensionConfig path source

          -- Run configurable rules (pattern-based rules from config)
          rulesConfig = acRulesConfig ctx
          moduleName = extractModuleName source
          configurableRulesDiags = applyConfigurableRules rulesConfig path moduleName source
          restrictionDiags = checkRestrictions rulesConfig path moduleName source

          -- Extract used names from the entire source (excluding import section)
          -- This catches all uses including class method signatures, type declarations, etc.
          sourceUsedNames = extractUsedNamesFromSource source imports

      -- Analyze Template Haskell usage and find HIE file if available
      -- HIE files contain TH-expanded code, so they have accurate usage info
      thResult <- analyzeFileWithTH
                    path
                    thSplices
                    sourceUsedNames
                    (importsThRoots importsConfig)
                    (optHieDir (acOptions ctx))

      -- Emit warnings about missing HIE files for TH code
      let thWarningDiags = map mkTHWarningDiag (tarWarnings thResult)

      -- Run redundant code checks (with auto-fixes) using AST-based detection
      redundantDiags <- detectRedundantAST defaultRedundantConfig path "" m

      let -- Get used names (may include TH root patterns)
          usedNames = tarUsedNames thResult

          -- Decide whether to run import checks
          -- If TH is present but no HIE file, and suppress-for-th is enabled, skip unused checks
          shouldCheckUnused = not (tarSuppressUnused thResult && importsSuppressForTH importsConfig)

          -- Run import checks (conditionally suppress unused for TH files without HIE)
          importDiags = if shouldCheckUnused
                        then checkImports importsConfig path imports usedNames
                        else checkImportsNoUnused importsConfig path imports

          -- Combine all diagnostics using DList for O(1) append
          allDiagsDL = DL.fromList namingDiags
                    <> DL.fromList patternDiags
                    <> DL.fromList importDiags
                    <> DL.fromList thWarningDiags
                    <> DL.fromList perfDiags
                    <> DL.fromList spaceLeakDiags
                    <> DL.fromList redundantDiags
                    <> DL.fromList modernizeDiags
                    <> DL.fromList securityDiags
                    <> DL.fromList extensionDiags
                    <> DL.fromList configurableRulesDiags
                    <> DL.fromList restrictionDiags

          -- Convert to list only once at the end
          allDiags = DL.toList allDiagsDL

          -- Parse and apply suppression comments (-- argus:ignore, etc.)
          suppressions = parseSuppressions path source
          filteredDiags = applySuppressions suppressions allDiags

      pure FileResult
        { fileResultPath = path
        , fileResultDiagnostics = filteredDiags
        , fileResultSymbols = types
        , fileResultImports = map (QualifiedName Nothing . iiModuleName) imports
        , fileResultExports = map (QualifiedName Nothing . eiName) exports
        }

-- | Convert parse error to diagnostic
parseErrorToDiag :: ParseError -> Diagnostic
parseErrorToDiag err = Diagnostic
  { diagSpan = mkSrcSpanRaw (peFile err) (peLine err) (peColumn err) (peLine err) (peColumn err + 1)
  , diagSeverity = Error
  , diagKind = Custom "parse-error"
  , diagMessage = peMessage err
  , diagCode = Just "parse/error"
  , diagFixes = []
  , diagRelated = []
  }

-- | Create diagnostic for TH-related warnings (e.g., missing HIE files)
mkTHWarningDiag :: Text -> Diagnostic
mkTHWarningDiag warning = Diagnostic
  { diagSpan = noSrcSpan
  , diagSeverity = Warning
  , diagKind = Custom "th-analysis"
  , diagMessage = warning
  , diagCode = Just "th/no-hie-file"
  , diagFixes = []
  , diagRelated = []
  }

-- | Run import checks but skip unused detection
-- Used when TH is present but no HIE file available
checkImportsNoUnused :: ImportsConfig -> FilePath -> [ImportInfo] -> [Diagnostic]
checkImportsNoUnused cfg path imports =
  -- Run qualified suggestions and explicit requirements, but NOT unused detection
  let qualifiedDiags = suggestQualifiedImports path cfg imports
      explicitDiags = if importsRequireExplicit cfg
                      then checkExplicitImports path imports
                      else []
  in qualifiedDiags ++ explicitDiags

--------------------------------------------------------------------------------
-- Legacy Compatibility
--------------------------------------------------------------------------------

-- | Legacy checkFile function for backwards compatibility
checkFile :: Config -> FilePath -> IO (Maybe (String, [Diagnostic]))
checkFile config path = do
  let opts = defaultOptions { optTargetPaths = [path] }
  rulesConfig <- loadRulesConfig Nothing
  let ctx = defaultContext config opts rulesConfig
  result <- analyzeFile ctx path
  let diags = fileResultDiagnostics result
  if null diags
    then pure Nothing
    else do
      content <- readFile path
      let newContent = applyFixes (T.pack content) (concatMap diagFixes diags)
      pure $ Just (T.unpack newContent, diags)

-- | Legacy checkFiles function
checkFiles :: Config -> [FilePath] -> IO [(FilePath, [Diagnostic])]
checkFiles config paths = do
  let opts = defaultOptions { optTargetPaths = paths }
  rulesConfig <- loadRulesConfig Nothing
  let ctx = defaultContext config opts rulesConfig
  results <- analyzeFiles ctx paths
  pure [(p, fileResultDiagnostics fr) | (p, fr) <- Map.toList (resultFiles results)]

--------------------------------------------------------------------------------
-- File Discovery
--------------------------------------------------------------------------------

-- | Find all Haskell files in given paths, supporting glob patterns
-- Examples:
--   - "src/Foo.hs" - single file
--   - "src/" - directory (recursively finds .hs files)
--   - "src/**/*.hs" - glob pattern for all .hs files in src and subdirectories
--   - "*.hs" - all .hs files in current directory
findHaskellFiles :: [FilePath] -> IO [FilePath]
findHaskellFiles paths = concat <$> mapM findInPath paths
  where
    findInPath path
      -- Check if it's a glob pattern
      | hasGlobPattern path = do
          -- Use the Glob library to match files
          matched <- glob path
          pure $ filter isHaskell matched

      | otherwise = do
          isFile <- doesFileExist path
          isDir <- doesDirectoryExist path
          if isFile && isHaskell path
            then pure [path]
            else if isDir
              then do
                contents <- listDirectory path
                concat <$> mapM (findInPath . (path </>)) contents
              else pure []

    isHaskell path = takeExtension path == ".hs"

    -- Check if path contains glob characters
    hasGlobPattern path = any (`elem` path) ("*?[" :: String)

-- | Simple glob matching using System.FilePath.Glob
glob :: FilePath -> IO [FilePath]
glob pat = do
  let (dir, filePattern) = splitGlobPattern pat
  baseDir <- if null dir then getCurrentDirectory else pure dir
  files <- findAllFilesIn baseDir
  pure $ filter (matchGlobPattern filePattern . makeRelative baseDir) files
  where
    splitGlobPattern p =
      let parts = splitPath p
          (dirParts, patternParts) = break hasGlobChars parts
      in (joinPath dirParts, joinPath patternParts)

    hasGlobChars s = any (`elem` s) ("*?[" :: String)

    -- Find all files recursively
    findAllFilesIn :: FilePath -> IO [FilePath]
    findAllFilesIn dir = do
      exists <- doesDirectoryExist dir
      if not exists
        then pure []
        else do
          entries <- listDirectory dir
          results <- forM entries $ \entry -> do
            let fullPath = dir </> entry
            isFile <- doesFileExist fullPath
            isDir <- doesDirectoryExist fullPath
            if isFile
              then pure [fullPath]
              else if isDir
                then findAllFilesIn fullPath
                else pure []
          pure $ concat results

    makeRelative base path
      | base `isPrefixOf'` path = drop (length base + 1) path
      | otherwise = path

    isPrefixOf' pre str = pre == take (length pre) str

-- | Match a path against a glob pattern
-- Supports:
--   - * matches any single path component (not including /)
--   - ** matches zero or more path components
--   - ? matches any single character
--   - [abc] matches any character in the brackets
matchGlobPattern :: FilePath -> FilePath -> Bool
matchGlobPattern pat filePath = match (normalizePath pat) (normalizePath filePath)
  where
    -- Normalize path: split and remove trailing slashes from components
    normalizePath p = map stripSlash (splitPath p)
    stripSlash s = filter (/= '/') s

    match [] [] = True
    match [] _ = False
    match _ [] = False
    match ("**":ps) parts = any (match ps) (tails parts)
    match (p:ps) (part:parts) = matchComponent p part && match ps parts

    matchComponent patComp fileComp = matchChars patComp fileComp

    matchChars [] [] = True
    matchChars [] _ = False
    matchChars _ [] = False
    matchChars ('*':ps) str = any (matchChars ps) (tails str)
    matchChars ('?':ps) (_:str) = matchChars ps str
    matchChars ('[':ps) (c:str) =
      case break (== ']') ps of
        (chars, ']':rest) -> matchBracket chars c && matchChars rest str
        _ -> False
    matchChars (p:ps) (c:str) = p == c && matchChars ps str

    -- Match character against bracket expression (supports ranges like a-z)
    matchBracket :: String -> Char -> Bool
    matchBracket chars c = go chars
      where
        go [] = False
        go [x] = c == x
        go (x:'-':y:rest)
          | x <= y    = (c >= x && c <= y) || go rest
          | otherwise = c == x || c == '-' || go (y:rest)
        go (x:rest) = c == x || go rest

    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'

-- Helper to get current working directory
getCurrentDirectory :: IO FilePath
getCurrentDirectory = System.Directory.getCurrentDirectory

-- | Filter files that match any exclude pattern
-- Supports glob patterns like "Generated/**", "*.gen.hs", etc.
filterExcluded :: [FilePath] -> [FilePath] -> [FilePath]
filterExcluded excludePatterns files =
  filter (not . matchesAnyExclude) files
  where
    matchesAnyExclude file = any (`matchesFile` file) excludePatterns

    -- Check if an exclude pattern matches a file path
    matchesFile excludePat file
      -- If pattern starts with **, match against full path
      | take 2 excludePat == "**" = matchGlobPattern excludePat file
      -- If pattern contains /, match against full path
      | '/' `elem` excludePat = matchGlobPattern excludePat file
      -- Otherwise, match against filename only
      | otherwise = matchGlobPattern excludePat (takeFileName file)

    takeFileName path = case break (== '/') (reverse path) of
      (name, _) -> reverse name

--------------------------------------------------------------------------------
-- Name Usage Extraction
--------------------------------------------------------------------------------

-- | Extract all identifiers and operators used anywhere in the source file
-- This is more comprehensive than extractUsedNames as it catches:
-- - Class method signatures
-- - Data/type declarations
-- - Instance declarations
-- - Standalone type signatures
-- - Deriving clauses
-- We exclude the import section to avoid counting imported names as "used"
extractUsedNamesFromSource :: Text -> [ImportInfo] -> Set Text
extractUsedNamesFromSource source imports =
  Set.fromList $ extractIdentifiers nonImportSource ++ extractOperators nonImportSource
  where
    -- Find the end of imports section to exclude it
    -- Imports are typically at the top after module declaration
    nonImportSource = removeImportLines source imports

-- | Remove import lines from source to avoid counting them as usage
-- This uses the span information from parsed imports
removeImportLines :: Text -> [ImportInfo] -> Text
removeImportLines source imports =
  let sourceLines = T.lines source
      importLineNums = Set.fromList $ concatMap getImportLineRange imports
      nonImportLines = [ (i, line)
                       | (i, line) <- zip [1..] sourceLines
                       , i `Set.notMember` importLineNums
                       ]
  in T.unlines $ map snd nonImportLines
  where
    -- Get all line numbers that an import spans
    getImportLineRange imp =
      let impSpan = iiSpan imp
          start = srcSpanStartLine impSpan
          end = srcSpanEndLine impSpan
      in [start..end]

-- | Extract all identifiers and operators used in function bodies AND type signatures
-- This is used for import checking to determine which imports are actually used
-- NOTE: Type signatures are critical - many imports are type-only (constraints, type aliases)
-- DEPRECATED: Use extractUsedNamesFromSource instead for more comprehensive coverage
extractUsedNames :: [FunctionInfo] -> Set Text
extractUsedNames functions =
  Set.fromList $ concatMap extractFromFunction functions
  where
    extractFromFunction fi =
      -- Extract from function body
      concatMap extractAll (map snd (fiBody fi))
      -- Extract from type signature (includes constraints like MonadLogger, types like IO)
      ++ extractFromSignature (fiSignature fi)
      -- Extract from argument types
      ++ concatMap extractFromArgument (fiArguments fi)

    extractAll line = extractIdentifiers line ++ extractOperators line

    -- Extract names from type signatures (constraints, types)
    extractFromSignature Nothing = []
    extractFromSignature (Just ti) =
      extractIdentifiers (tiText ti)
      ++ concatMap extractIdentifiers (tiArgTypes ti)
      ++ extractIdentifiers (tiRetType ti)

    -- Extract names from argument types
    extractFromArgument arg = case aiType arg of
      Nothing -> []
      Just t -> extractIdentifiers t

-- | Extract identifiers from a line of code
-- Identifies sequences of alphanumeric characters and underscores that look like identifiers
extractIdentifiers :: Text -> [Text]
extractIdentifiers text = go (T.unpack text) [] []
  where
    go :: String -> String -> [Text] -> [Text]
    go [] current acc = finalize current acc
    go (c:cs) current acc
      | isIdentStart c && null current =
          go cs [c] acc
      | isIdentChar c && not (null current) =
          go cs (current ++ [c]) acc
      | not (null current) =
          go cs [] (finalize current acc)
      | otherwise =
          go cs [] acc

    finalize [] acc = acc
    finalize current acc =
      let ident = T.pack current
      in if isValidIdentifier ident
         then ident : acc
         else acc

    -- Filter out obvious non-identifiers
    isValidIdentifier t
      | T.null t = False
      | T.all (`elem` ['0'..'9']) t = False  -- All digits
      | t `elem` keywords = False            -- Haskell keywords
      | otherwise = True

    keywords =
      [ "case", "class", "data", "default", "deriving", "do", "else"
      , "foreign", "if", "import", "in", "infix", "infixl", "infixr"
      , "instance", "let", "module", "newtype", "of", "then", "type"
      , "where", "_"
      ]

-- | Extract operators from a line of code
-- Identifies sequences of operator characters like .=, <>, .:, etc.
extractOperators :: Text -> [Text]
extractOperators text = go (T.unpack text) [] []
  where
    go :: String -> String -> [Text] -> [Text]
    go [] current acc = finalize current acc
    go (c:cs) current acc
      | isOperatorChar c && null current =
          go cs [c] acc
      | isOperatorChar c && not (null current) =
          go cs (current ++ [c]) acc
      | not (null current) =
          go cs [] (finalize current acc)
      | otherwise =
          go cs [] acc

    finalize [] acc = acc
    finalize current acc =
      let op = T.pack current
      in if isValidOperator op
         then op : acc
         else acc

    -- Filter out syntax elements that aren't real operators
    isValidOperator op
      | T.null op = False
      | T.length op == 1 && T.head op `elem` syntaxChars = False
      | op `elem` syntaxOperators = False
      | otherwise = True

    -- Single-character syntax elements that aren't operators
    syntaxChars :: String
    syntaxChars = "=|\\@:."

    -- Multi-character syntax that looks like operators but aren't
    syntaxOperators :: [Text]
    syntaxOperators =
      [ "::", "->", "<-", "=>", "..", "|", "=", "@", "\\"
      , "--"  -- Line comments
      ]

--------------------------------------------------------------------------------
-- Module Name Extraction
--------------------------------------------------------------------------------

-- | Extract module name from source code
-- Parses the "module Foo.Bar where" line to get the module name
extractModuleName :: Text -> Text
extractModuleName source =
  case find isModuleLine (T.lines source) of
    Just line -> parseModuleName line
    Nothing -> ""
  where
    isModuleLine line =
      let stripped = T.stripStart line
      in "module " `T.isPrefixOf` stripped

    parseModuleName line =
      let stripped = T.stripStart line
          -- Remove "module " prefix
          afterModule = T.drop 7 stripped
          -- Take until "where" or "(" (for export list)
          modName = T.takeWhile (\c -> c /= '(' && c /= ' ' && c /= '\t') afterModule
      in T.strip modName
