{-# LANGUAGE StrictData #-}

-- |
-- Module      : Linter.Core
-- Description : Main orchestration for the linter
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides the main orchestration for the linter,
-- coordinating parsing, analysis, rule application, and output.
module Linter.Core
  ( -- * Main entry points
    runLinter
  , runLinterOnFile
  , runLinterOnFiles

    -- * Analysis functions
  , analyzeFile
  , analyzeFiles
  , AnalysisContext (..)

    -- * Legacy compatibility
  , checkFile
  , checkFiles
  ) where

import Control.Monad (forM, forM_, when)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)

import Linter.Analysis.DepGraph
import Linter.Analysis.Semantic hiding (extractTypes)
import Linter.Analysis.Syntactic
import Linter.Analysis.Unused
import Linter.Config
import Linter.Output.Terminal
import Linter.Output.Json
import Linter.Output.Sarif
import Linter.Output.Types
import Linter.Refactor.Engine
import Linter.Refactor.ExactPrint
import Linter.Rules.Naming
import Linter.Rules.Patterns
import Linter.Rules.Imports
import Linter.Types
import Linter.Utils (isIdentChar, isIdentStart)

--------------------------------------------------------------------------------
-- Analysis Context
--------------------------------------------------------------------------------

-- | Context for analysis
data AnalysisContext = AnalysisContext
  { acConfig    :: Config
  , acOptions   :: LinterOptions
  , acHieData   :: [HieData]       -- ^ Pre-loaded HIE data (for full mode)
  , acDepGraph  :: Maybe DepGraph  -- ^ Pre-built dependency graph
  }

-- | Create a default analysis context
defaultContext :: Config -> LinterOptions -> AnalysisContext
defaultContext cfg opts = AnalysisContext
  { acConfig = cfg
  , acOptions = opts
  , acHieData = []
  , acDepGraph = Nothing
  }

--------------------------------------------------------------------------------
-- Main Entry Points
--------------------------------------------------------------------------------

-- | Run the linter with given options
runLinter :: LinterOptions -> IO AnalysisResult
runLinter opts = do
  -- Load configuration
  config <- loadConfig (optConfigFile opts)

  -- Find all Haskell files
  files <- findHaskellFiles (optTargetPaths opts)

  -- Create analysis context
  ctx <- case optMode opts of
    QuickMode -> pure $ defaultContext config opts
    FullMode -> do
      hies <- case optHieDir opts of
        Just dir -> loadHieFiles dir
        Nothing  -> pure []
      pure $ AnalysisContext config opts hies Nothing
    PluginMode -> pure $ defaultContext config opts  -- Plugin mode handled separately

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

  pure results

-- | Run linter on a single file
runLinterOnFile :: AnalysisContext -> FilePath -> IO FileResult
runLinterOnFile ctx path = analyzeFile ctx path

-- | Run linter on multiple files
runLinterOnFiles :: AnalysisContext -> [FilePath] -> IO AnalysisResult
runLinterOnFiles = analyzeFiles

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
      allDiags = concatMap (fileResultDiagnostics . snd) results

      -- Count by severity
      sevCounts = foldr countSeverity Map.empty allDiags

      -- Unused code detection (for full mode)
      unused = case acDepGraph ctx of
        Just graph -> urUnreachable $
          detectUnused (toUnusedConfig $ cfgUnused $ acConfig ctx) graph (acHieData ctx)
        Nothing -> Set.empty

  pure AnalysisResult
    { resultFiles = fileMap
    , resultUnusedCode = unused
    , resultDiagCount = sevCounts
    }
  where
    countSeverity d = Map.insertWith (+) (diagSeverity d) 1

    toUnusedConfig uc = Linter.Analysis.Unused.UnusedConfig
      { ucRoots = unusedRoots uc
      , ucThRoots = unusedThRoots uc
      , ucCheckFunctions = unusedCheckFunctions uc
      , ucCheckTypes = unusedCheckTypes uc
      , ucCheckImports = unusedCheckImports uc
      , ucCheckExports = unusedCheckExports uc
      }

-- | Analyze a single file
analyzeFile :: AnalysisContext -> FilePath -> IO FileResult
analyzeFile ctx path = do
  content <- TE.decodeUtf8 <$> BS.readFile path
  parseResult <- parseFile path

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
          m = prModule pr
          source = prSource pr

          -- Extract information
          functions = extractFunctions path source m
          imports = extractImports path m
          exports = extractExports path m
          types = extractTypes path m

          -- Run naming convention checks
          namingDiags = checkNamingConventions (cfgNaming config) path source functions

          -- Run pattern checks
          patternDiags = checkPatterns (cfgPatterns config) path source functions

          -- Extract used names from function bodies for import checking
          usedNames = extractUsedNames functions

          -- Run import checks
          importDiags = checkImports (cfgImports config) path imports usedNames

          -- Combine all diagnostics
          allDiags = namingDiags ++ patternDiags ++ importDiags

      pure FileResult
        { fileResultPath = path
        , fileResultDiagnostics = allDiags
        , fileResultSymbols = types
        , fileResultImports = map (QualifiedName Nothing . iiModuleName) imports
        , fileResultExports = map (QualifiedName Nothing . eiName) exports
        }

-- | Convert parse error to diagnostic
parseErrorToDiag :: ParseError -> Diagnostic
parseErrorToDiag err = Diagnostic
  { diagSpan = SrcSpan (peFile err) (peLine err) (peColumn err) (peLine err) (peColumn err + 1)
  , diagSeverity = Error
  , diagKind = Custom "parse-error"
  , diagMessage = peMessage err
  , diagCode = Just "parse/error"
  , diagFixes = []
  , diagRelated = []
  }

--------------------------------------------------------------------------------
-- Legacy Compatibility
--------------------------------------------------------------------------------

-- | Legacy checkFile function for backwards compatibility
checkFile :: Config -> FilePath -> IO (Maybe (String, [Diagnostic]))
checkFile config path = do
  let opts = defaultOptions { optTargetPaths = [path] }
      ctx = defaultContext config opts
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
      ctx = defaultContext config opts
  results <- analyzeFiles ctx paths
  pure [(p, fileResultDiagnostics fr) | (p, fr) <- Map.toList (resultFiles results)]

--------------------------------------------------------------------------------
-- File Discovery
--------------------------------------------------------------------------------

-- | Find all Haskell files in given paths
findHaskellFiles :: [FilePath] -> IO [FilePath]
findHaskellFiles paths = concat <$> mapM findInPath paths
  where
    findInPath path = do
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

--------------------------------------------------------------------------------
-- Name Usage Extraction
--------------------------------------------------------------------------------

-- | Extract all identifiers used in function bodies
-- This is used for import checking to determine which imports are actually used
extractUsedNames :: [FunctionInfo] -> Set Text
extractUsedNames functions =
  Set.fromList $ concatMap extractFromFunction functions
  where
    extractFromFunction fi =
      concatMap (extractIdentifiers . snd) (fiBody fi)

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
