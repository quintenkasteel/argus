{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Argus.Analysis.TemplateHaskell
-- Description : General Template Haskell analysis for unused import detection
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides general Template Haskell handling for unused import detection.
--
-- Key insight: HIE files are generated AFTER Template Haskell expansion, so they
-- contain the fully expanded code with all identifier references. By using HIE-based
-- analysis, we automatically get correct unused import detection for TH code.
--
-- When HIE files are not available but TH is present, we emit a warning and
-- can optionally suppress unused import warnings to avoid false positives.
module Argus.Analysis.TemplateHaskell
  ( -- * TH Detection
    hasTHSplices
  , THInfo (..)
  , analyzeTH

    -- * HIE File Discovery
  , findHieFileForSource
  , hieFileExists
  , findHieDbForSource

    -- * Used names extraction
  , extractUsedNamesFromHie
  , extractUsedNamesFromHieDb

    -- * Combined analysis result
  , THAnalysisResult (..)
  , analyzeFileWithTH

    -- * Roots matching
  , matchesRoots
  ) where

import Control.Exception (try, SomeException)
import Data.List (isSuffixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesFileExist, listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeBaseName, takeDirectory, takeFileName, dropExtension, makeRelative)
import Text.Regex.TDFA ((=~))

-- GHC API for reading HIE files directly
import "ghc" GHC.Iface.Ext.Binary (readHieFile, hie_file_result)
import "ghc" GHC.Iface.Ext.Types (HieFile(..), HieASTs(..), getAsts, Identifier)
import "ghc" GHC.Iface.Ext.Utils (generateReferencesMap)
import "ghc" GHC.Types.Name (Name, nameOccName)
import "ghc" GHC.Types.Name.Occurrence (occNameString)

-- SQLite for querying hiedb database directly (cross-GHC-version compatible)
import Database.SQLite.Simple (open, close, query, Only(..))

import HieDb.Utils (makeNc)

import Argus.Analysis.Syntactic (ThSpliceInfo(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Information about Template Haskell usage in a file
data THInfo = THInfo
  { thiHasSplices     :: Bool           -- ^ File contains TH splices
  , thiSpliceCount    :: Int            -- ^ Number of splices found
  , thiSpliceExprs    :: [Text]         -- ^ Splice expressions for debugging
  }
  deriving stock (Eq, Show)

-- | Result of TH-aware analysis
data THAnalysisResult = THAnalysisResult
  { tarUsedNames      :: Set Text       -- ^ Names used in the file
  , tarHieFileFound   :: Bool           -- ^ Whether HIE file was found
  , tarHieFilePath    :: Maybe FilePath -- ^ Path to HIE file if found
  , tarTHInfo         :: THInfo         -- ^ TH information
  , tarWarnings       :: [Text]         -- ^ Warnings (e.g., "HIE file not found for TH file")
  , tarSuppressUnused :: Bool           -- ^ Should suppress unused import warnings?
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- TH Detection
--------------------------------------------------------------------------------

-- | Check if a list of TH splices indicates the file uses Template Haskell
hasTHSplices :: [ThSpliceInfo] -> Bool
hasTHSplices = not . null

-- | Analyze TH usage in a file
analyzeTH :: [ThSpliceInfo] -> THInfo
analyzeTH splices = THInfo
  { thiHasSplices = not (null splices)
  , thiSpliceCount = length splices
  , thiSpliceExprs = map tsiExpr splices
  }

--------------------------------------------------------------------------------
-- HIE File Discovery
--------------------------------------------------------------------------------

-- | Check if HIE file exists for a source file
hieFileExists :: FilePath -> Maybe FilePath -> IO Bool
hieFileExists sourcePath mHieDir = do
  mPath <- findHieFileForSource sourcePath mHieDir
  case mPath of
    Just _ -> pure True
    Nothing -> pure False

-- | Find the HIE file corresponding to a source file
-- Searches in common locations: .hie directory, alongside source, etc.
-- Handles nested module paths like Render/DomainSwitch.hs -> Render/DomainSwitch.hie
findHieFileForSource :: FilePath -> Maybe FilePath -> IO (Maybe FilePath)
findHieFileForSource sourcePath mHieDir = do
  let baseName = dropExtension (takeFileName sourcePath)
      sourceDir = takeDirectory sourcePath

      -- Try to extract module path from source (e.g., "src/Render/DomainSwitch.hs" -> "Render/DomainSwitch")
      -- This handles nested modules in HIE directories
      modulePath = extractModulePath sourcePath

      -- Common HIE file locations to check
      candidates = case mHieDir of
        Just hieDir ->
          [ hieDir </> modulePath <> ".hie"  -- Try full module path first
          , hieDir </> baseName <> ".hie"     -- Fall back to just filename
          ]
        Nothing ->
          [ sourceDir </> ".hie" </> baseName <> ".hie"
          , sourceDir </> baseName <> ".hie"
          , ".hie" </> baseName <> ".hie"
          , ".hie" </> modulePath <> ".hie"
          ]

  -- First try direct candidates
  found <- findExisting candidates

  -- If not found and we have a HIE dir, try recursive search
  case (found, mHieDir) of
    (Just p, _) -> pure (Just p)
    (Nothing, Just hieDir) -> searchHieDirectory hieDir sourcePath
    _ -> pure Nothing

-- | Extract module path from source file path
-- E.g., "/home/user/project/src/Render/DomainSwitch.hs" -> "Render/DomainSwitch"
extractModulePath :: FilePath -> FilePath
extractModulePath sourcePath =
  let parts = splitPath sourcePath
      -- Drop common prefixes like "src/", "app/", "lib/"
      relevant = dropWhile (\p -> p `elem` ["src/", "app/", "lib/", "src\\", "app\\", "lib\\"]) parts
      -- Join and remove .hs extension
  in dropExtension $ joinPath relevant
  where
    splitPath p = case break (== '/') p of
      (a, '/':rest) -> (a ++ "/") : splitPath rest
      (a, "") -> [a]
      _ -> [p]
    joinPath = foldr (</>) ""

-- | Find the first existing file from a list of candidates
findExisting :: [FilePath] -> IO (Maybe FilePath)
findExisting [] = pure Nothing
findExisting (p:ps) = do
  exists <- doesFileExist p
  if exists then pure (Just p) else findExisting ps

-- | Search for HIE file by scanning a directory recursively
searchHieDirectory :: FilePath -> FilePath -> IO (Maybe FilePath)
searchHieDirectory hieDir sourcePath = do
  let targetBaseName = dropExtension (takeFileName sourcePath)
  exists <- doesDirectoryExist hieDir
  if not exists
    then pure Nothing
    else searchRecursive hieDir targetBaseName

searchRecursive :: FilePath -> String -> IO (Maybe FilePath)
searchRecursive dir targetBase = do
  contents <- listDirectory dir
  -- Check files in current directory
  let hieFiles = filter (\f -> takeBaseName f == targetBase && ".hie" `isSuffixOf` f) contents
  case hieFiles of
    (f:_) -> pure $ Just (dir </> f)
    [] -> do
      -- Check subdirectories
      subdirs <- filterM (doesDirectoryExist . (dir </>)) contents
      results <- mapM (\d -> searchRecursive (dir </> d) targetBase) subdirs
      pure $ listToMaybe $ catMaybes results
  where
    filterM p = foldr (\x acc -> do
      b <- p x
      rest <- acc
      pure $ if b then x:rest else rest) (pure [])

--------------------------------------------------------------------------------
-- HIE-based Used Names Extraction (direct HIE file reading)
--------------------------------------------------------------------------------

-- | Extract all used names from a HIE file
-- This reads the HIE file directly and extracts all identifier references.
-- The HIE file contains the EXPANDED Template Haskell code, so all references
-- from TH-generated code are included.
--
-- Returns the set of all identifier names used in the file.
extractUsedNamesFromHie :: FilePath -> IO (Either Text (Set Text))
extractUsedNamesFromHie hiePath = do
  result <- try @SomeException $ do
    -- Create a NameCache for reading HIE files (GHC 9.10+ uses NameCache directly)
    nc <- makeNc

    -- Read the HIE file
    hieResult <- readHieFile nc hiePath
    let hieFile = hie_file_result hieResult

    -- Extract all identifier references from the HIE ASTs
    let asts = getAsts $ hie_asts hieFile
        -- generateReferencesMap gives us: Map Identifier [(Span, IdentifierDetails)]
        refMap = generateReferencesMap (Map.elems asts)

        -- Extract unique names from all identifiers
        names = Set.fromList $ concatMap identifierToNames $ Map.keys refMap

    pure names

  case result of
    Left err -> pure $ Left $ T.pack $ "Failed to read HIE file: " <> show err
    Right names -> pure $ Right names

-- | Convert a GHC Identifier to a list of names (usually just one)
-- An Identifier is Either ModuleName Name
identifierToNames :: Identifier -> [Text]
identifierToNames ident = case ident of
  Left _moduleName -> []  -- Module imports, not relevant for unused import detection
  Right name -> [nameToText name]

-- | Convert a GHC Name to Text
nameToText :: Name -> Text
nameToText name = T.pack $ occNameString $ nameOccName name

--------------------------------------------------------------------------------
-- HieDb Database Query (cross-GHC-version compatible)
--------------------------------------------------------------------------------

-- | Find a .hiedb database for the given source file
-- Searches in the HIE directory and common locations
-- The `argus index` command creates .hiedb in the project root
findHieDbForSource :: FilePath -> Maybe FilePath -> IO (Maybe FilePath)
findHieDbForSource sourcePath mHieDir = do
  let sourceDir = takeDirectory sourcePath
      candidates = case mHieDir of
        Just hieDir ->
          [ takeDirectory hieDir </> ".hiedb"  -- Project root (argus index creates here)
          , hieDir </> ".hiedb"
          , hieDir </> "hiedb"
          ]
        Nothing ->
          [ ".hiedb"                           -- Project root (most common)
          , sourceDir </> ".hiedb"
          , ".hie" </> ".hiedb"
          , sourceDir </> ".hie" </> ".hiedb"
          ]
  findExisting candidates

-- | Extract used names from a hiedb SQLite database
-- This is cross-GHC-version compatible because it queries the SQLite database
-- directly, not the binary HIE files.
--
-- The hiedb database should be created by running:
--   hiedb -D .hie/.hiedb index .hie
-- with a hiedb version matching your project's GHC version.
extractUsedNamesFromHieDb :: FilePath -> FilePath -> IO (Either Text (Set Text))
extractUsedNamesFromHieDb dbPath sourcePath = do
  result <- try @SomeException $ do
    conn <- open dbPath

    -- Query all identifier occurrences from this source file
    -- The refs table has: src (source file), occ (occurrence name), mod, unit, position
    -- We need to find refs where the source file matches
    -- Note: src paths in hiedb may be relative or absolute, try both
    let srcBase = takeFileName sourcePath
        srcRelative = makeRelative "." sourcePath

    -- Query for occurrences - the occ column contains names like "v:foo" or "tc:Bar"
    -- We need to extract just the name part after the namespace prefix
    rows <- query conn
      "SELECT DISTINCT occ FROM refs WHERE src LIKE ? OR src LIKE ? OR src LIKE ?"
      (srcBase, "%" </> srcBase, "%" </> srcRelative) :: IO [Only Text]

    close conn

    -- Extract names from occurrence strings (format: "namespace:name")
    let names = Set.fromList $ map (extractOccName . (\(Only n) -> n)) rows

    pure names

  case result of
    Left err -> pure $ Left $ T.pack $ show err
    Right names -> pure $ Right names

-- | Extract the name part from a hiedb occurrence string
-- Occurrences are stored as "namespace:name" (e.g., "v:foo", "tc:Maybe")
extractOccName :: Text -> Text
extractOccName occ = case T.breakOn ":" occ of
  (_, rest) | not (T.null rest) -> T.drop 1 rest  -- Drop the ":"
  _ -> occ  -- No namespace prefix, use as-is

--------------------------------------------------------------------------------
-- Roots Matching
--------------------------------------------------------------------------------

-- | Check if any import item matches the root patterns
-- Root patterns are regex patterns for names that should always be considered "used"
matchesRoots :: [Text] -> Text -> Bool
matchesRoots patterns name =
  any (\pat -> T.unpack name =~ T.unpack pat) patterns

-- | Check if any TH splice expression matches root patterns
-- This is useful for identifying TH that generates code using certain imports
spliceMatchesRoots :: [Text] -> Text -> Bool
spliceMatchesRoots patterns spliceExpr =
  any (\pat -> T.unpack spliceExpr =~ T.unpack pat) patterns

--------------------------------------------------------------------------------
-- Combined Analysis
--------------------------------------------------------------------------------

-- | Analyze a file with TH awareness
-- Uses HIE file if available, otherwise falls back to source analysis with warnings
analyzeFileWithTH
  :: FilePath           -- ^ Source file path
  -> [ThSpliceInfo]     -- ^ Extracted TH splices
  -> Set Text           -- ^ Used names from source analysis
  -> [Text]             -- ^ TH root patterns (from config)
  -> Maybe FilePath     -- ^ Optional HIE directory
  -> IO THAnalysisResult
analyzeFileWithTH sourcePath splices sourceUsedNames thRoots mHieDir = do
  let thInfo = analyzeTH splices

  -- Check if any splices match TH root patterns
  let spliceExprs = map tsiExpr splices
      matchedRoots = Set.fromList
        [ rootPat
        | rootPat <- thRoots
        , any (spliceMatchesRoots [rootPat]) spliceExprs
        ]

      -- Add matched root patterns to used names
      rootNames = Set.map extractRootName matchedRoots
      usedWithRoots = sourceUsedNames `Set.union` rootNames

  -- Try to find HIE file
  mHiePath <- findHieFileForSource sourcePath mHieDir

  let hasTH = thiHasSplices thInfo

  -- Also look for hiedb database as fallback
  mHieDbPath <- findHieDbForSource sourcePath mHieDir

  case mHiePath of
    Just hiePath -> do
      -- HIE file found - try to extract used names from it
      hieResult <- extractUsedNamesFromHie hiePath

      case hieResult of
        Right hieUsedNames -> do
          -- Successfully extracted names from HIE file
          -- Merge HIE-based names with source-based names for comprehensive coverage
          let allUsedNames = usedWithRoots `Set.union` hieUsedNames
          pure THAnalysisResult
            { tarUsedNames = allUsedNames
            , tarHieFileFound = True
            , tarHieFilePath = Just hiePath
            , tarTHInfo = thInfo
            , tarWarnings = []
            , tarSuppressUnused = False  -- Don't suppress - we have accurate data from HIE
            }

        Left hieErr -> do
          -- Failed to read HIE file directly (likely GHC version mismatch)
          -- Try querying hiedb database as fallback
          case mHieDbPath of
            Just dbPath -> do
              dbResult <- extractUsedNamesFromHieDb dbPath sourcePath
              case dbResult of
                Right dbUsedNames -> do
                  -- Successfully got names from hiedb
                  let allUsedNames = usedWithRoots `Set.union` dbUsedNames
                  pure THAnalysisResult
                    { tarUsedNames = allUsedNames
                    , tarHieFileFound = True
                    , tarHieFilePath = Just hiePath
                    , tarTHInfo = thInfo
                    , tarWarnings = []
                    , tarSuppressUnused = False
                    }
                Left _dbErr ->
                  -- hiedb query also failed - give up with clear instructions
                  mkVersionMismatchResult sourcePath hiePath hieErr hasTH thInfo usedWithRoots

            Nothing ->
              -- No hiedb database found - give instructions
              mkVersionMismatchResult sourcePath hiePath hieErr hasTH thInfo usedWithRoots

    Nothing ->
      -- No HIE file found - try hiedb database
      case mHieDbPath of
        Just dbPath -> do
          dbResult <- extractUsedNamesFromHieDb dbPath sourcePath
          case dbResult of
            Right dbUsedNames -> do
              let allUsedNames = usedWithRoots `Set.union` dbUsedNames
              pure THAnalysisResult
                { tarUsedNames = allUsedNames
                , tarHieFileFound = False
                , tarHieFilePath = Nothing
                , tarTHInfo = thInfo
                , tarWarnings = []
                , tarSuppressUnused = False
                }
            Left _dbErr ->
              -- hiedb query failed
              mkNoHieResult sourcePath hasTH thInfo usedWithRoots

        Nothing ->
          -- No HIE file and no hiedb
          mkNoHieResult sourcePath hasTH thInfo usedWithRoots

-- | Create result for version mismatch case with clear instructions
mkVersionMismatchResult :: FilePath -> FilePath -> Text -> Bool -> THInfo -> Set Text -> IO THAnalysisResult
mkVersionMismatchResult _sourcePath hiePath hieErr hasTH thInfo usedWithRoots = do
  let isVersionMismatch = "versions don't match" `T.isInfixOf` hieErr
      instructions = if isVersionMismatch
        then "\n\nTo fix GHC version mismatch, either:\n" <>
             "1. Install hiedb matching your project's GHC and run: hiedb -D .hiedb index <hie-dir>\n" <>
             "   Then argus can query the database across GHC versions.\n" <>
             "2. Build argus with the same GHC version as your project.\n" <>
             "3. Rebuild your project's HIE files with argus's GHC version."
        else ""
      warning = "HIE file found but failed to read: " <> hieErr <> instructions
  pure THAnalysisResult
    { tarUsedNames = usedWithRoots
    , tarHieFileFound = True
    , tarHieFilePath = Just hiePath
    , tarTHInfo = thInfo
    , tarWarnings = [warning]
    , tarSuppressUnused = hasTH
    }

-- | Create result when no HIE file found
mkNoHieResult :: FilePath -> Bool -> THInfo -> Set Text -> IO THAnalysisResult
mkNoHieResult sourcePath hasTH thInfo usedWithRoots =
  if hasTH
    then do
      let warning = "No HIE file found for " <> T.pack sourcePath <>
                    " which contains " <> T.pack (show (thiSpliceCount thInfo)) <>
                    " Template Haskell splice(s). " <>
                    "Unused import detection is disabled to avoid false positives. " <>
                    "Compile with -fwrite-ide-info to generate HIE files."
      pure THAnalysisResult
        { tarUsedNames = usedWithRoots
        , tarHieFileFound = False
        , tarHieFilePath = Nothing
        , tarTHInfo = thInfo
        , tarWarnings = [warning]
        , tarSuppressUnused = True
        }
    else
      pure THAnalysisResult
        { tarUsedNames = usedWithRoots
        , tarHieFileFound = False
        , tarHieFilePath = Nothing
        , tarTHInfo = thInfo
        , tarWarnings = []
        , tarSuppressUnused = False
        }

-- | Extract a simple name from a root pattern (for adding to used names)
-- This is a heuristic - extracts alphanumeric parts
extractRootName :: Text -> Text
extractRootName pat =
  -- Remove regex syntax to get approximate name
  T.filter (\c -> c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])) $
  T.dropWhile (== '^') $ T.dropWhileEnd (== '$') pat
