{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Baseline.IO
-- Description : Baseline file I/O operations
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module handles loading, saving, and managing baseline files.
-- Baselines are stored as JSON files and can be updated incrementally.
module Argus.Baseline.IO
  ( -- * File Operations
    loadBaseline
  , saveBaseline
  , baselineExists

    -- * Baseline Management
  , createBaseline
  , updateBaseline
  , mergeBaselines

    -- * Default Paths
  , defaultBaselinePath
  , findBaselineFile

    -- * Error Types
  , BaselineError (..)
  , formatBaselineError
  ) where

import Control.Exception (try, IOException)
import Data.Aeson (eitherDecodeStrict)
import Data.Aeson.Encode.Pretty (encodePretty', defConfig, confIndent, Indent(..))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (nubBy)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Argus.Types (Diagnostic)
import Argus.Baseline.Types

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

-- | Errors that can occur during baseline operations
data BaselineError
  = BaselineNotFound FilePath
  | BaselineParseError FilePath String
  | BaselineWriteError FilePath String
  | BaselineVersionMismatch BaselineVersion BaselineVersion
  deriving stock (Eq, Show)

-- | Format a baseline error for display
formatBaselineError :: BaselineError -> Text
formatBaselineError = \case
  BaselineNotFound path ->
    "Baseline file not found: " <> T.pack path
  BaselineParseError path err ->
    "Failed to parse baseline file " <> T.pack path <> ": " <> T.pack err
  BaselineWriteError path err ->
    "Failed to write baseline file " <> T.pack path <> ": " <> T.pack err
  BaselineVersionMismatch expected found ->
    "Baseline version mismatch: expected " <> T.pack (show expected)
    <> ", found " <> T.pack (show found)

--------------------------------------------------------------------------------
-- File Operations
--------------------------------------------------------------------------------

-- | Load a baseline from a file
loadBaseline :: FilePath -> IO (Either BaselineError Baseline)
loadBaseline path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ BaselineNotFound path
    else do
      result <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
      case result of
        Left exc ->
          pure $ Left $ BaselineParseError path (show exc)
        Right contents ->
          case eitherDecodeStrict contents of
            Left err ->
              pure $ Left $ BaselineParseError path err
            Right baseline ->
              if baselineVersion baseline > currentBaselineVersion
                then pure $ Left $ BaselineVersionMismatch
                  currentBaselineVersion (baselineVersion baseline)
                else pure $ Right baseline

-- | Save a baseline to a file
saveBaseline :: FilePath -> Baseline -> IO (Either BaselineError ())
saveBaseline path baseline = do
  -- Ensure directory exists
  createDirectoryIfMissing True (takeDirectory path)

  -- Write with pretty formatting
  let prettyConfig = defConfig { confIndent = Spaces 2 }
      contents = encodePretty' prettyConfig baseline
  result <- try (BL.writeFile path contents) :: IO (Either IOException ())
  case result of
    Left exc -> pure $ Left $ BaselineWriteError path (show exc)
    Right () -> pure $ Right ()

-- | Check if a baseline file exists
baselineExists :: FilePath -> IO Bool
baselineExists = doesFileExist

--------------------------------------------------------------------------------
-- Baseline Management
--------------------------------------------------------------------------------

-- | Create a new baseline from diagnostics
createBaseline
  :: [Diagnostic]       -- ^ Current diagnostics to baseline
  -> Maybe Text         -- ^ Optional description
  -> Maybe Text         -- ^ Optional git commit
  -> IO Baseline
createBaseline diagnostics description commit = do
  now <- getCurrentTime
  let entries = map (`mkBaselineEntry` Nothing) diagnostics
  pure Baseline
    { baselineVersion = currentBaselineVersion
    , baselineCreated = now
    , baselineDescription = description
    , baselineCommit = commit
    , baselineEntries = entries
    , baselineMetadata = Map.empty
    }

-- | Update an existing baseline with new diagnostics
--
-- This preserves existing entries that still match current diagnostics
-- and adds new entries for diagnostics not in the baseline.
-- Resolved issues (in baseline but not current) can optionally be kept.
updateBaseline
  :: Baseline           -- ^ Existing baseline
  -> [Diagnostic]       -- ^ Current diagnostics
  -> Bool               -- ^ Keep resolved issues
  -> Maybe Text         -- ^ Optional new commit hash
  -> IO Baseline
updateBaseline baseline diagnostics keepResolved mCommit = do
  now <- getCurrentTime

  let oldEntries = baselineEntries baseline
      newFingerprints = map computeFingerprint diagnostics

      -- Find entries that still match current diagnostics
      stillRelevant entry = any (`fingerprintMatches` entry) newFingerprints

      -- Keep old entries that are still relevant (or all if keepResolved)
      keptEntries
        | keepResolved = oldEntries
        | otherwise = filter stillRelevant oldEntries

      -- Find diagnostics not covered by existing entries
      isCovered diag = any (fingerprintMatches (computeFingerprint diag)) oldEntries
      newDiagnostics = filter (not . isCovered) diagnostics

      -- Create entries for new diagnostics
      newEntries = map (`mkBaselineEntry` Nothing) newDiagnostics

      -- Combine entries, removing duplicates
      allEntries = nubBy sameEntry (keptEntries ++ newEntries)

  pure baseline
    { baselineCreated = now
    , baselineCommit = mCommit
    , baselineEntries = allEntries
    }
  where
    sameEntry a b =
      beFile a == beFile b &&
      beLine a == beLine b &&
      beRule a == beRule b &&
      beFingerprint a == beFingerprint b

-- | Merge multiple baselines into one
--
-- Entries are deduplicated based on fingerprint.
-- The newest metadata is preserved.
mergeBaselines :: [Baseline] -> IO Baseline
mergeBaselines [] = getCurrentTime >>= pure . emptyBaseline
mergeBaselines baselines = do
  now <- getCurrentTime

  -- Combine all entries, keeping unique by fingerprint
  let allEntries = concatMap baselineEntries baselines
      uniqueEntries = nubBy (\a b -> beFingerprint a == beFingerprint b) allEntries

  -- Merge metadata from all baselines
  let mergedMetadata = Map.unions (map baselineMetadata baselines)

  -- Use description from first baseline with one
  let description = foldr (\b acc -> baselineDescription b <|> acc)
        Nothing baselines
        where
          Nothing <|> y = y
          x <|> _ = x

  -- Use most recent commit
  let commit = foldr (\b acc -> baselineCommit b <|> acc)
        Nothing baselines
        where
          Nothing <|> y = y
          x <|> _ = x

  pure Baseline
    { baselineVersion = currentBaselineVersion
    , baselineCreated = now
    , baselineDescription = description
    , baselineCommit = commit
    , baselineEntries = uniqueEntries
    , baselineMetadata = mergedMetadata
    }

--------------------------------------------------------------------------------
-- Default Paths
--------------------------------------------------------------------------------

-- | Default baseline file path
defaultBaselinePath :: FilePath
defaultBaselinePath = ".argus-baseline.json"

-- | Search for a baseline file in standard locations
--
-- Searches in order:
-- 1. Current directory
-- 2. .argus/ directory
-- 3. Project root (if in git repo)
findBaselineFile :: IO (Maybe FilePath)
findBaselineFile = do
  let candidates =
        [ ".argus-baseline.json"
        , ".argus/baseline.json"
        , "argus-baseline.json"
        ]
  findFirst candidates
  where
    findFirst [] = pure Nothing
    findFirst (p:ps) = do
      exists <- doesFileExist p
      if exists
        then pure (Just p)
        else findFirst ps
