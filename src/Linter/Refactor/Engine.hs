{-# LANGUAGE StrictData #-}

-- |
-- Module      : Linter.Refactor.Engine
-- Description : Main refactoring engine
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides the main refactoring engine that coordinates
-- the application of fixes and transformations to source code.
module Linter.Refactor.Engine
  ( -- * Refactoring operations
    applyDiagnosticFixes
  , applyAllFixes
  , previewFix

    -- * File operations
  , refactorFile
  , refactorFiles
  , RefactorResult (..)

    -- * Options
  , RefactorOptions (..)
  , defaultRefactorOptions
  ) where

import Control.Monad (when, forM)
import Data.ByteString qualified as BS
import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import System.Directory (copyFile, doesFileExist)
import System.FilePath ((<.>))

import Linter.Refactor.ExactPrint
import Linter.Types

--------------------------------------------------------------------------------
-- Refactor Options
--------------------------------------------------------------------------------

-- | Options for refactoring
data RefactorOptions = RefactorOptions
  { roSafeOnly    :: Bool  -- ^ Only apply safe fixes
  , roPreview     :: Bool  -- ^ Show preview before applying
  , roBackup      :: Bool  -- ^ Create backup files
  , roInteractive :: Bool  -- ^ Prompt for each fix
  }
  deriving stock (Eq, Show)

-- | Default refactoring options
defaultRefactorOptions :: RefactorOptions
defaultRefactorOptions = RefactorOptions
  { roSafeOnly    = True
  , roPreview     = True
  , roBackup      = True
  , roInteractive = False
  }

--------------------------------------------------------------------------------
-- Refactor Results
--------------------------------------------------------------------------------

-- | Result of a refactoring operation
data RefactorResult = RefactorResult
  { rrFilePath     :: FilePath
  , rrFixesApplied :: Int
  , rrNewContent   :: Text
  , rrOriginal     :: Text
  , rrSuccess      :: Bool
  , rrMessage      :: Maybe Text
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Main Refactoring Functions
--------------------------------------------------------------------------------

-- | Apply fixes from diagnostics to a file
applyDiagnosticFixes :: RefactorOptions -> FilePath -> [Diagnostic] -> IO RefactorResult
applyDiagnosticFixes opts path diagnostics = do
  content <- TE.decodeUtf8 <$> BS.readFile path
  let fixes = concatMap diagFixes diagnostics
      filteredFixes = if roSafeOnly opts
                      then filter fixIsPreferred fixes
                      else fixes
  applyAllFixes opts path content filteredFixes

-- | Apply all fixes to source content
applyAllFixes :: RefactorOptions -> FilePath -> Text -> [Fix] -> IO RefactorResult
applyAllFixes opts path original fixes = do
  when (roBackup opts) $ do
    let backupPath = path <.> "bak"
    TIO.writeFile backupPath original

  let newContent = foldl' applyFix original fixes

  when (not (roPreview opts)) $
    TIO.writeFile path newContent

  pure RefactorResult
    { rrFilePath = path
    , rrFixesApplied = length fixes
    , rrNewContent = newContent
    , rrOriginal = original
    , rrSuccess = True
    , rrMessage = Nothing
    }

-- | Preview a fix without applying it
previewFix :: Text -> Fix -> Text
previewFix = applyFix

--------------------------------------------------------------------------------
-- File Operations
--------------------------------------------------------------------------------

-- | Refactor a single file with given diagnostics
refactorFile :: RefactorOptions -> FilePath -> [Diagnostic] -> IO RefactorResult
refactorFile opts path diagnostics = do
  exists <- doesFileExist path
  if not exists
    then pure RefactorResult
      { rrFilePath = path
      , rrFixesApplied = 0
      , rrNewContent = ""
      , rrOriginal = ""
      , rrSuccess = False
      , rrMessage = Just "File does not exist"
      }
    else applyDiagnosticFixes opts path diagnostics

-- | Refactor multiple files
refactorFiles :: RefactorOptions -> [(FilePath, [Diagnostic])] -> IO [RefactorResult]
refactorFiles opts = mapM (uncurry (refactorFile opts))
