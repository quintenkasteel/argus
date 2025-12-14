{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Refactor.Engine
-- Description : Main refactoring engine orchestrating safe code transformations
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides the main refactoring engine that coordinates
-- the application of fixes and transformations to source code.
--
-- == New Features (v2.0)
--
-- * __Conflict Detection__: Automatically detects overlapping fixes
-- * __Safe Application__: Validates syntax after each fix
-- * __Transactional__: All-or-nothing semantics with rollback
-- * __Interactive Mode__: Review each fix with colored diffs
module Argus.Refactor.Engine
  ( -- * Safe Refactoring (Recommended)
    refactorSafely
  , refactorInteractive
  , EngineResult (..)
  , EngineOptions (..)
  , RefactorMode (..)
  , defaultEngineOptions

    -- * Legacy API (for backwards compatibility)
  , applyDiagnosticFixes
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

import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)
import System.FilePath ((<.>))

import Argus.Refactor.ExactPrint (applyFix)
import Argus.Refactor.SafeRefactor
import Argus.Refactor.Validation
import Argus.Refactor.FixGraph (ConflictStrategy(..))
import Argus.Types

--------------------------------------------------------------------------------
-- New Engine API
--------------------------------------------------------------------------------

-- | Comprehensive options for the refactoring engine
data EngineOptions = EngineOptions
  { -- Mode selection
    eoMode           :: RefactorMode
    -- Validation
  , eoValidate       :: Bool              -- ^ Validate after each fix
  , eoValidateLevel  :: ValidationLevel   -- ^ How strict
    -- Safety
  , eoSafeOnly       :: Bool              -- ^ Only preferred fixes
  , eoTransactional  :: Bool              -- ^ All-or-nothing
  , eoBackup         :: Bool              -- ^ Create backups
    -- Conflict handling
  , eoConflictStrat  :: ConflictStrategy
    -- Output
  , eoDryRun         :: Bool              -- ^ Don't actually write
  , eoVerbose        :: Bool              -- ^ Detailed output
  , eoShowDiff       :: Bool              -- ^ Show diffs
  }
  deriving stock (Eq, Show)

-- | Refactoring mode
data RefactorMode
  = ModeAuto          -- ^ Apply all fixes automatically
  | ModeInteractive   -- ^ Prompt for each fix
  | ModePreview       -- ^ Show what would change, don't apply
  deriving stock (Eq, Show)

-- | Default engine options (safe and validated)
defaultEngineOptions :: EngineOptions
defaultEngineOptions = EngineOptions
  { eoMode          = ModeAuto
  , eoValidate      = True
  , eoValidateLevel = SyntaxValidation
  , eoSafeOnly      = True
  , eoTransactional = True
  , eoBackup        = True
  , eoConflictStrat = PreferPreferred
  , eoDryRun        = False
  , eoVerbose       = False
  , eoShowDiff      = True
  }

-- | Result from the new engine
data EngineResult = EngineResult
  { erSuccess      :: Bool
  , erFilesChanged :: Int
  , erFixesApplied :: Int
  , erFixesSkipped :: Int
  , erFixesFailed  :: Int
  , erConflicts    :: Int
  , erDetails      :: SafeRefactorResult
  }
  deriving stock (Eq, Show)

-- | Safely refactor files using the new engine
refactorSafely :: EngineOptions
               -> [(FilePath, [Diagnostic])]
               -> IO EngineResult
refactorSafely opts filesDiags = do
  let safeOpts = toSafeOptions opts

  result <- safeApplyFixes safeOpts filesDiags

  pure EngineResult
    { erSuccess = srrSuccess result
    , erFilesChanged = rsFilesModified (srrStats result)
    , erFixesApplied = rsAppliedFixes (srrStats result)
    , erFixesSkipped = rsSkippedFixes (srrStats result)
    , erFixesFailed = rsFailedFixes (srrStats result)
    , erConflicts = rsConflictsFound (srrStats result)
    , erDetails = result
    }

-- | Interactive refactoring with user prompts
refactorInteractive :: [(FilePath, [Diagnostic])] -> IO EngineResult
refactorInteractive = refactorSafely defaultEngineOptions
  { eoMode = ModeInteractive }

-- | Convert engine options to safe refactor options
toSafeOptions :: EngineOptions -> SafeRefactorOptions
toSafeOptions EngineOptions{..} = defaultSafeOptions
  { sroValidationLevel = if eoValidate then eoValidateLevel else NoValidation
  , sroValidateEachFix = eoValidate
  , sroConflictStrategy = eoConflictStrat
  , sroTransactional = eoTransactional
  , sroCreateBackups = eoBackup
  , sroInteractive = eoMode == ModeInteractive
  , sroShowDiff = eoShowDiff
  , sroVerbose = eoVerbose
  , sroSafeOnly = eoSafeOnly
  , sroDryRun = eoDryRun || eoMode == ModePreview
  }

--------------------------------------------------------------------------------
-- Legacy Refactor Options (Backwards Compatibility)
--------------------------------------------------------------------------------

-- | Legacy options for refactoring
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
-- Legacy Refactor Results
--------------------------------------------------------------------------------

-- | Result of a refactoring operation (legacy)
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
-- Legacy Main Refactoring Functions
--------------------------------------------------------------------------------

-- | Apply fixes from diagnostics to a file (legacy, uses new engine internally)
applyDiagnosticFixes :: RefactorOptions -> FilePath -> [Diagnostic] -> IO RefactorResult
applyDiagnosticFixes opts path diagnostics = do
  content <- TE.decodeUtf8 <$> BS.readFile path

  -- Use new safe refactoring engine
  let engineOpts = defaultEngineOptions
        { eoSafeOnly = roSafeOnly opts
        , eoDryRun = roPreview opts
        , eoBackup = roBackup opts
        , eoMode = if roInteractive opts then ModeInteractive else ModeAuto
        }

  result <- refactorSafely engineOpts [(path, diagnostics)]

  -- Extract final content from result
  let finalContent = case srrApplied (erDetails result) of
        [] -> content
        applied -> afNewContent (last applied)

  pure RefactorResult
    { rrFilePath = path
    , rrFixesApplied = erFixesApplied result
    , rrNewContent = finalContent
    , rrOriginal = content
    , rrSuccess = erSuccess result
    , rrMessage = if erSuccess result
                  then Nothing
                  else Just $ "Failed to apply " <> T.pack (show (erFixesFailed result)) <> " fix(es)"
    }

-- | Apply all fixes to source content (legacy, for simple use cases)
applyAllFixes :: RefactorOptions -> FilePath -> Text -> [Fix] -> IO RefactorResult
applyAllFixes opts path original fixes = do
  when (roBackup opts) $ do
    let backupPath = path <.> "bak"
    TIO.writeFile backupPath original

  -- Convert to diagnostics format for new engine
  let fakeDiag fix = Diagnostic
        { diagSpan = case fixEdits fix of
            (e:_) -> fixEditSpan e
            [] -> mkSrcSpanRaw path 1 1 1 1
        , diagSeverity = Suggestion
        , diagKind = CodePattern
        , diagMessage = fixTitle fix
        , diagCode = Nothing
        , diagFixes = [fix]
        , diagRelated = []
        }
      diagnostics = map fakeDiag fixes

  -- Use new engine
  let engineOpts = defaultEngineOptions
        { eoSafeOnly = roSafeOnly opts
        , eoDryRun = roPreview opts
        , eoBackup = False  -- We already created backup above
        , eoMode = if roInteractive opts then ModeInteractive else ModeAuto
        }

  result <- refactorSafely engineOpts [(path, diagnostics)]

  let finalContent = case srrApplied (erDetails result) of
        [] -> original
        applied -> afNewContent (last applied)

  pure RefactorResult
    { rrFilePath = path
    , rrFixesApplied = erFixesApplied result
    , rrNewContent = finalContent
    , rrOriginal = original
    , rrSuccess = erSuccess result
    , rrMessage = Nothing
    }

-- | Preview a fix without applying it
previewFix :: Text -> Fix -> Text
previewFix = applyFix

--------------------------------------------------------------------------------
-- Legacy File Operations
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
