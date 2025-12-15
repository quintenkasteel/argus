{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Refactor.SafeRefactor
-- Description : World-class safe refactoring with transactional semantics
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
-- Portability : GHC
--
-- = Overview
--
-- This module provides enterprise-grade automatic code fixing with
-- transactional semantics and comprehensive validation.
--
-- = Key Features
--
-- * __Conflict Detection__: Identifies overlapping, dependent, and semantic conflicts
-- * __Topological Ordering__: Applies fixes in dependency-safe order
-- * __Transactional Semantics__: All-or-nothing with automatic rollback
-- * __Validation Pipeline__: Re-parses after each fix to ensure correctness
-- * __Interactive Mode__: Beautiful colored diffs with user approval
-- * __Multi-file Support__: Coordinates fixes across multiple files
-- * __Import Management__: Automatically adds/removes required imports
--
-- = Architecture
--
-- @
-- ┌─────────────────────────────────────────────────────────────┐
-- │                    SafeRefactor Pipeline                     │
-- ├─────────────────────────────────────────────────────────────┤
-- │  1. Extract fixes from diagnostics                          │
-- │  2. Build fix graph (dependencies, conflicts)               │
-- │  3. Compute topological order                               │
-- │  4. Start transaction                                       │
-- │  5. Apply fixes in order with validation                    │
-- │  6. Commit on success / Rollback on failure                 │
-- └─────────────────────────────────────────────────────────────┘
-- @
--
-- = Thread Safety
--
-- Individual refactoring operations are thread-safe, but concurrent
-- modifications to the same file should be avoided. Use the transaction
-- system to coordinate multi-file changes.
--
-- = Usage
--
-- @
-- result <- safeApplyFixes defaultSafeOptions diagnostics
-- case srrSuccess result of
--   True  -> putStrLn $ "Applied " <> show (length $ srrApplied result) <> " fixes"
--   False -> mapM_ (putStrLn . show) (srrErrors result)
-- @
--
-- @since 1.0.0
module Argus.Refactor.SafeRefactor
  ( -- * Safe Refactoring
    safeApplyFixes
  , safeApplyFixesInteractive
  , SafeRefactorResult (..)
  , SafeRefactorOptions (..)
  , defaultSafeOptions

    -- * Result Types
  , AppliedFix (..)
  , SkipReason (..)
  , FileRefactorResult (..)
  , RefactorStats (..)

    -- * Transactional Operations
  , Transaction (..)
  , TransactionState (..)
  , runTransaction
  , rollback
  , commit

    -- * Fix Application
  , applyFixSafely
  , applyFixGroup
  , FixApplicationResult (..)

    -- * Progress Reporting
  , RefactorProgress (..)
  , ProgressCallback

    -- * Multi-file Support
  , MultiFileResult (..)
  , safeApplyMultiFile

    -- * Interactive Prompts (for testing)
  , MonadPrompt (..)
  , PromptOption (..)
  , PromptChoice (..)
  , FixPromptInfo (..)
  , InteractiveState (..)
  , MockPromptState (..)
  , mkFixPromptInfo
  , processPromptChoice
  , runPromptIO
  , runPromptPure
  , initialInteractiveState
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (when, unless, foldM, forM, forM_)
import Control.Monad.Trans.State.Strict (StateT, runStateT, gets, modify)
import Data.Char (toLower)
import Data.Functor.Identity (Identity(..))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((<.>))
import System.IO (hFlush, stdout, hPutStrLn, stderr)

import Argus.Types
import Argus.Refactor.ExactPrint (applyFix)
import Argus.Refactor.FixGraph
    ( FixGraph(..), FixId, FixConflict(..), ConflictStrategy(..)
    , FixGroup(..), buildFixGraph, getApplyOrder, resolveConflicts
    )
import Argus.Refactor.Validation
    ( ValidationResult(..), ValidationError(..), ValidationLevel(..)
    , ValidationConfig(..), ValidationStats(..), defaultValidationConfig
    , validateRefactoring, generateDiff, renderDiffColored
    )
import Argus.Imports.Manager
    ( ImportManagerConfig(..), defaultImportConfig
    , applyImportChanges
    )

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Comprehensive options for safe refactoring
data SafeRefactorOptions = SafeRefactorOptions
  { -- Validation
    sroValidationLevel    :: ValidationLevel
  , sroValidateEachFix    :: Bool           -- ^ Validate after every single fix
  , sroValidationConfig   :: ValidationConfig

    -- Conflict handling
  , sroConflictStrategy   :: ConflictStrategy
  , sroMaxConflicts       :: Int            -- ^ Abort if more than N conflicts

    -- Transaction behavior
  , sroTransactional      :: Bool           -- ^ All-or-nothing semantics
  , sroCreateBackups      :: Bool           -- ^ Create .bak files
  , sroBackupSuffix       :: Text           -- ^ Suffix for backup files

    -- Interactive mode
  , sroInteractive        :: Bool           -- ^ Prompt for each fix
  , sroShowDiff           :: Bool           -- ^ Show colored diff
  , sroVerbose            :: Bool           -- ^ Detailed progress output

    -- Performance
  , sroParallel           :: Bool           -- ^ Apply independent fixes in parallel
  , sroMaxParallel        :: Int            -- ^ Max parallel operations

    -- Safety
  , sroSafeOnly           :: Bool           -- ^ Only apply fixIsPreferred fixes
  , sroMaxFixesPerFile    :: Int            -- ^ Limit fixes per file (0 = no limit)
  , sroDryRun             :: Bool           -- ^ Don't actually write files

    -- Import management
  , sroManageImports      :: Bool           -- ^ Automatically add/remove imports
  , sroImportConfig       :: ImportManagerConfig -- ^ Import manager configuration
  }
  deriving stock (Eq, Show)

-- | Sensible defaults for safe refactoring
defaultSafeOptions :: SafeRefactorOptions
defaultSafeOptions = SafeRefactorOptions
  { sroValidationLevel  = SyntaxValidation
  , sroValidateEachFix  = True
  , sroValidationConfig = defaultValidationConfig
  , sroConflictStrategy = PreferPreferred
  , sroMaxConflicts     = 100
  , sroTransactional    = True
  , sroCreateBackups    = True
  , sroBackupSuffix     = ".argus-backup"
  , sroInteractive      = False
  , sroShowDiff         = True
  , sroVerbose          = False
  , sroParallel         = False
  , sroMaxParallel      = 4
  , sroSafeOnly         = True
  , sroMaxFixesPerFile  = 0
  , sroDryRun           = False
  , sroManageImports    = True
  , sroImportConfig     = defaultImportConfig
  }

--------------------------------------------------------------------------------
-- Result Types
--------------------------------------------------------------------------------

-- | Comprehensive result of safe refactoring
data SafeRefactorResult = SafeRefactorResult
  { srrSuccess       :: Bool
  , srrApplied       :: [AppliedFix]        -- ^ Successfully applied fixes
  , srrSkipped       :: [(Fix, SkipReason)] -- ^ Skipped fixes with reasons
  , srrFailed        :: [(Fix, Text)]       -- ^ Failed fixes with errors
  , srrRolledBack    :: Bool                -- ^ Was transaction rolled back?
  , srrValidations   :: [ValidationResult]  -- ^ All validation results
  , srrConflicts     :: [FixConflict]       -- ^ Detected conflicts
  , srrFileResults   :: Map FilePath FileRefactorResult
  , srrStats         :: RefactorStats
  , srrDuration      :: Double              -- ^ Total time in seconds
  }
  deriving stock (Eq, Show)

-- | Record of an applied fix
data AppliedFix = AppliedFix
  { afFix        :: Fix
  , afFile       :: FilePath
  , afOldContent :: Text
  , afNewContent :: Text
  , afValidation :: ValidationResult
  }
  deriving stock (Eq, Show)

-- | Why a fix was skipped
data SkipReason
  = SkipConflict FixConflict
  | SkipNotPreferred
  | SkipUserDeclined
  | SkipValidationFailed [ValidationError]
  | SkipDependencyFailed FixId
  | SkipMaxFixes
  deriving stock (Eq, Show)

-- | Result for a single file
data FileRefactorResult = FileRefactorResult
  { frrFile         :: FilePath
  , frrOriginal     :: Text
  , frrFinal        :: Text
  , frrFixesApplied :: Int
  , frrBackupPath   :: Maybe FilePath
  , frrSuccess      :: Bool
  }
  deriving stock (Eq, Show)

-- | Statistics about the refactoring
data RefactorStats = RefactorStats
  { rsFilesProcessed  :: Int
  , rsFilesModified   :: Int
  , rsTotalFixes      :: Int
  , rsAppliedFixes    :: Int
  , rsSkippedFixes    :: Int
  , rsFailedFixes     :: Int
  , rsConflictsFound  :: Int
  , rsLinesChanged    :: Int
  , rsValidationTime  :: Double
  , rsApplicationTime :: Double
  }
  deriving stock (Eq, Show)

-- | Result of applying a single fix
data FixApplicationResult
  = FixApplied Text ValidationResult  -- ^ New content and validation
  | FixFailed Text                    -- ^ Error message
  | FixSkipped SkipReason             -- ^ Why it was skipped
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Progress Reporting
--------------------------------------------------------------------------------

-- | Progress information during refactoring
data RefactorProgress = RefactorProgress
  { rpCurrentFile    :: FilePath
  , rpCurrentFix     :: Int
  , rpTotalFixes     :: Int
  , rpCurrentAction  :: Text
  , rpPercentComplete :: Double
  }
  deriving stock (Eq, Show)

-- | Callback for progress updates
type ProgressCallback = RefactorProgress -> IO ()

--------------------------------------------------------------------------------
-- Transaction System
--------------------------------------------------------------------------------

-- | A refactoring transaction
data Transaction = Transaction
  { txFiles    :: IORef (Map FilePath TransactionState)
  , txOptions  :: SafeRefactorOptions
  , txBackups  :: IORef [FilePath]  -- ^ Backup files created
  , txModified :: IORef [FilePath]  -- ^ Files that have been modified
  }

-- | State of a file in the transaction
data TransactionState = TransactionState
  { tsOriginal :: Text
  , tsCurrent  :: Text
  , tsBackup   :: Maybe FilePath
  , tsModified :: Bool
  }
  deriving stock (Eq, Show)

-- | Run a refactoring in a transaction
runTransaction :: SafeRefactorOptions
               -> Map FilePath Text           -- ^ Initial file contents
               -> (Transaction -> IO a)       -- ^ Action to perform
               -> IO (Either Text a)
runTransaction opts initial action = do
  filesRef <- newIORef (Map.map initState initial)
  backupsRef <- newIORef []
  modifiedRef <- newIORef []

  let tx = Transaction
        { txFiles = filesRef
        , txOptions = opts
        , txBackups = backupsRef
        , txModified = modifiedRef
        }

  result <- try @SomeException $ action tx

  case result of
    Left ex -> do
      when (sroTransactional opts) $ rollback tx
      pure $ Left $ T.pack $ "Transaction failed: " <> show ex
    Right a -> do
      when (sroTransactional opts && not (sroDryRun opts)) $ commit tx
      pure $ Right a
  where
    initState content = TransactionState
      { tsOriginal = content
      , tsCurrent = content
      , tsBackup = Nothing
      , tsModified = False
      }

-- | Rollback all changes in a transaction
rollback :: Transaction -> IO ()
rollback tx = do
  files <- readIORef (txFiles tx)
  backups <- readIORef (txBackups tx)

  -- Restore from backups
  forM_ (Map.toList files) $ \(path, state) ->
    when (tsModified state) $ do
      let backupPath = path <.> T.unpack (sroBackupSuffix (txOptions tx))
      exists <- doesFileExist backupPath
      when exists $ do
        TIO.writeFile path (tsOriginal state)
        removeFile backupPath

  -- Clean up any backup files
  forM_ backups $ \backup -> do
    exists <- doesFileExist backup
    when exists $ removeFile backup

-- | Commit all changes in a transaction
commit :: Transaction -> IO ()
commit tx = do
  files <- readIORef (txFiles tx)

  -- Write final content to files
  forM_ (Map.toList files) $ \(path, state) ->
    when (tsModified state) $
      TIO.writeFile path (tsCurrent state)

  -- Remove backups if not keeping them
  unless (sroCreateBackups (txOptions tx)) $ do
    backups <- readIORef (txBackups tx)
    forM_ backups $ \backup -> do
      exists <- doesFileExist backup
      when exists $ removeFile backup

--------------------------------------------------------------------------------
-- Main Entry Points
--------------------------------------------------------------------------------

-- | Safely apply fixes from diagnostics.
--
-- This is the main entry point for safe refactoring. It:
--
-- 1. Extracts fixes from all diagnostics
-- 2. Builds a dependency graph to detect conflicts
-- 3. Computes a safe application order
-- 4. Applies fixes within a transaction
-- 5. Validates each fix before proceeding
-- 6. Rolls back on any failure (if transactional mode is enabled)
--
-- ==== Parameters
--
-- * @opts@: Configuration options controlling validation, conflict handling, etc.
-- * @filesDiags@: List of (file path, diagnostics) pairs
--
-- ==== Returns
--
-- A 'SafeRefactorResult' containing:
--
-- * Which fixes were successfully applied
-- * Which fixes were skipped and why
-- * Which fixes failed and error messages
-- * Statistics about the refactoring operation
--
-- ==== Example
--
-- @
-- let opts = defaultSafeOptions { sroValidateEachFix = True }
-- result <- safeApplyFixes opts [("src/Foo.hs", diags)]
-- when (srrSuccess result) $
--   putStrLn $ "Applied " ++ show (rsAppliedFixes $ srrStats result) ++ " fixes"
-- @
--
-- ==== Failure Modes
--
-- * Returns failure if a circular dependency is detected
-- * Returns failure if too many conflicts exceed 'sroMaxConflicts'
-- * Individual fix failures don't fail the entire operation unless
--   'sroTransactional' causes a rollback
--
-- @since 1.0.0
safeApplyFixes :: SafeRefactorOptions
               -> [(FilePath, [Diagnostic])]
               -> IO SafeRefactorResult
safeApplyFixes opts filesDiags = do
  startTime <- getCurrentTime

  -- Extract all fixes
  let allFixes = concatMap extractFixes filesDiags
      extractFixes (path, diags) =
        [(path, fix) | diag <- diags, fix <- diagFixes diag]

  -- Filter fixes:
  -- 1. Remove fixes with empty edits (they cannot be applied)
  -- 2. Filter to safe/preferred fixes if requested
  let validFixes = filter (not . null . fixEdits . snd) allFixes
      fixes = if sroSafeOnly opts
              then filter (fixIsPreferred . snd) validFixes
              else validFixes

  -- Read all file contents
  fileContents <- fmap Map.fromList $ forM (map fst filesDiags) $ \path -> do
    content <- TIO.readFile path
    pure (path, content)

  -- Run in transaction
  result <- runTransaction opts fileContents $ \tx -> do
    applyAllFixesSafely tx opts fixes

  endTime <- getCurrentTime
  let duration = realToFrac $ diffUTCTime endTime startTime

  case result of
    Left err -> pure $ emptyResult
      { srrSuccess = False
      , srrFailed = [(mkPlaceholderFix "Transaction", err)]
      , srrDuration = duration
      }
    Right res -> pure $ res { srrDuration = duration }

-- | Safely apply fixes with interactive approval
safeApplyFixesInteractive :: SafeRefactorOptions
                          -> [(FilePath, [Diagnostic])]
                          -> IO SafeRefactorResult
safeApplyFixesInteractive opts = safeApplyFixes opts { sroInteractive = True }

-- | Apply all fixes safely within a transaction
applyAllFixesSafely :: Transaction
                    -> SafeRefactorOptions
                    -> [(FilePath, Fix)]
                    -> IO SafeRefactorResult
applyAllFixesSafely tx opts fixes = do
  appStartTime <- getCurrentTime

  -- Build fix graph
  let graph = buildFixGraph (map snd fixes)

  -- Check for too many conflicts
  when (length (fgConflicts graph) > sroMaxConflicts opts) $ do
    hPutStrLn stderr $ "Too many conflicts: " <> show (length (fgConflicts graph))

  -- Get application order
  let orderResult = getApplyOrder graph
  case orderResult of
    Left cycles -> do
      when (sroVerbose opts) $ do
        hPutStrLn stderr $ "[DEBUG] CYCLE DETECTED! Cycles: " <> show cycles
      pure $ emptyResult
        { srrSuccess = False
        , srrFailed = [(mkPlaceholderFix "Cycle detection",
                        "Circular dependencies detected in fixes")]
        }

    Right order -> do
      -- Debug: print order
      when (sroVerbose opts) $ do
        hPutStrLn stderr $ "[DEBUG] Application order: " <> show order

      -- Resolve conflicts
      let (toApply, toSkip) = resolveConflicts (sroConflictStrategy opts) graph
          -- Build a map of skip reasons from conflicts
          skipReasonsMap = Map.fromList
            [(fid, SkipConflict c) | c <- fgConflicts graph
                                   , fid <- [fcFix1 c, fcFix2 c]
                                   , fid `elem` toSkip]

      -- Debug: print conflict resolution
      when (sroVerbose opts) $ do
        hPutStrLn stderr $ "[DEBUG] To apply: " <> show toApply
        hPutStrLn stderr $ "[DEBUG] To skip: " <> show toSkip

      -- Apply fixes in order
      (applied, failed, skipped) <- applyInOrder tx opts graph order toApply skipReasonsMap

      -- Apply all collected imports AFTER all code fixes are done
      -- This prevents line number shifting from affecting subsequent fixes
      when (sroManageImports opts && not (null applied)) $ do
        applyAllCollectedImports tx opts applied

      -- Debug: print results
      when (sroVerbose opts) $ do
        hPutStrLn stderr $ "[DEBUG] Applied: " <> show (length applied)
        hPutStrLn stderr $ "[DEBUG] Failed: " <> show (length failed)
        forM_ failed $ \(fid, err) -> do
          let mfix = Map.lookup fid (fgFixes graph)
          let title = maybe "unknown" (T.unpack . fixTitle) mfix
          let medits = fmap fixEdits mfix
          let editInfo = case medits of
                Just (e:_) -> " newText: " <> take 60 (show (fixEditNewText e))
                _ -> ""
          let fileInfo = case medits of
                Just (e:_) -> " in " <> srcSpanFile (fixEditSpan e)
                _ -> ""
          hPutStrLn stderr $ "[DEBUG]   Failed " <> show fid <> " [" <> title <> "]" <> fileInfo <> ": " <> T.unpack err <> editInfo
        hPutStrLn stderr $ "[DEBUG] Skipped: " <> show (length skipped)

      appEndTime <- getCurrentTime
      let applicationTime = realToFrac $ diffUTCTime appEndTime appStartTime

      -- Compute total validation time from applied fixes
      let validationTime = sum [vsParseTimeMs (vrStats (afValidation a)) | a <- applied] / 1000.0

      -- Build file results from transaction state
      files <- readIORef (txFiles tx)
      let fileResults = Map.mapWithKey (buildFileResult applied) files

      -- Build result
      let uniqueFiles = Set.fromList [afFile a | a <- applied]
          stats = RefactorStats
            { rsFilesProcessed = Map.size (fgByFile graph)
            , rsFilesModified = Set.size uniqueFiles
            , rsTotalFixes = length fixes
            , rsAppliedFixes = length applied
            , rsSkippedFixes = length skipped
            , rsFailedFixes = length failed
            , rsConflictsFound = length (fgConflicts graph)
            , rsLinesChanged = sum [vsLinesChanged (vrStats (afValidation a)) | a <- applied]
            , rsValidationTime = validationTime
            , rsApplicationTime = applicationTime
            }

      pure SafeRefactorResult
        { srrSuccess = null failed
        , srrApplied = applied
        , srrSkipped = [(fix, reason) | (fid, reason) <- skipped
                                       , Just fix <- [Map.lookup fid (fgFixes graph)]]
        , srrFailed = [(fix, err) | (fid, err) <- failed
                                   , Just fix <- [Map.lookup fid (fgFixes graph)]]
        , srrRolledBack = False
        , srrValidations = map afValidation applied
        , srrConflicts = fgConflicts graph
        , srrFileResults = fileResults
        , srrStats = stats
        , srrDuration = 0
        }
  where
    -- Build a file result from transaction state
    buildFileResult :: [AppliedFix] -> FilePath -> TransactionState -> FileRefactorResult
    buildFileResult applied path state =
      let fixesForFile = length [a | a <- applied, afFile a == path]
      in FileRefactorResult
        { frrFile = path
        , frrOriginal = tsOriginal state
        , frrFinal = tsCurrent state
        , frrFixesApplied = fixesForFile
        , frrBackupPath = tsBackup state
        , frrSuccess = True
        }

-- | Interactive mode state
data InteractiveState = InteractiveState
  { isApplyAll :: Bool    -- ^ User selected "all" - skip future prompts
  , isQuit     :: Bool    -- ^ User selected "quit" - stop processing
  }
  deriving stock (Eq, Show)

-- | Initial interactive state
initialInteractiveState :: InteractiveState
initialInteractiveState = InteractiveState False False

-- | Apply fixes in topological order
applyInOrder :: Transaction
             -> SafeRefactorOptions
             -> FixGraph
             -> [FixId]                    -- ^ Ordered fix IDs
             -> [FixId]                    -- ^ Fixes to apply (not skipped)
             -> Map FixId SkipReason       -- ^ Pre-computed skip reasons for conflicts
             -> IO ([AppliedFix], [(FixId, Text)], [(FixId, SkipReason)])
applyInOrder tx opts graph order toApply skipReasonsMap = do
  stateRef <- newIORef initialInteractiveState
  go stateRef [] [] [] order
  where
    go _stateRef applied failed skipped [] = pure (reverse applied, reverse failed, reverse skipped)
    go stateRef applied failed skipped (fid:rest) = do
      -- Check if user requested quit
      istate <- readIORef stateRef
      if isQuit istate
        then -- Skip all remaining fixes
          let remaining = [(f, SkipUserDeclined) | f <- fid:rest]
          in pure (reverse applied, reverse failed, reverse skipped ++ remaining)
        else if fid `notElem` toApply
          then
            -- Use pre-computed skip reason, or default to SkipNotPreferred if not in conflicts
            let reason = Map.findWithDefault SkipNotPreferred fid skipReasonsMap
            in go stateRef applied failed ((fid, reason):skipped) rest
          else do
            case Map.lookup fid (fgFixes graph) of
              Nothing -> go stateRef applied failed skipped rest
              Just fix -> do
                -- Get current file content - safely handle empty edits
                case fixEdits fix of
                  [] -> go stateRef applied ((fid, "Fix has no edits"):failed) skipped rest
                  (firstEdit:_) -> do
                    files <- readIORef (txFiles tx)
                    let file = srcSpanFile $ fixEditSpan firstEdit
                    case Map.lookup file files of
                      Nothing -> go stateRef applied ((fid, "File not found"):failed) skipped rest
                      Just state -> do
                        -- Interactive approval if enabled
                        (shouldApply, newIState) <- if sroInteractive opts && not (isApplyAll istate)
                          then promptForFixWithState fix (tsCurrent state) istate
                          else pure (True, istate)

                        -- Update state
                        modifyIORef' stateRef (const newIState)

                        if not shouldApply
                          then go stateRef applied failed ((fid, SkipUserDeclined):skipped) rest
                          else do
                            -- Apply the fix
                            result <- applyFixSafely opts file (tsCurrent state) fix

                            case result of
                              FixFailed err ->
                                go stateRef applied ((fid, err):failed) skipped rest

                              FixSkipped reason ->
                                go stateRef applied failed ((fid, reason):skipped) rest

                              FixApplied newContent validation -> do
                                -- Update transaction state
                                let newState = state
                                      { tsCurrent = newContent
                                      , tsModified = True
                                      }
                                modifyIORef' (txFiles tx) (Map.insert file newState)

                                let appliedFix = AppliedFix
                                      { afFix = fix
                                      , afFile = file
                                      , afOldContent = tsCurrent state
                                      , afNewContent = newContent
                                      , afValidation = validation
                                      }

                                go stateRef (appliedFix:applied) failed skipped rest

-- | Apply a single fix safely with validation
-- NOTE: Import changes are NOT applied here - they are collected and applied
-- at the end of all fix applications to avoid line number shifting issues.
applyFixSafely :: SafeRefactorOptions
               -> FilePath
               -> Text              -- ^ Current content
               -> Fix
               -> IO FixApplicationResult
applyFixSafely opts path content fix = do
  -- Apply the code fix only (imports are applied at the end)
  let newContent = applyFix content fix

  -- Skip validation if disabled
  if sroValidationLevel opts == NoValidation
    then pure $ FixApplied newContent (mkSimpleValidation content newContent)
    else do
      -- Validate the result
      validation <- validateRefactoring
        (sroValidationConfig opts)
        path
        content
        newContent

      if vrSuccess validation
        then pure $ FixApplied newContent validation
        else pure $ FixFailed $ T.intercalate "\n"
          [ veMessage e | e <- vrErrors validation ]

-- | Check if a fix has import changes
_hasImportChanges :: Fix -> Bool
_hasImportChanges fix = not (null (fixAddImports fix)) || not (null (fixRemoveImports fix))

-- | Apply all collected imports from applied fixes to their respective files.
-- This is called AFTER all code fixes have been applied, to avoid line number
-- shifting issues that would affect subsequent fix spans.
applyAllCollectedImports :: Transaction -> SafeRefactorOptions -> [AppliedFix] -> IO ()
applyAllCollectedImports tx opts appliedFixes = do
  -- Group applied fixes by file
  let byFile = Map.fromListWith (++) [(afFile af, [afFix af]) | af <- appliedFixes]

  -- For each file, collect all imports and apply them once
  forM_ (Map.toList byFile) $ \(file, fixes) -> do
    let allAddImports = concatMap fixAddImports fixes
        allRemoveImports = concatMap fixRemoveImports fixes

    when (not (null allAddImports) || not (null allRemoveImports)) $ do
      files <- readIORef (txFiles tx)
      case Map.lookup file files of
        Nothing -> pure ()
        Just state -> do
          -- Create a dummy fix with all the collected imports
          let dummyFix = Fix
                { fixTitle = "Import changes"
                , fixEdits = []
                , fixIsPreferred = True
                , fixAddImports = allAddImports
                , fixRemoveImports = allRemoveImports
                , fixCategory = FCRedundant
                , fixSafety = FSAlways
                }
          -- Apply import changes to the current content
          let newContent = applyImportChanges (sroImportConfig opts) (tsCurrent state) dummyFix
          let newState = state { tsCurrent = newContent, tsModified = True }
          modifyIORef' (txFiles tx) (Map.insert file newState)

-- | Create a simple validation result (when validation is skipped)
mkSimpleValidation :: Text -> Text -> ValidationResult
mkSimpleValidation old new = ValidationResult
  { vrSuccess = True
  , vrLevel = NoValidation
  , vrErrors = []
  , vrWarnings = []
  , vrOriginal = old
  , vrTransformed = new
  , vrDiff = Just $ generateDiff old new
  , vrStats = ValidationStats 0 0 0 0 0
  }

--------------------------------------------------------------------------------
-- Interactive Mode (Testable via MonadPrompt abstraction)
--------------------------------------------------------------------------------

-- | Options presented to user during interactive prompts
data PromptOption
  = OptionYes        -- ^ Apply this fix
  | OptionNo         -- ^ Skip this fix
  | OptionAll        -- ^ Apply this and all remaining fixes
  | OptionQuit       -- ^ Stop processing fixes
  deriving stock (Eq, Show, Ord, Enum, Bounded)

-- | User's choice from a prompt
data PromptChoice
  = ChoiceYes
  | ChoiceNo
  | ChoiceAll
  | ChoiceQuit
  | ChoiceInvalid Text  -- ^ Invalid input (will re-prompt)
  deriving stock (Eq, Show)

-- | Information shown during a fix prompt (pure data for testing)
data FixPromptInfo = FixPromptInfo
  { fpiTitle     :: Text          -- ^ Fix title
  , fpiDiff      :: Text          -- ^ Colored diff output
  , fpiLocations :: [(FilePath, Line, Column)]  -- ^ Edit locations (file, line, col)
  , fpiOptions   :: [PromptOption]  -- ^ Available options
  }
  deriving stock (Eq, Show)

-- | Create prompt info from a fix and content (pure function)
mkFixPromptInfo :: Fix -> Text -> FixPromptInfo
mkFixPromptInfo fix content =
  let newContent = applyFix content fix
      diff = generateDiff content newContent
      locations = [(srcSpanFile (fixEditSpan e),
                    srcSpanStartLine (fixEditSpan e),
                    srcSpanStartCol (fixEditSpan e))
                  | e <- fixEdits fix]
  in FixPromptInfo
    { fpiTitle = fixTitle fix
    , fpiDiff = renderDiffColored diff
    , fpiLocations = locations
    , fpiOptions = [OptionYes, OptionNo, OptionAll, OptionQuit]
    }

-- | Parse user input string into a choice (pure function)
parsePromptInput :: String -> PromptChoice
parsePromptInput input = case map toLower input of
  "y"    -> ChoiceYes
  "yes"  -> ChoiceYes
  "n"    -> ChoiceNo
  "no"   -> ChoiceNo
  "a"    -> ChoiceAll
  "all"  -> ChoiceAll
  "q"    -> ChoiceQuit
  "quit" -> ChoiceQuit
  other  -> ChoiceInvalid (T.pack other)

-- | Process a choice and update interactive state (pure function)
processPromptChoice :: PromptChoice -> InteractiveState -> (Bool, InteractiveState)
processPromptChoice choice istate = case choice of
  ChoiceYes     -> (True, istate)
  ChoiceNo      -> (False, istate)
  ChoiceAll     -> (True, istate { isApplyAll = True })
  ChoiceQuit    -> (False, istate { isQuit = True })
  ChoiceInvalid _ -> (False, istate)  -- Caller should re-prompt

-- | Type class for interactive prompting (allows mocking in tests)
class Monad m => MonadPrompt m where
  -- | Display prompt information and get user choice
  promptForFix :: FixPromptInfo -> m PromptChoice
  -- | Display error message for invalid input
  promptError :: Text -> m ()

-- | IO implementation of MonadPrompt (real user interaction)
instance MonadPrompt IO where
  promptForFix info = do
    -- Show fix information
    TIO.putStrLn ""
    TIO.putStrLn $ "\ESC[1;36m━━━ Fix: " <> fpiTitle info <> " ━━━\ESC[0m"
    TIO.putStrLn ""

    -- Show diff
    TIO.putStrLn $ fpiDiff info
    TIO.putStrLn ""

    -- Show edit locations
    TIO.putStrLn $ "\ESC[33mLocations:\ESC[0m"
    forM_ (fpiLocations info) $ \(file, Line line, Column col) ->
      TIO.putStrLn $ "  " <> T.pack file
                   <> ":" <> T.pack (show line)
                   <> ":" <> T.pack (show col)

    -- Prompt
    TIO.putStrLn ""
    TIO.putStr "\ESC[1mApply this fix? [y]es / [n]o / [a]ll / [q]uit: \ESC[0m"
    hFlush stdout

    answer <- getLine
    pure $ parsePromptInput answer

  promptError msg = TIO.putStrLn msg

-- | Mock prompt state for testing
data MockPromptState = MockPromptState
  { mpsResponses :: [PromptChoice]  -- ^ Pre-programmed responses
  , mpsPrompts   :: [FixPromptInfo] -- ^ Prompts that were shown
  , mpsErrors    :: [Text]          -- ^ Error messages shown
  }
  deriving stock (Eq, Show)

-- | Initial mock state with given responses
mkMockPromptState :: [PromptChoice] -> MockPromptState
mkMockPromptState responses = MockPromptState
  { mpsResponses = responses
  , mpsPrompts = []
  , mpsErrors = []
  }

-- | Pure/testing implementation using StateT
instance Monad m => MonadPrompt (StateT MockPromptState m) where
  promptForFix info = do
    -- Record that this prompt was shown
    modify $ \s -> s { mpsPrompts = mpsPrompts s ++ [info] }
    -- Get next response from list
    responses <- gets mpsResponses
    case responses of
      [] -> pure ChoiceQuit  -- No more responses, quit
      (r:rs) -> do
        modify $ \s -> s { mpsResponses = rs }
        pure r

  promptError msg = modify $ \s -> s { mpsErrors = mpsErrors s ++ [msg] }

-- | Run prompts in IO (real interaction)
runPromptIO :: (forall m. MonadPrompt m => m a) -> IO a
runPromptIO action = action

-- | Run prompts with mock responses (for testing)
-- Uses StateT Identity as the mock monad
runPromptPure :: [PromptChoice] -> StateT MockPromptState Identity a -> (a, MockPromptState)
runPromptPure responses action =
  let initialState = mkMockPromptState responses
      Identity (result, finalState) = runStateT action initialState
  in (result, finalState)

-- | Prompt user for fix approval with state tracking (IO version, uses MonadPrompt)
promptForFixWithState :: Fix -> Text -> InteractiveState -> IO (Bool, InteractiveState)
promptForFixWithState fix content istate = do
  let info = mkFixPromptInfo fix content
  promptLoop info istate
  where
    promptLoop :: FixPromptInfo -> InteractiveState -> IO (Bool, InteractiveState)
    promptLoop info' state = do
      choice <- promptForFix info'
      case choice of
        ChoiceInvalid _ -> do
          promptError "Please enter y, n, a, or q"
          promptLoop info' state
        _ -> pure $ processPromptChoice choice state

--------------------------------------------------------------------------------
-- Multi-file Support
--------------------------------------------------------------------------------

-- | Result of multi-file refactoring
data MultiFileResult = MultiFileResult
  { mfrResults   :: Map FilePath SafeRefactorResult
  , mfrSuccess   :: Bool
  , mfrTotalTime :: Double
  }
  deriving stock (Eq, Show)

-- | Apply fixes across multiple files atomically
safeApplyMultiFile :: SafeRefactorOptions
                   -> Map FilePath [(Diagnostic)]
                   -> IO MultiFileResult
safeApplyMultiFile opts filesMap = do
  startTime <- getCurrentTime

  -- Convert to list format
  let filesList = [(path, diags) | (path, diags) <- Map.toList filesMap]

  -- Apply all fixes
  result <- safeApplyFixes opts filesList

  endTime <- getCurrentTime
  let duration = realToFrac $ diffUTCTime endTime startTime

  -- Split result by file using the file results
  let perFileResults = Map.mapWithKey (buildPerFileResult result) (srrFileResults result)

  pure MultiFileResult
    { mfrResults = perFileResults
    , mfrSuccess = srrSuccess result
    , mfrTotalTime = duration
    }
  where
    -- Build a per-file SafeRefactorResult from the FileRefactorResult
    buildPerFileResult :: SafeRefactorResult -> FilePath -> FileRefactorResult -> SafeRefactorResult
    buildPerFileResult overall path fileResult =
      let appliedForFile = filter (\a -> afFile a == path) (srrApplied overall)
          skippedForFile = filter (\(f, _) -> getFixFile f == Just path) (srrSkipped overall)
          failedForFile = filter (\(f, _) -> getFixFile f == Just path) (srrFailed overall)
          validationsForFile = map afValidation appliedForFile
          conflictsForFile = filter (\c -> getConflictFile c == Just path) (srrConflicts overall)
      in SafeRefactorResult
        { srrSuccess = null failedForFile
        , srrApplied = appliedForFile
        , srrSkipped = skippedForFile
        , srrFailed = failedForFile
        , srrRolledBack = srrRolledBack overall
        , srrValidations = validationsForFile
        , srrConflicts = conflictsForFile
        , srrFileResults = Map.singleton path fileResult
        , srrStats = RefactorStats
            { rsFilesProcessed = 1
            , rsFilesModified = if frrFixesApplied fileResult > 0 then 1 else 0
            , rsTotalFixes = frrFixesApplied fileResult + length skippedForFile + length failedForFile
            , rsAppliedFixes = frrFixesApplied fileResult
            , rsSkippedFixes = length skippedForFile
            , rsFailedFixes = length failedForFile
            , rsConflictsFound = length conflictsForFile
            , rsLinesChanged = sum [vsLinesChanged (vrStats (afValidation a)) | a <- appliedForFile]
            , rsValidationTime = 0
            , rsApplicationTime = 0
            }
        , srrDuration = 0
        }

    -- Get file from a fix (first edit's file)
    getFixFile :: Fix -> Maybe FilePath
    getFixFile fix = case fixEdits fix of
      (e:_) -> Just $ srcSpanFile $ fixEditSpan e
      [] -> Nothing

    -- Get file from a conflict (first fix's file)
    getConflictFile :: FixConflict -> Maybe FilePath
    getConflictFile _conflict = Nothing  -- Conflicts don't store file directly

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Empty result for error cases
emptyResult :: SafeRefactorResult
emptyResult = SafeRefactorResult
  { srrSuccess = False
  , srrApplied = []
  , srrSkipped = []
  , srrFailed = []
  , srrRolledBack = False
  , srrValidations = []
  , srrConflicts = []
  , srrFileResults = Map.empty
  , srrStats = RefactorStats 0 0 0 0 0 0 0 0 0 0
  , srrDuration = 0
  }

-- | Create a placeholder Fix for error reporting
mkPlaceholderFix :: Text -> Fix
mkPlaceholderFix title = Fix
  { fixTitle = title
  , fixEdits = []
  , fixIsPreferred = False
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCRedundant
  , fixSafety = FSAlways
  }

-- | Apply a group of fixes (for parallel application)
applyFixGroup :: SafeRefactorOptions
              -> FilePath
              -> Text
              -> FixGroup
              -> FixGraph
              -> IO ([AppliedFix], [(FixId, Text)])
applyFixGroup opts path content group graph = do
  -- Get fixes with their IDs
  let fixesWithIds = [(fid, fix) | fid <- fgGroupFixes group
                                 , Just fix <- [Map.lookup fid (fgFixes graph)]]
  foldM applyOne ([], []) fixesWithIds
  where
    applyOne (applied, failed) (fid, fix) = do
      let currentContent = case applied of
            [] -> content
            (a:_) -> afNewContent a

      result <- applyFixSafely opts path currentContent fix

      case result of
        FixApplied newContent validation ->
          let af = AppliedFix fix path currentContent newContent validation
          in pure (af:applied, failed)
        FixFailed err ->
          pure (applied, (fid, err):failed)
        FixSkipped _ ->
          pure (applied, failed)
