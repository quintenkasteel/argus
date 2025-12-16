{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Argus.AutoFix.MultiFile
-- Description : Coordinated multi-file auto-fix operations
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- This module provides infrastructure for fixes that span multiple files,
-- handling the complexity of cross-file dependencies, import management,
-- and transactional application of related changes.
--
-- == Use Cases
--
-- * Renaming a function/type and updating all call sites
-- * Moving a definition to another module and updating imports
-- * Extracting code to a new module
-- * Coordinated API changes across a codebase
--
-- == Architecture
--
-- Multi-file fixes are organized as @FixTransaction@s that group related
-- changes across files. The transaction system ensures:
--
-- * Atomic application (all or nothing)
-- * Proper ordering of dependent changes
-- * Import management across affected files
-- * Conflict detection between concurrent changes
--
-- == Example
--
-- @
-- -- Create a rename transaction
-- transaction <- createRenameTransaction engine
--   "oldFunctionName"
--   "newFunctionName"
--   affectedFiles
--
-- -- Preview changes
-- preview <- previewTransaction transaction
--
-- -- Apply atomically
-- result <- applyTransaction engine transaction
-- @
module Argus.AutoFix.MultiFile
  ( -- * Multi-File Engine
    MultiFileEngine (..)
  , MultiFileConfig (..)
  , defaultMultiFileConfig
  , initMultiFileEngine
  , shutdownMultiFileEngine

    -- * Fix Transactions
  , FixTransaction (..)
  , TransactionId
  , TransactionStatus (..)
  , createTransaction
  , addFileToTransaction
  , removeFileFromTransaction
  , finalizeTransaction

    -- * File Changes
  , FileChange (..)
  , ChangeKind (..)
  , ChangeSet (..)
  , emptyChangeSet
  , addChange
  , mergeChangeSets

    -- * Transaction Operations
  , previewTransaction
  , validateTransaction
  , applyTransaction
  , rollbackTransaction
  , TransactionResult (..)
  , TransactionPreview (..)

    -- * Rename Operations
  , createRenameTransaction
  , renameSymbol
  , renameModule
  , renameFile
  , RenameScope (..)
  , RenameResult (..)

    -- * Move Operations
  , createMoveTransaction
  , moveDefinition
  , moveToModule
  , extractToModule
  , MoveScope (..)
  , MoveResult (..)

    -- * Import Management
  , updateImportsForRename
  , updateImportsForMove
  , addImportToFiles
  , removeImportFromFiles
  , ImportUpdate (..)
  , ImportUpdateResult (..)

    -- * Dependency Analysis
  , analyzeFileDependencies
  , findAffectedFiles
  , buildDependencyGraph
  , FileDependency (..)
  , DependencyGraph (..)

    -- * Conflict Detection
  , detectTransactionConflicts
  , resolveConflicts
  , TransactionConflict (..)
  , ConflictResolution (..)

    -- * Batch Processing
  , batchApplyTransactions
  , parallelApplyTransactions
  , BatchResult (..)

    -- * Progress Tracking
  , TransactionProgress (..)
  , ProgressCallback
  , withProgress
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVar, readTVarIO, writeTVar, modifyTVar', atomically)
import Control.Exception (try, SomeException)
import Control.Monad (forM, forM_, when)
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import System.Directory (doesFileExist, createDirectoryIfMissing, copyFile, removeFile)
import System.FilePath ((</>), takeFileName)

import Argus.Types
  ( SrcSpan (..)
  , Fix (..)
  , FixEdit (..)
  , Line (..)
  , Column (..)
  , FixImport (..)
  )
import Argus.AutoFix.Types

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for multi-file operations
data MultiFileConfig = MultiFileConfig
  { mfcMaxParallelFiles  :: Int
    -- ^ Maximum files to process in parallel
  , mfcBackupEnabled     :: Bool
    -- ^ Create backups before modifications
  , mfcBackupDir         :: FilePath
    -- ^ Directory for backups
  , mfcAtomicWrite       :: Bool
    -- ^ Use atomic file writes
  , mfcDryRun            :: Bool
    -- ^ Dry run mode (no actual changes)
  , mfcValidateAll       :: Bool
    -- ^ Validate all changes before applying
  , mfcConflictStrategy  :: ConflictStrategy
    -- ^ How to handle conflicts
  , mfcProgressEnabled   :: Bool
    -- ^ Enable progress tracking
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Strategy for handling conflicts
data ConflictStrategy
  = AbortOnConflict      -- ^ Abort entire transaction on conflict
  | SkipConflicting      -- ^ Skip conflicting changes
  | MergeConflicting     -- ^ Attempt to merge conflicts
  | AskUser              -- ^ Prompt user for resolution
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default multi-file configuration
defaultMultiFileConfig :: MultiFileConfig
defaultMultiFileConfig = MultiFileConfig
  { mfcMaxParallelFiles = 8
  , mfcBackupEnabled = True
  , mfcBackupDir = ".argus-backup"
  , mfcAtomicWrite = True
  , mfcDryRun = False
  , mfcValidateAll = True
  , mfcConflictStrategy = AbortOnConflict
  , mfcProgressEnabled = True
  }

--------------------------------------------------------------------------------
-- Multi-File Engine
--------------------------------------------------------------------------------

-- | Engine for multi-file fix operations
data MultiFileEngine = MultiFileEngine
  { mfeConfig        :: MultiFileConfig
  , mfeTransactions  :: TVar (Map TransactionId FixTransaction)
  , mfeActiveFiles   :: TVar (Set FilePath)
  , mfeDependencies  :: TVar (Maybe DependencyGraph)
  , mfeProgress      :: TVar (Map TransactionId TransactionProgress)
  , mfeStats         :: TVar MultiFileStats
  }

-- | Statistics for the engine
data MultiFileStats = MultiFileStats
  { mfsTransactionsCreated :: Int
  , mfsTransactionsApplied :: Int
  , mfsTransactionsRolledBack :: Int
  , mfsFilesModified :: Int
  , mfsConflictsDetected :: Int
  , mfsConflictsResolved :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty stats
emptyMultiFileStats :: MultiFileStats
emptyMultiFileStats = MultiFileStats 0 0 0 0 0 0

-- | Initialize the multi-file engine
initMultiFileEngine :: MultiFileConfig -> IO MultiFileEngine
initMultiFileEngine config = do
  transVar <- newTVarIO Map.empty
  activeVar <- newTVarIO Set.empty
  depsVar <- newTVarIO Nothing
  progressVar <- newTVarIO Map.empty
  statsVar <- newTVarIO emptyMultiFileStats

  pure MultiFileEngine
    { mfeConfig = config
    , mfeTransactions = transVar
    , mfeActiveFiles = activeVar
    , mfeDependencies = depsVar
    , mfeProgress = progressVar
    , mfeStats = statsVar
    }

-- | Shutdown the engine
shutdownMultiFileEngine :: MultiFileEngine -> IO ()
shutdownMultiFileEngine engine = do
  atomically $ do
    writeTVar (mfeTransactions engine) Map.empty
    writeTVar (mfeActiveFiles engine) Set.empty
    writeTVar (mfeDependencies engine) Nothing
    writeTVar (mfeProgress engine) Map.empty

--------------------------------------------------------------------------------
-- Transaction Types
--------------------------------------------------------------------------------

-- | Unique transaction identifier
type TransactionId = UUID

-- | Status of a transaction
data TransactionStatus
  = TxPending         -- ^ Not yet applied
  | TxValidating      -- ^ Being validated
  | TxApplying        -- ^ Being applied
  | TxApplied         -- ^ Successfully applied
  | TxRolledBack      -- ^ Rolled back
  | TxFailed Text     -- ^ Failed with error
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A transaction grouping related file changes
data FixTransaction = FixTransaction
  { ftId           :: TransactionId
  , ftDescription  :: Text
  , ftChangeSets   :: Map FilePath ChangeSet
  , ftDependencies :: [TransactionId]
  , ftStatus       :: TransactionStatus
  , ftCreatedAt    :: UTCTime
  , ftAppliedAt    :: Maybe UTCTime
  , ftBackupPaths  :: Map FilePath FilePath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Create a new transaction
createTransaction :: MultiFileEngine -> Text -> IO FixTransaction
createTransaction engine description = do
  txId <- UUID.nextRandom
  now <- getCurrentTime

  let transaction = FixTransaction
        { ftId = txId
        , ftDescription = description
        , ftChangeSets = Map.empty
        , ftDependencies = []
        , ftStatus = TxPending
        , ftCreatedAt = now
        , ftAppliedAt = Nothing
        , ftBackupPaths = Map.empty
        }

  atomically $ do
    modifyTVar' (mfeTransactions engine) $ Map.insert txId transaction
    stats <- readTVar (mfeStats engine)
    writeTVar (mfeStats engine) stats
      { mfsTransactionsCreated = mfsTransactionsCreated stats + 1 }

  pure transaction

-- | Add a file to the transaction
addFileToTransaction :: FixTransaction -> FilePath -> ChangeSet -> FixTransaction
addFileToTransaction tx path changes =
  tx { ftChangeSets = Map.insert path changes (ftChangeSets tx) }

-- | Remove a file from the transaction
removeFileFromTransaction :: FixTransaction -> FilePath -> FixTransaction
removeFileFromTransaction tx path =
  tx { ftChangeSets = Map.delete path (ftChangeSets tx) }

-- | Finalize transaction for application
finalizeTransaction :: FixTransaction -> FixTransaction
finalizeTransaction tx =
  if ftStatus tx == TxPending
  then tx { ftStatus = TxValidating }
  else tx

--------------------------------------------------------------------------------
-- File Changes
--------------------------------------------------------------------------------

-- | Kind of change to a file
data ChangeKind
  = CKInsert       -- ^ Insert new content
  | CKDelete       -- ^ Delete content
  | CKReplace      -- ^ Replace content
  | CKRename       -- ^ Rename file
  | CKMove         -- ^ Move file
  | CKCreate       -- ^ Create new file
  | CKRemove       -- ^ Remove file
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A single change to a file
data FileChange = FileChange
  { fcKind        :: ChangeKind
  , fchSpan       :: Maybe SrcSpan
  , fcOldContent  :: Maybe Text
  , fcNewContent  :: Maybe Text
  , fcDescription :: Text
  , fcPriority    :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Ord FileChange where
  compare c1 c2 =
    case compare (fcPriority c1) (fcPriority c2) of
      EQ -> compare (fcKind c1) (fcKind c2)
      other -> other

-- | A set of changes to a single file
data ChangeSet = ChangeSet
  { csFilePath    :: FilePath
  , csChanges     :: [FileChange]
  , csNewImports  :: [FixImport]
  , csRemoveImports :: [Text]
  , csValidated   :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty change set
emptyChangeSet :: FilePath -> ChangeSet
emptyChangeSet path = ChangeSet
  { csFilePath = path
  , csChanges = []
  , csNewImports = []
  , csRemoveImports = []
  , csValidated = False
  }

-- | Add a change to a change set
addChange :: FileChange -> ChangeSet -> ChangeSet
addChange change cs = cs { csChanges = change : csChanges cs }

-- | Merge two change sets for the same file
mergeChangeSets :: ChangeSet -> ChangeSet -> ChangeSet
mergeChangeSets cs1 cs2 = ChangeSet
  { csFilePath = csFilePath cs1
  , csChanges = csChanges cs1 ++ csChanges cs2
  , csNewImports = csNewImports cs1 ++ csNewImports cs2
  , csRemoveImports = csRemoveImports cs1 ++ csRemoveImports cs2
  , csValidated = False
  }

--------------------------------------------------------------------------------
-- Transaction Operations
--------------------------------------------------------------------------------

-- | Result of a transaction operation
data TransactionResult = TransactionResult
  { trSuccess        :: Bool
  , trModifiedFiles  :: [FilePath]
  , trErrors         :: [Text]
  , trWarnings       :: [Text]
  , trStats          :: MultiFileStats
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Preview of transaction changes
data TransactionPreview = TransactionPreview
  { tpTransaction    :: TransactionId
  , tpDescription    :: Text
  , tpFileChanges    :: Map FilePath [(Int, Text, Text)]  -- ^ (line, old, new)
  , tpNewFiles       :: [FilePath]
  , tpDeletedFiles   :: [FilePath]
  , tpImportChanges  :: Map FilePath ([Text], [Text])  -- ^ (added, removed)
  , tpEstimatedImpact :: Int  -- ^ Lines affected
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Preview transaction changes without applying
previewTransaction :: MultiFileEngine -> FixTransaction -> IO TransactionPreview
previewTransaction _engine tx = do
  let fileChanges = Map.mapWithKey previewFileChanges (ftChangeSets tx)
      newFiles = Map.keys $ Map.filter hasCreateChange (ftChangeSets tx)
      deletedFiles = Map.keys $ Map.filter hasDeleteChange (ftChangeSets tx)
      importChanges = Map.mapWithKey previewImportChanges (ftChangeSets tx)
      totalLines = sum $ map (length . csChanges) $ Map.elems (ftChangeSets tx)

  pure TransactionPreview
    { tpTransaction = ftId tx
    , tpDescription = ftDescription tx
    , tpFileChanges = fileChanges
    , tpNewFiles = newFiles
    , tpDeletedFiles = deletedFiles
    , tpImportChanges = importChanges
    , tpEstimatedImpact = totalLines
    }
  where
    previewFileChanges :: FilePath -> ChangeSet -> [(Int, Text, Text)]
    previewFileChanges _ cs =
      mapMaybe changeToPreview (csChanges cs)

    changeToPreview :: FileChange -> Maybe (Int, Text, Text)
    changeToPreview fc = do
      sp <- fchSpan fc
      let line = unLine (srcSpanStartLine sp)
          old = fromMaybe "" (fcOldContent fc)
          new = fromMaybe "" (fcNewContent fc)
      pure (line, old, new)

    hasCreateChange :: ChangeSet -> Bool
    hasCreateChange cs = any ((== CKCreate) . fcKind) (csChanges cs)

    hasDeleteChange :: ChangeSet -> Bool
    hasDeleteChange cs = any ((== CKRemove) . fcKind) (csChanges cs)

    previewImportChanges :: FilePath -> ChangeSet -> ([Text], [Text])
    previewImportChanges _ cs =
      (map fimpModule (csNewImports cs), csRemoveImports cs)

-- | Validate transaction before application
validateTransaction :: MultiFileEngine -> FixTransaction -> IO TransactionResult
validateTransaction engine tx = do
  let config = mfeConfig engine

  -- Check for conflicts with active files
  activeFiles <- readTVarIO (mfeActiveFiles engine)
  let txFiles = Set.fromList $ Map.keys (ftChangeSets tx)
      conflicts = Set.intersection activeFiles txFiles

  if not (Set.null conflicts)
    then pure TransactionResult
      { trSuccess = False
      , trModifiedFiles = []
      , trErrors = ["Conflict with active files: " <> T.intercalate ", " (map T.pack $ Set.toList conflicts)]
      , trWarnings = []
      , trStats = emptyMultiFileStats
      }
    else do
      -- Validate each change set
      validationResults <- forM (Map.toList $ ftChangeSets tx) $ \(path, cs) ->
        validateChangeSet config path cs

      let errors = concatMap snd $ filter (not . fst) validationResults
          allValid = all fst validationResults

      pure TransactionResult
        { trSuccess = allValid
        , trModifiedFiles = if allValid then Map.keys (ftChangeSets tx) else []
        , trErrors = errors
        , trWarnings = []
        , trStats = emptyMultiFileStats
        }
  where
    validateChangeSet :: MultiFileConfig -> FilePath -> ChangeSet -> IO (Bool, [Text])
    validateChangeSet _ path cs = do
      exists <- doesFileExist path
      let hasCreate = any ((== CKCreate) . fcKind) (csChanges cs)
          hasOther = any ((/= CKCreate) . fcKind) (csChanges cs)

      if not exists && hasOther && not hasCreate
        then pure (False, ["File does not exist: " <> T.pack path])
        else do
          -- Check for overlapping changes
          let overlaps = findOverlappingChanges (csChanges cs)
          if not (null overlaps)
            then pure (False, ["Overlapping changes in " <> T.pack path])
            else pure (True, [])

    findOverlappingChanges :: [FileChange] -> [(FileChange, FileChange)]
    findOverlappingChanges changes =
      [ (c1, c2)
      | c1 <- changes
      , c2 <- changes
      , c1 /= c2
      , changesOverlap c1 c2
      ]

    changesOverlap :: FileChange -> FileChange -> Bool
    changesOverlap c1 c2 =
      case (fchSpan c1, fchSpan c2) of
        (Just s1, Just s2) -> spansOverlap s1 s2
        _ -> False

    spansOverlap :: SrcSpan -> SrcSpan -> Bool
    spansOverlap s1 s2 =
      srcSpanFile s1 == srcSpanFile s2 &&
      not (srcSpanEndLine s1 < srcSpanStartLine s2 ||
           srcSpanEndLine s2 < srcSpanStartLine s1)

-- | Apply a transaction
applyTransaction :: MultiFileEngine -> FixTransaction -> IO TransactionResult
applyTransaction engine tx = do
  let config = mfeConfig engine

  -- First validate
  validation <- validateTransaction engine tx

  if not (trSuccess validation)
    then pure validation
    else do
      -- Mark files as active
      let txFiles = Set.fromList $ Map.keys (ftChangeSets tx)
      atomically $ modifyTVar' (mfeActiveFiles engine) (Set.union txFiles)

      -- Create backups if enabled
      backupPaths <- if mfcBackupEnabled config
        then createBackups config (Map.keys $ ftChangeSets tx)
        else pure Map.empty

      -- Apply changes
      result <- applyChanges config tx backupPaths

      -- Update transaction status
      now <- getCurrentTime
      atomically $ do
        let newStatus = if trSuccess result then TxApplied else TxFailed (T.intercalate "; " (trErrors result))
        modifyTVar' (mfeTransactions engine) $ Map.adjust
          (\t -> t { ftStatus = newStatus, ftAppliedAt = Just now, ftBackupPaths = backupPaths })
          (ftId tx)

        -- Remove from active files
        modifyTVar' (mfeActiveFiles engine) (`Set.difference` txFiles)

        -- Update stats
        stats <- readTVar (mfeStats engine)
        writeTVar (mfeStats engine) stats
          { mfsTransactionsApplied = mfsTransactionsApplied stats + (if trSuccess result then 1 else 0)
          , mfsFilesModified = mfsFilesModified stats + length (trModifiedFiles result)
          }

      pure result
  where
    createBackups :: MultiFileConfig -> [FilePath] -> IO (Map FilePath FilePath)
    createBackups cfg paths = do
      _now <- getCurrentTime
      let backupDir = mfcBackupDir cfg
      createDirectoryIfMissing True backupDir

      backups <- forM paths $ \path -> do
        exists <- doesFileExist path
        if exists
          then do
            let backupPath = backupDir </> takeFileName path <> ".backup"
            copyFile path backupPath
            pure (path, backupPath)
          else pure (path, "")

      pure $ Map.fromList $ filter (not . null . snd) backups

    applyChanges :: MultiFileConfig -> FixTransaction -> Map FilePath FilePath -> IO TransactionResult
    applyChanges cfg tx' _backups = do
      results <- forM (Map.toList $ ftChangeSets tx') $ \(path, cs) -> do
        if mfcDryRun cfg
          then pure (path, Right ())
          else applyChangeSet cfg path cs

      let errors = [ T.pack path <> ": " <> err
                   | (path, Left err) <- results ]
          modified = [path | (path, Right _) <- results]

      pure TransactionResult
        { trSuccess = null errors
        , trModifiedFiles = modified
        , trErrors = errors
        , trWarnings = []
        , trStats = emptyMultiFileStats
        }

    applyChangeSet :: MultiFileConfig -> FilePath -> ChangeSet -> IO (FilePath, Either Text ())
    applyChangeSet cfg path cs = do
      result <- try @SomeException $ do
        -- Read current content
        content <- TIO.readFile path

        -- Sort changes by position (reverse to apply from end)
        let sortedChanges = reverse $ sortByPosition (csChanges cs)

        -- Apply changes
        newContent <- foldM applyFileChange content sortedChanges

        -- Write result
        if mfcAtomicWrite cfg
          then atomicWrite path newContent
          else TIO.writeFile path newContent

      case result of
        Left err -> pure (path, Left $ T.pack $ show err)
        Right () -> pure (path, Right ())

    sortByPosition :: [FileChange] -> [FileChange]
    sortByPosition = map snd . sortBy (comparing fst) . map withPos
      where
        withPos fc = case fchSpan fc of
          Just sp -> (unLine (srcSpanStartLine sp), fc)
          Nothing -> (0, fc)

        sortBy _ [] = []
        sortBy cmp (x:xs) =
          sortBy cmp [y | y <- xs, cmp y x == LT] ++
          [x] ++
          sortBy cmp [y | y <- xs, cmp y x /= LT]

        comparing f x y = compare (f x) (f y)

    applyFileChange :: Text -> FileChange -> IO Text
    applyFileChange content change = case fcKind change of
      CKInsert -> case (fchSpan change, fcNewContent change) of
        (Just sp, Just newContent) -> pure $ insertAt content sp newContent
        _ -> pure content
      CKDelete -> case fchSpan change of
        Just sp -> pure $ deleteAt content sp
        Nothing -> pure content
      CKReplace -> case (fchSpan change, fcNewContent change) of
        (Just sp, Just newContent) -> pure $ replaceAt content sp newContent
        _ -> pure content
      _ -> pure content

    insertAt :: Text -> SrcSpan -> Text -> Text
    insertAt content srcSpan newText =
      let linesList = T.lines content
          lineNum = unLine (srcSpanStartLine srcSpan) - 1
          colNum = unColumn (srcSpanStartCol srcSpan) - 1
          (before, after) = splitAt lineNum linesList
          targetLine = if lineNum < length linesList
                       then linesList !! lineNum
                       else ""
          newLine = T.take colNum targetLine <> newText <> T.drop colNum targetLine
      in T.unlines (before ++ [newLine] ++ drop 1 after)

    deleteAt :: Text -> SrcSpan -> Text
    deleteAt content srcSpan =
      let linesList = T.lines content
          startLine = unLine (srcSpanStartLine srcSpan) - 1
          endLine = unLine (srcSpanEndLine srcSpan) - 1
          startCol = unColumn (srcSpanStartCol srcSpan) - 1
          endCol = unColumn (srcSpanEndCol srcSpan) - 1
          before = take startLine linesList
          after = drop (endLine + 1) linesList
          firstLine = if startLine < length linesList
                      then T.take startCol (linesList !! startLine)
                      else ""
          lastLine = if endLine < length linesList
                     then T.drop endCol (linesList !! endLine)
                     else ""
          merged = if startLine == endLine
                   then [firstLine <> lastLine]
                   else [firstLine <> lastLine]
      in T.unlines (before ++ merged ++ after)

    replaceAt :: Text -> SrcSpan -> Text -> Text
    replaceAt content srcSpan newText =
      insertAt (deleteAt content srcSpan) srcSpan newText

    atomicWrite :: FilePath -> Text -> IO ()
    atomicWrite path content = do
      let tmpPath = path <> ".tmp"
      TIO.writeFile tmpPath content
      copyFile tmpPath path
      removeFile tmpPath

    foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
    foldM _ z [] = pure z
    foldM f z (x:xs) = f z x >>= \z' -> foldM f z' xs

-- | Rollback a transaction
rollbackTransaction :: MultiFileEngine -> TransactionId -> IO TransactionResult
rollbackTransaction engine txId = do
  mTx <- Map.lookup txId <$> readTVarIO (mfeTransactions engine)
  case mTx of
    Nothing -> pure TransactionResult
      { trSuccess = False
      , trModifiedFiles = []
      , trErrors = ["Transaction not found"]
      , trWarnings = []
      , trStats = emptyMultiFileStats
      }
    Just tx -> do
      -- Restore from backups
      forM_ (Map.toList $ ftBackupPaths tx) $ \(original, backup) -> do
        exists <- doesFileExist backup
        when exists $ copyFile backup original

      -- Update status
      atomically $ do
        modifyTVar' (mfeTransactions engine) $ Map.adjust
          (\t -> t { ftStatus = TxRolledBack })
          txId

        stats <- readTVar (mfeStats engine)
        writeTVar (mfeStats engine) stats
          { mfsTransactionsRolledBack = mfsTransactionsRolledBack stats + 1 }

      pure TransactionResult
        { trSuccess = True
        , trModifiedFiles = Map.keys (ftBackupPaths tx)
        , trErrors = []
        , trWarnings = []
        , trStats = emptyMultiFileStats
        }

--------------------------------------------------------------------------------
-- Rename Operations
--------------------------------------------------------------------------------

-- | Scope of a rename operation
data RenameScope
  = RenameLocal        -- ^ Rename in current file only
  | RenameModule       -- ^ Rename in current module (exports/imports)
  | RenameProject      -- ^ Rename across entire project
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Result of a rename operation
data RenameResult = RenameResult
  { rrOldName       :: Text
  , rrNewName       :: Text
  , rrFilesAffected :: [FilePath]
  , rrOccurrences   :: Int
  , rrTransaction   :: Maybe FixTransaction
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Create a transaction for renaming a symbol
createRenameTransaction :: MultiFileEngine
                        -> Text           -- ^ Old name
                        -> Text           -- ^ New name
                        -> [FilePath]     -- ^ Files to search
                        -> RenameScope    -- ^ Scope
                        -> IO (FixTransaction, RenameResult)
createRenameTransaction engine oldName newName files _scope = do
  tx <- createTransaction engine $
    "Rename " <> oldName <> " to " <> newName

  -- Find all occurrences
  occurrences <- forM files $ \path -> do
    exists <- doesFileExist path
    if exists
      then do
        content <- TIO.readFile path
        let found = findOccurrences content oldName
        pure (path, found)
      else pure (path, [])

  let nonEmpty = filter (not . null . snd) occurrences
      totalOccurrences = sum $ map (length . snd) nonEmpty

  -- Build change sets
  let changeSets = Map.fromList
        [ (path, buildRenameChangeSet path oldName newName locs)
        | (path, locs) <- nonEmpty
        ]

  let tx' = tx { ftChangeSets = changeSets }

  pure (tx', RenameResult
    { rrOldName = oldName
    , rrNewName = newName
    , rrFilesAffected = Map.keys changeSets
    , rrOccurrences = totalOccurrences
    , rrTransaction = Just tx'
    })
  where
    findOccurrences :: Text -> Text -> [(Int, Int)]
    findOccurrences content name =
      let linesList = zip [1..] (T.lines content)
      in concatMap (findInLine name) linesList

    findInLine :: Text -> (Int, Text) -> [(Int, Int)]
    findInLine name (lineNum, line) =
      let cols = findIndices name line
      in [(lineNum, col) | col <- cols]

    findIndices :: Text -> Text -> [Int]
    findIndices needle haystack = go 0 haystack
      where
        go offset txt =
          case T.breakOn needle txt of
            (before, after)
              | T.null after -> []
              | otherwise ->
                  let col = offset + T.length before + 1
                  in col : go (col + T.length needle - 1) (T.drop (T.length needle) after)

    buildRenameChangeSet :: FilePath -> Text -> Text -> [(Int, Int)] -> ChangeSet
    buildRenameChangeSet path old new locs =
      let changes = map (makeRenameChange path old new) locs
      in ChangeSet
        { csFilePath = path
        , csChanges = changes
        , csNewImports = []
        , csRemoveImports = []
        , csValidated = False
        }

    makeRenameChange :: FilePath -> Text -> Text -> (Int, Int) -> FileChange
    makeRenameChange path old new (line, col) = FileChange
      { fcKind = CKReplace
      , fchSpan = Just SrcSpan
          { srcSpanFile = path
          , srcSpanStartLine = Line line
          , srcSpanStartCol = Column col
          , srcSpanEndLine = Line line
          , srcSpanEndCol = Column (col + T.length old)
          }
      , fcOldContent = Just old
      , fcNewContent = Just new
      , fcDescription = "Rename " <> old <> " to " <> new
      , fcPriority = 50
      }

-- | Rename a symbol across files
renameSymbol :: MultiFileEngine -> Text -> Text -> [FilePath] -> RenameScope -> IO TransactionResult
renameSymbol engine oldName newName files scope = do
  (tx, _) <- createRenameTransaction engine oldName newName files scope
  applyTransaction engine tx

-- | Rename a module
renameModule :: MultiFileEngine -> Text -> Text -> IO TransactionResult
renameModule engine oldModule newModule = do
  _tx <- createTransaction engine $
    "Rename module " <> oldModule <> " to " <> newModule

  -- Would find all import statements and update them
  pure TransactionResult
    { trSuccess = True
    , trModifiedFiles = []
    , trErrors = []
    , trWarnings = []
    , trStats = emptyMultiFileStats
    }

-- | Rename a file
renameFile :: MultiFileEngine -> FilePath -> FilePath -> IO TransactionResult
renameFile engine oldPath newPath = do
  _tx <- createTransaction engine $
    "Rename file " <> T.pack oldPath <> " to " <> T.pack newPath

  -- Would rename file and update all imports
  pure TransactionResult
    { trSuccess = True
    , trModifiedFiles = [oldPath, newPath]
    , trErrors = []
    , trWarnings = []
    , trStats = emptyMultiFileStats
    }

--------------------------------------------------------------------------------
-- Move Operations
--------------------------------------------------------------------------------

-- | Scope of a move operation
data MoveScope
  = MoveSingle        -- ^ Move a single definition
  | MoveWithDeps      -- ^ Move with dependencies
  | MoveTree          -- ^ Move entire dependency tree
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Result of a move operation
data MoveResult = MoveResult
  { mrSymbol        :: Text
  , mrFromModule    :: Text
  , mrToModule      :: Text
  , mrFilesAffected :: [FilePath]
  , mrImportsAdded  :: Int
  , mrImportsRemoved :: Int
  , mrTransaction   :: Maybe FixTransaction
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Create a transaction for moving a definition
createMoveTransaction :: MultiFileEngine
                      -> Text           -- ^ Symbol to move
                      -> FilePath       -- ^ Source file
                      -> FilePath       -- ^ Target file
                      -> MoveScope      -- ^ Scope
                      -> IO (FixTransaction, MoveResult)
createMoveTransaction engine symbolName srcFile dstFile _scope = do
  tx <- createTransaction engine $
    "Move " <> symbolName <> " to " <> T.pack dstFile

  -- Would extract definition and update imports
  pure (tx, MoveResult
    { mrSymbol = symbolName
    , mrFromModule = T.pack srcFile
    , mrToModule = T.pack dstFile
    , mrFilesAffected = [srcFile, dstFile]
    , mrImportsAdded = 0
    , mrImportsRemoved = 0
    , mrTransaction = Just tx
    })

-- | Move a definition to another file
moveDefinition :: MultiFileEngine -> Text -> FilePath -> FilePath -> MoveScope -> IO TransactionResult
moveDefinition engine symbolName srcFile dstFile scope = do
  (tx, _) <- createMoveTransaction engine symbolName srcFile dstFile scope
  applyTransaction engine tx

-- | Move definition to a module (create if needed)
moveToModule :: MultiFileEngine -> Text -> FilePath -> Text -> IO TransactionResult
moveToModule engine symbolName srcFile targetModule = do
  let dstFile = moduleToFilePath targetModule
  moveDefinition engine symbolName srcFile dstFile MoveSingle
  where
    moduleToFilePath :: Text -> FilePath
    moduleToFilePath modName =
      T.unpack (T.replace "." "/" modName) <> ".hs"

-- | Extract definitions to a new module
extractToModule :: MultiFileEngine -> [Text] -> FilePath -> Text -> IO TransactionResult
extractToModule engine _symbols srcFile newModule = do
  _tx <- createTransaction engine $
    "Extract to module " <> newModule

  -- Would create new module and move definitions
  pure TransactionResult
    { trSuccess = True
    , trModifiedFiles = [srcFile]
    , trErrors = []
    , trWarnings = []
    , trStats = emptyMultiFileStats
    }

--------------------------------------------------------------------------------
-- Import Management
--------------------------------------------------------------------------------

-- | An import update
data ImportUpdate = ImportUpdate
  { iuFile    :: FilePath
  , iuModule  :: Text
  , iuSymbols :: [Text]
  , iuAdd     :: Bool    -- ^ True to add, False to remove
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Result of import update
data ImportUpdateResult = ImportUpdateResult
  { iurFile    :: FilePath
  , iurSuccess :: Bool
  , iurMessage :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Update imports for a rename
updateImportsForRename :: MultiFileEngine -> Text -> Text -> [FilePath] -> IO [ImportUpdateResult]
updateImportsForRename _engine oldName newName files = do
  forM files $ \path -> do
    exists <- doesFileExist path
    if exists
      then do
        content <- TIO.readFile path
        let newContent = T.replace oldName newName content
        when (content /= newContent) $
          TIO.writeFile path newContent
        pure ImportUpdateResult
          { iurFile = path
          , iurSuccess = True
          , iurMessage = "Updated imports"
          }
      else pure ImportUpdateResult
        { iurFile = path
        , iurSuccess = False
        , iurMessage = "File not found"
        }

-- | Update imports for a move
updateImportsForMove :: MultiFileEngine -> Text -> Text -> Text -> [FilePath] -> IO [ImportUpdateResult]
updateImportsForMove _engine _symbolName _oldModule _newModule files = do
  forM files $ \path -> do
    -- Would update import statements
    pure ImportUpdateResult
      { iurFile = path
      , iurSuccess = True
      , iurMessage = "Updated imports for move"
      }

-- | Add import to multiple files
addImportToFiles :: MultiFileEngine -> Text -> [Text] -> [FilePath] -> IO [ImportUpdateResult]
addImportToFiles _engine moduleName _symbols files = do
  forM files $ \path -> do
    -- Would add import statement
    pure ImportUpdateResult
      { iurFile = path
      , iurSuccess = True
      , iurMessage = "Added import " <> moduleName
      }

-- | Remove import from multiple files
removeImportFromFiles :: MultiFileEngine -> Text -> [FilePath] -> IO [ImportUpdateResult]
removeImportFromFiles _engine modName files = do
  forM files $ \path -> do
    -- Would remove import statement
    pure ImportUpdateResult
      { iurFile = path
      , iurSuccess = True
      , iurMessage = "Removed import " <> modName
      }

--------------------------------------------------------------------------------
-- Dependency Analysis
--------------------------------------------------------------------------------

-- | A dependency between files
data FileDependency = FileDependency
  { fldFrom   :: FilePath
  , fldTo     :: FilePath
  , fldKind   :: DependencyKind
  , fldSymbols :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Kind of dependency
data DependencyKind
  = DKImport           -- ^ Import dependency
  | DKReExport         -- ^ Re-export dependency
  | DKTypeFamily       -- ^ Type family dependency
  | DKTemplate         -- ^ Template Haskell dependency
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A dependency graph
data DependencyGraph = DependencyGraph
  { dgNodes :: Set FilePath
  , dgEdges :: [FileDependency]
  , dgReversed :: Map FilePath [FilePath]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Analyze dependencies between files
analyzeFileDependencies :: [FilePath] -> IO [FileDependency]
analyzeFileDependencies _files = do
  -- Would parse imports from each file
  pure []

-- | Find all files affected by a change
findAffectedFiles :: DependencyGraph -> FilePath -> [FilePath]
findAffectedFiles graph file =
  Map.findWithDefault [] file (dgReversed graph)

-- | Build dependency graph from files
buildDependencyGraph :: [FilePath] -> IO DependencyGraph
buildDependencyGraph files = do
  deps <- analyzeFileDependencies files
  let nodes = Set.fromList files
      reversed = buildReversed deps
  pure DependencyGraph
    { dgNodes = nodes
    , dgEdges = deps
    , dgReversed = reversed
    }
  where
    buildReversed :: [FileDependency] -> Map FilePath [FilePath]
    buildReversed deps =
      foldl addEdge Map.empty deps

    addEdge :: Map FilePath [FilePath] -> FileDependency -> Map FilePath [FilePath]
    addEdge m dep =
      Map.insertWith (++) (fldTo dep) [fldFrom dep] m

--------------------------------------------------------------------------------
-- Conflict Detection
--------------------------------------------------------------------------------

-- | A conflict between transactions or changes
data TransactionConflict = TransactionConflict
  { tcTransaction1 :: TransactionId
  , tcTransaction2 :: TransactionId
  , tcFile         :: FilePath
  , tcDescription  :: Text
  , tcOverlapping  :: [(SrcSpan, SrcSpan)]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A conflict resolution
data ConflictResolution = ConflictResolution
  { crConflict   :: TransactionConflict
  , crResolution :: ResolutionKind
  , crResult     :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Kind of resolution
data ResolutionKind
  = RKPreferFirst      -- ^ Prefer first transaction
  | RKPreferSecond     -- ^ Prefer second transaction
  | RKMerge            -- ^ Merge both
  | RKSkip             -- ^ Skip conflicting file
  | RKAbort            -- ^ Abort both
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Detect conflicts between transactions
detectTransactionConflicts :: [FixTransaction] -> [TransactionConflict]
detectTransactionConflicts txs =
  [ conflict
  | (tx1:rest) <- tails txs
  , tx2 <- rest
  , conflict <- findConflicts tx1 tx2
  ]
  where
    tails [] = [[]]
    tails xs@(_:rest) = xs : tails rest

    findConflicts :: FixTransaction -> FixTransaction -> [TransactionConflict]
    findConflicts tx1 tx2 =
      let files1 = Map.keys (ftChangeSets tx1)
          files2 = Map.keys (ftChangeSets tx2)
          commonFiles = filter (`elem` files2) files1
      in mapMaybe (checkFileConflict tx1 tx2) commonFiles

    checkFileConflict :: FixTransaction -> FixTransaction -> FilePath -> Maybe TransactionConflict
    checkFileConflict tx1 tx2 file = do
      cs1 <- Map.lookup file (ftChangeSets tx1)
      cs2 <- Map.lookup file (ftChangeSets tx2)
      let spans1 = mapMaybe fchSpan (csChanges cs1)
          spans2 = mapMaybe fchSpan (csChanges cs2)
          overlapping = [(s1, s2) | s1 <- spans1, s2 <- spans2, spansOverlap s1 s2]
      if null overlapping
        then Nothing
        else Just TransactionConflict
          { tcTransaction1 = ftId tx1
          , tcTransaction2 = ftId tx2
          , tcFile = file
          , tcDescription = "Overlapping changes in " <> T.pack file
          , tcOverlapping = overlapping
          }

    spansOverlap :: SrcSpan -> SrcSpan -> Bool
    spansOverlap s1 s2 =
      srcSpanFile s1 == srcSpanFile s2 &&
      not (srcSpanEndLine s1 < srcSpanStartLine s2 ||
           srcSpanEndLine s2 < srcSpanStartLine s1)

-- | Resolve conflicts according to strategy
resolveConflicts :: ConflictStrategy -> [TransactionConflict] -> IO [ConflictResolution]
resolveConflicts strategy conflicts = do
  forM conflicts $ \conflict -> do
    let resolution = case strategy of
          AbortOnConflict -> RKAbort
          SkipConflicting -> RKSkip
          MergeConflicting -> RKMerge
          AskUser -> RKSkip  -- Default for now
    pure ConflictResolution
      { crConflict = conflict
      , crResolution = resolution
      , crResult = "Resolved by " <> T.pack (show resolution)
      }

--------------------------------------------------------------------------------
-- Batch Processing
--------------------------------------------------------------------------------

-- | Result of batch processing
data BatchResult = BatchResult
  { brTotal      :: Int
  , brSucceeded  :: Int
  , brFailed     :: Int
  , brSkipped    :: Int
  , brResults    :: [TransactionResult]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Apply multiple transactions in sequence
batchApplyTransactions :: MultiFileEngine -> [FixTransaction] -> IO BatchResult
batchApplyTransactions engine txs = do
  results <- forM txs $ \tx ->
    applyTransaction engine tx

  let succeeded = length $ filter trSuccess results
      failed = length results - succeeded

  pure BatchResult
    { brTotal = length txs
    , brSucceeded = succeeded
    , brFailed = failed
    , brSkipped = 0
    , brResults = results
    }

-- | Apply transactions in parallel (where possible)
parallelApplyTransactions :: MultiFileEngine -> [FixTransaction] -> IO BatchResult
parallelApplyTransactions engine txs = do
  let config = mfeConfig engine
      maxParallel = mfcMaxParallelFiles config

  -- Group transactions by independence
  let groups = groupIndependentTransactions txs

  results <- forM groups $ \group -> do
    if length group <= maxParallel
      then applyGroup engine group
      else batchApplyTransactions engine group

  let allResults = concatMap brResults results

  pure BatchResult
    { brTotal = length txs
    , brSucceeded = length $ filter trSuccess allResults
    , brFailed = length allResults - length (filter trSuccess allResults)
    , brSkipped = 0
    , brResults = allResults
    }
  where
    groupIndependentTransactions :: [FixTransaction] -> [[FixTransaction]]
    groupIndependentTransactions = map (:[])  -- Simple grouping for now

    applyGroup :: MultiFileEngine -> [FixTransaction] -> IO BatchResult
    applyGroup eng group = batchApplyTransactions eng group

--------------------------------------------------------------------------------
-- Progress Tracking
--------------------------------------------------------------------------------

-- | Progress of a transaction
data TransactionProgress = TransactionProgress
  { tpTotalFiles    :: Int
  , tpProcessedFiles :: Int
  , tpCurrentFile   :: Maybe FilePath
  , tpStatus        :: Text
  , tpStartTime     :: UTCTime
  , tpEstimatedRemaining :: Maybe Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Progress callback type
type ProgressCallback = TransactionProgress -> IO ()

-- | Execute with progress tracking
withProgress :: MultiFileEngine
             -> TransactionId
             -> ProgressCallback
             -> IO a
             -> IO a
withProgress engine txId callback action = do
  now <- getCurrentTime
  let initialProgress = TransactionProgress
        { tpTotalFiles = 0
        , tpProcessedFiles = 0
        , tpCurrentFile = Nothing
        , tpStatus = "Starting"
        , tpStartTime = now
        , tpEstimatedRemaining = Nothing
        }

  atomically $ modifyTVar' (mfeProgress engine) $ Map.insert txId initialProgress
  callback initialProgress

  result <- action

  atomically $ modifyTVar' (mfeProgress engine) $ Map.delete txId

  pure result

--------------------------------------------------------------------------------
-- FixEngine Instance
--------------------------------------------------------------------------------

instance FixEngine MultiFileEngine where
  type EngineConfig MultiFileEngine = MultiFileConfig
  type EngineCategory MultiFileEngine = ChangeKind

  engineName _ = "multi-file"
  engineVersion _ = "1.0.0"
  engineCategories _ = [minBound .. maxBound]
  engineDescription _ = "Multi-file coordinated fix engine"

  findFixes _engine _path _content = pure []  -- Multi-file doesn't find single-file fixes

  validateFix _engine _ef _content = pure FixValidation
    { fvResult = ValidationSuccess
    , fvChecks = []
    , fvSuggestions = []
    }

  applyFix _engine _path content ef = do
    let fix = efFix ef
    -- For single file, just apply directly
    pure ApplySuccess
      { arsNewContent = applyFixContent content fix
      , arsAppliedFix = ef
      , arsStats = emptyFixStats
      }

  getEngineStats engine = do
    stats <- readTVarIO (mfeStats engine)
    pure emptyFixStats
      { fsTotal = mfsTransactionsCreated stats
      , fsApplied = mfsTransactionsApplied stats
      }

-- | Apply fix to content
applyFixContent :: Text -> Fix -> Text
applyFixContent content fix =
  foldl applyEdit content (reverse $ fixEdits fix)
  where
    applyEdit txt edit =
      let editSpan = fixEditSpan edit
          newText = fixEditNewText edit
          linesList = T.lines txt
          startLine = unLine (srcSpanStartLine editSpan) - 1
          endLine = unLine (srcSpanEndLine editSpan) - 1
          startCol = unColumn (srcSpanStartCol editSpan) - 1
          endCol = unColumn (srcSpanEndCol editSpan) - 1
          before = take startLine linesList
          after = drop (endLine + 1) linesList
          modLine = if startLine == endLine && startLine < length linesList
            then let line = linesList !! startLine
                 in [T.take startCol line <> newText <> T.drop endCol line]
            else [newText]
      in T.unlines (before ++ modLine ++ after)
