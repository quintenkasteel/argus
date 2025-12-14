{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Argus.Refactor.SequentialFixer
-- Description : Sequential fix application with span adjustment
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides a robust system for applying multiple fixes
-- to the same file sequentially. After each fix is applied:
--
-- 1. Compute span deltas to track how positions shifted
-- 2. Adjust remaining fixes' spans to account for the changes
-- 3. Re-validate that adjusted fixes are still applicable
-- 4. Optionally run compilation check
--
-- == Why Sequential Application?
--
-- The naive approach of applying all fixes at once has issues:
--
-- * No validation between fixes (one bad fix breaks everything)
-- * No ability to skip/rollback individual fixes
-- * No re-detection of new issues after each fix
--
-- Sequential application with span adjustment solves these problems.
--
-- == Usage
--
-- @
-- result <- applyFixesSequentially config content fixes
-- case seqResult result of
--   SeqSuccess applied -> -- All fixes applied
--   SeqPartial applied failed -> -- Some fixes failed
--   SeqFailed err -> -- Complete failure
-- @
module Argus.Refactor.SequentialFixer
  ( -- * Sequential Application
    applyFixesSequentially
  , applyFixesWithRedetection
  , SequentialConfig (..)
  , defaultSequentialConfig
  , SequentialResult (..)
  , SequentialOutcome (..)
  , AppliedFixInfo (..)
  , FailedFixInfo (..)

    -- * Re-detection
  , RedetectionConfig (..)
  , redetectAfterFix
  , shouldRedetect

    -- * Compilation Validation
  , validateCompilation
  , CompilationResult (..)
  , CompilationError (..)
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Exit (ExitCode(..))
import System.IO.Temp (writeSystemTempFile)
import System.Process (readProcessWithExitCode)
import System.Directory (removeFile)
import System.FilePath (takeDirectory, takeFileName)

import Argus.Types
import Argus.Refactor.ExactPrint (applyFix)
import Argus.Refactor.SpanAdjustment
import Argus.Refactor.Validation
  ( validateSyntax
  , ValidationError(..)
  , ValidationSeverity(..)
  )
import Argus.Analysis.Syntactic (parseModule)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for sequential fix application.
data SequentialConfig = SequentialConfig
  { scValidateAfterEach   :: Bool    -- ^ Validate syntax after each fix
  , scStopOnFirstFailure  :: Bool    -- ^ Stop if a fix fails
  , scAdjustSpans         :: Bool    -- ^ Adjust remaining spans after each fix
  , scValidateCompilation :: Bool    -- ^ Run GHC compilation check at end
  , scMaxFixes            :: Int     -- ^ Maximum fixes to apply (0 = no limit)
  , scVerbose             :: Bool    -- ^ Verbose output
  , scRedetectIssues      :: Bool    -- ^ Re-run linter after each fix
  }
  deriving stock (Eq, Show)

-- | Default configuration.
defaultSequentialConfig :: SequentialConfig
defaultSequentialConfig = SequentialConfig
  { scValidateAfterEach = True
  , scStopOnFirstFailure = False
  , scAdjustSpans = True
  , scValidateCompilation = False
  , scMaxFixes = 0
  , scVerbose = False
  , scRedetectIssues = False
  }

-- | Configuration for re-detection after fixes.
data RedetectionConfig = RedetectionConfig
  { rcEnabled        :: Bool           -- ^ Enable re-detection
  , rcLintFunction   :: FilePath -> Text -> IO [Diagnostic]
    -- ^ Function to lint code and return diagnostics
  , rcMergeStrategy  :: MergeStrategy  -- ^ How to handle newly detected issues
  }

-- | Strategy for merging newly detected issues.
data MergeStrategy
  = IgnoreNew        -- ^ Ignore newly detected issues
  | AddNew           -- ^ Add new issues to the queue
  | ReplaceAll       -- ^ Replace queue with new issues
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Result Types
--------------------------------------------------------------------------------

-- | Result of sequential fix application.
data SequentialResult = SequentialResult
  { seqOutcome      :: SequentialOutcome
  , seqApplied      :: [AppliedFixInfo]   -- ^ Successfully applied fixes
  , seqFailed       :: [FailedFixInfo]    -- ^ Failed fixes
  , seqSkipped      :: [Fix]              -- ^ Skipped fixes (due to span invalidation)
  , seqFinalContent :: Text               -- ^ Final content after all fixes
  , seqDeltas       :: [SpanDelta]        -- ^ All accumulated deltas
  , seqCompilation  :: Maybe CompilationResult -- ^ Compilation check result
  }
  deriving stock (Eq, Show)

-- | Outcome of sequential application.
data SequentialOutcome
  = SeqSuccess        -- ^ All fixes applied successfully
  | SeqPartial        -- ^ Some fixes applied, some failed
  | SeqFailed Text    -- ^ Complete failure with error
  deriving stock (Eq, Show)

-- | Information about a successfully applied fix.
data AppliedFixInfo = AppliedFixInfo
  { afiOriginalFix   :: Fix          -- ^ The original fix
  , afiAdjustedFix   :: Fix          -- ^ The fix after span adjustment
  , afiContentBefore :: Text         -- ^ Content before this fix
  , afiContentAfter  :: Text         -- ^ Content after this fix
  , afiDelta         :: SpanDelta    -- ^ Delta produced by this fix
  }
  deriving stock (Eq, Show)

-- | Information about a failed fix.
data FailedFixInfo = FailedFixInfo
  { ffiFix      :: Fix           -- ^ The fix that failed
  , ffiReason   :: FailureReason -- ^ Why it failed
  , ffiErrors   :: [Text]        -- ^ Error messages
  }
  deriving stock (Eq, Show)

-- | Reason a fix failed.
data FailureReason
  = SpanInvalidated    -- ^ Span became invalid after previous fix
  | ValidationFailed   -- ^ Syntax validation failed after applying
  | ParseError         -- ^ Could not parse result
  | InternalError      -- ^ Unexpected error
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Sequential Application
--------------------------------------------------------------------------------

-- | Apply fixes sequentially with span adjustment.
applyFixesSequentially :: SequentialConfig
                       -> FilePath
                       -> Text
                       -> [Fix]
                       -> IO SequentialResult
applyFixesSequentially config path content fixes = do
  -- Limit fixes if configured
  let limitedFixes = case scMaxFixes config of
        0 -> fixes
        n -> take n fixes

  -- State: current content, accumulated deltas, results
  contentRef <- newIORef content
  deltasRef <- newIORef ([] :: [SpanDelta])
  appliedRef <- newIORef ([] :: [AppliedFixInfo])
  failedRef <- newIORef ([] :: [FailedFixInfo])
  skippedRef <- newIORef ([] :: [Fix])

  -- Process each fix
  let go [] = pure ()
      go (fix:rest) = do
        currentContent <- readIORef contentRef
        currentDeltas <- readIORef deltasRef

        -- Adjust the fix's spans based on accumulated deltas
        let maybeAdjusted = if scAdjustSpans config
                            then adjustFix currentDeltas fix
                            else Just fix

        case maybeAdjusted of
          Nothing -> do
            -- Span was invalidated by previous fixes
            modifyIORef' skippedRef (fix :)
            when (scVerbose config) $
              TIO.putStrLn $ "[SKIP] Fix '" <> fixTitle fix <> "' - span invalidated"
            go rest

          Just adjustedFix -> do
            -- Apply the fix
            let newContent = applyFix currentContent adjustedFix

            -- Validate if configured
            validationResult <- if scValidateAfterEach config
                                then validateSyntax path newContent
                                else pure $ Right ()

            case validationResult of
              Left errors -> do
                -- Validation failed
                let failInfo = FailedFixInfo
                      { ffiFix = fix
                      , ffiReason = ValidationFailed
                      , ffiErrors = map veMessage errors
                      }
                modifyIORef' failedRef (failInfo :)
                when (scVerbose config) $
                  TIO.putStrLn $ "[FAIL] Fix '" <> fixTitle fix <> "' - validation failed"

                if scStopOnFirstFailure config
                  then pure ()
                  else go rest

              Right _ -> do
                -- Compute delta from this fix
                let delta = case fixEdits adjustedFix of
                      [] -> SpanDelta path (1, 1) (1, 1) 0 0 1 1
                      (edit:_) ->
                        let span = fixEditSpan edit
                            oldText = extractSpanText currentContent span
                            newText = fixEditNewText edit
                        in computeDelta span oldText newText

                -- Record success
                let appliedInfo = AppliedFixInfo
                      { afiOriginalFix = fix
                      , afiAdjustedFix = adjustedFix
                      , afiContentBefore = currentContent
                      , afiContentAfter = newContent
                      , afiDelta = delta
                      }

                modifyIORef' appliedRef (appliedInfo :)
                modifyIORef' deltasRef (++ [delta])
                writeIORef contentRef newContent

                when (scVerbose config) $
                  TIO.putStrLn $ "[OK] Applied fix '" <> fixTitle fix <> "'"

                go rest

  -- Run the processing
  go limitedFixes

  -- Get final state
  finalContent <- readIORef contentRef
  appliedList <- reverse <$> readIORef appliedRef
  failedList <- reverse <$> readIORef failedRef
  skippedList <- reverse <$> readIORef skippedRef
  deltasList <- readIORef deltasRef

  -- Run compilation check if configured
  compilationResult <- if scValidateCompilation config && not (null appliedList)
                       then Just <$> validateCompilation path finalContent
                       else pure Nothing

  -- Determine outcome
  let outcome
        | null failedList && null skippedList = SeqSuccess
        | not (null appliedList) = SeqPartial
        | otherwise = SeqFailed "No fixes could be applied"

  pure SequentialResult
    { seqOutcome = outcome
    , seqApplied = appliedList
    , seqFailed = failedList
    , seqSkipped = skippedList
    , seqFinalContent = finalContent
    , seqDeltas = deltasList
    , seqCompilation = compilationResult
    }

-- | Apply fixes with re-detection after each fix.
-- This re-runs the linter after each fix to detect new issues
-- or verify that target issues are actually fixed.
applyFixesWithRedetection :: SequentialConfig
                          -> RedetectionConfig
                          -> FilePath
                          -> Text
                          -> [Fix]
                          -> IO SequentialResult
applyFixesWithRedetection config redetectConfig path content initialFixes = do
  fixQueueRef <- newIORef initialFixes
  allAppliedRef <- newIORef ([] :: [AppliedFixInfo])
  allFailedRef <- newIORef ([] :: [FailedFixInfo])
  allSkippedRef <- newIORef ([] :: [Fix])
  contentRef <- newIORef content
  deltasRef <- newIORef ([] :: [SpanDelta])

  let processNext = do
        queue <- readIORef fixQueueRef
        case queue of
          [] -> pure ()  -- Done
          (fix:rest) -> do
            writeIORef fixQueueRef rest
            currentContent <- readIORef contentRef
            currentDeltas <- readIORef deltasRef

            -- Apply single fix
            result <- applyFixesSequentially
              config { scMaxFixes = 1, scRedetectIssues = False }
              path
              currentContent
              [fix]

            case seqOutcome result of
              SeqSuccess -> do
                modifyIORef' allAppliedRef (++ seqApplied result)
                modifyIORef' deltasRef (++ seqDeltas result)
                writeIORef contentRef (seqFinalContent result)

                -- Re-detect if configured
                when (rcEnabled redetectConfig) $ do
                  newContent <- readIORef contentRef
                  newDiags <- rcLintFunction redetectConfig path newContent
                  let newFixes = concatMap diagFixes newDiags

                  case rcMergeStrategy redetectConfig of
                    IgnoreNew -> pure ()
                    AddNew -> modifyIORef' fixQueueRef (++ newFixes)
                    ReplaceAll -> writeIORef fixQueueRef newFixes

              SeqPartial -> do
                modifyIORef' allAppliedRef (++ seqApplied result)
                modifyIORef' allFailedRef (++ seqFailed result)
                modifyIORef' deltasRef (++ seqDeltas result)
                writeIORef contentRef (seqFinalContent result)

              SeqFailed _ -> do
                modifyIORef' allFailedRef (++ seqFailed result)
                modifyIORef' allSkippedRef (++ seqSkipped result)

            -- Continue processing
            processNext

  processNext

  -- Get final results
  finalContent <- readIORef contentRef
  appliedList <- readIORef allAppliedRef
  failedList <- readIORef allFailedRef
  skippedList <- readIORef allSkippedRef
  deltasList <- readIORef deltasRef

  let outcome
        | null failedList && null skippedList = SeqSuccess
        | not (null appliedList) = SeqPartial
        | otherwise = SeqFailed "No fixes could be applied"

  pure SequentialResult
    { seqOutcome = outcome
    , seqApplied = appliedList
    , seqFailed = failedList
    , seqSkipped = skippedList
    , seqFinalContent = finalContent
    , seqDeltas = deltasList
    , seqCompilation = Nothing
    }

--------------------------------------------------------------------------------
-- Re-detection
--------------------------------------------------------------------------------

-- | Re-detect issues after a fix is applied.
redetectAfterFix :: RedetectionConfig
                 -> FilePath
                 -> Text
                 -> IO [Diagnostic]
redetectAfterFix config path content =
  if rcEnabled config
  then rcLintFunction config path content
  else pure []

-- | Check if re-detection should run.
shouldRedetect :: RedetectionConfig -> Bool
shouldRedetect = rcEnabled

--------------------------------------------------------------------------------
-- Compilation Validation
--------------------------------------------------------------------------------

-- | Result of compilation check.
data CompilationResult
  = CompileSuccess
  | CompileFailure [CompilationError]
  deriving stock (Eq, Show)

-- | A compilation error.
data CompilationError = CompilationError
  { ceFile    :: FilePath
  , ceLine    :: Maybe Int
  , ceColumn  :: Maybe Int
  , ceMessage :: Text
  }
  deriving stock (Eq, Show)

-- | Validate that code compiles using GHC.
validateCompilation :: FilePath -> Text -> IO CompilationResult
validateCompilation originalPath source = do
  -- Write to temp file
  let dir = takeDirectory originalPath
      baseName = takeFileName originalPath
  tempPath <- writeSystemTempFile baseName (T.unpack source)

  -- Run GHC
  result <- try @SomeException $ readProcessWithExitCode "ghc"
    [ "-fno-code"
    , "-fdefer-type-errors"
    , "-w"
    , "-ignore-dot-ghci"
    , "-no-link"
    , "-i" <> dir
    , tempPath
    ] ""

  -- Clean up
  _ <- try @SomeException $ removeFile tempPath

  case result of
    Left ex -> pure $ CompileFailure
      [CompilationError originalPath Nothing Nothing (T.pack $ show ex)]

    Right (ExitSuccess, _, _) -> pure CompileSuccess

    Right (ExitFailure _, _, stderr) ->
      pure $ CompileFailure $ parseGhcErrors originalPath stderr

-- | Parse GHC error output.
parseGhcErrors :: FilePath -> String -> [CompilationError]
parseGhcErrors defaultFile output =
  let errorLines = filter (not . null) $ lines output
  in if null errorLines
     then [CompilationError defaultFile Nothing Nothing "Compilation failed"]
     else map (parseGhcError defaultFile) errorLines

parseGhcError :: FilePath -> String -> CompilationError
parseGhcError defaultFile line =
  case break (== ':') line of
    (file, ':':rest) ->
      case break (== ':') rest of
        (lineStr, ':':rest2) ->
          case break (== ':') rest2 of
            (colStr, ':':msg) ->
              CompilationError
                { ceFile = if null file then defaultFile else file
                , ceLine = readMaybe lineStr
                , ceColumn = readMaybe colStr
                , ceMessage = T.strip $ T.pack msg
                }
            _ -> simpleError line
        _ -> simpleError line
    _ -> simpleError line
  where
    simpleError msg = CompilationError defaultFile Nothing Nothing (T.pack msg)
    readMaybe s = case reads s of
      [(n, "")] -> Just n
      _ -> Nothing

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Extract text from a span (copied from SpanAdjustment for local use).
extractSpanText :: Text -> SrcSpan -> Text
extractSpanText content span =
  let contentLines = T.lines content
      startLine = srcSpanStartLineRaw span
      endLine = srcSpanEndLineRaw span
      startCol = srcSpanStartColRaw span
      endCol = srcSpanEndColRaw span

  in if startLine == endLine
     then
       if startLine > 0 && startLine <= length contentLines
       then let line = contentLines !! (startLine - 1)
            in T.take (endCol - startCol) $ T.drop (startCol - 1) line
       else ""
     else
       let relevantLines = take (endLine - startLine + 1) $ drop (startLine - 1) contentLines
       in case relevantLines of
            [] -> ""
            [l] -> T.drop (startCol - 1) l
            (first:rest) ->
              let firstPart = T.drop (startCol - 1) first
                  lastPart = case reverse rest of
                               [] -> ""
                               (l:_) -> T.take (endCol - 1) l
                  middleParts = case rest of
                                  [] -> []
                                  [_] -> []
                                  xs -> init xs
              in T.intercalate "\n" ([firstPart] ++ middleParts ++ [lastPart])
