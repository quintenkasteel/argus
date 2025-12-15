{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Rules.Concurrency
-- Description : Detect concurrency anti-patterns in Haskell code
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module detects common concurrency issues including:
--
-- * STM anti-patterns (retry without orElse, unbounded retries)
-- * Async exception handling issues
-- * Race condition patterns
-- * Deadlock-prone patterns
-- * Unsafe concurrent variable usage
module Argus.Rules.Concurrency
  ( -- * Detection
    detectConcurrencyIssues
  , ConcurrencyFinding (..)
  , ConcurrencyCategory (..)

    -- * Configuration
  , ConcurrencyConfig (..)
  , defaultConcurrencyConfig
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Argus.Types

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Category of concurrency issue
data ConcurrencyCategory
  = STMAntiPattern       -- ^ STM usage anti-patterns
  | AsyncException       -- ^ Async exception handling issues
  | RaceCondition        -- ^ Potential race conditions
  | DeadlockRisk         -- ^ Deadlock-prone patterns
  | UnsafeConcurrent     -- ^ Unsafe concurrent operations
  | ResourceLeak         -- ^ Resource leak in concurrent code
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A detected concurrency issue
data ConcurrencyFinding = ConcurrencyFinding
  { cfCategory    :: ConcurrencyCategory
  , cfSpan        :: SrcSpan
  , cfCode        :: Text           -- ^ Original code
  , cfExplanation :: Text           -- ^ Why it's problematic
  , cfSuggestion  :: Maybe Text     -- ^ Suggested fix
  , cfSeverity    :: Severity
  , cfAutoFix     :: [Fix]          -- ^ Auto-fix if available
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for concurrency detection
data ConcurrencyConfig = ConcurrencyConfig
  { ccEnabled          :: Bool   -- ^ Enable detection
  , ccCheckSTM         :: Bool   -- ^ Check STM patterns
  , ccCheckAsync       :: Bool   -- ^ Check async exception handling
  , ccCheckRace        :: Bool   -- ^ Check for race conditions
  , ccCheckDeadlock    :: Bool   -- ^ Check for deadlock patterns
  , ccCheckUnsafe      :: Bool   -- ^ Check unsafe concurrent operations
  , ccCheckResource    :: Bool   -- ^ Check resource leaks
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration
defaultConcurrencyConfig :: ConcurrencyConfig
defaultConcurrencyConfig = ConcurrencyConfig
  { ccEnabled = True
  , ccCheckSTM = True
  , ccCheckAsync = True
  , ccCheckRace = True
  , ccCheckDeadlock = True
  , ccCheckUnsafe = True
  , ccCheckResource = True
  }

--------------------------------------------------------------------------------
-- Detection
--------------------------------------------------------------------------------

-- | Detect concurrency issues in source code
detectConcurrencyIssues :: ConcurrencyConfig -> FilePath -> Text -> [Diagnostic]
detectConcurrencyIssues config path content
  | not (ccEnabled config) = []
  | otherwise =
    let findings = concat
          [ if ccCheckSTM config then detectSTMPatterns path content else []
          , if ccCheckAsync config then detectAsyncPatterns path content else []
          , if ccCheckRace config then detectRacePatterns path content else []
          , if ccCheckDeadlock config then detectDeadlockPatterns path content else []
          , if ccCheckUnsafe config then detectUnsafePatterns path content else []
          , if ccCheckResource config then detectResourcePatterns path content else []
          ]
    in map findingToDiagnostic findings

--------------------------------------------------------------------------------
-- STM Patterns
--------------------------------------------------------------------------------

-- | Detect STM anti-patterns
detectSTMPatterns :: FilePath -> Text -> [ConcurrencyFinding]
detectSTMPatterns path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkSTMLine path) linesWithNums

checkSTMLine :: FilePath -> (Int, Text) -> [ConcurrencyFinding]
checkSTMLine path (lineNum, line) = catMaybes
  [ -- retry without orElse
    if "retry" `T.isInfixOf` line && not ("orElse" `T.isInfixOf` line)
       && not (isComment line)
    then Just ConcurrencyFinding
      { cfCategory = STMAntiPattern
      , cfSpan = mkSpan path lineNum line "retry"
      , cfCode = T.strip line
      , cfExplanation = "Using 'retry' without 'orElse' can cause unbounded blocking"
      , cfSuggestion = Just "Consider using 'retry `orElse` alternative' to provide fallback"
      , cfSeverity = Warning
      , cfAutoFix = []
      }
    else Nothing

  , -- readTVar followed by writeTVar without check
    if "readTVar" `T.isInfixOf` line && "writeTVar" `T.isInfixOf` line
    then Just ConcurrencyFinding
      { cfCategory = STMAntiPattern
      , cfSpan = mkSpan path lineNum line "TVar"
      , cfCode = T.strip line
      , cfExplanation = "Read-modify-write on same line may indicate non-atomic pattern"
      , cfSuggestion = Just "Consider using 'modifyTVar' or 'stateTVar' for atomic update"
      , cfSeverity = Suggestion
      , cfAutoFix = []
      }
    else Nothing

  , -- atomically nested
    if T.count "atomically" line > 1
    then Just ConcurrencyFinding
      { cfCategory = STMAntiPattern
      , cfSpan = mkSpan path lineNum line "atomically"
      , cfCode = T.strip line
      , cfExplanation = "Nested 'atomically' blocks will cause runtime error"
      , cfSuggestion = Just "Remove nested atomically and compose STM actions instead"
      , cfSeverity = Error
      , cfAutoFix = []
      }
    else Nothing

  , -- unsafeIOToSTM
    if "unsafeIOToSTM" `T.isInfixOf` line
    then Just ConcurrencyFinding
      { cfCategory = STMAntiPattern
      , cfSpan = mkSpan path lineNum line "unsafeIOToSTM"
      , cfCode = T.strip line
      , cfExplanation = "'unsafeIOToSTM' can break STM invariants and cause inconsistency"
      , cfSuggestion = Just "Move IO operations outside of atomically block"
      , cfSeverity = Warning
      , cfAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Async Patterns
--------------------------------------------------------------------------------

-- | Detect async exception handling issues
detectAsyncPatterns :: FilePath -> Text -> [ConcurrencyFinding]
detectAsyncPatterns path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkAsyncLine path) linesWithNums

checkAsyncLine :: FilePath -> (Int, Text) -> [ConcurrencyFinding]
checkAsyncLine path (lineNum, line) = catMaybes
  [ -- catch without async exception handling
    if "catch " `T.isInfixOf` line && not ("SomeAsyncException" `T.isInfixOf` line)
       && not ("AsyncException" `T.isInfixOf` line) && not (isComment line)
    then Just ConcurrencyFinding
      { cfCategory = AsyncException
      , cfSpan = mkSpan path lineNum line "catch"
      , cfCode = T.strip line
      , cfExplanation = "Using 'catch' may swallow async exceptions, breaking cancellation"
      , cfSuggestion = Just "Use 'catchJust' or check for async exceptions explicitly"
      , cfSeverity = Warning
      , cfAutoFix = []
      }
    else Nothing

  , -- mask_ instead of mask
    if "mask_ " `T.isInfixOf` line || line `T.isSuffixOf` "mask_"
    then Just ConcurrencyFinding
      { cfCategory = AsyncException
      , cfSpan = mkSpan path lineNum line "mask_"
      , cfCode = T.strip line
      , cfExplanation = "'mask_' discards restore function, preventing interruptible operations"
      , cfSuggestion = Just "Use 'mask' and call restore for interruptible points"
      , cfSeverity = Suggestion
      , cfAutoFix = []
      }
    else Nothing

  , -- throwTo without mask
    if "throwTo" `T.isInfixOf` line && not ("mask" `T.isInfixOf` line)
    then Just ConcurrencyFinding
      { cfCategory = AsyncException
      , cfSpan = mkSpan path lineNum line "throwTo"
      , cfCode = T.strip line
      , cfExplanation = "'throwTo' may be delivered at unexpected times"
      , cfSuggestion = Just "Consider target thread's async exception state"
      , cfSeverity = Suggestion
      , cfAutoFix = []
      }
    else Nothing

  , -- uninterruptibleMask
    if "uninterruptibleMask" `T.isInfixOf` line
    then Just ConcurrencyFinding
      { cfCategory = AsyncException
      , cfSpan = mkSpan path lineNum line "uninterruptibleMask"
      , cfCode = T.strip line
      , cfExplanation = "'uninterruptibleMask' prevents all async exceptions, may cause deadlocks"
      , cfSuggestion = Just "Prefer 'mask' unless uninterruptibility is essential"
      , cfSeverity = Warning
      , cfAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Race Condition Patterns
--------------------------------------------------------------------------------

-- | Detect potential race conditions
detectRacePatterns :: FilePath -> Text -> [ConcurrencyFinding]
detectRacePatterns path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkRaceLine path) linesWithNums

checkRaceLine :: FilePath -> (Int, Text) -> [ConcurrencyFinding]
checkRaceLine path (lineNum, line) = catMaybes
  [ -- IORef without atomicModify
    if ("readIORef" `T.isInfixOf` line || "writeIORef" `T.isInfixOf` line)
       && not ("atomicModifyIORef" `T.isInfixOf` line)
       && not (isComment line)
    then Just ConcurrencyFinding
      { cfCategory = RaceCondition
      , cfSpan = mkSpan path lineNum line "IORef"
      , cfCode = T.strip line
      , cfExplanation = "Non-atomic IORef operations may cause race conditions"
      , cfSuggestion = Just "Use 'atomicModifyIORef' for thread-safe updates"
      , cfSeverity = Warning
      , cfAutoFix = []
      }
    else Nothing

  , -- modifyMVar_ pattern
    if "takeMVar" `T.isInfixOf` line
    then Just ConcurrencyFinding
      { cfCategory = RaceCondition
      , cfSpan = mkSpan path lineNum line "takeMVar"
      , cfCode = T.strip line
      , cfExplanation = "'takeMVar' followed by 'putMVar' is not exception-safe"
      , cfSuggestion = Just "Use 'modifyMVar_' or 'modifyMVar' for safe update"
      , cfSeverity = Warning
      , cfAutoFix = []
      }
    else Nothing

  , -- forkIO without link/wait
    if "forkIO" `T.isInfixOf` line && not ("link" `T.isInfixOf` line)
       && not ("wait" `T.isInfixOf` line) && not ("async" `T.isInfixOf` line)
    then Just ConcurrencyFinding
      { cfCategory = RaceCondition
      , cfSpan = mkSpan path lineNum line "forkIO"
      , cfCode = T.strip line
      , cfExplanation = "'forkIO' without supervision may lose exceptions silently"
      , cfSuggestion = Just "Consider 'async' from async package for supervised concurrency"
      , cfSeverity = Suggestion
      , cfAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Deadlock Patterns
--------------------------------------------------------------------------------

-- | Detect deadlock-prone patterns
detectDeadlockPatterns :: FilePath -> Text -> [ConcurrencyFinding]
detectDeadlockPatterns path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkDeadlockLine path) linesWithNums

checkDeadlockLine :: FilePath -> (Int, Text) -> [ConcurrencyFinding]
checkDeadlockLine path (lineNum, line) = catMaybes
  [ -- takeMVar multiple times
    if T.count "takeMVar" line > 1
    then Just ConcurrencyFinding
      { cfCategory = DeadlockRisk
      , cfSpan = mkSpan path lineNum line "takeMVar"
      , cfCode = T.strip line
      , cfExplanation = "Multiple 'takeMVar' on same line suggests lock ordering issues"
      , cfSuggestion = Just "Ensure consistent lock ordering to prevent deadlocks"
      , cfSeverity = Warning
      , cfAutoFix = []
      }
    else Nothing

  , -- QSem/QSemN
    if "waitQSem" `T.isInfixOf` line
    then Just ConcurrencyFinding
      { cfCategory = DeadlockRisk
      , cfSpan = mkSpan path lineNum line "waitQSem"
      , cfCode = T.strip line
      , cfExplanation = "QSem/QSemN can cause deadlocks if not properly released"
      , cfSuggestion = Just "Ensure 'signalQSem' is called in all code paths (use bracket)"
      , cfSeverity = Suggestion
      , cfAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Unsafe Concurrent Operations
--------------------------------------------------------------------------------

-- | Detect unsafe concurrent operations
detectUnsafePatterns :: FilePath -> Text -> [ConcurrencyFinding]
detectUnsafePatterns path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkUnsafeConcLine path) linesWithNums

checkUnsafeConcLine :: FilePath -> (Int, Text) -> [ConcurrencyFinding]
checkUnsafeConcLine path (lineNum, line) = catMaybes
  [ -- threadDelay with magic number
    if "threadDelay" `T.isInfixOf` line && hasHardcodedNumber line
    then Just ConcurrencyFinding
      { cfCategory = UnsafeConcurrent
      , cfSpan = mkSpan path lineNum line "threadDelay"
      , cfCode = T.strip line
      , cfExplanation = "Hardcoded delays are fragile for synchronization"
      , cfSuggestion = Just "Use proper synchronization primitives (MVar, STM, async)"
      , cfSeverity = Warning
      , cfAutoFix = []
      }
    else Nothing

  , -- timeout with exception
    if "timeout" `T.isInfixOf` line && "throwIO" `T.isInfixOf` line
    then Just ConcurrencyFinding
      { cfCategory = UnsafeConcurrent
      , cfSpan = mkSpan path lineNum line "timeout"
      , cfCode = T.strip line
      , cfExplanation = "Timeout with exception may leave resources in inconsistent state"
      , cfSuggestion = Just "Use bracket or ensure cleanup in timeout handler"
      , cfSeverity = Warning
      , cfAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Resource Leak Patterns
--------------------------------------------------------------------------------

-- | Detect resource leaks in concurrent code
detectResourcePatterns :: FilePath -> Text -> [ConcurrencyFinding]
detectResourcePatterns path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkResourceLine path) linesWithNums

checkResourceLine :: FilePath -> (Int, Text) -> [ConcurrencyFinding]
checkResourceLine path (lineNum, line) = catMaybes
  [ -- forkIO without bracket
    if "forkIO" `T.isInfixOf` line && not ("bracket" `T.isInfixOf` line)
       && ("open" `T.isInfixOf` T.toLower line || "acquire" `T.isInfixOf` T.toLower line)
    then Just ConcurrencyFinding
      { cfCategory = ResourceLeak
      , cfSpan = mkSpan path lineNum line "forkIO"
      , cfCode = T.strip line
      , cfExplanation = "Resource acquisition in forked thread without cleanup"
      , cfSuggestion = Just "Use 'bracket' or 'withAsync' for resource cleanup"
      , cfSeverity = Warning
      , cfAutoFix = []
      }
    else Nothing

  , -- Chan without bound
    if ("newChan" `T.isInfixOf` line || "writeChan" `T.isInfixOf` line)
       && not ("TBQueue" `T.isInfixOf` line) && not ("bounded" `T.isInfixOf` T.toLower line)
    then Just ConcurrencyFinding
      { cfCategory = ResourceLeak
      , cfSpan = mkSpan path lineNum line "Chan"
      , cfCode = T.strip line
      , cfExplanation = "Unbounded Chan can grow without limit, causing memory leaks"
      , cfSuggestion = Just "Consider using 'TBQueue' from stm for bounded queues"
      , cfSeverity = Suggestion
      , cfAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Convert finding to diagnostic
findingToDiagnostic :: ConcurrencyFinding -> Diagnostic
findingToDiagnostic ConcurrencyFinding{..} = Diagnostic
  { diagSpan = cfSpan
  , diagSeverity = cfSeverity
  , diagKind = PerformanceIssue  -- Using PerformanceIssue as closest match
  , diagMessage = cfExplanation <> maybe "" (\s -> ". " <> s) cfSuggestion
  , diagCode = Just $ "concurrency/" <> categoryToCode cfCategory
  , diagFixes = cfAutoFix
  , diagRelated = []
  }

-- | Convert category to diagnostic code suffix
categoryToCode :: ConcurrencyCategory -> Text
categoryToCode = \case
  STMAntiPattern -> "stm"
  AsyncException -> "async-exception"
  RaceCondition -> "race"
  DeadlockRisk -> "deadlock"
  UnsafeConcurrent -> "unsafe"
  ResourceLeak -> "resource-leak"

-- | Create span for a keyword match
mkSpan :: FilePath -> Int -> Text -> Text -> SrcSpan
mkSpan path lineNum line keyword =
  let col = maybe 1 (+ 1) $ T.findIndex (== T.head keyword) line
  in mkSrcSpanRaw path lineNum col lineNum (col + T.length keyword)

-- | Check if a line is a comment
isComment :: Text -> Bool
isComment line =
  let stripped = T.stripStart line
  in "--" `T.isPrefixOf` stripped || "{-" `T.isPrefixOf` stripped

-- | Check if line has hardcoded numbers
hasHardcodedNumber :: Text -> Bool
hasHardcodedNumber line = any (`T.isInfixOf` line) ["1000", "2000", "5000", "10000", "100000"]
