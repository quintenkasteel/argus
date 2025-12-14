{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.IO
-- Description : IO safety and best practice rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for IO operations, file handling, and resource management.
-- Encourages safe IO patterns and proper resource cleanup.
--
-- == Rule Categories
--
-- * __File Handling__: Safe file operations
-- * __Resource Management__: Resource cleanup patterns
-- * __Handle Operations__: Handle usage best practices
-- * __System Operations__: System call patterns

module Argus.Rules.Builtin.IO
  ( -- * Rule Sets
    ioRules
  , fileHandlingRules
  , resourceManagementRules
  , handleRules
  , systemRules

    -- * File Handling
  , readFileStrict
  , writeFileAtomic
  , withFileBracket
  , openFileClose
  , hGetContentsLazy
  , appendFileHandle
  , removeFileExists
  , renameFileCross

    -- * Resource Management
  , bracketPattern
  , bracketOnError
  , finallyPattern
  , withResourcePattern
  , allocaPattern
  , maskAsync
  , uninterruptibleMask

    -- * Handle Operations
  , hCloseCheck
  , hFlushBefore
  , hSetBuffering
  , hIsEOFCheck
  , hSeekBounds
  , hTellPosition
  , handleLeak

    -- * System Operations
  , getEnvMissing
  , lookupEnvPrefer
  , setEnvEmpty
  , getCurrentDirectory
  , setCurrentDirectory
  , getArgsEmpty
  , exitFailure
  , systemCommand

    -- * Rule Count
  , ioRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All IO-related rules.
ioRules :: [Rule]
ioRules = mconcat
  [ fileHandlingRules
  , resourceManagementRules
  , handleRules
  , systemRules
  ]

-- | Total count of IO rules.
ioRuleCount :: Int
ioRuleCount = length ioRules

--------------------------------------------------------------------------------
-- File Handling Rules
--------------------------------------------------------------------------------

-- | Rules for safe file handling.
fileHandlingRules :: [Rule]
fileHandlingRules =
  [ readFileStrict
  , writeFileAtomic
  , withFileBracket
  , openFileClose
  , hGetContentsLazy
  , appendFileHandle
  , removeFileExists
  , renameFileCross
  ]

-- | readFile is lazy and keeps handle open.
--
-- @
-- readFile path  -- Lazy IO, can leak handles
-- @
readFileStrict :: Rule
readFileStrict =
  rule "readFile-strict" $
    match ("readFile _path" ==> "T.readFile _path")
    & category Safety
    & severity Warning
    & message "readFile uses lazy IO which can leak handles"
    & note "Use strict readFile from Data.Text.IO or ByteString"

-- | writeFile should be atomic.
--
-- @
-- writeFile path contents  -- Not atomic, can corrupt on crash
-- @
writeFileAtomic :: Rule
writeFileAtomic =
  rule "writeFile-atomic" $
    match ("writeFile _path _contents" ==> "writeFile _path _contents")
    & category Safety
    & severity Info
    & message "writeFile is not atomic - may corrupt file on crash"
    & note "Consider writeFileAtomic from unix-compat or atomic-write"
    & safetyLevel ManualReview

-- | Use withFile instead of openFile/hClose.
--
-- @
-- h <- openFile path mode; ...; hClose h
-- ==> withFile path mode $ \h -> ...
-- @
withFileBracket :: Rule
withFileBracket =
  rule "withFile-bracket" $
    matchText "openFile .+ .+Mode"
    & category Safety
    & severity Warning
    & message "Use withFile instead of openFile/hClose"
    & note "withFile ensures handle is closed even on exceptions"

-- | openFile without corresponding hClose.
--
-- @
-- h <- openFile path mode  -- Must call hClose!
-- @
openFileClose :: Rule
openFileClose =
  rule "openFile-hClose" $
    match ("openFile _path _mode" ==> "openFile _path _mode")
    & category Safety
    & severity Warning
    & message "openFile without visible hClose - possible handle leak"
    & note "Use withFile for automatic cleanup"
    & safetyLevel ManualReview

-- | hGetContents is lazy.
--
-- @
-- hGetContents h  -- Lazy, keeps handle open until fully consumed
-- @
hGetContentsLazy :: Rule
hGetContentsLazy =
  rule "hGetContents-lazy" $
    match ("hGetContents _h" ==> "hGetContents _h")
    & category Safety
    & severity Warning
    & message "hGetContents uses lazy IO - handle remains open"
    & note "Use strict variant or read entire contents"
    & safetyLevel ManualReview

-- | appendFile reopens file each call.
--
-- @
-- mapM_ (appendFile path) lines  -- Inefficient, opens file repeatedly
-- @
appendFileHandle :: Rule
appendFileHandle =
  rule "appendFile-handle" $
    matchText "mapM_.+appendFile"
    & category Performance
    & severity Warning
    & message "Multiple appendFile calls reopen file each time"
    & note "Open file once with AppendMode and write multiple times"

-- | Check file exists before removing.
--
-- @
-- removeFile path  -- Throws if file doesn't exist
-- @
removeFileExists :: Rule
removeFileExists =
  rule "removeFile-exists" $
    match ("removeFile _path" ==> "removeFile _path")
    & category Safety
    & severity Info
    & message "removeFile throws if file doesn't exist"
    & note "Check with doesFileExist or use removePathForcibly"
    & safetyLevel ManualReview

-- | renameFile may fail across filesystems.
--
-- @
-- renameFile src dst  -- May fail across filesystems
-- @
renameFileCross :: Rule
renameFileCross =
  rule "renameFile-cross" $
    match ("renameFile _src _dst" ==> "renameFile _src _dst")
    & category Safety
    & severity Info
    & message "renameFile may fail across different filesystems"
    & note "For cross-filesystem moves, copy then delete"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Resource Management Rules
--------------------------------------------------------------------------------

-- | Rules for proper resource management.
resourceManagementRules :: [Rule]
resourceManagementRules =
  [ bracketPattern
  , bracketOnError
  , finallyPattern
  , withResourcePattern
  , allocaPattern
  , maskAsync
  , uninterruptibleMask
  ]

-- | Use bracket for resource acquisition.
--
-- @
-- bracket acquire release use
-- @
bracketPattern :: Rule
bracketPattern =
  rule "bracket-pattern" $
    match ("bracket _acquire _release _use" ==> "bracket _acquire _release _use")
    & category Style
    & severity Info
    & message "Using bracket for resource management - good practice"
    & safetyLevel ManualReview

-- | bracketOnError for error-only cleanup.
--
-- @
-- bracketOnError acquire release use  -- Release only on error
-- @
bracketOnError :: Rule
bracketOnError =
  rule "bracketOnError-pattern" $
    match ("bracketOnError _acquire _release _use" ==> "bracketOnError _acquire _release _use")
    & category Style
    & severity Info
    & message "bracketOnError releases only when inner action throws"
    & safetyLevel ManualReview

-- | Use finally for cleanup.
--
-- @
-- action `finally` cleanup
-- @
finallyPattern :: Rule
finallyPattern =
  rule "finally-pattern" $
    match ("finally _action _cleanup" ==> "finally _action _cleanup")
    & category Style
    & severity Info
    & message "Using finally for cleanup - ensures cleanup runs"
    & safetyLevel ManualReview

-- | Use withX pattern for resources.
--
-- @
-- with* functions ensure proper cleanup
-- @
withResourcePattern :: Rule
withResourcePattern =
  rule "with-resource" $
    matchText "^with[A-Z][a-zA-Z]+ "
    & category Style
    & severity Info
    & message "Using withX pattern for resource management"
    & safetyLevel ManualReview

-- | alloca should be used carefully.
--
-- @
-- alloca $ \ptr -> ...
-- @
allocaPattern :: Rule
allocaPattern =
  rule "alloca-pattern" $
    match ("alloca _f" ==> "alloca _f")
    & category Safety
    & severity Warning
    & message "alloca allocates on stack - ensure ptr doesn't escape"
    & note "Ptr from alloca must not be used outside the callback"
    & safetyLevel ManualReview

-- | mask for async exception safety.
--
-- @
-- mask $ \restore -> ...
-- @
maskAsync :: Rule
maskAsync =
  rule "mask-async" $
    match ("mask _f" ==> "mask _f")
    & category Safety
    & severity Info
    & message "Using mask for async exception safety"
    & note "Ensure restore is called appropriately"
    & safetyLevel ManualReview

-- | uninterruptibleMask is dangerous.
--
-- @
-- uninterruptibleMask $ \restore -> ...
-- @
uninterruptibleMask :: Rule
uninterruptibleMask =
  rule "uninterruptibleMask-dangerous" $
    match ("uninterruptibleMask _f" ==> "uninterruptibleMask _f")
    & category Safety
    & severity Warning
    & message "uninterruptibleMask blocks all async exceptions"
    & note "Use sparingly - can prevent thread from being killed"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Handle Rules
--------------------------------------------------------------------------------

-- | Rules for handle operations.
handleRules :: [Rule]
handleRules =
  [ hCloseCheck
  , hFlushBefore
  , hSetBuffering
  , hIsEOFCheck
  , hSeekBounds
  , hTellPosition
  , handleLeak
  ]

-- | Check handle is valid before hClose.
--
-- @
-- hClose h  -- May throw if already closed
-- @
hCloseCheck :: Rule
hCloseCheck =
  rule "hClose-check" $
    match ("hClose _h" ==> "hClose _h")
    & category Safety
    & severity Info
    & message "hClose throws if handle is already closed"
    & note "Consider using hIsOpen or ignoring exception"
    & safetyLevel ManualReview

-- | Flush before sensitive operations.
--
-- @
-- hFlush h  -- Ensures buffered data is written
-- @
hFlushBefore :: Rule
hFlushBefore =
  rule "hFlush-before" $
    match ("hFlush _h" ==> "hFlush _h")
    & category Style
    & severity Info
    & message "Using hFlush - ensure it's called before close or fork"
    & safetyLevel ManualReview

-- | Consider buffer settings.
--
-- @
-- hSetBuffering h mode
-- @
hSetBuffering :: Rule
hSetBuffering =
  rule "hSetBuffering-pattern" $
    match ("hSetBuffering _h _mode" ==> "hSetBuffering _h _mode")
    & category Performance
    & severity Info
    & message "Setting buffer mode - choose appropriate mode for use case"
    & note "NoBuffering for interactive, BlockBuffering for bulk"
    & safetyLevel ManualReview

-- | Check EOF before reading.
--
-- @
-- hIsEOF h  -- Check before reading to avoid exception
-- @
hIsEOFCheck :: Rule
hIsEOFCheck =
  rule "hIsEOF-check" $
    match ("hIsEOF _h" ==> "hIsEOF _h")
    & category Style
    & severity Info
    & message "Checking for EOF - good practice before reading"
    & safetyLevel ManualReview

-- | hSeek bounds checking.
--
-- @
-- hSeek h mode offset  -- May throw on invalid position
-- @
hSeekBounds :: Rule
hSeekBounds =
  rule "hSeek-bounds" $
    match ("hSeek _h _mode _offset" ==> "hSeek _h _mode _offset")
    & category Safety
    & severity Info
    & message "hSeek may throw on invalid position"
    & note "Ensure offset is within file bounds"
    & safetyLevel ManualReview

-- | hTell for getting position.
--
-- @
-- hTell h  -- Returns current position
-- @
hTellPosition :: Rule
hTellPosition =
  rule "hTell-position" $
    match ("hTell _h" ==> "hTell _h")
    & category Style
    & severity Info
    & message "Using hTell to get handle position"
    & safetyLevel ManualReview

-- | Handle leak detection.
--
-- @
-- openFile path mode  -- Without withFile suggests leak
-- @
handleLeak :: Rule
handleLeak =
  rule "handle-leak" $
    matchText "openFile"
    & category Safety
    & severity Warning
    & message "openFile without withFile - possible handle leak"
    & note "Prefer withFile for automatic resource cleanup"

--------------------------------------------------------------------------------
-- System Rules
--------------------------------------------------------------------------------

-- | Rules for system operations.
systemRules :: [Rule]
systemRules =
  [ getEnvMissing
  , lookupEnvPrefer
  , setEnvEmpty
  , getCurrentDirectory
  , setCurrentDirectory
  , getArgsEmpty
  , exitFailure
  , systemCommand
  ]

-- | getEnv throws on missing variable.
--
-- @
-- getEnv "VAR"  ==>  lookupEnv "VAR"
-- @
getEnvMissing :: Rule
getEnvMissing =
  rule "getEnv-missing" $
    match ("getEnv _var" ==> "lookupEnv _var")
    & category Safety
    & severity Warning
    & message "getEnv throws if environment variable is missing"
    & note "Use lookupEnv for safe lookup"

-- | Prefer lookupEnv over getEnv.
--
-- @
-- lookupEnv "VAR"  -- Returns Maybe String
-- @
lookupEnvPrefer :: Rule
lookupEnvPrefer =
  rule "lookupEnv-prefer" $
    match ("lookupEnv _var" ==> "lookupEnv _var")
    & category Style
    & severity Info
    & message "Using lookupEnv - returns Maybe for safe handling"
    & safetyLevel ManualReview

-- | setEnv with empty value.
--
-- @
-- setEnv "VAR" ""  -- Behavior varies by platform
-- @
setEnvEmpty :: Rule
setEnvEmpty =
  rule "setEnv-empty" $
    match ("setEnv _var \"\"" ==> "unsetEnv _var")
    & category Safety
    & severity Info
    & message "setEnv with empty string - use unsetEnv to clear"
    & note "Empty environment variable behavior varies by platform"

-- | getCurrentDirectory may change.
--
-- @
-- getCurrentDirectory  -- May change between calls
-- @
getCurrentDirectory :: Rule
getCurrentDirectory =
  rule "getCurrentDirectory-cache" $
    match ("getCurrentDirectory" ==> "getCurrentDirectory")
    & category Safety
    & severity Info
    & message "getCurrentDirectory result may change between calls"
    & note "Cache result if used multiple times"
    & safetyLevel ManualReview

-- | setCurrentDirectory affects global state.
--
-- @
-- setCurrentDirectory path  -- Global state change
-- @
setCurrentDirectory :: Rule
setCurrentDirectory =
  rule "setCurrentDirectory-global" $
    match ("setCurrentDirectory _path" ==> "setCurrentDirectory _path")
    & category Safety
    & severity Warning
    & message "setCurrentDirectory modifies global process state"
    & note "Affects all threads; prefer withCurrentDirectory"

-- | getArgs may be empty.
--
-- @
-- getArgs  -- May return empty list
-- @
getArgsEmpty :: Rule
getArgsEmpty =
  rule "getArgs-empty" $
    match ("getArgs" ==> "getArgs")
    & category Safety
    & severity Info
    & message "getArgs may return empty list"
    & note "Handle case of no arguments"
    & safetyLevel ManualReview

-- | exitFailure vs exitSuccess.
--
-- @
-- exitFailure  -- Terminates with exit code 1
-- @
exitFailure :: Rule
exitFailure =
  rule "exitFailure-terminates" $
    match ("exitFailure" ==> "exitFailure")
    & category Safety
    & severity Info
    & message "exitFailure terminates the program"
    & note "Ensure cleanup is done before calling"
    & safetyLevel ManualReview

-- | system command injection.
--
-- @
-- system cmd  -- Vulnerable to command injection
-- @
systemCommand :: Rule
systemCommand =
  rule "system-command-injection" $
    match ("system _cmd" ==> "system _cmd")
    & category Security
    & severity Error
    & message "system is vulnerable to command injection"
    & note "Use typed process APIs like System.Process.Typed"
    & safetyLevel ManualReview
