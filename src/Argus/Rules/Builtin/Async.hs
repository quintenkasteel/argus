{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Async
-- Description : Async and concurrency rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for async operations, concurrency patterns, STM,
-- and parallel programming best practices.
--
-- == Rule Categories
--
-- * __Async__: Async library patterns
-- * __STM__: Software Transactional Memory
-- * __MVar__: MVar operations
-- * __Exceptions__: Async exception handling
-- * __Parallel__: Parallel evaluation

module Argus.Rules.Builtin.Async
  ( -- * Rule Sets
    asyncRules
  , asyncLibRules
  , stmRules
  , mvarRules
  , asyncExceptionRules
  , parallelRules

    -- * Async Library Rules
  , asyncPattern
  , asyncWait
  , asyncCancel
  , withAsyncPattern
  , concurrently
  , race
  , mapConcurrently
  , pooledMapConcurrently

    -- * STM Rules
  , atomically
  , stmRetry
  , stmOrElse
  , tvarNew
  , tvarRead
  , tvarWrite
  , tvarModify
  , checkSTM

    -- * MVar Rules
  , mvarNew
  , mvarTake
  , mvarPut
  , mvarRead
  , mvarModify
  , mvarEmpty
  , mvarDeadlock

    -- * Async Exception Rules
  , maskPattern
  , asyncUninterruptibleMask
  , bracketAsync
  , catchAsync
  , throwToPattern
  , asyncExceptionType

    -- * Parallel Rules
  , parEval
  , rpar
  , rseq
  , parMap
  , parList
  , deepseqForce

    -- * Rule Count
  , asyncRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All async-related rules.
asyncRules :: [Rule]
asyncRules = mconcat
  [ asyncLibRules
  , stmRules
  , mvarRules
  , asyncExceptionRules
  , parallelRules
  ]

-- | Total count of async rules.
asyncRuleCount :: Int
asyncRuleCount = length asyncRules

--------------------------------------------------------------------------------
-- Async Library Rules
--------------------------------------------------------------------------------

-- | Rules for async library patterns.
asyncLibRules :: [Rule]
asyncLibRules =
  [ asyncPattern
  , asyncWait
  , asyncCancel
  , withAsyncPattern
  , concurrently
  , race
  , mapConcurrently
  , pooledMapConcurrently
  ]

-- | async pattern.
--
-- @
-- a <- async action  -- Fork async
-- @
asyncPattern :: Rule
asyncPattern =
  rule "async-pattern" $
    match ("async _action" ==> "async _action")
    & category Style
    & severity Info
    & message "Forking async action - remember to wait or cancel"
    & safetyLevel ManualReview

-- | wait for async.
--
-- @
-- wait a  -- Block until complete
-- @
asyncWait :: Rule
asyncWait =
  rule "async-wait" $
    match ("wait _a" ==> "wait _a")
    & category Style
    & severity Info
    & message "Waiting for async result"
    & safetyLevel ManualReview

-- | cancel async.
--
-- @
-- cancel a  -- Cancel async
-- @
asyncCancel :: Rule
asyncCancel =
  rule "async-cancel" $
    match ("cancel _a" ==> "cancel _a")
    & category Style
    & severity Info
    & message "Cancelling async - will throw AsyncCancelled"
    & safetyLevel ManualReview

-- | withAsync pattern.
--
-- @
-- withAsync action $ \\a -> ...  -- Scoped async
-- @
withAsyncPattern :: Rule
withAsyncPattern =
  rule "withAsync-pattern" $
    match ("withAsync _action _k" ==> "withAsync _action _k")
    & category Style
    & severity Info
    & message "Using withAsync - automatically cancels on scope exit"
    & safetyLevel ManualReview

-- | concurrently pattern.
--
-- @
-- concurrently action1 action2  -- Run both
-- @
concurrently :: Rule
concurrently =
  rule "concurrently" $
    match ("concurrently _a _b" ==> "concurrently _a _b")
    & category Style
    & severity Info
    & message "Running actions concurrently - both must complete"
    & safetyLevel ManualReview

-- | race pattern.
--
-- @
-- race action1 action2  -- First to complete wins
-- @
race :: Rule
race =
  rule "race" $
    match ("race _a _b" ==> "race _a _b")
    & category Style
    & severity Info
    & message "Racing actions - loser is cancelled"
    & safetyLevel ManualReview

-- | mapConcurrently.
--
-- @
-- mapConcurrently f xs  -- Concurrent map
-- @
mapConcurrently :: Rule
mapConcurrently =
  rule "mapConcurrently" $
    match ("mapConcurrently _f _xs" ==> "mapConcurrently _f _xs")
    & category Style
    & severity Info
    & message "Concurrent map - unbounded parallelism"
    & note "Consider pooledMapConcurrently for bounded"
    & safetyLevel ManualReview

-- | pooledMapConcurrently.
--
-- @
-- pooledMapConcurrently f xs  -- Bounded concurrent map
-- @
pooledMapConcurrently :: Rule
pooledMapConcurrently =
  rule "pooledMapConcurrently" $
    matchText "pooledMapConcurrently"
    & category Style
    & severity Info
    & message "Using pooled concurrent map - good for resource control"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- STM Rules
--------------------------------------------------------------------------------

-- | Rules for STM patterns.
stmRules :: [Rule]
stmRules =
  [ atomically
  , stmRetry
  , stmOrElse
  , tvarNew
  , tvarRead
  , tvarWrite
  , tvarModify
  , checkSTM
  ]

-- | atomically block.
--
-- @
-- atomically $ do ...  -- STM transaction
-- @
atomically :: Rule
atomically =
  rule "atomically" $
    match ("atomically _stm" ==> "atomically _stm")
    & category Style
    & severity Info
    & message "STM transaction - atomic and composable"
    & safetyLevel ManualReview

-- | retry in STM.
--
-- @
-- retry  -- Block until TVar changes
-- @
stmRetry :: Rule
stmRetry =
  rule "stm-retry" $
    matchText "\\bretry\\b"
    & category Style
    & severity Info
    & message "STM retry - will block until transaction can succeed"
    & safetyLevel ManualReview

-- | @orElse@ in STM.
--
-- @
-- action1 \`orElse\` action2  -- Try alternatives
-- @
stmOrElse :: Rule
stmOrElse =
  rule "stm-orElse" $
    matchText "`orElse`|orElse\\s"
    & category Style
    & severity Info
    & message "STM orElse - provides alternative on retry"
    & safetyLevel ManualReview

-- | newTVar.
--
-- @
-- newTVar initialValue  -- Create TVar
-- @
tvarNew :: Rule
tvarNew =
  rule "tvar-new" $
    matchText "newTVar|newTVarIO"
    & category Style
    & severity Info
    & message "Creating new TVar"
    & safetyLevel ManualReview

-- | readTVar.
--
-- @
-- readTVar var  -- Read TVar in STM
-- @
tvarRead :: Rule
tvarRead =
  rule "tvar-read" $
    match ("readTVar _var" ==> "readTVar _var")
    & category Style
    & severity Info
    & message "Reading TVar in STM"
    & safetyLevel ManualReview

-- | writeTVar.
--
-- @
-- writeTVar var value  -- Write TVar in STM
-- @
tvarWrite :: Rule
tvarWrite =
  rule "tvar-write" $
    match ("writeTVar _var _val" ==> "writeTVar _var _val")
    & category Style
    & severity Info
    & message "Writing TVar in STM"
    & safetyLevel ManualReview

-- | modifyTVar.
--
-- @
-- modifyTVar var f  -- Modify TVar in STM
-- @
tvarModify :: Rule
tvarModify =
  rule "tvar-modify" $
    matchText "modifyTVar"
    & category Style
    & severity Info
    & message "Modifying TVar - consider modifyTVar' for strictness"
    & safetyLevel ManualReview

-- | check in STM.
--
-- @
-- check condition  -- Retry if false
-- @
checkSTM :: Rule
checkSTM =
  rule "check-stm" $
    matchText "\\bcheck\\s"
    & category Style
    & severity Info
    & message "STM check - retries if condition is false"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- MVar Rules
--------------------------------------------------------------------------------

-- | Rules for MVar operations.
mvarRules :: [Rule]
mvarRules =
  [ mvarNew
  , mvarTake
  , mvarPut
  , mvarRead
  , mvarModify
  , mvarEmpty
  , mvarDeadlock
  ]

-- | newMVar.
--
-- @
-- newMVar initialValue  -- Create MVar
-- @
mvarNew :: Rule
mvarNew =
  rule "mvar-new" $
    matchText "newMVar|newEmptyMVar"
    & category Style
    & severity Info
    & message "Creating new MVar"
    & safetyLevel ManualReview

-- | takeMVar.
--
-- @
-- takeMVar var  -- Take and empty
-- @
mvarTake :: Rule
mvarTake =
  rule "mvar-take" $
    match ("takeMVar _var" ==> "takeMVar _var")
    & category Style
    & severity Info
    & message "Taking from MVar - blocks if empty"
    & safetyLevel ManualReview

-- | putMVar.
--
-- @
-- putMVar var value  -- Put into MVar
-- @
mvarPut :: Rule
mvarPut =
  rule "mvar-put" $
    match ("putMVar _var _val" ==> "putMVar _var _val")
    & category Style
    & severity Info
    & message "Putting into MVar - blocks if full"
    & safetyLevel ManualReview

-- | readMVar.
--
-- @
-- readMVar var  -- Read without taking
-- @
mvarRead :: Rule
mvarRead =
  rule "mvar-read" $
    match ("readMVar _var" ==> "readMVar _var")
    & category Style
    & severity Info
    & message "Reading MVar - atomic read without taking"
    & safetyLevel ManualReview

-- | modifyMVar.
--
-- @
-- modifyMVar var f  -- Atomic modify
-- @
mvarModify :: Rule
mvarModify =
  rule "mvar-modify" $
    matchText "modifyMVar"
    & category Style
    & severity Info
    & message "Modifying MVar - consider modifyMVar' for strictness"
    & safetyLevel ManualReview

-- | Empty MVar check.
--
-- @
-- isEmptyMVar var  -- Check if empty
-- @
mvarEmpty :: Rule
mvarEmpty =
  rule "mvar-empty" $
    matchText "isEmptyMVar"
    & category Style
    & severity Warning
    & message "isEmptyMVar - result may be stale immediately"
    & note "Race condition warning"

-- | Potential deadlock pattern.
--
-- @
-- takeMVar ... takeMVar  -- Multiple takes
-- @
mvarDeadlock :: Rule
mvarDeadlock =
  rule "mvar-deadlock" $
    matchText "takeMVar.*takeMVar"
    & category Correctness
    & severity Warning
    & message "Multiple takeMVar calls - potential deadlock"

--------------------------------------------------------------------------------
-- Async Exception Rules
--------------------------------------------------------------------------------

-- | Rules for async exception handling.
asyncExceptionRules :: [Rule]
asyncExceptionRules =
  [ maskPattern
  , asyncUninterruptibleMask
  , bracketAsync
  , catchAsync
  , throwToPattern
  , asyncExceptionType
  ]

-- | mask for async exceptions.
--
-- @
-- mask $ \\restore -> ...  -- Mask async exceptions
-- @
maskPattern :: Rule
maskPattern =
  rule "mask-pattern" $
    matchText "\\bmask\\s*\\$|\\bmask\\s+_"
    & category Style
    & severity Info
    & message "Masking async exceptions - use restore in blocking operations"
    & safetyLevel ManualReview

-- | uninterruptibleMask.
--
-- @
-- uninterruptibleMask $ ...  -- Fully mask
-- @
asyncUninterruptibleMask :: Rule
asyncUninterruptibleMask =
  rule "uninterruptibleMask" $
    matchText "uninterruptibleMask"
    & category Style
    & severity Warning
    & message "uninterruptibleMask - can delay cancellation indefinitely"
    & note "Use sparingly and keep brief"

-- | bracket with async exceptions.
--
-- @
-- bracket acquire release use  -- Exception-safe
-- @
bracketAsync :: Rule
bracketAsync =
  rule "bracket-async" $
    match ("bracket _acquire _release _use" ==> "bracket _acquire _release _use")
    & category Style
    & severity Info
    & message "bracket handles async exceptions in acquire/release"
    & safetyLevel ManualReview

-- | Catching async exceptions.
--
-- @
-- catch action handler  -- May catch AsyncException
-- @
catchAsync :: Rule
catchAsync =
  rule "catch-async" $
    matchText "catch.*SomeException|catch.*SomeAsyncException"
    & category Safety
    & severity Warning
    & message "Catching all exceptions may swallow async exceptions"
    & note "Re-throw async exceptions or use specific types"

-- | throwTo pattern.
--
-- @
-- throwTo threadId exception  -- Async throw
-- @
throwToPattern :: Rule
throwToPattern =
  rule "throwTo-pattern" $
    match ("throwTo _tid _ex" ==> "throwTo _tid _ex")
    & category Style
    & severity Info
    & message "Throwing async exception to thread"
    & safetyLevel ManualReview

-- | AsyncException type.
--
-- @
-- :: AsyncException  -- Async exception type
-- @
asyncExceptionType :: Rule
asyncExceptionType =
  rule "async-exception-type" $
    matchText ":: AsyncException|AsyncCancelled|UserInterrupt"
    & category Style
    & severity Info
    & message "Working with async exception types"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Parallel Rules
--------------------------------------------------------------------------------

-- | Rules for parallel evaluation.
parallelRules :: [Rule]
parallelRules =
  [ parEval
  , rpar
  , rseq
  , parMap
  , parList
  , deepseqForce
  ]

-- | par for parallel evaluation.
--
-- @
-- x \`par\` y \`pseq\` (x + y)  -- Parallel evaluation
-- @
parEval :: Rule
parEval =
  rule "par-eval" $
    matchText "`par`|\\bpar\\s"
    & category Performance
    & severity Info
    & message "Using par for parallel sparks"
    & safetyLevel ManualReview

-- | rpar strategy.
--
-- @
-- rpar  -- Spark for parallel evaluation
-- @
rpar :: Rule
rpar =
  rule "rpar" $
    matchText "\\brpar\\b"
    & category Performance
    & severity Info
    & message "Using rpar strategy for parallel evaluation"
    & safetyLevel ManualReview

-- | rseq strategy.
--
-- @
-- rseq  -- Sequential evaluation
-- @
rseq :: Rule
rseq =
  rule "rseq" $
    matchText "\\brseq\\b"
    & category Performance
    & severity Info
    & message "Using rseq strategy for sequential evaluation"
    & safetyLevel ManualReview

-- | parMap for parallel map.
--
-- @
-- parMap rpar f xs  -- Parallel map
-- @
parMap :: Rule
parMap =
  rule "parMap" $
    matchText "\\bparMap\\b"
    & category Performance
    & severity Info
    & message "Using parMap for parallel mapping"
    & safetyLevel ManualReview

-- | parList strategy.
--
-- @
-- parList rpar  -- Parallel list strategy
-- @
parList :: Rule
parList =
  rule "parList" $
    matchText "\\bparList\\b"
    & category Performance
    & severity Info
    & message "Using parList strategy"
    & safetyLevel ManualReview

-- | deepseq/force for full evaluation.
--
-- @
-- force x  -- Fully evaluate
-- @
deepseqForce :: Rule
deepseqForce =
  rule "deepseq-force" $
    matchText "\\bforce\\b|\\bdeepseq\\b"
    & category Performance
    & severity Info
    & message "Using force/deepseq for full evaluation"
    & safetyLevel ManualReview
