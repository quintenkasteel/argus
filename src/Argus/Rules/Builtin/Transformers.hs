{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Transformers
-- Description : Monad transformer and mtl rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for monad transformers, mtl, and effect patterns.
-- Encourages proper usage and common simplifications.
--
-- == Rule Categories
--
-- * __Reader__: ReaderT patterns
-- * __State__: StateT patterns
-- * __Writer__: WriterT patterns
-- * __Except__: ExceptT/Either patterns
-- * __Lift__: Lifting operations

module Argus.Rules.Builtin.Transformers
  ( -- * Rule Sets
    mtlTransformerRules
  , readerRules
  , stateRules
  , writerRules
  , exceptRules
  , liftRules

    -- * Reader Rules
  , askRedundant
  , asksSimplify
  , localPattern
  , readerToFunction
  , runReaderTwice
  , askReturn
  , readerMapEnv

    -- * State Rules
  , getReturn
  , putGet
  , modifyGet
  , stateGetPut
  , getModify
  , execStateRedundant
  , evalStateRedundant

    -- * Writer Rules
  , tellMempty
  , tellMconcat
  , writerLazy
  , writerStrict
  , listenIgnore
  , passSimplify
  , censorPattern

    -- * Except Rules
  , throwECatch
  , catchEThrow
  , runExceptTPattern
  , eitherToExceptT
  , exceptTToEither
  , tryPattern
  , exceptTLift

    -- * Lift Rules
  , liftReturn
  , liftBind
  , liftLift
  , liftIO
  , hoistPattern
  , mapReaderT

    -- * Rule Count
  , transformerRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All transformer-related rules.
mtlTransformerRules :: [Rule]
mtlTransformerRules = mconcat
  [ readerRules
  , stateRules
  , writerRules
  , exceptRules
  , liftRules
  ]

-- | Total count of transformer rules.
transformerRuleCount :: Int
transformerRuleCount = length mtlTransformerRules

--------------------------------------------------------------------------------
-- Reader Rules
--------------------------------------------------------------------------------

-- | Rules for ReaderT patterns.
readerRules :: [Rule]
readerRules =
  [ askRedundant
  , asksSimplify
  , localPattern
  , readerToFunction
  , runReaderTwice
  , askReturn
  , readerMapEnv
  ]

-- | ask >>= return is just ask.
--
-- @
-- ask >>= return  ==>  ask
-- @
askRedundant :: Rule
askRedundant =
  rule "ask-redundant" $
    match ("ask >>= return" ==> "ask")
    & category Style
    & severity Suggestion
    & message "ask >>= return is just ask"

-- | asks simplification.
--
-- @
-- ask >>= \r -> return (f r)  ==>  asks f
-- @
asksSimplify :: Rule
asksSimplify =
  rule "asks-simplify" $
    match ("fmap _f ask" ==> "asks _f")
    & category Style
    & severity Suggestion
    & message "Use asks for function application on environment"

-- | local pattern for temporary environment.
--
-- @
-- local f action  -- Runs action with modified environment
-- @
localPattern :: Rule
localPattern =
  rule "local-pattern" $
    match ("local _f _action" ==> "local _f _action")
    & category Style
    & severity Info
    & message "Using local to temporarily modify environment"
    & safetyLevel ManualReview

-- | Reader is just function.
--
-- @
-- Reader f  -- Same as function application
-- @
readerToFunction :: Rule
readerToFunction =
  rule "reader-to-function" $
    match ("Reader _f" ==> "_f")
    & category Style
    & severity Info
    & message "Reader is essentially a function - consider if needed"
    & safetyLevel ManualReview

-- | runReader twice is redundant.
--
-- @
-- runReader (runReader m r1) r2  -- Redundant nesting
-- @
runReaderTwice :: Rule
runReaderTwice =
  rule "runReader-twice" $
    matchText "runReader.*runReader"
    & category Style
    & severity Warning
    & message "Nested runReader - consider restructuring"

-- | ask followed by return.
--
-- @
-- do { r <- ask; return r }  ==>  ask
-- @
askReturn :: Rule
askReturn =
  rule "ask-return" $
    matchText "do.*<- ask.*return"
    & category Style
    & severity Suggestion
    & message "Simplify ask binding followed by return"

-- | Reader map environment.
--
-- @
-- withReaderT f action  -- Transform environment
-- @
readerMapEnv :: Rule
readerMapEnv =
  rule "reader-map-env" $
    match ("withReaderT _f _action" ==> "withReaderT _f _action")
    & category Style
    & severity Info
    & message "withReaderT transforms the environment type"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- State Rules
--------------------------------------------------------------------------------

-- | Rules for StateT patterns.
stateRules :: [Rule]
stateRules =
  [ getReturn
  , putGet
  , modifyGet
  , stateGetPut
  , getModify
  , execStateRedundant
  , evalStateRedundant
  ]

-- | get >>= return is get.
--
-- @
-- get >>= return  ==>  get
-- @
getReturn :: Rule
getReturn =
  rule "get-return" $
    match ("get >>= return" ==> "get")
    & category Style
    & severity Suggestion
    & message "get >>= return is just get"

-- | put then get is return.
--
-- @
-- put s >> get  ==>  put s >> return s
-- @
putGet :: Rule
putGet =
  rule "put-get" $
    match ("put _s >> get" ==> "put _s >> return _s")
    & category Style
    & severity Suggestion
    & message "put s >> get returns the same s"

-- | modify then get.
--
-- @
-- modify f >> get  -- Gets modified state
-- @
modifyGet :: Rule
modifyGet =
  rule "modify-get" $
    match ("modify _f >> get" ==> "modify _f >> get")
    & category Style
    & severity Info
    & message "modify then get - consider gets f or state"
    & safetyLevel ManualReview

-- | state is get then put.
--
-- @
-- do { s <- get; put (f s); return (g s) }  ==>  state (\s -> (g s, f s))
-- @
stateGetPut :: Rule
stateGetPut =
  rule "state-get-put" $
    matchText "do.*<- get.*put"
    & category Style
    & severity Suggestion
    & message "Consider using state combinator"

-- | get then modify.
--
-- @
-- do { s <- get; modify f }  -- Can use gets
-- @
getModify :: Rule
getModify =
  rule "get-modify" $
    matchText "do.*<- get.*modify"
    & category Style
    & severity Info
    & message "get then modify - ensure state is used"
    & safetyLevel ManualReview

-- | execState ignores result.
--
-- @
-- execState action s  -- Only returns final state
-- @
execStateRedundant :: Rule
execStateRedundant =
  rule "execState-redundant" $
    match ("execState _action _s" ==> "execState _action _s")
    & category Style
    & severity Info
    & message "execState ignores action result - ensure this is intended"
    & safetyLevel ManualReview

-- | evalState ignores state.
--
-- @
-- evalState action s  -- Only returns result, drops state
-- @
evalStateRedundant :: Rule
evalStateRedundant =
  rule "evalState-redundant" $
    match ("evalState _action _s" ==> "evalState _action _s")
    & category Style
    & severity Info
    & message "evalState drops final state - ensure this is intended"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Writer Rules
--------------------------------------------------------------------------------

-- | Rules for WriterT patterns.
writerRules :: [Rule]
writerRules =
  [ tellMempty
  , tellMconcat
  , writerLazy
  , writerStrict
  , listenIgnore
  , passSimplify
  , censorPattern
  ]

-- | tell mempty is no-op.
--
-- @
-- tell mempty  ==>  return ()
-- @
tellMempty :: Rule
tellMempty =
  rule "tell-mempty" $
    match ("tell mempty" ==> "pure ()")
    & category Style
    & severity Suggestion
    & message "tell mempty is a no-op"

-- | Multiple tells to mconcat.
--
-- @
-- tell a >> tell b  ==>  tell (a <> b)
-- @
tellMconcat :: Rule
tellMconcat =
  rule "tell-mconcat" $
    match ("tell _a >> tell _b" ==> "tell (_a <> _b)")
    & category Style
    & severity Suggestion
    & message "Combine multiple tells with (<>)"

-- | Lazy WriterT space leak.
--
-- @
-- WriterT  -- Lazy by default, can leak space
-- @
writerLazy :: Rule
writerLazy =
  rule "writer-lazy" $
    matchText "import.*Control.Monad.Writer[^.S]"
    & category Performance
    & severity Warning
    & message "Lazy WriterT can leak space"
    & note "Use Control.Monad.Writer.Strict instead"

-- | Strict WriterT.
--
-- @
-- import Control.Monad.Writer.Strict
-- @
writerStrict :: Rule
writerStrict =
  rule "writer-strict" $
    matchText "Control.Monad.Writer.Strict"
    & category Style
    & severity Info
    & message "Using strict WriterT - good for avoiding space leaks"
    & safetyLevel ManualReview

-- | listen ignoring output.
--
-- @
-- fst <$> listen action  ==>  action
-- @
listenIgnore :: Rule
listenIgnore =
  rule "listen-ignore" $
    match ("fst <$> listen _action" ==> "_action")
    & category Style
    & severity Suggestion
    & message "fst <$> listen discards the log - just use action"

-- | pass simplification.
--
-- @
-- pass (action >>= \a -> return (a, id))  ==>  action
-- @
passSimplify :: Rule
passSimplify =
  rule "pass-simplify" $
    matchText "pass.*return.*,.*id"
    & category Style
    & severity Suggestion
    & message "pass with id function can be simplified"

-- | censor pattern.
--
-- @
-- censor f action  -- Modifies log output
-- @
censorPattern :: Rule
censorPattern =
  rule "censor-pattern" $
    match ("censor _f _action" ==> "censor _f _action")
    & category Style
    & severity Info
    & message "censor transforms the log output"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Except Rules
--------------------------------------------------------------------------------

-- | Rules for ExceptT patterns.
exceptRules :: [Rule]
exceptRules =
  [ throwECatch
  , catchEThrow
  , runExceptTPattern
  , eitherToExceptT
  , exceptTToEither
  , tryPattern
  , exceptTLift
  ]

-- | throwError then catchError.
--
-- @
-- catchError (throwError e) f  ==>  f e
-- @
throwECatch :: Rule
throwECatch =
  rule "throw-catch" $
    match ("catchError (throwError _e) _f" ==> "_f _e")
    & category Style
    & severity Suggestion
    & message "catchError immediately after throwError - just apply handler"

-- | catchError then throwError.
--
-- @
-- catchError action throwError  ==>  action
-- @
catchEThrow :: Rule
catchEThrow =
  rule "catch-throw" $
    match ("catchError _action throwError" ==> "_action")
    & category Style
    & severity Suggestion
    & message "catchError with throwError as handler is identity"

-- | runExceptT pattern.
--
-- @
-- runExceptT action  -- Returns Either
-- @
runExceptTPattern :: Rule
runExceptTPattern =
  rule "runExceptT-pattern" $
    match ("runExceptT _action" ==> "runExceptT _action")
    & category Style
    & severity Info
    & message "runExceptT unwraps to Either - handle both cases"
    & safetyLevel ManualReview

-- | Either to ExceptT.
--
-- @
-- ExceptT (return either)  ==>  except either
-- @
eitherToExceptT :: Rule
eitherToExceptT =
  rule "either-to-exceptT" $
    match ("ExceptT (return _either)" ==> "except _either")
    & category Style
    & severity Suggestion
    & message "Use except to lift Either into ExceptT"

-- | ExceptT to Either.
--
-- @
-- runExceptT (except either)  ==>  return either
-- @
exceptTToEither :: Rule
exceptTToEither =
  rule "exceptT-to-either" $
    match ("runExceptT (except _either)" ==> "return _either")
    & category Style
    & severity Suggestion
    & message "runExceptT . except is identity in the monad"

-- | try pattern.
--
-- @
-- try action  -- Catches exceptions as Left
-- @
tryPattern :: Rule
tryPattern =
  rule "try-pattern" $
    match ("try _action" ==> "try _action")
    & category Style
    & severity Info
    & message "try catches exceptions and returns Either"
    & safetyLevel ManualReview

-- | lift in ExceptT.
--
-- @
-- lift action  -- Lifts successful action into ExceptT
-- @
exceptTLift :: Rule
exceptTLift =
  rule "exceptT-lift" $
    matchText "lift.*ExceptT"
    & category Style
    & severity Info
    & message "lift in ExceptT always succeeds"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Lift Rules
--------------------------------------------------------------------------------

-- | Rules for lifting operations.
liftRules :: [Rule]
liftRules =
  [ liftReturn
  , liftBind
  , liftLift
  , liftIO
  , hoistPattern
  , mapReaderT
  ]

-- | lift . return is return.
--
-- @
-- lift (return x)  ==>  return x
-- @
liftReturn :: Rule
liftReturn =
  rule "lift-return" $
    match ("lift (return _x)" ==> "return _x")
    & category Style
    & severity Suggestion
    & message "lift . return is just return"

-- | lift bind pattern.
--
-- @
-- lift (m >>= f)  ==>  lift m >>= (lift . f)
-- @
liftBind :: Rule
liftBind =
  rule "lift-bind" $
    match ("lift (_m >>= _f)" ==> "lift _m >>= (lift . _f)")
    & category Style
    & severity Info
    & message "lift distributes over (>>=)"
    & safetyLevel ManualReview

-- | Double lift.
--
-- @
-- lift (lift x)  -- Lifting through two layers
-- @
liftLift :: Rule
liftLift =
  rule "lift-lift" $
    match ("lift (lift _x)" ==> "lift (lift _x)")
    & category Style
    & severity Info
    & message "Double lift - consider if transformer stack is appropriate"
    & safetyLevel ManualReview

-- | liftIO from MonadIO.
--
-- @
-- liftIO action  -- Lift IO to MonadIO m
-- @
liftIO :: Rule
liftIO =
  rule "liftIO-pattern" $
    match ("liftIO _action" ==> "liftIO _action")
    & category Style
    & severity Info
    & message "Using liftIO to lift IO action"
    & safetyLevel ManualReview

-- | hoist pattern.
--
-- @
-- hoist f  -- Change base monad
-- @
hoistPattern :: Rule
hoistPattern =
  rule "hoist-pattern" $
    match ("hoist _f _action" ==> "hoist _f _action")
    & category Style
    & severity Info
    & message "hoist changes the base monad of a transformer"
    & safetyLevel ManualReview

-- | mapReaderT pattern.
--
-- @
-- mapReaderT f action  -- Transform inner monad
-- @
mapReaderT :: Rule
mapReaderT =
  rule "mapReaderT-pattern" $
    match ("mapReaderT _f _action" ==> "mapReaderT _f _action")
    & category Style
    & severity Info
    & message "mapReaderT transforms the inner computation"
    & safetyLevel ManualReview
