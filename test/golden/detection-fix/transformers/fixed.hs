{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ImportQualifiedPost #-}

module TransformerPatterns where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except (except)
import Data.Functor.Identity

--------------------------------------------------------------------------------
-- Reader Patterns
--------------------------------------------------------------------------------

-- ask >>= return is just ask
askRedundantPattern :: ReaderT env m env
askRedundantPattern = ask >>= return

-- fmap f ask should be asks f
asksPatternFmap :: Functor m => (env -> b) -> ReaderT env m b
asksPatternFmap f = fmap f ask

-- ask in do-notation returning immediately
askReturnDo :: ReaderT env m env
askReturnDo = do
  r <- ask
return r

-- Nested runReader (anti-pattern)
nestedRunReader :: Reader Int (Reader Int Int) -> Int -> Int -> Int
nestedRunReader computation env1 env2 = runReader (runReader computation env1) env2

-- Reader is essentially a function
unnecessaryReader :: Reader Int String
unnecessaryReader = reader show

--------------------------------------------------------------------------------
-- State Patterns
--------------------------------------------------------------------------------

-- get >>= return is just get
getRedundantPattern :: StateT s m s
getRedundantPattern = get >>= return

-- put then get returns the same value
putGetPattern :: StateT Int m Int
putGetPattern = do
  put 42
  get

-- get then put with modification
getModifyPattern :: StateT Int m ()
getModifyPattern = do
  s <- get
  modify (+1)

-- modify then get
modifyGetPattern :: StateT Int m Int
modifyGetPattern = do
  modify (*2)
  get

-- get, put, return - should use state combinator
stateGetPutReturn :: StateT Int m String
stateGetPutReturn = do
  s <- get
  put (s + 1)
return (show s)

--------------------------------------------------------------------------------
-- Writer Patterns
--------------------------------------------------------------------------------

-- tell mempty is no-op
tellMemptyPattern :: WriterT [String] m ()
tellMemptyPattern = tell mempty

-- Multiple tells should be combined
multipleTells :: WriterT [String] m ()
multipleTells = do
  tell ["first"]
  tell ["second"]

-- listen and discard log
listenIgnorePattern :: WriterT [String] m Int
listenIgnorePattern = fst <$> listen (return 42)

-- pass with id function
passIdentityPattern :: WriterT [String] m Int
passIdentityPattern = pass (return (42, id))

--------------------------------------------------------------------------------
-- Except Patterns
--------------------------------------------------------------------------------

-- throwError then catchError
throwCatchPattern :: ExceptT String m Int
throwCatchPattern = catchError (throwError "error") handler
where handler e = pure 0

-- catchError with throwError as handler (identity)
catchThrowIdentity :: ExceptT String m Int
catchThrowIdentity = catchError computation throwError
where computation = pure 42

-- ExceptT (return either) should use except
exceptTReturnPattern :: Monad m => Either String Int -> ExceptT String m Int
exceptTReturnPattern either = ExceptT (return either)

-- runExceptT . except is identity in monad
runExceptExceptPattern :: Monad m => Either String Int -> m (Either String Int)
runExceptExceptPattern either = runExceptT (except either)

--------------------------------------------------------------------------------
-- Lift Patterns
--------------------------------------------------------------------------------

-- lift . return is return
liftReturnPattern :: (MonadTrans t, Monad m, Monad (t m)) => Int -> t m Int
liftReturnPattern x = lift (return x)

-- Double lift pattern
doubleLiftPattern :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m), Monad (t1 (t2 m)))
                  => m Int -> t1 (t2 m) Int
doubleLiftPattern x = lift (lift x)

-- Triple lift pattern (even worse)
tripleLiftPattern :: (MonadTrans t1, MonadTrans t2, MonadTrans t3,
                      Monad m, Monad (t3 m), Monad (t2 (t3 m)), Monad (t1 (t2 (t3 m))))
                  => m Int -> t1 (t2 (t3 m)) Int
tripleLiftPattern x = lift (lift (lift x))

-- lift bind distribution
liftBindPattern :: (MonadTrans t, Monad m, Monad (t m)) => m a -> (a -> m b) -> t m b
liftBindPattern m f = lift (m >>= f)

--------------------------------------------------------------------------------
-- Complex Realistic Examples
--------------------------------------------------------------------------------

-- Reader with redundant ask
computeWithEnv :: Monad m => ReaderT Config m Result
computeWithEnv = do
  config <- ask
return (process config)
  where
    process = id

-- State with get/put cycle
updateCounter :: StateT Counter m Counter
updateCounter = do
  counter <- get
  put (counter + 1)
return counter

-- Writer with multiple tells in sequence
logMultipleSteps :: WriterT [String] m ()
logMultipleSteps = do
  tell ["Step 1"]
  tell ["Step 2"]
  tell ["Step 3"]

-- ExceptT with immediate catch
validateInput :: ExceptT ValidationError m Input
validateInput = catchError (throwError invalidInput) recoverFromError
where recoverFromError _ = pure defaultInput

-- Nested transformer with lifts
nestedTransformerStack :: ReaderT Config (StateT AppState (ExceptT Error IO)) ()
nestedTransformerStack = do
  config <- ask
  lift (lift (lift (putStrLn "Deep in the stack")))
  state <- lift get
return ()

-- Redundant transformer unwrap/wrap
redundantUnwrapWrap :: Monad m => Either String Int -> m (Either String Int)
redundantUnwrapWrap value = runExceptT (except value)

-- State combinator candidates
statePattern1 :: StateT Int m String
statePattern1 = do
  x <- get
  put (x * 2)
return (show x)

statePattern2 :: StateT Int m Int
statePattern2 = do
  s <- get
  put (s + 10)
return (s + 5)

--------------------------------------------------------------------------------
-- Type aliases for clarity
--------------------------------------------------------------------------------

type Config = Int
type Result = Int
type Counter = Int
type Input = String
type ValidationError = String
type AppState = Int
type Error = String

defaultInput :: Input
defaultInput = ""

invalidInput :: ValidationError
invalidInput = "invalid"