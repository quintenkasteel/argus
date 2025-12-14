{-# LANGUAGE OverloadedStrings #-}

module AsyncExamples where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception

--------------------------------------------------------------------------------
-- Prefer async over forkIO
--------------------------------------------------------------------------------

-- Should warn: use async instead of forkIO for better exception handling
badForkIO :: IO () -> IO ThreadId
badForkIO action = forkIO action

-- Should warn: forkIO swallows exceptions
forkIOWithComputation :: IO ()
forkIOWithComputation = do
  _ <- forkIO $ putStrLn "background task"
  pure ()

-- Good: using async
goodAsync :: IO () -> IO (Async ())
goodAsync action = async action

--------------------------------------------------------------------------------
-- Use withAsync for resource safety
--------------------------------------------------------------------------------

-- Should warn: prefer withAsync for automatic cancellation
manualAsyncCleanup :: IO () -> IO ()
manualAsyncCleanup action = do
  a <- async action
  result <- wait a
  pure result

-- Good: withAsync handles cleanup
goodWithAsync :: IO () -> IO ()
goodWithAsync action = withAsync action wait

--------------------------------------------------------------------------------
-- Prefer race over manual patterns
--------------------------------------------------------------------------------

-- Should warn: pattern could use race
manualRacePattern :: IO a -> IO b -> IO (Either a b)
manualRacePattern action1 action2 = do
  a1 <- async action1
  a2 <- async action2
  result <- waitEither a1 a2
  case result of
    Left x -> cancel a2 >> pure (Left x)
    Right y -> cancel a1 >> pure (Right y)

-- Good: using race
goodRace :: IO a -> IO b -> IO (Either a b)
goodRace = race

--------------------------------------------------------------------------------
-- Use concurrently for parallel operations
--------------------------------------------------------------------------------

-- Should warn: pattern could use concurrently
manualConcurrently :: IO a -> IO b -> IO (a, b)
manualConcurrently action1 action2 = do
  a1 <- async action1
  a2 <- async action2
  r1 <- wait a1
  r2 <- wait a2
  pure (r1, r2)

-- Good: using concurrently
goodConcurrently :: IO a -> IO b -> IO (a, b)
goodConcurrently = concurrently

--------------------------------------------------------------------------------
-- Async exception safety
--------------------------------------------------------------------------------

-- Should warn: missing mask for exception safety
unsafeResourceAllocation :: IO () -> IO ()
unsafeResourceAllocation cleanup = do
  resource <- allocateResource
  result <- useResource resource
  cleanup
  pure result
  where
    allocateResource = pure ()
    useResource _ = pure ()

-- Should warn: missing bracket for cleanup
missingBracket :: IO ()
missingBracket = do
  handle <- openResource
  result <- processResource handle
  closeResource handle
  pure result
  where
    openResource = pure ()
    processResource _ = pure ()
    closeResource _ = pure ()

-- Good: using bracket
goodBracket :: IO ()
goodBracket = bracket openResource closeResource processResource
  where
    openResource = pure ()
    closeResource _ = pure ()
    processResource _ = pure ()

--------------------------------------------------------------------------------
-- STM and async interactions
--------------------------------------------------------------------------------

-- Should warn: using MVar when STM would be better
mvarConcurrentAccess :: MVar Int -> IO Int
mvarConcurrentAccess var = do
  val <- takeMVar var
  let newVal = val + 1
  putMVar var newVal
  pure newVal

--------------------------------------------------------------------------------
-- Cancellation patterns
--------------------------------------------------------------------------------

-- Should warn: cancel without waiting can cause issues
cancelWithoutWait :: Async a -> IO ()
cancelWithoutWait a = cancel a

-- Good: using cancel with proper handling
goodCancel :: Async a -> IO ()
goodCancel a = do
  cancel a
  _ <- waitCatch a
  pure ()

--------------------------------------------------------------------------------
-- Timeout patterns
--------------------------------------------------------------------------------

-- Should warn: timeout returning Maybe is often better than throwing
timeoutWithException :: Int -> IO a -> IO a
timeoutWithException micros action = do
  result <- timeout micros action
  case result of
    Just x -> pure x
    Nothing -> error "timeout"

-- Good: returning Maybe
goodTimeout :: Int -> IO a -> IO (Maybe a)
goodTimeout = timeout
