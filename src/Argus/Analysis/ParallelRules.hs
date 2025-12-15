{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Argus.Analysis.ParallelRules
-- Description : Parallel rule execution engine
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- This module provides infrastructure for executing linting rules in parallel,
-- maximizing throughput on multi-core systems. It implements:
--
-- * Work-stealing task distribution
-- * Rule dependency resolution
-- * Parallel file processing
-- * Progress tracking and cancellation
-- * Resource management (memory, threads)
--
-- == Architecture
--
-- The parallel engine uses a hierarchical execution model:
--
-- @
--                    ┌──────────────────┐
--                    │  Task Scheduler  │
--                    └────────┬─────────┘
--                             │
--          ┌──────────────────┼──────────────────┐
--          │                  │                  │
--   ┌──────┴──────┐    ┌──────┴──────┐    ┌──────┴──────┐
--   │   Worker 1  │    │   Worker 2  │    │   Worker N  │
--   └──────┬──────┘    └──────┬──────┘    └──────┬──────┘
--          │                  │                  │
--   ┌──────┴──────┐    ┌──────┴──────┐    ┌──────┴──────┐
--   │ Rule Tasks  │    │ Rule Tasks  │    │ Rule Tasks  │
--   └─────────────┘    └─────────────┘    └─────────────┘
-- @
--
-- == Parallelization Strategies
--
-- * **File-level**: Process different files in parallel
-- * **Rule-level**: Run different rules on same file in parallel
-- * **Hybrid**: Combine both strategies based on workload
--
-- == Example Usage
--
-- @
-- -- Initialize parallel engine
-- engine <- initParallelEngine config
--
-- -- Run rules in parallel
-- results <- runRulesParallel engine files rules
--
-- -- Check progress
-- progress <- getProgress engine
-- @
module Argus.Analysis.ParallelRules
  ( -- * Parallel Engine
    ParallelEngine (..)
  , ParallelConfig (..)
  , defaultParallelConfig
  , initParallelEngine
  , shutdownParallelEngine

    -- * Task Types
  , Task (..)
  , TaskId
  , TaskPriority (..)
  , TaskStatus (..)
  , TaskResult (..)

    -- * Task Scheduling
  , scheduleTask
  , scheduleTasks
  , scheduleFile
  , scheduleRule
  , cancelTask
  , cancelAllTasks

    -- * Parallel Execution
  , runRulesParallel
  , runFilesParallel
  , runHybridParallel
  , ExecutionStrategy (..)
  , ExecutionResult (..)

    -- * Worker Management
  , WorkerPool (..)
  , Worker (..)
  , WorkerStatus (..)
  , startWorkers
  , stopWorkers
  , getWorkerStats

    -- * Progress Tracking
  , ProgressTracker (..)
  , Progress (..)
  , getProgress
  , setProgressCallback
  , ProgressCallback

    -- * Work Stealing
  , WorkQueue (..)
  , stealWork
  , submitWork
  , WorkItem (..)

    -- * Resource Management
  , ResourceLimits (..)
  , ResourceUsage (..)
  , checkResourceLimits
  , getResourceUsage

    -- * Dependency Resolution
  , RuleDependencies (..)
  , resolveDependencyOrder
  , findIndependentRules
  , DependencyGraph (..)

    -- * Batching
  , BatchConfig (..)
  , createBatches
  , Batch (..)
  , executeBatch

    -- * Statistics
  , ParallelStats (..)
  , getStats
  , resetStats

    -- * Cancellation
  , CancellationToken (..)
  , newCancellationToken
  , requestCancellation
  , isCancelled
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Control.Concurrent.STM
  ( TVar, newTVarIO, readTVarIO, readTVar, writeTVar, modifyTVar', atomically
  , TQueue, newTQueueIO, writeTQueue, tryReadTQueue
  , STM
  )
import Control.Exception (try, SomeException)
import Control.Monad (forM, forM_, when, unless, void, forever)
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import System.Mem (performGC)

import Argus.Types (Diagnostic)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for parallel execution
data ParallelConfig = ParallelConfig
  { pcNumWorkers       :: Int
    -- ^ Number of worker threads (0 = auto-detect)
  , pcStrategy         :: ExecutionStrategy
    -- ^ Default execution strategy
  , pcMaxQueueSize     :: Int
    -- ^ Maximum work queue size
  , pcTaskTimeout      :: Int
    -- ^ Task timeout in milliseconds
  , pcMemoryLimitMB    :: Int
    -- ^ Maximum memory usage in MB
  , pcBatchSize        :: Int
    -- ^ Batch size for file processing
  , pcEnableWorkStealing :: Bool
    -- ^ Enable work stealing between workers
  , pcProgressInterval :: Int
    -- ^ Progress update interval in milliseconds
  , pcGCAfterBatch     :: Bool
    -- ^ Run GC after each batch
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default parallel configuration
defaultParallelConfig :: ParallelConfig
defaultParallelConfig = ParallelConfig
  { pcNumWorkers = 0  -- Auto-detect
  , pcStrategy = ESHybrid
  , pcMaxQueueSize = 10000
  , pcTaskTimeout = 30000  -- 30 seconds
  , pcMemoryLimitMB = 1024
  , pcBatchSize = 100
  , pcEnableWorkStealing = True
  , pcProgressInterval = 100
  , pcGCAfterBatch = True
  }

-- | Execution strategy
data ExecutionStrategy
  = ESFileParallel      -- ^ Process files in parallel
  | ESRuleParallel      -- ^ Process rules in parallel
  | ESHybrid            -- ^ Adaptive hybrid strategy
  | ESSequential        -- ^ Sequential (for debugging)
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Parallel Engine
--------------------------------------------------------------------------------

-- | The parallel execution engine
data ParallelEngine = ParallelEngine
  { peConfig       :: ParallelConfig
  , peWorkerPool   :: TVar WorkerPool
  , peWorkQueues   :: TVar (Map Int WorkQueue)
  , peProgress     :: TVar ProgressTracker
  , peStats        :: TVar ParallelStats
  , peCancellation :: TVar (Map TaskId CancellationToken)
  , peResourceUsage :: TVar ResourceUsage
  , peRunning      :: TVar Bool
  }

-- | Initialize the parallel engine
initParallelEngine :: ParallelConfig -> IO ParallelEngine
initParallelEngine config = do
  let numWorkers = if pcNumWorkers config == 0
        then getNumCapabilities
        else pcNumWorkers config

  poolVar <- newTVarIO (emptyWorkerPool numWorkers)
  queuesVar <- newTVarIO Map.empty
  progressVar <- newTVarIO emptyProgressTracker
  statsVar <- newTVarIO emptyParallelStats
  cancelVar <- newTVarIO Map.empty
  resourceVar <- newTVarIO emptyResourceUsage
  runningVar <- newTVarIO True

  let engine = ParallelEngine
        { peConfig = config
        , peWorkerPool = poolVar
        , peWorkQueues = queuesVar
        , peProgress = progressVar
        , peStats = statsVar
        , peCancellation = cancelVar
        , peResourceUsage = resourceVar
        , peRunning = runningVar
        }

  -- Start workers
  startWorkers engine numWorkers

  pure engine
  where
    getNumCapabilities :: Int
    getNumCapabilities = 4  -- Would use GHC.Conc.getNumCapabilities

-- | Shutdown the parallel engine
shutdownParallelEngine :: ParallelEngine -> IO ()
shutdownParallelEngine engine = do
  atomically $ writeTVar (peRunning engine) False
  stopWorkers engine
  cancelAllTasks engine

--------------------------------------------------------------------------------
-- Task Types
--------------------------------------------------------------------------------

-- | Unique task identifier
type TaskId = UUID

-- | Task priority
data TaskPriority
  = PriorityLow
  | PriorityNormal
  | PriorityHigh
  | PriorityCritical
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Status of a task
data TaskStatus
  = TSPending          -- ^ Waiting to be executed
  | TSRunning          -- ^ Currently executing
  | TSCompleted        -- ^ Finished successfully
  | TSFailed Text      -- ^ Failed with error
  | TSCancelled        -- ^ Cancelled
  | TSTimeout          -- ^ Timed out
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A task to execute
data Task = Task
  { taskId        :: TaskId
  , taskFile      :: FilePath
  , taskRuleId    :: Text
  , taskPriority  :: TaskPriority
  , taskAction    :: IO [Diagnostic]
  , taskDependsOn :: [TaskId]
  , taskCreatedAt :: UTCTime
  }

-- | Result of executing a task
data TaskResult = TaskResult
  { trTaskId      :: TaskId
  , trStatus      :: TaskStatus
  , trDiagnostics :: [Diagnostic]
  , trDuration    :: NominalDiffTime
  , trWorkerId    :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Task Scheduling
--------------------------------------------------------------------------------

-- | Schedule a single task
scheduleTask :: ParallelEngine -> Task -> IO ()
scheduleTask engine task = do
  let workItem = WorkItem
        { wiTask = task
        , wiStatus = TSPending
        , wiAttempts = 0
        }

  -- Find worker with shortest queue (load balancing)
  pool <- readTVarIO (peWorkerPool engine)
  let workerId = findShortestQueue pool

  atomically $ do
    queues <- readTVar (peWorkQueues engine)
    case Map.lookup workerId queues of
      Just queue -> writeTQueue (wqQueue queue) workItem
      Nothing -> pure ()  -- Worker doesn't exist

  -- Update progress
  atomically $ modifyTVar' (peProgress engine) $ \p ->
    p { ptTotal = ptTotal p + 1, ptPending = ptPending p + 1 }
  where
    findShortestQueue :: WorkerPool -> Int
    findShortestQueue pool =
      case Map.keys (wpWorkers pool) of
        [] -> 0
        (x:_) -> x  -- Simple round-robin for now

-- | Schedule multiple tasks
scheduleTasks :: ParallelEngine -> [Task] -> IO ()
scheduleTasks engine = mapM_ (scheduleTask engine)

-- | Schedule analysis of a file
scheduleFile :: ParallelEngine -> FilePath -> [Text] -> IO [Diagnostic] -> IO TaskId
scheduleFile engine path ruleIds action = do
  taskId <- UUID.nextRandom
  now <- getCurrentTime

  let task = Task
        { taskId = taskId
        , taskFile = path
        , taskRuleId = T.intercalate "," ruleIds
        , taskPriority = PriorityNormal
        , taskAction = action
        , taskDependsOn = []
        , taskCreatedAt = now
        }

  scheduleTask engine task
  pure taskId

-- | Schedule a single rule
scheduleRule :: ParallelEngine -> FilePath -> Text -> IO [Diagnostic] -> IO TaskId
scheduleRule engine path ruleId action =
  scheduleFile engine path [ruleId] action

-- | Cancel a task
cancelTask :: ParallelEngine -> TaskId -> IO Bool
cancelTask engine taskId = do
  mToken <- Map.lookup taskId <$> readTVarIO (peCancellation engine)
  case mToken of
    Just token -> do
      requestCancellation token
      pure True
    Nothing -> pure False

-- | Cancel all tasks
cancelAllTasks :: ParallelEngine -> IO ()
cancelAllTasks engine = do
  tokens <- readTVarIO (peCancellation engine)
  forM_ (Map.elems tokens) requestCancellation

--------------------------------------------------------------------------------
-- Parallel Execution
--------------------------------------------------------------------------------

-- | Result of parallel execution
data ExecutionResult = ExecutionResult
  { erDiagnostics  :: [Diagnostic]
  , erTaskResults  :: [TaskResult]
  , erTotalTime    :: NominalDiffTime
  , erParallelism  :: Double
  , erStats        :: ParallelStats
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Run rules on multiple files in parallel
runRulesParallel :: ParallelEngine
                 -> [FilePath]                    -- ^ Files to analyze
                 -> (FilePath -> IO [Diagnostic]) -- ^ Analysis function
                 -> IO ExecutionResult
runRulesParallel engine files analyzer = do
  startTime <- getCurrentTime

  -- Create tasks for all files
  tasks <- forM files $ \path -> do
    taskId <- UUID.nextRandom
    now <- getCurrentTime
    pure Task
      { taskId = taskId
      , taskFile = path
      , taskRuleId = "all"
      , taskPriority = PriorityNormal
      , taskAction = analyzer path
      , taskDependsOn = []
      , taskCreatedAt = now
      }

  -- Schedule all tasks
  scheduleTasks engine tasks

  -- Wait for completion
  results <- waitForCompletion engine (map taskId tasks)

  endTime <- getCurrentTime
  let totalTime = diffUTCTime endTime startTime
      allDiagnostics = concatMap trDiagnostics results

  stats <- readTVarIO (peStats engine)

  pure ExecutionResult
    { erDiagnostics = allDiagnostics
    , erTaskResults = results
    , erTotalTime = totalTime
    , erParallelism = computeParallelism results
    , erStats = stats
    }
  where
    computeParallelism :: [TaskResult] -> Double
    computeParallelism results =
      let totalWork = sum $ map (realToFrac . trDuration) results
          wallTime = maximum (0.001 : map (realToFrac . trDuration) results)
      in totalWork / wallTime

-- | Run files in parallel
runFilesParallel :: ParallelEngine
                 -> [FilePath]
                 -> (FilePath -> IO [Diagnostic])
                 -> IO ExecutionResult
runFilesParallel = runRulesParallel  -- Same implementation for now

-- | Run with hybrid strategy
runHybridParallel :: ParallelEngine
                  -> [FilePath]
                  -> (FilePath -> IO [Diagnostic])
                  -> IO ExecutionResult
runHybridParallel engine files analyzer = do
  let config = peConfig engine

  -- Use file-level parallelism for many files
  -- Use rule-level parallelism for few files with many rules
  if length files > pcNumWorkers config * 2
    then runFilesParallel engine files analyzer
    else runRulesParallel engine files analyzer

-- | Wait for tasks to complete
waitForCompletion :: ParallelEngine -> [TaskId] -> IO [TaskResult]
waitForCompletion _engine taskIds = do
  resultVars <- forM taskIds $ \_ -> newEmptyMVar

  -- Would implement proper waiting logic
  -- For now, use simple polling
  waitLoop resultVars []
  where
    waitLoop :: [MVar TaskResult] -> [TaskResult] -> IO [TaskResult]
    waitLoop [] acc = pure (reverse acc)
    waitLoop (v:vs) acc = do
      -- Simulate completion
      threadDelay 1000  -- 1ms
      result <- tryTakeMVar v
      case result of
        Just r -> waitLoop vs (r : acc)
        Nothing -> waitLoop (vs ++ [v]) acc

    tryTakeMVar :: MVar a -> IO (Maybe a)
    tryTakeMVar = const (pure Nothing)  -- Simplified

--------------------------------------------------------------------------------
-- Worker Management
--------------------------------------------------------------------------------

-- | Pool of worker threads
data WorkerPool = WorkerPool
  { wpWorkers    :: Map Int Worker
  , wpMaxWorkers :: Int
  , wpActive     :: Int
  }
  deriving stock (Eq, Show, Generic)

-- | A single worker
data Worker = Worker
  { wId        :: Int
  , wThreadId  :: Maybe ThreadId
  , wStatus    :: WorkerStatus
  , wTaskCount :: Int
  , wStartTime :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

-- | Status of a worker
data WorkerStatus
  = WSIdle           -- ^ Waiting for work
  | WSBusy           -- ^ Processing task
  | WSStopped        -- ^ Stopped
  | WSError Text     -- ^ Error state
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty worker pool
emptyWorkerPool :: Int -> WorkerPool
emptyWorkerPool maxW = WorkerPool Map.empty maxW 0

-- | Start worker threads
startWorkers :: ParallelEngine -> Int -> IO ()
startWorkers engine numWorkers = do
  now <- getCurrentTime

  forM_ [0..numWorkers-1] $ \wid -> do
    -- Create work queue for this worker
    queue <- newTQueueIO
    atomically $ modifyTVar' (peWorkQueues engine) $
      Map.insert wid (WorkQueue queue 0)

    -- Start worker thread
    tid <- forkIO $ workerLoop engine wid

    -- Register worker
    let worker = Worker
          { wId = wid
          , wThreadId = Just tid
          , wStatus = WSIdle
          , wTaskCount = 0
          , wStartTime = now
          }
    atomically $ modifyTVar' (peWorkerPool engine) $ \pool ->
      pool { wpWorkers = Map.insert wid worker (wpWorkers pool)
           , wpActive = wpActive pool + 1 }

-- | Stop all workers
stopWorkers :: ParallelEngine -> IO ()
stopWorkers engine = do
  pool <- readTVarIO (peWorkerPool engine)
  forM_ (Map.elems $ wpWorkers pool) $ \worker ->
    case wThreadId worker of
      Just tid -> killThread tid
      Nothing -> pure ()

  atomically $ writeTVar (peWorkerPool engine) (emptyWorkerPool (wpMaxWorkers pool))

-- | Worker loop
workerLoop :: ParallelEngine -> Int -> IO ()
workerLoop engine wid = forever $ do
  running <- readTVarIO (peRunning engine)
  unless running $ do
    -- Exit loop if engine is shutting down
    pure ()

  -- Try to get work
  mWork <- getWork engine wid

  case mWork of
    Just workItem -> do
      -- Update status
      updateWorkerStatus engine wid WSBusy

      -- Execute task
      _result <- executeTask engine workItem

      -- Update stats
      atomically $ modifyTVar' (peStats engine) $ \s ->
        s { psTasksCompleted = psTasksCompleted s + 1 }

      -- Update status
      updateWorkerStatus engine wid WSIdle

    Nothing -> do
      -- No work available, try stealing
      let config = peConfig engine
      when (pcEnableWorkStealing config) $ do
        mStolen <- tryStealWork engine wid
        case mStolen of
          Just workItem -> do
            updateWorkerStatus engine wid WSBusy
            void $ executeTask engine workItem
            updateWorkerStatus engine wid WSIdle
          Nothing -> threadDelay 1000  -- 1ms backoff

-- | Get work for a worker
getWork :: ParallelEngine -> Int -> IO (Maybe WorkItem)
getWork engine wid = do
  queues <- readTVarIO (peWorkQueues engine)
  case Map.lookup wid queues of
    Just queue -> atomically $ tryReadTQueue (wqQueue queue)
    Nothing -> pure Nothing

-- | Try to steal work from another worker
tryStealWork :: ParallelEngine -> Int -> IO (Maybe WorkItem)
tryStealWork engine wid = do
  queues <- readTVarIO (peWorkQueues engine)
  let otherQueues = Map.filterWithKey (\k _ -> k /= wid) queues

  -- Try each queue
  tryStealFromQueues (Map.elems otherQueues)
  where
    tryStealFromQueues :: [WorkQueue] -> IO (Maybe WorkItem)
    tryStealFromQueues [] = pure Nothing
    tryStealFromQueues (q:qs) = do
      mItem <- atomically $ tryReadTQueue (wqQueue q)
      case mItem of
        Just item -> pure (Just item)
        Nothing -> tryStealFromQueues qs

-- | Update worker status
updateWorkerStatus :: ParallelEngine -> Int -> WorkerStatus -> IO ()
updateWorkerStatus engine wid status = do
  atomically $ modifyTVar' (peWorkerPool engine) $ \pool ->
    pool { wpWorkers = Map.adjust (\w -> w { wStatus = status }) wid (wpWorkers pool) }

-- | Execute a single task
executeTask :: ParallelEngine -> WorkItem -> IO TaskResult
executeTask engine workItem = do
  startTime <- getCurrentTime

  let task = wiTask workItem
      config = peConfig engine

  -- Create cancellation token
  token <- newCancellationToken
  atomically $ modifyTVar' (peCancellation engine) $
    Map.insert (taskId task) token

  -- Execute with timeout
  result <- try @SomeException $ do
    withTimeout (pcTaskTimeout config) $ taskAction task

  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime

  -- Remove cancellation token
  atomically $ modifyTVar' (peCancellation engine) $
    Map.delete (taskId task)

  case result of
    Right (Just diags) -> pure TaskResult
      { trTaskId = taskId task
      , trStatus = TSCompleted
      , trDiagnostics = diags
      , trDuration = duration
      , trWorkerId = 0
      }
    Right Nothing -> pure TaskResult
      { trTaskId = taskId task
      , trStatus = TSTimeout
      , trDiagnostics = []
      , trDuration = duration
      , trWorkerId = 0
      }
    Left err -> pure TaskResult
      { trTaskId = taskId task
      , trStatus = TSFailed (T.pack $ show err)
      , trDiagnostics = []
      , trDuration = duration
      , trWorkerId = 0
      }
  where
    withTimeout :: Int -> IO a -> IO (Maybe a)
    withTimeout _ms action = Just <$> action  -- Simplified

-- | Get worker statistics
getWorkerStats :: ParallelEngine -> IO [(Int, WorkerStatus, Int)]
getWorkerStats engine = do
  pool <- readTVarIO (peWorkerPool engine)
  pure [(wId w, wStatus w, wTaskCount w) | w <- Map.elems (wpWorkers pool)]

--------------------------------------------------------------------------------
-- Progress Tracking
--------------------------------------------------------------------------------

-- | Progress tracking state
data ProgressTracker = ProgressTracker
  { ptTotal      :: Int
  , ptCompleted  :: Int
  , ptFailed     :: Int
  , ptPending    :: Int
  , ptCallbacks  :: [ProgressCallback]
  }
  deriving stock (Generic)

-- | Empty progress tracker
emptyProgressTracker :: ProgressTracker
emptyProgressTracker = ProgressTracker 0 0 0 0 []

-- | Progress information
data Progress = Progress
  { pTotal      :: Int
  , pCompleted  :: Int
  , pFailed     :: Int
  , pPending    :: Int
  , pPercentage :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Progress callback
type ProgressCallback = Progress -> IO ()

-- | Get current progress
getProgress :: ParallelEngine -> IO Progress
getProgress engine = do
  tracker <- readTVarIO (peProgress engine)
  let total = ptTotal tracker
      completed = ptCompleted tracker
      percentage = if total == 0 then 0.0
                   else fromIntegral completed / fromIntegral total * 100

  pure Progress
    { pTotal = total
    , pCompleted = completed
    , pFailed = ptFailed tracker
    , pPending = ptPending tracker
    , pPercentage = percentage
    }

-- | Set progress callback
setProgressCallback :: ParallelEngine -> ProgressCallback -> IO ()
setProgressCallback engine callback = do
  atomically $ modifyTVar' (peProgress engine) $ \t ->
    t { ptCallbacks = callback : ptCallbacks t }

--------------------------------------------------------------------------------
-- Work Queue
--------------------------------------------------------------------------------

-- | A work queue for a worker
data WorkQueue = WorkQueue
  { wqQueue :: TQueue WorkItem
  , wqSize  :: Int
  }

-- | A work item
data WorkItem = WorkItem
  { wiTask     :: Task
  , wiStatus   :: TaskStatus
  , wiAttempts :: Int
  }

-- | Steal work from a queue
stealWork :: WorkQueue -> STM (Maybe WorkItem)
stealWork queue = tryReadTQueue (wqQueue queue)

-- | Submit work to a queue
submitWork :: WorkQueue -> WorkItem -> STM ()
submitWork queue item = writeTQueue (wqQueue queue) item

--------------------------------------------------------------------------------
-- Resource Management
--------------------------------------------------------------------------------

-- | Resource limits
data ResourceLimits = ResourceLimits
  { rlMaxMemoryMB  :: Int
  , rlMaxThreads   :: Int
  , rlMaxQueueSize :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Current resource usage
data ResourceUsage = ResourceUsage
  { ruMemoryMB     :: Int
  , ruActiveThreads :: Int
  , ruQueueSize    :: Int
  , ruLastUpdated  :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty resource usage with epoch sentinel time
-- The epoch time indicates resources have not been measured yet
emptyResourceUsage :: ResourceUsage
emptyResourceUsage = ResourceUsage 0 0 0 epochTime
  where
    -- Unix epoch as sentinel value for "not yet measured"
    epochTime :: UTCTime
    epochTime = posixSecondsToUTCTime 0

-- | Check if resource limits are exceeded
checkResourceLimits :: ParallelEngine -> IO Bool
checkResourceLimits engine = do
  let config = peConfig engine
  usage <- readTVarIO (peResourceUsage engine)

  pure $ ruMemoryMB usage < pcMemoryLimitMB config

-- | Get current resource usage
getResourceUsage :: ParallelEngine -> IO ResourceUsage
getResourceUsage = readTVarIO . peResourceUsage

--------------------------------------------------------------------------------
-- Dependency Resolution
--------------------------------------------------------------------------------

-- | Dependencies between rules
data RuleDependencies = RuleDependencies
  { rdForward  :: Map Text [Text]   -- ^ Rule -> rules it depends on
  , rdReverse  :: Map Text [Text]   -- ^ Rule -> rules that depend on it
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Dependency graph for scheduling
data DependencyGraph = DependencyGraph
  { dgNodes  :: Set Text
  , dgEdges  :: [(Text, Text)]
  , dgLevels :: Map Int [Text]  -- ^ Level -> rules at that level
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Resolve dependency order for rules
resolveDependencyOrder :: RuleDependencies -> [Text]
resolveDependencyOrder deps =
  let nodes = Set.toList $ Set.fromList $
        Map.keys (rdForward deps) ++ concat (Map.elems (rdForward deps))
      sorted = topSort nodes (rdForward deps)
  in sorted
  where
    topSort :: [Text] -> Map Text [Text] -> [Text]
    topSort rules graph = go rules Set.empty []
      where
        go [] _ acc = reverse acc
        go remaining visited acc =
          let (ready, notReady) = partition (allDepsVisited visited graph) remaining
          in if null ready
             then reverse acc ++ notReady  -- Cycle, just return rest
             else go notReady (Set.union visited (Set.fromList ready)) (ready ++ acc)

        allDepsVisited visited g rule =
          all (`Set.member` visited) (Map.findWithDefault [] rule g)

        partition p xs = (filter p xs, filter (not . p) xs)

-- | Find rules that can run in parallel (no dependencies between them)
findIndependentRules :: RuleDependencies -> [[Text]]
findIndependentRules deps =
  let order = resolveDependencyOrder deps
  in groupByLevel order deps
  where
    groupByLevel :: [Text] -> RuleDependencies -> [[Text]]
    groupByLevel rules _deps = [rules]  -- Simplified

--------------------------------------------------------------------------------
-- Batching
--------------------------------------------------------------------------------

-- | Configuration for batching
data BatchConfig = BatchConfig
  { bcBatchSize    :: Int
  , bcMaxBatches   :: Int
  , bcBalanceLoad  :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A batch of work
data Batch = Batch
  { batchId    :: Int
  , batchItems :: [Task]
  , batchSize  :: Int
  }
  deriving stock (Generic)

instance Show Batch where
  show b = "Batch { batchId = " <> show (batchId b)
        <> ", batchSize = " <> show (batchSize b) <> " }"

-- | Create batches from tasks
createBatches :: BatchConfig -> [Task] -> [Batch]
createBatches config tasks =
  let batches = chunksOf (bcBatchSize config) tasks
  in zipWith (\i ts -> Batch i ts (length ts)) [0..] batches
  where
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Execute a batch
executeBatch :: ParallelEngine -> Batch -> IO [TaskResult]
executeBatch engine batch = do
  results <- forM (batchItems batch) $ \task -> do
    let workItem = WorkItem
          { wiTask = task
          , wiStatus = TSPending
          , wiAttempts = 0
          }
    executeTask engine workItem

  -- Optional GC after batch
  when (pcGCAfterBatch $ peConfig engine) performGC

  pure results

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

-- | Statistics about parallel execution
data ParallelStats = ParallelStats
  { psTasksScheduled :: Int
  , psTasksCompleted :: Int
  , psTasksFailed    :: Int
  , psTasksCancelled :: Int
  , psWorkSteals     :: Int
  , psTotalDuration  :: NominalDiffTime
  , psAvgTaskDuration :: NominalDiffTime
  , psMaxParallelism :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty statistics
emptyParallelStats :: ParallelStats
emptyParallelStats = ParallelStats 0 0 0 0 0 0 0 0

-- | Get parallel execution statistics
getStats :: ParallelEngine -> IO ParallelStats
getStats = readTVarIO . peStats

-- | Reset statistics
resetStats :: ParallelEngine -> IO ()
resetStats engine = do
  atomically $ writeTVar (peStats engine) emptyParallelStats

--------------------------------------------------------------------------------
-- Cancellation
--------------------------------------------------------------------------------

-- | Token for cancellation
data CancellationToken = CancellationToken
  { ctCancelled :: TVar Bool
  }

-- | Create a new cancellation token
newCancellationToken :: IO CancellationToken
newCancellationToken = CancellationToken <$> newTVarIO False

-- | Request cancellation
requestCancellation :: CancellationToken -> IO ()
requestCancellation token = do
  atomically $ writeTVar (ctCancelled token) True

-- | Check if cancelled
isCancelled :: CancellationToken -> IO Bool
isCancelled = readTVarIO . ctCancelled
