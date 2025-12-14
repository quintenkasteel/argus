{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ParallelSpec (spec) where

import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Argus.Types
import Argus.Analysis.Parallel

spec :: Spec
spec = do
  describe "Argus.Analysis.Parallel" $ do
    describe "ParallelConfig" $ do
      describe "defaultParallelConfig" $ do
        it "has sensible default workers (auto-detect)" $ do
          pcNumWorkers defaultParallelConfig `shouldBe` Nothing

        it "has reasonable chunk size" $ do
          pcChunkSize defaultParallelConfig `shouldSatisfy` (> 0)
          pcChunkSize defaultParallelConfig `shouldSatisfy` (<= 100)

        it "enables GC between chunks by default" $ do
          pcForceGC defaultParallelConfig `shouldBe` True

        it "has reasonable timeout" $ do
          pcTimeout defaultParallelConfig `shouldSatisfy` maybe True (> 0)

        it "does not retry on error by default" $ do
          pcRetryOnError defaultParallelConfig `shouldBe` False

      describe "custom configuration" $ do
        it "allows setting number of workers" $ do
          let config = defaultParallelConfig { pcNumWorkers = Just 4 }
          pcNumWorkers config `shouldBe` Just 4

        it "allows setting chunk size" $ do
          let config = defaultParallelConfig { pcChunkSize = 10 }
          pcChunkSize config `shouldBe` 10

        it "allows disabling GC" $ do
          let config = defaultParallelConfig { pcForceGC = False }
          pcForceGC config `shouldBe` False

        it "allows enabling retry on error" $ do
          let config = defaultParallelConfig { pcRetryOnError = True }
          pcRetryOnError config `shouldBe` True

    describe "ProgressEvent" $ do
      it "ProgressStarted contains file count" $ do
        let event = ProgressStarted 10
        case event of
          ProgressStarted n -> n `shouldBe` 10
          _ -> expectationFailure "Expected ProgressStarted"

      it "ProgressFileStarted contains file path" $ do
        let event = ProgressFileStarted "/path/to/file.hs"
        case event of
          ProgressFileStarted p -> p `shouldBe` "/path/to/file.hs"
          _ -> expectationFailure "Expected ProgressFileStarted"

      it "ProgressFileCompleted contains path and count" $ do
        let event = ProgressFileCompleted "/path/to/file.hs" 5
        case event of
          ProgressFileCompleted p n -> do
            p `shouldBe` "/path/to/file.hs"
            n `shouldBe` 5
          _ -> expectationFailure "Expected ProgressFileCompleted"

      it "ProgressFileFailed contains path and error" $ do
        let event = ProgressFileFailed "/path/to/file.hs" "parse error"
        case event of
          ProgressFileFailed p e -> do
            p `shouldBe` "/path/to/file.hs"
            e `shouldBe` "parse error"
          _ -> expectationFailure "Expected ProgressFileFailed"

      it "ProgressChunkCompleted contains indices" $ do
        let event = ProgressChunkCompleted 2 5
        case event of
          ProgressChunkCompleted c t -> do
            c `shouldBe` 2
            t `shouldBe` 5
          _ -> expectationFailure "Expected ProgressChunkCompleted"

      it "ProgressCompleted contains stats" $ do
        let stats = ProgressStats 10 2 50 5.0 0.5
            event = ProgressCompleted stats
        case event of
          ProgressCompleted s -> s `shouldBe` stats
          _ -> expectationFailure "Expected ProgressCompleted"

      it "has Eq instance" $ do
        ProgressStarted 10 `shouldBe` ProgressStarted 10
        ProgressStarted 10 `shouldNotBe` ProgressStarted 20

      it "has Show instance" $ do
        show (ProgressStarted 10) `shouldContain` "10"

    describe "ProgressStats" $ do
      it "stores file analysis counts" $ do
        let stats = ProgressStats
              { psFilesAnalyzed = 100
              , psFilesFailed = 5
              , psTotalDiags = 250
              , psElapsedSeconds = 30.5
              , psAvgSeconds = 0.305
              }
        psFilesAnalyzed stats `shouldBe` 100
        psFilesFailed stats `shouldBe` 5
        psTotalDiags stats `shouldBe` 250

      it "stores timing information" $ do
        let stats = ProgressStats 100 5 250 30.5 0.305
        psElapsedSeconds stats `shouldBe` 30.5
        psAvgSeconds stats `shouldBe` 0.305

      it "has Eq instance" $ do
        let stats1 = ProgressStats 10 0 20 1.0 0.1
            stats2 = ProgressStats 10 0 20 1.0 0.1
        stats1 `shouldBe` stats2

    describe "analyzeFilesParallel" $ do
      it "returns empty map for empty file list" $ do
        let analyzer _ = pure $ FileResult "test.hs" [] [] [] []
        results <- analyzeFilesParallel defaultParallelConfig analyzer []
        Map.size results `shouldBe` 0

      it "analyzes single file" $ do
        let analyzer path = pure $ FileResult path [] [] [] []
        results <- analyzeFilesParallel defaultParallelConfig analyzer ["test.hs"]
        Map.size results `shouldBe` 1
        Map.member "test.hs" results `shouldBe` True

      it "analyzes multiple files" $ do
        let files = ["a.hs", "b.hs", "c.hs"]
            analyzer path = pure $ FileResult path [] [] [] []
        results <- analyzeFilesParallel defaultParallelConfig analyzer files
        Map.size results `shouldBe` 3
        all (\f -> Map.member f results) files `shouldBe` True

      it "preserves diagnostics from analyzer" $ do
        let diag = mkTestDiagnostic "test warning"
            analyzer _ = pure $ FileResult "test.hs" [diag] [] [] []
        results <- analyzeFilesParallel defaultParallelConfig analyzer ["test.hs"]
        case Map.lookup "test.hs" results of
          Just fr -> length (fileResultDiagnostics fr) `shouldBe` 1
          Nothing -> expectationFailure "Expected file result"

      it "handles analyzer exceptions gracefully" $ do
        let analyzer path
              | path == "fail.hs" = error "simulated failure"
              | otherwise = pure $ FileResult path [] [] [] []
        results <- analyzeFilesParallel defaultParallelConfig analyzer
          ["good.hs", "fail.hs"]
        -- The good file should succeed, bad file excluded from results
        Map.member "good.hs" results `shouldBe` True

    describe "analyzeFilesParallelWithProgress" $ do
      it "calls progress callback with ProgressStarted" $ do
        eventsRef <- newIORef []
        let callback e = atomicModifyIORef' eventsRef $ \es -> (e:es, ())
            analyzer path = pure $ FileResult path [] [] [] []
            isProgressStarted (ProgressStarted _) = True
            isProgressStarted _ = False
        _ <- analyzeFilesParallelWithProgress
          defaultParallelConfig analyzer ["test.hs"] callback
        events <- readIORef eventsRef
        any isProgressStarted events `shouldBe` True

      it "calls progress callback with ProgressCompleted" $ do
        eventsRef <- newIORef []
        let callback e = atomicModifyIORef' eventsRef $ \es -> (e:es, ())
            analyzer path = pure $ FileResult path [] [] [] []
            isProgressCompleted (ProgressCompleted _) = True
            isProgressCompleted _ = False
        _ <- analyzeFilesParallelWithProgress
          defaultParallelConfig analyzer ["test.hs"] callback
        events <- readIORef eventsRef
        any isProgressCompleted events `shouldBe` True

      it "calls progress callback for each file" $ do
        eventsRef <- newIORef []
        let callback e = atomicModifyIORef' eventsRef $ \es -> (e:es, ())
            analyzer path = pure $ FileResult path [] [] [] []
            files = ["a.hs", "b.hs", "c.hs"]
        _ <- analyzeFilesParallelWithProgress
          defaultParallelConfig analyzer files callback
        events <- readIORef eventsRef
        let fileStarted = length [e | e@(ProgressFileStarted _) <- events]
            fileCompleted = length [e | e@(ProgressFileCompleted _ _) <- events]
        fileStarted `shouldBe` 3
        fileCompleted `shouldBe` 3

      it "reports failed files in callback" $ do
        eventsRef <- newIORef []
        let callback e = atomicModifyIORef' eventsRef $ \es -> (e:es, ())
            analyzer path
              | path == "fail.hs" = error "simulated failure"
              | otherwise = pure $ FileResult path [] [] [] []
            isProgressFailed (ProgressFileFailed _ _) = True
            isProgressFailed _ = False
        _ <- analyzeFilesParallelWithProgress
          defaultParallelConfig analyzer ["good.hs", "fail.hs"] callback
        events <- readIORef eventsRef
        any isProgressFailed events `shouldBe` True

      it "stats reflect actual analysis results" $ do
        statsRef <- newIORef Nothing
        let callback (ProgressCompleted s) = writeIORef statsRef (Just s)
            callback _ = pure ()
            analyzer path = pure $ FileResult path [mkTestDiagnostic "warning"] [] [] []
        _ <- analyzeFilesParallelWithProgress
          defaultParallelConfig analyzer ["a.hs", "b.hs"] callback
        Just stats <- readIORef statsRef
        psFilesAnalyzed stats `shouldBe` 2
        psFilesFailed stats `shouldBe` 0
        psTotalDiags stats `shouldBe` 2

    describe "WorkerPool" $ do
      describe "newWorkerPool" $ do
        it "creates pool with specified workers" $ do
          pool <- newWorkerPool 4
          -- Pool created successfully, verify it shuts down cleanly
          shutdownPool pool

        it "creates pool with zero workers" $ do
          pool <- newWorkerPool 0
          -- Pool created successfully with zero workers
          shutdownPool pool

      describe "submitTask" $ do
        it "submits task to pool" $ do
          pool <- newWorkerPool 2
          submitTask pool (pure (42 :: Int))
          shutdownPool pool

      describe "shutdownPool" $ do
        it "shuts down cleanly" $ do
          pool <- newWorkerPool 2
          shutdownPool pool
          -- Should complete without hanging

    describe "chunksOf" $ do
      it "splits empty list" $ do
        chunksOfTest 5 ([] :: [Int]) `shouldBe` []

      it "splits list smaller than chunk size" $ do
        chunksOfTest 5 [1,2,3] `shouldBe` [[1,2,3]]

      it "splits list equal to chunk size" $ do
        chunksOfTest 3 [1,2,3] `shouldBe` [[1,2,3]]

      it "splits list larger than chunk size" $ do
        chunksOfTest 2 [1,2,3,4,5] `shouldBe` [[1,2],[3,4],[5]]

      it "handles chunk size of 1" $ do
        chunksOfTest 1 [1,2,3] `shouldBe` [[1],[2],[3]]

    describe "Edge Cases" $ do
      it "handles large file count" $ do
        let files = ["file" ++ show n ++ ".hs" | n <- [1..100]]
            analyzer path = pure $ FileResult path [] [] [] []
            config = defaultParallelConfig { pcChunkSize = 20 }
        results <- analyzeFilesParallel config analyzer files
        Map.size results `shouldBe` 100

      it "handles concurrent analysis correctly" $ do
        resultsRef <- newIORef []
        let files = ["file" ++ show n ++ ".hs" | n <- [1..50]]
            analyzer path = do
              atomicModifyIORef' resultsRef $ \rs -> (path:rs, ())
              pure $ FileResult path [] [] [] []
        _ <- analyzeFilesParallel defaultParallelConfig analyzer files
        analyzed <- readIORef resultsRef
        length analyzed `shouldBe` 50

      it "handles files with very long paths" $ do
        let longPath = concat (replicate 50 "very/deep/path/") ++ "Module.hs"
            analyzer path = pure $ FileResult path [] [] [] []
        results <- analyzeFilesParallel defaultParallelConfig analyzer [longPath]
        Map.size results `shouldBe` 1
        Map.member longPath results `shouldBe` True

      it "handles duplicate file paths" $ do
        let files = ["same.hs", "same.hs", "same.hs"]
            analyzer path = pure $ FileResult path [] [] [] []
        results <- analyzeFilesParallel defaultParallelConfig analyzer files
        -- Results stored in map by path, so duplicates are overwritten
        Map.member "same.hs" results `shouldBe` True

      it "handles slow analyzer without hanging" $ do
        let analyzer path = do
              threadDelay 10000  -- 10ms delay
              pure $ FileResult path [] [] [] []
            config = defaultParallelConfig { pcNumWorkers = Just 4 }
        results <- analyzeFilesParallel config analyzer ["a.hs", "b.hs"]
        Map.size results `shouldBe` 2

      it "handles mixed success and failure gracefully" $ do
        let analyzer path
              | path == "fail1.hs" = error "error1"
              | path == "fail2.hs" = error "error2"
              | otherwise = pure $ FileResult path [] [] [] []
            files = ["good1.hs", "fail1.hs", "good2.hs", "fail2.hs", "good3.hs"]
        results <- analyzeFilesParallel defaultParallelConfig analyzer files
        Map.member "good1.hs" results `shouldBe` True
        Map.member "good2.hs" results `shouldBe` True
        Map.member "good3.hs" results `shouldBe` True

    describe "Configuration Variations" $ do
      it "works with very small chunk size" $ do
        let files = ["a.hs", "b.hs", "c.hs", "d.hs", "e.hs"]
            analyzer path = pure $ FileResult path [] [] [] []
            config = defaultParallelConfig { pcChunkSize = 1 }
        results <- analyzeFilesParallel config analyzer files
        Map.size results `shouldBe` 5

      it "works with chunk size larger than file count" $ do
        let files = ["a.hs", "b.hs"]
            analyzer path = pure $ FileResult path [] [] [] []
            config = defaultParallelConfig { pcChunkSize = 1000 }
        results <- analyzeFilesParallel config analyzer files
        Map.size results `shouldBe` 2

      it "works with single worker" $ do
        let files = ["a.hs", "b.hs", "c.hs"]
            analyzer path = pure $ FileResult path [] [] [] []
            config = defaultParallelConfig { pcNumWorkers = Just 1 }
        results <- analyzeFilesParallel config analyzer files
        Map.size results `shouldBe` 3

      it "works with GC disabled" $ do
        let files = ["a.hs", "b.hs", "c.hs"]
            analyzer path = pure $ FileResult path [] [] [] []
            config = defaultParallelConfig { pcForceGC = False }
        results <- analyzeFilesParallel config analyzer files
        Map.size results `shouldBe` 3

    describe "ProgressStats validation" $ do
      it "records correct elapsed time" $ do
        statsRef <- newIORef Nothing
        let callback (ProgressCompleted s) = writeIORef statsRef (Just s)
            callback _ = pure ()
            analyzer path = do
              threadDelay 5000  -- 5ms
              pure $ FileResult path [] [] [] []
        _ <- analyzeFilesParallelWithProgress
          defaultParallelConfig analyzer ["test.hs"] callback
        Just stats <- readIORef statsRef
        psElapsedSeconds stats `shouldSatisfy` (>= 0.005)

      it "calculates average time per file" $ do
        statsRef <- newIORef Nothing
        let callback (ProgressCompleted s) = writeIORef statsRef (Just s)
            callback _ = pure ()
            analyzer path = pure $ FileResult path [] [] [] []
        _ <- analyzeFilesParallelWithProgress
          defaultParallelConfig analyzer ["a.hs", "b.hs", "c.hs"] callback
        Just stats <- readIORef statsRef
        psAvgSeconds stats `shouldSatisfy` (>= 0)

      it "counts diagnostics correctly" $ do
        statsRef <- newIORef Nothing
        let callback (ProgressCompleted s) = writeIORef statsRef (Just s)
            callback _ = pure ()
            mkDiags n = [mkTestDiagnostic "warning" | _ <- [1..n]]
            analyzer path
              | path == "many.hs" = pure $ FileResult path (mkDiags 5) [] [] []
              | otherwise = pure $ FileResult path (mkDiags 1) [] [] []
        _ <- analyzeFilesParallelWithProgress
          defaultParallelConfig analyzer ["many.hs", "one.hs"] callback
        Just stats <- readIORef statsRef
        psTotalDiags stats `shouldBe` 6

-- Helper functions

mkTestDiagnostic :: T.Text -> Diagnostic
mkTestDiagnostic msg = Diagnostic
  { diagSpan = mkSrcSpanRaw "test.hs" 1 1 1 10
  , diagSeverity = Warning
  , diagKind = CodePattern
  , diagMessage = msg
  , diagCode = Just "TEST001"
  , diagFixes = []
  , diagRelated = []
  }

-- Re-implementation of chunksOf for testing (since it's not exported)
chunksOfTest :: Int -> [a] -> [[a]]
chunksOfTest _ [] = []
chunksOfTest n xs =
  let (chunk, rest) = splitAt n xs
  in chunk : chunksOfTest n rest
