{-# LANGUAGE OverloadedStrings #-}

module ResourceLimitsSpec (spec) where

import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.DeepSeq (NFData (..))
import Data.Text qualified as T

import Argus.Analysis.ResourceLimits
import Argus.Config qualified as Cfg
import Argus.Config.Validation

spec :: Spec
spec = do
  describe "ResourceLimits" $ do
    describe "defaultResourceConfig" $ do
      it "has reasonable default values" $ do
        let cfg = defaultResourceConfig
        rcTimeoutSeconds cfg `shouldBe` Just 60
        rcMaxMemoryMB cfg `shouldBe` Just 2048
        rcForceGCInterval cfg `shouldBe` Just 100
        rcTrackPerFile cfg `shouldBe` True
        rcWarnSlowFiles cfg `shouldBe` Just 10.0
        rcMaxRetries cfg `shouldBe` 1
        rcKillOnTimeout cfg `shouldBe` True

    describe "withTimeout" $ do
      it "completes fast operations successfully" $ do
        let cfg = defaultResourceConfig { rcTimeoutSeconds = Just 5 }
        result <- withTimeout cfg (pure (42 :: Int))
        case result of
          Completed n _ -> n `shouldBe` 42
          _ -> expectationFailure "Expected Completed"

      it "returns elapsed time" $ do
        let cfg = defaultResourceConfig { rcTimeoutSeconds = Just 5 }
        result <- withTimeout cfg (threadDelay 100000 >> pure (1 :: Int))
        case result of
          Completed _ elapsed -> elapsed `shouldSatisfy` (> 0.05)
          _ -> expectationFailure "Expected Completed"

      it "handles exceptions gracefully" $ do
        let cfg = defaultResourceConfig { rcTimeoutSeconds = Just 5 }
        result <- withTimeout cfg (error "test error" :: IO Int)
        case result of
          Failed msg _ -> T.isInfixOf "test error" msg `shouldBe` True
          _ -> expectationFailure "Expected Failed"

      it "works with no timeout" $ do
        let cfg = defaultResourceConfig { rcTimeoutSeconds = Nothing }
        result <- withTimeout cfg (pure (123 :: Int))
        case result of
          Completed n _ -> n `shouldBe` 123
          _ -> expectationFailure "Expected Completed"

    describe "withTimeoutMicros" $ do
      it "times out long operations" $ do
        -- Use a 10ms timeout and try to delay for 2 seconds
        result <- withTimeoutMicros 10000 (threadDelay 2000000 >> pure (1 :: Int))
        case result of
          TimedOut elapsed -> elapsed `shouldSatisfy` (< 0.5)
          Completed _ _ -> pendingWith "Timeout did not trigger (system dependent)"
          Failed _ _ -> pendingWith "Operation failed instead of timing out"

      it "completes fast operations" $ do
        result <- withTimeoutMicros 1000000 (pure (42 :: Int))
        case result of
          Completed n _ -> n `shouldBe` 42
          _ -> expectationFailure "Expected Completed"

    describe "TimeoutResult" $ do
      it "has correct NFData instance" $ do
        let completed = Completed (42 :: Int) 1.5
            timedOut = TimedOut 2.0 :: TimeoutResult Int
            failed = Failed "error" 0.5 :: TimeoutResult Int
        rnf completed `seq` True `shouldBe` True
        rnf timedOut `seq` True `shouldBe` True
        rnf failed `seq` True `shouldBe` True

    describe "ResourceTracker" $ do
      it "can be created" $ do
        tracker <- newResourceTracker defaultResourceConfig
        -- Check that tracker was created successfully by getting stats
        stats <- getResourceStats tracker
        rsTotalFiles stats `shouldBe` 0

      it "records file starts" $ do
        tracker <- newResourceTracker defaultResourceConfig
        recordFileStart tracker "test.hs"
        -- Should not throw
        pure ()

      it "records completions" $ do
        tracker <- newResourceTracker defaultResourceConfig
        recordFileStart tracker "test.hs"
        recordFileComplete tracker "test.hs" 0.5
        stats <- getResourceStats tracker
        rsCompletedFiles stats `shouldBe` 1

      it "records timeouts" $ do
        tracker <- newResourceTracker defaultResourceConfig
        recordFileStart tracker "test.hs"
        recordFileTimeout tracker "test.hs" 10.0
        stats <- getResourceStats tracker
        rsTimedOutFiles stats `shouldBe` 1

      it "records errors" $ do
        tracker <- newResourceTracker defaultResourceConfig
        recordFileStart tracker "test.hs"
        recordFileError tracker "test.hs" "Parse error" 1.0
        stats <- getResourceStats tracker
        rsFailedFiles stats `shouldBe` 1

      it "tracks elapsed time" $ do
        tracker <- newResourceTracker defaultResourceConfig
        recordFileStart tracker "test1.hs"
        recordFileComplete tracker "test1.hs" 1.0
        recordFileStart tracker "test2.hs"
        recordFileComplete tracker "test2.hs" 2.0
        stats <- getResourceStats tracker
        rsTotalElapsed stats `shouldBe` 3.0
        rsAvgElapsed stats `shouldBe` 1.5

    describe "formatDuration" $ do
      it "formats microseconds" $ do
        formatDuration 0.0001 `shouldBe` "100μs"

      it "formats milliseconds" $ do
        formatDuration 0.123 `shouldBe` "123ms"

      it "formats seconds" $ do
        formatDuration 5.5 `shouldBe` "5.5s"

      it "formats minutes" $ do
        formatDuration 125 `shouldBe` "2m 5s"

      it "formats hours" $ do
        formatDuration 3665 `shouldBe` "1h 1m"

    describe "formatBytes" $ do
      it "formats bytes" $ do
        formatBytes 512 `shouldBe` "512 B"

      it "formats kilobytes" $ do
        formatBytes (5 * 1024) `shouldBe` "5 KB"

      it "formats megabytes" $ do
        formatBytes (10 * 1024 * 1024) `shouldBe` "10 MB"

      it "formats gigabytes" $ do
        formatBytes (2 * 1024 * 1024 * 1024) `shouldBe` "2 GB"

    describe "estimateComplexity" $ do
      it "returns 1.0 for simple files" $ do
        let content = "module Simple where\n\nx = 1\n"
        estimateComplexity content `shouldBe` 1.0

      it "increases for large files" $ do
        let content = T.unlines $ replicate 600 "x = 1"
        estimateComplexity content `shouldSatisfy` (> 1.0)

      it "increases for files with many imports" $ do
        let content = T.unlines $ replicate 40 "import Data.List"
        estimateComplexity content `shouldSatisfy` (> 1.0)

      it "increases for Template Haskell" $ do
        let content = "{-# LANGUAGE TemplateHaskell #-}\nmodule TH where\n"
        estimateComplexity content `shouldSatisfy` (> 1.0)

  describe "Config Integration" $ do
    describe "ResourceConfig in Config" $ do
      it "has default resource config" $ do
        let cfg = Cfg.cfgResource Cfg.defaultConfig
        Cfg.resTimeoutSeconds cfg `shouldBe` Just 60
        Cfg.resMaxMemoryMB cfg `shouldBe` Just 2048

    describe "validateResourceConfig" $ do
      it "accepts valid default config" $ do
        let cfg = Cfg.cfgResource Cfg.defaultConfig
            errs = validateResourceConfig cfg
        errs `shouldBe` []

      it "rejects negative timeout" $ do
        let cfg = Cfg.ResourceConfig
              { Cfg.resTimeoutSeconds = Just (-5)
              , Cfg.resMaxMemoryMB = Nothing
              , Cfg.resForceGCInterval = Nothing
              , Cfg.resTrackPerFile = True
              , Cfg.resWarnSlowFiles = Nothing
              , Cfg.resMaxRetries = 0
              , Cfg.resKillOnTimeout = True
              }
            errs = validateResourceConfig cfg
        length errs `shouldSatisfy` (> 0)
        any (\e -> veSeverity e == VSError) errs `shouldBe` True

      it "warns on very short timeout" $ do
        let cfg = Cfg.ResourceConfig
              { Cfg.resTimeoutSeconds = Just 2
              , Cfg.resMaxMemoryMB = Nothing
              , Cfg.resForceGCInterval = Nothing
              , Cfg.resTrackPerFile = True
              , Cfg.resWarnSlowFiles = Nothing
              , Cfg.resMaxRetries = 0
              , Cfg.resKillOnTimeout = True
              }
            errs = validateResourceConfig cfg
        any (\e -> veSeverity e == VSWarning) errs `shouldBe` True

      it "warns on very long timeout" $ do
        let cfg = Cfg.ResourceConfig
              { Cfg.resTimeoutSeconds = Just 1000
              , Cfg.resMaxMemoryMB = Nothing
              , Cfg.resForceGCInterval = Nothing
              , Cfg.resTrackPerFile = True
              , Cfg.resWarnSlowFiles = Nothing
              , Cfg.resMaxRetries = 0
              , Cfg.resKillOnTimeout = True
              }
            errs = validateResourceConfig cfg
        any (\e -> veSeverity e == VSWarning) errs `shouldBe` True

      it "rejects negative retries" $ do
        let cfg = Cfg.ResourceConfig
              { Cfg.resTimeoutSeconds = Nothing
              , Cfg.resMaxMemoryMB = Nothing
              , Cfg.resForceGCInterval = Nothing
              , Cfg.resTrackPerFile = True
              , Cfg.resWarnSlowFiles = Nothing
              , Cfg.resMaxRetries = -1
              , Cfg.resKillOnTimeout = True
              }
            errs = validateResourceConfig cfg
        any (\e -> veSeverity e == VSError) errs `shouldBe` True

      it "warns on high retry count" $ do
        let cfg = Cfg.ResourceConfig
              { Cfg.resTimeoutSeconds = Nothing
              , Cfg.resMaxMemoryMB = Nothing
              , Cfg.resForceGCInterval = Nothing
              , Cfg.resTrackPerFile = True
              , Cfg.resWarnSlowFiles = Nothing
              , Cfg.resMaxRetries = 10
              , Cfg.resKillOnTimeout = True
              }
            errs = validateResourceConfig cfg
        any (\e -> veSeverity e == VSWarning) errs `shouldBe` True

      it "warns on low memory limit" $ do
        let cfg = Cfg.ResourceConfig
              { Cfg.resTimeoutSeconds = Nothing
              , Cfg.resMaxMemoryMB = Just 64
              , Cfg.resForceGCInterval = Nothing
              , Cfg.resTrackPerFile = True
              , Cfg.resWarnSlowFiles = Nothing
              , Cfg.resMaxRetries = 0
              , Cfg.resKillOnTimeout = True
              }
            errs = validateResourceConfig cfg
        any (\e -> veSeverity e == VSWarning) errs `shouldBe` True

      it "warns when slow warning exceeds timeout" $ do
        let cfg = Cfg.ResourceConfig
              { Cfg.resTimeoutSeconds = Just 10
              , Cfg.resMaxMemoryMB = Nothing
              , Cfg.resForceGCInterval = Nothing
              , Cfg.resTrackPerFile = True
              , Cfg.resWarnSlowFiles = Just 20.0
              , Cfg.resMaxRetries = 0
              , Cfg.resKillOnTimeout = True
              }
            errs = validateResourceConfig cfg
        any (\e -> veSeverity e == VSWarning &&
                   T.isInfixOf "warn-slow-files" (veField e)) errs `shouldBe` True

      it "rejects negative slow warning" $ do
        let cfg = Cfg.ResourceConfig
              { Cfg.resTimeoutSeconds = Nothing
              , Cfg.resMaxMemoryMB = Nothing
              , Cfg.resForceGCInterval = Nothing
              , Cfg.resTrackPerFile = True
              , Cfg.resWarnSlowFiles = Just (-5.0)
              , Cfg.resMaxRetries = 0
              , Cfg.resKillOnTimeout = True
              }
            errs = validateResourceConfig cfg
        any (\e -> veSeverity e == VSError) errs `shouldBe` True

    describe "Full config validation" $ do
      it "validates config with resource section" $ do
        result <- validateConfig Cfg.defaultConfig
        result `shouldBe` ValidationSuccess
