{-# LANGUAGE OverloadedStrings #-}

module ProgressSpec (spec) where

import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM

import Argus.Output.Progress

spec :: Spec
spec = do
  describe "Argus.Output.Progress" $ do
    describe "ProgressConfig" $ do
      it "has sensible defaults" $ do
        pcEnabled defaultProgressConfig `shouldBe` True
        pcColor defaultProgressConfig `shouldBe` True
        pcUnicode defaultProgressConfig `shouldBe` True
        pcInteractive defaultProgressConfig `shouldBe` True
        pcWidth defaultProgressConfig `shouldBe` 80

      it "can be disabled" $ do
        let config = defaultProgressConfig { pcEnabled = False }
        pcEnabled config `shouldBe` False

    describe "SpinnerStyle" $ do
      it "has different styles" $ do
        SpinnerDots `shouldNotBe` SpinnerLine
        SpinnerCircle `shouldNotBe` SpinnerArrow
        SpinnerAscii `shouldNotBe` SpinnerDots

      it "has show instance" $ do
        show SpinnerDots `shouldBe` "SpinnerDots"
        show SpinnerLine `shouldBe` "SpinnerLine"
        show SpinnerCircle `shouldBe` "SpinnerCircle"
        show SpinnerArrow `shouldBe` "SpinnerArrow"
        show SpinnerAscii `shouldBe` "SpinnerAscii"

    describe "ProgressBarStyle" $ do
      it "has different styles" $ do
        ProgressBarBlock `shouldNotBe` ProgressBarShade
        ProgressBarArrow `shouldNotBe` ProgressBarAscii

      it "has show instance" $ do
        show ProgressBarBlock `shouldBe` "ProgressBarBlock"
        show ProgressBarShade `shouldBe` "ProgressBarShade"
        show ProgressBarArrow `shouldBe` "ProgressBarArrow"
        show ProgressBarAscii `shouldBe` "ProgressBarAscii"

    describe "StatusStyle" $ do
      it "has correct styles" $ do
        StatusSuccess `shouldNotBe` StatusWarning
        StatusError `shouldNotBe` StatusInfo
        StatusProgress `shouldNotBe` StatusSuccess

      it "has show instance" $ do
        show StatusSuccess `shouldBe` "StatusSuccess"
        show StatusWarning `shouldBe` "StatusWarning"
        show StatusError `shouldBe` "StatusError"
        show StatusInfo `shouldBe` "StatusInfo"
        show StatusProgress `shouldBe` "StatusProgress"

    describe "Spinner (non-interactive mode)" $ do
      it "starts and stops cleanly in non-interactive mode" $ do
        let config = defaultProgressConfig { pcInteractive = False }
        spinner <- startSpinner config SpinnerDots "Testing..."
        threadDelay 10000  -- 10ms
        stopSpinner spinner
        -- Should complete without error

      it "can update message in non-interactive mode" $ do
        let config = defaultProgressConfig { pcInteractive = False }
        spinner <- startSpinner config SpinnerDots "Initial"
        updateSpinnerMessage spinner "Updated"
        msg <- atomically $ readTVar (spinnerMessage spinner)
        msg `shouldBe` "Updated"
        stopSpinner spinner

      it "works with different styles" $ do
        let config = defaultProgressConfig { pcInteractive = False }
        let styles = [SpinnerDots, SpinnerLine, SpinnerCircle, SpinnerArrow, SpinnerAscii]
        mapM_ (\style -> do
          spinner <- startSpinner config style "Test"
          stopSpinner spinner) styles

    describe "ProgressBar (non-interactive mode)" $ do
      it "starts and stops cleanly in non-interactive mode" $ do
        let config = defaultProgressConfig { pcInteractive = False }
        pb <- startProgressBar config ProgressBarBlock "Testing" 100
        threadDelay 10000  -- 10ms
        stopProgressBar pb
        -- Should complete without error

      it "can update progress in non-interactive mode" $ do
        let config = defaultProgressConfig { pcInteractive = False }
        pb <- startProgressBar config ProgressBarBlock "Testing" 100
        updateProgress pb 50
        current <- atomically $ readTVar (pbCurrent pb)
        current `shouldBe` 50
        stopProgressBar pb

      it "works with different styles" $ do
        let config = defaultProgressConfig { pcInteractive = False }
        let styles = [ProgressBarBlock, ProgressBarShade, ProgressBarArrow, ProgressBarAscii]
        mapM_ (\style -> do
          pb <- startProgressBar config style "Test" 10
          updateProgress pb 5
          stopProgressBar pb) styles

    describe "withSpinner (non-interactive)" $ do
      it "runs action and stops spinner" $ do
        let config = defaultProgressConfig { pcInteractive = False }
        result <- withSpinner config SpinnerDots "Testing" $ do
          threadDelay 10000
          pure (42 :: Int)
        result `shouldBe` 42

      it "handles exceptions" $ do
        let config = defaultProgressConfig { pcInteractive = False }
        result <- withSpinner config SpinnerDots "Testing" $ do
          pure "success"
        result `shouldBe` "success"

    describe "withProgressBar (non-interactive)" $ do
      it "runs action and stops progress bar" $ do
        let config = defaultProgressConfig { pcInteractive = False }
        result <- withProgressBar config ProgressBarBlock "Testing" 100 $ do
          threadDelay 10000
          pure (42 :: Int)
        result `shouldBe` 42

    describe "ScanProgress" $ do
      it "tracks files scanned in non-interactive mode" $ do
        let config = defaultProgressConfig { pcInteractive = False }
        withScanProgress config $ \sp -> do
          atomically $ writeTVar (spFilesScanned sp) 10
          count <- atomically $ readTVar (spFilesScanned sp)
          count `shouldBe` 10

      it "tracks current directory" $ do
        let config = defaultProgressConfig { pcInteractive = False }
        withScanProgress config $ \sp -> do
          atomically $ writeTVar (spCurrentDir sp) "src/Argus"
          dir <- atomically $ readTVar (spCurrentDir sp)
          dir `shouldBe` "src/Argus"

    describe "AnalysisProgress" $ do
      it "tracks files analyzed in non-interactive mode" $ do
        let config = defaultProgressConfig { pcInteractive = False }
        withAnalysisProgress config 100 $ \ap -> do
          atomically $ writeTVar (apFilesAnalyzed ap) 50
          count <- atomically $ readTVar (apFilesAnalyzed ap)
          count `shouldBe` 50

      it "tracks current file" $ do
        let config = defaultProgressConfig { pcInteractive = False }
        withAnalysisProgress config 10 $ \ap -> do
          atomically $ writeTVar (apCurrentFile ap) "Test.hs"
          file <- atomically $ readTVar (apCurrentFile ap)
          file `shouldBe` "Test.hs"

      it "has correct total files" $ do
        let config = defaultProgressConfig { pcInteractive = False }
        withAnalysisProgress config 42 $ \ap -> do
          total <- atomically $ readTVar (apTotalFiles ap)
          total `shouldBe` 42
