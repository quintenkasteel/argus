{-# LANGUAGE OverloadedStrings #-}

module ConcurrencySpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Argus.Rules.Concurrency
import Argus.Types (Diagnostic(..), Severity(..))

spec :: Spec
spec = do
  describe "Argus.Rules.Concurrency" $ do
    describe "defaultConcurrencyConfig" $ do
      it "is enabled by default" $ do
        ccEnabled defaultConcurrencyConfig `shouldBe` True

      it "has all checks enabled by default" $ do
        ccCheckSTM defaultConcurrencyConfig `shouldBe` True
        ccCheckAsync defaultConcurrencyConfig `shouldBe` True
        ccCheckRace defaultConcurrencyConfig `shouldBe` True
        ccCheckDeadlock defaultConcurrencyConfig `shouldBe` True
        ccCheckUnsafe defaultConcurrencyConfig `shouldBe` True
        ccCheckResource defaultConcurrencyConfig `shouldBe` True

    describe "detectConcurrencyIssues" $ do
      describe "when disabled" $ do
        it "returns empty list" $ do
          let config = defaultConcurrencyConfig { ccEnabled = False }
              code = "action = atomically retry"
          detectConcurrencyIssues config "test.hs" code `shouldBe` []

      describe "STM patterns" $ do
        it "detects 'retry' without 'orElse'" $ do
          let code = "action = atomically retry"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "retry" . diagMessage) diags `shouldBe` True

        it "does not flag 'retry' with 'orElse'" $ do
          let code = "action = atomically (retry `orElse` alternative)"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          any (\d -> "retry" `T.isInfixOf` diagMessage d && "orElse" `T.isInfixOf` diagMessage d) diags `shouldBe` False

        it "detects 'readTVar' and 'writeTVar' on same line" $ do
          let code = "action = do { x <- readTVar ref; writeTVar ref (x + 1) }"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects nested 'atomically'" $ do
          let code = "action = atomically $ atomically something"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "atomically" . diagMessage) diags `shouldBe` True

        it "detects 'unsafeIOToSTM'" $ do
          let code = "action = atomically (unsafeIOToSTM doIO)"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "unsafeIOToSTM" . diagMessage) diags `shouldBe` True

        it "respects ccCheckSTM = False" $ do
          let config = defaultConcurrencyConfig { ccCheckSTM = False }
              code = "action = atomically retry"
          detectConcurrencyIssues config "test.hs" code `shouldBe` []

      describe "async exception patterns" $ do
        it "detects 'catch' without async exception handling" $ do
          let code = "action = catch doSomething handler"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "catch" . diagMessage) diags `shouldBe` True

        it "does not flag catch with AsyncException" $ do
          let code = "action = catch doSomething (\\(e :: AsyncException) -> handler)"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          -- Should not produce async warning for this line
          any (\d -> "catch" `T.isInfixOf` diagMessage d && "async" `T.isInfixOf` T.toLower (diagMessage d)) diags `shouldBe` False

        it "detects 'mask_'" $ do
          let code = "action = mask_ doSomething"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects 'uninterruptibleMask'" $ do
          let code = "action = uninterruptibleMask $ \\restore -> doWork"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "uninterruptibleMask" . diagMessage) diags `shouldBe` True

        it "respects ccCheckAsync = False" $ do
          let config = defaultConcurrencyConfig { ccCheckAsync = False }
              code = "action = catch doSomething handler"
          -- Only async-related warnings should be filtered
          let diags = detectConcurrencyIssues config "test.hs" code
          all (\d -> not ("catch" `T.isInfixOf` diagMessage d)) diags `shouldBe` True

      describe "race condition patterns" $ do
        it "detects non-atomic IORef operations" $ do
          let code = "action = do { x <- readIORef ref; writeIORef ref (x + 1) }"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "IORef" . diagMessage) diags `shouldBe` True

        it "detects 'takeMVar' pattern" $ do
          let code = "action = do { x <- takeMVar mvar; putMVar mvar (f x) }"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "takeMVar" . diagMessage) diags `shouldBe` True

        it "detects unsupervised 'forkIO'" $ do
          let code = "action = forkIO doWork"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "forkIO" . diagMessage) diags `shouldBe` True

        it "respects ccCheckRace = False" $ do
          let config = defaultConcurrencyConfig { ccCheckRace = False }
              code = "action = do { x <- readIORef ref; writeIORef ref (x + 1) }"
          let diags = detectConcurrencyIssues config "test.hs" code
          all (\d -> not ("IORef" `T.isInfixOf` diagMessage d)) diags `shouldBe` True

      describe "deadlock patterns" $ do
        it "detects multiple 'takeMVar' on same line" $ do
          let code = "action = do { a <- takeMVar mv1; b <- takeMVar mv2 }"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "deadlock" . T.toLower . diagMessage) diags `shouldBe` True

        it "detects 'waitQSem'" $ do
          let code = "action = waitQSem sem >> doWork"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "respects ccCheckDeadlock = False" $ do
          let config = defaultConcurrencyConfig { ccCheckDeadlock = False }
              code = "action = do { a <- takeMVar mv1; b <- takeMVar mv2 }"
          let diags = detectConcurrencyIssues config "test.hs" code
          all (\d -> not ("deadlock" `T.isInfixOf` T.toLower (diagMessage d))) diags `shouldBe` True

      describe "unsafe concurrent patterns" $ do
        it "detects hardcoded 'threadDelay'" $ do
          let code = "action = threadDelay 1000"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "delays" . diagMessage) diags `shouldBe` True

        it "detects timeout with throwIO" $ do
          let code = "action = timeout 5 action >>= maybe (throwIO TimeoutException) pure"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "respects ccCheckUnsafe = False" $ do
          let config = defaultConcurrencyConfig { ccCheckUnsafe = False }
              code = "action = threadDelay 1000"
          let diags = detectConcurrencyIssues config "test.hs" code
          all (\d -> not ("threadDelay" `T.isInfixOf` diagMessage d)) diags `shouldBe` True

      describe "resource leak patterns" $ do
        it "detects unbounded Chan" $ do
          let code = "action = newChan >>= \\chan -> writeChan chan value"
              diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "Chan" . diagMessage) diags `shouldBe` True

        it "respects ccCheckResource = False" $ do
          let config = defaultConcurrencyConfig { ccCheckResource = False }
              code = "action = newChan >>= \\chan -> writeChan chan value"
          let diags = detectConcurrencyIssues config "test.hs" code
          all (\d -> not ("Chan" `T.isInfixOf` diagMessage d)) diags `shouldBe` True

    describe "ConcurrencyCategory" $ do
      it "has all expected categories" $ do
        minBound `shouldBe` STMAntiPattern
        maxBound `shouldBe` ResourceLeak

      it "has 6 categories" $ do
        let categories = [minBound..maxBound] :: [ConcurrencyCategory]
        length categories `shouldBe` 6

    describe "diagnostic codes" $ do
      it "uses concurrency/ prefix for diagnostic codes" $ do
        let code = "action = atomically retry"
            diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
        all (maybe False (T.isPrefixOf "concurrency/") . diagCode) diags `shouldBe` True

    describe "comment handling" $ do
      it "ignores patterns in comments" $ do
        let code = "-- action = atomically retry"
            diags = detectConcurrencyIssues defaultConcurrencyConfig "test.hs" code
        length diags `shouldBe` 0
