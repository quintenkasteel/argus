{-# LANGUAGE OverloadedStrings #-}

module ErrorHandlingSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Argus.Rules.ErrorHandling
import Argus.Types (Diagnostic(..))

spec :: Spec
spec = do
  describe "Argus.Rules.ErrorHandling" $ do
    describe "defaultErrorHandlingConfig" $ do
      it "is enabled by default" $ do
        ehEnabled defaultErrorHandlingConfig `shouldBe` True

      it "has all checks enabled by default" $ do
        ehCheckBroadCatch defaultErrorHandlingConfig `shouldBe` True
        ehCheckSilent defaultErrorHandlingConfig `shouldBe` True
        ehCheckPartial defaultErrorHandlingConfig `shouldBe` True
        ehCheckPropagation defaultErrorHandlingConfig `shouldBe` True
        ehCheckCleanup defaultErrorHandlingConfig `shouldBe` True
        ehCheckMonadError defaultErrorHandlingConfig `shouldBe` True

    describe "detectErrorHandlingIssues" $ do
      describe "when disabled" $ do
        it "returns empty list" $ do
          let config = defaultErrorHandlingConfig { ehEnabled = False }
              code = "action = catch doSomething (\\(e :: SomeException) -> handler)"
          detectErrorHandlingIssues config "test.hs" code `shouldBe` []

      describe "broad exception catching" $ do
        it "detects catching SomeException" $ do
          let code = "action = catch doSomething (\\(e :: SomeException) -> handler)"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "SomeException" . diagMessage) diags `shouldBe` True

        it "detects wildcard exception handler" $ do
          let code = "action = catch doSomething (\\_ -> return ())"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects handle with SomeException" $ do
          let code = "action = handle (\\(e :: SomeException) -> handler) doSomething"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects catchIOError with const" $ do
          let code = "action = catchIOError doSomething (const (return Nothing))"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "respects ehCheckBroadCatch = False" $ do
          let config = defaultErrorHandlingConfig { ehCheckBroadCatch = False }
              code = "action = catch doSomething (\\(e :: SomeException) -> handler)"
          detectErrorHandlingIssues config "test.hs" code `shouldBe` []

      describe "silent failure" $ do
        it "detects 'void $ try'" $ do
          let code = "action = void $ try doSomething"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "void" . diagMessage) diags `shouldBe` True

        it "detects catch returning unit" $ do
          let code = "action = catch doSomething (\\_ -> return ())"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects discarded runExceptT" $ do
          let code = "action = _ <- runExceptT computation"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "runExceptT" . diagMessage) diags `shouldBe` True

        it "detects fromMaybe with silent defaults" $ do
          let code = "result = fromMaybe \"\" maybeValue"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "respects ehCheckSilent = False" $ do
          let config = defaultErrorHandlingConfig { ehCheckSilent = False }
              code = "action = void $ try doSomething"
          let diags = detectErrorHandlingIssues config "test.hs" code
          all (\d -> not ("void" `T.isInfixOf` diagMessage d)) diags `shouldBe` True

      describe "partial patterns" $ do
        it "detects 'let Just x = ...'" $ do
          let code = "foo = let Just x = maybeval in x"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "Just" . diagMessage) diags `shouldBe` True

        it "detects 'let Right x = ...'" $ do
          let code = "foo = let Right x = eitherval in x"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "Right" . diagMessage) diags `shouldBe` True

        it "detects list pattern without handling empty" $ do
          let code = "foo = let (x:_) = xs in x"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "respects ehCheckPartial = False" $ do
          let config = defaultErrorHandlingConfig { ehCheckPartial = False }
              code = "foo = let Just x = maybeval in x"
          let diags = detectErrorHandlingIssues config "test.hs" code
          all (\d -> not ("Just" `T.isInfixOf` diagMessage d)) diags `shouldBe` True

      describe "improper error propagation" $ do
        it "detects throwError with string" $ do
          let code = "action = throwError \"Something went wrong\""
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "String" . diagMessage) diags `shouldBe` True

        it "detects error with string" $ do
          let code = "action = error \"This should not happen\""
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "error" . diagMessage) diags `shouldBe` True

        it "detects fail with string" $ do
          let code = "action = fail \"Pattern match failure\""
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects Left with string" $ do
          let code = "result = Left \"Error occurred\""
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "respects ehCheckPropagation = False" $ do
          let config = defaultErrorHandlingConfig { ehCheckPropagation = False }
              code = "action = throwError \"Something went wrong\""
          let diags = detectErrorHandlingIssues config "test.hs" code
          all (\d -> not ("throwError" `T.isInfixOf` diagMessage d)) diags `shouldBe` True

      describe "resource cleanup" $ do
        it "detects openFile without bracket" $ do
          let code = "action = do { h <- openFile path ReadMode; contents <- hGetContents h }"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "handle" . T.toLower . diagMessage) diags `shouldBe` True

        it "does not flag withFile" $ do
          let code = "action = withFile path ReadMode (\\h -> hGetContents h)"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          -- Should not produce open warnings since withFile is used
          all (\d -> not ("open" `T.isInfixOf` T.toLower (diagMessage d) && "File" `T.isInfixOf` diagMessage d)) diags `shouldBe` True

        it "detects malloc without bracket" $ do
          let code = "action = do { ptr <- malloc; poke ptr value }"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "respects ehCheckCleanup = False" $ do
          let config = defaultErrorHandlingConfig { ehCheckCleanup = False }
              code = "action = do { h <- openFile path ReadMode; contents <- hGetContents h }"
          let diags = detectErrorHandlingIssues config "test.hs" code
          all (\d -> not ("cleanup" `T.isInfixOf` T.toLower (diagMessage d))) diags `shouldBe` True

      describe "MonadError misuse" $ do
        it "detects liftIO throwIO in ExceptT" $ do
          let code = "action = liftIO $ throwIO SomeException"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "ExceptT" . diagMessage) diags `shouldBe` True

        it "detects verbose ExceptT construction" $ do
          let code = "action = ExceptT $ pure $ Left err"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "verbose" . diagMessage) diags `shouldBe` True

        it "detects chaining after runExceptT" $ do
          let code = "action = runExceptT computation >> doSomethingElse"
              diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
          length diags `shouldSatisfy` (> 0)

        it "respects ehCheckMonadError = False" $ do
          let config = defaultErrorHandlingConfig { ehCheckMonadError = False }
              code = "action = liftIO $ throwIO SomeException"
          let diags = detectErrorHandlingIssues config "test.hs" code
          all (\d -> not ("ExceptT" `T.isInfixOf` diagMessage d)) diags `shouldBe` True

    describe "ErrorHandlingCategory" $ do
      it "has all expected categories" $ do
        minBound `shouldBe` BroadException
        maxBound `shouldBe` MonadErrorMisuse

      it "has 6 categories" $ do
        let categories = [minBound..maxBound] :: [ErrorHandlingCategory]
        length categories `shouldBe` 6

    describe "diagnostic codes" $ do
      it "uses error-handling/ prefix for diagnostic codes" $ do
        let code = "action = catch doSomething (\\(e :: SomeException) -> handler)"
            diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
        all (maybe False (T.isPrefixOf "error-handling/") . diagCode) diags `shouldBe` True

    describe "comment handling" $ do
      it "ignores patterns in comments" $ do
        let code = "-- action = error \"This is a comment\""
            diags = detectErrorHandlingIssues defaultErrorHandlingConfig "test.hs" code
        -- error patterns should be ignored in comments
        all (\d -> not ("error" `T.isInfixOf` diagMessage d && "exception" `T.isInfixOf` T.toLower (diagMessage d))) diags `shouldBe` True
