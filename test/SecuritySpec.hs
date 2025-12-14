{-# LANGUAGE OverloadedStrings #-}

module SecuritySpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Argus.Rules.Security
import Argus.Types (Diagnostic(..), Severity(..))

spec :: Spec
spec = do
  describe "Argus.Rules.Security" $ do
    describe "defaultSecurityConfig" $ do
      it "has enabled set to True by default" $ do
        scEnabled defaultSecurityConfig `shouldBe` True

      it "has all checks enabled by default" $ do
        scCheckUnsafe defaultSecurityConfig `shouldBe` True
        scCheckInjection defaultSecurityConfig `shouldBe` True
        scCheckCrypto defaultSecurityConfig `shouldBe` True
        scCheckSecrets defaultSecurityConfig `shouldBe` True
        scCheckFFI defaultSecurityConfig `shouldBe` True
        scCheckDebug defaultSecurityConfig `shouldBe` True

      it "allows unsafe in tests by default" $ do
        scAllowUnsafeInTest defaultSecurityConfig `shouldBe` True

    describe "detectSecurityIssues" $ do
      describe "when disabled" $ do
        it "returns empty list" $ do
          let config = defaultSecurityConfig { scEnabled = False }
              code = "unsafePerformIO $ readFile \"secret\""
          detectSecurityIssues config "test.hs" code `shouldBe` []

      describe "unsafe functions" $ do
        it "detects unsafePerformIO" $ do
          let code = "result = unsafePerformIO $ readFile \"file\""
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "unsafePerformIO" . diagMessage) diags `shouldBe` True

        it "detects unsafeInterleaveIO" $ do
          let code = "lazyRead = unsafeInterleaveIO getContents"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "unsafeInterleaveIO" . diagMessage) diags `shouldBe` True

        it "detects unsafeDupablePerformIO" $ do
          let code = "result = unsafeDupablePerformIO action"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects unsafeCoerce" $ do
          let code = "converted = unsafeCoerce value"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "unsafeCoerce" . diagMessage) diags `shouldBe` True

        it "detects inlinePerformIO" $ do
          let code = "result = inlinePerformIO action"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects accursedUnutterablePerformIO" $ do
          let code = "result = accursedUnutterablePerformIO action"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)

        it "skips unsafe functions in test files when configured" $ do
          let code = "result = unsafePerformIO action"
              config = defaultSecurityConfig { scAllowUnsafeInTest = True }
              diags = detectSecurityIssues config "test/MySpec.hs" code
          -- Should skip UnsafeFunction category in test files
          diags `shouldBe` []

        it "reports unsafe functions in test files when not allowed" $ do
          let code = "result = unsafePerformIO action"
              config = defaultSecurityConfig { scAllowUnsafeInTest = False }
              diags = detectSecurityIssues config "test/MySpec.hs" code
          length diags `shouldSatisfy` (> 0)

      describe "injection risks" $ do
        it "detects SQL injection with string concatenation" $ do
          let code = "query db $ \"SELECT * FROM users WHERE id = \" ++ userId"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "SQL injection" . diagMessage) diags `shouldBe` True

        it "detects shell command injection" $ do
          let code = "callCommand $ \"rm -rf \" ++ userInput"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "shell" . diagMessage) diags `shouldBe` True

        it "detects path traversal risk" $ do
          let code = "readFile $ baseDir </> userInput"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "path traversal" . diagMessage) diags `shouldBe` True

        it "detects XSS with unescaped HTML" $ do
          let code = "toHtml $ userInput"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "XSS" . diagMessage) diags `shouldBe` True

      describe "cryptographic issues" $ do
        it "detects MD5 usage" $ do
          let code = "hash = MD5.hash password"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "MD5" . diagMessage) diags `shouldBe` True

        it "detects SHA1 usage" $ do
          let code = "hash = SHA1.hash data"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "SHA-1" . diagMessage) diags `shouldBe` True

        it "detects DES usage" $ do
          let code = "encrypted = DES.encrypt key plaintext"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "DES" . diagMessage) diags `shouldBe` True

        it "does not flag 3DES as insecure DES" $ do
          let code = "encrypted = 3DES.encrypt key plaintext"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          -- Should not have DES warning for 3DES
          any (T.isInfixOf "DES is insecure" . diagMessage) diags `shouldBe` False

      describe "hardcoded secrets" $ do
        it "detects hardcoded password" $ do
          let code = "password = \"supersecret123\""
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "hardcoded" . diagMessage) diags `shouldBe` True

        it "detects hardcoded API key" $ do
          let code = "apiKey = \"sk_live_abc123xyz\""
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects hardcoded token" $ do
          let code = "authToken = \"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9\""
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)

        it "does not flag non-secret strings" $ do
          let code = "greeting = \"Hello, World!\""
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          any (T.isInfixOf "hardcoded" . diagMessage) diags `shouldBe` False

      describe "unsafe FFI" $ do
        it "detects FFI import with pure type" $ do
          let code = "foreign import ccall \"strlen\" c_strlen :: CString -> CSize"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "FFI" . diagMessage) diags `shouldBe` True

        it "detects unsafe FFI call" $ do
          let code = "foreign import ccall unsafe \"fast_op\" c_fast :: Int -> IO Int"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)

        it "does not flag safe FFI with IO" $ do
          let code = "foreign import ccall safe \"slow_op\" c_slow :: Int -> IO Int"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          -- Should not have FFI warning for safe IO import
          diags `shouldBe` []

      describe "debug code" $ do
        it "detects Debug.Trace import" $ do
          let code = "import Debug.Trace"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "trace" . diagMessage) diags `shouldBe` True

        it "detects trace function usage" $ do
          let code = "result = trace \"debug\" value"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects traceShow usage" $ do
          let code = "result = traceShow value expr"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects traceM usage" $ do
          let code = "do { traceM \"debug message\"; action }"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects security-related TODO" $ do
          let code = "-- TODO: fix security vulnerability in auth"
              diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "TODO" . diagMessage) diags `shouldBe` True

        it "skips debug code in test files when configured" $ do
          let code = "result = trace \"debug\" value"
              config = defaultSecurityConfig { scAllowUnsafeInTest = True }
              diags = detectSecurityIssues config "test/MySpec.hs" code
          -- Should skip DebugCode category in test files
          diags `shouldBe` []

      describe "configuration options" $ do
        it "respects scCheckUnsafe = False" $ do
          let code = "result = unsafePerformIO action"
              config = defaultSecurityConfig { scCheckUnsafe = False }
              diags = detectSecurityIssues config "src/Main.hs" code
          diags `shouldBe` []

        it "respects scCheckInjection = False" $ do
          let code = "query db $ \"SELECT * FROM users WHERE id = \" ++ userId"
              config = defaultSecurityConfig { scCheckInjection = False }
              diags = detectSecurityIssues config "src/Main.hs" code
          diags `shouldBe` []

        it "respects scCheckCrypto = False" $ do
          let code = "hash = MD5.hash password"
              config = defaultSecurityConfig { scCheckCrypto = False }
              diags = detectSecurityIssues config "src/Main.hs" code
          diags `shouldBe` []

        it "respects scCheckSecrets = False" $ do
          let code = "password = \"supersecret123\""
              config = defaultSecurityConfig { scCheckSecrets = False }
              diags = detectSecurityIssues config "src/Main.hs" code
          diags `shouldBe` []

        it "respects scCheckFFI = False" $ do
          let code = "foreign import ccall \"strlen\" c_strlen :: CString -> CSize"
              config = defaultSecurityConfig { scCheckFFI = False }
              diags = detectSecurityIssues config "src/Main.hs" code
          diags `shouldBe` []

        it "respects scCheckDebug = False" $ do
          let code = "import Debug.Trace"
              config = defaultSecurityConfig { scCheckDebug = False }
              diags = detectSecurityIssues config "src/Main.hs" code
          diags `shouldBe` []

    describe "SecurityCategory" $ do
      it "has all expected categories" $ do
        -- Just verify the enum is complete by checking bounds
        minBound `shouldBe` UnsafeFunction
        maxBound `shouldBe` TemplateHaskellRisk

    describe "diagnostic codes" $ do
      it "includes CWE identifiers where applicable" $ do
        let code = "result = unsafePerformIO action"
            diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
        length diags `shouldSatisfy` (> 0)
        -- Check that CWE is included in the code
        any (maybe False (T.isInfixOf "CWE-") . diagCode) diags `shouldBe` True

      it "uses security/ prefix for all codes" $ do
        let code = T.unlines
              [ "result = unsafePerformIO action"
              , "hash = MD5.hash data"
              ]
            diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
        all (maybe False (T.isPrefixOf "security/") . diagCode) diags `shouldBe` True

    describe "severity levels" $ do
      it "assigns Error severity to unsafeCoerce" $ do
        let code = "converted = unsafeCoerce value"
            diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
        any (\d -> diagSeverity d == Error) diags `shouldBe` True

      it "assigns Warning severity to unsafePerformIO" $ do
        let code = "result = unsafePerformIO action"
            diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
        any (\d -> diagSeverity d == Warning) diags `shouldBe` True

      it "assigns Error severity to SQL injection" $ do
        let code = "query db $ \"SELECT * FROM users WHERE id = \" ++ userId"
            diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
        any (\d -> diagSeverity d == Error) diags `shouldBe` True

      it "assigns Error severity to hardcoded secrets" $ do
        let code = "password = \"supersecret123\""
            diags = detectSecurityIssues defaultSecurityConfig "src/Main.hs" code
        any (\d -> diagSeverity d == Error) diags `shouldBe` True

    ---------------------------------------------------------------------------
    -- P1-03: Security all-disabled tests
    ---------------------------------------------------------------------------

    describe "all-disabled configuration" $ do
      it "returns empty list when all checks are disabled" $ do
        let config = defaultSecurityConfig
              { scEnabled = True  -- Module enabled, but all individual checks disabled
              , scCheckUnsafe = False
              , scCheckInjection = False
              , scCheckCrypto = False
              , scCheckSecrets = False
              , scCheckFFI = False
              , scCheckDebug = False
              }
            code = T.unlines
              [ "result = unsafePerformIO action"
              , "query db $ \"SELECT * FROM users WHERE id = \" ++ userId"
              , "hash = MD5.hash password"
              , "password = \"supersecret123\""
              , "foreign import ccall \"strlen\" c_strlen :: CString -> CSize"
              , "import Debug.Trace"
              ]
            diags = detectSecurityIssues config "src/Main.hs" code
        diags `shouldBe` []

      it "returns empty list when module is disabled" $ do
        let config = defaultSecurityConfig { scEnabled = False }
            code = T.unlines
              [ "result = unsafePerformIO action"
              , "query db $ \"SELECT * FROM users WHERE id = \" ++ userId"
              , "hash = MD5.hash password"
              , "password = \"supersecret123\""
              ]
            diags = detectSecurityIssues config "src/Main.hs" code
        diags `shouldBe` []

      it "enables only unsafe checks when only scCheckUnsafe is True" $ do
        let config = defaultSecurityConfig
              { scEnabled = True
              , scCheckUnsafe = True
              , scCheckInjection = False
              , scCheckCrypto = False
              , scCheckSecrets = False
              , scCheckFFI = False
              , scCheckDebug = False
              }
            code = T.unlines
              [ "result = unsafePerformIO action"
              , "query db $ \"SELECT * FROM users WHERE id = \" ++ userId"
              , "hash = MD5.hash password"
              ]
            diags = detectSecurityIssues config "src/Main.hs" code
        -- Should only detect unsafe functions, not injection or crypto
        length diags `shouldBe` 1
        any (T.isInfixOf "unsafePerformIO" . diagMessage) diags `shouldBe` True
        any (T.isInfixOf "SQL injection" . diagMessage) diags `shouldBe` False
        any (T.isInfixOf "MD5" . diagMessage) diags `shouldBe` False

      it "enables only injection checks when only scCheckInjection is True" $ do
        let config = defaultSecurityConfig
              { scEnabled = True
              , scCheckUnsafe = False
              , scCheckInjection = True
              , scCheckCrypto = False
              , scCheckSecrets = False
              , scCheckFFI = False
              , scCheckDebug = False
              }
            code = T.unlines
              [ "result = unsafePerformIO action"
              , "query db $ \"SELECT * FROM users WHERE id = \" ++ userId"
              , "hash = MD5.hash password"
              ]
            diags = detectSecurityIssues config "src/Main.hs" code
        -- Should only detect injection, not unsafe or crypto
        any (T.isInfixOf "SQL injection" . diagMessage) diags `shouldBe` True
        any (T.isInfixOf "unsafePerformIO" . diagMessage) diags `shouldBe` False
        any (T.isInfixOf "MD5" . diagMessage) diags `shouldBe` False

      it "enables only crypto checks when only scCheckCrypto is True" $ do
        let config = defaultSecurityConfig
              { scEnabled = True
              , scCheckUnsafe = False
              , scCheckInjection = False
              , scCheckCrypto = True
              , scCheckSecrets = False
              , scCheckFFI = False
              , scCheckDebug = False
              }
            code = T.unlines
              [ "result = unsafePerformIO action"
              , "query db $ \"SELECT * FROM users WHERE id = \" ++ userId"
              , "hash = MD5.hash password"
              ]
            diags = detectSecurityIssues config "src/Main.hs" code
        -- Should only detect crypto, not unsafe or injection
        any (T.isInfixOf "MD5" . diagMessage) diags `shouldBe` True
        any (T.isInfixOf "unsafePerformIO" . diagMessage) diags `shouldBe` False
        any (T.isInfixOf "SQL injection" . diagMessage) diags `shouldBe` False

      it "enables only secrets checks when only scCheckSecrets is True" $ do
        let config = defaultSecurityConfig
              { scEnabled = True
              , scCheckUnsafe = False
              , scCheckInjection = False
              , scCheckCrypto = False
              , scCheckSecrets = True
              , scCheckFFI = False
              , scCheckDebug = False
              }
            code = T.unlines
              [ "result = unsafePerformIO action"
              , "password = \"supersecret123\""
              ]
            diags = detectSecurityIssues config "src/Main.hs" code
        -- Should only detect hardcoded secrets
        any (T.isInfixOf "hardcoded" . diagMessage) diags `shouldBe` True
        any (T.isInfixOf "unsafePerformIO" . diagMessage) diags `shouldBe` False

      it "enables only FFI checks when only scCheckFFI is True" $ do
        let config = defaultSecurityConfig
              { scEnabled = True
              , scCheckUnsafe = False
              , scCheckInjection = False
              , scCheckCrypto = False
              , scCheckSecrets = False
              , scCheckFFI = True
              , scCheckDebug = False
              }
            code = T.unlines
              [ "result = unsafePerformIO action"
              , "foreign import ccall \"strlen\" c_strlen :: CString -> CSize"
              ]
            diags = detectSecurityIssues config "src/Main.hs" code
        -- Should only detect FFI issues
        any (T.isInfixOf "FFI" . diagMessage) diags `shouldBe` True
        any (T.isInfixOf "unsafePerformIO" . diagMessage) diags `shouldBe` False

      it "enables only debug checks when only scCheckDebug is True" $ do
        let config = defaultSecurityConfig
              { scEnabled = True
              , scCheckUnsafe = False
              , scCheckInjection = False
              , scCheckCrypto = False
              , scCheckSecrets = False
              , scCheckFFI = False
              , scCheckDebug = True
              }
            code = T.unlines
              [ "result = unsafePerformIO action"
              , "import Debug.Trace"
              ]
            diags = detectSecurityIssues config "src/Main.hs" code
        -- Should only detect debug code
        any (T.isInfixOf "trace" . diagMessage) diags `shouldBe` True
        any (T.isInfixOf "unsafePerformIO" . diagMessage) diags `shouldBe` False

      it "detects all issue types when all checks enabled" $ do
        let config = defaultSecurityConfig
            code = T.unlines
              [ "result = unsafePerformIO action"
              , "query db $ \"SELECT * FROM users WHERE id = \" ++ userId"
              , "hash = MD5.hash password"
              , "password = \"supersecret123\""
              , "import Debug.Trace"
              ]
            diags = detectSecurityIssues config "src/Main.hs" code
        -- Should detect multiple types
        length diags `shouldSatisfy` (>= 4)
        any (T.isInfixOf "unsafePerformIO" . diagMessage) diags `shouldBe` True
        any (T.isInfixOf "SQL injection" . diagMessage) diags `shouldBe` True
        any (T.isInfixOf "MD5" . diagMessage) diags `shouldBe` True
        any (T.isInfixOf "hardcoded" . diagMessage) diags `shouldBe` True
