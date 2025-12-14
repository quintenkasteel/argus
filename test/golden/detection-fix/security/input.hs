{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module SecurityIssues where

import System.IO.Unsafe (unsafePerformIO)
import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)
import System.Random (randomRIO)
import System.FilePath ((</>))
import Debug.Trace (trace, traceShow, traceShowId, traceId, traceM)
import Data.Text (Text)
import Data.Text qualified as T

-- 1. Unsafe Functions
unsafeExample :: String -> Int
unsafeExample str = unsafePerformIO $ do
  putStrLn str
  return 42

-- 2. Unsafe Coercion (bypasses type system)
coerceExample :: Int -> String
coerceExample n = unsafeCoerce n

-- 3. SQL Injection Risk (string concatenation in query)
sqlInjectionExample :: String -> String
sqlInjectionExample userId =
  let query = "SELECT * FROM users WHERE id = '" ++ userId ++ "'"
  in query

-- SQL with execute pattern
executeExample :: String -> String
executeExample cmd =
  let execute = "DELETE FROM users WHERE name = " ++ cmd
  in execute

-- 4. Shell Command Injection
shellInjectionExample :: String -> String
shellInjectionExample userInput =
  let shell = "echo " ++ userInput
  in shell

-- Shell with system pattern
systemExample :: String -> String
systemExample arg =
  let system = "rm -rf " ++ arg
  in system

-- 5. Path Traversal Risk
pathTraversalExample :: String -> FilePath
pathTraversalExample userPath =
  let path = "/data" </> userPath
  in path

-- 6. Hardcoded Secret
apiKeyExample :: String
apiKeyExample = "sk-1234567890abcdef"

passwordExample :: String
passwordExample = "admin123"

secretTokenExample :: String
secretTokenExample = "secret_key_xyz"

-- 7. Weak Cryptography patterns
md5Example :: String -> String
md5Example input = "MD5 hash of " ++ input

sha1Example :: String -> String
sha1Example input = "SHA1 hash of " ++ input

desExample :: String -> String
desExample input = "DES encrypt " ++ input

-- 8. Insecure Random (System.Random for security)
tokenGenerator :: IO String
tokenGenerator = do
  n <- randomRIO (0, 999999 :: Int)
  return $ "token-" ++ show n

-- 9. Debug Code (Debug.Trace usage)
debugExample1 :: Int -> Int
debugExample1 x = trace "Debug: calculating" (x + 1)

debugExample2 :: Int -> Int
debugExample2 x = traceShow x (x * 2)

debugExample3 :: Int -> Int
debugExample3 = traceShowId

debugExample4 :: String -> String
debugExample4 = traceId

debugExample5 :: IO ()
debugExample5 = traceM "Debug message in IO"

-- 10. Security TODO comment
-- TODO: Add proper authentication and encryption before production deployment

-- 11. XSS Risk (unescaped HTML)
htmlExample :: String -> String
htmlExample userInput = toHtml $ userInput
  where toHtml x = "<div>" ++ x ++ "</div>"

-- 12. Unsafe FFI with pure type
foreign import ccall unsafe "abs"
  c_abs_pure :: Int -> Int
