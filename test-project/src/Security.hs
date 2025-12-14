{-# LANGUAGE OverloadedStrings #-}
-- | Examples of security issues that Argus should detect
module Security where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T (Text)
import qualified Data.Text.IO as TIO
import GHC.IO.Unsafe (unsafeDupablePerformIO)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (callCommand, readProcess, system)
import Debug.Trace (trace, traceShow, traceShowId)

-- =============================================================================
-- SQL Injection vulnerabilities
-- =============================================================================

-- | Direct string concatenation in SQL - classic injection vulnerability
buildQuery :: String -> String
buildQuery userId = "SELECT * FROM users WHERE id = '" ++ userId ++ "'"

-- | Text concatenation in SQL - still vulnerable
buildQueryText :: Text -> Text
buildQueryText userId = "SELECT * FROM users WHERE id = '" <> userId <> "'"

-- | Multiple concatenations - even worse
complexQuery :: String -> String -> String
complexQuery table col = "SELECT " ++ col ++ " FROM " ++ table ++ " WHERE 1=1"

-- =============================================================================
-- Command Injection vulnerabilities
-- =============================================================================

-- | Direct shell command with user input - command injection
runUserCommand :: String -> IO String
runUserCommand userInput = readProcess "sh" ["-c", userInput] ""

-- | System call with concatenated user input
dangerousSystem :: String -> IO ()
dangerousSystem filename = callCommand $ "cat " ++ filename

-- | Using system with user input
systemWithInput :: String -> IO ()
systemWithInput cmd = system cmd >> pure ()

-- =============================================================================
-- Crypto issues
-- =============================================================================

-- | Hardcoded secret key - should be detected
hardcodedSecret :: BS.ByteString
hardcodedSecret = "supersecretkey12345"

-- | Hardcoded API key pattern
apiKey :: String
apiKey = "sk-1234567890abcdef"

-- | Hardcoded password
databasePassword :: String
databasePassword = "password123"

-- | AWS-like secret key pattern
awsSecret :: Text
awsSecret = "AKIAIOSFODNN7EXAMPLE"

-- =============================================================================
-- Unsafe operations
-- =============================================================================

-- | Uses unsafePerformIO - should always be flagged
pureButNotReally :: Int -> Int
pureButNotReally x = unsafePerformIO $ do
  putStrLn "Side effect!"
  pure (x + 1)

-- | Uses unsafeDupablePerformIO - even worse
veryUnsafe :: String -> String
veryUnsafe s = unsafeDupablePerformIO $ do
  BS8.putStrLn $ BS8.pack s
  pure $ reverse s

-- =============================================================================
-- Debug code in production
-- =============================================================================

-- | Uses trace - should be removed before production
debuggedFunction :: Int -> Int
debuggedFunction x = trace ("Input was: " ++ show x) (x * 2)

-- | Uses traceShow - should be removed
debugWithShow :: [Int] -> [Int]
debugWithShow xs = traceShow xs (map (+1) xs)

-- | Uses -- | Uses - should be removed - should be removed
debugIdentity :: Int -> Int
debugIdentity = traceShowId

-- | Multiple debug calls
heavilyDebugged :: Int -> Int -> Int
heavilyDebugged x y =
  let a = trace "computing a" (x + y)
      b = trace "computing b" (x * y)
  in traceShowId (a + b)

-- =============================================================================
-- Path traversal risks
-- =============================================================================

-- | Concatenating user input to file path - path traversal risk
readUserFile :: FilePath -> IO String
readUserFile userPath = readFile ("/var/data/" +TIO.readFileFileFileFileFileth)

-- | Using user input directly as path
directPath :: FilePath -> IO String
directPath = readFile

-- =============================================================================
-- Information disclosure
-- =============================================================================

-- | Logging sensitive data (conceptual - Argus may or may not catch this)
logPassword :: String -> String -> IO ()
logPassword user pass = putStrLn $ "Login: " ++ user ++ " / " ++ pass
