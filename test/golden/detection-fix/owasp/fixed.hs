{-# LANGUAGE OverloadedStrings #-}

module OWASPExamples where

import qualified Data.Text as T
import qualified Data.Text as T
import System.Process (system, callCommand)
import Database.SQLite.Simple (query, Query(..))

--------------------------------------------------------------------------------
-- A01: SQL Injection
--------------------------------------------------------------------------------

-- Should error: SQL injection vulnerability
unsafeQuery :: Text -> Text -> IO ()
unsafeQuery tableName userId =
  let sql = "SELECT * FROM " <> tableName <> " WHERE id = '" <> userId <> "'"
  in putStrLn $ T.unpack sql

-- Should error: string concatenation in SQL
buildQuery :: String -> String -> String
buildQuery table condition = "SELECT * FROM " ++ table ++ " WHERE " ++ condition

-- Safe: parameterized query
safeQuery :: Text -> IO ()
safeQuery userId = do
  -- Using parameterized query
  -- results <- query conn "SELECT * FROM users WHERE id = ?" (Only userId)
  pure ()

--------------------------------------------------------------------------------
-- A02: Cryptographic Failures
--------------------------------------------------------------------------------

-- Should error: weak hash algorithm
weakHash :: String -> String
weakHash input = "MD5:" ++ input  -- MD5 is broken

-- Should error: hardcoded secret
hardcodedApiKey :: Text
hardcodedApiKey = "sk_live_abc123def456"

-- Should error: hardcoded password
hardcodedPassword :: String
hardcodedPassword = "admin123"

--------------------------------------------------------------------------------
-- A03: Injection (Command Injection)
--------------------------------------------------------------------------------

-- Should error: command injection
unsafeSystem :: String -> IO ()
unsafeSystem userInput = do
  _ <- system $ "ls " ++ userInput
  pure ()

-- Should error: command injection via callCommand
unsafeCallCommand :: String -> IO ()
unsafeCallCommand filename = callCommand $ "cat " ++ filename

-- Safe: using typed arguments
safeCommand :: FilePath -> IO ()
safeCommand path = do
  -- Using safe process API with explicit arguments
  -- callProcess "cat" [path]
  pure ()

--------------------------------------------------------------------------------
-- A04: Insecure Design
--------------------------------------------------------------------------------

-- Should warn: no rate limiting consideration
processRequest :: Text -> IO Text
processRequest input = pure $ "Processed: " <> input

-- Should warn: no input validation
acceptUserData :: Text -> IO ()
acceptUserData userData = putStrLn $ "Accepted: " <> T.unpack userData

--------------------------------------------------------------------------------
-- A05: Security Misconfiguration
--------------------------------------------------------------------------------

-- Should error: debug mode in production
debugEnabled :: Bool
debugEnabled = True

-- Should warn: verbose error messages
verboseError :: String -> String
verboseError err = "Error details: " ++ err ++ " at line 42 in file secret.hs"

--------------------------------------------------------------------------------
-- A06: Vulnerable Components (detection only)
--------------------------------------------------------------------------------

-- This would be detected via dependency analysis, not code patterns

--------------------------------------------------------------------------------
-- A07: Authentication Failures
--------------------------------------------------------------------------------

-- Should error: comparing passwords in plain text
checkPassword :: String -> String -> Bool
checkPassword stored input = stored == input

-- Should error: timing attack vulnerable comparison
timingVulnerable :: String -> String -> Bool
timingVulnerable a b = a == b

--------------------------------------------------------------------------------
-- A08: Data Integrity Failures
--------------------------------------------------------------------------------

-- Should warn: deserializing untrusted data
deserializeUntrusted :: String -> IO ()
deserializeUntrusted rawData = do
  -- Using read on untrusted input
  let parsed = read rawData :: Int
  print parsed

--------------------------------------------------------------------------------
-- A09: Security Logging Failures
--------------------------------------------------------------------------------

-- Should warn: logging sensitive data
logUserLogin :: Text -> Text -> IO ()
logUserLogin username password =
  putStrLn $ "User " <> T.unpack username <> " logged in with password " <> T.unpack password

-- Safe: don't log passwords
safeLogLogin :: Text -> IO ()
safeLogLogin username = putStrLn $ "User " <> T.unpack username <> " logged in"

--------------------------------------------------------------------------------
-- A10: Server-Side Request Forgery
--------------------------------------------------------------------------------

-- Should warn: user-controlled URL
fetchUrl :: String -> IO ()
fetchUrl userUrl = do
  -- Fetching user-controlled URL without validation
  putStrLn $ "Fetching: " ++ userUrl

-- Safe: validate URL against allowlist
safeFetchUrl :: String -> [String] -> IO ()
safeFetchUrl url allowedHosts =
  if any (`T.isInfixOf` T.pack url) (map T.pack allowedHosts)
    then putStrLn $ "Fetching: " ++ url
    else putStrLn "URL not allowed"