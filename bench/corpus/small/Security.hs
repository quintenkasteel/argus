-- | Module with security-sensitive patterns
module Security where

import System.IO.Unsafe (unsafePerformIO)
import System.Process (callCommand)

-- | Uses unsafePerformIO (security issue)
unsafeRead :: FilePath -> String
unsafeRead path = unsafePerformIO $ readFile path

-- | Command execution (security issue)
runUserCommand :: String -> IO ()
runUserCommand cmd = callCommand cmd

-- | Unsafe coercion example
unsafeCoerceExample :: a -> b
unsafeCoerceExample = unsafeCoerce
  where unsafeCoerce _ = error "fake unsafeCoerce"

-- | Debug trace in production code
debugValue :: Show a => a -> a
debugValue x = trace (show x) x
  where trace _ y = y  -- fake trace

-- | Hardcoded credential (security issue)
apiKey :: String
apiKey = "sk-secret-api-key-12345"

-- | SQL injection vulnerability pattern
buildQuery :: String -> String
buildQuery userInput = "SELECT * FROM users WHERE name = '" ++ userInput ++ "'"
