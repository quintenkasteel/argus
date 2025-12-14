{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Security
-- Description : Security-focused lint rules for Haskell code
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- This module provides lint rules for detecting potential security issues
-- in Haskell code. These rules focus on unsafe operations, data validation,
-- and common vulnerability patterns.
--
-- == Rule Categories
--
-- * __Unsafe Operations__: Type-unsafe coercions and memory operations
-- * __Input Validation__: SQL injection, path traversal, etc.
-- * __Secrets Management__: Hardcoded credentials, insecure storage
-- * __Cryptography__: Weak algorithms, improper usage
--
-- == Security Considerations
--
-- These rules are heuristic-based and cannot guarantee security:
--
-- * False positives: Safe code may be flagged
-- * False negatives: Not all vulnerabilities will be detected
-- * Context matters: Some flagged patterns are safe in specific contexts
--
-- Always perform thorough security reviews beyond automated linting.
--
-- == References
--
-- * <https://owasp.org/www-project-top-ten/ OWASP Top Ten>
-- * <https://wiki.haskell.org/Avoiding_IO Avoiding IO>
-- * <https://hackage.haskell.org/package/safe Haskell Safe Library>

module Argus.Rules.Builtin.Security
  ( -- * Rule Sets
    securityRules
  , unsafeOpRules
  , injectionRules
  , secretsRules
  , cryptoRules

    -- * Individual Rules
    -- ** Unsafe Operations
  , avoidUnsafeCoerce
  , avoidUnsafePerformIO
  , avoidUnsafeInterleaveIO
  , avoidUnsafeDupablePerformIO
  , avoidAccursedUnutterablePerformIO
  , avoidInlinePerformIO
  , avoidUnsafeFixIO
  , avoidUnsafeLocalState

    -- ** Injection Vulnerabilities
  , avoidRawSql
  , avoidStringInterpolationSql
  , avoidSystemCommand
  , avoidShellCommand
  , avoidUncheckedPath

    -- ** Secrets Management
  , avoidHardcodedPassword
  , avoidHardcodedSecret
  , avoidPlaintextCredentials

    -- ** Cryptography
  , avoidMD5
  , avoidSHA1
  , avoidDES
  , avoidECB
  , avoidWeakRandom

    -- * Rule Metadata
  , securityRuleCount
  ) where

import Data.Text (Text)
import Argus.Rules.DSL
import Argus.Types (Severity(..))

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All security rules combined.
--
-- This comprehensive set should be enabled for any code handling:
--
-- * User input
-- * External data
-- * Sensitive information
-- * Network communication
securityRules :: [Rule]
securityRules = unsafeOpRules ++ injectionRules ++ secretsRules ++ cryptoRules

-- | Rules for detecting unsafe operations.
--
-- These operations bypass type safety or purity guarantees.
-- Use only when absolutely necessary and with extreme care.
unsafeOpRules :: [Rule]
unsafeOpRules =
  [ avoidUnsafeCoerce
  , avoidUnsafePerformIO
  , avoidUnsafeInterleaveIO
  , avoidUnsafeDupablePerformIO
  , avoidAccursedUnutterablePerformIO
  , avoidInlinePerformIO
  , avoidUnsafeFixIO
  , avoidUnsafeLocalState
  ]

-- | Rules for detecting injection vulnerabilities.
--
-- These rules flag patterns that may lead to:
--
-- * SQL injection
-- * Command injection
-- * Path traversal
injectionRules :: [Rule]
injectionRules =
  [ avoidRawSql
  , avoidStringInterpolationSql
  , avoidSystemCommand
  , avoidShellCommand
  , avoidUncheckedPath
  ]

-- | Rules for secrets management.
--
-- Hardcoded credentials and secrets should never appear in source code.
secretsRules :: [Rule]
secretsRules =
  [ avoidHardcodedPassword
  , avoidHardcodedSecret
  , avoidPlaintextCredentials
  ]

-- | Rules for cryptographic best practices.
--
-- These rules flag weak or outdated cryptographic algorithms.
cryptoRules :: [Rule]
cryptoRules =
  [ avoidMD5
  , avoidSHA1
  , avoidDES
  , avoidECB
  , avoidWeakRandom
  ]

--------------------------------------------------------------------------------
-- Unsafe Operations
--------------------------------------------------------------------------------

-- | Detect use of 'unsafeCoerce'.
--
-- == Problem
--
-- 'unsafeCoerce' bypasses the type system entirely, potentially causing:
--
-- * Segmentation faults
-- * Memory corruption
-- * Undefined behavior
--
-- @
-- unsafeCoerce (1 :: Int) :: String  -- Disaster!
-- @
--
-- == Solution
--
-- Use type-safe alternatives:
--
-- @
-- -- Use type classes
-- class Convert a b where
--   convert :: a -> b
--
-- -- Use Data.Coerce for newtypes
-- coerce :: Coercible a b => a -> b
--
-- -- Use explicit conversion functions
-- show :: Show a => a -> String
-- @
--
-- == When unsafeCoerce Might Be Justified
--
-- * Internal implementation of type-safe interfaces
-- * Performance-critical FFI code
-- * GHC internals and compiler extensions
--
-- Even then, document extensively and test thoroughly.
avoidUnsafeCoerce :: Rule
avoidUnsafeCoerce = rule "security/avoid-unsafeCoerce" $
  match (pat "unsafeCoerce"
         `fromModule` "Unsafe.Coerce")
  & severity Error
  & message "unsafeCoerce bypasses type safety - can cause crashes and undefined behavior"
  & note "Use type classes, Data.Coerce for newtypes, or explicit conversion functions"
  & category Security
  & safetyLevel Unsafe

-- | Detect use of 'unsafePerformIO'.
--
-- == Problem
--
-- 'unsafePerformIO' breaks referential transparency:
--
-- @
-- import System.IO.Unsafe
--
-- bad :: Int
-- bad = unsafePerformIO (readFile \"count.txt\" >>= pure . read)
-- -- This can be evaluated 0, 1, or many times depending on optimization!
-- @
--
-- == Solution
--
-- Keep IO in IO:
--
-- @
-- good :: IO Int
-- good = read <$> readFile \"count.txt\"
-- @
--
-- == Limited Safe Uses
--
-- Some established patterns are considered safe:
--
-- * Global mutable state with NOINLINE pragma
-- * Pure interface to referentially transparent IO (e.g., memoization)
--
-- Always use 'unsafePerformIO' with NOINLINE to prevent CSE/inlining issues.
avoidUnsafePerformIO :: Rule
avoidUnsafePerformIO = rule "security/avoid-unsafePerformIO" $
  match (pat "unsafePerformIO"
         `fromModule` "System.IO.Unsafe")
  & severity Error
  & message "unsafePerformIO breaks referential transparency - use IO monad instead"
  & note "If necessary, use NOINLINE pragma and ensure the IO is truly pure"
  & category Security
  & safetyLevel Unsafe

-- | Detect use of 'unsafeInterleaveIO'.
--
-- == Problem
--
-- 'unsafeInterleaveIO' defers IO actions lazily, causing:
--
-- * Non-deterministic evaluation order
-- * Resource leaks (file handles kept open)
-- * Debugging nightmares
--
-- @
-- -- File handle may never be closed!
-- contents <- unsafeInterleaveIO (readFile \"large.txt\")
-- @
--
-- == Solution
--
-- Use streaming libraries for lazy IO:
--
-- @
-- import Conduit
-- import Streaming
-- @
--
-- Or use strict IO with explicit chunking.
avoidUnsafeInterleaveIO :: Rule
avoidUnsafeInterleaveIO = rule "security/avoid-unsafeInterleaveIO" $
  match (pat "unsafeInterleaveIO"
         `fromModule` "System.IO.Unsafe")
  & severity Warning
  & message "unsafeInterleaveIO causes non-deterministic IO ordering and resource leaks"
  & note "Use streaming libraries (conduit, streaming) for lazy IO needs"
  & category Security
  & safetyLevel Unsafe

-- | Detect use of 'unsafeDupablePerformIO'.
--
-- == Problem
--
-- Even more dangerous than 'unsafePerformIO' - can be duplicated across threads.
avoidUnsafeDupablePerformIO :: Rule
avoidUnsafeDupablePerformIO = rule "security/avoid-unsafeDupablePerformIO" $
  match (pat "unsafeDupablePerformIO"
         `fromModule` "System.IO.Unsafe")
  & severity Error
  & message "unsafeDupablePerformIO can duplicate effects across threads"
  & note "Even more dangerous than unsafePerformIO - effects may run multiple times"
  & category Security
  & safetyLevel Unsafe

-- | Detect the most dangerous unsafe function.
avoidAccursedUnutterablePerformIO :: Rule
avoidAccursedUnutterablePerformIO = rule "security/avoid-accursedUnutterablePerformIO" $
  match (pat "accursedUnutterablePerformIO"
         `fromModule` "Data.ByteString.Internal")
  & severity Error
  & message "accursedUnutterablePerformIO is extremely dangerous - the name is a warning"
  & note "Only for GHC/base internals. If you need this, you're likely doing something wrong"
  & category Security
  & safetyLevel Unsafe

-- | Detect inlinePerformIO from bytestring internals.
avoidInlinePerformIO :: Rule
avoidInlinePerformIO = rule "security/avoid-inlinePerformIO" $
  match (pat "inlinePerformIO"
         `fromModule` "Data.ByteString.Internal")
  & severity Error
  & message "inlinePerformIO is for library internals only"
  & note "Use unsafePerformIO with NOINLINE if you must escape IO"
  & category Security
  & safetyLevel Unsafe

-- | Detect unsafeFixIO usage.
avoidUnsafeFixIO :: Rule
avoidUnsafeFixIO = rule "security/avoid-unsafeFixIO" $
  match (pat "unsafeFixIO"
         `fromModule` "System.IO.Unsafe")
  & severity Warning
  & message "unsafeFixIO can cause non-termination and is hard to reason about"
  & note "Use mfix from Control.Monad.Fix for safe recursive IO"
  & category Security
  & safetyLevel Unsafe

-- | Detect unsafeLocalState usage.
avoidUnsafeLocalState :: Rule
avoidUnsafeLocalState = rule "security/avoid-unsafeLocalState" $
  match (pat "unsafeLocalState")
  & severity Error
  & message "unsafeLocalState is an internal function that breaks safety guarantees"
  & note "Use proper ST monad for local mutable state"
  & category Security
  & safetyLevel Unsafe

--------------------------------------------------------------------------------
-- Injection Vulnerabilities
--------------------------------------------------------------------------------

-- | Detect potential SQL injection via rawSql.
--
-- == Problem
--
-- Using 'rawSql' with string concatenation allows SQL injection:
--
-- @
-- -- VULNERABLE!
-- rawSql (\"SELECT * FROM users WHERE name = '\" <> userName <> \"'\") []
-- @
--
-- == Solution
--
-- Always use parameterized queries:
--
-- @
-- -- Safe: parameterized
-- rawSql \"SELECT * FROM users WHERE name = ?\" [PersistText userName]
--
-- -- Better: use esqueleto or persistent DSL
-- select $ from $ \\user -> do
--   where_ (user ^. UserName ==. val userName)
--   return user
-- @
avoidRawSql :: Rule
avoidRawSql = rule "security/avoid-rawSql-interpolation" $
  match (pat "rawSql"
         `fromModule` "Database.Persist.Sql")
  & severity Warning
  & message "rawSql can lead to SQL injection if query is built from user input"
  & note "Use parameterized queries or esqueleto/persistent DSL for type-safe queries"
  & category Security
  & safetyLevel ManualReview

-- | Detect string interpolation in SQL contexts.
avoidStringInterpolationSql :: Rule
avoidStringInterpolationSql = rule "security/avoid-sql-string-concat" $
  match (pat "execute"
         `fromModule` "Database.PostgreSQL.Simple")
  & severity Warning
  & message "Ensure SQL queries use parameterized arguments, not string interpolation"
  & note "Use Query type with ? placeholders and pass parameters separately"
  & category Security
  & safetyLevel ManualReview

-- | Detect system command execution.
--
-- == Problem
--
-- 'system' executes shell commands, enabling command injection:
--
-- @
-- -- VULNERABLE!
-- system (\"rm -rf \" ++ userInput)
-- @
--
-- == Solution
--
-- Use typed-process or process with argument lists:
--
-- @
-- import System.Process.Typed
--
-- -- Safe: arguments are separate, not shell-interpreted
-- runProcess_ $ proc \"rm\" [\"-rf\", safeDir]
-- @
avoidSystemCommand :: Rule
avoidSystemCommand = rule "security/avoid-system" $
  match (pat "system"
         `fromModule` "System.Process")
  & severity Warning
  & message "system executes shell commands - vulnerable to command injection"
  & note "Use typed-process or callProcess with explicit argument list"
  & category Security
  & safetyLevel ManualReview

-- | Detect shell command execution.
avoidShellCommand :: Rule
avoidShellCommand = rule "security/avoid-shell" $
  match (pat "shell"
         `fromModule` "System.Process")
  & severity Warning
  & message "shell invokes system shell - vulnerable to command injection"
  & note "Use proc with explicit argument list instead of shell string"
  & category Security
  & safetyLevel ManualReview

-- | Detect unchecked file path construction.
--
-- == Problem
--
-- Direct path concatenation can enable path traversal attacks:
--
-- @
-- -- VULNERABLE!
-- readFile (\"uploads/\" ++ userFilename)
-- -- User could pass \"../../../etc/passwd\"
-- @
--
-- == Solution
--
-- Use path validation:
--
-- @
-- import System.FilePath (takeFileName, (</>))
--
-- -- Safe: strip directory components
-- let safeName = takeFileName userFilename
-- readFile (\"uploads\" </> safeName)
-- @
avoidUncheckedPath :: Rule
avoidUncheckedPath = rule "security/check-filepath" $
  match (pat "</>"
         `fromModule` "System.FilePath")
  & severity Info
  & message "Ensure file paths from user input are validated to prevent path traversal"
  & note "Use takeFileName to strip directory components from untrusted input"
  & category Security
  & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Secrets Management
--------------------------------------------------------------------------------

-- | Detect hardcoded passwords.
--
-- == Problem
--
-- Hardcoded credentials in source code:
--
-- * Get committed to version control
-- * Are visible to anyone with code access
-- * Are difficult to rotate
--
-- == Solution
--
-- Use environment variables or secret management:
--
-- @
-- import System.Environment (getEnv)
--
-- password <- getEnv \"DB_PASSWORD\"
--
-- -- Or use a secrets manager
-- import AWS.SecretsManager
-- @
avoidHardcodedPassword :: Rule
avoidHardcodedPassword = rule "security/no-hardcoded-password" $
  match (pat "password = \"")
  & severity Error
  & message "Hardcoded password detected - use environment variables or secrets manager"
  & note "Never commit credentials to source control"
  & category Security
  & safetyLevel ManualReview

-- | Detect hardcoded secrets or API keys.
avoidHardcodedSecret :: Rule
avoidHardcodedSecret = rule "security/no-hardcoded-secret" $
  match (pat "apiKey = \"")
  & severity Error
  & message "Hardcoded API key detected - use environment variables or secrets manager"
  & note "Rotate any exposed keys immediately"
  & category Security
  & safetyLevel ManualReview

-- | Detect plaintext credential storage.
avoidPlaintextCredentials :: Rule
avoidPlaintextCredentials = rule "security/no-plaintext-credentials" $
  match (pat "credentials = \"")
  & severity Error
  & message "Plaintext credentials detected - use secure credential storage"
  & note "Hash passwords with bcrypt/argon2, use env vars for API keys"
  & category Security
  & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Cryptography
--------------------------------------------------------------------------------

-- | Detect MD5 usage.
--
-- == Problem
--
-- MD5 is cryptographically broken:
--
-- * Collision attacks are practical
-- * Should never be used for security purposes
--
-- @
-- -- INSECURE
-- import Crypto.Hash.MD5
--
-- hash password  -- MD5 is broken!
-- @
--
-- == Solution
--
-- Use modern hash functions:
--
-- @
-- -- For passwords
-- import Crypto.BCrypt
-- hashPasswordUsingPolicy slowerBcryptHashingPolicy password
--
-- -- For general hashing
-- import Crypto.Hash (SHA256, hash)
-- @
avoidMD5 :: Rule
avoidMD5 = rule "security/avoid-MD5" $
  match (pat "hash"
         `fromModule` "Crypto.Hash.MD5")
  & severity Error
  & message "MD5 is cryptographically broken - use SHA-256 or better"
  & note "For passwords, use bcrypt or argon2. For integrity, use SHA-256+"
  & category Security
  & safetyLevel Unsafe

-- | Detect SHA1 usage.
--
-- == Problem
--
-- SHA-1 has known collision attacks and is deprecated for security use.
--
-- == Solution
--
-- Use SHA-256 or SHA-3.
avoidSHA1 :: Rule
avoidSHA1 = rule "security/avoid-SHA1" $
  match (pat "hash"
         `fromModule` "Crypto.Hash.SHA1")
  & severity Warning
  & message "SHA-1 is deprecated for security - use SHA-256 or SHA-3"
  & note "SHA-1 collisions have been demonstrated; migrate to SHA-256"
  & category Security
  & safetyLevel MostlySafe

-- | Detect DES usage.
--
-- == Problem
--
-- DES has a 56-bit key, easily brute-forced:
--
-- * DES can be cracked in hours
-- * Even 3DES is considered weak
avoidDES :: Rule
avoidDES = rule "security/avoid-DES" $
  match (pat "DES"
         `fromModule` "Crypto.Cipher.DES")
  & severity Error
  & message "DES has a 56-bit key and is trivially crackable - use AES"
  & note "Use AES-256-GCM for symmetric encryption"
  & category Security
  & safetyLevel Unsafe

-- | Detect ECB mode usage.
--
-- == Problem
--
-- ECB mode encrypts identical plaintext blocks to identical ciphertext:
--
-- * Reveals patterns in data
-- * Famous \"ECB penguin\" demonstration
--
-- == Solution
--
-- Use authenticated encryption modes:
--
-- @
-- -- Use GCM mode
-- import Crypto.Cipher.AES (AES256)
-- import Crypto.Cipher.Types (cipherInit, aeadInit, AEAD_GCM)
-- @
avoidECB :: Rule
avoidECB = rule "security/avoid-ECB" $
  match (pat "ecbEncrypt")
  & severity Error
  & message "ECB mode reveals patterns in encrypted data - use GCM or CTR mode"
  & note "ECB is never secure for data encryption; use authenticated modes like GCM"
  & category Security
  & safetyLevel Unsafe

-- | Detect weak random number generation.
--
-- == Problem
--
-- System.Random is not cryptographically secure:
--
-- @
-- import System.Random
--
-- -- INSECURE for security purposes!
-- randomRIO (0, 999999) :: IO Int  -- Predictable!
-- @
--
-- == Solution
--
-- Use cryptographically secure randomness:
--
-- @
-- import Crypto.Random (getRandomBytes)
-- import Crypto.Random.Types (MonadRandom)
-- @
avoidWeakRandom :: Rule
avoidWeakRandom = rule "security/avoid-weak-random" $
  match (pat "randomRIO"
         `fromModule` "System.Random")
  & severity Warning
  & message "System.Random is not cryptographically secure for security use"
  & note "For tokens, keys, IVs, use Crypto.Random from cryptonite"
  & category Security
  & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Metadata
--------------------------------------------------------------------------------

-- | Total number of security rules.
securityRuleCount :: Int
securityRuleCount = length securityRules
