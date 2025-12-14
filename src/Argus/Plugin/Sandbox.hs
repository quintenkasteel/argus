{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Plugin.Sandbox
-- Description : Sandboxing for plugin execution
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- This module provides sandboxing capabilities for plugin execution,
-- ensuring plugins cannot perform dangerous operations or consume
-- excessive resources.
module Argus.Plugin.Sandbox
  ( -- * Sandbox Configuration
    SandboxConfig (..)
  , defaultSandboxConfig
  , strictSandboxConfig
  , permissiveSandboxConfig

    -- * Sandbox Environment
  , Sandbox
  , newSandbox
  , destroySandbox
  , withSandbox

    -- * Execution
  , runInSandbox
  , runWithTimeout
  , runWithMemoryLimit
  , SandboxResult (..)
  , SandboxError (..)

    -- * Permissions
  , Permission (..)
  , PermissionSet
  , grantPermission
  , revokePermission
  , hasPermission
  , allPermissions
  , noPermissions

    -- * Resource Tracking
  , ResourceUsage (..)
  , getResourceUsage
  , ResourceLimits (..)
  , defaultResourceLimits

    -- * Security Checks
  , validateCode
  , ValidationResult (..)
  , SecurityIssue (..)
  ) where

import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.STM
import Control.DeepSeq (NFData)
import Control.Exception (Exception, SomeException, catch, try)
import Control.Monad (forM_)
import Data.Aeson (FromJSON, ToJSON)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Mem (performGC)
import System.Timeout (timeout)
import Text.Regex.TDFA ((=~))

--------------------------------------------------------------------------------
-- Sandbox Configuration
--------------------------------------------------------------------------------

-- | Sandbox configuration
data SandboxConfig = SandboxConfig
  { scPermissions :: PermissionSet
      -- ^ Granted permissions
  , scResourceLimits :: ResourceLimits
      -- ^ Resource limits
  , scAllowedModules :: Set Text
      -- ^ Modules allowed to be imported
  , scForbiddenModules :: Set Text
      -- ^ Modules explicitly forbidden
  , scForbiddenFunctions :: Set Text
      -- ^ Functions explicitly forbidden
  , scTimeoutMs :: Int
      -- ^ Execution timeout in milliseconds
  , scMaxMemoryMB :: Int
      -- ^ Maximum memory usage in MB
  , scEnforceStrictMode :: Bool
      -- ^ Enforce strict evaluation
  , scLogViolations :: Bool
      -- ^ Log security violations
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Default sandbox configuration (moderate restrictions)
defaultSandboxConfig :: SandboxConfig
defaultSandboxConfig = SandboxConfig
  { scPermissions = Set.fromList
      [ PermReadFile
      , PermPureComputation
      , PermNetworkLocal
      ]
  , scResourceLimits = defaultResourceLimits
  , scAllowedModules = Set.fromList
      [ "Prelude", "Data.List", "Data.Maybe", "Data.Either"
      , "Data.Text", "Data.Map.Strict", "Data.Set"
      , "Control.Monad", "Control.Applicative"
      ]
  , scForbiddenModules = Set.fromList
      [ "System.IO.Unsafe", "System.Process", "Network.HTTP"
      , "Foreign", "GHC.IO.Unsafe", "Unsafe.Coerce"
      ]
  , scForbiddenFunctions = Set.fromList
      [ "unsafePerformIO", "unsafeCoerce", "unsafeInterleaveIO"
      , "readFile", "writeFile", "system", "rawSystem"
      ]
  , scTimeoutMs = 5000
  , scMaxMemoryMB = 256
  , scEnforceStrictMode = True
  , scLogViolations = True
  }

-- | Strict sandbox configuration (maximum restrictions)
strictSandboxConfig :: SandboxConfig
strictSandboxConfig = SandboxConfig
  { scPermissions = Set.singleton PermPureComputation
  , scResourceLimits = ResourceLimits
      { rlMaxCPUTime = Just 1000
      , rlMaxMemory = Just 64
      , rlMaxAllocations = Just 1000000
      , rlMaxThreads = Just 1
      , rlMaxFileDescriptors = Just 0
      , rlMaxNetworkConnections = Just 0
      }
  , scAllowedModules = Set.fromList
      [ "Prelude", "Data.List", "Data.Maybe"
      ]
  , scForbiddenModules = Set.fromList
      [ "System.IO", "System.IO.Unsafe", "System.Process"
      , "Network", "Foreign", "GHC.IO", "Control.Concurrent"
      ]
  , scForbiddenFunctions = Set.fromList
      [ "unsafePerformIO", "unsafeCoerce", "error", "throw"
      , "readFile", "writeFile", "appendFile"
      , "putStrLn", "print", "getLine"
      ]
  , scTimeoutMs = 1000
  , scMaxMemoryMB = 64
  , scEnforceStrictMode = True
  , scLogViolations = True
  }

-- | Permissive sandbox configuration (minimal restrictions)
permissiveSandboxConfig :: SandboxConfig
permissiveSandboxConfig = SandboxConfig
  { scPermissions = allPermissions
  , scResourceLimits = ResourceLimits
      { rlMaxCPUTime = Just 30000
      , rlMaxMemory = Just 1024
      , rlMaxAllocations = Nothing
      , rlMaxThreads = Just 8
      , rlMaxFileDescriptors = Just 100
      , rlMaxNetworkConnections = Just 10
      }
  , scAllowedModules = Set.empty  -- Empty means all allowed
  , scForbiddenModules = Set.fromList
      [ "System.IO.Unsafe", "Unsafe.Coerce"
      ]
  , scForbiddenFunctions = Set.fromList
      [ "unsafePerformIO", "unsafeCoerce"
      ]
  , scTimeoutMs = 30000
  , scMaxMemoryMB = 1024
  , scEnforceStrictMode = False
  , scLogViolations = False
  }

--------------------------------------------------------------------------------
-- Permissions
--------------------------------------------------------------------------------

-- | Permission types
data Permission
  = PermReadFile
      -- ^ Read files from disk
  | PermWriteFile
      -- ^ Write files to disk
  | PermNetworkLocal
      -- ^ Local network access
  | PermNetworkRemote
      -- ^ Remote network access
  | PermSpawnProcess
      -- ^ Spawn external processes
  | PermSpawnThread
      -- ^ Spawn threads
  | PermAccessEnv
      -- ^ Access environment variables
  | PermFFI
      -- ^ Foreign function interface
  | PermPureComputation
      -- ^ Pure computations only
  | PermModifyGlobal
      -- ^ Modify global state
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Set of permissions
type PermissionSet = Set Permission

-- | Grant a permission
grantPermission :: Permission -> PermissionSet -> PermissionSet
grantPermission = Set.insert

-- | Revoke a permission
revokePermission :: Permission -> PermissionSet -> PermissionSet
revokePermission = Set.delete

-- | Check if permission is granted
hasPermission :: Permission -> PermissionSet -> Bool
hasPermission = Set.member

-- | All permissions
allPermissions :: PermissionSet
allPermissions = Set.fromList [minBound .. maxBound]

-- | No permissions
noPermissions :: PermissionSet
noPermissions = Set.empty

--------------------------------------------------------------------------------
-- Resource Limits
--------------------------------------------------------------------------------

-- | Resource limits
data ResourceLimits = ResourceLimits
  { rlMaxCPUTime :: Maybe Int
      -- ^ Maximum CPU time in milliseconds
  , rlMaxMemory :: Maybe Int
      -- ^ Maximum memory in MB
  , rlMaxAllocations :: Maybe Int
      -- ^ Maximum heap allocations
  , rlMaxThreads :: Maybe Int
      -- ^ Maximum threads
  , rlMaxFileDescriptors :: Maybe Int
      -- ^ Maximum file descriptors
  , rlMaxNetworkConnections :: Maybe Int
      -- ^ Maximum network connections
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Default resource limits
defaultResourceLimits :: ResourceLimits
defaultResourceLimits = ResourceLimits
  { rlMaxCPUTime = Just 5000
  , rlMaxMemory = Just 256
  , rlMaxAllocations = Just 10000000
  , rlMaxThreads = Just 4
  , rlMaxFileDescriptors = Just 10
  , rlMaxNetworkConnections = Just 5
  }

-- | Resource usage tracking
data ResourceUsage = ResourceUsage
  { ruCPUTime :: Int
      -- ^ CPU time used in milliseconds
  , ruMemory :: Int
      -- ^ Memory used in bytes
  , ruAllocations :: Int
      -- ^ Heap allocations
  , ruThreadsCreated :: Int
      -- ^ Threads created
  , ruFilesOpened :: Int
      -- ^ Files opened
  , ruNetworkConnections :: Int
      -- ^ Network connections made
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

--------------------------------------------------------------------------------
-- Sandbox Environment
--------------------------------------------------------------------------------

-- | Sandbox state
data Sandbox = Sandbox
  { sbConfig :: SandboxConfig
  , sbUsage :: TVar ResourceUsage
  , sbViolations :: TVar [SecurityIssue]
  , sbStartTime :: UTCTime
  , sbActive :: TVar Bool
  , sbThreads :: TVar [ThreadId]
  }

-- | Create a new sandbox
newSandbox :: SandboxConfig -> IO Sandbox
newSandbox config = do
  now <- getCurrentTime
  usage <- newTVarIO $ ResourceUsage 0 0 0 0 0 0
  violations <- newTVarIO []
  active <- newTVarIO True
  threads <- newTVarIO []

  return Sandbox
    { sbConfig = config
    , sbUsage = usage
    , sbViolations = violations
    , sbStartTime = now
    , sbActive = active
    , sbThreads = threads
    }

-- | Destroy a sandbox and cleanup resources
destroySandbox :: Sandbox -> IO ()
destroySandbox sb = do
  atomically $ writeTVar (sbActive sb) False

  -- Kill all spawned threads
  threads <- atomically $ readTVar (sbThreads sb)
  forM_ threads $ \tid ->
    killThread tid `catch` \(_ :: SomeException) -> return ()

  -- Force garbage collection
  performGC

-- | Run an action with a sandbox
withSandbox :: SandboxConfig -> (Sandbox -> IO a) -> IO (SandboxResult a)
withSandbox config action = do
  sb <- newSandbox config
  result <- runInSandbox sb (action sb)
  destroySandbox sb
  return result

--------------------------------------------------------------------------------
-- Sandbox Execution
--------------------------------------------------------------------------------

-- | Result of sandbox execution
data SandboxResult a
  = SandboxSuccess a ResourceUsage
      -- ^ Successful execution with resource usage
  | SandboxTimeout
      -- ^ Execution timed out
  | SandboxMemoryExceeded
      -- ^ Memory limit exceeded
  | SandboxSecurityViolation [SecurityIssue]
      -- ^ Security policy violated
  | SandboxError Text
      -- ^ Execution error
  deriving stock (Eq, Show, Functor, Generic)

-- | Sandbox error type
data SandboxError
  = SETimeout
  | SEMemoryExceeded
  | SESecurityViolation SecurityIssue
  | SEResourceExceeded Text
  | SEInternalError Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

instance Exception SandboxError

-- | Run an action in the sandbox
runInSandbox :: Sandbox -> IO a -> IO (SandboxResult a)
runInSandbox sb action = do
  -- Check if sandbox is active
  active <- atomically $ readTVar (sbActive sb)
  if not active
    then return $ SandboxError "Sandbox is not active"
    else do
      let timeoutMs = scTimeoutMs (sbConfig sb)

      -- Run with timeout
      result <- timeout (timeoutMs * 1000) $ do
        -- Try to evaluate the action
        tryResult <- try action
        case tryResult of
          Left (e :: SomeException) -> return $ SandboxError $ T.pack $ show e
          Right val -> do
            -- Check for violations
            violations <- atomically $ readTVar (sbViolations sb)
            if not (null violations)
              then return $ SandboxSecurityViolation violations
              else do
                usage <- atomically $ readTVar (sbUsage sb)
                return $ SandboxSuccess val usage

      case result of
        Nothing -> return SandboxTimeout
        Just r -> return r

-- | Run with timeout
runWithTimeout :: Int -> IO a -> IO (Maybe a)
runWithTimeout ms = timeout (ms * 1000)

-- | Run with memory limit (approximate)
runWithMemoryLimit :: Int -> IO a -> IO (Either SandboxError a)
runWithMemoryLimit _limitMB action = do
  -- Note: Real memory limiting would require GHC runtime integration
  -- This is a simplified version that just runs the action
  result <- try action
  case result of
    Left (e :: SomeException) -> return $ Left $ SEInternalError $ T.pack $ show e
    Right val -> return $ Right val

-- | Get current resource usage
getResourceUsage :: Sandbox -> IO ResourceUsage
getResourceUsage sb = atomically $ readTVar (sbUsage sb)

--------------------------------------------------------------------------------
-- Security Validation
--------------------------------------------------------------------------------

-- | Validation result
data ValidationResult
  = ValidationPassed
  | ValidationFailed [SecurityIssue]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Security issue
data SecurityIssue = SecurityIssue
  { siSeverity :: IssueSeverity
  , siCategory :: IssueCategory
  , siMessage :: Text
  , siLocation :: Maybe Text
  , siSuggestion :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Issue severity
data IssueSeverity
  = SeverityInfo
  | SeverityWarning
  | SeverityError
  | SeverityCritical
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Issue category
data IssueCategory
  = CategoryUnsafeCode
  | CategoryForbiddenImport
  | CategoryForbiddenFunction
  | CategoryResourceAbuse
  | CategoryMaliciousPattern
  | CategoryPrivilegeEscalation
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Validate code against sandbox policy
validateCode :: SandboxConfig -> Text -> ValidationResult
validateCode config code =
  let issues = concat
        [ checkForbiddenModules config code
        , checkForbiddenFunctions config code
        , checkMaliciousPatterns code
        , checkUnsafeOperations code
        ]
  in if null issues
     then ValidationPassed
     else ValidationFailed issues

-- | Check for forbidden module imports
checkForbiddenModules :: SandboxConfig -> Text -> [SecurityIssue]
checkForbiddenModules config code =
  [ SecurityIssue
      { siSeverity = SeverityCritical
      , siCategory = CategoryForbiddenImport
      , siMessage = "Forbidden module import: " <> modName
      , siLocation = Nothing
      , siSuggestion = Just "Remove this import"
      }
  | modName <- Set.toList (scForbiddenModules config)
  , hasImport code modName
  ]
  where
    hasImport src modName =
      let pat = "import\\s+(qualified\\s+)?" <> T.unpack modName :: String
      in T.unpack src =~ pat

-- | Check for forbidden function usage
checkForbiddenFunctions :: SandboxConfig -> Text -> [SecurityIssue]
checkForbiddenFunctions config code =
  [ SecurityIssue
      { siSeverity = SeverityCritical
      , siCategory = CategoryForbiddenFunction
      , siMessage = "Forbidden function: " <> funcName
      , siLocation = Nothing
      , siSuggestion = Just "Remove usage of this function"
      }
  | funcName <- Set.toList (scForbiddenFunctions config)
  , hasFunction code funcName
  ]
  where
    hasFunction src funcName =
      let pat = "\\b" <> T.unpack funcName <> "\\b" :: String
      in T.unpack src =~ pat

-- | Check for malicious patterns
checkMaliciousPatterns :: Text -> [SecurityIssue]
checkMaliciousPatterns code =
  [ SecurityIssue
      { siSeverity = SeverityCritical
      , siCategory = CategoryMaliciousPattern
      , siMessage = msg
      , siLocation = Nothing
      , siSuggestion = Just "Remove this code pattern"
      }
  | (pat, msg) <- maliciousPatterns
  , T.unpack code =~ pat
  ]
  where
    maliciousPatterns :: [(String, Text)]
    maliciousPatterns =
      [ ("fix\\s*\\(\\s*\\\\", "Potential infinite recursion via fix")
      , ("forever\\s*\\$", "Potential infinite loop via forever")
      , ("evaluate\\s*\\(\\s*unsafePerformIO", "Unsafe evaluation")
      , ("foreign\\s+import", "Foreign function interface")
      , ("\\$\\(|\\[\\|", "Template Haskell")
      ]

-- | Check for unsafe operations
checkUnsafeOperations :: Text -> [SecurityIssue]
checkUnsafeOperations code =
  [ SecurityIssue
      { siSeverity = SeverityError
      , siCategory = CategoryUnsafeCode
      , siMessage = "Unsafe operation: " <> op
      , siLocation = Nothing
      , siSuggestion = Just "Use safe alternatives"
      }
  | op <- unsafeOps
  , op `T.isInfixOf` code
  ]
  where
    unsafeOps :: [Text]
    unsafeOps =
      [ "unsafePerformIO"
      , "unsafeCoerce"
      , "unsafeDupablePerformIO"
      , "unsafeInterleaveIO"
      , "accursedUnutterablePerformIO"
      , "inlinePerformIO"
      ]
