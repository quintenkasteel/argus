{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Runtime.Types
-- Description : Types for runtime Haskell evaluation
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- Core types for the runtime Haskell evaluation system using the hint package.
-- This module provides type-safe abstractions for evaluating user-defined
-- Haskell code at runtime.
module Argus.Runtime.Types
  ( -- * Runtime Environment
    RuntimeEnv (..)
  , RuntimeConfig (..)
  , defaultRuntimeConfig

    -- * Evaluation Results
  , RuntimeResult (..)
  , RuntimeError (..)
  , ErrorKind (..)

    -- * Predicate Types
  , PredicateCode (..)
  , TransformCode (..)
  , ModuleSource (..)

    -- * Evaluation Context
  , EvalContext (..)
  , mkEvalContext

    -- * Capabilities
  , RuntimeCapabilities (..)
  , defaultCapabilities
  , restrictedCapabilities

    -- * Module Loading
  , LoadedModule (..)
  , ModuleStatus (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Runtime Environment
--------------------------------------------------------------------------------

-- | Runtime environment for Haskell evaluation
data RuntimeEnv = RuntimeEnv
  { reConfig :: RuntimeConfig
  , reLoadedModules :: Map FilePath LoadedModule
  , reImportedModules :: Set Text
  , reCapabilities :: RuntimeCapabilities
  , reInitTime :: UTCTime
  }
  deriving stock (Show, Generic)

-- | Configuration for the runtime interpreter
data RuntimeConfig = RuntimeConfig
  { rcSearchPaths :: [FilePath]
      -- ^ Paths to search for modules
  , rcDefaultImports :: [Text]
      -- ^ Modules to import by default
  , rcTimeout :: Int
      -- ^ Timeout for evaluation in milliseconds
  , rcMaxMemory :: Maybe Int
      -- ^ Maximum memory usage in MB (Nothing = unlimited)
  , rcSandboxed :: Bool
      -- ^ Whether to run in sandboxed mode
  , rcAllowUnsafe :: Bool
      -- ^ Whether to allow unsafe operations
  , rcCacheCompiled :: Bool
      -- ^ Whether to cache compiled expressions
  , rcVerbose :: Bool
      -- ^ Enable verbose logging
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Default runtime configuration
defaultRuntimeConfig :: RuntimeConfig
defaultRuntimeConfig = RuntimeConfig
  { rcSearchPaths = []
  , rcDefaultImports =
      [ "Prelude"
      , "Data.Text"
      , "Data.Map.Strict"
      , "Data.Set"
      , "Data.Maybe"
      , "Data.List"
      , "Control.Monad"
      ]
  , rcTimeout = 5000  -- 5 seconds
  , rcMaxMemory = Just 256  -- 256 MB
  , rcSandboxed = True
  , rcAllowUnsafe = False
  , rcCacheCompiled = True
  , rcVerbose = False
  }

--------------------------------------------------------------------------------
-- Evaluation Results
--------------------------------------------------------------------------------

-- | Result of runtime evaluation
data RuntimeResult a
  = RuntimeSuccess a
  | RuntimeFailure RuntimeError
  deriving stock (Eq, Show, Functor, Generic)
  deriving anyclass (NFData)

instance Applicative RuntimeResult where
  pure = RuntimeSuccess
  RuntimeSuccess f <*> RuntimeSuccess x = RuntimeSuccess (f x)
  RuntimeFailure e <*> _ = RuntimeFailure e
  _ <*> RuntimeFailure e = RuntimeFailure e

instance Monad RuntimeResult where
  RuntimeSuccess x >>= f = f x
  RuntimeFailure e >>= _ = RuntimeFailure e

-- | Runtime evaluation error
data RuntimeError = RuntimeError
  { reKind :: ErrorKind
  , reMessage :: Text
  , reLocation :: Maybe Text
  , reContext :: Maybe Text
  , reSuggestion :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Kinds of runtime errors
data ErrorKind
  = ParseError
      -- ^ Failed to parse Haskell code
  | TypeCheckError
      -- ^ Type checking failed
  | EvaluationError
      -- ^ Runtime evaluation failed
  | TimeoutError
      -- ^ Evaluation timed out
  | MemoryError
      -- ^ Memory limit exceeded
  | ModuleNotFound
      -- ^ Required module not found
  | SecurityViolation
      -- ^ Attempted forbidden operation
  | InternalError
      -- ^ Internal interpreter error
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

--------------------------------------------------------------------------------
-- Code Types
--------------------------------------------------------------------------------

-- | User-defined predicate code
newtype PredicateCode = PredicateCode { unPredicateCode :: Text }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, FromJSON, ToJSON, NFData)

-- | User-defined transformation code
newtype TransformCode = TransformCode { unTransformCode :: Text }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, FromJSON, ToJSON, NFData)

-- | Source of a user module
data ModuleSource
  = ModuleFile FilePath
      -- ^ Load from file path
  | ModuleInline Text Text
      -- ^ Module name and inline content
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

--------------------------------------------------------------------------------
-- Evaluation Context
--------------------------------------------------------------------------------

-- | Context provided to user predicates/transforms
data EvalContext = EvalContext
  { ecFilePath :: Text
      -- ^ Current file being analyzed
  , ecModuleName :: Text
      -- ^ Current module name
  , ecLineNumber :: Int
      -- ^ Line number of match
  , ecColumnNumber :: Int
      -- ^ Column number of match
  , ecMatchedText :: Text
      -- ^ The text that was matched
  , ecMetavars :: Map Text Text
      -- ^ Captured metavariables ($X, $Y, etc.)
  , ecFullContent :: Text
      -- ^ Full file content
  , ecImports :: [Text]
      -- ^ Current module's imports
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Create an evaluation context from basic info
mkEvalContext :: Text -> Text -> Int -> Int -> Text -> Map Text Text -> Text -> EvalContext
mkEvalContext fp mn ln col matched mvars content = EvalContext
  { ecFilePath = fp
  , ecModuleName = mn
  , ecLineNumber = ln
  , ecColumnNumber = col
  , ecMatchedText = matched
  , ecMetavars = mvars
  , ecFullContent = content
  , ecImports = extractImports content
  }
  where
    extractImports :: Text -> [Text]
    extractImports txt =
      [ T.strip $ T.drop 7 line
      | line <- T.lines txt
      , "import " `T.isPrefixOf` T.stripStart line
      ]

--------------------------------------------------------------------------------
-- Capabilities
--------------------------------------------------------------------------------

-- | Capabilities granted to runtime code
data RuntimeCapabilities = RuntimeCapabilities
  { capFileRead :: Bool
      -- ^ Can read files
  , capFileWrite :: Bool
      -- ^ Can write files
  , capNetwork :: Bool
      -- ^ Can make network requests
  , capProcess :: Bool
      -- ^ Can spawn processes
  , capEnvVars :: Bool
      -- ^ Can read environment variables
  , capAllowedModules :: Set Text
      -- ^ Modules allowed to import
  , capForbiddenModules :: Set Text
      -- ^ Modules explicitly forbidden
  , capAllowedFunctions :: Set Text
      -- ^ Functions explicitly allowed
  , capForbiddenFunctions :: Set Text
      -- ^ Functions explicitly forbidden
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Default capabilities (fairly permissive)
defaultCapabilities :: RuntimeCapabilities
defaultCapabilities = RuntimeCapabilities
  { capFileRead = False
  , capFileWrite = False
  , capNetwork = False
  , capProcess = False
  , capEnvVars = False
  , capAllowedModules = Set.fromList
      [ "Prelude", "Data.Text", "Data.Map.Strict", "Data.Set"
      , "Data.List", "Data.Maybe", "Data.Either", "Data.Char"
      , "Control.Monad", "Control.Applicative"
      , "Text.Regex.TDFA"
      ]
  , capForbiddenModules = Set.fromList
      [ "System.IO.Unsafe", "System.Process", "Network.HTTP"
      , "System.Environment", "Foreign", "Foreign.Ptr"
      , "GHC.IO.Unsafe"
      ]
  , capAllowedFunctions = Set.empty  -- Empty = all allowed
  , capForbiddenFunctions = Set.fromList
      [ "unsafePerformIO", "unsafeCoerce", "unsafeDupablePerformIO"
      , "unsafeInterleaveIO", "inlinePerformIO"
      ]
  }

-- | Restricted capabilities for untrusted code
restrictedCapabilities :: RuntimeCapabilities
restrictedCapabilities = RuntimeCapabilities
  { capFileRead = False
  , capFileWrite = False
  , capNetwork = False
  , capProcess = False
  , capEnvVars = False
  , capAllowedModules = Set.fromList
      [ "Prelude", "Data.Text", "Data.List", "Data.Maybe"
      , "Data.Char", "Control.Monad"
      ]
  , capForbiddenModules = Set.fromList
      [ "System.IO", "System.IO.Unsafe", "System.Process"
      , "Network", "Foreign", "GHC.IO"
      ]
  , capAllowedFunctions = Set.empty
  , capForbiddenFunctions = Set.fromList
      [ "unsafePerformIO", "unsafeCoerce", "readFile", "writeFile"
      , "appendFile", "getLine", "putStrLn", "print"
      ]
  }

--------------------------------------------------------------------------------
-- Module Loading
--------------------------------------------------------------------------------

-- | A loaded user module
data LoadedModule = LoadedModule
  { lmPath :: FilePath
  , lmName :: Text
  , lmExports :: [Text]
  , lmStatus :: ModuleStatus
  , lmLoadTime :: UTCTime
  , lmChecksum :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Status of a loaded module
data ModuleStatus
  = ModuleLoaded
      -- ^ Successfully loaded
  | ModuleError Text
      -- ^ Failed to load with error
  | ModuleOutdated
      -- ^ File changed since loading
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
