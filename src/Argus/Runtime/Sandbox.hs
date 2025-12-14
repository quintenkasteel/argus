{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Runtime.Sandbox
-- Description : Security sandboxing for runtime evaluation
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- This module provides security sandboxing for the runtime Haskell evaluation.
-- It validates code before execution, prevents access to dangerous functions,
-- and enforces capability restrictions.
module Argus.Runtime.Sandbox
  ( -- * Sandbox Configuration
    SandboxConfig (..)
  , defaultSandboxConfig
  , strictSandboxConfig

    -- * Code Validation
  , validateCodeSecurity
  , SecurityIssue (..)
  , ViolationType (..)

    -- * AST Analysis
  , analyzeCodeAST
  , CodeAnalysis (..)
  , DetectedFeature (..)

    -- * Import Filtering
  , filterImports
  , ImportFilterResult (..)

    -- * Resource Limits
  , ResourceLimits (..)
  , defaultResourceLimits
  , enforceResourceLimits
  ) where

import Control.Monad (forM_)
import Data.Char (isAlphaNum, isSpace)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Text.Regex.TDFA ((=~))

import Argus.Runtime.Types

--------------------------------------------------------------------------------
-- Sandbox Configuration
--------------------------------------------------------------------------------

-- | Sandbox configuration
data SandboxConfig = SandboxConfig
  { scForbiddenModules :: Set Text
      -- ^ Modules that cannot be imported
  , scForbiddenFunctions :: Set Text
      -- ^ Functions that cannot be used
  , scForbiddenPatterns :: [Text]
      -- ^ Regex patterns that are forbidden
  , scAllowFFI :: Bool
      -- ^ Allow Foreign Function Interface
  , scAllowTemplateHaskell :: Bool
      -- ^ Allow Template Haskell
  , scAllowUnsafe :: Bool
      -- ^ Allow System.IO.Unsafe and related
  , scMaxStringLength :: Int
      -- ^ Maximum string literal length
  , scMaxRecursionDepth :: Int
      -- ^ Maximum recursion depth hint
  , scResourceLimits :: ResourceLimits
      -- ^ Resource usage limits
  }
  deriving stock (Eq, Show)

-- | Default sandbox configuration (moderately restrictive)
defaultSandboxConfig :: SandboxConfig
defaultSandboxConfig = SandboxConfig
  { scForbiddenModules = Set.fromList
      [ "System.IO.Unsafe"
      , "System.Process"
      , "System.Environment"
      , "Network.HTTP"
      , "Network.HTTP.Client"
      , "Network.Socket"
      , "Foreign"
      , "Foreign.Ptr"
      , "Foreign.C"
      , "Foreign.Marshal"
      , "GHC.IO.Unsafe"
      , "GHC.Exts"
      , "Unsafe.Coerce"
      ]
  , scForbiddenFunctions = Set.fromList
      [ "unsafePerformIO"
      , "unsafeCoerce"
      , "unsafeDupablePerformIO"
      , "unsafeInterleaveIO"
      , "inlinePerformIO"
      , "accursedUnutterablePerformIO"
      , "readFile"
      , "writeFile"
      , "appendFile"
      , "readIO"
      , "readLn"
      , "getLine"
      , "getContents"
      , "interact"
      , "putStr"
      , "putStrLn"
      , "print"
      , "getArgs"
      , "getEnv"
      , "getEnvironment"
      , "setEnv"
      , "unsetEnv"
      , "system"
      , "rawSystem"
      , "callProcess"
      , "callCommand"
      , "createProcess"
      , "spawnProcess"
      , "spawnCommand"
      , "openFile"
      , "hGetContents"
      , "hPutStr"
      , "hPutStrLn"
      , "hClose"
      , "hFlush"
      ]
  , scForbiddenPatterns =
      [ "import\\s+qualified\\s+System\\.IO\\.Unsafe"
      , "import\\s+System\\.Process"
      , "foreign\\s+import"
      , "\\{-#\\s*LANGUAGE\\s+.*Unsafe"
      , "\\$\\(|\\[\\|"  -- Template Haskell splices and quotes
      ]
  , scAllowFFI = False
  , scAllowTemplateHaskell = False
  , scAllowUnsafe = False
  , scMaxStringLength = 10000
  , scMaxRecursionDepth = 100
  , scResourceLimits = defaultResourceLimits
  }

-- | Strict sandbox configuration (very restrictive)
strictSandboxConfig :: SandboxConfig
strictSandboxConfig = defaultSandboxConfig
  { scForbiddenModules = scForbiddenModules defaultSandboxConfig
      `Set.union` Set.fromList
        [ "System.IO"
        , "Control.Concurrent"
        , "Control.Exception"
        , "Debug.Trace"
        , "GHC.Prim"
        , "GHC.Base"
        ]
  , scForbiddenFunctions = scForbiddenFunctions defaultSandboxConfig
      `Set.union` Set.fromList
        [ "trace"
        , "traceShow"
        , "traceShowId"
        , "traceM"
        , "traceIO"
        , "error"
        , "errorWithoutStackTrace"
        , "throw"
        , "throwIO"
        , "evaluate"
        , "seq"
        , "deepseq"
        , "force"
        ]
  , scMaxStringLength = 1000
  , scMaxRecursionDepth = 50
  , scResourceLimits = ResourceLimits
      { rlMaxMemory = Just 64
      , rlMaxTime = Just 1000
      , rlMaxAllocations = Just 1000000
      , rlMaxFileHandles = Just 0
      , rlMaxThreads = Just 1
      }
  }

--------------------------------------------------------------------------------
-- Security Issue
--------------------------------------------------------------------------------

-- | A detected security issue
data SecurityIssue = SecurityIssue
  { siType :: ViolationType
  , siMessage :: Text
  , siLocation :: Maybe (Int, Int)
      -- ^ (line, column) if available
  , siSeverity :: ViolationSeverity
  }
  deriving stock (Eq, Show)

-- | Types of security violations
data ViolationType
  = ForbiddenModule Text
  | ForbiddenFunction Text
  | ForbiddenPattern Text
  | FFIUsage
  | TemplateHaskellUsage
  | UnsafeUsage
  | ResourceLimitExceeded Text
  | MaliciousPattern Text
  deriving stock (Eq, Show)

-- | Severity of violation
data ViolationSeverity
  = ViolationWarning
  | ViolationError
  | ViolationCritical
  deriving stock (Eq, Show, Ord)

--------------------------------------------------------------------------------
-- Code Validation
--------------------------------------------------------------------------------

-- | Validate code for security issues
validateCodeSecurity :: SandboxConfig -> Text -> [SecurityIssue]
validateCodeSecurity cfg code = concat
  [ checkForbiddenModules cfg code
  , checkForbiddenFunctions cfg code
  , checkForbiddenPatterns cfg code
  , checkFFI cfg code
  , checkTemplateHaskell cfg code
  , checkUnsafe cfg code
  , checkStringLiterals cfg code
  , checkMaliciousPatterns code
  ]

-- | Check for forbidden module imports
checkForbiddenModules :: SandboxConfig -> Text -> [SecurityIssue]
checkForbiddenModules cfg code =
  [ SecurityIssue
      { siType = ForbiddenModule modName
      , siMessage = "Import of forbidden module: " <> modName
      , siLocation = findLocation code ("import" <> modName)
      , siSeverity = ViolationCritical
      }
  | modName <- Set.toList (scForbiddenModules cfg)
  , hasImport code modName
  ]
  where
    hasImport :: Text -> Text -> Bool
    hasImport src modName =
      let pat = "import\\s+(qualified\\s+)?" <> T.unpack modName :: String
      in T.unpack src =~ pat

-- | Check for forbidden function usage
checkForbiddenFunctions :: SandboxConfig -> Text -> [SecurityIssue]
checkForbiddenFunctions cfg code =
  [ SecurityIssue
      { siType = ForbiddenFunction funcName
      , siMessage = "Use of forbidden function: " <> funcName
      , siLocation = findLocation code funcName
      , siSeverity = ViolationCritical
      }
  | funcName <- Set.toList (scForbiddenFunctions cfg)
  , hasFunction code funcName
  ]
  where
    hasFunction :: Text -> Text -> Bool
    hasFunction src funcName =
      let funcPat = "\\b" <> T.unpack funcName <> "\\b" :: String
      in T.unpack src =~ funcPat

-- | Check for forbidden patterns
checkForbiddenPatterns :: SandboxConfig -> Text -> [SecurityIssue]
checkForbiddenPatterns cfg code =
  [ SecurityIssue
      { siType = ForbiddenPattern forbidPat
      , siMessage = "Forbidden pattern detected: " <> forbidPat
      , siLocation = Nothing
      , siSeverity = ViolationError
      }
  | forbidPat <- scForbiddenPatterns cfg
  , T.unpack code =~ T.unpack forbidPat
  ]

-- | Check for FFI usage
checkFFI :: SandboxConfig -> Text -> [SecurityIssue]
checkFFI cfg code
  | scAllowFFI cfg = []
  | otherwise =
      [ SecurityIssue
          { siType = FFIUsage
          , siMessage = "Foreign Function Interface usage detected"
          , siLocation = findLocation code "foreign import"
          , siSeverity = ViolationCritical
          }
      | hasFFI code
      ]
  where
    hasFFI :: Text -> Bool
    hasFFI src = T.unpack src =~ ("foreign\\s+import" :: String) ||
                 T.unpack src =~ ("foreign\\s+export" :: String)

-- | Check for Template Haskell usage
checkTemplateHaskell :: SandboxConfig -> Text -> [SecurityIssue]
checkTemplateHaskell cfg code
  | scAllowTemplateHaskell cfg = []
  | otherwise =
      [ SecurityIssue
          { siType = TemplateHaskellUsage
          , siMessage = "Template Haskell usage detected"
          , siLocation = Nothing
          , siSeverity = ViolationError
          }
      | hasTH code
      ]
  where
    hasTH :: Text -> Bool
    hasTH src = T.unpack src =~ ("\\$\\(" :: String) ||  -- splice
                T.unpack src =~ ("\\[\\|" :: String) ||  -- quote
                T.unpack src =~ ("\\[d\\|" :: String) || -- dec quote
                T.unpack src =~ ("\\[t\\|" :: String) || -- type quote
                T.unpack src =~ ("\\[p\\|" :: String)    -- pattern quote

-- | Check for unsafe operations
checkUnsafe :: SandboxConfig -> Text -> [SecurityIssue]
checkUnsafe cfg code
  | scAllowUnsafe cfg = []
  | otherwise =
      [ SecurityIssue
          { siType = UnsafeUsage
          , siMessage = "Unsafe operation detected: " <> unsafePat
          , siLocation = findLocation code unsafePat
          , siSeverity = ViolationCritical
          }
      | unsafePat <- unsafePatterns
      , T.isInfixOf unsafePat code
      ]
  where
    unsafePatterns =
      [ "unsafePerformIO"
      , "unsafeCoerce"
      , "unsafeDupablePerformIO"
      , "unsafeInterleaveIO"
      , "System.IO.Unsafe"
      , "Unsafe.Coerce"
      ]

-- | Check for overly long string literals
checkStringLiterals :: SandboxConfig -> Text -> [SecurityIssue]
checkStringLiterals cfg code =
  [ SecurityIssue
      { siType = ResourceLimitExceeded "string literal"
      , siMessage = "String literal exceeds maximum length: " <> T.pack (show len)
      , siLocation = Nothing
      , siSeverity = ViolationWarning
      }
  | len <- [maxStringLength code]
  , len > scMaxStringLength cfg
  ]
  where
    maxStringLength :: Text -> Int
    maxStringLength src =
      let strings = extractStrings (T.unpack src)
      in if null strings then 0 else maximum $ map length strings

    extractStrings :: String -> [String]
    extractStrings [] = []
    extractStrings ('"':rest) =
      let (str, remainder) = extractUntilQuote rest
      in str : extractStrings remainder
    extractStrings (_:rest) = extractStrings rest

    extractUntilQuote :: String -> (String, String)
    extractUntilQuote [] = ([], [])
    extractUntilQuote ('\\':'"':rest) =
      let (s, r) = extractUntilQuote rest
      in ('"':s, r)
    extractUntilQuote ('\\':'\\':rest) =
      let (s, r) = extractUntilQuote rest
      in ('\\':s, r)
    extractUntilQuote ('"':rest) = ([], rest)
    extractUntilQuote (c:rest) =
      let (s, r) = extractUntilQuote rest
      in (c:s, r)

-- | Check for malicious patterns
checkMaliciousPatterns :: Text -> [SecurityIssue]
checkMaliciousPatterns code =
  [ SecurityIssue
      { siType = MaliciousPattern desc
      , siMessage = desc
      , siLocation = Nothing
      , siSeverity = ViolationCritical
      }
  | (malPat, desc) <- maliciousPatterns
  , T.unpack code =~ malPat
  ]
  where
    maliciousPatterns :: [(String, Text)]
    maliciousPatterns =
      [ ("fix\\s*\\(\\s*\\\\", "Potential infinite recursion via fix combinator")
      , ("forever\\s*\\$", "Potential infinite loop via forever")
      , ("iterate\\s+id", "Potential infinite loop via iterate id")
      , ("repeat\\s+", "Unbounded list via repeat")
      , ("cycle\\s+", "Unbounded list via cycle")
      ]

-- | Find location of a pattern in code
findLocation :: Text -> Text -> Maybe (Int, Int)
findLocation code searchPat =
  case T.breakOn searchPat code of
    (before, after)
      | T.null after -> Nothing
      | otherwise ->
          let lines' = T.lines before
              lineNum = length lines'
              colNum = case lines' of
                [] -> 1
                ls -> T.length (last ls) + 1
          in Just (lineNum, colNum)

--------------------------------------------------------------------------------
-- AST Analysis
--------------------------------------------------------------------------------

-- | Analysis of code structure
data CodeAnalysis = CodeAnalysis
  { caImports :: [Text]
  , caExports :: [Text]
  , caFunctions :: [Text]
  , caDetectedFeatures :: [DetectedFeature]
  , caComplexity :: Int
  , caLinesOfCode :: Int
  }
  deriving stock (Eq, Show)

-- | Detected language features
data DetectedFeature
  = FeatureRecursion
  | FeatureHigherOrder
  | FeatureTypeClass
  | FeatureGADT
  | FeatureTypeFamilies
  | FeatureRankNTypes
  | FeatureExistentials
  | FeatureTemplateHaskell
  | FeatureFFI
  | FeatureUnsafe
  deriving stock (Eq, Show, Ord, Bounded, Enum)

-- | Analyze code structure
analyzeCodeAST :: Text -> CodeAnalysis
analyzeCodeAST code = CodeAnalysis
  { caImports = extractImports code
  , caExports = extractExports code
  , caFunctions = extractFunctions code
  , caDetectedFeatures = detectFeatures code
  , caComplexity = estimateComplexity code
  , caLinesOfCode = length $ filter (not . T.null . T.strip) $ T.lines code
  }
  where
    extractImports :: Text -> [Text]
    extractImports src =
      [ T.strip $ T.drop 7 line
      | line <- T.lines src
      , "import " `T.isPrefixOf` T.stripStart line
      ]

    extractExports :: Text -> [Text]
    extractExports src =
      let moduleDecl = T.unlines $ takeWhile (not . ("where" `T.isSuffixOf`) . T.strip) $ T.lines src
      in if "module " `T.isInfixOf` moduleDecl
         then parseExportList moduleDecl
         else []

    parseExportList :: Text -> [Text]
    parseExportList decl =
      case T.breakOn "(" decl of
        (_, rest)
          | T.null rest -> []
          | otherwise ->
              let exports = T.takeWhile (/= ')') $ T.drop 1 rest
              in map T.strip $ T.splitOn "," exports

    extractFunctions :: Text -> [Text]
    extractFunctions src =
      [ T.strip $ T.takeWhile isIdentChar line
      | line <- T.lines src
      , not $ T.null line
      , not $ T.isPrefixOf "--" $ T.stripStart line
      , not $ T.isPrefixOf "{-" $ T.stripStart line
      , not $ T.isPrefixOf "import" $ T.stripStart line
      , not $ T.isPrefixOf "module" $ T.stripStart line
      , not $ T.isPrefixOf "data" $ T.stripStart line
      , not $ T.isPrefixOf "type" $ T.stripStart line
      , not $ T.isPrefixOf "newtype" $ T.stripStart line
      , not $ T.isPrefixOf "class" $ T.stripStart line
      , not $ T.isPrefixOf "instance" $ T.stripStart line
      , startsWithLower line
      , " = " `T.isInfixOf` line || " :: " `T.isInfixOf` line
      ]

    isIdentChar c = isAlphaNum c || c == '_' || c == '\''

    startsWithLower line =
      case T.uncons (T.stripStart line) of
        Just (c, _) -> c >= 'a' && c <= 'z'
        Nothing -> False

    detectFeatures :: Text -> [DetectedFeature]
    detectFeatures src = concat
      [ [FeatureRecursion | hasRecursion src]
      , [FeatureHigherOrder | hasHigherOrder src]
      , [FeatureTypeClass | hasTypeClass src]
      , [FeatureGADT | hasGADT src]
      , [FeatureTypeFamilies | hasTypeFamilies src]
      , [FeatureRankNTypes | hasRankNTypes src]
      , [FeatureExistentials | hasExistentials src]
      , [FeatureTemplateHaskell | hasTemplateHaskell src]
      , [FeatureFFI | hasFFI' src]
      , [FeatureUnsafe | hasUnsafe src]
      ]

    hasRecursion src = T.unpack src =~ ("\\bfix\\b" :: String) ||
                       T.unpack src =~ ("\\blet\\s+\\w+\\s*=.*\\bwhere\\b" :: String)
    hasHigherOrder src = T.unpack src =~ ("->\\s*\\(" :: String)
    hasTypeClass src = T.unpack src =~ ("\\bclass\\b|\\binstance\\b" :: String)
    hasGADT src = T.unpack src =~ ("\\bGADTs\\b|\\bdata\\s+\\w+\\s+where\\b" :: String)
    hasTypeFamilies src = T.unpack src =~ ("\\btype\\s+family\\b|\\bdata\\s+family\\b" :: String)
    hasRankNTypes src = T.unpack src =~ ("\\bforall\\b" :: String)
    hasExistentials src = T.unpack src =~ ("\\bexists\\b|ExistentialQuantification" :: String)
    hasTemplateHaskell src = T.unpack src =~ ("\\$\\(|\\[\\||TemplateHaskell" :: String)
    hasFFI' src = T.unpack src =~ ("\\bforeign\\s+import\\b" :: String)
    hasUnsafe src = T.unpack src =~ ("\\bunsafe\\b|Unsafe" :: String)

    estimateComplexity :: Text -> Int
    estimateComplexity src =
      let keywords = ["if", "case", "let", "where", "do", "\\", ">>", ">>="]
          count kw = length $ T.breakOnAll kw src
      in sum $ map count keywords

--------------------------------------------------------------------------------
-- Import Filtering
--------------------------------------------------------------------------------

-- | Result of import filtering
data ImportFilterResult = ImportFilterResult
  { ifrAllowed :: [Text]
  , ifrDenied :: [(Text, Text)]  -- (module, reason)
  , ifrWarnings :: [Text]
  }
  deriving stock (Eq, Show)

-- | Filter imports based on sandbox config
filterImports :: SandboxConfig -> [Text] -> ImportFilterResult
filterImports cfg imports = ImportFilterResult
  { ifrAllowed = allowed
  , ifrDenied = denied
  , ifrWarnings = warnings
  }
  where
    (allowed, denied, warnings) = foldr checkImport ([], [], []) imports

    checkImport :: Text -> ([Text], [(Text, Text)], [Text]) -> ([Text], [(Text, Text)], [Text])
    checkImport imp (a, d, w)
      | imp `Set.member` scForbiddenModules cfg =
          (a, (imp, "Module is forbidden") : d, w)
      | isSystemModule imp =
          (a, (imp, "System modules not allowed") : d, w)
      | isNetworkModule imp =
          (a, (imp, "Network modules not allowed") : d, w)
      | isSuspicious imp =
          (a, d, ("Suspicious import: " <> imp) : w)
      | otherwise =
          (imp : a, d, w)

    isSystemModule m = "System." `T.isPrefixOf` m
    isNetworkModule m = "Network." `T.isPrefixOf` m
    isSuspicious m = "Unsafe" `T.isInfixOf` m || "Internal" `T.isInfixOf` m

--------------------------------------------------------------------------------
-- Resource Limits
--------------------------------------------------------------------------------

-- | Resource usage limits
data ResourceLimits = ResourceLimits
  { rlMaxMemory :: Maybe Int
      -- ^ Maximum memory in MB
  , rlMaxTime :: Maybe Int
      -- ^ Maximum time in milliseconds
  , rlMaxAllocations :: Maybe Int
      -- ^ Maximum heap allocations
  , rlMaxFileHandles :: Maybe Int
      -- ^ Maximum open file handles
  , rlMaxThreads :: Maybe Int
      -- ^ Maximum concurrent threads
  }
  deriving stock (Eq, Show)

-- | Default resource limits
defaultResourceLimits :: ResourceLimits
defaultResourceLimits = ResourceLimits
  { rlMaxMemory = Just 256
  , rlMaxTime = Just 5000
  , rlMaxAllocations = Just 10000000
  , rlMaxFileHandles = Just 10
  , rlMaxThreads = Just 4
  }

-- | Enforce resource limits (returns violations)
enforceResourceLimits :: ResourceLimits -> Int -> Int -> Int -> Int -> Int -> [SecurityIssue]
enforceResourceLimits limits memMB timeMs allocs fds threads = concat
  [ checkLimit "memory" (rlMaxMemory limits) memMB
  , checkLimit "time" (rlMaxTime limits) timeMs
  , checkLimit "allocations" (rlMaxAllocations limits) allocs
  , checkLimit "file handles" (rlMaxFileHandles limits) fds
  , checkLimit "threads" (rlMaxThreads limits) threads
  ]
  where
    checkLimit :: Text -> Maybe Int -> Int -> [SecurityIssue]
    checkLimit name mLimit actual = case mLimit of
      Nothing -> []
      Just limit
        | actual > limit ->
            [ SecurityIssue
                { siType = ResourceLimitExceeded name
                , siMessage = name <> " limit exceeded: " <>
                              T.pack (show actual) <> " > " <> T.pack (show limit)
                , siLocation = Nothing
                , siSeverity = ViolationError
                }
            ]
        | otherwise -> []
