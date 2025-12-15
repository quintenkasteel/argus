{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Rules.Security
-- Description : Security vulnerability detection
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module detects security vulnerabilities and unsafe patterns in Haskell code,
-- including unsafe functions, injection vulnerabilities, and cryptographic issues.
module Argus.Rules.Security
  ( -- * Detection
    detectSecurityIssues
  , SecurityFinding (..)
  , SecurityCategory (..)

    -- * Configuration
  , SecurityConfig (..)
  , defaultSecurityConfig
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (mapMaybe, catMaybes)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import Argus.Analysis.TextProcessing (extractCode, patternInCode)
import Argus.Types
import Argus.Utils (isComment)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Category of security issue
data SecurityCategory
  = UnsafeFunction       -- ^ Use of unsafe* functions
  | InjectionRisk        -- ^ SQL/Shell/Path injection risk
  | CryptoIssue          -- ^ Cryptographic weakness
  | HardcodedSecret      -- ^ Hardcoded credentials
  | PathTraversal        -- ^ Directory traversal risk
  | UnsafeFFI            -- ^ Unsafe FFI usage
  | DebugCode            -- ^ Debug code in production
  | InsecureRandom       -- ^ Weak random number generation
  | UnsafeCoercion       -- ^ Type coercion bypassing safety
  | TemplateHaskellRisk  -- ^ TH security concerns
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A detected security finding
data SecurityFinding = SecurityFinding
  { sfCategory    :: SecurityCategory  -- ^ Type of issue
  , sfSpan        :: SrcSpan           -- ^ Location
  , sfCode        :: Text              -- ^ The problematic code
  , sfDescription :: Text              -- ^ What's wrong
  , sfRisk        :: Text              -- ^ Risk explanation
  , sfMitigation  :: Text              -- ^ How to fix
  , sfSeverity    :: Severity          -- ^ How serious
  , sfCWE         :: Maybe Text        -- ^ CWE identifier if applicable
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for security detection
data SecurityConfig = SecurityConfig
  { scEnabled           :: Bool   -- ^ Enable security checks
  , scCheckUnsafe       :: Bool   -- ^ Check unsafe functions
  , scCheckInjection    :: Bool   -- ^ Check injection vulnerabilities
  , scCheckCrypto       :: Bool   -- ^ Check crypto issues
  , scCheckSecrets      :: Bool   -- ^ Check hardcoded secrets
  , scCheckFFI          :: Bool   -- ^ Check FFI safety
  , scCheckDebug        :: Bool   -- ^ Check debug code
  , scAllowUnsafeInTest :: Bool   -- ^ Allow unsafe in tests
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration
defaultSecurityConfig :: SecurityConfig
defaultSecurityConfig = SecurityConfig
  { scEnabled = True
  , scCheckUnsafe = True
  , scCheckInjection = True
  , scCheckCrypto = True
  , scCheckSecrets = True
  , scCheckFFI = True
  , scCheckDebug = True
  , scAllowUnsafeInTest = True
  }

--------------------------------------------------------------------------------
-- Detection
--------------------------------------------------------------------------------

-- | Detect security issues in source code
detectSecurityIssues :: SecurityConfig -> FilePath -> Text -> [Diagnostic]
detectSecurityIssues config path content
  | not (scEnabled config) = []
  | otherwise =
    let isTest = "Test" `T.isInfixOf` T.pack path || "Spec" `T.isInfixOf` T.pack path
        issues = concat
          [ if scCheckUnsafe config then detectUnsafeFunctions path content isTest else []
          , if scCheckInjection config then detectInjectionRisks path content else []
          , if scCheckCrypto config then detectCryptoIssues path content else []
          , if scCheckSecrets config then detectHardcodedSecrets path content else []
          , if scCheckFFI config then detectUnsafeFFI path content else []
          , if scCheckDebug config then detectDebugCode path content else []
          ]
        filtered = if scAllowUnsafeInTest config && isTest
                   then filter (not . isUnsafeCategory . sfCategory) issues
                   else issues
    in map issueToDiagnostic filtered
  where
    isUnsafeCategory UnsafeFunction = True
    isUnsafeCategory UnsafeCoercion = True
    isUnsafeCategory DebugCode = True
    isUnsafeCategory _ = False

--------------------------------------------------------------------------------
-- Unsafe Function Detection
--------------------------------------------------------------------------------

-- | Detect use of unsafe functions
detectUnsafeFunctions :: FilePath -> Text -> Bool -> [SecurityFinding]
detectUnsafeFunctions path content _isTest =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkUnsafeLine path) linesWithNums

checkUnsafeLine :: FilePath -> (Int, Text) -> [SecurityFinding]
checkUnsafeLine path (lineNum, line)
  | isComment line = []  -- Skip comment lines
  | otherwise = catMaybes
  [ -- unsafePerformIO
    if patternInCode "unsafePerformIO" line
    then Just SecurityFinding
      { sfCategory = UnsafeFunction
      , sfSpan = mkSpan path lineNum "unsafePerformIO" line
      , sfCode = T.strip line
      , sfDescription = "Use of unsafePerformIO"
      , sfRisk = "Breaks referential transparency and can cause unpredictable behavior"
      , sfMitigation = "Use proper IO monad or ST monad for mutable state"
      , sfSeverity = Warning
      , sfCWE = Just "CWE-676"
      }
    else Nothing

  , -- unsafeInterleaveIO
    if patternInCode "unsafeInterleaveIO" line
    then Just SecurityFinding
      { sfCategory = UnsafeFunction
      , sfSpan = mkSpan path lineNum "unsafeInterleaveIO" line
      , sfCode = T.strip line
      , sfDescription = "Use of unsafeInterleaveIO"
      , sfRisk = "Can cause non-deterministic behavior and resource leaks"
      , sfMitigation = "Use streaming libraries (conduit, pipes) for lazy IO"
      , sfSeverity = Warning
      , sfCWE = Just "CWE-676"
      }
    else Nothing

  , -- unsafeDupablePerformIO
    if patternInCode "unsafeDupablePerformIO" line
    then Just SecurityFinding
      { sfCategory = UnsafeFunction
      , sfSpan = mkSpan path lineNum "unsafeDupablePerformIO" line
      , sfCode = T.strip line
      , sfDescription = "Use of unsafeDupablePerformIO"
      , sfRisk = "IO action may be executed multiple times on multicore systems"
      , sfMitigation = "Ensure action is idempotent or use unsafePerformIO"
      , sfSeverity = Warning
      , sfCWE = Just "CWE-676"
      }
    else Nothing

  , -- unsafeCoerce
    if patternInCode "unsafeCoerce" line
    then Just SecurityFinding
      { sfCategory = UnsafeCoercion
      , sfSpan = mkSpan path lineNum "unsafeCoerce" line
      , sfCode = T.strip line
      , sfDescription = "Use of unsafeCoerce"
      , sfRisk = "Bypasses type system, can cause segfaults or undefined behavior"
      , sfMitigation = "Use proper type conversions or coerce from Data.Coerce"
      , sfSeverity = Error
      , sfCWE = Just "CWE-704"
      }
    else Nothing

  , -- inlinePerformIO
    if patternInCode "inlinePerformIO" line
    then Just SecurityFinding
      { sfCategory = UnsafeFunction
      , sfSpan = mkSpan path lineNum "inlinePerformIO" line
      , sfCode = T.strip line
      , sfDescription = "Use of inlinePerformIO"
      , sfRisk = "Even more unsafe than unsafePerformIO due to inlining"
      , sfMitigation = "Avoid if possible, ensure strict semantics"
      , sfSeverity = Error
      , sfCWE = Just "CWE-676"
      }
    else Nothing

  , -- accursedUnutterablePerformIO
    if patternInCode "accursedUnutterablePerformIO" line
    then Just SecurityFinding
      { sfCategory = UnsafeFunction
      , sfSpan = mkSpan path lineNum "accursedUnutterablePerformIO" line
      , sfCode = T.strip line
      , sfDescription = "Use of accursedUnutterablePerformIO"
      , sfRisk = "Extremely unsafe, the name says it all"
      , sfMitigation = "Do not use unless you really know what you're doing"
      , sfSeverity = Error
      , sfCWE = Just "CWE-676"
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Injection Risk Detection
--------------------------------------------------------------------------------

-- | Detect injection vulnerability patterns
detectInjectionRisks :: FilePath -> Text -> [SecurityFinding]
detectInjectionRisks path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkInjectionLine path) linesWithNums

checkInjectionLine :: FilePath -> (Int, Text) -> [SecurityFinding]
checkInjectionLine path (lineNum, line)
  | isComment line = []  -- Skip comment lines
  | otherwise = catMaybes
  [ -- SQL injection (string concatenation in queries)
    if hasQueryPattern line && hasStringConcat line
    then Just SecurityFinding
      { sfCategory = InjectionRisk
      , sfSpan = mkSpan path lineNum "query" line
      , sfCode = T.strip line
      , sfDescription = "Potential SQL injection"
      , sfRisk = "String concatenation in SQL queries can lead to SQL injection"
      , sfMitigation = "Use parameterized queries or prepared statements"
      , sfSeverity = Error
      , sfCWE = Just "CWE-89"
      }
    else Nothing

  , -- Shell injection
    if hasShellPattern line && (hasStringConcat line || hasVariable line)
    then Just SecurityFinding
      { sfCategory = InjectionRisk
      , sfSpan = mkSpan path lineNum "shell" line
      , sfCode = T.strip line
      , sfDescription = "Potential shell command injection"
      , sfRisk = "Dynamic shell commands can be exploited for command injection"
      , sfMitigation = "Use typed process APIs with explicit arguments, not shell strings"
      , sfSeverity = Error
      , sfCWE = Just "CWE-78"
      }
    else Nothing

  , -- Path traversal
    if hasPathPattern line && hasVariable line
    then Just SecurityFinding
      { sfCategory = PathTraversal
      , sfSpan = mkSpan path lineNum "</>" line
      , sfCode = T.strip line
      , sfDescription = "Potential path traversal"
      , sfRisk = "Unvalidated path components can access files outside intended directory"
      , sfMitigation = "Validate and sanitize path components, use canonicalizePath"
      , sfSeverity = Warning
      , sfCWE = Just "CWE-22"
      }
    else Nothing

  , -- XSS in HTML generation
    if hasHtmlPattern line && hasVariable line
    then Just SecurityFinding
      { sfCategory = InjectionRisk
      , sfSpan = mkSpan path lineNum "html" line
      , sfCode = T.strip line
      , sfDescription = "Potential XSS vulnerability"
      , sfRisk = "Unescaped user input in HTML can lead to cross-site scripting"
      , sfMitigation = "Use proper HTML escaping libraries (blaze-html, lucid)"
      , sfSeverity = Warning
      , sfCWE = Just "CWE-79"
      }
    else Nothing
  ]

hasQueryPattern :: Text -> Bool
hasQueryPattern line =
  let code = extractCode line
  in any (`T.isInfixOf` code) ["query ", "execute ", "rawQuery", "rawSql", "SELECT ", "INSERT ", "UPDATE ", "DELETE "]

hasShellPattern :: Text -> Bool
hasShellPattern line =
  let code = extractCode line
  in any (`T.isInfixOf` code) ["shell ", "callCommand", "system ", "rawSystem", "proc ", "readProcess"]

hasPathPattern :: Text -> Bool
hasPathPattern line =
  let code = extractCode line
  in " </> " `T.isInfixOf` code || "joinPath" `T.isInfixOf` code

hasHtmlPattern :: Text -> Bool
hasHtmlPattern line =
  let code = extractCode line
  in any (`T.isInfixOf` code) ["toHtml", "rawHtml", "<script", "preEscaped", "toMarkup"]

hasStringConcat :: Text -> Bool
hasStringConcat line =
  let code = extractCode line
  in " ++ " `T.isInfixOf` code || " <> " `T.isInfixOf` code

hasVariable :: Text -> Bool
hasVariable line =
  let code = extractCode line
  in any (`T.isInfixOf` code) [" $ ", "input", "user", "param", "arg", "request"]

--------------------------------------------------------------------------------
-- Cryptographic Issue Detection
--------------------------------------------------------------------------------

-- | Detect cryptographic weaknesses
detectCryptoIssues :: FilePath -> Text -> [SecurityFinding]
detectCryptoIssues path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkCryptoLine path) linesWithNums

checkCryptoLine :: FilePath -> (Int, Text) -> [SecurityFinding]
checkCryptoLine path (lineNum, line)
  | isComment line = []  -- Skip comment lines
  | otherwise = catMaybes
  [ -- System.Random for crypto
    if patternInCode "System.Random" line && any (`T.isInfixOf` content)
       ["password", "token", "secret", "key", "crypto", "encrypt", "hash"]
    then Just SecurityFinding
      { sfCategory = InsecureRandom
      , sfSpan = mkSpan path lineNum "System.Random" line
      , sfCode = T.strip line
      , sfDescription = "System.Random used in security-sensitive context"
      , sfRisk = "System.Random is not cryptographically secure"
      , sfMitigation = "Use Crypto.Random or cryptonite for secure random generation"
      , sfSeverity = Error
      , sfCWE = Just "CWE-338"
      }
    else Nothing

  , -- MD5
    if patternInCode "MD5" line
    then Just SecurityFinding
      { sfCategory = CryptoIssue
      , sfSpan = mkSpan path lineNum "MD5" line
      , sfCode = T.strip line
      , sfDescription = "Use of MD5 hash"
      , sfRisk = "MD5 is cryptographically broken and unsuitable for security"
      , sfMitigation = "Use SHA-256 or stronger from cryptonite"
      , sfSeverity = Warning
      , sfCWE = Just "CWE-328"
      }
    else Nothing

  , -- SHA1
    if patternInCode "SHA1" line && not (patternInCode "256" line)
    then Just SecurityFinding
      { sfCategory = CryptoIssue
      , sfSpan = mkSpan path lineNum "SHA1" line
      , sfCode = T.strip line
      , sfDescription = "Use of SHA-1 hash"
      , sfRisk = "SHA-1 has known weaknesses and should not be used for new applications"
      , sfMitigation = "Use SHA-256 or SHA-3 from cryptonite"
      , sfSeverity = Suggestion
      , sfCWE = Just "CWE-328"
      }
    else Nothing

  , -- DES
    if patternInCode "DES" line && not (patternInCode "3DES" line)
    then Just SecurityFinding
      { sfCategory = CryptoIssue
      , sfSpan = mkSpan path lineNum "DES" line
      , sfCode = T.strip line
      , sfDescription = "Use of DES encryption"
      , sfRisk = "DES is insecure due to small key size"
      , sfMitigation = "Use AES-256 from cryptonite"
      , sfSeverity = Error
      , sfCWE = Just "CWE-327"
      }
    else Nothing
  ]
  where
    content = line  -- Would need full file context

--------------------------------------------------------------------------------
-- Hardcoded Secret Detection
--------------------------------------------------------------------------------

-- | Detect hardcoded secrets and credentials
detectHardcodedSecrets :: FilePath -> Text -> [SecurityFinding]
detectHardcodedSecrets path content =
  let linesWithNums = zip [1..] (T.lines content)
  in mapMaybe (checkSecretLine path) linesWithNums

checkSecretLine :: FilePath -> (Int, Text) -> Maybe SecurityFinding
checkSecretLine path (lineNum, line)
  | isComment line = Nothing  -- Skip comment lines
  -- Pattern: variable with secret name assigned to string literal
  | hasSecretName line && hasStringLiteral line = Just SecurityFinding
      { sfCategory = HardcodedSecret
      , sfSpan = mkSpan path lineNum "secret" line
      , sfCode = maskSecret $ T.strip line
      , sfDescription = "Potential hardcoded secret"
      , sfRisk = "Hardcoded credentials can be extracted from source code or binaries"
      , sfMitigation = "Use environment variables or a secrets management system"
      , sfSeverity = Error
      , sfCWE = Just "CWE-798"
      }
  | otherwise = Nothing

hasSecretName :: Text -> Bool
hasSecretName line =
  let lower = T.toLower line
  in any (`T.isInfixOf` lower)
     ["password", "passwd", "secret", "apikey", "api_key", "token", "credential",
      "private_key", "privatekey", "auth"]

hasStringLiteral :: Text -> Bool
hasStringLiteral line =
  -- Check for = followed by a string
  " = \"" `T.isInfixOf` line && T.length (T.filter (== '"') line) >= 2

-- | Mask the actual secret value in the diagnostic
maskSecret :: Text -> Text
maskSecret line =
  case T.breakOn "\"" line of
    (before, rest) ->
      case T.breakOn "\"" (T.drop 1 rest) of
        (_, after) -> before <> "\"***MASKED***" <> after

--------------------------------------------------------------------------------
-- Unsafe FFI Detection
--------------------------------------------------------------------------------

-- | Detect unsafe FFI usage
detectUnsafeFFI :: FilePath -> Text -> [SecurityFinding]
detectUnsafeFFI path content =
  let linesWithNums = zip [1..] (T.lines content)
  in mapMaybe (checkFFILine path) linesWithNums

checkFFILine :: FilePath -> (Int, Text) -> Maybe SecurityFinding
checkFFILine path (lineNum, line)
  -- Pure FFI import (no IO in return type)
  | patternInCode "foreign import" line && not (patternInCode "IO " line) = Just SecurityFinding
      { sfCategory = UnsafeFFI
      , sfSpan = mkSpan path lineNum "foreign" line
      , sfCode = T.strip line
      , sfDescription = "FFI import with pure type"
      , sfRisk = "FFI calls with pure types bypass Haskell's safety guarantees"
      , sfMitigation = "Use IO in FFI return types unless you're certain the function is pure"
      , sfSeverity = Warning
      , sfCWE = Just "CWE-676"
      }
  -- unsafe FFI call
  | patternInCode "foreign import" line && patternInCode "unsafe" line = Just SecurityFinding
      { sfCategory = UnsafeFFI
      , sfSpan = mkSpan path lineNum "unsafe" line
      , sfCode = T.strip line
      , sfDescription = "Unsafe FFI call"
      , sfRisk = "Unsafe FFI calls cannot be interrupted and may cause issues"
      , sfMitigation = "Use safe FFI unless performance is critical and call is very fast"
      , sfSeverity = Suggestion
      , sfCWE = Nothing
      }
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Debug Code Detection
--------------------------------------------------------------------------------

-- | Detect debug code that shouldn't be in production
detectDebugCode :: FilePath -> Text -> [SecurityFinding]
detectDebugCode path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkDebugLine path) linesWithNums

checkDebugLine :: FilePath -> (Int, Text) -> [SecurityFinding]
checkDebugLine path (lineNum, line) =
  let todoFindings = checkTodoFindings
      codeFindings = if isComment line then [] else checkCodeFindings
  in todoFindings ++ codeFindings
  where
    -- TODO/FIXME with security implications - check in comments (original line)
    checkTodoFindings :: [SecurityFinding]
    checkTodoFindings = catMaybes
      [ if any (`T.isInfixOf` T.toLower line) ["todo", "fixme", "hack", "xxx"] &&
           any (`T.isInfixOf` T.toLower line) ["security", "auth", "password", "encrypt", "vulnerability"]
        then Just SecurityFinding
          { sfCategory = DebugCode
          , sfSpan = mkSpan path lineNum "TODO" line
          , sfCode = T.strip line
          , sfDescription = "Security-related TODO/FIXME"
          , sfRisk = "Unresolved security items may indicate vulnerabilities"
          , sfMitigation = "Address security TODOs before deployment"
          , sfSeverity = Warning
          , sfCWE = Nothing
          }
        else Nothing
      ]

    -- Debug code checks - skip comment lines
    checkCodeFindings :: [SecurityFinding]
    checkCodeFindings = catMaybes
      [ -- Debug.Trace import
        if patternInCode "import Debug.Trace" line
        then Just SecurityFinding
          { sfCategory = DebugCode
          , sfSpan = mkSpan path lineNum "Debug.Trace" line
          , sfCode = T.strip line
          , sfDescription = "Debug.Trace import in code"
          , sfRisk = "Debug traces can leak sensitive information and impact performance"
          , sfMitigation = "Remove Debug.Trace import before deployment"
          , sfSeverity = Warning
          , sfCWE = Just "CWE-489"
          }
        else Nothing

      , -- trace function
        if patternInCode "trace " line && not (patternInCode "Debug.Trace" line)
        then Just SecurityFinding
          { sfCategory = DebugCode
          , sfSpan = mkSpan path lineNum "trace" line
          , sfCode = T.strip line
          , sfDescription = "Debug trace in code"
          , sfRisk = "Debug traces can leak sensitive information and impact performance"
          , sfMitigation = "Remove debug traces before deployment"
          , sfSeverity = Warning
          , sfCWE = Just "CWE-489"
          }
        else Nothing

      , -- traceShow
        if patternInCode "traceShow " line
        then Just SecurityFinding
          { sfCategory = DebugCode
          , sfSpan = mkSpan path lineNum "traceShow" line
          , sfCode = T.strip line
          , sfDescription = "Debug traceShow in code"
          , sfRisk = "Debug traces can leak sensitive information and impact performance"
          , sfMitigation = "Remove debug traces before deployment"
          , sfSeverity = Warning
          , sfCWE = Just "CWE-489"
          }
        else Nothing

      , -- traceShowId - can be auto-fixed to id
        if patternInCode "traceShowId " line
        then Just SecurityFinding
          { sfCategory = DebugCode
          , sfSpan = mkSpan path lineNum "traceShowId" line
          , sfCode = T.strip line
          , sfDescription = "Debug traceShowId in code"
          , sfRisk = "Debug traces can leak sensitive information and impact performance"
          , sfMitigation = "Remove debug traces before deployment"
          , sfSeverity = Warning
          , sfCWE = Just "CWE-489"
          }
        else Nothing

      , -- traceM
        if patternInCode "traceM " line
        then Just SecurityFinding
          { sfCategory = DebugCode
          , sfSpan = mkSpan path lineNum "traceM" line
          , sfCode = T.strip line
          , sfDescription = "Debug traceM in code"
          , sfRisk = "Debug traces can leak sensitive information and impact performance"
          , sfMitigation = "Remove debug traces before deployment"
          , sfSeverity = Warning
          , sfCWE = Just "CWE-489"
          }
        else Nothing

      , -- traceId - can be auto-fixed to id
        if patternInCode "traceId " line
        then Just SecurityFinding
          { sfCategory = DebugCode
          , sfSpan = mkSpan path lineNum "traceId" line
          , sfCode = T.strip line
          , sfDescription = "Debug traceId in code"
          , sfRisk = "Debug traces can leak sensitive information and impact performance"
          , sfMitigation = "Replace with id or remove"
          , sfSeverity = Warning
          , sfCWE = Just "CWE-489"
          }
        else Nothing
      ]

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Create a span for a pattern match
mkSpan :: FilePath -> Int -> Text -> Text -> SrcSpan
mkSpan path lineNum needle line =
  let col = case T.breakOn needle line of
              (before, _) -> T.length before + 1
  in mkSrcSpanRaw path lineNum col lineNum (col + T.length needle)

-- | Convert finding to diagnostic
issueToDiagnostic :: SecurityFinding -> Diagnostic
issueToDiagnostic SecurityFinding{..} = Diagnostic
  { diagSpan = sfSpan
  , diagSeverity = sfSeverity
  , diagKind = SecurityIssue
  , diagMessage = sfDescription <> ": " <> sfRisk <> ". " <> sfMitigation
  , diagCode = Just $ "security/" <> categoryCode sfCategory <>
               maybe "" (\cwe -> " [" <> cwe <> "]") sfCWE
  , diagFixes = generateSecurityFixes sfCategory sfSpan sfCode
  , diagRelated = []
  }

-- | Generate auto-fixes for security issues (primarily debug code removal)
-- Strategy: Replace just the pattern span with minimal text change
generateSecurityFixes :: SecurityCategory -> SrcSpan -> Text -> [Fix]
generateSecurityFixes DebugCode srcSpan code
  -- traceShowId x → x (remove "traceShowId " leaving the argument)
  -- The srcSpan covers "traceShowId", extend by 1 to include trailing space
  | "traceShowId " `T.isInfixOf` code =
      let patternSpan = extendSpan srcSpan 1  -- Include the trailing space
      in [mkSecurityFix "Remove traceShowId" [FixEdit patternSpan ""] True]
  -- traceId x → x
  | "traceId " `T.isInfixOf` code =
      let patternSpan = extendSpan srcSpan 1  -- Include trailing space
      in [mkSecurityFix "Remove traceId" [FixEdit patternSpan ""] True]
  -- trace "msg" x → x  (need to remove trace, message, and space)
  -- This is harder because we need to find the end of the message string
  | "trace \"" `T.isInfixOf` code =
      []  -- Complex pattern, needs AST-based handling
  -- traceShow x y → y (harder to fix automatically)
  | "traceShow " `T.isInfixOf` code =
      []  -- Needs more context to fix safely
  -- traceM "msg" → pure () (in do-blocks)
  -- Just remove "traceM " and let user add pure () if needed
  | "traceM " `T.isInfixOf` code =
      let patternSpan = extendSpan srcSpan 1
      in [mkSecurityFix "Remove traceM" [FixEdit patternSpan "pure () -- removed: traceM "] False]
  -- Debug.Trace import line → remove entire line
  -- For imports, the srcSpan covers "Debug.Trace", but we want to remove the whole import
  | "import Debug.Trace" `T.isInfixOf` code =
      -- Can't easily get line length here, skip auto-fix for now
      -- The AST-based rules in ConfigurableRules handle this better
      []
  | otherwise = []
generateSecurityFixes _ _ _ = []

-- | Extend a span's end column by n characters
extendSpan :: SrcSpan -> Int -> SrcSpan
extendSpan srcSpan n = mkSrcSpanRaw
  (srcSpanFile srcSpan)
  (srcSpanStartLineRaw srcSpan)
  (srcSpanStartColRaw srcSpan)
  (srcSpanEndLineRaw srcSpan)
  (srcSpanEndColRaw srcSpan + n)

-- | Helper to create a security fix with proper category and safety
mkSecurityFix :: Text -> [FixEdit] -> Bool -> Fix
mkSecurityFix title edits preferred = Fix
  { fixTitle = title
  , fixEdits = edits
  , fixIsPreferred = preferred
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCSecurity
  , fixSafety = FSReview
  }

categoryCode :: SecurityCategory -> Text
categoryCode = \case
  UnsafeFunction -> "unsafe-function"
  InjectionRisk -> "injection"
  CryptoIssue -> "crypto"
  HardcodedSecret -> "hardcoded-secret"
  PathTraversal -> "path-traversal"
  UnsafeFFI -> "unsafe-ffi"
  DebugCode -> "debug-code"
  InsecureRandom -> "insecure-random"
  UnsafeCoercion -> "unsafe-coerce"
  TemplateHaskellRisk -> "template-haskell"
