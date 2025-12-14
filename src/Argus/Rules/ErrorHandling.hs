{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Rules.ErrorHandling
-- Description : Detect error handling anti-patterns in Haskell code
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module detects common error handling issues including:
--
-- * Exception handling anti-patterns
-- * Missing error cases
-- * Either/Maybe misuse
-- * Improper error propagation
-- * Resource cleanup issues
module Argus.Rules.ErrorHandling
  ( -- * Detection
    detectErrorHandlingIssues
  , ErrorHandlingFinding (..)
  , ErrorHandlingCategory (..)

    -- * Configuration
  , ErrorHandlingConfig (..)
  , defaultErrorHandlingConfig
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Argus.Types

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Category of error handling issue
data ErrorHandlingCategory
  = BroadException        -- ^ Catching overly broad exceptions
  | SilentFailure         -- ^ Silently ignoring errors
  | PartialPattern        -- ^ Partial pattern matching
  | ImproperPropagation   -- ^ Not propagating errors properly
  | ResourceCleanup       -- ^ Missing cleanup on error
  | MonadErrorMisuse      -- ^ MonadError/ExceptT misuse
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A detected error handling issue
data ErrorHandlingFinding = ErrorHandlingFinding
  { ehCategory    :: ErrorHandlingCategory
  , ehSpan        :: SrcSpan
  , ehCode        :: Text           -- ^ Original code
  , ehExplanation :: Text           -- ^ Why it's problematic
  , ehSuggestion  :: Maybe Text     -- ^ Suggested fix
  , ehSeverity    :: Severity
  , ehAutoFix     :: [Fix]          -- ^ Auto-fix if available
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for error handling detection
data ErrorHandlingConfig = ErrorHandlingConfig
  { ehEnabled            :: Bool   -- ^ Enable detection
  , ehCheckBroadCatch    :: Bool   -- ^ Check for overly broad exception catching
  , ehCheckSilent        :: Bool   -- ^ Check for silently ignored errors
  , ehCheckPartial       :: Bool   -- ^ Check for partial patterns
  , ehCheckPropagation   :: Bool   -- ^ Check for error propagation issues
  , ehCheckCleanup       :: Bool   -- ^ Check for resource cleanup
  , ehCheckMonadError    :: Bool   -- ^ Check for MonadError misuse
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration
defaultErrorHandlingConfig :: ErrorHandlingConfig
defaultErrorHandlingConfig = ErrorHandlingConfig
  { ehEnabled = True
  , ehCheckBroadCatch = True
  , ehCheckSilent = True
  , ehCheckPartial = True
  , ehCheckPropagation = True
  , ehCheckCleanup = True
  , ehCheckMonadError = True
  }

--------------------------------------------------------------------------------
-- Detection
--------------------------------------------------------------------------------

-- | Detect error handling issues in source code
detectErrorHandlingIssues :: ErrorHandlingConfig -> FilePath -> Text -> [Diagnostic]
detectErrorHandlingIssues config path content
  | not (ehEnabled config) = []
  | otherwise =
    let findings = concat
          [ if ehCheckBroadCatch config then detectBroadCatch path content else []
          , if ehCheckSilent config then detectSilentFailure path content else []
          , if ehCheckPartial config then detectPartialPatterns path content else []
          , if ehCheckPropagation config then detectPropagation path content else []
          , if ehCheckCleanup config then detectCleanup path content else []
          , if ehCheckMonadError config then detectMonadError path content else []
          ]
    in map findingToDiagnostic findings

--------------------------------------------------------------------------------
-- Broad Exception Catching
--------------------------------------------------------------------------------

-- | Detect overly broad exception catching
detectBroadCatch :: FilePath -> Text -> [ErrorHandlingFinding]
detectBroadCatch path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkBroadCatchLine path) linesWithNums

checkBroadCatchLine :: FilePath -> (Int, Text) -> [ErrorHandlingFinding]
checkBroadCatchLine path (lineNum, line) = catMaybes
  [ -- catch SomeException
    if "SomeException" `T.isInfixOf` line && "catch" `T.isInfixOf` line
    then Just ErrorHandlingFinding
      { ehCategory = BroadException
      , ehSpan = mkSpan path lineNum line "SomeException"
      , ehCode = T.strip line
      , ehExplanation = "Catching 'SomeException' is too broad and catches all exceptions"
      , ehSuggestion = Just "Catch specific exception types or use 'catches' for multiple"
      , ehSeverity = Warning
      , ehAutoFix = []
      }
    else Nothing

  , -- catch _ (wildcard handler)
    if "catch " `T.isInfixOf` line && "_ ->" `T.isInfixOf` line
    then Just ErrorHandlingFinding
      { ehCategory = BroadException
      , ehSpan = mkSpan path lineNum line "catch"
      , ehCode = T.strip line
      , ehExplanation = "Wildcard exception handler catches all exceptions"
      , ehSuggestion = Just "Specify the exception type explicitly"
      , ehSeverity = Warning
      , ehAutoFix = []
      }
    else Nothing

  , -- handle without type
    if "handle " `T.isInfixOf` line && ":: " `T.isInfixOf` line
       && "SomeException" `T.isInfixOf` line
    then Just ErrorHandlingFinding
      { ehCategory = BroadException
      , ehSpan = mkSpan path lineNum line "handle"
      , ehCode = T.strip line
      , ehExplanation = "Using 'handle' with 'SomeException' catches too broadly"
      , ehSuggestion = Just "Use specific exception types in handlers"
      , ehSeverity = Warning
      , ehAutoFix = []
      }
    else Nothing

  , -- catchIOError without check
    if "catchIOError" `T.isInfixOf` line && "const" `T.isInfixOf` line
    then Just ErrorHandlingFinding
      { ehCategory = BroadException
      , ehSpan = mkSpan path lineNum line "catchIOError"
      , ehCode = T.strip line
      , ehExplanation = "Ignoring IOError details loses important error information"
      , ehSuggestion = Just "Examine the IOError to handle different cases appropriately"
      , ehSeverity = Suggestion
      , ehAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Silent Failure Detection
--------------------------------------------------------------------------------

-- | Detect silently ignored errors
detectSilentFailure :: FilePath -> Text -> [ErrorHandlingFinding]
detectSilentFailure path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkSilentLine path) linesWithNums

checkSilentLine :: FilePath -> (Int, Text) -> [ErrorHandlingFinding]
checkSilentLine path (lineNum, line) = catMaybes
  [ -- void $ try
    if "void" `T.isInfixOf` line && "try" `T.isInfixOf` line
    then Just ErrorHandlingFinding
      { ehCategory = SilentFailure
      , ehSpan = mkSpan path lineNum line "void"
      , ehCode = T.strip line
      , ehExplanation = "'void $ try' discards exception information"
      , ehSuggestion = Just "Handle the Either result from 'try' explicitly"
      , ehSeverity = Warning
      , ehAutoFix = []
      }
    else Nothing

  , -- catch returning ()
    if "catch" `T.isInfixOf` line && "return ()" `T.isInfixOf` line
    then Just ErrorHandlingFinding
      { ehCategory = SilentFailure
      , ehSpan = mkSpan path lineNum line "catch"
      , ehCode = T.strip line
      , ehExplanation = "Exception handler returning unit silently ignores errors"
      , ehSuggestion = Just "Log the error or re-throw a more specific exception"
      , ehSeverity = Warning
      , ehAutoFix = []
      }
    else Nothing

  , -- _ <- runExceptT
    if "_ <-" `T.isInfixOf` line && "runExceptT" `T.isInfixOf` line
    then Just ErrorHandlingFinding
      { ehCategory = SilentFailure
      , ehSpan = mkSpan path lineNum line "runExceptT"
      , ehCode = T.strip line
      , ehExplanation = "Discarding 'ExceptT' result ignores potential errors"
      , ehSuggestion = Just "Handle the Either result from 'runExceptT'"
      , ehSeverity = Warning
      , ehAutoFix = []
      }
    else Nothing

  , -- Maybe defaulting silently
    if "fromMaybe" `T.isInfixOf` line && ("\"\"" `T.isInfixOf` line || "0" `T.isInfixOf` line)
    then Just ErrorHandlingFinding
      { ehCategory = SilentFailure
      , ehSpan = mkSpan path lineNum line "fromMaybe"
      , ehCode = T.strip line
      , ehExplanation = "Using a silent default may hide unexpected Nothing values"
      , ehSuggestion = Just "Consider logging when the default is used, or handle Nothing explicitly"
      , ehSeverity = Suggestion
      , ehAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Partial Pattern Detection
--------------------------------------------------------------------------------

-- | Detect partial pattern matching
detectPartialPatterns :: FilePath -> Text -> [ErrorHandlingFinding]
detectPartialPatterns path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkPartialLine path) linesWithNums

checkPartialLine :: FilePath -> (Int, Text) -> [ErrorHandlingFinding]
checkPartialLine path (lineNum, line) = catMaybes
  [ -- let Just x = ...
    if "let Just " `T.isInfixOf` line && " = " `T.isInfixOf` line
       && not ("case" `T.isInfixOf` line)
    then Just ErrorHandlingFinding
      { ehCategory = PartialPattern
      , ehSpan = mkSpan path lineNum line "Just"
      , ehCode = T.strip line
      , ehExplanation = "Partial pattern 'let Just' will crash on Nothing"
      , ehSuggestion = Just "Use 'case' expression or 'maybe' function to handle Nothing"
      , ehSeverity = Warning
      , ehAutoFix = []
      }
    else Nothing

  , -- let Right x = ...
    if "let Right " `T.isInfixOf` line && " = " `T.isInfixOf` line
       && not ("case" `T.isInfixOf` line)
    then Just ErrorHandlingFinding
      { ehCategory = PartialPattern
      , ehSpan = mkSpan path lineNum line "Right"
      , ehCode = T.strip line
      , ehExplanation = "Partial pattern 'let Right' will crash on Left"
      , ehSuggestion = Just "Use 'case' expression or 'either' function"
      , ehSeverity = Warning
      , ehAutoFix = []
      }
    else Nothing

  , -- let (x:xs) = ...
    if "let (" `T.isInfixOf` line && ":_)" `T.isInfixOf` line
    then Just ErrorHandlingFinding
      { ehCategory = PartialPattern
      , ehSpan = mkSpan path lineNum line "let"
      , ehCode = T.strip line
      , ehExplanation = "Partial pattern on list will crash on empty list"
      , ehSuggestion = Just "Use 'uncons' or 'case' expression to handle empty list"
      , ehSeverity = Warning
      , ehAutoFix = []
      }
    else Nothing

  , -- pattern match without _ or complete cases
    if "case " `T.isInfixOf` line && " of" `T.isInfixOf` line
       && not ("_" `T.isInfixOf` line)
       && not (isComment line)
    then Just ErrorHandlingFinding
      { ehCategory = PartialPattern
      , ehSpan = mkSpan path lineNum line "case"
      , ehCode = T.strip line
      , ehExplanation = "Case expression may not be exhaustive"
      , ehSuggestion = Just "Add a catch-all pattern '_' or ensure all constructors are handled"
      , ehSeverity = Suggestion
      , ehAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Error Propagation Detection
--------------------------------------------------------------------------------

-- | Detect improper error propagation
detectPropagation :: FilePath -> Text -> [ErrorHandlingFinding]
detectPropagation path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkPropagationLine path) linesWithNums

checkPropagationLine :: FilePath -> (Int, Text) -> [ErrorHandlingFinding]
checkPropagationLine path (lineNum, line) = catMaybes
  [ -- throwError with string
    if "throwError" `T.isInfixOf` line && "\"" `T.isInfixOf` line
    then Just ErrorHandlingFinding
      { ehCategory = ImproperPropagation
      , ehSpan = mkSpan path lineNum line "throwError"
      , ehCode = T.strip line
      , ehExplanation = "String errors lose type safety and structured information"
      , ehSuggestion = Just "Use a custom error type instead of String"
      , ehSeverity = Suggestion
      , ehAutoFix = []
      }
    else Nothing

  , -- error with string
    if "error \"" `T.isInfixOf` line && not (isComment line)
    then Just ErrorHandlingFinding
      { ehCategory = ImproperPropagation
      , ehSpan = mkSpan path lineNum line "error"
      , ehCode = T.strip line
      , ehExplanation = "'error' throws an exception that cannot be caught safely"
      , ehSuggestion = Just "Use 'throwIO' with a specific exception type, or return Either/Maybe"
      , ehSeverity = Warning
      , ehAutoFix = []
      }
    else Nothing

  , -- fail with string in IO
    if "fail \"" `T.isInfixOf` line && not (isComment line)
    then Just ErrorHandlingFinding
      { ehCategory = ImproperPropagation
      , ehSpan = mkSpan path lineNum line "fail"
      , ehCode = T.strip line
      , ehExplanation = "'fail' behavior varies by monad, use explicit error handling"
      , ehSuggestion = Just "Use 'throwIO' for IO, 'throwError' for ExceptT"
      , ehSeverity = Warning
      , ehAutoFix = []
      }
    else Nothing

  , -- Left with plain string
    if "Left \"" `T.isInfixOf` line
    then Just ErrorHandlingFinding
      { ehCategory = ImproperPropagation
      , ehSpan = mkSpan path lineNum line "Left"
      , ehCode = T.strip line
      , ehExplanation = "String errors in Either lose structure and type safety"
      , ehSuggestion = Just "Use a custom error ADT for the Left case"
      , ehSeverity = Suggestion
      , ehAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Resource Cleanup Detection
--------------------------------------------------------------------------------

-- | Detect missing resource cleanup on error
detectCleanup :: FilePath -> Text -> [ErrorHandlingFinding]
detectCleanup path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkCleanupLine path) linesWithNums

checkCleanupLine :: FilePath -> (Int, Text) -> [ErrorHandlingFinding]
checkCleanupLine path (lineNum, line) = catMaybes
  [ -- open without bracket
    if ("openFile" `T.isInfixOf` line || "open" `T.isInfixOf` T.toLower line)
       && not ("withFile" `T.isInfixOf` line) && not ("bracket" `T.isInfixOf` line)
       && not (isComment line)
    then Just ErrorHandlingFinding
      { ehCategory = ResourceCleanup
      , ehSpan = mkSpan path lineNum line "open"
      , ehCode = T.strip line
      , ehExplanation = "File handle may not be closed on exception"
      , ehSuggestion = Just "Use 'withFile' or 'bracket openFile hClose' for safe handling"
      , ehSeverity = Warning
      , ehAutoFix = []
      }
    else Nothing

  , -- allocate without bracket
    if ("malloc" `T.isInfixOf` line || "new " `T.isInfixOf` line)
       && not ("bracket" `T.isInfixOf` line) && not ("finally" `T.isInfixOf` line)
    then Just ErrorHandlingFinding
      { ehCategory = ResourceCleanup
      , ehSpan = mkSpan path lineNum line "alloc"
      , ehCode = T.strip line
      , ehExplanation = "Memory allocation without guaranteed cleanup"
      , ehSuggestion = Just "Use 'bracket malloc free' or 'alloca' for stack allocation"
      , ehSeverity = Warning
      , ehAutoFix = []
      }
    else Nothing

  , -- acquire without release pattern
    if "acquire" `T.isInfixOf` T.toLower line
       && not ("bracket" `T.isInfixOf` line) && not ("release" `T.isInfixOf` T.toLower line)
    then Just ErrorHandlingFinding
      { ehCategory = ResourceCleanup
      , ehSpan = mkSpan path lineNum line "acquire"
      , ehCode = T.strip line
      , ehExplanation = "Resource acquisition without visible cleanup"
      , ehSuggestion = Just "Use bracket pattern: 'bracket acquire release use'"
      , ehSeverity = Suggestion
      , ehAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- MonadError Misuse Detection
--------------------------------------------------------------------------------

-- | Detect MonadError/ExceptT misuse
detectMonadError :: FilePath -> Text -> [ErrorHandlingFinding]
detectMonadError path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkMonadErrorLine path) linesWithNums

checkMonadErrorLine :: FilePath -> (Int, Text) -> [ErrorHandlingFinding]
checkMonadErrorLine path (lineNum, line) = catMaybes
  [ -- liftIO throwIO inside ExceptT
    if "liftIO" `T.isInfixOf` line && "throwIO" `T.isInfixOf` line
    then Just ErrorHandlingFinding
      { ehCategory = MonadErrorMisuse
      , ehSpan = mkSpan path lineNum line "throwIO"
      , ehCode = T.strip line
      , ehExplanation = "'liftIO . throwIO' in ExceptT bypasses error handling"
      , ehSuggestion = Just "Use 'throwError' to keep errors in the ExceptT layer"
      , ehSeverity = Warning
      , ehAutoFix = []
      }
    else Nothing

  , -- ExceptT . pure . Left
    if "ExceptT" `T.isInfixOf` line && "pure" `T.isInfixOf` line && "Left" `T.isInfixOf` line
    then Just ErrorHandlingFinding
      { ehCategory = MonadErrorMisuse
      , ehSpan = mkSpan path lineNum line "ExceptT"
      , ehCode = T.strip line
      , ehExplanation = "'ExceptT . pure . Left' is verbose"
      , ehSuggestion = Just "Use 'throwError' instead"
      , ehSeverity = Suggestion
      , ehAutoFix = []
      }
    else Nothing

  , -- runExceptT immediately followed by case
    if "runExceptT" `T.isInfixOf` line && ">>" `T.isInfixOf` line
    then Just ErrorHandlingFinding
      { ehCategory = MonadErrorMisuse
      , ehSpan = mkSpan path lineNum line "runExceptT"
      , ehCode = T.strip line
      , ehExplanation = "Chaining after runExceptT ignores the error"
      , ehSuggestion = Just "Bind and handle the Either result explicitly"
      , ehSeverity = Warning
      , ehAutoFix = []
      }
    else Nothing

  , -- catchError with throwError
    if "catchError" `T.isInfixOf` line && "throwError" `T.isInfixOf` line
       && not ("$" `T.isInfixOf` line)  -- Allow transformation
    then Just ErrorHandlingFinding
      { ehCategory = MonadErrorMisuse
      , ehSpan = mkSpan path lineNum line "catchError"
      , ehCode = T.strip line
      , ehExplanation = "Catching and immediately re-throwing may be unnecessary"
      , ehSuggestion = Just "Consider if the error transformation adds value"
      , ehSeverity = Suggestion
      , ehAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Convert finding to diagnostic
findingToDiagnostic :: ErrorHandlingFinding -> Diagnostic
findingToDiagnostic ErrorHandlingFinding{..} = Diagnostic
  { diagSpan = ehSpan
  , diagSeverity = ehSeverity
  , diagKind = CodePattern
  , diagMessage = ehExplanation <> maybe "" (\s -> ". " <> s) ehSuggestion
  , diagCode = Just $ "error-handling/" <> categoryToCode ehCategory
  , diagFixes = ehAutoFix
  , diagRelated = []
  }

-- | Convert category to diagnostic code suffix
categoryToCode :: ErrorHandlingCategory -> Text
categoryToCode = \case
  BroadException -> "broad-catch"
  SilentFailure -> "silent-failure"
  PartialPattern -> "partial-pattern"
  ImproperPropagation -> "propagation"
  ResourceCleanup -> "resource-cleanup"
  MonadErrorMisuse -> "monad-error"

-- | Create span for a keyword match
mkSpan :: FilePath -> Int -> Text -> Text -> SrcSpan
mkSpan path lineNum line keyword =
  let col = maybe 1 (+ 1) $ T.findIndex (== T.head keyword) line
  in mkSrcSpanRaw path lineNum col lineNum (col + T.length keyword)

-- | Check if a line is a comment
isComment :: Text -> Bool
isComment line =
  let stripped = T.stripStart line
  in "--" `T.isPrefixOf` stripped || "{-" `T.isPrefixOf` stripped
