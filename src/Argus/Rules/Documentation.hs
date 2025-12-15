{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Rules.Documentation
-- Description : Detect documentation issues in Haskell code
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module detects documentation quality issues including:
--
-- * Missing module documentation
-- * Missing function documentation
-- * Incomplete Haddock documentation
-- * Stale or misleading documentation
-- * Missing examples in documentation
module Argus.Rules.Documentation
  ( -- * Detection
    detectDocumentationIssues
  , DocumentationFinding (..)
  , DocumentationCategory (..)

    -- * Configuration
  , DocumentationConfig (..)
  , defaultDocumentationConfig
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Argus.Types

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Category of documentation issue
data DocumentationCategory
  = MissingModuleDoc      -- ^ Missing module documentation
  | MissingFunctionDoc    -- ^ Missing function documentation
  | MissingTypeDoc        -- ^ Missing type documentation
  | IncompleteDoc         -- ^ Incomplete Haddock (missing fields, etc.)
  | StaleDoc              -- ^ Documentation that may be stale
  | MissingExample        -- ^ Missing usage examples
  | TodoInDoc             -- ^ TODO/FIXME in documentation
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A detected documentation issue
data DocumentationFinding = DocumentationFinding
  { dfCategory    :: DocumentationCategory
  , dfSpan        :: SrcSpan
  , dfName        :: Text           -- ^ Name of undocumented item
  , dfExplanation :: Text           -- ^ Description of the issue
  , dfSeverity    :: Severity
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for documentation detection
data DocumentationConfig = DocumentationConfig
  { dcEnabled              :: Bool   -- ^ Enable detection
  , dcCheckModuleDoc       :: Bool   -- ^ Check for module documentation
  , dcCheckFunctionDoc     :: Bool   -- ^ Check for function documentation
  , dcCheckTypeDoc         :: Bool   -- ^ Check for type documentation
  , dcCheckIncomplete      :: Bool   -- ^ Check for incomplete docs
  , dcCheckStale           :: Bool   -- ^ Check for stale documentation
  , dcCheckExamples        :: Bool   -- ^ Check for missing examples
  , dcCheckTodos           :: Bool   -- ^ Check for TODOs in documentation
  , dcMinExportedFunctions :: Int    -- ^ Min exported functions to require docs
  , dcExportedOnly         :: Bool   -- ^ Only check exported items
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration
defaultDocumentationConfig :: DocumentationConfig
defaultDocumentationConfig = DocumentationConfig
  { dcEnabled = True
  , dcCheckModuleDoc = True
  , dcCheckFunctionDoc = True
  , dcCheckTypeDoc = True
  , dcCheckIncomplete = True
  , dcCheckStale = True
  , dcCheckExamples = False  -- Off by default, can be noisy
  , dcCheckTodos = True
  , dcMinExportedFunctions = 3
  , dcExportedOnly = True
  }

--------------------------------------------------------------------------------
-- Detection
--------------------------------------------------------------------------------

-- | Detect documentation issues in source code
detectDocumentationIssues :: DocumentationConfig -> FilePath -> Text -> [Diagnostic]
detectDocumentationIssues config path content
  | not (dcEnabled config) = []
  | otherwise =
    let findings = concat
          [ if dcCheckModuleDoc config then detectMissingModuleDoc path content else []
          , if dcCheckFunctionDoc config then detectMissingFunctionDoc config path content else []
          , if dcCheckTypeDoc config then detectMissingTypeDoc config path content else []
          , if dcCheckIncomplete config then detectIncompleteDoc path content else []
          , if dcCheckStale config then detectStaleDoc path content else []
          , if dcCheckExamples config then detectMissingExamples path content else []
          , if dcCheckTodos config then detectTodosInDoc path content else []
          ]
    in map findingToDiagnostic findings

--------------------------------------------------------------------------------
-- Module Documentation
--------------------------------------------------------------------------------

-- | Detect missing module documentation
detectMissingModuleDoc :: FilePath -> Text -> [DocumentationFinding]
detectMissingModuleDoc path content =
  let hasModuleDoc = "-- |" `T.isInfixOf` content && "Module" `T.isInfixOf` content
      hasModuleDecl = "module " `T.isInfixOf` content
      moduleLines = filter (T.isInfixOf "module ") (T.lines content)
      moduleName = case moduleLines of
        (l:_) -> T.strip $ T.takeWhile (\c -> c /= '(' && c /= '\n') $
                   T.drop (T.length "module ") $ T.stripStart l
        [] -> "Unknown"
  in if hasModuleDecl && not hasModuleDoc
     then [DocumentationFinding
       { dfCategory = MissingModuleDoc
       , dfSpan = mkSrcSpanRaw path 1 1 1 1
       , dfName = moduleName
       , dfExplanation = "Module '" <> moduleName <> "' is missing Haddock documentation"
       , dfSeverity = Warning
       }]
     else []

--------------------------------------------------------------------------------
-- Function Documentation
--------------------------------------------------------------------------------

-- | Detect missing function documentation
detectMissingFunctionDoc :: DocumentationConfig -> FilePath -> Text -> [DocumentationFinding]
detectMissingFunctionDoc config path content =
  let exports = extractExports content
      functionSigs = extractFunctionSignatures path content
      -- Filter to exported functions if configured
      toCheck = if dcExportedOnly config
                then filter (\(name, _, _) -> name `elem` exports || null exports) functionSigs
                else functionSigs
  in if length exports >= dcMinExportedFunctions config
     then mapMaybe (checkFunctionDoc content) toCheck
     else []

-- | Extract exported names from module header
extractExports :: Text -> [Text]
extractExports content =
  let lines' = T.lines content
      moduleHeaderLines = takeWhile (not . T.isInfixOf "where") lines'
      headerText = T.unlines moduleHeaderLines
      -- Simple extraction: look for names in parentheses
      inParens = case T.breakOn "(" headerText of
        (_, rest) ->
          let insideParens = T.takeWhile (/= ')') (T.drop 1 rest)
          in map T.strip $ T.splitOn "," insideParens
  in filter (not . T.null) inParens

-- | Extract function signatures with locations
extractFunctionSignatures :: FilePath -> Text -> [(Text, Int, Bool)]
extractFunctionSignatures _path content =
  let linesWithNums = zip [1..] (T.lines content)
      findSigs (lineNum, line)
        | " :: " `T.isInfixOf` line && not (isComment line) && not (isInDataDecl line) =
          let name = T.strip $ T.takeWhile (\c -> c /= ':' && c /= ' ') $
                     T.stripStart line
              hasDoc = checkPrecedingDoc (lineNum - 1) (T.lines content)
          in Just (name, lineNum, hasDoc)
        | otherwise = Nothing
  in mapMaybe findSigs linesWithNums

-- | Check if the preceding lines have a Haddock comment
checkPrecedingDoc :: Int -> [Text] -> Bool
checkPrecedingDoc lineIdx lines'
  | lineIdx <= 0 = False
  | lineIdx > length lines' = False
  | otherwise =
    let prevLine = lines' !! (lineIdx - 1)
    in "-- |" `T.isPrefixOf` T.stripStart prevLine
       || "-- ^" `T.isPrefixOf` T.stripStart prevLine
       || "{-|" `T.isInfixOf` prevLine
       || checkPrecedingDoc (lineIdx - 1) lines'  -- Check further back for multi-line docs

-- | Check if function is documented
checkFunctionDoc :: Text -> (Text, Int, Bool) -> Maybe DocumentationFinding
checkFunctionDoc _content (name, lineNum, hasDoc)
  | hasDoc = Nothing
  | isPrivateHelper name = Nothing
  | otherwise = Just DocumentationFinding
    { dfCategory = MissingFunctionDoc
    , dfSpan = mkSrcSpanRaw "" lineNum 1 lineNum (T.length name + 1)
    , dfName = name
    , dfExplanation = "Function '" <> name <> "' is exported but has no documentation"
    , dfSeverity = Suggestion
    }

-- | Check if name looks like a private helper (starts with underscore, etc.)
isPrivateHelper :: Text -> Bool
isPrivateHelper name =
  "_" `T.isPrefixOf` name
  || "helper" `T.isInfixOf` T.toLower name
  || "internal" `T.isInfixOf` T.toLower name

--------------------------------------------------------------------------------
-- Type Documentation
--------------------------------------------------------------------------------

-- | Detect missing type documentation
detectMissingTypeDoc :: DocumentationConfig -> FilePath -> Text -> [DocumentationFinding]
detectMissingTypeDoc config path content =
  let typeDecls = extractTypeDeclarations path content
      exports = extractExports content
      toCheck = if dcExportedOnly config
                then filter (\(name, _, _) -> name `elem` exports || null exports) typeDecls
                else typeDecls
  in mapMaybe (checkTypeDoc content) toCheck

-- | Extract type declarations
extractTypeDeclarations :: FilePath -> Text -> [(Text, Int, Bool)]
extractTypeDeclarations _path content =
  let linesWithNums = zip [1..] (T.lines content)
      findTypes (lineNum, line)
        | "data " `T.isPrefixOf` T.stripStart line && not (isComment line) =
          let name = extractTypeName "data " line
              hasDoc = checkPrecedingDoc (lineNum - 1) (T.lines content)
          in Just (name, lineNum, hasDoc)
        | "newtype " `T.isPrefixOf` T.stripStart line && not (isComment line) =
          let name = extractTypeName "newtype " line
              hasDoc = checkPrecedingDoc (lineNum - 1) (T.lines content)
          in Just (name, lineNum, hasDoc)
        | "type " `T.isPrefixOf` T.stripStart line
            && not ("type family" `T.isInfixOf` line)
            && not (isComment line) =
          let name = extractTypeName "type " line
              hasDoc = checkPrecedingDoc (lineNum - 1) (T.lines content)
          in Just (name, lineNum, hasDoc)
        | "class " `T.isPrefixOf` T.stripStart line && not (isComment line) =
          let name = extractClassName line
              hasDoc = checkPrecedingDoc (lineNum - 1) (T.lines content)
          in Just (name, lineNum, hasDoc)
        | otherwise = Nothing
  in mapMaybe findTypes linesWithNums

-- | Extract type name from declaration
extractTypeName :: Text -> Text -> Text
extractTypeName prefix line =
  let afterPrefix = T.drop (T.length prefix) $ T.stripStart line
      name = T.takeWhile (\c -> c /= ' ' && c /= '=') afterPrefix
  in T.strip name

-- | Extract class name from declaration
extractClassName :: Text -> Text
extractClassName line =
  let afterClass = T.drop 6 $ T.stripStart line  -- "class "
      -- Handle constraints: "Ord a => Foo a" -> "Foo"
      withoutConstraint = case T.breakOn "=>" afterClass of
        (_, rest) | not (T.null rest) -> T.drop 2 rest
        _ -> afterClass
      name = T.takeWhile (\c -> c /= ' ' && c /= '(' && c /= '{') $
             T.stripStart withoutConstraint
  in T.strip name

-- | Check if type is documented
checkTypeDoc :: Text -> (Text, Int, Bool) -> Maybe DocumentationFinding
checkTypeDoc _ (name, lineNum, hasDoc)
  | hasDoc = Nothing
  | otherwise = Just DocumentationFinding
    { dfCategory = MissingTypeDoc
    , dfSpan = mkSrcSpanRaw "" lineNum 1 lineNum (T.length name + 1)
    , dfName = name
    , dfExplanation = "Type '" <> name <> "' is missing documentation"
    , dfSeverity = Suggestion
    }

--------------------------------------------------------------------------------
-- Incomplete Documentation
--------------------------------------------------------------------------------

-- | Detect incomplete Haddock documentation
detectIncompleteDoc :: FilePath -> Text -> [DocumentationFinding]
detectIncompleteDoc path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkIncompleteLine path) linesWithNums

checkIncompleteLine :: FilePath -> (Int, Text) -> [DocumentationFinding]
checkIncompleteLine path (lineNum, line) = catMaybes
  [ -- Record field without doc
    if ":: " `T.isInfixOf` line && "{" `T.isInfixOf` line
       && not ("-- ^" `T.isInfixOf` line) && not (isComment line)
    then Just DocumentationFinding
      { dfCategory = IncompleteDoc
      , dfSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line)
      , dfName = "record field"
      , dfExplanation = "Record field is missing documentation (use -- ^ comment)"
      , dfSeverity = Suggestion
      }
    else Nothing

  , -- Constructor without doc
    if "| " `T.isPrefixOf` T.stripStart line
       && not ("-- ^" `T.isInfixOf` line) && not (isComment line)
       && isInDataDecl line
    then Just DocumentationFinding
      { dfCategory = IncompleteDoc
      , dfSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line)
      , dfName = extractConstructorName line
      , dfExplanation = "Data constructor is missing documentation"
      , dfSeverity = Suggestion
      }
    else Nothing
  ]

-- | Extract constructor name from "| Constructor"
extractConstructorName :: Text -> Text
extractConstructorName line =
  let afterPipe = T.drop 1 $ T.dropWhile (/= '|') line
      name = T.takeWhile (\c -> c /= ' ' && c /= '{') $ T.stripStart afterPipe
  in T.strip name

--------------------------------------------------------------------------------
-- Stale Documentation
--------------------------------------------------------------------------------

-- | Detect potentially stale documentation
detectStaleDoc :: FilePath -> Text -> [DocumentationFinding]
detectStaleDoc path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkStaleLine path) linesWithNums

checkStaleLine :: FilePath -> (Int, Text) -> [DocumentationFinding]
checkStaleLine path (lineNum, line) = catMaybes
  [ -- Very old date references
    if ("2015" `T.isInfixOf` line || "2016" `T.isInfixOf` line
        || "2017" `T.isInfixOf` line || "2018" `T.isInfixOf` line)
       && isDocComment line
    then Just DocumentationFinding
      { dfCategory = StaleDoc
      , dfSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line)
      , dfName = "date reference"
      , dfExplanation = "Documentation contains old date references"
      , dfSeverity = Suggestion
      }
    else Nothing

  , -- Deprecated without @deprecated tag
    if "deprecated" `T.isInfixOf` T.toLower line && isDocComment line
       && not ("@deprecated" `T.isInfixOf` T.toLower line)
    then Just DocumentationFinding
      { dfCategory = StaleDoc
      , dfSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line)
      , dfName = "deprecated mention"
      , dfExplanation = "Use @deprecated tag for proper deprecation warnings"
      , dfSeverity = Suggestion
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Missing Examples
--------------------------------------------------------------------------------

-- | Detect missing examples in documentation
detectMissingExamples :: FilePath -> Text -> [DocumentationFinding]
detectMissingExamples path content =
  let exports = extractExports content
      -- Only check if there are substantial exports
  in if length exports >= 5 && not ("@" `T.isInfixOf` content && ">>>" `T.isInfixOf` content)
     then [DocumentationFinding
       { dfCategory = MissingExample
       , dfSpan = mkSrcSpanRaw path 1 1 1 1
       , dfName = "module"
       , dfExplanation = "Module has many exports but no doctests/examples"
       , dfSeverity = Suggestion
       }]
     else []

--------------------------------------------------------------------------------
-- TODO in Documentation
--------------------------------------------------------------------------------

-- | Detect TODOs in documentation
detectTodosInDoc :: FilePath -> Text -> [DocumentationFinding]
detectTodosInDoc path content =
  let linesWithNums = zip [1..] (T.lines content)
  in mapMaybe (checkTodoLine path) linesWithNums

checkTodoLine :: FilePath -> (Int, Text) -> Maybe DocumentationFinding
checkTodoLine path (lineNum, line)
  | isDocComment line &&
    ("TODO" `T.isInfixOf` T.toUpper line || "FIXME" `T.isInfixOf` T.toUpper line)
  = Just DocumentationFinding
    { dfCategory = TodoInDoc
    , dfSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line)
    , dfName = "TODO/FIXME"
    , dfExplanation = "Documentation contains unresolved TODO or FIXME"
    , dfSeverity = Suggestion
    }
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Convert finding to diagnostic
findingToDiagnostic :: DocumentationFinding -> Diagnostic
findingToDiagnostic DocumentationFinding{..} = Diagnostic
  { diagSpan = dfSpan
  , diagSeverity = dfSeverity
  , diagKind = Custom "documentation"
  , diagMessage = dfExplanation
  , diagCode = Just $ "documentation/" <> categoryToCode dfCategory
  , diagFixes = []
  , diagRelated = []
  }

-- | Convert category to diagnostic code suffix
categoryToCode :: DocumentationCategory -> Text
categoryToCode = \case
  MissingModuleDoc -> "missing-module"
  MissingFunctionDoc -> "missing-function"
  MissingTypeDoc -> "missing-type"
  IncompleteDoc -> "incomplete"
  StaleDoc -> "stale"
  MissingExample -> "missing-example"
  TodoInDoc -> "todo"

-- | Check if a line is a comment
isComment :: Text -> Bool
isComment line =
  let stripped = T.stripStart line
  in "--" `T.isPrefixOf` stripped || "{-" `T.isPrefixOf` stripped

-- | Check if a line is a Haddock doc comment
isDocComment :: Text -> Bool
isDocComment line =
  let stripped = T.stripStart line
  in "-- |" `T.isPrefixOf` stripped
     || "-- ^" `T.isPrefixOf` stripped
     || "{-|" `T.isInfixOf` stripped

-- | Check if line is inside a data declaration context
-- Heuristic: lines inside data declarations typically start with indentation,
-- start with |, or contain record field syntax
isInDataDecl :: Text -> Bool
isInDataDecl line =
  let stripped = T.stripStart line
      hasIndent = T.length line > T.length stripped && T.length stripped > 0
      startsWithPipe = "|" `T.isPrefixOf` stripped
      -- Record field with :: in the middle of a data decl
      isRecordField = hasIndent && "::" `T.isInfixOf` line &&
                      (T.any (== '{') line || T.any (== '}') line || T.any (== ',') line)
  in startsWithPipe || isRecordField
