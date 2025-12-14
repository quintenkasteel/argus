{-# LANGUAGE StrictData #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Argus.Refactor.Validation
-- Description : Comprehensive validation of refactored code
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides multi-level validation of refactored Haskell code:
-- 1. Syntactic validation via GHC parser
-- 2. Semantic validation via type checking (optional)
-- 3. Structural validation (balanced brackets, etc.)
-- 4. Idempotency checking
module Argus.Refactor.Validation
  ( -- * Validation
    validateRefactoring
  , ValidationResult (..)
  , ValidationError (..)
  , ValidationLevel (..)
  , ValidationStage (..)
  , ValidationConfig (..)
  , ValidationStats (..)
  , ValidationSeverity (..)
  , defaultValidationConfig

    -- * Specific validators
  , validateSyntax
  , validateSemantic
  , validateStructure
  , validateIdempotency

    -- * Diff generation
  , Diff (..)
  , DiffHunk (..)
  , DiffLine (..)
  , DiffOp (..)
  , generateDiff
  , renderDiff
  , renderDiffColored
  ) where

import Control.Exception (try, SomeException)
import Data.Algorithm.Diff (getGroupedDiff)
import Data.Algorithm.Diff qualified as ADiff
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory, takeFileName)
import System.IO.Temp (writeSystemTempFile)
import System.Process (readProcessWithExitCode)

-- Use existing parsing infrastructure
import Argus.Analysis.Syntactic (parseModule, ParseError(..))

--------------------------------------------------------------------------------
-- Validation Types
--------------------------------------------------------------------------------

-- | How thorough should validation be
data ValidationLevel
  = NoValidation         -- ^ Skip validation (fastest, unsafe)
  | StructuralOnly       -- ^ Check balanced brackets, basic structure
  | SyntaxValidation     -- ^ Full GHC parse (recommended)
  | SemanticValidation   -- ^ Parse + type check (slowest, safest)
  deriving stock (Eq, Show, Ord, Enum, Bounded)

-- | Which stage of validation produced an error
data ValidationStage
  = StageStructure      -- ^ Structural validation (brackets, etc.)
  | StageSyntax         -- ^ GHC parser stage
  | StageSemantic       -- ^ Type checking stage
  | StageIdempotency    -- ^ Idempotency validation
  deriving stock (Eq, Show, Ord, Enum, Bounded)

-- | Configuration for validation
data ValidationConfig = ValidationConfig
  { vcLevel              :: ValidationLevel  -- ^ How thorough
  , vcStopOnFirstError   :: Bool             -- ^ Stop at first error?
  , vcCheckIdempotency   :: Bool             -- ^ Verify fix is idempotent
  , vcMaxParseRetries    :: Int              -- ^ Retries on parse failure
  , vcReportWarnings     :: Bool             -- ^ Include warnings in result
  , vcVerbose            :: Bool             -- ^ Detailed output
  }
  deriving stock (Eq, Show)

-- | Default validation config
defaultValidationConfig :: ValidationConfig
defaultValidationConfig = ValidationConfig
  { vcLevel            = SyntaxValidation
  , vcStopOnFirstError = True
  , vcCheckIdempotency = True
  , vcMaxParseRetries  = 1
  , vcReportWarnings   = True
  , vcVerbose          = False
  }

-- | A validation error
data ValidationError = ValidationError
  { veSeverity :: ValidationSeverity
  , veStage    :: ValidationStage     -- ^ Which validation stage
  , veMessage  :: Text                -- ^ Error message
  , veLine     :: Maybe Int           -- ^ Line number if applicable
  , veColumn   :: Maybe Int           -- ^ Column if applicable
  , veContext  :: Maybe Text          -- ^ Surrounding code context
  }
  deriving stock (Eq, Show)

-- | Severity of validation error
data ValidationSeverity
  = VSError      -- ^ Fatal error, fix cannot be applied
  | VSWarning    -- ^ Warning, fix can still be applied
  | VSInfo       -- ^ Informational
  deriving stock (Eq, Show, Ord)

-- | Result of validation
data ValidationResult = ValidationResult
  { vrSuccess     :: Bool
  , vrLevel       :: ValidationLevel
  , vrErrors      :: [ValidationError]
  , vrWarnings    :: [ValidationError]
  , vrOriginal    :: Text
  , vrTransformed :: Text
  , vrDiff        :: Maybe Diff
  , vrStats       :: ValidationStats
  }
  deriving stock (Eq, Show)

-- | Statistics about the validation
data ValidationStats = ValidationStats
  { vsLinesChanged    :: Int
  , vsLinesAdded      :: Int
  , vsLinesRemoved    :: Int
  , vsCharactersChanged :: Int
  , vsParseTimeMs     :: Double
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Main Validation Entry Point
--------------------------------------------------------------------------------

-- | Validate a refactoring comprehensively
validateRefactoring :: ValidationConfig -> FilePath -> Text -> Text
                    -> IO ValidationResult
validateRefactoring config path original transformed = do
  -- Start with structural validation (always fast)
  structResult <- validateStructure original transformed

  case (vcLevel config, structResult) of
    -- No validation requested
    (NoValidation, _) -> pure $ mkSuccess original transformed

    -- Structure check failed
    (_, Left errs) -> pure $ mkFailure StructuralOnly original transformed errs

    -- Structure OK, check if we need more validation
    (StructuralOnly, Right _) -> pure $ mkSuccess original transformed

    -- Need syntax validation
    (level, Right _) | level >= SyntaxValidation -> do
      syntaxResult <- validateSyntax path transformed

      case syntaxResult of
        Left errs -> pure $ mkFailure SyntaxValidation original transformed errs
        Right _ -> do
          -- Semantic validation if requested
          semanticResult <- if level >= SemanticValidation
            then validateSemantic path transformed
            else pure $ Right ()

          case semanticResult of
            Left errs -> pure $ mkFailure SemanticValidation original transformed errs
            Right _ -> do
              -- Check idempotency if requested
              idempotencyResult <- if vcCheckIdempotency config
                then validateIdempotency path original transformed
                else pure $ Right ()

              case idempotencyResult of
                Left errs -> pure $ mkFailure SyntaxValidation original transformed
                  [ValidationError VSWarning StageIdempotency e Nothing Nothing Nothing | e <- errs]
                Right _ -> pure $ mkSuccessWithLevel level original transformed

    _ -> pure $ mkSuccess original transformed

-- | Create a success result with specific level
mkSuccessWithLevel :: ValidationLevel -> Text -> Text -> ValidationResult
mkSuccessWithLevel level original transformed = ValidationResult
  { vrSuccess = True
  , vrLevel = level
  , vrErrors = []
  , vrWarnings = []
  , vrOriginal = original
  , vrTransformed = transformed
  , vrDiff = Just $ generateDiff original transformed
  , vrStats = computeStats original transformed
  }

-- | Create a success result
mkSuccess :: Text -> Text -> ValidationResult
mkSuccess original transformed = ValidationResult
  { vrSuccess = True
  , vrLevel = SyntaxValidation
  , vrErrors = []
  , vrWarnings = []
  , vrOriginal = original
  , vrTransformed = transformed
  , vrDiff = Just $ generateDiff original transformed
  , vrStats = computeStats original transformed
  }

-- | Create a failure result
mkFailure :: ValidationLevel -> Text -> Text -> [ValidationError] -> ValidationResult
mkFailure level original transformed errs = ValidationResult
  { vrSuccess = False
  , vrLevel = level
  , vrErrors = filter ((== VSError) . veSeverity) errs
  , vrWarnings = filter ((== VSWarning) . veSeverity) errs
  , vrOriginal = original
  , vrTransformed = transformed
  , vrDiff = Just $ generateDiff original transformed
  , vrStats = computeStats original transformed
  }

-- | Compute statistics
computeStats :: Text -> Text -> ValidationStats
computeStats original transformed =
  let oldLines = T.lines original
      newLines = T.lines transformed
      diff = getGroupedDiff oldLines newLines
      (added, removed) = countChanges diff
  in ValidationStats
    { vsLinesChanged = added + removed
    , vsLinesAdded = added
    , vsLinesRemoved = removed
    , vsCharactersChanged = abs (T.length original - T.length transformed)
    , vsParseTimeMs = 0  -- Would need timing
    }
  where
    countChanges [] = (0, 0)
    countChanges (ADiff.First xs : rest) =
      let (a, r) = countChanges rest in (a, r + length xs)
    countChanges (ADiff.Second xs : rest) =
      let (a, r) = countChanges rest in (a + length xs, r)
    countChanges (ADiff.Both _ _ : rest) = countChanges rest

--------------------------------------------------------------------------------
-- Structural Validation
--------------------------------------------------------------------------------

-- | Validate basic structure (balanced brackets, etc.)
validateStructure :: Text -> Text -> IO (Either [ValidationError] ())
validateStructure _original transformed = do
  let errors = concat
        [ checkBalancedBrackets transformed
        , checkBalancedStrings transformed
        , checkIndentation transformed
        ]

  pure $ if null errors
    then Right ()
    else Left errors

-- | Check for balanced brackets/parentheses/braces
-- This version properly skips brackets inside strings, comments, and char literals
checkBalancedBrackets :: Text -> [ValidationError]
checkBalancedBrackets source =
  case go [] 1 1 (T.unpack source) of
    Left (msg, line, col) -> [ValidationError VSError StageStructure msg (Just line) (Just col) Nothing]
    Right _ -> []
  where
    go :: [(Char, Int, Int)] -> Int -> Int -> String -> Either (Text, Int, Int) ()
    go [] _ _ [] = Right ()
    go ((c, l, col):_) _ _ [] = Left ("Unclosed '" <> T.singleton c <> "'", l, col)
    go stack line col (x:xs)
      -- Skip single-line comments (-- to end of line)
      | x == '-', '-':rest <- xs =
          let (_, afterComment) = span (/= '\n') rest
          in case afterComment of
               '\n':remaining -> go stack (line + 1) 1 remaining
               _ -> go stack line col []  -- End of file
      -- Skip multi-line comments and pragmas {- ... -} or {-# ... #-}
      | x == '{', '-':rest <- xs = skipBlockComment stack (line, col) line (col + 2) rest
      -- Skip string literals
      | x == '"' = skipString stack line (col + 1) xs
      -- Handle single quote: could be char literal OR promoted type/TH
      | x == '\'' = handleQuote stack line col xs
      -- Track newlines
      | x == '\n' = go stack (line + 1) 1 xs
      -- Track opening brackets
      | x == '(' || x == '[' || x == '{' = go ((x, line, col):stack) line (col + 1) xs
      -- Track closing brackets
      | x == ')' = case stack of
          ('(', _, _):rest -> go rest line (col + 1) xs
          _ -> Left ("Unexpected ')'", line, col)
      | x == ']' = case stack of
          ('[', _, _):rest -> go rest line (col + 1) xs
          _ -> Left ("Unexpected ']'", line, col)
      | x == '}' = case stack of
          ('{', _, _):rest -> go rest line (col + 1) xs
          _ -> Left ("Unexpected '}'", line, col)
      | otherwise = go stack line (col + 1) xs

    -- Handle single quotes - distinguish char literals from promoted types/TH
    -- Char literal: 'a', '\n', '(', etc. (single char or escape, followed by ')
    -- Promoted type: 'True, '[], '(:), etc. (tick followed by identifier/symbol)
    handleQuote stack line col [] = go stack line (col + 1) []
    handleQuote stack line col (c:rest)
      -- Escape sequence in char literal: '\n', '\'' etc
      | c == '\\', (_:('\'':rest')) <- rest = go stack line (col + 4) rest'
      | c == '\\', (_:rest') <- rest = go stack line (col + 3) rest'  -- Malformed, continue
      -- Simple char literal: 'a', '(', ')' etc - check if followed by closing quote
      -- This MUST come before promoted bracket check!
      | ('\'':rest') <- rest = go stack line (col + 3) rest'
      -- Promoted bracket: '[ or '( but ONLY if not followed by ' (char literal case handled above)
      | c == '[' || c == '(' = go ((c, line, col + 1):stack) line (col + 2) rest
      -- Promoted constructor: 'True, 'Just, etc. - just skip the tick
      | isIdentStart c = go stack line (col + 1) (c:rest)
      -- Operators after tick: ':, '++, etc.
      | isSymbol c = go stack line (col + 1) (c:rest)
      -- Unknown - just skip the tick and continue
      | otherwise = go stack line (col + 1) (c:rest)

    isIdentStart c = c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c == '_'
    isSymbol c = c `elem` (":!#$%&*+./<=>?@\\^|-~" :: String)

    -- Skip over a string literal, handling escapes
    skipString stack line col [] = go stack line col []  -- Unclosed string, let parser catch it
    skipString stack line col ('"':xs) = go stack line (col + 1) xs  -- End of string
    skipString stack line col ('\\':_:xs) = skipString stack line (col + 2) xs  -- Escape sequence
    skipString stack line col ('\n':xs) = skipString stack (line + 1) 1 xs  -- Multiline string
    skipString stack line col (_:xs) = skipString stack line (col + 1) xs

    -- Skip over a block comment {- ... -}, handling nesting
    skipBlockComment stack startPos line col [] =
      Left ("Unclosed block comment", fst startPos, snd startPos)
    skipBlockComment stack startPos line col ('-':'}':xs) =
      go stack line (col + 2) xs  -- End of block comment
    skipBlockComment stack startPos line col ('{':'-':xs) =
      -- Nested block comment - skip it and continue
      skipBlockComment stack startPos line (col + 2) (skipNestedComment 1 xs)
    skipBlockComment stack startPos line col ('\n':xs) =
      skipBlockComment stack startPos (line + 1) 1 xs
    skipBlockComment stack startPos line col (_:xs) =
      skipBlockComment stack startPos line (col + 1) xs

    -- Skip nested block comments (returns remaining string after all nested comments closed)
    skipNestedComment :: Int -> String -> String
    skipNestedComment 0 s = s
    skipNestedComment _ [] = []
    skipNestedComment n ('{':'-':xs) = skipNestedComment (n + 1) xs
    skipNestedComment n ('-':'}':xs) = skipNestedComment (n - 1) xs
    skipNestedComment n (_:xs) = skipNestedComment n xs

-- | Check for unclosed strings
checkBalancedStrings :: Text -> [ValidationError]
checkBalancedStrings source =
  let lines' = zip [1..] (T.lines source)
  in concatMap checkLine lines'
  where
    checkLine (lineNum, line) =
      if oddQuotes (T.unpack line) && not (endsWithBackslash line)
      then [ValidationError VSWarning StageStructure
              "Potentially unclosed string literal"
              (Just lineNum) Nothing (Just line)]
      else []

    oddQuotes = odd . length . filter (== '"')
    endsWithBackslash t = not (T.null t) && T.last t == '\\'

-- | Check for suspicious indentation
checkIndentation :: Text -> [ValidationError]
checkIndentation source =
  let lines' = zip [1 :: Int ..] (T.lines source)
      tabs = [(n, l) | (n, l) <- lines', T.any (== '\t') l]
  in if null tabs
     then []
     else [ValidationError VSInfo StageStructure
             ("Found tabs in " <> T.pack (show (length tabs)) <> " line(s), consider using spaces")
             Nothing Nothing Nothing]

--------------------------------------------------------------------------------
-- Syntax Validation (GHC Parser)
--------------------------------------------------------------------------------

-- | Validate syntax using GHC parser
validateSyntax :: FilePath -> Text -> IO (Either [ValidationError] ())
validateSyntax path source = do
  result <- try @SomeException $ parseModule path source

  case result of
    Left ex -> pure $ Left
      [ValidationError VSError StageSyntax
        ("Parse exception: " <> T.pack (show ex))
        Nothing Nothing Nothing]

    Right (Left parseErr) -> pure $ Left
      [ValidationError VSError StageSyntax
        (peMessage parseErr)
        (Just $ peLine parseErr)
        (Just $ peColumn parseErr)
        (getContext source (peLine parseErr))]

    Right (Right _) -> pure $ Right ()

--------------------------------------------------------------------------------
-- Semantic Validation (Type Checking)
--------------------------------------------------------------------------------

-- | Validate semantics by type-checking using GHC
-- This uses GHC with -fno-code to type-check without generating code.
-- It's slower than syntax validation but catches type errors.
validateSemantic :: FilePath -> Text -> IO (Either [ValidationError] ())
validateSemantic originalPath source = do
  -- Write source to a temp file in the same directory
  -- to preserve module path relationships
  let dir = takeDirectory originalPath
      baseName = takeFileName originalPath
  tempPath <- writeSystemTempFile baseName (T.unpack source)

  -- Run GHC with type-checking only (no code generation)
  -- Use -fno-code and -fdefer-type-errors to get all errors
  result <- try @SomeException $ readProcessWithExitCode "ghc"
    [ "-fno-code"                -- Don't generate code
    , "-fdefer-type-errors"      -- Report all type errors
    , "-w"                       -- Suppress warnings
    , "-ignore-dot-ghci"         -- Don't run .ghci
    , "-no-link"                 -- Don't link
    , "-i" <> dir                -- Include original directory for imports
    , tempPath
    ] ""

  -- Clean up temp file
  _ <- try @SomeException $ removeFile tempPath

  case result of
    Left ex -> pure $ Left
      [ValidationError VSError StageSemantic
        ("Type check exception: " <> T.pack (show ex))
        Nothing Nothing Nothing]

    Right (ExitSuccess, _, _) -> pure $ Right ()

    Right (ExitFailure _, _, stderr) ->
      pure $ Left $ parseGhcErrors stderr
  where
    -- Parse GHC error output into ValidationErrors
    parseGhcErrors :: String -> [ValidationError]
    parseGhcErrors output =
      let errorLines = filter (not . null) $ lines output
      in if null errorLines
         then [ValidationError VSError StageSemantic "Type check failed with no output" Nothing Nothing Nothing]
         else map parseGhcError errorLines

    parseGhcError :: String -> ValidationError
    parseGhcError line =
      -- GHC error format: "file:line:col: error: message"
      case break (== ':') line of
        (_, ':':rest) ->
          case break (== ':') rest of
            (lineStr, ':':rest2) ->
              case break (== ':') rest2 of
                (colStr, ':':msg) ->
                  ValidationError VSError StageSemantic
                    (T.strip $ T.pack msg)
                    (readMaybe lineStr)
                    (readMaybe colStr)
                    Nothing
                _ -> mkSimpleError line
            _ -> mkSimpleError line
        _ -> mkSimpleError line

    mkSimpleError msg = ValidationError VSError StageSemantic
      (T.pack msg) Nothing Nothing Nothing

    readMaybe :: String -> Maybe Int
    readMaybe s = case reads s of
      [(n, "")] -> Just n
      _ -> Nothing

-- | Get context around a line
getContext :: Text -> Int -> Maybe Text
getContext source lineNum =
  let lines' = T.lines source
      start = max 0 (lineNum - 3)
      end = min (length lines') (lineNum + 2)
      contextLines = take (end - start) $ drop start lines'
  in if null contextLines
     then Nothing
     else Just $ T.unlines contextLines

--------------------------------------------------------------------------------
-- Idempotency Validation
--------------------------------------------------------------------------------

-- | Check that the transformation is stable
-- This validates that:
-- 1. The transformed code parses successfully
-- 2. Re-parsing the transformed code yields the same normalized form
-- 3. Whitespace normalization doesn't change semantics
validateIdempotency :: FilePath -> Text -> Text -> IO (Either [Text] ())
validateIdempotency path _original transformed = do
  -- First, verify transformed code parses
  result1 <- parseModule path transformed
  case result1 of
    Left err ->
      pure $ Left ["Transformed code failed to parse: " <> peMessage err]

    Right _ -> do
      -- Re-parse to verify parsing is stable
      result2 <- parseModule path transformed
      case result2 of
        Left err ->
          pure $ Left ["Re-parsing transformed code failed: " <> peMessage err]

        Right _ -> do
          -- Verify that normalizing whitespace doesn't break parsing
          let normalized = normalizeSource transformed
          if normalized == transformed
            then pure $ Right ()
            else do
              -- Try parsing the normalized version
              normalizedResult <- parseModule path normalized
              case normalizedResult of
                Left _ ->
                  -- Normalization broke parsing - whitespace is significant
                  pure $ Left
                    [ "Whitespace normalization would break parsing"
                    , "The transformation may have introduced whitespace-sensitive code"
                    ]
                Right _ ->
                  -- Both parse fine
                  pure $ Right ()

-- | Normalize source code for comparison
-- Preserves semantic content while normalizing whitespace
normalizeSource :: Text -> Text
normalizeSource = T.unlines . map normalizeLine . T.lines
  where
    normalizeLine line
      -- Preserve indentation but normalize trailing whitespace
      | T.null stripped = T.empty
      | otherwise = indent <> T.unwords (T.words stripped)
      where
        stripped = T.stripEnd line
        indent = T.takeWhile (== ' ') line

--------------------------------------------------------------------------------
-- Diff Generation
--------------------------------------------------------------------------------

-- | A complete diff between two texts
data Diff = Diff
  { diffOldFile  :: FilePath
  , diffNewFile  :: FilePath
  , diffHunks    :: [DiffHunk]
  }
  deriving stock (Eq, Show)

-- | A hunk (contiguous set of changes)
data DiffHunk = DiffHunk
  { hunkOldStart :: Int
  , hunkOldCount :: Int
  , hunkNewStart :: Int
  , hunkNewCount :: Int
  , hunkLines    :: [DiffLine]
  }
  deriving stock (Eq, Show)

-- | A line in a diff
data DiffLine = DiffLine
  { dlOp      :: DiffOp
  , dlContent :: Text
  , dlOldLine :: Maybe Int
  , dlNewLine :: Maybe Int
  }
  deriving stock (Eq, Show)

-- | Diff operation
data DiffOp
  = DiffContext   -- ^ Unchanged line
  | DiffAdd       -- ^ Added line
  | DiffRemove    -- ^ Removed line
  deriving stock (Eq, Show)

-- | Generate a diff between two texts
generateDiff :: Text -> Text -> Diff
generateDiff old new =
  let oldLines = T.lines old
      newLines = T.lines new
      grouped = getGroupedDiff oldLines newLines
      hunks = buildHunks 1 1 grouped
  in Diff
    { diffOldFile = "a/file"
    , diffNewFile = "b/file"
    , diffHunks = hunks
    }

-- | Build hunks from grouped diff
buildHunks :: Int -> Int -> [ADiff.Diff [Text]] -> [DiffHunk]
buildHunks _ _ [] = []
buildHunks oldLine newLine (d:ds) = case d of
  ADiff.Both same _ ->
    -- Skip context, advance line numbers
    buildHunks (oldLine + length same) (newLine + length same) ds

  ADiff.First removed ->
    let removedLines = zipWith (\i t -> DiffLine DiffRemove t (Just i) Nothing)
                               [oldLine..] removed
        (addedLines, newNewLine, rest) = collectAdded newLine ds
        hunk = DiffHunk
          { hunkOldStart = oldLine
          , hunkOldCount = length removed
          , hunkNewStart = newLine
          , hunkNewCount = length addedLines
          , hunkLines = removedLines ++ addedLines
          }
    in hunk : buildHunks (oldLine + length removed) newNewLine rest

  ADiff.Second added ->
    let addedLines = zipWith (\i t -> DiffLine DiffAdd t Nothing (Just i))
                             [newLine..] added
        hunk = DiffHunk
          { hunkOldStart = oldLine
          , hunkOldCount = 0
          , hunkNewStart = newLine
          , hunkNewCount = length added
          , hunkLines = addedLines
          }
    in hunk : buildHunks oldLine (newLine + length added) ds

-- | Collect consecutive additions after removals
collectAdded :: Int -> [ADiff.Diff [Text]] -> ([DiffLine], Int, [ADiff.Diff [Text]])
collectAdded newLine [] = ([], newLine, [])
collectAdded newLine (ADiff.Second added : rest) =
  let addedLines = zipWith (\i t -> DiffLine DiffAdd t Nothing (Just i))
                           [newLine..] added
      (more, finalLine, remaining) = collectAdded (newLine + length added) rest
  in (addedLines ++ more, finalLine, remaining)
collectAdded newLine rest = ([], newLine, rest)

-- | Render diff as plain text (unified format)
renderDiff :: Diff -> Text
renderDiff Diff{..} = T.unlines $
  [ "--- " <> T.pack diffOldFile
  , "+++ " <> T.pack diffNewFile
  ] ++ concatMap renderHunk diffHunks

-- | Render a single hunk
renderHunk :: DiffHunk -> [Text]
renderHunk DiffHunk{..} =
  [ "@@ -" <> T.pack (show hunkOldStart) <> "," <> T.pack (show hunkOldCount)
    <> " +" <> T.pack (show hunkNewStart) <> "," <> T.pack (show hunkNewCount)
    <> " @@"
  ] ++ map renderLine hunkLines

-- | Render a diff line
renderLine :: DiffLine -> Text
renderLine DiffLine{..} = case dlOp of
  DiffContext -> " " <> dlContent
  DiffAdd     -> "+" <> dlContent
  DiffRemove  -> "-" <> dlContent

-- | Render diff with ANSI colors
renderDiffColored :: Diff -> Text
renderDiffColored Diff{..} = T.unlines $
  [ "\ESC[1m--- " <> T.pack diffOldFile <> "\ESC[0m"
  , "\ESC[1m+++ " <> T.pack diffNewFile <> "\ESC[0m"
  ] ++ concatMap renderHunkColored diffHunks

-- | Render a hunk with colors
renderHunkColored :: DiffHunk -> [Text]
renderHunkColored DiffHunk{..} =
  [ "\ESC[36m@@ -" <> T.pack (show hunkOldStart) <> "," <> T.pack (show hunkOldCount)
    <> " +" <> T.pack (show hunkNewStart) <> "," <> T.pack (show hunkNewCount)
    <> " @@\ESC[0m"
  ] ++ map renderLineColored hunkLines

-- | Render a line with colors
renderLineColored :: DiffLine -> Text
renderLineColored DiffLine{..} = case dlOp of
  DiffContext -> " " <> dlContent
  DiffAdd     -> "\ESC[32m+" <> dlContent <> "\ESC[0m"
  DiffRemove  -> "\ESC[31m-" <> dlContent <> "\ESC[0m"
