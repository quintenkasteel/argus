{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Analysis.Complexity
-- Description : Code complexity analysis for Haskell
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides comprehensive code complexity metrics adapted for
-- functional programming, including cyclomatic complexity, cognitive
-- complexity, and various other code quality metrics.
module Argus.Analysis.Complexity
  ( -- * Complexity Metrics
    ComplexityMetrics (..)
  , FunctionMetrics (..)
  , ModuleMetrics (..)
  , RecursionType (..)

    -- * Analysis Functions
  , analyzeComplexity
  , analyzeFunction
  , analyzeModule
  , calculateCyclomaticComplexity
  , calculateCognitiveComplexity
  , detectRecursionType

    -- * Helper Analysis Functions
  , calculateNestingDepth
  , countParameters
  , countPatternBranches
  , countGuards
  , countLambdaDepth
  , estimateMonadStack
  , countTypeConstraints
  , countLocalBindings

    -- * Thresholds
  , ComplexityThresholds (..)
  , defaultThresholds

    -- * Diagnostics
  , complexityDiagnostics
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import Argus.Types

--------------------------------------------------------------------------------
-- Complexity Metrics Data Types
--------------------------------------------------------------------------------

-- | Comprehensive complexity metrics for a function
data FunctionMetrics = FunctionMetrics
  { fmName               :: Text           -- ^ Function name
  , fmSpan               :: SrcSpan        -- ^ Source location
  , fmCyclomaticComplexity :: Int          -- ^ Traditional cyclomatic complexity
  , fmCognitiveComplexity  :: Int          -- ^ Cognitive complexity (readability)
  , fmLineCount          :: Int            -- ^ Lines of code
  , fmNestingDepth       :: Int            -- ^ Maximum nesting depth
  , fmParameterCount     :: Int            -- ^ Number of parameters
  , fmPatternBranches    :: Int            -- ^ Number of pattern match branches
  , fmGuardCount         :: Int            -- ^ Number of guards
  , fmLambdaDepth        :: Int            -- ^ Maximum lambda nesting depth
  , fmMonadStackDepth    :: Int            -- ^ Monad transformer stack depth
  , fmTypeConstraintCount :: Int           -- ^ Number of type class constraints
  , fmLocalBindings      :: Int            -- ^ Number of local let/where bindings
  , fmRecursionType      :: RecursionType  -- ^ Type of recursion used
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Type of recursion detected
data RecursionType
  = NoRecursion
  | DirectRecursion
  | TailRecursion
  | MutualRecursion
  | IndirectRecursion
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Module-level metrics
data ModuleMetrics = ModuleMetrics
  { mmModuleName         :: Text              -- ^ Module name
  , mmFilePath           :: FilePath          -- ^ File path
  , mmTotalLines         :: Int               -- ^ Total lines
  , mmCodeLines          :: Int               -- ^ Non-blank, non-comment lines
  , mmCommentLines       :: Int               -- ^ Comment lines
  , mmBlankLines         :: Int               -- ^ Blank lines
  , mmFunctionCount      :: Int               -- ^ Number of functions
  , mmTypeCount          :: Int               -- ^ Number of type declarations
  , mmClassCount         :: Int               -- ^ Number of type classes
  , mmInstanceCount      :: Int               -- ^ Number of instances
  , mmImportCount        :: Int               -- ^ Number of imports
  , mmExportCount        :: Int               -- ^ Number of exports
  , mmAverageComplexity  :: Double            -- ^ Average cyclomatic complexity
  , mmMaxComplexity      :: Int               -- ^ Maximum complexity
  , mmFunctions          :: [FunctionMetrics] -- ^ Per-function metrics
  , mmCodeToCommentRatio :: Double            -- ^ Code to comment ratio
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Combined complexity metrics
data ComplexityMetrics = ComplexityMetrics
  { cmModules            :: Map FilePath ModuleMetrics
  , cmTotalComplexity    :: Int
  , cmAverageComplexity  :: Double
  , cmHighComplexityFunctions :: [FunctionMetrics]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Thresholds
--------------------------------------------------------------------------------

-- | Configurable thresholds for complexity warnings
data ComplexityThresholds = ComplexityThresholds
  { ctCyclomaticWarning    :: Int     -- ^ Cyclomatic complexity warning threshold
  , ctCyclomaticError      :: Int     -- ^ Cyclomatic complexity error threshold
  , ctCognitiveWarning     :: Int     -- ^ Cognitive complexity warning
  , ctCognitiveError       :: Int     -- ^ Cognitive complexity error
  , ctLineLengthWarning    :: Int     -- ^ Function line count warning
  , ctNestingWarning       :: Int     -- ^ Nesting depth warning
  , ctParameterWarning     :: Int     -- ^ Parameter count warning
  , ctPatternBranchWarning :: Int     -- ^ Pattern branches warning
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default thresholds based on industry standards
defaultThresholds :: ComplexityThresholds
defaultThresholds = ComplexityThresholds
  { ctCyclomaticWarning    = 10
  , ctCyclomaticError      = 20
  , ctCognitiveWarning     = 15
  , ctCognitiveError       = 25
  , ctLineLengthWarning    = 50
  , ctNestingWarning       = 4
  , ctParameterWarning     = 5
  , ctPatternBranchWarning = 10
  }

--------------------------------------------------------------------------------
-- Analysis Functions
--------------------------------------------------------------------------------

-- | Analyze complexity of source code text
analyzeComplexity :: FilePath -> Text -> ComplexityMetrics
analyzeComplexity path content =
  let modMetrics = analyzeModule path content
      functions = mmFunctions modMetrics
      highComplexity = filter (\f -> fmCyclomaticComplexity f > 10) functions
      totalComplexity = sum $ map fmCyclomaticComplexity functions
      avgComplexity = if null functions then 0
                      else fromIntegral totalComplexity / fromIntegral (length functions)
  in ComplexityMetrics
    { cmModules = Map.singleton path modMetrics
    , cmTotalComplexity = totalComplexity
    , cmAverageComplexity = avgComplexity
    , cmHighComplexityFunctions = highComplexity
    }

-- | Analyze a single module
analyzeModule :: FilePath -> Text -> ModuleMetrics
analyzeModule path content =
  let linesOfCode = T.lines content
      totalLines = length linesOfCode
      (codeLines, commentLines, blankLines) = countLineTypes linesOfCode
      functions = extractFunctions path content
      moduleName = extractModuleName content
      imports = countImports content
      exports = countExports content
      types = countTypes content
      classes = countClasses content
      instances = countInstances content
      avgComplexity = if null functions then 0
                      else fromIntegral (sum $ map fmCyclomaticComplexity functions)
                           / fromIntegral (length functions)
      maxComplexity = if null functions then 0
                      else maximum $ map fmCyclomaticComplexity functions
      codeToComment = if commentLines == 0 then fromIntegral codeLines
                      else fromIntegral codeLines / fromIntegral commentLines
  in ModuleMetrics
    { mmModuleName = moduleName
    , mmFilePath = path
    , mmTotalLines = totalLines
    , mmCodeLines = codeLines
    , mmCommentLines = commentLines
    , mmBlankLines = blankLines
    , mmFunctionCount = length functions
    , mmTypeCount = types
    , mmClassCount = classes
    , mmInstanceCount = instances
    , mmImportCount = imports
    , mmExportCount = exports
    , mmAverageComplexity = avgComplexity
    , mmMaxComplexity = maxComplexity
    , mmFunctions = functions
    , mmCodeToCommentRatio = codeToComment
    }

-- | Analyze a single function
analyzeFunction :: FilePath -> Text -> Text -> Int -> Int -> FunctionMetrics
analyzeFunction path name body startLine endLine =
  let cyclomatic = calculateCyclomaticComplexity body
      cognitive = calculateCognitiveComplexity body
      lineCount = endLine - startLine + 1
      nesting = calculateNestingDepth body
      params = countParameters body
      patterns = countPatternBranches body
      guards = countGuards body
      lambdas = countLambdaDepth body
      monadStack = estimateMonadStack body
      constraints = countTypeConstraints body
      locals = countLocalBindings body
      recursion = detectRecursionType name body
  in FunctionMetrics
    { fmName = name
    , fmSpan = mkSrcSpanRaw path startLine 1 endLine 1
    , fmCyclomaticComplexity = cyclomatic
    , fmCognitiveComplexity = cognitive
    , fmLineCount = lineCount
    , fmNestingDepth = nesting
    , fmParameterCount = params
    , fmPatternBranches = patterns
    , fmGuardCount = guards
    , fmLambdaDepth = lambdas
    , fmMonadStackDepth = monadStack
    , fmTypeConstraintCount = constraints
    , fmLocalBindings = locals
    , fmRecursionType = recursion
    }

--------------------------------------------------------------------------------
-- Cyclomatic Complexity Calculation
--------------------------------------------------------------------------------

-- | Calculate cyclomatic complexity adapted for Haskell
-- Counts decision points: pattern matches, guards, if-then-else, case branches
calculateCyclomaticComplexity :: Text -> Int
calculateCyclomaticComplexity code =
  let base = 1
      caseCount = countOccurrences "case " code
      ifCount = countOccurrences " if " code + countOccurrences "\nif " code
      guardCount = countOccurrences " | " code
      patternCount = countOccurrences " -> " code
      -- Count function clause patterns (lines starting with function name followed by patterns)
      -- This handles patterns like: foo [] = ...; foo (x:xs) = ...
      functionClauseCount = countFunctionClauses code
      -- Each pattern match branch adds to complexity
      -- Subtract one for each case (the default path)
      adjusted = base + caseCount + ifCount + guardCount +
                 max 0 (patternCount - caseCount) +
                 max 0 (functionClauseCount - 1)  -- Multiple clauses add complexity
  in max 1 adjusted

-- | Count the number of function clauses (pattern-matching definitions)
-- Handles cases like:
--   length' [] = 0
--   length' (x:xs) = 1 + length' xs
countFunctionClauses :: Text -> Int
countFunctionClauses code =
  let ls = T.lines code
      -- Find lines that look like function clauses (start with same identifier and have =)
      clauseLines = filter isFunctionClauseLine ls
  in length clauseLines
  where
    isFunctionClauseLine line =
      let stripped = T.strip line
      in not (T.null stripped) &&
         isLowerFirst stripped &&
         " = " `T.isInfixOf` stripped &&
         not (startsWithKeyword stripped)

    isLowerFirst t = case T.uncons t of
      Just (c, _) -> c >= 'a' && c <= 'z'
      Nothing -> False

    startsWithKeyword t =
      any (`T.isPrefixOf` t)
        ["let ", "where", "if ", "then ", "else ", "case ", "import ", "module ", "data ", "type ", "newtype ", "class ", "instance "]

--------------------------------------------------------------------------------
-- Cognitive Complexity Calculation
--------------------------------------------------------------------------------

-- | Calculate cognitive complexity (how hard code is to understand)
-- Based on SonarSource's cognitive complexity model, adapted for Haskell
calculateCognitiveComplexity :: Text -> Int
calculateCognitiveComplexity code =
  let -- Basic structure increments
      caseComplexity = countOccurrences "case " code * 1
      ifComplexity = countOccurrences " if " code * 1
      guardComplexity = countOccurrences " | " code * 1

      -- Nesting penalties (estimate based on indentation)
      nestingPenalty = estimateNestingPenalty code

      -- Break in linear flow
      doNotationComplexity = countOccurrences " do\n" code + countOccurrences " do " code
      whereComplexity = countOccurrences " where\n" code
      letComplexity = countOccurrences " let " code + countOccurrences "\nlet " code

      -- Recursion adds complexity
      -- (detected by function name appearing in its body - simplified)

      -- Complex operators
      bindComplexity = countOccurrences " >>= " code
      composeComplexity = countOccurrences " . " code `div` 3  -- Composition chains

  in caseComplexity + ifComplexity + guardComplexity + nestingPenalty +
     doNotationComplexity + whereComplexity + letComplexity +
     bindComplexity + composeComplexity

-- | Estimate nesting penalty based on indentation depth
estimateNestingPenalty :: Text -> Int
estimateNestingPenalty code =
  let ls = T.lines code
      indentLevels = map getIndentLevel ls
      maxIndent = if null indentLevels then 0 else maximum indentLevels
      -- Penalize deep nesting
  in if maxIndent > 4 then (maxIndent - 4) * 2 else 0
  where
    getIndentLevel line =
      let stripped = T.stripStart line
      in if T.null stripped then 0
         else (T.length line - T.length stripped) `div` 2

--------------------------------------------------------------------------------
-- Helper Analysis Functions
--------------------------------------------------------------------------------

-- | Calculate maximum nesting depth
calculateNestingDepth :: Text -> Int
calculateNestingDepth code =
  let ls = T.lines code
      depths = map getIndentLevel ls
  in if null depths then 0 else maximum depths `div` 2
  where
    getIndentLevel line =
      let stripped = T.stripStart line
      in if T.null stripped then 0
         else T.length line - T.length stripped

-- | Count parameters in a function signature/definition
countParameters :: Text -> Int
countParameters code =
  case T.lines code of
    [] -> 0
    (firstLine:_) ->
      -- Count arrows in type signature or parameters before =
      let beforeEquals = T.takeWhile (/= '=') firstLine
          arrowCount = countOccurrences " -> " beforeEquals
          -- Or count space-separated identifiers after function name
          tokens = T.words beforeEquals
      in max arrowCount (max 0 (length tokens - 1))

-- | Count pattern match branches
countPatternBranches :: Text -> Int
countPatternBranches code =
  countOccurrences " -> " code + countOccurrences "\n  | " code

-- | Count guard expressions
countGuards :: Text -> Int
countGuards code = countOccurrences " | " code - countOccurrences " || " code

-- | Count lambda nesting depth
countLambdaDepth :: Text -> Int
countLambdaDepth code =
  let lambdaCount = countOccurrences "\\" code
  in min lambdaCount 5  -- Cap at 5 for reasonable measure

-- | Estimate monad transformer stack depth from type
estimateMonadStack :: Text -> Int
estimateMonadStack code =
  let transformers = ["ReaderT", "StateT", "WriterT", "ExceptT", "MaybeT", "ListT", "RWST"]
      counts = map (\t -> countOccurrences t code) transformers
  in sum counts

-- | Count type class constraints
-- Only count "=>" once (avoid double-counting " => " which contains "=>")
countTypeConstraints :: Text -> Int
countTypeConstraints code = countOccurrences "=>" code

-- | Count local bindings (let/where)
countLocalBindings :: Text -> Int
countLocalBindings code =
  countOccurrences " let " code + countOccurrences "\nlet " code +
  countOccurrences "where" code

-- | Detect recursion type
detectRecursionType :: Text -> Text -> RecursionType
detectRecursionType funcName body
  | not (funcName `T.isInfixOf` body) = NoRecursion
  | isTailRecursive funcName body = TailRecursion
  | otherwise = DirectRecursion

-- | Check if recursion is tail recursive
-- A recursive call is tail recursive if it's the last operation -
-- there's no additional processing of its result (like `n * factorial(n-1)`)
isTailRecursive :: Text -> Text -> Bool
isTailRecursive funcName body =
  let ls = T.lines body
      nonEmptyLines = filter (not . T.null . T.strip) ls
  in case nonEmptyLines of
       [] -> False
       xs ->
         let lastLine = T.strip $ last xs
             -- Strip common prefixes that don't affect tail recursion
             stripped = stripPrefixes lastLine
             -- For tail recursion, the function call should be the "top-level" expression
             -- NOT nested inside an operator like "1 + funcName x"
             -- Check if there's "operator funcName" pattern which means result is used
             hasOperatorOnResult = any (`T.isInfixOf` stripped)
               [ "+ " <> funcName
               , "- " <> funcName
               , "* " <> funcName
               , "/ " <> funcName
               , "++ " <> funcName
               , ": " <> funcName
               , "<> " <> funcName
               , "&& " <> funcName
               , "|| " <> funcName
               ]
         in funcName `T.isInfixOf` lastLine && not hasOperatorOnResult
  where
    -- Strip leading keywords/symbols that don't affect whether it's tail recursive
    stripPrefixes t =
      let prefixes = ["else ", "then ", "in ", "-> ", "| otherwise = ", "| "]
          stripped = T.stripStart t
      in foldl stripOnePrefix stripped prefixes

    stripOnePrefix t prefix
      | prefix `T.isPrefixOf` t = T.drop (T.length prefix) t
      | otherwise = t

--------------------------------------------------------------------------------
-- Line Type Counting
--------------------------------------------------------------------------------

-- | Count code lines, comment lines, and blank lines
countLineTypes :: [Text] -> (Int, Int, Int)
countLineTypes = foldr categorize (0, 0, 0)
  where
    categorize line (code, comments, blanks)
      | T.null (T.strip line) = (code, comments, blanks + 1)
      | isCommentLine line = (code, comments + 1, blanks)
      | otherwise = (code + 1, comments, blanks)

    isCommentLine line =
      let stripped = T.stripStart line
      in "--" `T.isPrefixOf` stripped ||
         "{-" `T.isPrefixOf` stripped ||
         "-}" `T.isSuffixOf` T.stripEnd stripped

--------------------------------------------------------------------------------
-- Content Extraction
--------------------------------------------------------------------------------

-- | Extract module name from content
extractModuleName :: Text -> Text
extractModuleName content =
  case filter ("module " `T.isPrefixOf`) (T.lines content) of
    [] -> "Unknown"
    (line:_) ->
      let afterModule = T.drop 7 line  -- Drop "module "
          beforeWhere = T.takeWhile (\c -> c /= ' ' && c /= '(') afterModule
      in T.strip beforeWhere

-- | Extract function definitions and analyze them
extractFunctions :: FilePath -> Text -> [FunctionMetrics]
extractFunctions path content =
  let ls = zip [1..] (T.lines content)
      -- Find lines that look like function definitions (simplified)
      functionStarts = findFunctionStarts ls
      -- Group into function bodies
      functionBodies = groupFunctionBodies functionStarts ls
  in map (\(name, body, start, end) -> analyzeFunction path name body start end) functionBodies

-- | Find function definition start lines
findFunctionStarts :: [(Int, Text)] -> [(Int, Text)]
findFunctionStarts = filter isFunctionStart
  where
    isFunctionStart (_, line) =
      let stripped = T.stripStart line
          -- Function definitions start with lowercase and aren't imports/keywords
      in not (T.null stripped) &&
         isLower (T.head stripped) &&
         not ("import " `T.isPrefixOf` stripped) &&
         not ("module " `T.isPrefixOf` stripped) &&
         not ("where" `T.isPrefixOf` stripped) &&
         not ("let " `T.isPrefixOf` stripped) &&
         not ("in " `T.isPrefixOf` stripped) &&
         not ("data " `T.isPrefixOf` stripped) &&
         not ("type " `T.isPrefixOf` stripped) &&
         not ("newtype " `T.isPrefixOf` stripped) &&
         not ("class " `T.isPrefixOf` stripped) &&
         not ("instance " `T.isPrefixOf` stripped) &&
         -- Has a space (function name followed by something)
         " " `T.isInfixOf` stripped &&
         -- Not a comment
         not ("--" `T.isPrefixOf` stripped)

    isLower c = c >= 'a' && c <= 'z'

-- | Group function bodies
groupFunctionBodies :: [(Int, Text)] -> [(Int, Text)] -> [(Text, Text, Int, Int)]
groupFunctionBodies starts allLines =
  let startLines = map fst starts
      -- For each start, find the end (next function start or end of file)
      -- Use 'drop 1' instead of 'tail' to avoid partial function
  in zipWith (extractBody allLines) starts (drop 1 startLines ++ [length allLines + 1])
  where
    extractBody :: [(Int, Text)] -> (Int, Text) -> Int -> (Text, Text, Int, Int)
    extractBody allLns (startLine, firstLine) endLine =
      let name = T.takeWhile (\c -> c /= ' ' && c /= ':') (T.stripStart firstLine)
          bodyLines = filter (\(ln, _) -> ln >= startLine && ln < endLine) allLns
          body = T.unlines $ map snd bodyLines
      in (name, body, startLine, endLine - 1)

-- | Count various content types
countImports :: Text -> Int
countImports = length . filter ("import " `T.isPrefixOf`) . map T.stripStart . T.lines

countExports :: Text -> Int
countExports content =
  let exportSection = T.takeWhile (/= ')') $ T.drop 1 $ T.dropWhile (/= '(') content
  in length $ filter (== ',') (T.unpack exportSection)

countTypes :: Text -> Int
countTypes content =
  let dataCount = countOccurrences "data " content
      newtypeCount = countOccurrences "newtype " content
      -- Count "type " but subtract newtype occurrences to avoid double-counting
      -- since "newtype " contains "type "
      typeCount = countOccurrences "type " content - newtypeCount
  in dataCount + newtypeCount + typeCount

countClasses :: Text -> Int
countClasses = countOccurrences "class "

countInstances :: Text -> Int
countInstances = countOccurrences "instance "

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- | Count occurrences of a substring
countOccurrences :: Text -> Text -> Int
countOccurrences needle haystack
  | T.null needle = 0
  | otherwise = go 0 haystack
  where
    go count text
      | T.null text = count
      | needle `T.isPrefixOf` text = go (count + 1) (T.drop (T.length needle) text)
      | otherwise = go count (T.drop 1 text)

--------------------------------------------------------------------------------
-- Diagnostics Generation
--------------------------------------------------------------------------------

-- | Generate diagnostics from complexity metrics
complexityDiagnostics :: ComplexityThresholds -> ComplexityMetrics -> [Diagnostic]
complexityDiagnostics thresholds metrics =
  concatMap (functionDiagnostics thresholds) (cmHighComplexityFunctions metrics)

-- | Generate diagnostics for a single function
functionDiagnostics :: ComplexityThresholds -> FunctionMetrics -> [Diagnostic]
functionDiagnostics ComplexityThresholds{..} FunctionMetrics{..} =
  mapMaybe id
    [ -- Cyclomatic complexity
      if fmCyclomaticComplexity >= ctCyclomaticError
        then Just $ mkDiag Error "cyclomatic-complexity-error"
          ("Function '" <> fmName <> "' has cyclomatic complexity of " <>
           T.pack (show fmCyclomaticComplexity) <> " (threshold: " <>
           T.pack (show ctCyclomaticError) <> "). Consider breaking it into smaller functions.")
        else if fmCyclomaticComplexity >= ctCyclomaticWarning
          then Just $ mkDiag Warning "cyclomatic-complexity-warning"
            ("Function '" <> fmName <> "' has cyclomatic complexity of " <>
             T.pack (show fmCyclomaticComplexity) <> " (threshold: " <>
             T.pack (show ctCyclomaticWarning) <> "). Consider simplifying.")
          else Nothing

    , -- Cognitive complexity
      if fmCognitiveComplexity >= ctCognitiveError
        then Just $ mkDiag Error "cognitive-complexity-error"
          ("Function '" <> fmName <> "' has cognitive complexity of " <>
           T.pack (show fmCognitiveComplexity) <> ". Code is too hard to understand.")
        else if fmCognitiveComplexity >= ctCognitiveWarning
          then Just $ mkDiag Warning "cognitive-complexity-warning"
            ("Function '" <> fmName <> "' has cognitive complexity of " <>
             T.pack (show fmCognitiveComplexity) <> ". Consider simplifying for readability.")
          else Nothing

    , -- Line count
      if fmLineCount >= ctLineLengthWarning
        then Just $ mkDiag Warning "function-length"
          ("Function '" <> fmName <> "' is " <> T.pack (show fmLineCount) <>
           " lines long. Consider breaking it up.")
        else Nothing

    , -- Nesting depth
      if fmNestingDepth >= ctNestingWarning
        then Just $ mkDiag Warning "nesting-depth"
          ("Function '" <> fmName <> "' has nesting depth of " <>
           T.pack (show fmNestingDepth) <> ". Deep nesting reduces readability.")
        else Nothing

    , -- Parameter count
      if fmParameterCount >= ctParameterWarning
        then Just $ mkDiag Warning "parameter-count"
          ("Function '" <> fmName <> "' has " <> T.pack (show fmParameterCount) <>
           " parameters. Consider using a record type.")
        else Nothing

    , -- Pattern branches
      if fmPatternBranches >= ctPatternBranchWarning
        then Just $ mkDiag Warning "pattern-branches"
          ("Function '" <> fmName <> "' has " <> T.pack (show fmPatternBranches) <>
           " pattern branches. Consider using helper functions.")
        else Nothing
    ]
  where
    mkDiag severity code msg = Diagnostic
      { diagSpan = fmSpan
      , diagSeverity = severity
      , diagKind = Custom "complexity"
      , diagMessage = msg
      , diagCode = Just $ "complexity/" <> code
      , diagFixes = []
      , diagRelated = []
      }
