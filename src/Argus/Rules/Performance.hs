{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Performance
-- Description : Detection of performance anti-patterns
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module detects common performance anti-patterns in Haskell code,
-- including inefficient data structures, algorithmic issues, and more.
module Argus.Rules.Performance
  ( -- * Detection
    detectPerformanceIssues
  , PerformanceFinding (..)
  , PerformanceCategory (..)

    -- * Configuration
  , PerformanceConfig (..)
  , defaultPerformanceConfig

    -- * Specific Detectors
  , detectBoxingIssues
  , detectInlineOpportunities
  , detectMemoryIssues
  , detectStrictnessOpportunities
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (catMaybes, mapMaybe)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import Argus.Types
import Argus.Analysis.TextProcessing
  ( isCodeLine, extractCode, patternInCode, detectChainedFunction )

--------------------------------------------------------------------------------
-- Performance-Specific Utilities
--------------------------------------------------------------------------------

-- | Check if line contains lazy mutable reference operations (can accumulate thunks)
hasLazyMutableOp :: Text -> Bool
hasLazyMutableOp line =
  let code = extractCode line
  in any (`T.isInfixOf` code)
       [ "modifyIORef "    -- Lazy modifyIORef (not modifyIORef')
       , "modifyMVar "     -- Lazy modifyMVar
       , "modifyMVar_ "    -- Lazy modifyMVar_
       , "modifyTVar "     -- Lazy modifyTVar
       , "modifySTRef "    -- Lazy modifySTRef
       , "writeIORef "     -- Can be lazy if value is lazy
       , "writeTVar "      -- Can be lazy if value is lazy
       , "putMVar "        -- Can be lazy
       , "atomicModifyIORef " -- Lazy atomic modify
       ]

-- | Check if line already uses strict variants
hasStrictMutableOp :: Text -> Bool
hasStrictMutableOp line =
  let code = extractCode line
  in any (`T.isInfixOf` code)
       [ "modifyIORef'"    -- Strict variant
       , "modifyMVar'"     -- Strict variant
       , "modifyMVar_'"    -- Strict variant
       , "modifyTVar'"     -- Strict variant
       , "modifySTRef'"    -- Strict variant
       , "atomicModifyIORef'" -- Strict atomic
       , "$!"              -- Strict application
       , "seq "            -- Explicit forcing
       , "deepseq"         -- Deep forcing
       , "evaluate "       -- Force evaluation
       ]

-- | Check if line is just reading (read operations are already strict)
hasReadOnlyOp :: Text -> Bool
hasReadOnlyOp line =
  let code = extractCode line
  in any (`T.isInfixOf` code)
       [ "readIORef"       -- Reading is strict
       , "readTVar"        -- Reading is strict
       , "readTVarIO"      -- Reading is strict
       , "readMVar"        -- Reading is strict
       , "takeMVar"        -- Taking is strict
       , "tryReadMVar"     -- Reading is strict
       , "tryTakeMVar"     -- Taking is strict
       , "readSTRef"       -- Reading is strict
       , ":: IORef"        -- Type signature only
       , ":: TVar"         -- Type signature only
       , ":: MVar"         -- Type signature only
       , "newIORef"        -- Creating (no accumulation yet)
       , "newTVar"         -- Creating
       , "newTVarIO"       -- Creating
       , "newMVar"         -- Creating
       , "newEmptyMVar"    -- Creating
       ]

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Category of performance issue
data PerformanceCategory
  = InefficientDataStructure   -- ^ Wrong data structure for the job
  | AlgorithmicComplexity      -- ^ O(n²) or worse patterns
  | StringUsage                -- ^ String instead of Text/ByteString
  | LazyDataStructure          -- ^ Using lazy when strict needed
  | UnnecessaryAllocation      -- ^ Allocating when not needed
  | BoxingOverhead             -- ^ Unboxed types would be better
  | FusionBlocker              -- ^ Patterns that prevent stream fusion
  | InefficientIO              -- ^ Inefficient IO patterns
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A detected performance finding
data PerformanceFinding = PerformanceFinding
  { pfCategory    :: PerformanceCategory  -- ^ Type of issue
  , pfSpan        :: SrcSpan              -- ^ Location
  , pfCode        :: Text                 -- ^ The problematic code
  , pfExplanation :: Text                 -- ^ Why this is a problem
  , pfFixHint     :: Text                 -- ^ How to fix it (text explanation)
  , pfSeverity    :: Severity             -- ^ How serious
  , pfComplexity  :: Maybe Text           -- ^ Time complexity if known
  , pfAutoFix     :: [Fix]                -- ^ Auto-fixes if available
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for performance detection
data PerformanceConfig = PerformanceConfig
  { perfEnabled             :: Bool   -- ^ Enable detection
  , perfCheckDataStructures :: Bool   -- ^ Check for inefficient structures
  , perfCheckAlgorithms     :: Bool   -- ^ Check for algorithmic issues
  , perfCheckStrings        :: Bool   -- ^ Check String usage
  , perfCheckLazy           :: Bool   -- ^ Check lazy vs strict
  , perfCheckAllocation     :: Bool   -- ^ Check unnecessary allocation
  , perfCheckIO             :: Bool   -- ^ Check IO patterns
  , perfCheckFusion         :: Bool   -- ^ Check fusion blockers
  , perfCheckBoxing         :: Bool   -- ^ Check boxing overhead
  , perfCheckInline         :: Bool   -- ^ Check inline opportunities
  , perfCheckMemory         :: Bool   -- ^ Check memory issues
  , perfCheckStrictness     :: Bool   -- ^ Check strictness opportunities
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration
defaultPerformanceConfig :: PerformanceConfig
defaultPerformanceConfig = PerformanceConfig
  { perfEnabled = True
  , perfCheckDataStructures = True
  , perfCheckAlgorithms = True
  , perfCheckStrings = True
  , perfCheckLazy = True
  , perfCheckAllocation = True
  , perfCheckIO = True
  , perfCheckFusion = True
  , perfCheckBoxing = True
  , perfCheckInline = False   -- Off by default - can be noisy
  , perfCheckMemory = True
  , perfCheckStrictness = False  -- Off by default - can be noisy
  }

--------------------------------------------------------------------------------
-- Detection
--------------------------------------------------------------------------------

-- | Detect performance issues in source code
detectPerformanceIssues :: PerformanceConfig -> FilePath -> Text -> [Diagnostic]
detectPerformanceIssues config path content
  | not (perfEnabled config) = []
  | otherwise =
    let issues = concat
          [ if perfCheckDataStructures config then detectDataStructureIssues path content else []
          , if perfCheckAlgorithms config then detectAlgorithmicIssues path content else []
          , if perfCheckStrings config then detectStringIssues path content else []
          , if perfCheckLazy config then detectLazyIssues path content else []
          , if perfCheckAllocation config then detectAllocationIssues path content else []
          , if perfCheckIO config then detectIOIssues path content else []
          , if perfCheckFusion config then detectFusionBlockers path content else []
          , if perfCheckBoxing config then detectBoxingIssues path content else []
          , if perfCheckInline config then detectInlineOpportunities path content else []
          , if perfCheckMemory config then detectMemoryIssues path content else []
          , if perfCheckStrictness config then detectStrictnessOpportunities path content else []
          ]
    in map issueToDiagnostic issues

--------------------------------------------------------------------------------
-- Data Structure Issues
--------------------------------------------------------------------------------

-- | Detect inefficient data structure usage
detectDataStructureIssues :: FilePath -> Text -> [PerformanceFinding]
detectDataStructureIssues path content =
  let codeLines = filter (isCodeLine . snd) $ zip [1..] (T.lines content)
  in concatMap (checkDataStructureLine path content) codeLines

checkDataStructureLine :: FilePath -> Text -> (Int, Text) -> [PerformanceFinding]
checkDataStructureLine path content (lineNum, line)
  | not (isCodeLine line) = []  -- Skip comments
  | otherwise = catMaybes
  [ -- nub is O(n²)
    if patternInCode " nub " line || patternInCode "nub " line
    then Just PerformanceFinding
      { pfCategory = AlgorithmicComplexity
      , pfSpan = mkSpan path lineNum "nub" line
      , pfCode = T.strip line
      , pfExplanation = "nub is O(n²) as it uses Eq comparison"
      , pfFixHint = "Use nubOrd from containers or Data.List.Extra for O(n log n), or use Set/HashSet"
      , pfSeverity = Warning
      , pfComplexity = Just "O(n²)", pfAutoFix = []
      }
    else Nothing

  , -- sort . nub pattern
    if patternInCode "sort . nub" line || patternInCode "sort $ nub" line
    then Just PerformanceFinding
      { pfCategory = AlgorithmicComplexity
      , pfSpan = mkSpan path lineNum "sort" line
      , pfCode = T.strip line
      , pfExplanation = "sort . nub is O(n² + n log n)"
      , pfFixHint = "Use Set.toAscList . Set.fromList for O(n log n)"
      , pfSeverity = Warning
      , pfComplexity = Just "O(n²)", pfAutoFix = []
      }
    else Nothing

  , -- List lookup with elem
    if patternInCode " elem " line && not ("Set" `T.isInfixOf` content) &&
       not ("Map" `T.isInfixOf` content)
    then Just PerformanceFinding
      { pfCategory = InefficientDataStructure
      , pfSpan = mkSpan path lineNum "elem" line
      , pfCode = T.strip line
      , pfExplanation = "elem is O(n) - if called repeatedly, consider Set for O(log n) or HashSet for O(1)"
      , pfFixHint = "Use Set.member or HashSet.member for frequent lookups"
      , pfSeverity = Suggestion
      , pfComplexity = Just "O(n)", pfAutoFix = []
      }
    else Nothing

  , -- (++) in left fold pattern
    if patternInCode " ++ " line && (patternInCode "foldl" line || patternInCode "foldl'" line)
    then Just PerformanceFinding
      { pfCategory = AlgorithmicComplexity
      , pfSpan = mkSpan path lineNum "++" line
      , pfCode = T.strip line
      , pfExplanation = "(++) is O(n) in left argument; in foldl this becomes O(n²)"
      , pfFixHint = "Use difference lists, Builder, or foldr with (++)"
      , pfSeverity = Warning
      , pfComplexity = Just "O(n²)", pfAutoFix = []
      }
    else Nothing

  , -- assoc list instead of Map
    if patternInCode "lookup " line && not (patternInCode "Map.lookup" line) &&
       not (patternInCode "M.lookup" line) && "[(String" `T.isInfixOf` content
    then Just PerformanceFinding
      { pfCategory = InefficientDataStructure
      , pfSpan = mkSpan path lineNum "lookup" line
      , pfCode = T.strip line
      , pfExplanation = "Association list lookup is O(n)"
      , pfFixHint = "Use Data.Map or Data.HashMap for O(log n) or O(1) lookup"
      , pfSeverity = Suggestion
      , pfComplexity = Just "O(n)", pfAutoFix = []
      }
    else Nothing

  , -- List filter + length pattern
    if patternInCode "length" line && patternInCode "filter" line
    then Just PerformanceFinding
      { pfCategory = UnnecessaryAllocation
      , pfSpan = mkSpan path lineNum "length" line
      , pfCode = T.strip line
      , pfExplanation = "length . filter allocates intermediate list"
      , pfFixHint = "Use foldl' counting: foldl' (\\n x -> if p x then n+1 else n) 0"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Algorithmic Complexity Issues
--------------------------------------------------------------------------------

-- | Detect algorithmic complexity issues
detectAlgorithmicIssues :: FilePath -> Text -> [PerformanceFinding]
detectAlgorithmicIssues path content =
  let codeLines = filter (isCodeLine . snd) $ zip [1..] (T.lines content)
      lineLevelIssues = concatMap (checkAlgorithmicLine path) codeLines
      fileLevelIssues = checkFileLevelAlgorithmicIssues path content
  in lineLevelIssues ++ fileLevelIssues

-- | Check for file-level algorithmic issues (reported only once per file)
checkFileLevelAlgorithmicIssues :: FilePath -> Text -> [PerformanceFinding]
checkFileLevelAlgorithmicIssues path content =
  let codeLines = filter isCodeLine (T.lines content)
      mapCount = length $ filter (patternInCode "map ") codeLines
      filterCount = length $ filter (patternInCode "filter ") codeLines
  in catMaybes
    [ -- Multiple list traversals - report once at file level
      if mapCount > 5 || filterCount > 5
      then Just PerformanceFinding
        { pfCategory = UnnecessaryAllocation
        , pfSpan = mkSrcSpanRaw path 1 1 1 1  -- File-level span
        , pfCode = "Multiple map/filter usages"
        , pfExplanation = "File has " <> T.pack (show mapCount) <> " map and " <>
                          T.pack (show filterCount) <> " filter calls - " <>
                          "consider combining chained operations"
        , pfFixHint = "Look for map f . map g patterns to combine into map (f . g)"
        , pfSeverity = Suggestion
        , pfComplexity = Nothing
        , pfAutoFix = []
        }
      else Nothing
    ]

checkAlgorithmicLine :: FilePath -> (Int, Text) -> [PerformanceFinding]
checkAlgorithmicLine path (lineNum, line)
  | not (isCodeLine line) = []  -- Skip comments and blank lines
  | otherwise = catMaybes
  [ -- Nested list comprehension with elem (only check actual code)
    if patternInCode "[" line && patternInCode "|" line && patternInCode "elem" line
    then Just PerformanceFinding
      { pfCategory = AlgorithmicComplexity
      , pfSpan = mkSpan path lineNum "elem" line
      , pfCode = T.strip line
      , pfExplanation = "elem in list comprehension may cause O(n²) if outer iteration is large"
      , pfFixHint = "Pre-compute Set from lookup list for O(n log n) total"
      , pfSeverity = Suggestion
      , pfComplexity = Just "O(n²)", pfAutoFix = []
      }
    else Nothing

  , -- reverse with take (only on code, not comments)
    if patternInCode "reverse" line && patternInCode "take" line
    then Just PerformanceFinding
      { pfCategory = AlgorithmicComplexity
      , pfSpan = mkSpan path lineNum "reverse" line
      , pfCode = T.strip line
      , pfExplanation = "reverse forces entire list just to take a few elements"
      , pfFixHint = "Consider using a Seq or keeping data in reversed order"
      , pfSeverity = Suggestion
      , pfComplexity = Just "O(n)", pfAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- String Usage Issues
--------------------------------------------------------------------------------

-- | Detect String usage where Text/ByteString would be better
detectStringIssues :: FilePath -> Text -> [PerformanceFinding]
detectStringIssues path content =
  let codeLines = filter (isCodeLine . snd) $ zip [1..] (T.lines content)
      hasTextImport = "Data.Text" `T.isInfixOf` content
  in if hasTextImport
     then []  -- Already using Text
     else mapMaybe (checkStringLine path) codeLines

checkStringLine :: FilePath -> (Int, Text) -> Maybe PerformanceFinding
checkStringLine path (lineNum, line)
  | not (isCodeLine line) = Nothing
  | patternInCode " :: String" line || patternInCode ":: String " line ||
    patternInCode "[String]" line = Just PerformanceFinding
      { pfCategory = StringUsage
      , pfSpan = mkSpan path lineNum "String" line
      , pfCode = T.strip line
      , pfExplanation = "String is [Char] - inefficient for large text"
      , pfFixHint = "Use Text for Unicode text or ByteString for binary data"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
  | patternInCode "readFile " line && not (patternInCode "T.readFile" line) = Just PerformanceFinding
      { pfCategory = StringUsage
      , pfSpan = mkSpan path lineNum "readFile" line
      , pfCode = T.strip line
      , pfExplanation = "Prelude.readFile returns String - inefficient for large files"
      , pfFixHint = "Use Data.Text.IO.readFile or Data.ByteString.readFile"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Lazy vs Strict Issues
--------------------------------------------------------------------------------

-- | Detect lazy data structure usage where strict would be better
detectLazyIssues :: FilePath -> Text -> [PerformanceFinding]
detectLazyIssues path content =
  let codeLines = filter (isCodeLine . snd) $ zip [1..] (T.lines content)
  in mapMaybe (checkLazyLine path) codeLines

checkLazyLine :: FilePath -> (Int, Text) -> Maybe PerformanceFinding
checkLazyLine path (lineNum, line)
  | not (isCodeLine line) = Nothing
  | patternInCode "Data.Map " line && not (patternInCode "Strict" line) = Just PerformanceFinding
      { pfCategory = LazyDataStructure
      , pfSpan = mkSpan path lineNum "Data.Map" line
      , pfCode = T.strip line
      , pfExplanation = "Data.Map is lazy in values - can accumulate thunks"
      , pfFixHint = "Use Data.Map.Strict unless you need lazy evaluation"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
  | patternInCode "Data.IntMap " line && not (patternInCode "Strict" line) = Just PerformanceFinding
      { pfCategory = LazyDataStructure
      , pfSpan = mkSpan path lineNum "Data.IntMap" line
      , pfCode = T.strip line
      , pfExplanation = "Data.IntMap is lazy in values"
      , pfFixHint = "Use Data.IntMap.Strict"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
  | patternInCode "Data.HashMap " line && not (patternInCode "Strict" line) = Just PerformanceFinding
      { pfCategory = LazyDataStructure
      , pfSpan = mkSpan path lineNum "Data.HashMap" line
      , pfCode = T.strip line
      , pfExplanation = "Data.HashMap is lazy in values"
      , pfFixHint = "Use Data.HashMap.Strict"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Allocation Issues
--------------------------------------------------------------------------------

-- | Detect unnecessary allocation patterns
detectAllocationIssues :: FilePath -> Text -> [PerformanceFinding]
detectAllocationIssues path content =
  let codeLines = filter (isCodeLine . snd) $ zip [1..] (T.lines content)
  in concatMap (checkAllocationLine path) codeLines

checkAllocationLine :: FilePath -> (Int, Text) -> [PerformanceFinding]
checkAllocationLine path (lineNum, line)
  | not (isCodeLine line) = []  -- Skip comments
  | otherwise = catMaybes
  [ -- map f . map g pattern - detect chained maps via . or $
    -- Match patterns like: "map f . map g", "map f $ map g $ xs", etc.
    -- Uses flexible detection: two "map " occurrences on the same line
    if detectChainedFunction "map" line
    then Just PerformanceFinding
      { pfCategory = UnnecessaryAllocation
      , pfSpan = mkSpan path lineNum "map" line
      , pfCode = T.strip line
      , pfExplanation = "map f . map g allocates intermediate list"
      , pfFixHint = "Combine into map (f . g)"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  , -- filter . filter pattern - detect chained filters via . or $
    -- Match patterns like: "filter p . filter q", "filter p $ filter q $ xs", etc.
    if detectChainedFunction "filter" line
    then Just PerformanceFinding
      { pfCategory = UnnecessaryAllocation
      , pfSpan = mkSpan path lineNum "filter" line
      , pfCode = T.strip line
      , pfExplanation = "filter p . filter q allocates intermediate list"
      , pfFixHint = "Combine into filter (\\x -> p x && q x)"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  , -- concat . map pattern (concatMap)
    if patternInCode "concat . map" line || patternInCode "concat $ map" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "concat . map" "concatMap" $ T.replace "concat $ map" "concatMap" (T.strip line)
      in Just PerformanceFinding
        { pfCategory = UnnecessaryAllocation
        , pfSpan = mkSpan path lineNum "concat" line
        , pfCode = T.strip line
        , pfExplanation = "concat . map allocates intermediate list of lists"
        , pfFixHint = "Use concatMap for single-pass"
        , pfSeverity = Warning
        , pfComplexity = Nothing
        , pfAutoFix = [mkPerfFix "Use concatMap" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- concat (map f xs) pattern
    if patternInCode "concat (map" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          -- Replace "concat (map f xs)" with "concatMap f xs" by:
          -- 1. Replace "concat (map" with "concatMap"
          -- 2. Remove the matching closing paren from "concat (..."
          replaced = T.replace "concat (map" "concatMap" (T.strip line)
          -- Find and remove the extra closing paren that paired with "concat ("
          fixed = removeExtraParen replaced
      in Just PerformanceFinding
        { pfCategory = UnnecessaryAllocation
        , pfSpan = mkSpan path lineNum "concat" line
        , pfCode = T.strip line
        , pfExplanation = "concat (map f xs) allocates intermediate list of lists"
        , pfFixHint = "Use concatMap f xs for single-pass"
        , pfSeverity = Warning
        , pfComplexity = Nothing
        , pfAutoFix = [mkPerfFix "Use concatMap" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- mconcat $ map → foldMap
    if patternInCode "mconcat $ map" line || patternInCode "mconcat . map" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "mconcat $ map" "foldMap" $ T.replace "mconcat . map" "foldMap" (T.strip line)
      in Just PerformanceFinding
        { pfCategory = UnnecessaryAllocation
        , pfSpan = mkSpan path lineNum "mconcat" line
        , pfCode = T.strip line
        , pfExplanation = "mconcat . map allocates intermediate list"
        , pfFixHint = "Use foldMap for single-pass"
        , pfSeverity = Warning
        , pfComplexity = Nothing
        , pfAutoFix = [mkPerfFixWithImports "Use foldMap" [FixEdit lineSpan fixed] True
                        [mkPerfImport "Data.Foldable" "foldMap"]]
        }
    else Nothing

  , -- mconcat (map f xs) → foldMap f xs
    if patternInCode "mconcat (map" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          -- Replace "mconcat (map f xs)" with "foldMap f xs"
          replaced = T.replace "mconcat (map" "foldMap" (T.strip line)
          fixed = removeExtraParen replaced
      in Just PerformanceFinding
        { pfCategory = UnnecessaryAllocation
        , pfSpan = mkSpan path lineNum "mconcat" line
        , pfCode = T.strip line
        , pfExplanation = "mconcat (map f xs) allocates intermediate list"
        , pfFixHint = "Use foldMap f xs for single-pass"
        , pfSeverity = Warning
        , pfComplexity = Nothing
        , pfAutoFix = [mkPerfFixWithImports "Use foldMap" [FixEdit lineSpan fixed] True
                        [mkPerfImport "Data.Foldable" "foldMap"]]
        }
    else Nothing

  , -- foldr (++) [] → concat
    if patternInCode "foldr (++) []" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "foldr (++) []" "concat" (T.strip line)
      in Just PerformanceFinding
        { pfCategory = UnnecessaryAllocation
        , pfSpan = mkSpan path lineNum "foldr (++)" line
        , pfCode = T.strip line
        , pfExplanation = "foldr (++) [] is the definition of concat"
        , pfFixHint = "Use concat directly"
        , pfSeverity = Suggestion
        , pfComplexity = Nothing
        , pfAutoFix = [mkPerfFix "Use concat" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- words . unwords pattern
    if patternInCode "words . unwords" line || patternInCode "words $ unwords" line
    then Just PerformanceFinding
      { pfCategory = UnnecessaryAllocation
      , pfSpan = mkSpan path lineNum "words" line
      , pfCode = T.strip line
      , pfExplanation = "words . unwords is identity but allocates"
      , pfFixHint = "Remove this identity operation"
      , pfSeverity = Warning
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  , -- head . sort → minimum (with fix)
    if patternInCode "head . sort" line || patternInCode "head $ sort" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "head . sort" "minimum" $ T.replace "head $ sort" "minimum" (T.strip line)
      in Just PerformanceFinding
        { pfCategory = AlgorithmicComplexity
        , pfSpan = mkSpan path lineNum "head" line
        , pfCode = T.strip line
        , pfExplanation = "head . sort is O(n log n), minimum is O(n)"
        , pfFixHint = "Use minimum for O(n) performance"
        , pfSeverity = Warning
        , pfComplexity = Just "O(n log n) → O(n)"
        , pfAutoFix = [mkPerfFix "Use minimum" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- last . sort → maximum
    if patternInCode "last . sort" line || patternInCode "last $ sort" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "last . sort" "maximum" $ T.replace "last $ sort" "maximum" (T.strip line)
      in Just PerformanceFinding
        { pfCategory = AlgorithmicComplexity
        , pfSpan = mkSpan path lineNum "last" line
        , pfCode = T.strip line
        , pfExplanation = "last . sort is O(n log n), maximum is O(n)"
        , pfFixHint = "Use maximum for O(n) performance"
        , pfSeverity = Warning
        , pfComplexity = Just "O(n log n) → O(n)"
        , pfAutoFix = [mkPerfFix "Use maximum" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- reverse . sort → sortBy (flip compare) or sortDesc
    if patternInCode "reverse . sort" line || patternInCode "reverse $ sort" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "reverse . sort" "sortBy (flip compare)" $ T.replace "reverse $ sort" "sortBy (flip compare)" (T.strip line)
      in Just PerformanceFinding
        { pfCategory = UnnecessaryAllocation
        , pfSpan = mkSpan path lineNum "reverse" line
        , pfCode = T.strip line
        , pfExplanation = "reverse . sort allocates twice"
        , pfFixHint = "Use sortBy (flip compare) or sortDown"
        , pfSeverity = Suggestion
        , pfComplexity = Nothing
        , pfAutoFix = [mkPerfFix "Use sortBy (flip compare)" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  -- =====================
  -- AUTO-FIXABLE PATTERNS
  -- =====================

  , -- length xs == 0 → null xs
    if patternInCode "length " line && patternInCode " == 0" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = replacePattern "length ([^ ]+) == 0" "null \\1" line
      in Just PerformanceFinding
        { pfCategory = AlgorithmicComplexity
        , pfSpan = mkSpan path lineNum "length" line
        , pfCode = T.strip line
        , pfExplanation = "length xs == 0 traverses entire list to check emptiness"
        , pfFixHint = "Use 'null xs' for O(1) check"
        , pfSeverity = Warning
        , pfComplexity = Just "O(n) → O(1)"
        , pfAutoFix = case fixed of
            Just f -> [mkPerfFix "Use null" [FixEdit lineSpan f] True]
            Nothing -> []
        }
    else Nothing

  , -- length xs > 0 → not (null xs)
    if patternInCode "length " line && patternInCode " > 0" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = replacePattern "length ([^ ]+) > 0" "not (null \\1)" line
      in Just PerformanceFinding
        { pfCategory = AlgorithmicComplexity
        , pfSpan = mkSpan path lineNum "length" line
        , pfCode = T.strip line
        , pfExplanation = "length xs > 0 traverses entire list to check non-emptiness"
        , pfFixHint = "Use 'not (null xs)' for O(1) check"
        , pfSeverity = Warning
        , pfComplexity = Just "O(n) → O(1)"
        , pfAutoFix = case fixed of
            Just f -> [mkPerfFix "Use not (null ...)" [FixEdit lineSpan f] True]
            Nothing -> []
        }
    else Nothing

  , -- length xs /= 0 → not (null xs)
    if patternInCode "length " line && patternInCode " /= 0" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = replacePattern "length ([^ ]+) /= 0" "not (null \\1)" line
      in Just PerformanceFinding
        { pfCategory = AlgorithmicComplexity
        , pfSpan = mkSpan path lineNum "length" line
        , pfCode = T.strip line
        , pfExplanation = "length xs /= 0 traverses entire list to check non-emptiness"
        , pfFixHint = "Use 'not (null xs)' for O(1) check"
        , pfSeverity = Warning
        , pfComplexity = Just "O(n) → O(1)"
        , pfAutoFix = case fixed of
            Just f -> [mkPerfFix "Use not (null ...)" [FixEdit lineSpan f] True]
            Nothing -> []
        }
    else Nothing

  , -- any id → or
    if patternInCode "any id " line || patternInCode "any id$" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "any id" "or" (T.strip line)
      in Just PerformanceFinding
        { pfCategory = UnnecessaryAllocation
        , pfSpan = mkSpan path lineNum "any id" line
        , pfCode = T.strip line
        , pfExplanation = "any id is equivalent to or"
        , pfFixHint = "Use 'or' for clearer intent"
        , pfSeverity = Suggestion
        , pfComplexity = Nothing
        , pfAutoFix = [mkPerfFix "Use or" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- all id → and
    if patternInCode "all id " line || patternInCode "all id$" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "all id" "and" (T.strip line)
      in Just PerformanceFinding
        { pfCategory = UnnecessaryAllocation
        , pfSpan = mkSpan path lineNum "all id" line
        , pfCode = T.strip line
        , pfExplanation = "all id is equivalent to and"
        , pfFixHint = "Use 'and' for clearer intent"
        , pfSeverity = Suggestion
        , pfComplexity = Nothing
        , pfAutoFix = [mkPerfFix "Use and" [FixEdit lineSpan fixed] True]
        }
    else Nothing
  ]

-- | Simple pattern replacement (regex-like but simplified)
replacePattern :: Text -> Text -> Text -> Maybe Text
replacePattern _pat _replacement txt =
  -- For simple patterns like "length foo == 0" → "null foo"
  -- This is a simplified version - real implementation would use regex
  case (T.breakOn "length " txt, T.breakOn " == 0" txt, T.breakOn " > 0" txt, T.breakOn " /= 0" txt) of
    ((before, rest), _, _, _)
      | "length " `T.isPrefixOf` rest
      , " == 0" `T.isInfixOf` rest ->
        let afterLength = T.drop 7 rest  -- drop "length "
            (varPart, _) = T.breakOn " ==" afterLength
        in Just $ before <> "null " <> T.strip varPart <> T.drop (T.length varPart + 5) afterLength
    ((before, rest), _, (_, _), _)
      | "length " `T.isPrefixOf` rest
      , " > 0" `T.isInfixOf` rest ->
        let afterLength = T.drop 7 rest
            (varPart, _) = T.breakOn " >" afterLength
        in Just $ before <> "not (null " <> T.strip varPart <> ")" <> T.drop (T.length varPart + 4) afterLength
    ((before, rest), _, _, (_, _))
      | "length " `T.isPrefixOf` rest
      , " /= 0" `T.isInfixOf` rest ->
        let afterLength = T.drop 7 rest
            (varPart, _) = T.breakOn " /=" afterLength
        in Just $ before <> "not (null " <> T.strip varPart <> ")" <> T.drop (T.length varPart + 5) afterLength
    _ -> Nothing

--------------------------------------------------------------------------------
-- IO Performance Issues
--------------------------------------------------------------------------------

-- | Detect inefficient IO patterns
detectIOIssues :: FilePath -> Text -> [PerformanceFinding]
detectIOIssues path content =
  let codeLines = filter (isCodeLine . snd) $ zip [1..] (T.lines content)
  in concatMap (checkIOLine path content) codeLines

checkIOLine :: FilePath -> Text -> (Int, Text) -> [PerformanceFinding]
checkIOLine path content (lineNum, line)
  | not (isCodeLine line) = []
  | otherwise = catMaybes
  [ -- appendFile in loop (detected by presence in function with loop patterns)
    if patternInCode "appendFile" line && any (`T.isInfixOf` content) ["forM_", "mapM_", "for_"]
    then Just PerformanceFinding
      { pfCategory = InefficientIO
      , pfSpan = mkSpan path lineNum "appendFile" line
      , pfCode = T.strip line
      , pfExplanation = "appendFile in loop opens/closes file each iteration"
      , pfFixHint = "Use withFile and hPutStrLn, or collect data and write once"
      , pfSeverity = Warning
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  -- Note: Removed file-level "many putStrLn" check as it was causing false positives
  -- by reporting on every putStrLn line when count > 5. This is better done as
  -- a file-level check in detectIOIssues.
  ]

--------------------------------------------------------------------------------
-- Stream Fusion Blockers
--------------------------------------------------------------------------------

-- | Detect patterns that block stream fusion
detectFusionBlockers :: FilePath -> Text -> [PerformanceFinding]
detectFusionBlockers path content =
  let codeLines = filter (isCodeLine . snd) $ zip [1..] (T.lines content)
  in concatMap (checkFusionLine path content) codeLines

checkFusionLine :: FilePath -> Text -> (Int, Text) -> [PerformanceFinding]
checkFusionLine path content (lineNum, line)
  | not (isCodeLine line) = []
  | otherwise = catMaybes
  [ -- toList . fromList pattern
    if patternInCode "toList . fromList" line || patternInCode "toList $ fromList" line
    then Just PerformanceFinding
      { pfCategory = FusionBlocker
      , pfSpan = mkSpan path lineNum "toList" line
      , pfCode = T.strip line
      , pfExplanation = "toList . fromList creates intermediate structure"
      , pfFixHint = "This is likely unnecessary - review the transformation"
      , pfSeverity = Warning
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  , -- force/evaluate patterns in hot loops
    if (patternInCode "force" line || patternInCode "evaluate" line) &&
       any (`T.isInfixOf` content) ["forM ", "mapM ", "replicateM "]
    then Just PerformanceFinding
      { pfCategory = FusionBlocker
      , pfSpan = mkSpan path lineNum "force" line
      , pfCode = T.strip line
      , pfExplanation = "force/evaluate in loops may prevent fusion"
      , pfFixHint = "Consider using strict data types or UNPACK pragmas instead"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  , -- Vector freeze/thaw in loop
    if (patternInCode "freeze" line || patternInCode "thaw" line) &&
       "Vector" `T.isInfixOf` content
    then Just PerformanceFinding
      { pfCategory = FusionBlocker
      , pfSpan = mkSpan path lineNum "freeze" line
      , pfCode = T.strip line
      , pfExplanation = "freeze/thaw copies the entire vector"
      , pfFixHint = "Use modify or unsafeFreeze/unsafeThaw if safe"
      , pfSeverity = Warning
      , pfComplexity = Just "O(n)", pfAutoFix = []
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
issueToDiagnostic :: PerformanceFinding -> Diagnostic
issueToDiagnostic PerformanceFinding{..} = Diagnostic
  { diagSpan = pfSpan
  , diagSeverity = pfSeverity
  , diagKind = PerformanceIssue
  , diagMessage = pfExplanation <>
                  maybe "" (\c -> " (Complexity: " <> c <> ")") pfComplexity <>
                  ". " <> pfFixHint
  , diagCode = Just $ "performance/" <> categoryCode pfCategory
  , diagFixes = pfAutoFix
  , diagRelated = []
  }

categoryCode :: PerformanceCategory -> Text
categoryCode = \case
  InefficientDataStructure -> "data-structure"
  AlgorithmicComplexity -> "algorithm"
  StringUsage -> "string"
  LazyDataStructure -> "lazy"
  UnnecessaryAllocation -> "allocation"
  BoxingOverhead -> "boxing"
  FusionBlocker -> "fusion"
  InefficientIO -> "io"

--------------------------------------------------------------------------------
-- Boxing Overhead Detection
--------------------------------------------------------------------------------

-- | Detect boxing overhead issues
detectBoxingIssues :: FilePath -> Text -> [PerformanceFinding]
detectBoxingIssues path content =
  let codeLines = filter (isCodeLine . snd) $ zip [1..] (T.lines content)
  in concatMap (checkBoxingLine path content) codeLines

checkBoxingLine :: FilePath -> Text -> (Int, Text) -> [PerformanceFinding]
checkBoxingLine path content (lineNum, line)
  | not (isCodeLine line) = []
  | otherwise = catMaybes
  [ -- Integer in numeric computations
    if patternInCode " Integer " line || patternInCode ":: Integer" line
    then Just PerformanceFinding
      { pfCategory = BoxingOverhead
      , pfSpan = mkSpan path lineNum "Integer" line
      , pfCode = T.strip line
      , pfExplanation = "Integer is arbitrary precision and boxed"
      , pfFixHint = "Use Int64/Word64 if range is known, or Int for machine word"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  , -- Ratio usage
    if patternInCode "Ratio " line || patternInCode "Rational" line
    then Just PerformanceFinding
      { pfCategory = BoxingOverhead
      , pfSpan = mkSpan path lineNum "Ratio" line
      , pfCode = T.strip line
      , pfExplanation = "Ratio/Rational operations are relatively expensive"
      , pfFixHint = "Consider Double if precision allows, or specialized rational libs"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  , -- Maybe in hot path (boxing)
    if patternInCode "Maybe " line && (patternInCode "fold" line || patternInCode "map" line || patternInCode "for" line)
    then Just PerformanceFinding
      { pfCategory = BoxingOverhead
      , pfSpan = mkSpan path lineNum "Maybe" line
      , pfCode = T.strip line
      , pfExplanation = "Maybe in tight loops adds boxing overhead"
      , pfFixHint = "Consider strict-base-types or unboxed sums (MaybeUnpack pattern)"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  , -- (,) pair unpacking
    if patternInCode "(,)" line && not ("UNPACK" `T.isInfixOf` content)
    then Just PerformanceFinding
      { pfCategory = BoxingOverhead
      , pfSpan = mkSpan path lineNum "(,)" line
      , pfCode = T.strip line
      , pfExplanation = "Pairs are boxed by default"
      , pfFixHint = "Use strict data types with UNPACK pragmas in performance-critical code"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- INLINE/INLINABLE Pragma Suggestions
--------------------------------------------------------------------------------

-- | Detect functions that would benefit from INLINE pragmas
detectInlineOpportunities :: FilePath -> Text -> [PerformanceFinding]
detectInlineOpportunities path content =
  let codeLines = filter (isCodeLine . snd) $ zip [1..] (T.lines content)
      hasInlinePragma = "INLINE" `T.isInfixOf` content
  in if hasInlinePragma
     then []  -- Already using INLINE pragmas
     else concatMap (checkInlineLine path content) codeLines

checkInlineLine :: FilePath -> Text -> (Int, Text) -> [PerformanceFinding]
checkInlineLine path content (lineNum, line)
  | not (isCodeLine line) = []
  | otherwise = catMaybes
  [ -- Small functions that could benefit from inlining
    if (patternInCode " . " line || patternInCode " $ " line) &&
       not ("where" `T.isInfixOf` content) &&
       T.length line < 80 &&
       patternInCode " = " line &&
       not (patternInCode "::" line)
    then Just PerformanceFinding
      { pfCategory = FusionBlocker
      , pfSpan = mkSpan path lineNum "=" line
      , pfCode = T.strip line
      , pfExplanation = "Small composed functions often benefit from inlining"
      , pfFixHint = "Add {-# INLINE funcName #-} for cross-module optimization"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  , -- Typeclass method implementations
    if "instance " `T.isInfixOf` content && patternInCode " = " line &&
       not ("SPECIALIZE" `T.isInfixOf` content)
    then Just PerformanceFinding
      { pfCategory = FusionBlocker
      , pfSpan = mkSpan path lineNum "instance" line
      , pfCode = T.strip line
      , pfExplanation = "Typeclass methods benefit from SPECIALIZE pragmas"
      , pfFixHint = "Add {-# SPECIALIZE method :: ConcreteType #-} for common types"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  , -- Recursive functions without INLINABLE
    if patternInCode "where" line && any (\w -> T.count w content > 1) (T.words line) &&
       not ("INLINABLE" `T.isInfixOf` content)
    then Just PerformanceFinding
      { pfCategory = FusionBlocker
      , pfSpan = mkSpan path lineNum "where" line
      , pfCode = T.strip line
      , pfExplanation = "Recursive functions with worker-wrapper can use INLINABLE"
      , pfFixHint = "Add {-# INLINABLE funcName #-} to expose unfolding"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Memory Profiling Hints
--------------------------------------------------------------------------------

-- | Detect patterns that may cause excessive memory usage
detectMemoryIssues :: FilePath -> Text -> [PerformanceFinding]
detectMemoryIssues path content =
  let codeLines = filter (isCodeLine . snd) $ zip [1..] (T.lines content)
  in concatMap (checkMemoryLine path content) codeLines

checkMemoryLine :: FilePath -> Text -> (Int, Text) -> [PerformanceFinding]
checkMemoryLine path content (lineNum, line)
  | not (isCodeLine line) = []
  | otherwise = catMaybes
  [ -- Large list literal
    if patternInCode "[" line && patternInCode ".." line &&
       not ("take" `T.isInfixOf` content)
    then Just PerformanceFinding
      { pfCategory = UnnecessaryAllocation
      , pfSpan = mkSpan path lineNum "[" line
      , pfCode = T.strip line
      , pfExplanation = "Enumeration [a..b] materializes entire list"
      , pfFixHint = "Consider using Vector.enumFromTo or streaming if large"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  , -- replicateM without streaming
    if patternInCode "replicateM " line && not (patternInCode "replicateM_" line)
    then Just PerformanceFinding
      { pfCategory = UnnecessaryAllocation
      , pfSpan = mkSpan path lineNum "replicateM" line
      , pfCode = T.strip line
      , pfExplanation = "replicateM collects all results in memory"
      , pfFixHint = "Use replicateM_ if results aren't needed, or stream"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  , -- sequence over large lists
    if patternInCode "sequence " line && not (patternInCode "sequence_" line)
    then Just PerformanceFinding
      { pfCategory = UnnecessaryAllocation
      , pfSpan = mkSpan path lineNum "sequence" line
      , pfCode = T.strip line
      , pfExplanation = "sequence collects all results before returning"
      , pfFixHint = "Use sequence_ if results aren't needed, or streaming"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  , -- Mutable reference modifications that can accumulate thunks
    -- Only flag modification operations, not read operations (which are already strict)
    if hasLazyMutableOp line && not (hasStrictMutableOp line) && not (hasReadOnlyOp line)
    then Just PerformanceFinding
      { pfCategory = LazyDataStructure
      , pfSpan = mkSpan path lineNum "Ref" line
      , pfCode = T.strip line
      , pfExplanation = "Mutable reference modification can accumulate thunks if not forced"
      , pfFixHint = "Use strict variants: modifyIORef'/modifyMVar'/modifyTVar'"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  , -- Long-lived closures
    if patternInCode "\\_ ->" line || patternInCode "\\ _ ->" line
    then Just PerformanceFinding
      { pfCategory = UnnecessaryAllocation
      , pfSpan = mkSpan path lineNum "\\" line
      , pfCode = T.strip line
      , pfExplanation = "Unused lambda argument still captures closure environment"
      , pfFixHint = "Use const or restructure to avoid closure capture"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Strictness Annotation Suggestions
--------------------------------------------------------------------------------

-- | Detect opportunities for bang patterns/strictness
detectStrictnessOpportunities :: FilePath -> Text -> [PerformanceFinding]
detectStrictnessOpportunities path content =
  let codeLines = filter (isCodeLine . snd) $ zip [1..] (T.lines content)
      hasStrictData = "StrictData" `T.isInfixOf` content || "Strict" `T.isInfixOf` content
  in if hasStrictData
     then []  -- Already using strict by default
     else concatMap (checkStrictnessLine path content) codeLines

checkStrictnessLine :: FilePath -> Text -> (Int, Text) -> [PerformanceFinding]
checkStrictnessLine path content (lineNum, line)
  | not (isCodeLine line) = []
  | otherwise = catMaybes
  [ -- Accumulator patterns without bang
    if (patternInCode "foldl'" line || patternInCode "foldM'" line) &&
       not (patternInCode "!" line)
    then Just PerformanceFinding
      { pfCategory = LazyDataStructure
      , pfSpan = mkSpan path lineNum "foldl'" line
      , pfCode = T.strip line
      , pfExplanation = "foldl' forces WHNF but accumulator fields may still be lazy"
      , pfFixHint = "Use bang patterns on accumulator components: !acc"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  , -- data type fields without strictness
    if patternInCode "data " line && patternInCode "::" line &&
       not (patternInCode "!" line) && not ("UNPACK" `T.isInfixOf` line)
    then Just PerformanceFinding
      { pfCategory = LazyDataStructure
      , pfSpan = mkSpan path lineNum "data" line
      , pfCode = T.strip line
      , pfExplanation = "Data type fields are lazy by default"
      , pfFixHint = "Consider {-# LANGUAGE StrictData #-} or explicit ! on fields"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing

  , -- let bindings in monadic code
    if patternInCode "let " line && patternInCode " = " line &&
       "do" `T.isInfixOf` content && not (patternInCode "!" line)
    then Just PerformanceFinding
      { pfCategory = LazyDataStructure
      , pfSpan = mkSpan path lineNum "let" line
      , pfCode = T.strip line
      , pfExplanation = "let bindings in do blocks create thunks"
      , pfFixHint = "Use let !x = expr for strict evaluation, or <- pure expr"
      , pfSeverity = Suggestion
      , pfComplexity = Nothing, pfAutoFix = []
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Fix Helper
--------------------------------------------------------------------------------

-- | Helper to create a performance fix with proper category and safety
mkPerfFix :: Text -> [FixEdit] -> Bool -> Fix
mkPerfFix title edits preferred = mkPerfFixWithImports title edits preferred []

-- | Helper to create a performance fix with imports
mkPerfFixWithImports :: Text -> [FixEdit] -> Bool -> [FixImport] -> Fix
mkPerfFixWithImports title edits preferred imports = Fix
  { fixTitle = title
  , fixEdits = edits
  , fixIsPreferred = preferred
  , fixAddImports = imports
  , fixRemoveImports = []
  , fixCategory = FCPerformance
  , fixSafety = FSMostly
  }

-- | Create a simple explicit import for performance fixes
mkPerfImport :: Text -> Text -> FixImport
mkPerfImport modName symName = FixImport
  { fimpModule = modName
  , fimpSymbols = [ImportSymbol symName ISTFunction []]
  , fimpQualified = Nothing
  , fimpHiding = False
  , fimpPackage = Nothing
  }

-- | Remove an extra closing paren from transformed expressions
-- When we replace "concat (map f xs)" with "concatMap f xs", we're left with
-- an extra ")" at the end that was the closing paren for "concat (".
-- This function removes the last unbalanced ")" from the expression.
removeExtraParen :: Text -> Text
removeExtraParen t =
  let str = T.unpack t
      -- Find position of the last ')' that would make parens unbalanced
      removeLastParen = go 0 [] str
      go :: Int -> String -> String -> String
      go _ acc [] = reverse acc
      go depth acc (c:cs)
        | c == '(' = go (depth + 1) (c:acc) cs
        | c == ')' && depth > 0 = go (depth - 1) (c:acc) cs
        | c == ')' && depth == 0 && null cs = reverse acc  -- Skip last unbalanced ')'
        | c == ')' && depth == 0 = go depth (c:acc) cs  -- Keep balanced ')' for now
        | otherwise = go depth (c:acc) cs
  in T.pack removeLastParen
