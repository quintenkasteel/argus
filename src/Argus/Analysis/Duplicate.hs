{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Analysis.Duplicate
-- Description : Code clone and duplicate detection for Haskell
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides comprehensive code clone detection capabilities,
-- identifying exact clones (Type 1), renamed clones (Type 2), and
-- structural clones (Type 3) in Haskell codebases.
module Argus.Analysis.Duplicate
  ( -- * Types
    Clone (..)
  , CloneType (..)
  , CloneLocation (..)
  , DuplicateConfig (..)
  , DuplicateReport (..)
  , DuplicateMetrics (..)
  , RefactorSuggestion (..)
  , RefactorEffort (..)

    -- * Configuration
  , defaultDuplicateConfig

    -- * Analysis
  , analyzeDuplicates
  , detectClones
  , findDuplicateFunctions

    -- * Diagnostics
  , duplicateDiagnostics
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Map.Strict qualified as Map
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Word (Word64)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import Argus.Types
import Argus.Analysis.Fingerprint
import Argus.Analysis.Similarity

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

-- | Type of code clone detected
data CloneType
  = ExactClone       -- ^ Type 1: Identical except whitespace/comments
  | RenamedClone     -- ^ Type 2: Identical after alpha-renaming variables
  | StructuralClone  -- ^ Type 3: Same structure, different values/literals
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A detected code clone
data Clone = Clone
  { cloneType       :: CloneType        -- ^ Classification of the clone
  , cloneLocations  :: [CloneLocation]  -- ^ All locations of this clone
  , cloneSimilarity :: Double           -- ^ Similarity score (0.0 to 1.0)
  , cloneLineCount  :: Int              -- ^ Lines of duplicated code
  , cloneTokenCount :: Int              -- ^ Approximate token count
  , cloneHash       :: Word64           -- ^ Structural hash for deduplication
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Location of a clone instance
data CloneLocation = CloneLocation
  { clFile     :: FilePath    -- ^ Source file path
  , clFunction :: Text        -- ^ Function name
  , clSpan     :: SrcSpan     -- ^ Exact source span
  , clCode     :: Text        -- ^ Original source code
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Configuration for duplicate detection
data DuplicateConfig = DuplicateConfig
  { dcMinLineCount        :: Int           -- ^ Minimum lines to consider (default: 5)
  , dcMinTokenCount       :: Int           -- ^ Minimum tokens (default: 20)
  , dcSimilarityThreshold :: Double        -- ^ Similarity threshold (default: 0.8)
  , dcDetectTypes         :: [CloneType]   -- ^ Which clone types to detect
  , dcIgnorePatterns      :: [Text]        -- ^ Function patterns to ignore (regex)
  , dcCrossModule         :: Bool          -- ^ Detect cross-module clones
  , dcEnabled             :: Bool          -- ^ Enable duplicate detection
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration for duplicate detection
defaultDuplicateConfig :: DuplicateConfig
defaultDuplicateConfig = DuplicateConfig
  { dcMinLineCount = 5
  , dcMinTokenCount = 20
  , dcSimilarityThreshold = 0.8
  , dcDetectTypes = [ExactClone, RenamedClone, StructuralClone]
  , dcIgnorePatterns = []
  , dcCrossModule = True
  , dcEnabled = True
  }

-- | Report of duplicate detection results
data DuplicateReport = DuplicateReport
  { drClones             :: [Clone]              -- ^ All detected clones
  , drMetrics            :: DuplicateMetrics     -- ^ Summary metrics
  , drHotspots           :: [(FilePath, Int)]    -- ^ Files with most duplication
  , drSuggestedRefactors :: [RefactorSuggestion] -- ^ Suggested refactoring
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Metrics about code duplication
data DuplicateMetrics = DuplicateMetrics
  { dmTotalFunctions      :: Int      -- ^ Total functions analyzed
  , dmDuplicateFunctions  :: Int      -- ^ Functions involved in duplication
  , dmDuplicationRatio    :: Double   -- ^ Percentage of code duplicated
  , dmCloneCount          :: Int      -- ^ Number of clone groups
  , dmLargestClone        :: Int      -- ^ Lines in largest clone
  , dmTotalDuplicateLines :: Int      -- ^ Total lines of duplicated code
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Suggested refactoring to reduce duplication
data RefactorSuggestion = RefactorSuggestion
  { rsDescription :: Text           -- ^ Description of the refactoring
  , rsLocations   :: [CloneLocation] -- ^ Affected locations
  , rsEffort      :: RefactorEffort -- ^ Estimated effort
  , rsBenefit     :: Int            -- ^ Lines that would be deduplicated
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Effort level for refactoring
data RefactorEffort
  = LowEffort      -- ^ Simple extraction
  | MediumEffort   -- ^ Needs parameterization
  | HighEffort     -- ^ Complex refactoring
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Main Analysis Functions
--------------------------------------------------------------------------------

-- | Analyze a codebase for duplicates
analyzeDuplicates :: DuplicateConfig -> [(FilePath, Text)] -> DuplicateReport
analyzeDuplicates config files
  | not (dcEnabled config) = emptyReport
  | otherwise =
      let -- Extract and fingerprint all functions
          allFunctions = concatMap (extractFunctionsFromFile config) files

          -- Detect clones
          clones = detectClones config allFunctions

          -- Calculate metrics
          metrics = calculateMetrics allFunctions clones

          -- Find hotspots
          hotspots = findHotspots clones

          -- Generate refactoring suggestions
          suggestions = generateSuggestions clones

      in DuplicateReport
        { drClones = clones
        , drMetrics = metrics
        , drHotspots = hotspots
        , drSuggestedRefactors = suggestions
        }

-- | Empty report for when detection is disabled
emptyReport :: DuplicateReport
emptyReport = DuplicateReport
  { drClones = []
  , drMetrics = DuplicateMetrics 0 0 0.0 0 0 0
  , drHotspots = []
  , drSuggestedRefactors = []
  }

-- | Extract and fingerprint functions from a file
extractFunctionsFromFile :: DuplicateConfig -> (FilePath, Text) -> [FunctionFingerprint]
extractFunctionsFromFile DuplicateConfig{..} (path, content) =
  let functions = extractFunctionBodies content
      fingerprints = map (createFingerprint path) functions
      -- Filter by size
      filtered = filter (\fp ->
        fpLineCount fp >= dcMinLineCount &&
        fpNodeCount fp >= 3 &&  -- Minimum meaningful size
        not (shouldIgnore dcIgnorePatterns (fpName fp))
        ) fingerprints
  in filtered

-- | Extract function bodies from source code
extractFunctionBodies :: Text -> [(Text, Text, Int, Int)]  -- (name, body, startLine, endLine)
extractFunctionBodies content =
  let ls = zip [1..] (T.lines content)
      -- Find function starts (simplified heuristic)
      starts = findFunctionStarts ls
  in groupFunctions starts ls

-- | Find lines that start function definitions
findFunctionStarts :: [(Int, Text)] -> [(Int, Text)]
findFunctionStarts = filter isFunctionStart
  where
    isFunctionStart (_, line) =
      let stripped = T.stripStart line
      in case T.uncons stripped of
           Just (c, _) -> isLower c &&
                          not (startsWithKeyword stripped) &&
                          (" = " `T.isInfixOf` stripped || " ::" `T.isInfixOf` stripped)
           Nothing -> False

    isLower c = c >= 'a' && c <= 'z'

    startsWithKeyword t = any (`T.isPrefixOf` t)
      ["import ", "module ", "where", "let ", "in ", "data ", "type ",
       "newtype ", "class ", "instance ", "deriving "]

-- | Group function bodies
groupFunctions :: [(Int, Text)] -> [(Int, Text)] -> [(Text, Text, Int, Int)]
groupFunctions starts allLines =
  let startLines = map fst starts
      -- For each start, find the end (next function start or end of file)
      ends = drop 1 startLines ++ [length allLines + 1]
  in zipWith (extractFunction allLines) starts ends

extractFunction :: [(Int, Text)] -> (Int, Text) -> Int -> (Text, Text, Int, Int)
extractFunction allLines (startLine, firstLine) endLine =
  let name = extractFunctionName firstLine
      bodyLines = filter (\(ln, _) -> ln >= startLine && ln < endLine) allLines
      body = T.unlines $ map snd bodyLines
  in (name, body, startLine, endLine - 1)

extractFunctionName :: Text -> Text
extractFunctionName line =
  let stripped = T.stripStart line
  in T.takeWhile (\c -> c /= ' ' && c /= ':') stripped

-- | Create fingerprint for a function
createFingerprint :: FilePath -> (Text, Text, Int, Int) -> FunctionFingerprint
createFingerprint path (name, body, startLine, endLine) =
  let srcSpan = mkSrcSpanRaw path startLine 1 endLine 1
  in fingerprintFunction name body srcSpan

-- | Check if function should be ignored based on patterns
shouldIgnore :: [Text] -> Text -> Bool
shouldIgnore patterns name =
  any (`T.isInfixOf` name) patterns

--------------------------------------------------------------------------------
-- Clone Detection
--------------------------------------------------------------------------------

-- | Detect clones from fingerprints
detectClones :: DuplicateConfig -> [FunctionFingerprint] -> [Clone]
detectClones DuplicateConfig{..} fingerprints =
  let -- Group by hash for exact/renamed clones
      byHash = Map.fromListWith (++) $
        map (\fp -> (fpHash fp, [fp])) fingerprints

      -- Find exact clones (same hash)
      exactClones = if ExactClone `elem` dcDetectTypes
        then mapMaybe (createCloneGroup ExactClone 1.0) $
             filter ((>= 2) . length) $ Map.elems byHash
        else []

      -- For structural clones, use similarity-based clustering
      structuralClones = if StructuralClone `elem` dcDetectTypes
        then findStructuralClones dcSimilarityThreshold fingerprints
        else []

      -- Merge and deduplicate
      allClones = mergeClones (exactClones ++ structuralClones)

  in sortBy (comparing (Down . cloneLineCount)) allClones

-- | Find structural clones using similarity clustering
findStructuralClones :: Double -> [FunctionFingerprint] -> [Clone]
findStructuralClones threshold fps =
  let config = ClusterConfig
        { ccSimilarityThreshold = threshold
        , ccMinNodes = 5
        , ccMaxClusters = 50
        }
      clusters = clusterBySimilarity config fps
      -- Convert clusters to clones
  in mapMaybe (clusterToClone threshold) clusters

-- | Convert a cluster to a Clone
clusterToClone :: Double -> [FunctionFingerprint] -> Maybe Clone
clusterToClone _threshold fps
  | length fps < 2 = Nothing
  | otherwise =
      let locations = map fpToLocation fps
          avgSimilarity = computeAvgSimilarity fps
          lineCount = maximum $ map fpLineCount fps
          tokenCount = sum $ map fpNodeCount fps
          hash = maybe 0 fpHash (listToMaybe fps)
          cloneType = if avgSimilarity >= 0.99
                      then RenamedClone
                      else StructuralClone
      in Just Clone
        { cloneType = cloneType
        , cloneLocations = locations
        , cloneSimilarity = avgSimilarity
        , cloneLineCount = lineCount
        , cloneTokenCount = tokenCount
        , cloneHash = hash
        }

-- | Convert fingerprint to clone location
fpToLocation :: FunctionFingerprint -> CloneLocation
fpToLocation FunctionFingerprint{..} = CloneLocation
  { clFile = srcSpanFile fpSpan
  , clFunction = fpName
  , clSpan = fpSpan
  , clCode = fpOriginalCode
  }

-- | Compute average similarity within a cluster
computeAvgSimilarity :: [FunctionFingerprint] -> Double
computeAvgSimilarity fps
  | length fps < 2 = 1.0
  | otherwise =
      let pairs = [(fp1, fp2) | fp1 <- fps, fp2 <- fps, fpName fp1 < fpName fp2]
          similarities = map (\(fp1, fp2) ->
            simScore $ computeSimilarity (fpNormalized fp1) (fpNormalized fp2)) pairs
      in if null similarities then 1.0 else sum similarities / fromIntegral (length similarities)

-- | Create a clone group from fingerprints
createCloneGroup :: CloneType -> Double -> [FunctionFingerprint] -> Maybe Clone
createCloneGroup cloneType similarity fps
  | length fps < 2 = Nothing
  | otherwise = Just Clone
    { cloneType = cloneType
    , cloneLocations = map fpToLocation fps
    , cloneSimilarity = similarity
    , cloneLineCount = maximum $ map fpLineCount fps
    , cloneTokenCount = sum $ map fpNodeCount fps
    , cloneHash = maybe 0 fpHash (listToMaybe fps)
    }

-- | Merge overlapping clones by grouping those with overlapping locations
-- and selecting the best representative from each group
mergeClones :: [Clone] -> [Clone]
mergeClones [] = []
mergeClones clones =
  let -- Group clones by whether they share any overlapping locations
      groups = groupByOverlap clones
      -- From each group, select the best representative
  in map selectBestClone groups

-- | Group clones that have overlapping locations
groupByOverlap :: [Clone] -> [[Clone]]
groupByOverlap = foldr insertIntoGroup []
  where
    insertIntoGroup :: Clone -> [[Clone]] -> [[Clone]]
    insertIntoGroup clone groups =
      case findOverlappingGroup clone groups of
        Nothing -> [clone] : groups
        Just (overlapping, rest) -> (clone : overlapping) : rest

    findOverlappingGroup :: Clone -> [[Clone]] -> Maybe ([Clone], [[Clone]])
    findOverlappingGroup _ [] = Nothing
    findOverlappingGroup clone (g:gs)
      | any (clonesOverlap clone) g = Just (g, gs)
      | otherwise = case findOverlappingGroup clone gs of
          Nothing -> Nothing
          Just (found, rest) -> Just (found, g : rest)

-- | Check if two clones have overlapping locations
clonesOverlap :: Clone -> Clone -> Bool
clonesOverlap c1 c2 =
  any (\loc1 -> any (locationsOverlap loc1) (cloneLocations c2)) (cloneLocations c1)

-- | Check if two locations overlap
locationsOverlap :: CloneLocation -> CloneLocation -> Bool
locationsOverlap loc1 loc2 =
  clFile loc1 == clFile loc2 && spansOverlap (clSpan loc1) (clSpan loc2)

-- | Check if two source spans overlap
spansOverlap :: SrcSpan -> SrcSpan -> Bool
spansOverlap s1 s2 =
  srcSpanFile s1 == srcSpanFile s2 &&
  not (srcSpanEndLine s1 < srcSpanStartLine s2 ||
       srcSpanEndLine s2 < srcSpanStartLine s1 ||
       (srcSpanEndLine s1 == srcSpanStartLine s2 &&
        srcSpanEndCol s1 < srcSpanStartCol s2) ||
       (srcSpanEndLine s2 == srcSpanStartLine s1 &&
        srcSpanEndCol s2 < srcSpanStartCol s1))

-- | Select the best clone from a group of overlapping clones
-- Prefers: higher similarity, more locations, exact clones over structural
selectBestClone :: [Clone] -> Clone
selectBestClone [] = Clone ExactClone [] 0 0 0 0  -- Empty case returns default
selectBestClone [c] = c
selectBestClone clones =
  let scored = map (\c -> (cloneScore c, c)) clones
      sorted = sortBy (comparing (Down . fst)) scored
  in case sorted of
       ((_, best):_) -> best
       [] -> Clone ExactClone [] 0 0 0 0  -- Defensive, should not happen

-- | Score a clone for ranking (higher is better)
cloneScore :: Clone -> (Double, Int, Int)
cloneScore Clone{..} =
  ( cloneSimilarity
  , cloneTypeScore cloneType
  , length cloneLocations
  )

-- | Score for clone type (exact > renamed > structural)
cloneTypeScore :: CloneType -> Int
cloneTypeScore ExactClone = 3
cloneTypeScore RenamedClone = 2
cloneTypeScore StructuralClone = 1

--------------------------------------------------------------------------------
-- Find Duplicate Functions (Simplified API)
--------------------------------------------------------------------------------

-- | Find duplicate functions in code (simplified API)
findDuplicateFunctions :: Text -> [(Text, Text, Double)]
findDuplicateFunctions content =
  let functions = extractFunctionBodies content
      fingerprints = map (createFingerprint "<input>") functions

      -- Find pairs with high similarity
      pairs = [ (fpName fp1, fpName fp2, sim)
              | fp1 <- fingerprints
              , fp2 <- fingerprints
              , fpName fp1 < fpName fp2
              , let sim = simScore $ computeSimilarity (fpNormalized fp1) (fpNormalized fp2)
              , sim >= 0.7
              ]
  in pairs

--------------------------------------------------------------------------------
-- Metrics and Reporting
--------------------------------------------------------------------------------

-- | Calculate duplication metrics
calculateMetrics :: [FunctionFingerprint] -> [Clone] -> DuplicateMetrics
calculateMetrics fps clones =
  let totalFunctions = length fps
      duplicatedFunctions = length $ Set.fromList $
        concatMap (map clFunction . cloneLocations) clones
      totalLines = sum $ map fpLineCount fps
      duplicateLines = sum $ map cloneLineCount clones
      duplicationRatio = if totalLines == 0
        then 0.0
        else fromIntegral duplicateLines / fromIntegral totalLines
      largestClone = if null clones then 0 else maximum $ map cloneLineCount clones
  in DuplicateMetrics
    { dmTotalFunctions = totalFunctions
    , dmDuplicateFunctions = duplicatedFunctions
    , dmDuplicationRatio = duplicationRatio
    , dmCloneCount = length clones
    , dmLargestClone = largestClone
    , dmTotalDuplicateLines = duplicateLines
    }

-- | Find files with most duplication
findHotspots :: [Clone] -> [(FilePath, Int)]
findHotspots clones =
  let fileLines = concatMap (\c ->
        map (\loc -> (clFile loc, cloneLineCount c)) (cloneLocations c)) clones
      grouped = Map.fromListWith (+) fileLines
  in sortBy (comparing (Down . snd)) $ Map.toList grouped

-- | Generate refactoring suggestions
generateSuggestions :: [Clone] -> [RefactorSuggestion]
generateSuggestions clones =
  let -- Focus on largest clones
      significant = take 10 $ sortBy (comparing (Down . cloneLineCount)) clones
  in map cloneToSuggestion significant

cloneToSuggestion :: Clone -> RefactorSuggestion
cloneToSuggestion Clone{..} =
  let effort = case cloneType of
        ExactClone -> LowEffort
        RenamedClone -> LowEffort
        StructuralClone -> MediumEffort
      description = case cloneType of
        ExactClone ->
          "Extract common function: These " <> T.pack (show $ length cloneLocations) <>
          " functions are identical and can be merged"
        RenamedClone ->
          "Extract parameterized function: These functions differ only in variable names"
        StructuralClone ->
          "Consider abstracting: These functions have similar structure (" <>
          T.pack (show (round (cloneSimilarity * 100) :: Int)) <> "% similar)"
  in RefactorSuggestion
    { rsDescription = description
    , rsLocations = cloneLocations
    , rsEffort = effort
    , rsBenefit = cloneLineCount * (length cloneLocations - 1)
    }

--------------------------------------------------------------------------------
-- Diagnostics
--------------------------------------------------------------------------------

-- | Generate diagnostics for detected clones
duplicateDiagnostics :: DuplicateConfig -> DuplicateReport -> [Diagnostic]
duplicateDiagnostics _config report =
  concatMap cloneToDiagnostics (drClones report)

-- | Convert a clone to diagnostics
cloneToDiagnostics :: Clone -> [Diagnostic]
cloneToDiagnostics Clone{..} =
  let severity = case cloneType of
        ExactClone -> Warning
        RenamedClone -> Warning
        StructuralClone -> Suggestion

      code = case cloneType of
        ExactClone -> "duplicate/exact-clone"
        RenamedClone -> "duplicate/renamed-clone"
        StructuralClone -> "duplicate/structural-clone"

      otherLocations = case cloneLocations of
        [] -> []
        [_] -> []
        (_first:rest) -> rest

      message loc =
        "Duplicated code (" <> T.pack (show (round (cloneSimilarity * 100) :: Int)) <>
        "% similar to " <> clFunction loc <> " in " <> T.pack (clFile loc) <> ")"

  in [ Diagnostic
       { diagSpan = clSpan loc
       , diagSeverity = severity
       , diagKind = Custom "duplicate"
       , diagMessage = message otherLoc
       , diagCode = Just code
       , diagFixes = []
       , diagRelated = map (\other -> (clSpan other,
           "Also appears in " <> clFunction other)) otherLocations
       }
     | loc <- cloneLocations
     , otherLoc <- otherLocations
     , clFunction loc /= clFunction otherLoc
     ]
