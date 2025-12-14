{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Refactor.ConflictPreview
-- Description : Preview and analyze fix conflicts before application
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides user-friendly conflict preview functionality:
--
-- * Visualize conflicting fixes with their affected regions
-- * Show what would happen with different resolution strategies
-- * Generate diff-style previews of changes
-- * Provide conflict summaries for CLI and LSP
-- * Support interactive conflict resolution
module Argus.Refactor.ConflictPreview
  ( -- * Preview Types
    ConflictPreview (..)
  , ConflictDetails (..)
  , ResolutionPreview (..)
  , ConflictSummary (..)
  , ConflictSeverity (..)

    -- * Preview Generation
  , generateConflictPreview
  , previewResolution
  , previewAllStrategies
  , getConflictSummary

    -- * Conflict Analysis
  , analyzeConflicts
  , getConflictingPairs
  , getAffectedFiles
  , getAffectedSpans

    -- * Rendering
  , renderConflictPreview
  , renderConflictDiff
  , renderResolutionOptions
  , formatConflictForTerminal
  , formatConflictForJson

    -- * Interactive Support
  , ConflictChoice (..)
  , applyConflictChoice
  , getConflictChoices

    -- * Utilities
  , conflictSeverity
  , isResolvable
  , suggestResolution
  ) where

import Data.Aeson ((.=), object, encode)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Types
import Argus.Refactor.FixGraph

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Complete preview of all conflicts for a set of fixes
data ConflictPreview = ConflictPreview
  { cpFixes            :: [Fix]              -- ^ All proposed fixes
  , cpConflicts        :: [ConflictDetails]  -- ^ Detailed conflict information
  , cpAffectedFiles    :: Set FilePath       -- ^ Files with conflicts
  , cpTotalConflicts   :: Int                -- ^ Total number of conflicts
  , cpResolvable       :: Int                -- ^ Number automatically resolvable
  , cpManualRequired   :: Int                -- ^ Number requiring manual resolution
  , cpSuggested        :: ConflictStrategy   -- ^ Suggested resolution strategy
  , cpResolutions      :: Map ConflictStrategy ResolutionPreview  -- ^ Preview each strategy
  }
  deriving stock (Eq, Show)

-- | Detailed information about a single conflict
data ConflictDetails = ConflictDetails
  { cdFix1        :: Fix              -- ^ First conflicting fix
  , cdFix2        :: Fix              -- ^ Second conflicting fix
  , cdFix1Index   :: Int              -- ^ Index of first fix
  , cdFix2Index   :: Int              -- ^ Index of second fix
  , cdType        :: ConflictType     -- ^ Type of conflict
  , cdDescription :: Text             -- ^ Human-readable description
  , cdFile        :: FilePath         -- ^ File where conflict occurs
  , cdSpan1       :: SrcSpan          -- ^ Span affected by fix1
  , cdSpan2       :: SrcSpan          -- ^ Span affected by fix2
  , cdSeverity    :: ConflictSeverity -- ^ How serious is this conflict
  , cdSuggestion  :: Text             -- ^ Suggested resolution
  }
  deriving stock (Eq, Show)

-- | Severity of a conflict
data ConflictSeverity
  = CSLow       -- ^ Minor overlap, likely auto-resolvable
  | CSMedium    -- ^ Significant conflict, but can be resolved
  | CSHigh      -- ^ Major conflict, manual intervention likely needed
  | CSCritical  -- ^ Cannot be automatically resolved
  deriving stock (Eq, Show, Ord, Enum, Bounded)

-- | Preview of what happens with a particular resolution strategy
data ResolutionPreview = ResolutionPreview
  { rpStrategy       :: ConflictStrategy   -- ^ The strategy used
  , rpAppliedFixes   :: [Int]              -- ^ Indices of fixes that would be applied
  , rpSkippedFixes   :: [Int]              -- ^ Indices of fixes that would be skipped
  , rpAppliedCount   :: Int                -- ^ Count of applied fixes
  , rpSkippedCount   :: Int                -- ^ Count of skipped fixes
  , rpRemainingConflicts :: Int            -- ^ Conflicts remaining (should be 0)
  }
  deriving stock (Eq, Show)

-- | Summary statistics for conflicts
data ConflictSummary = ConflictSummary
  { csTotal          :: Int
  , csByType         :: Map ConflictType Int
  , csBySeverity     :: Map ConflictSeverity Int
  , csByFile         :: Map FilePath Int
  , csAutoResolvable :: Int
  , csManualRequired :: Int
  }
  deriving stock (Eq, Show)

-- | User's choice for resolving a conflict
data ConflictChoice
  = ChooseFix1           -- ^ Use the first fix
  | ChooseFix2           -- ^ Use the second fix
  | ChooseSkipBoth       -- ^ Skip both fixes
  | ChooseMerge          -- ^ Try to merge (if possible)
  | ChooseDefer          -- ^ Defer to later
  deriving stock (Eq, Show, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Preview Generation
--------------------------------------------------------------------------------

-- | Generate a complete conflict preview for a list of fixes
generateConflictPreview :: [Fix] -> ConflictPreview
generateConflictPreview fixes =
  let graph = buildFixGraph fixes
      rawConflicts = fgConflicts graph
      details = map (enrichConflict fixes) rawConflicts
      affectedFiles = Set.fromList $ map cdFile details
      resolvable = length $ filter (isResolvableConflict . cdType) details
      manual = length details - resolvable
      suggested = suggestStrategy details
      resolutions = Map.fromList
        [ (s, previewResolution s graph)
        | s <- [SkipAll, SkipSecond, PreferPreferred, PreferSmaller]
        ]
  in ConflictPreview
    { cpFixes = fixes
    , cpConflicts = details
    , cpAffectedFiles = affectedFiles
    , cpTotalConflicts = length details
    , cpResolvable = resolvable
    , cpManualRequired = manual
    , cpSuggested = suggested
    , cpResolutions = resolutions
    }

-- | Enrich a raw conflict with detailed information
enrichConflict :: [Fix] -> FixConflict -> ConflictDetails
enrichConflict fixes FixConflict{..} =
  let fix1 = fixes !! unFixId fcFix1
      fix2 = fixes !! unFixId fcFix2
      (span1, span2) = getConflictSpans fix1 fix2
      file = srcSpanFile span1
      severity = conflictSeverity fcType span1 span2
      suggestion = suggestResolution fcType fix1 fix2
  in ConflictDetails
    { cdFix1 = fix1
    , cdFix2 = fix2
    , cdFix1Index = unFixId fcFix1
    , cdFix2Index = unFixId fcFix2
    , cdType = fcType
    , cdDescription = fcDesc
    , cdFile = file
    , cdSpan1 = span1
    , cdSpan2 = span2
    , cdSeverity = severity
    , cdSuggestion = suggestion
    }

-- | Get the conflicting spans from two fixes
getConflictSpans :: Fix -> Fix -> (SrcSpan, SrcSpan)
getConflictSpans fix1 fix2 =
  case (fixEdits fix1, fixEdits fix2) of
    (e1:_, e2:_) -> (fixEditSpan e1, fixEditSpan e2)
    _ -> (noSrcSpan, noSrcSpan)

-- | Preview what a resolution strategy would do
previewResolution :: ConflictStrategy -> FixGraph -> ResolutionPreview
previewResolution strategy graph =
  let (applied, skipped) = resolveConflicts strategy graph
      appliedInts = map unFixId applied
      skippedInts = map unFixId skipped
  in ResolutionPreview
    { rpStrategy = strategy
    , rpAppliedFixes = appliedInts
    , rpSkippedFixes = skippedInts
    , rpAppliedCount = length applied
    , rpSkippedCount = length skipped
    , rpRemainingConflicts = 0  -- Strategy always resolves all
    }

-- | Preview all available strategies
previewAllStrategies :: [Fix] -> Map ConflictStrategy ResolutionPreview
previewAllStrategies fixes =
  let graph = buildFixGraph fixes
      strategies = [SkipAll, SkipSecond, PreferPreferred, PreferSmaller, PreferSeverity]
  in Map.fromList [(s, previewResolution s graph) | s <- strategies]

-- | Get summary statistics for conflicts
getConflictSummary :: ConflictPreview -> ConflictSummary
getConflictSummary preview =
  let conflicts = cpConflicts preview
      byType = Map.fromListWith (+) [(cdType c, 1) | c <- conflicts]
      bySeverity = Map.fromListWith (+) [(cdSeverity c, 1) | c <- conflicts]
      byFile = Map.fromListWith (+) [(cdFile c, 1) | c <- conflicts]
  in ConflictSummary
    { csTotal = cpTotalConflicts preview
    , csByType = byType
    , csBySeverity = bySeverity
    , csByFile = byFile
    , csAutoResolvable = cpResolvable preview
    , csManualRequired = cpManualRequired preview
    }

--------------------------------------------------------------------------------
-- Conflict Analysis
--------------------------------------------------------------------------------

-- | Analyze conflicts and return detailed breakdown
analyzeConflicts :: [Fix] -> ([ConflictDetails], ConflictSummary)
analyzeConflicts fixes =
  let preview = generateConflictPreview fixes
  in (cpConflicts preview, getConflictSummary preview)

-- | Get pairs of conflicting fix indices
getConflictingPairs :: [Fix] -> [(Int, Int)]
getConflictingPairs fixes =
  let preview = generateConflictPreview fixes
  in [(cdFix1Index c, cdFix2Index c) | c <- cpConflicts preview]

-- | Get all files affected by conflicts
getAffectedFiles :: [Fix] -> Set FilePath
getAffectedFiles fixes = cpAffectedFiles (generateConflictPreview fixes)

-- | Get all spans affected by conflicts
getAffectedSpans :: [Fix] -> [(SrcSpan, SrcSpan)]
getAffectedSpans fixes =
  let preview = generateConflictPreview fixes
  in [(cdSpan1 c, cdSpan2 c) | c <- cpConflicts preview]

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- | Render a conflict preview as human-readable text
renderConflictPreview :: ConflictPreview -> Text
renderConflictPreview preview
  | cpTotalConflicts preview == 0 = "No conflicts detected."
  | otherwise = T.unlines
      [ "=== Fix Conflict Summary ==="
      , ""
      , "Total conflicts: " <> T.pack (show $ cpTotalConflicts preview)
      , "Auto-resolvable: " <> T.pack (show $ cpResolvable preview)
      , "Manual required: " <> T.pack (show $ cpManualRequired preview)
      , "Affected files:  " <> T.pack (show $ Set.size $ cpAffectedFiles preview)
      , ""
      , "Suggested strategy: " <> showStrategy (cpSuggested preview)
      , ""
      , "=== Conflict Details ==="
      , ""
      , T.intercalate "\n---\n" $ map renderConflictDetail (cpConflicts preview)
      ]

-- | Render a single conflict detail
renderConflictDetail :: ConflictDetails -> Text
renderConflictDetail cd = T.unlines
  [ "Conflict in " <> T.pack (cdFile cd) <> ":"
  , "  Type: " <> showConflictType (cdType cd)
  , "  Severity: " <> showSeverity (cdSeverity cd)
  , ""
  , "  Fix 1 (#" <> T.pack (show $ cdFix1Index cd) <> "): " <> fixTitle (cdFix1 cd)
  , "    Location: " <> showSpan' (cdSpan1 cd)
  , ""
  , "  Fix 2 (#" <> T.pack (show $ cdFix2Index cd) <> "): " <> fixTitle (cdFix2 cd)
  , "    Location: " <> showSpan' (cdSpan2 cd)
  , ""
  , "  Suggestion: " <> cdSuggestion cd
  ]

-- | Render a diff-style preview of a conflict
renderConflictDiff :: ConflictDetails -> Text -> Text
renderConflictDiff cd content =
  let lns = T.lines content
      startLine1 = unLine (srcSpanStartLine $ cdSpan1 cd)
      endLine1 = unLine (srcSpanEndLine $ cdSpan1 cd)
      startLine2 = unLine (srcSpanStartLine $ cdSpan2 cd)
      endLine2 = unLine (srcSpanEndLine $ cdSpan2 cd)

      contextBefore = max 1 (min startLine1 startLine2 - 3)
      contextAfter = min (length lns) (max endLine1 endLine2 + 3)

      relevantLines = take (contextAfter - contextBefore + 1) $ drop (contextBefore - 1) lns

      annotate lineNum line
        | lineNum >= startLine1 && lineNum <= endLine1 = "- " <> line
        | lineNum >= startLine2 && lineNum <= endLine2 = "+ " <> line
        | otherwise = "  " <> line

      annotated = zipWith annotate [contextBefore..] relevantLines
  in T.unlines
    [ "--- " <> T.pack (cdFile cd) <> " (Fix 1: " <> fixTitle (cdFix1 cd) <> ")"
    , "+++ " <> T.pack (cdFile cd) <> " (Fix 2: " <> fixTitle (cdFix2 cd) <> ")"
    , "@@ -" <> T.pack (show contextBefore) <> "," <> T.pack (show $ length relevantLines) <> " @@"
    ] <> T.unlines annotated

-- | Render available resolution options
renderResolutionOptions :: ConflictPreview -> Text
renderResolutionOptions preview = T.unlines $
  [ "=== Resolution Options ==="
  , ""
  ] ++ map renderOption (Map.toList $ cpResolutions preview)
  where
    renderOption (strategy, rp) = T.unlines
      [ "Strategy: " <> showStrategy strategy
      , "  Would apply: " <> T.pack (show $ rpAppliedCount rp) <> " fixes"
      , "  Would skip:  " <> T.pack (show $ rpSkippedCount rp) <> " fixes"
      , ""
      ]

-- | Format conflict for terminal display (with ANSI colors)
formatConflictForTerminal :: ConflictDetails -> Text
formatConflictForTerminal cd = T.unlines
  [ "\x1b[31m\x1b[1mConflict\x1b[0m in \x1b[36m" <> T.pack (cdFile cd) <> "\x1b[0m"
  , ""
  , "  \x1b[33mType:\x1b[0m " <> showConflictType (cdType cd)
  , "  \x1b[33mSeverity:\x1b[0m " <> colorSeverity (cdSeverity cd)
  , ""
  , "  \x1b[32mFix 1:\x1b[0m " <> fixTitle (cdFix1 cd)
  , "        " <> showSpan' (cdSpan1 cd)
  , ""
  , "  \x1b[32mFix 2:\x1b[0m " <> fixTitle (cdFix2 cd)
  , "        " <> showSpan' (cdSpan2 cd)
  , ""
  , "  \x1b[34mSuggestion:\x1b[0m " <> cdSuggestion cd
  ]
  where
    colorSeverity CSLow = "\x1b[32mLow\x1b[0m"
    colorSeverity CSMedium = "\x1b[33mMedium\x1b[0m"
    colorSeverity CSHigh = "\x1b[31mHigh\x1b[0m"
    colorSeverity CSCritical = "\x1b[31m\x1b[1mCritical\x1b[0m"

-- | Format conflict as JSON
formatConflictForJson :: ConflictPreview -> BL.ByteString
formatConflictForJson preview = encode $ object
  [ "totalConflicts" .= cpTotalConflicts preview
  , "resolvable" .= cpResolvable preview
  , "manualRequired" .= cpManualRequired preview
  , "affectedFiles" .= Set.toList (cpAffectedFiles preview)
  , "suggestedStrategy" .= showStrategy (cpSuggested preview)
  , "conflicts" .= map conflictToJson (cpConflicts preview)
  ]
  where
    conflictToJson cd = object
      [ "type" .= showConflictType (cdType cd)
      , "severity" .= showSeverity (cdSeverity cd)
      , "file" .= cdFile cd
      , "fix1" .= object
          [ "index" .= cdFix1Index cd
          , "title" .= fixTitle (cdFix1 cd)
          , "line" .= unLine (srcSpanStartLine $ cdSpan1 cd)
          ]
      , "fix2" .= object
          [ "index" .= cdFix2Index cd
          , "title" .= fixTitle (cdFix2 cd)
          , "line" .= unLine (srcSpanStartLine $ cdSpan2 cd)
          ]
      , "suggestion" .= cdSuggestion cd
      ]

--------------------------------------------------------------------------------
-- Interactive Support
--------------------------------------------------------------------------------

-- | Apply user's choice to resolve a conflict
applyConflictChoice :: ConflictChoice -> ConflictDetails -> (Maybe Int, Maybe Int)
-- ^ (fix to apply, fix to skip)
applyConflictChoice choice cd = case choice of
  ChooseFix1 -> (Just $ cdFix1Index cd, Just $ cdFix2Index cd)
  ChooseFix2 -> (Just $ cdFix2Index cd, Just $ cdFix1Index cd)
  ChooseSkipBoth -> (Nothing, Nothing)  -- Both skipped
  ChooseMerge -> (Just $ cdFix1Index cd, Just $ cdFix2Index cd)  -- Try both
  ChooseDefer -> (Nothing, Nothing)

-- | Get available choices for a conflict
getConflictChoices :: ConflictDetails -> [(ConflictChoice, Text)]
getConflictChoices cd =
  [ (ChooseFix1, "Apply: " <> fixTitle (cdFix1 cd))
  , (ChooseFix2, "Apply: " <> fixTitle (cdFix2 cd))
  , (ChooseSkipBoth, "Skip both fixes")
  , (ChooseDefer, "Decide later")
  ] ++ mergeable
  where
    mergeable
      | cdSeverity cd <= CSMedium = [(ChooseMerge, "Try to merge both")]
      | otherwise = []

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Calculate conflict severity based on type and span relationship
conflictSeverity :: ConflictType -> SrcSpan -> SrcSpan -> ConflictSeverity
conflictSeverity ctype span1 span2 = case ctype of
  OverlapConflict ->
    if spanContains span1 span2 || spanContains span2 span1
    then CSHigh  -- One completely contains the other
    else if spanAdjacent span1 span2
    then CSLow   -- Adjacent spans, likely mergeable
    else CSMedium
  IdentifierConflict -> CSHigh  -- Renaming conflicts are serious
  ScopeConflict -> CSCritical
  SemanticConflict -> CSCritical

-- | Check if a conflict can be auto-resolved
isResolvable :: ConflictDetails -> Bool
isResolvable cd = cdSeverity cd < CSHigh

-- | Check if a conflict type is auto-resolvable
isResolvableConflict :: ConflictType -> Bool
isResolvableConflict OverlapConflict = True
isResolvableConflict IdentifierConflict = False
isResolvableConflict ScopeConflict = False
isResolvableConflict SemanticConflict = False

-- | Suggest a resolution for a conflict
suggestResolution :: ConflictType -> Fix -> Fix -> Text
suggestResolution ctype fix1 fix2 = case ctype of
  OverlapConflict ->
    if fixIsPreferred fix1 && not (fixIsPreferred fix2)
    then "Apply '" <> fixTitle fix1 <> "' (preferred)"
    else if fixIsPreferred fix2 && not (fixIsPreferred fix1)
    then "Apply '" <> fixTitle fix2 <> "' (preferred)"
    else if length (fixEdits fix1) < length (fixEdits fix2)
    then "Apply '" <> fixTitle fix1 <> "' (smaller, more targeted)"
    else "Apply '" <> fixTitle fix2 <> "' (smaller, more targeted)"
  IdentifierConflict ->
    "Manual review required - conflicting identifier renames"
  ScopeConflict ->
    "Manual review required - scope changes conflict"
  SemanticConflict ->
    "Manual review required - semantic changes conflict"

-- | Suggest the best resolution strategy based on conflicts
suggestStrategy :: [ConflictDetails] -> ConflictStrategy
suggestStrategy conflicts
  | null conflicts = SkipAll
  | all ((== CSLow) . cdSeverity) conflicts = Merge
  | any hasPreferred conflicts = PreferPreferred
  | otherwise = PreferSmaller
  where
    hasPreferred cd = fixIsPreferred (cdFix1 cd) /= fixIsPreferred (cdFix2 cd)

-- | Show strategy as text
showStrategy :: ConflictStrategy -> Text
showStrategy SkipAll = "Skip All Conflicting"
showStrategy SkipSecond = "Keep First, Skip Second"
showStrategy PreferPreferred = "Prefer Marked Preferred"
showStrategy PreferSeverity = "Prefer Higher Severity"
showStrategy PreferSmaller = "Prefer Smaller Fixes"
showStrategy Interactive = "Interactive Selection"
showStrategy Merge = "Attempt Merge"

-- | Show conflict type as text
showConflictType :: ConflictType -> Text
showConflictType OverlapConflict = "Overlapping edits"
showConflictType IdentifierConflict = "Identifier conflict"
showConflictType ScopeConflict = "Scope conflict"
showConflictType SemanticConflict = "Semantic conflict"

-- | Show severity as text
showSeverity :: ConflictSeverity -> Text
showSeverity CSLow = "Low"
showSeverity CSMedium = "Medium"
showSeverity CSHigh = "High"
showSeverity CSCritical = "Critical"

-- | Show span as text
showSpan' :: SrcSpan -> Text
showSpan' s = T.pack $ srcSpanFile s
  <> ":" <> show (srcSpanStartLine s)
  <> ":" <> show (srcSpanStartCol s)
  <> "-" <> show (srcSpanEndLine s)
  <> ":" <> show (srcSpanEndCol s)
