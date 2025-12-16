{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Argus.Refactor.SpanAdjustment
-- Description : Span adjustment for sequential fix application
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
-- Portability : GHC
--
-- = Overview
--
-- This module provides span adjustment functionality for applying
-- multiple fixes to the same file sequentially. When a fix is applied,
-- it may change the positions of subsequent code, making their original
-- spans invalid. This module computes "deltas" that describe how positions
-- shift, and can adjust spans accordingly.
--
-- = The Problem
--
-- When applying multiple fixes to the same file:
--
-- 1. Fix A replaces text at lines 5-6, removing 1 line
-- 2. Fix B targets line 10, which is now actually line 9
-- 3. Without adjustment, Fix B would edit the wrong location
--
-- = The Solution
--
-- After each fix is applied, compute a @SpanDelta@ that describes:
--
-- * Where the edit occurred
-- * How many lines were added\/removed
-- * How columns shifted on affected lines
--
-- Then adjust all remaining fix spans using these deltas.
--
-- = Architecture
--
-- @
-- ┌─────────────────────────────────────────────────────────────────────┐
-- │                      Span Adjustment Pipeline                       │
-- │                                                                     │
-- │  Apply Fix ──► Compute Delta ──► Add to Accumulator ──► Adjust Next │
-- │       │              │                  │                    │      │
-- │       ▼              ▼                  ▼                    ▼      │
-- │   newContent    SpanDelta         [SpanDelta]          adjustedFix  │
-- └─────────────────────────────────────────────────────────────────────┘
-- @
--
-- = Thread Safety
--
-- All functions are pure and thread-safe. @DeltaAccumulator@ is mutable
-- but should be used single-threaded within a fix application session.
--
-- = Usage
--
-- @
-- -- Apply first fix
-- let newContent = applyFix content fix1
--     delta = computeDelta (getFixSpan fix1) (getOldText fix1) (getNewText fix1)
--
-- -- Adjust remaining fixes
-- let adjustedFixes = map (adjustFixSpan [delta]) remainingFixes
--
-- -- Continue with adjusted fixes
-- @
--
-- @since 1.0.0
module Argus.Refactor.SpanAdjustment
  ( -- * Delta Types
    SpanDelta (..)
  , DeltaAccumulator

    -- * Computing Deltas
  , computeDelta
  , computeEditDelta
  , computeFixDelta

    -- * Applying Deltas
  , adjustSpan
  , adjustSpanWithDeltas
  , adjustFix
  , adjustFixes
  , adjustFixEdit

    -- * Delta Accumulation
  , emptyAccumulator
  , addDelta
  , getDeltas

    -- * Utilities
  , spanAffectedByDelta
  , deltaAffectsLine
  , mergeDeltas
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Types
  ( SrcSpan(..), Fix(..), FixEdit(..), Line(..), Column(..)
  , srcSpanStartLineRaw, srcSpanStartColRaw
  , srcSpanEndLineRaw, srcSpanEndColRaw
  )

--------------------------------------------------------------------------------
-- Delta Types
--------------------------------------------------------------------------------

-- | Describes how a text edit shifts subsequent positions.
--
-- When text is replaced, all positions after the edit may shift:
-- - If lines are added, subsequent line numbers increase
-- - If lines are removed, subsequent line numbers decrease
-- - If text on the same line changes length, column numbers shift
data SpanDelta = SpanDelta
  { sdFile       :: FilePath  -- ^ File this delta applies to
  , sdEditStart  :: (Int, Int) -- ^ (line, col) where edit started (1-indexed)
  , sdEditEnd    :: (Int, Int) -- ^ (line, col) where edit ended (1-indexed)
  , sdLineDelta  :: Int        -- ^ Net lines added (negative = removed)
  , sdColDelta   :: Int        -- ^ Net columns on the end line (for same-line edits)
  , sdNewEndLine :: Int        -- ^ Line where the edit ends after replacement
  , sdNewEndCol  :: Int        -- ^ Column where the edit ends after replacement
  }
  deriving stock (Eq, Show)

-- | Accumulator for multiple deltas, kept sorted by position
newtype DeltaAccumulator = DeltaAccumulator { unDeltas :: [SpanDelta] }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Computing Deltas
--------------------------------------------------------------------------------

-- | Compute the delta from replacing text at a span.
--
-- @computeDelta span oldText newText@ computes how replacing @oldText@
-- with @newText@ at @span@ affects subsequent positions.
computeDelta :: SrcSpan -> Text -> Text -> SpanDelta
computeDelta theSpan oldText newText =
  let oldLines = T.lines oldText
      newLines = T.lines newText

      oldLineCount = max 1 (length oldLines)
      newLineCount = max 1 (length newLines)

      lineDelta = newLineCount - oldLineCount

      -- For single-line edits, compute column delta
      colDelta = if oldLineCount == 1 && newLineCount == 1
                 then T.length newText - T.length oldText
                 else 0

      startLine = srcSpanStartLineRaw theSpan
      startCol = srcSpanStartColRaw theSpan
      endLine = srcSpanEndLineRaw theSpan

      -- After replacement, the edit ends at:
      newEndLine = startLine + newLineCount - 1
      newEndCol = if newLineCount == 1
                  then startCol + T.length newText
                  else case newLines of
                         [] -> 1
                         xs -> T.length (last xs) + 1

  in SpanDelta
    { sdFile = srcSpanFile theSpan
    , sdEditStart = (startLine, startCol)
    , sdEditEnd = (endLine, srcSpanEndColRaw theSpan)
    , sdLineDelta = lineDelta
    , sdColDelta = colDelta
    , sdNewEndLine = newEndLine
    , sdNewEndCol = newEndCol
    }

-- | Compute delta from a FixEdit, given the original content.
computeEditDelta :: Text -> FixEdit -> SpanDelta
computeEditDelta content (FixEdit theSpan newText) =
  let oldText = extractSpanText content theSpan
  in computeDelta theSpan oldText newText

-- | Compute deltas from all edits in a Fix.
-- Returns deltas sorted by position (for correct sequential application).
computeFixDelta :: Text -> Fix -> [SpanDelta]
computeFixDelta content fix =
  let edits = sortBy (comparing (srcSpanStartLine . fixEditSpan)) (fixEdits fix)
  in map (computeEditDelta content) edits

-- | Extract text at a source span from content.
extractSpanText :: Text -> SrcSpan -> Text
extractSpanText content theSpan =
  let contentLines = T.lines content
      startLine = srcSpanStartLineRaw theSpan
      endLine = srcSpanEndLineRaw theSpan
      startCol = srcSpanStartColRaw theSpan
      endCol = srcSpanEndColRaw theSpan

  in if startLine == endLine
     then -- Single line: extract columns
       if startLine > 0 && startLine <= length contentLines
       then let line = contentLines !! (startLine - 1)
            in T.take (endCol - startCol) $ T.drop (startCol - 1) line
       else ""
     else -- Multi-line
       let relevantLines = take (endLine - startLine + 1) $ drop (startLine - 1) contentLines
       in case relevantLines of
            [] -> ""
            [l] -> T.drop (startCol - 1) l
            (first:rest) ->
              let firstPart = T.drop (startCol - 1) first
                  lastPart = case reverse rest of
                               [] -> ""
                               (l:_) -> T.take (endCol - 1) l
                  middleParts = case rest of
                                  [_] -> []
                                  xs -> init xs
              in T.intercalate "\n" ([firstPart] ++ middleParts ++ [lastPart])

--------------------------------------------------------------------------------
-- Applying Deltas
--------------------------------------------------------------------------------

-- | Adjust a span based on a single delta.
--
-- The adjustment rules are:
-- 1. If span is entirely before delta edit: no change
-- 2. If span overlaps with delta edit: span is invalid (return Nothing)
-- 3. If span starts after delta edit ends: adjust by line/col delta
-- 4. If span is on same line as edit end: adjust column
adjustSpan :: SpanDelta -> SrcSpan -> Maybe SrcSpan
adjustSpan delta theSpan
  -- Different file: no effect
  | srcSpanFile theSpan /= sdFile delta = Just theSpan

  -- Span entirely before edit: no change
  | spanEndsBefore theSpan (sdEditStart delta) = Just theSpan

  -- Span overlaps with edit region: invalid
  | spansOverlapDelta delta theSpan = Nothing

  -- Span starts after edit: apply full adjustment
  | spanStartsAfter theSpan (sdNewEndLine delta, sdNewEndCol delta) =
      Just $ adjustSpanFull delta theSpan

  -- Span on same line as edit end: adjust column only
  | srcSpanStartLineRaw theSpan == sdNewEndLine delta =
      Just $ adjustSpanColumn delta theSpan

  -- Otherwise: no change (this shouldn't happen)
  | otherwise = Just theSpan

-- | Adjust a span with multiple deltas (applied in order).
adjustSpanWithDeltas :: [SpanDelta] -> SrcSpan -> Maybe SrcSpan
adjustSpanWithDeltas deltas theSpan = foldl' applyDelta (Just theSpan) deltas
  where
    applyDelta Nothing _ = Nothing
    applyDelta (Just s) d = adjustSpan d s

-- | Adjust all edits in a fix.
adjustFix :: [SpanDelta] -> Fix -> Maybe Fix
adjustFix deltas fix = do
  adjustedEdits <- traverse (adjustFixEdit deltas) (fixEdits fix)
  pure fix { fixEdits = adjustedEdits }

-- | Adjust multiple fixes, filtering out those that become invalid.
adjustFixes :: [SpanDelta] -> [Fix] -> [Fix]
adjustFixes deltas fixes =
  [ adjusted | fix <- fixes, Just adjusted <- [adjustFix deltas fix] ]

-- | Adjust a single fix edit.
adjustFixEdit :: [SpanDelta] -> FixEdit -> Maybe FixEdit
adjustFixEdit deltas (FixEdit theSpan newText) = do
  adjustedSpan <- adjustSpanWithDeltas deltas theSpan
  pure $ FixEdit adjustedSpan newText

--------------------------------------------------------------------------------
-- Delta Accumulation
--------------------------------------------------------------------------------

-- | Create an empty delta accumulator.
emptyAccumulator :: DeltaAccumulator
emptyAccumulator = DeltaAccumulator []

-- | Add a delta to the accumulator.
-- Deltas are kept sorted by position for correct application.
addDelta :: SpanDelta -> DeltaAccumulator -> DeltaAccumulator
addDelta delta (DeltaAccumulator deltas) =
  DeltaAccumulator $ sortBy (comparing sdEditStart) (delta : deltas)

-- | Get all deltas from the accumulator.
getDeltas :: DeltaAccumulator -> [SpanDelta]
getDeltas = unDeltas

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Check if a span is affected by a delta.
spanAffectedByDelta :: SpanDelta -> SrcSpan -> Bool
spanAffectedByDelta delta theSpan =
  srcSpanFile theSpan == sdFile delta
  && not (spanEndsBefore theSpan (sdEditStart delta))

-- | Check if a delta affects a specific line.
deltaAffectsLine :: SpanDelta -> Int -> Bool
deltaAffectsLine delta line =
  line >= fst (sdEditStart delta)

-- | Merge multiple deltas into one (if they're consecutive).
mergeDeltas :: [SpanDelta] -> Maybe SpanDelta
mergeDeltas [] = Nothing
mergeDeltas [d] = Just d
mergeDeltas (d1:d2:rest)
  | sdFile d1 == sdFile d2
  , snd (sdEditEnd d1) <= fst (sdEditStart d2) =
      mergeDeltas $ merged : rest
  | otherwise = Nothing
  where
    merged = SpanDelta
      { sdFile = sdFile d1
      , sdEditStart = sdEditStart d1
      , sdEditEnd = sdEditEnd d2
      , sdLineDelta = sdLineDelta d1 + sdLineDelta d2
      , sdColDelta = sdColDelta d2  -- Use the last edit's column delta
      , sdNewEndLine = sdNewEndLine d2
      , sdNewEndCol = sdNewEndCol d2
      }

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Check if span ends before a position.
spanEndsBefore :: SrcSpan -> (Int, Int) -> Bool
spanEndsBefore theSpan (ln, col) =
  srcSpanEndLineRaw theSpan < ln
  || (srcSpanEndLineRaw theSpan == ln && srcSpanEndColRaw theSpan <= col)

-- | Check if span starts after a position.
spanStartsAfter :: SrcSpan -> (Int, Int) -> Bool
spanStartsAfter theSpan (ln, col) =
  srcSpanStartLineRaw theSpan > ln
  || (srcSpanStartLineRaw theSpan == ln && srcSpanStartColRaw theSpan >= col)

-- | Check if span overlaps with the edit region of a delta.
spansOverlapDelta :: SpanDelta -> SrcSpan -> Bool
spansOverlapDelta delta theSpan =
  let (editStartLine, editStartCol) = sdEditStart delta
      (editEndLine, editEndCol) = sdEditEnd delta
      theSpanStartLine = srcSpanStartLineRaw theSpan
      theSpanStartCol = srcSpanStartColRaw theSpan
      theSpanEndLine = srcSpanEndLineRaw theSpan
      theSpanEndCol = srcSpanEndColRaw theSpan
  in not (theSpanEndLine < editStartLine
          || (theSpanEndLine == editStartLine && theSpanEndCol <= editStartCol)
          || theSpanStartLine > editEndLine
          || (theSpanStartLine == editEndLine && theSpanStartCol >= editEndCol))

-- | Apply full line/column adjustment to a span.
adjustSpanFull :: SpanDelta -> SrcSpan -> SrcSpan
adjustSpanFull delta theSpan = theSpan
  { srcSpanStartLine = Line $ srcSpanStartLineRaw theSpan + sdLineDelta delta
  , srcSpanEndLine = Line $ srcSpanEndLineRaw theSpan + sdLineDelta delta
  }

-- | Apply column-only adjustment (for spans on the same line as edit end).
adjustSpanColumn :: SpanDelta -> SrcSpan -> SrcSpan
adjustSpanColumn delta theSpan
  -- Single line span on the affected line
  | srcSpanStartLineRaw theSpan == srcSpanEndLineRaw theSpan = theSpan
      { srcSpanStartCol = Column $ srcSpanStartColRaw theSpan + sdColDelta delta
      , srcSpanEndCol = Column $ srcSpanEndColRaw theSpan + sdColDelta delta
      }
  -- Multi-line span starting on affected line
  | otherwise = theSpan
      { srcSpanStartCol = Column $ srcSpanStartColRaw theSpan + sdColDelta delta
      }
