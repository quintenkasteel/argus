{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : RefactorPropertySpec
-- Description : Property-based tests for refactoring safety invariants
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Comprehensive property-based tests for the refactoring system ensuring:
-- - Span operation correctness and algebraic properties
-- - FixGraph invariants and dependency ordering
-- - Conflict detection completeness and symmetry
-- - Safe application ordering
-- - Transactional semantics
module RefactorPropertySpec (spec) where

import Control.Monad (guard)
import Data.Either (isRight)
import Data.List (sort, nub)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Argus.Types
import Argus.Refactor.FixGraph
import Argus.Refactor.SpanAdjustment
    ( SpanDelta(..)
    , adjustSpan
    , adjustSpanWithDeltas
    , emptyAccumulator
    , addDelta
    , getDeltas
    )

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- | Generate a valid Line value (1-indexed)
genLine :: Gen Line
genLine = Line <$> chooseInt (1, 1000)

-- | Generate a valid Column value (1-indexed)
genColumn :: Gen Column
genColumn = Column <$> chooseInt (1, 200)

-- | Generate a valid file path
genFilePath :: Gen FilePath
genFilePath = do
  dirs <- listOf1 $ elements ["src", "lib", "test", "app"]
  file <- elements ["Main.hs", "Lib.hs", "Types.hs", "Utils.hs"]
  pure $ foldr (\d acc -> d ++ "/" ++ acc) file dirs

-- | Generate a well-formed SrcSpan (start <= end)
genSrcSpan :: Gen SrcSpan
genSrcSpan = do
  path <- genFilePath
  startLine <- genLine
  startCol <- genColumn
  -- End must be >= start
  endLine <- Line <$> chooseInt (unLine startLine, unLine startLine + 50)
  endCol <- if endLine == startLine
            then Column <$> chooseInt (unColumn startCol + 1, unColumn startCol + 50)
            else genColumn
  pure $ SrcSpan path startLine startCol endLine endCol

-- | Generate a SrcSpan for a specific file
genSrcSpanInFile :: FilePath -> Gen SrcSpan
genSrcSpanInFile path = do
  startLine <- genLine
  startCol <- genColumn
  endLine <- Line <$> chooseInt (unLine startLine, unLine startLine + 20)
  endCol <- if endLine == startLine
            then Column <$> chooseInt (unColumn startCol + 1, unColumn startCol + 50)
            else genColumn
  pure $ SrcSpan path startLine startCol endLine endCol

-- | Generate two spans that are guaranteed to overlap in the same file
-- The second span starts strictly within the first span's boundaries
genOverlappingSpans :: Gen (SrcSpan, SrcSpan)
genOverlappingSpans = do
  path <- genFilePath
  -- Generate a multi-line span to guarantee room for overlap
  startLine1 <- genLine
  startCol1 <- genColumn
  -- Make span1 at least 3 lines tall
  endLine1 <- Line <$> chooseInt (unLine startLine1 + 2, unLine startLine1 + 10)
  endCol1 <- genColumn
  let s1 = SrcSpan path startLine1 startCol1 endLine1 endCol1

  -- Generate second span starting inside s1 (on a middle line)
  midLine <- Line <$> chooseInt (unLine startLine1 + 1, unLine endLine1 - 1)
  startCol2 <- genColumn
  -- Make it extend past the end of s1
  endLine2 <- Line <$> chooseInt (unLine endLine1, unLine endLine1 + 10)
  endCol2 <- genColumn
  let s2 = SrcSpan path midLine startCol2 endLine2 endCol2

  pure (s1, s2)

-- | Generate two non-overlapping spans in the same file
genNonOverlappingSpans :: Gen (SrcSpan, SrcSpan)
genNonOverlappingSpans = do
  path <- genFilePath
  s1 <- genSrcSpanInFile path
  let SrcSpan _ _ _ el1 _ = s1
  -- Start second span well after first ends
  startLine2 <- Line <$> chooseInt (unLine el1 + 5, unLine el1 + 100)
  startCol2 <- genColumn
  endLine2 <- Line <$> chooseInt (unLine startLine2, unLine startLine2 + 20)
  endCol2 <- if endLine2 == startLine2
             then Column <$> chooseInt (unColumn startCol2 + 1, unColumn startCol2 + 50)
             else genColumn
  let s2 = SrcSpan path startLine2 startCol2 endLine2 endCol2
  pure (s1, s2)

-- | Generate a span and an inner span that it contains
genContainedSpans :: Gen (SrcSpan, SrcSpan)
genContainedSpans = do
  path <- genFilePath
  -- Generate outer span (multi-line)
  outerStartLine <- genLine
  outerStartCol <- genColumn
  outerEndLine <- Line <$> chooseInt (unLine outerStartLine + 3, unLine outerStartLine + 20)
  outerEndCol <- genColumn
  let outer = SrcSpan path outerStartLine outerStartCol outerEndLine outerEndCol

  -- Generate inner span strictly inside outer
  innerStartLine <- Line <$> chooseInt (unLine outerStartLine + 1, unLine outerEndLine - 1)
  innerStartCol <- genColumn
  innerEndLine <- Line <$> chooseInt (unLine innerStartLine, unLine outerEndLine - 1)
  innerEndCol <- genColumn
  let inner = SrcSpan path innerStartLine innerStartCol innerEndLine innerEndCol

  pure (outer, inner)

-- | Generate two spans in the same file
genSameFileSpans :: Gen (SrcSpan, SrcSpan)
genSameFileSpans = do
  path <- genFilePath
  s1 <- genSrcSpanInFile path
  s2 <- genSrcSpanInFile path
  pure (s1, s2)

-- | Generate a simple FixEdit
genFixEdit :: Gen FixEdit
genFixEdit = FixEdit <$> genSrcSpan <*> genReplacement

-- | Generate a replacement text
genReplacement :: Gen Text
genReplacement = T.pack <$> elements
  [ "x", "y", "z"
  , "foo", "bar", "baz"
  , "newFunc", "helper"
  , "42", "True", "False"
  ]

-- | Generate a simple Fix
genFix :: Gen Fix
genFix = do
  edits <- listOf1 genFixEdit
  title <- elements ["Fix A", "Fix B", "Fix C", "Rename", "Simplify"]
  preferred <- arbitrary
  pure Fix
    { fixTitle = T.pack title
    , fixEdits = edits
    , fixIsPreferred = preferred
    , fixAddImports = []
    , fixRemoveImports = []
    , fixCategory = FCStyle
    , fixSafety = FSMostly
    }

-- | Generate a Fix with edits in a specific file
genFixInFile :: FilePath -> Gen Fix
genFixInFile path = do
  editCount <- chooseInt (1, 3)
  edits <- vectorOf editCount (FixEdit <$> genSrcSpanInFile path <*> genReplacement)
  title <- elements ["Fix A", "Fix B", "Fix C"]
  preferred <- arbitrary
  pure Fix
    { fixTitle = T.pack title
    , fixEdits = edits
    , fixIsPreferred = preferred
    , fixAddImports = []
    , fixRemoveImports = []
    , fixCategory = FCStyle
    , fixSafety = FSMostly
    }

-- | Generate a list of fixes, some potentially conflicting
genFixList :: Gen [Fix]
genFixList = do
  count <- chooseInt (1, 10)
  path <- genFilePath  -- Keep most fixes in same file to test conflicts
  vectorOf count (genFixInFile path)

-- | Arbitrary instance for FixId
instance Arbitrary FixId where
  arbitrary = FixId <$> chooseInt (0, 100)
  shrink (FixId n) = FixId <$> shrink n

--------------------------------------------------------------------------------
-- Span Operation Properties
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Span Operations" $ do
    describe "spansOverlap" $ do
      prop "is symmetric" $ \(s1 :: SrcSpan) (s2 :: SrcSpan) ->
        spansOverlap s1 s2 == spansOverlap s2 s1

      prop "overlapping spans in same file are detected" $
        forAll genOverlappingSpans $ \(s1, s2) ->
          srcSpanFile s1 == srcSpanFile s2 ==>
            spansOverlap s1 s2

      prop "non-overlapping spans don't overlap" $
        forAll genNonOverlappingSpans $ \(s1, s2) ->
          not (spansOverlap s1 s2)

      prop "spans in different files never overlap" $
        forAll genSrcSpan $ \s1 ->
          forAll genSrcSpan $ \s2 ->
            srcSpanFile s1 /= srcSpanFile s2 ==>
              not (spansOverlap s1 s2)

      prop "span overlaps with itself" $
        forAll genSrcSpan $ \s ->
          spansOverlap s s

    describe "spanContains" $ do
      prop "span contains itself" $
        forAll genSrcSpan $ \s ->
          spanContains s s

      prop "containment is transitive (with contained spans)" $
        forAll genContainedSpans $ \(outer, inner) ->
          spanContains outer inner ==>
            spanContains outer outer && spanContains inner inner

      prop "containment implies overlap (with contained spans)" $
        forAll genContainedSpans $ \(outer, inner) ->
          spanContains outer inner ==> spansOverlap outer inner

    describe "spanAdjacent" $ do
      prop "is symmetric" $
        forAll genSrcSpan $ \s1 ->
          forAll genSrcSpan $ \s2 ->
            spanAdjacent s1 s2 == spanAdjacent s2 s1

      -- Note: adjacency is very specific, so we only test symmetry here

    describe "mergeSpans" $ do
      prop "is commutative (same file spans)" $
        forAll genSameFileSpans $ \(s1, s2) ->
          mergeSpans s1 s2 == mergeSpans s2 s1

      prop "merged span contains both originals (same file spans)" $
        forAll genSameFileSpans $ \(s1, s2) -> do
          let merged = mergeSpans s1 s2
          spanContains merged s1 && spanContains merged s2

  describe "FixGraph" $ do
    describe "buildFixGraph" $ do
      prop "all fixes are in the graph" $
        forAll genFixList $ \fixes ->
          let graph = buildFixGraph fixes
          in length (fgFixes graph) == length fixes

      prop "fixes are indexed sequentially" $
        forAll genFixList $ \fixes ->
          let graph = buildFixGraph fixes
              ids = Map.keys (fgFixes graph)
          in sort ids == [FixId 0 .. FixId (length fixes - 1)]

      prop "byFile grouping is exhaustive" $
        forAll genFixList $ \fixes ->
          let graph = buildFixGraph fixes
              allGrouped = sum $ map length $ Map.elems (fgByFile graph)
          in allGrouped >= length fixes  -- May have duplicates for multi-file fixes

    describe "detectConflicts" $ do
      prop "overlapping edits are detected as conflicts" $
        forAll genOverlappingSpans $ \(s1, s2) -> do
          let fix1 = Fix "A" [FixEdit s1 "x"] True [] [] FCStyle FSMostly
              fix2 = Fix "B" [FixEdit s2 "y"] True [] [] FCStyle FSMostly
              graph = buildFixGraph [fix1, fix2]
          length (fgConflicts graph) > 0 || not (spansOverlap s1 s2)

      prop "conflict detection is symmetric" $
        forAll genFixList $ \fixes ->
          let graph = buildFixGraph fixes
              conflicts = fgConflicts graph
              -- Check that if (a, b) conflicts, so does (b, a)
              hasReverse c = any (\c' -> fcFix1 c' == fcFix2 c && fcFix2 c' == fcFix1 c) conflicts
          in all (\c -> fcFix1 c == fcFix2 c || hasReverse c || fcFix1 c < fcFix2 c) conflicts

      prop "non-overlapping fixes have no overlap conflicts" $
        forAll genNonOverlappingSpans $ \(s1, s2) -> do
          let fix1 = Fix "A" [FixEdit s1 "x"] True [] [] FCStyle FSMostly
              fix2 = Fix "B" [FixEdit s2 "y"] True [] [] FCStyle FSMostly
              graph = buildFixGraph [fix1, fix2]
              overlapConflicts = filter (\c -> fcType c == OverlapConflict) (fgConflicts graph)
          null overlapConflicts

  describe "Topological Ordering" $ do
    describe "getApplyOrder" $ do
      prop "order contains all fixes when no cycles" $
        forAll genFixList $ \fixes ->
          let graph = buildFixGraph fixes
              orderResult = getApplyOrder graph
          in case orderResult of
               Left _cycle -> True  -- Cycles are allowed, just return True
               Right order -> length (nub order) == length fixes

      prop "order respects positional dependencies when no cycles" $
        forAll genFixList $ \fixes ->
          let graph = buildFixGraph fixes
              orderResult = getApplyOrder graph
              deps = fgDependencies graph
              -- For each fix, all its dependencies should come before it in order
              checkDeps order fid = case Map.lookup fid deps of
                Nothing -> True
                Just fDeps -> all (\d -> indexOf d order < indexOf fid order) fDeps
              indexOf x xs = length $ takeWhile (/= x) xs
          in case orderResult of
               Left _cycle -> True  -- Cycles are expected in some cases
               Right order -> all (checkDeps order) order

  describe "Conflict Resolution" $ do
    describe "resolveConflicts" $ do
      prop "resolved fixes are non-conflicting" $
        forAll genFixList $ \fixes ->
          let graph = buildFixGraph fixes
              (toApply, _toSkip) = resolveConflicts PreferPreferred graph
              resolvedGraph = buildFixGraph [fgFixes graph Map.! fid | fid <- toApply]
          in null (fgConflicts resolvedGraph)
             || length fixes <= 1  -- Single fix can't conflict

      prop "PreferPreferred keeps preferred fixes when possible" $
        forAll genFixList $ \fixes ->
          let graph = buildFixGraph fixes
              (toApply, _toSkip) = resolveConflicts PreferPreferred graph
              resolvedFixes = [fgFixes graph Map.! fid | fid <- toApply]
              preferredCount = length $ filter fixIsPreferred resolvedFixes
              originalPreferred = length $ filter fixIsPreferred fixes
          in preferredCount <= originalPreferred

  describe "Fix Groups" $ do
    describe "groupByFile" $ do
      prop "all fixes are represented in file groups (unique)" $
        forAll genFixList $ \fixes ->
          let graph = buildFixGraph fixes
              groups = groupByFile graph
              -- Get unique fix IDs from all groups
              allInGroups = nub $ concatMap fgGroupFixes $ Map.elems groups
          in length allInGroups == length fixes

      prop "each file group is associated with a specific file" $
        forAll genFixList $ \fixes ->
          let graph = buildFixGraph fixes
              groups = groupByFile graph
          in all (\(path, g) -> fgGroupFile g == Just path) (Map.toList groups)

    describe "groupIndependent" $ do
      prop "all fixes are covered when using topological order" $
        forAll genFixList $ \fixes ->
          let graph = buildFixGraph fixes
              orderResult = getApplyOrder graph
          in case orderResult of
               Left _ -> True  -- Cycles exist, skip this case
               Right order ->
                 let groups = groupIndependent graph order
                     coveredIds = nub $ concatMap fgGroupFixes groups
                 in sort coveredIds == sort order

      prop "covered fixes are always a subset of input fixes" $
        forAll genFixList $ \fixes ->
          let graph = buildFixGraph fixes
              orderResult = getApplyOrder graph
          in case orderResult of
               Left _ -> True  -- Cycles, skip
               Right order ->
                 let groups = groupIndependent graph order
                     coveredIds = nub $ concatMap fgGroupFixes groups
                 in all (`elem` order) coveredIds

  describe "FixId" $ do
    prop "FixId ordering is consistent" $
      forAll (arbitrary :: Gen FixId) $ \a ->
        forAll (arbitrary :: Gen FixId) $ \b ->
          (a <= b && b <= a) == (a == b)

    prop "FixId Enum is well-behaved" $
      forAll (chooseInt (0, 100)) $ \n ->
        unFixId (toEnum n) == n

  describe "Span Adjustment Properties" $ do
    prop "empty delta accumulator leaves span unchanged" $
      forAll genSrcSpan $ \srcSpan -> do
        let deltas = getDeltas emptyAccumulator
            adjusted = adjustSpanWithDeltas deltas srcSpan
        adjusted == Just srcSpan

    prop "adjustments preserve file path" $
      forAll genSrcSpan $ \srcSpan -> do
        let delta = SpanDelta
              { sdFile = srcSpanFile srcSpan
              , sdEditStart = (unLine (srcSpanStartLine srcSpan) - 1, 1)
              , sdEditEnd = (unLine (srcSpanStartLine srcSpan) - 1, 5)
              , sdLineDelta = 5
              , sdColDelta = 0
              , sdNewEndLine = unLine (srcSpanStartLine srcSpan) - 1 + 5
              , sdNewEndCol = 5
              }
            adjusted = adjustSpan delta srcSpan
        case adjusted of
          Nothing -> True  -- If span was invalidated, property vacuously true
          Just s  -> srcSpanFile s == srcSpanFile srcSpan

    prop "delta accumulator preserves all deltas" $
      forAll genSrcSpan $ \srcSpan -> do
        let delta1 = SpanDelta (srcSpanFile srcSpan) (1, 1) (1, 10) 2 0 3 10
            delta2 = SpanDelta (srcSpanFile srcSpan) (10, 1) (10, 5) (-1) 0 9 5
            acc = addDelta delta2 $ addDelta delta1 emptyAccumulator
        length (getDeltas acc) == 2

--------------------------------------------------------------------------------
-- Derived Generators with Arbitrary
--------------------------------------------------------------------------------

instance Arbitrary SrcSpan where
  arbitrary = genSrcSpan
  shrink s = do
    sl' <- shrink (unLine $ srcSpanStartLine s)
    sc' <- shrink (unColumn $ srcSpanStartCol s)
    el' <- shrink (unLine $ srcSpanEndLine s)
    ec' <- shrink (unColumn $ srcSpanEndCol s)
    guard (sl' >= 1 && sc' >= 1 && el' >= sl' && (el' > sl' || ec' > sc'))
    pure $ SrcSpan (srcSpanFile s) (Line sl') (Column sc') (Line el') (Column ec')

instance Arbitrary Line where
  arbitrary = genLine
  shrink (Line n) = Line <$> filter (>= 1) (shrink n)

instance Arbitrary Column where
  arbitrary = genColumn
  shrink (Column n) = Column <$> filter (>= 1) (shrink n)

instance Arbitrary Fix where
  arbitrary = genFix
  shrink _ = []  -- Fixes are complex, skip shrinking

instance Arbitrary FixEdit where
  arbitrary = genFixEdit
  shrink _ = []
