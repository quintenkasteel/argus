{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Refactor.FixGraph
-- Description : Dependency graph for fixes with topological ordering
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
-- Portability : GHC
--
-- = Overview
--
-- This module provides sophisticated dependency analysis for fixes,
-- building graphs to detect conflicts, order dependencies, and enable
-- safe parallel application.
--
-- = Architecture
--
-- @
-- ┌─────────────────────────────────────────────────────────────────────┐
-- │                          FixGraph                                   │
-- │  ┌─────────────┐  ┌──────────────────┐  ┌──────────────────────┐   │
-- │  │  fgFixes    │  │  fgDependencies  │  │    fgConflicts       │   │
-- │  │ Map FixId   │  │  Map FixId       │  │  [FixConflict]       │   │
-- │  │     Fix     │  │      [FixId]     │  │                      │   │
-- │  └─────────────┘  └──────────────────┘  └──────────────────────┘   │
-- │  ┌─────────────────────────────┐  ┌────────────────────────────┐   │
-- │  │    fgDependents             │  │    fgByFile                │   │
-- │  │    Map FixId [FixId]        │  │    Map FilePath [FixId]    │   │
-- │  └─────────────────────────────┘  └────────────────────────────┘   │
-- └─────────────────────────────────────────────────────────────────────┘
-- @
--
-- = Key Operations
--
-- * __Graph Building__: 'buildFixGraph' constructs dependency graph from fixes
-- * __Cycle Detection__: 'hasCycles' and 'getCycles' identify unresolvable dependencies
-- * __Ordering__: 'getApplyOrder' returns topologically sorted fixes
-- * __Grouping__: 'getIndependentGroups' enables parallel application
-- * __Conflict Detection__: 'detectConflicts' finds overlapping/incompatible fixes
--
-- = Dependency Types
--
-- * __Spatial__: Fixes that modify overlapping source spans
-- * __Semantic__: Fixes where one's output affects another's applicability
-- * __Rule-based__: Explicit dependencies declared in rule definitions
--
-- = Thread Safety
--
-- 'FixGraph' is immutable after construction and safe for concurrent reads.
--
-- @since 1.0.0
module Argus.Refactor.FixGraph
  ( -- * Fix Graph
    FixGraph (..)
  , FixId (..)
  , buildFixGraph
  , buildFixGraphWithRuleDeps
  , getApplyOrder
  , getIndependentGroups
  , hasCycles
  , getCycles

    -- * Dependencies
  , FixDependency (..)
  , DependencyType (..)
  , analyzeDependencies
  , RuleDependencyMap

    -- * Conflict Detection
  , FixConflict (..)
  , ConflictType (..)
  , detectConflicts
  , resolveConflicts
  , ConflictStrategy (..)

    -- * Fix Grouping
  , FixGroup (..)
  , groupByFile
  , groupIndependent
  , mergeGroups

    -- * Span Operations
  , spansOverlap
  , spanContains
  , spanAdjacent
  , mergeSpans
  ) where

import Data.List (partition, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Graph (stronglyConnComp, SCC(..))

import Argus.Types

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

-- | Unique identifier for a fix (based on index in list)
-- Using newtype for type safety - prevents accidental mixing with other Ints
newtype FixId = FixId { unFixId :: Int }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Enum)

-- | A graph of fixes with dependencies
data FixGraph = FixGraph
  { fgFixes        :: Map FixId Fix        -- ^ All fixes by ID
  , fgDependencies :: Map FixId [FixId]    -- ^ Fix -> fixes it depends on
  , fgDependents   :: Map FixId [FixId]    -- ^ Fix -> fixes that depend on it
  , fgConflicts    :: [FixConflict]        -- ^ Detected conflicts
  , fgByFile       :: Map FilePath [FixId] -- ^ Fixes grouped by file
  }
  deriving stock (Eq, Show)

-- | A dependency between two fixes
data FixDependency = FixDependency
  { fdFrom :: FixId
  , fdTo   :: FixId
  , fdType :: DependencyType
  }
  deriving stock (Eq, Show)

-- | Type of dependency
data DependencyType
  = PositionalDep      -- ^ Fix A changes positions that Fix B references
  | IdentifierDep Text -- ^ Fix A renames identifier that Fix B uses
  | ImportDep Text     -- ^ Fix A adds import that Fix B requires
  | ScopeDep           -- ^ Fix A affects scope that contains Fix B
  | RuleDep Text       -- ^ Fix A from rule X depends on rule Y being applied first
  deriving stock (Eq, Show)

-- | Map from rule ID to (dependencies, conflicts)
type RuleDependencyMap = Map Text ([Text], [Text])

-- | A conflict between fixes
data FixConflict = FixConflict
  { fcFix1 :: FixId
  , fcFix2 :: FixId
  , fcType :: ConflictType
  , fcDesc :: Text
  }
  deriving stock (Eq, Show)

-- | Type of conflict
data ConflictType
  = OverlapConflict     -- ^ Fixes edit overlapping regions
  | IdentifierConflict  -- ^ Both rename same identifier differently
  | ScopeConflict       -- ^ Conflicting scope changes
  | SemanticConflict    -- ^ Changes would break semantics
  deriving stock (Eq, Show, Ord)

-- | Strategy for resolving conflicts
data ConflictStrategy
  = SkipAll             -- ^ Skip all conflicting fixes
  | SkipSecond          -- ^ Keep first fix, skip second
  | PreferPreferred     -- ^ Keep fixIsPreferred, skip others
  | PreferSeverity      -- ^ Keep higher severity fixes
  | PreferSmaller       -- ^ Keep smaller (more targeted) fixes
  | Interactive         -- ^ Ask user
  | Merge               -- ^ Try to merge if possible
  deriving stock (Eq, Show, Ord)

-- | A group of fixes that can be applied together
data FixGroup = FixGroup
  { fgGroupFixes :: [FixId]
  , fgGroupFile  :: Maybe FilePath  -- ^ If all in same file
  , fgGroupOrder :: Int             -- ^ Application order
  , fgGroupSafe  :: Bool            -- ^ All fixes are safe (preferred)
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Graph Construction
--------------------------------------------------------------------------------

-- | Build a fix graph from a list of fixes
buildFixGraph :: [Fix] -> FixGraph
buildFixGraph fixes =
  let indexed = zip (map FixId [0..]) fixes
      fixMap = Map.fromList indexed

      -- Group by file
      byFile = groupFixesByFile indexed

      -- Analyze dependencies
      deps = analyzeDependencies indexed

      -- Build dependency maps
      depMap = Map.fromListWith (++)
        [(fdFrom d, [fdTo d]) | d <- deps]
      revDepMap = Map.fromListWith (++)
        [(fdTo d, [fdFrom d]) | d <- deps]

      -- Detect conflicts
      conflicts = detectAllConflicts indexed

  in FixGraph
    { fgFixes = fixMap
    , fgDependencies = depMap
    , fgDependents = revDepMap
    , fgConflicts = conflicts
    , fgByFile = byFile
    }

-- | Build a fix graph with rule-level dependencies
--
-- Takes a rule dependency map (from ConfigurableRules.buildRuleDependencyMap)
-- and incorporates rule-level dependencies into the fix graph. This allows
-- fixes to be ordered based on rule dependencies (e.g., "import-add" before
-- "use-qualified").
--
-- The fix title is used to extract the rule ID using 'extractRuleIdFromFix'.
-- If a fix's rule depends on another rule that also has fixes in this batch,
-- those dependencies are added to the graph.
buildFixGraphWithRuleDeps :: RuleDependencyMap -> [Fix] -> FixGraph
buildFixGraphWithRuleDeps ruleDeps fixes =
  let baseGraph = buildFixGraph fixes
      indexed = zip (map FixId [0..]) fixes

      -- Build rule ID -> FixId mapping
      ruleToFixes = Map.fromListWith (++)
        [ (ruleId, [fid])
        | (fid, fix) <- indexed
        , ruleId <- maybeToList $ extractRuleIdFromFix fix
        ]

      -- Analyze rule-level dependencies
      ruleDepsAnalyzed = analyzeRuleDependencies indexed ruleDeps ruleToFixes

      -- Analyze rule-level conflicts
      ruleConflictsAnalyzed = analyzeRuleConflicts indexed ruleDeps ruleToFixes

      -- Merge with existing dependencies
      existingDeps = fgDependencies baseGraph
      ruleDepsMap = Map.fromListWith (++)
        [(fdFrom d, [fdTo d]) | d <- ruleDepsAnalyzed]
      mergedDeps = Map.unionWith (++) existingDeps ruleDepsMap

      -- Rebuild reverse deps
      mergedRevDeps = Map.fromListWith (++)
        [(to, [from]) | (from, tos) <- Map.toList mergedDeps, to <- tos]

      -- Merge conflicts
      mergedConflicts = nub $ fgConflicts baseGraph ++ ruleConflictsAnalyzed

  in baseGraph
    { fgDependencies = mergedDeps
    , fgDependents = mergedRevDeps
    , fgConflicts = mergedConflicts
    }
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

-- | Extract rule ID from a fix's title
--
-- Rule IDs in titles follow the pattern: "[rule-id] message" or "rule-id: message"
-- Examples:
--   - "[performance/length-null] Use null instead of length == 0"
--   - "partial/head: Use headMay instead of head"
extractRuleIdFromFix :: Fix -> Maybe Text
extractRuleIdFromFix fix =
  let title = fixTitle fix
  in case T.stripPrefix "[" title of
       Just rest ->
         case T.breakOn "]" rest of
           (ruleId, remaining) | not (T.null remaining) -> Just ruleId
           _ -> extractColonFormat title
       Nothing -> extractColonFormat title
  where
    extractColonFormat :: Text -> Maybe Text
    extractColonFormat t =
      case T.breakOn ": " t of
        (prefix, rest) | not (T.null rest) && "/" `T.isInfixOf` prefix -> Just prefix
        _ -> Nothing

-- | Analyze rule-level dependencies between fixes
analyzeRuleDependencies :: [(FixId, Fix)] -> RuleDependencyMap -> Map Text [FixId] -> [FixDependency]
analyzeRuleDependencies indexed ruleDeps ruleToFixes =
  [ FixDependency fid1 fid2 (RuleDep depRule)
  | (fid1, fix1) <- indexed
  , ruleId1 <- maybeToList $ extractRuleIdFromFix fix1
  , (depRules, _) <- maybeToList $ Map.lookup ruleId1 ruleDeps
  , depRule <- depRules
  , fid2 <- fromMaybe [] (Map.lookup depRule ruleToFixes)
  , fid1 /= fid2
  ]
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

-- | Analyze rule-level conflicts between fixes
analyzeRuleConflicts :: [(FixId, Fix)] -> RuleDependencyMap -> Map Text [FixId] -> [FixConflict]
analyzeRuleConflicts indexed ruleDeps ruleToFixes =
  [ FixConflict fid1 fid2 SemanticConflict
      ("Rules " <> ruleId1 <> " and " <> conflictRule <> " are mutually exclusive")
  | (fid1, fix1) <- indexed
  , ruleId1 <- maybeToList $ extractRuleIdFromFix fix1
  , (_, conflictRules) <- maybeToList $ Map.lookup ruleId1 ruleDeps
  , conflictRule <- conflictRules
  , fid2 <- fromMaybe [] (Map.lookup conflictRule ruleToFixes)
  , fid1 < fid2  -- Only report each conflict once
  ]
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

-- | Group fixes by their target file
groupFixesByFile :: [(FixId, Fix)] -> Map FilePath [FixId]
groupFixesByFile fixes = Map.fromListWith (++)
  [ (srcSpanFile (fixEditSpan edit), [fid])
  | (fid, fix) <- fixes
  , edit <- fixEdits fix
  ]

--------------------------------------------------------------------------------
-- Dependency Analysis
--------------------------------------------------------------------------------

-- | Analyze dependencies between fixes
analyzeDependencies :: [(FixId, Fix)] -> [FixDependency]
analyzeDependencies fixes = concat
  [ analyzePositionalDeps fixes
  , analyzeIdentifierDeps fixes
  ]

-- | Find positional dependencies (Fix A edits region before Fix B's region)
analyzePositionalDeps :: [(FixId, Fix)] -> [FixDependency]
analyzePositionalDeps fixes =
  [ FixDependency fid1 fid2 PositionalDep
  | (fid1, fix1) <- fixes
  , (fid2, fix2) <- fixes
  , fid1 /= fid2
  , fixAffectsPosition fix1 fix2
  ]

-- | Check if fix1 could affect the positions referenced by fix2
-- A positional dependency exists when fix1 ENDS BEFORE fix2 STARTS
-- and applying fix1 would change the number of characters/lines,
-- thus shifting fix2's positions. Overlapping spans are conflicts,
-- not dependencies, and are handled separately.
fixAffectsPosition :: Fix -> Fix -> Bool
fixAffectsPosition fix1 fix2 =
  any (\e1 -> any (editAffects e1) (fixEdits fix2)) (fixEdits fix1)
  where
    editAffects e1 e2 =
      let s1 = fixEditSpan e1
          s2 = fixEditSpan e2
      in srcSpanFile s1 == srcSpanFile s2
         && not (spansOverlap s1 s2)  -- Overlapping = conflict, not dependency
         && strictlyBefore s1 s2       -- s1 ends before s2 starts
         && changesLength e1 s1        -- Only if fix1 changes text length

    -- Check if s1 ends strictly before s2 starts
    strictlyBefore s1 s2 =
      srcSpanEndLine s1 < srcSpanStartLine s2 ||
      (srcSpanEndLine s1 == srcSpanStartLine s2 &&
       srcSpanEndCol s1 < srcSpanStartCol s2)

    -- Check if an edit changes the text length
    changesLength e1 s1 =
      T.length (fixEditNewText e1) /= spanLength s1

    spanLength s =
      if srcSpanStartLine s == srcSpanEndLine s
      then unColumn (srcSpanEndCol s) - unColumn (srcSpanStartCol s)
      else 1000  -- Multi-line, assume changes length

-- | Find identifier dependencies (Fix A renames something Fix B uses)
analyzeIdentifierDeps :: [(FixId, Fix)] -> [FixDependency]
analyzeIdentifierDeps fixes =
  [ FixDependency fid1 fid2 (IdentifierDep oldName)
  | (fid1, fix1) <- fixes
  , (fid2, fix2) <- fixes
  , fid1 /= fid2
  , oldName <- extractRenamedIdentifiers fix1
  , identifierUsedInFix oldName fix2
  ]

-- | Extract identifiers being renamed by a fix
extractRenamedIdentifiers :: Fix -> [Text]
extractRenamedIdentifiers fix =
  -- Simple heuristic: if the fix title starts with "Rename" or "Replace"
  -- and the old/new texts look like identifiers
  case T.words (fixTitle fix) of
    ("Rename":old:"to":_) -> [old]
    ("Replace":old:"with":_) -> [old]
    _ -> []

-- | Check if an identifier is used in a fix's edit
identifierUsedInFix :: Text -> Fix -> Bool
identifierUsedInFix ident fix =
  any (\e -> ident `T.isInfixOf` fixEditNewText e) (fixEdits fix)

--------------------------------------------------------------------------------
-- Conflict Detection
--------------------------------------------------------------------------------

-- | Detect all conflicts between fixes
detectAllConflicts :: [(FixId, Fix)] -> [FixConflict]
detectAllConflicts fixes = concat
  [ detectOverlapConflicts fixes
  , detectIdentifierConflicts fixes
  ]

-- | Detect overlapping edit regions
detectOverlapConflicts :: [(FixId, Fix)] -> [FixConflict]
detectOverlapConflicts fixes =
  [ FixConflict fid1 fid2 OverlapConflict
      ("Edits overlap at " <> showSpan s1 <> " and " <> showSpan s2)
  | (fid1, fix1) <- fixes
  , (fid2, fix2) <- fixes
  , fid1 < fid2  -- Only check each pair once
  , (s1, s2) <- findOverlappingSpans (fixEdits fix1) (fixEdits fix2)
  ]

-- | Find overlapping spans between two edit lists
findOverlappingSpans :: [FixEdit] -> [FixEdit] -> [(SrcSpan, SrcSpan)]
findOverlappingSpans edits1 edits2 =
  [ (fixEditSpan e1, fixEditSpan e2)
  | e1 <- edits1
  , e2 <- edits2
  , spansOverlap (fixEditSpan e1) (fixEditSpan e2)
  ]

-- | Detect conflicting identifier renames
detectIdentifierConflicts :: [(FixId, Fix)] -> [FixConflict]
detectIdentifierConflicts fixes =
  [ FixConflict fid1 fid2 IdentifierConflict
      ("Both fixes rename identifier '" <> ident <> "' differently")
  | (fid1, fix1) <- fixes
  , (fid2, fix2) <- fixes
  , fid1 < fid2
  , ident <- extractRenamedIdentifiers fix1
  , ident `elem` extractRenamedIdentifiers fix2
  ]

-- | Show a span as text
showSpan :: SrcSpan -> Text
showSpan s = T.pack $ srcSpanFile s <> ":" <> show (srcSpanStartLine s)
             <> ":" <> show (srcSpanStartCol s)

-- | Public API for conflict detection
detectConflicts :: [Fix] -> [FixConflict]
detectConflicts fixes = detectAllConflicts (zip (map FixId [0..]) fixes)

--------------------------------------------------------------------------------
-- Conflict Resolution
--------------------------------------------------------------------------------

-- | Resolve conflicts using given strategy
resolveConflicts :: ConflictStrategy -> FixGraph -> ([FixId], [FixId])
                 -- ^ (fixes to apply, fixes to skip)
resolveConflicts strategy graph =
  let conflicts = fgConflicts graph
      allFixes = Map.keys (fgFixes graph)
  in case strategy of
    SkipAll ->
      let conflicting = Set.fromList $
            concatMap (\c -> [fcFix1 c, fcFix2 c]) conflicts
      in partition (`Set.notMember` conflicting) allFixes

    SkipSecond ->
      let toSkip = Set.fromList $ map fcFix2 conflicts
      in partition (`Set.notMember` toSkip) allFixes

    PreferPreferred ->
      let resolveOne FixConflict{..} =
            case (fixIsPreferred <$> Map.lookup fcFix1 (fgFixes graph),
                  fixIsPreferred <$> Map.lookup fcFix2 (fgFixes graph)) of
              (Just True, Just False) -> fcFix2
              (Just False, Just True) -> fcFix1
              _ -> fcFix2  -- Default: skip second
          toSkip = Set.fromList $ map resolveOne conflicts
      in partition (`Set.notMember` toSkip) allFixes

    PreferSmaller ->
      let editCount fid = maybe 0 (length . fixEdits) (Map.lookup fid (fgFixes graph))
          resolveOne FixConflict{..} =
            if editCount fcFix1 <= editCount fcFix2
            then fcFix2 else fcFix1
          toSkip = Set.fromList $ map resolveOne conflicts
      in partition (`Set.notMember` toSkip) allFixes

    PreferSeverity ->
      -- Since Fix doesn't have severity, use heuristic:
      -- 1. Preferred fixes are higher priority (safety-related)
      -- 2. If both have same preferred status, prefer the one with more edits (more comprehensive fix)
      let fixPriority fid = case Map.lookup fid (fgFixes graph) of
            Nothing -> (False, 0 :: Int)
            Just fix -> (fixIsPreferred fix, length (fixEdits fix))
          resolveOne FixConflict{..} =
            let p1 = fixPriority fcFix1
                p2 = fixPriority fcFix2
            in if p1 >= p2 then fcFix2 else fcFix1  -- Skip the lower priority one
          toSkip = Set.fromList $ map resolveOne conflicts
      in partition (`Set.notMember` toSkip) allFixes

    Interactive ->
      -- For interactive mode, we apply all fixes and let the user decide
      -- during the interactive prompting phase. Pass all fixes as applicable.
      (allFixes, [])

    Merge ->
      -- Merge attempts to apply both fixes if they don't overlap at the character level.
      -- Since our span-based conflict detection is conservative, some "conflicts" may
      -- actually be non-overlapping at the character level.
      -- For now, use same logic as PreferPreferred - full merge requires edit-level analysis
      let resolveOne FixConflict{..} =
            case (fixIsPreferred <$> Map.lookup fcFix1 (fgFixes graph),
                  fixIsPreferred <$> Map.lookup fcFix2 (fgFixes graph)) of
              (Just True, Just False) -> fcFix2
              (Just False, Just True) -> fcFix1
              _ -> fcFix2  -- Default: skip second
          toSkip = Set.fromList $ map resolveOne conflicts
      in partition (`Set.notMember` toSkip) allFixes

--------------------------------------------------------------------------------
-- Topological Ordering
--------------------------------------------------------------------------------

-- | Get fixes in safe application order (topologically sorted)
getApplyOrder :: FixGraph -> Either [FixId] [FixId]
              -- ^ Left: cycle members, Right: ordered fix IDs
getApplyOrder graph =
  let fixes = Map.keys (fgFixes graph)
      edges = [ (fid, fid, fromMaybe [] (Map.lookup fid (fgDependencies graph)))
              | fid <- fixes
              ]
      sccs = stronglyConnComp edges
  in case findCycles sccs of
    cycles@(_:_) -> Left (concat cycles)
    [] -> Right $ concatMap flattenSCC sccs
  where
    flattenSCC (AcyclicSCC v) = [v]
    flattenSCC (CyclicSCC vs) = vs

    findCycles [] = []
    findCycles (CyclicSCC vs : rest) = vs : findCycles rest
    findCycles (_ : rest) = findCycles rest

-- | Check if graph has cycles
hasCycles :: FixGraph -> Bool
hasCycles graph = case getApplyOrder graph of
  Left _ -> True
  Right _ -> False

-- | Get cycle members if any
getCycles :: FixGraph -> [[FixId]]
getCycles graph =
  let fixes = Map.keys (fgFixes graph)
      edges = [ (fid, fid, fromMaybe [] (Map.lookup fid (fgDependencies graph)))
              | fid <- fixes
              ]
      sccs = stronglyConnComp edges
  in [ vs | CyclicSCC vs <- sccs, length vs > 1 ]

--------------------------------------------------------------------------------
-- Independent Grouping
--------------------------------------------------------------------------------

-- | Get groups of fixes that can be applied independently (in parallel)
getIndependentGroups :: FixGraph -> [FixGroup]
getIndependentGroups graph =
  let (toApply, _) = resolveConflicts PreferPreferred graph
  in case getApplyOrder graph of
    Left _ -> []  -- Has cycles, can't determine order
    Right ordered ->
      let applicable = filter (`elem` toApply) ordered
      in groupIndependent graph applicable

-- | Group fixes that don't depend on each other
groupIndependent :: FixGraph -> [FixId] -> [FixGroup]
groupIndependent graph fixes = go 0 Set.empty fixes []
  where
    go _ _ [] acc = reverse acc
    go order applied (fid:rest) acc =
      let deps = fromMaybe [] (Map.lookup fid (fgDependencies graph))
          -- Can apply if all dependencies are already applied
          canApply = all (`Set.member` applied) deps

          -- Find all fixes at this level (no deps on remaining)
          (thisLevel, nextLevel) =
            if canApply
            then partition (canApplyNow applied) (fid:rest)
            else ([], fid:rest)

          newGroup = if null thisLevel
                     then Nothing
                     else Just FixGroup
                       { fgGroupFixes = thisLevel
                       , fgGroupFile = getCommonFile thisLevel
                       , fgGroupOrder = order
                       , fgGroupSafe = all isSafe thisLevel
                       }

          newApplied = applied `Set.union` Set.fromList thisLevel
      in case newGroup of
        Nothing -> go order applied rest acc
        Just g -> go (order + 1) newApplied nextLevel (g:acc)

    canApplyNow applied fid =
      let deps = fromMaybe [] (Map.lookup fid (fgDependencies graph))
      in all (`Set.member` applied) deps

    getCommonFile fids =
      case nub [f | fid <- fids
                  , Just fix <- [Map.lookup fid (fgFixes graph)]
                  , edit <- fixEdits fix
                  , let f = srcSpanFile (fixEditSpan edit)] of
        [f] -> Just f
        _ -> Nothing

    isSafe fid = maybe False fixIsPreferred (Map.lookup fid (fgFixes graph))

-- | Group fixes by file
groupByFile :: FixGraph -> Map FilePath FixGroup
groupByFile graph = Map.mapWithKey mkGroup (fgByFile graph)
  where
    mkGroup file fids = FixGroup
      { fgGroupFixes = fids
      , fgGroupFile = Just file
      , fgGroupOrder = 0
      , fgGroupSafe = all isSafe fids
      }
    isSafe fid = maybe False fixIsPreferred (Map.lookup fid (fgFixes graph))

-- | Merge compatible fix groups
mergeGroups :: [FixGroup] -> [FixGroup]
mergeGroups [] = []
mergeGroups [g] = [g]
mergeGroups (g1:g2:rest)
  | canMerge g1 g2 = mergeGroups (merged : rest)
  | otherwise = g1 : mergeGroups (g2:rest)
  where
    canMerge a b = fgGroupFile a == fgGroupFile b
                && fgGroupOrder a == fgGroupOrder b
    merged = g1 { fgGroupFixes = fgGroupFixes g1 ++ fgGroupFixes g2 }

--------------------------------------------------------------------------------
-- Span Operations
--------------------------------------------------------------------------------

-- | Check if two spans overlap
spansOverlap :: SrcSpan -> SrcSpan -> Bool
spansOverlap s1 s2
  | srcSpanFile s1 /= srcSpanFile s2 = False
  | otherwise = not (before s1 s2 || before s2 s1)
  where
    before a b =
      srcSpanEndLine a < srcSpanStartLine b ||
      (srcSpanEndLine a == srcSpanStartLine b &&
       srcSpanEndCol a <= srcSpanStartCol b)

-- | Check if span a contains span b
spanContains :: SrcSpan -> SrcSpan -> Bool
spanContains outer inner =
  srcSpanFile outer == srcSpanFile inner
  && (srcSpanStartLine outer < srcSpanStartLine inner
      || (srcSpanStartLine outer == srcSpanStartLine inner
          && srcSpanStartCol outer <= srcSpanStartCol inner))
  && (srcSpanEndLine outer > srcSpanEndLine inner
      || (srcSpanEndLine outer == srcSpanEndLine inner
          && srcSpanEndCol outer >= srcSpanEndCol inner))

-- | Check if spans are adjacent (can be merged)
spanAdjacent :: SrcSpan -> SrcSpan -> Bool
spanAdjacent s1 s2
  | srcSpanFile s1 /= srcSpanFile s2 = False
  | srcSpanEndLine s1 == srcSpanStartLine s2 &&
    srcSpanEndCol s1 == srcSpanStartCol s2 = True
  | srcSpanEndLine s2 == srcSpanStartLine s1 &&
    srcSpanEndCol s2 == srcSpanStartCol s1 = True
  | otherwise = False

-- | Merge two spans into one
mergeSpans :: SrcSpan -> SrcSpan -> SrcSpan
mergeSpans s1 s2 = SrcSpan
  { srcSpanFile = srcSpanFile s1
  , srcSpanStartLine = min (srcSpanStartLine s1) (srcSpanStartLine s2)
  , srcSpanStartCol = if srcSpanStartLine s1 <= srcSpanStartLine s2
                      then if srcSpanStartLine s1 == srcSpanStartLine s2
                           then min (srcSpanStartCol s1) (srcSpanStartCol s2)
                           else srcSpanStartCol s1
                      else srcSpanStartCol s2
  , srcSpanEndLine = max (srcSpanEndLine s1) (srcSpanEndLine s2)
  , srcSpanEndCol = if srcSpanEndLine s1 >= srcSpanEndLine s2
                    then if srcSpanEndLine s1 == srcSpanEndLine s2
                         then max (srcSpanEndCol s1) (srcSpanEndCol s2)
                         else srcSpanEndCol s1
                    else srcSpanEndCol s2
  }
