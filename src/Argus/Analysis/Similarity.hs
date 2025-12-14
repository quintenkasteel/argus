{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Analysis.Similarity
-- Description : Tree Edit Distance and similarity algorithms for clone detection
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides algorithms for computing similarity between ASTs,
-- using a proper tree-structured edit distance algorithm that preserves
-- the hierarchical nature of ASTs rather than flattening them.
module Argus.Analysis.Similarity
  ( -- * Similarity Metrics
    Similarity (..)
  , computeSimilarity
  , computeTreeEditDistance

    -- * Clone Clustering
  , clusterBySimilarity
  , ClusterConfig (..)
  , defaultClusterConfig

    -- * Utility Functions
  , isAboveThreshold
  , similarityRatio
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Data.Array (Array, array, (!))
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import Argus.Analysis.Fingerprint

--------------------------------------------------------------------------------
-- Similarity Types
--------------------------------------------------------------------------------

-- | Similarity result between two code fragments
data Similarity = Similarity
  { simScore       :: Double     -- ^ Similarity score (0.0 to 1.0)
  , simEditDist    :: Int        -- ^ Tree edit distance
  , simMaxSize     :: Int        -- ^ Maximum of the two AST sizes
  , simCommonNodes :: Int        -- ^ Number of common structural nodes
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Configuration for clustering
data ClusterConfig = ClusterConfig
  { ccSimilarityThreshold :: Double    -- ^ Minimum similarity to be considered a clone
  , ccMinNodes            :: Int       -- ^ Minimum AST nodes to consider
  , ccMaxClusters         :: Int       -- ^ Maximum number of clusters to return
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default cluster configuration
defaultClusterConfig :: ClusterConfig
defaultClusterConfig = ClusterConfig
  { ccSimilarityThreshold = 0.8
  , ccMinNodes = 10
  , ccMaxClusters = 100
  }

--------------------------------------------------------------------------------
-- Tree Edit Distance (Zhang-Shasha Algorithm adapted for AST)
--------------------------------------------------------------------------------

-- | Compute tree edit distance between two normalized ASTs
-- Uses a recursive algorithm that respects tree structure
computeTreeEditDistance :: NormalizedAST -> NormalizedAST -> Int
computeTreeEditDistance ast1 ast2 = treeEditDistance ast1 ast2

-- | Proper tree edit distance that respects tree structure
-- Uses dynamic programming on subtree comparisons
treeEditDistance :: NormalizedAST -> NormalizedAST -> Int
treeEditDistance t1 t2 = case (t1, t2) of
  -- Base cases: literals
  (NLit l1, NLit l2) -> if l1 == l2 then 0 else 1

  -- Variables match if both are variables (after alpha-renaming)
  (NVar _, NVar _) -> 0

  -- Constructors must match exactly
  (NCon n1, NCon n2) -> if n1 == n2 then 0 else 1

  -- Operators must match exactly
  (NOp o1, NOp o2) -> if o1 == o2 then 0 else 1

  -- Wildcards always match
  (NWildcard, NWildcard) -> 0

  -- Application: compare function and argument
  (NApp f1 x1, NApp f2 x2) ->
    treeEditDistance f1 f2 + treeEditDistance x1 x2

  -- Lambda: compare bodies (ignore binding indices as they're normalized)
  (NLam _ b1, NLam _ b2) -> treeEditDistance b1 b2

  -- Negation: compare inner expression
  (NNeg e1, NNeg e2) -> treeEditDistance e1 e2

  -- If expressions: compare all three parts
  (NIf c1 t1' e1, NIf c2 t2' e2) ->
    treeEditDistance c1 c2 + treeEditDistance t1' t2' + treeEditDistance e1 e2

  -- Infix: operators must match, compare operands
  (NInfix op1 l1 r1, NInfix op2 l2 r2) ->
    (if op1 == op2 then 0 else 1) + treeEditDistance l1 l2 + treeEditDistance r1 r2

  -- Lists: use sequence edit distance
  (NList xs1, NList xs2) ->
    sequenceEditDistance xs1 xs2

  -- Tuples: use sequence edit distance
  (NTuple xs1, NTuple xs2) ->
    sequenceEditDistance xs1 xs2

  -- Case expressions: compare scrutinee and alternatives
  (NCase s1 alts1, NCase s2 alts2) ->
    treeEditDistance s1 s2 + alternativesEditDistance alts1 alts2

  -- Let expressions: compare bindings and body
  (NLet binds1 body1, NLet binds2 body2) ->
    bindingsEditDistance binds1 binds2 + treeEditDistance body1 body2

  -- Do notation: compare statement sequences
  (NDo stmts1, NDo stmts2) ->
    stmtsEditDistance stmts1 stmts2

  -- Records: compare fields
  (NRecord fields1, NRecord fields2) ->
    fieldsEditDistance fields1 fields2

  -- Sections: compare inner expressions and operators
  (NSection ml1 op1 mr1, NSection ml2 op2 mr2) ->
    (if op1 == op2 then 0 else 1) +
    maybeEditDistance ml1 ml2 +
    maybeEditDistance mr1 mr2

  -- Different node types: cost is based on size difference
  _ -> countNodesAST t1 + countNodesAST t2

-- | Edit distance for Maybe NormalizedAST
maybeEditDistance :: Maybe NormalizedAST -> Maybe NormalizedAST -> Int
maybeEditDistance Nothing Nothing = 0
maybeEditDistance (Just a) (Just b) = treeEditDistance a b
maybeEditDistance (Just a) Nothing = countNodesAST a
maybeEditDistance Nothing (Just b) = countNodesAST b

-- | Sequence edit distance using dynamic programming
sequenceEditDistance :: [NormalizedAST] -> [NormalizedAST] -> Int
sequenceEditDistance xs ys =
  let n = length xs
      m = length ys
  in if n == 0 then sum (map countNodesAST ys)
     else if m == 0 then sum (map countNodesAST xs)
     else seqDP xs ys n m

-- | Dynamic programming for sequence edit distance
seqDP :: [NormalizedAST] -> [NormalizedAST] -> Int -> Int -> Int
seqDP xs ys n m =
  let xArr = array (1, n) $ zip [1..] xs
      yArr = array (1, m) $ zip [1..] ys

      -- DP table
      dp :: Array (Int, Int) Int
      dp = array ((0, 0), (n, m))
        [((i, j), cell i j) | i <- [0..n], j <- [0..m]]

      cell 0 j = sum [countNodesAST (yArr ! k) | k <- [1..j]]
      cell i 0 = sum [countNodesAST (xArr ! k) | k <- [1..i]]
      cell i j =
        let replCost = treeEditDistance (xArr ! i) (yArr ! j)
            delCost = countNodesAST (xArr ! i)
            insCost = countNodesAST (yArr ! j)
        in minimum
          [ dp ! (i-1, j) + delCost      -- Delete
          , dp ! (i, j-1) + insCost      -- Insert
          , dp ! (i-1, j-1) + replCost   -- Replace/Match
          ]
  in dp ! (n, m)

-- | Edit distance for case alternatives
alternativesEditDistance :: [(NormalizedPattern, NormalizedAST)]
                         -> [(NormalizedPattern, NormalizedAST)] -> Int
alternativesEditDistance alts1 alts2 =
  let bodies1 = map snd alts1
      bodies2 = map snd alts2
  in sequenceEditDistance bodies1 bodies2 +
     patternsEditDistance (map fst alts1) (map fst alts2)

-- | Edit distance for patterns
patternsEditDistance :: [NormalizedPattern] -> [NormalizedPattern] -> Int
patternsEditDistance ps1 ps2 =
  let n = length ps1
      m = length ps2
  in if n == 0 then m
     else if m == 0 then n
     else patternSeqDP ps1 ps2 n m

-- | DP for pattern sequence edit distance
patternSeqDP :: [NormalizedPattern] -> [NormalizedPattern] -> Int -> Int -> Int
patternSeqDP ps1 ps2 n m =
  let pArr1 = array (1, n) $ zip [1..] ps1
      pArr2 = array (1, m) $ zip [1..] ps2

      dp :: Array (Int, Int) Int
      dp = array ((0, 0), (n, m))
        [((i, j), cell i j) | i <- [0..n], j <- [0..m]]

      cell 0 j = j
      cell i 0 = i
      cell i j =
        let cost = patternEditDistance (pArr1 ! i) (pArr2 ! j)
        in minimum
          [ dp ! (i-1, j) + 1
          , dp ! (i, j-1) + 1
          , dp ! (i-1, j-1) + cost
          ]
  in dp ! (n, m)

-- | Edit distance between two patterns
patternEditDistance :: NormalizedPattern -> NormalizedPattern -> Int
patternEditDistance p1 p2 = case (p1, p2) of
  (NPVar _, NPVar _) -> 0
  (NPLit l1, NPLit l2) -> if l1 == l2 then 0 else 1
  (NPCon n1 ps1, NPCon n2 ps2) ->
    (if n1 == n2 then 0 else 1) + patternsEditDistance ps1 ps2
  (NPWild, NPWild) -> 0
  (NPTuple ps1, NPTuple ps2) -> patternsEditDistance ps1 ps2
  (NPList ps1, NPList ps2) -> patternsEditDistance ps1 ps2
  (NPAs _ p1', NPAs _ p2') -> patternEditDistance p1' p2'
  (NPView e1 p1', NPView e2 p2') ->
    treeEditDistance e1 e2 + patternEditDistance p1' p2'
  _ -> 1  -- Different pattern types

-- | Edit distance for let bindings
bindingsEditDistance :: [(Int, NormalizedAST)] -> [(Int, NormalizedAST)] -> Int
bindingsEditDistance binds1 binds2 =
  sequenceEditDistance (map snd binds1) (map snd binds2)

-- | Edit distance for do-notation statements
stmtsEditDistance :: [NormalizedStmt] -> [NormalizedStmt] -> Int
stmtsEditDistance stmts1 stmts2 =
  let bodies1 = map stmtBody stmts1
      bodies2 = map stmtBody stmts2
  in sequenceEditDistance bodies1 bodies2
  where
    stmtBody (NStmtBind _ e) = e
    stmtBody (NStmtExpr e) = e
    stmtBody (NStmtLet binds) = NLet binds NWildcard

-- | Edit distance for record fields
fieldsEditDistance :: [(Text, NormalizedAST)] -> [(Text, NormalizedAST)] -> Int
fieldsEditDistance fields1 fields2 =
  let exprs1 = map snd fields1
      exprs2 = map snd fields2
      nameDiff = length $ filter (\(n1, _) -> n1 `notElem` map fst fields2) fields1
  in nameDiff + sequenceEditDistance exprs1 exprs2

--------------------------------------------------------------------------------
-- Similarity Computation
--------------------------------------------------------------------------------

-- | Compute comprehensive similarity between two normalized ASTs
computeSimilarity :: NormalizedAST -> NormalizedAST -> Similarity
computeSimilarity ast1 ast2 =
  let nodes1 = countNodesAST ast1
      nodes2 = countNodesAST ast2
      maxSize = max nodes1 nodes2
      editDist = computeTreeEditDistance ast1 ast2
      totalNodes = nodes1 + nodes2
      commonNodes = (totalNodes - editDist) `div` 2
      -- Similarity score: 1 - normalized edit distance
      score = if totalNodes == 0
              then 1.0
              else 1.0 - (fromIntegral editDist / fromIntegral totalNodes)
  in Similarity
    { simScore = max 0.0 $ min 1.0 score
    , simEditDist = editDist
    , simMaxSize = maxSize
    , simCommonNodes = max 0 commonNodes
    }

-- | Count nodes in AST
countNodesAST :: NormalizedAST -> Int
countNodesAST = \case
  NLit _ -> 1
  NVar _ -> 1
  NApp f x -> 1 + countNodesAST f + countNodesAST x
  NLam _ body -> 1 + countNodesAST body
  NCase scrut alts -> 1 + countNodesAST scrut + sum (map (countNodesAST . snd) alts)
  NIf c t e -> 1 + countNodesAST c + countNodesAST t + countNodesAST e
  NLet binds body -> 1 + countNodesAST body + sum (map (countNodesAST . snd) binds)
  NDo stmts -> 1 + sum (map countStmtNodesAST stmts)
  NInfix _ l r -> 1 + countNodesAST l + countNodesAST r
  NList xs -> 1 + sum (map countNodesAST xs)
  NTuple xs -> 1 + sum (map countNodesAST xs)
  NRecord fields -> 1 + sum (map (countNodesAST . snd) fields)
  NWildcard -> 1
  NCon _ -> 1
  NOp _ -> 1
  NNeg e -> 1 + countNodesAST e
  NSection ml _ mr -> 1 + maybe 0 countNodesAST ml + maybe 0 countNodesAST mr
  where
    countStmtNodesAST = \case
      NStmtBind _ expr -> 1 + countNodesAST expr
      NStmtExpr expr -> countNodesAST expr
      NStmtLet binds -> 1 + sum (map (countNodesAST . snd) binds)

-- | Check if similarity is above threshold
isAboveThreshold :: Double -> Similarity -> Bool
isAboveThreshold threshold sim = simScore sim >= threshold

-- | Get the similarity ratio as a percentage string
similarityRatio :: Similarity -> Text
similarityRatio sim =
  T.pack $ show (round (simScore sim * 100) :: Int) <> "%"

--------------------------------------------------------------------------------
-- Clustering
--------------------------------------------------------------------------------

-- | Cluster fingerprints by similarity
-- Uses single-linkage clustering with similarity threshold
clusterBySimilarity :: ClusterConfig -> [FunctionFingerprint] -> [[FunctionFingerprint]]
clusterBySimilarity ClusterConfig{..} fingerprints =
  let -- Filter by minimum node count
      filtered = filter (\fp -> fpNodeCount fp >= ccMinNodes) fingerprints

      -- Compute pairwise similarities above threshold
      pairs = findSimilarPairs ccSimilarityThreshold filtered

      -- Build clusters using union-find approach
      clusters = buildClusters filtered pairs

      -- Sort by cluster size (largest first) and limit
      sorted = take ccMaxClusters $
               sortBy (comparing (Down . length)) $
               filter (\c -> length c >= 2) clusters
  in sorted

-- | Find all pairs of fingerprints above similarity threshold
findSimilarPairs :: Double -> [FunctionFingerprint] -> [(Int, Int)]
findSimilarPairs threshold fps =
  let indexed = zip [0..] fps
  in [ (i, j)
     | (i, fp1) <- indexed
     , (j, fp2) <- indexed
     , i < j
     , quickSimilarityCheck fp1 fp2
     , let sim = computeSimilarity (fpNormalized fp1) (fpNormalized fp2)
     , simScore sim >= threshold
     ]

-- | Quick similarity check using hash and size for pre-filtering
quickSimilarityCheck :: FunctionFingerprint -> FunctionFingerprint -> Bool
quickSimilarityCheck fp1 fp2 =
  let sizeDiff = abs (fpNodeCount fp1 - fpNodeCount fp2)
      maxSize = max (fpNodeCount fp1) (fpNodeCount fp2)
      -- If sizes differ by more than 50%, skip detailed comparison
  in maxSize > 0 && fromIntegral sizeDiff / fromIntegral maxSize < 0.5

-- | Build clusters from similar pairs using union-find
buildClusters :: [FunctionFingerprint] -> [(Int, Int)] -> [[FunctionFingerprint]]
buildClusters fps pairs =
  let n = length fps
      fpsArr = array (0, max 0 (n-1)) $ zip [0..] fps

      -- Simple union-find with path compression
      initialParent = Map.fromList [(i, i) | i <- [0..n-1]]

      find :: Map Int Int -> Int -> (Int, Map Int Int)
      find parent x =
        let p = Map.findWithDefault x x parent
        in if p == x
           then (x, parent)
           else let (root, parent') = find parent p
                in (root, Map.insert x root parent')

      union :: Map Int Int -> Int -> Int -> Map Int Int
      union parent x y =
        let (rootX, parent1) = find parent x
            (rootY, parent2) = find parent1 y
        in if rootX == rootY
           then parent2
           else Map.insert rootX rootY parent2

      -- Apply all unions
      finalParent = foldl (\p (x, y) -> union p x y) initialParent pairs

      -- Group by root
      grouped = Map.fromListWith (++) $
        [ (root, [fpsArr ! i]) | i <- [0..n-1]
        , let (root, _) = find finalParent i ]

  in if n == 0 then [] else Map.elems grouped
