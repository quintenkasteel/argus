{-# LANGUAGE OverloadedStrings #-}

module SimilaritySpec (spec) where

import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Argus.Analysis.Fingerprint
import Argus.Analysis.Similarity
import Argus.Types (SrcSpan(..), mkSrcSpanRaw)

spec :: Spec
spec = do
  describe "Argus.Analysis.Similarity" $ do
    describe "computeTreeEditDistance" $ do
      it "returns 0 for identical ASTs" $ do
        let ast = NInfix "+" (NVar 0) (NLit (NLitInt 1))
        computeTreeEditDistance ast ast `shouldBe` 0

      it "returns 1 for single node difference" $ do
        let ast1 = NLit (NLitInt 1)
            ast2 = NLit (NLitInt 2)
        computeTreeEditDistance ast1 ast2 `shouldBe` 1

      it "returns edit distance for different structures" $ do
        let ast1 = NVar 0
            ast2 = NInfix "+" (NVar 0) (NVar 1)
        -- ast2 has more nodes
        computeTreeEditDistance ast1 ast2 `shouldSatisfy` (> 0)

      it "is symmetric" $ do
        let ast1 = NInfix "+" (NVar 0) (NVar 1)
            ast2 = NInfix "*" (NVar 0) (NVar 1)
        computeTreeEditDistance ast1 ast2 `shouldBe` computeTreeEditDistance ast2 ast1

    describe "computeSimilarity" $ do
      it "returns score of 1.0 for identical ASTs" $ do
        let ast = NInfix "+" (NVar 0) (NLit (NLitInt 1))
            sim = computeSimilarity ast ast
        simScore sim `shouldBe` 1.0

      it "returns score > 0 for similar ASTs" $ do
        let ast1 = NInfix "+" (NVar 0) (NLit (NLitInt 1))
            ast2 = NInfix "+" (NVar 0) (NLit (NLitInt 2))
            sim = computeSimilarity ast1 ast2
        simScore sim `shouldSatisfy` (> 0)

      it "returns lower score for different ASTs" $ do
        let ast1 = NLit (NLitInt 1)
            ast2 = NInfix "+" (NVar 0) (NInfix "*" (NVar 1) (NVar 2))
            sim = computeSimilarity ast1 ast2
        simScore sim `shouldSatisfy` (< 0.5)

      it "provides edit distance" $ do
        let ast1 = NVar 0
            ast2 = NApp (NVar 0) (NVar 1)
            sim = computeSimilarity ast1 ast2
        simEditDist sim `shouldSatisfy` (> 0)

      it "provides common nodes count" $ do
        let ast = NInfix "+" (NVar 0) (NVar 1)
            sim = computeSimilarity ast ast
        simCommonNodes sim `shouldSatisfy` (> 0)

    describe "isAboveThreshold" $ do
      it "returns True for similarity above threshold" $ do
        let sim = Similarity 0.9 2 10 8
        isAboveThreshold 0.8 sim `shouldBe` True

      it "returns False for similarity below threshold" $ do
        let sim = Similarity 0.7 5 10 6
        isAboveThreshold 0.8 sim `shouldBe` False

      it "returns True for exact threshold" $ do
        let sim = Similarity 0.8 3 10 7
        isAboveThreshold 0.8 sim `shouldBe` True

    describe "similarityRatio" $ do
      it "formats as percentage" $ do
        let sim = Similarity 0.85 2 10 8
        similarityRatio sim `shouldBe` "85%"

      it "rounds to nearest integer" $ do
        let sim = Similarity 0.876 2 10 8
        similarityRatio sim `shouldBe` "88%"

    describe "defaultClusterConfig" $ do
      it "has sensible threshold" $ do
        ccSimilarityThreshold defaultClusterConfig `shouldBe` 0.8

      it "has sensible minimum nodes" $ do
        ccMinNodes defaultClusterConfig `shouldBe` 10

      it "has sensible max clusters" $ do
        ccMaxClusters defaultClusterConfig `shouldBe` 100

    describe "clusterBySimilarity" $ do
      it "returns empty for empty input" $ do
        let clusters = clusterBySimilarity defaultClusterConfig []
        clusters `shouldBe` []

      it "returns empty for single fingerprint" $ do
        let fp = fingerprintFunction "foo" "foo x = x + 1" testSpan
            config = defaultClusterConfig { ccMinNodes = 1 }
            clusters = clusterBySimilarity config [fp]
        clusters `shouldBe` []  -- Need at least 2 for a cluster

      it "clusters identical functions" $ do
        let fp1 = fingerprintFunction "foo" "foo x = x + 1 + 2 + 3 + 4 + 5" testSpan
            fp2 = fingerprintFunction "bar" "bar y = y + 1 + 2 + 3 + 4 + 5" testSpan
            config = defaultClusterConfig { ccMinNodes = 1 }
            clusters = clusterBySimilarity config [fp1, fp2]
        -- Should have exactly one cluster with both functions
        length clusters `shouldBe` 1
        length (head clusters) `shouldBe` 2

      it "does not cluster very different functions" $ do
        let fp1 = fingerprintFunction "foo" "foo x = x" testSpan
            fp2 = fingerprintFunction "bar" "bar x y z = case x of { Just a -> a + y * z; Nothing -> z }" testSpan
            config = defaultClusterConfig { ccMinNodes = 1 }
            clusters = clusterBySimilarity config [fp1, fp2]
        -- Should not cluster these
        null clusters `shouldBe` True

    describe "Property-based tests" $ do
      prop "edit distance is non-negative" $ \(seed :: Int) ->
        let ast1 = NLit (NLitInt $ fromIntegral seed)
            ast2 = NLit (NLitInt $ fromIntegral (seed + 1))
        in computeTreeEditDistance ast1 ast2 >= 0

      prop "similarity score is between 0 and 1" $ \(seed :: Int) ->
        let ast1 = NLit (NLitInt $ fromIntegral seed)
            ast2 = NInfix "+" (NVar 0) (NLit (NLitInt $ fromIntegral seed))
            sim = computeSimilarity ast1 ast2
        in simScore sim >= 0 && simScore sim <= 1

      prop "edit distance of identical ASTs is 0" $ \(seed :: Int) ->
        let ast = NLit (NLitInt $ fromIntegral seed)
        in computeTreeEditDistance ast ast == 0

testSpan :: SrcSpan
testSpan = mkSrcSpanRaw "test.hs" 1 1 10 1
