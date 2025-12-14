{-# LANGUAGE OverloadedStrings #-}

module FingerprintSpec (spec) where

import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Argus.Analysis.Fingerprint
import Argus.Types (SrcSpan(..), mkSrcSpanRaw)

spec :: Spec
spec = do
  describe "Argus.Analysis.Fingerprint" $ do
    describe "fingerprintFunction" $ do
      it "creates fingerprint with correct name" $ do
        let fp = fingerprintFunction "foo" "foo x = x + 1" testSpan
        fpName fp `shouldBe` "foo"

      it "creates fingerprint with correct line count" $ do
        let code = T.unlines ["foo x = x + 1", "  where", "    helper = 2"]
            fp = fingerprintFunction "foo" code testSpan
        fpLineCount fp `shouldBe` 3

      it "computes non-zero hash for non-empty code" $ do
        let fp = fingerprintFunction "foo" "foo x = x + 1" testSpan
        fpHash fp `shouldSatisfy` (/= 0)

      it "computes same hash for alpha-equivalent functions" $ do
        let fp1 = fingerprintFunction "foo" "foo x = x + 1" testSpan
            fp2 = fingerprintFunction "bar" "bar y = y + 1" testSpan
        -- After alpha-renaming, these should have the same structure
        fpHash fp1 `shouldBe` fpHash fp2

      it "computes different hash for different structures" $ do
        let fp1 = fingerprintFunction "foo" "foo x = x + 1" testSpan
            fp2 = fingerprintFunction "bar" "bar x = x * 2" testSpan
        fpHash fp1 `shouldSatisfy` (/= fpHash fp2)

    describe "normalizeFromText" $ do
      it "normalizes simple expression" $ do
        let norm = normalizeFromText "x + 1"
        norm `shouldSatisfy` \case
          NInfix "+" _ _ -> True
          _ -> False

      it "normalizes lambda expression" $ do
        let norm = normalizeFromText "\\x -> x + 1"
        norm `shouldSatisfy` \case
          NLam _ _ -> True
          _ -> False

      it "normalizes if expression" $ do
        let norm = normalizeFromText "if x > 0 then x else 0"
        norm `shouldSatisfy` \case
          NIf _ _ _ -> True
          _ -> False

      it "normalizes application" $ do
        let norm = normalizeFromText "foo bar baz"
        norm `shouldSatisfy` \case
          NApp _ _ -> True
          _ -> False

      it "normalizes constructor" $ do
        let norm = normalizeFromText "Just"
        norm `shouldSatisfy` \case
          NCon "Just" -> True
          _ -> False

    describe "computeHash" $ do
      it "produces consistent hash for same AST" $ do
        let ast = NInfix "+" (NVar 0) (NLit (NLitInt 1))
        computeHash ast `shouldBe` computeHash ast

      it "produces different hash for different literals" $ do
        let ast1 = NLit (NLitInt 1)
            ast2 = NLit (NLitInt 2)
        computeHash ast1 `shouldSatisfy` (/= computeHash ast2)

      it "produces different hash for different operators" $ do
        let ast1 = NInfix "+" (NVar 0) (NVar 1)
            ast2 = NInfix "*" (NVar 0) (NVar 1)
        computeHash ast1 `shouldSatisfy` (/= computeHash ast2)

      it "handles nested structures" $ do
        let ast = NApp (NApp (NVar 0) (NVar 1)) (NVar 2)
        computeHash ast `shouldSatisfy` (> 0)

    describe "countNodes" $ do
      it "counts single literal as 1" $ do
        let ast = NLit (NLitInt 42)
        countNodes ast `shouldBe` 1

      it "counts variable as 1" $ do
        let ast = NVar 0
        countNodes ast `shouldBe` 1

      it "counts application correctly" $ do
        let ast = NApp (NVar 0) (NVar 1)
        countNodes ast `shouldBe` 3  -- App + 2 vars

      it "counts nested structure correctly" $ do
        let ast = NInfix "+" (NVar 0) (NInfix "*" (NVar 1) (NVar 2))
        countNodes ast `shouldBe` 5  -- 2 infix + 3 vars

    describe "computeDepth" $ do
      it "returns 1 for single node" $ do
        let ast = NLit (NLitInt 42)
        computeDepth ast `shouldBe` 1

      it "returns 2 for simple application" $ do
        let ast = NApp (NVar 0) (NLit (NLitInt 1))
        computeDepth ast `shouldBe` 2

      it "returns correct depth for nested structure" $ do
        let ast = NApp (NApp (NApp (NVar 0) (NVar 1)) (NVar 2)) (NVar 3)
        computeDepth ast `shouldBe` 4

    describe "fingerprintEqual" $ do
      it "returns True for identical fingerprints" $ do
        let fp = fingerprintFunction "foo" "foo x = x + 1" testSpan
        fingerprintEqual fp fp `shouldBe` True

      it "returns True for alpha-equivalent code" $ do
        let fp1 = fingerprintFunction "foo" "foo x = x + 1" testSpan
            fp2 = fingerprintFunction "bar" "bar y = y + 1" testSpan
        fingerprintEqual fp1 fp2 `shouldBe` True

      it "returns False for different code" $ do
        let fp1 = fingerprintFunction "foo" "foo x = x + 1" testSpan
            fp2 = fingerprintFunction "bar" "bar x = x * 2" testSpan
        fingerprintEqual fp1 fp2 `shouldBe` False

    describe "fingerprintSimilar" $ do
      it "returns True for identical code at high threshold" $ do
        let fp1 = fingerprintFunction "foo" "foo x = x + 1" testSpan
            fp2 = fingerprintFunction "bar" "bar y = y + 1" testSpan
        fingerprintSimilar 0.9 fp1 fp2 `shouldBe` True

      it "returns False for very different code at high threshold" $ do
        let fp1 = fingerprintFunction "foo" "foo x = x + 1" testSpan
            fp2 = fingerprintFunction "bar" "bar x y z = case x of { Just a -> a; Nothing -> 0 }" testSpan
        fingerprintSimilar 0.9 fp1 fp2 `shouldBe` False

    describe "Property-based tests" $ do
      prop "hash is deterministic" $ \(seed :: Int) ->
        let code = "foo x = x + " <> T.pack (show seed)
            fp1 = fingerprintFunction "foo" code testSpan
            fp2 = fingerprintFunction "foo" code testSpan
        in fpHash fp1 == fpHash fp2

      prop "node count is always positive" $ \(seed :: Int) ->
        let code = "foo x = x + " <> T.pack (show seed)
            fp = fingerprintFunction "foo" code testSpan
        in fpNodeCount fp > 0

      prop "depth is always positive" $ \(seed :: Int) ->
        let code = "foo x = x + " <> T.pack (show seed)
            fp = fingerprintFunction "foo" code testSpan
        in fpDepth fp > 0

testSpan :: SrcSpan
testSpan = mkSrcSpanRaw "test.hs" 1 1 10 1
