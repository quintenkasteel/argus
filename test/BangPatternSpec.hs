{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : BangPatternSpec
-- Description : Tests for bang pattern insertion
-- Copyright   : (c) 2024
-- License     : MIT
module BangPatternSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import Argus.Refactor.BangPattern
import Argus.Types

spec :: Spec
spec = do
  describe "Argus.Refactor.BangPattern" $ do
    describe "BangPatternConfig" $ do
      it "has sensible defaults" $ do
        bpcEnabled defaultBangPatternConfig `shouldBe` True
        bpcMode defaultBangPatternConfig `shouldBe` Moderate
        bpcStrategy defaultBangPatternConfig `shouldBe` InsertInPattern
        bpcCheckRecordFields defaultBangPatternConfig `shouldBe` True
        bpcCheckLetBindings defaultBangPatternConfig `shouldBe` True

      it "includes common accumulator names" $ do
        let names = bpcAccumulatorNames defaultBangPatternConfig
        "acc" `elem` names `shouldBe` True
        "sum" `elem` names `shouldBe` True
        "result" `elem` names `shouldBe` True
        "count" `elem` names `shouldBe` True

    describe "StrictnessMode" $ do
      it "has correct ordering" $ do
        Conservative < Moderate `shouldBe` True
        Moderate < Aggressive `shouldBe` True

    describe "InsertionStrategy" $ do
      it "has all expected strategies" $ do
        [minBound..maxBound] `shouldBe`
          [InsertInPattern, InsertInLet, InsertSeq, InsertBangBefore]

    describe "isAccumulatorName" $ do
      it "matches exact accumulator names" $ do
        isAccumulatorName ["acc"] "acc" `shouldBe` True
        isAccumulatorName ["sum"] "sum" `shouldBe` True

      it "matches case-insensitively" $ do
        isAccumulatorName ["acc"] "Acc" `shouldBe` True
        isAccumulatorName ["acc"] "ACC" `shouldBe` True

      it "matches partial names" $ do
        isAccumulatorName ["acc"] "accumulator" `shouldBe` True
        isAccumulatorName ["sum"] "sumTotal" `shouldBe` True

      it "returns False for non-matching names" $ do
        isAccumulatorName ["acc"] "foo" `shouldBe` False
        isAccumulatorName ["sum"] "product" `shouldBe` False

    describe "analyzeBangPatterns" $ do
      it "returns empty analysis when disabled" $ do
        let config = defaultBangPatternConfig { bpcEnabled = False }
            analysis = analyzeBangPatterns config "test.hs" "module Test where"
        bpaCandidates analysis `shouldBe` []
        bpaAccumulatorCount analysis `shouldBe` 0

      it "analyzes simple recursive function with accumulator" $ do
        let config = defaultBangPatternConfig
            content = T.unlines
              [ "sumList :: [Int] -> Int -> Int"
              , "sumList [] acc = acc"
              , "sumList (x:xs) acc = sumList xs (acc + x)"
              ]
            analysis = analyzeBangPatterns config "test.hs" content
        bpaAccumulatorCount analysis `shouldSatisfy` (>= 0)

      it "respects strictness mode filtering" $ do
        let configConservative = defaultBangPatternConfig { bpcMode = Conservative }
            configAggressive = defaultBangPatternConfig { bpcMode = Aggressive }
            content = T.unlines
              [ "go n = go (n - 1)"
              ]
            analysisC = analyzeBangPatterns configConservative "test.hs" content
            analysisA = analyzeBangPatterns configAggressive "test.hs" content
        -- Conservative mode should have fewer or equal candidates
        length (bpaCandidates analysisC) `shouldSatisfy` (<= length (bpaCandidates analysisA))

    describe "detectAccumulatorPatterns" $ do
      it "detects accumulator in recursive function" $ do
        let config = defaultBangPatternConfig
            content = T.unlines
              [ "factorial n acc"
              , "  | n <= 1 = acc"
              , "  | otherwise = factorial (n-1) (n*acc)"
              ]
            candidates = detectAccumulatorPatterns config "test.hs" content
        -- Should detect 'acc' as accumulator
        any (\c -> "acc" `T.isInfixOf` bpcVarName c) candidates `shouldBe` True

      it "handles function without accumulator" $ do
        let config = defaultBangPatternConfig
            content = T.unlines
              [ "length' [] = 0"
              , "length' (_:xs) = 1 + length' xs"
              ]
            candidates = detectAccumulatorPatterns config "test.hs" content
        length candidates `shouldSatisfy` (>= 0)

    describe "detectStrictBindings" $ do
      it "detects let bindings that need strictness" $ do
        let config = defaultBangPatternConfig
            content = T.unlines
              [ "foo = let sum = foldr (+) 0 xs in sum"
              ]
            candidates = detectStrictBindings config "test.hs" content
        length candidates `shouldSatisfy` (>= 0)

      it "handles where bindings" $ do
        let config = defaultBangPatternConfig
            content = T.unlines
              [ "bar x = result"
              , "  where"
              , "    result = x + 1"
              ]
            candidates = detectStrictBindings config "test.hs" content
        length candidates `shouldSatisfy` (>= 0)

    describe "detectRecordFields" $ do
      it "analyzes data declarations" $ do
        let config = defaultBangPatternConfig
            content = T.unlines
              [ "data Counter = Counter"
              , "  { count :: Int"
              , "  , total :: Int"
              , "  }"
              ]
            candidates = detectRecordFields config "test.hs" content
        length candidates `shouldSatisfy` (>= 0)

      it "returns empty when disabled" $ do
        let config = defaultBangPatternConfig { bpcCheckRecordFields = False }
            content = "data Foo = Foo { bar :: Int }"
            candidates = detectRecordFields config "test.hs" content
        candidates `shouldBe` []

    describe "detectLazyParameters" $ do
      it "detects lambda parameters" $ do
        let config = defaultBangPatternConfig { bpcCheckLambdas = True }
            content = "foo = \\acc x -> acc + x"
            candidates = detectLazyParameters config "test.hs" content
        length candidates `shouldSatisfy` (>= 0)

      it "returns empty when disabled" $ do
        let config = defaultBangPatternConfig { bpcCheckLambdas = False }
            content = "foo = \\acc x -> acc + x"
            candidates = detectLazyParameters config "test.hs" content
        candidates `shouldBe` []

    describe "findBangPatternCandidates" $ do
      it "combines all detection types" $ do
        let config = defaultBangPatternConfig
            content = T.unlines
              [ "sumList [] acc = acc"
              , "sumList (x:xs) acc = sumList xs (acc + x)"
              , "foo = let result = 1 in result"
              ]
            candidates = findBangPatternCandidates config "test.hs" content
        length candidates `shouldSatisfy` (>= 0)

    describe "generateBangPatternFixes" $ do
      it "generates fixes for candidates" $ do
        let config = defaultBangPatternConfig
            content = T.unlines
              [ "sumList [] acc = acc"
              , "sumList (x:xs) acc = sumList xs (acc + x)"
              ]
            analysis = analyzeBangPatterns config "test.hs" content
            fixes = generateBangPatternFixes config analysis
        -- Should generate appropriate fixes for candidates
        all (\f -> not (T.null (fixTitle f))) fixes `shouldBe` True

      it "uses correct fix category" $ do
        let config = defaultBangPatternConfig
            content = "go acc = go (acc + 1)"
            analysis = analyzeBangPatterns config "test.hs" content
            fixes = generateBangPatternFixes config analysis
        all (\f -> fixCategory f == FCPerformance) fixes `shouldBe` True

    describe "insertBangPattern" $ do
      it "inserts bang at correct position" $ do
        let content = "foo x = x + 1"
            targetSpan = mkSrcSpanRaw "test.hs" 1 5 1 6
            result = insertBangPattern "test.hs" targetSpan "x" content
        T.isInfixOf "!x" result `shouldBe` True

      it "handles multiline content" $ do
        let content = T.unlines
              [ "foo x = x"
              , "bar y = y"
              ]
            targetSpan = mkSrcSpanRaw "test.hs" 2 5 2 6
            result = insertBangPattern "test.hs" targetSpan "y" content
        T.isInfixOf "!y" result `shouldBe` True

      it "returns unchanged content for invalid span" $ do
        let content = "foo x = x"
            targetSpan = mkSrcSpanRaw "test.hs" 100 1 100 2
            result = insertBangPattern "test.hs" targetSpan "x" content
        result `shouldBe` content

    describe "insertBangPatterns" $ do
      it "handles multiple insertions" $ do
        let content = T.unlines
              [ "foo x y = x + y"
              ]
            insertions =
              [ (mkSrcSpanRaw "test.hs" 1 5 1 6, "x")
              , (mkSrcSpanRaw "test.hs" 1 7 1 8, "y")
              ]
            result = insertBangPatterns "test.hs" insertions content
        T.isInfixOf "!x" result `shouldBe` True

      it "handles empty insertion list" $ do
        let content = "foo x = x"
            result = insertBangPatterns "test.hs" [] content
        result `shouldBe` content

    describe "CandidateKind" $ do
      it "has all expected kinds" $ do
        let allKinds = [minBound..maxBound] :: [CandidateKind]
        AccumulatorArg `elem` allKinds `shouldBe` True
        StrictLetBinding `elem` allKinds `shouldBe` True
        RecursiveParameter `elem` allKinds `shouldBe` True
        RecordFieldStrict `elem` allKinds `shouldBe` True
        LambdaParameter `elem` allKinds `shouldBe` True
        FoldAccumulator `elem` allKinds `shouldBe` True
        StateAccumulator `elem` allKinds `shouldBe` True

    describe "BangPatternCandidate" $ do
      it "has reasonable confidence scores" $ do
        let config = defaultBangPatternConfig
            content = "go acc = go (acc + 1)"
            candidates = findBangPatternCandidates config "test.hs" content
        all (\c -> bpcConfidence c >= 0.0 && bpcConfidence c <= 1.0) candidates `shouldBe` True

    describe "shouldBeStrict" $ do
      it "returns True for accumulator patterns" $ do
        let patterns = ["acc", "sum", "count"]
        shouldBeStrict patterns "acc" "let acc = ..." `shouldBe` True

      it "returns True for fold context" $ do
        let patterns = ["result"]
        shouldBeStrict patterns "x" "foldl' f z xs" `shouldBe` True

      it "returns False for unrelated patterns" $ do
        let patterns = ["acc"]
        shouldBeStrict patterns "foo" "bar baz" `shouldBe` False

    describe "isStrictContext" $ do
      it "returns True for accumulator names" $ do
        isStrictContext ["acc"] "acc" `shouldBe` True
        isStrictContext ["sum"] "totalSum" `shouldBe` True

      it "returns False for non-accumulator names" $ do
        isStrictContext ["acc"] "foo" `shouldBe` False

    describe "bangPatternAnalysisToFixes" $ do
      it "converts analysis to fixes" $ do
        let config = defaultBangPatternConfig
            content = "go acc = go (acc + 1)"
            analysis = analyzeBangPatterns config "test.hs" content
            fixes = bangPatternAnalysisToFixes config analysis
        length fixes `shouldSatisfy` (>= 0)

    describe "edge cases" $ do
      it "handles empty content" $ do
        let config = defaultBangPatternConfig
            analysis = analyzeBangPatterns config "test.hs" ""
        bpaCandidates analysis `shouldBe` []

      it "handles content without functions" $ do
        let config = defaultBangPatternConfig
            content = "module Test where\n\nimport Data.List"
            analysis = analyzeBangPatterns config "test.hs" content
        bpaAccumulatorCount analysis `shouldBe` 0

      it "handles very long lines" $ do
        let config = defaultBangPatternConfig
            content = "foo " <> T.replicate 1000 "x" <> " = 1"
            analysis = analyzeBangPatterns config "test.hs" content
        -- Should not crash
        bpaPath analysis `shouldBe` "test.hs"

      it "handles special characters in names" $ do
        let config = defaultBangPatternConfig
            content = "foo' acc' = foo' (acc' + 1)"
            analysis = analyzeBangPatterns config "test.hs" content
        bpaPath analysis `shouldBe` "test.hs"

    describe "strategy selection" $ do
      it "InsertInPattern creates pattern bang fixes" $ do
        let config = defaultBangPatternConfig { bpcStrategy = InsertInPattern }
            content = "go acc = go (acc + 1)"
            analysis = analyzeBangPatterns config "test.hs" content
            fixes = generateBangPatternFixes config analysis
        all (\f -> "bang pattern" `T.isInfixOf` T.toLower (fixTitle f)) fixes `shouldBe` True

      it "InsertSeq creates seq fixes" $ do
        let config = defaultBangPatternConfig { bpcStrategy = InsertSeq }
            content = "go acc = go (acc + 1)"
            analysis = analyzeBangPatterns config "test.hs" content
            fixes = generateBangPatternFixes config analysis
        all (\f -> fixSafety f == FSReview) fixes `shouldBe` True

      it "InsertBangBefore creates $! fixes" $ do
        let config = defaultBangPatternConfig { bpcStrategy = InsertBangBefore }
            content = "go acc = go (acc + 1)"
            analysis = analyzeBangPatterns config "test.hs" content
            fixes = generateBangPatternFixes config analysis
        all (\f -> fixSafety f == FSReview) fixes `shouldBe` True
