{-# LANGUAGE OverloadedStrings #-}

module PragmasSpec (spec) where

import Data.Set qualified as Set
import Test.Hspec

import Argus.Analysis.Syntactic (PragmaInfo(..))
import Argus.Rules.Pragmas
import Argus.Types (SrcSpan(..), noSrcSpan)

spec :: Spec
spec = do
  describe "detectUsedExtensions" $ do
    describe "BangPatterns" $ do
      it "detects bang patterns in function arguments" $ do
        let source = "foo !x = x + 1"
        Set.member "BangPatterns" (detectUsedExtensions source) `shouldBe` True

      it "does not false positive on inequality" $ do
        let source = "foo = x != y"
        Set.member "BangPatterns" (detectUsedExtensions source) `shouldBe` False

    describe "LambdaCase" $ do
      it "detects \\case" $ do
        let source = "foo = \\case Just x -> x; Nothing -> 0"
        Set.member "LambdaCase" (detectUsedExtensions source) `shouldBe` True

      it "detects \\ case with space" $ do
        let source = "foo = \\ case Just x -> x"
        Set.member "LambdaCase" (detectUsedExtensions source) `shouldBe` True

    describe "MultiWayIf" $ do
      it "detects multi-way if" $ do
        let source = "foo x = if | x > 0 -> 1 | otherwise -> 0"
        Set.member "MultiWayIf" (detectUsedExtensions source) `shouldBe` True

    describe "TupleSections" $ do
      it "detects tuple sections with leading comma" $ do
        let source = "foo = (,x)"
        Set.member "TupleSections" (detectUsedExtensions source) `shouldBe` True

      it "detects tuple sections with trailing comma" $ do
        let source = "foo = (x,)"
        Set.member "TupleSections" (detectUsedExtensions source) `shouldBe` True

    describe "NumericUnderscores" $ do
      it "detects numeric underscores" $ do
        let source = "million = 1_000_000"
        Set.member "NumericUnderscores" (detectUsedExtensions source) `shouldBe` True

      it "does not false positive on identifiers with underscores" $ do
        let source = "foo_bar = 123"
        Set.member "NumericUnderscores" (detectUsedExtensions source) `shouldBe` False

    describe "BinaryLiterals" $ do
      it "detects binary literals" $ do
        let source = "flags = 0b1010"
        Set.member "BinaryLiterals" (detectUsedExtensions source) `shouldBe` True

    describe "TypeApplications" $ do
      it "detects type applications" $ do
        let source = "foo = read @Int \"123\""
        Set.member "TypeApplications" (detectUsedExtensions source) `shouldBe` True

    describe "RecordWildCards" $ do
      it "detects record wildcards" $ do
        let source = "foo Person{..} = name"
        Set.member "RecordWildCards" (detectUsedExtensions source) `shouldBe` True

    describe "UnicodeSyntax" $ do
      it "detects unicode arrow" $ do
        let source = "foo :: Int → Int"
        Set.member "UnicodeSyntax" (detectUsedExtensions source) `shouldBe` True

      it "detects unicode forall" $ do
        let source = "foo :: ∀ a. a → a"
        Set.member "UnicodeSyntax" (detectUsedExtensions source) `shouldBe` True

    describe "RecursiveDo" $ do
      it "detects mdo" $ do
        let source = "foo = mdo x <- action; return x"
        Set.member "RecursiveDo" (detectUsedExtensions source) `shouldBe` True

    describe "Arrows" $ do
      it "detects proc syntax" $ do
        let source = "circuit = proc x -> returnA -< x"
        Set.member "Arrows" (detectUsedExtensions source) `shouldBe` True

  describe "findUnusedPragmas" $ do
    let mkPragma ext = PragmaInfo noSrcSpan ext True

    it "finds unused pragmas" $ do
      let pragmas = [mkPragma "LambdaCase", mkPragma "BangPatterns"]
          usedExts = Set.singleton "BangPatterns"
          unused = findUnusedPragmas pragmas usedExts
      length unused `shouldBe` 1
      piExtension (head unused) `shouldBe` "LambdaCase"

    it "returns empty for all used pragmas" $ do
      let pragmas = [mkPragma "LambdaCase"]
          usedExts = Set.singleton "LambdaCase"
      findUnusedPragmas pragmas usedExts `shouldBe` []

    it "ignores disabled pragmas" $ do
      let pragmas = [PragmaInfo noSrcSpan "LambdaCase" False]
          usedExts = Set.empty
      findUnusedPragmas pragmas usedExts `shouldBe` []

    it "ignores extensions we cannot check" $ do
      let pragmas = [mkPragma "GADTs"]  -- Not in checkableExtensions
          usedExts = Set.empty
      findUnusedPragmas pragmas usedExts `shouldBe` []

  describe "checkPragmas" $ do
    let mkPragma ext = PragmaInfo noSrcSpan ext True

    it "generates diagnostics for unused pragmas" $ do
      let pragmas = [mkPragma "LambdaCase"]
          source = "foo x = x + 1"  -- No lambda case usage
          diags = checkPragmas "Test.hs" source pragmas
      length diags `shouldBe` 1

    it "generates no diagnostics when pragma is used" $ do
      let pragmas = [mkPragma "LambdaCase"]
          source = "foo = \\case Just x -> x"
          diags = checkPragmas "Test.hs" source pragmas
      diags `shouldBe` []

    it "handles multiple pragmas" $ do
      let pragmas = [mkPragma "LambdaCase", mkPragma "BangPatterns", mkPragma "TupleSections"]
          source = "foo !x = (,x)"  -- Uses BangPatterns and TupleSections, not LambdaCase
          diags = checkPragmas "Test.hs" source pragmas
      length diags `shouldBe` 1
      -- The unused one should be LambdaCase
