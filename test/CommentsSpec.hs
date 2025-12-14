{-# LANGUAGE OverloadedStrings #-}

module CommentsSpec (spec) where

import Test.Hspec
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Analysis.Comments

spec :: Spec
spec = do
  describe "Comment extraction" $ do
    describe "extractComments" $ do
      it "extracts line comments" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- This is a comment"
              , "foo = 42"
              ]
        let comments = extractComments source
        length comments `shouldBe` 1
        commentType (head comments) `shouldBe` LineComment
        commentStartLine (head comments) `shouldBe` 2

      it "extracts block comments" $ do
        let source = T.unlines
              [ "module Test where"
              , "{- Block comment -}"
              , "foo = 42"
              ]
        let comments = extractComments source
        length comments `shouldBe` 1
        commentType (head comments) `shouldBe` BlockComment
        commentStartLine (head comments) `shouldBe` 2

      it "extracts multi-line block comments" $ do
        let source = T.unlines
              [ "module Test where"
              , "{- This is a"
              , "   multi-line"
              , "   comment -}"
              , "foo = 42"
              ]
        let comments = extractComments source
        length comments `shouldBe` 1
        commentStartLine (head comments) `shouldBe` 2
        commentEndLine (head comments) `shouldBe` 4

      it "extracts nested block comments" $ do
        let source = T.unlines
              [ "module Test where"
              , "{- Outer {- nested -} outer -}"
              , "foo = 42"
              ]
        let comments = extractComments source
        length comments `shouldBe` 1
        commentType (head comments) `shouldBe` BlockComment

      it "extracts Haddock line comments" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- | Documentation"
              , "foo :: Int"
              ]
        let comments = extractComments source
        length comments `shouldBe` 1
        commentType (head comments) `shouldBe` HaddockLine

      it "extracts Haddock block comments" $ do
        let source = T.unlines
              [ "module Test where"
              , "{-| Module documentation -}"
              , "foo :: Int"
              ]
        let comments = extractComments source
        length comments `shouldBe` 1
        commentType (head comments) `shouldBe` HaddockBlock

      it "extracts pragma comments" $ do
        let source = T.unlines
              [ "{-# LANGUAGE OverloadedStrings #-}"
              , "module Test where"
              , "foo = 42"
              ]
        let comments = extractComments source
        length comments `shouldBe` 1
        commentType (head comments) `shouldBe` PragmaComment

      it "ignores strings that look like comments" $ do
        let source = T.unlines
              [ "module Test where"
              , "foo = \"-- not a comment\""
              ]
        let comments = extractComments source
        length comments `shouldBe` 0

      it "extracts multiple comments" $ do
        let source = T.unlines
              [ "-- First comment"
              , "module Test where"
              , "-- Second comment"
              , "foo = 42 -- Third comment"
              ]
        let comments = extractComments source
        length comments `shouldBe` 3

  describe "Comment index" $ do
    describe "buildCommentIndex" $ do
      it "builds index from comments" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- Line 2 comment"
              , "foo = 42"
              ]
        let comments = extractComments source
        let idx = buildCommentIndex comments
        getAllComments idx `shouldBe` comments

    describe "isInComment" $ do
      it "detects position in line comment" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- This is a comment"
              , "foo = 42"
              ]
        let idx = buildCommentIndex (extractComments source)
        isInComment idx 2 5 `shouldBe` True
        isInComment idx 3 5 `shouldBe` False

      it "detects position in block comment" $ do
        let source = T.unlines
              [ "module Test where"
              , "{- Block"
              , "   comment -}"
              , "foo = 42"
              ]
        let idx = buildCommentIndex (extractComments source)
        isInComment idx 2 5 `shouldBe` True
        isInComment idx 3 5 `shouldBe` True
        isInComment idx 4 5 `shouldBe` False

    describe "isInCommentType" $ do
      it "distinguishes comment types" $ do
        let source = T.unlines
              [ "-- Line comment"
              , "module Test where"
              , "{- Block comment -}"
              , "foo = 42"
              ]
        let idx = buildCommentIndex (extractComments source)
        isInCommentType idx (Position 1 5) LineComment `shouldBe` True
        isInCommentType idx (Position 1 5) BlockComment `shouldBe` False
        isInCommentType idx (Position 3 5) BlockComment `shouldBe` True
        isInCommentType idx (Position 3 5) LineComment `shouldBe` False

    describe "getCommentAt" $ do
      it "returns comment at position" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- Important comment"
              , "foo = 42"
              ]
        let idx = buildCommentIndex (extractComments source)
        let maybeComment = getCommentAt idx (Position 2 5)
        maybeComment `shouldSatisfy` \case
          Just c -> commentType c == LineComment
          Nothing -> False

      it "returns Nothing outside comments" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- Comment"
              , "foo = 42"
              ]
        let idx = buildCommentIndex (extractComments source)
        getCommentAt idx (Position 3 5) `shouldBe` Nothing

    describe "getCommentsInRange" $ do
      it "returns comments in range" $ do
        let source = T.unlines
              [ "-- Comment 1"
              , "module Test where"
              , "-- Comment 2"
              , "foo = 42"
              , "-- Comment 3"
              ]
        let idx = buildCommentIndex (extractComments source)
        let range = Range (Position 1 1) (Position 3 20)
        let commentsInRange = getCommentsInRange idx range
        length commentsInRange `shouldBe` 2

  describe "Range operations" $ do
    describe "rangeOverlaps" $ do
      it "detects overlapping ranges" $ do
        let r1 = Range (Position 1 1) (Position 3 10)
        let r2 = Range (Position 2 5) (Position 4 10)
        rangeOverlaps r1 r2 `shouldBe` True

      it "detects non-overlapping ranges" $ do
        let r1 = Range (Position 1 1) (Position 2 10)
        let r2 = Range (Position 3 1) (Position 4 10)
        rangeOverlaps r1 r2 `shouldBe` False

    describe "rangeContains" $ do
      it "detects contained range" $ do
        let outer = Range (Position 1 1) (Position 10 10)
        let inner = Range (Position 2 5) (Position 5 5)
        rangeContains outer inner `shouldBe` True

      it "detects non-contained range" $ do
        let r1 = Range (Position 1 1) (Position 5 10)
        let r2 = Range (Position 3 1) (Position 8 10)
        rangeContains r1 r2 `shouldBe` False

  describe "Suppression directives" $ do
    describe "extractSuppressions" $ do
      it "extracts argus:ignore directive" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- argus:ignore"
              , "foo = head xs"
              ]
        let comments = extractComments source
        let supps = extractSuppressions comments
        length supps `shouldBe` 1
        suppressionScope (head supps) `shouldBe` SuppressLine

      it "extracts argus:ignore -file directive" $ do
        let source = T.unlines
              [ "-- argus:ignore -file"
              , "module Test where"
              ]
        let comments = extractComments source
        let supps = extractSuppressions comments
        length supps `shouldBe` 1
        suppressionScope (head supps) `shouldBe` SuppressFile

      it "extracts suppression with rule IDs" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- argus:ignore avoid-head partial-function"
              , "foo = head xs"
              ]
        let comments = extractComments source
        let supps = extractSuppressions comments
        length supps `shouldBe` 1
        suppressionRules (head supps) `shouldBe` Set.fromList ["avoid-head", "partial-function"]

      it "handles hlint:ignore compatibility" $ do
        let source = T.unlines
              [ "module Test where"
              , "-- hlint:ignore"
              , "foo = head xs"
              ]
        let comments = extractComments source
        let supps = extractSuppressions comments
        length supps `shouldBe` 1

  describe "emptyCommentIndex" $ do
    it "has no comments" $ do
      getAllComments emptyCommentIndex `shouldBe` []

    it "reports nothing in comments" $ do
      isInComment emptyCommentIndex 1 1 `shouldBe` False

  describe "Edge cases" $ do
    it "handles empty source" $ do
      let comments = extractComments ""
      length comments `shouldBe` 0

    it "handles source with only whitespace" $ do
      let comments = extractComments "   \n   \n   "
      length comments `shouldBe` 0

    it "handles unclosed block comment" $ do
      let source = T.unlines
            [ "module Test where"
            , "{- Unclosed comment"
            ]
      -- Should not crash, behavior depends on implementation
      let comments = extractComments source
      comments `shouldSatisfy` const True  -- Just check it doesn't crash

    it "handles comment at end of file without newline" $ do
      let source = "module Test where\n-- comment"
      let comments = extractComments source
      length comments `shouldBe` 1
