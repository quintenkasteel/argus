{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : Performance benchmarks for the Haskell linter
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Benchmarks for core linter operations including:
-- - Word boundary replacement
-- - Pattern matching
-- - Text substitution
module Main where

import Criterion.Main
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Linter.Utils
import Linter.Refactor.Substitution
import Linter.Rules.Patterns

--------------------------------------------------------------------------------
-- Test Data
--------------------------------------------------------------------------------

-- | Small text (single line)
smallText :: Text
smallText = "let x = head xs in tail ys"

-- | Medium text (realistic function)
mediumText :: Text
mediumText = T.unlines
  [ "processData :: [Item] -> IO [Result]"
  , "processData items = do"
  , "  let first = head items"
  , "      rest = tail items"
  , "  results <- forM items $ \\item -> do"
  , "    let value = fromJust (lookup item mapping)"
  , "    process (head values) (tail values)"
  , "  pure $ head results : tail results"
  ]

-- | Large text (module-sized)
largeText :: Text
largeText = T.replicate 100 mediumText

-- | Text with many identifiers to replace
manyIdentifiersText :: Text
manyIdentifiersText = T.intercalate " " $ replicate 1000 "head xs tail ys fromJust val"

-- | Text with strings and comments
textWithStringsAndComments :: Text
textWithStringsAndComments = T.unlines
  [ "-- This is a comment with head in it"
  , "foo = \"This string has head and tail\""
  , "bar = head xs  -- and tail here"
  , "{- Block comment with head -}"
  , "baz = {- inline head -} head ys"
  ]

-- | Substitution map (common replacements)
commonSubstitutions :: Map.Map Text Text
commonSubstitutions = Map.fromList
  [ ("head", "headMay")
  , ("tail", "tailMay")
  , ("last", "lastMay")
  , ("init", "initMay")
  , ("fromJust", "fromMaybe defaultVal")
  ]

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ bgroup "replaceWordBoundary"
      [ bench "small/single" $ nf (replaceWordBoundary "head" "headMay") smallText
      , bench "medium/single" $ nf (replaceWordBoundary "head" "headMay") mediumText
      , bench "large/single" $ nf (replaceWordBoundary "head" "headMay") largeText
      , bench "many-identifiers" $ nf (replaceWordBoundary "head" "headMay") manyIdentifiersText
      ]
  , bgroup "replaceWordBoundaryPreserve"
      [ bench "small/single" $ nf (replaceWordBoundaryPreserve "head" "headMay") smallText
      , bench "medium/single" $ nf (replaceWordBoundaryPreserve "head" "headMay") mediumText
      , bench "large/single" $ nf (replaceWordBoundaryPreserve "head" "headMay") largeText
      , bench "with-strings-comments" $ nf (replaceWordBoundaryPreserve "head" "headMay") textWithStringsAndComments
      ]
  , bgroup "substituteExpr"
      [ bench "small/5-subs" $ nf (substituteExpr commonSubstitutions) smallText
      , bench "medium/5-subs" $ nf (substituteExpr commonSubstitutions) mediumText
      , bench "large/5-subs" $ nf (substituteExpr commonSubstitutions) largeText
      ]
  , bgroup "matchPattern"
      [ bench "simple-match" $ nf (matchPattern "head") smallText
      , bench "wildcard-match" $ nf (matchPattern "head *") mediumText
      , bench "no-match" $ nf (matchPattern "nonexistent") mediumText
      ]
  , bgroup "matchesAtWordBoundary"
      [ bench "match-present" $ nf (matchesAtWordBoundary "head") mediumText
      , bench "match-absent" $ nf (matchesAtWordBoundary "nonexistent") mediumText
      , bench "large-text" $ nf (matchesAtWordBoundary "head") largeText
      ]
  , bgroup "isIdentChar"
      [ bench "alphanumeric" $ nf (map isIdentChar) ['a'..'z']
      , bench "mixed" $ nf (map isIdentChar) "abc123_'!@#$%"
      ]
  ]
