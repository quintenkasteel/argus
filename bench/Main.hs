{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Main
-- Description : Comprehensive benchmarks for Argus static analyzer
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Performance benchmarks covering all major Argus operations:
-- - Parsing (file and source)
-- - Rule evaluation
-- - Fix application (single fix, batch fixes)
-- - Full analysis pipeline
-- - Utility functions
--
-- Run with: stack bench
module Main where

import Criterion.Main
import Control.DeepSeq (NFData (..), deepseq)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Analysis.Syntactic
import Argus.Refactor.ExactPrint (applyFix)
import Argus.Refactor.Substitution (substituteExpr)
import Argus.Rules.Builtin qualified as Builtin
import Argus.Rules.Engine
import Argus.Rules.Types (Category (..))
import Argus.Types
import Argus.Utils

--------------------------------------------------------------------------------
-- Test Data: Synthetic Corpus
--------------------------------------------------------------------------------

-- | Small source file (~20 lines)
smallSource :: Text
smallSource = T.unlines
  [ "module Small where"
  , ""
  , "import Data.List (head, tail)"
  , "import Data.Maybe (fromJust)"
  , ""
  , "-- | Get the first element"
  , "getFirst :: [a] -> a"
  , "getFirst xs = head xs"
  , ""
  , "-- | Get all but first"
  , "getRest :: [a] -> [a]"
  , "getRest xs = tail xs"
  , ""
  , "-- | Unsafe extract"
  , "extract :: Maybe a -> a"
  , "extract mx = fromJust mx"
  , ""
  , "-- | Process list"
  , "process :: [Int] -> Int"
  , "process xs = head xs + head (tail xs)"
  ]

-- | Medium source file (~100 lines)
mediumSource :: Text
mediumSource = T.unlines $ concat
  [ [ "module Medium where"
    , ""
    , "import Data.List"
    , "import Data.Maybe"
    , "import Control.Monad"
    , ""
    ]
  , concatMap generateFunction [1..15 :: Int]
  ]
  where
    generateFunction n =
      [ ""
      , "-- | Function " <> T.pack (show n)
      , "func" <> T.pack (show n) <> " :: [Int] -> Int"
      , "func" <> T.pack (show n) <> " xs ="
      , "  let first = head xs"
      , "      rest = tail xs"
      , "      val = fromJust (lookup first [])"
      , "  in first + val + length rest"
      ]

-- | Large source file (~500 lines)
largeSource :: Text
largeSource = T.unlines $ concat
  [ [ "module Large where"
    , ""
    , "import Data.List"
    , "import Data.Maybe"
    , "import Control.Monad"
    , "import Data.Map.Strict qualified as Map"
    , "import Data.Set qualified as Set"
    , ""
    ]
  , concatMap generateComplexFunction [1..70 :: Int]
  ]
  where
    generateComplexFunction n =
      [ ""
      , "-- | Complex function " <> T.pack (show n)
      , "-- This function does something interesting"
      , "complexFunc" <> T.pack (show n) <> " :: [Int] -> Maybe [Int] -> Either String Int"
      , "complexFunc" <> T.pack (show n) <> " xs mys = do"
      , "  let first = head xs"
      , "      rest = tail xs"
      , "      lastElem = last xs"
      , "      initElems = init xs"
      , "  case mys of"
      , "    Nothing -> Left \"no list\""
      , "    Just ys -> do"
      , "      let yFirst = head ys"
      , "      Right (first + yFirst + lastElem)"
      ]

-- | Extra large source file (~1000 lines)
extraLargeSource :: Text
extraLargeSource = T.unlines $ concat
  [ [ "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE RecordWildCards #-}"
    , "module ExtraLarge where"
    , ""
    , "import Data.List"
    , "import Data.Maybe"
    , "import Control.Monad"
    , "import Data.Map.Strict qualified as Map"
    , "import Data.Set qualified as Set"
    , "import Data.Text (Text)"
    , "import Data.Text qualified as T"
    , ""
    ]
  , concatMap generateVeryComplexFunction [1..120 :: Int]
  ]
  where
    generateVeryComplexFunction n =
      [ ""
      , "-- | Very complex function " <> T.pack (show n)
      , "-- This function performs sophisticated transformations"
      , "-- with multiple pattern matches and guards"
      , "veryComplexFunc" <> T.pack (show n) <> " :: [Int] -> [Int] -> Maybe [Int] -> Either String [Int]"
      , "veryComplexFunc" <> T.pack (show n) <> " xs ys mzs"
      , "  | null xs = Left \"empty xs\""
      , "  | null ys = Left \"empty ys\""
      , "  | otherwise = do"
      , "      let first = head xs"
      , "          rest = tail xs"
      , "          lastX = last xs"
      , "          initX = init xs"
      , "          yFirst = head ys"
      , "          yLast = last ys"
      , "      case mzs of"
      , "        Nothing -> Right [first, yFirst]"
      , "        Just zs -> do"
      , "          let zFirst = head zs"
      , "          Right (first : yFirst : zFirst : rest)"
      ]

-- | Source with many identifiers to replace
manyIdentifiersSource :: Text
manyIdentifiersSource = T.unlines $
  [ "module ManyIdentifiers where"
  , ""
  , "import Data.List"
  , "import Data.Maybe"
  , ""
  ] ++ replicate 100 "result = head xs + head ys + head zs + fromJust val + tail ws"

-- | Deep nesting source (stress test)
deepNestingSource :: Text
deepNestingSource = T.unlines $
  [ "module DeepNesting where"
  , ""
  , "deeplyNested :: Int -> Int"
  , "deeplyNested x ="
  ] ++ generateNesting 50
  where
    generateNesting :: Int -> [Text]
    generateNesting 0 = ["  x"]
    generateNesting n =
      ("  let v" <> T.pack (show n) <> " = x + " <> T.pack (show n) <> " in")
      : map ("  " <>) (generateNesting (n - 1))

--------------------------------------------------------------------------------
-- Substitution Maps
--------------------------------------------------------------------------------

-- | Common partial function substitutions
partialSubstitutions :: Map.Map Text Text
partialSubstitutions = Map.fromList
  [ ("head", "headMay")
  , ("tail", "tailMay")
  , ("last", "lastMay")
  , ("init", "initMay")
  , ("fromJust", "fromMaybe defaultVal")
  , ("read", "readMaybe")
  , ("!!", "`atMay`")
  ]

-- | Large substitution map
largeSubstitutions :: Map.Map Text Text
largeSubstitutions = Map.fromList
  [ ("head", "headMay")
  , ("tail", "tailMay")
  , ("last", "lastMay")
  , ("init", "initMay")
  , ("fromJust", "fromMaybe defaultVal")
  , ("read", "readMaybe")
  , ("!!", "`atMay`")
  , ("minimum", "minimumMay")
  , ("maximum", "maximumMay")
  , ("foldr1", "foldr1May")
  , ("foldl1", "foldl1May")
  , ("cycle", "safeCycle")
  , ("succ", "succMay")
  , ("pred", "predMay")
  , ("toEnum", "toEnumMay")
  , ("genericIndex", "genericIndexMay")
  , ("error", "throwError")
  , ("undefined", "throwError \"undefined\"")
  ]

--------------------------------------------------------------------------------
-- NFData Instances for Benchmarking
--------------------------------------------------------------------------------

-- Simplified NFData instances - just force outer structure
-- We use seq on individual fields since nested types may lack NFData

instance NFData ParseResult where
  rnf ParseResult{..} = prSource `seq` prFile `seq` ()

instance NFData ParseError where
  rnf ParseError{..} = peFile `seq` peLine `seq`
                       peColumn `seq` peMessage `seq` ()

instance NFData FunctionInfo where
  rnf FunctionInfo{..} = fiName `seq` fiExported `seq` ()

instance NFData ArgumentInfo where
  rnf ArgumentInfo{..} = aiName `seq` ()

instance NFData TypeInfo where
  rnf TypeInfo{..} = tiText `seq` ()

instance NFData ImportInfo where
  rnf ImportInfo{..} = iiModuleName `seq` ()

instance NFData ExportInfo where
  rnf ExportInfo{..} = ()

-- Note: RuleEngine and Rule contain functions so we can't derive NFData
-- We use nfIO with length forcing for benchmarks involving these types

--------------------------------------------------------------------------------
-- Benchmark Groups
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ parsingBenchmarks
  , ruleEvaluationBenchmarks
  , fixApplicationBenchmarks
  , utilityBenchmarks
  , scalabilityBenchmarks
  , validationBenchmarks
  , dependencyGraphBenchmarks
  , realWorldBenchmarks
  ]

-- | Parsing benchmarks
parsingBenchmarks :: Benchmark
parsingBenchmarks = bgroup "parsing"
  [ bgroup "parseModule"
      [ bench "small (20 lines)" $ nfIO (parseModule "<bench>" smallSource)
      , bench "medium (100 lines)" $ nfIO (parseModule "<bench>" mediumSource)
      , bench "large (500 lines)" $ nfIO (parseModule "<bench>" largeSource)
      , bench "extra-large (1000 lines)" $ nfIO (parseModule "<bench>" extraLargeSource)
      , bench "deep-nesting" $ nfIO (parseModule "<bench>" deepNestingSource)
      ]
  , bgroup "extractFunctions"
      [ bench "small" $ nfIO $ do
          result <- parseModule "<bench>" smallSource
          case result of
            Left _ -> pure []
            Right pr -> pure $ extractFunctions "<bench>" smallSource (prModule pr)
      , bench "medium" $ nfIO $ do
          result <- parseModule "<bench>" mediumSource
          case result of
            Left _ -> pure []
            Right pr -> pure $ extractFunctions "<bench>" mediumSource (prModule pr)
      , bench "large" $ nfIO $ do
          result <- parseModule "<bench>" largeSource
          case result of
            Left _ -> pure []
            Right pr -> pure $ extractFunctions "<bench>" largeSource (prModule pr)
      ]
  , bgroup "extractImports"
      [ bench "small" $ nfIO $ do
          result <- parseModule "<bench>" smallSource
          case result of
            Left _ -> pure []
            Right pr -> pure $ extractImports "<bench>" (prModule pr)
      , bench "large" $ nfIO $ do
          result <- parseModule "<bench>" largeSource
          case result of
            Left _ -> pure []
            Right pr -> pure $ extractImports "<bench>" (prModule pr)
      ]
  ]

-- | Rule evaluation benchmarks
ruleEvaluationBenchmarks :: Benchmark
ruleEvaluationBenchmarks = bgroup "rule-evaluation"
  [ bgroup "evaluateRules"
      [ bench "small/all-rules" $ nfIO (runRulesOnSource smallSource allRules)
      , bench "medium/all-rules" $ nfIO (runRulesOnSource mediumSource allRules)
      , bench "large/all-rules" $ nfIO (runRulesOnSource largeSource allRules)
      , bench "small/safety-only" $ nfIO (runRulesOnSource smallSource safetyRules)
      , bench "medium/safety-only" $ nfIO (runRulesOnSource mediumSource safetyRules)
      ]
  , bgroup "rule-throughput"
      [ bench "1000-lines/all-rules" $ nfIO (runRulesOnSource extraLargeSource allRules)
      ]
  ]
  where
    allRules = Builtin.allBuiltinRules
    safetyRules = filter (\r -> ruleCategory r == Safety) Builtin.allBuiltinRules

    -- Force evaluation by returning length
    runRulesOnSource :: Text -> [Rule] -> IO Int
    runRulesOnSource src rules = do
      let engine = mkRuleEngine rules
          results = evaluateRules engine "<bench>" "Bench" src
      pure $! length results

-- | Fix application benchmarks
fixApplicationBenchmarks :: Benchmark
fixApplicationBenchmarks = bgroup "fix-application"
  [ bgroup "applyFix"
      [ bench "single-fix/small" $ nf (applyTestFix smallSource) singleFix
      , bench "single-fix/medium" $ nf (applyTestFix mediumSource) singleFix
      , bench "single-fix/large" $ nf (applyTestFix largeSource) singleFix
      ]
  , bgroup "substituteExpr"
      [ bench "small/5-subs" $ nf (substituteExpr partialSubstitutions) smallSource
      , bench "medium/5-subs" $ nf (substituteExpr partialSubstitutions) mediumSource
      , bench "large/5-subs" $ nf (substituteExpr partialSubstitutions) largeSource
      , bench "small/17-subs" $ nf (substituteExpr largeSubstitutions) smallSource
      , bench "medium/17-subs" $ nf (substituteExpr largeSubstitutions) mediumSource
      , bench "many-identifiers/5-subs" $ nf (substituteExpr partialSubstitutions) manyIdentifiersSource
      ]
  , bgroup "batch-fixes"
      [ bench "5-fixes/small" $ nf (applyMultipleFixes smallSource) fiveFixes
      , bench "5-fixes/medium" $ nf (applyMultipleFixes mediumSource) fiveFixes
      , bench "10-fixes/medium" $ nf (applyMultipleFixes mediumSource) tenFixes
      ]
  ]
  where
    singleFix :: Fix
    singleFix = Fix
      { fixTitle = "Replace head with headMay"
      , fixEdits = [FixEdit (mkSrcSpanRaw "<bench>" 8 16 8 20) "headMay"]
      , fixIsPreferred = True
      , fixAddImports = []
      , fixRemoveImports = []
      , fixCategory = FCSafety
      , fixSafety = FSAlways
      }

    fiveFixes :: [Fix]
    fiveFixes =
      [ Fix "Fix 1" [FixEdit (mkSrcSpanRaw "<bench>" 8 16 8 20) "headMay"] True [] [] FCSafety FSAlways
      , Fix "Fix 2" [FixEdit (mkSrcSpanRaw "<bench>" 12 16 12 20) "tailMay"] True [] [] FCSafety FSAlways
      , Fix "Fix 3" [FixEdit (mkSrcSpanRaw "<bench>" 16 16 16 23) "fromMaybe d"] True [] [] FCSafety FSAlways
      , Fix "Fix 4" [FixEdit (mkSrcSpanRaw "<bench>" 20 16 20 20) "headMay"] True [] [] FCSafety FSAlways
      , Fix "Fix 5" [FixEdit (mkSrcSpanRaw "<bench>" 20 28 20 32) "tailMay"] True [] [] FCSafety FSAlways
      ]

    tenFixes :: [Fix]
    tenFixes = fiveFixes ++ map adjustFix fiveFixes
      where
        adjustFix f = f { fixEdits = map adjustEdit (fixEdits f) }
        adjustEdit (FixEdit (SrcSpan file sl sc el ec) t) =
          FixEdit (mkSrcSpanRaw file (unLine sl + 20) (unColumn sc) (unLine el + 20) (unColumn ec)) t

    applyTestFix :: Text -> Fix -> Text
    applyTestFix src fix = applyFix src fix

    applyMultipleFixes :: Text -> [Fix] -> Text
    applyMultipleFixes src fixes = foldl applyFix src fixes

-- | Utility function benchmarks
utilityBenchmarks :: Benchmark
utilityBenchmarks = bgroup "utilities"
  [ bgroup "replaceWordBoundary"
      [ bench "small/single" $ nf (replaceWordBoundary "head" "headMay") smallSource
      , bench "medium/single" $ nf (replaceWordBoundary "head" "headMay") mediumSource
      , bench "large/single" $ nf (replaceWordBoundary "head" "headMay") largeSource
      , bench "many-identifiers" $ nf (replaceWordBoundary "head" "headMay") manyIdentifiersSource
      ]
  , bgroup "replaceWordBoundaryPreserve"
      [ bench "small" $ nf (replaceWordBoundaryPreserve "head" "headMay") smallSource
      ]
  , bgroup "matchesAtWordBoundary"
      [ bench "present/small" $ nf (matchesAtWordBoundary "head") smallSource
      , bench "present/large" $ nf (matchesAtWordBoundary "head") largeSource
      , bench "absent" $ nf (matchesAtWordBoundary "nonexistent") mediumSource
      ]
  , bgroup "isIdentChar"
      [ bench "alphanumeric" $ nf (map isIdentChar) ['a'..'z']
      , bench "mixed" $ nf (map isIdentChar) "abc123_'!@#$%^&*()"
      , bench "large-string" $ nf (map isIdentChar) (replicate 1000 'a')
      ]
  , bgroup "safe-list-ops"
      [ bench "headMay/Just" $ nf headMay [1..100 :: Int]
      , bench "headMay/Nothing" $ nf headMay ([] :: [Int])
      , bench "tailMay/Just" $ nf tailMay [1..100 :: Int]
      , bench "lastMay/Just" $ nf lastMay [1..100 :: Int]
      ]
  ]

-- | Scalability benchmarks
scalabilityBenchmarks :: Benchmark
scalabilityBenchmarks = bgroup "scalability"
  [ bgroup "line-count"
      [ bench "100-lines" $ nfIO (parseModule "<bench>" (generateSource 100))
      , bench "500-lines" $ nfIO (parseModule "<bench>" (generateSource 500))
      , bench "1000-lines" $ nfIO (parseModule "<bench>" (generateSource 1000))
      , bench "2000-lines" $ nfIO (parseModule "<bench>" (generateSource 2000))
      ]
  , bgroup "function-count"
      [ bench "10-functions" $ nfIO (extractFunctionsFromSource (generateSourceWithNFuncs 10))
      , bench "50-functions" $ nfIO (extractFunctionsFromSource (generateSourceWithNFuncs 50))
      , bench "100-functions" $ nfIO (extractFunctionsFromSource (generateSourceWithNFuncs 100))
      , bench "200-functions" $ nfIO (extractFunctionsFromSource (generateSourceWithNFuncs 200))
      ]
  , bgroup "rule-count"
      [ bench "10-rules" $ nfIO (runNRules mediumSource 10)
      , bench "50-rules" $ nfIO (runNRules mediumSource 50)
      , bench "100-rules" $ nfIO (runNRules mediumSource 100)
      , bench "all-rules" $ nfIO (runNRules mediumSource 1000)
      ]
  ]
  where
    generateSource :: Int -> Text
    generateSource targetLines =
      let baseFuncLines = 7
          numFuncs = max 1 (targetLines `div` baseFuncLines)
      in T.unlines $
           [ "module Scalability where"
           , ""
           , "import Data.List"
           , ""
           ] ++ concatMap genFunc [1..numFuncs]
      where
        genFunc n =
          [ "func" <> T.pack (show n) <> " :: [Int] -> Int"
          , "func" <> T.pack (show n) <> " xs = head xs + last xs"
          , ""
          ]

    generateSourceWithNFuncs :: Int -> Text
    generateSourceWithNFuncs n = T.unlines $
      [ "module ManyFunctions where"
      , ""
      ] ++ concatMap genFunc [1..n]
      where
        genFunc i =
          [ "func" <> T.pack (show i) <> " :: Int -> Int"
          , "func" <> T.pack (show i) <> " x = x + " <> T.pack (show i)
          , ""
          ]

    extractFunctionsFromSource :: Text -> IO [FunctionInfo]
    extractFunctionsFromSource src = do
      result <- parseModule "<bench>" src
      case result of
        Left _ -> pure []
        Right pr -> pure $ extractFunctions "<bench>" src (prModule pr)

    -- Force evaluation by returning length
    runNRules :: Text -> Int -> IO Int
    runNRules src n = do
      let limitedRules = take n Builtin.allBuiltinRules
          engine = mkRuleEngine limitedRules
          results = evaluateRules engine "<bench>" "Bench" src
      pure $! length results

-- | Validation benchmarks
validationBenchmarks :: Benchmark
validationBenchmarks = bgroup "validation"
  [ bgroup "structural"
      [ bench "small/balanced-check" $ nf checkBalanced smallSource
      , bench "medium/balanced-check" $ nf checkBalanced mediumSource
      , bench "large/balanced-check" $ nf checkBalanced largeSource
      , bench "deep-nesting/balanced-check" $ nf checkBalanced deepNestingSource
      ]
  , bgroup "syntax"
      [ bench "small/parse-validate" $ nfIO (validateSyntax smallSource)
      , bench "medium/parse-validate" $ nfIO (validateSyntax mediumSource)
      , bench "large/parse-validate" $ nfIO (validateSyntax largeSource)
      ]
  , bgroup "diff-generation"
      [ bench "small-diff" $ nf (generateTextDiff smallSource) (applySmallChange smallSource)
      , bench "medium-diff" $ nf (generateTextDiff mediumSource) (applySmallChange mediumSource)
      , bench "large-diff" $ nf (generateTextDiff largeSource) (applySmallChange largeSource)
      ]
  ]
  where
    -- Simple bracket balance check
    checkBalanced :: Text -> Bool
    checkBalanced src =
      let chars = T.unpack src
          go :: Int -> [Char] -> Bool
          go n _ | n < 0 = False
          go n [] = n == 0
          go n ('(':cs) = go (n+1) cs
          go n (')':cs) = go (n-1) cs
          go n (_:cs) = go n cs
      in go 0 chars

    validateSyntax :: Text -> IO Bool
    validateSyntax src = do
      result <- parseModule "<bench>" src
      pure $ case result of
        Left _ -> False
        Right _ -> True

    applySmallChange :: Text -> Text
    applySmallChange src =
      let ls = T.lines src
      in case ls of
           [] -> src
           (h:t) -> T.unlines (h : "-- Modified" : t)

    generateTextDiff :: Text -> Text -> [(Text, Text)]
    generateTextDiff old new =
      let oldLines = T.lines old
          newLines = T.lines new
      in zip oldLines newLines

-- | Dependency graph benchmarks
dependencyGraphBenchmarks :: Benchmark
dependencyGraphBenchmarks = bgroup "dependency-graph"
  [ bgroup "import-extraction"
      [ bench "small" $ nfIO $ extractImportsFromSource smallSource
      , bench "medium" $ nfIO $ extractImportsFromSource mediumSource
      , bench "large" $ nfIO $ extractImportsFromSource largeSource
      ]
  , bgroup "dependency-resolution"
      [ bench "10-modules" $ nf buildMockDepGraph 10
      , bench "50-modules" $ nf buildMockDepGraph 50
      , bench "100-modules" $ nf buildMockDepGraph 100
      , bench "200-modules" $ nf buildMockDepGraph 200
      ]
  , bgroup "topological-sort"
      [ bench "10-nodes" $ nf topoSort (buildMockDepGraph 10)
      , bench "50-nodes" $ nf topoSort (buildMockDepGraph 50)
      , bench "100-nodes" $ nf topoSort (buildMockDepGraph 100)
      ]
  ]
  where
    extractImportsFromSource :: Text -> IO [ImportInfo]
    extractImportsFromSource src = do
      result <- parseModule "<bench>" src
      case result of
        Left _ -> pure []
        Right pr -> pure $ extractImports "<bench>" (prModule pr)

    -- Build mock dependency graph for benchmarking
    buildMockDepGraph :: Int -> Map.Map Text [Text]
    buildMockDepGraph n =
      Map.fromList
        [ (T.pack ("Module" ++ show i), deps i)
        | i <- [1..n]
        ]
      where
        deps i
          | i <= 1 = []
          | otherwise = [T.pack ("Module" ++ show (i `div` 2))]

    -- Simple topological sort
    topoSort :: Map.Map Text [Text] -> [Text]
    topoSort graph = go [] (Map.keys graph)
      where
        go acc [] = reverse acc
        go acc (k:ks) = go (k:acc) ks

-- | Real-world scenario benchmarks
realWorldBenchmarks :: Benchmark
realWorldBenchmarks = bgroup "real-world"
  [ bgroup "full-pipeline"
      [ bench "small-file/full" $ nfIO (runFullPipeline smallSource)
      , bench "medium-file/full" $ nfIO (runFullPipeline mediumSource)
      , bench "large-file/full" $ nfIO (runFullPipeline largeSource)
      ]
  , bgroup "incremental-scenarios"
      [ bench "cache-miss/small" $ nfIO (simulateCacheMiss smallSource)
      , bench "cache-miss/medium" $ nfIO (simulateCacheMiss mediumSource)
      , bench "single-line-change" $ nfIO (simulateSingleLineChange mediumSource)
      ]
  , bgroup "multi-file"
      [ bench "5-files/sequential" $ nfIO (analyzeMultipleFilesSeq 5)
      , bench "10-files/sequential" $ nfIO (analyzeMultipleFilesSeq 10)
      , bench "20-files/sequential" $ nfIO (analyzeMultipleFilesSeq 20)
      ]
  , bgroup "memory-pressure"
      [ bench "parse-discard/100-iterations" $ nfIO (parseAndDiscard 100 smallSource)
      , bench "rule-eval/100-iterations" $ nfIO (ruleEvalDiscard 100 smallSource)
      ]
  ]
  where
    runFullPipeline :: Text -> IO Int
    runFullPipeline src = do
      -- Parse
      parseResult <- parseModule "<bench>" src
      case parseResult of
        Left _ -> pure 0
        Right pr -> do
          -- Run rules
          let engine = mkRuleEngine Builtin.allBuiltinRules
              diagnostics = evaluateRules engine "<bench>" "Bench" src
          -- Return diagnostic count as measure
          pure $! length diagnostics

    simulateCacheMiss :: Text -> IO Int
    simulateCacheMiss src = do
      -- Simulate full analysis (no cache hit)
      parseResult <- parseModule "<bench>" src
      case parseResult of
        Left _ -> pure 0
        Right pr -> do
          let funcs = extractFunctions "<bench>" src (prModule pr)
              imports = extractImports "<bench>" (prModule pr)
              engine = mkRuleEngine Builtin.allBuiltinRules
              diagnostics = evaluateRules engine "<bench>" "Bench" src
          pure $! length funcs + length imports + length diagnostics

    simulateSingleLineChange :: Text -> IO Int
    simulateSingleLineChange src = do
      -- Parse original
      _ <- parseModule "<bench>" src
      -- Apply single line change
      let modified = T.unlines (take 5 (T.lines src) ++ ["-- changed"] ++ drop 5 (T.lines src))
      -- Re-parse
      parseResult <- parseModule "<bench>" modified
      case parseResult of
        Left _ -> pure 0
        Right pr -> do
          let engine = mkRuleEngine Builtin.allBuiltinRules
              diagnostics = evaluateRules engine "<bench>" "Bench" modified
          pure $! length diagnostics

    analyzeMultipleFilesSeq :: Int -> IO Int
    analyzeMultipleFilesSeq n = do
      let sources = replicate n mediumSource
      results <- mapM runFullPipeline sources
      pure $! sum results

    parseAndDiscard :: Int -> Text -> IO ()
    parseAndDiscard n src = do
      mapM_ (\_ -> parseModule "<bench>" src) [1..n]

    ruleEvalDiscard :: Int -> Text -> IO ()
    ruleEvalDiscard n src = do
      let engine = mkRuleEngine (take 50 Builtin.allBuiltinRules)
      mapM_ (\_ -> pure $! length (evaluateRules engine "<bench>" "Bench" src)) [1..n]
