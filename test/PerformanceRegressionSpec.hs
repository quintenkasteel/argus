{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : PerformanceRegressionSpec
-- Description : Performance regression tests for Argus
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Tracks performance baselines and detects regressions.
-- Tests must fail if performance degrades by more than 20%.
module PerformanceRegressionSpec (spec) where

import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec

import Argus.Analysis.Syntactic (parseModule, extractFunctions, ParseResult(..), ParseError(..))
import Argus.Config (defaultConfig)
import Argus.Core (analyzeSource, AnalysisContext, defaultContext, runArgusOnFile)
import Argus.Refactor.Engine (refactorFile, RefactorOptions(..))
import Argus.Rules.ConfigurableRules (defaultRulesConfig)
import Argus.Types

--------------------------------------------------------------------------------
-- Baseline Management
--------------------------------------------------------------------------------

-- | Performance baseline for a specific test
data PerfBaseline = PerfBaseline
  { pbTestName        :: Text       -- ^ Name of the test
  , pbBaselineSeconds :: Double     -- ^ Baseline time in seconds
  , pbRecordedAt      :: Text       -- ^ When baseline was recorded
  , pbEnvironment     :: Text       -- ^ Environment description
  }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON PerfBaseline where
  toJSON PerfBaseline{..} = Aeson.object
    [ "testName"        Aeson..= pbTestName
    , "baselineSeconds" Aeson..= pbBaselineSeconds
    , "recordedAt"      Aeson..= pbRecordedAt
    , "environment"     Aeson..= pbEnvironment
    ]

instance Aeson.FromJSON PerfBaseline where
  parseJSON = Aeson.withObject "PerfBaseline" $ \o -> do
    pbTestName        <- o Aeson..: "testName"
    pbBaselineSeconds <- o Aeson..: "baselineSeconds"
    pbRecordedAt      <- o Aeson..: "recordedAt"
    pbEnvironment     <- o Aeson..: "environment"
    pure PerfBaseline{..}

-- | All baselines
newtype PerfBaselines = PerfBaselines
  { unBaselines :: Map.Map Text PerfBaseline }
  deriving stock (Show, Eq, Generic)

instance Aeson.ToJSON PerfBaselines where
  toJSON (PerfBaselines m) = Aeson.toJSON m

instance Aeson.FromJSON PerfBaselines where
  parseJSON v = PerfBaselines <$> Aeson.parseJSON v

-- | Path to baselines file
baselinesFile :: FilePath
baselinesFile = "test/data/performance-baselines.json"

-- | Load baselines from disk
loadBaselines :: IO PerfBaselines
loadBaselines = do
  exists <- doesFileExist baselinesFile
  if exists
    then do
      content <- BSL.readFile baselinesFile
      case Aeson.eitherDecode content of
        Left err -> do
          putStrLn $ "Warning: Failed to parse baselines: " <> err
          pure (PerfBaselines Map.empty)
        Right baselines -> pure baselines
    else pure (PerfBaselines Map.empty)

-- | Save baselines to disk
saveBaselines :: PerfBaselines -> IO ()
saveBaselines baselines = do
  createDirectoryIfMissing True "test/data"
  BSL.writeFile baselinesFile (Aeson.encode baselines)

-- | Update or add a baseline
updateBaseline :: Text -> Double -> Text -> PerfBaselines -> PerfBaselines
updateBaseline testName seconds env (PerfBaselines m) =
  let baseline = PerfBaseline testName seconds "recorded-during-test" env
  in PerfBaselines (Map.insert testName baseline m)

-- | Regression threshold (20%)
regressionThreshold :: Double
regressionThreshold = 0.20  -- 20%

-- | Check if time represents a regression
isRegression :: Double -> Double -> Bool
isRegression baseline current =
  current > baseline * (1.0 + regressionThreshold)

-- | Calculate percentage increase
percentageIncrease :: Double -> Double -> Double
percentageIncrease baseline current =
  ((current - baseline) / baseline) * 100.0

--------------------------------------------------------------------------------
-- Timing Utilities
--------------------------------------------------------------------------------

-- | Time an IO action and return the result and elapsed seconds
timeIO :: IO a -> IO (a, Double)
timeIO action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  let elapsed = realToFrac (diffUTCTime end start)
  pure (result, elapsed)

-- | Time a pure computation by forcing evaluation
timePure :: NFData a => a -> IO Double
timePure computation = do
  start <- getCurrentTime
  _ <- evaluate (force computation)
  end <- getCurrentTime
  let elapsed = realToFrac (diffUTCTime end start)
  pure elapsed

-- | Run a performance test and compare against baseline
runPerfTest :: Text -> IO a -> Spec
runPerfTest testName action = do
  it (T.unpack testName <> " - performance regression check") $ do
    -- Load existing baselines
    baselines <- loadBaselines

    -- Run the test and measure time
    (_, elapsed) <- timeIO action

    -- Check against baseline
    case Map.lookup testName (unBaselines baselines) of
      Nothing -> do
        -- No baseline exists, record this as baseline
        let newBaselines = updateBaseline testName elapsed "test-env" baselines
        saveBaselines newBaselines
        putStrLn $ "\n  ✓ Baseline recorded: " <> T.unpack testName <> " = " <> show elapsed <> "s"

      Just baseline -> do
        let baselineTime = pbBaselineSeconds baseline
            increase = percentageIncrease baselineTime elapsed

        putStrLn $ "\n  Baseline: " <> show baselineTime <> "s"
        putStrLn $ "  Current:  " <> show elapsed <> "s"
        putStrLn $ "  Change:   " <> showChange increase

        -- Assert no regression
        if isRegression baselineTime elapsed
          then expectationFailure $
                 "Performance regression detected!\n" <>
                 "  Test: " <> T.unpack testName <> "\n" <>
                 "  Baseline: " <> show baselineTime <> "s\n" <>
                 "  Current: " <> show elapsed <> "s\n" <>
                 "  Increase: " <> show increase <> "%\n" <>
                 "  Threshold: " <> show (regressionThreshold * 100) <> "%"
          else pure ()
  where
    showChange :: Double -> String
    showChange pct
      | pct > 0   = "+" <> show pct <> "% (slower)"
      | pct < 0   = show pct <> "% (faster)"
      | otherwise = "0% (same)"

--------------------------------------------------------------------------------
-- Test Inputs
--------------------------------------------------------------------------------

-- | Path to realistic benchmark file
benchmarkFile :: FilePath
benchmarkFile = "test/data/perf-benchmark.hs"

-- | Load benchmark file content
loadBenchmarkFile :: IO Text
loadBenchmarkFile = do
  content <- TE.decodeUtf8 <$> BSL.toStrict <$> BSL.readFile benchmarkFile
  pure content

-- | Source file with many issues (for testing analysis with issues)
manyIssuesSource :: Text
manyIssuesSource = T.unlines
  [ "module ManyIssues where"
  , ""
  , "import Data.List (head, tail, last, init)"
  , "import Data.Maybe (fromJust)"
  , ""
  , "-- | Many partial function usages"
  , "unsafeHead :: [a] -> a"
  , "unsafeHead = head"
  , ""
  , "unsafeTail :: [a] -> [a]"
  , "unsafeTail = tail"
  , ""
  , "unsafeLast :: [a] -> a"
  , "unsafeLast = last"
  , ""
  , "unsafeInit :: [a] -> [a]"
  , "unsafeInit = init"
  , ""
  , "unsafeExtract :: Maybe a -> a"
  , "unsafeExtract = fromJust"
  , ""
  , "unsafeIndex :: [a] -> Int -> a"
  , "unsafeIndex xs n = xs !! n"
  , ""
  , "-- | Performance issues"
  , "inefficientNub :: [Int] -> [Int]"
  , "inefficientNub = nub"
  , ""
  , "inefficientElem :: Int -> [Int] -> Bool"
  , "inefficientElem = elem"
  , ""
  , "inefficientConcat :: [[a]] -> [a]"
  , "inefficientConcat = concat . map id"
  , ""
  , "inefficientFilter :: [Int] -> Int"
  , "inefficientFilter = length . filter (> 0)"
  , ""
  , "-- | Redundant code"
  , "redundantId :: a -> a"
  , "redundantId x = id (id (id x))"
  , ""
  , "redundantParen :: Int -> Int"
  , "redundantParen x = ((((x))))"
  , ""
  , "redundantLambda :: (a -> b) -> a -> b"
  , "redundantLambda f = \\x -> f x"
  , ""
  , "-- | Naming violations (snake_case instead of camelCase)"
  , "bad_function_name :: Int -> Int"
  , "bad_function_name x = x"
  , ""
  , "another_bad_name :: Int -> Int"
  , "another_bad_name x = x"
  ]

-- | Source file with no issues (clean code)
noIssuesSource :: Text
noIssuesSource = T.unlines
  [ "module NoIssues where"
  , ""
  , "import Data.List (foldl')"
  , "import Data.Maybe (fromMaybe)"
  , "import Safe (headMay, tailMay, lastMay, atMay)"
  , ""
  , "-- | Safe head operation"
  , "safeHead :: [a] -> Maybe a"
  , "safeHead = headMay"
  , ""
  , "-- | Safe tail operation"
  , "safeTail :: [a] -> Maybe [a]"
  , "safeTail = tailMay"
  , ""
  , "-- | Safe last operation"
  , "safeLast :: [a] -> Maybe a"
  , "safeLast = lastMay"
  , ""
  , "-- | Safe index operation"
  , "safeIndex :: [a] -> Int -> Maybe a"
  , "safeIndex = atMay"
  , ""
  , "-- | Extract with default"
  , "safeExtract :: a -> Maybe a -> a"
  , "safeExtract = fromMaybe"
  , ""
  , "-- | Sum with strict left fold"
  , "sumStrict :: [Int] -> Int"
  , "sumStrict = foldl' (+) 0"
  , ""
  , "-- | Well-named function"
  , "processItems :: [a] -> [a]"
  , "processItems = id"
  , ""
  , "-- | Clean implementation"
  , "cleanFunction :: Int -> Int -> Int"
  , "cleanFunction x y = x + y"
  ]

--------------------------------------------------------------------------------
-- Performance Tests
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Argus Performance Regression Tests" $ do

    describe "Parsing Performance" $ do

      runPerfTest "parse-realistic-file" $ do
        source <- loadBenchmarkFile
        result <- parseModule benchmarkFile source
        case result of
          Left err -> error $ "Parse failed: " <> T.unpack (peMessage err)
          Right pr -> pure $! T.length (prSource pr)

      runPerfTest "parse-many-issues-source" $ do
        result <- parseModule "<test>" manyIssuesSource
        case result of
          Left _ -> error "Parse failed"
          Right pr -> pure $! T.length (prSource pr)

      runPerfTest "parse-clean-source" $ do
        result <- parseModule "<test>" noIssuesSource
        case result of
          Left _ -> error "Parse failed"
          Right pr -> pure $! T.length (prSource pr)

    describe "Analysis Performance" $ do

      runPerfTest "analyze-file-with-many-issues" $ do
        let config = defaultConfig
            opts = defaultOptions
            rulesConfig = defaultRulesConfig
            ctx = defaultContext config opts rulesConfig
        diags <- analyzeSource ctx "<test>" manyIssuesSource
        pure $! length diags

      runPerfTest "analyze-file-with-no-issues" $ do
        let config = defaultConfig
            opts = defaultOptions
            rulesConfig = defaultRulesConfig
            ctx = defaultContext config opts rulesConfig
        diags <- analyzeSource ctx "<test>" noIssuesSource
        pure $! length diags

      runPerfTest "analyze-realistic-file" $ do
        source <- loadBenchmarkFile
        let config = defaultConfig
            opts = defaultOptions
            rulesConfig = defaultRulesConfig
            ctx = defaultContext config opts rulesConfig
        diags <- analyzeSource ctx benchmarkFile source
        pure $! length diags

      runPerfTest "extract-functions-from-realistic-file" $ do
        source <- loadBenchmarkFile
        result <- parseModule benchmarkFile source
        case result of
          Left _ -> error "Parse failed"
          Right pr -> do
            let functions = extractFunctions benchmarkFile source (prModule pr)
            pure $! length functions

    describe "Fix Application Performance" $ do

      runPerfTest "apply-single-fix" $ do
        let source = "main = head [1,2,3]"
            fix = Fix
              { fixTitle = "Replace head with headMay"
              , fixEdits = [FixEdit (mkSrcSpanRaw "<test>" 1 8 1 12) "headMay"]
              , fixIsPreferred = True
              , fixAddImports = []
              , fixRemoveImports = []
              , fixCategory = FCSafety
              , fixSafety = FSAlways
              }
            result = applyFix source fix
        pure $! T.length result

      runPerfTest "apply-multiple-fixes" $ do
        let source = manyIssuesSource
            fixes =
              [ Fix "Fix 1" [FixEdit (mkSrcSpanRaw "<test>" 8 16 8 20) "headMay"] True [] [] FCSafety FSAlways
              , Fix "Fix 2" [FixEdit (mkSrcSpanRaw "<test>" 12 16 12 20) "tailMay"] True [] [] FCSafety FSAlways
              , Fix "Fix 3" [FixEdit (mkSrcSpanRaw "<test>" 16 16 16 20) "lastMay"] True [] [] FCSafety FSAlways
              , Fix "Fix 4" [FixEdit (mkSrcSpanRaw "<test>" 20 16 20 20) "initMay"] True [] [] FCSafety FSAlways
              , Fix "Fix 5" [FixEdit (mkSrcSpanRaw "<test>" 24 19 24 27) "fromMaybe d"] True [] [] FCSafety FSAlways
              ]
            result = foldl applyFix source fixes
        pure $! T.length result

      runPerfTest "apply-fixes-to-realistic-file" $ do
        source <- loadBenchmarkFile
        -- Apply a sample fix
        let fix = Fix
              { fixTitle = "Sample fix"
              , fixEdits = [FixEdit (mkSrcSpanRaw benchmarkFile 10 10 10 14) "safe"]
              , fixIsPreferred = True
              , fixAddImports = []
              , fixRemoveImports = []
              , fixCategory = FCSafety
              , fixSafety = FSAlways
              }
            result = applyFix source fix
        pure $! T.length result

    describe "Baseline Comparison Tests" $ do

      it "can load and save baselines" $ do
        let testBaseline = PerfBaseline "test" 1.0 "now" "test-env"
            baselines = PerfBaselines (Map.singleton "test" testBaseline)

        saveBaselines baselines
        loaded <- loadBaselines

        Map.lookup "test" (unBaselines loaded) `shouldBe` Just testBaseline

      it "detects regressions correctly" $ do
        let baseline = 1.0
            good = 1.15  -- 15% increase - acceptable
            bad = 1.25   -- 25% increase - regression

        isRegression baseline good `shouldBe` False
        isRegression baseline bad `shouldBe` True

      it "calculates percentage increase correctly" $ do
        let baseline = 1.0
            current = 1.2
            increase = percentageIncrease baseline current
        -- Use approximate equality due to floating point representation
        -- (1.2 - 1.0) isn't exactly 0.2 in IEEE 754
        abs (increase - 20.0) `shouldSatisfy` (< 0.0001)

      it "handles performance improvements correctly" $ do
        let baseline = 1.0
            faster = 0.8  -- 20% faster

        isRegression baseline faster `shouldBe` False

    describe "Scalability Tests" $ do

      runPerfTest "analyze-100-lines" $ do
        let source = generateSource 100
            config = defaultConfig
            opts = defaultOptions
            rulesConfig = defaultRulesConfig
            ctx = defaultContext config opts rulesConfig
        diags <- analyzeSource ctx "<test>" source
        pure $! length diags

      runPerfTest "analyze-500-lines" $ do
        let source = generateSource 500
            config = defaultConfig
            opts = defaultOptions
            rulesConfig = defaultRulesConfig
            ctx = defaultContext config opts rulesConfig
        diags <- analyzeSource ctx "<test>" source
        pure $! length diags

      runPerfTest "analyze-1000-lines" $ do
        let source = generateSource 1000
            config = defaultConfig
            opts = defaultOptions
            rulesConfig = defaultRulesConfig
            ctx = defaultContext config opts rulesConfig
        diags <- analyzeSource ctx "<test>" source
        pure $! length diags

--------------------------------------------------------------------------------
-- Test Data Generators
--------------------------------------------------------------------------------

-- | Generate source code with specified number of lines
generateSource :: Int -> Text
generateSource targetLines =
  let baseFuncLines = 5
      numFuncs = max 1 (targetLines `div` baseFuncLines)
  in T.unlines $
       [ "module Generated where"
       , ""
       , "import Data.List (head, tail, last)"
       , ""
       ] ++ concatMap genFunc [1..numFuncs]
  where
    genFunc :: Int -> [Text]
    genFunc n =
      [ "func" <> T.pack (show n) <> " :: [Int] -> Int"
      , "func" <> T.pack (show n) <> " xs = head xs + last xs"
      , ""
      ]

-- | Helper to apply a fix to source code
applyFix :: Text -> Fix -> Text
applyFix source fix =
  -- Simple implementation: replace the span with new text
  -- For a full implementation, this would use ExactPrint
  let edits = fixEdits fix
      sortedEdits = reverse edits  -- Apply in reverse order to preserve positions
  in foldl applyEdit source sortedEdits
  where
    applyEdit :: Text -> FixEdit -> Text
    applyEdit src (FixEdit span newText) =
      let sourceLines = T.lines src
          startLine = srcSpanStartLineRaw span - 1
          startCol = srcSpanStartColRaw span - 1
          endLine = srcSpanEndLineRaw span - 1
          endCol = srcSpanEndColRaw span - 1
      in if startLine >= 0 && startLine < length sourceLines
         then
           let targetLine = sourceLines !! startLine
               before = T.take startCol targetLine
               after = T.drop endCol targetLine
               newLine = before <> newText <> after
               newLines = take startLine sourceLines ++ [newLine] ++ drop (startLine + 1) sourceLines
           in T.unlines newLines
         else src
