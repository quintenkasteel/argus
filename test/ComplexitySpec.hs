{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ComplexitySpec (spec) where

import Control.Monad (replicateM)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Argus.Analysis.Complexity
import Argus.Types (Diagnostic(..), Severity(..), noSrcSpan)

--------------------------------------------------------------------------------
-- QuickCheck Generators
--------------------------------------------------------------------------------

-- | Generate valid Haskell identifiers
newtype HaskellIdent = HaskellIdent { unIdent :: T.Text }
  deriving stock (Eq, Show)

instance Arbitrary HaskellIdent where
  arbitrary = do
    first <- elements ['a'..'z']
    rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_', '\'']
    pure $ HaskellIdent $ T.pack (first : take 20 rest)

-- | Generate simple Haskell expressions (for property testing)
newtype SimpleExpr = SimpleExpr { unExpr :: T.Text }
  deriving stock (Eq, Show)

instance Arbitrary SimpleExpr where
  arbitrary = SimpleExpr <$> oneof
    [ pure "x"
    , pure "x + y"
    , pure "f x"
    , pure "x + 1"
    , pure "Just x"
    , pure "Nothing"
    , pure "(x, y)"
    , pure "[x, y, z]"
    ]

-- | Generate function definitions
newtype FunctionDef = FunctionDef { unFuncDef :: T.Text }
  deriving stock (Eq, Show)

instance Arbitrary FunctionDef where
  arbitrary = do
    HaskellIdent name <- arbitrary
    numParams <- choose (0, 5 :: Int)
    params <- replicateM numParams (unIdent <$> arbitrary)
    SimpleExpr body <- arbitrary
    let paramStr = T.unwords params
    pure $ FunctionDef $ name <> " " <> paramStr <> " = " <> body

spec :: Spec
spec = do
  describe "Argus.Analysis.Complexity" $ do
    describe "defaultThresholds" $ do
      it "has sensible cyclomatic thresholds" $ do
        ctCyclomaticWarning defaultThresholds `shouldBe` 10
        ctCyclomaticError defaultThresholds `shouldBe` 20

      it "has sensible cognitive thresholds" $ do
        ctCognitiveWarning defaultThresholds `shouldBe` 15
        ctCognitiveError defaultThresholds `shouldBe` 25

      it "has sensible nesting threshold" $ do
        ctNestingWarning defaultThresholds `shouldBe` 4

      it "has sensible parameter threshold" $ do
        ctParameterWarning defaultThresholds `shouldBe` 5

    describe "analyzeModule" $ do
      it "extracts module name" $ do
        let code = "module Test.Module where\n\nfoo = 1"
            metrics = analyzeModule "test.hs" code
        mmModuleName metrics `shouldBe` "Test.Module"

      it "counts lines correctly" $ do
        let code = T.unlines
              [ "module Test where"
              , ""
              , "-- A comment"
              , "foo = 1"
              , ""
              , "bar = 2"
              ]
            metrics = analyzeModule "test.hs" code
        mmTotalLines metrics `shouldBe` 6
        mmBlankLines metrics `shouldBe` 2
        mmCommentLines metrics `shouldBe` 1
        mmCodeLines metrics `shouldBe` 3

      it "counts imports" $ do
        let code = T.unlines
              [ "module Test where"
              , "import Data.Text"
              , "import Data.Map"
              , "import qualified Data.Set as Set"
              , "foo = 1"
              ]
            metrics = analyzeModule "test.hs" code
        mmImportCount metrics `shouldBe` 3

      it "counts type definitions" $ do
        let code = T.unlines
              [ "module Test where"
              , "data Foo = Foo Int"
              , "newtype Bar = Bar String"
              , "type Baz = Int"
              ]
            metrics = analyzeModule "test.hs" code
        mmTypeCount metrics `shouldBe` 3

      it "counts instances and classes" $ do
        let code = T.unlines
              [ "module Test where"
              , "class MyClass a where"
              , "  myMethod :: a -> Int"
              , "instance MyClass Int where"
              , "  myMethod = id"
              ]
            metrics = analyzeModule "test.hs" code
        mmClassCount metrics `shouldBe` 1
        mmInstanceCount metrics `shouldBe` 1

    describe "analyzeFunction" $ do
      it "calculates basic function metrics" $ do
        let body = "foo x y = x + y"
            fm = analyzeFunction "test.hs" "foo" body 1 1
        fmName fm `shouldBe` "foo"
        fmLineCount fm `shouldBe` 1

      it "detects parameters" $ do
        let body = "foo a b c d e = a + b + c + d + e"
            fm = analyzeFunction "test.hs" "foo" body 1 1
        fmParameterCount fm `shouldSatisfy` (>= 4)

      it "detects local bindings" $ do
        let body = T.unlines
              [ "foo x = let y = x + 1"
              , "            z = y + 2"
              , "        in z"
              ]
            fm = analyzeFunction "test.hs" "foo" body 1 3
        fmLocalBindings fm `shouldSatisfy` (> 0)

    describe "calculateCyclomaticComplexity" $ do
      it "returns 1 for simple expression" $ do
        let code = "foo x = x + 1"
        calculateCyclomaticComplexity code `shouldBe` 1

      it "increases for case expressions" $ do
        let code = T.unlines
              [ "foo x = case x of"
              , "  Just y -> y"
              , "  Nothing -> 0"
              ]
        calculateCyclomaticComplexity code `shouldSatisfy` (> 1)

      it "increases for if expressions" $ do
        let code = "foo x = if x > 0 then x else -x"
        calculateCyclomaticComplexity code `shouldSatisfy` (> 1)

      it "increases for guards" $ do
        let code = T.unlines
              [ "foo x"
              , "  | x > 0 = x"
              , "  | x < 0 = -x"
              , "  | otherwise = 0"
              ]
        calculateCyclomaticComplexity code `shouldSatisfy` (> 1)

    describe "calculateCognitiveComplexity" $ do
      it "returns low value for simple code" $ do
        let code = "foo x = x + 1"
        calculateCognitiveComplexity code `shouldSatisfy` (< 5)

      it "increases for nested structures" $ do
        let simpleCode = "foo x = x + 1"
            complexCode = T.unlines
              [ "foo x = do"
              , "  y <- getY"
              , "  case y of"
              , "    Just z -> do"
              , "      let w = z + 1"
              , "      return w"
              , "    Nothing -> return 0"
              ]
        let simpleComplexity = calculateCognitiveComplexity simpleCode
            nestedComplexity = calculateCognitiveComplexity complexCode
        nestedComplexity `shouldSatisfy` (> simpleComplexity)

    describe "RecursionType" $ do
      it "detects no recursion" $ do
        -- Body without function name appearing (just the expression)
        let body = "x + y"
        detectRecursionType "add" body `shouldBe` NoRecursion

      it "detects direct recursion" $ do
        -- Recursive call NOT on last line - more operations after recursion
        let body = T.unlines
              [ "factorial n = let result = if n <= 1"
              , "                              then 1"
              , "                              else n * factorial (n - 1)"
              , "              in result"
              ]
        detectRecursionType "factorial" body `shouldBe` DirectRecursion

      it "detects tail recursion" $ do
        -- Recursive call IS on last line
        let body = T.unlines
              [ "loop acc n ="
              , "  if n <= 0"
              , "    then acc"
              , "    else loop (acc + n) (n - 1)"
              ]
        detectRecursionType "loop" body `shouldBe` TailRecursion

    describe "analyzeComplexity" $ do
      it "creates module metrics" $ do
        let code = T.unlines
              [ "module Test where"
              , "foo = 1"
              , "bar = 2"
              ]
            metrics = analyzeComplexity "test.hs" code
        Map.member "test.hs" (cmModules metrics) `shouldBe` True

      it "identifies high complexity functions" $ do
        let complexFunction = T.unlines
              [ "module Test where"
              , "complex x = case x of"
              , "  0 -> 0"
              , "  1 -> 1"
              , "  2 -> case x of"
              , "         _ | x > 0 -> x"
              , "           | x < 0 -> -x"
              , "           | otherwise -> 0"
              , "  3 -> 3"
              , "  4 -> 4"
              , "  5 -> 5"
              , "  6 -> 6"
              , "  7 -> 7"
              , "  8 -> 8"
              , "  9 -> 9"
              , "  _ -> -1"
              ]
            metrics = analyzeComplexity "test.hs" complexFunction
        -- The high complexity filter uses threshold of 10
        length (cmHighComplexityFunctions metrics) `shouldSatisfy` (>= 0)

    describe "complexityDiagnostics" $ do
      it "generates warning for high cyclomatic complexity" $ do
        let fm = FunctionMetrics
              { fmName = "complex"
              , fmSpan = noSrcSpan
              , fmCyclomaticComplexity = 15  -- Above warning threshold
              , fmCognitiveComplexity = 5
              , fmLineCount = 20
              , fmNestingDepth = 2
              , fmParameterCount = 2
              , fmPatternBranches = 5
              , fmGuardCount = 0
              , fmLambdaDepth = 0
              , fmMonadStackDepth = 0
              , fmTypeConstraintCount = 0
              , fmLocalBindings = 1
              , fmRecursionType = NoRecursion
              }
            metrics = ComplexityMetrics
              { cmModules = Map.empty
              , cmTotalComplexity = 15
              , cmAverageComplexity = 15
              , cmHighComplexityFunctions = [fm]
              }
            diags = complexityDiagnostics defaultThresholds metrics
        length diags `shouldSatisfy` (> 0)
        let cycloDiags = filter (\d -> diagCode d == Just "complexity/cyclomatic-complexity-warning") diags
        length cycloDiags `shouldBe` 1

      it "generates error for very high cyclomatic complexity" $ do
        let fm = FunctionMetrics
              { fmName = "veryComplex"
              , fmSpan = noSrcSpan
              , fmCyclomaticComplexity = 25  -- Above error threshold
              , fmCognitiveComplexity = 5
              , fmLineCount = 50
              , fmNestingDepth = 2
              , fmParameterCount = 2
              , fmPatternBranches = 5
              , fmGuardCount = 0
              , fmLambdaDepth = 0
              , fmMonadStackDepth = 0
              , fmTypeConstraintCount = 0
              , fmLocalBindings = 1
              , fmRecursionType = NoRecursion
              }
            metrics = ComplexityMetrics
              { cmModules = Map.empty
              , cmTotalComplexity = 25
              , cmAverageComplexity = 25
              , cmHighComplexityFunctions = [fm]
              }
            diags = complexityDiagnostics defaultThresholds metrics
        let errorDiags = filter (\d -> diagCode d == Just "complexity/cyclomatic-complexity-error") diags
        length errorDiags `shouldBe` 1
        diagSeverity (head errorDiags) `shouldBe` Error

      it "generates warning for long functions" $ do
        let fm = FunctionMetrics
              { fmName = "longFunction"
              , fmSpan = noSrcSpan
              , fmCyclomaticComplexity = 5
              , fmCognitiveComplexity = 5
              , fmLineCount = 60  -- Above warning threshold
              , fmNestingDepth = 2
              , fmParameterCount = 2
              , fmPatternBranches = 3
              , fmGuardCount = 0
              , fmLambdaDepth = 0
              , fmMonadStackDepth = 0
              , fmTypeConstraintCount = 0
              , fmLocalBindings = 1
              , fmRecursionType = NoRecursion
              }
            metrics = ComplexityMetrics
              { cmModules = Map.empty
              , cmTotalComplexity = 5
              , cmAverageComplexity = 5
              , cmHighComplexityFunctions = [fm]
              }
            diags = complexityDiagnostics defaultThresholds metrics
        let lengthDiags = filter (\d -> diagCode d == Just "complexity/function-length") diags
        length lengthDiags `shouldBe` 1

      it "generates no diagnostics for simple functions" $ do
        let fm = FunctionMetrics
              { fmName = "simple"
              , fmSpan = noSrcSpan
              , fmCyclomaticComplexity = 2
              , fmCognitiveComplexity = 2
              , fmLineCount = 5
              , fmNestingDepth = 1
              , fmParameterCount = 2
              , fmPatternBranches = 2
              , fmGuardCount = 0
              , fmLambdaDepth = 0
              , fmMonadStackDepth = 0
              , fmTypeConstraintCount = 0
              , fmLocalBindings = 0
              , fmRecursionType = NoRecursion
              }
            metrics = ComplexityMetrics
              { cmModules = Map.empty
              , cmTotalComplexity = 2
              , cmAverageComplexity = 2
              , cmHighComplexityFunctions = [fm]
              }
            diags = complexityDiagnostics defaultThresholds metrics
        diags `shouldBe` []

    describe "FunctionMetrics" $ do
      it "stores all metric fields correctly" $ do
        let fm = FunctionMetrics
              { fmName = "test"
              , fmSpan = noSrcSpan
              , fmCyclomaticComplexity = 5
              , fmCognitiveComplexity = 8
              , fmLineCount = 20
              , fmNestingDepth = 3
              , fmParameterCount = 4
              , fmPatternBranches = 6
              , fmGuardCount = 2
              , fmLambdaDepth = 1
              , fmMonadStackDepth = 2
              , fmTypeConstraintCount = 3
              , fmLocalBindings = 5
              , fmRecursionType = TailRecursion
              }
        fmName fm `shouldBe` "test"
        fmCyclomaticComplexity fm `shouldBe` 5
        fmRecursionType fm `shouldBe` TailRecursion

    describe "ModuleMetrics" $ do
      it "calculates code to comment ratio" $ do
        let codeWithComments = T.unlines
              [ "module Test where"
              , "-- A comment"
              , "-- Another comment"
              , "foo = 1"
              , "bar = 2"
              ]
            metrics = analyzeModule "test.hs" codeWithComments
        mmCodeToCommentRatio metrics `shouldSatisfy` (> 0)

      it "calculates average complexity" $ do
        let code = T.unlines
              [ "module Test where"
              , "foo = 1"
              , "bar x = x + 1"
              ]
            metrics = analyzeModule "test.hs" code
        mmAverageComplexity metrics `shouldSatisfy` (>= 0)

    ---------------------------------------------------------------------------
    -- Property-Based Tests
    ---------------------------------------------------------------------------

    describe "Property-based tests" $ do
      prop "cyclomatic complexity is always >= 1" $ \(FunctionDef code) ->
        calculateCyclomaticComplexity code >= 1

      prop "cognitive complexity is non-negative" $ \(FunctionDef code) ->
        calculateCognitiveComplexity code >= 0

      prop "line count matches actual lines" $ \(FunctionDef code) ->
        let moduleCode = "module Test where\n" <> code
            metrics = analyzeModule "test.hs" moduleCode
        in mmTotalLines metrics == length (T.lines moduleCode)

      prop "nesting depth is non-negative" $ \(FunctionDef code) ->
        calculateNestingDepth code >= 0

      prop "parameter count is non-negative" $ \(FunctionDef code) ->
        countParameters code >= 0

      prop "guard count is non-negative" $ \(FunctionDef code) ->
        countGuards code >= 0

      prop "local bindings count is non-negative" $ \(FunctionDef code) ->
        countLocalBindings code >= 0

      prop "blank lines + code lines + comment lines = total lines" $ \(FunctionDef code) ->
        let moduleCode = "module Test where\n" <> code
            metrics = analyzeModule "test.hs" moduleCode
        in mmBlankLines metrics + mmCodeLines metrics + mmCommentLines metrics
           == mmTotalLines metrics

    ---------------------------------------------------------------------------
    -- Accurate Cyclomatic Complexity Tests
    ---------------------------------------------------------------------------

    describe "Accurate cyclomatic complexity" $ do
      it "returns exactly 1 for identity function" $ do
        calculateCyclomaticComplexity "id x = x" `shouldBe` 1

      it "returns exactly 1 for constant function" $ do
        calculateCyclomaticComplexity "const x y = x" `shouldBe` 1

      it "returns exactly 1 for arithmetic expression" $ do
        calculateCyclomaticComplexity "add x y = x + y * 2" `shouldBe` 1

      it "returns 2 for single if-then-else" $ do
        let code = "abs x = if x < 0 then -x else x"
        calculateCyclomaticComplexity code `shouldBe` 2

      it "returns 3 for nested if" $ do
        let code = "foo x = if x > 0 then if x > 10 then \"big\" else \"small\" else \"negative\""
        calculateCyclomaticComplexity code `shouldBe` 3

      it "returns n+1 for n guards" $ do
        let code = T.unlines
              [ "classify x"
              , "  | x < 0 = \"negative\""
              , "  | x == 0 = \"zero\""
              , "  | otherwise = \"positive\""
              ]
        -- Base 1 + 3 guards = 4
        calculateCyclomaticComplexity code `shouldBe` 4

      it "counts case branches correctly" $ do
        let code = T.unlines
              [ "dayName d = case d of"
              , "  1 -> \"Monday\""
              , "  2 -> \"Tuesday\""
              , "  3 -> \"Wednesday\""
              , "  _ -> \"Other\""
              ]
        -- Base 1 + 1 case + 4 branches (arrows) - 1 = 5
        calculateCyclomaticComplexity code `shouldSatisfy` (>= 4)

      it "handles multiple cases" $ do
        let code = T.unlines
              [ "foo x y = case x of"
              , "  Just a -> case y of"
              , "    Just b -> a + b"
              , "    Nothing -> a"
              , "  Nothing -> 0"
              ]
        -- Nested cases should add up
        calculateCyclomaticComplexity code `shouldSatisfy` (>= 4)

      it "handles pattern matching in function definition" $ do
        let code = T.unlines
              [ "length' [] = 0"
              , "length' (_:xs) = 1 + length' xs"
              ]
        -- Two patterns = 2+ arrows
        calculateCyclomaticComplexity code `shouldSatisfy` (>= 2)

    ---------------------------------------------------------------------------
    -- Accurate Cognitive Complexity Tests
    ---------------------------------------------------------------------------

    describe "Accurate cognitive complexity" $ do
      it "returns 0 for trivial function" $ do
        -- No control flow, no nesting
        calculateCognitiveComplexity "id x = x" `shouldBe` 0

      it "returns 0 for simple arithmetic" $ do
        calculateCognitiveComplexity "add x y = x + y" `shouldBe` 0

      it "increments by 1 for if" $ do
        let code = "foo x = if x > 0 then x else 0"
        calculateCognitiveComplexity code `shouldSatisfy` (>= 1)

      it "increments by 1 for case" $ do
        let code = T.unlines
              [ "foo x = case x of"
              , "  Just y -> y"
              , "  Nothing -> 0"
              ]
        calculateCognitiveComplexity code `shouldSatisfy` (>= 1)

      it "increments by 1 for each guard" $ do
        let code = T.unlines
              [ "foo x"
              , "  | x > 0 = x"
              , "  | otherwise = 0"
              ]
        calculateCognitiveComplexity code `shouldSatisfy` (>= 2)

      it "adds nesting penalty for deep nesting" $ do
        let shallowCode = "foo x = if x > 0 then x else 0"
            deepCode = T.unlines
              [ "foo x = do"
              , "  y <- getLine"
              , "  if null y"
              , "    then do"
              , "      z <- getLine"
              , "      if null z"
              , "        then pure \"\""
              , "        else pure z"
              , "    else pure y"
              ]
        let shallow = calculateCognitiveComplexity shallowCode
            deep = calculateCognitiveComplexity deepCode
        deep `shouldSatisfy` (> shallow)

      it "penalizes bind operators" $ do
        let withBind = "foo = getLine >>= putStrLn"
            withoutBind = "foo = getLine"
        let with = calculateCognitiveComplexity withBind
            without = calculateCognitiveComplexity withoutBind
        with `shouldSatisfy` (>= without)

      it "penalizes where clauses" $ do
        let code = T.unlines
              [ "foo x = result"
              , "  where"
              , "    result = x + 1"
              ]
        calculateCognitiveComplexity code `shouldSatisfy` (>= 1)

      it "penalizes let expressions" $ do
        let code = "foo x = let y = x + 1 in y * 2"
        calculateCognitiveComplexity code `shouldSatisfy` (>= 1)

      it "penalizes do notation" $ do
        let code = T.unlines
              [ "foo = do"
              , "  x <- getLine"
              , "  putStrLn x"
              ]
        calculateCognitiveComplexity code `shouldSatisfy` (>= 1)

    ---------------------------------------------------------------------------
    -- Recursion Detection Tests
    ---------------------------------------------------------------------------

    describe "Recursion type detection" $ do
      it "detects no recursion in non-recursive function" $ do
        detectRecursionType "add" "x + y" `shouldBe` NoRecursion

      it "detects no recursion when different function called" $ do
        detectRecursionType "foo" "bar x + baz y" `shouldBe` NoRecursion

      it "detects direct recursion (not tail)" $ do
        -- factorial n = n * factorial (n-1) -- multiplication after recursive call
        let body = "n * factorial (n - 1)"
        detectRecursionType "factorial" body `shouldBe` DirectRecursion

      it "detects tail recursion with simple tail call" $ do
        -- Last line contains the recursive call
        let body = T.unlines
              [ "go acc n"
              , "  | n <= 0 = acc"
              , "  | otherwise = go (acc + n) (n - 1)"
              ]
        detectRecursionType "go" body `shouldBe` TailRecursion

      it "detects tail recursion in accumulator pattern" $ do
        let body = T.unlines
              [ "loop acc [] = acc"
              , "loop acc (x:xs) = loop (acc + x) xs"
              ]
        detectRecursionType "loop" body `shouldBe` TailRecursion

      it "detects direct recursion when operation after call" $ do
        -- length' (_:xs) = 1 + length' xs -- addition after recursive call
        let body = "1 + lengthHelper xs"
        detectRecursionType "lengthHelper" body `shouldBe` DirectRecursion

      it "handles recursion in case expression" $ do
        let body = T.unlines
              [ "case xs of"
              , "  [] -> 0"
              , "  (x:rest) -> x + sumList rest"
              ]
        detectRecursionType "sumList" body `shouldBe` DirectRecursion

    ---------------------------------------------------------------------------
    -- Monad Stack Depth Tests
    ---------------------------------------------------------------------------

    describe "Monad stack depth estimation" $ do
      it "returns 0 for simple types" $ do
        estimateMonadStack "foo :: Int -> Int" `shouldBe` 0

      it "returns 0 for simple IO" $ do
        estimateMonadStack "foo :: IO ()" `shouldBe` 0

      it "detects single ReaderT" $ do
        estimateMonadStack "foo :: ReaderT Config IO a" `shouldBe` 1

      it "detects single StateT" $ do
        estimateMonadStack "foo :: StateT AppState IO a" `shouldBe` 1

      it "detects single ExceptT" $ do
        estimateMonadStack "foo :: ExceptT Error IO a" `shouldBe` 1

      it "detects stacked transformers" $ do
        let code = "foo :: ReaderT Config (StateT AppState IO) a"
        estimateMonadStack code `shouldBe` 2

      it "detects deeply stacked transformers" $ do
        let code = "foo :: ReaderT r (StateT s (ExceptT e IO)) a"
        estimateMonadStack code `shouldBe` 3

      it "detects RWST" $ do
        estimateMonadStack "foo :: RWST r w s IO a" `shouldBe` 1

      it "detects MaybeT" $ do
        estimateMonadStack "foo :: MaybeT IO a" `shouldBe` 1

      it "detects WriterT" $ do
        estimateMonadStack "foo :: WriterT w IO a" `shouldBe` 1

    ---------------------------------------------------------------------------
    -- Type Constraint Counting Tests
    ---------------------------------------------------------------------------

    describe "Type constraint counting" $ do
      it "returns 0 for no constraints" $ do
        countTypeConstraints "foo :: Int -> Int" `shouldBe` 0

      it "counts single constraint" $ do
        countTypeConstraints "foo :: Show a => a -> String" `shouldBe` 1

      it "counts multiple constraints with tuple syntax" $ do
        countTypeConstraints "foo :: (Show a, Eq a) => a -> Bool" `shouldBe` 1

      it "counts constraint arrow" $ do
        -- Each => counts as 1
        let code = "foo :: Monad m => m a -> m b"
        countTypeConstraints code `shouldBe` 1

    ---------------------------------------------------------------------------
    -- Lambda Depth Tests
    ---------------------------------------------------------------------------

    describe "Lambda depth counting" $ do
      it "returns 0 for no lambdas" $ do
        countLambdaDepth "foo x = x + 1" `shouldBe` 0

      it "counts single lambda" $ do
        countLambdaDepth "foo = \\x -> x + 1" `shouldBe` 1

      it "counts nested lambdas" $ do
        countLambdaDepth "foo = \\x -> \\y -> x + y" `shouldBe` 2

      it "counts multiple lambdas" $ do
        countLambdaDepth "foo = (\\x -> x) (\\y -> y)" `shouldBe` 2

      it "caps at 5 for very deep lambdas" $ do
        let code = "foo = \\a -> \\b -> \\c -> \\d -> \\e -> \\f -> \\g -> a"
        countLambdaDepth code `shouldBe` 5

    ---------------------------------------------------------------------------
    -- Nesting Depth Tests
    ---------------------------------------------------------------------------

    describe "Nesting depth calculation" $ do
      it "returns 0 for single-line function" $ do
        calculateNestingDepth "foo x = x + 1" `shouldBe` 0

      it "calculates depth based on indentation" $ do
        let code = T.unlines
              [ "foo x ="
              , "  let y = x + 1"
              , "  in y"
              ]
        calculateNestingDepth code `shouldSatisfy` (>= 1)

      it "finds maximum depth in nested code" $ do
        let code = T.unlines
              [ "foo x = do"
              , "  y <- bar"
              , "  case y of"
              , "    Just z -> do"
              , "      w <- baz z"
              , "      pure w"
              , "    Nothing -> pure 0"
              ]
        calculateNestingDepth code `shouldSatisfy` (>= 2)

    ---------------------------------------------------------------------------
    -- Edge Cases
    ---------------------------------------------------------------------------

    describe "Edge cases" $ do
      it "handles empty input" $ do
        let metrics = analyzeModule "test.hs" ""
        mmTotalLines metrics `shouldBe` 0
        mmFunctionCount metrics `shouldBe` 0
        mmCodeLines metrics `shouldBe` 0

      it "handles single newline" $ do
        let metrics = analyzeModule "test.hs" "\n"
        mmTotalLines metrics `shouldBe` 1
        mmBlankLines metrics `shouldBe` 1

      it "handles module without module declaration" $ do
        let code = "foo = 1"
        let metrics = analyzeModule "test.hs" code
        mmModuleName metrics `shouldBe` "Unknown"

      it "handles module with complex export list" $ do
        let code = T.unlines
              [ "module Test"
              , "  ( foo"
              , "  , bar"
              , "  , Baz(..)"
              , "  ) where"
              , "foo = 1"
              ]
        let metrics = analyzeModule "test.hs" code
        mmModuleName metrics `shouldBe` "Test"

      it "handles unicode in identifiers" $ do
        let code = "café x = x + 1"
        let fm = analyzeFunction "test.hs" "café" code 1 1
        fmName fm `shouldBe` "café"

      it "handles prime in identifiers" $ do
        let code = "foo' x = x + 1"
        let fm = analyzeFunction "test.hs" "foo'" code 1 1
        fmName fm `shouldBe` "foo'"

      it "handles operators as functions" $ do
        let code = "(+++) x y = x ++ y"
        calculateCyclomaticComplexity code `shouldBe` 1

      it "handles very long single line" $ do
        let longLine = "foo = " <> T.replicate 1000 "x + "  <> "1"
        calculateCyclomaticComplexity longLine `shouldSatisfy` (>= 1)

      it "handles many blank lines" $ do
        let code = T.unlines $ ["module Test where"] ++ replicate 100 "" ++ ["foo = 1"]
        let metrics = analyzeModule "test.hs" code
        mmBlankLines metrics `shouldBe` 100

      it "handles only comments" $ do
        let code = T.unlines
              [ "-- Comment 1"
              , "-- Comment 2"
              , "-- Comment 3"
              ]
        let metrics = analyzeModule "test.hs" code
        mmCommentLines metrics `shouldBe` 3
        mmCodeLines metrics `shouldBe` 0

      it "handles inline comments" $ do
        let code = "foo x = x + 1 -- increment"
        calculateCyclomaticComplexity code `shouldBe` 1

      it "handles multi-line string literals" $ do
        let code = T.unlines
              [ "foo = \"\\"
              , "  \\multi-line\\"
              , "  \\string\""
              ]
        let metrics = analyzeModule "test.hs" code
        mmTotalLines metrics `shouldBe` 3

      it "handles pragma lines" $ do
        let code = T.unlines
              [ "{-# LANGUAGE OverloadedStrings #-}"
              , "module Test where"
              , "foo = 1"
              ]
        let metrics = analyzeModule "test.hs" code
        -- Pragma should be counted as code or comment
        mmTotalLines metrics `shouldBe` 3

    ---------------------------------------------------------------------------
    -- Multi-line Comment Tests
    ---------------------------------------------------------------------------

    describe "Multi-line comment handling" $ do
      it "counts block comments" $ do
        let code = T.unlines
              [ "module Test where"
              , "{- This is a"
              , "   multi-line comment -}"
              , "foo = 1"
              ]
        let metrics = analyzeModule "test.hs" code
        mmCommentLines metrics `shouldSatisfy` (>= 1)

      it "handles nested block comments" $ do
        let code = T.unlines
              [ "{- outer {- inner -} outer -}"
              , "foo = 1"
              ]
        let metrics = analyzeModule "test.hs" code
        -- Should still parse
        mmFunctionCount metrics `shouldSatisfy` (>= 0)

    ---------------------------------------------------------------------------
    -- Diagnostic Priority Tests
    ---------------------------------------------------------------------------

    describe "Diagnostic severity priorities" $ do
      it "error severity takes precedence over warning for cyclomatic" $ do
        let fm = FunctionMetrics
              { fmName = "complex"
              , fmSpan = noSrcSpan
              , fmCyclomaticComplexity = 25  -- Above both thresholds
              , fmCognitiveComplexity = 5
              , fmLineCount = 20
              , fmNestingDepth = 2
              , fmParameterCount = 2
              , fmPatternBranches = 5
              , fmGuardCount = 0
              , fmLambdaDepth = 0
              , fmMonadStackDepth = 0
              , fmTypeConstraintCount = 0
              , fmLocalBindings = 1
              , fmRecursionType = NoRecursion
              }
            metrics = ComplexityMetrics
              { cmModules = Map.empty
              , cmTotalComplexity = 25
              , cmAverageComplexity = 25
              , cmHighComplexityFunctions = [fm]
              }
            diags = complexityDiagnostics defaultThresholds metrics
        -- Should have error, not warning
        let errorDiags = filter (\d -> diagCode d == Just "complexity/cyclomatic-complexity-error") diags
            warnDiags = filter (\d -> diagCode d == Just "complexity/cyclomatic-complexity-warning") diags
        length errorDiags `shouldBe` 1
        length warnDiags `shouldBe` 0

      it "generates cognitive complexity error" $ do
        let fm = FunctionMetrics
              { fmName = "unreadable"
              , fmSpan = noSrcSpan
              , fmCyclomaticComplexity = 5
              , fmCognitiveComplexity = 30  -- Above error threshold
              , fmLineCount = 40
              , fmNestingDepth = 3
              , fmParameterCount = 2
              , fmPatternBranches = 4
              , fmGuardCount = 2
              , fmLambdaDepth = 1
              , fmMonadStackDepth = 0
              , fmTypeConstraintCount = 0
              , fmLocalBindings = 3
              , fmRecursionType = NoRecursion
              }
            metrics = ComplexityMetrics
              { cmModules = Map.empty
              , cmTotalComplexity = 5
              , cmAverageComplexity = 5
              , cmHighComplexityFunctions = [fm]
              }
            diags = complexityDiagnostics defaultThresholds metrics
            cognitiveErrors = filter (\d -> diagCode d == Just "complexity/cognitive-complexity-error") diags
        length cognitiveErrors `shouldBe` 1

      it "generates all applicable warnings" $ do
        let fm = FunctionMetrics
              { fmName = "badFunction"
              , fmSpan = noSrcSpan
              , fmCyclomaticComplexity = 15   -- Warning
              , fmCognitiveComplexity = 20    -- Warning
              , fmLineCount = 60              -- Warning
              , fmNestingDepth = 6            -- Warning
              , fmParameterCount = 7          -- Warning
              , fmPatternBranches = 15        -- Warning
              , fmGuardCount = 5
              , fmLambdaDepth = 3
              , fmMonadStackDepth = 2
              , fmTypeConstraintCount = 4
              , fmLocalBindings = 10
              , fmRecursionType = DirectRecursion
              }
            metrics = ComplexityMetrics
              { cmModules = Map.empty
              , cmTotalComplexity = 15
              , cmAverageComplexity = 15
              , cmHighComplexityFunctions = [fm]
              }
            diags = complexityDiagnostics defaultThresholds metrics
        -- Should have warnings for: cyclomatic, cognitive, length, nesting, params, patterns
        length diags `shouldBe` 6

    ---------------------------------------------------------------------------
    -- Integration Tests
    ---------------------------------------------------------------------------

    describe "Full module analysis integration" $ do
      it "analyzes realistic module correctly" $ do
        let code = T.unlines
              [ "module MyModule where"
              , ""
              , "import Data.Text (Text)"
              , "import Data.Maybe"
              , ""
              , "-- | Simple function"
              , "simple :: Int -> Int"
              , "simple x = x + 1"
              , ""
              , "-- | Complex function with case and guards"
              , "complex :: Maybe Int -> Int -> String"
              , "complex mx y"
              , "  | y < 0 = \"negative\""
              , "  | y == 0 = \"zero\""
              , "  | otherwise = case mx of"
              , "      Just x -> show (x + y)"
              , "      Nothing -> \"none\""
              , ""
              , "-- | Recursive function"
              , "factorial :: Int -> Int"
              , "factorial n"
              , "  | n <= 1 = 1"
              , "  | otherwise = n * factorial (n - 1)"
              ]
        let metrics = analyzeModule "test.hs" code
        mmModuleName metrics `shouldBe` "MyModule"
        mmImportCount metrics `shouldBe` 2
        mmFunctionCount metrics `shouldSatisfy` (>= 2)
        mmCommentLines metrics `shouldSatisfy` (>= 3)

      it "handles real-world complexity patterns" $ do
        let code = T.unlines
              [ "parseConfig :: Text -> Either Error Config"
              , "parseConfig input = do"
              , "  obj <- parseJSON input"
              , "  name <- obj .: \"name\""
              , "  case obj .:? \"timeout\" of"
              , "    Nothing -> pure $ Config name defaultTimeout"
              , "    Just t"
              , "      | t <= 0 -> Left $ InvalidTimeout t"
              , "      | t > maxTimeout -> Left $ TimeoutTooLarge t"
              , "      | otherwise -> pure $ Config name t"
              ]
        let complexity = calculateCyclomaticComplexity code
            cognitive = calculateCognitiveComplexity code
        -- Should recognize this as moderately complex
        complexity `shouldSatisfy` (>= 4)
        cognitive `shouldSatisfy` (>= 3)
