{-# LANGUAGE OverloadedStrings #-}

module SpaceLeaksSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Argus.Rules.SpaceLeaks
import Argus.Types (Diagnostic(..), Severity(..), Fix(..), FixEdit(..), srcSpanStartColRaw)

spec :: Spec
spec = do
  describe "Argus.Rules.SpaceLeaks" $ do
    describe "defaultSpaceLeakConfig" $ do
      it "has enabled set to True by default" $ do
        slcEnabled defaultSpaceLeakConfig `shouldBe` True

      it "has all checks enabled by default" $ do
        slcCheckLazyFolds defaultSpaceLeakConfig `shouldBe` True
        slcCheckLazyState defaultSpaceLeakConfig `shouldBe` True
        slcCheckLazyData defaultSpaceLeakConfig `shouldBe` True
        slcCheckLazyIO defaultSpaceLeakConfig `shouldBe` True
        slcCheckAccumulators defaultSpaceLeakConfig `shouldBe` True
        slcCheckInfinite defaultSpaceLeakConfig `shouldBe` True

    describe "detectSpaceLeaks" $ do
      describe "when disabled" $ do
        it "returns empty list" $ do
          let config = defaultSpaceLeakConfig { slcEnabled = False }
              code = "result = foldl (+) 0 [1..1000000]"
          detectSpaceLeaks config "src/Main.hs" code `shouldBe` []

      describe "lazy fold detection" $ do
        it "detects foldl usage (not foldl')" $ do
          let code = "total = foldl (+) 0 items"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "foldl" . diagMessage) diags `shouldBe` True
          any (T.isInfixOf "thunks" . diagMessage) diags `shouldBe` True

        it "does not flag foldl'" $ do
          let code = "total = foldl' (+) 0 items"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          any (T.isInfixOf "foldl accumulates thunks" . diagMessage) diags `shouldBe` False

        it "does not flag foldl1" $ do
          let code = "maximum = foldl1 max items"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          any (T.isInfixOf "foldl accumulates thunks" . diagMessage) diags `shouldBe` False

        it "finds foldl at correct position (not inside function names)" $ do
          -- The function name contains "foldl" as a substring, but the fix should target
          -- the standalone "foldl" function call, not the substring in "sumWithLazyFold"
          let code = "sumWithLazyFold xs = foldl (+) 0 xs"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          -- Check that fixes target the correct column (foldl at position 22, 1-indexed)
          let fixes = concatMap diagFixes diags
          let edits = concatMap fixEdits fixes
          let relevantEdits = filter (\e -> fixEditNewText e == "foldl'") edits
          length relevantEdits `shouldSatisfy` (> 0)
          -- The span should start at column 22 (where "foldl" is), not column 12 (inside "sumWithLazyFold")
          let correctColumn = any (\e -> srcSpanStartColRaw (fixEditSpan e) >= 20) relevantEdits
          correctColumn `shouldBe` True

        it "detects sum on large ranges" $ do
          let code = "total = sum [1..1000000]"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "sum" . diagMessage) diags `shouldBe` True

        it "detects product on large ranges" $ do
          let code = "result = product [1..100]"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)

      describe "lazy State monad detection" $ do
        it "detects lazy State import" $ do
          let code = "import Control.Monad.State\n\nrun = runState action initState"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "State" . diagMessage) diags `shouldBe` True

        it "does not flag Strict State import" $ do
          let code = "import Control.Monad.State.Strict\n\nrun = runState action initState"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          any (T.isInfixOf "Lazy State monad" . diagMessage) diags `shouldBe` False

        it "does not flag Trans.State.Strict import" $ do
          let code = "import Control.Monad.Trans.State.Strict\n\nrun = runStateT action initState"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          any (T.isInfixOf "Lazy State monad" . diagMessage) diags `shouldBe` False

      describe "lazy data detection" $ do
        it "detects non-strict data fields" $ do
          let code = T.unlines
                [ "data Person = Person"
                , "  { name :: String"
                , "  , age :: Int"
                , "  }"
                ]
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          -- Should detect because no StrictData and no bang patterns
          length diags `shouldSatisfy` (> 0)

        it "does not flag when StrictData is enabled" $ do
          let code = T.unlines
                [ "{-# LANGUAGE StrictData #-}"
                , "data Person = Person"
                , "  { name :: String"
                , "  , age :: Int"
                , "  }"
                ]
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          any (T.isInfixOf "Non-strict data fields" . diagMessage) diags `shouldBe` False

        it "does not flag fields with bang patterns" $ do
          let code = T.unlines
                [ "data Person = Person"
                , "  { name :: !String"
                , "  , age :: !Int"
                , "  }"
                ]
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          -- Fields with ! are strict
          any (T.isInfixOf "Non-strict data fields" . diagMessage) diags `shouldBe` False

      describe "lazy IO detection" $ do
        it "detects getContents usage" $ do
          let code = "input <- getContents"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "getContents" . diagMessage) diags `shouldBe` True
          any (T.isInfixOf "lazy IO" . diagMessage) diags `shouldBe` True

        it "detects hGetContents usage" $ do
          let code = "contents <- hGetContents handle"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "hGetContents" . diagMessage) diags `shouldBe` True

        it "detects Prelude.readFile usage" $ do
          let code = "contents <- readFile \"file.txt\""
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "readFile" . diagMessage) diags `shouldBe` True

        it "does not flag T.readFile (strict Text)" $ do
          let code = "contents <- T.readFile \"file.txt\""
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          any (T.isInfixOf "Prelude.readFile" . diagMessage) diags `shouldBe` False

        it "detects interact usage" $ do
          let code = "main = interact reverse"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "interact" . diagMessage) diags `shouldBe` True

      describe "lazy accumulator detection" $ do
        it "detects non-strict accumulator in recursive helper" $ do
          let code = "loop acc n = if n <= 0 then acc else loop (acc + n) (n - 1)"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "Accumulator" . diagMessage) diags `shouldBe` True

        it "does not flag when $! is used" $ do
          let code = "go acc n = if n <= 0 then acc else go (acc $! n) (n - 1)"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          any (T.isInfixOf "may not be strictly evaluated" . diagMessage) diags `shouldBe` False

        it "does not flag when seq is used" $ do
          let code = "helper acc n = acc `seq` if n <= 0 then acc else helper (acc + n) (n - 1)"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          any (T.isInfixOf "may not be strictly evaluated" . diagMessage) diags `shouldBe` False

      describe "infinite structure detection" $ do
        it "detects reverse on infinite list indicators" $ do
          let code = "lastOnes = reverse [1..]"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "reverse" . diagMessage) diags `shouldBe` True
          any (T.isInfixOf "infinite" . diagMessage) diags `shouldBe` True

        it "detects length on infinite list indicators" $ do
          let code = "count = length [0..]"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "length" . diagMessage) diags `shouldBe` True

        it "detects last on infinite list indicators" $ do
          let code = "x = last $ repeat 1"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "last" . diagMessage) diags `shouldBe` True

        it "detects operations on cycle results" $ do
          let code = "x = last $ cycle [1,2,3]"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)

        it "detects operations on iterate results" $ do
          let code = "x = length $ iterate (+1) 0"
              diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)

      describe "configuration options" $ do
        it "respects slcCheckLazyFolds = False" $ do
          let code = "total = foldl (+) 0 items"
              config = defaultSpaceLeakConfig { slcCheckLazyFolds = False }
              diags = detectSpaceLeaks config "src/Main.hs" code
          any (T.isInfixOf "foldl accumulates thunks" . diagMessage) diags `shouldBe` False

        it "respects slcCheckLazyState = False" $ do
          let code = "import Control.Monad.State\n\nrun = runState action"
              config = defaultSpaceLeakConfig { slcCheckLazyState = False }
              diags = detectSpaceLeaks config "src/Main.hs" code
          any (T.isInfixOf "Lazy State monad" . diagMessage) diags `shouldBe` False

        it "respects slcCheckLazyIO = False" $ do
          let code = "input <- getContents"
              config = defaultSpaceLeakConfig { slcCheckLazyIO = False }
              diags = detectSpaceLeaks config "src/Main.hs" code
          any (T.isInfixOf "getContents uses lazy IO" . diagMessage) diags `shouldBe` False

        it "respects slcCheckInfinite = False" $ do
          let code = "count = length [0..]"
              config = defaultSpaceLeakConfig { slcCheckInfinite = False }
              diags = detectSpaceLeaks config "src/Main.hs" code
          any (T.isInfixOf "infinite" . diagMessage) diags `shouldBe` False

    describe "diagnostic properties" $ do
      it "uses space-leak/ prefix for all codes" $ do
        let code = "total = foldl (+) 0 items"
            diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
        all (maybe False (T.isPrefixOf "space-leak/") . diagCode) diags `shouldBe` True

      it "includes fix suggestions" $ do
        let code = "total = foldl (+) 0 items"
            diags = detectSpaceLeaks defaultSpaceLeakConfig "src/Main.hs" code
        any (T.isInfixOf "foldl'" . diagMessage) diags `shouldBe` True

    describe "SpaceLeakCategory" $ do
      it "has all expected categories" $ do
        minBound `shouldBe` LazyFold
        maxBound `shouldBe` InfiniteStructure
