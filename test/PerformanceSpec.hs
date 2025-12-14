{-# LANGUAGE OverloadedStrings #-}

module PerformanceSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Argus.Rules.Performance
import Argus.Types (Diagnostic(..), Severity(..), Fix(..), FixEdit(..))

spec :: Spec
spec = do
  describe "Argus.Rules.Performance" $ do
    describe "defaultPerformanceConfig" $ do
      it "has enabled set to True by default" $ do
        perfEnabled defaultPerformanceConfig `shouldBe` True

      it "has all checks enabled by default" $ do
        perfCheckDataStructures defaultPerformanceConfig `shouldBe` True
        perfCheckAlgorithms defaultPerformanceConfig `shouldBe` True
        perfCheckStrings defaultPerformanceConfig `shouldBe` True
        perfCheckLazy defaultPerformanceConfig `shouldBe` True
        perfCheckAllocation defaultPerformanceConfig `shouldBe` True
        perfCheckIO defaultPerformanceConfig `shouldBe` True
        perfCheckFusion defaultPerformanceConfig `shouldBe` True

    describe "detectPerformanceIssues" $ do
      describe "when disabled" $ do
        it "returns empty list" $ do
          let config = defaultPerformanceConfig { perfEnabled = False }
              code = "xs = nub [1,2,3]"
          detectPerformanceIssues config "src/Main.hs" code `shouldBe` []

      describe "data structure issues" $ do
        it "detects nub usage (O(n^2))" $ do
          let code = "unique = nub [1,2,3,1,2]"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "nub" . diagMessage) diags `shouldBe` True
          any (T.isInfixOf "O(n" . diagMessage) diags `shouldBe` True

        it "detects sort . nub pattern" $ do
          let code = "sorted = sort . nub $ items"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "sort" . diagMessage) diags `shouldBe` True

        it "detects elem usage" $ do
          let code = "found = elem x list"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "elem" . diagMessage) diags `shouldBe` True

        it "detects ++ in foldl" $ do
          let code = "result = foldl (\\acc x -> acc ++ [x]) [] items"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "++" . diagMessage) diags `shouldBe` True

        it "detects length . filter pattern" $ do
          let code = "count = length $ filter predicate items"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "length" . diagMessage) diags `shouldBe` True

      describe "algorithmic issues" $ do
        it "detects reverse + take pattern" $ do
          let code = "lastN = take n . reverse $ items"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "reverse" . diagMessage) diags `shouldBe` True

      describe "string usage issues" $ do
        it "detects String type annotations when no Text import" $ do
          let code = "name :: String\nname = \"test\""
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "String" . diagMessage) diags `shouldBe` True

        it "does not flag String when Text is imported" $ do
          let code = "import Data.Text\nname :: String\nname = \"test\""
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          -- Should skip String detection if Text is already imported
          any (T.isInfixOf "String is [Char]" . diagMessage) diags `shouldBe` False

        it "detects readFile (lazy version)" $ do
          let code = "contents <- readFile \"file.txt\""
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "readFile" . diagMessage) diags `shouldBe` True

      describe "lazy vs strict issues" $ do
        it "detects lazy Data.Map import" $ do
          let code = "import Data.Map \nmap = M.empty"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "Data.Map" . diagMessage) diags `shouldBe` True

        it "does not flag Data.Map.Strict" $ do
          let code = "import Data.Map.Strict\nmap = M.empty"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          any (T.isInfixOf "Data.Map is lazy" . diagMessage) diags `shouldBe` False

        it "detects lazy Data.IntMap" $ do
          let code = "import Data.IntMap \nmap = IM.empty"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "Data.IntMap" . diagMessage) diags `shouldBe` True

        it "detects lazy Data.HashMap" $ do
          let code = "import Data.HashMap \nmap = HM.empty"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "Data.HashMap" . diagMessage) diags `shouldBe` True

      describe "allocation issues" $ do
        it "detects map . map pattern" $ do
          let code = "result = map f $ map g $ items"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "map" . diagMessage) diags `shouldBe` True

        it "detects filter . filter pattern" $ do
          let code = "result = filter p $ filter q $ items"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "filter" . diagMessage) diags `shouldBe` True

        it "detects concat . map pattern" $ do
          let code = "result = concat . map f $ items"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "concat" . diagMessage) diags `shouldBe` True

        it "detects concat (map f xs) pattern and generates correct fix" $ do
          let code = "flatMap f xs = concat (map f xs)"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          -- The fix should produce balanced parentheses (no extra closing paren)
          let fixes = concatMap diagFixes diags
          length fixes `shouldSatisfy` (> 0)
          let fixTexts = concatMap (\f -> map fixEditNewText (fixEdits f)) fixes
          -- Should produce "flatMap f xs = concatMap f xs" without extra )
          any (\t -> "concatMap f xs" `T.isInfixOf` t && not ("concatMap f xs)" `T.isInfixOf` t)) fixTexts `shouldBe` True

        it "detects mconcat (map f xs) pattern and generates correct fix" $ do
          let code = "combineAll f xs = mconcat (map f xs)"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          -- The fix should produce balanced parentheses
          let fixes = concatMap diagFixes diags
          length fixes `shouldSatisfy` (> 0)
          let fixTexts = concatMap (\f -> map fixEditNewText (fixEdits f)) fixes
          -- Should produce "combineAll f xs = foldMap f xs" without extra )
          any (\t -> "foldMap f xs" `T.isInfixOf` t && not ("foldMap f xs)" `T.isInfixOf` t)) fixTexts `shouldBe` True

        it "detects words . unwords pattern" $ do
          let code = "ws = words . unwords $ tokens"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "words" . diagMessage) diags `shouldBe` True

      describe "IO issues" $ do
        it "detects appendFile in loop context" $ do
          let code = T.unlines
                [ "writeAll items = forM_ items $ \\item ->"
                , "  appendFile \"output.txt\" (show item)"
                ]
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "appendFile" . diagMessage) diags `shouldBe` True

      describe "fusion blockers" $ do
        it "detects toList . fromList pattern" $ do
          let code = "items = toList . fromList $ xs"
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "toList" . diagMessage) diags `shouldBe` True

        it "detects freeze/thaw with Vector" $ do
          let code = T.unlines
                [ "import Data.Vector"
                , "frozen = freeze mutableVec"
                ]
              diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
          length diags `shouldSatisfy` (> 0)
          any (T.isInfixOf "freeze" . diagMessage) diags `shouldBe` True

      describe "configuration options" $ do
        it "respects perfCheckDataStructures = False" $ do
          let code = "unique = nub [1,2,3]"
              config = defaultPerformanceConfig { perfCheckDataStructures = False }
              diags = detectPerformanceIssues config "src/Main.hs" code
          -- nub is detected by data structures check
          any (T.isInfixOf "nub is O(n" . diagMessage) diags `shouldBe` False

        it "respects perfCheckStrings = False" $ do
          let code = "name :: String\nname = \"test\""
              config = defaultPerformanceConfig { perfCheckStrings = False }
              diags = detectPerformanceIssues config "src/Main.hs" code
          any (T.isInfixOf "String is [Char]" . diagMessage) diags `shouldBe` False

        it "respects perfCheckLazy = False" $ do
          let code = "import Data.Map \nmap = M.empty"
              config = defaultPerformanceConfig { perfCheckLazy = False }
              diags = detectPerformanceIssues config "src/Main.hs" code
          any (T.isInfixOf "Data.Map is lazy" . diagMessage) diags `shouldBe` False

        it "respects perfCheckAllocation = False" $ do
          let code = "result = concat . map f $ items"
              config = defaultPerformanceConfig { perfCheckAllocation = False }
              diags = detectPerformanceIssues config "src/Main.hs" code
          any (T.isInfixOf "concat . map" . diagMessage) diags `shouldBe` False

        it "respects perfCheckFusion = False" $ do
          let code = "items = toList . fromList $ xs"
              config = defaultPerformanceConfig { perfCheckFusion = False }
              diags = detectPerformanceIssues config "src/Main.hs" code
          any (T.isInfixOf "toList . fromList" . diagMessage) diags `shouldBe` False

    describe "diagnostic properties" $ do
      it "uses performance/ prefix for all codes" $ do
        let code = "unique = nub [1,2,3]"
            diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
        all (maybe False (T.isPrefixOf "performance/") . diagCode) diags `shouldBe` True

      it "includes complexity info when available" $ do
        let code = "unique = nub [1,2,3]"
            diags = detectPerformanceIssues defaultPerformanceConfig "src/Main.hs" code
        any (T.isInfixOf "Complexity:" . diagMessage) diags `shouldBe` True

    describe "PerformanceCategory" $ do
      it "has all expected categories" $ do
        minBound `shouldBe` InefficientDataStructure
        maxBound `shouldBe` InefficientIO
