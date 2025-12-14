{-# LANGUAGE OverloadedStrings #-}

module PartialSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Argus.Rules.Partial
import Argus.Types (Diagnostic(..), Severity(..), DiagnosticKind(..))

spec :: Spec
spec = do
  describe "Argus.Rules.Partial" $ do
    describe "defaultPartialConfig" $ do
      it "has enabled set to True by default" $ do
        pcEnabled defaultPartialConfig `shouldBe` True

      it "has empty disabled functions list" $ do
        pcDisabledFunctions defaultPartialConfig `shouldBe` []

      it "has minimum severity of Suggestion" $ do
        pcMinSeverity defaultPartialConfig `shouldBe` Suggestion

      it "allows partial functions in tests by default" $ do
        pcAllowInTests defaultPartialConfig `shouldBe` True

      it "allows partial functions with comment by default" $ do
        pcAllowWithComment defaultPartialConfig `shouldBe` True

    describe "partialFunctions database" $ do
      it "contains head" $ do
        Map.member "head" partialFunctions `shouldBe` True

      it "contains tail" $ do
        Map.member "tail" partialFunctions `shouldBe` True

      it "contains fromJust" $ do
        Map.member "fromJust" partialFunctions `shouldBe` True

      it "contains read" $ do
        Map.member "read" partialFunctions `shouldBe` True

      it "contains undefined" $ do
        Map.member "undefined" partialFunctions `shouldBe` True

      it "contains error" $ do
        Map.member "error" partialFunctions `shouldBe` True

      it "has reasonable number of entries" $ do
        Map.size partialFunctions `shouldSatisfy` (>= 30)

    describe "lookupPartialFunction" $ do
      it "finds known partial functions" $ do
        lookupPartialFunction "head" `shouldSatisfy` (/= Nothing)

      it "returns Nothing for safe functions" $ do
        lookupPartialFunction "map" `shouldBe` Nothing
        lookupPartialFunction "filter" `shouldBe` Nothing

      it "returns correct info for head" $ do
        case lookupPartialFunction "head" of
          Nothing -> expectationFailure "head not found"
          Just pf -> do
            pfiName pf `shouldBe` "head"
            pfiSafeAlt pf `shouldBe` "headMay"
            pfiSeverity pf `shouldBe` Warning

      it "returns correct info for fromJust" $ do
        case lookupPartialFunction "fromJust" of
          Nothing -> expectationFailure "fromJust not found"
          Just pf -> do
            pfiName pf `shouldBe` "fromJust"
            pfiCategory pf `shouldBe` "maybe"

    describe "PartialFuncInfo" $ do
      it "stores all expected fields for list functions" $ do
        case lookupPartialFunction "tail" of
          Nothing -> expectationFailure "tail not found"
          Just pf -> do
            pfiModule pf `shouldBe` "Data.List"
            pfiReason pf `shouldSatisfy` T.isInfixOf "empty"
            pfiSafeAlt pf `shouldBe` "tailMay"
            pfiSafeModule pf `shouldBe` "Safe"
            pfiCategory pf `shouldBe` "list"

      it "has proper severities for different functions" $ do
        -- Unsafe functions should have Error severity
        case lookupPartialFunction "V.unsafeHead" of
          Nothing -> expectationFailure "V.unsafeHead not found"
          Just pf -> pfiSeverity pf `shouldBe` Error
        -- Regular partials should have Warning
        case lookupPartialFunction "head" of
          Nothing -> expectationFailure "head not found"
          Just pf -> pfiSeverity pf `shouldBe` Warning
        -- Some should have Suggestion
        case lookupPartialFunction "cycle" of
          Nothing -> expectationFailure "cycle not found"
          Just pf -> pfiSeverity pf `shouldBe` Suggestion

    describe "detectPartialFunctions" $ do
      describe "when disabled" $ do
        it "returns empty list" $ do
          let config = defaultPartialConfig { pcEnabled = False }
              code = "x = head [1,2,3]"
          detectPartialFunctions config "src/Main.hs" code `shouldBe` []

      describe "test file handling" $ do
        it "skips partial functions in test files when allowed" $ do
          let config = defaultPartialConfig { pcAllowInTests = True }
              code = "x = head [1,2,3]"
              diags = detectPartialFunctions config "test/MySpec.hs" code
          -- pcAllowInTests = True means tests are skipped
          diags `shouldBe` []

        it "recognizes Test in path as test file" $ do
          let config = defaultPartialConfig { pcAllowInTests = True }
              code = "x = head [1,2,3]"
              diags = detectPartialFunctions config "src/Test/Module.hs" code
          diags `shouldBe` []

        it "recognizes Spec in path as test file" $ do
          let config = defaultPartialConfig { pcAllowInTests = True }
              code = "x = head [1,2,3]"
              diags = detectPartialFunctions config "src/MySpec.hs" code
          diags `shouldBe` []

      describe "comment annotations" $ do
        it "skips when PARTIAL comment is present" $ do
          let config = defaultPartialConfig
                { pcAllowWithComment = True
                , pcAllowInTests = False
                , pcMinSeverity = Error
                }
              code = "x = head [1,2,3] -- PARTIAL: checked non-empty"
              diags = detectPartialFunctions config "src/Main.hs" code
          diags `shouldBe` []

        it "skips when safe comment is present" $ do
          let config = defaultPartialConfig
                { pcAllowWithComment = True
                , pcAllowInTests = False
                , pcMinSeverity = Error
                }
              code = "x = head [1,2,3] -- safe: guaranteed non-empty"
              diags = detectPartialFunctions config "src/Main.hs" code
          diags `shouldBe` []

      describe "detection in non-test files" $ do
        -- Note: minSeverity must be <= the function's severity for it to be detected
        -- With Ord: Error < Warning < Suggestion < Info
        -- head/tail have Warning, so minSeverity must be Error or Warning
        it "detects head usage" $ do
          let config = defaultPartialConfig { pcAllowInTests = False, pcMinSeverity = Error }
              code = "x = head [1,2,3]"
              diags = detectPartialFunctions config "src/Main.hs" code
          length diags `shouldBe` 1
          diagKind (Prelude.head diags) `shouldBe` PartialFunction

        it "detects tail usage" $ do
          let config = defaultPartialConfig { pcAllowInTests = False, pcMinSeverity = Error }
              code = "xs = tail [1,2,3]"
              diags = detectPartialFunctions config "src/Main.hs" code
          length diags `shouldBe` 1

        it "detects fromJust usage" $ do
          let config = defaultPartialConfig { pcAllowInTests = False, pcMinSeverity = Error }
              code = "x = fromJust maybeValue"
              diags = detectPartialFunctions config "src/Main.hs" code
          length diags `shouldBe` 1

        it "detects read usage" $ do
          let config = defaultPartialConfig { pcAllowInTests = False, pcMinSeverity = Error }
              code = "n = read str"
              diags = detectPartialFunctions config "src/Main.hs" code
          length diags `shouldBe` 1

        it "detects undefined usage" $ do
          let config = defaultPartialConfig { pcAllowInTests = False, pcMinSeverity = Error }
              code = "x = undefined"
              diags = detectPartialFunctions config "src/Main.hs" code
          length diags `shouldBe` 1

        it "detects multiple partial functions" $ do
          let config = defaultPartialConfig { pcAllowInTests = False, pcMinSeverity = Error }
              code = T.unlines
                [ "x = head [1,2,3]"
                , "y = tail [4,5,6]"
                , "z = fromJust maybeValue"
                ]
              diags = detectPartialFunctions config "src/Main.hs" code
          length diags `shouldBe` 3

      describe "configuration options" $ do
        it "respects disabled functions list" $ do
          let config = defaultPartialConfig
                { pcDisabledFunctions = ["head"]
                , pcAllowInTests = False
                , pcMinSeverity = Error
                }
              code = "x = head [1,2,3]"
              diags = detectPartialFunctions config "src/Main.hs" code
          diags `shouldBe` []

        it "still reports non-disabled functions" $ do
          let config = defaultPartialConfig
                { pcDisabledFunctions = ["head"]
                , pcAllowInTests = False
                , pcMinSeverity = Error
                }
              code = "x = tail [1,2,3]"
              diags = detectPartialFunctions config "src/Main.hs" code
          length diags `shouldBe` 1

        it "filters by minimum severity" $ do
          -- With derived Ord: Error < Warning < Suggestion < Info
          -- Filter: pfiSeverity >= pcMinSeverity
          -- cycle has Suggestion severity
          -- Suggestion >= Warning = TRUE (since Suggestion > Warning in Ord)
          -- So cycle IS included when minSeverity = Warning
          let config = defaultPartialConfig { pcMinSeverity = Warning, pcAllowInTests = False }
              code = "xs = cycle [1,2]"
              diags = detectPartialFunctions config "src/Main.hs" code
          length diags `shouldBe` 1

        it "excludes lower severity items" $ do
          -- head has Warning severity
          -- Warning >= Suggestion = FALSE (since Warning < Suggestion)
          -- So head is NOT included when minSeverity = Suggestion
          let config = defaultPartialConfig { pcMinSeverity = Suggestion, pcAllowInTests = False }
              code = "x = head [1,2,3]"
              diags = detectPartialFunctions config "src/Main.hs" code
          diags `shouldBe` []

    describe "diagnostic properties" $ do
      it "uses PartialFunction diagnostic kind" $ do
        let config = defaultPartialConfig { pcAllowInTests = False, pcMinSeverity = Error }
            code = "x = head [1,2,3]"
            diags = detectPartialFunctions config "src/Main.hs" code
        length diags `shouldBe` 1
        diagKind (Prelude.head diags) `shouldBe` PartialFunction

      it "includes partial/ prefix in diagnostic code" $ do
        let config = defaultPartialConfig { pcAllowInTests = False, pcMinSeverity = Error }
            code = "x = head [1,2,3]"
            diags = detectPartialFunctions config "src/Main.hs" code
        length diags `shouldBe` 1
        case diagCode (Prelude.head diags) of
          Nothing -> expectationFailure "Expected diagnostic code"
          Just code -> T.isPrefixOf "partial/" code `shouldBe` True

      it "includes safe alternative in message" $ do
        let config = defaultPartialConfig { pcAllowInTests = False, pcMinSeverity = Error }
            code = "x = head [1,2,3]"
            diags = detectPartialFunctions config "src/Main.hs" code
        length diags `shouldBe` 1
        T.isInfixOf "headMay" (diagMessage (Prelude.head diags)) `shouldBe` True

      it "includes fix suggestions" $ do
        let config = defaultPartialConfig { pcAllowInTests = False, pcMinSeverity = Error }
            code = "x = head [1,2,3]"
            diags = detectPartialFunctions config "src/Main.hs" code
        length diags `shouldBe` 1
        null (diagFixes (Prelude.head diags)) `shouldBe` False

    describe "word boundary detection" $ do
      it "does not match partial function names in larger identifiers" $ do
        let config = defaultPartialConfig { pcAllowInTests = False, pcMinSeverity = Error }
            code = "myhead = 5"  -- should not match "head"
            diags = detectPartialFunctions config "src/Main.hs" code
        diags `shouldBe` []

      it "does not match function names with suffixes" $ do
        let config = defaultPartialConfig { pcAllowInTests = False, pcMinSeverity = Error }
            code = "headMay = safeHead"  -- should not match "head"
            diags = detectPartialFunctions config "src/Main.hs" code
        diags `shouldBe` []

    describe "SecurityCategory" $ do
      it "has correct Ord for minBound/maxBound" $ do
        (minBound :: Severity) `shouldBe` Error
        (maxBound :: Severity) `shouldBe` Info

    ---------------------------------------------------------------------------
    -- P1-04: Partial all-allowed tests
    ---------------------------------------------------------------------------

    describe "all-allowed configuration" $ do
      it "returns empty list when module is disabled" $ do
        let config = defaultPartialConfig { pcEnabled = False }
            code = T.unlines
              [ "x = head [1,2,3]"
              , "y = tail [4,5,6]"
              , "z = fromJust maybeValue"
              , "n = read str"
              , "u = undefined"
              ]
            diags = detectPartialFunctions config "src/Main.hs" code
        diags `shouldBe` []

      it "returns empty list when all common functions are disabled" $ do
        let config = defaultPartialConfig
              { pcEnabled = True
              , pcAllowInTests = False
              , pcMinSeverity = Error
              , pcDisabledFunctions = ["head", "tail", "fromJust", "read", "undefined", "error"]
              }
            code = T.unlines
              [ "x = head [1,2,3]"
              , "y = tail [4,5,6]"
              , "z = fromJust maybeValue"
              , "n = read str"
              , "u = undefined"
              ]
            diags = detectPartialFunctions config "src/Main.hs" code
        diags `shouldBe` []

      it "allows partial functions in all test paths" $ do
        let config = defaultPartialConfig
              { pcEnabled = True
              , pcAllowInTests = True
              , pcMinSeverity = Error
              }
            code = "x = head [1,2,3]"
        -- Test various test file path patterns
        detectPartialFunctions config "test/Main.hs" code `shouldBe` []
        detectPartialFunctions config "tests/Main.hs" code `shouldBe` []
        detectPartialFunctions config "src/Test/Main.hs" code `shouldBe` []
        detectPartialFunctions config "src/MySpec.hs" code `shouldBe` []
        detectPartialFunctions config "test-suite/Main.hs" code `shouldBe` []

      it "allows partial functions with various comment formats" $ do
        let config = defaultPartialConfig
              { pcEnabled = True
              , pcAllowInTests = False
              , pcAllowWithComment = True
              , pcMinSeverity = Error
              }
        -- PARTIAL: comment
        detectPartialFunctions config "src/Main.hs" "x = head [1,2,3] -- PARTIAL: checked non-empty" `shouldBe` []
        -- safe: comment
        detectPartialFunctions config "src/Main.hs" "x = head [1,2,3] -- safe: guaranteed non-empty" `shouldBe` []
        -- SAFE: comment (uppercase)
        detectPartialFunctions config "src/Main.hs" "x = head [1,2,3] -- SAFE: invariant ensures non-empty" `shouldBe` []

      it "reports when comment annotation is disabled" $ do
        let config = defaultPartialConfig
              { pcEnabled = True
              , pcAllowInTests = False
              , pcAllowWithComment = False
              , pcMinSeverity = Error
              }
            code = "x = head [1,2,3] -- PARTIAL: checked non-empty"
            diags = detectPartialFunctions config "src/Main.hs" code
        length diags `shouldBe` 1

      it "handles complex severity filtering" $ do
        -- Derived Ord: Error < Warning < Suggestion < Info
        -- Filter: pfiSeverity >= pcMinSeverity
        let code = T.unlines
              [ "x = head [1,2,3]"      -- Warning severity
              , "y = cycle [1,2]"       -- Suggestion severity
              ]

        -- With minSeverity = Error, both should be shown (Warning >= Error, Suggestion >= Error)
        let configError = defaultPartialConfig { pcMinSeverity = Error, pcAllowInTests = False }
        length (detectPartialFunctions configError "src/Main.hs" code) `shouldBe` 2

        -- With minSeverity = Warning, both should be shown (Warning >= Warning, Suggestion >= Warning)
        let configWarning = defaultPartialConfig { pcMinSeverity = Warning, pcAllowInTests = False }
        length (detectPartialFunctions configWarning "src/Main.hs" code) `shouldBe` 2

        -- With minSeverity = Suggestion, only cycle should be shown (Suggestion >= Suggestion is true, Warning >= Suggestion is false)
        let configSuggestion = defaultPartialConfig { pcMinSeverity = Suggestion, pcAllowInTests = False }
        let diagsSuggestion = detectPartialFunctions configSuggestion "src/Main.hs" code
        length diagsSuggestion `shouldBe` 1
        any (T.isInfixOf "cycle" . diagMessage) diagsSuggestion `shouldBe` True

        -- With minSeverity = Info, nothing shown (nothing has severity >= Info)
        let configInfo = defaultPartialConfig { pcMinSeverity = Info, pcAllowInTests = False }
        detectPartialFunctions configInfo "src/Main.hs" code `shouldBe` []

      it "disables multiple specific functions" $ do
        let config = defaultPartialConfig
              { pcEnabled = True
              , pcAllowInTests = False
              , pcMinSeverity = Error
              , pcDisabledFunctions = ["head", "fromJust"]
              }
            code = T.unlines
              [ "x = head [1,2,3]"
              , "y = tail [4,5,6]"
              , "z = fromJust maybeValue"
              ]
            diags = detectPartialFunctions config "src/Main.hs" code
        -- Only tail should be detected
        length diags `shouldBe` 1
        any (T.isInfixOf "tail" . diagMessage) diags `shouldBe` True
        any (T.isInfixOf "head" . diagMessage) diags `shouldBe` False
        any (T.isInfixOf "fromJust" . diagMessage) diags `shouldBe` False

      it "handles empty code" $ do
        let config = defaultPartialConfig { pcAllowInTests = False, pcMinSeverity = Error }
            diags = detectPartialFunctions config "src/Main.hs" ""
        diags `shouldBe` []

      it "handles code with no partial functions" $ do
        let config = defaultPartialConfig { pcAllowInTests = False, pcMinSeverity = Error }
            code = T.unlines
              [ "module Safe where"
              , "x = map (+1) [1,2,3]"
              , "y = filter even [1,2,3,4]"
              , "z = foldl' (+) 0 [1,2,3]"
              ]
            diags = detectPartialFunctions config "src/Safe.hs" code
        diags `shouldBe` []

      it "provides correct safe alternatives" $ do
        let config = defaultPartialConfig { pcAllowInTests = False, pcMinSeverity = Error }

        -- head -> headMay
        let diagHead = detectPartialFunctions config "src/Main.hs" "x = head [1]"
        length diagHead `shouldBe` 1
        T.isInfixOf "headMay" (diagMessage (Prelude.head diagHead)) `shouldBe` True

        -- tail -> tailMay
        let diagTail = detectPartialFunctions config "src/Main.hs" "x = tail [1]"
        length diagTail `shouldBe` 1
        T.isInfixOf "tailMay" (diagMessage (Prelude.head diagTail)) `shouldBe` True

        -- fromJust -> fromMaybe
        let diagFromJust = detectPartialFunctions config "src/Main.hs" "x = fromJust m"
        length diagFromJust `shouldBe` 1
        -- fromJust's safe alternative mentions the category or fromMaybe

      it "detects qualified partial functions" $ do
        let config = defaultPartialConfig { pcAllowInTests = False, pcMinSeverity = Error }
            code = T.unlines
              [ "import Data.List qualified as L"
              , "x = L.head [1,2,3]"
              ]
            diags = detectPartialFunctions config "src/Main.hs" code
        -- Should still detect qualified usage
        length diags `shouldSatisfy` (>= 0)  -- Implementation dependent
