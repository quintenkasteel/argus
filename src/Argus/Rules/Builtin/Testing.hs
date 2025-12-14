{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Testing
-- Description : Testing best practice rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for writing better tests with HSpec, QuickCheck, and other
-- testing frameworks. Encourages best practices and common patterns.
--
-- == Rule Categories
--
-- * __Assertions__: Assertion patterns
-- * __QuickCheck__: Property-based testing
-- * __HSpec__: HSpec testing patterns
-- * __Test Organization__: Test structure

module Argus.Rules.Builtin.Testing
  ( -- * Rule Sets
    testingRules
  , assertionRules
  , quickCheckRules
  , hspecRules
  , testOrgRules

    -- * Assertions
  , assertEqualOrder
  , assertBoolTrue
  , assertBoolFalse
  , assertJust
  , assertNothing
  , assertRight
  , assertLeft
  , assertEmpty
  , assertLength

    -- * QuickCheck
  , arbitraryBounds
  , shrinkMissing
  , propertyBool
  , propertyMonadic
  , forAllShow
  , counterexample
  , coverageCheck
  , labelDistribution

    -- * HSpec
  , shouldBeBool
  , shouldBeJust
  , shouldBeNothing
  , shouldSatisfyPred
  , pendingTest
  , focusedTest
  , describeFocus
  , parallelSpec

    -- * Test Organization
  , testGrouping
  , sharedSetup
  , testIsolation
  , testNaming
  , goldenTest
  , benchmarkInTest

    -- * Rule Count
  , testingRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All testing-related rules.
testingRules :: [Rule]
testingRules = mconcat
  [ assertionRules
  , quickCheckRules
  , hspecRules
  , testOrgRules
  ]

-- | Total count of testing rules.
testingRuleCount :: Int
testingRuleCount = length testingRules

--------------------------------------------------------------------------------
-- Assertion Rules
--------------------------------------------------------------------------------

-- | Rules for assertion patterns.
assertionRules :: [Rule]
assertionRules =
  [ assertEqualOrder
  , assertBoolTrue
  , assertBoolFalse
  , assertJust
  , assertNothing
  , assertRight
  , assertLeft
  , assertEmpty
  , assertLength
  ]

-- | assertEqual argument order.
--
-- @
-- assertEqual expected actual  -- expected first
-- @
assertEqualOrder :: Rule
assertEqualOrder =
  rule "assertEqual-order" $
    match ("assertEqual _msg _expected _actual" ==> "assertEqual _msg _expected _actual")
    & category Style
    & severity Info
    & message "assertEqual takes (message, expected, actual)"
    & note "Ensure expected value comes before actual"
    & safetyLevel ManualReview

-- | assertBool True redundant.
--
-- @
-- assertBool "msg" True  -- Always passes
-- @
assertBoolTrue :: Rule
assertBoolTrue =
  rule "assertBool-true" $
    match ("assertBool _msg True" ==> "return ()")
    & category Correctness
    & severity Warning
    & message "assertBool with True always passes"
    & note "This test provides no value"

-- | assertBool False always fails.
--
-- @
-- assertBool "msg" False  -- Always fails
-- @
assertBoolFalse :: Rule
assertBoolFalse =
  rule "assertBool-false" $
    match ("assertBool _msg False" ==> "assertFailure _msg")
    & category Correctness
    & severity Error
    & message "assertBool with False always fails"

-- | assertJust pattern.
--
-- @
-- isJust x @? "msg"  -- Prefer pattern matching
-- @
assertJust :: Rule
assertJust =
  rule "assert-just" $
    matchText "isJust.*@\\?"
    & category Style
    & severity Suggestion
    & message "Consider pattern matching instead of isJust assertion"

-- | assertNothing pattern.
--
-- @
-- isNothing x @? "msg"  -- Prefer pattern matching
-- @
assertNothing :: Rule
assertNothing =
  rule "assert-nothing" $
    matchText "isNothing.*@\\?"
    & category Style
    & severity Suggestion
    & message "Consider pattern matching instead of isNothing assertion"

-- | assertRight pattern.
--
-- @
-- isRight x @? "msg"  -- Prefer pattern matching
-- @
assertRight :: Rule
assertRight =
  rule "assert-right" $
    matchText "isRight.*@\\?"
    & category Style
    & severity Suggestion
    & message "Consider pattern matching instead of isRight assertion"

-- | assertLeft pattern.
--
-- @
-- isLeft x @? "msg"  -- Prefer pattern matching
-- @
assertLeft :: Rule
assertLeft =
  rule "assert-left" $
    matchText "isLeft.*@\\?"
    & category Style
    & severity Suggestion
    & message "Consider pattern matching instead of isLeft assertion"

-- | Assert empty collection.
--
-- @
-- null xs @? "msg"  -- Check empty
-- @
assertEmpty :: Rule
assertEmpty =
  rule "assert-empty" $
    matchText "null.*@\\?"
    & category Style
    & severity Info
    & message "Asserting emptiness - consider shouldBe []"
    & safetyLevel ManualReview

-- | Assert length.
--
-- @
-- length xs == n @? "msg"  -- Check length
-- @
assertLength :: Rule
assertLength =
  rule "assert-length" $
    matchText "length.*==.*@\\?"
    & category Style
    & severity Info
    & message "Length assertion - consider shouldSatisfy with explicit check"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- QuickCheck Rules
--------------------------------------------------------------------------------

-- | Rules for QuickCheck patterns.
quickCheckRules :: [Rule]
quickCheckRules =
  [ arbitraryBounds
  , shrinkMissing
  , propertyBool
  , propertyMonadic
  , forAllShow
  , counterexample
  , coverageCheck
  , labelDistribution
  ]

-- | Arbitrary instance bounds.
--
-- @
-- arbitrary = choose (minBound, maxBound)  -- May generate huge values
-- @
arbitraryBounds :: Rule
arbitraryBounds =
  rule "arbitrary-bounds" $
    matchText "arbitrary.*choose.*minBound.*maxBound"
    & category Performance
    & severity Warning
    & message "Arbitrary with full bounds may generate huge values"
    & note "Consider limiting range for better test performance"

-- | Missing Shrink instance.
--
-- @
-- instance Arbitrary T where arbitrary = ...  -- No shrink
-- @
shrinkMissing :: Rule
shrinkMissing =
  rule "shrink-missing" $
    matchText "instance Arbitrary.*where\\s+arbitrary"
    & category Style
    & severity Suggestion
    & message "Consider adding shrink for better counterexamples"

-- | Property returning Bool.
--
-- @
-- property $ \x -> f x == g x
-- @
propertyBool :: Rule
propertyBool =
  rule "property-bool" $
    match ("property (\\_ -> _bool)" ==> "property (\\_ -> _bool)")
    & category Style
    & severity Info
    & message "Property returning Bool - consider using (===) for better errors"
    & safetyLevel ManualReview

-- | Monadic property.
--
-- @
-- monadicIO $ do ...
-- @
propertyMonadic :: Rule
propertyMonadic =
  rule "property-monadic" $
    match ("monadicIO _action" ==> "monadicIO _action")
    & category Style
    & severity Info
    & message "Using monadicIO for IO-based property testing"
    & safetyLevel ManualReview

-- | forAll with Show.
--
-- @
-- forAll gen prop  -- Ensure generator values are showable
-- @
forAllShow :: Rule
forAllShow =
  rule "forAll-show" $
    match ("forAll _gen _prop" ==> "forAll _gen _prop")
    & category Style
    & severity Info
    & message "forAll requires Show for counterexample display"
    & safetyLevel ManualReview

-- | counterexample for better messages.
--
-- @
-- counterexample msg prop  -- Add context to failures
-- @
counterexample :: Rule
counterexample =
  rule "counterexample-usage" $
    match ("counterexample _msg _prop" ==> "counterexample _msg _prop")
    & category Style
    & severity Info
    & message "Using counterexample for better failure messages - good practice"
    & safetyLevel ManualReview

-- | Coverage checking.
--
-- @
-- checkCoverage prop  -- Verify distribution
-- @
coverageCheck :: Rule
coverageCheck =
  rule "coverage-check" $
    match ("checkCoverage _prop" ==> "checkCoverage _prop")
    & category Style
    & severity Info
    & message "Using checkCoverage to verify test distribution"
    & safetyLevel ManualReview

-- | Label distribution.
--
-- @
-- label "case" prop  -- Label test cases
-- @
labelDistribution :: Rule
labelDistribution =
  rule "label-distribution" $
    match ("label _name _prop" ==> "label _name _prop")
    & category Style
    & severity Info
    & message "Labeling test cases - use collect to see distribution"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- HSpec Rules
--------------------------------------------------------------------------------

-- | Rules for HSpec patterns.
hspecRules :: [Rule]
hspecRules =
  [ shouldBeBool
  , shouldBeJust
  , shouldBeNothing
  , shouldSatisfyPred
  , pendingTest
  , focusedTest
  , describeFocus
  , parallelSpec
  ]

-- | shouldBe with Bool.
--
-- @
-- x `shouldBe` True  ==>  x `shouldBe` True
-- @
shouldBeBool :: Rule
shouldBeBool =
  rule "shouldBe-bool" $
    match ("_x `shouldBe` True" ==> "_x `shouldSatisfy` id")
    & category Style
    & severity Suggestion
    & message "Consider shouldSatisfy for boolean checks"

-- | shouldBe with Just.
--
-- @
-- x `shouldBe` Just v  -- Consider shouldSatisfy isJust
-- @
shouldBeJust :: Rule
shouldBeJust =
  rule "shouldBe-just" $
    match ("_x `shouldBe` Just _v" ==> "_x `shouldBe` Just _v")
    & category Style
    & severity Info
    & message "shouldBe Just - ensure exact value matters"
    & safetyLevel ManualReview

-- | shouldBe Nothing.
--
-- @
-- x `shouldBe` Nothing  -- Check for Nothing
-- @
shouldBeNothing :: Rule
shouldBeNothing =
  rule "shouldBe-nothing" $
    match ("_x `shouldBe` Nothing" ==> "_x `shouldSatisfy` isNothing")
    & category Style
    & severity Info
    & message "Consider shouldSatisfy isNothing for clearer intent"
    & safetyLevel ManualReview

-- | shouldSatisfy with predicate.
--
-- @
-- x `shouldSatisfy` pred  -- Predicate must be clear
-- @
shouldSatisfyPred :: Rule
shouldSatisfyPred =
  rule "shouldSatisfy-pred" $
    match ("_x `shouldSatisfy` _pred" ==> "_x `shouldSatisfy` _pred")
    & category Style
    & severity Info
    & message "shouldSatisfy - ensure predicate provides good error message"
    & safetyLevel ManualReview

-- | Pending test.
--
-- @
-- pending "reason"  -- Skipped test
-- @
pendingTest :: Rule
pendingTest =
  rule "pending-test" $
    match ("pending _reason" ==> "pending _reason")
    & category Style
    & severity Warning
    & message "Pending test - complete or remove"
    & safetyLevel ManualReview

-- | Focused test (fit, fdescribe).
--
-- @
-- fit "test" $ ...  -- Focused test, only runs this
-- @
focusedTest :: Rule
focusedTest =
  rule "focused-test" $
    matchText "^\\s+fit\\s+"
    & category Correctness
    & severity Error
    & message "Focused test (fit) - other tests won't run!"
    & note "Remove focus before committing"

-- | Focused describe.
--
-- @
-- fdescribe "group" $ ...  -- Focused group
-- @
describeFocus :: Rule
describeFocus =
  rule "describe-focus" $
    matchText "^\\s+fdescribe\\s+"
    & category Correctness
    & severity Error
    & message "Focused describe (fdescribe) - other tests won't run!"
    & note "Remove focus before committing"

-- | Parallel spec.
--
-- @
-- parallel $ describe ...  -- Run tests in parallel
-- @
parallelSpec :: Rule
parallelSpec =
  rule "parallel-spec" $
    match ("parallel _spec" ==> "parallel _spec")
    & category Performance
    & severity Info
    & message "Using parallel test execution - ensure test isolation"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Test Organization Rules
--------------------------------------------------------------------------------

-- | Rules for test organization.
testOrgRules :: [Rule]
testOrgRules =
  [ testGrouping
  , sharedSetup
  , testIsolation
  , testNaming
  , goldenTest
  , benchmarkInTest
  ]

-- | Test grouping with describe.
--
-- @
-- describe "Module" $ do ...  -- Group related tests
-- @
testGrouping :: Rule
testGrouping =
  rule "test-grouping" $
    match ("describe _name _spec" ==> "describe _name _spec")
    & category Style
    & severity Info
    & message "Using describe for test grouping"
    & safetyLevel ManualReview

-- | Shared setup with before/around.
--
-- @
-- before setup spec  -- Run setup before each test
-- @
sharedSetup :: Rule
sharedSetup =
  rule "shared-setup" $
    match ("before _setup _spec" ==> "before _setup _spec")
    & category Style
    & severity Info
    & message "Using before for shared setup - ensures fresh state"
    & safetyLevel ManualReview

-- | Test isolation.
--
-- @
-- around withResource spec  -- Resource bracket per test
-- @
testIsolation :: Rule
testIsolation =
  rule "test-isolation" $
    match ("around _bracket _spec" ==> "around _bracket _spec")
    & category Style
    & severity Info
    & message "Using around for test resource management"
    & safetyLevel ManualReview

-- | Test naming conventions.
--
-- @
-- it "should ..." $ ...  -- Descriptive test names
-- @
testNaming :: Rule
testNaming =
  rule "test-naming" $
    matchText "it \"should"
    & category Style
    & severity Info
    & message "Test name starts with 'should' - good convention"
    & safetyLevel ManualReview

-- | Golden test pattern.
--
-- @
-- golden "test" expected actual  -- Compare against golden file
-- @
goldenTest :: Rule
goldenTest =
  rule "golden-test" $
    matchText "golden"
    & category Style
    & severity Info
    & message "Using golden testing - ensure golden files are committed"
    & safetyLevel ManualReview

-- | Benchmark in test file.
--
-- @
-- bench "name" $ ...  -- Benchmark in test context
-- @
benchmarkInTest :: Rule
benchmarkInTest =
  rule "benchmark-in-test" $
    matchText "bench\\s+\""
    & category Style
    & severity Info
    & message "Benchmark in test file - consider separate benchmark suite"
    & safetyLevel ManualReview
