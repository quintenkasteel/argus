{-# LANGUAGE OverloadedStrings #-}

module TestingExamples where

import Test.Hspec

--------------------------------------------------------------------------------
-- Prefer shouldBe over assertEqual
--------------------------------------------------------------------------------

-- Should suggest: use shouldBe
useAssertEqual :: Int -> Expectation
useAssertEqual x = assertEqual "values should match" 42 x

-- Good: shouldBe
useShouldBe :: Int -> Expectation
useShouldBe x = x `shouldBe` 42

--------------------------------------------------------------------------------
-- Avoid expectationFailure for assertions
--------------------------------------------------------------------------------

-- Should warn: use specific matchers
manualAssertion :: Bool -> Expectation
manualAssertion b = if b then pure () else expectationFailure "expected true"

-- Good: use shouldBe
properAssertion :: Bool -> Expectation
properAssertion b = b `shouldBe` True

--------------------------------------------------------------------------------
-- Use describe for grouping
--------------------------------------------------------------------------------

-- Should suggest: use describe for related tests
flatTests :: Spec
flatTests = do
  it "test 1" $ pure ()
  it "test 2" $ pure ()
  it "test 3" $ pure ()

-- Good: grouped with describe
groupedTests :: Spec
groupedTests = describe "Feature X" $ do
  it "does thing 1" $ pure ()
  it "does thing 2" $ pure ()

--------------------------------------------------------------------------------
-- Use specific matchers
--------------------------------------------------------------------------------

-- Should suggest: use shouldContain
manualContains :: [Int] -> Expectation
manualContains xs = (5 `elem` xs) `shouldBe` True

-- Good: shouldContain
useContain :: [Int] -> Expectation
useContain xs = xs `shouldContain` [5]

-- Should suggest: use shouldSatisfy
manualSatisfy :: Int -> Expectation
manualSatisfy x = (x > 0) `shouldBe` True

-- Good: shouldSatisfy
useSatisfy :: Int -> Expectation
useSatisfy x = x `shouldSatisfy` (> 0)

--------------------------------------------------------------------------------
-- Avoid redundant IO in pure tests
--------------------------------------------------------------------------------

-- Should warn: unnecessary IO
redundantIO :: Spec
redundantIO = it "pure test" $ do
  let result = 1 + 1
  result `shouldBe` 2

-- Good: direct assertion
directAssertion :: Spec
directAssertion = it "pure test" $
  (1 + 1) `shouldBe` 2

--------------------------------------------------------------------------------
-- Use before/after for setup/teardown
--------------------------------------------------------------------------------

-- Should suggest: use before for repeated setup
repeatedSetup :: Spec
repeatedSetup = do
  it "test 1" $ do
    let resource = setupResource
    useResource resource `shouldBe` True
  it "test 2" $ do
    let resource = setupResource
    useResource2 resource `shouldBe` True
  where
    setupResource = ()
    useResource _ = True
    useResource2 _ = True

-- Good: using before
withSetup :: Spec
withSetup = before setupResource $ do
  it "test 1" $ \resource ->
    useResource resource `shouldBe` True
  it "test 2" $ \resource ->
    useResource2 resource `shouldBe` True
  where
    setupResource = pure ()
    useResource _ = True
    useResource2 _ = True

--------------------------------------------------------------------------------
-- Prefer property tests for invariants
--------------------------------------------------------------------------------

-- Should suggest: consider property test
manyExamples :: Spec
manyExamples = do
  it "reverse twice is identity 1" $ [1] `shouldBe` [1]
  it "reverse twice is identity 2" $ [1, 2] `shouldBe` [1,2]
  it "reverse twice is identity 3" $ [1, 2, 3] `shouldBe` [1,2,3]

-- Good: property test (conceptual)
-- propertyTest :: Spec
-- propertyTest = prop "reverse twice is identity" $ \xs ->
--   reverse (reverse xs) === xs

--------------------------------------------------------------------------------
-- Use pending for incomplete tests
--------------------------------------------------------------------------------

-- Should warn: empty test body
emptyTest :: Spec
emptyTest = it "TODO: implement this test" $ pure ()

-- Good: use pending
pendingTest :: Spec
pendingTest = it "TODO: implement this test" pending