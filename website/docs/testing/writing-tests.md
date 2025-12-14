---
sidebar_position: 2
title: Writing Tests
description: How to write tests for Argus
---

# Writing Tests

This guide explains how to write tests for Argus, including unit tests, property tests, and integration tests.

## Test Framework

Argus uses:
- **Hspec** for test organization and assertions
- **QuickCheck** for property-based testing
- **hspec-golden** for golden file tests

## Test File Structure

```haskell
-- test/MyFeatureSpec.hs
module MyFeatureSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Argus.MyFeature
import TestUtils

spec :: Spec
spec = do
  describe "MyFeature" $ do
    describe "functionA" $ do
      it "handles normal input" $ do
        functionA normalInput `shouldBe` expectedOutput

      it "handles edge cases" $ do
        functionA edgeCase `shouldBe` edgeOutput

    describe "functionB" $ do
      prop "satisfies property" $ \x ->
        property x

      it "integrates with other features" $ do
        -- Integration test
        result <- runWithSetup
        result `shouldSatisfy` isValid
```

## Unit Tests

### Basic Assertions

```haskell
spec :: Spec
spec = do
  describe "Pattern matching" $ do
    it "matches exact patterns" $ do
      let pattern = compilePattern "head $X"
          expr = parseExpr "head items"
      matchPattern pattern expr `shouldBe` Just [("X", "items")]

    it "returns Nothing for non-matches" $ do
      let pattern = compilePattern "head $X"
          expr = parseExpr "tail items"
      matchPattern pattern expr `shouldBe` Nothing

    it "binds multiple variables" $ do
      let pattern = compilePattern "$F $X $Y"
          expr = parseExpr "foldr (+) 0 xs"
      matchPattern pattern expr `shouldBe` Just
        [ ("F", "foldr")
        , ("X", "(+)")
        , ("Y", "0 xs")
        ]
```

### Testing with AST

```haskell
spec :: Spec
spec = do
  describe "AST analysis" $ do
    it "finds head usages" $ do
      let source = "module Test where\nf x = head x"
      parsed <- parseModule source
      let usages = findHeadUsages parsed
      length usages `shouldBe` 1

    it "extracts correct span" $ do
      let source = "module Test where\nf x = head x"
      parsed <- parseModule source
      let [usage] = findHeadUsages parsed
      spanLine (usageSpan usage) `shouldBe` 2
      spanColumn (usageSpan usage) `shouldBe` 7
```

### Testing Rules

```haskell
spec :: Spec
spec = do
  describe "partial/head rule" $ do
    it "detects simple head usage" $ do
      diags <- runRule headRule "head items"
      diags `shouldSatisfy` (not . null)

    it "suggests headMay" $ do
      diags <- runRule headRule "head items"
      let [diag] = diags
      diagSuggestion diag `shouldBe` Just "headMay items"

    it "ignores safe head" $ do
      diags <- runRule headRule "Safe.headMay items"
      diags `shouldBe` []
```

## Property Tests

### QuickCheck Properties

```haskell
spec :: Spec
spec = do
  describe "Pattern compilation" $ do
    prop "compiles any valid pattern" $ \patternStr ->
      isValidPattern patternStr ==>
        compilePattern patternStr `shouldSatisfy` isJust

    prop "compiled pattern matches itself" $ \patternStr ->
      isValidPattern patternStr ==>
        let compiled = compilePattern patternStr
            expr = patternToExpr patternStr
        in matchPattern compiled expr `shouldSatisfy` isJust

    prop "substitution preserves semantics" $ \pattern replacement bindings ->
      let original = applyBindings bindings pattern
          substituted = applyBindings bindings replacement
      in semanticallyEqual original substituted
```

### Custom Generators

```haskell
-- Generate valid Haskell expressions
instance Arbitrary HsExpr where
  arbitrary = oneof
    [ HsVar <$> arbitrary
    , HsLit <$> arbitrary
    , HsApp <$> arbitrary <*> arbitrary
    , HsLam <$> arbitrary <*> arbitrary
    ]

-- Generate valid patterns
instance Arbitrary Pattern where
  arbitrary = oneof
    [ MetaVar <$> elements ["X", "Y", "Z", "F", "G"]
    , pure Wildcard
    , App <$> arbitrary <*> arbitrary
    ]

-- Shrinking for better error messages
  shrink (App f x) = [f, x] ++ [App f' x' | (f', x') <- shrink (f, x)]
  shrink _ = []
```

### Property Modifiers

```haskell
spec :: Spec
spec = do
  describe "Fix application" $ do
    -- Limit test size for performance
    modifyMaxSize (const 20) $ do
      prop "preserves valid Haskell" $ \source fix ->
        isValidHaskell source ==>
          let result = applyFix fix source
          in isValidHaskell result

    -- More tests for edge cases
    modifyMaxSuccess (const 1000) $ do
      prop "handles all patterns" $ \pattern ->
        compilePattern pattern `shouldSatisfy` compiles
```

## Integration Tests

### Full Pipeline Tests

```haskell
spec :: Spec
spec = do
  describe "Full analysis pipeline" $ do
    it "analyzes project" $ do
      result <- analyzeProject defaultConfig ["test-project/"]
      resultErrors result `shouldBe` []
      length (resultDiagnostics result) `shouldSatisfy` (> 0)

    it "applies fixes correctly" $ do
      let source = "module Test where\nf = head items"
      (fixed, diags) <- analyzeAndFix defaultConfig source
      fixed `shouldContain` "headMay"
      diags `shouldSatisfy` null
```

### With Fixtures

```haskell
spec :: Spec
spec = do
  describe "Integration" $ around withTestProject $ do
    it "finds all issues in project" $ \projectDir -> do
      result <- analyzeProject config [projectDir]
      length (resultDiagnostics result) `shouldBe` 10

    it "fixes all auto-fixable issues" $ \projectDir -> do
      fixed <- fixProject config [projectDir]
      reanalysis <- analyzeProject config [projectDir]
      resultAutoFixable reanalysis `shouldBe` []

withTestProject :: (FilePath -> IO ()) -> IO ()
withTestProject action = withSystemTempDirectory "argus-test" $ \dir -> do
  copyTestProject "test/fixtures/sample-project" dir
  action dir
```

## Test Utilities

### Common Helpers

```haskell
-- test/TestUtils.hs
module TestUtils where

-- Parse expression for testing
parseExpr :: Text -> HsExpr GhcPs
parseExpr source = case runParser parseExpression source of
  Right expr -> expr
  Left err -> error $ "Parse failed: " ++ show err

-- Parse module for testing
parseModule :: Text -> IO ParsedModule
parseModule source = case parseModuleWithComments source of
  Right mod -> return mod
  Left err -> fail $ "Parse failed: " ++ show err

-- Run rule on source
runRule :: Rule -> Text -> IO [Diagnostic]
runRule rule source = do
  mod <- parseModule $ "module Test where\nf = " <> source
  return $ evaluateRule rule mod Nothing

-- Check diagnostic properties
hasDiagnostic :: RuleId -> [Diagnostic] -> Bool
hasDiagnostic ruleId diags = any ((== ruleId) . diagRule) diags

-- Check for specific suggestion
hasSuggestion :: Text -> [Diagnostic] -> Bool
hasSuggestion text diags = any ((== Just text) . diagSuggestion) diags
```

### Custom Matchers

```haskell
-- Custom HSpec matchers
shouldHaveDiagnostic :: [Diagnostic] -> RuleId -> Expectation
shouldHaveDiagnostic diags ruleId =
  diags `shouldSatisfy` hasDiagnostic ruleId

shouldHaveFix :: [Diagnostic] -> Text -> Expectation
shouldHaveFix diags fixText =
  case find (hasFix fixText) diags of
    Just _ -> return ()
    Nothing -> expectationFailure $
      "Expected fix containing: " ++ show fixText

shouldParseAs :: Text -> HsExpr GhcPs -> Expectation
shouldParseAs source expected =
  parseExpr source `shouldBe` expected
```

## Golden Tests

### Setup

```haskell
spec :: Spec
spec = do
  describe "Output formatting" $ do
    it "formats terminal output correctly" $ do
      diags <- loadDiagnostics "test/fixtures/sample-diags.json"
      output <- formatTerminal diags
      output `shouldMatchGolden` "test/golden/terminal-output.txt"

    it "formats JSON correctly" $ do
      diags <- loadDiagnostics "test/fixtures/sample-diags.json"
      output <- formatJson diags
      output `shouldMatchGolden` "test/golden/json-output.json"
```

### Golden File Management

```bash
# Update golden files after intentional changes
stack test --ta '--golden-reset'

# Update specific golden file
stack test --ta '-m "terminal output" --golden-reset'
```

## Test Organization

### By Feature

```haskell
-- test/Rules/PartialSpec.hs
module Rules.PartialSpec (spec) where

spec :: Spec
spec = do
  describe "Partial function rules" $ do
    headSpec
    tailSpec
    lastSpec
    initSpec

headSpec :: Spec
headSpec = describe "partial/head" $ do
  -- head tests

tailSpec :: Spec
tailSpec = describe "partial/tail" $ do
  -- tail tests
```

### Shared Context

```haskell
spec :: Spec
spec = do
  describe "With HIE context" $ do
    around withHIEContext $ do
      it "resolves types" $ \hie -> do
        let ty = getTypeAt (1, 1) hie
        ty `shouldBe` Just "Int"

withHIEContext :: (HIEContext -> IO ()) -> IO ()
withHIEContext action = do
  hie <- loadTestHIE "test/fixtures/sample.hie"
  action hie
```

## Best Practices

1. **Test one thing per test** - Each `it` block tests one behavior
2. **Use descriptive names** - Test names should explain what's tested
3. **Test edge cases** - Empty lists, Nothing, boundary conditions
4. **Use property tests** - For laws and invariants
5. **Isolate tests** - Each test should be independent
6. **Clean up resources** - Use `around` for setup/teardown

## Next Steps

- **[Test Fixtures](./test-fixtures)**: Managing test data
- **[Golden Tests](./golden-tests)**: Expected output testing
- **[Running Tests](./running-tests)**: Test execution
