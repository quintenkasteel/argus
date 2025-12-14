---
sidebar_position: 4
title: Testing Guide
description: How to write and run tests for Argus
---

# Testing Guide

This guide explains how to write tests for Argus contributions. Good tests ensure your changes work correctly and don't break existing functionality.

## Test Framework

Argus uses:

| Framework | Purpose |
|-----------|---------|
| Hspec | Test organization and assertions |
| QuickCheck | Property-based testing |
| hspec-golden | Golden file tests |

## Running Tests

### All Tests

```bash
# Run complete test suite
stack test

# With verbose output
stack test --ta '-v'
```

### Specific Tests

```bash
# By module name
stack test --ta '-m "ASTMatch"'

# By test description
stack test --ta '-m "detects head usage"'

# Multiple patterns
stack test --ta '-m "Partial" -m "Security"'
```

### Test Options

```bash
# Stop on first failure
stack test --ta '--fail-fast'

# Run with fixed seed (reproducible)
stack test --ta '--seed 12345'

# Parallel execution
stack test --ta '-j 4'
```

## Writing Unit Tests

### Basic Structure

```haskell
-- test/MyFeatureSpec.hs
module MyFeatureSpec (spec) where

import Test.Hspec
import Argus.MyFeature
import TestUtils

spec :: Spec
spec = do
  describe "MyFeature" $ do
    describe "functionA" $ do
      it "handles normal input" $ do
        functionA normalInput `shouldBe` expectedOutput

      it "handles edge case" $ do
        functionA [] `shouldBe` defaultValue

    describe "functionB" $ do
      it "returns Nothing for invalid input" $ do
        functionB invalid `shouldBe` Nothing
```

### Testing Rules

```haskell
spec :: Spec
spec = do
  describe "partial/head rule" $ do
    it "detects simple head usage" $ do
      let source = "module T where\nf xs = head xs"
      diags <- runRule headRule source
      length diags `shouldBe` 1

    it "provides correct fix" $ do
      let source = "module T where\nf xs = head xs"
      diags <- runRule headRule source
      diagFix (head diags) `shouldBe` Just "headMay xs"

    it "ignores qualified Safe.head" $ do
      let source = "module T where\nimport Safe\nf xs = Safe.headMay xs"
      diags <- runRule headRule source
      diags `shouldBe` []

    it "detects head in complex expressions" $ do
      let source = "module T where\nf xs = show (head xs)"
      diags <- runRule headRule source
      length diags `shouldBe` 1
```

### Testing Fixes

```haskell
spec :: Spec
spec = do
  describe "Fix application" $ do
    it "applies simple replacement" $ do
      let source = "module T where\nf xs = head xs"
          expectedFixed = "module T where\nf xs = headMay xs"
      (fixed, _) <- analyzeAndFix defaultConfig source
      fixed `shouldBe` expectedFixed

    it "adds required imports" $ do
      let source = "module T where\nf xs = head xs"
      (fixed, _) <- analyzeAndFix defaultConfig source
      fixed `shouldContain` "import Safe (headMay)"

    it "preserves formatting" $ do
      let source = "module T where\n\nf xs = head xs  -- comment"
      (fixed, _) <- analyzeAndFix defaultConfig source
      fixed `shouldContain` "-- comment"
```

## Property-Based Tests

### Basic Properties

```haskell
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Pattern matching" $ do
    prop "compiled pattern matches itself" $ \pattern ->
      isValidPattern pattern ==>
        let compiled = compilePattern pattern
            expr = patternToExpr pattern
        in matchPattern compiled expr `shouldSatisfy` isJust

    prop "match bindings are correct" $ \pattern expr ->
      isValidPattern pattern ==>
        case matchPattern (compilePattern pattern) expr of
          Just bindings -> applyBindings bindings pattern == expr
          Nothing -> True
```

### Custom Generators

```haskell
-- Generate valid Haskell identifiers
genIdentifier :: Gen Text
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ T.pack (first : rest)

-- Generate simple expressions
genExpr :: Gen Text
genExpr = oneof
  [ genIdentifier
  , do
      f <- genIdentifier
      x <- genIdentifier
      return $ f <> " " <> x
  , do
      x <- genIdentifier
      return $ "(" <> x <> ")"
  ]

-- Use in tests
prop "parses generated expressions" $ forAll genExpr $ \expr ->
  parseExpr expr `shouldSatisfy` isRight
```

### Shrinking

```haskell
-- Custom shrinking for better error messages
instance Arbitrary Pattern where
  arbitrary = ...

  shrink (App f x) = [f, x] ++ [App f' x' | (f', x') <- shrink (f, x)]
  shrink (MetaVar _) = []
  shrink Wildcard = []
```

## Golden Tests

### Setup

```haskell
-- test/GoldenSpec.hs
module GoldenSpec (spec) where

import Test.Hspec
import Test.Hspec.Golden
import Argus

spec :: Spec
spec = do
  describe "Output formats" $ do
    golden "terminal-output" $ do
      source <- readFile "test/golden/input/sample.hs"
      diags <- analyze defaultConfig source
      return $ formatTerminal diags

    golden "json-output" $ do
      source <- readFile "test/golden/input/sample.hs"
      diags <- analyze defaultConfig source
      return $ formatJson diags
```

### Managing Golden Files

```bash
# Update golden files after intentional changes
stack test --ta '--golden-reset'

# Update specific golden file
stack test --ta '-m "terminal-output" --golden-reset'

# Review changes
git diff test/golden/expected/
```

### Golden File Structure

```
test/golden/
├── input/
│   ├── simple.hs
│   ├── complex.hs
│   └── with-issues.hs
└── expected/
    ├── simple.terminal.txt
    ├── simple.json.txt
    ├── complex.terminal.txt
    └── with-issues.terminal.txt
```

## Integration Tests

### Full Pipeline Tests

```haskell
spec :: Spec
spec = do
  describe "Full analysis pipeline" $ do
    it "analyzes project successfully" $ do
      result <- analyzeProject defaultConfig ["test/fixtures/sample-project/"]
      resultErrors result `shouldBe` []

    it "finds expected issues" $ do
      result <- analyzeProject defaultConfig ["test/fixtures/known-issues/"]
      length (resultDiagnostics result) `shouldBe` 5

    it "applies all fixes correctly" $ do
      withTempCopy "test/fixtures/fixable/" $ \tmpDir -> do
        fixProject defaultConfig [tmpDir]
        result <- analyzeProject defaultConfig [tmpDir]
        resultAutoFixable result `shouldBe` []
```

### CLI Tests

```haskell
spec :: Spec
spec = do
  describe "CLI" $ do
    it "parses check command" $ do
      let args = ["check", "src/", "--format", "json"]
      parseArgs args `shouldBe` Right (CheckCmd ["src/"] JsonFormat)

    it "handles --help" $ do
      let args = ["--help"]
      parseArgs args `shouldBe` Right HelpCmd

    it "returns error for invalid command" $ do
      let args = ["invalid"]
      parseArgs args `shouldSatisfy` isLeft
```

## Test Utilities

### TestUtils Module

```haskell
-- test/TestUtils.hs
module TestUtils where

import Argus
import Test.Hspec

-- Run rule on source
runRule :: Rule -> Text -> IO [Diagnostic]
runRule rule source = do
  let fullSource = ensureModule source
  mod <- parseModule fullSource
  return $ evaluateRule rule mod Nothing

-- Ensure source has module declaration
ensureModule :: Text -> Text
ensureModule source
  | "module " `T.isPrefixOf` source = source
  | otherwise = "module Test where\n" <> source

-- Parse expression for testing
parseExpr :: Text -> Either ParseError (HsExpr GhcPs)
parseExpr = runParser parseExpression

-- Apply fixes to source
applyFixes :: [Diagnostic] -> Text -> Text
applyFixes diags source = foldl' applyFix source (mapMaybe diagFix diags)

-- Temporary directory helper
withTempCopy :: FilePath -> (FilePath -> IO a) -> IO a
withTempCopy src action = withSystemTempDirectory "argus-test" $ \tmp -> do
  copyDirectoryRecursive src tmp
  action tmp
```

### Custom Matchers

```haskell
-- Custom HSpec matchers
shouldHaveRule :: [Diagnostic] -> RuleId -> Expectation
shouldHaveRule diags ruleId =
  any ((== ruleId) . diagRule) diags `shouldBe` True

shouldHaveFix :: [Diagnostic] -> Text -> Expectation
shouldHaveFix diags fixText =
  any (maybe False (fixText `T.isInfixOf`) . diagFix) diags `shouldBe` True

shouldParseSuccessfully :: Text -> Expectation
shouldParseSuccessfully source =
  parseModule source `shouldSatisfy` isRight
```

## Test Best Practices

### Organization

1. **One spec file per module**: `ModuleName.hs` → `ModuleNameSpec.hs`
2. **Group related tests**: Use nested `describe` blocks
3. **Clear test names**: Describe behavior, not implementation

### Coverage

1. **Happy path**: Normal expected usage
2. **Edge cases**: Empty input, boundaries
3. **Error cases**: Invalid input, failures
4. **Integration**: Component interactions

### Quality

1. **Independent tests**: No test depends on another
2. **Deterministic**: Same result every run
3. **Fast**: Avoid unnecessary IO
4. **Focused**: One assertion per test

## Running in CI

### CI Configuration

```yaml
# .github/workflows/test.yml
test:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v3
    - uses: haskell/actions/setup@v2
      with:
        enable-stack: true

    - name: Run tests
      run: stack test --pedantic

    - name: Run with coverage
      run: stack test --coverage
```

### Coverage Requirements

Aim for:
- **Rules**: 100% coverage of match cases
- **Core**: 90%+ coverage
- **CLI**: 80%+ coverage

## Next Steps

- **[Adding Rules](./adding-rules)**: Create new rules
- **[Pull Requests](./pull-requests)**: Submit changes
- **[Code Style](./code-style)**: Follow conventions
