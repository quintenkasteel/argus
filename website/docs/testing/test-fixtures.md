---
sidebar_position: 3
title: Test Fixtures
description: Managing test data and fixtures
---

# Test Fixtures

Test fixtures provide consistent test data for Argus tests. This guide explains how to create and use fixtures.

## Fixture Organization

```
test/
├── fixtures/
│   ├── source/              # Haskell source files
│   │   ├── simple.hs
│   │   ├── complex.hs
│   │   └── with-issues.hs
│   │
│   ├── config/              # Configuration files
│   │   ├── default.toml
│   │   ├── strict.toml
│   │   └── custom-rules.toml
│   │
│   ├── hie/                 # HIE files for semantic tests
│   │   └── Sample.hie
│   │
│   ├── projects/            # Complete test projects
│   │   ├── sample-lib/
│   │   └── sample-app/
│   │
│   └── expected/            # Expected outputs
│       ├── diagnostics.json
│       └── fixed-source.hs
│
├── golden/                  # Golden test files
│   ├── input/
│   └── expected/
│
└── data/                    # Additional test data
    └── patterns.json
```

## Source Fixtures

### Simple Test Files

```haskell
-- test/fixtures/source/partial-functions.hs
module PartialFunctions where

-- Should trigger partial/head
useHead :: [a] -> a
useHead xs = head xs

-- Should trigger partial/tail
useTail :: [a] -> [a]
useTail xs = tail xs

-- Safe alternative - no warning
useSafeHead :: [a] -> Maybe a
useSafeHead xs = Safe.headMay xs
```

### Files with Known Issues

```haskell
-- test/fixtures/source/known-issues.hs
-- This file has exactly 5 issues:
-- 1. partial/head at line 5
-- 2. partial/tail at line 8
-- 3. redundant/id at line 11
-- 4. performance/length-null at line 14
-- 5. modernize/return-pure at line 17

module KnownIssues where

issue1 = head items           -- line 5
issue2 = tail items           -- line 8
issue3 = id value             -- line 11
issue4 = length xs == 0       -- line 14
issue5 = return x             -- line 17
```

### Complex Source Files

```haskell
-- test/fixtures/source/complex.hs
-- Complex file for performance and integration testing
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Complex where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

-- Complex nested structure
processData :: Map Text [Int] -> Either String (Map Text Int)
processData input = do
  validated <- validateInput input
  let transformed = transformData validated
  return $ aggregateData transformed
  where
    validateInput m
      | Map.null m = Left "Empty input"
      | otherwise = Right m

    transformData = Map.map (filter (> 0))

    aggregateData = Map.map sum
```

## Configuration Fixtures

### Test Configurations

```toml
# test/fixtures/config/strict.toml
[rules]
enabled = true
fail-on = "warning"

[rules.severity]
"partial/*" = "error"
"security/*" = "error"
"performance/*" = "error"
```

```toml
# test/fixtures/config/custom-rules.toml
[[patterns.rules]]
name = "test-rule"
match = "testPattern $X"
fix = "replacedPattern $X"
message = "Test rule message"
```

## Loading Fixtures

### Source File Loading

```haskell
-- test/TestUtils.hs

loadFixture :: FilePath -> IO Text
loadFixture name = do
  let path = "test/fixtures/source/" <> name
  TIO.readFile path

loadFixtureModule :: FilePath -> IO ParsedModule
loadFixtureModule name = do
  source <- loadFixture name
  parseModule source

-- With expected diagnostics
data FixtureWithExpected = FixtureWithExpected
  { fweSource :: Text
  , fweExpectedDiags :: Int
  , fweExpectedRules :: [RuleId]
  }

loadFixtureWithExpected :: FilePath -> IO FixtureWithExpected
loadFixtureWithExpected name = do
  let sourcePath = "test/fixtures/source/" <> name
      expectedPath = "test/fixtures/expected/" <> name <> ".json"
  source <- TIO.readFile sourcePath
  expected <- decodeFileStrict expectedPath
  return $ FixtureWithExpected source expected
```

### Configuration Loading

```haskell
loadTestConfig :: FilePath -> IO Config
loadTestConfig name = do
  let path = "test/fixtures/config/" <> name
  loadConfig path

withTestConfig :: FilePath -> (Config -> IO a) -> IO a
withTestConfig name action = do
  config <- loadTestConfig name
  action config
```

### Project Loading

```haskell
withTestProject :: FilePath -> (FilePath -> IO a) -> IO a
withTestProject name action = do
  let projectPath = "test/fixtures/projects/" <> name
  -- Copy to temp directory for isolation
  withSystemTempDirectory "argus-test" $ \tmpDir -> do
    copyDirectoryRecursive projectPath tmpDir
    action tmpDir
```

## HIE Fixtures

### Generating HIE Files

```bash
# Generate HIE files for test fixtures
cd test/fixtures/source
stack build --ghc-options="-fwrite-ide-info -hiedir=../hie"
```

### Loading HIE Fixtures

```haskell
loadTestHIE :: FilePath -> IO HIEContext
loadTestHIE name = do
  let path = "test/fixtures/hie/" <> name <> ".hie"
  loadHIEFile path

withHIE :: FilePath -> (HIEContext -> IO a) -> IO a
withHIE name action = do
  hie <- loadTestHIE name
  action hie
```

## Expected Output Fixtures

### Diagnostic Expectations

```json
// test/fixtures/expected/partial-functions.json
{
  "file": "partial-functions.hs",
  "diagnostics": [
    {
      "line": 5,
      "rule": "partial/head",
      "severity": "warning"
    },
    {
      "line": 8,
      "rule": "partial/tail",
      "severity": "warning"
    }
  ]
}
```

### Using Expected Output

```haskell
spec :: Spec
spec = do
  describe "Partial function detection" $ do
    it "finds expected issues" $ do
      source <- loadFixture "partial-functions.hs"
      expected <- loadExpected "partial-functions.json"
      diags <- analyze source
      matchDiagnostics diags expected `shouldBe` True

matchDiagnostics :: [Diagnostic] -> ExpectedDiagnostics -> Bool
matchDiagnostics actual expected =
  length actual == length (expectedDiags expected) &&
  all (matchesSome actual) (expectedDiags expected)
```

## Inline Fixtures

### Small Test Cases

```haskell
spec :: Spec
spec = do
  describe "head detection" $ do
    it "detects simple usage" $ do
      let source = [r|
        module Test where
        f xs = head xs
      |]
      diags <- runAnalysis source
      diags `shouldHaveRule` "partial/head"

    it "detects in let binding" $ do
      let source = [r|
        module Test where
        f xs = let x = head xs in x
      |]
      diags <- runAnalysis source
      diags `shouldHaveRule` "partial/head"
```

### QuasiQuoter for Source

```haskell
-- Using raw string quasi-quoter
import Text.RawString.QQ (r)

testSource :: Text
testSource = [r|
  module TestModule where

  import Data.List

  processItems :: [Int] -> Int
  processItems items = head items
|]
```

## Fixture Generators

### Generate Test Cases

```haskell
-- Generate source with N issues
generateSourceWithIssues :: Int -> Text
generateSourceWithIssues n = T.unlines $
  ["module Generated where"] ++
  [T.pack $ "issue" ++ show i ++ " = head items" | i <- [1..n]]

-- Generate complex nested source
generateNestedSource :: Int -> Text
generateNestedSource depth = T.unlines
  [ "module Nested where"
  , "f = " <> generateNested depth
  ]
  where
    generateNested 0 = "x"
    generateNested d = "case x of\n" <>
      T.replicate d "  " <> "A -> " <> generateNested (d-1)
```

## Fixture Isolation

### Temp Directory Usage

```haskell
-- Ensure tests don't interfere
withIsolatedFixture :: FilePath -> (FilePath -> IO a) -> IO a
withIsolatedFixture fixture action =
  withSystemTempDirectory "argus-test" $ \tmpDir -> do
    let srcPath = "test/fixtures/" <> fixture
    let dstPath = tmpDir </> takeFileName fixture
    copyFile srcPath dstPath
    action dstPath

-- For project fixtures
withIsolatedProject :: FilePath -> (FilePath -> IO a) -> IO a
withIsolatedProject project action =
  withSystemTempDirectory "argus-test" $ \tmpDir -> do
    let srcPath = "test/fixtures/projects/" <> project
    copyDirectoryRecursive srcPath tmpDir
    action tmpDir
```

### Cleanup

```haskell
-- Ensure cleanup even on failure
withCleanup :: IO a -> (a -> IO ()) -> (a -> IO b) -> IO b
withCleanup acquire release action = do
  resource <- acquire
  result <- action resource `onException` release resource
  release resource
  return result
```

## Best Practices

1. **Keep fixtures minimal** - Include only what's needed for the test
2. **Document fixtures** - Add comments explaining purpose
3. **Use isolation** - Copy to temp directories when modifying
4. **Version expected outputs** - Keep in sync with code changes
5. **Generate when possible** - Use generators for variations

## Next Steps

- **[Golden Tests](./golden-tests)**: Expected output comparison
- **[Writing Tests](./writing-tests)**: Test implementation
- **[Running Tests](./running-tests)**: Test execution
