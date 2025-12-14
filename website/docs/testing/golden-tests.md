---
sidebar_position: 4
title: Golden Tests
description: Testing with expected output files
---

# Golden Tests

Golden tests compare actual output against expected output files. They're ideal for testing output formatting, fix generation, and any functionality with stable text output.

## Overview

Golden tests work by:
1. Running code to produce output
2. Comparing output to a "golden" expected file
3. Failing if they differ
4. Optionally updating the golden file

## Golden Test Structure

```
test/golden/
├── input/                 # Input files
│   ├── simple.hs
│   ├── complex.hs
│   └── with-issues.hs
│
├── expected/              # Expected output files
│   ├── simple.terminal.txt
│   ├── simple.json.txt
│   ├── simple.fixed.hs
│   ├── complex.terminal.txt
│   └── with-issues.terminal.txt
│
└── GoldenSpec.hs          # Golden test definitions
```

## Writing Golden Tests

### Basic Golden Test

```haskell
-- test/GoldenSpec.hs
module GoldenSpec (spec) where

import Test.Hspec
import Test.Hspec.Golden
import Argus

spec :: Spec
spec = do
  describe "Terminal output" $ do
    golden "simple" $ do
      source <- readFile "test/golden/input/simple.hs"
      diags <- analyze defaultConfig source
      return $ formatTerminal diags

    golden "complex" $ do
      source <- readFile "test/golden/input/complex.hs"
      diags <- analyze defaultConfig source
      return $ formatTerminal diags
```

### Custom Golden

```haskell
import Test.Hspec.Golden

spec :: Spec
spec = do
  describe "JSON output" $ do
    it "matches expected" $ do
      source <- readFile "test/golden/input/simple.hs"
      diags <- analyze defaultConfig source
      let output = formatJson diags
      output `shouldMatchGolden` "test/golden/expected/simple.json.txt"

-- Custom matcher
shouldMatchGolden :: Text -> FilePath -> Expectation
shouldMatchGolden actual goldenPath = do
  expected <- TIO.readFile goldenPath
  if actual == expected
    then return ()
    else do
      -- Show diff on failure
      putStrLn $ "Golden mismatch for: " ++ goldenPath
      putStrLn $ showDiff expected actual
      expectationFailure "Output does not match golden file"
```

### With Normalization

```haskell
-- Normalize output before comparison
golden "normalized" $ do
  output <- runAnalysis input
  return $ normalize output

normalize :: Text -> Text
normalize = T.unlines
  . map normalizeTimestamps
  . map normalizePaths
  . T.lines

normalizeTimestamps :: Text -> Text
normalizeTimestamps = T.replace timestampPattern "[TIMESTAMP]"

normalizePaths :: Text -> Text
normalizePaths = T.replace homePath "[HOME]"
```

## Golden Test Categories

### Output Format Tests

```haskell
spec :: Spec
spec = do
  describe "Output formats" $ do
    forM_ testCases $ \(name, formatter) ->
      describe name $ do
        goldenFormat name formatter "simple"
        goldenFormat name formatter "complex"
        goldenFormat name formatter "with-issues"

goldenFormat :: String -> Formatter -> String -> Spec
goldenFormat formatName formatter inputName =
  golden (inputName ++ "-" ++ formatName) $ do
    source <- loadInput inputName
    diags <- analyze defaultConfig source
    return $ runFormatter formatter diags

testCases =
  [ ("terminal", formatTerminal)
  , ("json", formatJson)
  , ("sarif", formatSarif)
  ]
```

### Fix Generation Tests

```haskell
spec :: Spec
spec = do
  describe "Fix generation" $ do
    golden "head-fix" $ do
      let source = "module T where\nf = head xs"
      (fixed, _) <- analyzeAndFix defaultConfig source
      return fixed

    golden "multiple-fixes" $ do
      source <- loadInput "multiple-issues"
      (fixed, _) <- analyzeAndFix defaultConfig source
      return fixed
```

### CLI Output Tests

```haskell
spec :: Spec
spec = do
  describe "CLI output" $ do
    golden "help" $ do
      output <- runCLI ["--help"]
      return output

    golden "version" $ do
      output <- runCLI ["--version"]
      return output

    golden "check-output" $ do
      output <- runCLI ["check", "test/golden/input/simple.hs"]
      return $ normalizeOutput output
```

## Managing Golden Files

### Updating Golden Files

```bash
# Update all golden files
stack test --ta '--golden-reset'

# Update specific test
stack test --ta '-m "simple" --golden-reset'

# Interactive update (shows diff first)
stack test --ta '--golden-update'
```

### Reviewing Changes

```bash
# Show diffs for golden file changes
git diff test/golden/expected/

# Review specific file
diff test/golden/expected/simple.terminal.txt.new \
     test/golden/expected/simple.terminal.txt
```

### Git Integration

```gitignore
# .gitignore
# Don't ignore golden files - they're test expectations
!test/golden/expected/

# Do ignore temporary new files
test/golden/expected/*.new
```

## Input Files

### Simple Input

```haskell
-- test/golden/input/simple.hs
module Simple where

-- Single issue
useHead :: [a] -> a
useHead xs = head xs
```

### Complex Input

```haskell
-- test/golden/input/complex.hs
{-# LANGUAGE LambdaCase #-}

module Complex where

import Data.List (sort)

-- Multiple issues in complex code
process :: [Int] -> Int
process = head . sort . filter (> 0)

transform :: Maybe a -> a
transform = \case
  Just x -> x
  Nothing -> error "unexpected Nothing"
```

### With All Categories

```haskell
-- test/golden/input/all-categories.hs
module AllCategories where

-- partial
issue1 = head items

-- redundant
issue2 = id value

-- performance
issue3 = length xs == 0

-- modernize
issue4 = return x

-- security
issue5 = unsafePerformIO action
```

## Expected Output Examples

### Terminal Output

```
-- test/golden/expected/simple.terminal.txt
test/golden/input/simple.hs:5:12: warning [partial/head]
  Use headMay instead of head
  |
5|   useHead xs = head xs
  |               ^^^^
  = suggestion: Replace with `headMay xs`

Found 1 warning in 1 file
```

### JSON Output

```json
// test/golden/expected/simple.json.txt
{
  "version": "1.0",
  "diagnostics": [
    {
      "file": "test/golden/input/simple.hs",
      "line": 5,
      "column": 12,
      "severity": "warning",
      "rule": "partial/head",
      "message": "Use headMay instead of head",
      "suggestion": "headMay xs"
    }
  ],
  "summary": {
    "files": 1,
    "warnings": 1
  }
}
```

### Fixed Source

```haskell
-- test/golden/expected/simple.fixed.hs
module Simple where

import Safe (headMay)

-- Single issue (fixed)
useHead :: [a] -> Maybe a
useHead xs = headMay xs
```

## Best Practices

### 1. Keep Golden Files Small

```haskell
-- Good: focused test
golden "head-warning" $ do
  return $ formatDiagnostic headDiag

-- Avoid: large output that's hard to review
golden "entire-project" $ do
  output <- analyzeEntireProject
  return output
```

### 2. Normalize Variable Content

```haskell
-- Normalize content that varies
normalize output = output
  & T.replace currentTime "[TIME]"
  & T.replace currentDir "[DIR]"
  & T.replace (T.pack $ show version) "[VERSION]"
```

### 3. Organize by Feature

```
test/golden/
├── output/
│   ├── terminal/
│   ├── json/
│   └── sarif/
├── fixes/
│   ├── partial/
│   └── redundant/
└── cli/
    ├── help/
    └── errors/
```

### 4. Document Changes

```bash
# When updating golden files, explain why in commit message
git commit -m "Update golden files: new diagnostic format

Changed terminal output to include suggestion on same line.
All golden files updated to reflect new format."
```

## Troubleshooting

### Test Fails on CI but Passes Locally

```haskell
-- Likely cause: path or system differences
-- Solution: normalize all variable content

normalize = normalizeLineEndings
  . normalizePaths
  . normalizeTimestamps
```

### Large Diffs

```bash
# Use a visual diff tool
vimdiff test/golden/expected/complex.txt actual-output.txt

# Or diff-so-fancy
diff test/golden/expected/complex.txt actual-output.txt | diff-so-fancy
```

### Accidental Updates

```bash
# Revert golden files to last commit
git checkout HEAD -- test/golden/expected/

# Or restore specific file
git checkout HEAD -- test/golden/expected/simple.terminal.txt
```

## Next Steps

- **[Running Tests](./running-tests)**: Execute golden tests
- **[Writing Tests](./writing-tests)**: Other test types
- **[Test Fixtures](./test-fixtures)**: Input file management
