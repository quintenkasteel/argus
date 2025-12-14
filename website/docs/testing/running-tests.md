---
sidebar_position: 1
title: Running Tests
description: How to run Argus test suite
---

# Running Tests

Argus has an extensive test suite covering all functionality. This guide explains how to run tests during development.

## Quick Start

```bash
# Run all tests
stack test

# Run with verbose output
stack test --ta '-v'

# Run specific test module
stack test --ta '-m "ASTMatch"'
```

## Test Categories

### Unit Tests

```bash
# Run unit tests only
stack test --ta '-m "Unit"'

# Specific unit test file
stack test --ta '-m "RuleEngine"'
stack test --ta '-m "ASTMatch"'
stack test --ta '-m "Substitution"'
```

### Integration Tests

```bash
# Run integration tests
stack test --ta '-m "Integration"'

# E2E integration
stack test --ta '-m "E2E"'
```

### Property Tests

```bash
# Run QuickCheck property tests
stack test --ta '-m "Property"'

# With specific seed for reproducibility
stack test --ta '--seed 12345'
```

### Golden Tests

```bash
# Run golden tests (expected output comparison)
stack test --ta '-m "Golden"'

# Update golden files
stack test --ta '--golden-reset'
```

## Test Runner Options

### Filtering

```bash
# Match test name pattern
stack test --ta '-m "pattern"'

# Exclude pattern
stack test --ta '--skip "slow"'

# Multiple patterns
stack test --ta '-m "ASTMatch" -m "Substitution"'
```

### Output Control

```bash
# Verbose output
stack test --ta '-v'

# Show all test names
stack test --ta '--format progress'

# Documentation format
stack test --ta '--format doc'
```

### Parallelism

```bash
# Run tests in parallel
stack test --ta '-j 4'

# Sequential (for debugging)
stack test --ta '-j 1'
```

### Failure Handling

```bash
# Stop on first failure
stack test --ta '--fail-fast'

# Rerun only failed tests
stack test --ta '--rerun'

# Rerun failed with new tests
stack test --ta '--rerun-all-on-success'
```

## Test Files

Test files are located in `test/`:

```
test/
├── Spec.hs              # Main test driver
├── TestUtils.hs         # Shared test utilities
│
├── ASTMatchSpec.hs      # AST pattern matching
├── CoreSpec.hs          # Core functionality
├── RuleEngineSpec.hs    # Rule evaluation
├── SubstitutionSpec.hs  # Pattern substitution
├── ExactPrintSpec.hs    # Formatting preservation
│
├── PartialSpec.hs       # Partial function rules
├── SecuritySpec.hs      # Security rules
├── PerformanceSpec.hs   # Performance rules
├── RedundantSpec.hs     # Redundant code rules
│
├── ConfigSpec.hs        # Configuration parsing
├── CLISpec.hs           # CLI parsing
├── OutputSpec.hs        # Output formatting
│
├── IntegrationSpec.hs   # Integration tests
├── E2EIntegrationSpec.hs # End-to-end tests
├── GoldenSpec.hs        # Golden file tests
├── PropertySpec.hs      # Property-based tests
│
└── golden/              # Golden test data
    ├── input/
    └── expected/
```

## Running Specific Tests

### By Module

```bash
# Core functionality
stack test --ta '-m "Core"'

# Rule categories
stack test --ta '-m "Partial"'
stack test --ta '-m "Security"'
stack test --ta '-m "Performance"'

# Analysis
stack test --ta '-m "Syntactic"'
stack test --ta '-m "Semantic"'
stack test --ta '-m "DepGraph"'

# Refactoring
stack test --ta '-m "ExactPrint"'
stack test --ta '-m "SafeRefactor"'
```

### By Test Name

```bash
# Specific test case
stack test --ta '-m "detects head usage"'

# Test within describe block
stack test --ta '-m "ASTMatch/matchPattern"'
```

## Coverage

### Generate Coverage Report

```bash
# Build with coverage
stack test --coverage

# View coverage report
open .stack-work/install/.../hpc/combined/all/hpc_index.html
```

### Coverage by Module

```bash
# Coverage for specific package
stack test --coverage argus

# Detailed coverage report
stack test --coverage --ta '--report-coverage'
```

## Debugging Tests

### Verbose Output

```bash
# Maximum verbosity
stack test --ta '-v2'

# Print test timing
stack test --ta '--times'
```

### Debug Single Test

```bash
# Run single test with full output
stack test --ta '-m "specific test" -v'

# With GHC debugging
stack test --ghc-options="-debug" --ta '-m "test"'
```

### REPL Testing

```bash
# Load tests in REPL
stack ghci test/Spec.hs

# Run specific spec
> hspec ASTMatchSpec.spec

# Run single test
> hspec $ it "test name" $ do ...
```

## Continuous Testing

### Watch Mode

```bash
# Rerun tests on file changes
stack test --file-watch

# With specific tests
stack test --file-watch --ta '-m "Core"'
```

### CI Commands

```bash
# Full CI test run
stack test --pedantic

# With coverage and JUnit output
stack test --coverage --ta '--format junit > test-results.xml'
```

## Performance Testing

### Benchmarks

```bash
# Run benchmarks
stack bench

# Specific benchmark
stack bench --ba '-m "ASTMatch"'

# Compare against baseline
stack bench --ba '--baseline baseline.csv'
```

### Profiling

```bash
# Profile test run
stack test --profile --ta '-m "slow test"'

# View profile
hp2ps -c argus-test.hp
open argus-test.ps
```

## Common Test Patterns

### Test with Setup

```haskell
describe "RuleEngine" $ do
  before (loadTestConfig "test/fixtures/config.toml") $ do
    it "evaluates rules correctly" $ \config -> do
      result <- runRules config testModule
      result `shouldSatisfy` hasNoErrors
```

### Property Test

```haskell
describe "Substitution" $ do
  prop "roundtrips correctly" $ \pattern ->
    let compiled = compilePattern pattern
        roundtrip = decompile (compile pattern)
    in roundtrip === pattern
```

### Golden Test

```haskell
describe "Output" $ do
  it "matches expected format" $ do
    output <- runAnalysis "test/golden/input/sample.hs"
    output `shouldMatchGolden` "test/golden/expected/sample.txt"
```

## Troubleshooting

### Tests Hanging

```bash
# Run with timeout
stack test --ta '--timeout 60'

# Find slow tests
stack test --ta '--times' | sort -k2 -n -r | head
```

### Flaky Tests

```bash
# Run multiple times
for i in {1..10}; do stack test --ta '-m "flaky"'; done

# With fixed seed
stack test --ta '--seed 12345 -m "flaky"'
```

### Out of Memory

```bash
# Limit memory
stack test --ghc-options="+RTS -M2G -RTS"

# Run sequential
stack test --ta '-j 1'
```

## Next Steps

- **[Writing Tests](./writing-tests)**: How to add new tests
- **[Test Fixtures](./test-fixtures)**: Using test data
- **[Golden Tests](./golden-tests)**: Expected output testing
