# Argus Performance Regression Tests

## Overview

The performance regression tests track Argus performance over time and automatically detect regressions. Tests fail if performance degrades by more than 20%.

## Files

- `test/PerformanceRegressionSpec.hs` - Main test suite with baseline tracking
- `test/data/perf-benchmark.hs` - Realistic Haskell code for benchmarking (444 lines)
- `test/data/performance-baselines.json` - Baseline measurements storage
- `.github/workflows/performance.yml` - CI workflow for automated performance testing

## Test Coverage

### 1. Parsing Performance
- Parse realistic file (400+ lines)
- Parse file with many issues
- Parse clean file with no issues

### 2. Analysis Performance
- Analyze file with many issues (50+ diagnostics)
- Analyze file with no issues (clean code)
- Analyze realistic file
- Extract functions from realistic file

### 3. Fix Application Performance
- Apply single fix
- Apply multiple fixes sequentially
- Apply fixes to realistic file

### 4. Scalability Tests
- Analyze 100-line files
- Analyze 500-line files
- Analyze 1000-line files

### 5. Baseline Management Tests
- Load and save baselines
- Detect regressions correctly
- Calculate percentage increase
- Handle performance improvements

## How It Works

### Baseline Recording

On first run, each test records its execution time as a baseline:

```
✓ Baseline recorded: parse-realistic-file = 0.123s
```

Baselines are saved to `test/data/performance-baselines.json`:

```json
{
  "parse-realistic-file": {
    "testName": "parse-realistic-file",
    "baselineSeconds": 0.123,
    "recordedAt": "recorded-during-test",
    "environment": "test-env"
  }
}
```

### Regression Detection

On subsequent runs, each test compares against its baseline:

```
Baseline: 0.123s
Current:  0.145s
Change:   +17.9% (slower)
```

If the increase exceeds 20%, the test fails:

```
Performance regression detected!
  Test: parse-realistic-file
  Baseline: 0.123s
  Current: 0.156s
  Increase: 26.8%
  Threshold: 20%
```

### Regression Threshold

The threshold is set to 20% to account for normal variation while catching real regressions:

```haskell
regressionThreshold :: Double
regressionThreshold = 0.20  -- 20%

isRegression :: Double -> Double -> Bool
isRegression baseline current =
  current > baseline * (1.0 + regressionThreshold)
```

## Running Tests

### Locally

Run all performance tests:

```bash
stack test --ta '-m "PerformanceRegression"'
```

Run specific test group:

```bash
stack test --ta '-m "Parsing Performance"'
stack test --ta '-m "Analysis Performance"'
stack test --ta '-m "Fix Application Performance"'
```

### In CI

Performance tests run automatically:

1. **On every push/PR** - Runs regression tests
2. **Daily at 2 AM UTC** - Scheduled runs for baseline tracking
3. **Manual trigger** - Via GitHub Actions workflow dispatch

### Resetting Baselines

To re-establish baselines after intentional changes:

```bash
# Delete existing baselines
rm test/data/performance-baselines.json

# Run tests to record new baselines
stack test --ta '-m "PerformanceRegression"'
```

## CI Workflow Features

### Performance Regression Job

- Runs on every push/PR
- Compares against cached baselines
- Fails if regressions detected
- Posts results as PR comment

### Benchmark Comparison Job

- Runs Criterion benchmarks
- Saves results as artifacts
- Generates HTML reports
- Compares with previous runs

### Memory Profiling Job

- Runs on push/schedule only
- Generates heap profiles
- Checks for space leaks
- Uploads profiling artifacts

## Interpreting Results

### Baseline Recording

```
✓ Baseline recorded: analyze-realistic-file = 1.234s
```

First run established a baseline. Subsequent runs will compare against this.

### No Regression

```
Baseline: 1.234s
Current:  1.280s
Change:   +3.7% (slower)
```

Performance is within acceptable range (< 20% degradation).

### Regression Detected

```
Performance regression detected!
  Test: analyze-realistic-file
  Baseline: 1.234s
  Current: 1.543s
  Increase: 25.0%
  Threshold: 20%
```

Performance degraded by more than 20%. Investigate the change.

### Performance Improvement

```
Baseline: 1.234s
Current:  0.987s
Change:   -20.0% (faster)
```

Performance improved! Consider updating baseline.

## Benchmark File

`test/data/perf-benchmark.hs` contains realistic Haskell code:

- **Lines**: 444
- **Functions**: 40+
- **Data types**: 4 (User, Post, Comment, Tag)
- **Intentional issues**:
  - Partial functions (head, tail, fromJust)
  - O(n) operations (elem, nub)
  - Inefficient patterns (concat . map)

This provides a representative workload for testing Argus performance.

## Baseline Storage Format

```json
{
  "test-name": {
    "testName": "test-name",
    "baselineSeconds": 1.234,
    "recordedAt": "recorded-during-test",
    "environment": "test-env"
  }
}
```

Fields:
- `testName` - Unique identifier
- `baselineSeconds` - Baseline execution time
- `recordedAt` - When recorded (for reference)
- `environment` - Environment description

## Best Practices

### 1. Run Tests Consistently

Run on the same machine with similar load for accurate comparisons.

### 2. Update Baselines After Intentional Changes

After legitimate performance improvements/degradations:

```bash
rm test/data/performance-baselines.json
stack test --ta '-m "PerformanceRegression"'
```

### 3. Investigate Regressions

When a regression is detected:

1. Review recent changes
2. Profile the code with `stack build --profile`
3. Run heap profiling: `argus +RTS -h -p`
4. Check for algorithmic changes
5. Look for accidental space leaks

### 4. Track Trends

Use the CI artifacts to track performance over time:

- View benchmark HTML reports
- Compare CSV results across commits
- Monitor heap profiles for memory trends

## Example Output

```
Argus Performance Regression Tests
  Parsing Performance
    parse-realistic-file - performance regression check
      ✓ Baseline recorded: parse-realistic-file = 0.089s
    parse-many-issues-source - performance regression check
      Baseline: 0.045s
      Current:  0.051s
      Change:   +13.3% (slower)
    parse-clean-source - performance regression check
      Baseline: 0.038s
      Current:  0.036s
      Change:   -5.3% (faster)

  Analysis Performance
    analyze-file-with-many-issues - performance regression check
      Baseline: 0.234s
      Current:  0.287s
      Change:   +22.6% (slower)
      FAILED: Performance regression detected!
        Test: analyze-file-with-many-issues
        Baseline: 0.234s
        Current: 0.287s
        Increase: 22.6%
        Threshold: 20%
```

## Configuration

### Threshold

Adjust the regression threshold in `PerformanceRegressionSpec.hs`:

```haskell
regressionThreshold :: Double
regressionThreshold = 0.20  -- Change to 0.15 for 15%, etc.
```

### Baselines File

Change the storage location:

```haskell
baselinesFile :: FilePath
baselinesFile = "test/data/performance-baselines.json"
```

## Troubleshooting

### Tests Won't Compile

Ensure all dependencies are built:

```bash
stack build --test --no-run-tests
```

### Baselines Not Persisting

Check write permissions:

```bash
ls -la test/data/
chmod 644 test/data/performance-baselines.json
```

### CI Tests Failing

Check the workflow logs for:
- Baseline cache misses
- Environment differences
- Actual regressions

### Inconsistent Results

Performance tests can be affected by:
- System load
- CPU throttling
- Background processes
- GC timing

Run multiple times and look for consistent trends.

## Future Enhancements

Potential improvements:

1. **Statistical Analysis** - Use mean + stddev over multiple runs
2. **Percentile Tracking** - Track p50, p95, p99 latencies
3. **Automatic Regression Bisection** - Find the commit that caused regression
4. **Memory Regression Tests** - Track memory usage over time
5. **Comparative Benchmarks** - Compare against other linters
