# Argus Benchmark Corpus

This directory contains synthetic Haskell source files for benchmarking Argus.

## Directory Structure

```
corpus/
├── small/           # Small files (~30-50 lines each)
│   ├── Simple.hs
│   ├── PartialFunctions.hs
│   └── Security.hs
├── medium/          # Medium files (~100-200 lines)
│   └── DataProcessor.hs
├── large/           # Large files (~500+ lines)
│   └── LargeModule.hs
├── complex/         # High cyclomatic complexity
│   └── ComplexLogic.hs
└── template-haskell/ # Template Haskell patterns
    └── THExample.hs
```

## Corpus Statistics

| Category | Files | Approx. Lines | Purpose |
|----------|-------|---------------|---------|
| small | 3 | ~150 total | Quick parsing benchmarks |
| medium | 1+ | ~150 each | Typical module size |
| large | 1+ | ~500 each | Stress testing |
| complex | 1 | ~200 | High cyclomatic complexity |
| template-haskell | 1 | ~60 | TH detection testing |

## Code Patterns Included

### Partial Functions
- `head`, `tail`, `init`, `last`
- `fromJust`, `!!`
- `error`, `undefined`
- `maximum`, `minimum` on potentially empty lists

### Security Issues
- `unsafePerformIO` usage
- Command injection patterns
- Hardcoded credentials
- SQL injection patterns

### Performance Issues
- `foldl` instead of `foldl'`
- String concatenation in loops
- Repeated list operations

### Style Issues
- Naming convention violations
- Missing type signatures
- Redundant code patterns

## Usage

The corpus is used by `bench/Main.hs` for benchmarking. The benchmarks
primarily use inline-generated source to avoid I/O overhead, but these
files can be used for:

1. File-based benchmarks
2. Integration testing
3. Manual testing during development
4. CI performance regression detection

## Adding New Corpus Files

When adding new files:

1. Choose the appropriate category directory
2. Ensure the file is syntactically valid Haskell
3. Include a mix of issues for the linter to detect
4. Update this README with any new patterns

## Generating Large Corpus

For very large benchmarks, use the generators in `bench/Main.hs` which
create synthetic source programmatically:

- `generateSource n` - Generate n lines of code
- `generateSourceWithNFuncs n` - Generate n functions
