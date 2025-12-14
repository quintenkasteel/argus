# Argus Test Project

This project demonstrates all major features of the Argus Haskell static analyzer. Each module contains intentional issues that Argus should detect.

## Building

```bash
cd test-project
stack build
```

This generates HIE files in `.hie/` for full mode analysis.

## Running Argus

Quick mode (source-based analysis):
```bash
cabal run argus -- check -c test-project/argus.toml -m quick test-project/src
```

Full mode (requires compatible GHC version for HIE files):
```bash
cabal run argus -- check -c test-project/argus.toml -m full --hie-dir test-project/.hie test-project/src
```

## Latest Analysis Results

**Summary**: 525 issues found across 14 files with 139 auto-fix opportunities

## Module Guide

### PartialFunctions.hs
Demonstrates detection of partial functions:
- `head`, `tail`, `init`, `last` - list operations that crash on empty lists
- `!!` - partial indexing
- `fromJust` - crashes on Nothing
- `read` - crashes on invalid input
- `error` and `undefined` - explicit crashes

**Expected detections**: ~15 partial function warnings

### Security.hs
Demonstrates security rule detection:
- SQL injection via string concatenation
- Command injection via `system` and `readProcess`
- Hardcoded secrets (passwords, API keys)
- `unsafePerformIO` and `unsafeDupablePerformIO` usage
- Debug code (`trace`, `traceShow`, `traceShowId`)
- Path traversal risks

**Expected detections**: ~20 security warnings/errors

### Performance.hs
Demonstrates performance issue detection:
- `String` instead of `Text` usage
- List used as map/set (O(n) lookups)
- `length xs == 0` instead of `null xs`
- `head . sort` instead of `minimum`
- `concat . map` instead of `concatMap`
- `mconcat . map` instead of `foldMap`
- `liftM` instead of `fmap`
- `mapM` instead of `traverse`
- Lazy State monad issues

**Expected detections**: ~40 performance suggestions

### SpaceLeaks.hs
Demonstrates space leak pattern detection:
- `foldl` instead of `foldl'` (lazy accumulator)
- Lazy IO (`readFile`, `hGetContents`)
- Non-strict record fields (thunk accumulation)
- Lazy State monad
- CAF (Constant Applicative Form) space leaks
- Non-strict accumulators in manual folds

**Expected detections**: ~25 space leak warnings

### Complexity.hs
Demonstrates complexity metric detection:
- High cyclomatic complexity (many branches)
- High cognitive complexity (deep nesting)
- Long functions with too many expressions
- Complex boolean expressions
- Functions with too many parameters

**Expected detections**: Complexity warnings for `classifyNumber`, `complexMatcher`, `deeplyNested`, `manyParams`

### Imports.hs
Demonstrates import analysis:
- Unused imports (`Data.Maybe`, `Data.Map`)
- Partially used import lists
- Wildcard imports that should be explicit
- Missing explicit export list

**Expected detections**: ~10 import-related suggestions

### Pragmas.hs
Demonstrates LANGUAGE pragma analysis:
- Used pragmas: `OverloadedStrings`, `BangPatterns`, `LambdaCase`, `TupleSections`, `RecordWildCards`, `NamedFieldPuns`, `DeriveGeneric`, `DeriveFunctor`, `MultiWayIf`
- Unused pragmas: `FlexibleContexts`, `FlexibleInstances`, `TypeFamilies`, `GADTs`, `RankNTypes`, `ScopedTypeVariables`

**Expected detections**: Warnings for unused pragmas

### Duplicates.hs
Demonstrates duplicate code detection:
- Exact duplicates (`processUser1` / `processUser2`)
- Near-duplicates with minor differences
- Similar validation functions
- Copy-paste with variable renaming

**Expected detections**: Similarity warnings for duplicate code

### Architecture/*.hs
Demonstrates architecture analysis:
- **Core.hs**: Business logic that incorrectly depends on DB and UI layers
- **DB.hs**: Database layer with duplicated types
- **UI.hs**: UI layer with embedded business logic

Layer violations detected:
- Core imports DB (should be abstracted)
- Core imports UI (should be abstracted)

**Expected detections**: Layer violation warnings

### Lib.hs, Utils.hs
Demonstrates unused code detection:
- `unusedExportedFunction` - exported but never used
- `unusedInternalFunction` - not exported and not used
- `helperUnused` - exported but never imported
- `AnotherUnused` - exported type never used
- `privateUnused` - internal unused function

**Expected detections**: Unused code warnings

### AutofixTest.hs
Demonstrates auto-fix capabilities:
- Performance patterns with fixes
- Redundant patterns with fixes
- Modernization suggestions with fixes
- Space leak patterns with fixes

## Issue Categories

| Category | Description | Example |
|----------|-------------|---------|
| `naming/type` | Type naming conventions | `String` should be `Text` |
| `naming/variable` | Variable naming conventions | `u` should be `user` |
| `performance/string` | String efficiency | Use `Text` for text processing |
| `performance/io` | IO efficiency | Line-buffered putStrLn |
| `space-leak/lazy-data` | Lazy record fields | Add `StrictData` pragma |
| `space-leak/lazy-io` | Lazy IO | Use strict ByteString/Text IO |
| `space-leak/lazy-accumulator` | Non-strict accumulators | Use BangPatterns |
| `space-leaks/lazy-state` | Lazy State monad | Use Strict variant |
| `security/unsafe-io` | Unsafe IO operations | `unsafePerformIO` |
| `security/trace` | Debug code | Remove `trace` calls |
| `partial/head` | Partial functions | Use `headMay` |
| `partial/fromJust` | Partial functions | Pattern match instead |
| `imports/unused` | Unused imports | Remove unused imports |
| `extension/missing` | Missing pragmas | Add needed extensions |
| `extension/unused` | Unused pragmas | Remove unused extensions |
| `modernize/monoid` | Code modernization | Use `foldMap` |
| `complexity/cyclomatic` | Function complexity | Refactor complex functions |

## Configuration

The `argus.toml` file configures all analysis rules. Key sections:

- `[general]` - Directories, exclusions, analysis mode
- `[unused]` - Dead code detection settings
- `[naming]` - Naming convention rules
- `[patterns]` - Pattern-based rules with auto-fixes
- `[imports]` - Import analysis settings
- `[complexity]` - Complexity thresholds
- `[fix]` - Auto-fix settings
- `[resource]` - Timeout and resource limits

## Output Formats

Argus supports multiple output formats:
- `terminal` - Colored terminal output (default)
- `json` - Machine-readable JSON
- `sarif` - SARIF format for CI/CD integration
- `html` - HTML report
- `junit` - JUnit XML for test frameworks
- `codeclimate` - Code Climate format
- `checkstyle` - Checkstyle XML format
