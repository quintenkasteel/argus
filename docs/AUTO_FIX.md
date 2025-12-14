# Argus Auto-Fix System

This document describes the auto-fix capabilities, guarantees, and limitations of the Argus static analyzer.

## Overview

The Argus auto-fix system consists of 28 modules (~16,000 lines) providing:

- 7 fix engine types (Boolean, List, Partial, Monad, Semantic, Learning, Template)
- 60+ individual fixes across multiple domains
- Conflict detection and dependency ordering
- Configurable validation levels
- Transaction semantics with rollback

## Fix Engine Architecture

### FixEngine Typeclass

All fix engines implement:

```haskell
class FixEngine engine where
  engineName :: engine -> Text
  engineVersion :: engine -> Text
  engineCategories :: engine -> [EngineCategory engine]
  findFixes :: engine -> FilePath -> Text -> IO [EnrichedFix]
  validateFix :: engine -> EnrichedFix -> Text -> IO FixValidation
  applyFix :: engine -> FilePath -> Text -> EnrichedFix -> IO FixApplicationResult
  applyFixes :: engine -> FilePath -> Text -> [EnrichedFix] -> IO (Text, [FixApplicationResult])
  canHandle :: engine -> Diagnostic -> Bool
```

### Implemented Engines

| Engine | Purpose |
|--------|---------|
| BooleanFixEngine | Boolean simplification (20+ rules) |
| ListFixEngine | List operation improvements (15+ rules) |
| PartialFixEngine | Partial function replacements (10+ rules) |
| MonadFixEngine | Monadic pattern fixes (10+ rules) |
| SemanticFixEngine | Type-aware fixes using HIE |
| LearningEngine | Pattern discovery |
| TemplateEngine | Template-based generation |

## Validation Levels

| Level | Description | Guarantees | Performance |
|-------|-------------|------------|-------------|
| `none` | No validation | None | Fastest |
| `structural` | Bracket balance only | Basic structural | Fast |
| `syntax` | Full GHC parser | Code parses | Moderate |
| `semantic` | Parse + type check | Code compiles | Slowest |

**Default: `syntax`** - ensures code parses but does NOT type-check.

### Using Semantic Validation

```bash
argus fix src/ --validate-level semantic
```

Semantic validation requires HIE files and is significantly slower but guarantees type correctness.

## Conflict Detection

### Conflict Types

| Type | Description |
|------|-------------|
| `OverlappingSpan` | Fixes modify overlapping regions |
| `SameLocation` | Same exact location |
| `SemanticConflict` | Opposite changes |
| `ResourceConflict` | Named resource conflict |

### Resolution Strategies

| Strategy | Behavior |
|----------|----------|
| `skip` | Skip all conflicts |
| `preferred` | Prefer "safe" fixes (default) |
| `first` | Apply first, skip others |
| `severity` | Prefer higher severity |
| `smaller` | Prefer smaller changes |

```bash
argus fix src/ --conflict-strategy severity
```

## Dependency Ordering

Fixes are applied in topologically sorted order:

1. Build dependency graph from fix relationships
2. Detect circular dependencies
3. Sort fixes to respect `MustApplyBefore`, `MustApplyAfter` constraints
4. Apply in resolved order

Circular dependencies are detected but NOT automatically resolved - those fixes fail.

## Transaction Behavior

### With Transactions (default)

```bash
argus fix src/  # --no-transactional NOT specified
```

1. Read all target files into memory
2. Apply fixes to in-memory copies
3. Validate after each fix
4. On validation failure: discard ALL changes
5. On success: write files and create backups

### Without Transactions

```bash
argus fix src/ --no-transactional
```

Fixes applied and written immediately. Partial application possible on failure.

### Backup Behavior

With `--no-backup` NOT specified (default):
- Backup files created with `.argus-backup` suffix
- Backups written before modified file
- Manual cleanup required

## Safety Levels

| Level | Description | Auto-apply? |
|-------|-------------|-------------|
| `Safe` | Guaranteed correct | Yes (default) |
| `Mostly` | Generally safe with exceptions | No |
| `Unsafe` | Requires manual review | No |
| `Unknown` | Unknown safety | No |

```bash
argus fix src/              # Safe only (default)
argus fix src/ --unsafe     # Include unsafe fixes
```

## Individual Fixes

### Boolean Fixes

- `not True` → `False`
- `not False` → `True`
- `True && x` → `x`
- `False && x` → `False`
- `x && True` → `x`
- `x && False` → `False`
- `True || x` → `True`
- `False || x` → `x`
- `x && x` → `x`
- `x || x` → `x`
- De Morgan's laws
- `if True then x else y` → `x`
- `x == True` → `x`
- `x == False` → `not x`

### List Fixes

- `length xs == 0` → `null xs`
- `length xs > 0` → `not (null xs)`
- `nub xs` → `ordNub xs` (when Ord available)
- `concat (map f xs)` → `concatMap f xs`
- `reverse (reverse xs)` → `xs`
- `sort (sort xs)` → `sort xs`
- `head xs` → `listToMaybe xs`
- `foldr f z xs` → `foldl' f z xs` (when appropriate)

### Partial Function Fixes

- `head xs` → `headMay xs`
- `tail xs` → `tailMay xs`
- `init xs` → `initMay xs`
- `last xs` → `lastMay xs`
- `xs !! i` → `atMay xs i`
- `fromJust x` → explicit pattern match
- `read str` → `readMaybe str`
- `maximum xs` → `maximumMay xs`
- `minimum xs` → `minimumMay xs`

### Monadic Fixes

- `return x >>= f` → `f x`
- `m >>= return` → `m`
- `liftM f m` → `fmap f m`
- `liftM2 f m1 m2` → `f <$> m1 <*> m2`
- do-notation simplifications

## Guarantees and Limitations

### What IS Guaranteed

1. **Syntactic Correctness** (with `syntax` validation): Fixed code parses with GHC parser
2. **Conflict Detection**: Overlapping spans are identified and reported
3. **Dependency Ordering**: Fixes applied in topologically sorted order
4. **Rollback on Failure**: With transactions, validation failures discard all changes
5. **Backup Creation**: Original files backed up before modification

### What is NOT Guaranteed

1. **Semantic Correctness**: Default validation does NOT type-check. Code may fail to compile.
2. **Type Preservation**: Types may change. Use `--validate-level semantic` for type safety.
3. **OS-Level Atomicity**: File writes are sequential I/O, not atomic. Crash during write = potential corruption.
4. **Circular Dependency Resolution**: Cycles are detected but not resolved.
5. **Parallel Execution**: Pipeline parallelism is simulated, runs sequentially.

## Configuration

### Via CLI

```bash
argus fix src/ \
  --validate-level semantic \
  --conflict-strategy preferred \
  --no-backup \
  --verbose-fix \
  --dry-run
```

### Via TOML

```toml
[fix]
enabled = true
safe-only = true
backup = true

[fix.auto-imports]
enabled = true
add-missing = true
remove-unused = true
organize = false
```

## Pipeline System

Fixes pass through configurable pipeline stages:

| Stage Type | Purpose |
|------------|---------|
| FilterStage | Filter fixes by condition |
| TransformStage | Transform individual fixes |
| ValidateStage | Validate fixes against content |
| SortStage | Order fixes |
| LimitStage | Limit number of fixes |
| DeduplicateStage | Remove duplicate fixes |

### Common Filters

- `filterByConfidence`: Minimum confidence threshold
- `filterByCategory`: Include only specific categories
- `filterBySafety`: Minimum safety level
- `filterNonConflicting`: Exclude conflicting fixes

### Common Sorters

- `sortByConfidence`: Highest confidence first
- `sortBySafety`: Safest first
- `sortBySpan`: By source position
- `sortByPriority`: By priority tags

## Best Practices

1. **Always preview first**: Use `--dry-run` before applying fixes
2. **Start with safe-only**: Default behavior, most conservative
3. **Enable semantic validation for critical code**: `--validate-level semantic`
4. **Keep backups**: Don't use `--no-backup` on important code
5. **Use interactive mode for review**: `--interactive` shows each fix with diff
6. **Test after fixing**: Run your test suite after auto-fix
