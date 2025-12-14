# Argus Warnings Policy

This document defines the warning policy for the Argus codebase, including which warnings are enabled, how they are enforced, and how to handle exceptions.

## Table of Contents

1. [Warning Groups](#warning-groups)
2. [Enforcement Strategy](#enforcement-strategy)
3. [Exception Handling](#exception-handling)
4. [Developer Workflow](#developer-workflow)
5. [CI Configuration](#ci-configuration)

---

## Warning Groups

### Tier 1: Errors (Always Enforced)

These warnings are **errors** in CI and must be fixed before merging.

| Flag | Description | Rationale |
|------|-------------|-----------|
| `-Wunused-imports` | Unused import declarations | Dead code, slower compilation |
| `-Wunused-top-binds` | Unused top-level bindings | Dead code |
| `-Wincomplete-patterns` | Non-exhaustive patterns | Runtime crashes |
| `-Wincomplete-uni-patterns` | Incomplete unification patterns | Runtime crashes |
| `-Wincomplete-record-updates` | Incomplete record updates | Partial updates |
| `-Wmissing-deriving-strategies` | Missing deriving strategy | Ambiguous behavior |
| `-Wpartial-fields` | Partial record selectors | Runtime crashes |
| `-Wunused-packages` | Unused package dependencies | Build bloat |

### Tier 2: Warnings (Should Fix)

These warnings should be fixed but don't block CI in exceptional cases.

| Flag | Description | Rationale |
|------|-------------|-----------|
| `-Wunused-matches` | Unused pattern bindings | Code clarity |
| `-Wunused-local-binds` | Unused local bindings | Dead code |
| `-Wname-shadowing` | Variable shadowing | Confusion, bugs |
| `-Wredundant-constraints` | Unnecessary type constraints | API clarity |
| `-Worphans` | Orphan instances | Compilation issues |
| `-Wdeprecations` | Use of deprecated features | Future compatibility |

### Tier 3: Advisory (Informational)

These are enabled but don't require immediate action.

| Flag | Description | Rationale |
|------|-------------|-----------|
| `-Wcompat` | Future GHC compatibility | Proactive fixes |
| `-Widentities` | Unnecessary identity ops | Minor optimization |
| `-Wredundant-bang-patterns` | Unnecessary strictness | Code clarity |

---

## Enforcement Strategy

### Local Development

Developers should run with warnings enabled:

```bash
# Standard development build (warnings shown)
make dev

# Strict build (warnings as errors)
make build-strict

# Pre-commit check
make check-warnings
```

### package.yaml Configuration

```yaml
ghc-options:
  # Core warnings (always enabled)
  - -Wall
  - -Wcompat
  - -Widentities
  - -Wincomplete-record-updates
  - -Wincomplete-uni-patterns
  - -Wmissing-deriving-strategies
  - -Wpartial-fields
  - -Wredundant-constraints
  - -Wunused-packages

  # Additional strict warnings
  - -Wunused-top-binds
  - -Wunused-local-binds
  - -Wunused-matches
  - -Wname-shadowing

  # HIE support
  - -fwrite-ide-info
  - -hiedir=.hie

# For CI/strict builds, add via flag or command line:
# -Werror
```

### CI Pipeline

```yaml
# CI builds use -Werror via --pedantic
- name: Build with strict warnings
  run: stack build --fast --pedantic
```

The `--pedantic` flag enables `-Werror -Wunused-packages`, treating all warnings as errors.

---

## Exception Handling

### When Exceptions Are Allowed

Exceptions to the warning policy are allowed **only** in these cases:

1. **Third-party compatibility**: When interfacing with external APIs that require patterns triggering warnings
2. **Generated code**: Machine-generated code (e.g., TH splices) where warnings can't be fixed
3. **Temporary migration**: During active refactoring (must be tracked in TODO)

### How to Document Exceptions

#### Option 1: Per-Module Pragma (Preferred)

```haskell
{-# OPTIONS_GHC -Wno-unused-matches #-}
-- | Module: Argus.Generated.Types
--
-- Note: This module contains generated code.
-- Warning disabled: -Wno-unused-matches
-- Reason: TH-generated patterns include unused bindings
-- Ticket: #123
module Argus.Generated.Types where
```

#### Option 2: Inline Suppression (For Single Instances)

```haskell
-- When a specific warning can't be avoided
{-# ANN someFunction "HLint: ignore" #-}
someFunction :: Int -> Int
someFunction x = x  -- Warning suppressed for specific reason
```

### Exception Registry

All exceptions must be documented in `WARNINGS_EXCEPTIONS.md`:

```markdown
## Warning Exceptions

| Module | Warning | Reason | Ticket |
|--------|---------|--------|--------|
| Argus.Plugin | -Worphans | GHC plugin API requires orphans | #45 |
```

### Review Process

1. Exceptions require code review approval
2. Exceptions must have a tracking ticket
3. Exceptions are reviewed quarterly for removal

---

## Developer Workflow

### Before Committing

```bash
# 1. Run quick build to check for warnings
make dev

# 2. If warnings exist, fix them
# ... make fixes ...

# 3. Run strict build to verify
make build-strict

# 4. Run tests
make test
```

### Pre-Commit Hook (Optional)

Add to `.git/hooks/pre-commit`:

```bash
#!/bin/bash
# Check for warnings before commit

echo "Checking for warnings..."
if ! stack build --fast 2>&1 | grep -q "warning:"; then
    echo "No warnings found."
    exit 0
else
    echo "Warnings found! Please fix before committing."
    stack build --fast 2>&1 | grep "warning:" | head -20
    exit 1
fi
```

### Fixing Common Warnings

#### Unused Import

```haskell
-- Before
import Data.Text (Text, pack, unpack)  -- Warning: unpack is unused

-- After
import Data.Text (Text, pack)
```

#### Unused Variable

```haskell
-- Before
processItem (Item name price) = ...  -- Warning: price is unused

-- Option 1: Prefix with underscore
processItem (Item name _price) = ...

-- Option 2: Use wildcard
processItem (Item name _) = ...
```

#### Name Shadowing

```haskell
-- Before
process :: Int -> Int
process n = let n = n + 1 in n  -- Warning: n shadows outer n

-- After
process :: Int -> Int
process n = let n' = n + 1 in n'
```

#### Missing Deriving Strategy

```haskell
-- Before
data MyType = MyType deriving (Show, Eq)  -- Warning

-- After
data MyType = MyType
  deriving stock (Show, Eq)
```

#### Incomplete Patterns

```haskell
-- Before
head' :: [a] -> a
head' (x:_) = x  -- Warning: non-exhaustive

-- After
head' :: [a] -> Maybe a
head' (x:_) = Just x
head' []    = Nothing
```

---

## CI Configuration

### GitHub Actions Workflow

```yaml
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Build with warnings as errors
        run: stack build --fast --pedantic

      - name: Fail on warnings
        run: |
          WARNINGS=$(stack build --fast 2>&1 | grep -c "warning:" || echo "0")
          if [ "$WARNINGS" -gt 0 ]; then
            echo "Found $WARNINGS warnings!"
            stack build --fast 2>&1 | grep "warning:" | head -50
            exit 1
          fi
```

### Warning Count Tracking

```yaml
      - name: Count and report warnings
        run: |
          WARNINGS=$(stack build --fast 2>&1 | grep -c "warning:" || echo "0")
          echo "## Warning Count: $WARNINGS" >> $GITHUB_STEP_SUMMARY

          if [ "$WARNINGS" -gt 0 ]; then
            echo '```' >> $GITHUB_STEP_SUMMARY
            stack build --fast 2>&1 | grep "warning:" | head -20 >> $GITHUB_STEP_SUMMARY
            echo '```' >> $GITHUB_STEP_SUMMARY
          fi
```

---

## Quick Reference

### Commands

| Command | Description |
|---------|-------------|
| `make dev` | Build with warnings (no error) |
| `make build-strict` | Build with `-Werror` |
| `make check-warnings` | Count warnings without building |
| `stack build --pedantic` | CI-equivalent strict build |

### Warning Levels

| Level | Makefile Target | CI Behavior |
|-------|-----------------|-------------|
| Development | `make dev` | Warnings shown |
| Strict | `make build-strict` | Warnings are errors |
| CI | (automatic) | Must pass with zero warnings |

### GHC Warning Flag Reference

```bash
# Show all available warning flags
ghc --show-options | grep "^-W"

# Show warnings enabled by -Wall
ghc --show-options | grep -E "^-W.*all"
```

---

## Rationale

### Why Strict Warnings?

1. **Code Quality**: Warnings often indicate bugs or code smells
2. **Maintainability**: Clean code is easier to understand and modify
3. **Performance**: Unused code slows compilation
4. **Professionalism**: Production code should be warning-free
5. **Future-proofing**: `-Wcompat` catches future breaking changes

### Why Not Disable Warnings?

1. **Hiding problems**: Disabling warnings doesn't fix issues
2. **Accumulated debt**: Ignored warnings multiply over time
3. **Lost context**: Future developers won't know why warnings were disabled
4. **Broken windows**: Accepting warnings leads to more warnings

### Balance

This policy balances strictness with practicality:

- Tier 1 warnings are non-negotiable (errors)
- Tier 2 warnings are expected to be fixed (warnings)
- Tier 3 warnings are informational (advisory)
- Documented exceptions are allowed for valid reasons

---

*Document Version: 1.0*
*Last Updated: 2025-12-14*
