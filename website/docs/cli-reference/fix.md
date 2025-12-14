---
sidebar_position: 3
title: argus fix
description: Automatically fix detected issues in Haskell code
---

# argus fix

Automatically apply fixes for detected issues in Haskell source files.

## Synopsis

```bash
argus fix [OPTIONS] [FILES|DIRECTORIES...]
```

## Description

The `fix` command analyzes Haskell source files and automatically applies fixes for issues that have auto-fix support. Argus uses exact-print technology to preserve formatting, comments, and whitespace.

## Arguments

| Argument | Description |
|----------|-------------|
| `FILES` | Specific `.hs` files to fix |
| `DIRECTORIES` | Directories to scan recursively for `.hs` files |

## Options

### Fix Mode

| Option | Description |
|--------|-------------|
| `--preview` | Show fixes without applying them |
| `--dry-run` | Alias for `--preview` |
| `-i, --interactive` | Confirm each fix interactively |
| `--safe-only` | Apply only fixes marked as "safe" |
| `--all` | Apply all fixes including "needs-review" |

### Fix Selection

| Option | Description |
|--------|-------------|
| `--rules RULES` | Only fix issues from specified rules |
| `--categories CATS` | Only fix issues from categories |
| `--exclude-rules RULES` | Skip fixes for specific rules |
| `--only-safety LEVEL` | Only apply fixes at safety level or safer |

### Backup and Safety

| Option | Description |
|--------|-------------|
| `--backup` | Create `.bak` backup files (default: enabled) |
| `--no-backup` | Disable backup file creation |
| `--backup-dir DIR` | Store backups in specified directory |
| `--validate` | Validate fixes compile after applying |
| `--no-validate` | Skip post-fix validation |

### Import Management

| Option | Description |
|--------|-------------|
| `--add-imports` | Add imports required by fixes (default) |
| `--no-add-imports` | Don't modify imports |
| `--remove-unused-imports` | Remove imports made unused by fixes |
| `--organize-imports` | Reorganize imports after fixing |

### Output

| Option | Description |
|--------|-------------|
| `--diff` | Show unified diff of changes |
| `--stats` | Show fix statistics |
| `-v, --verbose` | Verbose output showing each fix |
| `-q, --quiet` | Only show errors |

### File Filtering

Same options as `argus check`:

| Option | Description |
|--------|-------------|
| `--include GLOB` | Only fix files matching pattern |
| `--exclude GLOB` | Skip files matching pattern |
| `--exclude-dir DIR` | Skip directories |

## Safety Levels

Argus categorizes fixes by safety:

| Level | Description | Applied with |
|-------|-------------|--------------|
| `safe` | Always preserves semantics | `--safe-only`, default |
| `mostly-safe` | Safe in most cases | default |
| `needs-review` | May change semantics | `--all` only |
| `manual` | No auto-fix available | never |

## Examples

### Basic Usage

```bash
# Preview all available fixes
argus fix --preview src/

# Apply safe fixes
argus fix src/

# Apply all fixes including needs-review
argus fix --all src/

# Interactive mode
argus fix -i src/
```

### Preview Mode

```bash
# See what would be changed
argus fix --preview src/

# With diff output
argus fix --preview --diff src/

# Preview specific rule fixes
argus fix --preview --rules "partial/*" src/
```

### Selective Fixing

```bash
# Only partial function fixes
argus fix --rules "partial/*" src/

# Only safe fixes
argus fix --safe-only src/

# Multiple categories
argus fix --categories "partial,redundant" src/

# Exclude specific rules
argus fix --exclude-rules "modernize/return-pure" src/
```

### Interactive Mode

```bash
# Confirm each fix
argus fix -i src/
```

Interactive prompt:

```
src/Api/Handler.hs:45:12
  Issue: Use headMay instead of head
  Fix: Replace `head items` with `headMay items`

  Apply this fix? [y/n/a/q/?]
```

Options:
- `y` - Apply this fix
- `n` - Skip this fix
- `a` - Apply all remaining fixes
- `q` - Quit without more fixes
- `?` - Show help

### Backup Management

```bash
# With backups (default)
argus fix src/
# Creates src/File.hs.bak for each modified file

# Without backups
argus fix --no-backup src/

# Backup to specific directory
argus fix --backup-dir .backups/ src/
```

### Import Management

```bash
# Add required imports automatically
argus fix --add-imports src/

# Don't touch imports
argus fix --no-add-imports src/

# Also remove unused imports
argus fix --remove-unused-imports src/

# Full import organization
argus fix --organize-imports src/
```

### Validation

```bash
# Validate each file compiles after fix
argus fix --validate src/

# Skip validation (faster)
argus fix --no-validate src/
```

### Diff Output

```bash
# Show diff of changes
argus fix --diff src/
```

Output:

```diff
--- src/Api/Handler.hs
+++ src/Api/Handler.hs
@@ -1,4 +1,5 @@
 module Api.Handler where

+import Safe (headMay)
 import Data.List

@@ -45,7 +46,7 @@
 getFirst :: [Item] -> Maybe Item
 getFirst items =
-  let first = head items
+  let first = headMay items
   in first
```

## Fix Examples

### Partial Functions

```haskell
-- Before
first = head items

-- After (with import added)
import Safe (headMay)
first = headMay items
```

### Redundant Code

```haskell
-- Before
result = id value

-- After
result = value
```

### Modernization

```haskell
-- Before
doubled <- liftM (*2) getValue

-- After
doubled <- fmap (*2) getValue
```

### Performance

```haskell
-- Before
isEmpty = length xs == 0

-- After
isEmpty = null xs
```

## Transactional Fixes

Argus applies fixes transactionally:

1. Parse and validate source file
2. Calculate all fixes
3. Apply fixes in reverse source order
4. Validate result compiles (if `--validate`)
5. Write file or rollback on error

If validation fails:

```
Error: Fix validation failed for src/Handler.hs
  Applied fixes caused compilation error:
    Variable not in scope: headMay

  Rolled back changes. Consider:
    - Using --add-imports to add required imports
    - Applying fix manually
```

## Statistics

```bash
argus fix --stats src/
```

Output:

```
Argus Fix Report
================

Files scanned:    156
Files modified:    23
Fixes applied:     47
Fixes skipped:     12 (needs-review)

By Category:
  partial:     15
  redundant:   20
  modernize:   12

By Safety:
  safe:         35
  mostly-safe:  12
  needs-review: 12 (skipped)

Imports added:     8
Imports removed:   0

Backup files: 23 (in .bak files)
```

## Configuration

Configure fix behavior in `argus.toml`:

```toml
[fix]
# Default safety level
safety = "mostly-safe"

# Create backups
backup = true
backup-dir = ".argus-backups"

# Auto-add imports
add-imports = true

# Validate after fix
validate = true

# Rules to skip for auto-fix
skip-rules = [
  "modernize/return-pure",  # Team prefers return
]

[fix.imports]
# Qualify certain modules
qualified = ["Data.Map", "Data.Set"]

# Preferred qualifications
[fix.imports.as]
"Data.Map" = "Map"
"Data.Text" = "T"
```

## Batch Fixing

For large codebases:

```bash
# Fix in batches with progress
argus fix --stats --verbose src/

# Fix incrementally (changed files only)
argus fix --incremental src/

# Fix specific category first
argus fix --categories partial src/
argus fix --categories redundant src/
argus fix --categories modernize src/
```

## Undo Fixes

Using backup files:

```bash
# Restore from .bak files
for f in src/**/*.hs.bak; do mv "$f" "${f%.bak}"; done
```

Using git:

```bash
# Discard fix changes
git checkout -- src/

# Or reset specific file
git checkout -- src/Api/Handler.hs
```

## Exit Codes

| Code | Condition |
|------|-----------|
| `0` | Fixes applied successfully (or none needed) |
| `1` | Some fixes failed or were skipped |
| `2` | Configuration error |
| `3` | File not found |
| `4` | Parse error |
| `5` | Validation failed |

## See Also

- **[argus check](./check)**: Analyze without fixing
- **[Auto-Fix Guide](../usage-guide/auto-fix)**: Detailed fix documentation
- **[Rules Overview](../rules/overview)**: Rules with auto-fix support
- **[Configuration](../configuration/fix-section)**: Fix configuration options
