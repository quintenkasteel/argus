---
sidebar_position: 2
title: Auto-Fix System
description: Safe, transactional automatic code fixes
---

# Auto-Fix System

Argus provides an enterprise-grade auto-fix system that safely repairs issues in your codebase. This guide covers how to use it effectively.

## Overview

The auto-fix system is built on several core principles:

1. **Safety First**: Never leave code in a broken state
2. **Transparency**: Always show what will change
3. **Control**: You decide what gets applied
4. **Validation**: Verify fixes don't break syntax

## Basic Usage

### Preview Mode

Always preview changes before applying:

```bash
argus fix src/ --preview
```

Output:

```diff
--- src/MyModule.hs
+++ src/MyModule.hs
@@ -42,1 +42,1 @@
-let first = head items
+let first = headMay items

2 fixes available (1 safe, 1 needs review)
Use --safe-only to apply safe fixes, or --all for all fixes
```

### Applying Fixes

```bash
# Apply only safe, non-breaking fixes (recommended)
argus fix src/ --safe-only

# Apply all available fixes
argus fix src/ --all

# Dry run - show what would happen without applying
argus fix src/ --dry-run
```

### Interactive Mode

Review and approve each fix individually:

```bash
argus fix src/ --interactive
```

```
Fix 1/5: src/MyModule.hs:42:5
Rule: partial/head
Replace 'head' with 'headMay'

-let first = head items
+let first = headMay items

[y] Apply  [n] Skip  [a] Apply all  [s] Skip all  [q] Quit
> y

✓ Applied fix 1/5

Fix 2/5: src/Performance.hs:15:1
...
```

## Safety Levels

Every fix has a safety level indicating how confident Argus is that the fix won't change semantics:

| Level | Description | `--safe-only` | Example |
|-------|-------------|---------------|---------|
| `safe` | Always safe to apply | Included | `length xs == 0` → `null xs` |
| `mostly-safe` | Safe in typical cases | Included | `foldl` → `foldl'` |
| `needs-review` | May need human review | Excluded | Removing unused import |
| `unsafe` | May change semantics | Excluded | `String` → `Text` |

### Viewing Safety Levels

```bash
argus fix src/ --preview --show-safety
```

```diff
--- src/MyModule.hs [SAFE]
+++ src/MyModule.hs
@@ -42,1 +42,1 @@
-let first = head items
+let first = headMay items

--- src/Types.hs [NEEDS-REVIEW]
+++ src/Types.hs
@@ -10,1 +10,1 @@
-type Name = String
+type Name = Text
```

## Conflict Resolution

When multiple fixes affect the same code region, Argus detects and resolves conflicts.

### Conflict Detection

```bash
argus fix src/ --preview
```

```
Conflict detected at src/MyModule.hs:42-45:
  Fix 1: Replace 'head' with 'headMay'
  Fix 2: Replace 'tail' with 'tailMay'

Both fixes affect overlapping regions.
Resolution: Applying Fix 1 only (higher priority)
```

### Resolution Strategies

Configure conflict resolution in `argus.toml`:

```toml
[fix]
# How to resolve conflicts
conflict-strategy = "prefer-severity"  # Options below
```

| Strategy | Behavior |
|----------|----------|
| `skip-all` | Skip all conflicting fixes |
| `skip-second` | Keep first, skip second |
| `prefer-preferred` | Keep explicitly preferred fixes |
| `prefer-smaller` | Keep more targeted fixes |
| `prefer-severity` | Keep higher-severity fixes |
| `interactive` | Ask user to choose |

## Transactional Semantics

Argus ensures your code is never left in a broken state:

### All-or-Nothing

Each file is fixed atomically:

```bash
argus fix src/MyModule.hs
```

If any fix in a file fails validation, all fixes to that file are rolled back.

### Backup Files

```bash
# Enable backups (default: on)
argus fix src/ --backup

# Disable backups
argus fix src/ --no-backup
```

Backups are created as `.argus-backup` files:

```
src/MyModule.hs
src/MyModule.hs.argus-backup  # Original content
```

### Validation Pipeline

After applying fixes, Argus:

1. **Re-parses** the modified file
2. **Verifies** syntax is correct
3. **Checks** that the fix was applied correctly
4. **Rolls back** if any check fails

```bash
# Skip validation (not recommended)
argus fix src/ --no-validate
```

## Import Management

Many fixes require adding or modifying imports. Argus handles this automatically.

### Auto-Import

```toml
[fix.auto-imports]
enabled = true
add-missing = true
remove-unused = true
```

Example:

```haskell
-- Before
let first = head items

-- After (with auto-import)
import Safe (headMay)

let first = headMay items
```

### Qualified Imports

```toml
[fix.auto-imports]
qualify-new = true
```

```haskell
-- Before
let first = head items

-- After (qualified)
import qualified Safe

let first = Safe.headMay items
```

### Import Organization

```toml
[fix.auto-imports]
organize = true
group-by-category = true
```

Groups imports by:
1. Prelude and base
2. External packages
3. Internal modules

## Multi-File Fixes

Some fixes span multiple files (e.g., renaming exports):

```bash
argus fix src/ --multi-file
```

Multi-file fixes are applied atomically—if any file fails, all changes are rolled back.

## Fix Categories

Fixes are organized by category:

```bash
# Apply only specific categories
argus fix src/ --categories performance,safety

# Exclude categories
argus fix src/ --exclude-categories modernize
```

| Category | Description | Example |
|----------|-------------|---------|
| `performance` | Performance improvements | `length == 0` → `null` |
| `safety` | Replace partial functions | `head` → `headMay` |
| `modernize` | Update to modern APIs | `return` → `pure` |
| `redundant` | Remove redundant code | `id x` → `x` |
| `imports` | Clean up imports | Remove unused |
| `space-leaks` | Fix lazy evaluation issues | `foldl` → `foldl'` |

## Selective Fixing

### By Rule

```bash
# Fix only specific rules
argus fix src/ --rules partial/head,partial/tail

# Exclude specific rules
argus fix src/ --exclude-rules modernize/return-pure
```

### By File

```bash
# Fix specific files
argus fix src/MyModule.hs src/Utils.hs

# Exclude files
argus fix src/ --exclude "Test/**"
```

### By Location

```bash
# Fix within line range
argus fix src/MyModule.hs --lines 10-50
```

## Reporting

### Fix Report

```bash
argus fix src/ --safe-only --report fixes.json
```

```json
{
  "applied": [
    {
      "file": "src/MyModule.hs",
      "line": 42,
      "rule": "partial/head",
      "safety": "safe",
      "before": "head items",
      "after": "headMay items"
    }
  ],
  "skipped": [...],
  "failed": [...],
  "summary": {
    "applied": 15,
    "skipped": 3,
    "failed": 0
  }
}
```

### Diff Output

```bash
# Generate unified diff
argus fix src/ --preview --format diff > changes.patch

# Apply later with standard tools
patch -p1 < changes.patch
```

## Best Practices

### 1. Always Preview First

```bash
argus fix src/ --preview
```

### 2. Start with Safe Fixes

```bash
argus fix src/ --safe-only
```

### 3. Use Interactive for Uncertain Fixes

```bash
argus fix src/ --interactive
```

### 4. Keep Backups

```bash
argus fix src/ --backup
```

### 5. Run Tests After Fixing

```bash
argus fix src/ --safe-only && stack test
```

### 6. Commit Frequently

```bash
argus fix src/ --safe-only
git add -p  # Review changes
git commit -m "Apply Argus safe fixes"
```

## Troubleshooting

### Fix Failed Validation

```
Error: Fix failed validation at src/MyModule.hs:42
Syntax error after applying fix
Rolling back...
```

This usually means the fix pattern didn't account for all syntax variants. Report as a bug.

### Import Conflicts

```
Warning: Import conflict - Safe.headMay vs Safe.Partial.headMay
Using: Safe.headMay
```

Configure preferred imports:

```toml
[fix.auto-imports]
preferred = [
  { name = "headMay", module = "Safe" }
]
```

### Too Many Changes

For large fixes, break them up:

```bash
# Fix one category at a time
argus fix src/ --categories safety
git commit -m "Apply safety fixes"

argus fix src/ --categories performance
git commit -m "Apply performance fixes"
```

## Next Steps

- **[CI Integration](./ci-integration)**: Automate fixes in your pipeline
- **[Custom Rules](../rules/custom-rules)**: Write rules with fixes
- **[Configuration Reference](../configuration/fix-section)**: Full fix configuration
