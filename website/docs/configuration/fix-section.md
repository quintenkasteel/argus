---
sidebar_position: 6
title: Fix Configuration
description: Configure auto-fix behavior, safety, and imports
---

# Fix Configuration

The `[fix]` section configures how Argus applies automatic fixes, including safety levels, backups, and import management.

## Basic Configuration

```toml
[fix]
# Enable auto-fix functionality
enabled = true

# Create backup files before fixing
backup = true

# Default safety level
safety = "mostly-safe"

# Validate files compile after fixing
validate = true
```

## Safety Levels

Argus categorizes fixes by safety:

| Level | Description | Default Behavior |
|-------|-------------|-----------------|
| `safe` | Always preserves semantics | Applied |
| `mostly-safe` | Safe in most cases | Applied |
| `needs-review` | May change semantics | Skipped |
| `manual` | No auto-fix available | Never applied |

```toml
[fix]
# Which safety levels to apply
safety = "mostly-safe"  # Apply safe and mostly-safe

# Or be more conservative
safety = "safe"  # Only apply guaranteed-safe fixes

# Or more aggressive (requires --all flag)
safety = "needs-review"
```

### Per-Rule Safety Override

```toml
[fix.safety]
# Override safety classification
"partial/head" = "safe"      # Consider this fix always safe
"modernize/return-pure" = "needs-review"  # Be more cautious
```

## Backup Configuration

```toml
[fix]
# Create backups
backup = true

# Backup directory (default: same as source with .bak suffix)
backup-dir = ".argus-backups"

# Backup naming: suffix, directory, timestamp
backup-style = "suffix"

# Keep N backups per file
backup-count = 3

# Clean old backups after N days
backup-retention = 7
```

### Backup Styles

```toml
# Suffix: src/File.hs.bak
backup-style = "suffix"

# Directory: .backups/src/File.hs
backup-style = "directory"

# Timestamp: src/File.hs.20240115-143200.bak
backup-style = "timestamp"
```

## Validation

```toml
[fix]
# Validate fixes compile
validate = true

# Validation timeout in seconds
validate-timeout = 60

# Validation command (default: use stack/cabal)
# validate-command = "stack build --fast"

# Rollback on validation failure
rollback-on-failure = true
```

### Validation Behavior

When validation is enabled:

1. Apply fix to file
2. Run type checker on file
3. If fails, rollback and report error
4. If succeeds, keep fix

## Import Management

```toml
[fix]
# Automatically add required imports
add-imports = true

# Remove imports made unused by fixes
remove-unused-imports = false

# Organize imports after fixing
organize-imports = false
```

### Import Style

```toml
[fix.imports]
# Prefer qualified imports
prefer-qualified = true

# Default qualifications
[fix.imports.as]
"Data.Map" = "Map"
"Data.Map.Strict" = "Map"
"Data.Set" = "Set"
"Data.Text" = "T"
"Data.Text.Lazy" = "TL"
"Data.ByteString" = "BS"
"Data.ByteString.Lazy" = "BSL"

# Import grouping
group-imports = true
import-groups = ["Prelude", "base", "external", "project"]
```

### Import Placement

```toml
[fix.imports]
# Where to add new imports
placement = "alphabetical"  # sorted position
# placement = "end"         # at end of imports
# placement = "start"       # at start of imports

# Separate import groups with blank line
separate-groups = true
```

## Rule-Specific Fix Settings

### Skip Fixes for Certain Rules

```toml
[fix]
# Don't auto-fix these rules
skip-rules = [
  "modernize/return-pure",   # Team prefers return
  "redundant/dollar-simple", # Style preference
]
```

### Per-Rule Import Additions

```toml
[fix.imports.for-rule]
"partial/head" = { module = "Safe", names = ["headMay"] }
"partial/tail" = { module = "Safe", names = ["tailMay"] }
"partial/last" = { module = "Safe", names = ["lastMay"] }
```

## Fix Ordering

```toml
[fix]
# Order to apply fixes
# reverse: Apply from bottom of file to top (default)
# forward: Apply from top to bottom
# by-rule: Group fixes by rule, then apply
order = "reverse"
```

### Why Reverse Order?

Applying fixes from bottom to top prevents earlier fixes from invalidating the line/column positions of later fixes.

## Interactive Mode Settings

```toml
[fix.interactive]
# Default choice: yes, no, all, quit
default = "yes"

# Show diff in interactive mode
show-diff = true

# Context lines in diff
context-lines = 3
```

## Batch Processing

```toml
[fix.batch]
# Maximum files to fix in one run
max-files = 100

# Stop after N errors
max-errors = 10

# Parallel fix application
parallel = false  # Sequential is safer
```

## Conflict Resolution

When fixes conflict (overlapping source ranges):

```toml
[fix.conflicts]
# Resolution strategy
# first: Apply first fix, skip conflicts
# ask: Ask user in interactive mode
# skip: Skip all conflicting fixes
strategy = "first"
```

## Dry Run and Preview

```toml
[fix]
# Always preview before applying
preview = false

# Show unified diff in preview
diff-format = "unified"
```

## Complete Example

```toml
[fix]
# Basic settings
enabled = true
backup = true
validate = true

# Safety
safety = "mostly-safe"

[fix.safety]
"partial/head" = "safe"

# Backup settings
backup-dir = ".argus-backups"
backup-style = "directory"
backup-count = 3

# Validation
validate-timeout = 60
rollback-on-failure = true

# Import management
add-imports = true
remove-unused-imports = false
organize-imports = false

[fix.imports]
prefer-qualified = true
placement = "alphabetical"
separate-groups = true

[fix.imports.as]
"Data.Map" = "Map"
"Data.Set" = "Set"
"Data.Text" = "T"

[fix.imports.for-rule]
"partial/head" = { module = "Safe", names = ["headMay"] }
"partial/tail" = { module = "Safe", names = ["tailMay"] }

# Skip certain rules
skip-rules = [
  "modernize/return-pure",
]

# Fix ordering
order = "reverse"

# Interactive settings
[fix.interactive]
show-diff = true
context-lines = 3

# Batch settings
[fix.batch]
max-files = 100
max-errors = 10
parallel = false

# Conflict resolution
[fix.conflicts]
strategy = "first"
```

## Command-Line Overrides

```bash
# Preview mode
argus fix --preview src/

# Safety levels
argus fix --safe-only src/
argus fix --all src/  # Include needs-review

# Backup control
argus fix --no-backup src/
argus fix --backup-dir ./backups src/

# Validation
argus fix --validate src/
argus fix --no-validate src/

# Import management
argus fix --add-imports src/
argus fix --organize-imports src/

# Interactive
argus fix -i src/
```

## Per-Scope Settings

```toml
# Relaxed fixing in tests
[[scopes]]
modules = ["*Spec", "Test.*"]
[scopes.fix]
safety = "needs-review"  # Allow more aggressive fixes
validate = false         # Skip validation

# Conservative in production code
[[scopes]]
modules = ["*.API.*", "*.Core.*"]
[scopes.fix]
safety = "safe"          # Only guaranteed-safe fixes
validate = true          # Always validate
```

## See Also

- **[argus fix](../cli-reference/fix)**: Fix command reference
- **[Auto-Fix Guide](../usage-guide/auto-fix)**: Using auto-fix
- **[Rules Configuration](./rules-section)**: Rule settings
- **[Custom Rules](../rules/custom-rules)**: Defining fix patterns
