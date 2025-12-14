---
sidebar_position: 2
title: Basic Usage
description: Learn the fundamental Argus commands
---

# Basic Usage

This guide covers the essential Argus commands for everyday development.

## The Check Command

The `check` command is your primary tool for static analysis:

```bash
argus check [FILES/DIRS...] [OPTIONS]
```

### Analyzing Source Files

```bash
# Analyze a directory
argus check src/

# Analyze specific files
argus check src/MyModule.hs src/Utils.hs

# Analyze multiple directories
argus check src/ app/ lib/
```

### Understanding Output

Argus produces GHC-style diagnostic output:

```
src/MyModule.hs:42:5: warning: [partial/head]
    Use of partial function 'head': Crashes on empty list.
    Consider using 'headMay' from 'Safe' instead.
    |
 42 | let first = head items
    |             ^^^^
```

Output components:
- **Location**: `src/MyModule.hs:42:5` (file:line:column)
- **Severity**: `warning` (error, warning, suggestion, or info)
- **Rule ID**: `[partial/head]` (category/rule-name)
- **Message**: Description of the issue
- **Context**: Source code with the problem highlighted

### Severity Levels

| Severity | Meaning | Exit Code |
|----------|---------|-----------|
| `error` | Critical issue that should block CI | 1 |
| `warning` | Issue that should be addressed | 0 (or 1 with `--strict`) |
| `suggestion` | Improvement opportunity | 0 |
| `info` | Informational note | 0 |

### Common Options

```bash
# Use a specific config file
argus check -c custom-config.toml src/

# Choose analysis mode
argus check --mode quick src/   # Fast, syntax-only (default)
argus check --mode full src/    # Deep, requires HIE files

# Control output format
argus check --format terminal src/  # Colored terminal (default)
argus check --format json src/      # Machine-readable JSON
argus check --format sarif src/     # GitHub Code Scanning

# Show more context
argus check --context-lines 5 src/

# Group output by rule instead of file
argus check --group-by rule src/

# Exclude patterns
argus check -e "Test/**" -e "*.gen.hs" src/

# Verbose output
argus check -v src/
```

## The Fix Command

The `fix` command automatically repairs issues:

```bash
argus fix [FILES/DIRS...] [OPTIONS]
```

### Preview Mode

Always preview changes before applying:

```bash
argus fix src/ --preview
```

Output shows proposed changes as diffs:

```diff
--- src/MyModule.hs
+++ src/MyModule.hs
@@ -42,1 +42,1 @@
-let first = head items
+let first = headMay items
```

### Applying Fixes

```bash
# Apply only safe, non-breaking fixes (recommended)
argus fix src/ --safe-only

# Apply all available fixes
argus fix src/ --all

# Interactive mode - approve each fix
argus fix src/ --interactive
```

### Backup and Validation

```bash
# Create backup files before modifying (default: on)
argus fix src/ --backup

# Skip validation after fixes (not recommended)
argus fix src/ --no-validate
```

### Fix Safety Levels

Argus categorizes fixes by safety:

| Level | Description | `--safe-only` |
|-------|-------------|---------------|
| `safe` | Always safe to apply | Included |
| `mostly-safe` | Safe in most cases | Included |
| `needs-review` | May need human review | Excluded |
| `unsafe` | May change semantics | Excluded |

## The Unused Command

Detect dead code in your project:

```bash
argus unused [FILES/DIRS...] [OPTIONS]
```

### Basic Usage

```bash
argus unused src/
```

Output:

```
Unused code analysis (quick mode)

Unused exports:
  src/Utils.hs: helperFunction (line 45)

Unused functions:
  src/Internal.hs: privateFunction (line 89)

Unused imports:
  src/MyModule.hs: Data.Map (entire module)
  src/Handler.hs: fromJust (from Data.Maybe)

Summary: 4 unused items found
```

### Specifying Entry Points

Define your application's roots:

```bash
# Single entry point
argus unused src/ --roots "Main.main"

# Multiple entry points
argus unused src/ --roots "Main.main,Server.runServer,^Paths_"

# With Template Haskell roots
argus unused src/ --roots "Main.main" --th-roots "parseJSON,toJSON"
```

### Check Types

```bash
# Check only specific categories
argus unused src/ --check-functions
argus unused src/ --check-types
argus unused src/ --check-imports
argus unused src/ --check-exports

# Generate dependency graph
argus unused src/ --show-graph > deps.dot
```

## The Init Command

Generate a configuration file:

```bash
argus init
```

Creates `argus.toml` with sensible defaults. Use `--force` to overwrite existing:

```bash
argus init --force
```

## Output and Exit Codes

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success (no errors, warnings allowed) |
| 1 | Errors found |
| 2 | Configuration or runtime error |

### Strict Mode

Make warnings fail the build:

```bash
argus check src/ --strict
# Exit code 1 if any warnings or errors
```

### Machine-Readable Output

For CI and tooling:

```bash
# JSON output
argus check src/ --format json > results.json

# SARIF for GitHub
argus check src/ --format sarif > results.sarif

# JUnit for test frameworks
argus check src/ --format junit > results.xml
```

## Common Workflows

### Development Loop

Quick iteration during coding:

```bash
# Watch mode - re-analyzes on save
argus watch src/

# Or manual quick checks
argus check --mode quick src/
```

### Pre-Commit Check

Before committing changes:

```bash
# Full analysis of changed files
argus check --mode quick $(git diff --name-only --cached | grep '\.hs$')
```

### CI Pipeline

```bash
# Strict analysis with SARIF output
argus check src/ --strict --format sarif > results.sarif
```

### Code Review

Analyze a specific module in depth:

```bash
argus check src/CriticalModule.hs --mode full --context-lines 5 -v
```

## Tips

### Filter by Rule Category

```bash
# Disable specific categories in config
argus check -c <(cat argus.toml; echo '[rules]'; echo 'disabled = ["performance/*"]') src/
```

### Combine with Other Tools

```bash
# Run Argus and HLint together
argus check src/ --format json > argus.json
hlint src/ --json > hlint.json
```

### Performance

```bash
# For large codebases, use parallel analysis
argus check src/ --jobs 4

# Cache results for incremental analysis
argus check src/ --cache
```

## Next Steps

- **[Configuration](./configuration)**: Customize rules and settings
- **[Project Setup](./project-setup)**: Configure HIE files for full mode
- **[Analysis Modes](../usage-guide/analysis-modes)**: Understand Quick vs Full vs Plugin
