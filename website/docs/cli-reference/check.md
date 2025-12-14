---
sidebar_position: 2
title: argus check
description: Run static analysis on Haskell source files
---

# argus check

Run static analysis on Haskell source files to detect issues, code smells, and potential bugs.

## Synopsis

```bash
argus check [OPTIONS] [FILES|DIRECTORIES...]
```

## Description

The `check` command analyzes Haskell source files and reports issues based on the enabled rules. It supports multiple analysis modes, from fast syntactic checks to deep semantic analysis using HIE files.

## Arguments

| Argument | Description |
|----------|-------------|
| `FILES` | Specific `.hs` files to analyze |
| `DIRECTORIES` | Directories to scan recursively for `.hs` files |

If no files or directories are specified, Argus checks the current directory.

## Options

### Analysis Mode

| Option | Description |
|--------|-------------|
| `--quick` | Fast syntactic analysis only (no HIE) |
| `--full` | Full analysis with HIE files (default if available) |
| `--plugin` | Use GHC plugin for deepest analysis |
| `--mode MODE` | Explicitly set analysis mode |

### Rule Selection

| Option | Description |
|--------|-------------|
| `--rules RULES` | Enable only specified rules (comma-separated) |
| `--categories CATS` | Enable only specified categories |
| `--exclude-rules RULES` | Disable specific rules |
| `--exclude-categories CATS` | Disable entire categories |
| `--only PATTERN` | Only rules matching glob pattern |

### Severity Control

| Option | Description |
|--------|-------------|
| `--severity LEVEL` | Minimum severity to report: `error`, `warning`, `suggestion`, `info` |
| `--fail-on LEVEL` | Exit with error if issues at this level or higher |
| `--warnings-as-errors` | Treat warnings as errors |
| `--no-suggestions` | Hide suggestions (only errors/warnings) |

### File Filtering

| Option | Description |
|--------|-------------|
| `--include GLOB` | Only check files matching pattern |
| `--exclude GLOB` | Skip files matching pattern |
| `--exclude-dir DIR` | Skip entire directories |
| `--ignore-generated` | Skip generated files (detected by comment) |

### HIE Options

| Option | Description |
|--------|-------------|
| `--hie-dir DIR` | HIE files directory (default: `.hie`) |
| `--hie-required` | Fail if HIE files are missing |
| `--no-hie` | Disable HIE-based analysis |
| `--rebuild-hie` | Force HIE index rebuild |

### Performance

| Option | Description |
|--------|-------------|
| `-j, --jobs N` | Number of parallel workers (`auto` for CPU count) |
| `--cache` | Enable result caching |
| `--no-cache` | Disable caching |
| `--incremental` | Only analyze changed files |
| `--since REF` | Analyze files changed since git ref |

### Output

| Option | Description |
|--------|-------------|
| `-f, --format FORMAT` | Output format (default: `terminal`) |
| `-o, --output FILE` | Write output to file |
| `--stats` | Show analysis statistics |
| `--timing` | Show timing information |
| `--summary` | Show summary only, not individual issues |

## Examples

### Basic Usage

```bash
# Check current directory
argus check

# Check specific files
argus check src/Main.hs src/Lib.hs

# Check directory recursively
argus check src/

# Check multiple directories
argus check src/ app/ lib/
```

### Rule Selection

```bash
# Check only partial function rules
argus check --rules "partial/*" src/

# Check multiple categories
argus check --categories "security,performance" src/

# Exclude specific rules
argus check --exclude-rules "redundant/dollar-simple" src/

# Exclude entire categories
argus check --exclude-categories "suggestion,info" src/

# Only security and partial rules
argus check --only "security/*,partial/*" src/
```

### Severity Filtering

```bash
# Only errors
argus check --severity error src/

# Errors and warnings only
argus check --severity warning src/

# Fail CI on any warning
argus check --fail-on warning src/

# Treat all warnings as errors
argus check --warnings-as-errors src/
```

### Analysis Modes

```bash
# Fast syntax-only check
argus check --quick src/

# Full semantic analysis
argus check --full src/

# Explicit mode selection
argus check --mode quick src/
argus check --mode full src/
argus check --mode plugin src/
```

### File Filtering

```bash
# Only check files in specific directory pattern
argus check --include "src/Api/**/*.hs" .

# Skip test files
argus check --exclude "*Spec.hs" --exclude "*Test.hs" src/

# Skip directories
argus check --exclude-dir test --exclude-dir bench src/

# Skip generated files
argus check --ignore-generated src/
```

### HIE Configuration

```bash
# Specify HIE directory
argus check --hie-dir .hie src/

# Require HIE files (fail if missing)
argus check --hie-required src/

# Force syntax-only (no HIE)
argus check --no-hie src/

# Rebuild HIE index before checking
argus check --rebuild-hie src/
```

### Performance Optimization

```bash
# Parallel analysis with all cores
argus check -j auto src/

# Limited parallelism
argus check -j 4 src/

# Enable caching
argus check --cache src/

# Incremental (changed files only)
argus check --incremental src/

# Changed since last commit
argus check --since HEAD~1 src/

# Changed from main branch
argus check --since main src/
```

### Output Formats

```bash
# Default terminal output
argus check src/

# JSON output
argus check -f json src/

# SARIF for security tools
argus check -f sarif src/ -o results.sarif

# HTML report
argus check -f html src/ -o report.html

# GitHub Actions annotations
argus check -f github src/

# JUnit XML for CI
argus check -f junit src/ -o results.xml
```

### Statistics and Timing

```bash
# Show analysis statistics
argus check --stats src/

# Show timing breakdown
argus check --timing src/

# Both stats and timing
argus check --stats --timing src/

# Summary only (no individual issues)
argus check --summary src/
```

## Output

### Terminal Output

```
src/Api/Handler.hs:45:12: warning [partial/head]
  Use headMay instead of head
  |
45|   let first = head items
  |               ^^^^
  = suggestion: Replace with `headMay items`
  = note: head throws exception on empty list

src/Api/Handler.hs:67:5: error [security/sql-injection]
  Potential SQL injection vulnerability
  |
67|   rawSql query []
  |   ^^^^^^^^^^^^^^
  = note: Use parameterized queries instead

Found 2 issues (1 error, 1 warning) in 15 files
Analysis time: 0.82s
```

### With Statistics

```
Argus Analysis Report
=====================

Files analyzed: 156
Lines of code: 12,450
Analysis time: 2.3s

Issues by Severity:
  Errors:      3
  Warnings:   12
  Suggestions: 8
  Info:        2

Issues by Category:
  partial:     5
  security:    3
  performance: 7
  redundant:   10

Top Rules:
  partial/head:           4
  redundant/id:           3
  performance/length-null: 3
```

## Exit Codes

| Code | Condition |
|------|-----------|
| `0` | No issues found (or only below `--fail-on` level) |
| `1` | Issues found at or above `--fail-on` level |
| `2` | Configuration error |
| `3` | File not found |
| `4` | Parse error in source file |

## Configuration

Check behavior can be configured in `argus.toml`:

```toml
[general]
# Default directories to check
directories = ["src", "app", "lib"]

# Files to exclude
exclude = ["**/*_generated.hs", "**/Paths_*.hs"]

# Analysis mode
mode = "full"

[analysis]
# Parallel workers
parallel = "auto"

# Enable caching
cache = true

[rules]
# Default severity threshold
severity = "warning"

# Disabled rules
disabled = ["redundant/dollar-simple"]

# Category severities
[rules.severity]
"partial/*" = "error"
"security/*" = "error"
```

## Integration with Daemon

For faster repeated checks, use the daemon:

```bash
# Start daemon
argus daemon start

# Check using daemon (much faster)
argus check --daemon src/

# Stop daemon when done
argus daemon stop
```

## See Also

- **[argus fix](./fix)**: Auto-fix detected issues
- **[argus unused](./unused)**: Detect unused code
- **[Output Formats](./output-formats)**: Detailed format documentation
- **[Rules Overview](../rules/overview)**: Available rules
