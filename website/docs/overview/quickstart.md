---
sidebar_position: 3
title: Quickstart
description: Get Argus running on your project in 5 minutes
---

# Quickstart

Get Argus analyzing your Haskell project in 5 minutes.

## Prerequisites

- **GHC 9.10.3** (via Stack)
- **Stack** build tool
- A Haskell project to analyze

## Step 1: Install Argus

```bash
# Clone the repository
git clone https://github.com/quinten/argus.git
cd argus

# Build and install
stack build
stack install
```

This installs the `argus` executable to your Stack bin path (typically `~/.local/bin`).

:::tip Verify Installation
```bash
argus --version
# Argus 1.0.0
```
:::

## Step 2: Initialize Configuration

Navigate to your project and create a configuration file:

```bash
cd /path/to/your/project
argus init
```

This creates `argus.toml` with sensible defaults:

```toml
[general]
directories = ["src", "app"]
exclude = [".stack-work/**", "dist-newstyle/**"]
mode = "quick"

[unused]
enabled = true
roots = ["^Main.main$", "^Paths_.*"]

[security]
enabled = true

[performance]
enabled = true

[complexity]
enabled = true
cyclomatic-warning = 10
```

## Step 3: Run Your First Analysis

```bash
# Analyze your source directory
argus check src/
```

You'll see output like:

```
src/MyModule.hs:42:5: warning: [partial/head]
    Use of partial function 'head': Crashes on empty list.
    Consider using 'headMay' from 'Safe' instead.
    |
 42 | let first = head items
    |             ^^^^

src/Database.hs:78:12: error: [security/sql-injection]
    Potential SQL injection: query built with string concatenation.
    Use parameterized queries instead.
    |
 78 | query = "SELECT * FROM users WHERE id = '" ++ id ++ "'"
    |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Found 15 issues (2 errors, 8 warnings, 5 suggestions)
```

## Step 4: Preview Auto-Fixes

Many issues can be fixed automatically. Preview the fixes first:

```bash
argus fix src/ --preview
```

Output shows the proposed changes:

```diff
--- src/MyModule.hs
+++ src/MyModule.hs
@@ -42,1 +42,1 @@
-let first = head items
+let first = headMay items

--- src/Performance.hs
+++ src/Performance.hs
@@ -15,1 +15,1 @@
-isEmpty xs = length xs == 0
+isEmpty xs = null xs
```

## Step 5: Apply Safe Fixes

Apply only safe, non-breaking fixes:

```bash
argus fix src/ --safe-only
```

For more control, use interactive mode:

```bash
argus fix src/ --interactive
```

This prompts for each fix:

```
src/MyModule.hs:42:5 [partial/head]
Replace 'head' with 'headMay'

Apply this fix? [y]es / [n]o / [a]ll / [q]uit: y
✓ Applied fix to src/MyModule.hs
```

## Step 6: Detect Unused Code

Find dead code in your project:

```bash
argus unused src/ --roots "Main.main"
```

Output:

```
Unused code analysis (quick mode)

Unused exports:
  src/Utils.hs: helperUnused (line 45)
  src/Types.hs: OldDataType (line 23)

Unused functions:
  src/Internal.hs: privateFunction (line 89)

Unused imports:
  src/MyModule.hs: Data.Map (entire module unused)

Summary: 4 unused items found
```

## Step 7: Generate Reports

### For GitHub Code Scanning

```bash
argus check src/ --format sarif > results.sarif
```

Upload to GitHub Actions:

```yaml
- name: Run Argus
  run: argus check src/ --format sarif > results.sarif

- name: Upload SARIF
  uses: github/codeql-action/upload-sarif@v2
  with:
    sarif_file: results.sarif
```

### HTML Report

```bash
argus check src/ --format html > report.html
open report.html
```

### JSON for Custom Tooling

```bash
argus check src/ --format json > results.json
```

## Common Workflows

### Quick Iteration (Development)

```bash
# Fast syntax-only analysis
argus check --mode quick src/
```

### Deep Analysis (Pre-Commit)

```bash
# Full semantic analysis (requires HIE files)
stack build --ghc-options="-fwrite-ide-info -hiedir=.hie"
argus check --mode full src/
```

### Continuous Monitoring

```bash
# Watch mode - re-analyzes on file changes
argus watch src/
```

### CI Pipeline

```bash
# Strict mode - exit code 1 on any warnings
argus check src/ --format sarif --strict > results.sarif
```

## What's Next?

Now that you have Argus running, explore:

- **[Installation](../getting-started/installation)**: Advanced installation options
- **[Configuration](../getting-started/configuration)**: Customize Argus for your project
- **[Analysis Modes](../usage-guide/analysis-modes)**: Understand Quick, Full, and Plugin modes
- **[Rules Overview](../rules/overview)**: Explore all 1,100+ rules
- **[CI Integration](../usage-guide/ci-integration)**: Set up automated analysis

## Getting Help

- **CLI Help**: `argus --help` or `argus <command> --help`
- **GitHub Issues**: [Report bugs or request features](https://github.com/quinten/argus/issues)
- **Troubleshooting**: [Common issues and solutions](../faq/troubleshooting)
