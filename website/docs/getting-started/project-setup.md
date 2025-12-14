---
sidebar_position: 4
title: Project Setup
description: Set up your project for Argus analysis
---

# Project Setup

This guide covers how to configure your Haskell project for optimal Argus analysis, including HIE file generation for full semantic analysis.

## Quick Mode Setup

Quick mode requires no special setup—Argus parses your source files directly:

```bash
argus check --mode quick src/
```

This is ideal for:
- Development iteration
- Pre-commit hooks
- Initial exploration of a new codebase

## Full Mode Setup

Full mode uses HIE (Haskell Interface Extended) files for deep semantic analysis. This requires compiling your project with HIE generation enabled.

### Stack Configuration

Add HIE generation to your `stack.yaml`:

```yaml
# stack.yaml
resolver: lts-24.21

ghc-options:
  "$locals":
    -fwrite-ide-info
    -hiedir=.hie

# Optional: extra dependencies
extra-deps: []
```

Or in `package.yaml`:

```yaml
# package.yaml
ghc-options:
  - -Wall
  - -fwrite-ide-info
  - -hiedir=.hie
```

### Building with HIE Files

```bash
# Clean build to ensure fresh HIE files
stack clean
stack build
```

Verify HIE files were generated:

```bash
ls .hie/
# Should show .hie files for your modules
```

### Running Full Mode

```bash
argus check --mode full --hie-dir .hie src/
```

Or configure in `argus.toml`:

```toml
[general]
mode = "full"
hie-dir = ".hie"
```

## Plugin Mode Setup

Plugin mode provides compile-time analysis with maximum precision.

### Stack Configuration

```yaml
# stack.yaml
extra-deps:
  - argus-1.0.0  # Or path to local Argus

ghc-options:
  "$locals":
    -fplugin=Argus.Plugin
```

### Verification

When you build, you'll see Argus output during compilation:

```bash
stack build
# [1 of 5] Compiling MyModule
# Argus: warning: [partial/head] Use of partial function 'head'
```

## CI/CD Integration

### GitHub Actions

```yaml
# .github/workflows/argus.yml
name: Argus Analysis

on: [push, pull_request]

jobs:
  analyze:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Setup Haskell
        uses: haskell-actions/setup@v2
        with:
          ghc-version: '9.10.3'
          enable-stack: true

      - name: Cache Stack
        uses: actions/cache@v4
        with:
          path: |
            ~/.stack
            .stack-work
          key: ${{ runner.os }}-stack-${{ hashFiles('stack.yaml.lock') }}

      - name: Build with HIE
        run: stack build --ghc-options="-fwrite-ide-info -hiedir=.hie"

      - name: Install Argus
        run: |
          git clone https://github.com/quinten/argus.git /tmp/argus
          cd /tmp/argus && stack install

      - name: Run Argus
        run: argus check --mode full --format sarif > results.sarif

      - name: Upload SARIF
        uses: github/codeql-action/upload-sarif@v3
        with:
          sarif_file: results.sarif
```

### GitLab CI

```yaml
# .gitlab-ci.yml
stages:
  - build
  - analyze

build:
  stage: build
  image: haskell:9.10.3
  script:
    - stack build --ghc-options="-fwrite-ide-info -hiedir=.hie"
  artifacts:
    paths:
      - .hie/
      - .stack-work/
    expire_in: 1 hour

argus:
  stage: analyze
  image: haskell:9.10.3
  needs: [build]
  script:
    - git clone https://github.com/quinten/argus.git /tmp/argus
    - cd /tmp/argus && stack install
    - argus check --mode full --format codeclimate > codeclimate.json
  artifacts:
    reports:
      codequality: codeclimate.json
```

### Pre-Commit Hook

Create `.git/hooks/pre-commit`:

```bash
#!/bin/bash
set -e

# Get staged Haskell files
STAGED_HS=$(git diff --cached --name-only --diff-filter=ACM | grep '\.hs$' || true)

if [ -n "$STAGED_HS" ]; then
    echo "Running Argus on staged files..."
    argus check --mode quick $STAGED_HS
fi
```

Make it executable:

```bash
chmod +x .git/hooks/pre-commit
```

## Project Structure Recommendations

### Recommended Layout

```
my-project/
├── app/
│   └── Main.hs
├── src/
│   └── MyProject/
│       ├── Types.hs      # Core types (no dependencies)
│       ├── Core.hs       # Core logic
│       ├── Data/         # Database layer
│       ├── Service/      # Business logic
│       └── API/          # External interfaces
├── test/
│   └── Spec.hs
├── argus.toml
├── package.yaml
└── stack.yaml
```

### Corresponding Argus Config

```toml
[general]
directories = ["src", "app"]
exclude = ["test/**", ".stack-work/**"]

[architecture]
enabled = true

[[architecture.layers]]
name = "Core"
patterns = ["*.Types", "*.Core"]
can-import = ["Core"]

[[architecture.layers]]
name = "Data"
patterns = ["*.Data.*"]
can-import = ["Core", "Data"]

[[architecture.layers]]
name = "Service"
patterns = ["*.Service.*"]
can-import = ["Core", "Data", "Service"]

[[architecture.layers]]
name = "API"
patterns = ["*.API.*"]
can-import = ["Core", "Data", "Service", "API"]
```

## Working with Large Codebases

### Incremental Analysis

For large projects, use incremental mode:

```bash
# First run builds cache
argus check --mode full --cache src/

# Subsequent runs use cache
argus check --mode full --cache src/
```

### Parallel Analysis

```bash
# Use multiple threads
argus check --jobs 4 src/
```

### Selective Analysis

```bash
# Analyze only changed files
git diff --name-only HEAD~1 | grep '\.hs$' | xargs argus check
```

## Integrating with Existing Tools

### Alongside HLint

Run both tools for comprehensive analysis:

```bash
#!/bin/bash
# lint.sh

echo "Running HLint..."
hlint src/ --json > hlint.json

echo "Running Argus..."
argus check src/ --format json > argus.json

echo "Combining results..."
jq -s '.[0] + .[1]' hlint.json argus.json > combined.json
```

### With GHC Warnings

Argus complements GHC's built-in warnings:

```yaml
# package.yaml
ghc-options:
  - -Wall
  - -Wcompat
  - -Widentities
  - -Wincomplete-record-updates
  - -Wincomplete-uni-patterns
  - -Wpartial-fields
  - -Wredundant-constraints
  - -fwrite-ide-info
  - -hiedir=.hie
```

## Troubleshooting

### HIE Files Not Found

```
Error: No HIE files found in .hie/
```

Solution:
1. Verify `stack.yaml` has `-fwrite-ide-info -hiedir=.hie`
2. Run `stack clean && stack build`
3. Check `.hie/` directory exists and contains files

### GHC Version Mismatch

```
Error: HIE files were generated with GHC 9.8, but Argus uses GHC 9.10
```

Solution:
1. Rebuild with matching GHC version
2. Or use quick mode which doesn't require HIE files

### Out of Memory

```
Error: Out of memory during analysis
```

Solution:
1. Increase memory limit: `+RTS -M8G -RTS`
2. Analyze in smaller batches
3. Use quick mode for initial passes

### Slow Analysis

If analysis is slow:
1. Enable caching: `--cache`
2. Use parallel analysis: `--jobs N`
3. Exclude generated files in config
4. Consider quick mode for development

## Next Steps

- **[Analysis Modes](../usage-guide/analysis-modes)**: Understand Quick vs Full vs Plugin
- **[CI Integration](../usage-guide/ci-integration)**: Detailed CI setup
- **[Working with HIE](../usage-guide/working-with-hie)**: Deep dive into HIE analysis
