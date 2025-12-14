---
sidebar_position: 1
title: CLI Overview
description: Complete command-line interface reference for Argus
---

# CLI Reference

Argus provides a comprehensive command-line interface for static analysis, code fixing, and project management.

## Command Summary

| Command | Description |
|---------|-------------|
| [`argus check`](./check) | Run static analysis on Haskell files |
| [`argus fix`](./fix) | Automatically fix detected issues |
| [`argus unused`](./unused) | Detect unused code and exports |
| [`argus init`](./init) | Initialize project configuration |
| [`argus index`](./index-cmd) | Build HIE file index for analysis |
| [`argus watch`](./watch) | Watch files and analyze on change |
| [`argus daemon`](./daemon) | Run as background daemon service |
| [`argus lsp`](./lsp) | Start Language Server Protocol server |

## Global Options

These options are available for all commands:

```bash
argus [GLOBAL OPTIONS] <command> [COMMAND OPTIONS]
```

### Configuration

| Option | Description |
|--------|-------------|
| `--config FILE` | Use specified config file (default: `argus.toml`) |
| `--no-config` | Ignore all configuration files |
| `--project-root DIR` | Set project root directory |

### Output Control

| Option | Description |
|--------|-------------|
| `-q, --quiet` | Suppress non-essential output |
| `-v, --verbose` | Enable verbose output |
| `--debug` | Enable debug output (very verbose) |
| `--color MODE` | Color output: `auto`, `always`, `never` |
| `--no-color` | Disable colored output |

### Format Selection

| Option | Description |
|--------|-------------|
| `-f, --format FORMAT` | Output format (see [Output Formats](./output-formats)) |
| `-o, --output FILE` | Write output to file instead of stdout |

Available formats: `terminal`, `json`, `sarif`, `html`, `junit`, `codeclimate`, `checkstyle`, `github`

### Help and Version

| Option | Description |
|--------|-------------|
| `-h, --help` | Show help message |
| `--version` | Show version information |
| `--help-format FORMAT` | Show detailed help for output format |

## Exit Codes

| Code | Meaning |
|------|---------|
| `0` | Success (no issues found or all fixes applied) |
| `1` | Issues found (errors or warnings) |
| `2` | Configuration error |
| `3` | File not found |
| `4` | Parse error in source files |
| `5` | Internal error |

## Environment Variables

| Variable | Description |
|----------|-------------|
| `ARGUS_CONFIG` | Default configuration file path |
| `ARGUS_COLOR` | Default color mode (`auto`, `always`, `never`) |
| `ARGUS_VERBOSE` | Enable verbose output if set |
| `ARGUS_CACHE_DIR` | Cache directory location |
| `ARGUS_HIE_DIR` | HIE files directory |
| `ARGUS_PARALLEL` | Number of parallel workers |
| `NO_COLOR` | Disable colors if set (standard) |

## Quick Examples

### Basic Analysis

```bash
# Check single file
argus check src/Main.hs

# Check entire directory
argus check src/

# Check with specific rules
argus check --rules "partial/*,security/*" src/

# Check with verbose output
argus check -v src/
```

### Auto-Fix

```bash
# Preview fixes
argus fix --preview src/

# Apply safe fixes only
argus fix --safe-only src/

# Apply all fixes interactively
argus fix -i src/
```

### Output Formats

```bash
# JSON output
argus check -f json src/ -o report.json

# SARIF for CI
argus check -f sarif src/ -o results.sarif

# HTML report
argus check -f html src/ -o report.html
```

### CI Integration

```bash
# GitHub Actions annotations
argus check -f github src/

# Exit code control
argus check --fail-on warning src/

# JUnit for test reporters
argus check -f junit src/ -o argus-results.xml
```

## Configuration File Discovery

Argus searches for configuration in this order:

1. Path specified with `--config`
2. `argus.toml` in current directory
3. `linter.toml` in current directory
4. `.argus.toml` in current directory
5. `argus.toml` in parent directories (up to git root)
6. `~/.config/argus/config.toml` (user config)

## Shell Completion

### Bash

```bash
# Add to ~/.bashrc
eval "$(argus --completion bash)"
```

### Zsh

```bash
# Add to ~/.zshrc
eval "$(argus --completion zsh)"
```

### Fish

```bash
# Add to ~/.config/fish/config.fish
argus --completion fish | source
```

## Performance Tips

### Parallel Processing

```bash
# Use all CPU cores
argus check -j auto src/

# Limit to 4 workers
argus check -j 4 src/

# Single-threaded (for debugging)
argus check -j 1 src/
```

### Caching

```bash
# Enable result caching
argus check --cache src/

# Clear cache
argus check --clear-cache src/

# Cache to specific directory
argus check --cache-dir .argus-cache src/
```

### Incremental Analysis

```bash
# Analyze only changed files
argus check --incremental src/

# Use git to determine changes
argus check --since HEAD~1 src/

# Compare against branch
argus check --since main src/
```

## Common Workflows

### Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

# Check staged files only
git diff --cached --name-only --diff-filter=d | \
  grep '\.hs$' | \
  xargs -r argus check --fail-on error

exit $?
```

### CI Pipeline

```bash
# Full analysis with SARIF output
argus check -f sarif src/ -o argus.sarif

# Upload to GitHub Security tab
# (handled by github/codeql-action/upload-sarif)
```

### IDE Integration

```bash
# Start LSP server
argus lsp

# Daemon for faster repeated checks
argus daemon start
argus check --daemon src/
```

## Next Steps

- **[argus check](./check)**: Detailed check command reference
- **[argus fix](./fix)**: Auto-fix command reference
- **[Output Formats](./output-formats)**: All supported output formats
- **[Configuration](../configuration/file-format)**: Configuration file reference
