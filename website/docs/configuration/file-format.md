---
sidebar_position: 1
title: Configuration File Format
description: Complete reference for argus.toml configuration
---

# Configuration File Format

Argus uses TOML format for configuration. This guide covers the complete file structure and all available options.

## File Location

Argus searches for configuration in this order:

1. `--config` command-line option
2. `argus.toml` in current directory
3. `linter.toml` in current directory
4. `.argus.toml` in current directory
5. `argus.toml` in parent directories (up to git root)
6. `~/.config/argus/config.toml` (user global config)

## File Structure

```toml
# argus.toml - Complete configuration example

# General settings
[general]
directories = ["src", "app", "lib"]
exclude = ["**/Paths_*.hs", "**/*_generated.hs"]
mode = "full"

# Analysis settings
[analysis]
parallel = "auto"
cache = true
incremental = true

# Rule configuration
[rules]
enabled = true
severity = "warning"
disabled = ["redundant/dollar-simple"]

[rules.severity]
"partial/*" = "error"
"security/*" = "error"

# Naming conventions
[naming]
enabled = true

[[naming.types]]
pattern = "*Id"
replacement = "Key *"

# Complexity thresholds
[complexity]
enabled = true
max-cyclomatic = 15
max-cognitive = 20

# Output settings
[output]
format = "terminal"
color = "auto"

# HIE file settings
[hie]
directory = ".hie"
required = false

# Fix settings
[fix]
backup = true
safety = "mostly-safe"

# Daemon settings
[daemon]
port = 4567
auto-start = false

# Custom pattern rules
[[patterns.rules]]
name = "custom-rule"
match = "pattern"
message = "message"

# Function restrictions
[[restrictions.functions]]
name = "unsafePerformIO"
within = []
message = "Forbidden function"

# Module restrictions
[[restrictions.modules]]
names = ["Data.Map"]
as = "Map"

# Scoped configurations
[[scopes]]
modules = ["*Spec", "Test.*"]
ignore = ["partial/*"]
```

## Section Reference

| Section | Purpose |
|---------|---------|
| `[general]` | Basic project settings |
| `[analysis]` | Analysis behavior |
| `[rules]` | Rule enable/disable and severity |
| `[naming]` | Naming convention rules |
| `[complexity]` | Complexity thresholds |
| `[output]` | Output format and style |
| `[hie]` | HIE file configuration |
| `[fix]` | Auto-fix settings |
| `[daemon]` | Daemon service settings |
| `[[patterns.rules]]` | Custom pattern rules |
| `[[restrictions.*]]` | Function/module restrictions |
| `[[scopes]]` | Scoped rule overrides |

## General Section

```toml
[general]
# Source directories to analyze
directories = ["src", "app", "lib"]

# Glob patterns to exclude
exclude = [
  "**/Paths_*.hs",
  "**/*_generated.hs",
  "**/Setup.hs",
]

# Files to always include (overrides exclude)
include = []

# Analysis mode: quick, full, plugin
mode = "full"

# Project root (auto-detected if not set)
# root = "/path/to/project"
```

### Directory Patterns

```toml
[general]
# Single directory
directories = ["src"]

# Multiple directories
directories = ["src", "app", "lib", "exe"]

# With glob patterns
directories = ["src/**", "packages/*/src"]
```

### Exclude Patterns

```toml
[general]
exclude = [
  # Generated files
  "**/Paths_*.hs",
  "**/*_generated.hs",

  # Build artifacts
  "dist/**",
  ".stack-work/**",

  # Legacy code
  "src/Legacy/**",

  # Specific files
  "src/Setup.hs",
]
```

## Analysis Section

```toml
[analysis]
# Parallel workers: "auto", number, or 1 for sequential
parallel = "auto"

# Enable result caching
cache = true

# Cache directory
cache-dir = ".argus-cache"

# Incremental analysis (only changed files)
incremental = true

# Timeout per file in seconds
timeout = 30

# Memory limit in MB (0 = unlimited)
memory-limit = 0
```

## Rules Section

See [Rules Configuration](./rules-section) for complete details.

```toml
[rules]
# Master enable/disable
enabled = true

# Default minimum severity to report
severity = "warning"

# Exit with error at this severity or above
fail-on = "error"

# Disabled rules (by name or pattern)
disabled = [
  "redundant/dollar-simple",
  "modernize/return-pure",
]

# Enabled rules (if set, only these are enabled)
# enabled-only = ["partial/*", "security/*"]

# Per-rule severity overrides
[rules.severity]
"partial/*" = "error"
"security/*" = "error"
"performance/*" = "warning"
"modernize/*" = "suggestion"
```

## Naming Section

See [Naming Configuration](./naming-section) for complete details.

```toml
[naming]
enabled = true
check-types = true
check-variables = true
check-functions = true

# Type name mappings
[[naming.types]]
pattern = "*Id"
replacement = "Key *"
severity = "warning"

# Variable naming by type
[[naming.variables]]
type = "Key *"
from = "*"
to = "*K"
```

## Complexity Section

See [Complexity Configuration](./complexity-section) for complete details.

```toml
[complexity]
enabled = true

# Cyclomatic complexity (branches)
max-cyclomatic = 15

# Cognitive complexity (mental effort)
max-cognitive = 20

# Maximum function length in lines
max-function-lines = 100

# Maximum module length in lines
max-module-lines = 1000

# Maximum parameters per function
max-parameters = 5

# Nesting depth limit
max-nesting = 4
```

## Output Section

See [Output Configuration](./output-section) for complete details.

```toml
[output]
# Default format
format = "terminal"

# Color mode: auto, always, never
color = "auto"

# Context lines around issues
context-lines = 2

# Group issues by: file, rule, severity
group-by = "file"

# Show timing information
timing = false

# Show statistics
stats = false
```

## HIE Section

```toml
[hie]
# HIE files directory
directory = ".hie"

# Require HIE files for analysis
required = false

# Auto-rebuild index
auto-rebuild = true

# Index storage location
index-dir = ".argus-index"
```

## Fix Section

See [Fix Configuration](./fix-section) for complete details.

```toml
[fix]
# Create backup files
backup = true

# Backup directory
backup-dir = ".argus-backups"

# Default safety level
safety = "mostly-safe"

# Validate files compile after fix
validate = true

# Auto-add required imports
add-imports = true
```

## Daemon Section

```toml
[daemon]
# TCP port
port = 4567

# Unix socket path (alternative to port)
# socket = "/tmp/argus.sock"

# Auto-start daemon
auto-start = false

# Maximum memory in MB
max-memory = 512

# Worker threads
workers = 4

# Idle timeout in seconds
idle-timeout = 3600
```

## Pattern Rules

Custom pattern-based rules:

```toml
[[patterns.rules]]
name = "my-rule"
match = "pattern $X"
fix = "replacement $X"
severity = "warning"
message = "Description"
category = "custom"
safety = "safe"
note = "Additional information"
enabled = true
```

See [Custom Rules](../rules/custom-rules) for complete DSL reference.

## Restrictions

### Function Restrictions

```toml
[[restrictions.functions]]
name = "unsafePerformIO"
within = []  # Empty = banned everywhere
message = "Never use unsafePerformIO"

[[restrictions.functions]]
name = "trace"
within = ["*.Debug", "Test.*"]  # Allowed only here
message = "trace only in debug/test"
```

### Module Restrictions

```toml
[[restrictions.modules]]
names = ["Data.Map", "Data.Map.Strict"]
as = "Map"  # Require qualified import
message = "Import Data.Map qualified as Map"
```

### Extension Restrictions

```toml
[[restrictions.extensions]]
name = "TemplateHaskell"
within = ["*.TH", "TH.*"]
message = "TemplateHaskell only in TH modules"
```

## Scopes

Apply different configurations to different parts of the codebase:

```toml
# Relax rules in tests
[[scopes]]
modules = ["*Spec", "Test.*", "*Test*"]
ignore = ["partial/*", "complexity/*"]

# Strict rules for public API
[[scopes]]
modules = ["*.API.*", "*.Public.*"]
[scopes.severity]
"partial/*" = "error"
"security/*" = "error"

# Ignore generated code entirely
[[scopes]]
modules = ["Generated.*", "Paths_*"]
ignore = ["ALL"]
```

See [Scopes Configuration](./scopes-section) for complete details.

## Environment Variables

Override configuration with environment variables:

| Variable | Overrides |
|----------|-----------|
| `ARGUS_CONFIG` | Config file path |
| `ARGUS_MODE` | `general.mode` |
| `ARGUS_PARALLEL` | `analysis.parallel` |
| `ARGUS_COLOR` | `output.color` |
| `ARGUS_CACHE_DIR` | `analysis.cache-dir` |
| `ARGUS_HIE_DIR` | `hie.directory` |

## Validation

Validate your configuration:

```bash
argus check --validate-config
```

Output:

```
Configuration valid: argus.toml

Sections:
  [general]    ✓
  [analysis]   ✓
  [rules]      ✓
  [naming]     ✓
  [complexity] ✓
  [output]     ✓

Custom Rules: 3 defined
Restrictions: 5 defined
Scopes: 2 defined
```

## Including Other Files

Split configuration across files:

```toml
# argus.toml
[general]
directories = ["src"]

[rules]
# Include rules from other files
include = [
  "rules/security.toml",
  "rules/company.toml",
]

# Include all files in directory
include-dir = "rules/"
```

## Next Steps

- **[Rules Configuration](./rules-section)**: Rule settings in detail
- **[Naming Configuration](./naming-section)**: Naming conventions
- **[Complexity Configuration](./complexity-section)**: Complexity thresholds
- **[Custom Rules](../rules/custom-rules)**: Pattern DSL reference
