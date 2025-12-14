---
sidebar_position: 3
title: Configuration
description: Configure Argus with argus.toml
---

# Configuration

Argus uses TOML configuration files to customize analysis behavior. This guide covers creating and understanding your configuration.

## Configuration Files

Argus looks for configuration in this order:

1. File specified with `-c` / `--config` flag
2. `argus.toml` in the current directory
3. `linter.toml` in the current directory (legacy)
4. `~/.config/argus/config.toml` (user defaults)

## Creating Configuration

Generate a starter configuration:

```bash
argus init
```

This creates `argus.toml` with documented defaults.

## Configuration Sections

### General Settings

```toml
[general]
# Directories to analyze
directories = ["src", "app", "lib"]

# Patterns to exclude (glob syntax)
exclude = [
  ".stack-work/**",
  "dist-newstyle/**",
  "dist/**",
  "Generated/**",
  "*.gen.hs",
  ".git/**",
]

# Analysis mode: "quick", "full", or "plugin"
mode = "quick"

# HIE files directory (for full mode)
hie-dir = ".hie"
```

### Output Settings

```toml
[output]
# Format: "terminal", "json", "sarif", "html", "junit", "codeclimate", "checkstyle"
format = "terminal"

# Enable colored output
color = true

# Group output: "file", "rule", or "severity"
group-by = "file"

# Show source context
show-context = true

# Lines of context to show (0-10)
context-lines = 2
```

### Unused Code Detection

```toml
[unused]
enabled = true

# What to check
check-functions = true
check-types = true
check-imports = true
check-exports = true
check-constructors = true
check-record-fields = true
check-local-binds = true
check-instances = false  # Often noisy

# Entry points (regex patterns)
roots = [
  "^Main.main$",
  "^Paths_.*",       # Cabal-generated
  "^Spec\\.",        # Test specs
]

# Template Haskell generated functions
th-roots = [
  "parseJSON",
  "toJSON",
  "makeLenses",
  "makeClassy",
  "deriveJSON",
]

# Auto-detect typeclass instances as roots
typeclass-roots = true

# Auto-detect derived instances as roots
derive-roots = true

# Minimum confidence for unused detection (0.0-1.0)
min-confidence = 0.7

# Ignore patterns (prefix matching)
ignore-patterns = ["_*", "unused*"]
```

### Naming Conventions

```toml
[naming]
enabled = true

# Type signature mappings
[[naming.types]]
pattern = "LocationId"
replacement = "Key Location"
severity = "warning"

[[naming.types]]
pattern = "UserId"
replacement = "Key User"
severity = "warning"

# Variable naming based on type
[[naming.variables]]
type = "Key Location"
from = "*"        # Match any name
to = "locationK"  # Suggested name
severity = "warning"

[[naming.variables]]
type = "Entity Page"
from = "Entity * *"   # Pattern with wildcards
to = "Entity pK pV"   # Template replacement
severity = "warning"
```

### Pattern Rules

Define custom pattern-based rules:

```toml
[patterns]
enabled = true

[[patterns.rules]]
name = "avoid-head"
match = "head"
fix = "headMay"
severity = "warning"
message = "Use headMay instead of partial head"

[[patterns.rules]]
name = "prefer-text"
match = "String"
severity = "suggestion"
message = "Consider using Text instead of String for better performance"

[[patterns.rules]]
name = "prefer-foldl-prime"
match = "foldl "
fix = "foldl' "
severity = "suggestion"
message = "Use foldl' (strict) to avoid space leaks"
```

### Import Analysis

```toml
[imports]
# Remove unused imports
remove-unused = true

# Suggest qualified imports for these modules
suggest-qualified = [
  "Data.Map",
  "Data.Set",
  "Data.Text",
  "Data.ByteString",
  "Data.HashMap.Strict",
]

# Require explicit import lists (no wildcards)
require-explicit = false

# Combine fragmented imports from same module
combine = true
```

### Security Rules

```toml
[security]
enabled = true

# Individual check toggles
check-injection = true
check-crypto = true
check-secrets = true
check-unsafe = true
check-ffi = true
check-debug = true
```

### Performance Rules

```toml
[performance]
enabled = true

check-data-structures = true
check-algorithms = true
check-string-types = true
check-lazy-strict = true
check-fusion = true
check-boxing = true
```

### Complexity Thresholds

```toml
[complexity]
enabled = true

# Cyclomatic complexity (decision points)
cyclomatic-warning = 10
cyclomatic-error = 20

# Cognitive complexity (understandability)
cognitive-warning = 15
cognitive-error = 25

# Function length (lines)
line-length-warning = 50

# Nesting depth
nesting-warning = 4

# Parameter count
parameter-warning = 5

# Case branches
pattern-branch-warning = 10
```

### Auto-Fix Settings

```toml
[fix]
enabled = true

# Only apply safe fixes by default
safe-only = true

# Preview changes before applying
preview = true

# Create backup files
backup = true

[fix.auto-imports]
# Automatically add missing imports for fixes
enabled = true
add-missing = true
remove-unused = true
organize = false
use-explicit = true
qualify-new = false
```

### Architecture Rules

```toml
[architecture]
enabled = true

# Maximum allowed cycle length
max-cycle-length = 10

# Instability threshold (Ce / (Ca + Ce))
instability-threshold = 0.8

# Maximum coupling
coupling-threshold = 15

# Check for orphan instances
check-orphans = true

# Define architectural layers
[[architecture.layers]]
name = "Core"
patterns = ["*.Types", "*.Core"]
can-import = ["Core"]

[[architecture.layers]]
name = "Data"
patterns = ["*.Data.*", "*.Model.*"]
can-import = ["Core", "Data"]

[[architecture.layers]]
name = "Service"
patterns = ["*.Service.*", "*.Logic.*"]
can-import = ["Core", "Data", "Service"]

[[architecture.layers]]
name = "API"
patterns = ["*.API.*", "*.Handler.*"]
can-import = ["Core", "Data", "Service", "API"]

[[architecture.layers]]
name = "App"
patterns = ["Main", "*.App", "*.CLI"]
can-import = ["Core", "Data", "Service", "API", "App"]
```

### Resource Limits

```toml
[resource]
# Analysis timeout per file (seconds)
timeout-seconds = 30

# Retry failed analyses
max-retries = 3

# Kill on timeout
kill-on-timeout = true

# Track timing per file
track-per-file = false
```

### Disabling Rules

```toml
[rules]
# Disable specific rules by ID
disabled = [
  "performance/allocation",
  "performance/boxing",
  "modernize/traversable",
  "extension/missing",
]
```

## Environment-Specific Config

Use different configs for different contexts:

```bash
# Development (relaxed)
argus check -c argus.dev.toml src/

# CI (strict)
argus check -c argus.ci.toml src/
```

## Inline Suppressions

Suppress warnings in code with comments:

```haskell
-- Suppress for specific line
let x = head items  -- argus:ignore partial/head

-- Suppress with reason
let y = fromJust val  -- argus:ignore partial/fromJust: checked above

-- Suppress for block
-- argus:ignore-begin security/trace
debugFunction x = trace "debug" x
-- argus:ignore-end
```

## Validating Configuration

Check your configuration for errors:

```bash
argus check --validate-config
```

## Example: Full Configuration

Here's a complete example for a web application:

```toml
# argus.toml - Web Application Configuration

[general]
directories = ["src", "app"]
exclude = [".stack-work/**", "dist-newstyle/**", "Generated/**"]
mode = "quick"
hie-dir = ".hie"

[output]
format = "terminal"
color = true
group-by = "file"
context-lines = 2

[unused]
enabled = true
roots = ["^Main.main$", "^Paths_"]
th-roots = ["parseJSON", "toJSON", "deriveJSON", "makeLenses"]

[naming]
enabled = true

[[naming.types]]
pattern = "UserId"
replacement = "Key User"

[patterns]
enabled = true

[[patterns.rules]]
name = "avoid-head"
match = "head"
fix = "headMay"
severity = "warning"
message = "Use headMay for safe list operations"

[imports]
remove-unused = true
suggest-qualified = ["Data.Map", "Data.Text"]

[security]
enabled = true

[performance]
enabled = true

[complexity]
enabled = true
cyclomatic-warning = 10
cyclomatic-error = 20

[fix]
enabled = true
safe-only = true
backup = true

[architecture]
enabled = true

[[architecture.layers]]
name = "Domain"
patterns = ["*.Domain.*", "*.Types"]
can-import = ["Domain"]

[[architecture.layers]]
name = "Application"
patterns = ["*.Service.*", "*.UseCase.*"]
can-import = ["Domain", "Application"]

[[architecture.layers]]
name = "Infrastructure"
patterns = ["*.Database.*", "*.External.*"]
can-import = ["Domain", "Application", "Infrastructure"]

[[architecture.layers]]
name = "Presentation"
patterns = ["*.API.*", "*.Handler.*", "*.Web.*"]
can-import = ["Domain", "Application", "Infrastructure", "Presentation"]

[resource]
timeout-seconds = 30

[rules]
disabled = ["extension/missing"]
```

## Next Steps

- **[Project Setup](./project-setup)**: Configure HIE files for full mode
- **[Analysis Modes](../usage-guide/analysis-modes)**: Choose the right mode
- **[Custom Rules](../rules/custom-rules)**: Write project-specific rules
