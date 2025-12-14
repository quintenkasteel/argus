---
sidebar_position: 2
title: Rules Configuration
description: Configure rule enabling, severity, and behavior
---

# Rules Configuration

The `[rules]` section controls which rules are enabled, their severity levels, and specific rule behavior.

## Basic Configuration

```toml
[rules]
# Master switch - disable all rules
enabled = true

# Default minimum severity to report
severity = "warning"

# Exit with error code at this level
fail-on = "error"
```

## Enabling and Disabling Rules

### Disable Specific Rules

```toml
[rules]
disabled = [
  "redundant/dollar-simple",
  "modernize/return-pure",
  "redundant/parens",
]
```

### Disable by Pattern

```toml
[rules]
disabled = [
  "modernize/*",      # All modernize rules
  "redundant/*",      # All redundant rules
  "*/dollar*",        # Any rule containing "dollar"
]
```

### Enable Only Specific Rules

```toml
[rules]
# When set, ONLY these rules are enabled
enabled-only = [
  "partial/*",
  "security/*",
  "performance/space-leak",
]
```

### Enable by Category

```toml
[rules]
# Enable specific categories
categories = [
  "partial",
  "security",
  "performance",
]
```

## Severity Levels

Available severity levels (highest to lowest):

| Level | Use Case |
|-------|----------|
| `error` | Must fix, blocks CI |
| `warning` | Should fix |
| `suggestion` | Consider fixing |
| `info` | Informational only |

### Default Severity

```toml
[rules]
# Minimum severity to report
severity = "warning"  # Hide info and suggestion

# Or show everything
severity = "info"
```

### Per-Rule Severity

```toml
[rules.severity]
# Treat partial functions as errors
"partial/*" = "error"
"partial/head" = "error"
"partial/tail" = "error"

# Security issues are errors
"security/*" = "error"

# Performance is warning
"performance/*" = "warning"

# Modernization is just suggestion
"modernize/*" = "suggestion"

# Redundant code is info
"redundant/*" = "info"
```

### Category Severity

```toml
[categories]
partial = "error"
security = "error"
performance = "warning"
modernize = "suggestion"
redundant = "info"
complexity = "warning"
```

## CI Exit Codes

```toml
[rules]
# Exit with error (code 1) if issues at this level or above
fail-on = "error"      # Only fail on errors
fail-on = "warning"    # Fail on warnings or errors
fail-on = "suggestion" # Fail on any issue
```

### Warnings as Errors

```toml
[rules]
# Treat all warnings as errors
warnings-as-errors = true
```

## Rule Categories

### Built-in Categories

| Category | Description |
|----------|-------------|
| `partial` | Partial function usage |
| `security` | Security vulnerabilities |
| `performance` | Performance issues |
| `space-leak` | Memory/space leaks |
| `complexity` | Code complexity |
| `architecture` | Architectural issues |
| `import` | Import problems |
| `naming` | Naming conventions |
| `extension` | LANGUAGE pragmas |
| `modernize` | Code modernization |
| `redundant` | Redundant code |
| `custom` | Custom rules |

### Configure by Category

```toml
[categories]
# Enable/disable entire categories
partial = true
security = true
performance = true
modernize = false  # Disable modernization rules

# Or set severity
partial = "error"
security = "error"
modernize = "off"  # Same as disabled
```

## Specific Rule Options

Some rules have additional configuration:

### Partial Functions

```toml
[rules.partial]
# Allow these partial functions
allowed = ["error"]  # error is sometimes acceptable

# In specific modules
allowed-in = {
  "head" = ["*.Spec", "Test.*"],
  "tail" = ["*.Spec", "Test.*"],
}
```

### Complexity Rules

```toml
[rules.complexity]
# Override thresholds for specific rules
cyclomatic-threshold = 15
cognitive-threshold = 20

# Per-module overrides
[rules.complexity.overrides]
"Api.Handler" = { cyclomatic = 25 }  # Complex handler
```

### Import Rules

```toml
[rules.import]
# Require qualified imports for these modules
require-qualified = [
  "Data.Map",
  "Data.Set",
  "Data.Text",
]

# Preferred qualification aliases
[rules.import.as]
"Data.Map" = "Map"
"Data.Map.Strict" = "Map"
"Data.Text" = "T"
"Data.ByteString" = "BS"
```

## Rule Documentation

```toml
[rules]
# Include documentation URLs in output
include-docs = true

# Custom documentation base URL
docs-url = "https://my-org.github.io/argus-rules"
```

## Rule Metadata

Access rule information:

```bash
# List all rules
argus rules list

# Show rule details
argus rules show partial/head

# List rules by category
argus rules list --category security
```

## Complete Example

```toml
[rules]
# Enable rules
enabled = true

# Report warnings and above
severity = "warning"

# CI fails on errors
fail-on = "error"

# Disabled rules
disabled = [
  "redundant/dollar-simple",  # Team prefers $ style
  "redundant/parens",         # Extra parens OK for clarity
]

# Per-rule severity overrides
[rules.severity]
# Critical - must fix
"partial/*" = "error"
"security/*" = "error"
"performance/space-leak" = "error"

# Important - should fix
"performance/*" = "warning"
"architecture/*" = "warning"
"complexity/*" = "warning"

# Nice to have
"modernize/*" = "suggestion"
"redundant/*" = "suggestion"

# Informational
"extension/unused" = "info"

# Category settings
[categories]
partial = "error"
security = "error"

# Partial function configuration
[rules.partial]
allowed-in = {
  "head" = ["*Spec", "Test.*"],
  "tail" = ["*Spec", "Test.*"],
}

# Import configuration
[rules.import]
require-qualified = ["Data.Map", "Data.Set"]

[rules.import.as]
"Data.Map" = "Map"
"Data.Set" = "Set"
```

## Inheritance and Overrides

### Project vs User Config

User config (`~/.config/argus/config.toml`) is applied first, then project config overrides:

```toml
# ~/.config/argus/config.toml (user defaults)
[rules]
severity = "warning"

[rules.severity]
"security/*" = "error"
```

```toml
# project/argus.toml (project overrides)
[rules]
# This overrides user config
disabled = ["modernize/*"]

# This merges with user config
[rules.severity]
"partial/*" = "error"  # Added to user settings
```

### Scoped Overrides

Override rules for specific modules:

```toml
# Test files: relax rules
[[scopes]]
modules = ["*Spec", "Test.*"]
[scopes.rules]
disabled = ["partial/*", "complexity/*"]

# API modules: strict rules
[[scopes]]
modules = ["*.API.*"]
[scopes.rules.severity]
"partial/*" = "error"
"security/*" = "error"
```

## Command-Line Overrides

Override configuration from command line:

```bash
# Enable only specific rules
argus check --rules "partial/*,security/*" src/

# Override severity
argus check --severity error src/

# Override fail-on
argus check --fail-on warning src/

# Disable rules
argus check --exclude-rules "modernize/*" src/
```

## See Also

- **[Rules Overview](../rules/overview)**: All available rules
- **[Scopes Configuration](./scopes-section)**: Module-specific rules
- **[Custom Rules](../rules/custom-rules)**: Define new rules
- **[CLI Reference](../cli-reference/check)**: Command-line options
