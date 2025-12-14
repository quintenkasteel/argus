---
sidebar_position: 7
title: Scopes Configuration
description: Apply different rules to different parts of your codebase
---

# Scopes Configuration

Scopes allow you to apply different rule configurations to different parts of your codebase. This is essential for handling test code, legacy code, generated files, and modules with different requirements.

## Basic Scope Definition

```toml
[[scopes]]
# Module patterns this scope applies to
modules = ["*Spec", "Test.*", "*Test*"]

# Rules to ignore in this scope
ignore = ["partial/*", "complexity/*"]
```

## Module Patterns

### Glob Patterns

| Pattern | Matches |
|---------|---------|
| `*Spec` | `MyModuleSpec`, `HandlerSpec` |
| `Test.*` | `Test.Unit`, `Test.Integration.Api` |
| `*.Internal.*` | `App.Internal.Types`, `Lib.Internal.Core` |
| `**/*.Generated` | Any nested `.Generated` module |

### Examples

```toml
# Test modules
[[scopes]]
modules = ["*Spec", "*Test", "Test.*"]

# Internal modules
[[scopes]]
modules = ["*.Internal", "*.Internal.*"]

# Generated code
[[scopes]]
modules = ["*.Generated", "Generated.*", "Paths_*"]

# Legacy code
[[scopes]]
modules = ["Legacy.*", "*.Legacy.*", "Old*"]

# API modules
[[scopes]]
modules = ["*.API", "*.API.*", "API.*"]
```

## Ignoring Rules

### Ignore Specific Rules

```toml
[[scopes]]
modules = ["*Spec"]
ignore = [
  "partial/head",
  "partial/tail",
  "complexity/cyclomatic",
]
```

### Ignore by Pattern

```toml
[[scopes]]
modules = ["*Spec"]
ignore = [
  "partial/*",      # All partial function rules
  "complexity/*",   # All complexity rules
  "modernize/*",    # All modernization rules
]
```

### Ignore All Rules

```toml
[[scopes]]
modules = ["Generated.*", "Paths_*"]
ignore = ["ALL"]  # Ignore all rules
```

## Severity Overrides

### Relax Severity

```toml
[[scopes]]
modules = ["*Spec", "Test.*"]

[scopes.severity]
"partial/*" = "info"        # Downgrade to info
"complexity/*" = "off"      # Disable entirely
"redundant/*" = "suggestion"
```

### Strict Severity

```toml
[[scopes]]
modules = ["*.API.*", "*.Public.*"]

[scopes.severity]
"partial/*" = "error"       # Upgrade to error
"security/*" = "error"
"performance/*" = "warning"
```

## Rule Configuration per Scope

### Complexity Thresholds

```toml
[[scopes]]
modules = ["*Spec"]

[scopes.complexity]
max-cyclomatic = 25        # Higher limit for tests
max-function-lines = 200   # Long test setups OK
max-nesting = 6           # More nesting allowed
```

### Naming Rules

```toml
[[scopes]]
modules = ["*.API.*"]

[scopes.naming]
min-variable = 3           # Require longer names
check-abbreviations = true # Enforce full words
```

## Common Scope Configurations

### Test Code

```toml
[[scopes]]
modules = ["*Spec", "*Test", "Test.*", "*Tests"]
ignore = [
  "partial/*",           # Partial functions OK in tests
  "complexity/*",        # Complex test setups OK
  "security/undefined",  # undefined OK for test stubs
  "naming/*",           # Relaxed naming in tests
]

[scopes.severity]
"redundant/*" = "off"   # Don't care about redundancy
```

### Generated Code

```toml
[[scopes]]
modules = ["Generated.*", "*.Generated.*", "Paths_*"]
ignore = ["ALL"]
```

### Legacy Code

```toml
[[scopes]]
modules = ["Legacy.*", "*.Legacy.*"]
ignore = [
  "modernize/*",        # Don't suggest modernization
  "complexity/*",       # Accept existing complexity
]

[scopes.severity]
"partial/*" = "warning"  # Warn but don't error
"security/*" = "error"   # Still enforce security
```

### Internal Modules

```toml
[[scopes]]
modules = ["*.Internal", "*.Internal.*"]

[scopes.severity]
"architecture/exposed-internal" = "off"  # OK to be internal
"naming/*" = "suggestion"                # Relaxed naming
```

### Public API

```toml
[[scopes]]
modules = ["*.API", "*.API.*", "*.Public.*"]

[scopes.severity]
"partial/*" = "error"
"security/*" = "error"
"naming/*" = "warning"

[scopes.complexity]
max-cyclomatic = 10    # Lower complexity limit
max-parameters = 4     # Fewer parameters

[scopes.naming]
min-variable = 3       # Longer names required
```

### Database/Persistence

```toml
[[scopes]]
modules = ["*.Database.*", "*.Persist.*", "*.Repository.*"]

[scopes.severity]
"security/sql-injection" = "error"
"performance/*" = "warning"

# Custom restrictions
[[scopes.restrictions.functions]]
name = "unsafePerformIO"
within = []
message = "Never use in database code"
```

### Handlers/Controllers

```toml
[[scopes]]
modules = ["*.Handler.*", "*.Controller.*"]

[scopes.severity]
"security/*" = "error"
"partial/*" = "error"

[scopes.complexity]
max-cyclomatic = 20   # Handlers can be complex
```

## Scope Priority

When multiple scopes match, rules are merged with later scopes taking priority:

```toml
# First: general test relaxation
[[scopes]]
modules = ["*Spec"]
ignore = ["partial/*"]

# Second: stricter for API tests
[[scopes]]
modules = ["*API*Spec"]
[scopes.severity]
"partial/*" = "warning"  # Override: warn in API tests
```

For `ApiHandlerSpec`:
1. Matches `*Spec` → ignore partial rules
2. Matches `*API*Spec` → but then set to warning
3. Result: partial rules are warnings

## Conditional Scopes

### Based on Imports

```toml
[[scopes]]
modules = ["*"]
has-import = ["Test.Hspec"]  # Modules importing Hspec
ignore = ["partial/*"]
```

### Based on Extensions

```toml
[[scopes]]
modules = ["*"]
has-extension = ["TemplateHaskell"]
ignore = ["complexity/*"]  # TH can be complex
```

## Custom Rules per Scope

### Add Scope-Specific Rules

```toml
[[scopes]]
modules = ["*.Handler.*"]

# Only in handlers
[[scopes.patterns.rules]]
name = "structured-logging"
match = "putStrLn"
severity = "warning"
message = "Use structured logging in handlers"

[[scopes.patterns.rules]]
name = "no-trace"
match = "trace"
severity = "error"
message = "No trace in handlers"
```

### Scope-Specific Restrictions

```toml
[[scopes]]
modules = ["*.Production.*"]

[[scopes.restrictions.functions]]
name = "trace"
within = []
message = "No trace in production code"

[[scopes.restrictions.functions]]
name = "debug"
within = []
message = "No debug in production code"
```

## Complete Example

```toml
# Default configuration applies everywhere
[rules]
enabled = true
severity = "warning"

[rules.severity]
"partial/*" = "error"
"security/*" = "error"

# === SCOPES ===

# Test code: relaxed
[[scopes]]
modules = ["*Spec", "*Test", "Test.*"]
ignore = [
  "partial/*",
  "complexity/*",
  "naming/*",
  "redundant/*",
]

# Generated code: ignore completely
[[scopes]]
modules = ["Generated.*", "Paths_*", "*.Generated"]
ignore = ["ALL"]

# Legacy code: warn but don't error
[[scopes]]
modules = ["Legacy.*", "*.Legacy.*"]
[scopes.severity]
"partial/*" = "warning"
"modernize/*" = "off"

# Public API: strict
[[scopes]]
modules = ["*.API.*", "*.Public.*"]
[scopes.severity]
"partial/*" = "error"
"security/*" = "error"
"naming/*" = "warning"

[scopes.complexity]
max-cyclomatic = 10
max-parameters = 4

# Handlers: security focused
[[scopes]]
modules = ["*.Handler.*"]

[[scopes.patterns.rules]]
name = "use-structured-logging"
match = "putStrLn"
severity = "warning"
message = "Use structured logging"

[[scopes.restrictions.functions]]
name = "unsafePerformIO"
within = []
message = "Never use in handlers"

# Database code: SQL security
[[scopes]]
modules = ["*.Database.*", "*.Repository.*"]
[scopes.severity]
"security/sql-injection" = "error"
```

## Debugging Scopes

```bash
# Show which scope applies to a file
argus check --show-scope src/Handler.hs

# List all scopes
argus config show-scopes

# Validate scope patterns
argus config validate --scopes
```

Output:

```
Scopes for src/Api/Handler.hs:
  1. [*.Handler.*] (handlers)
  2. [*.API.*] (public API)

Active configuration:
  - partial/*: error (from API scope)
  - security/*: error (from API scope)
  - complexity/cyclomatic: max 10 (from API scope)
```

## See Also

- **[Rules Configuration](./rules-section)**: Base rule settings
- **[Custom Rules](../rules/custom-rules)**: Defining custom rules
- **[Configuration File](./file-format)**: Full config reference
