---
sidebar_position: 13
title: Custom Rules
description: Define project-specific rules with the Argus DSL
---

# Custom Rules

Argus provides a powerful DSL for defining project-specific rules. This guide covers creating custom pattern rules, restrictions, and scoped configurations.

## Pattern Rules

### Basic Pattern

```toml
[[patterns.rules]]
name = "avoid-head"
match = "head"
fix = "headMay"
severity = "warning"
message = "Use headMay instead of partial head"
```

When Argus sees `head items`, it warns and suggests `headMay items`.

### Pattern Variables

Use metavariables to match expressions:

| Variable | Matches |
|----------|---------|
| `$X`, `$Y`, `$Z` | Any single expression |
| `$F`, `$G` | Functions |
| `$N` | Numeric literals |
| `$T` | Types |
| `$FUNC` | Function names |
| `$_` or `___` | Anything (ignored) |

```toml
[[patterns.rules]]
name = "length-null"
match = "length $X == 0"
fix = "null $X"
severity = "warning"
message = "Use null for O(1) emptiness check"
```

Matches:
- `length items == 0`
- `length (getList x) == 0`
- `length myList == 0`

### Multiple Patterns

```toml
[[patterns.rules]]
name = "avoid-partial-head"
patterns = [
  "head $X",
  "Prelude.head $X",
  "Data.List.head $X"
]
fix = "headMay $X"
message = "Use headMay from Safe"
```

### Pattern with Function Application

```toml
[[patterns.rules]]
name = "head-sort"
match = "head (sort $X)"
fix = "minimum $X"
message = "Use minimum instead of head . sort"

[[patterns.rules]]
name = "head-sort-compose"
match = "head . sort"
fix = "minimum"
message = "Use minimum instead of head . sort"
```

## Rule Properties

### Required Properties

| Property | Description |
|----------|-------------|
| `name` | Unique rule identifier |
| `match` or `patterns` | Pattern(s) to match |
| `message` | Warning message |

### Optional Properties

| Property | Description | Default |
|----------|-------------|---------|
| `fix` | Replacement pattern | None |
| `severity` | `error`, `warning`, `suggestion`, `info` | `warning` |
| `category` | Rule category | `custom` |
| `safety` | Fix safety level | `safe` |
| `note` | Additional information | None |
| `enabled` | Enable/disable rule | `true` |

### Fix Safety Levels

```toml
[[patterns.rules]]
name = "foldl-strict"
match = "foldl $F $Z $X"
fix = "foldl' $F $Z $X"
safety = "mostly-safe"
note = "Requires importing Data.List (foldl')"
```

| Level | When to Use |
|-------|-------------|
| `safe` | Always safe, no semantic change |
| `mostly-safe` | Safe in most cases |
| `needs-review` | May need human review |
| `manual` | No auto-fix available |

## Import Management

### Adding Imports

```toml
[[patterns.rules]]
name = "use-headMay"
match = "head $X"
fix = "headMay $X"
add-import = { module = "Safe", names = ["headMay"] }
```

### Qualified Imports

```toml
[[patterns.rules]]
name = "use-text-unpack"
match = "unpack $X"
fix = "T.unpack $X"
add-import = { module = "Data.Text", qualified = true, as = "T" }
```

## Side Conditions

### Type Conditions

```toml
[[patterns.rules]]
name = "string-to-text"
match = "$F :: String"
conditions = [
  { type = "has-type", value = "String" }
]
severity = "suggestion"
message = "Consider using Text instead of String"
```

### Context Conditions

```toml
[[patterns.rules]]
name = "io-trace"
match = "trace $MSG $X"
conditions = [
  { type = "in-module", pattern = "*.Production.*" }
]
severity = "error"
message = "No trace in production code"

[[patterns.rules]]
name = "trace-allowed-in-debug"
match = "trace $MSG $X"
conditions = [
  { type = "in-module", pattern = "*.Debug.*" }
]
severity = "info"
message = "Trace allowed in debug modules"
```

### Available Conditions

| Condition | Description |
|-----------|-------------|
| `has-type` | Expression has specific type |
| `has-typeclass` | Type has typeclass instance |
| `in-module` | Within matching module |
| `not-in-module` | Not in matching module |
| `in-function` | Within specific function |
| `has-import` | Module imports something |
| `has-pragma` | File has LANGUAGE pragma |

## Function Restrictions

### Ban Functions

```toml
[[restrictions.functions]]
name = "unsafePerformIO"
within = []  # Banned everywhere
message = "unsafePerformIO is forbidden"

[[restrictions.functions]]
name = "head"
within = ["*.Spec", "Test.*"]  # Only in tests
message = "Use headMay instead of head"
```

### Allow in Specific Modules

```toml
[[restrictions.functions]]
name = "trace"
within = ["*.Debug", "*.Tracing"]
message = "trace only allowed in debug modules"
```

## Module Restrictions

### Require Qualified Import

```toml
[[restrictions.modules]]
names = ["Data.Map", "Data.Map.Strict", "Data.Map.Lazy"]
as = "Map"
message = "Import Data.Map qualified as Map"

[[restrictions.modules]]
names = ["Data.Text"]
as = "T"
message = "Import Data.Text qualified as T"
```

## Extension Restrictions

```toml
[[restrictions.extensions]]
name = "TemplateHaskell"
within = ["*.TH", "TH.*"]
message = "TemplateHaskell only in TH modules"

[[restrictions.extensions]]
name = "UndecidableInstances"
within = []  # Banned
message = "UndecidableInstances is forbidden"
```

## Scoped Configurations

Apply different rules to different parts of your codebase:

### Relax Rules in Tests

```toml
[[scopes]]
modules = ["*.Spec", "Test.*", "*Test*"]
ignore = [
  "partial/head",
  "partial/tail",
  "security/undefined",
  "complexity/*"
]
```

### Strict Rules for API

```toml
[[scopes]]
modules = ["*.API.*", "*.Public.*"]
[scopes.severity]
"partial/*" = "error"
"security/*" = "error"
```

### Ignore Generated Code

```toml
[[scopes]]
modules = ["Generated.*", "**/Generated/**", "Paths_*"]
ignore = ["ALL"]
```

## DSL Examples

### Domain-Specific Rules

```toml
# E-commerce project
[[patterns.rules]]
name = "use-money-type"
match = "price :: Double"
severity = "error"
message = "Use Money type for prices, not Double"

[[patterns.rules]]
name = "validate-email"
match = "email :: String"
severity = "warning"
message = "Consider using Email newtype"
```

### Framework-Specific Rules

```toml
# Servant API
[[patterns.rules]]
name = "servant-handler-return"
match = "throwError"
conditions = [
  { type = "in-module", pattern = "*.Handler.*" }
]
severity = "info"
message = "Consider using ServerError constructors"

# Yesod
[[patterns.rules]]
name = "yesod-redirect"
match = "redirect $URL"
severity = "info"
message = "Prefer typed routes over string URLs"
```

### Company Standards

```toml
# Database access
[[patterns.rules]]
name = "no-raw-sql"
match = "rawSql"
within = ["*.Repository.*"]
severity = "error"
message = "Use Esqueleto or Persistent, not raw SQL"

# Logging
[[patterns.rules]]
name = "structured-logging"
match = "putStrLn"
within = ["*.Service.*", "*.Handler.*"]
severity = "warning"
message = "Use structured logging (logInfo, logError)"
```

## Testing Custom Rules

### Check Rule Matches

```bash
# Test on specific file
argus check --rules my-custom-rule src/Test.hs -v
```

### Debug Pattern Matching

```bash
# Verbose pattern matching output
argus check --debug-patterns src/
```

## Loading Custom Rules

### From Config File

Rules in `argus.toml` are automatically loaded.

### From Separate File

```toml
# argus.toml
[rules]
include = ["rules/security.toml", "rules/company.toml"]
```

### From Directory

```toml
[rules]
include-dir = "rules/"
```

## Complete Example

```toml
# Company-wide Argus rules

# Security
[[patterns.rules]]
name = "no-raw-sql"
patterns = ["rawSql $Q", "rawExecute $Q"]
severity = "error"
message = "Use type-safe database queries"
category = "security"

[[patterns.rules]]
name = "no-hardcoded-secrets"
match = "password = $LITERAL"
severity = "error"
message = "No hardcoded passwords"
category = "security"

# Performance
[[patterns.rules]]
name = "prefer-text"
match = "$F :: String -> String"
severity = "suggestion"
message = "Consider Text for text processing"
category = "performance"

# Domain
[[patterns.rules]]
name = "use-money"
match = "amount :: Double"
severity = "warning"
message = "Use Money type for amounts"
category = "domain"

# Restrictions
[[restrictions.functions]]
name = "unsafePerformIO"
within = []
message = "Never use unsafePerformIO"

[[restrictions.modules]]
names = ["Data.Map"]
as = "Map"

# Scopes
[[scopes]]
modules = ["Test.*", "*.Spec"]
ignore = ["partial/*", "performance/*"]
```

## Next Steps

- **[Rules Overview](./overview)**: All built-in rules
- **[Configuration](../configuration/rules-section)**: Full rules configuration
- **[Architecture](../architecture/rule-engine)**: How the rule engine works
