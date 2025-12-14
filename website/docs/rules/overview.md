---
sidebar_position: 1
title: Rules Overview
description: Understanding Argus's rule system and categories
---

# Rules Overview

Argus includes over 1,100 rules organized into 16 categories. This guide explains how the rule system works and how to configure it.

## Rule Architecture

### How Rules Work

Each rule consists of:

1. **Pattern**: What code to match
2. **Message**: Explanation of the issue
3. **Severity**: How serious the issue is
4. **Fix** (optional): Automatic fix
5. **Safety Level**: How safe the fix is

### Rule Identification

Rules are identified by category and name:

```
category/rule-name
```

Examples:
- `partial/head` - Partial function `head`
- `security/sql-injection` - SQL injection vulnerability
- `performance/length-null` - Using `length` for emptiness check

## Rule Categories

| Category | Count | Description |
|----------|-------|-------------|
| [`partial`](./partial-functions) | 12 | Partial function usage |
| [`security`](./security) | 8 | Security vulnerabilities |
| [`performance`](./performance) | 12 | Performance anti-patterns |
| [`space-leaks`](./space-leaks) | 5 | Space leak patterns |
| [`complexity`](./complexity) | 6 | Code complexity metrics |
| [`architecture`](./architecture) | 4 | Architectural violations |
| [`imports`](./imports) | 4 | Import hygiene |
| [`naming`](./naming) | Variable | Naming conventions |
| [`pragmas`](./pragmas) | 20+ | LANGUAGE pragma analysis |
| [`modernize`](./modernize) | 11 | Code modernization |
| [`redundant`](./redundant) | 8 | Redundant code |
| `style` | Variable | Style conventions |
| `correctness` | Variable | Correctness issues |
| `concurrency` | Variable | Concurrency issues |
| `error-handling` | Variable | Error handling |
| `custom` | Variable | User-defined rules |

## Severity Levels

| Severity | Meaning | Exit Code | CI Impact |
|----------|---------|-----------|-----------|
| `error` | Must fix | 1 | Fails build |
| `warning` | Should fix | 0 | Warning only |
| `suggestion` | Consider fixing | 0 | Info only |
| `info` | Informational | 0 | Info only |

## Configuring Rules

### Enabling/Disabling Rules

In `argus.toml`:

```toml
[rules]
# Disable specific rules
disabled = [
  "modernize/return-pure",
  "performance/boxing",
]

# Disable entire categories
disabled-categories = ["style"]
```

### Changing Severity

```toml
[rules.severity]
"partial/head" = "error"
"modernize/return-pure" = "info"
```

### Category-Level Configuration

```toml
[categories]
performance = "warn"
security = "error"
modernize = "suggestion"
```

## Inline Suppressions

Suppress rules directly in code:

```haskell
-- Single line suppression
let x = head items  -- argus:ignore partial/head

-- With reason
let x = fromJust val  -- argus:ignore partial/fromJust: validated above

-- Block suppression
-- argus:ignore-begin security/trace
debugFunction x = trace "debug" x
debugHelper y = traceShow y y
-- argus:ignore-end

-- Suppress multiple rules
let x = head (tail items)  -- argus:ignore partial/head,partial/tail
```

## Understanding Rule Output

```
src/MyModule.hs:42:5: warning: [partial/head]
    Use of partial function 'head': Crashes on empty list.
    Consider using 'headMay' from 'Safe' instead.
    |
 42 | let first = head items
    |             ^^^^
    Fix available: Replace with 'headMay items'
```

Components:
- **Location**: `src/MyModule.hs:42:5`
- **Severity**: `warning`
- **Rule ID**: `[partial/head]`
- **Message**: Explanation of the issue
- **Context**: Source code with highlight
- **Fix**: Available automatic fix

## Rule Metadata

Each rule includes metadata:

```toml
[[rules.custom]]
id = "performance/length-null"
category = "performance"
severity = "warning"
safety = "safe"          # Fix safety level
message = "Use null for O(1) emptiness check"
pattern = "length $X == 0"
fix = "null $X"
note = "null is O(1), length is O(n)"
```

### Safety Levels

| Level | Meaning | Auto-Applied |
|-------|---------|--------------|
| `safe` | Always safe | Yes |
| `mostly-safe` | Usually safe | With config |
| `needs-review` | Review recommended | No |
| `manual` | No auto-fix | No |

## Builtin vs Custom Rules

### Builtin Rules

Argus ships with 39 builtin rule modules:

```
Safety       Performance    Security       Style
Correctness  Brackets       TypeAnnotations CodeSmells
Lambda       Monadic        ListRecursion   TypeclassLaws
Foldable     Numeric        Strings         Records
Prelude      IO             Containers      Transformers
Testing      FFI            TypeFamilies    GADTs
Deriving     Lenses         Async           List
Maybe        Either         Boolean         Comparison
Tuple        Composition    Arrow           Monoid
Pattern      Imports        OWASP
```

### Custom Rules

Define project-specific rules in `argus.toml`:

```toml
[[patterns.rules]]
name = "avoid-direct-db"
match = "runDB"
severity = "error"
message = "Use repository pattern instead of direct DB access"
```

See [Custom Rules](./custom-rules) for the full DSL.

## Rule Statistics

Running Argus reports statistics:

```bash
argus check src/ --stats
```

```
Rule Statistics:
  Total rules evaluated: 1,104
  Rules triggered: 47
  By category:
    partial: 15
    performance: 12
    security: 8
    modernize: 7
    imports: 5
  By severity:
    error: 3
    warning: 32
    suggestion: 12
```

## Common Configuration Patterns

### Strict for Security

```toml
[categories]
security = "error"

[rules.severity]
"partial/fromJust" = "error"
"security/trace" = "error"
```

### Relaxed for Development

```toml
[categories]
modernize = "info"
style = "off"

[rules]
disabled = ["complexity/*"]
```

### Team Standards

```toml
# Naming conventions
[naming]
enabled = true

[[naming.types]]
pattern = "*Id"
replacement = "Key *"
severity = "warning"

# Required qualified imports
[imports]
suggest-qualified = ["Data.Map", "Data.Text", "Data.ByteString"]
```

## Next Steps

Explore each rule category:

- **[Partial Functions](./partial-functions)**: Safe alternatives to partial functions
- **[Security](./security)**: Security vulnerability detection
- **[Performance](./performance)**: Performance anti-patterns
- **[Space Leaks](./space-leaks)**: Lazy evaluation issues
- **[Complexity](./complexity)**: Code complexity metrics
- **[Custom Rules](./custom-rules)**: Writing your own rules
