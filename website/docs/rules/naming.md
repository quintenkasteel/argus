---
sidebar_position: 9
title: Naming Rules
description: Enforce consistent naming conventions
---

# Naming Rules

Argus enforces naming conventions for types and variables based on configurable patterns.

## Type Naming

### Type Mappings

Map semantic type aliases to standard patterns:

```toml
[naming]
enabled = true

[[naming.types]]
pattern = "LocationId"
replacement = "Key Location"
severity = "warning"

[[naming.types]]
pattern = "UserId"
replacement = "Key User"
severity = "warning"

[[naming.types]]
pattern = "PageId"
replacement = "Key Page"
severity = "warning"
```

Detection:

```haskell
-- Warning: Use 'Key Location' instead of 'LocationId'
getLocation :: LocationId -> IO Location
```

### Pattern Syntax

| Pattern | Matches |
|---------|---------|
| `LocationId` | Exact match |
| `*Id` | `UserId`, `PageId`, `OrderId` |
| `*_id` | `user_id`, `page_id` |

### With Auto-Fix

```toml
[[naming.types]]
pattern = "LocationId"
replacement = "Key Location"
fix = true  # Enable auto-fix
```

## Variable Naming

### By Type

Name variables based on their type:

```toml
[[naming.variables]]
type = "Key Location"
from = "*"          # Match any current name
to = "locationK"    # Suggested name
severity = "warning"

[[naming.variables]]
type = "Key User"
from = "*"
to = "userK"
severity = "warning"
```

Detection:

```haskell
-- Warning: Variable of type 'Key Location' should be named 'locationK'
getLocation :: Key Location -> IO Location
getLocation lid = ...  -- 'lid' doesn't match pattern
```

### Pattern Variables

Use wildcards in naming patterns:

```toml
[[naming.variables]]
type = "Entity Page"
from = "Entity * *"     # Pattern with wildcards
to = "Entity pK pV"     # Replacement with wildcards
severity = "warning"
```

This suggests:
- `Entity PageKey PageValue` should use `pK` and `pV`
- `Entity k v` should become `Entity pK pV`

### Underscore Preservation

When a variable starts with `_`, the underscore is preserved:

```haskell
-- Original: _x :: Key Location
-- Suggested: _locationK
```

## Common Patterns

### Key Types (Persistent)

```toml
[[naming.types]]
pattern = "*Id"
replacement = "Key *"
severity = "suggestion"

[[naming.variables]]
type = "Key *"
from = "*"
to = "*K"
```

### Entity Types

```toml
[[naming.variables]]
type = "Entity *"
from = "*"
to = "*E"

[[naming.variables]]
type = "Entity * *"
from = "Entity * *"
to = "Entity *K *V"
```

### Handler Types

```toml
[[naming.variables]]
type = "Handler *"
from = "*"
to = "*Handler"
```

### Monad Types

```toml
[[naming.variables]]
type = "ReaderT * * *"
from = "*"
to = "*Reader"

[[naming.variables]]
type = "StateT * * *"
from = "*"
to = "*State"
```

## Function Naming

### Prefix Conventions

```toml
[[naming.functions]]
pattern = "get*"
context = "IO"  # Only in IO context
severity = "suggestion"
message = "Consider 'fetch*' for IO operations"

[[naming.functions]]
pattern = "is*"
returns = "Bool"
severity = "suggestion"
message = "Boolean predicates should start with 'is' or 'has'"
```

### Unsafe Functions

```toml
[[naming.functions]]
uses = ["unsafePerformIO", "unsafeCoerce"]
pattern = "unsafe*"
severity = "warning"
message = "Functions using unsafe operations should have 'unsafe' prefix"
```

## Module Naming

```toml
[[naming.modules]]
pattern = "*.Impl"
severity = "warning"
message = "Avoid 'Impl' suffix - use descriptive names"

[[naming.modules]]
pattern = "*.Utils"
severity = "suggestion"
message = "Consider more specific module names than 'Utils'"
```

## Case Conventions

### Type Names

```toml
[naming.conventions]
types = "PascalCase"      # MyType, UserData
type-variables = "lowercase"  # a, b, m
```

### Variable Names

```toml
[naming.conventions]
variables = "camelCase"   # myVariable, userData
functions = "camelCase"   # myFunction, processData
```

### Module Names

```toml
[naming.conventions]
modules = "PascalCase"    # Data.Map, MyApp.Types
```

## Length Constraints

```toml
[naming.length]
min-variable = 2          # No single-letter variables (except common ones)
max-variable = 30
max-function = 40
max-type = 50

# Exceptions for common single-letter variables
allow-single-letter = ["x", "y", "n", "m", "a", "b", "f", "k", "v"]
```

## Abbreviation Rules

```toml
[naming.abbreviations]
# Define allowed abbreviations
allowed = ["id", "db", "io", "url", "api", "http", "json", "xml"]

# Warn on unknown abbreviations
warn-unknown = true
```

## Scope-Based Rules

Different rules for different contexts:

```toml
# Stricter naming in public API
[[scopes]]
modules = ["*.API.*", "*.Public.*"]
[scopes.naming]
min-variable = 3  # No abbreviations

# Relaxed in tests
[[scopes]]
modules = ["*Spec", "Test.*"]
[scopes.naming]
enabled = false
```

## Examples

### Web Application

```toml
[naming]
enabled = true

# Key types
[[naming.types]]
pattern = "*Id"
replacement = "Key *"

# Variables by type
[[naming.variables]]
type = "Key *"
from = "*"
to = "*K"

[[naming.variables]]
type = "Entity *"
from = "*"
to = "*Entity"

# Request/Response
[[naming.variables]]
type = "Request"
from = "*"
to = "req"

[[naming.variables]]
type = "Response"
from = "*"
to = "resp"
```

### Domain-Driven Design

```toml
[naming]
enabled = true

# Aggregate roots
[[naming.types]]
pattern = "*Aggregate"
message = "Use '*Root' for aggregate roots"
replacement = "*Root"

# Value objects
[[naming.types]]
pattern = "*VO"
message = "Use full name for value objects"
```

## Suppression

```haskell
-- Intentional short name
let x = compute y  -- argus:ignore naming/variable

-- Or document why
let x = compute y  -- argus:ignore naming/variable: math convention
```

## Configuration

```toml
[naming]
enabled = true
check-types = true
check-variables = true
check-functions = true
check-modules = true

# Global severity
severity = "suggestion"
```

## Next Steps

- **[Pragmas Rules](./pragmas)**: LANGUAGE extension analysis
- **[Configuration](../configuration/naming-section)**: Full naming configuration
- **[Custom Rules](./custom-rules)**: Create custom naming rules
