---
sidebar_position: 3
title: Naming Configuration
description: Configure naming conventions for types, variables, and functions
---

# Naming Configuration

The `[naming]` section configures naming convention enforcement for types, variables, functions, and modules.

## Basic Configuration

```toml
[naming]
# Master enable/disable
enabled = true

# What to check
check-types = true
check-variables = true
check-functions = true
check-modules = true

# Default severity
severity = "warning"
```

## Type Name Mappings

Map semantic type aliases to standard patterns:

```toml
[[naming.types]]
pattern = "LocationId"
replacement = "Key Location"
severity = "warning"

[[naming.types]]
pattern = "UserId"
replacement = "Key User"

[[naming.types]]
pattern = "PageId"
replacement = "Key Page"
```

### Pattern Wildcards

Use `*` to match any sequence:

```toml
# Match any *Id pattern
[[naming.types]]
pattern = "*Id"
replacement = "Key *"
message = "Use Key type instead of Id suffix"

# Match *VO (Value Object)
[[naming.types]]
pattern = "*VO"
replacement = "*"
message = "Avoid VO suffix"
```

### Auto-Fix

Enable automatic fixing:

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
from = "*"           # Match any name
to = "locationK"     # Suggest this name
severity = "warning"

[[naming.variables]]
type = "Key User"
from = "*"
to = "userK"

[[naming.variables]]
type = "Key Page"
from = "*"
to = "pageK"
```

### Pattern Variables

Use wildcards in replacement:

```toml
# Key * -> *K
[[naming.variables]]
type = "Key *"
from = "*"
to = "*K"

# Entity * -> *Entity or *E
[[naming.variables]]
type = "Entity *"
from = "*"
to = "*E"

# Entity * * (key and value)
[[naming.variables]]
type = "Entity * *"
from = "Entity * *"
to = "Entity *K *V"
```

### Underscore Preservation

Leading underscores are preserved:

```haskell
-- _x :: Key Location
-- Suggestion: _locationK (underscore preserved)
```

## Function Naming

### Prefix Conventions

```toml
[[naming.functions]]
pattern = "get*"
context = "IO"
severity = "suggestion"
message = "Consider 'fetch*' for IO operations"

[[naming.functions]]
returns = "Bool"
pattern = "is*"
severity = "info"
message = "Boolean predicates should use 'is' or 'has' prefix"
```

### Unsafe Function Naming

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

[[naming.modules]]
pattern = "*.Internal"
severity = "info"
message = "Internal modules should not be imported directly"
```

## Case Conventions

### Type Names

```toml
[naming.conventions]
types = "PascalCase"           # MyType, UserData
type-variables = "lowercase"   # a, b, m, f
data-constructors = "PascalCase"
```

### Value Names

```toml
[naming.conventions]
variables = "camelCase"    # myVariable, userData
functions = "camelCase"    # myFunction, processData
```

### Module Names

```toml
[naming.conventions]
modules = "PascalCase"     # Data.Map, MyApp.Types
```

### Available Conventions

| Convention | Example |
|------------|---------|
| `PascalCase` | `MyType`, `UserData` |
| `camelCase` | `myVariable`, `getData` |
| `snake_case` | `my_variable`, `get_data` |
| `SCREAMING_SNAKE` | `MAX_SIZE`, `DEFAULT_VALUE` |
| `lowercase` | `a`, `m`, `xs` |

## Length Constraints

```toml
[naming.length]
# Variable name length
min-variable = 2
max-variable = 30

# Function name length
max-function = 40

# Type name length
max-type = 50

# Allowed single-letter names
allow-single-letter = ["x", "y", "n", "m", "a", "b", "f", "k", "v", "e", "i"]
```

### Context-Aware Length

```toml
[naming.length]
# Longer names for exported items
[naming.length.exported]
min-variable = 3
min-function = 3

# Short names OK for local bindings
[naming.length.local]
min-variable = 1
allow-single-letter = true
```

## Abbreviations

```toml
[naming.abbreviations]
# Allowed abbreviations
allowed = [
  "id", "db", "io", "url", "api",
  "http", "json", "xml", "sql",
  "ui", "cli", "env", "cfg", "ctx",
]

# Warn on unknown abbreviations
warn-unknown = true

# Custom abbreviation definitions
[naming.abbreviations.definitions]
"cfg" = "config"
"ctx" = "context"
"env" = "environment"
```

## Prefixes and Suffixes

### Required Prefixes

```toml
[[naming.prefixes]]
for = "handler"  # Functions handling requests
prefix = "handle"
message = "Handler functions should start with 'handle'"

[[naming.prefixes]]
for = "boolean"  # Boolean values
prefix = ["is", "has", "can", "should"]
message = "Boolean names should indicate true/false meaning"
```

### Required Suffixes

```toml
[[naming.suffixes]]
for = "monad-transformer"
suffix = "T"
message = "Monad transformers should end with 'T'"

[[naming.suffixes]]
for = "exception"
suffix = ["Exception", "Error"]
message = "Exceptions should end with 'Exception' or 'Error'"
```

## Domain-Specific Rules

### Web Application

```toml
[[naming.variables]]
type = "Request"
from = "*"
to = "req"

[[naming.variables]]
type = "Response"
from = "*"
to = "resp"

[[naming.variables]]
type = "Handler *"
from = "*"
to = "*Handler"
```

### Database Types

```toml
# Persistent-style
[[naming.types]]
pattern = "*Id"
replacement = "Key *"

[[naming.variables]]
type = "Key *"
from = "*"
to = "*K"

[[naming.variables]]
type = "Entity *"
from = "*"
to = "*E"
```

### Monad Transformers

```toml
[[naming.variables]]
type = "ReaderT * * *"
from = "*"
to = "*Reader"

[[naming.variables]]
type = "StateT * * *"
from = "*"
to = "*State"

[[naming.variables]]
type = "ExceptT * * *"
from = "*"
to = "*Except"
```

## Scope-Based Configuration

### Strict for API

```toml
[[scopes]]
modules = ["*.API.*", "*.Public.*"]
[scopes.naming]
min-variable = 3
check-abbreviations = true
severity = "warning"
```

### Relaxed for Tests

```toml
[[scopes]]
modules = ["*Spec", "Test.*"]
[scopes.naming]
enabled = false
```

### Legacy Code

```toml
[[scopes]]
modules = ["Legacy.*"]
[scopes.naming]
enabled = false  # Don't enforce on legacy
```

## Complete Example

```toml
[naming]
enabled = true
check-types = true
check-variables = true
check-functions = true
check-modules = true
severity = "warning"

# Type mappings
[[naming.types]]
pattern = "*Id"
replacement = "Key *"
fix = true

# Variable naming
[[naming.variables]]
type = "Key *"
from = "*"
to = "*K"

[[naming.variables]]
type = "Entity *"
from = "*"
to = "*E"

[[naming.variables]]
type = "Request"
from = "*"
to = "req"

[[naming.variables]]
type = "Response"
from = "*"
to = "resp"

# Function naming
[[naming.functions]]
uses = ["unsafePerformIO"]
pattern = "unsafe*"
severity = "warning"
message = "Unsafe functions need 'unsafe' prefix"

# Module naming
[[naming.modules]]
pattern = "*.Utils"
severity = "suggestion"
message = "Consider more specific module name"

# Conventions
[naming.conventions]
types = "PascalCase"
variables = "camelCase"
functions = "camelCase"
modules = "PascalCase"

# Length
[naming.length]
min-variable = 2
max-variable = 30
allow-single-letter = ["x", "y", "n", "m", "a", "b", "f", "k", "v"]

# Abbreviations
[naming.abbreviations]
allowed = ["id", "db", "io", "url", "api", "http", "json"]
warn-unknown = true
```

## Suppression

```haskell
-- Intentional short name
let x = compute y  -- argus:ignore naming/variable

-- Explain why
let x = compute y  -- argus:ignore naming/variable: math convention
```

## See Also

- **[Naming Rules](../rules/naming)**: Naming rule reference
- **[Rules Configuration](./rules-section)**: General rule settings
- **[Scopes Configuration](./scopes-section)**: Module-specific settings
