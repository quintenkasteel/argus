---
sidebar_position: 8
title: Extensions Configuration
description: Configure LANGUAGE extension analysis and recommendations
---

# Extensions Configuration

The `[extensions]` section configures how Argus analyzes LANGUAGE pragmas, detects unused extensions, and suggests appropriate extensions.

## Basic Configuration

```toml
[extensions]
# Enable extension analysis
enabled = true

# Check for unused extensions
check-unused = true

# Check for missing extensions
check-missing = true

# Check for redundant (implied) extensions
check-redundant = true

# Warn about dangerous extensions
check-dangerous = true
```

## Unused Extension Detection

```toml
[extensions]
check-unused = true

# Severity for unused extension warnings
unused-severity = "warning"

# Extensions to never warn about (always allow)
allow-unused = [
  "NoImplicitPrelude",  # May be project-wide
  "StrictData",         # Affects all data types
]
```

### Detectable Extensions

Argus can reliably detect usage of these extensions:

| Extension | Detection Method |
|-----------|------------------|
| `LambdaCase` | `\case` syntax |
| `RecordWildCards` | `{..}` patterns |
| `TypeApplications` | `@Type` syntax |
| `BangPatterns` | `!pattern` |
| `ViewPatterns` | `(view -> pat)` |
| `MultiWayIf` | `if | ...` syntax |
| `TupleSections` | `(,x)` patterns |
| `OverloadedStrings` | String literal types |
| `TemplateHaskell` | `$()`, `[||]` |
| `QuasiQuotes` | `[name|...|]` |
| `GADTs` | GADT syntax |
| `TypeFamilies` | `type family` |

## Missing Extension Detection

```toml
[extensions]
check-missing = true

# Severity for missing extension errors
missing-severity = "error"

# Don't suggest these extensions
never-suggest = [
  "UndecidableInstances",
  "IncoherentInstances",
]
```

## Redundant Extension Detection

Some extensions imply others:

```toml
[extensions]
check-redundant = true
redundant-severity = "suggestion"
```

### Extension Implications

| Extension | Implies |
|-----------|---------|
| `GADTs` | `GADTSyntax`, `MonoLocalBinds` |
| `TypeFamilies` | `MonoLocalBinds`, `KindSignatures` |
| `RankNTypes` | `ExplicitForAll` |
| `ScopedTypeVariables` | `ExplicitForAll` |
| `FlexibleInstances` | `TypeSynonymInstances` |

## Dangerous Extension Warnings

```toml
[extensions]
check-dangerous = true
dangerous-severity = "warning"

# Override severity for specific extensions
[extensions.dangerous]
"UndecidableInstances" = "warning"
"IncoherentInstances" = "error"
"Unsafe" = "error"
```

### Dangerous Extensions

| Extension | Risk |
|-----------|------|
| `UndecidableInstances` | Non-terminating type checking |
| `IncoherentInstances` | Unpredictable instance selection |
| `OverlappingInstances` | Import-order dependent |
| `ImpredicativeTypes` | Limited support, bugs |
| `Unsafe` | Bypasses safety checks |

## Suggested Default Extensions

```toml
[extensions]
# Suggest these as safe defaults
suggest-defaults = [
  "BangPatterns",
  "DeriveGeneric",
  "DerivingStrategies",
  "GeneralizedNewtypeDeriving",
  "LambdaCase",
  "OverloadedStrings",
  "RecordWildCards",
  "ScopedTypeVariables",
  "TypeApplications",
]

# Severity for default suggestions
suggest-defaults-severity = "suggestion"

# Only suggest if used in N% of files
suggest-threshold = 0.5
```

## Global vs Per-File Extensions

```toml
[extensions]
# Suggest moving to package.yaml if used in N% of files
global-threshold = 0.7

# Message when suggesting global
global-message = "Consider adding to default-extensions in package.yaml"
```

Example output:

```
Suggestion: Consider enabling LambdaCase globally
  Used in: 45 out of 50 files (90%)
  Add to: package.yaml default-extensions
```

## Required Extensions

Require certain extensions in all files:

```toml
[extensions]
# These must be present (or in default-extensions)
required = [
  "StrictData",
  "DerivingStrategies",
]

required-severity = "error"
required-message = "Project requires {extension} to be enabled"
```

## Forbidden Extensions

Ban certain extensions entirely:

```toml
[extensions]
# These are never allowed
forbidden = [
  "IncoherentInstances",
  "Unsafe",
]

forbidden-severity = "error"
```

## Scoped Extension Rules

Different rules for different modules:

```toml
# Allow TemplateHaskell only in TH modules
[[scopes]]
modules = ["*"]

[[scopes.extensions.restrictions]]
name = "TemplateHaskell"
allowed-in = ["*.TH", "TH.*", "*.Quasi"]
message = "TemplateHaskell only in TH modules"

# Allow UndecidableInstances in type-level code
[[scopes]]
modules = ["*.Types.*", "*.TypeLevel.*"]

[scopes.extensions]
allow = ["UndecidableInstances"]
```

## Extension Groups

Define groups of related extensions:

```toml
[extensions.groups]
# Deriving extensions
deriving = [
  "DeriveGeneric",
  "DeriveFunctor",
  "DeriveFoldable",
  "DeriveTraversable",
  "DeriveAnyClass",
  "DerivingStrategies",
  "DerivingVia",
]

# Type-level programming
type-level = [
  "TypeFamilies",
  "DataKinds",
  "TypeOperators",
  "GADTs",
  "RankNTypes",
]

# Records
records = [
  "RecordWildCards",
  "NamedFieldPuns",
  "DuplicateRecordFields",
  "OverloadedRecordDot",
]
```

Use groups in configuration:

```toml
[extensions]
suggest-defaults = ["@deriving", "LambdaCase"]

[[scopes]]
modules = ["*.Types.*"]
[scopes.extensions]
allow = ["@type-level"]
```

## Auto-Fix

```toml
[extensions.fix]
# Remove unused extensions
remove-unused = true

# Add missing extensions
add-missing = true

# Remove redundant extensions
remove-redundant = true

# Where to add new extensions: start, end, alphabetical
placement = "alphabetical"
```

## Complete Example

```toml
[extensions]
# Enable all checks
enabled = true
check-unused = true
check-missing = true
check-redundant = true
check-dangerous = true

# Severities
unused-severity = "warning"
missing-severity = "error"
redundant-severity = "suggestion"
dangerous-severity = "warning"

# Safe defaults to suggest
suggest-defaults = [
  "BangPatterns",
  "DeriveGeneric",
  "DerivingStrategies",
  "GeneralizedNewtypeDeriving",
  "LambdaCase",
  "OverloadedStrings",
  "ScopedTypeVariables",
  "TypeApplications",
]

# Global threshold
global-threshold = 0.7

# Required everywhere
required = ["StrictData", "DerivingStrategies"]
required-severity = "error"

# Forbidden everywhere
forbidden = ["IncoherentInstances", "Unsafe"]

# Allow these even if unused
allow-unused = ["NoImplicitPrelude", "StrictData"]

# Dangerous extension overrides
[extensions.dangerous]
"UndecidableInstances" = "warning"
"IncoherentInstances" = "error"

# Extension groups
[extensions.groups]
deriving = [
  "DeriveGeneric",
  "DeriveFunctor",
  "DeriveAnyClass",
  "DerivingStrategies",
]

# Auto-fix settings
[extensions.fix]
remove-unused = true
add-missing = true
placement = "alphabetical"

# Scope: TH modules
[[scopes]]
modules = ["*.TH", "TH.*"]
[scopes.extensions]
allow = ["TemplateHaskell", "QuasiQuotes"]

# Scope: Type-level modules
[[scopes]]
modules = ["*.Types.*", "*.TypeLevel.*"]
[scopes.extensions]
allow = ["UndecidableInstances", "TypeFamilies"]
```

## Command-Line

```bash
# Check extensions only
argus check --rules "extension/*" src/

# Fix unused extensions
argus fix --rules "extension/unused" src/

# List extensions usage
argus extensions list src/
```

## See Also

- **[Pragma Rules](../rules/pragmas)**: Extension rule reference
- **[Rules Configuration](./rules-section)**: General rule settings
- **[Scopes Configuration](./scopes-section)**: Module-specific rules
