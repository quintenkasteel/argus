---
sidebar_position: 8
title: Import Rules
description: Maintain clean and organized imports
---

# Import Rules

Argus analyzes imports to find unused imports, suggest qualified imports, and help maintain clean import organization.

## Unused Import Detection

### Quick Mode (Heuristic)

In quick mode, Argus uses heuristics to detect unused imports:

```haskell
import Data.Map (lookup, insert, delete)  -- Warning: 'delete' unused

myFunc m = lookup "key" (insert "k" "v" m)
```

### Full Mode (Precise)

In full mode with HIE files, detection is precise:

```haskell
import Data.Map (*)  -- Warning: only 'lookup' and 'insert' used

myFunc m = lookup "key" (insert "k" "v" m)
```

**Rule:** `imports/unused`

### Configuration

```toml
[imports]
remove-unused = true
```

### Auto-Fix

```bash
argus fix src/ --categories imports
```

```diff
-import Data.Map (lookup, insert, delete)
+import Data.Map (lookup, insert)
```

## Qualified Import Suggestions

### Why Qualified Imports?

Some modules export names that clash with Prelude or each other:

```haskell
import Data.Map (lookup)   -- Shadows Prelude.lookup
import Data.Text (head)    -- Shadows Prelude.head

-- Ambiguous: which lookup?
result = lookup key myMap
```

### Configuration

```toml
[imports]
suggest-qualified = [
  "Data.Map",
  "Data.Map.Strict",
  "Data.Set",
  "Data.Text",
  "Data.Text.Lazy",
  "Data.ByteString",
  "Data.ByteString.Lazy",
  "Data.HashMap.Strict",
  "Data.Vector",
]
```

### Detection

```
Suggestion: Import Data.Map qualified
  Current: import Data.Map (lookup, insert)
  Suggested: import qualified Data.Map as Map
  Reason: Avoids shadowing Prelude functions
```

**Rule:** `imports/suggest-qualified`

### Common Aliases

| Module | Alias |
|--------|-------|
| `Data.Map` / `Data.Map.Strict` | `Map` or `M` |
| `Data.Set` | `Set` or `S` |
| `Data.Text` | `T` |
| `Data.Text.Lazy` | `TL` |
| `Data.ByteString` | `BS` |
| `Data.ByteString.Lazy` | `BSL` |
| `Data.HashMap.Strict` | `HM` |
| `Data.Vector` | `V` |

## Explicit Import Lists

### Require Explicit Imports

```toml
[imports]
require-explicit = true
```

Warns on wildcard imports:

```haskell
import Data.Map (*)  -- Warning: use explicit import list
```

**Rule:** `imports/wildcard`

### Exceptions

Allow wildcards for certain modules:

```toml
[imports]
allow-wildcard = [
  "Prelude",
  "ClassyPrelude",
  "RIO",
  "*.Types",  # Internal types modules
]
```

## Import Combination

Combine fragmented imports from the same module:

```haskell
-- Before
import Data.Text (Text)
import Data.Text (pack)
import Data.Text (unpack)

-- After
import Data.Text (Text, pack, unpack)
```

**Rule:** `imports/combine`

```toml
[imports]
combine = true
```

## Import Organization

### Sorting

```haskell
-- Before
import Data.Text
import Data.Map
import Control.Monad
import Data.List

-- After (alphabetized)
import Control.Monad
import Data.List
import Data.Map
import Data.Text
```

**Rule:** `imports/unsorted`

### Grouping

Organize imports by category:

```haskell
-- Standard library
import Control.Monad
import Data.List

-- External packages
import Data.Aeson
import Database.PostgreSQL.Simple

-- Internal modules
import MyApp.Types
import MyApp.Utils
```

**Rule:** `imports/ungrouped`

```toml
[imports]
organize = true
group-by-category = true
```

## Redundant Imports

### Prelude Re-exports

```haskell
import Data.List (map, filter)  -- Warning: already in Prelude
```

**Rule:** `imports/redundant-prelude`

### Submodule Overlap

```haskell
import Data.Map              -- Includes everything
import Data.Map (lookup)     -- Redundant
```

**Rule:** `imports/redundant-submodule`

## Missing Imports

When auto-fix adds functions, it also adds necessary imports:

```toml
[fix.auto-imports]
enabled = true
add-missing = true
```

Example fix:

```diff
 module MyModule where

+import Safe (headMay)
+
 process :: [Int] -> Maybe Int
-process xs = Just (head xs)
+process xs = headMay xs
```

## Import Aliases

### Consistent Aliases

Enforce consistent import aliases:

```toml
[[imports.aliases]]
module = "Data.Text"
alias = "T"
required = true

[[imports.aliases]]
module = "Data.Map.Strict"
alias = "Map"
required = true
```

Detection:

```
Inconsistent import alias:
  Found: import qualified Data.Text as Text
  Expected: import qualified Data.Text as T
```

**Rule:** `imports/inconsistent-alias`

### Custom Alias Strategy

```toml
[qualify-import]
enabled = true
strategy = "last-part"  # Use last part of module name
# Or: "acronym", "custom"

custom-aliases = [
  { module = "Data.ByteString", alias = "BS" }
]
```

## Safe Import Patterns

### Avoiding Partial Functions

When importing modules with partial functions:

```haskell
-- Import safe alternatives explicitly
import Data.Map (Map, lookup, findWithDefault)  -- No (!)

-- Or import qualified to make partial functions obvious
import qualified Data.Map as Map
result = Map.lookup key m  -- Safe
```

### Type-Only Imports

```haskell
-- Import only types (when available)
import Data.Map (Map)
import qualified Data.Map as Map  -- For functions
```

## Configuration Summary

```toml
[imports]
# Basic settings
remove-unused = true
suggest-qualified = ["Data.Map", "Data.Text", "Data.ByteString"]
require-explicit = false  # Warn on wildcards
combine = true

# Organization
organize = false  # Auto-organize imports
group-by-category = false
sort = true

# Aliases
[[imports.aliases]]
module = "Data.Text"
alias = "T"

# Allow wildcards for specific modules
allow-wildcard = ["Prelude", "*.Types"]
```

## Best Practices

1. **Explicit imports** for documentation:
```haskell
import Data.Map (Map, lookup, insert)  -- Clear what's used
```

2. **Qualified for clashing modules**:
```haskell
import qualified Data.Map.Strict as Map
```

3. **Consistent aliases** across codebase

4. **Remove unused imports** regularly

5. **Group and sort** for readability

## Next Steps

- **[Naming Rules](./naming)**: Naming conventions
- **[Configuration Reference](../configuration/imports-section)**: Full import configuration
- **[Auto-Fix](../usage-guide/auto-fix)**: Automatic import cleanup
