---
sidebar_position: 10
title: Pragma Rules
description: Detect unused and missing LANGUAGE extensions
---

# Pragma Rules

Argus analyzes LANGUAGE pragmas (extensions) to find unused extensions and suggest missing ones.

## Unused Extension Detection

Argus checks 20+ extensions that can be reliably detected:

```haskell
{-# LANGUAGE LambdaCase #-}       -- Warning if \case not used
{-# LANGUAGE RecordWildCards #-}  -- Warning if {..} not used
{-# LANGUAGE TypeApplications #-} -- Warning if @Type not used
{-# LANGUAGE BangPatterns #-}     -- Warning if ! patterns not used
{-# LANGUAGE UnicodeSyntax #-}    -- Warning if unicode symbols not used
```

**Rule:** `extension/unused`

### Detectable Extensions

| Extension | Detection Method |
|-----------|------------------|
| `LambdaCase` | `\case` syntax |
| `RecordWildCards` | `{..}` pattern |
| `TypeApplications` | `@Type` syntax |
| `BangPatterns` | `!pattern` |
| `ViewPatterns` | `(view -> pat)` |
| `PatternGuards` | Multiple guards |
| `MultiWayIf` | `if | ...` syntax |
| `TupleSections` | `(,x)` or `(x,)` |
| `OverloadedStrings` | String literals |
| `OverloadedLists` | List literals with types |
| `NamedFieldPuns` | `Rec{field}` |
| `NumericUnderscores` | `1_000_000` |
| `BinaryLiterals` | `0b1010` |
| `HexFloatLiterals` | `0x1.5p10` |
| `NegativeLiterals` | `-123` as literal |
| `UnicodeSyntax` | `∷`, `→`, `∀` |
| `PostfixOperators` | `n!` |
| `TemplateHaskell` | `$()`, `[||]` |
| `QuasiQuotes` | `[sql|...|]` |
| `DeriveGeneric` | `deriving Generic` |
| `DeriveFunctor` | `deriving Functor` |
| `DeriveAnyClass` | Arbitrary deriving |
| `GADTs` | GADT syntax |
| `TypeFamilies` | `type family` |
| `RankNTypes` | `forall` in types |

### Example

```haskell
{-# LANGUAGE LambdaCase #-}       -- Used
{-# LANGUAGE RecordWildCards #-}  -- UNUSED
{-# LANGUAGE BangPatterns #-}     -- Used

-- Uses LambdaCase
processResult = \case
  Success x -> handleSuccess x
  Failure e -> handleError e

-- Uses BangPatterns
loop !acc [] = acc
loop !acc (x:xs) = loop (acc + x) xs

-- RecordWildCards not used anywhere
```

Output:

```
Warning: Unused LANGUAGE pragma
  Extension: RecordWildCards
  Location: line 2
  Suggestion: Remove if not needed
```

## Missing Extension Detection

Argus can detect when code requires an extension that isn't enabled:

```haskell
-- Missing: {-# LANGUAGE LambdaCase #-}

handler = \case  -- Error: requires LambdaCase
  A -> ...
  B -> ...
```

**Rule:** `extension/missing`

### Common Missing Extensions

```haskell
-- Needs OverloadedStrings
message :: Text
message = "hello"  -- String literal used as Text

-- Needs RecordWildCards
data Config = Config { host :: String, port :: Int }
printConfig Config{..} = putStrLn host  -- {..} syntax

-- Needs BangPatterns
strictArg !x = x + 1  -- ! in pattern

-- Needs LambdaCase
handler = \case A -> 1; B -> 2  -- \case syntax
```

## Redundant Pragma Detection

### Implied by Other Extensions

Some extensions imply others:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTSyntax #-}  -- Redundant: implied by GADTs

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}  -- Redundant: subset of RankNTypes

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}  -- Redundant: implied
```

**Rule:** `extension/redundant`

### Extension Implications

| Extension | Implies |
|-----------|---------|
| `GADTs` | `GADTSyntax`, `MonoLocalBinds` |
| `TypeFamilies` | `MonoLocalBinds`, `KindSignatures` |
| `RankNTypes` | `ExplicitForAll` |
| `ScopedTypeVariables` | `ExplicitForAll` |
| `FlexibleInstances` | `TypeSynonymInstances` |

## Safe Extension Defaults

Argus suggests extensions that are widely considered safe defaults:

```toml
[extensions]
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
```

**Rule:** `extension/suggest-default`

## Dangerous Extension Detection

Warn about extensions with footguns:

```haskell
{-# LANGUAGE UndecidableInstances #-}  -- Warning: can cause loops
{-# LANGUAGE IncoherentInstances #-}   -- Warning: unpredictable
{-# LANGUAGE OverlappingInstances #-}  -- Warning: order-dependent
```

**Rule:** `extension/dangerous`

### Dangerous Extensions

| Extension | Risk |
|-----------|------|
| `UndecidableInstances` | Non-terminating type checking |
| `IncoherentInstances` | Unpredictable instance selection |
| `OverlappingInstances` | Import-order dependent |
| `ImpredicativeTypes` | Limited support, bugs |
| `Unsafe` | Bypasses safety checks |

## Per-File vs Global Extensions

### Suggest Moving to package.yaml

When an extension is used in most files:

```
Suggestion: Consider enabling LambdaCase globally
  Used in: 45 out of 50 files
  Add to: package.yaml default-extensions
```

**Rule:** `extension/consider-global`

### Configuration

```toml
[extensions]
global-threshold = 0.7  # Suggest global if used in >70% of files
```

## Configuration

### Basic Settings

```toml
[extensions]
enabled = true
check-unused = true
check-missing = true
check-redundant = true
check-dangerous = true
```

### Severity

```toml
[rules.severity]
"extension/unused" = "warning"
"extension/missing" = "error"
"extension/dangerous" = "warning"
```

### Disable Specific Checks

```toml
[rules]
disabled = [
  "extension/missing",  # Don't warn about missing extensions
]
```

### Scope-Based

```toml
# Relax in test files
[[scopes]]
modules = ["*Spec", "Test.*"]
ignore = ["extension/unused"]
```

## Auto-Fix

### Remove Unused Extensions

```bash
argus fix src/ --rules extension/unused
```

```diff
-{-# LANGUAGE LambdaCase #-}
-{-# LANGUAGE RecordWildCards #-}
 {-# LANGUAGE BangPatterns #-}
+{-# LANGUAGE LambdaCase #-}

 module MyModule where
```

### Add Missing Extensions

```bash
argus fix src/ --rules extension/missing
```

```diff
+{-# LANGUAGE OverloadedStrings #-}
+
 module MyModule where

 import Data.Text (Text)
```

## Best Practices

1. **Use default-extensions** in package.yaml for common extensions
2. **Keep file-specific extensions** only for unusual ones
3. **Remove unused extensions** to reduce compile time
4. **Document dangerous extensions** with comments
5. **Use DerivingStrategies** to make deriving explicit

### package.yaml Example

```yaml
default-extensions:
  - BangPatterns
  - DeriveGeneric
  - DerivingStrategies
  - LambdaCase
  - OverloadedStrings
  - ScopedTypeVariables
  - TypeApplications
```

## Next Steps

- **[Modernize Rules](./modernize)**: Code modernization suggestions
- **[Configuration](../configuration/file-format)**: Full configuration reference
- **[Custom Rules](./custom-rules)**: Create custom extension rules
