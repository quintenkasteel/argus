---
sidebar_position: 12
title: Redundant Code Rules
description: Remove unnecessary and redundant code patterns
---

# Redundant Code Rules

Argus detects code patterns that are redundant or can be simplified without changing behavior.

## Identity Operations

### `id x` → `x`

```haskell
-- Redundant
result = id value

-- Simplified
result = value
```

**Rule:** `redundant/id`

**Auto-fix:** Available (safe)

### `map id` → identity

```haskell
-- Redundant
result = map id list

-- Simplified
result = list
```

**Rule:** `redundant/map-id`

**Auto-fix:** Available (safe)

## Double Operations

### Double Negation

```haskell
-- Redundant
isValid = not (not condition)

-- Simplified
isValid = condition
```

**Rule:** `redundant/double-not`

**Auto-fix:** Available (safe)

### Double Reverse

```haskell
-- Redundant
result = reverse (reverse list)

-- Simplified
result = list
```

**Rule:** `redundant/double-reverse`

**Auto-fix:** Available (safe)

### Double fromIntegral

```haskell
-- Redundant
x = fromIntegral (fromIntegral n :: Int)

-- Simplified
x = fromIntegral n
```

**Rule:** `redundant/double-fromIntegral`

## Boolean Redundancy

### If-Then-Else with Booleans

```haskell
-- Redundant
result = if condition then True else False

-- Simplified
result = condition
```

**Rule:** `redundant/if-then-else-bool`

**Auto-fix:** Available (safe)

```haskell
-- Redundant
result = if condition then False else True

-- Simplified
result = not condition
```

**Rule:** `redundant/if-then-else-bool-not`

**Auto-fix:** Available (safe)

### Comparison with True/False

```haskell
-- Redundant
if condition == True then ...
if condition == False then ...

-- Simplified
if condition then ...
if not condition then ...
```

**Rule:** `redundant/compare-bool`

## Maybe Redundancy

### `maybe Nothing Just`

```haskell
-- Redundant
result = maybe Nothing Just value

-- Simplified (identity for Maybe)
result = value
```

**Rule:** `redundant/maybe-Just-id`

**Auto-fix:** Available (safe)

### `fromMaybe` with Default

```haskell
-- Redundant
result = fromMaybe defaultValue (Just value)

-- Simplified
result = value
```

**Rule:** `redundant/fromMaybe-Just`

### `isJust` / `isNothing` with Pattern

```haskell
-- Redundant
case x of
  Just _ -> True
  Nothing -> False

-- Simplified
isJust x
```

**Rule:** `redundant/case-isJust`

## Either Redundancy

### `either Left Right`

```haskell
-- Redundant
result = either Left Right value

-- Simplified (identity for Either)
result = value
```

**Rule:** `redundant/either-Left-Right`

**Auto-fix:** Available (safe)

## List Redundancy

### Empty List Operations

```haskell
-- Redundant
result = x ++ []
result = [] ++ x

-- Simplified
result = x
```

**Rule:** `redundant/append-empty`

### `take` / `drop` with 0

```haskell
-- Redundant
result = take 0 list  -- Always []
result = drop 0 list  -- Always list

-- Simplified
result = []
result = list
```

**Rule:** `redundant/take-drop-zero`

### `filter (const True)`

```haskell
-- Redundant
result = filter (const True) list

-- Simplified
result = list
```

**Rule:** `redundant/filter-const-true`

## Function Application

### `($)` at End of Chain

```haskell
-- Redundant (no right-hand side benefit)
result = f $ x

-- Simpler
result = f x
```

**Rule:** `redundant/dollar-simple` (suggestion)

### Unnecessary Parentheses

```haskell
-- Redundant
result = (f) x
result = f (x)

-- Simpler
result = f x
```

**Rule:** `redundant/parens`

## Lambda Simplification

### Eta Reduction

```haskell
-- Redundant
mapped = map (\x -> f x) list

-- Simplified
mapped = map f list
```

**Rule:** `redundant/eta`

### Lambda with `const`

```haskell
-- Redundant
result = map (\_ -> defaultValue) list

-- Simplified
result = map (const defaultValue) list
-- Or even simpler:
result = replicate (length list) defaultValue
```

**Rule:** `redundant/lambda-const`

## Monadic Redundancy

### `>>= return`

```haskell
-- Redundant
result = action >>= return

-- Simplified
result = action
```

**Rule:** `redundant/bind-return`

### `return` then `(>>=)`

```haskell
-- Redundant
result = return x >>= f

-- Simplified
result = f x
```

**Rule:** `redundant/return-bind`

### `fmap` in `do`

```haskell
-- Sometimes redundant
do
  x <- fmap f action
  return x

-- Simpler
fmap f action
```

**Rule:** `redundant/do-fmap-return`

## Comparison Redundancy

### `x == x`

```haskell
-- Always True (for types with reflexive Eq)
condition = x == x

-- Simplified
condition = True
```

**Rule:** `redundant/compare-self`

### Negated Comparisons

```haskell
-- Redundant
x = not (a == b)
y = not (a < b)

-- Simplified
x = a /= b
y = a >= b
```

**Rule:** `redundant/negated-comparison`

## Type Annotations

### Redundant Signatures

```haskell
-- Sometimes redundant
f :: Int -> Int
f x = x + 1  -- Type is obvious

-- Keep signatures for:
-- - Exported functions
-- - Complex types
-- - Documentation
```

**Rule:** `redundant/type-signature` (disabled by default)

## Configuration

### Enable Redundant Rules

```toml
[categories]
redundant = "warning"
```

### Severity

```toml
[rules.severity]
"redundant/id" = "warning"
"redundant/dollar-simple" = "info"
"redundant/parens" = "info"
```

### Disable Specific Rules

```toml
[rules]
disabled = [
  "redundant/dollar-simple",  # Team prefers $ style
  "redundant/parens",         # Extra parens for clarity
]
```

## Auto-Fix

Apply redundant code fixes:

```bash
argus fix src/ --categories redundant --preview
```

All redundant code fixes are safe.

## Complete Rule Reference

| Rule | Before | After | Safety |
|------|--------|-------|--------|
| `id` | `id x` | `x` | Safe |
| `map-id` | `map id xs` | `xs` | Safe |
| `double-not` | `not (not x)` | `x` | Safe |
| `double-reverse` | `reverse (reverse xs)` | `xs` | Safe |
| `if-then-else-bool` | `if c then True else False` | `c` | Safe |
| `if-then-else-bool-not` | `if c then False else True` | `not c` | Safe |
| `maybe-Just-id` | `maybe Nothing Just x` | `x` | Safe |
| `either-Left-Right` | `either Left Right x` | `x` | Safe |
| `append-empty` | `xs ++ []` | `xs` | Safe |
| `bind-return` | `m >>= return` | `m` | Safe |
| `return-bind` | `return x >>= f` | `f x` | Safe |

## Best Practices

1. **Apply redundant fixes** - They never change semantics
2. **Review eta-reduction** - Sometimes explicit is clearer
3. **Keep type signatures** - Even if "redundant" for documentation
4. **Parentheses for clarity** - May suppress `redundant/parens`

## Next Steps

- **[Custom Rules](./custom-rules)**: Define project-specific rules
- **[Auto-Fix](../usage-guide/auto-fix)**: Apply fixes automatically
- **[Configuration](../configuration/rules-section)**: Configure rules
