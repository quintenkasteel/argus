---
sidebar_position: 2
title: Partial Functions
description: Detect and replace partial functions with safe alternatives
---

# Partial Function Rules

Partial functions are functions that don't handle all possible inputs, causing runtime crashes. Argus detects 50+ partial functions and suggests safe alternatives.

## Why Avoid Partial Functions?

```haskell
-- This crashes at runtime on empty list
head []  -- *** Exception: Prelude.head: empty list

-- This crashes on Nothing
fromJust Nothing  -- *** Exception: Maybe.fromJust: Nothing

-- This crashes on unparseable input
read "not a number" :: Int  -- *** Exception: Prelude.read: no parse
```

Partial functions are a common source of runtime errors in Haskell. Argus helps you find and replace them with safe alternatives.

## List Functions

### `head` / `tail` / `init` / `last`

```haskell
-- Unsafe
first = head items
rest = tail items

-- Safe alternatives
first = headMay items      -- Maybe a
rest = tailMay items       -- Maybe [a]
first = listToMaybe items  -- Maybe a (Prelude)
```

**Rules:**
| Rule | Pattern | Safe Alternative |
|------|---------|------------------|
| `partial/head` | `head xs` | `headMay xs` |
| `partial/tail` | `tail xs` | `tailMay xs` |
| `partial/init` | `init xs` | `initMay xs` |
| `partial/last` | `last xs` | `lastMay xs` |

**Configuration:**

```toml
[[patterns.rules]]
name = "avoid-head"
match = "head"
fix = "headMay"
severity = "warning"
```

### `!!` (Index)

```haskell
-- Unsafe
item = items !! 5

-- Safe alternatives
item = items !? 5           -- Maybe a (from safe)
item = atMay items 5        -- Maybe a
item = items ^? ix 5        -- Maybe a (from lens)
```

**Rule:** `partial/bang-index`

### `minimum` / `maximum`

```haskell
-- Unsafe on empty list
smallest = minimum items
largest = maximum items

-- Safe alternatives
smallest = minimumMay items  -- Maybe a
largest = maximumMay items   -- Maybe a
```

**Rules:** `partial/minimum`, `partial/maximum`

### `foldl1` / `foldr1`

```haskell
-- Unsafe
combined = foldl1 (<>) items

-- Safe alternatives
combined = foldl1May (<>) items  -- Maybe a
combined = fold items            -- Requires Monoid
combined = foldl' (<>) mempty items  -- Explicit base
```

**Rules:** `partial/foldl1`, `partial/foldr1`

## Maybe Functions

### `fromJust`

```haskell
-- Unsafe
value = fromJust maybeValue

-- Safe alternatives
value = fromMaybe defaultValue maybeValue
value = maybeValue & fromMaybe defaultValue
value = case maybeValue of
  Just v  -> v
  Nothing -> defaultValue
```

**Rule:** `partial/fromJust`

**Why no auto-fix?** The safe alternative requires a default value that Argus can't infer.

## Read/Parse Functions

### `read`

```haskell
-- Unsafe
number = read str :: Int

-- Safe alternatives
number = readMaybe str :: Maybe Int
number = readEither str :: Either String Int
```

**Rule:** `partial/read`

**Auto-fix:** Changes return type from `a` to `Maybe a`, which may require further changes.

## Map Functions

### `Map.!`

```haskell
-- Unsafe
value = myMap ! key

-- Safe alternatives
value = Map.lookup key myMap        -- Maybe v
value = Map.findWithDefault def key myMap  -- v
value = myMap ^? ix key             -- Maybe v (lens)
```

**Rule:** `partial/map-bang`

## Vector Functions

### `Vector.head` / `Vector.!`

```haskell
-- Unsafe
first = V.head vec
item = vec V.! 5

-- Safe alternatives
first = V.headM vec   -- Maybe a
item = vec V.!? 5     -- Maybe a
```

**Rules:** `partial/vector-head`, `partial/vector-index`

## Text/ByteString Functions

### `Text.head` / `Text.tail`

```haskell
-- Unsafe
firstChar = T.head text

-- Safe alternatives
firstChar = T.uncons text  -- Maybe (Char, Text)
```

**Rules:** `partial/text-head`, `partial/text-tail`

### `ByteString.head` / `ByteString.tail`

```haskell
-- Unsafe
firstByte = BS.head bytes

-- Safe alternatives
firstByte = BS.uncons bytes  -- Maybe (Word8, ByteString)
```

**Rules:** `partial/bytestring-head`, `partial/bytestring-tail`

## Enum Functions

### `succ` / `pred` / `toEnum`

```haskell
-- Unsafe (crashes at bounds)
next = succ maxBound
prev = pred minBound
value = toEnum 999 :: Bool

-- Safe alternatives
next = succMay x      -- Maybe a
prev = predMay x      -- Maybe a
value = toEnumMay 999 -- Maybe Bool
```

**Rules:** `partial/succ`, `partial/pred`, `partial/toEnum`

## Division Functions

### `div` / `mod` / `quot` / `rem`

```haskell
-- Unsafe (division by zero)
result = x `div` 0

-- Safe alternatives (from Safe)
result = x `Safe.div` y  -- Returns 0 on division by zero
result = divMay x y      -- Maybe Int
```

**Rules:** `partial/div`, `partial/mod`, `partial/quot`, `partial/rem`

## Error Functions

### `error` / `undefined`

```haskell
-- Unsafe
result = if condition then value else error "impossible"
placeholder = undefined

-- Safe alternatives
result = if condition then Just value else Nothing
result = fromMaybe (error "impossible") mValue  -- Document intent
placeholder = _  -- Typed hole (compile-time)
```

**Rules:** `partial/error`, `partial/undefined`

**Note:** These require manual review as there's no universal safe alternative.

## Suppression Patterns

### Intentional Usage

When partial function usage is intentional:

```haskell
-- Document why it's safe
let first = head items  -- PARTIAL: items guaranteed non-empty by caller

-- Or use Argus suppression
let first = head items  -- argus:ignore partial/head: validated above
```

### Test Code

Relax rules in test files:

```toml
[[scopes]]
modules = ["*.Spec", "Test.*"]
ignore = ["partial/head", "partial/tail"]
```

## Safe Package

Most safe alternatives come from the `safe` package:

```bash
# Add to your dependencies
stack build --package safe
```

```haskell
import Safe (headMay, tailMay, readMay, atMay, ...)
```

## Complete Reference

| Partial | Safe Alternative | Package |
|---------|------------------|---------|
| `head` | `headMay` | safe |
| `tail` | `tailMay` | safe |
| `init` | `initMay` | safe |
| `last` | `lastMay` | safe |
| `!!` | `atMay`, `!?` | safe |
| `minimum` | `minimumMay` | safe |
| `maximum` | `maximumMay` | safe |
| `foldl1` | `foldl1May` | safe |
| `foldr1` | `foldr1May` | safe |
| `fromJust` | `fromMaybe` | base |
| `read` | `readMaybe` | base |
| `Map.!` | `Map.lookup` | containers |
| `succ` | `succMay` | safe |
| `pred` | `predMay` | safe |
| `toEnum` | `toEnumMay` | safe |
| `div` | `Safe.div` | safe |

## Configuration

```toml
# Make partial functions errors
[rules.severity]
"partial/head" = "error"
"partial/fromJust" = "error"
"partial/undefined" = "error"

# Auto-fix settings
[fix]
enabled = true

# Partial function fixes need review
[fix.safety]
"partial/*" = "needs-review"
```

## Next Steps

- **[Security Rules](./security)**: Security vulnerability detection
- **[Performance Rules](./performance)**: Performance anti-patterns
- **[Custom Rules](./custom-rules)**: Define project-specific rules
