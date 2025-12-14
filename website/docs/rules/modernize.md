---
sidebar_position: 11
title: Modernization Rules
description: Update code to use modern Haskell idioms
---

# Modernization Rules

Argus suggests updates to use modern Haskell idioms and APIs, particularly changes from the Applicative-Monad Proposal (AMP) and Semigroup-Monoid Proposal (SMP).

## Applicative vs Monad

### `return` → `pure`

Since GHC 7.10, `Applicative` is a superclass of `Monad`. Use `pure` for more general code:

```haskell
-- Old style
getValue :: IO Int
getValue = return 42

-- Modern style
getValue :: IO Int
getValue = pure 42
```

**Rule:** `modernize/return-pure`

**Auto-fix:** Available (safe)

### `liftM` → `fmap`

```haskell
-- Old style
doubled <- liftM (*2) getValue

-- Modern style
doubled <- fmap (*2) getValue
-- Or with (<$>)
doubled <- (*2) <$> getValue
```

**Rule:** `modernize/liftM-fmap`

**Auto-fix:** Available (safe)

### `liftM2` → `liftA2`

```haskell
-- Old style
result <- liftM2 (+) getX getY

-- Modern style
result <- liftA2 (+) getX getY
-- Or with Applicative operators
result <- (+) <$> getX <*> getY
```

**Rule:** `modernize/liftM2-liftA2`

**Auto-fix:** Available (safe)

### `ap` → `(<*>)`

```haskell
-- Old style
result = Just (+1) `ap` Just 5

-- Modern style
result = Just (+1) <*> Just 5
```

**Rule:** `modernize/ap-star`

## Traversable vs Monad

### `mapM` → `traverse`

```haskell
-- Old style (requires Monad)
results <- mapM processItem items

-- Modern style (requires only Applicative)
results <- traverse processItem items
```

**Rule:** `modernize/mapM-traverse`

**Auto-fix:** Available (safe)

### `mapM_` → `traverse_`

```haskell
-- Old style
mapM_ print items

-- Modern style
traverse_ print items
```

**Rule:** `modernize/mapM_-traverse_`

**Auto-fix:** Available (safe)

### `forM` → `for`

```haskell
-- Old style
forM items $ \item -> do
  process item

-- Modern style
for items $ \item -> do
  process item
```

**Rule:** `modernize/forM-for`

### `sequence` → `sequenceA`

```haskell
-- Old style
results <- sequence [action1, action2, action3]

-- Modern style
results <- sequenceA [action1, action2, action3]
```

**Rule:** `modernize/sequence-sequenceA`

## Semigroup vs Monoid

### `mappend` → `(<>)`

Since GHC 8.4, `Semigroup` is a superclass of `Monoid`:

```haskell
-- Old style
combined = mappend x y

-- Modern style
combined = x <> y
```

**Rule:** `modernize/mappend-semigroup`

**Auto-fix:** Available (safe)

### `mempty <> x` → `x`

```haskell
-- Redundant
result = mempty <> value

-- Simplified
result = value
```

**Rule:** `modernize/mempty-identity`

## Foldable Improvements

### `foldMap id` → `fold`

```haskell
-- Old style
combined = foldMap id items

-- Modern style
combined = fold items
```

**Rule:** `modernize/foldMap-id`

**Auto-fix:** Available (safe)

### `concat` with Foldable

```haskell
-- Old style (lists only)
result = concat listOfLists

-- Modern style (any Foldable of lists)
result = fold foldableOfLists
```

**Rule:** `modernize/concat-fold`

## Function Composition

### `(.)` vs `(<$>)`

```haskell
-- Sometimes clearer with (<$>)
result = f . g . h $ x

-- Alternative
result = f . g . h $ x  -- Keep if clearer
result = f <$> g <$> h x  -- If working with functors
```

### `flip` Patterns

```haskell
-- Old style
result = flip lookup myMap key

-- Modern style (if clearer)
result = lookup key myMap
```

## Error Handling

### `fail` Usage

```haskell
-- Old style: relies on MonadFail
parseItem str = do
  guard (not $ null str)
  case parse str of
    Just x  -> return x
    Nothing -> fail "parse error"

-- Modern style: explicit MonadFail or error handling
parseItem :: MonadFail m => String -> m Item
parseItem str = ...
```

**Rule:** `modernize/monad-fail`

## Record Updates

### `RecordWildCards` → `NamedFieldPuns`

```haskell
-- With RecordWildCards
printPerson Person{..} = putStrLn name

-- Often clearer with explicit fields
printPerson Person{name} = putStrLn name
```

**Rule:** `modernize/record-wildcards` (suggestion level)

## do Notation

### Single Statement `do`

```haskell
-- Unnecessary do
action = do
  performSingle

-- Without do
action = performSingle
```

**Rule:** `modernize/single-do`

### `do return x` → `pure x`

```haskell
-- Verbose
getValue = do
  x <- compute
  return x

-- Cleaner
getValue = compute
```

**Rule:** `modernize/do-return`

## List Operations

### `++` vs `<>`

```haskell
-- Lists only
combined = list1 ++ list2

-- More general (works with any Semigroup)
combined = list1 <> list2
```

**Rule:** `modernize/append-semigroup`

Note: This is a suggestion, not always preferred.

## Configuration

### Enable Modernization Rules

```toml
[categories]
modernize = "suggestion"
```

### Severity

```toml
[rules.severity]
"modernize/return-pure" = "suggestion"
"modernize/mapM-traverse" = "suggestion"
"modernize/mappend-semigroup" = "warning"
```

### Disable Specific Rules

```toml
[rules]
disabled = [
  "modernize/return-pure",  # Team prefers return
]
```

## Auto-Fix

Apply all modernization fixes:

```bash
argus fix src/ --categories modernize --preview
```

Most modernization fixes are safe and don't change semantics.

## Complete Rule Reference

| Rule | Before | After | Safety |
|------|--------|-------|--------|
| `return-pure` | `return x` | `pure x` | Safe |
| `liftM-fmap` | `liftM f x` | `fmap f x` | Safe |
| `liftM2-liftA2` | `liftM2 f x y` | `liftA2 f x y` | Safe |
| `mapM-traverse` | `mapM f xs` | `traverse f xs` | Safe |
| `mapM_-traverse_` | `mapM_ f xs` | `traverse_ f xs` | Safe |
| `forM-for` | `forM xs f` | `for xs f` | Safe |
| `forM_-for_` | `forM_ xs f` | `for_ xs f` | Safe |
| `sequence-sequenceA` | `sequence xs` | `sequenceA xs` | Safe |
| `mappend-semigroup` | `mappend x y` | `x <> y` | Safe |
| `foldMap-id` | `foldMap id xs` | `fold xs` | Safe |

## Best Practices

1. **Use Applicative** when you don't need Monad
2. **Use Traversable** instead of Monad-specific functions
3. **Use Semigroup** operators for combining
4. **Keep code readable** - not all modernizations improve clarity

## Next Steps

- **[Redundant Rules](./redundant)**: Remove redundant code
- **[Custom Rules](./custom-rules)**: Define project-specific rules
- **[Auto-Fix](../usage-guide/auto-fix)**: Apply fixes automatically
