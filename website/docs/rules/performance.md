---
sidebar_position: 4
title: Performance Rules
description: Detect performance anti-patterns and inefficiencies
---

# Performance Rules

Argus detects performance anti-patterns including inefficient algorithms, suboptimal data structures, and patterns that prevent GHC optimizations.

## Algorithm Inefficiencies

### Length for Emptiness Check

```haskell
-- Slow: O(n) - traverses entire list
isEmpty xs = length xs == 0
hasItems xs = length xs > 0

-- Fast: O(1) - stops at first element
isEmpty xs = null xs
hasItems xs = not (null xs)
```

**Rules:** `performance/length-null`, `performance/length-gt-0`

**Auto-fix:** Available (safe)

### Head of Sorted List

```haskell
-- Slow: O(n log n) - sorts entire list
smallest = head (sort items)
largest = last (sort items)

-- Fast: O(n) - single pass
smallest = minimum items
largest = maximum items
```

**Rules:** `performance/head-sort`, `performance/last-sort`

**Auto-fix:** Available (safe)

### Concat After Map

```haskell
-- Slower: two passes, intermediate list
flattened = concat (map f items)
flattened = concat $ map f items

-- Faster: single pass
flattened = concatMap f items
```

**Rules:** `performance/concat-map`, `performance/concat-map-compose`

**Auto-fix:** Available (safe)

### Mconcat After Map

```haskell
-- Slower: two passes
combined = mconcat (map f items)

-- Faster: single pass with Monoid
combined = foldMap f items
```

**Rule:** `performance/mconcat-map`

**Auto-fix:** Available (safe)

### Nub Performance

```haskell
-- Slow: O(n²) - pairwise equality checks
unique = nub items

-- Faster: O(n log n) - requires Ord
unique = ordNub items
unique = Set.toList (Set.fromList items)
```

**Rule:** `performance/nub`

**Auto-fix:** Available but needs-review (changes constraint)

## Data Structure Issues

### String vs Text

```haskell
-- Slow: String is [Char], linked list
processText :: String -> String
processText = map toUpper

-- Fast: Text is packed UTF-16
processText :: Text -> Text
processText = T.toUpper
```

**Rule:** `performance/string-type`

**Note:** This is a suggestion, not an error, as String is sometimes appropriate.

### List as Set/Map

```haskell
-- Slow: O(n) lookup
contains items x = x `elem` items
findItem items k = lookup k items

-- Fast: O(log n) lookup
contains itemSet x = Set.member x itemSet
findItem itemMap k = Map.lookup k itemMap
```

**Rule:** `performance/list-as-container`

### Repeated Lookups

```haskell
-- Slow: multiple O(log n) lookups
if Map.member k m
  then Just (m Map.! k)
  else Nothing

-- Fast: single lookup
Map.lookup k m
```

**Rule:** `performance/repeated-lookup`

## Fusion Blockers

### Intermediate Lists

```haskell
-- Slow: creates intermediate list
result = sum (map (*2) (filter even items))

-- Better: may fuse
result = sum . map (*2) . filter even $ items

-- Best: explicit fusion
result = foldl' (\acc x -> if even x then acc + x*2 else acc) 0 items
```

**Rule:** `performance/fusion-blocker`

### Breaking Stream Fusion

```haskell
-- Breaks fusion: length forces evaluation
result = map f (take (length xs) ys)

-- May fuse: use zipWith instead
result = zipWith (\_ y -> f y) xs ys
```

**Rule:** `performance/fusion-break`

## Boxing Issues

### Unnecessary Boxing

```haskell
-- Boxing: Int wrapped in constructor
data Stats = Stats { count :: Int, total :: Int }

-- Unboxed: direct Int representation
data Stats = Stats { count :: {-# UNPACK #-} !Int
                   , total :: {-# UNPACK #-} !Int }
```

**Rule:** `performance/boxing`

**Note:** May increase memory in some cases.

## IO Efficiency

### Buffering

```haskell
-- Potentially slow: line buffered
mapM_ putStrLn items

-- Faster for many lines: use builder
mapM_ (TIO.putStrLn . TL.toStrict . TB.toLazyText . TB.fromString) items
```

**Rule:** `performance/io-buffering`

### Lazy IO

```haskell
-- Unpredictable performance
contents <- readFile "large.txt"
print (length contents)  -- Forces entire file

-- Predictable: strict IO
contents <- T.readFile "large.txt"
print (T.length contents)
```

**Rule:** `performance/lazy-io`

See also: [Space Leaks](./space-leaks)

## Monadic Performance

### Map vs Traverse

```haskell
-- Pre-AMP: requires Monad
results <- mapM processItem items
mapM_ logItem items

-- Post-AMP: requires only Applicative
results <- traverse processItem items
traverse_ logItem items
```

**Rules:** `performance/mapM-traverse`, `performance/mapM_-traverse_`

**Note:** These are also modernization suggestions.

## Numeric Performance

### Integer vs Int

```haskell
-- Slower: arbitrary precision
factorial :: Integer -> Integer
factorial n = product [1..n]

-- Faster: fixed precision (when safe)
factorial :: Int -> Int
factorial n = product [1..n]
```

**Rule:** `performance/integer-int`

**Note:** Only suggested when overflow isn't a concern.

### Floating Point

```haskell
-- Slower: Double precision
result :: Double
result = realToFrac x * y

-- Consider: Float if precision allows
result :: Float
result = realToFrac x * y
```

**Rule:** `performance/double-float`

## Common Patterns

### Foldr for Association

```haskell
-- Slow: repeated (++)
combined = foldr (++) [] lists

-- Fast: built-in
combined = concat lists
```

**Rule:** `performance/foldr-concat`

### Map ID

```haskell
-- Pointless: map id does nothing
result = map id items

-- Just use the list
result = items
```

**Rule:** `performance/map-id`

**Auto-fix:** Available (safe)

## Configuration

### Enable Performance Rules

```toml
[performance]
enabled = true
check-data-structures = true
check-algorithms = true
check-string-types = true
check-lazy-strict = true
check-fusion = true
check-boxing = true
```

### Severity Levels

```toml
[rules.severity]
"performance/length-null" = "warning"
"performance/nub" = "suggestion"
"performance/string-type" = "info"
```

### Disable Noisy Rules

```toml
[rules]
disabled = [
  "performance/boxing",      # May be verbose
  "performance/string-type", # Project uses String intentionally
]
```

## Benchmarking

When Argus flags a performance issue, consider benchmarking:

```haskell
import Criterion.Main

main = defaultMain
  [ bench "length == 0" $ wf (length items == 0)
  , bench "null"        $ wf (null items)
  ]
```

Performance rules are based on common patterns, but actual impact depends on your use case.

## GHC Optimizations

Some rules help GHC optimize better:

```haskell
-- Enable optimizations
-- stack.yaml: ghc-options: -O2

-- Rules that help GHC:
-- - Fusion-friendly patterns
-- - Strict data types
-- - INLINE pragmas where appropriate
```

## Complete Rule Reference

| Rule | Pattern | Better Pattern | Auto-Fix |
|------|---------|----------------|----------|
| `length-null` | `length xs == 0` | `null xs` | Yes |
| `length-gt-0` | `length xs > 0` | `not (null xs)` | Yes |
| `head-sort` | `head (sort xs)` | `minimum xs` | Yes |
| `last-sort` | `last (sort xs)` | `maximum xs` | Yes |
| `concat-map` | `concat (map f xs)` | `concatMap f xs` | Yes |
| `mconcat-map` | `mconcat (map f xs)` | `foldMap f xs` | Yes |
| `nub` | `nub xs` | `ordNub xs` | Review |
| `foldr-concat` | `foldr (++) []` | `concat` | Yes |
| `map-id` | `map id xs` | `xs` | Yes |

## Next Steps

- **[Space Leaks](./space-leaks)**: Memory efficiency issues
- **[Complexity](./complexity)**: Code complexity metrics
- **[Custom Rules](./custom-rules)**: Add project-specific rules
