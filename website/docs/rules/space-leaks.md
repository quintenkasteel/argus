---
sidebar_position: 5
title: Space Leak Rules
description: Detect patterns that cause memory leaks in Haskell
---

# Space Leak Rules

Space leaks occur when Haskell's lazy evaluation causes unexpected memory retention. Argus detects common patterns that lead to space leaks.

## Understanding Space Leaks

In Haskell, expressions are evaluated lazily by default. This can cause problems:

```haskell
-- Space leak: accumulates thunks
badSum = foldl (+) 0 [1..1000000]
-- Builds: (((0 + 1) + 2) + 3) + ... as unevaluated thunks

-- No leak: evaluates immediately
goodSum = foldl' (+) 0 [1..1000000]
-- Evaluates each addition before the next
```

## Lazy Fold Functions

### `foldl` vs `foldl'`

```haskell
-- Space leak: lazy accumulator
total = foldl (+) 0 numbers

-- Fixed: strict accumulator
import Data.List (foldl')
total = foldl' (+) 0 numbers
```

**Rule:** `space-leaks/foldl`

**Auto-fix:** Available (mostly-safe, requires import)

### When Lazy is Correct

Sometimes lazy `foldl` is intentional:

```haskell
-- Intentional lazy evaluation for infinite structures
takeFirst = foldl (\_ x -> x) undefined (take 10 infiniteList)
```

Suppress with:
```haskell
-- argus:ignore space-leaks/foldl: intentional lazy evaluation
```

## Lazy State Monad

### Import Pattern

```haskell
-- Space leak prone
import Control.Monad.State

-- Better: explicit strict
import Control.Monad.State.Strict
```

**Rule:** `space-leaks/lazy-state`

**Auto-fix:** Available (mostly-safe)

### Usage Pattern

```haskell
-- Lazy State accumulates thunks
runComputation :: State Int ()
runComputation = do
  modify (+1)  -- Builds thunk
  modify (+1)  -- Another thunk
  -- Thunks accumulate

-- Strict State evaluates immediately
runComputation :: State.Strict.State Int ()
runComputation = do
  modify' (+1)  -- Evaluates
  modify' (+1)  -- Evaluates
```

## Lazy Writer Monad

```haskell
-- Space leak prone
import Control.Monad.Writer

-- Better: explicit strict
import Control.Monad.Writer.Strict
```

**Rule:** `space-leaks/lazy-writer`

**Auto-fix:** Available (mostly-safe)

## Lazy Data Structures

### Lazy Map

```haskell
-- Lazy values in map
import Data.Map

-- Strict values
import Data.Map.Strict
```

**Rule:** `space-leaks/lazy-map`

### Lazy IntMap

```haskell
-- Lazy values
import Data.IntMap

-- Strict values
import Data.IntMap.Strict
```

**Rule:** `space-leaks/lazy-intmap`

### Lazy Record Fields

```haskell
-- Space leak: lazy fields accumulate thunks
data Stats = Stats
  { count :: Int
  , total :: Int
  }

incrementStats :: Stats -> Stats
incrementStats s = s { count = count s + 1 }  -- Thunk

-- Fixed: strict fields
data Stats = Stats
  { count :: !Int
  , total :: !Int
  }

-- Or use StrictData extension
{-# LANGUAGE StrictData #-}
data Stats = Stats { count :: Int, total :: Int }
```

**Rule:** `space-leaks/lazy-data`

## Lazy IO

### `readFile` / `getContents`

```haskell
-- Space leak: holds file handle open, unpredictable memory
contents <- readFile "large.txt"
print (length contents)

-- Fixed: strict IO
import qualified Data.Text.IO as TIO
contents <- TIO.readFile "large.txt"
print (T.length contents)

-- Or ByteString
import qualified Data.ByteString as BS
contents <- BS.readFile "large.txt"
```

**Rule:** `space-leaks/lazy-io`

### `hGetContents`

```haskell
-- Space leak and resource leak
handle <- openFile "file.txt" ReadMode
contents <- hGetContents handle
hClose handle  -- Too early! Contents not read yet

-- Fixed: use bracket with strict read
withFile "file.txt" ReadMode $ \h -> do
  contents <- TIO.hGetContents h
  process contents
```

**Rule:** `space-leaks/hGetContents`

### `interact`

```haskell
-- Lazy stdin/stdout
main = interact (map toUpper)

-- Fixed: strict processing
main = TIO.interact (T.toUpper)
```

**Rule:** `space-leaks/interact`

## Accumulator Patterns

### Non-Strict Accumulator

```haskell
-- Space leak: acc is lazy
sumWithCount :: [Int] -> (Int, Int)
sumWithCount = go (0, 0)
  where
    go acc [] = acc
    go (s, c) (x:xs) = go (s + x, c + 1) xs

-- Fixed: bang patterns
sumWithCount :: [Int] -> (Int, Int)
sumWithCount = go (0, 0)
  where
    go !acc [] = acc
    go (!s, !c) (x:xs) = go (s + x, c + 1) xs
```

**Rule:** `space-leaks/lazy-accumulator`

### Using `seq`

```haskell
-- Manual strictness
sumWithCount = go (0, 0)
  where
    go acc [] = acc
    go (s, c) (x:xs) =
      let s' = s + x
          c' = c + 1
      in s' `seq` c' `seq` go (s', c') xs
```

## CAF Space Leaks

Constant Applicative Forms (CAFs) are top-level values:

```haskell
-- CAF: computed once, retained forever
bigList :: [Int]
bigList = [1..10000000]

-- If partially used, retains entire list
firstTen = take 10 bigList
```

**Rule:** `space-leaks/caf`

**Note:** This is an info-level rule as CAFs are sometimes intentional.

## Detection Strategies

### Profiling

```bash
# Compile with profiling
stack build --profile

# Run with heap profiling
stack exec -- myprogram +RTS -hc -p

# View profile
hp2ps -c myprogram.hp
```

### WeakRef Check

```haskell
-- Check if value is retained
import System.Mem.Weak
testRetention value = do
  weak <- mkWeakPtr value Nothing
  performGC
  deRefWeak weak  -- Nothing if collected
```

## Configuration

### Enable Space Leak Rules

```toml
[space-leaks]
enabled = true
check-lazy-folds = true
check-lazy-state = true
check-lazy-data = true
check-lazy-io = true
check-accumulators = true
check-caf = false  # Often noisy
```

### Severity

```toml
[rules.severity]
"space-leaks/foldl" = "warning"
"space-leaks/lazy-io" = "error"
```

## Best Practices

### 1. Use Strict by Default

```haskell
{-# LANGUAGE StrictData #-}

-- Or in package.yaml
default-extensions:
  - StrictData
```

### 2. Use Strict Imports

```haskell
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
```

### 3. Bang Patterns for Accumulators

```haskell
{-# LANGUAGE BangPatterns #-}

loop !acc [] = acc
loop !acc (x:xs) = loop (acc + x) xs
```

### 4. Profile Regularly

Run heap profiling to catch space leaks early.

## Complete Rule Reference

| Rule | Pattern | Fix |
|------|---------|-----|
| `space-leaks/foldl` | `foldl f z xs` | `foldl' f z xs` |
| `space-leaks/lazy-state` | `import Control.Monad.State` | `import Control.Monad.State.Strict` |
| `space-leaks/lazy-writer` | `import Control.Monad.Writer` | `import Control.Monad.Writer.Strict` |
| `space-leaks/lazy-map` | `import Data.Map` | `import Data.Map.Strict` |
| `space-leaks/lazy-intmap` | `import Data.IntMap` | `import Data.IntMap.Strict` |
| `space-leaks/lazy-io` | `readFile path` | `T.readFile path` |
| `space-leaks/lazy-accumulator` | Non-strict accumulator | Add `!` patterns |
| `space-leaks/lazy-data` | Lazy record fields | Add `!` or `StrictData` |

## Next Steps

- **[Complexity Rules](./complexity)**: Code complexity metrics
- **[Performance Rules](./performance)**: Other performance issues
- **[Configuration](../configuration/file-format)**: Full configuration reference
