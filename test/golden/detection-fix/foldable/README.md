# Foldable Golden Test

This golden test validates the detection and fixing of Foldable/Traversable-related patterns.

## Test Coverage

### Fold Optimization Patterns (10 patterns)
- `foldl` → `foldl'` (space leak prevention)
- `foldr (+) 0` → `foldl' (+) 0` (commutative operations)
- `foldMap id` → `fold`
- `concat (map f)` → `concatMap f` / `foldMap f`
- `foldr (||) False` → `or`
- `foldr (&&) True` → `and`
- `foldl' (+) 0` → `sum`
- `foldl' (*) 1` → `product`
- Manual counting fold → `length`
- `length xs == 0` → `null xs` (performance)

### Traversal Modernization (8 patterns)
- `mapM` → `traverse`
- `forM` → `for`
- `sequenceA (fmap f)` → `traverse f`
- `void (traverse f)` → `traverse_ f`
- `mapM_` → `traverse_`
- `forM_` → `for_`
- `void (sequence xs)` → `sequence_ xs`
- `sequenceA (map f)` → `traverse f`

### Specialized Container Functions (6 patterns)
- `elem x (Set.fromList xs)` → `Set.member x (Set.fromList xs)`
- `notElem x (Set.toList s)` → `Set.notMember x s`
- `foldl1 max` → `maximum`
- `foldl1 min` → `minimum`
- `head (sortBy cmp)` → `maximumBy cmp` (O(n) vs O(n log n))
- `last (sortBy cmp)` → `minimumBy cmp`

### List to Foldable Generalization (8 patterns)
- `Data.List.any` → `any`
- `Data.List.all` → `all`
- `Data.List.null` → `null`
- `Data.List.length` → `length`
- `Data.List.elem` → `elem`
- `Data.List.sum` → `sum`
- `Data.List.product` → `product`
- `concat` → `fold` (for Foldable context)

## Files

- `input.hs` - Code with foldable anti-patterns (compiles with warnings)
- `fixed.hs` - Corrected code with optimal foldable usage
- `diagnostics.golden` - Expected JSON output (29 diagnostics)

## Notes

All patterns are demonstrated with:
- Complete, valid Haskell that compiles
- Realistic function signatures and implementations
- Clear comments indicating expected transformations
- Complex examples showing real-world usage patterns
