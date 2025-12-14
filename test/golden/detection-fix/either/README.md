# Either Golden Tests

This directory contains golden tests for Either-related linting rules.

## Test Coverage

The test cases cover 23 different Either simplification and optimization rules:

### Identity Patterns
- `either Left Right e` → `e`
- `either (const False) (const True) e` → `isRight e`
- `either (const True) (const False) e` → `isLeft e`

### Functor Patterns
- `either Left (Right . f) e` → `fmap f e`
- `either (Left . f) Right e` → `first f e`
- Case expressions to `either` combinator

### Constant Folding
- `isLeft (Left x)` → `True`
- `isLeft (Right x)` → `False`
- `isRight (Left x)` → `False`
- `isRight (Right x)` → `True`

### fromLeft/fromRight Simplification
- `fromLeft def (Left val)` → `val`
- `fromLeft def (Right val)` → `def`
- `fromRight def (Left val)` → `def`
- `fromRight def (Right val)` → `val`

### List Operations
- `lefts [Left x]` → `[x]`
- `lefts [Right x]` → `[]`
- `rights [Left x]` → `[]`
- `rights [Right x]` → `[x]`
- `partitionEithers [Left x]` → `([x], [])`
- `partitionEithers [Right x]` → `([], [x])`

### Bifunctor Identity
- `bimap id id e` → `e`
- `first id e` → `e`
- `second id e` → `e`
- `fmap id e` → `e`

## Files

- `input.hs`: Source code with patterns that trigger Either rules
- `fixed.hs`: Expected output after applying all fixes
- `diagnostics.golden`: Expected JSON diagnostics (23 rules)

All files are valid, compilable Haskell code with proper type signatures.
