# String Golden Tests

This directory contains golden test files for string and text manipulation rules.

## Files

- `input.hs`: Haskell code with inefficient string patterns
- `fixed.hs`: Expected output after applying all applicable fixes
- `diagnostics.golden`: Expected JSON diagnostics output

## Test Coverage

### Pack/Unpack Patterns (4 cases)
- `T.pack (T.unpack x)` → `x` (identity roundtrip)
- `T.unpack (T.pack x)` → `x` (identity roundtrip)
- `T.pack (BS8.unpack x)` → `decodeUtf8 x` (efficient ByteString conversion)
- `T.pack (T.unpack x ++ T.unpack y)` → `x <> y` (avoid unpack/repack)

### Concatenation Patterns (6 cases)
- `T.concat xs` → `mconcat xs` (use general Monoid)
- `mappend x y` → `x <> y` (modern operator)
- `x ++ y` → `x <> y` (Semigroup instead of list-specific)
- `a <> b <> c <> d` → `mconcat [a, b, c, d]` (multiple appends)
- `T.concat [many, pieces]` → suggest Builder for efficiency
- `intercalate " " xs` → `unwords xs` (specialized function)

### Null/Empty Checking (4 cases)
- `x == ""` → `T.null x` (O(1) check)
- `T.length x == 0` → `T.null x` (avoid length)
- `T.length x > 0` → `not (T.null x)` (avoid length)
- `T.length x /= 0` → `not (T.null x)` (avoid length)

### Case Conversion (2 cases)
- `T.map toLower x` → `T.toLower x` (specialized function)
- `T.map toUpper x` → `T.toUpper x` (specialized function)

### Other Manipulation (2 cases)
- `T.dropWhile isSpace (T.dropWhileEnd isSpace x)` → `T.strip x` (trim both ends)
- `(T.take n x, T.drop n x)` → `T.splitAt n x` (single traversal)

### Safety Warnings (2 cases)
- `T.head x` → warns about unsafe operation (no fix, suggest T.uncons)
- `T.last x` → warns about unsafe operation (no fix, suggest T.unsnoc)

### Literal Strings (1 case)
- `T.pack "literal"` → suggest OverloadedStrings extension

## Total Rules Tested

18 string-related rules are exercised by these golden tests, covering:
- Performance optimizations
- Safety improvements
- Modern idiom suggestions
- Code simplifications

## Compilation

Both input.hs and fixed.hs compile successfully with GHC 9.10.3:

```bash
stack ghc -- -fno-code input.hs
stack ghc -- -fno-code fixed.hs
```

## Pattern Examples

All patterns are demonstrated in realistic contexts, including:
- User input processing
- Message building
- Input validation
- String normalization
- ByteString conversions
