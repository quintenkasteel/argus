# Detection + Fix Golden Tests

This directory contains golden tests that verify Argus detection and auto-fix functionality end-to-end.

## Structure

Each test case is a directory containing:

```
test-case/
├── input.hs           # Source file with issues to detect
├── diagnostics.golden # Expected diagnostics (sorted JSON)
└── fixed.hs           # Expected output after single-pass fix
```

## Test Execution

The golden tests verify three properties:

1. **Detection**: Detected diagnostics match `diagnostics.golden` exactly
2. **Fix**: Output after applying fixes matches `fixed.hs` exactly (byte-for-byte)
3. **Convergence**: Fixes are idempotent - second pass produces no changes

## Creating a New Test Case

1. Create a new directory under `test/golden/detection-fix/`
2. Create `input.hs` with code containing issues
3. Run argus to generate expected diagnostics:
   ```bash
   stack run -- check your-case/input.hs --format=json
   ```
4. Create `diagnostics.golden` from the JSON output (sorted, simplified)
5. Run argus fix and capture output for `fixed.hs`:
   ```bash
   cp input.hs /tmp/test.hs
   stack run -- fix /tmp/test.hs
   cat /tmp/test.hs > fixed.hs
   ```
6. Run tests to verify:
   ```bash
   stack test --ta '-m "your-case"'
   ```

## diagnostics.golden Format

JSON array of objects with fields:
- `code`: Rule identifier (e.g., "redundant/if-true-false")
- `message`: Diagnostic message
- `severity`: "Error", "Warning", "Suggestion", or "Info"
- `hasFix`: Boolean indicating if auto-fix is available

Example:
```json
[{"code":"redundant/if-true-false","hasFix":true,"message":"'if x then True else False' is equivalent to 'x'","severity":"Warning"}]
```

## Important Notes

- The test framework uses a different configuration than the CLI
- Some rules enabled in CLI may not be enabled in tests
- Always verify expected output using the test framework, not CLI
- The `fixed.hs` file must match **single-pass** output exactly
- Character count must match exactly (no trailing newline differences)

## Current Test Cases

| Case | Description | Issues |
|------|-------------|--------|
| boolean-simplify | if-true-false, double-not, eq-true | 3 |
| list-operations | length-compare, concat-map, head-sort | 3 |
| modernization | return→pure, mapM→traverse, liftM→fmap | 3 |
| partial-functions | head, fromJust usage | 2 |
| space-leaks | foldl→foldl' | 1 |
| multi-rule | Multiple rules interacting | 4 |
| edge-cases | Unicode, pattern guards, nested exprs | 9 |

## Running Tests

```bash
# Run all golden tests
stack test --ta '-m "DetectionFixGolden"'

# Run specific test case
stack test --ta '-m "boolean-simplify"'

# Run with verbose output
stack test --ta '-m "DetectionFixGolden" --format=failed-examples'
```

## Troubleshooting

### Character Count Mismatch
If `fixed.hs` has wrong character count:
```bash
# Use printf without trailing newline
printf '%s' "$(cat /tmp/actual-output.hs)" > fixed.hs
wc -c fixed.hs  # Verify exact count
```

### Different Diagnostics Than CLI
The test framework uses `defaultConfig` which may have different rules enabled.
Always verify using the test output, not CLI output.

### Parse Errors
CPP directives (`#ifdef`, `#endif`) are not supported - remove them from test files.
