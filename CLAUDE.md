# Argus - Haskell Static Analyzer

Enterprise-grade static analysis for Haskell: syntactic pattern matching, semantic HIE analysis, GHC plugin integration.

## Build Commands

**Use `make` targets exclusively.** Run `make help` for the complete list.

| Command | Purpose |
|---------|---------|
| `make dev` | Fast development build (-O0) |
| `make watch` | ghcid watch mode (instant feedback) |
| `make test-fast` | Quick test run |
| `make ci` | Full CI check - **must pass before commit** |
| `make build-strict` | Build with -Werror |
| `make run ARGS="..."` | Run argus with arguments |

### Workflow

```bash
make dev          # Iterate
make test-fast    # Verify
make ci           # Before commit
```

### Testing

```bash
make test                                           # All tests
make test-fast                                      # Quick (reuses build)
make run ARGS="test --test-arguments='-m Pattern'"  # Specific tests
make coverage-report                                # HTML coverage
```

## Project Structure

```
src/Argus/
├── Types.hs      # Core types: Diagnostic, Fix, SrcSpan
├── Core.hs       # Main orchestration
├── Rules/        # Linting rules (Engine, ASTMatch, DSL)
├── Refactor/     # Auto-fix (SafeRefactor, ExactPrint)
├── Analysis/     # Syntactic, Semantic, DepGraph, Unused
├── HIE/          # Type-aware analysis via HIE files
└── Output/       # Formatters (Terminal, Json, Sarif)
```

## Coding Conventions

- **Newtypes**: Use semantic types (`Line`, `Column`, `Seconds`)
- **Strictness**: `StrictData` enabled; bang patterns for accumulators
- **Deriving**: Explicit `deriving stock` or `deriving anyclass`
- **Imports**: Qualified for `Data.Map`, `Data.Set`, `Data.Text`
- **Errors**: `Either`/`Maybe` composition; avoid exceptions

## Rules

### Do

- Fix warnings immediately (enforced in CI with `-Werror`)
- Use `make ci` before every commit
- Handle partial functions explicitly (`head`, `tail`, `!!`, `fromJust`)
- Add tests in `test/*Spec.hs` for new rules

### Do Not

- Run `stack` or `cabal` directly (use `make` targets)
- Simplify test cases when debugging - fix with exact types
- Add stub functions, `undefined`, `error`, or TODO placeholders
- Commit code that doesn't pass `make ci`
- Add time estimates to plans

## Adding Rules

1. Add rule in appropriate `src/Argus/Rules/*.hs` module
2. Register in `Argus.Rules.Engine.allBuiltinRules`
3. Add tests in `test/*Spec.hs`

## Configuration

Project config: `argus.toml` or `linter.toml`
- `[rules]`: Enable/disable rules
- `[naming]`: Naming conventions
- `[complexity]`: Thresholds

## References

- @package.yaml - Dependencies and build config
- @WARNINGS_POLICY.md - Warning tier definitions
- @.claude/rules/ghc-api.md - GHC 9.10.3 constructor signatures
