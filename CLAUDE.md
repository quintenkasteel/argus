# Argus - Haskell Static Analyzer

Enterprise-grade static analysis tool for Haskell combining syntactic pattern matching, semantic HIE analysis, and GHC plugin integration.

## Build System

**IMPORTANT: Use Stack exclusively, NOT Cabal.**

### Development Workflow

```bash
# Fast iteration (recommended for daily development)
make dev              # Fast build with warnings shown
make watch            # ghcid watch mode (instant feedback)

# Standard builds
make build            # Pedantic build with GC tuning
make build-strict     # Build with -Werror

# Testing
make test             # Run all tests
make test-fast        # Quick test without full rebuild

# Quality checks
make check-warnings   # Count warnings in codebase
make lint             # Run argus on itself
make ci               # CI-equivalent (strict build + tests)
```

### Direct Stack Commands

```bash
stack build              # Build the project
stack test               # Run all tests
stack test --ta '-m "pattern"'  # Run specific tests
stack run -- check src/  # Run linter on source
stack run -- fix src/    # Auto-fix issues
stack ghci               # REPL with project loaded
```

- **Resolver**: LTS 24.21 (GHC 9.10.3)
- **Package config**: `package.yaml` (Hpack generates `argus.cabal`)

### Warning Policy

- **Warnings are enforced in CI** - all PRs must build cleanly with `--pedantic`
- Fix warnings as you go; don't accumulate technical debt
- See `WARNINGS_POLICY.md` for tier definitions and exception handling
- Before committing: `make build-strict` must pass

## Project Structure

```
src/Argus/
├── Types.hs           # Core types: Diagnostic, Fix, SrcSpan
├── Core.hs            # Main orchestration engine
├── CLI.hs             # Command-line interface
├── Config.hs          # TOML/YAML configuration
├── Analysis/          # Analysis modules (Syntactic, Semantic, DepGraph, Unused, etc.)
├── Rules/             # Linting rules (Engine, ASTMatch, DSL, Partial, Security, etc.)
├── Refactor/          # Auto-fix engine (Engine, ExactPrint, Substitution, SafeRefactor)
├── Output/            # Formatters (Terminal, Json, Sarif, Html, JUnit)
├── HIE/               # HIE file analysis (Query, TypeInfo, SymbolTable)
└── Imports/           # Import management
```

## Key Modules

| Module                    | Purpose                        |
| ------------------------- | ------------------------------ |
| `Argus.Rules.Engine`      | Unified generic rule evaluator |
| `Argus.Rules.ASTMatch`    | AST pattern matching via SYB   |
| `Argus.Rules.DSL`         | DSL for custom rules           |
| `Argus.Refactor.Engine`   | Safe automatic fixing          |
| `Argus.Analysis.Semantic` | HIE-based type-aware analysis  |
| `Argus.HIE.TypeInfo`      | Type information extraction    |

## Coding Conventions

- **Type safety**: Use semantic newtypes (`Line`, `Column`, `Seconds`)
- **Strictness**: `StrictData` enabled globally; use bang patterns for accumulator arguments
- **Deriving**: Always use `deriving stock` or `deriving anyclass` explicitly
- **Imports**: Qualified imports for `Data.Map`, `Data.Set`, `Data.Text`
- **Error handling**: Use `Either`/`Maybe` composition, avoid exceptions in main flow
- **DO NOT simplify test cases** - fix issues properly with exact Haskell types

## GHC API (GHC 9.10.3)

Stack uses native GHC 9.10.3. Key constructor signatures:

| Constructor | Args | Signature                          |
| ----------- | ---- | ---------------------------------- |
| `HsPar`     | 2    | `HsPar ext expr`                   |
| `HsLam`     | 3    | `HsLam ext lamCase matchGroup`     |
| `HsLet`     | 3    | `HsLet ext binds body`             |
| `ParPat`    | 2    | `ParPat ext pat`                   |
| `AsPat`     | 3    | `AsPat ext name pat`               |
| `HsAppType` | 3    | `HsAppType ext expr tyArg`         |

## CLI Commands

```bash
argus check [FILES]     # Static analysis
argus fix [FILES]       # Auto-fix with preview
argus unused [FILES]    # Unused code detection
argus init              # Config initialization
argus watch [FILES]     # File-watch mode
argus daemon start      # Background daemon
argus lsp               # LSP server mode
```

## Testing

- Framework: Hspec + QuickCheck
- Test files: `test/*Spec.hs` (60+ spec files)
- Test data: `test/data/` and `test/data-result/`

```bash
stack test                           # All tests
stack test --ta '-m "ASTMatch"'      # Specific module
stack test --ta '--seed 12345'       # Reproducible QuickCheck
```

## Configuration

Project config in `argus.toml` or `linter.toml`:

- `[general]`: Directories, exclusions, analysis mode
- `[rules]`: Enable/disable specific rules
- `[naming]`: Type/variable naming patterns
- `[complexity]`: Thresholds for complexity warnings

## Common Patterns

### Adding a New Rule

1. Add rule descriptor in appropriate `Rules/*.hs` module
2. Register in `Argus.Rules.Engine.allBuiltinRules`
3. Add tests in corresponding `test/*Spec.hs`

### Working with HIE Files

```bash
stack run -- index .     # Build HIE database
# Ensure stack.yaml has: ghc-options: -fwrite-ide-info -hiedir=.hie
```

## Do Not

- Use Cabal directly (use Stack)
- Commit `.hie/` directory contents
- Add time estimates to plans
- Simplify test cases when debugging
- Include stub functions, undefined, error, or TODO placeholders
- Disable warnings globally (fix the code instead)
- Leave warnings unfixed (fix as you go)
- Use partial functions without explicit handling (head, tail, !!, etc.)
