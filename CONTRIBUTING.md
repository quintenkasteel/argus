# Contributing to Argus

Thank you for your interest in contributing to Argus! This document provides guidelines and instructions for contributing.

## Table of Contents

1. [Code of Conduct](#code-of-conduct)
2. [Getting Started](#getting-started)
3. [Development Setup](#development-setup)
4. [Development Workflow](#development-workflow)
5. [Coding Standards](#coding-standards)
6. [Testing Requirements](#testing-requirements)
7. [Pull Request Process](#pull-request-process)
8. [Adding New Rules](#adding-new-rules)
9. [Documentation](#documentation)

---

## Code of Conduct

- Be respectful and inclusive
- Focus on constructive feedback
- Help others learn and grow
- Keep discussions technical and on-topic

---

## Getting Started

### Prerequisites

- **GHC 9.10.3** (via Stack LTS 24.21)
- **Stack** build tool
- Git

### Quick Start

```bash
# Clone the repository
git clone https://github.com/quintenkasteel/argus.git
cd argus

# Build and test
make ci

# Run development build
make dev
```

---

## Development Setup

### Build System

Argus uses **Stack** exclusively via Makefile targets. Do not use `cabal` or `stack` directly.

| Command | Purpose |
|---------|---------|
| `make dev` | Fast development build (-O0) |
| `make watch` | ghcid watch mode for instant feedback |
| `make test-fast` | Run tests quickly |
| `make ci` | Full CI check (must pass before commit) |
| `make build-strict` | Build with -Werror |
| `make docs` | Generate Haddock documentation |

### Recommended Workflow

```bash
# 1. Make changes
vim src/Argus/Rules/MyRule.hs

# 2. Quick iteration
make dev

# 3. Verify tests
make test-fast

# 4. Full CI check before commit
make ci
```

---

## Development Workflow

### Branch Naming

- `feature/description` - New features
- `fix/description` - Bug fixes
- `docs/description` - Documentation changes
- `refactor/description` - Code refactoring

### Commit Messages

Use clear, descriptive commit messages:

```
Add strictness rule for lazy accumulator patterns

- Detect foldl with non-strict accumulator
- Suggest foldl' replacement
- Add auto-fix support
```

### Before Committing

1. Run `make ci` - must pass
2. Update tests if behavior changed
3. Update documentation if API changed
4. Add CHANGELOG.md entry for user-facing changes

---

## Coding Standards

### Haskell Style

#### Imports

```haskell
-- Qualified imports for common modules
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

-- Explicit imports for types
import Argus.Types (Diagnostic (..), Fix (..), SrcSpan (..))
```

#### Strictness

```haskell
-- StrictData is enabled by default
-- Use bang patterns for accumulators
go !acc [] = acc
go !acc (x:xs) = go (acc + x) xs
```

#### Deriving

```haskell
-- Always use explicit deriving strategies
data MyType = MyType
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
```

#### Newtypes

```haskell
-- Use newtypes for semantic types
newtype Line = Line { unLine :: Int }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num)
```

### Error Handling

```haskell
-- Use Either/Maybe composition, not exceptions
processFile :: FilePath -> IO (Either AnalysisError [Diagnostic])
processFile path = runExceptT $ do
  content <- ExceptT $ readFileEither path
  parsed <- ExceptT $ pure $ parseModule content
  pure $ analyze parsed

-- Avoid partial functions
-- BAD:  head xs
-- GOOD: listToMaybe xs
```

### Documentation

```haskell
-- | Brief description of the function.
--
-- More detailed explanation if needed.
--
-- ==== Examples
--
-- >>> myFunction 42
-- Right "success"
--
myFunction :: Int -> Either Error Text
myFunction n = ...
```

---

## Testing Requirements

### Test Coverage

All new code must include tests:

- **Unit tests** for individual functions
- **Property tests** for invariants (using QuickCheck)
- **Golden tests** for detection/fix pairs

### Test Location

```
test/
├── MyModuleSpec.hs       # Unit tests
├── MyPropertySpec.hs     # Property tests
└── golden/
    └── my-rule/          # Golden test data
        ├── input.hs
        ├── diagnostics.golden
        └── fixed.hs
```

### Running Tests

```bash
# All tests
make test

# Fast (reuses build)
make test-fast

# Specific tests
stack test --test-arguments='-m "pattern"'
```

### Golden Tests

For rule detection and fixes:

```haskell
-- test/golden/my-rule/input.hs
module Test where

badCode = head []  -- Should trigger rule

-- test/golden/my-rule/diagnostics.golden
input.hs:4:11: warning: [partial/head]
  Use of partial function 'head'

-- test/golden/my-rule/fixed.hs
module Test where

badCode = Safe.headMay []  -- Fixed version
```

---

## Pull Request Process

### Checklist

Before submitting:

- [ ] `make ci` passes
- [ ] Tests added/updated
- [ ] Documentation updated
- [ ] CHANGELOG.md entry added (if user-facing)
- [ ] No `undefined`, `error`, or TODO placeholders

### Review Process

1. Create PR with clear description
2. Address reviewer feedback
3. Ensure CI passes
4. Squash commits if requested
5. Maintainer merges

### PR Template

```markdown
## Summary

Brief description of changes.

## Changes

- Change 1
- Change 2

## Testing

How was this tested?

## Checklist

- [ ] `make ci` passes
- [ ] Tests added
- [ ] Docs updated
```

---

## Adding New Rules

### Rule Location

Add rules in the appropriate category under `src/Argus/Rules/Builtin/`:

| Category | Module | Description |
|----------|--------|-------------|
| Safety | `Safety.hs` | Partial functions, unsafe ops |
| Performance | `Performance.hs` | Space leaks, strictness |
| Security | `Security.hs` | Injection, crypto issues |
| Style | `Style.hs` | Naming, formatting |

### Rule Structure

```haskell
-- src/Argus/Rules/Builtin/Safety.hs

myNewRule :: Rule
myNewRule = mkRule "safety/my-rule"
  & setCategory Safety
  & setSeverity SWarning
  & setMessage "Description of the issue"
  & matchExpr pattern
  & withFix fixAction
  where
    pattern = ... -- AST pattern to match
    fixAction = ... -- Fix to apply
```

### Rule Registration

Register in `Argus.Rules.Engine.allBuiltinRules`:

```haskell
allBuiltinRules :: [Rule]
allBuiltinRules =
  [ -- ... existing rules
  , Safety.myNewRule  -- Add new rule
  ]
```

### Rule Tests

Add tests in the corresponding spec file:

```haskell
-- test/SafetyRulesSpec.hs
describe "myNewRule" $ do
  it "detects the pattern" $ do
    let code = "badCode = unsafeFunction x"
    diagnostics <- runRule myNewRule code
    length diagnostics `shouldBe` 1

  it "provides correct fix" $ do
    let code = "badCode = unsafeFunction x"
    fixed <- applyFix myNewRule code
    fixed `shouldBe` "badCode = safeFunction x"
```

---

## Documentation

### Code Documentation

- All exported functions must have Haddock comments
- Module headers required
- Examples for complex functions

### User Documentation

- Update README.md for user-facing features
- Add CLI help text for new commands
- Update rule documentation

### API Documentation

Generate with:

```bash
make docs
make docs-open  # Opens in browser
```

---

## Questions?

- Open an issue for questions or discussions
- Check existing issues before creating new ones
- Use descriptive titles and include relevant context

---

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
