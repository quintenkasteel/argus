---
sidebar_position: 1
title: Analysis Modes
description: Understanding Quick, Full, and Plugin analysis modes
---

# Analysis Modes

Argus provides three analysis modes, each with different trade-offs between speed, depth, and setup requirements.

## Mode Comparison

| Feature | Quick Mode | Full Mode | Plugin Mode |
|---------|------------|-----------|-------------|
| Compilation required | No | Yes (HIE files) | Yes (with plugin) |
| Speed | ~1000 files/sec | Varies by HIE size | Per-module at compile |
| Type information | No | Yes | Yes |
| Cross-module analysis | Limited | Full | Full |
| Setup complexity | None | Medium | High |

## Quick Mode

Quick mode performs syntax-only analysis using GHC's parser. It's the default mode and requires no special setup.

### When to Use

- **Development iteration**: Fast feedback while coding
- **Pre-commit hooks**: Quick checks before commits
- **Initial exploration**: Assessing a new codebase
- **CI quick-fail**: Early exit on obvious issues

### Capabilities

```bash
argus check --mode quick src/
```

Quick mode detects:

| Category | Examples |
|----------|----------|
| Partial functions | `head`, `tail`, `fromJust`, `!!` |
| Security patterns | `unsafePerformIO`, `trace`, `system` |
| Space leak patterns | `foldl`, lazy `State`, lazy IO |
| Naming violations | Type and variable naming conventions |
| Import issues | Unused imports, missing qualified |
| Pragma problems | Unused LANGUAGE extensions |
| Simple performance | `length xs == 0`, `concat . map` |

### Limitations

Quick mode cannot:
- Resolve wildcard imports (uses heuristics)
- Detect unused exports reliably
- Analyze Template Haskell generated code
- Perform cross-module analysis
- Validate type-level rules

### Example Output

```
src/MyModule.hs:42:5: warning: [partial/head]
    Use of partial function 'head': Crashes on empty list.
    |
 42 | let first = head items
    |             ^^^^
```

## Full Mode

Full mode uses HIE (Haskell Interface Extended) files for deep semantic analysis with full type information.

### When to Use

- **Pre-release checks**: Thorough analysis before deployment
- **Unused code detection**: Finding truly dead code
- **Architecture analysis**: Enforcing module boundaries
- **Type-aware rules**: Rules that need type information

### Setup Requirements

1. **Generate HIE files** during compilation:

```yaml
# stack.yaml
ghc-options:
  "$locals":
    -fwrite-ide-info
    -hiedir=.hie
```

2. **Build your project**:

```bash
stack build
```

3. **Run full mode**:

```bash
argus check --mode full --hie-dir .hie src/
```

### Additional Capabilities

Full mode adds:

| Capability | Description |
|------------|-------------|
| Type-aware unused detection | Knows which types are actually used |
| Wildcard import resolution | Precisely identifies unused wildcard imports |
| TH awareness | Understands Template Haskell generated code |
| Cross-module analysis | Tracks dependencies across modules |
| Architecture validation | Enforces layer boundaries |
| Coupling metrics | Measures module coupling |

### Example: Type-Aware Unused Detection

```haskell
-- In quick mode: might miss that `helper` is used via TH
-- In full mode: correctly identifies usage through makeLenses
module MyModule where

import Control.Lens (makeLenses)

data Config = Config { _configPath :: FilePath }
makeLenses ''Config

-- Full mode knows `configPath` lens is generated and used
```

### Example: Wildcard Import Resolution

```haskell
import Data.Map (*)  -- Quick mode can't check this precisely

-- Full mode:
-- Warning: Data.Map imported but only 'lookup' and 'insert' used
-- Suggestion: import Data.Map (lookup, insert)
```

### Performance Considerations

Full mode performance depends on:
- Size of HIE files
- Number of cross-module dependencies
- Complexity of type-level rules

Optimize with:
```bash
# Use caching for incremental analysis
argus check --mode full --cache src/

# Parallel analysis
argus check --mode full --jobs 4 src/
```

## Plugin Mode

Plugin mode integrates Argus directly into GHC's compilation pipeline for maximum precision.

### When to Use

- **Compile-time enforcement**: Fail builds on violations
- **Maximum precision**: Analysis with full GHC context
- **Strictness analysis**: Deep strictness checking
- **Core analysis**: Analyze GHC Core output

### Setup

```yaml
# stack.yaml
extra-deps:
  - argus-1.0.0

ghc-options:
  "$locals":
    -fplugin=Argus.Plugin
    -fplugin-opt=Argus.Plugin:config=argus.toml
```

### Plugin Options

```yaml
ghc-options:
  "$locals":
    -fplugin=Argus.Plugin
    -fplugin-opt=Argus.Plugin:config=argus.toml
    -fplugin-opt=Argus.Plugin:severity=warning  # or error
    -fplugin-opt=Argus.Plugin:rules=partial,security
```

### Plugin-Only Features

| Feature | Description |
|---------|-------------|
| Core analysis | Analyze GHC Core for optimization issues |
| Strictness validation | Verify strictness annotations |
| Type-level enforcement | Compile-time type rule checking |
| Demand analysis | Detect unused arguments in Core |

### Example: Compile-Time Errors

```bash
$ stack build
[1 of 5] Compiling MyModule

MyModule.hs:42:5: error: [Argus/partial/head]
    Use of partial function 'head'
    |
 42 | let first = head items
    |             ^^^^

Build failed.
```

## Mode Selection Strategy

### Development Workflow

```bash
# While coding - fast feedback
argus check --mode quick src/

# Before committing - more thorough
stack build  # generates HIE
argus check --mode full src/
```

### CI Pipeline

```yaml
stages:
  - quick-check  # Fast fail on obvious issues
  - build        # Generate HIE files
  - full-check   # Deep analysis

quick-check:
  script: argus check --mode quick src/

build:
  script: stack build

full-check:
  needs: [build]
  script: argus check --mode full src/
```

### Gradual Adoption

1. **Start with quick mode** on everything
2. **Enable full mode** on critical modules
3. **Add plugin mode** for core components

```toml
# Different configs for different modules
# argus-quick.toml
[general]
mode = "quick"

# argus-full.toml
[general]
mode = "full"
directories = ["src/Core", "src/Domain"]
```

## Mixing Modes

You can run different modes on different parts of your codebase:

```bash
# Quick mode on everything
argus check --mode quick src/

# Full mode on critical modules only
argus check --mode full --hie-dir .hie src/Core/ src/Security/
```

## Mode-Specific Configuration

Some rules behave differently per mode:

```toml
[unused]
# Quick mode uses heuristics
# Full mode uses HIE data
enabled = true

# These are more accurate in full mode
check-exports = true
check-imports = true
```

## Next Steps

- **[Auto-Fix](./auto-fix)**: Learn about the safe auto-fix system
- **[Working with HIE](./working-with-hie)**: Deep dive into HIE file analysis
- **[CI Integration](./ci-integration)**: Set up automated analysis
