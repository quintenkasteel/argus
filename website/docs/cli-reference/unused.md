---
sidebar_position: 4
title: argus unused
description: Detect unused code, exports, and dependencies
---

# argus unused

Detect unused code, exports, imports, and dependencies across your Haskell project.

## Synopsis

```bash
argus unused [OPTIONS] [FILES|DIRECTORIES...]
```

## Description

The `unused` command performs deep analysis to find code that is defined but never used. This includes functions, types, exports, imports, and even entire modules. It uses HIE files for accurate cross-module analysis.

## Arguments

| Argument | Description |
|----------|-------------|
| `FILES` | Specific `.hs` files to analyze |
| `DIRECTORIES` | Directories to scan recursively |

## Options

### Detection Scope

| Option | Description |
|--------|-------------|
| `--functions` | Detect unused functions (default: on) |
| `--types` | Detect unused types and data constructors |
| `--exports` | Detect unnecessary exports |
| `--imports` | Detect unused imports |
| `--modules` | Detect entirely unused modules |
| `--all` | Enable all detection types |

### Analysis Options

| Option | Description |
|--------|-------------|
| `--public` | Only analyze public (exported) API |
| `--internal` | Only analyze internal (non-exported) code |
| `--cross-module` | Enable cross-module analysis (requires HIE) |
| `--entry-points MODULES` | Consider these modules as entry points |

### Filtering

| Option | Description |
|--------|-------------|
| `--include PATTERN` | Only analyze matching files |
| `--exclude PATTERN` | Skip matching files |
| `--ignore-tests` | Exclude test modules from analysis |
| `--ignore-main` | Exclude Main modules |
| `--min-lines N` | Only report functions with N+ lines |

### Output

| Option | Description |
|--------|-------------|
| `-f, --format FORMAT` | Output format |
| `-o, --output FILE` | Write output to file |
| `--stats` | Show usage statistics |
| `--by-module` | Group results by module |
| `--by-type` | Group results by type (function, type, etc.) |

## Examples

### Basic Usage

```bash
# Find all unused code
argus unused src/

# Specific detection types
argus unused --functions src/
argus unused --imports src/
argus unused --exports src/

# Enable all detection
argus unused --all src/
```

### Unused Functions

```bash
# Find unused functions
argus unused --functions src/
```

Output:

```
Unused Functions
================

src/Utils/Helpers.hs:34:1
  Function: formatDate
  Lines: 34-45 (12 lines)
  Defined but never referenced

src/Api/Internal.hs:78:1
  Function: validateInput
  Lines: 78-92 (15 lines)
  Only used in commented code

Found 2 unused functions (27 lines of dead code)
```

### Unused Exports

```bash
# Find unnecessary exports
argus unused --exports src/
```

Output:

```
Unnecessary Exports
===================

src/Types.hs
  Module exports 45 items, 12 are unused outside module:
    - internalHelper    (line 23)
    - debugFormat       (line 45)
    - legacyParser      (line 67, deprecated)
    ...

src/Api/Types.hs
  Module exports 20 items, 3 are unused:
    - OldRequestType    (line 12)
    - parseOldFormat    (line 34)
    - convertLegacy     (line 56)

Consider removing from export list or marking internal.
```

### Unused Imports

```bash
# Find unused imports
argus unused --imports src/
```

Output:

```
Unused Imports
==============

src/Handler.hs:5:1
  import Data.Map (Map, lookup, insert, delete)
         ~~~~~~~~
  Unused: delete
  Suggestion: import Data.Map (Map, lookup, insert)

src/Handler.hs:7:1
  import Control.Monad (when, unless, forM_)
         ~~~~~~~~~~~~~~
  Entirely unused
  Suggestion: Remove import

src/Parser.hs:12:1
  import qualified Data.Text as T
         ~~~~~~~~~~~~~~~~~~~~~~~~
  Unused qualified import
  Suggestion: Remove import
```

### Unused Modules

```bash
# Find entirely unused modules
argus unused --modules src/
```

Output:

```
Unused Modules
==============

src/Legacy/OldParser.hs
  Module not imported anywhere
  Lines: 234
  Last modified: 2023-06-15

src/Utils/Deprecated.hs
  Module not imported anywhere
  Lines: 89
  Last modified: 2022-11-20

src/Internal/Debug.hs
  Only imported by unused module src/Legacy/OldParser.hs
  Lines: 45

Total: 3 unused modules (368 lines)
```

### Cross-Module Analysis

```bash
# Full cross-module unused detection
argus unused --cross-module --all src/
```

This requires HIE files for accurate results:

```bash
# Build with HIE files first
stack build --ghc-options="-fwrite-ide-info -hiedir=.hie"

# Then analyze
argus unused --cross-module src/
```

### Entry Point Configuration

```bash
# Specify entry points (code reachable from here is "used")
argus unused --entry-points "Main,App.Server,App.CLI" src/

# For libraries, specify public API
argus unused --entry-points "MyLib,MyLib.Public" src/
```

### Filtering Results

```bash
# Ignore test modules
argus unused --ignore-tests src/

# Only report substantial functions (10+ lines)
argus unused --min-lines 10 src/

# Exclude specific directories
argus unused --exclude "Generated/*" --exclude "Paths_*" src/
```

### Output Formats

```bash
# JSON output
argus unused -f json src/ -o unused.json

# Group by module
argus unused --by-module src/

# Group by type
argus unused --by-type src/
```

### Statistics

```bash
argus unused --stats src/
```

Output:

```
Unused Code Statistics
======================

Total definitions analyzed: 1,234
  Functions:    890
  Types:        234
  Exports:      567

Unused:
  Functions:     23 (2.6%)
  Types:          5 (2.1%)
  Exports:       34 (6.0%)
  Imports:       56
  Modules:        3

Dead code lines: 456 (3.2% of codebase)

By Module (top 5):
  Legacy/Parser.hs:      156 lines unused
  Utils/Deprecated.hs:    89 lines unused
  Internal/Debug.hs:      45 lines unused
  Api/V1/Handlers.hs:     32 lines unused
  Types/Internal.hs:      28 lines unused
```

## Understanding Results

### Function Usage

A function is considered "used" if:
- Called directly in code
- Passed as an argument
- Used in a type signature
- Exported and imported elsewhere
- Referenced in Template Haskell

### Export Analysis

An export is "unnecessary" if:
- Never imported by any other module
- Only imported by modules in the same package
- Only used internally within the module

### Import Analysis

An import is "unused" if:
- None of the imported names are used
- Qualified prefix is never used
- Implicitly imported module has no effects

## Configuration

```toml
[unused]
enabled = true

# Detection types
functions = true
types = true
exports = true
imports = true
modules = true

# Entry points for reachability analysis
entry-points = ["Main", "Lib"]

# Ignore patterns
ignore = [
  "**/Test/**",
  "**/Spec.hs",
  "**/Paths_*.hs",
]

# Minimum lines for function reporting
min-lines = 5

# Cross-module analysis
cross-module = true
```

## Integration with CI

```yaml
# GitHub Actions example
- name: Check for unused code
  run: |
    argus unused --all --cross-module src/

# Fail if unused exports exceed threshold
- name: Check unused exports
  run: |
    COUNT=$(argus unused --exports -f json src/ | jq '.count')
    if [ "$COUNT" -gt 10 ]; then
      echo "Too many unused exports: $COUNT"
      exit 1
    fi
```

## Auto-Fix Support

Some unused code issues can be fixed automatically:

```bash
# Remove unused imports
argus fix --rules "import/unused" src/

# Note: Removing unused functions/exports requires manual review
```

## Exit Codes

| Code | Condition |
|------|-----------|
| `0` | Analysis complete (unused code may exist) |
| `1` | Unused code found (with `--fail-on-unused`) |
| `2` | Configuration error |
| `3` | File not found |
| `4` | HIE files required but missing |

## See Also

- **[argus check](./check)**: Full static analysis
- **[Import Rules](../rules/imports)**: Import-related rules
- **[Working with HIE](../usage-guide/working-with-hie)**: HIE file setup
- **[Configuration](../configuration/rules-section)**: Unused detection config
