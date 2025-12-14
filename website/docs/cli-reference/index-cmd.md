---
sidebar_position: 6
title: argus index
description: Build and manage HIE file index for semantic analysis
---

# argus index

Build and manage the HIE file index used for semantic analysis.

## Synopsis

```bash
argus index [OPTIONS] [DIRECTORIES...]
```

## Description

The `index` command builds a searchable index from HIE (Haskell Interface Extended) files. This index enables fast semantic queries for type information, cross-references, and symbol lookups.

## Arguments

| Argument | Description |
|----------|-------------|
| `DIRECTORIES` | Directories containing HIE files (default: `.hie`) |

## Options

### Build Options

| Option | Description |
|--------|-------------|
| `--rebuild` | Force complete index rebuild |
| `--update` | Update index with changed files only |
| `--clean` | Remove existing index before building |
| `--verify` | Verify HIE files are valid |

### Source Options

| Option | Description |
|--------|-------------|
| `--hie-dir DIR` | HIE files directory (default: `.hie`) |
| `--src-dirs DIRS` | Source directories to correlate |
| `--include PATTERN` | Only index matching HIE files |
| `--exclude PATTERN` | Skip matching HIE files |

### Index Location

| Option | Description |
|--------|-------------|
| `--index-dir DIR` | Index storage directory |
| `--in-memory` | Keep index in memory only |
| `--database FILE` | SQLite database for index |

### Output

| Option | Description |
|--------|-------------|
| `--stats` | Show indexing statistics |
| `--progress` | Show progress during indexing |
| `-v, --verbose` | Verbose output |
| `-q, --quiet` | Minimal output |

### Query Options

| Option | Description |
|--------|-------------|
| `--query SYMBOL` | Query index for symbol |
| `--type-of EXPR` | Look up type of expression |
| `--references NAME` | Find all references to name |
| `--definitions NAME` | Find definitions of name |

## Prerequisites

HIE files must be generated during compilation:

```bash
# Stack
stack build --ghc-options="-fwrite-ide-info -hiedir=.hie"

# Configure in stack.yaml for permanent setting
ghc-options:
  "$locals": -fwrite-ide-info -hiedir=.hie
```

## Examples

### Building the Index

```bash
# Build index from default .hie directory
argus index

# Build from specific directory
argus index .hie/

# Rebuild entire index
argus index --rebuild

# Update changed files only
argus index --update

# With progress output
argus index --progress
```

### Index Management

```bash
# Clean and rebuild
argus index --clean --rebuild

# Verify HIE files are valid
argus index --verify

# Custom index location
argus index --index-dir .argus-index/
```

### Querying the Index

```bash
# Find all references to a function
argus index --references "processItem"

# Find definitions
argus index --definitions "Handler"

# Look up type
argus index --type-of "processItem"

# Query specific symbol
argus index --query "Data.Map.lookup"
```

### Advanced Usage

```bash
# Index specific HIE files
argus index --include "src/**/*.hie" .hie/

# Exclude test HIE files
argus index --exclude "*Spec.hie" --exclude "*Test.hie" .hie/

# Correlate with source directories
argus index --src-dirs "src,app" .hie/
```

## Index Statistics

```bash
argus index --stats
```

Output:

```
HIE Index Statistics
====================

Index Location: .argus-index/
Index Size:     12.3 MB
Last Updated:   2024-01-15 14:32:00

HIE Files:
  Total:        234
  Indexed:      234
  Skipped:      0
  Errors:       0

Symbols:
  Types:        1,234
  Functions:    5,678
  Classes:      89
  Instances:    234

References:
  Total:        45,678
  Cross-module: 12,345

Build Time:     3.2s
```

## Query Results

### References Query

```bash
argus index --references "processItem"
```

Output:

```
References to 'processItem'
===========================

Definition:
  src/Processor.hs:45:1
    processItem :: Item -> IO Result

References (12):
  src/Handler.hs:23:15      result <- processItem item
  src/Handler.hs:67:8       mapM processItem items
  src/Batch.hs:34:12        traverse processItem batch
  src/Api.hs:89:5           liftIO $ processItem req
  test/ProcessorSpec.hs:15  processItem testItem
  ...
```

### Type Query

```bash
argus index --type-of "processItem"
```

Output:

```
Type of 'processItem'
=====================

  processItem :: Item -> IO Result

Full type (expanded):
  processItem :: Item -> IO Result
    where Item = Record { itemId :: Int, itemName :: Text }
          Result = Either Error Success

Defined at: src/Processor.hs:45:1
```

### Definitions Query

```bash
argus index --definitions "Handler"
```

Output:

```
Definitions of 'Handler'
========================

Type:
  src/Types.hs:23:1
    type Handler a = ReaderT Config (ExceptT Error IO) a

Type Alias:
  src/Api/Types.hs:12:1
    type Handler = Servant.Handler

Data Constructor:
  src/Internal/Handler.hs:5:1
    data Handler = Handler { ... }
```

## Index Format

The index stores:

- **Symbol table**: All defined names with locations
- **Type information**: Types for expressions and bindings
- **Reference graph**: Cross-references between definitions
- **Module graph**: Import/export relationships
- **Source correlation**: HIE to source file mapping

## Performance

### Index Size

| Project Size | HIE Files | Index Size | Build Time |
|--------------|-----------|------------|------------|
| Small (10k LOC) | ~50 | ~5 MB | ~1s |
| Medium (50k LOC) | ~200 | ~25 MB | ~5s |
| Large (200k LOC) | ~800 | ~100 MB | ~20s |

### Query Performance

- Symbol lookup: under 1ms
- Reference search: under 10ms
- Type query: under 5ms
- Full text search: under 100ms

## Incremental Updates

```bash
# First build: full index
argus index

# After code changes: incremental update
argus index --update
```

Incremental updates:
1. Detect changed HIE files (by timestamp)
2. Remove old entries for changed modules
3. Index only changed modules
4. Update cross-references

## Integration with Check

```bash
# Build index, then check
argus index && argus check --full src/

# Or let check build index automatically
argus check --full --rebuild-hie src/
```

## Daemon Integration

With the daemon running:

```bash
# Daemon maintains index in memory
argus daemon start

# Queries use daemon's index (instant)
argus index --query "lookup" --daemon

# Index updates are automatic
```

## Configuration

```toml
[hie]
# HIE files directory
directory = ".hie"

# Index storage
index-dir = ".argus-index"

# Auto-rebuild on check
auto-rebuild = true

# Index timeout (seconds)
timeout = 300

# Exclude from indexing
exclude = [
  "**/Paths_*.hie",
  "**/Setup.hie",
]
```

## Troubleshooting

### Missing HIE Files

```
Error: No HIE files found in .hie/

  Ensure you build with HIE generation:
    stack build --ghc-options="-fwrite-ide-info -hiedir=.hie"

  Or add to stack.yaml:
    ghc-options:
      "$locals": -fwrite-ide-info -hiedir=.hie
```

### HIE Version Mismatch

```
Error: HIE file version mismatch
  File: .hie/src/Main.hie
  Expected: 9103
  Found: 9003

  Rebuild with current GHC version:
    stack clean && stack build
```

### Corrupt Index

```bash
# Clean and rebuild
argus index --clean --rebuild --verify
```

## Exit Codes

| Code | Condition |
|------|-----------|
| `0` | Index built/updated successfully |
| `1` | Some HIE files failed to index |
| `2` | No HIE files found |
| `3` | Index corruption detected |
| `4` | Query returned no results |

## See Also

- **[Working with HIE](../usage-guide/working-with-hie)**: HIE file deep dive
- **[argus check](./check)**: Using the index for analysis
- **[Analysis Modes](../usage-guide/analysis-modes)**: Full vs quick analysis
- **[argus daemon](./daemon)**: Persistent index with daemon
