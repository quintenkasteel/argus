# Argus CLI Reference

Complete reference for all 13 Argus commands and their options.

## Global Options

These options apply to all commands:

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| config | `-c`, `--config` | (search) | Path to configuration file |
| verbose | `-v`, `--verbose` | false | Enable verbose output |
| no-color | `--no-color` | false | Disable colored output |
| parallel | `-j`, `--parallel` | 4 | Number of parallel jobs |

Configuration file search order: `argus.toml`, `linter.toml`, `.linter.toml`, `linter.yaml`, `.linter.yaml`, `config.yaml`.

---

## check

Run static analysis on Haskell source files.

### Synopsis

```bash
argus check [TARGETS...] [OPTIONS]
```

### Options

#### Analysis

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| mode | `-m`, `--mode` | quick | Analysis mode: quick, full, plugin |
| hie-dir | `--hie-dir` | .hie | Directory containing HIE files |

#### Output

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| format | `-f`, `--format` | terminal | Output format (see Output Formats) |
| group-by | `--group-by` | file | Group by: file, rule, severity |
| context | `--context` | false | Show source context |
| context-lines | `--context-lines` | 2 | Lines of context to show |

#### CI/CD

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| baseline | `-b`, `--baseline` | - | Baseline file for comparison |
| fail-on-new | `--fail-on-new` | false | Exit 1 if new issues vs baseline |
| fail-on-severity | `--fail-on-severity` | - | Exit 1 at severity threshold |
| fail-on-count | `--fail-on-count` | - | Exit 1 if total exceeds N |
| fail-on-delta | `--fail-on-delta` | - | Exit 1 if net new exceeds N |
| update-baseline | `--update-baseline` | false | Update baseline after analysis |
| ci-quiet | `--ci-quiet` | false | Minimal CI output |

### Examples

```bash
# Basic analysis
argus check src/

# Full mode with HIE
argus check --mode full --hie-dir .hie src/

# SARIF output for GitHub
argus check --format sarif src/ > results.sarif

# CI with baseline
argus check src/ --baseline .argus-baseline.json --fail-on-new --update-baseline
```

---

## fix

Apply automatic fixes with validation and conflict resolution.

### Synopsis

```bash
argus fix [TARGETS...] [OPTIONS]
```

### Options

#### Fix Behavior

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| dry-run | `-n`, `--dry-run` | false | Show changes without applying |
| interactive | `-i`, `--interactive` | false | Prompt for each fix |
| rule | `-r`, `--rule` | - | Apply only specific rule |
| unsafe | `--unsafe` | false | Allow unsafe fixes |

#### Validation

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| no-validate | `--no-validate` | false | Skip validation |
| validate-level | `--validate-level` | syntax | none, structural, syntax, semantic |

#### Conflict Resolution

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| conflict-strategy | `--conflict-strategy` | preferred | skip, preferred, first, severity, smaller |

#### Safety

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| no-backup | `--no-backup` | false | Don't create backups |
| no-transactional | `--no-transactional` | false | Don't rollback on failure |
| no-diff | `--no-diff` | false | Don't show diffs |
| verbose-fix | `--verbose-fix` | false | Detailed progress |

### Examples

```bash
# Preview fixes
argus fix src/ --dry-run

# Interactive approval
argus fix src/ --interactive

# Semantic validation
argus fix src/ --validate-level semantic

# Apply all fixes including unsafe
argus fix src/ --unsafe
```

---

## unused

Detect unused functions, types, imports, and exports.

### Synopsis

```bash
argus unused [TARGETS...] [OPTIONS]
```

### Options

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| mode | `-m`, `--mode` | full | Analysis mode: full, plugin |
| root | `--root` | - | Root patterns (repeatable) |
| format | `-f`, `--format` | terminal | Output format |
| context | `--context` | false | Show source context |
| context-lines | `--context-lines` | 2 | Lines of context |

### Examples

```bash
# Detect unused code
argus unused src/

# With custom roots
argus unused src/ --root "^Main.main$" --root "^Test."

# JSON output
argus unused src/ --format json
```

---

## init

Generate a configuration file with sensible defaults.

### Synopsis

```bash
argus init [OPTIONS]
```

### Options

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| force | `-f`, `--force` | false | Overwrite existing config |

### Examples

```bash
argus init
argus init --force
```

---

## index

Build HIE database for semantic analysis.

### Synopsis

```bash
argus index [DIR] [OPTIONS]
```

### Options

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| no-build | `--no-build` | false | Skip project build |
| clean | `--clean` | false | Clean before build |
| db | `-d`, `--db` | .hiedb | Database path |
| no-symlink | `--no-symlink` | false | Don't create .hie symlink |
| yes | `-y`, `--yes` | false | Auto-accept prompts |

### Examples

```bash
# Build and index
argus index

# Index without building
argus index --no-build

# Custom database location
argus index --db my-hiedb
```

---

## watch

Watch files and continuously analyze on changes.

### Synopsis

```bash
argus watch [DIRS...] [OPTIONS]
```

### Options

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| debounce | `--debounce` | 500 | Debounce delay (ms) |
| poll-interval | `--poll-interval` | 1000 | Polling interval (ms) |
| no-clear | `--no-clear` | false | Don't clear screen |
| no-timestamp | `--no-timestamp` | false | Don't show timestamps |

### Examples

```bash
# Watch src directory
argus watch src/

# Fast updates
argus watch src/ --debounce 100

# Multiple directories
argus watch src/ app/ test/
```

---

## diff

Compare current analysis with baseline or git ref.

### Synopsis

```bash
argus diff [TARGETS...] [OPTIONS]
```

### Options

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| baseline | `-b`, `--baseline` | - | Baseline file |
| git-ref | `-g`, `--git-ref` | - | Git ref (HEAD~1, main) |
| new-only | `--new-only` | false | Show only new issues |
| fixed-only | `--fixed-only` | false | Show only fixed issues |
| format | `-f`, `--format` | terminal | terminal, json |

### Examples

```bash
# Compare with baseline
argus diff src/ --baseline .argus-baseline.json

# Compare with git ref
argus diff src/ --git-ref main

# Show only regressions
argus diff src/ --git-ref HEAD~1 --new-only
```

---

## baseline

Create a baseline snapshot for CI comparison.

### Synopsis

```bash
argus baseline [TARGETS...] [OPTIONS]
```

### Options

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| output | `-o`, `--output` | .argus-baseline.json | Output file |
| force | `-f`, `--force` | false | Overwrite existing |

### Examples

```bash
# Create baseline
argus baseline src/

# Custom output
argus baseline src/ --output my-baseline.json
```

---

## stats

Display analysis statistics with charts.

### Synopsis

```bash
argus stats [TARGETS...] [OPTIONS]
```

### Options

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| by-rule | `--by-rule` | false | Group by rule |
| by-file | `--by-file` | false | Group by file |
| by-severity | `--by-severity` | false | Group by severity |
| format | `-f`, `--format` | terminal | terminal, json |

### Examples

```bash
# Statistics by severity
argus stats src/ --by-severity

# Top rules
argus stats src/ --by-rule

# JSON output
argus stats src/ --format json
```

---

## daemon

Run as background service for fast repeated analysis.

### Synopsis

```bash
argus daemon <ACTION> [OPTIONS]
```

### Actions

| Action | Description |
|--------|-------------|
| start | Start the daemon |
| stop | Stop the daemon |
| status | Check daemon status |
| reload | Reload configuration |
| check [FILES...] | Analyze via daemon |

### Options

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| socket | `--socket` | (system) | Unix socket path |
| port | `--port` | - | TCP port |
| idle-timeout | `--idle-timeout` | - | Auto-shutdown seconds |
| verbose | `-v`, `--verbose` | false | Verbose logging |

### Examples

```bash
# Start daemon
argus daemon start

# Check status
argus daemon status

# Analyze via daemon
argus daemon check src/MyModule.hs

# Stop daemon
argus daemon stop
```

---

## lsp

Run as LSP server for IDE integration.

### Synopsis

```bash
argus lsp [OPTIONS]
```

### Options

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| debug-log | `--debug-log` | - | Debug log file |
| analyze-on-change | `--analyze-on-change` | false | Analyze every change |
| debounce | `--debounce` | 500 | Debounce time (ms) |
| no-progress | `--no-progress` | false | Disable progress |

### Examples

```bash
# Start LSP server
argus lsp

# With debug logging
argus lsp --debug-log /tmp/argus-lsp.log
```

---

## architecture

Analyze module dependencies and coupling metrics.

### Synopsis

```bash
argus architecture [TARGETS...] [OPTIONS]
```

### Options

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| format | `-f`, `--format` | terminal | terminal, json, dot |
| graph | `-g`, `--graph` | false | Generate DOT graph |
| graph-output | `-o`, `--graph-output` | - | DOT file output |
| no-metrics | `--no-metrics` | false | Hide coupling metrics |
| no-violations | `--no-violations` | false | Hide layer violations |
| no-cycles | `--no-cycles` | false | Hide circular deps |

### Examples

```bash
# Full architecture analysis
argus architecture src/

# Generate DOT graph
argus architecture src/ --graph --graph-output deps.dot

# JSON output
argus architecture src/ --format json
```

---

## pack

Manage rule packs.

### Synopsis

```bash
argus pack <ACTION> [OPTIONS]
```

### Actions

| Action | Arguments | Description |
|--------|-----------|-------------|
| list | - | List available packs |
| show | PACK | Show pack details |
| validate | [FILE] | Validate pack |
| export | PACK OUTPUT | Export to file |
| import | FILE | Import from file |
| create | PACK | Create new pack |

### Options

| Option | Flag | Default | Description |
|--------|------|---------|-------------|
| version | `-V`, `--version` | - | Pack version |
| author | `-a`, `--author` | - | Pack author |
| json | `--json` | false | JSON output |

### Examples

```bash
# List packs
argus pack list

# Show pack details
argus pack show security

# Export pack
argus pack export security my-security-pack.toml

# Import pack
argus pack import custom-rules.toml
```

---

## Output Formats

Available for `--format` option:

| Format | Commands | Description |
|--------|----------|-------------|
| terminal | All | Colored terminal output |
| plain | check, unused | Plain text (no colors) |
| json | All | Machine-readable JSON |
| sarif | check | GitHub Code Scanning |
| html | check | Interactive HTML report |
| junit | check | JUnit XML |
| checkstyle | check | Checkstyle XML |
| codeclimate | check | Code Climate JSON |
| dot | architecture | Graphviz DOT graph |

---

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success (no issues or below threshold) |
| 1 | Issues found or threshold exceeded |
| 2 | Invalid arguments or configuration |
| 3 | Internal error |
