# Argus

[![CI](https://github.com/quintenkasteel/argus/actions/workflows/ci.yml/badge.svg)](https://github.com/quintenkasteel/argus/actions/workflows/ci.yml)
[![Coverage](https://github.com/quintenkasteel/argus/actions/workflows/coverage.yml/badge.svg)](https://github.com/quintenkasteel/argus/actions/workflows/coverage.yml)
[![Hackage](https://img.shields.io/hackage/v/argus.svg)](https://hackage.haskell.org/package/argus)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Docs](https://img.shields.io/badge/docs-quintenkasteel.github.io%2Fargus-blue)](https://quintenkasteel.github.io/argus/)

**Haskell Static Analyzer**

Argus is a static analysis tool for Haskell combining syntactic pattern matching, semantic HIE analysis, and GHC plugin integration. It detects bugs, security issues, performance problems, and architectural violations.

## Features

- **Multi-Mode Analysis**: Quick (syntax-only), full (HIE-based semantic), or GHC plugin
- **400+ Built-in Rules**: 39 categories covering safety, performance, security, style, and more
- **Unused Code Detection**: Dependency graph analysis with Template Haskell awareness
- **Security Analysis**: Injection vulnerabilities, unsafe functions, hardcoded secrets
- **Performance Detection**: Anti-patterns, space leaks, fusion blockers
- **Complexity Metrics**: Cyclomatic, cognitive complexity, nesting depth
- **Architecture Analysis**: Circular dependencies, layer violations, coupling metrics
- **Auto-Fix Engine**: Conflict detection, validation, backup, and rollback
- **9 Output Formats**: Terminal, JSON, SARIF, HTML, JUnit, Checkstyle, CodeClimate, Plain, DOT
- **13 CLI Commands**: check, fix, unused, init, index, watch, diff, baseline, stats, daemon, lsp, architecture, pack

## Table of Contents

- [Installation](#installation)
- [Quick Start](#quick-start)
- [Commands](#commands)
- [Analysis Modes](#analysis-modes)
- [Rule Categories](#rule-categories)
- [Auto-Fix](#auto-fix)
- [Output Formats](#output-formats)
- [Configuration](#configuration)
- [IDE Integration](#ide-integration)

## Installation

### From Hackage

```bash
cabal install argus
# or
stack install argus
```

### From Source

```bash
git clone https://github.com/quintenkasteel/argus.git
cd argus
stack build
stack install
```

## Quick Start

```bash
# Initialize configuration
argus init

# Run analysis
argus check src/

# Preview fixes without applying
argus fix src/ --dry-run

# Detect unused code
argus unused src/

# Generate SARIF for GitHub Code Scanning
argus check src/ --format sarif > results.sarif
```

## Commands

Argus provides 13 commands. Global options (`-c/--config`, `-v/--verbose`, `--no-color`, `-j/--parallel`) apply to all.

### `argus check`

Run static analysis on Haskell source files.

```bash
argus check [FILES/DIRS...] [OPTIONS]

Analysis Options:
  -m, --mode MODE           quick|full|plugin (default: quick)
  --hie-dir DIR             HIE files directory (default: .hie)

Output Options:
  -f, --format FORMAT       terminal|json|sarif|html|junit|checkstyle|codeclimate|plain
  --group-by GROUP          file|rule|severity (default: file)
  --context                 Show source context
  --context-lines N         Lines of context (default: 2)

CI Options:
  -b, --baseline FILE       Baseline file for comparison
  --fail-on-new             Exit 1 if new issues found vs baseline
  --fail-on-severity LEVEL  Exit 1 if issues at severity: error|warning|suggestion|info
  --fail-on-count N         Exit 1 if total issues exceed N
  --fail-on-delta N         Exit 1 if net new issues exceed N
  --update-baseline         Update baseline file after analysis
  --ci-quiet                Minimal output for CI pipelines
```

### `argus fix`

Apply automatic fixes with validation and conflict resolution.

```bash
argus fix [FILES/DIRS...] [OPTIONS]

Fix Options:
  -n, --dry-run             Show changes without applying
  -i, --interactive         Prompt for each fix
  -r, --rule RULE           Apply only specific rule
  --unsafe                  Allow unsafe fixes (default: safe-only)

Validation Options:
  --no-validate             Skip syntax validation after fixes
  --validate-level LEVEL    none|structural|syntax|semantic (default: syntax)

Conflict Options:
  --conflict-strategy STR   skip|preferred|first|severity|smaller (default: preferred)

Safety Options:
  --no-backup               Don't create backup files
  --no-transactional        Don't rollback on validation failure
  --no-diff                 Don't show colored diffs
  --verbose-fix             Show detailed fix progress
```

### `argus unused`

Detect unused functions, types, imports, and exports.

```bash
argus unused [FILES/DIRS...] [OPTIONS]

  -m, --mode MODE           full|plugin (default: full)
  --root PATTERN            Additional root patterns (repeatable)
  -f, --format FORMAT       Output format
  --context                 Show source context
  --context-lines N         Lines of context (default: 2)
```

### `argus init`

Generate a configuration file with sensible defaults.

```bash
argus init [--force]
```

### `argus index`

Build HIE database for semantic analysis.

```bash
argus index [DIR] [OPTIONS]

  --no-build                Skip building the project
  --clean                   Clean before building
  -d, --db PATH             Custom database path (default: .hiedb)
  --no-symlink              Don't create .hie symlink
  -y, --yes                 Auto-accept prompts
```

### `argus watch`

Watch files and continuously analyze on changes.

```bash
argus watch [DIRS...] [OPTIONS]

  --debounce MS             Debounce delay (default: 500)
  --poll-interval MS        Polling interval (default: 1000)
  --no-clear                Don't clear screen on updates
  --no-timestamp            Don't show timestamps
```

### `argus diff`

Compare current analysis with baseline or git ref.

```bash
argus diff [FILES/DIRS...] [OPTIONS]

  -b, --baseline FILE       Baseline file to compare against
  -g, --git-ref REF         Git ref to compare (e.g., HEAD~1, main)
  --new-only                Show only new issues
  --fixed-only              Show only fixed issues
  -f, --format FORMAT       terminal|json
```

### `argus baseline`

Create a baseline snapshot for CI comparison.

```bash
argus baseline [FILES/DIRS...] [OPTIONS]

  -o, --output FILE         Output file (default: .argus-baseline.json)
  -f, --force               Overwrite existing baseline
```

### `argus stats`

Display analysis statistics with charts.

```bash
argus stats [FILES/DIRS...] [OPTIONS]

  --by-rule                 Group by rule
  --by-file                 Group by file
  --by-severity             Group by severity
  -f, --format FORMAT       terminal|json
```

### `argus daemon`

Run as background service for fast repeated analysis.

```bash
argus daemon <ACTION> [OPTIONS]

Actions:
  start                     Start the daemon
  stop                      Stop the daemon
  status                    Check daemon status
  reload                    Reload configuration
  check [FILES...]          Analyze files via daemon

Options:
  --socket PATH             Unix socket path
  --port PORT               TCP port (alternative to socket)
  --idle-timeout SECONDS    Shutdown after N seconds idle
```

### `argus lsp`

Run as LSP server for IDE integration.

```bash
argus lsp [OPTIONS]

  --debug-log FILE          Write debug log to file
  --analyze-on-change       Analyze on every change (expensive)
  --debounce MS             Debounce time (default: 500)
  --no-progress             Disable progress reporting
```

### `argus architecture`

Analyze module dependencies and coupling metrics.

```bash
argus architecture [FILES/DIRS...] [OPTIONS]

  -f, --format FORMAT       terminal|json|dot
  -g, --graph               Generate DOT graph output
  -o, --graph-output FILE   Write DOT graph to file
  --no-metrics              Don't show coupling metrics
  --no-violations           Don't show layer violations
  --no-cycles               Don't show circular dependencies
```

### `argus pack`

Manage rule packs.

```bash
argus pack <ACTION> [OPTIONS]

Actions:
  list                      List available packs
  show PACK                 Show pack details
  validate [FILE]           Validate a pack
  export PACK OUTPUT        Export pack to file
  import FILE               Import pack from file
  create PACK               Create new custom pack

Options:
  -V, --version VERSION     Pack version
  -a, --author AUTHOR       Pack author
  --json                    Output as JSON
```

## Analysis Modes

### Quick Mode (Default)

Fast syntax-only analysis using GHC's parser. No compilation required.

```bash
argus check --mode quick src/
```

Detects: naming issues, anti-patterns, import problems, partial functions, space leak patterns.

### Full Mode

Semantic analysis using HIE files. Requires compilation with `-fwrite-ide-info`.

```bash
# Compile with HIE output
stack build --ghc-options="-fwrite-ide-info -hiedir=.hie"
# or
cabal build --ghc-options="-fwrite-ide-info -hiedir=.hie"

# Analyze
argus check --mode full src/
```

Adds: type-aware analysis, Template Haskell resolution, cross-module dependencies, precise wildcard import checking.

### Plugin Mode

Compile-time analysis via GHC plugin.

```cabal
ghc-options: -fplugin=Argus.Plugin
```

## Rule Categories

Argus includes 400+ rules across 39 categories:

| Category | Description |
|----------|-------------|
| Safety | Partial functions (head, tail, fromJust, !!, etc.) |
| Performance | Inefficient algorithms, O(n) where O(1) possible |
| Security | Injection, unsafe functions, hardcoded secrets |
| SpaceLeaks | foldl, lazy state, thunk accumulation |
| Correctness | Logic errors, typeclass law violations |
| Style | Code style and readability |
| Modernization | Deprecated patterns, newer APIs |
| Imports | Unused imports, qualification suggestions |
| Complexity | Cyclomatic/cognitive complexity thresholds |
| Architecture | Layer violations, circular dependencies |

Additional categories: Boolean, List, Maybe, Either, Monadic, Lambda, Foldable, Containers, Transformers, Testing, FFI, TypeFamilies, GADTs, Deriving, Lenses, Async, Concurrency, ErrorHandling, Documentation, Redundant, and more.

### Partial Functions

Detects 50+ partial functions with safe alternatives:

| Partial | Safe Alternative |
|---------|------------------|
| `head` | `headMay` (Safe) |
| `tail` | `tailMay` (Safe) |
| `fromJust` | `fromMaybe` / pattern match |
| `read` | `readMaybe` (Text.Read) |
| `!!` | `atMay` / `!?` |
| `maximum` | `maximumMay` |
| `M.!` | `M.lookup` / `M.!?` |

Suppress with `-- PARTIAL: reason` comment.

## Auto-Fix

Argus provides automatic code fixing with safety guarantees.

### Validation Levels

| Level | Description | Default |
|-------|-------------|---------|
| `none` | No validation | No |
| `structural` | Bracket balance | No |
| `syntax` | GHC parser validation | **Yes** |
| `semantic` | Parse + type check | No |

Default validation ensures code parses after fixes. For type safety, use `--validate-level semantic`.

### Conflict Resolution

When fixes overlap, Argus detects conflicts and applies resolution strategies:

| Strategy | Description |
|----------|-------------|
| `skip` | Skip all conflicting fixes |
| `preferred` | Prefer safe fixes (default) |
| `first` | Apply first, skip later |
| `severity` | Prefer higher severity |
| `smaller` | Prefer smaller changes |

### Transaction Behavior

With `--no-transactional` omitted (default):
- All fixes applied to in-memory copy first
- Validation run after each fix
- On validation failure, all changes discarded
- Backup files created with `.argus-backup` suffix

File writes are sequential, not OS-atomic. For critical code, use `--dry-run` first.

### Example Usage

```bash
# Preview all fixes
argus fix src/ --dry-run

# Interactive mode with per-fix approval
argus fix src/ --interactive

# Apply safe fixes only
argus fix src/

# Apply all fixes including unsafe
argus fix src/ --unsafe

# With type-level validation
argus fix src/ --validate-level semantic
```

## Output Formats

| Format | Flag | Use Case |
|--------|------|----------|
| `terminal` | Default | Human-readable with colors |
| `plain` | `--format plain` | No ANSI colors, for logs |
| `json` | `--format json` | Machine-readable |
| `sarif` | `--format sarif` | GitHub Code Scanning |
| `html` | `--format html` | Interactive HTML report |
| `junit` | `--format junit` | Jenkins, GitLab CI, GitHub Actions |
| `checkstyle` | `--format checkstyle` | Checkstyle-compatible tools |
| `codeclimate` | `--format codeclimate` | GitLab MR integration |
| `dot` | `--format dot` | Graphviz (architecture only) |

### GitHub Actions Integration

```yaml
- name: Run Argus
  run: argus check src/ --format sarif > results.sarif

- name: Upload SARIF
  uses: github/codeql-action/upload-sarif@v3
  with:
    sarif_file: results.sarif
```

### GitLab CI Integration

```yaml
argus:
  script:
    - argus check src/ --format codeclimate > gl-code-quality-report.json
  artifacts:
    reports:
      codequality: gl-code-quality-report.json
```

## Configuration

Create `argus.toml` in your project root. Supported formats: TOML (`.toml`) and YAML (`.yaml`, `.yml`).

Search order: `argus.toml`, `linter.toml`, `.linter.toml`, `linter.yaml`, `.linter.yaml`, `config.yaml`.

### Configuration Sections

```toml
[general]
directories = ["src", "app"]
exclude = [".stack-work/**", "dist-newstyle/**", "Generated/**"]
mode = "quick"      # quick|full|plugin
hie-dir = ".hie"

[output]
format = "terminal"
color = true
group-by = "file"   # file|rule|severity
show-context = true
context-lines = 2

[unused]
enabled = true
check-functions = true
check-types = true
check-imports = true
check-exports = true
check-constructors = true
check-record-fields = true
check-local-binds = true
min-confidence = 0.5
roots = ["^Main\\.main$", "^Paths_.*"]
th-roots = ["parseJSON", "toJSON", "makeLenses"]

[naming]
enabled = true

[[naming.types]]
pattern = "LocationId"
replacement = "Key Location"
severity = "warning"

[[naming.variables]]
type = "Key Location"
to = "locationK"

[patterns]
enabled = true

[[patterns.rules]]
name = "avoid-head"
match = "head"
fix = "headMay"
severity = "warning"
message = "Use headMay instead of partial head"

[imports]
remove-unused = true
suggest-qualified = ["Data.Map", "Data.Set", "Data.Text", "Data.ByteString"]
require-explicit = false
allow-unqualified-types = true
allow-unqualified-operators = true
combine = true

[fix]
enabled = true
safe-only = true
backup = true

[fix.auto-imports]
enabled = true
add-missing = true
remove-unused = true

[complexity]
enabled = true
cyclomatic-warning = 10
cyclomatic-error = 20
cognitive-warning = 15
cognitive-error = 25
line-length-warning = 50
nesting-warning = 4
parameter-warning = 5
pattern-branch-warning = 10

[resource]
timeout-seconds = 60
max-memory-mb = 2048
force-gc-interval = 100
warn-slow-files = 10.0

[qualify-import]
enabled = true
strategy = "last-part"  # last-part|first-letter|initials|first-n-chars
custom-aliases = [["Data.Text", "T"], ["Data.ByteString", "BS"]]

[architecture]
enabled = true
max-cycle-length = 10
instability-threshold = 0.8
coupling-threshold = 15
check-orphans = true

[[architecture.layers]]
name = "Core"
patterns = ["*.Types", "*.Core"]
can-import = ["Core"]

[[architecture.layers]]
name = "Service"
patterns = ["*.Service.*"]
can-import = ["Core", "Service"]

[[architecture.layers]]
name = "API"
patterns = ["*.API.*", "*.Handler.*"]
can-import = ["Core", "Service", "API"]
```

### Environment Variables

Override configuration via environment:

| Variable | Purpose |
|----------|---------|
| `ARGUS_CONFIG` | Config file path |
| `ARGUS_MODE` | Analysis mode |
| `ARGUS_HIE_DIR` | HIE directory |
| `ARGUS_EXCLUDE` | Exclude patterns (comma-separated) |
| `ARGUS_OUTPUT_FORMAT` | Output format |
| `ARGUS_COLOR` | Enable colors (true/false) |
| `ARGUS_CONTEXT_LINES` | Context lines |
| `ARGUS_TIMEOUT` | Per-file timeout (seconds) |
| `ARGUS_MEMORY_LIMIT` | Max memory (MB) |

## IDE Integration

### LSP Server

```bash
argus lsp
```

Provides: diagnostics, hover information, code actions for fixes.

### VS Code

Use the generic LSP client or configure via Haskell extension settings:

```json
{
  "haskell.serverExecutablePath": "argus",
  "haskell.serverExtraArgs": ["lsp"]
}
```

### Neovim

```lua
require('lspconfig').argus = {
  default_config = {
    cmd = { "argus", "lsp" },
    filetypes = { "haskell", "lhaskell" },
    root_dir = require('lspconfig.util').root_pattern("*.cabal", "stack.yaml", "cabal.project"),
  }
}
require('lspconfig').argus.setup({})
```

## GHC Plugin

For compile-time analysis:

### Cabal

```cabal
build-depends: argus
ghc-options: -fplugin=Argus.Plugin
```

### Stack

```yaml
# stack.yaml
extra-deps:
  - argus-1.0.0

# package.yaml or .cabal
ghc-options: -fplugin=Argus.Plugin
```

## License

MIT License - see [LICENSE](LICENSE) for details.
