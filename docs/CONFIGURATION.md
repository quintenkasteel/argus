# Argus Configuration Reference

Complete reference for all Argus configuration options.

## File Formats

Argus supports TOML (preferred) and YAML configuration files.

### Search Order

1. `argus.toml`
2. `linter.toml`
3. `.linter.toml`
4. `linter.yaml`
5. `.linter.yaml`
6. `config.yaml`

Or specify explicitly: `argus check --config path/to/config.toml`

---

## Configuration Sections

### [general]

Core analysis settings.

```toml
[general]
directories = ["src", "app"]           # Directories to analyze
exclude = [                            # Glob patterns to exclude
  ".stack-work/**",
  "dist-newstyle/**",
  "dist/**",
  ".cabal/**",
  "Generated/**",
  "*.gen.hs",
  "**/autogen/**",
  "**/Paths_*.hs",
  "**/PackageInfo_*.hs",
  ".git/**",
]
mode = "quick"                         # quick | full | plugin
hie-dir = ".hie"                       # HIE files directory (for full mode)
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| directories | [String] | ["src", "app"] | Directories to analyze |
| exclude | [String] | (see above) | Glob patterns to exclude |
| mode | String | "quick" | Analysis mode |
| hie-dir | String | ".hie" | HIE directory path |

---

### [output]

Output formatting options.

```toml
[output]
format = "terminal"                    # Output format
color = true                           # Use ANSI colors
group-by = "file"                      # file | rule | severity
show-context = true                    # Show source context
context-lines = 2                      # Lines of context
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| format | String | "terminal" | terminal, plain, json, sarif, html, junit, checkstyle, codeclimate |
| color | Bool | true | Enable colored output |
| group-by | String | "file" | Grouping strategy |
| show-context | Bool | true | Show source context |
| context-lines | Int | 2 | Lines of context |

---

### [unused]

Unused code detection settings.

```toml
[unused]
enabled = true
check-functions = true                 # Check unused functions
check-types = true                     # Check unused types
check-imports = true                   # Check unused imports
check-exports = true                   # Check unused exports
check-constructors = true              # Check unused constructors
check-record-fields = true             # Check unused record fields
check-local-binds = true               # Check unused local bindings
check-instances = false                # Check unused instances (many false positives)
typeclass-roots = true                 # Weeder-style typeclass roots
derive-roots = true                    # Treat derived instances as roots
min-confidence = 0.5                   # Confidence threshold (0.0-1.0)
roots = [                              # Root patterns (regex)
  "^Main\\.main$",
  "^Paths_.*"
]
th-roots = [                           # Template Haskell roots
  "parseJSON",
  "toJSON",
  "makeLenses",
  "makeClassy",
  "share",
  "widgetFile"
]
ignore-patterns = []                   # Patterns to ignore
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| enabled | Bool | true | Enable unused code detection |
| check-functions | Bool | true | Check unused functions |
| check-types | Bool | true | Check unused types |
| check-imports | Bool | true | Check unused imports |
| check-exports | Bool | true | Check unused exports |
| check-constructors | Bool | true | Check unused constructors |
| check-record-fields | Bool | true | Check unused record fields |
| check-local-binds | Bool | true | Check unused local bindings |
| check-instances | Bool | false | Check unused instances |
| typeclass-roots | Bool | true | Weeder-style roots |
| derive-roots | Bool | true | Derived instances as roots |
| min-confidence | Float | 0.5 | Confidence threshold |
| roots | [String] | (see above) | Root patterns (regex) |
| th-roots | [String] | (see above) | TH function roots |
| ignore-patterns | [String] | [] | Patterns to ignore |

---

### [naming]

Naming convention rules.

```toml
[naming]
enabled = true

[[naming.types]]
pattern = "LocationId"
replacement = "Key Location"
severity = "warning"
message = "Use Key Location instead of LocationId"
within = "*.Model.*"                   # Optional module pattern

[[naming.variables]]
type = "Key Location"                  # Type pattern
from = "*"                             # Name pattern (optional, None = any)
to = "locationK"                       # Target name
severity = "warning"
message = "Use locationK for Key Location values"
```

#### Type Rules

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| pattern | String | Yes | Pattern to match |
| replacement | String | Yes | Replacement pattern |
| severity | String | Yes | error, warning, suggestion, info |
| message | String | No | Custom message |
| within | String | No | Module pattern restriction |

#### Variable Rules

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| type | String | Yes | Type pattern |
| from | String | No | Name pattern (None = any) |
| to | String | Yes | Target name |
| severity | String | Yes | error, warning, suggestion, info |
| message | String | No | Custom message |

---

### [patterns]

Pattern-based linting rules.

```toml
[patterns]
enabled = true

[[patterns.rules]]
name = "avoid-head"
match = "head"                         # Haskell syntax pattern
fix = "headMay"                        # Optional replacement
where = "Foldable t => t a"            # Optional type constraint
severity = "warning"
message = "Use headMay instead of partial head"
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| name | String | Yes | Rule identifier |
| match | String | Yes | Pattern to match |
| fix | String | No | Replacement pattern |
| where | String | No | Type constraint |
| severity | String | Yes | error, warning, suggestion, info |
| message | String | Yes | Diagnostic message |

---

### [imports]

Import analysis and management.

```toml
[imports]
remove-unused = true                   # Remove unused imports
suggest-qualified = [                  # Suggest qualified imports
  "Data.Map",
  "Data.Set",
  "Data.Text",
  "Data.ByteString",
  "Data.HashMap.Strict",
  "Data.IntMap",
  "Data.IntSet"
]
allow-unqualified-types = true         # Allow "import Data.Text (Text)"
allow-unqualified-operators = true     # Allow unqualified operators
require-explicit = false               # Require explicit import lists
combine = true                         # Combine fragmented imports
th-roots = [                           # TH patterns
  "widgetFile",
  "makeLenses"
]
suppress-for-th = true                 # Suppress warnings for TH files
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| remove-unused | Bool | true | Remove unused imports |
| suggest-qualified | [String] | (see above) | Modules to suggest qualified |
| allow-unqualified-types | Bool | true | Allow type-only unqualified |
| allow-unqualified-operators | Bool | true | Allow operator-only unqualified |
| require-explicit | Bool | false | Require explicit import lists |
| combine | Bool | true | Combine fragmented imports |
| th-roots | [String] | (see above) | TH patterns |
| suppress-for-th | Bool | true | Suppress for TH files |

---

### [fix]

Auto-fix settings.

```toml
[fix]
enabled = true
safe-only = true                       # Only apply safe fixes
preview = true                         # Show preview (deprecated, use CLI --dry-run)
backup = true                          # Create backup files

[fix.auto-imports]
enabled = true
add-missing = true                     # Add missing imports
remove-unused = true                   # Remove unused imports
organize = false                       # Organize imports
use-explicit = true                    # Use explicit import lists
qualify-new = false                    # Qualify new imports
group-by-category = false              # Group by category
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| enabled | Bool | true | Enable auto-fix |
| safe-only | Bool | true | Only safe fixes |
| preview | Bool | true | Show preview (deprecated) |
| backup | Bool | true | Create backups |

#### [fix.auto-imports]

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| enabled | Bool | true | Enable import management |
| add-missing | Bool | true | Add missing imports |
| remove-unused | Bool | true | Remove unused imports |
| organize | Bool | false | Organize imports |
| use-explicit | Bool | true | Use explicit lists |
| qualify-new | Bool | false | Qualify new imports |
| group-by-category | Bool | false | Group by category |

---

### [complexity]

Complexity thresholds.

```toml
[complexity]
enabled = true
cyclomatic-warning = 10                # Cyclomatic complexity warning
cyclomatic-error = 20                  # Cyclomatic complexity error
cognitive-warning = 15                 # Cognitive complexity warning
cognitive-error = 25                   # Cognitive complexity error
line-length-warning = 50               # Function line count warning
nesting-warning = 4                    # Nesting depth warning
parameter-warning = 5                  # Parameter count warning
pattern-branch-warning = 10            # Pattern branch count warning
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| enabled | Bool | true | Enable complexity checks |
| cyclomatic-warning | Int | 10 | Cyclomatic warning threshold |
| cyclomatic-error | Int | 20 | Cyclomatic error threshold |
| cognitive-warning | Int | 15 | Cognitive warning threshold |
| cognitive-error | Int | 25 | Cognitive error threshold |
| line-length-warning | Int | 50 | Function lines warning |
| nesting-warning | Int | 4 | Nesting depth warning |
| parameter-warning | Int | 5 | Parameter count warning |
| pattern-branch-warning | Int | 10 | Pattern branches warning |

---

### [resource]

Resource limits and timeouts.

```toml
[resource]
timeout-seconds = 60                   # Per-file timeout
max-memory-mb = 2048                   # Maximum memory
force-gc-interval = 100                # GC every N files
track-per-file = true                  # Track per-file usage
warn-slow-files = 10.0                 # Warn if > N seconds
max-retries = 1                        # Retries on failure
kill-on-timeout = true                 # Kill thread on timeout
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| timeout-seconds | Int | 60 | Per-file timeout |
| max-memory-mb | Int | 2048 | Max memory MB |
| force-gc-interval | Int | 100 | GC interval (files) |
| track-per-file | Bool | true | Track per-file usage |
| warn-slow-files | Float | 10.0 | Slow file threshold (s) |
| max-retries | Int | 1 | Retry count |
| kill-on-timeout | Bool | true | Kill on timeout |

---

### [qualify-import]

Import qualification strategy.

```toml
[qualify-import]
enabled = true
strategy = "last-part"                 # Alias strategy
custom-aliases = [                     # Custom module aliases
  ["Data.Text", "T"],
  ["Data.ByteString", "BS"],
  ["Data.Map.Strict", "Map"],
  ["Data.HashMap.Strict", "HM"]
]
max-alias-length = 5                   # Maximum alias length
prefer-uppercase = false               # Prefer uppercase aliases
allow-conflicts = true                 # Allow conflicts (suffix with numbers)
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| enabled | Bool | true | Enable qualification |
| strategy | String | "last-part" | last-part, first-letter, initials, first-n-chars |
| custom-aliases | [[String, String]] | (see above) | Module → alias mappings |
| max-alias-length | Int | - | Max alias length |
| prefer-uppercase | Bool | false | Uppercase aliases |
| allow-conflicts | Bool | true | Allow conflicts |

#### Strategy Options

| Strategy | Example | Result |
|----------|---------|--------|
| last-part | Data.Text | Text |
| first-letter | Data.Text | T |
| initials | Data.Text | DT |
| first-n-chars | Data.Text | Tex (n=3) |

---

### [architecture]

Architecture analysis settings.

```toml
[architecture]
enabled = true
max-cycle-length = 10                  # Max cycle to detect
instability-threshold = 0.8            # Instability warning
coupling-threshold = 15                # Coupling warning
check-orphans = true                   # Check orphan instances
check-qualified = true                 # Check qualification

[[architecture.layers]]
name = "Core"
patterns = ["*.Types", "*.Core"]
can-import = ["Core"]

[[architecture.layers]]
name = "Data"
patterns = ["*.Data.*", "*.Model.*"]
can-import = ["Core", "Data"]

[[architecture.layers]]
name = "Service"
patterns = ["*.Service.*", "*.Logic.*"]
can-import = ["Core", "Data", "Service"]

[[architecture.layers]]
name = "API"
patterns = ["*.API.*", "*.Handler.*"]
can-import = ["Core", "Data", "Service", "API"]

[[architecture.layers]]
name = "App"
patterns = ["Main", "*.App", "*.CLI"]
can-import = ["Core", "Data", "Service", "API", "App"]
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| enabled | Bool | true | Enable architecture checks |
| max-cycle-length | Int | 10 | Max cycle length |
| instability-threshold | Float | 0.8 | Instability warning |
| coupling-threshold | Int | 15 | Coupling warning |
| check-orphans | Bool | true | Check orphan instances |
| check-qualified | Bool | true | Check qualification |

#### Layer Configuration

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| name | String | Yes | Layer name |
| patterns | [String] | Yes | Module patterns |
| can-import | [String] | Yes | Allowed layer imports |

---

## Environment Variables

Override configuration via environment variables:

| Variable | Purpose |
|----------|---------|
| `ARGUS_CONFIG` | Config file path |
| `ARGUS_MODE` | Analysis mode (quick/full/plugin) |
| `ARGUS_HIE_DIR` | HIE directory path |
| `ARGUS_EXCLUDE` | Exclude patterns (comma-separated) |
| `ARGUS_OUTPUT_FORMAT` | Output format |
| `ARGUS_COLOR` | Enable colors (true/false) |
| `ARGUS_CONTEXT_LINES` | Lines of context |
| `ARGUS_TIMEOUT` | Per-file timeout (seconds) |
| `ARGUS_MEMORY_LIMIT` | Max memory (MB) |

Environment variables take precedence over config file values.

---

## Example Complete Configuration

```toml
[general]
directories = ["src", "app", "lib"]
exclude = [".stack-work/**", "dist-newstyle/**"]
mode = "quick"
hie-dir = ".hie"

[output]
format = "terminal"
color = true
group-by = "file"
show-context = true
context-lines = 2

[unused]
enabled = true
check-functions = true
check-types = true
check-imports = true
roots = ["^Main\\.main$"]
th-roots = ["parseJSON", "toJSON", "makeLenses"]

[naming]
enabled = true

[[naming.types]]
pattern = "LocationId"
replacement = "Key Location"
severity = "warning"

[imports]
remove-unused = true
suggest-qualified = ["Data.Map", "Data.Set", "Data.Text"]
combine = true

[fix]
enabled = true
safe-only = true
backup = true

[complexity]
enabled = true
cyclomatic-warning = 10
cyclomatic-error = 20

[resource]
timeout-seconds = 60
max-memory-mb = 2048

[architecture]
enabled = true
max-cycle-length = 10

[[architecture.layers]]
name = "Core"
patterns = ["*.Types", "*.Core"]
can-import = ["Core"]

[[architecture.layers]]
name = "App"
patterns = ["Main", "*.App"]
can-import = ["Core", "App"]
```
