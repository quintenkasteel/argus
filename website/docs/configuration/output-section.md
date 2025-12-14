---
sidebar_position: 5
title: Output Configuration
description: Configure output format, colors, and display options
---

# Output Configuration

The `[output]` section configures how Argus displays results, including format, colors, verbosity, and grouping options.

## Basic Configuration

```toml
[output]
# Default output format
format = "terminal"

# Color mode: auto, always, never
color = "auto"

# Verbosity: quiet, normal, verbose, debug
verbosity = "normal"
```

## Format Selection

```toml
[output]
# Available formats:
# terminal, json, sarif, html, junit,
# codeclimate, checkstyle, github
format = "terminal"
```

### Terminal Format Options

```toml
[output.terminal]
# Context lines around issues
context-lines = 2

# Show line numbers
line-numbers = true

# Group issues: file, rule, severity, none
group-by = "file"

# Compact mode (one line per issue)
compact = false

# Show fix suggestions inline
show-fixes = true

# Show rule documentation links
show-docs = false

# Maximum width (0 = auto)
width = 0

# Show issue count summary
summary = true
```

### JSON Format Options

```toml
[output.json]
# Pretty print
pretty = true

# Include source code context
include-source = false

# Include fix information
include-fixes = true

# Schema version
schema-version = "1.0"
```

### SARIF Format Options

```toml
[output.sarif]
# Include rule documentation
include-help = true

# Include fix suggestions
include-fixes = true

# Inline source snippets
inline-source = false
```

### HTML Format Options

```toml
[output.html]
# Report title
title = "Argus Analysis Report"

# Include full source files
full-source = false

# Embed CSS (single file output)
embed = true

# Theme: light, dark, auto
theme = "auto"

# Include navigation
navigation = true

# Include summary charts
charts = true
```

### JUnit Format Options

```toml
[output.junit]
# Test suite name
suite-name = "Argus"

# Include stack traces
stack-traces = false

# Failure type mapping
[output.junit.types]
error = "Error"
warning = "Warning"
suggestion = "Suggestion"
```

## Color Configuration

```toml
[output]
# Color mode
# auto: Use colors if terminal supports it
# always: Always use colors
# never: Never use colors
color = "auto"

# Custom color scheme
[output.colors]
error = "red"
warning = "yellow"
suggestion = "blue"
info = "cyan"
file = "green"
line-number = "dim"
rule-id = "magenta"
```

### Available Colors

| Color | Description |
|-------|-------------|
| `black` | Black text |
| `red` | Red text |
| `green` | Green text |
| `yellow` | Yellow text |
| `blue` | Blue text |
| `magenta` | Magenta text |
| `cyan` | Cyan text |
| `white` | White text |
| `dim` | Dimmed text |
| `bold` | Bold text |
| `underline` | Underlined text |

### Combined Styles

```toml
[output.colors]
error = "bold red"
warning = "bold yellow"
file = "bold green"
```

## Verbosity Levels

```toml
[output]
# quiet: Only errors, minimal output
# normal: Standard output (default)
# verbose: Include additional context
# debug: Full debugging information
verbosity = "normal"
```

### Quiet Mode

```toml
[output]
verbosity = "quiet"
```

Output:
```
src/Handler.hs:45:12: error [partial/head]
src/Api.hs:23:5: error [security/sql-injection]
2 errors
```

### Verbose Mode

```toml
[output]
verbosity = "verbose"
```

Output includes:
- Detailed rule explanations
- Related documentation links
- Analysis timing information
- Cache hit/miss statistics

### Debug Mode

```toml
[output]
verbosity = "debug"
```

Output includes:
- Internal processing steps
- AST matching details
- Performance metrics
- Memory usage

## Grouping and Sorting

```toml
[output.terminal]
# Group by: file, rule, severity, category, none
group-by = "file"

# Sort by: file, line, severity, rule
sort-by = "severity"

# Sort order: asc, desc
sort-order = "desc"
```

### Group by File (default)

```
src/Handler.hs
  45:12 warning [partial/head] Use headMay instead of head
  67:5  error   [security/...]  Potential SQL injection

src/Api.hs
  23:5 warning [partial/tail] Use tailMay instead of tail
```

### Group by Severity

```toml
[output.terminal]
group-by = "severity"
```

```
Errors (2)
  src/Handler.hs:67:5  [security/...]  Potential SQL injection
  src/Api.hs:89:12     [security/...]  Hardcoded secret

Warnings (3)
  src/Handler.hs:45:12 [partial/head]  Use headMay
  src/Api.hs:23:5      [partial/tail]  Use tailMay
  src/Utils.hs:12:1    [redundant/id]  Redundant id
```

### Group by Rule

```toml
[output.terminal]
group-by = "rule"
```

```
partial/head (2 occurrences)
  src/Handler.hs:45:12
  src/Utils.hs:89:3

security/sql-injection (1 occurrence)
  src/Api.hs:23:5
```

## Statistics and Timing

```toml
[output]
# Show analysis statistics
stats = true

# Show timing information
timing = true

# Show progress during analysis
progress = true
```

### Statistics Output

```
Argus Analysis Statistics
=========================
Files analyzed:    156
Lines of code:     12,450
Analysis time:     2.3s

Issues by Severity:
  Errors:       3
  Warnings:    12
  Suggestions:  8
  Info:         2

Issues by Category:
  partial:      5
  security:     3
  performance:  7
  redundant:   10
```

### Timing Output

```
Timing Breakdown
================
Parse:          0.8s (35%)
Analyze:        1.2s (52%)
Format output:  0.3s (13%)
Total:          2.3s

Slowest files:
  Handler.hs:   0.4s
  Parser.hs:    0.3s
  Types.hs:     0.2s
```

## Progress Reporting

```toml
[output]
# Show progress: auto, always, never
progress = "auto"

# Progress style: bar, dots, spinner
progress-style = "bar"
```

Progress output:
```
Analyzing [=========>          ] 45% (70/156 files)
```

## File Output

```toml
[output]
# Default output file (format-specific)
# file = "report.txt"

# Output directory for multi-file reports
# output-dir = "reports/"
```

## Filtering Output

```toml
[output]
# Only show issues in these files
# include-files = ["src/Api/**"]

# Hide issues in these files
# exclude-files = ["**/Generated/**"]

# Hide specific rules from output
# hide-rules = ["info/*"]
```

## Complete Example

```toml
[output]
# Basic settings
format = "terminal"
color = "auto"
verbosity = "normal"

# Show stats and timing
stats = true
timing = true
progress = "auto"

[output.terminal]
# Display settings
context-lines = 2
line-numbers = true
group-by = "file"
sort-by = "severity"
compact = false

# Content
show-fixes = true
show-docs = false
summary = true

# Width
width = 0

[output.colors]
error = "bold red"
warning = "bold yellow"
suggestion = "blue"
info = "cyan"
file = "bold green"
line-number = "dim"
rule-id = "magenta"

[output.json]
pretty = true
include-source = true
include-fixes = true

[output.html]
title = "Argus Report"
theme = "auto"
embed = true
charts = true
```

## Environment Variables

| Variable | Overrides |
|----------|-----------|
| `ARGUS_COLOR` | `output.color` |
| `ARGUS_FORMAT` | `output.format` |
| `ARGUS_VERBOSE` | `output.verbosity` |
| `NO_COLOR` | Forces `color = never` |
| `TERM` | Used for auto color detection |

## Command-Line Overrides

```bash
# Output format
argus check -f json src/

# Color control
argus check --color always src/
argus check --no-color src/

# Verbosity
argus check -v src/      # verbose
argus check -q src/      # quiet
argus check --debug src/ # debug

# Output file
argus check -o report.json src/
```

## See Also

- **[Output Formats](../cli-reference/output-formats)**: Format reference
- **[CLI Reference](../cli-reference/check)**: Command-line options
- **[CI Integration](../usage-guide/ci-integration)**: CI-specific output
