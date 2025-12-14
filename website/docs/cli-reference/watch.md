---
sidebar_position: 7
title: argus watch
description: Watch files and analyze on changes
---

# argus watch

Watch Haskell source files and automatically run analysis when they change.

## Synopsis

```bash
argus watch [OPTIONS] [DIRECTORIES...]
```

## Description

The `watch` command monitors your source files for changes and automatically runs analysis. This provides immediate feedback during development without manually running `argus check`.

## Arguments

| Argument | Description |
|----------|-------------|
| `DIRECTORIES` | Directories to watch (default: configured source dirs) |

## Options

### Watch Behavior

| Option | Description |
|--------|-------------|
| `--debounce MS` | Wait MS milliseconds after change before analyzing (default: 300) |
| `--poll` | Use polling instead of filesystem events |
| `--poll-interval MS` | Polling interval in milliseconds |
| `--no-initial` | Don't run analysis on startup |

### Analysis Options

| Option | Description |
|--------|-------------|
| `--mode MODE` | Analysis mode: `quick`, `full` |
| `--rules RULES` | Rules to check |
| `--categories CATS` | Categories to check |
| `--severity LEVEL` | Minimum severity |

### File Filtering

| Option | Description |
|--------|-------------|
| `--include GLOB` | Only watch matching files |
| `--exclude GLOB` | Ignore matching files |
| `--exclude-dir DIR` | Ignore directories |

### Output

| Option | Description |
|--------|-------------|
| `--clear` | Clear screen before each analysis |
| `--notify` | Send desktop notifications |
| `--bell` | Sound bell on issues found |
| `--format FORMAT` | Output format |
| `-q, --quiet` | Minimal output |

### Commands

| Option | Description |
|--------|-------------|
| `--on-success CMD` | Run command on successful analysis |
| `--on-failure CMD` | Run command when issues found |
| `--on-change CMD` | Run command on any change |

## Examples

### Basic Watch

```bash
# Watch current directory
argus watch

# Watch specific directories
argus watch src/ app/

# Watch with clear screen
argus watch --clear src/
```

### Watch Output

```
Argus watching src/... (Ctrl+C to stop)

[14:32:15] Analyzing src/Handler.hs...
✓ No issues found (0.12s)

[14:32:45] Analyzing src/Handler.hs...
⚠ 2 warnings found (0.15s)

  src/Handler.hs:45:12: warning [partial/head]
    Use headMay instead of head
    |
  45|   let first = head items
    |               ^^^^

  src/Handler.hs:67:5: warning [redundant/id]
    Redundant use of id
    |
  67|   result = id value
    |            ^^

[14:33:02] Analyzing src/Handler.hs...
✓ No issues found (0.11s)
```

### Analysis Configuration

```bash
# Quick mode for faster feedback
argus watch --mode quick src/

# Specific rules only
argus watch --rules "partial/*,security/*" src/

# Errors only
argus watch --severity error src/
```

### Notifications

```bash
# Desktop notifications on issues
argus watch --notify src/

# Bell sound
argus watch --bell src/

# Both
argus watch --notify --bell src/
```

### Custom Commands

```bash
# Run tests on success
argus watch --on-success "stack test" src/

# Notify on failure
argus watch --on-failure "notify-send 'Argus: Issues found'" src/

# Custom script on any change
argus watch --on-change "./scripts/check.sh" src/
```

### Filtering

```bash
# Exclude test files
argus watch --exclude "*Spec.hs" --exclude "*Test.hs" src/

# Only watch specific patterns
argus watch --include "src/Api/**/*.hs" src/

# Exclude directories
argus watch --exclude-dir test --exclude-dir bench .
```

### Performance Tuning

```bash
# Longer debounce for slow systems
argus watch --debounce 500 src/

# Use polling (for network filesystems)
argus watch --poll --poll-interval 1000 src/

# Skip initial analysis
argus watch --no-initial src/
```

## Watch Events

Argus responds to these filesystem events:

| Event | Action |
|-------|--------|
| File modified | Analyze changed file |
| File created | Analyze new file |
| File deleted | Remove from results |
| File renamed | Analyze as new file |

## Integration with Editors

### VS Code

With the Argus extension, watch runs automatically. For manual setup:

```json
// .vscode/tasks.json
{
  "version": "2.0.0",
  "tasks": [{
    "label": "Argus Watch",
    "type": "shell",
    "command": "argus watch --clear src/",
    "isBackground": true,
    "problemMatcher": {
      "pattern": {
        "regexp": "^(.+):(\\d+):(\\d+): (\\w+) \\[(.+)\\]$",
        "file": 1,
        "line": 2,
        "column": 3,
        "severity": 4,
        "message": 5
      },
      "background": {
        "activeOnStart": true,
        "beginsPattern": "Analyzing",
        "endsPattern": "found"
      }
    }
  }]
}
```

### Terminal Multiplexer

```bash
# tmux: run in separate pane
tmux split-window "argus watch --clear src/"

# Or in background
argus watch src/ &
```

### Vim/Neovim

```vim
" In a terminal buffer
:term argus watch --clear src/
```

## Daemon Mode Comparison

| Feature | `argus watch` | `argus daemon` |
|---------|---------------|----------------|
| Use case | Development feedback | CI/IDE integration |
| Output | Real-time terminal | On-demand queries |
| Resource usage | Medium | Low (idle) |
| Startup time | Instant | Once, then instant |

For IDE integration, prefer the daemon. For terminal-based development, use watch.

## Configuration

```toml
[watch]
# Debounce time in milliseconds
debounce = 300

# Clear screen before each analysis
clear = true

# Enable notifications
notify = false

# Analysis mode
mode = "quick"

# Excluded patterns
exclude = [
  "*Spec.hs",
  "*Test.hs",
  "**/Generated/**",
]
```

## Resource Usage

| Setting | CPU | Memory |
|---------|-----|--------|
| `--mode quick` | Low | ~50 MB |
| `--mode full` | Medium | ~200 MB |
| `--poll` | Higher | Same |

## Troubleshooting

### High CPU Usage

```bash
# Increase debounce
argus watch --debounce 1000 src/

# Exclude unnecessary directories
argus watch --exclude-dir node_modules --exclude-dir .git src/
```

### Not Detecting Changes

```bash
# Use polling mode
argus watch --poll src/

# Check inotify limits (Linux)
cat /proc/sys/fs/inotify/max_user_watches
# Increase if needed:
echo 65536 | sudo tee /proc/sys/fs/inotify/max_user_watches
```

### Network Filesystems

```bash
# Always use polling for NFS, SSHFS, etc.
argus watch --poll --poll-interval 2000 src/
```

## Exit Codes

| Code | Condition |
|------|-----------|
| `0` | Normal exit (Ctrl+C) |
| `1` | Configuration error |
| `2` | Watch setup failed |
| `130` | Interrupted (SIGINT) |

## Signals

| Signal | Action |
|--------|--------|
| `SIGINT` (Ctrl+C) | Stop watching, exit |
| `SIGHUP` | Reload configuration |
| `SIGUSR1` | Force re-analysis of all files |

## See Also

- **[argus check](./check)**: One-time analysis
- **[argus daemon](./daemon)**: Background service
- **[IDE Integration](../usage-guide/ide-integration)**: Editor setup
- **[Configuration](../configuration/file-format)**: Watch settings
