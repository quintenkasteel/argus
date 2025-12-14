---
sidebar_position: 8
title: argus daemon
description: Run Argus as a background service
---

# argus daemon

Run Argus as a background daemon service for fast, on-demand analysis.

## Synopsis

```bash
argus daemon <COMMAND> [OPTIONS]
```

## Description

The `daemon` command manages a background Argus service that maintains loaded HIE data and caches for instant analysis. This is ideal for IDE integration and frequent checks.

## Commands

| Command | Description |
|---------|-------------|
| `start` | Start the daemon |
| `stop` | Stop the daemon |
| `restart` | Restart the daemon |
| `status` | Show daemon status |
| `check` | Run analysis via daemon |

## Options

### Start Options

| Option | Description |
|--------|-------------|
| `--port PORT` | Listen port (default: 4567) |
| `--socket FILE` | Unix socket path |
| `--foreground` | Run in foreground (don't daemonize) |
| `--log FILE` | Log file path |
| `--log-level LEVEL` | Log level: debug, info, warn, error |
| `--pid-file FILE` | PID file path |

### Resource Options

| Option | Description |
|--------|-------------|
| `--max-memory MB` | Maximum memory usage |
| `--workers N` | Number of worker threads |
| `--idle-timeout SEC` | Shutdown after idle period |
| `--cache-size MB` | Maximum cache size |

### Analysis Options

| Option | Description |
|--------|-------------|
| `--hie-dir DIR` | HIE files directory |
| `--auto-reload` | Reload HIE files on change |
| `--project DIR` | Project root directory |

## Examples

### Starting the Daemon

```bash
# Start with defaults
argus daemon start

# Start on specific port
argus daemon start --port 5000

# Start with Unix socket
argus daemon start --socket /tmp/argus.sock

# Run in foreground (for debugging)
argus daemon start --foreground

# With custom resource limits
argus daemon start --max-memory 512 --workers 4
```

### Managing the Daemon

```bash
# Check status
argus daemon status

# Stop the daemon
argus daemon stop

# Restart
argus daemon restart
```

### Using the Daemon

```bash
# Run check via daemon
argus check --daemon src/

# Or directly
argus daemon check src/Handler.hs

# Query daemon
argus daemon check --query "type-of:processItem"
```

## Status Output

```bash
argus daemon status
```

```
Argus Daemon Status
===================

Status:       Running
PID:          12345
Uptime:       2h 34m 12s
Port:         4567 (TCP)
Socket:       /tmp/argus-12345.sock

Resources:
  Memory:     156 MB / 512 MB
  Workers:    4 / 4 active
  Cache:      89 MB / 256 MB

Index:
  HIE Files:  234 loaded
  Modules:    234 indexed
  Last Update: 5 minutes ago

Statistics:
  Requests:   1,234
  Avg Time:   45ms
  Cache Hits: 89%

Clients:
  Connected:  2
  - VS Code (pid 67890)
  - Neovim (pid 11223)
```

## Daemon Protocol

The daemon accepts requests via TCP or Unix socket:

### JSON-RPC Interface

```json
// Request
{
  "jsonrpc": "2.0",
  "method": "check",
  "params": {
    "file": "/path/to/File.hs",
    "rules": ["partial/*", "security/*"]
  },
  "id": 1
}

// Response
{
  "jsonrpc": "2.0",
  "result": {
    "diagnostics": [
      {
        "file": "/path/to/File.hs",
        "line": 45,
        "column": 12,
        "severity": "warning",
        "rule": "partial/head",
        "message": "Use headMay instead of head"
      }
    ]
  },
  "id": 1
}
```

### Available Methods

| Method | Description |
|--------|-------------|
| `check` | Analyze file(s) |
| `fix` | Get fix suggestions |
| `unused` | Check for unused code |
| `query` | Query type/reference info |
| `reload` | Reload HIE files |
| `status` | Get daemon status |
| `shutdown` | Stop the daemon |

## Configuration

```toml
[daemon]
# Auto-start daemon
auto-start = false

# Connection settings
port = 4567
socket = "/tmp/argus.sock"

# Resource limits
max-memory = 512    # MB
workers = 4
cache-size = 256    # MB

# Behavior
idle-timeout = 3600  # Shutdown after 1 hour idle
auto-reload = true   # Reload HIE on changes

# Logging
log-file = "/tmp/argus-daemon.log"
log-level = "info"
```

## IDE Integration

### VS Code

The Argus extension automatically manages the daemon:

```json
// settings.json
{
  "argus.daemon.enabled": true,
  "argus.daemon.port": 4567
}
```

### Neovim

```lua
-- init.lua
require('lspconfig').argus.setup({
  cmd = { "argus", "lsp", "--daemon" },
  -- Uses daemon for analysis
})
```

### Emacs

```elisp
;; Use daemon for flycheck
(setq flycheck-haskell-argus-executable "argus")
(setq flycheck-haskell-argus-args '("check" "--daemon"))
```

## Performance Comparison

| Scenario | Without Daemon | With Daemon |
|----------|----------------|-------------|
| First check | 2.5s | 2.5s (loads) |
| Subsequent | 2.5s | 0.1s |
| After file change | 2.5s | 0.15s |
| Type query | N/A | 5ms |

The daemon provides 10-25x faster analysis for repeated checks.

## Memory Management

The daemon manages memory automatically:

```bash
# Set memory limit
argus daemon start --max-memory 512

# Monitor memory
argus daemon status | grep Memory
```

When memory limit is approached:
1. Evict least-recently-used cache entries
2. Unload unused HIE data
3. If still over limit, restart workers

## Auto-Reload

With `--auto-reload`, the daemon watches for:

- HIE file changes (after recompilation)
- Configuration file changes
- Source file changes (for incremental analysis)

```bash
# Enable auto-reload
argus daemon start --auto-reload

# Manual reload
argus daemon reload
```

## Multiple Projects

Run separate daemons per project:

```bash
# Project A
cd /path/to/project-a
argus daemon start --port 4567 --pid-file /tmp/argus-a.pid

# Project B
cd /path/to/project-b
argus daemon start --port 4568 --pid-file /tmp/argus-b.pid
```

Or use socket files:

```bash
# Project A
argus daemon start --socket /tmp/project-a.sock

# Project B
argus daemon start --socket /tmp/project-b.sock
```

## Logging

```bash
# Start with debug logging
argus daemon start --log /tmp/argus.log --log-level debug

# View logs
tail -f /tmp/argus.log
```

Log format:

```
2024-01-15 14:32:15 INFO  [main] Daemon starting on port 4567
2024-01-15 14:32:15 INFO  [index] Loading 234 HIE files
2024-01-15 14:32:18 INFO  [index] Index built in 3.2s
2024-01-15 14:32:20 DEBUG [worker-1] Check request: src/Handler.hs
2024-01-15 14:32:20 DEBUG [worker-1] Analysis complete: 2 issues (45ms)
```

## Troubleshooting

### Daemon Won't Start

```bash
# Check if already running
argus daemon status

# Check port availability
lsof -i :4567

# Start in foreground to see errors
argus daemon start --foreground
```

### Connection Refused

```bash
# Verify daemon is running
argus daemon status

# Check correct port/socket
argus check --daemon --port 4567 src/
```

### High Memory Usage

```bash
# Lower memory limit
argus daemon restart --max-memory 256

# Reduce cache size
argus daemon restart --cache-size 128
```

### Stale Results

```bash
# Force reload
argus daemon reload

# Or restart
argus daemon restart
```

## Exit Codes

| Code | Condition |
|------|-----------|
| `0` | Success |
| `1` | Daemon not running (for stop/status) |
| `2` | Port already in use |
| `3` | Permission denied |
| `4` | Configuration error |

## Signals

| Signal | Action |
|--------|--------|
| `SIGTERM` | Graceful shutdown |
| `SIGINT` | Graceful shutdown |
| `SIGHUP` | Reload configuration |
| `SIGUSR1` | Reload HIE files |

## See Also

- **[argus lsp](./lsp)**: LSP server (uses daemon internally)
- **[argus watch](./watch)**: File watching
- **[IDE Integration](../usage-guide/ide-integration)**: Editor setup
- **[Configuration](../configuration/daemon-section)**: Daemon settings
