---
sidebar_position: 9
title: argus lsp
description: Start the Language Server Protocol server
---

# argus lsp

Start the Argus Language Server Protocol (LSP) server for IDE integration.

## Synopsis

```bash
argus lsp [OPTIONS]
```

## Description

The `lsp` command starts an LSP server that provides Argus diagnostics, code actions, and quick fixes directly in your editor. It communicates via stdin/stdout using the standard LSP protocol.

## Options

### Server Configuration

| Option | Description |
|--------|-------------|
| `--stdio` | Use stdin/stdout (default) |
| `--tcp PORT` | Listen on TCP port |
| `--socket FILE` | Listen on Unix socket |
| `--log FILE` | Log file for debugging |
| `--log-level LEVEL` | Log level: debug, info, warn, error |

### Analysis Options

| Option | Description |
|--------|-------------|
| `--mode MODE` | Analysis mode: quick, full |
| `--rules RULES` | Enable specific rules |
| `--config FILE` | Configuration file |
| `--hie-dir DIR` | HIE files directory |

### Performance

| Option | Description |
|--------|-------------|
| `--daemon` | Use background daemon |
| `--no-daemon` | Don't use daemon |
| `--workers N` | Number of worker threads |
| `--debounce MS` | Debounce time for diagnostics |

## LSP Capabilities

The Argus LSP server provides:

| Capability | Description |
|------------|-------------|
| Diagnostics | Real-time issue detection |
| Code Actions | Quick fixes for issues |
| Hover | Information about issues |
| Code Lens | Issue counts per function |
| Commands | Fix all, ignore rule, etc. |

## Examples

### Basic Usage

```bash
# Start LSP server (stdio)
argus lsp

# With TCP connection
argus lsp --tcp 5000

# With debug logging
argus lsp --log /tmp/argus-lsp.log --log-level debug
```

### With Daemon

```bash
# Use daemon for faster analysis
argus lsp --daemon

# Start daemon if not running
argus daemon start && argus lsp --daemon
```

## Editor Configuration

### VS Code

Install the Argus extension, or configure manually:

```json
// settings.json
{
  "argus.enable": true,
  "argus.executablePath": "argus",
  "argus.lspArgs": ["lsp", "--daemon"],
  "argus.diagnostics.enable": true,
  "argus.codeActions.enable": true
}
```

### Neovim (nvim-lspconfig)

```lua
-- init.lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

if not configs.argus then
  configs.argus = {
    default_config = {
      cmd = { 'argus', 'lsp', '--daemon' },
      filetypes = { 'haskell', 'lhaskell' },
      root_dir = lspconfig.util.root_pattern(
        'stack.yaml',
        'cabal.project',
        '*.cabal',
        'package.yaml'
      ),
      settings = {},
    },
  }
end

lspconfig.argus.setup({
  on_attach = function(client, bufnr)
    -- Your on_attach configuration
  end,
})
```

### Vim (vim-lsp)

```vim
" .vimrc
if executable('argus')
  au User lsp_setup call lsp#register_server({
    \ 'name': 'argus',
    \ 'cmd': {server_info->['argus', 'lsp', '--daemon']},
    \ 'allowlist': ['haskell', 'lhaskell'],
    \ })
endif
```

### Emacs (lsp-mode)

```elisp
;; init.el
(require 'lsp-mode)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection '("argus" "lsp" "--daemon"))
  :major-modes '(haskell-mode haskell-literate-mode)
  :server-id 'argus))

(add-hook 'haskell-mode-hook #'lsp)
```

### Emacs (eglot)

```elisp
;; init.el
(require 'eglot)

(add-to-list 'eglot-server-programs
             '((haskell-mode haskell-literate-mode)
               . ("argus" "lsp" "--daemon")))
```

### Helix

```toml
# ~/.config/helix/languages.toml
[[language]]
name = "haskell"
language-servers = ["argus", "haskell-language-server"]

[language-server.argus]
command = "argus"
args = ["lsp", "--daemon"]
```

### Sublime Text (LSP)

```json
// LSP.sublime-settings
{
  "clients": {
    "argus": {
      "enabled": true,
      "command": ["argus", "lsp", "--daemon"],
      "selector": "source.haskell"
    }
  }
}
```

## LSP Features

### Diagnostics

Argus sends diagnostics for detected issues:

```json
{
  "uri": "file:///path/to/Handler.hs",
  "diagnostics": [
    {
      "range": {
        "start": {"line": 44, "character": 11},
        "end": {"line": 44, "character": 15}
      },
      "severity": 2,
      "code": "partial/head",
      "source": "argus",
      "message": "Use headMay instead of head"
    }
  ]
}
```

### Code Actions

Available code actions for issues:

- **Quick Fix**: Apply suggested fix
- **Fix All**: Apply all fixes of this type
- **Ignore Rule**: Add suppression comment
- **Ignore Line**: Suppress for this line only
- **Show Documentation**: Open rule documentation

### Hover Information

Hovering over an issue shows:

```
⚠️ partial/head (warning)

Use headMay instead of head

The function `head` throws an exception on empty lists.
Consider using `headMay` from the Safe package which
returns Nothing for empty lists.

Fix: Replace `head items` with `headMay items`

[View Documentation] [Apply Fix]
```

### Code Lens

Shows issue counts above functions:

```haskell
-- ⚠️ 2 warnings
processItems :: [Item] -> IO ()
processItems items = do
  let first = head items    -- warning
  let last = last items     -- warning
  ...
```

## Configuration via LSP

Initialize with custom settings:

```json
{
  "initializationOptions": {
    "mode": "full",
    "rules": {
      "enabled": ["partial/*", "security/*"],
      "disabled": ["modernize/*"]
    },
    "severity": {
      "partial/*": "error"
    }
  }
}
```

Workspace configuration:

```json
{
  "settings": {
    "argus": {
      "diagnostics": {
        "enabled": true,
        "onSave": true,
        "onType": true
      },
      "codeActions": {
        "enabled": true,
        "quickFix": true,
        "fixAll": true
      },
      "codeLens": {
        "enabled": true
      }
    }
  }
}
```

## Diagnostics Timing

| Setting | Behavior |
|---------|----------|
| `onType` | Analyze as you type (with debounce) |
| `onSave` | Analyze when file is saved |
| `onOpen` | Analyze when file is opened |

Configure debounce for typing:

```bash
argus lsp --debounce 500  # Wait 500ms after typing stops
```

## Integration with HLS

Argus can run alongside Haskell Language Server:

### Separate Servers

```lua
-- Neovim: Both servers
lspconfig.hls.setup({})
lspconfig.argus.setup({})
```

Both provide diagnostics, with Argus focusing on linting and HLS on type checking.

### Diagnostic Filtering

Some editors allow filtering diagnostics by source:

```json
// VS Code: Show both
{
  "haskell.diagnostics.enable": true,
  "argus.diagnostics.enable": true
}
```

## Performance

### Startup Time

| Mode | First Analysis | Subsequent |
|------|----------------|------------|
| Quick | ~100ms | ~50ms |
| Full (no daemon) | ~2s | ~500ms |
| Full (with daemon) | ~100ms | ~50ms |

### Memory Usage

| Mode | Memory |
|------|--------|
| Quick | ~50 MB |
| Full | ~200 MB |
| With Daemon | Shared with daemon |

## Logging and Debugging

```bash
# Enable debug logging
argus lsp --log /tmp/argus-lsp.log --log-level debug

# View logs
tail -f /tmp/argus-lsp.log
```

Log entries:

```
[14:32:15] INFO  Argus LSP server starting
[14:32:15] INFO  Using daemon on port 4567
[14:32:15] DEBUG Received: initialize
[14:32:15] DEBUG Sent: initialize response
[14:32:16] DEBUG Received: textDocument/didOpen Handler.hs
[14:32:16] DEBUG Analyzing Handler.hs
[14:32:16] DEBUG Sent: diagnostics (2 issues)
```

## Troubleshooting

### No Diagnostics

1. Check server is running:
   ```bash
   argus lsp --log /tmp/argus.log
   ```

2. Verify file type:
   - Must be `.hs` or `.lhs`
   - Must be in a Haskell project

3. Check configuration:
   - Is Argus enabled in settings?
   - Are rules configured correctly?

### Slow Analysis

```bash
# Use daemon for faster analysis
argus lsp --daemon

# Or use quick mode
argus lsp --mode quick
```

### Server Crashes

```bash
# Check logs
tail -100 /tmp/argus-lsp.log

# Run in foreground with debug
argus lsp --log /dev/stderr --log-level debug
```

## Exit Codes

| Code | Condition |
|------|-----------|
| `0` | Normal exit |
| `1` | Configuration error |
| `2` | Failed to start server |
| `3` | Daemon connection failed |

## See Also

- **[argus daemon](./daemon)**: Background service
- **[IDE Integration](../usage-guide/ide-integration)**: Detailed setup guides
- **[Configuration](../configuration/file-format)**: LSP settings
- **[argus check](./check)**: Command-line analysis
