---
sidebar_position: 4
title: IDE Integration
description: Integrate Argus with your code editor
---

# IDE Integration

Argus provides an LSP (Language Server Protocol) server for real-time diagnostics in your editor, plus support for various IDE-specific integrations.

## LSP Server

The Argus LSP server provides real-time analysis as you code.

### Starting the Server

```bash
argus lsp
```

The server communicates via stdin/stdout using the LSP protocol.

### LSP Features

| Feature | Status | Description |
|---------|--------|-------------|
| Diagnostics | Full | Real-time issue detection |
| Code Actions | Full | Quick fixes for issues |
| Hover | Full | Information on hover |
| Go to Definition | Full | Navigate to definitions |
| Find References | Full | Find all references |
| Document Symbols | Full | Outline view |
| Progress | Full | Analysis progress indicators |

### Server Options

```bash
# With custom config
argus lsp --config argus.toml

# Verbose logging
argus lsp --verbose

# Log to file
argus lsp --log-file /tmp/argus-lsp.log
```

## VS Code

### Option 1: Generic LSP Extension

1. Install "Generic LSP Client" extension
2. Configure in `settings.json`:

```json
{
  "lsp.languageServers": {
    "argus": {
      "command": "argus",
      "args": ["lsp"],
      "filetypes": ["haskell"]
    }
  }
}
```

### Option 2: Haskell Extension

The Haskell extension (haskell.haskell) supports multiple language servers. Add Argus as an additional server:

```json
{
  "haskell.languageServerVariant": "haskell-language-server",
  "haskell.plugin.argus.globalOn": true
}
```

### Option 3: SARIF Viewer

For asynchronous analysis with SARIF output:

1. Install "SARIF Viewer" extension
2. Run analysis:

```bash
argus check src/ --format sarif > .argus-results.sarif
```

3. Open `.argus-results.sarif` in VS Code

### Recommended Settings

```json
{
  "files.associations": {
    "*.hs": "haskell"
  },
  "editor.formatOnSave": false,
  "[haskell]": {
    "editor.rulers": [80, 100]
  }
}
```

## Neovim

### Native LSP (nvim-lspconfig)

```lua
-- In your init.lua or lspconfig setup

local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

-- Define Argus as a custom server
if not configs.argus then
  configs.argus = {
    default_config = {
      cmd = { 'argus', 'lsp' },
      filetypes = { 'haskell', 'lhaskell' },
      root_dir = lspconfig.util.root_pattern(
        'argus.toml',
        'stack.yaml',
        'cabal.project',
        '*.cabal',
        'package.yaml'
      ),
      settings = {},
    },
  }
end

-- Enable Argus
lspconfig.argus.setup({
  on_attach = function(client, bufnr)
    -- Your on_attach function
  end,
  capabilities = capabilities,
})
```

### With Haskell Language Server

Run Argus alongside HLS:

```lua
-- Enable both HLS and Argus
lspconfig.hls.setup({})
lspconfig.argus.setup({})
```

### Diagnostic Display

```lua
-- Customize diagnostic display
vim.diagnostic.config({
  virtual_text = {
    prefix = '●',
    source = 'always',
  },
  float = {
    source = 'always',
  },
  signs = true,
  underline = true,
})

-- Custom signs for Argus diagnostics
vim.fn.sign_define('DiagnosticSignError', { text = '', texthl = 'DiagnosticSignError' })
vim.fn.sign_define('DiagnosticSignWarn', { text = '', texthl = 'DiagnosticSignWarn' })
vim.fn.sign_define('DiagnosticSignInfo', { text = '', texthl = 'DiagnosticSignInfo' })
vim.fn.sign_define('DiagnosticSignHint', { text = '', texthl = 'DiagnosticSignHint' })
```

### Keybindings

```lua
vim.keymap.set('n', '<leader>af', ':!argus fix % --preview<CR>', { desc = 'Argus preview fixes' })
vim.keymap.set('n', '<leader>aF', ':!argus fix % --safe-only<CR>', { desc = 'Argus apply safe fixes' })
vim.keymap.set('n', '<leader>ac', ':!argus check %<CR>', { desc = 'Argus check file' })
```

## Emacs

### Using lsp-mode

```elisp
;; In your init.el

(require 'lsp-mode)

;; Register Argus as an LSP server
(lsp-register-client
  (make-lsp-client
    :new-connection (lsp-stdio-connection '("argus" "lsp"))
    :major-modes '(haskell-mode haskell-literate-mode)
    :server-id 'argus
    :priority -1))  ; Lower priority than HLS

;; Enable for Haskell buffers
(add-hook 'haskell-mode-hook #'lsp)
```

### Using eglot (Emacs 29+)

```elisp
(require 'eglot)

(add-to-list 'eglot-server-programs
             '((haskell-mode haskell-literate-mode) . ("argus" "lsp")))

(add-hook 'haskell-mode-hook #'eglot-ensure)
```

### Flycheck Integration

For non-LSP usage with flycheck:

```elisp
(require 'flycheck)

(flycheck-define-checker haskell-argus
  "Argus static analyzer for Haskell."
  :command ("argus" "check" "--format" "json" source)
  :error-parser flycheck-parse-json
  :modes (haskell-mode haskell-literate-mode))

(add-to-list 'flycheck-checkers 'haskell-argus)
```

## IntelliJ IDEA

### LSP Support Plugin

1. Install "LSP Support" plugin from JetBrains Marketplace
2. Configure in Settings → Languages & Frameworks → LSP:

```
Server: argus
Executable: /path/to/argus
Arguments: lsp
File Extensions: .hs
```

### External Tool

1. Go to Settings → Tools → External Tools
2. Add new tool:
   - Name: Argus Check
   - Program: argus
   - Arguments: check $FilePath$
   - Working directory: $ProjectFileDir$

## Sublime Text

### LSP Package

1. Install "LSP" package via Package Control
2. Add to LSP settings:

```json
{
  "clients": {
    "argus": {
      "enabled": true,
      "command": ["argus", "lsp"],
      "selector": "source.haskell"
    }
  }
}
```

## Vim (Classic)

### ALE (Asynchronous Lint Engine)

```vim
" In your .vimrc

let g:ale_linters = {
\   'haskell': ['argus'],
\}

let g:ale_haskell_argus_executable = 'argus'
let g:ale_haskell_argus_options = 'check --format json'
```

### Syntastic (Legacy)

```vim
let g:syntastic_haskell_checkers = ['argus']
let g:syntastic_haskell_argus_exec = 'argus'
let g:syntastic_haskell_argus_args = 'check --format json'
```

## Watch Mode

For editors without LSP support, use watch mode:

```bash
# In a terminal, run watch mode
argus watch src/

# Output updates on file save
Watching src/ for changes...
[14:32:15] src/MyModule.hs changed
  ⚠ warning [partial/head] line 42
[14:32:20] src/Utils.hs changed
  ✓ no issues
```

## Terminal Integration

### tmux/screen

Run Argus watch in a split:

```bash
# tmux
tmux split-window -h 'argus watch src/'

# screen
screen -S argus argus watch src/
```

### Kitty/iTerm

Set up a hotkey to run analysis:

```bash
# In kitty.conf
map ctrl+shift+a launch --type=overlay argus check --format terminal src/
```

## Troubleshooting

### LSP Not Starting

```bash
# Test LSP manually
echo '{"jsonrpc":"2.0","method":"initialize","id":1,"params":{}}' | argus lsp
```

### No Diagnostics

1. Check Argus config exists:
```bash
ls argus.toml || argus init
```

2. Verify server is running:
```bash
# Check for argus processes
ps aux | grep argus
```

3. Check LSP logs:
```bash
argus lsp --log-file /tmp/argus.log
tail -f /tmp/argus.log
```

### Performance Issues

For large projects:

```toml
# argus.toml
[general]
mode = "quick"  # Use quick mode for real-time
exclude = ["test/**", "dist/**"]

[resource]
timeout-seconds = 5  # Shorter timeout for LSP
```

### Conflicting with HLS

Run servers on different priorities or disable overlapping features:

```lua
-- Neovim: Disable Argus diagnostics if using HLS
lspconfig.argus.setup({
  handlers = {
    ["textDocument/publishDiagnostics"] = function() end,
  },
})
```

## Next Steps

- **[Working with HIE](./working-with-hie)**: Deep semantic analysis
- **[CLI Reference](../cli-reference)**: All commands and options
- **[Configuration](../configuration/file-format)**: Customize Argus
