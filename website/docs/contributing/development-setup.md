---
sidebar_position: 1
title: Development Setup
description: Setting up your Argus development environment
---

# Development Setup

This guide walks you through setting up a complete Argus development environment.

## Prerequisites

### Required Tools

| Tool | Version | Purpose |
|------|---------|---------|
| Stack | 2.13+ | Build system |
| GHC | 9.10.3 | Haskell compiler |
| Git | 2.30+ | Version control |

### Optional Tools

| Tool | Purpose |
|------|---------|
| HLS | IDE integration |
| ghcid | Fast recompilation |
| hlint | Additional linting |

## Installation

### 1. Install Stack

```bash
# Linux/macOS
curl -sSL https://get.haskellstack.org/ | sh

# Verify installation
stack --version
```

### 2. Clone Repository

```bash
git clone https://github.com/your-org/argus.git
cd argus
```

### 3. Build Project

```bash
# Initial build (downloads GHC)
stack build

# Build with optimizations
stack build --fast

# Build with all warnings
stack build --pedantic
```

### 4. Run Tests

```bash
# Run all tests
stack test

# Run with verbose output
stack test --ta '-v'

# Run specific tests
stack test --ta '-m "ASTMatch"'
```

### 5. Verify Setup

```bash
# Run Argus on itself
stack run -- check src/

# Check version
stack run -- --version
```

## IDE Setup

### VS Code with HLS

1. Install Haskell extension
2. Configure settings:

```json
{
  "haskell.serverExecutablePath": "haskell-language-server-wrapper",
  "haskell.formattingProvider": "ormolu"
}
```

3. Open the project folder

### Vim/Neovim

1. Install coc.nvim or nvim-lspconfig
2. Configure HLS:

```vim
" coc-settings.json
{
  "languageserver": {
    "haskell": {
      "command": "haskell-language-server-wrapper",
      "args": ["--lsp"],
      "filetypes": ["haskell", "lhaskell"]
    }
  }
}
```

### Emacs

1. Install lsp-mode and lsp-haskell
2. Configure:

```elisp
(use-package lsp-haskell
  :ensure t
  :config
  (setq lsp-haskell-server-path "haskell-language-server-wrapper"))
```

## Development Workflow

### Fast Feedback Loop

Use ghcid for instant feedback:

```bash
# Install ghcid
stack install ghcid

# Run with type checking
ghcid --command="stack ghci"

# Run with tests
ghcid --command="stack ghci test/Spec.hs" --test=":main"
```

### Building Efficiently

```bash
# Fast builds (skip optimization)
stack build --fast

# Build only changed modules
stack build --file-watch

# Build and copy executable
stack install
```

### Running Argus

```bash
# Via Stack
stack run -- check src/

# After stack install
argus check src/

# With specific config
stack run -- --config custom.toml check src/
```

## Project Structure

```
argus/
├── app/
│   └── Main.hs              # CLI entry point
│
├── src/
│   └── Argus/               # Main source
│       ├── Types.hs         # Core types
│       ├── Core.hs          # Orchestration
│       ├── CLI.hs           # CLI parsing
│       ├── Config.hs        # Configuration
│       ├── Analysis/        # Analysis modules
│       ├── Rules/           # Lint rules
│       ├── Refactor/        # Auto-fix
│       ├── Output/          # Formatters
│       └── HIE/             # HIE integration
│
├── test/
│   ├── Spec.hs              # Test driver
│   ├── TestUtils.hs         # Test helpers
│   ├── *Spec.hs             # Test modules
│   └── golden/              # Golden test data
│
├── bench/
│   └── Main.hs              # Benchmarks
│
├── data/
│   └── default-rules.toml   # Built-in rules
│
├── stack.yaml               # Stack config
├── package.yaml             # Package definition
└── argus.toml               # Default config
```

## Configuration Files

### stack.yaml

```yaml
resolver: lts-24.21

packages:
  - .

ghc-options:
  "$locals": -fwrite-ide-info -hiedir=.hie
```

### package.yaml

```yaml
name: argus
version: 0.1.0

dependencies:
  - base >= 4.7 && < 5
  - ghc
  - ghc-paths
  - syb
  # ... more dependencies

ghc-options:
  - -Wall
  - -Wcompat
```

## Common Tasks

### Adding a Dependency

1. Add to `package.yaml`:

```yaml
dependencies:
  - new-package >= 1.0
```

2. Rebuild:

```bash
stack build
```

### Updating GHC Version

1. Update `stack.yaml`:

```yaml
resolver: lts-XX.YY  # New LTS
```

2. Rebuild:

```bash
stack build
```

### Generating Documentation

```bash
# Build Haddock docs
stack haddock

# Open documentation
open .stack-work/dist/*/doc/html/argus/index.html
```

## Troubleshooting

### Build Fails

```bash
# Clean and rebuild
stack clean
stack build

# Clear cache completely
rm -rf .stack-work
stack build
```

### Test Failures

```bash
# Run single failing test
stack test --ta '-m "failing test" -v'

# With fixed seed
stack test --ta '--seed 12345'
```

### GHC Version Issues

```bash
# Check resolver GHC version
stack exec -- ghc --version

# Ensure matching versions
stack setup
```

### HLS Not Working

```bash
# Generate HIE files
stack build --ghc-options="-fwrite-ide-info"

# Restart HLS in editor
```

## Next Steps

- **[Code Style](./code-style)**: Coding conventions
- **[Adding Rules](./adding-rules)**: Create new rules
- **[Testing Guide](./testing-guide)**: Write tests
