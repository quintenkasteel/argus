---
sidebar_position: 1
title: Installation
description: Install Argus using Stack
---

# Installation

Argus is built with Stack and requires GHC 9.10.3. This guide covers installation from source.

## Prerequisites

### Stack

Argus uses Stack exclusively for building. Install Stack if you haven't already:

```bash
# macOS (Homebrew)
brew install haskell-stack

# Linux (recommended)
curl -sSL https://get.haskellstack.org/ | sh

# Windows (Chocolatey)
choco install haskell-stack
```

Verify your installation:

```bash
stack --version
# The Haskell Tool Stack, Version 2.13.1
```

### GHC Version

Argus requires GHC 9.10.3, which Stack will automatically download and manage:

```bash
stack setup
```

## Installing from Source

### Clone and Build

```bash
# Clone the repository
git clone https://github.com/quinten/argus.git
cd argus

# Build the project
stack build

# Install to your PATH
stack install
```

The `stack install` command copies the `argus` executable to `~/.local/bin/` (Linux/macOS) or the equivalent Stack bin directory on Windows.

### Verify Installation

```bash
argus --version
# Argus 1.0.0

argus --help
# Shows available commands and options
```

### Add to PATH

Ensure Stack's bin directory is in your PATH:

```bash
# Add to ~/.bashrc, ~/.zshrc, or equivalent
export PATH="$HOME/.local/bin:$PATH"
```

## Build Options

### Development Build

For faster iteration during development:

```bash
stack build --fast
```

### Optimized Build

For production use with full optimizations:

```bash
stack build --ghc-options="-O2"
```

### With HIE Files

To enable full mode analysis on Argus itself:

```bash
stack build --ghc-options="-fwrite-ide-info -hiedir=.hie"
```

## Running Without Installing

You can run Argus directly without installing:

```bash
stack run -- check src/
stack run -- fix src/ --preview
stack run -- unused src/
```

## Verifying the Installation

Run Argus on its own source code:

```bash
cd /path/to/argus
argus check src/
```

You should see diagnostic output (Argus analyzes itself and reports some issues).

## Updating Argus

To update to the latest version:

```bash
cd /path/to/argus
git pull origin main
stack build
stack install
```

## Supported Platforms

| Platform | Status | Notes |
|----------|--------|-------|
| Linux (x86_64) | Fully supported | Primary development platform |
| macOS (x86_64) | Fully supported | Intel Macs |
| macOS (ARM64) | Fully supported | Apple Silicon |
| Windows (x86_64) | Supported | Some path handling differences |

## Dependencies

Argus depends on several Haskell packages that Stack manages automatically:

| Dependency | Purpose |
|------------|---------|
| `ghc-lib-parser` | GHC's parser for syntax analysis |
| `ghc` | GHC library for HIE file access |
| `hiedb` | HIE database queries |
| `tomland` | TOML configuration parsing |
| `optparse-applicative` | CLI argument parsing |
| `prettyprinter` | Diagnostic output formatting |
| `aeson` | JSON serialization |

Full dependency list is in `package.yaml`.

## Troubleshooting Installation

### Stack Download Issues

If Stack can't download GHC:

```bash
# Try with a different mirror
stack setup --resolver lts-24.21

# Or download GHC manually and point Stack to it
stack setup --ghc-variant=standard
```

### Build Failures

If the build fails:

```bash
# Clean and rebuild
stack clean
stack build

# Check for dependency conflicts
stack build --dry-run
```

### Permission Issues

If `stack install` fails with permission errors:

```bash
# Ensure bin directory exists
mkdir -p ~/.local/bin

# Check ownership
ls -la ~/.local/bin
```

### Memory Issues

For large codebases, you may need to increase Stack's memory limit:

```bash
stack build --ghc-options="+RTS -M4G -RTS"
```

## Next Steps

- **[Basic Usage](./basic-usage)**: Learn the fundamental commands
- **[Configuration](./configuration)**: Set up `argus.toml` for your project
- **[Project Setup](./project-setup)**: Configure HIE files and workflows
