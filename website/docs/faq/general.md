---
sidebar_position: 1
title: General FAQ
description: Frequently asked questions about Argus
---

# General FAQ

Common questions about Argus and its features.

## About Argus

### What is Argus?

Argus is a static analysis tool for Haskell that detects common issues, suggests improvements, and can automatically fix many problems. It combines:

- **Syntactic analysis**: Pattern matching on source code
- **Semantic analysis**: Type-aware checks using HIE files
- **Auto-fix**: Safe automatic code corrections

### How is Argus different from HLint?

| Feature | Argus | HLint |
|---------|-------|-------|
| Syntactic rules | Yes | Yes |
| Type-aware rules | Yes | Limited |
| Auto-fix | Yes | Limited |
| Security rules | Yes | No |
| Space leak detection | Yes | No |
| Architecture rules | Yes | No |
| Custom rules | DSL + Haskell | YAML + Haskell |
| GHC plugin mode | Yes | No |

### What GHC versions are supported?

Argus currently supports:
- GHC 9.10.x (primary)
- GHC 9.8.x (supported)
- GHC 9.6.x (supported)

Older versions may work but are not tested.

### Is Argus production-ready?

Yes. Argus is designed for production use with:
- Comprehensive test suite
- CI/CD integration
- Safe auto-fix with rollback
- Performance optimization

## Analysis

### What kinds of issues does Argus detect?

Argus detects issues across categories:

- **Partial functions**: `head`, `tail`, `fromJust`, `!!`
- **Security**: Path traversal, injection vulnerabilities
- **Performance**: Inefficient patterns, space leaks
- **Redundant code**: Unnecessary operations
- **Complexity**: Overly complex functions
- **Naming**: Convention violations
- **Imports**: Unused, redundant, missing
- **Modernization**: Outdated patterns

### What's the difference between analysis modes?

| Mode | Speed | Depth | Requirements |
|------|-------|-------|--------------|
| Quick | Fast | Syntactic only | None |
| Full | Medium | + HIE types | HIE files |
| Plugin | Slow | + GHC internals | Rebuild |

### Can Argus analyze Template Haskell?

Template Haskell is partially supported:
- Splices are analyzed as-is
- Generated code is not analyzed in quick mode
- Full mode with HIE can see some generated code

### Does Argus work with CPP?

Yes, with limitations:
- Argus analyzes the source as-is
- CPP branches are not evaluated
- Consider running on preprocessed output for complete analysis

## Auto-Fix

### Is auto-fix safe?

Yes, Argus auto-fix is designed to be safe:

- **Preview first**: `--dry-run` shows changes without applying
- **Backups**: `--backup` saves originals
- **Valid output**: Fixes produce parseable code
- **Semantic preservation**: Fixes maintain behavior

### Can I undo auto-fixes?

Yes:

```bash
# If you used --backup
argus fix --restore src/

# Using git
git checkout -- .

# Specific file
git checkout -- src/Problem.hs
```

### Why doesn't every warning have a fix?

Some issues can't be automatically fixed because:

- Multiple valid solutions exist
- Fix requires understanding intent
- Change would affect other code
- Type-level changes needed

### Can I customize fix behavior?

Yes, in `argus.toml`:

```toml
[fix]
# Only apply these rules
only = ["partial/*", "redundant/*"]

# Skip these rules
skip = ["modernize/*"]

# Import handling
add-imports = true
qualify-imports = false
```

## Configuration

### Where should I put my config file?

Recommended locations:

1. **Project root**: `argus.toml` (most common)
2. **Hidden file**: `.argus.toml`
3. **Alternative name**: `linter.toml`

### How do I disable a rule globally?

In `argus.toml`:

```toml
[rules]
disable = ["rule-id"]
```

### How do I disable a rule for one line?

```haskell
-- argus:ignore rule-id
problematicCode
```

### How do I change a rule's severity?

In `argus.toml`:

```toml
[rules.severity]
"partial/head" = "error"
"redundant/id" = "hint"
```

### Can I have different settings for test code?

Yes, use scopes:

```toml
[[scopes]]
paths = ["test/**"]

[scopes.rules]
disable = ["partial/*"]

[scopes.rules.severity]
"*" = "hint"
```

## Integration

### How do I use Argus in CI?

```yaml
# GitHub Actions
- name: Run Argus
  run: |
    stack run -- check src/ --format sarif > results.sarif

- name: Upload results
  uses: github/codeql-action/upload-sarif@v2
  with:
    sarif_file: results.sarif
```

### Does Argus support LSP?

Yes:

```bash
argus lsp
```

Configure your editor to use Argus as an LSP server.

### Can I use Argus with pre-commit?

Yes:

```yaml
# .pre-commit-config.yaml
repos:
  - repo: local
    hooks:
      - id: argus
        name: argus
        entry: stack run -- check
        language: system
        types: [haskell]
```

### How do I use Argus with VS Code?

1. Install the Haskell extension
2. Configure to use Argus LSP:

```json
{
  "haskell.serverExecutablePath": "argus",
  "haskell.serverArgs": ["lsp"]
}
```

## Rules

### How many rules does Argus have?

Argus includes 1,100+ rules across 16 categories:
- Partial functions
- Security
- Performance
- Space leaks
- Redundant code
- Complexity
- And more...

### Can I write custom rules?

Yes, two ways:

**DSL (TOML)**:
```toml
[[patterns.rules]]
name = "my-rule"
match = "problematic $X"
fix = "better $X"
message = "Use better instead"
```

**Haskell**:
```haskell
myRule :: Rule
myRule = patternRule
  { ruleId = "custom/my-rule"
  , ruleMatch = "problematic $X"
  , ruleFix = Just "better $X"
  }
```

### How do I see all available rules?

```bash
# List all rules
argus rules list

# Filter by category
argus rules list --category partial

# Search rules
argus rules search "head"
```

### Why is a rule not triggering?

Check:
1. Is rule enabled? Check config
2. Right analysis mode? Some rules need `--mode full`
3. File excluded? Check `exclude` patterns
4. Suppressed? Check for `argus:ignore` comments

## Performance

### How fast is Argus?

Typical performance:
- Quick mode: ~1000 files/second
- Full mode: ~100 files/second (with cached HIE)
- Plugin mode: Adds 10-30% to build time

### How do I make Argus faster?

1. **Use quick mode**: `--mode quick`
2. **Enable parallelism**: `--parallel 8`
3. **Exclude generated code**: Configure `exclude`
4. **Use daemon mode**: `argus daemon start`
5. **Pre-build HIE files**: `stack build`

### Why is first run slow?

First run may be slow because:
- HIE files being generated
- Caches being built
- Full project scan

Subsequent runs use caches and are faster.

## Compatibility

### Does Argus work on Windows?

Yes, Argus works on:
- Linux
- macOS
- Windows (via Stack)

### Does Argus work with Cabal?

Argus is built with Stack, but can analyze Cabal projects. For HIE files with Cabal:

```bash
cabal build --ghc-options="-fwrite-ide-info -hiedir=.hie"
```

### Does Argus work with Nix?

Yes, you can use Argus with Nix-based projects. Ensure HIE files are generated during build.

## Troubleshooting

For detailed troubleshooting, see:
- [Troubleshooting Guide](./troubleshooting)
- [Error Messages](./error-messages)
