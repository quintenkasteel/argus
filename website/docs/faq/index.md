---
sidebar_position: 11
title: FAQ & Troubleshooting
description: Frequently asked questions and common issues
---

# FAQ & Troubleshooting

Answers to common questions and solutions to frequent issues.

## Quick Links

| Section | Description |
|---------|-------------|
| [General FAQ](./general) | Common questions about Argus |
| [Troubleshooting](./troubleshooting) | Solutions to common problems |
| [Error Messages](./error-messages) | Understanding error output |

## Most Common Questions

### How do I suppress a specific warning?

Use inline comments:

```haskell
-- argus:ignore partial/head
result = head items
```

Or configure in `argus.toml`:

```toml
[rules]
disable = ["partial/head"]
```

### Why isn't my config file being used?

Argus looks for configuration in this order:
1. `--config` flag path
2. `argus.toml` in current directory
3. `linter.toml` in current directory
4. `.argus.toml` in current directory
5. Default configuration

Check: `argus check --verbose src/` to see which config is loaded.

### How do I enable HIE-based analysis?

1. Enable HIE generation in `stack.yaml`:

```yaml
ghc-options:
  "$locals": -fwrite-ide-info -hiedir=.hie
```

2. Rebuild your project:

```bash
stack build
```

3. Run Argus with full mode:

```bash
argus check --mode full src/
```

### Why is analysis slow?

Common causes:
- **Large files**: Split large modules
- **Full mode**: Use `--mode quick` for faster analysis
- **No parallelism**: Use `--parallel 4`

### How do I fix all auto-fixable issues?

```bash
# Preview fixes
argus fix --dry-run src/

# Apply fixes
argus fix src/

# Apply with backup
argus fix --backup src/
```

## Getting Help

- **GitHub Issues**: Report bugs
- **GitHub Discussions**: Ask questions
- **Documentation**: Browse this site

## Common Sections

### Installation Issues

- [Stack installation problems](./troubleshooting#stack-issues)
- [GHC version mismatches](./troubleshooting#ghc-version)
- [Missing dependencies](./troubleshooting#dependencies)

### Analysis Issues

- [False positives](./troubleshooting#false-positives)
- [Missing warnings](./troubleshooting#missing-warnings)
- [Performance problems](./troubleshooting#performance)

### Configuration Issues

- [Config not loading](./troubleshooting#config-not-found)
- [Rules not applying](./troubleshooting#rules-not-working)
- [Severity not changing](./troubleshooting#severity-issues)
