---
sidebar_position: 2
title: Troubleshooting
description: Solutions to common Argus problems
---

# Troubleshooting

Solutions to common problems when using Argus.

## Installation Issues

### Stack Installation Fails {#stack-issues}

**Problem**: `stack build` fails during Argus installation.

**Solutions**:

1. Update Stack:
```bash
stack upgrade
```

2. Clear cache and retry:
```bash
rm -rf ~/.stack/snapshots
stack build
```

3. Check disk space:
```bash
df -h ~/.stack
```

### GHC Version Mismatch {#ghc-version}

**Problem**: Error about incompatible GHC version.

**Message**:
```
Error: This package requires GHC 9.10.x but you have 9.8.x
```

**Solution**:

1. Check required version in `stack.yaml`
2. Let Stack install correct version:
```bash
stack setup
stack build
```

### Missing Dependencies {#dependencies}

**Problem**: Build fails with missing package.

**Message**:
```
Could not find package xyz
```

**Solution**:

1. Update package index:
```bash
stack update
```

2. Check `extra-deps` in `stack.yaml`:
```yaml
extra-deps:
  - xyz-1.2.3
```

## Analysis Issues

### No Output {#no-output}

**Problem**: Argus runs but produces no output.

**Causes**:
- No issues found
- Files excluded
- Wrong path

**Debug**:
```bash
# Verbose mode
argus check --verbose src/

# Check file discovery
argus check --list-files src/
```

### False Positives {#false-positives}

**Problem**: Argus reports issues that aren't real problems.

**Solutions**:

1. **Suppress specific occurrence**:
```haskell
-- argus:ignore rule-id
code
```

2. **Disable rule globally**:
```toml
[rules]
disable = ["rule-id"]
```

3. **Report the bug**: Open a GitHub issue with minimal reproduction

### Missing Warnings {#missing-warnings}

**Problem**: Expected warning not shown.

**Causes**:
- Rule disabled
- Wrong analysis mode
- File excluded
- Inline suppression

**Debug**:
```bash
# Check enabled rules
argus rules list --enabled

# Check if file is excluded
argus check --verbose --list-files src/

# Try full mode
argus check --mode full src/
```

### Performance Issues {#performance}

**Problem**: Analysis is very slow.

**Solutions**:

1. **Use quick mode**:
```bash
argus check --mode quick src/
```

2. **Enable parallelism**:
```bash
argus check --parallel 8 src/
```

3. **Use daemon mode**:
```bash
argus daemon start
argus check src/  # Uses daemon
```

4. **Exclude generated files**:
```toml
[general]
exclude = ["**/Generated/**", "**/*.gen.hs"]
```

5. **Pre-build HIE files**:
```bash
stack build  # Generate HIE files once
```

### Out of Memory {#memory}

**Problem**: Argus crashes with out of memory error.

**Solutions**:

1. **Reduce parallelism**:
```bash
argus check --parallel 1 src/
```

2. **Process fewer files**:
```bash
argus check src/Module1.hs src/Module2.hs
```

3. **Increase heap size**:
```bash
argus check +RTS -M4G -RTS src/
```

4. **Use quick mode**:
```bash
argus check --mode quick src/
```

## Configuration Issues

### Config Not Found {#config-not-found}

**Problem**: Argus ignores configuration file.

**Check**:
```bash
argus check --verbose src/
# Look for "Loading config from: ..."
```

**Solutions**:

1. **Verify file location**: Must be in current directory or specified with `--config`

2. **Check filename**: Use `argus.toml`, `linter.toml`, or `.argus.toml`

3. **Validate TOML syntax**:
```bash
# Check for syntax errors
cat argus.toml | python3 -c "import toml, sys; toml.load(sys.stdin)"
```

### Rules Not Working {#rules-not-working}

**Problem**: Rules in config don't apply.

**Solutions**:

1. **Check rule ID is correct**:
```bash
argus rules list | grep "rule-name"
```

2. **Verify syntax**:
```toml
# Correct
[rules]
enable = ["partial/head"]

# Wrong
[rules]
enable = [partial/head]  # Missing quotes
```

3. **Check scope conflicts**:
```toml
# Global setting
[rules]
enable = ["my-rule"]

# May be overridden by scope
[[scopes]]
paths = ["src/**"]
[scopes.rules]
disable = ["my-rule"]  # Disables in src/
```

### Severity Not Changing {#severity-issues}

**Problem**: Rule severity remains unchanged.

**Solutions**:

1. **Correct syntax**:
```toml
[rules.severity]
"partial/head" = "error"  # Quotes required
```

2. **Check pattern matching**:
```toml
# Specific rule
[rules.severity]
"partial/head" = "error"

# All in category
[rules.severity]
"partial/*" = "error"

# All rules
[rules.severity]
"*" = "warning"
```

## HIE Issues

### HIE Files Not Found {#hie-not-found}

**Problem**: Full mode fails to find HIE files.

**Message**:
```
Warning: No HIE files found
```

**Solutions**:

1. **Enable HIE generation** in `stack.yaml`:
```yaml
ghc-options:
  "$locals": -fwrite-ide-info -hiedir=.hie
```

2. **Rebuild project**:
```bash
stack clean
stack build
```

3. **Verify HIE files exist**:
```bash
find .hie -name "*.hie" | head
```

### HIE Version Mismatch {#hie-version}

**Problem**: HIE files incompatible with current GHC.

**Message**:
```
Error: HIE file version mismatch
```

**Solution**:

Regenerate HIE files:
```bash
# Clean HIE directory
rm -rf .hie

# Rebuild
stack build
```

### Stale HIE Files {#stale-hie}

**Problem**: Analysis results don't match current source.

**Solution**:

Rebuild affected modules:
```bash
# Force rebuild
stack build --force-dirty

# Or touch the file
touch src/Changed.hs
stack build
```

## Fix Issues

### Fix Not Applied {#fix-not-applied}

**Problem**: `argus fix` doesn't change files.

**Causes**:
- Dry run mode
- No auto-fixable issues
- Permission issues

**Debug**:
```bash
# Check for fixable issues
argus check src/ | grep "fix available"

# Verbose fix
argus fix --verbose src/
```

### Fix Produces Invalid Code {#invalid-fix}

**Problem**: Fixed code doesn't compile.

**Solutions**:

1. **Report bug**: This shouldn't happen - open an issue

2. **Restore backup**:
```bash
argus fix --restore src/
```

3. **Use git**:
```bash
git checkout -- .
```

### Missing Imports After Fix {#missing-imports}

**Problem**: Fix adds function but not import.

**Solutions**:

1. **Enable import addition**:
```toml
[fix]
add-imports = true
```

2. **Manual import**: Add the required import

## Output Issues

### Output Format Problems

**Problem**: Output format not working.

**Solutions**:

1. **Check format name**:
```bash
# Valid formats
argus check --format terminal src/
argus check --format json src/
argus check --format sarif src/
```

2. **Redirect to file**:
```bash
argus check --format json src/ > results.json
```

### SARIF Upload Fails

**Problem**: CI can't process SARIF output.

**Solutions**:

1. **Validate SARIF**:
```bash
argus check --format sarif src/ | jq .
```

2. **Check version compatibility**: Ensure SARIF 2.1.0 is supported

## Editor Integration

### LSP Not Starting {#lsp-issues}

**Problem**: LSP server fails to start.

**Debug**:
```bash
# Test LSP manually
argus lsp --verbose
```

**Solutions**:

1. **Check PATH**: Ensure `argus` is in PATH
2. **Check arguments**: Verify editor configuration
3. **Check logs**: Look at editor's LSP logs

### LSP Not Showing Diagnostics

**Problem**: Editor doesn't show Argus warnings.

**Solutions**:

1. **Check language ID**: Must be `haskell`
2. **Verify file is saved**: LSP analyzes saved files
3. **Check initialization**: Restart LSP server

## Still Having Issues?

### Getting Help

1. **Check documentation**: Browse this site
2. **Search issues**: Check GitHub issues
3. **Ask question**: Open GitHub discussion
4. **Report bug**: Open GitHub issue

### When Reporting Bugs

Include:
- Argus version: `argus --version`
- GHC version: `ghc --version`
- OS and version
- Minimal reproduction
- Expected vs actual behavior
- Config file (if relevant)
