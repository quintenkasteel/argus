---
sidebar_position: 3
title: Error Messages
description: Understanding Argus error messages
---

# Error Messages

This guide explains common Argus error messages and how to resolve them.

## Parse Errors

### ParseError: Invalid syntax

**Message**:
```
ParseError: Invalid syntax at line 42, column 15
  |
42| function x y = let in x
  |               ^^^
```

**Cause**: Source file has syntax errors that prevent parsing.

**Solution**: Fix the syntax error in your source file. Argus cannot analyze files with parse errors.

### ParseError: Unsupported extension

**Message**:
```
ParseError: Language extension 'UnboxedTuples' not supported
```

**Cause**: File uses an extension not enabled by default.

**Solution**: Add extension to file or config:

```haskell
{-# LANGUAGE UnboxedTuples #-}
```

Or in `argus.toml`:
```toml
[general]
extensions = ["UnboxedTuples"]
```

## Configuration Errors

### ConfigError: Invalid TOML

**Message**:
```
ConfigError: Invalid TOML at line 5
  Expected '=' after key
```

**Cause**: Configuration file has TOML syntax error.

**Solution**: Fix the syntax error. Common issues:

```toml
# Wrong: missing quotes
[rules]
enable = [partial/head]

# Right: with quotes
[rules]
enable = ["partial/head"]
```

### ConfigError: Unknown rule

**Message**:
```
ConfigError: Unknown rule 'partial/heaf' in configuration
  Did you mean 'partial/head'?
```

**Cause**: Rule ID is misspelled or doesn't exist.

**Solution**: Use correct rule ID:

```bash
# List all rules to find correct ID
argus rules list | grep partial
```

### ConfigError: Invalid severity

**Message**:
```
ConfigError: Invalid severity 'warn' for rule 'partial/head'
  Valid values: error, warning, info, hint
```

**Cause**: Severity value is not recognized.

**Solution**: Use valid severity:
```toml
[rules.severity]
"partial/head" = "warning"  # Not 'warn'
```

### ConfigError: Invalid glob pattern

**Message**:
```
ConfigError: Invalid glob pattern '***/src'
  at key 'general.include'
```

**Cause**: Glob pattern syntax is incorrect.

**Solution**: Fix glob pattern:
```toml
[general]
include = ["**/src/**"]  # Correct
```

## HIE Errors

### HIEError: File not found

**Message**:
```
HIEError: No HIE file found for src/Module.hs
  Expected: .hie/Module.hie
```

**Cause**: HIE files haven't been generated.

**Solution**:
1. Enable HIE in `stack.yaml`:
```yaml
ghc-options:
  "$locals": -fwrite-ide-info -hiedir=.hie
```

2. Rebuild:
```bash
stack build
```

### HIEError: Version mismatch

**Message**:
```
HIEError: HIE file version mismatch
  File version: 9.8.2
  Expected: 9.10.1
```

**Cause**: HIE files were generated with different GHC version.

**Solution**: Regenerate HIE files:
```bash
rm -rf .hie
stack build
```

### HIEError: Corrupt file

**Message**:
```
HIEError: Failed to read HIE file .hie/Module.hie
  Corrupt or incomplete file
```

**Cause**: HIE file is corrupted.

**Solution**: Delete and regenerate:
```bash
rm .hie/Module.hie
stack build src/Module.hs
```

## Analysis Errors

### AnalysisError: Timeout

**Message**:
```
AnalysisError: Analysis timeout after 60s
  File: src/LargeModule.hs
```

**Cause**: File is too complex or large.

**Solutions**:

1. Increase timeout:
```bash
argus check --timeout 120 src/
```

2. Split large files

3. Exclude problematic file:
```toml
[general]
exclude = ["src/LargeModule.hs"]
```

### AnalysisError: Internal error

**Message**:
```
AnalysisError: Internal error in rule 'partial/head'
  Please report this bug
```

**Cause**: Bug in Argus.

**Solution**: Report the issue on GitHub with:
- Argus version
- Minimal reproduction code
- Full error message

## Fix Errors

### FixError: Conflict

**Message**:
```
FixError: Overlapping fixes at line 42
  Fix 1: Replace 'head xs' with 'headMay xs'
  Fix 2: Replace 'head' with 'headDef []'
```

**Cause**: Multiple fixes apply to the same location.

**Solution**: Argus applies the first fix. To choose different:
1. Run with specific rule:
```bash
argus fix --only "partial/head" src/
```

2. Fix manually

### FixError: Parse failure

**Message**:
```
FixError: Fixed code does not parse
  Original was valid, fix is invalid
  This is a bug - please report
```

**Cause**: Bug in fix generation.

**Solution**: Report on GitHub with reproduction case.

### FixError: Permission denied

**Message**:
```
FixError: Permission denied writing to src/Module.hs
```

**Cause**: File is read-only or permission issues.

**Solution**:
```bash
chmod u+w src/Module.hs
```

## Output Errors

### OutputError: Invalid format

**Message**:
```
OutputError: Unknown output format 'xml'
  Valid formats: terminal, json, sarif, html, junit, codeclimate, checkstyle, github
```

**Cause**: Specified format is not supported.

**Solution**: Use valid format name:
```bash
argus check --format json src/
```

### OutputError: Write failed

**Message**:
```
OutputError: Failed to write to output.json
  Permission denied
```

**Cause**: Can't write to output location.

**Solution**: Check permissions and path:
```bash
# Ensure directory exists
mkdir -p reports
argus check --format json src/ > reports/output.json
```

## LSP Errors

### LSPError: Initialization failed

**Message**:
```
LSPError: Failed to initialize LSP server
  Port 8080 already in use
```

**Cause**: Another process using the port.

**Solution**: Use different port or stop other process:
```bash
argus lsp --port 8081
```

### LSPError: Client disconnected

**Message**:
```
LSPError: Client disconnected unexpectedly
```

**Cause**: Editor closed connection.

**Solution**: Restart the editor's LSP client.

## Daemon Errors

### DaemonError: Already running

**Message**:
```
DaemonError: Daemon already running (PID: 12345)
```

**Cause**: Daemon already started.

**Solution**: Use existing daemon or restart:
```bash
argus daemon stop
argus daemon start
```

### DaemonError: Connection refused

**Message**:
```
DaemonError: Cannot connect to daemon
  Connection refused at localhost:7890
```

**Cause**: Daemon not running.

**Solution**: Start daemon:
```bash
argus daemon start
```

## Error Code Reference

| Exit Code | Meaning |
|-----------|---------|
| 0 | Success, no issues found |
| 1 | Analysis completed, issues found |
| 2 | Configuration error |
| 3 | Parse error |
| 4 | Internal error |
| 5 | Permission error |
| 6 | Timeout |

## Getting More Information

### Verbose Mode

```bash
argus check --verbose src/
```

Shows:
- Configuration loading
- File discovery
- Rule evaluation
- Timing information

### Debug Mode

```bash
argus check --debug src/
```

Shows:
- All of verbose output
- AST details
- Match attempts
- Internal state

### Report Issues

When reporting errors, include:
- Full error message
- Command used
- Argus version (`argus --version`)
- GHC version (`ghc --version`)
- Minimal reproduction
- Configuration file (if relevant)
