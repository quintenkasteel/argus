---
sidebar_position: 1
title: Introduction
description: Learn about Argus, the all-seeing Haskell static analyzer
---

# Introduction to Argus

**Argus** is an enterprise-grade static analysis tool for Haskell that goes far beyond traditional linting. Named after the hundred-eyed giant of Greek mythology, Argus sees everything in your codebase—from simple style issues to complex architectural violations.

## What is Argus?

Argus combines three powerful analysis approaches:

1. **Syntactic Pattern Matching**: Fast, compilation-free analysis using GHC's parser
2. **Semantic Analysis via HIE Files**: Deep type-aware analysis using GHC's interface files
3. **GHC Plugin Integration**: Compile-time analysis with full type information

This multi-mode architecture lets you choose the right trade-off between speed and depth for your workflow.

## Why Argus?

### Beyond Simple Linting

Most Haskell linters focus on style and simple patterns. Argus provides:

- **Security analysis** that catches injection vulnerabilities and hardcoded secrets
- **Performance detection** that identifies space leaks and fusion blockers
- **Architecture analysis** that enforces layer boundaries and detects circular dependencies
- **Unused code detection** with Template Haskell awareness

### Production-Ready Auto-Fix

Argus doesn't just report issues—it fixes them:

- **Transactional semantics**: All-or-nothing application with automatic rollback
- **Conflict detection**: Identifies overlapping edits and dependent fixes
- **Validation pipeline**: Re-parses after each fix to ensure correctness
- **Interactive mode**: Review and approve each change individually

### CI/CD Integration

Argus integrates seamlessly with modern development workflows:

- **SARIF output** for GitHub Code Scanning and Azure DevOps
- **JSON output** for custom tooling and dashboards
- **JUnit XML** for test framework integration
- **Exit codes** that work with CI pipelines

## Who Should Use Argus?

### Application Developers

Use Argus to catch common mistakes before they reach production:

- Partial function usage that causes runtime crashes
- Space leaks from lazy evaluation
- Security vulnerabilities in web handlers
- Inefficient algorithms and data structures

### Library Authors

Ensure your library meets quality standards:

- API consistency and naming conventions
- Proper error handling (no `error` or `undefined`)
- Performance characteristics documented and enforced
- Minimal dependencies and clean architecture

### Tech Leads and Architects

Enforce team standards and architectural decisions:

- Define and enforce layer boundaries
- Detect circular dependencies before they become problems
- Track complexity metrics over time
- Standardize coding conventions across teams

### Security Teams

Audit Haskell codebases for security issues:

- SQL injection via string concatenation
- Command injection through shell calls
- Hardcoded credentials and API keys
- Unsafe FFI and `unsafePerformIO` usage

## Key Capabilities

| Capability | Description |
|------------|-------------|
| **1,100+ Rules** | Comprehensive coverage across 16 categories |
| **3 Analysis Modes** | Quick, Full (HIE), and Plugin modes |
| **8 Output Formats** | Terminal, JSON, SARIF, HTML, JUnit, and more |
| **Safe Auto-Fix** | Transactional refactoring with rollback |
| **LSP Server** | Real-time diagnostics in your editor |
| **Watch Mode** | Continuous analysis as you code |
| **Architecture Analysis** | Layer violations, circular deps, coupling metrics |
| **Custom Rules** | DSL for project-specific rules |

## How Argus Compares

| Feature | Argus | HLint | GHC Warnings |
|---------|-------|-------|--------------|
| Syntax analysis | ✓ | ✓ | ✓ |
| Semantic analysis | ✓ | Limited | ✓ |
| Security rules | ✓ | - | - |
| Space leak detection | ✓ | Limited | - |
| Architecture analysis | ✓ | - | - |
| Auto-fix | ✓ | ✓ | - |
| SARIF output | ✓ | - | - |
| Custom DSL | ✓ | ✓ | - |
| Template Haskell aware | ✓ | - | ✓ |

## Philosophy

Argus is built on several core principles:

### Correctness Over Convenience

We prioritize accurate analysis over false positives. Every rule is designed to catch real issues, not stylistic preferences.

### Configurable, Not Opinionated

Argus provides sensible defaults but allows extensive customization. Disable rules that don't fit your project, adjust severity levels, or write your own rules.

### Fast Feedback Loops

Quick mode provides instant feedback without compilation. Full mode gives deeper analysis when you need it. You choose when to use each.

### Safe Refactoring

Auto-fix never makes changes it can't verify. The transactional system ensures your code is never left in a broken state.

## Next Steps

- **[Key Features](./key-features)**: Explore Argus's capabilities in depth
- **[Quickstart](./quickstart)**: Get running in 5 minutes
- **[Installation](../getting-started/installation)**: Detailed installation instructions
