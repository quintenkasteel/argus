# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.0] - 2024-12-14

### Added
- Initial release of Argus - The All-Seeing Haskell Static Analyzer
- Multi-mode analysis: quick (syntax-only), full (HIE-based), plugin (GHC integration)
- Comprehensive rule categories:
  - Naming conventions with wildcard pattern support
  - Partial function detection (50+ functions with safe alternatives)
  - Security analysis (injection, crypto, secrets, unsafe functions)
  - Performance anti-pattern detection
  - Space leak detection
  - Complexity metrics (cyclomatic, cognitive, nesting)
  - Architecture analysis (circular deps, layer violations, coupling)
  - Import hygiene
  - LANGUAGE pragma analysis
- Safe auto-fix engine with:
  - Conflict detection and resolution
  - Topological ordering of fixes
  - Transactional semantics with rollback
  - Validation pipeline
  - Interactive and preview modes
  - Multi-file coordination
- Output formats: Terminal (colored), JSON, SARIF, HTML
- TOML/YAML configuration
- LSP server for IDE integration
- GHC plugin for compile-time analysis
- Watch mode for continuous analysis
- Daemon mode for background processing
- GitHub Actions CI/CD integration
