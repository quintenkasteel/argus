# Changelog

All notable changes to Argus will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Haddock API documentation generation via `make docs`
- CONTRIBUTING.md with contribution guidelines
- Architecture documentation with diagrams
- Expanded benchmark suite with comprehensive coverage
- Learning-based fix suggestions (experimental)
- Multi-project analysis support
- Custom rule authoring capabilities

### Changed
- Improved LSP server stability with enhanced connection handling
- Enhanced plugin sandboxing with resource limits
- Optimized incremental analysis performance
- Expanded OWASP rule coverage

### Fixed
- LSP server connection handling edge cases
- Fix validation edge cases with nested expressions

## [1.0.0] - 2024-12-14

### Added

#### Core Analysis Engine
- **Syntactic Analysis**: Full GHC 9.10.3 AST-based linting
- **Semantic Analysis**: HIE file integration for type-aware analysis
- **Incremental Analysis**: Smart caching with dependency tracking
- **Parallel Analysis**: Multi-core rule execution with work stealing

#### Rule System (400+ Rules)
- **Rule DSL**: 1,500+ line embedded DSL for rule definition
- **39 Rule Categories**:
  - Safety (partial functions, unsafe operations)
  - Performance (space leaks, strictness issues)
  - Security (injection, crypto weaknesses)
  - OWASP Top 10 coverage
  - Style (naming, formatting, idioms)
  - Correctness (logic errors, type misuse)
  - Code Smells (complexity, duplication)
  - And 32 more specialized categories
- **AST Pattern Matching**: Flexible pattern language
- **Side Conditions**: Type, context, and expression predicates
- **Configurable Rules**: TOML-based custom rule definitions
- **Expression Language**: Custom expressions for complex conditions

#### Auto-Fix System
- **Safe Refactoring**: Transactional semantics with automatic rollback
- **Conflict Detection**: Overlapping and dependent fix detection
- **Topological Ordering**: Dependency-safe fix application order
- **Multi-Level Validation**:
  - Structural validation (balanced brackets)
  - Syntax validation (GHC parser)
  - Semantic validation (type checking)
  - Idempotency verification
- **Interactive Mode**: Colored diffs with user approval
- **Import Management**: Automatic import addition/removal
- **Multi-File Support**: Coordinated cross-file fixes

#### Output Formats (9 Total)
- Terminal (ANSI colored)
- JSON (structured)
- SARIF (Static Analysis Results Interchange Format)
- HTML (browsable reports)
- JUnit XML (CI integration)
- CodeClimate (quality platform)
- Checkstyle (legacy tooling)
- Plain text
- Enterprise format

#### CLI Commands (13 Total)
| Command | Description |
|---------|-------------|
| `check` | Run linting analysis |
| `fix` | Apply auto-fixes |
| `watch` | File watching mode |
| `init` | Initialize configuration |
| `index` | Build HIE index |
| `diff` | Diff-based analysis |
| `baseline` | Baseline management |
| `stats` | Analysis statistics |
| `daemon` | Background daemon mode |
| `lsp` | Language Server Protocol server |
| `architecture` | Architecture analysis |
| `pack` | Rule pack management |
| `unused` | Dead code detection |

#### IDE Integration
- LSP server with full protocol support
- Hover information with rule explanations
- Code actions for quick fixes
- Diagnostic navigation
- Completion suggestions

#### HIE Integration
- Symbol table queries
- Type information extraction
- Cross-module analysis
- Definition/reference tracking
- Incremental HIE loading

#### Advanced Features
- **Baseline Support**: Track and suppress known issues
- **Plugin System**: Extensible architecture with hot-reload
- **Watch Mode**: Real-time file monitoring with debouncing
- **Daemon Mode**: Persistent background analysis
- **GHC Plugin**: Compile-time analysis integration

### Security Features
- OWASP Top 10 rule coverage
- SQL injection detection patterns
- Command injection analysis
- XSS prevention rules
- Unsafe deserialization warnings
- Cryptographic weakness detection
- Secret/credential detection

### Performance Features
- Space leak detection
- Strictness analysis recommendations
- Lazy evaluation warnings
- Memory efficiency rules
- Algorithmic complexity hints

### Developer Experience
- Comprehensive error messages with suggestions
- Progress reporting for long analyses
- Configurable verbosity levels
- Git-aware baseline diffing
- Suppression comments support

### Testing
- 6,351 tests passing
- Property-based testing with QuickCheck
- Golden tests for detection/fix validation
- Fuzz testing for parser robustness
- Integration tests for HIE features

### Documentation
- README.md with comprehensive usage guide
- CLAUDE.md with developer instructions
- WARNINGS_POLICY.md for contribution standards
- Haddock API documentation

---

## Upgrade Guide

### Upgrading to 1.0.0

This is the initial release. No upgrade path required.

### Future Upgrades

When upgrading between versions:
1. Review the changelog for breaking changes
2. Update configuration files as noted
3. Re-run `argus init` if configuration format changed
4. Clear HIE cache with `argus index --rebuild` if HIE format changed

---

## Deprecation Policy

- Deprecated features are marked in documentation
- Deprecated features emit warnings during use
- Deprecated features are removed after 2 minor versions
- Major version bumps may remove deprecated features immediately

---

## Known Issues

See [GitHub Issues](https://github.com/quintenkasteel/argus/issues) for current known issues.

### Workarounds

| Issue | Workaround |
|-------|------------|
| Large file analysis slow | Use `--parallel` flag |
| HIE files stale | Run `argus index --rebuild` |

---

## Contributors

- Quinten Kasteel - Initial development and architecture

---

## Links

- [Documentation](https://github.com/quintenkasteel/argus#readme)
- [Issue Tracker](https://github.com/quintenkasteel/argus/issues)
- [API Reference](docs/) (via `make docs`)

---

*For detailed API changes, see the Haddock documentation generated via `make docs`.*
