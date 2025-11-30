# Haskell Linter Enhancement Plan

## Research Summary: Where Other Tools Fall Short

Based on deep research into the Haskell static analysis ecosystem, here are the key gaps:

### Existing Tools and Their Limitations

| Tool | Strength | Limitations |
|------|----------|-------------|
| **HLint** | AST-based suggestions | No types, no scope info, can't handle CPP |
| **Stan** | HIE-based, type-aware | Beta phase, limited inspection set (~50) |
| **Weeder** | Dead code detection | Can't handle TH splices, type families, CPP |
| **Homplexity** | Complexity metrics | Basic metrics only, outdated |
| **dead-code-detection** | Cross-module | Crashes on unknown constructs |

### Gaps in the Ecosystem

1. **No comprehensive complexity analysis** adapted for functional programming
2. **Limited security vulnerability detection** (injection, unsafe patterns)
3. **No space leak detection** at static analysis level
4. **Poor Template Haskell support** across all tools
5. **No architecture/dependency visualization**
6. **Limited auto-fix capabilities** for detected issues
7. **No incremental analysis** for large codebases

---

## Enhancement Categories

### Phase 1: Code Complexity Analysis (Priority: HIGH)

**Why**: Homplexity is outdated; no tool provides cognitive complexity for Haskell.

#### 1.1 Cyclomatic Complexity (Adapted for FP)
```
New module: src/Linter/Analysis/Complexity.hs
```

Metrics to implement:
- **Pattern match branches** - each case alternative adds complexity
- **Guard complexity** - each guard adds to complexity
- **Higher-order function depth** - nested lambdas/compositions
- **Monad transformer stacks** - deep stacks indicate complexity
- **Type class constraint count** - many constraints = complex interface

#### 1.2 Cognitive Complexity
Penalize:
- Deeply nested `do` blocks
- Complex pattern matching (nested, with guards)
- Long function chains (`.`, `>>=`, `>=>`)
- Implicit recursion patterns

#### 1.3 Function Length & Declaration Depth
- Lines per function
- Nesting depth of expressions
- Code-to-comment ratio

**Output**: Complexity score per function, module-level aggregates, threshold warnings.

---

### Phase 2: Anti-Pattern & Performance Detection (Priority: HIGH)

**Why**: Stan has ~50 inspections; we can provide more comprehensive coverage.

#### 2.1 Partial Function Detection
```
New module: src/Linter/Rules/Partial.hs
```

Detect usage of:
| Function | Risk | Safer Alternative |
|----------|------|-------------------|
| `head`, `tail`, `init`, `last` | Runtime crash on [] | Pattern match, `headMay` |
| `!!` | Index out of bounds | `atMay`, `!?` |
| `fromJust` | Crashes on Nothing | Pattern match, `fromMaybe` |
| `read` | Parse failure | `readMaybe` |
| `minimum`, `maximum` | Empty list crash | `minimumMay` |
| `foldl1`, `foldr1` | Empty list crash | `foldl'`, explicit base |
| `toEnum` | Out of range | Bounds checking |

#### 2.2 Space Leak Detection
```
New module: src/Linter/Rules/SpaceLeaks.hs
```

Detect:
- `foldl` without `'` (suggest `foldl'`)
- Lazy `State` monad usage (suggest `Strict`)
- Non-strict record fields without `StrictData`
- Lazy accumulator patterns in recursion
- Multiple references to lazy lists (Bird's mean example)
- `getContents` / lazy IO patterns

#### 2.3 Infinite List Hazards
Detect functions that hang on infinite input:
- `reverse`, `length`, `sum`, `product` on potentially infinite lists
- `isSuffixOf` without bounds

#### 2.4 Performance Anti-Patterns
- `nub` on large lists (suggest `nubOrd` or `HashSet`)
- `++` in left-associative chains (suggest difference lists)
- String instead of Text/ByteString for IO
- `Data.Map` instead of `Data.Map.Strict`
- HashMap without `Hashable` instance checks

---

### Phase 3: Security Analysis (Priority: MEDIUM-HIGH)

**Why**: No Haskell tool focuses on security vulnerabilities systematically.

#### 3.1 Unsafe Function Detection
```
New module: src/Linter/Rules/Security.hs
```

Flag:
- `unsafePerformIO`, `unsafeInterleaveIO`, `unsafeDupablePerformIO`
- `unsafeCoerce`
- FFI imports with pure types
- `inlinePerformIO`

#### 3.2 Injection Vulnerability Detection
- SQL injection: String concatenation in database queries
- Shell injection: `System.Process.shell` with user input
- Path traversal: Unchecked `</>` with user input
- XSS: HTML output without escaping

#### 3.3 Cryptography Concerns
- Weak random (`System.Random` for crypto)
- Hardcoded secrets detection
- Deprecated crypto algorithms

#### 3.4 Safe Haskell Compliance
- Report modules that could use `-XSafe` but don't
- Flag `-XTrustworthy` without justification

---

### Phase 4: Advanced Semantic Analysis (Priority: MEDIUM)

**Why**: Leverage our new HIE support for deeper analysis.

#### 4.1 Type-Aware Unused Detection
```
Enhanced: src/Linter/Analysis/Unused.hs
```

Using HIE data:
- Unused type class instances
- Unused type family instances
- Orphan instance detection
- Unused LANGUAGE pragmas (with type info)

#### 4.2 Cross-Module Analysis
- Circular import detection with visualization
- Module dependency depth warnings
- Internal module exposure detection
- Re-export chain analysis

#### 4.3 Polymorphism Complexity
- Detect "stringly typed" patterns
- Flag excessive `Any`/`Dynamic` usage
- Warn on type-level computation complexity

---

### Phase 5: Architecture Analysis (Priority: MEDIUM)

**Why**: No Haskell tool provides architecture-level insights.

#### 5.1 Module Structure Analysis
```
New module: src/Linter/Analysis/Architecture.hs
```

- Layer violation detection (configurable layers)
- Component boundary enforcement
- Import pattern analysis (qualified vs unqualified)
- Module coupling metrics

#### 5.2 Dependency Visualization
- Generate DOT graphs of module dependencies
- Highlight circular dependencies
- Show import chains

#### 5.3 Package Health
- Detect deprecated package usage
- Flag packages with known vulnerabilities
- Suggest lighter alternatives

---

### Phase 6: Template Haskell Support (Priority: MEDIUM)

**Why**: All existing tools struggle with TH.

#### 6.1 TH-Aware Analysis
```
New module: src/Linter/Analysis/TemplateHaskell.hs
```

- Parse TH splice results (when possible)
- Track TH-generated bindings
- Warn on TH that runs IO during compilation
- Support for common TH libraries (lens, aeson, etc.)

---

### Phase 7: Enhanced Auto-Fix (Priority: HIGH)

**Why**: Our format-preserving refactoring is a unique strength.

#### 7.1 Expanded Fix Coverage
Auto-fix for:
- Partial function replacement
- `foldl` -> `foldl'`
- String -> Text migrations
- Import optimization (sorting, grouping)
- Dead code removal
- Unused import removal
- LANGUAGE pragma cleanup

#### 7.2 Interactive Refactoring
- Preview mode with diff
- Batch fix application
- Undo support

---

### Phase 8: IDE Integration & Developer Experience (Priority: MEDIUM)

#### 8.1 LSP Server Mode
```
New module: src/Linter/LSP.hs
```

- Provide diagnostics via LSP
- Code actions for auto-fixes
- Hover information for lint explanations

#### 8.2 Watch Mode
- File watcher for continuous linting
- Incremental analysis (only changed files)
- Integration with ghcid

#### 8.3 CI/CD Integration
- GitHub Actions support
- GitLab CI templates
- Pre-commit hooks

---

## Implementation Priority Matrix

| Phase | Effort | Impact | Priority |
|-------|--------|--------|----------|
| 2. Anti-Patterns | Medium | Very High | 1 |
| 1. Complexity | Medium | High | 2 |
| 7. Auto-Fix | Low | High | 3 |
| 3. Security | Medium | High | 4 |
| 4. Semantic | High | Medium | 5 |
| 5. Architecture | Medium | Medium | 6 |
| 8. IDE/DX | High | High | 7 |
| 6. Template Haskell | High | Medium | 8 |

---

## Proposed File Structure

```
src/Linter/
  Analysis/
    Complexity.hs      # NEW: Complexity metrics
    Architecture.hs    # NEW: Module structure analysis
    TemplateHaskell.hs # NEW: TH support
    Semantic.hs        # ENHANCED: HIE-based analysis
    Unused.hs          # ENHANCED: Type-aware unused
    DepGraph.hs        # EXISTING
    Syntactic.hs       # EXISTING
  Rules/
    Partial.hs         # NEW: Partial function detection
    SpaceLeaks.hs      # NEW: Space leak patterns
    Security.hs        # NEW: Security vulnerabilities
    Performance.hs     # NEW: Performance anti-patterns
    Types.hs           # EXISTING
    Naming.hs          # EXISTING
    Patterns.hs        # EXISTING
    Imports.hs         # EXISTING
    Parser.hs          # EXISTING
  Output/
    Dot.hs             # NEW: GraphViz output
    ...existing...
  LSP/
    Server.hs          # NEW: LSP support
    Handlers.hs        # NEW: LSP handlers
```

---

## Unique Value Propositions

What makes our linter different:

1. **Comprehensive** - More inspections than Stan + HLint combined
2. **Format-Preserving Fixes** - Using ghc-exactprint
3. **HIE + AST Hybrid** - Best of both worlds
4. **Complexity Metrics** - First proper FP complexity analysis
5. **Security Focus** - Unique in Haskell ecosystem
6. **Architecture Insights** - Enterprise-grade analysis
7. **TH Support** - Addresses major ecosystem gap

---

## Success Metrics

- [ ] 100+ unique inspections
- [ ] Complexity scores for all functions
- [ ] Auto-fix for 50%+ of warnings
- [ ] Zero false positives for partial function detection
- [ ] < 1 second analysis for 10k LOC projects
- [ ] LSP integration working with major editors

---

## Sources

- [Stan - Haskell STatic ANalyser](https://github.com/kowainik/stan)
- [HLint](https://hackage.haskell.org/package/hlint)
- [Weeder](https://hackage.haskell.org/package/weeder)
- [Homplexity](https://hackage.haskell.org/package/homplexity)
- [Cognitive Complexity - SonarSource](https://www.sonarsource.com/docs/CognitiveComplexity.pdf)
- [Space Leak Avoidance](https://chshersh.com/blog/2022-08-08-space-leak.html)
- [Safe Haskell](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/safe_haskell.html)
- [Liquid Haskell](https://ucsd-progsys.github.io/liquidhaskell/)
- [Stan All Inspections](https://github.com/kowainik/stan/wiki/All-Inspections)
