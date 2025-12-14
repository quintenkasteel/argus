# Argus Build Performance Plan

This document outlines the strategy for improving compilation speed and enforcing strict warning hygiene in the Argus codebase.

## Table of Contents

1. [Research Summary](#research-summary)
2. [Current State Analysis](#current-state-analysis)
3. [Baseline Measurements](#baseline-measurements)
4. [Bottleneck Analysis](#bottleneck-analysis)
5. [Implementation Plan](#implementation-plan)
6. [Success Criteria](#success-criteria)

---

## Research Summary

### Sources Consulted

- [Well-Typed: Reducing Haskell Parallel Build Times Using Semaphores](https://www.well-typed.com/blog/2023/08/reducing-haskell-parallel-build-times/)
- [HaskellWiki: Performance/GHC](https://wiki.haskell.org/Performance/GHC)
- [GHC User's Guide: Optimisation](https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html)
- [Matt Parsons: Keeping Compilation Fast](https://www.parsonsmatt.org/2019/11/27/keeping_compilation_fast.html)
- [Matt Parsons: Template Haskell Performance Tips](https://www.parsonsmatt.org/2021/07/12/template_haskell_performance_tips.html)
- [rybczak.net: How to Reduce Compilation Times](https://rybczak.net/2016/03/26/how-to-reduce-compilation-times-of-haskell-projects/)
- [ndmitchell/ghcid](https://github.com/ndmitchell/ghcid)
- [Mercury: Announcing ghciwatch](https://mercury.com/blog/announcing-ghciwatch)
- [freckle/stack-action](https://github.com/freckle/stack-action)
- [freckle/stack-cache-action](https://github.com/freckle/stack-cache-action)
- [GitHub Actions Caching for Stack](https://raehik.github.io/2021/03/01/caching-stack-and-cabal-haskell-builds-on-github-actions.html)
- [Stack Documentation: Cabal Flags and GHC Options](https://docs.haskellstack.org/en/stable/tutorial/cabal_flags_and_ghc_options/)
- [functor.tokyo: GHC Warnings You Should Enable](https://functor.tokyo/blog/2017-07-28-ghc-warnings-you-should-enable)

### Key Findings

#### 1. Parallel Build Optimization (GHC 9.8+)

GHC 9.8+ with Cabal 3.12+ supports build semaphores for better parallel coordination. Well-Typed observed **22% wall-clock speedup** compiling lens using `-j --semaphore`.

**Recommendation**: Use `-j` flag with appropriate memory settings.

#### 2. GC Tuning for Faster Builds

Modifying GC allocation area has huge impact on parallel builds:
- `-A128m` (128MB allocation area)
- `-n2m` (2MB nursery)
- Can reduce GC time by up to **80%**

**Recommendation**: Add `+RTS -A128m -n2m -RTS` to build commands.

#### 3. Optimization Levels

- `-O0`: Fastest compilation, no optimization (development)
- `-O1`: Default, moderate optimization
- `-O2`: Full optimization, slowest compilation (release)

**Recommendation**: Use `-O0` (via `--fast`) for development, `-O2` for releases.

#### 4. Module Size and Structure

- GHC compilation is **superlinear** with module size
- Sweet spot: 50-200 lines per module
- Broader, shallower module graphs compile faster in parallel
- Template Haskell defeats incremental compilation

**Recommendation**: Keep modules small, isolate TH to dedicated modules.

#### 5. CI Caching Best Practices

- Cache `~/.stack` and `.stack-work`
- Use content-based cache keys (`hashFiles('stack.yaml.lock')`)
- Reset file timestamps after checkout (prevents unnecessary rebuilds)
- GitHub cache limit: 5GB per repo, unused caches deleted after 7 days

**Recommendation**: Use `freckle/stack-action` or implement proper caching.

#### 6. Development Tooling

- **ghcid**: "GHCi as a daemon" - instant feedback on save
- **ghciwatch**: Mercury's faster alternative to ghcid
- Both use `-fno-code` for ~2x faster reload

**Recommendation**: Add `.ghcid` configuration file for developers.

---

## Current State Analysis

### Codebase Metrics

| Metric | Value |
|--------|-------|
| Source modules (src/) | 201 |
| Test modules (test/) | 221 |
| Total Haskell files | 422+ |
| GHC Version | 9.10.3 |
| Stack Resolver | LTS 24.21 |
| Current Warnings | ~684 |

### Current Build Configuration

**package.yaml warnings**:
```yaml
ghc-options:
  - -Wall
  - -Wcompat
  - -Widentities
  - -Wincomplete-record-updates
  - -Wincomplete-uni-patterns
  - -Wmissing-deriving-strategies
  - -Wpartial-fields
  - -Wredundant-constraints
  - -Wunused-packages
  - -fwrite-ide-info
  - -hiedir=.hie
```

**Issues Identified**:
1. No `-Werror` enforcement
2. ~684 warnings currently in codebase
3. No parallel build flags in default config
4. No GC tuning for builds
5. CI caching could be more efficient
6. No `.ghcid` file for rapid development

---

## Baseline Measurements

### How to Measure Build Times

#### Clean Build (Full Rebuild)

```bash
# Measure clean build time
stack clean
time stack build --fast 2>&1 | tee build-clean.log

# Count warnings
grep -c "warning:" build-clean.log
```

#### Incremental Build (Single Module Change)

```bash
# Touch a core module and measure rebuild
touch src/Argus/Types.hs
time stack build --fast 2>&1 | tee build-incremental.log
```

#### Dependency Build

```bash
# Measure dependency resolution time
stack clean
time stack build --only-dependencies --fast
```

### Baseline Results Storage

Store baseline results in `build-metrics/`:

```
build-metrics/
├── baseline-clean.txt        # Clean build time
├── baseline-incremental.txt  # Incremental build time
├── baseline-deps.txt         # Dependency build time
├── warning-count.txt         # Number of warnings
└── ci-build-time.txt         # CI workflow duration
```

### Current Estimates (Pre-Optimization)

| Build Type | Estimated Time |
|------------|----------------|
| Clean build (no deps) | ~5-10 min |
| Incremental build | ~30-60 sec |
| Full test suite | ~3-5 min |
| CI total | ~15-20 min |

---

## Bottleneck Analysis

### Suspected Bottlenecks

#### 1. Large Modules

**Hypothesis**: Some modules may exceed 500+ lines, causing superlinear compilation.

**Verification**:
```bash
# Find largest modules
find src -name "*.hs" -exec wc -l {} + | sort -rn | head -20
```

#### 2. Template Haskell Usage

**Hypothesis**: TH usage causes forced recompilation.

**Verification**:
```bash
# Find TH usage
grep -r "TemplateHaskell\|Language.Haskell.TH\|\$(" src/ --include="*.hs" | wc -l
```

#### 3. Orphan Instances

**Hypothesis**: Orphan instances cause unnecessary recompilation.

**Verification**:
```bash
# Check for orphan warnings during build
stack build --fast 2>&1 | grep -i "orphan"
```

#### 4. Deep Module Dependencies

**Hypothesis**: Changes to core modules cascade through many dependents.

**Verification**:
```bash
# Analyze module dependencies (requires graphmod)
stack exec -- graphmod src/**/*.hs | head -100
```

#### 5. Excessive INLINE Pragmas

**Hypothesis**: Over-inlining causes code bloat and slow compilation.

**Verification**:
```bash
# Count INLINE pragmas
grep -r "INLINE" src/ --include="*.hs" | wc -l
```

#### 6. Warning Noise

**Hypothesis**: 684+ warnings indicate code quality issues causing unnecessary work.

**Verification**: Already confirmed - requires systematic fixing.

---

## Implementation Plan

### Phase 1: Immediate Wins (Low Risk)

**Goal**: Quick wins with minimal code changes.

**Changes**:

1. Add optimized GHC flags to Makefile
2. Create `.ghcid` configuration
3. Add fast development targets
4. Improve CI caching strategy

**Expected Impact**: 20-30% faster development builds

**Risk**: Low - configuration changes only

**Verification**: Compare build times before/after

**Checklist**:
- [ ] Update Makefile with optimized targets
- [ ] Create `.ghcid` file
- [ ] Add `make dev` target for fast iteration
- [ ] Update CI workflow caching

### Phase 2: Warning Cleanup (Medium Risk)

**Goal**: Achieve zero-warning builds with `-Werror`.

**Changes**:

1. Fix all existing warnings (~684)
2. Enable `-Werror` in package.yaml (with flag)
3. Add CI enforcement
4. Update developer documentation

**Expected Impact**: Cleaner codebase, faster incremental builds (less noise)

**Risk**: Medium - code changes required, potential for regressions

**Verification**: `stack build --pedantic` succeeds with no warnings

**Checklist**:
- [ ] Categorize and prioritize warnings
- [ ] Fix unused import warnings
- [ ] Fix unused variable warnings
- [ ] Fix name shadowing warnings
- [ ] Fix redundant constraint warnings
- [ ] Fix incomplete pattern warnings
- [ ] Enable `-Werror` via flag
- [ ] Add CI enforcement

### Phase 3: Build Configuration Optimization (Low Risk)

**Goal**: Optimize build parallelism and memory usage.

**Changes**:

1. Add parallel build flags (`-j`)
2. Add GC tuning flags (`+RTS -A128m -n2m -RTS`)
3. Configure split sections for smaller binaries
4. Add build profiles (dev, ci, release)

**Expected Impact**: 30-50% faster parallel builds

**Risk**: Low - configuration changes only

**Verification**: Compare parallel vs sequential build times

**Checklist**:
- [ ] Add `-j` to ghc-options
- [ ] Add GC tuning to RTS options
- [ ] Create build profiles in Makefile
- [ ] Document build commands

### Phase 4: Structural Improvements (Higher Risk)

**Goal**: Improve module structure for better parallelism.

**Changes**:

1. Identify and split large modules (>500 lines)
2. Isolate Template Haskell to dedicated modules
3. Reduce circular dependencies
4. Optimize export lists

**Expected Impact**: 20-40% faster incremental builds

**Risk**: Higher - structural refactoring

**Verification**: Module dependency graph analysis

**Checklist**:
- [ ] Audit modules over 500 lines
- [ ] Create TH isolation plan
- [ ] Identify circular dependencies
- [ ] Refactor as needed

### Phase 5: CI Pipeline Optimization (Medium Risk)

**Goal**: Faster, more efficient CI builds.

**Changes**:

1. Use `freckle/stack-action` for better caching
2. Add file timestamp restoration
3. Parallelize independent jobs
4. Add build time tracking
5. Implement incremental testing

**Expected Impact**: 40-60% faster CI builds

**Risk**: Medium - workflow changes

**Verification**: CI build time metrics

**Checklist**:
- [ ] Migrate to freckle/stack-action
- [ ] Add timestamp restoration step
- [ ] Optimize job dependencies
- [ ] Add build time reporting
- [ ] Implement test result caching

---

## Success Criteria

### Build Performance Targets

| Metric | Current | Target | Stretch |
|--------|---------|--------|---------|
| Clean build (no deps) | ~10 min | <5 min | <3 min |
| Incremental build | ~60 sec | <20 sec | <10 sec |
| Full test suite | ~5 min | <3 min | <2 min |
| CI total time | ~20 min | <10 min | <7 min |
| Warning count | 684 | 0 | 0 |

### Quality Gates

1. **Zero Warnings**: `stack build --pedantic` must pass
2. **CI Enforcement**: All PRs must pass warning checks
3. **Build Time Regression**: Alert if build time increases >20%
4. **Test Coverage**: Maintain >80% coverage

### Monitoring

1. Track build times in CI artifacts
2. Report warning counts in PR checks
3. Alert on build time regressions
4. Weekly build performance reports

---

## Quick Reference

### Development Commands

```bash
# Fast development build (no optimizations)
make dev

# Watch mode with instant feedback
make watch

# Full build with all warnings
make build

# Run tests quickly
make test-fast

# Full CI-equivalent build
make ci
```

### Build Profiles

| Profile | Optimizations | Warnings | Use Case |
|---------|---------------|----------|----------|
| `dev` | -O0 | Enabled | Daily development |
| `test` | -O0 | Enabled | Running tests |
| `ci` | -O0 | -Werror | CI builds |
| `release` | -O2 | -Werror | Production builds |

---

## Appendix: Warning Categories

### Must Fix (Errors in CI)

- `-Wunused-imports`: Unused imports
- `-Wunused-matches`: Unused variables
- `-Wunused-top-binds`: Unused top-level bindings
- `-Wincomplete-patterns`: Non-exhaustive patterns
- `-Wincomplete-uni-patterns`: Incomplete unification patterns
- `-Wmissing-deriving-strategies`: Missing deriving strategies

### Should Fix (Warnings)

- `-Wname-shadowing`: Variable shadowing
- `-Wredundant-constraints`: Unnecessary constraints
- `-Wpartial-fields`: Partial record field accessors
- `-Wunused-packages`: Unused package dependencies

### Advisory (Info)

- `-Wcompat`: Future compatibility
- `-Widentities`: Unnecessary identity operations

---

*Document Version: 1.0*
*Last Updated: 2025-12-14*
