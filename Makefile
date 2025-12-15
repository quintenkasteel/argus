ROOT = $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
SHELL := /bin/bash

# ==============================================================================
# Build Configuration
# ==============================================================================

# GHC RTS options for faster builds (reduces GC time by up to 80%)
GHC_RTS := +RTS -A128m -n2m -RTS

# Parallel compilation
GHC_PARALLEL := -j

# Combined build flags
BUILD_FLAGS := --ghc-options "$(GHC_PARALLEL) $(GHC_RTS)"

# ==============================================================================
# Phony Targets
# ==============================================================================

.PHONY: help dev watch build ci test test-fast clean deps metrics lint \
        check-warnings coverage coverage-report bench build-strict build-watch \
        fix-warnings warnings-report docs docs-open

.DEFAULT_GOAL := help

# ==============================================================================
# Help
# ==============================================================================

help:
	@echo "Argus - Haskell Static Analyzer"
	@echo ""
	@echo "DEVELOPMENT (daily workflow):"
	@echo "  make dev           - Fast development build (-O0, warnings shown)"
	@echo "  make watch         - Watch mode with instant feedback (ghcid)"
	@echo "  make build-watch   - Stack file-watch mode with rebuild"
	@echo ""
	@echo "BUILD:"
	@echo "  make build         - Standard build with pedantic warnings"
	@echo "  make build-strict  - Build with -Werror (warnings are errors)"
	@echo "  make ci            - CI-equivalent build (strict + tests)"
	@echo "  make deps          - Build dependencies only"
	@echo "  make clean         - Clean build artifacts"
	@echo ""
	@echo "TEST:"
	@echo "  make test          - Run all tests"
	@echo "  make test-fast     - Run tests (reuses existing build)"
	@echo "  make coverage      - Run tests with HPC coverage"
	@echo "  make coverage-report - Generate HTML coverage report"
	@echo ""
	@echo "QUALITY:"
	@echo "  make check-warnings  - Count and categorize warnings"
	@echo "  make warnings-report - Detailed warnings by file"
	@echo "  make lint            - Run argus on itself"
	@echo "  make bench           - Run benchmarks"
	@echo ""
	@echo "DOCUMENTATION:"
	@echo "  make docs            - Generate Haddock API documentation"
	@echo "  make docs-open       - Generate and open documentation in browser"
	@echo ""
	@echo "METRICS:"
	@echo "  make metrics       - Measure build times and report"
	@echo ""
	@echo "GOLDEN PATH (recommended workflow):"
	@echo "  1. make dev        - Quick iteration"
	@echo "  2. make test-fast  - Verify tests pass"
	@echo "  3. make ci         - Full CI check before commit"

# ==============================================================================
# Development Builds
# ==============================================================================

# Fast development build - no optimizations, warnings shown but not errors
dev:
	@echo "Building in development mode (fast, -O0)..."
	@stack build --fast $(BUILD_FLAGS)

# Watch mode using ghcid for instant feedback
watch:
	@command -v ghcid >/dev/null 2>&1 || (echo "Installing ghcid..." && stack install ghcid)
	@ghcid

# Watch for changes and rebuild (stack-based)
build-watch:
	@stack build --file-watch --fast $(BUILD_FLAGS)

# ==============================================================================
# Production Builds
# ==============================================================================

# Build dependencies only (useful for CI caching)
deps:
	@echo "Building dependencies..."
	@stack build --only-dependencies --fast $(BUILD_FLAGS)

# Standard build with pedantic warnings
build:
	@echo "Building with pedantic warnings..."
	@stack build --fast --pedantic $(BUILD_FLAGS)

# Strict build - warnings are errors
build-strict:
	@echo "Building with -Werror (warnings as errors)..."
	@stack build --fast --ghc-options "-Werror $(GHC_PARALLEL) $(GHC_RTS)"

# CI-equivalent build (strict + tests) - uses single build pass
ci:
	@echo "Building with -Werror (warnings as errors)..."
	@stack test --no-run-tests --fast --ghc-options "-Werror $(GHC_PARALLEL) $(GHC_RTS)"
	@echo "Running test suite..."
	@stack test --fast $(BUILD_FLAGS)
	@echo ""
	@echo "=============================================="
	@echo "CI build complete - all checks passed!"
	@echo "=============================================="

# ==============================================================================
# Testing
# ==============================================================================

# Run all tests
test:
	@echo "Running test suite..."
	@stack test --fast $(BUILD_FLAGS)

# Run tests quickly (reuses build, skips dependency check)
test-fast:
	@echo "Running tests (fast mode)..."
	@stack test --fast $(BUILD_FLAGS) --test-arguments="--hide-successes"

# Coverage requires a clean rebuild with coverage flags
coverage:
	@echo "Building with coverage enabled (requires full rebuild)..."
	@stack clean
	@stack test --coverage --ghc-options "-fhpc"
	@echo ""
	@echo "Coverage data generated. Run 'make coverage-report' for HTML."

coverage-report: coverage
	@echo "Generating HTML coverage report..."
	@stack hpc report --all --destdir=coverage-report
	@echo "Report generated in coverage-report/hpc_index.html"

# ==============================================================================
# Quality Checks
# ==============================================================================

# Count and categorize warnings
check-warnings:
	@echo "Analyzing warnings..."
	@echo ""
	@echo "=== Warning Count by Category ==="
	@stack build --fast 2>&1 | grep -oE '\[-W[a-z-]+\]' | sort | uniq -c | sort -rn || echo "0 warnings"
	@echo ""
	@TOTAL=$$(stack build --fast 2>&1 | grep -c "warning:" || echo "0"); \
	echo "=== Total: $$TOTAL warnings ==="

# Detailed warnings report by file
warnings-report:
	@echo "Generating detailed warnings report..."
	@stack build --fast 2>&1 | grep "warning:" | \
		sed 's|$(ROOT)||g' | \
		cut -d':' -f1 | sort | uniq -c | sort -rn | head -30
	@echo ""
	@echo "Top 30 files with most warnings shown above."

# Self-lint with argus
lint:
	@echo "Running argus on itself..."
	@stack run -- check src/ app/

# Run benchmarks
bench:
	@echo "Running benchmarks..."
	@stack bench

# ==============================================================================
# Metrics and Profiling
# ==============================================================================

# Measure build times
metrics:
	@echo "=== Build Metrics ==="
	@echo ""
	@echo "--- Clean Build Time ---"
	@stack clean
	@time stack build --fast $(BUILD_FLAGS) 2>&1 | tail -5
	@echo ""
	@echo "--- Incremental Build Time (touch Types.hs) ---"
	@touch src/Argus/Types.hs
	@time stack build --fast $(BUILD_FLAGS) 2>&1 | tail -5
	@echo ""
	@echo "--- Test Suite Time ---"
	@time stack test --fast $(BUILD_FLAGS) 2>&1 | tail -10
	@echo ""
	@echo "--- Warning Count ---"
	@stack build --fast 2>&1 | grep -c "warning:" || echo "0"

# ==============================================================================
# Cleanup
# ==============================================================================

clean:
	@echo "Cleaning build artifacts..."
	@stack clean
	@rm -rf .stack-work/dist/**/hpc
	@rm -rf coverage-report/
	@echo "Clean complete."

# ==============================================================================
# Utility Targets
# ==============================================================================

# Run the linter with specific arguments
run:
	@stack run -- $(ARGS)

# Generate PDF from markdown (requires md2pdf)
%.pdf: %.md
	@command -v md2pdf >/dev/null 2>&1 && md2pdf $< || echo "md2pdf not installed"

# ==============================================================================
# Documentation
# ==============================================================================

# Generate Haddock API documentation
docs:
	@echo "Generating Haddock documentation..."
	@stack haddock --no-haddock-deps $(BUILD_FLAGS)
	@echo ""
	@echo "Documentation generated."
	@echo "Location: $$(stack path --local-doc-root)/argus-1.0.0/index.html"

# Generate and open documentation in browser
docs-open: docs
	@echo "Opening documentation in browser..."
	@xdg-open "$$(stack path --local-doc-root)/argus-1.0.0/index.html" 2>/dev/null || \
	 open "$$(stack path --local-doc-root)/argus-1.0.0/index.html" 2>/dev/null || \
	 echo "Could not open browser. Documentation is at: $$(stack path --local-doc-root)/argus-1.0.0/index.html"
