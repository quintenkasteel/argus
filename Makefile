ROOT = $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
SHELL := /bin/bash

# GHC RTS options for faster builds
GHC_RTS := +RTS -A128m -n2m -RTS
GHC_PARALLEL := -j

.PHONY: help dev build build-strict build-watch ci watch run test test-fast coverage coverage-report clean bench lint check-warnings

default: help

help:
	@echo "Argus - Haskell Static Analyzer"
	@echo ""
	@echo "Development targets:"
	@echo "  dev           - Fast development build (no optimizations, warnings shown)"
	@echo "  watch         - Watch mode with instant feedback (ghcid)"
	@echo "  build-watch   - Build and watch for changes (stack)"
	@echo ""
	@echo "Build targets:"
	@echo "  build         - Standard build with pedantic warnings"
	@echo "  build-strict  - Build with -Werror (warnings are errors)"
	@echo "  ci            - CI-equivalent build (strict + tests)"
	@echo "  clean         - Clean build artifacts"
	@echo ""
	@echo "Test targets:"
	@echo "  test          - Run all tests"
	@echo "  test-fast     - Run tests without full rebuild"
	@echo "  coverage      - Run tests with HPC coverage (requires rebuild)"
	@echo "  coverage-report - Generate HTML coverage report"
	@echo ""
	@echo "Quality targets:"
	@echo "  check-warnings - Count warnings without building"
	@echo "  lint          - Run argus on itself"
	@echo "  bench         - Run benchmarks"

# ============================================================================
# Development Builds
# ============================================================================

# Fast development build - no optimizations, warnings shown but not errors
dev:
	@stack build --fast --ghc-options "$(GHC_PARALLEL) $(GHC_RTS)"

# Watch mode using ghcid for instant feedback
watch:
	@command -v ghcid >/dev/null 2>&1 || (echo "Installing ghcid..." && stack install ghcid)
	@ghcid --command="stack ghci --ghci-options='-fno-code -fwrite-interface'"

# ============================================================================
# Production Builds
# ============================================================================

# Standard build with pedantic warnings
build:
	@stack build --fast --pedantic --ghc-options "$(GHC_PARALLEL) $(GHC_RTS)" --copy-bins

# Strict build - warnings are errors
build-strict:
	@stack build --fast --ghc-options "-Werror $(GHC_PARALLEL) $(GHC_RTS)"

# CI-equivalent build (strict + tests)
ci: build-strict test
	@echo "CI build complete - all checks passed"

# Watch for changes and rebuild (stack-based)
build-watch:
	@stack install --file-watch --fast --pedantic --ghc-options "$(GHC_PARALLEL) $(GHC_RTS)"

clean:
	@stack clean
	@rm -rf .stack-work/dist/**/hpc

# Testing
test:
	@stack test --fast

test-fast:
	@stack test --fast --no-run-tests 2>/dev/null || true
	@stack test --fast

# Coverage requires a clean rebuild with coverage flags
coverage:
	@echo "Building with coverage enabled (this requires a full rebuild)..."
	@stack clean
	@stack test --coverage --ghc-options "-fhpc"
	@echo ""
	@echo "Coverage data generated. Run 'make coverage-report' for HTML report."

coverage-report: coverage
	@echo "Generating HTML coverage report..."
	@stack hpc report --all --destdir=coverage-report
	@echo "Coverage report generated in coverage-report/"
	@echo "Open coverage-report/hpc_index.html to view"

# Benchmarks
bench:
	@stack bench

# Self-lint
lint:
	@stack run -- check src/

# ============================================================================
# Quality Checks
# ============================================================================

# Count warnings without full build
check-warnings:
	@echo "Counting warnings in codebase..."
	@stack build --fast 2>&1 | grep -c "warning:" || echo "0"
	@echo ""
	@echo "Top warnings:"
	@stack build --fast 2>&1 | grep "warning:" | sort | uniq -c | sort -rn | head -20

run:
	@code-conventions data --in-place --improve

%.pdf: %.md
	fish -c 'nvm use 19 && md2pdf $<' && open $@
