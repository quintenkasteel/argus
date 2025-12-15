#!/usr/bin/env bash
# Argus Haskell Linter - Generic CI Script
#
# This script can be used with any CI system (Jenkins, CircleCI, Travis, etc.)
# It runs Argus static analysis and generates reports.
#
# Usage:
#   ./generic-ci.sh [paths...]
#
# Environment Variables:
#   ARGUS_BIN       - Path to argus binary (default: argus)
#   OUTPUT_FORMAT   - Report format: terminal, json, sarif, codeclimate, junit (default: terminal)
#   FAIL_ON_ERROR   - Exit with error on lint failures: true/false (default: true)
#   REPORT_DIR      - Directory for reports (default: ./argus-reports)
#   BASELINE_FILE   - Baseline file for comparison (optional)
#
# Examples:
#   # Basic usage
#   ./generic-ci.sh src/ app/
#
#   # With custom options
#   FAIL_ON_ERROR=false OUTPUT_FORMAT=sarif ./generic-ci.sh src/
#
#   # Generate all report formats
#   OUTPUT_FORMAT=all ./generic-ci.sh src/ app/

set -euo pipefail

# Configuration with defaults
ARGUS_BIN="${ARGUS_BIN:-argus}"
OUTPUT_FORMAT="${OUTPUT_FORMAT:-terminal}"
FAIL_ON_ERROR="${FAIL_ON_ERROR:-true}"
REPORT_DIR="${REPORT_DIR:-./argus-reports}"
BASELINE_FILE="${BASELINE_FILE:-}"

# Colors for terminal output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Paths to analyze (default: src/ app/)
LINT_PATHS="${*:-src/ app/}"

# Print colored message
log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
log_warning() { echo -e "${YELLOW}[WARNING]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Check if argus is installed
check_argus() {
    if ! command -v "$ARGUS_BIN" &> /dev/null; then
        log_error "Argus not found at: $ARGUS_BIN"
        log_info "Install Argus from: https://github.com/quintenkasteel/argus"
        log_info "Or build from source:"
        log_info "  git clone https://github.com/quintenkasteel/argus.git"
        log_info "  cd argus && stack build --copy-bins"
        exit 1
    fi
    log_info "Using Argus: $($ARGUS_BIN --version 2>&1 || echo 'version unknown')"
}

# Create report directory
setup_reports() {
    mkdir -p "$REPORT_DIR"
    log_info "Reports will be saved to: $REPORT_DIR"
}

# Initialize Argus config if needed
init_config() {
    if [ ! -f argus.toml ] && [ ! -f linter.toml ]; then
        log_info "Initializing Argus configuration..."
        "$ARGUS_BIN" init --quiet 2>/dev/null || true
    fi
}

# Run main analysis
run_analysis() {
    local exit_code=0
    local report_file="$REPORT_DIR/argus-results.txt"

    log_info "Running Argus analysis on: $LINT_PATHS"

    set +e
    "$ARGUS_BIN" check $LINT_PATHS --format terminal --color always 2>&1 | tee "$report_file"
    exit_code=${PIPESTATUS[0]}
    set -e

    # Count issues
    local errors=$(grep -c "error:" "$report_file" 2>/dev/null || echo "0")
    local warnings=$(grep -c "warning:" "$report_file" 2>/dev/null || echo "0")

    echo ""
    log_info "Analysis Summary:"
    echo "  Errors:   $errors"
    echo "  Warnings: $warnings"
    echo ""

    # Export for CI systems
    echo "ARGUS_ERRORS=$errors" > "$REPORT_DIR/argus.env"
    echo "ARGUS_WARNINGS=$warnings" >> "$REPORT_DIR/argus.env"
    echo "ARGUS_EXIT_CODE=$exit_code" >> "$REPORT_DIR/argus.env"

    return $exit_code
}

# Generate additional report formats
generate_reports() {
    log_info "Generating reports..."

    case "$OUTPUT_FORMAT" in
        all)
            log_info "Generating all report formats..."
            "$ARGUS_BIN" check $LINT_PATHS --format sarif > "$REPORT_DIR/argus-results.sarif" 2>&1 || true
            "$ARGUS_BIN" check $LINT_PATHS --format json > "$REPORT_DIR/argus-results.json" 2>&1 || true
            "$ARGUS_BIN" check $LINT_PATHS --format codeclimate > "$REPORT_DIR/codeclimate.json" 2>&1 || true
            "$ARGUS_BIN" check $LINT_PATHS --format junit > "$REPORT_DIR/junit-report.xml" 2>&1 || true
            "$ARGUS_BIN" check $LINT_PATHS --format checkstyle > "$REPORT_DIR/checkstyle.xml" 2>&1 || true
            "$ARGUS_BIN" check $LINT_PATHS --format html > "$REPORT_DIR/argus-report.html" 2>&1 || true
            ;;
        sarif)
            "$ARGUS_BIN" check $LINT_PATHS --format sarif > "$REPORT_DIR/argus-results.sarif" 2>&1 || true
            ;;
        json)
            "$ARGUS_BIN" check $LINT_PATHS --format json > "$REPORT_DIR/argus-results.json" 2>&1 || true
            ;;
        codeclimate)
            "$ARGUS_BIN" check $LINT_PATHS --format codeclimate > "$REPORT_DIR/codeclimate.json" 2>&1 || true
            ;;
        junit)
            "$ARGUS_BIN" check $LINT_PATHS --format junit > "$REPORT_DIR/junit-report.xml" 2>&1 || true
            ;;
        checkstyle)
            "$ARGUS_BIN" check $LINT_PATHS --format checkstyle > "$REPORT_DIR/checkstyle.xml" 2>&1 || true
            ;;
        html)
            "$ARGUS_BIN" check $LINT_PATHS --format html > "$REPORT_DIR/argus-report.html" 2>&1 || true
            ;;
        terminal|plain)
            # Already generated in run_analysis
            ;;
        *)
            log_warning "Unknown format: $OUTPUT_FORMAT, using terminal"
            ;;
    esac
}

# Compare against baseline if provided
compare_baseline() {
    if [ -n "$BASELINE_FILE" ] && [ -f "$BASELINE_FILE" ]; then
        log_info "Comparing against baseline: $BASELINE_FILE"
        "$ARGUS_BIN" diff $LINT_PATHS --baseline "$BASELINE_FILE" > "$REPORT_DIR/baseline-diff.txt" 2>&1 || true

        local new_issues=$(grep -c "new:" "$REPORT_DIR/baseline-diff.txt" 2>/dev/null || echo "0")
        local fixed_issues=$(grep -c "fixed:" "$REPORT_DIR/baseline-diff.txt" 2>/dev/null || echo "0")

        echo ""
        log_info "Baseline Comparison:"
        echo "  New issues:   $new_issues"
        echo "  Fixed issues: $fixed_issues"
        echo ""

        if [ "$new_issues" -gt 0 ]; then
            log_warning "Found $new_issues new issue(s) compared to baseline"
            return 1
        fi
    fi
    return 0
}

# Generate fix suggestions
generate_fix_suggestions() {
    log_info "Generating auto-fix suggestions..."
    "$ARGUS_BIN" fix $LINT_PATHS --dry-run --format diff > "$REPORT_DIR/fix-suggestions.diff" 2>&1 || true

    if [ -s "$REPORT_DIR/fix-suggestions.diff" ]; then
        log_info "Auto-fix suggestions available in: $REPORT_DIR/fix-suggestions.diff"
        log_info "Apply fixes with: argus fix $LINT_PATHS"
    else
        log_info "No auto-fixes available"
    fi
}

# Print summary
print_summary() {
    local exit_code=$1

    echo ""
    echo "=========================================="
    echo "           Argus Analysis Summary         "
    echo "=========================================="
    echo ""

    if [ -f "$REPORT_DIR/argus.env" ]; then
        source "$REPORT_DIR/argus.env"
        echo "Errors:   ${ARGUS_ERRORS:-0}"
        echo "Warnings: ${ARGUS_WARNINGS:-0}"
        echo ""
    fi

    echo "Reports generated in: $REPORT_DIR/"
    ls -la "$REPORT_DIR/" 2>/dev/null || true
    echo ""

    if [ "$exit_code" -eq 0 ]; then
        log_success "All checks passed!"
    else
        if [ "$FAIL_ON_ERROR" = "true" ]; then
            log_error "Analysis found issues. Build will fail."
        else
            log_warning "Analysis found issues. Continuing (FAIL_ON_ERROR=false)."
        fi
    fi
    echo ""
}

# Main execution
main() {
    echo ""
    echo "=========================================="
    echo "       Argus Haskell Static Analyzer      "
    echo "=========================================="
    echo ""

    check_argus
    setup_reports
    init_config

    local analysis_exit_code=0
    run_analysis || analysis_exit_code=$?

    generate_reports
    compare_baseline || analysis_exit_code=1
    generate_fix_suggestions

    print_summary $analysis_exit_code

    # Determine final exit code
    if [ "$FAIL_ON_ERROR" = "true" ] && [ "$analysis_exit_code" -ne 0 ]; then
        exit 1
    fi

    exit 0
}

# Run main function
main "$@"
