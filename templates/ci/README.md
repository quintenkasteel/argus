# Argus CI/CD Integration Templates

This directory contains ready-to-use CI/CD integration templates for running Argus static analysis in your projects.

## Available Templates

| File | Platform | Description |
|------|----------|-------------|
| `github-actions.yml` | GitHub Actions | Full-featured workflow with SARIF, PR comments, auto-fix |
| `gitlab-ci.yml` | GitLab CI | Pipeline with CodeClimate, JUnit, baseline comparison |
| `generic-ci.sh` | Any CI System | Universal shell script for Jenkins, CircleCI, Travis, etc. |

## Quick Start

### GitHub Actions

1. Copy `github-actions.yml` to your project:
   ```bash
   mkdir -p .github/workflows
   cp github-actions.yml .github/workflows/argus.yml
   ```

2. Push to trigger the workflow:
   ```bash
   git add .github/workflows/argus.yml
   git commit -m "Add Argus linting"
   git push
   ```

3. View results in the Actions tab and Code Scanning alerts.

### GitLab CI

1. Copy `gitlab-ci.yml` to your project root:
   ```bash
   cp gitlab-ci.yml .gitlab-ci.yml
   # Or include it in your existing .gitlab-ci.yml
   ```

2. Push to trigger the pipeline:
   ```bash
   git add .gitlab-ci.yml
   git commit -m "Add Argus linting"
   git push
   ```

3. View results in the pipeline and Code Quality reports.

### Generic CI (Jenkins, CircleCI, Travis, etc.)

1. Copy the script to your project:
   ```bash
   cp generic-ci.sh scripts/argus-lint.sh
   chmod +x scripts/argus-lint.sh
   ```

2. Call from your CI configuration:
   ```yaml
   # Jenkins (Jenkinsfile)
   stage('Lint') {
     steps {
       sh './scripts/argus-lint.sh src/ app/'
     }
   }

   # CircleCI (.circleci/config.yml)
   - run:
       name: Argus Lint
       command: ./scripts/argus-lint.sh src/ app/

   # Travis CI (.travis.yml)
   script:
     - ./scripts/argus-lint.sh src/ app/
   ```

## Configuration Options

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ARGUS_BIN` | `argus` | Path to Argus binary |
| `LINT_PATHS` | `src/ app/` | Paths to analyze |
| `OUTPUT_FORMAT` | `terminal` | Report format: terminal, json, sarif, codeclimate, junit, html, all |
| `FAIL_ON_ERROR` | `true` | Exit with error code on lint failures |
| `REPORT_DIR` | `./argus-reports` | Directory for generated reports |
| `BASELINE_FILE` | (none) | Baseline file for comparison |

### Report Formats

| Format | Description | Use Case |
|--------|-------------|----------|
| `terminal` | Colored console output | Local development, logs |
| `json` | JSON diagnostic array | Custom processing |
| `sarif` | SARIF 2.1.0 | GitHub Code Scanning, VS Code |
| `codeclimate` | CodeClimate JSON | GitLab Code Quality |
| `junit` | JUnit XML | Test result aggregation |
| `checkstyle` | Checkstyle XML | Legacy tools integration |
| `html` | HTML report | Human-readable reports |
| `all` | All formats | Full CI pipeline |

## Features

### GitHub Actions Features
- **SARIF Integration**: Uploads results to GitHub Code Scanning
- **PR Comments**: Posts analysis summary on pull requests
- **Auto-fix Suggestions**: Shows suggested fixes as diff
- **Caching**: Caches Stack dependencies for faster builds
- **Matrix Build**: Supports multiple OS testing

### GitLab CI Features
- **Code Quality**: Generates CodeClimate reports for MR diffs
- **SAST Integration**: SARIF reports for security scanning
- **JUnit Reports**: Test-style reporting in pipelines
- **Baseline Comparison**: Compare against baseline files
- **Statistics Tracking**: Historical lint statistics

### Generic Script Features
- **Multiple Formats**: Generate all report types at once
- **Baseline Support**: Compare against previous baselines
- **Fix Suggestions**: Generate auto-fix diffs
- **Colored Output**: Terminal colors for readability
- **Exit Codes**: Proper exit codes for CI integration

## Baseline Support

Create a baseline to track new issues without failing on existing ones:

```bash
# Generate baseline
argus baseline src/ app/ > .argus-baseline.json

# Commit baseline
git add .argus-baseline.json
git commit -m "Add Argus baseline"

# CI will now only fail on NEW issues
```

## Auto-Fix Integration

Enable auto-fix suggestions in PRs:

```bash
# Generate fix suggestions (dry run)
argus fix src/ app/ --dry-run --format diff

# Apply fixes
argus fix src/ app/

# Review and commit
git diff
git add -A
git commit -m "Apply Argus auto-fixes"
```

## Customization

### Custom Rule Configuration

Create `argus.toml` in your project root:

```toml
[rules]
# Disable specific rules
disabled = ["style/trailing-whitespace", "naming/camelCase"]

# Set severity overrides
[rules.severity]
"partial/head" = "error"
"performance/lazy-accumulator" = "warning"

[paths]
# Exclude directories
exclude = ["test/", "generated/"]
```

### Adding Custom Rules

Create `.argus/rules/` directory for project-specific rules:

```toml
# .argus/rules/project-rules.toml
[[rules]]
id = "project/no-debug"
pattern = "Debug.trace"
message = "Remove debug traces before commit"
severity = "error"
```

## Troubleshooting

### Argus Not Found

```bash
# Option 1: Build from source
git clone https://github.com/quintenkasteel/argus.git
cd argus && stack build --copy-bins

# Option 2: Add to PATH
export PATH="$HOME/.local/bin:$PATH"
```

### Cache Issues

```bash
# Clear Stack cache
rm -rf ~/.stack .stack-work

# Rebuild
stack build
```

### Report Validation Errors

```bash
# Validate SARIF
python3 -m json.tool argus-results.sarif

# Validate CodeClimate
python3 -c "import json; json.load(open('codeclimate.json'))"
```

## Support

- Documentation: https://github.com/quintenkasteel/argus
- Issues: https://github.com/quintenkasteel/argus/issues
