---
sidebar_position: 10
title: Output Formats
description: All supported output formats for Argus results
---

# Output Formats

Argus supports multiple output formats for different use cases: human-readable terminal output, machine-readable JSON, CI integration formats, and more.

## Format Selection

```bash
argus check -f <format> [files]
argus check --format <format> [files]
```

## Available Formats

| Format | Use Case |
|--------|----------|
| `terminal` | Human-readable (default) |
| `json` | Machine processing |
| `sarif` | Security tools, GitHub |
| `html` | Reports, sharing |
| `junit` | CI test reporters |
| `codeclimate` | CodeClimate, GitLab |
| `checkstyle` | Jenkins, legacy tools |
| `github` | GitHub Actions annotations |

## Terminal Format

Default human-readable output with colors and context.

```bash
argus check src/
```

Output:

```
src/Handler.hs:45:12: warning [partial/head]
  Use headMay instead of head
  |
45|   let first = head items
  |               ^^^^
  = suggestion: Replace with `headMay items`
  = note: head throws exception on empty list

src/Api.hs:23:5: error [security/sql-injection]
  Potential SQL injection vulnerability
  |
23|   rawSql query []
  |   ^^^^^^^^^^^^^^
  = note: Use parameterized queries

─────────────────────────────────
Found 2 issues (1 error, 1 warning)
Analyzed 15 files in 0.82s
```

### Terminal Options

```bash
# No colors
argus check --no-color src/

# Compact output (one line per issue)
argus check --compact src/

# Show context lines
argus check --context 3 src/

# Group by file
argus check --group-by file src/

# Group by rule
argus check --group-by rule src/
```

### Compact Format

```bash
argus check --compact src/
```

```
src/Handler.hs:45:12: warning [partial/head] Use headMay instead of head
src/Api.hs:23:5: error [security/sql-injection] Potential SQL injection
```

## JSON Format

Machine-readable output for tooling and scripting.

```bash
argus check -f json src/
```

```json
{
  "version": "1.0",
  "tool": {
    "name": "argus",
    "version": "0.2.0"
  },
  "analysis": {
    "mode": "full",
    "files": 15,
    "duration_ms": 820
  },
  "summary": {
    "total": 2,
    "errors": 1,
    "warnings": 1,
    "suggestions": 0,
    "info": 0
  },
  "diagnostics": [
    {
      "file": "src/Handler.hs",
      "line": 45,
      "column": 12,
      "end_line": 45,
      "end_column": 16,
      "severity": "warning",
      "rule": "partial/head",
      "category": "partial",
      "message": "Use headMay instead of head",
      "suggestion": "Replace with `headMay items`",
      "fix": {
        "available": true,
        "safety": "safe",
        "replacement": "headMay items"
      },
      "context": {
        "line_content": "  let first = head items",
        "function": "processItems"
      }
    },
    {
      "file": "src/Api.hs",
      "line": 23,
      "column": 5,
      "end_line": 23,
      "end_column": 19,
      "severity": "error",
      "rule": "security/sql-injection",
      "category": "security",
      "message": "Potential SQL injection vulnerability",
      "note": "Use parameterized queries",
      "fix": {
        "available": false
      }
    }
  ]
}
```

### JSON Options

```bash
# Pretty print
argus check -f json --pretty src/

# Include source context
argus check -f json --include-source src/

# Output to file
argus check -f json -o report.json src/
```

## SARIF Format

Static Analysis Results Interchange Format for security tools and GitHub code scanning.

```bash
argus check -f sarif src/ -o results.sarif
```

```json
{
  "$schema": "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json",
  "version": "2.1.0",
  "runs": [
    {
      "tool": {
        "driver": {
          "name": "Argus",
          "version": "0.2.0",
          "informationUri": "https://github.com/your-org/argus",
          "rules": [
            {
              "id": "partial/head",
              "name": "PartialHead",
              "shortDescription": {
                "text": "Avoid partial function head"
              },
              "fullDescription": {
                "text": "The head function throws an exception on empty lists. Use headMay for safe access."
              },
              "defaultConfiguration": {
                "level": "warning"
              },
              "helpUri": "https://argus.dev/docs/rules/partial-functions#head"
            }
          ]
        }
      },
      "results": [
        {
          "ruleId": "partial/head",
          "level": "warning",
          "message": {
            "text": "Use headMay instead of head"
          },
          "locations": [
            {
              "physicalLocation": {
                "artifactLocation": {
                  "uri": "src/Handler.hs"
                },
                "region": {
                  "startLine": 45,
                  "startColumn": 12,
                  "endLine": 45,
                  "endColumn": 16
                }
              }
            }
          ],
          "fixes": [
            {
              "description": {
                "text": "Replace with headMay"
              },
              "artifactChanges": [
                {
                  "artifactLocation": {
                    "uri": "src/Handler.hs"
                  },
                  "replacements": [
                    {
                      "deletedRegion": {
                        "startLine": 45,
                        "startColumn": 12,
                        "endLine": 45,
                        "endColumn": 23
                      },
                      "insertedContent": {
                        "text": "headMay items"
                      }
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }
  ]
}
```

### GitHub Code Scanning

Upload SARIF to GitHub:

```yaml
# .github/workflows/argus.yml
- name: Run Argus
  run: argus check -f sarif src/ -o results.sarif

- name: Upload SARIF
  uses: github/codeql-action/upload-sarif@v2
  with:
    sarif_file: results.sarif
```

## HTML Format

Generate HTML reports for sharing and documentation.

```bash
argus check -f html src/ -o report.html
```

Features:
- Syntax-highlighted code snippets
- Filterable issue list
- Summary statistics
- Collapsible file sections
- Dark/light mode

### HTML Options

```bash
# Include full source files
argus check -f html --full-source src/ -o report.html

# Custom title
argus check -f html --title "Sprint Review Report" src/

# Embed CSS (single file)
argus check -f html --embed src/ -o report.html
```

## JUnit Format

XML format compatible with CI test reporters.

```bash
argus check -f junit src/ -o results.xml
```

```xml
<?xml version="1.0" encoding="UTF-8"?>
<testsuites name="Argus" tests="15" failures="2" time="0.82">
  <testsuite name="src/Handler.hs" tests="1" failures="1">
    <testcase name="partial/head:45:12" time="0.01">
      <failure type="warning" message="Use headMay instead of head">
        File: src/Handler.hs
        Line: 45
        Column: 12

        let first = head items
                    ^^^^

        Suggestion: Replace with `headMay items`
      </failure>
    </testcase>
  </testsuite>
  <testsuite name="src/Api.hs" tests="1" failures="1">
    <testcase name="security/sql-injection:23:5" time="0.01">
      <failure type="error" message="Potential SQL injection vulnerability">
        File: src/Api.hs
        Line: 23
        Column: 5

        rawSql query []
        ^^^^^^^^^^^^^^
      </failure>
    </testcase>
  </testsuite>
</testsuites>
```

### CI Integration

```yaml
# Jenkins
- sh 'argus check -f junit src/ -o argus-results.xml'
- junit 'argus-results.xml'

# GitLab CI
argus-lint:
  script:
    - argus check -f junit src/ -o argus-results.xml
  artifacts:
    reports:
      junit: argus-results.xml
```

## CodeClimate Format

Compatible with CodeClimate and GitLab Code Quality.

```bash
argus check -f codeclimate src/ -o gl-code-quality-report.json
```

```json
[
  {
    "type": "issue",
    "check_name": "partial/head",
    "description": "Use headMay instead of head",
    "content": {
      "body": "The head function throws an exception on empty lists."
    },
    "categories": ["Bug Risk"],
    "severity": "minor",
    "fingerprint": "abc123...",
    "location": {
      "path": "src/Handler.hs",
      "lines": {
        "begin": 45,
        "end": 45
      }
    }
  }
]
```

### GitLab Code Quality

```yaml
# .gitlab-ci.yml
code_quality:
  image: haskell:9.10
  script:
    - stack run -- check -f codeclimate src/ -o gl-code-quality-report.json
  artifacts:
    reports:
      codequality: gl-code-quality-report.json
```

## Checkstyle Format

XML format for legacy tools and Jenkins Checkstyle plugin.

```bash
argus check -f checkstyle src/ -o checkstyle-result.xml
```

```xml
<?xml version="1.0" encoding="UTF-8"?>
<checkstyle version="4.3">
  <file name="src/Handler.hs">
    <error line="45" column="12"
           severity="warning"
           message="Use headMay instead of head"
           source="argus.partial.head"/>
  </file>
  <file name="src/Api.hs">
    <error line="23" column="5"
           severity="error"
           message="Potential SQL injection vulnerability"
           source="argus.security.sql-injection"/>
  </file>
</checkstyle>
```

## GitHub Actions Format

Native GitHub Actions annotations.

```bash
argus check -f github src/
```

Output:

```
::warning file=src/Handler.hs,line=45,col=12::Use headMay instead of head [partial/head]
::error file=src/Api.hs,line=23,col=5::Potential SQL injection vulnerability [security/sql-injection]
```

### GitHub Actions Workflow

```yaml
- name: Run Argus
  run: argus check -f github src/
```

Issues appear as annotations on the PR:

```
⚠️ src/Handler.hs#L45
   Use headMay instead of head [partial/head]

❌ src/Api.hs#L23
   Potential SQL injection vulnerability [security/sql-injection]
```

## Combining Formats

Generate multiple formats in one run:

```bash
# Terminal + JSON
argus check src/ | tee >(argus check -f json src/ > report.json)

# Or use multiple output flags
argus check -f terminal -f json -o report.json -f sarif -o results.sarif src/
```

## Custom Formats

### Template-Based Output

```bash
# Use custom template
argus check --format-template ./my-template.txt src/
```

Template syntax:

```
{{#each diagnostics}}
{{file}}:{{line}} - {{message}}
{{/each}}

Total: {{summary.total}} issues
```

### JSON to Custom Format

```bash
# Transform JSON output
argus check -f json src/ | jq -r '.diagnostics[] | "\(.file):\(.line): \(.message)"'
```

## Configuration

Set default format in `argus.toml`:

```toml
[output]
# Default format
format = "terminal"

# Terminal options
color = "auto"
context-lines = 2
group-by = "file"

# JSON options
json-pretty = true
json-include-source = false

# HTML options
html-embed = true
html-title = "Argus Report"
```

## Format Comparison

| Format | Human | Machine | CI | Size |
|--------|-------|---------|-----|------|
| terminal | ✅ | ❌ | ❌ | Small |
| json | ❌ | ✅ | ✅ | Medium |
| sarif | ❌ | ✅ | ✅ | Large |
| html | ✅ | ❌ | ❌ | Large |
| junit | ❌ | ✅ | ✅ | Medium |
| codeclimate | ❌ | ✅ | ✅ | Medium |
| checkstyle | ❌ | ✅ | ✅ | Medium |
| github | ✅ | ✅ | ✅ | Small |

## See Also

- **[argus check](./check)**: Analysis command
- **[CI Integration](../usage-guide/ci-integration)**: CI setup guides
- **[Configuration](../configuration/output-section)**: Output settings
