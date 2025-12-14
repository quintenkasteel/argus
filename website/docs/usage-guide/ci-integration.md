---
sidebar_position: 3
title: CI Integration
description: Integrate Argus into your CI/CD pipeline
---

# CI Integration

Argus integrates with all major CI/CD platforms. This guide covers setup for GitHub Actions, GitLab CI, and other common systems.

## GitHub Actions

### Basic Analysis

```yaml
# .github/workflows/argus.yml
name: Argus Analysis

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  argus:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Setup Haskell
        uses: haskell-actions/setup@v2
        with:
          ghc-version: '9.10.3'
          enable-stack: true
          stack-version: 'latest'

      - name: Cache Stack
        uses: actions/cache@v4
        with:
          path: |
            ~/.stack
            .stack-work
          key: ${{ runner.os }}-stack-${{ hashFiles('stack.yaml.lock', 'package.yaml') }}
          restore-keys: |
            ${{ runner.os }}-stack-

      - name: Install Argus
        run: |
          git clone https://github.com/quinten/argus.git /tmp/argus
          cd /tmp/argus
          stack build
          stack install

      - name: Run Argus (Quick Mode)
        run: argus check --mode quick src/
```

### With SARIF Upload

```yaml
# .github/workflows/argus-sarif.yml
name: Argus Security Scan

on:
  push:
    branches: [main]
  pull_request:

jobs:
  analyze:
    runs-on: ubuntu-latest
    permissions:
      security-events: write
      contents: read

    steps:
      - uses: actions/checkout@v4

      - name: Setup Haskell
        uses: haskell-actions/setup@v2
        with:
          ghc-version: '9.10.3'
          enable-stack: true

      - name: Cache
        uses: actions/cache@v4
        with:
          path: |
            ~/.stack
            .stack-work
            .hie
          key: ${{ runner.os }}-full-${{ hashFiles('stack.yaml.lock') }}

      - name: Build with HIE
        run: stack build --ghc-options="-fwrite-ide-info -hiedir=.hie"

      - name: Install Argus
        run: |
          git clone https://github.com/quinten/argus.git /tmp/argus
          cd /tmp/argus && stack install

      - name: Run Argus
        run: argus check --mode full --format sarif > results.sarif
        continue-on-error: true

      - name: Upload SARIF
        uses: github/codeql-action/upload-sarif@v3
        with:
          sarif_file: results.sarif
          category: argus
```

### PR Comments

```yaml
# .github/workflows/argus-pr.yml
name: Argus PR Review

on:
  pull_request:

jobs:
  review:
    runs-on: ubuntu-latest
    permissions:
      pull-requests: write

    steps:
      - uses: actions/checkout@v4

      - name: Setup and Install Argus
        # ... (setup steps from above)

      - name: Run Argus on Changed Files
        id: argus
        run: |
          CHANGED=$(git diff --name-only origin/${{ github.base_ref }}...HEAD | grep '\.hs$' || true)
          if [ -n "$CHANGED" ]; then
            argus check --format json $CHANGED > results.json
            echo "has_results=true" >> $GITHUB_OUTPUT
          else
            echo "has_results=false" >> $GITHUB_OUTPUT
          fi
        continue-on-error: true

      - name: Post PR Comment
        if: steps.argus.outputs.has_results == 'true'
        uses: actions/github-script@v7
        with:
          script: |
            const fs = require('fs');
            const results = JSON.parse(fs.readFileSync('results.json', 'utf8'));

            if (results.diagnostics && results.diagnostics.length > 0) {
              let body = '## Argus Analysis Results\n\n';
              body += `Found ${results.diagnostics.length} issues:\n\n`;

              for (const d of results.diagnostics.slice(0, 10)) {
                body += `- **${d.severity}** \`${d.file}:${d.line}\`: ${d.message}\n`;
              }

              if (results.diagnostics.length > 10) {
                body += `\n... and ${results.diagnostics.length - 10} more issues.\n`;
              }

              await github.rest.issues.createComment({
                owner: context.repo.owner,
                repo: context.repo.repo,
                issue_number: context.issue.number,
                body: body
              });
            }
```

## GitLab CI

### Basic Setup

```yaml
# .gitlab-ci.yml
stages:
  - build
  - analyze

variables:
  STACK_ROOT: "${CI_PROJECT_DIR}/.stack-root"

.haskell-setup: &haskell-setup
  image: haskell:9.10.3
  cache:
    key: ${CI_COMMIT_REF_SLUG}
    paths:
      - .stack-root/
      - .stack-work/

build:
  <<: *haskell-setup
  stage: build
  script:
    - stack build --ghc-options="-fwrite-ide-info -hiedir=.hie"
  artifacts:
    paths:
      - .hie/
      - .stack-work/
    expire_in: 1 hour

argus:
  <<: *haskell-setup
  stage: analyze
  needs: [build]
  script:
    - git clone https://github.com/quinten/argus.git /tmp/argus
    - cd /tmp/argus && stack install
    - cd ${CI_PROJECT_DIR}
    - argus check --mode full --format codeclimate > codeclimate.json
  artifacts:
    reports:
      codequality: codeclimate.json
```

### With Code Climate

```yaml
argus:
  stage: analyze
  script:
    # ... setup ...
    - argus check --mode full --format codeclimate > gl-code-quality-report.json
  artifacts:
    reports:
      codequality: gl-code-quality-report.json
    paths:
      - gl-code-quality-report.json
```

## Jenkins

### Jenkinsfile

```groovy
pipeline {
    agent {
        docker {
            image 'haskell:9.10.3'
        }
    }

    stages {
        stage('Setup') {
            steps {
                sh '''
                    stack setup
                    git clone https://github.com/quinten/argus.git /tmp/argus
                    cd /tmp/argus && stack install
                '''
            }
        }

        stage('Build') {
            steps {
                sh 'stack build --ghc-options="-fwrite-ide-info -hiedir=.hie"'
            }
        }

        stage('Analyze') {
            steps {
                sh 'argus check --mode full --format checkstyle > argus-report.xml'
            }
            post {
                always {
                    recordIssues(
                        tools: [checkStyle(pattern: 'argus-report.xml')]
                    )
                }
            }
        }
    }
}
```

## CircleCI

```yaml
# .circleci/config.yml
version: 2.1

executors:
  haskell:
    docker:
      - image: haskell:9.10.3

jobs:
  build-and-analyze:
    executor: haskell
    steps:
      - checkout

      - restore_cache:
          keys:
            - stack-{{ checksum "stack.yaml.lock" }}
            - stack-

      - run:
          name: Build with HIE
          command: stack build --ghc-options="-fwrite-ide-info -hiedir=.hie"

      - save_cache:
          key: stack-{{ checksum "stack.yaml.lock" }}
          paths:
            - ~/.stack
            - .stack-work

      - run:
          name: Install Argus
          command: |
            git clone https://github.com/quinten/argus.git /tmp/argus
            cd /tmp/argus && stack install

      - run:
          name: Run Argus
          command: argus check --mode full --format junit > test-results/argus.xml

      - store_test_results:
          path: test-results

workflows:
  analyze:
    jobs:
      - build-and-analyze
```

## Azure DevOps

```yaml
# azure-pipelines.yml
trigger:
  - main

pool:
  vmImage: 'ubuntu-latest'

steps:
  - task: UseHaskellVersion@1
    inputs:
      version: '9.10.3'

  - script: |
      stack build --ghc-options="-fwrite-ide-info -hiedir=.hie"
    displayName: 'Build with HIE'

  - script: |
      git clone https://github.com/quinten/argus.git /tmp/argus
      cd /tmp/argus && stack install
    displayName: 'Install Argus'

  - script: |
      argus check --mode full --format sarif > $(Build.ArtifactStagingDirectory)/results.sarif
    displayName: 'Run Argus'

  - task: PublishBuildArtifacts@1
    inputs:
      pathToPublish: '$(Build.ArtifactStagingDirectory)/results.sarif'
      artifactName: 'CodeAnalysis'
```

## Pre-Commit Hooks

### Setup

```bash
# Install pre-commit framework
pip install pre-commit
```

### Configuration

```yaml
# .pre-commit-config.yaml
repos:
  - repo: local
    hooks:
      - id: argus
        name: Argus Analysis
        entry: argus check --mode quick
        language: system
        types: [haskell]
        pass_filenames: true
```

### Manual Git Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

set -e

# Get staged Haskell files
STAGED=$(git diff --cached --name-only --diff-filter=ACM | grep '\.hs$' || true)

if [ -n "$STAGED" ]; then
    echo "Running Argus on staged files..."
    argus check --mode quick $STAGED

    if [ $? -ne 0 ]; then
        echo "Argus found issues. Fix them or commit with --no-verify"
        exit 1
    fi
fi
```

## Exit Codes

Use exit codes to control CI behavior:

| Exit Code | Meaning | CI Action |
|-----------|---------|-----------|
| 0 | No errors | Pass |
| 1 | Errors found | Fail |
| 2 | Configuration error | Fail |

### Strict Mode

```bash
# Fail on warnings too
argus check --strict src/
```

### Warning Threshold

```bash
# Custom threshold
argus check src/ --max-warnings 10
```

## Output Formats

Choose the right format for your CI:

| Format | Best For | Command |
|--------|----------|---------|
| SARIF | GitHub, Azure DevOps | `--format sarif` |
| JSON | Custom tooling | `--format json` |
| JUnit | Jenkins, most CIs | `--format junit` |
| CodeClimate | GitLab | `--format codeclimate` |
| Checkstyle | Jenkins, legacy | `--format checkstyle` |

## Caching Strategies

### Cache HIE Files

```yaml
# GitHub Actions
- uses: actions/cache@v4
  with:
    path: |
      ~/.stack
      .stack-work
      .hie
    key: ${{ runner.os }}-${{ hashFiles('stack.yaml.lock') }}
```

### Incremental Analysis

```bash
# Only analyze changed files
CHANGED=$(git diff --name-only HEAD~1 | grep '\.hs$' || true)
if [ -n "$CHANGED" ]; then
    argus check $CHANGED
fi
```

## Best Practices

### 1. Fast Feedback First

```yaml
jobs:
  quick-check:
    # Runs immediately on all pushes
    script: argus check --mode quick src/

  full-check:
    needs: [quick-check, build]
    # Runs after quick check passes
    script: argus check --mode full src/
```

### 2. Cache Aggressively

Cache Stack, HIE files, and Argus installation.

### 3. Fail Fast on Errors

```yaml
script: argus check --mode quick src/
# Only continue if quick mode passes
```

### 4. Review Warnings

```yaml
# Don't fail on warnings in development
script: argus check src/ || echo "Warnings found"

# But do fail on release branches
script: argus check src/ --strict
```

### 5. Track Trends

Store results over time to track code quality trends:

```yaml
- name: Upload Results
  uses: actions/upload-artifact@v4
  with:
    name: argus-results-${{ github.sha }}
    path: results.json
```

## Next Steps

- **[IDE Integration](./ide-integration)**: Editor setup
- **[Output Formats](../cli-reference/output-formats)**: Format details
- **[Configuration](../configuration/file-format)**: Customize for CI
