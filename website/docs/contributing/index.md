---
sidebar_position: 10
title: Contributing
description: How to contribute to Argus development
---

# Contributing to Argus

Welcome to the Argus contributor guide! This section explains how to contribute to Argus, from reporting bugs to adding new rules and features.

## Ways to Contribute

### Report Issues

Found a bug or have a feature request?

- **Bug reports**: Include minimal reproduction code
- **Feature requests**: Describe the use case
- **Documentation issues**: Point out unclear or missing docs

### Improve Documentation

Documentation improvements are always welcome:

- Fix typos and clarify wording
- Add examples and use cases
- Improve API documentation
- Translate documentation

### Add Rules

Extend Argus with new lint rules:

- Create rules for new patterns
- Port rules from other linters
- Add domain-specific rules
- Improve existing rule accuracy

### Fix Bugs

Help make Argus more reliable:

- Investigate reported issues
- Write regression tests
- Submit bug fixes
- Improve error messages

### Add Features

Extend Argus functionality:

- New analysis modes
- Output format support
- Editor integrations
- Performance improvements

## Getting Started

### 1. Set Up Development Environment

```bash
# Clone the repository
git clone https://github.com/your-org/argus.git
cd argus

# Build with Stack
stack build

# Run tests
stack test

# Run Argus
stack run -- check src/
```

### 2. Understand the Codebase

Key directories:

```
src/Argus/
├── Types.hs           # Core types
├── Core.hs            # Main orchestration
├── CLI.hs             # Command-line interface
├── Config.hs          # Configuration
├── Analysis/          # Analysis modules
├── Rules/             # Lint rules
├── Refactor/          # Auto-fix engine
├── Output/            # Output formatters
└── HIE/               # HIE integration
```

### 3. Find Something to Work On

- Check open issues labeled `good-first-issue`
- Look for `help-wanted` labels
- Review the roadmap for planned features
- Ask in discussions for guidance

### 4. Make Your Changes

```bash
# Create a branch
git checkout -b feature/my-contribution

# Make changes
# ...

# Run tests
stack test

# Check your changes
stack run -- check src/
```

### 5. Submit a Pull Request

- Write clear commit messages
- Include tests for new functionality
- Update documentation as needed
- Reference related issues

## Quick Links

| Topic | Description |
|-------|-------------|
| [Development Setup](./development-setup) | Setting up your environment |
| [Code Style](./code-style) | Coding conventions |
| [Adding Rules](./adding-rules) | Creating new lint rules |
| [Testing Guide](./testing-guide) | Writing and running tests |
| [Pull Requests](./pull-requests) | PR guidelines |

## Community Guidelines

### Be Respectful

- Treat everyone with respect
- Welcome newcomers
- Be patient with questions
- Give constructive feedback

### Be Collaborative

- Discuss before major changes
- Share knowledge
- Help review others' PRs
- Celebrate contributions

### Be Professional

- Stay focused on technical merit
- Avoid personal attacks
- Accept feedback gracefully
- Keep discussions productive

## Questions?

- **GitHub Discussions**: For questions and ideas
- **Issues**: For bugs and feature requests
- **Pull Requests**: For code contributions

Thank you for contributing to Argus!
