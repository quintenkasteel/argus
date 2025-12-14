---
sidebar_position: 5
title: Pull Requests
description: Guidelines for submitting pull requests
---

# Pull Request Guidelines

This guide explains how to submit high-quality pull requests to Argus.

## Before You Start

### Check Existing Work

1. **Search issues**: Is there an existing issue for this?
2. **Search PRs**: Has someone already started work?
3. **Check roadmap**: Is this planned feature work?

### Discuss First

For significant changes, open an issue first to:
- Confirm the change is wanted
- Discuss implementation approach
- Get feedback on design

## Creating Your Branch

### Branch Naming

```bash
# Feature
git checkout -b feature/add-new-rule

# Bug fix
git checkout -b fix/false-positive-head

# Documentation
git checkout -b docs/improve-readme

# Refactoring
git checkout -b refactor/simplify-engine
```

### Keep Branch Focused

- One logical change per branch
- Don't mix features with unrelated fixes
- Keep commits atomic

## Making Changes

### Development Workflow

```bash
# 1. Create branch
git checkout -b feature/my-change

# 2. Make changes
# ... edit files ...

# 3. Run tests
stack test

# 4. Run linter
stack run -- check src/

# 5. Commit
git add .
git commit -m "Add new rule for detecting X"

# 6. Push
git push -u origin feature/my-change
```

### Commit Messages

Follow conventional commits:

```
type(scope): description

[optional body]

[optional footer]
```

Types:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation
- `refactor`: Code restructure
- `test`: Test changes
- `chore`: Build/tooling

Examples:

```
feat(rules): add detection for redundant id usage

Detects patterns like `id x` and suggests replacing with just `x`.
Includes auto-fix capability.

Closes #123
```

```
fix(parser): handle multi-line string literals

Previously, string literals spanning multiple lines caused
parse errors in certain contexts.
```

### Code Quality Checklist

Before submitting:

- [ ] Tests pass: `stack test`
- [ ] No warnings: `stack build --pedantic`
- [ ] Linter clean: `stack run -- check src/`
- [ ] Documentation updated
- [ ] CHANGELOG entry added (if user-facing)

## Submitting the PR

### PR Title

Clear, concise summary:

```
Good:
- Add partial/fromJust rule with auto-fix
- Fix false positive in head detection
- Improve performance of AST traversal

Bad:
- Update code
- Fix bug
- WIP
```

### PR Description

Use this template:

```markdown
## Summary

Brief description of changes.

## Changes

- Added X
- Fixed Y
- Updated Z

## Testing

How was this tested?

- [ ] Unit tests added
- [ ] Integration tests pass
- [ ] Manual testing done

## Related Issues

Fixes #123
Related to #456
```

### PR Checklist

Include in description:

```markdown
## Checklist

- [ ] Tests added/updated
- [ ] Documentation updated
- [ ] CHANGELOG updated (if user-facing)
- [ ] Code follows style guide
- [ ] Commits are atomic and well-described
```

## Code Review Process

### What Reviewers Look For

1. **Correctness**: Does it work as intended?
2. **Tests**: Are changes tested?
3. **Style**: Does it follow conventions?
4. **Performance**: Any performance concerns?
5. **Documentation**: Is it documented?

### Responding to Feedback

- Be receptive to suggestions
- Ask questions if unclear
- Explain your reasoning
- Push updates to same branch

```bash
# After addressing feedback
git add .
git commit -m "Address review feedback"
git push
```

### Resolving Conflicts

```bash
# Update from main
git checkout main
git pull

# Rebase your branch
git checkout feature/my-change
git rebase main

# Resolve conflicts, then
git add .
git rebase --continue

# Force push (branch only)
git push --force-with-lease
```

## Types of Changes

### Adding Rules

Required:
- Rule implementation in `src/Argus/Rules/`
- Tests in `test/`
- Registration in `Engine.hs`

Nice to have:
- Documentation in website
- Example in tests

PR description should include:
- Rule ID and description
- Example code that triggers
- Fix provided (if any)

### Bug Fixes

Required:
- Fix implementation
- Regression test
- CHANGELOG entry

PR description should include:
- Description of bug
- Root cause
- How fix addresses it

### New Features

Required:
- Implementation
- Tests
- Documentation
- CHANGELOG entry

Consider:
- Breaking changes?
- Configuration needed?
- Migration guide?

### Documentation

Required:
- Accurate content
- Proper formatting
- Working links

PR description should explain:
- What documentation changed
- Why change was needed

## After Merge

### Clean Up

```bash
# Delete local branch
git checkout main
git pull
git branch -d feature/my-change

# Delete remote branch (if not auto-deleted)
git push origin --delete feature/my-change
```

### Verify

- Check CI passed on main
- Verify feature works in main
- Close related issues

## Common Issues

### Tests Failing in CI

```bash
# Run exact CI commands locally
stack test --pedantic

# Check for platform-specific issues
# CI runs on Linux
```

### Merge Conflicts

Keep branch up to date:

```bash
# Regularly sync with main
git fetch origin
git rebase origin/main
```

### Large Diffs

Break into smaller PRs:
- Core functionality first
- Tests separately if needed
- Documentation in follow-up

### Stale PR

If your PR sits for a while:
- Rebase on latest main
- Ping reviewers
- Ask if changes are still wanted

## PR Examples

### Good PR

```markdown
## Add partial/fromJust rule

Adds detection for `fromJust` usage which can throw runtime errors.

## Changes

- New rule `partial/fromJust` in `src/Argus/Rules/Partial.hs`
- Auto-fix suggesting `fromMaybe` with explicit default
- Tests in `test/PartialSpec.hs`
- Documentation in rule reference

## Example

```haskell
-- Before (triggers warning)
getValue = fromJust maybeValue

-- After (fixed)
getValue = fromMaybe defaultValue maybeValue
```

## Testing

- Added 5 unit tests covering:
  - Simple usage
  - Nested in expressions
  - Qualified import
  - False positive cases
- All existing tests pass

Fixes #42
```

### PR Needing Work

```markdown
## fix stuff

changed some files
```

Problems:
- Vague title
- No description
- No context
- No testing info

## Need Help?

- Ask in issue before starting
- Request early review on WIP
- Tag maintainers for guidance
- Join discussions

Thank you for contributing!
