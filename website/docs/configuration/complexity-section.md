---
sidebar_position: 4
title: Complexity Configuration
description: Configure code complexity thresholds and metrics
---

# Complexity Configuration

The `[complexity]` section configures complexity analysis thresholds for cyclomatic complexity, cognitive complexity, and other code metrics.

## Basic Configuration

```toml
[complexity]
# Enable complexity analysis
enabled = true

# Default severity for complexity issues
severity = "warning"
```

## Cyclomatic Complexity

Cyclomatic complexity measures the number of linearly independent paths through code. Each branch (`if`, `case`, guard) adds to the count.

```toml
[complexity]
# Maximum cyclomatic complexity per function
max-cyclomatic = 15

# Severity at different thresholds
[complexity.cyclomatic]
warning = 10    # Warn at 10
error = 20      # Error at 20
```

### What Increases Cyclomatic Complexity

| Construct | Increment |
|-----------|-----------|
| `if` expression | +1 |
| `case` branch | +1 per branch |
| Guard clause | +1 per guard |
| `&&` / `||` | +1 |
| Pattern match alternative | +1 |

### Example

```haskell
-- Cyclomatic complexity: 5
process :: Item -> Result
process item
  | isSpecial item = handleSpecial item    -- +1
  | isUrgent item  = handleUrgent item     -- +1
  | otherwise = case category item of
      A -> processA item                   -- +1
      B -> processB item                   -- +1
      C -> processC item                   -- +1
```

## Cognitive Complexity

Cognitive complexity measures how difficult code is to understand. It accounts for nesting, recursion, and control flow breaks.

```toml
[complexity]
# Maximum cognitive complexity per function
max-cognitive = 20

[complexity.cognitive]
warning = 15
error = 30
```

### What Increases Cognitive Complexity

| Construct | Base | Nesting Bonus |
|-----------|------|---------------|
| `if` / `case` | +1 | +1 per level |
| `else` branch | +1 | +1 per level |
| Nested control | +1 | cumulative |
| Recursion | +1 | - |
| `&&` / `||` sequence | +1 | - |

### Example

```haskell
-- Cognitive complexity: 8
process :: [Item] -> IO ()
process items = do
  forM_ items $ \item ->           -- +1
    when (isValid item) $ do       -- +1 (nesting +1)
      if isSpecial item            -- +1 (nesting +2)
        then handleSpecial item
        else do                    -- +1
          case category item of    -- +1 (nesting +3)
            A -> processA item
            B -> processB item
            _ -> return ()
```

## Function Length

```toml
[complexity]
# Maximum lines per function
max-function-lines = 100

# Maximum statements per function
max-function-statements = 50

[complexity.function-lines]
warning = 50
error = 100
```

### Counting Rules

- Empty lines are not counted
- Comment lines are not counted
- Only executable statements count

## Module Complexity

```toml
[complexity]
# Maximum lines per module
max-module-lines = 1000

# Maximum functions per module
max-functions-per-module = 50

# Maximum exports per module
max-exports = 30
```

## Parameter Count

```toml
[complexity]
# Maximum parameters per function
max-parameters = 5

# Maximum type parameters
max-type-parameters = 4
```

### Example

```haskell
-- Warning: 7 parameters exceeds limit of 5
createUser :: String -> String -> Int -> String
           -> String -> Bool -> Maybe String -> User
```

Suggestion: Use a configuration record:

```haskell
data UserConfig = UserConfig
  { name :: String
  , email :: String
  , age :: Int
  -- ...
  }

createUser :: UserConfig -> User
```

## Nesting Depth

```toml
[complexity]
# Maximum nesting depth
max-nesting = 4

[complexity.nesting]
warning = 4
error = 6
```

### What Counts as Nesting

- `do` blocks
- `let ... in`
- `where` clauses
- `case` expressions
- `if` expressions
- Lambda expressions

### Example

```haskell
-- Nesting depth: 5 (exceeds limit)
process = do                     -- Level 1
  items <- getItems
  forM_ items $ \item -> do      -- Level 2
    when (isValid item) $ do     -- Level 3
      case category item of      -- Level 4
        A -> do                  -- Level 5 (warning!)
          handleA item
```

## Boolean Expression Complexity

```toml
[complexity]
# Maximum operators in boolean expression
max-boolean-operators = 4
```

### Example

```haskell
-- Warning: 5 boolean operators
isEligible x =
  isActive x && hasAccount x && isVerified x
  && (isPremium x || hasCredits x) && not (isBanned x)
```

## Per-Function Overrides

Override thresholds for specific functions:

```toml
[complexity.overrides]
# Complex parser is allowed more complexity
"Parser.parseExpression" = { cyclomatic = 30, cognitive = 40 }

# Main dispatcher
"Main.dispatch" = { cyclomatic = 25 }

# Generated code
"Generated.*" = { enabled = false }
```

## Per-Module Overrides

```toml
[complexity.modules]
# Legacy modules have relaxed limits
"Legacy.*" = {
  max-cyclomatic = 30,
  max-function-lines = 200,
}

# Test modules
"*Spec" = {
  max-function-lines = 200,  # Test setup can be long
  max-nesting = 6,
}
```

## Aggregated Metrics

```toml
[complexity.aggregate]
# Average complexity across module
max-average-cyclomatic = 5

# Average function length
max-average-function-lines = 30

# Total module complexity
max-total-complexity = 200
```

## Reporting

### Show Complexity in Output

```toml
[complexity]
# Show complexity values in diagnostics
show-values = true
```

Output:

```
src/Handler.hs:45:1: warning [complexity/cyclomatic]
  Function 'processItem' has cyclomatic complexity 18 (max: 15)

  Consider breaking into smaller functions.
```

### Complexity Report

```bash
# Generate complexity report
argus check --complexity-report src/

# Output to file
argus check --complexity-report -o complexity.json src/
```

Report includes:
- Per-function metrics
- Module summaries
- Hot spots (highest complexity)
- Trend data (if available)

## Visualization

```bash
# Generate complexity heatmap
argus check --complexity-heatmap src/ -o heatmap.html
```

## Complete Example

```toml
[complexity]
enabled = true
severity = "warning"

# Cyclomatic complexity
max-cyclomatic = 15

[complexity.cyclomatic]
warning = 10
error = 20

# Cognitive complexity
max-cognitive = 20

[complexity.cognitive]
warning = 15
error = 30

# Function size
max-function-lines = 100
max-function-statements = 50

[complexity.function-lines]
warning = 50
error = 100

# Module size
max-module-lines = 1000
max-functions-per-module = 50
max-exports = 30

# Other metrics
max-parameters = 5
max-nesting = 4
max-boolean-operators = 4

# Show values in output
show-values = true

# Per-function overrides
[complexity.overrides]
"Parser.parseExpression" = { cyclomatic = 30 }
"Main.dispatch" = { cyclomatic = 25 }

# Per-module overrides
[complexity.modules]
"Legacy.*" = { max-cyclomatic = 30 }
"*Spec" = { max-function-lines = 200 }
```

## Suppression

```haskell
-- Intentionally complex function
-- argus:ignore complexity/cyclomatic
dispatch :: Command -> IO ()
dispatch cmd = case cmd of
  -- many cases...
```

Or for a specific threshold:

```haskell
-- argus:ignore complexity/cyclomatic: parser requires many branches
parseExpr :: String -> Either Error Expr
```

## Best Practices

1. **Start with defaults** - Adjust after baseline analysis
2. **Address high complexity first** - Focus on functions > 20 cyclomatic
3. **Use cognitive complexity** - Better reflects readability
4. **Extract helper functions** - Reduce nesting and complexity
5. **Consider domain** - Parsers/dispatchers may need higher limits

## See Also

- **[Complexity Rules](../rules/complexity)**: Complexity rule reference
- **[Rules Configuration](./rules-section)**: General rule settings
- **[Architecture Rules](../rules/architecture)**: Module-level metrics
