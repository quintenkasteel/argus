---
sidebar_position: 6
title: Complexity Rules
description: Track and enforce code complexity metrics
---

# Complexity Rules

Argus tracks multiple complexity metrics to help maintain readable, maintainable code. High complexity often indicates code that's hard to understand, test, and modify.

## Complexity Metrics

### Cyclomatic Complexity

Cyclomatic complexity measures the number of linearly independent paths through code—essentially counting decision points.

**How it's calculated:**
- Start with 1
- +1 for each `if`, `case`, guard, `||`, `&&`

```haskell
-- Cyclomatic complexity: 5
classifyNumber :: Int -> String
classifyNumber n
  | n < 0     = "negative"  -- +1
  | n == 0    = "zero"      -- +1
  | n < 10    = "small"     -- +1
  | n < 100   = "medium"    -- +1
  | otherwise = "large"
-- Base 1 + 4 guards = 5
```

**Rule:** `complexity/cyclomatic`

**Thresholds:**
| Range | Assessment |
|-------|------------|
| 1-10 | Simple, low risk |
| 11-20 | Moderate complexity |
| 21-50 | High complexity |
| 50+ | Very high risk |

### Cognitive Complexity

Cognitive complexity measures how hard code is to understand, accounting for nesting and cognitive overhead.

**How it's calculated:**
- +1 for each control structure
- Additional +1 for each level of nesting
- +1 for breaking linear flow (recursion, jumps)

```haskell
-- Cognitive complexity: 8
processItems :: [Item] -> IO ()
processItems items = do
  forM_ items $ \item -> do           -- +1 (loop)
    when (isValid item) $ do           -- +2 (if + nesting)
      case itemType item of            -- +3 (case + nesting)
        TypeA -> handleA item
        TypeB -> handleB item
        _     -> return ()
  cleanup                              -- No increment
```

**Rule:** `complexity/cognitive`

**Thresholds:**
| Range | Assessment |
|-------|------------|
| 1-15 | Easy to understand |
| 16-25 | Requires concentration |
| 26+ | Hard to understand |

### Function Length

Lines of code per function.

```haskell
-- Warning: function exceeds 50 lines
longFunction :: Config -> IO Result
longFunction config = do
  -- 60 lines of code
  ...
```

**Rule:** `complexity/function-length`

### Nesting Depth

Maximum indentation level in a function.

```haskell
-- Warning: nesting depth 5
deeplyNested x = do
  when condition1 $ do           -- depth 1
    forM_ items $ \item -> do    -- depth 2
      case item of               -- depth 3
        A -> when valid $ do     -- depth 4
          if special             -- depth 5
            then handle
            else skip
```

**Rule:** `complexity/nesting`

### Parameter Count

Number of function parameters.

```haskell
-- Warning: 8 parameters
configureService :: Host -> Port -> User -> Pass -> DB -> Pool -> Timeout -> Retry -> IO Service

-- Better: use a record
data ServiceConfig = ServiceConfig
  { host    :: Host
  , port    :: Port
  , user    :: User
  -- ...
  }

configureService :: ServiceConfig -> IO Service
```

**Rule:** `complexity/parameters`

### Pattern Branches

Number of alternatives in a case expression.

```haskell
-- Warning: 15 case branches
handleMessage :: Message -> IO ()
handleMessage msg = case messageType msg of
  Type1  -> handler1
  Type2  -> handler2
  -- ... 13 more cases
  Type15 -> handler15
```

**Rule:** `complexity/pattern-branches`

## Additional Metrics

### Recursion Type

Argus identifies recursion patterns:

| Type | Description |
|------|-------------|
| None | No recursion |
| Direct | Function calls itself |
| Tail | Tail recursive (optimizable) |
| Mutual | Functions call each other |

```haskell
-- Direct recursion
factorial n = if n <= 1 then 1 else n * factorial (n - 1)

-- Tail recursion
factorial n = go n 1
  where go 0 acc = acc
        go n acc = go (n - 1) (n * acc)

-- Mutual recursion
isEven 0 = True
isEven n = isOdd (n - 1)
isOdd 0 = False
isOdd n = isEven (n - 1)
```

**Rule:** `complexity/recursion-type` (info level)

### Lambda Nesting

Depth of nested lambda expressions.

```haskell
-- Warning: deeply nested lambdas
complicated = \x -> \y -> \z -> \w -> x + y + z + w

-- Better: use regular function
simpler x y z w = x + y + z + w
```

**Rule:** `complexity/lambda-nesting`

### Monad Transformer Stack

Depth of monad transformer stacks.

```haskell
-- Warning: deep transformer stack
type AppM = ReaderT Config (StateT AppState (ExceptT AppError (WriterT [Log] IO)))

-- Consider: use effects library or flatten
```

**Rule:** `complexity/transformer-depth`

### Type Constraint Count

Number of constraints on a type signature.

```haskell
-- Warning: many constraints
complexFunction :: (Monad m, MonadReader Config m, MonadState State m,
                    MonadError Error m, MonadIO m, HasLogger m) => Int -> m ()

-- Consider: use constraint synonym
type AppConstraints m = (Monad m, MonadReader Config m, MonadState State m, ...)
complexFunction :: AppConstraints m => Int -> m ()
```

**Rule:** `complexity/constraint-count`

## Configuration

### Setting Thresholds

```toml
[complexity]
enabled = true

# Cyclomatic complexity
cyclomatic-warning = 10
cyclomatic-error = 20

# Cognitive complexity
cognitive-warning = 15
cognitive-error = 25

# Function length (lines)
function-length-warning = 50
function-length-error = 100

# Nesting depth
nesting-warning = 4
nesting-error = 6

# Parameter count
parameter-warning = 5
parameter-error = 8

# Case branches
pattern-branch-warning = 10
pattern-branch-error = 20

# Local bindings (let/where)
local-binding-warning = 10
```

### Per-Module Thresholds

```toml
[[scopes]]
modules = ["*.Parser", "*.Lexer"]
[scopes.complexity]
cyclomatic-warning = 25  # Parsers are naturally complex
```

### Disable Complexity Rules

```toml
[rules]
disabled = ["complexity/function-length"]
```

## Refactoring Strategies

### Reducing Cyclomatic Complexity

1. **Extract functions**:
```haskell
-- Before: high complexity
process x = case classify x of
  A -> if condition1 then handleA1 else handleA2
  B -> if condition2 then handleB1 else handleB2

-- After: distributed complexity
process x = case classify x of
  A -> handleA x
  B -> handleB x

handleA x = if condition1 x then handleA1 else handleA2
```

2. **Use pattern matching**:
```haskell
-- Before
if x == A then ... else if x == B then ... else ...

-- After
case x of
  A -> ...
  B -> ...
  _ -> ...
```

### Reducing Cognitive Complexity

1. **Flatten nesting**:
```haskell
-- Before
when cond1 $ when cond2 $ when cond3 $ action

-- After
when (cond1 && cond2 && cond3) action
```

2. **Early return**:
```haskell
-- Before
process x = do
  when (isValid x) $ do
    result <- compute x
    when (isGood result) $ do
      store result

-- After
process x = do
  guard (isValid x)
  result <- compute x
  guard (isGood result)
  store result
```

### Reducing Parameter Count

```haskell
-- Before: many parameters
createUser name email age city country zip phone verified = ...

-- After: configuration record
data UserConfig = UserConfig { name :: Text, email :: Text, ... }
createUser :: UserConfig -> User
```

## Viewing Complexity Reports

```bash
# Show complexity metrics
argus check --complexity-report src/

# Output:
# src/MyModule.hs
#   processData: cyclomatic=15, cognitive=22, lines=45
#   handleRequest: cyclomatic=8, cognitive=12, lines=30
```

## CI Integration

### Fail on High Complexity

```bash
argus check --mode quick src/ --max-complexity 20
```

### Trend Tracking

```bash
argus check --format json src/ | jq '.complexity' > complexity-report.json
```

## Best Practices

1. **Keep functions small**: Aim for fewer than 50 lines
2. **Limit nesting**: Maximum 4 levels
3. **Use meaningful abstractions**: Extract complex conditionals
4. **Prefer composition**: Small functions that compose
5. **Document complex code**: Explain why complexity is necessary

## Next Steps

- **[Architecture Rules](./architecture)**: Module-level analysis
- **[Naming Rules](./naming)**: Naming conventions
- **[Custom Rules](./custom-rules)**: Define your own metrics
