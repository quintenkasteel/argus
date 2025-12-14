---
sidebar_position: 2
title: Code Style
description: Coding conventions for Argus
---

# Code Style

This guide documents the coding conventions used in Argus. Following these conventions ensures consistency across the codebase.

## General Principles

1. **Clarity over cleverness** - Write readable code
2. **Type safety** - Use the type system to prevent errors
3. **Explicit over implicit** - Make intentions clear
4. **Consistency** - Follow existing patterns

## Module Structure

### Standard Layout

```haskell
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module description.
--
-- More detailed documentation if needed.
module Argus.ModuleName
  ( -- * Exported Types
    MyType(..)
  , OtherType
    -- * Functions
  , publicFunction
  , anotherFunction
  ) where

-- Standard library imports
import Control.Monad (forM, when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

-- Qualified imports
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- Project imports
import Argus.Types
import Argus.Utils

-- Internal types (not exported)
data InternalType = ...

-- Exported types
data MyType = ...

-- Functions
publicFunction :: ...
publicFunction = ...

-- Internal helpers at bottom
helper :: ...
helper = ...
```

### Import Organization

```haskell
-- 1. Standard library (alphabetical)
import Control.Monad
import Data.List
import Data.Maybe

-- 2. External packages (alphabetical)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

-- 3. Project modules (alphabetical)
import Argus.Config
import Argus.Types
```

### Qualified Import Conventions

| Module | Alias |
|--------|-------|
| `Data.Map.Strict` | `Map` |
| `Data.Set` | `Set` |
| `Data.Text` | `T` |
| `Data.Text.IO` | `TIO` |
| `Data.ByteString` | `BS` |
| `Data.ByteString.Lazy` | `BSL` |
| `Data.Vector` | `V` |
| `Data.Aeson` | `Aeson` |

## Type Definitions

### Use Semantic Newtypes

```haskell
-- Good: semantic types
newtype Line = Line Int
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)

newtype Column = Column Int
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)

newtype Seconds = Seconds Double
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Fractional)

-- Bad: raw primitives
type Line = Int    -- No type safety
type Column = Int  -- Easy to mix up
```

### Explicit Deriving Strategy

```haskell
-- Good: explicit strategy
data Config = Config
  { configRules :: [Rule]
  , configPaths :: [FilePath]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Bad: implicit strategy
data Config = Config { ... }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
```

### Record Syntax

```haskell
-- Use record syntax for 3+ fields
data Diagnostic = Diagnostic
  { diagRule     :: RuleId
  , diagSeverity :: Severity
  , diagMessage  :: Text
  , diagSpan     :: SrcSpan
  , diagFix      :: Maybe Fix
  }

-- Simple constructors for 1-2 fields
data Result a
  = Success a
  | Failure Text
```

### Strict Fields by Default

```haskell
{-# LANGUAGE StrictData #-}

-- Fields strict by default
data Stats = Stats
  { statsFiles    :: Int
  , statsWarnings :: Int
  , statsErrors   :: Int
  }

-- Explicit lazy when needed
data LazyConfig = LazyConfig
  { lcRules :: ~[Rule]  -- Lazy field
  , lcCache :: ~Cache
  }
```

## Function Style

### Type Signatures

```haskell
-- Always include type signatures
analyzeFile :: Config -> FilePath -> IO [Diagnostic]
analyzeFile config path = do
  ...

-- Use constraints explicitly
matchPattern
  :: (Data a, Typeable a)
  => Pattern
  -> a
  -> Maybe Bindings
```

### Pattern Matching

```haskell
-- Good: exhaustive patterns
processSeverity :: Severity -> Text
processSeverity = \case
  Error   -> "error"
  Warning -> "warning"
  Info    -> "info"
  Hint    -> "hint"

-- Good: guard clauses
validateConfig :: Config -> Either Text Config
validateConfig config
  | null (configPaths config) = Left "No paths specified"
  | configParallel config < 1 = Left "Parallel must be >= 1"
  | otherwise = Right config
```

### Error Handling

```haskell
-- Use Either for expected failures
parseRule :: Text -> Either ParseError Rule
parseRule input = ...

-- Use Maybe for absence
findRule :: RuleId -> [Rule] -> Maybe Rule
findRule ruleId rules = find ((== ruleId) . ruleId) rules

-- Avoid exceptions in pure code
-- Bad:
processUnsafe xs = head xs  -- Throws on empty

-- Good:
processSafe xs = case xs of
  (x:_) -> Just x
  []    -> Nothing
```

### Point-Free Style

```haskell
-- Good: clear point-free
filterWarnings :: [Diagnostic] -> [Diagnostic]
filterWarnings = filter ((== Warning) . diagSeverity)

-- Bad: overly complex point-free
process = fmap (uncurry (flip const)) . filter (not . null . snd) . zip [1..]

-- When in doubt, use explicit arguments
processData items =
  items
    & filter isValid
    & map transform
    & sortBy priority
```

## Naming Conventions

### Functions

```haskell
-- Verbs for actions
analyzeFile :: ...
parseConfig :: ...
formatOutput :: ...

-- Predicates with "is/has/can"
isValid :: Diagnostic -> Bool
hasWarnings :: [Diagnostic] -> Bool
canAutoFix :: Rule -> Bool

-- Transformations: "to/from"
toJson :: Diagnostic -> Aeson.Value
fromConfig :: Config -> Settings
```

### Types

```haskell
-- Nouns for data types
data Diagnostic = ...
data Config = ...
data RuleEngine = ...

-- "Settings/Options" for config bundles
data AnalysisSettings = ...
data OutputOptions = ...
```

### Record Fields

```haskell
-- Prefix with abbreviated type name
data Rule = Rule
  { ruleId       :: RuleId
  , ruleName     :: Text
  , ruleSeverity :: Severity
  }

data Config = Config
  { cfgRules   :: [Rule]
  , cfgPaths   :: [FilePath]
  , cfgVerbose :: Bool
  }
```

## Documentation

### Module Documentation

```haskell
-- | Rule engine for pattern matching and analysis.
--
-- This module provides the core rule evaluation functionality,
-- including:
--
-- * Pattern compilation
-- * AST matching
-- * Fix generation
--
-- == Usage
--
-- @
-- let rules = loadRules config
-- diags <- evaluateRules rules source
-- @
module Argus.Rules.Engine where
```

### Function Documentation

```haskell
-- | Analyze a Haskell source file for issues.
--
-- Parses the file and runs all enabled rules against it.
--
-- @
-- diags <- analyzeFile defaultConfig "src/Main.hs"
-- @
analyzeFile
  :: Config      -- ^ Analysis configuration
  -> FilePath    -- ^ Path to source file
  -> IO [Diagnostic]
analyzeFile config path = ...
```

### Inline Comments

```haskell
-- Explain non-obvious logic
analyzeModule mod = do
  -- First pass: collect all definitions
  let defs = collectDefinitions mod

  -- Second pass: analyze usage
  -- (requires defs to be complete)
  usages <- analyzeUsage defs mod

  -- Combine results
  return $ matchUsages defs usages
```

## Formatting

### Line Length

- Target: 80 characters
- Maximum: 100 characters

### Indentation

- 2 spaces for continuation
- 4 spaces for `where` blocks

```haskell
analyzeFile config path = do
  source <- readFile path
  let parsed = parseModule source
  case parsed of
    Left err ->
      return [parseErrorDiag err]
    Right mod ->
      runAnalysis config mod
  where
    runAnalysis cfg m =
      evaluateRules (cfgRules cfg) m
```

### Alignment

```haskell
-- Align record fields
data Config = Config
  { cfgRules   :: [Rule]
  , cfgPaths   :: [FilePath]
  , cfgOutput  :: OutputFormat
  , cfgVerbose :: Bool
  }

-- Align imports when grouped
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Text       as T

-- Align case branches
formatSeverity = \case
  Error   -> "error"
  Warning -> "warning"
  Info    -> "info"
```

## Best Practices

### Use StrictData

```haskell
{-# LANGUAGE StrictData #-}

-- Prevents space leaks from lazy fields
```

### Bang Patterns for Accumulators

```haskell
-- Good: strict accumulator
countDiags :: [Diagnostic] -> Int
countDiags = go 0
  where
    go !acc []     = acc
    go !acc (_:xs) = go (acc + 1) xs

-- Or use foldl'
countDiags = foldl' (\acc _ -> acc + 1) 0
```

### Avoid Partial Functions

```haskell
-- Bad
process xs = head xs

-- Good
process xs = case xs of
  (x:_) -> x
  []    -> defaultValue

-- Or use safe alternatives
import Safe (headMay)
process = headMay
```

## Next Steps

- **[Adding Rules](./adding-rules)**: Create lint rules
- **[Testing Guide](./testing-guide)**: Write tests
- **[Pull Requests](./pull-requests)**: Submit changes
