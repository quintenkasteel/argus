---
sidebar_position: 1
title: Architecture Overview
description: High-level architecture of Argus static analyzer
---

# Architecture Overview

Argus is designed as a modular, extensible static analysis framework. This document provides a high-level overview of its architecture and key components.

## System Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         CLI / LSP                               │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐           │
│  │  check  │  │   fix   │  │ unused  │  │   lsp   │           │
│  └────┬────┘  └────┬────┘  └────┬────┘  └────┬────┘           │
└───────┼────────────┼────────────┼────────────┼─────────────────┘
        │            │            │            │
        └────────────┴────────────┴────────────┘
                           │
┌──────────────────────────┼──────────────────────────────────────┐
│                    Core Engine                                   │
│  ┌────────────────┐  ┌────────────────┐  ┌────────────────┐    │
│  │   Orchestrator │  │  Rule Engine   │  │ Fix Engine     │    │
│  │                │  │                │  │                │    │
│  │  - File load   │  │  - Evaluation  │  │  - Apply fixes │    │
│  │  - Parallel    │  │  - Matching    │  │  - Validation  │    │
│  │  - Caching     │  │  - Scoring     │  │  - Exact print │    │
│  └────────────────┘  └────────────────┘  └────────────────┘    │
└──────────────────────────┬──────────────────────────────────────┘
                           │
┌──────────────────────────┼──────────────────────────────────────┐
│                    Analysis Layer                                │
│  ┌────────────┐  ┌────────────┐  ┌────────────┐  ┌───────────┐ │
│  │  Syntactic │  │  Semantic  │  │  DepGraph  │  │  Unused   │ │
│  │            │  │            │  │            │  │           │ │
│  │  - Parse   │  │  - Types   │  │  - Imports │  │  - Dead   │ │
│  │  - AST     │  │  - HIE     │  │  - Cycles  │  │  - Reach  │ │
│  │  - Pattern │  │  - Symbol  │  │  - Layers  │  │  - Export │ │
│  └────────────┘  └────────────┘  └────────────┘  └───────────┘ │
└──────────────────────────┬──────────────────────────────────────┘
                           │
┌──────────────────────────┼──────────────────────────────────────┐
│                    Data Layer                                    │
│  ┌────────────────┐  ┌────────────────┐  ┌────────────────┐    │
│  │   GHC Parser   │  │   HIE Index    │  │    Config      │    │
│  │                │  │                │  │                │    │
│  │  - Parse .hs   │  │  - Load .hie   │  │  - TOML parse  │    │
│  │  - AST types   │  │  - Query       │  │  - Validation  │    │
│  │  - Locations   │  │  - Types       │  │  - Scopes      │    │
│  └────────────────┘  └────────────────┘  └────────────────┘    │
└─────────────────────────────────────────────────────────────────┘
```

## Module Structure

```
src/Argus/
├── Types.hs              # Core types (Diagnostic, Fix, SrcSpan)
├── Core.hs               # Main orchestration engine
├── CLI.hs                # Command-line interface
├── Config.hs             # TOML configuration parsing
│
├── Analysis/             # Analysis modules
│   ├── Syntactic.hs      # AST-based analysis
│   ├── Semantic.hs       # HIE-based type analysis
│   ├── DepGraph.hs       # Dependency graph analysis
│   └── Unused.hs         # Dead code detection
│
├── Rules/                # Rule system
│   ├── Engine.hs         # Unified rule evaluator
│   ├── ASTMatch.hs       # AST pattern matching
│   ├── DSL.hs            # Custom rule DSL
│   ├── Partial.hs        # Partial function rules
│   ├── Security.hs       # Security rules
│   └── ...               # Other rule modules
│
├── Refactor/             # Auto-fix system
│   ├── Engine.hs         # Fix application engine
│   ├── ExactPrint.hs     # Formatting preservation
│   ├── Substitution.hs   # Code substitution
│   └── SafeRefactor.hs   # Safe fix validation
│
├── Output/               # Output formatters
│   ├── Terminal.hs       # Terminal output
│   ├── Json.hs           # JSON output
│   ├── Sarif.hs          # SARIF format
│   └── Html.hs           # HTML reports
│
├── HIE/                  # HIE file handling
│   ├── Query.hs          # HIE queries
│   ├── TypeInfo.hs       # Type information
│   └── SymbolTable.hs    # Symbol resolution
│
└── Imports/              # Import management
    ├── Analysis.hs       # Import analysis
    └── Manager.hs        # Import modifications
```

## Data Flow

### Analysis Pipeline

```
Source Files (.hs)
        │
        ▼
┌───────────────────┐
│    GHC Parser     │  Parse source to AST
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│  Syntactic Pass   │  Fast pattern matching
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│   HIE Loading     │  Load type information (optional)
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│  Semantic Pass    │  Type-aware analysis
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│   Rule Engine     │  Evaluate all rules
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│    Diagnostics    │  Collected issues
└───────────────────┘
```

### Fix Pipeline

```
Diagnostics
        │
        ▼
┌───────────────────┐
│  Fix Generation   │  Generate fix suggestions
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│  Conflict Check   │  Detect overlapping fixes
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│   Exact Print     │  Apply preserving format
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│   Validation      │  Type check result
└─────────┬─────────┘
          │
          ▼
Modified Source
```

## Key Components

### Core Engine (Argus.Core)

The orchestrator that coordinates analysis:

```haskell
-- Main entry point
analyzeProject :: Config -> [FilePath] -> IO AnalysisResult

-- Per-file analysis
analyzeFile :: Config -> FilePath -> IO [Diagnostic]

-- Parallel processing
analyzeParallel :: Config -> [FilePath] -> IO [Diagnostic]
```

Responsibilities:
- File discovery and loading
- Parallel analysis coordination
- Result aggregation
- Caching management

### Rule Engine (Argus.Rules.Engine)

Unified rule evaluation system:

```haskell
-- Rule definition
data Rule = Rule
  { ruleId       :: RuleId
  , ruleCategory :: Category
  , ruleSeverity :: Severity
  , ruleCheck    :: RuleChecker
  , ruleFix      :: Maybe FixGenerator
  }

-- Evaluate all rules
evaluateRules :: [Rule] -> ParsedModule -> [Diagnostic]
```

Features:
- Pattern-based matching
- Type-aware rules (with HIE)
- Configurable severity
- Fix generation

### AST Matching (Argus.Rules.ASTMatch)

Generic AST traversal using SYB (Scrap Your Boilerplate):

```haskell
-- Pattern matching
matchPattern :: Pattern -> HsExpr -> Maybe Bindings

-- AST traversal
traverseAST :: Data a => (a -> [Diagnostic]) -> ParsedModule -> [Diagnostic]
```

### Semantic Analysis (Argus.Analysis.Semantic)

HIE-powered type-aware analysis:

```haskell
-- Type queries
getTypeAt :: SrcSpan -> HieFile -> Maybe Type

-- Symbol resolution
resolveSymbol :: Name -> HieFiles -> [Definition]

-- Cross-module analysis
findReferences :: Name -> HieFiles -> [SrcSpan]
```

### Refactor Engine (Argus.Refactor.Engine)

Safe automatic code modification:

```haskell
-- Apply fix
applyFix :: Fix -> ParsedModule -> Either Error ParsedModule

-- Validate result
validateFix :: FilePath -> IO Bool

-- Transaction support
applyFixTransaction :: [Fix] -> IO (Either Error ())
```

## Type System

### Core Types

```haskell
-- Source location
data SrcSpan = SrcSpan
  { spanFile   :: FilePath
  , spanStart  :: (Line, Column)
  , spanEnd    :: (Line, Column)
  }

-- Diagnostic issue
data Diagnostic = Diagnostic
  { diagSpan     :: SrcSpan
  , diagSeverity :: Severity
  , diagRule     :: RuleId
  , diagMessage  :: Text
  , diagFix      :: Maybe Fix
  , diagNotes    :: [Text]
  }

-- Severity levels
data Severity
  = Error
  | Warning
  | Suggestion
  | Info

-- Fix suggestion
data Fix = Fix
  { fixSpan        :: SrcSpan
  , fixReplacement :: Text
  , fixDescription :: Text
  , fixSafety      :: Safety
  }
```

### Newtypes for Safety

```haskell
newtype Line = Line Int
newtype Column = Column Int
newtype RuleId = RuleId Text
newtype Category = Category Text
```

## Analysis Modes

### Quick Mode (Syntactic Only)

- Parse source files with GHC
- Pattern match on AST
- No type information
- ~10x faster than full mode

### Full Mode (With HIE)

- Load HIE files for type info
- Type-aware pattern matching
- Cross-module analysis
- Symbol resolution

### Plugin Mode (GHC Plugin)

- Run as GHC plugin during compilation
- Access to full type checker state
- Deepest analysis possible
- Incremental updates

## Concurrency Model

```haskell
-- Parallel file analysis
analyzeParallel :: Config -> [FilePath] -> IO [Diagnostic]
analyzeParallel config files = do
  -- Use bounded parallelism
  let workers = configWorkers config
  pooledMapConcurrently workers (analyzeFile config) files
```

Features:
- Bounded thread pool
- Per-file parallelism
- Shared HIE cache
- Lock-free result collection

## Caching Strategy

```haskell
data CacheEntry = CacheEntry
  { cacheHash       :: Hash
  , cacheTimestamp  :: UTCTime
  , cacheDiagnostics :: [Diagnostic]
  }

-- Check cache validity
isCacheValid :: FilePath -> CacheEntry -> IO Bool
isCacheValid path entry = do
  currentHash <- hashFile path
  return (currentHash == cacheHash entry)
```

Cached:
- Parse results
- HIE file index
- Analysis results
- Fix validations

## Extension Points

### Custom Rules

```haskell
-- Define custom rule
myRule :: Rule
myRule = Rule
  { ruleId = "custom/my-rule"
  , ruleCategory = "custom"
  , ruleSeverity = Warning
  , ruleCheck = \mod -> checkPattern myPattern mod
  , ruleFix = Just $ \match -> generateFix match
  }
```

### Custom Output Formats

```haskell
-- Implement Formatter
class Formatter a where
  formatDiagnostics :: [Diagnostic] -> a -> Text
  formatSummary :: AnalysisResult -> a -> Text
```

### Plugin Integration

```haskell
-- GHC plugin interface
plugin :: Plugin
plugin = defaultPlugin
  { typeCheckResultAction = argusTypeCheck
  }
```

## Performance Characteristics

| Operation | Complexity | Notes |
|-----------|------------|-------|
| Parse file | O(n) | n = file size |
| AST match | O(n × p) | p = patterns |
| HIE lookup | O(log m) | m = modules |
| Fix apply | O(n) | Single pass |
| Full analysis | O(n × r) | r = rules |

### Benchmarks

| Project Size | Quick Mode | Full Mode |
|--------------|------------|-----------|
| 1K LOC | 0.1s | 0.5s |
| 10K LOC | 0.5s | 2s |
| 100K LOC | 3s | 15s |
| 1M LOC | 25s | 120s |

## Next Steps

- **[Rule Engine](./rule-engine)**: How rules work
- **[HIE Integration](./hie-integration)**: Type-aware analysis
- **[Fix System](./fix-system)**: Auto-fix architecture
- **[Analysis Passes](./analysis-passes)**: Analysis pipeline details
