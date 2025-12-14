---
sidebar_position: 2
title: Rule Engine
description: How Argus evaluates and matches rules
---

# Rule Engine Architecture

The Rule Engine is the core component that evaluates rules against source code. This document explains how rules are defined, matched, and executed.

## Overview

```
┌─────────────────────────────────────────────────────────────┐
│                      Rule Engine                             │
│                                                              │
│  ┌───────────┐    ┌───────────┐    ┌───────────┐           │
│  │   Rule    │    │  Matcher  │    │ Evaluator │           │
│  │  Registry │───▶│           │───▶│           │           │
│  └───────────┘    └───────────┘    └───────────┘           │
│        │                │                │                   │
│        ▼                ▼                ▼                   │
│  ┌───────────┐    ┌───────────┐    ┌───────────┐           │
│  │  Builtin  │    │    AST    │    │Diagnostic │           │
│  │   Rules   │    │  Patterns │    │  Builder  │           │
│  └───────────┘    └───────────┘    └───────────┘           │
│        │                                                     │
│        ▼                                                     │
│  ┌───────────┐                                              │
│  │  Custom   │                                              │
│  │   Rules   │                                              │
│  └───────────┘                                              │
└─────────────────────────────────────────────────────────────┘
```

## Rule Definition

### Rule Structure

```haskell
data Rule = Rule
  { ruleId          :: RuleId
  , ruleName        :: Text
  , ruleCategory    :: Category
  , ruleDescription :: Text
  , ruleSeverity    :: Severity
  , ruleChecker     :: RuleChecker
  , ruleFixer       :: Maybe FixGenerator
  , ruleConditions  :: [Condition]
  , ruleEnabled     :: Bool
  }

type RuleChecker = ParsedModule -> HIEContext -> [Match]
type FixGenerator = Match -> Maybe Fix
```

### Rule Categories

```haskell
data Category
  = Partial         -- Partial function usage
  | Security        -- Security vulnerabilities
  | Performance     -- Performance issues
  | SpaceLeak       -- Memory leaks
  | Complexity      -- Code complexity
  | Architecture    -- Structural issues
  | Import          -- Import problems
  | Naming          -- Naming conventions
  | Extension       -- LANGUAGE pragmas
  | Modernize       -- Modernization
  | Redundant       -- Redundant code
  | Custom Text     -- User-defined
```

## Rule Registry

### Builtin Rules

All builtin rules are registered at startup:

```haskell
builtinRules :: [Rule]
builtinRules = concat
  [ partialRules        -- 50+ rules
  , securityRules       -- 30+ rules
  , performanceRules    -- 40+ rules
  , spaceLeakRules      -- 20+ rules
  , complexityRules     -- 15+ rules
  , architectureRules   -- 25+ rules
  , importRules         -- 15+ rules
  , namingRules         -- 20+ rules
  , extensionRules      -- 25+ rules
  , modernizeRules      -- 30+ rules
  , redundantRules      -- 35+ rules
  ]
-- Total: 300+ builtin rules
```

### Custom Rules

Custom rules from configuration:

```haskell
loadCustomRules :: Config -> IO [Rule]
loadCustomRules config = do
  patterns <- parsePatternRules (configPatterns config)
  restrictions <- parseRestrictions (configRestrictions config)
  return (patterns ++ restrictions)
```

### Rule Resolution

```haskell
resolveRules :: Config -> [Rule]
resolveRules config =
  let allRules = builtinRules ++ customRules
      enabled = filter (isEnabled config) allRules
      scoped = applyScopedConfig config enabled
  in scoped
```

## Pattern Matching

### AST Patterns

Patterns are AST templates with metavariables:

```haskell
data Pattern
  = Exact HsExpr              -- Exact match
  | MetaVar Text              -- $X, $F, etc.
  | Wildcard                  -- $_, ___
  | App Pattern Pattern       -- Function application
  | InfixOp Text Pattern Pattern  -- Infix operator
  | Lambda [Pattern] Pattern  -- Lambda expression
  | Case Pattern [(Pattern, Pattern)]  -- Case expression
```

### Pattern Compilation

Patterns are compiled for efficient matching:

```haskell
compilePattern :: Text -> CompiledPattern
compilePattern text =
  let parsed = parsePattern text
      normalized = normalizePattern parsed
      indexed = indexPattern normalized
  in CompiledPattern
       { cpPattern = normalized
       , cpIndex = indexed
       , cpHash = hashPattern normalized
       }
```

### Pattern Matching Algorithm

```haskell
matchPattern :: CompiledPattern -> HsExpr -> Maybe Bindings
matchPattern cp expr = case (cpPattern cp, expr) of
  -- Metavariable: bind and succeed
  (MetaVar name, _) ->
    Just (singleBinding name expr)

  -- Wildcard: always succeeds
  (Wildcard, _) ->
    Just emptyBindings

  -- Exact match: compare AST nodes
  (Exact template, target)
    | equivalent template target -> Just emptyBindings
    | otherwise -> Nothing

  -- Application: match recursively
  (App pf pa, HsApp _ ef ea) -> do
    bf <- matchPattern (compilePattern pf) ef
    ba <- matchPattern (compilePattern pa) ea
    mergeBindings bf ba

  -- Other patterns...
  _ -> Nothing
```

### SYB Traversal

AST traversal uses Scrap Your Boilerplate:

```haskell
findMatches :: Data a => CompiledPattern -> a -> [Match]
findMatches pattern ast =
  everything (++) ([] `mkQ` checkExpr) ast
  where
    checkExpr :: HsExpr GhcPs -> [Match]
    checkExpr expr =
      case matchPattern pattern expr of
        Just bindings -> [Match (getSpan expr) bindings]
        Nothing -> []
```

## Rule Evaluation

### Evaluation Context

```haskell
data EvalContext = EvalContext
  { ecModule     :: ParsedModule
  , ecHIE        :: Maybe HIEContext
  , ecConfig     :: Config
  , ecScope      :: ScopeConfig
  , ecBindings   :: Map Text Type  -- Type bindings if available
  }
```

### Single Rule Evaluation

```haskell
evaluateRule :: Rule -> EvalContext -> [Diagnostic]
evaluateRule rule ctx
  | not (ruleEnabled rule) = []
  | not (checkConditions rule ctx) = []
  | otherwise =
      let matches = ruleChecker rule (ecModule ctx) (ecHIE ctx)
          diagnostics = map (toDiagnostic rule) matches
          withFixes = map (attachFix rule) diagnostics
      in withFixes
```

### Batch Evaluation

```haskell
evaluateRules :: [Rule] -> EvalContext -> [Diagnostic]
evaluateRules rules ctx =
  -- Group rules by pattern for efficiency
  let grouped = groupByPattern rules
      results = map (evaluateGroup ctx) grouped
  in concat results

evaluateGroup :: EvalContext -> [Rule] -> [Diagnostic]
evaluateGroup ctx rules =
  -- Single AST traversal for multiple patterns
  let patterns = map rulePattern rules
      matches = traverseWithPatterns patterns (ecModule ctx)
  in concatMap (matchToDiagnostics rules) matches
```

## Condition System

### Condition Types

```haskell
data Condition
  = HasType Type              -- Expression has type
  | HasTypeclass Typeclass    -- Type has instance
  | InModule ModulePattern    -- In matching module
  | NotInModule ModulePattern
  | InFunction FunctionName
  | HasImport ImportPattern
  | HasPragma Extension
  | HasExtension Extension
  | Custom (EvalContext -> Bool)
```

### Condition Evaluation

```haskell
checkConditions :: Rule -> EvalContext -> Bool
checkConditions rule ctx =
  all (evalCondition ctx) (ruleConditions rule)

evalCondition :: EvalContext -> Condition -> Bool
evalCondition ctx cond = case cond of
  HasType ty ->
    case ecHIE ctx of
      Nothing -> True  -- Skip if no HIE
      Just hie -> checkHasType hie ty

  InModule pat ->
    matchModulePattern pat (ecModule ctx)

  HasImport pat ->
    any (matchImport pat) (moduleImports (ecModule ctx))

  -- Other conditions...
```

## Type-Aware Matching

### With HIE Context

```haskell
data HIEContext = HIEContext
  { hieFile    :: HieFile
  , hieTypes   :: TypeIndex
  , hieRefs    :: RefIndex
  , hieScopes  :: ScopeIndex
  }

-- Type-aware pattern matching
matchWithType :: Pattern -> Type -> HsExpr -> HIEContext -> Maybe Bindings
matchWithType pattern expectedType expr hie =
  case matchPattern pattern expr of
    Nothing -> Nothing
    Just bindings -> do
      -- Verify type constraint
      actualType <- lookupType (getSpan expr) hie
      guard (unifies expectedType actualType)
      return bindings
```

### Type Queries

```haskell
-- Get type at location
lookupType :: SrcSpan -> HIEContext -> Maybe Type
lookupType span hie =
  case lookupSpan span (hieTypes hie) of
    Nothing -> Nothing
    Just types -> Just (head types)

-- Check if types unify
unifies :: Type -> Type -> Bool
unifies expected actual =
  case runUnify expected actual of
    Left _ -> False
    Right _ -> True
```

## Fix Generation

### Fix Structure

```haskell
data Fix = Fix
  { fixSpan        :: SrcSpan
  , fixReplacement :: Text
  , fixDescription :: Text
  , fixSafety      :: Safety
  , fixImports     :: [Import]  -- Imports to add
  }

data Safety
  = Safe         -- Always preserves semantics
  | MostlySafe   -- Safe in most cases
  | NeedsReview  -- May change semantics
  | Manual       -- No auto-fix
```

### Fix Generation from Match

```haskell
generateFix :: Rule -> Match -> Maybe Fix
generateFix rule match = do
  -- Get fix template from rule
  template <- ruleFixer rule
  -- Apply bindings to template
  replacement <- applyBindings (matchBindings match) template
  return Fix
    { fixSpan = matchSpan match
    , fixReplacement = replacement
    , fixDescription = ruleDescription rule
    , fixSafety = ruleSafety rule
    , fixImports = ruleRequiredImports rule
    }
```

## Performance Optimizations

### Pattern Indexing

```haskell
-- Index patterns by root constructor
data PatternIndex = PatternIndex
  { idxApp      :: [CompiledPattern]  -- HsApp patterns
  , idxVar      :: [CompiledPattern]  -- HsVar patterns
  , idxLit      :: [CompiledPattern]  -- HsLit patterns
  , idxLam      :: [CompiledPattern]  -- Lambda patterns
  , idxCase     :: [CompiledPattern]  -- Case patterns
  , idxOther    :: [CompiledPattern]  -- Unindexed
  }

-- Fast lookup by constructor
lookupPatterns :: HsExpr -> PatternIndex -> [CompiledPattern]
lookupPatterns expr idx = case expr of
  HsApp{}   -> idxApp idx
  HsVar{}   -> idxVar idx
  HsLit{}   -> idxLit idx
  HsLam{}   -> idxLam idx
  HsCase{}  -> idxCase idx
  _         -> idxOther idx
```

### Caching

```haskell
-- Cache compiled patterns
patternCache :: IORef (Map Text CompiledPattern)

getCachedPattern :: Text -> IO CompiledPattern
getCachedPattern text = do
  cache <- readIORef patternCache
  case Map.lookup text cache of
    Just cp -> return cp
    Nothing -> do
      let cp = compilePattern text
      modifyIORef patternCache (Map.insert text cp)
      return cp
```

### Early Termination

```haskell
-- Stop on first error if configured
evaluateWithLimit :: Config -> [Rule] -> EvalContext -> [Diagnostic]
evaluateWithLimit config rules ctx
  | configStopOnError config =
      takeWhileInclusive (not . isError) (evaluateRules rules ctx)
  | otherwise =
      evaluateRules rules ctx
```

## Rule Debugging

### Debug Output

```haskell
-- Enable rule debugging
debugRule :: Rule -> EvalContext -> IO [Diagnostic]
debugRule rule ctx = do
  putStrLn $ "Evaluating rule: " ++ show (ruleId rule)
  putStrLn $ "Pattern: " ++ show (rulePattern rule)

  let matches = findMatches (rulePattern rule) (ecModule ctx)
  putStrLn $ "Matches found: " ++ show (length matches)

  forM_ matches $ \match -> do
    putStrLn $ "  At: " ++ show (matchSpan match)
    putStrLn $ "  Bindings: " ++ show (matchBindings match)

  return $ evaluateRule rule ctx
```

### Pattern Testing

```bash
# Test pattern matching
argus debug-pattern "head $X" src/Handler.hs

# Show match details
argus debug-pattern --verbose "head $X" src/
```

## Next Steps

- **[AST Matching](./ast-matching)**: Pattern matching details
- **[HIE Integration](./hie-integration)**: Type-aware analysis
- **[Custom Rules](../rules/custom-rules)**: Writing rules
