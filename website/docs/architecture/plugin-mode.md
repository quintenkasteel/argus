---
sidebar_position: 6
title: Plugin Mode
description: GHC plugin integration for deepest analysis
---

# GHC Plugin Mode

Plugin mode runs Argus as a GHC plugin during compilation, providing the deepest possible analysis with full access to GHC's internal state.

## Overview

```
┌────────────────────────────────────────────────────────────────┐
│                        GHC Pipeline                             │
│                                                                 │
│  Source → Parse → Rename → Typecheck → Desugar → Core → ...   │
│                      │          │                               │
│                      │          │                               │
│               ┌──────┴──────────┴──────┐                       │
│               │     Argus Plugin       │                       │
│               │                        │                       │
│               │  - Full type info      │                       │
│               │  - Constraint solving  │                       │
│               │  - Instance resolution │                       │
│               │  - Core analysis       │                       │
│               └────────────┬───────────┘                       │
│                            │                                    │
│                            ▼                                    │
│                      Diagnostics                                │
└────────────────────────────────────────────────────────────────┘
```

## Enabling Plugin Mode

### Stack Configuration

```yaml
# stack.yaml
ghc-options:
  "$locals":
    - -fplugin=Argus.Plugin
    - -fplugin-opt=Argus.Plugin:config=argus.toml
```

### Package Dependency

```yaml
# package.yaml
dependencies:
  - argus-plugin  # Plugin package
```

### Cabal Configuration

```bash
cabal build --ghc-options="-fplugin=Argus.Plugin"
```

## Plugin Implementation

### Plugin Entry Point

```haskell
module Argus.Plugin (plugin) where

import GHC.Plugins

plugin :: Plugin
plugin = defaultPlugin
  { typeCheckResultAction = argusTypeCheck
  , parsedResultAction = argusParse
  , renamedResultAction = argusRename
  , pluginRecompile = purePlugin
  }

argusTypeCheck :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
argusTypeCheck opts modSum env = do
  -- Run type-aware analysis
  diags <- runAnalysis opts modSum env
  -- Report diagnostics
  mapM_ reportDiagnostic diags
  return env
```

### Hooking Into GHC Phases

```haskell
-- After parsing
argusParse :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
argusParse opts modSum parsed = do
  -- Syntactic analysis with full parser state
  diags <- runSyntacticChecks opts parsed
  mapM_ (liftIO . reportDiag) diags
  return parsed

-- After renaming
argusRename :: [CommandLineOption] -> TcGblEnv -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
argusRename opts env group = do
  -- Analysis with resolved names
  diags <- runRenamedChecks opts env group
  mapM_ reportDiagnostic diags
  return (env, group)

-- After type checking
argusTypeCheck :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
argusTypeCheck opts modSum env = do
  -- Full type-aware analysis
  diags <- runTypedChecks opts modSum env
  mapM_ reportDiagnostic diags
  return env
```

## Plugin-Only Features

### Full Type Information

```haskell
-- Access to GHC's type environment
getTypeEnv :: TcGblEnv -> TypeEnv
getTypeEnv env = tcg_type_env env

-- Query any expression's type
getExprType :: HsExpr GhcTc -> Type
getExprType expr = case expr of
  HsVar _ (L _ id) -> idType id
  HsApp _ fun _ -> funResultType (getExprType fun)
  -- ... full type access
```

### Constraint Information

```haskell
-- Access to unsolved constraints
getWantedConstraints :: TcGblEnv -> WantedConstraints
getWantedConstraints env = tcg_wanteds env

-- Check for ambiguous types
checkAmbiguity :: Type -> TcM [Diagnostic]
checkAmbiguity ty = do
  (_, wanteds) <- captureConstraints $ checkAmbiguousType ty
  return $ map ambiguityDiag (bagToList wanteds)
```

### Instance Resolution

```haskell
-- Check if instance exists
hasInstance :: Class -> [Type] -> TcM Bool
hasInstance cls tys = do
  (matches, _, _) <- matchInstEnv cls tys
  return (not (null matches))

-- Find instance source
findInstanceSource :: Class -> [Type] -> TcM (Maybe SrcSpan)
findInstanceSource cls tys = do
  (matches, _, _) <- matchInstEnv cls tys
  case matches of
    (inst, _) : _ -> return $ Just (getSrcSpan inst)
    _ -> return Nothing
```

### Core-Level Analysis

```haskell
-- After desugaring
argusCorePlugin :: [CommandLineOption] -> ModGuts -> CoreM ModGuts
argusCorePlugin opts guts = do
  -- Analyze Core representation
  let binds = mg_binds guts
  diags <- analyzeCore opts binds
  liftIO $ mapM_ printDiagnostic diags
  return guts

analyzeCore :: [CommandLineOption] -> CoreProgram -> CoreM [Diagnostic]
analyzeCore opts binds = do
  -- Detect space leaks in Core
  leaks <- findSpaceLeaks binds
  -- Detect unnecessary allocations
  allocs <- findUnnecessaryAllocs binds
  return (leaks ++ allocs)
```

## Plugin Analysis Examples

### Detect Missing Instance

```haskell
missingInstanceRule :: TcGblEnv -> TcM [Diagnostic]
missingInstanceRule env = do
  let binds = tcg_binds env
  fmap concat $ forM (bagToList binds) $ \bind -> do
    let exprs = universeBi bind :: [HsExpr GhcTc]
    fmap catMaybes $ forM exprs $ \expr -> do
      case expr of
        HsApp _ (L _ (HsVar _ (L _ fun))) _
          | isClassOp fun -> do
              let ty = idType fun
              inst <- hasInstance (classFromOp fun) [getArgType expr]
              if inst
                then return Nothing
                else return $ Just $ missingInstanceDiag expr
        _ -> return Nothing
```

### Detect Redundant Constraint

```haskell
redundantConstraintRule :: TcGblEnv -> TcM [Diagnostic]
redundantConstraintRule env = do
  let sigs = tcg_sigs env
  fmap concat $ forM (bagToList sigs) $ \sig -> do
    let constraints = sigConstraints sig
        body = sigBody sig
    usedConstraints <- collectUsedConstraints body
    return
      [ redundantConstraintDiag c
      | c <- constraints
      , c `notElem` usedConstraints
      ]
```

### Detect Space Leak in Core

```haskell
spaceLeakInCore :: CoreProgram -> [Diagnostic]
spaceLeakInCore binds =
  [ spaceLeakDiag bind
  | bind <- binds
  , hasAccumulatorThunk bind
  ]

hasAccumulatorThunk :: CoreBind -> Bool
hasAccumulatorThunk (NonRec _ expr) = checkExpr expr
hasAccumulatorThunk (Rec pairs) = any (checkExpr . snd) pairs

checkExpr :: CoreExpr -> Bool
checkExpr expr = case expr of
  -- Detect: let acc = ... in loop acc
  Let (NonRec acc _) body
    | isRecursiveCall body && usesLazily acc body -> True
  _ -> False
```

## Incremental Compilation

### Recompilation Avoidance

```haskell
-- Plugin doesn't force recompilation
pluginRecompile :: [CommandLineOption] -> IO PluginRecompile
pluginRecompile _ = return NoForceRecompile

-- Cache analysis results
argusCaching :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
argusCaching opts modSum env = do
  let modName = ms_mod modSum
  cached <- liftIO $ lookupCache modName
  case cached of
    Just diags -> do
      mapM_ reportDiagnostic diags
      return env
    Nothing -> do
      diags <- runAnalysis opts modSum env
      liftIO $ updateCache modName diags
      mapM_ reportDiagnostic diags
      return env
```

## Configuration

### Plugin Options

```yaml
# stack.yaml
ghc-options:
  "$locals":
    - -fplugin=Argus.Plugin
    - -fplugin-opt=Argus.Plugin:config=argus.toml
    - -fplugin-opt=Argus.Plugin:verbose
    - -fplugin-opt=Argus.Plugin:fail-on=warning
```

### Plugin-Specific Config

```toml
# argus.toml
[plugin]
enabled = true

# Plugin-only rules
enable-core-analysis = true
enable-constraint-analysis = true

# Performance
cache-results = true
cache-dir = ".argus-plugin-cache"

# Reporting
fail-on = "error"
warnings-as-errors = false
```

## Performance

### Compilation Impact

| Analysis Level | Overhead |
|---------------|----------|
| Syntactic only | +5-10% |
| With types | +10-20% |
| With Core | +20-30% |

### Optimization Tips

```toml
[plugin]
# Limit analysis scope
analyze-local-only = true

# Skip generated code
skip-generated = true

# Disable expensive checks
enable-core-analysis = false
```

## Comparison with Other Modes

| Feature | Quick | Full | Plugin |
|---------|-------|------|--------|
| Parse info | ✓ | ✓ | ✓ |
| Type info | - | Via HIE | Direct |
| Constraints | - | - | ✓ |
| Core analysis | - | - | ✓ |
| Incremental | ✓ | ✓ | ✓ |
| Overhead | Low | Medium | Higher |

## Troubleshooting

### Plugin Not Loading

```
Error: Could not find module 'Argus.Plugin'

Ensure argus-plugin is in dependencies:
  dependencies:
    - argus-plugin
```

### Version Mismatch

```
Error: Plugin was compiled with GHC 9.8 but project uses 9.10

Rebuild argus-plugin with matching GHC version
```

### Performance Issues

```bash
# Profile plugin overhead
stack build --ghc-options="-fplugin=Argus.Plugin -dppr-stats"

# Disable expensive analysis
# Add to argus.toml: enable-core-analysis = false
```

## Next Steps

- **[Analysis Passes](./analysis-passes)**: Full pipeline
- **[HIE Integration](./hie-integration)**: Alternative approach
- **[Configuration](../configuration/file-format)**: Plugin settings
