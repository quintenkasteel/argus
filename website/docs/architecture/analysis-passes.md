---
sidebar_position: 5
title: Analysis Passes
description: The complete analysis pipeline from source to diagnostics
---

# Analysis Passes

Argus processes source code through multiple analysis passes, each building on the results of previous passes. This document details the complete pipeline.

## Pipeline Overview

```
Source File (.hs)
      │
      ▼
┌─────────────────┐
│   1. Parse      │  GHC parser → AST + annotations
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  2. Normalize   │  Desugar, resolve names
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  3. Syntactic   │  Pattern matching on AST
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  4. HIE Load    │  Load type information (optional)
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  5. Semantic    │  Type-aware analysis
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  6. Cross-Mod   │  Multi-module analysis
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  7. Aggregate   │  Collect and deduplicate
└────────┬────────┘
         │
         ▼
    Diagnostics
```

## Pass 1: Parsing

### GHC Parser Integration

```haskell
parseFile :: FilePath -> IO (Either ParseError ParsedModule)
parseFile path = do
  content <- readFile path
  let opts = defaultParserOpts
        { pExtensions = enabledExtensions
        , pExactPrint = True
        }
  return $ runParser opts content

data ParsedModule = ParsedModule
  { pm_mod_name    :: ModuleName
  , pm_parsed_source :: HsModule GhcPs
  , pm_annotations  :: ApiAnns
  , pm_pragmas      :: [Pragma]
  , pm_imports      :: [ImportDecl]
  }
```

### Extension Handling

```haskell
-- Detect and enable required extensions
enableExtensions :: Text -> [Extension]
enableExtensions source =
  let pragmas = extractPragmas source
      explicit = concatMap pragmaExtensions pragmas
      implicit = inferExtensions source
  in nub (explicit ++ implicit)

inferExtensions :: Text -> [Extension]
inferExtensions source
  | "\\case" `isInfixOf` source = [LambdaCase]
  | "{..}" `isInfixOf` source = [RecordWildCards]
  | otherwise = []
```

### Error Recovery

```haskell
-- Parse with error recovery for partial analysis
parseWithRecovery :: FilePath -> IO (ParsedModule, [ParseError])
parseWithRecovery path = do
  result <- parseFile path
  case result of
    Right mod -> return (mod, [])
    Left err -> do
      -- Try to recover partial parse
      partial <- recoverPartialParse path err
      return (partial, [err])
```

## Pass 2: Normalization

### AST Normalization

```haskell
normalize :: ParsedModule -> NormalizedModule
normalize pm = NormalizedModule
  { nm_decls = normalizeDecls (pm_parsed_source pm)
  , nm_imports = normalizeImports (pm_imports pm)
  , nm_exports = extractExports (pm_parsed_source pm)
  , nm_source = pm
  }

normalizeDecls :: HsModule GhcPs -> [NormalizedDecl]
normalizeDecls mod = map normalizeDecl (hsmodDecls mod)

normalizeDecl :: HsDecl GhcPs -> NormalizedDecl
normalizeDecl decl = case decl of
  ValD _ bind -> normalizeBinding bind
  SigD _ sig -> normalizeSignature sig
  TyClD _ tycl -> normalizeTypeDecl tycl
  _ -> OtherDecl decl
```

### Name Resolution (Local)

```haskell
-- Build local scope for name resolution
buildLocalScope :: NormalizedModule -> LocalScope
buildLocalScope nm =
  let topLevel = extractTopLevelNames nm
      imported = extractImportedNames (nm_imports nm)
  in LocalScope
       { lsTopLevel = topLevel
       , lsImported = imported
       , lsInScope = topLevel `Map.union` imported
       }
```

## Pass 3: Syntactic Analysis

### Pattern-Based Rules

```haskell
syntacticPass :: NormalizedModule -> Config -> [Diagnostic]
syntacticPass nm config =
  let rules = syntacticRules config
      patterns = map rulePattern rules
      matches = traverseWithPatterns patterns (nm_source nm)
  in concatMap (matchToDiagnostic rules) matches

traverseWithPatterns :: [CompiledPattern] -> ParsedModule -> [Match]
traverseWithPatterns patterns mod =
  everything (++) ([] `mkQ` checkExpr) (pm_parsed_source mod)
  where
    checkExpr expr =
      [ Match (getSpan expr) bindings pattern
      | pattern <- patterns
      , Just bindings <- [matchPattern pattern expr]
      ]
```

### Rules in Syntactic Pass

| Category | Rules | Example |
|----------|-------|---------|
| Partial | 50+ | `head xs` |
| Redundant | 35+ | `id x`, `map id` |
| Modernize | 30+ | `return x` |
| Performance | 20+ | `length xs == 0` |

### AST Traversal Strategies

```haskell
-- Top-down traversal
topDown :: Data a => (forall b. Data b => b -> b) -> a -> a
topDown f = gmapT (topDown f) . f

-- Bottom-up traversal
bottomUp :: Data a => (forall b. Data b => b -> b) -> a -> a
bottomUp f = f . gmapT (bottomUp f)

-- Query traversal (collect results)
everything :: Data a => (r -> r -> r) -> GenericQ r -> a -> r
everything k f x = foldl k (f x) (gmapQ (everything k f) x)
```

## Pass 4: HIE Loading

### Conditional Loading

```haskell
loadHIE :: Config -> FilePath -> IO (Maybe HIEContext)
loadHIE config path
  | configMode config == Quick = return Nothing
  | otherwise = do
      let hieDir = configHIEDir config
      let hiePath = hiePathFor hieDir path
      exists <- doesFileExist hiePath
      if exists
        then Just <$> loadAndIndex hiePath
        else return Nothing
```

### HIE Context Building

```haskell
loadAndIndex :: FilePath -> IO HIEContext
loadAndIndex hiePath = do
  hie <- readHieFile hiePath
  return HIEContext
    { hcFile = hie
    , hcTypes = buildTypeIndex hie
    , hcRefs = buildRefIndex hie
    , hcScopes = buildScopeIndex hie
    }

buildTypeIndex :: HieFile -> TypeIndex
buildTypeIndex hie =
  Map.fromList
    [ (nodeSpan node, types)
    | node <- toList (hie_asts hie)
    , let types = mapMaybe (lookupType hie) (nodeType (nodeInfo node))
    ]
```

## Pass 5: Semantic Analysis

### Type-Aware Rules

```haskell
semanticPass :: NormalizedModule -> HIEContext -> Config -> [Diagnostic]
semanticPass nm hie config =
  let rules = semanticRules config
      ctx = SemanticContext
        { scModule = nm
        , scHIE = hie
        , scConfig = config
        }
  in concatMap (evaluateSemanticRule ctx) rules

evaluateSemanticRule :: SemanticContext -> Rule -> [Diagnostic]
evaluateSemanticRule ctx rule =
  let checker = ruleSemanticChecker rule
      matches = checker ctx
  in map (toDiagnostic rule) matches
```

### Semantic Checks

```haskell
-- Check for String where Text expected
stringTextCheck :: SemanticContext -> [Match]
stringTextCheck ctx =
  [ Match span ()
  | (span, ty) <- Map.toList (hcTypes (scHIE ctx))
  , isStringType ty
  , expectedText <- getExpectedType span ctx
  , isTextType expectedText
  ]

-- Check for missing Eq instance
eqCheck :: SemanticContext -> [Match]
eqCheck ctx =
  [ Match span ()
  | (span, ty) <- Map.toList (hcTypes (scHIE ctx))
  , usesEq span ctx
  , not (hasEqInstance ty ctx)
  ]
```

### Scope-Aware Analysis

```haskell
-- Detect shadowing
shadowingCheck :: SemanticContext -> [Match]
shadowingCheck ctx =
  [ Match span ()
  | binding <- localBindings ctx
  , let name = bindingName binding
  , let outerScope = getOuterScope binding ctx
  , name `elem` outerScope
  ]
```

## Pass 6: Cross-Module Analysis

### Dependency Graph

```haskell
crossModulePass :: [NormalizedModule] -> HIEIndex -> Config -> [Diagnostic]
crossModulePass modules idx config =
  let depGraph = buildDependencyGraph modules idx
      circularDeps = findCircularDependencies depGraph
      unusedExports = findUnusedExports modules idx
      orphanInstances = findOrphanInstances modules idx
  in concat
       [ map circularDiag circularDeps
       , map unusedExportDiag unusedExports
       , map orphanDiag orphanInstances
       ]
```

### Unused Export Detection

```haskell
findUnusedExports :: [NormalizedModule] -> HIEIndex -> [(ModuleName, Name)]
findUnusedExports modules idx =
  [ (mod, export)
  | nm <- modules
  , let mod = nm_mod_name nm
  , export <- nm_exports nm
  , not (isUsedElsewhere export idx)
  , not (isEntryPoint export)
  ]

isUsedElsewhere :: Name -> HIEIndex -> Bool
isUsedElsewhere name idx =
  let refs = findReferences name idx
      defMod = nameModule name
  in any ((/= defMod) . spanModule) refs
```

### Architecture Checks

```haskell
architectureChecks :: DependencyGraph -> Config -> [Diagnostic]
architectureChecks graph config =
  let layerViolations = checkLayerViolations graph (configLayers config)
      cyclicDeps = findCycles graph
      highCoupling = findHighCoupling graph (configCouplingThreshold config)
  in concat [layerViolations, cyclicDeps, highCoupling]

checkLayerViolations :: DependencyGraph -> [Layer] -> [Diagnostic]
checkLayerViolations graph layers =
  [ Diagnostic span Error (layerViolationMsg from to)
  | (from, to) <- edges graph
  , Just fromLayer <- [findLayer from layers]
  , Just toLayer <- [findLayer to layers]
  , not (canDepend fromLayer toLayer)
  ]
```

## Pass 7: Aggregation

### Collecting Results

```haskell
aggregatePass :: [[Diagnostic]] -> [Diagnostic]
aggregatePass passes =
  let all = concat passes
      deduped = deduplicate all
      sorted = sortBy diagOrdering deduped
      filtered = filterBySeverity sorted
  in filtered

deduplicate :: [Diagnostic] -> [Diagnostic]
deduplicate diags =
  let grouped = groupBy sameIssue diags
  in map head grouped

sameIssue :: Diagnostic -> Diagnostic -> Bool
sameIssue a b =
  diagSpan a == diagSpan b &&
  diagRule a == diagRule b
```

### Priority Resolution

```haskell
-- When multiple rules match same location
resolvePriority :: [Diagnostic] -> Diagnostic
resolvePriority diags =
  -- Prefer higher severity
  maximumBy (comparing diagSeverity) diags
```

### Suppression Handling

```haskell
-- Remove suppressed diagnostics
filterSuppressed :: [Diagnostic] -> ParsedModule -> [Diagnostic]
filterSuppressed diags mod =
  let suppressions = extractSuppressions mod
  in filter (not . isSuppressed suppressions) diags

extractSuppressions :: ParsedModule -> [Suppression]
extractSuppressions mod =
  [ Suppression rule span
  | comment <- moduleComments mod
  , Just rule <- [parseSuppressionComment comment]
  , let span = commentSpan comment
  ]
```

## Parallel Execution

### File-Level Parallelism

```haskell
analyzeParallel :: Config -> [FilePath] -> IO [Diagnostic]
analyzeParallel config files = do
  -- Create worker pool
  let workers = configWorkers config
  pool <- createPool workers

  -- Analyze files in parallel
  results <- pooledMapConcurrently pool (analyzeFile config) files

  -- Aggregate (sequential)
  return $ aggregatePass results
```

### Pass-Level Parallelism

```haskell
-- Run independent passes in parallel
analyzeFile :: Config -> FilePath -> IO [Diagnostic]
analyzeFile config path = do
  -- Parse (sequential, needed by all)
  parsed <- parseFile path
  normalized <- normalize parsed

  -- Run syntactic and HIE load in parallel
  (syntacticDiags, hieCtx) <- concurrently
    (return $ syntacticPass normalized config)
    (loadHIE config path)

  -- Semantic depends on HIE
  semanticDiags <- case hieCtx of
    Nothing -> return []
    Just ctx -> return $ semanticPass normalized ctx config

  return $ aggregatePass [syntacticDiags, semanticDiags]
```

## Incremental Analysis

### Change Detection

```haskell
incrementalAnalyze :: Config -> [FilePath] -> Cache -> IO ([Diagnostic], Cache)
incrementalAnalyze config files cache = do
  -- Find changed files
  changed <- filterM (hasChanged cache) files

  -- Analyze only changed files
  newDiags <- analyzeParallel config changed

  -- Merge with cached results
  let cachedDiags = getCachedDiags cache (files \\ changed)
  let allDiags = aggregatePass [newDiags, cachedDiags]

  -- Update cache
  newCache <- updateCache cache changed newDiags

  return (allDiags, newCache)

hasChanged :: Cache -> FilePath -> IO Bool
hasChanged cache path = do
  currentHash <- hashFile path
  let cachedHash = getCachedHash cache path
  return (currentHash /= cachedHash)
```

## Performance Characteristics

### Time Complexity by Pass

| Pass | Complexity | Typical Time |
|------|------------|--------------|
| Parse | O(n) | 10-50ms |
| Normalize | O(n) | 5-20ms |
| Syntactic | O(n × p) | 20-100ms |
| HIE Load | O(1) | 10-50ms |
| Semantic | O(n × r) | 50-200ms |
| Cross-Mod | O(m²) | 100-500ms |
| Aggregate | O(d log d) | 1-10ms |

Where:
- n = file size
- p = number of patterns
- r = number of semantic rules
- m = number of modules
- d = number of diagnostics

## Next Steps

- **[Rule Engine](./rule-engine)**: Rule implementation
- **[HIE Integration](./hie-integration)**: Type-aware analysis
- **[Performance](../faq/performance)**: Optimization tips
