---
sidebar_position: 4
title: Fix System
description: How Argus generates and applies automatic fixes
---

# Fix System Architecture

The Fix System handles automatic code repair, ensuring fixes preserve formatting, handle imports, and maintain code correctness.

## Overview

```
┌────────────────────────────────────────────────────────────┐
│                      Fix System                             │
│                                                             │
│  ┌──────────┐    ┌──────────┐    ┌──────────┐             │
│  │   Fix    │    │  Exact   │    │   Fix    │             │
│  │Generator │───▶│  Print   │───▶│ Applier  │             │
│  └──────────┘    └──────────┘    └──────────┘             │
│        │              │               │                    │
│        ▼              ▼               ▼                    │
│  ┌──────────┐    ┌──────────┐    ┌──────────┐             │
│  │ Pattern  │    │ Format   │    │Validator │             │
│  │Substitut │    │ Preserve │    │          │             │
│  └──────────┘    └──────────┘    └──────────┘             │
│                                       │                    │
│                                       ▼                    │
│                              ┌──────────┐                  │
│                              │  Import  │                  │
│                              │ Manager  │                  │
│                              └──────────┘                  │
└────────────────────────────────────────────────────────────┘
```

## Fix Data Structure

### Core Types

```haskell
data Fix = Fix
  { fixSpan        :: SrcSpan       -- Location to replace
  , fixReplacement :: Text          -- New code
  , fixDescription :: Text          -- Human-readable description
  , fixSafety      :: Safety        -- Safety classification
  , fixImports     :: [ImportAdd]   -- Imports to add
  , fixRemoves     :: [ImportRemove]-- Imports to remove
  }

data Safety
  = Safe         -- Guaranteed semantics preservation
  | MostlySafe   -- Safe in most contexts
  | NeedsReview  -- May change behavior
  | Manual       -- Cannot auto-fix

data ImportAdd = ImportAdd
  { addModule    :: ModuleName
  , addNames     :: [Name]
  , addQualified :: Bool
  , addAs        :: Maybe Text
  }
```

### Fix Generation

```haskell
-- Generate fix from rule match
generateFix :: Rule -> Match -> Maybe Fix
generateFix rule match = do
  -- Get fix template from rule
  template <- ruleFixTemplate rule

  -- Apply matched bindings
  let bindings = matchBindings match
  replacement <- applyTemplate template bindings

  -- Build complete fix
  return Fix
    { fixSpan = matchSpan match
    , fixReplacement = replacement
    , fixDescription = ruleDescription rule
    , fixSafety = ruleSafety rule
    , fixImports = ruleImports rule
    }
```

## Exact Print Engine

The Exact Print engine preserves formatting when modifying code.

### How Exact Print Works

```haskell
-- GHC's exact print annotations
data ApiAnns = ApiAnns
  { apiAnnComments  :: [Comment]      -- Comments
  , apiAnnPragmas   :: [Pragma]       -- Pragmas
  , apiAnnKeywords  :: Map SrcSpan [Located Token]  -- Keywords
  , apiAnnDeltaPos  :: Map SrcSpan DeltaPos  -- Whitespace
  }

-- Apply fix preserving formatting
applyWithExactPrint :: Fix -> ParsedModule -> ParsedModule
applyWithExactPrint fix mod =
  let anns = pm_annotations mod
      -- Find the target node
      target = findNode (fixSpan fix) (pm_parsed_source mod)
      -- Calculate delta positions
      delta = calculateDelta (fixSpan fix) (fixReplacement fix)
      -- Update annotations
      newAnns = adjustAnnotations delta anns
      -- Replace the node
      newAst = replaceNode target (fixReplacement fix) (pm_parsed_source mod)
  in mod { pm_parsed_source = newAst, pm_annotations = newAnns }
```

### Preserving Comments

```haskell
-- Before fix
result = head items  -- Get first item

-- After fix (comment preserved)
result = headMay items  -- Get first item
```

### Preserving Indentation

```haskell
-- Before fix
process =
  let x = head items
  in transform x

-- After fix (indentation preserved)
process =
  let x = headMay items
  in transform x
```

## Pattern Substitution

### Template Syntax

```
$X        - Single expression
$F        - Function
$ARGS...  - Multiple arguments
$_        - Wildcard (discarded)
```

### Substitution Algorithm

```haskell
applyTemplate :: Template -> Bindings -> Maybe Text
applyTemplate template bindings = case template of
  Literal text -> Just text

  Var name -> Map.lookup name bindings

  Concat parts -> do
    resolved <- mapM (`applyTemplate` bindings) parts
    return (Text.concat resolved)

  Conditional cond thenT elseT -> do
    condVal <- applyTemplate cond bindings
    if evalCondition condVal
      then applyTemplate thenT bindings
      else applyTemplate elseT bindings
```

### Example

```haskell
-- Rule: head $X -> headMay $X
-- Match: head (filter p xs)
-- Bindings: { X = "filter p xs" }
-- Template: "headMay $X"
-- Result: "headMay (filter p xs)"
```

## Fix Application Pipeline

### Single File

```haskell
applyFixes :: [Fix] -> FilePath -> IO (Either Error ())
applyFixes fixes path = do
  -- Read source
  source <- readFile path

  -- Parse with exact print annotations
  parsed <- parseWithAnnotations source

  -- Sort fixes (reverse source order)
  let sorted = sortBy (comparing (Down . fixStart)) fixes

  -- Check for conflicts
  case detectConflicts sorted of
    Just conflict -> return $ Left conflict
    Nothing -> do
      -- Apply fixes in order
      let result = foldl' applyFix parsed sorted

      -- Render with exact print
      let output = exactPrint result

      -- Validate
      validated <- validate path output
      if validated
        then writeFile path output >> return (Right ())
        else return $ Left ValidationFailed
```

### Conflict Detection

```haskell
detectConflicts :: [Fix] -> Maybe Conflict
detectConflicts fixes =
  let spans = map fixSpan fixes
  in findOverlapping spans

findOverlapping :: [SrcSpan] -> Maybe Conflict
findOverlapping [] = Nothing
findOverlapping (x:xs) =
  case find (overlaps x) xs of
    Just y -> Just (Conflict x y)
    Nothing -> findOverlapping xs

overlaps :: SrcSpan -> SrcSpan -> Bool
overlaps a b =
  spanStart a < spanEnd b && spanEnd a > spanStart b
```

### Batch Application

```haskell
-- Apply fixes to multiple files
applyBatch :: Map FilePath [Fix] -> IO BatchResult
applyBatch fixesByFile = do
  results <- forM (Map.toList fixesByFile) $ \(path, fixes) -> do
    result <- applyFixes fixes path
    return (path, result)

  return BatchResult
    { brSucceeded = [p | (p, Right _) <- results]
    , brFailed = [(p, e) | (p, Left e) <- results]
    }
```

## Import Management

### Adding Imports

```haskell
addImport :: ImportAdd -> ParsedModule -> ParsedModule
addImport imp mod =
  let imports = pm_imports mod
      newImport = makeImportDecl imp
      -- Find insertion point
      insertAt = findImportInsertPoint imports imp
      -- Insert preserving format
      newImports = insertAt insertAt newImport imports
  in mod { pm_imports = newImports }

makeImportDecl :: ImportAdd -> ImportDecl
makeImportDecl imp = ImportDecl
  { ideclName = mkModuleName (addModule imp)
  , ideclQualified = addQualified imp
  , ideclAs = addAs imp
  , ideclHiding = Just (False, addNames imp)
  }
```

### Import Placement

```haskell
-- Find correct position for new import
findImportInsertPoint :: [ImportDecl] -> ImportAdd -> Int
findImportInsertPoint imports new =
  case importPlacement of
    Alphabetical ->
      -- Insert in alphabetical order
      length $ takeWhile (< addModule new) (map importModule imports)

    EndOfGroup ->
      -- Insert at end of matching group
      case findMatchingGroup imports new of
        Just idx -> idx + 1
        Nothing -> length imports

    End ->
      -- Insert at end of all imports
      length imports
```

### Removing Unused Imports

```haskell
removeUnusedImports :: Fix -> ParsedModule -> ParsedModule
removeUnusedImports fix mod =
  let usedNames = collectUsedNames (pm_parsed_source mod)
      imports = pm_imports mod
      prunedImports = map (pruneImport usedNames) imports
      nonEmptyImports = filter (not . isEmptyImport) prunedImports
  in mod { pm_imports = nonEmptyImports }
```

## Validation

### Type Checking

```haskell
validateFix :: FilePath -> Text -> IO Bool
validateFix path source = do
  -- Write to temp file
  tmpPath <- writeTemp source

  -- Run type checker
  result <- typeCheck tmpPath

  -- Clean up
  removeFile tmpPath

  return $ case result of
    TypeCheckSuccess -> True
    TypeCheckFailure _ -> False
```

### Rollback on Failure

```haskell
applyWithRollback :: Fix -> FilePath -> IO (Either Error ())
applyWithRollback fix path = do
  -- Create backup
  backup <- readFile path

  -- Apply fix
  result <- applyFix fix path

  case result of
    Right () -> do
      -- Validate
      valid <- validateFix path
      if valid
        then return (Right ())
        else do
          -- Rollback
          writeFile path backup
          return $ Left ValidationFailed

    Left err -> do
      writeFile path backup
      return $ Left err
```

## Transaction Support

### Transactional Fixes

```haskell
data FixTransaction = FixTransaction
  { txFixes   :: [(FilePath, [Fix])]
  , txBackups :: IORef (Map FilePath Text)
  }

runTransaction :: FixTransaction -> IO (Either Error ())
runTransaction tx = do
  -- Create all backups
  forM_ (txFixes tx) $ \(path, _) -> do
    backup <- readFile path
    modifyIORef (txBackups tx) (Map.insert path backup)

  -- Apply all fixes
  results <- forM (txFixes tx) $ \(path, fixes) ->
    applyFixes fixes path

  -- Check for failures
  case sequence results of
    Right _ -> do
      -- Validate all
      validations <- forM (map fst (txFixes tx)) validateFix
      if and validations
        then return (Right ())
        else rollbackAll tx >> return (Left ValidationFailed)

    Left err -> do
      rollbackAll tx
      return (Left err)

rollbackAll :: FixTransaction -> IO ()
rollbackAll tx = do
  backups <- readIORef (txBackups tx)
  forM_ (Map.toList backups) $ \(path, content) ->
    writeFile path content
```

## Safety Classification

### Determining Safety

```haskell
classifySafety :: Rule -> Fix -> Safety
classifySafety rule fix =
  case ruleSafety rule of
    -- Rule specifies safety
    Just safety -> safety

    -- Infer from fix characteristics
    Nothing -> inferSafety fix

inferSafety :: Fix -> Safety
inferSafety fix
  | isSimpleRename fix = Safe
  | onlyRemovesCode fix = Safe
  | changesControlFlow fix = NeedsReview
  | changesTypes fix = NeedsReview
  | otherwise = MostlySafe
```

### Safety Examples

| Fix | Safety | Reason |
|-----|--------|--------|
| `id x` → `x` | Safe | Removes redundant code |
| `head` → `headMay` | MostlySafe | Changes return type |
| `foldl` → `foldl'` | MostlySafe | May change evaluation |
| Reorder imports | Safe | No semantic change |
| Add type annotation | Safe | Documentation only |

## Performance

### Benchmarks

| Operation | Time (single file) |
|-----------|--------------------|
| Parse with annotations | 10-50ms |
| Apply single fix | 1-5ms |
| Exact print | 10-30ms |
| Type validation | 100-500ms |

### Optimization: Batch Exact Print

```haskell
-- Apply multiple fixes before printing once
applyMultiple :: [Fix] -> ParsedModule -> ParsedModule
applyMultiple fixes mod =
  let sorted = sortFixes fixes
      applied = foldl' applyFixToAST mod sorted
  in applied  -- Single exact print at the end
```

## Next Steps

- **[Analysis Passes](./analysis-passes)**: Full pipeline
- **[Auto-Fix Guide](../usage-guide/auto-fix)**: Using fixes
- **[argus fix](../cli-reference/fix)**: Fix command
