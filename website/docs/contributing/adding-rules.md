---
sidebar_position: 3
title: Adding Rules
description: How to create new lint rules for Argus
---

# Adding Rules

This guide explains how to add new lint rules to Argus. Rules are the core of Argus's analysis capability.

## Rule Architecture

### Rule Components

```
┌─────────────────────────────────────────────┐
│                   Rule                       │
│                                             │
│  ┌─────────────┐  ┌─────────────────────┐  │
│  │  Metadata   │  │     Matcher         │  │
│  │             │  │                     │  │
│  │  - ID       │  │  - Pattern/AST      │  │
│  │  - Name     │  │  - Condition        │  │
│  │  - Severity │  │  - Context check    │  │
│  │  - Category │  │                     │  │
│  └─────────────┘  └─────────────────────┘  │
│                                             │
│  ┌─────────────────────────────────────┐   │
│  │           Fix Generator              │   │
│  │                                      │   │
│  │  - Replacement text                  │   │
│  │  - Import additions                  │   │
│  │  - Context-aware fix                 │   │
│  └─────────────────────────────────────┘   │
└─────────────────────────────────────────────┘
```

### Rule Types

| Type | Use Case | Implementation |
|------|----------|----------------|
| Pattern | Simple code patterns | DSL-based |
| AST | Complex structure | GHC API |
| Semantic | Type-aware | HIE queries |
| Custom | Arbitrary logic | Full Haskell |

## Creating Pattern Rules

### DSL-Based Rules

The simplest way to add rules:

```haskell
-- src/Argus/Rules/MyRules.hs
module Argus.Rules.MyRules (myRules) where

import Argus.Rules.DSL
import Argus.Types

myRules :: [Rule]
myRules =
  [ patternRule
      { ruleId = "my-category/my-rule"
      , ruleName = "My Rule"
      , ruleDescription = "Detects problematic pattern"
      , ruleSeverity = Warning
      , ruleCategory = "my-category"
      , ruleMatch = "problematicFunction $X"
      , ruleFix = Just "betterFunction $X"
      , ruleMessage = "Use betterFunction instead of problematicFunction"
      }
  ]
```

### Pattern Syntax

```
Pattern         ::= Literal | MetaVar | Application | Other
MetaVar         ::= '$' Name
Application     ::= Pattern Pattern
Wildcard        ::= '_'
ExprWildcard    ::= '$_'

Examples:
  head $X           -- matches: head items, head (foo bar)
  map $F $X         -- matches: map show items
  $F $X $Y          -- matches any binary application
  foldr $F $Z $XS   -- matches foldr with any args
  \$X -> $BODY      -- matches any lambda
```

### Pattern Rule Examples

```haskell
-- Detect 'length xs == 0' pattern
lengthNullRule :: Rule
lengthNullRule = patternRule
  { ruleId = "performance/length-null"
  , ruleName = "Use null instead of length == 0"
  , ruleSeverity = Warning
  , ruleCategory = "performance"
  , ruleMatch = "length $X == 0"
  , ruleFix = Just "null $X"
  , ruleMessage = "Use null for O(1) emptiness check"
  }

-- Detect 'fmap f (pure x)'
fmapPureRule :: Rule
fmapPureRule = patternRule
  { ruleId = "redundant/fmap-pure"
  , ruleName = "Simplify fmap over pure"
  , ruleSeverity = Hint
  , ruleCategory = "redundant"
  , ruleMatch = "fmap $F (pure $X)"
  , ruleFix = Just "pure ($F $X)"
  , ruleMessage = "fmap f (pure x) simplifies to pure (f x)"
  }
```

## Creating AST Rules

### Using GHC API

For complex patterns requiring AST access:

```haskell
-- src/Argus/Rules/ComplexRule.hs
module Argus.Rules.ComplexRule (complexRule) where

import GHC.Hs
import GHC.Plugins (unLoc)
import Argus.Types
import Argus.Rules.ASTMatch

complexRule :: Rule
complexRule = Rule
  { ruleId = "complex/my-rule"
  , ruleName = "Complex Pattern Detection"
  , ruleSeverity = Warning
  , ruleCategory = "complex"
  , ruleMatcher = astMatcher matchComplex
  , ruleFixGen = Just generateFix
  }

matchComplex :: HsExpr GhcPs -> Maybe Match
matchComplex expr = case expr of
  -- Match: case x of { True -> a; False -> b }
  HsCase _ scrutinee mg
    | isBooleanCase mg -> Just $ Match
        { matchSpan = getSpan expr
        , matchBindings = [("scrutinee", scrutinee)]
        }
  _ -> Nothing

isBooleanCase :: MatchGroup GhcPs (LHsExpr GhcPs) -> Bool
isBooleanCase mg =
  let alts = unLoc <$> mg_alts mg
  in length alts == 2 && all isBoolPattern (map m_pats alts)

generateFix :: Match -> Maybe Fix
generateFix match = Just Fix
  { fixReplacement = "if " <> showExpr scrutinee <> " then a else b"
  , fixSpan = matchSpan match
  }
```

### AST Traversal with SYB

```haskell
import Data.Generics (everything, mkQ)

-- Find all occurrences of a pattern
findAllHeadUsages :: HsModule GhcPs -> [SrcSpan]
findAllHeadUsages = everything (++) ([] `mkQ` findHead)
  where
    findHead :: HsExpr GhcPs -> [SrcSpan]
    findHead (HsApp _ (L loc (HsVar _ (L _ name))) _)
      | showName name == "head" = [locToSpan loc]
    findHead _ = []
```

## Creating Semantic Rules

### HIE-Based Rules

For type-aware analysis:

```haskell
-- src/Argus/Rules/SemanticRule.hs
module Argus.Rules.SemanticRule (semanticRule) where

import Argus.HIE.Query
import Argus.HIE.TypeInfo
import Argus.Types

semanticRule :: Rule
semanticRule = Rule
  { ruleId = "semantic/partial-on-list"
  , ruleName = "Partial function on list type"
  , ruleSeverity = Warning
  , ruleCategory = "partial"
  , ruleMatcher = semanticMatcher checkPartialOnList
  , ruleFixGen = Nothing  -- Requires type-aware fix
  }

checkPartialOnList :: HIEContext -> HsExpr GhcPs -> Maybe Match
checkPartialOnList hie expr = case expr of
  HsApp _ (L _ (HsVar _ (L _ name))) arg
    | isPartialFunction name -> do
        -- Get type from HIE
        argType <- getTypeAt hie (getSpan arg)
        -- Check if argument is a list
        if isListType argType
          then Just $ Match (getSpan expr) []
          else Nothing
  _ -> Nothing

isPartialFunction :: RdrName -> Bool
isPartialFunction name = showName name `elem`
  ["head", "tail", "init", "last", "!!"]

isListType :: Type -> Bool
isListType ty = case splitTyConApp_maybe ty of
  Just (tc, _) -> tyConName tc == listTyConName
  Nothing -> False
```

## Rule Registration

### Adding to Rule Engine

```haskell
-- src/Argus/Rules/Engine.hs

import Argus.Rules.MyRules (myRules)

allBuiltinRules :: [Rule]
allBuiltinRules = concat
  [ partialRules
  , securityRules
  , performanceRules
  , myRules  -- Add your rules here
  ]
```

### Category Registration

```haskell
-- src/Argus/Rules/Categories.hs

allCategories :: [Category]
allCategories =
  [ Category "partial" "Partial Functions"
  , Category "security" "Security Issues"
  , Category "my-category" "My Custom Category"  -- Add category
  ]
```

## Writing Rule Tests

### Basic Rule Test

```haskell
-- test/MyRulesSpec.hs
module MyRulesSpec (spec) where

import Test.Hspec
import Argus.Rules.MyRules
import TestUtils

spec :: Spec
spec = do
  describe "my-category/my-rule" $ do
    it "detects problematic pattern" $ do
      let source = "module T where\nf = problematicFunction items"
      diags <- runRule myRule source
      length diags `shouldBe` 1
      diagRule (head diags) `shouldBe` "my-category/my-rule"

    it "suggests correct fix" $ do
      let source = "module T where\nf = problematicFunction items"
      diags <- runRule myRule source
      diagFix (head diags) `shouldBe` Just "betterFunction items"

    it "ignores correct usage" $ do
      let source = "module T where\nf = betterFunction items"
      diags <- runRule myRule source
      diags `shouldBe` []
```

### Property Tests

```haskell
spec :: Spec
spec = do
  describe "my-rule properties" $ do
    prop "fix produces valid Haskell" $ \source ->
      isValidHaskell source ==>
        let diags = runRuleSync myRule source
            fixed = applyFixes diags source
        in isValidHaskell fixed

    prop "rule is idempotent" $ \source ->
      let diags1 = runRuleSync myRule source
          fixed1 = applyFixes diags1 source
          diags2 = runRuleSync myRule fixed1
      in null diags2
```

## Rule Guidelines

### Correctness

1. **No false positives**: Only flag real issues
2. **Sound fixes**: Fixes must preserve semantics
3. **Handle edge cases**: Empty lists, nested patterns

### Performance

1. **Efficient matching**: Avoid O(n²) traversals
2. **Lazy evaluation**: Don't force unnecessary work
3. **Pattern order**: Check cheaper conditions first

### User Experience

1. **Clear messages**: Explain why it's a problem
2. **Actionable fixes**: Provide working replacements
3. **Good diagnostics**: Include context

### Testing

1. **Positive cases**: Rule triggers correctly
2. **Negative cases**: Rule doesn't over-trigger
3. **Fix correctness**: Applied fixes work
4. **Edge cases**: Empty, nested, complex

## Rule Template

```haskell
-- src/Argus/Rules/NewRule.hs
module Argus.Rules.NewRule (newRule) where

import Argus.Rules.DSL
import Argus.Types

newRule :: Rule
newRule = patternRule
  { ruleId = "category/rule-name"
  , ruleName = "Human-readable name"
  , ruleDescription = "Detailed description of what this rule detects"
  , ruleSeverity = Warning
  , ruleCategory = "category"
  , ruleMatch = "pattern $X"
  , ruleFix = Just "replacement $X"
  , ruleMessage = "Brief message shown to user"
  , ruleRationale = "Why this pattern is problematic"
  , ruleExamples =
      [ Example "bad code" "good code"
      ]
  }
```

## Next Steps

- **[Testing Guide](./testing-guide)**: Write comprehensive tests
- **[Code Style](./code-style)**: Follow conventions
- **[Pull Requests](./pull-requests)**: Submit your rule
