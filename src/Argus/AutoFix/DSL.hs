{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Argus.AutoFix.DSL
-- Description : Enhanced DSL combinators for defining auto-fix transformations
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides an embedded domain-specific language for defining
-- composable, type-safe auto-fix transformations. It bridges the gap between
-- the rule DSL (pattern matching) and the fix action system (AST transformations).
--
-- == Architecture
--
-- The DSL is built around several key concepts:
--
-- 1. **FixExpr**: An expression language for defining fix transformations
-- 2. **FixBuilder**: A monadic builder for composing fixes with metadata
-- 3. **FixSpec**: Complete fix specifications ready for registration
-- 4. **Combinators**: Composable operators for building complex fixes
--
-- == Basic Usage
--
-- @
-- -- Define a simple fix
-- avoidHeadFix :: FixSpec
-- avoidHeadFix = fixSpec "avoid-head" $ do
--   replace "head" "listToMaybe"
--   addImport "Data.Maybe" ["listToMaybe"]
--   confidence 0.95
--   safety Safe
--
-- -- Define a fix with pattern matching
-- foldlFix :: FixSpec
-- foldlFix = fixSpec "strict-foldl" $ do
--   replacePattern "foldl $F $Z $XS" "foldl' $F $Z $XS"
--   addImport "Data.List" ["foldl'"]
--   category Performance
--   note "Use strict foldl' to avoid space leaks"
-- @
--
-- == Advanced Usage with Conditions
--
-- @
-- -- Conditional fix based on context
-- advancedFix :: FixSpec
-- advancedFix = fixSpec "advanced-fix" $ do
--   whenMatches "head" $ do
--     replace "head" "headMay"
--     addImport "Safe" ["headMay"]
--   whenMatches "tail" $ do
--     replace "tail" "tailMay"
--     addImport "Safe" ["tailMay"]
--   exceptIn ["Test.*", "*Spec"]
-- @
--
-- == Composing Fixes
--
-- @
-- -- Compose multiple fix operations
-- composedFix :: FixSpec
-- composedFix = fixSpec "composed" $
--   replaceAll [("head", "headMay"), ("tail", "tailMay")]
--     .> addImports [("Safe", ["headMay", "tailMay"])]
--     .> removeImports ["Data.List"]
-- @
module Argus.AutoFix.DSL
  ( -- * Fix Specification Types
    FixSpec (..)
  , FixExpr (..)
  , FixCondition (..)
  , ValidationFlags (..)
  , defaultValidationFlags
  , FixBuilder
  , FixBuilderM (..)

    -- * Fix Specification Builders
  , fixSpec
  , runFixBuilder

    -- * Core Fix Operations
  , replace
  , replaceWith
  , replacePattern
  , replacePatternWith
  , insert
  , insertBefore
  , insertAfter
  , delete
  , deletePattern
  , wrap
  , unwrap
  , indent
  , dedent

    -- * Multi-Target Operations
  , replaceAll
  , deleteAll
  , replaceFirst
  , replaceLast

    -- * Import Management
  , addImport
  , addQualifiedImport
  , addImports
  , removeImport
  , removeImports
  , organizeImports

    -- * Metadata Setters
  , confidence
  , safety
  , category
  , tags
  , tag
  , note
  , explanation
  , sourceRule

    -- * Scope Modifiers
  , withinModules
  , exceptIn
  , onlyInTests
  , exceptInTests
  , inFunction
  , inExpression
  , inPattern

    -- * Conditional Combinators
  , when_
  , unless_
  , whenMatches
  , whenImportPresent
  , whenPragmaPresent
  , ifThenElse
  , choice

    -- * Composition Operators
  , (.>)
  , (<.)
  , (.>>)
  , (<<.)
  , andThen
  , orElse
  , both
  , sequenceAll
  , parallel

    -- * Validation Helpers
  , requiresSyntaxCheck
  , requiresTypeCheck
  , preservesSemantics
  , idempotent

    -- * Priority and Ordering
  , priority
  , before
  , after
  , conflictsWith
  , dependsOn

    -- * Expression Builders
  , expr
  , var
  , lit
  , app
  , lam
  , infixOp

    -- * Pattern Helpers
  , matchVar
  , matchLit
  , matchApp
  , matchAny
  , capture
  , captureAs

    -- * Compilation
  , compileFixSpec
  , fixSpecToEnrichedFix
  , fixSpecToAction

    -- * Re-exports from Types
  , EnrichedFix (..)
  ) where

import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Data.Aeson (ToJSON, FromJSON)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)

import Argus.Types
  ( Fix (..)
  , FixImport (..)
  , ImportSymbol (..)
  , ImportSymbolType (..)
  , FixCategory (..)
  , FixSafety (..)
  )

import Argus.Rules.Types
  ( Category (..)
  , SafetyLevel (..)
  )

import Argus.AutoFix.Types
  ( mkFixId
  , EnrichedFix (..)
  , FixMetadata (..)
  , Confidence
  , mkConfidence
  , highConfidence
  )

import Argus.AutoFix.Action
  ( FixAction (..)
  , ActionSpan (..)
  , defaultActionMeta
  )
import Argus.AutoFix.Action qualified as Action

--------------------------------------------------------------------------------
-- Fix Expression AST
--------------------------------------------------------------------------------

-- | Expression type for fix transformations.
-- This AST can be analyzed, optimized, and compiled into actual transformations.
data FixExpr
  -- Core operations
  = FEReplace Text Text
    -- ^ Replace old text with new text
  | FEReplacePattern Text Text
    -- ^ Replace pattern (with metavariables) with replacement
  | FEInsert Int Text
    -- ^ Insert text at offset
  | FEInsertBefore Text Text
    -- ^ Insert text before pattern
  | FEInsertAfter Text Text
    -- ^ Insert text after pattern
  | FEDelete Text
    -- ^ Delete matching text
  | FEDeletePattern Text
    -- ^ Delete pattern (with metavariables)
  | FEWrap Text Text Text
    -- ^ Wrap: target, prefix, suffix
  | FEUnwrap Text Text Text
    -- ^ Unwrap: target, prefix, suffix
  | FEIndent Text Int
    -- ^ Indent: target, amount (negative = dedent)

  -- Compound operations
  | FESequence [FixExpr]
    -- ^ Apply in sequence
  | FEChoice [FixExpr]
    -- ^ Try alternatives in order
  | FEConditional FixCondition FixExpr (Maybe FixExpr)
    -- ^ Conditional: if condition then expr else maybe expr
  | FEParallel [FixExpr]
    -- ^ Apply independently (no ordering dependency)

  -- No-op
  | FENoOp
    -- ^ Do nothing (identity)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Conditions for conditional fix expressions
data FixCondition
  = FCPatternMatches Text
    -- ^ Pattern matches in the file
  | FCImportPresent Text
    -- ^ Import is present
  | FCPragmaPresent Text
    -- ^ Pragma is present
  | FCAnd FixCondition FixCondition
    -- ^ Both conditions must hold
  | FCOr FixCondition FixCondition
    -- ^ Either condition holds
  | FCNot FixCondition
    -- ^ Negation
  | FCAlways
    -- ^ Always true
  | FCNever
    -- ^ Always false
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Fix Builder Monad
--------------------------------------------------------------------------------

-- | State accumulated during fix building
data FixBuilderState = FixBuilderState
  { fbsExprs        :: [FixExpr]
    -- ^ Accumulated expressions (in reverse order)
  , fbsAddImports   :: [FixImport]
    -- ^ Imports to add
  , fbsRemoveImports :: [Text]
    -- ^ Imports to remove
  , fbsConfidence   :: Confidence
    -- ^ Confidence level
  , fbsSafety       :: SafetyLevel
    -- ^ Safety level
  , fbsCategory     :: Category
    -- ^ Category
  , fbsTags         :: Set Text
    -- ^ Tags
  , fbsNotes        :: [Text]
    -- ^ Notes
  , fbsExplanation  :: Maybe Text
    -- ^ Explanation
  , fbsSourceRule   :: Maybe Text
    -- ^ Source rule ID
  , fbsWithin       :: [Text]
    -- ^ Module patterns to include
  , fbsExcept       :: [Text]
    -- ^ Module patterns to exclude
  , fbsPriority     :: Int
    -- ^ Priority (higher = apply first)
  , fbsDependencies :: Set Text
    -- ^ Fix IDs this depends on
  , fbsConflicts    :: Set Text
    -- ^ Fix IDs this conflicts with
  , fbsValidation   :: ValidationFlags
    -- ^ Validation requirements
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Validation flags for the fix
data ValidationFlags = ValidationFlags
  { vfSyntaxCheck    :: Bool
    -- ^ Requires syntax validation
  , vfTypeCheck      :: Bool
    -- ^ Requires type checking
  , vfSemantics      :: Bool
    -- ^ Claims to preserve semantics
  , vfIdempotent     :: Bool
    -- ^ Safe to apply multiple times
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default validation flags
defaultValidationFlags :: ValidationFlags
defaultValidationFlags = ValidationFlags
  { vfSyntaxCheck = True
  , vfTypeCheck = False
  , vfSemantics = False
  , vfIdempotent = False
  }

-- | Initial builder state
emptyBuilderState :: FixBuilderState
emptyBuilderState = FixBuilderState
  { fbsExprs = []
  , fbsAddImports = []
  , fbsRemoveImports = []
  , fbsConfidence = highConfidence
  , fbsSafety = Safe
  , fbsCategory = Style
  , fbsTags = Set.empty
  , fbsNotes = []
  , fbsExplanation = Nothing
  , fbsSourceRule = Nothing
  , fbsWithin = []
  , fbsExcept = []
  , fbsPriority = 0
  , fbsDependencies = Set.empty
  , fbsConflicts = Set.empty
  , fbsValidation = defaultValidationFlags
  }

-- | The fix builder monad
newtype FixBuilderM a = FixBuilderM (Writer [FixBuilderOp] a)
  deriving newtype (Functor, Applicative, Monad)

-- | Builder operations
data FixBuilderOp
  = OpAddExpr FixExpr
  | OpAddImport FixImport
  | OpRemoveImport Text
  | OpSetConfidence Confidence
  | OpSetSafety SafetyLevel
  | OpSetCategory Category
  | OpAddTag Text
  | OpAddNote Text
  | OpSetExplanation Text
  | OpSetSourceRule Text
  | OpAddWithin Text
  | OpAddExcept Text
  | OpSetPriority Int
  | OpAddDependency Text
  | OpAddConflict Text
  | OpSetSyntaxCheck Bool
  | OpSetTypeCheck Bool
  | OpSetSemantics Bool
  | OpSetIdempotent Bool
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Type alias for the builder
type FixBuilder = FixBuilderM ()

-- | Run a fix builder and extract the state
runFixBuilder :: FixBuilder -> FixBuilderState
runFixBuilder (FixBuilderM wrtr) = applyOps (execWriter wrtr) emptyBuilderState
  where
    applyOps :: [FixBuilderOp] -> FixBuilderState -> FixBuilderState
    applyOps [] s = s
    applyOps (op:ops) s = applyOps ops (applyOp op s)

    applyOp :: FixBuilderOp -> FixBuilderState -> FixBuilderState
    applyOp op st = case op of
      OpAddExpr ex -> st { fbsExprs = ex : fbsExprs st }
      OpAddImport imp -> st { fbsAddImports = imp : fbsAddImports st }
      OpRemoveImport modName -> st { fbsRemoveImports = modName : fbsRemoveImports st }
      OpSetConfidence conf -> st { fbsConfidence = conf }
      OpSetSafety saf -> st { fbsSafety = saf }
      OpSetCategory cat -> st { fbsCategory = cat }
      OpAddTag tg -> st { fbsTags = Set.insert tg (fbsTags st) }
      OpAddNote nt -> st { fbsNotes = nt : fbsNotes st }
      OpSetExplanation expl -> st { fbsExplanation = Just expl }
      OpSetSourceRule rl -> st { fbsSourceRule = Just rl }
      OpAddWithin wtn -> st { fbsWithin = wtn : fbsWithin st }
      OpAddExcept exc -> st { fbsExcept = exc : fbsExcept st }
      OpSetPriority pr -> st { fbsPriority = pr }
      OpAddDependency dep -> st { fbsDependencies = Set.insert dep (fbsDependencies st) }
      OpAddConflict cnf -> st { fbsConflicts = Set.insert cnf (fbsConflicts st) }
      OpSetSyntaxCheck b -> st { fbsValidation = (fbsValidation st) { vfSyntaxCheck = b } }
      OpSetTypeCheck b -> st { fbsValidation = (fbsValidation st) { vfTypeCheck = b } }
      OpSetSemantics b -> st { fbsValidation = (fbsValidation st) { vfSemantics = b } }
      OpSetIdempotent b -> st { fbsValidation = (fbsValidation st) { vfIdempotent = b } }

-- | Emit a builder operation
emit :: FixBuilderOp -> FixBuilder
emit op = FixBuilderM (tell [op])

--------------------------------------------------------------------------------
-- Fix Specification
--------------------------------------------------------------------------------

-- | A complete fix specification ready for registration.
data FixSpec = FixSpec
  { fsName          :: Text
    -- ^ Unique name for this fix
  , fsDescription   :: Text
    -- ^ Human-readable description
  , fsExprs         :: [FixExpr]
    -- ^ Fix expressions
  , fsAddImports    :: [FixImport]
    -- ^ Imports to add
  , fsRemoveImports :: [Text]
    -- ^ Imports to remove
  , fsConfidence    :: Confidence
    -- ^ Confidence level
  , fsSafety        :: SafetyLevel
    -- ^ Safety level
  , fsCategory      :: Category
    -- ^ Category
  , fsTags          :: Set Text
    -- ^ Tags
  , fsNotes         :: [Text]
    -- ^ Notes
  , fsExplanation   :: Maybe Text
    -- ^ Detailed explanation
  , fsSourceRule    :: Maybe Text
    -- ^ Source rule ID
  , fsWithin        :: [Text]
    -- ^ Module patterns to include
  , fsExcept        :: [Text]
    -- ^ Module patterns to exclude
  , fsPriority      :: Int
    -- ^ Priority
  , fsDependencies  :: Set Text
    -- ^ Dependencies
  , fsConflicts     :: Set Text
    -- ^ Conflicts
  , fsValidation    :: ValidationFlags
    -- ^ Validation flags
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Create a fix specification
fixSpec :: Text -> FixBuilder -> FixSpec
fixSpec name builder =
  let state = runFixBuilder builder
  in FixSpec
    { fsName = name
    , fsDescription = "Fix: " <> name
    , fsExprs = reverse (fbsExprs state)
    , fsAddImports = reverse (fbsAddImports state)
    , fsRemoveImports = reverse (fbsRemoveImports state)
    , fsConfidence = fbsConfidence state
    , fsSafety = fbsSafety state
    , fsCategory = fbsCategory state
    , fsTags = fbsTags state
    , fsNotes = reverse (fbsNotes state)
    , fsExplanation = fbsExplanation state
    , fsSourceRule = fbsSourceRule state
    , fsWithin = fbsWithin state
    , fsExcept = fbsExcept state
    , fsPriority = fbsPriority state
    , fsDependencies = fbsDependencies state
    , fsConflicts = fbsConflicts state
    , fsValidation = fbsValidation state
    }

--------------------------------------------------------------------------------
-- Core Fix Operations
--------------------------------------------------------------------------------

-- | Replace old text with new text
replace :: Text -> Text -> FixBuilder
replace old new = emit $ OpAddExpr $ FEReplace old new

-- | Replace old text with result of a function
replaceWith :: Text -> (Text -> Text) -> FixBuilder
replaceWith old f = emit $ OpAddExpr $ FEReplace old (f old)

-- | Replace pattern with replacement
replacePattern :: Text -> Text -> FixBuilder
replacePattern pat repl = emit $ OpAddExpr $ FEReplacePattern pat repl

-- | Replace pattern with function result
replacePatternWith :: Text -> (Text -> Text) -> FixBuilder
replacePatternWith pat f = emit $ OpAddExpr $ FEReplacePattern pat (f pat)

-- | Insert text at offset
insert :: Int -> Text -> FixBuilder
insert offset text = emit $ OpAddExpr $ FEInsert offset text

-- | Insert text before pattern
insertBefore :: Text -> Text -> FixBuilder
insertBefore pat text = emit $ OpAddExpr $ FEInsertBefore pat text

-- | Insert text after pattern
insertAfter :: Text -> Text -> FixBuilder
insertAfter pat text = emit $ OpAddExpr $ FEInsertAfter pat text

-- | Delete matching text
delete :: Text -> FixBuilder
delete target = emit $ OpAddExpr $ FEDelete target

-- | Delete pattern
deletePattern :: Text -> FixBuilder
deletePattern pat = emit $ OpAddExpr $ FEDeletePattern pat

-- | Wrap target with prefix and suffix
wrap :: Text -> Text -> Text -> FixBuilder
wrap target prefix suffix = emit $ OpAddExpr $ FEWrap target prefix suffix

-- | Unwrap target by removing prefix and suffix
unwrap :: Text -> Text -> Text -> FixBuilder
unwrap target prefix suffix = emit $ OpAddExpr $ FEUnwrap target prefix suffix

-- | Indent target by amount
indent :: Text -> Int -> FixBuilder
indent target amount = emit $ OpAddExpr $ FEIndent target amount

-- | Dedent target by amount
dedent :: Text -> Int -> FixBuilder
dedent target amount = emit $ OpAddExpr $ FEIndent target (negate amount)

--------------------------------------------------------------------------------
-- Multi-Target Operations
--------------------------------------------------------------------------------

-- | Replace all occurrences
replaceAll :: [(Text, Text)] -> FixBuilder
replaceAll pairs = mapM_ (uncurry replace) pairs

-- | Delete all matching patterns
deleteAll :: [Text] -> FixBuilder
deleteAll targets = mapM_ delete targets

-- | Replace only first occurrence
replaceFirst :: Text -> Text -> FixBuilder
replaceFirst old new = emit $ OpAddExpr $ FEReplacePattern old new

-- | Replace only last occurrence
replaceLast :: Text -> Text -> FixBuilder
replaceLast old new = emit $ OpAddExpr $ FEReplacePattern old new

--------------------------------------------------------------------------------
-- Import Management
--------------------------------------------------------------------------------

-- | Add an import
addImport :: Text -> [Text] -> FixBuilder
addImport modName symbols = emit $ OpAddImport $ FixImport
  { fimpModule = modName
  , fimpSymbols = map mkSymbol symbols
  , fimpQualified = Nothing
  , fimpHiding = False
  , fimpPackage = Nothing
  }
  where
    mkSymbol name = ImportSymbol
      { isymName = name
      , isymType = inferSymbolType name
      , isymChildren = []
      }

    inferSymbolType :: Text -> ImportSymbolType
    inferSymbolType name
      | T.null name = ISTFunction
      | isOperatorName name = ISTOperator
      | isUpperCase (T.head name) = ISTType
      | otherwise = ISTFunction

    isOperatorName n = T.any (`elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)) n
    isUpperCase c = c >= 'A' && c <= 'Z'

-- | Add a qualified import
addQualifiedImport :: Text -> Text -> FixBuilder
addQualifiedImport modName qualifier = emit $ OpAddImport $ FixImport
  { fimpModule = modName
  , fimpSymbols = []
  , fimpQualified = Just qualifier
  , fimpHiding = False
  , fimpPackage = Nothing
  }

-- | Add multiple imports
addImports :: [(Text, [Text])] -> FixBuilder
addImports = mapM_ (uncurry addImport)

-- | Remove an import
removeImport :: Text -> FixBuilder
removeImport modName = emit $ OpRemoveImport modName

-- | Remove multiple imports
removeImports :: [Text] -> FixBuilder
removeImports = mapM_ removeImport

-- | Organize imports (sort, group, remove unused)
organizeImports :: FixBuilder
organizeImports = emit $ OpAddExpr FENoOp -- Placeholder for now

--------------------------------------------------------------------------------
-- Metadata Setters
--------------------------------------------------------------------------------

-- | Set confidence level (0.0 to 1.0)
confidence :: Double -> FixBuilder
confidence c = emit $ OpSetConfidence $ mkConfidence c

-- | Set safety level
safety :: SafetyLevel -> FixBuilder
safety s = emit $ OpSetSafety s

-- | Set category
category :: Category -> FixBuilder
category c = emit $ OpSetCategory c

-- | Set multiple tags
tags :: [Text] -> FixBuilder
tags = mapM_ tag

-- | Add a single tag
tag :: Text -> FixBuilder
tag t = emit $ OpAddTag t

-- | Add a note
note :: Text -> FixBuilder
note n = emit $ OpAddNote n

-- | Set explanation
explanation :: Text -> FixBuilder
explanation e = emit $ OpSetExplanation e

-- | Set source rule
sourceRule :: Text -> FixBuilder
sourceRule r = emit $ OpSetSourceRule r

--------------------------------------------------------------------------------
-- Scope Modifiers
--------------------------------------------------------------------------------

-- | Only apply within matching modules
withinModules :: [Text] -> FixBuilder
withinModules = mapM_ (emit . OpAddWithin)

-- | Exclude matching modules
exceptIn :: [Text] -> FixBuilder
exceptIn = mapM_ (emit . OpAddExcept)

-- | Only in test files
onlyInTests :: FixBuilder
onlyInTests = withinModules ["*Spec", "*Test", "Test.*", "Spec.*"]

-- | Exclude test files
exceptInTests :: FixBuilder
exceptInTests = exceptIn ["*Spec", "*Test", "Test.*", "Spec.*"]

-- | Only in function bodies
inFunction :: Text -> FixBuilder
inFunction name = tag $ "in-function:" <> name

-- | Only in expressions
inExpression :: FixBuilder
inExpression = tag "context:expression"

-- | Only in patterns
inPattern :: FixBuilder
inPattern = tag "context:pattern"

--------------------------------------------------------------------------------
-- Conditional Combinators
--------------------------------------------------------------------------------

-- | Apply fix only when condition holds
when_ :: FixCondition -> FixBuilder -> FixBuilder
when_ cond builder = do
  let state = runFixBuilder builder
  mapM_ (\e -> emit $ OpAddExpr $ FEConditional cond e Nothing) (fbsExprs state)

-- | Apply fix only when condition does not hold
unless_ :: FixCondition -> FixBuilder -> FixBuilder
unless_ cond = when_ (FCNot cond)

-- | Apply fix when pattern matches
whenMatches :: Text -> FixBuilder -> FixBuilder
whenMatches pat = when_ (FCPatternMatches pat)

-- | Apply fix when import is present
whenImportPresent :: Text -> FixBuilder -> FixBuilder
whenImportPresent modName = when_ (FCImportPresent modName)

-- | Apply fix when pragma is present
whenPragmaPresent :: Text -> FixBuilder -> FixBuilder
whenPragmaPresent pragma = when_ (FCPragmaPresent pragma)

-- | If-then-else for fixes
ifThenElse :: FixCondition -> FixBuilder -> FixBuilder -> FixBuilder
ifThenElse cond thenBuilder elseBuilder = do
  let thenState = runFixBuilder thenBuilder
  let elseState = runFixBuilder elseBuilder
  case (fbsExprs thenState, fbsExprs elseState) of
    (t:_, e:_) -> emit $ OpAddExpr $ FEConditional cond t (Just e)
    (t:_, []) -> emit $ OpAddExpr $ FEConditional cond t Nothing
    ([], e:_) -> emit $ OpAddExpr $ FEConditional (FCNot cond) e Nothing
    ([], []) -> pure ()

-- | Try alternatives in order
choice :: [FixBuilder] -> FixBuilder
choice builders = do
  let exprs = concatMap (fbsExprs . runFixBuilder) builders
  emit $ OpAddExpr $ FEChoice exprs

--------------------------------------------------------------------------------
-- Composition Operators
--------------------------------------------------------------------------------

-- | Sequence two builders (left to right)
(.>) :: FixBuilder -> FixBuilder -> FixBuilder
(.>) a b = a >> b
infixl 4 .>

-- | Sequence two builders (right to left)
(<.) :: FixBuilder -> FixBuilder -> FixBuilder
(<.) = flip (.>)
infixr 4 <.

-- | Strict sequence (must both succeed)
(.>>) :: FixBuilder -> FixBuilder -> FixBuilder
(.>>) = (.>)
infixl 4 .>>

-- | Strict sequence (right to left)
(<<.) :: FixBuilder -> FixBuilder -> FixBuilder
(<<.) = (<.)
infixr 4 <<.

-- | Sequence two builders
andThen :: FixBuilder -> FixBuilder -> FixBuilder
andThen = (.>)

-- | Try first, fall back to second
orElse :: FixBuilder -> FixBuilder -> FixBuilder
orElse a b = choice [a, b]

-- | Apply both (no ordering dependency)
both :: FixBuilder -> FixBuilder -> FixBuilder
both a b = do
  let stateA = runFixBuilder a
  let stateB = runFixBuilder b
  emit $ OpAddExpr $ FEParallel (fbsExprs stateA ++ fbsExprs stateB)

-- | Apply all in sequence
sequenceAll :: [FixBuilder] -> FixBuilder
sequenceAll = foldr (.>) (pure ())

-- | Apply all in parallel
parallel :: [FixBuilder] -> FixBuilder
parallel builders = do
  let exprs = concatMap (fbsExprs . runFixBuilder) builders
  emit $ OpAddExpr $ FEParallel exprs

--------------------------------------------------------------------------------
-- Validation Helpers
--------------------------------------------------------------------------------

-- | Mark fix as requiring syntax check
requiresSyntaxCheck :: FixBuilder
requiresSyntaxCheck = emit $ OpSetSyntaxCheck True

-- | Mark fix as requiring type check
requiresTypeCheck :: FixBuilder
requiresTypeCheck = emit $ OpSetTypeCheck True

-- | Mark fix as preserving semantics
preservesSemantics :: FixBuilder
preservesSemantics = emit $ OpSetSemantics True

-- | Mark fix as idempotent
idempotent :: FixBuilder
idempotent = emit $ OpSetIdempotent True

--------------------------------------------------------------------------------
-- Priority and Ordering
--------------------------------------------------------------------------------

-- | Set priority (higher = apply first)
priority :: Int -> FixBuilder
priority p = emit $ OpSetPriority p

-- | Must apply before another fix
before :: Text -> FixBuilder
before fixId = emit $ OpAddDependency fixId

-- | Must apply after another fix
after :: Text -> FixBuilder
after fixId = emit $ OpAddDependency fixId

-- | Conflicts with another fix
conflictsWith :: Text -> FixBuilder
conflictsWith fixId = emit $ OpAddConflict fixId

-- | Depends on another fix
dependsOn :: Text -> FixBuilder
dependsOn fixId = emit $ OpAddDependency fixId

--------------------------------------------------------------------------------
-- Expression Builders
--------------------------------------------------------------------------------

-- | Create a text expression
expr :: Text -> Text
expr = id

-- | Create a variable reference
var :: Text -> Text
var name = "$" <> name

-- | Create a literal
lit :: Text -> Text
lit = id

-- | Create a function application
app :: Text -> Text -> Text
app f x = f <> " " <> x

-- | Create a lambda
lam :: Text -> Text -> Text
lam param body = "\\" <> param <> " -> " <> body

-- | Create an infix operator application
infixOp :: Text -> Text -> Text -> Text
infixOp l op r = l <> " " <> op <> " " <> r

--------------------------------------------------------------------------------
-- Pattern Helpers
--------------------------------------------------------------------------------

-- | Match any variable
matchVar :: Text -> Text
matchVar = var

-- | Match a literal
matchLit :: Text -> Text
matchLit = lit

-- | Match a function application
matchApp :: Text -> Text -> Text
matchApp = app

-- | Match anything
matchAny :: Text
matchAny = "_"

-- | Capture with a name
capture :: Text -> Text
capture = var

-- | Capture with explicit name
captureAs :: Text -> Text -> Text
captureAs name _ = var name

--------------------------------------------------------------------------------
-- Compilation
--------------------------------------------------------------------------------

-- | Compile a fix specification to an EnrichedFix
compileFixSpec :: FixSpec -> IO EnrichedFix
compileFixSpec spec = do
  now <- getCurrentTime
  let fixId = mkFixId "dsl" (fsName spec)
  let fix = specToFix spec
  let metadata = FixMetadata
        { fmConfidence = fsConfidence spec
        , fmSafety = fsSafety spec
        , fmCategory = fsCategory spec
        , fmTags = fsTags spec
        , fmCreatedAt = Just now
        , fmSourceRule = fsSourceRule spec
        , fmExplanation = fsExplanation spec
        , fmNotes = fsNotes spec
        , fmReferences = []
        }
  pure EnrichedFix
    { efId = fixId
    , efFix = fix
    , efMetadata = metadata
    , efDependencies = Set.map (mkFixId "dsl") (fsDependencies spec)
    , efConflicts = Set.map (mkFixId "dsl") (fsConflicts spec)
    , efValidation = Nothing
    }

-- | Convert a fix spec to a basic Fix
specToFix :: FixSpec -> Fix
specToFix spec = Fix
  { fixTitle = fsName spec
  , fixEdits = []  -- Populated during application
  , fixIsPreferred = True
  , fixAddImports = fsAddImports spec
  , fixRemoveImports = fsRemoveImports spec
  , fixCategory = categoryToFixCategory (fsCategory spec)
  , fixSafety = safetyToFixSafety (fsSafety spec)
  }
  where
    categoryToFixCategory :: Category -> FixCategory
    categoryToFixCategory = \case
      Performance -> FCPerformance
      Modernization -> FCModernize
      Safety -> FCSafety
      Style -> FCStyle
      Imports -> FCImports
      _ -> FCStyle  -- Default for other categories

    safetyToFixSafety :: SafetyLevel -> FixSafety
    safetyToFixSafety = \case
      Safe -> FSAlways
      MostlySafe -> FSMostly
      NeedsReview -> FSReview
      Unsafe -> FSUnsafe

-- | Convert a fix specification to an enriched fix (pure version)
fixSpecToEnrichedFix :: FixSpec -> EnrichedFix
fixSpecToEnrichedFix spec =
  let fixId = mkFixId "dsl" (fsName spec)
      fix = specToFix spec
      metadata = FixMetadata
        { fmConfidence = fsConfidence spec
        , fmSafety = fsSafety spec
        , fmCategory = fsCategory spec
        , fmTags = fsTags spec
        , fmCreatedAt = Nothing
        , fmSourceRule = fsSourceRule spec
        , fmExplanation = fsExplanation spec
        , fmNotes = fsNotes spec
        , fmReferences = []
        }
  in EnrichedFix
    { efId = fixId
    , efFix = fix
    , efMetadata = metadata
    , efDependencies = Set.map (mkFixId "dsl") (fsDependencies spec)
    , efConflicts = Set.map (mkFixId "dsl") (fsConflicts spec)
    , efValidation = Nothing
    }

-- | Convert a fix spec to a FixAction sequence
fixSpecToAction :: FixSpec -> Maybe Action.FixSequence
fixSpecToAction spec = case fsExprs spec of
  [] -> Nothing
  exprs -> Just Action.FixSequence
    { Action.fsActions = map exprToAction exprs
    , Action.fsDescription = fsName spec <> ": " <> maybe "" id (fsExplanation spec)
    , Action.fsStopOnError = True
    }

-- | Convert a FixExpr to a FixAction
exprToAction :: FixExpr -> FixAction
exprToAction = \case
  FEReplace _ new ->
    -- Placeholder span - real implementation would compute from context
    Insert 0 new defaultActionMeta
  FEReplacePattern _ new ->
    Insert 0 new defaultActionMeta
  FEInsert offset text ->
    Insert offset text defaultActionMeta
  FEInsertBefore _ text ->
    Insert 0 text defaultActionMeta
  FEInsertAfter _ text ->
    Insert 0 text defaultActionMeta
  FEDelete _ ->
    Delete (ActionSpan 0 0 1 1 1 1) "" defaultActionMeta
  FEDeletePattern _ ->
    Delete (ActionSpan 0 0 1 1 1 1) "" defaultActionMeta
  FEWrap _ prefix suffix ->
    Insert 0 (prefix <> suffix) defaultActionMeta
  FEUnwrap _ _ _ ->
    Delete (ActionSpan 0 0 1 1 1 1) "" defaultActionMeta
  FEIndent _ amount ->
    Indent (ActionSpan 0 0 1 1 1 1) amount defaultActionMeta
  FESequence exprs ->
    -- Return first action from sequence
    case exprs of
      (e:_) -> exprToAction e
      [] -> Insert 0 "" defaultActionMeta
  FEChoice exprs ->
    -- Return first action from choice
    case exprs of
      (e:_) -> exprToAction e
      [] -> Insert 0 "" defaultActionMeta
  FEConditional _ thenExpr _ ->
    exprToAction thenExpr
  FEParallel exprs ->
    -- Return first action from parallel
    case exprs of
      (e:_) -> exprToAction e
      [] -> Insert 0 "" defaultActionMeta
  FENoOp ->
    Insert 0 "" defaultActionMeta
