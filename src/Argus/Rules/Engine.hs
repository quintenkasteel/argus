{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Rules.Engine
-- Description : Unified rule evaluation engine
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
-- Portability : GHC
--
-- = Overview
--
-- This module provides the unified rule evaluation engine that evaluates "Argus.Rules.Types.Rule"
-- values against source code to produce "Argus.Types.Diagnostic" values.
--
-- = Architecture
--
-- The engine works directly with the unified "Argus.Rules.Types.Rule" type:
--
-- @
-- Rule → evaluateRules → [Diagnostic]
-- @
--
-- __Evaluation Pipeline__:
--
-- 1. Build @RuleEvalContext@ from engine, file path, and content
-- 2. Partition rules into text-based and AST-based
-- 3. For each text\/regex rule: match pattern against each line
-- 4. For each AST rule: match pattern against parsed AST
-- 5. Evaluate side conditions for each match
-- 6. Generate "Argus.Types.Diagnostic" with optional "Argus.Types.Fix" for successful matches
--
-- = Matching Strategies
--
-- * __Text patterns__: Simple text with metavariables (@$X@, @$F@)
-- * __Regex patterns__: Full regex support via TDFA
-- * __AST patterns__: Structural matching on parsed Haskell AST
--
-- = Comment Awareness
--
-- The engine can skip matches inside comments (@reCommentAware@):
--
-- * Code-targeting rules automatically exclude comments
-- * Documentation rules target Haddock comments specifically
-- * Rules can specify explicit targets via @RuleTarget@
--
-- = Parallelism
--
-- Multiple parallel strategies are available:
--
-- * @ParallelRules@: Evaluate rules concurrently (good for many rules)
-- * @ParallelLines@: Evaluate lines concurrently (good for large files)
-- * @ParallelBoth@: Maximum parallelism (both rules and lines)
--
-- = Usage
--
-- @
-- -- Basic usage
-- let engine = defaultEngine
--     diagnostics = evaluateRules engine filepath moduleName content
--
-- -- With AST support (IO)
-- diagnostics <- evaluateRulesIO engine filepath moduleName content
--
-- -- With parallelism
-- diagnostics <- evaluateRulesParallelIO ParallelRules engine filepath moduleName content
-- @
--
-- @since 1.0.0
module Argus.Rules.Engine
  ( -- * Core Engine Types
    RuleEngine (..)
  , MatchContext (..)
  , RuleMatch (..)

    -- * Unified Evaluation Infrastructure
  , RuleEvalContext (..)
  , mkRuleEvalContext
  , evaluateTextRulesWithContext
  , evaluateTextRulesParallelWithContext
  , evaluateRulesWithASTAndContext
  , evaluateRulesParallelWithASTAndContext

    -- * Side Condition Evaluation
  , evalSideCondition
  , evalSideConditionIO

    -- * HIE Context
  , HIEContext
  , mkHIEContext
  , emptyHIEContext

    -- * Engine Construction
  , mkRuleEngine
  , defaultEngine

    -- * Rule Evaluation
  , evaluateRules
  , evaluateRulesWithAST
  , evaluateRulesIO
  , evaluateRule
  , matchToDiagnostic

    -- * Parallel Rule Evaluation
  , evaluateRulesParallel
  , evaluateRulesParallelIO
  , ParallelStrategy (..)
  , defaultParallelStrategy

    -- * Text Substitution
  , interpolateMessage

    -- * Comment Handling (via Argus.Analysis.Comments)
  , CommentIndex
  , Comments.extractComments
  , Comments.buildCommentIndex
  , Comments.isInComment

    -- * Re-exports from unified types
  , Rule (..)
  , Category (..)
  , SafetyLevel (..)
  , SideCondition (..)
  , RulePattern (..)
  , ImportSpec (..)
  ) where

import Control.DeepSeq (NFData(..))
import Control.Parallel.Strategies (parMap, rseq)
import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Regex.TDFA ((=~))

-- DList for O(1) list concatenation during accumulation
import Argus.Internal.DList (DList)
import Argus.Internal.DList qualified as DL

import Argus.Types (Severity(..), Diagnostic(..), DiagnosticKind(..), Fix(..), FixEdit(..), SrcSpan, mkSrcSpanRaw)
import Argus.Types qualified as AT
import Argus.Rules.Types
    ( Rule(..), Category(..), SafetyLevel(..), SideCondition(..)
    , RulePattern(..), ImportSpec(..), RuleTarget(..)
    , CommentType(..)
    , categoryToFixCategory, safetyToFixSafety, importSpecToFixImport
    , allCategories, effectiveTarget
    )
import Argus.Rules.Types qualified as RT
import Argus.Rules.Builtin qualified as Builtin
import Argus.Analysis.Comments qualified as Comments
import Argus.Analysis.Comments (CommentIndex, Position(..))
import Argus.Analysis.Comments qualified as CT (CommentType(..))
import Argus.Analysis.Syntactic qualified as Syntactic
import Argus.Rules.ASTMatch qualified as AST
import Argus.Rules.ASTMatch (HsModule, GhcPs)
import Argus.Rules.SideConditions qualified as SC
import Argus.Rules.SideConditions (HIEContext)
import Argus.Rules.SideConditionHelpers qualified as SCH

--------------------------------------------------------------------------------
-- Core Engine Types
--------------------------------------------------------------------------------

-- | The unified rule evaluation engine.
--
-- Holds the collection of rules and configuration for rule evaluation.
-- Use 'mkRuleEngine' or 'defaultEngine' for construction.
--
-- __Configuration Options__:
--
-- * 'reEnabledCategories': Filter rules by category
-- * 'reDisabledRules': Explicitly disable specific rule IDs
-- * 'reOverrides': Override severity for specific rules
-- * 'reCommentAware': Skip matches inside comments (recommended)
--
-- __Example__:
--
-- @
-- -- Custom engine with only security and performance rules
-- let engine = (mkRuleEngine myRules)
--       { reEnabledCategories = Set.fromList [Security, Performance]
--       , reCommentAware = True
--       }
-- @
--
-- @since 1.0.0
data RuleEngine = RuleEngine
  { reRules             :: [Rule]
    -- ^ All loaded rules. Includes builtin rules and user-defined rules.
  , reEnabledCategories :: Set Category
    -- ^ Categories that are enabled. Rules in disabled categories are skipped.
  , reDisabledRules     :: Set Text
    -- ^ Explicitly disabled rule IDs. Takes precedence over category enable.
  , reOverrides         :: Map Text Severity
    -- ^ Per-rule severity overrides. Key is rule ID.
  , reCommentAware      :: Bool
    -- ^ If 'True', code-targeting rules skip matches inside comments.
  }
  deriving stock (Show, Generic)

-- | Context available during pattern matching.
--
-- Provides all information needed to evaluate a match, including
-- file metadata, source content, and captured metavariables.
--
-- Created internally by the engine; not typically constructed directly.
--
-- @since 1.0.0
data MatchContext = MatchContext
  { mcFilePath      :: FilePath
    -- ^ Path to the source file being analyzed.
  , mcModuleName    :: Text
    -- ^ Haskell module name (e.g., @\"Argus.Rules.Engine\"@).
  , mcLineNumber    :: Int
    -- ^ Current line number (1-indexed).
  , mcLineText      :: Text
    -- ^ Full text of the current line.
  , mcFullContent   :: Text
    -- ^ Complete file content (for cross-line analysis).
  , mcCommentIndex  :: CommentIndex
    -- ^ Pre-built comment index for efficient comment detection.
  , mcMetavars      :: Map Text Text
    -- ^ Captured metavariables from pattern matching.
    -- Key is metavariable name (e.g., @\"$X\"@), value is captured text.
  , mcMatchColumn   :: Int
    -- ^ Column where the match starts (1-indexed, for comment checking).
  }
  deriving stock (Show)

-- | Result of a successful rule match.
--
-- Contains all information needed to generate a @Diagnostic@:
-- the matched rule, source location, captured text, and context.
--
-- @since 1.0.0
data RuleMatch = RuleMatch
  { rmRule       :: Rule
    -- ^ The rule that matched.
  , rmSpan       :: SrcSpan
    -- ^ Source location of the match.
  , rmMatchedText :: Text
    -- ^ The actual text that was matched.
  , rmMetavars   :: Map Text Text
    -- ^ Captured metavariables for substitution in fixes.
  , rmContext    :: MatchContext
    -- ^ Full match context (file info, content, etc.).
  }
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Unified Evaluation Infrastructure
--------------------------------------------------------------------------------

-- | Shared context for rule evaluation.
--
-- Captures common setup performed by all evaluation functions: comment
-- extraction, comment index building, and rule filtering. Using a shared
-- context eliminates duplicated setup code across evaluation variants.
--
-- __Lifecycle__:
--
-- 1. Created via 'mkRuleEvalContext'
-- 2. Passed to evaluation functions ('evaluateTextRulesWithContext', etc.)
-- 3. May be reused for multiple evaluation strategies on same file
--
-- __Invariants__:
--
-- * 'recEnabledRules' = 'recTextRules' ++ 'recASTRules' (partitioned)
-- * 'recCommentIndex' is empty if 'reCommentAware' is 'False'
-- * Rules are pre-filtered by category, disabled set, and module scope
--
-- __Performance Note__:
--
-- Building comment indices and filtering rules has non-trivial cost.
-- Reuse the same context when applying multiple strategies to avoid
-- redundant computation.
--
-- @since 1.0.0
data RuleEvalContext = RuleEvalContext
  { recEngine       :: RuleEngine
    -- ^ The rule engine configuration (categories, overrides, etc.).
  , recFilePath     :: FilePath
    -- ^ Source file path (for diagnostics and module matching).
  , recModuleName   :: Text
    -- ^ Module name for scope filtering (e.g., @\"Argus.Rules.Engine\"@).
  , recContent      :: Text
    -- ^ Full source content (used for cross-line analysis).
  , recCommentIndex :: CommentIndex
    -- ^ Pre-built comment index for efficient comment detection.
    -- Empty if comment-aware mode is disabled.
  , recEnabledRules :: [Rule]
    -- ^ All enabled rules after filtering by category, disabled set,
    -- and module scope. Union of 'recTextRules' and 'recASTRules'.
  , recTextRules    :: [Rule]
    -- ^ Rules with 'TextPatternSpec' or 'RegexPatternSpec' patterns.
    -- Evaluated via line-by-line text matching.
  , recASTRules     :: [Rule]
    -- ^ Rules with 'ASTPatternSpec' patterns.
    -- Evaluated via AST traversal (requires parsing).
  }

-- | Build a @RuleEvalContext@ from engine and file data.
--
-- Consolidates common setup logic: extracts comments, builds comment index,
-- filters rules by category\/scope, and partitions into text vs AST rules.
--
-- __Parameters__:
--
-- * @engine@: Rule engine with configuration and rule collection
-- * @filepath@: Source file path (used for module matching and diagnostics)
-- * @moduleName@: Haskell module name (for scope-based rule filtering)
-- * @content@: Complete file contents
--
-- __Returns__:
--
-- A fully initialized context ready for evaluation functions.
--
-- __Example__:
--
-- @
-- let ctx = mkRuleEvalContext engine \"src\/Foo.hs\" \"Foo\" content
-- -- Reuse ctx for multiple strategies
-- let seqDiags = evaluateTextRulesWithContext ctx
-- let parDiags = evaluateTextRulesParallelWithContext ParallelRules ctx
-- @
--
-- __Complexity__: O(n) where n is the number of lines (for comment extraction).
--
-- @since 1.0.0
mkRuleEvalContext :: RuleEngine -> FilePath -> Text -> Text -> RuleEvalContext
mkRuleEvalContext engine filepath moduleName content =
  let comments = Comments.extractComments content
      commentIndex = if reCommentAware engine
                       then Comments.buildCommentIndex comments
                       else Comments.emptyCommentIndex
      enabledRules = filter (isRuleEnabled engine moduleName) (reRules engine)
      (astRules, textRules) = partitionRules enabledRules
  in RuleEvalContext
    { recEngine       = engine
    , recFilePath     = filepath
    , recModuleName   = moduleName
    , recContent      = content
    , recCommentIndex = commentIndex
    , recEnabledRules = enabledRules
    , recTextRules    = textRules
    , recASTRules     = astRules
    }

-- | Evaluate text-based rules using a pre-built context.
--
-- Core text rule evaluation function. Iterates over lines, applies each
-- text\/regex rule, checks side conditions, and produces diagnostics.
--
-- __Implementation Details__:
--
-- * Uses 'DList' internally for O(1) append during line iteration
-- * Skips comment lines for code-targeting rules when comment-aware
-- * Evaluates side conditions before generating diagnostics
-- * Generates fix suggestions where rules provide replacements
--
-- __Parameters__:
--
-- * @ctx@: Pre-built evaluation context from 'mkRuleEvalContext'
--
-- __Returns__:
--
-- List of diagnostics from text\/regex rule matches.
-- AST rules in the context are ignored (use 'evaluateRulesWithASTAndContext').
--
-- __Example__:
--
-- @
-- let ctx = mkRuleEvalContext engine filepath moduleName content
-- let diagnostics = evaluateTextRulesWithContext ctx
-- @
--
-- @since 1.0.0
evaluateTextRulesWithContext :: RuleEvalContext -> [Diagnostic]
evaluateTextRulesWithContext ctx =
  DL.toList $ foldMap (evaluateRuleDL (recEngine ctx) (recFilePath ctx) (recModuleName ctx)
                                      (recContent ctx) (recCommentIndex ctx))
                      (recTextRules ctx)

-- | Evaluate text rules with parallel strategy.
--
-- Applies parallelism according to the specified strategy:
--
-- * @SequentialEval@: No parallelism (baseline, same as 'evaluateTextRulesWithContext')
-- * @ParallelRules@: Rules evaluated in parallel via @parMap@
-- * 'ParallelLines': Lines evaluated in parallel per rule
-- * 'ParallelBoth': Both rules and lines in parallel (most aggressive)
-- * 'ChunkedParallel': Rules processed in chunks
--
-- __Parameters__:
--
-- * @strategy@: Parallelism strategy to use
-- * @ctx@: Pre-built evaluation context
--
-- __Returns__:
--
-- List of diagnostics (order may vary with parallel strategies).
--
-- __Performance Guidance__:
--
-- * 'ParallelRules': Best for files with many rules but few lines
-- * 'ParallelLines': Best for large files with few rules
-- * 'ParallelBoth': Best for large files with many rules
-- * 'ChunkedParallel': Best for controlling parallelism granularity
--
-- __Thread Safety__:
--
-- This function uses GHC sparks for parallelism. Ensure the runtime
-- is compiled with @-threaded@ and invoked with @+RTS -N@.
--
-- @since 1.0.0
evaluateTextRulesParallelWithContext :: ParallelStrategy -> RuleEvalContext -> [Diagnostic]
evaluateTextRulesParallelWithContext strategy ctx =
  case strategy of
    SequentialEval ->
      evaluateTextRulesWithContext ctx

    ParallelRules ->
      -- parMap needs regular lists, but we can still use DList per-rule
      concat $ parMap rseq
        (evaluateRule (recEngine ctx) (recFilePath ctx) (recModuleName ctx)
                      (recContent ctx) (recCommentIndex ctx))
        (recTextRules ctx)

    ParallelLines ->
      -- Use DList to accumulate across rules
      DL.toList $ foldMap (DL.fromList . evaluateRuleParallelLines (recEngine ctx) (recFilePath ctx)
                           (recModuleName ctx) (recContent ctx) (recCommentIndex ctx))
                          (recTextRules ctx)

    ParallelBoth ->
      concat $ parMap rseq
        (evaluateRuleParallelLines (recEngine ctx) (recFilePath ctx)
         (recModuleName ctx) (recContent ctx) (recCommentIndex ctx))
        (recTextRules ctx)

    ChunkedParallel chunkSize ->
      let chunks = chunksOf chunkSize (recTextRules ctx)
          evalChunk = DL.toList . foldMap (evaluateRuleDL (recEngine ctx) (recFilePath ctx)
                                           (recModuleName ctx) (recContent ctx) (recCommentIndex ctx))
      in concat $ parMap rseq evalChunk chunks

--------------------------------------------------------------------------------
-- Side Condition Evaluation
--------------------------------------------------------------------------------

-- | Evaluate a 'SideCondition' against a match context (pure version).
--
-- Side conditions refine rule matches by checking additional constraints
-- that cannot be expressed in the pattern itself. This pure version uses
-- only syntactic information available in the @MatchContext@.
--
-- __Condition Categories__:
--
-- * __Location predicates__: 'NotInComment', 'NotInString', 'InFunctionBody'
-- * __Type predicates__: 'HasType', 'IsNumeric', 'IsList' (limited without HIE)
-- * __Expression predicates__: 'IsLiteral', 'IsVariable', 'NotEqual'
-- * __Context predicates__: 'HasImport', 'HasPragma', 'InModule', 'InTestFile'
-- * __Combinators__: 'And', 'Or', 'Not', 'Always', 'Never'
--
-- __Limitations__:
--
-- Many type-aware conditions (e.g., 'HasType', 'HasTypeClass', 'IsPure')
-- return 'True' (permissive) without HIE data. For accurate type checking,
-- use 'evalSideConditionIO' with a populated 'HIEContext'.
--
-- __Parameters__:
--
-- * @ctx@: Match context with captured metavariables and file info
-- * @cond@: Side condition to evaluate
--
-- __Returns__:
--
-- 'True' if the condition is satisfied, 'False' otherwise.
--
-- __Example__:
--
-- @
-- let cond = And [NotInComment, NotEqual \"$X\" \"$Y\"]
-- let passes = evalSideCondition matchCtx cond
-- @
--
-- @since 1.0.0
evalSideCondition :: MatchContext -> SideCondition -> Bool
evalSideCondition ctx = \case
  -- Location predicates
  NotInComment -> not $ Comments.isInComment (mcCommentIndex ctx) (mcLineNumber ctx) (mcMatchColumn ctx)
  NotInString -> not $ isInStringLiteral (mcLineText ctx)
  NotInImport -> not $ isImportLine (mcLineText ctx)
  InFunctionBody -> not $ isTopLevel (mcLineText ctx)
  InCommentType ct ->
    let pos = Position (mcLineNumber ctx) (mcMatchColumn ctx)
        ctConvert = convertCommentType ct
    in Comments.isInCommentType (mcCommentIndex ctx) pos ctConvert
  InStringLiteral -> isInStringLiteral (mcLineText ctx)

  -- Type predicates (require HIE data for full accuracy)
  HasType var expectedType ->
    case Map.lookup var (mcMetavars ctx) of
      Just val -> checkType val expectedType
      Nothing -> True  -- Be permissive if not captured
  HasTypeClass var _cls ->
    case Map.lookup var (mcMetavars ctx) of
      Just _val -> True  -- Would need HIE data
      Nothing -> True
  TypeMatches var pat ->
    case Map.lookup var (mcMetavars ctx) of
      Just val -> checkType val pat
      Nothing -> True
  IsNumeric var ->
    case Map.lookup var (mcMetavars ctx) of
      Just val -> isNumericValue val
      Nothing -> True
  IsString var ->
    case Map.lookup var (mcMetavars ctx) of
      Just val -> isStringValue val
      Nothing -> True
  IsList var ->
    case Map.lookup var (mcMetavars ctx) of
      Just val -> isListValue val
      Nothing -> True
  IsMaybe _var -> True  -- Would need HIE data
  IsMonad _var _monad -> True  -- Would need HIE data
  IsPure _var -> True  -- Would need HIE data

  -- Expression predicates
  IsLiteral var ->
    case Map.lookup var (mcMetavars ctx) of
      Just val -> isLiteralValue val
      Nothing -> False
  IsVariable var ->
    case Map.lookup var (mcMetavars ctx) of
      Just val -> isIdentifierValue val
      Nothing -> False
  IsApplication _var -> True  -- Would need AST
  IsLambda _var -> True  -- Would need AST
  IsAtomic var ->
    case Map.lookup var (mcMetavars ctx) of
      Just val -> isLiteralValue val || isIdentifierValue val
      Nothing -> True
  IsConstructor var ->
    case Map.lookup var (mcMetavars ctx) of
      Just val -> isConstructorValue val
      Nothing -> True
  NotEqual var1 var2 ->
    case (Map.lookup var1 (mcMetavars ctx), Map.lookup var2 (mcMetavars ctx)) of
      (Just v1, Just v2) -> v1 /= v2
      _ -> True
  FreeIn _v1 _v2 -> True  -- Would need AST analysis
  NotFreeIn _v1 _v2 -> True  -- Would need AST analysis
  ComplexityLT _var _n -> True  -- Would need AST analysis
  ComplexityGT _var _n -> True  -- Would need AST analysis
  ComplexityCond _var _ord _n -> True  -- Would need AST analysis

  -- Name predicates
  NotIn var vals ->
    case Map.lookup var (mcMetavars ctx) of
      Just val -> val `notElem` vals
      Nothing -> True

  -- Type contains
  TypeContains var typeName ->
    case Map.lookup var (mcMetavars ctx) of
      Just val -> typeName `T.isInfixOf` val
      Nothing -> True

  -- Context predicates
  HasImport modName -> modName `T.isInfixOf` mcFullContent ctx
  HasPragma pragma -> pragma `T.isInfixOf` mcFullContent ctx
  InModule modPat -> matchesGlob (mcModuleName ctx) modPat
  InTestFile -> isTestFile (mcFilePath ctx)
  NotInTestFile -> not $ isTestFile (mcFilePath ctx)

  -- Expression structure predicates
  NotBind _var -> True  -- Would need AST analysis
  IsEtaReducible _var _func -> True  -- Would need AST analysis

  -- Deriving and pattern analysis predicates
  NoDerivingStrategy -> True  -- Would need AST analysis
  WildcardNotLast -> True  -- Would need AST analysis
  HasPatternOverlap -> True  -- Would need AST analysis
  IsPatternIncomplete -> True  -- Would need AST analysis
  HasAmbiguousType -> True  -- Would need HIE data
  UsesDefaultOptions -> True  -- Would need AST analysis

  -- Context predicates (semantic context detection)
  InContext _ctx -> True  -- Would need semantic analysis

  -- Combinators
  And conds -> all (evalSideCondition ctx) conds
  Or conds -> any (evalSideCondition ctx) conds
  Not cond -> not $ evalSideCondition ctx cond
  Always -> True
  Never -> False

-- Helper functions for side condition evaluation

isInStringLiteral :: Text -> Bool
isInStringLiteral line =
  let quoteCount = T.length $ T.filter (== '"') line
  in odd quoteCount

-- | Delegate to shared helper: check if line is import statement
isImportLine :: Text -> Bool
isImportLine = SCH.isImportLine

-- | Delegate to shared helper: check if line is at top level
isTopLevel :: Text -> Bool
isTopLevel = SCH.isTopLevel

-- | Check type compatibility (requires HIE for real type checking)
checkType :: Text -> Text -> Bool
checkType _val _expectedType = True  -- Requires type info from HIE

-- | Delegate to shared helper: check if value is a literal
isLiteralValue :: Text -> Bool
isLiteralValue = SCH.isLiteralValue

-- | Delegate to shared helper: check if value is numeric
isNumericValue :: Text -> Bool
isNumericValue = SCH.isNumericLiteral

-- | Delegate to shared helper: check if value is a string
isStringValue :: Text -> Bool
isStringValue = SCH.isStringLiteral

-- | Delegate to shared helper: check if value is a list
isListValue :: Text -> Bool
isListValue = SCH.isListValue

-- | Delegate to shared helper: check if value is an identifier
isIdentifierValue :: Text -> Bool
isIdentifierValue = SCH.isIdentifier

-- | Delegate to shared helper: check if value is a constructor
isConstructorValue :: Text -> Bool
isConstructorValue = SCH.isConstructor

-- | Delegate to shared helper: check if file is a test file
isTestFile :: FilePath -> Bool
isTestFile = SCH.isTestFile

-- | Delegate to shared helper: glob pattern matching
matchesGlob :: Text -> Text -> Bool
matchesGlob = SCH.matchesGlob

-- | Convert from Rules.Types.CommentType to Analysis.Comments.CommentType
convertCommentType :: CommentType -> CT.CommentType
convertCommentType = \case
  CTLineComment   -> CT.LineComment
  CTBlockComment  -> CT.BlockComment
  CTHaddockLine   -> CT.HaddockLine
  CTHaddockBlock  -> CT.HaddockBlock
  CTPragma        -> CT.PragmaComment

--------------------------------------------------------------------------------
-- HIE-Backed Side Condition Evaluation
--------------------------------------------------------------------------------

-- | Evaluate a side condition with HIE support (IO version).
--
-- Provides accurate type-aware evaluation using the HIE database.
-- Falls back to syntactic checks when HIE data is unavailable for
-- a specific symbol or file.
--
-- __Type-Aware Conditions__:
--
-- These conditions benefit most from HIE data:
--
-- * 'HasType': Checks exact type of captured variable
-- * 'HasTypeClass': Checks if type implements a typeclass
-- * 'IsPure': Determines if expression has no side effects
-- * 'IsMonad': Checks if type is a specific monad
--
-- __Parameters__:
--
-- * @hieCtx@: HIE context with database connection
-- * @ctx@: Match context with captured metavariables
-- * @cond@: Side condition to evaluate
--
-- __Returns__:
--
-- IO action producing 'True' if condition satisfied.
--
-- __Example__:
--
-- @
-- hieCtx <- mkHIEContext \".hie\"
-- passes <- evalSideConditionIO hieCtx matchCtx (HasType \"$X\" \"Int\")
-- @
--
-- @since 1.0.0
evalSideConditionIO :: HIEContext -> MatchContext -> SideCondition -> IO Bool
evalSideConditionIO hieCtx ctx cond =
  SC.evalSideConditionIO
    hieCtx
    (mcMetavars ctx)
    (T.pack $ mcFilePath ctx)
    (mcModuleName ctx)
    (mcLineNumber ctx)
    (mcMatchColumn ctx)
    (mcCommentIndex ctx)
    cond

-- | Create an empty HIE context with known-types database only.
--
-- This context uses a built-in database of common Haskell types
-- (e.g., @Int@, @String@, @Maybe@) for basic type checking.
-- Use this when HIE files are unavailable.
--
-- __Limitations__:
--
-- * No project-specific type information
-- * Cannot resolve user-defined types
-- * Type class membership checks are limited
--
-- __Example__:
--
-- @
-- let hieCtx = emptyHIEContext
-- -- Will use known-types for basic checks
-- passes <- evalSideConditionIO hieCtx ctx (IsNumeric \"$X\")
-- @
--
-- @since 1.0.0
emptyHIEContext :: HIEContext
emptyHIEContext = SC.emptyHIEContext

-- | Create a HIE context from a HIE database directory.
--
-- Loads type information from HIE files generated by GHC.
-- HIE files are created when compiling with @-fwrite-ide-info@.
--
-- __Parameters__:
--
-- * @path@: Directory containing HIE files (typically @.hie@)
--
-- __Returns__:
--
-- IO action producing a context with loaded type information.
--
-- __Preconditions__:
--
-- * Directory should exist (returns empty context if not)
-- * HIE files should match current source (stale files may give wrong results)
--
-- __Example__:
--
-- @
-- hieCtx <- mkHIEContext \".hie\"
-- -- Now has full type information for the project
-- passes <- evalSideConditionIO hieCtx ctx (HasType \"$X\" \"MyType\")
-- @
--
-- @since 1.0.0
mkHIEContext :: FilePath -> IO HIEContext
mkHIEContext = SC.mkHIEContext

--------------------------------------------------------------------------------
-- Engine Construction
--------------------------------------------------------------------------------

-- | Create a rule engine with the given rules.
--
-- Initializes an engine with all categories enabled, no disabled rules,
-- no severity overrides, and comment-aware mode on. Customize the
-- returned engine by modifying its fields.
--
-- __Default Configuration__:
--
-- * All categories enabled
-- * No rules disabled
-- * No severity overrides
-- * Comment-aware mode enabled
--
-- __Parameters__:
--
-- * @rules@: List of rules to include in the engine
--
-- __Returns__:
--
-- A configured @RuleEngine@ ready for evaluation.
--
-- __Example__:
--
-- @
-- -- Engine with custom rules only
-- let engine = mkRuleEngine myCustomRules
--
-- -- Engine with filtered categories
-- let securityEngine = (mkRuleEngine allRules)
--       { reEnabledCategories = Set.singleton Security }
--
-- -- Engine with specific rules disabled
-- let relaxedEngine = (mkRuleEngine allRules)
--       { reDisabledRules = Set.fromList [\"partial\/head\", \"partial\/tail\"] }
-- @
--
-- @since 1.0.0
mkRuleEngine :: [Rule] -> RuleEngine
mkRuleEngine rules = RuleEngine
  { reRules = rules
  , reEnabledCategories = Set.fromList allCategories
  , reDisabledRules = Set.empty
  , reOverrides = Map.empty
  , reCommentAware = True
  }

-- | Default engine with all built-in rules.
--
-- Includes all rules from 'Argus.Rules.Builtin':
--
-- * Safety rules (partial functions, unsafe operations)
-- * Performance rules (space leaks, inefficient patterns)
-- * Security rules (OWASP, injection vulnerabilities)
-- * Style rules (naming, imports, redundancy)
-- * Correctness rules (type errors, logic bugs)
--
-- __Usage__:
--
-- @
-- let diagnostics = evaluateRules defaultEngine filepath moduleName content
-- @
--
-- @since 1.0.0
defaultEngine :: RuleEngine
defaultEngine = mkRuleEngine Builtin.allBuiltinRules

--------------------------------------------------------------------------------
-- Rule Evaluation
--------------------------------------------------------------------------------

-- | Evaluate all text\/regex rules against source code (pure version).
--
-- Main entry point for text-based rule evaluation. For AST rules or
-- HIE-based type checking, use 'evaluateRulesIO' instead.
--
-- __Evaluation Process__:
--
-- 1. Builds @RuleEvalContext@ (extracts comments, filters rules)
-- 2. Iterates through source lines
-- 3. Applies each text\/regex rule pattern
-- 4. Checks side conditions for matches
-- 5. Generates diagnostics with optional fixes
--
-- __Parameters__:
--
-- * @engine@: Configured rule engine
-- * @filepath@: Source file path (for diagnostics)
-- * @moduleName@: Haskell module name (for scope filtering)
-- * @content@: Complete source file content
--
-- __Returns__:
--
-- List of diagnostics from matched rules. Empty if no violations found.
--
-- __Example__:
--
-- @
-- content <- T.readFile \"src\/Foo.hs\"
-- let diagnostics = evaluateRules defaultEngine \"src\/Foo.hs\" \"Foo\" content
-- forM_ diagnostics $ \\diag ->
--   putStrLn $ show (diagSpan diag) <> \": \" <> diagMessage diag
-- @
--
-- __Note__: This function only evaluates text\/regex patterns. Rules with
-- 'ASTPatternSpec' are silently skipped. Use 'evaluateRulesIO' for full
-- rule support.
--
-- @since 1.0.0
evaluateRules :: RuleEngine -> FilePath -> Text -> Text -> [Diagnostic]
evaluateRules engine filepath moduleName content =
  let ctx = mkRuleEvalContext engine filepath moduleName content
  in evaluateTextRulesWithContext ctx

-- | Check if a rule is enabled
isRuleEnabled :: RuleEngine -> Text -> Rule -> Bool
isRuleEnabled engine moduleName rule =
  ruleEnabled rule
  && ruleCategory rule `Set.member` reEnabledCategories engine
  && ruleId rule `Set.notMember` reDisabledRules engine
  && matchesWithinScope (ruleWithin rule) (ruleExcept rule) moduleName

matchesWithinScope :: [Text] -> [Text] -> Text -> Bool
matchesWithinScope within except moduleName
  | not (null except) && any (matchesGlob moduleName) except = False
  | null within = True
  | otherwise = any (matchesGlob moduleName) within

-- | Evaluate a single rule against source content.
--
-- Lower-level function for evaluating one rule at a time. Typically
-- called internally by 'evaluateTextRulesWithContext', but exposed for
-- custom evaluation logic.
--
-- __Parameters__:
--
-- * @engine@: Rule engine (for severity overrides, comment handling)
-- * @filepath@: Source file path
-- * @moduleName@: Haskell module name
-- * @content@: Complete source content
-- * @commentIndex@: Pre-built comment index
-- * @rule@: The rule to evaluate
--
-- __Returns__:
--
-- List of diagnostics produced by this rule. Empty if no matches.
--
-- __Implementation__:
--
-- Uses 'DList' internally for O(1) append during line iteration,
-- converting to list only at the end.
--
-- @since 1.0.0
evaluateRule :: RuleEngine -> FilePath -> Text -> Text -> CommentIndex -> Rule -> [Diagnostic]
evaluateRule engine filepath moduleName content commentIndex rule =
  DL.toList $ evaluateRuleDL engine filepath moduleName content commentIndex rule

-- | Evaluate a single rule, returning a DList for efficient accumulation
--
-- This is the core evaluation function used by evaluateTextRulesWithContext.
-- Returns DList to allow O(1) concatenation when evaluating multiple rules.
evaluateRuleDL :: RuleEngine -> FilePath -> Text -> Text -> CommentIndex -> Rule -> DList Diagnostic
evaluateRuleDL engine filepath moduleName content commentIndex rule =
  let lines' = zip [1..] (T.lines content)
      -- Use foldMap with DList for O(1) append during line iteration
      matchesDL = foldMap (matchLineDL engine rule filepath moduleName content commentIndex) lines'
  in fmap (matchToDiagnostic engine) matchesDL

-- | Match a rule against a single line
matchLine :: RuleEngine -> Rule -> FilePath -> Text -> Text -> CommentIndex -> (Int, Text) -> [RuleMatch]
matchLine engine rule filepath moduleName fullContent commentIndex lineInfo =
  DL.toList $ matchLineDL engine rule filepath moduleName fullContent commentIndex lineInfo

-- | Match a rule against a single line, returning DList for efficient accumulation
matchLineDL :: RuleEngine -> Rule -> FilePath -> Text -> Text -> CommentIndex -> (Int, Text) -> DList RuleMatch
matchLineDL engine rule filepath moduleName fullContent commentIndex (lineNum, lineText) =
  let baseCtx = MatchContext
        { mcFilePath = filepath
        , mcModuleName = moduleName
        , mcLineNumber = lineNum
        , mcLineText = lineText
        , mcFullContent = fullContent
        , mcCommentIndex = commentIndex
        , mcMetavars = Map.empty
        , mcMatchColumn = 1  -- Will be updated when match is found
        }
      -- Check if line is entirely a comment (more efficient than checking every position)
      isCommentLine = "--" `T.isPrefixOf` T.stripStart lineText
      target = effectiveTarget rule
      -- For code-targeting rules, skip entirely if line is a comment
      skipLine = reCommentAware engine && target == TargetCode && isCommentLine
  in if skipLine
     then DL.empty
     else matchRulePatternDL rule baseCtx (rulePattern rule)

-- | Match using a RulePattern (unified pattern type)
_matchRulePattern :: Rule -> MatchContext -> RulePattern -> [RuleMatch]
_matchRulePattern rule ctx pat = DL.toList $ matchRulePatternDL rule ctx pat

-- | Match using a RulePattern, returning DList for efficient accumulation
matchRulePatternDL :: Rule -> MatchContext -> RulePattern -> DList RuleMatch
matchRulePatternDL rule ctx = \case
  TextPatternSpec pat ->
    -- Check if it's a metavariable pattern or simple text
    -- Support both $X style and _x style metavariables
    if hasMetavariables pat
    then case matchPatternWithMetavars pat (mcLineText ctx) of
           Just (col, matched, metavars) ->
             let ctx' = ctx { mcMetavars = metavars }
             in checkConditionsAndBuildDL' rule ctx' col matched
           Nothing -> DL.empty
    else if matchTextPattern pat (mcLineText ctx)
         then checkConditionsAndBuildDL rule ctx pat
         else DL.empty

  RegexPatternSpec pat ->
    case matchRegexPattern pat (mcLineText ctx) of
      Just (col, matched) ->
        let ctx' = ctx { mcMetavars = Map.singleton "$0" matched }
        in checkConditionsAndBuildDL' rule ctx' col matched
      Nothing -> DL.empty

  ASTPatternSpec _astPat ->
    -- AST matching is handled separately in the IO evaluation path.
    -- Rules with ASTPatternSpec are partitioned out by mkRuleEvalContext
    -- and processed via AST.applyASTRules in evaluateRulesWithAST*.
    --
    -- This branch is a defensive fallback that should never be reached
    -- in normal operation since evaluateTextRulesWithContext only receives
    -- text/regex rules (recTextRules). If we reach here, it means:
    -- 1. Someone called evaluateRule directly with an AST rule, or
    -- 2. The rule partitioning logic has a bug.
    --
    -- Return empty to avoid runtime errors; the AST rule will be handled
    -- when using the IO evaluation path (evaluateRulesIO, etc.).
    DL.empty

-- | Check conditions and build match
_checkConditionsAndBuild :: Rule -> MatchContext -> Text -> [RuleMatch]
_checkConditionsAndBuild rule ctx pat = DL.toList $ checkConditionsAndBuildDL rule ctx pat

_checkConditionsAndBuild' :: Rule -> MatchContext -> Int -> Text -> [RuleMatch]
_checkConditionsAndBuild' rule ctx col matched = DL.toList $ checkConditionsAndBuildDL' rule ctx col matched

-- | Check conditions and build match, returning DList
checkConditionsAndBuildDL :: Rule -> MatchContext -> Text -> DList RuleMatch
checkConditionsAndBuildDL rule ctx pat =
  case findPatternInLine pat (mcLineText ctx) of
    Just (col, matched) -> checkConditionsAndBuildDL' rule ctx col matched
    Nothing -> DL.empty

-- | Core condition checking and match building, returning DList
checkConditionsAndBuildDL' :: Rule -> MatchContext -> Int -> Text -> DList RuleMatch
checkConditionsAndBuildDL' rule ctx col matched =
  let ctx' = ctx { mcMatchColumn = col }  -- Update context with actual match column
      target = effectiveTarget rule
      -- Build location conditions based on target type
      targetConditions = case target of
        TargetCode          -> [NotInComment, NotInString]  -- Skip comments and strings
        TargetComments      -> []  -- Match in any comment (side conditions can be more specific)
        TargetDocumentation -> []  -- Match in documentation (side conditions can filter)
        TargetPragmas       -> []  -- Match in pragmas
        TargetStrings       -> [InStringLiteral]  -- Only match in strings
        TargetAll           -> []  -- Match anywhere
      allConditions = targetConditions ++ ruleConditions rule
      allPass = all (evalSideCondition ctx') allConditions
  in if allPass
     then DL.singleton RuleMatch
       { rmRule = rule
       , rmSpan = mkSrcSpanRaw (mcFilePath ctx') (mcLineNumber ctx') col (mcLineNumber ctx') (col + T.length matched)
       , rmMatchedText = matched
       , rmMetavars = mcMetavars ctx'
       , rmContext = ctx'
       }
     else DL.empty

-- | Match a text pattern with smart word boundaries
-- Only add \b where the pattern has word characters at that edge
matchTextPattern :: Text -> Text -> Bool
matchTextPattern textPat line =
  let pat = smartWordBoundary textPat
  in T.unpack line =~ T.unpack pat
{-# INLINE matchTextPattern #-}

-- | Find pattern location in line
findPatternInLine :: Text -> Text -> Maybe (Int, Text)
findPatternInLine textPat line =
  let pat = smartWordBoundary textPat
      result :: (String, String, String)
      result = T.unpack line =~ T.unpack pat
  in case result of
    (before, match, _) | not (null match) ->
      Just (length before + 1, T.pack match)
    _ -> Nothing
{-# INLINE findPatternInLine #-}

-- | Add word boundaries only where appropriate
smartWordBoundary :: Text -> Text
smartWordBoundary textPat =
  let escaped = escapeForRegex textPat
      startsWithWord = not (T.null textPat) && isWordChar (T.head textPat)
      endsWithWord = not (T.null textPat) && isWordChar (T.last textPat)
      prefix = if startsWithWord then "\\b" else ""
      suffix = if endsWithWord then "\\b" else ""
  in prefix <> escaped <> suffix
  where
    isWordChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
                || (c >= '0' && c <= '9') || c == '_' || c == '\''
{-# INLINE smartWordBoundary #-}

-- | Match regex pattern
matchRegexPattern :: Text -> Text -> Maybe (Int, Text)
matchRegexPattern regexPat line =
  let result :: (String, String, String)
      result = T.unpack line =~ T.unpack regexPat
  in case result of
    (before, match, _) | not (null match) ->
      Just (length before + 1, T.pack match)
    _ -> Nothing
{-# INLINE matchRegexPattern #-}

-- | Check if pattern contains metavariables (either $X or _x style)
hasMetavariables :: Text -> Bool
hasMetavariables pat =
  -- Check for $X style metavariables
  "$" `T.isInfixOf` pat ||
  -- Check for _x style metavariables (underscore followed by lowercase letter)
  T.unpack pat =~ ("_[a-z][a-zA-Z0-9_]*" :: String)

-- | Match pattern with metavariables and capture them
matchPatternWithMetavars :: Text -> Text -> Maybe (Int, Text, Map Text Text)
matchPatternWithMetavars patternText line =
  let metavars = extractMetavariables patternText
      regexPat = patternToRegex patternText metavars
      result :: (String, String, String, [String])
      result = T.unpack line =~ T.unpack regexPat
  in case result of
    (before, match, _, groups) | not (null match) ->
      let captures = Map.fromList $ zip metavars (map T.pack groups)
      in Just (length before + 1, T.pack match, captures)
    _ -> Nothing

-- | Extract metavariables from pattern
-- Supports both $X style (uppercase) and _x style (underscore + lowercase)
extractMetavariables :: Text -> [Text]
extractMetavariables pat =
  let -- Extract $X style metavariables
      dollarMatches :: [[String]]
      dollarMatches = T.unpack pat =~ ("\\$[A-Z][A-Z0-9_]*" :: String)
      dollarVars = mapMaybe (fmap T.pack . listToMaybe) dollarMatches
      -- Extract _x style metavariables (underscore + lowercase identifier)
      underscoreMatches :: [[String]]
      underscoreMatches = T.unpack pat =~ ("_[a-z][a-zA-Z0-9_]*" :: String)
      underscoreVars = mapMaybe (fmap T.pack . listToMaybe) underscoreMatches
  in nub (dollarVars ++ underscoreVars)

-- | Convert pattern to regex with capturing groups
patternToRegex :: Text -> [Text] -> Text
patternToRegex pat metavars =
  let escaped = escapeForRegex pat
  in foldr replaceMetavar escaped metavars
  where
    replaceMetavar mv txt =
      T.replace (escapeForRegex mv) (metavarToCapture mv) txt

    -- POSIX regex (TDFA) doesn't support non-greedy (.+?), use specific patterns
    -- Handle both $X and _x style metavariables
    metavarToCapture mv
      -- Dollar-prefixed uppercase metavariables
      | mv == "$X"    = "([a-zA-Z_][a-zA-Z0-9_']*)"  -- Identifier
      | mv == "$Y"    = "([a-zA-Z_][a-zA-Z0-9_']*)"  -- Identifier
      | mv == "$Z"    = "([a-zA-Z_][a-zA-Z0-9_']*)"  -- Identifier
      | mv == "$F"    = "([a-z][a-zA-Z0-9'_]*)"      -- Function name
      | mv == "$FUNC" = "([a-z][a-zA-Z0-9'_]*)"      -- Function name
      | mv == "$N"    = "([0-9]+)"                   -- Number
      | mv == "$T"    = "([A-Z][a-zA-Z0-9'_]*)"      -- Type name
      | mv == "$TYPE" = "([A-Z][a-zA-Z0-9'_]*)"      -- Type name
      -- Underscore-prefixed lowercase metavariables (HLint style)
      | "_" `T.isPrefixOf` mv = "([a-zA-Z_][a-zA-Z0-9_']*)"  -- Any identifier
      -- Default fallback
      | otherwise     = "([a-zA-Z_][a-zA-Z0-9_']*)"  -- Default: identifier

-- | Escape special regex characters
escapeForRegex :: Text -> Text
escapeForRegex = T.concatMap escape
  where
    escape c
      | c `elem` (".^*+?{}[]\\|()" :: String) = T.pack ['\\', c]
      | c == '$' = "\\$"
      | otherwise = T.singleton c

--------------------------------------------------------------------------------
-- Diagnostic Generation
--------------------------------------------------------------------------------

-- | Convert a @RuleMatch@ to a @Diagnostic@.
--
-- Transforms a successful rule match into a user-facing diagnostic,
-- applying severity overrides and generating fix suggestions.
--
-- __Transformation Steps__:
--
-- 1. Apply severity override if configured for this rule ID
-- 2. Map rule category to diagnostic kind
-- 3. Interpolate metavariables in the message
-- 4. Build fix from replacement template (if present)
--
-- __Parameters__:
--
-- * @engine@: Rule engine (for severity overrides)
-- * @match@: The successful rule match
--
-- __Returns__:
--
-- A @Diagnostic@ ready for output.
--
-- @since 1.0.0
matchToDiagnostic :: RuleEngine -> RuleMatch -> Diagnostic
matchToDiagnostic engine match =
  let rule = rmRule match
      severity = fromMaybe (ruleSeverity rule) $ Map.lookup (ruleId rule) (reOverrides engine)
      fix' = buildFix match
  in Diagnostic
    { diagSpan = rmSpan match
    , diagSeverity = severity
    , diagKind = categoryToKind (ruleCategory rule)
    , diagMessage = interpolateMessage (ruleMessage rule) (rmMetavars match)
    , diagCode = Just $ ruleId rule
    , diagFixes = catMaybes [fix']
    , diagRelated = []
    }

-- | Build fix from unified Rule
--
-- Uses ruleReplacement, ruleSafety, ruleAddImports, ruleRemoveImports directly!
buildFix :: RuleMatch -> Maybe Fix
buildFix match = do
  repl <- ruleReplacement (rmRule match)
  let replacement = interpolateMessage repl (rmMetavars match)
      rule = rmRule match
  pure Fix
    { fixTitle = fromMaybe ("Apply: " <> ruleId rule) (ruleFixDescription rule)
    , fixEdits = [FixEdit (rmSpan match) replacement]
    , fixIsPreferred = ruleSafety rule == Safe
    , fixAddImports = map importSpecToFixImport (ruleAddImports rule)
    , fixRemoveImports = ruleRemoveImports rule
    , fixCategory = categoryToFixCategory (ruleCategory rule)
    , fixSafety = safetyToFixSafety (ruleSafety rule)
    }

-- | Interpolate metavariables in a message or replacement template.
--
-- Substitutes captured metavariable values into text templates.
-- Used for both diagnostic messages and fix replacements.
--
-- __Supported Metavariable Formats__:
--
-- * @$X@, @$Y@, @$Z@: Identifier captures
-- * @$F@, @$FUNC@: Function name captures
-- * @$T@, @$TYPE@: Type name captures
-- * @$N@: Numeric captures
-- * @_x@, @_foo@: HLint-style captures
--
-- __Algorithm__:
--
-- 1. Finds all metavariable occurrences with their positions
-- 2. Sorts by position descending (right-to-left)
-- 3. Applies substitutions without position shifting
--
-- __Parameters__:
--
-- * @msg@: Template text with metavariable placeholders
-- * @vars@: Map from metavariable name to captured value
--
-- __Returns__:
--
-- Text with all metavariables replaced by their captured values.
--
-- __Example__:
--
-- @
-- let vars = Map.fromList [(\"$F\", \"foo\"), (\"$X\", \"bar\")]
-- interpolateMessage \"Use $F instead of $X\" vars
-- -- Returns: \"Use foo instead of bar\"
-- @
--
-- __Invariant__:
--
-- Metavariable boundaries are respected: @$X@ won't match inside @$XS@.
--
-- @since 1.0.0
interpolateMessage :: Text -> Map Text Text -> Text
interpolateMessage msg vars
  | Map.null vars = msg
  | otherwise = substituteMetavars msg (Map.toList vars)

-- | Substitute metavariables in text, processing from right to left
-- to avoid position shifting issues
substituteMetavars :: Text -> [(Text, Text)] -> Text
substituteMetavars txt [] = txt
substituteMetavars txt subs =
  -- Find all metavariable occurrences with their positions
  let allOccurrences = concatMap (findMetavarOccurrences txt) subs
      -- Sort by position descending (right to left) to avoid shifting
      sortedOccurrences = sortByPositionDesc allOccurrences
  in applySubstitutionsRTL txt sortedOccurrences
  where
    -- Sort by start position descending
    sortByPositionDesc = sortOn (\(pos, _, _) -> negate pos)

-- | Find all occurrences of a metavariable in text
-- Returns list of (startPos, length, replacement)
findMetavarOccurrences :: Text -> (Text, Text) -> [(Int, Int, Text)]
findMetavarOccurrences txt (metavar, replacement) = go 0 txt
  where
    metavarLen = T.length metavar
    go !offset remaining
      | T.null remaining = []
      | otherwise = case T.breakOn metavar remaining of
          (before, after)
            | T.null after -> []
            | otherwise ->
                let pos = offset + T.length before
                    -- Check this is a proper metavariable boundary
                    -- (not part of a longer identifier)
                    isValidBoundary = isMetavarBoundary txt pos metavarLen
                in if isValidBoundary
                   then (pos, metavarLen, replacement) :
                        go (pos + metavarLen) (T.drop metavarLen after)
                   else go (pos + 1) (T.drop 1 after)

-- | Check if a position represents a valid metavariable boundary
-- Metavariables like $XS should not match inside longer identifiers
isMetavarBoundary :: Text -> Int -> Int -> Bool
isMetavarBoundary txt pos len =
  let afterEnd = pos + len
      -- Check character after the metavariable
      charAfter = if afterEnd < T.length txt
                  then Just (T.index txt afterEnd)
                  else Nothing
      -- For $-prefixed metavars, the char after shouldn't be alphanumeric/underscore
      -- (which would mean it's part of a longer metavar like $XS vs $XSOMETHING)
      validAfter = case charAfter of
        Nothing -> True
        Just c -> not (isMetavarChar c)
  in validAfter
  where
    isMetavarChar c = (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'

-- | Apply substitutions from right to left
applySubstitutionsRTL :: Text -> [(Int, Int, Text)] -> Text
applySubstitutionsRTL = foldl' applyOne
  where
    applyOne txt (pos, len, replacement) =
      let (before, rest) = T.splitAt pos txt
          after = T.drop len rest
      in before <> replacement <> after

-- | Map unified Category to diagnostic kind
categoryToKind :: Category -> DiagnosticKind
categoryToKind = \case
  Security -> SecurityIssue
  Performance -> PerformanceIssue
  SpaceLeaks -> PerformanceIssue
  Safety -> PartialFunction
  Imports -> ImportStyle
  Naming -> NamingConvention
  Extensions -> AT.Custom "extension"
  Modernization -> CodePattern
  Redundant -> RedundantCode
  Complexity -> AT.Custom "complexity"
  Concurrency -> AT.Custom "concurrency"
  ErrorHandling -> AT.Custom "error-handling"
  Documentation -> AT.Custom "documentation"
  Style -> CodePattern
  Correctness -> AT.Custom "correctness"
  RT.Custom name -> AT.Custom name

--------------------------------------------------------------------------------
-- AST-Integrated Rule Evaluation
--------------------------------------------------------------------------------

-- | Evaluate rules with a pre-parsed AST.
--
-- Combines text-based and AST-based rule evaluation. Use this when you
-- already have a parsed AST to avoid redundant parsing.
--
-- __Evaluation Flow__:
--
-- 1. Evaluates text\/regex rules via line iteration
-- 2. Evaluates AST rules via tree traversal
-- 3. Merges results into single diagnostic list
--
-- __Parameters__:
--
-- * @engine@: Configured rule engine
-- * @filepath@: Source file path
-- * @moduleName@: Haskell module name
-- * @content@: Source content (for text rules and fix generation)
-- * @hsModule@: Pre-parsed GHC AST
--
-- __Returns__:
--
-- IO action producing combined diagnostics from all rule types.
--
-- __Example__:
--
-- @
-- parseResult <- Syntactic.parseModule filepath content
-- case parseResult of
--   Right pr -> do
--     diagnostics <- evaluateRulesWithAST engine filepath moduleName content (prModule pr)
--     -- Process diagnostics...
--   Left err -> -- Handle parse error
-- @
--
-- @since 1.0.0
evaluateRulesWithAST :: RuleEngine -> FilePath -> Text -> Text -> HsModule GhcPs -> IO [Diagnostic]
evaluateRulesWithAST engine filepath moduleName content hsModule = do
  let ctx = mkRuleEvalContext engine filepath moduleName content
      -- Evaluate text-based rules using consolidated context
      textDiagnostics = evaluateTextRulesWithContext ctx
  -- Evaluate AST-based rules via ASTMatch module
  astDiagnostics <- AST.applyASTRules (recASTRules ctx) filepath content hsModule
  pure $ textDiagnostics ++ astDiagnostics

-- | Evaluate rules with a pre-built context and parsed AST.
--
-- Variant that accepts a @RuleEvalContext@ directly, avoiding redundant
-- setup when context is already available (e.g., from parallel evaluation).
--
-- __Parameters__:
--
-- * @ctx@: Pre-built evaluation context
-- * @hsModule@: Pre-parsed GHC AST
--
-- __Returns__:
--
-- IO action producing combined diagnostics.
--
-- @since 1.0.0
evaluateRulesWithASTAndContext :: RuleEvalContext -> HsModule GhcPs -> IO [Diagnostic]
evaluateRulesWithASTAndContext ctx hsModule = do
  let textDiagnostics = evaluateTextRulesWithContext ctx
  astDiagnostics <- AST.applyASTRules (recASTRules ctx) (recFilePath ctx) (recContent ctx) hsModule
  pure $ textDiagnostics ++ astDiagnostics

-- | Evaluate all rules with automatic parsing (main IO entry point).
--
-- Recommended entry point for full rule evaluation. Parses the source
-- file automatically and evaluates both text-based and AST-based rules.
--
-- __Graceful Degradation__:
--
-- If parsing fails (syntax errors in source), falls back to text-based
-- rules only. No diagnostics are lost; AST rules simply don't apply.
--
-- __Parameters__:
--
-- * @engine@: Configured rule engine
-- * @filepath@: Source file path
-- * @moduleName@: Haskell module name
-- * @content@: Complete source content
--
-- __Returns__:
--
-- IO action producing all applicable diagnostics.
--
-- __Example__:
--
-- @
-- content <- T.readFile \"src\/Foo.hs\"
-- diagnostics <- evaluateRulesIO defaultEngine \"src\/Foo.hs\" \"Foo\" content
-- unless (null diagnostics) $
--   forM_ diagnostics printDiagnostic
-- @
--
-- __Recommended For__:
--
-- * CLI tools and CI pipelines
-- * LSP servers (with caching of parsed AST)
-- * Any context where full rule coverage is needed
--
-- @since 1.0.0
evaluateRulesIO :: RuleEngine -> FilePath -> Text -> Text -> IO [Diagnostic]
evaluateRulesIO engine filepath moduleName content = do
  let ctx = mkRuleEvalContext engine filepath moduleName content
  parseResult <- Syntactic.parseModule filepath content
  case parseResult of
    Right pr ->
      evaluateRulesWithASTAndContext ctx (Syntactic.prModule pr)
    Left _parseError ->
      -- Parsing failed - fall back to text-based rules only
      pure $ evaluateTextRulesWithContext ctx

-- | Partition rules into AST-based and text-based
partitionRules :: [Rule] -> ([Rule], [Rule])
partitionRules = foldr go ([], [])
  where
    go rule (ast, text) = case rulePattern rule of
      ASTPatternSpec _ -> (rule : ast, text)
      _                -> (ast, rule : text)

--------------------------------------------------------------------------------
-- Parallel Rule Evaluation
--------------------------------------------------------------------------------

-- | Strategy for parallel rule evaluation.
--
-- Different strategies optimize for different workload characteristics.
-- Choose based on your file sizes and rule counts.
--
-- __Strategy Comparison__:
--
-- @
-- Strategy          | Best For                    | Overhead
-- ------------------|-----------------------------|---------
-- SequentialEval    | Small files, few rules      | None
-- ParallelRules     | Many rules, small files     | Low
-- ParallelLines     | Large files, few rules      | Medium
-- ParallelBoth      | Large files, many rules     | High
-- ChunkedParallel n | Fine-grained control        | Variable
-- @
--
-- __Example__:
--
-- @
-- -- For a large codebase with many rules
-- diagnostics <- evaluateRulesParallelIO ParallelRules engine fp mod content
--
-- -- For a single large file
-- diagnostics <- evaluateRulesParallelIO ParallelLines engine fp mod content
--
-- -- For maximum parallelism on multi-core systems
-- diagnostics <- evaluateRulesParallelIO ParallelBoth engine fp mod content
-- @
--
-- @since 1.0.0
data ParallelStrategy
  = ParallelRules
    -- ^ Evaluate rules in parallel using 'parMap'. Each rule runs
    -- independently on the full file. Best for many rules, smaller files.
  | ParallelLines
    -- ^ Evaluate lines in parallel per rule. Best for large files
    -- with fewer rules.
  | ParallelBoth
    -- ^ Maximum parallelism: both rules and lines run in parallel.
    -- Highest overhead but best for large files with many rules.
  | SequentialEval
    -- ^ No parallelism (baseline). Best for small files or debugging.
  | ChunkedParallel Int
    -- ^ Process rules in chunks of N. Allows fine-grained control
    -- over parallelism granularity. The 'Int' specifies chunk size.
  deriving stock (Eq, Show, Generic)

instance NFData ParallelStrategy where
  rnf ParallelRules        = ()
  rnf ParallelLines        = ()
  rnf ParallelBoth         = ()
  rnf SequentialEval       = ()
  rnf (ChunkedParallel n)  = rnf n

-- | Default parallel strategy.
--
-- Currently 'ParallelRules', which provides good performance for most
-- workloads with minimal overhead.
--
-- @since 1.0.0
defaultParallelStrategy :: ParallelStrategy
defaultParallelStrategy = ParallelRules

-- | Evaluate text\/regex rules in parallel (pure version).
--
-- Applies parallelism to text-based rule evaluation. For full rule
-- support including AST rules, use 'evaluateRulesParallelIO'.
--
-- __Parameters__:
--
-- * @strategy@: Parallelism strategy
-- * @engine@: Configured rule engine
-- * @filepath@: Source file path
-- * @moduleName@: Haskell module name
-- * @content@: Source content
--
-- __Returns__:
--
-- List of diagnostics (order may vary with parallel strategies).
--
-- __Example__:
--
-- @
-- let diagnostics = evaluateRulesParallel ParallelRules engine fp mod content
-- @
--
-- __Thread Safety__:
--
-- Requires @-threaded@ RTS and @+RTS -N@ for actual parallelism.
--
-- @since 1.0.0
evaluateRulesParallel :: ParallelStrategy -> RuleEngine -> FilePath -> Text -> Text -> [Diagnostic]
evaluateRulesParallel strategy engine filepath moduleName content =
  let ctx = mkRuleEvalContext engine filepath moduleName content
  in evaluateTextRulesParallelWithContext strategy ctx

-- | Helper: Evaluate a single rule with parallel line processing
evaluateRuleParallelLines :: RuleEngine -> FilePath -> Text -> Text -> CommentIndex -> Rule -> [Diagnostic]
evaluateRuleParallelLines engine filepath moduleName content commentIndex rule =
  let lines' = zip [1..] (T.lines content)
      matches = concat $ parMap rseq
        (matchLine engine rule filepath moduleName content commentIndex)
        lines'
  in map (matchToDiagnostic engine) matches

-- | Parallel rule evaluation with AST support (IO version).
--
-- Full-featured parallel evaluation that includes AST-based rules.
-- Parses the source file and applies both text and AST rules with
-- the specified parallelism strategy.
--
-- __Graceful Degradation__:
--
-- If parsing fails, falls back to parallel text-based evaluation only.
--
-- __Parameters__:
--
-- * @strategy@: Parallelism strategy for text rules
-- * @engine@: Configured rule engine
-- * @filepath@: Source file path
-- * @moduleName@: Haskell module name
-- * @content@: Source content
--
-- __Returns__:
--
-- IO action producing combined diagnostics from all rule types.
--
-- __Example__:
--
-- @
-- -- Parallel evaluation with AST support
-- diagnostics <- evaluateRulesParallelIO ParallelBoth engine fp mod content
-- @
--
-- __Recommended For__:
--
-- * Performance-critical applications
-- * Multi-core systems with large codebases
-- * Build systems with parallel file processing
--
-- @since 1.0.0
evaluateRulesParallelIO :: ParallelStrategy -> RuleEngine -> FilePath -> Text -> Text -> IO [Diagnostic]
evaluateRulesParallelIO strategy engine filepath moduleName content = do
  let ctx = mkRuleEvalContext engine filepath moduleName content
  parseResult <- Syntactic.parseModule filepath content
  case parseResult of
    Right pr ->
      evaluateRulesParallelWithASTAndContext strategy ctx (Syntactic.prModule pr)
    Left _parseError ->
      -- Parsing failed - fall back to text-based rules only
      pure $ evaluateTextRulesParallelWithContext strategy ctx

-- | Parallel rule evaluation with a parsed AST
--
-- Uses consolidated RuleEvalContext to eliminate duplication.
_evaluateRulesParallelWithAST
  :: ParallelStrategy
  -> RuleEngine
  -> FilePath
  -> Text
  -> Text
  -> HsModule GhcPs
  -> IO [Diagnostic]
_evaluateRulesParallelWithAST strategy engine filepath moduleName content hsModule =
  let ctx = mkRuleEvalContext engine filepath moduleName content
  in evaluateRulesParallelWithASTAndContext strategy ctx hsModule

-- | Parallel rule evaluation with a pre-built context and parsed AST
--
-- This variant accepts a RuleEvalContext directly, avoiding redundant setup.
evaluateRulesParallelWithASTAndContext
  :: ParallelStrategy
  -> RuleEvalContext
  -> HsModule GhcPs
  -> IO [Diagnostic]
evaluateRulesParallelWithASTAndContext strategy ctx hsModule = do
  let textDiagnostics = evaluateTextRulesParallelWithContext strategy ctx
  -- AST rules are evaluated via ASTMatch (which may have its own parallelism)
  astDiagnostics <- AST.applyASTRules (recASTRules ctx) (recFilePath ctx) (recContent ctx) hsModule
  pure $ textDiagnostics ++ astDiagnostics

-- | Split a list into chunks of a given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
  in chunk : chunksOf n rest
