{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Rules.Engine
-- Description : Unified rule evaluation engine using unified Rule type
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides a unified rule evaluation engine that works directly with
-- the unified 'Rule' type from "Argus.Rules.Types". No conversions needed!
--
-- == Architecture
--
-- The Engine evaluates 'Rule' directly:
--
-- @
-- Rule → evaluateRule → Diagnostic
-- @
--
-- No intermediate 'RuleDescriptor' or conversion functions.
--
-- == Features
--
-- * Evaluates unified Rule type directly (from DSL or TOML)
-- * Multiple matching strategies (text, regex, AST via RulePattern)
-- * Rich metadata (categories, safety levels, fix imports)
-- * Side conditions (unified SideCondition type)
-- * Comment-aware analysis
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
import Argus.Rules.Types qualified as RT (CommentType(..))
import Argus.Analysis.Syntactic qualified as Syntactic
import Argus.Rules.ASTMatch qualified as AST
import Argus.Rules.ASTMatch (HsModule, GhcPs)
import Argus.Rules.SideConditions qualified as SC
import Argus.Rules.SideConditions (HIEContext)
import Argus.Rules.SideConditionHelpers qualified as SCH

--------------------------------------------------------------------------------
-- Core Engine Types
--------------------------------------------------------------------------------

-- | The unified rule evaluation engine
--
-- Uses the unified 'Rule' type directly - no RuleDescriptor conversion needed!
data RuleEngine = RuleEngine
  { reRules             :: [Rule]             -- ^ All loaded rules (unified type!)
  , reEnabledCategories :: Set Category       -- ^ Enabled categories (unified type!)
  , reDisabledRules     :: Set Text           -- ^ Explicitly disabled rule IDs
  , reOverrides         :: Map Text Severity  -- ^ Rule-specific severity overrides
  , reCommentAware      :: Bool               -- ^ Skip matches in comments
  }
  deriving stock (Show, Generic)

-- | Context available during matching
data MatchContext = MatchContext
  { mcFilePath      :: FilePath          -- ^ Current file path
  , mcModuleName    :: Text              -- ^ Current module name
  , mcLineNumber    :: Int               -- ^ Current line number
  , mcLineText      :: Text              -- ^ Full line text
  , mcFullContent   :: Text              -- ^ Full file content
  , mcCommentIndex  :: CommentIndex      -- ^ Proper comment index (multi-line aware)
  , mcMetavars      :: Map Text Text     -- ^ Captured metavariables
  , mcMatchColumn   :: Int               -- ^ Column where the match starts (for comment checking)
  }
  deriving stock (Show)

-- | Result of a successful match
data RuleMatch = RuleMatch
  { rmRule       :: Rule                -- ^ The matched rule (unified type!)
  , rmSpan       :: SrcSpan             -- ^ Source location
  , rmMatchedText :: Text               -- ^ The matched code
  , rmMetavars   :: Map Text Text       -- ^ Captured metavariables
  , rmContext    :: MatchContext        -- ^ Full context
  }
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Unified Evaluation Infrastructure
--------------------------------------------------------------------------------

-- | Shared context for rule evaluation (reduces duplication across variants)
--
-- This captures the common setup performed by all evaluation functions:
-- comment extraction, comment index building, and rule filtering.
data RuleEvalContext = RuleEvalContext
  { recEngine       :: RuleEngine       -- ^ The rule engine configuration
  , recFilePath     :: FilePath         -- ^ Source file path
  , recModuleName   :: Text             -- ^ Module name
  , recContent      :: Text             -- ^ Full source content
  , recCommentIndex :: CommentIndex     -- ^ Pre-built comment index
  , recEnabledRules :: [Rule]           -- ^ Pre-filtered enabled rules
  , recTextRules    :: [Rule]           -- ^ Text/regex pattern rules
  , recASTRules     :: [Rule]           -- ^ AST pattern rules
  }

-- | Build a RuleEvalContext from engine and file data
--
-- This consolidates the common setup logic that was duplicated across
-- evaluateRules, evaluateRulesWithAST, evaluateRulesParallel, etc.
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

-- | Evaluate text-based rules using a pre-built context
--
-- This is the core text rule evaluation function that all variants call into.
-- It eliminates the duplicated line enumeration logic.
-- Uses DList internally for O(1) append during accumulation.
evaluateTextRulesWithContext :: RuleEvalContext -> [Diagnostic]
evaluateTextRulesWithContext ctx =
  DL.toList $ foldMap (evaluateRuleDL (recEngine ctx) (recFilePath ctx) (recModuleName ctx)
                                      (recContent ctx) (recCommentIndex ctx))
                      (recTextRules ctx)

-- | Evaluate text rules with parallel strategy
-- Uses DList for efficient accumulation in non-parallel paths.
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

-- | Evaluate a unified SideCondition in context
--
-- This evaluates the unified 'SideCondition' type from "Argus.Rules.Types"
-- directly - no conversion needed!
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

-- | Evaluate a side condition with HIE support (IO-based)
--
-- This provides real type-aware evaluation using HIE database.
-- Falls back to syntactic checks when HIE data is unavailable.
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

-- | Create an empty HIE context (uses known-types database only)
emptyHIEContext :: HIEContext
emptyHIEContext = SC.emptyHIEContext

-- | Create a HIE context from a database path
mkHIEContext :: FilePath -> IO HIEContext
mkHIEContext = SC.mkHIEContext

--------------------------------------------------------------------------------
-- Engine Construction
--------------------------------------------------------------------------------

-- | Create a rule engine with the given rules
--
-- Uses unified 'Rule' type directly - no conversion needed!
mkRuleEngine :: [Rule] -> RuleEngine
mkRuleEngine rules = RuleEngine
  { reRules = rules
  , reEnabledCategories = Set.fromList allCategories
  , reDisabledRules = Set.empty
  , reOverrides = Map.empty
  , reCommentAware = True
  }

-- | Default engine with all built-in rules from the Builtin modules
defaultEngine :: RuleEngine
defaultEngine = mkRuleEngine Builtin.allBuiltinRules

--------------------------------------------------------------------------------
-- Rule Evaluation
--------------------------------------------------------------------------------

-- | Evaluate all rules against source code
--
-- Uses unified 'Rule' type directly - no conversion needed!
-- Now uses the consolidated RuleEvalContext to eliminate duplication.
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

-- | Evaluate a single rule
--
-- Uses unified 'Rule' type and 'RulePattern' directly!
-- Uses DList internally for O(1) append during line iteration.
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
matchRulePattern :: Rule -> MatchContext -> RulePattern -> [RuleMatch]
matchRulePattern rule ctx pat = DL.toList $ matchRulePatternDL rule ctx pat

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
checkConditionsAndBuild :: Rule -> MatchContext -> Text -> [RuleMatch]
checkConditionsAndBuild rule ctx pat = DL.toList $ checkConditionsAndBuildDL rule ctx pat

checkConditionsAndBuild' :: Rule -> MatchContext -> Int -> Text -> [RuleMatch]
checkConditionsAndBuild' rule ctx col matched = DL.toList $ checkConditionsAndBuildDL' rule ctx col matched

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

-- | Match regex pattern
matchRegexPattern :: Text -> Text -> Maybe (Int, Text)
matchRegexPattern regexPat line =
  let result :: (String, String, String)
      result = T.unpack line =~ T.unpack regexPat
  in case result of
    (before, match, _) | not (null match) ->
      Just (length before + 1, T.pack match)
    _ -> Nothing

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

-- | Convert a match to a diagnostic
--
-- Uses unified 'Rule' type directly!
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

-- | Interpolate metavariables in message
--
-- Uses proper metavariable-aware substitution that:
-- 1. Only replaces exact metavariable tokens (e.g., $XS, $F)
-- 2. Processes replacements from right to left to avoid position shifting
-- 3. Doesn't corrupt text when replacement values contain pattern-like substrings
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

-- | Evaluate rules with a pre-parsed AST (for when you already have the AST)
--
-- This function combines text-based matching with AST-based matching for rules
-- that specify ASTPatternSpec. Text-based rules are evaluated using the
-- consolidated RuleEvalContext, while AST rules are evaluated via ASTMatch.
evaluateRulesWithAST :: RuleEngine -> FilePath -> Text -> Text -> HsModule GhcPs -> IO [Diagnostic]
evaluateRulesWithAST engine filepath moduleName content hsModule = do
  let ctx = mkRuleEvalContext engine filepath moduleName content
      -- Evaluate text-based rules using consolidated context
      textDiagnostics = evaluateTextRulesWithContext ctx
  -- Evaluate AST-based rules via ASTMatch module
  astDiagnostics <- AST.applyASTRules (recASTRules ctx) filepath content hsModule
  pure $ textDiagnostics ++ astDiagnostics

-- | Evaluate rules with a pre-built context and parsed AST
--
-- This variant accepts a RuleEvalContext directly, avoiding redundant setup
-- when context is already available.
evaluateRulesWithASTAndContext :: RuleEvalContext -> HsModule GhcPs -> IO [Diagnostic]
evaluateRulesWithASTAndContext ctx hsModule = do
  let textDiagnostics = evaluateTextRulesWithContext ctx
  astDiagnostics <- AST.applyASTRules (recASTRules ctx) (recFilePath ctx) (recContent ctx) hsModule
  pure $ textDiagnostics ++ astDiagnostics

-- | Evaluate rules with automatic parsing (main IO entry point)
--
-- This parses the source file and evaluates both text-based and AST-based rules.
-- For files that fail to parse, only text-based rules are evaluated.
-- Uses consolidated RuleEvalContext to eliminate duplication.
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

-- | Strategy for parallel rule evaluation
data ParallelStrategy
  = ParallelRules        -- ^ Evaluate rules in parallel (good for many rules)
  | ParallelLines        -- ^ Evaluate lines in parallel (good for large files)
  | ParallelBoth         -- ^ Both rules and lines in parallel (most aggressive)
  | SequentialEval       -- ^ No parallelism (baseline)
  | ChunkedParallel Int  -- ^ Process rules in chunks of N in parallel
  deriving stock (Eq, Show, Generic)

instance NFData ParallelStrategy where
  rnf ParallelRules        = ()
  rnf ParallelLines        = ()
  rnf ParallelBoth         = ()
  rnf SequentialEval       = ()
  rnf (ChunkedParallel n)  = rnf n

-- | Default parallel strategy
defaultParallelStrategy :: ParallelStrategy
defaultParallelStrategy = ParallelRules

-- | Evaluate rules in parallel using sparks
--
-- This version evaluates multiple rules concurrently on the same file content.
-- Best for files with many rules to apply.
-- Uses consolidated RuleEvalContext to eliminate duplication.
--
-- @
-- diagnostics <- evaluateRulesParallel ParallelRules engine filepath moduleName content
-- @
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

-- | Parallel rule evaluation with AST support (IO version)
--
-- This version parses the file and evaluates both text-based and AST-based
-- rules with parallelism support. Uses consolidated RuleEvalContext.
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
evaluateRulesParallelWithAST
  :: ParallelStrategy
  -> RuleEngine
  -> FilePath
  -> Text
  -> Text
  -> HsModule GhcPs
  -> IO [Diagnostic]
evaluateRulesParallelWithAST strategy engine filepath moduleName content hsModule =
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
