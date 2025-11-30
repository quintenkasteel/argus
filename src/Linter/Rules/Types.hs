{-# LANGUAGE StrictData #-}

-- |
-- Module      : Linter.Rules.Types
-- Description : Rule system types for the linter
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module defines the type system for linter rules, including
-- pattern matching, transformations, and rule application.
module Linter.Rules.Types
  ( -- * Rule types
    Rule (..)
  , RuleId (..)
  , RuleMatch (..)
  , RuleResult (..)

    -- * Pattern types
  , Pattern (..)
  , PatternVar (..)
  , TypePattern (..)

    -- * Matching context
  , MatchContext (..)
  , Binding (..)
  , Bindings

    -- * Rule application
  , RuleApplication (..)
  , applyRuleResult
  ) where

import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Linter.Types

--------------------------------------------------------------------------------
-- Rule Identifiers
--------------------------------------------------------------------------------

-- | Unique rule identifier
newtype RuleId = RuleId { unRuleId :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

--------------------------------------------------------------------------------
-- Patterns
--------------------------------------------------------------------------------

-- | A pattern variable that can match anything
data PatternVar = PatternVar
  { pvName       :: Text       -- ^ Variable name (e.g., "x", "xs")
  , pvConstraint :: Maybe Text -- ^ Optional type constraint
  }
  deriving stock (Eq, Show, Generic)

-- | A pattern for matching Haskell expressions/types
data Pattern
  = PWildcard                   -- ^ Matches anything (_)
  | PVar PatternVar             -- ^ Matches and binds a variable
  | PLiteral Text               -- ^ Matches a literal value
  | PConstructor Text [Pattern] -- ^ Constructor application (e.g., Entity p v)
  | PApplication Pattern Pattern -- ^ Function application
  | PInfix Pattern Text Pattern  -- ^ Infix operator
  | PTuple [Pattern]             -- ^ Tuple pattern
  | PList [Pattern]              -- ^ List pattern
  | PParens Pattern              -- ^ Parenthesized pattern
  | PTyped Pattern TypePattern   -- ^ Pattern with type annotation
  deriving stock (Eq, Show, Generic)

-- | Type pattern for matching types
data TypePattern
  = TPWildcard                     -- ^ Matches any type (*)
  | TPVar Text                     -- ^ Type variable
  | TPConstructor Text [TypePattern] -- ^ Type constructor application
  | TPFunction TypePattern TypePattern -- ^ Function type
  | TPTuple [TypePattern]          -- ^ Tuple type
  | TPList TypePattern             -- ^ List type
  deriving stock (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Bindings
--------------------------------------------------------------------------------

-- | A binding from a pattern variable to a matched value
data Binding = Binding
  { bindingName  :: Text       -- ^ Pattern variable name
  , bindingValue :: Text       -- ^ Matched source text
  , bindingType  :: Maybe Text -- ^ Inferred type (if available)
  , bindingSpan  :: SrcSpan    -- ^ Source location
  }
  deriving stock (Eq, Show, Generic)

-- | Collection of bindings from pattern matching
type Bindings = Map Text Binding

--------------------------------------------------------------------------------
-- Matching Context
--------------------------------------------------------------------------------

-- | Context for rule matching
data MatchContext = MatchContext
  { mcFilePath     :: FilePath       -- ^ Current file
  , mcModuleName   :: Maybe Text     -- ^ Module name
  , mcFunctionName :: Maybe Text     -- ^ Enclosing function
  , mcBindings     :: Bindings       -- ^ Current bindings
  , mcInScope      :: Map Text Text  -- ^ Names in scope with their types
  }
  deriving stock (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

-- | A linter rule
data Rule = Rule
  { ruleId       :: RuleId           -- ^ Unique identifier
  , ruleName     :: Text             -- ^ Human-readable name
  , rulePattern  :: Pattern          -- ^ Pattern to match
  , ruleFix      :: Maybe Pattern    -- ^ Optional replacement pattern
  , ruleWhere    :: Maybe TypePattern -- ^ Type constraint
  , ruleSeverity :: Severity         -- ^ Severity level
  , ruleMessage  :: Text             -- ^ Message template
  , ruleEnabled  :: Bool             -- ^ Is rule enabled?
  }
  deriving stock (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Rule Matching Results
--------------------------------------------------------------------------------

-- | A successful rule match
data RuleMatch = RuleMatch
  { matchRule     :: RuleId    -- ^ The rule that matched
  , matchSpan     :: SrcSpan   -- ^ Where it matched
  , matchBindings :: Bindings  -- ^ Captured bindings
  , matchOriginal :: Text      -- ^ Original matched text
  }
  deriving stock (Eq, Show, Generic)

-- | Result of applying a rule
data RuleResult = RuleResult
  { resMatch      :: RuleMatch     -- ^ The match
  , resDiagnostic :: Diagnostic    -- ^ Generated diagnostic
  , resFix        :: Maybe Fix     -- ^ Optional fix
  }
  deriving stock (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Rule Application
--------------------------------------------------------------------------------

-- | Represents a pending rule application
data RuleApplication = RuleApplication
  { raFilePath    :: FilePath
  , raResult      :: RuleResult
  , raNewContent  :: Maybe Text  -- ^ New content after fix (if applicable)
  }
  deriving stock (Eq, Show, Generic)

-- | Apply a rule result to generate a diagnostic
applyRuleResult :: Rule -> RuleMatch -> RuleResult
applyRuleResult rule match = RuleResult
  { resMatch = match
  , resDiagnostic = Diagnostic
      { diagSpan = matchSpan match
      , diagSeverity = ruleSeverity rule
      , diagKind = CodePattern
      , diagMessage = interpolateMessage (ruleMessage rule) (matchBindings match)
      , diagCode = Just (unRuleId $ ruleId rule)
      , diagFixes = maybeToList $ ruleFix rule >>= mkFix match
      , diagRelated = []
      }
  , resFix = ruleFix rule >>= mkFix match
  }
  where
    maybeToList Nothing  = []
    maybeToList (Just x) = [x]

-- | Interpolate message with bindings
interpolateMessage :: Text -> Bindings -> Text
interpolateMessage template bindings = foldr replaceVar template (Map.toList bindings)
  where
    replaceVar (name, binding) msg =
      T.replace ("{" <> name <> "}") (bindingValue binding) msg

-- | Create a fix from a pattern and bindings
mkFix :: RuleMatch -> Pattern -> Maybe Fix
mkFix match pat = Just Fix
  { fixTitle = "Apply suggested fix"
  , fixEdits = [FixEdit
      { fixEditSpan = matchSpan match
      , fixEditNewText = substitutePattern pat (matchBindings match)
      }]
  , fixIsPreferred = True
  }

-- | Substitute bindings into a pattern to produce replacement text
substitutePattern :: Pattern -> Bindings -> Text
substitutePattern pat bindings = case pat of
  PWildcard -> "_"
  PVar pv ->
    case Map.lookup (pvName pv) bindings of
      Just b  -> bindingValue b
      Nothing -> pvName pv
  PLiteral t -> t
  PConstructor name pats ->
    name <> " " <> T.intercalate " " (map (`substitutePattern` bindings) pats)
  PApplication f x ->
    substitutePattern f bindings <> " " <> substitutePattern x bindings
  PInfix l op r ->
    substitutePattern l bindings <> " " <> op <> " " <> substitutePattern r bindings
  PTuple pats ->
    "(" <> T.intercalate ", " (map (`substitutePattern` bindings) pats) <> ")"
  PList pats ->
    "[" <> T.intercalate ", " (map (`substitutePattern` bindings) pats) <> "]"
  PParens p ->
    "(" <> substitutePattern p bindings <> ")"
  PTyped p _ ->
    substitutePattern p bindings
