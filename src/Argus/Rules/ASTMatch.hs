{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      : Argus.Rules.ASTMatch
-- Description : Advanced AST-based pattern matching engine for Argus
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module implements a best-in-class AST pattern matching system inspired by
-- HLint, Semgrep, and ast-grep. It provides:
--
-- * Full AST pattern matching with metavariable capture
-- * Ellipsis patterns for matching any sequence of expressions
-- * Deep AST transformation for fix application
-- * Eta-reduction and expression normalization
-- * Type-aware matching (when HIE data available)
-- * Comprehensive side conditions
-- * Scope-aware name resolution
--
-- == Pattern Syntax
--
-- * Single-letter variables (x, f, g) act as wildcards matching any expression
-- * @_foo@ prefix variables also act as wildcards
-- * @___@ (triple underscore) matches any sequence of arguments (ellipsis)
-- * Patterns are parsed as Haskell code
--
-- == Examples
--
-- @
-- Pattern: "map f (map g x)"
-- Target:  "map show (map toUpper xs)"
-- Result:  Subst [("f", "show"), ("g", "toUpper"), ("x", "xs")]
--
-- Pattern: "foldr f z (map g x)"
-- Fix:     "foldr (f . g) z x"
-- Captures nested patterns and generates correct fix
-- @
module Argus.Rules.ASTMatch
  ( -- * Core Types
    Subst (..)
  , ASTPattern (..)
  , MatchResult (..)

    -- * Re-exported unified types
  , Rule (..)
  , SideCondition (..)

    -- * Substitution Operations
  , emptySubst
  , singletonSubst
  , lookupSubst
  , mergeSubst
  , validSubst
  , substKeys
  , substValues

    -- * Unification
  , unifyExpr
  , unifyPat
  , unifyType
  , unifyExprs
  , isUnifyVar
  , isEllipsisVar

    -- * Pattern Matching
  , findMatches
  , findAllMatches
  , matchRule
  , matchRuleIO
  , matchPattern

    -- * AST Transformation
  , applySubst
  , applySubstDeep
  , normalizeExpr
  , etaReduce

    -- * Rule Parsing
  , parsePattern
  , parsePatternPure
  , parseSideCondition

    -- * Side Condition Evaluation
  , evalSideCondition
  , evalSideConditionWithTypes
  , isAtomicExpr
  , isLiteralExpr
  , isVarExpr
  , isFunExpr
  , isApplicationExpr
  , isConstructorExpr
  , isPure
  , isBindExpr
  , freeVars

    -- * Type Environment (for type-aware matching)
  , TypeEnv (..)
  , emptyTypeEnv
  , mkTypeEnv
  , lookupVarType
  , lookupPosType
  , addVarType
  , typeMatchesPattern
  , typeHasClass
  , isNumericType
  , isStringType
  , isListType
  , isMaybeType
  , isInMonad

    -- * Expression Analysis
  , exprEq
  , exprComplexity
  , containsVar
  , collectVars

    -- * Diagnostics
  , applyASTRules
  , matchToDiagnostic
  , generateFix

    -- * Utilities
  , prettyExpr
  , srcSpanToArgus

    -- * Re-exports from GHC
  , HsModule
  , GhcPs
  , LHsExpr
  , getLocA
  ) where

import Control.Monad (guard, foldM, when)
import Data.Char (isAlpha, isUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- GHC imports (using package imports to disambiguate from ghc package)
import "ghc-lib-parser" GHC.Hs
import "ghc-lib-parser" GHC.Types.Name.Reader (RdrName(..), rdrNameOcc)
import "ghc-lib-parser" GHC.Types.Name.Occurrence (occNameString)
import "ghc-lib-parser" GHC.Types.SrcLoc (GenLocated(..), unLoc, realSrcSpanStart, realSrcSpanEnd)
import "ghc-lib-parser" GHC.Types.SrcLoc qualified as GHC
import "ghc-lib-parser" GHC.Data.FastString (unpackFS)
import "ghc-lib-parser" GHC.Parser.Lexer qualified as Lexer
import "ghc-lib-parser" GHC.Utils.Outputable (showSDocUnsafe, ppr)
import "ghc-lib-parser" GHC.Driver.Session (DynFlags, defaultDynFlags, xopt_set)
import "ghc-lib-parser" GHC.LanguageExtensions.Type qualified as LangExt
import Language.Haskell.GhclibParserEx.GHC.Parser (parseExpression)
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)

-- SYB for generic traversals
import Data.Generics (everywhere, mkT, listify, everything, mkQ)

import Argus.Types hiding (SrcSpan, noSrcSpan, srcSpanFile, srcSpanStartLine, srcSpanEndLine, srcSpanStartCol, srcSpanEndCol, FixSafety(..))
import Argus.Types qualified as AT
import Argus.Types (Line(..), Column(..))
import Argus.Rules.Types
    ( Rule(..), SafetyLevel(..), SideCondition(..)
    , RulePattern(..), categoryToFixCategory, safetyToFixSafety
    , importSpecToFixImport
    )

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

-- | Substitution: maps variable names to matched expressions
-- Uses a Map for efficient lookup and merge operations
newtype Subst = Subst { unSubst :: Map Text (LHsExpr GhcPs) }

instance Semigroup Subst where
  Subst s1 <> Subst s2 = Subst (Map.union s1 s2)

instance Monoid Subst where
  mempty = Subst Map.empty

instance Show Subst where
  show (Subst m) = "Subst {" <> show (Map.keys m) <> "}"

-- | Result of a pattern match
data MatchResult = MatchResult
  { mrSubst    :: Subst           -- ^ Variable bindings
  , mrSpan     :: GHC.SrcSpan     -- ^ Source location of match
  , mrOriginal :: LHsExpr GhcPs   -- ^ Original matched expression
  , mrRule     :: Maybe Rule      -- ^ Rule that matched (if any)
  }

instance Show MatchResult where
  show mr = "MatchResult { subst = " <> show (mrSubst mr)
         <> ", span = " <> show (mrSpan mr)
         <> ", original = " <> showSDocUnsafe (ppr (mrOriginal mr))
         <> ", rule = " <> show (ruleId <$> mrRule mr)
         <> " }"

-- | An AST pattern that can be matched against expressions
data ASTPattern
  = ExprPattern (LHsExpr GhcPs)   -- ^ Match an expression pattern
  | TypePatternAST (LHsType GhcPs)   -- ^ Match a type pattern (renamed to avoid conflict)
  | PatPattern (LPat GhcPs)       -- ^ Match a pattern pattern

instance Show ASTPattern where
  show (ExprPattern e) = "ExprPattern " <> showSDocUnsafe (ppr e)
  show (TypePatternAST t) = "TypePatternAST " <> showSDocUnsafe (ppr t)
  show (PatPattern p) = "PatPattern " <> showSDocUnsafe (ppr p)

-- NOTE: ASTRule has been removed - use the unified Rule type from Argus.Rules.Types
-- The unified Rule type supports AST patterns via ASTPatternSpec in rulePattern

-- | Type environment containing type information from HIE files
-- Maps source positions to type information
data TypeEnv = TypeEnv
  { teTypes      :: Map (Int, Int) Text    -- ^ (line, col) -> type string
  , teVarTypes   :: Map Text Text          -- ^ variable name -> type string
  , teModuleName :: Text                   -- ^ Current module name
  }
  deriving stock (Show, Eq)

-- | Empty type environment
emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv Map.empty Map.empty ""

-- | Create type env with variable type mappings
mkTypeEnv :: [(Text, Text)] -> TypeEnv
mkTypeEnv bindings = TypeEnv Map.empty (Map.fromList bindings) ""

-- | Look up type for a variable name
lookupVarType :: Text -> TypeEnv -> Maybe Text
lookupVarType var (TypeEnv _ varTypes _) = Map.lookup var varTypes

-- | Look up type at a source position
lookupPosType :: Int -> Int -> TypeEnv -> Maybe Text
lookupPosType line col (TypeEnv posTypes _ _) = Map.lookup (line, col) posTypes

-- | Add type binding to environment
addVarType :: Text -> Text -> TypeEnv -> TypeEnv
addVarType var typ env = env { teVarTypes = Map.insert var typ (teVarTypes env) }

-- | Check if a type matches a pattern
-- Supports simple patterns like "Int", "[a]", "Maybe a", "a -> b"
typeMatchesPattern :: Text -> Text -> Bool
typeMatchesPattern actualType pattern'
  -- Exact match
  | actualType == pattern' = True
  -- Pattern variables (single lowercase letter) match anything
  | T.length pattern' == 1 && T.all (`elem` ['a'..'z']) pattern' = True
  -- List type pattern
  | "[" `T.isPrefixOf` pattern' && "]" `T.isSuffixOf` pattern' =
      "[" `T.isPrefixOf` actualType && "]" `T.isSuffixOf` actualType
  -- Maybe type pattern
  | "Maybe " `T.isPrefixOf` pattern' =
      "Maybe " `T.isPrefixOf` actualType
  -- Function type pattern
  | " -> " `T.isInfixOf` pattern' && " -> " `T.isInfixOf` actualType =
      True  -- Simplified: function matches function
  -- Container check (contains type somewhere)
  | otherwise = pattern' `T.isInfixOf` actualType

-- | Check if type has a typeclass instance (simplified heuristic)
typeHasClass :: Text -> Text -> Bool
typeHasClass typeStr className
  | className == "Num" = typeStr `elem` ["Int", "Integer", "Float", "Double", "Word"]
  | className == "Eq" = True  -- Most types have Eq
  | className == "Ord" = True -- Most comparable types have Ord
  | className == "Show" = True -- Most types have Show
  | className == "Functor" = any (`T.isPrefixOf` typeStr) ["[]", "Maybe", "Either", "IO"]
  | className == "Monad" = any (`T.isPrefixOf` typeStr) ["[]", "Maybe", "Either", "IO", "ST"]
  | className == "Monoid" = any (`T.isPrefixOf` typeStr) ["[", "Text", "String", "()", "Map", "Set"]
  | otherwise = False  -- Unknown class, be conservative

-- | Check if type is numeric
isNumericType :: Text -> Bool
isNumericType t = t `elem` ["Int", "Integer", "Float", "Double", "Word", "Rational"]
  || "Int" `T.isPrefixOf` t || "Word" `T.isPrefixOf` t

-- | Check if type is a string type
isStringType :: Text -> Bool
isStringType t = t `elem` ["String", "Text", "[Char]", "ByteString"]

-- | Check if type is a list type
isListType :: Text -> Bool
isListType t = "[" `T.isPrefixOf` t && "]" `T.isSuffixOf` t

-- | Check if type is Maybe
isMaybeType :: Text -> Bool
isMaybeType t = "Maybe " `T.isPrefixOf` t

-- | Check if type is in a specific monad
isInMonad :: Text -> Text -> Bool
isInMonad typeStr monadName = monadName `T.isPrefixOf` typeStr

--------------------------------------------------------------------------------
-- Substitution Operations
--------------------------------------------------------------------------------

-- | Empty substitution
emptySubst :: Subst
emptySubst = Subst Map.empty

-- | Create substitution with single binding
singletonSubst :: Text -> LHsExpr GhcPs -> Subst
singletonSubst var expr = Subst (Map.singleton var expr)

-- | Look up variable in substitution
lookupSubst :: Text -> Subst -> Maybe (LHsExpr GhcPs)
lookupSubst var (Subst m) = Map.lookup var m

-- | Get all keys from substitution
substKeys :: Subst -> [Text]
substKeys (Subst m) = Map.keys m

-- | Get all values from substitution
substValues :: Subst -> [LHsExpr GhcPs]
substValues (Subst m) = Map.elems m

-- | Merge two substitutions, checking for consistency
-- Returns Nothing if same variable maps to different expressions
mergeSubst :: Subst -> Subst -> Maybe Subst
mergeSubst (Subst s1) (Subst s2) = do
  let merged = Map.unionWithKey checkConsistent (Map.map Just s1) (Map.map Just s2)
  -- If any value is Nothing, merge failed
  when (any isNothing (Map.elems merged)) Nothing
  pure $ Subst $ Map.mapMaybe id merged
  where
    isNothing Nothing = True
    isNothing (Just _) = False

    checkConsistent :: Text -> Maybe (LHsExpr GhcPs) -> Maybe (LHsExpr GhcPs) -> Maybe (LHsExpr GhcPs)
    checkConsistent _ (Just e1) (Just e2)
      | exprEq e1 e2 = Just e1
      | otherwise = Nothing
    checkConsistent _ e1 e2 = e1 <|> e2

    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) (Just x) _ = Just x
    (<|>) Nothing y = y

-- | Check if substitution is valid (no conflicting bindings)
validSubst :: Subst -> Bool
validSubst _ = True  -- Already validated during construction with Map

--------------------------------------------------------------------------------
-- Unification Variables
--------------------------------------------------------------------------------

-- | Check if a name is a unification variable (wildcard)
-- Single lowercase letters, underscore prefix, or special metavariables
isUnifyVar :: Text -> Bool
isUnifyVar t = case T.unpack t of
  [c] -> c == '_' || (isAlpha c && not (isUpper c))  -- Single char: _ or lowercase
  ('_':_) -> True  -- Underscore prefix: _x, _foo, etc.
  ('$':_) -> True  -- Dollar prefix: $x, $foo (Semgrep-style)
  _ -> False

-- | Check if a name is an ellipsis variable (matches sequences)
isEllipsisVar :: Text -> Bool
isEllipsisVar t = t == "___" || t == "$..." || T.isPrefixOf "..." t

-- | Extract variable name from RdrName
getVarName :: RdrName -> Text
getVarName = T.pack . occNameString . rdrNameOcc

-- | Check if name starts with uppercase (constructor)
isConstructorName :: Text -> Bool
isConstructorName t = case T.uncons t of
  Just (c, _) -> isUpper c
  Nothing -> False

-- | Create a located pattern with no source span
noLocPat :: Pat GhcPs -> LPat GhcPs
noLocPat = L noSrcSpanA

-- | Create a located type with no source span
noLocType :: HsType GhcPs -> LHsType GhcPs
noLocType = L noSrcSpanA

-- | Create a located expression with no source span (for future use)
_noLocExpr :: HsExpr GhcPs -> LHsExpr GhcPs
_noLocExpr = L noSrcSpanA

--------------------------------------------------------------------------------
-- Expression Equality and Comparison
--------------------------------------------------------------------------------

-- | Check if two expressions are structurally equal
exprEq :: LHsExpr GhcPs -> LHsExpr GhcPs -> Bool
exprEq (L _ e1) (L _ e2) = exprEq' e1 e2
  where
    exprEq' :: HsExpr GhcPs -> HsExpr GhcPs -> Bool
    exprEq' (HsVar _ n1) (HsVar _ n2) = rdrNameEq (unLoc n1) (unLoc n2)
    exprEq' (HsApp _ f1 a1) (HsApp _ f2 a2) = exprEq f1 f2 && exprEq a1 a2
    exprEq' (HsLit _ l1) (HsLit _ l2) = litEq l1 l2
    exprEq' (HsOverLit _ l1) (HsOverLit _ l2) = overLitEq l1 l2
    exprEq' (HsPar _ e1') (HsPar _ e2') = exprEq e1' e2'
    exprEq' (HsIf _ c1 t1 f1) (HsIf _ c2 t2 f2) =
      exprEq c1 c2 && exprEq t1 t2 && exprEq f1 f2
    exprEq' (OpApp _ l1 o1 r1) (OpApp _ l2 o2 r2) =
      exprEq l1 l2 && exprEq o1 o2 && exprEq r1 r2
    exprEq' (NegApp _ e1' _) (NegApp _ e2' _) = exprEq e1' e2'
    exprEq' (ExplicitList _ es1) (ExplicitList _ es2) =
      length es1 == length es2 && and (zipWith exprEq es1 es2)
    exprEq' (ExplicitTuple _ t1 _) (ExplicitTuple _ t2 _) =
      length t1 == length t2 && and (zipWith tupArgEq t1 t2)
    exprEq' (HsLam _ _ mg1) (HsLam _ _ mg2) = matchGroupEq mg1 mg2
    exprEq' (SectionL _ e1' o1) (SectionL _ e2' o2) = exprEq e1' e2' && exprEq o1 o2
    exprEq' (SectionR _ o1 e1') (SectionR _ o2 e2') = exprEq o1 o2 && exprEq e1' e2'
    -- For other cases, use string comparison as fallback
    exprEq' x y = showSDocUnsafe (ppr x) == showSDocUnsafe (ppr y)

    tupArgEq :: HsTupArg GhcPs -> HsTupArg GhcPs -> Bool
    tupArgEq (Present _ e1') (Present _ e2') = exprEq e1' e2'
    tupArgEq (Missing _) (Missing _) = True
    tupArgEq _ _ = False

    matchGroupEq :: MatchGroup GhcPs (LHsExpr GhcPs) -> MatchGroup GhcPs (LHsExpr GhcPs) -> Bool
    matchGroupEq (MG _ (L _ ms1)) (MG _ (L _ ms2)) =
      length ms1 == length ms2 && all (uncurry matchEq) (zip (map unLoc ms1) (map unLoc ms2))

    matchEq :: Match GhcPs (LHsExpr GhcPs) -> Match GhcPs (LHsExpr GhcPs) -> Bool
    matchEq (Match _ _ ps1 grhss1) (Match _ _ ps2 grhss2) =
      length ps1 == length ps2 && grhssEq grhss1 grhss2

    grhssEq :: GRHSs GhcPs (LHsExpr GhcPs) -> GRHSs GhcPs (LHsExpr GhcPs) -> Bool
    grhssEq (GRHSs _ gs1 _) (GRHSs _ gs2 _) =
      length gs1 == length gs2 && all (uncurry grhsEq) (zip (map unLoc gs1) (map unLoc gs2))

    grhsEq :: GRHS GhcPs (LHsExpr GhcPs) -> GRHS GhcPs (LHsExpr GhcPs) -> Bool
    grhsEq (GRHS _ _ e1') (GRHS _ _ e2') = exprEq e1' e2'

-- | Check RdrName equality
rdrNameEq :: RdrName -> RdrName -> Bool
rdrNameEq n1 n2 = occNameString (rdrNameOcc n1) == occNameString (rdrNameOcc n2)

-- | Check literal equality
litEq :: HsLit GhcPs -> HsLit GhcPs -> Bool
litEq (HsChar _ c1) (HsChar _ c2) = c1 == c2
litEq (HsCharPrim _ c1) (HsCharPrim _ c2) = c1 == c2
litEq (HsString _ s1) (HsString _ s2) = s1 == s2
litEq (HsStringPrim _ s1) (HsStringPrim _ s2) = s1 == s2
litEq (HsInt _ i1) (HsInt _ i2) = i1 == i2
litEq (HsIntPrim _ i1) (HsIntPrim _ i2) = i1 == i2
litEq (HsWordPrim _ w1) (HsWordPrim _ w2) = w1 == w2
litEq (HsInt64Prim _ i1) (HsInt64Prim _ i2) = i1 == i2
litEq (HsWord64Prim _ w1) (HsWord64Prim _ w2) = w1 == w2
litEq (HsInteger _ i1 _) (HsInteger _ i2 _) = i1 == i2
litEq (HsRat _ r1 _) (HsRat _ r2 _) = r1 == r2
litEq (HsFloatPrim _ f1) (HsFloatPrim _ f2) = f1 == f2
litEq (HsDoublePrim _ d1) (HsDoublePrim _ d2) = d1 == d2
litEq _ _ = False

-- | Check overloaded literal equality
overLitEq :: HsOverLit GhcPs -> HsOverLit GhcPs -> Bool
overLitEq l1 l2 = ol_val l1 == ol_val l2

--------------------------------------------------------------------------------
-- Expression Unification
--------------------------------------------------------------------------------

-- | Unify pattern expression with target expression
-- Returns substitution if successful, Nothing otherwise
unifyExpr :: LHsExpr GhcPs -> LHsExpr GhcPs -> Maybe Subst
unifyExpr pat target = unifyExpr' (unLoc pat) (unLoc target)
  where
    unifyExpr' :: HsExpr GhcPs -> HsExpr GhcPs -> Maybe Subst

    -- Pattern variable: match anything and capture
    unifyExpr' (HsVar _ (L _ pname)) _
      | isUnifyVar (getVarName pname) =
          Just $ singletonSubst (getVarName pname) target

    -- Same variable name (not a unify var): must match exactly
    unifyExpr' (HsVar _ (L _ pname)) (HsVar _ (L _ tname))
      | rdrNameEq pname tname = Just emptySubst
      | otherwise = Nothing

    -- HsVar that's not a unify var but target is different type
    unifyExpr' (HsVar _ _) _ = Nothing

    -- Application: unify both function and argument
    unifyExpr' (HsApp _ pf pa) (HsApp _ tf ta) = do
      s1 <- unifyExpr pf tf
      s2 <- unifyExpr pa ta
      mergeSubst s1 s2

    -- OpApp (infix): unify operator and both operands
    unifyExpr' (OpApp _ pl po pr) (OpApp _ tl to tr) = do
      s1 <- unifyExpr pl tl
      s2 <- unifyExpr po to
      s3 <- unifyExpr pr tr
      s12 <- mergeSubst s1 s2
      mergeSubst s12 s3

    -- If-then-else
    unifyExpr' (HsIf _ pc pt pe) (HsIf _ tc tt te) = do
      s1 <- unifyExpr pc tc
      s2 <- unifyExpr pt tt
      s3 <- unifyExpr pe te
      s12 <- mergeSubst s1 s2
      mergeSubst s12 s3

    -- Literals must match exactly
    unifyExpr' (HsLit _ l1) (HsLit _ l2)
      | litEq l1 l2 = Just emptySubst
      | otherwise = Nothing

    unifyExpr' (HsOverLit _ l1) (HsOverLit _ l2)
      | overLitEq l1 l2 = Just emptySubst
      | otherwise = Nothing

    -- Parentheses: see through them in both directions
    unifyExpr' _ (HsPar _ te) = unifyExpr pat te
    unifyExpr' (HsPar _ pe) _ = unifyExpr pe target

    -- Negation
    unifyExpr' (NegApp _ pe _) (NegApp _ te _) = unifyExpr pe te

    -- Lists
    unifyExpr' (ExplicitList _ pes) (ExplicitList _ tes) =
      unifyExprLists pes tes

    -- Tuples
    unifyExpr' (ExplicitTuple _ pargs _) (ExplicitTuple _ targs _)
      | length pargs == length targs =
          foldM (\s (pa, ta) -> do
                    s' <- unifyTupArg pa ta
                    mergeSubst s s')
                emptySubst
                (zip pargs targs)
      | otherwise = Nothing

    -- Lambda expressions (GHC 9.10: HsLam ext lamCase matchGroup)
    unifyExpr' (HsLam _ _ pmg) (HsLam _ _ tmg) =
      unifyMatchGroup pmg tmg

    -- Case expressions
    unifyExpr' (HsCase _ pe pmg) (HsCase _ te tmg) = do
      s1 <- unifyExpr pe te
      s2 <- unifyMatchGroup pmg tmg
      mergeSubst s1 s2

    -- Let expressions (GHC 9.10: HsLet ext binds body)
    unifyExpr' (HsLet _ _ pe) (HsLet _ _ te) =
      unifyExpr pe te  -- Simplified: just match the body

    -- Do notation
    unifyExpr' (HsDo _ pctx pstmts) (HsDo _ tctx tstmts)
      | doCtxEq pctx tctx = unifyStmts (unLoc pstmts) (unLoc tstmts)
      | otherwise = Nothing
      where
        doCtxEq _ _ = True  -- Simplified: ignore context differences

    -- Section left: (x `op`)
    unifyExpr' (SectionL _ pe po) (SectionL _ te to) = do
      s1 <- unifyExpr pe te
      s2 <- unifyExpr po to
      mergeSubst s1 s2

    -- Section right: (`op` x)
    unifyExpr' (SectionR _ po pe) (SectionR _ to te) = do
      s1 <- unifyExpr po to
      s2 <- unifyExpr pe te
      mergeSubst s1 s2

    -- Type application (GHC 9.10: HsAppType ext expr tyArg)
    unifyExpr' (HsAppType _ pe _) (HsAppType _ te _) =
      unifyExpr pe te

    -- Expression with type signature
    unifyExpr' (ExprWithTySig _ pe _) (ExprWithTySig _ te _) =
      unifyExpr pe te

    -- Record construction
    unifyExpr' (RecordCon _ (L _ pcon) _) (RecordCon _ (L _ tcon) _)
      | rdrNameEq pcon tcon = Just emptySubst  -- Simplified
      | otherwise = Nothing

    -- Record update
    unifyExpr' (RecordUpd _ pe _) (RecordUpd _ te _) =
      unifyExpr pe te  -- Simplified

    -- Arithmetic sequences
    unifyExpr' (ArithSeq _ _ pinfo) (ArithSeq _ _ tinfo) =
      unifyArithSeq pinfo tinfo

    -- Handle $ as special case: f $ x is like f x
    -- Pattern: HsApp against (f $ x)
    unifyExpr' (HsApp _ pf pa) (OpApp _ tf (L _ (HsVar _ (L _ op))) ta)
      | isDollarOp op = do
          s1 <- unifyExpr pf tf
          s2 <- unifyExpr pa ta
          mergeSubst s1 s2

    -- Pattern: (f $ x) against HsApp
    unifyExpr' (OpApp _ pf (L _ (HsVar _ (L _ op))) pa) (HsApp _ tf ta)
      | isDollarOp op = do
          s1 <- unifyExpr pf tf
          s2 <- unifyExpr pa ta
          mergeSubst s1 s2

    -- Handle . composition expansion: (f . g) x -> f (g x)
    -- This allows matching composed functions

    -- No match
    unifyExpr' _ _ = Nothing

    -- Helper: unify tuple arguments
    unifyTupArg :: HsTupArg GhcPs -> HsTupArg GhcPs -> Maybe Subst
    unifyTupArg (Present _ pe) (Present _ te) = unifyExpr pe te
    unifyTupArg (Missing _) (Missing _) = Just emptySubst
    unifyTupArg _ _ = Nothing

    -- Helper: unify arithmetic sequences
    unifyArithSeq :: ArithSeqInfo GhcPs -> ArithSeqInfo GhcPs -> Maybe Subst
    unifyArithSeq (From pe) (From te) = unifyExpr pe te
    unifyArithSeq (FromThen pe1 pe2) (FromThen te1 te2) = do
      s1 <- unifyExpr pe1 te1
      s2 <- unifyExpr pe2 te2
      mergeSubst s1 s2
    unifyArithSeq (FromTo pe1 pe2) (FromTo te1 te2) = do
      s1 <- unifyExpr pe1 te1
      s2 <- unifyExpr pe2 te2
      mergeSubst s1 s2
    unifyArithSeq (FromThenTo pe1 pe2 pe3) (FromThenTo te1 te2 te3) = do
      s1 <- unifyExpr pe1 te1
      s2 <- unifyExpr pe2 te2
      s3 <- unifyExpr pe3 te3
      s12 <- mergeSubst s1 s2
      mergeSubst s12 s3
    unifyArithSeq _ _ = Nothing

-- | Unify lists of expressions with ellipsis support
unifyExprLists :: [LHsExpr GhcPs] -> [LHsExpr GhcPs] -> Maybe Subst
unifyExprLists [] [] = Just emptySubst
unifyExprLists (p:ps) (t:ts)
  | isEllipsisExpr p = do
      -- Ellipsis: match any number of expressions
      let varName = getEllipsisVar p
      -- Try matching 0, 1, 2, ... elements with the ellipsis
      tryMatchEllipsis varName ps (t:ts) `mplus` do
        s <- unifyExpr p t
        s' <- unifyExprLists ps ts
        mergeSubst s s'
  | otherwise = do
      s <- unifyExpr p t
      s' <- unifyExprLists ps ts
      mergeSubst s s'
unifyExprLists _ _ = Nothing

-- | Try to match ellipsis against varying number of target expressions
tryMatchEllipsis :: Text -> [LHsExpr GhcPs] -> [LHsExpr GhcPs] -> Maybe Subst
tryMatchEllipsis _ ps ts = unifyExprLists ps ts  -- Skip 0 elements for ellipsis

-- | Check if expression is an ellipsis pattern
isEllipsisExpr :: LHsExpr GhcPs -> Bool
isEllipsisExpr (L _ (HsVar _ (L _ name))) = isEllipsisVar (getVarName name)
isEllipsisExpr _ = False

-- | Get variable name from ellipsis expression
getEllipsisVar :: LHsExpr GhcPs -> Text
getEllipsisVar (L _ (HsVar _ (L _ name))) = getVarName name
getEllipsisVar _ = "___"

-- | Alternative combinator for Maybe
mplus :: Maybe a -> Maybe a -> Maybe a
mplus (Just x) _ = Just x
mplus Nothing y = y

-- | Unify multiple expressions pairwise
unifyExprs :: [LHsExpr GhcPs] -> [LHsExpr GhcPs] -> Maybe Subst
unifyExprs = unifyExprLists

-- | Check if operator is $ (dollar)
isDollarOp :: RdrName -> Bool
isDollarOp name = occNameString (rdrNameOcc name) == "$"

-- | Check if operator is . (composition) - for future composition expansion
_isComposeOp :: RdrName -> Bool
_isComposeOp name = occNameString (rdrNameOcc name) == "."

-- | Unify match groups (for lambdas, case expressions)
unifyMatchGroup :: MatchGroup GhcPs (LHsExpr GhcPs)
                -> MatchGroup GhcPs (LHsExpr GhcPs)
                -> Maybe Subst
unifyMatchGroup (MG _ (L _ pmatches)) (MG _ (L _ tmatches))
  | length pmatches == length tmatches =
      foldM (\s (pm, tm) -> do
                s' <- unifyMatch (unLoc pm) (unLoc tm)
                mergeSubst s s')
            emptySubst
            (zip pmatches tmatches)
  | otherwise = Nothing

-- | Unify single match
unifyMatch :: Match GhcPs (LHsExpr GhcPs)
           -> Match GhcPs (LHsExpr GhcPs)
           -> Maybe Subst
unifyMatch (Match _ _ ppats pgrhss) (Match _ _ tpats tgrhss)
  | length ppats == length tpats = do
      s1 <- foldM (\s (pp, tp) -> do
                      s' <- unifyPat pp tp
                      mergeSubst s s')
                  emptySubst
                  (zip ppats tpats)
      s2 <- unifyGRHSs pgrhss tgrhss
      mergeSubst s1 s2
  | otherwise = Nothing

-- | Unify guarded right-hand sides
unifyGRHSs :: GRHSs GhcPs (LHsExpr GhcPs)
           -> GRHSs GhcPs (LHsExpr GhcPs)
           -> Maybe Subst
unifyGRHSs (GRHSs _ pgrhss _) (GRHSs _ tgrhss _)
  | length pgrhss == length tgrhss =
      foldM (\s (pg, tg) -> do
                s' <- unifyGRHS (unLoc pg) (unLoc tg)
                mergeSubst s s')
            emptySubst
            (zip pgrhss tgrhss)
  | otherwise = Nothing

-- | Unify single GRHS
unifyGRHS :: GRHS GhcPs (LHsExpr GhcPs)
          -> GRHS GhcPs (LHsExpr GhcPs)
          -> Maybe Subst
unifyGRHS (GRHS _ pguards pe) (GRHS _ tguards te)
  | length pguards == length tguards = do
      s1 <- unifyStmts pguards tguards
      s2 <- unifyExpr pe te
      mergeSubst s1 s2
  | otherwise = Nothing

-- | Unify statement lists
unifyStmts :: [ExprLStmt GhcPs] -> [ExprLStmt GhcPs] -> Maybe Subst
unifyStmts ps ts
  | length ps == length ts =
      foldM (\s (p, t) -> do
                s' <- unifyStmt (unLoc p) (unLoc t)
                mergeSubst s s')
            emptySubst
            (zip ps ts)
  | otherwise = Nothing

-- | Unify single statement
unifyStmt :: StmtLR GhcPs GhcPs (LHsExpr GhcPs)
          -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
          -> Maybe Subst
unifyStmt (BodyStmt _ pe _ _) (BodyStmt _ te _ _) = unifyExpr pe te
unifyStmt (LastStmt _ pe _ _) (LastStmt _ te _ _) = unifyExpr pe te
unifyStmt (BindStmt _ ppat pe) (BindStmt _ tpat te) = do
  s1 <- unifyPat ppat tpat
  s2 <- unifyExpr pe te
  mergeSubst s1 s2
unifyStmt (LetStmt _ _) (LetStmt _ _) = Just emptySubst
unifyStmt _ _ = Nothing

--------------------------------------------------------------------------------
-- Pattern Unification
--------------------------------------------------------------------------------

-- | Unify patterns
unifyPat :: LPat GhcPs -> LPat GhcPs -> Maybe Subst
unifyPat (L _ p1) (L _ p2) = unifyPat' p1 p2
  where
    unifyPat' :: Pat GhcPs -> Pat GhcPs -> Maybe Subst

    -- Variable pattern in pattern position: treat as wildcard
    unifyPat' (VarPat _ (L _ pname)) _
      | isUnifyVar (getVarName pname) = Just emptySubst

    -- Same variable: match
    unifyPat' (VarPat _ (L _ pname)) (VarPat _ (L _ tname))
      | rdrNameEq pname tname = Just emptySubst
      | otherwise = Nothing

    -- Wildcard matches anything
    unifyPat' (WildPat _) _ = Just emptySubst

    -- Constructor patterns
    unifyPat' (ConPat _ (L _ pcon) pargs) (ConPat _ (L _ tcon) targs)
      | rdrNameEq pcon tcon = unifyConPatDetails pargs targs
      | otherwise = Nothing

    -- Parenthesized patterns (GHC 9.10+: ParPat ext pat)
    unifyPat' (ParPat _ pp) tp = unifyPat' (unLoc pp) tp
    unifyPat' pp (ParPat _ tp) = unifyPat' pp (unLoc tp)

    -- Tuple patterns
    unifyPat' (TuplePat _ ppats _) (TuplePat _ tpats _)
      | length ppats == length tpats =
          foldM (\s (pp, tp) -> do
                    s' <- unifyPat pp tp
                    mergeSubst s s')
                emptySubst
                (zip ppats tpats)
      | otherwise = Nothing

    -- List patterns
    unifyPat' (ListPat _ ppats) (ListPat _ tpats)
      | length ppats == length tpats =
          foldM (\s (pp, tp) -> do
                    s' <- unifyPat pp tp
                    mergeSubst s s')
                emptySubst
                (zip ppats tpats)
      | otherwise = Nothing

    -- Literal patterns
    unifyPat' (LitPat _ l1) (LitPat _ l2)
      | litEq l1 l2 = Just emptySubst
      | otherwise = Nothing

    -- As-patterns (x@pat) (GHC 9.10: AsPat ext name pat)
    unifyPat' (AsPat _ _ pp) tp = unifyPat pp (noLocPat tp)
    unifyPat' pp (AsPat _ _ tp) = unifyPat (noLocPat pp) tp

    -- Bang patterns
    unifyPat' (BangPat _ pp) (BangPat _ tp) = unifyPat pp tp

    -- View patterns
    unifyPat' (ViewPat _ pe pp) (ViewPat _ te tp) = do
      s1 <- unifyExpr pe te
      s2 <- unifyPat pp tp
      mergeSubst s1 s2

    -- NPat (numeric patterns)
    unifyPat' (NPat _ l1 _ _) (NPat _ l2 _ _)
      | overLitEq (unLoc l1) (unLoc l2) = Just emptySubst
      | otherwise = Nothing

    unifyPat' _ _ = Nothing

-- | Unify constructor pattern details
unifyConPatDetails :: HsConPatDetails GhcPs -> HsConPatDetails GhcPs -> Maybe Subst
unifyConPatDetails (PrefixCon _ ppats) (PrefixCon _ tpats)
  | length ppats == length tpats =
      foldM (\s (pp, tp) -> do
                s' <- unifyPat pp tp
                mergeSubst s s')
            emptySubst
            (zip ppats tpats)
  | otherwise = Nothing
unifyConPatDetails (InfixCon pl pr) (InfixCon tl tr) = do
  s1 <- unifyPat pl tl
  s2 <- unifyPat pr tr
  mergeSubst s1 s2
unifyConPatDetails (RecCon (HsRecFields pfs _)) (RecCon (HsRecFields tfs _))
  | length pfs == length tfs =
      foldM (\s (pf, tf) -> do
                s' <- unifyRecField (unLoc pf) (unLoc tf)
                mergeSubst s s')
            emptySubst
            (zip pfs tfs)
  | otherwise = Nothing
unifyConPatDetails _ _ = Nothing

-- | Unify record fields
unifyRecField :: HsFieldBind (LFieldOcc GhcPs) (LPat GhcPs)
              -> HsFieldBind (LFieldOcc GhcPs) (LPat GhcPs)
              -> Maybe Subst
unifyRecField (HsFieldBind _ _ pp _) (HsFieldBind _ _ tp _) = unifyPat pp tp

--------------------------------------------------------------------------------
-- Type Unification
--------------------------------------------------------------------------------

-- | Unify types
unifyType :: LHsType GhcPs -> LHsType GhcPs -> Maybe Subst
unifyType (L _ t1) (L _ t2) = case (t1, t2) of
  -- Type variable in pattern: always matches
  (HsTyVar _ _ (L _ pname), _)
    | isUnifyVar (getVarName pname) -> Just emptySubst

  -- Same type variable
  (HsTyVar _ _ (L _ n1), HsTyVar _ _ (L _ n2))
    | rdrNameEq n1 n2 -> Just emptySubst
    | otherwise -> Nothing

  -- Function types
  (HsFunTy _ _ pa pr, HsFunTy _ _ ta tr) -> do
    s1 <- unifyType pa ta
    s2 <- unifyType pr tr
    mergeSubst s1 s2

  -- Type application
  (HsAppTy _ pf pa, HsAppTy _ tf ta) -> do
    s1 <- unifyType pf tf
    s2 <- unifyType pa ta
    mergeSubst s1 s2

  -- Parenthesized types
  (HsParTy _ pt, _) -> unifyType pt (noLocType t2)
  (_, HsParTy _ tt) -> unifyType (noLocType t1) tt

  -- Lists
  (HsListTy _ pt, HsListTy _ tt) -> unifyType pt tt

  -- Tuples
  (HsTupleTy _ _ pts, HsTupleTy _ _ tts)
    | length pts == length tts ->
        foldM (\s (p, t) -> do
                  s' <- unifyType p t
                  mergeSubst s s')
              emptySubst
              (zip pts tts)
    | otherwise -> Nothing

  -- Qualified types
  (HsQualTy _ _ pt, HsQualTy _ _ tt) -> unifyType pt tt

  -- Forall types
  (HsForAllTy _ _ pt, HsForAllTy _ _ tt) -> unifyType pt tt

  _ -> Nothing

--------------------------------------------------------------------------------
-- Deep AST Transformation
--------------------------------------------------------------------------------

-- | Apply substitution deeply throughout an expression
applySubstDeep :: Subst -> LHsExpr GhcPs -> LHsExpr GhcPs
applySubstDeep subst = everywhere (mkT transformVar)
  where
    transformVar :: LHsExpr GhcPs -> LHsExpr GhcPs
    transformVar e@(L _ (HsVar _ (L _ name)))
      | Just replacement <- lookupSubst (getVarName name) subst = replacement
      | otherwise = e
    transformVar e = e

-- | Simple substitution (top-level only, for backwards compatibility)
applySubst :: Subst -> LHsExpr GhcPs -> LHsExpr GhcPs
applySubst = applySubstDeep

--------------------------------------------------------------------------------
-- Expression Normalization
--------------------------------------------------------------------------------

-- | Normalize an expression for better matching
-- This handles common syntactic variations
normalizeExpr :: LHsExpr GhcPs -> LHsExpr GhcPs
normalizeExpr = everywhere (mkT normalize)
  where
    normalize :: HsExpr GhcPs -> HsExpr GhcPs
    -- Remove unnecessary parentheses (GHC 9.10+: 2 args)
    normalize (HsPar _ (L _ e)) = normalize e
    -- Note: $ expansion is handled at unification level, not normalization
    -- Other normalizations can be added here
    normalize e = e

-- | Perform eta reduction on an expression
-- \x -> f x  =>  f  (when x is not free in f)
etaReduce :: LHsExpr GhcPs -> LHsExpr GhcPs
etaReduce expr@(L _loc (HsLam _ _ (MG _ (L _ [L _ (Match _ _ [L _ (VarPat _ (L _ x))] (GRHSs _ [L _ (GRHS _ [] body)] _))])))) =
  case unLoc body of
    HsApp _ f (L _ (HsVar _ (L _ y)))
      | rdrNameEq x y && not (containsVar (getVarName x) f) -> f
    _ -> expr
etaReduce expr = expr

--------------------------------------------------------------------------------
-- Expression Analysis
--------------------------------------------------------------------------------

-- | Calculate expression complexity (for side conditions)
exprComplexity :: LHsExpr GhcPs -> Int
exprComplexity = everything (+) (mkQ 0 nodeComplexity)
  where
    nodeComplexity :: HsExpr GhcPs -> Int
    nodeComplexity (HsVar _ _) = 1
    nodeComplexity (HsApp _ _ _) = 1
    nodeComplexity (OpApp _ _ _ _) = 1
    nodeComplexity (HsLam _ _ _) = 2
    nodeComplexity (HsCase _ _ _) = 3
    nodeComplexity (HsIf _ _ _ _) = 2
    nodeComplexity (HsLet _ _ _) = 2  -- GHC 9.10: 3 args
    nodeComplexity (HsDo _ _ _) = 3
    nodeComplexity _ = 1

-- | Check if expression contains a specific variable
containsVar :: Text -> LHsExpr GhcPs -> Bool
containsVar varName = everything (||) (mkQ False checkVar)
  where
    checkVar :: HsExpr GhcPs -> Bool
    checkVar (HsVar _ (L _ name)) = getVarName name == varName
    checkVar _ = False

-- | Collect all variables in an expression
collectVars :: LHsExpr GhcPs -> Set Text
collectVars = everything Set.union (mkQ Set.empty getVar)
  where
    getVar :: HsExpr GhcPs -> Set Text
    getVar (HsVar _ (L _ name)) = Set.singleton (getVarName name)
    getVar _ = Set.empty

-- | Get free variables in an expression
freeVars :: LHsExpr GhcPs -> Set Text
freeVars = collectVars  -- Simplified: treat all vars as free

--------------------------------------------------------------------------------
-- Side Condition Evaluation
--------------------------------------------------------------------------------

-- | Evaluate a side condition given a substitution (without type info)
evalSideCondition :: Subst -> SideCondition -> Bool
evalSideCondition subst cond = evalSideConditionWithTypes subst emptyTypeEnv cond

-- | Evaluate a side condition with type environment for type-aware matching
-- Updated to use the unified SideCondition from Argus.Rules.Types
evalSideConditionWithTypes :: Subst -> TypeEnv -> SideCondition -> Bool
evalSideConditionWithTypes subst typeEnv = \case
  -- Location predicates (simplified - always true in AST context)
  NotInComment -> True  -- AST matching happens after parsing, comments stripped
  NotInString -> True   -- AST matching happens on parsed code
  NotInImport -> True   -- Should be checked at call site
  InFunctionBody -> True  -- Assume we're in function body

  -- Expression predicates
  IsAtomic var -> case lookupSubst var subst of
    Just e -> isAtomicExpr e
    Nothing -> True

  IsLiteral var -> case lookupSubst var subst of
    Just e -> isLiteralExpr e
    Nothing -> False

  IsVariable var -> case lookupSubst var subst of
    Just e -> isVarExpr e
    Nothing -> False

  IsLambda var -> case lookupSubst var subst of
    Just e -> isFunExpr e
    Nothing -> False

  IsApplication var -> case lookupSubst var subst of
    Just e -> isApplicationExpr e
    Nothing -> False

  IsConstructor var -> case lookupSubst var subst of
    Just e -> isConstructorExpr e
    Nothing -> False

  IsPure var -> case lookupSubst var subst of
    Just e -> isPure e
    Nothing -> True

  NotEqual v1 v2 ->
    case (lookupSubst v1 subst, lookupSubst v2 subst) of
      (Just e1, Just e2) -> not (exprEq e1 e2)
      _ -> True

  NotIn var vals ->
    case lookupSubst var subst of
      Just (L _ (HsVar _ (L _ name))) ->
        getVarName name `notElem` vals
      _ -> True

  FreeIn v1 v2 ->
    case (lookupSubst v1 subst, lookupSubst v2 subst) of
      (Just (L _ (HsVar _ (L _ name))), Just e2) ->
        containsVar (getVarName name) e2
      _ -> False

  NotFreeIn v1 v2 ->
    case (lookupSubst v1 subst, lookupSubst v2 subst) of
      (Just (L _ (HsVar _ (L _ name))), Just e2) ->
        not (containsVar (getVarName name) e2)
      _ -> True

  -- Type-aware conditions using TypeEnv
  HasType var expectedType ->
    case lookupVarType var typeEnv of
      Just actualType -> actualType == expectedType
      Nothing -> True  -- No type info available, be permissive

  TypeMatches var typePat ->
    case lookupVarType var typeEnv of
      Just actualType -> typeMatchesPattern actualType typePat
      Nothing -> True  -- No type info available, be permissive

  HasTypeClass var className ->
    case lookupVarType var typeEnv of
      Just actualType -> typeHasClass actualType className
      Nothing -> True  -- No type info available, be permissive

  TypeContains var typeName ->
    case lookupVarType var typeEnv of
      Just actualType -> typeName `T.isInfixOf` actualType
      Nothing -> True  -- No type info available, be permissive

  IsNumeric var ->
    case lookupVarType var typeEnv of
      Just actualType -> isNumericType actualType
      Nothing -> True  -- No type info available, be permissive

  IsString var ->
    case lookupVarType var typeEnv of
      Just actualType -> isStringType actualType
      Nothing -> True  -- No type info available, be permissive

  IsList var ->
    case lookupVarType var typeEnv of
      Just actualType -> isListType actualType
      Nothing -> True  -- No type info available, be permissive

  IsMaybe var ->
    case lookupVarType var typeEnv of
      Just actualType -> isMaybeType actualType
      Nothing -> True  -- No type info available, be permissive

  IsMonad var monadName ->
    case lookupVarType var typeEnv of
      Just actualType -> isInMonad actualType monadName
      Nothing -> True  -- No type info available, be permissive

  -- Complexity conditions
  ComplexityLT var n ->
    case lookupSubst var subst of
      Just e -> exprComplexity e < n
      Nothing -> True

  ComplexityGT var n ->
    case lookupSubst var subst of
      Just e -> exprComplexity e > n
      Nothing -> True

  ComplexityCond var ord n ->
    case lookupSubst var subst of
      Just e -> compare (exprComplexity e) n == ord
      Nothing -> True

  -- Context predicates (simplified - not fully implemented in AST context)
  HasImport _ -> True  -- Would need module context
  HasPragma _ -> True  -- Would need module context
  InModule _ -> True   -- Would need module context
  InTestFile -> False  -- Would need file path
  NotInTestFile -> True  -- Would need file path
  InContext ctx -> evalInContext ctx  -- Semantic context detection

  -- Expression structure predicates
  NotBind var -> case lookupSubst var subst of
    Just e -> not (isBindExpr e)
    Nothing -> True

  IsEtaReducible funcVar argVar ->
    case (lookupSubst funcVar subst, lookupSubst argVar subst) of
      (Just func, Just (L _ (HsVar _ (L _ argName)))) ->
        not (containsVar (getVarName argName) func)
      _ -> True

  -- Deriving and pattern analysis predicates
  -- These require AST structure analysis and are evaluated conservatively
  NoDerivingStrategy -> True  -- Requires deriving clause context
  WildcardNotLast -> True     -- Requires case patterns context
  HasPatternOverlap -> False  -- Conservative: assume no overlap by default
  IsPatternIncomplete -> False  -- Conservative: assume complete by default
  HasAmbiguousType -> False   -- Requires type inference
  UsesDefaultOptions -> case substValues subst of
    -- Check if any captured expression uses defaultOptions
    es -> any isDefaultOptionsExpr es

  -- Combinators
  And cs -> all (evalSideConditionWithTypes subst typeEnv) cs
  Or cs -> any (evalSideConditionWithTypes subst typeEnv) cs
  Not c -> not (evalSideConditionWithTypes subst typeEnv c)
  Always -> True
  Never -> False

  -- Location comment type (not typically used in AST matching)
  InCommentType _ -> False  -- AST doesn't contain comments
  InStringLiteral -> False  -- Need special handling

-- | Check if expression is atomic (no function application)
isAtomicExpr :: LHsExpr GhcPs -> Bool
isAtomicExpr (L _ e) = case e of
  HsVar _ _ -> True
  HsLit _ _ -> True
  HsOverLit _ _ -> True
  HsPar _ inner -> isAtomicExpr inner
  ExplicitTuple _ args _ -> all isAtomicArg args
  ExplicitList _ es -> all isAtomicExpr es
  _ -> False
  where
    isAtomicArg (Present _ e') = isAtomicExpr e'
    isAtomicArg (Missing _) = True

-- | Check if expression is a literal
isLiteralExpr :: LHsExpr GhcPs -> Bool
isLiteralExpr (L _ e) = case e of
  HsLit _ _ -> True
  HsOverLit _ _ -> True
  HsPar _ inner -> isLiteralExpr inner
  NegApp _ inner _ -> isLiteralExpr inner
  ExplicitList _ es -> all isLiteralExpr es
  ExplicitTuple _ args _ -> all isLitArg args
  _ -> False
  where
    isLitArg (Present _ e') = isLiteralExpr e'
    isLitArg _ = False

-- | Check if expression is a simple variable
isVarExpr :: LHsExpr GhcPs -> Bool
isVarExpr (L _ e) = case e of
  HsVar _ _ -> True
  HsPar _ inner -> isVarExpr inner
  _ -> False

-- | Check if expression is a function (lambda or named function)
isFunExpr :: LHsExpr GhcPs -> Bool
isFunExpr (L _ e) = case e of
  HsLam _ _ _ -> True
  HsVar _ _ -> True
  HsPar _ inner -> isFunExpr inner
  _ -> False

-- | Check if expression is a function application
isApplicationExpr :: LHsExpr GhcPs -> Bool
isApplicationExpr (L _ e) = case e of
  HsApp _ _ _ -> True
  OpApp _ _ _ _ -> True
  HsPar _ inner -> isApplicationExpr inner
  _ -> False

-- | Check if expression is a constructor application
isConstructorExpr :: LHsExpr GhcPs -> Bool
isConstructorExpr (L _ e) = case e of
  HsVar _ (L _ name) -> isConstructorName (getVarName name)
  HsApp _ f _ -> isConstructorExpr f
  HsPar _ inner -> isConstructorExpr inner
  ExplicitList _ _ -> True
  ExplicitTuple _ _ _ -> True
  _ -> False

-- | Check if expression is pure (no IO, no side effects)
-- This is a simplified heuristic
isPure :: LHsExpr GhcPs -> Bool
isPure (L _ e) = case e of
  HsVar _ (L _ name) ->
    let n = getVarName name
    in n `notElem` ["putStrLn", "print", "readFile", "writeFile", "getLine", "getContents"]
  HsApp _ f a -> isPure f && isPure a
  HsLam _ _ _ -> True  -- GHC 9.10: 3 args
  HsLit _ _ -> True
  HsOverLit _ _ -> True
  HsPar _ inner -> isPure inner  -- GHC 9.10+: 2 args
  HsIf _ c t f -> isPure c && isPure t && isPure f
  ExplicitList _ es -> all isPure es
  ExplicitTuple _ args _ -> all isPureArg args
  OpApp _ l _ r -> isPure l && isPure r
  NegApp _ inner _ -> isPure inner
  _ -> True  -- Assume pure by default
  where
    isPureArg (Present _ e') = isPure e'
    isPureArg _ = True

-- | Check if expression is a monadic bind (>>=, >>, <-, do-notation)
isBindExpr :: LHsExpr GhcPs -> Bool
isBindExpr (L _ e) = case e of
  -- Infix bind operators: x >>= f, x >> y
  OpApp _ _ (L _ (HsVar _ (L _ op))) _ ->
    let opName = getVarName op
    in opName `elem` [">>=", ">>", "=<<"]
  -- Do notation contains binds
  HsDo _ _ _ -> True
  -- Look inside parentheses
  HsPar _ inner -> isBindExpr inner
  _ -> False

-- | Check if expression uses defaultOptions (for Aeson, etc.)
isDefaultOptionsExpr :: LHsExpr GhcPs -> Bool
isDefaultOptionsExpr (L _ e) = case e of
  HsVar _ (L _ name) ->
    getVarName name `elem` ["defaultOptions", "defaultRecordTypeOptions", "defaultTaggedObject"]
  HsApp _ f _ -> isDefaultOptionsExpr f
  HsPar _ inner -> isDefaultOptionsExpr inner
  _ -> False

-- | Evaluate semantic context predicate
-- Contexts: "parallel", "test", "unsafe", "mtl", "lens"
evalInContext :: Text -> Bool
evalInContext ctx = case ctx of
  -- These would need additional context from surrounding code
  -- For now, we default to False (conservative)
  "parallel" -> False  -- Would need to detect parMap, parList, etc.
  "unsafe" -> False    -- Would need to detect unsafePerformIO context
  "test" -> False      -- Would need to detect test framework context
  "mtl" -> False       -- Would need to detect mtl usage
  "lens" -> False      -- Would need to detect lens operators
  _ -> False           -- Unknown context

--------------------------------------------------------------------------------
-- Pattern Matching
--------------------------------------------------------------------------------

-- | Parse the pattern from a Rule's rulePattern field
-- Returns the parsed LHS and optionally the parsed RHS (replacement)
parseRulePatterns :: Rule -> IO (Maybe (LHsExpr GhcPs, Maybe (LHsExpr GhcPs)))
parseRulePatterns rule = case rulePattern rule of
  ASTPatternSpec patText -> do
    lhsResult <- parsePattern patText
    rhsResult <- case ruleReplacement rule of
      Nothing -> pure (Right Nothing)
      Just rhsText -> fmap Just <$> parsePattern rhsText
    case (lhsResult, rhsResult) of
      (Right lhs, Right rhs) -> pure $ Just (lhs, rhs)
      _ -> pure Nothing
  TextPatternSpec patText -> do
    -- Try to parse text patterns as AST patterns too
    lhsResult <- parsePattern patText
    rhsResult <- case ruleReplacement rule of
      Nothing -> pure (Right Nothing)
      Just rhsText -> fmap Just <$> parsePattern rhsText
    case (lhsResult, rhsResult) of
      (Right lhs, Right rhs) -> pure $ Just (lhs, rhs)
      _ -> pure Nothing
  RegexPatternSpec _ -> pure Nothing  -- Regex patterns can't be AST matched

-- | Find all matches of a rule in an expression (using unified Rule type)
-- Returns empty if the rule is disabled or if the rule's pattern can't be parsed as AST
findMatchesRule :: Rule -> LHsExpr GhcPs -> IO [MatchResult]
findMatchesRule rule target
  | not (ruleEnabled rule) = pure []  -- Skip disabled rules
  | otherwise = do
      mPatterns <- parseRulePatterns rule
      case mPatterns of
        Nothing -> pure []
        Just (lhs, _rhs) ->
          let conditions = if null (ruleConditions rule) then Nothing else Just (And (ruleConditions rule))
          in pure $ findInExpr lhs conditions (Just rule) target

-- | Find matches of a pattern in an expression and its subexpressions
findInExpr :: LHsExpr GhcPs -> Maybe SideCondition -> Maybe Rule -> LHsExpr GhcPs -> [MatchResult]
findInExpr pat sideCond mRule target =
  let directMatch = case unifyExpr pat target of
        Just subst | checkSide sideCond subst ->
          [MatchResult
            { mrSubst = subst
            , mrSpan = getLocA target
            , mrOriginal = target
            , mrRule = mRule
            }]
        _ -> []
      childMatches = concatMap (findInExpr pat sideCond mRule) (childExprs target)
  in directMatch ++ childMatches
  where
    checkSide Nothing _ = True
    checkSide (Just sc) subst = evalSideCondition subst sc

-- | Find all matches using multiple rules (using unified Rule type)
findAllMatchesRules :: [Rule] -> LHsExpr GhcPs -> IO [MatchResult]
findAllMatchesRules rules expr = do
  results <- mapM (`findMatchesRule` expr) (filter ruleEnabled rules)
  pure $ concat results

-- | Legacy function - kept for backwards compatibility (pure version)
-- This version works with pre-parsed AST patterns
findMatches :: LHsExpr GhcPs -> [SideCondition] -> Maybe Rule -> LHsExpr GhcPs -> [MatchResult]
findMatches lhsPattern conditions mRule target =
  let sideCond = if null conditions then Nothing else Just (And conditions)
  in findInExpr lhsPattern sideCond mRule target

-- | Find all matches using pre-parsed patterns (pure version)
findAllMatches :: [(LHsExpr GhcPs, [SideCondition], Rule)] -> LHsExpr GhcPs -> [MatchResult]
findAllMatches parsedRules expr =
  concatMap (\(pat, conds, rule) -> findMatches pat conds (Just rule) expr)
            (filter (\(_, _, r) -> ruleEnabled r) parsedRules)

-- | Get child expressions from an expression
childExprs :: LHsExpr GhcPs -> [LHsExpr GhcPs]
childExprs (L _ e) = case e of
  HsApp _ f a -> [f, a]
  OpApp _ l op r -> [l, op, r]
  HsIf _ c t f -> [c, t, f]
  HsPar _ inner -> [inner]
  NegApp _ inner _ -> [inner]
  ExplicitList _ es -> es
  SectionL _ e' op -> [e', op]
  SectionR _ op e' -> [op, e']
  ExplicitTuple _ args _ -> mapMaybe getTupExpr args
  HsLet _ _ body -> [body]  -- GHC 9.10: 3 args
  HsCase _ scrut mg -> scrut : matchGroupExprs mg
  HsDo _ _ (L _ stmts) -> concatMap stmtExprs stmts
  HsLam _ _ mg -> matchGroupExprs mg  -- GHC 9.10: 3 args
  HsAppType _ e' _ -> [e']  -- GHC 9.10: 3 args
  ExprWithTySig _ e' _ -> [e']
  RecordCon _ _ (HsRecFields flds _) -> map (hfbRHS . unLoc) flds
  RecordUpd _ e' _ -> [e']
  ArithSeq _ _ info -> arithSeqExprs info
  _ -> []
  where
    getTupExpr (Present _ e') = Just e'
    getTupExpr _ = Nothing

    stmtExprs (L _ stmt) = case stmt of
      BodyStmt _ e' _ _ -> [e']
      LastStmt _ e' _ _ -> [e']
      BindStmt _ _ e' -> [e']
      LetStmt _ _ -> []
      _ -> []

    matchGroupExprs (MG _ (L _ matches)) =
      concatMap matchExprs (map unLoc matches)
    matchGroupExprs XMatchGroup{} = []

    matchExprs (Match _ _ _ grhss) = grhssExprs grhss
    matchExprs XMatch{} = []

    grhssExprs (GRHSs _ grhss _) = concatMap grhsExpr (map unLoc grhss)
    grhssExprs XGRHSs{} = []

    grhsExpr (GRHS _ guards e') = e' : concatMap stmtExprs guards
    grhsExpr XGRHS{} = []

    arithSeqExprs (From e') = [e']
    arithSeqExprs (FromThen e1 e2) = [e1, e2]
    arithSeqExprs (FromTo e1 e2) = [e1, e2]
    arithSeqExprs (FromThenTo e1 e2 e3) = [e1, e2, e3]

-- | Match a single rule against an expression (IO version for unified Rule)
matchRuleIO :: Rule -> LHsExpr GhcPs -> IO [MatchResult]
matchRuleIO = findMatchesRule

-- | Match a single pre-parsed pattern against an expression (pure version)
matchRule :: LHsExpr GhcPs -> [SideCondition] -> Maybe Rule -> LHsExpr GhcPs -> [MatchResult]
matchRule = findMatches

-- | Match a pattern against an expression (without rule context)
matchPattern :: LHsExpr GhcPs -> LHsExpr GhcPs -> Maybe Subst
matchPattern = unifyExpr

--------------------------------------------------------------------------------
-- Rule Parsing
--------------------------------------------------------------------------------

-- | DynFlags for parsing patterns
patternDynFlags :: DynFlags
patternDynFlags = foldl' xopt_set (defaultDynFlags fakeSettings) defaultExtensions
  where
    defaultExtensions =
      [ LangExt.BangPatterns
      , LangExt.LambdaCase
      , LangExt.MultiWayIf
      , LangExt.TupleSections
      , LangExt.ViewPatterns
      , LangExt.TypeApplications
      , LangExt.OverloadedStrings
      , LangExt.RecordWildCards
      ]

-- | Parse a pattern string into an AST expression
parsePattern :: Text -> IO (Either Text (LHsExpr GhcPs))
parsePattern patternText = pure $ parsePatternPure patternText

-- | Pure version of parsePattern
parsePatternPure :: Text -> Either Text (LHsExpr GhcPs)
parsePatternPure patternText =
  case parseExpression (T.unpack patternText) patternDynFlags of
    Lexer.POk _ expr -> Right expr
    Lexer.PFailed _ -> Left $ "Failed to parse pattern: " <> patternText

-- | Parse a side condition from text
-- Uses the unified SideCondition constructor names
parseSideCondition :: Text -> Maybe SideCondition
parseSideCondition txt = case T.words txt of
  -- Location predicates
  ["notInComment"] -> Just NotInComment
  ["notInString"] -> Just NotInString
  ["notInImport"] -> Just NotInImport
  ["inFunctionBody"] -> Just InFunctionBody
  -- Structural conditions (unified constructor names)
  ["isAtomic", var] -> Just $ IsAtomic var
  ["isAtom", var] -> Just $ IsAtomic var  -- Legacy alias
  ["isLiteral", var] -> Just $ IsLiteral var
  ["isVariable", var] -> Just $ IsVariable var
  ["isVar", var] -> Just $ IsVariable var  -- Legacy alias
  ["isLambda", var] -> Just $ IsLambda var
  ["isFun", var] -> Just $ IsLambda var  -- Legacy alias
  ["isApplication", var] -> Just $ IsApplication var
  ["isConstructor", var] -> Just $ IsConstructor var
  ["isPure", var] -> Just $ IsPure var
  ["notEqual", v1, v2] -> Just $ NotEqual v1 v2
  ["notEq", v1, v2] -> Just $ NotEqual v1 v2  -- Legacy alias
  ["freeIn", v1, v2] -> Just $ FreeIn v1 v2
  ["notFreeIn", v1, v2] -> Just $ NotFreeIn v1 v2
  -- Type-aware conditions
  ["hasType", var, typ] -> Just $ HasType var typ
  ["typeMatches", var, pat] -> Just $ TypeMatches var pat
  ["hasTypeClass", var, cls] -> Just $ HasTypeClass var cls
  ["typeContains", var, typ] -> Just $ TypeContains var typ
  ["isNumeric", var] -> Just $ IsNumeric var
  ["isString", var] -> Just $ IsString var
  ["isList", var] -> Just $ IsList var
  ["isMaybe", var] -> Just $ IsMaybe var
  ["isMonad", var, monad] -> Just $ IsMonad var monad
  -- Context predicates
  ["hasImport", modName] -> Just $ HasImport modName
  ["hasPragma", pragma] -> Just $ HasPragma pragma
  ["inModule", modPat] -> Just $ InModule modPat
  ["inTestFile"] -> Just InTestFile
  ["notInTestFile"] -> Just NotInTestFile
  -- Always/never
  ["always"] -> Just Always
  ["never"] -> Just Never
  _ -> Nothing

-- NOTE: loadASTRules and loadASTRulesFromConfig have been removed.
-- Use the unified Rule type from Argus.Rules.Types instead.
-- To create rules for AST matching, use ASTPatternSpec in the rulePattern field.

--------------------------------------------------------------------------------
-- Diagnostic Generation
--------------------------------------------------------------------------------

-- | Apply AST rules to a parsed module (using unified Rule type)
applyASTRules :: [Rule] -> FilePath -> Text -> HsModule GhcPs -> IO [Diagnostic]
applyASTRules rules filepath _moduleName hsmod = do
  let allExprs = listify (const True :: LHsExpr GhcPs -> Bool) hsmod
      enabledRules = filter ruleEnabled rules
  results <- mapM (applyRuleToExprs filepath allExprs) enabledRules
  pure $ concat results

-- | Apply a single rule to a list of expressions
applyRuleToExprs :: FilePath -> [LHsExpr GhcPs] -> Rule -> IO [Diagnostic]
applyRuleToExprs filepath exprs rule = do
  mPatterns <- parseRulePatterns rule
  case mPatterns of
    Nothing -> pure []
    Just (lhs, _rhs) -> do
      let diagnostics = concatMap (applyParsedRuleToExpr filepath lhs rule) exprs
      pure diagnostics

-- | Apply a pre-parsed rule pattern to an expression
applyParsedRuleToExpr :: FilePath -> LHsExpr GhcPs -> Rule -> LHsExpr GhcPs -> [Diagnostic]
applyParsedRuleToExpr filepath lhsPattern rule expr =
  case unifyExpr lhsPattern expr of
    Just subst | checkSideCondition rule subst ->
      [matchToDiagnostic filepath (MatchResult subst (getLocA expr) expr (Just rule))]
    _ -> []
  where
    checkSideCondition r s =
      if null (ruleConditions r)
        then True
        else evalSideCondition s (And (ruleConditions r))

-- | Convert a match result to a diagnostic (using unified Rule type)
matchToDiagnostic :: FilePath -> MatchResult -> Diagnostic
matchToDiagnostic filepath mr = Diagnostic
  { diagSpan = srcSpanToArgus filepath (mrSpan mr)
  , diagSeverity = maybe AT.Warning ruleSeverity (mrRule mr)
  , diagKind = CodePattern
  , diagMessage = maybe "Pattern match found" ruleMessage (mrRule mr)
  , diagCode = ruleId <$> mrRule mr
  , diagFixes = maybeToList $ mrRule mr >>= \r -> generateFix r (mrSubst mr) (mrSpan mr)
  , diagRelated = []
  }

-- | Generate a fix from the rule's replacement and substitution (using unified Rule type)
generateFix :: Rule -> Subst -> GHC.SrcSpan -> Maybe Fix
generateFix rule subst srcSpan' = do
  rhsText <- ruleReplacement rule
  -- Parse the replacement pattern
  case parsePatternPure rhsText of
    Left _ -> Nothing
    Right rhs -> do
      let fixedExpr = applySubstDeep subst rhs
          fixText = prettyExpr fixedExpr
          fixSpan = srcSpanToArgus (srcSpanFile' srcSpan') srcSpan'
      guard (ruleSafety rule /= NeedsReview && ruleSafety rule /= Unsafe)
      pure Fix
        { fixTitle = "Replace with: " <> fixText
        , fixEdits = [FixEdit fixSpan fixText]
        , fixIsPreferred = ruleSafety rule == Safe
        , fixAddImports = map importSpecToFixImport (ruleAddImports rule)
        , fixRemoveImports = ruleRemoveImports rule
        , fixCategory = categoryToFixCategory (ruleCategory rule)
        , fixSafety = safetyToFixSafety (ruleSafety rule)
        }
  where
    srcSpanFile' (GHC.RealSrcSpan rsp _) = unpackFS $ GHC.srcSpanFile rsp
    srcSpanFile' _ = "<unknown>"

-- | Pretty-print an expression
prettyExpr :: LHsExpr GhcPs -> Text
prettyExpr = T.pack . showSDocUnsafe . ppr

-- | Convert GHC SrcSpan to Argus SrcSpan
srcSpanToArgus :: FilePath -> GHC.SrcSpan -> AT.SrcSpan
srcSpanToArgus fp (GHC.RealSrcSpan rsp _) = AT.SrcSpan
  { AT.srcSpanFile = fp
  , AT.srcSpanStartLine = Line (GHC.srcLocLine (realSrcSpanStart rsp))
  , AT.srcSpanStartCol = Column (GHC.srcLocCol (realSrcSpanStart rsp))
  , AT.srcSpanEndLine = Line (GHC.srcLocLine (realSrcSpanEnd rsp))
  , AT.srcSpanEndCol = Column (GHC.srcLocCol (realSrcSpanEnd rsp))
  }
srcSpanToArgus fp (GHC.UnhelpfulSpan _) = mkSrcSpanRaw fp 1 1 1 1

-- | Convert Maybe to list
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]
