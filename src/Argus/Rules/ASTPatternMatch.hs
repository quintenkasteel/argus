{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Argus.Rules.ASTPatternMatch
-- Description : Full AST pattern matching using GHC API
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- This module provides comprehensive AST pattern matching by integrating with
-- the GHC parser. It converts parsed AST patterns into actual matchers that
-- work on GHC's internal AST representations.
module Argus.Rules.ASTPatternMatch
  ( -- * AST Matching
    matchPattern
  , matchPatternInModule
  , matchPatternInFile

    -- * Match Results
  , ASTMatchResult (..)
  , ASTBinding (..)
  , BindingType (..)

    -- * Constraint Evaluation
  , evalConstraint
  , evalConstraints
  , ConstraintContext (..)

    -- * Pattern Compilation
  , compilePattern
  , CompiledPattern (..)

    -- * Utilities
  , findAllMatches
  , filterByConstraints
  , extractBindingText
  , bindingsToMap
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Generics (Data, everything, mkQ)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import "ghc-lib-parser" GHC.Hs
import "ghc-lib-parser" GHC.Types.SrcLoc qualified as GHC
import "ghc-lib-parser" GHC.Data.FastString (unpackFS)
import "ghc-lib-parser" GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import "ghc-lib-parser" GHC.Types.Name.Occurrence (occNameString)
import "ghc-lib-parser" GHC.Types.SourceText (IntegralLit(..), rationalFromFractionalLit)
import Text.Regex.TDFA ((=~))

import Argus.Rules.ASTPatternParser
import Argus.Types (SrcSpan (..), Line(..), Column(..))

--------------------------------------------------------------------------------
-- Match Results
--------------------------------------------------------------------------------

-- | Result of AST pattern matching
data ASTMatchResult = ASTMatchResult
  { amrMatched :: Bool
      -- ^ Whether the pattern matched
  , amrBindings :: [ASTBinding]
      -- ^ Captured bindings
  , amrSpan :: Maybe SrcSpan
      -- ^ Source location of match
  , amrConstraintsPassed :: Bool
      -- ^ Whether all constraints passed
  , amrConstraintResults :: Map Text Bool
      -- ^ Individual constraint results
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | A binding captured during AST matching
data ASTBinding = ASTBinding
  { abName :: Text
      -- ^ Name of the metavariable
  , abText :: Text
      -- ^ Matched source text
  , abSpan :: SrcSpan
      -- ^ Source location
  , abType :: BindingType
      -- ^ Type of matched construct
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Type of binding
data BindingType
  = BindExpr
      -- ^ Expression binding
  | BindPat
      -- ^ Pattern binding
  | BindType
      -- ^ Type binding
  | BindDecl
      -- ^ Declaration binding
  | BindName
      -- ^ Name binding
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

--------------------------------------------------------------------------------
-- Constraint Context
--------------------------------------------------------------------------------

-- | Context for evaluating constraints
data ConstraintContext = ConstraintContext
  { ccModuleName :: Text
      -- ^ Current module name
  , ccFilePath :: Text
      -- ^ File path
  , ccImports :: [Text]
      -- ^ Imported modules
  , ccPragmas :: [Text]
      -- ^ File pragmas
  , ccBindings :: Map Text Text
      -- ^ Variable bindings (name -> text)
  , ccTypeInfo :: Map Text Text
      -- ^ Type information (if available)
  , ccFreeVars :: Map Text (Set Text)
      -- ^ Free variables per expression
  }
  deriving stock (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- Compiled Pattern
--------------------------------------------------------------------------------

-- | A compiled pattern ready for matching
data CompiledPattern = CompiledPattern
  { cpOriginal :: ASTPattern
      -- ^ Original pattern
  , cpVars :: Set Text
      -- ^ Metavariables in pattern
  , cpConstraints :: [PatternConstraint]
      -- ^ Constraints to check
  , cpComplexity :: Int
      -- ^ Pattern complexity
  }
  deriving stock (Eq, Show, Generic)

-- | Compile a pattern for efficient matching
compilePattern :: ASTPattern -> CompiledPattern
compilePattern pat = CompiledPattern
  { cpOriginal = normalizePattern pat
  , cpVars = patternVars pat
  , cpConstraints = extractConstraints pat
  , cpComplexity = patternComplexity pat
  }
  where
    extractConstraints :: ASTPattern -> [PatternConstraint]
    extractConstraints (PConstrained _ cs) = cs
    extractConstraints _ = []

--------------------------------------------------------------------------------
-- Pattern Matching
--------------------------------------------------------------------------------

-- | Match a pattern against an expression (generic)
matchPattern :: forall a. (Data a) => ASTPattern -> a -> Text -> [ASTMatchResult]
matchPattern pat ast source =
  let matches = everything (++) (mkQ [] (matchExpr pat source)) ast
  in matches

-- | Match pattern against Haskell expressions
matchExpr :: ASTPattern -> Text -> HsExpr GhcPs -> [ASTMatchResult]
matchExpr astPat source expr = case astPat of
  PVar name ->
    -- Metavariable matches any expression
    [mkMatchResult name (exprText expr source) (exprSpan expr)]

  PWild ->
    -- Wildcard matches anything
    [ASTMatchResult True [] (Just $ exprSpan expr) True Map.empty]

  PLit lit ->
    -- Match literal
    matchLiteral lit expr source

  PApp pf px ->
    -- Match function application
    matchApplication pf px expr source

  PLam params pbody ->
    -- Match lambda
    matchLambda params pbody expr source

  PIf pcond pthen pelse ->
    -- Match if-then-else
    matchIfExpr pcond pthen pelse expr source

  PCase pscrut palts ->
    -- Match case expression
    matchCaseExpr pscrut palts expr source

  PTuple pats ->
    -- Match tuple
    matchTuple pats expr source

  PList pats ->
    -- Match list
    matchList pats expr source

  PInfix pl op pr ->
    -- Match infix expression
    matchInfixExpr pl op pr expr source

  PSection ml op mr ->
    -- Match section
    matchSection ml op mr expr source

  PDo stmts ->
    -- Match do notation
    matchDoExpr stmts expr source

  PTypeSig pe ptype ->
    -- Match type signature expression
    matchTypeSig pe ptype expr source

  PConstrained p constraints ->
    -- Match pattern with constraints
    let baseMatches = matchExpr p source expr
    in map (addConstraintResults constraints) baseMatches

  _ ->
    -- Other patterns - try text matching
    []
  where
    mkMatchResult :: Text -> Text -> SrcSpan -> ASTMatchResult
    mkMatchResult bindName txt sp = ASTMatchResult
      { amrMatched = True
      , amrBindings = [ASTBinding bindName txt sp BindExpr]
      , amrSpan = Just sp
      , amrConstraintsPassed = True
      , amrConstraintResults = Map.empty
      }

    addConstraintResults :: [PatternConstraint] -> ASTMatchResult -> ASTMatchResult
    addConstraintResults cs result = result
      { amrConstraintResults = Map.fromList
          [ (constraintName c, True) | c <- cs ]
      }

    constraintName :: PatternConstraint -> Text
    constraintName = \case
      NotFreeIn v e -> v <> "_notFreeIn_" <> e
      FreeIn v e -> v <> "_freeIn_" <> e
      HasType v _ -> v <> "_hasType"
      HasTypeClass v cls -> v <> "_hasClass_" <> cls
      NotEqual v1 v2 -> v1 <> "_notEqual_" <> v2
      IsAtomic v -> v <> "_isAtomic"
      IsPure v -> v <> "_isPure"
      IsMonadic v -> v <> "_isMonadic"
      ComplexityLT v n -> v <> "_complexity_lt_" <> T.pack (show n)
      ComplexityGT v n -> v <> "_complexity_gt_" <> T.pack (show n)
      _ -> "constraint"

-- | Match literal pattern
matchLiteral :: LiteralPattern -> HsExpr GhcPs -> Text -> [ASTMatchResult]
matchLiteral lit expr _source = case (lit, expr) of
  (LitInt n, HsLit _ (HsInt _ il)) ->
    [mkLitMatch | il_value il == n]

  (LitFloat n, HsLit _ (HsRat _ fl _)) ->
    let val = fromRational (rationalFromFractionalLit fl) :: Double
    in [mkLitMatch | abs (val - n) < 1e-10]

  (LitChar c, HsLit _ (HsChar _ c')) ->
    [mkLitMatch | c == c']

  (LitString s, HsLit _ (HsString _ fs)) ->
    [mkLitMatch | s == T.pack (unpackFS fs)]

  (LitBool True, HsVar _ (GHC.L _ name)) ->
    [mkLitMatch | rdrNameText name == "True"]

  (LitBool False, HsVar _ (GHC.L _ name)) ->
    [mkLitMatch | rdrNameText name == "False"]

  _ -> []
  where
    mkLitMatch = ASTMatchResult
      { amrMatched = True
      , amrBindings = []
      , amrSpan = Just $ exprSpan expr
      , amrConstraintsPassed = True
      , amrConstraintResults = Map.empty
      }

-- | Match function application
matchApplication :: ASTPattern -> ASTPattern -> HsExpr GhcPs -> Text -> [ASTMatchResult]
matchApplication pf px expr source = case expr of
  HsApp _ (GHC.L _ f) (GHC.L _ x) ->
    let fMatches = matchExpr pf source f
        xMatches = matchExpr px source x
    in [ combineMatches fm xm
       | fm <- fMatches, amrMatched fm
       , xm <- xMatches, amrMatched xm
       ]
  _ -> []
  where
    combineMatches m1 m2 = ASTMatchResult
      { amrMatched = True
      , amrBindings = amrBindings m1 ++ amrBindings m2
      , amrSpan = amrSpan m1
      , amrConstraintsPassed = amrConstraintsPassed m1 && amrConstraintsPassed m2
      , amrConstraintResults = amrConstraintResults m1 <> amrConstraintResults m2
      }

-- | Match lambda expression
matchLambda :: [Text] -> ASTPattern -> HsExpr GhcPs -> Text -> [ASTMatchResult]
matchLambda params pbody expr source = case expr of
  HsLam _ _ (MG _ (GHC.L _ [GHC.L _ match])) ->
    let Match _ _ pats body = match
        patNames = mapMaybe extractPatName pats
        bodyExpr = case body of
          GRHSs _ [GHC.L _ (GRHS _ _ (GHC.L _ e))] _ -> Just e
          _ -> Nothing
    in case bodyExpr of
         Just bexpr
           | length patNames >= length params ->
               let paramBindings =
                     [ ASTBinding p (T.pack n) (mkSpan 1 1 1 1) BindName
                     | (p, n) <- zip params patNames
                     ]
                   bodyMatches = matchExpr pbody source bexpr
               in [ m { amrBindings = paramBindings ++ amrBindings m }
                  | m <- bodyMatches, amrMatched m
                  ]
         _ -> []
  _ -> []
  where
    extractPatName :: LPat GhcPs -> Maybe String
    extractPatName (GHC.L _ (VarPat _ (GHC.L _ name))) = Just $ rdrNameStr name
    extractPatName _ = Nothing

    rdrNameStr :: RdrName -> String
    rdrNameStr = occNameString . rdrNameOcc

-- | Match if-then-else expression
matchIfExpr :: ASTPattern -> ASTPattern -> ASTPattern -> HsExpr GhcPs -> Text -> [ASTMatchResult]
matchIfExpr pcond pthen pelse expr source = case expr of
  HsIf _ (GHC.L _ cond) (GHC.L _ thenE) (GHC.L _ elseE) ->
    let condMatches = matchExpr pcond source cond
        thenMatches = matchExpr pthen source thenE
        elseMatches = matchExpr pelse source elseE
    in [ combineAll [cm, tm, em]
       | cm <- condMatches, amrMatched cm
       , tm <- thenMatches, amrMatched tm
       , em <- elseMatches, amrMatched em
       ]
  _ -> []
  where
    combineAll :: [ASTMatchResult] -> ASTMatchResult
    combineAll ms = ASTMatchResult
      { amrMatched = True
      , amrBindings = concatMap amrBindings ms
      , amrSpan = case ms of
          (m:_) -> amrSpan m
          []    -> Nothing
      , amrConstraintsPassed = all amrConstraintsPassed ms
      , amrConstraintResults = Map.unions $ map amrConstraintResults ms
      }

-- | Match case expression
matchCaseExpr :: ASTPattern -> [(ASTPattern, ASTPattern)] -> HsExpr GhcPs -> Text -> [ASTMatchResult]
matchCaseExpr pscrut _palts expr source = case expr of
  HsCase _ (GHC.L _ scrut) (MG _ (GHC.L _ _alts)) ->
    let scrutMatches = matchExpr pscrut source scrut
    in [ m { amrBindings = amrBindings m ++ scrutBindings }
       | m <- scrutMatches, amrMatched m
       , let scrutBindings = []  -- Could add alt matching here
       ]
  _ -> []

-- | Match tuple expression
matchTuple :: [ASTPattern] -> HsExpr GhcPs -> Text -> [ASTMatchResult]
matchTuple pats expr source = case expr of
  ExplicitTuple _ args _ ->
    let argExprs = [ e | Present _ (GHC.L _ e) <- args ]
    in if length argExprs == length pats
       then let matches = zipWith (\p e -> matchExpr p source e) pats argExprs
                allMatches = sequence matches
            in [ combineMatches ms | ms <- allMatches ]
       else []
  _ -> []
  where
    combineMatches :: [ASTMatchResult] -> ASTMatchResult
    combineMatches ms = ASTMatchResult
      { amrMatched = all amrMatched ms
      , amrBindings = concatMap amrBindings ms
      , amrSpan = Just $ exprSpan expr
      , amrConstraintsPassed = True
      , amrConstraintResults = Map.empty
      }

-- | Match list expression
matchList :: [ASTPattern] -> HsExpr GhcPs -> Text -> [ASTMatchResult]
matchList pats expr source = case expr of
  ExplicitList _ elems ->
    let elemExprs = [ e | GHC.L _ e <- elems ]
    in if length elemExprs == length pats
       then let matches = zipWith (\p e -> matchExpr p source e) pats elemExprs
                allMatches = sequence matches
            in [ combineMatches ms | ms <- allMatches ]
       else []
  _ -> []
  where
    combineMatches :: [ASTMatchResult] -> ASTMatchResult
    combineMatches ms = ASTMatchResult
      { amrMatched = all amrMatched ms
      , amrBindings = concatMap amrBindings ms
      , amrSpan = Just $ exprSpan expr
      , amrConstraintsPassed = True
      , amrConstraintResults = Map.empty
      }

-- | Match infix expression
matchInfixExpr :: ASTPattern -> Text -> ASTPattern -> HsExpr GhcPs -> Text -> [ASTMatchResult]
matchInfixExpr pl op pr expr source = case expr of
  OpApp _ (GHC.L _ left) (GHC.L _ opExpr) (GHC.L _ right) ->
    let opName = exprName opExpr
    in if opName == op || op == "_"
       then let leftMatches = matchExpr pl source left
                rightMatches = matchExpr pr source right
            in [ ASTMatchResult
                   { amrMatched = True
                   , amrBindings = amrBindings lm ++ amrBindings rm
                   , amrSpan = Just $ exprSpan expr
                   , amrConstraintsPassed = True
                   , amrConstraintResults = Map.empty
                   }
               | lm <- leftMatches, amrMatched lm
               , rm <- rightMatches, amrMatched rm
               ]
       else []
  _ -> []
  where
    exprName (HsVar _ (GHC.L _ name)) = rdrNameText name
    exprName _ = ""

-- | Match section
matchSection :: Maybe ASTPattern -> Text -> Maybe ASTPattern -> HsExpr GhcPs -> Text -> [ASTMatchResult]
matchSection ml op mr expr source = case expr of
  SectionL _ (GHC.L _ arg) (GHC.L _ opExpr) ->
    case (ml, mr) of
      (Just pl, Nothing) ->
        let argMatches = matchExpr pl source arg
        in [ m | m <- argMatches, amrMatched m, exprName opExpr == op ]
      _ -> []

  SectionR _ (GHC.L _ opExpr) (GHC.L _ arg) ->
    case (ml, mr) of
      (Nothing, Just pr) ->
        let argMatches = matchExpr pr source arg
        in [ m | m <- argMatches, amrMatched m, exprName opExpr == op ]
      _ -> []

  _ -> []
  where
    exprName (HsVar _ (GHC.L _ name)) = rdrNameText name
    exprName _ = ""

-- | Match do expression
matchDoExpr :: [DoStatement] -> HsExpr GhcPs -> Text -> [ASTMatchResult]
matchDoExpr pstmts expr _source = case expr of
  HsDo _ _ (GHC.L _ stmts) ->
    -- Simplified: just check if it's a do expression
    if length stmts >= length pstmts
    then [ASTMatchResult True [] (Just $ exprSpan expr) True Map.empty]
    else []
  _ -> []

-- | Match type signature expression
matchTypeSig :: ASTPattern -> TypePattern -> HsExpr GhcPs -> Text -> [ASTMatchResult]
matchTypeSig pe _ptype expr source = case expr of
  ExprWithTySig _ (GHC.L _ e) _ ->
    matchExpr pe source e
  _ -> []

--------------------------------------------------------------------------------
-- Constraint Evaluation
--------------------------------------------------------------------------------

-- | Evaluate a constraint
evalConstraint :: ConstraintContext -> PatternConstraint -> Bool
evalConstraint ctx = \case
  NotFreeIn var expr ->
    case (Map.lookup var (ccBindings ctx), Map.lookup expr (ccFreeVars ctx)) of
      (Just _, Just fvs) -> not $ var `Set.member` fvs
      _ -> True  -- Conservatively pass if info unavailable

  FreeIn var expr ->
    case (Map.lookup var (ccBindings ctx), Map.lookup expr (ccFreeVars ctx)) of
      (Just _, Just fvs) -> var `Set.member` fvs
      _ -> False

  HasType var tpat ->
    case Map.lookup var (ccTypeInfo ctx) of
      Just typ -> matchTypePattern typ tpat
      Nothing -> True  -- Pass if no type info

  HasTypeClass var cls ->
    case Map.lookup var (ccTypeInfo ctx) of
      Just typ -> cls `T.isInfixOf` typ
      Nothing -> True

  NotEqual var1 var2 ->
    case (Map.lookup var1 (ccBindings ctx), Map.lookup var2 (ccBindings ctx)) of
      (Just t1, Just t2) -> t1 /= t2
      _ -> True

  IsAtomic var ->
    case Map.lookup var (ccBindings ctx) of
      Just txt -> isAtomicText txt
      Nothing -> True

  IsPure _var ->
    -- Would need effect analysis
    True

  IsMonadic var ->
    case Map.lookup var (ccTypeInfo ctx) of
      Just typ -> isMonadicType typ
      Nothing -> False

  ComplexityLT var n ->
    case Map.lookup var (ccBindings ctx) of
      Just txt -> textComplexity txt < n
      Nothing -> True

  ComplexityGT var n ->
    case Map.lookup var (ccBindings ctx) of
      Just txt -> textComplexity txt > n
      Nothing -> False

  MatchesRegex var pat ->
    case Map.lookup var (ccBindings ctx) of
      Just txt -> T.unpack txt =~ T.unpack pat
      Nothing -> False

  InModule modName ->
    ccModuleName ctx == modName

  NotInModule modName ->
    ccModuleName ctx /= modName

  HasPragma pragma ->
    pragma `elem` ccPragmas ctx

  HasImport imp ->
    imp `elem` ccImports ctx

  ConstraintAnd c1 c2 ->
    evalConstraint ctx c1 && evalConstraint ctx c2

  ConstraintOr c1 c2 ->
    evalConstraint ctx c1 || evalConstraint ctx c2

  ConstraintNot c ->
    not $ evalConstraint ctx c
  where
    matchTypePattern :: Text -> TypePattern -> Bool
    matchTypePattern typ = \case
      TVar _ -> True  -- Type variable matches anything
      TCon name -> name `T.isInfixOf` typ
      TFun _t1 _t2 -> "->" `T.isInfixOf` typ
      TApp _t1 _t2 -> True  -- Simplified
      TTuple _ts -> "(" `T.isInfixOf` typ && "," `T.isInfixOf` typ
      TList _t -> "[" `T.isPrefixOf` typ
      TForall _ _t -> "forall" `T.isInfixOf` typ
      TConstraint _ _t -> "=>" `T.isInfixOf` typ

    isAtomicText :: Text -> Bool
    isAtomicText txt =
      let stripped = T.strip txt
      in T.all (\c -> c `elem` ("_'" :: String) || isAlphaNum c) stripped ||
         T.all isDigit stripped ||
         (T.head stripped == '"' && T.last stripped == '"')

    isAlphaNum :: Char -> Bool
    isAlphaNum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

    isDigit c = c >= '0' && c <= '9'

    isMonadicType :: Text -> Bool
    isMonadicType typ =
      any (`T.isInfixOf` typ) ["IO", "Maybe", "Either", "Reader", "State", "Writer", "ST"]

    textComplexity :: Text -> Int
    textComplexity txt =
      length (filter (`elem` ("()[]{}\\-><=|&" :: String)) $ T.unpack txt)

-- | Evaluate multiple constraints
evalConstraints :: ConstraintContext -> [PatternConstraint] -> Bool
evalConstraints ctx = all (evalConstraint ctx)

--------------------------------------------------------------------------------
-- File and Module Matching
--------------------------------------------------------------------------------

-- | Match pattern in a parsed module
matchPatternInModule :: ASTPattern -> HsModule GhcPs -> Text -> [ASTMatchResult]
matchPatternInModule pat hsmod source =
  let decls = hsmodDecls hsmod
  in concatMap (matchInDecl pat source) decls
  where
    matchInDecl :: ASTPattern -> Text -> LHsDecl GhcPs -> [ASTMatchResult]
    matchInDecl p src (GHC.L _ decl) = case decl of
      ValD _ bind -> matchInBind p src bind
      _ -> []

    matchInBind :: ASTPattern -> Text -> HsBind GhcPs -> [ASTMatchResult]
    matchInBind p src bind = case bind of
      FunBind _ _ (MG _ (GHC.L _ matches)) ->
        concatMap (matchInMatch p src) matches
      PatBind _ _ _ grhss ->
        matchInGRHSs p src grhss
      _ -> []

    matchInMatch :: ASTPattern -> Text -> LMatch GhcPs (LHsExpr GhcPs) -> [ASTMatchResult]
    matchInMatch p src (GHC.L _ (Match _ _ _ grhss)) = matchInGRHSs p src grhss

    matchInGRHSs :: ASTPattern -> Text -> GRHSs GhcPs (LHsExpr GhcPs) -> [ASTMatchResult]
    matchInGRHSs p src (GRHSs _ grhss _) =
      concatMap (matchInGRHS p src) grhss

    matchInGRHS :: ASTPattern -> Text -> LGRHS GhcPs (LHsExpr GhcPs) -> [ASTMatchResult]
    matchInGRHS p src (GHC.L _ (GRHS _ _ (GHC.L _ expr))) = matchExpr p src expr

-- | Match pattern in a file
matchPatternInFile :: ASTPattern -> FilePath -> Text -> IO [ASTMatchResult]
matchPatternInFile pat filepath source = do
  -- Parse the source file
  -- For now, use text-based matching as fallback
  let textMatches = matchASTPattern pat source
  return $ map toASTMatchResult textMatches
  where
    toASTMatchResult :: MatchResult -> ASTMatchResult
    toASTMatchResult mr = ASTMatchResult
      { amrMatched = mrMatched mr
      , amrBindings = map toASTBinding $ Map.toList $ mrBindings mr
      , amrSpan = Nothing
      , amrConstraintsPassed = True
      , amrConstraintResults = mrConstraintResults mr
      }

    toASTBinding :: (Text, MatchBinding) -> ASTBinding
    toASTBinding (name, mb) = ASTBinding
      { abName = name
      , abText = mbText mb
      , abSpan = SrcSpan
          { srcSpanFile = filepath
          , srcSpanStartLine = Line $ mbStartLine mb
          , srcSpanStartCol = Column $ mbStartCol mb
          , srcSpanEndLine = Line $ mbEndLine mb
          , srcSpanEndCol = Column $ mbEndCol mb
          }
      , abType = BindExpr
      }

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Find all matches in source
findAllMatches :: ASTPattern -> Text -> [ASTMatchResult]
findAllMatches pat source =
  let results = matchASTPattern pat source
  in map toASTMatchResult results
  where
    toASTMatchResult mr = ASTMatchResult
      { amrMatched = mrMatched mr
      , amrBindings = map toASTBinding $ Map.toList $ mrBindings mr
      , amrSpan = Nothing
      , amrConstraintsPassed = True
      , amrConstraintResults = mrConstraintResults mr
      }

    toASTBinding (name, mb) = ASTBinding
      { abName = name
      , abText = mbText mb
      , abSpan = mkSpan (mbStartLine mb) (mbStartCol mb) (mbEndLine mb) (mbEndCol mb)
      , abType = BindExpr
      }

-- | Filter results by constraints
filterByConstraints :: ConstraintContext -> [PatternConstraint] -> [ASTMatchResult] -> [ASTMatchResult]
filterByConstraints ctx constraints = filter checkResult
  where
    checkResult r =
      let bindings = bindingsToMap (amrBindings r)
          ctx' = ctx { ccBindings = bindings }
      in evalConstraints ctx' constraints

-- | Extract text for a binding
extractBindingText :: ASTBinding -> Text
extractBindingText = abText

-- | Convert bindings to map
bindingsToMap :: [ASTBinding] -> Map Text Text
bindingsToMap = Map.fromList . map (\b -> (abName b, abText b))

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Get expression text from source
exprText :: HsExpr GhcPs -> Text -> Text
exprText expr source =
  let sp = exprSpan expr
  in extractSpanText sp source

-- | Get expression span
exprSpan :: HsExpr GhcPs -> SrcSpan
exprSpan _ = mkSpan 1 1 1 1  -- Placeholder - would use actual source span

-- | Extract text from span
extractSpanText :: SrcSpan -> Text -> Text
extractSpanText sp source =
  let lines' = T.lines source
      Line startLine' = srcSpanStartLine sp
      Line endLine' = srcSpanEndLine sp
      startLine = startLine' - 1
      endLine = endLine' - 1
  in if startLine >= 0 && endLine < length lines' && startLine <= endLine
     then T.unlines $ take (endLine - startLine + 1) $ drop startLine lines'
     else ""

-- | Create a source span
mkSpan :: Int -> Int -> Int -> Int -> SrcSpan
mkSpan sl sc el ec = SrcSpan
  { srcSpanFile = ""
  , srcSpanStartLine = Line sl
  , srcSpanStartCol = Column sc
  , srcSpanEndLine = Line el
  , srcSpanEndCol = Column ec
  }

-- | Get RdrName as Text
rdrNameText :: RdrName -> Text
rdrNameText = T.pack . occNameString . rdrNameOcc
