{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Argus.Analysis.Fingerprint
-- Description : AST fingerprinting for code clone detection using GHC AST
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides AST fingerprinting capabilities for detecting
-- code clones and duplicated logic. It uses the GHC parser (via ghc-lib-parser)
-- for accurate AST analysis and structural hashing with proper alpha-renaming.
module Argus.Analysis.Fingerprint
  ( -- * Types
    FunctionFingerprint (..)
  , NormalizedAST (..)
  , NormalizedPattern (..)
  , NormalizedLit (..)
  , NormalizedStmt (..)

    -- * Fingerprinting Functions
  , fingerprintFunction
  , fingerprintExpr
  , normalizeAST
  , normalizeFromText
  , normalizeExpr
  , computeHash

    -- * Metrics
  , countNodes
  , computeDepth

    -- * Comparison
  , fingerprintEqual
  , fingerprintSimilar
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Data.Hashable (Hashable, hash)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Bits (xor, rotateL)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- GHC Parser imports
import "ghc-lib-parser" GHC.Hs
import "ghc-lib-parser" GHC.Types.SrcLoc (GenLocated (L), unLoc)
import "ghc-lib-parser" GHC.Types.Name.Reader (rdrNameOcc)
import "ghc-lib-parser" GHC.Types.Name.Occurrence (occNameString)
import "ghc-lib-parser" GHC.Driver.Session (DynFlags, defaultDynFlags, xopt_set)
import "ghc-lib-parser" GHC.LanguageExtensions.Type (Extension(..))
import "ghc-lib-parser" GHC.Parser.Lexer qualified as Lexer
import "ghc-lib-parser" GHC.Data.FastString (unpackFS)
import "ghc-lib-parser" GHC.Data.Bag (bagToList)
import "ghc-lib-parser" GHC.Types.SourceText (IntegralLit(..), rationalFromFractionalLit)
import Language.Haskell.GhclibParserEx.GHC.Parser qualified as GhclibParser
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)

import Argus.Types (SrcSpan(..))

--------------------------------------------------------------------------------
-- Core Data Types
--------------------------------------------------------------------------------

-- | Fingerprint of a function for clone detection
data FunctionFingerprint = FunctionFingerprint
  { fpName         :: Text              -- ^ Function name
  , fpSpan         :: SrcSpan           -- ^ Source location
  , fpHash         :: Word64            -- ^ Structural hash
  , fpTokenHash    :: Word64            -- ^ Token sequence hash (for pre-filtering)
  , fpNormalized   :: NormalizedAST     -- ^ Normalized AST
  , fpNodeCount    :: Int               -- ^ Number of AST nodes
  , fpDepth        :: Int               -- ^ Maximum AST depth
  , fpLineCount    :: Int               -- ^ Number of source lines
  , fpOriginalCode :: Text              -- ^ Original source code
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Normalized AST with alpha-renamed variables
-- Variables are replaced with de Bruijn indices for structural comparison
data NormalizedAST
  = NLit NormalizedLit                          -- ^ Literal value
  | NVar Int                                     -- ^ Variable (de Bruijn index)
  | NApp NormalizedAST NormalizedAST             -- ^ Function application
  | NLam Int NormalizedAST                       -- ^ Lambda with bound variable index
  | NCase NormalizedAST [(NormalizedPattern, NormalizedAST)]  -- ^ Case expression
  | NIf NormalizedAST NormalizedAST NormalizedAST -- ^ If expression
  | NLet [(Int, NormalizedAST)] NormalizedAST    -- ^ Let bindings
  | NDo [NormalizedStmt]                         -- ^ Do notation
  | NInfix Text NormalizedAST NormalizedAST      -- ^ Infix operator
  | NList [NormalizedAST]                        -- ^ List literal/expression
  | NTuple [NormalizedAST]                       -- ^ Tuple
  | NRecord [(Text, NormalizedAST)]              -- ^ Record construction
  | NWildcard                                    -- ^ Wildcard/hole (for partial matching)
  | NCon Text                                    -- ^ Constructor
  | NOp Text                                     -- ^ Operator
  | NNeg NormalizedAST                           -- ^ Negation
  | NSection (Maybe NormalizedAST) Text (Maybe NormalizedAST) -- ^ Section (left/right)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

-- | Normalized pattern for case expressions
data NormalizedPattern
  = NPVar Int                    -- ^ Variable pattern (alpha-renamed)
  | NPLit NormalizedLit          -- ^ Literal pattern
  | NPCon Text [NormalizedPattern]  -- ^ Constructor pattern
  | NPWild                       -- ^ Wildcard pattern
  | NPTuple [NormalizedPattern]  -- ^ Tuple pattern
  | NPList [NormalizedPattern]   -- ^ List pattern
  | NPAs Int NormalizedPattern   -- ^ As-pattern
  | NPView NormalizedAST NormalizedPattern -- ^ View pattern
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

-- | Normalized literal values
data NormalizedLit
  = NLitInt Integer
  | NLitFloat Double
  | NLitChar Char
  | NLitString Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

-- | Normalized do-notation statement
data NormalizedStmt
  = NStmtBind Int NormalizedAST   -- ^ x <- expr
  | NStmtExpr NormalizedAST       -- ^ expr
  | NStmtLet [(Int, NormalizedAST)]  -- ^ let bindings
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

--------------------------------------------------------------------------------
-- Alpha Renaming State (Scope-aware)
--------------------------------------------------------------------------------

-- | State for alpha-renaming during normalization
-- Uses a scope stack for proper de Bruijn indexing
data AlphaState = AlphaState
  { asNextVar   :: Int                  -- ^ Next variable index
  , asScopes    :: [Map Text Int]       -- ^ Stack of scopes (inner first)
  }

emptyAlphaState :: AlphaState
emptyAlphaState = AlphaState 0 [Map.empty]

-- | Enter a new scope
pushScope :: AlphaState -> AlphaState
pushScope st = st { asScopes = Map.empty : asScopes st }

-- | Exit current scope
popScope :: AlphaState -> AlphaState
popScope st = st { asScopes = drop 1 (asScopes st) }

-- | Look up a variable in current scope chain
lookupVar :: Text -> AlphaState -> Maybe Int
lookupVar name AlphaState{..} =
  go asScopes
  where
    go [] = Nothing
    go (scope:rest) = case Map.lookup name scope of
      Just idx -> Just idx
      Nothing -> go rest

-- | Bind a new variable in current scope
bindVar :: Text -> AlphaState -> (Int, AlphaState)
bindVar name st@AlphaState{..} =
  let idx = asNextVar
      newScope = case asScopes of
        [] -> [Map.singleton name idx]
        (s:rest) -> Map.insert name idx s : rest
  in (idx, st { asNextVar = idx + 1, asScopes = newScope })

-- | Look up or create a new variable index (for free variables)
lookupOrCreate :: Text -> AlphaState -> (Int, AlphaState)
lookupOrCreate name st =
  case lookupVar name st of
    Just idx -> (idx, st)
    Nothing -> bindVar name st

--------------------------------------------------------------------------------
-- Fingerprinting Functions
--------------------------------------------------------------------------------

-- | Create a fingerprint for a function from its source code
-- Uses ghc-lib-parser for accurate AST parsing
fingerprintFunction :: Text -> Text -> SrcSpan -> FunctionFingerprint
fingerprintFunction name code span =
  let normalized = normalizeFromText code
      nodeCount = countNodes normalized
      depth = computeDepth normalized
      structHash = computeHash normalized
      tokHash = computeTokenHash code
  in FunctionFingerprint
    { fpName = name
    , fpSpan = span
    , fpHash = structHash
    , fpTokenHash = tokHash
    , fpNormalized = normalized
    , fpNodeCount = nodeCount
    , fpDepth = depth
    , fpLineCount = length $ T.lines code
    , fpOriginalCode = code
    }

-- | Fingerprint a single expression (for sub-expression matching)
fingerprintExpr :: Text -> (Word64, NormalizedAST)
fingerprintExpr code =
  let normalized = normalizeFromText code
  in (computeHash normalized, normalized)

-- | Normalize source code to AST using GHC parser
normalizeFromText :: Text -> NormalizedAST
normalizeFromText code =
  let source = T.unpack code
  in case parseExprWithDynFlags baseDynFlags source of
    Just expr -> normalizeExpr emptyAlphaState (unLoc expr)
    Nothing ->
      -- If direct expression parsing fails, try parsing as a function definition
      parseAsFunction code

-- | Try to parse code as a function definition
parseAsFunction :: Text -> NormalizedAST
parseAsFunction code =
  let source = T.unpack code
      wrapped = "module M where\n" ++ source
  in case GhclibParser.parseFile "<input>" baseDynFlags wrapped of
    Lexer.POk _ (L _ m) ->
      case hsmodDecls m of
        (L _ (ValD _ (FunBind _ _ mg)) : _) ->
          normalizeMatchGroup emptyAlphaState mg
        (L _ (ValD _ (PatBind { pat_rhs = grhss })) : _) ->  -- GHC 9.10: record syntax
          normalizeGRHSs emptyAlphaState grhss
        _ -> NWildcard
    Lexer.PFailed _ -> NWildcard

-- | Parse expression with given DynFlags
parseExprWithDynFlags :: DynFlags -> String -> Maybe (LHsExpr GhcPs)
parseExprWithDynFlags dflags source =
  case GhclibParser.parseExpression source dflags of
    Lexer.POk _ expr -> Just expr
    Lexer.PFailed _ -> Nothing

-- | Base DynFlags for parsing with common extensions enabled
baseDynFlags :: DynFlags
baseDynFlags = foldl xopt_set (defaultDynFlags fakeSettings) defaultExtensions
  where
    defaultExtensions =
      [ BangPatterns
      , ConstraintKinds
      , DataKinds
      , DefaultSignatures
      , DeriveAnyClass
      , DeriveFunctor
      , DeriveGeneric
      , DerivingStrategies
      , DerivingVia
      , ExistentialQuantification
      , ExplicitForAll
      , FlexibleContexts
      , FlexibleInstances
      , GADTs
      , GeneralizedNewtypeDeriving
      , ImportQualifiedPost
      , LambdaCase
      , MultiParamTypeClasses
      , OverloadedStrings
      , RankNTypes
      , RecordWildCards
      , ScopedTypeVariables
      , TupleSections
      , TypeApplications
      , TypeFamilies
      , TypeOperators
      , ViewPatterns
      ]

--------------------------------------------------------------------------------
-- GHC AST Normalization
--------------------------------------------------------------------------------

-- | Normalize a GHC expression to our simplified representation
normalizeExpr :: AlphaState -> HsExpr GhcPs -> NormalizedAST
normalizeExpr st expr = case expr of
  -- Variables
  HsVar _ (L _ name) ->
    let nameText = T.pack $ occNameString $ rdrNameOcc name
    in case lookupVar nameText st of
      Just idx -> NVar idx
      Nothing ->
        -- Free variable or constructor
        if isConstructorName nameText
          then NCon nameText
          else let (idx, _) = lookupOrCreate nameText st in NVar idx

  -- Literals
  HsLit _ lit -> NLit $ normalizeLit lit
  HsOverLit _ olit -> NLit $ normalizeOverLit olit

  -- Application
  HsApp _ f arg ->
    NApp (normalizeExpr st (unLoc f)) (normalizeExpr st (unLoc arg))

  -- Type application (ignore type, keep expression) - GHC 9.10: 3 args
  HsAppType _ f _ -> normalizeExpr st (unLoc f)

  -- Lambda - GHC 9.10: 3 args
  HsLam _ _ mg -> normalizeMatchGroup st mg

  -- Let expression - GHC 9.10: 3 args
  HsLet _ binds body ->
    let (bindings, st') = normalizeLocalBinds st binds
    in NLet bindings (normalizeExpr st' (unLoc body))

  -- If expression
  HsIf _ cond t f ->
    NIf (normalizeExpr st (unLoc cond))
        (normalizeExpr st (unLoc t))
        (normalizeExpr st (unLoc f))

  -- Case expression
  HsCase _ scrut mg ->
    let scrutNorm = normalizeExpr st (unLoc scrut)
        alts = normalizeMatchGroupAlts st mg
    in NCase scrutNorm alts

  -- Do notation
  HsDo _ _ (L _ stmts) ->
    NDo $ normalizeStmts st stmts

  -- Infix operator
  OpApp _ l op r ->
    let opText = extractOpName (unLoc op)
    in NInfix opText (normalizeExpr st (unLoc l)) (normalizeExpr st (unLoc r))

  -- Negation
  NegApp _ e _ -> NNeg (normalizeExpr st (unLoc e))

  -- Sections
  SectionL _ e op ->
    let opText = extractOpName (unLoc op)
    in NSection (Just $ normalizeExpr st (unLoc e)) opText Nothing

  SectionR _ op e ->
    let opText = extractOpName (unLoc op)
    in NSection Nothing opText (Just $ normalizeExpr st (unLoc e))

  -- Explicit list
  ExplicitList _ exprs ->
    NList $ map (normalizeExpr st . unLoc) exprs

  -- Explicit tuple
  ExplicitTuple _ args boxity ->
    let normArgs = map normalizeTupleArg args
    in NTuple normArgs
    where
      normalizeTupleArg (Present _ e) = normalizeExpr st (unLoc e)
      normalizeTupleArg _ = NWildcard

  -- Parenthesized expression - GHC 9.10: 2 args
  HsPar _ inner -> normalizeExpr st (unLoc inner)

  -- Record construction
  RecordCon _ (L _ con) fields ->
    let conName = T.pack $ occNameString $ rdrNameOcc con
        fieldList = normalizeRecordBinds st fields
    in NRecord [(conName, NRecord fieldList)]

  -- Record update
  RecordUpd _ e fields ->
    let baseExpr = normalizeExpr st (unLoc e)
    in NApp baseExpr NWildcard  -- Simplified: treat update as application

  -- Expression with type signature
  ExprWithTySig _ e _ -> normalizeExpr st (unLoc e)

  -- Arithmetic sequence
  ArithSeq _ _ seqInfo -> normalizeArithSeq st seqInfo

  -- Other cases
  _ -> NWildcard

-- | Normalize a match group (for lambdas and function definitions)
normalizeMatchGroup :: AlphaState -> MatchGroup GhcPs (LHsExpr GhcPs) -> NormalizedAST
normalizeMatchGroup st (MG _ (L _ matches)) =
  case matches of
    [] -> NWildcard
    [L _ match] -> normalizeMatch st match
    ms ->
      -- Multiple clauses: create case-like structure
      let alts = map (normalizeMatchToAlt st . unLoc) ms
      in NCase NWildcard alts

-- | Normalize a single match
normalizeMatch :: AlphaState -> Match GhcPs (LHsExpr GhcPs) -> NormalizedAST
normalizeMatch st (Match _ _ pats grhss) =
  let st' = pushScope st
      (patNorms, st'') = normalizePats st' pats
      body = normalizeGRHSs st'' grhss
  in foldr (\patIdx acc -> NLam patIdx acc) body (map patToIdx patNorms)
  where
    patToIdx (NPVar idx) = idx
    patToIdx _ = -1  -- Non-variable patterns get -1

-- | Normalize a match to an alternative (for case expressions)
normalizeMatchToAlt :: AlphaState -> Match GhcPs (LHsExpr GhcPs) -> (NormalizedPattern, NormalizedAST)
normalizeMatchToAlt st (Match _ _ pats grhss) =
  let st' = pushScope st
      (patNorms, st'') = normalizePats st' pats
      body = normalizeGRHSs st'' grhss
      combinedPat = case patNorms of
        [] -> NPWild
        [p] -> p
        ps -> NPTuple ps
  in (combinedPat, body)

-- | Normalize match group to list of alternatives (for case expressions)
normalizeMatchGroupAlts :: AlphaState -> MatchGroup GhcPs (LHsExpr GhcPs) -> [(NormalizedPattern, NormalizedAST)]
normalizeMatchGroupAlts st (MG _ (L _ matches)) =
  map (normalizeMatchToAlt st . unLoc) matches

-- | Normalize GRHSs (guarded right-hand sides)
normalizeGRHSs :: AlphaState -> GRHSs GhcPs (LHsExpr GhcPs) -> NormalizedAST
normalizeGRHSs st (GRHSs _ grhss localBinds) =
  let (bindings, st') = normalizeLocalBinds st localBinds
      body = case grhss of
        [] -> NWildcard
        [L _ (GRHS _ [] body')] -> normalizeExpr st' (unLoc body')
        [L _ (GRHS _ _guards body')] ->
          -- For guarded expressions, just normalize the body
          -- Full guard handling would require normalizing guard statements
          normalizeExpr st' (unLoc body')
        _ ->
          -- Multiple GRHSs become case-like
          NWildcard
  in if null bindings
     then body
     else NLet bindings body

-- | Normalize local bindings
normalizeLocalBinds :: AlphaState -> HsLocalBinds GhcPs -> ([(Int, NormalizedAST)], AlphaState)
normalizeLocalBinds st (HsValBinds _ (ValBinds _ binds _)) =
  let bindList = bagToList binds
  in foldr processBinding ([], st) bindList
  where
    processBinding (L _ bind) (acc, s) = case bind of
      FunBind _ (L _ name) mg ->
        let nameText = T.pack $ occNameString $ rdrNameOcc name
            (idx, s') = bindVar nameText s
            body = normalizeMatchGroup s' mg
        in ((idx, body) : acc, s')
      PatBind { pat_lhs = pat, pat_rhs = grhss } ->  -- GHC 9.10: record syntax
        -- For pattern bindings, extract variable names
        let (patNorm, s') = normalizePat s (unLoc pat)
            body = normalizeGRHSs s' grhss
            idx = case patNorm of
              NPVar i -> i
              _ -> -1
        in ((idx, body) : acc, s')
      _ -> (acc, s)
normalizeLocalBinds st (HsValBinds _ (XValBindsLR _)) = ([], st)
normalizeLocalBinds st (HsIPBinds _ _) = ([], st)
normalizeLocalBinds st (EmptyLocalBinds _) = ([], st)

-- | Normalize patterns
normalizePats :: AlphaState -> [LPat GhcPs] -> ([NormalizedPattern], AlphaState)
normalizePats st [] = ([], st)
normalizePats st (p:ps) =
  let (patNorm, st') = normalizePat st (unLoc p)
      (restNorm, st'') = normalizePats st' ps
  in (patNorm : restNorm, st'')

-- | Normalize a single pattern
normalizePat :: AlphaState -> Pat GhcPs -> (NormalizedPattern, AlphaState)
normalizePat st pat = case pat of
  WildPat _ -> (NPWild, st)

  VarPat _ (L _ name) ->
    let nameText = T.pack $ occNameString $ rdrNameOcc name
        (idx, st') = bindVar nameText st
    in (NPVar idx, st')

  LitPat _ lit -> (NPLit $ normalizeLit lit, st)

  NPat _ (L _ olit) _ _ -> (NPLit $ normalizeOverLit olit, st)

  ConPat _ (L _ con) details ->
    let conName = T.pack $ occNameString $ rdrNameOcc con
        (subPats, st') = normalizeConPatDetails st details
    in (NPCon conName subPats, st')

  TuplePat _ pats _ ->
    let (patNorms, st') = normalizePats st pats
    in (NPTuple patNorms, st')

  ListPat _ pats ->
    let (patNorms, st') = normalizePats st pats
    in (NPList patNorms, st')

  AsPat _ (L _ name) pat' ->  -- GHC 9.10: 3 args (ext, name, pat)
    let nameText = T.pack $ occNameString $ rdrNameOcc name
        (idx, st') = bindVar nameText st
        (patNorm, st'') = normalizePat st' (unLoc pat')
    in (NPAs idx patNorm, st'')

  ViewPat _ expr pat' ->
    let exprNorm = normalizeExpr st (unLoc expr)
        (patNorm, st') = normalizePat st (unLoc pat')
    in (NPView exprNorm patNorm, st')

  ParPat _ pat' ->  -- GHC 9.10: 2 args
    normalizePat st (unLoc pat')

  BangPat _ pat' ->
    normalizePat st (unLoc pat')

  SigPat _ pat' _ ->
    normalizePat st (unLoc pat')

  _ -> (NPWild, st)

-- | Normalize constructor pattern details
normalizeConPatDetails :: AlphaState -> HsConPatDetails GhcPs -> ([NormalizedPattern], AlphaState)
normalizeConPatDetails st details = case details of
  PrefixCon _ args ->
    normalizePats st args
  InfixCon l r ->
    let (lNorm, st') = normalizePat st (unLoc l)
        (rNorm, st'') = normalizePat st' (unLoc r)
    in ([lNorm, rNorm], st'')
  RecCon (HsRecFields fields _) ->
    let patList = map (hfbRHS . unLoc) fields
    in normalizePats st patList

-- | Normalize do-notation statements
normalizeStmts :: AlphaState -> [LStmt GhcPs (LHsExpr GhcPs)] -> [NormalizedStmt]
normalizeStmts _ [] = []
normalizeStmts st (L _ stmt : rest) = case stmt of
  BodyStmt _ body _ _ ->
    NStmtExpr (normalizeExpr st (unLoc body)) : normalizeStmts st rest

  BindStmt _ pat body ->
    let (patNorm, st') = normalizePat st (unLoc pat)
        idx = case patNorm of
          NPVar i -> i
          _ -> -1
    in NStmtBind idx (normalizeExpr st (unLoc body)) : normalizeStmts st' rest

  LetStmt _ binds ->
    let (bindings, st') = normalizeLocalBinds st binds
    in NStmtLet bindings : normalizeStmts st' rest

  _ -> normalizeStmts st rest

-- | Normalize a literal
normalizeLit :: HsLit GhcPs -> NormalizedLit
normalizeLit lit = case lit of
  HsChar _ c -> NLitChar c
  HsCharPrim _ c -> NLitChar c
  HsString _ fs -> NLitString $ T.pack $ unpackFS fs
  HsStringPrim _ bs -> NLitString $ T.pack $ show bs
  HsInt _ il -> NLitInt $ il_value il
  HsIntPrim _ i -> NLitInt i
  HsWordPrim _ w -> NLitInt $ fromIntegral w
  HsInt8Prim _ i -> NLitInt $ fromIntegral i
  HsInt16Prim _ i -> NLitInt $ fromIntegral i
  HsInt32Prim _ i -> NLitInt $ fromIntegral i
  HsInt64Prim _ i -> NLitInt $ fromIntegral i
  HsWord8Prim _ w -> NLitInt $ fromIntegral w
  HsWord16Prim _ w -> NLitInt $ fromIntegral w
  HsWord32Prim _ w -> NLitInt $ fromIntegral w
  HsWord64Prim _ w -> NLitInt $ fromIntegral w
  HsFloatPrim _ r -> NLitFloat $ fromRational $ rationalFromFractionalLit r
  HsDoublePrim _ r -> NLitFloat $ fromRational $ rationalFromFractionalLit r
  _ -> NLitInt 0  -- Fallback

-- | Normalize an overloaded literal
normalizeOverLit :: HsOverLit GhcPs -> NormalizedLit
normalizeOverLit (OverLit _ val) = case val of
  HsIntegral il -> NLitInt $ il_value il
  HsFractional fl -> NLitFloat $ fromRational $ rationalFromFractionalLit fl
  HsIsString _ fs -> NLitString $ T.pack $ unpackFS fs

-- | Normalize arithmetic sequence
normalizeArithSeq :: AlphaState -> ArithSeqInfo GhcPs -> NormalizedAST
normalizeArithSeq st seqInfo = case seqInfo of
  From e -> NApp (NCon "enumFrom") (normalizeExpr st (unLoc e))
  FromThen e1 e2 ->
    NApp (NApp (NCon "enumFromThen") (normalizeExpr st (unLoc e1)))
         (normalizeExpr st (unLoc e2))
  FromTo e1 e2 ->
    NApp (NApp (NCon "enumFromTo") (normalizeExpr st (unLoc e1)))
         (normalizeExpr st (unLoc e2))
  FromThenTo e1 e2 e3 ->
    NApp (NApp (NApp (NCon "enumFromThenTo") (normalizeExpr st (unLoc e1)))
               (normalizeExpr st (unLoc e2)))
         (normalizeExpr st (unLoc e3))

-- | Normalize record bindings
normalizeRecordBinds :: AlphaState -> HsRecordBinds GhcPs -> [(Text, NormalizedAST)]
normalizeRecordBinds st (HsRecFields fields _) =
  map normalizeField fields
  where
    normalizeField (L _ field) =
      let L _ fieldOcc = hfbLHS field
          fieldName = T.pack $ occNameString $ rdrNameOcc $ unLoc $ foLabel fieldOcc
          fieldExpr = normalizeExpr st (unLoc $ hfbRHS field)
      in (fieldName, fieldExpr)

-- | Extract operator name from expression
extractOpName :: HsExpr GhcPs -> Text
extractOpName (HsVar _ (L _ name)) = T.pack $ occNameString $ rdrNameOcc name
extractOpName _ = "op"

-- | Check if a name is a constructor (starts with uppercase)
isConstructorName :: Text -> Bool
isConstructorName t =
  not (T.null t) &&
  let c = T.head t
  in (c >= 'A' && c <= 'Z') || c == ':'

--------------------------------------------------------------------------------
-- Structural Hashing
--------------------------------------------------------------------------------

-- | Compute structural hash of normalized AST (Merkle-tree style)
computeHash :: NormalizedAST -> Word64
computeHash = \case
  NLit lit -> hashLit lit
  NVar idx -> hashWithTag 1 $ fromIntegral idx
  NApp f x -> hashWithTag 2 $ combineHashes [computeHash f, computeHash x]
  NLam idx body -> hashWithTag 3 $ combineHashes [fromIntegral idx, computeHash body]
  NCase scrut alts -> hashWithTag 4 $ combineHashes $
    computeHash scrut : map hashAlt alts
  NIf c t e -> hashWithTag 5 $ combineHashes [computeHash c, computeHash t, computeHash e]
  NLet binds body -> hashWithTag 6 $ combineHashes $
    computeHash body : map (computeHash . snd) binds
  NDo stmts -> hashWithTag 7 $ combineHashes $ map hashStmt stmts
  NInfix op l r -> hashWithTag 8 $ combineHashes [hashText op, computeHash l, computeHash r]
  NList xs -> hashWithTag 9 $ combineHashes $ map computeHash xs
  NTuple xs -> hashWithTag 10 $ combineHashes $ map computeHash xs
  NRecord fields -> hashWithTag 11 $ combineHashes $
    map (\(k, v) -> combineHashes [hashText k, computeHash v]) fields
  NWildcard -> hashWithTag 12 0
  NCon name -> hashWithTag 13 $ hashText name
  NOp op -> hashWithTag 14 $ hashText op
  NNeg e -> hashWithTag 15 $ computeHash e
  NSection ml op mr -> hashWithTag 16 $ combineHashes $
    [hashText op] ++ maybe [] (pure . computeHash) ml ++ maybe [] (pure . computeHash) mr

  where
    hashLit = \case
      NLitInt n -> hashWithTag 20 $ fromIntegral (hash n)
      NLitFloat d -> hashWithTag 21 $ fromIntegral (hash d)
      NLitChar c -> hashWithTag 22 $ fromIntegral (hash c)
      NLitString s -> hashWithTag 23 $ hashText s

    hashAlt (pat, body) = combineHashes [hashPattern pat, computeHash body]

    hashPattern = \case
      NPVar idx -> hashWithTag 30 $ fromIntegral idx
      NPLit lit -> hashWithTag 31 $ fromIntegral (hash lit)
      NPCon name pats -> hashWithTag 32 $ combineHashes $
        hashText name : map hashPattern pats
      NPWild -> hashWithTag 33 0
      NPTuple pats -> hashWithTag 34 $ combineHashes $ map hashPattern pats
      NPList pats -> hashWithTag 35 $ combineHashes $ map hashPattern pats
      NPAs idx pat -> hashWithTag 36 $ combineHashes [fromIntegral idx, hashPattern pat]
      NPView expr pat -> hashWithTag 37 $ combineHashes [computeHash expr, hashPattern pat]

    hashStmt = \case
      NStmtBind idx expr -> hashWithTag 40 $ combineHashes [fromIntegral idx, computeHash expr]
      NStmtExpr expr -> hashWithTag 41 $ computeHash expr
      NStmtLet binds -> hashWithTag 42 $ combineHashes $ map (computeHash . snd) binds

    hashWithTag :: Word64 -> Word64 -> Word64
    hashWithTag tag val = rotateL (tag `xor` val) 7

    hashText :: Text -> Word64
    hashText = fromIntegral . hash

    combineHashes :: [Word64] -> Word64
    combineHashes = foldr (\h acc -> rotateL (h `xor` acc) 5) 0

-- | Compute token hash for quick pre-filtering
computeTokenHash :: Text -> Word64
computeTokenHash code =
  let tokens = T.words $ T.filter (\c -> c /= '(' && c /= ')' && c /= '[' && c /= ']') code
      tokenHashes = map (fromIntegral . hash) tokens
  in foldr xor 0 tokenHashes

--------------------------------------------------------------------------------
-- AST Metrics
--------------------------------------------------------------------------------

-- | Count the number of nodes in normalized AST
countNodes :: NormalizedAST -> Int
countNodes = \case
  NLit _ -> 1
  NVar _ -> 1
  NApp f x -> 1 + countNodes f + countNodes x
  NLam _ body -> 1 + countNodes body
  NCase scrut alts -> 1 + countNodes scrut + sum (map (countNodes . snd) alts)
  NIf c t e -> 1 + countNodes c + countNodes t + countNodes e
  NLet binds body -> 1 + countNodes body + sum (map (countNodes . snd) binds)
  NDo stmts -> 1 + sum (map countStmtNodes stmts)
  NInfix _ l r -> 1 + countNodes l + countNodes r
  NList xs -> 1 + sum (map countNodes xs)
  NTuple xs -> 1 + sum (map countNodes xs)
  NRecord fields -> 1 + sum (map (countNodes . snd) fields)
  NWildcard -> 1
  NCon _ -> 1
  NOp _ -> 1
  NNeg e -> 1 + countNodes e
  NSection ml _ mr -> 1 + maybe 0 countNodes ml + maybe 0 countNodes mr
  where
    countStmtNodes = \case
      NStmtBind _ expr -> 1 + countNodes expr
      NStmtExpr expr -> countNodes expr
      NStmtLet binds -> 1 + sum (map (countNodes . snd) binds)

-- | Compute maximum depth of normalized AST
computeDepth :: NormalizedAST -> Int
computeDepth = \case
  NLit _ -> 1
  NVar _ -> 1
  NApp f x -> 1 + max (computeDepth f) (computeDepth x)
  NLam _ body -> 1 + computeDepth body
  NCase scrut alts -> 1 + max (computeDepth scrut) (maxDepthAlts alts)
  NIf c t e -> 1 + maximum [computeDepth c, computeDepth t, computeDepth e]
  NLet binds body -> 1 + max (computeDepth body) (maxDepthBinds binds)
  NDo stmts -> 1 + maxDepthStmts stmts
  NInfix _ l r -> 1 + max (computeDepth l) (computeDepth r)
  NList xs -> 1 + maxDepthList xs
  NTuple xs -> 1 + maxDepthList xs
  NRecord fields -> 1 + maxDepthList (map snd fields)
  NWildcard -> 1
  NCon _ -> 1
  NOp _ -> 1
  NNeg e -> 1 + computeDepth e
  NSection ml _ mr -> 1 + max (maybe 0 computeDepth ml) (maybe 0 computeDepth mr)
  where
    maxDepthAlts [] = 0
    maxDepthAlts alts = maximum $ map (computeDepth . snd) alts

    maxDepthBinds [] = 0
    maxDepthBinds binds = maximum $ map (computeDepth . snd) binds

    maxDepthList [] = 0
    maxDepthList xs = maximum $ map computeDepth xs

    maxDepthStmts [] = 0
    maxDepthStmts stmts = maximum $ map stmtDepth stmts

    stmtDepth = \case
      NStmtBind _ expr -> computeDepth expr
      NStmtExpr expr -> computeDepth expr
      NStmtLet binds -> maxDepthBinds binds

--------------------------------------------------------------------------------
-- Comparison Functions
--------------------------------------------------------------------------------

-- | Check if two fingerprints are structurally equal (Type 1-2 clones)
fingerprintEqual :: FunctionFingerprint -> FunctionFingerprint -> Bool
fingerprintEqual fp1 fp2 =
  fpHash fp1 == fpHash fp2 &&
  fpNormalized fp1 == fpNormalized fp2

-- | Check if two fingerprints are similar (for Type 3 clone detection)
-- Returns True if the structural similarity is above threshold
fingerprintSimilar :: Double -> FunctionFingerprint -> FunctionFingerprint -> Bool
fingerprintSimilar threshold fp1 fp2 =
  let similarity = computeSimilarity (fpNormalized fp1) (fpNormalized fp2)
  in similarity >= threshold

-- | Compute structural similarity between two normalized ASTs
-- Returns a value between 0.0 (completely different) and 1.0 (identical)
computeSimilarity :: NormalizedAST -> NormalizedAST -> Double
computeSimilarity ast1 ast2 =
  let nodes1 = countNodes ast1
      nodes2 = countNodes ast2
      commonNodes = countCommonNodes ast1 ast2
      totalNodes = nodes1 + nodes2
  in if totalNodes == 0
     then 1.0
     else 2.0 * fromIntegral commonNodes / fromIntegral totalNodes

-- | Count common structural nodes between two ASTs
countCommonNodes :: NormalizedAST -> NormalizedAST -> Int
countCommonNodes ast1 ast2 = case (ast1, ast2) of
  (NLit l1, NLit l2) | l1 == l2 -> 1
  (NVar _, NVar _) -> 1  -- Variables always match after alpha-renaming
  (NApp f1 x1, NApp f2 x2) -> 1 + countCommonNodes f1 f2 + countCommonNodes x1 x2
  (NLam _ b1, NLam _ b2) -> 1 + countCommonNodes b1 b2
  (NIf c1 t1 e1, NIf c2 t2 e2) ->
    1 + countCommonNodes c1 c2 + countCommonNodes t1 t2 + countCommonNodes e1 e2
  (NInfix op1 l1 r1, NInfix op2 l2 r2) | op1 == op2 ->
    1 + countCommonNodes l1 l2 + countCommonNodes r1 r2
  (NList xs1, NList xs2) ->
    1 + sum (zipWith countCommonNodes xs1 xs2)
  (NTuple xs1, NTuple xs2) ->
    1 + sum (zipWith countCommonNodes xs1 xs2)
  (NCon n1, NCon n2) | n1 == n2 -> 1
  (NOp o1, NOp o2) | o1 == o2 -> 1
  (NWildcard, NWildcard) -> 1
  (NNeg e1, NNeg e2) -> 1 + countCommonNodes e1 e2
  _ -> 0

--------------------------------------------------------------------------------
-- Normalize AST (for full GHC AST integration)
--------------------------------------------------------------------------------

-- | Normalize a GHC AST to our simplified representation
normalizeAST :: LHsExpr GhcPs -> NormalizedAST
normalizeAST (L _ expr) = normalizeExpr emptyAlphaState expr
