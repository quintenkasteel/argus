{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Extra.Function (extract, getArgByType) where

import ClassyPrelude
import Data.String.Interpolate (i)
import qualified Data.Text as Text
import Function (Function (..), FunctionArgument (..))
import Language.Haskell.Exts
  ( Decl (..),
    Exp (..),
    Match (..),
    Name (..),
    Pat (..),
    Rhs (..),
    SrcSpanInfo,
    Type (..),
  )
import qualified Language.Haskell.Exts as Haskell

extract :: FilePath -> [Decl SrcSpanInfo] -> [Function]
extract _ decls = extract' decls
  where
    extract' :: [Decl SrcSpanInfo] -> [Function]
    extract' ((TypeSig _ names ty) : f@(FunBind _ funcBind) : xs) =
      catMaybes (fmap (toFunction f funcBind ty) names) ++ extract' xs
    extract' (_ : xs) = extract' xs
    extract' [] = []

    toFunction ::
      Decl SrcSpanInfo ->
      [Match SrcSpanInfo] ->
      Type SrcSpanInfo ->
      Name SrcSpanInfo ->
      Maybe Function
    toFunction f ((Match _ _ vars _ _) : _) ty n =
      Just
        Function
          { name = pack (Haskell.prettyPrint n),
            arguments = fmap (uncurry makeFunctionArgument) (zip (getFuncTypes ty) (fmap getPatInfo vars)),
            body = extractBodyLines f
          }
      where
        getFuncTypes :: Type SrcSpanInfo -> [(Text, (Int, Int))]
        getFuncTypes (TyFun srcSpanInfo ty1 ty2) = (pack (Haskell.prettyPrint ty1), fromInfo srcSpanInfo) : getFuncTypes ty2
        getFuncTypes (TyParen _ ty) = getFuncTypes ty
        getFuncTypes ty = [(pack (Haskell.prettyPrint ty), getTypeStartPos ty)]

        getPatInfo :: Pat SrcSpanInfo -> (Text, (Int, Int))
        getPatInfo v = (pack (Haskell.prettyPrint v), getPatStartPos v)

        makeFunctionArgument :: (Text, (Int, Int)) -> (Text, (Int, Int)) -> FunctionArgument
        makeFunctionArgument (t, tPos) (v, pos) = FunctionArgument t tPos v pos
    toFunction _ _ _ _ = Nothing

-- Extracts the line numbers and text of the body of a function declaration
extractBodyLines :: Decl SrcSpanInfo -> [(Int, Text)]
extractBodyLines decl =
  case decl of
    FunBind _ matches -> concatMap (extractMatchLines . getMatchBody) matches
    _ -> []

-- Extracts the body of a match
getMatchBody :: Match l -> Exp l
getMatchBody (Match _ _ _ (UnGuardedRhs _ exp) _) = exp
getMatchBody _ = error "Pattern match failure: expected UnGuardedRhs"

-- Extracts the line numbers and text of the body of a match
extractMatchLines :: Exp SrcSpanInfo -> [(Int, Text)]
extractMatchLines (Lambda _ _ e) = extractLambdaLines e
extractMatchLines e = extractExpLines e

-- Extracts the line numbers and text of the body of a lambda expression
extractLambdaLines :: Exp SrcSpanInfo -> [(Int, Text)]
extractLambdaLines (Lambda _ _ e) = extractLambdaLines e
extractLambdaLines e = extractExpLines e

-- Extracts the line numbers and text of an expression
extractExpLines :: Exp SrcSpanInfo -> [(Int, Text)]
extractExpLines exp =
  zip
    [(getExpStartPos exp) ..]
    ((lines (Text.strip (pack (Haskell.prettyPrint exp)))))

getArgByType :: Text -> [FunctionArgument] -> Maybe FunctionArgument
getArgByType typeArg =
  find (\(FunctionArgument {type_}) -> typeArg == type_ || [i|(#{typeArg})|] == type_)

-- Function to extract start position for each constructor of Pat
getPatStartPos :: Pat SrcSpanInfo -> (Int, Int)
getPatStartPos (PApp srcSpanInfo _ _) = fromInfo srcSpanInfo
getPatStartPos (PAsPat srcSpanInfo _ _) = fromInfo srcSpanInfo
getPatStartPos (PatTypeSig srcSpanInfo _ _) = fromInfo srcSpanInfo
getPatStartPos (PBangPat srcSpanInfo _) = fromInfo srcSpanInfo
getPatStartPos (PInfixApp srcSpanInfo _ _ _) = fromInfo srcSpanInfo
getPatStartPos (PIrrPat srcSpanInfo _) = fromInfo srcSpanInfo
getPatStartPos (PList srcSpanInfo _) = fromInfo srcSpanInfo
getPatStartPos (PLit srcSpanInfo _ _) = fromInfo srcSpanInfo
getPatStartPos (PNPlusK srcSpanInfo _ _) = fromInfo srcSpanInfo
getPatStartPos (PParen srcSpanInfo _) = fromInfo srcSpanInfo
getPatStartPos (PRec srcSpanInfo _ _) = fromInfo srcSpanInfo
getPatStartPos (PRPat srcSpanInfo _) = fromInfo srcSpanInfo
getPatStartPos (PTuple srcSpanInfo _ _) = fromInfo srcSpanInfo
getPatStartPos (PUnboxedSum srcSpanInfo _ _ _) = fromInfo srcSpanInfo
getPatStartPos (PVar srcSpanInfo _) = fromInfo srcSpanInfo
getPatStartPos (PViewPat srcSpanInfo _ _) = fromInfo srcSpanInfo
getPatStartPos (PWildCard srcSpanInfo) = fromInfo srcSpanInfo
getPatStartPos (PXETag srcSpanInfo _ _ _) = fromInfo srcSpanInfo
getPatStartPos (PXPatTag srcSpanInfo _) = fromInfo srcSpanInfo
getPatStartPos (PXPcdata srcSpanInfo _) = fromInfo srcSpanInfo
getPatStartPos (PXRPats srcSpanInfo _) = fromInfo srcSpanInfo
getPatStartPos (PXTag srcSpanInfo _ _ _ _) = fromInfo srcSpanInfo
getPatStartPos (PSplice srcSpanInfo _) = fromInfo srcSpanInfo
getPatStartPos (PQuasiQuote srcSpanInfo _ _) = fromInfo srcSpanInfo

fromInfo :: SrcSpanInfo -> (Int, Int)
fromInfo srcSpanInfo = (Haskell.srcSpanStartLine span, Haskell.srcSpanStartColumn span)
  where
    span = Haskell.srcInfoSpan srcSpanInfo

-- Helper function to get SrcSpanInfo from a Type
getTypeStartPos :: Type SrcSpanInfo -> (Int, Int)
getTypeStartPos (TyFun srcSpanInfo _ _) = fromInfo srcSpanInfo
getTypeStartPos (TyVar srcSpanInfo _) = fromInfo srcSpanInfo
getTypeStartPos (TyCon srcSpanInfo _) = fromInfo srcSpanInfo
getTypeStartPos (TyTuple srcSpanInfo _ _) = fromInfo srcSpanInfo
getTypeStartPos (TyList srcSpanInfo _) = fromInfo srcSpanInfo
getTypeStartPos (TyApp srcSpanInfo _ _) = fromInfo srcSpanInfo
getTypeStartPos (TyParen srcSpanInfo _) = fromInfo srcSpanInfo
getTypeStartPos (TyInfix srcSpanInfo _ _ _) = fromInfo srcSpanInfo
getTypeStartPos (TyKind srcSpanInfo _ _) = fromInfo srcSpanInfo
getTypeStartPos _ = error "Unsupported type"

-- Function to extract starting position from various expressions
getExpStartPos :: Exp SrcSpanInfo -> Int
getExpStartPos (Var srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (OverloadedLabel srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (IPVar srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (Con srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (Lit srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (InfixApp srcSpanInfo _ _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (App srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (NegApp srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (Lambda srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (Let srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (If srcSpanInfo _ _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (MultiIf srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (Case srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (Do srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (MDo srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (Tuple srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (UnboxedSum srcSpanInfo _ _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (TupleSection srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (List srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (ParArray srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (Paren srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (LeftSection srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (RightSection srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (RecConstr srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (RecUpdate srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (EnumFrom srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (EnumFromTo srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (EnumFromThen srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (EnumFromThenTo srcSpanInfo _ _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (ParArrayFromTo srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (ParArrayFromThenTo srcSpanInfo _ _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (ListComp srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (ParComp srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (ParArrayComp srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (ExpTypeSig srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (VarQuote srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (TypQuote srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (BracketExp srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (SpliceExp srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (QuasiQuote srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (TypeApp srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (XTag srcSpanInfo _ _ _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (XETag srcSpanInfo _ _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (XPcdata srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (XExpTag srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (XChildTag srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (CorePragma srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (SCCPragma srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (GenPragma srcSpanInfo _ _ _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (Proc srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (LeftArrApp srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (RightArrApp srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (LeftArrHighApp srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (RightArrHighApp srcSpanInfo _ _) = fst (fromInfo srcSpanInfo)
getExpStartPos (ArrOp srcSpanInfo _) = fst (fromInfo srcSpanInfo)
getExpStartPos (LCase srcSpanInfo _) = fst (fromInfo srcSpanInfo)
