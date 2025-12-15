{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Rules.ExprLang.Eval
-- Description : Evaluator for the Argus expression language
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- This module provides evaluation for Argus's typed expression language.
-- It includes type checking and safe evaluation with resource limits.
module Argus.Rules.ExprLang.Eval
  ( -- * Evaluation
    eval
  , evalWithContext
  , evalToBool
  , evalToString

    -- * Result Types
  , EvalResult (..)

    -- * Type Checking
  , typeCheck
  , inferType
  , TypeCheckResult (..)

    -- * Environment Management
  , extendEnv
  , lookupVar
  , bindMetavar

    -- * Built-in Evaluation
  , evalBuiltin
  ) where

import Control.Monad (foldM)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Text.Regex.TDFA ((=~), getAllTextMatches, AllTextMatches(..))

import Argus.Rules.ExprLang.Types

--------------------------------------------------------------------------------
-- Evaluation Result
--------------------------------------------------------------------------------

-- | Result of evaluation
data EvalResult
  = EvalOk Value
  | EvalErr EvalError
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Main Evaluation Functions
--------------------------------------------------------------------------------

-- | Evaluate an expression
eval :: EvalEnv -> Expr -> EvalResult
eval env expr
  | eeRecursionDepth env > eeMaxRecursion env =
      EvalErr $ EvalError "Maximum recursion depth exceeded" Nothing []
  | otherwise = evalExpr env expr

-- | Evaluate with a match context
evalWithContext :: MatchContext -> Expr -> EvalResult
evalWithContext ctx expr =
  let env = emptyEvalEnv { eeMatchContext = ctx }
  in eval env expr

-- | Evaluate to boolean
evalToBool :: EvalEnv -> Expr -> Either EvalError Bool
evalToBool env expr = case eval env expr of
  EvalOk (VBool b) -> Right b
  EvalOk v -> Left $ EvalError
    ("Expected Bool, got " <> showValueType v) Nothing []
  EvalErr e -> Left e

-- | Evaluate to string
evalToString :: EvalEnv -> Expr -> Either EvalError Text
evalToString env expr = case eval env expr of
  EvalOk (VString s) -> Right s
  EvalOk v -> Right $ showValue v
  EvalErr e -> Left e

--------------------------------------------------------------------------------
-- Expression Evaluation
--------------------------------------------------------------------------------

-- | Evaluate an expression
evalExpr :: EvalEnv -> Expr -> EvalResult
evalExpr env = \case
  ELit v -> EvalOk v

  EVar name -> case lookupVar env name of
    Just v -> EvalOk v
    Nothing -> EvalErr $ EvalError
      ("Undefined variable: " <> name) Nothing []

  EApp f args -> evalApp env f args

  ELam params body -> EvalOk $ VFunc params body env

  ELet bindings body -> evalLet env bindings body

  EIf cond thenE elseE -> evalIf env cond thenE elseE

  EOp op e1 e2 -> evalBinOp env op e1 e2

  ENot e -> evalNot env e

  EBuiltin b -> EvalOk $ VBuiltin b

  EField e field -> evalField env e field

  EIndex e idx -> evalIndex env e idx

  EList es -> evalList env es

  ERecord fields -> evalRecord env fields

  EMatch _ -> EvalOk $ VString $ mcMatchedText $ eeMatchContext env

  EMetavar name ->
    case Map.lookup name (mcMetavars $ eeMatchContext env) of
      Just txt -> EvalOk $ VString txt
      Nothing -> EvalErr $ EvalError
        ("Undefined metavariable: $" <> name) Nothing []

-- | Evaluate function application
evalApp :: EvalEnv -> Expr -> [Expr] -> EvalResult
evalApp env f args = case eval env f of
  EvalOk (VFunc params body closure)
    | length args >= length params ->
        let env' = closure
              { eeRecursionDepth = eeRecursionDepth env + 1
              , eeMatchContext = eeMatchContext env
              }
        in case sequenceResults (map (eval env) args) of
          Nothing -> EvalErr $ EvalError "Failed to evaluate arguments" Nothing []
          Just vals ->
            let env'' = foldl' (\e (p, v) -> extendEnv e p v) env' (zip params vals)
            in eval env'' body
    | otherwise -> EvalErr $ EvalError
        ("Not enough arguments: expected " <> T.pack (show $ length params) <>
         ", got " <> T.pack (show $ length args)) Nothing []

  EvalOk (VBuiltin b) -> evalBuiltinApp env b args

  EvalOk v -> EvalErr $ EvalError
    ("Cannot apply non-function: " <> showValueType v) Nothing []

  EvalErr e -> EvalErr e
  where
    sequenceResults :: [EvalResult] -> Maybe [Value]
    sequenceResults [] = Just []
    sequenceResults (EvalOk v : rs) = (v :) <$> sequenceResults rs
    sequenceResults (EvalErr _ : _) = Nothing

-- | Evaluate let expression
evalLet :: EvalEnv -> [(Text, Expr)] -> Expr -> EvalResult
evalLet env bindings body =
  let evalBinding e (name, expr) = case eval e expr of
        EvalOk v -> Just $ extendEnv e name v
        EvalErr _ -> Nothing
  in case foldM evalBinding env bindings of
    Just env' -> eval env' body
    Nothing -> EvalErr $ EvalError "Failed to evaluate let binding" Nothing []

-- | Evaluate if expression
evalIf :: EvalEnv -> Expr -> Expr -> Expr -> EvalResult
evalIf env cond thenE elseE = case eval env cond of
  EvalOk (VBool True) -> eval env thenE
  EvalOk (VBool False) -> eval env elseE
  EvalOk v -> EvalErr $ EvalError
    ("If condition must be Bool, got " <> showValueType v) Nothing []
  EvalErr e -> EvalErr e

-- | Evaluate binary operation
evalBinOp :: EvalEnv -> BinOp -> Expr -> Expr -> EvalResult
evalBinOp env op e1 e2 = do
  let r1 = eval env e1
      r2 = eval env e2
  case (r1, r2) of
    (EvalOk v1, EvalOk v2) -> evalOp op v1 v2
    (EvalErr e, _) -> EvalErr e
    (_, EvalErr e) -> EvalErr e

-- | Evaluate a binary operator
evalOp :: BinOp -> Value -> Value -> EvalResult
evalOp op v1 v2 = case op of
  OpAdd -> numericOp (+) (+) v1 v2
  OpSub -> numericOp (-) (-) v1 v2
  OpMul -> numericOp (*) (*) v1 v2
  OpDiv -> case (v1, v2) of
    (VInt _, VInt 0) -> EvalErr $ EvalError "Division by zero" Nothing []
    (VFloat _, VFloat 0) -> EvalErr $ EvalError "Division by zero" Nothing []
    _ -> numericOp div (/) v1 v2
  OpMod -> case (v1, v2) of
    (VInt n1, VInt n2) | n2 /= 0 -> EvalOk $ VInt (n1 `mod` n2)
    (VInt _, VInt 0) -> EvalErr $ EvalError "Modulo by zero" Nothing []
    _ -> EvalErr $ EvalError "Modulo requires integers" Nothing []

  OpEq -> EvalOk $ VBool $ valueEq v1 v2
  OpNeq -> EvalOk $ VBool $ not $ valueEq v1 v2
  OpLt -> compareOp (<) v1 v2
  OpLte -> compareOp (<=) v1 v2
  OpGt -> compareOp (>) v1 v2
  OpGte -> compareOp (>=) v1 v2

  OpAnd -> case (v1, v2) of
    (VBool b1, VBool b2) -> EvalOk $ VBool (b1 && b2)
    _ -> EvalErr $ EvalError "And requires booleans" Nothing []

  OpOr -> case (v1, v2) of
    (VBool b1, VBool b2) -> EvalOk $ VBool (b1 || b2)
    _ -> EvalErr $ EvalError "Or requires booleans" Nothing []

  OpConcat -> case (v1, v2) of
    (VString s1, VString s2) -> EvalOk $ VString (s1 <> s2)
    (VList l1, VList l2) -> EvalOk $ VList (l1 ++ l2)
    _ -> EvalErr $ EvalError "Concat requires strings or lists" Nothing []

  OpCons -> case v2 of
    VList l -> EvalOk $ VList (v1 : l)
    _ -> EvalErr $ EvalError "Cons requires list as second argument" Nothing []

  OpIn -> case (v1, v2) of
    (VString s, VList l) -> EvalOk $ VBool $ VString s `elem` l
    (v, VList l) -> EvalOk $ VBool $ v `elem` l
    (VString s1, VString s2) -> EvalOk $ VBool $ s1 `T.isInfixOf` s2
    _ -> EvalErr $ EvalError "In requires list or string" Nothing []
  where
    numericOp :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Value -> Value -> EvalResult
    numericOp intOp floatOp va vb = case (va, vb) of
      (VInt n1, VInt n2) -> EvalOk $ VInt (intOp n1 n2)
      (VFloat f1, VFloat f2) -> EvalOk $ VFloat (floatOp f1 f2)
      (VInt n, VFloat f) -> EvalOk $ VFloat (floatOp (fromInteger n) f)
      (VFloat f, VInt n) -> EvalOk $ VFloat (floatOp f (fromInteger n))
      _ -> EvalErr $ EvalError "Arithmetic requires numbers" Nothing []

    compareOp :: (forall a. Ord a => a -> a -> Bool) -> Value -> Value -> EvalResult
    compareOp cmp va vb = case (va, vb) of
      (VInt n1, VInt n2) -> EvalOk $ VBool (cmp n1 n2)
      (VFloat f1, VFloat f2) -> EvalOk $ VBool (cmp f1 f2)
      (VString s1, VString s2) -> EvalOk $ VBool (cmp s1 s2)
      _ -> EvalErr $ EvalError "Comparison requires comparable types" Nothing []

-- | Evaluate logical not
evalNot :: EvalEnv -> Expr -> EvalResult
evalNot env e = case eval env e of
  EvalOk (VBool b) -> EvalOk $ VBool (not b)
  EvalOk v -> EvalErr $ EvalError
    ("Not requires Bool, got " <> showValueType v) Nothing []
  EvalErr e' -> EvalErr e'

-- | Evaluate field access
evalField :: EvalEnv -> Expr -> Text -> EvalResult
evalField env e field = case eval env e of
  EvalOk (VRecord fields) ->
    case Map.lookup field fields of
      Just v -> EvalOk v
      Nothing -> EvalErr $ EvalError ("No field: " <> field) Nothing []
  EvalOk v -> EvalErr $ EvalError
    ("Field access requires record, got " <> showValueType v) Nothing []
  EvalErr e' -> EvalErr e'

-- | Evaluate index access
evalIndex :: EvalEnv -> Expr -> Expr -> EvalResult
evalIndex env e idx = case (eval env e, eval env idx) of
  (EvalOk (VList l), EvalOk (VInt i))
    | i >= 0 && fromInteger i < length l ->
        EvalOk $ l !! fromInteger i
    | otherwise -> EvalErr $ EvalError "Index out of bounds" Nothing []
  (EvalOk (VString s), EvalOk (VInt i))
    | i >= 0 && fromInteger i < T.length s ->
        EvalOk $ VString $ T.singleton $ T.index s (fromInteger i)
    | otherwise -> EvalErr $ EvalError "Index out of bounds" Nothing []
  (EvalOk v, _) -> EvalErr $ EvalError
    ("Index access requires list or string, got " <> showValueType v) Nothing []
  (EvalErr e', _) -> EvalErr e'

-- | Evaluate list
evalList :: EvalEnv -> [Expr] -> EvalResult
evalList env es = do
  let results = map (eval env) es
  case sequence' results of
    Just vs -> EvalOk $ VList vs
    Nothing -> EvalErr $ EvalError "Failed to evaluate list element" Nothing []
  where
    sequence' :: [EvalResult] -> Maybe [Value]
    sequence' [] = Just []
    sequence' (EvalOk v : rs) = (v :) <$> sequence' rs
    sequence' (EvalErr _ : _) = Nothing

-- | Evaluate record
evalRecord :: EvalEnv -> [(Text, Expr)] -> EvalResult
evalRecord env fields = do
  let evalField' (name, expr) = case eval env expr of
        EvalOk v -> Just (name, v)
        EvalErr _ -> Nothing
  case mapM evalField' fields of
    Just pairs -> EvalOk $ VRecord $ Map.fromList pairs
    Nothing -> EvalErr $ EvalError "Failed to evaluate record field" Nothing []

--------------------------------------------------------------------------------
-- Built-in Evaluation
--------------------------------------------------------------------------------

-- | Evaluate built-in function application
evalBuiltinApp :: EvalEnv -> Builtin -> [Expr] -> EvalResult
evalBuiltinApp env builtin args = do
  let argResults = map (eval env) args
  case sequence' argResults of
    Just argVals -> evalBuiltin env builtin argVals
    Nothing -> EvalErr $ EvalError "Failed to evaluate builtin arguments" Nothing []
  where
    sequence' [] = Just []
    sequence' (EvalOk v : rs) = (v :) <$> sequence' rs
    sequence' (EvalErr _ : _) = Nothing

-- | Evaluate a built-in function
evalBuiltin :: EvalEnv -> Builtin -> [Value] -> EvalResult
evalBuiltin env builtin args = case builtin of
  -- Text operations
  BLength -> case args of
    [VString s] -> EvalOk $ VInt $ fromIntegral $ T.length s
    [VList l] -> EvalOk $ VInt $ fromIntegral $ length l
    _ -> wrongArgs "length" 1 args

  BConcat -> case args of
    [VList vs] -> case vs of
      (VString _ : _) -> EvalOk $ VString $ T.concat [s | VString s <- vs]
      (VList _ : _) -> EvalOk $ VList $ concat [l | VList l <- vs]
      [] -> EvalOk $ VList []
      _ -> EvalErr $ EvalError "concat requires list of strings or lists" Nothing []
    _ -> wrongArgs "concat" 1 args

  BToUpper -> case args of
    [VString s] -> EvalOk $ VString $ T.toUpper s
    _ -> wrongArgs "toUpper" 1 args

  BToLower -> case args of
    [VString s] -> EvalOk $ VString $ T.toLower s
    _ -> wrongArgs "toLower" 1 args

  BStrip -> case args of
    [VString s] -> EvalOk $ VString $ T.strip s
    _ -> wrongArgs "strip" 1 args

  BSplit -> case args of
    [VString delim, VString s] ->
      EvalOk $ VList $ map VString $ T.splitOn delim s
    _ -> wrongArgs "split" 2 args

  BContains -> case args of
    [VString needle, VString haystack] ->
      EvalOk $ VBool $ needle `T.isInfixOf` haystack
    _ -> wrongArgs "contains" 2 args

  BStartsWith -> case args of
    [VString prefix, VString s] ->
      EvalOk $ VBool $ prefix `T.isPrefixOf` s
    _ -> wrongArgs "startsWith" 2 args

  BEndsWith -> case args of
    [VString suffix, VString s] ->
      EvalOk $ VBool $ suffix `T.isSuffixOf` s
    _ -> wrongArgs "endsWith" 2 args

  BReplace -> case args of
    [VString old, VString new, VString s] ->
      EvalOk $ VString $ T.replace old new s
    _ -> wrongArgs "replace" 3 args

  BRegexMatch -> case args of
    [VString pat, VString s] ->
      EvalOk $ VBool $ T.unpack s =~ T.unpack pat
    _ -> wrongArgs "regexMatch" 2 args

  BRegexFind -> case args of
    [VString pat, VString s] ->
      let matches = getAllTextMatches (T.unpack s =~ T.unpack pat) :: [String]
      in EvalOk $ VList $ map (VString . T.pack) matches
    _ -> wrongArgs "regexFind" 2 args

  -- List operations
  BHead -> case args of
    [VList []] -> EvalOk VNothing
    [VList (x:_)] -> EvalOk $ VJust x
    _ -> wrongArgs "head" 1 args

  BTail -> case args of
    [VList []] -> EvalOk $ VList []
    [VList (_:xs)] -> EvalOk $ VList xs
    _ -> wrongArgs "tail" 1 args

  BInit -> case args of
    [VList []] -> EvalOk $ VList []
    [VList xs] -> EvalOk $ VList $ init xs
    _ -> wrongArgs "init" 1 args

  BLast -> case args of
    [VList []] -> EvalOk VNothing
    [VList xs] -> EvalOk $ VJust $ last xs
    _ -> wrongArgs "last" 1 args

  BTake -> case args of
    [VInt n, VList xs] -> EvalOk $ VList $ take (fromInteger n) xs
    _ -> wrongArgs "take" 2 args

  BDrop -> case args of
    [VInt n, VList xs] -> EvalOk $ VList $ drop (fromInteger n) xs
    _ -> wrongArgs "drop" 2 args

  BReverse -> case args of
    [VList xs] -> EvalOk $ VList $ reverse xs
    [VString s] -> EvalOk $ VString $ T.reverse s
    _ -> wrongArgs "reverse" 1 args

  BSort -> case args of
    [VList xs] -> EvalOk $ VList $ sortValues xs
    _ -> wrongArgs "sort" 1 args

  BNub -> case args of
    [VList xs] -> EvalOk $ VList $ nubValues xs
    _ -> wrongArgs "nub" 1 args

  BFilter -> case args of
    [VFunc params body closure, VList xs] -> case params of
      (p:_) -> do
        let filterOne v = case eval (extendEnv closure p v) body of
              EvalOk (VBool True) -> Just v
              _ -> Nothing
        EvalOk $ VList $ mapMaybe filterOne xs
      [] -> EvalErr $ EvalError "filter: function requires at least one parameter" Nothing []
    [VBuiltin b, VList xs] -> do
      let filterOne v = case evalBuiltin env b [v] of
            EvalOk (VBool True) -> Just v
            _ -> Nothing
      EvalOk $ VList $ mapMaybe filterOne xs
    _ -> wrongArgs "filter" 2 args

  BMap -> case args of
    [VFunc params body closure, VList xs] -> case params of
      (p:_) -> do
        let mapOne v = eval (extendEnv closure p v) body
            results = map mapOne xs
        case sequence' results of
          Just vs -> EvalOk $ VList vs
          Nothing -> EvalErr $ EvalError "map function failed" Nothing []
      [] -> EvalErr $ EvalError "map: function requires at least one parameter" Nothing []
    [VBuiltin b, VList xs] -> do
      let mapOne v = evalBuiltin env b [v]
          results = map mapOne xs
      case sequence' results of
        Just vs -> EvalOk $ VList vs
        Nothing -> EvalErr $ EvalError "map function failed" Nothing []
    _ -> wrongArgs "map" 2 args

  BFoldl -> case args of
    [VFunc params body closure, acc, VList xs] -> case params of
      (p0:p1:_) -> do
        let foldOne a x =
              let env' = extendEnv (extendEnv closure p0 a) p1 x
              in eval env' body
        foldM' foldOne acc xs
      _ -> EvalErr $ EvalError "foldl: function requires at least two parameters" Nothing []
    _ -> wrongArgs "foldl" 3 args

  BFoldr -> case args of
    [VFunc params body closure, acc, VList xs] -> case params of
      (p0:p1:_) -> do
        let foldOne x a =
              let env' = extendEnv (extendEnv closure p0 x) p1 a
              in eval env' body
        foldrM' foldOne acc xs
      _ -> EvalErr $ EvalError "foldr: function requires at least two parameters" Nothing []
    _ -> wrongArgs "foldr" 3 args

  BAll -> case args of
    [VFunc params body closure, VList xs] -> case params of
      (p:_) -> do
        let checkOne v = case eval (extendEnv closure p v) body of
              EvalOk (VBool b) -> Just b
              _ -> Nothing
        case mapM checkOne xs of
          Just bs -> EvalOk $ VBool $ and bs
          Nothing -> EvalErr $ EvalError "all predicate failed" Nothing []
      [] -> EvalErr $ EvalError "all: predicate function requires at least one parameter" Nothing []
    _ -> wrongArgs "all" 2 args

  BAny -> case args of
    [VFunc params body closure, VList xs] -> case params of
      (p:_) -> do
        let checkOne v = case eval (extendEnv closure p v) body of
              EvalOk (VBool b) -> Just b
              _ -> Nothing
        case mapM checkOne xs of
          Just bs -> EvalOk $ VBool $ or bs
          Nothing -> EvalErr $ EvalError "any predicate failed" Nothing []
      [] -> EvalErr $ EvalError "any: predicate function requires at least one parameter" Nothing []
    _ -> wrongArgs "any" 2 args

  BElem -> case args of
    [v, VList xs] -> EvalOk $ VBool $ v `elem` xs
    _ -> wrongArgs "elem" 2 args

  BNotElem -> case args of
    [v, VList xs] -> EvalOk $ VBool $ v `notElem` xs
    _ -> wrongArgs "notElem" 2 args

  BZip -> case args of
    [VList xs, VList ys] ->
      EvalOk $ VList $ zipWith (\x y -> VRecord $ Map.fromList [("fst", x), ("snd", y)]) xs ys
    _ -> wrongArgs "zip" 2 args

  BZipWith -> case args of
    [VFunc params body closure, VList xs, VList ys] -> do
      let zipOne x y =
            let env' = extendEnv (extendEnv closure (params !! 0) x) (params !! 1) y
            in eval env' body
          results = zipWith zipOne xs ys
      case sequence' results of
        Just vs -> EvalOk $ VList vs
        Nothing -> EvalErr $ EvalError "zipWith function failed" Nothing []
    _ -> wrongArgs "zipWith" 3 args

  BFlatten -> case args of
    [VList xs] -> EvalOk $ VList $ concat [l | VList l <- xs]
    _ -> wrongArgs "flatten" 1 args

  -- Set operations
  BUnion -> case args of
    [VSet s1, VSet s2] -> EvalOk $ VSet $ s1 `Set.union` s2
    _ -> wrongArgs "union" 2 args

  BIntersect -> case args of
    [VSet s1, VSet s2] -> EvalOk $ VSet $ s1 `Set.intersection` s2
    _ -> wrongArgs "intersect" 2 args

  BDifference -> case args of
    [VSet s1, VSet s2] -> EvalOk $ VSet $ s1 `Set.difference` s2
    _ -> wrongArgs "difference" 2 args

  BIsSubset -> case args of
    [VSet s1, VSet s2] -> EvalOk $ VBool $ s1 `Set.isSubsetOf` s2
    _ -> wrongArgs "isSubset" 2 args

  BFromList -> case args of
    [VList xs] -> EvalOk $ VSet $ Set.fromList [s | VString s <- xs]
    _ -> wrongArgs "fromList" 1 args

  BToList -> case args of
    [VSet s] -> EvalOk $ VList $ map VString $ Set.toList s
    _ -> wrongArgs "toList" 1 args

  -- Numeric operations
  BAbs -> case args of
    [VInt n] -> EvalOk $ VInt $ abs n
    [VFloat f] -> EvalOk $ VFloat $ abs f
    _ -> wrongArgs "abs" 1 args

  BMax -> case args of
    [VInt a, VInt b] -> EvalOk $ VInt $ max a b
    [VFloat a, VFloat b] -> EvalOk $ VFloat $ max a b
    _ -> wrongArgs "max" 2 args

  BMin -> case args of
    [VInt a, VInt b] -> EvalOk $ VInt $ min a b
    [VFloat a, VFloat b] -> EvalOk $ VFloat $ min a b
    _ -> wrongArgs "min" 2 args

  BSum -> case args of
    [VList xs] -> EvalOk $ VInt $ sum [n | VInt n <- xs]
    _ -> wrongArgs "sum" 1 args

  BProduct -> case args of
    [VList xs] -> EvalOk $ VInt $ product [n | VInt n <- xs]
    _ -> wrongArgs "product" 1 args

  BCeiling -> case args of
    [VFloat f] -> EvalOk $ VInt $ ceiling f
    _ -> wrongArgs "ceiling" 1 args

  BFloor -> case args of
    [VFloat f] -> EvalOk $ VInt $ floor f
    _ -> wrongArgs "floor" 1 args

  BRound -> case args of
    [VFloat f] -> EvalOk $ VInt $ round f
    _ -> wrongArgs "round" 1 args

  -- Context queries (0-arity)
  BModuleName -> EvalOk $ VString $ mcModuleName $ eeMatchContext env
  BFilePath -> EvalOk $ VString $ mcFilePath $ eeMatchContext env
  BLineNumber -> EvalOk $ VInt $ fromIntegral $ mcLineNumber $ eeMatchContext env
  BColumnNumber -> EvalOk $ VInt $ fromIntegral $ mcColumnNumber $ eeMatchContext env
  BMatchedText -> EvalOk $ VString $ mcMatchedText $ eeMatchContext env
  BImports -> EvalOk $ VList $ map VString $ mcImports $ eeMatchContext env
  BExports -> EvalOk $ VList $ map VString $ mcExports $ eeMatchContext env
  BPragmas -> EvalOk $ VList $ map VString $ mcPragmas $ eeMatchContext env

  BMetavar -> case args of
    [VString name] ->
      case Map.lookup name (mcMetavars $ eeMatchContext env) of
        Just txt -> EvalOk $ VJust $ VString txt
        Nothing -> EvalOk VNothing
    _ -> wrongArgs "metavar" 1 args

  BHasImport -> case args of
    [VString modName] ->
      EvalOk $ VBool $ modName `elem` mcImports (eeMatchContext env)
    _ -> wrongArgs "hasImport" 1 args

  BHasPragma -> case args of
    [VString pragma] ->
      EvalOk $ VBool $ pragma `elem` mcPragmas (eeMatchContext env)
    _ -> wrongArgs "hasPragma" 1 args

  -- AST queries
  BComplexity -> case args of
    [VString txt] -> EvalOk $ VInt $ fromIntegral $ estimateComplexity txt
    _ -> wrongArgs "complexity" 1 args

  BFreeVars -> case args of
    [VString txt] -> EvalOk $ VSet $ extractFreeVars txt
    _ -> wrongArgs "freeVars" 1 args

  BBindings -> case args of
    [VString txt] -> EvalOk $ VSet $ extractBindings txt
    _ -> wrongArgs "bindings" 1 args

  BIsLiteral -> case args of
    [VString txt] -> EvalOk $ VBool $ isLiteralText txt
    _ -> wrongArgs "isLiteral" 1 args

  BIsVariable -> case args of
    [VString txt] -> EvalOk $ VBool $ isVariableText txt
    _ -> wrongArgs "isVariable" 1 args

  BIsApplication -> case args of
    [VString txt] -> EvalOk $ VBool $ isApplicationText txt
    _ -> wrongArgs "isApplication" 1 args

  BIsLambda -> case args of
    [VString txt] -> EvalOk $ VBool $ isLambdaText txt
    _ -> wrongArgs "isLambda" 1 args

  BIsAtomic -> case args of
    [VString txt] -> EvalOk $ VBool $ isAtomicText txt
    _ -> wrongArgs "isAtomic" 1 args

  BArity -> case args of
    [VString txt] -> EvalOk $ VInt $ fromIntegral $ estimateArity txt
    _ -> wrongArgs "arity" 1 args

  -- Type queries
  BTypeOf -> case args of
    [VString _] -> EvalOk VNothing  -- Would need HIE info
    _ -> wrongArgs "typeOf" 1 args

  BKindOf -> case args of
    [VString _] -> EvalOk VNothing
    _ -> wrongArgs "kindOf" 1 args

  BHasType -> case args of
    [VString _, VString _] -> EvalOk $ VBool False  -- Would need HIE
    _ -> wrongArgs "hasType" 2 args

  BHasTypeClass -> case args of
    [VString _, VString _] -> EvalOk $ VBool False  -- Would need HIE
    _ -> wrongArgs "hasTypeClass" 2 args

  BIsFunctor -> case args of
    [VString _] -> EvalOk $ VBool False
    _ -> wrongArgs "isFunctor" 1 args

  BIsMonad -> case args of
    [VString _] -> EvalOk $ VBool False
    _ -> wrongArgs "isMonad" 1 args

  BIsApplicative -> case args of
    [VString _] -> EvalOk $ VBool False
    _ -> wrongArgs "isApplicative" 1 args

  -- Purity queries
  BIsPure -> case args of
    [VString txt] -> EvalOk $ VBool $ isPureText txt
    _ -> wrongArgs "isPure" 1 args

  BHasEffect -> case args of
    [VString txt] -> EvalOk $ VBool $ hasEffectText txt
    _ -> wrongArgs "hasEffect" 1 args

  BEffectType -> case args of
    [VString _] -> EvalOk VNothing
    _ -> wrongArgs "effectType" 1 args

  -- Utilities
  BIf -> case args of
    [VBool cond, thenV, elseV] -> EvalOk $ if cond then thenV else elseV
    _ -> wrongArgs "if" 3 args

  BNot -> case args of
    [VBool b] -> EvalOk $ VBool $ not b
    _ -> wrongArgs "not" 1 args

  BError -> case args of
    [VString msg] -> EvalErr $ EvalError msg Nothing []
    _ -> wrongArgs "error" 1 args

  BTrace -> case args of
    [VString _, v] -> EvalOk v  -- In production, just return the value
    _ -> wrongArgs "trace" 2 args

  BShow -> case args of
    [v] -> EvalOk $ VString $ showValue v
    _ -> wrongArgs "show" 1 args

  BRead -> case args of
    [VString _] -> EvalOk VNothing  -- Simplified
    _ -> wrongArgs "read" 1 args
  where
    wrongArgs :: Text -> Int -> [Value] -> EvalResult
    wrongArgs name expected actual = EvalErr $ EvalError
      (name <> " expected " <> T.pack (show expected) <> " arguments, got " <>
       T.pack (show $ length actual))
      Nothing []

    sequence' :: [EvalResult] -> Maybe [Value]
    sequence' [] = Just []
    sequence' (EvalOk v : rs) = (v :) <$> sequence' rs
    sequence' (EvalErr _ : _) = Nothing

    foldM' :: (Value -> Value -> EvalResult) -> Value -> [Value] -> EvalResult
    foldM' _ acc [] = EvalOk acc
    foldM' f acc (x:xs) = case f acc x of
      EvalOk v -> foldM' f v xs
      EvalErr e -> EvalErr e

    foldrM' :: (Value -> Value -> EvalResult) -> Value -> [Value] -> EvalResult
    foldrM' _ acc [] = EvalOk acc
    foldrM' f acc (x:xs) = case foldrM' f acc xs of
      EvalOk v -> f x v
      EvalErr e -> EvalErr e

--------------------------------------------------------------------------------
-- Type Checking
--------------------------------------------------------------------------------

-- | Type check result
data TypeCheckResult
  = TypeOk ExprType
  | TypeErr TypeError
  deriving stock (Eq, Show)

-- | Type check an expression
typeCheck :: TypeEnv -> Expr -> TypeCheckResult
typeCheck env = inferType env

-- | Infer the type of an expression
inferType :: TypeEnv -> Expr -> TypeCheckResult
inferType env = \case
  ELit v -> TypeOk $ valueType v

  EVar name -> case Map.lookup name env of
    Just (TScheme _ t) -> TypeOk t
    Nothing -> TypeErr $ TypeError ("Undefined: " <> name) Nothing Nothing Nothing

  EApp f args -> case inferType env f of
    TypeOk (TFunc argTypes retType)
      | length args == length argTypes -> TypeOk retType
      | otherwise -> TypeErr $ TypeError "Wrong number of arguments" Nothing Nothing Nothing
    TypeOk TAny -> TypeOk TAny
    TypeOk t -> TypeErr $ TypeError ("Not a function: " <> T.pack (show t)) Nothing Nothing Nothing
    TypeErr e -> TypeErr e

  ELam params body -> case inferType env body of
    TypeOk t -> TypeOk $ TFunc (replicate (length params) TAny) t
    TypeErr e -> TypeErr e

  ELet _bindings body -> inferType env body

  EIf cond thenE elseE -> case (inferType env cond, inferType env thenE, inferType env elseE) of
    (TypeOk TBool, TypeOk t1, TypeOk t2)
      | t1 == t2 -> TypeOk t1
      | otherwise -> TypeOk TAny
    (TypeOk TBool, TypeOk t, TypeErr _) -> TypeOk t
    (TypeOk TBool, TypeErr _, TypeOk t) -> TypeOk t
    (TypeErr e, _, _) -> TypeErr e
    (TypeOk t, _, _) -> TypeErr $ TypeError ("If condition must be Bool, got " <> T.pack (show t)) Nothing Nothing Nothing

  EOp op _ _ -> TypeOk $ opResultType op

  ENot _ -> TypeOk TBool

  EBuiltin b -> TypeOk $ builtinType b

  EField _ _ -> TypeOk TAny

  EIndex e _ -> case inferType env e of
    TypeOk (TList t) -> TypeOk t
    TypeOk TString -> TypeOk TString
    _ -> TypeOk TAny

  EList [] -> TypeOk $ TList TAny
  EList (e:_) -> case inferType env e of
    TypeOk t -> TypeOk $ TList t
    TypeErr err -> TypeErr err

  ERecord fields -> TypeOk $ TRecord $ Map.fromList [(n, TAny) | (n, _) <- fields]

  EMatch _ -> TypeOk TString

  EMetavar _ -> TypeOk TString
  where
    opResultType :: BinOp -> ExprType
    opResultType = \case
      OpAdd -> TInt
      OpSub -> TInt
      OpMul -> TInt
      OpDiv -> TInt
      OpMod -> TInt
      OpEq -> TBool
      OpNeq -> TBool
      OpLt -> TBool
      OpLte -> TBool
      OpGt -> TBool
      OpGte -> TBool
      OpAnd -> TBool
      OpOr -> TBool
      OpConcat -> TAny
      OpCons -> TAny
      OpIn -> TBool

    builtinType :: Builtin -> ExprType
    builtinType b =
      let TScheme _ t = biType $ builtinInfo b
      in t

--------------------------------------------------------------------------------
-- Environment Management
--------------------------------------------------------------------------------

-- | Extend environment with a binding
extendEnv :: EvalEnv -> Text -> Value -> EvalEnv
extendEnv env name val = env
  { eeBindings = Map.insert name val (eeBindings env)
  }

-- | Look up a variable
lookupVar :: EvalEnv -> Text -> Maybe Value
lookupVar env name = Map.lookup name (eeBindings env)

-- | Bind a metavariable
bindMetavar :: EvalEnv -> Text -> Text -> EvalEnv
bindMetavar env name txt = env
  { eeMatchContext = (eeMatchContext env)
      { mcMetavars = Map.insert name txt (mcMetavars $ eeMatchContext env)
      }
  }

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Show value type
showValueType :: Value -> Text
showValueType = \case
  VBool _ -> "Bool"
  VInt _ -> "Int"
  VFloat _ -> "Float"
  VString _ -> "String"
  VList _ -> "List"
  VRecord _ -> "Record"
  VSet _ -> "Set"
  VNothing -> "Nothing"
  VJust _ -> "Just"
  VFunc {} -> "Function"
  VBuiltin _ -> "Builtin"

-- | Show value
showValue :: Value -> Text
showValue = \case
  VBool b -> if b then "true" else "false"
  VInt n -> T.pack $ show n
  VFloat f -> T.pack $ show f
  VString s -> "\"" <> s <> "\""
  VList vs -> "[" <> T.intercalate ", " (map showValue vs) <> "]"
  VRecord m -> "{" <> T.intercalate ", " [k <> ": " <> showValue v | (k, v) <- Map.toList m] <> "}"
  VSet s -> "Set(" <> T.intercalate ", " (map (\x -> "\"" <> x <> "\"") $ Set.toList s) <> ")"
  VNothing -> "Nothing"
  VJust v -> "Just(" <> showValue v <> ")"
  VFunc params _ _ -> "\\(" <> T.intercalate ", " params <> ") -> ..."
  VBuiltin b -> T.pack $ show b

-- | Value equality
valueEq :: Value -> Value -> Bool
valueEq (VBool a) (VBool b) = a == b
valueEq (VInt a) (VInt b) = a == b
valueEq (VFloat a) (VFloat b) = abs (a - b) < 1e-10
valueEq (VString a) (VString b) = a == b
valueEq (VList a) (VList b) = length a == length b && and (zipWith valueEq a b)
valueEq (VSet a) (VSet b) = a == b
valueEq VNothing VNothing = True
valueEq (VJust a) (VJust b) = valueEq a b
valueEq _ _ = False

-- | Get type of value
valueType :: Value -> ExprType
valueType = \case
  VBool _ -> TBool
  VInt _ -> TInt
  VFloat _ -> TFloat
  VString _ -> TString
  VList [] -> TList TAny
  VList (v:_) -> TList $ valueType v
  VRecord m -> TRecord $ Map.map valueType m
  VSet _ -> TSet TString
  VNothing -> TMaybe TAny
  VJust v -> TMaybe $ valueType v
  VFunc params _ _ -> TFunc (replicate (length params) TAny) TAny
  VBuiltin b -> biType (builtinInfo b) & \(TScheme _ t) -> t
  where
    (&) = flip ($)

-- | Sort values
sortValues :: [Value] -> [Value]
sortValues = sortBy compareValue
  where
    sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    sortBy cmp = foldr insert []
      where
        insert x [] = [x]
        insert x (y:ys) = case cmp x y of
          GT -> y : insert x ys
          _ -> x : y : ys

    compareValue (VInt a) (VInt b) = compare a b
    compareValue (VFloat a) (VFloat b) = compare a b
    compareValue (VString a) (VString b) = compare a b
    compareValue (VBool a) (VBool b) = compare a b
    compareValue a b = compare (showValue a) (showValue b)

-- | Nub values
nubValues :: [Value] -> [Value]
nubValues = go []
  where
    go _ [] = []
    go seen (x:xs)
      | any (valueEq x) seen = go seen xs
      | otherwise = x : go (x:seen) xs

-- | Estimate complexity of text
estimateComplexity :: Text -> Int
estimateComplexity txt =
  let ops = length $ filter (`elem` ("()[]{}\\-><=|&" :: String)) $ T.unpack txt
      keywords = length $ filter (`T.isInfixOf` txt) ["if", "case", "let", "where", "do"]
  in ops + keywords * 2

-- | Extract free variables
extractFreeVars :: Text -> Set Text
extractFreeVars txt =
  let words' = T.words txt
      isVar w = case T.uncons w of
        Just (c, _) -> c >= 'a' && c <= 'z'
        Nothing -> False
  in Set.fromList $ filter isVar words'

-- | Extract bindings
extractBindings :: Text -> Set Text
extractBindings txt =
  let lines' = T.lines txt
      findBinding line = case T.breakOn "=" (T.strip line) of
        (name, rest)
          | not (T.null rest) && T.all (\c -> c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' || c == ' ') name ->
              case T.words name of
                (w:_) -> Just $ T.strip w
                [] -> Nothing
          | otherwise -> Nothing
  in Set.fromList $ mapMaybe findBinding lines'

-- | Check if text is a literal
isLiteralText :: Text -> Bool
isLiteralText txt =
  let stripped = T.strip txt
  in T.all (\c -> c >= '0' && c <= '9') stripped ||
     isQuotedWith '"' stripped ||
     isQuotedWith '\'' stripped ||
     stripped == "True" || stripped == "False"
  where
    isQuotedWith :: Char -> Text -> Bool
    isQuotedWith q t = case T.uncons t of
      Just (c, rest) | c == q -> case T.unsnoc rest of
        Just (_, c') -> c' == q
        Nothing -> False
      _ -> False

-- | Check if text is a variable
isVariableText :: Text -> Bool
isVariableText txt =
  let stripped = T.strip txt
  in case T.uncons stripped of
       Just (c, rest) -> (c >= 'a' && c <= 'z' || c == '_') &&
                        T.all (\x -> x >= 'a' && x <= 'z' || x >= 'A' && x <= 'Z' ||
                                    x >= '0' && x <= '9' || x == '_' || x == '\'') rest
       Nothing -> False

-- | Check if text is a function application
isApplicationText :: Text -> Bool
isApplicationText txt = " " `T.isInfixOf` T.strip txt && not (isLambdaText txt)

-- | Check if text is a lambda
isLambdaText :: Text -> Bool
isLambdaText txt = "\\" `T.isPrefixOf` T.strip txt || "λ" `T.isPrefixOf` T.strip txt

-- | Check if text is atomic
isAtomicText :: Text -> Bool
isAtomicText txt = isLiteralText txt || isVariableText txt

-- | Estimate arity of function
estimateArity :: Text -> Int
estimateArity txt =
  let stripped = T.strip txt
  in if isLambdaText stripped
     then countArrows stripped
     else 0
  where
    countArrows t = T.count "->" t

-- | Check if text is pure (no IO indicators)
isPureText :: Text -> Bool
isPureText txt =
  let ioIndicators = ["IO", "readFile", "writeFile", "putStrLn", "getLine", "print", "unsafePerformIO"]
  in not $ any (`T.isInfixOf` txt) ioIndicators

-- | Check if text has effects
hasEffectText :: Text -> Bool
hasEffectText = not . isPureText
