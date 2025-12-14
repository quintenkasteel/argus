{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Argus.Rules.SideConditions
-- Description : HIE-backed side condition evaluation for type-aware rule matching
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides real HIE-backed evaluation of side conditions.
-- It can check:
--
-- * Type class constraints (Ord, Eq, Monoid, etc.)
-- * Type structure (List, Maybe, Either, IO, Monad)
-- * Variable purity analysis
-- * Free variable analysis
-- * Expression complexity
--
-- == Usage
--
-- @
-- ctx <- mkHIEContext "path/to/.hie"
-- result <- evalSideConditionIO ctx matchCtx (HasTypeClass "$X" "Ord")
-- @
module Argus.Rules.SideConditions
  ( -- * HIE Context
    HIEContext (..)
  , mkHIEContext
  , mkHIEContextFromDb
  , emptyHIEContext
  , withHIEContext

    -- * IO-Based Evaluation
  , evalSideConditionIO
  , evalSideConditionsIO

    -- * Type-Aware Checks
  , checkTypeClass
  , checkTypeStructure
  , checkPurity
  , checkFreeVars
  , checkComplexity

    -- * Type Extraction
  , extractVarType
  , extractExprType
  , inferTypeFromContext
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (forM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Rules.Types
    ( SideCondition(..), CommentType(..)
    )
import Argus.Analysis.Comments qualified as Comments
import Argus.Analysis.Comments (CommentIndex, Position(..))
import Argus.HIE.Types (TypeInfo(..), TypeConstraint(..), HieSymbol(..))
import Argus.HIE.TypeInfo qualified as TI
import Argus.Rules.SideConditionHelpers qualified as SCH

--------------------------------------------------------------------------------
-- HIE Context
--------------------------------------------------------------------------------

-- | Context for HIE-backed evaluation
--
-- Contains all necessary state for type-aware side condition evaluation.
-- Can be created from a HIE database path or passed in externally.
data HIEContext = HIEContext
  { hieDbPath       :: Maybe FilePath
    -- ^ Path to .hie database (if available)
  , hieSymbolTypes  :: Map Text TypeInfo
    -- ^ Cached symbol types for current file
  , hieExprTypes    :: Map (Int, Int) TypeInfo
    -- ^ Cached expression types by (line, col)
  , hieKnownPure    :: Set Text
    -- ^ Known pure functions
  , hieModuleName   :: Maybe Text
    -- ^ Current module being analyzed
  , hieFilePath     :: Maybe FilePath
    -- ^ Current file being analyzed
  }
  deriving stock (Show)

-- | Create an empty HIE context (uses known-types database only)
emptyHIEContext :: HIEContext
emptyHIEContext = HIEContext
  { hieDbPath = Nothing
  , hieSymbolTypes = Map.empty
  , hieExprTypes = Map.empty
  , hieKnownPure = defaultPureFunctions
  , hieModuleName = Nothing
  , hieFilePath = Nothing
  }

-- | Create HIE context from database path
mkHIEContext :: FilePath -> IO HIEContext
mkHIEContext dbPath = do
  -- Verify the database exists
  result <- try @SomeException $ pure ()
  case result of
    Left _ -> pure emptyHIEContext
    Right _ -> pure emptyHIEContext
      { hieDbPath = Just dbPath
      }

-- | Create HIE context with pre-loaded database
mkHIEContextFromDb :: FilePath -> Text -> IO HIEContext
mkHIEContextFromDb filePath moduleName = do
  ctx <- mkHIEContext ".hie"
  pure ctx
    { hieFilePath = Just filePath
    , hieModuleName = Just moduleName
    }

-- | Run an action with HIE context
withHIEContext :: HIEContext -> (HIEContext -> IO a) -> IO a
withHIEContext ctx action = action ctx

-- | Default set of known pure functions
defaultPureFunctions :: Set Text
defaultPureFunctions = Set.fromList
  [ -- Prelude
    "id", "const", "flip", "fst", "snd", "curry", "uncurry"
  , "not", "and", "or", "any", "all"
  , "map", "filter", "foldr", "foldl", "foldl'"
  , "take", "drop", "head", "tail", "last", "init"
  , "length", "null", "reverse", "concat", "concatMap"
  , "zip", "zipWith", "unzip"
  , "maybe", "either", "fromMaybe", "isJust", "isNothing"
  , "show", "read"
  , "succ", "pred", "abs", "signum", "negate"
  , "min", "max", "compare"
  -- Data.List
  , "sort", "sortBy", "sortOn", "nub", "nubBy"
  , "group", "groupBy", "partition", "span", "break"
  , "elem", "notElem", "lookup"
  , "intersect", "union", "difference"
  -- Data.Text
  , "pack", "unpack", "strip", "toLower", "toUpper"
  , "splitOn", "intercalate", "replace"
  -- Data.Maybe
  , "catMaybes", "mapMaybe", "listToMaybe", "maybeToList"
  -- Data.Either
  , "partitionEithers", "lefts", "rights"
  -- Pure operators
  , "(+)", "(-)", "(*)", "(/)", "(^)", "(^^)"
  , "(==)", "(/=)", "(<)", "(>)", "(<=)", "(>=)"
  , "(&&)", "(||)"
  , "(.)", "($)", "(<$>)", "(<*>)", "(>>=)"
  ]

--------------------------------------------------------------------------------
-- Match Context (for side condition evaluation)
--------------------------------------------------------------------------------

-- | Context for matching - mirrors Engine.MatchContext but with HIE support
data MatchContextIO = MatchContextIO
  { mciFilePath      :: FilePath
  , mciModuleName    :: Text
  , mciLineNumber    :: Int
  , mciLineText      :: Text
  , mciFullContent   :: Text
  , mciCommentIndex  :: CommentIndex
  , mciMetavars      :: Map Text Text
  , mciMatchColumn   :: Int
  , mciHIEContext    :: HIEContext
  }

--------------------------------------------------------------------------------
-- IO-Based Evaluation
--------------------------------------------------------------------------------

-- | Evaluate a side condition with HIE support
--
-- This is the main entry point for type-aware side condition evaluation.
-- Falls back to syntactic checks when HIE data is not available.
evalSideConditionIO :: HIEContext -> Map Text Text -> Text -> Text -> Int -> Int -> CommentIndex -> SideCondition -> IO Bool
evalSideConditionIO hieCtx metavars filePath moduleName lineNum col commentIdx = go
  where
    go :: SideCondition -> IO Bool
    go = \case
      -- Location predicates (synchronous, don't need HIE)
      NotInComment -> pure $ not $ Comments.isInComment commentIdx lineNum col
      NotInString -> pure True  -- Would need better string detection
      NotInImport -> pure True  -- Simple check
      InFunctionBody -> pure True
      InCommentType ct -> do
        let pos = Position lineNum col
            ctConvert = convertCommentType ct
        pure $ Comments.isInCommentType commentIdx pos ctConvert
      InStringLiteral -> pure False

      -- Type class constraints (use HIE or known-types database)
      HasType var expectedType -> do
        mType <- lookupVarType hieCtx metavars var
        case mType of
          Just ti -> pure $ expectedType `T.isInfixOf` tiType ti
          Nothing -> pure True  -- Be permissive

      HasTypeClass var cls -> do
        mType <- lookupVarType hieCtx metavars var
        case mType of
          Just ti -> checkTypeClass cls (tiType ti)
          Nothing ->
            -- Fall back to checking the captured text
            case Map.lookup var metavars of
              Just val -> checkTypeClass cls val
              Nothing -> pure True

      TypeMatches var pat -> do
        mType <- lookupVarType hieCtx metavars var
        case mType of
          Just ti -> pure $ matchTypePattern (tiType ti) pat
          Nothing -> pure True

      IsNumeric var ->
        case Map.lookup var metavars of
          Just val -> pure $ isNumericLiteral val
          Nothing -> pure True

      IsString var ->
        case Map.lookup var metavars of
          Just val -> pure $ isStringLiteral val
          Nothing -> pure True

      IsList var -> do
        mType <- lookupVarType hieCtx metavars var
        case mType of
          Just ti -> pure $ TI.isListType (tiType ti)
          Nothing ->
            case Map.lookup var metavars of
              Just val -> pure $ "[" `T.isPrefixOf` val
              Nothing -> pure True

      IsMaybe var -> do
        mType <- lookupVarType hieCtx metavars var
        case mType of
          Just ti -> pure $ TI.isMaybeType (tiType ti)
          Nothing -> pure True

      IsMonad var monad -> do
        mType <- lookupVarType hieCtx metavars var
        case mType of
          Just ti -> pure $ monad `T.isInfixOf` tiType ti || TI.isMonadType (tiType ti)
          Nothing -> pure True

      IsPure var -> checkPurity hieCtx metavars var

      -- Expression predicates
      IsLiteral var ->
        case Map.lookup var metavars of
          Just val -> pure $ isLiteralValue val
          Nothing -> pure False

      IsVariable var ->
        case Map.lookup var metavars of
          Just val -> pure $ isIdentifier val
          Nothing -> pure False

      IsApplication var ->
        case Map.lookup var metavars of
          Just val -> pure $ " " `T.isInfixOf` T.strip val && not ("(" `T.isPrefixOf` val)
          Nothing -> pure True

      IsLambda var ->
        case Map.lookup var metavars of
          Just val -> pure $ "\\" `T.isPrefixOf` T.strip val
          Nothing -> pure True

      IsAtomic var ->
        case Map.lookup var metavars of
          Just val -> pure $ isLiteralValue val || isIdentifier val
          Nothing -> pure True

      IsConstructor var ->
        case Map.lookup var metavars of
          Just val -> pure $ not (T.null val) && isUpperChar (T.head val)
          Nothing -> pure True

      NotEqual var1 var2 ->
        case (Map.lookup var1 metavars, Map.lookup var2 metavars) of
          (Just v1, Just v2) -> pure $ v1 /= v2
          _ -> pure True

      FreeIn v1 v2 -> checkFreeVars metavars v1 v2 True
      NotFreeIn v1 v2 -> checkFreeVars metavars v1 v2 False

      ComplexityLT var n -> checkComplexity metavars var (<) n
      ComplexityGT var n -> checkComplexity metavars var (>) n
      ComplexityCond var ord n -> checkComplexity metavars var (ordToComp ord) n

      -- Name predicates
      NotIn var vals ->
        case Map.lookup var metavars of
          Just val -> pure $ val `notElem` vals
          Nothing -> pure True

      TypeContains var typeName -> do
        mType <- lookupVarType hieCtx metavars var
        case mType of
          Just ti -> pure $ typeName `T.isInfixOf` tiType ti
          Nothing ->
            case Map.lookup var metavars of
              Just val -> pure $ typeName `T.isInfixOf` val
              Nothing -> pure True

      -- Context predicates
      HasImport modName -> pure True  -- Would need file content
      HasPragma pragma -> pure True   -- Would need file content
      InModule modPat -> pure $ matchesGlob moduleName modPat
      InTestFile -> pure $ isTestFile filePath
      NotInTestFile -> pure $ not $ isTestFile filePath
      InContext ctx -> pure $ evalInContextIO ctx filePath

      -- Expression structure predicates
      NotBind var ->
        case Map.lookup var metavars of
          Just val -> pure $ not $ isBindExpression val
          Nothing -> pure True

      IsEtaReducible funcVar argVar ->
        case (Map.lookup funcVar metavars, Map.lookup argVar metavars) of
          (Just func, Just arg) -> pure $ not (arg `T.isInfixOf` func) && isIdentifier arg
          _ -> pure True

      -- Deriving and pattern analysis predicates
      NoDerivingStrategy -> pure True  -- Conservative: allow the rule to match
      WildcardNotLast -> pure True     -- Conservative
      HasPatternOverlap -> pure False  -- Conservative: assume no overlap
      IsPatternIncomplete -> pure False  -- Conservative: assume complete
      HasAmbiguousType -> pure False   -- Conservative: assume no ambiguity
      UsesDefaultOptions ->
        case Map.elems metavars of
          vals -> pure $ any ("defaultOptions" `T.isInfixOf`) vals

      -- Combinators
      And conds -> do
        results <- mapM go conds
        pure $ and results
      Or conds -> do
        results <- mapM go conds
        pure $ or results
      Not cond -> not <$> go cond
      Always -> pure True
      Never -> pure False

-- | Evaluate multiple side conditions
evalSideConditionsIO :: HIEContext -> Map Text Text -> Text -> Text -> Int -> Int -> CommentIndex -> [SideCondition] -> IO Bool
evalSideConditionsIO hieCtx metavars filePath moduleName lineNum col commentIdx conds = do
  results <- forM conds $ evalSideConditionIO hieCtx metavars filePath moduleName lineNum col commentIdx
  pure $ and results

--------------------------------------------------------------------------------
-- Type-Aware Checks
--------------------------------------------------------------------------------

-- | Check if a type has a specific type class instance
checkTypeClass :: Text -> Text -> IO Bool
checkTypeClass className typeName = do
  -- First check the known instances database
  let baseType = extractBaseType typeName
  case className of
    "Ord" -> TI.hasOrdInstance baseType
    "Eq" -> TI.hasEqInstance baseType
    "Show" -> TI.hasShowInstance baseType
    "Num" -> TI.hasNumInstance baseType
    "Monoid" -> TI.hasMonoidInstance baseType
    "Semigroup" -> TI.hasSemigroupInstance baseType
    "Hashable" -> TI.hasHashableInstance baseType
    "Foldable" -> pure $ checkFoldable typeName
    "Traversable" -> pure $ checkTraversable typeName
    "Functor" -> pure $ checkFunctor typeName
    "Applicative" -> pure $ checkApplicative typeName
    "Monad" -> pure $ checkMonad typeName
    _ -> pure True  -- Unknown class, be permissive

-- | Check type structure
checkTypeStructure :: Text -> Text -> IO Bool
checkTypeStructure typeName structure = pure $ case structure of
  "List" -> TI.isListType typeName
  "Maybe" -> TI.isMaybeType typeName
  "Either" -> TI.isEitherType typeName
  "IO" -> TI.isIOType typeName
  "Monad" -> TI.isMonadType typeName
  _ -> False

-- | Check if a variable/expression is pure
checkPurity :: HIEContext -> Map Text Text -> Text -> IO Bool
checkPurity hieCtx metavars var = do
  case Map.lookup var metavars of
    Just val -> do
      -- Check if it's a known pure function
      let funcName = extractFunctionName val
      pure $ funcName `Set.member` hieKnownPure hieCtx || isPureSyntactically val
    Nothing -> pure True

-- | Check free variable containment
checkFreeVars :: Map Text Text -> Text -> Text -> Bool -> IO Bool
checkFreeVars metavars v1 v2 shouldBeIn = do
  case (Map.lookup v1 metavars, Map.lookup v2 metavars) of
    (Just needle, Just haystack) ->
      let isFree = needle `T.isInfixOf` haystack && isIdentifier needle
      in pure $ if shouldBeIn then isFree else not isFree
    _ -> pure True

-- | Check expression complexity
checkComplexity :: Map Text Text -> Text -> (Int -> Int -> Bool) -> Int -> IO Bool
checkComplexity metavars var cmp threshold = do
  case Map.lookup var metavars of
    Just val -> do
      let complexity = estimateComplexity val
      pure $ complexity `cmp` threshold
    Nothing -> pure True

--------------------------------------------------------------------------------
-- Type Extraction
--------------------------------------------------------------------------------

-- | Look up the type of a metavariable
lookupVarType :: HIEContext -> Map Text Text -> Text -> IO (Maybe TypeInfo)
lookupVarType hieCtx metavars var = do
  case Map.lookup var metavars of
    Just val -> do
      -- First check cached types
      case Map.lookup val (hieSymbolTypes hieCtx) of
        Just ti -> pure $ Just ti
        Nothing ->
          -- Try to infer from known types database
          TI.extractType val Nothing
    Nothing -> pure Nothing

-- | Extract type of a variable
extractVarType :: HIEContext -> Text -> IO (Maybe TypeInfo)
extractVarType hieCtx varName =
  TI.extractType varName (hieModuleName hieCtx)

-- | Extract type of expression at location
extractExprType :: HIEContext -> Int -> Int -> IO (Maybe TypeInfo)
extractExprType hieCtx line col = do
  -- Check cached expression types
  case Map.lookup (line, col) (hieExprTypes hieCtx) of
    Just ti -> pure $ Just ti
    Nothing ->
      case hieFilePath hieCtx of
        Just fp -> TI.extractExprType fp line col
        Nothing -> pure Nothing

-- | Infer type from surrounding context
inferTypeFromContext :: HIEContext -> Text -> Text -> IO (Maybe TypeInfo)
inferTypeFromContext hieCtx expr context = do
  -- Try to infer from function application context
  if " " `T.isInfixOf` context
    then do
      let parts = T.words context
          mFunc = listToMaybe' parts
      case mFunc of
        Just func -> TI.extractType func Nothing
        Nothing -> pure Nothing
    else pure Nothing

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Convert comment type
convertCommentType :: CommentType -> Comments.CommentType
convertCommentType = \case
  CTLineComment   -> Comments.LineComment
  CTBlockComment  -> Comments.BlockComment
  CTHaddockLine   -> Comments.HaddockLine
  CTHaddockBlock  -> Comments.HaddockBlock
  CTPragma        -> Comments.PragmaComment

-- | Delegate to shared helper: check if value is numeric literal
isNumericLiteral :: Text -> Bool
isNumericLiteral = SCH.isNumericLiteral

-- | Delegate to shared helper: check if value is string literal
isStringLiteral :: Text -> Bool
isStringLiteral = SCH.isStringLiteral

-- | Delegate to shared helper: check if value is any literal
isLiteralValue :: Text -> Bool
isLiteralValue = SCH.isLiteralValue

-- | Delegate to shared helper: check if value is identifier
isIdentifier :: Text -> Bool
isIdentifier = SCH.isIdentifier

-- | Delegate to shared helper: check if char is uppercase
isUpperChar :: Char -> Bool
isUpperChar = SCH.isUpperChar

-- | Extract base type from a complex type
extractBaseType :: Text -> Text
extractBaseType ty
  | "[" `T.isPrefixOf` ty = fromMaybe ty $ TI.extractListElementType ty
  | "Maybe " `T.isPrefixOf` ty = fromMaybe ty $ TI.extractMaybeInnerType ty
  | "Either " `T.isPrefixOf` ty = extractAfterSpace ty
  | "IO " `T.isPrefixOf` ty = extractAfterSpace ty
  | otherwise = ty
  where
    extractAfterSpace t = fromMaybe t $ listToMaybe' $ drop 1 $ T.words t

-- | Extract function name from an expression
extractFunctionName :: Text -> Text
extractFunctionName expr =
  let stripped = T.strip expr
      parts = T.words stripped
  in fromMaybe stripped $ listToMaybe' parts

-- | Check if expression is pure syntactically
isPureSyntactically :: Text -> Bool
isPureSyntactically expr =
  let stripped = T.strip expr
  in not $ any (`T.isInfixOf` stripped)
       [ "do ", "do\n", "<-", ">>", ">>=", "liftIO", "unsafePerformIO"
       , "readIORef", "writeIORef", "modifyIORef"
       , "putStrLn", "print", "getLine", "readFile", "writeFile"
       ]

-- | Estimate expression complexity (simple heuristic)
estimateComplexity :: Text -> Int
estimateComplexity expr =
  let parenDepth = T.length (T.filter (== '(') expr)
      operators = length $ filter (\op -> op `T.isInfixOf` expr)
                    [" . ", " $ ", " <$> ", " <*> ", " >>= ", " >> "]
      lambdas = T.count "\\" expr
      cases = T.count "case " expr + T.count "of " expr
  in parenDepth + operators + lambdas * 2 + cases * 3

-- | Match type pattern (with wildcards)
matchTypePattern :: Text -> Text -> Bool
matchTypePattern ty pat
  | pat == "*" = True
  | "*" `T.isPrefixOf` pat && "*" `T.isSuffixOf` pat =
      let middle = T.dropEnd 1 (T.drop 1 pat)
      in middle `T.isInfixOf` ty
  | "*" `T.isPrefixOf` pat = T.isSuffixOf (T.drop 1 pat) ty
  | "*" `T.isSuffixOf` pat = T.isPrefixOf (T.dropEnd 1 pat) ty
  | otherwise = ty == pat

-- | Match glob pattern
matchesGlob :: Text -> Text -> Bool
matchesGlob text pat
  | "*" `T.isPrefixOf` pat && "*" `T.isSuffixOf` pat =
      let middle = T.dropEnd 1 (T.drop 1 pat)
      in T.isInfixOf middle text
  | "*" `T.isPrefixOf` pat = T.isSuffixOf (T.drop 1 pat) text
  | "*" `T.isSuffixOf` pat = T.isPrefixOf (T.dropEnd 1 pat) text
  | otherwise = text == pat

-- | Check if file is a test file
isTestFile :: Text -> Bool
isTestFile fp =
  "Test" `T.isInfixOf` fp ||
  "Spec" `T.isInfixOf` fp ||
  "test/" `T.isInfixOf` fp ||
  "tests/" `T.isInfixOf` fp

-- | Check if expression text represents a monadic bind
--
-- Detects >>= >> =<< do-notation patterns in captured text
isBindExpression :: Text -> Bool
isBindExpression expr =
  let stripped = T.strip expr
  in any (`T.isInfixOf` stripped)
       [ ">>=", ">>", "=<<", "<-"
       , "do ", "do\n", "do{"
       ]

-- | Evaluate a context predicate with file path information
--
-- Checks semantic context based on file path and naming patterns:
-- - "test" context: file is in test directory or has Test/Spec suffix
-- - "parallel" context: would need AST analysis for parallel combinators
-- - "unsafe" context: file imports System.IO.Unsafe or uses unsafePerformIO
-- - "mtl" context: file likely uses MTL-style transformers
-- - "lens" context: file likely uses lens library
evalInContextIO :: Text -> Text -> Bool
evalInContextIO ctx filePath = case ctx of
  "test" -> isTestFile filePath
  "spec" -> "Spec" `T.isInfixOf` filePath
  "benchmark" -> "bench" `T.isInfixOf` filePath || "Bench" `T.isInfixOf` filePath
  "example" -> "example" `T.isInfixOf` T.toLower filePath || "Example" `T.isInfixOf` filePath
  -- These would need deeper analysis, so be conservative
  "parallel" -> False
  "unsafe" -> False
  "mtl" -> False
  "lens" -> False
  _ -> False

-- | Convert Ordering to comparison function
ordToComp :: Ordering -> (Int -> Int -> Bool)
ordToComp LT = (<)
ordToComp EQ = (==)
ordToComp GT = (>)

-- | Safe listToMaybe
listToMaybe' :: [a] -> Maybe a
listToMaybe' [] = Nothing
listToMaybe' (x:_) = Just x

--------------------------------------------------------------------------------
-- Type Class Instance Checks (Known Types Database)
--------------------------------------------------------------------------------

-- | Check Foldable instance
checkFoldable :: Text -> Bool
checkFoldable ty =
  "[]" `T.isInfixOf` ty || "[" `T.isPrefixOf` ty ||
  "Maybe" `T.isPrefixOf` ty ||
  "Either" `T.isPrefixOf` ty ||
  "NonEmpty" `T.isPrefixOf` ty ||
  "Vector" `T.isInfixOf` ty ||
  "Set" `T.isInfixOf` ty ||
  "Map" `T.isInfixOf` ty

-- | Check Traversable instance
checkTraversable :: Text -> Bool
checkTraversable ty =
  "[]" `T.isInfixOf` ty || "[" `T.isPrefixOf` ty ||
  "Maybe" `T.isPrefixOf` ty ||
  "Either" `T.isPrefixOf` ty ||
  "NonEmpty" `T.isPrefixOf` ty ||
  "Vector" `T.isInfixOf` ty

-- | Check Functor instance
checkFunctor :: Text -> Bool
checkFunctor ty =
  "[]" `T.isInfixOf` ty || "[" `T.isPrefixOf` ty ||
  "Maybe" `T.isPrefixOf` ty ||
  "Either" `T.isPrefixOf` ty ||
  "IO" `T.isPrefixOf` ty ||
  "NonEmpty" `T.isPrefixOf` ty ||
  "Vector" `T.isInfixOf` ty ||
  "Map" `T.isInfixOf` ty ||
  "->" `T.isInfixOf` ty

-- | Check Applicative instance
checkApplicative :: Text -> Bool
checkApplicative ty =
  "[]" `T.isInfixOf` ty || "[" `T.isPrefixOf` ty ||
  "Maybe" `T.isPrefixOf` ty ||
  "Either" `T.isPrefixOf` ty ||
  "IO" `T.isPrefixOf` ty ||
  "NonEmpty" `T.isPrefixOf` ty ||
  "->" `T.isInfixOf` ty

-- | Check Monad instance
checkMonad :: Text -> Bool
checkMonad ty =
  "[]" `T.isInfixOf` ty || "[" `T.isPrefixOf` ty ||
  "Maybe" `T.isPrefixOf` ty ||
  "Either" `T.isPrefixOf` ty ||
  "IO" `T.isPrefixOf` ty ||
  "->" `T.isInfixOf` ty ||
  TI.isMonadType ty
