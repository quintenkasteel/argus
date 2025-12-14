{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Rules.SideConditions.Parser
-- Description : Text-based parser for TOML side conditions
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides a complete text-based parser for side conditions
-- that can be specified in TOML configuration files. It enables users
-- to write type-aware rules without touching Haskell code.
--
-- == Supported Syntax
--
-- === Expression Predicates
-- @
-- isLiteral $X           -- Variable must be a literal value
-- isVariable $X          -- Variable must be a simple identifier
-- isNumeric $X           -- Variable has numeric type
-- isString $X            -- Variable has String/Text type
-- isList $X              -- Variable has list type
-- isMaybe $X             -- Variable has Maybe type
-- isAtomic $X            -- Variable must be atomic (literal or var)
-- isApplication $X       -- Variable is a function application
-- isLambda $X            -- Variable is a lambda expression
-- isConstructor $X       -- Variable is a data constructor
-- @
--
-- === Type Predicates
-- @
-- hasType $X Int         -- Variable has exact type
-- hasClass $X Ord        -- Type has class instance
-- typeMatches $X Maybe*  -- Type matches pattern (with wildcards)
-- typeContains $X IO     -- Type contains given type
-- isPure $X              -- Expression is pure (no IO effects)
-- isMonad $X IO          -- Variable is in specific monad
-- @
--
-- === Comparison Predicates
-- @
-- notEq $X $Y            -- Two variables bind to different values
-- freeIn $X $Y           -- $X is free in $Y's expression
-- notFreeIn $X $Y        -- $X is NOT free in $Y's expression
-- @
--
-- === Complexity Predicates
-- @
-- complexityLt $X 5      -- Expression complexity < 5
-- complexityGt $X 10     -- Expression complexity > 10
-- @
--
-- === Context Predicates
-- @
-- hasImport Data.Text    -- Module has import
-- hasPragma Strict       -- File has LANGUAGE pragma
-- inModule *Spec         -- Code is in module matching pattern
-- inTestFile             -- Code is in test file
-- notInTestFile          -- Code is NOT in test file
-- inContext parallel     -- In named context
-- @
--
-- === Location Predicates
-- @
-- notInComment           -- Match not inside comment
-- notInString            -- Match not inside string literal
-- notInImport            -- Match not in import statement
-- inFunctionBody         -- Match inside function body
-- @
--
-- === Expression Structure
-- @
-- notBind $X             -- Not a monadic bind expression
-- isEtaReducible $F $X   -- Function can be eta-reduced
-- noDerivingStrategy     -- Deriving has no explicit strategy
-- @
--
-- === Boolean Combinators
-- @
-- isLiteral $X && isNumeric $X      -- Both conditions
-- isLiteral $X || isVariable $X     -- Either condition
-- not isLiteral $X                  -- Negation
-- @
module Argus.Rules.SideConditions.Parser
  ( -- * Main Parsing Functions
    parseSideConditionText
  , parseSideConditions
  , parseAndValidate

    -- * Predicate Evaluation
  , evaluateSideCondition
  , evaluateSideConditions
  , SideConditionError(..)

    -- * Testing Utilities
  , validateSyntax
  , describeCondition
  ) where

import Data.Char (isSpace, isDigit, isAlphaNum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

import Argus.Rules.Types (SideCondition(..))
import Argus.Rules.SideConditionHelpers qualified as SCH
import Argus.HIE.TypeInfo qualified as TI
  ( checkKnownInstance
  , isListType
  , isMaybeType
  , isIOType
  , isMonadType
  , lookupKnownType
  , defaultKnownTypes
  )
import Argus.HIE.Types (TypeInfo(..))

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

-- | Errors that can occur during parsing or evaluation
data SideConditionError
  = ParseError Text Text        -- ^ Parse error: condition text, message
  | UnknownPredicate Text       -- ^ Unknown predicate name
  | InvalidArgCount Text Int Int -- ^ Predicate, expected, actual
  | InvalidArgument Text Text   -- ^ Predicate, argument
  | EvaluationError Text        -- ^ Runtime evaluation error
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Main Parsing Functions
--------------------------------------------------------------------------------

-- | Parse a side condition from text
--
-- Supports the full range of predicates documented in the module header.
-- Returns Nothing for invalid/unparseable conditions.
--
-- Examples:
-- @
-- parseSideConditionText "isLiteral $X"
--   == Just (IsLiteral "$X")
--
-- parseSideConditionText "hasType $X Int"
--   == Just (HasType "$X" "Int")
--
-- parseSideConditionText "isLiteral $X && isNumeric $X"
--   == Just (And [IsLiteral "$X", IsNumeric "$X"])
-- @
parseSideConditionText :: Text -> Maybe SideCondition
parseSideConditionText text =
  let trimmed = T.strip text
  in if T.null trimmed
     then Nothing
     else parseCondition trimmed

-- | Parse multiple side conditions separated by semicolons or commas
--
-- Multiple conditions are combined with 'And'.
parseSideConditions :: Text -> Maybe SideCondition
parseSideConditions text =
  let trimmed = T.strip text
  in if T.null trimmed
     then Nothing
     else
       let parts = splitConditions trimmed
           parsed = mapMaybe parseSideConditionText parts
       in case parsed of
            [] -> Nothing
            [single] -> Just single
            multiple -> Just $ And multiple

-- | Parse and validate a side condition, returning detailed errors
parseAndValidate :: Text -> Either SideConditionError SideCondition
parseAndValidate text =
  let trimmed = T.strip text
  in if T.null trimmed
     then Left $ ParseError text "Empty condition"
     else case parseCondition trimmed of
       Just cond -> Right cond
       Nothing -> Left $ ParseError text "Failed to parse condition"

--------------------------------------------------------------------------------
-- Internal Parsing
--------------------------------------------------------------------------------

-- | Split text by semicolons and commas (condition separators)
splitConditions :: Text -> [Text]
splitConditions text =
  let parts = concatMap (T.splitOn ",") (T.splitOn ";" text)
  in filter (not . T.null) $ map T.strip parts

-- | Parse a single condition (may contain && or ||)
parseCondition :: Text -> Maybe SideCondition
parseCondition text
  -- Handle boolean combinators first
  | " && " `T.isInfixOf` text = parseAndCondition text
  | " || " `T.isInfixOf` text = parseOrCondition text
  | "not " `T.isPrefixOf` text = Not <$> parseCondition (T.drop 4 text)
  | "!" `T.isPrefixOf` text = Not <$> parseCondition (T.drop 1 text)
  -- Handle parenthesized conditions
  | "(" `T.isPrefixOf` text && ")" `T.isSuffixOf` text =
      parseCondition (T.drop 1 $ T.dropEnd 1 text)
  -- Handle simple predicates
  | otherwise = parsePredicate text

-- | Parse AND conditions
parseAndCondition :: Text -> Maybe SideCondition
parseAndCondition text =
  let parts = T.splitOn " && " text
      parsed = mapMaybe parseCondition parts
  in if length parsed == length parts
     then Just $ And parsed
     else Nothing

-- | Parse OR conditions
parseOrCondition :: Text -> Maybe SideCondition
parseOrCondition text =
  let parts = T.splitOn " || " text
      parsed = mapMaybe parseCondition parts
  in if length parsed == length parts
     then Just $ Or parsed
     else Nothing

-- | Parse a single predicate (no combinators)
parsePredicate :: Text -> Maybe SideCondition
parsePredicate text =
  let words' = T.words (T.strip text)
  in case words' of
    -- Zero-argument predicates
    ["always"] -> Just Always
    ["never"] -> Just Never
    ["notInComment"] -> Just NotInComment
    ["notInString"] -> Just NotInString
    ["notInImport"] -> Just NotInImport
    ["inFunctionBody"] -> Just InFunctionBody
    ["inStringLiteral"] -> Just InStringLiteral
    ["inTestFile"] -> Just InTestFile
    ["notInTestFile"] -> Just NotInTestFile
    ["noDerivingStrategy"] -> Just NoDerivingStrategy
    ["wildcardNotLast"] -> Just WildcardNotLast
    ["hasPatternOverlap"] -> Just HasPatternOverlap
    ["isPatternIncomplete"] -> Just IsPatternIncomplete
    ["hasAmbiguousType"] -> Just HasAmbiguousType
    ["usesDefaultOptions"] -> Just UsesDefaultOptions

    -- One-argument predicates
    ["isLiteral", var] -> Just $ IsLiteral var
    ["isVariable", var] -> Just $ IsVariable var
    ["isVar", var] -> Just $ IsVariable var
    ["isNumeric", var] -> Just $ IsNumeric var
    ["isString", var] -> Just $ IsString var
    ["isList", var] -> Just $ IsList var
    ["isMaybe", var] -> Just $ IsMaybe var
    ["isPure", var] -> Just $ IsPure var
    ["isAtomic", var] -> Just $ IsAtomic var
    ["isApplication", var] -> Just $ IsApplication var
    ["isApp", var] -> Just $ IsApplication var
    ["isLambda", var] -> Just $ IsLambda var
    ["isConstructor", var] -> Just $ IsConstructor var
    ["isCon", var] -> Just $ IsConstructor var
    ["notBind", var] -> Just $ NotBind var

    -- Context predicates with one arg
    ["hasImport", modName] -> Just $ HasImport modName
    ["hasPragma", pragma] -> Just $ HasPragma pragma
    ["inModule", pat] -> Just $ InModule pat
    ["inContext", ctx] -> Just $ InContext ctx

    -- Two-argument predicates
    ["hasType", var, typ] -> Just $ HasType var typ
    ["hasClass", var, cls] -> Just $ HasTypeClass var cls
    ["hasTypeClass", var, cls] -> Just $ HasTypeClass var cls
    ["typeMatches", var, pat] -> Just $ TypeMatches var pat
    ["typeContains", var, typ] -> Just $ TypeContains var typ
    ["isMonad", var, monad] -> Just $ IsMonad var monad
    ["notEq", a, b] -> Just $ NotEqual a b
    ["notEqual", a, b] -> Just $ NotEqual a b
    ["freeIn", v1, v2] -> Just $ FreeIn v1 v2
    ["notFreeIn", v1, v2] -> Just $ NotFreeIn v1 v2
    ["isEtaReducible", f, x] -> Just $ IsEtaReducible f x

    -- Complexity predicates with number
    ["complexityLt", var, nStr] -> ComplexityLT var <$> parseNum nStr
    ["complexityGt", var, nStr] -> ComplexityGT var <$> parseNum nStr
    ["complexityLT", var, nStr] -> ComplexityLT var <$> parseNum nStr
    ["complexityGT", var, nStr] -> ComplexityGT var <$> parseNum nStr

    -- Complexity condition with ordering
    ["complexity", var, op, nStr] -> do
      n <- parseNum nStr
      ord' <- parseOrd op
      Just $ ComplexityCond var ord' n

    -- NotIn with list
    ("notIn" : var : rest) -> Just $ NotIn var rest

    -- Handle equality/comparison operators inline
    _ -> parseInlineOperator text
  where
    parseNum :: Text -> Maybe Int
    parseNum t = readMaybe (T.unpack t)

    parseOrd :: Text -> Maybe Ordering
    parseOrd "<" = Just LT
    parseOrd "==" = Just EQ
    parseOrd ">" = Just GT
    parseOrd "lt" = Just LT
    parseOrd "eq" = Just EQ
    parseOrd "gt" = Just GT
    parseOrd _ = Nothing

-- | Parse inline operators like "$X != $Y"
parseInlineOperator :: Text -> Maybe SideCondition
parseInlineOperator text
  | " != " `T.isInfixOf` text =
      let [a, b] = T.splitOn " != " text
      in Just $ NotEqual (T.strip a) (T.strip b)
  | " == " `T.isInfixOf` text =
      let [a, b] = T.splitOn " == " text
          av = T.strip a
          bv = T.strip b
      in Just $ Not (NotEqual av bv)  -- Equal = Not NotEqual
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Predicate Evaluation
--------------------------------------------------------------------------------

-- | Evaluate a side condition against captured metavariables
--
-- This is a syntactic evaluation that doesn't require HIE data.
-- For full type-aware evaluation, use the functions in SideConditions module.
evaluateSideCondition :: Map Text Text -> SideCondition -> Bool
evaluateSideCondition vars = go
  where
    go :: SideCondition -> Bool
    go = \case
      -- Always/Never
      Always -> True
      Never -> False

      -- Boolean combinators
      And conds -> all go conds
      Or conds -> any go conds
      Not cond -> not (go cond)

      -- Expression predicates (syntactic checks)
      IsLiteral var -> maybe False isLiteralText (Map.lookup var vars)
      IsVariable var -> maybe False isIdentifierText (Map.lookup var vars)
      IsNumeric var -> maybe False isNumericText (Map.lookup var vars)
      IsString var -> maybe False isStringText (Map.lookup var vars)
      IsList var -> maybe False isListText (Map.lookup var vars)
      IsAtomic var -> maybe False (\v -> isLiteralText v || isIdentifierText v) (Map.lookup var vars)
      IsApplication var -> maybe False isApplicationText (Map.lookup var vars)
      IsLambda var -> maybe False isLambdaText (Map.lookup var vars)
      IsConstructor var -> maybe False isConstructorText (Map.lookup var vars)

      -- Comparison predicates
      NotEqual a b -> case (Map.lookup a vars, Map.lookup b vars) of
        (Just va, Just vb) -> va /= vb
        _ -> True  -- Be permissive if vars not found

      FreeIn needle haystack -> case (Map.lookup needle vars, Map.lookup haystack vars) of
        (Just n, Just h) -> isIdentifierText n && n `T.isInfixOf` h
        _ -> True

      NotFreeIn needle haystack -> case (Map.lookup needle vars, Map.lookup haystack vars) of
        (Just n, Just h) -> not (isIdentifierText n && n `T.isInfixOf` h)
        _ -> True

      -- Complexity predicates
      ComplexityLT var n -> maybe True (\v -> estimateComplexity v < n) (Map.lookup var vars)
      ComplexityGT var n -> maybe True (\v -> estimateComplexity v > n) (Map.lookup var vars)
      ComplexityCond var ord' n -> maybe True (\v -> compareOrd (estimateComplexity v) ord' n) (Map.lookup var vars)

      -- NotIn predicate
      NotIn var vals -> case Map.lookup var vars of
        Just v -> v `notElem` vals
        Nothing -> True

      -- Expression structure
      NotBind var -> maybe True (not . isBindText) (Map.lookup var vars)
      IsEtaReducible f x -> case (Map.lookup f vars, Map.lookup x vars) of
        (Just fv, Just xv) -> isIdentifierText xv && not (xv `T.isInfixOf` fv)
        _ -> True

      -- Type predicates - use known-types database for evaluation
      HasType var expectedType ->
        case Map.lookup var vars of
          Just val ->
            -- Check if the captured value has the expected type using known-types DB
            case TI.lookupKnownType TI.defaultKnownTypes val of
              Just ti -> expectedType `T.isInfixOf` tiType ti
              Nothing -> inferTypeFromValue val expectedType
          Nothing -> True  -- Be permissive if var not found

      HasTypeClass var cls ->
        case Map.lookup var vars of
          Just val ->
            -- Check if we can infer the type and verify the instance
            let inferredType = inferTypeFromValueSimple val
            in case inferredType of
                 Just ty -> TI.checkKnownInstance cls ty
                 Nothing -> True  -- Be permissive
          Nothing -> True

      TypeMatches var pat ->
        case Map.lookup var vars of
          Just val ->
            case TI.lookupKnownType TI.defaultKnownTypes val of
              Just ti -> matchTypePattern (tiType ti) pat
              Nothing -> matchTypePattern val pat
          Nothing -> True

      TypeContains var searchType ->
        case Map.lookup var vars of
          Just val ->
            case TI.lookupKnownType TI.defaultKnownTypes val of
              Just ti -> searchType `T.isInfixOf` tiType ti
              Nothing -> searchType `T.isInfixOf` val
          Nothing -> True

      IsPure var ->
        case Map.lookup var vars of
          Just val ->
            -- Check if the value is a known pure function
            case TI.lookupKnownType TI.defaultKnownTypes val of
              Just ti -> not (TI.isIOType (tiType ti))
              Nothing -> isPureValue val
          Nothing -> True

      IsMonad var monadName ->
        case Map.lookup var vars of
          Just val ->
            case TI.lookupKnownType TI.defaultKnownTypes val of
              Just ti -> monadName `T.isInfixOf` tiType ti || TI.isMonadType (tiType ti)
              Nothing -> monadName `T.isInfixOf` val
          Nothing -> True

      IsMaybe var ->
        case Map.lookup var vars of
          Just val ->
            case TI.lookupKnownType TI.defaultKnownTypes val of
              Just ti -> TI.isMaybeType (tiType ti)
              Nothing -> "Maybe" `T.isInfixOf` val || "Just" `T.isPrefixOf` val || val == "Nothing"
          Nothing -> True

      -- Context predicates - need file/module context
      HasImport _ -> True
      HasPragma _ -> True
      InModule _ -> True
      InTestFile -> True
      NotInTestFile -> True
      InContext _ -> True

      -- Location predicates - need comment index
      NotInComment -> True
      NotInString -> True
      NotInImport -> True
      InFunctionBody -> True
      InStringLiteral -> False
      InCommentType _ -> False

      -- Pattern analysis predicates
      NoDerivingStrategy -> True
      WildcardNotLast -> True
      HasPatternOverlap -> False
      IsPatternIncomplete -> False
      HasAmbiguousType -> False
      UsesDefaultOptions -> maybe False ("defaultOptions" `T.isInfixOf`)
                                        (Map.lookup "$X" vars)

-- | Evaluate multiple side conditions (all must pass)
evaluateSideConditions :: Map Text Text -> [SideCondition] -> Bool
evaluateSideConditions vars = all (evaluateSideCondition vars)

--------------------------------------------------------------------------------
-- Validation and Description
--------------------------------------------------------------------------------

-- | Validate syntax without parsing (for quick checks)
validateSyntax :: Text -> Either SideConditionError ()
validateSyntax text =
  case parseSideConditionText text of
    Just _ -> Right ()
    Nothing -> Left $ ParseError text "Invalid syntax"

-- | Generate human-readable description of a condition
describeCondition :: SideCondition -> Text
describeCondition = \case
  Always -> "always matches"
  Never -> "never matches"
  And conds -> "all of: " <> T.intercalate ", " (map describeCondition conds)
  Or conds -> "any of: " <> T.intercalate ", " (map describeCondition conds)
  Not cond -> "not: " <> describeCondition cond

  IsLiteral var -> var <> " is a literal value"
  IsVariable var -> var <> " is a variable"
  IsNumeric var -> var <> " is numeric"
  IsString var -> var <> " is a string"
  IsList var -> var <> " is a list"
  IsMaybe var -> var <> " is a Maybe"
  IsAtomic var -> var <> " is atomic (literal or variable)"
  IsApplication var -> var <> " is a function application"
  IsLambda var -> var <> " is a lambda"
  IsConstructor var -> var <> " is a constructor"
  IsPure var -> var <> " is pure (no IO effects)"

  HasType var typ -> var <> " has type " <> typ
  HasTypeClass var cls -> "type of " <> var <> " has instance for " <> cls
  TypeMatches var pat -> "type of " <> var <> " matches pattern " <> pat
  TypeContains var typ -> "type of " <> var <> " contains " <> typ
  IsMonad var m -> var <> " is in monad " <> m

  NotEqual a b -> a <> " != " <> b
  FreeIn v1 v2 -> v1 <> " is free in " <> v2
  NotFreeIn v1 v2 -> v1 <> " is not free in " <> v2

  ComplexityLT var n -> "complexity of " <> var <> " < " <> T.pack (show n)
  ComplexityGT var n -> "complexity of " <> var <> " > " <> T.pack (show n)
  ComplexityCond var ord' n -> "complexity of " <> var <> " " <> showOrd ord' <> " " <> T.pack (show n)

  NotIn var vals -> var <> " not in [" <> T.intercalate ", " vals <> "]"

  HasImport m -> "has import " <> m
  HasPragma p -> "has pragma " <> p
  InModule pat -> "in module matching " <> pat
  InTestFile -> "in test file"
  NotInTestFile -> "not in test file"
  InContext ctx -> "in context " <> ctx

  NotInComment -> "not inside comment"
  NotInString -> "not inside string"
  NotInImport -> "not in import"
  InFunctionBody -> "inside function body"
  InStringLiteral -> "inside string literal"
  InCommentType ct -> "inside " <> T.pack (show ct)

  NotBind var -> var <> " is not a monadic bind"
  IsEtaReducible f x -> f <> " is eta-reducible with " <> x
  NoDerivingStrategy -> "deriving has no explicit strategy"
  WildcardNotLast -> "wildcard is not last case"
  HasPatternOverlap -> "patterns have overlap"
  IsPatternIncomplete -> "pattern match is incomplete"
  HasAmbiguousType -> "expression has ambiguous type"
  UsesDefaultOptions -> "uses default options"
  where
    showOrd LT = "<"
    showOrd EQ = "=="
    showOrd GT = ">"

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Delegate to shared helper: check if text is literal
isLiteralText :: Text -> Bool
isLiteralText t
  | t == "()" = True  -- Unit literal handled specially
  | otherwise = SCH.isLiteralValue t

-- | Delegate to shared helper: check if text is numeric
isNumericText :: Text -> Bool
isNumericText = SCH.isNumericLiteral

-- | Delegate to shared helper: check if text is string literal
isStringText :: Text -> Bool
isStringText = SCH.isStringLiteral

-- | Delegate to shared helper: check if text is list literal
isListText :: Text -> Bool
isListText = SCH.isListValue . T.strip

-- | Delegate to shared helper: check if text is identifier
isIdentifierText :: Text -> Bool
isIdentifierText = SCH.isIdentifier

-- | Delegate to shared helper: check if text is constructor
isConstructorText :: Text -> Bool
isConstructorText = SCH.isConstructor

-- | Check if text is a function application
isApplicationText :: Text -> Bool
isApplicationText t =
  " " `T.isInfixOf` T.strip t &&
  not ("\\" `T.isPrefixOf` T.strip t) &&
  not ("(" `T.isPrefixOf` T.strip t && ")" `T.isSuffixOf` T.strip t)

-- | Check if text is a lambda
isLambdaText :: Text -> Bool
isLambdaText t = "\\" `T.isPrefixOf` T.strip t

-- | Check if text contains monadic bind operators
isBindText :: Text -> Bool
isBindText t =
  let stripped = T.strip t
  in any (`T.isInfixOf` stripped)
       [ ">>=", ">>", "=<<", "<-"
       , "do ", "do\n", "do{"
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

-- | Compare with ordering
compareOrd :: Int -> Ordering -> Int -> Bool
compareOrd x LT y = x < y
compareOrd x EQ y = x == y
compareOrd x GT y = x > y

-- | Infer type from value and check if it matches expected type
inferTypeFromValue :: Text -> Text -> Bool
inferTypeFromValue val expectedType
  | isNumericText val = expectedType `elem` ["Int", "Integer", "Double", "Float", "Num", "a"]
  | isStringText val = expectedType `elem` ["String", "Text", "[Char]", "a"]
  | isListText val = expectedType `elem` ["[a]", "List", "[]", "a"] || "[" `T.isPrefixOf` expectedType
  | val == "True" || val == "False" = expectedType `elem` ["Bool", "a"]
  | val == "Nothing" = expectedType `elem` ["Maybe a", "Maybe", "a"] || "Maybe" `T.isPrefixOf` expectedType
  | "Just " `T.isPrefixOf` val = expectedType `elem` ["Maybe a", "Maybe", "a"] || "Maybe" `T.isPrefixOf` expectedType
  | "()" == val = expectedType `elem` ["()", "Unit", "a"]
  | "," `T.isInfixOf` val && "(" `T.isPrefixOf` val = expectedType `elem` ["(a, b)", "(,)", "a"] || "(" `T.isPrefixOf` expectedType
  | otherwise = True  -- Be permissive for unknown values

-- | Infer a simple type from a value (for instance checking)
inferTypeFromValueSimple :: Text -> Maybe Text
inferTypeFromValueSimple val
  | isNumericText val =
      if "." `T.isInfixOf` val then Just "Double" else Just "Int"
  | isStringText val = Just "String"
  | val == "True" || val == "False" = Just "Bool"
  | val == "()" = Just "()"
  | "Just " `T.isPrefixOf` val = Just "Maybe"
  | val == "Nothing" = Just "Maybe"
  | "[" `T.isPrefixOf` val && "]" `T.isSuffixOf` val = Just "[a]"
  | otherwise = Nothing

-- | Check if a value is known to be pure
isPureValue :: Text -> Bool
isPureValue val =
  let stripped = T.strip val
      -- IO indicators
      ioIndicators = ["IO ", "putStrLn", "print", "readFile", "writeFile",
                      "getLine", "getContents", "readIO", "hPutStrLn",
                      "modifyIORef", "newIORef", "readIORef", "writeIORef",
                      "atomically", "newTVar", "readTVar", "writeTVar",
                      "forkIO", "killThread", "threadDelay"]
  in not $ any (`T.isInfixOf` stripped) ioIndicators

-- | Check if type matches a pattern (with * wildcards)
matchTypePattern :: Text -> Text -> Bool
matchTypePattern ty pat
  | "*" `T.isSuffixOf` pat =
      let prefix = T.dropEnd 1 pat
      in prefix `T.isPrefixOf` ty
  | "*" `T.isPrefixOf` pat =
      let suffix = T.drop 1 pat
      in suffix `T.isSuffixOf` ty
  | "*" `T.isInfixOf` pat =
      -- Split pattern at * and check both parts
      let parts = T.splitOn "*" pat
      in case parts of
           [p1, p2] -> p1 `T.isPrefixOf` ty && p2 `T.isSuffixOf` ty
           _ -> pat `T.isInfixOf` ty
  | otherwise = ty == pat || pat `T.isInfixOf` ty
