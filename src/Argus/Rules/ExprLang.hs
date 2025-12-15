{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.ExprLang
-- Description : Typed expression language for Argus rules
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- This module provides a typed expression language for defining custom
-- predicates and conditions in TOML rules. It offers a safer alternative
-- to raw Haskell evaluation with built-in functions for code analysis.
--
-- == Overview
--
-- The expression language supports:
--
-- * Literals: booleans, integers, floats, strings, lists, records
-- * Operators: arithmetic, comparison, logical, string
-- * Built-in functions: 70+ functions for text, list, type, and AST operations
-- * Metavariables: Access captured pattern variables ($x, $body, etc.)
-- * Context queries: Module name, file path, imports, pragmas, etc.
--
-- == Example Usage in TOML
--
-- @
-- [[rules.custom]]
-- id = "complex-lambda"
-- pattern = "\\\\$x -> $body"
-- condition_expr = \"\"\"
--   complexity($body) > 10 &&
--   not(isAtomic($body)) &&
--   length(freeVars($body)) > 3
-- \"\"\"
-- message = "Lambda body is too complex"
--
-- [[rules.custom]]
-- id = "unused-parameter"
-- pattern = "\\\\$x -> $body"
-- condition_expr = "not(elem($x, freeVars($body)))"
-- message = "Parameter $x is unused"
-- replacement = "$body"
-- @
--
-- == Built-in Functions
--
-- === Text Operations
--
-- * @length(s)@ - Length of string
-- * @toUpper(s)@, @toLower(s)@ - Case conversion
-- * @strip(s)@ - Remove whitespace
-- * @split(delim, s)@ - Split string
-- * @contains(needle, haystack)@ - Substring check
-- * @startsWith(prefix, s)@, @endsWith(suffix, s)@ - Prefix/suffix check
-- * @replace(old, new, s)@ - Replace occurrences
-- * @regexMatch(pattern, s)@ - Regex matching
--
-- === List Operations
--
-- * @head(xs)@, @tail(xs)@, @init(xs)@, @last(xs)@ - List access
-- * @take(n, xs)@, @drop(n, xs)@ - Subsequences
-- * @reverse(xs)@, @sort(xs)@, @nub(xs)@ - Transformations
-- * @filter(pred, xs)@, @map(f, xs)@ - Higher-order operations
-- * @foldl(f, acc, xs)@, @foldr(f, acc, xs)@ - Folds
-- * @all(pred, xs)@, @any(pred, xs)@ - Predicates
-- * @elem(x, xs)@, @notElem(x, xs)@ - Membership
--
-- === AST Queries
--
-- * @complexity(expr)@ - Estimate expression complexity
-- * @freeVars(expr)@ - Get free variables
-- * @isLiteral(expr)@, @isVariable(expr)@ - Expression type checks
-- * @isApplication(expr)@, @isLambda(expr)@ - Structure checks
-- * @isAtomic(expr)@ - Check if atomic (literal or variable)
-- * @arity(expr)@ - Estimate function arity
--
-- === Type Queries
--
-- * @typeOf(expr)@ - Get inferred type (requires HIE)
-- * @hasType(expr, type)@ - Check type (requires HIE)
-- * @hasTypeClass(expr, class)@ - Check typeclass (requires HIE)
-- * @isPure(expr)@ - Check if expression is pure
-- * @hasEffect(expr)@ - Check if has effects
--
-- === Context Queries
--
-- * @moduleName@ - Current module name
-- * @filePath@ - Current file path
-- * @lineNumber@, @columnNumber@ - Match location
-- * @matchedText@ - Full matched text
-- * @metavar(name)@ - Get metavariable value
-- * @imports@, @exports@, @pragmas@ - Module info
-- * @hasImport(mod)@, @hasPragma(pragma)@ - Check presence
module Argus.Rules.ExprLang
  ( -- * Parsing
    parseExpr
  , parseValue
  , parseType
  , ParseResult (..)
  , ParseError (..)

    -- * Types
  , Expr (..)
  , Value (..)
  , ExprType (..)
  , BinOp (..)
  , Builtin (..)
  , BuiltinInfo (..)
  , allBuiltins
  , builtinInfo

    -- * Type Checking
  , typeCheck
  , inferType
  , TypeCheckResult (..)
  , TypeScheme (..)
  , TypeError (..)
  , TypeEnv

    -- * Evaluation
  , eval
  , evalWithContext
  , evalToBool
  , evalToString
  , EvalResult (..)
  , EvalError (..)

    -- * Environment
  , EvalEnv (..)
  , emptyEvalEnv
  , extendEnv
  , lookupVar
  , bindMetavar

    -- * Match Context
  , MatchContext (..)
  , emptyMatchContext

    -- * Integration
  , evalConditionExpr
  , parseAndEval
  , compileExpr
  , CompiledExpr (..)
  ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Argus.Rules.ExprLang.Eval
import Argus.Rules.ExprLang.Parser
import Argus.Rules.ExprLang.Types

--------------------------------------------------------------------------------
-- Integration Functions
--------------------------------------------------------------------------------

-- | Evaluate a condition expression from a TOML rule
evalConditionExpr :: MatchContext -> Text -> Either Text Bool
evalConditionExpr ctx exprText =
  case parseExpr exprText of
    ParseErr err -> Left $ peMessage err
    ParseOk expr ->
      let env = emptyEvalEnv { eeMatchContext = ctx }
      in case evalToBool env expr of
           Right b -> Right b
           Left err -> Left $ evMessage err

-- | Parse and evaluate an expression
parseAndEval :: EvalEnv -> Text -> Either Text Value
parseAndEval env exprText =
  case parseExpr exprText of
    ParseErr err -> Left $ peMessage err
    ParseOk expr ->
      case eval env expr of
        Argus.Rules.ExprLang.Eval.EvalOk v -> Right v
        Argus.Rules.ExprLang.Eval.EvalErr err -> Left $ evMessage err

-- | A compiled expression ready for evaluation
data CompiledExpr = CompiledExpr
  { ceExpr :: Expr
  , ceType :: Maybe ExprType
  , ceSource :: Text
  }
  deriving stock (Eq, Show)

-- | Compile an expression for repeated evaluation
compileExpr :: Text -> Either ParseError CompiledExpr
compileExpr source =
  case parseExpr source of
    ParseErr err -> Left err
    ParseOk expr ->
      let typ = case typeCheck Map.empty expr of
            TypeOk t -> Just t
            TypeErr _ -> Nothing
      in Right $ CompiledExpr
           { ceExpr = expr
           , ceType = typ
           , ceSource = source
           }
