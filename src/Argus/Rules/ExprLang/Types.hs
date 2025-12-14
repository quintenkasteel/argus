{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Rules.ExprLang.Types
-- Description : Types for the Argus expression language
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- This module defines the types for Argus's typed expression language,
-- which provides a safer alternative to raw Haskell for defining custom
-- predicates and conditions in TOML rules.
module Argus.Rules.ExprLang.Types
  ( -- * Expression Types
    Expr (..)
  , BinOp (..)
  , Value (..)
  , ExprType (..)

    -- * Built-in Functions
  , Builtin (..)
  , BuiltinInfo (..)
  , allBuiltins
  , builtinInfo

    -- * Type System
  , TypeScheme (..)
  , TypeVar
  , TypeError (..)
  , TypeEnv

    -- * Evaluation Context
  , EvalEnv (..)
  , emptyEvalEnv
  , EvalError (..)

    -- * Pattern Context
  , MatchContext (..)
  , emptyMatchContext
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Expression Types
--------------------------------------------------------------------------------

-- | Expression AST
data Expr
  = ELit Value
      -- ^ Literal value
  | EVar Text
      -- ^ Variable reference
  | EApp Expr [Expr]
      -- ^ Function application
  | ELam [Text] Expr
      -- ^ Lambda abstraction
  | ELet [(Text, Expr)] Expr
      -- ^ Let binding
  | EIf Expr Expr Expr
      -- ^ Conditional
  | EOp BinOp Expr Expr
      -- ^ Binary operation
  | ENot Expr
      -- ^ Logical negation
  | EBuiltin Builtin
      -- ^ Built-in function
  | EField Expr Text
      -- ^ Field access (e.g., ctx.matchedText)
  | EIndex Expr Expr
      -- ^ Index access (e.g., list[0])
  | EList [Expr]
      -- ^ List literal
  | ERecord [(Text, Expr)]
      -- ^ Record literal
  | EMatch Expr
      -- ^ Reference to matched pattern ($match)
  | EMetavar Text
      -- ^ Metavariable from pattern ($x)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Binary operators
data BinOp
  = OpAdd | OpSub | OpMul | OpDiv | OpMod
  | OpEq | OpNeq | OpLt | OpLte | OpGt | OpGte
  | OpAnd | OpOr
  | OpConcat
  | OpCons
  | OpIn
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Runtime values
data Value
  = VBool Bool
  | VInt Integer
  | VFloat Double
  | VString Text
  | VList [Value]
  | VRecord (Map Text Value)
  | VSet (Set Text)
  | VNothing
  | VJust Value
  | VFunc [Text] Expr EvalEnv
      -- ^ Closure
  | VBuiltin Builtin
      -- ^ Partially applied builtin
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Expression types
data ExprType
  = TBool
  | TInt
  | TFloat
  | TString
  | TList ExprType
  | TSet ExprType
  | TRecord (Map Text ExprType)
  | TMaybe ExprType
  | TFunc [ExprType] ExprType
  | TVar TypeVar
  | TAny
      -- ^ Top type (matches anything)
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Type variable
type TypeVar = Text

--------------------------------------------------------------------------------
-- Built-in Functions
--------------------------------------------------------------------------------

-- | Built-in functions
data Builtin
  -- Text operations
  = BLength
  | BConcat
  | BToUpper
  | BToLower
  | BStrip
  | BSplit
  | BContains
  | BStartsWith
  | BEndsWith
  | BReplace
  | BRegexMatch
  | BRegexFind

  -- List operations
  | BHead
  | BTail
  | BInit
  | BLast
  | BTake
  | BDrop
  | BReverse
  | BSort
  | BNub
  | BFilter
  | BMap
  | BFoldl
  | BFoldr
  | BAll
  | BAny
  | BElem
  | BNotElem
  | BZip
  | BZipWith
  | BFlatten

  -- Set operations
  | BUnion
  | BIntersect
  | BDifference
  | BIsSubset
  | BFromList
  | BToList

  -- Numeric operations
  | BAbs
  | BMax
  | BMin
  | BSum
  | BProduct
  | BCeiling
  | BFloor
  | BRound

  -- Type queries
  | BTypeOf
  | BKindOf
  | BHasType
  | BHasTypeClass
  | BIsFunctor
  | BIsMonad
  | BIsApplicative

  -- Purity/Effect queries
  | BIsPure
  | BHasEffect
  | BEffectType

  -- AST queries
  | BComplexity
  | BFreeVars
  | BBindings
  | BIsLiteral
  | BIsVariable
  | BIsApplication
  | BIsLambda
  | BIsAtomic
  | BArity

  -- Context queries
  | BModuleName
  | BFilePath
  | BLineNumber
  | BColumnNumber
  | BMatchedText
  | BMetavar
  | BImports
  | BExports
  | BPragmas
  | BHasImport
  | BHasPragma

  -- Utilities
  | BIf
  | BNot
  | BError
  | BTrace
  | BShow
  | BRead
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Information about a built-in function
data BuiltinInfo = BuiltinInfo
  { biName :: Text
  , biDescription :: Text
  , biType :: TypeScheme
  , biArity :: Int
  , biPure :: Bool
  }
  deriving stock (Eq, Show, Generic)

-- | Get all built-in functions
allBuiltins :: [Builtin]
allBuiltins = [minBound .. maxBound]

-- | Get information about a built-in function
builtinInfo :: Builtin -> BuiltinInfo
builtinInfo = \case
  -- Text operations
  BLength -> BuiltinInfo "length" "Get length of string or list"
    (TScheme [] (TFunc [TString] TInt)) 1 True
  BConcat -> BuiltinInfo "concat" "Concatenate strings or lists"
    (TScheme ["a"] (TFunc [TList (TVar "a")] (TVar "a"))) 1 True
  BToUpper -> BuiltinInfo "toUpper" "Convert to uppercase"
    (TScheme [] (TFunc [TString] TString)) 1 True
  BToLower -> BuiltinInfo "toLower" "Convert to lowercase"
    (TScheme [] (TFunc [TString] TString)) 1 True
  BStrip -> BuiltinInfo "strip" "Remove leading/trailing whitespace"
    (TScheme [] (TFunc [TString] TString)) 1 True
  BSplit -> BuiltinInfo "split" "Split string by delimiter"
    (TScheme [] (TFunc [TString, TString] (TList TString))) 2 True
  BContains -> BuiltinInfo "contains" "Check if string contains substring"
    (TScheme [] (TFunc [TString, TString] TBool)) 2 True
  BStartsWith -> BuiltinInfo "startsWith" "Check if string starts with prefix"
    (TScheme [] (TFunc [TString, TString] TBool)) 2 True
  BEndsWith -> BuiltinInfo "endsWith" "Check if string ends with suffix"
    (TScheme [] (TFunc [TString, TString] TBool)) 2 True
  BReplace -> BuiltinInfo "replace" "Replace occurrences in string"
    (TScheme [] (TFunc [TString, TString, TString] TString)) 3 True
  BRegexMatch -> BuiltinInfo "regexMatch" "Check if string matches regex"
    (TScheme [] (TFunc [TString, TString] TBool)) 2 True
  BRegexFind -> BuiltinInfo "regexFind" "Find all regex matches"
    (TScheme [] (TFunc [TString, TString] (TList TString))) 2 True

  -- List operations
  BHead -> BuiltinInfo "head" "Get first element"
    (TScheme ["a"] (TFunc [TList (TVar "a")] (TMaybe (TVar "a")))) 1 True
  BTail -> BuiltinInfo "tail" "Get all but first element"
    (TScheme ["a"] (TFunc [TList (TVar "a")] (TList (TVar "a")))) 1 True
  BInit -> BuiltinInfo "init" "Get all but last element"
    (TScheme ["a"] (TFunc [TList (TVar "a")] (TList (TVar "a")))) 1 True
  BLast -> BuiltinInfo "last" "Get last element"
    (TScheme ["a"] (TFunc [TList (TVar "a")] (TMaybe (TVar "a")))) 1 True
  BTake -> BuiltinInfo "take" "Take first n elements"
    (TScheme ["a"] (TFunc [TInt, TList (TVar "a")] (TList (TVar "a")))) 2 True
  BDrop -> BuiltinInfo "drop" "Drop first n elements"
    (TScheme ["a"] (TFunc [TInt, TList (TVar "a")] (TList (TVar "a")))) 2 True
  BReverse -> BuiltinInfo "reverse" "Reverse a list"
    (TScheme ["a"] (TFunc [TList (TVar "a")] (TList (TVar "a")))) 1 True
  BSort -> BuiltinInfo "sort" "Sort a list"
    (TScheme ["a"] (TFunc [TList (TVar "a")] (TList (TVar "a")))) 1 True
  BNub -> BuiltinInfo "nub" "Remove duplicates"
    (TScheme ["a"] (TFunc [TList (TVar "a")] (TList (TVar "a")))) 1 True
  BFilter -> BuiltinInfo "filter" "Filter list by predicate"
    (TScheme ["a"] (TFunc [TFunc [TVar "a"] TBool, TList (TVar "a")] (TList (TVar "a")))) 2 True
  BMap -> BuiltinInfo "map" "Map function over list"
    (TScheme ["a", "b"] (TFunc [TFunc [TVar "a"] (TVar "b"), TList (TVar "a")] (TList (TVar "b")))) 2 True
  BFoldl -> BuiltinInfo "foldl" "Left fold over list"
    (TScheme ["a", "b"] (TFunc [TFunc [TVar "b", TVar "a"] (TVar "b"), TVar "b", TList (TVar "a")] (TVar "b"))) 3 True
  BFoldr -> BuiltinInfo "foldr" "Right fold over list"
    (TScheme ["a", "b"] (TFunc [TFunc [TVar "a", TVar "b"] (TVar "b"), TVar "b", TList (TVar "a")] (TVar "b"))) 3 True
  BAll -> BuiltinInfo "all" "Check if all elements satisfy predicate"
    (TScheme ["a"] (TFunc [TFunc [TVar "a"] TBool, TList (TVar "a")] TBool)) 2 True
  BAny -> BuiltinInfo "any" "Check if any element satisfies predicate"
    (TScheme ["a"] (TFunc [TFunc [TVar "a"] TBool, TList (TVar "a")] TBool)) 2 True
  BElem -> BuiltinInfo "elem" "Check if element is in list"
    (TScheme ["a"] (TFunc [TVar "a", TList (TVar "a")] TBool)) 2 True
  BNotElem -> BuiltinInfo "notElem" "Check if element is not in list"
    (TScheme ["a"] (TFunc [TVar "a", TList (TVar "a")] TBool)) 2 True
  BZip -> BuiltinInfo "zip" "Zip two lists together"
    (TScheme ["a", "b"] (TFunc [TList (TVar "a"), TList (TVar "b")] (TList (TRecord (Map.fromList [("fst", TVar "a"), ("snd", TVar "b")]))))) 2 True
  BZipWith -> BuiltinInfo "zipWith" "Zip two lists with function"
    (TScheme ["a", "b", "c"] (TFunc [TFunc [TVar "a", TVar "b"] (TVar "c"), TList (TVar "a"), TList (TVar "b")] (TList (TVar "c")))) 3 True
  BFlatten -> BuiltinInfo "flatten" "Flatten nested list"
    (TScheme ["a"] (TFunc [TList (TList (TVar "a"))] (TList (TVar "a")))) 1 True

  -- Set operations
  BUnion -> BuiltinInfo "union" "Set union"
    (TScheme ["a"] (TFunc [TSet (TVar "a"), TSet (TVar "a")] (TSet (TVar "a")))) 2 True
  BIntersect -> BuiltinInfo "intersect" "Set intersection"
    (TScheme ["a"] (TFunc [TSet (TVar "a"), TSet (TVar "a")] (TSet (TVar "a")))) 2 True
  BDifference -> BuiltinInfo "difference" "Set difference"
    (TScheme ["a"] (TFunc [TSet (TVar "a"), TSet (TVar "a")] (TSet (TVar "a")))) 2 True
  BIsSubset -> BuiltinInfo "isSubset" "Check if first set is subset of second"
    (TScheme ["a"] (TFunc [TSet (TVar "a"), TSet (TVar "a")] TBool)) 2 True
  BFromList -> BuiltinInfo "fromList" "Create set from list"
    (TScheme ["a"] (TFunc [TList (TVar "a")] (TSet (TVar "a")))) 1 True
  BToList -> BuiltinInfo "toList" "Convert set to list"
    (TScheme ["a"] (TFunc [TSet (TVar "a")] (TList (TVar "a")))) 1 True

  -- Numeric operations
  BAbs -> BuiltinInfo "abs" "Absolute value"
    (TScheme [] (TFunc [TInt] TInt)) 1 True
  BMax -> BuiltinInfo "max" "Maximum of two values"
    (TScheme ["a"] (TFunc [TVar "a", TVar "a"] (TVar "a"))) 2 True
  BMin -> BuiltinInfo "min" "Minimum of two values"
    (TScheme ["a"] (TFunc [TVar "a", TVar "a"] (TVar "a"))) 2 True
  BSum -> BuiltinInfo "sum" "Sum of list"
    (TScheme [] (TFunc [TList TInt] TInt)) 1 True
  BProduct -> BuiltinInfo "product" "Product of list"
    (TScheme [] (TFunc [TList TInt] TInt)) 1 True
  BCeiling -> BuiltinInfo "ceiling" "Round up"
    (TScheme [] (TFunc [TFloat] TInt)) 1 True
  BFloor -> BuiltinInfo "floor" "Round down"
    (TScheme [] (TFunc [TFloat] TInt)) 1 True
  BRound -> BuiltinInfo "round" "Round to nearest integer"
    (TScheme [] (TFunc [TFloat] TInt)) 1 True

  -- Type queries
  BTypeOf -> BuiltinInfo "typeOf" "Get type of matched expression"
    (TScheme [] (TFunc [TString] (TMaybe TString))) 1 True
  BKindOf -> BuiltinInfo "kindOf" "Get kind of type"
    (TScheme [] (TFunc [TString] (TMaybe TString))) 1 True
  BHasType -> BuiltinInfo "hasType" "Check if expression has type"
    (TScheme [] (TFunc [TString, TString] TBool)) 2 True
  BHasTypeClass -> BuiltinInfo "hasTypeClass" "Check if type has typeclass"
    (TScheme [] (TFunc [TString, TString] TBool)) 2 True
  BIsFunctor -> BuiltinInfo "isFunctor" "Check if type is a Functor"
    (TScheme [] (TFunc [TString] TBool)) 1 True
  BIsMonad -> BuiltinInfo "isMonad" "Check if type is a Monad"
    (TScheme [] (TFunc [TString] TBool)) 1 True
  BIsApplicative -> BuiltinInfo "isApplicative" "Check if type is Applicative"
    (TScheme [] (TFunc [TString] TBool)) 1 True

  -- Purity/Effect queries
  BIsPure -> BuiltinInfo "isPure" "Check if expression is pure"
    (TScheme [] (TFunc [TString] TBool)) 1 True
  BHasEffect -> BuiltinInfo "hasEffect" "Check if expression has effects"
    (TScheme [] (TFunc [TString] TBool)) 1 True
  BEffectType -> BuiltinInfo "effectType" "Get effect type of expression"
    (TScheme [] (TFunc [TString] (TMaybe TString))) 1 True

  -- AST queries
  BComplexity -> BuiltinInfo "complexity" "Calculate AST complexity"
    (TScheme [] (TFunc [TString] TInt)) 1 True
  BFreeVars -> BuiltinInfo "freeVars" "Get free variables"
    (TScheme [] (TFunc [TString] (TSet TString))) 1 True
  BBindings -> BuiltinInfo "bindings" "Get local bindings"
    (TScheme [] (TFunc [TString] (TSet TString))) 1 True
  BIsLiteral -> BuiltinInfo "isLiteral" "Check if expression is literal"
    (TScheme [] (TFunc [TString] TBool)) 1 True
  BIsVariable -> BuiltinInfo "isVariable" "Check if expression is variable"
    (TScheme [] (TFunc [TString] TBool)) 1 True
  BIsApplication -> BuiltinInfo "isApplication" "Check if expression is application"
    (TScheme [] (TFunc [TString] TBool)) 1 True
  BIsLambda -> BuiltinInfo "isLambda" "Check if expression is lambda"
    (TScheme [] (TFunc [TString] TBool)) 1 True
  BIsAtomic -> BuiltinInfo "isAtomic" "Check if expression is atomic"
    (TScheme [] (TFunc [TString] TBool)) 1 True
  BArity -> BuiltinInfo "arity" "Get function arity"
    (TScheme [] (TFunc [TString] TInt)) 1 True

  -- Context queries
  BModuleName -> BuiltinInfo "moduleName" "Get current module name"
    (TScheme [] TString) 0 True
  BFilePath -> BuiltinInfo "filePath" "Get current file path"
    (TScheme [] TString) 0 True
  BLineNumber -> BuiltinInfo "lineNumber" "Get match line number"
    (TScheme [] TInt) 0 True
  BColumnNumber -> BuiltinInfo "columnNumber" "Get match column number"
    (TScheme [] TInt) 0 True
  BMatchedText -> BuiltinInfo "matchedText" "Get matched text"
    (TScheme [] TString) 0 True
  BMetavar -> BuiltinInfo "metavar" "Get metavariable value"
    (TScheme [] (TFunc [TString] (TMaybe TString))) 1 True
  BImports -> BuiltinInfo "imports" "Get module imports"
    (TScheme [] (TList TString)) 0 True
  BExports -> BuiltinInfo "exports" "Get module exports"
    (TScheme [] (TList TString)) 0 True
  BPragmas -> BuiltinInfo "pragmas" "Get file pragmas"
    (TScheme [] (TList TString)) 0 True
  BHasImport -> BuiltinInfo "hasImport" "Check if module is imported"
    (TScheme [] (TFunc [TString] TBool)) 1 True
  BHasPragma -> BuiltinInfo "hasPragma" "Check if pragma is present"
    (TScheme [] (TFunc [TString] TBool)) 1 True

  -- Utilities
  BIf -> BuiltinInfo "if" "Conditional expression"
    (TScheme ["a"] (TFunc [TBool, TVar "a", TVar "a"] (TVar "a"))) 3 True
  BNot -> BuiltinInfo "not" "Logical negation"
    (TScheme [] (TFunc [TBool] TBool)) 1 True
  BError -> BuiltinInfo "error" "Raise an error"
    (TScheme ["a"] (TFunc [TString] (TVar "a"))) 1 False
  BTrace -> BuiltinInfo "trace" "Debug trace"
    (TScheme ["a"] (TFunc [TString, TVar "a"] (TVar "a"))) 2 False
  BShow -> BuiltinInfo "show" "Convert to string"
    (TScheme ["a"] (TFunc [TVar "a"] TString)) 1 True
  BRead -> BuiltinInfo "read" "Parse from string"
    (TScheme ["a"] (TFunc [TString] (TMaybe (TVar "a")))) 1 True

--------------------------------------------------------------------------------
-- Type System
--------------------------------------------------------------------------------

-- | Type scheme (polymorphic types)
data TypeScheme = TScheme [TypeVar] ExprType
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Type error
data TypeError = TypeError
  { teMessage :: Text
  , teExpected :: Maybe ExprType
  , teActual :: Maybe ExprType
  , teLocation :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Type environment
type TypeEnv = Map Text TypeScheme

--------------------------------------------------------------------------------
-- Evaluation Context
--------------------------------------------------------------------------------

-- | Evaluation environment
data EvalEnv = EvalEnv
  { eeBindings :: Map Text Value
  , eeMatchContext :: MatchContext
  , eeTypeInfo :: Map Text Text
  , eeRecursionDepth :: Int
  , eeMaxRecursion :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Empty evaluation environment
emptyEvalEnv :: EvalEnv
emptyEvalEnv = EvalEnv
  { eeBindings = Map.empty
  , eeMatchContext = emptyMatchContext
  , eeTypeInfo = Map.empty
  , eeRecursionDepth = 0
  , eeMaxRecursion = 100
  }

-- | Evaluation error
data EvalError = EvalError
  { evMessage :: Text
  , evLocation :: Maybe Text
  , evStackTrace :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

--------------------------------------------------------------------------------
-- Pattern Context
--------------------------------------------------------------------------------

-- | Context from pattern matching
data MatchContext = MatchContext
  { mcFilePath :: Text
  , mcModuleName :: Text
  , mcLineNumber :: Int
  , mcColumnNumber :: Int
  , mcMatchedText :: Text
  , mcMetavars :: Map Text Text
  , mcImports :: [Text]
  , mcExports :: [Text]
  , mcPragmas :: [Text]
  , mcFreeVars :: Map Text (Set Text)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Empty match context
emptyMatchContext :: MatchContext
emptyMatchContext = MatchContext
  { mcFilePath = ""
  , mcModuleName = ""
  , mcLineNumber = 0
  , mcColumnNumber = 0
  , mcMatchedText = ""
  , mcMetavars = Map.empty
  , mcImports = []
  , mcExports = []
  , mcPragmas = []
  , mcFreeVars = Map.empty
  }
