{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module GADTsExamples where

import Data.Kind (Type)

--------------------------------------------------------------------------------
-- Missing standalone deriving
--------------------------------------------------------------------------------

-- Should suggest: add standalone deriving for Show
data Expr a where
  LitInt  :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add     :: Expr Int -> Expr Int -> Expr Int
  If      :: Expr Bool -> Expr a -> Expr a -> Expr a

-- Good: has standalone deriving
data ExprGood a where
  LitIntG  :: Int -> ExprGood Int
  LitBoolG :: Bool -> ExprGood Bool
  AddG     :: ExprGood Int -> ExprGood Int -> ExprGood Int
  IfG      :: ExprGood Bool -> ExprGood a -> ExprGood a -> ExprGood a

deriving instance Show (ExprGood a)

--------------------------------------------------------------------------------
-- Non-exhaustive GADT patterns
--------------------------------------------------------------------------------

-- Should warn: non-exhaustive pattern match
eval :: Expr a -> a
eval (LitInt n) = n
eval (LitBool b) = b
eval (Add e1 e2) = eval e1 + eval e2
-- Missing: If case

-- Good: exhaustive
evalGood :: ExprGood a -> a
evalGood (LitIntG n) = n
evalGood (LitBoolG b) = b
evalGood (AddG e1 e2) = evalGood e1 + evalGood e2
evalGood (IfG c t e) = if evalGood c then evalGood t else evalGood e

--------------------------------------------------------------------------------
-- Type witness not used
--------------------------------------------------------------------------------

data TypeRep a where
  IntRep  :: TypeRep Int
  BoolRep :: TypeRep Bool
  ListRep :: TypeRep a -> TypeRep [a]

-- Should warn: type witness not used to refine type
unusedWitness :: TypeRep a -> a -> String
unusedWitness _ x = "value"  -- Ignores the type information

-- Good: uses type witness
usedWitness :: TypeRep a -> a -> String
usedWitness IntRep n = "Int: " ++ show n
usedWitness BoolRep b = "Bool: " ++ show b
usedWitness (ListRep _) xs = "List"

--------------------------------------------------------------------------------
-- GADT could be simpler ADT
--------------------------------------------------------------------------------

-- Should suggest: simple ADT would suffice (all constructors same type)
data SimpleGADT a where
  MkSimple1 :: Int -> SimpleGADT Int
  MkSimple2 :: Int -> SimpleGADT Int
  MkSimple3 :: Int -> SimpleGADT Int

-- Good: actually uses GADT capabilities
data ProperGADT a where
  ProperInt  :: Int -> ProperGADT Int
  ProperBool :: Bool -> ProperGADT Bool
  ProperList :: [a] -> ProperGADT [a]

--------------------------------------------------------------------------------
-- Missing kind signature
--------------------------------------------------------------------------------

-- Should suggest: add kind signature for clarity
data HList xs where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- Good: explicit kind signature
data HListGood (xs :: [Type]) where
  HNilG  :: HListGood '[]
  HConsG :: x -> HListGood xs -> HListGood (x ': xs)

--------------------------------------------------------------------------------
-- Redundant type equality constraint
--------------------------------------------------------------------------------

-- Should warn: redundant type equality (always satisfied)
data Foo a where
  MkFoo :: (a ~ a) => a -> Foo a  -- a ~ a is always true

-- Good: no redundant constraint
data FooGood a where
  MkFooGood :: a -> FooGood a

--------------------------------------------------------------------------------
-- Existential could use existential syntax
--------------------------------------------------------------------------------

-- Should suggest: consider ExistentialQuantification syntax
data SomeShow where
  MkSomeShow :: Show a => a -> SomeShow

-- This is fine - GADT syntax is often clearer

--------------------------------------------------------------------------------
-- Pattern match could be simplified
--------------------------------------------------------------------------------

-- Should suggest: combine patterns with same RHS
processExpr :: Expr a -> String
processExpr (LitInt _) = "literal"
processExpr (LitBool _) = "literal"  -- Same as above
processExpr (Add _ _) = "binary op"
processExpr (If _ _ _) = "conditional"

-- Good: would need type class or different approach
processExprGood :: ExprGood a -> String
processExprGood (LitIntG n) = "int: " ++ show n
processExprGood (LitBoolG b) = "bool: " ++ show b
processExprGood (AddG _ _) = "addition"
processExprGood (IfG _ _ _) = "conditional"

--------------------------------------------------------------------------------
-- Unsafe coercion in GADT
--------------------------------------------------------------------------------

-- Should error: unsafe type coercion
data Coerce a b where
  UnsafeCoerce :: a -> Coerce a b  -- Can extract as any type!

-- Good: proper type-safe GADT
data SafeCoerce a b where
  Refl :: SafeCoerce a a  -- Only allows same-type coercion
