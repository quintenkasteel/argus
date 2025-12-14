{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.GADTs
-- Description : GADTs and type-level programming rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for GADTs, existential types, rank-N types,
-- and advanced type system features.
--
-- == Rule Categories
--
-- * __GADTs__: GADT declarations and patterns
-- * __Existentials__: Existential types
-- * __RankN__: Higher-rank types
-- * __TypeLevel__: Type-level programming

module Argus.Rules.Builtin.GADTs
  ( -- * Rule Sets
    gadtRules
  , gadtDeclRules
  , existentialRules
  , rankNRules
  , typeLevelRules

    -- * GADT Declaration Rules
  , gadtSyntax
  , gadtPattern
  , gadtReturn
  , gadtEquality
  , gadtScrutinee
  , strictGADT

    -- * Existential Rules
  , existentialData
  , existentialRecord
  , existentialConstraint
  , someType
  , existentialUnpack
  , existentialSkolem

    -- * Rank-N Rules
  , rankNType
  , impredicative
  , stPolymorphism
  , runSTPattern
  , forallScope
  , quantifiedConstraint

    -- * Type-Level Rules
  , singletons
  , promotedData
  , promotedList
  , symbolKind
  , natKind
  , typeNats

    -- * Rule Count
  , gadtRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All GADT-related rules.
gadtRules :: [Rule]
gadtRules = mconcat
  [ gadtDeclRules
  , existentialRules
  , rankNRules
  , typeLevelRules
  ]

-- | Total count of GADT rules.
gadtRuleCount :: Int
gadtRuleCount = length gadtRules

--------------------------------------------------------------------------------
-- GADT Declaration Rules
--------------------------------------------------------------------------------

-- | Rules for GADT declarations.
gadtDeclRules :: [Rule]
gadtDeclRules =
  [ gadtSyntax
  , gadtPattern
  , gadtReturn
  , gadtEquality
  , gadtScrutinee
  , strictGADT
  ]

-- | GADT syntax.
--
-- @
-- data T a where
--   C :: Int -> T Int
-- @
gadtSyntax :: Rule
gadtSyntax =
  rule "gadt-syntax" $
    matchText "^data [A-Z].*where$"
    & category Style
    & severity Info
    & message "GADT declaration with explicit constructor types"
    & safetyLevel ManualReview

-- | GADT pattern matching.
--
-- @
-- case x of
--   C n -> ...  -- Type refinement
-- @
gadtPattern :: Rule
gadtPattern =
  rule "gadt-pattern" $
    matchText "case.*of[\\s\\S]*::"
    & category Style
    & severity Info
    & message "Pattern matching on GADT - type refinement occurs"
    & safetyLevel ManualReview

-- | GADT return type.
--
-- @
-- C :: a -> T a  -- Polymorphic return
-- @
gadtReturn :: Rule
gadtReturn =
  rule "gadt-return" $
    matchText "[A-Z][a-zA-Z]*\\s*::"
    & category Style
    & severity Info
    & message "GADT constructor with explicit return type"
    & safetyLevel ManualReview

-- | GADT type equality.
--
-- @
-- C :: (a ~ Int) => T a  -- Equality constraint
-- @
gadtEquality :: Rule
gadtEquality =
  rule "gadt-equality" $
    matchText "[A-Z][a-zA-Z]*\\s*::.*~.*=>"
    & category Style
    & severity Info
    & message "GADT with type equality constraint"
    & safetyLevel ManualReview

-- | GADT scrutinee.
--
-- @
-- f :: T a -> a
-- f (C n) = n  -- Safe due to refinement
-- @
gadtScrutinee :: Rule
gadtScrutinee =
  rule "gadt-scrutinee" $
    matchText "^[a-z].*:: [A-Z].*->.*$"
    & category Style
    & severity Info
    & message "Function on GADT - ensure pattern coverage"
    & safetyLevel ManualReview

-- | Strict GADT fields.
--
-- @
-- C :: !Int -> T Int  -- Strict field
-- @
strictGADT :: Rule
strictGADT =
  rule "strict-gadt" $
    matchText "[A-Z][a-zA-Z]*\\s*::.*!"
    & category Performance
    & severity Info
    & message "Strict field in GADT constructor"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Existential Rules
--------------------------------------------------------------------------------

-- | Rules for existential types.
existentialRules :: [Rule]
existentialRules =
  [ existentialData
  , existentialRecord
  , existentialConstraint
  , someType
  , existentialUnpack
  , existentialSkolem
  ]

-- | Existential data type.
--
-- @
-- data T = forall a. C a  -- Existential
-- @
existentialData :: Rule
existentialData =
  rule "existential-data" $
    matchText "data.*= forall"
    & category Style
    & severity Info
    & message "Existential type - type hidden from outside"
    & safetyLevel ManualReview

-- | Existential with record.
--
-- @
-- data T = forall a. C { val :: a }  -- Existential record
-- @
existentialRecord :: Rule
existentialRecord =
  rule "existential-record" $
    matchText "forall.*\\{.*::"
    & category Style
    & severity Warning
    & message "Existential with record - field selector is partial"

-- | Existential with constraint.
--
-- @
-- data T = forall a. Show a => C a  -- With constraint
-- @
existentialConstraint :: Rule
existentialConstraint =
  rule "existential-constraint" $
    matchText "forall [a-z]+\\..*=>"
    & category Style
    & severity Info
    & message "Existential with constraint - enables operations on hidden type"
    & safetyLevel ManualReview

-- | Some type pattern.
--
-- @
-- data Some f = forall a. Some (f a)  -- Wrapper
-- @
someType :: Rule
someType =
  rule "some-type" $
    matchText "data Some"
    & category Style
    & severity Info
    & message "Some pattern for existential wrapper"
    & safetyLevel ManualReview

-- | Existential unpack.
--
-- @
-- case t of C x -> ...  -- x has skolem type
-- @
existentialUnpack :: Rule
existentialUnpack =
  rule "existential-unpack" $
    matchText "case.*of.*forall"
    & category Style
    & severity Info
    & message "Unpacking existential - type is skolem bound"
    & safetyLevel ManualReview

-- | Skolem escape.
--
-- @
-- f (C x) = x  -- Error: skolem escapes
-- @
existentialSkolem :: Rule
existentialSkolem =
  rule "existential-skolem" $
    matchText ":: forall.*\\. [a-z]$"
    & category Correctness
    & severity Warning
    & message "Potential skolem escape - existential type escaping scope"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Rank-N Rules
--------------------------------------------------------------------------------

-- | Rules for higher-rank types.
rankNRules :: [Rule]
rankNRules =
  [ rankNType
  , impredicative
  , stPolymorphism
  , runSTPattern
  , forallScope
  , quantifiedConstraint
  ]

-- | Rank-N type.
--
-- @
-- f :: (forall a. a -> a) -> Int  -- Rank-2
-- @
rankNType :: Rule
rankNType =
  rule "rank-n-type" $
    matchText ":: \\(forall"
    & category Style
    & severity Info
    & message "Rank-N type - function takes polymorphic argument"
    & safetyLevel ManualReview

-- | Impredicative types.
--
-- @
-- f :: [forall a. a -> a]  -- Impredicative
-- @
impredicative :: Rule
impredicative =
  rule "impredicative" $
    matchText "\\[forall"
    & category Style
    & severity Warning
    & message "Impredicative type - may not work without ImpredicativeTypes"
    & safetyLevel ManualReview

-- | ST polymorphism.
--
-- @
-- runST :: (forall s. ST s a) -> a  -- ST trick
-- @
stPolymorphism :: Rule
stPolymorphism =
  rule "st-polymorphism" $
    matchText "forall s\\. ST s"
    & category Style
    & severity Info
    & message "ST polymorphism pattern - ensures state doesn't escape"
    & safetyLevel ManualReview

-- | runST pattern.
--
-- @
-- runST $ do ...  -- Safe state
-- @
runSTPattern :: Rule
runSTPattern =
  rule "runST-pattern" $
    match ("runST _action" ==> "runST _action")
    & category Style
    & severity Info
    & message "Using runST for pure mutable state"
    & safetyLevel ManualReview

-- | Forall scope.
--
-- @
-- f :: forall a. a -> forall b. b -> (a, b)
-- @
forallScope :: Rule
forallScope =
  rule "forall-scope" $
    matchText "forall.*forall"
    & category Style
    & severity Info
    & message "Multiple foralls - check scoping is intended"
    & safetyLevel ManualReview

-- | Quantified constraint.
--
-- @
-- f :: (forall a. C a) => ...  -- Quantified constraint
-- @
quantifiedConstraint :: Rule
quantifiedConstraint =
  rule "quantified-constraint" $
    matchText "\\(forall.*=>\\)\\s*=>"
    & category Style
    & severity Info
    & message "Quantified constraint - requires QuantifiedConstraints"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Type-Level Rules
--------------------------------------------------------------------------------

-- | Rules for type-level programming.
typeLevelRules :: [Rule]
typeLevelRules =
  [ singletons
  , promotedData
  , promotedList
  , symbolKind
  , natKind
  , typeNats
  ]

-- | Singletons pattern.
--
-- @
-- data SBool (b :: Bool) where ...  -- Singleton
-- @
singletons :: Rule
singletons =
  rule "singletons" $
    matchText "data S[A-Z].*::.*where"
    & category Style
    & severity Info
    & message "Singleton type pattern"
    & safetyLevel ManualReview

-- | Promoted data constructor.
--
-- @
-- type F = 'True  -- Promoted Bool
-- @
promotedData :: Rule
promotedData =
  rule "promoted-data" $
    matchText "type.*= '[A-Z]"
    & category Style
    & severity Info
    & message "Using promoted data constructor"
    & safetyLevel ManualReview

-- | Promoted list.
--
-- @
-- type Xs = '[Int, Bool]  -- Type-level list
-- @
promotedList :: Rule
promotedList =
  rule "promoted-list" $
    matchText "'\\["
    & category Style
    & severity Info
    & message "Using promoted list at type level"
    & safetyLevel ManualReview

-- | Symbol kind.
--
-- @
-- type F :: Symbol -> Type  -- Type-level string
-- @
symbolKind :: Rule
symbolKind =
  rule "symbol-kind" $
    matchText ":: Symbol"
    & category Style
    & severity Info
    & message "Using Symbol kind for type-level strings"
    & safetyLevel ManualReview

-- | Nat kind.
--
-- @
-- type F :: Nat -> Type  -- Type-level natural
-- @
natKind :: Rule
natKind =
  rule "nat-kind" $
    matchText ":: Nat"
    & category Style
    & severity Info
    & message "Using Nat kind for type-level naturals"
    & safetyLevel ManualReview

-- | Type-level naturals.
--
-- @
-- type N = 42  -- Type-level number
-- @
typeNats :: Rule
typeNats =
  rule "type-nats" $
    matchText "type.*= [0-9]+"
    & category Style
    & severity Info
    & message "Using type-level natural number"
    & safetyLevel ManualReview
