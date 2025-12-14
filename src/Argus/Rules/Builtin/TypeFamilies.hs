{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.TypeFamilies
-- Description : Type family and type-level programming rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for type families, associated types, data families,
-- and type-level programming patterns.
--
-- == Rule Categories
--
-- * __Type Families__: Type family declarations
-- * __Data Families__: Data family patterns
-- * __Type Equality__: Type equality constraints
-- * __Type Operators__: Type-level operators

module Argus.Rules.Builtin.TypeFamilies
  ( -- * Rule Sets
    typeFamilyRules
  , typeFamDeclRules
  , dataFamRules
  , typeEqualityRules
  , typeOperatorRules

    -- * Type Family Declaration Rules
  , closedTypeFam
  , openTypeFam
  , injTypeFam
  , associatedTypeFam
  , typeFamDefault
  , typeFamKindSig
  , typeFamOverlap
  , typeSynFam

    -- * Data Family Rules
  , dataFamDecl
  , dataFamInstance
  , newtypeInstance
  , dataFamGADT
  , dataFamDefault
  , dataFamAssoc

    -- * Type Equality Rules
  , typeEquality
  , coercible
  , typeableConstraint
  , eqTilde
  , heteroEquality
  , proxyType

    -- * Type Operator Rules
  , typeAppOp
  , kindSignature
  , polyKinds
  , typeInType
  , starKind
  , constraintKind

    -- * Rule Count
  , typeFamilyRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All type family rules.
typeFamilyRules :: [Rule]
typeFamilyRules = mconcat
  [ typeFamDeclRules
  , dataFamRules
  , typeEqualityRules
  , typeOperatorRules
  ]

-- | Total count of type family rules.
typeFamilyRuleCount :: Int
typeFamilyRuleCount = length typeFamilyRules

--------------------------------------------------------------------------------
-- Type Family Declaration Rules
--------------------------------------------------------------------------------

-- | Rules for type family declarations.
typeFamDeclRules :: [Rule]
typeFamDeclRules =
  [ closedTypeFam
  , openTypeFam
  , injTypeFam
  , associatedTypeFam
  , typeFamDefault
  , typeFamKindSig
  , typeFamOverlap
  , typeSynFam
  ]

-- | Closed type family.
--
-- @
-- type family F a where ...  -- Closed, all cases defined
-- @
closedTypeFam :: Rule
closedTypeFam =
  rule "closed-type-family" $
    matchText "type family.*where"
    & category Style
    & severity Info
    & message "Closed type family - all cases must be defined here"
    & safetyLevel ManualReview

-- | Open type family.
--
-- @
-- type family F a  -- Open, instances elsewhere
-- @
openTypeFam :: Rule
openTypeFam =
  rule "open-type-family" $
    matchText "^type family [A-Z]"
    & category Style
    & severity Info
    & message "Open type family - can be extended with type instances"
    & safetyLevel ManualReview

-- | Injective type family.
--
-- @
-- type family F a = r | r -> a  -- Injective
-- @
injTypeFam :: Rule
injTypeFam =
  rule "injective-type-family" $
    matchText "type family.*\\|.*->"
    & category Style
    & severity Info
    & message "Injective type family - result determines arguments"
    & safetyLevel ManualReview

-- | Associated type family.
--
-- @
-- class C a where
--   type T a  -- Associated type
-- @
associatedTypeFam :: Rule
associatedTypeFam =
  rule "associated-type-family" $
    matchText "class.*where[\\s\\S]*type [A-Z]"
    & category Style
    & severity Info
    & message "Associated type family in class"
    & safetyLevel ManualReview

-- | Type family default.
--
-- @
-- type T a = DefaultType  -- Default instance
-- @
typeFamDefault :: Rule
typeFamDefault =
  rule "type-family-default" $
    matchText "type instance.*="
    & category Style
    & severity Info
    & message "Type family instance"
    & safetyLevel ManualReview

-- | Type family kind signature.
--
-- @
-- type family F (a :: k) :: Type  -- Explicit kind
-- @
typeFamKindSig :: Rule
typeFamKindSig =
  rule "type-family-kind-sig" $
    matchText "type family.*::"
    & category Style
    & severity Info
    & message "Type family with explicit kind signature - good practice"
    & safetyLevel ManualReview

-- | Overlapping type family instances.
--
-- @
-- type instance F [a] = ...
-- type instance F (Maybe a) = ...
-- @
typeFamOverlap :: Rule
typeFamOverlap =
  rule "type-family-overlap" $
    matchText "type instance F"
    & category Style
    & severity Info
    & message "Type family instance - ensure no overlaps with other instances"
    & safetyLevel ManualReview

-- | Type synonym family (deprecated).
--
-- @
-- type family F a :: *  -- Using old * syntax
-- @
typeSynFam :: Rule
typeSynFam =
  rule "type-syn-family" $
    matchText "type family.*:: \\*"
    & category Modernization
    & severity Suggestion
    & message "Consider using Type instead of * for kinds"

--------------------------------------------------------------------------------
-- Data Family Rules
--------------------------------------------------------------------------------

-- | Rules for data family patterns.
dataFamRules :: [Rule]
dataFamRules =
  [ dataFamDecl
  , dataFamInstance
  , newtypeInstance
  , dataFamGADT
  , dataFamDefault
  , dataFamAssoc
  ]

-- | Data family declaration.
--
-- @
-- data family D a  -- Data family
-- @
dataFamDecl :: Rule
dataFamDecl =
  rule "data-family-decl" $
    matchText "^data family"
    & category Style
    & severity Info
    & message "Data family declaration"
    & safetyLevel ManualReview

-- | Data family instance.
--
-- @
-- data instance D Int = DInt Int  -- Instance
-- @
dataFamInstance :: Rule
dataFamInstance =
  rule "data-family-instance" $
    matchText "^data instance"
    & category Style
    & severity Info
    & message "Data family instance"
    & safetyLevel ManualReview

-- | Newtype family instance.
--
-- @
-- newtype instance D Char = DChar Char  -- Newtype instance
-- @
newtypeInstance :: Rule
newtypeInstance =
  rule "newtype-instance" $
    matchText "^newtype instance"
    & category Style
    & severity Info
    & message "Newtype data family instance - more efficient"
    & safetyLevel ManualReview

-- | GADT-style data family.
--
-- @
-- data instance D a where ...  -- GADT syntax
-- @
dataFamGADT :: Rule
dataFamGADT =
  rule "data-family-gadt" $
    matchText "data instance.*where"
    & category Style
    & severity Info
    & message "GADT-style data family instance"
    & safetyLevel ManualReview

-- | Data family default.
--
-- @
-- class C a where
--   data D a = DefaultD  -- Default
-- @
dataFamDefault :: Rule
dataFamDefault =
  rule "data-family-default" $
    matchText "data.*=.*Default"
    & category Style
    & severity Info
    & message "Data family with default - provides fallback"
    & safetyLevel ManualReview

-- | Associated data family.
--
-- @
-- class C a where
--   data D a  -- Associated data
-- @
dataFamAssoc :: Rule
dataFamAssoc =
  rule "data-family-assoc" $
    matchText "class.*where[\\s\\S]*data [A-Z]"
    & category Style
    & severity Info
    & message "Associated data family in class"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Type Equality Rules
--------------------------------------------------------------------------------

-- | Rules for type equality.
typeEqualityRules :: [Rule]
typeEqualityRules =
  [ typeEquality
  , coercible
  , typeableConstraint
  , eqTilde
  , heteroEquality
  , proxyType
  ]

-- | Type equality constraint.
--
-- @
-- f :: (a ~ b) => ...  -- Type equality
-- @
typeEquality :: Rule
typeEquality =
  rule "type-equality" $
    matchText "\\([^)]*~[^)]*\\)\\s*=>"
    & category Style
    & severity Info
    & message "Type equality constraint"
    & safetyLevel ManualReview

-- | Coercible constraint.
--
-- @
-- f :: Coercible a b => ...  -- Safe coercion
-- @
coercible :: Rule
coercible =
  rule "coercible" $
    matchText "Coercible"
    & category Style
    & severity Info
    & message "Using Coercible for safe representational coercion"
    & safetyLevel ManualReview

-- | Typeable constraint.
--
-- @
-- f :: Typeable a => ...  -- Runtime type info
-- @
typeableConstraint :: Rule
typeableConstraint =
  rule "typeable-constraint" $
    matchText "Typeable"
    & category Style
    & severity Info
    & message "Typeable constraint - enables runtime type inspection"
    & safetyLevel ManualReview

-- | Type equality with tilde.
--
-- @
-- type F a b = a ~ b  -- Type-level equality
-- @
eqTilde :: Rule
eqTilde =
  rule "eq-tilde" $
    matchText "type.*=.*~"
    & category Style
    & severity Info
    & message "Type-level equality"
    & safetyLevel ManualReview

-- | Heterogeneous equality.
--
-- @
-- f :: (a :~: b) -> ...  -- Type equality proof
-- @
heteroEquality :: Rule
heteroEquality =
  rule "hetero-equality" $
    matchText ":~:|:~~:"
    & category Style
    & severity Info
    & message "Using type equality evidence"
    & safetyLevel ManualReview

-- | Proxy type usage.
--
-- @
-- f :: Proxy a -> ...  -- Type-level proxy
-- @
proxyType :: Rule
proxyType =
  rule "proxy-type" $
    matchText "Proxy\\s+[a-z]"
    & category Modernization
    & severity Suggestion
    & message "Consider TypeApplications instead of Proxy"

--------------------------------------------------------------------------------
-- Type Operator Rules
--------------------------------------------------------------------------------

-- | Rules for type operators.
typeOperatorRules :: [Rule]
typeOperatorRules =
  [ typeAppOp
  , kindSignature
  , polyKinds
  , typeInType
  , starKind
  , constraintKind
  ]

-- | Type application operator.
--
-- @
-- f @Int  -- Type application
-- @
typeAppOp :: Rule
typeAppOp =
  rule "type-app-op" $
    matchText "@[A-Z]"
    & category Style
    & severity Info
    & message "Using TypeApplications"
    & safetyLevel ManualReview

-- | Kind signature.
--
-- @
-- type F :: Type -> Type  -- Standalone kind sig
-- @
kindSignature :: Rule
kindSignature =
  rule "kind-signature" $
    matchText "^type [A-Z].*::"
    & category Style
    & severity Info
    & message "Standalone kind signature"
    & safetyLevel ManualReview

-- | PolyKinds usage.
--
-- @
-- {-# LANGUAGE PolyKinds #-}
-- @
polyKinds :: Rule
polyKinds =
  rule "poly-kinds" $
    matchText "PolyKinds"
    & category Style
    & severity Info
    & message "PolyKinds enables kind polymorphism"
    & safetyLevel ManualReview

-- | TypeInType usage (deprecated).
--
-- @
-- {-# LANGUAGE TypeInType #-}  -- Deprecated
-- @
typeInType :: Rule
typeInType =
  rule "type-in-type" $
    matchText "TypeInType"
    & category Modernization
    & severity Suggestion
    & message "TypeInType is deprecated - use PolyKinds and DataKinds instead"

-- | Star kind (old syntax).
--
-- @
-- :: * -> *  -- Old kind syntax
-- @
starKind :: Rule
starKind =
  rule "star-kind" $
    matchText "::\\s*\\*"
    & category Modernization
    & severity Suggestion
    & message "Consider using Type instead of * for kinds"

-- | Constraint kind.
--
-- @
-- type F :: Constraint  -- Constraint kind
-- @
constraintKind :: Rule
constraintKind =
  rule "constraint-kind" $
    matchText ":: Constraint"
    & category Style
    & severity Info
    & message "Using Constraint kind for type-level constraints"
    & safetyLevel ManualReview
