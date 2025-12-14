{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Deriving
-- Description : Deriving strategies and instance rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for deriving strategies, automatic instance generation,
-- standalone deriving, and related patterns.
--
-- == Rule Categories
--
-- * __Strategies__: Deriving strategy selection
-- * __Stock__: Stock derivable classes
-- * __Newtype__: Newtype deriving
-- * __Via__: DerivingVia patterns
-- * __Standalone__: Standalone deriving

module Argus.Rules.Builtin.Deriving
  ( -- * Rule Sets
    derivingRules
  , strategyRules
  , stockRules
  , newtypeDerivingRules
  , viaRules
  , standaloneRules

    -- * Strategy Rules
  , missingStrategy
  , stockStrategy
  , newtypeStrategy
  , anyclassStrategy
  , viaStrategy
  , defaultStrategy

    -- * Stock Derivable Rules
  , deriveEq
  , deriveOrd
  , deriveShow
  , deriveRead
  , deriveBounded
  , deriveEnum
  , deriveIx
  , deriveFunctor
  , deriveFoldable
  , deriveTraversable
  , deriveGeneric
  , deriveData
  , deriveLift

    -- * Newtype Deriving Rules
  , derivingNewtypeDeriving
  , newtypeNumeric
  , newtypeMonoid
  , newtypeCoercible
  , unsafeNewtypeDeriving

    -- * DerivingVia Rules
  , derivingVia
  , viaNewtype
  , viaMonoid
  , viaCompose
  , viaConst

    -- * Standalone Rules
  , standaloneDeriving
  , orphanDeriving
  , derivingFlexible

    -- * Rule Count
  , derivingRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All deriving-related rules.
derivingRules :: [Rule]
derivingRules = mconcat
  [ strategyRules
  , stockRules
  , newtypeDerivingRules
  , viaRules
  , standaloneRules
  ]

-- | Total count of deriving rules.
derivingRuleCount :: Int
derivingRuleCount = length derivingRules

--------------------------------------------------------------------------------
-- Strategy Rules
--------------------------------------------------------------------------------

-- | Rules for deriving strategy selection.
strategyRules :: [Rule]
strategyRules =
  [ missingStrategy
  , stockStrategy
  , newtypeStrategy
  , anyclassStrategy
  , viaStrategy
  , defaultStrategy
  ]

-- | Missing deriving strategy.
--
-- @
-- deriving (Eq, Show)  -- No strategy specified
-- @
missingStrategy :: Rule
missingStrategy =
  rule "missing-deriving-strategy" $
    matchText "deriving\\s*\\([^)]+\\)$"
    & category Style
    & severity Suggestion
    & message "Consider explicit deriving strategy"
    & note "Use DerivingStrategies extension"

-- | Stock deriving strategy.
--
-- @
-- deriving stock (Eq, Show)  -- Compiler generated
-- @
stockStrategy :: Rule
stockStrategy =
  rule "stock-strategy" $
    matchText "deriving stock"
    & category Style
    & severity Info
    & message "Using stock deriving - compiler-generated instances"
    & safetyLevel ManualReview

-- | Newtype deriving strategy.
--
-- @
-- deriving newtype Num  -- Through coercion
-- @
newtypeStrategy :: Rule
newtypeStrategy =
  rule "newtype-strategy" $
    matchText "deriving newtype"
    & category Style
    & severity Info
    & message "Using newtype deriving - coerced from underlying type"
    & safetyLevel ManualReview

-- | Anyclass deriving strategy.
--
-- @
-- deriving anyclass ToJSON  -- Default methods
-- @
anyclassStrategy :: Rule
anyclassStrategy =
  rule "anyclass-strategy" $
    matchText "deriving anyclass"
    & category Style
    & severity Info
    & message "Using anyclass deriving - uses default method implementations"
    & safetyLevel ManualReview

-- | Via deriving strategy.
--
-- @
-- deriving (Semigroup) via (Sum Int)  -- Via another type
-- @
viaStrategy :: Rule
viaStrategy =
  rule "via-strategy" $
    matchText "deriving.*via"
    & category Style
    & severity Info
    & message "Using DerivingVia for instance via another type"
    & safetyLevel ManualReview

-- | Default deriving.
--
-- @
-- deriving (Eq)  -- Relies on defaults
-- @
defaultStrategy :: Rule
defaultStrategy =
  rule "default-deriving-strategy" $
    matchText "^\\s+deriving\\s+\\("
    & category Style
    & severity Info
    & message "Deriving without explicit strategy"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Stock Derivable Rules
--------------------------------------------------------------------------------

-- | Rules for stock derivable classes.
stockRules :: [Rule]
stockRules =
  [ deriveEq
  , deriveOrd
  , deriveShow
  , deriveRead
  , deriveBounded
  , deriveEnum
  , deriveIx
  , deriveFunctor
  , deriveFoldable
  , deriveTraversable
  , deriveGeneric
  , deriveData
  , deriveLift
  ]

-- | Deriving Eq.
--
-- @
-- deriving Eq  -- Structural equality
-- @
deriveEq :: Rule
deriveEq =
  rule "derive-eq" $
    matchText "deriving.*\\bEq\\b"
    & category Style
    & severity Info
    & message "Deriving Eq - structural equality"
    & safetyLevel ManualReview

-- | Deriving Ord.
--
-- @
-- deriving Ord  -- Lexicographic ordering
-- @
deriveOrd :: Rule
deriveOrd =
  rule "derive-ord" $
    matchText "deriving.*\\bOrd\\b"
    & category Style
    & severity Info
    & message "Deriving Ord - lexicographic ordering by constructor"
    & safetyLevel ManualReview

-- | Deriving Show.
--
-- @
-- deriving Show  -- String representation
-- @
deriveShow :: Rule
deriveShow =
  rule "derive-show" $
    matchText "deriving.*\\bShow\\b"
    & category Style
    & severity Info
    & message "Deriving Show"
    & safetyLevel ManualReview

-- | Deriving Read.
--
-- @
-- deriving Read  -- Parser
-- @
deriveRead :: Rule
deriveRead =
  rule "derive-read" $
    matchText "deriving.*\\bRead\\b"
    & category Style
    & severity Suggestion
    & message "Deriving Read - consider if parsing is really needed"
    & note "Read instances can be fragile with show/read roundtrips"

-- | Deriving Bounded.
--
-- @
-- deriving Bounded  -- Min/max bounds
-- @
deriveBounded :: Rule
deriveBounded =
  rule "derive-bounded" $
    matchText "deriving.*\\bBounded\\b"
    & category Style
    & severity Info
    & message "Deriving Bounded - for enumeration types"
    & safetyLevel ManualReview

-- | Deriving Enum.
--
-- @
-- deriving Enum  -- Enumerable
-- @
deriveEnum :: Rule
deriveEnum =
  rule "derive-enum" $
    matchText "deriving.*\\bEnum\\b"
    & category Style
    & severity Info
    & message "Deriving Enum - enables [..] syntax"
    & safetyLevel ManualReview

-- | Deriving Ix.
--
-- @
-- deriving Ix  -- Array indexing
-- @
deriveIx :: Rule
deriveIx =
  rule "derive-ix" $
    matchText "deriving.*\\bIx\\b"
    & category Style
    & severity Info
    & message "Deriving Ix - for array indexing"
    & safetyLevel ManualReview

-- | Deriving Functor.
--
-- @
-- deriving Functor  -- Map over type parameter
-- @
deriveFunctor :: Rule
deriveFunctor =
  rule "derive-functor" $
    matchText "deriving.*\\bFunctor\\b"
    & category Style
    & severity Info
    & message "Deriving Functor - requires DeriveFunctor"
    & safetyLevel ManualReview

-- | Deriving Foldable.
--
-- @
-- deriving Foldable  -- Folding structure
-- @
deriveFoldable :: Rule
deriveFoldable =
  rule "derive-foldable" $
    matchText "deriving.*\\bFoldable\\b"
    & category Style
    & severity Info
    & message "Deriving Foldable - requires DeriveFoldable"
    & safetyLevel ManualReview

-- | Deriving Traversable.
--
-- @
-- deriving Traversable  -- Effectful traversal
-- @
deriveTraversable :: Rule
deriveTraversable =
  rule "derive-traversable" $
    matchText "deriving.*\\bTraversable\\b"
    & category Style
    & severity Info
    & message "Deriving Traversable - requires DeriveTraversable"
    & safetyLevel ManualReview

-- | Deriving Generic.
--
-- @
-- deriving Generic  -- Generic representation
-- @
deriveGeneric :: Rule
deriveGeneric =
  rule "derive-generic" $
    matchText "deriving.*\\bGeneric\\b"
    & category Style
    & severity Info
    & message "Deriving Generic - enables generic programming"
    & safetyLevel ManualReview

-- | Deriving Data.
--
-- @
-- deriving Data  -- SYB support
-- @
deriveData :: Rule
deriveData =
  rule "derive-data" $
    matchText "deriving.*\\bData\\b"
    & category Style
    & severity Info
    & message "Deriving Data - enables SYB generic operations"
    & safetyLevel ManualReview

-- | Deriving Lift.
--
-- @
-- deriving Lift  -- Template Haskell quotation
-- @
deriveLift :: Rule
deriveLift =
  rule "derive-lift" $
    matchText "deriving.*\\bLift\\b"
    & category Style
    & severity Info
    & message "Deriving Lift - for Template Haskell quotation"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Newtype Deriving Rules
--------------------------------------------------------------------------------

-- | Rules for newtype deriving.
newtypeDerivingRules :: [Rule]
newtypeDerivingRules =
  [ derivingNewtypeDeriving
  , newtypeNumeric
  , newtypeMonoid
  , newtypeCoercible
  , unsafeNewtypeDeriving
  ]

-- | Newtype deriving.
--
-- @
-- newtype Age = Age Int
--   deriving newtype Num
-- @
derivingNewtypeDeriving :: Rule
derivingNewtypeDeriving =
  rule "newtype-deriving" $
    matchText "newtype.*deriving"
    & category Style
    & severity Info
    & message "Newtype with deriving - consider which strategy"
    & safetyLevel ManualReview

-- | Newtype deriving numeric classes.
--
-- @
-- deriving newtype (Num, Real, Integral)
-- @
newtypeNumeric :: Rule
newtypeNumeric =
  rule "newtype-numeric" $
    matchText "newtype.*Num"
    & category Style
    & severity Info
    & message "Deriving numeric classes through newtype"
    & safetyLevel ManualReview

-- | Newtype deriving Monoid.
--
-- @
-- deriving newtype (Semigroup, Monoid)
-- @
newtypeMonoid :: Rule
newtypeMonoid =
  rule "newtype-monoid" $
    matchText "newtype.*Monoid|newtype.*Semigroup"
    & category Style
    & severity Info
    & message "Deriving Semigroup/Monoid through newtype"
    & safetyLevel ManualReview

-- | Newtype coercible.
--
-- @
-- coerce :: Age -> Int  -- Safe with newtype
-- @
newtypeCoercible :: Rule
newtypeCoercible =
  rule "newtype-coercible" $
    matchText "coerce.*::"
    & category Style
    & severity Info
    & message "Using coerce - ensure types are representationally equal"
    & safetyLevel ManualReview

-- | GeneralizedNewtypeDeriving risks.
--
-- @
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- @
unsafeNewtypeDeriving :: Rule
unsafeNewtypeDeriving =
  rule "unsafe-newtype-deriving" $
    matchText "GeneralizedNewtypeDeriving"
    & category Security
    & severity Info
    & message "GND can violate type abstractions"
    & note "Be careful with abstract types"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- DerivingVia Rules
--------------------------------------------------------------------------------

-- | Rules for DerivingVia patterns.
viaRules :: [Rule]
viaRules =
  [ derivingVia
  , viaNewtype
  , viaMonoid
  , viaCompose
  , viaConst
  ]

-- | DerivingVia extension.
--
-- @
-- {-# LANGUAGE DerivingVia #-}
-- @
derivingVia :: Rule
derivingVia =
  rule "deriving-via" $
    matchText "DerivingVia"
    & category Style
    & severity Info
    & message "DerivingVia enabled - powerful deriving mechanism"
    & safetyLevel ManualReview

-- | Via newtype wrapper.
--
-- @
-- deriving (Num) via (Sum Int)
-- @
viaNewtype :: Rule
viaNewtype =
  rule "via-newtype" $
    matchText "via \\([A-Z]"
    & category Style
    & severity Info
    & message "Deriving via newtype wrapper"
    & safetyLevel ManualReview

-- | Via monoid wrapper.
--
-- @
-- deriving Semigroup via (Dual a)
-- @
viaMonoid :: Rule
viaMonoid =
  rule "via-monoid" $
    matchText "via.*Dual|via.*Sum|via.*Product|via.*First|via.*Last"
    & category Style
    & severity Info
    & message "Deriving via standard monoid wrapper"
    & safetyLevel ManualReview

-- | Via Compose.
--
-- @
-- deriving Applicative via (Compose f g)
-- @
viaCompose :: Rule
viaCompose =
  rule "via-compose" $
    matchText "via.*Compose"
    & category Style
    & severity Info
    & message "Deriving via Compose - for composed functors"
    & safetyLevel ManualReview

-- | Via Const.
--
-- @
-- deriving Functor via (Const a)
-- @
viaConst :: Rule
viaConst =
  rule "via-const" $
    matchText "via.*Const"
    & category Style
    & severity Info
    & message "Deriving via Const - phantom type pattern"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Standalone Deriving Rules
--------------------------------------------------------------------------------

-- | Rules for standalone deriving.
standaloneRules :: [Rule]
standaloneRules =
  [ standaloneDeriving
  , orphanDeriving
  , derivingFlexible
  ]

-- | Standalone deriving.
--
-- @
-- deriving instance Show a => Show (T a)
-- @
standaloneDeriving :: Rule
standaloneDeriving =
  rule "standalone-deriving" $
    matchText "^deriving instance"
    & category Style
    & severity Info
    & message "Standalone deriving - useful for GADTs and orphans"
    & safetyLevel ManualReview

-- | Orphan deriving.
--
-- @
-- deriving instance Show ExternalType  -- Orphan!
-- @
orphanDeriving :: Rule
orphanDeriving =
  rule "orphan-deriving" $
    matchText "deriving instance.*[A-Z][a-zA-Z]*\\.[A-Z]"
    & category Style
    & severity Warning
    & message "Potential orphan deriving instance"
    & note "Consider newtype wrapper instead"

-- | Flexible deriving contexts.
--
-- @
-- deriving instance (C a, D b) => E (T a b)
-- @
derivingFlexible :: Rule
derivingFlexible =
  rule "deriving-flexible" $
    matchText "deriving instance.*,.*=>"
    & category Style
    & severity Info
    & message "Flexible context in standalone deriving"
    & safetyLevel ManualReview
