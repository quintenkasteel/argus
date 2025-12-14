{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.TypeAnnotations
-- Description : Type signature and annotation rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for encouraging proper type annotations, detecting redundant
-- constraints, and improving type-level documentation. Good type
-- signatures are essential for code maintainability and error messages.
--
-- == Rule Categories
--
-- * __Missing Signatures__: Top-level bindings without type signatures
-- * __Redundant Constraints__: Unnecessary typeclass constraints
-- * __Type Improvements__: Suggestions for better type expressions
-- * __Modern Type Features__: TypeApplications, type wildcards
--
-- == References
--
-- * <https://wiki.haskell.org/Type_signature Type Signatures Wiki>
-- * <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/type_applications.html Type Applications>

module Argus.Rules.Builtin.TypeAnnotations
  ( -- * Rule Sets
    typeAnnotationRules
  , signatureRules
  , constraintRules
  , typeImprovementRules
  , modernTypeRules

    -- * Missing Signatures
  , missingTopLevelSig
  , missingExportedSig
  , missingInstanceSig
  , missingPatternSig
  , missingLocalSig

    -- * Redundant Constraints
  , redundantEqOrd
  , redundantShowRead
  , redundantMonadApplicative
  , redundantFunctorApplicative
  , redundantMonoidSemigroup
  , redundantNumIntegral

    -- * Type Improvements
  , useNewtype
  , useTypeAlias
  , avoidStringly
  , preferTextOverString
  , useStrictFields
  , avoidPartialFields

    -- * Modern Type Features
  , useTypeApplications
  , useVisibleForall
  , useStandaloneKindSig
  , preferExplicitForall

    -- * Rule Count
  , typeAnnotationRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All type annotation related rules.
typeAnnotationRules :: [Rule]
typeAnnotationRules = mconcat
  [ signatureRules
  , constraintRules
  , typeImprovementRules
  , modernTypeRules
  ]

-- | Total count of type annotation rules.
typeAnnotationRuleCount :: Int
typeAnnotationRuleCount = length typeAnnotationRules

--------------------------------------------------------------------------------
-- Missing Signature Rules
--------------------------------------------------------------------------------

-- | Rules for detecting missing type signatures.
signatureRules :: [Rule]
signatureRules =
  [ missingTopLevelSig
  , missingExportedSig
  , missingInstanceSig
  , missingPatternSig
  , missingLocalSig
  ]

-- | Missing top-level type signature.
--
-- @
-- -- Bad
-- foo x = x + 1
--
-- -- Good
-- foo :: Num a => a -> a
-- foo x = x + 1
-- @
missingTopLevelSig :: Rule
missingTopLevelSig =
  rule "missing-top-level-sig" $
    matchText "^[a-z][a-zA-Z0-9_']* " -- Matches function definition without sig
    & category Documentation
    & severity Warning
    & message "Missing type signature for top-level binding"
    & note "Type signatures improve documentation and catch errors early"

-- | Missing signature on exported function.
--
-- @
-- module Foo (bar) where
-- bar x = x  -- WARN: exported but no signature
-- @
missingExportedSig :: Rule
missingExportedSig =
  rule "missing-exported-sig" $
    matchText "^[a-z][a-zA-Z0-9_']* "
    & category Documentation
    & severity Warning
    & message "Missing type signature for exported binding"
    & note "Exported functions should always have type signatures"

-- | Suggest InstanceSigs extension.
--
-- @
-- instance Show Foo where
--   show x = ...  -- Could benefit from type sig
-- @
missingInstanceSig :: Rule
missingInstanceSig =
  rule "missing-instance-sig" $
    matchText "instance .+ where"
    & category Style
    & severity Info
    & message "Consider using InstanceSigs for complex instances"
    & note "InstanceSigs extension allows type signatures in instance methods"

-- | Missing pattern signature in pattern synonyms.
--
-- @
-- pattern Foo x = Just x  -- Should have signature
-- @
missingPatternSig :: Rule
missingPatternSig =
  rule "missing-pattern-sig" $
    matchText "^pattern [A-Z]"
    & category Documentation
    & severity Suggestion
    & message "Consider adding pattern signature"
    & note "Pattern synonyms benefit from explicit type signatures"

-- | Complex local binding without signature.
--
-- @
-- foo = let complexHelper x y z = ... in ...
-- @
missingLocalSig :: Rule
missingLocalSig =
  rule "missing-local-sig" $
    matchText "let [a-z]+ .+ .+ .+ ="
    & category Style
    & severity Info
    & message "Consider type signature for complex local binding"
    & note "Local bindings with 3+ parameters often benefit from signatures"

--------------------------------------------------------------------------------
-- Redundant Constraint Rules
--------------------------------------------------------------------------------

-- | Rules for detecting redundant typeclass constraints.
constraintRules :: [Rule]
constraintRules =
  [ redundantEqOrd
  , redundantShowRead
  , redundantMonadApplicative
  , redundantFunctorApplicative
  , redundantMonoidSemigroup
  , redundantNumIntegral
  ]

-- | Redundant Eq when Ord is present.
--
-- @
-- foo :: (Ord a, Eq a) => ...  ==>  foo :: Ord a => ...
-- @
redundantEqOrd :: Rule
redundantEqOrd =
  rule "redundant-eq-ord" $
    matchText "\\(Ord [a-z]+, Eq [a-z]+\\)"
    & category Style
    & severity Suggestion
    & message "Redundant Eq constraint - Ord implies Eq"
    & note "Ord is a superclass of Eq, so Eq is implied"

-- | Redundant Show when Read roundtrip.
--
-- @
-- foo :: (Show a, Read a) => ...  -- Often indicates design issue
-- @
redundantShowRead :: Rule
redundantShowRead =
  rule "redundant-show-read" $
    matchText "\\(Show [a-z]+, Read [a-z]+\\)"
    & category Correctness
    & severity Warning
    & message "Show/Read pair may indicate fragile serialization"
    & note "Consider proper serialization (JSON, Binary) instead of Show/Read"

-- | Redundant Monad when Applicative suffices.
--
-- @
-- foo :: Monad m => m a -> m b -> m (a, b)
-- -- Could be: foo :: Applicative f => f a -> f b -> f (a, b)
-- @
redundantMonadApplicative :: Rule
redundantMonadApplicative =
  rule "redundant-monad-applicative" $
    matchText "Monad [a-z]+ =>"
    & category Style
    & severity Info
    & message "Consider if Applicative constraint suffices"
    & note "Applicative is weaker than Monad - prefer minimal constraints"
    & safetyLevel ManualReview

-- | Redundant Functor when Applicative is present.
--
-- @
-- foo :: (Applicative f, Functor f) => ...
-- @
redundantFunctorApplicative :: Rule
redundantFunctorApplicative =
  rule "redundant-functor-applicative" $
    matchText "\\(Applicative [a-z]+, Functor [a-z]+\\)"
    & category Style
    & severity Suggestion
    & message "Redundant Functor constraint - Applicative implies Functor"

-- | Redundant Semigroup when Monoid is present.
--
-- @
-- foo :: (Monoid a, Semigroup a) => ...
-- @
redundantMonoidSemigroup :: Rule
redundantMonoidSemigroup =
  rule "redundant-monoid-semigroup" $
    matchText "\\(Monoid [a-z]+, Semigroup [a-z]+\\)"
    & category Style
    & severity Suggestion
    & message "Redundant Semigroup constraint - Monoid implies Semigroup"

-- | Redundant Num when Integral is present.
--
-- @
-- foo :: (Integral a, Num a) => ...
-- @
redundantNumIntegral :: Rule
redundantNumIntegral =
  rule "redundant-num-integral" $
    matchText "\\(Integral [a-z]+, Num [a-z]+\\)"
    & category Style
    & severity Suggestion
    & message "Redundant Num constraint - Integral implies Num"

--------------------------------------------------------------------------------
-- Type Improvement Rules
--------------------------------------------------------------------------------

-- | Rules for improving type definitions.
typeImprovementRules :: [Rule]
typeImprovementRules =
  [ useNewtype
  , useTypeAlias
  , avoidStringly
  , preferTextOverString
  , useStrictFields
  , avoidPartialFields
  ]

-- | Suggest newtype for single-field data types.
--
-- @
-- data UserId = UserId Int  ==>  newtype UserId = UserId Int
-- @
useNewtype :: Rule
useNewtype =
  rule "use-newtype" $
    matchText "^data [A-Z][a-zA-Z]* = [A-Z][a-zA-Z]* [A-Z]?[a-z]"
    & category Performance
    & severity Suggestion
    & message "Consider newtype for single-constructor single-field type"
    & note "newtype has no runtime overhead unlike data"

-- | Suggest type alias for complex types.
--
-- @
-- foo :: Map String (Either String (Maybe Int)) -> ...
-- -- type Result = Either String (Maybe Int)
-- @
useTypeAlias :: Rule
useTypeAlias =
  rule "use-type-alias" $
    matchText "Either [A-Z][a-z]+ \\(Maybe"
    & category Style
    & severity Info
    & message "Consider type alias for complex nested types"
    & note "Type aliases improve readability for complex types"

-- | Avoid stringly-typed code.
--
-- @
-- data Config = Config { mode :: String }  -- Use sum type instead
-- @
avoidStringly :: Rule
avoidStringly =
  rule "avoid-stringly" $
    matchText ":: String[^-]"
    & category Style
    & severity Info
    & message "Consider newtype or sum type instead of String"
    & note "String fields often indicate missing domain types"
    & safetyLevel ManualReview

-- | Prefer Text over String.
--
-- @
-- foo :: String -> String  ==>  foo :: Text -> Text
-- @
preferTextOverString :: Rule
preferTextOverString =
  rule "prefer-text-over-string" $
    matchText ":: String ->"
    & category Performance
    & severity Info
    & message "Consider Text instead of String"
    & note "Text is more efficient for most text processing"
    & safetyLevel ManualReview

-- | Suggest strict fields in data types.
--
-- @
-- data Foo = Foo { bar :: Int }  ==>  data Foo = Foo { bar :: !Int }
-- @
useStrictFields :: Rule
useStrictFields =
  rule "use-strict-fields" $
    matchText "\\{ [a-z]+ :: [A-Z]"
    & category Performance
    & severity Info
    & message "Consider strict fields to prevent space leaks"
    & note "Use StrictData extension or bang patterns for strict fields"
    & safetyLevel ManualReview

-- | Avoid partial record fields.
--
-- @
-- data Result = Success { value :: Int } | Failure  -- value is partial!
-- @
avoidPartialFields :: Rule
avoidPartialFields =
  rule "avoid-partial-fields" $
    matchText "data .+ = .+ \\{.+\\} \\| [A-Z]"
    & category Safety
    & severity Warning
    & message "Partial record field - not all constructors have this field"
    & note "Accessing this field on other constructors will throw an exception"

--------------------------------------------------------------------------------
-- Modern Type Feature Rules
--------------------------------------------------------------------------------

-- | Rules for modern GHC type features.
modernTypeRules :: [Rule]
modernTypeRules =
  [ useTypeApplications
  , useVisibleForall
  , useStandaloneKindSig
  , preferExplicitForall
  ]

-- | Suggest TypeApplications over type annotations.
--
-- @
-- read "42" :: Int  ==>  read @Int "42"
-- @
useTypeApplications :: Rule
useTypeApplications =
  rule "use-type-applications" $
    match ("read _s :: _t" ==> "read @_t _s")
    & category Modernization
    & severity Info
    & message "Consider TypeApplications for explicit type instantiation"
    & note "TypeApplications can be clearer for polymorphic functions"

-- | Suggest visible forall in type signatures.
--
-- @
-- foo :: forall a. Proxy a -> a  ==>  foo :: forall a -> Proxy a -> a
-- @
useVisibleForall :: Rule
useVisibleForall =
  rule "use-visible-forall" $
    matchText "forall [a-z]+\\. Proxy [a-z]+ ->"
    & category Modernization
    & severity Info
    & message "Consider visible forall for type arguments"
    & note "GHC 9.10+ supports visible forall for required type arguments"

-- | Suggest standalone kind signatures.
--
-- @
-- data Foo (a :: Type) = ...  ==>  type Foo :: Type -> Type; data Foo a = ...
-- @
useStandaloneKindSig :: Rule
useStandaloneKindSig =
  rule "use-standalone-kind-sig" $
    matchText "data [A-Z][a-zA-Z]* \\([a-z]+ ::"
    & category Modernization
    & severity Info
    & message "Consider standalone kind signature"
    & note "Standalone kind signatures are clearer for complex kinds"

-- | Prefer explicit forall for clarity.
--
-- @
-- foo :: a -> b -> a  ==>  foo :: forall a b. a -> b -> a
-- @
preferExplicitForall :: Rule
preferExplicitForall =
  rule "prefer-explicit-forall" $
    matchText ":: [a-z]+ -> [a-z]+ ->"
    & category Style
    & severity Info
    & message "Consider explicit forall for polymorphic functions"
    & note "Explicit forall clarifies type variable scope"
    & safetyLevel ManualReview
