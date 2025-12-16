{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.TypeclassLaws
-- Description : Typeclass law violation detection
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for detecting potential violations of typeclass laws. These rules
-- use static analysis to identify instances that may break fundamental
-- algebraic laws, leading to subtle bugs.
--
-- == Rule Categories
--
-- * __Functor Laws__: Identity and composition
-- * __Applicative Laws__: Identity, composition, homomorphism, interchange
-- * __Monad Laws__: Left identity, right identity, associativity
-- * __Eq/Ord Laws__: Reflexivity, symmetry, transitivity
-- * __Monoid/Semigroup Laws__: Identity, associativity
-- * __Foldable/Traversable Laws__: Naturality, identity
--
-- == References
--
-- * <https://wiki.haskell.org/Typeclassopedia Typeclassopedia>
-- * <https://hackage.haskell.org/package/base/docs/Data-Functor.html Functor Laws>
-- * <https://wiki.haskell.org/Monad_laws Monad Laws>

module Argus.Rules.Builtin.TypeclassLaws
  ( -- * Rule Sets
    typeclassLawRules
  , functorLawRules
  , applicativeLawRules
  , monadLawRules
  , eqOrdLawRules
  , monoidLawRules
  , foldableLawRules

    -- * Functor Laws
  , functorIdentity
  , functorCompositionLaw
  , fmapConstLaw

    -- * Applicative Laws
  , applicativeIdentityLaw
  , applicativeCompositionLaw
  , applicativeHomomorphismLaw
  , applicativeInterchange

    -- * Monad Laws
  , monadLeftIdentityLaw
  , monadRightIdentityLaw
  , monadAssociativityLaw
  , monadApConsistency

    -- * Eq/Ord Laws
  , eqReflexivity
  , eqSymmetry
  , eqTransitivity
  , ordConsistency
  , ordTotality
  , ordReflexivity

    -- * Monoid/Semigroup Laws
  , monoidLeftIdentity
  , monoidRightIdentity
  , semigroupAssociativity
  , monoidConcat

    -- * Foldable/Traversable Laws
  , foldMapLaw
  , traverseIdentity
  , traverseNaturality

    -- * Rule Count
  , typeclassLawRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All typeclass law checking rules.
typeclassLawRules :: [Rule]
typeclassLawRules = mconcat
  [ functorLawRules
  , applicativeLawRules
  , monadLawRules
  , eqOrdLawRules
  , monoidLawRules
  , foldableLawRules
  ]

-- | Total count of typeclass law rules.
typeclassLawRuleCount :: Int
typeclassLawRuleCount = length typeclassLawRules

--------------------------------------------------------------------------------
-- Functor Law Rules
--------------------------------------------------------------------------------

-- | Rules for Functor law violations.
functorLawRules :: [Rule]
functorLawRules =
  [ functorIdentity
  , functorCompositionLaw
  , fmapConstLaw
  ]

-- | Functor identity law: fmap id = id
--
-- Detects implementations that don't preserve identity.
--
-- @
-- instance Functor Foo where
--   fmap f (Foo x) = Foo (f x + 1)  -- VIOLATES: fmap id /= id
-- @
functorIdentity :: Rule
functorIdentity =
  rule "functor-identity-law" $
    matchText "fmap [a-z]+ \\([A-Z][a-zA-Z]+ [a-z]+\\) = [A-Z][a-zA-Z]+ \\(.+ \\+ "
    & category Correctness
    & severity Error
    & message "Functor identity law violation: fmap id should equal id"
    & note "fmap id (Foo x) must equal Foo x, but this adds/modifies values"

-- | Functor composition law: fmap (f . g) = fmap f . fmap g
--
-- @
-- instance Functor Foo where
--   fmap f (Foo x) = Foo (f (x + 1))  -- May violate composition
-- @
functorCompositionLaw :: Rule
functorCompositionLaw =
  rule "functor-composition-law" $
    matchText "fmap [a-z]+ \\([A-Z][a-zA-Z]+ [a-z]+\\) = [A-Z][a-zA-Z]+ \\([a-z]+ \\([a-z]+"
    & category Correctness
    & severity Warning
    & message "Check Functor composition law: fmap (f . g) = fmap f . fmap g"
    & note "Ensure fmap doesn't modify structure beyond applying the function"
    & safetyLevel ManualReview

-- | fmap (const x) = x <$ pattern.
--
-- @
-- fmap (const x) fa  ==>  x <$ fa
-- @
fmapConstLaw :: Rule
fmapConstLaw =
  rule "fmap-const-law" $
    match ("fmap (const _x) _fa" ==> "_x <$ _fa")
    & category Style
    & severity Suggestion
    & message "Use (<$) instead of fmap (const x)"

--------------------------------------------------------------------------------
-- Applicative Law Rules
--------------------------------------------------------------------------------

-- | Rules for Applicative law violations.
applicativeLawRules :: [Rule]
applicativeLawRules =
  [ applicativeIdentityLaw
  , applicativeCompositionLaw
  , applicativeHomomorphismLaw
  , applicativeInterchange
  ]

-- | Applicative identity law: pure id <*> v = v
--
-- @
-- pure id <*> x  ==>  x
-- @
applicativeIdentityLaw :: Rule
applicativeIdentityLaw =
  rule "applicative-identity-law" $
    match ("pure id <*> _v" ==> "_v")
    & category Style
    & severity Suggestion
    & message "Applicative identity: pure id <*> v = v"

-- | Applicative composition law.
--
-- @
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- @
applicativeCompositionLaw :: Rule
applicativeCompositionLaw =
  rule "applicative-composition-law" $
    match ("pure (.) <*> _u <*> _v <*> _w" ==> "_u <*> (_v <*> _w)")
    & category Style
    & severity Suggestion
    & message "Applicative composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)"

-- | Applicative homomorphism: pure f <*> pure x = pure (f x)
--
-- @
-- pure f <*> pure x  ==>  pure (f x)
-- @
applicativeHomomorphismLaw :: Rule
applicativeHomomorphismLaw =
  rule "applicative-homomorphism-law" $
    match ("pure _f <*> pure _x" ==> "pure (_f _x)")
    & category Style
    & severity Suggestion
    & message "Applicative homomorphism: pure f <*> pure x = pure (f x)"

-- | Applicative interchange: u <*> pure y = pure ($ y) <*> u
--
-- @
-- f <*> pure x  -- Can be rewritten using interchange
-- @
applicativeInterchange :: Rule
applicativeInterchange =
  rule "applicative-interchange" $
    match ("_f <*> pure _x" ==> "pure ($ _x) <*> _f")
    & category Style
    & severity Info
    & message "Applicative interchange: u <*> pure y = pure ($ y) <*> u"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Monad Law Rules
--------------------------------------------------------------------------------

-- | Rules for Monad law violations.
monadLawRules :: [Rule]
monadLawRules =
  [ monadLeftIdentityLaw
  , monadRightIdentityLaw
  , monadAssociativityLaw
  , monadApConsistency
  ]

-- | Monad left identity: return a >>= k = k a
--
-- @
-- return x >>= f  ==>  f x
-- @
monadLeftIdentityLaw :: Rule
monadLeftIdentityLaw =
  rule "monad-left-identity" $
    match ("return _a >>= _k" ==> "_k _a")
    & category Style
    & severity Suggestion
    & message "Monad left identity: return a >>= k = k a"

-- | Monad right identity: m >>= return = m
--
-- @
-- m >>= return  ==>  m
-- @
monadRightIdentityLaw :: Rule
monadRightIdentityLaw =
  rule "monad-right-identity" $
    match ("_m >>= return" ==> "_m")
    & category Style
    & severity Suggestion
    & message "Monad right identity: m >>= return = m"

-- | Monad associativity: (m >>= f) >>= g = m >>= (\\x -> f x >>= g)
--
-- @
-- -- Nesting of >>= should be associative
-- @
monadAssociativityLaw :: Rule
monadAssociativityLaw =
  rule "monad-associativity" $
    match ("(_m >>= _f) >>= _g" ==> "_m >>= (\\x -> _f x >>= _g)")
    & category Style
    & severity Info
    & message "Monad associativity: (m >>= f) >>= g = m >>= (\\x -> f x >>= g)"
    & safetyLevel ManualReview

-- | Applicative/Monad consistency: (<*>) = ap
--
-- Detects inconsistent ap/(<*>) implementations.
--
-- @
-- instance Monad Foo where
--   return = ...
--   (>>=) = ...
--   -- Should ensure (\<*\>) = ap for consistency
-- @
monadApConsistency :: Rule
monadApConsistency =
  rule "monad-ap-consistency" $
    matchText "instance Monad [A-Z]"
    & category Correctness
    & severity Info
    & message "Ensure Applicative (<*>) is consistent with Monad ap"
    & note "With Monad, (<*>) should equal ap for consistent behavior"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Eq/Ord Law Rules
--------------------------------------------------------------------------------

-- | Rules for Eq/Ord law violations.
eqOrdLawRules :: [Rule]
eqOrdLawRules =
  [ eqReflexivity
  , eqSymmetry
  , eqTransitivity
  , ordConsistency
  , ordTotality
  , ordReflexivity
  ]

-- | Eq reflexivity: x == x must be True.
--
-- Detects Eq instances that may not be reflexive.
--
-- @
-- instance Eq Foo where
--   (Foo x) == (Foo y) = x > y  -- NOT REFLEXIVE!
-- @
eqReflexivity :: Rule
eqReflexivity =
  rule "eq-reflexivity" $
    matchText "instance Eq [A-Z].+== .+ = .+ [<>] "
    & category Correctness
    & severity Error
    & message "Eq instance may violate reflexivity: x == x should be True"
    & note "Using < or > in (==) implementation breaks reflexivity"

-- | Eq symmetry: x == y implies y == x.
--
-- @
-- instance Eq Foo where
--   (Foo x) == (Foo y) = x <= y  -- NOT SYMMETRIC!
-- @
eqSymmetry :: Rule
eqSymmetry =
  rule "eq-symmetry" $
    matchText "instance Eq [A-Z].+== .+ = .+ [<>]= "
    & category Correctness
    & severity Error
    & message "Eq instance may violate symmetry: x == y should imply y == x"

-- | Eq transitivity warning.
--
-- Complex Eq instances should be checked for transitivity.
eqTransitivity :: Rule
eqTransitivity =
  rule "eq-transitivity" $
    matchText "instance Eq [A-Z].+== .+ = .+ && "
    & category Correctness
    & severity Warning
    & message "Complex Eq instance - verify transitivity"
    & note "If x == y and y == z, then x == z must hold"
    & safetyLevel ManualReview

-- | Ord must be consistent with Eq.
--
-- @
-- -- If Eq uses different logic than Ord, problems arise
-- @
ordConsistency :: Rule
ordConsistency =
  rule "ord-consistency" $
    matchText "instance Ord [A-Z]"
    & category Correctness
    & severity Info
    & message "Ensure Ord is consistent with Eq"
    & note "compare x y == EQ should imply x == y"
    & safetyLevel ManualReview

-- | Ord totality: compare must return a defined result.
--
-- @
-- instance Ord Foo where
--   compare (Foo x) (Foo y) = undefined  -- VIOLATES TOTALITY
-- @
ordTotality :: Rule
ordTotality =
  rule "ord-totality" $
    matchText "compare .+ = undefined"
    & category Correctness
    & severity Error
    & message "Ord totality violation: compare must always return a result"

-- | Ord reflexivity: compare x x = EQ
--
-- @
-- instance Ord Foo where
--   compare (Foo x) (Foo y) = LT  -- NOT REFLEXIVE!
-- @
ordReflexivity :: Rule
ordReflexivity =
  rule "ord-reflexivity" $
    matchText "compare .+ .+ = [LG]T$"
    & category Correctness
    & severity Warning
    & message "Ord instance may violate reflexivity: compare x x should be EQ"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Monoid/Semigroup Law Rules
--------------------------------------------------------------------------------

-- | Rules for Monoid/Semigroup law violations.
monoidLawRules :: [Rule]
monoidLawRules =
  [ monoidLeftIdentity
  , monoidRightIdentity
  , semigroupAssociativity
  , monoidConcat
  ]

-- | Monoid left identity: mempty <> x = x
--
-- @
-- mempty <> x  ==>  x
-- @
monoidLeftIdentity :: Rule
monoidLeftIdentity =
  rule "monoid-left-identity" $
    match ("mempty <> _x" ==> "_x")
    & category Style
    & severity Suggestion
    & message "Monoid left identity: mempty <> x = x"

-- | Monoid right identity: x <> mempty = x
--
-- @
-- x <> mempty  ==>  x
-- @
monoidRightIdentity :: Rule
monoidRightIdentity =
  rule "monoid-right-identity" $
    match ("_x <> mempty" ==> "_x")
    & category Style
    & severity Suggestion
    & message "Monoid right identity: x <> mempty = x"

-- | Semigroup associativity: (x <> y) <> z = x <> (y <> z)
--
-- Detects potentially non-associative Semigroup instances.
--
-- @
-- instance Semigroup Foo where
--   (Foo x) <> (Foo y) = Foo (x - y)  -- NOT ASSOCIATIVE!
-- @
semigroupAssociativity :: Rule
semigroupAssociativity =
  rule "semigroup-associativity" $
    matchText "instance Semigroup [A-Z].+<> .+ = .+ - "
    & category Correctness
    & severity Error
    & message "Semigroup may violate associativity: (<>) uses subtraction"
    & note "Subtraction is not associative: (a - b) - c /= a - (b - c)"

-- | Use mconcat for folding monoids.
--
-- @
-- foldr (<>) mempty xs  ==>  mconcat xs
-- @
monoidConcat :: Rule
monoidConcat =
  rule "monoid-concat" $
    match ("foldr (<>) mempty _xs" ==> "mconcat _xs")
    & category Style
    & severity Suggestion
    & message "Use mconcat for folding monoids"

--------------------------------------------------------------------------------
-- Foldable/Traversable Law Rules
--------------------------------------------------------------------------------

-- | Rules for Foldable/Traversable law violations.
foldableLawRules :: [Rule]
foldableLawRules =
  [ foldMapLaw
  , traverseIdentity
  , traverseNaturality
  ]

-- | foldMap must be consistent with fold.
--
-- @
-- foldMap f = fold . fmap f
-- @
foldMapLaw :: Rule
foldMapLaw =
  rule "foldmap-law" $
    match ("fold (fmap _f _t)" ==> "foldMap _f _t")
    & category Style
    & severity Suggestion
    & message "Use foldMap instead of fold . fmap"

-- | Traversable identity: traverse Identity = Identity
--
-- @
-- traverse Identity x  ==>  Identity x
-- @
traverseIdentity :: Rule
traverseIdentity =
  rule "traverse-identity" $
    match ("traverse Identity _x" ==> "Identity _x")
    & category Style
    & severity Suggestion
    & message "Traversable identity: traverse Identity = Identity"

-- | Traversable naturality.
--
-- @
-- t . traverse f = traverse (t . f)  -- for appropriate t
-- @
traverseNaturality :: Rule
traverseNaturality =
  rule "traverse-naturality" $
    matchText "traverse .+ \\. traverse"
    & category Style
    & severity Info
    & message "Traversable naturality - check if traversals can be fused"
    & safetyLevel ManualReview
