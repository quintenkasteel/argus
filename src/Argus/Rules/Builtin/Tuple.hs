{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Tuple
-- Description : Tuple operation rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for tuple operations, bifunctor patterns, and related simplifications.

module Argus.Rules.Builtin.Tuple
  ( -- * Rule Sets
    tupleRules
  , basicTupleRules
  , tupleFunctionRules

    -- * Basic Tuple
  , fstPair
  , sndPair
  , fstSwap
  , sndSwap
  , swapSwap
  , pairFstSnd
  , curryFst
  , currySnd
  , uncurryPair
  , uncurryApply
  , curryUncurry
  , uncurryCurry

    -- * Tuple Functions
  , bimapPair
  , firstPair
  , secondPair
  , bimapIdF
  , bimapFId
  , bimapIdId
  , bothPair
  , pairSameFunc
  , dupFunction
  , fanout

    -- * Rule Count
  , tupleRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All tuple-related rules.
tupleRules :: [Rule]
tupleRules = mconcat
  [ basicTupleRules
  , tupleFunctionRules
  ]

-- | Total count of tuple rules.
tupleRuleCount :: Int
tupleRuleCount = length tupleRules

--------------------------------------------------------------------------------
-- Basic Tuple Rules
--------------------------------------------------------------------------------

-- | Rules for basic tuple operations.
basicTupleRules :: [Rule]
basicTupleRules =
  [ fstPair
  , sndPair
  , fstSwap
  , sndSwap
  , swapSwap
  , pairFstSnd
  , curryFst
  , currySnd
  , uncurryPair
  , uncurryApply
  , curryUncurry
  , uncurryCurry
  ]

-- | fst (x, y) ==> x
fstPair :: Rule
fstPair =
  rule "fst-pair" $
    match ("fst (_x, _y)" ==> "_x")
    & category Style
    & severity Suggestion
    & message "fst (x, y) is x"

-- | snd (x, y) ==> y
sndPair :: Rule
sndPair =
  rule "snd-pair" $
    match ("snd (_x, _y)" ==> "_y")
    & category Style
    & severity Suggestion
    & message "snd (x, y) is y"

-- | fst (swap (x, y)) ==> y
fstSwap :: Rule
fstSwap =
  rule "fst-swap" $
    match ("fst (swap (_x, _y))" ==> "_y")
    & category Style
    & severity Suggestion
    & message "fst (swap (x, y)) is y"

-- | snd (swap (x, y)) ==> x
sndSwap :: Rule
sndSwap =
  rule "snd-swap" $
    match ("snd (swap (_x, _y))" ==> "_x")
    & category Style
    & severity Suggestion
    & message "snd (swap (x, y)) is x"

-- | swap (swap x) ==> x
swapSwap :: Rule
swapSwap =
  rule "swap-swap" $
    match ("swap (swap _x)" ==> "_x")
    & category Style
    & severity Suggestion
    & message "Double swap is identity"

-- | (fst x, snd x) ==> x
pairFstSnd :: Rule
pairFstSnd =
  rule "pair-fst-snd" $
    matchText "\\(\\s*fst\\s+([a-z_]+)\\s*,\\s*snd\\s+\\1\\s*\\)"
    & category Style
    & severity Suggestion
    & message "(fst x, snd x) is x"

-- | curry fst ==> const
curryFst :: Rule
curryFst =
  rule "curry-fst" $
    match ("curry fst" ==> "const")
    & category Style
    & severity Suggestion
    & message "curry fst is const"

-- | curry snd ==> const id
currySnd :: Rule
currySnd =
  rule "curry-snd" $
    match ("curry snd" ==> "const id")
    & category Style
    & severity Suggestion
    & message "curry snd is const id (flip const)"

-- | uncurry (,) ==> id
uncurryPair :: Rule
uncurryPair =
  rule "uncurry-pair" $
    match ("uncurry (,)" ==> "id")
    & category Style
    & severity Suggestion
    & message "uncurry (,) is id on pairs"

-- | uncurry f (x, y) ==> f x y
uncurryApply :: Rule
uncurryApply =
  rule "uncurry-apply" $
    match ("uncurry _f (_x, _y)" ==> "_f _x _y")
    & category Style
    & severity Suggestion
    & message "uncurry f (x, y) is f x y"

-- | curry (uncurry f) ==> f
curryUncurry :: Rule
curryUncurry =
  rule "curry-uncurry" $
    match ("curry (uncurry _f)" ==> "_f")
    & category Style
    & severity Suggestion
    & message "curry . uncurry is identity"

-- | uncurry (curry f) ==> f
uncurryCurry :: Rule
uncurryCurry =
  rule "uncurry-curry" $
    match ("uncurry (curry _f)" ==> "_f")
    & category Style
    & severity Suggestion
    & message "uncurry . curry is identity"

--------------------------------------------------------------------------------
-- Tuple Function Rules
--------------------------------------------------------------------------------

-- | Rules for tuple functions.
tupleFunctionRules :: [Rule]
tupleFunctionRules =
  [ bimapPair
  , firstPair
  , secondPair
  , bimapIdF
  , bimapFId
  , bimapIdId
  , bothPair
  , pairSameFunc
  , dupFunction
  , fanout
  ]

-- | bimap f g (x, y) ==> (f x, g y)
bimapPair :: Rule
bimapPair =
  rule "bimap-pair" $
    match ("bimap _f _g (_x, _y)" ==> "(_f _x, _g _y)")
    & category Style
    & severity Suggestion
    & message "bimap f g (x, y) is (f x, g y)"

-- | first f (x, y) ==> (f x, y)
firstPair :: Rule
firstPair =
  rule "first-pair" $
    match ("first _f (_x, _y)" ==> "(_f _x, _y)")
    & category Style
    & severity Suggestion
    & message "first f (x, y) is (f x, y)"

-- | second f (x, y) ==> (x, f y)
secondPair :: Rule
secondPair =
  rule "second-pair" $
    match ("second _f (_x, _y)" ==> "(_x, _f _y)")
    & category Style
    & severity Suggestion
    & message "second f (x, y) is (x, f y)"

-- | bimap id f ==> second f
bimapIdF :: Rule
bimapIdF =
  rule "bimap-id-f" $
    match ("bimap id _f" ==> "second _f")
    & category Style
    & severity Suggestion
    & message "bimap id f is second f"

-- | bimap f id ==> first f
bimapFId :: Rule
bimapFId =
  rule "bimap-f-id" $
    match ("bimap _f id" ==> "first _f")
    & category Style
    & severity Suggestion
    & message "bimap f id is first f"

-- | bimap id id ==> id
bimapIdId :: Rule
bimapIdId =
  rule "bimap-id-id" $
    match ("bimap id id" ==> "id")
    & category Style
    & severity Suggestion
    & message "bimap id id is identity"

-- | both f (x, y) ==> (f x, f y)
bothPair :: Rule
bothPair =
  rule "both-pair" $
    match ("both _f (_x, _y)" ==> "(_f _x, _f _y)")
    & category Style
    & severity Suggestion
    & message "both f (x, y) is (f x, f y)"

-- | (f x, f y) - suggest both f.
pairSameFunc :: Rule
pairSameFunc =
  rule "pair-same-func" $
    matchText "\\(\\s*([a-z_]+)\\s+([a-z_]+)\\s*,\\s*\\1\\s+([a-z_]+)\\s*\\)"
    & category Style
    & severity Info
    & message "Same function applied to both - consider both f (x, y)"
    & safetyLevel ManualReview

-- | \x -> (x, x) ==> join (,)
dupFunction :: Rule
dupFunction =
  rule "dup-function" $
    matchText "\\\\\\s*([a-z_]+)\\s*->\\s*\\(\\s*\\1\\s*,\\s*\\1\\s*\\)"
    & category Style
    & severity Suggestion
    & message "\\x -> (x, x) is join (,)"

-- | (f &&& g) pattern.
fanout :: Rule
fanout =
  rule "fanout" $
    matchText "\\(&&&\\)"
    & category Style
    & severity Info
    & message "Using fanout (&&&) - (f &&& g) x = (f x, g x)"
    & safetyLevel ManualReview
