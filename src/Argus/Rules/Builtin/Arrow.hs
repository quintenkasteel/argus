{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Arrow
-- Description : Arrow operation rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for Arrow operations and combinators.

module Argus.Rules.Builtin.Arrow
  ( -- * Rule Sets
    arrowRules
  , basicArrowRules
  , arrowSimplifyRules

    -- * Basic Arrow
  , arrId
  , arrCompose
  , arrowFirstId
  , arrowSecondId
  , firstSecond
  , secondFirst
  , firstFirst
  , secondSecond
  , starId
  , ampersandId

    -- * Arrow Simplifications
  , arrFst
  , arrSnd
  , arrConst
  , fanoutFst
  , fanoutSnd
  , firstArrFst
  , secondArrSnd
  , returnACompose
  , composeReturnA
  , arrowLoop
  , kleisliArr

    -- * Rule Count
  , arrowRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All arrow-related rules.
arrowRules :: [Rule]
arrowRules = mconcat
  [ basicArrowRules
  , arrowSimplifyRules
  ]

-- | Total count of arrow rules.
arrowRuleCount :: Int
arrowRuleCount = length arrowRules

--------------------------------------------------------------------------------
-- Basic Arrow Rules
--------------------------------------------------------------------------------

-- | Rules for basic arrow operations.
basicArrowRules :: [Rule]
basicArrowRules =
  [ arrId
  , arrCompose
  , arrowFirstId
  , arrowSecondId
  , firstSecond
  , secondFirst
  , firstFirst
  , secondSecond
  , starId
  , ampersandId
  ]

-- | arr id ==> id
arrId :: Rule
arrId =
  rule "arr-id" $
    match ("arr id" ==> "id")
    & category Style
    & severity Suggestion
    & message "arr id is id"

-- | arr f >>> arr g ==> arr (g . f)
arrCompose :: Rule
arrCompose =
  rule "arr-compose" $
    match ("arr _f >>> arr _g" ==> "arr (_g . _f)")
    & category Style
    & severity Suggestion
    & message "arr f >>> arr g is arr (g . f)"

-- | first id ==> id
arrowFirstId :: Rule
arrowFirstId =
  rule "arrow-first-id" $
    match ("first id" ==> "id")
    & category Style
    & severity Suggestion
    & message "first id is id"

-- | second id ==> id
arrowSecondId :: Rule
arrowSecondId =
  rule "arrow-second-id" $
    match ("second id" ==> "id")
    & category Style
    & severity Suggestion
    & message "second id is id"

-- | first f >>> second g ==> f *** g
firstSecond :: Rule
firstSecond =
  rule "first-second" $
    match ("first _f >>> second _g" ==> "_f *** _g")
    & category Style
    & severity Suggestion
    & message "first f >>> second g is f *** g"

-- | second g >>> first f ==> f *** g
secondFirst :: Rule
secondFirst =
  rule "second-first" $
    match ("second _g >>> first _f" ==> "_f *** _g")
    & category Style
    & severity Suggestion
    & message "second g >>> first f is f *** g"

-- | first f >>> first g ==> first (g . f)
firstFirst :: Rule
firstFirst =
  rule "first-first" $
    match ("first _f >>> first _g" ==> "first (_g . _f)")
    & category Style
    & severity Suggestion
    & message "first f >>> first g is first (g . f)"

-- | second f >>> second g ==> second (g . f)
secondSecond :: Rule
secondSecond =
  rule "second-second" $
    match ("second _f >>> second _g" ==> "second (_g . _f)")
    & category Style
    & severity Suggestion
    & message "second f >>> second g is second (g . f)"

-- | (***) id id ==> id
starId :: Rule
starId =
  rule "star-id" $
    match ("id *** id" ==> "id")
    & category Style
    & severity Suggestion
    & message "id *** id is id"

-- | (&&&) id id ==> \x -> (x, x)
ampersandId :: Rule
ampersandId =
  rule "ampersand-id" $
    match ("id &&& id" ==> "id &&& id")
    & category Style
    & severity Info
    & message "id &&& id duplicates input: \\x -> (x, x)"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Arrow Simplification Rules
--------------------------------------------------------------------------------

-- | Rules for arrow simplifications.
arrowSimplifyRules :: [Rule]
arrowSimplifyRules =
  [ arrFst
  , arrSnd
  , arrConst
  , fanoutFst
  , fanoutSnd
  , firstArrFst
  , secondArrSnd
  , returnACompose
  , composeReturnA
  , arrowLoop
  , kleisliArr
  ]

-- | arr fst pattern.
arrFst :: Rule
arrFst =
  rule "arr-fst" $
    match ("arr fst" ==> "arr fst")
    & category Style
    & severity Info
    & message "Using arr fst"
    & safetyLevel ManualReview

-- | arr snd pattern.
arrSnd :: Rule
arrSnd =
  rule "arr-snd" $
    match ("arr snd" ==> "arr snd")
    & category Style
    & severity Info
    & message "Using arr snd"
    & safetyLevel ManualReview

-- | arr (const x) pattern.
arrConst :: Rule
arrConst =
  rule "arr-const" $
    match ("arr (const _x)" ==> "arr (const _x)")
    & category Style
    & severity Info
    & message "Using arr (const x)"
    & safetyLevel ManualReview

-- | f &&& g >>> arr fst ==> f
fanoutFst :: Rule
fanoutFst =
  rule "fanout-fst" $
    match ("_f &&& _g >>> arr fst" ==> "_f")
    & category Style
    & severity Suggestion
    & message "f &&& g >>> arr fst is f"

-- | f &&& g >>> arr snd ==> g
fanoutSnd :: Rule
fanoutSnd =
  rule "fanout-snd" $
    match ("_f &&& _g >>> arr snd" ==> "_g")
    & category Style
    & severity Suggestion
    & message "f &&& g >>> arr snd is g"

-- | first f >>> arr fst ==> f >>> arr fst
firstArrFst :: Rule
firstArrFst =
  rule "first-arr-fst" $
    match ("first _f >>> arr fst" ==> "_f >>> arr fst")
    & category Style
    & severity Suggestion
    & message "first f >>> arr fst ignores second component"

-- | second f >>> arr snd ==> f >>> arr snd
secondArrSnd :: Rule
secondArrSnd =
  rule "second-arr-snd" $
    match ("second _f >>> arr snd" ==> "_f >>> arr snd")
    & category Style
    & severity Suggestion
    & message "second f >>> arr snd ignores first component"

-- | returnA ==> arr id
returnACompose :: Rule
returnACompose =
  rule "returnA-compose" $
    match ("returnA" ==> "arr id")
    & category Style
    & severity Info
    & message "returnA is arr id"
    & safetyLevel ManualReview

-- | (>>>) f returnA ==> f
composeReturnA :: Rule
composeReturnA =
  rule "compose-returnA" $
    match ("_f >>> returnA" ==> "_f")
    & category Style
    & severity Suggestion
    & message "f >>> returnA is f"

-- | ArrowLoop pattern.
arrowLoop :: Rule
arrowLoop =
  rule "arrow-loop" $
    matchText "\\bloop\\b"
    & category Style
    & severity Info
    & message "Using ArrowLoop - be careful with strictness"
    & safetyLevel ManualReview

-- | Kleisli arr pattern.
kleisliArr :: Rule
kleisliArr =
  rule "kleisli-arr" $
    matchText "Kleisli"
    & category Style
    & severity Info
    & message "Using Kleisli arrow"
    & safetyLevel ManualReview
