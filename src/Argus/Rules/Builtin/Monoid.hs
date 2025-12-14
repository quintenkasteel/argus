{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Monoid
-- Description : Monoid and Semigroup rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for Monoid and Semigroup operations and simplifications.

module Argus.Rules.Builtin.Monoid
  ( -- * Rule Sets
    monoidRules
  , monoidSimplifyRules
  , semigroupWrapperRules

    -- * Monoid Simplifications
  , memptyAppend
  , appendMempty
  , mconcatEmpty
  , mconcatSingleton
  , mconcatPair
  , foldEmpty
  , foldSingleton
  , foldMapId
  , foldMapConst
  , stimesZero

    -- * Semigroup Wrappers
  , getSumAppend
  , getProductAppend
  , getAllAppend
  , getAnyAppend
  , getFirstJust
  , getLastJust
  , getDualAppend
  , getEndoAppend
  , appEndoApply
  , appEndoMconcat

    -- * Rule Count
  , monoidRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All monoid-related rules.
monoidRules :: [Rule]
monoidRules = mconcat
  [ monoidSimplifyRules
  , semigroupWrapperRules
  ]

-- | Total count of monoid rules.
monoidRuleCount :: Int
monoidRuleCount = length monoidRules

--------------------------------------------------------------------------------
-- Monoid Simplification Rules
--------------------------------------------------------------------------------

-- | Rules for monoid simplifications.
monoidSimplifyRules :: [Rule]
monoidSimplifyRules =
  [ memptyAppend
  , appendMempty
  , mconcatEmpty
  , mconcatSingleton
  , mconcatPair
  , foldEmpty
  , foldSingleton
  , foldMapId
  , foldMapConst
  , stimesZero
  ]

-- | mempty <> x ==> x
memptyAppend :: Rule
memptyAppend =
  rule "mempty-append" $
    match ("mempty <> _x" ==> "_x")
    & category Style
    & severity Suggestion
    & message "mempty <> x is x"

-- | x <> mempty ==> x
appendMempty :: Rule
appendMempty =
  rule "append-mempty" $
    match ("_x <> mempty" ==> "_x")
    & category Style
    & severity Suggestion
    & message "x <> mempty is x"

-- | mconcat [] ==> mempty
mconcatEmpty :: Rule
mconcatEmpty =
  rule "mconcat-empty" $
    match ("mconcat []" ==> "mempty")
    & category Style
    & severity Suggestion
    & message "mconcat [] is mempty"

-- | mconcat [x] ==> x
mconcatSingleton :: Rule
mconcatSingleton =
  rule "mconcat-singleton" $
    match ("mconcat [_x]" ==> "_x")
    & category Style
    & severity Suggestion
    & message "mconcat [x] is x"

-- | mconcat [x, y] ==> x <> y
mconcatPair :: Rule
mconcatPair =
  rule "mconcat-pair" $
    match ("mconcat [_x, _y]" ==> "_x <> _y")
    & category Style
    & severity Suggestion
    & message "mconcat [x, y] is x <> y"

-- | fold [] ==> mempty
foldEmpty :: Rule
foldEmpty =
  rule "fold-empty" $
    match ("fold []" ==> "mempty")
    & category Style
    & severity Suggestion
    & message "fold [] is mempty"

-- | fold [x] ==> x
foldSingleton :: Rule
foldSingleton =
  rule "fold-singleton" $
    match ("fold [_x]" ==> "_x")
    & category Style
    & severity Suggestion
    & message "fold [x] is x"

-- | foldMap id xs ==> fold xs
foldMapId :: Rule
foldMapId =
  rule "foldMap-id" $
    match ("foldMap id _xs" ==> "fold _xs")
    & category Style
    & severity Suggestion
    & message "foldMap id is fold"

-- | foldMap (const mempty) xs ==> mempty
foldMapConst :: Rule
foldMapConst =
  rule "foldMap-const" $
    match ("foldMap (const mempty) _xs" ==> "mempty")
    & category Style
    & severity Suggestion
    & message "foldMap (const mempty) is mempty"

-- | stimes 0 x ==> mempty (for Monoid)
stimesZero :: Rule
stimesZero =
  rule "stimes-zero" $
    match ("stimes 0 _x" ==> "mempty")
    & category Style
    & severity Suggestion
    & message "stimes 0 x is mempty"
    & note "Only for Monoid, not Semigroup"

--------------------------------------------------------------------------------
-- Semigroup Wrapper Rules
--------------------------------------------------------------------------------

-- | Rules for semigroup wrappers.
semigroupWrapperRules :: [Rule]
semigroupWrapperRules =
  [ getSumAppend
  , getProductAppend
  , getAllAppend
  , getAnyAppend
  , getFirstJust
  , getLastJust
  , getDualAppend
  , getEndoAppend
  , appEndoApply
  , appEndoMconcat
  ]

-- | getSum (Sum x <> Sum y) ==> x + y
getSumAppend :: Rule
getSumAppend =
  rule "getSum-append" $
    match ("getSum (Sum _x <> Sum _y)" ==> "_x + _y")
    & category Style
    & severity Suggestion
    & message "getSum (Sum x <> Sum y) is x + y"

-- | getProduct (Product x <> Product y) ==> x * y
getProductAppend :: Rule
getProductAppend =
  rule "getProduct-append" $
    match ("getProduct (Product _x <> Product _y)" ==> "_x * _y")
    & category Style
    & severity Suggestion
    & message "getProduct (Product x <> Product y) is x * y"

-- | getAll (All x <> All y) ==> x && y
getAllAppend :: Rule
getAllAppend =
  rule "getAll-append" $
    match ("getAll (All _x <> All _y)" ==> "_x && _y")
    & category Style
    & severity Suggestion
    & message "getAll (All x <> All y) is x && y"

-- | getAny (Any x <> Any y) ==> x || y
getAnyAppend :: Rule
getAnyAppend =
  rule "getAny-append" $
    match ("getAny (Any _x <> Any _y)" ==> "_x || _y")
    & category Style
    & severity Suggestion
    & message "getAny (Any x <> Any y) is x || y"

-- | getFirst (First (Just x) <> _) ==> Just x
getFirstJust :: Rule
getFirstJust =
  rule "getFirst-just" $
    matchText "getFirst\\s*\\(\\s*First\\s*\\(\\s*Just"
    & category Style
    & severity Info
    & message "getFirst with First (Just x) returns Just x regardless of rest"
    & safetyLevel ManualReview

-- | getLast (_ <> Last (Just x)) ==> Just x
getLastJust :: Rule
getLastJust =
  rule "getLast-just" $
    matchText "getLast.*Last\\s*\\(\\s*Just"
    & category Style
    & severity Info
    & message "getLast with Last (Just x) at end returns Just x"
    & safetyLevel ManualReview

-- | getDual (Dual x <> Dual y) ==> y <> x
getDualAppend :: Rule
getDualAppend =
  rule "getDual-append" $
    match ("getDual (Dual _x <> Dual _y)" ==> "_y <> _x")
    & category Style
    & severity Suggestion
    & message "getDual (Dual x <> Dual y) is y <> x"

-- | getEndo (Endo f <> Endo g) ==> f . g
getEndoAppend :: Rule
getEndoAppend =
  rule "getEndo-append" $
    match ("getEndo (Endo _f <> Endo _g)" ==> "_f . _g")
    & category Style
    & severity Suggestion
    & message "getEndo (Endo f <> Endo g) is f . g"

-- | appEndo (Endo f) x ==> f x
appEndoApply :: Rule
appEndoApply =
  rule "appEndo-apply" $
    match ("appEndo (Endo _f) _x" ==> "_f _x")
    & category Style
    & severity Suggestion
    & message "appEndo (Endo f) x is f x"

-- | appEndo (mconcat (map Endo fs)) x ==> foldr ($) x fs
appEndoMconcat :: Rule
appEndoMconcat =
  rule "appEndo-mconcat" $
    matchText "appEndo\\s*\\(\\s*mconcat\\s*\\(\\s*map\\s+Endo"
    & category Style
    & severity Suggestion
    & message "appEndo (mconcat (map Endo fs)) x is foldr ($) x fs"
