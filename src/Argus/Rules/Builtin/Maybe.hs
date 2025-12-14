{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Maybe
-- Description : Maybe operation rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Comprehensive rules for Maybe operations, simplifications,
-- and conversions.

module Argus.Rules.Builtin.Maybe
  ( -- * Rule Sets
    maybeRules
  , maybeSimplificationRules
  , maybeListRules
  , maybePatternRules

    -- * Maybe Simplifications
  , maybeNothingJust
  , maybeDefaultId
  , maybeFalseConstTrue
  , maybeTrueConstFalse
  , maybeFalseF
  , maybeTrueF
  , maybeEmptyPure
  , maybeMemptyPure
  , fromMaybeNothing
  , fromMaybeJust
  , isJustJust
  , isJustNothing
  , isNothingJust
  , isNothingNothing
  , justFromMaybe

    -- * Maybe/List Conversions
  , listToMaybeEmpty
  , listToMaybeSingleton
  , maybeToListNothing
  , maybeToListJust
  , catMaybesMapJust
  , catMaybesSingletonJust
  , catMaybesSingletonNothing
  , mapMaybeJust
  , mapMaybeId
  , mapFromJustFilter

    -- * Maybe Patterns
  , caseMaybeBind
  , caseMaybeFromMaybe
  , caseMaybeFmap
  , ifIsJustFromJust
  , ifIsNothingFromJust
  , maybeErrorId
  , maybeUndefinedId
  , joinFmapMaybe
  , fmapJust
  , fmapNothing
  , pureMaybe
  , returnMaybe
  , sequenceMaybe
  , maybeMaybeJoin
  , fromJustJust

    -- * Rule Count
  , maybeRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All maybe-related rules.
maybeRules :: [Rule]
maybeRules = mconcat
  [ maybeSimplificationRules
  , maybeListRules
  , maybePatternRules
  ]

-- | Total count of maybe rules.
maybeRuleCount :: Int
maybeRuleCount = length maybeRules

--------------------------------------------------------------------------------
-- Maybe Simplifications
--------------------------------------------------------------------------------

-- | Rules for maybe simplifications.
maybeSimplificationRules :: [Rule]
maybeSimplificationRules =
  [ maybeNothingJust
  , maybeDefaultId
  , maybeFalseConstTrue
  , maybeTrueConstFalse
  , maybeFalseF
  , maybeTrueF
  , maybeEmptyPure
  , maybeMemptyPure
  , fromMaybeNothing
  , fromMaybeJust
  , isJustJust
  , isJustNothing
  , isNothingJust
  , isNothingNothing
  , justFromMaybe
  ]

-- | maybe Nothing Just x ==> x
maybeNothingJust :: Rule
maybeNothingJust =
  rule "maybe-nothing-just" $
    match ("maybe Nothing Just _x" ==> "_x")
    & category Style
    & severity Suggestion
    & message "maybe Nothing Just is identity"

-- | maybe x id m ==> fromMaybe x m
maybeDefaultId :: Rule
maybeDefaultId =
  rule "maybe-default-id" $
    match ("maybe _x id _m" ==> "fromMaybe _x _m")
    & category Style
    & severity Suggestion
    & message "Use fromMaybe instead of maybe x id"

-- | maybe False (const True) m ==> isJust m
maybeFalseConstTrue :: Rule
maybeFalseConstTrue =
  rule "maybe-false-const-true" $
    match ("maybe False (const True) _m" ==> "isJust _m")
    & category Style
    & severity Suggestion
    & message "Use isJust instead of maybe False (const True)"

-- | maybe True (const False) m ==> isNothing m
maybeTrueConstFalse :: Rule
maybeTrueConstFalse =
  rule "maybe-true-const-false" $
    match ("maybe True (const False) _m" ==> "isNothing _m")
    & category Style
    & severity Suggestion
    & message "Use isNothing instead of maybe True (const False)"

-- | maybe False f m - suggest any.
maybeFalseF :: Rule
maybeFalseF =
  rule "maybe-false-f" $
    match ("maybe False _f _m" ==> "maybe False _f _m")
    & category Style
    & severity Info
    & message "Consider any from Foldable"
    & safetyLevel ManualReview

-- | maybe True f m - suggest all.
maybeTrueF :: Rule
maybeTrueF =
  rule "maybe-true-f" $
    match ("maybe True _f _m" ==> "maybe True _f _m")
    & category Style
    & severity Info
    & message "Consider all from Foldable"
    & safetyLevel ManualReview

-- | maybe [] pure m ==> maybeToList m
maybeEmptyPure :: Rule
maybeEmptyPure =
  rule "maybe-empty-pure" $
    match ("maybe [] pure _m" ==> "maybeToList _m")
    & category Style
    & severity Suggestion
    & message "Use maybeToList instead of maybe [] pure"

-- | maybe mempty pure m ==> foldMap pure m
maybeMemptyPure :: Rule
maybeMemptyPure =
  rule "maybe-mempty-pure" $
    match ("maybe mempty pure _m" ==> "foldMap pure _m")
    & category Style
    & severity Suggestion
    & message "Use foldMap for Maybe"

-- | fromMaybe x Nothing ==> x
fromMaybeNothing :: Rule
fromMaybeNothing =
  rule "fromMaybe-nothing" $
    match ("fromMaybe _x Nothing" ==> "_x")
    & category Style
    & severity Suggestion
    & message "fromMaybe on Nothing returns default"

-- | fromMaybe x (Just y) ==> y
fromMaybeJust :: Rule
fromMaybeJust =
  rule "fromMaybe-just" $
    match ("fromMaybe _x (Just _y)" ==> "_y")
    & category Style
    & severity Suggestion
    & message "fromMaybe on Just returns the value"

-- | isJust (Just x) ==> True
isJustJust :: Rule
isJustJust =
  rule "isJust-just" $
    match ("isJust (Just _x)" ==> "True")
    & category Style
    & severity Suggestion
    & message "isJust (Just x) is always True"

-- | isJust Nothing ==> False
isJustNothing :: Rule
isJustNothing =
  rule "isJust-nothing" $
    match ("isJust Nothing" ==> "False")
    & category Style
    & severity Suggestion
    & message "isJust Nothing is always False"

-- | isNothing (Just x) ==> False
isNothingJust :: Rule
isNothingJust =
  rule "isNothing-just" $
    match ("isNothing (Just _x)" ==> "False")
    & category Style
    & severity Suggestion
    & message "isNothing (Just x) is always False"

-- | isNothing Nothing ==> True
isNothingNothing :: Rule
isNothingNothing =
  rule "isNothing-nothing" $
    match ("isNothing Nothing" ==> "True")
    & category Style
    & severity Suggestion
    & message "isNothing Nothing is always True"

-- | Just (fromMaybe x m) pattern.
justFromMaybe :: Rule
justFromMaybe =
  rule "just-fromMaybe" $
    match ("Just (fromMaybe _x _m)" ==> "Just (fromMaybe _x _m)")
    & category Style
    & severity Info
    & message "Just (fromMaybe ...) might indicate a simpler solution"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Maybe/List Conversions
--------------------------------------------------------------------------------

-- | Rules for Maybe/List conversions.
maybeListRules :: [Rule]
maybeListRules =
  [ listToMaybeEmpty
  , listToMaybeSingleton
  , maybeToListNothing
  , maybeToListJust
  , catMaybesMapJust
  , catMaybesSingletonJust
  , catMaybesSingletonNothing
  , mapMaybeJust
  , mapMaybeId
  , mapFromJustFilter
  ]

-- | listToMaybe [] ==> Nothing
listToMaybeEmpty :: Rule
listToMaybeEmpty =
  rule "listToMaybe-empty" $
    match ("listToMaybe []" ==> "Nothing")
    & category Style
    & severity Suggestion
    & message "listToMaybe [] is Nothing"

-- | listToMaybe [x] ==> Just x
listToMaybeSingleton :: Rule
listToMaybeSingleton =
  rule "listToMaybe-singleton" $
    match ("listToMaybe [_x]" ==> "Just _x")
    & category Style
    & severity Suggestion
    & message "listToMaybe [x] is Just x"

-- | maybeToList Nothing ==> []
maybeToListNothing :: Rule
maybeToListNothing =
  rule "maybeToList-nothing" $
    match ("maybeToList Nothing" ==> "[]")
    & category Style
    & severity Suggestion
    & message "maybeToList Nothing is []"

-- | maybeToList (Just x) ==> [x]
maybeToListJust :: Rule
maybeToListJust =
  rule "maybeToList-just" $
    match ("maybeToList (Just _x)" ==> "[_x]")
    & category Style
    & severity Suggestion
    & message "maybeToList (Just x) is [x]"

-- | catMaybes (map Just xs) ==> xs
catMaybesMapJust :: Rule
catMaybesMapJust =
  rule "catMaybes-map-just" $
    match ("catMaybes (map Just _xs)" ==> "_xs")
    & category Style
    & severity Suggestion
    & message "catMaybes . map Just is identity"

-- | catMaybes [Just x] ==> [x]
catMaybesSingletonJust :: Rule
catMaybesSingletonJust =
  rule "catMaybes-singleton-just" $
    match ("catMaybes [Just _x]" ==> "[_x]")
    & category Style
    & severity Suggestion
    & message "catMaybes [Just x] is [x]"

-- | catMaybes [Nothing] ==> []
catMaybesSingletonNothing :: Rule
catMaybesSingletonNothing =
  rule "catMaybes-singleton-nothing" $
    match ("catMaybes [Nothing]" ==> "[]")
    & category Style
    & severity Suggestion
    & message "catMaybes [Nothing] is []"

-- | mapMaybe Just xs ==> xs
mapMaybeJust :: Rule
mapMaybeJust =
  rule "mapMaybe-just" $
    match ("mapMaybe Just _xs" ==> "_xs")
    & category Style
    & severity Suggestion
    & message "mapMaybe Just is identity"

-- | mapMaybe id xs ==> catMaybes xs
mapMaybeId :: Rule
mapMaybeId =
  rule "mapMaybe-id" $
    match ("mapMaybe id _xs" ==> "catMaybes _xs")
    & category Style
    & severity Suggestion
    & message "mapMaybe id is catMaybes"

-- | map fromJust (filter isJust xs) ==> catMaybes xs
mapFromJustFilter :: Rule
mapFromJustFilter =
  rule "map-fromJust-filter" $
    match ("map fromJust (filter isJust _xs)" ==> "catMaybes _xs")
    & category Style
    & severity Suggestion
    & message "Use catMaybes instead of map fromJust . filter isJust"

--------------------------------------------------------------------------------
-- Maybe Patterns
--------------------------------------------------------------------------------

-- | Rules for Maybe patterns.
maybePatternRules :: [Rule]
maybePatternRules =
  [ caseMaybeBind
  , caseMaybeFromMaybe
  , caseMaybeFmap
  , ifIsJustFromJust
  , ifIsNothingFromJust
  , maybeErrorId
  , maybeUndefinedId
  , joinFmapMaybe
  , fmapJust
  , fmapNothing
  , pureMaybe
  , returnMaybe
  , sequenceMaybe
  , maybeMaybeJoin
  , fromJustJust
  ]

-- | case m of { Nothing -> Nothing; Just x -> f x } ==> m >>= f
caseMaybeBind :: Rule
caseMaybeBind =
  rule "case-maybe-bind" $
    matchText "case\\s+[^o]+of\\s*\\{?\\s*Nothing\\s*->\\s*Nothing"
    & category Style
    & severity Suggestion
    & message "Case on Maybe with Nothing -> Nothing can be (>>=)"

-- | case m of { Nothing -> y; Just x -> x } ==> fromMaybe y m
caseMaybeFromMaybe :: Rule
caseMaybeFromMaybe =
  rule "case-maybe-fromMaybe" $
    matchText "case\\s+[^o]+of\\s*\\{?\\s*Nothing\\s*->"
    & category Style
    & severity Info
    & message "Case on Maybe might be fromMaybe"
    & safetyLevel ManualReview

-- | case m of { Just x -> Just (f x); Nothing -> Nothing } ==> fmap f m
caseMaybeFmap :: Rule
caseMaybeFmap =
  rule "case-maybe-fmap" $
    matchText "case\\s+[^o]+of\\s*\\{?\\s*Just\\s+[a-z_]+\\s*->\\s*Just"
    & category Style
    & severity Suggestion
    & message "Case with Just -> Just can be fmap"

-- | if isJust m then fromJust m else x ==> fromMaybe x m
ifIsJustFromJust :: Rule
ifIsJustFromJust =
  rule "if-isJust-fromJust" $
    matchText "if\\s+isJust\\s+[^t]+then\\s+fromJust"
    & category Style
    & severity Suggestion
    & message "Use fromMaybe instead of if isJust then fromJust"

-- | if isNothing m then x else fromJust m ==> fromMaybe x m
ifIsNothingFromJust :: Rule
ifIsNothingFromJust =
  rule "if-isNothing-fromJust" $
    matchText "if\\s+isNothing\\s+[^t]+then.*else\\s+fromJust"
    & category Style
    & severity Suggestion
    & message "Use fromMaybe instead of if isNothing then ... else fromJust"

-- | maybe (error "...") id m - warning.
maybeErrorId :: Rule
maybeErrorId =
  rule "maybe-error-id" $
    matchText "maybe\\s+\\(error"
    & category Safety
    & severity Warning
    & message "maybe with error default - consider handling Nothing properly"

-- | maybe undefined id m - warning.
maybeUndefinedId :: Rule
maybeUndefinedId =
  rule "maybe-undefined-id" $
    matchText "maybe\\s+undefined"
    & category Safety
    & severity Warning
    & message "maybe with undefined - handle Nothing properly"

-- | join (fmap f m) ==> m >>= f
joinFmapMaybe :: Rule
joinFmapMaybe =
  rule "join-fmap-maybe" $
    match ("join (fmap _f _m)" ==> "_m >>= _f")
    & category Style
    & severity Suggestion
    & message "join . fmap is (>>=)"

-- | fmap f (Just x) ==> Just (f x)
fmapJust :: Rule
fmapJust =
  rule "fmap-just" $
    match ("fmap _f (Just _x)" ==> "Just (_f _x)")
    & category Style
    & severity Suggestion
    & message "fmap over Just applies the function"

-- | fmap f Nothing ==> Nothing
fmapNothing :: Rule
fmapNothing =
  rule "fmap-nothing" $
    match ("fmap _f Nothing" ==> "Nothing")
    & category Style
    & severity Suggestion
    & message "fmap over Nothing is Nothing"

-- | pure x :: Maybe a pattern.
pureMaybe :: Rule
pureMaybe =
  rule "pure-maybe" $
    matchText "pure\\s+[^:]+::\\s*Maybe"
    & category Style
    & severity Info
    & message "pure for Maybe is Just"
    & safetyLevel ManualReview

-- | return x :: Maybe a pattern.
returnMaybe :: Rule
returnMaybe =
  rule "return-maybe" $
    matchText "return\\s+[^:]+::\\s*Maybe"
    & category Style
    & severity Info
    & message "return for Maybe is Just"
    & safetyLevel ManualReview

-- | sequence [Just x, Just y] pattern.
sequenceMaybe :: Rule
sequenceMaybe =
  rule "sequence-maybe" $
    matchText "sequence\\s*\\[\\s*Just"
    & category Style
    & severity Info
    & message "sequence on list of Justs"
    & safetyLevel ManualReview

-- | Maybe (Maybe a) - join pattern.
maybeMaybeJoin :: Rule
maybeMaybeJoin =
  rule "maybe-maybe-join" $
    matchText "Maybe\\s*\\(\\s*Maybe"
    & category Style
    & severity Info
    & message "Nested Maybe - consider join or (>>=)"
    & safetyLevel ManualReview

-- | fromJust (Just x) ==> x
fromJustJust :: Rule
fromJustJust =
  rule "fromJust-just" $
    match ("fromJust (Just _x)" ==> "_x")
    & category Style
    & severity Suggestion
    & message "fromJust (Just x) is x"
