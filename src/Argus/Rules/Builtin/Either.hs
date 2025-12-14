{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Either
-- Description : Either operation rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Comprehensive rules for Either operations, simplifications,
-- and conversions.

module Argus.Rules.Builtin.Either
  ( -- * Rule Sets
    eitherRules
  , eitherSimplificationRules
  , eitherConversionRules
  , eitherPatternRules

    -- * Either Simplifications
  , eitherLeftRight
  , eitherIdId
  , eitherConstFalseTrue
  , eitherConstTrueFalse
  , eitherLeftFmap
  , eitherFirstRight
  , isLeftLeft
  , isLeftRight
  , isRightLeft
  , isRightRight
  , fromLeftLeft
  , fromLeftRight
  , fromRightLeft
  , fromRightRight

    -- * Either Conversions
  , leftsSingletonLeft
  , leftsSingletonRight
  , rightsSingletonLeft
  , rightsSingletonRight
  , partitionEithersLeft
  , partitionEithersRight
  , eitherToMaybeLeft
  , eitherToMaybeRight
  , maybeToEitherRight
  , maybeToEitherLeft
  , caseEither

    -- * Either Pattern Rules
  , eitherBimapId
  , eitherFirstId
  , eitherSecondId
  , eitherFmapId
  , bitraverseEither
  , firstMaybeEither
  , secondMaybeEither
  , eitherSwap
  , leftMapRight
  , rightMapLeft

    -- * Rule Count
  , eitherRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All either-related rules.
eitherRules :: [Rule]
eitherRules = mconcat
  [ eitherSimplificationRules
  , eitherConversionRules
  , eitherPatternRules
  ]

-- | Total count of either rules.
eitherRuleCount :: Int
eitherRuleCount = length eitherRules

--------------------------------------------------------------------------------
-- Either Simplifications
--------------------------------------------------------------------------------

-- | Rules for either simplifications.
eitherSimplificationRules :: [Rule]
eitherSimplificationRules =
  [ eitherLeftRight
  , eitherIdId
  , eitherConstFalseTrue
  , eitherConstTrueFalse
  , eitherLeftFmap
  , eitherFirstRight
  , isLeftLeft
  , isLeftRight
  , isRightLeft
  , isRightRight
  , fromLeftLeft
  , fromLeftRight
  , fromRightLeft
  , fromRightRight
  ]

-- | either Left Right e ==> e
eitherLeftRight :: Rule
eitherLeftRight =
  rule "either-left-right" $
    match ("either Left Right _e" ==> "_e")
    & category Style
    & severity Suggestion
    & message "either Left Right is identity"

-- | either id id e - suggest fromLeft/fromRight.
eitherIdId :: Rule
eitherIdId =
  rule "either-id-id" $
    match ("either id id _e" ==> "either id id _e")
    & category Style
    & severity Info
    & message "either id id - types must match, consider fromLeft/fromRight"
    & safetyLevel ManualReview

-- | either (const False) (const True) e ==> isRight e
eitherConstFalseTrue :: Rule
eitherConstFalseTrue =
  rule "either-const-false-true" $
    match ("either (const False) (const True) _e" ==> "isRight _e")
    & category Style
    & severity Suggestion
    & message "Use isRight instead"

-- | either (const True) (const False) e ==> isLeft e
eitherConstTrueFalse :: Rule
eitherConstTrueFalse =
  rule "either-const-true-false" $
    match ("either (const True) (const False) _e" ==> "isLeft _e")
    & category Style
    & severity Suggestion
    & message "Use isLeft instead"

-- | either Left (Right . f) e ==> fmap f e
eitherLeftFmap :: Rule
eitherLeftFmap =
  rule "either-left-fmap" $
    match ("either Left (Right . _f) _e" ==> "fmap _f _e")
    & category Style
    & severity Suggestion
    & message "Use fmap instead of either Left (Right . f)"

-- | either (Left . f) Right e ==> first f e
eitherFirstRight :: Rule
eitherFirstRight =
  rule "either-first-right" $
    match ("either (Left . _f) Right _e" ==> "first _f _e")
    & category Style
    & severity Suggestion
    & message "Use first instead of either (Left . f) Right"

-- | isLeft (Left x) ==> True
isLeftLeft :: Rule
isLeftLeft =
  rule "isLeft-left" $
    match ("isLeft (Left _x)" ==> "True")
    & category Style
    & severity Suggestion
    & message "isLeft (Left x) is always True"

-- | isLeft (Right x) ==> False
isLeftRight :: Rule
isLeftRight =
  rule "isLeft-right" $
    match ("isLeft (Right _x)" ==> "False")
    & category Style
    & severity Suggestion
    & message "isLeft (Right x) is always False"

-- | isRight (Left x) ==> False
isRightLeft :: Rule
isRightLeft =
  rule "isRight-left" $
    match ("isRight (Left _x)" ==> "False")
    & category Style
    & severity Suggestion
    & message "isRight (Left x) is always False"

-- | isRight (Right x) ==> True
isRightRight :: Rule
isRightRight =
  rule "isRight-right" $
    match ("isRight (Right _x)" ==> "True")
    & category Style
    & severity Suggestion
    & message "isRight (Right x) is always True"

-- | fromLeft x (Left y) ==> y
fromLeftLeft :: Rule
fromLeftLeft =
  rule "fromLeft-left" $
    match ("fromLeft _x (Left _y)" ==> "_y")
    & category Style
    & severity Suggestion
    & message "fromLeft on Left returns the value"

-- | fromLeft x (Right y) ==> x
fromLeftRight :: Rule
fromLeftRight =
  rule "fromLeft-right" $
    match ("fromLeft _x (Right _y)" ==> "_x")
    & category Style
    & severity Suggestion
    & message "fromLeft on Right returns default"

-- | fromRight x (Left y) ==> x
fromRightLeft :: Rule
fromRightLeft =
  rule "fromRight-left" $
    match ("fromRight _x (Left _y)" ==> "_x")
    & category Style
    & severity Suggestion
    & message "fromRight on Left returns default"

-- | fromRight x (Right y) ==> y
fromRightRight :: Rule
fromRightRight =
  rule "fromRight-right" $
    match ("fromRight _x (Right _y)" ==> "_y")
    & category Style
    & severity Suggestion
    & message "fromRight on Right returns the value"

--------------------------------------------------------------------------------
-- Either Conversions
--------------------------------------------------------------------------------

-- | Rules for Either conversions.
eitherConversionRules :: [Rule]
eitherConversionRules =
  [ leftsSingletonLeft
  , leftsSingletonRight
  , rightsSingletonLeft
  , rightsSingletonRight
  , partitionEithersLeft
  , partitionEithersRight
  , eitherToMaybeLeft
  , eitherToMaybeRight
  , maybeToEitherRight
  , maybeToEitherLeft
  , caseEither
  ]

-- | lefts [Left x] ==> [x]
leftsSingletonLeft :: Rule
leftsSingletonLeft =
  rule "lefts-singleton-left" $
    match ("lefts [Left _x]" ==> "[_x]")
    & category Style
    & severity Suggestion
    & message "lefts [Left x] is [x]"

-- | lefts [Right x] ==> []
leftsSingletonRight :: Rule
leftsSingletonRight =
  rule "lefts-singleton-right" $
    match ("lefts [Right _x]" ==> "[]")
    & category Style
    & severity Suggestion
    & message "lefts [Right x] is []"

-- | rights [Left x] ==> []
rightsSingletonLeft :: Rule
rightsSingletonLeft =
  rule "rights-singleton-left" $
    match ("rights [Left _x]" ==> "[]")
    & category Style
    & severity Suggestion
    & message "rights [Left x] is []"

-- | rights [Right x] ==> [x]
rightsSingletonRight :: Rule
rightsSingletonRight =
  rule "rights-singleton-right" $
    match ("rights [Right _x]" ==> "[_x]")
    & category Style
    & severity Suggestion
    & message "rights [Right x] is [x]"

-- | partitionEithers [Left x] ==> ([x], [])
partitionEithersLeft :: Rule
partitionEithersLeft =
  rule "partitionEithers-left" $
    match ("partitionEithers [Left _x]" ==> "([_x], [])")
    & category Style
    & severity Suggestion
    & message "partitionEithers [Left x] is ([x], [])"

-- | partitionEithers [Right x] ==> ([], [x])
partitionEithersRight :: Rule
partitionEithersRight =
  rule "partitionEithers-right" $
    match ("partitionEithers [Right _x]" ==> "([], [_x])")
    & category Style
    & severity Suggestion
    & message "partitionEithers [Right x] is ([], [x])"

-- | either Just (const Nothing) e - leftToMaybe.
eitherToMaybeLeft :: Rule
eitherToMaybeLeft =
  rule "either-to-maybe-left" $
    match ("either Just (const Nothing) _e" ==> "leftToMaybe _e")
    & category Style
    & severity Suggestion
    & message "Use leftToMaybe from Data.Either.Combinators"

-- | either (const Nothing) Just e - rightToMaybe.
eitherToMaybeRight :: Rule
eitherToMaybeRight =
  rule "either-to-maybe-right" $
    match ("either (const Nothing) Just _e" ==> "rightToMaybe _e")
    & category Style
    & severity Suggestion
    & message "Use rightToMaybe from Data.Either.Combinators"

-- | maybe (Left x) Right m ==> maybeToRight x m
maybeToEitherRight :: Rule
maybeToEitherRight =
  rule "maybe-to-either-right" $
    match ("maybe (Left _x) Right _m" ==> "maybeToRight _x _m")
    & category Style
    & severity Suggestion
    & message "Use maybeToRight from Data.Either.Combinators"

-- | maybe (Right x) Left m ==> maybeToLeft x m
maybeToEitherLeft :: Rule
maybeToEitherLeft =
  rule "maybe-to-either-left" $
    match ("maybe (Right _x) Left _m" ==> "maybeToLeft _x _m")
    & category Style
    & severity Suggestion
    & message "Use maybeToLeft from Data.Either.Combinators"

-- | case e of { Left x -> f x; Right y -> g y } ==> either f g e
caseEither :: Rule
caseEither =
  rule "case-either" $
    matchText "case\\s+[^o]+of\\s*\\{?\\s*Left\\s+[a-z_]+\\s*->.*Right\\s+[a-z_]+\\s*->"
    & category Style
    & severity Suggestion
    & message "Case on Either can be 'either f g'"

--------------------------------------------------------------------------------
-- Either Pattern Rules
--------------------------------------------------------------------------------

-- | Rules for Either patterns.
eitherPatternRules :: [Rule]
eitherPatternRules =
  [ eitherBimapId
  , eitherFirstId
  , eitherSecondId
  , eitherFmapId
  , bitraverseEither
  , firstMaybeEither
  , secondMaybeEither
  , eitherSwap
  , leftMapRight
  , rightMapLeft
  ]

-- | bimap id id e ==> e
eitherBimapId :: Rule
eitherBimapId =
  rule "either-bimap-id" $
    match ("bimap id id _e" ==> "_e")
    & category Style
    & severity Suggestion
    & message "bimap id id is identity"

-- | first id e ==> e
eitherFirstId :: Rule
eitherFirstId =
  rule "either-first-id" $
    match ("first id _e" ==> "_e")
    & category Style
    & severity Suggestion
    & message "first id is identity"

-- | second id e ==> e
eitherSecondId :: Rule
eitherSecondId =
  rule "either-second-id" $
    match ("second id _e" ==> "_e")
    & category Style
    & severity Suggestion
    & message "second id is identity"

-- | fmap id e ==> e (for Either)
eitherFmapId :: Rule
eitherFmapId =
  rule "either-fmap-id" $
    matchText "fmap\\s+id\\s+[a-z_]+"
    & category Style
    & severity Suggestion
    & message "fmap id is identity"

-- | bitraverse pattern.
bitraverseEither :: Rule
bitraverseEither =
  rule "bitraverse-either" $
    matchText "bitraverse"
    & category Style
    & severity Info
    & message "Using bitraverse on Either/tuple"
    & safetyLevel ManualReview

-- | first from Maybe on Either.
firstMaybeEither :: Rule
firstMaybeEither =
  rule "first-maybe-either" $
    matchText "first.*Either"
    & category Style
    & severity Info
    & message "Using first on Either"
    & safetyLevel ManualReview

-- | second on Either.
secondMaybeEither :: Rule
secondMaybeEither =
  rule "second-maybe-either" $
    matchText "second.*Either"
    & category Style
    & severity Info
    & message "Using second on Either"
    & safetyLevel ManualReview

-- | Swapping Either.
eitherSwap :: Rule
eitherSwap =
  rule "either-swap" $
    match ("either Right Left _e" ==> "either Right Left _e")
    & category Style
    & severity Info
    & message "Swapping Either - consider swap from Data.Either.Combinators"
    & safetyLevel ManualReview

-- | Left (fmap f x) pattern.
leftMapRight :: Rule
leftMapRight =
  rule "left-map-right" $
    matchText "Left\\s*\\(\\s*fmap"
    & category Style
    & severity Info
    & message "Mapping inside Left constructor"
    & safetyLevel ManualReview

-- | Right (fmap f x) pattern.
rightMapLeft :: Rule
rightMapLeft =
  rule "right-map-left" $
    matchText "Right\\s*\\(\\s*fmap"
    & category Style
    & severity Info
    & message "Mapping inside Right constructor"
    & safetyLevel ManualReview
