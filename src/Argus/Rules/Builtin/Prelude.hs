{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Prelude
-- Description : Prelude function modernization and safety rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for modernizing Prelude usage and catching common Prelude pitfalls.
-- Suggests safer alternatives and more efficient patterns.
--
-- == Rule Categories
--
-- * __Partial Functions__: Avoid partial Prelude functions
-- * __Modernization__: Use modern alternatives
-- * __Boolean__: Boolean expression simplification
-- * __Maybe/Either__: Maybe and Either patterns

module Argus.Rules.Builtin.Prelude
  ( -- * Rule Sets
    preludeRules
  , partialPreludeRules
  , modernPreludeRules
  , booleanRules
  , maybeEitherRules

    -- * Partial Functions
  , unsafeHead
  , unsafeTail
  , unsafeInit
  , unsafeLast
  , unsafeIndex
  , unsafeFromJust
  , unsafeRead
  , unsafeMinimum
  , unsafeMaximum
  , unsafeFoldr1
  , unsafeFoldl1

    -- * Modernization
  , errorToException
  , undefinedWarning
  , printToText
  , putStrToText
  , appendFilePath
  , seqToDeepseq
  , preludeReturnToPure
  , preludeMapMToTraverse
  , preludeSequenceToSequenceA

    -- * Boolean
  , notNotElim
  , andToAll
  , orToAny
  , ifToBool
  , boolToIf
  , guardToBool
  , compareToBool

    -- * Maybe/Either
  , maybeToFromMaybe
  , fromMaybeToMaybe
  , isJustToMaybe
  , isNothingToMaybe
  , eitherToEither
  , leftToEither
  , rightToEither
  , maybeToEither
  , eitherToMaybe

    -- * Rule Count
  , preludeRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All Prelude-related rules.
preludeRules :: [Rule]
preludeRules = mconcat
  [ partialPreludeRules
  , modernPreludeRules
  , booleanRules
  , maybeEitherRules
  ]

-- | Total count of Prelude rules.
preludeRuleCount :: Int
preludeRuleCount = length preludeRules

--------------------------------------------------------------------------------
-- Partial Prelude Rules
--------------------------------------------------------------------------------

-- | Rules for avoiding partial Prelude functions.
partialPreludeRules :: [Rule]
partialPreludeRules =
  [ unsafeHead
  , unsafeTail
  , unsafeInit
  , unsafeLast
  , unsafeIndex
  , unsafeFromJust
  , unsafeRead
  , unsafeMinimum
  , unsafeMaximum
  , unsafeFoldr1
  , unsafeFoldl1
  ]

-- | head is partial.
--
-- @
-- head xs  ==>  headMay xs  or  listToMaybe xs
-- @
unsafeHead :: Rule
unsafeHead =
  rule "unsafe-head" $
    match ("head _xs" ==> "listToMaybe _xs")
    & category Safety
    & severity Warning
    & message "head throws on empty list"
    & note "Use listToMaybe, headMay, or pattern matching"

-- | tail is partial.
--
-- @
-- tail xs  ==>  drop 1 xs  or  tailMay xs
-- @
unsafeTail :: Rule
unsafeTail =
  rule "unsafe-tail" $
    match ("tail _xs" ==> "drop 1 _xs")
    & category Safety
    & severity Warning
    & message "tail throws on empty list"
    & note "Use drop 1, tailMay, or pattern matching"

-- | init is partial.
--
-- @
-- init xs  ==>  initMay xs
-- @
unsafeInit :: Rule
unsafeInit =
  rule "unsafe-init" $
    match ("init _xs" ==> "init _xs")
    & category Safety
    & severity Warning
    & message "init throws on empty list"
    & note "Use initMay or pattern matching"
    & safetyLevel ManualReview

-- | last is partial.
--
-- @
-- last xs  ==>  lastMay xs
-- @
unsafeLast :: Rule
unsafeLast =
  rule "unsafe-last" $
    match ("last _xs" ==> "last _xs")
    & category Safety
    & severity Warning
    & message "last throws on empty list"
    & note "Use lastMay or pattern matching"
    & safetyLevel ManualReview

-- | (!!) is partial.
--
-- @
-- xs !! n  ==>  xs !? n  or  atMay xs n
-- @
unsafeIndex :: Rule
unsafeIndex =
  rule "unsafe-index" $
    match ("_xs !! _n" ==> "_xs !! _n")
    & category Safety
    & severity Warning
    & message "(!!) throws on out-of-bounds index"
    & note "Use (!?) or atMay for safe indexing"
    & safetyLevel ManualReview

-- | fromJust is partial.
--
-- @
-- fromJust mx  ==>  fromMaybe defaultValue mx
-- @
unsafeFromJust :: Rule
unsafeFromJust =
  rule "unsafe-fromJust" $
    match ("fromJust _mx" ==> "fromJust _mx")
    & category Safety
    & severity Warning
    & message "fromJust throws on Nothing"
    & note "Use fromMaybe, maybe, or pattern matching"
    & safetyLevel ManualReview

-- | read is partial.
--
-- @
-- read s  ==>  readMaybe s
-- @
unsafeRead :: Rule
unsafeRead =
  rule "unsafe-read" $
    match ("read _s" ==> "readMaybe _s")
    & category Safety
    & severity Warning
    & message "read throws on parse failure"
    & note "Use readMaybe from Text.Read"

-- | minimum is partial.
--
-- @
-- minimum xs  -- throws on empty
-- @
unsafeMinimum :: Rule
unsafeMinimum =
  rule "unsafe-minimum" $
    match ("minimum _xs" ==> "minimum _xs")
    & category Safety
    & severity Warning
    & message "minimum throws on empty Foldable"
    & note "Use minimumMay or check for null first"
    & safetyLevel ManualReview

-- | maximum is partial.
--
-- @
-- maximum xs  -- throws on empty
-- @
unsafeMaximum :: Rule
unsafeMaximum =
  rule "unsafe-maximum" $
    match ("maximum _xs" ==> "maximum _xs")
    & category Safety
    & severity Warning
    & message "maximum throws on empty Foldable"
    & note "Use maximumMay or check for null first"
    & safetyLevel ManualReview

-- | foldr1 is partial.
--
-- @
-- foldr1 f xs  -- throws on empty
-- @
unsafeFoldr1 :: Rule
unsafeFoldr1 =
  rule "unsafe-foldr1" $
    match ("foldr1 _f _xs" ==> "foldr1 _f _xs")
    & category Safety
    & severity Warning
    & message "foldr1 throws on empty Foldable"
    & note "Use foldr with an explicit base case"
    & safetyLevel ManualReview

-- | foldl1 is partial.
--
-- @
-- foldl1 f xs  -- throws on empty
-- @
unsafeFoldl1 :: Rule
unsafeFoldl1 =
  rule "unsafe-foldl1" $
    match ("foldl1 _f _xs" ==> "foldl1 _f _xs")
    & category Safety
    & severity Warning
    & message "foldl1 throws on empty Foldable"
    & note "Use foldl' with an explicit base case"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Modern Prelude Rules
--------------------------------------------------------------------------------

-- | Rules for modernizing Prelude usage.
modernPreludeRules :: [Rule]
modernPreludeRules =
  [ errorToException
  , undefinedWarning
  , printToText
  , putStrToText
  , appendFilePath
  , seqToDeepseq
  , preludeReturnToPure
  , preludeMapMToTraverse
  , preludeSequenceToSequenceA
  ]

-- | error should be exceptional.
--
-- @
-- error "message"  -- Consider exceptions or Either
-- @
errorToException :: Rule
errorToException =
  rule "error-to-exception" $
    match ("error _msg" ==> "error _msg")
    & category Safety
    & severity Warning
    & message "error throws an exception"
    & note "Consider using Either, Maybe, or typed exceptions"
    & safetyLevel ManualReview

-- | undefined should not be in production code.
--
-- @
-- undefined  -- Placeholder that throws
-- @
undefinedWarning :: Rule
undefinedWarning =
  rule "undefined-warning" $
    match ("undefined" ==> "undefined")
    & category Safety
    & severity Error
    & message "undefined in code - this will throw at runtime"
    & note "Replace with actual implementation or typed hole"
    & safetyLevel ManualReview

-- | print uses String Show.
--
-- @
-- print x  -- Uses String-based Show
-- @
printToText :: Rule
printToText =
  rule "print-to-text" $
    match ("print _x" ==> "print _x")
    & category Style
    & severity Info
    & message "print uses String-based Show instance"
    & note "Consider text-based printing for performance"
    & safetyLevel ManualReview

-- | putStr uses String.
--
-- @
-- putStr s  -- String-based
-- @
putStrToText :: Rule
putStrToText =
  rule "putStr-to-text" $
    match ("putStr _s" ==> "T.putStr _s")
    & category Performance
    & severity Info
    & message "putStr uses String; consider T.putStr for Text"
    & safetyLevel ManualReview

-- | FilePath is just String.
--
-- @
-- FilePath  -- Type alias for String
-- @
appendFilePath :: Rule
appendFilePath =
  rule "filepath-string" $
    matchText ":: FilePath"
    & category Style
    & severity Info
    & message "FilePath is a String type alias"
    & note "Consider using typed path libraries like path or filepath"
    & safetyLevel ManualReview

-- | seq vs deepseq.
--
-- @
-- seq x y  -- Only evaluates to WHNF
-- @
seqToDeepseq :: Rule
seqToDeepseq =
  rule "seq-to-deepseq" $
    match ("seq _x _y" ==> "seq _x _y")
    & category Performance
    & severity Info
    & message "seq only evaluates to WHNF"
    & note "For full evaluation, use deepseq from Control.DeepSeq"
    & safetyLevel ManualReview

-- | return to pure.
--
-- @
-- return x  ==>  pure x
-- @
preludeReturnToPure :: Rule
preludeReturnToPure =
  rule "prelude-return-to-pure" $
    match ("return _x" ==> "pure _x")
    & category Modernization
    & severity Suggestion
    & message "Use pure instead of return"
    & note "pure only requires Applicative, not Monad"

-- | mapM to traverse.
--
-- @
-- mapM f xs  ==>  traverse f xs
-- @
preludeMapMToTraverse :: Rule
preludeMapMToTraverse =
  rule "prelude-mapM-to-traverse" $
    match ("mapM _f _xs" ==> "traverse _f _xs")
    & category Modernization
    & severity Suggestion
    & message "Use traverse instead of mapM"
    & note "traverse only requires Applicative, not Monad"

-- | sequence to sequenceA.
--
-- @
-- sequence xs  ==>  sequenceA xs
-- @
preludeSequenceToSequenceA :: Rule
preludeSequenceToSequenceA =
  rule "prelude-sequence-to-sequenceA" $
    match ("sequence _xs" ==> "sequenceA _xs")
    & category Modernization
    & severity Suggestion
    & message "Use sequenceA instead of sequence"
    & note "sequenceA only requires Applicative, not Monad"

--------------------------------------------------------------------------------
-- Boolean Rules
--------------------------------------------------------------------------------

-- | Rules for boolean expression simplification.
booleanRules :: [Rule]
booleanRules =
  [ notNotElim
  , andToAll
  , orToAny
  , ifToBool
  , boolToIf
  , guardToBool
  , compareToBool
  ]

-- | Double negation elimination.
--
-- @
-- not (not x)  ==>  x
-- @
notNotElim :: Rule
notNotElim =
  rule "not-not-elim" $
    match ("not (not _x)" ==> "_x")
    & category Style
    & severity Suggestion
    & message "Double negation cancels out"

-- | Use and instead of foldr (&&) True.
--
-- @
-- all id [x, y, z]  ==>  and [x, y, z]
-- @
andToAll :: Rule
andToAll =
  rule "all-id-to-and" $
    match ("all id _xs" ==> "and _xs")
    & category Style
    & severity Suggestion
    & message "Use and instead of all id"

-- | Use any instead of foldr (||) False.
--
-- @
-- any id [x, y, z]  ==>  or [x, y, z]
-- @
orToAny :: Rule
orToAny =
  rule "any-id-to-or" $
    match ("any id _xs" ==> "or _xs")
    & category Style
    & severity Suggestion
    & message "Use or instead of any id"

-- | Simplify if returning Bool.
--
-- @
-- if cond then True else False  ==>  cond
-- @
ifToBool :: Rule
ifToBool =
  rule "if-to-bool" $
    match ("if _cond then True else False" ==> "_cond")
    & category Style
    & severity Suggestion
    & message "if cond then True else False is just cond"

-- | Simplify if to not.
--
-- @
-- if cond then False else True  ==>  not cond
-- @
boolToIf :: Rule
boolToIf =
  rule "if-to-not" $
    match ("if _cond then False else True" ==> "not _cond")
    & category Style
    & severity Suggestion
    & message "if cond then False else True is not cond"

-- | Guard to bool.
--
-- @
-- f x | cond = True | otherwise = False  ==>  f x = cond
-- @
guardToBool :: Rule
guardToBool =
  rule "guard-to-bool" $
    matchText "\\| .+ = True[\\s\\S]*\\| otherwise = False"
    & category Style
    & severity Suggestion
    & message "Guards returning True/False can be simplified"

-- | compare to Bool.
--
-- @
-- x == y  -- Returns Bool
-- @
compareToBool :: Rule
compareToBool =
  rule "compare-to-bool" $
    match ("compare _x _y == EQ" ==> "_x == _y")
    & category Style
    & severity Suggestion
    & message "Use (==) instead of compare _ _ == EQ"

--------------------------------------------------------------------------------
-- Maybe/Either Rules
--------------------------------------------------------------------------------

-- | Rules for Maybe and Either patterns.
maybeEitherRules :: [Rule]
maybeEitherRules =
  [ maybeToFromMaybe
  , fromMaybeToMaybe
  , isJustToMaybe
  , isNothingToMaybe
  , eitherToEither
  , leftToEither
  , rightToEither
  , maybeToEither
  , eitherToMaybe
  ]

-- | Use fromMaybe for default value.
--
-- @
-- case mx of { Just x -> x; Nothing -> def }  ==>  fromMaybe def mx
-- @
maybeToFromMaybe :: Rule
maybeToFromMaybe =
  rule "maybe-to-fromMaybe" $
    match ("maybe _def id _mx" ==> "fromMaybe _def _mx")
    & category Style
    & severity Suggestion
    & message "Use fromMaybe instead of maybe def id"

-- | fromMaybe with const.
--
-- @
-- fromMaybe x (Just y)  ==>  y
-- @
fromMaybeToMaybe :: Rule
fromMaybeToMaybe =
  rule "fromMaybe-Just" $
    match ("fromMaybe _x (Just _y)" ==> "_y")
    & category Style
    & severity Suggestion
    & message "fromMaybe on Just is redundant"

-- | isJust pattern.
--
-- @
-- isJust mx && ...  -- Consider pattern matching
-- @
isJustToMaybe :: Rule
isJustToMaybe =
  rule "isJust-to-pattern" $
    match ("isJust _mx" ==> "isJust _mx")
    & category Style
    & severity Info
    & message "Consider pattern matching instead of isJust"
    & safetyLevel ManualReview

-- | isNothing pattern.
--
-- @
-- isNothing mx  ==>  not (isJust mx)  or  mx == Nothing
-- @
isNothingToMaybe :: Rule
isNothingToMaybe =
  rule "isNothing-pattern" $
    match ("isNothing _mx" ==> "isNothing _mx")
    & category Style
    & severity Info
    & message "Consider pattern matching instead of isNothing"
    & safetyLevel ManualReview

-- | either identity.
--
-- @
-- either Left Right ex  ==>  ex
-- @
eitherToEither :: Rule
eitherToEither =
  rule "either-identity" $
    match ("either Left Right _ex" ==> "_ex")
    & category Style
    & severity Suggestion
    & message "either Left Right is identity"

-- | isLeft pattern.
--
-- @
-- isLeft ex  -- Consider pattern matching
-- @
leftToEither :: Rule
leftToEither =
  rule "isLeft-pattern" $
    match ("isLeft _ex" ==> "isLeft _ex")
    & category Style
    & severity Info
    & message "Consider pattern matching instead of isLeft"
    & safetyLevel ManualReview

-- | isRight pattern.
--
-- @
-- isRight ex  -- Consider pattern matching
-- @
rightToEither :: Rule
rightToEither =
  rule "isRight-pattern" $
    match ("isRight _ex" ==> "isRight _ex")
    & category Style
    & severity Info
    & message "Consider pattern matching instead of isRight"
    & safetyLevel ManualReview

-- | Maybe to Either conversion.
--
-- @
-- maybe (Left err) Right mx  ==>  note err mx
-- @
maybeToEither :: Rule
maybeToEither =
  rule "maybe-to-either" $
    match ("maybe (Left _err) Right _mx" ==> "note _err _mx")
    & category Style
    & severity Suggestion
    & message "Use note for Maybe to Either conversion"
    & note "note is from Data.Either.Extra or errors package"

-- | Either to Maybe conversion.
--
-- @
-- either (const Nothing) Just ex  ==>  hush ex
-- @
eitherToMaybe :: Rule
eitherToMaybe =
  rule "either-to-maybe" $
    match ("either (const Nothing) Just _ex" ==> "hush _ex")
    & category Style
    & severity Suggestion
    & message "Use hush for Either to Maybe conversion"
    & note "hush is from Data.Either.Extra or errors package"
