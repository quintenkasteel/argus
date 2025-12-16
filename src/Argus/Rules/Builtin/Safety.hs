{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Safety
-- Description : Safety rules for detecting partial functions and unsafe operations
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- This module provides lint rules for detecting partial functions and other
-- safety hazards in Haskell code. Partial functions are functions that can
-- throw exceptions or fail at runtime for some inputs.
--
-- == Rule Categories
--
-- The safety rules are organized into several categories:
--
-- * __Partial List Functions__: @head@, @tail@, @init@, @last@, @!!@
-- * __Partial Maybe Functions__: @fromJust@, @fromMaybe@ with bottom
-- * __Partial Pattern Matching__: Incomplete patterns, irrefutable patterns
-- * __Unsafe Operations__: @error@, @undefined@, @throw@
--
-- == Severity Levels
--
-- * 'Error': Code that will definitely fail at runtime
-- * 'Warning': Code that may fail at runtime
-- * 'Suggestion': Safer alternatives available
--
-- == Examples
--
-- @
-- -- Before: Partial function usage
-- firstElement = head xs  -- Fails on empty list!
--
-- -- After: Safe alternative
-- firstElement = headMay xs  -- Returns Nothing on empty list
-- @
--
-- == References
--
-- * <https://wiki.haskell.org/Avoiding_partial_functions Avoiding Partial Functions>
-- * <https://hackage.haskell.org/package/safe safe package>
-- * <https://www.parsonsmatt.org/2017/10/11/type_safety_back_and_forth.html Type Safety Back and Forth>

module Argus.Rules.Builtin.Safety
  ( -- * Rule Sets
    safetyRules
  , partialFunctionRules
  , unsafeOperationRules

    -- * Individual Rules
    -- ** Partial List Functions
  , avoidHead
  , avoidTail
  , avoidInit
  , avoidLast
  , avoidBangBang
  , avoidMaximum
  , avoidMinimum
  , avoidFoldr1
  , avoidFoldl1

    -- ** Partial Maybe Functions
  , avoidFromJust

    -- ** Partial Read Functions
  , avoidRead

    -- ** Partial Numeric Functions
  , avoidSucc
  , avoidPred
  , avoidToEnum
  , avoidGenericIndex

    -- ** Partial List Generators
  , avoidCycle

    -- ** Unsafe Operations
  , avoidError
  , avoidUndefined
  , avoidThrow
  , avoidAssert

    -- * Rule Metadata
  , safetyRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All safety rules combined.
--
-- This is the recommended set for most projects. It includes rules for
-- detecting partial functions and unsafe operations that can cause runtime
-- failures.
--
-- @
-- -- Enable all safety rules in your configuration
-- rules = safetyRules
-- @
safetyRules :: [Rule]
safetyRules = partialFunctionRules ++ unsafeOperationRules

-- | Rules for detecting partial function usage.
--
-- Partial functions are functions that can fail or throw exceptions for
-- some inputs. These rules suggest safer alternatives from the @safe@ package
-- or standard library patterns.
partialFunctionRules :: [Rule]
partialFunctionRules =
  [ avoidHead
  , avoidTail
  , avoidInit
  , avoidLast
  , avoidBangBang
  , avoidMaximum
  , avoidMinimum
  , avoidFoldr1
  , avoidFoldl1
  , avoidFromJust
  , avoidRead
  , avoidSucc
  , avoidPred
  , avoidToEnum
  , avoidGenericIndex
  , avoidCycle
  ]

-- | Rules for detecting unsafe operations.
--
-- These operations explicitly fail at runtime and should be avoided in
-- production code. They may be acceptable in tests or during development.
unsafeOperationRules :: [Rule]
unsafeOperationRules =
  [ avoidError
  , avoidUndefined
  , avoidThrow
  , avoidAssert
  ]

--------------------------------------------------------------------------------
-- Partial List Functions
--------------------------------------------------------------------------------

-- | Detect usage of 'head' on lists.
--
-- == Problem
--
-- @head@ throws an exception on empty lists:
--
-- @
-- head []  -- *** Exception: Prelude.head: empty list
-- @
--
-- == Solution
--
-- Use \"headMay\" from the @safe@ package, pattern matching, or @listToMaybe@:
--
-- @
-- import Safe (headMay)
--
-- -- Safe alternative
-- headMay []       -- Nothing
-- headMay [1,2,3]  -- Just 1
--
-- -- Pattern matching alternative
-- case xs of
--   (x:_) -> Just x
--   []    -> Nothing
--
-- -- Standard library alternative
-- import Data.Maybe (listToMaybe)
-- listToMaybe xs
-- @
--
-- == Metadata
--
-- * __Category__: Safety
-- * __Severity__: Warning
-- * __Safe to auto-fix__: Yes, with @safe@ package import
-- * __Since__: 1.0.0
avoidHead :: Rule
avoidHead = rule "safety/avoid-head" $
  match ("head $XS" ==> "headMay $XS"
         `fromModule` "Prelude"
         `addImport` ("Safe", ["headMay"]))
  & severity Warning
  & message "Avoid partial function 'head' - throws exception on empty list"
  & note "Use \"headMay\" from the \"safe\" package, or pattern match explicitly"
  & category Safety
  & safetyLevel Safe
  & fixDescription "Replace with 'headMay' from Safe package"

-- | Detect usage of 'tail' on lists.
--
-- == Problem
--
-- @tail@ throws an exception on empty lists:
--
-- @
-- tail []  -- *** Exception: Prelude.tail: empty list
-- @
--
-- == Solution
--
-- Use @tailMay@ from the @safe@ package or pattern matching:
--
-- @
-- import Safe (tailMay)
--
-- tailMay []       -- Nothing
-- tailMay [1,2,3]  -- Just [2,3]
-- @
--
-- == Metadata
--
-- * __Category__: Safety
-- * __Severity__: Warning
-- * __Safe to auto-fix__: Yes
-- * __Since__: 1.0.0
avoidTail :: Rule
avoidTail = rule "safety/avoid-tail" $
  match ("tail $XS" ==> "tailMay $XS"
         `fromModule` "Prelude"
         `addImport` ("Safe", ["tailMay"]))
  & severity Warning
  & message "Avoid partial function 'tail' - throws exception on empty list"
  & note "Use @tailMay@ from the safe package, or pattern match explicitly"
  & category Safety
  & safetyLevel Safe
  & fixDescription "Replace with @tailMay@ from Safe package"

-- | Detect usage of 'init' on lists.
--
-- == Problem
--
-- @init@ throws an exception on empty lists:
--
-- @
-- init []  -- *** Exception: Prelude.init: empty list
-- @
--
-- == Solution
--
-- Use @initMay@ from the @safe@ package:
--
-- @
-- import Safe (initMay)
--
-- initMay []       -- Nothing
-- initMay [1,2,3]  -- Just [1,2]
-- @
avoidInit :: Rule
avoidInit = rule "safety/avoid-init" $
  match ("init $XS" ==> "initMay $XS"
         `fromModule` "Prelude"
         `addImport` ("Safe", ["initMay"]))
  & severity Warning
  & message "Avoid partial function 'init' - throws exception on empty list"
  & note "Use @initMay@ from the safe package"
  & category Safety
  & safetyLevel Safe

-- | Detect usage of 'last' on lists.
--
-- == Problem
--
-- @last@ throws an exception on empty lists:
--
-- @
-- last []  -- *** Exception: Prelude.last: empty list
-- @
--
-- == Solution
--
-- Use @lastMay@ from the @safe@ package:
--
-- @
-- import Safe (lastMay)
--
-- lastMay []       -- Nothing
-- lastMay [1,2,3]  -- Just 3
-- @
avoidLast :: Rule
avoidLast = rule "safety/avoid-last" $
  match ("last $XS" ==> "lastMay $XS"
         `fromModule` "Prelude"
         `addImport` ("Safe", ["lastMay"]))
  & severity Warning
  & message "Avoid partial function 'last' - throws exception on empty list"
  & note "Use @lastMay@ from the safe package"
  & category Safety
  & safetyLevel Safe

-- | Detect usage of '!!' for list indexing.
--
-- == Problem
--
-- @!!@ throws an exception when the index is out of bounds:
--
-- @
-- [1,2,3] !! 5  -- *** Exception: Prelude.!!: index too large
-- @
--
-- == Solution
--
-- Use @atMay@ from the @safe@ package or @!?@ from containers:
--
-- @
-- import Safe (atMay)
--
-- atMay [1,2,3] 1  -- Just 2
-- atMay [1,2,3] 5  -- Nothing
-- @
avoidBangBang :: Rule
avoidBangBang = rule "safety/avoid-!!" $
  match ("$XS !! $N" ==> "atMay $XS $N"
         `fromModule` "Prelude"
         `addImport` ("Safe", ["atMay"]))
  & severity Warning
  & message "Avoid partial operator '!!' - throws exception on invalid index"
  & note "Use @atMay@ from the safe package for safe indexing"
  & category Safety
  & safetyLevel Safe

-- | Detect usage of 'maximum' on lists.
--
-- == Problem
--
-- @maximum@ throws an exception on empty lists:
--
-- @
-- maximum []  -- *** Exception: Prelude.maximum: empty list
-- @
--
-- == Solution
--
-- Use @maximumMay@ from the @safe@ package:
--
-- @
-- import Safe (maximumMay)
--
-- maximumMay []       -- Nothing
-- maximumMay [1,2,3]  -- Just 3
-- @
avoidMaximum :: Rule
avoidMaximum = rule "safety/avoid-maximum" $
  match ("maximum $XS" ==> "maximumMay $XS"
         `fromModule` "Prelude"
         `addImport` ("Safe", ["maximumMay"]))
  & severity Warning
  & message "Avoid partial function 'maximum' - throws exception on empty list"
  & note "Use @maximumMay@ from the safe package"
  & category Safety
  & safetyLevel Safe

-- | Detect usage of 'minimum' on lists.
--
-- == Problem
--
-- @minimum@ throws an exception on empty lists:
--
-- @
-- minimum []  -- *** Exception: Prelude.minimum: empty list
-- @
--
-- == Solution
--
-- Use @minimumMay@ from the @safe@ package:
--
-- @
-- import Safe (minimumMay)
--
-- minimumMay []       -- Nothing
-- minimumMay [1,2,3]  -- Just 1
-- @
avoidMinimum :: Rule
avoidMinimum = rule "safety/avoid-minimum" $
  match ("minimum $XS" ==> "minimumMay $XS"
         `fromModule` "Prelude"
         `addImport` ("Safe", ["minimumMay"]))
  & severity Warning
  & message "Avoid partial function 'minimum' - throws exception on empty list"
  & note "Use @minimumMay@ from the safe package"
  & category Safety
  & safetyLevel Safe

-- | Detect usage of 'foldr1' on lists.
--
-- == Problem
--
-- @foldr1@ throws an exception on empty lists:
--
-- @
-- foldr1 (+) []  -- *** Exception: Prelude.foldr1: empty list
-- @
--
-- == Solution
--
-- Use @foldr1May@ from the @safe@ package, or use @foldr@ with an initial value:
--
-- @
-- import Safe (foldr1May)
--
-- foldr1May (+) []       -- Nothing
-- foldr1May (+) [1,2,3]  -- Just 6
--
-- -- Alternative: use foldr with explicit base case
-- foldr (+) 0 [1,2,3]  -- 6
-- @
avoidFoldr1 :: Rule
avoidFoldr1 = rule "safety/avoid-foldr1" $
  match ("foldr1 $F $XS" ==> "foldr1May $F $XS"
         `fromModule` "Prelude"
         `addImport` ("Safe", ["foldr1May"]))
  & severity Warning
  & message "Avoid partial function 'foldr1' - throws exception on empty list"
  & note "Use @foldr1May@ from Safe, or @foldr@ with an explicit initial value"
  & category Safety
  & safetyLevel MostlySafe

-- | Detect usage of 'foldl1' on lists.
--
-- == Problem
--
-- @foldl1@ throws an exception on empty lists.
--
-- == Solution
--
-- Use @foldl1May@ from the @safe@ package, or use @foldl'@ with an initial value.
avoidFoldl1 :: Rule
avoidFoldl1 = rule "safety/avoid-foldl1" $
  match ("foldl1 $F $XS" ==> "foldl1May' $F $XS"
         `fromModule` "Prelude"
         `addImport` ("Safe", ["foldl1May'"]))
  & severity Warning
  & message "Avoid partial function 'foldl1' - throws exception on empty list"
  & note "Use @foldl1May'@ from Safe (strict version), or @foldl'@ with initial value"
  & category Safety
  & safetyLevel MostlySafe

--------------------------------------------------------------------------------
-- Partial Maybe Functions
--------------------------------------------------------------------------------

-- | Detect usage of @fromJust@.
--
-- == Problem
--
-- @fromJust@ throws an exception on @Nothing@:
--
-- @
-- fromJust Nothing  -- *** Exception: Maybe.fromJust: Nothing
-- @
--
-- == Solution
--
-- Use @fromMaybe@ with a default value, or pattern matching:
--
-- @
-- import Data.Maybe (fromMaybe)
--
-- -- With default value
-- fromMaybe defaultValue maybeValue
--
-- -- Pattern matching
-- case maybeValue of
--   Just x  -> x
--   Nothing -> defaultValue
--
-- -- With maybe function
-- maybe defaultValue id maybeValue
-- @
--
-- == Note
--
-- If you are absolutely certain the value is @Just@, consider adding
-- a comment explaining why, and using @fromMaybe (error "reason")@
-- to provide a better error message.
avoidFromJust :: Rule
avoidFromJust = rule "safety/avoid-fromJust" $
  match (pat "fromJust"
         `fromModule` "Data.Maybe")
  & severity Warning
  & message "Avoid partial function 'fromJust' - throws exception on Nothing"
  & note "Use @fromMaybe@ with a default, pattern matching, or @maybe@"
  & category Safety
  & safetyLevel Unsafe

--------------------------------------------------------------------------------
-- Partial Read Functions
--------------------------------------------------------------------------------

-- | Detect usage of 'read' without error handling.
--
-- == Problem
--
-- @read@ throws an exception on parse failure:
--
-- @
-- read "not a number" :: Int  -- *** Exception: Prelude.read: no parse
-- @
--
-- == Solution
--
-- Use @readMay@ from the @safe@ package or @readMaybe@ from @Text.Read@:
--
-- @
-- import Text.Read (readMaybe)
--
-- readMaybe "123" :: Maybe Int  -- Just 123
-- readMaybe "abc" :: Maybe Int  -- Nothing
-- @
avoidRead :: Rule
avoidRead = rule "safety/avoid-read" $
  match ("read $S" ==> "readMay $S"
         `fromModule` "Prelude"
         `addImport` ("Safe", ["readMay"]))
  & severity Warning
  & message "Avoid partial function 'read' - throws exception on parse failure"
  & note "Use @readMay@ from Safe, @readMaybe@ from Text.Read, or a proper parser"
  & category Safety
  & safetyLevel MostlySafe

--------------------------------------------------------------------------------
-- Unsafe Operations
--------------------------------------------------------------------------------

-- | Detect usage of 'error'.
--
-- == Problem
--
-- @error@ terminates the program with an exception. While sometimes necessary,
-- it should be used sparingly and never in library code that might be called
-- by others.
--
-- == Solution
--
-- Return an appropriate error type:
--
-- @
-- -- Use Either for recoverable errors
-- parseValue :: Text -> Either ParseError Value
--
-- -- Use Maybe for optional values
-- findUser :: UserId -> IO (Maybe User)
--
-- -- Use ExceptT for monadic error handling
-- processRequest :: Request -> ExceptT AppError IO Response
-- @
--
-- == When 'error' is acceptable
--
-- * In test code
-- * As a placeholder during development (TODO)
-- * For genuinely impossible cases (consider using @impossible@ pattern)
avoidError :: Rule
avoidError = rule "safety/avoid-error" $
  match (pat "error")
  & severity Error
  & message "Avoid 'error' - terminates program with exception"
  & note "Return an error type (Either, Maybe, ExceptT) or use typed errors"
  & category Safety
  & safetyLevel ManualReview
  & except ["*Test*", "*Spec*", "*Tests*"]

-- | Detect usage of 'undefined'.
--
-- == Problem
--
-- @undefined@ throws an exception when evaluated:
--
-- @
-- undefined  -- *** Exception: Prelude.undefined
-- @
--
-- == Solution
--
-- * Use typed holes during development: @_todo@
-- * Provide proper implementations
-- * Use 'error' with a descriptive message if truly needed
--
-- == Note
--
-- GHC's @-Werror=incomplete-patterns@ can help catch places where
-- @undefined@ might be used as a placeholder for unhandled cases.
avoidUndefined :: Rule
avoidUndefined = rule "safety/avoid-undefined" $
  match (pat "undefined")
  & severity Error
  & message "Avoid 'undefined' - throws exception when evaluated"
  & note "Use typed holes (_todo) during development, or provide implementation"
  & category Safety
  & safetyLevel ManualReview
  & except ["*Test*", "*Spec*", "*Tests*"]

-- | Detect usage of @throw@ in pure code.
--
-- == Problem
--
-- Using @throw@ in pure code is problematic because:
--
-- * Exceptions can escape from anywhere
-- * They bypass the type system
-- * They're hard to reason about
--
-- == Solution
--
-- Use @Either@ or @ExceptT@ for error handling in pure code.
-- Reserve @throwIO@ for IO code where exceptions are expected.
avoidThrow :: Rule
avoidThrow = rule "safety/avoid-throw" $
  match (pat "throw"
         `fromModule` "Control.Exception")
  & severity Warning
  & message "Avoid @throw@ in pure code - use Either or ExceptT for errors"
  & note "Use 'throwIO' in IO, or typed errors (Either, ExceptT) in pure code"
  & category Safety
  & safetyLevel ManualReview

-- | Detect usage of @assert@ in production code.
--
-- == Problem
--
-- @assert@ can be silently disabled with optimization flags (@-O@), making
-- your safety checks disappear in production:
--
-- @
-- assert (x > 0) (doSomething x)  -- Becomes just (doSomething x) with -O
-- @
--
-- == Solution
--
-- Use explicit error handling that isn't affected by compiler flags:
--
-- @
-- -- Use guards with error messages
-- doSomething x
--   | x <= 0    = error "x must be positive"
--   | otherwise = actualImplementation x
--
-- -- Or use Maybe/Either for recoverable validation
-- doSomethingSafe :: Int -> Either String Result
-- doSomethingSafe x
--   | x <= 0    = Left "x must be positive"
--   | otherwise = Right (actualImplementation x)
-- @
avoidAssert :: Rule
avoidAssert = rule "safety/avoid-assert" $
  match (pat "assert"
         `fromModule` "Control.Exception")
  & severity Warning
  & message "Avoid @assert@ - disabled by optimization flags (-O)"
  & note "Use explicit guards, Either, or runtime validation that isn't optimized away"
  & category Safety
  & safetyLevel ManualReview
  & except ["*Test*", "*Spec*", "*Tests*"]

--------------------------------------------------------------------------------
-- Partial Numeric Functions
--------------------------------------------------------------------------------

-- | Detect usage of 'succ' on bounded types.
--
-- == Problem
--
-- @succ@ throws an exception when called on @maxBound@:
--
-- @
-- succ (maxBound :: Int8)  -- *** Exception: Enum.succ: bad argument
-- succ (maxBound :: Word)  -- *** Exception: Enum.succ: bad argument
-- @
--
-- == Solution
--
-- Use @succMay@ from the @safe@ package or explicit bounds checking:
--
-- @
-- import Safe (succMay)
--
-- succMay (maxBound :: Int8)  -- Nothing
-- succMay (127 :: Int8)       -- Nothing
-- succMay (126 :: Int8)       -- Just 127
--
-- -- Or manual bounds checking
-- safeSucc :: (Eq a, Bounded a, Enum a) => a -> Maybe a
-- safeSucc x
--   | x == maxBound = Nothing
--   | otherwise     = Just (succ x)
-- @
avoidSucc :: Rule
avoidSucc = rule "safety/avoid-succ" $
  match ("succ $X" ==> "succMay $X"
         `fromModule` "Prelude"
         `addImport` ("Safe", ["succMay"]))
  & severity Suggestion
  & message "Consider using @succMay@ - succ throws on maxBound"
  & note "Use @succMay@ from Safe, or check bounds manually"
  & category Safety
  & safetyLevel MostlySafe

-- | Detect usage of 'pred' on bounded types.
--
-- == Problem
--
-- @pred@ throws an exception when called on @minBound@:
--
-- @
-- pred (minBound :: Int8)  -- *** Exception: Enum.pred: bad argument
-- pred (0 :: Word)         -- *** Exception: Enum.pred: bad argument
-- @
--
-- == Solution
--
-- Use @predMay@ from the @safe@ package or explicit bounds checking:
--
-- @
-- import Safe (predMay)
--
-- predMay (minBound :: Int8)  -- Nothing
-- predMay (0 :: Word)         -- Nothing
-- predMay (1 :: Word)         -- Just 0
-- @
avoidPred :: Rule
avoidPred = rule "safety/avoid-pred" $
  match ("pred $X" ==> "predMay $X"
         `fromModule` "Prelude"
         `addImport` ("Safe", ["predMay"]))
  & severity Suggestion
  & message "Consider using @predMay@ - pred throws on minBound"
  & note "Use @predMay@ from Safe, or check bounds manually"
  & category Safety
  & safetyLevel MostlySafe

-- | Detect usage of 'toEnum' without bounds checking.
--
-- == Problem
--
-- @toEnum@ throws an exception for out-of-range values:
--
-- @
-- toEnum 256 :: Word8   -- *** Exception: Enum.toEnum: tag too large
-- toEnum (-1) :: Word   -- *** Exception: Enum.toEnum: negative
-- toEnum 100 :: Bool    -- *** Exception: Enum.toEnum: bad argument
-- @
--
-- == Solution
--
-- Use @toEnumMay@ from the @safe@ package:
--
-- @
-- import Safe (toEnumMay)
--
-- toEnumMay 256 :: Maybe Word8  -- Nothing
-- toEnumMay 255 :: Maybe Word8  -- Just 255
-- toEnumMay 0   :: Maybe Bool   -- Just False
-- toEnumMay 2   :: Maybe Bool   -- Nothing
-- @
avoidToEnum :: Rule
avoidToEnum = rule "safety/avoid-toEnum" $
  match ("toEnum $N" ==> "toEnumMay $N"
         `fromModule` "Prelude"
         `addImport` ("Safe", ["toEnumMay"]))
  & severity Suggestion
  & message "Consider using @toEnumMay@ - toEnum throws on invalid values"
  & note "Use @toEnumMay@ from Safe to handle out-of-range values safely"
  & category Safety
  & safetyLevel MostlySafe

-- | Detect usage of @genericIndex@ for list indexing.
--
-- == Problem
--
-- @genericIndex@ is a partial function that throws on invalid indices,
-- just like @!!@ but accepting any Integral index type:
--
-- @
-- genericIndex [1,2,3] 5    -- *** Exception: index too large
-- genericIndex [1,2,3] (-1) -- *** Exception: negative index
-- @
--
-- == Solution
--
-- Use a bounds-checked alternative or check the index manually:
--
-- @
-- import Safe (atMay)
--
-- -- Convert to Int and use atMay
-- safeGenericIndex :: Integral i => [a] -> i -> Maybe a
-- safeGenericIndex xs i = atMay xs (fromIntegral i)
-- @
avoidGenericIndex :: Rule
avoidGenericIndex = rule "safety/avoid-genericIndex" $
  match (pat "genericIndex"
         `fromModule` "Data.List")
  & severity Warning
  & message "Avoid @genericIndex@ - throws on invalid index"
  & note "Use @atMay@ from Safe with @fromIntegral@, or check bounds manually"
  & category Safety
  & safetyLevel MostlySafe

--------------------------------------------------------------------------------
-- Partial List Generators
--------------------------------------------------------------------------------

-- | Detect usage of 'cycle' which can cause infinite loops.
--
-- == Problem
--
-- @cycle@ on an empty list creates an infinite loop that hangs the program:
--
-- @
-- cycle []  -- Hangs forever, no exception thrown!
-- take 5 (cycle [])  -- Also hangs forever
-- @
--
-- Unlike other partial functions that throw exceptions, @cycle []@ will
-- silently consume all CPU resources.
--
-- == Solution
--
-- Check for non-empty input before calling @cycle@:
--
-- @
-- import Data.List.NonEmpty (NonEmpty, cycle1)
-- import qualified Data.List.NonEmpty as NE
--
-- -- Use NonEmpty to guarantee non-empty input
-- safeCycle :: NonEmpty a -> [a]
-- safeCycle = NE.toList . NE.cycle1
--
-- -- Or check explicitly
-- maybeCycle :: [a] -> Maybe [a]
-- maybeCycle [] = Nothing
-- maybeCycle xs = Just (cycle xs)
-- @
avoidCycle :: Rule
avoidCycle = rule "safety/avoid-cycle" $
  match (pat "cycle"
         `fromModule` "Prelude")
  & severity Warning
  & message "Beware of 'cycle' - hangs forever on empty list with no exception"
  & note "Use 'cycle1' from Data.List.NonEmpty, or validate input is non-empty"
  & category Safety
  & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Metadata
--------------------------------------------------------------------------------

-- | Total number of safety rules.
safetyRuleCount :: Int
safetyRuleCount = length safetyRules
