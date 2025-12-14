{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

module PatternExamples where

import Data.List (null)

--------------------------------------------------------------------------------
-- Case Expression Simplifications
--------------------------------------------------------------------------------

-- Should simplify: case x of { _ -> y } can be simplified
caseWildcardSimple :: Int -> String
caseWildcardSimple x = case x of
  _ -> "always this"

-- Should simplify: case matching same variable is redundant
caseIdentityMatch :: Int -> Int
caseIdentityMatch x = case x of
  x -> x + 1

-- Should simplify: case True -> True; False -> False is identity
caseBoolIdentity :: Bool -> Bool
caseBoolIdentity x = case x of
  True -> True
  False -> False

-- Should simplify: case Left l -> Left l; Right r -> Right r is identity
caseEitherIdentity :: Either String Int -> Either String Int
caseEitherIdentity x = case x of
  Left l -> Left l
  Right r -> Right r

-- Should simplify: case (a, b) -> (a, b) is identity
casePairIdentity :: (Int, String) -> (Int, String)
casePairIdentity x = case x of
  (a, b) -> (a, b)

-- Should simplify: case [] -> True; _ -> False is null x
caseEmptyListCheck :: [a] -> Bool
caseEmptyListCheck x = case x of
  [] -> True
  _ -> False

-- Should simplify: case [] -> False; _ -> True is not (null x)
caseNonEmptyListCheck :: [a] -> Bool
caseNonEmptyListCheck x = case x of
  [] -> False
  _ -> True

-- Should simplify: case Nothing -> True; Just _ -> False is isNothing x
caseNothingCheck :: Maybe a -> Bool
caseNothingCheck x = case x of
  Nothing -> True
  Just _ -> False

-- Should simplify: case Nothing -> False; Just _ -> True is isJust x
caseJustCheck :: Maybe a -> Bool
caseJustCheck x = case x of
  Nothing -> False
  Just _ -> True

--------------------------------------------------------------------------------
-- Pattern Guard Simplifications
--------------------------------------------------------------------------------

-- Should use if-then-else: case with guard on wildcard could be if-then-else
patternGuardOnWildcard :: Int -> String
patternGuardOnWildcard x = case x of
  _ | x > 10 -> "large"
  _ -> "small"

-- Another guard example
guardedWildcard :: Maybe Int -> Int
guardedWildcard m = case m of
  _ | Just n <- m, n > 0 -> n
  _ -> 0

--------------------------------------------------------------------------------
-- Redundant and As-Patterns
--------------------------------------------------------------------------------

-- Should warn: As-pattern where the bound name is not used
asPatternUnused :: [Int] -> Int
asPatternUnused xs@(x:_) = x + 1

-- As-pattern that is actually used (good)
asPatternUsed :: [Int] -> ([Int], Int)
asPatternUsed xs@(x:_) = (xs, x)

-- Redundant as-pattern in nested structure
nestedAsUnused :: Maybe (Int, String) -> Maybe Int
nestedAsUnused mp@(Just (n, _)) = Just n
nestedAsUnused Nothing = Nothing

--------------------------------------------------------------------------------
-- Lazy Pattern Usage
--------------------------------------------------------------------------------

-- Should warn: Lazy pattern where laziness may not be needed
lazyPatternSimple :: (Int, String) -> Int
lazyPatternSimple ~(x, _) = x

-- Lazy pattern in case (potentially unnecessary)
lazyPatternCase :: Maybe (Int, Int) -> Int
lazyPatternCase m = case m of
  Just ~(a, b) -> a + b
  Nothing -> 0

-- Lazy pattern in function argument
lazyFunctionArg :: (Int, Int) -> Int
lazyFunctionArg ~(x, y) = if x > 0 then x else y

--------------------------------------------------------------------------------
-- Nested Case Expressions
--------------------------------------------------------------------------------

-- Should refactor: Nested case expressions could use pattern guards
nestedCaseSimple :: Maybe Int -> Maybe String -> String
nestedCaseSimple mx ms = case mx of
  Just x -> case ms of
    Just s -> s ++ show x
    Nothing -> show x
  Nothing -> "no value"

-- Deeply nested cases
deeplyNestedCase :: Either String Int -> Maybe Bool -> String
deeplyNestedCase e m = case e of
  Left err -> case m of
    Just True -> "error but confirmed: " ++ err
    _ -> "error: " ++ err
  Right n -> case m of
    Just True -> "success: " ++ show n
    _ -> show n

-- Triple nesting
tripleNested :: Maybe Int -> Maybe String -> Maybe Bool -> String
tripleNested mx ms mb = case mx of
  Just x -> case ms of
    Just s -> case mb of
      Just True -> s ++ show x ++ "!"
      _ -> s ++ show x
    Nothing -> show x
  Nothing -> "empty"

--------------------------------------------------------------------------------
-- Pattern Overlapping and Redundancy
--------------------------------------------------------------------------------

-- Overlapping patterns - wildcard before specific constructors
overlappingPattern :: Maybe Int -> String
overlappingPattern x = case x of
  _ -> "default"
  Just n -> "number: " ++ show n
  Nothing -> "nothing"

-- Redundant wildcard after wildcard
redundantWildcard :: Either String Int -> String
redundantWildcard e = case e of
  Left s -> s
  _ -> "right"
  _ -> "another wildcard"

--------------------------------------------------------------------------------
-- Complex Pattern Matching Examples
--------------------------------------------------------------------------------

-- Multiple evaluation in tuple pattern
multiEvalTuple :: [Int] -> [String] -> (Int, Int)
multiEvalTuple xs ys = case (length xs, length ys) of
  (0, _) -> (0, 0)
  (_, 0) -> (0, 0)
  (n, m) -> (n, m)

-- Tuple in case - performance consideration
tupleCasePattern :: [a] -> [b] -> Bool
tupleCasePattern xs ys = case (null xs, null ys) of
  (True, True) -> True
  _ -> False

--------------------------------------------------------------------------------
-- Pattern Matching with GADTs and View Patterns
--------------------------------------------------------------------------------

-- View pattern example (info only)
data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving stock (Eq, Show)

depth :: Tree a -> Int
depth Empty = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

-- View pattern usage
deepTree :: Tree a -> Bool
deepTree (depth -> d) = d > 10

-- Pattern synonym example
pattern Zero :: Int
pattern Zero = 0

pattern One :: Int
pattern One = 1

usePatternSynonym :: Int -> String
usePatternSynonym Zero = "zero"
usePatternSynonym One = "one"
usePatternSynonym n = show n

--------------------------------------------------------------------------------
-- Wildcards in Patterns
--------------------------------------------------------------------------------

-- Unused wildcard in complex pattern
wildcardInPattern :: Maybe (Either String Int) -> Maybe Int
wildcardInPattern (Just (Right n)) = Just n
wildcardInPattern _ = Nothing

-- Multiple wildcards
multipleWildcards :: (Int, String, Bool, Char) -> Int
multipleWildcards (n, _, _, _) = n

--------------------------------------------------------------------------------
-- Realistic Complex Examples
--------------------------------------------------------------------------------

-- Configuration extraction with nested case
extractConfig :: Maybe (Either String (Int, String, Bool)) -> String
extractConfig mc = case mc of
  Nothing -> "no config"
  Just ec -> case ec of
    Left err -> "config error: " ++ err
    Right cfg -> case cfg of
      (port, host, secure) ->
        if secure
        then "https://" ++ host ++ ":" ++ show port
        else "http://" ++ host ++ ":" ++ show port

-- Validation with redundant patterns
validateInput :: Maybe String -> Either String String
validateInput ms = case ms of
  Nothing -> Left "no input"
  Just "" -> Left "empty input"
  Just s -> Right s
  _ -> Left "unexpected"

-- Pattern with as-binding and lazy pattern
processData :: [(Int, String)] -> Maybe ([(Int, String)], Int)
processData pairs@(~(n, _):_) = Just (pairs, n)
processData [] = Nothing
