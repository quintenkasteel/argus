{-# LANGUAGE ImportQualifiedPost #-}

module MaybePatterns where

import Data.Maybe (fromMaybe, isJust, isNothing, fromJust, maybeToList, listToMaybe, catMaybes, mapMaybe)

--------------------------------------------------------------------------------
-- Maybe Simplification Patterns
--------------------------------------------------------------------------------

-- Should simplify: maybe Nothing Just m  ==>  m
identityMaybe :: Maybe a -> Maybe a
identityMaybe m = maybe Nothing Just m

-- Should simplify: maybe x id m  ==>  fromMaybe x m
maybeWithId :: a -> Maybe a -> a
maybeWithId x m = maybe x id m

-- Should simplify: maybe False (const True) m  ==>  isJust m
testIsJust :: Maybe a -> Bool
testIsJust m = maybe False (const True) m

-- Should simplify: maybe True (const False) m  ==>  isNothing m
testIsNothing :: Maybe a -> Bool
testIsNothing m = maybe True (const False) m

-- Should simplify: maybe [] pure m  ==>  maybeToList m
maybeToListPattern :: Maybe a -> [a]
maybeToListPattern m = maybe [] pure m

-- Should simplify: maybe mempty pure m  ==>  foldMap pure m
maybeMempty :: Maybe a -> [a]
maybeMempty m = maybe mempty pure m

-- Should simplify: fromMaybe x Nothing  ==>  x
fromMaybeOnNothing :: Int -> Int
fromMaybeOnNothing x = fromMaybe x Nothing

-- Should simplify: fromMaybe x (Just y)  ==>  y
fromMaybeOnJust :: Int -> Int -> Int
fromMaybeOnJust x y = fromMaybe x (Just y)

-- Should simplify: isJust (Just x)  ==>  True
isJustOnJust :: Int -> Bool
isJustOnJust x = isJust (Just x)

-- Should simplify: isJust Nothing  ==>  False
isJustOnNothing :: Bool
isJustOnNothing = isJust Nothing

-- Should simplify: isNothing (Just x)  ==>  False
isNothingOnJust :: Int -> Bool
isNothingOnJust x = isNothing (Just x)

-- Should simplify: isNothing Nothing  ==>  True
isNothingOnNothing :: Bool
isNothingOnNothing = isNothing Nothing

--------------------------------------------------------------------------------
-- Maybe/List Conversion Patterns
--------------------------------------------------------------------------------

-- Should simplify: listToMaybe []  ==>  Nothing
emptyListToMaybe :: Maybe a
emptyListToMaybe = listToMaybe []

-- Should simplify: listToMaybe [x]  ==>  Just x
singletonListToMaybe :: Int -> Maybe Int
singletonListToMaybe x = listToMaybe [x]

-- Should simplify: maybeToList Nothing  ==>  []
nothingToList :: [a]
nothingToList = maybeToList Nothing

-- Should simplify: maybeToList (Just x)  ==>  [x]
justToList :: Int -> [Int]
justToList x = maybeToList (Just x)

-- Should simplify: catMaybes (map Just xs)  ==>  xs
catMaybesMapJust :: [a] -> [a]
catMaybesMapJust xs = catMaybes (map Just xs)

-- Should simplify: catMaybes [Just x]  ==>  [x]
catMaybesSingletonJust :: Int -> [Int]
catMaybesSingletonJust x = catMaybes [Just x]

-- Should simplify: catMaybes [Nothing]  ==>  []
catMaybesSingletonNothing :: [a]
catMaybesSingletonNothing = catMaybes [Nothing]

-- Should simplify: mapMaybe Just xs  ==>  xs
mapMaybeWithJust :: [a] -> [a]
mapMaybeWithJust xs = mapMaybe Just xs

-- Should simplify: mapMaybe id xs  ==>  catMaybes xs
mapMaybeWithId :: [Maybe a] -> [a]
mapMaybeWithId xs = mapMaybe id xs

-- Should simplify: map fromJust (filter isJust xs)  ==>  catMaybes xs
mapFromJustFilterIsJust :: [Maybe a] -> [a]
mapFromJustFilterIsJust xs = map fromJust (filter isJust xs)

--------------------------------------------------------------------------------
-- Maybe Pattern Matching
--------------------------------------------------------------------------------

-- Should simplify: case m of Nothing -> Nothing; Just x -> Just (f x)  ==>  fmap f m
caseMaybeFmap :: (a -> b) -> Maybe a -> Maybe b
caseMaybeFmap f m = case m of
  Nothing -> Nothing
  Just x -> Just (f x)

-- Should simplify: case m of Nothing -> y; Just x -> x  ==>  fromMaybe y m
caseMaybeFromMaybe :: a -> Maybe a -> a
caseMaybeFromMaybe y m = case m of
  Nothing -> y
  Just x -> x

-- Should simplify: case m of Nothing -> Nothing; Just x -> g x  ==>  m >>= g
caseMaybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
caseMaybeBind m g = case m of
  Nothing -> Nothing
  Just x -> g x

-- Should simplify: if isJust m then fromJust m else x  ==>  fromMaybe x m
ifIsJustFromJust :: Maybe a -> a -> a
ifIsJustFromJust m x = if isJust m then fromJust m else x

-- Should simplify: if isNothing m then x else fromJust m  ==>  fromMaybe x m
ifIsNothingFromJust :: Maybe a -> a -> a
ifIsNothingFromJust m x = if isNothing m then x else fromJust m

--------------------------------------------------------------------------------
-- Functor/Monad Patterns for Maybe
--------------------------------------------------------------------------------

-- Should simplify: fmap f (Just x)  ==>  Just (f x)
fmapOnJust :: (a -> b) -> a -> Maybe b
fmapOnJust f x = fmap f (Just x)

-- Should simplify: fmap f Nothing  ==>  Nothing
fmapOnNothing :: (a -> b) -> Maybe b
fmapOnNothing f = fmap f Nothing

-- Should simplify: join (fmap f m)  ==>  m >>= f
joinFmap :: Maybe a -> (a -> Maybe b) -> Maybe b
joinFmap m f = join (fmap f m)
  where
    join :: Maybe (Maybe a) -> Maybe a
    join Nothing = Nothing
    join (Just x) = x

-- Should simplify: fromJust (Just x)  ==>  x
fromJustOnJust :: Int -> Int
fromJustOnJust x = fromJust (Just x)

--------------------------------------------------------------------------------
-- Complex realistic examples
--------------------------------------------------------------------------------

-- Extract value with default
getUserName :: Maybe String -> String
getUserName maybeName = fromMaybe "Anonymous" maybeName

-- Check if value exists
hasValue :: Maybe a -> Bool
hasValue m = maybe False (const True) m

-- Convert to singleton list or empty
toSingletonList :: Maybe a -> [a]
toSingletonList m = maybe [] pure m

-- Filter and extract values
extractValidValues :: [Maybe a] -> [a]
extractValidValues xs = map fromJust (filter isJust xs)

-- Pattern match with transformation
transformMaybe :: (Int -> Int) -> Maybe Int -> Maybe Int
transformMaybe f m = case m of
  Nothing -> Nothing
  Just x -> Just (f x)

-- Conditional extraction
conditionalExtract :: Maybe Int -> Int
conditionalExtract m = if isJust m then fromJust m else 0
