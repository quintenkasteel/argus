{-# LANGUAGE ImportQualifiedPost #-}

module EitherPatterns where

import Data.Either (Either(..), either, isLeft, isRight, fromLeft, fromRight, lefts, rights, partitionEithers)
import Data.Bifunctor (bimap, first, second)
import Data.Maybe (Maybe (..))
import Data.Functor (fmap)

--------------------------------------------------------------------------------
-- Either Identity Patterns
--------------------------------------------------------------------------------

-- Should simplify: either Left Right e ==> e
eitherIdentity :: Either a b -> Either a b
eitherIdentity e = e

-- Should simplify: either (const False) (const True) e ==> isRight e
checkIsRight :: Either a b -> Bool
checkIsRight e = either (const False) (const True) e

-- Should simplify: either (const True) (const False) e ==> isLeft e
checkIsLeft :: Either a b -> Bool
checkIsLeft e = either (const True) (const False) e

--------------------------------------------------------------------------------
-- Either Functor Patterns
--------------------------------------------------------------------------------

-- Should simplify: either Left (Right . f) e ==> fmap f e
mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f e = either Left (Right . f) e

-- Should simplify: either (Left . f) Right e ==> first f e
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f e = either (Left . f) Right e

-- Case expression that could use either combinator
-- Should suggest: case e of Left x -> f x; Right y -> g y ==> either f g e
caseToEither :: (a -> c) -> (b -> c) -> Either a b -> c
caseToEither f g e = case e of
  Left x -> f x
  Right y -> g y

--------------------------------------------------------------------------------
-- isLeft/isRight Constant Folding
--------------------------------------------------------------------------------

-- Should simplify: isLeft (Left x) ==> True
leftIsLeft :: a -> Bool
leftIsLeft x = isLeft (Left x)

-- Should simplify: isLeft (Right x) ==> False
rightIsLeft :: b -> Bool
rightIsLeft x = isLeft (Right x)

-- Should simplify: isRight (Left x) ==> False
leftIsRight :: a -> Bool
leftIsRight x = isRight (Left x)

-- Should simplify: isRight (Right x) ==> True
rightIsRight :: b -> Bool
rightIsRight x = isRight (Right x)

--------------------------------------------------------------------------------
-- fromLeft/fromRight Simplification
--------------------------------------------------------------------------------

-- Should simplify: fromLeft x (Left y) ==> y
fromLeftOnLeft :: a -> a -> a
fromLeftOnLeft def val = fromLeft def (Left val)

-- Should simplify: fromLeft x (Right y) ==> x
fromLeftOnRight :: a -> b -> a
fromLeftOnRight def val = fromLeft def (Right val)

-- Should simplify: fromRight x (Left y) ==> x
fromRightOnLeft :: b -> a -> b
fromRightOnLeft def val = fromRight def (Left val)

-- Should simplify: fromRight x (Right y) ==> y
fromRightOnRight :: b -> b -> b
fromRightOnRight def val = fromRight def (Right val)

--------------------------------------------------------------------------------
-- Lists of Either
--------------------------------------------------------------------------------

-- Should simplify: lefts [Left x] ==> [x]
leftsOnSingletonLeft :: a -> [a]
leftsOnSingletonLeft x = lefts [Left x]

-- Should simplify: lefts [Right x] ==> []
leftsOnSingletonRight :: b -> [a]
leftsOnSingletonRight x = lefts [Right x]

-- Should simplify: rights [Left x] ==> []
rightsOnSingletonLeft :: a -> [b]
rightsOnSingletonLeft x = rights [Left x]

-- Should simplify: rights [Right x] ==> [x]
rightsOnSingletonRight :: b -> [b]
rightsOnSingletonRight x = rights [Right x]

-- Should simplify: partitionEithers [Left x] ==> ([x], [])
partitionLeft :: a -> ([a], [b])
partitionLeft x = partitionEithers [Left x]

-- Should simplify: partitionEithers [Right x] ==> ([], [x])
partitionRight :: b -> ([a], [b])
partitionRight x = partitionEithers [Right x]

--------------------------------------------------------------------------------
-- Bifunctor Identity Patterns
--------------------------------------------------------------------------------

-- Should simplify: bimap id id e ==> e
bimapIdentity :: Either a b -> Either a b
bimapIdentity e = bimap id id e

-- Should simplify: first id e ==> e
firstIdentity :: Either a b -> Either a b
firstIdentity e = first id e

-- Should simplify: second id e ==> e
secondIdentity :: Either a b -> Either a b
secondIdentity e = second id e

-- Should simplify: fmap id e ==> e
fmapIdentity :: Either a b -> Either a b
fmapIdentity e = e

--------------------------------------------------------------------------------
-- Complex Realistic Examples
--------------------------------------------------------------------------------

-- Parse and validate user input
validateInput :: String -> Either String Int
validateInput input =
  case parseInput input of
    Left err -> Left err
    Right val -> Right (val * 2)
  where
    parseInput :: String -> Either String Int
    parseInput s = if null s then Left "empty" else Right (length s)

-- Redundant Left/Right wrapping in case
processResult :: Either String Int -> Either String String
processResult result = case result of
  Left err -> Left err
  Right value -> Right (show value)

-- Multiple either patterns
complexEither :: Either String Int -> Bool
complexEither e =
  if either (const True) (const False) e
    then either (const False) (const True) e
    else isRight e

-- Chained either operations
chainedEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
chainedEither f g e = either (Left . f) (Right . g) e