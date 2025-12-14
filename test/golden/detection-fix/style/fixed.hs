{-# LANGUAGE OverloadedStrings #-}

module StyleIssues where

import Control.Monad (when)
import Data.Maybe (fromMaybe)

-- Unnecessary parentheses around simple expressions
calculate1 :: Int -> Int -> Int
calculate1 x y = (x) + (y)

-- Unnecessary parentheses around variables
getValue :: Maybe Int -> Int
getValue m = fromMaybe (0) m

-- Unnecessary parentheses around literals
isPositive :: Int -> Bool
isPositive n = n > (0)

-- Nested unnecessary parentheses
nested :: Int -> Int
nested x = ((x + 1))

-- Redundant parentheses in function application
apply :: (Int -> Int) -> Int -> Int
apply (f) x = (f) (x)

-- Redundant parentheses in list
numbers :: [Int]
numbers = [(1), (2), (3)]

-- Redundant parentheses in tuple
pair :: (Int, Bool)
pair = ((42), (True))

-- Using $ where composition would be cleaner
transform :: String -> String
transform s = reverse $ filter (/= ' ') $ map toUpper s
  where
    toUpper c = c

-- Redundant $ with simple argument
printValue :: Int -> IO ()
printValue x = print $ x

-- Redundant nested dollar (syntax error - removed as invalid)
-- This pattern doesn't compile in Haskell

-- Redundant let/in for single binding
addOne :: Int -> Int
addOne x = let result = x + 1 in result

-- Redundant let/in that just returns the binding
identity :: Int -> Int
identity x = let y = x in y

-- Verbose lambda that could be point-free
mapInc :: [Int] -> [Int]
mapInc xs = map (\x -> x + 1) xs

-- Lambda that's just function application
applyFunc :: (a -> b) -> a -> b
applyFunc f = \x -> f x

-- Redundant do block for single action
singleAction :: IO ()
singleAction = do
  putStrLn "Hello"

-- Redundant do with single return
returnOnly :: IO Int
returnOnly = do
return 42

-- Verbose if-then-else that could use when
conditionalAction :: Bool -> IO ()
conditionalAction flag = if flag then putStrLn "Yes" else pure ()

-- Redundant id composition
composeId :: (a -> b) -> a -> b
composeId f = f

-- Another id composition
idCompose :: (a -> b) -> a -> b
idCompose f = f

-- Redundant parentheses in operators
addMul :: Int -> Int -> Int -> Int
addMul x y z = (x * y) + z

-- Unnecessary parentheses around infix operator
infixExample :: Bool -> Bool -> Bool
infixExample a b = (a && b) || False

-- Redundant not (not x)
doubleNegation :: Bool -> Bool
doubleNegation x = x

-- Comparison with True
compareTrue :: Bool -> Bool
compareTrue x = x

-- Comparison with False
compareFalse :: Bool -> Bool
compareFalse x = not x

-- Redundant if True/False
redundantIf1 :: Bool -> Bool
redundantIf1 x = x

-- Another redundant if pattern
redundantIf2 :: Bool -> Bool
redundantIf2 x = not x

-- Redundant map id
mapIdentity :: [a] -> [a]
mapIdentity xs = xs

-- Redundant fmap id
fmapIdentity :: Functor f => f a -> f a
fmapIdentity x = fmap x

-- Redundant reverse . reverse
doubleReverse :: [a] -> [a]
doubleReverse xs = xs

-- Point-free opportunity with composition
pointFreeExample :: Int -> Int
pointFreeExample x = (+1) ((*2) x)

-- Verbose lambda in higher-order function
filterExample :: [Int] -> [Int]
filterExample xs = filter (\x -> even x) xs

-- Redundant lambda wrapper
wrapperLambda :: (a -> b) -> a -> b
wrapperLambda f = \a -> f a

-- Unnecessary parentheses in case expression
caseParens :: Maybe Int -> Int
caseParens m = case m of
  Just (x) -> (x)
  Nothing -> (0)

-- Redundant pure () in if
pureUnit :: Bool -> IO ()
pureUnit c = if c then pure () else putStrLn "No"

-- Redundant return () in if
returnUnit :: Bool -> IO ()
returnUnit c = if c then pure () else putStrLn "No"