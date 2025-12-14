{-# LANGUAGE OverloadedStrings #-}

module BooleanTests where

-- Basic Boolean simplifications

-- if x then True else False -> x
boolCheck1 :: Bool -> Bool
boolCheck1 x = x

-- if x then False else True -> not x
boolCheck2 :: Bool -> Bool
boolCheck2 x = not x

-- not (not x) -> x
doubleNot :: Bool -> Bool
doubleNot x = x

-- x == True -> x
eqTrueCheck :: Bool -> Bool
eqTrueCheck x = x

-- x == False -> not x
eqFalseCheck :: Bool -> Bool
eqFalseCheck x = not x

-- True == x -> x
trueEqCheck :: Bool -> Bool
trueEqCheck x = True == x

-- False == x -> not x
falseEqCheck :: Bool -> Bool
falseEqCheck x = False == x

-- x /= True -> not x
neTrueCheck :: Bool -> Bool
neTrueCheck x = x /= True

-- x /= False -> x
neFalseCheck :: Bool -> Bool
neFalseCheck x = x /= False

-- if x then y else y -> y (redundant conditional)
redundantCond :: Bool -> Int -> Int
redundantCond x y = if x then y else y

-- Basic Boolean operations

-- not True -> False
notTrueExpr :: Bool
notTrueExpr = not True

-- not False -> True
notFalseExpr :: Bool
notFalseExpr = not False

-- True && x -> x
trueAndExpr :: Bool -> Bool
trueAndExpr x = True && x

-- False && x -> False
falseAndExpr :: Bool -> Bool
falseAndExpr x = False && x

-- x && False -> False
andFalseExpr :: Bool -> Bool
andFalseExpr x = x && False

-- True || x -> True
trueOrExpr :: Bool -> Bool
trueOrExpr x = True || x

-- False || x -> x
falseOrExpr :: Bool -> Bool
falseOrExpr x = False || x

-- x || True -> True
orTrueExpr :: Bool -> Bool
orTrueExpr x = x || True

-- x && x -> x
andSameExpr :: Bool -> Bool
andSameExpr flag = flag && flag

-- x || x -> x
orSameExpr :: Bool -> Bool
orSameExpr flag = flag || flag

-- De Morgan's laws

-- not x && not y -> not (x || y)
deMorgan1 :: Bool -> Bool -> Bool
deMorgan1 a b = not a && not b

-- not x || not y -> not (x && y)
deMorgan2 :: Bool -> Bool -> Bool
deMorgan2 a b = not a || not b

-- Contradictions and tautologies

-- x && not x -> False
contradiction1 :: Bool -> Bool
contradiction1 flag = flag && not flag

-- not x && x -> False
contradiction2 :: Bool -> Bool
contradiction2 flag = not flag && flag

-- x || not x -> True
tautology1 :: Bool -> Bool
tautology1 flag = flag || not flag

-- not x || x -> True
tautology2 :: Bool -> Bool
tautology2 flag = not flag || flag

-- If-then-else patterns

-- if True then x else y -> x
ifTrueExpr :: Int -> Int -> Int
ifTrueExpr x y = if True then x else y

-- if False then x else y -> y
ifFalseExpr :: Int -> Int -> Int
ifFalseExpr x y = if False then x else y

-- if c then True else x -> c || x
ifThenTrueExpr :: Bool -> Bool -> Bool
ifThenTrueExpr c x = if c then True else x

-- if c then False else x -> not c && x
ifThenFalseExpr :: Bool -> Bool -> Bool
ifThenFalseExpr c x = if c then False else x

-- if c then x else True -> not c || x
ifElseTrueExpr :: Bool -> Bool -> Bool
ifElseTrueExpr c x = if c then x else True

-- if c then x else False -> c && x
ifElseFalseExpr :: Bool -> Bool -> Bool
ifElseFalseExpr c x = if c then x else False

-- if not c then x else y -> if c then y else x
ifNotExpr :: Bool -> Int -> Int -> Int
ifNotExpr c x y = if not c then x else y

-- Complex boolean expressions in real code

-- Checking multiple conditions with redundancy
checkPermissions :: Bool -> Bool -> Bool -> Bool
checkPermissions isAdmin isOwner isGuest =
  if isAdmin then True else isOwner

-- Redundant boolean comparison in guard-like pattern
validateInput :: String -> Bool -> Bool
validateInput input valid =
  valid

-- Double negation in conditional
confusingLogic :: Bool -> Bool -> Bool
confusingLogic a b = a && b

-- Redundant if with same branches in both
redundantBranches :: Bool -> String -> String
redundantBranches cond msg = if cond then msg else msg

-- Boolean algebra simplification opportunity
complexBoolean :: Bool -> Bool -> Bool -> Bool
complexBoolean a b c = a && b || not c