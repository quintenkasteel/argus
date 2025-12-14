module BooleanSimplify where

-- Should simplify: if b then True else False -> b
example1 :: Bool -> Bool
example1 x = x

-- Should simplify: not (not x) -> x
example2 :: Bool -> Bool
example2 x = x

-- Should simplify: x == True -> x
example3 :: Bool -> Bool
example3 x = x