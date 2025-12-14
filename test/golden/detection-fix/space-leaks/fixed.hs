module SpaceLeaks where

import Data.List (foldl')
-- Should use foldl' instead of foldl
sumList :: [Int] -> Int
sumList xs = foldl' (+) 0 xs
