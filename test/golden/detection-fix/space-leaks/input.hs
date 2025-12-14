module SpaceLeaks where

-- Should use foldl' instead of foldl
sumList :: [Int] -> Int
sumList xs = foldl (+) 0 xs
