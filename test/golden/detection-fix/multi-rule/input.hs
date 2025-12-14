module MultiRule where

import Data.List (sort)

-- Multiple issues on same expression: partial + performance
-- head (sort xs) should be minimum xs
unsafeMinimum :: Ord a => [a] -> a
unsafeMinimum xs = head (sort xs)

-- Boolean simplification chained with redundant if
-- if (not (not x)) then True else False -> x
chainedBoolean :: Bool -> Bool
chainedBoolean x = if not (not x) then True else False

-- Both space leak and modernization
-- foldl + return -> foldl' + pure
mixedIssues :: [Int] -> IO Int
mixedIssues xs = return (foldl (+) 0 xs)
