module ListOperations where

import Data.List (sort)

-- Should use 'null' instead of 'length == 0'
isEmpty :: [a] -> Bool
isEmpty xs = length xs == 0

-- Should use 'concatMap' instead of 'concat . map'
flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f xs = concat (map f xs)

-- Should use 'minimum' instead of 'head . sort'
smallest :: Ord a => [a] -> a
smallest = head . sort
