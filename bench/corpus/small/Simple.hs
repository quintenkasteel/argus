-- | Simple module for benchmark corpus (small)
module Simple where

import Data.List (sort)
import Data.Maybe (fromMaybe)

-- | Get first element or default
getFirstOrDefault :: a -> [a] -> a
getFirstOrDefault def xs = case xs of
  [] -> def
  (x:_) -> x

-- | Safe tail operation
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

-- | Calculate sum of list
calculateSum :: Num a => [a] -> a
calculateSum = foldr (+) 0

-- | Filter positive numbers
positives :: (Num a, Ord a) => [a] -> [a]
positives = filter (> 0)

-- | Double all elements
doubleAll :: Num a => [a] -> [a]
doubleAll = map (* 2)

-- | Find maximum or use default
maxOrDefault :: Ord a => a -> [a] -> a
maxOrDefault def [] = def
maxOrDefault _ xs = maximum xs

-- | Concatenate with separator
joinWith :: [a] -> [[a]] -> [a]
joinWith sep = foldr1 (\a b -> a ++ sep ++ b)
