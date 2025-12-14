-- | Module with partial function usage for testing detection
module PartialFunctions where

import Data.List (head, tail, init, last)
import Data.Maybe (fromJust)

-- | Uses head (partial)
getFirst :: [a] -> a
getFirst xs = head xs

-- | Uses tail (partial)
getRest :: [a] -> [a]
getRest xs = tail xs

-- | Uses last (partial)
getLast :: [a] -> a
getLast xs = last xs

-- | Uses init (partial)
getInit :: [a] -> [a]
getInit xs = init xs

-- | Uses fromJust (partial)
extractJust :: Maybe a -> a
extractJust mx = fromJust mx

-- | Uses !! (partial)
getAt :: [a] -> Int -> a
getAt xs i = xs !! i

-- | Multiple partials in one function
process :: [Int] -> Int
process xs = head xs + last xs + head (tail xs)

-- | Nested partial usage
nested :: [[Int]] -> Int
nested xss = head (head xss)

-- | Partial with guard
withGuard :: [a] -> a
withGuard xs
  | null xs = error "empty list"
  | otherwise = head xs
