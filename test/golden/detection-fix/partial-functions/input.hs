module PartialFunctions where

import Data.Maybe (fromJust)

-- Partial: head
example1 :: [a] -> a
example1 xs = head xs

-- Partial: fromJust
example2 :: Maybe a -> a
example2 x = fromJust x
