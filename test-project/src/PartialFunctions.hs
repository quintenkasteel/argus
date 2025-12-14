{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Examples of partial function usage that Argus should detect
module PartialFunctions where

import Data.Maybe (fromJust)
import Data.List (head, tail, init, last, (!!))
import qualified Data.Map as M
import qualified Data.Text as T

-- | Uses partial 'head' - should suggest using 'listToMaybe' or pattern match
unsafeHead :: [a] -> a
unsafeHead xs = listToMaybe xs

-- | Uses partial 'tail' - should suggest safe alternative
unsafeTail :: [a] -> [a]
unsafeTail xs = tail xs

-- | Uses partial 'init' - should suggest safe alternative
unsafeInit :: [a] -> [a]
unsafeInit xs = init xs

-- | Uses partial 'last' - should suggest safe alternative
unsafeLast :: [a] -> a
unsafeLast xs = last xs

-- | Uses partial '!!' - should suggest safe alternative
unsafeIndex :: [a] -> Int -> a
unsafeIndex xs n = xs !! n

-- | Uses partial 'fromJust' - should suggest 'maybe' or pattern match
unsafeFromJust :: Maybe a -> a
unsafeFromJust = fromJust

-- | Uses multiple partial functions in one function
multiplePartials :: [Int] -> Int
multiplePartials xs = listToMaybe xs + last xs + (xs !! 2)

-- | Uses partial 'read' - should suggest 'readMaybe'
unsafeRead :: String -> Int
unsafeRead s = read s

-- | Uses partial Map lookup with (!)
unsafeMapLookup :: M.Map String Int -> Int
unsafeMapLookup m = m M.! "key"

-- | Uses pattern match that isn't exhaustive (compiler catches this)
-- partialPattern :: Maybe a -> a
-- partialPattern (Just x) = x

-- | Uses 'error' explicitly - should warn about explicit error
explicitError :: Int -> Int
explicitError x
  | x < 0 = error "negative input"
  | otherwise = x * 2

-- | Uses 'undefined' - should warn
usesUndefined :: Int -> Int
usesUndefined _ = undefined

-- | Safe alternatives for comparison
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeLookup :: Ord k => k -> M.Map k v -> Maybe v
safeLookup = M.lookup