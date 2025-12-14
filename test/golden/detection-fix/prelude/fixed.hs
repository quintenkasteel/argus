{-# LANGUAGE OverloadedStrings #-}

module PreludeExamples where

import Prelude
import qualified Data.Text as T')
import qualified Data.Text as T (Text)

--------------------------------------------------------------------------------
-- Partial functions
--------------------------------------------------------------------------------

-- Should error: partial head
getFirst :: [a] -> a
getFirst xs = listToMaybe xs

-- Good: safe pattern match
getFirstSafe :: [a] -> Maybe a
getFirstSafe [] = Nothing
getFirstSafe (x:_) = Just x

-- Should error: partial tail
getRest :: [a] -> [a]
getRest xs = tail xs

-- Good: safe pattern match
getRestSafe :: [a] -> [a]
getRestSafe [] = []
getRestSafe (_:xs) = xs

-- Should error: partial (!!)
getAt :: [a] -> Int -> a
getAt xs i = xs !! i

-- Good: safe indexing
getAtSafe :: [a] -> Int -> Maybe a
getAtSafe xs i
  | i < 0 || i >= length xs = Nothing
  | otherwise = Just (xs !! i)

--------------------------------------------------------------------------------
-- fromJust
--------------------------------------------------------------------------------

-- Should error: fromJust is partial
extractValue :: Maybe Int -> Int
extractValue mx = fromJust mx
  where fromJust (Just x) = x
        fromJust Nothing = error "fromJust Nothing"

-- Good: pattern match or default
extractValueSafe :: Maybe Int -> Int
extractValueSafe = maybe 0 id

--------------------------------------------------------------------------------
-- read without validation
--------------------------------------------------------------------------------

-- Should error: read can throw
parseNumber :: String -> Int
parseNumber s = read s

-- Good: readMaybe
parseNumberSafe :: String -> Maybe Int
parseNumberSafe s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

--------------------------------------------------------------------------------
-- error/undefined
--------------------------------------------------------------------------------

-- Should error: error in production code
processOrFail :: Bool -> Int
processOrFail True = 42
processOrFail False = error "unexpected false"

-- Good: use Either for failure
processOrFailSafe :: Bool -> Either String Int
processOrFailSafe True = Right 42
processOrFailSafe False = Left "unexpected false"

-- Should error: undefined
incomplete :: Int -> Int
incomplete x = undefined

-- Good: explicit type signature with error
incompleteSafe :: Int -> Either String Int
incompleteSafe _ = Left "not implemented"

--------------------------------------------------------------------------------
-- String instead of Text
--------------------------------------------------------------------------------

-- Should suggest: use Text instead of String
processString :: String -> String
processString s = map toUpper s
  where toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

-- Good: use Text
processText :: Text -> Text
processText = T.toUpper

--------------------------------------------------------------------------------
-- length on String
--------------------------------------------------------------------------------

-- Should warn: O(n) length on String
stringLength :: String -> Int
stringLength s = length s

-- Good: use Text
textLength :: Text -> Int
textLength = T.length

--------------------------------------------------------------------------------
-- Lazy foldl
--------------------------------------------------------------------------------

-- Should warn: lazy foldl may cause space leak
sumWithFoldl :: [Int] -> Int
sumWithFoldl xs = foldl' (+) 0 xs

-- Good: strict foldl'
sumWithFoldl' :: [Int] -> Int
sumWithFoldl' xs = foldl' (+) 0 xs
  where foldl' f z [] = z
        foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

--------------------------------------------------------------------------------
-- maximum/minimum on possibly empty list
--------------------------------------------------------------------------------

-- Should error: maximum on possibly empty list
getMax :: [Int] -> Int
getMax xs = maximum xs

-- Good: safe maximum
getMaxSafe :: [Int] -> Maybe Int
getMaxSafe [] = Nothing
getMaxSafe xs = Just (maximum xs)

--------------------------------------------------------------------------------
-- succ/pred without bounds check
--------------------------------------------------------------------------------

-- Should warn: pred on bounded type without check
prevEnum :: Int -> Int
prevEnum x = pred x  -- Can underflow on minBound

-- Good: bounded check
prevEnumSafe :: (Bounded a, Enum a, Eq a) => a -> Maybe a
prevEnumSafe x
  | x == minBound = Nothing
  | otherwise = Just (pred x)

--------------------------------------------------------------------------------
-- fromIntegral without bounds
--------------------------------------------------------------------------------

-- Should warn: fromIntegral may overflow
intToWord8 :: Int -> Word
intToWord8 x = fromIntegral x

-- Good: bounded conversion
intToWord8Safe :: Int -> Maybe Word
intToWord8Safe x
  | x < 0 = Nothing
  | x > fromIntegral (maxBound :: Word) = Nothing
  | otherwise = Just (fromIntegral x)

--------------------------------------------------------------------------------
-- cycle on empty list
--------------------------------------------------------------------------------

-- Should warn: cycle on potentially empty list
repeatList :: [a] -> [a]
repeatList xs = cycle xs  -- Fails on []

-- Good: check for empty
repeatListSafe :: [a] -> [a]
repeatListSafe [] = []
repeatListSafe xs = cycle xs

--------------------------------------------------------------------------------
-- init/last partial
--------------------------------------------------------------------------------

-- Should error: init is partial
dropLast :: [a] -> [a]
dropLast xs = init xs

-- Good: safe init
dropLastSafe :: [a] -> [a]
dropLastSafe [] = []
dropLastSafe [_] = []
dropLastSafe (x:xs) = x : dropLastSafe xs

-- Should error: last is partial
getLast :: [a] -> a
getLast xs = last xs

-- Good: safe last
getLastSafe :: [a] -> Maybe a
getLastSafe [] = Nothing
getLastSafe [x] = Just x
getLastSafe (_:xs) = getLastSafe xs