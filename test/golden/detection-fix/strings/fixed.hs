{-# LANGUAGE OverloadedStrings #-}

module StringPatterns where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Char (toLower, toUpper, isSpace)
import Data.List (intercalate)

--------------------------------------------------------------------------------
-- Pack/Unpack Patterns
--------------------------------------------------------------------------------

-- Should simplify: T.pack (T.unpack x)  ==>  x
packUnpackRoundtrip :: Text -> Text
packUnpackRoundtrip x = T.pack (T.unpack x)

-- Should simplify: T.unpack (T.pack x)  ==>  x
unpackPackRoundtrip :: String -> String
unpackPackRoundtrip x = T.unpack (T.pack x)

-- Should suggest using decodeUtf8: T.pack (BS8.unpack x)  ==>  decodeUtf8 x
byteStringToText :: BS.ByteString -> Text
byteStringToText x = T.pack (BS8.unpack x)

-- Should suggest avoiding roundtrip: T.pack (T.unpack x ++ T.unpack y)  ==>  x <> y
unpackConcatRepack :: Text -> Text -> Text
unpackConcatRepack x y = T.pack (T.unpack x ++ T.unpack y)

--------------------------------------------------------------------------------
-- String Concatenation Patterns
--------------------------------------------------------------------------------

-- Should suggest mconcat: T.concat xs  ==>  mconcat xs
concatTexts :: [Text] -> Text
concatTexts xs = T.concat xs

-- Should suggest (<>): mappend x y  ==>  x <> y
mappendToAppend :: Text -> Text -> Text
mappendToAppend x y = (<>) x y

-- Should suggest (<>): x ++ y  ==>  x <> y
plusPlusConcat :: String -> String -> String
plusPlusConcat x y = x ++ y

-- Should suggest mconcat: a <> b <> c <> d  ==>  mconcat [a, b, c, d]
multipleAppends :: Text -> Text -> Text -> Text -> Text
multipleAppends a b c d = a <> b <> c <> d

-- Should suggest Builder: T.concat [many, pieces, of, text, here]
concatManyTexts :: Text -> Text -> Text -> Text -> Text -> Text
concatManyTexts a b c d e = T.concat [a, b, c, d, e]

-- Should suggest unwords: intercalate " " xs  ==>  unwords xs
intercalateSpaces :: [String] -> String
intercalateSpaces xs = intercalate " " xs

--------------------------------------------------------------------------------
-- String Conversion Patterns
--------------------------------------------------------------------------------

-- Should warn about read safety: read s  ==>  readMaybe s
unsafeReadInt :: String -> Int
unsafeReadInt s = read s

-- Should suggest OverloadedStrings: T.pack "literal"  ==>  "literal"
packStringLiteral :: Text
packStringLiteral = T.pack "hello world"

-- Should suggest checking show/read: show (read s :: Int)
showReadPattern :: String -> String
showReadPattern s = show (read s :: Int)

-- Should warn about UTF-8 roundtrip: encodeUtf8 (decodeUtf8 x)
utf8Roundtrip :: BS.ByteString -> BS.ByteString
utf8Roundtrip x = encodeUtf8 (decodeUtf8 x)

--------------------------------------------------------------------------------
-- String Checking Patterns
--------------------------------------------------------------------------------

-- Should suggest T.null: x == ""  ==>  T.null x
emptyStringCheck :: Text -> Bool
emptyStringCheck x = x == ""

-- Should suggest T.null: T.length x == 0  ==>  T.null x
lengthZeroCheck :: Text -> Bool
lengthZeroCheck x = T.null x

-- Should suggest null check: T.length x > 0  ==>  not (T.null x)
lengthGreaterThanZero :: Text -> Bool
lengthGreaterThanZero x = T.not (null x)

-- Should suggest null check: T.length x /= 0  ==>  not (T.null x)
lengthNotZero :: Text -> Bool
lengthNotZero x = T.not (null x)

--------------------------------------------------------------------------------
-- String Manipulation Patterns
--------------------------------------------------------------------------------

-- Should suggest T.toLower: T.map toLower x  ==>  T.toLower x
mapToLowerCase :: Text -> Text
mapToLowerCase x = T.map toLower x

-- Should suggest T.toUpper: T.map toUpper x  ==>  T.toUpper x
mapToUpperCase :: Text -> Text
mapToUpperCase x = T.map toUpper x

-- Should suggest T.strip: T.dropWhile isSpace (T.dropWhileEnd isSpace x)  ==>  T.strip x
manualStrip :: Text -> Text
manualStrip x = T.dropWhile isSpace (T.dropWhileEnd isSpace x)

-- Should suggest T.splitAt: (T.take n x, T.drop n x)  ==>  T.splitAt n x
takeDropPair :: Int -> Text -> (Text, Text)
takeDropPair n x = (T.take n x, T.drop n x)

-- Should warn about T.head safety: T.head x
unsafeTextHead :: Text -> Char
unsafeTextHead x = T.head x

-- Should warn about T.last safety: T.last x
unsafeTextLast :: Text -> Char
unsafeTextLast x = T.last x

--------------------------------------------------------------------------------
-- Complex Realistic Examples
--------------------------------------------------------------------------------

-- Multiple string inefficiencies
processUserInput :: String -> Text
processUserInput input =
  T.pack (T.unpack (T.pack input))

-- Inefficient concatenation chain
buildMessage :: String -> String -> String -> String
buildMessage greeting name punctuation =
  greeting ++ " " ++ name ++ punctuation

-- Check if string is empty inefficiently
validateInput :: Text -> Bool
validateInput txt = T.null txt

-- Convert and check inefficiently
processAndCheck :: BS.ByteString -> Bool
processAndCheck bs =
  let txt = T.pack (BS8.unpack bs)
  in T.not (null txt)

-- Manual case conversion
normalizeString :: Text -> Text
normalizeString txt = T.map toLower txt

-- Unsafe string operations
getFirstChar :: Text -> Char
getFirstChar txt = T.head txt

-- Pack literal instead of using OverloadedStrings
welcomeMessage :: Text
welcomeMessage = T.pack "Welcome to the application!"

-- Inefficient string building
buildPath :: [String] -> String
buildPath components = intercalate " " components

-- Unsafe read operation
parseNumber :: String -> Int
parseNumber str = read str

-- Multiple appends
concatMany :: Text -> Text -> Text -> Text -> Text
concatMany w x y z = w <> x <> y <> z

-- Check empty with comparison
isEmpty :: Text -> Bool
isEmpty s = s == ""

-- Split using take and drop
splitText :: Int -> Text -> (Text, Text)
splitText n s = (T.take n s, T.drop n s)

-- Use mappend instead of diamond
joinTexts :: Text -> Text -> Text
joinTexts a b = (<>) a b

-- Get length and compare to zero
hasContent :: Text -> Bool
hasContent t = T.not (null t)

-- Manual strip implementation
trimWhitespace :: Text -> Text
trimWhitespace text = T.dropWhile isSpace (T.dropWhileEnd isSpace text)

-- Use concat instead of mconcat
mergeAll :: [Text] -> Text
mergeAll texts = T.concat texts

-- Roundtrip unpack/pack in concatenation
joinWithUnpack :: Text -> Text -> Text
joinWithUnpack first second = T.pack (T.unpack first ++ T.unpack second)

-- Map case conversion functions
convertToUpper :: Text -> Text
convertToUpper input = T.map toUpper input

-- ByteString conversion the hard way
convertByteString :: BS.ByteString -> Text
convertByteString bytes = T.pack (BS8.unpack bytes)

-- Unsafe last character access
getLastCharacter :: Text -> Char
getLastCharacter txt = T.last txt