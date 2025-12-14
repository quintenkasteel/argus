{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Strings
-- Description : String and Text manipulation rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for string manipulation patterns, encouraging use of Text
-- and efficient string operations.
--
-- == Rule Categories
--
-- * __String to Text__: Migrate from String to Text
-- * __Concatenation__: Efficient string building
-- * __Conversion__: String conversion patterns
-- * __Manipulation__: String manipulation best practices

module Argus.Rules.Builtin.Strings
  ( -- * Rule Sets
    stringRules
  , stringToTextRules
  , concatenationRules
  , stringConversionRules
  , manipulationRules

    -- * String to Text
  , preferText
  , packUnpack
  , unpackPack
  , textShow
  , readString
  , stringLiteral

    -- * Concatenation
  , concatToMconcat
  , appendToMappend
  , plusPlusToAppend
  , multipleAppend
  , builderPattern
  , intercalateToUnwords

    -- * Conversion
  , showReadId
  , readMaybePrefer
  , parseTimeString
  , encodeDecodeUtf8
  , unpackRepack
  , bsToText

    -- * Manipulation
  , nullToIsEmpty
  , lengthToCompare
  , headToUncons
  , lastToUnsnoc
  , takeDropToSplitAt
  , wordsToSplit
  , linesToSplit
  , stripToStrip
  , toLowerToCase
  , toUpperToCase

    -- * Rule Count
  , stringRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All string and text rules.
stringRules :: [Rule]
stringRules = mconcat
  [ stringToTextRules
  , concatenationRules
  , stringConversionRules
  , manipulationRules
  ]

-- | Total count of string rules.
stringRuleCount :: Int
stringRuleCount = length stringRules

--------------------------------------------------------------------------------
-- String to Text Rules
--------------------------------------------------------------------------------

-- | Rules for migrating String to Text.
stringToTextRules :: [Rule]
stringToTextRules =
  [ preferText
  , packUnpack
  , unpackPack
  , textShow
  , readString
  , stringLiteral
  ]

-- | Prefer Text over String for most uses.
--
-- @
-- foo :: String -> String  -- Consider Text
-- @
preferText :: Rule
preferText =
  rule "prefer-text" $
    matchText ":: String -> String"
    & category Performance
    & severity Info
    & message "Consider using Text instead of String"
    & note "Text is more memory-efficient and faster for most operations"
    & safetyLevel ManualReview

-- | pack . unpack is identity.
--
-- @
-- T.pack (T.unpack x)  ==>  x
-- @
packUnpack :: Rule
packUnpack =
  rule "pack-unpack" $
    match ("T.pack (T.unpack _x)" ==> "_x")
    & category Redundant
    & severity Warning
    & message "pack . unpack is identity for Text"

-- | unpack . pack is identity.
--
-- @
-- T.unpack (T.pack x)  ==>  x
-- @
unpackPack :: Rule
unpackPack =
  rule "unpack-pack" $
    match ("T.unpack (T.pack _x)" ==> "_x")
    & category Redundant
    & severity Warning
    & message "unpack . pack is identity for String"

-- | Use T.pack . show instead of show for Text.
--
-- @
-- T.pack (show x)  -- Consider using a Text show function
-- @
textShow :: Rule
textShow =
  rule "text-show" $
    match ("T.pack (show _x)" ==> "T.pack (show _x)")
    & category Style
    & severity Info
    & message "Consider using a direct Text show function"
    & note "Some libraries provide showt or similar"
    & safetyLevel ManualReview

-- | read with String is often error-prone.
--
-- @
-- read s :: Int  -- Throws on invalid input
-- @
readString :: Rule
readString =
  rule "read-string" $
    matchText "read [a-z]+ :: "
    & category Safety
    & severity Warning
    & message "read can throw exceptions on invalid input"
    & note "Consider using readMaybe from Text.Read"

-- | String literal overloading.
--
-- @
-- "literal" :: Text  -- Use OverloadedStrings
-- @
stringLiteral :: Rule
stringLiteral =
  rule "string-literal" $
    matchText "T.pack \"[^\"]*\""
    & category Style
    & severity Suggestion
    & message "Use OverloadedStrings instead of T.pack for literals"
    & note "Enable {-# LANGUAGE OverloadedStrings #-}"

--------------------------------------------------------------------------------
-- Concatenation Rules
--------------------------------------------------------------------------------

-- | Rules for efficient string concatenation.
concatenationRules :: [Rule]
concatenationRules =
  [ concatToMconcat
  , appendToMappend
  , plusPlusToAppend
  , multipleAppend
  , builderPattern
  , intercalateToUnwords
  ]

-- | Use mconcat instead of concat for Text.
--
-- @
-- T.concat xs  ==>  mconcat xs
-- @
concatToMconcat :: Rule
concatToMconcat =
  rule "concat-to-mconcat" $
    match ("T.concat _xs" ==> "mconcat _xs")
    & category Modernization
    & severity Suggestion
    & message "Use mconcat instead of T.concat"
    & note "mconcat is more general and works with any Monoid"

-- | Use (<>) instead of mappend.
--
-- @
-- mappend x y  ==>  x <> y
-- @
appendToMappend :: Rule
appendToMappend =
  rule "mappend-to-diamond" $
    match ("mappend _x _y" ==> "_x <> _y")
    & category Modernization
    & severity Suggestion
    & message "Use (<>) instead of mappend"

-- | Use (<>) instead of (++) for String.
--
-- @
-- x ++ y  ==>  x <> y
-- @
plusPlusToAppend :: Rule
plusPlusToAppend =
  rule "plusplus-to-diamond" $
    match ("_x ++ _y" ==> "_x <> _y")
    & category Modernization
    & severity Info
    & message "Consider using (<>) instead of (++)"
    & note "(<>) works with any Semigroup, not just lists"
    & safetyLevel ManualReview

-- | Multiple appends should use mconcat.
--
-- @
-- a <> b <> c <> d  ==>  mconcat [a, b, c, d]
-- @
multipleAppend :: Rule
multipleAppend =
  rule "multiple-append" $
    match ("_a <> _b <> _c <> _d" ==> "mconcat [_a, _b, _c, _d]")
    & category Style
    & severity Info
    & message "Consider mconcat for multiple concatenations"
    & safetyLevel ManualReview

-- | Use Builder for efficient Text construction.
--
-- @
-- T.concat [many, text, pieces]  -- Use Builder for efficiency
-- @
builderPattern :: Rule
builderPattern =
  rule "builder-pattern" $
    matchText "T.concat \\[.+,.+,.+,.+\\]"
    & category Performance
    & severity Info
    & message "Consider using Data.Text.Lazy.Builder for many concatenations"
    & safetyLevel ManualReview

-- | Use unwords instead of intercalate " ".
--
-- @
-- intercalate " " xs  ==>  unwords xs
-- @
intercalateToUnwords :: Rule
intercalateToUnwords =
  rule "intercalate-to-unwords" $
    match ("intercalate \" \" _xs" ==> "unwords _xs")
    & category Style
    & severity Suggestion
    & message "Use unwords instead of intercalate \" \""

--------------------------------------------------------------------------------
-- Conversion Rules
--------------------------------------------------------------------------------

-- | Rules for string conversion.
stringConversionRules :: [Rule]
stringConversionRules =
  [ showReadId
  , readMaybePrefer
  , parseTimeString
  , encodeDecodeUtf8
  , unpackRepack
  , bsToText
  ]

-- | show . read is often redundant.
--
-- @
-- show (read s :: Int)  -- May be redundant
-- @
showReadId :: Rule
showReadId =
  rule "show-read" $
    match ("show (read _s :: _t)" ==> "show (read _s :: _t)")
    & category Style
    & severity Info
    & message "show . read may be redundant or indicate type confusion"
    & safetyLevel ManualReview

-- | Use readMaybe instead of read.
--
-- @
-- read s  ==>  readMaybe s  (with proper handling)
-- @
readMaybePrefer :: Rule
readMaybePrefer =
  rule "readMaybe-prefer" $
    match ("read _s" ==> "readMaybe _s")
    & category Safety
    & severity Warning
    & message "Use readMaybe to handle parse failures gracefully"
    & note "read throws an exception on invalid input"

-- | Time parsing from string.
--
-- @
-- parseTimeM True defaultTimeLocale "%Y-%m-%d" s
-- @
parseTimeString :: Rule
parseTimeString =
  rule "parseTime-string" $
    matchText "parseTimeM .+ defaultTimeLocale"
    & category Style
    & severity Info
    & message "Time parsing - ensure format string matches input"
    & safetyLevel ManualReview

-- | encodeUtf8 . decodeUtf8 roundtrip.
--
-- @
-- encodeUtf8 (decodeUtf8 x)  -- May not be identity
-- @
encodeDecodeUtf8 :: Rule
encodeDecodeUtf8 =
  rule "encode-decode-utf8" $
    match ("encodeUtf8 (decodeUtf8 _x)" ==> "encodeUtf8 (decodeUtf8 _x)")
    & category Safety
    & severity Warning
    & message "encodeUtf8 . decodeUtf8 may not be identity"
    & note "Invalid UTF-8 sequences can cause data loss"
    & safetyLevel ManualReview

-- | Unpack and repack is wasteful.
--
-- @
-- T.pack (T.unpack x ++ T.unpack y)  ==>  x <> y
-- @
unpackRepack :: Rule
unpackRepack =
  rule "unpack-repack" $
    match ("T.pack (T.unpack _x ++ T.unpack _y)" ==> "_x <> _y")
    & category Performance
    & severity Warning
    & message "Avoid unpacking Text to String for concatenation"

-- | ByteString to Text conversion.
--
-- @
-- T.pack (BS.unpack x)  -- Use decodeUtf8
-- @
bsToText :: Rule
bsToText =
  rule "bs-to-text" $
    match ("T.pack (BS.unpack _x)" ==> "decodeUtf8 _x")
    & category Performance
    & severity Warning
    & message "Use decodeUtf8 instead of T.pack . BS.unpack"

--------------------------------------------------------------------------------
-- Manipulation Rules
--------------------------------------------------------------------------------

-- | Rules for string manipulation.
manipulationRules :: [Rule]
manipulationRules =
  [ nullToIsEmpty
  , lengthToCompare
  , headToUncons
  , lastToUnsnoc
  , takeDropToSplitAt
  , wordsToSplit
  , linesToSplit
  , stripToStrip
  , toLowerToCase
  , toUpperToCase
  ]

-- | Use T.null instead of == "".
--
-- @
-- x == ""  ==>  T.null x
-- T.length x == 0  ==>  T.null x
-- @
nullToIsEmpty :: Rule
nullToIsEmpty =
  rule "null-to-isEmpty" $
    match ("_x == \"\"" ==> "T.null _x")
    & category Performance
    & severity Suggestion
    & message "Use T.null instead of comparing to empty string"
    & note "T.null is O(1); comparison may be O(n)"

-- | Avoid length for comparison.
--
-- @
-- T.length x > 0  ==>  not (T.null x)
-- @
lengthToCompare :: Rule
lengthToCompare =
  rule "length-to-null" $
    match ("T.length _x > 0" ==> "not (T.null _x)")
    & category Performance
    & severity Suggestion
    & message "Use null check instead of length comparison"

-- | Use T.uncons instead of head.
--
-- @
-- T.head x  -- Throws on empty; use T.uncons
-- @
headToUncons :: Rule
headToUncons =
  rule "head-to-uncons" $
    match ("T.head _x" ==> "T.head _x")
    & category Safety
    & severity Warning
    & message "T.head throws on empty Text; consider T.uncons"
    & safetyLevel ManualReview

-- | Use T.unsnoc instead of last.
--
-- @
-- T.last x  -- Throws on empty; use T.unsnoc
-- @
lastToUnsnoc :: Rule
lastToUnsnoc =
  rule "last-to-unsnoc" $
    match ("T.last _x" ==> "T.last _x")
    & category Safety
    & severity Warning
    & message "T.last throws on empty Text; consider T.unsnoc"
    & safetyLevel ManualReview

-- | Use splitAt instead of take + drop.
--
-- @
-- (T.take n x, T.drop n x)  ==>  T.splitAt n x
-- @
takeDropToSplitAt :: Rule
takeDropToSplitAt =
  rule "take-drop-to-splitAt" $
    match ("(T.take _n _x, T.drop _n _x)" ==> "T.splitAt _n _x")
    & category Performance
    & severity Suggestion
    & message "Use splitAt instead of separate take and drop"

-- | words vs split.
--
-- @
-- T.split (== ' ') x  -- Similar to T.words but different
-- @
wordsToSplit :: Rule
wordsToSplit =
  rule "words-vs-split" $
    match ("T.split (== ' ') _x" ==> "T.split (== ' ') _x")
    & category Style
    & severity Info
    & message "T.split (== ' ') differs from T.words (whitespace handling)"
    & safetyLevel ManualReview

-- | lines vs split.
--
-- @
-- T.split (== '\\n') x  -- Similar to T.lines but different
-- @
linesToSplit :: Rule
linesToSplit =
  rule "lines-vs-split" $
    match ("T.split (== '\\n') _x" ==> "T.split (== '\\n') _x")
    & category Style
    & severity Info
    & message "T.split (== '\\n') differs from T.lines (trailing newline handling)"
    & safetyLevel ManualReview

-- | Use T.strip for trimming whitespace.
--
-- @
-- T.dropWhile isSpace (T.dropWhileEnd isSpace x)  ==>  T.strip x
-- @
stripToStrip :: Rule
stripToStrip =
  rule "strip-to-strip" $
    match ("T.dropWhile isSpace (T.dropWhileEnd isSpace _x)" ==> "T.strip _x")
    & category Style
    & severity Suggestion
    & message "Use T.strip to trim whitespace from both ends"

-- | Use T.toLower for case conversion.
--
-- @
-- T.map toLower x  ==>  T.toLower x
-- @
toLowerToCase :: Rule
toLowerToCase =
  rule "map-toLower" $
    match ("T.map toLower _x" ==> "T.toLower _x")
    & category Style
    & severity Suggestion
    & message "Use T.toLower instead of T.map toLower"

-- | Use T.toUpper for case conversion.
--
-- @
-- T.map toUpper x  ==>  T.toUpper x
-- @
toUpperToCase :: Rule
toUpperToCase =
  rule "map-toUpper" $
    match ("T.map toUpper _x" ==> "T.toUpper _x")
    & category Style
    & severity Suggestion
    & message "Use T.toUpper instead of T.map toUpper"
