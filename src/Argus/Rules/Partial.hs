{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Partial
-- Description : Detection of partial function usage
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module detects usage of partial functions that can crash at runtime,
-- such as head, tail, fromJust, etc. It provides suggestions for safer alternatives.
module Argus.Rules.Partial
  ( -- * Detection
    detectPartialFunctions
  , PartialFunctionUsage (..)

    -- * Partial Function Database
  , PartialFuncInfo (..)
  , partialFunctions
  , lookupPartialFunction

    -- * Configuration
  , PartialConfig (..)
  , defaultPartialConfig
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import Argus.Types

--------------------------------------------------------------------------------
-- Partial Function Database
--------------------------------------------------------------------------------

-- | Information about a partial function
data PartialFuncInfo = PartialFuncInfo
  { pfiName        :: Text      -- ^ Function name
  , pfiModule      :: Text      -- ^ Module it's from
  , pfiReason      :: Text      -- ^ Why it's partial
  , pfiSafeAlt     :: Text      -- ^ Safe alternative
  , pfiSafeModule  :: Text      -- ^ Module of safe alternative
  , pfiSeverity    :: Severity  -- ^ How dangerous
  , pfiCategory    :: Text      -- ^ Category (list, maybe, etc.)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | All known partial functions
partialFunctions :: Map Text PartialFuncInfo
partialFunctions = Map.fromList $ map (\pf -> (pfiName pf, pf))
  -- List functions
  [ PartialFuncInfo "head" "Data.List" "Crashes on empty list" "headMay" "Safe" Warning "list"
  , PartialFuncInfo "tail" "Data.List" "Crashes on empty list" "tailMay" "Safe" Warning "list"
  , PartialFuncInfo "init" "Data.List" "Crashes on empty list" "initMay" "Safe" Warning "list"
  , PartialFuncInfo "last" "Data.List" "Crashes on empty list" "lastMay" "Safe" Warning "list"
  , PartialFuncInfo "!!" "Data.List" "Index out of bounds" "atMay or !?" "Safe" Warning "list"
  , PartialFuncInfo "cycle" "Data.List" "Hangs on empty list" "cycleMay" "Safe.Foldable" Suggestion "list"
  , PartialFuncInfo "maximum" "Data.List" "Crashes on empty list" "maximumMay" "Safe" Warning "list"
  , PartialFuncInfo "minimum" "Data.List" "Crashes on empty list" "minimumMay" "Safe" Warning "list"
  , PartialFuncInfo "maximumBy" "Data.List" "Crashes on empty list" "maximumByMay" "Safe" Warning "list"
  , PartialFuncInfo "minimumBy" "Data.List" "Crashes on empty list" "minimumByMay" "Safe" Warning "list"
  , PartialFuncInfo "foldl1" "Data.List" "Crashes on empty list" "foldl1May" "Safe" Warning "list"
  , PartialFuncInfo "foldr1" "Data.List" "Crashes on empty list" "foldr1May" "Safe" Warning "list"
  , PartialFuncInfo "foldl1'" "Data.List" "Crashes on empty list" "foldl1May'" "Safe" Warning "list"
  , PartialFuncInfo "scanl1" "Data.List" "Crashes on empty list" "scanl1May" "Safe" Warning "list"
  , PartialFuncInfo "scanr1" "Data.List" "Crashes on empty list" "scanr1May" "Safe" Warning "list"

  -- Maybe functions
  , PartialFuncInfo "fromJust" "Data.Maybe" "Crashes on Nothing" "fromMaybe default or pattern match" "Data.Maybe" Warning "maybe"
  , PartialFuncInfo "fromJust" "GHC.Maybe" "Crashes on Nothing" "fromMaybe default or pattern match" "Data.Maybe" Warning "maybe"

  -- Read/Show
  , PartialFuncInfo "read" "Text.Read" "Crashes on parse failure" "readMaybe" "Text.Read" Warning "parse"
  , PartialFuncInfo "read" "Prelude" "Crashes on parse failure" "readMaybe" "Text.Read" Warning "parse"

  -- Enum functions
  , PartialFuncInfo "succ" "Prelude" "Crashes at maxBound" "succMay" "Safe.Enum" Suggestion "enum"
  , PartialFuncInfo "pred" "Prelude" "Crashes at minBound" "predMay" "Safe.Enum" Suggestion "enum"
  , PartialFuncInfo "toEnum" "Prelude" "Crashes on out-of-range" "toEnumMay" "Safe.Enum" Warning "enum"

  -- Vector/Array operations
  , PartialFuncInfo "V.head" "Data.Vector" "Crashes on empty" "V.headM" "Data.Vector" Warning "vector"
  , PartialFuncInfo "V.last" "Data.Vector" "Crashes on empty" "V.lastM" "Data.Vector" Warning "vector"
  , PartialFuncInfo "V.!" "Data.Vector" "Index out of bounds" "V.!?" "Data.Vector" Warning "vector"
  , PartialFuncInfo "V.unsafeHead" "Data.Vector" "Undefined on empty" "V.headM" "Data.Vector" Error "vector"
  , PartialFuncInfo "V.unsafeLast" "Data.Vector" "Undefined on empty" "V.lastM" "Data.Vector" Error "vector"
  , PartialFuncInfo "V.unsafeIndex" "Data.Vector" "No bounds check" "V.!?" "Data.Vector" Error "vector"

  -- Map operations
  , PartialFuncInfo "M.!" "Data.Map" "Key not found" "M.lookup or M.!?" "Data.Map" Warning "map"
  , PartialFuncInfo "M.findWithDefault" "Data.Map" "Consider M.lookup" "M.lookup" "Data.Map" Suggestion "map"
  , PartialFuncInfo "IM.!" "Data.IntMap" "Key not found" "IM.lookup or IM.!?" "Data.IntMap" Warning "map"

  -- Text operations
  , PartialFuncInfo "T.head" "Data.Text" "Crashes on empty" "T.uncons" "Data.Text" Warning "text"
  , PartialFuncInfo "T.tail" "Data.Text" "Crashes on empty" "T.uncons" "Data.Text" Warning "text"
  , PartialFuncInfo "T.init" "Data.Text" "Crashes on empty" "T.unsnoc" "Data.Text" Warning "text"
  , PartialFuncInfo "T.last" "Data.Text" "Crashes on empty" "T.unsnoc" "Data.Text" Warning "text"

  -- ByteString operations
  , PartialFuncInfo "B.head" "Data.ByteString" "Crashes on empty" "B.uncons" "Data.ByteString" Warning "bytestring"
  , PartialFuncInfo "B.tail" "Data.ByteString" "Crashes on empty" "B.uncons" "Data.ByteString" Warning "bytestring"
  , PartialFuncInfo "B.init" "Data.ByteString" "Crashes on empty" "B.unsnoc" "Data.ByteString" Warning "bytestring"
  , PartialFuncInfo "B.last" "Data.ByteString" "Crashes on empty" "B.unsnoc" "Data.ByteString" Warning "bytestring"

  -- NonEmpty constructors
  , PartialFuncInfo "fromList" "Data.List.NonEmpty" "Crashes on empty" "nonEmpty" "Data.List.NonEmpty" Warning "nonempty"

  -- Error/undefined
  , PartialFuncInfo "error" "Prelude" "Always crashes" "Either/Maybe for errors" "base" Info "error"
  , PartialFuncInfo "undefined" "Prelude" "Always crashes" "typed holes or proper implementation" "base" Warning "error"
  , PartialFuncInfo "errorWithoutStackTrace" "GHC.Err" "Always crashes" "Either/Maybe for errors" "base" Info "error"

  -- Numeric
  , PartialFuncInfo "div" "Prelude" "Division by zero" "Safe.div" "Safe" Suggestion "numeric"
  , PartialFuncInfo "mod" "Prelude" "Division by zero" "Safe.mod" "Safe" Suggestion "numeric"
  , PartialFuncInfo "quot" "Prelude" "Division by zero" "Safe.quot" "Safe" Suggestion "numeric"
  , PartialFuncInfo "rem" "Prelude" "Division by zero" "Safe.rem" "Safe" Suggestion "numeric"
  , PartialFuncInfo "divMod" "Prelude" "Division by zero" "Safe.divMod" "Safe" Suggestion "numeric"
  , PartialFuncInfo "quotRem" "Prelude" "Division by zero" "Safe.quotRem" "Safe" Suggestion "numeric"
  , PartialFuncInfo "/" "Prelude" "Division by zero for Fractional" "check for zero" "Prelude" Info "numeric"

  -- Sequence operations
  , PartialFuncInfo "Seq.index" "Data.Sequence" "Index out of bounds" "Seq.lookup" "Data.Sequence" Warning "sequence"
  , PartialFuncInfo "Seq.!?" "Data.Sequence" "Returns Nothing for invalid index" "Seq.lookup" "Data.Sequence" Info "sequence"

  -- Set operations
  , PartialFuncInfo "Set.findMin" "Data.Set" "Crashes on empty set" "Set.lookupMin" "Data.Set" Warning "set"
  , PartialFuncInfo "Set.findMax" "Data.Set" "Crashes on empty set" "Set.lookupMax" "Data.Set" Warning "set"
  , PartialFuncInfo "Set.deleteMin" "Data.Set" "Undefined on empty set" "Set.minView" "Data.Set" Warning "set"
  , PartialFuncInfo "Set.deleteMax" "Data.Set" "Undefined on empty set" "Set.maxView" "Data.Set" Warning "set"
  , PartialFuncInfo "Set.elemAt" "Data.Set" "Index out of bounds" "Set.lookupIndex with bounds check" "Data.Set" Warning "set"

  -- IntSet operations
  , PartialFuncInfo "IS.findMin" "Data.IntSet" "Crashes on empty set" "IS.lookupMin" "Data.IntSet" Warning "intset"
  , PartialFuncInfo "IS.findMax" "Data.IntSet" "Crashes on empty set" "IS.lookupMax" "Data.IntSet" Warning "intset"
  , PartialFuncInfo "IS.deleteMin" "Data.IntSet" "Undefined on empty set" "IS.minView" "Data.IntSet" Warning "intset"
  , PartialFuncInfo "IS.deleteMax" "Data.IntSet" "Undefined on empty set" "IS.maxView" "Data.IntSet" Warning "intset"

  -- Array operations
  , PartialFuncInfo "A.!" "Data.Array" "Index out of bounds" "A.bounds check first" "Data.Array" Warning "array"
  , PartialFuncInfo "A.//" "Data.Array" "Index out of bounds" "A.bounds check first" "Data.Array" Warning "array"

  -- IORef/STRef operations
  , PartialFuncInfo "readIORef" "Data.IORef" "May read uninitialized" "use Maybe wrapper" "Data.IORef" Info "ref"
  , PartialFuncInfo "readSTRef" "Data.STRef" "May read uninitialized" "use Maybe wrapper" "Data.STRef" Info "ref"

  -- Lazy IO (potentially problematic)
  , PartialFuncInfo "getContents" "System.IO" "Lazy IO can cause resource issues" "strict IO or streaming" "Data.ByteString" Suggestion "io"
  , PartialFuncInfo "hGetContents" "System.IO" "Lazy IO can cause resource issues" "strict IO or streaming" "Data.ByteString" Suggestion "io"
  , PartialFuncInfo "readFile" "Prelude" "Lazy IO and can crash" "strict readFile" "Data.Text.IO" Suggestion "io"

  -- MonadFail
  , PartialFuncInfo "fail" "Control.Monad" "Calls error in many monads" "use Maybe/Either/ExceptT" "Control.Monad.Except" Warning "monad"
  , PartialFuncInfo "mzero" "Control.Monad" "May call error in some monads" "explicit error handling" "Control.Monad" Info "monad"

  -- Exception throwing
  , PartialFuncInfo "throw" "Control.Exception" "Throws imprecise exception" "throwIO or ExceptT" "Control.Exception" Warning "exception"
  , PartialFuncInfo "throwIO" "Control.Exception" "Throws IO exception" "ExceptT for recoverable errors" "Control.Monad.Except" Suggestion "exception"
  , PartialFuncInfo "throwSTM" "Control.Concurrent.STM" "Throws STM exception" "use retry or explicit handling" "Control.Concurrent.STM" Suggestion "exception"

  -- Async exceptions
  , PartialFuncInfo "throwTo" "Control.Concurrent" "Sends async exception" "use message passing or MVars" "Control.Concurrent" Info "async"

  -- Pattern matching helpers often misused
  , PartialFuncInfo "Data.Either.fromLeft" "Data.Either" "Crashes on Right" "either id or pattern match" "Data.Either" Warning "either"
  , PartialFuncInfo "Data.Either.fromRight" "Data.Either" "Crashes on Left" "either or pattern match" "Data.Either" Warning "either"

  -- Tuple accessors (extended)
  , PartialFuncInfo "fst3" "Data.Tuple.Extra" "Only works on 3-tuple" "pattern match" "Data.Tuple.Extra" Info "tuple"
  , PartialFuncInfo "snd3" "Data.Tuple.Extra" "Only works on 3-tuple" "pattern match" "Data.Tuple.Extra" Info "tuple"
  , PartialFuncInfo "thd3" "Data.Tuple.Extra" "Only works on 3-tuple" "pattern match" "Data.Tuple.Extra" Info "tuple"

  -- Lazy ByteString
  , PartialFuncInfo "BL.head" "Data.ByteString.Lazy" "Crashes on empty" "BL.uncons" "Data.ByteString.Lazy" Warning "bytestring"
  , PartialFuncInfo "BL.tail" "Data.ByteString.Lazy" "Crashes on empty" "BL.uncons" "Data.ByteString.Lazy" Warning "bytestring"
  , PartialFuncInfo "BL.init" "Data.ByteString.Lazy" "Crashes on empty" "BL.unsnoc" "Data.ByteString.Lazy" Warning "bytestring"
  , PartialFuncInfo "BL.last" "Data.ByteString.Lazy" "Crashes on empty" "BL.unsnoc" "Data.ByteString.Lazy" Warning "bytestring"

  -- Lazy Text
  , PartialFuncInfo "TL.head" "Data.Text.Lazy" "Crashes on empty" "TL.uncons" "Data.Text.Lazy" Warning "text"
  , PartialFuncInfo "TL.tail" "Data.Text.Lazy" "Crashes on empty" "TL.uncons" "Data.Text.Lazy" Warning "text"
  , PartialFuncInfo "TL.init" "Data.Text.Lazy" "Crashes on empty" "TL.unsnoc" "Data.Text.Lazy" Warning "text"
  , PartialFuncInfo "TL.last" "Data.Text.Lazy" "Crashes on empty" "TL.unsnoc" "Data.Text.Lazy" Warning "text"

  -- Words/chars conversions
  , PartialFuncInfo "chr" "Data.Char" "Out of range crashes" "chr with bounds check" "Data.Char" Warning "char"
  , PartialFuncInfo "digitToInt" "Data.Char" "Non-digit crashes" "use readMaybe" "Text.Read" Warning "char"

  -- Bits operations
  , PartialFuncInfo "bit" "Data.Bits" "Negative/too large bit crashes" "use testBit safely" "Data.Bits" Suggestion "bits"
  , PartialFuncInfo "shiftL" "Data.Bits" "Negative shift undefined" "check shift amount" "Data.Bits" Suggestion "bits"
  , PartialFuncInfo "shiftR" "Data.Bits" "Negative shift undefined" "check shift amount" "Data.Bits" Suggestion "bits"

  -- Aeson partial operations
  , PartialFuncInfo ".:?" "Data.Aeson" "Silent failure to Maybe" ".:! for explicit Nothing" "Data.Aeson" Info "json"
  , PartialFuncInfo "decode" "Data.Aeson" "Returns Nothing on failure" "eitherDecode for errors" "Data.Aeson" Suggestion "json"

  -- Time partial operations
  , PartialFuncInfo "parseTimeOrError" "Data.Time" "Crashes on parse failure" "parseTimeM" "Data.Time" Warning "time"
  , PartialFuncInfo "readTime" "Data.Time" "Crashes on parse failure" "parseTimeM" "Data.Time" Warning "time"

  -- Generic Foldable partials
  , PartialFuncInfo "foldr1" "Data.Foldable" "Crashes on empty" "foldr with explicit base" "Data.Foldable" Warning "foldable"
  , PartialFuncInfo "foldl1" "Data.Foldable" "Crashes on empty" "foldl' with explicit base" "Data.Foldable" Warning "foldable"
  , PartialFuncInfo "maximum" "Data.Foldable" "Crashes on empty" "maximumMay" "Safe" Warning "foldable"
  , PartialFuncInfo "minimum" "Data.Foldable" "Crashes on empty" "minimumMay" "Safe" Warning "foldable"
  ]

-- | Lookup a partial function by name
lookupPartialFunction :: Text -> Maybe PartialFuncInfo
lookupPartialFunction = flip Map.lookup partialFunctions

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for partial function detection
data PartialConfig = PartialConfig
  { pcEnabled            :: Bool        -- ^ Enable partial function detection
  , pcDisabledFunctions  :: [Text]      -- ^ Functions to ignore
  , pcMinSeverity        :: Severity    -- ^ Minimum severity to report
  , pcAllowInTests       :: Bool        -- ^ Allow partial functions in test files
  , pcAllowWithComment   :: Bool        -- ^ Allow if annotated with comment
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration
defaultPartialConfig :: PartialConfig
defaultPartialConfig = PartialConfig
  { pcEnabled = True
  , pcDisabledFunctions = []
  , pcMinSeverity = Suggestion
  , pcAllowInTests = True
  , pcAllowWithComment = True
  }

--------------------------------------------------------------------------------
-- Usage Detection
--------------------------------------------------------------------------------

-- | A detected partial function usage
data PartialFunctionUsage = PartialFunctionUsage
  { pfuFunction   :: PartialFuncInfo  -- ^ The partial function
  , pfuSpan       :: SrcSpan          -- ^ Where it's used
  , pfuContext    :: Text             -- ^ Surrounding context
  , pfuInTest     :: Bool             -- ^ Is this in a test file?
  , pfuHasComment :: Bool             -- ^ Has safety comment?
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Detect partial function usage in source code
detectPartialFunctions :: PartialConfig -> FilePath -> Text -> [Diagnostic]
detectPartialFunctions config path content
  | not (pcEnabled config) = []
  | otherwise =
    let usages = findPartialUsages path content
        filtered = filterUsages config usages
    in map usageToDiagnostic filtered

-- | Find all partial function usages
findPartialUsages :: FilePath -> Text -> [PartialFunctionUsage]
findPartialUsages path content =
  let linesWithNums = zip [1..] (T.lines content)
      isTestFile = isTestPath (T.pack path)
  in concatMap (findInLine path isTestFile) linesWithNums

-- | Check if a file path is a test file
isTestPath :: Text -> Bool
isTestPath path =
  let lowerPath = T.toLower path
      pathComponents = T.splitOn "/" path
  in -- Check for Test/Spec in filename
     "Test" `T.isInfixOf` path ||
     "Spec" `T.isInfixOf` path ||
     -- Check for test directories (case-insensitive)
     "test/" `T.isPrefixOf` lowerPath ||
     "tests/" `T.isPrefixOf` lowerPath ||
     "/test/" `T.isInfixOf` lowerPath ||
     "/tests/" `T.isInfixOf` lowerPath ||
     "test-suite/" `T.isPrefixOf` lowerPath ||
     "/test-suite/" `T.isInfixOf` lowerPath ||
     -- Check for test as a path component
     "test" `elem` map T.toLower pathComponents ||
     "tests" `elem` map T.toLower pathComponents

-- | Find partial function usages in a single line
findInLine :: FilePath -> Bool -> (Int, Text) -> [PartialFunctionUsage]
findInLine path isTest (lineNum, line) =
  let hasComment = "-- PARTIAL:" `T.isInfixOf` line ||
                   "-- safe:" `T.isInfixOf` T.toLower line ||
                   "{- PARTIAL" `T.isInfixOf` line
  in mapMaybe (checkFunction path isTest hasComment lineNum line) (Map.keys partialFunctions)

-- | Check if a specific partial function is used in a line
checkFunction :: FilePath -> Bool -> Bool -> Int -> Text -> Text -> Maybe PartialFunctionUsage
checkFunction path isTest hasComment lineNum line funcName =
  case Map.lookup funcName partialFunctions of
    Nothing -> Nothing
    Just pf ->
      if funcName `isUsedIn` line
        then Just PartialFunctionUsage
          { pfuFunction = pf
          , pfuSpan = findFunctionSpan path lineNum funcName line
          , pfuContext = T.strip line
          , pfuInTest = isTest
          , pfuHasComment = hasComment
          }
        else Nothing

-- | Check if a function name is used in a line (not just as substring)
isUsedIn :: Text -> Text -> Bool
isUsedIn funcName line =
  -- Handle both qualified and unqualified names
  let -- Check for unqualified use with word boundaries
      -- unqualifiedPattern = funcName  -- unused variable
      -- Check for qualified use like "Data.List.head"
      qualifiedPatterns = [funcName, "." <> funcName]
  in any (`isWordIn` line) qualifiedPatterns

-- | Check if a word appears as a whole word in text
isWordIn :: Text -> Text -> Bool
isWordIn word text =
  let tokens = tokenize text
  in word `elem` tokens || any (T.isSuffixOf ("." <> word)) tokens

-- | Simple tokenization
tokenize :: Text -> [Text]
tokenize = T.words . T.map replaceOp
  where
    replaceOp c
      | c `elem` ("()[]{},.;`" :: String) = ' '
      | otherwise = c

-- | Find the span of a function usage
findFunctionSpan :: FilePath -> Int -> Text -> Text -> SrcSpan
findFunctionSpan path lineNum funcName line =
  let col = findColumn funcName line
  in mkSrcSpanRaw path lineNum col lineNum (col + T.length funcName)

-- | Find column of function in line
findColumn :: Text -> Text -> Int
findColumn funcName line =
  case T.breakOn funcName line of
    (before, _) -> T.length before + 1

-- | Filter usages based on configuration
filterUsages :: PartialConfig -> [PartialFunctionUsage] -> [PartialFunctionUsage]
filterUsages PartialConfig{..} = filter shouldReport
  where
    shouldReport usage =
      let pf = pfuFunction usage
      in pfiName pf `notElem` pcDisabledFunctions &&
         pfiSeverity pf >= pcMinSeverity &&
         not (pcAllowInTests && pfuInTest usage) &&
         not (pcAllowWithComment && pfuHasComment usage)

-- | Convert usage to diagnostic
usageToDiagnostic :: PartialFunctionUsage -> Diagnostic
usageToDiagnostic PartialFunctionUsage{..} =
  let pf = pfuFunction
  in Diagnostic
    { diagSpan = pfuSpan
    , diagSeverity = pfiSeverity pf
    , diagKind = PartialFunction
    , diagMessage = T.concat
        [ "Use of partial function '", pfiName pf, "': "
        , pfiReason pf
        , ". Consider using '", pfiSafeAlt pf, "' from '", pfiSafeModule pf, "' instead."
        ]
    , diagCode = Just $ "partial/" <> pfiCategory pf <> "/" <> pfiName pf
    , diagFixes = [makeFix pf pfuSpan]
    , diagRelated = []
    }

-- | Create an auto-fix suggestion with required imports
makeFix :: PartialFuncInfo -> SrcSpan -> Fix
makeFix pf srcSpan = Fix
  { fixTitle = "Replace with " <> pfiSafeAlt pf
  , fixEdits = [FixEdit srcSpan (primaryAlt pf)]
  , fixIsPreferred = True
  , fixAddImports = makeImports pf
  , fixRemoveImports = []
  , fixCategory = FCSafety
  , fixSafety = FSMostly
  }
  where
    -- Extract the primary alternative (first word if multiple options)
    primaryAlt :: PartialFuncInfo -> Text
    primaryAlt p = case T.words (pfiSafeAlt p) of
      (alt:_) -> alt
      [] -> pfiSafeAlt p

    -- Generate imports for the safe alternative
    makeImports :: PartialFuncInfo -> [FixImport]
    makeImports p
      -- Skip imports for Prelude functions or manual fixes
      | pfiSafeModule p `elem` ["base", "Prelude", "Data.Maybe"] &&
        pfiSafeAlt p `elem` ["pattern match", "fromMaybe default or pattern match"] = []
      -- Safe package functions (headMay, tailMay, etc.)
      | pfiSafeModule p == "Safe" = [mkImport "Safe" (primaryAlt p)]
      | pfiSafeModule p == "Safe.Foldable" = [mkImport "Safe.Foldable" (primaryAlt p)]
      | pfiSafeModule p == "Safe.Enum" = [mkImport "Safe.Enum" (primaryAlt p)]
      -- Text.Read.readMaybe
      | pfiSafeModule p == "Text.Read" = [mkImport "Text.Read" (primaryAlt p)]
      -- Vector alternatives
      | pfiSafeModule p == "Data.Vector" =
          [mkQualifiedImport "Data.Vector" "V"]
      -- Map alternatives
      | pfiSafeModule p == "Data.Map" =
          [mkQualifiedImport "Data.Map" "M"]
      | pfiSafeModule p == "Data.IntMap" =
          [mkQualifiedImport "Data.IntMap" "IM"]
      -- Text alternatives (uncons, unsnoc)
      | pfiSafeModule p == "Data.Text" =
          [mkQualifiedImport "Data.Text" "T"]
      -- ByteString alternatives
      | pfiSafeModule p == "Data.ByteString" =
          [mkQualifiedImport "Data.ByteString" "B"]
      -- NonEmpty
      | pfiSafeModule p == "Data.List.NonEmpty" =
          [mkImport "Data.List.NonEmpty" (primaryAlt p)]
      -- Default case
      | otherwise = [mkImport (pfiSafeModule p) (primaryAlt p)]

-- | Create a simple explicit import
mkImport :: Text -> Text -> FixImport
mkImport modName symName = FixImport
  { fimpModule = modName
  , fimpSymbols = [ImportSymbol symName ISTFunction []]
  , fimpQualified = Nothing
  , fimpHiding = False
  , fimpPackage = safePackage modName
  }
  where
    -- Add package qualifier for 'safe' package
    safePackage m
      | "Safe" `T.isPrefixOf` m = Just "safe"
      | otherwise = Nothing

-- | Create a qualified import
mkQualifiedImport :: Text -> Text -> FixImport
mkQualifiedImport modName alias = FixImport
  { fimpModule = modName
  , fimpSymbols = []
  , fimpQualified = Just alias
  , fimpHiding = False
  , fimpPackage = Nothing
  }
