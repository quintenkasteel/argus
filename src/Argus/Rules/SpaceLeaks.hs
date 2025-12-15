{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.SpaceLeaks
-- Description : Detection of space leak patterns
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module detects common patterns that lead to space leaks in Haskell,
-- including lazy folds, non-strict accumulators, and lazy state usage.
module Argus.Rules.SpaceLeaks
  ( -- * Detection
    detectSpaceLeaks
  , SpaceLeakPattern (..)
  , SpaceLeakCategory (..)

    -- * Configuration
  , SpaceLeakConfig (..)
  , defaultSpaceLeakConfig
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (mapMaybe, catMaybes)
import Data.Char (isAlphaNum)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import Argus.Analysis.TextProcessing (isCodeLine, extractCode, patternInCode)
import Argus.Types

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Category of space leak
data SpaceLeakCategory
  = LazyFold           -- ^ Using foldl instead of foldl'
  | LazyAccumulator    -- ^ Non-strict accumulator in recursion
  | LazyStateMonad     -- ^ Using lazy State instead of Strict
  | LazyData           -- ^ Non-strict data fields
  | LazyIO             -- ^ Lazy IO patterns
  | ThunkAccumulation  -- ^ Patterns that accumulate thunks
  | InfiniteStructure  -- ^ Operations on potentially infinite structures
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A detected space leak pattern
data SpaceLeakPattern = SpaceLeakPattern
  { slpCategory    :: SpaceLeakCategory  -- ^ Type of space leak
  , slpSpan        :: SrcSpan            -- ^ Location
  , slpCode        :: Text               -- ^ The problematic code
  , slpExplanation :: Text               -- ^ Why this is a problem
  , slpFix         :: Text               -- ^ How to fix it
  , slpSeverity    :: Severity           -- ^ How serious
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for space leak detection
data SpaceLeakConfig = SpaceLeakConfig
  { slcEnabled           :: Bool   -- ^ Enable detection
  , slcCheckLazyFolds    :: Bool   -- ^ Check for foldl
  , slcCheckLazyState    :: Bool   -- ^ Check for lazy State
  , slcCheckLazyData     :: Bool   -- ^ Check for non-strict fields
  , slcCheckLazyIO       :: Bool   -- ^ Check for lazy IO
  , slcCheckAccumulators :: Bool   -- ^ Check accumulator patterns
  , slcCheckInfinite     :: Bool   -- ^ Check infinite structure operations
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration
defaultSpaceLeakConfig :: SpaceLeakConfig
defaultSpaceLeakConfig = SpaceLeakConfig
  { slcEnabled = True
  , slcCheckLazyFolds = True
  , slcCheckLazyState = True
  , slcCheckLazyData = True
  , slcCheckLazyIO = True
  , slcCheckAccumulators = True
  , slcCheckInfinite = True
  }

--------------------------------------------------------------------------------
-- Detection
--------------------------------------------------------------------------------

-- | Detect space leak patterns in source code
detectSpaceLeaks :: SpaceLeakConfig -> FilePath -> Text -> [Diagnostic]
detectSpaceLeaks config path content
  | not (slcEnabled config) = []
  | otherwise =
    let patterns = concat
          [ if slcCheckLazyFolds config then detectLazyFolds path content else []
          , if slcCheckLazyState config then detectLazyState path content else []
          , if slcCheckLazyData config then detectLazyData path content else []
          , if slcCheckLazyIO config then detectLazyIO path content else []
          , if slcCheckAccumulators config then detectLazyAccumulators path content else []
          , if slcCheckInfinite config then detectInfiniteOps path content else []
          ]
    in map patternToDiagnostic patterns

--------------------------------------------------------------------------------
-- Lazy Fold Detection
--------------------------------------------------------------------------------

-- | Detect use of lazy foldl
detectLazyFolds :: FilePath -> Text -> [SpaceLeakPattern]
detectLazyFolds path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkLazyFoldLine path) linesWithNums

checkLazyFoldLine :: FilePath -> (Int, Text) -> [SpaceLeakPattern]
checkLazyFoldLine path (lineNum, line)
  | not (isCodeLine line) = []
  | otherwise = catMaybes
  [ -- Check for foldl without '
    if patternInCode "foldl " line && not (patternInCode "foldl'" line) &&
       not (patternInCode "foldl1" line)
    then Just SpaceLeakPattern
      { slpCategory = LazyFold
      , slpSpan = mkSpan path lineNum "foldl" line
      , slpCode = line  -- Use original line for consistent span alignment
      , slpExplanation = "foldl accumulates thunks that can cause stack overflow on large lists"
      , slpFix = "Use foldl' from Data.List for strict left fold"
      , slpSeverity = Warning
      }
    else Nothing

  , -- Check for foldr on large data
    if patternInCode "foldr " line && any (`patternInCode` line) [" sum ", " product ", " length "]
    then Just SpaceLeakPattern
      { slpCategory = LazyFold
      , slpSpan = mkSpan path lineNum "foldr" line
      , slpCode = line  -- Use original line for consistent span alignment
      , slpExplanation = "foldr with strict accumulation (sum/product/length) can cause space leaks"
      , slpFix = "Consider using foldl' or dedicated strict functions"
      , slpSeverity = Suggestion
      }
    else Nothing

  , -- Check for lazy sum/product on large input
    if (patternInCode "sum " line || patternInCode "product " line) &&
       any (`patternInCode` line) ["[1..", "[0..", "take ", " $ "]
    then Just SpaceLeakPattern
      { slpCategory = LazyFold
      , slpSpan = mkSpan path lineNum "sum" line
      , slpCode = line  -- Use original line for consistent span alignment
      , slpExplanation = "sum/product can accumulate thunks on large ranges"
      , slpFix = "Consider using foldl' (+) 0 or a streaming library"
      , slpSeverity = Suggestion
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Lazy State Monad Detection
--------------------------------------------------------------------------------

-- | Detect use of lazy State monad
detectLazyState :: FilePath -> Text -> [SpaceLeakPattern]
detectLazyState path content =
  let linesWithNums = zip [1..] (T.lines content)
      hasStrictState = "Control.Monad.State.Strict" `T.isInfixOf` content ||
                       "Control.Monad.Trans.State.Strict" `T.isInfixOf` content
  in if hasStrictState
     then []  -- Already using strict state
     else mapMaybe (checkLazyStateLine path) linesWithNums

checkLazyStateLine :: FilePath -> (Int, Text) -> Maybe SpaceLeakPattern
checkLazyStateLine path (lineNum, line)
  | not (isCodeLine line) = Nothing
  | patternInCode "import " line &&
    patternInCode "Control.Monad.State" line &&
    not (patternInCode "Strict" line) = Just SpaceLeakPattern
      { slpCategory = LazyStateMonad
      -- Use full line span for import statements since we'll replace the whole line
      , slpSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
      , slpCode = line  -- Use original line for consistent span alignment
      , slpExplanation = "Lazy State monad accumulates thunks in state updates"
      , slpFix = "Use Control.Monad.State.Strict or Control.Monad.Trans.State.Strict"
      , slpSeverity = Warning
      }
  | patternInCode "StateT " line && not (patternInCode "Strict" content) = Just SpaceLeakPattern
      { slpCategory = LazyStateMonad
      , slpSpan = mkSpan path lineNum "StateT" line
      , slpCode = line  -- Use original line for consistent span alignment
      , slpExplanation = "Default StateT is lazy and can cause space leaks"
      , slpFix = "Import from Control.Monad.Trans.State.Strict"
      , slpSeverity = Suggestion
      }
  | otherwise = Nothing
  where
    content = line  -- Context not available, use line

--------------------------------------------------------------------------------
-- Lazy Data Field Detection
--------------------------------------------------------------------------------

-- | Detect non-strict data fields
detectLazyData :: FilePath -> Text -> [SpaceLeakPattern]
detectLazyData path content =
  let hasStrictData = "{-# LANGUAGE StrictData #-}" `T.isInfixOf` content
      linesWithNums = zip [1..] (T.lines content)
  in if hasStrictData
     then []  -- StrictData enabled
     else mapMaybe (checkLazyDataLine path) linesWithNums

checkLazyDataLine :: FilePath -> (Int, Text) -> Maybe SpaceLeakPattern
checkLazyDataLine path (lineNum, line)
  | isDataField line && not (hasStrictAnnotation line) = Just SpaceLeakPattern
      { slpCategory = LazyData
      , slpSpan = mkSpan path lineNum "data" line
      , slpCode = line  -- Use original line for consistent span alignment
      , slpExplanation = "Non-strict data fields can accumulate thunks"
      , slpFix = "Add {-# LANGUAGE StrictData #-} or use bang patterns (!)"
      , slpSeverity = Suggestion
      }
  | otherwise = Nothing

isDataField :: Text -> Bool
isDataField line =
  let stripped = T.stripStart line
  in ("data " `T.isPrefixOf` stripped || "{ " `T.isPrefixOf` stripped ||
      ", " `T.isPrefixOf` stripped) &&
     " :: " `T.isInfixOf` line &&
     not ("!" `T.isInfixOf` line)

hasStrictAnnotation :: Text -> Bool
hasStrictAnnotation line = "!" `T.isInfixOf` line || "{-# UNPACK #-}" `T.isInfixOf` line

--------------------------------------------------------------------------------
-- Lazy IO Detection
--------------------------------------------------------------------------------

-- | Detect lazy IO patterns
detectLazyIO :: FilePath -> Text -> [SpaceLeakPattern]
detectLazyIO path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkLazyIOLine path) linesWithNums

checkLazyIOLine :: FilePath -> (Int, Text) -> [SpaceLeakPattern]
checkLazyIOLine path (lineNum, line)
  | not (isCodeLine line) = []
  | otherwise = catMaybes
  [ -- getContents (only unqualified calls - qualified versions like TIO.getContents are strict)
    -- Also exclude hGetContents to avoid double-matching (it contains "getContents")
    let codePart = extractCode line
    in if patternInCode "getContents" line &&
          not (patternInCode "hGetContents" line) &&
          isUnqualifiedCall "getContents" codePart
    then Just SpaceLeakPattern
      { slpCategory = LazyIO
      , slpSpan = mkSpan path lineNum "getContents" line
      , slpCode = line  -- Use original line, not stripped, for consistent span/code alignment
      , slpExplanation = "getContents uses lazy IO which can cause resource leaks"
      , slpFix = "Use strict IO or a streaming library (conduit, pipes)"
      , slpSeverity = Warning
      }
    else Nothing

  , -- hGetContents (only unqualified calls - qualified versions like TIO.hGetContents are strict)
    let codePart = extractCode line
    in if patternInCode "hGetContents" line &&
          isUnqualifiedCall "hGetContents" codePart
    then Just SpaceLeakPattern
      { slpCategory = LazyIO
      , slpSpan = mkSpan path lineNum "hGetContents" line
      , slpCode = line  -- Use original line, not stripped, for consistent span/code alignment
      , slpExplanation = "hGetContents uses lazy IO and may not close handle properly"
      , slpFix = "Use strict ByteString/Text IO or streaming"
      , slpSeverity = Warning
      }
    else Nothing

  , -- readFile (lazy version - only unqualified calls)
    let codePart = extractCode line
    in if patternInCode "readFile" line &&
          isUnqualifiedCall "readFile" codePart &&
          not (patternInCode "strict" (T.toLower line))
    then Just SpaceLeakPattern
      { slpCategory = LazyIO
      , slpSpan = mkSpan path lineNum "readFile" line
      , slpCode = line  -- Use original line, not stripped, for consistent span/code alignment
      , slpExplanation = "Prelude.readFile uses lazy IO"
      , slpFix = "Use Data.Text.IO.readFile or Data.ByteString.readFile"
      , slpSeverity = Suggestion
      }
    else Nothing

  , -- interact (only unqualified calls)
    let codePart = extractCode line
    in if patternInCode "interact" line &&
          isUnqualifiedCall "interact" codePart
    then Just SpaceLeakPattern
      { slpCategory = LazyIO
      , slpSpan = mkSpan path lineNum "interact" line
      , slpCode = line  -- Use original line, not stripped, for consistent span/code alignment
      , slpExplanation = "interact uses lazy IO"
      , slpFix = "Consider strict IO or streaming for reliability"
      , slpSeverity = Suggestion
      }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Lazy Accumulator Detection
--------------------------------------------------------------------------------

-- | Detect non-strict accumulators in recursive functions
detectLazyAccumulators :: FilePath -> Text -> [SpaceLeakPattern]
detectLazyAccumulators path content =
  let linesWithNums = zip [1..] (T.lines content)
  in mapMaybe (checkAccumulatorLine path) linesWithNums

checkAccumulatorLine :: FilePath -> (Int, Text) -> Maybe SpaceLeakPattern
checkAccumulatorLine path (lineNum, line)
  -- Pattern: recursive function with accumulator not forced
  | hasAccumulatorPattern line && not (hasStrictPattern line) = Just SpaceLeakPattern
      { slpCategory = LazyAccumulator
      , slpSpan = mkSpan path lineNum "acc" line
      , slpCode = line  -- Use original line for consistent span alignment
      , slpExplanation = "Accumulator may not be strictly evaluated, causing thunk buildup"
      , slpFix = "Use seq, ($!), or BangPatterns to force evaluation"
      , slpSeverity = Suggestion
      }
  | otherwise = Nothing

hasAccumulatorPattern :: Text -> Bool
hasAccumulatorPattern line =
  let code = extractCode line
  in any (`T.isInfixOf` code) [" acc ", " acc)", "(acc ", " n + 1", " n - 1", " count ", " total "] &&
     any (`T.isInfixOf` code) [" go ", " loop ", " helper ", " aux "]

hasStrictPattern :: Text -> Bool
hasStrictPattern line =
  let code = extractCode line
  in any (`T.isInfixOf` code) [" $! ", " `seq` ", "!acc", "!(", "{-# UNPACK"]

--------------------------------------------------------------------------------
-- Infinite Structure Operation Detection
--------------------------------------------------------------------------------

-- | Detect operations that are unsafe on infinite structures
detectInfiniteOps :: FilePath -> Text -> [SpaceLeakPattern]
detectInfiniteOps path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkInfiniteOpLine path) linesWithNums

checkInfiniteOpLine :: FilePath -> (Int, Text) -> [SpaceLeakPattern]
checkInfiniteOpLine path (lineNum, line)
  | not (isCodeLine line) = []
  | otherwise = catMaybes
  [ -- reverse on potentially infinite list
    if patternInCode "reverse " line && hasInfiniteIndicator line
    then Just SpaceLeakPattern
      { slpCategory = InfiniteStructure
      , slpSpan = mkSpan path lineNum "reverse" line
      , slpCode = line  -- Use original line for consistent span alignment
      , slpExplanation = "reverse will hang on infinite lists"
      , slpFix = "Ensure the list is finite or use a different approach"
      , slpSeverity = Warning
      }
    else Nothing

  , -- length on potentially infinite list
    if patternInCode "length " line && hasInfiniteIndicator line
    then Just SpaceLeakPattern
      { slpCategory = InfiniteStructure
      , slpSpan = mkSpan path lineNum "length" line
      , slpCode = line  -- Use original line for consistent span alignment
      , slpExplanation = "length will hang on infinite lists"
      , slpFix = "Use a bounded check or ensure finite input"
      , slpSeverity = Warning
      }
    else Nothing

  , -- last on potentially infinite list
    if patternInCode "last " line && hasInfiniteIndicator line
    then Just SpaceLeakPattern
      { slpCategory = InfiniteStructure
      , slpSpan = mkSpan path lineNum "last" line
      , slpCode = line  -- Use original line for consistent span alignment
      , slpExplanation = "last will hang on infinite lists"
      , slpFix = "Use head/take or ensure finite input"
      , slpSeverity = Warning
      }
    else Nothing
  ]

hasInfiniteIndicator :: Text -> Bool
hasInfiniteIndicator line =
  let code = extractCode line
  in any (`T.isInfixOf` code) ["[1..]", "[0..]", "repeat ", "cycle ", "iterate "]

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Create a span for a pattern match
mkSpan :: FilePath -> Int -> Text -> Text -> SrcSpan
mkSpan path lineNum needle line =
  let col = case T.breakOn needle line of
              (before, _) -> T.length before + 1
  in mkSrcSpanRaw path lineNum col lineNum (col + T.length needle)

-- | Convert pattern to diagnostic with auto-fixes
patternToDiagnostic :: SpaceLeakPattern -> Diagnostic
patternToDiagnostic SpaceLeakPattern{..} = Diagnostic
  { diagSpan = slpSpan
  , diagSeverity = slpSeverity
  , diagKind = CodePattern
  , diagMessage = slpExplanation <> ". " <> slpFix
  , diagCode = Just $ "space-leak/" <> categoryCode slpCategory
  , diagFixes = generateFixes slpCategory slpSpan slpCode
  , diagRelated = []
  }

-- | Generate auto-fixes for space leak patterns
generateFixes :: SpaceLeakCategory -> SrcSpan -> Text -> [Fix]
generateFixes category srcSpan code = case category of
  LazyFold
    | "foldl " `T.isInfixOf` code ->
        [mkSpaceLeakFix "Use strict foldl'"
          [FixEdit (adjustSpanForWord "foldl" srcSpan code) "foldl'"]
          True [mkSimpleImport "Data.List" "foldl'"]]
    | "foldr " `T.isInfixOf` code && " sum " `T.isInfixOf` code ->
        [mkSpaceLeakFix "Use foldl' for strict accumulation"
          [FixEdit (adjustSpanForWord "foldr" srcSpan code) "foldl'"]
          False [mkSimpleImport "Data.List" "foldl'"]]
    | otherwise -> []

  LazyStateMonad
    -- Handle ".Lazy" suffix variants first (more specific)
    | "Control.Monad.State.Lazy" `T.isInfixOf` code ->
        [mkSpaceLeakFix "Use Control.Monad.State.Strict"
          [FixEdit srcSpan (T.replace "Control.Monad.State.Lazy" "Control.Monad.State.Strict" code)]
          True []]
    | "Control.Monad.Trans.State.Lazy" `T.isInfixOf` code ->
        [mkSpaceLeakFix "Use Control.Monad.Trans.State.Strict"
          [FixEdit srcSpan (T.replace "Control.Monad.Trans.State.Lazy" "Control.Monad.Trans.State.Strict" code)]
          True []]
    -- Handle non-.Lazy variants
    | "Control.Monad.State " `T.isInfixOf` code &&
      not ("Strict" `T.isInfixOf` code) ->
        [mkSpaceLeakFix "Use Control.Monad.State.Strict"
          [FixEdit srcSpan (T.replace "Control.Monad.State " "Control.Monad.State.Strict " code)]
          True []]
    | "Control.Monad.Trans.State " `T.isInfixOf` code &&
      not ("Strict" `T.isInfixOf` code) ->
        [mkSpaceLeakFix "Use Control.Monad.Trans.State.Strict"
          [FixEdit srcSpan (T.replace "Control.Monad.Trans.State " "Control.Monad.Trans.State.Strict " code)]
          True []]
    -- Fallback without trailing space
    | "Control.Monad.State" `T.isInfixOf` code &&
      not ("Strict" `T.isInfixOf` code) &&
      not (".Lazy" `T.isInfixOf` code) ->
        [mkSpaceLeakFix "Use Control.Monad.State.Strict"
          [FixEdit srcSpan (T.replace "Control.Monad.State" "Control.Monad.State.Strict" code)]
          True []]
    | "Control.Monad.Trans.State" `T.isInfixOf` code &&
      not ("Strict" `T.isInfixOf` code) &&
      not (".Lazy" `T.isInfixOf` code) ->
        [mkSpaceLeakFix "Use Control.Monad.Trans.State.Strict"
          [FixEdit srcSpan (T.replace "Control.Monad.Trans.State" "Control.Monad.Trans.State.Strict" code)]
          True []]
    -- Data.Map and Data.IntMap
    | "Data.Map.Lazy" `T.isInfixOf` code ->
        [mkSpaceLeakFix "Use Data.Map.Strict"
          [FixEdit srcSpan (T.replace "Data.Map.Lazy" "Data.Map.Strict" code)]
          True []]
    | "Data.Map " `T.isInfixOf` code &&
      not ("Strict" `T.isInfixOf` code) ->
        [mkSpaceLeakFix "Use Data.Map.Strict"
          [FixEdit srcSpan (T.replace "Data.Map " "Data.Map.Strict " code)]
          True []]
    | "Data.IntMap.Lazy" `T.isInfixOf` code ->
        [mkSpaceLeakFix "Use Data.IntMap.Strict"
          [FixEdit srcSpan (T.replace "Data.IntMap.Lazy" "Data.IntMap.Strict" code)]
          True []]
    | "Data.IntMap " `T.isInfixOf` code &&
      not ("Strict" `T.isInfixOf` code) ->
        [mkSpaceLeakFix "Use Data.IntMap.Strict"
          [FixEdit srcSpan (T.replace "Data.IntMap " "Data.IntMap.Strict " code)]
          True []]
    | "Data.HashMap.Lazy" `T.isInfixOf` code ->
        [mkSpaceLeakFix "Use Data.HashMap.Strict"
          [FixEdit srcSpan (T.replace "Data.HashMap.Lazy" "Data.HashMap.Strict" code)]
          True []]
    | otherwise -> []

  LazyIO
    -- Only generate fixes for unqualified calls - qualified versions are already strict
    | "getContents" `T.isInfixOf` code &&
      not ("hGetContents" `T.isInfixOf` code) &&
      isUnqualifiedCall "getContents" code ->
        [ mkSpaceLeakFix "Use Data.Text.IO.getContents"
            [FixEdit (adjustSpanForWord "getContents" srcSpan code) "TIO.getContents"]
            True [mkQualifiedImport "Data.Text.IO" "TIO"]
        , mkSpaceLeakFix "Use Data.ByteString.getContents"
            [FixEdit (adjustSpanForWord "getContents" srcSpan code) "BS.getContents"]
            False [mkQualifiedImport "Data.ByteString" "BS"]
        ]
    | "hGetContents" `T.isInfixOf` code &&
      isUnqualifiedCall "hGetContents" code ->
        [ mkSpaceLeakFix "Use Data.Text.IO.hGetContents"
            [FixEdit (adjustSpanForWord "hGetContents" srcSpan code) "TIO.hGetContents"]
            True [mkQualifiedImport "Data.Text.IO" "TIO"]
        , mkSpaceLeakFix "Use Data.ByteString.hGetContents"
            [FixEdit (adjustSpanForWord "hGetContents" srcSpan code) "BS.hGetContents"]
            False [mkQualifiedImport "Data.ByteString" "BS"]
        ]
    | "readFile" `T.isInfixOf` code &&
      isUnqualifiedCall "readFile" code ->
        [ mkSpaceLeakFix "Use Data.Text.IO.readFile"
            [FixEdit (adjustSpanForWord "readFile" srcSpan code) "TIO.readFile"]
            True [mkQualifiedImport "Data.Text.IO" "TIO"]
        , mkSpaceLeakFix "Use Data.ByteString.readFile"
            [FixEdit (adjustSpanForWord "readFile" srcSpan code) "BS.readFile"]
            False [mkQualifiedImport "Data.ByteString" "BS"]
        ]
    | "interact" `T.isInfixOf` code &&
      isUnqualifiedCall "interact" code ->
        [ mkSpaceLeakFix "Use strict Text-based IO"
            [FixEdit (adjustSpanForWord "interact" srcSpan code) "TIO.interact"]
            False [mkQualifiedImport "Data.Text.IO" "TIO"]
        ]
    | otherwise -> []

  -- LazyData and LazyAccumulator: These require complex AST-based transformations
  -- that we cannot provide with simple text edits. The diagnostic message already
  -- explains what the user should do. Don't generate empty edits that would fail.
  LazyData -> []
  LazyAccumulator -> []
  ThunkAccumulation -> []
  InfiniteStructure -> []

-- | Helper to create a space leak fix with proper category and safety
mkSpaceLeakFix :: Text -> [FixEdit] -> Bool -> [FixImport] -> Fix
mkSpaceLeakFix title edits preferred imports = Fix
  { fixTitle = title
  , fixEdits = edits
  , fixIsPreferred = preferred
  , fixAddImports = imports
  , fixRemoveImports = []
  , fixCategory = FCSpaceLeaks
  , fixSafety = FSMostly
  }

-- | Create a simple import for a function
mkSimpleImport :: Text -> Text -> FixImport
mkSimpleImport modName symName = FixImport
  { fimpModule = modName
  , fimpSymbols = [ImportSymbol symName ISTFunction []]
  , fimpQualified = Nothing
  , fimpHiding = False
  , fimpPackage = Nothing
  }

-- | Create a qualified import
mkQualifiedImport :: Text -> Text -> FixImport
mkQualifiedImport modName alias = FixImport
  { fimpModule = modName
  , fimpSymbols = []
  , fimpQualified = Just alias
  , fimpHiding = False
  , fimpPackage = Nothing
  }

-- | Adjust span to cover just the specific word in the code
-- The span from mkSpan already points at the word; this just verifies word boundaries
-- and returns a span covering exactly the word length
adjustSpanForWord :: Text -> SrcSpan -> Text -> SrcSpan
adjustSpanForWord word srcSpan' code =
  -- Verify the word exists at word boundaries in the code
  case findStandaloneWord word code of
    Just _ ->
      -- The span already points at the word (from mkSpan), just ensure end column is correct
      let startColRaw = srcSpanStartColRaw srcSpan'
      in srcSpan'
        { srcSpanEndCol = Column (startColRaw + T.length word)
        }
    Nothing -> srcSpan'

-- | Find a standalone word (at word boundaries), not as a substring
findStandaloneWord :: Text -> Text -> Maybe Int
findStandaloneWord word code = go 0 (T.unpack code)
  where
    wordStr = T.unpack word
    wordLen = length wordStr

    go _ [] = Nothing
    go pos str@(_c:cs)
      | wordStr `isPrefixOfAt` str =
          let beforeOk = pos == 0 || not (isIdentChar (T.index code (pos - 1)))
              afterOk = pos + wordLen >= T.length code ||
                        not (isIdentChar (T.index code (pos + wordLen)))
          in if beforeOk && afterOk
             then Just pos
             else go (pos + 1) cs
      | otherwise = go (pos + 1) cs

    isPrefixOfAt :: String -> String -> Bool
    isPrefixOfAt [] _ = True
    isPrefixOfAt _ [] = False
    isPrefixOfAt (x:xs) (y:ys) = x == y && isPrefixOfAt xs ys

    isIdentChar :: Char -> Bool
    isIdentChar c = c == '_' || c == '\'' || isAlphaNum c

-- | Check if a function appears unqualified in code.
-- Returns True if the function is called without a module qualifier (e.g., @hGetContents@)
-- Returns False if the function is qualified (e.g., @TIO.hGetContents@, @Data.Text.IO.hGetContents@)
--
-- This properly handles any qualifier, not just hardcoded ones like TIO or BS.
-- A qualified call is detected by checking if the function name is preceded by a dot
-- that follows a valid module name character (uppercase letter, digit, or underscore).
isUnqualifiedCall :: Text -> Text -> Bool
isUnqualifiedCall funcName code =
  case findStandaloneWord funcName code of
    Nothing -> False  -- Function not found at all
    Just pos
      | pos == 0 -> True  -- At start of code, definitely unqualified
      | T.index code (pos - 1) /= '.' -> True  -- Not preceded by dot, unqualified
      | pos < 2 -> False  -- Preceded by dot but no room for qualifier
      | otherwise ->
          -- Preceded by dot - check if it's a qualified name (Module.func)
          -- Look for uppercase letter before the dot (module names start with uppercase)
          let charBeforeDot = T.index code (pos - 2)
          in not (isModuleChar charBeforeDot)
  where
    isModuleChar c = isAlphaNum c || c == '_' || c == '\''

categoryCode :: SpaceLeakCategory -> Text
categoryCode = \case
  LazyFold -> "lazy-fold"
  LazyAccumulator -> "lazy-accumulator"
  LazyStateMonad -> "lazy-state"
  LazyData -> "lazy-data"
  LazyIO -> "lazy-io"
  ThunkAccumulation -> "thunk-accumulation"
  InfiniteStructure -> "infinite-structure"
