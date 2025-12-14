{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.AutoFix.Partial
-- Description : Automated fixes for partial functions
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides comprehensive auto-fix capabilities for partial functions,
-- replacing them with total alternatives:
--
-- * head/tail/init/last → safe alternatives with Maybe
-- * fromJust → explicit pattern matching or fromMaybe
-- * read → readMaybe
-- * !! → safe indexing
-- * maximum/minimum → safe alternatives
-- * Pattern match completeness
--
-- == Safety Features
--
-- * Suggests appropriate safe alternatives
-- * Handles context-specific replacements
-- * Preserves error handling semantics
-- * Adds necessary imports
--
-- == Usage
--
-- @
-- engine <- newPartialFixEngine defaultPartialFixConfig
-- result <- fixPartialExprs engine filePath content
-- @
module Argus.AutoFix.Partial
  ( -- * Fix Engine
    PartialFixEngine (..)
  , newPartialFixEngine
  , PartialFixConfig (..)
  , defaultPartialFixConfig

    -- * Fixing
  , fixPartialExprs
  , fixPartialFile
  , fixPartialDiagnostics
  , PartialFixResult (..)
  , PartialFixData (..)
  , PartialFix (..)
  , PartialFixCategory (..)

    -- * Individual Fix Functions
    -- ** List Partials
  , fixHeadPartial
  , fixTailPartial
  , fixInitPartial
  , fixLastPartial
  , fixIndexPartial
  , fixCyclePartial

    -- ** Foldable Partials
  , fixMaximumPartial
  , fixMinimumPartial
  , fixFoldr1Partial
  , fixFoldl1Partial

    -- ** Maybe Partials
  , fixFromJustPartial
  , fixMaybeToListPartial

    -- ** Read Partials
  , fixReadPartial
  , fixReadPartialDefault

    -- ** Error Partials
  , fixErrorPartial
  , fixUndefinedPartial

    -- ** Division Partials
  , fixDivPartial
  , fixModPartial
  , fixQuotPartial
  , fixRemPartial
  , fixDivModPartial
  , fixQuotRemPartial

    -- ** Numeric Partials
  , fixSqrtPartial
  , fixLogPartial
  , fixRecipPartial

    -- ** Tuple Partials
  , fixFstPartial
  , fixSndPartial

    -- * Validation
  , validatePartialFix
  , PartialFixValidation (..)

    -- * Statistics
  , PartialFixStats (..)
  , emptyPartialStats
  , mergePartialStats
  ) where

import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Data.Aeson (ToJSON, FromJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Regex.TDFA ((=~))

import Argus.Types
  ( SrcSpan(..)
  , Line(..)
  , Column(..)
  , Diagnostic(..)
  , Fix(..)
  , FixEdit(..)
  )

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for Partial fix engine
data PartialFixConfig = PartialFixConfig
  { pfcEnabledCategories :: [PartialFixCategory]
      -- ^ Which categories of fixes to apply
  , pfcValidateFixes     :: Bool
      -- ^ Validate fixes before applying
  , pfcMaxFixesPerFile   :: Int
      -- ^ Maximum number of fixes to apply per file
  , pfcPreferMaybe       :: Bool
      -- ^ Prefer Maybe over Either for error handling
  , pfcAddDefaultValues  :: Bool
      -- ^ Add default values where possible
  , pfcPreserveSemantics :: Bool
      -- ^ Only suggest semantically equivalent transforms
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration
defaultPartialFixConfig :: PartialFixConfig
defaultPartialFixConfig = PartialFixConfig
  { pfcEnabledCategories = [minBound .. maxBound]
  , pfcValidateFixes     = True
  , pfcMaxFixesPerFile   = 100
  , pfcPreferMaybe       = True
  , pfcAddDefaultValues  = False
  , pfcPreserveSemantics = True
  }

-- | Categories of Partial fixes
data PartialFixCategory
  = ListPartial           -- ^ head, tail, init, last, !!
  | FoldablePartial       -- ^ maximum, minimum, foldr1, foldl1
  | MaybePartial          -- ^ fromJust
  | ReadPartial           -- ^ read
  | ErrorPartial          -- ^ error, undefined
  | DivisionPartial       -- ^ div, mod, quot, rem by zero
  | NumericPartial        -- ^ sqrt negative, log non-positive
  | TuplePartial          -- ^ fst, snd on wrong types
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Engine
--------------------------------------------------------------------------------

-- | Partial fix engine with caching
data PartialFixEngine = PartialFixEngine
  { pfeConfig     :: PartialFixConfig
  , pfeCache      :: TVar (Map FilePath [PartialFix])
  , pfeStats      :: TVar PartialFixStats
  }

-- | Create a new Partial fix engine
newPartialFixEngine :: PartialFixConfig -> IO PartialFixEngine
newPartialFixEngine config = do
  cache <- newTVarIO Map.empty
  stats <- newTVarIO emptyPartialStats
  pure PartialFixEngine
    { pfeConfig = config
    , pfeCache  = cache
    , pfeStats  = stats
    }

--------------------------------------------------------------------------------
-- Fix Types
--------------------------------------------------------------------------------

-- | Result of applying Partial fixes
data PartialFixResult
  = PartialFixSuccess PartialFixData
  | PartialFixPartialResult PartialFixData [Text]  -- ^ Data and errors
  | PartialFixFailure Text                         -- ^ Error message
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Common data for successful/partial fixes
data PartialFixData = PartialFixData
  { pfdFixes      :: [PartialFix]
  , pfdStats      :: PartialFixStats
  , pfdNewContent :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A single Partial fix
data PartialFix = PartialFix
  { pfSpan        :: SrcSpan
  , pfOriginal    :: Text
  , pfReplacement :: Text
  , pfCategory    :: PartialFixCategory
  , pfRuleId      :: Text
  , pfMessage     :: Text
  , pfValidation  :: PartialFixValidation
  , pfAddImports  :: [Text]
  , pfPartialFunc :: Text     -- ^ The partial function being replaced
  , pfSafeFunc    :: Text     -- ^ The safe alternative
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Validation result for a fix
data PartialFixValidation
  = PartialValidationPassed
  | PartialValidationWarning Text
  | PartialValidationFailed Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Statistics about Partial fixes
data PartialFixStats = PartialFixStats
  { pfsListPartials      :: Int
  , pfsFoldablePartials  :: Int
  , pfsMaybePartials     :: Int
  , pfsReadPartials      :: Int
  , pfsErrorPartials     :: Int
  , pfsDivisionPartials  :: Int
  , pfsNumericPartials   :: Int
  , pfsTuplePartials     :: Int
  , pfsTotalFixes        :: Int
  , pfsValidationsPassed :: Int
  , pfsValidationsFailed :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty statistics
emptyPartialStats :: PartialFixStats
emptyPartialStats = PartialFixStats 0 0 0 0 0 0 0 0 0 0 0

-- | Merge two statistics
mergePartialStats :: PartialFixStats -> PartialFixStats -> PartialFixStats
mergePartialStats a b = PartialFixStats
  { pfsListPartials = pfsListPartials a + pfsListPartials b
  , pfsFoldablePartials = pfsFoldablePartials a + pfsFoldablePartials b
  , pfsMaybePartials = pfsMaybePartials a + pfsMaybePartials b
  , pfsReadPartials = pfsReadPartials a + pfsReadPartials b
  , pfsErrorPartials = pfsErrorPartials a + pfsErrorPartials b
  , pfsDivisionPartials = pfsDivisionPartials a + pfsDivisionPartials b
  , pfsNumericPartials = pfsNumericPartials a + pfsNumericPartials b
  , pfsTuplePartials = pfsTuplePartials a + pfsTuplePartials b
  , pfsTotalFixes = pfsTotalFixes a + pfsTotalFixes b
  , pfsValidationsPassed = pfsValidationsPassed a + pfsValidationsPassed b
  , pfsValidationsFailed = pfsValidationsFailed a + pfsValidationsFailed b
  }

--------------------------------------------------------------------------------
-- Main Fixing Functions
--------------------------------------------------------------------------------

-- | Fix Partial expressions in a file content
fixPartialExprs :: PartialFixEngine
                -> FilePath
                -> Text
                -> IO PartialFixResult
fixPartialExprs engine path content = do
  let config = pfeConfig engine
      enabledCats = pfcEnabledCategories config

  -- Find all potential fixes
  let allFixes = findAllPartialFixes path content enabledCats config

  -- Validate fixes if enabled
  validatedFixes <- if pfcValidateFixes config
    then mapM (validateAndMarkPartial content) allFixes
    else pure allFixes

  -- Filter to only valid fixes
  let goodFixes = filter isValidPartialFix validatedFixes
      limitedFixes = take (pfcMaxFixesPerFile config) goodFixes

  -- Apply fixes
  case applyPartialFixes content limitedFixes of
    Left err -> pure $ PartialFixFailure err
    Right newContent -> do
      let stats = computePartialStats validatedFixes
          errors = map getPartialValidationError $ filter (not . isValidPartialFix) validatedFixes

      -- Update engine stats
      atomically $ modifyTVar' (pfeStats engine) (mergePartialStats stats)

      let fixData = PartialFixData limitedFixes stats newContent
      if null errors
        then pure $ PartialFixSuccess fixData
        else pure $ PartialFixPartialResult (PartialFixData goodFixes stats newContent) errors

-- | Fix a file directly
fixPartialFile :: PartialFixEngine
               -> FilePath
               -> IO PartialFixResult
fixPartialFile engine path = do
  result <- try @SomeException $ do
    content <- T.pack <$> readFile path
    fixPartialExprs engine path content
  case result of
    Left e -> pure $ PartialFixFailure $ T.pack $ show e
    Right r -> pure r

-- | Fix based on existing diagnostics
fixPartialDiagnostics :: PartialFixEngine
                      -> [Diagnostic]
                      -> Text
                      -> IO PartialFixResult
fixPartialDiagnostics _engine diags content = do
  let partialDiags = filter isPartialDiagnostic diags
      fixes = catMaybes $ map partialDiagnosticToFix partialDiags

  case applyPartialFixes content fixes of
    Left err -> pure $ PartialFixFailure err
    Right newContent -> do
      let stats = computePartialStats fixes
      pure $ PartialFixSuccess $ PartialFixData fixes stats newContent

-- | Check if a diagnostic is Partial-related
isPartialDiagnostic :: Diagnostic -> Bool
isPartialDiagnostic diag = case diagCode diag of
  Just code -> any (`T.isInfixOf` code) partialKeywords
  Nothing -> False
  where
    partialKeywords = ["head", "tail", "init", "last", "fromJust", "read",
                       "maximum", "minimum", "foldr1", "foldl1", "error",
                       "undefined", "div", "mod", "quot", "rem", "sqrt", "log"]

-- | Convert a diagnostic to a Partial fix
partialDiagnosticToFix :: Diagnostic -> Maybe PartialFix
partialDiagnosticToFix diag = case diagFixes diag of
  (fix':_) -> case fixEdits fix' of
    (FixEdit span' replacement:_) ->
      Just PartialFix
        { pfSpan = span'
        , pfOriginal = ""
        , pfReplacement = replacement
        , pfCategory = inferPartialCategory (fromMaybe "" $ diagCode diag)
        , pfRuleId = fromMaybe "unknown" $ diagCode diag
        , pfMessage = diagMessage diag
        , pfValidation = PartialValidationPassed
        , pfAddImports = []
        , pfPartialFunc = extractPartialFunc (fromMaybe "" $ diagCode diag)
        , pfSafeFunc = ""
        }
    _ -> Nothing
  _ -> Nothing

-- | Infer category from rule ID
inferPartialCategory :: Text -> PartialFixCategory
inferPartialCategory ruleId
  | any (`T.isInfixOf` ruleId) ["head", "tail", "init", "last", "!!"] = ListPartial
  | any (`T.isInfixOf` ruleId) ["maximum", "minimum", "foldr1", "foldl1"] = FoldablePartial
  | "fromJust" `T.isInfixOf` ruleId = MaybePartial
  | "read" `T.isInfixOf` ruleId = ReadPartial
  | any (`T.isInfixOf` ruleId) ["error", "undefined"] = ErrorPartial
  | any (`T.isInfixOf` ruleId) ["div", "mod", "quot", "rem"] = DivisionPartial
  | any (`T.isInfixOf` ruleId) ["sqrt", "log", "recip"] = NumericPartial
  | any (`T.isInfixOf` ruleId) ["fst", "snd"] = TuplePartial
  | otherwise = ListPartial

-- | Extract partial function name from rule ID
extractPartialFunc :: Text -> Text
extractPartialFunc ruleId =
  let funcs = ["head", "tail", "init", "last", "fromJust", "read",
               "maximum", "minimum", "foldr1", "foldl1", "error",
               "undefined", "div", "mod", "quot", "rem", "sqrt", "log",
               "fst", "snd", "cycle", "!!"]
  in fromMaybe "" $ find' (`T.isInfixOf` ruleId) funcs
  where
    find' _ [] = Nothing
    find' p (x:xs) = if p x then Just x else find' p xs

--------------------------------------------------------------------------------
-- Finding Fixes
--------------------------------------------------------------------------------

-- | Find all Partial fixes in content
findAllPartialFixes :: FilePath -> Text -> [PartialFixCategory] -> PartialFixConfig -> [PartialFix]
findAllPartialFixes path content enabledCats config =
  let lineList = zip ([1..] :: [Int]) (T.lines content)
  in concatMap (findPartialFixesInLine path enabledCats config) lineList

-- | Find fixes in a single line
findPartialFixesInLine :: FilePath -> [PartialFixCategory] -> PartialFixConfig -> (Int, Text) -> [PartialFix]
findPartialFixesInLine path enabledCats config (lineNum, lineText) = catMaybes
  [ -- List partials
    guard' ListPartial $ fixHeadPartial path lineNum lineText config
  , guard' ListPartial $ fixTailPartial path lineNum lineText config
  , guard' ListPartial $ fixInitPartial path lineNum lineText config
  , guard' ListPartial $ fixLastPartial path lineNum lineText config
  , guard' ListPartial $ fixIndexPartial path lineNum lineText config
  , guard' ListPartial $ fixCyclePartial path lineNum lineText

    -- Foldable partials
  , guard' FoldablePartial $ fixMaximumPartial path lineNum lineText config
  , guard' FoldablePartial $ fixMinimumPartial path lineNum lineText config
  , guard' FoldablePartial $ fixFoldr1Partial path lineNum lineText config
  , guard' FoldablePartial $ fixFoldl1Partial path lineNum lineText config

    -- Maybe partials
  , guard' MaybePartial $ fixFromJustPartial path lineNum lineText config
  , guard' MaybePartial $ fixMaybeToListPartial path lineNum lineText

    -- Read partials
  , guard' ReadPartial $ fixReadPartial path lineNum lineText config
  , guard' ReadPartial $ fixReadPartialDefault path lineNum lineText config

    -- Error partials
  , guard' ErrorPartial $ fixErrorPartial path lineNum lineText config
  , guard' ErrorPartial $ fixUndefinedPartial path lineNum lineText config

    -- Division partials
  , guard' DivisionPartial $ fixDivPartial path lineNum lineText config
  , guard' DivisionPartial $ fixModPartial path lineNum lineText config
  , guard' DivisionPartial $ fixQuotPartial path lineNum lineText config
  , guard' DivisionPartial $ fixRemPartial path lineNum lineText config
  , guard' DivisionPartial $ fixDivModPartial path lineNum lineText config
  , guard' DivisionPartial $ fixQuotRemPartial path lineNum lineText config

    -- Numeric partials
  , guard' NumericPartial $ fixSqrtPartial path lineNum lineText config
  , guard' NumericPartial $ fixLogPartial path lineNum lineText config
  , guard' NumericPartial $ fixRecipPartial path lineNum lineText config

    -- Tuple partials
  , guard' TuplePartial $ fixFstPartial path lineNum lineText config
  , guard' TuplePartial $ fixSndPartial path lineNum lineText config
  ]
  where
    guard' cat mFix = if cat `elem` enabledCats then mFix else Nothing

--------------------------------------------------------------------------------
-- List Partial Fixes
--------------------------------------------------------------------------------

-- | Fix: head xs → headMay xs (or headDef default xs)
fixHeadPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixHeadPartial path lineNum line config =
  let pattern' = "\\bhead\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:xs:_):_) | not ("headMay" `T.isInfixOf` line) &&
                         not ("headDef" `T.isInfixOf` line) ->
         let replacement = if pfcAddDefaultValues config
                           then "headDef defaultValue " <> xs
                           else "headMay " <> xs
             safeFunc = if pfcAddDefaultValues config then "headDef" else "headMay"
         in Just $ makePartialFix path lineNum line full replacement ListPartial
                                  "head-partial" "head is partial - crashes on empty list"
                                  ["Safe"] "head" safeFunc
       _ -> Nothing

-- | Fix: tail xs → tailMay xs (or tailDef default xs)
fixTailPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixTailPartial path lineNum line config =
  let pattern' = "\\btail\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:xs:_):_) | not ("tailMay" `T.isInfixOf` line) &&
                         not ("tailDef" `T.isInfixOf` line) ->
         let replacement = if pfcAddDefaultValues config
                           then "tailDef [] " <> xs
                           else "tailMay " <> xs
             safeFunc = if pfcAddDefaultValues config then "tailDef" else "tailMay"
         in Just $ makePartialFix path lineNum line full replacement ListPartial
                                  "tail-partial" "tail is partial - crashes on empty list"
                                  ["Safe"] "tail" safeFunc
       _ -> Nothing

-- | Fix: init xs → initMay xs (or initDef default xs)
fixInitPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixInitPartial path lineNum line config =
  let pattern' = "\\binit\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:xs:_):_) | not ("initMay" `T.isInfixOf` line) &&
                         not ("initDef" `T.isInfixOf` line) ->
         let replacement = if pfcAddDefaultValues config
                           then "initDef [] " <> xs
                           else "initMay " <> xs
             safeFunc = if pfcAddDefaultValues config then "initDef" else "initMay"
         in Just $ makePartialFix path lineNum line full replacement ListPartial
                                  "init-partial" "init is partial - crashes on empty list"
                                  ["Safe"] "init" safeFunc
       _ -> Nothing

-- | Fix: last xs → lastMay xs (or lastDef default xs)
fixLastPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixLastPartial path lineNum line config =
  let pattern' = "\\blast\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:xs:_):_) | not ("lastMay" `T.isInfixOf` line) &&
                         not ("lastDef" `T.isInfixOf` line) ->
         let replacement = if pfcAddDefaultValues config
                           then "lastDef defaultValue " <> xs
                           else "lastMay " <> xs
             safeFunc = if pfcAddDefaultValues config then "lastDef" else "lastMay"
         in Just $ makePartialFix path lineNum line full replacement ListPartial
                                  "last-partial" "last is partial - crashes on empty list"
                                  ["Safe"] "last" safeFunc
       _ -> Nothing

-- | Fix: xs !! n → atMay xs n (or atDef default xs n)
fixIndexPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixIndexPartial path lineNum line config =
  let pattern' = "([a-zA-Z_][a-zA-Z0-9_']*)\\s*!!\\s*([0-9]+|[a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:xs:n:_):_) ->
         let replacement = if pfcAddDefaultValues config
                           then "atDef defaultValue " <> xs <> " " <> n
                           else "atMay " <> xs <> " " <> n
             safeFunc = if pfcAddDefaultValues config then "atDef" else "atMay"
         in Just $ makePartialFix path lineNum line full replacement ListPartial
                                  "index-partial" "!! is partial - crashes on out of bounds"
                                  ["Safe"] "!!" safeFunc
       _ -> Nothing

-- | Fix: cycle [] → warn about infinite loop
fixCyclePartial :: FilePath -> Int -> Text -> Maybe PartialFix
fixCyclePartial path lineNum line =
  let pattern' = "cycle\\s+\\[\\]" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) ->
         Just $ makePartialFix path lineNum line match
                               "error \"cycle: empty list\""
                               ListPartial
                               "cycle-empty" "cycle [] hangs forever"
                               [] "cycle" "error"
       _ -> Nothing

--------------------------------------------------------------------------------
-- Foldable Partial Fixes
--------------------------------------------------------------------------------

-- | Fix: maximum xs → maximumMay xs
fixMaximumPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixMaximumPartial path lineNum line config =
  let pattern' = "\\bmaximum\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:xs:_):_) | not ("maximumMay" `T.isInfixOf` line) &&
                         not ("maximumDef" `T.isInfixOf` line) ->
         let replacement = if pfcAddDefaultValues config
                           then "maximumDef defaultValue " <> xs
                           else "maximumMay " <> xs
             safeFunc = if pfcAddDefaultValues config then "maximumDef" else "maximumMay"
         in Just $ makePartialFix path lineNum line full replacement FoldablePartial
                                  "maximum-partial" "maximum is partial - crashes on empty"
                                  ["Safe"] "maximum" safeFunc
       _ -> Nothing

-- | Fix: minimum xs → minimumMay xs
fixMinimumPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixMinimumPartial path lineNum line config =
  let pattern' = "\\bminimum\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:xs:_):_) | not ("minimumMay" `T.isInfixOf` line) &&
                         not ("minimumDef" `T.isInfixOf` line) ->
         let replacement = if pfcAddDefaultValues config
                           then "minimumDef defaultValue " <> xs
                           else "minimumMay " <> xs
             safeFunc = if pfcAddDefaultValues config then "minimumDef" else "minimumMay"
         in Just $ makePartialFix path lineNum line full replacement FoldablePartial
                                  "minimum-partial" "minimum is partial - crashes on empty"
                                  ["Safe"] "minimum" safeFunc
       _ -> Nothing

-- | Fix: foldr1 f xs → foldr1May f xs
fixFoldr1Partial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixFoldr1Partial path lineNum line _config =
  let pattern' = "\\bfoldr1\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:f:xs:_):_) | not ("foldr1May" `T.isInfixOf` line) ->
         Just $ makePartialFix path lineNum line full
                               ("foldr1May " <> f <> " " <> xs)
                               FoldablePartial
                               "foldr1-partial" "foldr1 is partial - crashes on empty"
                               ["Safe"] "foldr1" "foldr1May"
       _ -> Nothing

-- | Fix: foldl1 f xs → foldl1May f xs
fixFoldl1Partial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixFoldl1Partial path lineNum line _config =
  let pattern' = "\\bfoldl1\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:f:xs:_):_) | not ("foldl1May" `T.isInfixOf` line) ->
         Just $ makePartialFix path lineNum line full
                               ("foldl1May " <> f <> " " <> xs)
                               FoldablePartial
                               "foldl1-partial" "foldl1 is partial - crashes on empty"
                               ["Safe"] "foldl1" "foldl1May"
       _ -> Nothing

--------------------------------------------------------------------------------
-- Maybe Partial Fixes
--------------------------------------------------------------------------------

-- | Fix: fromJust x → fromMaybe default x
fixFromJustPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixFromJustPartial path lineNum line config =
  let pattern' = "\\bfromJust\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:_):_) ->
         let replacement = if pfcAddDefaultValues config
                           then "fromMaybe defaultValue " <> x
                           else "fromMaybe (error \"fromJust Nothing\") " <> x
         in Just $ makePartialFix path lineNum line full replacement MaybePartial
                                  "fromJust-partial" "fromJust is partial - crashes on Nothing"
                                  ["Data.Maybe"] "fromJust" "fromMaybe"
       _ -> Nothing

-- | Fix: maybeToList (Just x) → [x]
fixMaybeToListPartial :: FilePath -> Int -> Text -> Maybe PartialFix
fixMaybeToListPartial path lineNum line =
  let pattern' = "maybeToList\\s+\\(Just\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:_):_) ->
         Just $ makePartialFix path lineNum line full ("[" <> x <> "]") MaybePartial
                               "maybeToList-just" "maybeToList (Just x) = [x]"
                               [] "maybeToList" "[x]"
       _ -> Nothing

--------------------------------------------------------------------------------
-- Read Partial Fixes
--------------------------------------------------------------------------------

-- | Fix: read s → readMaybe s
fixReadPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixReadPartial path lineNum line config =
  if pfcPreferMaybe config
  then
    let pattern' = "\\bread\\s+([a-zA-Z_][a-zA-Z0-9_']*|\"[^\"]*\")" :: Text
    in case (line =~ pattern' :: [[Text]]) of
         ((full:s:_):_) | not ("readMaybe" `T.isInfixOf` line) &&
                          not ("readEither" `T.isInfixOf` line) ->
           Just $ makePartialFix path lineNum line full ("readMaybe " <> s) ReadPartial
                                 "read-partial" "read is partial - crashes on parse failure"
                                 ["Text.Read"] "read" "readMaybe"
         _ -> Nothing
  else Nothing

-- | Fix: read s → readMaybe s with default
fixReadPartialDefault :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixReadPartialDefault path lineNum line config =
  if pfcAddDefaultValues config
  then
    let pattern' = "\\bread\\s+([a-zA-Z_][a-zA-Z0-9_']*|\"[^\"]*\")" :: Text
    in case (line =~ pattern' :: [[Text]]) of
         ((full:s:_):_) | not ("readMaybe" `T.isInfixOf` line) ->
           Just $ makePartialFix path lineNum line full
                                 ("fromMaybe defaultValue (readMaybe " <> s <> ")")
                                 ReadPartial
                                 "read-partial-default" "read with default value"
                                 ["Text.Read", "Data.Maybe"] "read" "readMaybe"
         _ -> Nothing
  else Nothing

--------------------------------------------------------------------------------
-- Error Partial Fixes
--------------------------------------------------------------------------------

-- | Fix: error "msg" → Left "msg" (in Either context)
fixErrorPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixErrorPartial path lineNum line _config =
  let pattern' = "\\berror\\s+\"([^\"]+)\"" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:msg:_):_) | not ("throwError" `T.isInfixOf` line) ->
         -- Only suggest if this looks like it's in an Either/Maybe context
         if "Either" `T.isInfixOf` line || "Maybe" `T.isInfixOf` line
         then Just $ makePartialFix path lineNum line full ("Left \"" <> msg <> "\"") ErrorPartial
                                    "error-either" "Consider using Either instead of error"
                                    [] "error" "Left"
         else Nothing
       _ -> Nothing

-- | Fix: undefined → provide typed hole for compiler assistance
-- Instead of error, we use a typed hole (_) which gives better compiler feedback
-- and forces the developer to implement properly
fixUndefinedPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixUndefinedPartial path lineNum line _config =
  let pattern' = "\\bundefined\\b" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) && not ("-- argus:ignore" `T.isInfixOf` T.toLower line) ->
         Just $ makePartialFix path lineNum line match
                               "_"  -- Typed hole forces implementation
                               ErrorPartial
                               "undefined-partial" "undefined crashes at runtime - use typed hole for compiler guidance"
                               [] "undefined" "_"
       _ -> Nothing

--------------------------------------------------------------------------------
-- Division Partial Fixes
--------------------------------------------------------------------------------

-- | Fix: x `div` y → safeDivMay x y
fixDivPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixDivPartial path lineNum line _config =
  let pattern' = "([a-zA-Z0-9_']+)\\s*`div`\\s*([a-zA-Z0-9_']+)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:y:_):_) ->
         Just $ makePartialFix path lineNum line full
                               ("if " <> y <> " == 0 then Nothing else Just (" <> x <> " `div` " <> y <> ")")
                               DivisionPartial
                               "div-zero" "div crashes on zero divisor"
                               [] "div" "safeDivMay"
       _ -> Nothing

-- | Fix: x `mod` y → safeModMay x y
fixModPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixModPartial path lineNum line _config =
  let pattern' = "([a-zA-Z0-9_']+)\\s*`mod`\\s*([a-zA-Z0-9_']+)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:y:_):_) ->
         Just $ makePartialFix path lineNum line full
                               ("if " <> y <> " == 0 then Nothing else Just (" <> x <> " `mod` " <> y <> ")")
                               DivisionPartial
                               "mod-zero" "mod crashes on zero divisor"
                               [] "mod" "safeModMay"
       _ -> Nothing

-- | Fix: x `quot` y → safeQuotMay x y
fixQuotPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixQuotPartial path lineNum line _config =
  let pattern' = "([a-zA-Z0-9_']+)\\s*`quot`\\s*([a-zA-Z0-9_']+)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:y:_):_) ->
         Just $ makePartialFix path lineNum line full
                               ("if " <> y <> " == 0 then Nothing else Just (" <> x <> " `quot` " <> y <> ")")
                               DivisionPartial
                               "quot-zero" "quot crashes on zero divisor"
                               [] "quot" "safeQuotMay"
       _ -> Nothing

-- | Fix: x `rem` y → safeRemMay x y
fixRemPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixRemPartial path lineNum line _config =
  let pattern' = "([a-zA-Z0-9_']+)\\s*`rem`\\s*([a-zA-Z0-9_']+)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:y:_):_) ->
         Just $ makePartialFix path lineNum line full
                               ("if " <> y <> " == 0 then Nothing else Just (" <> x <> " `rem` " <> y <> ")")
                               DivisionPartial
                               "rem-zero" "rem crashes on zero divisor"
                               [] "rem" "safeRemMay"
       _ -> Nothing

-- | Fix: divMod x y → safeDivModMay x y
fixDivModPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixDivModPartial path lineNum line _config =
  let pattern' = "divMod\\s+([a-zA-Z0-9_']+)\\s+([a-zA-Z0-9_']+)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:y:_):_) ->
         Just $ makePartialFix path lineNum line full
                               ("if " <> y <> " == 0 then Nothing else Just (divMod " <> x <> " " <> y <> ")")
                               DivisionPartial
                               "divMod-zero" "divMod crashes on zero divisor"
                               [] "divMod" "safeDivModMay"
       _ -> Nothing

-- | Fix: quotRem x y → safeQuotRemMay x y
fixQuotRemPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixQuotRemPartial path lineNum line _config =
  let pattern' = "quotRem\\s+([a-zA-Z0-9_']+)\\s+([a-zA-Z0-9_']+)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:y:_):_) ->
         Just $ makePartialFix path lineNum line full
                               ("if " <> y <> " == 0 then Nothing else Just (quotRem " <> x <> " " <> y <> ")")
                               DivisionPartial
                               "quotRem-zero" "quotRem crashes on zero divisor"
                               [] "quotRem" "safeQuotRemMay"
       _ -> Nothing

--------------------------------------------------------------------------------
-- Numeric Partial Fixes
--------------------------------------------------------------------------------

-- | Fix: sqrt x → safeSqrt x (check for negative)
fixSqrtPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixSqrtPartial path lineNum line _config =
  let pattern' = "\\bsqrt\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:_):_) ->
         Just $ makePartialFix path lineNum line full
                               ("if " <> x <> " < 0 then Nothing else Just (sqrt " <> x <> ")")
                               NumericPartial
                               "sqrt-negative" "sqrt of negative produces NaN"
                               [] "sqrt" "safeSqrt"
       _ -> Nothing

-- | Fix: log x → safeLog x (check for non-positive)
fixLogPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixLogPartial path lineNum line _config =
  let pattern' = "\\blog\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:_):_) ->
         Just $ makePartialFix path lineNum line full
                               ("if " <> x <> " <= 0 then Nothing else Just (log " <> x <> ")")
                               NumericPartial
                               "log-nonpositive" "log of non-positive produces -Infinity or NaN"
                               [] "log" "safeLog"
       _ -> Nothing

-- | Fix: recip x → safeRecip x (check for zero)
fixRecipPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixRecipPartial path lineNum line _config =
  let pattern' = "\\brecip\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:_):_) ->
         Just $ makePartialFix path lineNum line full
                               ("if " <> x <> " == 0 then Nothing else Just (recip " <> x <> ")")
                               NumericPartial
                               "recip-zero" "recip 0 produces Infinity"
                               [] "recip" "safeRecip"
       _ -> Nothing

--------------------------------------------------------------------------------
-- Tuple Partial Fixes
--------------------------------------------------------------------------------

-- | Fix: fst on non-tuple → pattern match
fixFstPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixFstPartial path lineNum line _config =
  -- Only match fst applied to something that might not be a tuple
  let pattern' = "fst\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:_):_) | not (":: (" `T.isInfixOf` line) ->
         -- Suggest explicit pattern matching
         Just $ makePartialFix path lineNum line full
                               ("case " <> x <> " of (a, _) -> a")
                               TuplePartial
                               "fst-pattern" "Consider explicit pattern match for clarity"
                               [] "fst" "pattern match"
       _ -> Nothing

-- | Fix: snd on non-tuple → pattern match
fixSndPartial :: FilePath -> Int -> Text -> PartialFixConfig -> Maybe PartialFix
fixSndPartial path lineNum line _config =
  let pattern' = "snd\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:_):_) | not (":: (" `T.isInfixOf` line) ->
         Just $ makePartialFix path lineNum line full
                               ("case " <> x <> " of (_, b) -> b")
                               TuplePartial
                               "snd-pattern" "Consider explicit pattern match for clarity"
                               [] "snd" "pattern match"
       _ -> Nothing

--------------------------------------------------------------------------------
-- Fix Helper
--------------------------------------------------------------------------------

-- | Create a PartialFix
makePartialFix :: FilePath -> Int -> Text -> Text -> Text -> PartialFixCategory
               -> Text -> Text -> [Text] -> Text -> Text -> PartialFix
makePartialFix path lineNum lineText original replacement cat ruleId msg imports partialFunc safeFunc =
  let startCol = fromMaybe 1 $ findPartialColumn original lineText
      endCol = startCol + T.length original
  in PartialFix
    { pfSpan = SrcSpan
        { srcSpanFile = path
        , srcSpanStartLine = Line lineNum
        , srcSpanStartCol = Column startCol
        , srcSpanEndLine = Line lineNum
        , srcSpanEndCol = Column endCol
        }
    , pfOriginal = original
    , pfReplacement = replacement
    , pfCategory = cat
    , pfRuleId = ruleId
    , pfMessage = msg
    , pfValidation = PartialValidationPassed
    , pfAddImports = imports
    , pfPartialFunc = partialFunc
    , pfSafeFunc = safeFunc
    }

-- | Find column where pattern starts in line
findPartialColumn :: Text -> Text -> Maybe Int
findPartialColumn pattern' lineContent = case T.breakOn pattern' lineContent of
  (before, after) | not (T.null after) -> Just $ T.length before + 1
  _ -> Nothing

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

-- | Validate a Partial fix
validatePartialFix :: Text -> PartialFix -> PartialFixValidation
validatePartialFix content fix =
  let original = pfOriginal fix
      replacement = pfReplacement fix
  in
    if T.null replacement && not (T.null original)
    then PartialValidationFailed "Replacement is empty"
    else if not (original `T.isInfixOf` content)
    then PartialValidationFailed "Original pattern not found in content"
    else if not (balancedPartialParens replacement)
    then PartialValidationWarning "Replacement has unbalanced parentheses"
    else PartialValidationPassed

-- | Check if parentheses are balanced
balancedPartialParens :: Text -> Bool
balancedPartialParens = go (0 :: Int) (0 :: Int)
  where
    go p b text
      | p < 0 || b < 0 = False
      | T.null text = p == 0 && b == 0
      | otherwise =
          let c = T.head text
              rest = T.tail text
          in case c of
               '(' -> go (p + 1) b rest
               ')' -> go (p - 1) b rest
               '[' -> go p (b + 1) rest
               ']' -> go p (b - 1) rest
               _ -> go p b rest

-- | Validate and mark a fix
validateAndMarkPartial :: Text -> PartialFix -> IO PartialFix
validateAndMarkPartial content fix = do
  let validation = validatePartialFix content fix
  pure fix { pfValidation = validation }

-- | Check if a fix passed validation
isValidPartialFix :: PartialFix -> Bool
isValidPartialFix fix = case pfValidation fix of
  PartialValidationPassed -> True
  PartialValidationWarning _ -> True
  PartialValidationFailed _ -> False

-- | Get validation error message
getPartialValidationError :: PartialFix -> Text
getPartialValidationError fix = case pfValidation fix of
  PartialValidationFailed err -> pfRuleId fix <> ": " <> err
  PartialValidationWarning warn -> pfRuleId fix <> " (warning): " <> warn
  PartialValidationPassed -> ""

--------------------------------------------------------------------------------
-- Applying Fixes
--------------------------------------------------------------------------------

-- | Apply multiple Partial fixes to content
applyPartialFixes :: Text -> [PartialFix] -> Either Text Text
applyPartialFixes content fixes =
  let sortedFixes = reverse $ sortPartialFixesByPosition fixes
  in foldl applyOnePartial (Right content) sortedFixes
  where
    applyOnePartial (Left err) _ = Left err
    applyOnePartial (Right txt) fix = applySinglePartialFix txt fix

-- | Sort fixes by their start position
sortPartialFixesByPosition :: [PartialFix] -> [PartialFix]
sortPartialFixesByPosition = foldr insertSortedPartial []
  where
    insertSortedPartial fix [] = [fix]
    insertSortedPartial fix (x:xs)
      | comparePartialPosition fix x == LT = fix : x : xs
      | otherwise = x : insertSortedPartial fix xs

    comparePartialPosition f1 f2 =
      let l1 = srcSpanStartLine (pfSpan f1)
          l2 = srcSpanStartLine (pfSpan f2)
          c1 = srcSpanStartCol (pfSpan f1)
          c2 = srcSpanStartCol (pfSpan f2)
      in case compare l1 l2 of
           EQ -> compare c1 c2
           r -> r

-- | Apply a single fix to content
applySinglePartialFix :: Text -> PartialFix -> Either Text Text
applySinglePartialFix content fix =
  let span' = pfSpan fix
      lineNum = unLine $ srcSpanStartLine span'
      startCol = unColumn $ srcSpanStartCol span'
      endCol = unColumn $ srcSpanEndCol span'
      lines' = T.lines content
  in if lineNum < 1 || lineNum > length lines'
     then Left $ "Invalid line number: " <> T.pack (show lineNum)
     else
       let targetLine = lines' !! (lineNum - 1)
           prefix = T.take (startCol - 1) targetLine
           suffix = T.drop (endCol - 1) targetLine
           newLine = prefix <> pfReplacement fix <> suffix
           newLines = take (lineNum - 1) lines' ++ [newLine] ++ drop lineNum lines'
       in Right $ T.unlines newLines

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

-- | Compute statistics from fixes
computePartialStats :: [PartialFix] -> PartialFixStats
computePartialStats fixes = foldl addPartialFixToStats emptyPartialStats fixes

-- | Add a fix to statistics
addPartialFixToStats :: PartialFixStats -> PartialFix -> PartialFixStats
addPartialFixToStats stats fix =
  let updateValidation s = case pfValidation fix of
        PartialValidationPassed -> s { pfsValidationsPassed = pfsValidationsPassed s + 1 }
        PartialValidationWarning _ -> s { pfsValidationsPassed = pfsValidationsPassed s + 1 }
        PartialValidationFailed _ -> s { pfsValidationsFailed = pfsValidationsFailed s + 1 }
      updateCategory s = case pfCategory fix of
        ListPartial -> s { pfsListPartials = pfsListPartials s + 1 }
        FoldablePartial -> s { pfsFoldablePartials = pfsFoldablePartials s + 1 }
        MaybePartial -> s { pfsMaybePartials = pfsMaybePartials s + 1 }
        ReadPartial -> s { pfsReadPartials = pfsReadPartials s + 1 }
        ErrorPartial -> s { pfsErrorPartials = pfsErrorPartials s + 1 }
        DivisionPartial -> s { pfsDivisionPartials = pfsDivisionPartials s + 1 }
        NumericPartial -> s { pfsNumericPartials = pfsNumericPartials s + 1 }
        TuplePartial -> s { pfsTuplePartials = pfsTuplePartials s + 1 }
  in updateCategory $ updateValidation $ stats { pfsTotalFixes = pfsTotalFixes stats + 1 }
