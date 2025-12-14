{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Imports.AutoQualify
-- Description : Automatic import qualification
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides automatic import qualification functionality:
--
-- * Detect imports that should be qualified based on configuration
-- * Detect name clashes between imports and suggest qualification
-- * Apply automatic qualification with appropriate aliases
--
-- == Usage
--
-- @
-- let config = defaultAutoQualifyConfig
--       { aqcRequireQualified = Set.fromList ["Data.Map", "Data.Set"]
--       }
-- result <- autoQualifyImports config parsedModule
-- case result of
--   AutoQualifyResult modifications warnings -> ...
-- @
module Argus.Imports.AutoQualify
  ( -- * Configuration
    AutoQualifyConfig (..)
  , defaultAutoQualifyConfig
  , QualifyStrategy (..)

    -- * Analysis
  , analyzeImportClashes
  , detectRequiredQualification
  , suggestQualification

    -- * Application
  , autoQualifyImports
  , applyQualifications
  , AutoQualifyResult (..)
  , QualificationSuggestion (..)

    -- * Common Aliases
  , standardAliases
  , suggestAlias
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Argus.Imports.Manager (ParsedImport(..), qualifyImport)
import Argus.Types
  ( SrcSpan, Diagnostic(..), Severity(..), DiagnosticKind(..)
  , Fix(..), FixEdit(..), FixSafety(..), FixCategory(..), mkSrcSpanRaw
  )

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Strategy for when to qualify imports
data QualifyStrategy
  = QualifyNever           -- ^ Never auto-qualify
  | QualifyOnClash         -- ^ Only qualify when names clash
  | QualifyAlways          -- ^ Always qualify specified modules
  | QualifyPreferExplicit  -- ^ Prefer explicit imports, qualify containers
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Configuration for auto-qualification
data AutoQualifyConfig = AutoQualifyConfig
  { aqcStrategy           :: QualifyStrategy
    -- ^ Overall qualification strategy
  , aqcRequireQualified   :: Set Text
    -- ^ Modules that must always be qualified
  , aqcSuggestQualified   :: Set Text
    -- ^ Modules that should be qualified when used unqualified
  , aqcPreferredAliases   :: Map Text Text
    -- ^ Preferred aliases for modules (e.g., "Data.Map.Strict" -> "Map")
  , aqcMaxUnqualifiedExports :: Int
    -- ^ Max exports before suggesting qualification
  , aqcDetectClashes      :: Bool
    -- ^ Detect and warn about name clashes
  , aqcAutoFix            :: Bool
    -- ^ Automatically fix qualification issues
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Sensible defaults for auto-qualification
defaultAutoQualifyConfig :: AutoQualifyConfig
defaultAutoQualifyConfig = AutoQualifyConfig
  { aqcStrategy = QualifyOnClash
  , aqcRequireQualified = Set.fromList
      [ "Data.Map"
      , "Data.Map.Strict"
      , "Data.Map.Lazy"
      , "Data.Set"
      , "Data.IntMap"
      , "Data.IntMap.Strict"
      , "Data.IntSet"
      , "Data.HashMap.Strict"
      , "Data.HashSet"
      , "Data.Sequence"
      , "Data.Text"
      , "Data.Text.Lazy"
      , "Data.ByteString"
      , "Data.ByteString.Lazy"
      , "Data.Vector"
      ]
  , aqcSuggestQualified = Set.fromList
      [ "Data.List"
      , "Data.Maybe"
      , "Data.Either"
      , "Data.Foldable"
      , "Data.Traversable"
      , "Control.Monad"
      , "Control.Applicative"
      ]
  , aqcPreferredAliases = standardAliases
  , aqcMaxUnqualifiedExports = 10
  , aqcDetectClashes = True
  , aqcAutoFix = False
  }

--------------------------------------------------------------------------------
-- Standard Aliases
--------------------------------------------------------------------------------

-- | Standard aliases for common modules
standardAliases :: Map Text Text
standardAliases = Map.fromList
  [ ("Data.Map", "Map")
  , ("Data.Map.Strict", "Map")
  , ("Data.Map.Lazy", "LMap")
  , ("Data.Set", "Set")
  , ("Data.IntMap", "IntMap")
  , ("Data.IntMap.Strict", "IntMap")
  , ("Data.IntSet", "IntSet")
  , ("Data.HashMap.Strict", "HashMap")
  , ("Data.HashSet", "HashSet")
  , ("Data.Sequence", "Seq")
  , ("Data.Text", "T")
  , ("Data.Text.Lazy", "TL")
  , ("Data.Text.IO", "TIO")
  , ("Data.Text.Encoding", "TE")
  , ("Data.ByteString", "BS")
  , ("Data.ByteString.Lazy", "BL")
  , ("Data.ByteString.Char8", "BS8")
  , ("Data.ByteString.Builder", "BSB")
  , ("Data.Vector", "V")
  , ("Data.Vector.Mutable", "MV")
  , ("Data.Vector.Unboxed", "VU")
  , ("Data.Aeson", "AE")
  , ("Data.Aeson.Types", "AET")
  , ("Control.Monad.State", "State")
  , ("Control.Monad.State.Strict", "State")
  , ("Control.Monad.Reader", "Reader")
  , ("Control.Monad.Writer", "Writer")
  , ("Control.Monad.Except", "Except")
  , ("Control.Monad.Trans.State.Strict", "State")
  , ("Control.Concurrent.STM", "STM")
  , ("Control.Concurrent.MVar", "MVar")
  , ("System.FilePath", "FP")
  , ("System.Directory", "Dir")
  , ("System.Environment", "Env")
  , ("System.IO", "IO")
  , ("Text.Printf", "Printf")
  , ("Text.Read", "Read")
  , ("GHC.Generics", "G")
  ]

-- | Suggest an alias for a module
suggestAlias :: Map Text Text -> Text -> Text
suggestAlias aliases modName =
  case Map.lookup modName aliases of
    Just alias -> alias
    Nothing -> deriveAlias modName

-- | Derive an alias from module name (last component, capitalized)
deriveAlias :: Text -> Text
deriveAlias modName =
  case T.splitOn "." modName of
    [] -> modName
    parts ->
      let lastPart = last parts
          -- Handle common patterns
          alias = case lastPart of
            "Strict" -> case parts of
              xs@(_:_:_) -> last (init xs)
              _ -> lastPart
            "Lazy" -> case parts of
              xs@(_:_:_) -> last (init xs) <> "L"
              _ -> lastPart
            _ -> lastPart
      in alias

--------------------------------------------------------------------------------
-- Analysis
--------------------------------------------------------------------------------

-- | A suggestion to qualify an import
data QualificationSuggestion = QualificationSuggestion
  { qsModule      :: Text
    -- ^ Module that should be qualified
  , qsReason      :: QualificationReason
    -- ^ Why qualification is suggested
  , qsAlias       :: Maybe Text
    -- ^ Suggested alias
  , qsImport      :: ParsedImport
    -- ^ The original import
  , qsSpan        :: Maybe SrcSpan
    -- ^ Location of the import
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Reason for qualification suggestion
data QualificationReason
  = ReasonRequired          -- ^ Module in required-qualified list
  | ReasonSuggested         -- ^ Module in suggested-qualified list
  | ReasonNameClash Text    -- ^ Name clashes with another import
  | ReasonTooManyExports    -- ^ Module exports too many symbols
  | ReasonContainerType     -- ^ Container type module (Map, Set, etc.)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Result of auto-qualification analysis
data AutoQualifyResult = AutoQualifyResult
  { aqrSuggestions   :: [QualificationSuggestion]
    -- ^ List of qualification suggestions
  , aqrModifiedImports :: [ParsedImport]
    -- ^ Imports after modifications
  , aqrDiagnostics   :: [Diagnostic]
    -- ^ Warnings/errors generated
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  -- Note: No NFData instance due to Diagnostic not having one

-- | Analyze imports for name clashes
analyzeImportClashes :: [ParsedImport] -> [(ParsedImport, ParsedImport, Text)]
analyzeImportClashes imports =
  let -- Get open imports (those that could cause clashes)
      openImports = filter isOpenImport imports
      -- For each pair, check if they could clash
      pairs = [(a, b) | a <- openImports, b <- openImports, piModule a < piModule b]
  in concatMap findClashes pairs
  where
    isOpenImport imp = not (piQualified imp) && null (piExplicit imp) && not (piHiding imp)

    findClashes :: (ParsedImport, ParsedImport) -> [(ParsedImport, ParsedImport, Text)]
    findClashes (a, b) =
      -- Without actual module exports, we can only detect potential clashes
      -- based on common patterns
      let commonSymbols = findCommonSymbols (piModule a) (piModule b)
      in [(a, b, sym) | sym <- commonSymbols]

    -- Common symbols that appear in multiple modules
    findCommonSymbols :: Text -> Text -> [Text]
    findCommonSymbols modA modB
      | "Data.Map" `T.isInfixOf` modA && "Data.IntMap" `T.isInfixOf` modB =
          ["empty", "singleton", "insert", "delete", "lookup", "member", "null", "size"]
      | "Data.Set" `T.isInfixOf` modA && "Data.IntSet" `T.isInfixOf` modB =
          ["empty", "singleton", "insert", "delete", "member", "null", "size"]
      | "Data.List" `T.isInfixOf` modA && "Data.Text" `T.isInfixOf` modB =
          ["head", "tail", "length", "null", "map", "filter", "concat", "take", "drop"]
      | otherwise = []

-- | Detect imports that require qualification based on config
detectRequiredQualification :: AutoQualifyConfig -> [ParsedImport] -> [QualificationSuggestion]
detectRequiredQualification config imports =
  let required = concatMap (checkRequired config) imports
      suggested = if aqcStrategy config /= QualifyNever
                  then concatMap (checkSuggested config) imports
                  else []
      clashes = if aqcDetectClashes config
                then analyzeClashSuggestions config imports
                else []
  in required ++ suggested ++ clashes

-- | Check if an import requires qualification
checkRequired :: AutoQualifyConfig -> ParsedImport -> [QualificationSuggestion]
checkRequired config imp
  | piQualified imp = []  -- Already qualified
  | piModule imp `Set.member` aqcRequireQualified config =
      [QualificationSuggestion
        { qsModule = piModule imp
        , qsReason = ReasonRequired
        , qsAlias = Just $ suggestAlias (aqcPreferredAliases config) (piModule imp)
        , qsImport = imp
        , qsSpan = piSpan imp
        }]
  | otherwise = []

-- | Check if an import is suggested for qualification
checkSuggested :: AutoQualifyConfig -> ParsedImport -> [QualificationSuggestion]
checkSuggested config imp
  | piQualified imp = []
  | not (null (piExplicit imp)) = []  -- Explicit imports are fine
  | piModule imp `Set.member` aqcSuggestQualified config =
      [QualificationSuggestion
        { qsModule = piModule imp
        , qsReason = ReasonSuggested
        , qsAlias = Just $ suggestAlias (aqcPreferredAliases config) (piModule imp)
        , qsImport = imp
        , qsSpan = piSpan imp
        }]
  | otherwise = []

-- | Convert clashes to suggestions
analyzeClashSuggestions :: AutoQualifyConfig -> [ParsedImport] -> [QualificationSuggestion]
analyzeClashSuggestions config imports =
  let clashes = analyzeImportClashes imports
  in concatMap (clashToSuggestions config) clashes
  where
    clashToSuggestions cfg (a, b, sym) =
      [ QualificationSuggestion
          { qsModule = piModule a
          , qsReason = ReasonNameClash sym
          , qsAlias = Just $ suggestAlias (aqcPreferredAliases cfg) (piModule a)
          , qsImport = a
          , qsSpan = piSpan a
          }
      , QualificationSuggestion
          { qsModule = piModule b
          , qsReason = ReasonNameClash sym
          , qsAlias = Just $ suggestAlias (aqcPreferredAliases cfg) (piModule b)
          , qsImport = b
          , qsSpan = piSpan b
          }
      ]

-- | Suggest qualification for a single import
suggestQualification :: AutoQualifyConfig -> ParsedImport -> Maybe QualificationSuggestion
suggestQualification config imp
  | piQualified imp = Nothing
  | piModule imp `Set.member` aqcRequireQualified config =
      Just QualificationSuggestion
        { qsModule = piModule imp
        , qsReason = ReasonRequired
        , qsAlias = Just $ suggestAlias (aqcPreferredAliases config) (piModule imp)
        , qsImport = imp
        , qsSpan = piSpan imp
        }
  | piModule imp `Set.member` aqcSuggestQualified config && null (piExplicit imp) =
      Just QualificationSuggestion
        { qsModule = piModule imp
        , qsReason = ReasonSuggested
        , qsAlias = Just $ suggestAlias (aqcPreferredAliases config) (piModule imp)
        , qsImport = imp
        , qsSpan = piSpan imp
        }
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Application
--------------------------------------------------------------------------------

-- | Auto-qualify imports based on configuration
autoQualifyImports :: AutoQualifyConfig -> [ParsedImport] -> AutoQualifyResult
autoQualifyImports config imports =
  let suggestions = detectRequiredQualification config imports

      -- Apply modifications if auto-fix is enabled
      modifiedImports = if aqcAutoFix config
                        then applyQualifications suggestions imports
                        else imports

      -- Generate diagnostics
      diagnostics = map suggestionToDiagnostic suggestions
  in AutoQualifyResult
       { aqrSuggestions = suggestions
       , aqrModifiedImports = modifiedImports
       , aqrDiagnostics = diagnostics
       }

-- | Apply qualification suggestions to imports
applyQualifications :: [QualificationSuggestion] -> [ParsedImport] -> [ParsedImport]
applyQualifications suggestions imports =
  foldl applyOne imports suggestions
  where
    applyOne :: [ParsedImport] -> QualificationSuggestion -> [ParsedImport]
    applyOne imps sug = qualifyImport (qsModule sug) (qsAlias sug) imps

-- | Convert a suggestion to a diagnostic
suggestionToDiagnostic :: QualificationSuggestion -> Diagnostic
suggestionToDiagnostic QualificationSuggestion{..} = Diagnostic
  { diagSpan = case qsSpan of
      Just sp -> sp
      Nothing -> mkSrcSpanRaw "<unknown>" 1 1 1 1
  , diagSeverity = case qsReason of
      ReasonRequired -> Error
      ReasonNameClash _ -> Warning
      _ -> Suggestion
  , diagKind = ImportStyle
  , diagMessage = mkMessage qsReason
  , diagCode = Just "imports/qualify"
  , diagFixes = [mkFix qsModule qsAlias qsSpan]
  , diagRelated = []
  }
  where
    mkMessage reason = case reason of
      ReasonRequired ->
        "Module '" <> qsModule <> "' should be imported qualified"
      ReasonSuggested ->
        "Consider qualifying import of '" <> qsModule <> "'"
      ReasonNameClash sym ->
        "Symbol '" <> sym <> "' may clash; consider qualifying '" <> qsModule <> "'"
      ReasonTooManyExports ->
        "Module '" <> qsModule <> "' exports many symbols; consider explicit imports or qualification"
      ReasonContainerType ->
        "Container module '" <> qsModule <> "' should be qualified to avoid name clashes"

    mkFix :: Text -> Maybe Text -> Maybe SrcSpan -> Fix
    mkFix modName alias spanMay =
      Fix
        { fixTitle = "Qualify import of " <> modName
        , fixEdits = case spanMay of
            Just sp -> [FixEdit
              { fixEditSpan = sp
              , fixEditNewText = renderQualifiedImport modName alias
              }]
            Nothing -> []
        , fixIsPreferred = True
        , fixAddImports = []
        , fixRemoveImports = []
        , fixCategory = FCImports
        , fixSafety = FSAlways
        }

    renderQualifiedImport :: Text -> Maybe Text -> Text
    renderQualifiedImport modName aliasOpt =
      "import " <> modName <> " qualified" <>
        maybe "" (" as " <>) aliasOpt
