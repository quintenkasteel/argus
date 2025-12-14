{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Rules.Extensions
-- Description : Language extension and pragma management
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides intelligent extension management including:
-- - Detecting missing extensions
-- - Suggesting extension additions
-- - Detecting redundant extensions
-- - Extension safety analysis
module Argus.Rules.Extensions
  ( -- * Extension detection
    detectExtensionIssues
  , ExtensionFinding (..)
  , ExtensionCategory (..)

    -- * Extension database
  , ExtensionInfo (..)
  , extensionDatabase

    -- * Configuration
  , ExtensionConfig (..)
  , defaultExtensionConfig

    -- * Utilities
  , parseExtensions
  , suggestExtension
  , generatePragmaFix
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (find)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import Argus.Types

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Category of extension finding
data ExtensionCategory
  = MissingExtension      -- ^ Extension needed but not enabled
  | RedundantExtension    -- ^ Extension enabled but not used
  | ImpliedExtension      -- ^ Extension implied by another
  | UnsafeExtension       -- ^ Potentially dangerous extension
  | DeprecatedExtension   -- ^ Extension that is deprecated
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A detected extension issue
data ExtensionFinding = ExtensionFinding
  { efCategory    :: ExtensionCategory
  , efSpan        :: SrcSpan
  , efExtension   :: Text           -- ^ Extension name
  , efExplanation :: Text           -- ^ Why this is an issue
  , efSeverity    :: Severity
  , efAutoFix     :: [Fix]          -- ^ Auto-fix if available
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Information about a language extension
data ExtensionInfo = ExtensionInfo
  { extName        :: Text           -- ^ Extension name
  , extImplies     :: [Text]         -- ^ Extensions this implies
  , extImpliedBy   :: [Text]         -- ^ Extensions that imply this
  , extSafe        :: Bool           -- ^ Is this extension safe?
  , extDeprecated  :: Bool           -- ^ Is this deprecated?
  , extReplacement :: Maybe Text     -- ^ Replacement if deprecated
  , extPatterns    :: [Text]         -- ^ Code patterns requiring this extension
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for extension checking
data ExtensionConfig = ExtensionConfig
  { ecEnabled             :: Bool    -- ^ Enable extension checks
  , ecCheckMissing        :: Bool    -- ^ Check for missing extensions
  , ecCheckRedundant      :: Bool    -- ^ Check for redundant extensions
  , ecCheckUnsafe         :: Bool    -- ^ Warn about unsafe extensions
  , ecCheckDeprecated     :: Bool    -- ^ Warn about deprecated extensions
  , ecRecommendedExtensions :: [Text] -- ^ Extensions to always recommend
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration
defaultExtensionConfig :: ExtensionConfig
defaultExtensionConfig = ExtensionConfig
  { ecEnabled = True
  , ecCheckMissing = True
  , ecCheckRedundant = True
  , ecCheckUnsafe = True
  , ecCheckDeprecated = True
  , ecRecommendedExtensions = ["StrictData", "OverloadedStrings"]
  }

--------------------------------------------------------------------------------
-- Extension Database
--------------------------------------------------------------------------------

-- | Database of known extensions and their properties
extensionDatabase :: Map Text ExtensionInfo
extensionDatabase = Map.fromList
  -- Type system extensions
  [ ("GADTs", ExtensionInfo
      { extName = "GADTs"
      , extImplies = ["GADTSyntax", "MonoLocalBinds"]
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["where", "data.*where"]
      })

  , ("TypeFamilies", ExtensionInfo
      { extName = "TypeFamilies"
      , extImplies = ["MonoLocalBinds", "KindSignatures"]
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["type family", "data family", "type instance"]
      })

  , ("DataKinds", ExtensionInfo
      { extName = "DataKinds"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = [":: '", ":: '["]
      })

  , ("TypeOperators", ExtensionInfo
      { extName = "TypeOperators"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["type.*:+:", "type.*:*:"]
      })

  , ("RankNTypes", ExtensionInfo
      { extName = "RankNTypes"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["forall.*\\..*->"]
      })

  , ("ScopedTypeVariables", ExtensionInfo
      { extName = "ScopedTypeVariables"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["forall.*\\.", ":: a ->.*a"]
      })

  , ("MultiParamTypeClasses", ExtensionInfo
      { extName = "MultiParamTypeClasses"
      , extImplies = []
      , extImpliedBy = ["FunctionalDependencies"]
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["class.*=>.*where"]
      })

  , ("FunctionalDependencies", ExtensionInfo
      { extName = "FunctionalDependencies"
      , extImplies = ["MultiParamTypeClasses"]
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["class.*|.*where"]
      })

  , ("FlexibleInstances", ExtensionInfo
      { extName = "FlexibleInstances"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = []
      })

  , ("FlexibleContexts", ExtensionInfo
      { extName = "FlexibleContexts"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = []
      })

  , ("UndecidableInstances", ExtensionInfo
      { extName = "UndecidableInstances"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = False  -- Can cause non-termination
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = []
      })

  , ("IncoherentInstances", ExtensionInfo
      { extName = "IncoherentInstances"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = False  -- Very dangerous
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = []
      })

  , ("OverlappingInstances", ExtensionInfo
      { extName = "OverlappingInstances"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = False  -- Can cause confusion
      , extDeprecated = True
      , extReplacement = Just "Use OVERLAPPING/OVERLAPPABLE pragmas"
      , extPatterns = []
      })

  -- Syntax extensions
  , ("OverloadedStrings", ExtensionInfo
      { extName = "OverloadedStrings"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = [":: Text", ":: ByteString"]
      })

  , ("OverloadedLists", ExtensionInfo
      { extName = "OverloadedLists"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = [":: Vector", ":: Set", ":: Map"]
      })

  , ("RecordWildCards", ExtensionInfo
      { extName = "RecordWildCards"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["{..}", "where .."]
      })

  , ("NamedFieldPuns", ExtensionInfo
      { extName = "NamedFieldPuns"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = []
      })

  , ("LambdaCase", ExtensionInfo
      { extName = "LambdaCase"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["\\case"]
      })

  , ("MultiWayIf", ExtensionInfo
      { extName = "MultiWayIf"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["if |"]
      })

  , ("BlockArguments", ExtensionInfo
      { extName = "BlockArguments"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = []
      })

  , ("BinaryLiterals", ExtensionInfo
      { extName = "BinaryLiterals"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["0b"]
      })

  , ("NumericUnderscores", ExtensionInfo
      { extName = "NumericUnderscores"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["[0-9]_[0-9]"]
      })

  -- Deriving extensions
  , ("DerivingStrategies", ExtensionInfo
      { extName = "DerivingStrategies"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["deriving stock", "deriving newtype", "deriving anyclass", "deriving via"]
      })

  , ("DeriveFunctor", ExtensionInfo
      { extName = "DeriveFunctor"
      , extImplies = []
      , extImpliedBy = ["DeriveTraversable"]
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["deriving.*Functor"]
      })

  , ("DeriveFoldable", ExtensionInfo
      { extName = "DeriveFoldable"
      , extImplies = []
      , extImpliedBy = ["DeriveTraversable"]
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["deriving.*Foldable"]
      })

  , ("DeriveTraversable", ExtensionInfo
      { extName = "DeriveTraversable"
      , extImplies = ["DeriveFunctor", "DeriveFoldable"]
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["deriving.*Traversable"]
      })

  , ("DeriveGeneric", ExtensionInfo
      { extName = "DeriveGeneric"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["deriving.*Generic"]
      })

  , ("DeriveAnyClass", ExtensionInfo
      { extName = "DeriveAnyClass"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["deriving anyclass"]
      })

  , ("GeneralizedNewtypeDeriving", ExtensionInfo
      { extName = "GeneralizedNewtypeDeriving"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = False  -- Can be unsafe with certain type families
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["newtype.*deriving"]
      })

  , ("DerivingVia", ExtensionInfo
      { extName = "DerivingVia"
      , extImplies = ["DerivingStrategies"]
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["deriving.*via"]
      })

  -- Strictness extensions
  , ("StrictData", ExtensionInfo
      { extName = "StrictData"
      , extImplies = []
      , extImpliedBy = ["Strict"]
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = []
      })

  , ("Strict", ExtensionInfo
      { extName = "Strict"
      , extImplies = ["StrictData"]
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = []
      })

  , ("BangPatterns", ExtensionInfo
      { extName = "BangPatterns"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["!"]
      })

  -- Template Haskell
  , ("TemplateHaskell", ExtensionInfo
      { extName = "TemplateHaskell"
      , extImplies = []
      , extImpliedBy = ["TemplateHaskellQuotes"]
      , extSafe = False  -- Can execute arbitrary code
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["$", "$(", "'", "''"]
      })

  , ("QuasiQuotes", ExtensionInfo
      { extName = "QuasiQuotes"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["\\[.*\\|"]
      })

  -- Misc
  , ("CPP", ExtensionInfo
      { extName = "CPP"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["#if", "#ifdef", "#endif", "#define"]
      })

  , ("MagicHash", ExtensionInfo
      { extName = "MagicHash"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = False  -- Low-level
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["#"]
      })

  , ("UnboxedTuples", ExtensionInfo
      { extName = "UnboxedTuples"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = False  -- Low-level
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["(#", "#)"]
      })

  , ("PatternSynonyms", ExtensionInfo
      { extName = "PatternSynonyms"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["pattern "]
      })

  , ("ViewPatterns", ExtensionInfo
      { extName = "ViewPatterns"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["->"]
      })

  , ("TypeApplications", ExtensionInfo
      { extName = "TypeApplications"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["@"]
      })

  , ("ImportQualifiedPost", ExtensionInfo
      { extName = "ImportQualifiedPost"
      , extImplies = []
      , extImpliedBy = []
      , extSafe = True
      , extDeprecated = False
      , extReplacement = Nothing
      , extPatterns = ["import.*qualified$"]
      })
  ]

--------------------------------------------------------------------------------
-- Detection
--------------------------------------------------------------------------------

-- | Detect extension-related issues in source code
detectExtensionIssues :: ExtensionConfig -> FilePath -> Text -> [Diagnostic]
detectExtensionIssues config path content
  | not (ecEnabled config) = []
  | otherwise =
    let enabledExts = parseExtensions content
        findings = concat
          [ if ecCheckMissing config then detectMissingExtensions path content enabledExts else []
          , if ecCheckRedundant config then detectRedundantExtensions path content enabledExts else []
          , if ecCheckUnsafe config then detectUnsafeExtensions path enabledExts else []
          , if ecCheckDeprecated config then detectDeprecatedExtensions path enabledExts else []
          ]
    in map findingToDiagnostic findings

-- | Parse extensions from LANGUAGE pragmas
parseExtensions :: Text -> [Text]
parseExtensions content =
  let pragmaLines = filter ("{-# LANGUAGE" `T.isInfixOf`) (T.lines content)
  in concatMap extractExtensions pragmaLines
  where
    extractExtensions line =
      let cleaned = T.replace "{-# LANGUAGE " "" $ T.replace " #-}" "" $ T.replace "," " " line
      in T.words cleaned

-- | Detect missing extensions
detectMissingExtensions :: FilePath -> Text -> [Text] -> [ExtensionFinding]
detectMissingExtensions path content enabledExts =
  let linesWithNums = zip [1..] (T.lines content)
      needed = detectNeededExtensions content
      missing = filter (`notElem` enabledExts) needed
  in mapMaybe (createMissingFinding path content linesWithNums) missing

-- | Detect which extensions are needed based on syntax
detectNeededExtensions :: Text -> [Text]
detectNeededExtensions content = catMaybes
  [ if "\\case" `T.isInfixOf` content then Just "LambdaCase" else Nothing
  , if "{..}" `T.isInfixOf` content then Just "RecordWildCards" else Nothing
  , if "deriving stock" `T.isInfixOf` content then Just "DerivingStrategies" else Nothing
  , if "deriving anyclass" `T.isInfixOf` content then Just "DeriveAnyClass" else Nothing
  , if "deriving via" `T.isInfixOf` content then Just "DerivingVia" else Nothing
  , if "deriving" `T.isInfixOf` content && "Generic" `T.isInfixOf` content then Just "DeriveGeneric" else Nothing
  , if "0b" `T.isInfixOf` content then Just "BinaryLiterals" else Nothing
  , if "import" `T.isInfixOf` content && "qualified" `T.isInfixOf` content && hasQualifiedPost content
    then Just "ImportQualifiedPost" else Nothing
  , if "pattern " `T.isInfixOf` content && not ("case" `T.isInfixOf` content) then Just "PatternSynonyms" else Nothing
  ]
  where
    hasQualifiedPost txt =
      any (\line -> "import " `T.isPrefixOf` T.stripStart line &&
                    " qualified" `T.isSuffixOf` T.stripEnd line)
          (T.lines txt)

-- | Create a finding for a missing extension
createMissingFinding :: FilePath -> Text -> [(Int, Text)] -> Text -> Maybe ExtensionFinding
createMissingFinding path _content _lines extName = Just ExtensionFinding
  { efCategory = MissingExtension
  , efSpan = mkSrcSpanRaw path 1 1 1 1
  , efExtension = extName
  , efExplanation = "Extension " <> extName <> " appears to be needed"
  , efSeverity = Warning
  , efAutoFix = [generatePragmaFix path extName]
  }

-- | Detect redundant extensions
detectRedundantExtensions :: FilePath -> Text -> [Text] -> [ExtensionFinding]
detectRedundantExtensions path content enabledExts =
  let implied = concatMap getImpliedExtensions enabledExts
      redundant = filter (`elem` implied) enabledExts
  in map (createRedundantFinding path) redundant

-- | Get extensions implied by a given extension
getImpliedExtensions :: Text -> [Text]
getImpliedExtensions extName = case Map.lookup extName extensionDatabase of
  Nothing -> []
  Just info -> extImplies info

-- | Create a finding for a redundant extension
createRedundantFinding :: FilePath -> Text -> ExtensionFinding
createRedundantFinding path extName = ExtensionFinding
  { efCategory = RedundantExtension
  , efSpan = mkSrcSpanRaw path 1 1 1 1  -- Would need to find actual pragma
  , efExtension = extName
  , efExplanation = "Extension " <> extName <> " is implied by another enabled extension"
  , efSeverity = Suggestion
  , efAutoFix = []  -- Would need to find and remove pragma
  }

-- | Detect unsafe extensions
detectUnsafeExtensions :: FilePath -> [Text] -> [ExtensionFinding]
detectUnsafeExtensions path enabledExts =
  mapMaybe checkUnsafe enabledExts
  where
    checkUnsafe extName = case Map.lookup extName extensionDatabase of
      Nothing -> Nothing
      Just info
        | not (extSafe info) -> Just ExtensionFinding
            { efCategory = UnsafeExtension
            , efSpan = mkSrcSpanRaw path 1 1 1 1
            , efExtension = extName
            , efExplanation = "Extension " <> extName <> " can have unsafe implications"
            , efSeverity = Info
            , efAutoFix = []
            }
        | otherwise -> Nothing

-- | Detect deprecated extensions
detectDeprecatedExtensions :: FilePath -> [Text] -> [ExtensionFinding]
detectDeprecatedExtensions path enabledExts =
  mapMaybe checkDeprecated enabledExts
  where
    checkDeprecated extName = case Map.lookup extName extensionDatabase of
      Nothing -> Nothing
      Just info
        | extDeprecated info -> Just ExtensionFinding
            { efCategory = DeprecatedExtension
            , efSpan = mkSrcSpanRaw path 1 1 1 1
            , efExtension = extName
            , efExplanation = "Extension " <> extName <> " is deprecated" <>
                maybe "" (". " <>) (extReplacement info)
            , efSeverity = Warning
            , efAutoFix = []
            }
        | otherwise -> Nothing

--------------------------------------------------------------------------------
-- Suggestions and Fixes
--------------------------------------------------------------------------------

-- | Suggest an extension for a given code pattern
suggestExtension :: Text -> Maybe Text
suggestExtension codePattern = case find matches (Map.elems extensionDatabase) of
  Nothing -> Nothing
  Just info -> Just (extName info)
  where
    matches info = any (`T.isInfixOf` codePattern) (extPatterns info)

-- | Generate a fix to add a pragma
generatePragmaFix :: FilePath -> Text -> Fix
generatePragmaFix path extName = Fix
  { fixTitle = "Add {-# LANGUAGE " <> extName <> " #-}"
  , fixEdits = [FixEdit
      (mkSrcSpanRaw path 1 1 1 1)
      ("{-# LANGUAGE " <> extName <> " #-}\n")]
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCStyle
  , fixSafety = FSAlways
  }

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Convert finding to diagnostic
findingToDiagnostic :: ExtensionFinding -> Diagnostic
findingToDiagnostic ExtensionFinding{..} = Diagnostic
  { diagSpan = efSpan
  , diagSeverity = efSeverity
  , diagKind = CodePattern
  , diagMessage = efExplanation
  , diagCode = Just $ "extension/" <> categoryCode efCategory
  , diagFixes = efAutoFix
  , diagRelated = []
  }

categoryCode :: ExtensionCategory -> Text
categoryCode = \case
  MissingExtension -> "missing"
  RedundantExtension -> "redundant"
  ImpliedExtension -> "implied"
  UnsafeExtension -> "unsafe"
  DeprecatedExtension -> "deprecated"
