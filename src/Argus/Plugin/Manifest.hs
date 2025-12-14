{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Plugin.Manifest
-- Description : Plugin manifest format and validation
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- This module defines the manifest format for Argus plugins, including
-- metadata, dependencies, permissions, and capabilities.
module Argus.Plugin.Manifest
  ( -- * Manifest Types
    PluginManifest (..)
  , ManifestMetadata (..)
  , ManifestDependency (..)
  , ManifestCapability (..)
  , ManifestEntryPoint (..)

    -- * Version Types
  , Version (..)
  , VersionConstraint (..)
  , parseVersion
  , parseVersionConstraint
  , satisfiesConstraint

    -- * Parsing
  , parseManifest
  , parseManifestFile
  , ManifestError (..)

    -- * Validation
  , validateManifest
  , ValidationError (..)

    -- * Generation
  , generateManifest
  , manifestToToml
  , manifestToYaml

    -- * Utilities
  , defaultManifest
  , emptyManifest
  ) where

import Control.DeepSeq (NFData)
import Control.Exception (try)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as JSON
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Manifest Types
--------------------------------------------------------------------------------

-- | Plugin manifest
data PluginManifest = PluginManifest
  { pmMetadata :: ManifestMetadata
      -- ^ Plugin metadata
  , pmDependencies :: [ManifestDependency]
      -- ^ Required dependencies
  , pmCapabilities :: [ManifestCapability]
      -- ^ Provided capabilities
  , pmEntryPoints :: [ManifestEntryPoint]
      -- ^ Plugin entry points
  , pmPermissions :: Set Text
      -- ^ Required permissions
  , pmConfiguration :: Map Text ConfigSchema
      -- ^ Configuration schema
  , pmExamples :: [Example]
      -- ^ Usage examples
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Plugin metadata
data ManifestMetadata = ManifestMetadata
  { mmId :: Text
      -- ^ Unique plugin identifier
  , mmName :: Text
      -- ^ Human-readable name
  , mmVersion :: Version
      -- ^ Plugin version
  , mmDescription :: Text
      -- ^ Short description
  , mmLongDescription :: Maybe Text
      -- ^ Extended description
  , mmAuthor :: Text
      -- ^ Author name
  , mmEmail :: Maybe Text
      -- ^ Author email
  , mmHomepage :: Maybe Text
      -- ^ Project homepage
  , mmRepository :: Maybe Text
      -- ^ Source repository
  , mmLicense :: Text
      -- ^ License identifier (SPDX)
  , mmKeywords :: [Text]
      -- ^ Search keywords
  , mmCategory :: Text
      -- ^ Plugin category
  , mmMinArgusVersion :: Maybe Version
      -- ^ Minimum Argus version
  , mmMaxArgusVersion :: Maybe Version
      -- ^ Maximum Argus version
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Plugin dependency
data ManifestDependency = ManifestDependency
  { mdName :: Text
      -- ^ Dependency name
  , mdVersion :: VersionConstraint
      -- ^ Version constraint
  , mdOptional :: Bool
      -- ^ Whether dependency is optional
  , mdFeatures :: [Text]
      -- ^ Required features
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Plugin capability
data ManifestCapability = ManifestCapability
  { mcName :: Text
      -- ^ Capability name
  , mcDescription :: Text
      -- ^ Capability description
  , mcProvides :: [Text]
      -- ^ What this capability provides
  , mcRequires :: [Text]
      -- ^ What this capability requires
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Plugin entry point
data ManifestEntryPoint = ManifestEntryPoint
  { meName :: Text
      -- ^ Entry point name
  , meType :: EntryPointType
      -- ^ Entry point type
  , meModule :: Text
      -- ^ Module path
  , meFunction :: Text
      -- ^ Function name
  , meDescription :: Maybe Text
      -- ^ Description
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Entry point type
data EntryPointType
  = EPRule
      -- ^ Provides rules
  | EPAnalyzer
      -- ^ Provides analyzer
  | EPFixer
      -- ^ Provides auto-fix
  | EPFormatter
      -- ^ Provides output formatter
  | EPCommand
      -- ^ Provides CLI command
  | EPHook
      -- ^ Provides lifecycle hook
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Configuration schema
data ConfigSchema = ConfigSchema
  { csType :: ConfigType
  , csDescription :: Text
  , csDefault :: Maybe Text
  , csRequired :: Bool
  , csValidation :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Configuration type
data ConfigType
  = CTString
  | CTInt
  | CTFloat
  | CTBool
  | CTList ConfigType
  | CTMap ConfigType
  | CTEnum [Text]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Usage example
data Example = Example
  { exName :: Text
  , exDescription :: Text
  , exCode :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

--------------------------------------------------------------------------------
-- Version Types
--------------------------------------------------------------------------------

-- | Semantic version
data Version = Version
  { vMajor :: Int
  , vMinor :: Int
  , vPatch :: Int
  , vPreRelease :: Maybe Text
  , vBuild :: Maybe Text
  }
  deriving stock (Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

instance Show Version where
  show v = show (vMajor v) <> "." <> show (vMinor v) <> "." <> show (vPatch v)
        <> maybe "" (\pr -> "-" <> T.unpack pr) (vPreRelease v)
        <> maybe "" (\b -> "+" <> T.unpack b) (vBuild v)

instance Ord Version where
  compare v1 v2 = compare (vMajor v1, vMinor v1, vPatch v1)
                          (vMajor v2, vMinor v2, vPatch v2)

-- | Version constraint
data VersionConstraint
  = VCExact Version
      -- ^ Exact version match
  | VCRange Version Version
      -- ^ Version range [min, max]
  | VCMinimum Version
      -- ^ Minimum version (>=)
  | VCMaximum Version
      -- ^ Maximum version (<=)
  | VCCaret Version
      -- ^ Caret constraint (^)
  | VCTilde Version
      -- ^ Tilde constraint (~)
  | VCAny
      -- ^ Any version
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

-- | Parse a version string
parseVersion :: Text -> Either Text Version
parseVersion txt =
  case T.splitOn "." (T.takeWhile isVersionChar txt) of
    [major, minor, patch] ->
      case (readMaybe $ T.unpack major, readMaybe $ T.unpack minor, readMaybe $ T.unpack patch) of
        (Just ma, Just mi, Just pa) -> Right $ Version ma mi pa preRelease build
        _ -> Left $ "Invalid version components: " <> txt
    [major, minor] ->
      case (readMaybe $ T.unpack major, readMaybe $ T.unpack minor) of
        (Just ma, Just mi) -> Right $ Version ma mi 0 preRelease build
        _ -> Left $ "Invalid version components: " <> txt
    [major] ->
      case readMaybe $ T.unpack major of
        Just ma -> Right $ Version ma 0 0 preRelease build
        _ -> Left $ "Invalid version: " <> txt
    _ -> Left $ "Invalid version format: " <> txt
  where
    isVersionChar c = c >= '0' && c <= '9' || c == '.'
    (versionPart, rest) = T.break (\c -> c == '-' || c == '+') txt
    (preRelease, build) = case T.uncons rest of
      Just ('-', r) -> let (pr, b) = T.break (== '+') r
                       in (Just pr, if T.null b then Nothing else Just (T.drop 1 b))
      Just ('+', r) -> (Nothing, Just r)
      _ -> (Nothing, Nothing)

-- | Parse a version constraint
parseVersionConstraint :: Text -> Either Text VersionConstraint
parseVersionConstraint txt
  | T.null txt = Right VCAny
  | "*" == txt = Right VCAny
  | "^" `T.isPrefixOf` txt = VCCaret <$> parseVersion (T.drop 1 txt)
  | "~" `T.isPrefixOf` txt = VCTilde <$> parseVersion (T.drop 1 txt)
  | ">=" `T.isPrefixOf` txt = VCMinimum <$> parseVersion (T.drop 2 txt)
  | "<=" `T.isPrefixOf` txt = VCMaximum <$> parseVersion (T.drop 2 txt)
  | " - " `T.isInfixOf` txt =
      let [minV, maxV] = T.splitOn " - " txt
      in VCRange <$> parseVersion minV <*> parseVersion maxV
  | otherwise = VCExact <$> parseVersion txt

-- | Check if a version satisfies a constraint
satisfiesConstraint :: Version -> VersionConstraint -> Bool
satisfiesConstraint v = \case
  VCAny -> True
  VCExact target -> v == target
  VCMinimum minV -> v >= minV
  VCMaximum maxV -> v <= maxV
  VCRange minV maxV -> v >= minV && v <= maxV
  VCCaret target ->
    vMajor v == vMajor target &&
    (vMajor v > 0 || vMinor v == vMinor target) &&
    v >= target
  VCTilde target ->
    vMajor v == vMajor target &&
    vMinor v == vMinor target &&
    v >= target

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- | Manifest parse error
data ManifestError
  = MEFileNotFound FilePath
  | MEParseError Text
  | MEValidationError [ValidationError]
  deriving stock (Eq, Show, Generic)

-- | Parse a manifest from TOML text
parseManifest :: Text -> Either ManifestError PluginManifest
parseManifest txt = do
  -- Simple TOML-like parsing
  let lines' = filter (not . T.null . T.strip) $ T.lines txt
      sections = groupSections lines'

  metadata <- parseMetadata sections
  deps <- parseDependencies sections
  caps <- parseCapabilities sections
  entries <- parseEntryPoints sections
  perms <- parsePermissions sections

  Right $ PluginManifest
    { pmMetadata = metadata
    , pmDependencies = deps
    , pmCapabilities = caps
    , pmEntryPoints = entries
    , pmPermissions = perms
    , pmConfiguration = Map.empty
    , pmExamples = []
    }
  where
    groupSections :: [Text] -> Map Text [(Text, Text)]
    groupSections ls = go "" Map.empty ls
      where
        go _ acc [] = acc
        go section acc (l:rest)
          | "[" `T.isPrefixOf` T.strip l =
              let newSection = T.strip $ T.dropEnd 1 $ T.drop 1 $ T.strip l
              in go newSection acc rest
          | "=" `T.isInfixOf` l =
              let (key, val) = T.breakOn "=" l
                  key' = T.strip key
                  val' = T.strip $ T.drop 1 val
              in go section (Map.insertWith (++) section [(key', val')] acc) rest
          | otherwise = go section acc rest

    parseMetadata :: Map Text [(Text, Text)] -> Either ManifestError ManifestMetadata
    parseMetadata sections =
      let meta = fromMaybe [] $ Map.lookup "metadata" sections
          get k = lookup k meta
      in case (get "id", get "name", get "version", get "author", get "license") of
           (Just id', Just name, Just ver, Just author, Just lic) ->
             case parseVersion (unquote ver) of
               Right v -> Right $ ManifestMetadata
                 { mmId = unquote id'
                 , mmName = unquote name
                 , mmVersion = v
                 , mmDescription = maybe "" unquote $ get "description"
                 , mmLongDescription = unquote <$> get "long_description"
                 , mmAuthor = unquote author
                 , mmEmail = unquote <$> get "email"
                 , mmHomepage = unquote <$> get "homepage"
                 , mmRepository = unquote <$> get "repository"
                 , mmLicense = unquote lic
                 , mmKeywords = maybe [] parseList $ get "keywords"
                 , mmCategory = maybe "general" unquote $ get "category"
                 , mmMinArgusVersion = eitherToMaybe . parseVersion . unquote =<< get "min_argus_version"
                 , mmMaxArgusVersion = eitherToMaybe . parseVersion . unquote =<< get "max_argus_version"
                 }
               Left e -> Left $ MEParseError $ "Invalid version: " <> e
           _ -> Left $ MEParseError "Missing required metadata fields"

    parseDependencies :: Map Text [(Text, Text)] -> Either ManifestError [ManifestDependency]
    parseDependencies sections =
      let deps = fromMaybe [] $ Map.lookup "dependencies" sections
      in Right $ mapMaybe parseDep deps
      where
        parseDep (name, constraint) =
          case parseVersionConstraint (unquote constraint) of
            Right vc -> Just $ ManifestDependency
              { mdName = name
              , mdVersion = vc
              , mdOptional = False
              , mdFeatures = []
              }
            Left _ -> Nothing

    parseCapabilities :: Map Text [(Text, Text)] -> Either ManifestError [ManifestCapability]
    parseCapabilities sections =
      let caps = fromMaybe [] $ Map.lookup "capabilities" sections
      in Right $ map parseCap caps
      where
        parseCap (name, desc) = ManifestCapability
          { mcName = name
          , mcDescription = unquote desc
          , mcProvides = []
          , mcRequires = []
          }

    parseEntryPoints :: Map Text [(Text, Text)] -> Either ManifestError [ManifestEntryPoint]
    parseEntryPoints sections =
      let entries = fromMaybe [] $ Map.lookup "entry_points" sections
      in Right $ mapMaybe parseEntry entries
      where
        parseEntry (name, val) =
          let parts = T.splitOn ":" (unquote val)
          in case parts of
               [modName, funcName] -> Just $ ManifestEntryPoint
                 { meName = name
                 , meType = EPRule
                 , meModule = modName
                 , meFunction = funcName
                 , meDescription = Nothing
                 }
               _ -> Nothing

    parsePermissions :: Map Text [(Text, Text)] -> Either ManifestError (Set Text)
    parsePermissions sections =
      let perms = fromMaybe [] $ Map.lookup "permissions" sections
      in Right $ Set.fromList $ map (unquote . snd) perms

    parseList :: Text -> [Text]
    parseList txt =
      let stripped = T.strip $ T.dropWhile (== '[') $ T.dropWhileEnd (== ']') txt
      in map (T.strip . unquote) $ T.splitOn "," stripped

    unquote :: Text -> Text
    unquote t =
      let t' = T.strip t
      in if T.length t' >= 2 && T.head t' == '"' && T.last t' == '"'
         then T.init $ T.tail t'
         else t'

    eitherToMaybe :: Either a b -> Maybe b
    eitherToMaybe (Right x) = Just x
    eitherToMaybe (Left _) = Nothing

    (&) = flip ($)

-- | Parse a manifest file
parseManifestFile :: FilePath -> IO (Either ManifestError PluginManifest)
parseManifestFile path = do
  result <- try $ TIO.readFile path
  case result of
    Left (_ :: IOError) -> return $ Left $ MEFileNotFound path
    Right content -> return $ parseManifest content

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

-- | Validation error
data ValidationError = ValidationError
  { veField :: Text
  , veMessage :: Text
  , veSeverity :: ValidationSeverity
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Validation severity
data ValidationSeverity
  = VSError
  | VSWarning
  | VSInfo
  deriving stock (Eq, Show, Ord, Bounded, Enum, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Validate a manifest
validateManifest :: PluginManifest -> [ValidationError]
validateManifest manifest = concat
  [ validateMetadata (pmMetadata manifest)
  , validateDependencies (pmDependencies manifest)
  , validateEntryPoints (pmEntryPoints manifest)
  ]
  where
    validateMetadata :: ManifestMetadata -> [ValidationError]
    validateMetadata meta = concat
      [ [ ValidationError "id" "Plugin ID must not be empty" VSError
        | T.null (mmId meta)
        ]
      , [ ValidationError "name" "Plugin name must not be empty" VSError
        | T.null (mmName meta)
        ]
      , [ ValidationError "description" "Consider adding a description" VSWarning
        | T.null (mmDescription meta)
        ]
      , [ ValidationError "license" "License should be SPDX identifier" VSWarning
        | not $ isValidLicense (mmLicense meta)
        ]
      ]

    validateDependencies :: [ManifestDependency] -> [ValidationError]
    validateDependencies deps =
      [ ValidationError ("dependency." <> mdName dep)
                       "Dependency name should not be empty" VSError
      | dep <- deps
      , T.null (mdName dep)
      ]

    validateEntryPoints :: [ManifestEntryPoint] -> [ValidationError]
    validateEntryPoints entries = concatMap checkEntry entries
      where
        checkEntry entry = concat
          [ [ ValidationError ("entry_point." <> meName entry)
                             "Entry point name must not be empty" VSError
            | T.null (meName entry)
            ]
          , [ ValidationError ("entry_point." <> meName entry)
                             "Module name must not be empty" VSError
            | T.null (meModule entry)
            ]
          ]

    isValidLicense :: Text -> Bool
    isValidLicense lic = lic `elem` commonLicenses

    commonLicenses :: [Text]
    commonLicenses =
      [ "MIT", "Apache-2.0", "BSD-3-Clause", "BSD-2-Clause"
      , "GPL-2.0", "GPL-3.0", "LGPL-2.1", "LGPL-3.0"
      , "MPL-2.0", "ISC", "Unlicense", "WTFPL"
      ]

--------------------------------------------------------------------------------
-- Generation
--------------------------------------------------------------------------------

-- | Generate a default manifest for a plugin
generateManifest :: Text -> Text -> Text -> PluginManifest
generateManifest pluginId name author = defaultManifest
  { pmMetadata = (pmMetadata defaultManifest)
      { mmId = pluginId
      , mmName = name
      , mmAuthor = author
      }
  }

-- | Convert manifest to TOML
manifestToToml :: PluginManifest -> Text
manifestToToml manifest = T.unlines $ concat
  [ [ "[metadata]"
    , "id = " <> quote (mmId meta)
    , "name = " <> quote (mmName meta)
    , "version = " <> quote (T.pack $ show $ mmVersion meta)
    , "description = " <> quote (mmDescription meta)
    , "author = " <> quote (mmAuthor meta)
    , "license = " <> quote (mmLicense meta)
    , maybe "" (\e -> "email = " <> quote e) (mmEmail meta)
    , maybe "" (\h -> "homepage = " <> quote h) (mmHomepage meta)
    , "category = " <> quote (mmCategory meta)
    , "keywords = [" <> T.intercalate ", " (map quote $ mmKeywords meta) <> "]"
    , ""
    , "[dependencies]"
    ]
  , [ mdName dep <> " = " <> quote (showConstraint $ mdVersion dep)
    | dep <- pmDependencies manifest
    ]
  , [ ""
    , "[entry_points]"
    ]
  , [ meName ep <> " = " <> quote (meModule ep <> ":" <> meFunction ep)
    | ep <- pmEntryPoints manifest
    ]
  , [ ""
    , "[permissions]"
    ]
  , [ "permission_" <> T.pack (show i) <> " = " <> quote p
    | (i, p) <- zip [(1::Int)..] (Set.toList $ pmPermissions manifest)
    ]
  ]
  where
    meta = pmMetadata manifest
    quote t = "\"" <> t <> "\""

    showConstraint :: VersionConstraint -> Text
    showConstraint = \case
      VCAny -> "*"
      VCExact v -> T.pack $ show v
      VCMinimum v -> ">=" <> T.pack (show v)
      VCMaximum v -> "<=" <> T.pack (show v)
      VCRange min' max' -> T.pack (show min') <> " - " <> T.pack (show max')
      VCCaret v -> "^" <> T.pack (show v)
      VCTilde v -> "~" <> T.pack (show v)

-- | Convert manifest to YAML
manifestToYaml :: PluginManifest -> Text
manifestToYaml manifest = T.unlines $ concat
  [ [ "metadata:"
    , "  id: " <> mmId meta
    , "  name: " <> mmName meta
    , "  version: " <> T.pack (show $ mmVersion meta)
    , "  description: " <> mmDescription meta
    , "  author: " <> mmAuthor meta
    , "  license: " <> mmLicense meta
    , ""
    , "dependencies:"
    ]
  , [ "  - name: " <> mdName dep <> "\n    version: " <> showConstraint (mdVersion dep)
    | dep <- pmDependencies manifest
    ]
  , [ ""
    , "entry_points:"
    ]
  , [ "  - name: " <> meName ep <> "\n    module: " <> meModule ep <> "\n    function: " <> meFunction ep
    | ep <- pmEntryPoints manifest
    ]
  ]
  where
    meta = pmMetadata manifest

    showConstraint :: VersionConstraint -> Text
    showConstraint = \case
      VCAny -> "*"
      VCExact v -> T.pack $ show v
      VCMinimum v -> ">=" <> T.pack (show v)
      VCMaximum v -> "<=" <> T.pack (show v)
      VCRange min' max' -> T.pack (show min') <> " - " <> T.pack (show max')
      VCCaret v -> "^" <> T.pack (show v)
      VCTilde v -> "~" <> T.pack (show v)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Default manifest
defaultManifest :: PluginManifest
defaultManifest = PluginManifest
  { pmMetadata = ManifestMetadata
      { mmId = "my-plugin"
      , mmName = "My Plugin"
      , mmVersion = Version 0 1 0 Nothing Nothing
      , mmDescription = "A plugin for Argus"
      , mmLongDescription = Nothing
      , mmAuthor = "Unknown"
      , mmEmail = Nothing
      , mmHomepage = Nothing
      , mmRepository = Nothing
      , mmLicense = "MIT"
      , mmKeywords = []
      , mmCategory = "general"
      , mmMinArgusVersion = Nothing
      , mmMaxArgusVersion = Nothing
      }
  , pmDependencies = []
  , pmCapabilities = []
  , pmEntryPoints = []
  , pmPermissions = Set.empty
  , pmConfiguration = Map.empty
  , pmExamples = []
  }

-- | Empty manifest
emptyManifest :: PluginManifest
emptyManifest = defaultManifest
