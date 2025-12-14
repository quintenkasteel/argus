{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Rules.Pack
-- Description : Rule pack system for grouping related rules
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides a system for organizing rules into named "packs" that can
-- be enabled or disabled as a group. Packs support:
--
-- * Named collections of rules (e.g., "security", "performance", "style")
-- * Hierarchical pack composition (packs can include other packs)
-- * Rule inheritance and overrides
-- * Priority-based conflict resolution
-- * Built-in packs and user-defined custom packs
--
-- == Usage
--
-- @
-- -- Get all rules for enabled packs
-- let config = defaultPackConfig { pcEnabledPacks = ["security", "performance"] }
-- rules <- getPackRules config
--
-- -- Create a custom pack
-- let myPack = RulePack
--       { rpName = "my-team"
--       , rpDescription = "Team-specific rules"
--       , rpRuleIds = ["use-strict-text", "avoid-head"]
--       , rpIncludes = ["security"]  -- Include all security rules too
--       , rpExcludes = []
--       , rpPriority = 100
--       }
-- @
module Argus.Rules.Pack
  ( -- * Configuration
    PackConfig (..)
  , defaultPackConfig

    -- * Rule Pack Types
  , RulePack (..)
  , PackId
  , PackPriority
  , PackResolution (..)

    -- * Built-in Packs
  , builtinPacks
  , securityPack
  , performancePack
  , stylePack
  , correctnessPack
  , modernizePack
  , strictPack
  , minimalPack
  , allRulesPack

    -- * Pack Operations
  , getPackRules
  , resolvePacks
  , expandPack
  , mergePackRules

    -- * Pack Management
  , registerPack
  , unregisterPack
  , getPackByName
  , listPacks
  , validatePack

    -- * Pack Registry
  , PackRegistry
  , newPackRegistry
  , defaultPackRegistry

    -- * Query Functions
  , isRuleInPack
  , getRulesByPack
  , getPacksByRule
  , getPackInfo

    -- * Pack Profiles
  , PackProfile (..)
  , applyProfile
  , defaultProfile
  , strictProfile
  , ciProfile

    -- * Pack Versioning
  , PackVersion (..)
  , PackManifest (..)
  , VersionedPack (..)
  , VersionBump (..)
  , parseVersion
  , formatVersion
  , compareVersions
  , bumpVersion

    -- * Pack Import/Export
  , exportPack
  , importPack
  , exportPackToFile
  , importPackFromFile
  , serializePack
  , deserializePack
  , createVersionedPack
  ) where

import Control.Concurrent.STM
import Control.DeepSeq (NFData)
import Control.Exception (try, SomeException)
import Data.Aeson (ToJSON(..), FromJSON(..), encode, eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing, Down(..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Configuration Types
--------------------------------------------------------------------------------

-- | Pack identifier
type PackId = Text

-- | Pack priority (higher = more important)
type PackPriority = Int

-- | Configuration for the pack system
data PackConfig = PackConfig
  { pcEnabledPacks    :: [PackId]     -- ^ Packs to enable
  , pcDisabledPacks   :: [PackId]     -- ^ Packs to explicitly disable
  , pcEnabledRules    :: [Text]       -- ^ Individual rules to enable (override)
  , pcDisabledRules   :: [Text]       -- ^ Individual rules to disable (override)
  , pcCustomPacks     :: [RulePack]   -- ^ User-defined custom packs
  , pcAllowDuplicates :: Bool         -- ^ Allow same rule from multiple packs
  , pcDefaultEnabled  :: Bool         -- ^ Enable all packs by default
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default pack configuration
defaultPackConfig :: PackConfig
defaultPackConfig = PackConfig
  { pcEnabledPacks = ["default"]
  , pcDisabledPacks = []
  , pcEnabledRules = []
  , pcDisabledRules = []
  , pcCustomPacks = []
  , pcAllowDuplicates = True
  , pcDefaultEnabled = False
  }

--------------------------------------------------------------------------------
-- Rule Pack Types
--------------------------------------------------------------------------------

-- | A named collection of rules
data RulePack = RulePack
  { rpName        :: PackId           -- ^ Unique pack identifier
  , rpDescription :: Text             -- ^ Human-readable description
  , rpRuleIds     :: [Text]           -- ^ Rules directly included in this pack
  , rpIncludes    :: [PackId]         -- ^ Other packs to include (inheritance)
  , rpExcludes    :: [Text]           -- ^ Rules to exclude (even if inherited)
  , rpPriority    :: PackPriority     -- ^ Priority for conflict resolution
  , rpCategory    :: Text             -- ^ Category (e.g., "safety", "style")
  , rpTags        :: [Text]           -- ^ Searchable tags
  , rpEnabled     :: Bool             -- ^ Whether pack is enabled by default
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Result of resolving pack dependencies
data PackResolution = PackResolution
  { prEnabledRules   :: Set Text      -- ^ All enabled rules
  , prDisabledRules  :: Set Text      -- ^ Explicitly disabled rules
  , prActivePacks    :: [PackId]      -- ^ Packs that contributed rules
  , prRuleSource     :: Map Text PackId -- ^ Which pack each rule came from
  , prWarnings       :: [Text]        -- ^ Resolution warnings
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

--------------------------------------------------------------------------------
-- Built-in Packs
--------------------------------------------------------------------------------

-- | All built-in rule packs
builtinPacks :: [RulePack]
builtinPacks =
  [ securityPack
  , performancePack
  , stylePack
  , correctnessPack
  , modernizePack
  , strictPack
  , minimalPack
  , allRulesPack
  , defaultPack
  ]

-- | Default pack - recommended rules for most projects
defaultPack :: RulePack
defaultPack = RulePack
  { rpName = "default"
  , rpDescription = "Recommended default rules for most Haskell projects"
  , rpRuleIds = []  -- Includes from other packs
  , rpIncludes = ["correctness", "style-essential"]
  , rpExcludes = []
  , rpPriority = 50
  , rpCategory = "general"
  , rpTags = ["default", "recommended"]
  , rpEnabled = True
  }

-- | Security-focused rules pack
securityPack :: RulePack
securityPack = RulePack
  { rpName = "security"
  , rpDescription = "Security-focused rules detecting vulnerabilities and unsafe patterns"
  , rpRuleIds =
      [ "avoid-unsafe-perform-io"
      , "avoid-unsafe-coerce"
      , "avoid-unsafeInterleaveIO"
      , "avoid-unsafe-ffi"
      , "avoid-shell-exec"
      , "validate-user-input"
      , "avoid-hardcoded-secrets"
      , "secure-random"
      , "sql-injection"
      , "command-injection"
      , "path-traversal"
      , "xss-prevention"
      , "crypto-weak-hash"
      , "crypto-weak-cipher"
      , "tls-certificate-validation"
      , "avoid-eval"
      , "safe-deserialization"
      ]
  , rpIncludes = []
  , rpExcludes = []
  , rpPriority = 100  -- High priority - security rules take precedence
  , rpCategory = "safety"
  , rpTags = ["security", "owasp", "vulnerability", "safe"]
  , rpEnabled = True
  }

-- | Performance optimization rules pack
performancePack :: RulePack
performancePack = RulePack
  { rpName = "performance"
  , rpDescription = "Rules for detecting performance issues and optimization opportunities"
  , rpRuleIds =
      [ "use-strict-text"
      , "use-strict-bytestring"
      , "avoid-string-concat"
      , "use-text-builder"
      , "use-foldl-strict"
      , "avoid-lazy-io"
      , "use-seq"
      , "use-bang-patterns"
      , "avoid-nub"
      , "use-hashmap"
      , "use-vector"
      , "avoid-repeated-lookup"
      , "use-strict-state"
      , "avoid-space-leaks"
      , "use-bytestring-builder"
      , "inline-small-functions"
      , "specialize-polymorphic"
      , "avoid-unnecessary-allocation"
      ]
  , rpIncludes = []
  , rpExcludes = []
  , rpPriority = 80
  , rpCategory = "optimization"
  , rpTags = ["performance", "speed", "memory", "optimization"]
  , rpEnabled = True
  }

-- | Style and consistency rules pack
stylePack :: RulePack
stylePack = RulePack
  { rpName = "style"
  , rpDescription = "Code style and consistency rules"
  , rpRuleIds =
      [ "consistent-naming"
      , "type-signature-required"
      , "use-qualified-imports"
      , "import-order"
      , "export-list"
      , "module-documentation"
      , "function-documentation"
      , "use-where-clause"
      , "use-let-in"
      , "avoid-long-lines"
      , "avoid-trailing-whitespace"
      , "consistent-indentation"
      , "record-syntax"
      , "deriving-order"
      , "pragma-order"
      , "use-newtype"
      , "explicit-forall"
      ]
  , rpIncludes = []
  , rpExcludes = []
  , rpPriority = 30
  , rpCategory = "style"
  , rpTags = ["style", "formatting", "consistency", "readability"]
  , rpEnabled = False  -- Style rules are optional
  }

-- | Correctness rules pack (catches bugs)
correctnessPack :: RulePack
correctnessPack = RulePack
  { rpName = "correctness"
  , rpDescription = "Rules that catch potential bugs and correctness issues"
  , rpRuleIds =
      [ "avoid-head"
      , "avoid-tail"
      , "avoid-partial"
      , "avoid-fromJust"
      , "avoid-error"
      , "avoid-undefined"
      , "total-pattern-match"
      , "exhaustive-guards"
      , "unused-bindings"
      , "unused-imports"
      , "type-defaults"
      , "monomorphism-restriction"
      , "infinite-recursion"
      , "dead-code"
      , "unreachable-code"
      , "null-check"
      , "division-by-zero"
      , "array-bounds"
      ]
  , rpIncludes = []
  , rpExcludes = []
  , rpPriority = 90
  , rpCategory = "correctness"
  , rpTags = ["bugs", "correctness", "partial", "safety"]
  , rpEnabled = True
  }

-- | Modernization rules pack
modernizePack :: RulePack
modernizePack = RulePack
  { rpName = "modernize"
  , rpDescription = "Rules suggesting modern Haskell idioms and patterns"
  , rpRuleIds =
      [ "use-applicative"
      , "use-traverse"
      , "use-foldMap"
      , "use-coerce"
      , "use-deriving-via"
      , "use-deriving-strategies"
      , "use-record-wildcards"
      , "use-lambda-case"
      , "use-multi-way-if"
      , "use-type-applications"
      , "use-named-field-puns"
      , "use-overloaded-strings"
      , "use-overloaded-lists"
      , "use-block-arguments"
      , "use-pattern-synonyms"
      , "use-view-patterns"
      ]
  , rpIncludes = []
  , rpExcludes = []
  , rpPriority = 40
  , rpCategory = "modernize"
  , rpTags = ["modern", "idioms", "extensions", "upgrade"]
  , rpEnabled = False
  }

-- | Strict rules pack (maximum strictness)
strictPack :: RulePack
strictPack = RulePack
  { rpName = "strict"
  , rpDescription = "All rules enabled with strict settings"
  , rpRuleIds = []
  , rpIncludes = ["security", "performance", "correctness", "style", "modernize"]
  , rpExcludes = []
  , rpPriority = 10
  , rpCategory = "meta"
  , rpTags = ["strict", "all", "comprehensive"]
  , rpEnabled = False
  }

-- | Minimal rules pack (essential only)
minimalPack :: RulePack
minimalPack = RulePack
  { rpName = "minimal"
  , rpDescription = "Minimal set of essential rules"
  , rpRuleIds =
      [ "avoid-head"
      , "avoid-tail"
      , "avoid-fromJust"
      , "avoid-undefined"
      , "unused-imports"
      ]
  , rpIncludes = []
  , rpExcludes = []
  , rpPriority = 60
  , rpCategory = "meta"
  , rpTags = ["minimal", "essential", "basic"]
  , rpEnabled = False
  }

-- | All rules pack (everything)
allRulesPack :: RulePack
allRulesPack = RulePack
  { rpName = "all"
  , rpDescription = "Enable all available rules"
  , rpRuleIds = []
  , rpIncludes = ["security", "performance", "correctness", "style", "modernize"]
  , rpExcludes = []
  , rpPriority = 1
  , rpCategory = "meta"
  , rpTags = ["all", "complete"]
  , rpEnabled = False
  }

-- | Essential style rules (subset of style pack)
styleEssentialPack :: RulePack
styleEssentialPack = RulePack
  { rpName = "style-essential"
  , rpDescription = "Essential style rules without being too strict"
  , rpRuleIds =
      [ "consistent-naming"
      , "import-order"
      , "use-qualified-imports"
      ]
  , rpIncludes = []
  , rpExcludes = []
  , rpPriority = 35
  , rpCategory = "style"
  , rpTags = ["style", "essential"]
  , rpEnabled = True
  }

--------------------------------------------------------------------------------
-- Pack Registry
--------------------------------------------------------------------------------

-- | Registry of all available packs
data PackRegistry = PackRegistry
  { prBuiltin :: Map PackId RulePack   -- ^ Built-in packs
  , prCustom  :: TVar (Map PackId RulePack)  -- ^ User-defined packs
  }

-- | Create a new pack registry
newPackRegistry :: IO PackRegistry
newPackRegistry = do
  customVar <- newTVarIO Map.empty
  pure PackRegistry
    { prBuiltin = Map.fromList [(rpName p, p) | p <- builtinPacks ++ [styleEssentialPack]]
    , prCustom = customVar
    }

-- | Get default pack registry with all built-in packs
defaultPackRegistry :: IO PackRegistry
defaultPackRegistry = newPackRegistry

-- | Register a custom pack
registerPack :: PackRegistry -> RulePack -> IO ()
registerPack PackRegistry{..} pack =
  atomically $ modifyTVar' prCustom $ Map.insert (rpName pack) pack

-- | Unregister a custom pack
unregisterPack :: PackRegistry -> PackId -> IO ()
unregisterPack PackRegistry{..} packId =
  atomically $ modifyTVar' prCustom $ Map.delete packId

-- | Get a pack by name
getPackByName :: PackRegistry -> PackId -> IO (Maybe RulePack)
getPackByName PackRegistry{..} packId = do
  custom <- readTVarIO prCustom
  pure $ Map.lookup packId prBuiltin <|> Map.lookup packId custom
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Nothing <|> y = y
    x <|> _ = x

-- | List all available packs
listPacks :: PackRegistry -> IO [RulePack]
listPacks PackRegistry{..} = do
  custom <- readTVarIO prCustom
  pure $ Map.elems prBuiltin ++ Map.elems custom

-- | Validate a pack definition
validatePack :: PackRegistry -> RulePack -> IO [Text]
validatePack registry pack = do
  allPacks <- listPacks registry
  let packNames = Set.fromList $ map rpName allPacks
      -- Check for undefined includes
      undefinedIncludes = filter (`Set.notMember` packNames) (rpIncludes pack)
      includeWarnings = ["Unknown pack in includes: " <> p | p <- undefinedIncludes]
      -- Check for circular includes (simple check)
      selfInclude = rpName pack `elem` rpIncludes pack
      selfWarning = ["Pack includes itself: " <> rpName pack | selfInclude]
      -- Check for empty pack
      emptyWarning = ["Pack has no rules and no includes" |
                      null (rpRuleIds pack) && null (rpIncludes pack)]
  pure $ includeWarnings ++ selfWarning ++ emptyWarning

--------------------------------------------------------------------------------
-- Pack Operations
--------------------------------------------------------------------------------

-- | Get all rules for enabled packs
getPackRules :: PackRegistry -> PackConfig -> IO (Set Text)
getPackRules registry config = do
  resolution <- resolvePacks registry config
  pure $ prEnabledRules resolution

-- | Resolve pack dependencies and compute final rule set
resolvePacks :: PackRegistry -> PackConfig -> IO PackResolution
resolvePacks registry PackConfig{..} = do
  allPacks <- listPacks registry

  -- Build pack lookup
  let packMap = Map.fromList [(rpName p, p) | p <- allPacks ++ pcCustomPacks]

  -- Determine which packs are enabled
  let enabledPackIds = if pcDefaultEnabled
        then Set.toList $ Set.fromList (map rpName allPacks) `Set.difference`
                          Set.fromList pcDisabledPacks
        else pcEnabledPacks

  -- Expand all enabled packs (resolving includes)
  let (rules, sources, warnings) = foldl' (expandAndCollect packMap Set.empty)
                                          (Set.empty, Map.empty, [])
                                          enabledPackIds

  -- Apply explicit enables and disables
  let enabledRules = Set.union rules (Set.fromList pcEnabledRules)
      disabledRules = Set.fromList pcDisabledRules
      finalRules = enabledRules `Set.difference` disabledRules

  pure PackResolution
    { prEnabledRules = finalRules
    , prDisabledRules = disabledRules
    , prActivePacks = enabledPackIds
    , prRuleSource = sources
    , prWarnings = warnings
    }
  where
    expandAndCollect
      :: Map PackId RulePack
      -> Set PackId  -- Already visited (for cycle detection)
      -> (Set Text, Map Text PackId, [Text])
      -> PackId
      -> (Set Text, Map Text PackId, [Text])
    expandAndCollect packMap visited (rules, sources, warnings) packId
      | packId `Set.member` visited =
          (rules, sources, ("Circular pack dependency: " <> packId) : warnings)
      | otherwise =
          case Map.lookup packId packMap of
            Nothing ->
              (rules, sources, ("Unknown pack: " <> packId) : warnings)
            Just pack ->
              let visited' = Set.insert packId visited
                  -- First expand includes
                  (rulesFromIncludes, sourcesFromIncludes, warningsFromIncludes) =
                    foldl' (expandAndCollect packMap visited')
                           (Set.empty, Map.empty, [])
                           (rpIncludes pack)
                  -- Then add this pack's rules
                  packRules = Set.fromList (rpRuleIds pack)
                  excludes = Set.fromList (rpExcludes pack)
                  combinedRules = (rules `Set.union` rulesFromIncludes `Set.union` packRules)
                                  `Set.difference` excludes
                  -- Track sources
                  newSources = Map.fromList [(r, packId) | r <- rpRuleIds pack]
                  combinedSources = Map.union sources (Map.union sourcesFromIncludes newSources)
              in (combinedRules, combinedSources, warnings ++ warningsFromIncludes)

-- | Expand a single pack to get all its rules
expandPack :: PackRegistry -> PackId -> IO (Set Text)
expandPack registry packId = do
  let config = defaultPackConfig { pcEnabledPacks = [packId] }
  resolution <- resolvePacks registry config
  pure $ prEnabledRules resolution

-- | Merge rules from multiple packs with priority handling
mergePackRules :: [RulePack] -> Set Text
mergePackRules packs =
  let sorted = sortBy (comparing (Down . rpPriority)) packs
      allRules = concatMap rpRuleIds sorted
      allExcludes = Set.fromList $ concatMap rpExcludes sorted
  in Set.fromList allRules `Set.difference` allExcludes

--------------------------------------------------------------------------------
-- Query Functions
--------------------------------------------------------------------------------

-- | Check if a rule is in a specific pack
isRuleInPack :: PackRegistry -> Text -> PackId -> IO Bool
isRuleInPack registry ruleId packId = do
  rules <- expandPack registry packId
  pure $ ruleId `Set.member` rules

-- | Get all rules for a specific pack
getRulesByPack :: PackRegistry -> PackId -> IO [Text]
getRulesByPack registry packId = do
  rules <- expandPack registry packId
  pure $ Set.toList rules

-- | Get all packs that contain a specific rule
getPacksByRule :: PackRegistry -> Text -> IO [PackId]
getPacksByRule registry ruleId = do
  packs <- listPacks registry
  filterM (isRuleInPack registry ruleId . rpName) packs >>= pure . map rpName
  where
    filterM _ [] = pure []
    filterM p (x:xs) = do
      b <- p x
      rest <- filterM p xs
      pure $ if b then x : rest else rest

-- | Get detailed pack information
getPackInfo :: PackRegistry -> PackId -> IO (Maybe (RulePack, Int))
getPackInfo registry packId = do
  mPack <- getPackByName registry packId
  case mPack of
    Nothing -> pure Nothing
    Just pack -> do
      rules <- expandPack registry packId
      pure $ Just (pack, Set.size rules)

--------------------------------------------------------------------------------
-- Pack Profiles
--------------------------------------------------------------------------------

-- | A profile combines pack configuration with other settings
data PackProfile = PackProfile
  { ppName        :: Text             -- ^ Profile name
  , ppDescription :: Text             -- ^ Description
  , ppConfig      :: PackConfig       -- ^ Pack configuration
  , ppStrict      :: Bool             -- ^ Treat warnings as errors
  , ppAutoFix     :: Bool             -- ^ Enable auto-fix by default
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Apply a profile to get pack configuration
applyProfile :: PackProfile -> PackConfig
applyProfile = ppConfig

-- | Default profile - recommended for most projects
defaultProfile :: PackProfile
defaultProfile = PackProfile
  { ppName = "default"
  , ppDescription = "Recommended settings for most Haskell projects"
  , ppConfig = defaultPackConfig
  , ppStrict = False
  , ppAutoFix = False
  }

-- | Strict profile - all checks enabled
strictProfile :: PackProfile
strictProfile = PackProfile
  { ppName = "strict"
  , ppDescription = "All rules enabled with strict checking"
  , ppConfig = defaultPackConfig
      { pcEnabledPacks = ["strict"]
      , pcDefaultEnabled = False
      }
  , ppStrict = True
  , ppAutoFix = False
  }

-- | CI profile - optimized for continuous integration
ciProfile :: PackProfile
ciProfile = PackProfile
  { ppName = "ci"
  , ppDescription = "Settings optimized for CI pipelines"
  , ppConfig = defaultPackConfig
      { pcEnabledPacks = ["correctness", "security"]
      , pcDisabledPacks = ["style"]  -- Don't fail CI for style
      }
  , ppStrict = True  -- Fail on any issues
  , ppAutoFix = False
  }

--------------------------------------------------------------------------------
-- Pack Versioning
--------------------------------------------------------------------------------

-- | Semantic version for a pack
data PackVersion = PackVersion
  { pvMajor :: Int
  , pvMinor :: Int
  , pvPatch :: Int
  , pvPreRelease :: Maybe Text  -- e.g., "alpha", "beta", "rc1"
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON PackVersion where
  toJSON PackVersion{..} = toJSON $ formatVersion PackVersion{..}

instance FromJSON PackVersion where
  parseJSON v = do
    txt <- parseJSON v
    case parseVersion txt of
      Just pv -> pure pv
      Nothing -> fail $ "Invalid version format: " <> T.unpack txt

instance Ord PackVersion where
  compare = compareVersions

-- | Format version as string
formatVersion :: PackVersion -> Text
formatVersion PackVersion{..} =
  let base = T.pack (show pvMajor) <> "." <> T.pack (show pvMinor) <> "." <> T.pack (show pvPatch)
  in case pvPreRelease of
       Nothing -> base
       Just pre -> base <> "-" <> pre

-- | Parse version from string
parseVersion :: Text -> Maybe PackVersion
parseVersion txt =
  let (baseStr, preRelease) = case T.breakOn "-" txt of
        (b, r) | T.null r -> (b, Nothing)
               | otherwise -> (b, Just (T.drop 1 r))
      parts = T.splitOn "." baseStr
  in case parts of
       [majT, minT, patT] -> do
         maj <- readMaybe (T.unpack majT)
         mino <- readMaybe (T.unpack minT)
         pat <- readMaybe (T.unpack patT)
         Just PackVersion
           { pvMajor = maj
           , pvMinor = mino
           , pvPatch = pat
           , pvPreRelease = preRelease
           }
       _ -> Nothing

-- | Compare two versions
compareVersions :: PackVersion -> PackVersion -> Ordering
compareVersions v1 v2 =
  case compare (pvMajor v1) (pvMajor v2) of
    EQ -> case compare (pvMinor v1) (pvMinor v2) of
      EQ -> case compare (pvPatch v1) (pvPatch v2) of
        EQ -> comparePreRelease (pvPreRelease v1) (pvPreRelease v2)
        other -> other
      other -> other
    other -> other
  where
    -- Pre-release versions are less than release versions
    comparePreRelease Nothing Nothing = EQ
    comparePreRelease Nothing (Just _) = GT  -- Release > pre-release
    comparePreRelease (Just _) Nothing = LT  -- Pre-release < release
    comparePreRelease (Just a) (Just b) = compare a b

-- | Pack manifest with metadata for sharing
data PackManifest = PackManifest
  { pmName         :: Text
  , pmVersion      :: PackVersion
  , pmAuthor       :: Maybe Text
  , pmLicense      :: Maybe Text
  , pmRepository   :: Maybe Text
  , pmHomepage     :: Maybe Text
  , pmMinArgusVer  :: Maybe PackVersion  -- Minimum required Argus version
  , pmCreatedAt    :: Maybe UTCTime
  , pmUpdatedAt    :: Maybe UTCTime
  , pmChecksum     :: Maybe Text         -- SHA256 hash of rules
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | A versioned pack with full metadata
data VersionedPack = VersionedPack
  { vpManifest :: PackManifest
  , vpPack     :: RulePack
  , vpRules    :: [Text]  -- Inline rule definitions (TOML/JSON)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

--------------------------------------------------------------------------------
-- Pack Import/Export
--------------------------------------------------------------------------------

-- | Export a pack to a versioned format
exportPack :: RulePack -> PackVersion -> Maybe Text -> VersionedPack
exportPack pack version author = VersionedPack
  { vpManifest = PackManifest
      { pmName = rpName pack
      , pmVersion = version
      , pmAuthor = author
      , pmLicense = Just "MIT"
      , pmRepository = Nothing
      , pmHomepage = Nothing
      , pmMinArgusVer = Just PackVersion { pvMajor = 1, pvMinor = 0, pvPatch = 0, pvPreRelease = Nothing }
      , pmCreatedAt = Nothing
      , pmUpdatedAt = Nothing
      , pmChecksum = Nothing
      }
  , vpPack = pack
  , vpRules = []  -- Rules are referenced by ID, not inlined
  }

-- | Import a versioned pack
importPack :: VersionedPack -> RulePack
importPack = vpPack

-- | Serialize a versioned pack to JSON
serializePack :: VersionedPack -> BL.ByteString
serializePack = encode

-- | Deserialize a versioned pack from JSON
deserializePack :: BL.ByteString -> Either Text VersionedPack
deserializePack bs = case eitherDecode bs of
  Left err -> Left $ T.pack err
  Right vp -> Right vp

-- | Export a pack to a file
exportPackToFile :: FilePath -> VersionedPack -> IO (Either Text ())
exportPackToFile path vpack = do
  result <- try $ BL.writeFile path (serializePack vpack)
  case result of
    Left (e :: SomeException) -> pure $ Left $ T.pack $ show e
    Right () -> pure $ Right ()

-- | Import a pack from a file
importPackFromFile :: FilePath -> IO (Either Text VersionedPack)
importPackFromFile path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ "File not found: " <> T.pack path
    else do
      result <- try $ BL.readFile path
      case result of
        Left (e :: SomeException) -> pure $ Left $ T.pack $ show e
        Right bs -> pure $ deserializePack bs

-- | Create a new pack with timestamp
createVersionedPack :: RulePack -> PackVersion -> Maybe Text -> IO VersionedPack
createVersionedPack pack version author = do
  now <- getCurrentTime
  let vp = exportPack pack version author
      manifest = vpManifest vp
  pure vp { vpManifest = manifest { pmCreatedAt = Just now, pmUpdatedAt = Just now } }

-- | Bump version
bumpVersion :: PackVersion -> VersionBump -> PackVersion
bumpVersion PackVersion{..} bump = case bump of
  BumpMajor -> PackVersion { pvMajor = pvMajor + 1, pvMinor = 0, pvPatch = 0, pvPreRelease = Nothing }
  BumpMinor -> PackVersion { pvMajor = pvMajor, pvMinor = pvMinor + 1, pvPatch = 0, pvPreRelease = Nothing }
  BumpPatch -> PackVersion { pvMajor = pvMajor, pvMinor = pvMinor, pvPatch = pvPatch + 1, pvPreRelease = Nothing }

data VersionBump = BumpMajor | BumpMinor | BumpPatch
  deriving stock (Eq, Show)
