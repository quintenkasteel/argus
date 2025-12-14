{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Suppression
-- Description : Diagnostic suppression and baseline management
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides mechanisms for suppressing diagnostics:
--
-- * Inline suppression comments (@-- argus:ignore@, @-- argus:disable@)
-- * Baseline files for known issues
-- * Per-rule and per-file suppression
--
-- == Suppression Comments
--
-- @
-- -- argus:ignore
-- head xs  -- Next line is ignored
--
-- -- argus:ignore avoid-head
-- head xs  -- Only avoid-head rule is ignored on next line
--
-- foo = head xs -- argus:ignore-line
-- -- Ignores this specific line
--
-- -- argus:disable avoid-head
-- head xs  -- Disabled from here...
-- tail ys
-- -- argus:enable avoid-head
-- head zs  -- ...until here (this line not suppressed)
--
-- -- argus:disable-next-line avoid-head
-- head xs  -- Only next line is suppressed
--
-- foo = head xs -- argus:disable-line avoid-head
-- -- Only this specific line is suppressed
--
-- -- argus:ignore-file
-- -- Ignores all diagnostics in this file
-- @
--
-- == Baseline Files
--
-- Baseline files (.argus-baseline.json) capture current issues:
--
-- @
-- {
--   "version": 1,
--   "created": "2024-01-01T00:00:00Z",
--   "issues": [
--     {"file": "src/Main.hs", "line": 10, "rule": "avoid-head", "hash": "abc123"}
--   ]
-- }
-- @
module Argus.Suppression
  ( -- * Suppression Types
    Suppression (..)
  , SuppressionKind (..)
  , SuppressionScope (..)

    -- * Baseline Types
  , Baseline (..)
  , BaselineEntry (..)
  , BaselineVersion

    -- * Parsing Suppressions
  , parseSuppressions
  , parseSuppressionComment
  , extractSuppressions

    -- * Applying Suppressions
  , applySuppression
  , applySuppressions
  , filterSuppressed

    -- * Baseline Operations
  , loadBaseline
  , saveBaseline
  , createBaseline
  , mergeBaselines
  , baselineContains

    -- * Utilities
  , hashDiagnostic
  , matchesSuppression

    -- * Adding Suppressions
  , addToBaseline
  , addSuppressionEntry
  , defaultBaselinePath
  ) where

import Crypto.Hash (SHA256, Digest, hash)
import Data.Aeson (ToJSON (..), FromJSON (..), object, (.=), (.:), (.:?), withObject)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show, iso8601ParseM)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)

import Argus.Types

--------------------------------------------------------------------------------
-- Suppression Types
--------------------------------------------------------------------------------

-- | A suppression directive
data Suppression = Suppression
  { suppKind     :: SuppressionKind    -- ^ What type of suppression
  , suppScope    :: SuppressionScope   -- ^ Scope of suppression
  , suppRules    :: Maybe (Set Text)   -- ^ Specific rules (Nothing = all)
  , suppReason   :: Maybe Text         -- ^ Optional reason
  , suppLocation :: SrcSpan            -- ^ Where the suppression is defined
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Kind of suppression
data SuppressionKind
  = IgnoreNext       -- ^ Ignore next line
  | IgnoreLine       -- ^ Ignore this line (inline comment)
  | IgnoreFile       -- ^ Ignore entire file
  | IgnoreBlock      -- ^ Ignore until matching end comment
  | DisableRule      -- ^ Disable rule for rest of file
  | DisableNextLine  -- ^ Disable for next line only (alias for IgnoreNext)
  | DisableLine      -- ^ Disable for current line (alias for IgnoreLine)
  | EnableRule       -- ^ Re-enable previously disabled rule
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Scope of suppression
data SuppressionScope
  = ScopeLine Int            -- ^ Single line number
  | ScopeRange Int Int       -- ^ Line range (inclusive)
  | ScopeFile                -- ^ Entire file
  | ScopeGlobal              -- ^ Global (via config)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Baseline Types
--------------------------------------------------------------------------------

-- | Current baseline format version
type BaselineVersion = Int

currentBaselineVersion :: BaselineVersion
currentBaselineVersion = 1

-- | A baseline of known issues
data Baseline = Baseline
  { blVersion    :: BaselineVersion
  , blCreated    :: UTCTime
  , blUpdated    :: UTCTime
  , blEntries    :: [BaselineEntry]
  , blMetadata   :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Baseline where
  toJSON Baseline{..} = object
    [ "version"  .= blVersion
    , "created"  .= iso8601Show blCreated
    , "updated"  .= iso8601Show blUpdated
    , "issues"   .= blEntries
    , "metadata" .= blMetadata
    ]

instance FromJSON Baseline where
  parseJSON = withObject "Baseline" $ \o -> do
    blVersion  <- o .: "version"
    createdStr <- o .: "created"
    updatedStr <- o .: "updated"
    blEntries  <- o .: "issues"
    blMetadata <- o .:? "metadata"
    blCreated  <- maybe (fail "Invalid created timestamp") pure (iso8601ParseM createdStr)
    blUpdated  <- maybe (fail "Invalid updated timestamp") pure (iso8601ParseM updatedStr)
    pure Baseline{..}

-- | An entry in the baseline
data BaselineEntry = BaselineEntry
  { beFile      :: FilePath      -- ^ File path
  , beLine      :: Int           -- ^ Line number
  , beRule      :: Text          -- ^ Rule code
  , beHash      :: Text          -- ^ Content hash for stability
  , beMessage   :: Maybe Text    -- ^ Original message (for reference)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Parsing Suppressions
--------------------------------------------------------------------------------

-- | Parse all suppression comments from source text
parseSuppressions :: FilePath -> Text -> [Suppression]
parseSuppressions path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (parseLineSuppressions path) linesWithNums

-- | Parse suppressions from a single line
parseLineSuppressions :: FilePath -> (Int, Text) -> [Suppression]
parseLineSuppressions path (lineNum, line) =
  mapMaybe (parseComment path lineNum) (extractComments line)

-- | Extract comments from a line
extractComments :: Text -> [Text]
extractComments line =
  let -- Handle -- comments
      dashComment = case T.breakOn "--" line of
        (_, rest) | not (T.null rest) -> [T.drop 2 rest]
        _ -> []
      -- Handle {- -} comments
      blockComments = extractBlockComments line
  in dashComment ++ blockComments

-- | Extract {- -} style comments
extractBlockComments :: Text -> [Text]
extractBlockComments line = go line []
  where
    go txt acc = case T.breakOn "{-" txt of
      (_, rest) | not (T.null rest) ->
        case T.breakOn "-}" (T.drop 2 rest) of
          (content, rest') | not (T.null rest') ->
            go (T.drop 2 rest') (T.strip content : acc)
          _ -> acc
      _ -> acc

-- | Parse a suppression comment
parseComment :: FilePath -> Int -> Text -> Maybe Suppression
parseComment path lineNum comment = do
  -- Check for argus: prefix
  stripped <- T.stripPrefix "argus:" (T.stripStart comment)
          <|> T.stripPrefix "ARGUS:" (T.stripStart comment)
          <|> T.stripPrefix " argus:" (T.stripStart comment)
          <|> T.stripPrefix " ARGUS:" (T.stripStart comment)

  parseSuppressionComment path lineNum stripped
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) Nothing y = y
    (<|>) x       _ = x

-- | Parse the content of a suppression comment (after 'argus:' prefix)
parseSuppressionComment :: FilePath -> Int -> Text -> Maybe Suppression
parseSuppressionComment path lineNum txt = do
  let (directive, rest) = T.break (== ' ') (T.stripStart txt)
      params = T.strip rest

  case T.toLower directive of
    "ignore" -> Just Suppression
      { suppKind = IgnoreNext
      , suppScope = ScopeLine (lineNum + 1)
      , suppRules = parseRuleList params
      , suppReason = extractReason params
      , suppLocation = mkSrcSpanRaw path lineNum 1 lineNum (T.length txt + 1)
      }

    "ignore-line" -> Just Suppression
      { suppKind = IgnoreLine
      , suppScope = ScopeLine lineNum
      , suppRules = parseRuleList params
      , suppReason = extractReason params
      , suppLocation = mkSrcSpanRaw path lineNum 1 lineNum (T.length txt + 1)
      }

    "ignore-file" -> Just Suppression
      { suppKind = IgnoreFile
      , suppScope = ScopeFile
      , suppRules = parseRuleList params
      , suppReason = extractReason params
      , suppLocation = mkSrcSpanRaw path lineNum 1 lineNum (T.length txt + 1)
      }

    "disable" -> Just Suppression
      { suppKind = DisableRule
      , suppScope = ScopeRange lineNum maxBound
      , suppRules = parseRuleList params
      , suppReason = extractReason params
      , suppLocation = mkSrcSpanRaw path lineNum 1 lineNum (T.length txt + 1)
      }

    "disable-next-line" -> Just Suppression
      { suppKind = DisableNextLine
      , suppScope = ScopeLine (lineNum + 1)
      , suppRules = parseRuleList params
      , suppReason = extractReason params
      , suppLocation = mkSrcSpanRaw path lineNum 1 lineNum (T.length txt + 1)
      }

    "disable-line" -> Just Suppression
      { suppKind = DisableLine
      , suppScope = ScopeLine lineNum
      , suppRules = parseRuleList params
      , suppReason = extractReason params
      , suppLocation = mkSrcSpanRaw path lineNum 1 lineNum (T.length txt + 1)
      }

    "enable" -> Just Suppression
      { suppKind = EnableRule
      , suppScope = ScopeRange lineNum maxBound
      , suppRules = parseRuleList params
      , suppReason = extractReason params
      , suppLocation = mkSrcSpanRaw path lineNum 1 lineNum (T.length txt + 1)
      }

    _ -> Nothing

-- | Parse a comma-separated list of rule names
parseRuleList :: Text -> Maybe (Set Text)
parseRuleList txt
  | T.null stripped = Nothing
  | otherwise =
      let rules = filter (not . T.null) $ map T.strip $ T.splitOn "," stripped
          -- Remove reason part if present
          rulesOnly = takeWhile (not . T.isPrefixOf "--") rules
      in if null rulesOnly then Nothing else Just (Set.fromList rulesOnly)
  where
    stripped = T.strip txt

-- | Extract reason from parameters (after --)
extractReason :: Text -> Maybe Text
extractReason txt = case T.breakOn "--" txt of
  (_, rest) | not (T.null rest) -> Just (T.strip $ T.drop 2 rest)
  _ -> Nothing

-- | Extract all suppressions from source
extractSuppressions :: FilePath -> Text -> [Suppression]
extractSuppressions = parseSuppressions

--------------------------------------------------------------------------------
-- Applying Suppressions
--------------------------------------------------------------------------------

-- | Check if a diagnostic matches a suppression
matchesSuppression :: Suppression -> Diagnostic -> Bool
matchesSuppression Suppression{..} diag =
  scopeMatches && ruleMatches
  where
    diagLine = srcSpanStartLineRaw (diagSpan diag)

    scopeMatches = case suppScope of
      ScopeLine l    -> diagLine == l
      ScopeRange l h -> diagLine >= l && diagLine <= h
      ScopeFile      -> True
      ScopeGlobal    -> True

    ruleMatches = case suppRules of
      Nothing    -> True  -- All rules suppressed
      Just rules -> case diagCode diag of
        Nothing   -> False  -- Can't match if no code
        Just code -> code `Set.member` rules ||
                     any (`T.isInfixOf` code) rules

-- | Apply a single suppression to a list of diagnostics
applySuppression :: Suppression -> [Diagnostic] -> [Diagnostic]
applySuppression supp = filter (not . matchesSuppression supp)

-- | Apply all suppressions to diagnostics
-- This handles disable/enable pairs properly by processing them in order
applySuppressions :: [Suppression] -> [Diagnostic] -> [Diagnostic]
applySuppressions supps diags =
  let -- Separate enable directives from other suppressions
      (enables, others) = partitionEnables supps
      -- Build effective suppressions by resolving disable/enable pairs
      effective = resolveDisableEnable others enables
  in foldr applySuppression diags effective
  where
    partitionEnables :: [Suppression] -> ([Suppression], [Suppression])
    partitionEnables = foldr go ([], [])
      where
        go s (es, os) = case suppKind s of
          EnableRule -> (s : es, os)
          _          -> (es, s : os)

    -- Resolve disable/enable pairs to create effective suppressions
    -- For each disable, find the corresponding enable (if any) and limit the range
    resolveDisableEnable :: [Suppression] -> [Suppression] -> [Suppression]
    resolveDisableEnable disables enables =
      map (limitDisableRange enables) disables

    -- Limit a disable suppression's range if there's a matching enable
    limitDisableRange :: [Suppression] -> Suppression -> Suppression
    limitDisableRange enables supp =
      case suppKind supp of
        DisableRule ->
          case suppScope supp of
            ScopeRange startLine _ ->
              -- Find the first enable after this disable that matches the same rules
              case findMatchingEnable startLine (suppRules supp) enables of
                Just enableLine ->
                  supp { suppScope = ScopeRange startLine (enableLine - 1) }
                Nothing ->
                  supp  -- No matching enable, keep original range
            _ -> supp
        _ -> supp

    -- Find the first enable directive after the given line that matches the rules
    findMatchingEnable :: Int -> Maybe (Set Text) -> [Suppression] -> Maybe Int
    findMatchingEnable startLine rules enables =
      let matching = [ enableLine
                     | Suppression { suppKind = EnableRule
                                   , suppScope = ScopeRange enableLine _
                                   , suppRules = enableRules
                                   } <- enables
                     , enableLine > startLine
                     , rulesMatch rules enableRules
                     ]
      in case matching of
           [] -> Nothing
           (l:_) -> Just l

    -- Check if two rule sets match (both Nothing or both have overlap)
    rulesMatch :: Maybe (Set Text) -> Maybe (Set Text) -> Bool
    rulesMatch Nothing Nothing = True
    rulesMatch (Just r1) (Just r2) = not $ Set.null $ Set.intersection r1 r2
    rulesMatch (Just _) Nothing = True   -- Enable all re-enables specific rules
    rulesMatch Nothing (Just _) = True   -- Disable all can be re-enabled by specific rules

-- | Filter diagnostics based on suppressions in source
filterSuppressed :: FilePath -> Text -> [Diagnostic] -> [Diagnostic]
filterSuppressed path content diags =
  let supps = parseSuppressions path content
  in applySuppressions supps diags

--------------------------------------------------------------------------------
-- Baseline Operations
--------------------------------------------------------------------------------

-- | Load a baseline from file
loadBaseline :: FilePath -> IO (Maybe Baseline)
loadBaseline path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      content <- BL.readFile path
      pure $ Aeson.decode content

-- | Save a baseline to file
saveBaseline :: FilePath -> Baseline -> IO ()
saveBaseline path baseline = do
  let json = Aeson.encode baseline
  BL.writeFile path json

-- | Create a baseline from current diagnostics
createBaseline :: [(FilePath, [Diagnostic])] -> IO Baseline
createBaseline filesDiags = do
  now <- getCurrentTime
  let entries = concatMap mkEntries filesDiags
  pure Baseline
    { blVersion  = currentBaselineVersion
    , blCreated  = now
    , blUpdated  = now
    , blEntries  = entries
    , blMetadata = Nothing
    }
  where
    mkEntries :: (FilePath, [Diagnostic]) -> [BaselineEntry]
    mkEntries (file, diags) = map (mkEntry file) diags

    mkEntry :: FilePath -> Diagnostic -> BaselineEntry
    mkEntry file diag = BaselineEntry
      { beFile    = file
      , beLine    = srcSpanStartLineRaw (diagSpan diag)
      , beRule    = fromMaybe "unknown" (diagCode diag)
      , beHash    = hashDiagnostic diag
      , beMessage = Just (diagMessage diag)
      }

-- | Merge two baselines, keeping newer entries
mergeBaselines :: Baseline -> Baseline -> Baseline
mergeBaselines old new = new
  { blEntries = uniqueEntries $ blEntries new ++ blEntries old
  , blCreated = blCreated old  -- Preserve original creation time
  }
  where
    uniqueEntries :: [BaselineEntry] -> [BaselineEntry]
    uniqueEntries = go Set.empty []

    go :: Set (FilePath, Int, Text) -> [BaselineEntry] -> [BaselineEntry] -> [BaselineEntry]
    go _ acc [] = reverse acc
    go seen acc (e:es) =
      let key = (beFile e, beLine e, beRule e)
      in if key `Set.member` seen
         then go seen acc es
         else go (Set.insert key seen) (e : acc) es

-- | Check if a diagnostic is in the baseline
baselineContains :: Baseline -> Diagnostic -> FilePath -> Bool
baselineContains Baseline{..} diag file =
  any matchesEntry blEntries
  where
    diagLine = srcSpanStartLineRaw (diagSpan diag)
    diagRule = fromMaybe "unknown" (diagCode diag)
    diagHash = hashDiagnostic diag

    matchesEntry :: BaselineEntry -> Bool
    matchesEntry BaselineEntry{..} =
      beFile == file &&
      beLine == diagLine &&
      beRule == diagRule &&
      beHash == diagHash

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Create a stable hash of a diagnostic for baseline matching
hashDiagnostic :: Diagnostic -> Text
hashDiagnostic diag =
  let content = T.concat
        [ diagMessage diag
        , fromMaybe "" (diagCode diag)
        , T.pack $ show (srcSpanStartLineRaw $ diagSpan diag)
        , T.pack $ show (srcSpanStartColRaw $ diagSpan diag)
        ]
      digest :: Digest SHA256
      digest = hash (TE.encodeUtf8 content)
  in T.pack $ take 16 $ show digest

--------------------------------------------------------------------------------
-- Adding Suppressions
--------------------------------------------------------------------------------

-- | Default path for the baseline file
defaultBaselinePath :: FilePath
defaultBaselinePath = ".argus-baseline.json"

-- | Add a single entry to the baseline file
-- Creates the file if it doesn't exist
addToBaseline :: FilePath -> BaselineEntry -> IO ()
addToBaseline path entry = do
  existing <- loadBaseline path
  now <- getCurrentTime
  case existing of
    Nothing -> do
      -- Create new baseline with this entry
      let baseline = Baseline
            { blVersion  = currentBaselineVersion
            , blCreated  = now
            , blUpdated  = now
            , blEntries  = [entry]
            , blMetadata = Just "Created by Argus LSP"
            }
      saveBaseline path baseline
    Just bl -> do
      -- Add to existing baseline if not already present
      let alreadyExists = any (entryMatches entry) (blEntries bl)
      if alreadyExists
        then pure ()  -- Already suppressed
        else do
          let updated = bl
                { blUpdated = now
                , blEntries = entry : blEntries bl
                }
          saveBaseline path updated
  where
    entryMatches :: BaselineEntry -> BaselineEntry -> Bool
    entryMatches e1 e2 =
      beFile e1 == beFile e2 &&
      beLine e1 == beLine e2 &&
      beRule e1 == beRule e2

-- | Add a suppression entry from file URI and rule code
-- Returns the path to the baseline file that was updated
addSuppressionEntry :: Text -> Maybe Text -> Int -> IO FilePath
addSuppressionEntry fileUri ruleCode lineNum = do
  let filePath = uriToPath fileUri
      entry = BaselineEntry
        { beFile    = filePath
        , beLine    = lineNum
        , beRule    = fromMaybe "unknown" ruleCode
        , beHash    = hashEntry filePath lineNum ruleCode
        , beMessage = Just $ "Suppressed via IDE at line " <> T.pack (show lineNum)
        }
  addToBaseline defaultBaselinePath entry
  pure defaultBaselinePath
  where
    uriToPath :: Text -> FilePath
    uriToPath uri = T.unpack $ fromMaybe uri $ T.stripPrefix "file://" uri

    hashEntry :: FilePath -> Int -> Maybe Text -> Text
    hashEntry file line code =
      let content = T.concat
            [ T.pack file
            , T.pack (show line)
            , fromMaybe "" code
            ]
          digest :: Digest SHA256
          digest = hash (TE.encodeUtf8 content)
      in T.pack $ take 16 $ show digest

