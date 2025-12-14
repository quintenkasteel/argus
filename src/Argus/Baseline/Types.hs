{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Argus.Baseline.Types
-- Description : Types for baseline comparison and regression detection
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module defines the core types used for baseline management,
-- enabling tracking of known issues and detection of regressions.
module Argus.Baseline.Types
  ( -- * Baseline Types
    Baseline (..)
  , BaselineEntry (..)
  , BaselineVersion
  , currentBaselineVersion

    -- * Diff Types
  , BaselineDiff (..)
  , DiffStats (..)
  , DiffEntry (..)
  , DiffKind (..)

    -- * CI Mode Types
  , CIOptions (..)
  , ExitReason (..)
  , CIResult (..)

    -- * Entry Matching
  , DiagnosticFingerprint (..)
  , computeFingerprint
  , fingerprintMatches

    -- * Constructors
  , emptyBaseline
  , emptyDiff
  , mkBaselineEntry
  ) where

import Crypto.Hash (SHA256, Digest, hash)
import Data.Aeson
  ( ToJSON (..)
  , FromJSON (..)
  , object
  , (.=)
  , (.:)
  , (.:?)
  , (.!=)
  , withObject
  )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Argus.Types (Diagnostic (..), Severity (..), SrcSpan (..), unLine, unColumn)

--------------------------------------------------------------------------------
-- Version
--------------------------------------------------------------------------------

-- | Baseline file format version
type BaselineVersion = Int

-- | Current baseline format version
currentBaselineVersion :: BaselineVersion
currentBaselineVersion = 2

--------------------------------------------------------------------------------
-- Baseline Types
--------------------------------------------------------------------------------

-- | A baseline captures known issues at a point in time
data Baseline = Baseline
  { baselineVersion     :: BaselineVersion      -- ^ Format version
  , baselineCreated     :: UTCTime              -- ^ Creation timestamp
  , baselineDescription :: Maybe Text           -- ^ Optional description
  , baselineCommit      :: Maybe Text           -- ^ Git commit hash (optional)
  , baselineEntries     :: [BaselineEntry]      -- ^ Known issues
  , baselineMetadata    :: Map Text Text        -- ^ Additional metadata
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Baseline where
  toJSON Baseline{..} = object
    [ "version"     .= baselineVersion
    , "created"     .= baselineCreated
    , "description" .= baselineDescription
    , "commit"      .= baselineCommit
    , "entries"     .= baselineEntries
    , "metadata"    .= baselineMetadata
    ]

instance FromJSON Baseline where
  parseJSON = withObject "Baseline" $ \o -> do
    version <- o .:? "version" .!= 1
    Baseline
      <$> pure version
      <*> o .: "created"
      <*> o .:? "description"
      <*> o .:? "commit"
      <*> o .: "entries"
      <*> o .:? "metadata" .!= Map.empty

-- | An entry in the baseline representing a known issue
data BaselineEntry = BaselineEntry
  { beFile        :: FilePath             -- ^ File path
  , beLine        :: Int                  -- ^ Line number
  , beColumn      :: Int                  -- ^ Column number
  , beRule        :: Text                 -- ^ Rule ID that triggered
  , beMessage     :: Text                 -- ^ Diagnostic message
  , beSeverity    :: Severity             -- ^ Severity level
  , beFingerprint :: Text                 -- ^ Content fingerprint for matching
  , beReason      :: Maybe Text           -- ^ Reason for baseline (optional)
  , beExpires     :: Maybe UTCTime        -- ^ Expiration date (optional)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BaselineEntry where
  toJSON BaselineEntry{..} = object
    [ "file"        .= beFile
    , "line"        .= beLine
    , "column"      .= beColumn
    , "rule"        .= beRule
    , "message"     .= beMessage
    , "severity"    .= beSeverity
    , "fingerprint" .= beFingerprint
    , "reason"      .= beReason
    , "expires"     .= beExpires
    ]

instance FromJSON BaselineEntry where
  parseJSON = withObject "BaselineEntry" $ \o ->
    BaselineEntry
      <$> o .: "file"
      <*> o .: "line"
      <*> o .:? "column" .!= 1
      <*> o .: "rule"
      <*> o .:? "message" .!= ""
      <*> o .:? "severity" .!= Warning
      <*> o .: "fingerprint"
      <*> o .:? "reason"
      <*> o .:? "expires"

instance Ord BaselineEntry where
  compare a b = compare
    (beFile a, beLine a, beColumn a, beRule a)
    (beFile b, beLine b, beColumn b, beRule b)

-- | Empty baseline for initialization
emptyBaseline :: UTCTime -> Baseline
emptyBaseline now = Baseline
  { baselineVersion     = currentBaselineVersion
  , baselineCreated     = now
  , baselineDescription = Nothing
  , baselineCommit      = Nothing
  , baselineEntries     = []
  , baselineMetadata    = Map.empty
  }

--------------------------------------------------------------------------------
-- Fingerprinting
--------------------------------------------------------------------------------

-- | A fingerprint for matching diagnostics across runs
data DiagnosticFingerprint = DiagnosticFingerprint
  { dfFile      :: FilePath
  , dfRule      :: Text
  , dfMessage   :: Text
  , dfLineRange :: (Int, Int)  -- ^ Line range for fuzzy matching
  , dfHash      :: Text        -- ^ SHA256 of key components
  }
  deriving stock (Eq, Show, Generic)

-- | Compute a fingerprint for a diagnostic
computeFingerprint :: Diagnostic -> DiagnosticFingerprint
computeFingerprint diag =
  let file = srcSpanFile (diagSpan diag)
      rule = maybe "" id (diagCode diag)
      msg = diagMessage diag
      line = unLine (srcSpanStartLine (diagSpan diag))
      -- Allow 3-line fuzzy matching window
      lineRange = (max 1 (line - 3), line + 3)
      -- Hash key components
      hashInput = TE.encodeUtf8 $ T.pack file <> "|" <> rule <> "|" <> msg
      hashResult = show (hash hashInput :: Digest SHA256)
  in DiagnosticFingerprint
    { dfFile = file
    , dfRule = rule
    , dfMessage = msg
    , dfLineRange = lineRange
    , dfHash = T.pack $ take 16 hashResult
    }

-- | Check if a fingerprint matches a baseline entry
fingerprintMatches :: DiagnosticFingerprint -> BaselineEntry -> Bool
fingerprintMatches fp entry =
  -- Primary match: same fingerprint hash
  dfHash fp == beFingerprint entry ||
  -- Fuzzy match: same file, rule, and line within range
  (dfFile fp == beFile entry &&
   dfRule fp == beRule entry &&
   let (minL, maxL) = dfLineRange fp
   in beLine entry >= minL && beLine entry <= maxL)

-- | Create a baseline entry from a diagnostic
mkBaselineEntry :: Diagnostic -> Maybe Text -> BaselineEntry
mkBaselineEntry diag reason =
  let fp = computeFingerprint diag
  in BaselineEntry
    { beFile = srcSpanFile (diagSpan diag)
    , beLine = unLine (srcSpanStartLine (diagSpan diag))
    , beColumn = unColumn (srcSpanStartCol (diagSpan diag))
    , beRule = maybe "" id (diagCode diag)
    , beMessage = diagMessage diag
    , beSeverity = diagSeverity diag
    , beFingerprint = dfHash fp
    , beReason = reason
    , beExpires = Nothing
    }

--------------------------------------------------------------------------------
-- Diff Types
--------------------------------------------------------------------------------

-- | Result of comparing current analysis against a baseline
data BaselineDiff = BaselineDiff
  { bdNew       :: [DiffEntry]    -- ^ Issues not in baseline (regressions)
  , bdResolved  :: [DiffEntry]    -- ^ Issues fixed since baseline
  , bdExisting  :: [DiffEntry]    -- ^ Issues matching baseline
  , bdExpired   :: [DiffEntry]    -- ^ Baselined issues past expiration
  , bdStats     :: DiffStats      -- ^ Summary statistics
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BaselineDiff where
  toJSON BaselineDiff{..} = object
    [ "new"      .= bdNew
    , "resolved" .= bdResolved
    , "existing" .= bdExisting
    , "expired"  .= bdExpired
    , "stats"    .= bdStats
    ]

-- | Statistics about the diff
data DiffStats = DiffStats
  { dsNewCount      :: Int    -- ^ Count of new issues
  , dsResolvedCount :: Int    -- ^ Count of resolved issues
  , dsExistingCount :: Int    -- ^ Count of existing issues
  , dsExpiredCount  :: Int    -- ^ Count of expired baselined issues
  , dsDelta         :: Int    -- ^ Net change (new - resolved)
  , dsTotalCurrent  :: Int    -- ^ Total issues in current analysis
  , dsTotalBaseline :: Int    -- ^ Total issues in baseline
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DiffStats where
  toJSON DiffStats{..} = object
    [ "new"           .= dsNewCount
    , "resolved"      .= dsResolvedCount
    , "existing"      .= dsExistingCount
    , "expired"       .= dsExpiredCount
    , "delta"         .= dsDelta
    , "totalCurrent"  .= dsTotalCurrent
    , "totalBaseline" .= dsTotalBaseline
    ]

-- | A single entry in the diff result
data DiffEntry = DiffEntry
  { deKind        :: DiffKind
  , deDiagnostic  :: Maybe Diagnostic   -- ^ Current diagnostic (for new/existing)
  , deBaseline    :: Maybe BaselineEntry -- ^ Baseline entry (for resolved/existing)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DiffEntry where
  toJSON DiffEntry{..} = object
    [ "kind"       .= deKind
    , "diagnostic" .= fmap diagToJson deDiagnostic
    , "baseline"   .= deBaseline
    ]
    where
      diagToJson d = object
        [ "file"     .= srcSpanFile (diagSpan d)
        , "line"     .= unLine (srcSpanStartLine (diagSpan d))
        , "column"   .= unColumn (srcSpanStartCol (diagSpan d))
        , "rule"     .= diagCode d
        , "message"  .= diagMessage d
        , "severity" .= diagSeverity d
        ]

-- | Kind of diff entry
data DiffKind
  = DiffNew       -- ^ New issue (regression)
  | DiffResolved  -- ^ Resolved issue (improvement)
  | DiffExisting  -- ^ Issue exists in both
  | DiffExpired   -- ^ Baselined issue past expiration
  deriving stock (Eq, Show, Generic)

instance ToJSON DiffKind where
  toJSON DiffNew      = "new"
  toJSON DiffResolved = "resolved"
  toJSON DiffExisting = "existing"
  toJSON DiffExpired  = "expired"

-- | Empty diff result
emptyDiff :: BaselineDiff
emptyDiff = BaselineDiff
  { bdNew      = []
  , bdResolved = []
  , bdExisting = []
  , bdExpired  = []
  , bdStats    = DiffStats 0 0 0 0 0 0 0
  }

--------------------------------------------------------------------------------
-- CI Mode Types
--------------------------------------------------------------------------------

-- | Options for CI mode operation
data CIOptions = CIOptions
  { ciBaselinePath    :: Maybe FilePath      -- ^ Path to baseline file
  , ciFailOnNew       :: Bool                -- ^ Fail if new issues found
  , ciFailOnSeverity  :: Maybe Severity      -- ^ Fail if issues at this severity
  , ciFailOnCount     :: Maybe Int           -- ^ Fail if issue count exceeds
  , ciFailOnDelta     :: Maybe Int           -- ^ Fail if delta exceeds
  , ciUpdateBaseline  :: Bool                -- ^ Update baseline with current
  , ciQuiet           :: Bool                -- ^ Minimal output
  }
  deriving stock (Eq, Show)

-- | Reason for CI failure
data ExitReason
  = ExitSuccess             -- ^ No issues or all within baseline
  | ExitIssuesFound         -- ^ Issues found (normal behavior)
  | ExitNewIssues Int       -- ^ New issues vs baseline
  | ExitSeverityThreshold   -- ^ Issues exceed severity threshold
  | ExitCountThreshold Int  -- ^ Issues exceed count threshold
  | ExitDeltaThreshold Int  -- ^ Delta exceeds threshold
  | ExitConfigError Text    -- ^ Configuration error
  | ExitParseError Text     -- ^ Parse error in source files
  deriving stock (Eq, Show)

-- | Result of CI analysis
data CIResult = CIResult
  { crExitReason :: ExitReason
  , crExitCode   :: Int
  , crDiff       :: Maybe BaselineDiff
  , crMessage    :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON CIResult where
  toJSON CIResult{..} = object
    [ "exitCode" .= crExitCode
    , "reason"   .= show crExitReason
    , "message"  .= crMessage
    , "diff"     .= crDiff
    ]
