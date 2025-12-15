{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Argus.Output.CodeClimate
-- Description : Code Climate JSON output format
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides Code Climate JSON output format.
-- Code Climate format is used by GitLab CI/CD for displaying
-- code quality reports in merge requests.
module Argus.Output.CodeClimate
  ( renderCodeClimate
  , codeClimateFormatter
  ) where

import Data.Aeson (ToJSON (..), encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.Char (ord)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word ()
import GHC.Generics (Generic)
import Numeric (showHex)

import Argus.Output.Types
import Argus.Types

-- | Code Climate formatter
codeClimateFormatter :: Formatter
codeClimateFormatter = Formatter
  { fmtFormat = CodeClimateFormat
  , fmtRender = renderCodeClimate
  }

-- | Render analysis result as Code Climate JSON
renderCodeClimate :: OutputOptions -> AnalysisResult -> Output
renderCodeClimate _opts result = Output
  { outText = TE.decodeUtf8 $ BL.toStrict $ encode (toCodeClimateIssues result)
  , outSummary = mkSummary result
  }

-- | Convert analysis result to Code Climate issues
toCodeClimateIssues :: AnalysisResult -> [CodeClimateIssue]
toCodeClimateIssues result = concatMap fileToIssues (Map.toList (resultFiles result))
  where
    fileToIssues (path, fr) = map (diagToIssue path) (fileResultDiagnostics fr)

-- | Code Climate issue representation
data CodeClimateIssue = CodeClimateIssue
  { cciType          :: Text
  , cciCheckName     :: Text
  , cciDescription   :: Text
  , cciContent       :: Maybe CodeClimateContent
  , cciCategories    :: [Text]
  , cciLocation      :: CodeClimateLocation
  , cciSeverity      :: Text
  , cciFingerprint   :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CodeClimateIssue where
  toJSON CodeClimateIssue{..} = object $
    [ "type"        .= cciType
    , "check_name"  .= cciCheckName
    , "description" .= cciDescription
    , "categories"  .= cciCategories
    , "location"    .= cciLocation
    , "severity"    .= cciSeverity
    , "fingerprint" .= cciFingerprint
    ] ++ maybe [] (\c -> ["content" .= c]) cciContent

-- | Code Climate content (additional details)
data CodeClimateContent = CodeClimateContent
  { cccBody :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CodeClimateContent where
  toJSON CodeClimateContent{..} = object
    [ "body" .= cccBody
    ]

-- | Code Climate location
data CodeClimateLocation = CodeClimateLocation
  { cclPath  :: Text
  , cclLines :: CodeClimateLines
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CodeClimateLocation where
  toJSON CodeClimateLocation{..} = object
    [ "path"  .= cclPath
    , "lines" .= cclLines
    ]

-- | Code Climate line range
data CodeClimateLines = CodeClimateLines
  { cclBegin :: Int
  , cclEnd   :: Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CodeClimateLines where
  toJSON CodeClimateLines{..} = object
    [ "begin" .= cclBegin
    , "end"   .= cclEnd
    ]

-- | Convert a diagnostic to a Code Climate issue
diagToIssue :: FilePath -> Diagnostic -> CodeClimateIssue
diagToIssue path diag = CodeClimateIssue
  { cciType = "issue"
  , cciCheckName = fromMaybe "argus" (diagCode diag)
  , cciDescription = diagMessage diag
  , cciContent = Nothing
  , cciCategories = kindToCategories (diagKind diag)
  , cciLocation = CodeClimateLocation
      { cclPath = T.pack path
      , cclLines = CodeClimateLines
          { cclBegin = srcSpanStartLineRaw (diagSpan diag)
          , cclEnd = srcSpanEndLineRaw (diagSpan diag)
          }
      }
  , cciSeverity = severityToCodeClimate (diagSeverity diag)
  , cciFingerprint = generateFingerprint path diag
  }

-- | Convert severity to Code Climate severity
severityToCodeClimate :: Severity -> Text
severityToCodeClimate Error      = "blocker"
severityToCodeClimate Warning    = "major"
severityToCodeClimate Suggestion = "minor"
severityToCodeClimate Info       = "info"

-- | Convert diagnostic kind to Code Climate categories
kindToCategories :: DiagnosticKind -> [Text]
kindToCategories = \case
  NamingConvention   -> ["Style"]
  UnusedCode         -> ["Style", "Clarity"]
  UnusedImport       -> ["Style", "Clarity"]
  RedundantCode      -> ["Style", "Clarity"]
  CodePattern        -> ["Style"]
  TypeSignature      -> ["Clarity"]
  ImportStyle        -> ["Style"]
  TemplateHaskellRef -> ["Complexity"]
  SecurityIssue      -> ["Security"]
  PerformanceIssue   -> ["Performance"]
  ArchitecturalIssue -> ["Complexity", "Duplication"]
  SpaceLeak          -> ["Performance"]
  PartialFunction    -> ["Bug Risk"]
  ComplexityIssue    -> ["Complexity"]
  Custom _           -> ["Style"]

-- | Generate a unique fingerprint for the issue
-- This is used to track the same issue across runs
generateFingerprint :: FilePath -> Diagnostic -> Text
generateFingerprint path diag =
  let components = T.intercalate ":"
        [ T.pack path
        , T.pack $ show $ srcSpanStartLineRaw $ diagSpan diag
        , fromMaybe "unknown" $ diagCode diag
        , T.take 50 $ diagMessage diag
        ]
  in md5Hex components

-- | Simple fingerprint hash (not cryptographic MD5, just a stable hash)
md5Hex :: Text -> Text
md5Hex input =
  let hash = simpleHash input
  in T.pack $ showHex hash ""

-- | Simple hash function for fingerprinting
simpleHash :: Text -> Word
simpleHash text =
  let chars = T.unpack text
  in foldl (\h c -> h * 31 + fromIntegral (ord c)) 0 chars

-- | Maybe helper
fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x
