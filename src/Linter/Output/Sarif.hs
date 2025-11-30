{-# LANGUAGE StrictData #-}

-- |
-- Module      : Linter.Output.Sarif
-- Description : SARIF output formatting
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides SARIF (Static Analysis Results Interchange Format)
-- output for integration with GitHub Code Scanning and other tools.
module Linter.Output.Sarif
  ( -- * Rendering
    renderSarif

    -- * SARIF types
  , SarifLog (..)
  , SarifRun (..)
  , SarifTool (..)
  , SarifResult (..)
  , SarifLocation (..)
  ) where

import Data.Aeson (ToJSON (..), encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)

import Linter.Output.Types
import Linter.Types

--------------------------------------------------------------------------------
-- SARIF Types (v2.1.0)
--------------------------------------------------------------------------------

-- | SARIF log format
data SarifLog = SarifLog
  { slVersion :: Text
  , slSchema  :: Text
  , slRuns    :: [SarifRun]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SarifLog where
  toJSON SarifLog{..} = object
    [ "version" .= slVersion
    , "$schema" .= slSchema
    , "runs"    .= slRuns
    ]

-- | SARIF run
data SarifRun = SarifRun
  { srTool    :: SarifTool
  , srResults :: [SarifResult]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SarifRun where
  toJSON SarifRun{..} = object
    [ "tool"    .= srTool
    , "results" .= srResults
    ]

-- | SARIF tool information
data SarifTool = SarifTool
  { stDriver :: SarifDriver
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SarifTool where
  toJSON SarifTool{..} = object
    [ "driver" .= stDriver
    ]

-- | SARIF driver (tool) information
data SarifDriver = SarifDriver
  { sdName           :: Text
  , sdVersion        :: Text
  , sdInformationUri :: Text
  , sdRules          :: [SarifRule]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SarifDriver where
  toJSON SarifDriver{..} = object
    [ "name"           .= sdName
    , "version"        .= sdVersion
    , "informationUri" .= sdInformationUri
    , "rules"          .= sdRules
    ]

-- | SARIF rule definition
data SarifRule = SarifRule
  { sruId               :: Text
  , sruName             :: Text
  , sruShortDescription :: SarifMessage
  , sruDefaultLevel     :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SarifRule where
  toJSON SarifRule{..} = object
    [ "id"               .= sruId
    , "name"             .= sruName
    , "shortDescription" .= sruShortDescription
    , "defaultConfiguration" .= object ["level" .= sruDefaultLevel]
    ]

-- | SARIF message
data SarifMessage = SarifMessage
  { smText :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SarifMessage where
  toJSON SarifMessage{..} = object ["text" .= smText]

-- | SARIF result
data SarifResult = SarifResult
  { sresRuleId    :: Text
  , sresLevel     :: Text
  , sresMessage   :: SarifMessage
  , sresLocations :: [SarifLocation]
  , sresFixes     :: [SarifFix]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SarifResult where
  toJSON SarifResult{..} = object
    [ "ruleId"    .= sresRuleId
    , "level"     .= sresLevel
    , "message"   .= sresMessage
    , "locations" .= sresLocations
    , "fixes"     .= sresFixes
    ]

-- | SARIF location
data SarifLocation = SarifLocation
  { slPhysicalLocation :: SarifPhysicalLocation
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SarifLocation where
  toJSON SarifLocation{..} = object
    [ "physicalLocation" .= slPhysicalLocation
    ]

-- | SARIF physical location
data SarifPhysicalLocation = SarifPhysicalLocation
  { splArtifactLocation :: SarifArtifactLocation
  , splRegion           :: SarifRegion
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SarifPhysicalLocation where
  toJSON SarifPhysicalLocation{..} = object
    [ "artifactLocation" .= splArtifactLocation
    , "region"           .= splRegion
    ]

-- | SARIF artifact location
data SarifArtifactLocation = SarifArtifactLocation
  { salUri :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SarifArtifactLocation where
  toJSON SarifArtifactLocation{..} = object
    [ "uri" .= salUri
    ]

-- | SARIF region
data SarifRegion = SarifRegion
  { sregStartLine   :: Int
  , sregStartColumn :: Int
  , sregEndLine     :: Int
  , sregEndColumn   :: Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SarifRegion where
  toJSON SarifRegion{..} = object
    [ "startLine"   .= sregStartLine
    , "startColumn" .= sregStartColumn
    , "endLine"     .= sregEndLine
    , "endColumn"   .= sregEndColumn
    ]

-- | SARIF fix
data SarifFix = SarifFix
  { sfDescription   :: SarifMessage
  , sfArtifactChanges :: [SarifArtifactChange]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SarifFix where
  toJSON SarifFix{..} = object
    [ "description"     .= sfDescription
    , "artifactChanges" .= sfArtifactChanges
    ]

-- | SARIF artifact change
data SarifArtifactChange = SarifArtifactChange
  { sacArtifactLocation :: SarifArtifactLocation
  , sacReplacements     :: [SarifReplacement]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SarifArtifactChange where
  toJSON SarifArtifactChange{..} = object
    [ "artifactLocation" .= sacArtifactLocation
    , "replacements"     .= sacReplacements
    ]

-- | SARIF replacement
data SarifReplacement = SarifReplacement
  { srpDeletedRegion   :: SarifRegion
  , srpInsertedContent :: SarifMessage
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SarifReplacement where
  toJSON SarifReplacement{..} = object
    [ "deletedRegion"   .= srpDeletedRegion
    , "insertedContent" .= srpInsertedContent
    ]

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- | Render analysis result as SARIF
renderSarif :: OutputOptions -> AnalysisResult -> Text
renderSarif _ result =
  TE.decodeUtf8 $ BL.toStrict $ encode $ toSarifLog result

-- | Convert result to SARIF log
toSarifLog :: AnalysisResult -> SarifLog
toSarifLog result = SarifLog
  { slVersion = "2.1.0"
  , slSchema = "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json"
  , slRuns = [toSarifRun result]
  }

-- | Convert result to SARIF run
toSarifRun :: AnalysisResult -> SarifRun
toSarifRun result = SarifRun
  { srTool = SarifTool
      { stDriver = SarifDriver
          { sdName = "haskell-linter"
          , sdVersion = "1.0.0"
          , sdInformationUri = "https://github.com/quinten/haskell-linter"
          , sdRules = extractRules result
          }
      }
  , srResults = concatMap toSarifResults $ Map.toList (resultFiles result)
  }

-- | Extract rules from diagnostics
extractRules :: AnalysisResult -> [SarifRule]
extractRules result =
  let allDiags = concatMap fileResultDiagnostics $ Map.elems (resultFiles result)
      uniqueRules = map head $ groupBy (\a b -> diagCode a == diagCode b) allDiags
  in map toSarifRule uniqueRules
  where
    groupBy _ [] = []
    groupBy f (x:xs) = (x : takeWhile (f x) xs) : groupBy f (dropWhile (f x) xs)

-- | Convert diagnostic to SARIF rule
toSarifRule :: Diagnostic -> SarifRule
toSarifRule diag = SarifRule
  { sruId = maybe "unknown" id (diagCode diag)
  , sruName = kindToName (diagKind diag)
  , sruShortDescription = SarifMessage (diagMessage diag)
  , sruDefaultLevel = severityToLevel (diagSeverity diag)
  }

-- | Convert diagnostic kind to name
kindToName :: DiagnosticKind -> Text
kindToName NamingConvention   = "naming-convention"
kindToName UnusedCode         = "unused-code"
kindToName UnusedImport       = "unused-import"
kindToName RedundantCode      = "redundant-code"
kindToName CodePattern        = "code-pattern"
kindToName TypeSignature      = "type-signature"
kindToName ImportStyle        = "import-style"
kindToName TemplateHaskellRef = "template-haskell"
kindToName (Custom name)      = name

-- | Convert severity to SARIF level
severityToLevel :: Severity -> Text
severityToLevel Error      = "error"
severityToLevel Warning    = "warning"
severityToLevel Suggestion = "note"
severityToLevel Info       = "note"

-- | Convert file results to SARIF results
toSarifResults :: (FilePath, FileResult) -> [SarifResult]
toSarifResults (path, fr) = map (toSarifResult path) (fileResultDiagnostics fr)

-- | Convert diagnostic to SARIF result
toSarifResult :: FilePath -> Diagnostic -> SarifResult
toSarifResult path diag = SarifResult
  { sresRuleId = maybe "unknown" id (diagCode diag)
  , sresLevel = severityToLevel (diagSeverity diag)
  , sresMessage = SarifMessage (diagMessage diag)
  , sresLocations = [toSarifLocation path (diagSpan diag)]
  , sresFixes = map (toSarifFix path) (diagFixes diag)
  }

-- | Convert span to SARIF location
toSarifLocation :: FilePath -> SrcSpan -> SarifLocation
toSarifLocation path span = SarifLocation
  { slPhysicalLocation = SarifPhysicalLocation
      { splArtifactLocation = SarifArtifactLocation (T.pack path)
      , splRegion = SarifRegion
          { sregStartLine = srcSpanStartLine span
          , sregStartColumn = srcSpanStartCol span
          , sregEndLine = srcSpanEndLine span
          , sregEndColumn = srcSpanEndCol span
          }
      }
  }

-- | Convert fix to SARIF fix
toSarifFix :: FilePath -> Fix -> SarifFix
toSarifFix path fix = SarifFix
  { sfDescription = SarifMessage (fixTitle fix)
  , sfArtifactChanges = [SarifArtifactChange
      { sacArtifactLocation = SarifArtifactLocation (T.pack path)
      , sacReplacements = map toSarifReplacement (fixEdits fix)
      }]
  }

-- | Convert fix edit to SARIF replacement
toSarifReplacement :: FixEdit -> SarifReplacement
toSarifReplacement edit = SarifReplacement
  { srpDeletedRegion = SarifRegion
      { sregStartLine = srcSpanStartLine (fixEditSpan edit)
      , sregStartColumn = srcSpanStartCol (fixEditSpan edit)
      , sregEndLine = srcSpanEndLine (fixEditSpan edit)
      , sregEndColumn = srcSpanEndCol (fixEditSpan edit)
      }
  , srpInsertedContent = SarifMessage (fixEditNewText edit)
  }
