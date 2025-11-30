{-# LANGUAGE StrictData #-}

-- |
-- Module      : Linter.Output.Json
-- Description : JSON output formatting
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides JSON output for linting results.
module Linter.Output.Json
  ( -- * Rendering
    renderJson
  , renderJsonPretty

    -- * Types
  , JsonOutput (..)
  ) where

import Data.Aeson (ToJSON (..), encode, object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)

import Linter.Output.Types
import Linter.Types

--------------------------------------------------------------------------------
-- JSON Output Type
--------------------------------------------------------------------------------

-- | JSON output structure
data JsonOutput = JsonOutput
  { joVersion     :: Text
  , joFiles       :: [JsonFile]
  , joSummary     :: Summary
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON JsonOutput where
  toJSON JsonOutput{..} = object
    [ "version" .= joVersion
    , "files"   .= joFiles
    , "summary" .= joSummary
    ]

-- | JSON file output
data JsonFile = JsonFile
  { jfPath        :: FilePath
  , jfDiagnostics :: [Diagnostic]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON JsonFile where
  toJSON JsonFile{..} = object
    [ "path"        .= jfPath
    , "diagnostics" .= jfDiagnostics
    ]

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- | Render analysis result as JSON
renderJson :: OutputOptions -> AnalysisResult -> Text
renderJson _ result =
  TE.decodeUtf8 $ BL.toStrict $ encode $ toJsonOutput result

-- | Render analysis result as pretty JSON
renderJsonPretty :: OutputOptions -> AnalysisResult -> Text
renderJsonPretty _ result =
  TE.decodeUtf8 $ BL.toStrict $ encodePretty $ toJsonOutput result

-- | Convert result to JSON output
toJsonOutput :: AnalysisResult -> JsonOutput
toJsonOutput result = JsonOutput
  { joVersion = "1.0.0"
  , joFiles = map toJsonFile $ Map.toList (resultFiles result)
  , joSummary = mkSummary result
  }

-- | Convert file result to JSON
toJsonFile :: (FilePath, FileResult) -> JsonFile
toJsonFile (path, fr) = JsonFile
  { jfPath = path
  , jfDiagnostics = fileResultDiagnostics fr
  }
