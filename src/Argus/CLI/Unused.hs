{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Unused
-- Description : Unused code detection command
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module implements the unused code detection command.
module Argus.CLI.Unused
  ( runUnused
  ) where

import Control.Monad (when)
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import System.Exit (exitFailure)
import System.FilePath ((</>))

import Argus.CLI.Types
import Argus.CLI.Common (parseOutputFormat)
import Argus.Core
import Argus.Types
import Argus.Output.Types
import Argus.Output.Terminal
import Argus.Output.Json
import Argus.Output.Sarif
import Argus.Output.Html
import Argus.Output.JUnit
import Argus.Output.CodeClimate
import Argus.Output.Checkstyle

-- | Run unused command
-- Now uses the same output formatting as check command
runUnused :: GlobalOptions -> UnusedOptions -> IO ()
runUnused global opts = do
  -- Use the target path for the HIE directory, defaulting to current directory
  let targetPaths = if null (uoTargets opts) then ["."] else uoTargets opts
      -- Resolve the HIE directory relative to the first target path
      hieDir = case targetPaths of
        (p:_) -> Just $ p </> ".hie"
        []    -> Just ".hie"

  let linterOpts = ArgusOptions
        { optMode = FullMode
        , optConfigFile = goConfigFile global
        , optTargetPaths = targetPaths
        , optHieDir = hieDir
        , optOutputFormat = uoOutputFormat opts
        , optApplyFixes = False
        , optInteractive = False
        , optPreview = False
        , optVerbose = goVerbose global
        , optNoColor = goNoColor global
        , optParallel = fromIntegral (goParallel global)
        }

  result <- runArgus linterOpts

  -- Use the same output formatting as check command
  let outputOpts = OutputOptions
        { ooFormat = parseOutputFormat (uoOutputFormat opts)
        , ooColor = not (goNoColor global)
        , ooGroupBy = "file"
        , ooShowContext = uoShowContext opts
        , ooContextLines = uoContextLines opts
        , ooVerbose = goVerbose global
        , ooSourceCache = Map.empty
        }

  let output = case uoOutputFormat opts of
        "json"        -> renderJson outputOpts result
        "sarif"       -> renderSarif outputOpts result
        "html"        -> renderHtml outputOpts result
        "plain"       -> renderTerminal outputOpts { ooColor = False } result
        "junit"       -> outText (renderJUnit outputOpts result)
        "codeclimate" -> outText (renderCodeClimate outputOpts result)
        "checkstyle"  -> outText (renderCheckstyle outputOpts result)
        _             -> renderTerminal outputOpts result

  TIO.putStrLn output

  -- Exit with error if there are any errors
  let hasErrors = any (== Error) $ Map.keys (resultDiagCount result)
  when hasErrors exitFailure

