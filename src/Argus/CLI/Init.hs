{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Init
-- Description : Init command for configuration setup
-- Copyright   : (c) 2024
-- License     : MIT
--
-- = Overview
--
-- This module implements the @argus init@ command, which creates a default
-- configuration file in the current directory.
--
-- = Behavior
--
-- 1. Check if @argus.toml@ already exists
-- 2. If exists and @--force@ not set, exit with error
-- 3. Write default configuration template
-- 4. Report success
--
-- @since 1.0.0
module Argus.CLI.Init
  ( -- * Command Entry Point
    runInit
  ) where

import Control.Monad (when)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Argus.CLI.Types
import Argus.CLI.Common (defaultConfigToml)

-- | Run the init command.
--
-- Creates @argus.toml@ with default configuration. Fails if file exists
-- unless @--force@ is specified.
--
-- @since 1.0.0
runInit :: GlobalOptions -> InitOptions -> IO ()
runInit _ opts = do
  let configPath = "argus.toml"
  exists <- doesFileExist configPath
  when (exists && not (ioForce opts)) $ do
    hPutStrLn stderr "Configuration file already exists. Use --force to overwrite."
    exitFailure

  TIO.writeFile configPath defaultConfigToml
  TIO.putStrLn $ "Created " <> T.pack configPath
