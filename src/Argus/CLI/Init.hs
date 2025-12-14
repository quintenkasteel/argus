{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Init
-- Description : Init command for configuration setup
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module implements the init command for creating configuration files.
module Argus.CLI.Init
  ( runInit
  ) where

import Control.Monad (when)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Argus.CLI.Types
import Argus.CLI.Common (defaultConfigToml)

-- | Run init command
runInit :: GlobalOptions -> InitOptions -> IO ()
runInit _global opts = do
  let configPath = "argus.toml"
  exists <- doesFileExist configPath
  when (exists && not (ioForce opts)) $ do
    hPutStrLn stderr "Configuration file already exists. Use --force to overwrite."
    exitFailure

  TIO.writeFile configPath defaultConfigToml
  TIO.putStrLn $ "Created " <> T.pack configPath
