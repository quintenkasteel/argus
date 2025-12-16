{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.LSP
-- Description : LSP server command
-- Copyright   : (c) 2024
-- License     : MIT
--
-- = Overview
--
-- This module implements the @argus lsp@ command, which runs Argus as
-- a Language Server Protocol server for IDE integration.
--
-- = LSP Features
--
-- * __Diagnostics__: Real-time issue detection
-- * __Code Actions__: Quick fixes for detected issues
-- * __Hover__: Rule documentation on hover
-- * __Progress__: Analysis progress reporting
--
-- = IDE Integration
--
-- Configure your editor to use @argus lsp@ as a language server:
--
-- __VS Code__ (settings.json):
--
-- @
-- {
--   "haskell.languageServerVariant": "argus",
--   "haskell.serverExecutablePath": "argus",
--   "haskell.serverExtraArgs": ["lsp"]
-- }
-- @
--
-- __Neovim__ (using nvim-lspconfig):
--
-- @
-- require(\"lspconfig\").argus.setup{}
-- @
--
-- @since 1.0.0
module Argus.CLI.LSP
  ( -- * Command Entry Point
    runLsp
  ) where

import Argus.CLI.Types
import Argus.LSP.Server qualified as LSP

-- | Run the LSP server.
--
-- Starts the Language Server Protocol server on stdio, communicating
-- with the IDE using JSON-RPC.
--
-- @since 1.0.0
runLsp :: GlobalOptions -> LspOptions -> IO ()
runLsp _global opts = do
  let lspCfg = LSP.defaultLspConfig
        { LSP.lspConfigFile = goConfigFile _global
        , LSP.lspDebugLog = loDebugLog opts
        , LSP.lspAnalyzeOnChange = loAnalyzeOnChange opts
        , LSP.lspDebounceMs = loDebounceMs opts
        , LSP.lspProgressReporting = loProgressReporting opts
        }
  LSP.runLspServer lspCfg
