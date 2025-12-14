{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.LSP
-- Description : LSP server command
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module implements the LSP server command.
module Argus.CLI.LSP
  ( runLsp
  ) where

import Argus.CLI.Types
import Argus.LSP.Server qualified as LSP

-- | Run LSP server
runLsp :: GlobalOptions -> LspOptions -> IO ()
runLsp global opts = do
  let lspCfg = LSP.defaultLspConfig
        { LSP.lspConfigFile = goConfigFile global
        , LSP.lspDebugLog = loDebugLog opts
        , LSP.lspAnalyzeOnChange = loAnalyzeOnChange opts
        , LSP.lspDebounceMs = loDebounceMs opts
        , LSP.lspProgressReporting = loProgressReporting opts
        }
  LSP.runLspServer lspCfg
