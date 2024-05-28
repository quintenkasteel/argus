{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

import ClassyPrelude
import qualified Config
import qualified Linter
import qualified Util

-- Main function to apply variable naming checks
main :: IO ()
main = do
  config <- Config.parse "config.yaml"
  print $ show config
  files <- Util.listHaskellFiles config.directory
  traverse_
    ( \f ->
        Linter.checkFile config f
          >>= maybe (pure ()) (\(c, l) -> Util.pPrint c >> Util.pPrint l)
    )
    files
