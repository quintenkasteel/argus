{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

import ClassyPrelude hiding (head)
import Config (Config)
import qualified Config
import qualified Linter
import qualified Util

main :: IO ()
main = do
  config <- Config.parse
  files <- Util.listHaskellFiles config.directory
  traverse_ (lintAndImproveFile config) files

lintAndImproveFile :: Config -> FilePath -> IO ()
lintAndImproveFile config file =
  Linter.checkFile config file
    >>= maybe
      (pure ())
      ( \(newContent, lints) ->
          outPutLints lints
            >> Util.writeToFile file (pack newContent)
      )
  where
    outPutLints =
      traverse_ Util.pPrint
    maybeImproveContent newContent =
      if config.improve
        then do
          improvedContent <-
            Util.runWithTimeouts
              (Improve.run (pack newContent))
              [(0, "Starting..."), (5, "Still busy?")]
          print $ show improvedContent
          pure newContent
        else pure (pack newContent)
