import ClassyPrelude
import qualified Config
import qualified Linter
import qualified System.Environment as Environment
import qualified Util

-- Directories to check
directoriesToCheck :: [FilePath]
directoriesToCheck = ["data"]

-- Main function to apply variable naming checks
main :: IO ()
main = do
  args <- Environment.getArgs
  config <- Config.parse "config.yaml"
  let dirs = if null args then directoriesToCheck else args
  files <- concat <$> mapM Util.listHaskellFiles dirs
  traverse_
    ( \f ->
        Linter.checkFile config f
          >>= maybe (pure ()) (\(c, l) -> Util.pPrint c >> Util.pPrint l)
    )
    files
