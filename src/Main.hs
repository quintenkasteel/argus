import ClassyPrelude
import Config (Config (..), Signature (..), Variable (..))
import qualified Config
import qualified Data.Map as Map
import Data.String.Interpolate (i)
import qualified Extra.Function as Function
import Function (Function (..), FunctionArgument (..))
import Language.Haskell.Exts
import Lint (Lint (..), LintMap)
import Linter
import qualified System.Environment as Environment
import Util

-- Directories to check
directoriesToCheck :: [FilePath]
directoriesToCheck = ["check"]

-- Main function to apply variable naming checks
main :: IO ()
main = do
  args <- Environment.getArgs
  config <- Config.parse "config.yaml"
  let dirs = if null args then directoriesToCheck else args
  files <- concat <$> mapM listHaskellFiles dirs
  traverse_
    ( \f ->
        Linter.checkFile config f
          >>= maybe (pure ()) (\(c, l) -> Util.pPrint c >> Util.pPrint l)
    )
    files
